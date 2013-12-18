package ch.fhnw.codegen

import ch.fhnw.imlcompiler.AST._
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.MethodVisitor
import java.io.FileOutputStream
import ch.fhnw.imlcompiler.SymbolTable
import scala.collection.mutable.ListBuffer
import ch.fhnw.imlcompiler.Store
import ch.fhnw.imlcompiler.ContextChecker

trait JVMByteCodeGen extends ContextChecker {

  class CodeGenContext(val cw: ClassWriter, val prog: Program, val st: SymbolTable)

  def writeCode(prog: Program, st: SymbolTable) = {
    val classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES);

    val context = new CodeGenContext(classWriter, prog, st);

    classWriter.visitSource(prog.name.value, null);

    writeFields()(context)
    writeClass()(context)
    writeConstructor()(context)
    writeMainProgramm()(context)

    val fileWriter = new FileOutputStream(prog.name.value + ".class")
    fileWriter.write(classWriter.toByteArray())
    fileWriter.close
  }

  def writeCommands(cmds: List[Cmd], mv: MethodVisitor, scope: ListBuffer[Store], localAccess: Boolean)(implicit context: CodeGenContext) = cmds.foreach(writeCommand(_, mv, scope, localAccess))
  def writeCommand(cmd: Cmd, mv: MethodVisitor, scope: ListBuffer[Store], localAccess: Boolean)(implicit context: CodeGenContext) {
    cmd match {
      case c: BecomesCmd => writeBecomesCmd(c, mv, scope, localAccess);
      case c: OutputCmd => writeOutputCmd(c, mv, scope, localAccess);
    }
  }

  def writeBecomesCmd(o: BecomesCmd, mv: MethodVisitor, scope: ListBuffer[Store], localAccess: Boolean)(implicit context: CodeGenContext) = {
    writeExpr(o.rhs, mv, scope, localAccess);

    if (localAccess) {
      
    } else {
    	writeExpr(o.rhs, mv, scope, localAccess)
    	
    	o.lhs match {
    	  case e:StoreExpr => {
    	    mv.visitFieldInsn(PUTSTATIC, context.prog.name.value, e.i.value, toJVMType(scope.find(_.typedIdent.i == e.i).get.typedIdent.t));
    	  }
    	  case _ => throw new IllegalStateException
    	}
    	
    }
  }

  def writeOutputCmd(o: OutputCmd, mv: MethodVisitor, scope: ListBuffer[Store], localAccess: Boolean)(implicit context: CodeGenContext) = {
    mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
    writeExpr(o.expr, mv, scope, localAccess)
    mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(" + toJVMType(returnType(o.expr, scope)(context.st)) + ")V");
  }

  //  def writeExprs(exprs: List[Expr], mv: MethodVisitor, scope: ListBuffer[Store], localAccess: Boolean = false)(implicit context: CodeGenContext) = exprs.foreach(writeExpr(_, mv, scope, localAccess))
  def writeExpr(expr: Expr, mv: MethodVisitor, scope: ListBuffer[Store], localAccess: Boolean)(implicit context: CodeGenContext) = {
    expr match {
      case e: StoreExpr => writeStoreAccess(e, mv, scope, localAccess)
      case d: DyadicExpr => writeDyadicExpr(d, mv, scope, localAccess)
      case l: LiteralExpr => writeLiteralExpr(l, mv, scope, localAccess)
    }
  }

  // TODO what is local and what not? (access inside main method NOT local, access of global imports NOT local)
  def writeStoreAccess(expr: StoreExpr, mv: MethodVisitor, scope: ListBuffer[Store], localAccess: Boolean)(implicit context: CodeGenContext) {
    if (localAccess) {
      // TODO impl
    } else {
      mv.visitFieldInsn(GETSTATIC, context.prog.name.value, expr.i.value, toJVMType(scope.find(_.typedIdent.i == expr.i).get.typedIdent.t))
    }
  }

  def writeDyadicExpr(dyadicExpr: DyadicExpr, mv: MethodVisitor, scope: ListBuffer[Store], localAccess: Boolean)(implicit context: CodeGenContext) {
    writeExpr(dyadicExpr.l, mv, scope, localAccess)
    writeExpr(dyadicExpr.r, mv, scope, localAccess)

    dyadicExpr.op match {
      case PlusOpr => mv.visitInsn(IADD)
      case DivOpr => mv.visitInsn(IDIV)
      case TimesOpr => mv.visitInsn(IMUL)
    }
  }

  def writeLiteralExpr(l: LiteralExpr, mv: MethodVisitor, scope: ListBuffer[Store], localAccess: Boolean)(implicit context: CodeGenContext) {
    l.l match {
      case IntLiteral(v) => mv.visitIntInsn(SIPUSH, v)
    }

  }

  def writeFields()(implicit context: CodeGenContext) {
    context.st.globalStores.foreach(s => context.cw.visitField(ACC_PRIVATE + ACC_STATIC, s.typedIdent.i.value, toJVMType(s.typedIdent.t).toString, null, null))
  }

  def writeClass()(implicit context: CodeGenContext) {
    context.cw.visit(V1_7, ACC_PUBLIC + ACC_SUPER, context.prog.name.value, null, "java/lang/Object", null);
  }

  def writeConstructor()(implicit context: CodeGenContext) {
    val constructor = context.cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
    constructor.visitVarInsn(ALOAD, 0);
    constructor.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V");
    constructor.visitInsn(RETURN);
    constructor.visitMaxs(0, 0);
    constructor.visitEnd();
  }

  def writeMainProgramm()(implicit context: CodeGenContext) {
    val main = context.cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null);
    main.visitVarInsn(ALOAD, 0);

    writeCommands(context.prog.commands, main, context.st.globalStores, false);

    main.visitMaxs(0, 0);
    main.visitInsn(RETURN);
    main.visitEnd();
  }

  def toJVMType(t: Type): String = t match {
    case IntType => "I"
    case BoolType => "Z"
    case ListType(t) => "[" + toJVMType(t)
    case Any => throw new IllegalStateException
  }
}