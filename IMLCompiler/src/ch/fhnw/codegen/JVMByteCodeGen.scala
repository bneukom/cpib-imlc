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
import org.objectweb.asm.Label
import ch.fhnw.imlcompiler.Scope
import scala.reflect.ClassTag

// TODO create internal write int method which always chooses the best one ICONST_1 BIPISH etc
trait JVMByteCodeGen extends ContextChecker {

  class CodeGenContext(val cw: ClassWriter, val prog: Program, val st: SymbolTable)

  def writeCode(prog: Program, st: SymbolTable) = {
    val classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES);

    val context = new CodeGenContext(classWriter, prog, st);

    classWriter.visitSource(prog.name.value, null);

    writeGlobalStores()(context)
    writeClass()(context)
    writeConstructor()(context)
    writeMainProgramm()(context)

    val fileWriter = new FileOutputStream(prog.name.value + ".class")
    fileWriter.write(classWriter.toByteArray())
    fileWriter.close
  }

  def writeCommands(cmds: List[Cmd], mv: MethodVisitor, scope: Scope, localAccess: Boolean)(implicit context: CodeGenContext) = cmds.foreach(writeCommand(_, mv, scope, localAccess))
  def writeCommand(cmd: Cmd, mv: MethodVisitor, scope: Scope, localAccess: Boolean)(implicit context: CodeGenContext) {
    cmd match {
      case c: BecomesCmd => writeBecomesCmd(c, mv, scope, localAccess);
      case c: OutputCmd => writeOutputCmd(c, mv, scope, localAccess);
      case c: IfCmd => writeIfCmd(c, mv, scope, localAccess)
      case c: SkipCmd => mv.visitInsn(NOP)
    }
  }

  def writeIfCmd(o: IfCmd, mv: MethodVisitor, scope: Scope, localAccess: Boolean)(implicit context: CodeGenContext) = {
    writeExpr(o.expr, mv, scope, localAccess)

    val l1 = new Label()
    val end = new Label()

    mv.visitJumpInsn(IFEQ, l1)
    writeCommands(o.ifCmd, mv, scope, localAccess)
    mv.visitJumpInsn(GOTO, end)
    mv.visitLabel(l1)
    writeCommands(o.elseCmd, mv, scope, localAccess)
    mv.visitLabel(end)
  }

  def writeBecomesCmd(o: BecomesCmd, mv: MethodVisitor, scope: Scope, localAccess: Boolean)(implicit context: CodeGenContext) = {
    writeExpr(o.rhs, mv, scope, localAccess);

    if (localAccess) {

    } else {
      o.lhs match {
        case e: StoreExpr => {
          val t = scope.find(_.typedIdent.i == e.i).get.typedIdent.t
          mv.visitFieldInsn(PUTSTATIC, context.prog.name.value, e.i.value, toJVMType(t));
        }
        case _ => throw new IllegalStateException
      }
    }
  }

  def writeOutputCmd(o: OutputCmd, mv: MethodVisitor, scope: Scope, localAccess: Boolean)(implicit context: CodeGenContext) = {

    val retType = returnType(o.expr, scope)(context.st);
    retType match {
      case l: ListType => {
        // use the deepToString library method
        mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
        writeExpr(o.expr, mv, scope, localAccess)
        mv.visitMethodInsn(INVOKESTATIC, "java/util/Arrays", "deepToString", "([Ljava/lang/Object;)Ljava/lang/String;");
        mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
      }
      case _ => {
        mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
        writeExpr(o.expr, mv, scope, localAccess)
        mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(" + toJVMType(returnType(o.expr, scope)(context.st)) + ")V");
      }
    }

  }

  //  def writeExprs(exprs: List[Expr], mv: MethodVisitor, scope: ListBuffer[Store], localAccess: Boolean = false)(implicit context: CodeGenContext) = exprs.foreach(writeExpr(_, mv, scope, localAccess))
  def writeExpr(expr: Expr, mv: MethodVisitor, scope: Scope, localAccess: Boolean)(implicit context: CodeGenContext) = {
    expr match {
      case e: StoreExpr => writeStoreAccess(e, mv, scope, localAccess)
      case d: DyadicExpr => writeDyadicExpr(d, mv, scope, localAccess)
      case m: MonadicExpr => writeMonadicExpr(m, mv, scope, localAccess)
      case l: LiteralExpr => writeLiteralExpr(l, mv, scope, localAccess)
    }
  }

  // TODO what is local and what not? (access inside main method NOT local, access of global imports NOT local)
  def writeStoreAccess(expr: StoreExpr, mv: MethodVisitor, scope: Scope, localAccess: Boolean)(implicit context: CodeGenContext) {
    if (localAccess) {
      // TODO impl
    } else {
      val vmType = toJVMType(scope.find(_.typedIdent.i == expr.i).get.typedIdent.t);
      mv.visitFieldInsn(GETSTATIC, context.prog.name.value, expr.i.value, vmType)
    }
  }

  def writeDyadicExpr(dyadicExpr: DyadicExpr, mv: MethodVisitor, scope: Scope, localAccess: Boolean)(implicit context: CodeGenContext) {

    dyadicExpr.op match {
      case PlusOpr =>
        writeExpr(dyadicExpr.l, mv, scope, localAccess); writeExpr(dyadicExpr.r, mv, scope, localAccess); mv.visitInsn(IADD)
      case MinusOpr =>
        writeExpr(dyadicExpr.l, mv, scope, localAccess); writeExpr(dyadicExpr.r, mv, scope, localAccess); mv.visitInsn(ISUB)
      case DivOpr =>
        writeExpr(dyadicExpr.l, mv, scope, localAccess); writeExpr(dyadicExpr.r, mv, scope, localAccess); mv.visitInsn(IDIV)
      case TimesOpr =>
        writeExpr(dyadicExpr.l, mv, scope, localAccess); writeExpr(dyadicExpr.r, mv, scope, localAccess); mv.visitInsn(IMUL)
      case EQ =>
        writeExpr(dyadicExpr.l, mv, scope, localAccess); writeExpr(dyadicExpr.r, mv, scope, localAccess); writeCheckEquals(mv);
      case ConsOpr =>
        writeExpr(dyadicExpr.r, mv, scope, localAccess); writeExpr(dyadicExpr.l, mv, scope, localAccess); writeListCons(mv);
    }
  }

  def writeMonadicExpr(monadicExpr: MonadicExpr, mv: MethodVisitor, scope: Scope, localAccess: Boolean)(implicit context: CodeGenContext) {
    monadicExpr.op match {
      case LengthOpr => writeExpr(monadicExpr.l, mv, scope, localAccess); mv.visitInsn(ARRAYLENGTH);
      case HeadOpr => {
        writeExpr(monadicExpr.l, mv, scope, localAccess);
        mv.visitInsn(ICONST_0);

        val listType = returnType(monadicExpr.l, scope)(context.st);
        val level = listLevel(listType);

        // since its an object array always use aaload
        mv.visitInsn(AALOAD);

        if (level > 1) {
          mv.visitTypeInsn(CHECKCAST, "[Ljava/lang/Object;");
        } else {
          deepType(listType) match {
            case IntType => {
              mv.visitTypeInsn(CHECKCAST, "java/lang/Integer");
              mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I");
            }
            case BoolType => // TODO
            case _ =>
          }
        }
      }
    }
  }

  def writeLiteralExpr(l: LiteralExpr, mv: MethodVisitor, scope: Scope, localAccess: Boolean, listLev: Int = 0)(implicit context: CodeGenContext) {
    l.l match {
      case BoolLiteral(b) =>
        mv.visitIntInsn(BIPUSH, if (b) 1 else 0)
      case IntLiteral(v) =>
        mv.visitLdcInsn(new Integer(v));
      case l: ListLiteral => {
        val retType = returnType(l, scope)(context.st);
        writeListLiteral(l, mv, scope, localAccess, listLevel(retType) - 1);
      }
    }
  }

  def writeListLiteral(l: ListLiteral, mv: MethodVisitor, scope: Scope, localAccess: Boolean, listLevel: Int)(implicit context: CodeGenContext) {
    mv.visitIntInsn(BIPUSH, l.l.size) // write array length

    val t = returnType(l, scope)(context.st);
    val arrType = ("[" * listLevel) + "Ljava/lang/Object;";
    mv.visitTypeInsn(ANEWARRAY, arrType); // type of array must be [Object for a list of [[Object

    l.l.zipWithIndex.foreach {
      case (e, i) =>
        mv.visitInsn(DUP);
        mv.visitIntInsn(BIPUSH, i);

        e match {
          case LiteralExpr(l: ListLiteral) => writeListLiteral(l, mv, scope, localAccess, listLevel - 1)
          case _ => writeExpr(e, mv, scope, localAccess)
        }

        toJVMType(returnType(e, scope)(context.st)) match {
          case "I" => mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;");
          case _ => // TODO implement more
        }
        mv.visitInsn(AASTORE);
    }
  }

  def writeGlobalStores()(implicit context: CodeGenContext) =
    context.st.globalStores.stores.foreach(s => context.cw.visitField(ACC_PRIVATE + ACC_STATIC, s.typedIdent.i.value, toJVMType(s.typedIdent.t).toString, null, null))

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

  def writeListCons(mv: MethodVisitor)(implicit scope: CodeGenContext) {

  }

  def writeCheckNotEquals(mv: MethodVisitor)(implicit scope: CodeGenContext) {
    val returnTrue = new Label();
    val end = new Label();
    mv.visitJumpInsn(IF_ICMPNE, returnTrue)
    mv.visitInsn(ICONST_0)
    mv.visitJumpInsn(GOTO, end)
    mv.visitLabel(returnTrue)
    mv.visitInsn(ICONST_1)
    mv.visitLabel(end)
  }

  def writeCheckEquals(mv: MethodVisitor)(implicit scope: CodeGenContext) {
    val returnFalse = new Label();
    val end = new Label();
    mv.visitJumpInsn(IF_ICMPNE, returnFalse)
    mv.visitInsn(ICONST_1)
    mv.visitJumpInsn(GOTO, end)
    mv.visitLabel(returnFalse)
    mv.visitInsn(ICONST_0)
    mv.visitLabel(end)
  }

  def toJVMType(t: Type): String = t match {
    case IntType => "I"
    case BoolType => "Z"
    case ListType(t) => "[Ljava/lang/Object;"
    case Any => "Ljava/lang/Object;"
    //    case ListType(t) => "[" + toJVMType(t)
    //    case Any => throw new IllegalStateException
  }
}