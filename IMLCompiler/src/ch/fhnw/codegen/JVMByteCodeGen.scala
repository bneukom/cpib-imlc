package ch.fhnw.codegen

import ch.fhnw.imlcompiler.AST._
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.MethodVisitor
import java.io.FileOutputStream
import ch.fhnw.imlcompiler.checking.SymbolTable
import scala.collection.mutable.ListBuffer
import ch.fhnw.imlcompiler.checking.Store
import ch.fhnw.imlcompiler.checking.ContextChecker
import org.objectweb.asm.Label
import ch.fhnw.imlcompiler.checking.Scope
import ch.fhnw.imlcompiler.checking.Store

// TODO create internal write int method which always chooses the best one ICONST_1 BIPISH etc
trait JVMByteCodeGen extends ContextChecker {

  class CodeGenContext(val cw: ClassWriter, val prog: Program, val st: SymbolTable)

  def writeCode(prog: Program, st: SymbolTable, path:String) = {
    val classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
    val context = new CodeGenContext(classWriter, prog, st);

    classWriter.visitSource(prog.name.value, null);

    writeGlobalStores()(context)
    writeClass()(context)
    writeConstructor()(context)
    writeRoutines()(context)
    writeMainProgramm()(context)
    writeDeepCopy()(context)
    writeCons()(context)
    writeTail()(context);

    val fileWriter = new FileOutputStream(path + "\\" +  prog.name.value + ".class")
    fileWriter.write(classWriter.toByteArray())
    fileWriter.close
  }

  def writeCommands(cmds: List[Cmd], mv: MethodVisitor, scope: Scope, insideRoutine: Boolean)(implicit context: CodeGenContext) = cmds.foreach(writeCommand(_, mv, scope, insideRoutine))
  def writeCommand(cmd: Cmd, mv: MethodVisitor, scope: Scope, insideRoutine: Boolean)(implicit context: CodeGenContext) {
    cmd match {
      case c: BecomesCmd => writeBecomesCmd(c, mv, scope, insideRoutine);
      case c: OutputCmd => writeOutputCmd(c, mv, scope, insideRoutine);
      case c: InputCmd => writeInputCmd(c, mv, scope, insideRoutine);
      case c: IfCmd => writeIfCmd(c, mv, scope, insideRoutine)
      case c: SkipCmd => mv.visitInsn(NOP)
      case c: WhileCmd => writeWhileCmd(c, mv, scope, insideRoutine);
      case c: CallCmd => writeCallCmd(c, mv, scope, insideRoutine);
    }
  }

  def writeWhileCmd(w: WhileCmd, mv: MethodVisitor, scope: Scope, insideRoutine: Boolean)(implicit context: CodeGenContext) = {
    val beforeExpr = new Label()
    val loop = new Label()

    mv.visitJumpInsn(GOTO, beforeExpr)
    mv.visitLabel(loop)
    writeCommands(w.cmd, mv, scope, insideRoutine)
    mv.visitLabel(beforeExpr)
    writeExpr(w.expr, mv, scope, insideRoutine)
    mv.visitJumpInsn(IFNE, loop)
  }

  def writeIfCmd(o: IfCmd, mv: MethodVisitor, scope: Scope, insideRoutine: Boolean)(implicit context: CodeGenContext) = {
    writeExpr(o.expr, mv, scope, insideRoutine)

    val l1 = new Label()
    val end = new Label()

    mv.visitJumpInsn(IFEQ, l1)
    writeCommands(o.ifCmd, mv, scope, insideRoutine)
    mv.visitJumpInsn(GOTO, end)
    mv.visitLabel(l1)
    writeCommands(o.elseCmd, mv, scope, insideRoutine)
    mv.visitLabel(end)
  }

  def writeOutputCmd(o: OutputCmd, mv: MethodVisitor, scope: Scope, insideRoutine: Boolean)(implicit context: CodeGenContext) = {

    val retType = returnType(o.expr, scope)(context.st);
    retType match {
      case l: ListType => {
        // use the deepToString library method
        mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
        writeExpr(o.expr, mv, scope, insideRoutine)
        mv.visitMethodInsn(INVOKESTATIC, "java/util/Arrays", "deepToString", "([Ljava/lang/Object;)Ljava/lang/String;");
        mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
      }
      case _ => {
        mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
        writeExpr(o.expr, mv, scope, insideRoutine)
        mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(" + toJVMType(returnType(o.expr, scope)(context.st)) + ")V");
      }
    }

  }

  def writeInputCmd(o: InputCmd, mv: MethodVisitor, scope: Scope, insideRoutine: Boolean)(implicit context: CodeGenContext) = {
    o.expr match {
      case s: StoreExpr => {
        val store = scope.get(s.i)

        val writeInput = () => {
          mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
          mv.visitLdcInsn("enter value for store " + store.typedIdent.i.value + ":" + store.typedIdent.t);
          mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");

          mv.visitTypeInsn(NEW, "java/util/Scanner");
          mv.visitInsn(DUP);
          mv.visitFieldInsn(GETSTATIC, "java/lang/System", "in", "Ljava/io/InputStream;");
          mv.visitMethodInsn(INVOKESPECIAL, "java/util/Scanner", "<init>", "(Ljava/io/InputStream;)V");

          store.typedIdent.t match {
            case IntType => mv.visitMethodInsn(INVOKEVIRTUAL, "java/util/Scanner", "nextInt", "()I");
            case BoolType => mv.visitMethodInsn(INVOKEVIRTUAL, "java/util/Scanner", "nextBoolean", "()Z");
            case _ => throw new IllegalStateException("tbd"); // TODO implement lists
          }
        }

        writeStoreAssignment(store.typedIdent.i, writeInput, mv, scope, insideRoutine)
      }
      case _ => throw new IllegalStateException;
    }
  }

  //  def writeExprs(exprs: List[Expr], mv: MethodVisitor, scope: ListBuffer[Store], insideRoutine: Boolean = false)(implicit context: CodeGenContext) = exprs.foreach(writeExpr(_, mv, scope, insideRoutine))
  def writeExpr(expr: Expr, mv: MethodVisitor, scope: Scope, insideRoutine: Boolean)(implicit context: CodeGenContext) = {
    expr match {
      case e: StoreExpr => writeStoreRead(e.i, mv, scope, insideRoutine)
      case d: DyadicExpr => writeDyadicExpr(d, mv, scope, insideRoutine)
      case m: MonadicExpr => writeMonadicExpr(m, mv, scope, insideRoutine)
      case l: LiteralExpr => writeLiteralExpr(l, mv, scope, insideRoutine)
      case f: FunCallExpr => writeFunCallExpr(f, mv, scope, insideRoutine);
      case _ => throw new IllegalStateException
    }
  }

  // store read
  def writeStoreRead(ident: Ident, mv: MethodVisitor, scope: Scope, insideRoutine: Boolean)(implicit context: CodeGenContext) {
    val store = scope.find(_.typedIdent.i == ident).get;
    val imlType = store.typedIdent.t;
    val vmType = toJVMType(imlType);

    if ((insideRoutine || store.synthetic) && !store.globImp) { // inside method or synthetic store access
      val index = if (insideRoutine) scope.stores.filter(!_.globImp).indexWhere(_.typedIdent.i == ident) else scope.stores.filter(_.synthetic).indexWhere(_.typedIdent.i == ident) + 1; // + 1 beacuse main param arguments

      store.flow match {
        case Some(Out) | Some(InOut) => {
          mv.visitVarInsn(ALOAD, index);
          mv.visitInsn(ICONST_0);

          imlType match {
            case IntType | BoolType => mv.visitInsn(IALOAD);
            case _ => mv.visitInsn(AALOAD); mv.visitTypeInsn(CHECKCAST, "[Ljava/lang/Object;");
          }
        }
        case _ => {
          imlType match {
            case IntType | BoolType => mv.visitVarInsn(ILOAD, index);
            case _ => mv.visitVarInsn(ALOAD, index);
          }
        }
      }

    } else { // either access via glob import or inside main method

      mv.visitFieldInsn(GETSTATIC, context.prog.name.value, ident.value, vmType)
    }
  }

  // store write
  def writeStoreAssignment(i: Ident, write: () => Unit, mv: MethodVisitor, scope: Scope, insideRoutine: Boolean)(implicit context: CodeGenContext) = {
    val store = scope.find(_.typedIdent.i == i).get;
    val imlType = store.typedIdent.t
    if ((insideRoutine || store.synthetic) && !store.globImp) {
      val index = if (insideRoutine) scope.stores.filter(!_.globImp).indexWhere(_.typedIdent.i == i) else scope.stores.filter(_.synthetic).indexWhere(_.typedIdent.i == i) + 1; // + 1 beacuse main param arguments

      store.flow match {
        case Some(Out) | Some(InOut) => {
          mv.visitVarInsn(ALOAD, index);
          mv.visitInsn(ICONST_0);
          write();
          imlType match {
            case IntType | BoolType => mv.visitInsn(IASTORE);
            case _ => mv.visitTypeInsn(CHECKCAST, "[Ljava/lang/Object;"); mv.visitInsn(AASTORE);
          }
        }
        case _ => {
          write();
          imlType match {
            case IntType | BoolType => mv.visitVarInsn(ISTORE, index);
            case _ => mv.visitVarInsn(ASTORE, index);
          }
        }
      }

    } else {
      write();
      mv.visitFieldInsn(PUTSTATIC, context.prog.name.value, i.value, toJVMType(imlType));
    }
  }

  def writeBecomesCmd(b: BecomesCmd, mv: MethodVisitor, scope: Scope, insideRoutine: Boolean)(implicit context: CodeGenContext) = {
    val writeRhsExpr = () => writeExpr(b.rhs, mv, scope, insideRoutine);
    b.lhs match {
      case e: StoreExpr => writeStoreAssignment(e.i, writeRhsExpr, mv, scope, insideRoutine)
      case _ => throw new IllegalStateException
    }
  }

  def writeDyadicExpr(dyadicExpr: DyadicExpr, mv: MethodVisitor, scope: Scope, insideRoutine: Boolean)(implicit context: CodeGenContext) {

    dyadicExpr.op match {
      case PlusOpr =>
        writeExpr(dyadicExpr.l, mv, scope, insideRoutine); writeExpr(dyadicExpr.r, mv, scope, insideRoutine); mv.visitInsn(IADD)
      case MinusOpr =>
        writeExpr(dyadicExpr.l, mv, scope, insideRoutine); writeExpr(dyadicExpr.r, mv, scope, insideRoutine); mv.visitInsn(ISUB)
      case DivOpr =>
        writeExpr(dyadicExpr.l, mv, scope, insideRoutine); writeExpr(dyadicExpr.r, mv, scope, insideRoutine); mv.visitInsn(IDIV)
      case TimesOpr =>
        writeExpr(dyadicExpr.l, mv, scope, insideRoutine); writeExpr(dyadicExpr.r, mv, scope, insideRoutine); mv.visitInsn(IMUL)
      case ModOpr =>
        writeExpr(dyadicExpr.l, mv, scope, insideRoutine); writeExpr(dyadicExpr.r, mv, scope, insideRoutine); mv.visitInsn(IREM)
      case EQ =>
        writeExpr(dyadicExpr.l, mv, scope, insideRoutine); writeExpr(dyadicExpr.r, mv, scope, insideRoutine); writeCheckEquals(mv);
      case LT =>
        writeExpr(dyadicExpr.l, mv, scope, insideRoutine); writeExpr(dyadicExpr.r, mv, scope, insideRoutine); writeCheckLessThan(mv);
      case LE =>
        writeExpr(dyadicExpr.l, mv, scope, insideRoutine); writeExpr(dyadicExpr.r, mv, scope, insideRoutine); writeCheckLessEquals(mv);
      case GT =>
        writeExpr(dyadicExpr.l, mv, scope, insideRoutine); writeExpr(dyadicExpr.r, mv, scope, insideRoutine); writeCheckGreaterThan(mv);
      case GE =>
        writeExpr(dyadicExpr.l, mv, scope, insideRoutine); writeExpr(dyadicExpr.r, mv, scope, insideRoutine); writeCheckGreaterEquals(mv);
      case ConsOpr =>
        writeListCons(dyadicExpr, mv, scope, insideRoutine);
      case Cor => writeCondOr(mv, dyadicExpr.l, dyadicExpr.r, scope, insideRoutine)
      case Cand => writeCondAnd(mv, dyadicExpr.l, dyadicExpr.r, scope, insideRoutine)
    }
  }

  def writeMonadicExpr(monadicExpr: MonadicExpr, mv: MethodVisitor, scope: Scope, insideRoutine: Boolean)(implicit context: CodeGenContext) {
    monadicExpr.op match {
      case LengthOpr => writeExpr(monadicExpr.l, mv, scope, insideRoutine); mv.visitInsn(ARRAYLENGTH);
      case HeadOpr => writeHeadOpr(monadicExpr, mv, scope, insideRoutine)
      case TailOpr => writeTailOpr(monadicExpr, mv, scope, insideRoutine)
      case MinusOpr => writeExpr(monadicExpr.l, mv, scope, insideRoutine); mv.visitInsn(INEG)
    }
  }

  def writeLiteralExpr(l: LiteralExpr, mv: MethodVisitor, scope: Scope, insideRoutine: Boolean, listLev: Int = 0)(implicit context: CodeGenContext) {
    l.l match {
      case BoolLiteral(b) =>
        mv.visitIntInsn(BIPUSH, if (b) 1 else 0)
      case IntLiteral(v) =>
        mv.visitLdcInsn(new Integer(v));
      case l: ListLiteral => {
        val retType = returnType(l, scope)(context.st);
        writeListLiteral(l, mv, scope, insideRoutine, listLevel(retType) - 1);
      }
    }
  }

  // TODO pretty much the same as writeCallCmd
  def writeFunCallExpr(f: FunCallExpr, mv: MethodVisitor, scope: Scope, insideRoutine: Boolean, listLev: Int = 0)(implicit context: CodeGenContext) {
    val routineDecl = context.st.routines.find(_._1 == f.i).get._2

    // write parameters to stack
    f.e.l.foreach(writeExpr(_, mv, scope, insideRoutine))

    // call function
    routineDecl match {
      case f: FunDecl => {
        val paramRet = "(" + f.params.map(s => toJVMType(s.ti.t)).mkString("") + ")" + toJVMType(f.returns.ti.t)

        mv.visitMethodInsn(INVOKESTATIC, context.prog.name.value, f.ident.value, paramRet);
      }
      case _ => throw new IllegalStateException
    }
  }

  def writeCallCmd(f: CallCmd, mv: MethodVisitor, scope: Scope, insideRoutine: Boolean, listLev: Int = 0)(implicit context: CodeGenContext) {
    val routineDecl = context.st.routines.find(_._1 == f.i).get._2.asInstanceOf[ProcDecl]
    val localEnd = if (insideRoutine) scope.stores.filter(!_.globImp).length else 1 // 1 beacuse of main string[] arguments

    // write parameters onto stack (out/inout need to be treated diferently)
    setupProcParams(f, mv, scope, insideRoutine, routineDecl, localEnd)

    // load all procedure parameteres onto stack
    loadProcParams(f, mv, scope, insideRoutine, routineDecl, localEnd)

    // call function
    routineDecl match {
      case f: ProcDecl => {
        val paramRet = getJVMProcParams(f);

        mv.visitMethodInsn(INVOKESTATIC, context.prog.name.value, f.ident.value, paramRet);
      }
      case _ => throw new IllegalStateException
    }

    // restore the out parameters
    restoreOutParams(f, mv, scope, insideRoutine, routineDecl, localEnd)
  }

  private def setupProcParams(f: CallCmd, mv: MethodVisitor, scope: Scope, insideRoutine: Boolean, routineDecl: ProcDecl, localEnd: Int)(implicit context: CodeGenContext): Unit = {
    f.e.l.foldLeft((0, localEnd))((a, e) => e match {
      case StoreExpr(ident, isInit) => {
        mv.visitInsn(ICONST_1);
        routineDecl.params(a._1).ti.t match {
          case IntType => mv.visitIntInsn(NEWARRAY, T_INT);
          case BoolType => mv.visitIntInsn(NEWARRAY, T_BOOLEAN)
          case ListType(_) => mv.visitTypeInsn(ANEWARRAY, "java/lang/Object");
          case _ => throw new IllegalStateException
        }
        mv.visitVarInsn(ASTORE, a._2);

        routineDecl.params(a._1) match {
          case Parameter(Some(Out), _, _, t) => (a._1 + 1, a._2 + 1)
          case Parameter(Some(InOut), _, _, t) => {
            mv.visitVarInsn(ALOAD, a._2);
            mv.visitInsn(ICONST_0);
            writeStoreRead(ident, mv, scope, insideRoutine)

            t.t match {
              case IntType | BoolType => mv.visitInsn(IASTORE);
              case _ => mv.visitInsn(AASTORE);
            }

            (a._1 + 1, a._2 + 1)
          }
          case _ => (a._1 + 1, a._2)
        }
      }
      case e => (a._1 + 1, a._2)
    })
  }

  private def loadProcParams(f: CallCmd, mv: MethodVisitor, scope: Scope, insideRoutine: Boolean, routineDecl: ProcDecl, localEnd: Int)(implicit context: CodeGenContext): (Int, Int) = {
    f.e.l.foldLeft((0, localEnd))((a, e) => e match {
      case StoreExpr(ident, isInit) => {
        routineDecl.params(a._1) match {
          case Parameter(Some(InOut | Out), _, _, t) => {
            mv.visitVarInsn(ALOAD, a._2);
            (a._1 + 1, a._2 + 1)
          }
          case _ => {
            writeExpr(e, mv, scope, insideRoutine)
            (a._1 + 1, a._2)
          }
        }
      }
      case e => {
        writeExpr(e, mv, scope, insideRoutine)
        (a._1 + 1, a._2)
      }
    })
  }

  private def restoreOutParams(f: CallCmd, mv: MethodVisitor, scope: Scope, insideRoutine: Boolean, routineDecl: ProcDecl, localEnd: Int)(implicit context: CodeGenContext): Unit = {
    f.e.l.foldLeft((0, localEnd))((a, e) => {
      e match {
        case StoreExpr(ident, isInit) => {
          routineDecl.params(a._1) match {
            case Parameter(Some(InOut | Out), _, _, t) => {
              val write = () => {
                mv.visitVarInsn(ALOAD, a._2);
                mv.visitInsn(ICONST_0);

                t.t match {
                  case IntType | BoolType => mv.visitInsn(IALOAD);
                  case _ => mv.visitInsn(AALOAD); mv.visitTypeInsn(CHECKCAST, "[Ljava/lang/Object;");
                }
              }

              writeStoreAssignment(ident, write, mv, scope, insideRoutine)

              (a._1 + 1, a._2 + 1)
            }
            case _ => (a._1 + 1, a._2)
          }
        }
        case e => (a._1 + 1, a._2)
      }
    })
  }

  def writeListLiteral(l: ListLiteral, mv: MethodVisitor, scope: Scope, insideRoutine: Boolean, listLevel: Int)(implicit context: CodeGenContext) {
    mv.visitIntInsn(BIPUSH, l.l.size) // write array length

    val t = returnType(l, scope)(context.st);
    val arrType = if (listLevel <= 1) "java/lang/Object" else ("[" * listLevel) + "Ljava/lang/Object;";

    mv.visitTypeInsn(ANEWARRAY, arrType); // type of array must be [Object for a list of [[Object

    l.l.zipWithIndex.foreach {
      case (e, i) =>
        mv.visitInsn(DUP);
        mv.visitIntInsn(BIPUSH, i);

        e match {
          case LiteralExpr(l: ListLiteral) => writeListLiteral(l, mv, scope, insideRoutine, listLevel - 1)
          case _ => writeExpr(e, mv, scope, insideRoutine)
        }

        toJVMType(returnType(e, scope)(context.st)) match {
          case "I" => mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;");
          case _ => // TODO implement more
        }
        mv.visitInsn(AASTORE);
    }
  }

  def writeGlobalStores()(implicit context: CodeGenContext) =
    context.st.globalScope.stores.filter(!_.synthetic).foreach(s => context.cw.visitField(ACC_PRIVATE + ACC_STATIC, s.typedIdent.i.value, toJVMType(s.typedIdent.t).toString, null, null))

  def writeClass()(implicit context: CodeGenContext) {
    context.cw.visit(V1_7, ACC_PUBLIC + ACC_SUPER, context.prog.name.value, null, "java/lang/Object", null);
  }

  def writeCons()(implicit context: CodeGenContext) {
    val mv = context.cw.visitMethod(ACC_PRIVATE + ACC_STATIC, "$cons", "([Ljava/lang/Object;)[Ljava/lang/Object;", null, null);
    mv.visitCode();
    val l0 = new Label();
    mv.visitLabel(l0);
    mv.visitVarInsn(ALOAD, 0);
    mv.visitInsn(ARRAYLENGTH);
    mv.visitInsn(ICONST_1);
    mv.visitInsn(IADD);
    mv.visitTypeInsn(ANEWARRAY, "java/lang/Object");
    mv.visitVarInsn(ASTORE, 1);
    val l1 = new Label();
    mv.visitLabel(l1);
    mv.visitInsn(ICONST_0);
    mv.visitVarInsn(ISTORE, 2);
    val l2 = new Label();
    mv.visitLabel(l2);
    val l3 = new Label();
    mv.visitJumpInsn(GOTO, l3);
    val l4 = new Label();
    mv.visitLabel(l4);
    mv.visitVarInsn(ALOAD, 0);
    mv.visitVarInsn(ILOAD, 2);
    mv.visitInsn(AALOAD);
    mv.visitVarInsn(ASTORE, 3);
    val l5 = new Label();
    mv.visitLabel(l5);
    mv.visitVarInsn(ALOAD, 3);
    mv.visitTypeInsn(INSTANCEOF, "[Ljava/lang/Object;");
    val l6 = new Label();
    mv.visitJumpInsn(IFEQ, l6);
    val l7 = new Label();
    mv.visitLabel(l7);
    mv.visitVarInsn(ALOAD, 1);
    mv.visitVarInsn(ILOAD, 2);
    mv.visitInsn(ICONST_1);
    mv.visitInsn(IADD);
    mv.visitVarInsn(ALOAD, 3);
    mv.visitMethodInsn(INVOKESTATIC, context.prog.name.value, "$deepCopy", "(Ljava/lang/Object;)Ljava/lang/Object;");
    mv.visitInsn(AASTORE);
    val l8 = new Label();
    mv.visitJumpInsn(GOTO, l8);
    mv.visitLabel(l6);
    mv.visitVarInsn(ALOAD, 1);
    mv.visitVarInsn(ILOAD, 2);
    mv.visitInsn(ICONST_1);
    mv.visitInsn(IADD);
    mv.visitVarInsn(ALOAD, 3);
    mv.visitInsn(AASTORE);
    mv.visitLabel(l8);
    mv.visitFrame(F_CHOP, 1, null, 0, null);
    mv.visitIincInsn(2, 1);
    mv.visitLabel(l3);
    mv.visitFrame(F_SAME, 0, null, 0, null);
    mv.visitVarInsn(ILOAD, 2);
    mv.visitVarInsn(ALOAD, 0);
    mv.visitInsn(ARRAYLENGTH);
    mv.visitJumpInsn(IF_ICMPLT, l4);
    val l9 = new Label();
    mv.visitLabel(l9);
    mv.visitVarInsn(ALOAD, 1);
    mv.visitInsn(ARETURN);
    val l10 = new Label();
    mv.visitLabel(l10);
    mv.visitLocalVariable("o", "[Ljava/lang/Object;", null, l0, l10, 0);
    mv.visitLocalVariable("result", "[Ljava/lang/Object;", null, l1, l10, 1);
    mv.visitLocalVariable("i", "I", null, l2, l9, 2);
    mv.visitLocalVariable("object", "Ljava/lang/Object;", null, l5, l8, 3);
    mv.visitMaxs(3, 4);
    mv.visitEnd();
  }

  def writeTail()(implicit context: CodeGenContext) {
    val mv = context.cw.visitMethod(ACC_PRIVATE + ACC_STATIC, "$tail", "([Ljava/lang/Object;)[Ljava/lang/Object;", null, null);
    mv.visitCode();
    val l0 = new Label();
    mv.visitLabel(l0);
    mv.visitVarInsn(ALOAD, 0);
    mv.visitInsn(ARRAYLENGTH);
    mv.visitInsn(ICONST_1);
    mv.visitInsn(ISUB);
    mv.visitTypeInsn(ANEWARRAY, "java/lang/Object");
    mv.visitVarInsn(ASTORE, 1);
    val l1 = new Label();
    mv.visitLabel(l1);
    mv.visitInsn(ICONST_1);
    mv.visitVarInsn(ISTORE, 2);
    val l2 = new Label();
    mv.visitLabel(l2);
    val l3 = new Label();
    mv.visitJumpInsn(GOTO, l3);
    val l4 = new Label();
    mv.visitLabel(l4);
    mv.visitVarInsn(ALOAD, 0);
    mv.visitVarInsn(ILOAD, 2);
    mv.visitInsn(AALOAD);
    mv.visitVarInsn(ASTORE, 3);
    val l5 = new Label();
    mv.visitLabel(l5);
    mv.visitVarInsn(ALOAD, 3);
    mv.visitTypeInsn(INSTANCEOF, "[Ljava/lang/Object;");
    val l6 = new Label();
    mv.visitJumpInsn(IFEQ, l6);
    val l7 = new Label();
    mv.visitLabel(l7);
    mv.visitVarInsn(ALOAD, 1);
    mv.visitVarInsn(ILOAD, 2);
    mv.visitInsn(ICONST_1);
    mv.visitInsn(ISUB);
    mv.visitVarInsn(ALOAD, 3);
    mv.visitMethodInsn(INVOKESTATIC, context.prog.name.value, "$deepCopy", "(Ljava/lang/Object;)Ljava/lang/Object;");
    mv.visitInsn(AASTORE);
    val l8 = new Label();
    mv.visitJumpInsn(GOTO, l8);
    mv.visitLabel(l6);
    mv.visitVarInsn(ALOAD, 1);
    mv.visitVarInsn(ILOAD, 2);
    mv.visitInsn(ICONST_1);
    mv.visitInsn(ISUB);
    mv.visitVarInsn(ALOAD, 3);
    mv.visitInsn(AASTORE);
    mv.visitLabel(l8);
    mv.visitFrame(F_CHOP, 1, null, 0, null);
    mv.visitIincInsn(2, 1);
    mv.visitLabel(l3);
    mv.visitFrame(F_SAME, 0, null, 0, null);
    mv.visitVarInsn(ILOAD, 2);
    mv.visitVarInsn(ALOAD, 0);
    mv.visitInsn(ARRAYLENGTH);
    mv.visitJumpInsn(IF_ICMPLT, l4);
    val l9 = new Label();
    mv.visitLabel(l9);
    mv.visitVarInsn(ALOAD, 1);
    mv.visitInsn(ARETURN);
    val l10 = new Label();
    mv.visitLabel(l10);
    mv.visitLocalVariable("o", "[Ljava/lang/Object;", null, l0, l10, 0);
    mv.visitLocalVariable("result", "[Ljava/lang/Object;", null, l1, l10, 1);
    mv.visitLocalVariable("i", "I", null, l2, l9, 2);
    mv.visitLocalVariable("object", "Ljava/lang/Object;", null, l5, l8, 3);
    mv.visitMaxs(3, 4);
    mv.visitEnd();
  }

  def writeDeepCopy()(implicit context: CodeGenContext) {
    val mv = context.cw.visitMethod(ACC_PRIVATE + ACC_STATIC, "$deepCopy", "(Ljava/lang/Object;)Ljava/lang/Object;", null, null);
    mv.visitCode();
    def l0 = new Label();
    mv.visitLabel(l0);
    mv.visitVarInsn(ALOAD, 0);
    mv.visitTypeInsn(INSTANCEOF, "[Ljava/lang/Object;");
    val l1 = new Label();
    mv.visitJumpInsn(IFEQ, l1);
    val l2 = new Label();
    mv.visitLabel(l2);
    mv.visitVarInsn(ALOAD, 0);
    mv.visitTypeInsn(CHECKCAST, "[Ljava/lang/Object;");
    mv.visitVarInsn(ASTORE, 1);
    val l3 = new Label();
    mv.visitLabel(l3);
    mv.visitVarInsn(ALOAD, 1);
    mv.visitInsn(ARRAYLENGTH);
    mv.visitTypeInsn(ANEWARRAY, "java/lang/Object");
    mv.visitVarInsn(ASTORE, 2);
    val l4 = new Label();
    mv.visitLabel(l4);
    mv.visitInsn(ICONST_0);
    mv.visitVarInsn(ISTORE, 3);
    val l5 = new Label();
    mv.visitLabel(l5);
    val l6 = new Label();
    mv.visitJumpInsn(GOTO, l6);
    val l7 = new Label();
    mv.visitLabel(l7);
    mv.visitVarInsn(ALOAD, 1);
    mv.visitVarInsn(ILOAD, 3);
    mv.visitInsn(AALOAD);
    mv.visitVarInsn(ASTORE, 4);
    val l8 = new Label();
    mv.visitLabel(l8);
    mv.visitVarInsn(ALOAD, 4);
    mv.visitTypeInsn(INSTANCEOF, "[Ljava/lang/Object;");
    val l9 = new Label();
    mv.visitJumpInsn(IFEQ, l9);
    val l10 = new Label();
    mv.visitLabel(l10);
    mv.visitVarInsn(ALOAD, 2);
    mv.visitVarInsn(ILOAD, 3);
    mv.visitVarInsn(ALOAD, 4);
    mv.visitMethodInsn(INVOKESTATIC, context.prog.name.value, "$deepCopy", "(Ljava/lang/Object;)Ljava/lang/Object;");
    mv.visitInsn(AASTORE);
    val l11 = new Label();
    mv.visitJumpInsn(GOTO, l11);
    mv.visitLabel(l9);
    mv.visitVarInsn(ALOAD, 2);
    mv.visitVarInsn(ILOAD, 3);
    mv.visitVarInsn(ALOAD, 4);
    mv.visitInsn(AASTORE);
    mv.visitLabel(l11);
    mv.visitFrame(F_CHOP, 1, null, 0, null);
    mv.visitIincInsn(3, 1);
    mv.visitLabel(l6);
    mv.visitFrame(F_SAME, 0, null, 0, null);
    mv.visitVarInsn(ILOAD, 3);
    mv.visitVarInsn(ALOAD, 1);
    mv.visitInsn(ARRAYLENGTH);
    mv.visitJumpInsn(IF_ICMPLT, l7);
    val l12 = new Label();
    mv.visitLabel(l12);
    mv.visitVarInsn(ALOAD, 2);
    mv.visitInsn(ARETURN);
    mv.visitLabel(l1);
    mv.visitFrame(F_CHOP, 3, null, 0, null);
    mv.visitVarInsn(ALOAD, 0);
    mv.visitInsn(ARETURN);
    val l13 = new Label();
    mv.visitLabel(l13);
    mv.visitLocalVariable("o", "Ljava/lang/Object;", null, l0, l13, 0);
    mv.visitLocalVariable("l", "[Ljava/lang/Object;", null, l3, l1, 1);
    mv.visitLocalVariable("result", "[Ljava/lang/Object;", null, l4, l1, 2);
    mv.visitLocalVariable("i", "I", null, l5, l12, 3);
    mv.visitLocalVariable("object", "Ljava/lang/Object;", null, l8, l11, 4);
    mv.visitMaxs(3, 5);
    mv.visitEnd();
  }

  def writeConstructor()(implicit context: CodeGenContext) {
    val constructor = context.cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
    constructor.visitVarInsn(ALOAD, 0);
    constructor.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V");
    constructor.visitInsn(RETURN);
    constructor.visitMaxs(0, 0);
    constructor.visitEnd();
  }

  def writeFunction(f: FunDecl)(implicit context: CodeGenContext) {
    val scope = context.st.getLocalStoreScope(f.ident);
    val paramRet = "(" + f.params.map(s => toJVMType(s.ti.t)).mkString("") + ")" + toJVMType(f.returns.ti.t)
    val fun = context.cw.visitMethod(ACC_PRIVATE + ACC_STATIC, f.ident.value, paramRet, null, null);

    writeCommands(f.cmds, fun, scope, true)

    // load return value onto stack
    writeStoreRead(f.returns.ti.i, fun, scope, true)

    f.returns.ti.t match {
      case IntType | BoolType => fun.visitInsn(IRETURN);
      case _ => fun.visitInsn(ARETURN);
    }

    fun.visitMaxs(0, 0); // ignore ignore
    fun.visitEnd();
  }

  def writeProcedure(f: ProcDecl)(implicit context: CodeGenContext) {
    val scope = context.st.getLocalStoreScope(f.ident);
    val paramRet = getJVMProcParams(f);
    val proc = context.cw.visitMethod(ACC_PRIVATE + ACC_STATIC, f.ident.value, paramRet, null, null);

    writeCommands(f.cmds, proc, scope, true)

    proc.visitInsn(RETURN);
    proc.visitMaxs(0, 0); // ignore ignore
    proc.visitEnd();
  }

  def writeRoutines()(implicit context: CodeGenContext) {

    context.prog.cpsDecl.foreach(r => r match {
      case f: FunDecl => writeFunction(f);
      case p: ProcDecl => writeProcedure(p);
      case _ => // TODO impl
    })
  }

  def writeMainProgramm()(implicit context: CodeGenContext) {
    val main = context.cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null);
    main.visitVarInsn(ALOAD, 0);

    writeCommands(context.prog.commands, main, context.st.globalScope, false);

    main.visitMaxs(0, 0); // ignore ignore
    main.visitInsn(RETURN);
    main.visitEnd();
  }

  def writeListCons(dyadicExpr: DyadicExpr, mv: MethodVisitor, scope: Scope, insideRoutine: Boolean)(implicit context: CodeGenContext) {

    writeExpr(dyadicExpr.r, mv, scope, insideRoutine);
    mv.visitMethodInsn(INVOKESTATIC, context.prog.name.value, "$cons", "([Ljava/lang/Object;)[Ljava/lang/Object;");
    mv.visitInsn(DUP)

    mv.visitInsn(ICONST_0);

    writeExpr(dyadicExpr.l, mv, scope, insideRoutine);

    val lType = returnType(dyadicExpr.l, scope)(context.st);
    lType match {
      case IntType => mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;");
      case _ => // TODO impl
    }

    mv.visitInsn(AASTORE);
  }

  private def writeHeadOpr(monadicExpr: MonadicExpr, mv: MethodVisitor, scope: Scope, insideRoutine: Boolean)(implicit context: CodeGenContext) = {
    writeExpr(monadicExpr.l, mv, scope, insideRoutine);

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

  private def writeTailOpr(monadicExpr: MonadicExpr, mv: MethodVisitor, scope: Scope, insideRoutine: Boolean)(implicit context: CodeGenContext) = {
    writeExpr(monadicExpr.l, mv, scope, insideRoutine)
    mv.visitMethodInsn(INVOKESTATIC, context.prog.name.value, "$tail", "([Ljava/lang/Object;)[Ljava/lang/Object;");
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

  def writeCheckEquals(mv: MethodVisitor) {
    val returnFalse = new Label();
    val end = new Label();
    mv.visitJumpInsn(IF_ICMPNE, returnFalse)
    mv.visitInsn(ICONST_1)
    mv.visitJumpInsn(GOTO, end)
    mv.visitLabel(returnFalse)
    mv.visitInsn(ICONST_0)
    mv.visitLabel(end)
  }

  def writeCheckLessThan(mv: MethodVisitor) {
    val returnFalse = new Label();
    val end = new Label();
    mv.visitJumpInsn(IF_ICMPGE, returnFalse)
    mv.visitInsn(ICONST_1)
    mv.visitJumpInsn(GOTO, end)
    mv.visitLabel(returnFalse)
    mv.visitInsn(ICONST_0)
    mv.visitLabel(end)
  }

  def writeCheckLessEquals(mv: MethodVisitor) {
    val returnFalse = new Label();
    val end = new Label();
    mv.visitJumpInsn(IF_ICMPGT, returnFalse)
    mv.visitInsn(ICONST_1)
    mv.visitJumpInsn(GOTO, end)
    mv.visitLabel(returnFalse)
    mv.visitInsn(ICONST_0)
    mv.visitLabel(end)
  }

  def writeCheckGreaterThan(mv: MethodVisitor) {
    val returnFalse = new Label();
    val end = new Label();
    mv.visitJumpInsn(IF_ICMPLE, returnFalse)
    mv.visitInsn(ICONST_1)
    mv.visitJumpInsn(GOTO, end)
    mv.visitLabel(returnFalse)
    mv.visitInsn(ICONST_0)
    mv.visitLabel(end)
  }
  def writeCheckGreaterEquals(mv: MethodVisitor) {
    val returnFalse = new Label();
    val end = new Label();
    mv.visitJumpInsn(IF_ICMPLT, returnFalse)
    mv.visitInsn(ICONST_1)
    mv.visitJumpInsn(GOTO, end)
    mv.visitLabel(returnFalse)
    mv.visitInsn(ICONST_0)
    mv.visitLabel(end)
  }

  def writeCondOr(mv: MethodVisitor, e1: Expr, e2: Expr, scope: Scope, insideRoutine: Boolean)(implicit context: CodeGenContext) {
    val returnTrue = new Label();
    val end = new Label();
    writeExpr(e1, mv, scope, insideRoutine)
    mv.visitJumpInsn(IFNE, returnTrue)
    writeExpr(e2, mv, scope, insideRoutine)
    mv.visitJumpInsn(IFNE, returnTrue)
    mv.visitInsn(ICONST_0) // false
    mv.visitJumpInsn(GOTO, end)
    mv.visitLabel(returnTrue)
    mv.visitInsn(ICONST_1) // true
    mv.visitLabel(end)
  }

  def writeCondAnd(mv: MethodVisitor, e1: Expr, e2: Expr, scope: Scope, insideRoutine: Boolean)(implicit context: CodeGenContext) {
    val returnFalse = new Label();
    val end = new Label();
    writeExpr(e1, mv, scope, insideRoutine)
    mv.visitJumpInsn(IFEQ, returnFalse)
    writeExpr(e2, mv, scope, insideRoutine)
    mv.visitJumpInsn(IFEQ, returnFalse)
    mv.visitInsn(ICONST_1) /* return true */
    mv.visitJumpInsn(GOTO, end)
    mv.visitLabel(returnFalse)
    mv.visitInsn(ICONST_0) /* return false */
    mv.visitLabel(end)
  }

  def toJVMType(t: Type): String = t match {
    case IntType => "I"
    case BoolType => "Z"
    case ListType(t) => "[Ljava/lang/Object;"
    case Any => "Ljava/lang/Object;"
  }

  def getJVMProcParams(f: ProcDecl) = {
    "(" + f.params.map(s => {
      s.f.get match { // TODO is get right? what are default values?
        case In => toJVMType(s.ti.t)
        case _ => s.ti.t match {
          case IntType | BoolType => "[" + toJVMType(s.ti.t);
          case _ => "[Ljava/lang/Object;"
        }
      }
    }).mkString("") + ")V";
  }

}