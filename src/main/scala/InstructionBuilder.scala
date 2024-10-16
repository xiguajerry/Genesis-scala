package net.spartanb312.genesis

import org.objectweb.asm.*
import org.objectweb.asm.tree.*

import scala.collection.mutable
import scala.compiletime.error

class LabelRecorder:
    private val map = mutable.Map[Any, Label]()

    def apply(key: Any): Label =
        if map.contains(key) then map(key)
        else
            val label = Label()
            map.put(key, label)
            label


case class InstructionBuilder(insnList: InsnList):
    val L = LabelRecorder()

    def +(insnList: InsnList): Unit = this.insnList.add(insnList)

    def +(insn: AbstractInsnNode): Unit = this.insnList.add(insn)

    def ~(insnList: InsnList): Unit =
        this.insnList.clear()
        this.insnList.add(insnList)


def L(key: Any)(using insnBuilder: InstructionBuilder): Label = insnBuilder.L(key)
extension(ctx: StringContext) def L(args: Any*)(using insnBuilder: InstructionBuilder): Label =
    insnBuilder.L(ctx.s(args*))


def TRY(builder: InstructionBuilder ?=> Unit)(using insnBuilder: InstructionBuilder): TryCatchBuilder =
    val tryBlock = instructions(builder)
    val startLabel = L("EX_START" + System.currentTimeMillis())
    val endLabel = L("EX_END" + System.currentTimeMillis() + 1000)
    val tryCatchBuilder = TryCatchBuilder(startLabel, endLabel, tryBlock)
    tryCatchBuilder


case class TryCatchBuilder(startLabel: Label, var endLabel: Label, tryBlock: InsnList, handlers: mutable.Buffer[HandlerBlock] = mutable.Buffer()):
    private var built = false

    infix def CATCH(exception: String): TemporaryHandlerBlock =
        if built then throw IllegalStateException("The try-catch statement has been built.")
        TemporaryHandlerBlock(this, exception)

    def unary_+(using insnBuilder: InstructionBuilder)(using methodBuilder: MethodBuilder): Unit =
        if handlers.isEmpty then throw IllegalStateException("No catch blocks have been found.")
        val finallyStart = L("EX_FINALLY" + System.currentTimeMillis() + 3000)
        +startLabel
        +tryBlock
        +endLabel
        GOTO(finallyStart)
        handlers.foreach: handler =>
            +handler.startLabel
            +handler.handlerBlock
            GOTO(finallyStart)
            methodBuilder.methodNode.visitTryCatchBlock(startLabel, endLabel, handler.startLabel, handler.exception)
        +finallyStart
        built = true


case class TemporaryHandlerBlock(tryCatchBuilder: TryCatchBuilder, exception: String):
    infix def apply(builder: InstructionBuilder ?=> Unit)(using insnBuilder: InstructionBuilder): TryCatchBuilder =
        val handlerBlock = instructions(builder)
        val startLabel = L("HANDLER_START" + System.currentTimeMillis() + 2000)
        tryCatchBuilder.handlers.addOne(HandlerBlock(startLabel, handlerBlock, exception))
        tryCatchBuilder


case class HandlerBlock(startLabel: Label, handlerBlock: InsnList, exception: String = "java/lang/Throwable")


def instructions(builder: InstructionBuilder ?=> Unit): InsnList =
    val insnList = InsnList()
    given InstructionBuilder(insnList)
    builder
    insnList


inline def NOP(using InstructionBuilder): Unit = +InsnNode(Opcodes.NOP)

inline def ACONST_NULL(using InstructionBuilder): Unit = +InsnNode(Opcodes.ACONST_NULL)
inline def ICONST_M1(using InstructionBuilder): Unit = +InsnNode(Opcodes.ICONST_M1)
inline def ICONST_0(using InstructionBuilder): Unit = +InsnNode(Opcodes.ICONST_0)
inline def ICONST_1(using InstructionBuilder): Unit = +InsnNode(Opcodes.ICONST_1)
inline def ICONST_2(using InstructionBuilder): Unit = +InsnNode(Opcodes.ICONST_2)
inline def ICONST_3(using InstructionBuilder): Unit = +InsnNode(Opcodes.ICONST_3)
inline def ICONST_4(using InstructionBuilder): Unit = +InsnNode(Opcodes.ICONST_4)
inline def ICONST_5(using InstructionBuilder): Unit = +InsnNode(Opcodes.ICONST_5)
inline def LCONST_0(using InstructionBuilder): Unit = +InsnNode(Opcodes.LCONST_0)
inline def LCONST_1(using InstructionBuilder): Unit = +InsnNode(Opcodes.LCONST_1)
inline def FCONST_0(using InstructionBuilder): Unit = +InsnNode(Opcodes.FCONST_0)
inline def FCONST_1(using InstructionBuilder): Unit = +InsnNode(Opcodes.FCONST_1)
inline def FCONST_2(using InstructionBuilder): Unit = +InsnNode(Opcodes.FCONST_2)
inline def DCONST_0(using InstructionBuilder): Unit = +InsnNode(Opcodes.DCONST_0)
inline def DCONST_1(using InstructionBuilder): Unit = +InsnNode(Opcodes.DCONST_1)

inline def BIPUSH(value: Int)(using InstructionBuilder): Unit = +IntInsnNode(Opcodes.BIPUSH, value)
inline def SIPUSH(value: Int)(using InstructionBuilder): Unit = +IntInsnNode(Opcodes.SIPUSH, value)

inline def LDC[V <: Any](value: V)(using InstructionBuilder): Unit =
    inline value match
        case _: String => +LdcInsnNode(value)
        case _: Int => +LdcInsnNode(value)
        case _: Float => +LdcInsnNode(value)
        case _: Long => +LdcInsnNode(value)
        case _: Double => +LdcInsnNode(value)
        case _: Type => +LdcInsnNode(value)
        case _: Handle => +LdcInsnNode(value)
        case _: ConstantDynamic => +LdcInsnNode(value)
        case _ => error("Unsupported type")

inline def ILOAD(index: Int)(using InstructionBuilder): Unit = +VarInsnNode(Opcodes.ILOAD, index)
inline def LLOAD(index: Int)(using InstructionBuilder): Unit = +VarInsnNode(Opcodes.LLOAD, index)
inline def FLOAD(index: Int)(using InstructionBuilder): Unit = +VarInsnNode(Opcodes.FLOAD, index)
inline def DLOAD(index: Int)(using InstructionBuilder): Unit = +VarInsnNode(Opcodes.DLOAD, index)
inline def ALOAD(index: Int)(using InstructionBuilder): Unit = +VarInsnNode(Opcodes.ALOAD, index)

inline def IALOAD(using InstructionBuilder): Unit = +InsnNode(Opcodes.IALOAD)
inline def LALOAD(using InstructionBuilder): Unit = +InsnNode(Opcodes.LALOAD)
inline def FALOAD(using InstructionBuilder): Unit = +InsnNode(Opcodes.FALOAD)
inline def DALOAD(using InstructionBuilder): Unit = +InsnNode(Opcodes.DALOAD)
inline def AALOAD(using InstructionBuilder): Unit = +InsnNode(Opcodes.AALOAD)
inline def BALOAD(using InstructionBuilder): Unit = +InsnNode(Opcodes.BALOAD)
inline def CALOAD(using InstructionBuilder): Unit = +InsnNode(Opcodes.CALOAD)
inline def SALOAD(using InstructionBuilder): Unit = +InsnNode(Opcodes.SALOAD)

inline def ISTORE(index: Int)(using InstructionBuilder): Unit = +VarInsnNode(Opcodes.ISTORE, index)
inline def LSTORE(index: Int)(using InstructionBuilder): Unit = +VarInsnNode(Opcodes.LSTORE, index)
inline def FSTORE(index: Int)(using InstructionBuilder): Unit = +VarInsnNode(Opcodes.FSTORE, index)
inline def DSTORE(index: Int)(using InstructionBuilder): Unit = +VarInsnNode(Opcodes.DSTORE, index)
inline def ASTORE(index: Int)(using InstructionBuilder): Unit = +VarInsnNode(Opcodes.ASTORE, index)

inline def IASTORE(using InstructionBuilder): Unit = +InsnNode(Opcodes.IASTORE)
inline def LASTORE(using InstructionBuilder): Unit = +InsnNode(Opcodes.LASTORE)
inline def FASTORE(using InstructionBuilder): Unit = +InsnNode(Opcodes.FASTORE)
inline def DASTORE(using InstructionBuilder): Unit = +InsnNode(Opcodes.DASTORE)
inline def AASTORE(using InstructionBuilder): Unit = +InsnNode(Opcodes.AASTORE)
inline def BASTORE(using InstructionBuilder): Unit = +InsnNode(Opcodes.BASTORE)
inline def CASTORE(using InstructionBuilder): Unit = +InsnNode(Opcodes.CASTORE)
inline def SASTORE(using InstructionBuilder): Unit = +InsnNode(Opcodes.SASTORE)

inline def POP(using InstructionBuilder): Unit = +InsnNode(Opcodes.POP)
inline def POP2(using InstructionBuilder): Unit = +InsnNode(Opcodes.POP2)
inline def DUP(using InstructionBuilder): Unit = +InsnNode(Opcodes.DUP)
inline def DUP_X1(using InstructionBuilder): Unit = +InsnNode(Opcodes.DUP_X1)
inline def DUP_X2(using InstructionBuilder): Unit = +InsnNode(Opcodes.DUP_X2)
inline def DUP2(using InstructionBuilder): Unit = +InsnNode(Opcodes.DUP2)
inline def DUP2_X1(using InstructionBuilder): Unit = +InsnNode(Opcodes.DUP2_X1)
inline def DUP2_X2(using InstructionBuilder): Unit = +InsnNode(Opcodes.DUP2_X2)
inline def SWAP(using InstructionBuilder): Unit = +InsnNode(Opcodes.SWAP)

inline def IADD(using InstructionBuilder): Unit = +InsnNode(Opcodes.IADD)
inline def LADD(using InstructionBuilder): Unit = +InsnNode(Opcodes.LADD)
inline def FADD(using InstructionBuilder): Unit = +InsnNode(Opcodes.FADD)
inline def DADD(using InstructionBuilder): Unit = +InsnNode(Opcodes.DADD)

inline def ISUB(using InstructionBuilder): Unit = +InsnNode(Opcodes.ISUB)
inline def LSUB(using InstructionBuilder): Unit = +InsnNode(Opcodes.LSUB)
inline def FSUB(using InstructionBuilder): Unit = +InsnNode(Opcodes.FSUB)
inline def DSUB(using InstructionBuilder): Unit = +InsnNode(Opcodes.DSUB)

inline def IMUL(using InstructionBuilder): Unit = +InsnNode(Opcodes.IMUL)
inline def LMUL(using InstructionBuilder): Unit = +InsnNode(Opcodes.LMUL)
inline def FMUL(using InstructionBuilder): Unit = +InsnNode(Opcodes.FMUL)
inline def DMUL(using InstructionBuilder): Unit = +InsnNode(Opcodes.DMUL)

inline def IDIV(using InstructionBuilder): Unit = +InsnNode(Opcodes.IDIV)
inline def LDIV(using InstructionBuilder): Unit = +InsnNode(Opcodes.LDIV)
inline def FDIV(using InstructionBuilder): Unit = +InsnNode(Opcodes.FDIV)
inline def DDIV(using InstructionBuilder): Unit = +InsnNode(Opcodes.DDIV)

inline def IREM(using InstructionBuilder): Unit = +InsnNode(Opcodes.IREM)
inline def LREM(using InstructionBuilder): Unit = +InsnNode(Opcodes.LREM)
inline def FREM(using InstructionBuilder): Unit = +InsnNode(Opcodes.FREM)
inline def DREM(using InstructionBuilder): Unit = +InsnNode(Opcodes.DREM)

inline def INEG(using InstructionBuilder): Unit = +InsnNode(Opcodes.INEG)
inline def LNEG(using InstructionBuilder): Unit = +InsnNode(Opcodes.LNEG)
inline def FNEG(using InstructionBuilder): Unit = +InsnNode(Opcodes.FNEG)
inline def DNEG(using InstructionBuilder): Unit = +InsnNode(Opcodes.DNEG)

inline def ISHL(using InstructionBuilder): Unit = +InsnNode(Opcodes.ISHL)
inline def LSHL(using InstructionBuilder): Unit = +InsnNode(Opcodes.LSHL)
inline def ISHR(using InstructionBuilder): Unit = +InsnNode(Opcodes.ISHR)
inline def LSHR(using InstructionBuilder): Unit = +InsnNode(Opcodes.LSHR)
inline def IUSHR(using InstructionBuilder): Unit = +InsnNode(Opcodes.IUSHR)
inline def LUSHR(using InstructionBuilder): Unit = +InsnNode(Opcodes.LUSHR)

inline def IAND(using InstructionBuilder): Unit = +InsnNode(Opcodes.IAND)
inline def LAND(using InstructionBuilder): Unit = +InsnNode(Opcodes.LAND)

inline def IOR(using InstructionBuilder): Unit = +InsnNode(Opcodes.IOR)
inline def LOR(using InstructionBuilder): Unit = +InsnNode(Opcodes.LOR)

inline def IXOR(using InstructionBuilder): Unit = +InsnNode(Opcodes.IXOR)
inline def LXOR(using InstructionBuilder): Unit = +InsnNode(Opcodes.LXOR)

inline def IINC(index: Int, incr: Int)(using InstructionBuilder): Unit = +IincInsnNode(index, incr)

inline def I2L(using InstructionBuilder): Unit = +InsnNode(Opcodes.I2L)
inline def I2F(using InstructionBuilder): Unit = +InsnNode(Opcodes.I2F)
inline def I2D(using InstructionBuilder): Unit = +InsnNode(Opcodes.I2D)

inline def L2I(using InstructionBuilder): Unit = +InsnNode(Opcodes.L2I)
inline def L2F(using InstructionBuilder): Unit = +InsnNode(Opcodes.L2F)
inline def L2D(using InstructionBuilder): Unit = +InsnNode(Opcodes.L2D)

inline def F2I(using InstructionBuilder): Unit = +InsnNode(Opcodes.F2I)
inline def F2L(using InstructionBuilder): Unit = +InsnNode(Opcodes.F2L)
inline def F2D(using InstructionBuilder): Unit = +InsnNode(Opcodes.F2D)

inline def D2I(using InstructionBuilder): Unit = +InsnNode(Opcodes.D2I)
inline def D2L(using InstructionBuilder): Unit = +InsnNode(Opcodes.D2L)
inline def D2F(using InstructionBuilder): Unit = +InsnNode(Opcodes.D2F)

inline def I2B(using InstructionBuilder): Unit = +InsnNode(Opcodes.I2B)
inline def I2C(using InstructionBuilder): Unit = +InsnNode(Opcodes.I2C)
inline def I2S(using InstructionBuilder): Unit = +InsnNode(Opcodes.I2S)

inline def LCMP(using InstructionBuilder): Unit = +InsnNode(Opcodes.LCMP)
inline def FCMPL(using InstructionBuilder): Unit = +InsnNode(Opcodes.FCMPL)
inline def FCMPG(using InstructionBuilder): Unit = +InsnNode(Opcodes.FCMPG)
inline def DCMPL(using InstructionBuilder): Unit = +InsnNode(Opcodes.DCMPL)
inline def DCMPG(using InstructionBuilder): Unit = +InsnNode(Opcodes.DCMPG)

inline def IFEQ(label: LabelNode)(using InstructionBuilder): Unit = +JumpInsnNode(Opcodes.IFEQ, label)
inline def IFNE(label: LabelNode)(using InstructionBuilder): Unit = +JumpInsnNode(Opcodes.IFNE, label)
inline def IFLT(label: LabelNode)(using InstructionBuilder): Unit = +JumpInsnNode(Opcodes.IFLT, label)
inline def IFGE(label: LabelNode)(using InstructionBuilder): Unit = +JumpInsnNode(Opcodes.IFGE, label)
inline def IFGT(label: LabelNode)(using InstructionBuilder): Unit = +JumpInsnNode(Opcodes.IFGT, label)
inline def IFLE(label: LabelNode)(using InstructionBuilder): Unit = +JumpInsnNode(Opcodes.IFLE, label)
inline def IF_ICMPEQ(label: LabelNode)(using InstructionBuilder): Unit = +JumpInsnNode(Opcodes.IF_ICMPEQ, label)
inline def IF_ICMPNE(label: LabelNode)(using InstructionBuilder): Unit = +JumpInsnNode(Opcodes.IF_ICMPNE, label)
inline def IF_ICMPLT(label: LabelNode)(using InstructionBuilder): Unit = +JumpInsnNode(Opcodes.IF_ICMPLT, label)
inline def IF_ICMPGE(label: LabelNode)(using InstructionBuilder): Unit = +JumpInsnNode(Opcodes.IF_ICMPGE, label)
inline def IF_ICMPGT(label: LabelNode)(using InstructionBuilder): Unit = +JumpInsnNode(Opcodes.IF_ICMPGT, label)
inline def IF_ICMPLE(label: LabelNode)(using InstructionBuilder): Unit = +JumpInsnNode(Opcodes.IF_ICMPLE, label)
inline def IF_ACMPEQ(label: LabelNode)(using InstructionBuilder): Unit = +JumpInsnNode(Opcodes.IF_ACMPEQ, label)
inline def IF_ACMPNE(label: LabelNode)(using InstructionBuilder): Unit = +JumpInsnNode(Opcodes.IF_ACMPNE, label)
inline def GOTO(label: LabelNode)(using InstructionBuilder): Unit = +JumpInsnNode(Opcodes.GOTO, label)
inline def JSR(label: LabelNode)(using InstructionBuilder): Unit = +JumpInsnNode(Opcodes.JSR, label)

inline def RET(index: Int)(using InstructionBuilder): Unit = +VarInsnNode(Opcodes.RET, index)

inline def TABLESWITCH(range: Range, default: LabelNode, labels: LabelNode*)(using InstructionBuilder): Unit = +TableSwitchInsnNode(range.start, range.end, default, labels*)
inline def LOOKUPSWITCH(default: LabelNode)(keys: Int*)(labels: LabelNode*)(using InstructionBuilder): Unit = +LookupSwitchInsnNode(default, keys.toArray, labels.toArray)
inline def LOOKUPSWITCH(default: LabelNode, blocks: Map[Int, LabelNode])(using InstructionBuilder): Unit =
    LOOKUPSWITCH(default)(blocks.keys.toSeq*)(blocks.values.toSeq*)

inline def IRETURN(using InstructionBuilder): Unit = +InsnNode(Opcodes.IRETURN)
inline def LRETURN(using InstructionBuilder): Unit = +InsnNode(Opcodes.LRETURN)
inline def FRETURN(using InstructionBuilder): Unit = +InsnNode(Opcodes.FRETURN)
inline def DRETURN(using InstructionBuilder): Unit = +InsnNode(Opcodes.DRETURN)
inline def ARETURN(using InstructionBuilder): Unit = +InsnNode(Opcodes.ARETURN)
inline def RETURN(using InstructionBuilder): Unit = +InsnNode(Opcodes.RETURN)

inline def GETSTATIC(owner: String, name: String, desc: String)(using InstructionBuilder): Unit = +FieldInsnNode(Opcodes.GETSTATIC, owner, name, desc)
inline def PUTSTATIC(owner: String, name: String, desc: String)(using InstructionBuilder): Unit = +FieldInsnNode(Opcodes.PUTSTATIC, owner, name, desc)
inline def GETFIELD(owner: String, name: String, desc: String)(using InstructionBuilder): Unit = +FieldInsnNode(Opcodes.GETFIELD, owner, name, desc)
inline def PUTFIELD(owner: String, name: String, desc: String)(using InstructionBuilder): Unit = +FieldInsnNode(Opcodes.PUTFIELD, owner, name, desc)

inline def INVOKEVIRTUAL(owner: String, name: String, desc: String)(using InstructionBuilder) = +MethodInsnNode(Opcodes.INVOKEVIRTUAL, owner, name, desc)
inline def INVOKESPECIAL(owner: String, name: String, desc: String)(using InstructionBuilder) = +MethodInsnNode(Opcodes.INVOKESPECIAL, owner, name, desc)
inline def INVOKESTATIC(owner: String, name: String, desc: String)(using InstructionBuilder) = +MethodInsnNode(Opcodes.INVOKESTATIC, owner, name, desc)
inline def INVOKEINTERFACE(owner: String, name: String, desc: String)(using InstructionBuilder) = +MethodInsnNode(Opcodes.INVOKEINTERFACE, owner, name, desc)

inline def INVOKEDYNAMIC(name: String, desc: String, bsm: Handle, bsmArgs: Any*)(using InstructionBuilder) = +InvokeDynamicInsnNode(name, desc, bsm, bsmArgs*)

inline def NEW(name: String)(using InstructionBuilder): Unit = +TypeInsnNode(Opcodes.NEW, name)
inline def ANEWARRAY(name: String)(using InstructionBuilder): Unit = +TypeInsnNode(Opcodes.ANEWARRAY, name)
inline def CHECKCAST(name: String)(using InstructionBuilder): Unit = +TypeInsnNode(Opcodes.CHECKCAST, name)
inline def INSTANCEOF(name: String)(using InstructionBuilder): Unit = +TypeInsnNode(Opcodes.INSTANCEOF, name)

inline def NEWARRAY(value: Int)(using InstructionBuilder): Unit = +IntInsnNode(Opcodes.NEWARRAY, value)

inline def ARRAYLENGTH(using InstructionBuilder): Unit = +InsnNode(Opcodes.ARRAYLENGTH)

inline def ATHROW(using InstructionBuilder): Unit = +InsnNode(Opcodes.ATHROW)
inline def MONITORENTER(using InstructionBuilder): Unit = +InsnNode(Opcodes.MONITORENTER)
inline def MONITOREXIT(using InstructionBuilder): Unit = +InsnNode(Opcodes.MONITOREXIT)

inline def MULTIANEWARRAY(desc: String, dimensions: Int)(using InstructionBuilder): Unit = +MultiANewArrayInsnNode(desc, dimensions)

inline def IFNULL(label: LabelNode)(using InstructionBuilder): Unit = +JumpInsnNode(Opcodes.IFNULL, label)
inline def IFNONNULL(label: LabelNode)(using InstructionBuilder): Unit = +JumpInsnNode(Opcodes.IFNONNULL, label)

inline def LABEL(label: Label)(using InstructionBuilder): Unit = +label

given label2LabelNode: Conversion[Label, LabelNode] with
    override def apply(x: Label): LabelNode = x.info match
        case node: LabelNode => node
        case _ =>
            val newLabelNode = LabelNode(x)
            x.info = newLabelNode
            newLabelNode

given labelMap2LabelNodeMap: Conversion[Map[Int, Label], Map[Int, LabelNode]] with
    override def apply(x: Map[Int, Label]): Map[Int, LabelNode] =
        Map((for (index, label) <- x yield (index, label.convert)).toSeq*)