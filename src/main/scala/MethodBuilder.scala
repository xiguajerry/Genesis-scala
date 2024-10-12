package net.spartanb312.genesis

import org.objectweb.asm.{ClassWriter, Label, Opcodes}
import org.objectweb.asm.tree.{ClassNode, InsnList, MethodNode}

import java.io.{File, FileOutputStream, FileWriter}
import scala.jdk.CollectionConverters.*

case class MethodBuilder(methodNode: MethodNode)

def INSTRUCTIONS(builder: InstructionBuilder ?=> Unit)(using methodBuilder: MethodBuilder): Unit =
    methodBuilder.methodNode.instructions.clear()
    given insnBuilder: InstructionBuilder(methodBuilder.methodNode.instructions)
    builder


def method(access: Int)(name: String)(desc: String)(signature: String = null)(exceptions: Array[String] = null)(builder: MethodBuilder ?=> Unit): MethodNode =
    val methodNode = MethodNode(access, name, desc, signature, exceptions)
    given MethodBuilder(methodNode)
    builder
    methodNode

//extension(insnList: InsnList)



@main
def main(): Unit =
    val methodNode = method(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC)("main")("([Ljava/lang/String;)V")(null)(null):
        INSTRUCTIONS:
            val labelA = Label()
            val labelB = Label()
            val labelC = Label()
            val labelD = Label()

            ICONST_1
            TABLESWITCH(1 to 2)(labelA)(labelB, labelC)
            LABEL(labelA)
            GOTO(labelD)

            LABEL(labelB)
            GETSTATIC("java/lang/System")("out")("Ljava/io/PrintStream;")
            LDC("Hello 1")
            INVOKEVIRTUAL("java/io/PrintStream")("println")("(Ljava/lang/String;)V")
            GOTO(labelD)

            LABEL(labelC)
            GOTO(labelD)

            LABEL(labelD)
            GETSTATIC("java/lang/System")("out")("Ljava/io/PrintStream;")
            LDC("End")
            INVOKEVIRTUAL("java/io/PrintStream")("println")("(Ljava/lang/String;)V")
            RETURN

    val classNode = ClassNode()
    classNode.name = "Main"
    classNode.superName = "java/lang/Object"
    classNode.methods = List(methodNode).asJava
    classNode.version = Opcodes.V1_8

    val writer = ClassWriter(ClassWriter.COMPUTE_FRAMES)
    classNode.accept(writer)
    FileOutputStream("D:\\Main.class").write(writer.toByteArray)