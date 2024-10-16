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


def method(access: Access, name: String, description: String, signature: String = null, exceptions: Array[String] = null)(builder: MethodBuilder ?=> Unit): MethodNode =
    val methodNode = MethodNode(access, name, description, signature, exceptions)
    given MethodBuilder(methodNode)
    builder
    methodNode


def clinit(builder: MethodBuilder ?=> Unit): MethodNode =
    val methodNode = MethodNode(PRIVATE | STATIC, "<clinit>", "()V", null, null)
    given MethodBuilder(methodNode)
    builder
    methodNode


def constructor(access: Access, description: String, signature: String, exception: Array[String] = null)(builder: MethodBuilder ?=> Unit): MethodNode =
    val methodNode = MethodNode(access, "<init>", description, signature, exception)
    given MethodBuilder(methodNode)
    builder
    methodNode


transparent inline def MethodNode(access: Access, name: String, description: String, signature: String, exceptions: Array[String]): org.objectweb.asm.tree.MethodNode =
    org.objectweb.asm.tree.MethodNode(access.toInt, name, description, signature, exceptions)


@main
def main(): Unit =
    val classNode1 = clazz(PUBLIC | STATIC, "Main"):
        CLINIT:
            INSTRUCTIONS:
                GETSTATIC("java/lang/System", "out", "Ljava/io/PrintStream;")
                LDC("CLINIT")
                INVOKEVIRTUAL("java/io/PrintStream", "println", "(Ljava/lang/String;)V")
                RETURN

        CONSTRUCTOR(PUBLIC, "()V"):
            INSTRUCTIONS:
                ALOAD(0)
                INVOKESPECIAL("java/lang/Object", "<init>", "()V")
                GETSTATIC("java/lang/System", "out", "Ljava/io/PrintStream;")
                LDC("Hello 1")
                INVOKEVIRTUAL("java/io/PrintStream", "println", "(Ljava/lang/String;)V")
                RETURN

        +method(PUBLIC | STATIC, "main", "([Ljava/lang/String;)V", null, null):
            INSTRUCTIONS:

                +(TRY {
                    ICONST_1
                    TABLESWITCH(1 to 2, L"labelA", L"labelB", L"labelC")
                    LABEL(L"labelA")
                    GOTO(L"labelD")

                    LABEL(L"labelB")
                    GETSTATIC("java/lang/System", "out", "Ljava/io/PrintStream;")
                    LDC("Hello 11")
                    INVOKEVIRTUAL("java/io/PrintStream", "println", "(Ljava/lang/String;)V")
                    GOTO(L"labelD")

                    LABEL(L"labelC")
                    GOTO(L"labelD")

                    LABEL(L"labelD")
                    GETSTATIC("java/lang/System", "out", "Ljava/io/PrintStream;")
                    LDC("End1")
                    INVOKEVIRTUAL("java/io/PrintStream", "println", "(Ljava/lang/String;)V")

                    NEW("java/lang/Exception")
                    DUP
                    LDC("hello")
                    INVOKESPECIAL("java/lang/Exception", "<init>", "(Ljava/lang/String;)V")
                    ATHROW
                } CATCH ("java/lang/Exception") apply {
                    POP
                    GETSTATIC("java/lang/System", "out", "Ljava/io/PrintStream;")
                    LDC("Exception1")
                    INVOKEVIRTUAL("java/io/PrintStream", "println", "(Ljava/lang/String;)V")
                })
                RETURN

    val writer = ClassWriter(ClassWriter.COMPUTE_FRAMES)
    classNode1.accept(writer)
    FileOutputStream("D:\\Main.class").write(writer.toByteArray)