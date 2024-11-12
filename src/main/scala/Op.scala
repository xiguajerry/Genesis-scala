package net.spartanb312.genesis.scala

import org.objectweb.asm.tree.{AbstractInsnNode, InsnList, MethodNode}
import scala.jdk.CollectionConverters.*


extension(insn: AbstractInsnNode)
    def unary_+(using insnBuilder: InstructionBuilder): Unit = insnBuilder + insn


extension(insnList: InsnList)
    def unary_+(using insnBuilder: InstructionBuilder): Unit = insnBuilder + insnList
    def unary_~(using insnBuilder: InstructionBuilder): Unit = insnBuilder ~ insnList
//    def unary_+(using methodBuilder: MethodBuilder): Unit =
//        InstructionBuilder(methodBuilder.methodNode.instructions) + insnList
//    def unary_~(using methodBuilder: MethodBuilder): Unit =
//        InstructionBuilder(methodBuilder.methodNode.instructions) ~ insnList


extension(methodNode: MethodNode)
    def unary_~(using classBuilder: ClassBuilder): Unit =
        if classBuilder.classNode.methods == null then
            classBuilder.classNode.methods = List().asJava
        classBuilder.classNode.methods.removeIf: m =>
            (m.name == methodNode.name) && (m.desc == methodNode.desc)
        classBuilder.classNode.methods.add(methodNode)

    def unary_+(using classBuilder: ClassBuilder): Unit =
        if classBuilder.classNode.methods == null then
            classBuilder.classNode.methods = List().asJava
        val existingMethodOp = classBuilder.classNode.methods.asScala.find: m =>
            (m.name == methodNode.name) && (m.desc == methodNode.desc)
        if existingMethodOp.isEmpty then ~methodNode
        else
            val existingMethod = existingMethodOp.get
            existingMethod.instructions.add(methodNode.instructions)
            if existingMethod.exceptions == null then existingMethod.exceptions = methodNode.exceptions
            else existingMethod.exceptions =
                (existingMethod.exceptions.asScala :++ methodNode.exceptions.asScala).distinct.asJava 
