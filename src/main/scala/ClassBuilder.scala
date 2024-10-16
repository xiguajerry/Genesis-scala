package net.spartanb312.genesis

import org.objectweb.asm.tree.{ClassNode, MethodNode}
import scala.jdk.CollectionConverters.*

case class ClassBuilder(classNode: ClassNode)

def clazz(access: Int, name: String, superName: String = "java/lang/Object", interfaces: List[String] = List(), signature: String = null, version: Int = Java7)(builder: ClassBuilder ?=> Unit): ClassNode =
    val classNode = ClassNode()
    classNode.access = access
    classNode.name = name
    classNode.superName = superName
    classNode.interfaces = interfaces.asJava
    classNode.signature = signature
    classNode.version = version
    given ClassBuilder(classNode)
    builder
    classNode

def CLINIT(builder: MethodBuilder ?=> Unit)(using classBuilder: ClassBuilder): Unit =
    val clinitMethod = clinit(builder)
    ~clinitMethod

def CONSTRUCTOR(access: Int, description: String, signature: String = null, exception: Array[String] = null)(builder: MethodBuilder ?=> Unit)(using classBuilder: ClassBuilder): Unit =
    val constructorMethod = constructor(access, description, signature, exception)(builder)
    ~constructorMethod
