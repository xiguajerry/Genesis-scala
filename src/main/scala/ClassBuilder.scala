package net.spartanb312.genesis.scala

import org.objectweb.asm.tree.{ClassNode, MethodNode}

import scala.jdk.CollectionConverters.*
import scala.quoted.{Expr, Quotes}

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

def CONSTRUCTOR(access: Access, description: String, signature: String = null, exception: Array[String] = null)(builder: MethodBuilder ?=> Unit)(using classBuilder: ClassBuilder): Unit =
    val constructorMethod = constructor(access, description, signature, exception)(builder)
    ~constructorMethod


//def foo(): 1 = fib(1)
//def bar1(): 2 = fib(3)
//def bar2(): 4 = fib(4)

inline def fib(i: Int) = ${ fibCode('i) }

def fibCode(n: Expr[Int])(using Quotes): Expr[Int] = n.valueOrAbort match
    case 1 | 2 => '{ 1 }
    case v => Expr(fibCode(Expr(v - 1)).valueOrAbort + fibCode(Expr(v - 2)).valueOrAbort)
