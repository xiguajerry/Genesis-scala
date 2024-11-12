package net.spartanb312.genesis.scala

import org.objectweb.asm.Opcodes

import scala.annotation.targetName

opaque type Access = Int

extension(a: Access)
    @targetName("or")
    def |(b: Access): Access = a | b
    @targetName("and")
    def &(b: Access): Access = a & b
    def has(b: Access): Boolean = (a & b) == b
    def toInt: Int = a

def PUBLIC: Access = Opcodes.ACC_PUBLIC
def PRIVATE: Access = Opcodes.ACC_PRIVATE
def PROTECTED: Access = Opcodes.ACC_PROTECTED
def STATIC: Access = Opcodes.ACC_STATIC
def FINAL: Access = Opcodes.ACC_FINAL
def SUPER: Access = Opcodes.ACC_SUPER
def SYNCHRONIZED: Access = Opcodes.ACC_SYNCHRONIZED
def OPEN: Access = Opcodes.ACC_OPEN
def TRANSITIVE: Access = Opcodes.ACC_TRANSITIVE
def VOLATILE: Access = Opcodes.ACC_VOLATILE
def BRIDGE: Access = Opcodes.ACC_BRIDGE
def STATIC_PHASE: Access = Opcodes.ACC_STATIC_PHASE
def VARARGS: Access = Opcodes.ACC_VARARGS
def TRANSIENT: Access = Opcodes.ACC_TRANSIENT
def NATIVE: Access = Opcodes.ACC_NATIVE
def INTERFACE: Access = Opcodes.ACC_INTERFACE
def ABSTRACT: Access = Opcodes.ACC_ABSTRACT
def STRICT: Access = Opcodes.ACC_STRICT
def SYNTHETIC: Access = Opcodes.ACC_SYNTHETIC
def ANNOTATION: Access = Opcodes.ACC_ANNOTATION
def ENUM: Access = Opcodes.ACC_ENUM
def MANDATED: Access = Opcodes.ACC_MANDATED
def MODULE: Access = Opcodes.ACC_MODULE

def RECORD: Access = Opcodes.ACC_RECORD
def DEPRECATED: Access = Opcodes.ACC_DEPRECATED