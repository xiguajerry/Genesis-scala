package net.spartanb312.genesis.scala

import org.objectweb.asm.{Handle, Opcodes}

def H_GETFIELD(owner: String, name: String, descriptor: String) = Handle(Opcodes.H_GETFIELD, owner, name, descriptor, false)
def H_GETSTATIC(owner: String, name: String, descriptor: String) = Handle(Opcodes.H_GETSTATIC, owner, name, descriptor, false)
def H_PUTFIELD(owner: String, name: String, descriptor: String) = Handle(Opcodes.H_PUTFIELD, owner, name, descriptor, false)
def H_PUTSTATIC(owner: String, name: String, descriptor: String) = Handle(Opcodes.H_PUTSTATIC, owner, name, descriptor, false)
def H_INVOKEVIRTUAL(owner: String, name: String, descriptor: String) = Handle(Opcodes.H_INVOKEVIRTUAL, owner, name, descriptor, false)
def H_INVOKESTATIC(owner: String, name: String, descriptor: String) = Handle(Opcodes.H_INVOKESTATIC, owner, name, descriptor, false)
def H_INVOKESPECIAL(owner: String, name: String, descriptor: String) = Handle(Opcodes.H_INVOKESPECIAL, owner, name, descriptor, false)
def H_NEWINVOKESPECIAL(owner: String, name: String, descriptor: String) = Handle(Opcodes.H_NEWINVOKESPECIAL, owner, name, descriptor, false)
def H_INVOKEINTERFACE(owner: String, name: String, descriptor: String) = Handle(Opcodes.H_INVOKEINTERFACE, owner, name, descriptor, true)