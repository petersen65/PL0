// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package assembler implements an emitter that generates LLVM assembly language (LLVM IR).
package assembler

type (
	// Module of the emitted assembly language.
	Module []string
)

// The assembler interface provides an abstract API for emitting LLVM assembly language.
type Assembler interface {
	GetModule() Module
	Constant(value any)
	VariableDeclaration(name, dataType string, ssa int, global bool)
	LoadVariable(name, dataType string, ssa int, global bool) int
	StoreVariable(name, dataType string, ssa int, global bool)
	Function(name, returnType string)
	EndFunction()
	Return(value any, valueType string)
}

// Return the public interface of the private assembler implementation.
func NewAssembler(source string) Assembler {
	return newAssembler(source)
}
