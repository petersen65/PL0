// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package assembler implements an emitter that generates LLVM assembly language (LLVM IR).
package assembler

// Operation codes for assembly language
const (
	_ = Operation(iota)
	ModuleId
	Constant
	VariableDeclaration
	LoadVariable
	StoreVariable
	Function
	EndFunction
	FunctionReturn
)

type (
	// Type for operation codes.
	Operation int

	// Instructions of the emitted assembler program.
	Instructions []Instruction

	// Instruction is the structured representation of an assembler operation.
	Instruction struct {
		Operation Operation // operation code of the instruction
		Name      string    // name of an identifier
		DataType  string    // data type of the identifier
		Ssa       int       // SSA number of the identifier
		Global    bool      // global or local variable
		Value     any       // value of the instruction
	}
)

type (
	// Module of the emitted assembly language.
	Module []string
)

// The assembler interface provides an abstract API for emitting LLVM assembly language.
type Assembler interface {
	GetModule() Module
	GetLastInstruction() (Instruction, error)
	GetInstruction(index int) (Instruction, error)
	Constant(value any, valueType string) int
	VariableDeclaration(name, dataType string, ssa int, global bool) int
	LoadVariable(name, dataType string, ssa int, global bool) (int, int)
	StoreVariable(name, dataType string, ssa int, global bool) int
	Function(name, returnType string) int
	EndFunction() int
	Return(value any, valueType string) int
}

// Return the public interface of the private assembler implementation.
func NewAssembler(source string) Assembler {
	return newAssembler(source)
}
