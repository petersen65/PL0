// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package assembler implements an emitter that generates LLVM assembly language (LLVM IR).
package assembler

// Operation codes for assembly language
const (
	_ = Operation(iota)
	ModuleId
	LoadConstant
	VariableDeclaration
	LoadVariable
	StoreVariable
	Add
	Subtract
	Multiply
	Divide
	Function
	EndFunction
	FunctionReturn
)

type (
	// Type for operation codes.
	Operation int

	// Instructions of the emitted assembler program.
	Instructions []Instruction

	// Constant is the structured representation of an assembler constant.
	Constant struct {
		Value    any    // value of the constant
		DataType string // data type of the constant
	}

	// Variable is the structured representation of an assembler variable.
	Variable struct {
		Name     string // name of the variable
		DataType string // data type of the variable
		Ssa      int    // SSA number of the variable
		Global   bool   // global or local variable
	}

	// Instruction is the structured representation of an assembler operation.
	Instruction struct {
		Operation Operation // operation code of the instruction
		Constant  Constant  // constant of the instruction
		Variable  Variable  // variable of the instruction
	}

	// Module of the emitted assembly language.
	Module []string

	// The assembler interface provides an abstract API for emitting LLVM assembly language.
	Assembler interface {
		GetModule() Module
		NewVirtualVariable(dataType string) *Variable
		AppendInstruction(instruction Instruction)
		GetLastInstruction() (*Instruction, error)
		GetInstruction(index int) (*Instruction, error)
		LoadConstant(value any, valueType string) int
		VariableDeclaration(name, dataType string, ssa int, global bool) int
		LoadVariable(name, dataType string, ssa int, global bool) (int, int)
		StoreVariable(name, dataType string, ssa int, global bool) int
		Add() int
		Subtract() int
		Multiply() int
		Divide() int
		Function(name, returnType string) int
		EndFunction() int
		ReturnValue(value any, valueType string) int
	}
)

// Return the public interface of the private assembler implementation.
func NewAssembler(source string) Assembler {
	return newAssembler(source)
}
