// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package assembler

import (
	"fmt"
	"io"

	cor "github.com/petersen65/PL0/v2/core"
)

// Private implementation of the assembler.
type assembler struct {
	virtualVariable int          // virtual variables counter
	instructions    Instructions // instructions of the emitted assembler program
}

// Return the public interface of the private assembler implementation.
func newAssembler(source string) Assembler {
	assembler := &assembler{instructions: make(Instructions, 0)}
	assembler.instructions = append(assembler.instructions, Instruction{Operation: ModuleId, Name: source})
	return assembler
}

// Print the module to a writer.
func (m Module) Print(print io.Writer, args ...any) error {
	for _, line := range m {
		if _, err := fmt.Fprintln(print, line); err != nil {
			return cor.NewGeneralError(cor.Assembler, failureMap, cor.Error, moduleExportFailed, nil, err)
		}
	}

	return nil
}

// Export the module to a writer in the specified format.
func (m Module) Export(format cor.ExportFormat, print io.Writer) error {
	switch format {
	case cor.Text:
		// print is a convenience function to export the module as a string to the print writer
		return m.Print(print)

	default:
		panic(cor.NewGeneralError(cor.Assembler, failureMap, cor.Fatal, unknownExportFormat, format, nil))
	}
}

// Generate module of the emitted assembler program.
func (a *assembler) GetModule() Module {
	module := make(Module, 0, len(a.instructions))

	for i, instr := range a.instructions {
		switch instr.Operation {
		case ModuleId:
			module = append(module, fmt.Sprintf("; ModuleID = '%v'", instr.Name), fmt.Sprintf("source_filename = \"%v\"", instr.Name))

		case Constant:
			// intentionally do nothing

		case VariableDeclaration:
			if instr.Global {
				module = append(module, fmt.Sprintf("@%v.%v = dso_local global %v 0", instr.Name, instr.Ssa, instr.DataType))
			} else {
				module = append(module, fmt.Sprintf("%%%v.%v = alloca %v", instr.Name, instr.Ssa, instr.DataType))
			}

		case LoadVariable:
			// loading a variable creates a new version of the variable
			if instr.Global {
				if instr.Ssa > 1 {
					module = append(
						module,
						fmt.Sprintf("%%%v.%v = load %v, ptr %%%v.%v",
							instr.Name, instr.Ssa+1, instr.DataType, instr.Name, instr.Ssa))
				} else {
					module = append(
						module,
						fmt.Sprintf("%%%v.%v = load %v, ptr @%v.%v",
							instr.Name, instr.Ssa+1, instr.DataType, instr.Name, instr.Ssa))
				}
			} else {
				module = append(
					module,
					fmt.Sprintf("%%%v.%v = load %v, ptr %%%v.%v",
						instr.Name, instr.Ssa+1, instr.DataType, instr.Name, instr.Ssa))
			}

		case StoreVariable:
			// store result of an expression into a variable
			right, rightType := expressionResult(a.instructions[i-1])
			result := variable(instr.Name, instr.Ssa, instr.Global)
			module = append(module, fmt.Sprintf("store %v %v, ptr %v", rightType, right, result))

		case Add:
			left, leftType := expressionResult(a.instructions[i-2])
			right, _ := expressionResult(a.instructions[i-1])
			result := a.newVirtualVariable()
			module = append(module, fmt.Sprintf("%v = add %v %v, %v", result, leftType, left, right))

		case Subtract:

		case Multiply:

		case Divide:

		case Function:
			module = append(module, fmt.Sprintf("define dso_local %v @%v() {", instr.DataType, instr.Name))

		case EndFunction:
			module = append(module, "}")

		case FunctionReturn:
			if instr.Value == nil {
				module = append(module, "ret void")
			} else {
				module = append(module, fmt.Sprintf("ret %v %v", instr.DataType, instr.Value.(int64)))
			}
		}
	}

	return module
}

// Expose the last instruction of the emitted assembler program.
func (a *assembler) GetLastInstruction() (Instruction, error) {
	if len(a.instructions) == 0 {
		return Instruction{}, cor.NewGeneralError(cor.Assembler, failureMap, cor.Error, instructionOutOfRange, 0, nil)
	}

	return a.instructions[len(a.instructions)-1], nil
}

// Expose an instruction of the emitted assembler program.
func (a *assembler) GetInstruction(index int) (Instruction, error) {
	if index < 0 || index >= len(a.instructions) {
		return Instruction{}, cor.NewGeneralError(cor.Assembler, failureMap, cor.Error, instructionOutOfRange, index, nil)
	}

	return a.instructions[index], nil
}

// Emit a constant value.
func (a *assembler) Constant(value any, valueType string) int {
	a.instructions = append(a.instructions, Instruction{Operation: Constant, Value: value, DataType: valueType})
	return len(a.instructions) - 1
}

// Emit a local or global variable declaration.
func (a *assembler) VariableDeclaration(name, dataType string, ssa int, global bool) int {
	a.instructions = append(a.instructions, Instruction{Operation: VariableDeclaration, Name: name, DataType: dataType, Ssa: ssa, Global: global})
	return len(a.instructions) - 1
}

// Load a variable.
func (a *assembler) LoadVariable(name, dataType string, ssa int, global bool) (int, int) {
	a.instructions = append(a.instructions, Instruction{Operation: LoadVariable, Name: name, DataType: dataType, Ssa: ssa, Global: global})
	return len(a.instructions) - 1, ssa + 1
}

func (a *assembler) StoreVariable(name, dataType string, ssa int, global bool) int {
	a.instructions = append(a.instructions, Instruction{Operation: StoreVariable, Name: name, DataType: dataType, Ssa: ssa, Global: global})
	return len(a.instructions) - 1
}

func (a *assembler) Add() int {
	a.instructions = append(a.instructions, Instruction{Operation: Add})
	return len(a.instructions) - 1
}

func (a *assembler) Subtract() int {
	a.instructions = append(a.instructions, Instruction{Operation: Subtract})
	return len(a.instructions) - 1
}

func (a *assembler) Multiply() int {
	a.instructions = append(a.instructions, Instruction{Operation: Multiply})
	return len(a.instructions) - 1
}

func (a *assembler) Divide() int {
	a.instructions = append(a.instructions, Instruction{Operation: Divide})
	return len(a.instructions) - 1
}

// Emit a function definition.
func (a *assembler) Function(name string, returnType string) int {
	a.instructions = append(a.instructions, Instruction{Operation: Function, Name: name, DataType: returnType})
	return len(a.instructions) - 1
}

// End a function definition.
func (a *assembler) EndFunction() int {
	a.instructions = append(a.instructions, Instruction{Operation: EndFunction})
	return len(a.instructions) - 1
}

// Return from a function.
func (a *assembler) Return(value any, valueType string) int {
	a.instructions = append(a.instructions, Instruction{Operation: FunctionReturn, Value: value, DataType: valueType})
	return len(a.instructions) - 1
}

// Create a new virtual variable name.
func (a *assembler) newVirtualVariable() string {
	a.virtualVariable++
	return fmt.Sprintf("%%%v", a.virtualVariable)
}

func variable(name string, ssa int, global bool) string {
	if global {
		return fmt.Sprintf("@%v.%v", name, ssa)
	}

	return fmt.Sprintf("%%%v.%v", name, ssa)	
}

func expressionResult(instr Instruction) (string, string) {
	var result string

	if instr.Operation == Constant {
		result = fmt.Sprintf("%v", instr.Value)
	} else if instr.Operation == LoadVariable {
		result = fmt.Sprintf("%%%v.%v", instr.Name, instr.Ssa+1)
	} else {
		panic(cor.NewGeneralError(cor.Assembler, failureMap, cor.Fatal, unexpectedOperation, instr.Operation, nil))
	}

	return result, instr.DataType
}
