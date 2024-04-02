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

// Names of arithmetic operators.
var arithmeticOperatorNames = map[Operation]string{
	Add:      "add",
	Subtract: "sub",
	Multiply: "mul",
	Divide:   "sdiv",
}

// Return the public interface of the private assembler implementation.
func newAssembler(source string) Assembler {
	assembler := &assembler{instructions: make(Instructions, 0)}
	assembler.instructions = append(assembler.instructions, Instruction{Operation: ModuleId, Variable: Variable{Name: source}})
	return assembler
}

// String representation of an assembler constant.
func (c Constant) String() string {
	return fmt.Sprintf("%v %v", c.DataType, c.Value)
}

// String representation of an assembler variable.
func (v Variable) String() string {
	var name string

	if v.Global {
		name = fmt.Sprintf("@%v", v.Name)
	} else {
		name = fmt.Sprintf("%%%v", v.Name)
	}

	if v.Ssa > 0 {
		name = fmt.Sprintf("%v.%v", name, v.Ssa)
	}

	return name
}

// Create a new version from a variable.
func (v *Variable) NewVersion() *Variable {
	return &Variable{Name: v.Name, DataType: v.DataType, Ssa: v.Ssa + 1, Global: false}
}

// Return any result of an expression and its data type as assembler strings.
func (i *Instruction) AsExpressionResult() (string, string) {
	var result, dataType string

	switch i.Operation {
	case LoadConstant:
		result = fmt.Sprintf("%v", i.Constant.Value)
		dataType = i.Constant.DataType

	case LoadVariable:
		result = i.Variable.NewVersion().String()
		dataType = i.Variable.DataType

	case Add, Subtract, Multiply, Divide:
		result = i.Variable.String()
		dataType = i.Variable.DataType

	default:
		panic(cor.NewGeneralError(cor.Assembler, failureMap, cor.Fatal, unexpectedOperation, i.Operation, nil))
	}

	return result, dataType
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
			source := instr.Variable.Name
			module = append(module, fmt.Sprintf("; ModuleID = '%v'", source), fmt.Sprintf("source_filename = \"%v\"", source))

		case LoadConstant:
			// intentionally do nothing

		case VariableDeclaration:
			variable := instr.Variable

			if variable.Global {
				module = append(module, fmt.Sprintf("%v = dso_local global %v 0", variable, variable.DataType))
			} else {
				module = append(module, fmt.Sprintf("%v = alloca %v", variable, variable.DataType))
			}

		case LoadVariable:
			// loading a variable creates a new version of that variable
			variable := instr.Variable
			newVersion := Variable{Name: variable.Name, DataType: variable.DataType, Ssa: variable.Ssa + 1, Global: false}
			module = append(module, fmt.Sprintf("%v = load %v, ptr %v", newVersion, variable.DataType, variable))

		case StoreVariable:
			right, rightType := a.instructions[i-1].AsExpressionResult()

			storeIn := Variable{
				Name:     instr.Variable.Name,
				DataType: instr.Variable.DataType,
				Ssa:      instr.Variable.Ssa,
				Global:   instr.Variable.Global}

			module = append(module, fmt.Sprintf("store %v %v, ptr %v", rightType, right, storeIn))

		case Add, Subtract, Multiply, Divide:
			// add the left and right operands of an expression and store the result in a new virtual variable
			left, leftType := a.instructions[i-2].AsExpressionResult()
			right, _ := a.instructions[i-1].AsExpressionResult()
			result := a.NewVirtualVariable(leftType)

			// update the instruction with the result of the expression (a new virtual variable)
			update, _ := a.GetInstruction(i)
			update.Variable = *result

			module = append(module,
				fmt.Sprintf("%v = %v %v %v, %v", result, arithmeticOperatorNames[instr.Operation], leftType, left, right))

		case Function:
			variable := instr.Variable
			module = append(module, fmt.Sprintf("define dso_local %v @%v() {", variable.DataType, variable.Name))

		case EndFunction:
			module = append(module, "}")

		case FunctionReturn:
			constant := instr.Constant

			if constant.Value == nil {
				module = append(module, "ret void")
			} else {
				module = append(module, fmt.Sprintf("ret %v %v", constant.DataType, constant.Value.(int64)))
			}
		}
	}

	return module
}

// Create a new virtual variable.
func (a *assembler) NewVirtualVariable(dataType string) *Variable {
	a.virtualVariable++
	return &Variable{Name: fmt.Sprintf("%v", a.virtualVariable), DataType: dataType, Ssa: 0, Global: false}
}

// Append an instruction to the emitted assembler program.
func (a *assembler) AppendInstruction(instruction Instruction) {
	a.instructions = append(a.instructions, instruction)
}

// Expose the last instruction of the emitted assembler program.
func (a *assembler) GetLastInstruction() (*Instruction, error) {
	if len(a.instructions) == 0 {
		return nil, cor.NewGeneralError(cor.Assembler, failureMap, cor.Error, instructionOutOfRange, 0, nil)
	}

	return &a.instructions[len(a.instructions)-1], nil
}

// Expose an instruction of the emitted assembler program.
func (a *assembler) GetInstruction(index int) (*Instruction, error) {
	if index < 0 || index >= len(a.instructions) {
		return nil, cor.NewGeneralError(cor.Assembler, failureMap, cor.Error, instructionOutOfRange, index, nil)
	}

	return &a.instructions[index], nil
}

// Emit a constant value.
func (a *assembler) LoadConstant(value any, valueType string) int {
	a.AppendInstruction(Instruction{Operation: LoadConstant, Constant: Constant{Value: value, DataType: valueType}})
	return len(a.instructions) - 1
}

// Emit a local or global variable declaration.
func (a *assembler) VariableDeclaration(name, dataType string, ssa int, global bool) int {
	a.AppendInstruction(
		Instruction{
			Operation: VariableDeclaration,
			Variable:  Variable{Name: name, DataType: dataType, Ssa: ssa, Global: global},
		})

	return len(a.instructions) - 1
}

// Load a variable.
func (a *assembler) LoadVariable(name, dataType string, ssa int, global bool) (int, int) {
	a.AppendInstruction(
		Instruction{
			Operation: LoadVariable,
			Variable:  Variable{Name: name, DataType: dataType, Ssa: ssa, Global: global},
		})

	return len(a.instructions) - 1, ssa + 1
}

func (a *assembler) StoreVariable(name, dataType string, ssa int, global bool) int {
	a.AppendInstruction(
		Instruction{
			Operation: StoreVariable,
			Variable:  Variable{Name: name, DataType: dataType, Ssa: ssa, Global: global},
		})

	return len(a.instructions) - 1
}

func (a *assembler) Add() int {
	a.AppendInstruction(Instruction{Operation: Add})
	return len(a.instructions) - 1
}

func (a *assembler) Subtract() int {
	a.AppendInstruction(Instruction{Operation: Subtract})
	return len(a.instructions) - 1
}

func (a *assembler) Multiply() int {
	a.AppendInstruction(Instruction{Operation: Multiply})
	return len(a.instructions) - 1
}

func (a *assembler) Divide() int {
	a.AppendInstruction(Instruction{Operation: Divide})
	return len(a.instructions) - 1
}

// Emit a function definition.
func (a *assembler) Function(name string, returnType string) int {
	a.AppendInstruction(Instruction{Operation: Function, Variable: Variable{Name: name, DataType: returnType}})
	return len(a.instructions) - 1
}

// End a function definition.
func (a *assembler) EndFunction() int {
	a.AppendInstruction(Instruction{Operation: EndFunction})
	return len(a.instructions) - 1
}

// Return a value from a function.
func (a *assembler) ReturnValue(value any, dataType string) int {
	a.AppendInstruction(Instruction{Operation: FunctionReturn, Constant: Constant{Value: value, DataType: dataType}})
	return len(a.instructions) - 1
}
