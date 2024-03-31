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
	module Module
}

// Return the public interface of the private assembler implementation.
func newAssembler(source string) Assembler {
	return &assembler{
		module: []string{
			fmt.Sprintf("; ModuleID = '%v'", source),
			fmt.Sprintf("source_filename = \"%v\"", source),
		},
	}
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

// Expose module of the emitted assembler program.
func (a *assembler) GetModule() Module {
	return a.module
}

// Emit a function definition.
func (a *assembler) Function(name string, returnType string) {
	a.module = append(a.module, fmt.Sprintf("define %v @%v() {", returnType, name))
}

// End a function definition.
func (a *assembler) EndFunction() {
	a.module = append(a.module, "}")
}

// Return from a function.
func (a *assembler) Return(value any, valueType string) {
	a.module = append(a.module, fmt.Sprintf("ret %v %v", valueType, value.(int64)))
}
