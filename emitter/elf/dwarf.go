// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package elf

import (
	"fmt"
	"strings"
)

// Represents a variable in the debug information with its name and line from the source code file.
type Variable struct {
	Name            string // name of the variable in the source code
	Offset          int32  // offset in the activation record where the variable is stored
	DeclarationLine uint32 // line number in the source code where the variable is declared
}

// Generates the DWARF .debug_str section for the ELF file, which contains a string table used in debug information.
func CreateDebugStringTable(compilationUnitName, functionName string, variables []Variable) string {
	var builder strings.Builder
	
	builder.WriteString(".section .debug_str,\"MS\",@progbits,1\n")
	builder.WriteString(fmt.Sprintf(".str_filename: .string \"%s\"\n", compilationUnitName))
	builder.WriteString(fmt.Sprintf(".str_main:     .string \"%s\"\n", functionName))

	seen := map[string]bool{}

	// Iterate through the variables and add their names to the string table, ensuring no duplicates.
	for _, v := range variables {
		if !seen[v.Name] {
			builder.WriteString(fmt.Sprintf(".str_%s:        .string \"%s\"\n", v.Name, v.Name))
			seen[v.Name] = true
		}
	}
	
	return builder.String()
}
