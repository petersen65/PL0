// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

// Package main implements the PL/0 compiler command line interface.
package main

import (
	"fmt"
	"os"

	"github.com/petersen65/PL0/compiler"
)

// Function main is the entry point for the PL/0 compiler command line interface.
// It parses the command line arguments and calls the appropriate functions.
//
// The command line arguments are:
//
//	-c[r|t|a|i] <source file> <target file>
//	-r <target file>
//	-p <target file>
func main() {
	fmt.Println("PL/0 Compiler Version 2.0.0 2024")
	fmt.Println("Copyright (c) 2024, Michael Petersen. All rights reserved.")

	switch {
	case len(os.Args) > 3 && os.Args[1] == "-c" && len(os.Args[2]) > 0 && len(os.Args[3]) > 0:
		compile(os.Args[2], os.Args[3], 0)

	case len(os.Args) > 2 && os.Args[1] == "-r" && len(os.Args[2]) > 0:
		run(os.Args[2])

	case len(os.Args) > 2 && os.Args[1] == "-i" && len(os.Args[2]) > 0:
		print(os.Args[2])

	case len(os.Args) > 3 && os.Args[1] == "-cr" && len(os.Args[2]) > 0 && len(os.Args[3]) > 0:
		if compile(os.Args[2], os.Args[3], 0) == nil {
			run(os.Args[3])
		}

	case len(os.Args) > 3 && os.Args[1] == "-ct" && len(os.Args[2]) > 0 && len(os.Args[3]) > 0:
		compile(os.Args[2], os.Args[3], compiler.PrintTokens)

	case len(os.Args) > 3 && os.Args[1] == "-ca" && len(os.Args[2]) > 0 && len(os.Args[3]) > 0:
		compile(os.Args[2], os.Args[3], compiler.PrintAbstractSyntax)

	case len(os.Args) > 3 && os.Args[1] == "-ci" && len(os.Args[2]) > 0 && len(os.Args[3]) > 0:
		if compile(os.Args[2], os.Args[3], 0) == nil {
			print(os.Args[3])
		}

	default:
		usage()
	}
}

// Function compile compiles the PL/0 source file pl0 and writes the IL/0 code to the target file il0.
func compile(pl0, il0 string, options compiler.Options) error {
	err := compiler.CompileFile(pl0, il0, options, os.Stdout)

	if err != nil {
		compiler.PrintErrorSummary(err, os.Stdout)
	} else {
		fmt.Println("Compilation successful")
	}

	return err
}

// Function 'run' executes the IL/0 code in the target file il0.
func run(il0 string) error {
	err := compiler.RunFile(il0, os.Stdout)

	if err != nil {
		compiler.PrintErrorSummary(err, os.Stdout)
	} else {
		fmt.Println("Run successful")
	}

	return err
}

// Function 'print' prints the IL/0 code in the target file il0 to the standard output (console).
func print(il0 string) error {
	err := compiler.PrintFile(il0, os.Stdout)

	if err != nil {
		compiler.PrintErrorSummary(err, os.Stdout)
	} else {
		fmt.Println("Print successful")
	}

	return err
}

// Function 'usage' prints the command line usage information to the standard output (console).
func usage() {
	fmt.Println("Usage: pl0 -c[r|t|a|i] <source file> <target file>")
	fmt.Println("       pl0 -r <target file>")
	fmt.Println("       pl0 -p <target file>")
}
