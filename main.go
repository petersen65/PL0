// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

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
		compiler.Driver(compiler.Compile, os.Args[2], os.Args[3], os.Stdout)

	case len(os.Args) > 2 && os.Args[1] == "-r" && len(os.Args[2]) > 0:
		compiler.Driver(compiler.Emulate, "", os.Args[2], os.Stdout)

	case len(os.Args) > 2 && os.Args[1] == "-i" && len(os.Args[2]) > 0:
		compiler.Driver(compiler.PrintIntermediateCode, "", os.Args[2], os.Stdout)

	case len(os.Args) > 3 && os.Args[1] == "-cr" && len(os.Args[2]) > 0 && len(os.Args[3]) > 0:
		compiler.Driver(compiler.Compile|compiler.Emulate, os.Args[2], os.Args[3], os.Stdout)

	case len(os.Args) > 3 && os.Args[1] == "-ct" && len(os.Args[2]) > 0 && len(os.Args[3]) > 0:
		compiler.Driver(compiler.Compile|compiler.PrintTokenStream, os.Args[2], os.Args[3], os.Stdout)

	case len(os.Args) > 3 && os.Args[1] == "-ca" && len(os.Args[2]) > 0 && len(os.Args[3]) > 0:
		compiler.Driver(compiler.Compile|compiler.PrintAbstractSyntaxTree, os.Args[2], os.Args[3], os.Stdout)

	case len(os.Args) > 3 && os.Args[1] == "-ci" && len(os.Args[2]) > 0 && len(os.Args[3]) > 0:
		compiler.Driver(compiler.Compile|compiler.PrintIntermediateCode, os.Args[2], os.Args[3], os.Stdout)

	default:
		usage()
	}
}

// Function 'usage' prints the command line usage information to the standard output (console).
func usage() {
	fmt.Println("Usage: pl0 -c[r|t|a|i] <source file> <target file>")
	fmt.Println("       pl0 -r <target file>")
	fmt.Println("       pl0 -p <target file>")
}
