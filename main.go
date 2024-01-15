// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package main

import (
	"fmt"
	"os"

	"github.com/petersen65/PL0/compiler"
)

func main() {
	fmt.Println("PL/0 Compiler Version 1.0.0 1986")
	fmt.Println("Copyright (c) 2024, Michael Petersen. All rights reserved.")

	switch {
	case len(os.Args) > 3 && os.Args[1] == "-c" && len(os.Args[2]) > 0 && len(os.Args[3]) > 0:
		compile(os.Args[2], os.Args[3])

	case len(os.Args) > 2 && os.Args[1] == "-r" && len(os.Args[2]) > 0:
		run(os.Args[2])

	case len(os.Args) > 2 && os.Args[1] == "-p" && len(os.Args[2]) > 0:
		print(os.Args[2])

	case len(os.Args) > 3 && os.Args[1] == "-cr" && len(os.Args[2]) > 0 && len(os.Args[3]) > 0:
		if compile(os.Args[2], os.Args[3]) == nil {
			run(os.Args[3])
		}

	case len(os.Args) > 3 && os.Args[1] == "-cp" && len(os.Args[2]) > 0 && len(os.Args[3]) > 0:
		if compile(os.Args[2], os.Args[3]) == nil {
			print(os.Args[3])
		}

	default:
		usage()
	}
}

func compile(pl0, il0 string) error {
	err := compiler.CompileFile(pl0, il0, os.Stdout)

	if err != nil {
		fmt.Println("Error:", err)
	} else {
		fmt.Println("Compilation successful")
	}

	return err
}

func run(il0 string) error {
	err := compiler.RunFile(il0, os.Stdout)

	if err != nil {
		fmt.Println("Error:", err)
	} else {
		fmt.Println("Run successful")
	}

	return err
}

func print(il0 string) error {
	err := compiler.PrintFile(il0, os.Stdout)

	if err != nil {
		fmt.Println("Error:", err)
	} else {
		fmt.Println("Print successful")
	}

	return err
}

func usage() {
	fmt.Println("Usage: pl0 -c[r|p] <source file> <target file>")
	fmt.Println("       pl0 -r <target file>")
	fmt.Println("       pl0 -p <target file>")
}
