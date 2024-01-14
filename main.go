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
		if err := compiler.CompileFile(os.Args[2], os.Args[3], os.Stdout); err != nil {
			fmt.Println("Summary:", err)
		} else {
			fmt.Println("Compilation successful")
		}

	case len(os.Args) > 2 && os.Args[1] == "-r" && len(os.Args[2]) > 0:
		if err := compiler.RunFile(os.Args[2], os.Stdout); err != nil {
			fmt.Println("Summary:", err)
		} else {
			fmt.Println("Run successful")
		}

	case len(os.Args) > 2 && os.Args[1] == "-p" && len(os.Args[2]) > 0:
		if err := compiler.PrintFile(os.Args[2], os.Stdout); err != nil {
			fmt.Println("Summary:", err)
		} else {
			fmt.Println("Print successful")
		}

	default:
		fmt.Println("Usage: pl0 -c <source file> <target file>")
		fmt.Println("       pl0 -r <target file>")
		fmt.Println("       pl0 -p <target file>")
	}
}
