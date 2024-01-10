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
	fmt.Println("PL/0 Compiler Version 1.0")
	fmt.Println("Copyright (c) 2024, Michael Petersen. All rights reserved.")

	if len(os.Args) < 3 {
		fmt.Println("Usage: pl0 <source file> <target file>")
	} else if err := compiler.CompileFile(os.Args[1], os.Args[2]); err != nil {
		fmt.Println("\nerror:", err)
	} else {
		fmt.Println("Compilation successful")
	}
}
