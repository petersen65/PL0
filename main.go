package main

import (
	"fmt"
	"os"

	"github.com/petersen65/PL0/compiler"
)

func main() {
	fmt.Println("PL/0 Compiler Version 0.1")
	fmt.Println("Copyright (c) 2024, Michael Petersen. All rights reserved.")

	if len(os.Args) < 2 {
		fmt.Println("Usage: pl0 <source file>")
		fmt.Println("error: no source file specified")
	} else {
		compiler.CompileFile(os.Args[1])
	}
}
