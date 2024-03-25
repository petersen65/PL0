// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package main implements the PL/0 compiler command line interface.
package main

import (
	"flag"
	"fmt"
	"os"

	com "github.com/petersen65/PL0/compiler"
)

// Text messages for the compiler command line interface.
const (
	textTitle        = "PL/0 Compiler"
	textVersion      = "Version 2.0.0 2024"
	textCopyright    = "Copyright (c) 2024, Michael Petersen. All rights reserved."
	textCompilerUse  = "Usage of PL/0 compiler"
	textCompileUsage = "compile PL/0 source file to IL/0 target file"
	textRunUsage     = "run IL/0 target file"
	textExportUsage  = "export intermediate representations to TOK/AST/COD target files"
	textSourceUsage  = "PL/0 source file"
	textTargetUsage  = "IL/0 target file"
)

// Function main is the entry point for the PL/0 compiler command line interface. It parses the command line arguments and calls the appropriate functions.
func main() {
	var options com.DriverOptions
	var compile, run, export bool
	var source, target string

	flag.BoolVar(&compile, "c", false, textCompileUsage)
	flag.BoolVar(&compile, "compile", false, textCompileUsage)
	flag.BoolVar(&run, "r", false, textRunUsage)
	flag.BoolVar(&run, "run", false, textRunUsage)
	flag.BoolVar(&export, "e", false, textExportUsage)
	flag.BoolVar(&export, "export", false, textExportUsage)
	flag.StringVar(&source, "s", "", textSourceUsage)
	flag.StringVar(&source, "source", "", textSourceUsage)
	flag.StringVar(&target, "t", "", textTargetUsage)
	flag.StringVar(&target, "target", "", textTargetUsage)

	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "%v:\n", textCompilerUse)
		fmt.Fprintf(os.Stderr, "  -c | --compile: %v\n", textCompileUsage)
		fmt.Fprintf(os.Stderr, "  -r | --run:     %v\n", textRunUsage)
		fmt.Fprintf(os.Stderr, "  -e | --export:  %v\n", textExportUsage)
		fmt.Fprintf(os.Stderr, "  -s | --source:  %v\n", textSourceUsage)
		fmt.Fprintf(os.Stderr, "  -t | --target:  %v\n", textTargetUsage)
	}

	flag.Parse()
	fmt.Fprintf(os.Stdout, "%v %v\n", textTitle, textVersion)
	fmt.Fprintf(os.Stdout, "%v\n", textCopyright)

	if compile {
		options |= com.Compile

		if source == "" || target == "" {
			flag.Usage()
			os.Exit(1)
		}
	}

	if run {
		options |= com.Emulate

		if target == "" {
			flag.Usage()
			os.Exit(1)
		}
	}

	if export {
		options |= com.Export

		if options&com.Compile == 0 || source == "" || target == "" {
			flag.Usage()
			os.Exit(1)
		}
	}

	com.Driver(options, source, target, os.Stdout)
}
