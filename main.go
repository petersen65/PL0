// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package main implements the PL/0 compiler command line interface.
package main

import (
	"flag"
	"fmt"
	"os"

	com "github.com/petersen65/PL0/v2/compiler"
)

// Text messages for the compiler command line interface.
const (
	textTitle         = "PL/0 Compiler"
	textVersion       = "Version 2.1.0 2024"
	textCopyright     = "Copyright (c) 2024, Michael Petersen. All rights reserved."
	textCompilerUsage = "Usage of PL/0 compiler"
	textPurgeUsage    = "purge target directory before compiling"
	textCompileUsage  = "compile PL/0 source file to IL/0 target file"
	textExportUsage   = "export intermediate representations to {.tok .ast .int .err} target files"
	textRunUsage      = "run IL/0 target file"
	textSourceUsage   = "PL/0 source file"
	textTargetUsage   = "IL/0 target file"
	textHelpUsage     = "print help message"
)

// CommitHash is the git commit hash of the current build and will be set by the external build system.
var CommitHash string

// Function main is the entry point for the PL/0 compiler command line interface. It parses the command line arguments and calls the appropriate functions.
func main() {
	var options com.DriverOption
	var help, compile, run, export, purge bool
	var source, target string

	// define valid command line flags
	flag.BoolVar(&purge, "p", false, textPurgeUsage)
	flag.BoolVar(&purge, "purge", false, textPurgeUsage)
	flag.BoolVar(&compile, "c", false, textCompileUsage)
	flag.BoolVar(&compile, "compile", false, textCompileUsage)
	flag.BoolVar(&export, "e", false, textExportUsage)
	flag.BoolVar(&export, "export", false, textExportUsage)
	flag.BoolVar(&run, "r", false, textRunUsage)
	flag.BoolVar(&run, "run", false, textRunUsage)
	flag.StringVar(&source, "s", "", textSourceUsage)
	flag.StringVar(&source, "source", "", textSourceUsage)
	flag.StringVar(&target, "t", "", textTargetUsage)
	flag.StringVar(&target, "target", "", textTargetUsage)
	flag.BoolVar(&help, "h", false, textHelpUsage)
	flag.BoolVar(&help, "help", false, textHelpUsage)

	// define usage function for command line flags
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "%v:\n", textCompilerUsage)
		fmt.Fprintf(os.Stderr, "  -p | --purge:   %v\n", textPurgeUsage)
		fmt.Fprintf(os.Stderr, "  -c | --compile: %v\n", textCompileUsage)
		fmt.Fprintf(os.Stderr, "  -e | --export:  %v\n", textExportUsage)
		fmt.Fprintf(os.Stderr, "  -r | --run:     %v\n", textRunUsage)
		fmt.Fprintf(os.Stderr, "  -s | --source:  %v\n", textSourceUsage)
		fmt.Fprintf(os.Stderr, "  -t | --target:  %v\n", textTargetUsage)
		fmt.Fprintf(os.Stderr, "  -h | --help:    %v\n", textHelpUsage)
	}

	// parse command line arguments into variables
	flag.Parse()

	// print title, version, and copyright
	fmt.Fprintf(os.Stdout, "%v %v %v\n", textTitle, textVersion, CommitHash)
	fmt.Fprintf(os.Stdout, "%v\n", textCopyright)

	// print help message if requested
	if help {
		flag.Usage()
		os.Exit(0)
	}

	// compile PL/0 source to IL/0 target
	if compile {
		options |= com.Compile

		if source == "" || target == "" {
			flag.Usage()
			os.Exit(1)
		}
	}

	// run IL/0 target
	if run {
		options |= com.Emulate

		if target == "" {
			flag.Usage()
			os.Exit(1)
		}
	}

	// export intermediate representations as {.tok .ast .int .err} targets
	if export {
		options |= com.Export

		// export requires compile option and source and target files
		if options&com.Compile == 0 || source == "" || target == "" {
			flag.Usage()
			os.Exit(1)
		}
	}

	// purge target directory
	if purge {
		options |= com.Clean

		// purge and run options without compilation do not make sense
		if (options&com.Compile == 0 && options&com.Emulate != 0) || target == "" {
			flag.Usage()
			os.Exit(1)
		}
	}

	// check if at least the compile, run, or purge option is set
	if options&com.Compile == 0 && options&com.Emulate == 0 && options&com.Clean == 0 {
		flag.Usage()
		os.Exit(1)
	}

	// call the compiler driver with the options and source and target files
	com.Driver(options, source, target, os.Stdout)
}
