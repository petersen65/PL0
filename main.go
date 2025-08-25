// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package main implements the PL/0 compiler command line interface.
package main

import (
	"flag"
	"fmt"
	"os"
	"strings"

	com "github.com/petersen65/PL0/v2/compiler"
	cor "github.com/petersen65/PL0/v2/core"
)

// Text messages for the compiler command line interface.
const (
	textTitle         = "PL/0 1976 Compiler"
	textVersion       = "Version 3.0.0"
	textCopyright     = "Copyright (c) 2024-2025, Michael Petersen. All rights reserved."
	textCompilerUsage = "Usage of the compiler"
	textPurgeUsage    = "purge build directory before compiling"
	textCompileUsage  = "compile source code file to target assembly file"
	textLinkUsage     = "link object files to create the output executable"
	textOptimizeUsage = "apply optimization algorithms during compilation"
	textExportUsage   = "export intermediate representations to build directory"
	textSourceUsage   = "source code file"
	textTargetUsage   = "target assembly file"
	textHelpUsage     = "print help message"
)

// CommitHash is the git commit hash of the current build and will be set by the external build system.
var CommitHash string

// Function main is the entry point for the compiler command line interface. It parses the command line arguments and calls the appropriate functions.
func main() {
	const separator = ","
	var options com.DriverOption
	var help, compile, link, export, purge bool
	var source, target, optimize string
	var optimization cor.Optimization

	// define valid command line flags
	flag.BoolVar(&purge, "p", false, textPurgeUsage)
	flag.BoolVar(&purge, "purge", false, textPurgeUsage)
	flag.BoolVar(&compile, "c", false, textCompileUsage)
	flag.BoolVar(&compile, "compile", false, textCompileUsage)
	flag.StringVar(&optimize, "o", "", textOptimizeUsage)
	flag.BoolVar(&link, "l", false, textLinkUsage)
	flag.BoolVar(&link, "link", false, textLinkUsage)
	flag.StringVar(&optimize, "optimize", "", textOptimizeUsage)
	flag.BoolVar(&export, "e", false, textExportUsage)
	flag.BoolVar(&export, "export", false, textExportUsage)
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
		fmt.Fprintf(os.Stderr, "  -l | --link:    %v\n", textLinkUsage)
		fmt.Fprintf(os.Stderr, "  -o | --optimize:%v\n", textOptimizeUsage)
		fmt.Fprintf(os.Stderr, "  -e | --export:  %v\n", textExportUsage)
		fmt.Fprintf(os.Stderr, "  -s | --source:  %v\n", textSourceUsage)
		fmt.Fprintf(os.Stderr, "  -t | --target:  %v\n", textTargetUsage)
		fmt.Fprintf(os.Stderr, "  -h | --help:    %v\n", textHelpUsage)
	}

	// parse command line arguments into variables
	flag.Parse()

	// create driver display name used to identify the source of any generated code
	display := strings.TrimSpace(fmt.Sprintf("%v %v %v", textTitle, textVersion, CommitHash))

	// print title, version, and copyright
	fmt.Fprintln(os.Stdout, display)
	fmt.Fprintln(os.Stdout, textCopyright)

	// print help message if requested
	if help {
		flag.Usage()
		os.Exit(0)
	}

	// compile source code file to target assembly file
	if compile {
		options |= com.Compile

		// compile requires source and target files
		if source == "" || target == "" {
			flag.Usage()
			os.Exit(1)
		}
	}

	// link object files to create the output executable
	if link {
		options |= com.Link

		// link requires compile option
		if options&com.Compile == 0 {
			flag.Usage()
			os.Exit(1)
		}
	}

	// apply optimizations during compilation
	if optimize != "" {
		options |= com.Optimize

		// validate optimization flag values
		for value := range strings.SplitSeq(optimize, separator) {
			switch value {
			case cor.Debug.String():
				optimization |= cor.Debug

			case cor.Release.String():
				optimization |= cor.Release

			default:
				flag.Usage()
				os.Exit(1)
			}
		}

		// the debug optimization turns off all optimization algorithms and always overrides any release optimizations
		if optimization&cor.Debug != 0 {
			optimization = cor.Debug
		}

		// optimize requires compile option
		if options&com.Compile == 0 {
			flag.Usage()
			os.Exit(1)
		}
	} else {
		// default optimization is debug
		optimization = cor.Debug
	}

	// export intermediate representations to build directory
	if export {
		options |= com.Export

		// export requires compile option
		if options&com.Compile == 0 {
			flag.Usage()
			os.Exit(1)
		}
	}

	// purge build directory
	if purge {
		options |= com.Clean

		// purge requires build directory from target assembly file
		if target == "" {
			flag.Usage()
			os.Exit(1)
		}
	}

	// check if at least the compile or purge option is set
	if options&com.Compile == 0 && options&com.Clean == 0 {
		flag.Usage()
		os.Exit(1)
	}

	// call the compiler driver to run the compilation and link process
	com.Driver(options, source, target, optimization, display, os.Stdout)
}
