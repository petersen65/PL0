// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package compiler provides functions that compile PL/0 source code into IL/0 intermediate language code.
package compiler

import (
	"fmt"
	"io"
	"os"

	ana "github.com/petersen65/PL0/analyzer"
	ast "github.com/petersen65/PL0/ast"
	emu "github.com/petersen65/PL0/emulator"
	gen "github.com/petersen65/PL0/generator"
	par "github.com/petersen65/PL0/parser"
	scn "github.com/petersen65/PL0/scanner"
	tok "github.com/petersen65/PL0/token"
)

// Options for the compilation driver as bit-mask.
const (
	Compile DriverOptions = 1 << iota
	PrintTokenStream
	PrintAbstractSyntaxTree
	PrintIntermediateCode
	Emulate
)

type (
	// DriverOptions for the compilation process.
	DriverOptions uint64

	// TranslationUnit represents source content and all intermediate results of the compilation process.
	TranslationUnit struct {
		SourceContent  []byte           // PL/0 utf-8 source content
		TokenStream    tok.TokenStream  // token stream of the PL/0 source content
		AbstractSyntax ast.Block        // abstract syntax tree of the PL/0 source code
		Sections       []byte           // IL/0 intermediate language code
		ErrorHandler   tok.ErrorHandler // error handler of the compilation process
	}
)

func Driver(options DriverOptions, source, target string, print io.Writer) {
	var translationUnit TranslationUnit
	var err error

	if options&Compile != 0 {
		print.Write([]byte(fmt.Sprintf("Compiling PL0 source '%v' to IL0 target '%v'\n", source, target)))
		translationUnit, err = CompileSourceToTranslationUnit(source)

		if err != nil {
			print.Write([]byte(fmt.Sprintf("Error compiling PL0 source '%v': %v\n", source, err)))
			return
		}

		if err = PersistSectionsToTarget(translationUnit.Sections, target); err != nil {
			print.Write([]byte(fmt.Sprintf("Error persisting IL0 target '%v': %v\n", target, err)))
			return
		}

		// print error report if errors with any severity occurred during compilation process
		translationUnit.ErrorHandler.PrintErrorReport(print)
	}

	if options&Compile != 0 && options&PrintTokenStream != 0 {
		translationUnit.TokenStream.Print(print, false)
	}

	if options&Compile != 0 && options&PrintAbstractSyntaxTree != 0 {
		ast.PrintAbstractSyntaxTree(translationUnit.AbstractSyntax, "", true, print)
	}

	if options&PrintIntermediateCode != 0 {
		print.Write([]byte(fmt.Sprintf("Printing IL0 target '%v'\n", target)))

		if err = PrintTarget(target, print); err != nil {
			print.Write([]byte(fmt.Sprintf("Error printing IL0 target '%v': %v\n", target, err)))
			return
		}
	}

	if options&Emulate != 0 {
		print.Write([]byte(fmt.Sprintf("Emulating IL0 target '%v'\n", target)))

		if err = EmulateTarget(target); err != nil {
			print.Write([]byte(fmt.Sprintf("Error emulating IL0 target '%v': %v\n", target, err)))
			return
		}
	}

	print.Write([]byte(fmt.Sprintf("Compiler Driver with PL0 source '%v' and IL0 target '%v' completed\n", source, target)))
}

func CompileSourceToTranslationUnit(source string) (TranslationUnit, error) {
	if content, err := os.ReadFile(source); err != nil {
		return TranslationUnit{}, err
	} else {
		return CompileContent(content), nil
	}
}

func PersistSectionsToTarget(sections []byte, target string) error {
	if program, err := os.Create(target); err != nil {
		return err
	} else {
		defer program.Close()

		if _, err := program.Write(sections); err != nil {
			return err
		}
	}

	return nil
}

// Print IL/0 target to the given writer.
func PrintTarget(target string, print io.Writer) error {
	if sections, err := os.ReadFile(target); err != nil {
		return err
	} else if err := emu.PrintSections(sections, print); err != nil {
		return err
	}

	return nil
}

// Run IL/0 target with the emulator.
func EmulateTarget(target string) error {
	if sections, err := os.ReadFile(target); err != nil {
		return err
	} else if err := emu.RunSections(sections); err != nil {
		return err
	}

	return nil
}

// Compile PL/0 UTF-8 encoded source content and return translation unit with all intermediate results and error handler.
func CompileContent(content []byte) TranslationUnit {
	// lexical analysis of PL/0 source content
	tokenStream, scannerError := scn.NewScanner().Scan(content)
	errorHandler := tok.NewErrorHandler(tokenStream)
	errorHandler.AppendError(scannerError) // nil errors are ignored

	// syntax analysis and semantic analysis of PL/0 token stream
	abstractSyntax, tokenHandler := par.NewParser(tokenStream, errorHandler).Parse()
	ana.NewNameAnalysis(abstractSyntax, errorHandler, tokenHandler).Analyze()

	// return if any fatal or error errors occurred during lexical, syntax, or semantic analysis
	if errorHandler.Count(tok.Fatal|tok.Error, tok.AllComponents) > 0 {
		return TranslationUnit{content, nil, nil, nil, errorHandler}
	}

	// code generation and emission of IL/0 intermediate language code based on abstract syntax tree
	emitter := gen.NewGenerator(abstractSyntax).Generate()
	sections, emitterError := emitter.Export()
	errorHandler.AppendError(emitterError) // nil errors are ignored

	// return if any fatal or error errors occurred during code generation and emission
	if errorHandler.Count(tok.Fatal|tok.Error, tok.AllComponents) > 0 {
		return TranslationUnit{content, nil, nil, nil, errorHandler}
	}

	// return translation unit with all intermediate results and error handler
	return TranslationUnit{content, tokenStream, abstractSyntax, sections, errorHandler}
}

/*




// Compile a PL/0 source file into an IL/0 program file and drive the compilation process with the given options.
func CompileFile(source, target string, options DriverOptions, print io.Writer) {
	print.Write([]byte(fmt.Sprintf("Compiling PL0 source file '%v' to IL0 program '%v'\n", source, target)))

	if content, err := os.ReadFile(source); err != nil {
		print.Write([]byte(fmt.Sprintf("Error reading PL0 source file '%v': %v\n", source, err)))
		return
	} else if program, err := os.Create(target); err != nil {
		print.Write([]byte(fmt.Sprintf("Error creating IL0 program file '%v': %v\n", target, err)))
		return
	} else {
		// source and target were successfully opened/created, so close them before returning from this function
		defer program.Close()

		// compile PL/0 source content into IL/0 program content
		tokenStream, abstractSyntax, sections, errorHandler := CompileContent(content)

		// print error report if errors with any severity occurred during compilation process
		errorHandler.PrintErrorReport(print)

		// write IL/0 program content to target file
		if sections != nil {
			if _, err := program.Write(sections); err != nil {
				print.Write([]byte(fmt.Sprintf("Error writing IL0 program file '%v': %v\n", target, err)))
				return
			}
		}

		// print token stream if requested by the caller
		if options&PrintTokenStream != 0 {
			scn.PrintTokenStream(tokenStream, print, false)
		}

		// print abstract syntax tree if requested by the caller
		if options&PrintAbstractSyntaxTree != 0 {
			ast.PrintAbstractSyntaxTree(abstractSyntax, "", true, print)
		}

		// print intermediate code if requested by the caller
		if sections != nil && options&PrintIntermediateCode != 0 {
			program.Close() // close the program file before printing it

			if err := PrintTarget(target, print); err != nil {
				print.Write([]byte(fmt.Sprintf("Error printing IL0 program file '%v': %v\n", target, err)))
				return
			}
		}

		// run the IL/0 program if requested by the caller
		if sections != nil && options&EmulateProgram != 0 {
			program.Close() // close the program file before running it

			if err := EmulateTarget(target, print); err != nil {
				print.Write([]byte(fmt.Sprintf("Error running IL0 program file '%v': %v\n", target, err)))
				return
			}
		}

		print.Write([]byte(fmt.Sprintf("Compilation of PL0 source file '%v' to IL0 program '%v' completed\n", source, target)))
	}
}
*/