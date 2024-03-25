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
	cor "github.com/petersen65/PL0/core"
	emu "github.com/petersen65/PL0/emulator"
	gen "github.com/petersen65/PL0/generator"
	par "github.com/petersen65/PL0/parser"
	scn "github.com/petersen65/PL0/scanner"
)

// Text messages for the compilation driver.
const (
	textCompiling          = "Compiling PL0 source '%v' to IL0 target '%v'\n"
	textErrorCompiling     = "Error compiling PL0 source '%v': %v\n"
	textErrorPersisting    = "Error persisting IL0 target '%v': %v\n"
	textPrinting           = "Printing IL0 target '%v'\n"
	textErrorPrinting      = "Error printing IL0 target '%v': %v\n"
	textEmulating          = "Emulating IL0 target '%v'\n"
	textErrorEmulating     = "Error emulating IL0 target '%v': %v\n"
	textDriverSourceTarget = "Compiler Driver with PL0 source '%v' and IL0 target '%v' completed\n"
	textDriverTarget       = "Compiler Driver with IL0 target '%v' completed\n"
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
		TokenStream    cor.TokenStream  // token stream of the PL/0 source content
		AbstractSyntax ast.Block        // abstract syntax tree of the PL/0 source code
		Sections       []byte           // IL/0 intermediate language code
		ErrorHandler   cor.ErrorHandler // error handler of the compilation process
	}
)

// Driver for the compilation process with the given options, source, target, and print writer.
func Driver(options DriverOptions, source, target string, print io.Writer) {
	var translationUnit TranslationUnit
	var err error

	// compile PL/0 source to IL/0 target and persist IL/0 sections to target
	if options&Compile != 0 {
		print.Write([]byte(fmt.Sprintf(textCompiling, source, target)))
		translationUnit, err = CompileSourceToTranslationUnit(source)

		if err != nil {
			print.Write([]byte(fmt.Sprintf(textErrorCompiling, source, err)))
			return
		}

		if err = PersistSectionsToTarget(translationUnit.Sections, target); err != nil {
			print.Write([]byte(fmt.Sprintf(textErrorPersisting, target, err)))
			return
		}

		// print error report if errors with any severity occurred during compilation process
		translationUnit.ErrorHandler.PrintErrorReport(print)
	}

	// print token stream
	if options&Compile != 0 && options&PrintTokenStream != 0 {
		translationUnit.TokenStream.Print(print)
	}

	// print abstract syntax tree
	if options&Compile != 0 && options&PrintAbstractSyntaxTree != 0 {
		translationUnit.AbstractSyntax.(*ast.BlockNode).Print(print)
	}

	// print intermediate code
	if options&PrintIntermediateCode != 0 {
		print.Write([]byte(fmt.Sprintf(textPrinting, target)))

		if err = PrintTarget(target, print); err != nil {
			print.Write([]byte(fmt.Sprintf(textErrorPrinting, target, err)))
			return
		}
	}

	// emulate IL/0 target
	if options&Emulate != 0 {
		print.Write([]byte(fmt.Sprintf(textEmulating, target)))

		if err = EmulateTarget(target); err != nil {
			print.Write([]byte(fmt.Sprintf(textErrorEmulating, target, err)))
			return
		}
	}

	// print driver completion message
	if options&Compile != 0 {
		print.Write([]byte(fmt.Sprintf(textDriverSourceTarget, source, target)))
	} else {
		print.Write([]byte(fmt.Sprintf(textDriverTarget, target)))
	}
}

// Compile PL/0 source and return translation unit with all intermediate results and error handler.
func CompileSourceToTranslationUnit(source string) (TranslationUnit, error) {
	if content, err := os.ReadFile(source); err != nil {
		return TranslationUnit{}, err
	} else {
		return CompileContent(content), nil
	}
}

// Persist IL/0 sections to the given target.
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

// Compile PL/0 UTF-8 encoded content and return translation unit with all intermediate results and error handler.
func CompileContent(content []byte) TranslationUnit {
	// lexical analysis of PL/0 content
	tokenStream, scannerError := scn.NewScanner().Scan(content)
	errorHandler := cor.NewErrorHandler(tokenStream)
	errorHandler.AppendError(scannerError) // nil errors are ignored

	// syntax analysis and semantic analysis of PL/0 token stream
	abstractSyntax, tokenHandler := par.NewParser(tokenStream, errorHandler).Parse()
	ana.NewNameAnalysis(abstractSyntax, errorHandler, tokenHandler).Analyze()

	// return if any fatal or error errors occurred during lexical, syntax, or semantic analysis
	if errorHandler.Count(cor.Fatal|cor.Error, cor.AllComponents) > 0 {
		return TranslationUnit{content, nil, nil, nil, errorHandler}
	}

	// code generation and emission of IL/0 intermediate language code based on abstract syntax tree
	emitter := gen.NewGenerator(abstractSyntax).Generate()
	sections, emitterError := emitter.Export()
	errorHandler.AppendError(emitterError) // nil errors are ignored

	// return if any fatal or error errors occurred during code generation and emission
	if errorHandler.Count(cor.Fatal|cor.Error, cor.AllComponents) > 0 {
		return TranslationUnit{content, nil, nil, nil, errorHandler}
	}

	// return translation unit with all intermediate results and error handler
	return TranslationUnit{content, tokenStream, abstractSyntax, sections, errorHandler}
}
