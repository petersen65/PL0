// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

// Package compiler provides functions that compile PL/0 source code into IL/0 intermediate language code.
package compiler

import (
	"errors"
	"fmt"
	"io"
	"os"

	ast "github.com/petersen65/PL0/ast"
	emu "github.com/petersen65/PL0/emulator"
	gen "github.com/petersen65/PL0/generator"
	par "github.com/petersen65/PL0/parser"
	scn "github.com/petersen65/PL0/scanner"
	tok "github.com/petersen65/PL0/token"
)

// Options for the compiler.
const (
	PrintTokens         Options = 1 << iota
	PrintAbstractSyntax Options = 1 << iota
)

// Options for the compiler.
type Options uint64

// Compile a PL/0 source file into an IL/0 program file.
func CompileFile(source, target string, options Options, print io.Writer) error {
	if _, err := print.Write([]byte(fmt.Sprintf("Compiling PL0 source file '%v' to IL0 program '%v'\n", source, target))); err != nil {
		return err
	}

	if content, err := os.ReadFile(source); err != nil {
		return err
	} else if program, err := os.Create(target); err != nil {
		return err
	} else {
		defer program.Close()
		sections, tokenStream, abstractSyntax, errorHandler, err := CompileContent(content)

		if err != nil {
			if errorHandler.Count() != 0 {
				errorHandler.PrintErrorReport(print)
			}

			return err
		}

		if _, err := program.Write(sections); err != nil {
			return err
		}

		if options&PrintTokens != 0 {
			scn.PrintTokenStream(tokenStream, print, false)
		}

		if options&PrintAbstractSyntax != 0 {
			ast.PrintAbstractSyntaxTree(abstractSyntax, "", true, print)
		}
	}

	return nil
}

// Run an IL/0 program file.
func RunFile(target string, print io.Writer) error {
	if _, err := print.Write([]byte(fmt.Sprintf("Running IL0 program '%v'\n", target))); err != nil {
		return err
	}

	if sections, err := os.ReadFile(target); err != nil {
		return err
	} else if err := emu.RunSections(sections); err != nil {
		return err
	}

	return nil
}

// Print an IL/0 program file.
func PrintFile(target string, print io.Writer) error {
	if _, err := print.Write([]byte(fmt.Sprintf("Printing IL0 program '%v'\n", target))); err != nil {
		return err
	}

	if sections, err := os.ReadFile(target); err != nil {
		return err
	} else if err := emu.PrintSections(sections, print); err != nil {
		return err
	}

	return nil
}

// Compile PL/0 UTF-8 encoded source content into a binary IL/0 program and return the program as a byte slice.
func CompileContent(content []byte) ([]byte, tok.TokenStream, ast.Block, tok.ErrorHandler, error) {
	tokenStream, scannerError := scn.NewScanner().Scan(content)
	errorHandler := tok.NewErrorHandler(tokenStream)
	abstractSyntax, parserErr := par.NewParser().Parse(tokenStream, errorHandler)
	scannerParserErrors := errors.Join(scannerError, parserErr)

	if scannerParserErrors == nil {
		emitter := gen.NewGenerator(abstractSyntax).Generate()
		sections, emitterError := emitter.Export()
		return sections, tokenStream, abstractSyntax, errorHandler, emitterError
	}

	return nil, tokenStream, nil, errorHandler, scannerParserErrors
}

// Print one or several errors as summary to the specified writer.
func PrintErrorSummary(err error, print io.Writer) {
	tok.PrintErrorSummary(err, print)
}
