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
	"strings"

	ast "github.com/petersen65/PL0/ast"
	emu "github.com/petersen65/PL0/emulator"
	gen "github.com/petersen65/PL0/generator"
	par "github.com/petersen65/PL0/parser"
	scn "github.com/petersen65/PL0/scanner"
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
		sections, tokenStream, abstractSyntax, errorReport, err := CompileContent(content)

		if err != nil {
			if len(errorReport) != 0 {
				PrintErrorReport(errorReport, print)
			}

			return err
		}

		if _, err := program.Write(sections); err != nil {
			return err
		}

		if options&PrintTokens != 0 {
			PrintTokenStream(tokenStream, print, false)
		}

		if options&PrintAbstractSyntax != 0 {
			PrintAbstractSyntaxTree(abstractSyntax, "", true, print)
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
// The token stream and error report are also returned if an error occurs during compilation.
func CompileContent(content []byte) ([]byte, scn.TokenStream, ast.Block, par.ErrorReport, error) {
	tokenStream, scannerError := scn.NewScanner().Scan(content)
	abstractSyntax, errorReport, parserErr := par.NewParser().Parse(tokenStream)
	scannerParserErrors := errors.Join(scannerError, parserErr)

	if scannerParserErrors == nil {
		emitter := gen.NewGenerator(abstractSyntax).Generate()
		sections, emitterError := emitter.Export()
		return sections, tokenStream, abstractSyntax, errorReport, emitterError
	}

	return nil, tokenStream, nil, errorReport, scannerParserErrors
}

// Print the token stream of the scanner to the specified writer.
func PrintTokenStream(tokenStream scn.TokenStream, print io.Writer, bottom bool) {
	var start, previousLine int
	print.Write([]byte("Token Stream:"))

	if len(tokenStream) == 0 {
		print.Write([]byte("\n"))
		return
	}

	if bottom {
		lastLine := tokenStream[len(tokenStream)-1].Line

		for start = len(tokenStream) - 1; start >= 0 && tokenStream[start].Line == lastLine; start-- {
		}

		start++
	} else {
		start = 0
	}

	for i := start; i < len(tokenStream); i++ {
		td := tokenStream[i]

		if td.Line != previousLine {
			print.Write([]byte(fmt.Sprintf("\n%v: %v\n", td.Line, strings.TrimSpace(string(td.CurrentLine)))))
			previousLine = td.Line
		}

		print.Write([]byte(fmt.Sprintf("%v,%-5v %v %v\n", td.Line, td.Column, td.TokenName, td.TokenValue)))
	}
}

// Print the abstract syntax tree of the parser to the specified writer.
func PrintAbstractSyntaxTree(node ast.Node, indent string, last bool, print io.Writer) {
	print.Write([]byte(fmt.Sprintf("%v+- %v\n", indent, node)))

	if last {
		indent += "   "
	} else {
		indent += "|  "
	}

	for i, child := range node.Children() {
		PrintAbstractSyntaxTree(child, indent, i == len(node.Children())-1, print)
	}
}

// Print one or several errors to the specified writer.
func PrintError(err error, print io.Writer) {
	if strings.Contains(err.Error(), "\n") {
		print.Write([]byte(fmt.Sprintf("Errors Summary:\n%v\n", err)))
	} else {
		print.Write([]byte(fmt.Sprintf("Error Summary: %v\n", err)))
	}
}

// Print the error report of the parser to the specified writer.
func PrintErrorReport(errorReport par.ErrorReport, print io.Writer) {
	print.Write([]byte("Error Report:"))

	if len(errorReport) == 0 {
		print.Write([]byte("\n"))
		return
	}

	for _, e := range errorReport {
		linePrefix := fmt.Sprintf("%5v: ", e.Line)
		trimmedLine := strings.TrimSpace(string(e.CurrentLine))
		trimmedSpaces := len(string(e.CurrentLine)) - len(trimmedLine)

		print.Write([]byte(fmt.Sprintf("\n%v%v\n", linePrefix, trimmedLine)))
		print.Write([]byte(fmt.Sprintf("%v^ %v\n", strings.Repeat(" ", e.Column+len(linePrefix)-trimmedSpaces-1), e.Err)))
	}
}
