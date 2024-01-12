// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package compiler

import (
	"fmt"
	"io"
	"os"
	"strings"

	emt "github.com/petersen65/PL0/emitter"
	par "github.com/petersen65/PL0/parser"
	scn "github.com/petersen65/PL0/scanner"
)

func CompileFile(source, target string, print io.Writer) error {
	if _, err := print.Write([]byte(fmt.Sprintf("Compiling PL0 source file '%v' to IL0 program '%v'\n", source, target))); err != nil {
		return err
	}

	if content, err := os.ReadFile(source); err != nil {
		return err
	} else if program, err := os.Create(target); err != nil {
		return err
	} else {
		defer program.Close()
		sections, concreteSyntax, errorReport, err := CompileContent(content)

		switch {
		case err != nil && concreteSyntax != nil && errorReport != nil:
			PrintErrorReport(errorReport, print)
			return err

		case err != nil && concreteSyntax != nil && errorReport == nil:
			PrintConcreteSyntax(concreteSyntax, print)
			return err

		default:
			if _, err := program.Write(sections); err != nil {
				return err
			}
		}
	}

	return nil
}

func RunFile(target string, print io.Writer) error {
	if _, err := print.Write([]byte(fmt.Sprintf("Running IL0 program '%v'\n", target))); err != nil {
		return err
	}

	if sections, err := os.ReadFile(target); err != nil {
		return err
	} else if err := RunSections(sections); err != nil {
		return err
	}

	return nil
}

func PrintFile(target string, print io.Writer) error {
	if _, err := print.Write([]byte(fmt.Sprintf("Printing IL0 program '%v'\n", target))); err != nil {
		return err
	}

	if sections, err := os.ReadFile(target); err != nil {
		return err
	} else if err := PrintSections(sections, print); err != nil {
		return err
	}

	return nil
}

func CompileContent(content []byte) ([]byte, scn.ConcreteSyntax, par.ErrorReport, error) {
	scanner := scn.NewScanner()

	if concreteSyntax, err := scanner.Scan(content); err != nil {
		return nil, concreteSyntax, nil, err
	} else {
		emitter := emt.NewEmitter()
		parser := par.NewParser()

		if errorReport, err := parser.Parse(concreteSyntax, emitter); err != nil {
			return nil, concreteSyntax, errorReport, err
		} else if sections, err := emitter.Export(); err != nil {
			return nil, concreteSyntax, errorReport, err
		} else {
			return sections, concreteSyntax, errorReport, nil
		}
	}
}

func RunSections(sections []byte) error {
	return newMachine().runProgram(sections)
}

func PrintSections(sections []byte, print io.Writer) error {
	return newMachine().printProgram(sections, print)
}

func PrintConcreteSyntax(concreteSyntax scn.ConcreteSyntax, print io.Writer) {
	var lastLine int
	print.Write([]byte("Concrete Syntax:"))

	for _, td := range concreteSyntax {
		if td.Line != lastLine {
			print.Write([]byte(fmt.Sprintf("\n%v: %v\n", td.Line, strings.TrimSpace(string(td.CurrentLine)))))
			lastLine = td.Line
		}

		print.Write([]byte(fmt.Sprintf("%v,%-5v %v %v\n", td.Line, td.Column, td.TokenName, td.TokenValue)))
	}
}

func PrintErrorReport(errorReport par.ErrorReport, print io.Writer) {
	print.Write([]byte("Error Report:"))

	for _, e := range errorReport {
		linePrefix := fmt.Sprintf("%5v: ", e.Line)
		trimmedLine := strings.TrimSpace(string(e.CurrentLine))
		trimmedSpaces := len(string(e.CurrentLine)) - len(trimmedLine)

		print.Write([]byte(fmt.Sprintf("\n%v%v\n", linePrefix, trimmedLine)))
		print.Write([]byte(fmt.Sprintf("%v^ %v\n", strings.Repeat(" ", e.Column+len(linePrefix)-trimmedSpaces-1), e.Err)))
	}
}
