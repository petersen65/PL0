// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

package compiler

import (
	"fmt"
	"os"
	"strings"

	emt "github.com/petersen65/PL0/emitter"
	par "github.com/petersen65/PL0/parser"
	scn "github.com/petersen65/PL0/scanner"
)

func CompileFile(source, target string) error {
	fmt.Printf("Compiling PL0 source file '%v' to IL0 program '%v'\n", source, target)

	if content, err := os.ReadFile(source); err != nil {
		return err
	} else if program, err := os.Create(target); err != nil {
		return err
	} else {
		defer program.Close()
		sections, concreteSyntax, errorReport, err := CompileContent(content)

		switch {
		case err != nil && concreteSyntax != nil && errorReport != nil:
			PrintErrorReport(errorReport)
			return err

		case err != nil && concreteSyntax != nil && errorReport == nil:
			PrintConcreteSyntax(concreteSyntax)
			return err

		default:
			if _, err := program.Write(sections); err != nil {
				return err
			}
		}
	}

	return nil
}

func RunFile(target string) error {
	fmt.Printf("Running IL0 program '%v'\n", target)

	if sections, err := os.ReadFile(target); err != nil {
		return err
	} else if err := RunSections(sections); err != nil {
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
	machine := newMachine()
	return machine.startProcess(sections)
}

func PrintConcreteSyntax(concreteSyntax scn.ConcreteSyntax) {
	var lastLine int

	fmt.Print("Concrete Syntax:")

	for _, td := range concreteSyntax {
		if td.Line != lastLine {
			fmt.Printf("\n%v: %v\n", td.Line, strings.TrimSpace(string(td.CurrentLine)))
			lastLine = td.Line
		}

		fmt.Printf("%v,%v\t%v %v\n", td.Line, td.Column, td.TokenName, td.TokenValue)
	}
}

func PrintErrorReport(errorReport par.ErrorReport) {
	fmt.Println("Error Report:")

	for _, e := range errorReport {
		fmt.Printf("\n%v: %v\n", e.Line, string(e.CurrentLine))
		fmt.Printf("%v^ %v\n", strings.Repeat(" ", e.Column+1), e.Err)
	}
}
