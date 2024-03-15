// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.
// Based on work Copyright (c) 1976, Niklaus Wirth, released in his book "Compilerbau, Teubner Studienbücher Informatik, 1986".

// Package emulator provides an emulation engine for IL/0.
package emulator

import "io"

// Run a binary IL/0 program and return an error if the program fails to execute.
func RunSections(sections []byte) error {
	return newMachine().runProgram(sections)
}

// Print a binary IL/0 program to the specified writer and return an error if the program fails to print.
func PrintSections(sections []byte, print io.Writer) error {
	return printProgram(sections, print)
}
