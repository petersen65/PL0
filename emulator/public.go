// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package emulator provides the emulation engine for IL/0.
package emulator

import cod "github.com/petersen65/PL0/v2/code"

// Run a binary IL/0 target and return an error if the target fails to execute.
func Run(raw []byte) error {
	return newMachine().runProgram(raw)
}

// Run an IL/C module and return an error if the module fails to execute.
func RunProcess(module cod.Module) error {
	return newMachine().runProcess(module)
}
