// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package emulator provides the emulation and JIT engine for AS/C.
package emulator

import cod "github.com/petersen65/PL0/v2/code"

// Run a binary AS/C target and return an error if the target fails to execute.
func Run(raw []byte) error {
	return newMachine().runProgram(raw)
}

// Load an IL/C module and return an error if the module fails to JIT compile and execute as AS/C.
func RunModule(module cod.Module) error {
	return newMachine().runProcess(module)
}
