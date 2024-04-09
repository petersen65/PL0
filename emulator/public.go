// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package emulator provides the emulation and JIT engine.
package emulator

import cod "github.com/petersen65/PL0/v2/code"

// Run a binary target and return an error if the target fails to execute.
func Run(raw []byte) error {
	return newMachine().runRaw(raw)
}

// Load a module and return an error if the module fails to JIT compile and execute.
func RunModule(module cod.Module) error {
	return newMachine().runModule(module)
}
