// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package emulator provides the emulation engine for assembly instructions.
package emulator

// The Machine interface provides methods for loading and running processes.
type Machine interface {
	Load(raw []byte) error
	RunProcess() error
}

// Create a new emulation machine with CPU, registers, memory, and stack that can run processes.
func NewMachine() Machine {
	return newMachine()
}
