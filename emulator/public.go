// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package emulator provides the emulation and JIT engine for assembly instructions.
package emulator

import (
	"io"

	cod "github.com/petersen65/PL0/v2/code"
	cor "github.com/petersen65/PL0/v2/core"
)

// The Machine interface provides methods for loading, linking, and running processes.
type Machine interface {
	Load(raw []byte) error
	LoadModule(module cod.Module) error
	Link() error
	RunProcess() error
	Print(print io.Writer, args ...any) error
	Export(format cor.ExportFormat, print io.Writer) error
}

// Create a new emulation machine with CPU, registers, memory, and stack that can run processes.
func NewMachine() Machine {
	return newMachine()
}
