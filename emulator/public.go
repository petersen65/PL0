// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package emulator provides the emulation and JIT engine.
package emulator

import (
	"io"

	cod "github.com/petersen65/PL0/v2/code"
	cor "github.com/petersen65/PL0/v2/core"
)

// Virtual machine that can run processes and modules.
type Machine interface {
	Load(raw []byte) error
	LoadModule(module cod.Module) error
	RunProcess() error
	Print(print io.Writer, args ...any) error
	Export(format cor.ExportFormat, print io.Writer) error
}

// Create a new emulation machine with CPU, registers and stack that can run binary processes and modules.
func NewMachine() Machine {
	return newMachine()
}
