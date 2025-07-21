// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package emitter implements the assembly code generation compiler phase by iterating over the intermediate code unit.
package emitter

import (
	cor "github.com/petersen65/PL0/v2/core"
	x64 "github.com/petersen65/PL0/v2/emitter/x86_64"
	ic "github.com/petersen65/PL0/v2/generator/intermediate"
)

type (
	// The Emitter interface provides methods for emitting assembly code for the target platform.
	Emitter interface {
		Emit()
		GetAssemblyCodeUnit() x64.AssemblyCodeUnit
	}
)

// Return the interface of the emitter implementation.
func NewEmitter(target cor.TargetPlatform, intermediateCode ic.IntermediateCodeUnit) Emitter {
	return newEmitter(target, intermediateCode)
}
