// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package emitter implements the emission of assembly code for a target platform based on an intermediate code representation.
package emitter

import (
	cor "github.com/petersen65/PL0/v2/core"
	x64 "github.com/petersen65/PL0/v2/emitter/x86_64"
	ic "github.com/petersen65/PL0/v2/generator/intermediate"
)

type (
	// Translate intermediate code into assembly code for a specific target platform and build configuration.
	Emitter interface {
		Emit()
		GetAssemblyCodeUnit() x64.AssemblyCodeUnit
	}
)

// Return the interface of the emitter implementation.
func NewEmitter(intermediateCode ic.IntermediateCodeUnit, buildConfiguration cor.BuildConfiguration, debugInformation cor.DebugInformation) Emitter {
	return newEmitter(intermediateCode, buildConfiguration, debugInformation)
}
