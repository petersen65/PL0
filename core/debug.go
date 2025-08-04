// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package core

type (
	DebugStringTable struct {
		CompilationUnit string
		Functions []FunctionDescription
	}

	FunctionDescription struct {
		Name      string
		Variables []VariableDescription
	}

	VariableDescription struct {
		Function     string
		Name         string
		TokenStreamIndex int
	}
)
