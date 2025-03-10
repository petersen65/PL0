// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package cfg implements the control graph representation (CFG) of intermediate code.
package cfg

import (
	"io"

	cor "github.com/petersen65/PL0/v2/core"
	gen "github.com/petersen65/PL0/v2/generator"
)

type (
	// ControlFlowGraph represents the control flow graph of an intermediate code unit.
	ControlFlowGraph interface {
		Build()
		AppendBasicBlock(basicBlock *basicBlock)
		Print(print io.Writer, args ...any) error
		Export(format cor.ExportFormat, print io.Writer) error
	}

	// Basic blocks are maximal sequences of consecutive instructions that execute without branching.
	BasicBlock interface {
		String() string
		AppendInstruction(instruction *gen.Instruction)
	}

	// Edges represent the control flow between basic blocks in the control flow graph.
	Edge interface {
	}
)

// Creates a new control flow graph for a specified intermediate code unit.
func NewControlFlowGraph(intermediateCode gen.IntermediateCodeUnit) ControlFlowGraph {
	return newControlFlowGraph(intermediateCode)
}

// NewBasicBlock creates a new basic block.
func NewBasicBlock() BasicBlock {
	return newBasicBlock()
}

// NewEdge creates a new edge between two basic blocks.
func NewEdge(from, to *basicBlock) Edge {
	return newEdge(from, to)
}
