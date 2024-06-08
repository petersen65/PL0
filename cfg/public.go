// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package cfg implements the control graph representation (CFG) of intermediate code.
package cfg

import (
	"io"

	cod "github.com/petersen65/PL0/v2/code"
	cor "github.com/petersen65/PL0/v2/core"
)

type (
	// ControlFlowGraph represents the control flow graph of an intermediate code module.
	ControlFlowGraph interface {
		Build()
		AppendBasicBlock(basicBlock *basicBlock)
		Print(print io.Writer, args ...any) error
		Export(format cor.ExportFormat, print io.Writer) error
	}

	// Basic blocks are maximal sequences of consecutive instructions that execute without branching.
	BasicBlock interface {
		String() string
		AppendInstruction(instruction *cod.Instruction)
	}

	// Edges represent the control flow between basic blocks in the control flow graph.
	Edge interface {
	}
)

// NewControlFlowGraph creates a new control flow graph for a specified module.
func NewControlFlowGraph(module cod.Module) ControlFlowGraph {
	return newControlFlowGraph(module)
}

// NewBasicBlock creates a new basic block.
func NewBasicBlock() BasicBlock {
	return newBasicBlock()
}

// NewEdge creates a new edge between two basic blocks.
func NewEdge(from, to *basicBlock) Edge {
	return newEdge(from, to)
}
