// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package cfg implements the control graph representation (CFG) of intermediate language code.
package cfg

import cod "github.com/petersen65/PL0/v2/code"

type ControlFlowGraph struct {
	module cod.Module
	blocks []*BasicBlock
	edges  []*Edge
}

type BasicBlock struct {
	instructions []*cod.Instruction
}

type Edge struct {
	from *BasicBlock
	to   *BasicBlock
}

func NewControlFlowGraph(module cod.Module) *ControlFlowGraph {
	return &ControlFlowGraph{
		module: module,
	}
}

func (cfg *ControlFlowGraph) Build() {
	// Create a new basic block.
	// bb := &BasicBlock{
	// 	instructions: make([]*cod.Instruction, 0),
	// }

	// Iterate over the module instructions.
	// for _, inst := range cfg.module.Instructions {
	// 	// Append the instruction to the basic block.
	// 	bb.instructions = append(bb.instructions, inst)
	// }
}
