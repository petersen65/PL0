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
		blocks: make([]*BasicBlock, 0),
		edges:  make([]*Edge, 0),
	}
}

func NewBasicBlock() *BasicBlock {
	return &BasicBlock{
		instructions: make([]*cod.Instruction, 0),
	}
}

func NewEdge(from, to *BasicBlock) *Edge {
	return &Edge{
		from: from,
		to:   to,
	}
}

func (cfg *ControlFlowGraph) Build() {
	var basicBlock *BasicBlock
	iterator := cfg.module.GetIterator()

	// partition three-address code operations in the intermediate language code into basic blocks
	for i := iterator.First(); i != nil; i = iterator.Next() {
		switch {
		// the first three-address code instruction in the intermediate language code is a leader
		case basicBlock == nil:
			fallthrough

		// any instruction that is the target of a conditional or unconditional jump is a leader
		case i.Label != "":
			// append the previous basic block to the control flow graph if it is not nil
			cfg.AppendBasicBlock(basicBlock)

			// create a new basic block and append the current instruction to it
			basicBlock = NewBasicBlock()
			basicBlock.AppendInstruction(i)

		// any instruction that immediately follows a conditional or unconditional jump is a leader
		case i.Code.Operation == cod.Jump ||
			i.Code.Operation == cod.JumpEqual ||
			i.Code.Operation == cod.JumpNotEqual ||
			i.Code.Operation == cod.JumpLess ||
			i.Code.Operation == cod.JumpLessEqual ||
			i.Code.Operation == cod.JumpGreater ||
			i.Code.Operation == cod.JumpGreaterEqual:

			// append the current instruction to the current basic block
			basicBlock.AppendInstruction(i)
			cfg.AppendBasicBlock(basicBlock)
			basicBlock = nil

		// remaining instructions in the intermediate language code are appended to the current basic block
		default:
			basicBlock.AppendInstruction(i)
		}
	}

	// append the last basic block to the control flow graph if it is not nil
	cfg.AppendBasicBlock(basicBlock)
}

// Append a basic block to the control flow graph if it is not nil.
func (cfg *ControlFlowGraph) AppendBasicBlock(bb *BasicBlock) {
	if bb != nil {
		cfg.blocks = append(cfg.blocks, bb)
	}
}

// Append an instruction to a basic block if it is not nil.
func (bb *BasicBlock) AppendInstruction(i *cod.Instruction) {
	if i != nil {
		bb.instructions = append(bb.instructions, i)
	}
}
