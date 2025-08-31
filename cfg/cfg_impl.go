// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package cfg

import (
	"encoding/json"
	"io"
	"strings"

	eh "github.com/petersen65/pl0/v3/errors"
	exp "github.com/petersen65/pl0/v3/export"
	ic "github.com/petersen65/pl0/v3/generator/intermediate"
)

type (
	// Implementation of the control flow graph interface that manages basic blocks and edges.
	controlFlowGraph struct {
		intermediateCode ic.IntermediateCodeUnit `json:"-"`      // intermediate code unit that the control flow graph is built from
		Blocks           []*basicBlock           `json:"blocks"` // basic blocks of the control flow graph
		edges            []*edge                 `json:"-"`      // edges of the control flow graph
	}

	// Implementation of the basic block interface that holds instructions of the block.
	basicBlock struct {
		Instructions []*ic.Instruction    `json:"instructions"` // instructions of the basic block
		variables    map[string]*variable `json:"-"`            // nontemporary variables in the basic block
	}

	// An edge connects two basic blocks in the control flow graph.
	edge struct {
		from *basicBlock // source basic block
		to   *basicBlock // destination basic block
	}

	// A nontemporary variable in the control flow graph.
	variable struct {
		name     string          // flattened name in the intermediate code
		liveness bool            // true if the variable is live at the current instruction
		nextUse  *ic.Instruction // next instruction that uses the variable
	}
)

// Create a new control flow graph for the specified intermediate code unit.
func newControlFlowGraph(intermediateCode ic.IntermediateCodeUnit) ControlFlowGraph {
	return &controlFlowGraph{
		intermediateCode: intermediateCode,
		Blocks:           make([]*basicBlock, 0),
		edges:            make([]*edge, 0),
	}
}

// Create a new basic block for storing consecutive instructions.
func newBasicBlock() BasicBlock {
	return &basicBlock{
		Instructions: make([]*ic.Instruction, 0),
		variables:    make(map[string]*variable),
	}
}

// Create a new edge to connect two basic blocks.
func newEdge(from, to *basicBlock) Edge {
	return &edge{
		from: from,
		to:   to,
	}
}

// Build the control flow graph by partitioning the intermediate code into basic blocks.
func (cfg *controlFlowGraph) Build() {
	var block *basicBlock
	iterator := cfg.intermediateCode.GetIterator()

	// partition three-address code operations in the intermediate code into basic blocks
	for i := iterator.First(); i != nil; i = iterator.Next() {
		switch {
		// the first three-address code instruction in the intermediate code is a leader
		case block == nil:
			fallthrough

		// any instruction that is the target of a conditional or unconditional jump is a leader
		case i.Quadruple.Operation == ic.BranchTarget:
			// append the previous basic block to the control flow graph if it is not nil
			cfg.AppendBasicBlock(block)

			// create a new basic block and append the current instruction to it
			block = newBasicBlock().(*basicBlock)
			block.AppendInstruction(i)

		// any instruction that immediately follows a conditional or unconditional jump is a leader
		case i.Quadruple.Operation == ic.Jump ||
			i.Quadruple.Operation == ic.JumpEqual ||
			i.Quadruple.Operation == ic.JumpNotEqual ||
			i.Quadruple.Operation == ic.JumpLess ||
			i.Quadruple.Operation == ic.JumpLessEqual ||
			i.Quadruple.Operation == ic.JumpGreater ||
			i.Quadruple.Operation == ic.JumpGreaterEqual:

			// append the current instruction to the current basic block
			block.AppendInstruction(i)
			cfg.AppendBasicBlock(block)
			block = nil

		// remaining instructions in the intermediate code are appended to the current basic block
		default:
			block.AppendInstruction(i)
		}
	}

	// append the last basic block to the control flow graph if it is not nil
	cfg.AppendBasicBlock(block)
}

// Append a basic block to the control flow graph if it is not nil.
func (cfg *controlFlowGraph) AppendBasicBlock(basicBlock *basicBlock) {
	if basicBlock != nil {
		cfg.Blocks = append(cfg.Blocks, basicBlock)
	}
}

// Print the control flow graph to the specified writer.
func (cfg *controlFlowGraph) Print(print io.Writer, args ...any) error {
	// enumerate all basic blocks of the control flow graph and print them to the writer
	for i, block := range cfg.Blocks {
		// print the basic block to the writer
		if _, err := print.Write([]byte(block.String())); err != nil {
			return eh.NewGeneralError(eh.ControlFlowGraph, failureMap, eh.Error, controlFlowGraphExportFailed, nil, err)
		}

		// print a newline character between basic blocks
		if i < len(cfg.Blocks)-1 {
			if _, err := print.Write([]byte("\n")); err != nil {
				return eh.NewGeneralError(eh.ControlFlowGraph, failureMap, eh.Error, controlFlowGraphExportFailed, nil, err)
			}
		}
	}

	return nil
}

// Export the control flow graph to the specified writer in the specified format.
func (cfg *controlFlowGraph) Export(format exp.ExportFormat, print io.Writer) error {
	switch format {
	case exp.Json:
		// export the control flow graph as a JSON object
		if raw, err := json.MarshalIndent(cfg, "", "  "); err != nil {
			return eh.NewGeneralError(eh.ControlFlowGraph, failureMap, eh.Error, controlFlowGraphExportFailed, nil, err)
		} else {
			_, err = print.Write(raw)

			if err != nil {
				err = eh.NewGeneralError(eh.ControlFlowGraph, failureMap, eh.Error, controlFlowGraphExportFailed, nil, err)
			}

			return err
		}

	case exp.Text:
		// print is a convenience function to export the control flow grpah as a string to the print writer
		return cfg.Print(print)

	default:
		panic(eh.NewGeneralError(eh.ControlFlowGraph, failureMap, eh.Fatal, unknownExportFormat, format, nil))
	}
}

// String representation of a basic block.
func (bb *basicBlock) String() string {
	var builder strings.Builder

	// enumerate all instructions in the basic block and write them to the string builder
	for _, instruction := range bb.Instructions {
		builder.WriteString(instruction.String())
		builder.WriteString("\n")
	}

	return builder.String()
}

// Append an instruction to a basic block if it is not nil.
func (bb *basicBlock) AppendInstruction(instruction *ic.Instruction) {
	if instruction != nil {

		bb.Instructions = append(bb.Instructions, instruction)
	}
}
