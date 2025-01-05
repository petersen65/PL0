// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package cfg

import (
	"encoding/json"
	"io"
	"strings"

	cod "github.com/petersen65/PL0/v2/code"
	cor "github.com/petersen65/PL0/v2/core"
)

type (
	// Implementation of the control flow graph interface that manages basic blocks and edges.
	controlFlowGraph struct {
		module cod.Module    `json:"-"`      // intermediate code module that the control flow graph is built from
		Blocks []*basicBlock `json:"blocks"` // basic blocks of the control flow graph
		edges  []*edge       `json:"-"`      // edges of the control flow graph
	}

	// Implementation of the basic block interface that holds instructions of the block.
	basicBlock struct {
		Instructions []*cod.Instruction   `json:"instructions"` // instructions of the basic block
		variables    map[string]*variable `json:"-"`            // nontemporary variables in the basic block
	}

	// An edge connects two basic blocks in the control flow graph.
	edge struct {
		from *basicBlock // source basic block
		to   *basicBlock // destination basic block
	}

	// A nontemporary variable in the control flow graph.
	variable struct {
		name     string           // flattened name in the intermediate code
		liveness bool             // true if the variable is live at the current instruction
		nextUse  *cod.Instruction // next instruction that uses the variable
	}
)

// Create a new control flow graph for the specified module.
func newControlFlowGraph(module cod.Module) ControlFlowGraph {
	return &controlFlowGraph{
		module: module,
		Blocks: make([]*basicBlock, 0),
		edges:  make([]*edge, 0),
	}
}

// Create a new basic block for storing consecutive instructions.
func newBasicBlock() BasicBlock {
	return &basicBlock{
		Instructions: make([]*cod.Instruction, 0),
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
	iterator := cfg.module.GetIterator()

	// partition three-address code operations in the intermediate code into basic blocks
	for i := iterator.First(); i != nil; i = iterator.Next() {
		switch {
		// the first three-address code instruction in the intermediate code is a leader
		case block == nil:
			fallthrough

		// any instruction that is the target of a conditional or unconditional jump is a leader
		case i.Code.Operation == cod.Target:
			// append the previous basic block to the control flow graph if it is not nil
			cfg.AppendBasicBlock(block)

			// create a new basic block and append the current instruction to it
			block = newBasicBlock().(*basicBlock)
			block.AppendInstruction(i)

		// any instruction that immediately follows a conditional or unconditional jump is a leader
		case i.Code.Operation == cod.Jump ||
			i.Code.Operation == cod.JumpEqual ||
			i.Code.Operation == cod.JumpNotEqual ||
			i.Code.Operation == cod.JumpLess ||
			i.Code.Operation == cod.JumpLessEqual ||
			i.Code.Operation == cod.JumpGreater ||
			i.Code.Operation == cod.JumpGreaterEqual:

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
			return cor.NewGeneralError(cor.ControlFlowGraph, failureMap, cor.Error, controlFlowGraphExportFailed, nil, err)
		}

		// print a newline character between basic blocks
		if i < len(cfg.Blocks)-1 {
			if _, err := print.Write([]byte("\n")); err != nil {
				return cor.NewGeneralError(cor.ControlFlowGraph, failureMap, cor.Error, controlFlowGraphExportFailed, nil, err)
			}
		}
	}

	return nil
}

// Export the control flow graph to the specified writer in the specified format.
func (cfg *controlFlowGraph) Export(format cor.ExportFormat, print io.Writer) error {
	switch format {
	case cor.Json:
		// export the control flow graph as a JSON object
		if raw, err := json.MarshalIndent(cfg, "", "  "); err != nil {
			return cor.NewGeneralError(cor.ControlFlowGraph, failureMap, cor.Error, controlFlowGraphExportFailed, nil, err)
		} else {
			_, err = print.Write(raw)

			if err != nil {
				err = cor.NewGeneralError(cor.ControlFlowGraph, failureMap, cor.Error, controlFlowGraphExportFailed, nil, err)
			}

			return err
		}

	case cor.Text:
		// print is a convenience function to export the control flow grpah as a string to the print writer
		return cfg.Print(print)

	default:
		panic(cor.NewGeneralError(cor.ControlFlowGraph, failureMap, cor.Fatal, unknownExportFormat, format, nil))
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
func (bb *basicBlock) AppendInstruction(instruction *cod.Instruction) {
	if instruction != nil {

		bb.Instructions = append(bb.Instructions, instruction)
	}
}
