// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package ast

import (
	"fmt"
	"io"

	eh "github.com/petersen65/pl0/v3/errors"
)

// Base structure for all nodes in the AST.
type commonNode struct {
	NodeKind   NodeKind `json:"node_kind"` // kind of node for each node
	ParentNode Node     `json:"-"`         // parent node for each node
}

// Map node kinds to their string representation.
var nodeKindNames = map[NodeKind]string{
	KindBlock:               "block",
	KindConstantDeclaration: "constant_declaration",
	KindVariableDeclaration: "variable_declaration",
	KindFunctionDeclaration: "function_declaration",
	KindLiteralUse:          "literal_use",
	KindIdentifierUse:       "identifier_use",
	KindUnaryOperation:      "unary_operation",
	KindBinaryOperation:     "binary_operation",
	KindComparisonOperation: "comparison_operation",
	KindAssignmentStatement: "assignment_statement",
	KindReadStatement:       "read_statement",
	KindWriteStatement:      "write_statement",
	KindCallStatement:       "call_statement",
	KindIfStatement:         "if_statement",
	KindWhileStatement:      "while_statement",
	KindCompoundStatement:   "compound_statement",
}

// Kind of node for each node in the AST.
func (n *commonNode) Kind() NodeKind {
	return n.NodeKind
}

// Parent node for each node in the AST.
func (n *commonNode) Parent() Node {
	return n.ParentNode
}

// Set the parent node for each node in the AST.
func (n *commonNode) SetParent(parent Node) {
	n.ParentNode = parent
}

// Walk traverses a abstract syntax tree in a specific order and calls the visitor or the visit function for each node.
// Example tree:
//
//	    A
//	   / \
//	  B   C
//	 / \   \
//	D   E   F
func walk(parent Node, order TraversalOrder, visitor any, visit func(node Node, visitor any)) error {
	// check preconditions for walking the tree and return an error if any are violated
	if parent == nil {
		return eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Error, cannotWalkOnNilNode, nil, nil)
	} else if visitor == nil && visit == nil {
		return eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Error, walkRequiresVisitorOrFunction, nil, nil)
	} else if _, ok := visitor.(Visitor); !ok && visit == nil {
		return eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Error, walkRequiresInterfaceOrFunction, nil, nil)
	}

	// filter out empty constants
	if constant, ok := parent.(*constantDeclarationNode); ok && constant.IdentifierName_ == emptyConstantName {
		return nil
	}

	// switch on the order of traversal
	switch order {
	// Pre-order traversal is a method of traversing a tree data structure in which each node is processed before (pre) its child nodes.
	// This is commonly used in certain tree-related algorithms, including those for parsing expressions and serializing or deserializing trees.
	//
	// The order of operations for pre-order traversal is:
	//   1. Visit the parent node
	//   2. Traverse the childs left to right in pre-order
	// A pre-order traversal would visit the nodes in the following order: A, B, D, E, C, F.
	case PreOrder:
		// call the visit function or visit the parent node
		if visit != nil {
			visit(parent, visitor)
		} else {
			parent.Accept(visitor.(Visitor))
		}

		// traverse the childs left to right in pre-order
		for _, child := range parent.Children() {
			walk(child, order, visitor, visit)
		}

	// In-order traversal is a method of traversing a tree data structure in which each node is processed between (in) its child nodes.
	// This traversal method visits the nodes of a binary search tree in ascending order (if the tree is correctly formed).
	// This can be useful for operations like printing out the nodes of the tree in sorted order.
	//
	// The order of operations for in-order traversal is:
	//   1. Traverse the left subtree in in-order
	//   2. Visit the parent node
	//   3. Traverse the right subtree in in-order
	// An in-order traversal would visit the nodes in the following order: D, B, E, A, C, F.
	case InOrder:
		if len(parent.Children()) != 2 {
			return eh.NewGeneralError(eh.AbstractSyntaxTree, failureMap, eh.Error, inOrderRequiresTwoChildren, nil, nil)
		}

		// traverse the left subtree in in-order
		walk(parent.Children()[0], order, visitor, visit)

		// call the visit function or visit the parent node
		if visit != nil {
			visit(parent, visitor)
		} else {
			parent.Accept(visitor.(Visitor))
		}

		// traverse the right subtree in in-order
		walk(parent.Children()[1], order, visitor, visit)

	// Post-order traversal is a method of traversing a tree data structure in which each node is processed after (post) its child nodes.
	// This method is often used when you need to ensure that a node is processed after its descendants, such as when deleting or freeing nodes of a tree.
	//
	// The order of operations for post-order traversal is:
	//   1. Traverse the childs left to right in post-order
	//   2. Visit the parent node
	// A post-order traversal would visit the nodes in the following order: D, E, B, F, C, A.
	case PostOrder:
		// traverse the childs left to right in post-order
		for _, child := range parent.Children() {
			walk(child, order, visitor, visit)
		}

		// call the visit function or visit the parent node
		if visit != nil {
			visit(parent, visitor)
		} else {
			parent.Accept(visitor.(Visitor))
		}

	// Level-order traversal is a method of traversing a tree data structure in which each node is processed level by level.
	// This method is often used when you need to process the nodes of a tree in a breadth-first manner.
	// In a level-order traversal, all nodes at the current depth (or "level") are processed before moving on to nodes at the next depth.
	// This is different from pre-order, in-order, and post-order traversals, which are all types of depth-first traversals.
	//
	// The order of operations for level-order traversal is:
	//   1. Visit the parent node
	//   2. Visit all the nodes at the next depth (i.e., the children of the parent node)
	//   3. Repeat step 2 for each subsequent depth, visiting all nodes at each depth before moving on to the next
	// A level-order traversal would visit the nodes in the following order: A, B, C, D, E, F.
	case LevelOrder:
		queue := make([]Node, 0)
		queue = append(queue, parent)

		for len(queue) > 0 {
			node := queue[0]  // get the first node in the queue
			queue = queue[1:] // remove the first node from the queue

			// call the visit function or visit the node
			if visit != nil {
				visit(node, visitor)
			} else {
				node.Accept(visitor.(Visitor))
			}

			queue = append(queue, node.Children()...) // add the node's children to the end of the queue
		}
	}

	return nil
}

// Print the abstract syntax tree to the specified writer by recursively traversing the tree in pre-order.
func printAbstractSyntaxTree(node Node, indent string, last bool, print io.Writer) error {
	if _, err := fmt.Fprintf(print, "%v+- %v\n", indent, node); err != nil {
		return err
	}

	if last {
		indent += "   "
	} else {
		indent += "|  "
	}

	for i, child := range node.Children() {
		if err := printAbstractSyntaxTree(child, indent, i == len(node.Children())-1, print); err != nil {
			return err
		}
	}

	return nil
}
