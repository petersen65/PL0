// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package code

import "github.com/petersen65/PL0/v2/ast"

type intermediateCode struct {
	abstractSyntax ast.Block
}

func newIntermediateCode(abstractSyntax ast.Block) IntermediateCode {
	return &intermediateCode{abstractSyntax: abstractSyntax}
}
