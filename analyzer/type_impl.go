// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package analyzer

import (
	ast "github.com/petersen65/pl0/v3/ast"
	ts "github.com/petersen65/pl0/v3/typesystem"
)

// ValidateOperatorCompatibility checks if an operator can be applied to given types
func ValidateOperatorCompatibility(op ast.ArithmeticOperation, leftType, rightType ts.TypeDescriptor) bool {
    if leftType == nil || rightType == nil {
        return false
    }
    
    // Types must match for most operations
    if !leftType.Equal(rightType) {
        return false
    }
    
    // Check if type has required capabilities
    return leftType.Capabilities()&op.Requirements != 0
}