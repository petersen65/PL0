// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package evaluation implements type-safe arithmetic with comprehensive overflow detection and IEEE 754 floating-point compliance.
package evaluation

import ast "github.com/petersen65/pl0/v3/ast"

// Division modes for the integer division.
const (
	Truncate DivisionMode = iota
	Floor
)

type (
	// Mode of division for all integer data types, either truncate or floor.
	DivisionMode int

	// Configuration for all constant arithmetic evaluations.
	ArithmeticConfiguration struct {
		Float32Epsilon      float64      `json:"float32_epsilon"`       // epsilon for float32 comparisons, must be between 0 and 1
		Float64Epsilon      float64      `json:"float64_epsilon"`       // epsilon for float64 comparisons, must be between 0 and 1
		TreatNaNAsWarning   bool         `json:"nan_as_warning"`        // if true, treat NaN results as warnings instead of errors
		TreatInfAsWarning   bool         `json:"inf_as_warning"`        // if true, treat Inf results as warnings instead of errors
		IntegerDivisionMode DivisionMode `json:"integer_division_mode"` // mode of integer division, either truncate or floor
	}

	// Type constraint for signed integers.
	SignedInteger interface {
		~int8 | ~int16 | ~int32 | ~int64
	}

	// Type constraint for unsigned integers.
	UnsignedInteger interface {
		~uint8 | ~uint16 | ~uint32 | ~uint64
	}

	// Type constraint for floating-point numbers.
	Float interface {
		~float32 | ~float64
	}

	// Type constraint for all numeric types.
	Numeric interface {
		SignedInteger | UnsignedInteger | Float
	}
)

// Set the global arithmetic configuration for all constant arithmetic evaluations.
func SetArithmeticConfiguration(config ArithmeticConfiguration) error {
	if err := config.Validate(); err != nil {
		return err
	}

	arithmeticConfiguration = config
	return nil
}

// Perform the unary operation on the operand for signed and unsigned integers and return the result or an error.
func PerformUnaryOperation(op ast.UnaryOperator, operand any) (any, error) {
	return performUnaryOperation(op, operand)
}

// Perform the arithmetic operation on both operands for all data types and return the result or an error.
func PerformArithmeticOperation(op ast.ArithmeticOperator, left, right any) (any, error) {
	return performArithmeticOperation(op, left, right)
}

// Perform the comparison operation on both operands for all data types and return the result or an error.
func PerformComparisonOperation(op ast.ComparisonOperator, left, right any) (any, error) {
	return performComparisonOperation(op, left, right)
}
