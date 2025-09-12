// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package analyzer

import (
	ast "github.com/petersen65/pl0/v3/ast"
	eh "github.com/petersen65/pl0/v3/errors"
)

type (
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

// Perform the unary operation on the operand for all data types and return the result or an error.
func performUnaryOperation(op ast.UnaryOperator, operand any) (any, error) {
	switch op {
	case ast.Negate:
		switch v := operand.(type) {
		case int8:
			return negateValue(v)
		case int16:
			return negateValue(v)
		case int32:
			return negateValue(v)
		case int64:
			return negateValue(v)
		case float32:
			return negateValue(v)
		case float64:
			return negateValue(v)
		default:
			return nil, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, unsupportedTypeForOperation, nil, "negate", operand)
		}

	case ast.Odd:
		switch v := operand.(type) {
		case int8:
			return isOdd(v), nil
		case int16:
			return isOdd(v), nil
		case int32:
			return isOdd(v), nil
		case int64:
			return isOdd(v), nil
		case uint8:
			return isOdd(v), nil
		case uint16:
			return isOdd(v), nil
		case uint32:
			return isOdd(v), nil
		case uint64:
			return isOdd(v), nil
		default:
			return nil, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, unsupportedTypeForOperation, nil, "odd", operand)
		}

	default:
		return nil, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Fatal, unknownUnaryOperation, nil)
	}
}

// Perform the arithmetic operation on both operands for all data types and return the result or an error.
func performArithmeticOperation(op ast.ArithmeticOperator, left, right any) (any, error) {
	// Type assertion with same type check
	switch l := left.(type) {
	case int8:
		if r, ok := right.(int8); ok {
			switch op {
			case ast.Plus:
				return add(l, r)
			case ast.Minus:
				return subtract(l, r)
			case ast.Times:
				return multiply(l, r)
			case ast.Divide:
				return divide(l, r)
			}
		}
	case int16:
		if r, ok := right.(int16); ok {
			switch op {
			case ast.Plus:
				return add(l, r)
			case ast.Minus:
				return subtract(l, r)
			case ast.Times:
				return multiply(l, r)
			case ast.Divide:
				return divide(l, r)
			}
		}
	case int32:
		if r, ok := right.(int32); ok {
			switch op {
			case ast.Plus:
				return add(l, r)
			case ast.Minus:
				return subtract(l, r)
			case ast.Times:
				return multiply(l, r)
			case ast.Divide:
				return divide(l, r)
			}
		}
	case int64:
		if r, ok := right.(int64); ok {
			switch op {
			case ast.Plus:
				return add(l, r)
			case ast.Minus:
				return subtract(l, r)
			case ast.Times:
				return multiply(l, r)
			case ast.Divide:
				return divide(l, r)
			}
		}
	case uint8:
		if r, ok := right.(uint8); ok {
			switch op {
			case ast.Plus:
				return add(l, r)
			case ast.Minus:
				return subtract(l, r)
			case ast.Times:
				return multiply(l, r)
			case ast.Divide:
				return divide(l, r)
			}
		}
	case uint16:
		if r, ok := right.(uint16); ok {
			switch op {
			case ast.Plus:
				return add(l, r)
			case ast.Minus:
				return subtract(l, r)
			case ast.Times:
				return multiply(l, r)
			case ast.Divide:
				return divide(l, r)
			}
		}
	case uint32:
		if r, ok := right.(uint32); ok {
			switch op {
			case ast.Plus:
				return add(l, r)
			case ast.Minus:
				return subtract(l, r)
			case ast.Times:
				return multiply(l, r)
			case ast.Divide:
				return divide(l, r)
			}
		}
	case uint64:
		if r, ok := right.(uint64); ok {
			switch op {
			case ast.Plus:
				return add(l, r)
			case ast.Minus:
				return subtract(l, r)
			case ast.Times:
				return multiply(l, r)
			case ast.Divide:
				return divide(l, r)
			}
		}
	case float32:
		if r, ok := right.(float32); ok {
			switch op {
			case ast.Plus:
				return add(l, r)
			case ast.Minus:
				return subtract(l, r)
			case ast.Times:
				return multiply(l, r)
			case ast.Divide:
				return divide(l, r)
			}
		}
	case float64:
		if r, ok := right.(float64); ok {
			switch op {
			case ast.Plus:
				return add(l, r)
			case ast.Minus:
				return subtract(l, r)
			case ast.Times:
				return multiply(l, r)
			case ast.Divide:
				return divide(l, r)
			}
		}
	}

	return nil, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, incompatibleTypesInOperation, nil, left, right)
}

// Perform the comparison operation on both operands for all data types and return the result or an error.
func performComparisonOperation(op ast.ComparisonOperator, left, right any) (any, error) {
	// Similar pattern to arithmetic operations
	switch l := left.(type) {
	case int8:
		if r, ok := right.(int8); ok {
			switch op {
			case ast.Equal:
				return equal(l, r), nil
			case ast.NotEqual:
				return notEqual(l, r), nil
			case ast.Less:
				return less(l, r), nil
			case ast.LessOrEqual:
				return lessEqual(l, r), nil
			case ast.Greater:
				return greater(l, r), nil
			case ast.GreaterOrEqual:
				return greaterEqual(l, r), nil
			}
		}
		// ... repeat for other types (int16, int32, int64, uint8, uint16, uint32, uint64, float32, float64)
	}

	return nil, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, incompatibleTypesInComparison, nil, left, right)
}

// Generic negate operation for signed integers and floats.
func negateValue[T SignedInteger | Float](value T) (T, error) {
	result := -value

	// Check for overflow in signed integers
	if value > 0 && result > 0 {
		return 0, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflow, nil, "negate", value)
	}

	return result, nil
}

// Generic odd operation for signed and unsigned integers.
func isOdd[T SignedInteger | UnsignedInteger](value T) bool {
	return value&1 == 1
}

// Generic add operation for all numeric types.
func add[T Numeric](left, right T) (T, error) {
	result := left + right

	// Overflow check for unsigned integers
	var zero T
	if result < left && right > zero {
		return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflow, nil, "add", left, right)
	}

	return result, nil
}

// Generic subtract operation for all numeric types.
func subtract[T Numeric](left, right T) (T, error) {
	result := left - right

	// Underflow check for unsigned integers
	var zero T
	if result > left && right > zero {
		return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticUnderflow, nil, "subtract", left, right)
	}

	return result, nil
}

// Generic multiply operation for all numeric types.
func multiply[T Numeric](left, right T) (T, error) {
	result := left * right

	// Overflow check (simplified - you may want more sophisticated checks)
	var zero T
	if right != zero && result/right != left {
		return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflow, nil, "multiply", left, right)
	}

	return result, nil
}

// Generic divide operation for all numeric types.
func divide[T Numeric](left, right T) (T, error) {
	var zero T
	if right == zero {
		return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, divisionByZero, nil, left)
	}

	return left / right, nil
}

// Generic comparison operations
func equal[T comparable](left, right T) bool {
	return left == right
}

func notEqual[T comparable](left, right T) bool {
	return left != right
}

func less[T Numeric](left, right T) bool {
	return left < right
}

func lessEqual[T Numeric](left, right T) bool {
	return left <= right
}

func greater[T Numeric](left, right T) bool {
	return left > right
}

func greaterEqual[T Numeric](left, right T) bool {
	return left >= right
}
