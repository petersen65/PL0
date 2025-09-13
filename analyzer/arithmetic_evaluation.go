// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package analyzer

import (
	"math"

	ast "github.com/petersen65/pl0/v3/ast"
	eh "github.com/petersen65/pl0/v3/errors"
)

// Division modes for the integer division.
const (
	Truncate DivisionMode = iota
	Floor
)

// Signed integers overflow values used in error messages.
const (
	overflowInt8  = "128"
	overflowInt16 = "32768"
	overflowInt32 = "2147483648"
	overflowInt64 = "9223372036854775808"
)

// Special float values used in error messages.
const (
	nan = "NaN"
	inf = "Inf"
)

type (
	// Mode of division for all integer data types, either truncate or floor.
	DivisionMode int

	// Configuration for all constant arithmetic evaluations.
	ArithmeticConfiguration struct {
		Float32Epsilon      float64
		Float64Epsilon      float64
		TreatNaNAsWarning   bool
		TreatInfAsWarning   bool
		IntegerDivisionMode DivisionMode
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

// Default configuration for all constant arithmetic evaluations.
var arithmeticConfiguration = ArithmeticConfiguration{
	Float32Epsilon:      1e-7,
	Float64Epsilon:      1e-15,
	TreatNaNAsWarning:   true,
	TreatInfAsWarning:   true,
	IntegerDivisionMode: Truncate,
}

// Set the global arithmetic configuration for all constant arithmetic evaluations.
func SetArithmeticConfiguration(config ArithmeticConfiguration) error {
	if err := config.Validate(); err != nil {
		return err
	}

	arithmeticConfiguration = config
	return nil
}

// Validate the arithmetic configuration and return an error if it is invalid.
func (c *ArithmeticConfiguration) Validate() error {
	if c.Float32Epsilon <= 0 || c.Float32Epsilon > 1 {
		return eh.NewGeneralError(eh.Analyzer, failureMap, eh.Fatal, float32EpsilonMustBeBetween0And1, nil, c.Float32Epsilon)
	}

	if c.Float64Epsilon <= 0 || c.Float64Epsilon > 1 {
		return eh.NewGeneralError(eh.Analyzer, failureMap, eh.Fatal, float64EpsilonMustBeBetween0And1, nil, c.Float64Epsilon)
	}	

	if c.Float32Epsilon > c.Float64Epsilon {
		return eh.NewGeneralError(eh.Analyzer, failureMap, eh.Fatal, float32EpsilonShouldNotBeGreaterThanFloat64Epsilon, nil, c.Float32Epsilon, c.Float64Epsilon)
	}

	return nil
}

// Perform the unary operation on the operand for signed and unsigned integers and return the result or an error.
func performUnaryOperation(op ast.UnaryOperator, operand any) (any, error) {
	switch op {
	case ast.Negate:
		switch v := operand.(type) {
		case int8:
			return negate(v)

		case int16:
			return negate(v)

		case int32:
			return negate(v)

		case int64:
			return negate(v)

		case float32:
			return negate(v)

		case float64:
			return negate(v)

		default:
			uo := ast.NewUnaryOperation(ast.Negate, nil, 0)
			return nil, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, dataTypeCannotBeUsedInUnaryOperation, nil, ast.Negate, uo.Requirements(), operand)
		}

	case ast.Odd:
		switch v := operand.(type) {
		case int8:
			return Odd(v), nil

		case int16:
			return Odd(v), nil

		case int32:
			return Odd(v), nil

		case int64:
			return Odd(v), nil

		case uint8:
			return Odd(v), nil

		case uint16:
			return Odd(v), nil

		case uint32:
			return Odd(v), nil

		case uint64:
			return Odd(v), nil

		default:
			uo := ast.NewUnaryOperation(ast.Odd, nil, 0)
			return nil, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, dataTypeCannotBeUsedInUnaryOperation, nil, ast.Odd, uo.Requirements(), operand)
		}

	default:
		return nil, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Fatal, unknownUnaryOperation, nil)
	}
}

// Perform the arithmetic operation on both operands for all data types and return the result or an error.
func performArithmeticOperation(op ast.ArithmeticOperator, left, right any) (any, error) {
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

			case ast.Modulo:
				return modulo(l, r)
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

			case ast.Modulo:
				return modulo(l, r)
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

			case ast.Modulo:
				return modulo(l, r)
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

			case ast.Modulo:
				return modulo(l, r)
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

			case ast.Modulo:
				return modulo(l, r)
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

			case ast.Modulo:
				return modulo(l, r)
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

			case ast.Modulo:
				return modulo(l, r)
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

			case ast.Modulo:
				return modulo(l, r)
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

	ao := ast.NewArithmeticOperation(op, nil, nil, 0)
	return nil, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, dataTypeCannotBeUsedInArithmeticOperation, nil, op, ao.Requirements(), left)
}

// Perform the comparison operation on both operands for all data types and return the result or an error.
func performComparisonOperation(op ast.ComparisonOperator, left, right any) (any, error) {
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

			case ast.LessEqual:
				return lessEqual(l, r), nil

			case ast.Greater:
				return greater(l, r), nil

			case ast.GreaterEqual:
				return greaterEqual(l, r), nil
			}
		}

	case int16:
		if r, ok := right.(int16); ok {
			switch op {
			case ast.Equal:
				return equal(l, r), nil

			case ast.NotEqual:
				return notEqual(l, r), nil

			case ast.Less:
				return less(l, r), nil

			case ast.LessEqual:
				return lessEqual(l, r), nil

			case ast.Greater:
				return greater(l, r), nil

			case ast.GreaterEqual:
				return greaterEqual(l, r), nil
			}
		}

	case int32:
		if r, ok := right.(int32); ok {
			switch op {
			case ast.Equal:
				return equal(l, r), nil

			case ast.NotEqual:
				return notEqual(l, r), nil

			case ast.Less:
				return less(l, r), nil

			case ast.LessEqual:
				return lessEqual(l, r), nil

			case ast.Greater:
				return greater(l, r), nil

			case ast.GreaterEqual:
				return greaterEqual(l, r), nil
			}
		}

	case int64:
		if r, ok := right.(int64); ok {
			switch op {
			case ast.Equal:
				return equal(l, r), nil

			case ast.NotEqual:
				return notEqual(l, r), nil

			case ast.Less:
				return less(l, r), nil

			case ast.LessEqual:
				return lessEqual(l, r), nil

			case ast.Greater:
				return greater(l, r), nil

			case ast.GreaterEqual:
				return greaterEqual(l, r), nil
			}
		}

	case uint8:
		if r, ok := right.(uint8); ok {
			switch op {
			case ast.Equal:
				return equal(l, r), nil

			case ast.NotEqual:
				return notEqual(l, r), nil

			case ast.Less:
				return less(l, r), nil

			case ast.LessEqual:
				return lessEqual(l, r), nil

			case ast.Greater:
				return greater(l, r), nil

			case ast.GreaterEqual:
				return greaterEqual(l, r), nil
			}
		}

	case uint16:
		if r, ok := right.(uint16); ok {
			switch op {
			case ast.Equal:
				return equal(l, r), nil

			case ast.NotEqual:
				return notEqual(l, r), nil

			case ast.Less:
				return less(l, r), nil

			case ast.LessEqual:
				return lessEqual(l, r), nil

			case ast.Greater:
				return greater(l, r), nil

			case ast.GreaterEqual:
				return greaterEqual(l, r), nil
			}
		}

	case uint32:
		if r, ok := right.(uint32); ok {
			switch op {
			case ast.Equal:
				return equal(l, r), nil

			case ast.NotEqual:
				return notEqual(l, r), nil

			case ast.Less:
				return less(l, r), nil

			case ast.LessEqual:
				return lessEqual(l, r), nil

			case ast.Greater:
				return greater(l, r), nil

			case ast.GreaterEqual:
				return greaterEqual(l, r), nil
			}
		}

	case uint64:
		if r, ok := right.(uint64); ok {
			switch op {
			case ast.Equal:
				return equal(l, r), nil

			case ast.NotEqual:
				return notEqual(l, r), nil

			case ast.Less:
				return less(l, r), nil

			case ast.LessEqual:
				return lessEqual(l, r), nil

			case ast.Greater:
				return greater(l, r), nil

			case ast.GreaterEqual:
				return greaterEqual(l, r), nil
			}
		}

	case float32:
		if r, ok := right.(float32); ok {
			// check for NaN operands and report as warning if configured
			if arithmeticConfiguration.TreatNaNAsWarning {
				if math.IsNaN(float64(l)) || math.IsNaN(float64(r)) {
					return false, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Warning, invalidFloatingPointOperationNaN, nil, op, l, r, nan)
				}
			}

			switch op {
			case ast.Equal:
				return equal(l, r), nil

			case ast.NotEqual:
				return notEqual(l, r), nil

			case ast.Less:
				return less(l, r), nil

			case ast.LessEqual:
				return lessEqual(l, r), nil

			case ast.Greater:
				return greater(l, r), nil

			case ast.GreaterEqual:
				return greaterEqual(l, r), nil
			}
		}

	case float64:
		if r, ok := right.(float64); ok {
			// check for NaN operands and report as warning if configured
			if arithmeticConfiguration.TreatNaNAsWarning {
				if math.IsNaN(l) || math.IsNaN(r) {
					return false, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Warning, invalidFloatingPointOperationNaN, nil, op, l, r, nan)
				}
			}

			switch op {
			case ast.Equal:
				return equal(l, r), nil

			case ast.NotEqual:
				return notEqual(l, r), nil

			case ast.Less:
				return less(l, r), nil

			case ast.LessEqual:
				return lessEqual(l, r), nil

			case ast.Greater:
				return greater(l, r), nil

			case ast.GreaterEqual:
				return greaterEqual(l, r), nil
			}
		}
	}

	co := ast.NewComparisonOperation(op, nil, nil, 0)
	return nil, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, dataTypeCannotBeUsedInComparisonOperation, nil, op, co.Requirements(), left)
}

// Generic negate operation for signed integers and floats with overflow checking for signed integers.
func negate[T SignedInteger | Float](value T) (T, error) {
	// check for overflow when negating minimum signed integer values
	switch v := any(value).(type) {
	case int8:
		if v == math.MinInt8 {
			return 0, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Negate, value, overflowInt8)
		}

	case int16:
		if v == math.MinInt16 {
			return 0, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Negate, value, overflowInt16)
		}

	case int32:
		if v == math.MinInt32 {
			return 0, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Negate, value, overflowInt32)
		}

	case int64:
		if v == math.MinInt64 {
			return 0, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Negate, value, overflowInt64)
		}
	}

	// perform negation after checks pass
	return -value, nil
}

// Generic odd operation for signed and unsigned integers.
func Odd[T SignedInteger | UnsignedInteger](value T) bool {
	return value&1 == 1
}

// Generic add operation for all numeric types with overflow checking for signed integers, unsigned integers, and floats.
func add[T Numeric](left, right T) (T, error) {
	var zero T

	// type-specific overflow checking before addition
	switch any(left).(type) {
	case int8:
		l, r := any(left).(int8), any(right).(int8)

		if r > 0 && l > math.MaxInt8-r {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Plus, left, right)
		}

		if r < 0 && l < math.MinInt8-r {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Plus, left, right)
		}

	case int16:
		l, r := any(left).(int16), any(right).(int16)

		if r > 0 && l > math.MaxInt16-r {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Plus, left, right)
		}

		if r < 0 && l < math.MinInt16-r {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Plus, left, right)
		}

	case int32:
		l, r := any(left).(int32), any(right).(int32)

		if r > 0 && l > math.MaxInt32-r {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Plus, left, right)
		}

		if r < 0 && l < math.MinInt32-r {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Plus, left, right)
		}

	case int64:
		l, r := any(left).(int64), any(right).(int64)

		if r > 0 && l > math.MaxInt64-r {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Plus, left, right)
		}

		if r < 0 && l < math.MinInt64-r {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Plus, left, right)
		}

	case uint8:
		l, r := any(left).(uint8), any(right).(uint8)

		if l > math.MaxUint8-r {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Plus, left, right)
		}

	case uint16:
		l, r := any(left).(uint16), any(right).(uint16)

		if l > math.MaxUint16-r {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Plus, left, right)
		}

	case uint32:
		l, r := any(left).(uint32), any(right).(uint32)

		if l > math.MaxUint32-r {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Plus, left, right)
		}

	case uint64:
		l, r := any(left).(uint64), any(right).(uint64)

		if l > math.MaxUint64-r {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Plus, left, right)
		}

	case float32:
		result := left + right

		if math.IsInf(float64(any(result).(float32)), 0) {
			if arithmeticConfiguration.TreatInfAsWarning {
				// return the infinity result with a warning (IEEE 754 compliant)
				return result, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Warning, arithmeticOverflowOnConstantExpression, nil, ast.Plus, left, right)
			}

			// return error and don't allow the operation
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Plus, left, right)
		}

		return result, nil

	case float64:
		result := left + right

		if math.IsInf(any(result).(float64), 0) {
			if arithmeticConfiguration.TreatInfAsWarning {
				// return the infinity result with a warning (IEEE 754 compliant)
				return result, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Warning, arithmeticOverflowOnConstantExpression, nil, ast.Plus, left, right)
			}

			// return error and don't allow the operation
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Plus, left, right)
		}

		return result, nil
	}

	// perform addition after checks pass
	return left + right, nil
}

// Generic subtract operation for all numeric types with overflow and underflow checking.
func subtract[T Numeric](left, right T) (T, error) {
	var zero T

	// type-specific overflow and underflow checking before subtraction
	switch any(left).(type) {
	case int8:
		l, r := any(left).(int8), any(right).(int8)

		if r < 0 && l > math.MaxInt8+r {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Minus, left, right)
		}

		if r > 0 && l < math.MinInt8+r {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticUnderflowOnConstantExpression, nil, ast.Minus, left, right)
		}

	case int16:
		l, r := any(left).(int16), any(right).(int16)

		if r < 0 && l > math.MaxInt16+r {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Minus, left, right)
		}

		if r > 0 && l < math.MinInt16+r {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticUnderflowOnConstantExpression, nil, ast.Minus, left, right)
		}

	case int32:
		l, r := any(left).(int32), any(right).(int32)

		if r < 0 && l > math.MaxInt32+r {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Minus, left, right)
		}

		if r > 0 && l < math.MinInt32+r {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticUnderflowOnConstantExpression, nil, ast.Minus, left, right)
		}

	case int64:
		l, r := any(left).(int64), any(right).(int64)

		if r < 0 && l > math.MaxInt64+r {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Minus, left, right)
		}

		if r > 0 && l < math.MinInt64+r {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticUnderflowOnConstantExpression, nil, ast.Minus, left, right)
		}

	case uint8:
		l, r := any(left).(uint8), any(right).(uint8)

		if l < r {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticUnderflowOnConstantExpression, nil, ast.Minus, left, right)
		}

	case uint16:
		l, r := any(left).(uint16), any(right).(uint16)

		if l < r {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticUnderflowOnConstantExpression, nil, ast.Minus, left, right)
		}

	case uint32:
		l, r := any(left).(uint32), any(right).(uint32)

		if l < r {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticUnderflowOnConstantExpression, nil, ast.Minus, left, right)
		}

	case uint64:
		l, r := any(left).(uint64), any(right).(uint64)

		if l < r {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticUnderflowOnConstantExpression, nil, ast.Minus, left, right)
		}

	case float32:
		result := left - right

		if math.IsInf(float64(any(result).(float32)), 0) {
			if arithmeticConfiguration.TreatInfAsWarning {
				// return the infinity result with a warning (IEEE 754 compliant)
				return result, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Warning, arithmeticOverflowOnConstantExpression, nil, ast.Minus, left, right)
			}

			// return error and don't allow the operation
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Minus, left, right)
		}

		return result, nil

	case float64:
		result := left - right

		if math.IsInf(any(result).(float64), 0) {
			if arithmeticConfiguration.TreatInfAsWarning {
				// return the infinity result with a warning (IEEE 754 compliant)
				return result, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Warning, arithmeticOverflowOnConstantExpression, nil, ast.Minus, left, right)
			}

			// return error and don't allow the operation
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Minus, left, right)
		}

		return result, nil
	}

	// perform subtraction after checks pass
	return left - right, nil
}

// Generic multiply operation for all numeric types with overflow checking for integers and floating point types.
func multiply[T Numeric](left, right T) (T, error) {
	var zero T

	// early return for zero multiplication
	if left == zero || right == zero {
		return zero, nil
	}

	// type-specific overflow checking before multiplication
	switch any(left).(type) {
	case int8:
		l, r := any(left).(int8), any(right).(int8)

		if l > 0 {
			if r > 0 && l > math.MaxInt8/r {
				return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Times, left, right)
			}

			if r < 0 && r < math.MinInt8/l {
				return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Times, left, right)
			}
		} else if l < 0 {
			if r > 0 && l < math.MinInt8/r {
				return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Times, left, right)
			}

			if r < 0 && l != 0 && r < math.MaxInt8/l {
				return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Times, left, right)
			}
		}

	case int16:
		l, r := any(left).(int16), any(right).(int16)

		if l > 0 {
			if r > 0 && l > math.MaxInt16/r {
				return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Times, left, right)
			}

			if r < 0 && r < math.MinInt16/l {
				return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Times, left, right)
			}
		} else if l < 0 {
			if r > 0 && l < math.MinInt16/r {
				return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Times, left, right)
			}

			if r < 0 && l != 0 && r < math.MaxInt16/l {
				return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Times, left, right)
			}
		}

	case int32:
		l, r := any(left).(int32), any(right).(int32)

		if l > 0 {
			if r > 0 && l > math.MaxInt32/r {
				return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Times, left, right)
			}

			if r < 0 && r < math.MinInt32/l {
				return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Times, left, right)
			}
		} else if l < 0 {
			if r > 0 && l < math.MinInt32/r {
				return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Times, left, right)
			}

			if r < 0 && l != 0 && r < math.MaxInt32/l {
				return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Times, left, right)
			}
		}

	case int64:
		l, r := any(left).(int64), any(right).(int64)

		if l > 0 {
			if r > 0 && l > math.MaxInt64/r {
				return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Times, left, right)
			}

			if r < 0 && r < math.MinInt64/l {
				return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Times, left, right)
			}
		} else if l < 0 {
			if r > 0 && l < math.MinInt64/r {
				return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Times, left, right)
			}

			if r < 0 && l != 0 && r < math.MaxInt64/l {
				return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Times, left, right)
			}
		}

	case uint8:
		l, r := any(left).(uint8), any(right).(uint8)

		if l > 0 && r > math.MaxUint8/l {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Times, left, right)
		}

	case uint16:
		l, r := any(left).(uint16), any(right).(uint16)

		if l > 0 && r > math.MaxUint16/l {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Times, left, right)
		}

	case uint32:
		l, r := any(left).(uint32), any(right).(uint32)

		if l > 0 && r > math.MaxUint32/l {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Times, left, right)
		}

	case uint64:
		l, r := any(left).(uint64), any(right).(uint64)

		if l > 0 && r > math.MaxUint64/l {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Times, left, right)
		}

	case float32:
		result := left * right

		if math.IsInf(float64(any(result).(float32)), 0) {
			if arithmeticConfiguration.TreatInfAsWarning {
				// return the infinity result with a warning (IEEE 754 compliant)
				return result, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Warning, arithmeticOverflowOnConstantExpression, nil, ast.Times, left, right)
			}

			// return error and don't allow the operation
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Times, left, right)
		}

		return result, nil

	case float64:
		result := left * right

		if math.IsInf(any(result).(float64), 0) {
			if arithmeticConfiguration.TreatInfAsWarning {
				// return the infinity result with a warning (IEEE 754 compliant)
				return result, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Warning, arithmeticOverflowOnConstantExpression, nil, ast.Times, left, right)
			}

			// return error and don't allow the operation
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Times, left, right)
		}

		return result, nil
	}

	// perform the multiplication after checks pass
	return left * right, nil
}

// Generic divide operation for all numeric types with division by zero and overflow checking for signed integers.
func divide[T Numeric](left, right T) (T, error) {
	var zero T

	if right == zero {
		return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, divisionByZeroInConstantExpression, nil, ast.Divide, left, right)
	}

	// check for signed integer overflow: MIN / -1
	switch l := any(left).(type) {
	case int8:
		r := any(right).(int8)

		if l == math.MinInt8 && r == -1 {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Divide, left, right)
		}

		// apply configured division mode for integers
		if arithmeticConfiguration.IntegerDivisionMode == Floor {
			return any(floorDivide(l, r)).(T), nil
		}

		// default truncate division (toward zero)
		return any(l / r).(T), nil

	case int16:
		r := any(right).(int16)

		if l == math.MinInt16 && r == -1 {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Divide, left, right)
		}

		// apply configured division mode for integers
		if arithmeticConfiguration.IntegerDivisionMode == Floor {
			return any(floorDivide(l, r)).(T), nil
		}

		// default truncate division (toward zero)
		return any(l / r).(T), nil

	case int32:
		r := any(right).(int32)

		if l == math.MinInt32 && r == -1 {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Divide, left, right)
		}

		// apply configured division mode for integers
		if arithmeticConfiguration.IntegerDivisionMode == Floor {
			return any(floorDivide(l, r)).(T), nil
		}

		// default truncate division (toward zero)
		return any(l / r).(T), nil

	case int64:
		r := any(right).(int64)

		if l == math.MinInt64 && r == -1 {
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, arithmeticOverflowOnConstantExpression, nil, ast.Divide, left, right)
		}

		// apply configured division mode for integers
		if arithmeticConfiguration.IntegerDivisionMode == Floor {
			return any(floorDivide(l, r)).(T), nil
		}

		// default truncate division (toward zero)
		return any(l / r).(T), nil

	case uint8, uint16, uint32, uint64:
		// unsigned integers: floor and truncate division are the same
		return left / right, nil

		// check for floating point special values
	case float32, float64:
		result := left / right

		// check for NaN result (0/0 or Inf/Inf)
		if math.IsNaN(float64(any(result).(float64))) {
			if arithmeticConfiguration.TreatNaNAsWarning {
				// return NaN with a warning (IEEE 754 compliant)
				return result, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Warning, invalidFloatingPointOperationNaN, nil, ast.Divide, left, right, nan)
			}

			// return error and don't allow the operation
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, invalidFloatingPointOperationNaN, nil, ast.Divide, left, right, nan)
		}

		// check for infinity result (x/0 where x != 0)
		if math.IsInf(float64(any(result).(float64)), 0) {
			if arithmeticConfiguration.TreatInfAsWarning {
				// return infinity with a warning (IEEE 754 compliant for x/0)
				return result, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Warning, invalidFloatingPointOperationInf, nil, ast.Divide, left, right, inf)
			}

			// return error and don't allow the operation
			return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, invalidFloatingPointOperationInf, nil, ast.Divide, left, right, inf)
		}

		return result, nil
	}

	return left / right, nil
}

// Generic floor division for signed integers (rounds toward negative infinity).
// Example: -7 / 2 = -4 (floor) vs -3 (truncate)
func floorDivide[T SignedInteger](dividend, divisor T) T {
	quotient := dividend / divisor
	remainder := dividend % divisor

	// adjust for floor division when signs differ and there's a remainder
	// if signs are different and there's a remainder, subtract 1 from quotient
	if (remainder != 0) && ((dividend < 0) != (divisor < 0)) {
		quotient--
	}

	return quotient
}

// Generic modulo operation for all integer types with division by zero checking.
func modulo[T SignedInteger | UnsignedInteger](left, right T) (T, error) {
	var zero T

	if right == zero {
		return zero, eh.NewGeneralError(eh.Analyzer, failureMap, eh.Error, divisionByZeroInConstantExpression, nil, ast.Modulo, left, right)
	}

	// check for signed integer overflow: MIN % -1 (result is 0, but we check for consistency)
	switch l := any(left).(type) {
	case int8:
		r := any(right).(int8)

		// MIN % -1 is actually 0, not an overflow, but check for consistency
		if l == math.MinInt8 && r == -1 {
			return 0, nil
		}

		// apply configured division mode for modulo operation
		if arithmeticConfiguration.IntegerDivisionMode == Floor {
			return any(floorModulo(l, r)).(T), nil
		}

		// default truncate modulo (toward zero)
		return any(l % r).(T), nil

	case int16:
		r := any(right).(int16)

		if l == math.MinInt16 && r == -1 {
			return 0, nil
		}

		// apply configured division mode for modulo operation
		if arithmeticConfiguration.IntegerDivisionMode == Floor {
			return any(floorModulo(l, r)).(T), nil
		}

		// default truncate modulo (toward zero)
		return any(l % r).(T), nil

	case int32:
		r := any(right).(int32)

		if l == math.MinInt32 && r == -1 {
			return 0, nil
		}

		// apply configured division mode for modulo operation
		if arithmeticConfiguration.IntegerDivisionMode == Floor {
			return any(floorModulo(l, r)).(T), nil
		}

		// default truncate modulo (toward zero)
		return any(l % r).(T), nil

	case int64:
		r := any(right).(int64)

		if l == math.MinInt64 && r == -1 {
			return 0, nil
		}

		// apply configured division mode for modulo operation
		if arithmeticConfiguration.IntegerDivisionMode == Floor {
			return any(floorModulo(l, r)).(T), nil
		}

		// default truncate modulo (toward zero)
		return any(l % r).(T), nil

	case uint8, uint16, uint32, uint64:
		// unsigned integers: floor and truncate modulo are the same
		return left % right, nil
	}

	return left % right, nil
}

// Generic floor modulo for signed integers (Euclidean modulo).
// Ensures the result has the same sign as the divisor.
// Example: -7 % 2 = 1 (floor) vs -1 (truncate)
func floorModulo[T SignedInteger](dividend, divisor T) T {
	remainder := dividend % divisor

	// adjust for floor modulo when signs differ and there's a remainder
	// the result should have the same sign as the divisor
	if (remainder != 0) && ((dividend < 0) != (divisor < 0)) {
		remainder += divisor
	}

	return remainder
}

// Generic equality comparison for all comparable types.
func equal[T comparable](left, right T) bool {
	// epsilon-based comparison for floating point types
	switch l := any(left).(type) {
	case float32:
		r := any(right).(float32)

		// NaN is never equal to anything, including itself
		if math.IsNaN(float64(l)) || math.IsNaN(float64(r)) {
			return false
		}

		// epsilon-based comparison for normal float32 values
		return math.Abs(float64(l-r)) < arithmeticConfiguration.Float32Epsilon

	case float64:
		r := any(right).(float64)

		// NaN is never equal to anything, including itself
		if math.IsNaN(l) || math.IsNaN(r) {
			return false
		}

		// epsilon-based comparison for normal float64 values
		return math.Abs(l-r) < arithmeticConfiguration.Float64Epsilon

	default:
		return left == right
	}
}

// Generic inequality comparison for all comparable types.
func notEqual[T comparable](left, right T) bool {
	// epsilon-based comparison for floating point types
	switch l := any(left).(type) {
	case float32:
		r := any(right).(float32)

		// NaN is always not equal to everything, including itself
		if math.IsNaN(float64(l)) || math.IsNaN(float64(r)) {
			return true
		}

		// epsilon-based comparison for normal float32 values
		return math.Abs(float64(l-r)) >= arithmeticConfiguration.Float32Epsilon

	case float64:
		r := any(right).(float64)

		// NaN is always not equal to everything, including itself
		if math.IsNaN(l) || math.IsNaN(r) {
			return true
		}

		// epsilon-based comparison for normal float64 values
		return math.Abs(l-r) >= arithmeticConfiguration.Float64Epsilon

	default:
		return left != right
	}
}

// Generic less-than comparison for all ordered types.
func less[T Numeric](left, right T) bool {
	// check for NaN in floating point comparisons
	switch l := any(left).(type) {
	case float32:
		r := any(right).(float32)

		// any comparison with NaN returns false
		if math.IsNaN(float64(l)) || math.IsNaN(float64(r)) {
			return false
		}

	case float64:
		r := any(right).(float64)

		// any comparison with NaN returns false
		if math.IsNaN(l) || math.IsNaN(r) {
			return false
		}
	}

	return left < right
}

// Generic less-than-or-equal comparison for all ordered types.
func lessEqual[T Numeric](left, right T) bool {
	// check for NaN in floating point comparisons
	switch l := any(left).(type) {
	case float32:
		r := any(right).(float32)

		// any comparison with NaN returns false
		if math.IsNaN(float64(l)) || math.IsNaN(float64(r)) {
			return false
		}

	case float64:
		r := any(right).(float64)

		// any comparison with NaN returns false
		if math.IsNaN(l) || math.IsNaN(r) {
			return false
		}
	}

	return left <= right
}

// Generic greater-than comparison for all ordered types.
func greater[T Numeric](left, right T) bool {
	// check for NaN in floating point comparisons
	switch l := any(left).(type) {
	case float32:
		r := any(right).(float32)

		// any comparison with NaN returns false
		if math.IsNaN(float64(l)) || math.IsNaN(float64(r)) {
			return false
		}

	case float64:
		r := any(right).(float64)

		// any comparison with NaN returns false
		if math.IsNaN(l) || math.IsNaN(r) {
			return false
		}
	}

	return left > right
}

// Generic greater-than-or-equal comparison for all ordered types.
func greaterEqual[T Numeric](left, right T) bool {
	// check for NaN in floating point comparisons
	switch l := any(left).(type) {
	case float32:
		r := any(right).(float32)

		// any comparison with NaN returns false
		if math.IsNaN(float64(l)) || math.IsNaN(float64(r)) {
			return false
		}

	case float64:
		r := any(right).(float64)

		// any comparison with NaN returns false
		if math.IsNaN(l) || math.IsNaN(r) {
			return false
		}
	}

	return left >= right
}
