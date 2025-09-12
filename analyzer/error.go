// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package analyzer

import eh "github.com/petersen65/pl0/v3/errors"

// Failure codes for the analyzer.
const (
	_ eh.Failure = iota + 4000
	invalidNameAnalysisState
	declarationValidationFailed
	usageValidationFailed
	capturedVariableDeterminationFailed
	unknownSymbolKind
	unknownExpressionKind
	unknownUnaryOperation
	identifierNotFound
	identifierAlreadyDeclared
	constantDataTypeNotFound
	variableDataTypeNotFound
	constantDataTypeCannotBeInferred
	constantExpressionMustBeConstant
	constantIdentifierHasBuiltInName
	variableIdentifierHasBuiltInName
	functionIdentifierHasBuiltInName
	procedureIdentifierHasBuiltInName
	functionReturnTypeNotFound
	functionParameterTypeNotFound
	procedureParameterTypeNotFound
	expectedConstantIdentifier
	expectedVariableIdentifier
	expectedFunctionIdentifier
	expectedProcedureIdentifier
	unusedConstantIdentifier
	unusedVariableIdentifier
	unusedFunctionIdentifier
	unusedProcedureIdentifier
	incompatibleDataTypesInArithmeticOperation
	dataTypeCannotBeUsedInArithmeticOperation
	incompatibleDataTypesInComparisonOperation
	dataTypeCannotBeUsedInComparisonOperation
	dataTypeCannotBeUsedInUnaryOperation
)

// Map failure codes to error messages.
var failureMap = map[eh.Failure]string{
	invalidNameAnalysisState:                   "name analysis is in an undefined state and cannot continue parsing",
	declarationValidationFailed:                "identifier declaration validation failed",
	usageValidationFailed:                      "identifier usage validation failed",
	capturedVariableDeterminationFailed:        "captured variable determination failed",
	unknownSymbolKind:                          "unknown symbol kind: %v",
	unknownExpressionKind:                      "unknown kind of expression: %v",
	unknownUnaryOperation:                      "unknown unary operation: %v",
	identifierNotFound:                         "identifier not found: %v",
	identifierAlreadyDeclared:                  "identifier already declared: %v",
	constantDataTypeNotFound:                   "constant data type not found: %v",
	variableDataTypeNotFound:                   "variable data type not found: %v",
	constantDataTypeCannotBeInferred:           "constant data type cannot be inferred for: %v",
	constantExpressionMustBeConstant:           "constant expression must be a constant at compile time: %v",
	constantIdentifierHasBuiltInName:           "constant identifier uses a built-in reserved name: %v",
	variableIdentifierHasBuiltInName:           "variable identifier uses a built-in reserved name: %v",
	functionIdentifierHasBuiltInName:           "function identifier uses a built-in reserved name: %v",
	procedureIdentifierHasBuiltInName:          "procedure identifier uses a built-in reserved name: %v",
	functionReturnTypeNotFound:                 "function return data type not found: %v",
	functionParameterTypeNotFound:              "function parameter data type not found: %v",
	procedureParameterTypeNotFound:             "procedure parameter data type not found: %v",
	expectedConstantIdentifier:                 "expected constant identifier, found %v",
	expectedVariableIdentifier:                 "expected variable identifier, found %v",
	expectedFunctionIdentifier:                 "expected function identifier, found %v",
	expectedProcedureIdentifier:                "expected procedure identifier, found %v",
	unusedConstantIdentifier:                   "constant declared but not used: %v",
	unusedVariableIdentifier:                   "variable declared but not used: %v",
	unusedFunctionIdentifier:                   "function declared but not used: %v",
	unusedProcedureIdentifier:                  "procedure declared but not used: %v",
	incompatibleDataTypesInArithmeticOperation: "incompatible operand data types in arithmetic operation %v: %v and %v",
	dataTypeCannotBeUsedInArithmeticOperation:  "operand data type cannot be used in arithmetic operation %v [%v]: %v",
	incompatibleDataTypesInComparisonOperation: "incompatible operand data types in comparison operation %v: %v and %v",
	dataTypeCannotBeUsedInComparisonOperation:  "operand data type cannot be used in comparison operation %v [%v]: %v",
	dataTypeCannotBeUsedInUnaryOperation:       "operand data type cannot be used in unary operation %v [%v]: %v",
}
