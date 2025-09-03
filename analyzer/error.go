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
	closureDeterminationFailed
	unknownSymbolKind
	identifierNotFound
	identifierAlreadyDeclared
	constantDataTypeNotFound
	variableDataTypeNotFound
	functionReturnTypeNotFound
	functionParameterTypeNotFound
	expectedConstantIdentifier
	expectedVariableIdentifier
	expectedProcedureIdentifier
	unusedConstantIdentifier
	unusedVariableIdentifier
	unusedProcedureIdentifier
)

// Map failure codes to error messages.
var failureMap = map[eh.Failure]string{
	invalidNameAnalysisState:      "name analysis is in an undefined state and cannot continue parsing",
	declarationValidationFailed:   "identifier declaration validation failed",
	usageValidationFailed:         "identifier usage validation failed",
	closureDeterminationFailed:    "closure determination failed",
	unknownSymbolKind:             "unknown symbol kind: %v",
	identifierNotFound:            "identifier not found: %v",
	identifierAlreadyDeclared:     "identifier already declared: %v",
	constantDataTypeNotFound:      "constant data type not found: %v",
	variableDataTypeNotFound:      "variable data type not found: %v",
	functionReturnTypeNotFound:    "function return data type not found: %v",
	functionParameterTypeNotFound: "function parameter data type not found: %v",
	expectedConstantIdentifier:    "expected constant identifier, found %v",
	expectedVariableIdentifier:    "expected variable identifier, found %v",
	expectedProcedureIdentifier:   "expected procedure identifier, found %v",
	unusedConstantIdentifier:      "constant declared but not used: %v",
	unusedVariableIdentifier:      "variable declared but not used: %v",
	unusedProcedureIdentifier:     "procedure declared but not used: %v",
}
