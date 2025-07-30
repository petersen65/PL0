// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package analyzer

import cor "github.com/petersen65/PL0/v2/core"

// Failure codes for the analyzer.
const (
	_ cor.Failure = iota + 4000
	invalidNameAnalysisState
	declarationValidationFailed
	usageValidationFailed
	closureDeterminationFailed
	unknownSymbolKind
	identifierNotFound
	identifierAlreadyDeclared
	expectedConstantIdentifier
	expectedVariableIdentifier
	expectedProcedureIdentifier
	unusedConstantIdentifier
	unusedVariableIdentifier
	unusedProcedureIdentifier
)

// Map failure codes to error messages.
var failureMap = map[cor.Failure]string{
	invalidNameAnalysisState:    "name analysis is in an undefined state and cannot continue parsing",
	declarationValidationFailed: "identifier declaration validation failed",
	usageValidationFailed:       "identifier usage validation failed",
	closureDeterminationFailed:  "closure determination failed",
	unknownSymbolKind:           "unknown symbol kind: %v",
	identifierNotFound:          "identifier not found: %v",
	identifierAlreadyDeclared:   "identifier already declared: %v",
	expectedConstantIdentifier:  "expected constant identifier, found %v",
	expectedVariableIdentifier:  "expected variable identifier, found %v",
	expectedProcedureIdentifier: "expected procedure identifier, found %v",
	unusedConstantIdentifier:    "constant declared but not used: %v",
	unusedVariableIdentifier:    "variable declared but not used: %v",
	unusedProcedureIdentifier:   "procedure declared but not used: %v",
}
