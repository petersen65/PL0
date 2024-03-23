// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package analyzer

import tok "github.com/petersen65/PL0/token"

// Failure codes for the PL/0 semantic analyzer.
const (
	_ = tok.Failure(iota + 3000)
	invalidNameAnalysisState
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
var failureMap = map[tok.Failure]string{
	invalidNameAnalysisState:    "name analysis is in an undefined state and cannot continue parsing",
	identifierNotFound:          "identifier not found: %v",
	identifierAlreadyDeclared:   "identifier already declared: %v",
	expectedConstantIdentifier:  "expected constant identifier, found %v",
	expectedVariableIdentifier:  "expected variable identifier, found %v",
	expectedProcedureIdentifier: "expected procedure identifier, found %v",
	unusedConstantIdentifier:    "constant declared but not used: %v",
	unusedVariableIdentifier:    "variable declared but not used: %v",
	unusedProcedureIdentifier:   "procedure declared but not used: %v",
}
