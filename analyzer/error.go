// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package analyzer

import tok "github.com/petersen65/PL0/token"

// Failure codes for the PL/0 semantic analyzer.
const (
	_ = tok.Failure(iota + 3000)
	invalidDeclarationAnalysisState
	declarationAnalysisError
	declarationAnalysisErrors
	identifierNotFound
	identifierAlreadyDeclared
)

// Map failure codes to error messages.
var failureMap = map[tok.Failure]string{
	invalidDeclarationAnalysisState: "declaration analysis is in an undefined state and cannot continue parsing",
	declarationAnalysisError:        "a declaration analysis error occurred",
	declarationAnalysisErrors:       "%v declaration analysis errors occurred",
	identifierNotFound:              "identifier not found: %v",
	identifierAlreadyDeclared:       "identifier already declared: %v",
}
