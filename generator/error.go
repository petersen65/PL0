// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package generator

import cor "github.com/petersen65/PL0/v2/core"

// Failure codes for the intermediate code generator.
const (
	_ = cor.Failure(iota + 5000)
	intermediateCodeGenerationFailed
	unsupportedDataTypeInConstantDeclaration
	unsupportedDataTypeInVariableDeclaration
	invalidContextInIdentifierUse
	unknownUnaryOperation
	unknownBinaryOperation
	unknownConditionalOperation
	unexpectedIntermediateCodeResult
)

// Map failure codes to error messages.
var failureMap = map[cor.Failure]string{
	intermediateCodeGenerationFailed:         "intermediate code generation failed",
	unsupportedDataTypeInConstantDeclaration: "unsupported data type in constant declaration: %v",
	unsupportedDataTypeInVariableDeclaration: "unsupported data type in variable declaration: %v",
	invalidContextInIdentifierUse:            "invalid context in identifier use",
	unknownUnaryOperation:                    "unknown unary operation",
	unknownBinaryOperation:                   "unknown binary operation",
	unknownConditionalOperation:              "unknown conditional operation",
	unexpectedIntermediateCodeResult:         "unexpected intermediate code result",
}
