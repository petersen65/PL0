// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package code

import cor "github.com/petersen65/PL0/v2/core"

// Failure codes for the intermediate code generator.
const (
	_ = cor.Failure(iota + 5000)
	unknownInstructionOption
	unknownDataTypeRepresentation
	invalidContextInIdentifierUse
	symbolTableUpdateFailed
	symbolMetaDataUpdateFailed
	unknownUnaryOperation
	unknownBinaryOperation
	unknownConditionalOperation
	unexpectedIntermediateCodeResult
	unknownExportFormat
	intermediateCodeExportFailed
)

// Map failure codes to error messages.
var failureMap = map[cor.Failure]string{
	unknownInstructionOption:         "unknown instruction option: %v",
	unknownDataTypeRepresentation:    "unknown data type representation: %v",
	invalidContextInIdentifierUse:    "invalid context in identifier use",
	symbolTableUpdateFailed:          "error while updating intermediate code symbol table: %v",
	symbolMetaDataUpdateFailed:       "error while updating symbol metadata from abstract syntax tree: %v",
	unknownUnaryOperation:            "unknown unary operation",
	unknownBinaryOperation:           "unknown binary operation",
	unknownConditionalOperation:      "unknown conditional operation",
	unexpectedIntermediateCodeResult: "unexpected intermediate code result",
	unknownExportFormat:              "unknown export format: %v",
	intermediateCodeExportFailed:     "failed to export intermediate code",
}
