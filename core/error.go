// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package core

// Failure codes for core components of the PL/0 compiler.
const (
	_ Failure = iota + 100
	errorKindNotSupported
	unknownExportFormat
	errorReportExportFailed
	tokenStreamExportFailed
)

// Map failure codes to error messages.
var failureMap = map[Failure]string{
	errorKindNotSupported:   "error kind not supported: %v",
	unknownExportFormat:     "unknown export format: %v",
	errorReportExportFailed: "failed to export the error report",
	tokenStreamExportFailed: "failed to export the token stream",
}
