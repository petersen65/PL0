// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package token

import eh "github.com/petersen65/pl0/v3/errors"

// Failure codes for token handling.
const (
	_ eh.Failure = iota + 500
	unknownExportFormat
	tokenStreamExportFailed
)

// Map failure codes to error messages.
var failureMap = map[eh.Failure]string{
	unknownExportFormat:     "unknown export format: %v",
	tokenStreamExportFailed: "failed to export the token stream",
}
