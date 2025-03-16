// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package cfg

import cor "github.com/petersen65/PL0/v2/core"

// Failure codes for the control flow graph.
const (
	_ = cor.Failure(iota + 7000)
	unknownExportFormat
	controlFlowGraphExportFailed
)

// Map failure codes to error messages.
var failureMap = map[cor.Failure]string{
	unknownExportFormat:          "unknown export format: %v",
	controlFlowGraphExportFailed: "failed to export control flow graph",
}
