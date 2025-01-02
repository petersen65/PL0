// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package scanner

import cor "github.com/petersen65/PL0/v2/core"

// Failure codes for the PL/0 scanner.
const (
	_ = cor.Failure(iota + 1000)
	eofComment
)

// Map failure codes to error messages.
var failureMap = map[cor.Failure]string{
	eofComment: "end of file reached inside comment",
}
