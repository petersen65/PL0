// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package scanner

import eh "github.com/petersen65/pl0/v3/errors"

// Failure codes for the scanner.
const (
	_ eh.Failure = iota + 1000
	eofComment
)

// Map failure codes to error messages.
var failureMap = map[eh.Failure]string{
	eofComment: "end of file reached inside comment",
}
