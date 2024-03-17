// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package scanner

import tok "github.com/petersen65/PL0/token"

// Failure codes for the PL/0 scanner.
const (
	_ = tok.Failure(iota + 1000)
	eofComment
)

// Map failure codes to error messages.
var failureMap = map[tok.Failure]string{
	eofComment: "end of file reached inside comment",
}
