// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package compiler

import (
	_ "embed"
	"os"
)

// Embed the source file of the PL/0 standard library at compile time.
//
//go:embed standard/standard.c
var embeddedStandardLibrary []byte

// Extract the embedded standard library source file to the standard source path location.
func ExtractStandardLibrary(standardSourcePath string) error {
	if err := os.WriteFile(standardSourcePath, embeddedStandardLibrary, 0644); err != nil {
		return err
	}

	return nil
}
