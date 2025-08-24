// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package compiler

import (
    _ "embed"
    "os"
    "path/filepath"
)

// Embed the standard C library source file at compile time
//go:embed standard/standard.c
var embeddedStandardC []byte

// ExtractStandardLibrary extracts the embedded standard.c file to the target directory
func ExtractStandardLibrary(targetDirectory string) (string, error) {
    // Ensure target directory exists
    if err := os.MkdirAll(targetDirectory, 0755); err != nil {
        return "", err
    }

    // Create the full path for the standard.c file
    standardPath := filepath.Join(targetDirectory, "standard.c")

    // Write the embedded content to disk
    if err := os.WriteFile(standardPath, embeddedStandardC, 0644); err != nil {
        return "", err
    }

    return standardPath, nil
}

// GetEmbeddedStandardC returns the embedded standard.c content
func GetEmbeddedStandardC() []byte {
    return embeddedStandardC
}