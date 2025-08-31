// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package export provides functionality for exporting intermediate results.
package export

import "io"

// Export formats for the compiler which can be used to export intermediate results.
const (
	Json ExportFormat = iota
	Text
	Binary
)

type (
	// Export formats for the compiler.
	ExportFormat int

	// Exporter is an interface that provides methods for exporting intermediate results.
	Exporter interface {
		Print(print io.Writer, args ...any) error
		Export(format ExportFormat, print io.Writer) error
	}

	// Importer is an interface that provides methods for importing intermediate results.
	Importer interface {
		Import(format ExportFormat, scan io.Reader) error
	}
)
