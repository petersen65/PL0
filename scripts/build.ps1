# Copyright 2024-2025 Michael Petersen. All rights reserved.
# Use of this source code is governed by an Apache license that can be found in the LICENSE file.

Set-Variable -Name 'LinkCommand' -Value "-X main.CommitHash=$(git rev-parse --short HEAD)"
go build -v -a -o bin/pl0.exe -ldflags $LinkCommand
