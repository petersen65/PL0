// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package emulator provides an emulation engine for IL/0.
package emulator

// Run a binary IL/0 target and return an error if the target fails to execute.
func Run(raw []byte) error {
	return newMachine().runProgram(raw)
}
