// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

package emulator

import cod "github.com/petersen65/PL0/v2/code"

// JIT compile a module into a text section of a process and return an error if the module fails to compile.
func (p *process) jitCompile(module cod.Module) error {
	p.text = make(textSection, 0)
	return nil
}
