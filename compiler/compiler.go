// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package compiler provides functions that compile PL/0 source code into assembly language code.
package compiler

import (
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	ana "github.com/petersen65/PL0/v2/analyzer"
	ast "github.com/petersen65/PL0/v2/ast"
	cod "github.com/petersen65/PL0/v2/code"
	cor "github.com/petersen65/PL0/v2/core"
	emu "github.com/petersen65/PL0/v2/emulator"
	par "github.com/petersen65/PL0/v2/parser"
	scn "github.com/petersen65/PL0/v2/scanner"
)

// Default target filename for the compilation driver.
const defaultTarget = "out"

// Default build directory for target files.
const defaultBuildDirectory = "build"

// Directory for exported intermediate representations.
const intermediateDirectory = "obj"

// Text messages for the compilation driver.
const (
	textCleaning           = "Cleaning target directory '%v'\n"
	textCompiling          = "Compiling source code '%v' to binary target '%v'\n"
	textErrorCompiling     = "Error compiling source code '%v': %v"
	textAbortCompilation   = "compilation aborted\n"
	textErrorPersisting    = "Error persisting binary target '%v': %v"
	textExporting          = "Exporting intermediate representations to '%v'\n"
	textErrorExporting     = "Error exporting intermediate representations '%v': %v"
	textEmulating          = "Emulating binary target '%v'\n"
	textErrorEmulating     = "Error emulating binary target '%v': %v"
	textDriverSourceTarget = "Compiler Driver with source code '%v' and binary target '%v' completed\n"
	textDriverTarget       = "Compiler Driver with binary target '%v' completed\n"
)

// Options for the compilation driver as bit-mask.
const (
	Clean DriverOption = 1 << iota
	Compile
	Export
	Emulate
)

// File extensions for all generated files.
const (
	_ Extension = iota
	Assembler
	Token
	Tree
	Error
	Intermediate
	Json
	Text
	Binary
	Ensure
)

type (
	// Driver options for the compilation process.
	DriverOption uint64

	// File extensions for files used during compilation.
	Extension int

	// TranslationUnit represents source content and all intermediate results of the compilation process.
	TranslationUnit struct {
		SourceContent  []byte           // PL/0 utf-8 source content
		TokenStream    cor.TokenStream  // token stream of the PL/0 source content
		AbstractSyntax ast.Block        // abstract syntax tree of the PL/0 source code
		Module         cod.Module       // intermediate code module
		ErrorHandler   cor.ErrorHandler // error handler of the compilation process
	}
)

// ExtensionMap maps file extensions to their string representation.
var ExtensionMap = map[Extension]string{
	Assembler:    ".asm",
	Token:        ".tok",
	Tree:         ".ast",
	Error:        ".err",
	Intermediate: ".ilc",
	Json:         ".json",
	Text:         ".txt",
	Binary:       ".bin",
	Ensure:       ".ensure",
}

// Driver for the compilation process with the given options, source, target, and print writer.
func Driver(options DriverOption, source, target string, print io.Writer) {
	var translationUnit TranslationUnit
	var targetDirectory, baseFileName string
	var err error

	// ensure target path exists and print persistence error message if an error occurred
	if targetDirectory, baseFileName, err = EnsureTargetPath(target); err != nil {
		fmt.Fprintf(print, textErrorPersisting, target, err)
		return
	}

	// cleaned and validated target path
	target = GetFullPath(targetDirectory, baseFileName, Binary)

	// clean target directory and assume that the first ensuring of the target path was successful
	if options&Clean != 0 {
		fmt.Fprintf(print, textCleaning, targetDirectory)
		os.RemoveAll(targetDirectory)

		// repeat ensuring existance of target path only if compile option is set
		if options&Compile != 0 {
			// repeat ensuring existance of target path after cleaning and print persistence error message if an error occurred this time
			if targetDirectory, baseFileName, err = EnsureTargetPath(target); err != nil {
				fmt.Fprintf(print, textErrorPersisting, target, err)
				return
			}

			// cleaned and validated target path after cleaning
			target = GetFullPath(targetDirectory, baseFileName, Binary)
		}
	}

	// compile source code to module and persist module to binary target
	if options&Compile != 0 {
		fmt.Fprintf(print, textCompiling, source, target)
		translationUnit, err = CompileSourceToTranslationUnit(source)

		// print compilation error message if an I/O error occurred during compilation
		if err != nil {
			fmt.Fprintf(print, textErrorCompiling, source, err)
			return
		}

		// print error report if errors with any severity occurred during compilation
		translationUnit.ErrorHandler.Print(print)

		// print abandon compilation message if any error occurred during compilation
		if translationUnit.ErrorHandler.HasErrors() {
			fmt.Fprintf(print, textErrorCompiling, source, textAbortCompilation)
		} else {
			// persist module to target and print persistence error message if an error occurred
			if err = PersistModuleToTarget(translationUnit.Module, target); err != nil {
				fmt.Fprintf(print, textErrorPersisting, target, err)
				return
			}
		}
	}

	// export intermediate representations to the target path
	if options&Export != 0 {
		fmt.Fprintf(print, textExporting, filepath.Join(targetDirectory, intermediateDirectory))

		if err = ExportIntermediateRepresentationsToTarget(translationUnit, targetDirectory, baseFileName); err != nil {
			fmt.Fprintf(print, textErrorExporting, filepath.Join(targetDirectory, intermediateDirectory), err)
			return
		}
	}

	// emulate binary target if no errors occurred during compilation
	if options&Emulate != 0 && !translationUnit.ErrorHandler.HasErrors() {
		fmt.Fprintf(print, textEmulating, target)

		if err = EmulateTarget(target); err != nil {
			fmt.Fprintf(print, textErrorEmulating, target, err)
			return
		}
	}

	// print driver completion message
	if options&Compile != 0 {
		fmt.Fprintf(print, textDriverSourceTarget, source, target)
	} else {
		fmt.Fprintf(print, textDriverTarget, target)
	}
}

// Ensure target path exists and create and remove empty ensure-file to proof write access.
func EnsureTargetPath(target string) (string, string, error) {
	var targetDirectory string
	target = filepath.Clean(target)

	// enforce default build directory if target is empty, current directory, or root directory
	if target == "." || target == "" || target == string(os.PathSeparator) {
		target = defaultBuildDirectory
	}

	// if target has no extension, it is assumed to be the target directory
	if filepath.Ext(target) == "" {
		targetDirectory = target
	} else {
		targetDirectory = filepath.Dir(target)
	}

	// enforce default build directory if target directory is empty, current directory, or root directory
	if targetDirectory == "." || targetDirectory == "" || targetDirectory == string(os.PathSeparator) {
		targetDirectory = defaultBuildDirectory
	}

	// 0755 is octal and means that the owner can read, write, and execute the file, and others can read and execute it
	if err := os.MkdirAll(targetDirectory, 0755); err != nil {
		return "", "", err
	}

	// return if target directory does not exist after trying to create it
	if _, err := os.Stat(targetDirectory); os.IsNotExist(err) {
		return "", "", err
	}

	// get base file name of target path without extension
	baseFileName := filepath.Base(strings.TrimSuffix(target, filepath.Ext(target)))

	// enforce default target file name if base file name is empty, current directory, or root directory
	if baseFileName == "." || baseFileName == "" || baseFileName == string(os.PathSeparator) || baseFileName == target {
		baseFileName = defaultTarget
	}

	// full file path of target with extension
	targetFullPath := filepath.Join(targetDirectory, baseFileName) + ExtensionMap[Ensure]

	// create and remove empty target file to proof write access
	if targetFile, err := os.Create(targetFullPath); err != nil {
		return "", "", err
	} else {
		defer func() {
			targetFile.Close()
			os.Remove(targetFullPath)
		}()
	}

	return targetDirectory, baseFileName, nil
}

// Compile source code and return translation unit with all intermediate results and error handler.
func CompileSourceToTranslationUnit(source string) (TranslationUnit, error) {
	if content, err := os.ReadFile(source); err != nil {
		return TranslationUnit{}, err
	} else {
		return CompileContent(content), nil
	}
}

// Persist a module to the given binary target.
func PersistModuleToTarget(module cod.Module, target string) error {
	var err error
	var program *os.File

	if program, err = os.Create(target); err != nil {
		return err
	} else {
		// safely close file and remove it if there is an error
		defer func() {
			program.Close()

			if err != nil {
				os.Remove(target)
			}
		}()

		machine := emu.NewMachine()

		if err = machine.LoadModule(module); err != nil {
			return err
		}

		if err = machine.Export(cor.Binary, program); err != nil {
			return err
		}
	}

	return nil
}

// Export all intermediate representations to the target path.
func ExportIntermediateRepresentationsToTarget(translationUnit TranslationUnit, targetDirectory, baseFileName string) error {
	var anyError error

	// create full intermediate directory path
	targetDirectory = filepath.Join(targetDirectory, intermediateDirectory)

	// create full directory path and check if an ensure-test file can be created by creating it and removing it afterwards
	if _, _, err := EnsureTargetPath(targetDirectory); err != nil {
		return err
	}

	// create all Json files for intermediate representations
	tsjFile, tsjErr := os.Create(GetFullPath(targetDirectory, baseFileName, Token, Json))
	asjFile, asjErr := os.Create(GetFullPath(targetDirectory, baseFileName, Tree, Json))
	erjFile, erjErr := os.Create(GetFullPath(targetDirectory, baseFileName, Error, Json))

	// create all Text files for intermediate representations
	tstFile, tstErr := os.Create(GetFullPath(targetDirectory, baseFileName, Token, Text))
	astFile, astErr := os.Create(GetFullPath(targetDirectory, baseFileName, Tree, Text))
	ictFile, ictErr := os.Create(GetFullPath(targetDirectory, baseFileName, Intermediate, Text))
	amtFile, amtErr := os.Create(GetFullPath(targetDirectory, baseFileName, Assembler, Text))
	ertFile, ertErr := os.Create(GetFullPath(targetDirectory, baseFileName, Error, Text))

	// create all Binary files for intermediate representations
	tsbFile, tsbErr := os.Create(GetFullPath(targetDirectory, baseFileName, Token, Binary))

	// check if any error occurred during file creations
	anyError = errors.Join(tsjErr, asjErr, erjErr, tstErr, astErr, ictErr, amtErr, ertErr, tsbErr)

	// close all files and remove target directory if any error occurred during file creations
	defer func() {
		// safely close file and remove it if it is empty
		var closeFile = func(file *os.File) {
			if file != nil {
				info, _ := file.Stat()
				file.Close()

				if info != nil && info.Size() == 0 {
					os.Remove(file.Name())
				}
			}
		}

		// safely close all files
		closeFile(tsjFile)
		closeFile(asjFile)
		closeFile(erjFile)
		closeFile(tstFile)
		closeFile(astFile)
		closeFile(ictFile)
		closeFile(amtFile)
		closeFile(ertFile)
		closeFile(tsbFile)

		// remove target directory if any error occurred during file creations
		if anyError != nil {
			os.RemoveAll(targetDirectory)
		}
	}()

	if anyError != nil {
		return anyError
	}

	// export all token stream representations to the target files
	if translationUnit.TokenStream != nil {
		tsjErr = translationUnit.TokenStream.Export(cor.Json, tsjFile)
		tstErr = translationUnit.TokenStream.Export(cor.Text, tstFile)
		tsbErr = translationUnit.TokenStream.Export(cor.Binary, tsbFile)
	}

	// export all abstract syntax representations to the target files
	if translationUnit.AbstractSyntax != nil {
		asjErr = translationUnit.AbstractSyntax.Export(cor.Json, asjFile)
		astErr = translationUnit.AbstractSyntax.Export(cor.Text, astFile)
	}

	// export all intermediate code representations to the target files
	if translationUnit.Module != nil {
		ictErr = translationUnit.Module.Export(cor.Text, ictFile)
	}

	// export all assembly representations to the target files
	if translationUnit.Module != nil {
		machine := emu.NewMachine()

		if amtErr = machine.LoadModule(translationUnit.Module); amtErr == nil {
			amtErr = machine.Export(cor.Text, amtFile)
		}
	}

	// export all error handler representations to the target files
	if translationUnit.ErrorHandler != nil {
		erjErr = translationUnit.ErrorHandler.Export(cor.Json, erjFile)
		ertErr = translationUnit.ErrorHandler.Export(cor.Text, ertFile)
	}

	// check if any error occurred during export of intermediate representations
	anyError = errors.Join(tsjErr, asjErr, erjErr, tstErr, astErr, ictErr, amtErr, ertErr, tsbErr)
	return anyError
}

// Run module with the emulator.
func EmulateModule(module cod.Module) error {
	machine := emu.NewMachine()

	if err := machine.LoadModule(module); err != nil {
		return err
	}

	if err := machine.RunProcess(); err != nil {
		return err
	}

	return nil
}

// Run binary target with the emulator.
func EmulateTarget(target string) error {
	if raw, err := os.ReadFile(target); err != nil {
		return err
	} else {
		machine := emu.NewMachine()

		if err := machine.Load(raw); err != nil {
			return err
		}

		if err := machine.RunProcess(); err != nil {
			return err
		}
	}

	return nil
}

// Return the full path of the target file with the given base file name and extensions.
func GetFullPath(targetDirectory, baseFileName string, extensions ...Extension) string {
	targetFullPath := filepath.Join(targetDirectory, baseFileName)

	for _, extension := range extensions {
		targetFullPath += ExtensionMap[extension]
	}

	return targetFullPath
}

// Compile UTF-8 encoded content and return translation unit with all intermediate results and error handler.
func CompileContent(content []byte) TranslationUnit {
	// lexical analysis of content
	tokenStream, scannerError := scn.NewScanner().Scan(content)
	errorHandler := cor.NewErrorHandler(tokenStream)
	errorHandler.AppendError(scannerError) // nil errors are ignored

	// syntax analysis and semantic analysis of token stream
	abstractSyntax, tokenHandler := par.NewParser(tokenStream, errorHandler).Parse()
	ana.NewNameAnalysis(abstractSyntax, errorHandler, tokenHandler).Analyze()

	// return if any fatal or error errors occurred during lexical, syntax, or semantic analysis
	if errorHandler.Count(cor.Fatal|cor.Error, cor.AllComponents) > 0 {
		return TranslationUnit{content, tokenStream, nil, nil, errorHandler}
	}

	// intermediate code generation based on the abstract syntax tree results in a module
	intermediate := cod.NewIntermediateCode(abstractSyntax)
	intermediate.Generate()
	module := intermediate.GetModule()

	// return if any fatal or error errors occurred during code generation
	if errorHandler.Count(cor.Fatal|cor.Error, cor.AllComponents) > 0 {
		return TranslationUnit{content, tokenStream, abstractSyntax, nil, errorHandler}
	}

	// return translation unit with all intermediate results and error handler
	return TranslationUnit{content, tokenStream, abstractSyntax, module, errorHandler}
}
