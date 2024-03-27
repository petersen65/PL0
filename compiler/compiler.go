// Copyright 2024 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package compiler provides functions that compile PL/0 source code into IL/0 intermediate language code.
package compiler

import (
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	ana "github.com/petersen65/PL0/analyzer"
	ast "github.com/petersen65/PL0/ast"
	cor "github.com/petersen65/PL0/core"
	emt "github.com/petersen65/PL0/emitter"
	emu "github.com/petersen65/PL0/emulator"
	gen "github.com/petersen65/PL0/generator"
	par "github.com/petersen65/PL0/parser"
	scn "github.com/petersen65/PL0/scanner"
)

// Default target filename for the compilation driver.
const defaultTarget = "out"

// Default build directory for target files.
const defaultBuildDirectory = "build"

// Directory for exported intermediate representations.
const intermediateDirectory = "obj"

// Text messages for the compilation driver.
const (
	textCleaning		   = "Cleaning target directory '%v'\n"
	textCompiling          = "Compiling PL0 source '%v' to IL0 target '%v'\n"
	textErrorCompiling     = "Error compiling PL0 source '%v': %v"
	textAbortCompilation   = "compilation aborted"
	textErrorPersisting    = "Error persisting IL0 target '%v': %v"
	textExporting          = "Exporting intermediate representations to '%v'\n"
	textErrorExporting     = "Error exporting intermediate representations '%v': %v"
	textEmulating          = "Emulating IL0 target '%v'\n"
	textErrorEmulating     = "Error emulating IL0 target '%v': %v"
	textDriverSourceTarget = "Compiler Driver with PL0 source '%v' and IL0 target '%v' completed\n"
	textDriverTarget       = "Compiler Driver with IL0 target '%v' completed\n"
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
	PL0
	IL0
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
		Sections       emt.TextSection  // IL/0 intermediate language target
		ErrorHandler   cor.ErrorHandler // error handler of the compilation process
	}
)

// ExtensionMap maps file extensions to their string representation.
var ExtensionMap = map[Extension]string{
	PL0:          ".pl0",
	IL0:          ".il0",
	Token:        ".tok",
	Tree:         ".ast",
	Error:        ".err",
	Intermediate: ".int",
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
		print.Write([]byte(fmt.Sprintf(textErrorPersisting, target, err)))
		return
	}

	// cleaned and validated target path
	target = GetFullPath(targetDirectory, baseFileName, IL0)

	// clean target directory and assume that the first ensuring of the target path was successful
	if options&Clean != 0 {
		print.Write([]byte(fmt.Sprintf(textCleaning, targetDirectory)))
		os.RemoveAll(targetDirectory)

		// repeat ensuring existance of target path only if compile option is set
		if options&Compile != 0 {
			// repeat ensuring existance of target path after cleaning and print persistence error message if an error occurred this time
			if targetDirectory, baseFileName, err = EnsureTargetPath(target); err != nil {
				print.Write([]byte(fmt.Sprintf(textErrorPersisting, target, err)))
				return
			}

			// cleaned and validated target path after cleaning
			target = GetFullPath(targetDirectory, baseFileName, IL0)
		}
	}

	// compile PL/0 source to IL/0 target and persist IL/0 sections to target
	if options&Compile != 0 {
		print.Write([]byte(fmt.Sprintf(textCompiling, source, target)))
		translationUnit, err = CompileSourceToTranslationUnit(source)

		// print compilation error message if an error occurred during compilation process
		if err != nil {
			print.Write([]byte(fmt.Sprintf(textErrorCompiling, source, err)))
			return
		}

		// print error report if errors with any severity occurred during compilation process
		translationUnit.ErrorHandler.Print(print)

		// abandon compilation if any fatal or error errors occurred during compilation process
		if translationUnit.ErrorHandler.Count(cor.Fatal|cor.Error, cor.AllComponents) > 0 {
			print.Write([]byte(fmt.Sprintf(textErrorCompiling, source, errors.New(textAbortCompilation))))
			return
		}

		// persist IL/0 sections to target and print persistence error message if an error occurred
		if err = PersistSectionsToTarget(translationUnit.Sections, target); err != nil {
			print.Write([]byte(fmt.Sprintf(textErrorPersisting, target, err)))
			return
		}
	}

	// export intermediate representations to the target path
	if options&Export != 0 {
		print.Write([]byte(fmt.Sprintf(textExporting, filepath.Join(targetDirectory, intermediateDirectory))))

		if err = ExportIntermediateRepresentationsToTarget(translationUnit, targetDirectory, baseFileName); err != nil {
			print.Write([]byte(fmt.Sprintf(textErrorExporting, filepath.Join(targetDirectory, intermediateDirectory), err)))
			return
		}
	}

	// emulate IL/0 target
	if options&Emulate != 0 {
		print.Write([]byte(fmt.Sprintf(textEmulating, target)))

		if err = EmulateTarget(target); err != nil {
			print.Write([]byte(fmt.Sprintf(textErrorEmulating, target, err)))
			return
		}
	}

	// print driver completion message
	if options&Compile != 0 {
		print.Write([]byte(fmt.Sprintf(textDriverSourceTarget, source, target)))
	} else {
		print.Write([]byte(fmt.Sprintf(textDriverTarget, target)))
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

// Compile PL/0 source and return translation unit with all intermediate results and error handler.
func CompileSourceToTranslationUnit(source string) (TranslationUnit, error) {
	if content, err := os.ReadFile(source); err != nil {
		return TranslationUnit{}, err
	} else {
		return CompileContent(content), nil
	}
}

// Persist IL/0 sections to the given target.
func PersistSectionsToTarget(sections emt.TextSection, target string) error {
	if program, err := os.Create(target); err != nil {
		return err
	} else {
		defer program.Close()

		if err := sections.Export(cor.Binary, program); err != nil {
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
	iljFile, iljErr := os.Create(GetFullPath(targetDirectory, baseFileName, Intermediate, Json))
	erjFile, erjErr := os.Create(GetFullPath(targetDirectory, baseFileName, Error, Json))

	// create all Text files for intermediate representations
	tstFile, tstErr := os.Create(GetFullPath(targetDirectory, baseFileName, Token, Text))
	astFile, astErr := os.Create(GetFullPath(targetDirectory, baseFileName, Tree, Text))
	iltFile, iltErr := os.Create(GetFullPath(targetDirectory, baseFileName, Intermediate, Text))
	ertFile, ertErr := os.Create(GetFullPath(targetDirectory, baseFileName, Error, Text))

	// create all Binary files for intermediate representations
	tsbFile, tsbErr := os.Create(GetFullPath(targetDirectory, baseFileName, Token, Binary))
	ilbFile, ilbErr := os.Create(GetFullPath(targetDirectory, baseFileName, Intermediate, Binary))

	// check if any error occurred during file creations
	anyError = errors.Join(tsjErr, iljErr, erjErr, tstErr, astErr, iltErr, ertErr, tsbErr, ilbErr)

	// close all files and remove target directory if any error occurred during file creations
	defer func() {
		var closeFile = func(file *os.File) {
			if file != nil {
				file.Close()
			}
		}

		closeFile(tsjFile)
		closeFile(iljFile)
		closeFile(erjFile)
		closeFile(tstFile)
		closeFile(astFile)
		closeFile(iltFile)
		closeFile(ertFile)
		closeFile(tsbFile)
		closeFile(ilbFile)

		if anyError != nil {
			os.RemoveAll(targetDirectory)
		}
	}()

	if anyError != nil {
		return anyError
	}

	// export all intermediate representations as Json
	tsjErr = translationUnit.TokenStream.Export(cor.Json, tsjFile)
	iljErr = translationUnit.Sections.Export(cor.Json, iljFile)
	erjErr = translationUnit.ErrorHandler.Export(cor.Json, erjFile)

	// export all intermediate representations as Text
	tstErr = translationUnit.TokenStream.Export(cor.Text, tstFile)
	astErr = translationUnit.AbstractSyntax.Export(cor.Text, astFile)
	iltErr = translationUnit.Sections.Export(cor.Text, iltFile)
	ertErr = translationUnit.ErrorHandler.Export(cor.Text, ertFile)

	// export all intermediate representations as Binary
	tsbErr = translationUnit.TokenStream.Export(cor.Binary, tsbFile)
	ilbErr = translationUnit.Sections.Export(cor.Binary, ilbFile)

	// check if any error occurred during export of intermediate representations
	anyError = errors.Join(tsjErr, iljErr, erjErr, tstErr, astErr, iltErr, ertErr, tsbErr, ilbErr)
	return anyError
}

// Run IL/0 target with the emulator.
func EmulateTarget(target string) error {
	if raw, err := os.ReadFile(target); err != nil {
		return err
	} else if err := emu.Run(raw); err != nil {
		return err
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

// Compile PL/0 UTF-8 encoded content and return translation unit with all intermediate results and error handler.
func CompileContent(content []byte) TranslationUnit {
	// lexical analysis of PL/0 content
	tokenStream, scannerError := scn.NewScanner().Scan(content)
	errorHandler := cor.NewErrorHandler(tokenStream)
	errorHandler.AppendError(scannerError) // nil errors are ignored

	// syntax analysis and semantic analysis of PL/0 token stream
	abstractSyntax, tokenHandler := par.NewParser(tokenStream, errorHandler).Parse()
	ana.NewNameAnalysis(abstractSyntax, errorHandler, tokenHandler).Analyze()

	// return if any fatal or error errors occurred during lexical, syntax, or semantic analysis
	if errorHandler.Count(cor.Fatal|cor.Error, cor.AllComponents) > 0 {
		return TranslationUnit{content, tokenStream, nil, nil, errorHandler}
	}

	// code generation and emission of IL/0 intermediate language code based on abstract syntax tree
	sections := gen.NewGenerator(abstractSyntax).Generate().GetSections()

	// return if any fatal or error errors occurred during code generation and emission
	if errorHandler.Count(cor.Fatal|cor.Error, cor.AllComponents) > 0 {
		return TranslationUnit{content, tokenStream, abstractSyntax, nil, errorHandler}
	}

	// return translation unit with all intermediate results and error handler
	return TranslationUnit{content, tokenStream, abstractSyntax, sections, errorHandler}
}
