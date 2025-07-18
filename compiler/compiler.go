// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package compiler provides functions that compile PL/0 source code into target language codes.
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
	cfg "github.com/petersen65/PL0/v2/cfg"
	cor "github.com/petersen65/PL0/v2/core"
	emi "github.com/petersen65/PL0/v2/emitter"
	amd "github.com/petersen65/PL0/v2/emitter/amd64"
	gen "github.com/petersen65/PL0/v2/generator"
	ic "github.com/petersen65/PL0/v2/generator/intermediate"
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
	textCompiling          = "Compiling source '%v' to target '%v' for platform '%v'\n"
	textErrorCompiling     = "Error compiling source '%v': %v"
	textAbortCompilation   = "compilation aborted\n"
	textErrorPersisting    = "Error persisting target file '%v': %v"
	textExporting          = "Exporting intermediate representations to '%v'\n"
	textErrorExporting     = "Error exporting intermediate representations '%v': %v"
	textDriverSourceTarget = "Compiler Driver with source '%v' and target '%v' completed\n"
)

// Options for the compilation driver as bit-mask.
const (
	Clean DriverOption = 1 << iota
	Compile
	Export
)

// File extensions for all generated files.
const (
	_ Extension = iota
	Error
	Token
	Tree
	Generator
	Control
	Emitter
	Assembly
	Runtime
	Json
	Text
	Ensure
)

type (
	// Driver options for the compilation process (bit-mask).
	DriverOption uint64

	// File extensions for files used during compilation.
	Extension int

	// TranslationUnit represents source content and all intermediate results of the compilation process.
	TranslationUnit struct {
		SourceContent    []byte                  // PL/0 utf-8 source content
		ErrorHandler     cor.ErrorHandler        // error handler of the compilation process
		TokenStream      cor.TokenStream         // token stream of the PL/0 source content
		AbstractSyntax   ast.Block               // abstract syntax tree of the token stream
		IntermediateCode ic.IntermediateCodeUnit // intermediate code unit of the abstract syntax tree
		ControlFlow      cfg.ControlFlowGraph    // control flow graph of the intermediate code unit
		AssemblyCode     amd.AssemblyCodeUnit    // assembly code of the intermediate code unit
	}
)

// ExtensionMap maps file extensions to their string representation.
var ExtensionMap = map[Extension]string{
	Error:     ".err",
	Token:     ".tok",
	Tree:      ".ast",
	Generator: ".icu",
	Control:   ".cfg",
	Emitter:   ".acu",
	Json:      ".json",
	Text:      ".txt",
	Assembly:  ".s",
	Runtime:   ".rt",
	Ensure:    ".ensure",
}

// Driver for the compilation process with the given options, source path, target path, and print writer.
func Driver(options DriverOption, sourcePath, targetPath string, print io.Writer) {
	var targetPlatform emi.TargetPlatform
	var targetDirectory, baseFileName, runtime string
	var translationUnit TranslationUnit
	var err error

	// set target platform for the compilation process
	// note: only Linux with AMD64 CPU and SSE2 instruction set is supported for now
	targetPlatform = emi.TargetPlatform{
		OperatingSystem: emi.Linux,
		Cpu:             emi.Amd64,
		InstructionSet:  emi.ISA_SSE2,
	}

	// ensure target path exists and print persistence error message if an error occurred
	if targetDirectory, baseFileName, err = EnsureTargetPath(targetPath); err != nil {
		fmt.Fprintf(print, textErrorPersisting, targetPath, err)
		return
	}

	// cleaned and validated target and runtime paths
	targetPath = GetFullPath(targetDirectory, baseFileName, Assembly)
	runtime = GetFullPath(targetDirectory, baseFileName, Runtime, Assembly)

	// clean target directory and assume that the first ensuring of the target path was successful
	if options&Clean != 0 {
		fmt.Fprintf(print, textCleaning, targetDirectory)
		os.RemoveAll(targetDirectory)

		// repeat ensuring existance of target path only if compile option is set
		if options&Compile != 0 {
			// repeat ensuring existance of target path after cleaning and print persistence error message if an error occurred this time
			if targetDirectory, baseFileName, err = EnsureTargetPath(targetPath); err != nil {
				fmt.Fprintf(print, textErrorPersisting, targetPath, err)
				return
			}

			// cleaned and validated target and runtime paths after cleaning
			targetPath = GetFullPath(targetDirectory, baseFileName, Assembly)
			runtime = GetFullPath(targetDirectory, baseFileName, Runtime, Assembly)
		}
	}

	// compile source code to translation unit and print an error report if errors occurred during compilation
	if options&Compile != 0 {
		fmt.Fprintf(print, textCompiling, sourcePath, targetPath, targetPlatform)
		translationUnit, err = CompileSourceToTranslationUnit(sourcePath, targetPlatform)

		// print error message if an I/O error occurred during compilation
		if err != nil {
			fmt.Fprintf(print, textErrorCompiling, sourcePath, err)
			return
		}

		// print error report if errors with any severity occurred during compilation
		translationUnit.ErrorHandler.Print(print)

		// print abandon compilation message if any error occurred during compilation
		if translationUnit.ErrorHandler.HasErrors() {
			fmt.Fprintf(print, textErrorCompiling, sourcePath, textAbortCompilation)
		}
	}

	// export intermediate representations to the target directory even if errors occurred during compilation
	if options&Compile != 0 && options&Export != 0 {
		fmt.Fprintf(print, textExporting, filepath.Join(targetDirectory, intermediateDirectory))

		// print error message if any error occurred during export
		if err = ExportIntermediateRepresentations(translationUnit, targetDirectory, baseFileName); err != nil {
			fmt.Fprintf(print, textErrorExporting, filepath.Join(targetDirectory, intermediateDirectory), err)
			return
		}
	}

	// persist application and runtime as target files and print persistence error message if an error occurred
	if options&Compile != 0 && !translationUnit.ErrorHandler.HasErrors() {
		if err = PersistApplication(translationUnit.AssemblyCode, targetPath); err != nil {
			fmt.Fprintf(print, textErrorPersisting, targetPath, err)
			return
		}
		if err = PersistRuntime(runtime); err != nil {
			fmt.Fprintf(print, textErrorPersisting, runtime, err)
			return
		}
	}

	// print driver completion message
	fmt.Fprintf(print, textDriverSourceTarget, sourcePath, targetPath)
}

// Compile source code and return translation unit with all intermediate results and error handler.
func CompileSourceToTranslationUnit(sourcePath string, targetPlatform emi.TargetPlatform) (TranslationUnit, error) {
	if content, err := os.ReadFile(sourcePath); err != nil {
		return TranslationUnit{}, err
	} else {
		return CompileContent(content, targetPlatform), nil
	}
}

// Compile UTF-8 encoded content and return translation unit with all intermediate results and error handler.
func CompileContent(content []byte, targetPlatform emi.TargetPlatform) TranslationUnit {
	// lexical analysis of content
	tokenStream, scannerError := scn.NewScanner().Scan(content)
	errorHandler := cor.NewErrorHandler(tokenStream)
	errorHandler.AppendError(scannerError) // nil errors are ignored

	// syntax analysis and semantic analysis of token stream
	abstractSyntax, tokenHandler := par.NewParser(tokenStream, errorHandler).Parse()
	ana.NewNameAnalysis(abstractSyntax, errorHandler, tokenHandler).Analyze()

	// return if any fatal or error errors occurred during lexical, syntax, or semantic analysis
	if errorHandler.Count(cor.Fatal|cor.Error, cor.AllComponents) > 0 {
		return TranslationUnit{content, errorHandler, tokenStream, nil, nil, nil, nil}
	}

	// code generation based on the abstract syntax tree results in an intermediate code unit
	generator := gen.NewGenerator(abstractSyntax)
	generator.Generate()
	intermediateCode := generator.GetIntermediateCodeUnit()

	// build control flow graph from an intermediate code unit
	controlFlow := cfg.NewControlFlowGraph(intermediateCode)
	controlFlow.Build()

	// emit assembly code for a target platform from the intermediate code unit
	emitter := emi.NewEmitter(targetPlatform, intermediateCode)
	emitter.Emit()
	assemblyCode := emitter.GetAssemblyCodeUnit()

	// return translation unit with all intermediate results and error handler
	return TranslationUnit{content, errorHandler, tokenStream, abstractSyntax, intermediateCode, controlFlow, assemblyCode}
}

// Persist the assembly code unit of the application.
func PersistApplication(unit amd.AssemblyCodeUnit, application string) error {
	return PersistAssemblyCodeUnit(unit, application)
}

// Persist the assembly code unit of the runtime.
func PersistRuntime(runtime string) error {
	assemblyCode := amd.NewAssemblyCodeUnit(amd.Runtime)
	assemblyCode.AppendRuntime()
	return PersistAssemblyCodeUnit(assemblyCode, runtime)
}

// Persist an assembly code unit to the given target file.
func PersistAssemblyCodeUnit(unit amd.AssemblyCodeUnit, target string) error {
	var err error
	var output *os.File

	if output, err = os.Create(target); err != nil {
		return err
	} else {
		// safely close file and remove it if there is an error
		defer func() {
			output.Close()

			if err != nil {
				os.Remove(target)
			}
		}()

		// export the assembly code unit to the assembly target
		if err = unit.Export(cor.Text, output); err != nil {
			return err
		}
	}

	return nil
}

// Export all intermediate representations to the target directory.
func ExportIntermediateRepresentations(translationUnit TranslationUnit, targetDirectory, baseFileName string) error {
	var anyError error

	// create full intermediate directory path
	targetDirectory = filepath.Join(targetDirectory, intermediateDirectory)

	// create full directory path and check if an ensure-test file can be created by creating it and removing it afterwards
	if _, _, err := EnsureTargetPath(targetDirectory); err != nil {
		return err
	}

	// create all Json files for intermediate representations
	erjFile, erjErr := os.Create(GetFullPath(targetDirectory, baseFileName, Error, Json))
	tsjFile, tsjErr := os.Create(GetFullPath(targetDirectory, baseFileName, Token, Json))
	asjFile, asjErr := os.Create(GetFullPath(targetDirectory, baseFileName, Tree, Json))
	icjFile, icjErr := os.Create(GetFullPath(targetDirectory, baseFileName, Generator, Json))
	cfjFile, cfjErr := os.Create(GetFullPath(targetDirectory, baseFileName, Control, Json))
	acjFile, acjErr := os.Create(GetFullPath(targetDirectory, baseFileName, Emitter, Json))

	// create all Text files for intermediate representations
	ertFile, ertErr := os.Create(GetFullPath(targetDirectory, baseFileName, Error, Text))
	tstFile, tstErr := os.Create(GetFullPath(targetDirectory, baseFileName, Token, Text))
	astFile, astErr := os.Create(GetFullPath(targetDirectory, baseFileName, Tree, Text))
	ictFile, ictErr := os.Create(GetFullPath(targetDirectory, baseFileName, Generator, Text))
	cftFile, cftErr := os.Create(GetFullPath(targetDirectory, baseFileName, Control, Text))
	actFile, actErr := os.Create(GetFullPath(targetDirectory, baseFileName, Emitter, Text))

	// check if any error occurred during file creations
	anyError = errors.
		Join(erjErr, tsjErr, asjErr, icjErr, cfjErr, acjErr, ertErr, tstErr, astErr, ictErr, cftErr, actErr)

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
		closeFile(erjFile)
		closeFile(tsjFile)
		closeFile(asjFile)
		closeFile(icjFile)
		closeFile(cfjFile)
		closeFile(acjFile)
		closeFile(ertFile)
		closeFile(tstFile)
		closeFile(astFile)
		closeFile(ictFile)
		closeFile(cftFile)
		closeFile(actFile)

		// remove target directory if any error occurred during file creations
		if anyError != nil {
			os.RemoveAll(targetDirectory)
		}
	}()

	if anyError != nil {
		return anyError
	}

	// export all error handler representations to the target files
	if translationUnit.ErrorHandler != nil {
		erjErr = translationUnit.ErrorHandler.Export(cor.Json, erjFile)
		ertErr = translationUnit.ErrorHandler.Export(cor.Text, ertFile)
	}

	// export all token stream representations to the target files
	if translationUnit.TokenStream != nil {
		tsjErr = translationUnit.TokenStream.Export(cor.Json, tsjFile)
		tstErr = translationUnit.TokenStream.Export(cor.Text, tstFile)
	}

	// export all abstract syntax tree representations to the target files
	if translationUnit.AbstractSyntax != nil {
		asjErr = translationUnit.AbstractSyntax.Export(cor.Json, asjFile)
		astErr = translationUnit.AbstractSyntax.Export(cor.Text, astFile)
	}

	// export all intermediate code representations to the target files
	if translationUnit.IntermediateCode != nil {
		icjErr = translationUnit.IntermediateCode.Export(cor.Json, icjFile)
		ictErr = translationUnit.IntermediateCode.Export(cor.Text, ictFile)
	}

	// export all control flow graph representations to the target files
	if translationUnit.ControlFlow != nil {
		cfjErr = translationUnit.ControlFlow.Export(cor.Json, cfjFile)
		cftErr = translationUnit.ControlFlow.Export(cor.Text, cftFile)
	}

	// export all assembly code representations to the target files
	if translationUnit.AssemblyCode != nil {
		acjErr = translationUnit.AssemblyCode.Export(cor.Json, acjFile)
		actErr = translationUnit.AssemblyCode.Export(cor.Text, actFile)
	}

	// check if any error occurred during export of intermediate representations
	anyError = errors.
		Join(erjErr, tsjErr, asjErr, icjErr, cfjErr, acjErr, ertErr, tstErr, astErr, ictErr, cftErr, actErr)

	return anyError
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

// Return the full path of the target file with the given base file name and extensions.
func GetFullPath(targetDirectory, baseFileName string, extensions ...Extension) string {
	targetFullPath := filepath.Join(targetDirectory, baseFileName)

	for _, extension := range extensions {
		targetFullPath += ExtensionMap[extension]
	}

	return targetFullPath
}
