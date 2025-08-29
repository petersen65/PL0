// Copyright 2024-2025 Michael Petersen. All rights reserved.
// Use of this source code is governed by an Apache license that can be found in the LICENSE file.

// Package compiler provides functions that compile source code into target language codes.
package compiler

import (
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	ana "github.com/petersen65/pl0/v3/analyzer"
	ast "github.com/petersen65/pl0/v3/ast"
	cfg "github.com/petersen65/pl0/v3/cfg"
	cor "github.com/petersen65/pl0/v3/core"
	emi "github.com/petersen65/pl0/v3/emitter"
	x64 "github.com/petersen65/pl0/v3/emitter/x86_64"
	gen "github.com/petersen65/pl0/v3/generator"
	ic "github.com/petersen65/pl0/v3/generator/intermediate"
	par "github.com/petersen65/pl0/v3/parser"
	scn "github.com/petersen65/pl0/v3/scanner"
)

// Default target filename for the compilation driver.
const defaultTarget = "out"

// Default build directory for all generated files.
const defaultBuildDirectory = "build"

// Directory for exported intermediate representations.
const intermediateDirectory = "tmp"

// The standard library source file contains all standard library routines for PL/0.
const standardFileName = "standard"

// Text messages for the compilation driver.
const (
	textCleaning                   = "Cleaning build directory '%v'\n"
	textCompiling                  = "Compiling source '%v' to target '%v' for platform '%v'\n"
	textOptimizing                 = "Applying optimization algorithms '%v'\n"
	textErrorCompiling             = "Error compiling source '%v': %v"
	textAbortCompilation           = "compilation aborted\n"
	textErrorReading               = "Error reading source file '%v': %v"
	textErrorWriting               = "Error writing target file '%v': %v"
	textExporting                  = "Exporting intermediate representations to '%v'\n"
	textErrorExporting             = "Error exporting intermediate representations '%v': %v"
	textLinking                    = "Linking output '%v' from object files '%v'\n"
	textErrorLinking               = "Error linking output '%v': %v"
	textDriverSourceTarget         = "Compiler Driver with source '%v' and target '%v' completed\n"
	textDriverSourceTargetOutput   = "Compiler Driver with source '%v', target '%v', and output '%v' completed\n"
	textErrorTargetPlatform        = "target platform '%v' not supported"
	textErrorGnuCompilerCollection = "Error validating GNU Compiler Collection: %v\n"
)

// Options for the compilation driver as bit-mask.
const (
	Clean DriverOption = 1 << iota
	Compile
	Link
	Optimize
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
	LanguageC
	Assembly
	Object
	Runtime
	Json
	Text
	Ensure
)

// Command lines and flags for the external C compiler, assembler and linker.
const (
	// Flags for assembling and linking the output executable with debugging information.
	DebugFlagsC         = "-std=c23 -m64 -ggdb -g3 -O0 -DDEBUG -fno-omit-frame-pointer -fverbose-asm -dA"
	DebugFlagsAssembler = "-m64 -ggdb"
	DebugFlagsLinker    = "-m64 -ggdb -fno-omit-frame-pointer"

	// Flags for assembling and linking the output executable with optimization algorithms.
	ReleaseFlagsC         = "-std=c23 -m64 -O2 -DNDEBUG"
	ReleaseFlagsAssembler = "-m64"
	ReleaseFlagsLinker    = "-m64 -s"

	// Validate required GNU Compiler Collection.
	CommandValidateGnuCompilerCollection = "gcc-15 -dumpmachine"

	// Compile, assemble, and link the standard library, runtime, target, and output executable.
	CommandAssembleStandardLibrary = "gcc-15 -no-pie %v -S -o %v %v"
	CommandCompileStandardLibrary  = "gcc-15 -no-pie %v -c -o %v %v"
	CommandAssembleRuntime         = "gcc-15 -no-pie %v -c -o %v %v"
	CommandAssembleTarget          = "gcc-15 -no-pie %v -c -o %v %v"
	CommandLinkOutputExecutable    = "gcc-15 -no-pie %v -o %v %v %v %v"
)

type (
	// Driver options for the compilation process (bit-mask).
	DriverOption uint64

	// File extensions for files used during compilation.
	Extension int

	// CompilationUnit represents source content and all intermediate results of the compilation process.
	CompilationUnit struct {
		Name             string                  // unique name of the compilation unit
		SourceContent    []byte                  // UTF-8 source content
		ErrorHandler     cor.ErrorHandler        // error handler of the compilation process
		TokenHandler     cor.TokenHandler        // token handler for the source content
		TokenStream      cor.TokenStream         // token stream of the source content
		AbstractSyntax   ast.Block               // abstract syntax tree of the token stream
		IntermediateCode ic.IntermediateCodeUnit // intermediate code unit of the abstract syntax tree
		ControlFlow      cfg.ControlFlowGraph    // control flow graph of the intermediate code unit
		AssemblyCode     x64.AssemblyCodeUnit    // assembly code of the intermediate code unit
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
	LanguageC: ".c",
	Json:      ".json",
	Text:      ".txt",
	Assembly:  ".s",
	Object:    ".o",
	Runtime:   ".rt",
	Ensure:    ".ensure",
}

// Driver for the compilation process with options, source path, target path, optimization algorithms, display name, and print writer.
func Driver(options DriverOption, sourcePath, targetPath string, optimization cor.Optimization, displayName string, print io.Writer) {
	var buildDirectory, buildAbsoluteDirectory, baseFileName, sourceAbsolutePath, targetAbsolutePath, runtimePath string
	var compilationUnit CompilationUnit
	var err error

	// ensure source path exists and print error message if an error occurred
	if sourcePath, sourceAbsolutePath, err = EnsureSourcePath(sourcePath); err != nil {
		fmt.Fprintf(print, textErrorReading, sourcePath, err)
		return
	}

	// ensure target path exists and print error message if an error occurred
	// note: the target path is used to derive the build directory from it
	if baseFileName, buildDirectory, buildAbsoluteDirectory, err = EnsureTargetPath(targetPath); err != nil {
		fmt.Fprintf(print, textErrorWriting, targetPath, err)
		return
	}

	// cleaned and validated target and runtime paths
	targetPath = GetFullPath(buildDirectory, baseFileName, Assembly)
	targetAbsolutePath = GetFullPath(buildAbsoluteDirectory, baseFileName, Assembly)
	runtimePath = GetFullPath(buildDirectory, baseFileName, Runtime, Assembly)

	// clean build directory and assume that the first ensuring of the target path was successful
	if options&Clean != 0 {
		fmt.Fprintf(print, textCleaning, buildDirectory)
		os.RemoveAll(buildDirectory)

		// repeat ensuring existance of target path only if compile option is set
		if options&Compile != 0 {
			// repeat ensuring existance of target path after cleaning and print persistence error message if an error occurred this time
			// note: the target path is used to derive the build directory from it
			if baseFileName, buildDirectory, buildAbsoluteDirectory, err = EnsureTargetPath(targetPath); err != nil {
				fmt.Fprintf(print, textErrorWriting, targetPath, err)
				return
			}

			// cleaned and validated target and runtime paths after cleaning
			targetPath = GetFullPath(buildDirectory, baseFileName, Assembly)
			targetAbsolutePath = GetFullPath(buildAbsoluteDirectory, baseFileName, Assembly)
			runtimePath = GetFullPath(buildDirectory, baseFileName, Runtime, Assembly)
		}
	}

	// define target platform for the executable
	// note: only Linux with x86_64 CPU, SSE2 instruction set, UTF-32 string encoding, and SystemV_AMD64 application binary interface is supported for now
	targetPlatform := cor.TargetPlatform{
		OperatingSystem:            cor.Linux,
		InstructionSetArchitecture: cor.X86_64,
		InstructionSet:             cor.ISA_SSE2,
		StringEncoding:             cor.UTF32,
		ApplicationBinaryInterface: cor.ABI_SystemV_AMD64,
	}

	// setup build configuration for the executable
	// note: paths include the source file name and the target file name
	buildConfiguration := cor.BuildConfiguration{
		SourcePath:         sourcePath,
		TargetPath:         targetPath,
		SourceAbsolutePath: sourceAbsolutePath,
		TargetAbsolutePath: targetAbsolutePath,
		TargetPlatform:     targetPlatform,
		DriverDisplayName:  displayName,
		OutputKind:         cor.Application,
		Optimization:       optimization,
	}

	// setup link configuration for the executable
	linkConfiguration := cor.LinkConfiguration{
		StandardSource:   GetFullPath(buildDirectory, standardFileName, LanguageC),
		StandardAssembly: GetFullPath(buildDirectory, standardFileName, Assembly),
		StandardObject:   GetFullPath(buildDirectory, standardFileName, Object),
		RuntimeAssembly:  GetFullPath(buildDirectory, baseFileName, Runtime, Assembly),
		RuntimeObject:    GetFullPath(buildDirectory, baseFileName, Runtime, Object),
		TargetAssembly:   GetFullPath(buildDirectory, baseFileName, Assembly),
		TargetObject:     GetFullPath(buildDirectory, baseFileName, Object),
		OutputExecutable: GetFullPath(buildDirectory, baseFileName),
	}

	// validate that the GNU Compiler Collection supports the target platform and print error message if an error occurred
	if options&Link != 0 {
		if err := ValidateGnuCompilerCollection(targetPlatform); err != nil {
			fmt.Fprintf(print, textErrorGnuCompilerCollection, err)
			return
		}
	}

	// compile source code to compilation unit and print an error report if errors occurred during compilation
	if options&Compile != 0 {
		fmt.Fprintf(print, textCompiling, sourcePath, targetPath, targetPlatform)

		// print optimization message if optimization algorithms are applied
		if options&Optimize != 0 {
			fmt.Fprintf(print, textOptimizing, optimization)
		}

		// compile source code to compilation unit and return I/O errors
		compilationUnit, err = CompileSourceToCompilationUnit(buildConfiguration)

		// print error message if an I/O error occurred during compilation
		if err != nil {
			fmt.Fprintf(print, textErrorCompiling, sourcePath, err)
			return
		}

		// print error report if errors with any severity occurred
		compilationUnit.ErrorHandler.Print(print)

		// print abandon compilation message if any error occurred
		if compilationUnit.ErrorHandler.HasErrors() {
			fmt.Fprintf(print, textErrorCompiling, sourcePath, textAbortCompilation)
		}
	}

	// export intermediate representations to the build directory even if errors occurred during compilation
	if options&Compile != 0 && options&Export != 0 {
		fmt.Fprintf(print, textExporting, filepath.Join(buildDirectory, intermediateDirectory))

		// print error message if any error occurred during export
		if err = ExportIntermediateRepresentations(compilationUnit, buildDirectory, baseFileName); err != nil {
			fmt.Fprintf(print, textErrorExporting, filepath.Join(buildDirectory, intermediateDirectory), err)
			return
		}
	}

	// persist application and runtime as target files and print persistence error message if an error occurred
	if options&Compile != 0 && !compilationUnit.ErrorHandler.HasErrors() {
		if err = PersistApplication(compilationUnit.AssemblyCode, targetPath); err != nil {
			fmt.Fprintf(print, textErrorWriting, targetPath, err)
			return
		}

		if err = PersistRuntime(targetPlatform, optimization, runtimePath, displayName); err != nil {
			fmt.Fprintf(print, textErrorWriting, runtimePath, err)
			return
		}
	}

	// link all compilation units and print linking error message if an error occurred
	if options&Link != 0 && options&Compile != 0 && !compilationUnit.ErrorHandler.HasErrors() {
		fmt.Fprintf(print,
			textLinking,
			linkConfiguration.OutputExecutable,
			strings.Join([]string{linkConfiguration.StandardObject, linkConfiguration.RuntimeObject, linkConfiguration.TargetObject}, ", "),
		)

		if err = LinkCompilationUnits(optimization&cor.Debug != 0, linkConfiguration); err != nil {
			fmt.Fprintf(print, textErrorLinking, linkConfiguration.OutputExecutable, err)
			return
		}
	}

	// print driver completion message for link and compilation cases
	if options&Link != 0 {
		fmt.Fprintf(print, textDriverSourceTargetOutput, sourcePath, targetPath, linkConfiguration.OutputExecutable)
	} else if options&Compile != 0 {
		fmt.Fprintf(print, textDriverSourceTarget, sourcePath, targetPath)
	}
}

// Compile source code and return compilation unit with all intermediate results and error handler.
func CompileSourceToCompilationUnit(buildConfiguration cor.BuildConfiguration) (CompilationUnit, error) {
	if content, err := os.ReadFile(buildConfiguration.SourcePath); err != nil {
		return CompilationUnit{}, err
	} else {
		return CompileContent(content, buildConfiguration), nil
	}
}

// Compile UTF-8 encoded content and return a compilation unit with all intermediate results and error handler.
func CompileContent(content []byte, buildConfiguration cor.BuildConfiguration) CompilationUnit {
	// lexical analysis of content
	tokenStream, scannerError := scn.NewScanner().Scan(content)
	errorHandler := cor.NewErrorHandler()
	errorHandler.AppendError(scannerError) // nil errors are ignored

	// syntax analysis and semantic analysis of token stream
	abstractSyntax, tokenHandler := par.NewParser(tokenStream, errorHandler).Parse()
	ana.NewAnalyzer(abstractSyntax, errorHandler, tokenHandler).Analyze()

	// return if any fatal or error errors occurred during lexical, syntax, or semantic analysis
	if errorHandler.Count(cor.Fatal|cor.Error, cor.AllComponents) > 0 {
		return CompilationUnit{
			Name:             buildConfiguration.SourcePath,
			SourceContent:    content,
			ErrorHandler:     errorHandler,
			TokenHandler:     tokenHandler,
			TokenStream:      tokenStream,
			AbstractSyntax:   nil,
			IntermediateCode: nil,
			ControlFlow:      nil,
			AssemblyCode:     nil,
		}
	}

	// code generation based on the abstract syntax tree results in an intermediate code unit and debug information
	generator := gen.NewGenerator(abstractSyntax, buildConfiguration, tokenHandler)
	generator.Generate()
	intermediateCode := generator.GetIntermediateCodeUnit()
	debugInformation := generator.GetDebugInformation()

	// build control flow graph from an intermediate code unit
	controlFlow := cfg.NewControlFlowGraph(intermediateCode)
	controlFlow.Build()

	// emit assembly code for a target platform from the intermediate code unit
	emitter := emi.NewEmitter(intermediateCode, buildConfiguration, debugInformation)
	emitter.Emit()
	assemblyCode := emitter.GetAssemblyCodeUnit()

	// return compilation unit with all intermediate results and error handler
	return CompilationUnit{
		Name:             buildConfiguration.SourcePath,
		SourceContent:    content,
		ErrorHandler:     errorHandler,
		TokenHandler:     tokenHandler,
		TokenStream:      tokenStream,
		AbstractSyntax:   abstractSyntax,
		IntermediateCode: intermediateCode,
		ControlFlow:      controlFlow,
		AssemblyCode:     assemblyCode,
	}
}

// Persist the assembly code unit of the application.
func PersistApplication(unit x64.AssemblyCodeUnit, targetPath string) error {
	return PersistAssemblyCodeUnit(unit, targetPath)
}

// Persist the assembly code unit of the runtime.
func PersistRuntime(targetPlatform cor.TargetPlatform, optimization cor.Optimization, runtimePath string, displayName string) error {
	// setup build configuration for the runtime which does not support debug information
	buildConfiguration := cor.BuildConfiguration{
		SourcePath:        runtimePath,
		TargetPath:        runtimePath,
		TargetPlatform:    targetPlatform,
		DriverDisplayName: displayName,
		OutputKind:        cor.Runtime,
		Optimization:      optimization &^ cor.Debug,
	}

	// create a new assembly code unit for the runtime without support for source code file to assembly code mapping
	unit := x64.NewAssemblyCodeUnit(buildConfiguration)
	unit.AppendRuntime()
	return PersistAssemblyCodeUnit(unit, runtimePath)
}

// Persist an assembly code unit to the given target file.
func PersistAssemblyCodeUnit(unit x64.AssemblyCodeUnit, targetPath string) error {
	var err error
	var output *os.File

	if output, err = os.Create(targetPath); err != nil {
		return err
	} else {
		// safely close file and remove it if there is an error
		defer func() {
			output.Close()

			if err != nil {
				os.Remove(targetPath)
			}
		}()

		// export the assembly code unit to the assembly target
		if err = unit.Export(cor.Text, output); err != nil {
			return err
		}
	}

	return nil
}

// Validate that the GNU Compiler Collection supports the target platform.
func ValidateGnuCompilerCollection(targetPlatform cor.TargetPlatform) error {
	parts := strings.Fields(CommandValidateGnuCompilerCollection)
	cmd := exec.Command(parts[0], parts[1:]...)

	if output, err := cmd.CombinedOutput(); err != nil {
		return err
	} else {
		result := strings.ToLower(string(output))

		if !strings.Contains(result, strings.ToLower(targetPlatform.OperatingSystem.String())) ||
			!strings.Contains(result, strings.ToLower(targetPlatform.InstructionSetArchitecture.String())) {
			return fmt.Errorf(textErrorTargetPlatform, targetPlatform)
		}

		return nil
	}
}

// Assemble and link the compilation units of the standard library, runtime, and target.
func LinkCompilationUnits(debug bool, linkConfiguration cor.LinkConfiguration) error {
	var flagsC, flagsAssembler, flagsLinker string

	if debug {
		// assemble and link with debug information
		flagsC = DebugFlagsC
		flagsAssembler = DebugFlagsAssembler
		flagsLinker = DebugFlagsLinker
	} else {
		// assemble and link with release optimizations
		flagsC = ReleaseFlagsC
		flagsAssembler = ReleaseFlagsAssembler
		flagsLinker = ReleaseFlagsLinker
	}

	// extract the embedded standard library source file
	if err := ExtractStandardLibrary(linkConfiguration.StandardSource); err != nil {
		return err
	}

	// assemble and link the output executable based on its compilation units
	// note: the standard library assembly file is only created for diagnostic purposes
	assembleAndLink := []string{
		fmt.Sprintf(
			CommandAssembleStandardLibrary,     // compile standard library to assembly file
			flagsC,                             // flags for C compiler
			linkConfiguration.StandardAssembly, // output assembly file
			linkConfiguration.StandardSource),  // input source file
		fmt.Sprintf(
			CommandCompileStandardLibrary,     // compile standard library to object file
			flagsC,                            // flags for C compiler
			linkConfiguration.StandardObject,  // output object file
			linkConfiguration.StandardSource), // input source file
		fmt.Sprintf(
			CommandAssembleRuntime,             // assemble runtime to object file
			flagsAssembler,                     // flags for assembler
			linkConfiguration.RuntimeObject,    // output object file
			linkConfiguration.RuntimeAssembly), // input assembly file
		fmt.Sprintf(
			CommandAssembleTarget,             // assemble target to object file
			flagsAssembler,                    // flags for assembler
			linkConfiguration.TargetObject,    // output object file
			linkConfiguration.TargetAssembly), // input assembly file
		fmt.Sprintf(
			CommandLinkOutputExecutable,        // link output executable file
			flagsLinker,                        // flags for linker
			linkConfiguration.OutputExecutable, // output executable file
			linkConfiguration.TargetObject,     // input object file of target
			linkConfiguration.RuntimeObject,    // input object file of runtime
			linkConfiguration.StandardObject),  // input object file of standard library
	}

	// execute all assemble and link commands in sequence and return if an error occurred
	for _, command := range assembleAndLink {
		parts := strings.Fields(command)
		cmd := exec.Command(parts[0], parts[1:]...)

		if err := cmd.Run(); err != nil {
			return err
		}
	}

	return nil
}

// Export all intermediate representations to the build directory.
func ExportIntermediateRepresentations(compilationUnit CompilationUnit, targetDirectory, baseFileName string) error {
	var anyError error

	// create full intermediate directory path
	targetDirectory = filepath.Join(targetDirectory, intermediateDirectory)

	// create full directory path and check if an ensure-test file can be created by creating it and removing it afterwards
	if _, _, _, err := EnsureTargetPath(targetDirectory); err != nil {
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

	// close all files and remove build directory if any error occurred during file creations
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

		// remove build directory if any error occurred during file creations
		if anyError != nil {
			os.RemoveAll(targetDirectory)
		}
	}()

	if anyError != nil {
		return anyError
	}

	// export all error handler representations to the target files
	if compilationUnit.ErrorHandler != nil {
		erjErr = compilationUnit.ErrorHandler.Export(cor.Json, erjFile)
		ertErr = compilationUnit.ErrorHandler.Export(cor.Text, ertFile)
	}

	// export all token stream representations to the target files
	if compilationUnit.TokenStream != nil {
		tsjErr = compilationUnit.TokenStream.Export(cor.Json, tsjFile)
		tstErr = compilationUnit.TokenStream.Export(cor.Text, tstFile)
	}

	// export all abstract syntax tree representations to the target files
	if compilationUnit.AbstractSyntax != nil {
		asjErr = compilationUnit.AbstractSyntax.Export(cor.Json, asjFile)
		astErr = compilationUnit.AbstractSyntax.Export(cor.Text, astFile)
	}

	// export all intermediate code representations to the target files
	if compilationUnit.IntermediateCode != nil {
		icjErr = compilationUnit.IntermediateCode.Export(cor.Json, icjFile)
		ictErr = compilationUnit.IntermediateCode.Export(cor.Text, ictFile)
	}

	// export all control flow graph representations to the target files
	if compilationUnit.ControlFlow != nil {
		cfjErr = compilationUnit.ControlFlow.Export(cor.Json, cfjFile)
		cftErr = compilationUnit.ControlFlow.Export(cor.Text, cftFile)
	}

	// export all assembly code representations to the target files
	if compilationUnit.AssemblyCode != nil {
		acjErr = compilationUnit.AssemblyCode.Export(cor.Json, acjFile)
		actErr = compilationUnit.AssemblyCode.Export(cor.Text, actFile)
	}

	// check if any error occurred during export of intermediate representations
	anyError = errors.
		Join(erjErr, tsjErr, asjErr, icjErr, cfjErr, acjErr, ertErr, tstErr, astErr, ictErr, cftErr, actErr)

	return anyError
}

// Ensure source path exists and open it to proof read access.
func EnsureSourcePath(source string) (string, string, error) {
	var sourceAbsolutePath string
	source = filepath.Clean(source)

	// return if source path does not exist
	if _, err := os.Stat(source); os.IsNotExist(err) {
		return "", "", err
	}

	// absolute full file path of source based on current working directory
	if absolutePath, err := filepath.Abs(source); err != nil {
		return "", "", err
	} else {
		sourceAbsolutePath = absolutePath
	}

	// open source file to proof read access
	if sourceFile, err := os.Open(source); err != nil {
		return "", "", err
	} else {
		defer sourceFile.Close()
	}

	return filepath.ToSlash(source), filepath.ToSlash(sourceAbsolutePath), nil
}

// Ensure target path exists and create and remove empty ensure-file to proof write access.
func EnsureTargetPath(target string) (string, string, string, error) {
	var targetDirectory, targetAbsoluteDirectory string
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
		return "", "", "", err
	}

	// return if target directory does not exist after trying to create it
	if _, err := os.Stat(targetDirectory); os.IsNotExist(err) {
		return "", "", "", err
	}

	// get base file name of target path without extension
	baseFileName := filepath.Base(strings.TrimSuffix(target, filepath.Ext(target)))

	// enforce default target file name if base file name is empty, current directory, or root directory
	if baseFileName == "." || baseFileName == "" || baseFileName == string(os.PathSeparator) || baseFileName == target {
		baseFileName = defaultTarget
	}

	// full file path of ensure file with extension
	ensureFullPath := filepath.Join(targetDirectory, baseFileName) + ExtensionMap[Ensure]

	// create and remove empty ensure file to proof write access
	if ensureFile, err := os.Create(ensureFullPath); err != nil {
		return "", "", "", err
	} else {
		defer func() {
			ensureFile.Close()
			os.Remove(ensureFullPath)
		}()
	}

	// absolute full directory path of target based on current working directory
	if absolutePath, err := filepath.Abs(targetDirectory); err != nil {
		return "", "", "", err
	} else {
		targetAbsoluteDirectory = absolutePath
	}

	return baseFileName, filepath.ToSlash(targetDirectory), filepath.ToSlash(targetAbsoluteDirectory), nil
}

// Return the full path of the target file with the given base file name and extensions.
func GetFullPath(targetDirectory, baseFileName string, extensions ...Extension) string {
	targetFullPath := filepath.Join(targetDirectory, baseFileName)

	for _, extension := range extensions {
		targetFullPath += ExtensionMap[extension]
	}

	return filepath.ToSlash(targetFullPath)
}
