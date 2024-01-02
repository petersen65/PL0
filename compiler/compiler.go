package compiler

import (
	"fmt"
	"io"
	"net/http"
	"os"
	"strings"

	"github.com/petersen65/PL0/emitter"
	"github.com/petersen65/PL0/parser"
	"github.com/petersen65/PL0/scanner"
)

func CompileContent(content []byte) error {
	fmt.Println("Compiling source content with length:", len(content), "bytes")

	scanner := scanner.NewScanner()
	concreteSyntax, err := scanner.Scan(content)
	PrintConcreteSyntax(concreteSyntax, err)

	emitter := emitter.NewEmitter()
	parser := parser.NewParser()
	errorReport, err := parser.Parse(concreteSyntax, emitter)
	PrintErrorReport(errorReport, err)

	return nil
}

func CompileFile(path string) error {
	if content, err := os.ReadFile(path); err != nil {
		return err
	} else {
		return CompileContent(content)
	}
}

func CompileReader(reader io.Reader) error {
	if content, err := io.ReadAll(reader); err != nil {
		return err
	} else {
		return CompileContent(content)
	}
}

func CompileHttp(url string) error {
	if response, err := http.Get(url); err != nil {
		return err
	} else {
		defer response.Body.Close()
		return CompileReader(response.Body)
	}
}

func PrintConcreteSyntax(concreteSyntax scanner.ConcreteSyntax, err error) {
	var lastLine int

	for _, td := range concreteSyntax {
		if td.Line != lastLine {
			fmt.Printf("\n%v: %v\n", td.Line, strings.TrimSpace(string(td.CurrentLine)))
			lastLine = td.Line
		}
		
		fmt.Printf("%v,%v\t%v %v\n", td.Line, td.Column, td.TokenName, td.TokenValue)
	}

	if err != nil {
		fmt.Printf("\n%v", err)
	}
}

func PrintErrorReport(errorReport parser.ErrorReport, err error) {
	if err != nil {
		fmt.Printf("\n%v", err)
	}

	for _, e := range errorReport {
		fmt.Printf("\n%v: %v\n", e.Line, strings.TrimSpace(string(e.CurrentLine)))
		fmt.Printf("%v,%v\t%v,%v\n", e.Line, e.Column, e.Err, e.Message)
	}
}
