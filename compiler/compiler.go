package compiler

import (
	"fmt"
	"io"
	"net/http"
	"os"
	"strings"

	"github.com/petersen65/PL0/scanner"
)

func CompileContent(content []byte) error {
	fmt.Println("Compiling source content with length:", len(content), "bytes")

	scanner := scanner.NewScanner()
	scanner.ResetSource(content)
	report, err := scanner.Scan()
	PrintScannerReport(report, err)

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

func PrintScannerReport(report scanner.Report, err error) {
	var lastLine int

	for _, d := range report {
		if d.Line != lastLine {
			fmt.Printf("\n%v: %v\n", d.Line, strings.TrimSpace(string(d.CurrentLine)))
			lastLine = d.Line
		}
		
		fmt.Printf("%v,%v\t%v %v\n", d.Line, d.Column, d.TokenName, d.TokenValue)
	}

	if err != nil {
		fmt.Printf("\n%v", err)
	}
}
