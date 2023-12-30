package compiler

import (
	"fmt"
	"io"
	"net/http"
	"os"

	"github.com/petersen65/PL0/scanner"
)

func CompileContent(content []byte) error {
	scanner := scanner.NewScanner()
	scanner.ResetSource(content)
	fmt.Println("Compiling source content with length:", len(content))
	runScanner(scanner)

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

func runScanner(scanner scanner.Scanner) {
	for {
		token, err := scanner.GetTokenName()

		if err != nil {
			fmt.Println(err)
			return
		}

		switch token {
		case "identifier":
			fmt.Println(token, scanner.GetTokenValue().(string))

		case "number":
			fmt.Println(token, scanner.GetTokenValue().(int64))

		case "eof":
			fmt.Println("Compilation successful")
			return

		default:
			fmt.Println(token)
		}
	}
}
