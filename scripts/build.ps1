Set-Variable -Name 'LinkCommand' -Value "-X main.CommitHash=$(git rev-parse --short HEAD)"
go build -v -a -o bin/pl0.exe -ldflags $LinkCommand
