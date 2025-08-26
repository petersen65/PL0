#!/bin/bash
# Copyright 2024-2025 Michael Petersen. All rights reserved.
# Use of this source code is governed by an Apache license that can be found in the LICENSE file.

# install PL/0 language extension for VS Code
echo "Installing or updating PL/0 language extension"

# detect the correct VS Code extensions directory
if [ -d "$HOME/.vscode-server/extensions" ]; then
    EXTENSION_BASE="$HOME/.vscode-server/extensions"
elif [ -d "$HOME/.vscode-remote/extensions" ]; then
    EXTENSION_BASE="$HOME/.vscode-remote/extensions"
elif [ -d "$HOME/.vscode-server-insiders/extensions" ]; then
    EXTENSION_BASE="$HOME/.vscode-server-insiders/extensions"
else
    echo "Error: Could not find VS Code extensions directory"
    echo "Searched in:"
    echo "  - $HOME/.vscode-server/extensions"
    echo "  - $HOME/.vscode-remote/extensions"
    echo "  - $HOME/.vscode-server-insiders/extensions"
    exit 1
fi

EXTENSION_DIR="$EXTENSION_BASE/pl0-language"
echo "Found VS Code extensions directory: $EXTENSION_BASE"

# remove old version if exists
if [ -d "$EXTENSION_DIR" ]; then
    echo "Removing existing PL/0 extension"
    rm -rf "$EXTENSION_DIR"
    echo "✗ PL/0 extension removed from: $EXTENSION_DIR"
fi

# create extension directory for the PL/0 language
mkdir --parents "$EXTENSION_DIR"

# copy extension to VS Code extensions directory
echo "Installing PL/0 extension"
cp --recursive .vscode/extensions/pl0-language/* "$EXTENSION_DIR"

# verify installation
if [ -d "$EXTENSION_DIR" ]; then
    echo "✓ PL/0 extension installed successfully to: $EXTENSION_DIR"
    echo ""
    echo "Files installed:"
    find "$EXTENSION_DIR" -type f -name "*.*" | sort
    echo ""
    echo "Please reload VS Code window: press Ctrl+Shift+P (or Cmd+Shift+P) and run 'Developer: Reload Window'"
else
    echo "Error: Installation failed"
    exit 1
fi