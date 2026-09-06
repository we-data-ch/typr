# Changelog

All notable changes to the "typR Language Support" extension will be documented in this file.

## [0.1.0] - 2024

### Added

- Initial release
- Syntax highlighting for `.ty` files
  - Keywords, types, operators, comments
  - String literals, numbers, booleans
  - Generic types and type parameters
  - Variant constructors
- Language Server Protocol (LSP) support
  - Hover information
  - Go to definition (same file)
  - Autocompletion for types and functions
- Commands
  - `typR: Check Project` - Run type checking
  - `typR: Build Project` - Build the project
  - `typR: Run Project` - Run the project
  - `typR: Check Current File` - Check current file
  - `typR: Run Current File` - Run current file
  - `typR: Restart Language Server` - Restart LSP
- Keyboard shortcuts
  - `Ctrl+Shift+C` - Check project
  - `Ctrl+Shift+B` - Build project
  - `F5` - Run project
- Configuration options
  - `typr.path` - Path to typr binary
  - `typr.enableLsp` - Enable/disable LSP
  - `typr.trace.server` - LSP trace level
