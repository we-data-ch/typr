# typR Language Support

VS Code extension providing language support for **typR** - a typed version of R with TypeScript-like typing and Rust-like syntax.

## Features

### Syntax Highlighting

Full syntax highlighting for `.ty` files including:
- Keywords (`fn`, `let`, `type`, `match`, `if`, `else`, etc.)
- Types (`int`, `num`, `char`, `bool`, `Option`, `Vec`, etc.)
- Operators (`->`, `=>`, `<-`, `|`, `@`, etc.)
- Comments (`#`)
- Strings, numbers, and booleans
- Generic types and type parameters
- Variant constructors (`.Some`, `.None`, etc.)

### Language Server Protocol (LSP)

When the `typr` binary is available, the extension provides:
- **Hover**: Display type information on hover
- **Go to Definition**: Navigate to definitions (within the same file)
- **Autocompletion**: Suggestions for types and functions

### Commands

| Command | Description | Shortcut |
|---------|-------------|----------|
| `typR: Check Project` | Run type checking on the project | `Ctrl+Shift+C` |
| `typR: Build Project` | Build the project | `Ctrl+Shift+B` |
| `typR: Run Project` | Run the project | `F5` |
| `typR: Check Current File` | Run type checking on current file | - |
| `typR: Run Current File` | Run the current file | - |
| `typR: Restart Language Server` | Restart the LSP server | - |

## Requirements

- **typr binary**: The `typr` transpiler must be installed and accessible in your PATH.

## Extension Settings

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| `typr.path` | string | `"typr"` | Path to the typr binary |
| `typr.enableLsp` | boolean | `true` | Enable/disable the language server |
| `typr.trace.server` | string | `"off"` | LSP trace level (`off`, `messages`, `verbose`) |

## Installation

### From VSIX file (Local)

1. Download the `.vsix` file
2. In VS Code, open Command Palette (`Ctrl+Shift+P`)
3. Run `Extensions: Install from VSIX...`
4. Select the downloaded `.vsix` file

### From Source

```bash
# Clone the repository
git clone <repository-url>
cd vscode_extension

# Install dependencies
npm install

# Build the extension
npm run compile

# Package as VSIX
npm run package
```

## typR Language Overview

typR is a typed version of R with:

- **TypeScript-like type annotations**: `let x: int <- 5;`
- **Rust-like syntax**: `fn`, `let`, `match`, `type`
- **Generic types**: `Option<T>`, `Vec<T>`, `[#N, T]`
- **Pattern matching**: `match value { Some(v) => ..., None => ... }`
- **Type aliases and unions**: `type Option<T> = .Some(T) | .None;`

### Example

```typr
# Define an Option type
type Option<T> = .Some(T) | .None;

# Function to unwrap an option
let unwrap <- fn(value: Option<T>): T {
    match value {
        Some(v) => v,
        None => stop("The value is not unwrappable.")
    }
};

# External function declaration
@print: (Any) -> Empty;

# Main code
let x: Option<int> <- .Some(42);
print(unwrap(x));
```

## Troubleshooting

### Language Server not starting

1. Verify `typr` is in your PATH: run `typr --version` in terminal
2. Check the Output panel (`typR` channel) for error messages
3. Try setting `typr.path` to the full path of the `typr` binary

### Syntax highlighting not working

1. Ensure your file has the `.ty` extension
2. Check that the language mode is set to "typR" (bottom-right of VS Code)

## Contributing

Contributions are welcome! Please open an issue or pull request on the repository.

## License

MIT
