# SimpleC Parser – CS 341 Project 4

This F# parser validates and parses SimpleC programs, a simplified subset of C. It checks syntax and provides error feedback for educational purposes.

## Features

- **Recursive-Descent Parsing**: Implements parsing for SimpleC based on BNF grammar rules using recursive descent.
- **Error Detection**: Identifies the first syntax error, outputting descriptive messages, e.g., `"expecting X, but found Y"`.
- **Simple Syntax Support**: Parses essential constructs like:
  - **Variable Declarations**: Supports integer declarations.
  - **Input/Output Statements**: Single-value input (`cin`) and output (`cout`).
  - **Assignment Statements**: Basic assignments to identifiers.
  - **Conditional Statements**: Basic `if` statements with optional `else` support.
  - **Basic Expressions**: Supports integer, string, and boolean literals along with binary operations.

## Usage

To use the parser, pass a list of tokens representing a SimpleC program to the `parse` function. The parser returns `"Success!"` if the program is valid, or an error message if syntax violations are found.

### Example

Here’s an example of validating a SimpleC program:

```fsharp
open compiler.parser

// Sample token list for a SimpleC program
let tokens = ["void"; "main"; "("; ")"; "{"; "int"; "identifier:x"; ";"; "cout"; "<<"; "identifier:x"; ";"; "}"; "$"]

// Parse tokens
let result = parse tokens
printfn "%s" result // Should print "Success!" or an error message if invalid.
