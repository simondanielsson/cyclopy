# Cyclopy

Cyclopy is a fast import-cycle checker for Python, written in OCaml. It detects circular dependencies in your Python projects, which can lead to hard-to-debug issues and unpredictable behavior.

## Features

- Fast detection of circular imports in Python projects
- Clear visualization of import cycles
- Easy to install and use (TODO)

## Installation

### Prerequisites

- OCaml (recommended version 4.14.0 or newer)
- opam (OCaml package manager)
- dune (OCaml build system)

### Setup

1. Clone the repository:

```bash
git clone https://github.com/yourusername/cyclopy.git
cd cyclopy
```

2. Create a local switch for the project:

```bash
opam switch create . --deps-only
eval $(opam env)
```

3. Install dependencies:

```bash
make install_dev
```

4. Build the project:

```bash
dune build
```

## Usage

Run Cyclopy by specifying the entry point of your Python project:

```bash
dune exec -- circular_imports /path/to/your/project/main.py
```

For instance:

```bash
dune exec -- circular_imports data/fixtures/main.py
```

For verbose output, add the `-v` flag:

```bash
dune exec -- circular_imports -v /path/to/your/project/main.py
```

### Example Output

When circular imports are found:

```
Found 3 circular import chain(s):

__init__ -> main -> __init__
main -> c -> main
main -> b -> c -> main
```

## How It Works

Cyclopy analyzes your Python files to extract import statements, builds a directed graph of dependencies, and then detects cycles in this graph. It uses OCaml's speed and strong type system to provide fast and reliable results.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

[MIT License](LICENSE)
```

This README provides clear installation instructions with local switch setup, usage examples, and a brief explanation of how the tool works. It also includes sections for features, contributing, and licensing information.
