# Mini Lustre Compiler

This project was created for Marc Pouzet and Thimothy Bourke's Synchronous Programming course. The aim is to compile a sub-fragment of the Lustre synchronous language.

## Usage

### Dependencies

The project relies on the following:

- **GoblintCil**: A library for `C`'s abstract syntax tree.
- **Dune**: An OCaml build system.
- **Menhir**: A parser generator.

### Installation

To set up and build the project, execute these steps:

1. **Create and Configure OPAM Switch**:
  - Create a new OPAM switch named `lmoch` with OCaml version 4.14.0:
  ```bash 
  opam switch create lmoch 4.14.0 --yes
  ```
  - Update environment variables for the new switch:
  ```bash
  eval $(opam env --switch=lmoch)
  ```
  - Install dependencies:
  ```bash
  opam install dune menhir goblint-cil --yes
  ```
  **Build the project**:
  ```bash
  dune build
  ```

### Example Usage

- Fibonacci Sequence:
  ```
  make
  ./lmoch.exe demo/Fibonacci.lus fibonacci
  gcc demo/fibonacci.c
  ./a.out
  ```

- Reset Functionnality:
  ```
  make
  ./lmoch.exe demo/Reset.lus main0
  gcc demo/Reset.c
  echo "0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0" | ./a.out
  ```

- Automaton :
  ```
  make
  ./lmoch.exe demo/ColorAutomaton.lus colorautomaton
  gcc demo/ColorAutomaton.c
  ./a.out
  ```

### Generic Usage

Basic command:
```
$ make
$ ./lmoch.exe [file.lus] [OPTIONS] [main-node]
```

Options:
- `-v` or `-verbose` : Print intermediate transformations.
- `-parse-only` : Stop after parsing.
- `-type-only` : Stop after type checking.
- `-automaton-only` : Stop after cleaning the automaton.
- `-clock-only` : Stop after clocking.
- `-norm-only` : Stop after normalization.
- `-sched-only` : Stop after scheduling.
- `-imp-only` : Stop after imperative translation.
- `-c-only` : Stop after C conversion.
- `-no-sleep` : Disable sleep call in the C main file.

Build and Execute in Main Directory:
```
make
cd examples
../lmoch.exe [file].lus -v [main-node] [options...]
```

### Test Management 
Run Tests:
```
make test --always-make    # If the output is empty, then the tests pass
```
Upgrade Tests:
```
make promote -i            # Upgrade all tests
```

## Work Done

The project has implemented features such as `when`, `merge` (on simple ADTs), `reset`, and `automata`. It comprises several compilation phases:

  + Clean_automaton (`Parsed` -> `Parsed`): Desugaring of automata.
  + Typing (`Parsed` -> `Typed`): Checks consistency and simplifies arrows and pre using fby.
  + Normalize (`Typed` -> `Normalised`): De-inline expressions into local variables, static simplification of arithmetic expressions.
  + Clocking (`Normalised` -> `Clocked`): Verifies clock consistency.
  + Schedule (`Clocked` -> `Scheduled`): Orders equations based on dependencies, causality analysis.
  + Imp (`Scheduled` -> `Imp`): Translates to an imperative language AST before translation to C.
  + Compile (`Imp` -> `C`): Utilizes Goblint.Cil's C AST, manages main function return and arguments.

## Contributing

There is currently no code of conduct for contributing, if you're willing to please let me know but this repository is not designed to become a larger project.

## License

This project is under MIT License.

## Acknowledgments

- Paul Patault and Émilien Lemaire for the architecture of the debugging/tests and dune structure, and for some test example files.
