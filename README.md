__BRAINFUCH__
*Like brainfuck, but not quite*

**Differences from Brainfuck**
`,` and `.` (store and print) are pure functions. This means that you need to provide your input along with the program. It's only possible to see the output after the program terminates (if it ever terminates).

**Features**
Brainfuch aims at being a production ready Brainfuck development toolkit, right now it's more *brain* than, ehm, *fact*.

- Lexer+Parser+Interpreter
- Opinionated Formatter
- ... a repl is in the making, but the API is a bit awkward, suggestions from professional Brainfuck developers (5+ years experience) are welcome

**Contributing**
More features I would like to see in the future:
- frontend for a compiler (LLVM?)
- a left-pad implementation in Brainfuch
