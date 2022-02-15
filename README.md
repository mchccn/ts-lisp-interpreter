# ts-lisp-interpreter

> Rudimentary Lisp interpreter implemented in TypeScript types.

### Types

```ts
type Program = `\
; Your code here
`;

type Tokenized = Tokenize<Program>;
//   ^? Tokens

type TokenTypes = Tokenized extends UnknownArray ? EachKey<Tokenized, "type"> : never;
//   ^? Each token's type for debugging

type Parsed = Parse<Tokenized>;
//   ^? Parsed expression

type Structure = Simplify<Parsed>;
//   ^? Expression as formatted string for debugging

type Output = Interpret<Parsed>;
//   ^? Execution output
```

(note: this is a work-in-progress but most of the work is done; what's left is to add in everything else)

**[playground](https://tsplay.dev/rudimentarylispinterpreter)**
