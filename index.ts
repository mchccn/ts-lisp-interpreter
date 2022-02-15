/**
 * Rudimentary Lisp interpreter implemented in TypeScript types
 * 
 * Full tokenization, parsing, and interpretation pipline
 * 
 * Copyright cursorsdottsx 2022
 */

// Aliases

type UnknownArray = ReadonlyArray<unknown>;

// Arithmetic

type Increment<A extends UnknownArray> = [...A, 0];

type Decrement<A extends UnknownArray> = A extends [infer _, ...infer Rest] ? Rest : [];

type Add<A extends UnknownArray, B extends UnknownArray> = [...A, ...B];

type Subtract<A extends UnknownArray, B extends UnknownArray> = B extends [infer _, ...infer Rest] ? Subtract<Decrement<A>, Rest> : A;

// Numbers

type IsNumber<S extends string> = S extends "" ? true : S extends `${infer C}${infer Rest}` ? C extends Digit ? IsNumber<Rest> : false : never;

type Repeat<A extends UnknownArray, T extends number[], R extends UnknownArray = []> = A extends [] ? R : T["length"] extends 0 ? R : Repeat<A, Decrement<T>, [...R, ...A]>;

type TenTimes<A extends UnknownArray> = [
    ...A,
    ...A,
    ...A,
    ...A,
    ...A,
    ...A,
    ...A,
    ...A,
    ...A,
    ...A,
];

type DigitToNumber<D extends string> = {
    "0": [];
    "1": [0];
    "2": [0, 0];
    "3": [0, 0, 0];
    "4": [0, 0, 0, 0];
    "5": [0, 0, 0, 0, 0];
    "6": [0, 0, 0, 0, 0, 0];
    "7": [0, 0, 0, 0, 0, 0, 0];
    "8": [0, 0, 0, 0, 0, 0, 0, 0];
    "9": [0, 0, 0, 0, 0, 0, 0, 0, 0];
}[D & Digit];

type Magnitude<N extends number[], R extends number[] = [0]> = N["length"] extends 0 ? [] : N["length"] extends 1 ? R : Magnitude<Decrement<N>, TenTimes<R>>;

type Expand<
    S extends string,
    Digits = Split<S>,
    Index extends number[] = Split<S>,
    R extends UnknownArray = []
> = Index["length"] extends 0
    ? R
    : S extends ""
        ? R
        : Digits extends [infer First, ...infer Rest] //@ts-ignore
            ? Expand<S, Rest, Decrement<Index>, [...R, ...Repeat<Magnitude<Index>, DigitToNumber<First & string>>]>
            : R;

type ToNumber<
    S extends string
> = IsNumber<S> extends true
    ? { type: "NUMBER"; value: Expand<S>; }
    : never;

// Strings

type Split<S extends string, R extends string[] = []> = S extends `${infer Char}${infer Rest}` ? Split<Rest, [...R, Char]> : R;

type StringLength<S extends string> = Split<S>["length"];

type SkipStart<S extends string, N extends number[]> = S extends "" ? "" : N["length"] extends 0 ? S : S extends `${infer _}${infer Rest}` ? SkipStart<Rest, Decrement<N>> : never;

type TakeStart<S extends string, N extends number[]> = S extends "" ? "" : N["length"] extends 0 ? "" : S extends `${infer C}${infer Rest}` ? `${C}${TakeStart<Rest, Decrement<N>>}` : never;

type Substring<
    S extends string,
    Start extends number[],
    End extends number[]
> = TakeStart<SkipStart<S, Start>, Subtract<End, Start>>;

type Digit = Split<"0123456789">[number];

type Alpha = Split<"qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM_">[number];

type Alphanumeric = Split<"qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM_0123456789">[number];

// Debug

type EachKey<
    A extends UnknownArray,
    K extends string,
    R extends UnknownArray = [],
> = A extends [infer First, ...infer Rest] ? EachKey<Rest, K, [...R, First[K & keyof First]]> : R;

type SimplifyEach<A, R extends string = "["> = A extends [] ? `${R}]` : A extends [infer First, ...infer Rest] ? SimplifyEach<Rest, R extends "[" ? `${R}${Simplify<First>}` : `${R}, ${Simplify<First>}`> : never;

type Simplify<
    E,
> = E extends { type: string; value: any; }
    ? E["type"] extends "SYMBOL"
        ? `sym:${E["value"]}`
        : E["type"] extends "NUMBER"
            ? `lit:${E["value"]["value"]["length"]}`
            : E["type"] extends "STRING"
                ? `lit:${E["value"]["value"]}`
                : E["type"] extends "BOOLEAN"
                    ? `lit:${E["value"]["value"]}`
                    : E["type"] extends "NIL"
                        ? `lit:nil`
                        : SimplifyEach<E["value"]>
    : never;

// Reference type

type Expr = {
    type: "LIST";
    value: Expr[];
} | {
    type: "SYMBOL";
    value: string;
} | {
    type: "LITERAL";
    value: Literal;
};

type Literal = {
    type: "NUMBER";
    value: number[];
} | {
    type: "BOOLEAN";
    value: boolean;
} | {
    type: "STRING";
    value: string;
} | {
    type: "NIL";
    value: null;
};

type Token = {
    type: "BEGIN_LIST" | "END_LIST" |  "SYMBOL" | "NUMBER" | "STRING" | "TRUE" | "FALSE" | "NIL";
    lexeme: string;
    literal: null;
} | {
    type: "END_LIST";
    lexeme: string;
    literal: null;
} | {
    type: "SYMBOL";
    lexeme: string;
    literal: null;
} | {
    type: "NUMBER";
    lexeme: string;
    literal: {
        type: "NUMBER";
        value: number[];
    };
} | {
    type: "STRING";
    lexeme: string;
    literal: {
        type: "STRING";
        value: string;
    }
} | {
    type: "TRUE";
    lexeme: string;
    literal: {
        type: "BOOLEAN";
        value: true;
    };
} | {
    type: "FALSE";
    lexeme: string;
    literal: {
        type: "BOOLEAN";
        value: false;
    };
} | {
    type: "NIL";
    lexeme: string;
    literal: {
        type: "NIL";
        value: null;
    };
};

// Implementation

type KeywordOrSymbol<S extends string> = S extends "true" ? "TRUE" : S extends "false" ? "FALSE" : S extends "nil" ? "NIL" : "SYMBOL";

type LiteralOrSymbol<S extends string> = S extends "true" ? { type: "BOOLEAN"; value: true } : S extends "false" ? { type: "BOOLEAN"; value: false } : S extends "nil" ? { type: "NIL"; value: null } : null;

type Comments<
    Source extends string,
    Current extends number[],
> = Split<Source>[Current["length"]] extends "\n"
        ? Current
        : Current["length"] extends StringLength<Source>
            ? Current
            : Comments<Source, Increment<Current>>;

type Strings<
    Source extends string,
    Start extends number[],
    Current extends number[],
> = Split<Source>[Increment<Current>["length"] & number] extends "\""
    ? [true, {
        type: "STRING";
        lexeme: Substring<Source, Start, Increment<Increment<Current>>>;
        literal: {
            type: "STRING";
            value: Substring<Source, Increment<Start>, Increment<Current>>;
        };
    }, Increment<Increment<Current>>]
    : Split<Source>[Increment<Current>["length"] & number] extends "\n"
        ? { error: "Unterminated string." }
        : Current["length"] extends StringLength<Source>
            ? { error: "Unterminated string." }
            : Strings<Source, Start, Increment<Current>>;

type Numbers<
    Source extends string,
    Start extends number[],
    Current extends number[],
> = Split<Source>[Current["length"]] extends Digit
    ? Numbers<Source, Start, Increment<Current>>
    : [true, {
        type: "NUMBER";
        lexeme: Substring<Source, Start, Current>;
        literal: ToNumber<Substring<Source, Start, Current>>;
    }, Current];

type Symbols<
    Source extends string,
    Start extends number[],
    Current extends number[],
> = Split<Source>[Current["length"]] extends Alphanumeric
    ? Symbols<Source, Start, Increment<Current>>
    : [true, {
        type: KeywordOrSymbol<Substring<Source, Start, Current>>;
        lexeme: Substring<Source, Start, Current>;
        literal: LiteralOrSymbol<Substring<Source, Start, Current>>;
    }, Current];

type ScanToken<
    Source extends string,
    Start extends number[],
    Current extends number[],
> = Split<Source>[Current["length"]] extends infer C
    ? C extends "("
        ? [true, { type: "BEGIN_LIST"; lexeme: C; literal: null; }, Increment<Current>]
        : C extends ")"
            ? [true, { type: "END_LIST"; lexeme: C; literal: null; }, Increment<Current>]
            : C extends " " | "\r" | "\t" | "\v" | "\f" | "\n"
                ? [false, null, Increment<Current>]
                : C extends ";"
                    ? [false, null, Comments<Source, Current>]
                    : C extends "\""
                        ? Strings<Source, Start, Current>
                        : C extends ">"
                            ? Split<Source>[Increment<Current>["length"] & number] extends "="
                                ? [true, { type: "SYMBOL"; lexeme: `${C}${Split<Source>[Increment<Current>["length"] & number]}`; literal: null }, Increment<Increment<Current>>]
                                : [true, { type: "SYMBOL"; lexeme: C; literal: null }, Increment<Current>]
                            :  C extends "<"
                                ? Split<Source>[Increment<Current>["length"] & number] extends "="
                                    ? [true, { type: "SYMBOL"; lexeme: `${C}${Split<Source>[Increment<Current>["length"] & number]}`; literal: null }, Increment<Increment<Current>>]
                                    : [true, { type: "SYMBOL"; lexeme: C; literal: null }, Increment<Current>]
                                :  C extends "="
                                    ? Split<Source>[Increment<Current>["length"] & number] extends "="
                                        ? [true, { type: "SYMBOL"; lexeme: `${C}${Split<Source>[Increment<Current>["length"] & number]}`; literal: null }, Increment<Increment<Current>>]
                                        : [true, { type: "SYMBOL"; lexeme: C; literal: null }, Increment<Current>]
                                    : C extends "!"
                                        ? Split<Source>[Increment<Current>["length"] & number] extends "="
                                            ? [true, { type: "SYMBOL"; lexeme: `${C}${Split<Source>[Increment<Current>["length"] & number]}`; literal: null }, Increment<Increment<Current>>]
                                            : [true, { type: "SYMBOL"; lexeme: C; literal: null }, Increment<Current>]
                                        : C extends "+"
                                            ? [true, { type: "SYMBOL"; lexeme: C; literal: null }, Increment<Current>]
                                            : C extends "-"
                                                ? [true, { type: "SYMBOL"; lexeme: C; literal: null }, Increment<Current>]
                                                : C extends "*"
                                                    ? [true, { type: "SYMBOL"; lexeme: C; literal: null }, Increment<Current>]
                                                    : C extends "/"
                                                        ? [true, { type: "SYMBOL"; lexeme: C; literal: null }, Increment<Current>]
                                                        : C extends Digit
                                                            ? Numbers<Source, Start, Current>
                                                            : C extends Alpha
                                                                ? Symbols<Source, Start, Current>
                                                                : { error: `Unexpected character '${C & string}' at index ${Current["length"]}.`}
    : never;

type ScanTokens<
    Source extends string,
    Start extends number[] = [],
    Current extends number[] = [],
    Tokens extends UnknownArray = [],
> = 
    Current["length"] extends StringLength<Source>
        ? Tokens
        : ScanToken<Source, Start, Current> extends [infer HasToken, infer Token, infer NewCurrent]
            ? NewCurrent extends number[]
                ? HasToken extends true
                    ? ScanTokens<Source, NewCurrent, NewCurrent, [...Tokens, Token]>
                    : ScanTokens<Source, NewCurrent, NewCurrent, Tokens>
                : never
            : ScanToken<Source, Start, Current>;

type Tokenize<Source extends string> = ScanTokens<Source>;

type ValidateParentheses<
    Tokens,
    Stack extends UnknownArray = [],
> = Tokens extends []
    ? Stack extends []
        ? true
        : false
    : Tokens extends [infer T, ...infer Rest]
        ? T extends Token
            ? T["type"] extends "BEGIN_LIST"
                ? ValidateParentheses<Rest, [...Stack, T]>
                : T["type"] extends "END_LIST"
                    ? Stack extends []
                        ? false
                        : Stack extends [...infer First, infer _]
                            ? ValidateParentheses<Rest, First>
                            : never
                    :  ValidateParentheses<Rest, Stack>
            : never
        : never;

type ParsePrimary<
    Tokens extends Token[],
    Current extends number[] = [],    
> = Tokens[Current["length"]]["type"] extends "BEGIN_LIST"
    ? ParseExpression<Tokens, Current>
    : Tokens[Current["length"]]["type"] extends "END_LIST"
        ? void
        : Tokens[Current["length"]]["type"] extends "TRUE" | "FALSE"
            ? [{ type: "BOOLEAN"; value: Tokens[Current["length"]]["literal"] }, Increment<Current>]
            : Tokens[Current["length"]]["type"] extends "NIL"
                ? [{ type: "NIL"; value: Tokens[Current["length"]]["literal"] }, Increment<Current>]
                : Tokens[Current["length"]]["type"] extends "STRING"
                    ? [{ type: "STRING"; value: Tokens[Current["length"]]["literal"] }, Increment<Current>]
                    : Tokens[Current["length"]]["type"] extends "NUMBER"
                        ? [{ type: "NUMBER"; value: Tokens[Current["length"]]["literal"] }, Increment<Current>]
                        : Tokens[Current["length"]]["type"] extends "SYMBOL"
                            ? [{ type: "SYMBOL"; value: Tokens[Current["length"]]["lexeme"] }, Increment<Current>]
                            : { error: "Unexpected token type." };

type ParseExpressionLoop<
    Tokens extends Token[],
    Current extends number[] = [],
    List extends UnknownArray = [],
> = Current["length"] extends Tokens["length"]
    ? [{ type: "LIST"; value: List }, Current]
    : Tokens[Current["length"]] extends "END_LIST"
        ? [{ type: "LIST"; value: List }, Increment<Current>]
        : ParsePrimary<Tokens, Current> extends void
            ? [{ type: "LIST"; value: List }, Current]
            : ParsePrimary<Tokens, Current> extends [infer Expr, infer NewCurrent]
            ? NewCurrent extends number[]
                ? ParseExpressionLoop<Tokens, NewCurrent, [...List, Expr]>
                : never
            : ParsePrimary<Tokens, Current>

type ParseExpression<
    Tokens extends Token[],
    Current extends number[] = [],
> = Tokens[Current["length"]]["type"] extends "BEGIN_LIST"
    ? ParseExpressionLoop<Tokens, Increment<Current>> extends [infer Expr, infer NewCurrent]
        ? NewCurrent extends number[]
            ? Tokens[NewCurrent["length"]]["type"] extends "END_LIST"
                ? [Expr, Increment<NewCurrent>]
                : { error: "Expected ')' after list contents." }
            : never
        : ParseExpressionLoop<Tokens, Increment<Current>> 
    : { error: "Expected '(' to begin list." };

type ParseTokens<
    Tokens extends Token[],
    Current extends number[] = [],
    List extends UnknownArray = [],
> = Current["length"] extends Tokens["length"]
    ? { type: "LIST"; value: List }
    : ParseExpression<Tokens, Current> extends [infer Expr, infer NewCurrent]
        ? NewCurrent extends number[]
            ? ParseTokens<Tokens, NewCurrent, [...List, Expr]>
            : never
        : ParseExpression<Tokens, Current>;

type Parse<Tokens extends Token[]> = ValidateParentheses<Tokens> extends true ? ParseTokens<Tokens> : { error: "Unmatched parentheses." };

type FormatOutput<A, R extends string = ""> = A extends [] ? R : A extends [infer First, ...infer Rest] ? First extends { output: any; } ? FormatOutput<Rest, R extends "" ? First["output"] : `${R} / ${First["output"]}`> : FormatOutput<Rest, R> : never;

type StringifyAll<A, R extends string = ""> = A extends [] ? R : A extends [infer First, ...infer Rest] ? StringifyAll<Rest, R extends "" ? Stringify<First> : `${R} ${Stringify<First>}`> : never;

type Stringify<
    E
> = E extends { type: string; value: any; }
    ? E["type"] extends "SYMBOL"
        ? E["value"] extends "print" | "+"
            ? `<fn ${E["value"]}>`
            : `[undefined]`
        : E["type"] extends "STRING" | "BOOLEAN"
            ? `${E["value"]["value"]}`
            : E["type"] extends "NUMBER"
                ? `${E["value"]["value"]["length"]}`
                : E["type"] extends "LIST" 
                    ? `(${StringifyAll<E["value"]>})`
                    : "nil"
    : never;

type PlusAll<A, R extends UnknownArray = []> = A extends [] ? R : A extends [infer First, ...infer Rest] ? First extends UnknownArray ? PlusAll<Rest, Add<R, First>> : never : never;

type NumberifyAll<A, R extends UnknownArray = []> = A extends [] ? R : A extends [infer First, ...infer Rest] ? NumberifyAll<Rest, [...R, Numberify<First>]> : never;

type Numberify<
    E
> = E extends { type: string; value: any; }
    ? E["type"] extends "SYMBOL"
        ? E["value"] extends "print" | "+"
            ? 1
            : 0
        : E["type"] extends "BOOLEAN"
            ? E["value"] extends true
                ? 1
                : 0
            : E["type"] extends "NUMBER"
                ? E["value"]["value"]["length"]
                : E["type"] extends "LIST"
                    ? E["value"]["length"]
                    : E["type"] extends "STRING"
                        ? IsNumber<E["value"]> extends true
                            ? ToNumber<E["value"]>["value"]["length"]
                            : 0
                        : 0
    : never;

type Print<E> = { output: E extends UnknownArray ? StringifyAll<E> : Stringify<E> };

type EvaluateEach<
    E,
    Output extends string = "",
    Env extends {} = {},
    R extends UnknownArray = [],
> = E extends []
    ? R
    : E extends [infer First, ...infer Rest]
        ? EvaluateEach<Rest, Output, Env, [...R, Evaluate<First, Output, Env>]>
        : never;

type Evaluate<
    E,
    Output extends string = "",
    Env extends {} = {},
> = E extends { type: string; value: any }
    ? E["type"] extends "LITERAL"
        ? E["value"]
            : E["type"] extends "SYMBOL"
                ? keyof Env extends E["value"]
                    ? Env[E["value"]]
                    : null
                : E["value"] extends []
                    ? []
                    : E["value"] extends [infer Head, ...infer Body]
                        ? Head extends { type: string; value: any }
                            ? Head["type"] extends "SYMBOL"
                                ? Head["value"] extends "print"
                                    ? Print<Body>
                                    : Head["value"] extends "+"
                                        ? PlusAll<NumberifyAll<Body>>
                                        : void
                                : EvaluateEach<E["value"], Output>
                            : never
                        : never
    : never;

type Interpret<
    E,
> = FormatOutput<E extends { type: "LIST"; value: any } ? EvaluateEach<E["value"]> : Evaluate<E>>;

type Program = `\
(print 1)
(print 6 9)
(print (1 2 3))
(print "hello world")
(print ("h" "i"))
(print true false)
(print nil)
(print (4 (2 (0))))
`;

type Tokenized = Tokenize<Program>;
//   ^?

type TokenTypes = Tokenized extends UnknownArray ? EachKey<Tokenized, "type"> : never;
//   ^?

type Parsed = Parse<Tokenized>;
//   ^?

type Structure = Simplify<Parsed>;
//   ^?

type Output = Interpret<Parsed>;
//   ^?
