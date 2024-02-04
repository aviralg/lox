module Parser

open Utilities

type TokenData =
    | LeftParen
    | RightParen
    | LeftBrace
    | RightBrace
    | Comma
    | Dot
    | Minus
    | Plus
    | Semicolon
    | Slash
    | Star
    | Bang
    | BangEqual
    | Equal
    | EqualEqual
    | Greater
    | GreaterEqual
    | Less
    | LessEqual
    | Identifier of string
    | String of string
    | Number of double
    | And
    | Class
    | Else
    | False
    | Fun
    | For
    | If
    | Nil
    | Or
    | Print
    | Return
    | Super
    | This
    | True
    | Var
    | While
    | Eof

let KeywordTable = 

type Location =
    { Row: int }

type Token = 
    { Location: Location
      TokenData: TokenData }

type Input =
    { Content: string
      mutable Pos: int }

    static member init content =
        { Content = content
          Pos = 0 }

    member self.isAtEnd =
        self.Pos >= self.Content.Length
    
    member self.peek =
        self.Content[self.Pos]

    member self.read =
        let c = self.Content[self.Pos]
        self.Pos <- self.Pos + 1
        c
 
    member self.readUntil c =
        self.readWhile (fun b -> c <> b)

    member self.readWhile f =
        while not self.isAtEnd && f self.peek do
            ignore self.read
        self.Pos - 1

    member self.readIf c =
        if self.isAtEnd
        then false 
        else 
            let res = self.peek = c
            if res then self.Pos <- self.Pos + 1 
            res

let isAlphaNumeric c =
    (c >= 'A' && c <= 'Z') ||
    (c >= 'a' && c <= 'z') ||
    (c >= '0' && c <= '9') ||
    c = '_'

let tokenizer (input: Input) =
    let mutable row = 0

    let makeToken tokenData =
        { Location = { Row = row }
          TokenData = tokenData }

    let readNumber () =
        let beg = input.Pos - 1
        let fin = input.readWhile System.Char.IsAsciiDigit
        input.Content[beg .. fin] |> double |> Number |> makeToken

    let readIdentifier () =
        let beg = input.Pos - 1
        let fin = input.readWhile isAlphaNumeric
        let tokenData =
            match input.Content[beg .. fin] with
            | "and" -> And
            | "class" -> Class
            | "else" -> Else
            | "false" -> False
            | "for" -> For
            | "fun" -> Fun
            | "if" -> If
            | "nil" -> Nil
            | "or" -> Or
            | "print" -> Print
            | "return" -> Return
            | "super" -> Super
            | "this" -> This
            | "true" -> True
            | "var" -> Var
            | "while" -> While
            | str -> Identifier str
        makeToken tokenData

    let rec readToken () =
        let c = input.read
        match c with
        | ' ' | '\t' | '\r' -> nextToken ()
        | '\n' ->
            row <- row + 1
            nextToken ()
        | '(' -> makeToken LeftParen
        | ')' -> makeToken RightParen
        | '{' -> makeToken LeftBrace
        | '}' -> makeToken RightBrace
        | ',' -> makeToken Comma
        | '.' -> makeToken Dot
        | '-' -> makeToken Minus
        | '+' -> makeToken Plus
        | ';' -> makeToken Semicolon
        | '*' -> makeToken Star
        | '!' -> makeToken <| if input.readIf '=' then BangEqual else Bang
        | '=' -> makeToken <| if input.readIf '=' then EqualEqual else Equal
        | '<' -> makeToken <| if input.readIf '=' then LessEqual else Less
        | '>' -> makeToken <| if input.readIf '=' then GreaterEqual else Greater
        | '/' -> 
            if input.readIf '/'
            then 
                input.readUntil '\n' |> ignore
                nextToken ()
            else
                makeToken Slash
        | d when System.Char.IsAsciiDigit d -> readNumber ()
        | i when System.Char.IsLetter i -> readIdentifier ()
        | _ -> failwith $"invalid character {c} at line {row}"
    
    and nextToken () =
        if input.isAtEnd 
        then makeToken Eof
        else readToken ()

    nextToken

let tokenize (str:string) =
    let input = Input.init str
    let tokenizer = tokenizer input

    let rec helper () =
        match tokenizer () with
        | {TokenData = Eof; Location = _} -> []
        | token -> token :: helper ()
    
    helper ()

type Expr =
    | Assign of name:Token * expr:Expr
    | Binary of left:Expr * op:Token * right:Expr
    | Call of callee:Expr * args:list<Expr>
    | Get of object:Expr * name:Token
    | Grouping of expr:Expr
    | Literal of expr:Expr // TODO - what is this?
    | Logical of left:Expr * op:Token * right:Expr
    | Set of obj:Expr * name:Token * value:Expr
    | Super of keyword:Token * method:Token
    | This of keyword:Token
    | Unary of op:Token * expr:Expr
    | Variable of name:Token



(*
    <Expr> -> Literal | Unary | Binary | Grouping

    <Literal> -> NUMBER | STRING | "true" | "false" | "nil"

    <

*)

(*
    static member ToSExp expr =
        let strs =
            match expr with
            | Assign (name, expr) -> [Token.ToSExp name; Expr.ToSExp expr]
            | Binary (left, op, right) -> [Token.ToSExp op; Expr.ToSexp left; Expr.ToSexp right]
            | Call (callee, args) -> [Expr.ToSExp callee; ]
            | Get (object, name)
            | Grouping (expr)
            | Literal (expr) // TODO - what is this?
            | Logical (left, op, right)
            | Set (obj, name, value)
            | Super (keyword, method)
            | This (keyword)
            | Unary (op, expr)
            | Variable (name)
        parenthesize strs
*)

(*
expression -> ...
equality   -> ...
comparison -> ...
term       -> ...
factor     -> factor ("/" | "*") unary
            | unary
unary      -> ("-" | "!") unary
            | primary
primary    -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")"
*)

let parser tokens =
