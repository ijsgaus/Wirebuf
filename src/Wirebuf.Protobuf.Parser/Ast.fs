namespace Wirebuf.Protobuf.Ast

open FParsec
open System
open System.Data.SqlTypes
open System.Linq
open System.Text
open Wirebuf.Ast

type AstNodeType =
    | Spaces = 1
    | Tabs = 2
    | NewLines = 3
    | Ident = 4
    | PunctuationSign = 5
    | Punctuation = 6
    | FullIdent = 7
    | IntLit = 8
    | IntLitNode = 9
    | Sign = 10
    | SignNode = 11
    | Exponent = 12
    | FloatLit = 13

type IAstNode = IAstNode<AstNodeType>
type AstNodePrefix = AstNodePrefix<AstNodeType>

type WhiteSpaces =
    | Space of uint32
    | Tabs of uint32
    | NewLine of string
    interface IWhitespaceNode<AstNodeType> with
        member __.Length =
            match __ with
            | Space(cnt) -> cnt
            | Tabs(cnt) -> cnt
            | NewLine(str) -> uint32 str.Length
        member __.NodeType =
            match __ with
            | Space(_) -> AstNodeType.Spaces
            | Tabs(_) -> AstNodeType.Tabs
            | NewLine(_) -> AstNodeType.NewLines
        member __.ToSource() =
            match __ with
            | Space(cnt) -> String.replicate (int32 cnt) " "
            | Tabs(cnt) -> String.replicate (int32 cnt) "\t"
            | NewLine(str) -> str
        member __.WhitespaceType =
            match __ with
            | Space(_) -> WhitespaceType.Spaces
            | Tabs(_) -> WhitespaceType.Tabs
            | NewLine(_) -> WhitespaceType.NewLines

type IAstComplexNode = IAstComplexNode<AstNodeType>

type PunctuationSign =
    | PunctuationSign of string
    interface IAstNode with
        member __.Length = let (PunctuationSign s) = __ in uint32 s.Length
        member __.NodeType =  AstNodeType.PunctuationSign
        member __.ToSource() = let (PunctuationSign s) = __ in s

type PunctuationNode(prefix : IWhitespaceNode<AstNodeType> list, sign : PunctuationSign) =
    inherit ComplexNode<AstNodeType>(prefix.Cast<IAstNode>().Union([sign]))
    member __.Sign = sign
    override __.NodeType = AstNodeType.Punctuation


type Ident =
    private | Ident of string
    static member Create str =
        if Enumerable.All(str, fun ch -> (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z'))
        then Result.Ok (Ident str)
        else Result.Error("Identifier must contains only ascii letter")
    static member CreateUnchecked str = Ident str
    interface IAstNode with
        member __.Length = let (Ident s) = __ in uint32 s.Length
        member __.NodeType = AstNodeType.Ident
        member __.ToSource() = let (Ident s) = __ in s


type FullIdent =
    private | FullIdent of AstNodePrefix * (Ident list)
    static member Create ids =
        match ids with
        | [] -> Result.Error "Qualified identifier must contains at least one identifier"
        | _ -> FullIdent([], ids)  |> Result.Ok
    static member Create(prefix, ids) =
        match ids with
        | [] -> Result.Error "Qualified identifier must contains at least one identifier"
        | _ -> FullIdent(prefix, ids)  |> Result.Ok
    static member CreateUnchecked ids = FullIdent([], ids)
    static member CreateUnchecked(prefix, ids) = FullIdent(prefix, ids)
    interface IAstComplexNode with
        member __.Prefix = let (FullIdent (p, _)) = __ in p
        member __.NodeType = AstNodeType.FullIdent
        member __.Nodes = seq {
            let (FullIdent (p, s)) = __
            yield! (p |> Seq.map (fun a -> a :> IAstNode))
            match s with
            | [] -> ()
            | [id] -> yield (id :> IAstNode)
            | id :: rest ->
                yield (id :> IAstNode)
                yield! (rest |> Seq.collect (fun o -> [ PunctuationSign "." :> IAstNode; o :> IAstNode ]))
        }
        member __.Length = AstNode.complexLength __
        member __.ToSource() = AstNode.complexSource __


type IntLit =
    | DecLit of string
    | OctalLit of string
    | HexLit of string
    interface IAstNode with
        member __.NodeType = AstNodeType.IntLit
        member __.Length =
            match __ with
            | DecLit s -> uint32 s.Length
            | OctalLit s -> uint32 s.Length
            | HexLit s -> (uint32 s.Length) + 2u
        member __.ToSource() =
            match __ with
            | DecLit s -> s
            | OctalLit s -> s
            | HexLit s -> "0x" + s

type IntLitNode(prefix : IWhitespaceNode<AstNodeType> seq, intLit : IntLit) =
    inherit ComplexNode<AstNodeType>(prefix.Cast<IAstNode>().Union([intLit]))
    override __.NodeType = AstNodeType.IntLitNode
    member __.Literal = intLit

type Sign =
    | Plus
    | Minus
    interface IAstNode with
        member __.Length = 1u
        member __.NodeType = AstNodeType.Sign
        member __.ToSource() =
            match __ with | Plus -> "+" | Minus -> "-"

type SignNode(prefix : IWhitespaceNode<AstNodeType> seq, sign : Sign) =
     inherit ComplexNode<AstNodeType>(prefix.Cast<IAstNode>().Union([sign]))
     override __.NodeType = AstNodeType.SignNode
     member __.Sign = sign


type ExponentLit =
    {
        IsUpper : bool
        Sign : Sign option
        Digits : string
    }
    interface IAstNode with
        member __.Length = 1u + (__.Sign |> Option.map AstNode.length |> Option.defaultValue 0u) + uint32 __.Digits.Length
        member __.NodeType = AstNodeType.Exponent
        member __.ToSource() =
            let sb = StringBuilder()
            let sb = if __.IsUpper then sb.Append("E") else sb.Append("e")
            let sb = sb.Append(__.Sign  |> Option.map AstNode.toSource |> Option.defaultValue "")
            let sb = sb.Append(__.Digits)
            sb.ToString()




type FloatLit =
    | Nan
    | Inf
    | FloatLit of WholePart : string * Exponent : (ExponentLit option)
    interface IAstNode with
        member __.Length =
            match __ with
            | Nan -> 3u
            | Inf -> 3u
            | FloatLit(wp, exp) ->
                (uint32 wp.Length) + (exp |> Option.map AstNode.length |> Option.defaultValue 0u)
        member __.ToSource() =
            match __ with
            | Nan -> "nan"
            | Inf -> "inf"
            | FloatLit(wp, exp) ->
                wp + (exp |> Option.map AstNode.toSource |> Option.defaultValue "")
        member __.NodeType = AstNodeType.FloatLit


type Quote = | SingleQuote | DoubleQuote
(*
type StrLit = {
    Quote : Quote
    Value : string
}

type Constant =
    | IdentConst of QualifiedIdentifier
    | IntConst of Sign * IntLit
    | FloatConst of Sign * FloatLit
    | StrConst of StrLit
    | BoolConst of bool

type ImportKind = | WeakImport | PublicImport

type ImportStatement = {
    Kind : ImportKind option
    Path : StrLit
}
*)


