namespace Wirebuf.Protobuf.Ast

open FParsec
open System
open System.Linq
open System.Net
open System.Text
open Wirebuf.Ast
open Wirebuf.Ast

type AstNodeType =
    | Spaces = 1
    | Tabs = 2
    | NewLines = 3
    | Ident = 4
    | FullIdent = 5
    | IntLit = 6
    | IntLitNode = 7
    | Dot = 8
    | Sign = 9
    | Exponent = 10
    | FloatLit = 11
    | StrLit = 12
    | Const = 13
    | ConstValue = 14
    | ConstNode = 15
    | Semicolon = 16
    | EmptyStatement = 17



type IAstNode = IAstNode<AstNodeType>
type AstNodePrefix = AstNodePrefix<AstNodeType>
type IWhitespaceNode = IWhitespaceNode<AstNodeType>

type WhiteSpaces =
    | Spaces of uint32
    | Tabs of uint32
    | NewLines of string
    interface IWhitespaceNode<AstNodeType> with
        member __.Length =
            match __ with
            | Spaces(cnt) -> cnt
            | Tabs(cnt) -> cnt
            | NewLines(str) -> uint32 str.Length
        member __.NodeType =
            match __ with
            | Spaces(_) -> AstNodeType.Spaces
            | Tabs(_) -> AstNodeType.Tabs
            | NewLine(_) -> AstNodeType.NewLines
        member __.ToSource() =
            match __ with
            | Spaces(cnt) -> String.replicate (int32 cnt) " "
            | Tabs(cnt) -> String.replicate (int32 cnt) "\t"
            | NewLines(str) -> str
        member __.WhitespaceType =
            match __ with
            | Spaces(_) -> WhitespaceType.Spaces
            | Tabs(_) -> WhitespaceType.Tabs
            | NewLines(_) -> WhitespaceType.NewLines

type IAstComplexNode = IAstComplexNode<AstNodeType>



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

type DotSymbol =
    | Dot
    interface IAstNode with
        member __.Length = 1u
        member __.ToSource() = "."
        member __.NodeType = AstNodeType.Dot


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
                yield! (rest |> Seq.collect (fun o -> [ Dot :> IAstNode; o :> IAstNode ]))
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



type StrLit =
    {
        Quote : Quote
        Original : string
        Value : string
    }
    interface IAstNode with
        member __.Length = (uint32 __.Original.Length) + 2u
        member __.ToSource() =
            let ch = match __.Quote with | SingleQuote -> "'" | DoubleQuote -> "\""
            ch + __.Original + ch
        member __.NodeType = AstNodeType.StrLit



type Const =
    | IdentConst of FullIdent
    | IntConst of IntLit
    | FloatConst of FloatLit
    | StrConst of StrLit
    | BoolConst of bool
    interface  IAstNode with
        member __.Length =
            match __ with
            | IdentConst id -> AstNode.length id
            | IntConst i -> AstNode.length i
            | FloatConst f -> AstNode.length f
            | StrConst s -> AstNode.length s
            | BoolConst b -> if b then 4u else 5u
        member __.ToSource() =
            match __ with
            | IdentConst id -> AstNode.toSource id
            | IntConst i -> AstNode.toSource i
            | FloatConst f -> AstNode.toSource f
            | StrConst s -> AstNode.toSource s
            | BoolConst b -> if b then "true" else "false"
        member  __.NodeType = AstNodeType.Const

type ConstValueNode(prefix, cn) =
    inherit ComplexNode<AstNodeType, Const>(prefix, cn, AstNodeType.ConstValue)


type ConstNode(prefix : AstNodePrefix, sign : Sign option, c : ComplexNode<AstNodeType, Const>) =
    inherit ComplexNode<AstNodeType>(prefix.Cast<IAstNode>()
                                          .Union(sign |> Option.map Seq.singleton<IAstNode> |> Option.defaultValue Seq.empty)
                                          .Union([c]))
    override __.NodeType = AstNodeType.ConstNode
    member __.Sign = sign
    member __.ValueNode = c
    member __.Value = c.Value

type SemicolonSymbol =
    | Semicolon
    interface IAstNode with
        member __.Length = 1u
        member __.ToSource() = ";"
        member __.NodeType = AstNodeType.Semicolon

type EmptyStatement(prefix) =
    inherit ComplexNode<AstNodeType, SemicolonSymbol>(prefix, Semicolon, AstNodeType.EmptyStatement)


(*
type ImportKind = | WeakImport | PublicImport

type ImportStatement = {
    Kind : ImportKind option
    Path : StrLit
}
*)



