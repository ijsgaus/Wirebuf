namespace Wirebuf.Ast

open System
open System.Linq
open System.Text

type Identifier =
    private | Identifier of string
    static member Create str =
        if Enumerable.All(str, fun ch -> (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z'))
        then Ok (Identifier str)
        else Error("Identifier must contains only ascii letter")
    static member CreateUnchecked str = Identifier str
    override __.ToString() = let (Identifier s) = __ in s

type QualifiedIdentifier =
    private | QualifiedIdentifier of Identifier list
    static member Create ids =
        match ids with
        | [] -> Error "Qualified identifier must contains at least one identifier"
        | _ -> QualifiedIdentifier ids  |> Ok
    static member CreateUnchecked ids = QualifiedIdentifier ids

    override __.ToString() =
        let (QualifiedIdentifier lst) = __
        let folder (st : StringBuilder) (id : Identifier) =
            if st.Length = 0 then st.Append(id.ToString()) else st.Append("." + id.ToString())
        let sb = lst |> List.fold folder (StringBuilder())
        sb.ToString()

type IntLit =
    | DecLit of string
    | OctalLit of string
    | HexLit of string

type Sign = | Plus | Minus


type ExponentLit = {
    Sign : Sign
    Digits : string
}


type FloatLit =
    | Nan
    | Inf
    | FloatLit of WholePart : string * Exponent : (ExponentLit option)

