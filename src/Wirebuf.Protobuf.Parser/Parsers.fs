namespace Wirebuf.Protobuf.Parser

open System
open System.Text
open FParsec
open Wirebuf.Protobuf.Ast

[<AutoOpen>]
module Parsers =
    // Letters and digits
    let pletter<'a>  : Parser<_, 'a> = asciiLetter
    let pdecimalDigit<'a> : Parser<_, 'a> = anyOf "0123456789"
    let poctalDigit<'a> : Parser<_, 'a> = anyOf "01234567"
    let phexDigit<'a> : Parser<_, 'a> = anyOf "0123456789ABCDEFabcdef"

    // Identifiers
    let pident<'a> : Parser<_, 'a> =
        pletter .>>. (many (pletter <|> pdecimalDigit <|> (pchar '_')))
        |>> fun (a, b) ->
                let sb = StringBuilder()
                sb.Append(a) |> ignore
                b |> Seq.fold (fun (st : StringBuilder) p -> st.Append p) sb |> ignore
                sb.ToString() |> Ident.CreateUnchecked

    let private  inOneId (a, b) =  List.append [a] b
    let private  inOneIdLst (a, b) =  List.append a [b]
    let private chArrToStr a =
        let folder (sb : StringBuilder) (ch : char)  = sb.Append(ch)
        a |> Seq.fold folder (StringBuilder ()) |> fun sb -> sb.ToString()

    let pfullIdent<'a> : Parser<_, 'a> =
        let tail = pchar '.' >>. pident
        (pident .>>. many tail) |>> (inOneId >> FullIdent.CreateUnchecked)

    let pmessageName<'a> : Parser<_, 'a>  = pident
    let penumName<'a> : Parser<_, 'a> = pident
    let pfieldName<'a> : Parser<_, 'a> = pident
    let poneofName<'a> : Parser<_, 'a> = pident
    let pmapName<'a> : Parser<_, 'a> = pident
    let pserviceName<'a> : Parser<_, 'a> = pident
    let prpcName<'a> : Parser<_, 'a> = pident

    let pmessageType<'a> : Parser<_, 'a> =
        opt (pchar '.') >>. (many pident) .>>. pmessageName |>> (inOneIdLst >> FullIdent.CreateUnchecked)

    let penumType<'a> : Parser<_, 'a> =
        opt (pchar '.') >>. (many pident) .>>. penumName |>> (inOneIdLst >> FullIdent.CreateUnchecked)

    // Integer literals

    let pdecimalLit<'a> : Parser<_, 'a> =
        anyOf "123456789" .>>. many pdecimalDigit |>> inOneId |>> chArrToStr |>> DecLit
    let poctalLit<'a> : Parser<_, 'a> =
            pchar '0' .>>. many poctalDigit |>> inOneId |>> chArrToStr |>> OctalLit
    let phexLit<'a> : Parser<_, 'a> =
        pchar '0' >>. anyOf "xX" >>. phexDigit .>>. many phexDigit  |>> inOneId |>> chArrToStr |>> HexLit
    let pintLit<'a> : Parser<_, 'a> =
        attempt phexLit <|> attempt poctalLit <|> pdecimalLit

    // Floating-point literals
    let pdecimals<'a> : Parser<_, 'a> =
        pdecimalDigit .>>. many pdecimalDigit |>> inOneId |>> chArrToStr

    let psign<'a> : Parser<_, 'a> =
        opt (anyOf "+-")
        |>> (fun p -> p |> Option.map (fun p -> if p = '-' then Minus else Plus))

    let pexponent<'a> : Parser<_, 'a> =
        let mapper ((upper, sign), digits) =
            {
                IsUpper = upper
                Sign = sign
                Digits = digits
            }
        anyOf "eE" |>> (fun p -> p = 'E') .>>. psign .>>. pdecimals |>> mapper

    let pfloatLit<'a> : Parser<_, 'a> =
        let pinf : Parser<_, 'a> = pstring "inf" |>> fun _ -> Inf
        let pnan : Parser<_, 'a> = pstring "nan" |>> fun _ -> Nan
        let pf1 : Parser<_, 'a> =
            let mapper ((d1, d2), exp) =
                FloatLit(d1 + "." + Option.defaultValue "" d2, exp)
            pdecimals .>> pchar '.' .>>. opt pdecimals .>>. opt pexponent |>> mapper
        let pf2 : Parser<_, 'a> =
            pdecimals .>>. pexponent |>> fun (a, b) -> FloatLit(a, Some b)
        let pf3 : Parser<_, 'a> =
            pchar '.' >>. pdecimals .>>. opt pexponent |>> fun (a, b) -> FloatLit ("." + a, b)
        let pf : Parser<_, 'a> =
            attempt pf1 <|> attempt pf2 <|> pf3
        attempt pf <|> pinf <|> pnan
(*
    // Boolean
    let pboolLit<'a> : Parser<_, 'a> =
        (pstring "true" |>> fun _ -> true) <|> (pstring "false" |>> fun _ -> false)

    // String literals
    let pquote<'a> : Parser<_, 'a> = anyOf "\"'" |>> fun ch -> ch.ToString()

    let private toUtf8Bytes (str : string) = Encoding.UTF8.GetBytes(str)

    let pcharEscape<'a> : Parser<_, 'a> =
        pchar '\\' >>. anyOf "abfnrtv\\\'\""
            |>> fun ch ->
                    match ch with
                    | 'a' -> "\a"
                    | 'b' -> "\b"
                    | 'f' -> "\f"
                    | 'n' -> "\n"
                    | 'r' -> "\r"
                    | 't' -> "\t"
                    | 'v' -> "\v"
                    | '\\' -> "\\"
                    | '\''  -> "'"
                    | '"' -> "\""
                    | _ -> invalidArg "ch" (sprintf "Unknown character value '%A'" ch)
            |>> toUtf8Bytes

    let poctEscape<'a> : Parser<_, 'a> =
        let octToInt ch = int(ch) - int('0')
        let mapper (v : char[]) =
            if v.[0] > '3' then invalidArg "v.[0]" "Octal escape to big"
            (octToInt v.[0]) * 64 + (octToInt v.[1]) * 8 + (octToInt v.[2]) |> byte |> fun p -> [| p |]
        pchar '\\' >>. parray 3 poctalDigit |>> mapper


    let phexEscape<'a> : Parser<_, 'a> =
        let hexToInt ch =
            match ch with
            | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> int(ch) - int('0')
            | 'A' | 'B' | 'C' | 'D' | 'E' | 'F'  -> int(ch) - int('A') + 10
            | 'a' | 'b' | 'c' | 'd' | 'e' | 'f'  -> int(ch) - int('a') + 10
            | _ -> invalidArg "ch" (sprintf "Unknown character value '%A'" ch)
        let mapper (v : char[]) =
            (hexToInt v.[0]) * 16 + (hexToInt v.[1]) |> byte |> fun p -> [| p |]
        pchar '\\' >>. anyOf "xX" >>. parray 2 phexDigit |>> mapper


    let pcharValue<'a> : Parser<_, 'a> =
        let simpleChar =
            noneOf "\0\n\\\"'" |>> (fun p -> p.ToString()) |>> toUtf8Bytes
        attempt phexEscape <|> attempt poctEscape <|> attempt pcharEscape <|> simpleChar

    let pstrLit<'a> : Parser<_, 'a> =
        let joinArrays arr =
            let farr = arr |> List.fold (fun (st : ResizeArray<_>) v -> st.AddRange(v); st ) (ResizeArray<_>())
            Encoding.UTF8.GetString(farr.ToArray())

        let squote1 = pchar '\'' >>. many (attempt pcharValue) .>> pchar '\'' |>> fun s -> {Quote = SingleQuote; Value = joinArrays(s) }
        let squote2 = pchar '"' >>. many (attempt pcharValue) .>> pchar '"' |>> fun s -> {Quote = DoubleQuote; Value = joinArrays(s) }
        squote1 <|> squote2

    // EmptyStatement
    let pemptyStatement<'a> : Parser<_, 'a> =  pstring ";"

    let private ws<'a> : Parser<_, 'a> = spaces

    // Constant
    let pconstant<'a> : Parser<_, 'a> =
        attempt (pboolLit |>> BoolConst)
        <|> attempt (psign .>> ws .>>. pfloatLit |>> FloatConst)
        <|> attempt (psign .>> ws .>>. pintLit |>> IntConst)
        <|> attempt (pstrLit |>> StrConst)
        <|> (pfullIdent |>> IdentConst)

    // Syntax
    let psyntax<'a> : Parser<_, 'a> =
        pstring "syntax"
        .>> ws
        .>> pchar '='
        .>> ws
        >>. (between (pstring "\"") (pstring "\"") (pstring "proto3" |>> fun _ -> 3))
        .>> ws
        .>> pchar ';'

    let pimport<'a> : Parser<_, 'a> =
        let pkind : Parser<_, 'a> =
            opt (pstring "weak" <|> (pstring "public"))
            |>> fun p -> p |> Option.map (fun t -> if t = "weak" then WeakImport else PublicImport)
        pstring "import"
        .>> ws
        >>. pkind
        .>> ws
        .>>. pstrLit
        .>> ws
        .>> pchar ';'
        |>> fun (a, b) -> { ImportStatement.Kind = a; Path = b }

    let ppackage<'a> : Parser<_, 'a> =
        pstring "package"
        .>> ws
        >>. pfullIdent
        .>> ws
        .>> pchar ';'
*)
