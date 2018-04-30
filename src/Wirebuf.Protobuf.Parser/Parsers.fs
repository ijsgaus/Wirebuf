namespace Wirebuf.Protobuf.Parser

open System
open System.Text
open FParsec
open Wirebuf.Ast

[<AutoOpen>]
module Parsers =
    // Letters and digits
    let letter<'a>  : Parser<_, 'a> = asciiLetter
    let decimalDigit<'a> : Parser<_, 'a> = anyOf "0123456789"
    let octalDigit<'a> : Parser<_, 'a> = anyOf "01234567"
    let hexDigit<'a> : Parser<_, 'a> = anyOf "0123456789ABCDEFabcdef"

    // Identifiers
    let ident<'a> : Parser<_, 'a> =
        letter .>>. (many (letter <|> decimalDigit <|> (pchar '_')))
        |>> fun (a, b) ->
                let sb = StringBuilder()
                sb.Append(a) |> ignore
                b |> Seq.fold (fun (st : StringBuilder) p -> st.Append p) sb |> ignore
                sb.ToString() |> Identifier.CreateUnchecked

    let private  inOneId (a, b) =  List.append [a] b
    let private  inOneIdLst (a, b) =  List.append a [b]
    let private chArrToStr a =
        let folder (sb : StringBuilder) (ch : char)  = sb.Append(ch)
        a |> Seq.fold folder (StringBuilder ()) |> fun sb -> sb.ToString()

    let fullIdent<'a> : Parser<_, 'a> =
        let tail = pchar '.' >>. ident
        (ident .>>. many tail) |>> (inOneId >> QualifiedIdentifier.CreateUnchecked)

    let messageName<'a> : Parser<_, 'a>  = ident
    let enumName<'a> : Parser<_, 'a> = ident
    let fieldName<'a> : Parser<_, 'a> = ident
    let oneofName<'a> : Parser<_, 'a> = ident
    let mapName<'a> : Parser<_, 'a> = ident
    let serviceName<'a> : Parser<_, 'a> = ident
    let rpcName<'a> : Parser<_, 'a> = ident

    let messageType<'a> : Parser<_, 'a> =
        opt (pchar '.') >>. (many ident) .>>. messageName |>> (inOneIdLst >> QualifiedIdentifier.CreateUnchecked)

    let enumType<'a> : Parser<_, 'a> =
        opt (pchar '.') >>. (many ident) .>>. enumName |>> (inOneIdLst >> QualifiedIdentifier.CreateUnchecked)

    // Integer literals

    let decimalLit<'a> : Parser<_, 'a> =
        anyOf "123456789" .>>. many decimalDigit |>> inOneId |>> chArrToStr |>> DecLit
    let octalLit<'a> : Parser<_, 'a> =
            pchar '0' .>>. many octalDigit |>> inOneId |>> chArrToStr |>> OctalLit
    let hexLit<'a> : Parser<_, 'a> =
        pchar '0' >>. anyOf "xX" >>. hexDigit .>>. many hexDigit  |>> inOneId |>> chArrToStr |>> HexLit
    let intLit<'a> : Parser<_, 'a> =
        attempt hexLit <|> attempt octalLit <|> decimalLit

    // Floating-point literals
    let decimals<'a> : Parser<_, 'a> =
        decimalDigit .>>. many decimalDigit |>> inOneId |>> chArrToStr
    let exponent<'a> : Parser<_, 'a> =
        let mapper (sign, digits) =
            {
                Sign = sign |> Option.map (fun p -> if p = '-' then Minus else Plus) |> Option.defaultValue Plus
                Digits = digits
            }
        anyOf "eE" >>. opt (anyOf "+-") .>>. decimals |>> mapper
    let floatLit<'a> : Parser<_, 'a> =
        let pinf : Parser<_, 'a> = pstring "inf" |>> fun _ -> Inf
        let pnan : Parser<_, 'a> = pstring "nan" |>> fun _ -> Nan
        let pf1 : Parser<_, 'a> =
            let mapper ((d1, d2), exp) =
                FloatLit(d1 + "." + Option.defaultValue "" d2, exp)
            decimals .>> pchar '.' .>>. opt decimals .>>. opt exponent |>> mapper
        let pf2 : Parser<_, 'a> =
            decimals .>>. exponent |>> fun (a, b) -> FloatLit(a, Some b)
        let pf3 : Parser<_, 'a> =
            pchar '.' >>. decimals .>>. opt exponent |>> fun (a, b) -> FloatLit ("." + a, b)
        let pf : Parser<_, 'a> =
            attempt pf1 <|> attempt pf2 <|> pf3
        attempt pf <|> pinf <|> pnan
    //floatLit = ( decimals "." [ decimals ] [ exponent ] | decimals exponent | "."decimals [ exponent ] ) | "inf" | "nan"

