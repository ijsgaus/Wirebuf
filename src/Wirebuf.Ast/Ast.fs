namespace Wirebuf.Ast
open System

type IAstNode<'nt when 'nt : enum<int>> =
    abstract Length : uint32
    abstract NodeType : 'nt
    abstract ToSource : unit -> string



type WhitespaceType =
    | Spaces = 1
    | Tabs = 2
    | NewLines = 3

type IWhitespaceNode<'nt when 'nt : enum<int>> =
    inherit IAstNode<'nt>
    abstract WhitespaceType : WhitespaceType

type AstNodePrefix<'nt when 'nt : enum<int>> = IWhitespaceNode<'nt> seq

type IAstComplexNode<'nt when 'nt : enum<int>> =
    inherit IAstNode<'nt>
    abstract Prefix : AstNodePrefix<'nt>
    abstract Nodes : IAstNode<'nt> seq

module AstNode =
    open System.Text

    let length<'nt, 't when 't :> IAstNode<'nt>> (n : 't) = n.Length
    let nodeType<'nt, 't when 't :> IAstNode<'nt>> (n : 't) = n.NodeType
    let toSource<'nt, 't when 't :> IAstNode<'nt>> (n : 't) = n.ToSource()
    let prefix<'nt, 't when 't :> IAstComplexNode<'nt>> (n : 't) = n.Prefix
    let nodes<'nt, 't when 't :> IAstComplexNode<'nt>> (n : 't) = n.Nodes
    let complexLength n = nodes n |> Seq.sumBy length
    let complexSource n =
        nodes n
        |> Seq.fold (fun (st : StringBuilder) n1 -> st.Append(toSource n1)) (StringBuilder())
        |> fun p -> p.ToString()


[<AbstractClass>]
type ComplexNode<'nt when 'nt : enum<int>>(items : IAstNode<'nt> seq) =
    member __.Nodes = items |> List.ofSeq
    abstract NodeType : 'nt
    interface IAstComplexNode<'nt> with
        member __.Prefix =
            items
            |> Seq.takeWhile (fun p -> p :? IWhitespaceNode<'nt>)
            |> Seq.map (fun p -> p :?> IWhitespaceNode<'nt>)
        member __.Nodes = __.Nodes |> Seq.ofList
        member __.NodeType = __.NodeType
        member __.Length = AstNode.complexLength __
        member __.ToSource() = AstNode.complexSource __
    override __.GetHashCode() = __.Nodes.GetHashCode()
    override __.Equals other =
        if other = null
        then false
        else
            if __.GetType() <> other.GetType()
            then false
            else __.Nodes = (other :?> ComplexNode<'nt>).Nodes






