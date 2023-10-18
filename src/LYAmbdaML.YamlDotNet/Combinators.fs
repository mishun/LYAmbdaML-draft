module YamlDotNet.FSharp.Combinators

open System
open System.Collections.Generic
open YamlDotNet
open YamlDotNet.FSharp.DOM


type YamlResult<'r> = Result<'r, string>

[<Struct>]
type YamlP<'r> = {
    Run : YamlNode -> YamlResult<'r>
}

[<Struct>]
type YamlMapP<'r> = {
    RunM : IDictionary<string, YamlNode> -> YamlResult<'r>
}


type YamlParserBuilder () =
    member __.Return (x : 'a) : YamlP<'a> = { Run = fun _ -> Ok x }
    member __.Zero () : YamlP<'a> = { Run = fun _ -> Error "Zero" }
    member __.BindReturn (x : YamlP<'a>, f : 'a -> 'b) : YamlP<'b> = { Run = x.Run >> Result.map f }

    member __.Bind (x : YamlP<'a>, f : 'a -> YamlP<'b>) : YamlP<'b> =
        let get yaml =
            match x.Run yaml with
            | Error err -> Error err
            | Ok x -> (f x).Run yaml
        { Run = get }

    member __.For (args : seq<'a>, f : 'a -> YamlP<'b>) : YamlP<'b seq> =
        let get yaml =
            let list = List<_>()
            let enumerator = args.GetEnumerator()
            let rec go () =
                if enumerator.MoveNext() then
                    match (f enumerator.Current).Run yaml with
                    | Error err -> Error err
                    | Ok value -> list.Add value ; go ()
                else
                    Ok (list.ToArray() :> IEnumerable<_>)
            go ()
        { Run = get }


type YamlMappingParserBuilder () =
    member __.Return (x : 'a) : YamlMapP<'a> = { RunM = fun _ -> Ok x }
    member __.Zero () : YamlMapP<'a> = { RunM = fun _ -> Error "Zero" }
    member __.BindReturn (x : YamlMapP<'a>, f : 'a -> 'b) : YamlMapP<'b> = { RunM = x.RunM >> Result.map f }

    member __.Bind (x : YamlMapP<'a>, f : 'a -> YamlMapP<'b>) : YamlMapP<'b> =
        let get yaml =
            match x.RunM yaml with
            | Error err -> Error err
            | Ok x -> (f x).RunM yaml
        { RunM = get }


let yamlP = YamlParserBuilder()
let yamlMapP = YamlMappingParserBuilder()


let ofScalar (parse : string voption -> string -> YamlResult<'a>) =
    let get yaml =
        match yaml with
        | YamlNode.Scalar({ Tag = tag }, { Value = str }) ->
            if tag.IsEmpty then
                parse ValueNone str
            else
                parse (ValueSome tag.Value) str
        | _ -> Error "scalar expected"
    { Run = get }


let ofMapping (parser : YamlMapP<'a>) : YamlP<'a> =
    let get yaml =
        match yaml with
        | YamlNode.Mapping(_, _, children) ->
            parser.RunM (dict (seq {
                for KeyValue(k, v) in children do
                    match k with
                    | YamlNode.Scalar(_, s) -> yield (s.Value, v)
                    | _ -> ()
            }))
        | _ -> Error "mapping expected"
    { Run = get }


let ychoice (parsers : YamlP<'a> seq) : YamlP<'a> =
    let get yaml =
        let enumerator = parsers.GetEnumerator()
        let rec go () =
            if enumerator.MoveNext() then
                match enumerator.Current.Run yaml with
                | Ok result -> Ok result
                | Error _ -> go ()
            else
                Error "ychoice: not a single parser matched"
        go()
    { Run = get }


let ygetWith (parser : YamlP<'a>) (key : string) : YamlMapP<'a> =
    let get (yamls : IDictionary<_, _>) =
        match yamls.TryGetValue key with
        | true, yaml -> parser.Run yaml
        | _ -> Error (sprintf "Key '%s' not found" key)
    { RunM = get }


let ygetOptWith (parser : YamlP<'a>) (key : string) : YamlMapP<'a voption> =
    let get (yamls : IDictionary<_, _>) =
        match yamls.TryGetValue key with
        | true, yaml -> parser.Run yaml |> Result.map ValueSome
        | _ -> Ok ValueNone
    { RunM = get }


let ypairWith (emitter : 't -> YamlNode) (key : string) (value : 't) =
    struct (key, emitter value)

let ymapping (pairs : struct (string * YamlNode) seq) =
    let children = readOnlyDict (seq {
        for (key, value) in pairs ->
            let keyNode = YamlNode.Scalar({ Anchor = Core.AnchorName.Empty ; Tag = Core.TagName.Empty ; Start = Core.Mark.Empty ; End = Core.Mark.Empty }, { Value = key ; Style = Core.ScalarStyle.DoubleQuoted })
            (keyNode, value)
    })
    YamlNode.Mapping({ Anchor = Core.AnchorName.Empty ; Tag = Core.TagName.Empty ; Start = Core.Mark.Empty ; End = Core.Mark.Empty }, Core.Events.MappingStyle.Any, children)

let ysequenceWith (emitter : 't -> YamlNode) (elements : 't seq) =
    let children = [| for el in elements -> emitter el |]
    YamlNode.Sequence({ Anchor = Core.AnchorName.Empty ; Tag = Core.TagName.Empty ; Start = Core.Mark.Empty ; End = Core.Mark.Empty }, Core.Events.SequenceStyle.Any, children)


[<RequireQualifiedAccess>]
module Emitters =
    let nullLiteral =
        let tag = Core.TagName("tag:yaml.org,2002:null")
        YamlNode.Scalar({ Anchor = Core.AnchorName.Empty ; Tag = tag ; Start = Core.Mark.Empty ; End = Core.Mark.Empty }, { Value = "null" ; Style = Core.ScalarStyle.Plain })

    let private stringHeader = { Anchor = Core.AnchorName.Empty ; Tag = Core.TagName.Empty ; Start = Core.Mark.Empty ; End = Core.Mark.Empty }
    let makeString (chars : ReadOnlySpan<char>) =
        let str = String(chars)
        YamlNode.Scalar(stringHeader, { Value = str ; Style = Core.ScalarStyle.DoubleQuoted })

    let string (value : string) =
        match value with
        | null -> nullLiteral
        | _ -> YamlNode.Scalar(stringHeader, { Value = value ; Style = Core.ScalarStyle.DoubleQuoted })

    let option emitElement (value : 'a option) =
        match value with
        | Some value ->
            ymapping (seq {
                ypairWith emitElement "value" value
            })
        | None -> nullLiteral

    let voption emitElement (value : 'a voption) =
        match value with
        | ValueSome value ->
            ymapping (seq {
                ypairWith emitElement "value" value
            })
        | ValueNone -> nullLiteral

    let enum< 't, 'u when 't : enum<'u> and 't : (new : unit -> 't) and 't : struct and 't :> ValueType > (value : 't) =
        let token = value.ToString()
        string token

    let inline byTryFormat2< ^t when ^t : (member TryFormat : Span<char> -> byref<int> -> bool) > (value : ^t) =
        use buffer = Buffers.MemoryPool<char>.Shared.Rent 128
        let mutable len = 0
        if value.TryFormat(buffer.Memory.Span, &len) then
            makeString(Span.op_Implicit (buffer.Memory.Span.Slice(0, len)))
        else
            string "???"

    let inline byTryFormat4< ^t when ^t : (member TryFormat : Span<char> -> byref<int> -> ReadOnlySpan<char> -> IFormatProvider -> bool) > (value : ^t, format : string) =
        use buffer = Buffers.MemoryPool<char>.Shared.Rent 128
        let mutable len = 0
        if value.TryFormat(buffer.Memory.Span, &len, format.AsSpan(), null) then
            makeString(Span.op_Implicit(buffer.Memory.Span.Slice(0, len)))
        else
            string "???"


[<RequireQualifiedAccess>]
module Parsers =
    let array (element : YamlP<'a>) : YamlP<'a[]> =
        let get yaml =
            match yaml with
            | YamlNode.Sequence(_, _, children) ->
                let results = List<_>()
                let rec go i =
                    if i < children.Length then
                        match element.Run children.[results.Count] with
                        | Ok result -> results.Add result ; go (i + 1)
                        | Error err -> Error err
                    else
                        Ok (results.ToArray())
                go 0
            | _ -> Error "sequence expected"
        { Run = get }

    let inline byTryParse< ^a when ^a : (static member TryParse : ReadOnlySpan<char> * byref<^a> -> bool) > =
        ofScalar (fun _ token ->
            let span = token.AsSpan()
            let mutable res = Unchecked.defaultof<^a>
            if ( ^a : (static member TryParse : ReadOnlySpan<char> * byref<^a> -> bool) (span, &res)) then
                Ok res
            else
                Error (sprintf "can not parse %O from '%s'" typeof<^a> token)
        )

    let enum< 't, 'u when 't : enum<'u> and 't : (new : unit -> 't) and 't : struct and 't :> ValueType > : YamlP<'t> =
        ofScalar (fun _ token ->
            let span = token.AsSpan()
            match Enum.TryParse span with
            | true, value -> Ok value
            | _ ->
                let sb = Text.StringBuilder(sprintf "can not parse enum %O from '%s'; possible variants are" typeof<'t> token)
                for option in Enum.GetValues typeof<'t> do
                    sb.AppendFormat(" '{0}'", option) |> ignore
                Error (sb.ToString())
        )

    let string  =
        ofScalar (fun tag token ->
            match tag, token with
            | ValueSome "tag:yaml.org,2002:null", "null" -> Ok null
            | _ -> Ok token
        )

    let option parseElement : YamlP<'a option> =
        ychoice (seq {
            yamlP.Bind(string, function | null | "null" -> yamlP.Return(None) | _ -> yamlP.Zero())
            ofMapping (yamlMapP {
                let! element = ygetWith parseElement "value"
                return Some element
            })
        })

    let voption parseElement : YamlP<'a voption> =
        ychoice (seq {
            yamlP.Bind(string, function | null | "null" -> yamlP.Return(ValueNone) | _ -> yamlP.Zero())
            ofMapping (yamlMapP {
                let! element = ygetWith parseElement "value"
                return ValueSome element
            })
        })

    let bool = byTryParse<bool>
    let int8 = byTryParse<int8>
    let uint8 = byTryParse<uint8>
    let int16 = byTryParse<int16>
    let uint16 = byTryParse<uint16>
    let int32 = byTryParse<int32>
    let uint32 = byTryParse<uint32>
    let int64 = byTryParse<int64>
    let uint64 = byTryParse<uint64>
    let float32 = byTryParse<float32>
    let float64 = byTryParse<float>
    let deciaml = byTryParse<decimal>
    let bigint = byTryParse<bigint>
    let guild = byTryParse<Guid>


module Defaults =
    [<Struct; RequireQualifiedAccess>]
    type YamlTypeMark<'t> = | Mark

    type YamlDefaults =
        static member inline fromYAML_marked (_ : YamlTypeMark<string>) = Parsers.string
        static member inline fromYAML_marked (_ : YamlTypeMark<bool>) = Parsers.bool
        static member inline fromYAML_marked (_ : YamlTypeMark<int8>) = Parsers.int8
        static member inline fromYAML_marked (_ : YamlTypeMark<uint8>) = Parsers.uint8
        static member inline fromYAML_marked (_ : YamlTypeMark<int16>) = Parsers.int16
        static member inline fromYAML_marked (_ : YamlTypeMark<uint16>) = Parsers.uint16
        static member inline fromYAML_marked (_ : YamlTypeMark<int32>) = Parsers.int32
        static member inline fromYAML_marked (_ : YamlTypeMark<uint32>) = Parsers.uint32
        static member inline fromYAML_marked (_ : YamlTypeMark<int64>) = Parsers.int64
        static member inline fromYAML_marked (_ : YamlTypeMark<uint64>) = Parsers.uint64
        static member inline fromYAML_marked (_ : YamlTypeMark<float32>) = Parsers.float32
        static member inline fromYAML_marked (_ : YamlTypeMark<float>) = Parsers.float64

        static member inline fromYAML_marked< 't, 'u when 't : enum<'u> and 't : (new : unit -> 't) and 't : struct and 't :> ValueType > (_ : YamlTypeMark<'t>) : YamlP<'t> =
            Parsers.enum

        static member inline fromYAML_marked< ^t when (^t or YamlDefaults) : (static member fromYAML_marked : YamlTypeMark<^t> -> YamlP<^t>) > (_ : YamlTypeMark<'t option>) : YamlP<'t option> =
            let parseElement = ((^t or YamlDefaults) : (static member fromYAML_marked : YamlTypeMark<^t> -> YamlP<^t>) YamlTypeMark<'t>.Mark)
            Parsers.option parseElement

        static member inline fromYAML_marked< ^t when (^t or YamlDefaults) : (static member fromYAML_marked : YamlTypeMark<^t> -> YamlP<^t>) > (_ : YamlTypeMark<'t voption>) : YamlP<'t voption> =
            let parseElement = ((^t or YamlDefaults) : (static member fromYAML_marked : YamlTypeMark<^t> -> YamlP<^t>) YamlTypeMark<'t>.Mark)
            Parsers.voption parseElement

        static member inline fromYAML_marked< ^t when (^t or YamlDefaults) : (static member fromYAML_marked : YamlTypeMark<^t> -> YamlP<^t>) > (_ : YamlTypeMark<'t[]>) : YamlP<'t[]> =
            let parseElement = ((^t or YamlDefaults) : (static member fromYAML_marked : YamlTypeMark<^t> -> YamlP<^t>) YamlTypeMark<'t>.Mark)
            Parsers.array parseElement

        static member inline fromYAML_marked< ^t when ^t : (static member fromYAML : YamlP<^t>) > (_ : YamlTypeMark<^t>) : YamlP<^t> =
             (^t : (static member fromYAML : YamlP<^t>) ())


        static member inline toYAML value = Emitters.string value
        static member inline toYAML (value : bool) = Emitters.byTryFormat2 value
        static member inline toYAML (value : int8) = Emitters.byTryFormat4(value, "")
        static member inline toYAML (value : uint8) = Emitters.byTryFormat4(value, "")
        static member inline toYAML (value : int16) = Emitters.byTryFormat4(value, "")
        static member inline toYAML (value : uint16) = Emitters.byTryFormat4(value, "")
        static member inline toYAML (value : int32) = Emitters.byTryFormat4(value, "")
        static member inline toYAML (value : uint32) = Emitters.byTryFormat4(value, "")
        static member inline toYAML (value : int64) = Emitters.byTryFormat4(value, "")
        static member inline toYAML (value : uint64) = Emitters.byTryFormat4(value, "")

        static member inline toYAML< 't, 'u when 't : enum<'u> and 't : (new : unit -> 't) and 't : struct and 't :> ValueType > (value : 't) =
            Emitters.enum value

        static member inline toYAML< ^t when (^t or YamlDefaults) : (static member toYAML : ^t -> YamlNode) > (value : ^t option) =
            let emitElement el = ((^t or YamlDefaults) : (static member toYAML : ^t -> YamlNode) el)
            Emitters.option emitElement value

        static member inline toYAML< ^t when (^t or YamlDefaults) : (static member toYAML : ^t -> YamlNode) > (value : ^t voption) =
            let emitElement el = ((^t or YamlDefaults) : (static member toYAML : ^t -> YamlNode) el)
            Emitters.voption emitElement value

        static member inline toYAML< ^t when (^t or YamlDefaults) : (static member toYAML : ^t -> YamlNode) > (value : ^t[]) =
            let emitElement el = ((^t or YamlDefaults) : (static member toYAML : ^t -> YamlNode) el)
            ysequenceWith emitElement value

        static member inline toYAML< ^t when (^t or YamlDefaults) : (static member toYAML : ^t -> YamlNode) > (value : ^t list) =
            let emitElement el = ((^t or YamlDefaults) : (static member toYAML : ^t -> YamlNode) el)
            ysequenceWith emitElement value

        static member inline toYAML< ^t when ^t : comparison and (^t or YamlDefaults) : (static member toYAML : ^t -> YamlNode) > (value : Set<^t>) =
            let emitElement el = ((^t or YamlDefaults) : (static member toYAML : ^t -> YamlNode) el)
            ysequenceWith emitElement value


    let inline defaultParser< ^t when (^t or YamlDefaults) : (static member fromYAML_marked : YamlTypeMark<^t> -> YamlP<^t>) > =
        ((^t or YamlDefaults) : (static member fromYAML_marked : YamlTypeMark<^t> -> YamlP<^t>) YamlTypeMark<^t>.Mark)

    let inline defaultEmitter< ^t when (^t or YamlDefaults) : (static member toYAML : ^t -> YamlNode) > (value : ^t) =
        ((^t or YamlDefaults) : (static member toYAML : ^t -> YamlNode) value)


let inline yget key = ygetWith Defaults.defaultParser key
let inline ygetOpt key = ygetOptWith Defaults.defaultParser key
let inline ypair key value = ypairWith Defaults.defaultEmitter key value
let inline ysequence value = ysequenceWith Defaults.defaultEmitter value


let parseYamlText (parser : YamlP<'a>) (text : string) =
    try
        use reader = new IO.StringReader(text)
        let p = Core.Parser(reader)
        match parseStream p with
        | [| dom |] -> parser.Run dom
        | _ -> Error "Bad number of documents"
    with :? Core.YamlException as ex ->
        Error (sprintf "YAML format error: %A" ex)

let inline ofYamlText text = parseYamlText Defaults.defaultParser text

let inline toYamlText value =
    use writer = new IO.StringWriter()
    let emitter = Core.Emitter(writer)
    emitStream(emitter, [| Defaults.defaultEmitter value |])
    writer.Flush()
    writer.ToString()
