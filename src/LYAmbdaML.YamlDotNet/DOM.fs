module YamlDotNet.FSharp.DOM

open System
open System.Collections.Generic
open System.Collections.Immutable
open YamlDotNet.Core
open YamlDotNet.FSharp.YamlDotNetHelper


type YamlCommon = {
    Anchor : AnchorName
    Tag : TagName
    Start : Mark
    End : Mark
}

type YamlScalar = {
    Value : string
    Style : ScalarStyle
}

[<RequireQualifiedAccess>]
type YamlNode =
    | Scalar of YamlCommon * YamlScalar
    | Mapping of YamlCommon * Events.MappingStyle * IReadOnlyDictionary<YamlNode, YamlNode>
    | Sequence of YamlCommon * Events.SequenceStyle * YamlNode[]
    | Alias of AnchorName


type DocumentLoadingState () =
    let anchors = Dictionary<AnchorName, YamlNode>()
    let nodesWithUnresolvedAliases = new List<YamlNode>()


    member __.OfferAnchor (anchor : AnchorName, node : YamlNode) =
        if not anchor.IsEmpty then
            if anchors.TryAdd(anchor, node) |> not then
                anchors.[anchor] <- node

    member __.GetNode (anchor, start : Mark, finish : Mark) =
        match anchors.TryGetValue(anchor) with
        | true, target -> target
        | _ -> AnchorNotFoundException(&start, &finish, $"The anchor '{anchor}' does not exists") |> raise

    member __.TryGetNode (anchor) =
        match anchors.TryGetValue(anchor) with
        | true, node -> ValueSome node
        | _ -> ValueNone

    member __.AddNodeWithUnresolvedAliases (node) =
        nodesWithUnresolvedAliases.Add(node)

//    member this.ResolveAliases () =
//        for node in nodesWithUnresolvedAliases do
//            node.ResolveAliases(this)



let parseCommon (event : Events.NodeEvent) = {
    Anchor = event.Anchor
    Tag = event.Tag
    Start = event.Start
    End = event.End
}

let rec parseNode (parser : IParser, ctx : DocumentLoadingState) =
    let token = parser.Current
    parser.MoveNext() |> ignore
    match token with
    | :? Events.Scalar as scalar ->
        let node = YamlNode.Scalar(parseCommon scalar, { Value = scalar.Value ; Style = scalar.Style })
        ctx.OfferAnchor(scalar.Anchor, node)
        node

    | :? Events.SequenceStart as sequenceStart ->
        let node = parseSequence(parser, sequenceStart, ctx)
        ctx.OfferAnchor(sequenceStart.Anchor, node)
        node

    | :? Events.MappingStart as mapping ->
        let node = parseMapping(parser, mapping, ctx)
        ctx.OfferAnchor(mapping.Anchor, node)
        node

    | :? Events.AnchorAlias as alias ->
        match ctx.TryGetNode alias.Value with
        | ValueSome node -> node
        | ValueNone -> YamlNode.Alias alias.Value

    | eventType ->
        ArgumentException($"The current event is of an unsupported type {eventType}", nameof(parser)) |> raise

and parseSequence (parser : IParser, sequenceStart : Events.SequenceStart, ctx : DocumentLoadingState) =
    let style = sequenceStart.Style

    let mutable hasUnresolvedAliases = false
    let mutable sequenceEnd = null
    let children = [|
        while not (parser.TryConsume<Events.SequenceEnd>(&sequenceEnd)) do
            match parseNode(parser, ctx) with
            | YamlNode.Alias _ as child ->
                hasUnresolvedAliases <- true
                yield child
            | child -> yield child
    |]

    let node = YamlNode.Sequence({ Anchor = sequenceStart.Anchor ; Tag = sequenceStart.Tag ; Start = sequenceStart.Start ; End = sequenceEnd.End }, style, children)
    if hasUnresolvedAliases then
        ctx.AddNodeWithUnresolvedAliases node
    node

and parseMapping (parser : IParser, mappingStart : Events.MappingStart, ctx : DocumentLoadingState) =
    let common = parseCommon mappingStart
    let style = mappingStart.Style

    let builder = ImmutableDictionary.CreateBuilder()
    let mutable hasUnresolvedAliases = false
    let mutable mappingEnd = null
    while not (parser.TryConsume<Events.MappingEnd>(&mappingEnd)) do
        let key = parseNode(parser, ctx)
        let value = parseNode(parser, ctx)
        if builder.TryAdd(key, value) |> not then
            //YamlException(key.Start, key.End, "Duplicate key", err) |> raise
            failwith "Duplicate key"
        //hasUnresolvedAliases |= key is YamlAliasNode || value is YamlAliasNode;

    let node = YamlNode.Mapping(common, style, builder.ToImmutable())
    if hasUnresolvedAliases then
        ctx.AddNodeWithUnresolvedAliases node
    node


let parseDocument (parser : IParser) =
    let state = DocumentLoadingState()
    parser.Consume<Events.DocumentStart>() |> ignore
    let rootNode = parseNode(parser, state)
    parser.Consume<Events.DocumentEnd>() |> ignore
    //state.ResolveAliases();
    rootNode


let parseStream (parser : IParser) =
    parser.Consume<Events.StreamStart>() |> ignore
    let mutable streamEnd = null
    let documents = [|
        while parser.TryConsume<Events.StreamEnd>(&streamEnd) |> not do
            yield parseDocument parser
    |]
    documents


let rec emitNode (emitter : IEmitter, node : YamlNode) =
    match node with
    | YamlNode.Scalar(common, scalar) ->
        emitter.Emit(Events.Scalar(common.Anchor, common.Tag, scalar.Value, scalar.Style, common.Tag.IsEmpty, false))

    | YamlNode.Mapping(common, style, map) ->
        //emitter.Emit(Events.MappingStart(common.Anchor, common.Tag, true, style))
        emitter.EmitMappingStart()
        for KeyValue(key, value) in map do
            emitNode(emitter, key)
            emitNode(emitter, value)
        emitter.EmitMappingEnd()

    | YamlNode.Sequence(common, style, sequence) ->
        //emitter.Emit(Events.SequenceStart(common.Anchor, common.Tag, common.Tag.IsEmpty, style))
        emitter.Emit(Events.SequenceStart(AnchorName.Empty, TagName.Empty, false, Events.SequenceStyle.Any))
        for node in sequence do
            emitNode(emitter, node)
        emitter.EmitSequenceEnd()

    | YamlNode.Alias alias ->
        NotSupportedException("A YamlAliasNode is an implementation detail and should never be saved.") |> raise

let emitDocument (emitter : IEmitter, node : YamlNode) =
    emitter.EmitDocumentStart()
    emitNode(emitter, node) // new EmitterState())
    emitter.Emit(Events.DocumentEnd(false))

let emitStream (emitter : IEmitter, documents : _ seq) =
    emitter.Emit(Events.StreamStart())
    for document in documents do
        emitDocument(emitter, document)
    emitter.Emit(Events.StreamEnd())
