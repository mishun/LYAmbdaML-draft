namespace YamlDotNet.FSharp

open System


module EventPatterns =
    open YamlDotNet.Core
    open YamlDotNet.Core.Events


    let (|Scalar|MappingStart|MappingEnd|Comment|YAnchorAlias|) (event : ParsingEvent) =
        match event with
        | :? Scalar as scalar -> Scalar
        | :? MappingStart as mapStart -> MappingStart
        | :? MappingEnd as mapEnd -> MappingEnd
        | :? SequenceStart as seqStart -> failwithf ""
        | :? SequenceEnd as seqEnd -> failwithf ""
        | :? Comment as comment -> Comment comment
        | :? AnchorAlias as alias -> YAnchorAlias alias
        | _ -> failwithf ""


    let nullTag = TagName("tag:yaml.org,2002:null")


    [<return: Struct>]
    let (|YamlNull|_|) (event : ParsingEvent) =
        match event with
        | :? NodeEvent as nodeEvent when nodeEvent.Tag = nullTag -> printfn "-------------------------" ; ValueSome YamlNull
        | :? Scalar as scalar when scalar.Style = ScalarStyle.Plain && not scalar.IsKey ->
            match scalar.Value with
            | "" | "~" | "null" | "Null" |"NULL" -> ValueSome YamlNull
            | _ -> ValueNone
        | _ -> ValueNone


module NodePatterns =
    open YamlDotNet.RepresentationModel

    let (|YamlScalar|YamlMapping|YamlSequence|) (node : YamlNode) =
        match node with
        | :? YamlScalarNode as scalar -> YamlScalar scalar
        | :? YamlMappingNode as map -> YamlMapping map
        | :? YamlSequenceNode as seq -> YamlSequence seq
//        | :? YamlAliasNode as alias -> YamlAlias
        | _ -> failwith ""

