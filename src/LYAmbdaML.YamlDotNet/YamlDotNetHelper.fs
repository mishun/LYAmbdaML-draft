module internal YamlDotNet.FSharp.YamlDotNetHelper

open System
open YamlDotNet.Core


let nullScalar = Events.Scalar(YamlDotNet.FSharp.EventPatterns.nullTag, "null")
let valueLiteral = "value"
let valueScalar = Events.Scalar(valueLiteral)
let mappingStart = Events.MappingStart()
let mappingEnd = Events.MappingEnd()
let seqStart = Events.SequenceStart(AnchorName.Empty, TagName.Empty, false, Events.SequenceStyle.Any)
let seqEnd = Events.SequenceEnd()
let docStart = Events.DocumentStart()


type IEmitter with
    member emitter.EmitDocumentStart () = emitter.Emit docStart
    member emitter.EmitSequenceStart () = emitter.Emit seqStart
    member emitter.EmitSequenceEnd () = emitter.Emit seqEnd
    member emitter.EmitMappingStart () = emitter.Emit mappingStart
    member emitter.EmitMappingEnd () = emitter.Emit mappingEnd
    member emitter.EmitNull () = emitter.Emit nullScalar
    member emitter.EmitScalar value =
        emitter.Emit(Events.Scalar(value))
