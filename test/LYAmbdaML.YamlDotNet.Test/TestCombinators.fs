module LYAmbdaML.Test.TestCombinators

open System
open Expecto
open FsCheck
open YamlDotNet.Core
open YamlDotNet.Serialization
open YamlDotNet.FSharp.Combinators
open YamlDotNet.FSharp.Serialization


module Example1 =
    type EnumExample =
        | One = 1
        | Two = 2
        | Three = 3

    type UnionData<'t> =
        | One
        | Two of TwoParam: 't
        | Three of Item1: string * Item2: 't

    type SubData<'t> = {
        Arr : int[]
        Top : 't
        Voption : int voption
        opt : string option
        en : EnumExample
        //ud: UnionData<int>
    } with
        static member fromYAML =
            ofMapping (yamlMapP {
                let! arr = ygetWith (Parsers.array Parsers.int32) "Arr"
                let! top = yget "Top"
                let! en = yget "en"
                let! opt = yget "opt"
                let! vopt = yget "Voption"
                //let! ud = yget "ud"
                return { Arr = arr ; Top = top ; Voption = vopt ; opt = opt ; en = en (* ; ud = ud *) }
            })

        static member toYAML (sd : SubData<string>) =
            ymapping (seq {
                ypair "Arr" sd.Arr
                ypair "Top" sd.Top
                ypair "en" sd.en
                ypair "opt" sd.opt
                ypair "Voption" sd.Voption
            })

    type Data = {
        [<YamlMember(Alias = "name")>] Name : string
        Count : int
        Ok : bool
        Subs : SubData<string>[]
        IntList : int list
        [<YamlMember(Alias = "int-set")>] IntSet : Set<int>
    } with
        static member fromYAML =
            ofMapping (yamlMapP {
                let! name = yget "name"
                let! count = yget "Count"
                let! ok = yget "Ok"
                let! subs = ygetWith (Parsers.array Defaults.defaultParser<SubData<string>>) "Subs"
                let! intList = yget "IntList"
                let! intSet = yget "int-set"
                return { Name = name ; Count = count ; Ok = ok ; Subs = subs ; IntList = List.ofArray intList ; IntSet = Set.ofArray intSet }
            })

        static member toYAML (d : Data) =
            ymapping (seq {
                ypair "name" d.Name
                ypair "Count" d.Count
                ypair "Ok" d.Ok
                ypair "Subs" d.Subs
                ypair "IntList" d.IntList
                ypair "int-set" d.IntSet
            })


    let yamlText ="""
Count: 69
Ok: True
Subs:
- &anchor
  Voption: null
  Arr:
    - 0
    - 1
    - 2
  opt: null
  Top: test
  en: Two
  ud:
      Three:
        Item1: hello
        Item2: 69
- Arr: [69]
  Top: test2 or test3
  opt: { value: 'Hello, world' }
  en: One
  ud: One
  Voption:
    value: 31337
- *anchor

IntList: [1, 2, 3, 4]
int-set:
  - 69
  - 31337
  - 256
name: This is a name
"""


let private deserializer =
    DeserializerBuilder()
        .WithFSharpTypes()
        .Build()

let private serializer =
    SerializerBuilder()
        .WithFSharpTypes()
        // .JsonCompatible()
        .WithEventEmitter((fun inner -> new EventEmitters.JsonEventEmitter(inner, YamlFormatter.Default)), (fun loc -> loc.InsteadOf<EventEmitters.TypeAssigningEventEmitter>()))
        .Build()


let customCheck () =
    try
        let res = deserializer.Deserialize<Example1.Data> Example1.yamlText
        printfn "Result = %A" res

        let back = serializer.Serialize(res)
        Console.ForegroundColor <- ConsoleColor.Red
        printfn "Serialization result:"
        Console.ResetColor()
        printfn "%s" back
    with :? YamlException as ex ->
        printfn "Error at [%A --- %A]\n%A" ex.Start ex.End ex.InnerException

    // let dom =
    //     use reader = new System.IO.StringReader(yaml)
    //     let parser = Parser(reader)
    //     YamlDotNet.FSharp.DOM.parseStream parser
    // printfn "DOM: %A\n" dom

    // let written =
    //     let writer = new System.IO.StringWriter()
    //     let emitter = Emitter(writer)
    //     YamlDotNet.FSharp.DOM.emitStream(emitter, dom)
    //     writer.ToString()
    // printfn "Emitted: %s" written

    try
        //let parser = Defaults.defaultParser<Example1.Data>
        let parserResult = ofYamlText Example1.yamlText : YamlResult<Example1.Data>

        Console.ForegroundColor <- ConsoleColor.Red
        printfn "Combinator parser result:"
        Console.ResetColor()
        printfn "%A" parserResult

        match parserResult with
        | Error _ -> ()
        | Ok res ->
            let emitted = toYamlText res
            Console.ForegroundColor <- ConsoleColor.Red
            printfn "Combinator emitter result:"
            Console.ResetColor()
            printfn "%s" emitted
    with ex ->
        printfn "Exception: %A" ex.Message


[<Tests>]
let test () =
    testList "Combinators tests" [
        testProperty "roundtrip with combinators / Example1" (fun (src : Example1.Data) ->
            let yaml = toYamlText src
            let parseResult = ofYamlText yaml : YamlResult<Example1.Data>
            (Ok src = parseResult) |@ sprintf "before =\n%A\nafter =\n%A\nyaml =\n%s\n" src parseResult yaml
        )

        // testProperty "serialize with combinators + deserialize auto / Example1" (fun (src : Example1.Data) ->
        //     let yaml = toYamlText src
        //     try
        //         let dst = deserializer.Deserialize<Example1.Data> yaml
        //         (src = dst) |@ sprintf "before =\n%A\nafter =\n%A\nyaml =\n%s\n" src dst yaml
        //     with ex ->
        //         false |@ sprintf "Exception %A at:\n%s" ex yaml
        // )

        testCase "Auto Serialize/Deserialize" (fun () ->
            let autoRes = deserializer.Deserialize<Example1.Data> Example1.yamlText
            let autoYaml = serializer.Serialize(autoRes)
            let autoRes' = deserializer.Deserialize<Example1.Data> autoYaml
            Expect.equal autoRes' autoRes $"YAML=\n{autoYaml}\n"

            let combiRes = match ofYamlText Example1.yamlText : YamlResult<Example1.Data> with | Ok res -> res | Error msg -> failwith (msg + "\n" + Example1.yamlText)
            let combiYaml = toYamlText combiRes
            Expect.equal combiRes autoRes $"YAML=\n{combiYaml}\n"
        )
    ]
