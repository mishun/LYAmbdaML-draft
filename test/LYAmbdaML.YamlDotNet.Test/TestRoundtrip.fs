module LYAmbdaML.Test.TestRoundtrip

open System
open Expecto
open FsCheck


type TestNameAttribute (name: string) =
    inherit Attribute()

    member __.Name = name


[<TestName("Standard record with a simple C# list")>]
type TestRecordOne = {
    One: string
    Two: int
    Three: string[]
    Four: string option
}

[<TestName("All optional fields")>]
type TestRecordTwo = { TwoOne: string option; TwoTwo: int option }

[<TestName("Record CLIMutable type"); CLIMutable>]
type TestRecordThree = { Three: string; Four: int }

[<TestName("Record with mutable field")>]
type TestRecordFour = {
    mutable Flag : bool
    String : string
}

[<Struct; TestName("Struct Record")>]
type TestRecordFive = {
    Flag : bool
    String : string
}

type TestEnum =
    | OptionA = 1uy
    | OptionB = 2uy
    | OptionC = 69uy

[<TestName("Record with internal body")>]
type TestRecordSix = internal {
    Field : struct (int * bool * int64)
    Number : int64
    DecimalNumber : decimal
    EnumField : TestEnum
    String : string
    Date : DateTime
}

[<Struct; TestName("Struct Record with weird field names")>]
type TestRecordSeven = {
    ``__$uperF!eld__`` : bool
    ``String#`` : string
}

type InnerNestedRecordWithCollections = {
    ArrayData: int array
    StringData: string
}

[<TestName("Struct nested record with arrays and strings")>]
type NestedRecordWithZeroValues = {
    NestedCollectedData: InnerNestedRecordWithCollections array
    NestedData: InnerNestedRecordWithCollections
    Data: int array
    Name: string
}

[<Struct; TestName("Struct record with primitive collections")>]
type StructRecordWithCollectionTestCases = {
    TextCollection: string array
    Data: int array
    Name: string
}

[<Struct; TestName("Struct record with inner complex types")>]
type StructRecordWithNestedTypes = {
    DataCollection: InnerNestedRecordWithCollections array
    Data: InnerNestedRecordWithCollections
}

[<Struct; TestName("Struct record with struct collection type")>]
type StructRecordWithNestedStructCollectionTypes = {
    StructDataCollection: StructRecordWithCollectionTestCases array
    Data: InnerNestedRecordWithCollections
}

[<Struct; TestName("Struct record with 2 generic arguments")>]
type StructWith2GenericArs<'t, 'r> = {
    Count : int
    Data : 't[]
    Data2 : 'r
}

// type CoreExample<'t> = {
//     Value : 't[]
// }


module ExampleTypesInsideModule = 
    [<RequireQualifiedAccess; TestName("Single case DU")>]
    type UnionOne = | One

    [<RequireQualifiedAccess; TestName("Multi case DU, No Fields")>]
    type UnionTwo = | One | Two

    [<RequireQualifiedAccess; TestName("One has string")>]
    type UnionThree = | One | Two of string

    [<RequireQualifiedAccess; TestName("All have fields")>]
    type UnionFour = | One of int | Two of string

    [<RequireQualifiedAccess; TestName("More than one field per case")>]
    type UnionFive = | One of int | Two of test1: string * test2: int

    [<RequireQualifiedAccess; TestName("More than one field per case; has array type")>]
    type UnionSix = | One of int | Two of test1: string * test2: int array

    [<RequireQualifiedAccess; TestName("More than one field per case; has array type and option type")>]
    type UnionSeven = | One of int option | Two of test1: int option * test2: int array

    [<RequireQualifiedAccess; TestName("Single case union with data")>]
    type UnionEight = | One of int option * two: int array

    [<TestName("Union with generic; two cases")>]
    type SerialisableOption<'t> = 
        | SerialisableSome of 't
        | SerialisableNone

    [<TestName("Union with generic; single case union")>]
    type Wrapper<'t> = | Wrapper of 't

    [<TestName("More than 4 cases; one case with no fields")>]
    type UnionNine = 
    | CaseOne of numbers: int array // If any of the above show it.
    | CaseTwo of strings: string array
    | CaseThreee of singleData: string
    | CaseFour

    [<Struct; TestName("Value union with no data inside")>]
    type ValueUnionNoData =
    | CaseOne
    | CaseTwo
    | CaseThreee

    [<Struct; TestName("Value union with no data inside")>]
    type ValueUnionWithData =
        | CaseOne
        | CaseTwo of string
        | CaseThreee of int * UnionSeven


module TestCase =
    open FsCheck
    open YamlDotNet.Serialization
    open YamlDotNet.FSharp.Serialization

    // F# does not allow nulls although FsCheck tries to stress C# interoperability.
    // Disabling it here because this library is for wrapping F# types only.
    type DataGenerator =
        static member Generate() : Arbitrary<string[]> =
            Gen.oneof ([ "One"; "Two"; "" ] |> List.map Gen.constant)
            |> Gen.listOf
            |> Gen.map List.toArray
            |> Arb.fromGen

        static member GenerateNonNullString() : Arbitrary<string> =
            Arb.Default.StringWithoutNullChars().Generator |> Gen.map (fun x -> x.Get) |> Gen.filter (box >> Operators.isNull >> not) |> Arb.fromGen

    let private fsCheckConfig = {
        FsCheckConfig.defaultConfig with
            maxTest = 1000
            //arbitrary = [ typeof<DataGenerator> ]
    }

    let private deserializer =
        DeserializerBuilder()
            .WithFSharpTypes()
            .Build()

    let private serializer =
        SerializerBuilder()
            .WithFSharpTypes()
            .WithDefaultScalarStyle(YamlDotNet.Core.ScalarStyle.DoubleQuoted) //.WithQuotingNecessaryStrings()
            // .WithEventEmitter((fun inner -> new JsonEventEmitter(inner, YamlFormatter.Default)), (fun loc -> loc.InsteadOf<TypeAssigningEventEmitter>()))
            // .JsonCompatible()
            .Build()

    let makePropertyTest<'t when 't : equality> =
        let name =
            match typeof<'t>.GetCustomAttributes(typeof<TestNameAttribute>, true) with
            | [| :? TestNameAttribute as attr |] -> $"{attr.Name} ({typeof<'t>})"
            | _ -> $"Roundtrip test case for type {typeof<'t>}"
        testPropertyWithConfig fsCheckConfig name (fun (x : 't) ->
            let yaml = serializer.Serialize(x)
            let y = deserializer.Deserialize<'t>(yaml)
            (x = y) |@ sprintf "before =\n%A\nafter =\n%A\nyaml =\n%s\n" x y yaml
        )

    let makeTestCase<'t when 't : equality> name (x : 't) =
        testCase name (fun () ->
            let yaml = serializer.Serialize(x)
            let y = deserializer.Deserialize<'t>(yaml)
            Expect.equal y x $"YAML=\n{yaml}\n"
        )

[<Tests>]
let test () =
    testList "Roundtrip tests" [
        testList "Manual" [
            TestCase.makeTestCase "Can serialise empty array, string and option" { One = ""; Two = 1; Three = [||]; Four = None }
            TestCase.makeTestCase "Can serialise option containing value" { One = ""; Two = 1; Three = [||]; Four = Some "TEST" }
            TestCase.makeTestCase "Can serialise string, array and option containing value" { One = "TEST"; Two = 1; Three = [| "TEST1" |]; Four = Some "TEST" }
        ]

        testList "Records" [
            TestCase.makePropertyTest<TestRecordOne>
            TestCase.makePropertyTest<TestRecordTwo>
            TestCase.makePropertyTest<TestRecordThree>
            TestCase.makePropertyTest<TestRecordFour>
            TestCase.makePropertyTest<TestRecordFive>
            // TestCase.makePropertyTest<TestRecordSix> // DateTime issue
            TestCase.makePropertyTest<TestRecordSeven>
            TestCase.makePropertyTest<NestedRecordWithZeroValues>
            TestCase.makePropertyTest<StructRecordWithCollectionTestCases>
            TestCase.makePropertyTest<StructRecordWithNestedTypes>
            TestCase.makePropertyTest<StructRecordWithNestedStructCollectionTypes>
            TestCase.makePropertyTest<StructWith2GenericArs<int, int>>
            TestCase.makePropertyTest<StructWith2GenericArs<string, int>>
            TestCase.makePropertyTest<StructWith2GenericArs<string, string[]>>
            TestCase.makePropertyTest<StructWith2GenericArs< StructWith2GenericArs<string list, (int voption) list> list, string[] option>>
        ]

        testList "Unions" [
            TestCase.makePropertyTest<ExampleTypesInsideModule.UnionOne>
            TestCase.makePropertyTest<ExampleTypesInsideModule.UnionTwo>
            TestCase.makePropertyTest<ExampleTypesInsideModule.UnionThree>
            TestCase.makePropertyTest<ExampleTypesInsideModule.UnionFour>
            TestCase.makePropertyTest<ExampleTypesInsideModule.UnionFive>
            TestCase.makePropertyTest<ExampleTypesInsideModule.UnionSix>
            TestCase.makePropertyTest<ExampleTypesInsideModule.UnionSeven>
            TestCase.makePropertyTest<ExampleTypesInsideModule.UnionEight>
            TestCase.makePropertyTest<ExampleTypesInsideModule.SerialisableOption<string>>
            TestCase.makePropertyTest<ExampleTypesInsideModule.Wrapper<string>>
            TestCase.makePropertyTest<ExampleTypesInsideModule.UnionNine>
            TestCase.makePropertyTest<ValueOption<Set<string>>>
            TestCase.makePropertyTest<Result<int, bool list>>
            TestCase.makePropertyTest<Result<string, uint16[]>>
            TestCase.makePropertyTest<ExampleTypesInsideModule.ValueUnionNoData>
            TestCase.makePropertyTest<ExampleTypesInsideModule.ValueUnionWithData>
        ]
    ]
