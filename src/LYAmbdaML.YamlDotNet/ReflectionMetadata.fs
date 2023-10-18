module YamlDotNet.FSharp.Serialization.ReflectionMetadata

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Collections.Immutable
open System.Reflection
open FSharp.Reflection


[<AbstractClass; Sealed>]
type CastHelpers =
    static member ArrayToList<'T> (elements : 'T[], length : int) : 'T list =
        Array.sub elements 0 length |> List.ofArray<'T>

    static member ArrayToSet<'T when 'T : comparison> (elements : 'T[], length : int) : Set<'T> =
        Array.sub elements 0 length |> Set.ofArray<'T>

    static member Method name =
        typeof<CastHelpers>.GetMethod(name, BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)


type CollectionMetadata = {
    FromArrayMethod : MethodInfo
}


type ContainerMetadata =
    | OptionLike
    | CollectionLike of CollectionMetadata


[<Struct; RequireQualifiedAccess>]
type FieldType = {
    Type : Type
    IsGeneric : bool
} with
    member this.TrySubstitute (genericArgs : Type[]) : Type =
        if this.IsGeneric then
            let rec subst (t : Type) : Type =
                match t with
                | t when not t.ContainsGenericParameters -> t
                | t when t.IsGenericParameter -> genericArgs.[ t.GenericParameterPosition ]
                | t when t.IsGenericType -> t.GetGenericTypeDefinition().MakeGenericType(t.GenericTypeArguments |> Array.map subst)
                | t when t.IsArray ->
                    let elt = subst (t.GetElementType())
                    match t.GetArrayRank() with
                    | 1 -> elt.MakeArrayType()
                    | rank -> elt.MakeArrayType(rank)
                | t -> t
            subst this.Type
        else
            this.Type

    static member Get (t : Type) : FieldType = {
        Type = t
        IsGeneric = t.ContainsGenericParameters
    }


[<Struct>]
type FieldMetadata = {
    Index : int
    ValueType : FieldType
    GetMethod : MethodInfo
    SerializationKey : string
}

[<Struct>]
type BagOfFieldsMetadata = {
    Fields : FieldMetadata[]
    Lookup : IReadOnlyDictionary<string, int>
} with
    member this.Count = this.Fields.Length

    member this.ValueTypeAt (index : int, genericArgs) =
        this.Fields.[index].ValueType.TrySubstitute genericArgs

    member this.LookupKey key =
        match this.Lookup.TryGetValue key with
        | true, fieldIndex -> ValueSome fieldIndex
        | _ -> ValueNone

    static member Build (properties : PropertyInfo[]) =
        let lookupBuilder = ImmutableDictionary.CreateBuilder()
        let fields = [|
            for (index, pi) in properties |> Seq.indexed ->
                let key = pi.Name
                let key =
                    match pi.GetCustomAttribute<YamlDotNet.Serialization.YamlMemberAttribute>() with
                    | null -> pi.Name
                    | attr when attr.Alias <> null -> attr.Alias
                    | _ -> pi.Name
                lookupBuilder.Add(key, index)
                {
                    Index = index
                    ValueType = FieldType.Get pi.PropertyType
                    GetMethod = pi.GetMethod
                    SerializationKey = key
                }
        |]
        {   Fields = fields
            Lookup = lookupBuilder.ToImmutable()
        }


type RecordMetadata = {
    Fields : BagOfFieldsMetadata
    //BoxedConstructor : obj[] -> obj
    ConstructorInfo : ConstructorInfo
} with
    static member Build (recordType : Type) =
        {   Fields = FSharpType.GetRecordFields(recordType, true) |> BagOfFieldsMetadata.Build
            //BoxedConstructor = FSharpValue.PreComputeRecordConstructor(recordType, true)
            ConstructorInfo = FSharpValue.PreComputeRecordConstructorInfo(recordType, true)
        }


type UnionCaseMetadata = {
    Index : int
    SerializationKey : string
    Fields : BagOfFieldsMetadata
    //BoxedConstructor : obj[] -> obj
    ConstructorInfo : MethodInfo
} with
    member case.IsEnumLike = (case.Fields.Count = 0)
    member case.IsSingleton = (case.Fields.Count = 1)


type UnionMetadata = {
    UnionCases : UnionCaseMetadata[]
    Lookup : IReadOnlyDictionary<string, int>
    GetTagInfo : MethodInfo
} with
    member this.NumberOfCases =
        this.UnionCases.Length

    member this.LookupIndexByKey (key, indexOut : byref<int>) =
        this.Lookup.TryGetValue(key, &indexOut)

    member this.LookupCaseByKey (key) =
        let mutable index = 0
        if this.Lookup.TryGetValue(key, &index) then
            ValueSome this.UnionCases.[index]
        else
            ValueNone

    static member Build (unionType : Type) =
        let lookupBuilder = ImmutableDictionary.CreateBuilder()
        let cases = [|
            for (index, case) in FSharpType.GetUnionCases(unionType, true) |> Seq.indexed ->
                let key = case.Name
                lookupBuilder.Add(key, index)
                {
                    Index = index
                    SerializationKey = key
                    Fields = case.GetFields() |> BagOfFieldsMetadata.Build
                    //BoxedConstructor = FSharpValue.PreComputeUnionConstructor(case, true)
                    ConstructorInfo = FSharpValue.PreComputeUnionConstructorInfo(case, true)
                }
        |]
        let getTagMethod =
            match FSharpValue.PreComputeUnionTagMemberInfo(unionType, true) with
            | :? PropertyInfo as tagProperty -> tagProperty.GetMethod
            | :? MethodInfo as tagMethod -> tagMethod
            | smth -> failwithf "Unexpected tag member: %A" smth
        {
            UnionCases = cases
            Lookup = lookupBuilder.ToImmutable()
            GetTagInfo = getTagMethod
        }


type GenericTypeMetadata =
    | GenericRecord of RecordMetadata
    | GenericUnion of UnionMetadata
    | Container of ContainerMetadata

type SpecificTypeMetadata =
    | SpecificRecord of RecordMetadata
    | SpecificUnion of UnionMetadata


let genericCache =
    ConcurrentDictionary<Type, _> (seq {
        KeyValuePair(typedefof<_ option>, OptionLike |> Container)
        KeyValuePair(typedefof<_ voption>, OptionLike |> Container)
        KeyValuePair(typedefof<_ list>, CollectionLike { FromArrayMethod = CastHelpers.Method(nameof CastHelpers.ArrayToList) } |> Container)
        KeyValuePair(typedefof<Set<_>>, CollectionLike { FromArrayMethod = CastHelpers.Method(nameof CastHelpers.ArrayToSet) } |> Container)
    })


[<RequireQualifiedAccess>]
type Inspect =
    | NonFSharp
    | Generic of Meta : GenericTypeMetadata * GenericArgs : Type[]
    | Specific of Meta : SpecificTypeMetadata

let inspectType (typeToInspect : Type) =
    if typeToInspect.IsGenericType then
        let typeDef = typeToInspect.GetGenericTypeDefinition()
        let typeArgs = typeToInspect.GenericTypeArguments
        match genericCache.TryGetValue typeDef with
        | true, meta ->
            Inspect.Generic(meta, typeArgs)
        | _ ->
            if FSharpType.IsRecord(typeDef, true) then
                let meta = RecordMetadata.Build typeDef
                Inspect.Generic(GenericRecord meta, typeArgs)
            elif FSharpType.IsUnion(typeDef, true) then
                let meta = UnionMetadata.Build typeDef
                Inspect.Generic(GenericUnion meta, typeArgs)
            else
                Inspect.NonFSharp
    else
        if FSharpType.IsRecord(typeToInspect, true) then
            let meta = RecordMetadata.Build typeToInspect
            Inspect.Specific(SpecificRecord meta)
        elif FSharpType.IsUnion(typeToInspect, true) then
            let meta = UnionMetadata.Build typeToInspect
            Inspect.Specific(SpecificUnion meta)
        else
            Inspect.NonFSharp


let unpopulatedValueOf (tp : Type) : obj voption =
    if tp.IsGenericType then
        let typeDef = tp.GetGenericTypeDefinition()

        if typeDef = typedefof<_ voption> then
            let cases = FSharpType.GetUnionCases(tp)
            FSharpValue.MakeUnion(cases.[0], [| |]) |> ValueSome
        elif typeDef = typedefof<_ voption> then
            let cases = FSharpType.GetUnionCases(tp)
            FSharpValue.MakeUnion(cases.[0], [| |]) |> ValueSome
        else
            //failwithf "No unpopulated value for generic type %A" typeDef
            ValueNone
    else
        //failwithf "No unpopulated value for type %A" tp
        ValueNone
