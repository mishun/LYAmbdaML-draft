namespace YamlDotNet.FSharp.Serialization

open System
open System.Collections
open System.Collections.Generic
open System.Reflection
open System.Runtime.CompilerServices
open FSharp.Reflection
open YamlDotNet.Core
open YamlDotNet.Serialization
open YamlDotNet.FSharp.YamlDotNetHelper
open YamlDotNet.FSharp.Serialization.ReflectionMetadata


type internal DynamicFieldsBuffer (size) =
    let values = Array.create size null
    let populated = Array.create size false

    member __.Populated = populated

    member __.Set (fieldIndex, value : obj) =
        values.[fieldIndex] <- value
        populated.[fieldIndex] <- true

    member __.ExtractWithMethod (method : MethodInfo) =
        method.Invoke(null, values)


type internal DynamicSeqBuffer (elType : Type) =
    let mutable elements : Array = Array.CreateInstance(elType, 16)
    let mutable count = 0

    member __.Push (el : obj) =
        if count >= elements.Length then
            let next = Array.CreateInstance(elType, 2 * elements.Length)
            Array.Copy(elements, next, elements.Length)
            elements <- next
        elements.SetValue(el, count)
        count <- count + 1

    member __.ExtractWithMethod (method : MethodInfo) =
        method.MakeGenericMethod(elType).Invoke(null, [| elements ; count |])


module CodeGen =
    open System.Threading
    open System.Reflection.Emit


    [<Struct>]
    type ParseSource = {
        Parser : IParser
        NestedObjectDeserializer : Func<IParser, Type, obj>
    } with
        member this.ParseValueDynamic (t : Type) =
            this.NestedObjectDeserializer.Invoke(this.Parser, t)

        member this.ParseOptionPrologue () =
            match this.Parser.Current with
            | YamlDotNet.FSharp.EventPatterns.YamlNull ->
                this.Parser.MoveNext() |> ignore
                false
            | :? Events.MappingStart ->
                this.Parser.MoveNext() |> ignore
                let propertyName = this.Parser.Consume<Events.Scalar>()
                if propertyName.Value <> valueLiteral then
                    failwithf "expected '%s', got '%s'" valueLiteral propertyName.Value
                true
            | token ->
                failwithf "unexpected token %A" token


    [<Struct>]
    type EmitTarget = {
        Emitter : IEmitter
        ObjectSerializer : ObjectSerializer
    } with
        member this.OpenMapping () = this.Emitter.EmitMappingStart()
        member this.CloseMapping () = this.Emitter.EmitMappingEnd()

        member this.OpenSequence () = this.Emitter.EmitSequenceStart()
        member this.CloseSequence () = this.Emitter.EmitSequenceEnd()

        member this.EmitKey (key : string) =
            this.Emitter.EmitScalar key

        member this.EmitValue<'t> (value : 't) =
            this.ObjectSerializer.Invoke(box value, typeof<'t>)

        member this.EmitValueDynamic (t : Type, value : obj) =
            this.ObjectSerializer.Invoke(value, t)

        member this.EmitKeyValue<'t> (key : string, value : 't) =
            this.Emitter.EmitScalar key
            this.ObjectSerializer.Invoke(box value, typeof<'t>)


        member this.OpenOption hasValue =
            if hasValue then
                this.OpenMapping()
                this.EmitKey "value"
            else
                this.Emitter.EmitNull()

        member this.OpenUnion (isEnumLike, unionCaseKey : string) =
            if isEnumLike then
                this.EmitKey unionCaseKey
            else
                this.OpenMapping()
                this.EmitKey unionCaseKey
                this.OpenMapping()

        member this.CloseUnion (isEnumLike) =
            if not isEnumLike then
                this.CloseMapping()
                this.CloseMapping()


        static member OpenMappingMethodInfo = typeof<EmitTarget>.GetMethod(nameof Unchecked.defaultof<EmitTarget>.OpenMapping)
        static member CloseMappingMethodInfo = typeof<EmitTarget>.GetMethod(nameof Unchecked.defaultof<EmitTarget>.CloseMapping)
        static member EmitKeyMethodInfo = typeof<EmitTarget>.GetMethod(nameof Unchecked.defaultof<EmitTarget>.EmitKey)
        static member EmitKeyValueMethodInfo = typeof<EmitTarget>.GetMethod(nameof Unchecked.defaultof<EmitTarget>.EmitKeyValue)
        static member OpenUnionMethodInfo = typeof<EmitTarget>.GetMethod(nameof Unchecked.defaultof<EmitTarget>.OpenUnion)
        static member CloseUnionMethodInfo = typeof<EmitTarget>.GetMethod(nameof Unchecked.defaultof<EmitTarget>.CloseUnion)


    type RecordContextBase (meta : RecordMetadata) =
        member __.LookupKey (key : string, index : byref<int>) =
            meta.Fields.Lookup.TryGetValue(key, &index)

        member __.EmitDynamic (target : byref<EmitTarget>, recordType : Type, value : obj) =
            let fieldValues = FSharpValue.GetRecordFields(value, true)
            let genericArgs = if recordType.IsGenericType then recordType.GenericTypeArguments else null
            target.OpenMapping()
            for field in meta.Fields.Fields do
                target.EmitKey field.SerializationKey
                target.EmitValueDynamic(field.ValueType.TrySubstitute genericArgs, fieldValues.[field.Index])
            target.CloseMapping()

        member __.ParseDynamic (source : byref<ParseSource>, recordType : Type) =
            let genericArgs = if recordType.IsGenericType then recordType.GenericTypeArguments else null
            let mappingStart = source.Parser.Consume<Events.MappingStart>()

            let fieldValues = Array.create meta.Fields.Count null
            let populated = Array.create meta.Fields.Count false

            let mutable mappingEnd = null
            while not (source.Parser.TryConsume<Events.MappingEnd>(&mappingEnd)) do
                let propertyName = source.Parser.Consume<Events.Scalar>()
                match meta.Fields.LookupKey(propertyName.Value) with
                | ValueSome fieldIndex ->
                    fieldValues.[fieldIndex] <- source.ParseValueDynamic(meta.Fields.ValueTypeAt(fieldIndex, genericArgs))
                    populated.[fieldIndex] <- true
                | ValueNone ->
                    printfn "Bad name %s" propertyName.Value
                    source.Parser.SkipThisAndNestedEvents()

            for index in 0 .. meta.Fields.Count - 1 do
                if not populated.[index] then
                    let tp = meta.Fields.ValueTypeAt(index, genericArgs)
                    match unpopulatedValueOf tp with
                    | ValueSome value ->
                        fieldValues.[index] <- value
                    | ValueNone ->
                        failwithf "No unpopulated value for type %A index %i field %s" tp index meta.Fields.Fields.[index].SerializationKey

            FSharpValue.MakeRecord(recordType, fieldValues) // TODO: this is kinda bad

        static member val ConstructorInfo =
            typeof<RecordContextBase>.GetConstructor([| typeof<RecordMetadata> |])


    type UnionContextBase (meta : UnionMetadata) =
        //member __.LookupKey (key : string, index : byref<int>) =
        //    meta.Fields.Lookup.TryGetValue(key, &index)

        member __.EmitDynamic (target : byref<EmitTarget>, unionType : Type, value : obj) =
            let (case, fieldValues) = FSharpValue.GetUnionFields(value, unionType)
            let genericArgs = if unionType.IsGenericType then unionType.GenericTypeArguments else null

            let metaCase = meta.UnionCases.[case.Tag]
            let isEnumLike = metaCase.Fields.Count = 0
            target.OpenUnion(isEnumLike, metaCase.SerializationKey)
            for field in metaCase.Fields.Fields do
                target.EmitKey field.SerializationKey
                target.EmitValueDynamic(field.ValueType.TrySubstitute genericArgs, fieldValues.[field.Index])
            target.CloseUnion(isEnumLike)

        member __.ParseDynamic (source : byref<ParseSource>, unionType : Type, genericArgs) =
            match source.Parser.Current with
            | :? Events.Scalar ->
                let scalar = source.Parser.Consume<Events.Scalar>()
                match meta.LookupCaseByKey scalar.Value with
                | ValueSome case when case.IsEnumLike ->
                    let method =
                        let method = case.ConstructorInfo
                        match genericArgs with
                        | null -> method
                        | _ -> method.DeclaringType.MakeGenericType(genericArgs).GetMethod(method.Name)
                    method.Invoke(null, Array.empty)
                | _ -> YamlException($"Can not get singleton union case from '{scalar.Value}'") |> raise

            | :? Events.MappingStart ->
                let _ = source.Parser.Consume<Events.MappingStart>()
                let tagScalar = source.Parser.Consume<Events.Scalar>()
                match meta.LookupCaseByKey tagScalar.Value with
                | ValueSome case ->
                    let _ = source.Parser.Consume<Events.MappingStart>()

                    let fieldValues = Array.create case.Fields.Count null
                    let populated = Array.create case.Fields.Count false

                    let mutable mappingEnd = null
                    while not (source.Parser.TryConsume<Events.MappingEnd>(&mappingEnd)) do
                        let propertyName = source.Parser.Consume<Events.Scalar>()
                        match case.Fields.Lookup.TryGetValue(propertyName.Value) with
                        | true, fieldIndex ->
                            fieldValues.[fieldIndex] <- source.ParseValueDynamic(case.Fields.ValueTypeAt(fieldIndex, genericArgs))
                            populated.[fieldIndex] <- true
                        | _ ->
                            let sb = Text.StringBuilder(sprintf "Bad name %s ; expected one of" propertyName.Value)
                            for f in case.Fields.Fields do
                                sb.Append($" {f.SerializationKey}") |> ignore
                            printfn "%O" sb
                            source.Parser.SkipThisAndNestedEvents()

                    let _ = source.Parser.Consume<Events.MappingEnd>()

                    for index in 0 .. case.Fields.Count - 1 do
                        if not populated.[index] then
                            let tp = case.Fields.ValueTypeAt(index, null)
                            match unpopulatedValueOf tp with
                            | ValueSome value ->
                                fieldValues.[index] <- value
                            | ValueNone ->
                                failwithf "No unpopulated value for type %A" tp

                    let method =
                        let method = case.ConstructorInfo
                        match genericArgs with
                        | null -> method
                        | _ -> method.DeclaringType.MakeGenericType(genericArgs).GetMethod(method.Name)
                    method.Invoke(null, fieldValues)

                | ValueNone -> YamlException($"Can not get singleton union case from '{tagScalar.Value}'") |> raise

            | _ -> YamlException("expected either scalar or mapping start") |> raise

        static member val ConstructorInfo =
            typeof<UnionContextBase>.GetConstructor([| typeof<UnionMetadata> |])


    type ManualContext () =
        static let cache =
            let t = typeof<ManualContext>
            Collections.Concurrent.ConcurrentDictionary<_, _> (seq {
                KeyValuePair(typedefof<_ option>, struct (t.GetMethod("EmitOption"), t.GetMethod("ParseOption")))
                KeyValuePair(typedefof<_ voption>, struct (t.GetMethod("EmitValueOption"), t.GetMethod("ParseValueOption")))
                KeyValuePair(typedefof<_ list>, struct (t.GetMethod("EmitList"), t.GetMethod("ParseList")))
                KeyValuePair(typedefof<Set<_>>, struct (t.GetMethod("EmitSet"), t.GetMethod("ParseSet")))
            })


        member __.ParseSeq<'t> (source : byref<ParseSource>) =
            let _ = source.Parser.Consume<Events.SequenceStart>()
            //let buffer = DynamicSeqBuffer genericArgs.[0]
            let buffer = List<'t>()
            let mutable seqEnd = null
            while not (source.Parser.TryConsume<Events.SequenceEnd>(&seqEnd)) do
                //nestedObjectDeserializer.Invoke(parser, genericArgs.[0]) |> buffer.Push
                source.ParseValueDynamic typeof<'t> |> unbox |> buffer.Add
            //buffer.ExtractWithMethod(meta.FromArrayMethod)
            buffer.ToArray()


        member __.EmitList<'t> (target : byref<EmitTarget>, list : 't list) =
            target.OpenSequence()
            for element in list do
                target.EmitValue<'t> element
            target.CloseSequence()

        member __.ParseList<'t> (source : byref<ParseSource>) : 't list =
            let array = __.ParseSeq &source
            List.ofArray array


        member __.EmitSet<'t when 't : comparison> (target : byref<EmitTarget>, set : Set<'t>) =
            target.OpenSequence()
            for element in set do
                target.EmitValue<'t> element
            target.CloseSequence()

        member __.ParseSet<'t when 't : comparison> (source : byref<ParseSource>) : Set<'t> =
            let array = __.ParseSeq &source
            Set.ofArray array


        member __.EmitOption<'t> (target : byref<EmitTarget>, opt : 't option) =
            target.OpenOption(Option.isSome opt)
            match opt with
            | None -> ()
            | Some value ->
                target.EmitValue value
                target.CloseMapping()

        member __.ParseOption<'t> (source : byref<ParseSource>) : 't option =
            if source.ParseOptionPrologue () then
                let element = source.ParseValueDynamic typeof<'t> |> unbox
                let _ = source.Parser.Consume<Events.MappingEnd>()
                Some element
            else
                None


        member __.EmitValueOption<'t> (target : byref<EmitTarget>, opt :'t voption) =
            target.OpenOption(ValueOption.isSome opt)
            match opt with
            | ValueNone -> ()
            | ValueSome value ->
                target.EmitValue value
                target.CloseMapping()

        member __.ParseValueOption<'t> (source : byref<ParseSource>) : 't voption =
            if source.ParseOptionPrologue () then
                let element = source.ParseValueDynamic typeof<'t> |> unbox
                let _ = source.Parser.Consume<Events.MappingEnd>()
                ValueSome element
            else
                ValueNone


        static member LookupEmitMethod (t : Type) =
            match cache.TryGetValue t with
            | true, (meth, _) -> ValueSome meth
            | _ -> ValueNone

        static member LookupParseMethod (t : Type) =
            match cache.TryGetValue t with
            | true, (_, meth) -> ValueSome meth
            | _ -> ValueNone


    module internal Dynamic =
        let private dynamicAssembly = AssemblyBuilder.DefineDynamicAssembly(AssemblyName("LYAmbdaML.Emitted"), AssemblyBuilderAccess.Run)
        let private dynamicModule = dynamicAssembly.DefineDynamicModule "LYAmbdaML.Emitted"
        let private dynamicPrefix = "LYAmbdaML.Generated"
        let mutable uniqueIDCounter = 0

        let declareType (nameTag : string, baseClass : Type) =
            let id = Interlocked.Increment &uniqueIDCounter
            let name = $"{dynamicPrefix}.{id}.{nameTag}"
            let attr = TypeAttributes.Public ||| TypeAttributes.Sealed // ||| TypeAttributes.Serializable
            dynamicModule.DefineType(name, attr, baseClass)


    let emitRecord (recordType : Type, meta : RecordMetadata) =
        printfn "EmitUnion on %A" recordType
        let typeBuilder = Dynamic.declareType(recordType.FullName, typeof<RecordContextBase>)
            // let id = Interlocked.Increment &uniqueIDCounter
            // let name = $"{dynamicPrefix}.{id}.{recordType.FullName}"
            // let attr = TypeAttributes.Public ||| TypeAttributes.Sealed // ||| TypeAttributes.Serializable
            // dynamicModule.DefineType(name, attr, typeof<RecordContextBase>)

        let constructor = typeBuilder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [| typeof<RecordMetadata> |])
        begin
            let gen = constructor.GetILGenerator()
            gen.Emit(OpCodes.Ldarg_0)
            gen.Emit(OpCodes.Ldarg_1)
            gen.Emit(OpCodes.Call, RecordContextBase.ConstructorInfo)
            gen.Emit(OpCodes.Ret)
        end

        let isValueType = recordType.IsValueType
        let isGeneric = recordType.IsGenericTypeDefinition

        let writeMethod = typeBuilder.DefineMethod("Emit", MethodAttributes.Public)
        begin
            let struct (genericParams, argumentType) =
                if isGeneric then
                    let genericParamDefs = writeMethod.DefineGenericParameters [| for arg in recordType.GetGenericArguments() -> arg.Name |]
                    let genericParams = [| for p in genericParamDefs -> p :> Type |]
                    let recordTypeSubst = recordType.MakeGenericType(genericParams)
                    struct (genericParams, recordTypeSubst)
                else
                    struct (null, recordType)
            writeMethod.SetParameters([| typeof<EmitTarget>.MakeByRefType() ; argumentType |])

            let gen = writeMethod.GetILGenerator()
            gen.Emit(OpCodes.Ldarg_1)
            gen.Emit(OpCodes.Call, EmitTarget.OpenMappingMethodInfo)

            for fld in meta.Fields.Fields do
                gen.Emit(OpCodes.Ldarg_1)
                gen.Emit(OpCodes.Ldstr, fld.SerializationKey)
                gen.Emit((if isValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 2)
                gen.Emit(OpCodes.Call, if isGeneric then TypeBuilder.GetMethod(argumentType, fld.GetMethod) else fld.GetMethod)
                gen.Emit(OpCodes.Call, EmitTarget.EmitKeyValueMethodInfo.MakeGenericMethod(fld.ValueType.TrySubstitute(genericParams)))

            gen.Emit(OpCodes.Ldarg_1)
            gen.Emit(OpCodes.Call, EmitTarget.CloseMappingMethodInfo)
            gen.Emit(OpCodes.Ret)
        end

        let tp = typeBuilder.CreateTypeInfo()
        let instance = Activator.CreateInstance(tp, [| box meta |])
        let method = tp.GetMethod("Emit")
        struct (instance, method)

    let emitUnion (unionType : Type, meta : UnionMetadata) =
        printfn "EmitUnion on %A" unionType
        let typeBuilder = Dynamic.declareType(unionType.FullName, typeof<RecordContextBase>)
            // let id = Interlocked.Increment &uniqueIDCounter
            // let name = $"{dynamicPrefix}.{id}.{unionType.FullName}"
            // let attr = TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable
            // dynamicModule.DefineType(name, attr, typeof<RecordContextBase>)

        let constructor = typeBuilder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [| typeof<UnionMetadata> |])
        begin
            let gen = constructor.GetILGenerator()
            gen.Emit(OpCodes.Ldarg_0)
            gen.Emit(OpCodes.Ldarg_1)
            gen.Emit(OpCodes.Call, UnionContextBase.ConstructorInfo)
            gen.Emit(OpCodes.Ret)
        end

        let isValueType = unionType.IsValueType
        let isGeneric = unionType.IsGenericTypeDefinition

        let writeMethod = typeBuilder.DefineMethod("Emit", MethodAttributes.Public)
        begin
            let struct (genericParams, argumentType) =
                if isGeneric then
                    let genericParamDefs = writeMethod.DefineGenericParameters [| for arg in unionType.GetGenericArguments() -> arg.Name |]
                    let genericParams = [| for p in genericParamDefs -> p :> Type |]
                    let recordTypeSubst = unionType.MakeGenericType(genericParams)
                    struct (genericParams, recordTypeSubst)
                else
                    struct (null, unionType)
            writeMethod.SetParameters([| typeof<EmitTarget>.MakeByRefType() ; argumentType |])

            let gen = writeMethod.GetILGenerator()

            gen.Emit((if isValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 2)
            gen.Emit(OpCodes.Call, if isGeneric then TypeBuilder.GetMethod(argumentType, meta.GetTagInfo) else meta.GetTagInfo)
            let caseJumpTable = [| for _ in 0 .. meta.NumberOfCases - 1 -> gen.DefineLabel() |]
            gen.Emit(OpCodes.Switch, caseJumpTable) // Dispatch on int union tag
            gen.Emit(OpCodes.Ret)

            for metaCase in meta.UnionCases do
                gen.MarkLabel(caseJumpTable.[metaCase.Index])
                let isEnumLike = metaCase.Fields.Count = 0

                gen.Emit(OpCodes.Ldarg_1)
                gen.Emit(OpCodes.Ldc_I4, if isEnumLike then 1 else 0)
                gen.Emit(OpCodes.Ldstr, metaCase.SerializationKey)
                gen.Emit(OpCodes.Call, EmitTarget.OpenUnionMethodInfo)

                for fld in metaCase.Fields.Fields do
                    gen.Emit(OpCodes.Ldarg_1)
                    gen.Emit(OpCodes.Ldstr, fld.SerializationKey)
                    let getMethod =
                        if isGeneric then // Holy shit
                            let subtypeDef = fld.GetMethod.DeclaringType.GetGenericTypeDefinition()
                            TypeBuilder.GetMethod(subtypeDef.MakeGenericType(genericParams), subtypeDef.GetMethod(fld.GetMethod.Name))
                        else
                            fld.GetMethod
                    gen.Emit((if isValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 2)
                    gen.Emit(OpCodes.Call, getMethod)
                    gen.Emit(OpCodes.Call, EmitTarget.EmitKeyValueMethodInfo.MakeGenericMethod(fld.ValueType.TrySubstitute(genericParams)))

                gen.Emit(OpCodes.Ldarg_1)
                gen.Emit(OpCodes.Ldc_I4, if isEnumLike then 1 else 0)
                gen.Emit(OpCodes.Call, EmitTarget.CloseUnionMethodInfo)

                gen.Emit(OpCodes.Ret)
        end

        let tp = typeBuilder.CreateTypeInfo()
        let instance = Activator.CreateInstance(tp, [| box meta |])
        let method = tp.GetMethod("Emit")
        struct (instance, method)


type FSharpNodeDeserializer () =
    interface INodeDeserializer with
        member __.Deserialize (parser, expectedType, nestedObjectDeserializer, result) =
            let mutable source = { CodeGen.ParseSource.Parser = parser ; CodeGen.ParseSource.NestedObjectDeserializer = nestedObjectDeserializer }
            match inspectType expectedType with
            | Inspect.NonFSharp ->
                false

            | Inspect.Specific(SpecificRecord meta) ->
                let ctx = CodeGen.RecordContextBase(meta)
                result <- ctx.ParseDynamic(&source, expectedType)
                true

            | Inspect.Generic(GenericRecord meta, genericArgs) ->
                let ctx = CodeGen.RecordContextBase(meta)
                result <- ctx.ParseDynamic(&source, expectedType)
                true

            | Inspect.Specific(SpecificUnion meta) ->
                let ctx = CodeGen.UnionContextBase(meta)
                result <- ctx.ParseDynamic(&source, expectedType, null)
                true

            | Inspect.Generic(GenericUnion meta, genericArgs) ->
                let ctx = CodeGen.UnionContextBase(meta)
                result <- ctx.ParseDynamic(&source, expectedType, genericArgs)
                true

            | Inspect.Generic(Container _, genericArgs) ->
                let ctx = CodeGen.ManualContext()
                match CodeGen.ManualContext.LookupParseMethod (expectedType.GetGenericTypeDefinition()) with
                | ValueSome meth ->
                    result <- meth.MakeGenericMethod(genericArgs).Invoke(ctx, [| box source |])
                | ValueNone -> failwith "Not implemented"
                true

    static member val Instance =
        FSharpNodeDeserializer()


type FSharpEmissionPhaseObjectGraphVisitor (args : EmissionPhaseObjectGraphVisitorArgs) =
    inherit ObjectGraphVisitors.ChainedObjectGraphVisitor (args.InnerVisitor)

    let cache = Concurrent.ConcurrentDictionary<Type, struct (obj * _)> ()

    override __.Enter (value, emitter) =
        let mutable target = { CodeGen.EmitTarget.Emitter = emitter ; CodeGen.EmitTarget.ObjectSerializer = args.NestedObjectSerializer }
        match inspectType value.Type with
        | Inspect.NonFSharp ->
            base.Enter(value, emitter)

        | Inspect.Specific(SpecificRecord meta) ->
            // let struct (inst, meth) =
            //     cache.GetOrAdd(value.Type, Func<_, _>(fun _ -> CodeGen.emitRecord(value.Type, meta)))
            // meth.Invoke(inst, [| box target ; value.Value |]) |> ignore

            let ctx = CodeGen.RecordContextBase(meta)
            ctx.EmitDynamic(&target, value.Type, value.Value)
            false

        | Inspect.Generic(GenericRecord meta, genericArgs) ->
            //let struct (inst, meth) = CodeGen.emitRecord(value.Type.GetGenericTypeDefinition(), meta)
            //meth.MakeGenericMethod(genericArgs).Invoke(inst, [| box target ; value.Value |]) |> ignore
            let ctx = CodeGen.RecordContextBase(meta)
            ctx.EmitDynamic(&target, value.Type, value.Value)
            false

        | Inspect.Specific(SpecificUnion meta) ->
            // let struct (inst, meth) =
            //     cache.GetOrAdd(value.Type, Func<_, _>(fun tp -> CodeGen.emitUnion(tp, meta)))
            // meth.Invoke(inst, [| box target ; value.Value |]) |> ignore

            let ctx = CodeGen.UnionContextBase(meta)
            ctx.EmitDynamic(&target, value.Type, value.Value)
            false

        | Inspect.Generic(GenericUnion meta, genericArgs) ->
            // let struct (inst, meth) = CodeGen.emitUnion(value.Type.GetGenericTypeDefinition(), meta)
            // meth.MakeGenericMethod(genericArgs).Invoke(inst, [| box target ; value.Value |]) |> ignore
            let ctx = CodeGen.UnionContextBase(meta)
            ctx.EmitDynamic(&target, value.Type, value.Value)
            false

        | Inspect.Generic(Container _, genericArgs) ->
            let inst = CodeGen.ManualContext()
            match CodeGen.ManualContext.LookupEmitMethod (value.Type.GetGenericTypeDefinition()) with
            | ValueSome meth ->
                meth.MakeGenericMethod(genericArgs).Invoke(inst, [| box target ; value.Value |]) |> ignore
            | ValueNone -> failwith "Not implemented"
            false

(*
type FSharpPreProcessingPhaseObjectGraphVisitor (nextVisitor : IObjectGraphVisitor<Nothing>) =
    interface IObjectGraphVisitor<Nothing> with
        member __.Enter (value, context) =
            nextVisitor.Enter(value, context)

        member __.EnterMapping (key, value, context) =
            nextVisitor.EnterMapping(key, value, context)
*)


[<Extension>]
type FSharpExtensions =
    [<Extension>]
    static member WithFSharpTypes(builder : DeserializerBuilder) =
        builder.WithNodeDeserializer(FSharpNodeDeserializer.Instance)

    [<Extension>]
    static member WithFSharpTypes(builder : SerializerBuilder) =
        builder
            .DisableAliases()
            .WithEmissionPhaseObjectGraphVisitor(
                (fun args -> FSharpEmissionPhaseObjectGraphVisitor(args)),
                (fun w -> w.OnTop())
            )
        //    .WithPreProcessingPhaseObjectGraphVisitor(
        //        (fun (innerVisitor: IObjectGraphVisitor<Nothing>) -> FSharpPreProcessingPhaseObjectGraphVisitor(innerVisitor)),
        //        (fun w -> w.OnTop())
        //    )
