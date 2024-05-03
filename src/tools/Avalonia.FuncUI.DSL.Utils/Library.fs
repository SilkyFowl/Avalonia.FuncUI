namespace Avalonia.FuncUI.DSL.Utils

open System
open System.Reflection

module Regex =
    open System.Text.RegularExpressions

    let replace (pattern: string) (replacement: string) (input: string) =
        Regex.Replace(input, pattern, replacement)

module Assembly =
    let tryGetCustomAttributeWith<'t, 's when 't: null and 't :> Attribute> (mapper: 't -> 's) (asm: Assembly) =
        let attr = asm.GetCustomAttribute<'t>()

        match attr with
        | null -> None
        | _ -> Some(mapper attr)

type Tree<'node, 'leaf> =
    | Leaf of 'leaf
    | Node of 'node * Tree<'node, 'leaf> list

type TypeMemberInfo =
    { TypeInfo: TypeInfo
      Constructors: ConstructorInfo[]
      StaticFields: FieldInfo[]
      StaticInitonlyFields: FieldInfo[]
      StaticProps: PropertyInfo[]
      StaticReadonlyProps: PropertyInfo[]
      StaticMethods: MethodInfo[]
      Events: EventInfo[]
      InitonlyFields: FieldInfo[]
      Fields: FieldInfo[]
      ReadonlyProps: PropertyInfo[]
      Props: PropertyInfo[]
      Methods: MethodInfo[] }

module TypeMemberInfo =
    let ofTypeInfo (info: TypeInfo) =
        let baseBindingFlags = BindingFlags.Public ||| BindingFlags.DeclaredOnly
        let instanceBindingFlags = baseBindingFlags ||| BindingFlags.Instance
        let staticBindingFlags = baseBindingFlags ||| BindingFlags.Static

        let staticInitonlyFields, staticFields =
            info.GetFields(staticBindingFlags)
            |> Array.filter (fun p -> not p.IsSpecialName)
            |> Array.partition (fun f -> f.IsInitOnly)

        let staticProps, staticReadonlyProps =
            info.GetProperties(staticBindingFlags)
            |> Array.filter (fun p -> not p.IsSpecialName)
            |> Array.partition (fun p -> p.CanWrite)

        let staticMethods =
            info.GetMethods(staticBindingFlags)
            |> Array.filter (fun p -> not p.IsSpecialName)

        let events =
            info.GetEvents(instanceBindingFlags)
            |> Array.filter (fun p -> not p.IsSpecialName)

        let constructors = info.GetConstructors(instanceBindingFlags)

        let fields, initonlyFields =
            info.GetFields(instanceBindingFlags)
            |> Array.filter (fun p -> not p.IsSpecialName)
            |> Array.partition (fun f -> f.IsInitOnly)

        let props, readonlyProps =
            info.GetProperties(instanceBindingFlags)
            |> Array.filter (fun p -> not p.IsSpecialName)
            |> Array.partition (fun p -> p.CanWrite)

        let methods =
            info.GetMethods(instanceBindingFlags)
            |> Array.filter (fun p -> not p.IsSpecialName)

        { TypeInfo = info
          Constructors = constructors
          StaticFields = staticFields
          StaticInitonlyFields = staticInitonlyFields
          StaticProps = staticProps
          StaticReadonlyProps = staticReadonlyProps
          StaticMethods = staticMethods
          Events = events
          InitonlyFields = initonlyFields
          Fields = fields
          ReadonlyProps = readonlyProps
          Props = props
          Methods = methods }

    let ofType (t: Type) = t.GetTypeInfo() |> ofTypeInfo

    open Microsoft.FSharp.Reflection
    open Avalonia
    open Avalonia.Interactivity

    type TypeFormatter(fNameSpace: string -> unit) =
        member this.sprintType(t: Type) =
            let rec loop (t: Type) =
                let t = t.GetTypeInfo()

                if t.IsGenericParameter then
                    $"'{t.Name}"
                else
                    fNameSpace t.Namespace

                    if t.IsArray then
                        let elementType = t.GetElementType() |> loop

                        let sep =
                            let rank = t.GetArrayRank() - 1
                            String.replicate rank ","

                        $"{elementType}[{sep}]"

                    else if FSharpType.IsFunction(t) then
                        t.GetGenericArguments() |> Array.map loop |> String.concat " -> "

                    else if t.IsByRef then
                        let elementType = t.GetElementType() |> loop
                        $"byref<{elementType}>"
                    else if t.IsPointer then
                        let elementType = t.GetElementType() |> loop
                        $"ptr<{elementType}>"
                    else if t.IsGenericType then
                        let typeName = Regex.replace "`\d+" "" t.Name

                        let typeParam = t.GetGenericArguments() |> Array.map loop |> String.concat ", "

                        $"{typeName}<{typeParam}>"
                    else
                        t.Name

            loop t

        member this.sprintCustomAttributes(m: MemberInfo) =

            m.GetCustomAttributesData()
            |> Seq.map (fun a ->
                fNameSpace a.AttributeType.Namespace

                this.sprintType a.AttributeType |> Regex.replace "(.+)Attribute" "$1")
            |> Seq.toArray

        member this.sprintMethod(m: MethodInfo) =
            let paramStr =
                m.GetParameters()
                |> Array.map (fun p -> $"{p.Name}:{this.sprintType p.ParameterType}")
                |> String.concat " * "

            let returnType = this.sprintType m.ReturnType

            String.concat
                " "
                [ if m.IsStatic then
                      "static"
                  if m.IsAbstract then
                      "abstract"
                  "member"
                  $"{m.Name}:"
                  $"({paramStr})"
                  "->"
                  returnType ]

        member this.sprintMethodGenericArgs(m: MethodBase) =
            if m.IsGenericMethod then
                let typeParam =
                    m.GetGenericArguments() |> Array.map this.sprintType |> String.concat ", "

                $"<{typeParam}>"
            else
                ""

        member this.sprintConstructor(c: ConstructorInfo) =
            let paramStr =
                c.GetParameters()
                |> Array.map (fun p -> $"{p.Name}:{this.sprintType p.ParameterType}")
                |> String.concat " * "

            let genericArgs = this.sprintMethodGenericArgs c
            let returnType = this.sprintType c.DeclaringType

            String.concat
                " "
                [ if c.IsStatic then
                      "static"
                  if c.IsAbstract then
                      "abstract"
                  $"new{genericArgs}:"
                  $"({paramStr})"
                  "->"
                  returnType ]

        member this.sprintProperty(p: PropertyInfo) =


            let returnType = this.sprintType p.PropertyType
            let genericArgs = this.sprintMethodGenericArgs p.GetMethod

            let accessors =
                [ if p.CanRead && p.GetMethod.IsPublic then
                      "get"
                  if p.CanWrite && p.SetMethod.IsPublic then
                      "set" ]

            String.concat
                " "
                [ if p.GetMethod.IsStatic then
                      "static"
                  if p.GetMethod.IsAbstract then
                      "abstract"
                  "member"
                  $"{p.Name}{genericArgs}:"
                  returnType
                  if accessors.Length > 0 then
                      "with"
                      String.concat ", " accessors ]

        member this.sprintField(f: FieldInfo) =

            let fieldType = this.sprintType f.FieldType

            let comments =
                [ if f.IsStatic then
                      "static"
                  if f.IsLiteral then
                      "C# constant"
                  if f.IsInitOnly then
                      "C# readonly" ]

            String.concat
                " "
                [ "val"
                  $"{f.Name}:"
                  fieldType
                  if comments.Length > 0 then
                      "//"
                      String.concat ", " comments ]

        member this.sprintEvent(e: EventInfo) =

            let eventType = this.sprintType e.EventHandlerType
            let genericArgs = this.sprintMethodGenericArgs e.AddMethod

            let accessors =
                [ if e.GetAddMethod().IsPublic then
                      "add"
                  if e.GetRemoveMethod().IsPublic then
                      "remove" ]

            String.concat
                " "
                [ if e.GetAddMethod().IsStatic then
                      "static"
                  if e.GetAddMethod().IsAbstract then
                      "abstract"
                  "member"
                  $"{e.Name}{genericArgs}:"
                  eventType
                  if accessors.Length > 0 then
                      "// with"
                      String.concat ", " accessors ]

    let private fmt = TypeFormatter(ignore)
    let sprintTypeName (info: TypeMemberInfo) = fmt.sprintType info.TypeInfo

    let sprintCodeBlock (info: TypeMemberInfo) =
        let namespaces = ResizeArray()

        let fmt =
            TypeFormatter(fun ns ->
                if not (namespaces.Contains(ns)) then
                    namespaces.Add(ns))

        let ns = Some $"namespace {info.TypeInfo.Namespace}"

        let attrs =
            let attrs =
                [ if info.TypeInfo.IsAbstract then
                      "AbstractClass"
                  yield! fmt.sprintCustomAttributes info.TypeInfo ]

            if attrs.IsEmpty then
                None
            else
                String.concat "; " attrs |> sprintf "[<%s>]" |> Some

        let typeStr = Some $"type {fmt.sprintType info.TypeInfo} ="

        let inheritStr =
            Option.ofObj info.TypeInfo.BaseType
            |> Option.filter (fun t -> t <> typeof<obj>)
            |> Option.map (fun t -> $"    inherit {fmt.sprintType t}")

        let sprintMembers (f: 'info -> string) attrs name (infos: 'info[]) =
            if Array.isEmpty infos then
                []
            else
                let addIndent = fun s -> Some $"    %s{s}"

                [ yield Some $"    // {name}"
                  for info in infos do
                      let attrs = [ yield! attrs; yield! fmt.sprintCustomAttributes info ]

                      if not attrs.IsEmpty then
                          yield attrs |> String.concat "; " |> sprintf "[<%s>]" |> addIndent

                      yield f info |> addIndent
                  yield Some "" ]


        let constructors =
            ("constructors", info.Constructors) ||> sprintMembers fmt.sprintConstructor []

        let staticFields =
            ("static fields", info.StaticFields) ||> sprintMembers fmt.sprintField []

        let staticInitonlyFields =
            ("static initonly fields", info.StaticInitonlyFields)
            ||> sprintMembers fmt.sprintField []

        let staticProps =
            ("static properties", info.StaticProps) ||> sprintMembers fmt.sprintProperty []

        let staticReadonlyProps =
            ("static readonly properties", info.StaticReadonlyProps)
            ||> sprintMembers fmt.sprintProperty []

        let staticMethods =
            ("static methods", info.StaticMethods) ||> sprintMembers fmt.sprintMethod []

        let events =
            ("events", info.Events) ||> sprintMembers fmt.sprintEvent [ "CLIEvent" ]

        let initonlyFields =
            ("initonly fields", info.InitonlyFields) ||> sprintMembers fmt.sprintField []

        let fields = ("fields", info.Fields) ||> sprintMembers fmt.sprintField []

        let readonlyProps =
            ("readonly properties", info.ReadonlyProps)
            ||> sprintMembers fmt.sprintProperty []

        let props = ("properties", info.Props) ||> sprintMembers fmt.sprintProperty []

        let methods = ("methods", info.Methods) ||> sprintMembers fmt.sprintMethod []

        if namespaces.Contains info.TypeInfo.Namespace then
            namespaces.Remove info.TypeInfo.Namespace |> ignore

        [ Some "```fs"
          ns
          Some ""
          for opNs in namespaces |> Seq.sort do
              Some $"open {opNs}"
          Some ""
          attrs
          typeStr
          inheritStr
          Some ""
          yield! constructors
          yield! staticFields
          yield! staticInitonlyFields
          yield! staticProps
          yield! staticReadonlyProps
          yield! staticMethods
          yield! events
          yield! initonlyFields
          yield! fields
          yield! readonlyProps
          yield! props
          yield! methods
          Some "```" ]
        |> List.choose id
        |> String.concat "\n"

    let hasInStaticFieldOrProperty (ty: Type) (info: TypeMemberInfo) =
        let isStaticFieldOrProperty (t: Type) =
            t.IsAssignableTo(ty) || t.IsAssignableTo(ty)

        info.TypeInfo.GetMembers(BindingFlags.Public ||| BindingFlags.DeclaredOnly ||| BindingFlags.Static)
        |> Array.exists (function
            | :? FieldInfo as p -> isStaticFieldOrProperty p.FieldType
            | :? PropertyInfo as p -> isStaticFieldOrProperty p.PropertyType
            | _ -> false)

    let hasAvaloniaPropertyOrRoutedEvent (info: TypeMemberInfo) =
        let isAvaloniaPropertyOrRouredEvent (t: Type) =
            t.IsAssignableTo(typeof<AvaloniaProperty>)
            || t.IsAssignableTo(typeof<RoutedEvent>)

        info.TypeInfo.GetMembers(BindingFlags.Public ||| BindingFlags.DeclaredOnly ||| BindingFlags.Static)
        |> Array.exists (function
            | :? FieldInfo as p -> isAvaloniaPropertyOrRouredEvent p.FieldType
            | :? PropertyInfo as p -> isAvaloniaPropertyOrRouredEvent p.PropertyType
            | _ -> false)

    let objTypeMemberInfo = ofType typeof<obj>
    let objTypeInfo = typeof<obj>.GetTypeInfo()

type TypeMemberInfoTree = Tree<TypeMemberInfo, TypeMemberInfo>

module TypeMemberInfoTree =
    let build (tymis: TypeMemberInfo seq) =
        let tymis = tymis |> Seq.toList

        let findAnsector (tymi: TypeMemberInfo) =
            let getBaseType (info: TypeInfo) =
                let info = info.BaseType.GetTypeInfo()

                if info.IsGenericType then
                    info.GetGenericTypeDefinition().GetTypeInfo()
                else
                    info

            let rec loop (tyi: TypeInfo) =
                if tyi = TypeMemberInfo.objTypeInfo then
                    TypeMemberInfo.objTypeMemberInfo
                else
                    let result =
                        tymis |> List.tryPick (fun x -> if x.TypeInfo = tyi then Some x else None)

                    match result with
                    | Some x -> x
                    | None -> getBaseType tyi |> loop

            tymi.TypeInfo |> getBaseType |> loop

        let tymiGroup = tymis |> List.groupBy findAnsector

        let tryPartition pred xs =
            let rec loop acc right left =
                match left with
                | [] -> acc, List.rev right
                | x :: xs ->
                    if pred x then
                        Some x, List.rev right @ xs
                    else
                        loop acc (x :: right) xs

            loop None [] xs

        let partition pred xs =
            match tryPartition pred xs with
            | Some x, xs -> x, xs
            | None, _ -> invalidArg "xs" "No element satisfies the predicate."

        let root, children =
            tymiGroup |> partition (fun (k, _) -> k = TypeMemberInfo.objTypeMemberInfo)

        let mutable count = 0

        let rec loop
            ((parent, children): TypeMemberInfo * list<TypeMemberInfo>)
            (descendants: list<TypeMemberInfo * list<TypeMemberInfo>>)
            : TypeMemberInfoTree =
            let childTrees, others =
                (descendants, children)
                ||> List.mapFold (fun descendants x ->
                    let child, descendants = descendants |> tryPartition (fun (k, _) -> k = x)

                    match child with
                    | None ->
                        count <- count + 1
                        Leaf x, descendants
                    | Some child -> loop child descendants, descendants)


            count <- count + 1
            Node(parent, childTrees)

        let tree = loop root children

        if count - 1 <> tymis.Length then
            invalidArg "tymis" "Some elements are not included in the tree."
        else
            tree

    let sprintMd (tree: TypeMemberInfoTree) =
        let rec loop (path: string list) (tree: TypeMemberInfoTree) =
            let inheritPath =
                match path with
                | [] -> ""
                | path -> String.concat " <- " path |> sprintf "base: %s"

            match tree with
            | Leaf x ->
                let name = TypeMemberInfo.sprintTypeName x
                let header = $"## {name}"

                String.concat
                    "\n\n"
                    [ header
                      if inheritPath <> "" then
                          inheritPath
                      TypeMemberInfo.sprintCodeBlock x ]

            | Node(x, children) ->
                let parentName = TypeMemberInfo.sprintTypeName x
                let pathStr str =
                    let link = Regex.replace "[<|>|']" "" str
                    $"[{str}](#{link})"
                let path =
                    pathStr parentName :: path

                let childLinks =
                    children
                    |> List.map (function
                        | Leaf x -> x
                        | Node(x, _) -> x)
                    |> List.map TypeMemberInfo.sprintTypeName
                    |> List.map pathStr
                    |> String.concat ", "
                    |> sprintf "Children: %s"

                let childrenStr = children |> List.map (loop path) |> String.concat "\n\n"

                let header = $"## {parentName}"

                String.concat
                    "\n\n"
                    [ header
                      if inheritPath <> "" then
                          inheritPath
                      childLinks
                      TypeMemberInfo.sprintCodeBlock x
                      childrenStr ]

        loop [] tree

type AvaloniaDslType =
    | AssignableToAvanloniaObject of TypeMemberInfo
    | HasAvaloniaPropertyOrRoutedEvent of TypeMemberInfo

    member this.TypeMemberInfo =
        match this with
        | AssignableToAvanloniaObject info -> info
        | HasAvaloniaPropertyOrRoutedEvent info -> info


module AvaloniaDslType =
    open Avalonia

    let tryCreate (t: Type) =
        let info = TypeMemberInfo.ofType t

        if t.IsAssignableTo(typeof<AvaloniaObject>) then
            TypeMemberInfo.ofType t |> AssignableToAvanloniaObject |> Some
        else if TypeMemberInfo.hasAvaloniaPropertyOrRoutedEvent info then
            TypeMemberInfo.ofType t |> HasAvaloniaPropertyOrRoutedEvent |> Some
        else
            None

    let typeMemberInfo =
        function
        | AssignableToAvanloniaObject info -> info
        | HasAvaloniaPropertyOrRoutedEvent info -> info

    let typeInfo x = (typeMemberInfo x).TypeInfo
    let baseType x = (typeInfo x).BaseType


    let ofAssembly (asm: Assembly) =
        asm.GetExportedTypes() |> Array.choose tryCreate

    let printCodeBlock = typeMemberInfo >> TypeMemberInfo.sprintCodeBlock
