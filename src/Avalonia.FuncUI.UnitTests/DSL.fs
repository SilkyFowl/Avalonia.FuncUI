module Avalonia.FuncUI.UnitTests.DSL.DSLTests

open Avalonia.FuncUI.DSL.Utils

open Xunit

open System
open System.Reflection

[<Fact>]
let ``check TypeMemberInfo report`` () =
    let avaloniaAssembly = Assembly.Load("Avalonia")

    Assert.Equal("Avalonia", avaloniaAssembly.GetName().Name)

    let avaloniaDataGridAssembly = Assembly.Load("Avalonia.Controls.DataGrid")
    Assert.Equal("Avalonia.Controls.DataGrid", avaloniaDataGridAssembly.GetName().Name)

    List.distinct
        [ yield! avaloniaAssembly.GetReferencedAssemblies()
          yield! avaloniaDataGridAssembly.GetReferencedAssemblies() ]
    |> List.iter (fun name -> Assembly.Load(name) |> ignore)

    let refedAssemblies =
        let matchWith fn =
            let refed = fn avaloniaAssembly
            fun other -> (fn other) = refed

        let matchProduct =
            Assembly.tryGetCustomAttributeWith<AssemblyProductAttribute, string> (fun asm -> asm.Product)
            |> matchWith

        let matchCompany =
            Assembly.tryGetCustomAttributeWith<AssemblyCompanyAttribute, string> (fun asm -> asm.Company)
            |> matchWith

        let matchCopyright =
            Assembly.tryGetCustomAttributeWith<AssemblyCopyrightAttribute, string> (fun asm -> asm.Copyright)
            |> matchWith

        let matchFileVersion =
            Assembly.tryGetCustomAttributeWith<AssemblyFileVersionAttribute, string> (fun asm -> asm.Version)
            |> matchWith

        AppDomain.CurrentDomain.GetAssemblies()
        |> Array.filter (fun asm ->
            matchProduct asm
            && matchCompany asm
            && matchCopyright asm
            && matchFileVersion asm)

    Assert.Equal(6, refedAssemblies.Length)

    let avaloniaObjInheritTypes, havaAvaloniaPropTypes =
        refedAssemblies
        |> Array.collect AvaloniaDslType.ofAssembly
        |> Array.sortBy (fun x ->
            let info = x.TypeMemberInfo.TypeInfo
            info.Namespace, info.Name)
        |> Array.partition (function
            | AssignableToAvanloniaObject _ -> true
            | HasAvaloniaPropertyOrRoutedEvent _ -> false)

    let avaloniaObjInheritTree =
        avaloniaObjInheritTypes
        |> Array.map (fun x -> x.TypeMemberInfo)
        |> TypeMemberInfoTree.build

    let avaloniaObjInheritTreeStr = TypeMemberInfoTree.sprintMd avaloniaObjInheritTree

    let havaAvaloniaPropTypesStr =
        havaAvaloniaPropTypes
        |> Array.map (fun x ->
            let info = x.TypeMemberInfo
            String.concat
                "\n\n"
                [ $"## {TypeMemberInfo.sprintTypeName info} - havaAvaloniaPropTypes"
                  TypeMemberInfo.sprintCodeBlock info ])
        |> String.concat "\n\n"

    let concatStr = $"{avaloniaObjInheritTreeStr}\n\n{havaAvaloniaPropTypesStr}"

    Assert.NotEmpty(concatStr)