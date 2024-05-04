namespace Avalonia.FuncUI.DSL

open System.Diagnostics.CodeAnalysis
open Avalonia.FuncUI.Types

[<AbstractClass; Sealed>]
type ViewBuilder =
    new: unit -> ViewBuilder
    static member Create<
        [<DynamicallyAccessedMembers(DynamicallyAccessedMemberTypes.PublicConstructors)>]
        'view>:
        attrs: IAttr<'view> list -> IView<'view>
