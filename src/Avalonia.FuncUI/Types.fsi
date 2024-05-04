namespace Avalonia.FuncUI

open Avalonia
open Avalonia.Controls
open System
open System.Threading
open System.Diagnostics.CodeAnalysis

module Types =
    [<CustomEquality; NoComparison; Struct>]
    type PropertyAccessor =
        { Name: string
          Getter: (AvaloniaObject -> obj) voption
          Setter: (AvaloniaObject * obj -> unit) voption }

        override Equals: other: obj -> bool
        override GetHashCode: unit -> int

    and Accessor =
        | InstanceProperty of PropertyAccessor
        | AvaloniaProperty of Avalonia.AvaloniaProperty

    and [<CustomEquality; NoComparison>] Property =
        { Accessor: Accessor
          Value: obj
          DefaultValueFactory: (unit -> obj) voption
          Comparer: (obj * obj -> bool) voption }

        override Equals: other: obj -> bool
        override GetHashCode: unit -> int

    and Content =
        { Accessor: Accessor
          Content: ViewContent }

    and ViewContent =
        | Single of IView option
        | Multiple of IView list

    and [<CustomEquality; NoComparison>] Subscription =
        { Name: string
          Subscribe: Control * Delegate -> CancellationTokenSource
          Func: Delegate
          FuncType: Type
          Scope: obj }

        override Equals: other: obj -> bool
        override GetHashCode: unit -> int

    and [<Struct; CustomEquality; NoComparison>] InitFunction =
        { Function: obj -> unit }

        override Equals: other: obj -> bool
        override GetHashCode: unit -> int

    and IAttr =
        abstract member UniqueName: string

        abstract member Property: Property voption
        abstract member Content: Content voption
        abstract member Subscription: Subscription voption
        abstract member InitFunction: InitFunction voption

    and [<Interface>] IAttr<'viewType> =
        inherit IAttr

    and Attr<'viewType> =
        | Property of Property
        | Content of Content
        | Subscription of Subscription
        | InitFunction of InitFunction

        interface IAttr<'viewType>
        interface IAttr

    and IView =
        abstract member ViewType: Type with get
        abstract member ViewKey: string voption
        abstract member Attrs: IAttr list with get
        abstract member ConstructorArgs: obj array with get
        abstract member Outlet: (AvaloniaObject -> unit) voption with get

    and IView<[<DynamicallyAccessedMembers(DynamicallyAccessedMemberTypes.PublicConstructors)>] 'viewType> =
        inherit IView
        abstract member Attrs: IAttr<'viewType> list with get

    and View<[<DynamicallyAccessedMembers(DynamicallyAccessedMemberTypes.PublicConstructors)>] 'viewType> =
        { ViewType: Type
          ViewKey: string voption
          Attrs: IAttr<'viewType> list
          ConstructorArgs: obj array
          Outlet: (AvaloniaObject -> unit) voption }

        interface IView
        interface IView<'viewType>

    [<return: Struct>]
    val internal (|Property'|_|): attr: IAttr -> Property voption

    [<return: Struct>]
    val internal (|Content'|_|): attr: IAttr -> Content voption

    [<return: Struct>]
    val internal (|Subscription'|_|): attr: IAttr -> Subscription voption

    [<return: Struct>]
    val internal (|InitFunction|_|): attr: IAttr -> InitFunction voption
