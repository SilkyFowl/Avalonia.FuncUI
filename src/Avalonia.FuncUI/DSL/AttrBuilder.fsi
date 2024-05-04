namespace Avalonia.FuncUI.DSL

[<NoComparison>]
[<Struct>]
type SubPatchOptions =
    /// Always updates the subscription. This should be used if you can't explicitly express your outer dependencies.
    | Always
    /// Never updates the subscription. This should be used most of the time. Use this if you don't depend on outer dependencies.
    | Never
    /// Update if 't changed. This is useful if your using some state ('t) and need to update the subscription if that state changed.
    | OnChangeOf of obj

    member internal ToScope: unit -> obj

namespace Avalonia.FuncUI.Builder

open System.Threading

open Avalonia
open Avalonia.Interactivity
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

type Comparer = obj * obj -> bool
type SubscriptionFactory<'arg> = AvaloniaObject * ('arg -> unit) * CancellationToken -> unit

[<AbstractClass; Sealed>]
type AttrBuilder<'view> =
    new: unit -> AttrBuilder<'view>

    /// Create a Property Attribute for an Avalonia Property
    static member CreateProperty<'value>:
        property: AvaloniaProperty<'value> * value: 'value * comparer: Comparer voption -> IAttr<'view>

    /// Create a Property Attribute for an Avalonia Property
    static member CreateProperty<'value>:
        property: AvaloniaProperty<'value> *
        value: 'value *
        comparer: Comparer voption *
        defaultValueFactory: (unit -> 'value) ->
            IAttr<'view>

    static member CreateProperty<'value>:
        property: AvaloniaProperty<ITemplate<Control>> * value: 'value * comparer: Comparer voption -> IAttr<'view>

    static member CreateProperty<'value>:
        property: AvaloniaProperty<obj> * value: string * comparer: Comparer voption -> IAttr<'view>

    /// Create a Property Attribute for an instance (non Avalonia) Property
    static member CreateProperty<'value>:
        name: string *
        value: 'value *
        getter: ('view -> 'value) voption *
        setter: ('view * 'value -> unit) voption *
        comparer: Comparer voption *
        defaultValueFactory: (unit -> 'value) ->
            IAttr<'view>

    /// <summary>
    /// Create a Property Attribute for an instance (non Avalonia) Property
    /// </summary>
    static member CreateProperty<'value>:
        name: string *
        value: 'value *
        getter: ('view -> 'value) voption *
        setter: ('view * 'value -> unit) voption *
        comparer: Comparer voption ->
            IAttr<'view>

    /// <summary>
    /// Create a Single Content Attribute for an Avalonia Property
    /// </summary>
    static member CreateContentSingle: property: AvaloniaProperty * singleContent: IView option -> IAttr<'view>

    /// <summary>
    /// Create a Single Content Attribute for an instance (non Avalonia) Property
    /// </summary>
    static member CreateContentSingle:
        name: string *
        getter: ('view -> obj) voption *
        setter: ('view * obj -> unit) voption *
        singleContent: IView option ->
            IAttr<'view>

    /// <summary>
    /// Create a Multiple Content Attribute for an Avalonia Property
    /// </summary>
    static member CreateContentMultiple: property: AvaloniaProperty * multipleContent: IView list -> IAttr<'view>

    /// <summary>
    /// Create a Multiple Content Attribute for an instance (non Avalonia) Property
    /// </summary>
    static member CreateContentMultiple:
        name: string *
        getter: ('view -> obj) voption *
        setter: ('view * obj -> unit) voption *
        multipleContent: IView list ->
            IAttr<'view>

    /// <summary>
    /// Create a Property Subscription Attribute for an Avalonia Direct Property
    /// </summary>
    static member CreateSubscription<'arg, 'owner when 'owner :> AvaloniaObject> :
        property: DirectProperty<'owner , 'arg> * func: ('arg -> unit) * ?subPatchOptions: SubPatchOptions ->
            IAttr<'view>

    /// <summary>
    /// Create a Property Subscription Attribute for an Avalonia Property
    /// </summary>
    static member CreateSubscription<'arg>:
        property: AvaloniaProperty<'arg> * func: ('arg -> unit) * ?subPatchOptions: SubPatchOptions -> IAttr<'view>

    /// <summary>
    /// Create a Routed Event Subscription Attribute for a Routed Event
    /// </summary>
    static member CreateSubscription<'arg when 'arg :> RoutedEventArgs>:
        routedEvent: RoutedEvent<'arg> * func: ('arg -> unit) * ?subPatchOptions: SubPatchOptions -> IAttr<'view>
            when 'arg :> RoutedEventArgs

    /// <summary>
    /// Create a Event Subscription Attribute for a .Net Event
    /// </summary>
    static member CreateSubscription<'arg>:
        name: string *
        factory: (AvaloniaObject * ('arg -> unit) * CancellationToken -> unit) *
        func: ('arg -> unit) *
        ?subPatchOptions: SubPatchOptions ->
            IAttr<'view>
