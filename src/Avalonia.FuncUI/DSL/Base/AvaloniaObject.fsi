namespace Avalonia.FuncUI.DSL

open Avalonia
open Avalonia.FuncUI
open Avalonia.FuncUI.Types
open System.Threading

[<AutoOpen>]
module AvaloniaObject =
    open Avalonia.FuncUI.Types
    open Avalonia.FuncUI.Builder

    type AvaloniaObject with
        /// <summary>
        /// Hook into the controls lifetime. This is called when the backing avalonia control is created.
        /// <example>
        /// <code>
        /// TextBlock.create [
        ///     TextBlock.init (fun textBlock -&gt;
        ///         textBlock.Bind(TextBlock.TextProperty, counter.Map string)
        ///         textBlock.Bind(TextBlock.ForegroundProperty, counter.Map (fun c -&gt;
        ///             if c &lt; 0
        ///             then Brushes.Red :&gt; IBrush
        ///             else Brushes.Green :&gt; IBrush
        ///         ))
        ///     )
        /// ]
        /// </code>
        /// </example>
        /// </summary>/
        static member init<'t when 't :> AvaloniaObject>: func: ('t -> unit) -> IAttr<'t>

        static member onPropertyChanged<'t when 't :> AvaloniaObject>:
            func: (AvaloniaPropertyChangedEventArgs -> unit) *
            ?subPatchOptions: SubPatchOptions ->
                IAttr<'t>

        member Bind: prop: DirectPropertyBase<'value> * readable: #IReadable<'value> -> unit
        member Bind: prop: StyledProperty<'value> * readable: #IReadable<'value> -> unit
