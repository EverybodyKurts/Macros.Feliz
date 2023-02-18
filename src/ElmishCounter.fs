namespace App

open Feliz
open Feliz.UseElmish
open Elmish

module Elmish =
    type Msg =
        | Increment
        | Decrement

    type State = { Count: int }

    let init() = { Count = 0 }, Cmd.none

    let update (msg: Msg) (state: State) : State * Cmd<'a> =
        match msg with
        | Increment -> { state with Count = state.Count + 1 }, Cmd.none
        | Decrement -> { state with Count = state.Count - 1 }, Cmd.none

    [<ReactComponent>]
    let Counter() : Fable.React.ReactElement =
        let state, dispatch = React.useElmish(init, update, [| |])
        Html.div [
            Html.h1 state.Count
            Html.button [
                prop.text "Increment"
                prop.onClick (fun _ -> dispatch Increment)
            ]

            Html.button [
                prop.text "Decrement"
                prop.onClick (fun _ -> dispatch Decrement)
            ]
        ]