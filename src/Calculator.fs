namespace App

open Feliz
open Elmish
open Feliz.UseElmish
open Fable.React

open Bootstrap
open Library.Domain

module Calculator =
    type Msg =
        | Increment
        | Decrement
        | UpdateMassAmount of float
        | ConvertAmountToKg
        | ConvertAmountToLb

    type MassUnit =
        | Kg
        | Lb

        member this.IsKilogram : bool =
            match this with
            | Kg -> true
            | _ -> false

        member this.IsPound : bool =
            match this with
            | Lb -> true
            | _ -> false

    type State = { Count: int; Weight: Mass option; MassUnit: MassUnit }

    let init() = { Count = 0; Weight = None; MassUnit = Kg }, Cmd.none

    let update (msg: Msg) (state: State) : State * Cmd<'a> =
        match msg with
        | Increment -> { state with Count = state.Count + 1 }, Cmd.none
        | Decrement -> { state with Count = state.Count - 1 }, Cmd.none
        | UpdateMassAmount amount ->
            let weight =
                match state.Weight with
                | Some (Mass.Lb _) -> Some <| Mass.CreateLb amount
                | Some (Mass.Kg _) -> Some <| Mass.CreateKg amount
                | None ->
                    if state.MassUnit = Kg then Some <| Mass.CreateKg amount
                    else Some <| Mass.CreateLb amount

            { state with Weight = weight }, Cmd.none

        | ConvertAmountToKg ->
            { state with
                Weight = state.Weight |> Option.map(fun w -> w.ToKg())
                MassUnit = Kg
            }, Cmd.none

        | ConvertAmountToLb ->
            { state with
                Weight = state.Weight |> Option.map(fun w -> w.ToLb())
                MassUnit = Lb
            }, Cmd.none

    let massAmountInput (dispatch: Msg -> unit) (weight: Mass option) : ReactElement =
        let baseProperties = [
            prop.type' "number"
            prop.className "form-control"
            prop.onChange (fun updatedAmount -> dispatch (UpdateMassAmount updatedAmount))
        ]

        let valueProperty =
            match weight with
            | Some w -> [ prop.value w.AsFloat ]
            | _ -> []

        Html.input (baseProperties @ valueProperty)

    let weightKgOption (dispatch: Msg -> unit) (massUnit: MassUnit) : ReactElement list =
        let optionName = "kgUnitOption"

        [
            Html.input [
                prop.type' "radio"
                prop.className "btn-check"
                prop.name "weightUnitOptions"
                prop.id optionName
                prop.isChecked massUnit.IsKilogram
                prop.onClick (fun _ -> dispatch ConvertAmountToKg)
            ]
            Html.label [
                prop.classes ["btn"; "btn-secondary"]
                prop.for' optionName
                prop.text "Kg"
            ]
        ]

    let weightLbOption (dispatch: Msg -> unit) (massUnit: MassUnit) : ReactElement list =
        let optionName = "lbUnitOption"

        [
            Html.input [
                prop.type' "radio"
                prop.className "btn-check"
                prop.name "weightUnitOptions"
                prop.id optionName
                prop.isChecked massUnit.IsPound
                prop.onClick (fun _ -> dispatch ConvertAmountToLb)
            ]
            Html.label [
                prop.classes ["btn"; "btn-secondary"]
                prop.for' optionName
                prop.text "Lb"
            ]
        ]

    [<ReactComponent>]
    let View() : ReactElement =
        let state, dispatch = React.useElmish(init, update, [| |])

        let massAmountH1 (maybeMass: Mass option) =
            match maybeMass with
            | Some mass -> Html.h1 mass.AsFloat
            | None -> Html.h1 "no amount"

        fluidContainer [
            row [
                col [
                    Html.h1 state.Count
                    massAmountH1 state.Weight
                    Html.button [
                        prop.text "Increment"
                        prop.onClick (fun _ -> dispatch Increment)
                    ]

                    Html.button [
                        prop.text "Decrement"
                        prop.onClick (fun _ -> dispatch Decrement)
                    ]

                    Html.div [
                        prop.className "input-group"
                        prop.children ([massAmountInput dispatch state.Weight] @ (weightKgOption dispatch state.MassUnit) @ (weightLbOption dispatch state.MassUnit))
                    ]
                ]
            ]
        ]