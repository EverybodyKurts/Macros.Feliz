namespace App

open Feliz
open Feliz.UseElmish
open Fable.React
open Elmish

open FsToolkit.ErrorHandling

open Bootstrap
open Library.Domain

module Calculator =
    type Msg =
        | Increment
        | Decrement
        | UpdateMassAmount of float
        | ConvertAmountToKg
        | ConvertAmountToLb

    type State = { Count: int; Weight: Mass option; MassUnit: MassUnit }

    let init() = { Count = 0; Weight = None; MassUnit = MassUnit.Kg }, Cmd.none

    let update (msg: Msg) (state: State) : State * Cmd<'a> =
        match msg with
        | Increment -> { state with Count = state.Count + 1 }, Cmd.none
        | Decrement -> { state with Count = state.Count - 1 }, Cmd.none
        | UpdateMassAmount amount ->
            let weight =
                option {
                    let! w = state.Weight

                    return w.UpdateAmount(amount)
                }
                |> Option.defaultValue (Mass.Create(amount, state.MassUnit))

            { state with Weight = Some weight }, Cmd.none

        | ConvertAmountToKg ->
            { state with
                Weight = state.Weight |> Option.map(fun w -> w.ToKg())
                MassUnit = MassUnit.Kg
            }, Cmd.none

        | ConvertAmountToLb ->
            { state with
                Weight = state.Weight |> Option.map(fun w -> w.ToLb())
                MassUnit = MassUnit.Lb
            }, Cmd.none

    let weightAmountLabel (inputHtmlId: string) : ReactElement =
        Html.label [
            prop.for' inputHtmlId
            prop.className "form-label"
            prop.text "Body Weight"
        ]

    let weightAmountInput (dispatch: Msg -> unit) (inputHtmlId: string) (weight: Mass option) : ReactElement =
        let baseProperties = [
            prop.id inputHtmlId
            prop.type' "number"
            prop.className "form-control"
            prop.placeholder "How much do you weigh?"
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

    let weightAmountHtml (dispatch: Msg -> unit) (weight: Mass option) (massUnit: MassUnit ): ReactElement =
        let inputId = "mass-amount-input"

        let inputHtmls =
            [ weightAmountInput dispatch inputId weight ]
            @ weightLbOption dispatch massUnit
            @ weightKgOption dispatch massUnit

        Html.div [
            prop.className "mb-3"
            prop.children [
                weightAmountLabel inputId

                Html.div [
                    prop.className "input-group"
                    prop.children inputHtmls
                ]
            ]
        ]

    [<ReactComponent>]
    let View() : ReactElement =
        let state, dispatch = React.useElmish(init, update, [| |])

        let massAmountH1 (maybeMass: Mass option) =
            match maybeMass with
            | Some mass -> Html.h1 mass.AsFloat
            | None -> Html.h1 "no amount"

        let weightAmountHtml =
            weightAmountHtml dispatch state.Weight state.MassUnit

        fluidContainer [
            row [
                col [
                    massAmountH1 state.Weight

                    weightAmountHtml
                ]
            ]
        ]