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

    type WeightHtml = {
        Weight: Mass option
        WeightUnit: MassUnit
        Dispatch: Msg -> unit
    } with
        member private _.InputHtmlId = "form-control"

        member this.Label : ReactElement =
            Html.label [
                prop.for' this.InputHtmlId
                prop.className "form-label"
                prop.text "Body Weight"
            ]

        member this.Input : ReactElement =
            let baseProperties = [
                prop.id this.InputHtmlId
                prop.type' "number"
                prop.className "form-control"
                prop.placeholder "How much do you weigh?"
                prop.onChange (fun updatedAmount -> this.Dispatch (UpdateMassAmount updatedAmount))
            ]

            let valueProperty =
                match this.Weight with
                | Some w -> [ prop.value w.AsFloat ]
                | _ -> []

            Html.input (baseProperties @ valueProperty)

        member this.WeightKgOption : ReactElement list =
            let optionName = "kgUnitOption"

            [
                Html.input [
                    prop.type' "radio"
                    prop.className "btn-check"
                    prop.name "weightUnitOptions"
                    prop.id optionName
                    prop.isChecked this.WeightUnit.IsKilogram
                    prop.onClick (fun _ -> this.Dispatch ConvertAmountToKg)
                ]
                Html.label [
                    prop.classes ["btn"; "btn-secondary"]
                    prop.for' optionName
                    prop.text "Kg"
                ]
            ]

        member this.WeightLbOption : ReactElement list =
            let optionName = "lbUnitOption"

            [
                Html.input [
                    prop.type' "radio"
                    prop.className "btn-check"
                    prop.name "weightUnitOptions"
                    prop.id optionName
                    prop.isChecked this.WeightUnit.IsPound
                    prop.onClick (fun _ -> this.Dispatch ConvertAmountToLb)
                ]
                Html.label [
                    prop.classes ["btn"; "btn-secondary"]
                    prop.for' optionName
                    prop.text "Lb"
                ]
            ]

        member this.View : ReactElement =
            let inputHtmls =
                [ this.Input ]
                @ this.WeightKgOption
                @ this.WeightLbOption

            Html.div [
                prop.className "mb-3"
                prop.children [
                    this.Label

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

        let weightHtml = {
            Weight =  state.Weight
            WeightUnit = state.MassUnit
            Dispatch = dispatch
        }

        fluidContainer [
            row [
                col [
                    massAmountH1 state.Weight

                    weightHtml.View
                ]
            ]
        ]