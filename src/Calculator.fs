namespace App

open Feliz
open Feliz.UseElmish
open Fable.React
open Elmish

open Browser

open Bootstrap
open Library.Form

module Calculator =
    type Msg =
        | UpdateWeightAmount of float
        | ConvertAmountToKg
        | ConvertAmountToLb
        | UpdateBodyfatPercentage of int

    type State = { BodyComposition: BodyComposition }

    let init() = { BodyComposition = BodyComposition.Default }, Cmd.none

    let update (msg: Msg) (state: State) : State * Cmd<'a> =
        match msg with
        | UpdateWeightAmount amount ->
            let bodyComposition = state.BodyComposition.UpdateWeightAmount amount

            { state with BodyComposition = bodyComposition }, Cmd.none

        | ConvertAmountToKg ->
            console.log "convert to kg"

            { state with
                BodyComposition = state.BodyComposition.ToKg()
            }, Cmd.none

        | ConvertAmountToLb ->
            console.log "convert to lb"

            let bc = state.BodyComposition.ToLb()

            console.log bc

            { state with
                BodyComposition = state.BodyComposition.ToLb()
            }, Cmd.none

        | UpdateBodyfatPercentage bodyFatPct ->
            { state with
                BodyComposition = state.BodyComposition.UpdateBodyfatPercentage(bodyFatPct)
            }, Cmd.none

    type BodyCompositionHtml = {
        BodyComposition: BodyComposition
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
                prop.onChange (fun updatedAmount -> this.Dispatch (UpdateWeightAmount updatedAmount))
            ]

            let valueProperty =
                match this.BodyComposition.Weight.Amount with
                | Some amount -> [ prop.value amount ]
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
                    prop.isChecked this.BodyComposition.Weight.Unit.IsKilogram
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
                    prop.isChecked this.BodyComposition.Weight.Unit.IsPound
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

    let bodyfatPctHtml : ReactElement =
        Html.div [
            prop.className "mb-3"
            prop.children [
                Html.label [
                    prop.for' "bodyfat-pct"
                    prop.className "form-label"
                    prop.text "Bodyfat %"
                ]
                Html.div [
                    prop.className "input-group"
                    prop.children [
                        Html.input [
                            prop.type' "text"
                            prop.className "form-control"
                            prop.placeholder "Enter your bodyfat %"
                            prop.ariaLabel "Bodyfat Percentage"
                            prop.ariaDescribedBy "bodyfat-pct"
                        ]

                        Html.span [
                            prop.className "input-group-text"
                            prop.id "bodyfat-pct"
                            prop.text "%"
                        ]
                    ]
                ]
            ]

        ]

    [<ReactComponent>]
    let View() : ReactElement =
        let state, dispatch = React.useElmish(init, update, [| |])

        let massAmountH1 (bodyComposition: BodyComposition) =
            match bodyComposition.Weight.Amount with
            | Some amt -> Html.h1 amt
            | None -> Html.h1 "no amount"

        let weightHtml = {
            BodyComposition = state.BodyComposition
            Dispatch = dispatch
        }

        fluidContainer [
            row [
                col [
                    massAmountH1 state.BodyComposition

                    weightHtml.View
                    bodyfatPctHtml
                ]
            ]
        ]