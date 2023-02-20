namespace App

open Feliz
open Feliz.UseElmish
open Fable.React
open Elmish

open Browser

open Bootstrap
open Library
open Library.Form

module Calculator =
    module BodyComposition =
        open Html

        type Msg =
            | UpdateWeightAmount of float
            | ConvertAmountToKg
            | ConvertAmountToLb
            | UpdateBodyfatPercentage of int

        let init() = BodyComposition.Default, Cmd.none

        let update (msg: Msg) (form: Form.BodyComposition) : Form.BodyComposition * Cmd<'a> =
            match msg with
            | UpdateWeightAmount amount ->
                form.UpdateWeightAmount(amount), Cmd.none

            | ConvertAmountToKg ->
                form.ToKg(), Cmd.none

            | ConvertAmountToLb ->
                form.ToLb(), Cmd.none

            | UpdateBodyfatPercentage bodyFatPct ->
                form.UpdateBodyfatPercentage(bodyFatPct), Cmd.none

        let bodyCompositionHtml (bodyComposition: Domain.BodyComposition) =
            Html.div [
                Html.div [
                    prop.text "Lean Muscle Mass:"
                ]
                Html.div [
                    prop.text bodyComposition.LeanMuscleMass.Text
                ]
            ]

        let view(form: Form.BodyComposition, dispatch: 'a -> unit) : ReactElement list =
            let bodyWeightFields = {
                Form = form
                UpdateWeightAmount = (fun updatedAmount -> dispatch (UpdateWeightAmount updatedAmount))
                SelectKgUnit = (fun _ -> dispatch ConvertAmountToKg)
                SelectLbUnit = (fun _ -> dispatch ConvertAmountToLb)
            }

            let bodyfatPctField = bodyfatPct (fun bfPct -> dispatch (UpdateBodyfatPercentage bfPct))

            let bcHtml =
                match form.Validate() with
                | Ok bc -> bodyCompositionHtml bc
                | Error _ ->
                    console.log form

                    Html.div [
                        prop.text "Body composition not valid"
                    ]

            [
                bodyWeightFields.View
                bodyfatPctField
                bcHtml
            ]

    type Msg =
        | BodyCompositionMsg of BodyComposition.Msg

    type State =
        | BodyCompositionStep of Form.BodyComposition
        // | DailyActivityStep of Domain.BodyComposition * Domain.DailyActivityLevel

    let init() = BodyCompositionStep BodyComposition.Default, Cmd.none

    let update (msg: Msg) (state: State) : State * Cmd<'a> =
        match msg, state with
        | BodyCompositionMsg msg, BodyCompositionStep bodyCompositionForm ->
            let (updated, cmd) = BodyComposition.update msg bodyCompositionForm

            let state = BodyCompositionStep updated

            state, Cmd.map BodyCompositionMsg cmd

    [<ReactComponent>]
    let View() : ReactElement =
        let form, dispatch = React.useElmish(init, update, [| |])

        let htmlElements =
            match form with
            | BodyCompositionStep form ->
                let bcDispatch = BodyCompositionMsg >> dispatch

                BodyComposition.view(form, bcDispatch)

        fluidContainer [
            row [
                col htmlElements
            ]
        ]