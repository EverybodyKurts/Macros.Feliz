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

    open Html

    [<ReactComponent>]
    let View() : ReactElement =
        let form, dispatch = React.useElmish(init, update, [| |])

        let bodyWeightFields = {
            Form = form
            UpdateWeightAmount = (fun updatedAmount -> dispatch (UpdateWeightAmount updatedAmount))
            SelectKgUnit = (fun _ -> dispatch ConvertAmountToKg)
            SelectLbUnit = (fun _ -> dispatch ConvertAmountToLb)
        }

        let bcHtml =
            match form.Validate() with
            | Ok bc -> bodyCompositionHtml bc
            | Error _ ->
                console.log form

                Html.div [
                    prop.text "Body composition not valid"
                ]

        fluidContainer [
            row [
                col [
                    bodyWeightFields.View
                    bodyfatPct (fun bfPct -> dispatch (UpdateBodyfatPercentage bfPct))
                    bcHtml
                ]
            ]
        ]