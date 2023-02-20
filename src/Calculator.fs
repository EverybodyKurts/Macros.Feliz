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
    /// This module encapsulates logic for step 1 of the macros wizard
    /// It wires up the user interface and events for body weight and body fat percentage.
    module BodyComposition =
        open Html

        /// The actions the user can perform when entering in their body weight & fat %
        type Msg =
            | ``Update Weight Amount`` of float
            | ``Convert Amount To Kg``
            | ``Convert Amount To Lb``
            | ``Update Bodyfat Percentage`` of int

        let init() = BodyComposition.Default, Cmd.none

        /// Updates the body composition form based on a user action
        let update (msg: Msg) (form: Form.BodyComposition) : Form.BodyComposition * Cmd<'a> =
            match msg with
            | ``Update Weight Amount`` amount ->
                form.UpdateWeightAmount(amount), Cmd.none

            | ``Convert Amount To Kg`` ->
                form.ToKg(), Cmd.none

            | ``Convert Amount To Lb`` ->
                form.ToLb(), Cmd.none

            | ``Update Bodyfat Percentage`` bodyFatPct ->
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
                UpdateWeightAmount = (fun updatedAmount -> dispatch (``Update Weight Amount`` updatedAmount))
                SelectKgUnit = (fun _ -> dispatch ``Convert Amount To Kg``)
                SelectLbUnit = (fun _ -> dispatch ``Convert Amount To Lb``)
            }

            let bodyfatPctField = bodyfatPct (fun bfPct -> dispatch (``Update Bodyfat Percentage`` bfPct))

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