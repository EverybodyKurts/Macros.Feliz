namespace App

open Browser
open Browser.Dom
open Feliz
open Elmish
open Feliz.UseElmish
open Fable.React

open FsToolkit.ErrorHandling

open Bootstrap
open Library
open Library.Input

module Calculator =
    open Html

    /// This module encapsulates logic for step 1 of the macros wizard
    /// It wires up the user interface and events for body weight and body fat percentage.
    module BodyComposition =

        /// The actions the user can perform when entering in their body weight & fat %
        type Msg =
            | ``Update Weight Amount`` of float
            | ``Convert Amount To Kg``
            | ``Convert Amount To Lb``
            | ``Update Bodyfat Percentage`` of int
            | ``Proceed to Next Step`` of Domain.BodyComposition

        /// Updates the body composition form based on a user action
        let update (msg: Msg) (form: Input.BodyComposition) : Input.BodyComposition * Cmd<'a> =
            match msg with
            | ``Update Weight Amount`` amount ->
                form.UpdateWeightAmount(amount), Cmd.none

            | ``Convert Amount To Kg`` ->
                form.ToKg(), Cmd.none

            | ``Convert Amount To Lb`` ->
                form.ToLb(), Cmd.none

            | ``Update Bodyfat Percentage`` bodyFatPct ->
                form.UpdateBodyfatPercentage(bodyFatPct), Cmd.none

            | ``Proceed to Next Step`` _ ->
                invalidOp "Message shouldn't reach here"

        let bodyCompositionHtml (bodyComposition: Domain.BodyComposition) : ReactElement =
            Html.div [
                Html.div [
                    prop.text "Lean Muscle Mass:"
                ]
                Html.div [
                    prop.text bodyComposition.LeanMuscleMass.Text
                ]
            ]

        let view(form: Input.BodyComposition, dispatch: 'a -> unit) : ReactElement list =
            let bcHtml =
                match form.Validate() with
                | Ok bc ->
                    bodyCompositionHtml bc
                | Error _ ->
                    console.log form

                    Html.div [
                        prop.text "Body composition not valid"
                    ]

            let nextStepHandler =
                option {
                    let! bodyComposition = form.Validate() |> Utilities.Result.toOption
                    return (fun _ -> dispatch (``Proceed to Next Step`` bodyComposition))
                }

            let bodyCompositionFields = BodyComposition.Fields.CreateEnabled(
                form = form,
                updateWeightAmount = (fun updatedAmount -> dispatch (``Update Weight Amount`` updatedAmount)),
                updateBodyfatPercentage = (fun bfPct -> dispatch (``Update Bodyfat Percentage`` bfPct)),
                selectKgUnit = (fun _ -> dispatch ``Convert Amount To Kg``),
                selectLbUnit = (fun _ -> dispatch ``Convert Amount To Lb``),
                ?proceedToNextStep = nextStepHandler
            )

            [
                bodyCompositionFields.Card
                bcHtml
            ]

    module DailyMacros =
        /// The actions the user can perform when entering in their body weight & fat %
        type Msg =
            | ``Select Activity Level`` of activityLevel: string
            | ``Select Protein Grams Per Kg Lean Body Mass`` of grams: float

        /// Updates the daily activity form
        let update (msg: Msg) (form:  Input.DailyMacros) : Input.DailyMacros * Cmd<'a> =
            match msg with
            | ``Select Activity Level`` dailyActivityLevel ->
                { form with DailyActivityLevel = dailyActivityLevel }, Cmd.none

            | ``Select Protein Grams Per Kg Lean Body Mass`` proteinGrams ->
                { form with ProteinGramsPerKgLeanBodyMass = proteinGrams }, Cmd.none

        let view(form: Input.DailyMacros, dispatch: 'a -> unit) : ReactElement =
            let eventHandlers = ({
                SelectActivityLevel = (fun event -> dispatch (``Select Activity Level`` event.Value))
                ChangeProteinGrams = (fun grams -> dispatch (``Select Protein Grams Per Kg Lean Body Mass`` grams))
            } : DailyMacros.EventHandlers)

            let dailyMacrosFields = DailyMacros.Fields.CreateEnabled(
                form = form,
                eventHandlers = eventHandlers
            )

            dailyMacrosFields.Card

    type Msg =
        | BodyCompositionMsg of BodyComposition.Msg
        | DailyMacrosMsg of DailyMacros.Msg

    type State =
        | BodyCompositionStep of form: Input.BodyComposition
        | DailyMacrosStep of form: Input.DailyMacros

    let init() = BodyCompositionStep BodyComposition.Default, Cmd.none

    let update (msg: Msg) (state: State) : State * Cmd<'a> =
        match msg, state with
        // proceed from body composition form to daily activity form
        | BodyCompositionMsg (BodyComposition.``Proceed to Next Step`` bodyComposition), _ ->
            let dailyMacrosForm = Input.DailyMacros.Create(bodyComposition)
            DailyMacrosStep dailyMacrosForm, Cmd.none

        | BodyCompositionMsg msg, BodyCompositionStep bodyCompositionForm ->
            let (updated, cmd) = BodyComposition.update msg bodyCompositionForm

            let state = BodyCompositionStep updated

            state, Cmd.map BodyCompositionMsg cmd

        | DailyMacrosMsg msg, DailyMacrosStep dailyMacrosForm ->
            let (updated, cmd) = DailyMacros.update msg dailyMacrosForm

            let state = DailyMacrosStep updated

            state, Cmd.map DailyMacrosMsg cmd

    [<ReactComponent>]
    let View() : ReactElement =
        let form, dispatch = React.useElmish(init, update, [| |])

        let htmlElements =
            match form with
            | BodyCompositionStep form ->
                let bcDispatch = BodyCompositionMsg >> dispatch

                [
                    yield! BodyComposition.view(form, bcDispatch)
                    yield (DailyMacros.Fields.CreateDisabled().Card)
                ]

            | DailyMacrosStep form ->
                let dmDispatch = DailyMacrosMsg >> dispatch

                [
                    yield BodyComposition.Fields.CreateDisabled(form = (form.BodyComposition |> Input.BodyComposition.Create)).Card
                    yield DailyMacros.view(form, dmDispatch)
                ]

        fluidContainer [
            row [
                col htmlElements
            ]
        ]