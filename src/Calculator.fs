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
                form.UpdateBodyfatPercentage (bodyFatPct), Cmd.none

            | ``Proceed to Next Step`` _ ->
                invalidOp "Message shouldn't reach here"

        let view (input: Input.BodyComposition, dispatch: 'a -> unit) : ReactElement list =
            let nextStepHandler =
                option {
                    let! bodyComposition = input.Validate() |> Utilities.Result.toOption
                    return (fun _ -> dispatch (``Proceed to Next Step`` bodyComposition))
                }

            let bodyCompositionFields = BodyComposition.Fields.CreateEnabled(
                bodyComposition = input,
                updateWeightAmount = (fun updatedAmount -> dispatch (``Update Weight Amount`` updatedAmount)),
                updateBodyfatPercentage = (fun bfPct -> dispatch (``Update Bodyfat Percentage`` bfPct)),
                selectKgUnit = (fun _ -> dispatch ``Convert Amount To Kg``),
                selectLbUnit = (fun _ -> dispatch ``Convert Amount To Lb``),
                ?proceedToNextStep = nextStepHandler
            )

            [
                bodyCompositionFields.Card
            ]

    module DailyMacros =
        /// The actions the user can perform when entering in their body weight & fat %
        type Msg =
            | ``Select Activity Level`` of activityLevel: string
            | ``Update Protein Macro Percentage`` of pct: float
            | ``Update Carb Macro Percentage`` of pct: float

        /// Updates the daily activity form
        let update (msg: Msg) (form: Input.DailyMacros) : Input.DailyMacros * Cmd<'a> =
            match msg with
            | ``Select Activity Level`` dailyActivityLevel ->
                { form with DailyActivityLevel = dailyActivityLevel }, Cmd.none

            | ``Update Protein Macro Percentage`` proteinPct ->
                let percentages = form.Percentages.UpdateProtein proteinPct
                { form with Percentages = percentages }, Cmd.none

            | ``Update Carb Macro Percentage`` carbPct ->
                let percentages = form.Percentages.UpdateCarbs carbPct
                { form with Percentages = percentages }, Cmd.none

        let view (input: Input.DailyMacros, dispatch: 'a -> unit) : ReactElement =
            let (eventHandlers: DailyMacros.EventHandlers) = {
                SelectActivityLevel = (fun event -> dispatch (``Select Activity Level`` event.Value))
                ChangeProteinPercentage = dispatch << ``Update Protein Macro Percentage``
                ChangeCarbPercentage = dispatch << ``Update Carb Macro Percentage``
            }

            let dailyMacrosFields = DailyMacros.Fields.CreateEnabled(
                input = input,
                eventHandlers = eventHandlers
            )

            dailyMacrosFields.Card

    type Msg =
        | BodyCompositionMsg of BodyComposition.Msg
        | DailyMacrosMsg of DailyMacros.Msg

    type State =
        | BodyCompositionStep of form: Input.BodyComposition
        | DailyMacrosStep of form: Input.DailyMacros

        member this.TryBodyComposition : Domain.BodyComposition option =
            match this with
            | BodyCompositionStep form ->
                form.Validate() |> Utilities.Result.toOption
            | DailyMacrosStep form ->
                Some form.BodyComposition

        member this.TryDailyMacros : Domain.DailyMacros option =
            match this with
            | BodyCompositionStep _ -> None
            | DailyMacrosStep form ->
                console.log $"{form.Validate()}"
                form.Validate() |> Utilities.Result.toOption

    let init() = BodyCompositionStep BodyComposition.Default, Cmd.none

    let update (msg: Msg) (state: State) : State * Cmd<'a> =
        match msg, state with
        // proceed from body composition form to daily activity form
        | BodyCompositionMsg (BodyComposition.``Proceed to Next Step`` bodyComposition), _ ->
            let dailyMacrosForm = Input.DailyMacros.Create (bodyComposition)
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
    let View () : ReactElement =
        let form, dispatch = React.useElmish(init, update, [| |])

        let inputs =
            match form with
            | BodyCompositionStep bodyComposition ->
                let bcDispatch = BodyCompositionMsg >> dispatch

                [
                    yield! BodyComposition.view(bodyComposition, bcDispatch)
                    DailyMacros.Fields.CreateDisabled().Card
                ]

            | DailyMacrosStep dailyMacros ->
                let dmDispatch = DailyMacrosMsg >> dispatch

                [
                    BodyComposition.Fields.CreateDisabled(bodyComposition = (dailyMacros.BodyComposition |> Input.BodyComposition.Create)).Card
                    DailyMacros.view(dailyMacros, dmDispatch)
                ]

        let results = Html.CalculatorResults.Create(
            ?bodyComposition = form.TryBodyComposition,
            ?dailyMacros = form.TryDailyMacros
        )

        fluidContainer [
            row [
                ``col-4`` inputs
                col [
                    results.BodyCompositionCard
                    results.DailyMacrosCard
                ]
            ]
        ]