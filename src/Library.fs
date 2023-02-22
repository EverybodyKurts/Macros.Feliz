namespace App

module Library =
    open FSharp.Core.Fluent

    [<AutoOpen>]
    module UnitsOfMeasure =
        [<Measure>] type g
        [<Measure>] type kg
        [<Measure>] type lb
        [<Measure>] type pct
        [<Measure>] type kcal

        let lbPerKg : float<lb/kg> = 2.20462262<lb/kg>
        let kgPerLb : float<kg/lb> = 1.0 / lbPerKg

    module Macronutrients =
        [<Literal>]
        let ProteinCaloriesPerGram : float<kcal/g> = 4.0<kcal/g>
        [<Literal>]
        let CarbCaloriesPerGram : float<kcal/g> = 4.0<kcal/g>
        [<Literal>]
        let FatCaloriesPerGram : float<kcal/g> = 9.0<kcal/g>

    module Domain =
        type Mass =
            | Lb of lb : float<lb>
            | Kg of kg : float<kg>

            static member (-) (mass1: Mass, mass2: Mass) : Mass =
                match mass1, mass2 with
                | Lb lb1, Lb lb2 -> Lb <| lb1 - lb2
                | Lb lb, Kg kg -> Lb <| lb - (kg * lbPerKg)
                | Kg kg1, Kg kg2 -> Kg <| kg1 - kg2
                | Kg kg, Lb lb -> Kg <| kg - (lb * kgPerLb)

            member this.ToLb() : Mass =
                match this with
                | Kg kg -> Lb <| kg * lbPerKg
                | _ -> this

            member this.ToKg() : Mass =
                match this with
                | Lb lb -> Kg <| lb * kgPerLb
                | _ -> this

            static member CreateLb(amount: float) : Mass =
                Lb <| amount * 1.0<lb>

            static member CreateKg(amount: float) : Mass =
                Kg <| amount * 1.0<kg>

            member this.Text : string =
                match this with
                | Kg kg -> $"{kg} kg"
                | Lb lb -> $"{lb} lb"

            member this.KgMeasure : float<kg> =
                match this.ToKg() with
                | Kg kg -> kg

        /// Calculate basal metabolic rate with the Katch-McArdle formula
        let basalMetabolicRate (leanBodyMass: float<kg>) : float<kcal> =
            370.0<kcal> + (21.6<kcal/kg> * leanBodyMass)

        type BodyComposition = {
            BodyWeight: Mass
            BodyfatPercentage: uint<pct>
        } with
            member this.FatMass : Mass =
                let bf = float this.BodyfatPercentage / 100.0

                match this.BodyWeight with
                | Kg kg -> Kg <| kg * bf
                | Lb lb -> Lb <| lb * bf

            member this.LeanMuscleMass : Mass =
                this.BodyWeight - this.FatMass

            member this.BasalMetabolicRate : float<kcal> =
                match this.LeanMuscleMass.ToKg() with
                | Kg kg -> basalMetabolicRate kg

        type DailyActivityLevel =
            | Sedentary
            | ``Mostly Sedentary``
            | ``Lightly Active``
            | ``Highly Active``

            member this.Multipliier : float =
                match this with
                | Sedentary -> 1.15
                | ``Mostly Sedentary`` -> 1.35
                | ``Lightly Active`` -> 1.55
                | ``Highly Active`` -> 1.75

        type DailyCaloricExpenditure = {
            BodyComposition: BodyComposition
            DailyActivityLevel: DailyActivityLevel
        } with
            member this.Total : float<kcal> =
                this.DailyActivityLevel.Multipliier * this.BodyComposition.BasalMetabolicRate

        type DailyMacronutrient = {
            Grams: float<g>
            Calories: float<kcal>
            Percentage: float<pct>
        }

        type DailyMacros = {
            DailyCaloricExpenditure: DailyCaloricExpenditure
            ProteinGramsPerKgLeanBodyMass: float<g/kg>
        } with

            member this.BodyComposition : BodyComposition =
                this.DailyCaloricExpenditure.BodyComposition

            member this.LeanMuscleMass : Mass =
                this.BodyComposition.LeanMuscleMass

            member this.Protein : DailyMacronutrient =
                let grams = this.LeanMuscleMass.KgMeasure * this.ProteinGramsPerKgLeanBodyMass
                let cals = grams * Macronutrients.ProteinCaloriesPerGram
                let percentage = (cals / this.DailyCaloricExpenditure.Total) * 100.0<pct>

                {
                    Grams = grams
                    Calories = cals
                    Percentage = percentage
                }

            member this.Fat : DailyMacronutrient =
                let calories = (this.DailyCaloricExpenditure.Total - this.Protein.Calories) / 2.0
                let grams = calories / Macronutrients.FatCaloriesPerGram
                let percentage = (calories / this.DailyCaloricExpenditure.Total) * 100.0<pct>

                {
                    Grams = grams
                    Calories = calories
                    Percentage = percentage
                }

            member this.Carbs : DailyMacronutrient =
                let calories = (this.DailyCaloricExpenditure.Total - this.Protein.Calories) / 2.0
                let grams = calories / Macronutrients.CarbCaloriesPerGram
                let percentage = (calories / this.DailyCaloricExpenditure.Total) * 100.0<pct>

                {
                    Grams = grams
                    Calories = calories
                    Percentage = percentage
                }


    module Form =
        open FsToolkit.ErrorHandling

        type WeightUnit =
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

        type Weight = {
            Amount: float option
            Unit: WeightUnit
        } with
            static member Create(weight: Domain.Mass) : Weight =
                match weight with
                | Domain.Mass.Kg kg ->
                    {
                        Amount = Some <| float kg
                        Unit = Kg
                    }
                | Domain.Mass.Lb lb ->
                    {
                        Amount = Some <| float lb
                        Unit = Lb
                    }

            static member Default : Weight =
                {
                    Amount = None
                    Unit = Lb
                }

            member this.Validate() : Result<Domain.Mass, string> =
                match this.Amount, this.Unit with
                | Some amount, Lb when amount >= 0 -> Ok <| Domain.Mass.CreateLb amount
                | Some amount, Kg when amount >= 0 -> Ok <| Domain.Mass.CreateKg amount
                | Some _, _ -> Error "Weight amount must be >= 0"
                | None, _ -> Error "Weight amount must be present"

            member this.ToKg() : Weight =
                match this.Validate() with
                | Ok weight -> weight.ToKg() |> Weight.Create
                | Error _ -> { this with Unit = Kg }

            member this.ToLb() : Weight =
                match this.Validate() with
                | Ok weight -> weight.ToLb() |> Weight.Create
                | Error _ -> { this with Unit = Lb }

        type BodyComposition = {
            Weight: Weight
            BodyfatPercentage: int option
        } with
            static member Default : BodyComposition =
                {
                    Weight = Weight.Default
                    BodyfatPercentage = None
                }

            member private this.ValidatePercentage() : Result<uint<pct>, string> =
                let bodyfatPercentages = [ 0 .. 100 ]

                match this.BodyfatPercentage with
                | Some bfPct when bodyfatPercentages |> List.contains bfPct ->
                    Ok <| (uint bfPct) * 1u<pct>
                | Some _ -> Error "Bodyfat % must be betwen 0 and 100"
                | None -> Error "Bodyfat % must be present"

            member this.Validate() : Validation<Domain.BodyComposition, string> =
                validation {
                    let! weight = this.Weight.Validate()
                    and! bfPct = this.ValidatePercentage()

                    return {
                        BodyWeight = weight
                        BodyfatPercentage = bfPct
                    }
                }

            member this.ToKg() : BodyComposition =
                { this with
                    Weight = this.Weight.ToKg()
                }

            member this.ToLb() : BodyComposition =
                { this with
                    Weight = this.Weight.ToLb()
                }

            member this.UpdateWeightAmount (amount: float) : BodyComposition =
                let weight = {
                    this.Weight with
                        Amount = Some amount
                }

                { this with Weight = weight }

            member this.UpdateBodyfatPercentage (percentage: int) : BodyComposition =
                { this with BodyfatPercentage = Some percentage }

    module Html =
        open Feliz

        type BodyCompositionFields = {
            Form: Form.BodyComposition
            UpdateWeightAmount: float -> unit
            UpdateBodyfatPercentage: int -> unit
            SelectKgUnit: Browser.Types.MouseEvent -> unit
            SelectLbUnit: Browser.Types.MouseEvent -> unit
            ProceedToNextStep: (Browser.Types.MouseEvent -> unit) option
        } with
            static member Create(form: Form.BodyComposition,
                                 updateWeightAmount: float -> unit,
                                 updateBodyfatPercentage: int -> unit,
                                 selectKgUnit: Browser.Types.MouseEvent -> unit,
                                 selectLbUnit: Browser.Types.MouseEvent -> unit,
                                 ?proceedToNextStep: Browser.Types.MouseEvent -> unit
                                ): BodyCompositionFields =

                {
                    Form = form
                    UpdateWeightAmount = updateWeightAmount
                    UpdateBodyfatPercentage = updateBodyfatPercentage
                    SelectKgUnit = selectKgUnit
                    SelectLbUnit = selectLbUnit
                    ProceedToNextStep = proceedToNextStep
                }

            member private _.InputHtmlId = "body-weight-amount-input"

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
                    prop.min 0
                    prop.className "form-control"
                    prop.placeholder "How much do you weigh?"
                    prop.onChange this.UpdateWeightAmount
                ]

                let valueProperty =
                    match this.Form.Weight.Amount with
                    | Some amount -> [ prop.value amount ]
                    | _ -> []

                Html.input (baseProperties @ valueProperty)

            member private _.WeightUnitOptions = "weightUnitOptions"

            member this.WeightKgOption : ReactElement list =
                let optionName = "kgUnitOption"

                [
                    Html.input [
                        prop.type' "radio"
                        prop.className "btn-check"
                        prop.name this.WeightUnitOptions
                        prop.id optionName
                        prop.isChecked this.Form.Weight.Unit.IsKilogram
                        prop.onClick this.SelectKgUnit
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
                        prop.name this.WeightUnitOptions
                        prop.id optionName
                        prop.isChecked this.Form.Weight.Unit.IsPound
                        prop.onClick this.SelectLbUnit
                    ]
                    Html.label [
                        prop.classes ["btn"; "btn-secondary"]
                        prop.for' optionName
                        prop.text "Lb"
                    ]
                ]

            member this.BodyWeightFields : ReactElement =
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

            member this.BodyfatPercentage : ReactElement =
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
                                    prop.type' "number"
                                    prop.min 0
                                    prop.max 100
                                    prop.className "form-control"
                                    prop.placeholder "Enter your bodyfat %"
                                    prop.ariaLabel "Bodyfat Percentage"
                                    prop.ariaDescribedBy "bodyfat-pct"
                                    prop.onChange this.UpdateBodyfatPercentage
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

            member this.Card : ReactElement =
                let isDisabled = this.ProceedToNextStep.IsNone

                let defaultButtonProperties =
                    [
                        prop.className "btn btn-primary"
                        prop.text "Next"
                        prop.disabled isDisabled
                    ]

                let handlerProperty = 
                    match this.ProceedToNextStep with
                    | Some handler -> [ prop.onClick handler]
                    | None -> []

                Html.div [
                    prop.className "card"

                    prop.children [
                        Html.div [
                            prop.className "card-header"
                            prop.text "Body Composition"
                        ]

                        Html.div [
                            prop.className "card-body"
                            prop.children [
                                this.BodyWeightFields
                                this.BodyfatPercentage
                            ]
                        ]

                        Html.div [
                            prop.className "card-footer"
                            prop.children [
                                Html.button (defaultButtonProperties @ handlerProperty)
                            ]
                        ]
                    ]
                ]
