namespace App

module Library =
    open System

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

    /// This module contains the app's business logic. It is at the core of the onion architecture.
    /// Once data has reached this module, it is in a valid state. This module doesn't contain logic
    /// for retrieving data, parsing data, or validating data.
    module Domain =
        type Mass =
            | Lb of lb : float<lb>
            | Kg of kg : float<kg>

            /// Subtract one mass from another
            static member (-) (mass1: Mass, mass2: Mass) : Mass =
                match mass1, mass2 with
                | Lb lb1, Lb lb2 -> Lb <| lb1 - lb2
                | Lb lb, Kg kg -> Lb <| lb - (kg * lbPerKg)
                | Kg kg1, Kg kg2 -> Kg <| kg1 - kg2
                | Kg kg, Lb lb -> Kg <| kg - (lb * kgPerLb)

            /// Add two masses together
            static member (*)(mass: Mass, num: float) : Mass =
                match mass with
                | Lb lb -> Lb <| lb * num
                | Kg kg -> Kg <| kg * num

            /// Convert the mass to pounds
            member this.ToLb() : Mass =
                match this with
                | Kg kg -> Lb <| kg * lbPerKg
                | _ -> this

            /// Convert the mass to kilograms
            member this.ToKg() : Mass =
                match this with
                | Lb lb -> Kg <| lb * kgPerLb
                | _ -> this

            /// Create a mass whose value is in pounds
            static member CreateLb(amount: float) : Mass =
                Lb <| amount * 1.0<lb>

            /// Create a mass whose value is in kilograms
            static member CreateKg(amount: float) : Mass =
                Kg <| amount * 1.0<kg>

            member this.Text : string =
                match this with
                | Kg kg -> $"{kg} kg"
                | Lb lb -> $"{lb} lb"

            member this.KgMeasure : float<kg> =
                match this.ToKg() with
                | Kg kg -> kg
                | _ -> invalidOp "Domain.Mass.KgMeasure threw an exception"

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
                this.LeanMuscleMass.KgMeasure
                |> basalMetabolicRate

            /// Compute body composition at a certain bodyfat % with the current lean muscle mass
            member this.AtBodyFatPercentage(bodyFatPercentage: uint<pct>) : BodyComposition =
                let bfPct = (float bodyFatPercentage) * 1.0<pct>

                let lmm = this.LeanMuscleMass
                let ratio = 100.0<pct> / (100.0<pct> - bfPct)

                {
                    BodyWeight = lmm * ratio
                    BodyfatPercentage = bodyFatPercentage
                }

            /// Project body composition at current bf % down to 6% bf
            member this.Projections : BodyComposition seq =
                seq { 6u<pct> .. 2u<pct> .. this.BodyfatPercentage}
                |> Seq.sortDescending
                |> Seq.map this.AtBodyFatPercentage
                |> Seq.skipWhile (fun bodyComposition -> bodyComposition = this)


        type DailyActivityLevel =
            | Sedentary
            | ``Mostly Sedentary``
            | ``Lightly Active``
            | ``Highly Active``

            member this.Multiplier : float =
                match this with
                | Sedentary -> 1.15
                | ``Mostly Sedentary`` -> 1.35
                | ``Lightly Active`` -> 1.55
                | ``Highly Active`` -> 1.75

            member this.Text : string =
                match this with
                | Sedentary -> "Sedentary"
                | ``Mostly Sedentary`` -> "Mostly Sedentary"
                | ``Lightly Active`` -> "Lightly Active"
                | ``Highly Active`` -> "Highly Active"

            static member Default =
                Sedentary

        type DailyMacronutrient = {
            Grams: float<g>
            Calories: float<kcal>
            Percentage: float<pct>
        }

        module ProteinGramsPerKgLeanBodyMass =
            let range = seq { 1.6<g/kg> .. 0.1<g/kg> .. 2.2<g/kg> }
            let average = range |> Seq.average
            let min = range |> Seq.min
            let max = range |> Seq.max

            let isIn (proteinGrams: float<g/kg>) : bool =
                min <= proteinGrams && proteinGrams <= max

        type DailyMacros = {
            BodyComposition: BodyComposition
            DailyActivityLevel: DailyActivityLevel
            ProteinGramsPerKgLeanBodyMass: float<g/kg>
        } with
            member this.LeanMuscleMass : Mass =
                this.BodyComposition.LeanMuscleMass

            member this.TotalCalories : float<kcal> =
                this.DailyActivityLevel.Multiplier * this.BodyComposition.BasalMetabolicRate

            member this.Protein : DailyMacronutrient =
                let grams = this.LeanMuscleMass.KgMeasure * this.ProteinGramsPerKgLeanBodyMass
                let cals = grams * Macronutrients.ProteinCaloriesPerGram
                let percentage = (cals / this.TotalCalories) * 100.0<pct>

                {
                    Grams = grams
                    Calories = cals
                    Percentage = percentage
                }

            member this.Fat : DailyMacronutrient =
                let calories = (this.TotalCalories - this.Protein.Calories) / 2.0
                let grams = calories / Macronutrients.FatCaloriesPerGram
                let percentage = (calories / this.TotalCalories) * 100.0<pct>

                {
                    Grams = grams
                    Calories = calories
                    Percentage = percentage
                }

            member this.Carbs : DailyMacronutrient =
                let calories = (this.TotalCalories - this.Protein.Calories) / 2.0
                let grams = calories / Macronutrients.CarbCaloriesPerGram
                let percentage = (calories / this.TotalCalories) * 100.0<pct>

                {
                    Grams = grams
                    Calories = calories
                    Percentage = percentage
                }

    /// Handles user input that is not yet validated
    module Input =
        open FsToolkit.ErrorHandling
        open Domain

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

        /// Body weight input
        type Weight = {
            Amount: float option
            Unit: WeightUnit
        } with
            static member Create(weight: Domain.Mass) : Weight =
                match weight with
                | Mass.Kg kg ->
                    {
                        Amount = Some <| float kg
                        Unit = Kg
                    }
                | Mass.Lb lb ->
                    {
                        Amount = Some <| float lb
                        Unit = Lb
                    }

            static member Default : Weight =
                {
                    Amount = None
                    Unit = Lb
                }

            /// Validate body weight input. Amount must be a parseable float that is >= 0
            member this.Validate() : Result<Domain.Mass, string> =
                match this.Amount, this.Unit with
                | Some amount, Lb when amount >= 0 -> Ok <| Mass.CreateLb amount
                | Some amount, Kg when amount >= 0 -> Ok <| Mass.CreateKg amount
                | Some _, _ -> Error "Weight amount must be >= 0"
                | None, _ -> Error "Weight amount must be present"

            /// Convert body weight to kg
            member this.ToKg() : Weight =
                match this.Validate() with
                | Ok weight -> weight.ToKg() |> Weight.Create
                | Error _ -> { this with Unit = Kg }

            /// Convert bodyweight to lb
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

            static member Create(bodyComposition: Domain.BodyComposition) : BodyComposition =
                {
                    Weight = Weight.Create bodyComposition.BodyWeight
                    BodyfatPercentage = Some <| int bodyComposition.BodyfatPercentage
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

        module ProteinGramsPerKgLeanBodyMass =
            let validate (proteinGrams: float) : Result<float<g/kg>,string> =
                let p = proteinGrams * 1.0<g/kg>

                if Domain.ProteinGramsPerKgLeanBodyMass.isIn p then
                    Ok p
                else
                    Error $"Protein grams must be between {Domain.ProteinGramsPerKgLeanBodyMass.min} && {Domain.ProteinGramsPerKgLeanBodyMass.max}"

        module DailyActivityLevel =
            let tryCreate (input: string) : Domain.DailyActivityLevel option =
                match input with
                | "sedentary" -> Some Sedentary
                | "moderate" -> Some ``Mostly Sedentary``
                | "very" -> Some ``Lightly Active``
                | "extra" -> Some ``Highly Active``
                | _ -> None

            let validate (input: string) : Result<Domain.DailyActivityLevel, string> =
                match input |> tryCreate with
                | Some dal -> Ok dal
                | None -> Error $"Invalid daily activity level: {input}"

        type DailyMacros = {
            BodyComposition: Domain.BodyComposition
            DailyActivityLevel: string
            ProteinGramsPerKgLeanBodyMass: float
        } with
            member this.Validate() : Validation<Domain.DailyMacros, string> =
                validation {
                    let! dal = this.DailyActivityLevel |> DailyActivityLevel.validate
                    and! proteinGrams = this.ProteinGramsPerKgLeanBodyMass |> ProteinGramsPerKgLeanBodyMass.validate

                    return {
                        BodyComposition = this.BodyComposition
                        DailyActivityLevel = dal
                        ProteinGramsPerKgLeanBodyMass = proteinGrams
                    }
                }

            static member Create(bodyComposition: Domain.BodyComposition, ?dailyActivityLevel: string, ?proteinGramsPerKgLeanBodyMass: float) : DailyMacros =
                let dailyActivityLevel = defaultArg dailyActivityLevel ""
                let averageProteinGrams = Domain.ProteinGramsPerKgLeanBodyMass.average |> float
                let proteinGrams = defaultArg proteinGramsPerKgLeanBodyMass averageProteinGrams

                {
                    BodyComposition = bodyComposition
                    DailyActivityLevel = dailyActivityLevel
                    ProteinGramsPerKgLeanBodyMass = proteinGrams
                }

    module Html =
        open FsToolkit.ErrorHandling
        open Feliz

        module BodyComposition =
            type EventHandlers = {
                UpdateWeightAmount: float -> unit
                UpdateBodyfatPercentage: int -> unit
                SelectKgUnit: Browser.Types.MouseEvent -> unit
                SelectLbUnit: Browser.Types.MouseEvent -> unit
                ProceedToNextStep: (Browser.Types.MouseEvent -> unit) option
            }

            type Status =
                | Enabled of EventHandlers
                | Disabled

            type Fields = {
                BodyComposition: Input.BodyComposition
                Status: Status
            } with
                static member CreateEnabled(bodyComposition: Input.BodyComposition,
                                            updateWeightAmount: float -> unit,
                                            updateBodyfatPercentage: int -> unit,
                                            selectKgUnit: Browser.Types.MouseEvent -> unit,
                                            selectLbUnit: Browser.Types.MouseEvent -> unit,
                                            ?proceedToNextStep: Browser.Types.MouseEvent -> unit
                                            ): Fields =

                    let enabledStatus =
                        Enabled {
                            UpdateWeightAmount = updateWeightAmount
                            UpdateBodyfatPercentage = updateBodyfatPercentage
                            SelectKgUnit = selectKgUnit
                            SelectLbUnit = selectLbUnit
                            ProceedToNextStep = proceedToNextStep
                        }

                    {
                        BodyComposition = bodyComposition
                        Status = enabledStatus
                    }

                static member CreateDisabled(bodyComposition: Input.BodyComposition) : Fields =
                    {
                        BodyComposition = bodyComposition
                        Status = Disabled
                    }

                member private _.InputHtmlId : string = "body-weight-amount-input"

                member this.IsEnabled : bool =
                    match this.Status with
                    | Enabled _ -> true
                    | _ -> false

                member this.IsDisabled : bool =
                    not this.IsEnabled

                member private this.TryEventHandlers : EventHandlers option =
                    match this.Status with
                    | Enabled eventHandlers -> Some eventHandlers
                    | _ -> None

                /// Display the html input for body weight amount
                member this.WeightAmountInput : ReactElement =
                    let changeProperty =
                        match this.TryEventHandlers with
                        | Some eventHandlers -> [ prop.onChange eventHandlers.UpdateWeightAmount ]
                        | _ -> []

                    let baseProperties = [
                        prop.id this.InputHtmlId
                        prop.type' "number"
                        prop.min 0
                        prop.className "form-control"
                        prop.placeholder "How much do you weigh?"
                        prop.disabled (not this.IsEnabled)
                    ]

                    let valueProperty =
                        match this.BodyComposition.Weight.Amount with
                        | Some amount -> [ prop.value amount ]
                        | _ -> []

                    Html.input (baseProperties @ valueProperty @ changeProperty)

                member private _.WeightUnitOptions = "weightUnitOptions"

                /// Display form field for body weight kg unit
                member this.WeightKgOption : ReactElement list =
                    let optionName = "kgUnitOption"

                    let changeProperty =
                        match this.TryEventHandlers with
                        | Some eventHandlers -> [ prop.onClick eventHandlers.SelectKgUnit ]
                        | _ -> []

                    let inputProperties =
                        [
                            prop.type' "radio"
                            prop.className "btn-check"
                            prop.name this.WeightUnitOptions
                            prop.id optionName
                            prop.isChecked this.BodyComposition.Weight.Unit.IsKilogram
                            prop.disabled (not this.IsEnabled)
                        ]

                    [
                        Html.input (inputProperties @ changeProperty)
                        Html.label [
                            prop.classes ["btn"; "btn-secondary"]
                            prop.for' optionName
                            prop.text "Kg"
                        ]
                    ]

                /// Display form field for body weight lb unit
                member this.WeightLbOption : ReactElement list =
                    let optionName = "lbUnitOption"

                    let changeProperty =
                        match this.TryEventHandlers with
                        | Some eventHandlers -> [ prop.onClick eventHandlers.SelectLbUnit ]
                        | None -> []

                    let inputProperties =
                        [
                            prop.type' "radio"
                            prop.className "btn-check"
                            prop.name this.WeightUnitOptions
                            prop.id optionName
                            prop.isChecked this.BodyComposition.Weight.Unit.IsPound
                            prop.disabled (not this.IsEnabled)
                        ]

                    [
                        Html.input (inputProperties @ changeProperty)
                        Html.label [
                            prop.classes ["btn"; "btn-secondary"]
                            prop.for' optionName
                            prop.text "Lb"
                        ]
                    ]

                /// Display form fields for body weight
                member this.BodyWeightFields : ReactElement =
                    let bodyWeightLabel =
                        Html.label [
                            prop.for' this.InputHtmlId
                            prop.className "form-label"
                            prop.text "Body Weight"
                        ]

                    let inputHtmls =
                        [ this.WeightAmountInput ]
                        @ this.WeightLbOption
                        @ this.WeightKgOption

                    Html.div [
                        prop.className "mb-3"
                        prop.children [
                            bodyWeightLabel

                            Html.div [
                                prop.className "input-group"
                                prop.children inputHtmls
                            ]
                        ]
                    ]

                /// Display form field for bodyfat %
                member this.BodyfatPercentage : ReactElement =
                    let changeProperty =
                        match this.TryEventHandlers with
                        | Some eventHandlers ->
                            [ prop.onChange eventHandlers.UpdateBodyfatPercentage ]
                        | None -> []

                    let inputProperties =
                        [
                            prop.type' "number"
                            prop.min 0
                            prop.max 100
                            prop.className "form-control"
                            prop.placeholder "Enter your bodyfat %"
                            prop.ariaLabel "Bodyfat Percentage"
                            prop.ariaDescribedBy "bodyfat-pct"
                            prop.disabled (not this.IsEnabled)
                        ]

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
                                    Html.input (inputProperties @ changeProperty)
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
                    let isDisabled =
                        match this.TryEventHandlers with
                        | Some eventHandlers -> eventHandlers.ProceedToNextStep.IsNone
                        | None -> true

                    let defaultButtonProperties =
                        [
                            prop.className "btn btn-primary"
                            prop.text "Next"
                            prop.disabled isDisabled
                        ]

                    let handlerProperty =
                        option {
                            let! eventHandlers = this.TryEventHandlers
                            let! handler = eventHandlers.ProceedToNextStep

                            return [ prop.onClick handler ]
                        } |> Option.defaultValue []


                    let headerCss =
                        if this.IsEnabled then
                            prop.classes ["card-header"; "text-bg-primary"]
                        else
                            prop.className "card-header"

                    Html.div [
                        prop.className "card mt-3"

                        prop.children [
                            Html.div [
                                headerCss
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

        module DailyMacros =
            type EventHandlers = {
                SelectActivityLevel: (Browser.Types.Event -> unit)
                ChangeProteinGrams: (float -> unit)
            }

            type Fields =
                | EnabledMacrosFields of input: Input.DailyMacros * eventHandlers: EventHandlers
                | DisabledMacrosFields of input: Input.DailyMacros option

                member this.IsEnabled : bool =
                    match this with
                    | EnabledMacrosFields _ -> true
                    | _ -> false

                member this.IsDisabled : bool =
                    not this.IsEnabled

                /// Retrieve the inputted daily activity level, if it exists
                member this.DailyActivityLevel : string option =
                    match this with
                    | EnabledMacrosFields (input, _) -> Some input.DailyActivityLevel
                    | DisabledMacrosFields input ->
                        option {
                            let! dailyMacros = input
                            return! dailyMacros.DailyActivityLevel
                        }

                member this.ProteinGramsPerKgLeanBodyMass : float =
                    match this with
                    | EnabledMacrosFields (input, _) ->
                        Decimal.Round(d = (input.ProteinGramsPerKgLeanBodyMass |> decimal), decimals = 2)
                        |> float
                    | DisabledMacrosFields input ->
                        let defaultValue =
                            Decimal.Round(d = (Domain.ProteinGramsPerKgLeanBodyMass.average |> decimal), decimals = 2)
                            |> float

                        option {
                            let! f = input

                            let rounded = Decimal.Round(d = (f.ProteinGramsPerKgLeanBodyMass |> decimal), decimals = 2) |> float

                            return rounded
                        } |> Option.defaultValue defaultValue

                member this.TryEventHandlers : EventHandlers option =
                    match this with
                    | EnabledMacrosFields (_, eventHandlers) -> Some eventHandlers
                    | _ -> None

                /// Display daily activility level dropdown
                member this.DailyActivityLevelDropdown : ReactElement =
                    let eventHandlerProperties =
                        match this.TryEventHandlers with
                        | Some eventHandlers -> [ prop.onSelect eventHandlers.SelectActivityLevel ]
                        | None -> []

                    let dropdownProperties = [
                        prop.disabled this.IsDisabled
                        prop.className "form-select"
                        prop.id "daily-activity-select"
                        prop.children [
                            // blank option
                            Html.option [
                                prop.text ""
                                prop.selected this.DailyActivityLevel.IsNone
                                prop.value ""
                            ]
                            // sedentary option
                            Html.option [
                                prop.text (Domain.DailyActivityLevel.Sedentary.ToString())
                                prop.selected (Some (Domain.DailyActivityLevel.Sedentary.ToString()) = this.DailyActivityLevel)
                                prop.value (Domain.DailyActivityLevel.Sedentary.ToString())
                            ]
                            // 'mostly sedentary' option
                            Html.option [
                                prop.text (Domain.DailyActivityLevel.``Mostly Sedentary``.ToString())
                                prop.selected (Some (Domain.DailyActivityLevel.``Mostly Sedentary``.ToString()) = this.DailyActivityLevel)
                                prop.value (Domain.DailyActivityLevel.``Mostly Sedentary``.ToString())
                            ]
                            // 'lightly active' option
                            Html.option [
                                prop.text (Domain.DailyActivityLevel.``Lightly Active``.ToString())
                                prop.selected (Some (Domain.DailyActivityLevel.``Lightly Active``.ToString()) = this.DailyActivityLevel)
                                prop.value (Domain.DailyActivityLevel.``Lightly Active``.ToString())
                            ]
                            // 'highly active' option
                            Html.option [
                                prop.text (Domain.DailyActivityLevel.``Highly Active``.ToString())
                                prop.selected (Some (Domain.DailyActivityLevel.``Highly Active``.ToString()) = this.DailyActivityLevel)
                                prop.value (Domain.DailyActivityLevel.``Highly Active``.ToString())
                            ]
                        ]
                    ]

                    Html.div [
                        prop.className "mb-3"
                        prop.children [
                            Html.label [
                                prop.for' "daily-activity-level-dropdown"
                                prop.className "form-label"
                                prop.text "Daily Activity Level"
                            ]
                            Html.select (eventHandlerProperties @ dropdownProperties)
                        ]
                    ]

                member this.ProteinGramsInput : ReactElement =
                    let eventHandlerProperties =
                        match this.TryEventHandlers with
                        | Some eventHandlers -> [ prop.onChange eventHandlers.ChangeProteinGrams ]
                        | None -> []

                    Html.div [
                        prop.className "mb-3"
                        prop.children [
                            Html.label [
                                prop.for' "protein-grams-input"
                                prop.className "form-label"
                                prop.text "Protein Grams Per Kg Lean Body Mass"
                            ]

                            Html.div [
                                prop.className "input-group"
                                prop.children [
                                    (Html.input ([
                                        prop.id "protein-grams-input"
                                        prop.className "form-control"
                                        prop.type' "range"
                                        prop.min (Domain.ProteinGramsPerKgLeanBodyMass.min |> float)
                                        prop.max (Domain.ProteinGramsPerKgLeanBodyMass.max |> float)
                                        prop.step 0.05
                                        prop.value this.ProteinGramsPerKgLeanBodyMass
                                        prop.disabled this.IsDisabled
                                    ] @ eventHandlerProperties))

                                    Html.span [
                                        prop.text $"{this.ProteinGramsPerKgLeanBodyMass} g / kg"
                                        prop.className "input-group-text"
                                    ]
                                ]
                            ]
                        ]
                    ]

                member this.Card : ReactElement =
                    let cardCss =
                        if this.IsEnabled then
                            prop.classes ["card-header"; "text-bg-primary"]
                        else
                            prop.className "card-header"

                    Html.div [
                        prop.className "card mt-3"

                        prop.children [
                            Html.div [
                                cardCss
                                prop.text "Daily Macros"
                            ]

                            Html.div [
                                prop.className "card-body"

                                prop.children [
                                    this.DailyActivityLevelDropdown
                                    this.ProteinGramsInput
                                ]
                            ]
                        ]
                    ]

                static member CreateEnabled(form: Input.DailyMacros, eventHandlers: EventHandlers) : Fields =
                    EnabledMacrosFields (form, eventHandlers)

                static member CreateDisabled(?form: Input.DailyMacros) : Fields =
                    DisabledMacrosFields form