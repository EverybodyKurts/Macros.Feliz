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

        module Float =
            let isBetween (min: float<'u>) (max: float<'u>) (value: float<'u>) : bool =
                min <= value && value <= max

            /// Clamp a float with a generic unit of measure between a min and max value
            let clamp (min: float<'u>) (max: float<'u>) (value: float<'u>) : float<'u> =
                // value is between min & max
                if isBetween min max value then value
                // min > value, choose min
                else if min > value then min
                else max

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
    [<AutoOpen>]
    module Domain =
        type Mass =
            | Lb of lb : float<lb>
            | Kg of kg : float<kg>

            /// Subtract one mass from another
            ///
            ///     (Lb 5.0<lb>) - (Lb 3.0<lb>) = (Lb 2.0<lb>)
            ///     (Kg 5.0<kg>) - (Kg 3.0<kg>) = (Kg 2.0<kg>)
            static member (-) (mass1: Mass, mass2: Mass) : Mass =
                match mass1, mass2 with
                | Lb lb1, Lb lb2 -> Lb <| lb1 - lb2
                | Lb lb, Kg kg -> Lb <| lb - (kg * lbPerKg)
                | Kg kg1, Kg kg2 -> Kg <| kg1 - kg2
                | Kg kg, Lb lb -> Kg <| kg - (lb * kgPerLb)

            /// Multiply a mass by a number
            static member (*) (mass: Mass, num: float) : Mass =
                match mass with
                | Lb lb -> Lb <| lb * num
                | Kg kg -> Kg <| kg * num

            /// Convert the mass to pounds
            member this.ToLb () : Mass =
                match this with
                | Kg kg -> Lb <| kg * lbPerKg
                | _ -> this

            /// Convert the mass to kilograms
            member this.ToKg () : Mass =
                match this with
                | Lb lb -> Kg <| lb * kgPerLb
                | _ -> this

            /// Create a mass whose unit of measure is lbs
            static member CreateLb (amount: float) : Mass =
                Lb <| amount * 1.0<lb>

            /// Create a mass whose unit of measure is kgs
            static member CreateKg (amount: float) : Mass =
                Kg <| amount * 1.0<kg>

            /// Represent the mass as a string
            ///
            ///     (Lb 5.0<lb>).Text = "5.0 lb"
            ///     (Kg 5.0<kg>).Text = "5.0 kg"
            member this.Text : string =
                match this with
                | Kg kg -> $"{Math.Round(float kg, 2)} kg"
                | Lb lb -> $"{Math.Round(float lb, 2)} lb"

            /// Convert the mass to the kg unit of measure
            member this.KgMeasure : float<kg> =
                match this.ToKg() with
                | Kg kg -> kg
                | _ -> invalidOp "Domain.Mass.KgMeasure threw an exception"

        /// <summary>
        /// Calculate basal metabolic rate with the Katch-McArdle formula
        /// </summary>
        ///
        /// <param name="leanBodyMass">Lean body mass in kg</param>
        /// <returns>Basal metabolic rate in kcal</returns>
        let basalMetabolicRate (leanBodyMass: float<kg>) : float<kcal> =
            370.0<kcal> + (21.6<kcal/kg> * leanBodyMass)

        type BodyComposition = {
            BodyWeight: Mass
            BodyfatPercentage: uint<pct>
        } with
            /// The amount of fat mass on the body
            member this.FatMass : Mass =
                let bf = float this.BodyfatPercentage / 100.0

                match this.BodyWeight with
                | Kg kg -> Kg <| kg * bf
                | Lb lb -> Lb <| lb * bf

            /// The amount of lean muscle mass on the body
            member this.LeanMuscleMass : Mass =
                this.BodyWeight - this.FatMass

            /// The number of calories burnt daily as the body performs basic (basal) life-sustaining functions
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

            /// Project body composition at current bodyfat % down to 6% bodyfat
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

        /// Holds the daily macronutrient breakdown for protein, carb, and fat.
        type DailyMacronutrient = internal {
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

        module DailyMacros =
            type MacroPercentages = {
                Protein: float<pct>
                Carbs: float<pct>
                Fat: float<pct>
            } with
                static member internal Default : MacroPercentages =
                    {
                        Protein = 34.0<pct>
                        Carbs = 33.0<pct>
                        Fat = 33.0<pct>
                    }

                static member internal Create (protein: float<pct>, carbs: float<pct>, fat: float<pct>) : MacroPercentages =
                    {
                        Protein = protein
                        Carbs = carbs
                        Fat = fat
                    }

                member this.UpdateProtein (protein: float<pct>) : MacroPercentages=
                    let clampPct = clampFloat (0.0<pct>) (100.0<pct>)

                    let protein = clampPct protein
                    let carb = protein - this.Protein

                    let fat =
                        if carb < 0.0<pct> then this.Fat + carb
                        else this.Fat

                    { this with
                        Protein = protein
                        Carbs = clampPct carb
                        Fat = clampPct fat
                    }

                member this.UpdateCarbs (carbs: float<pct>) : MacroPercentages =
                    let clampPct = clampFloat (0.0<pct>) (100.0<pct>)
                    let fat = carbs - this.Carbs


                    { this with
                        Carbs = clampPct carbs
                        Fat = clampPct fat
                    }

        type DailyMacros = {
            BodyComposition: BodyComposition
            DailyActivityLevel: DailyActivityLevel
            Percentages: DailyMacros.MacroPercentages
        }  with
            member this.LeanMuscleMass : Mass =
                this.BodyComposition.LeanMuscleMass

            member this.BodyWeight : Mass =
                this.BodyComposition.BodyWeight

            member this.TotalCalories : float<kcal> =
                this.DailyActivityLevel.Multiplier * this.BodyComposition.BasalMetabolicRate

            member this.Protein : DailyMacronutrient =
                let decimal = (this.Percentages.Protein |> float) / 100.0
                let calories = this.TotalCalories * decimal
                let grams = calories / Macronutrients.ProteinCaloriesPerGram

                {
                    Grams = grams
                    Calories = calories
                    Percentage = this.Percentages.Protein
                }

            member this.ProteinGramsPerKgLeanBodyMass : float<g/kg> =
                this.Protein.Grams / this.LeanMuscleMass.KgMeasure

            member this.ProteinGramsPerKgBodyWeight : float<g/kg> =
                this.Protein.Grams / this.BodyWeight.KgMeasure

            member this.CarbGramsPerKgLeanBodyMass : float<g/kg> =
                this.Carbs.Grams / this.LeanMuscleMass.KgMeasure

            member this.CarbGramsPerKgBodyWeight : float<g/kg> =
                this.Carbs.Grams / this.BodyWeight.KgMeasure

            member this.Carbs: DailyMacronutrient =
                let decimal = (this.Percentages.Carbs |> float) / 100.0
                let calories = this.TotalCalories * decimal
                let grams = calories / Macronutrients.CarbCaloriesPerGram

                {
                    Grams = grams
                    Calories = calories
                    Percentage = this.Percentages.Carbs
                }

            member this.Fat: DailyMacronutrient =
                let decimal = (this.Percentages.Fat |> float) / 100.0
                let calories = this.TotalCalories * decimal
                let grams = calories / Macronutrients.FatCaloriesPerGram

                {
                    Grams = grams
                    Calories = calories
                    Percentage = this.Percentages.Fat
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
            member this.Validate () : Result<Domain.Mass, string> =
                match this.Amount, this.Unit with
                | Some amount, Lb when amount >= 0 -> Ok <| Mass.CreateLb amount
                | Some amount, Kg when amount >= 0 -> Ok <| Mass.CreateKg amount
                | Some _, _ -> Error "Weight amount must be >= 0"
                | None, _ -> Error "Weight amount must be present"

            /// Convert body weight to kg
            member this.ToKg () : Weight =
                match this.Validate () with
                | Ok weight -> weight.ToKg () |> Weight.Create
                | Error _ -> { this with Unit = Kg }

            /// Convert bodyweight to lb
            member this.ToLb () : Weight =
                match this.Validate () with
                | Ok weight -> weight.ToLb () |> Weight.Create
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
                match input.ToLower() with
                | "sedentary" -> Some Sedentary
                | "mostly sedentary" -> Some ``Mostly Sedentary``
                | "lightly active" -> Some ``Lightly Active``
                | "highly active" -> Some ``Highly Active``
                | _ -> None

            let validate (input: string) : Result<Domain.DailyActivityLevel, string> =
                match input |> tryCreate with
                | Some dal -> Ok dal
                | None -> Error $"Invalid daily activity level: {input}"

        module DailyMacros =
            module Percentages =
                let (|ValidPercentage|_|) (pct: float) =
                    if 0.0 <= pct && pct <= 100.0 then Some <| pct * 1.0<pct>
                    else None

            open Percentages

            type Percentages = {
                Protein: float
                Carbs: float
                Fat: float
            } with
                static member Default : Percentages =
                    {
                        Protein = 34.0
                        Carbs = 33.0
                        Fat = 33.0
                    }

                member this.Validate () : Result<DailyMacros.MacroPercentages, string> =
                    match this.Protein, this.Carbs, this.Fat with
                    // all macronutrients provided, are valid, and sum to 100%
                    | ValidPercentage p, ValidPercentage c, ValidPercentage f when p + c + f = 100.0<pct> ->
                        Ok <| DailyMacros.MacroPercentages.Create(protein = p, carbs = c, fat = f)

                    // all macronutrients are provided and valid but do not sum to 100%
                    | ValidPercentage p, ValidPercentage c, ValidPercentage f ->
                        Error $"Macro percentages need to sum to 100 percent. Protein = {p}, Carbs = {c}, Fat = {f}"

                    // any other case, error out
                    | _ ->
                        Error $"Invalid percentages. Protein = {this.Protein}, Carbs = {this.Carbs}, Fat = {this.Fat} "

                member this.UpdateProtein (protein: float) : Percentages=
                    let clampPct value = Math.Clamp(value, 0.0, 100.0)

                    let protein = clampPct protein
                    let carbs = this.Carbs - (protein - this.Protein)

                    let fat =
                        if carbs < 0.0 then this.Fat + carbs
                        else this.Fat

                    { this with
                        Protein = protein
                        Carbs = clampPct carbs
                        Fat = clampPct fat
                    }

                member this.UpdateCarbs (carbs: float) : Percentages =
                    let clampPct value = Math.Clamp(value, 0.0, 100.0)
                    let fat = this.Fat - (carbs - this.Carbs)

                    { this with
                        Carbs = clampPct carbs
                        Fat = clampPct fat
                    }

        type DailyMacros = {
            BodyComposition: Domain.BodyComposition
            DailyActivityLevel: string
            Percentages: DailyMacros.Percentages
        } with
            member this.Validate () : Validation<Domain.DailyMacros, string> =
                validation {
                    let! dal = this.DailyActivityLevel |> DailyActivityLevel.validate
                    and! percentages = this.Percentages.Validate()

                    return {
                        BodyComposition = this.BodyComposition
                        DailyActivityLevel = dal
                        Percentages = percentages
                    }
                }

            static member Create (bodyComposition: Domain.BodyComposition, ?dailyActivityLevel: string, ?percentages: DailyMacros.Percentages) : DailyMacros =
                let dailyActivityLevel = defaultArg dailyActivityLevel ""
                let percentages = defaultArg percentages DailyMacros.Percentages.Default

                {
                    BodyComposition = bodyComposition
                    DailyActivityLevel = dailyActivityLevel
                    Percentages = percentages
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
                static member CreateEnabled (bodyComposition: Input.BodyComposition,
                                             updateWeightAmount: float -> unit,
                                             updateBodyfatPercentage: int -> unit,
                                             selectKgUnit: Browser.Types.MouseEvent -> unit,
                                             selectLbUnit: Browser.Types.MouseEvent -> unit,
                                             ?proceedToNextStep: Browser.Types.MouseEvent -> unit
                                             ) : Fields =

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

                static member CreateDisabled (bodyComposition: Input.BodyComposition) : Fields =
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
                SelectActivityLevel: Browser.Types.Event -> unit
                ChangeProteinPercentage: float -> unit
                ChangeCarbPercentage: float -> unit
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

                member this.ProteinPercentage : float =
                    match this with
                    | EnabledMacrosFields (input, _) ->
                        input.Percentages.Protein
                    | DisabledMacrosFields input ->
                        let defaultValue =
                            DailyMacros.MacroPercentages.Default.Protein |> float

                        option {
                            let! i = input

                            return i.Percentages.Protein
                        } |> Option.defaultValue defaultValue

                member this.CarbPercentage : float =
                    match this with
                    | EnabledMacrosFields (input, _) ->
                        input.Percentages.Carbs
                    | DisabledMacrosFields input ->
                        let defaultValue =
                            DailyMacros.MacroPercentages.Default.Carbs |> float

                        option {
                            let! i = input

                            return i.Percentages.Carbs
                        } |> Option.defaultValue defaultValue

                member this.FatPercentage : float =
                    match this with
                    | EnabledMacrosFields (input, _) ->
                        input.Percentages.Fat
                    | DisabledMacrosFields input ->
                        let defaultValue =
                            DailyMacros.MacroPercentages.Default.Fat |> float

                        option {
                            let! i = input

                            return i.Percentages.Fat
                        } |> Option.defaultValue defaultValue


                member this.TryEventHandlers : EventHandlers option =
                    match this with
                    | EnabledMacrosFields (_, eventHandlers) -> Some eventHandlers
                    | _ -> None

                /// Display daily activility level dropdown
                member this.DailyActivityLevelDropdown : ReactElement =
                    let eventHandlerProperties =
                        match this.TryEventHandlers with
                        | Some eventHandlers -> [ prop.onChange eventHandlers.SelectActivityLevel ]
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

                member this.ProteinPercentageInput : ReactElement =
                    let eventHandlerProperties =
                        match this.TryEventHandlers with
                        | Some eventHandlers -> [ prop.onChange eventHandlers.ChangeProteinPercentage ]
                        | None -> []

                    Html.div [
                        prop.className "mb-3"
                        prop.children [
                            Html.label [
                                prop.for' "protein-percentage-input-group"
                                prop.className "form-label"
                                prop.text "Protein %"
                            ]

                            Html.div [
                                prop.className "input-group"
                                prop.id "protein-percentage-input-group"
                                prop.children [
                                    (Html.input ([
                                        prop.id "protein-percentage-range-input"
                                        prop.className "form-control"
                                        prop.type' "range"
                                        prop.min 0.0
                                        prop.max 100.0
                                        prop.step 1.0
                                        prop.value this.ProteinPercentage
                                        prop.disabled this.IsDisabled
                                    ] @ eventHandlerProperties))

                                    Html.span [
                                        prop.text $"{this.ProteinPercentage} pct"
                                        prop.className "input-group-text bg-success text-white"
                                    ]
                                ]
                            ]
                        ]
                    ]

                member this.CarbPercentageInput : ReactElement =
                    let eventHandlerProperties =
                        match this.TryEventHandlers with
                        | Some eventHandlers -> [ prop.onChange eventHandlers.ChangeCarbPercentage ]
                        | None -> []

                    Html.div [
                        prop.className "mb-3"
                        prop.children [
                            Html.label [
                                prop.for' "carb-percentage-input-group"
                                prop.className "form-label"
                                prop.text "Carb %"
                            ]

                            Html.div [
                                prop.className "input-group"
                                prop.id "carb-percentage-input-group"
                                prop.children [
                                    (Html.input ([
                                        prop.id "carb-percentage-range-input"
                                        prop.className "form-control"
                                        prop.type' "range"
                                        prop.min 0.0
                                        prop.max 100.0
                                        prop.step 1.0
                                        prop.value this.CarbPercentage
                                        prop.disabled this.IsDisabled
                                    ] @ eventHandlerProperties))

                                    Html.span [
                                        prop.text $"{this.CarbPercentage} pct"
                                        prop.className "input-group-text bg-info text-white"
                                    ]
                                ]
                            ]
                        ]
                    ]

                member this.MacrosPercentageStackedProgressBar : ReactElement =
                    let progress (label: string) (bgColor: string) (percentage: float) =
                        Html.div [
                            prop.className "progress"
                            prop.role "progressbar"
                            prop.ariaLabel label
                            prop.ariaValueMin 0
                            prop.ariaValueMax 100
                            prop.ariaValueNow percentage
                            prop.width percentage
                            prop.style [ style.width (length.percent percentage) ]

                            prop.children [
                                Html.div [
                                    prop.classes ["progress-bar"; "progress-bar-striped"; $"bg-{bgColor}" ]
                                    prop.text $"{percentage} pct"
                                ]
                            ]
                        ]

                    Html.div [
                        prop.className "progress-stacked"

                        prop.children [
                            // protein progress bar
                            progress "Protein %" "success" this.ProteinPercentage
                            progress "Carb %" "info" this.CarbPercentage
                            progress "Fat %" "warning" this.FatPercentage
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
                                    this.ProteinPercentageInput
                                    this.CarbPercentageInput
                                    this.MacrosPercentageStackedProgressBar
                                ]
                            ]
                        ]
                    ]

                static member CreateEnabled (input: Input.DailyMacros, eventHandlers: EventHandlers) : Fields =
                    EnabledMacrosFields (input, eventHandlers)

                static member CreateDisabled (?input: Input.DailyMacros) : Fields =
                    DisabledMacrosFields input

        type CalculatorResults = {
            BodyComposition: Domain.BodyComposition option
            DailyMacros: Domain.DailyMacros option
        } with
            static member Create (?bodyComposition: Domain.BodyComposition, ?dailyMacros: Domain.DailyMacros) : CalculatorResults =
                let dmBodyComposition = dailyMacros |> Option.map(fun dm -> dm.BodyComposition)
                let bodyComposition =
                    match dmBodyComposition with
                    | Some _ -> dmBodyComposition
                    | None -> bodyComposition

                {
                    BodyComposition = bodyComposition
                    DailyMacros = dailyMacros
                }

            member this.Enabled : bool =
                this.BodyComposition.IsSome

            member private this.LmmText : string =
                option {
                    let! bc = this.BodyComposition

                    return bc.LeanMuscleMass.Text
                } |> Option.defaultValue ""

            member private this.FatMassText : string =
                option {
                    let! bc = this.BodyComposition

                    return bc.FatMass.Text
                } |> Option.defaultValue ""

            member private this.BasalMetabolicRateText : string =
                option {
                    let! bc = this.BodyComposition
                    let bmr = Math.Round(float bc.BasalMetabolicRate, 2)

                    return $"{bmr} kcal"
                } |> Option.defaultValue ""

            member private this.TotalCaloriesText : string =
                option {
                    let! dm = this.DailyMacros
                    let totCals = Math.Round(float dm.TotalCalories, 2)

                    return $"{totCals} kcal"
                } |> Option.defaultValue ""

            member this.BodyCompositionCard : ReactElement =
                let resultRow (title: string) (value: string) =
                    Html.tr [
                        prop.children [
                            Html.th [
                                prop.scope "row"
                                prop.text title
                                prop.className "col-4 ps-3"
                            ]

                            Html.td [
                                prop.text value
                            ]
                        ]
                    ]

                Html.div [
                    prop.className "card mt-3"
                    prop.children [
                        Html.div [
                            prop.className "card-header"
                            prop.text "Body Composition Result"
                        ]

                        Html.table [
                            prop.className "table mb-0"

                            prop.children [
                                Html.tbody [
                                    resultRow "Lean Muscle Mass" this.LmmText
                                    resultRow "Fat Mass" this.FatMassText
                                    resultRow "Resting Metabolic Rate" this.BasalMetabolicRateText
                                ]
                            ]
                        ]
                    ]
                ]

            member private this.TryProteinMacros : DailyMacronutrient option =
                option {
                    let! dm = this.DailyMacros

                    return dm.Protein
                }

            member private this.TryCarbMacros : DailyMacronutrient option =
                option {
                    let! dm = this.DailyMacros

                    return dm.Carbs
                }

            member private this.TryFatMacros : DailyMacronutrient option =
                option {
                    let! dm = this.DailyMacros

                    return dm.Fat
                }

            member private this.ProteinGramsPerKgBodyweightText : string =
                option {
                    let! dm = this.DailyMacros
                    let pgpkbw = dm.ProteinGramsPerKgBodyWeight

                    return $"{Math.Round(float pgpkbw, 2)} g / kg bodyweight"
                } |> Option.defaultValue ""

            member private this.ProteinGramsPerKgLeanMuscleMassText : string =
                option {
                    let! dm = this.DailyMacros
                    let lmm = dm.ProteinGramsPerKgLeanBodyMass

                    return $"{Math.Round(float lmm, 2)} g / kg lean muscle mass"
                } |> Option.defaultValue ""

            member private this.CarbGramsPerKgBodyweightText : string =
                option {
                    let! dm = this.DailyMacros
                    let bw = dm.CarbGramsPerKgBodyWeight

                    return $"{Math.Round(float bw, 2)} g / kg bodyweight"
                } |> Option.defaultValue ""

            member private this.CarbGramsPerKgLeanMuscleMassText: string =
                option {
                    let! dm = this.DailyMacros
                    let lmm = dm.CarbGramsPerKgLeanBodyMass

                    return $"{Math.Round(float lmm, 2)} g / kg lean muscle mass"
                } |> Option.defaultValue ""

            member private this.ProteinGramsText : string=
                option {
                    let! pm = this.TryProteinMacros

                    return $"{Math.Round(float pm.Grams, 2)}"
                } |> Option.defaultValue ""

            member private this.ProteinCalsText : string=
                option {
                    let! pm = this.TryProteinMacros

                    return $"{Math.Round(float pm.Calories, 2)}"
                } |> Option.defaultValue ""

            member private this.CarbGramsText : string=
                option {
                    let! pm = this.TryCarbMacros

                    return $"{Math.Round(float pm.Grams, 2)}"
                } |> Option.defaultValue ""

            member private this.CarbCalsText : string=
                option {
                    let! cm = this.TryCarbMacros

                    return $"{Math.Round(float cm.Calories, 2)}"
                } |> Option.defaultValue ""

            member private this.FatGramsText : string=
                option {
                    let! fm = this.TryFatMacros

                    return $"{Math.Round(float fm.Grams, 2)}"
                } |> Option.defaultValue ""

            member private this.FatCalsText : string=
                option {
                    let! fm = this.TryFatMacros

                    return $"{Math.Round(float fm.Calories, 2)}"
                } |> Option.defaultValue ""

            member _.MacronutrientTable (calsText: string, gramsText: string) : ReactElement =
                Html.table [
                    Html.tr [
                        Html.th [
                            prop.scope "row"
                            prop.text "kCal"
                        ]

                        Html.td [
                            prop.text calsText
                        ]
                    ]

                    Html.tr [
                        Html.th [
                            prop.scope "row"
                            prop.text "grams"
                        ]

                        Html.td [
                            prop.text gramsText
                        ]
                    ]
                ]

            member this.DailyMacrosCard : ReactElement =
                let resultRow (title: string) (value: string) =
                    Html.tr [
                        prop.children [
                            Html.th [
                                prop.scope "row"
                                prop.text title
                                prop.className "col-4 ps-3"
                            ]

                            Html.td [
                                prop.text value
                            ]
                        ]
                    ]

                Html.div [
                    prop.className "card mt-3"
                    prop.children [
                        Html.div [
                            prop.className "card-header"
                            prop.text "Daily Macros Result"
                        ]

                        Html.table [
                            prop.className "table mb-0"

                            prop.children [
                                Html.tbody [
                                    prop.children [
                                        resultRow "Tot Cals / Day" this.TotalCaloriesText
                                        resultRow "Protein Grams / Kg Bodyweight" this.ProteinGramsPerKgBodyweightText
                                        resultRow "Protein Grams / Kg Lean Muscle Mass" this.ProteinGramsPerKgLeanMuscleMassText
                                        resultRow "Carb Grams / Kg Bodyweight" this.CarbGramsPerKgBodyweightText
                                        resultRow "Carb Grams / Kg Lean Muscle Mass" this.CarbGramsPerKgLeanMuscleMassText

                                        // protein macronutrients
                                        Html.tr [
                                            prop.children [
                                                Html.th [
                                                    prop.scope "row"
                                                    prop.text "Protein"
                                                    prop.className "col-4 ps-3"
                                                ]

                                                Html.td [
                                                    this.MacronutrientTable(calsText = this.ProteinCalsText, gramsText = this.ProteinGramsText)
                                                ]
                                            ]
                                        ]

                                        // carb macronutrients
                                        Html.tr [
                                            prop.children [
                                                Html.th [
                                                    prop.scope "row"
                                                    prop.text "Carbs"
                                                    prop.className "col-4 ps-3"
                                                ]

                                                Html.td [
                                                    this.MacronutrientTable(calsText = this.CarbCalsText, gramsText = this.CarbGramsText)
                                                ]
                                            ]
                                        ]

                                        // fat macronutrients
                                        Html.tr [
                                            prop.children [
                                                Html.th [
                                                    prop.scope "row"
                                                    prop.text "Fat"
                                                    prop.className "col-4 ps-3"
                                                ]

                                                Html.td [
                                                    this.MacronutrientTable(calsText = this.FatCalsText, gramsText = this.FatGramsText)
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]