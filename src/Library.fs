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
            /// See if a float with a generic unit of measure is between a min and max value.
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
    /// for retrieving data, parsing data, or validating data. That's handled by modules that wrap around
    /// this one.
    [<AutoOpen>]
    module Domain =
        type Mass =
            | Lb of lb : float<lb>
            | Kg of kg : float<kg>

            /// Subtract one mass from another
            ///
            ///    (Lb 5.0<lb>) - (Lb 3.0<lb>) = (Lb 2.0<lb>)
            ///    (Kg 5.0<kg>) - (Kg 3.0<kg>) = (Kg 2.0<kg>)
            static member (-) (mass1: Mass, mass2: Mass) : Mass =
                match mass1, mass2 with
                | Lb lb1, Lb lb2 -> Lb <| lb1 - lb2
                | Lb lb, Kg kg -> Lb <| lb - (kg * lbPerKg)
                | Kg kg1, Kg kg2 -> Kg <| kg1 - kg2
                | Kg kg, Lb lb -> Kg <| kg - (lb * kgPerLb)

            /// Multiply a mass by a number
            ///
            ///    (Lb 5.0<lb>) * 2.0 = (Lb 10.0<lb>)
            ///    (Kg 5.0<kg>) * 2.0 = (Kg 10.0<kg>)
            static member (*) (mass: Mass, num: float) : Mass =
                match mass with
                | Lb lb -> Lb <| lb * num
                | Kg kg -> Kg <| kg * num

            /// Convert the mass to pounds
            ///
            ///    (Lb 5.0<lb>).ToLb() = (Lb 5.0<lb>)
            ///    (Kg 5.0<kg>).ToLb() = (Lb 11.02<lb>)
            member this.ToLb () : Mass =
                match this with
                | Kg kg -> Lb <| kg * lbPerKg
                | _ -> this

            /// Convert the mass to kilograms
            ///
            ///    (Lb 5.0<lb>).ToKg() = (Kg 2.27<kg>)
            ///    (Kg 5.0<kg>).ToKg() = (Kg 5.0<kg>)
            member this.ToKg () : Mass =
                match this with
                | Lb lb -> Kg <| lb * kgPerLb
                | _ -> this

            /// Create a mass whose unit of measure is lbs
            ///
            ///    Mass.CreateLb 5.0 = (Lb 5.0<lb>)
            static member CreateLb (amount: float) : Mass =
                Lb <| amount * 1.0<lb>

            /// Create a mass whose unit of measure is kgs
            ///
            ///    Mass.CreateKg 5.0 = (Kg 5.0<kg>)
            static member CreateKg (amount: float) : Mass =
                Kg <| amount * 1.0<kg>

            /// Represent the mass as a string
            ///
            ///    (Lb 5.0<lb>).Text = "5.0 lb"
            ///    (Kg 5.0<kg>).Text = "5.0 kg"
            member this.Text : string =
                match this with
                | Kg kg -> $"{Math.Round(float kg, 2)} kg"
                | Lb lb -> $"{Math.Round(float lb, 2)} lb"

            /// Convert the mass to the kg unit of measure
            ///
            ///    (Lb 5.0<lb>).KgMeasure = 2.27<kg>
            member this.KgMeasure : float<kg> =
                match this.ToKg() with
                | Kg kg -> kg
                | _ -> invalidOp "Domain.Mass.KgMeasure threw an exception"

        /// <summary>
        /// Calculate basal metabolic rate with the Katch-McArdle formula
        /// </summary>
        /// <param name="leanBodyMass">Lean body mass in kg</param>
        /// <returns>Basal metabolic rate in kcal</returns>
        let basalMetabolicRate (leanBodyMass: float<kg>) : float<kcal> =
            370.0<kcal> + (21.6<kcal/kg> * leanBodyMass)

        /// <summary>
        /// A person's body composition, which is composed of their body weight and their bodyfat percentage.
        /// </summary>
        /// <param name="BodyWeight">The person's body weight</param>
        /// <param name="BodyfatPercentage">The person's bodyfat percentage</param>
        /// <returns>The person's body composition</returns>
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

            /// A text representation of the basal metabolic rate
            member this.BasalMetabolicRateText : string =
                $"{Math.Round(float this.BasalMetabolicRate, 2)} kcal"

            /// The body fat % as a string
            member this.BodyfatPercentageText : string =
                $"{this.BodyfatPercentage} pct"

            /// Compute body composition at a certain bodyfat % with the current lean muscle mass.
            ///
            ///    { BodyWeight = 100.0<kg>; BodyfatPercentage = 20u<pct> }.AtBodyFatPercentage 10u<pct> = { BodyWeight = 88.0<kg>; BodyfatPercentage = 10u<pct> }
            member this.AtBodyFatPercentage (bodyFatPercentage: uint<pct>) : BodyComposition =
                let bfPct = (float bodyFatPercentage) * 1.0<pct>

                let lmm = this.LeanMuscleMass
                let ratio = 100.0<pct> / (100.0<pct> - bfPct)

                {
                    BodyWeight = lmm * ratio
                    BodyfatPercentage = bodyFatPercentage
                }

            /// Project body composition at current bodyfat % down to 6% bodyfat
            member this.Projections : BodyComposition seq =
                seq { 6u<pct> .. 2u<pct> .. this.BodyfatPercentage }
                |> Seq.sortDescending
                |> Seq.map this.AtBodyFatPercentage
                |> Seq.skipWhile (fun bodyComposition -> bodyComposition = this)

        /// A person's estimated daily activity level
        type DailyActivityLevel =
            | Sedentary
            | ``Mostly Sedentary``
            | ``Lightly Active``
            | ``Highly Active``

            /// The multiplier used in conjunction with a person's basal metabolic rate to find out how many calories they burn daily
            member this.Multiplier : float =
                match this with
                | Sedentary -> 1.15
                | ``Mostly Sedentary`` -> 1.35
                | ``Lightly Active`` -> 1.55
                | ``Highly Active`` -> 1.75

        /// Holds the daily macronutrient breakdown for protein, carb, and fat.
        type DailyMacronutrient = internal {
            Grams: float<g>
            Calories: float<kcal>
            Percentage: float<pct>
        } with
            member this.GramsText : string =
                let amount = Math.Round(float this.Grams, 2)
                $"{amount} g"

            member this.CaloriesText : string =
                let amount = Math.Round(float this.Calories, 2)
                $"{amount} kcal"

            member this.PercentageText : string =
                let amount = Math.Round(float this.Percentage, 2)
                $"{amount} pct"

        module ProteinGramsPerKgLeanBodyMass =
            /// The "recommended" range of protein grams per kilogram of lean body mass
            let range : float<g/kg> seq = seq { 1.6<g/kg> .. 0.1<g/kg> .. 2.2<g/kg> }

        module DailyMacros =
            /// The daily macro percentage breakdown between protein, carbs, & fat, totaling 100%.
            type Percentages = {
                Protein: float<pct>
                Carbs: float<pct>
                Fat: float<pct>
            } with
                /// The default daily macros. It gives a slight edge to protein.
                static member internal Default : Percentages =
                    {
                        Protein = 34.0<pct>
                        Carbs = 33.0<pct>
                        Fat = 33.0<pct>
                    }

                /// Create a daily macro percentage breakdown between protein, carbs, & fat, totaling 100%.
                /// This static method is accessibly only within the DailyMacros module.
                static member internal Create (protein: float<pct>, carbs: float<pct>, fat: float<pct>) : Percentages =
                    let total = protein + carbs + fat

                    if total <> 100.0<pct> then
                        raise <| ArgumentException $"Protein, carbs, & fat percentages must sum to 100 pct. Protein = {protein}, Carbs = {carbs}, Fat = {fat}"
                    else
                        {
                            Protein = protein
                            Carbs = carbs
                            Fat = fat
                        }

        /// A person's daily macros, based off their body composition, daily activit level, and daily macro percentages.
        type DailyMacros = {
            BodyComposition: BodyComposition
            DailyActivityLevel: DailyActivityLevel
            Percentages: DailyMacros.Percentages
        }  with
            member this.LeanMuscleMass : Mass =
                this.BodyComposition.LeanMuscleMass

            member this.BodyWeight : Mass =
                this.BodyComposition.BodyWeight

            /// Calculate the total # of calories the person burns daily based on their body composition & daily activity level
            member this.TotalCalories : float<kcal> =
                this.DailyActivityLevel.Multiplier * this.BodyComposition.BasalMetabolicRate

            /// The person's daily protein intake in grams, calories, and macro percentage
            member this.Protein : DailyMacronutrient =
                let decimal = (this.Percentages.Protein |> float) / 100.0
                let calories = this.TotalCalories * decimal
                let grams = calories / Macronutrients.ProteinCaloriesPerGram

                {
                    Grams = grams
                    Calories = calories
                    Percentage = this.Percentages.Protein
                }

            /// The person's protein grams per kg of lean body mass. Some nutrition sources (on the internet) use this ratio to calculate protein intake.
            member this.ProteinGramsPerKgLeanBodyMass : float<g/kg> =
                this.Protein.Grams / this.LeanMuscleMass.KgMeasure

            /// The person's protein grams per kg of body weight. Some nutrition sources (on the internet) use this ratio calculate protein intake.
            member this.ProteinGramsPerKgBodyWeight : float<g/kg> =
                this.Protein.Grams / this.BodyWeight.KgMeasure

            /// The person's carb grams per kg of lean body mass. Some nutrition sources (on the internet) use this ratio to calculate carb intake.
            member this.CarbGramsPerKgLeanBodyMass : float<g/kg> =
                this.Carbs.Grams / this.LeanMuscleMass.KgMeasure

            /// The person's carb grams per kg of body weight. Some nutrition sources (on the internet) use this ratio to calculate carb intake.
            member this.CarbGramsPerKgBodyWeight : float<g/kg> =
                this.Carbs.Grams / this.BodyWeight.KgMeasure

            /// The person's carb intake in grams, calories, and macro percentage
            member this.Carbs: DailyMacronutrient =
                let decimal = (this.Percentages.Carbs |> float) / 100.0
                let calories = this.TotalCalories * decimal
                let grams = calories / Macronutrients.CarbCaloriesPerGram

                {
                    Grams = grams
                    Calories = calories
                    Percentage = this.Percentages.Carbs
                }

            /// The person's fat intake in grams, calories, and macro percentage
            member this.Fat: DailyMacronutrient =
                let decimal = (this.Percentages.Fat |> float) / 100.0
                let calories = this.TotalCalories * decimal
                let grams = calories / Macronutrients.FatCaloriesPerGram

                {
                    Grams = grams
                    Calories = calories
                    Percentage = this.Percentages.Fat
                }

            /// Calculate the person's daily macros at a specified bodyfat percentage. This is useful for projecting macros at lower bodyfat percentages.
            member this.AtBodyFatPercentage (bodyFatPercentage: uint<pct>) : DailyMacros =
                let bodyComposition = this.BodyComposition.AtBodyFatPercentage bodyFatPercentage

                { this with
                    BodyComposition = bodyComposition
                }

    /// Handles user input that is not yet validated
    module Input =
        open FsToolkit.ErrorHandling
        open Domain

        /// The unit of measure for body weight. Users can select between the two weight units as a dropdown option.
        type WeightUnit =
            | Kg
            | Lb

            /// Whether or not the selected weight unit is kg
            member this.IsKilogram : bool =
                match this with
                | Kg -> true
                | _ -> false

            /// Whether or not the selected weight unit is lb
            member this.IsPound : bool =
                match this with
                | Lb -> true
                | _ -> false

        /// Body weight input. When the user first loads the page, the bodyweight amount is absent and the unit of measure defaults to lb.
        type Weight = {
            Amount: float option
            Unit: WeightUnit
        } with
            /// Create a body weight input based on validated body weight
            static member Create (weight: Domain.Mass) : Weight =
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

            /// The default body weight input. The amount is absent and the unit of measure is lb.
            static member Default : Weight =
                {
                    Amount = None
                    Unit = Lb
                }

            /// Validate body weight input. Amount must be a parseable float that is >= 0.
            member this.Validate () : Result<Domain.Mass, string> =
                match this.Amount, this.Unit with
                | Some amount, Lb when amount >= 0 -> Ok <| Mass.CreateLb amount
                | Some amount, Kg when amount >= 0 -> Ok <| Mass.CreateKg amount
                | Some _, _ -> Error "Weight amount must be >= 0"
                | None, _ -> Error "Weight amount must be present"

            /// Convert body weight to kg.
            /// - If the body weight is a valid amount, it will be converted to kg.
            /// - If the body weight is not a valid amount, the unit will be set to kg.
            member this.ToKg () : Weight =
                match this.Validate () with
                | Ok weight -> weight.ToKg () |> Weight.Create
                | Error _ -> { this with Unit = Kg }

            /// Convert bodyweight to lb.
            /// - If the body weight is a valid amount, it will be converted to lb.
            /// - If the body weight is not a valid amount, the unit will be set to lb.
            member this.ToLb () : Weight =
                match this.Validate () with
                | Ok weight -> weight.ToLb () |> Weight.Create
                | Error _ -> { this with Unit = Lb }

        /// Body composition input. When the user first loads the page, the bodyfat percentage is absent.
        type BodyComposition = {
            Weight: Weight
            BodyfatPercentage: int option
        } with
            /// The default body composition input. The bodyfat percentage is absent.
            static member Default : BodyComposition =
                {
                    Weight = Weight.Default
                    BodyfatPercentage = None
                }

            /// Create a body composition input based on validated body composition
            static member Create (bodyComposition: Domain.BodyComposition) : BodyComposition =
                {
                    Weight = Weight.Create bodyComposition.BodyWeight
                    BodyfatPercentage = Some <| int bodyComposition.BodyfatPercentage
                }

            /// <summary>
            /// Validate the bodyfat percentage. It must be between 0% and 100%.
            /// </summary>
            /// <returns>A result that contains either the validated bodyfat percentage or an error describing why the input was invalid.</returns>
            member private this.ValidatePercentage() : Result<uint<pct>, string> =
                match this.BodyfatPercentage with
                | Some bodyfatPercentage when 0 <= bodyfatPercentage && bodyfatPercentage <= 100 ->
                    Ok <| (uint bodyfatPercentage) * 1u<pct>
                | Some _ -> Error "Bodyfat % must be betwen 0 and 100"
                | None -> Error "Bodyfat % must be present"

            /// Validate the body composition input. The bodyfat percentage must be between 0% and 100%.
            /// If the bodyfat percentage is valid, the body composition will be validated.
            member this.Validate() : Validation<Domain.BodyComposition, string> =
                validation {
                    let! weight = this.Weight.Validate()
                    and! bfPct = this.ValidatePercentage()

                    return {
                        BodyWeight = weight
                        BodyfatPercentage = bfPct
                    }
                }

            /// Convert the body composition weight to kg.
            member this.ToKg() : BodyComposition =
                { this with
                    Weight = this.Weight.ToKg()
                }

            /// Convert the body composition weight to lb.
            member this.ToLb() : BodyComposition =
                { this with
                    Weight = this.Weight.ToLb()
                }

            /// Update the body composition weight amount
            member this.UpdateWeightAmount (amount: float) : BodyComposition =
                { this with BodyComposition.Weight.Amount = Some amount }

            member this.UpdateBodyfatPercentage (percentage: int) : BodyComposition =
                { this with BodyfatPercentage = Some percentage }

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

                member this.Validate () : Result<DailyMacros.Percentages, string> =
                    match this.Protein, this.Carbs, this.Fat with
                    // all macronutrients provided, are valid, and sum to 100%
                    | ValidPercentage p, ValidPercentage c, ValidPercentage f when p + c + f = 100.0<pct> ->
                        Ok <| DailyMacros.Percentages.Create(protein = p, carbs = c, fat = f)

                    // all macronutrients are provided and valid but do not sum to 100%
                    | ValidPercentage p, ValidPercentage c, ValidPercentage f ->
                        Error $"Macro percentages need to sum to 100 percent. Protein = {p}, Carbs = {c}, Fat = {f}"

                    // any other case, error out
                    | _ ->
                        Error $"Invalid percentages. Protein = {this.Protein}, Carbs = {this.Carbs}, Fat = {this.Fat} "

                member this.UpdateProtein (protein: float) : Percentages =
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

        module DailyMacronutrient =
            let table (dailyMacronutrient: DailyMacronutrient option) : ReactElement =
                let caloriesText = dailyMacronutrient |> Option.map _.CaloriesText |> Option.defaultValue ""
                let gramsText = dailyMacronutrient |> Option.map _.GramsText |> Option.defaultValue ""

                Html.table [
                    Html.tr [
                        Html.th [
                            prop.scope "row"
                            prop.text "kCal"
                        ]

                        Html.td [
                            prop.text caloriesText
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

            let headerTableRow (title: string) (dailyMacronutrient: DailyMacronutrient option) : ReactElement =
                Html.tr [
                    prop.children [
                        Html.th [
                            prop.scope "row"
                            prop.text title
                            prop.className "col-4 ps-3"
                        ]

                        Html.td [
                            table dailyMacronutrient
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
                            DailyMacros.Percentages.Default.Protein |> float

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
                            DailyMacros.Percentages.Default.Carbs |> float

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
                            DailyMacros.Percentages.Default.Fat |> float

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

        module CalculatorResults =
            let tableRow (title: string) (text: string) =
                Html.tr [
                    prop.children [
                        Html.th [
                            prop.scope "row"
                            prop.text title
                            prop.className "col-4 ps-3"
                        ]

                        Html.td [
                            prop.text text
                        ]
                    ]
                ]

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

                    return bc.BasalMetabolicRateText
                } |> Option.defaultValue ""

            member private this.TotalCaloriesText : string =
                option {
                    let! dm = this.DailyMacros
                    let totCals = Math.Round(float dm.TotalCalories, 2)

                    return $"{totCals} kcal"
                } |> Option.defaultValue ""

            member this.BodyCompositionCard : ReactElement =
                let bodyCompositionText =
                    option {
                        let! bc = this.BodyComposition

                        return $"Body Composition @ {bc.BodyfatPercentageText}"
                    } |> Option.defaultValue "Body Composition"

                Html.div [
                    prop.className "card mt-3"
                    prop.children [
                        Html.div [
                            prop.className "card-header"
                            prop.text bodyCompositionText
                        ]

                        Html.table [
                            prop.className "table mb-0"

                            prop.children [
                                Html.tbody [
                                    CalculatorResults.tableRow "Lean Muscle Mass" this.LmmText
                                    CalculatorResults.tableRow "Fat Mass" this.FatMassText
                                    CalculatorResults.tableRow "Resting Metabolic Rate" this.BasalMetabolicRateText
                                ]
                            ]
                        ]
                    ]
                ]

            member private this.TryProteinMacros : DailyMacronutrient option =
                this.DailyMacros |> Option.map _.Protein

            member private this.TryCarbMacros : DailyMacronutrient option =
                this.DailyMacros |> Option.map _.Carbs

            member private this.TryFatMacros : DailyMacronutrient option =
                this.DailyMacros |> Option.map _.Fat

            member private this.ProteinGramsPerKgBodyweightText : string =
                option {
                    let! dm = this.DailyMacros
                    let pgpkbw = dm.ProteinGramsPerKgBodyWeight

                    return $"{Math.Round(float pgpkbw, 2)} g / kg"
                } |> Option.defaultValue ""

            member private this.ProteinGramsPerKgLeanMuscleMassText : string =
                option {
                    let! dm = this.DailyMacros
                    let lmm = dm.ProteinGramsPerKgLeanBodyMass

                    return $"{Math.Round(float lmm, 2)} g / kg"
                } |> Option.defaultValue ""

            member private this.CarbGramsPerKgBodyweightText : string =
                option {
                    let! dm = this.DailyMacros
                    let bw = dm.CarbGramsPerKgBodyWeight

                    return $"{Math.Round(float bw, 2)} g / kg"
                } |> Option.defaultValue ""

            member private this.CarbGramsPerKgLeanMuscleMassText: string =
                option {
                    let! dm = this.DailyMacros
                    let lmm = dm.CarbGramsPerKgLeanBodyMass

                    return $"{Math.Round(float lmm, 2)} g / kg"
                } |> Option.defaultValue ""

            member this.DailyMacrosCard : ReactElement =
                Html.div [
                    prop.className "card mt-3"
                    prop.children [
                        Html.div [
                            prop.className "card-header"
                            prop.text "Daily Macros Result"
                        ]

                        Html.table [
                            prop.classes ["table"; "table-sm"; "mb-0"]

                            prop.children [
                                Html.tbody [
                                    prop.children [
                                        CalculatorResults.tableRow "Tot Cals / Day" this.TotalCaloriesText
                                        CalculatorResults.tableRow "Protein Grams / Kg Bodyweight" this.ProteinGramsPerKgBodyweightText
                                        CalculatorResults.tableRow "Protein Grams / Kg Lean Muscle Mass" this.ProteinGramsPerKgLeanMuscleMassText
                                        CalculatorResults.tableRow "Carb Grams / Kg Bodyweight" this.CarbGramsPerKgBodyweightText
                                        CalculatorResults.tableRow "Carb Grams / Kg Lean Muscle Mass" this.CarbGramsPerKgLeanMuscleMassText

                                        DailyMacronutrient.headerTableRow "Protein" this.TryProteinMacros
                                        DailyMacronutrient.headerTableRow "Carbs" this.TryCarbMacros
                                        DailyMacronutrient.headerTableRow "Fat" this.TryFatMacros
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]