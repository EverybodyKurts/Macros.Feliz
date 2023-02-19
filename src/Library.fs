namespace App

module Library =
    [<AutoOpen>]
    module UnitsOfMeasure =
        [<Measure>] type kg
        [<Measure>] type lb
        [<Measure>] type pct

        let lbPerKg = 2.20462262<lb/kg>
        let kgPerLb = 1.0 / lbPerKg

    module Domain =
        type MassUnit =
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

        type Mass =
            | Lb of lb : float<lb>
            | Kg of kg : float<kg>

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

        type BodyComposition = {
            Weight: Mass
            BodyfatPercentage: uint<pct>
        }

    module Form =
        open FsToolkit.ErrorHandling

        type Weight = {
            Amount: float option
            Unit: Domain.MassUnit
        } with
            static member Create(weight: Domain.Mass) : Weight =
                match weight with
                | Domain.Mass.Kg kg ->
                    {
                        Amount = Some <| float kg
                        Unit = Domain.MassUnit.Kg
                    }
                | Domain.Mass.Lb lb ->
                    {
                        Amount = Some <| float lb
                        Unit = Domain.MassUnit.Lb
                    }

            static member Default : Weight =
                {
                    Amount = None
                    Unit = Domain.MassUnit.Kg
                }

            member this.Validate() =
                match this.Amount, this.Unit with
                | Some amount, Domain.MassUnit.Lb when amount >= 0 -> Ok <| Domain.Mass.CreateLb amount
                | Some amount, Domain.MassUnit.Kg when amount >= 0 -> Ok <| Domain.Mass.CreateKg amount
                | Some _, _ -> Error "Weight amount must be >= 0"
                | None, _ -> Error "Weight amount must be present"

            member this.ToKg() : Weight =
                match this.Validate() with
                | Ok weight -> weight.ToKg() |> Weight.Create
                | Error _ -> { this with Unit = Domain.MassUnit.Kg }

            member this.ToLb() : Weight =
                match this.Validate() with
                | Ok weight -> weight.ToLb() |> Weight.Create
                | Error _ -> { this with Unit = Domain.MassUnit.Lb }

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
                match this.BodyfatPercentage with
                | Some bfPct when bfPct >= 0 -> Ok <| (uint bfPct) * 1u<pct>
                | Some _ -> Error "Bodyfat % must be >= 0"
                | None -> Error "Bodyfat % must be present"

            member this.Validate() : Validation<Domain.BodyComposition, string> =
                validation {
                    let! weight = this.Weight.Validate()
                    and! bfPct = this.ValidatePercentage()

                    return {
                        Weight = weight
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

