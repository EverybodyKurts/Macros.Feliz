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

            member this.AsFloat : float =
                match this with
                | Lb lb -> float lb
                | Kg kg -> float kg

            member this.UpdateAmount (amount: float) : Mass =
                match this with
                | Lb _ -> Lb <| amount * 1.0<lb>
                | Kg _ -> Kg <| amount * 1.0<kg>

            static member CreateLb(amount: float) : Mass =
                Lb <| amount * 1.0<lb>

            static member CreateKg(amount: float) : Mass =
                Kg <| amount * 1.0<kg>

            static member Create(amount: float, massUnit: MassUnit) : Mass =
                match massUnit with
                | MassUnit.Kg -> Mass.CreateKg amount
                | MassUnit.Lb -> Mass.CreateLb amount

        type BodyComposition = {
            Weight: Mass
            BodyfatPercentage: uint<pct>
        }