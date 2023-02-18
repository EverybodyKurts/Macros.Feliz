namespace App

open Feliz
open Fable.React

module Bootstrap =
    let fluidContainer (children: ReactElement seq) : ReactElement =
        Html.div [
            prop.className "container-fluid"
            prop.children children
        ]

    let row (children: ReactElement seq) : ReactElement =
        Html.div [
            prop.className "row"
            prop.children children
        ]

    let col (children: ReactElement seq) : ReactElement =
        Html.div [
            prop.className "col"
            prop.children children
        ]