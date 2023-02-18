module Main

open Feliz
open App
open Browser.Dom
open Fable.Core.JsInterop

importSideEffects "./styles/global.scss"

let htmlElement= document.getElementById "feliz-app"
let root = ReactDOM.createRoot htmlElement

root.render (Components.Router())