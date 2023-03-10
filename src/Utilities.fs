namespace App

module Utilities =
    [<RequireQualifiedAccess>]
    module Result =
        /// `Result.toOption` included with dotnet is not compatible with Fable.
        let inline toOption (result: Result<'ok, 'err>) : 'ok option =
            match result with
            | Ok ok -> Some ok
            | Error _ -> None

    [<RequireQualifiedAccess>]
    module Option =
        let inline toResult (error: 'err) (option: 'ok option) : Result<'ok, 'err> =
            match option with
            | Some ok -> Ok ok
            | None -> Error error
