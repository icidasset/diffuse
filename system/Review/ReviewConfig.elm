module ReviewConfig exposing (config)

import NoDebug.Log
import NoDebug.TodoOrToString
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Variables
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoDebug.Log.rule

    -- , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.Dependencies.rule

    -- , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Variables.rule
    ]
