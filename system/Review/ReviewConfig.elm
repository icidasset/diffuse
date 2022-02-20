module ReviewConfig exposing (config)

import NoDebug.Log
import NoDebug.TodoOrToString
import NoDeprecated
import NoDuplicatePorts
import NoExposingEverything
import NoImportingEverything
import NoMissingSubscriptionsCall
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoPrematureLetComputation
import NoRecursiveUpdate
import NoUnoptimizedRecursion
import NoUnsafeDivision
import NoUnsafePorts
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Variables
import NoUnusedPorts
import NoUselessSubscriptions
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoDebug.Log.rule
    , NoDeprecated.rule NoDeprecated.defaults
    , NoMissingSubscriptionsCall.rule
    -- , NoPrematureLetComputation.rule
    -- , NoRecursiveUpdate.rule
    -- , NoUnoptimizedRecursion.rule (NoUnoptimizedRecursion.optOutWithComment "IGNORE TCO")

    -- , NoUnsafeDivision.rule
    -- , NoUselessSubscriptions.rule
    --
    -- Unused
    ---------
    , NoUnused.Dependencies.rule
    , NoUnused.Modules.rule
    , NoUnused.Variables.rule
    ]
