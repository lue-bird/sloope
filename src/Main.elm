port module Main exposing (main)

-- I use a bunch of physics terms
-- incorrectly, it's easier for my brain that way but I'm sorry!

import Angle exposing (Angle)
import Arc2d exposing (Arc2d)
import Axis2d
import Browser
import Browser.Dom
import Browser.Events
import Color exposing (Color)
import Direction2d exposing (Direction2d)
import Duration exposing (Duration)
import Html exposing (Html)
import Json.Decode
import Length exposing (Length)
import LineSegment2d exposing (LineSegment2d)
import Parameter1d
import Platform.Cmd as Cmd
import Point2d exposing (Point2d)
import Polyline2d
import Quantity exposing (Quantity)
import Quantity.Interval
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Svg.PathD
import Task
import Time
import Vector2d exposing (Vector2d)


port playAudio :
    { name : String
    , volume : Float
    , playbackRate : Float
    , loop : Bool
    }
    -> Cmd event_


type alias State =
    { windowSize : { height : Float, width : Float }
    , specific : StateSpecific
    }


type StateSpecific
    = StateMenu
    | StateGameplay GameplayState


type alias GameplayState =
    { lastSimulationTime : Maybe Time.Posix
    , startTime : Maybe Time.Posix
    , playerInputSpeed :
        -- the unit of this is super fake
        -- but I don't really know what it is exactly
        Quantity Float (Quantity.Rate Length.Meters Duration.Seconds)
    , forwardsInputActive : Bool
    , backwardsInputActive : Bool
    , motorbikeCenter : Point2d Length.Meters ()
    , motorbikeAngle : Angle
    , motorbikeVelocity : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) ()
    , motorbikeRotationalSpeed : Quantity Float (Quantity.Rate Length.Meters Duration.Seconds)
    , motorbikeWheelAngle : Angle
    , lastPathContactTime : Maybe Time.Posix
    }


type Event
    = WindowSized { width : Float, height : Float }
    | StartButtonPressed
    | RespawnKeyDown
    | StartTimeReceived Time.Posix
    | SimulationTick Time.Posix
    | GameplayKeyDown GameplayKey
    | GameplayKeyUp GameplayKey


main : Program () State Event
main =
    Browser.document
        { init =
            \() ->
                ( { windowSize = { width = 1920, height = 1080 }
                  , specific = StateMenu
                  }
                , Browser.Dom.getViewport
                    |> Task.perform
                        (\viewport ->
                            WindowSized
                                { width = viewport.viewport.width
                                , height = viewport.viewport.height
                                }
                        )
                )
        , view =
            \state ->
                { title = "veloop"
                , body =
                    [ stateToHtml state
                    ]
                }
        , update = reactToEvent
        , subscriptions = stateToSubscriptions
        }


initialGameplayState : GameplayState
initialGameplayState =
    { lastSimulationTime = Nothing
    , startTime = Nothing
    , playerInputSpeed =
        Length.meters 0 |> Quantity.per Duration.second
    , forwardsInputActive = False
    , backwardsInputActive = False
    , motorbikeCenter =
        -- change to get a "checkpoint"
        Point2d.meters -0.6 0.5
    , motorbikeVelocity =
        Vector2d.meters 0.2 0
            |> Vector2d.per Duration.second
    , motorbikeAngle = Angle.turns -0.12
    , motorbikeRotationalSpeed =
        Length.meters 0.02
            |> Quantity.per Duration.second
    , motorbikeWheelAngle = Angle.turns 0.123
    , lastPathContactTime = Nothing
    }


stateToSubscriptions : State -> Sub Event
stateToSubscriptions state =
    [ Browser.Events.onResize
        (\width height ->
            WindowSized
                { width = width |> Basics.toFloat
                , height = height |> Basics.toFloat
                }
        )
    , case state.specific of
        StateMenu ->
            Browser.Events.onKeyDown
                (Json.Decode.map (\() -> StartButtonPressed)
                    spaceKeyJsonDecoder
                )

        StateGameplay _ ->
            [ Time.every (1000 / 60)
                SimulationTick
            , Browser.Events.onKeyDown
                (Json.Decode.map GameplayKeyDown gameplayKeyJsonDecoder)
            , Browser.Events.onKeyUp
                (Json.Decode.map GameplayKeyUp gameplayKeyJsonDecoder)
            , Browser.Events.onKeyDown
                (Json.Decode.map (\() -> RespawnKeyDown) respawnKeyJsonDecoder)
            ]
                |> Sub.batch
    ]
        |> Sub.batch


respawnKeyJsonDecoder : Json.Decode.Decoder ()
respawnKeyJsonDecoder =
    Json.Decode.andThen
        (\key ->
            case key of
                "r" ->
                    Json.Decode.succeed ()

                _ ->
                    Json.Decode.fail "unknown key, ignore"
        )
        (Json.Decode.field "key" Json.Decode.string)


type GameplayKey
    = GameplayKeyArrowLeft
    | GameplayKeyArrowRight


gameplayKeyJsonDecoder : Json.Decode.Decoder GameplayKey
gameplayKeyJsonDecoder =
    Json.Decode.andThen
        (\key ->
            case key of
                "ArrowLeft" ->
                    Json.Decode.succeed GameplayKeyArrowLeft

                "ArrowRight" ->
                    Json.Decode.succeed GameplayKeyArrowRight

                _ ->
                    Json.Decode.fail "unknown key, ignore"
        )
        (Json.Decode.field "key" Json.Decode.string)


reactToEvent : Event -> State -> ( State, Cmd Event )
reactToEvent event state =
    case event of
        WindowSized newSize ->
            ( { state
                | windowSize = newSize
              }
            , Cmd.none
            )

        StartButtonPressed ->
            ( { state
                | specific = StateGameplay initialGameplayState
              }
            , [ Time.now
                    |> Task.perform StartTimeReceived
              , playAudio
                    { name = "music"
                    , volume = 0.5
                    , playbackRate = 1
                    , loop = True
                    }
              ]
                |> Cmd.batch
            )

        RespawnKeyDown ->
            case state.specific of
                StateGameplay _ ->
                    ( { state
                        | specific = StateGameplay initialGameplayState
                      }
                    , Time.now
                        |> Task.perform StartTimeReceived
                    )

                StateMenu ->
                    ( state, Cmd.none )

        StartTimeReceived startTime ->
            case state.specific of
                StateGameplay gameplayState ->
                    ( { state
                        | specific =
                            StateGameplay { gameplayState | startTime = Just startTime }
                      }
                    , Cmd.none
                    )

                StateMenu ->
                    ( state, Cmd.none )

        GameplayKeyDown gameplayKey ->
            case state.specific of
                StateGameplay gameplayState ->
                    ( { state
                        | specific =
                            StateGameplay
                                (case gameplayKey of
                                    GameplayKeyArrowRight ->
                                        { gameplayState | forwardsInputActive = True }

                                    GameplayKeyArrowLeft ->
                                        { gameplayState | backwardsInputActive = True }
                                )
                      }
                    , if
                        Basics.not gameplayState.forwardsInputActive
                            && Basics.not gameplayState.backwardsInputActive
                            && (gameplayState.playerInputSpeed
                                    |> Quantity.abs
                                    |> Quantity.lessThan
                                        (Length.meters 0.1
                                            |> Quantity.per Duration.second
                                        )
                               )
                      then
                        case gameplayKey of
                            GameplayKeyArrowLeft ->
                                playAudio
                                    { name = "tough-motorbike-decelerate"
                                    , playbackRate = 1.35
                                    , volume = 0.051
                                    , loop = False
                                    }

                            GameplayKeyArrowRight ->
                                playAudio
                                    { name = "motorbike-accelerate"
                                    , playbackRate = 1
                                    , volume = 0.465
                                    , loop = False
                                    }

                      else
                        Cmd.none
                    )

                StateMenu ->
                    ( state, Cmd.none )

        GameplayKeyUp gameplayKey ->
            case state.specific of
                StateGameplay gameplayState ->
                    ( { state
                        | specific =
                            StateGameplay
                                (case gameplayKey of
                                    GameplayKeyArrowRight ->
                                        { gameplayState | forwardsInputActive = False }

                                    GameplayKeyArrowLeft ->
                                        { gameplayState | backwardsInputActive = False }
                                )
                      }
                    , Cmd.none
                    )

                StateMenu ->
                    ( state, Cmd.none )

        SimulationTick currentTime ->
            case state.specific of
                StateGameplay gameplayState ->
                    case gameplayState.lastSimulationTime of
                        Nothing ->
                            ( { state
                                | specific =
                                    StateGameplay
                                        { gameplayState | lastSimulationTime = Just currentTime }
                              }
                            , Cmd.none
                            )

                        Just lastSimulationTime ->
                            if
                                ((gameplayState.motorbikeCenter |> Point2d.yCoordinate)
                                    |> Quantity.lessThanOrEqualTo belowIsRespawn
                                )
                                    || ((gameplayState.motorbikeCenter |> Point2d.yCoordinate)
                                            |> Quantity.greaterThanOrEqualTo minimumDeathHeight
                                       )
                            then
                                ( { state
                                    | specific =
                                        StateGameplay
                                            initialGameplayState
                                  }
                                , Time.now
                                    |> Task.perform StartTimeReceived
                                )

                            else
                                let
                                    durationSinceLastTick : Duration
                                    durationSinceLastTick =
                                        Duration.from lastSimulationTime
                                            currentTime

                                    peekStateIfNoCollision : GameplayState
                                    peekStateIfNoCollision =
                                        let
                                            newMotorbikeWheelAngle : Quantity Float Angle.Radians
                                            newMotorbikeWheelAngle =
                                                gameplayState.motorbikeWheelAngle
                                                    |> Quantity.minus
                                                        (Angle.turns
                                                            (((gameplayState.playerInputSpeed
                                                                |> Quantity.for Duration.second
                                                                |> Length.inMeters
                                                                |> abs
                                                              )
                                                                / ((playerLengthBackToFrontAxis |> Length.inMeters)
                                                                    * pi
                                                                  )
                                                             )
                                                                ^ -- keep spinning even when input is faint
                                                                  0.21
                                                                * (gameplayState.playerInputSpeed
                                                                    |> Quantity.for Duration.second
                                                                    |> Quantity.sign
                                                                  )
                                                            )
                                                            |> quantityClampAbsToAtLeast
                                                                (Angle.turns 0.0019)
                                                            |> quantityClampAbsToAtMost
                                                                (Angle.turns 0.099)
                                                        )

                                            newPlayerInputSpeed : Quantity Float (Quantity.Rate Length.Meters Duration.Seconds)
                                            newPlayerInputSpeed =
                                                gameplayState.playerInputSpeed
                                                    |> Quantity.multiplyBy 0.89
                                                    |> Quantity.plus
                                                        (Length.meters
                                                            (0.1
                                                                * ((if gameplayState.forwardsInputActive then
                                                                        1

                                                                    else
                                                                        0
                                                                   )
                                                                    + (if gameplayState.backwardsInputActive then
                                                                        -1

                                                                       else
                                                                        0
                                                                      )
                                                                  )
                                                            )
                                                            |> Quantity.per Duration.second
                                                        )
                                                    |> quantityClampAbsToAtMost
                                                        (Length.meters 0.42
                                                            |> Quantity.per Duration.second
                                                        )

                                            newMotorbikeVelocity : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) ()
                                            newMotorbikeVelocity =
                                                gameplayState.motorbikeVelocity
                                                    |> Vector2d.plus
                                                        (gravity
                                                            |> Vector2d.for durationSinceLastTick
                                                        )
                                                    |> Vector2d.scaleBy 0.996
                                                    |> vector2dClampToMaxLength
                                                        (Length.meters 5.2
                                                            |> Quantity.per Duration.second
                                                        )

                                            newMotorbikeRotationalSpeed : Quantity Float (Quantity.Rate Length.Meters Duration.Seconds)
                                            newMotorbikeRotationalSpeed =
                                                -- prefer straightened out to current velocity direction?
                                                gameplayState.motorbikeRotationalSpeed
                                                    |> Quantity.multiplyBy 0.99
                                                    |> quantityClampAbsToAtMost
                                                        (Length.meters 0.9
                                                            |> Quantity.per Duration.second
                                                        )

                                            newMotorbikeRotationToApply : Angle
                                            newMotorbikeRotationToApply =
                                                Angle.turns
                                                    ((newMotorbikeRotationalSpeed
                                                        |> Quantity.for durationSinceLastTick
                                                        |> Length.inMeters
                                                     )
                                                        / ((playerLengthBackToFrontAxis |> Length.inMeters)
                                                            * pi
                                                          )
                                                    )
                                        in
                                        { gameplayState
                                            | lastSimulationTime = Just currentTime
                                            , motorbikeRotationalSpeed = newMotorbikeRotationalSpeed
                                            , motorbikeAngle =
                                                gameplayState.motorbikeAngle
                                                    |> Quantity.plus newMotorbikeRotationToApply
                                                    |> Angle.normalize
                                            , motorbikeVelocity = newMotorbikeVelocity
                                            , motorbikeCenter =
                                                gameplayState.motorbikeCenter
                                                    |> Point2d.translateBy
                                                        (newMotorbikeVelocity
                                                            |> Vector2d.for durationSinceLastTick
                                                        )
                                            , playerInputSpeed = newPlayerInputSpeed
                                            , motorbikeWheelAngle = newMotorbikeWheelAngle
                                        }
                                in
                                ( { state
                                    | specific =
                                        StateGameplay
                                            (simulateCollisionWithPeek
                                                { countOfAttemptsTryingToResolve = 0
                                                , peekStateIfNoCollision = peekStateIfNoCollision
                                                , durationSinceLastTick = durationSinceLastTick
                                                }
                                                gameplayState
                                            )
                                  }
                                , Cmd.none
                                )

                StateMenu ->
                    ( state, Cmd.none )


simulateCollisionWithPeek :
    { countOfAttemptsTryingToResolve : Int
    , durationSinceLastTick : Duration
    , peekStateIfNoCollision : GameplayState
    }
    -> GameplayState
    -> GameplayState
simulateCollisionWithPeek config state =
    if config.countOfAttemptsTryingToResolve >= 25 then
        -- let
        --     _ =
        --         Debug.log "failed to resolve collision, potentially clipping" ()
        -- in
        -- clip, which will look like a bug to the user
        -- illegal state but what else is there to do.
        -- I think reaching this state (which is actually reached sometimes currently)
        -- means that arc edge colliders are mis-oriented / don't trigger correctly
        config.peekStateIfNoCollision

    else if
        Basics.not
            (motorcycleWouldCollide
                { center = config.peekStateIfNoCollision.motorbikeCenter
                , angle = config.peekStateIfNoCollision.motorbikeAngle
                }
            )
    then
        -- let
        --     _ =
        --         if config.countOfAttemptsTryingToResolve >= 2 then
        --             Debug.log "resolved collision with countOfAttemptsTryingToResolve"
        --                 config.countOfAttemptsTryingToResolve
        --
        --         else
        --             config.countOfAttemptsTryingToResolve
        -- in
        config.peekStateIfNoCollision

    else
        let
            backWheelForce : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) ()
            backWheelForce =
                wheelCombinedCollisionForce
                    { wheelPosition =
                        motorbikeDeriveBackWheelPosition
                            { center = config.peekStateIfNoCollision.motorbikeCenter
                            , angle = config.peekStateIfNoCollision.motorbikeAngle
                            }
                    , wheelRotateDirection =
                        config.peekStateIfNoCollision.motorbikeAngle
                            |> Direction2d.fromAngle
                            |> Direction2d.rotateClockwise
                    , motorbikeVelocity = config.peekStateIfNoCollision.motorbikeVelocity
                    , motorbikeRotationalSpeed = config.peekStateIfNoCollision.motorbikeRotationalSpeed
                    , playerInputSpeed = config.peekStateIfNoCollision.playerInputSpeed
                    }

            frontWheelForce : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) ()
            frontWheelForce =
                wheelCombinedCollisionForce
                    { wheelPosition =
                        motorbikeDeriveFrontWheelPosition
                            { center = config.peekStateIfNoCollision.motorbikeCenter
                            , angle = config.peekStateIfNoCollision.motorbikeAngle
                            }
                    , wheelRotateDirection =
                        config.peekStateIfNoCollision.motorbikeAngle
                            |> Direction2d.fromAngle
                            |> Direction2d.rotateCounterclockwise
                    , motorbikeVelocity = config.peekStateIfNoCollision.motorbikeVelocity
                    , motorbikeRotationalSpeed = config.peekStateIfNoCollision.motorbikeRotationalSpeed
                    , playerInputSpeed = config.peekStateIfNoCollision.playerInputSpeed
                    }

            combinedNonRotationalWheelForce =
                backWheelForce
                    |> Vector2d.plus frontWheelForce

            combinedNonRotationalForceToApply : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) ()
            combinedNonRotationalForceToApply =
                combinedNonRotationalWheelForce
                    |> vector2dClampToMaxLength
                        (Length.meters 5.2
                            |> Quantity.per Duration.second
                        )

            combinedRotationalForceToApply : Quantity Float (Quantity.Rate Length.Meters Duration.Seconds)
            combinedRotationalForceToApply =
                Vector2d.cross
                    (Vector2d.from
                        (motorbikeDeriveBackWheelPosition
                            { center = state.motorbikeCenter
                            , angle = state.motorbikeAngle
                            }
                        )
                        state.motorbikeCenter
                    )
                    backWheelForce
                    |> Quantity.plus
                        (Vector2d.cross
                            (Vector2d.from
                                (motorbikeDeriveFrontWheelPosition
                                    { center = state.motorbikeCenter
                                    , angle = state.motorbikeAngle
                                    }
                                )
                                state.motorbikeCenter
                            )
                            frontWheelForce
                        )
                    |> Quantity.over_ Length.meter
                    |> quantityClampAbsToAtMost
                        (Length.meters 0.9
                            |> Quantity.per Duration.second
                        )
        in
        simulateCollisionWithPeek
            { countOfAttemptsTryingToResolve =
                (config.countOfAttemptsTryingToResolve |> Basics.abs) + 1
            , durationSinceLastTick = config.durationSinceLastTick
            , peekStateIfNoCollision =
                { state
                    | lastSimulationTime = config.peekStateIfNoCollision.lastSimulationTime
                    , lastPathContactTime = config.peekStateIfNoCollision.lastSimulationTime
                    , motorbikeRotationalSpeed = combinedRotationalForceToApply
                    , motorbikeVelocity =
                        combinedNonRotationalForceToApply
                    , motorbikeAngle =
                        state.motorbikeAngle
                            |> Quantity.plus
                                (Angle.turns
                                    ((combinedRotationalForceToApply
                                        |> Quantity.for config.durationSinceLastTick
                                        |> Length.inMeters
                                     )
                                        / ((playerLengthBackToFrontAxis |> Length.inMeters)
                                            * pi
                                          )
                                    )
                                )
                            |> Angle.normalize
                    , motorbikeCenter =
                        state.motorbikeCenter
                            |> Point2d.translateBy
                                (combinedNonRotationalForceToApply
                                    |> Vector2d.for config.durationSinceLastTick
                                )
                    , playerInputSpeed = config.peekStateIfNoCollision.playerInputSpeed
                    , motorbikeWheelAngle =
                        config.peekStateIfNoCollision.motorbikeWheelAngle
                }
            }
            state


motorcycleWouldCollide :
    { center : Point2d Length.Meters ()
    , angle : Angle
    }
    -> Bool
motorcycleWouldCollide newOrientation =
    (motorbikeDeriveBackWheelPosition newOrientation
        |> wheelCollisionsWithDrivingPath
        |> listIsFilled
    )
        || (motorbikeDeriveFrontWheelPosition newOrientation
                |> wheelCollisionsWithDrivingPath
                |> listIsFilled
           )


wheelCombinedCollisionForce :
    { wheelPosition : Point2d Length.Meters ()
    , motorbikeVelocity : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) ()
    , motorbikeRotationalSpeed : Quantity Float (Quantity.Rate Length.Meters Duration.Seconds)
    , wheelRotateDirection : Direction2d ()
    , playerInputSpeed : Quantity Float (Quantity.Rate Length.Meters Duration.Seconds)
    }
    -> Vector2d (Quantity.Rate Length.Meters Duration.Seconds) ()
wheelCombinedCollisionForce stateBeforeCollision =
    let
        forces : List (Vector2d (Quantity.Rate Length.Meters Duration.Seconds) ())
        forces =
            wheelCollisionsWithDrivingPath stateBeforeCollision.wheelPosition
                |> List.foldl
                    (\intersectingLineSegment forceSoFar ->
                        (stateBeforeCollision.motorbikeVelocity
                            |> Vector2d.plus
                                (stateBeforeCollision.motorbikeRotationalSpeed
                                    |> rotationalSpeedAtAngle
                                        stateBeforeCollision.wheelRotateDirection
                                )
                            |> Vector2d.plus
                                (Vector2d.withLength
                                    -- signed length!
                                    (stateBeforeCollision.playerInputSpeed
                                        |> Quantity.for Duration.second
                                    )
                                    (intersectingLineSegment
                                        |> LineSegment2d.direction
                                        |> Maybe.withDefault Direction2d.positiveX
                                    )
                                    |> Vector2d.per Duration.second
                                    |> Vector2d.scaleBy 15
                                )
                            |> Vector2d.mirrorAcross
                                (intersectingLineSegment
                                    |> LineSegment2d.axis
                                    |> Maybe.withDefault Axis2d.x
                                )
                        )
                            :: forceSoFar
                    )
                    []
    in
    case forces of
        [] ->
            Vector2d.zero

        force0 :: force1Up ->
            (force0 :: force1Up)
                |> List.foldl
                    (\force soFar -> soFar |> Vector2d.plus force)
                    Vector2d.zero
                |> Vector2d.scaleBy
                    (1 / (forces |> List.length |> Basics.toFloat))


wheelCollisionsWithDrivingPath :
    Point2d Length.Meters ()
    -> List (LineSegment2d Length.Meters ())
wheelCollisionsWithDrivingPath position =
    let
        wheelGeometry : { radius : Length, position : Point2d Length.Meters () }
        wheelGeometry =
            { radius = wheelRadius
            , position = position
            }
    in
    drivingPath
        |> List.concatMap
            (\pathSegment ->
                wheelCollisionsWithDrivingPathSegment
                    wheelGeometry
                    pathSegment
                    |> List.map
                        (\lineSegment2d ->
                            case pathSegment.drivingDirection of
                                Forwards ->
                                    lineSegment2d

                                Backwards ->
                                    lineSegment2d |> LineSegment2d.reverse
                        )
            )


wheelCollisionsWithDrivingPathSegment :
    { radius : Length, position : Point2d Length.Meters () }
    -> DrivingPathSegment
    -> List (LineSegment2d Length.Meters ())
wheelCollisionsWithDrivingPathSegment wheelGeometry drivingPathSegment =
    let
        drivingPathSegmentAsVeryRoughApproximateLineSegment : LineSegment2d Length.Meters ()
        drivingPathSegmentAsVeryRoughApproximateLineSegment =
            LineSegment2d.from drivingPathSegment.start drivingPathSegment.end
    in
    -- small optimization: skip entire segment if too far away
    if
        point2dDistanceBetween
            wheelGeometry.position
            (drivingPathSegmentAsVeryRoughApproximateLineSegment
                |> LineSegment2d.midpoint
            )
            |> Quantity.greaterThan
                (drivingPathSegmentAsVeryRoughApproximateLineSegment
                    |> LineSegment2d.length
                    |> Quantity.half
                    |> Quantity.plus playerLengthBackToFrontAxis
                )
    then
        []

    else
        -- before doing line collision:
        -- if the wheel is close but off to the start or end,
        -- bounce off the side
        case drivingPathSegmentAsVeryRoughApproximateLineSegment |> LineSegment2d.axis of
            Nothing ->
                -- Debug.todo "*sob*"
                []

            Just segmentAxis ->
                let
                    positionProjectedOntoSegmentAxis : Point2d Length.Meters ()
                    positionProjectedOntoSegmentAxis =
                        wheelGeometry.position |> Point2d.projectOnto segmentAxis
                in
                if
                    (point2dDistanceBetween
                        wheelGeometry.position
                        drivingPathSegment.end
                        |> Quantity.lessThanOrEqualTo
                            ((wheelRadius |> Quantity.half)
                                |> Quantity.plus
                                    (motorbikeStrokeWidth |> Quantity.half)
                                |> Quantity.plus
                                    (drivingPathStrokeWidth |> Quantity.half)
                            )
                    )
                        && (point2dDistanceBetween
                                positionProjectedOntoSegmentAxis
                                drivingPathSegment.start
                                |> Quantity.greaterThanOrEqualTo
                                    (drivingPathSegmentAsVeryRoughApproximateLineSegment
                                        |> LineSegment2d.length
                                        |> Quantity.plus
                                            (wheelRadius |> Quantity.multiplyBy 0.3)
                                    )
                           )
                then
                    [ LineSegment2d.from
                        (Point2d.meters 0 -((drivingPathStrokeWidth |> Length.inMeters) / 2))
                        (Point2d.meters 0 ((drivingPathStrokeWidth |> Length.inMeters) / 2))
                        |> LineSegment2d.rotateAround Point2d.origin
                            (segmentAxis |> Axis2d.direction |> Direction2d.toAngle)
                        |> LineSegment2d.translateBy
                            (drivingPathSegmentAsVeryRoughApproximateLineSegment
                                |> LineSegment2d.endPoint
                                |> point2dToVector
                            )
                    ]

                else if
                    (point2dDistanceBetween
                        wheelGeometry.position
                        drivingPathSegment.start
                        |> Quantity.lessThanOrEqualTo
                            ((wheelRadius |> Quantity.half)
                                |> Quantity.plus
                                    (motorbikeStrokeWidth |> Quantity.half)
                                |> Quantity.plus
                                    (drivingPathStrokeWidth |> Quantity.half)
                            )
                    )
                        && (point2dDistanceBetween
                                positionProjectedOntoSegmentAxis
                                (drivingPathSegmentAsVeryRoughApproximateLineSegment |> LineSegment2d.endPoint)
                                |> Quantity.greaterThanOrEqualTo
                                    (drivingPathSegmentAsVeryRoughApproximateLineSegment
                                        |> LineSegment2d.length
                                        |> Quantity.plus
                                            (wheelRadius |> Quantity.multiplyBy 0.3)
                                    )
                           )
                then
                    [ LineSegment2d.from
                        (Point2d.meters 0 ((drivingPathStrokeWidth |> Length.inMeters) / 2))
                        (Point2d.meters 0 -((drivingPathStrokeWidth |> Length.inMeters) / 2))
                        |> LineSegment2d.rotateAround Point2d.origin
                            (segmentAxis |> Axis2d.direction |> Direction2d.toAngle)
                        |> LineSegment2d.translateBy
                            (drivingPathSegmentAsVeryRoughApproximateLineSegment
                                |> LineSegment2d.startPoint
                                |> point2dToVector
                            )
                    ]

                else
                    drivingPathSegment.approximation
                        |> List.filterMap
                            (\segment ->
                                case
                                    { points = segment, width = drivingPathStrokeWidth }
                                        |> lineSegment2dCollidesWithCircle wheelGeometry
                                of
                                    Just _ ->
                                        Just segment

                                    Nothing ->
                                        Nothing
                            )


point2dDistanceBetween : Point2d units () -> Point2d units () -> Quantity Float units
point2dDistanceBetween a b =
    Vector2d.from a b
        |> Vector2d.length


point2dToVector : Point2d Length.Meters () -> Vector2d Length.Meters ()
point2dToVector point2d =
    point2d |> Point2d.toMeters |> Vector2d.fromMeters


rotationalSpeedAtAngle :
    Direction2d ()
    -> Quantity Float (Quantity.Rate Length.Meters Duration.Seconds)
    -> Vector2d (Quantity.Rate Length.Meters Duration.Seconds) ()
rotationalSpeedAtAngle angle motorbikeRotationalSpeed =
    Vector2d.withLength
        (motorbikeRotationalSpeed
            |> Quantity.for Duration.second
        )
        angle
        |> Vector2d.per Duration.second


{-| Just True means left, Just False means right
-}
lineSegment2dCollidesWithCircle :
    { radius : Length, position : Point2d Length.Meters () }
    -> { points : LineSegment2d Length.Meters (), width : Length }
    -> Maybe Bool
lineSegment2dCollidesWithCircle circle lineSegment =
    case lineSegment.points |> LineSegment2d.direction of
        Nothing ->
            -- invalid line segment
            Nothing

        Just lineSegmentDirection ->
            if
                Point2d.distanceFrom circle.position
                    (lineSegment.points |> LineSegment2d.midpoint)
                    |> Quantity.greaterThan
                        (lineSegment.points
                            |> LineSegment2d.length
                            |> Quantity.half
                            |> Quantity.plus circle.radius
                            |> Quantity.plus
                                lineSegment.width
                            |> Quantity.plus
                                (motorbikeStrokeWidth |> Quantity.half)
                        )
            then
                Nothing

            else
                let
                    signedDistanceIntervalOfLineSegmentToCircleCenter : Length
                    signedDistanceIntervalOfLineSegmentToCircleCenter =
                        lineSegment.points
                            |> LineSegment2d.signedDistanceFrom
                                (Axis2d.through circle.position
                                    lineSegmentDirection
                                )
                            |> -- since the interval is is exactly on value (because parallel)
                               -- we just check any value
                               Quantity.Interval.maxValue
                in
                if
                    signedDistanceIntervalOfLineSegmentToCircleCenter
                        |> Quantity.abs
                        |> Quantity.lessThanOrEqualTo
                            (circle.radius
                                |> Quantity.plus
                                    (lineSegment.width |> Quantity.half)
                                |> Quantity.plus
                                    (motorbikeStrokeWidth |> Quantity.half)
                            )
                then
                    Just
                        ((signedDistanceIntervalOfLineSegmentToCircleCenter
                            |> Length.inMeters
                         )
                            >= 0
                        )

                else
                    Nothing


gravity : Vector2d (Quantity.Rate (Quantity.Rate Length.Meters Duration.Seconds) Duration.Seconds) ()
gravity =
    Vector2d.meters 0 -4
        |> Vector2d.per Duration.second
        |> Vector2d.per Duration.second


belowIsRespawn : Length
belowIsRespawn =
    drivingPath
        |> List.foldl
            (\segment soFar ->
                soFar
                    |> Quantity.min (segment.start |> Point2d.yCoordinate)
                    |> Quantity.min (segment.end |> Point2d.yCoordinate)
            )
            (Length.meters 0)
        |> Quantity.minus (Length.meters 3)


minimumDeathHeight : Length
minimumDeathHeight =
    drivingPath
        |> List.foldl
            (\segment soFar ->
                soFar
                    |> Quantity.max (segment.start |> Point2d.yCoordinate)
                    |> Quantity.max (segment.end |> Point2d.yCoordinate)
            )
            (Length.meters 0)
        |> Quantity.plus (Length.meters 6)


drivingPathFullLength : Length
drivingPathFullLength =
    point2dDistanceBetween
        (case drivingPath |> List.head of
            Nothing ->
                Point2d.meters 0 0

            Just drivingPathFirstArc ->
                drivingPathFirstArc.start
        )
        (drivingPath
            |> List.foldl
                (\drivingPathArc soFar ->
                    if
                        (drivingPathArc.end |> Point2d.xCoordinate)
                            |> Quantity.greaterThan (soFar |> Point2d.xCoordinate)
                    then
                        drivingPathArc.end

                    else
                        soFar
                )
                (Point2d.meters 0 0)
        )
        |> Quantity.plus (Length.meters 1)


motorbikeDeriveBackWheelPosition :
    { center : Point2d Length.Meters ()
    , angle : Angle
    }
    -> Point2d Length.Meters ()
motorbikeDeriveBackWheelPosition motorbikeOrientation =
    motorbikeOrientation.center
        |> Point2d.translateBy
            (Vector2d.meters
                -((playerLengthBackToFrontAxis |> Length.inMeters) / 2)
                0
                |> Vector2d.rotateBy motorbikeOrientation.angle
            )


motorbikeDeriveFrontWheelPosition :
    { center : Point2d Length.Meters ()
    , angle : Angle
    }
    -> Point2d Length.Meters ()
motorbikeDeriveFrontWheelPosition motorbikeOrientation =
    motorbikeOrientation.center
        |> Point2d.translateBy
            (Vector2d.meters
                ((playerLengthBackToFrontAxis |> Length.inMeters) / 2)
                0
                |> Vector2d.rotateBy motorbikeOrientation.angle
            )


arcToRightToLineSegments :
    { start : Point2d Length.Meters ()
    , end : Point2d Length.Meters ()
    , bendPercentage : Float
    , drivingDirection : DrivingDirection
    }
    -> DrivingPathSegment
arcToRightToLineSegments arc =
    { start = arc.start
    , end = arc.end
    , bendPercentage = arc.bendPercentage
    , approximation =
        drivingPathSegmentToArc2d
            { start = arc.start
            , end = arc.end
            , bendPercentage = arc.bendPercentage
            }
            |> Arc2d.approximate (Length.meters 0.003)
            |> Polyline2d.segments
    , drivingDirection = arc.drivingDirection
    }


drivingPathSegmentToArc2d :
    { start : Point2d units ()
    , end : Point2d units ()
    , bendPercentage : Float
    }
    -> Arc2d units ()
drivingPathSegmentToArc2d geometry =
    Arc2d.from
        geometry.start
        geometry.end
        (Angle.turns (geometry.bendPercentage * 0.5))


stateToHtml : State -> Html Event
stateToHtml state =
    Svg.svg
        [ Svg.Attributes.viewBox
            ("0 0 "
                ++ (state.windowSize.width |> String.fromFloat)
                ++ " "
                ++ (state.windowSize.height |> String.fromFloat)
            )
        ]
        (case state.specific of
            StateMenu ->
                menuStateToHtml state.windowSize

            StateGameplay gameplayState ->
                gameplayStateToSvg state.windowSize gameplayState
        )


menuStateToHtml : { width : Float, height : Float } -> List (Svg Event)
menuStateToHtml windowSize =
    [ Svg.rect
        [ Svg.Attributes.width (windowSize.width |> px)
        , Svg.Attributes.height (windowSize.height |> px)
        , Svg.Attributes.fill (Color.rgb 0 0.2 0.2 |> Color.toCssString)
        , Svg.Events.onMouseDown StartButtonPressed
        ]
        []
    , svgTranslated
        { x = windowSize.width / 2
        , y = windowSize.height / 2
        }
        [ let
            windowScale : Float
            windowScale =
                Basics.min
                    windowSize.width
                    windowSize.height
                    * 0.2
          in
          svgScaled { x = windowScale, y = -windowScale }
            [ svgTranslated
                { x = 0, y = 0.7 }
                [ svgScaled { x = 1, y = -1 }
                    [ Svg.text_
                        [ Svg.Attributes.fontSize "0.14"
                        , Svg.Attributes.fill (Color.rgb 1 1 1 |> Color.toCssString)
                        , Svg.Attributes.textAnchor "middle"
                        , Svg.Attributes.pointerEvents "none"
                        ]
                        [ Svg.text "Hint: r to respawn, no need to refresh" ]
                    ]
                ]
            , svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.49"
                    , Svg.Attributes.fill (Color.rgb 1 1 1 |> Color.toCssString)
                    , Svg.Attributes.textAnchor "middle"
                    , Svg.Attributes.pointerEvents "none"
                    , Svg.Attributes.fontWeight "bold"
                    ]
                    [ Svg.text "space/click to start" ]
                ]
            , svgTranslated
                { x = 0
                , y = -1.5
                }
                [ svgScaled { x = 3.25, y = 3.25 }
                    [ motorbikeToSvg
                        { angle = Angle.turns 0
                        , velocity = Vector2d.meters 0 0 |> Vector2d.per Duration.second
                        , playerInputSpeed = Length.meters 0 |> Quantity.per Duration.second
                        , wheelAngle = Angle.turns 0.4
                        }
                    ]
                ]
            , svgTranslated { x = -1.4, y = -1.4 }
                [ svgScaled { x = 1, y = -1 }
                    [ Svg.text_
                        [ Svg.Attributes.fontSize "0.5"
                        , Svg.Attributes.fill (Color.rgba 0.8 0.6 0.5 0.5 |> Color.toCssString)
                        ]
                        [ Svg.text """🌫""" ]
                    ]
                ]
            , svgTranslated { x = 0, y = 2 }
                [ svgScaled { x = 1, y = 1 }
                    [ Svg.text_
                        [ Svg.Attributes.fontSize "0.5"
                        , Svg.Attributes.fill (Color.rgba 0.8 1 0.5 1 |> Color.toCssString)
                        ]
                        [ Svg.text """🪴""" ]
                    ]
                ]
            , svgArc
                (Arc2d.from (Point2d.meters -2 -0.5)
                    (Point2d.meters 2 -0.5)
                    (Angle.turns 0.4)
                )
                [ Svg.Attributes.fill "none"
                , Svg.Attributes.strokeWidth "0.1"
                , Svg.Attributes.stroke (Color.rgb 1 1 1 |> Color.toCssString)
                ]
            , svgArc
                (Arc2d.from (Point2d.meters -2 0.7)
                    (Point2d.meters 2 0.7)
                    (Angle.turns -0.4)
                )
                [ Svg.Attributes.fill "none"
                , Svg.Attributes.strokeWidth "0.1"
                , Svg.Attributes.stroke (Color.rgb 1 1 1 |> Color.toCssString)
                ]
            ]
        ]
    ]


spaceKeyJsonDecoder : Json.Decode.Decoder ()
spaceKeyJsonDecoder =
    Json.Decode.andThen
        (\key ->
            case key of
                " " ->
                    Json.Decode.succeed ()

                _ ->
                    Json.Decode.fail "unknown key, ignore"
        )
        (Json.Decode.field "key" Json.Decode.string)


gameplayStateToSvg : { width : Float, height : Float } -> GameplayState -> List (Svg Event)
gameplayStateToSvg windowSize state =
    let
        levelProgress : Float
        levelProgress =
            (state.motorbikeCenter
                |> Point2d.xCoordinate
                |> Length.inMeters
            )
                / (drivingPathFullLength
                    |> Length.inMeters
                  )
    in
    [ -- not used because slow to render svgDefinitions,
      Svg.rect
        [ Svg.Attributes.width (windowSize.width |> px)
        , Svg.Attributes.height (windowSize.height |> px)
        , Svg.Attributes.fill (Color.rgb (0.3 * levelProgress) (0.2 * (1 - levelProgress)) (0.2 * (1 - levelProgress)) |> Color.toCssString)
        ]
        []
    , let
        windowScale : Float
        windowScale =
            Basics.min
                windowSize.width
                windowSize.height
                * 0.2
      in
      [ motorbikeToSvg
            { velocity = state.motorbikeVelocity
            , angle = state.motorbikeAngle
            , playerInputSpeed = state.playerInputSpeed
            , wheelAngle = state.motorbikeWheelAngle
            }
      , let
            cameraPosition : { x : Float, y : Float }
            cameraPosition =
                state.motorbikeCenter |> Point2d.toMeters
        in
        [ drivingPath
            |> List.map
                (\drivingPathSegment ->
                    let
                        geometry : Arc2d Length.Meters ()
                        geometry =
                            { start = drivingPathSegment.start
                            , end = drivingPathSegment.end
                            , bendPercentage = drivingPathSegment.bendPercentage
                            }
                                |> drivingPathSegmentToArc2d
                    in
                    Svg.path
                        [ Svg.Attributes.d
                            (Svg.PathD.pathD
                                (Svg.PathD.M
                                    (drivingPathSegment.start
                                        |> Point2d.toTuple Length.inMeters
                                    )
                                    :: (geometry |> pathDArc)
                                    ++ [ Svg.PathD.L
                                            (drivingPathSegment.start
                                                |> Point2d.translateBy
                                                    (Vector2d.meters 0 -1000
                                                        |> Vector2d.rotateBy
                                                            (Angle.turns
                                                                (-0.07
                                                                    - 0.2
                                                                    * levelProgress
                                                                )
                                                            )
                                                    )
                                                |> Point2d.toTuple Length.inMeters
                                            )
                                       , Svg.PathD.Z
                                       ]
                                )
                            )
                        , Svg.Attributes.fill
                            (Color.rgba
                                0
                                0
                                0.07
                                (0.2 + 0.2 * levelProgress)
                                |> Color.toCssString
                            )
                        ]
                        []
                )
            |> Svg.g
                []
        , drivingPathSvg
        , textsAndScenerySvg
        ]
            |> svgTranslated
                { x = -cameraPosition.x
                , y = -cameraPosition.y
                }
      ]
        |> svgScaled
            { x = windowScale
            , y = -windowScale
            }
        |> List.singleton
        |> svgTranslated
            { x = windowSize.width / 2
            , y = windowSize.height / 2
            }
    ]


textsAndScenerySvg : Svg event_
textsAndScenerySvg =
    Svg.g
        [ Svg.Attributes.pointerEvents "none"
        ]
        [ svgTranslated { x = 0, y = 0.84 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.2"
                    , Svg.Attributes.fill (Color.rgb 1 1 1 |> Color.toCssString)
                    ]
                    [ Svg.text "arrow keys →/← to" ]
                ]
            ]
        , svgTranslated { x = -1.12, y = 0.36 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.19"
                    , Svg.Attributes.fill (Color.rgba 0 0.25 0 0.7 |> Color.toCssString)
                    ]
                    [ Svg.text """🌷""" ]
                ]
            ]
        , svgTranslated { x = 5.19, y = -2.56 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.1"
                    , Svg.Attributes.fill (Color.rgba 0.8 1 0.5 0.7 |> Color.toCssString)
                    ]
                    [ Svg.text """𓍢ִ໋🌷͙֒""" ]
                ]
            ]
        , svgTranslated { x = 5, y = -5.02 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.101"
                    , Svg.Attributes.fill (Color.rgba 1 1 1 1 |> Color.toCssString)
                    ]
                    [ Svg.text """𓍊𓋼𓍊𓋼𓍊𓍊𓋼𓍊𓋼𓍊""" ]
                ]
            ]
        , svgTranslated { x = 9.92, y = -0.9 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.101"
                    , Svg.Attributes.fill (Color.rgba 1 1 1 1 |> Color.toCssString)
                    ]
                    [ Svg.text """🌺""" ]
                ]
            ]
        , svgTranslated { x = 0, y = -0.1 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.73"
                    , Svg.Attributes.fill (Color.rgba 1 1 1 1 |> Color.toCssString)
                    , Svg.Attributes.style "filter: saturate(20%)"
                    ]
                    [ Svg.text """⛩️""" ]
                ]
            ]
        , svgTranslated { x = 1.05, y = -0.04 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.27"
                    , Svg.Attributes.fill (Color.rgb 1 1 1 |> Color.toCssString)
                    , Svg.Attributes.fontWeight "bold"
                    ]
                    [ Svg.text """🚩""" ]
                ]
            ]
        , svgTranslated { x = 0.4, y = -0.3 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.8"
                    , Svg.Attributes.fill (Color.rgba 0 0.25 0 0.7 |> Color.toCssString)
                    ]
                    [ Svg.text "෴" ]
                ]
            ]
        , svgTranslated { x = 0.47, y = -0.3 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.5"
                    , Svg.Attributes.fill (Color.rgba 0 0.25 0 0.7 |> Color.toCssString)
                    ]
                    [ Svg.text "෴" ]
                ]
            ]
        , svgTranslated { x = 0.1, y = -0.3 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.14"
                    , Svg.Attributes.fill (Color.rgba 0 0.25 0 0.3 |> Color.toCssString)
                    ]
                    [ Svg.text """🌸""" ]
                ]
            ]
        , svgTranslated { x = 1, y = -0.3 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.14"
                    , Svg.Attributes.fill (Color.rgba 0 0.25 0 0.7 |> Color.toCssString)
                    ]
                    [ Svg.text """🌸""" ]
                ]
            ]
        , svgTranslated { x = 0, y = 0.65 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.2"
                    , Svg.Attributes.fill (Color.rgb 1 1 1 |> Color.toCssString)
                    , Svg.Attributes.fontWeight "bold"
                    ]
                    [ Svg.text "accelerate forwards/backwards" ]
                ]
            ]
        , svgTranslated { x = 31, y = 0.5 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.2"
                    , Svg.Attributes.fill (Color.rgb 1 1 1 |> Color.toCssString)
                    , Svg.Attributes.fontWeight "bold"
                    ]
                    [ Svg.text "big jump!" ]
                ]
            ]
        , svgTranslated { x = 31.8, y = -4 }
            [ svgRotated (Angle.turns -0.25)
                [ svgScaled { x = 1, y = -1 }
                    [ Svg.text_
                        [ Svg.Attributes.fontSize "0.2"
                        , Svg.Attributes.fill (Color.rgb 1 1 1 |> Color.toCssString)
                        , Svg.Attributes.fontWeight "bold"
                        ]
                        [ Svg.text "noooooooooooooooooooooooooooooooooooooooooooooooooooooooo!" ]
                    ]
                ]
            ]
        , svgTranslated { x = 40, y = 1 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.2"
                    , Svg.Attributes.fill (Color.rgb 1 1 1 |> Color.toCssString)
                    , Svg.Attributes.fontWeight "bold"
                    ]
                    [ Svg.text "take speed" ]
                ]
            ]
        , svgTranslated { x = 46, y = 4 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.2"
                    , Svg.Attributes.fill (Color.rgba 1 1 1 0.4 |> Color.toCssString)
                    , Svg.Attributes.fontWeight "bold"
                    ]
                    [ Svg.text "☁️      ☁️" ]
                ]
            ]
        , svgTranslated { x = 42, y = 2 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.2"
                    , Svg.Attributes.fill (Color.rgba 1 1 1 0.27 |> Color.toCssString)
                    , Svg.Attributes.fontWeight "bold"
                    ]
                    [ Svg.text "☁️   *   ☁️" ]
                ]
            ]
        , svgTranslated { x = 45.3, y = 3.4 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.2"
                    , Svg.Attributes.fill (Color.rgb 1 1 1 |> Color.toCssString)
                    , Svg.Attributes.fontWeight "bold"
                    ]
                    [ Svg.text "☁️  .    .       ☁️" ]
                ]
            ]
        , List.range 0 11
            |> List.map
                (\i ->
                    svgTranslated { x = 45.26, y = 3.25 - 0.19 * (i |> Basics.toFloat) }
                        [ svgScaled { x = 1, y = -1 }
                            [ Svg.text_
                                [ Svg.Attributes.fontSize "0.2"
                                , Svg.Attributes.fill (Color.rgba 0 0 0 (1 - (i |> Basics.toFloat) / 14) |> Color.toCssString)
                                ]
                                [ Svg.text "⛆ ⛆⛆ ⛆" ]
                            ]
                        ]
                )
            |> Svg.g []
        , svgTranslated { x = 55, y = 2 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.8"
                    , Svg.Attributes.fill (Color.rgba 0 0 0 0.2 |> Color.toCssString)
                    , Svg.Attributes.fontWeight "bold"
                    ]
                    [ Svg.text """🌩""" ]
                ]
            ]
        , svgTranslated { x = 57.4, y = 2.6 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.5"
                    , Svg.Attributes.fill (Color.rgba 0 0 0.2 0.2 |> Color.toCssString)
                    ]
                    [ Svg.text """🌨""" ]
                ]
            ]
        , svgTranslated { x = 56.5, y = 0.2 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.5"
                    , Svg.Attributes.fill (Color.rgba 0 0 0 1 |> Color.toCssString)
                    ]
                    [ Svg.text """𖤣𖥧⚘ ⸙""" ]
                ]
            ]
        , svgTranslated { x = 62, y = 3.7 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.3"
                    , Svg.Attributes.fill (Color.rgba 0 0 0.2 1 |> Color.toCssString)
                    ]
                    [ Svg.text """🪧""" ]
                ]
            ]
        , svgTranslated { x = 61.95, y = 3.76 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.38"
                    , Svg.Attributes.fill (Color.rgba 0 0 0.2 1 |> Color.toCssString)
                    , Svg.Attributes.style "filter: saturate(20%) brightness(40%)"
                    ]
                    [ Svg.text """☣️""" ]
                ]
            ]
        , svgTranslated { x = 71.95, y = 1 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.4"
                    , Svg.Attributes.fill (Color.rgba 0.8 0.8 1 1 |> Color.toCssString)
                    ]
                    [ Svg.text """༄""" ]
                ]
            ]
        , svgTranslated { x = 88, y = 5.3 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.15"
                    , Svg.Attributes.fill (Color.rgb 1 1 1 |> Color.toCssString)
                    ]
                    [ Svg.text "keep forwards" ]
                ]
            ]
        , svgTranslated { x = 100.92, y = 14.04 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.28"
                    , Svg.Attributes.fill (Color.rgb 1 1 1 |> Color.toCssString)
                    ]
                    [ Svg.text """🚧""" ]
                ]
            ]
        , svgTranslated { x = 110.92, y = 6.16 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.8"
                    , Svg.Attributes.fill (Color.rgb 1 1 1 |> Color.toCssString)
                    ]
                    [ Svg.text """🪸""" ]
                ]
            ]
        , svgTranslated { x = 104.92, y = 10.16 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.5"
                    , Svg.Attributes.fill (Color.rgb 1 1 1 |> Color.toCssString)
                    ]
                    [ Svg.text """🐡""" ]
                ]
            ]
        , svgTranslated { x = 119.925, y = 14.07 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.3"
                    , Svg.Attributes.fill (Color.rgb 1 1 1 |> Color.toCssString)
                    ]
                    [ Svg.text """🚩""" ]
                ]
            ]
        , svgTranslated { x = 120, y = 15.2 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.2"
                    , Svg.Attributes.fill (Color.rgb 1 1 1 |> Color.toCssString)
                    , Svg.Attributes.fontWeight "bold"
                    ]
                    [ Svg.text "good job!" ]
                ]
            ]
        , svgTranslated { x = 120, y = 16.2 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.2"
                    , Svg.Attributes.fill (Color.rgb 1 1 1 |> Color.toCssString)
                    , Svg.Attributes.fontWeight "bold"
                    ]
                    [ Svg.text "Have a nice day!" ]
                ]
            ]
        , svgTranslated { x = 122, y = 8 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.2"
                    , Svg.Attributes.fill (Color.rgb 1 1 1 |> Color.toCssString)
                    , Svg.Attributes.fontWeight "bold"
                    ]
                    [ Svg.text """Ი︵𐑼""" ]
                ]
            ]
        , svgTranslated { x = 122.1, y = 7.7 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.2"
                    , Svg.Attributes.fill (Color.rgb 1 1 1 |> Color.toCssString)
                    , Svg.Attributes.fontWeight "bold"
                    ]
                    [ Svg.text "•ᴗ•   ₊˚⊹ᰔ    ꫂ❁" ]
                ]
            ]
        , svgTranslated { x = -8, y = -1 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.2"
                    , Svg.Attributes.fill (Color.rgb 1 1 1 |> Color.toCssString)
                    ]
                    [ Svg.text "nothing here" ]
                ]
            ]
        , svgTranslated { x = -8, y = -2 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.2"
                    , Svg.Attributes.fill (Color.rgb 1 1 1 |> Color.toCssString)
                    ]
                    [ Svg.text "૮꒰˶ᵔ ᵕ ᵔ˶꒱ა" ]
                ]
            ]
        , svgTranslated { x = 101.9, y = 14.04 }
            [ svgScaled { x = 1, y = -1 }
                [ Svg.text_
                    [ Svg.Attributes.fontSize "0.5"
                    , Svg.Attributes.fill (toxicColor |> Color.toCssString)
                    ]
                    [ Svg.text """∘˙○˚.• ･ﾟ ･ﾟ·:｡･ﾟﾟ･ ⋆｡˚ °‧ 𓆝 𓆟 𓆞 ·｡ ⋆𓂃 𓈒𓏸 𓂃 𓈒𓏸 ｡ﾟ☁︎｡⋆｡ ﾟ ﾟ｡⋆⸝⸝⸝⸝⸝⸝⸝⸝⸝⸝⸝⸝⸝⸝⸝""" ]
                ]
            ]
        , svgArc
            (drivingPathSegmentToArc2d
                { start = Point2d.meters 101.2 14
                , end = Point2d.meters 120 14
                , bendPercentage = 0.9
                }
            )
            [ Svg.Attributes.fill (toxicColor |> Color.toCssString)
            ]
        ]


toxicColor : Color
toxicColor =
    Color.rgba 0.3 0.95 0 0.5


drivingPathSvg : Svg Event
drivingPathSvg =
    drivingPath
        |> List.map drivingPathSegmentToSvg
        |> Svg.g []


motorbikeToSvg :
    { angle : Angle
    , velocity : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) ()
    , playerInputSpeed : Quantity Float (Quantity.Rate Length.Meters Duration.Seconds)
    , wheelAngle : Angle
    }
    -> Svg event_
motorbikeToSvg state =
    let
        motorbikeBackPosition : Point2d Length.Meters ()
        motorbikeBackPosition =
            Point2d.meters
                -((playerLengthBackToFrontAxis |> Length.inMeters) / 2)
                0

        motorbikeFrontPosition : Point2d Length.Meters ()
        motorbikeFrontPosition =
            Point2d.meters
                ((playerLengthBackToFrontAxis |> Length.inMeters) / 2)
                0
    in
    svgRotated state.angle
        [ motorbikeWheelToSvg
            { position = motorbikeBackPosition
            , angle = state.wheelAngle
            , playerInputSpeed = state.playerInputSpeed
            }
        , motorbikeWheelToSvg
            { position = motorbikeFrontPosition
            , angle = state.wheelAngle
            , playerInputSpeed = state.playerInputSpeed
            }
        , svgLineSegment
            { lineSegment =
                LineSegment2d.from
                    motorbikeBackPosition
                    motorbikeFrontPosition
            , color = motorbikeColor state.playerInputSpeed
            , width = motorbikeStrokeWidth
            }
            [ Svg.Attributes.strokeLinecap "round"
            ]
        , svgLineSegment
            { lineSegment =
                LineSegment2d.from
                    (Point2d.meters
                        0
                        -((playerLengthBackToFrontAxis |> Length.inMeters) / 8)
                    )
                    (Point2d.meters
                        0
                        ((playerLengthBackToFrontAxis |> Length.inMeters) / 8)
                    )
            , color = motorbikeColor state.playerInputSpeed
            , width = motorbikeStrokeWidth |> Quantity.half
            }
            [ Svg.Attributes.strokeLinecap "round"
            ]
        , svgLineSegment
            { lineSegment =
                LineSegment2d.from
                    (Point2d.meters
                        0
                        -((playerLengthBackToFrontAxis |> Length.inMeters) / 8)
                    )
                    (Point2d.meters
                        ((playerLengthBackToFrontAxis |> Length.inMeters) / 2)
                        0
                    )
            , color = motorbikeColor state.playerInputSpeed
            , width = motorbikeStrokeWidth |> Quantity.half
            }
            [ Svg.Attributes.strokeLinecap "round"
            ]
        , svgLineSegment
            { lineSegment =
                LineSegment2d.from
                    (Point2d.meters
                        0
                        ((playerLengthBackToFrontAxis |> Length.inMeters) / 8)
                    )
                    (Point2d.meters
                        ((playerLengthBackToFrontAxis |> Length.inMeters) / 2)
                        0
                    )
            , color = motorbikeColor state.playerInputSpeed
            , width = motorbikeStrokeWidth |> Quantity.half
            }
            [ Svg.Attributes.strokeLinecap "round"
            ]
        , svgLineSegment
            { lineSegment =
                LineSegment2d.from
                    (Point2d.meters
                        0
                        -((playerLengthBackToFrontAxis |> Length.inMeters) / 8)
                    )
                    (Point2d.meters
                        -((playerLengthBackToFrontAxis |> Length.inMeters) / 2)
                        0
                    )
            , color = motorbikeColor state.playerInputSpeed
            , width = motorbikeStrokeWidth |> Quantity.half
            }
            [ Svg.Attributes.strokeLinecap "round"
            ]
        , svgLineSegment
            { lineSegment =
                LineSegment2d.from
                    (Point2d.meters
                        0
                        ((playerLengthBackToFrontAxis |> Length.inMeters) / 8)
                    )
                    (Point2d.meters
                        -((playerLengthBackToFrontAxis |> Length.inMeters) / 2)
                        0
                    )
            , color = motorbikeColor state.playerInputSpeed
            , width = motorbikeStrokeWidth |> Quantity.half
            }
            [ Svg.Attributes.strokeLinecap "round"
            ]
        ]


motorbikeWheelToSvg :
    { position : Point2d Length.Meters ()
    , angle : Angle
    , playerInputSpeed : Quantity Float (Quantity.Rate Length.Meters Duration.Seconds)
    }
    -> Svg event_
motorbikeWheelToSvg state =
    svgTranslated (state.position |> Point2d.toMeters)
        [ svgRotated state.angle
            [ svgLineSegment
                { lineSegment =
                    LineSegment2d.from
                        (Point2d.meters 0 (wheelRadius |> Length.inMeters))
                        (Point2d.meters 0 -(wheelRadius |> Length.inMeters))
                , color = motorbikeColor state.playerInputSpeed
                , width = motorbikeStrokeWidth |> Quantity.half
                }
                []
            , svgLineSegment
                { lineSegment =
                    LineSegment2d.from
                        (Point2d.meters (wheelRadius |> Length.inMeters) 0)
                        (Point2d.meters -(wheelRadius |> Length.inMeters) 0)
                , color = motorbikeColor state.playerInputSpeed
                , width = motorbikeStrokeWidth |> Quantity.half
                }
                []
            , Svg.circle
                [ Svg.Attributes.cx (0 |> String.fromFloat)
                , Svg.Attributes.cy (0 |> String.fromFloat)
                , Svg.Attributes.r (wheelRadius |> Length.inMeters |> String.fromFloat)
                , Svg.Attributes.strokeWidth
                    (motorbikeStrokeWidth
                        |> Length.inMeters
                        |> String.fromFloat
                    )
                , Svg.Attributes.stroke
                    (motorbikeColor state.playerInputSpeed
                        |> Color.toCssString
                    )
                , Svg.Attributes.fill (colorTransparent |> Color.toCssString)
                ]
                []
            ]
        ]


motorbikeStrokeWidth : Length
motorbikeStrokeWidth =
    wheelRadius
        |> Quantity.multiplyBy 0.25


motorbikeColor : Quantity Float (Quantity.Rate Length.Meters Duration.Seconds) -> Color
motorbikeColor playerInputSpeed =
    let
        signedPercentage =
            (playerInputSpeed
                |> Quantity.for Duration.second
                |> Length.inMeters
            )
                * 20
                |> Basics.clamp -1 1
    in
    Color.rgb
        (0.7 + 0.3 * signedPercentage)
        0.6
        (Basics.max 0 (-0.4 * signedPercentage))


playerLengthBackToFrontAxis : Length
playerLengthBackToFrontAxis =
    Length.meters 0.3


wheelRadius : Length
wheelRadius =
    playerLengthBackToFrontAxis |> Quantity.multiplyBy 0.3


drivingPathSegmentToSvg : DrivingPathSegment -> Svg Event
drivingPathSegmentToSvg drivingPathSegment =
    Svg.g []
        [ svgArc
            ({ start = drivingPathSegment.start
             , end = drivingPathSegment.end
             , bendPercentage = drivingPathSegment.bendPercentage
             }
                |> drivingPathSegmentToArc2d
            )
            [ Svg.Attributes.strokeWidth (drivingPathStrokeWidth |> Quantity.multiplyBy 0.5 |> Length.inMeters |> String.fromFloat)
            , Svg.Attributes.stroke (Color.rgba 0.75 0.95 1 1 |> Color.toCssString)
            , Svg.Attributes.fill "none"

            -- , Svg.Attributes.filter "url(#glow)"
            ]
        ]


drivingPathStrokeWidth : Length
drivingPathStrokeWidth =
    Length.meters 0.1


svgLineSegment :
    { lineSegment : LineSegment2d Length.Meters coordinates_
    , width : Length
    , color : Color
    }
    -> List (Svg.Attribute event)
    -> Svg event
svgLineSegment config modifiers =
    let
        start : { x : Float, y : Float }
        start =
            config.lineSegment |> LineSegment2d.startPoint |> Point2d.toMeters

        end : { x : Float, y : Float }
        end =
            config.lineSegment |> LineSegment2d.endPoint |> Point2d.toMeters
    in
    Svg.line
        ([ Svg.Attributes.x1 (start.x |> String.fromFloat)
         , Svg.Attributes.y1 (start.y |> String.fromFloat)
         , Svg.Attributes.x2 (end.x |> String.fromFloat)
         , Svg.Attributes.y2 (end.y |> String.fromFloat)
         , Svg.Attributes.strokeWidth (config.width |> Length.inMeters |> String.fromFloat)
         , Svg.Attributes.stroke (config.color |> Color.toCssString)
         ]
            ++ modifiers
        )
        []


{-| Better but more complicated alternative: only arcs (start, end, bend (left negative, right positive))
possibly checking by subdividing arc into segments
-}
type alias DrivingPathSegment =
    { start : Point2d Length.Meters ()
    , end : Point2d Length.Meters ()
    , bendPercentage : Float
    , approximation :
        List (LineSegment2d Length.Meters ())
    , drivingDirection : DrivingDirection
    }


type DrivingDirection
    = Backwards
    | Forwards


drivingPath : List DrivingPathSegment
drivingPath =
    [ { start = Point2d.meters -1 0.3
      , end = Point2d.meters 1 -0.2
      , bendPercentage = 0.3
      , drivingDirection = Forwards
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 1.2 -0.3
      , end = Point2d.meters 2 -2
      , bendPercentage = 0.2
      , drivingDirection = Forwards
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 2 -3
      , end = Point2d.meters 4.8 -3
      , bendPercentage = 0.78
      , drivingDirection = Forwards
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 5 -5
      , end = Point2d.meters 6.8 -5.4
      , bendPercentage = -0.1
      , drivingDirection = Forwards
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 5 -2.5
      , end = Point2d.meters 6.8 -2.9
      , bendPercentage = 0.1
      , drivingDirection = Forwards
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 7 -3.4
      , end = Point2d.meters 10 -0.8
      , bendPercentage = 0.3
      , drivingDirection = Forwards
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 10.5 -1
      , end = Point2d.meters 13 -3.4
      , bendPercentage = 0.4
      , drivingDirection = Forwards
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 13 -3.4
      , end = Point2d.meters 16 -1
      , bendPercentage = 0.44
      , drivingDirection = Forwards
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 16.5 -1
      , end = Point2d.meters 18 -3.4
      , bendPercentage = -0.3
      , drivingDirection = Forwards
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 18 -3.4
      , end = Point2d.meters 21 -1.5
      , bendPercentage = 0.4
      , drivingDirection = Forwards
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 21.3 -1.5
      , end = Point2d.meters 25 -1
      , bendPercentage = -0.61
      , drivingDirection = Forwards
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 25 -1
      , end = Point2d.meters 30 -1
      , bendPercentage = 0.61
      , drivingDirection = Forwards
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 33.55 -0.75
      , end = Point2d.meters 40 -1
      , bendPercentage = 0.8
      , drivingDirection = Forwards
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 40 -1.2
      , end = Point2d.meters 48 3.4
      , bendPercentage = 0.3
      , drivingDirection = Forwards
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 52 2
      , end = Point2d.meters 61 2
      , bendPercentage = 0.5
      , drivingDirection = Forwards
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 64 3
      , end = Point2d.meters 61 3
      , bendPercentage = 0.5
      , drivingDirection = Backwards
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 63.9 2.2
      , end = Point2d.meters 68 1
      , bendPercentage = 0.5
      , drivingDirection = Forwards
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 72.2 1.1
      , end = Point2d.meters 67.9 1.6
      , bendPercentage = 0.4
      , drivingDirection = Backwards
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 72 0.2
      , end = Point2d.meters 80 1
      , bendPercentage = 0.75
      , drivingDirection = Forwards
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 81.42 2.9
      , end = Point2d.meters 81.4 4.5
      , drivingDirection = Forwards
      , bendPercentage = 0.5
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 75.6 5
      , end = Point2d.meters 78.401 1.5
      , bendPercentage = 0.8
      , drivingDirection = Forwards
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 82.8 0
      , end = Point2d.meters 88.8 2
      , drivingDirection = Forwards
      , bendPercentage = 0.4
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 90 3
      , end = Point2d.meters 85 4.8
      , drivingDirection = Forwards
      , bendPercentage = 0.9
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 82 4.2
      , end = Point2d.meters 85.8 3
      , drivingDirection = Backwards
      , bendPercentage = 0.33
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 84 7
      , end = Point2d.meters 84.01 3
      , drivingDirection = Backwards
      , bendPercentage = 0.99
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 84 6.2
      , end = Point2d.meters 90 6.2
      , drivingDirection = Forwards
      , bendPercentage = -0.1
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 90 6.2
      , end = Point2d.meters 95 8
      , drivingDirection = Forwards
      , bendPercentage = 0.17
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 95 8
      , end = Point2d.meters 98 10
      , drivingDirection = Forwards
      , bendPercentage = 0.05
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 98 10
      , end = Point2d.meters 100 12
      , drivingDirection = Forwards
      , bendPercentage = 0.03
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 100 12
      , end = Point2d.meters 101 14
      , drivingDirection = Forwards
      , bendPercentage = 0.02
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 101.2 14
      , end = Point2d.meters 120 14
      , drivingDirection = Forwards
      , bendPercentage = 0.9
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 118.5 10.3
      , end = Point2d.meters 118 14
      , drivingDirection = Backwards
      , bendPercentage = -0.8
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 101.8 14.3
      , end = Point2d.meters 107 14.3
      , drivingDirection = Forwards
      , bendPercentage = 0.06
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 109.2 14.1
      , end = Point2d.meters 113 14.3
      , drivingDirection = Forwards
      , bendPercentage = -0.06
      }
        |> arcToRightToLineSegments
    , { start = Point2d.meters 114.9 14.2
      , end = Point2d.meters 117.5 15
      , drivingDirection = Forwards
      , bendPercentage = 0.1
      }
        |> arcToRightToLineSegments
    ]


vector2dClampToMaxLength : Quantity Float units -> Vector2d units () -> Vector2d units ()
vector2dClampToMaxLength lengthMaximum vector2d =
    if vector2d |> Vector2d.length |> Quantity.greaterThan lengthMaximum then
        Vector2d.withLength lengthMaximum
            (vector2d
                |> Vector2d.direction
                |> Maybe.withDefault Direction2d.positiveY
            )

    else
        vector2d


quantityClampAbsToAtMost :
    Quantity Float units
    -> Quantity Float units
    -> Quantity Float units
quantityClampAbsToAtMost maxAbsValue quantity =
    quantity
        |> Quantity.clamp
            (maxAbsValue |> Quantity.negate)
            maxAbsValue


quantityClampAbsToAtLeast :
    Quantity Float units
    -> Quantity Float units
    -> Quantity Float units
quantityClampAbsToAtLeast maxAbsValue quantity =
    if quantity |> Quantity.lessThanZero then
        quantity
            |> Quantity.min
                (maxAbsValue |> Quantity.negate)

    else
        quantity
            |> Quantity.max maxAbsValue


colorTransparent : Color
colorTransparent =
    Color.rgba 0 0 0 0


px : Float -> String
px value =
    (value |> String.fromFloat) ++ "px"


svgTranslated : { x : Float, y : Float } -> List (Svg event) -> Svg event
svgTranslated offset elements =
    Svg.g
        [ Svg.Attributes.transform
            ([ "translate("
             , offset.x |> String.fromFloat
             , ", "
             , offset.y |> String.fromFloat
             , ")"
             ]
                |> String.concat
            )
        ]
        elements


svgScaled : { x : Float, y : Float } -> List (Svg event) -> Svg event
svgScaled scale elements =
    Svg.g
        [ Svg.Attributes.transform
            ([ "scale("
             , scale.x |> String.fromFloat
             , ", "
             , scale.y |> String.fromFloat
             , ")"
             ]
                |> String.concat
            )
        ]
        elements


svgRotated : Angle -> List (Svg event) -> Svg event
svgRotated angle elements =
    Svg.g
        [ Svg.Attributes.transform
            ([ "rotate("
             , angle |> Angle.normalize |> Angle.inDegrees |> String.fromFloat
             , ")"
             ]
                |> String.concat
            )
        ]
        elements


svgArc : Arc2d Length.Meters () -> List (Svg.Attribute event) -> Svg event
svgArc geometry modifiers =
    Svg.path
        (Svg.Attributes.d
            (Svg.PathD.pathD
                (Svg.PathD.M
                    (geometry
                        |> Arc2d.startPoint
                        |> Point2d.toTuple Length.inMeters
                    )
                    :: (geometry |> pathDArc)
                )
            )
            :: modifiers
        )
        []


pathDArc : Arc2d.Arc2d Length.Meters coordinates_ -> List Svg.PathD.Segment
pathDArc arcGeometry =
    let
        maxSegmentAngle : Angle
        maxSegmentAngle =
            Angle.turns (1 / 3)

        numSegments : Int
        numSegments =
            1 + floor (abs (Quantity.ratio (arcGeometry |> Arc2d.sweptAngle) maxSegmentAngle))
    in
    Parameter1d.trailing numSegments
        (\parameterValue ->
            Svg.PathD.A
                ( Arc2d.radius arcGeometry |> Length.inMeters
                , Arc2d.radius arcGeometry |> Length.inMeters
                )
                0
                False
                (arcGeometry |> Arc2d.sweptAngle |> Quantity.greaterThanOrEqualTo Quantity.zero)
                (Arc2d.pointOn arcGeometry parameterValue |> Point2d.toTuple Length.inMeters)
        )


{-| Prefer pattern matching when possible
-}
listIsFilled : List a_ -> Bool
listIsFilled list =
    case list of
        _ :: _ ->
            True

        [] ->
            False
