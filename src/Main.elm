module Main exposing (main)

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
import Html
import Length exposing (Length)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polyline2d
import Quantity exposing (Quantity)
import Quantity.Interval
import Svg exposing (Svg)
import Svg.Attributes
import Task
import Time
import Vector2d exposing (Vector2d)


type alias State =
    { windowSize : { height : Float, width : Float }
    , lastSimulationTime : Maybe Time.Posix
    , startTime : Maybe Time.Posix
    , motorbikeCenter : Point2d Length.Meters ()
    , motorbikeAngle : Angle
    , motorbikeVelocity : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) ()
    , motorbikeRotationalSpeed : Quantity Float (Quantity.Rate Length.Meters Duration.Seconds)
    }


deriveMotorbikePosition :
    { back : Point2d Length.Meters ()
    , front : Point2d Length.Meters ()
    }
    -> Point2d Length.Meters ()
deriveMotorbikePosition sides =
    LineSegment2d.from sides.back sides.front
        |> LineSegment2d.midpoint


deriveMotorbikeVelocity :
    { back : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) ()
    , front : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) ()
    }
    -> Vector2d (Quantity.Rate Length.Meters Duration.Seconds) ()
deriveMotorbikeVelocity sides =
    sides.back
        |> Vector2d.plus sides.front
        |> Vector2d.half


type Event
    = WindowSized { width : Float, height : Float }
    | StartTimeReceived Time.Posix
    | SimulationTick Time.Posix


main : Program () State Event
main =
    Browser.document
        { init =
            \() ->
                ( initialState
                , [ Browser.Dom.getViewport
                        |> Task.perform
                            (\viewport ->
                                WindowSized
                                    { width = viewport.viewport.width
                                    , height = viewport.viewport.height
                                    }
                            )
                  , Time.now
                        |> Task.perform
                            StartTimeReceived
                  ]
                    |> Cmd.batch
                )
        , view = stateToDocument
        , update = reactToEvent
        , subscriptions = stateToSubscriptions
        }


initialState : State
initialState =
    { windowSize = { width = 1920, height = 1080 }
    , lastSimulationTime = Nothing
    , startTime = Nothing
    , motorbikeCenter =
        Point2d.meters
            (-0.1 + (playerLengthBackToFrontAxis |> Length.inMeters) / 2)
            0.5
    , motorbikeVelocity =
        Vector2d.meters 0 0
            |> Vector2d.per Duration.second
    , motorbikeAngle = Angle.turns 0
    , motorbikeRotationalSpeed =
        Length.meters 0.02
            |> Quantity.per Duration.second
    }


stateToSubscriptions : State -> Sub Event
stateToSubscriptions _ =
    [ Browser.Events.onResize
        (\width height ->
            WindowSized
                { width = width |> Basics.toFloat
                , height = height |> Basics.toFloat
                }
        )
    , Time.every (1000 / 60)
        SimulationTick
    ]
        |> Sub.batch


reactToEvent : Event -> State -> ( State, Cmd Event )
reactToEvent event state =
    case event of
        WindowSized newSize ->
            ( { state | windowSize = newSize }
            , Cmd.none
            )

        StartTimeReceived startTime ->
            ( { state | startTime = Just startTime }
            , Cmd.none
            )

        SimulationTick currentTime ->
            case state.lastSimulationTime of
                Nothing ->
                    ( { state
                        | lastSimulationTime = Just currentTime
                      }
                    , Cmd.none
                    )

                Just lastSimulationTime ->
                    let
                        durationSinceLastTick : Duration
                        durationSinceLastTick =
                            Duration.from lastSimulationTime
                                currentTime
                    in
                    if
                        ((state.motorbikeCenter |> Point2d.yCoordinate)
                            |> Quantity.lessThanOrEqualTo maximumDeathHeight
                        )
                            || ((state.motorbikeCenter |> Point2d.yCoordinate)
                                    |> Quantity.greaterThanOrEqualTo minimumDeathHeight
                               )
                    then
                        ( { initialState
                            | windowSize = state.windowSize

                            -- re-add score, music etc
                          }
                        , Cmd.none
                        )

                    else
                        let
                            motorbikeBackWheelPosition : Point2d Length.Meters ()
                            motorbikeBackWheelPosition =
                                motorbikeDeriveBackWheelPosition
                                    { center = state.motorbikeCenter
                                    , angle = state.motorbikeAngle
                                    }

                            motorbikeFrontWheelPosition : Point2d Length.Meters ()
                            motorbikeFrontWheelPosition =
                                motorbikeDeriveFrontWheelPosition
                                    { center = state.motorbikeCenter
                                    , angle = state.motorbikeAngle
                                    }

                            backWheelForce : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) ()
                            backWheelForce =
                                wheelCombinedCollisionForce
                                    { wheelPosition = motorbikeBackWheelPosition
                                    , wheelRotateDirection =
                                        state.motorbikeAngle
                                            |> Direction2d.fromAngle
                                            |> Direction2d.rotateClockwise
                                    , motorbikeVelocity = state.motorbikeVelocity
                                    , motorbikeRotationalSpeed = state.motorbikeRotationalSpeed
                                    }

                            frontWheelForce : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) ()
                            frontWheelForce =
                                wheelCombinedCollisionForce
                                    { wheelPosition = motorbikeFrontWheelPosition
                                    , wheelRotateDirection =
                                        state.motorbikeAngle
                                            |> Direction2d.fromAngle
                                            |> Direction2d.rotateCounterclockwise
                                    , motorbikeVelocity = state.motorbikeVelocity
                                    , motorbikeRotationalSpeed = state.motorbikeRotationalSpeed
                                    }

                            _ =
                                case
                                    wheelCollisionsWithDrivingPath motorbikeFrontWheelPosition
                                        |> List.length
                                of
                                    0 ->
                                        0

                                    n ->
                                        Debug.log "front wheel collide" n

                            _ =
                                case
                                    wheelCollisionsWithDrivingPath motorbikeBackWheelPosition
                                        |> List.length
                                of
                                    0 ->
                                        0

                                    n ->
                                        Debug.log "back wheel collide" n

                            combinedNonRotationalForceFromWheelsToApply : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) ()
                            combinedNonRotationalForceFromWheelsToApply =
                                backWheelForce
                                    |> Vector2d.plus frontWheelForce
                                    |> Vector2d.plus
                                        (gravity
                                            |> Vector2d.for durationSinceLastTick
                                        )

                            combinedNonRotationalForceToApply : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) ()
                            combinedNonRotationalForceToApply =
                                combinedNonRotationalForceFromWheelsToApply
                                    |> Vector2d.plus
                                        (gravity
                                            |> Vector2d.for durationSinceLastTick
                                        )

                            combinedRotationalForceToApply : Quantity Float (Quantity.Rate Length.Meters Duration.Seconds)
                            combinedRotationalForceToApply =
                                Vector2d.cross
                                    (Vector2d.from motorbikeBackWheelPosition state.motorbikeCenter)
                                    backWheelForce
                                    |> Quantity.plus
                                        (Vector2d.cross
                                            (Vector2d.from motorbikeFrontWheelPosition state.motorbikeCenter)
                                            frontWheelForce
                                        )
                                    |> Quantity.over_ Length.meter

                            newMotorbikeVelocity : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) ()
                            newMotorbikeVelocity =
                                state.motorbikeVelocity
                                    |> Vector2d.plus
                                        combinedNonRotationalForceToApply
                                    |> Vector2d.scaleBy 0.986
                                    |> vector2dClampToMaxLength
                                        (Length.meters 2
                                            |> Quantity.per Duration.second
                                        )

                            newMotorbikeRotationalSpeed : Quantity Float (Quantity.Rate Length.Meters Duration.Seconds)
                            newMotorbikeRotationalSpeed =
                                state.motorbikeRotationalSpeed
                                    |> Quantity.plus
                                        combinedRotationalForceToApply
                                    |> Quantity.clamp
                                        (Length.meters -1.5
                                            |> Quantity.per Duration.second
                                        )
                                        (Length.meters 1.5
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

                            newMotorbikeAngle : Angle
                            newMotorbikeAngle =
                                state.motorbikeAngle
                                    |> Quantity.plus newMotorbikeRotationToApply
                                    |> Angle.normalize

                            motorcycleCenterWouldCollide :
                                { center : Point2d Length.Meters ()
                                , angle : Angle
                                }
                                -> Bool
                            motorcycleCenterWouldCollide newOrientation =
                                (motorbikeDeriveBackWheelPosition newOrientation
                                    |> wheelCollisionsWithDrivingPath
                                    |> listIsFilled
                                )
                                    || (motorbikeDeriveFrontWheelPosition newOrientation
                                            |> wheelCollisionsWithDrivingPath
                                            |> listIsFilled
                                       )
                        in
                        ( { state
                            | lastSimulationTime = Just currentTime
                            , motorbikeVelocity = newMotorbikeVelocity
                            , motorbikeRotationalSpeed = newMotorbikeRotationalSpeed
                            , motorbikeCenter =
                                if
                                    motorcycleCenterWouldCollide
                                        { center = state.motorbikeCenter
                                        , angle = state.motorbikeAngle
                                        }
                                then
                                    state.motorbikeCenter
                                        |> whileItIs
                                            (\translatedCenter ->
                                                motorcycleCenterWouldCollide
                                                    { center = translatedCenter
                                                    , angle = newMotorbikeAngle
                                                    }
                                            )
                                            (Point2d.translateBy
                                                -- if we just apply newMotorbikeVelocity
                                                -- it might point in the wrong direction
                                                -- same with combinedNonRotationalForceToApply
                                                -- where the gravity might overpower the collision force.
                                                (combinedNonRotationalForceFromWheelsToApply
                                                    |> Vector2d.for durationSinceLastTick
                                                )
                                            )

                                else
                                    state.motorbikeCenter
                                        |> Point2d.translateBy
                                            (newMotorbikeVelocity
                                                |> Vector2d.for durationSinceLastTick
                                            )
                            , motorbikeAngle = newMotorbikeAngle
                          }
                        , Cmd.none
                        )


wheelCombinedCollisionForce :
    { wheelPosition : Point2d Length.Meters ()
    , motorbikeVelocity : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) ()
    , motorbikeRotationalSpeed : Quantity Float (Quantity.Rate Length.Meters Duration.Seconds)
    , wheelRotateDirection : Direction2d ()
    }
    -> Vector2d (Quantity.Rate Length.Meters Duration.Seconds) ()
wheelCombinedCollisionForce stateBeforeCollision =
    wheelCollisionsWithDrivingPath stateBeforeCollision.wheelPosition
        |> List.foldl
            (\intersectingLineSegment forceSoFar ->
                forceSoFar
                    |> Vector2d.plus
                        (stateBeforeCollision.motorbikeVelocity
                            |> Vector2d.scaleBy 2
                            |> Vector2d.plus
                                (stateBeforeCollision.motorbikeRotationalSpeed
                                    |> rotationalSpeedAtAngle
                                        stateBeforeCollision.wheelRotateDirection
                                )
                            |> Vector2d.mirrorAcross
                                (intersectingLineSegment.segment
                                    |> LineSegment2d.axis
                                    |> Maybe.withDefault Axis2d.x
                                )
                        )
            )
            Vector2d.zero


wheelCollisionsWithDrivingPath :
    Point2d Length.Meters ()
    -> List { segment : LineSegment2d Length.Meters (), isLeft : Bool }
wheelCollisionsWithDrivingPath position =
    let
        wheelGeometry : { radius : Length, position : Point2d Length.Meters () }
        wheelGeometry =
            { radius = wheelRadius
            , position = position
            }
    in
    drivingPathSegments
        |> List.filterMap
            (\segment ->
                case
                    { points = segment, width = drivingPathStrokeWidth }
                        |> lineSegment2dCollidesWithCircle wheelGeometry
                of
                    Just isLeft ->
                        Just { segment = segment, isLeft = isLeft }

                    Nothing ->
                        Nothing
            )


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
            Debug.todo "huh??"

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
    Vector2d.meters 0 -1
        |> Vector2d.per Duration.second
        |> Vector2d.per Duration.second


maximumDeathHeight : Length
maximumDeathHeight =
    drivingPathSegments
        |> List.foldl
            (\segment soFar ->
                soFar
                    |> Quantity.min (segment |> LineSegment2d.startPoint |> Point2d.yCoordinate)
                    |> Quantity.min (segment |> LineSegment2d.endPoint |> Point2d.yCoordinate)
            )
            (Length.meters 0)


minimumDeathHeight : Length
minimumDeathHeight =
    drivingPathSegments
        |> List.foldl
            (\segment soFar ->
                soFar
                    |> Quantity.max (segment |> LineSegment2d.startPoint |> Point2d.yCoordinate)
                    |> Quantity.max (segment |> LineSegment2d.endPoint |> Point2d.yCoordinate)
            )
            (Length.meters 0)
        |> Quantity.plus (Length.meters 6)


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


arcToLineSegments :
    { start : Point2d Length.Meters ()
    , end : Point2d Length.Meters ()
    , bendPercentage : Float
    }
    -> List (LineSegment2d Length.Meters ())
arcToLineSegments arc =
    Arc2d.from arc.start arc.end (Angle.turns (arc.bendPercentage * 0.5))
        |> Arc2d.approximate (Length.meters 0.02)
        |> Polyline2d.segments


stateToDocument : State -> Browser.Document Event
stateToDocument state =
    { title = "sloope"
    , body =
        [ Svg.svg
            [ Svg.Attributes.viewBox
                ("0 0 "
                    ++ (state.windowSize.width |> String.fromFloat)
                    ++ " "
                    ++ (state.windowSize.height |> String.fromFloat)
                )
            ]
            [ Svg.rect
                [ Svg.Attributes.width (state.windowSize.width |> px)
                , Svg.Attributes.height (state.windowSize.height |> px)
                , Svg.Attributes.fill (Color.rgb 0.2 0 0.08 |> Color.toCssString)
                ]
                []
            , let
                windowScale : Float
                windowScale =
                    Basics.min
                        state.windowSize.width
                        state.windowSize.height
                        * 0.3
              in
              [ let
                    cameraPosition : { x : Float, y : Float }
                    cameraPosition =
                        state.motorbikeCenter |> Point2d.toMeters
                in
                drivingPathSegments
                    |> List.map drivingPathSegmentToSvg
                    |> svgTranslated
                        { x = -cameraPosition.x
                        , y = -cameraPosition.y
                        }
              , motorbikeToSvg
                    { velocity = state.motorbikeVelocity
                    , angle = state.motorbikeAngle
                    }
              ]
                |> svgScaled
                    { x = windowScale
                    , y = -windowScale
                    }
                |> List.singleton
                |> svgTranslated
                    { x = state.windowSize.width / 2
                    , y = state.windowSize.height / 2
                    }
            ]
        ]
    }


motorbikeToSvg :
    { angle : Angle
    , velocity : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) ()
    }
    -> Svg event
motorbikeToSvg state =
    let
        motorbikeBackPosition : Point2d Length.Meters ()
        motorbikeBackPosition =
            Point2d.origin
                |> Point2d.translateBy
                    (Vector2d.meters
                        -((playerLengthBackToFrontAxis |> Length.inMeters) / 2)
                        0
                        |> Vector2d.rotateBy state.angle
                    )

        motorbikeFrontPosition : Point2d Length.Meters ()
        motorbikeFrontPosition =
            Point2d.origin
                |> Point2d.translateBy
                    (Vector2d.meters
                        ((playerLengthBackToFrontAxis |> Length.inMeters) / 2)
                        0
                        |> Vector2d.rotateBy state.angle
                    )
    in
    Svg.g []
        [ motorbikeWheelToSvg
            { position = motorbikeBackPosition }
        , svgLineSegment
            { lineSegment =
                LineSegment2d.from
                    motorbikeBackPosition
                    motorbikeFrontPosition
            , color = motorbikeColor
            , width = motorbikeStrokeWidth
            }
            [ Svg.Attributes.strokeLinecap "round"
            ]
        , motorbikeWheelToSvg
            { position = motorbikeFrontPosition }
        ]


motorbikeWheelToSvg :
    { position : Point2d Length.Meters () }
    -> Svg event
motorbikeWheelToSvg state =
    let
        position : { x : Float, y : Float }
        position =
            state.position |> Point2d.toMeters
    in
    svgTranslated (state.position |> Point2d.toMeters)
        [ svgLineSegment
            { lineSegment =
                LineSegment2d.from
                    (Point2d.meters 0 (wheelRadius |> Length.inMeters))
                    (Point2d.meters 0 -(wheelRadius |> Length.inMeters))
            , color = motorbikeColor
            , width = motorbikeStrokeWidth
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
            , Svg.Attributes.stroke (motorbikeColor |> Color.toCssString)
            , Svg.Attributes.fill (colorTransparent |> Color.toCssString)
            ]
            []
        ]


motorbikeStrokeWidth : Length
motorbikeStrokeWidth =
    wheelRadius
        |> Quantity.multiplyBy 0.25


motorbikeColor : Color
motorbikeColor =
    Color.rgb 0.8 0.6 0


playerLengthBackToFrontAxis : Length
playerLengthBackToFrontAxis =
    Length.meters 0.3


wheelRadius : Length
wheelRadius =
    playerLengthBackToFrontAxis |> Quantity.multiplyBy 0.3


drivingPathSegmentToSvg : DrivingPathSegment -> Svg Event
drivingPathSegmentToSvg drivingPathSegment =
    svgLineSegment
        { lineSegment = drivingPathSegment
        , color = Color.rgb 0.75 0.95 1
        , width = drivingPathStrokeWidth
        }
        [ Svg.Attributes.strokeLinecap "round"
        ]


drivingPathStrokeWidth : Length
drivingPathStrokeWidth =
    Length.meters 0.05


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
    LineSegment2d Length.Meters ()


drivingPathSegments : List DrivingPathSegment
drivingPathSegments =
    [ { start = Point2d.meters -1 0.3
      , end = Point2d.meters 1 -0.2
      , bendPercentage = 0.3
      }
        |> arcToLineSegments
    , { start = Point2d.meters 1.2 -0.3
      , end = Point2d.meters 2 -2
      , bendPercentage = 0.2
      }
        |> arcToLineSegments
    , { start = Point2d.meters 2 -3
      , end = Point2d.meters 4.8 -3
      , bendPercentage = 0.9
      }
        |> arcToLineSegments
    ]
        |> List.concat


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


listMapAndFirstJust : (a -> Maybe b) -> List a -> Maybe b
listMapAndFirstJust elementToFound list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            case head |> elementToFound of
                Just found ->
                    Just found

                Nothing ->
                    listMapAndFirstJust elementToFound tail


whileItIs : (c -> Bool) -> (c -> c) -> c -> c
whileItIs keepChanging change initial =
    if keepChanging initial then
        whileItIs keepChanging change (change initial)

    else
        initial


{-| Prefer pattern matching when possible
-}
listIsFilled : List a -> Bool
listIsFilled list =
    case list of
        _ :: _ ->
            True

        [] ->
            False
