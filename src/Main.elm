module Main exposing (main)

import Audio
import Browser
import Browser.Dom
import Browser.Events
import Bytes
import Bytes.Parser
import Color exposing (Color)
import Direction2d exposing (Direction2d)
import Duration exposing (Duration)
import Element as Ui
import Element.Background as UiBackground
import Element.Border as UiBorder
import Element.Font as UiFont
import Element.Input as UiInput
import File
import File.Select
import Frame2d exposing (Frame2d)
import Html exposing (Html)
import Json.Decode
import Key
import LineSegment2d exposing (LineSegment2d)
import Midi
import Physics
import Pixels exposing (Pixels)
import PkgPorts
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Reaction exposing (Reaction)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Task
import Time
import TypedSvg as Svg
import TypedSvg.Attributes as SvgA
import TypedSvg.Core exposing (Svg)
import TypedSvg.Filters
import TypedSvg.Filters.Attributes
import TypedSvg.Types as Svg
import Vector2d exposing (Vector2d)


type SpecificOrShared specific shared
    = Specific specific
    | Shared shared


type alias SpecificAndShared specific shared =
    RecordWithoutConstructorFunction
        { shared : shared
        , specific : specific
        }


type MenuOrGame menu game
    = Menu menu
    | Game game


type alias Event =
    SpecificOrShared EventSpecific EventShared


type EventShared
    = AudioLoaded { piece : AudioPiece, result : Result Audio.LoadError Audio.Source }


type alias EventSpecific =
    MenuOrGame MenuEvent GameEvent


type MenuEvent
    = GameStartClicked


type GameEvent
    = GameWindowSized { width : Float, height : Float }
    | InitialTimeReceived Time.Posix
    | FrameTickPassed Time.Posix
    | KeyPressed Key.Key
    | KeyReleased Key.Key


type alias State =
    SpecificAndShared StateSpecific StateShared


type alias StateShared =
    RecordWithoutConstructorFunction
        { audioCollideStone : Result Audio.LoadError Audio.Source }


type alias StateSpecific =
    MenuOrGame MenuState GameState


type alias MenuState =
    RecordWithoutConstructorFunction
        {}


type alias EditorState =
    RecordWithoutConstructorFunction
        { windowSize : { width : Float, height : Float }
        , world : Physics.World BodyWhat
        , camera : Frame2d Pixels Float { defines : Float }
        }


type alias GameState =
    RecordWithoutConstructorFunction
        { windowSize : { width : Float, height : Float }
        , collideStoneAudioTimes : List Time.Posix
        , world : Physics.World BodyWhat
        , camera : Frame2d Pixels Float { defines : Float }
        , runStartTime : Time.Posix
        , lastSimulationTime : Time.Posix
        , keysPressed : List Key.Key
        }


type BodyWhat
    = Player PlayerWhat
    | Obstacle ObstacleWhat


type alias ObstacleWhat =
    {}


type alias PlayerWhat =
    RecordWithoutConstructorFunction
        { locationTrail : List (Point2d Pixels Float) }


type alias Effect =
    SpecificOrShared EffectSpecific EffectShared


type alias EffectSpecific =
    MenuOrGame MenuEffect GameEffect


type EffectShared
    = LoadAudio AudioPiece


type MenuEffect
    = MenuEffectNoneYet Never


type GameEffect
    = RequestInitialTime
    | GameRequestInitialWindowSize


type AudioPiece
    = AudioCollideStone


main : Program () (Audio.Model Event State) (Audio.Msg Event)
main =
    Audio.documentWithAudio
        { init =
            init >> Reaction.toTuple3 interpretEffect
        , update =
            \_ event ->
                reactTo event >> Reaction.toTuple3 interpretEffect
        , subscriptions =
            \_ -> subscriptions
        , view =
            \_ -> uiDocument
        , audio = \_ -> audio
        , audioPort =
            { toJS = PkgPorts.ports.audioPortToJS
            , fromJS = PkgPorts.ports.audioPortFromJS
            }
        }


obstacles : List (Physics.Body BodyWhat)
obstacles =
    [ Physics.rectangle (Obstacle {})
        { width = Pixels.float 100, height = obstacleWidth }
        |> Physics.moveTo
            (Point2d.fromRecord Pixels.float { x = 0, y = -80 })
    , Physics.rectangle (Obstacle {})
        { width = Pixels.float 100, height = obstacleWidth }
        |> Physics.moveTo
            (Point2d.fromRecord Pixels.float { x = 200, y = -40 })
    , Physics.polygon (Obstacle {})
        (List.map (Point2d.fromRecord Pixels.float)
            [ { x = -80, y = 0 }
            , { x = 0, y = 60 }
            , { x = 100, y = 0 }
            , { x = 0, y = -70 }
            ]
        )
        |> Physics.moveTo
            (Point2d.fromRecord Pixels.float { x = 350, y = 0 })
    ]


audioPieces : List AudioPiece
audioPieces =
    [ AudioCollideStone ]


init : () -> Reaction State Effect
init () =
    Reaction.to
        { specific = Menu {}
        , shared =
            { audioCollideStone = Err Audio.UnknownError }
        }
        |> Reaction.effectsAdd
            (audioPieces
                |> List.map (\piece -> LoadAudio piece |> Shared)
            )


initGame : Reaction GameState GameEffect
initGame =
    Reaction.to
        { windowSize =
            -- dummy
            { width = 1920, height = 1080 }
        , collideStoneAudioTimes = []
        , world =
            (Physics.circle (Player { locationTrail = [] }) { radius = playerRadius }
                |> Physics.moveTo (Point2d.fromRecord Pixels.float { x = 0, y = 0 })
                |> Physics.movable
            )
                :: obstacles
        , camera = Frame2d.atOrigin
        , runStartTime =
            -- dummy
            Time.millisToPosix -1
        , lastSimulationTime =
            -- dummy
            Time.millisToPosix -1
        , keysPressed = []
        }
        |> Reaction.effectsAdd [ RequestInitialTime, GameRequestInitialWindowSize ]


withShared : shared -> specific -> { specific : specific, shared : shared }
withShared shared =
    \specific -> { specific = specific, shared = shared }


reactTo : Event -> (State -> Reaction State Effect)
reactTo event =
    case event of
        Shared eventShared ->
            \state ->
                reactToShared eventShared state
                    |> Reaction.effectMap Shared

        Specific eventSpecific ->
            \state ->
                reactToSpecific eventSpecific state


reactToSpecific : EventSpecific -> (State -> Reaction State Effect)
reactToSpecific eventSpecific =
    case eventSpecific of
        Menu menuEvent ->
            \state ->
                case state.specific of
                    Menu menuState ->
                        menuState
                            |> menuReactTo menuEvent
                            |> Reaction.map (withShared state.shared)

                    _ ->
                        Reaction.to state

        Game gameEvent ->
            \state ->
                case state.specific of
                    Game gameState ->
                        gameState
                            |> gameReactTo gameEvent
                            |> Reaction.map (withShared state.shared)

                    _ ->
                        Reaction.to state


reactToShared : EventShared -> (State -> Reaction State EffectShared)
reactToShared eventShared =
    case eventShared of
        AudioLoaded audioLoaded ->
            case audioLoaded.piece of
                AudioCollideStone ->
                    \state ->
                        Reaction.to
                            { state
                                | shared =
                                    let
                                        shared =
                                            state.shared
                                    in
                                    { shared
                                        | audioCollideStone = audioLoaded.result
                                    }
                            }


menuReactTo : MenuEvent -> (MenuState -> Reaction StateSpecific Effect)
menuReactTo menuEvent =
    case menuEvent of
        GameStartClicked ->
            \_ -> initGame |> Reaction.map Game |> Reaction.effectMap (Game >> Specific)


gameReactTo : GameEvent -> (GameState -> Reaction StateSpecific Effect)
gameReactTo event =
    case event of
        GameWindowSized size ->
            \state -> Reaction.to ({ state | windowSize = size } |> Game)

        InitialTimeReceived initialTime ->
            \state ->
                Reaction.to
                    ({ state
                        | runStartTime = initialTime
                        , lastSimulationTime = initialTime
                     }
                        |> Game
                    )

        FrameTickPassed newSimulationTime ->
            \state ->
                let
                    sinceLastSimulation : Duration
                    sinceLastSimulation =
                        Duration.from state.lastSimulationTime newSimulationTime

                    -- |> Vector2d.scaleBy 0.94
                    updatedWorld : Physics.World BodyWhat
                    updatedWorld =
                        state.world
                            |> Physics.worldBodyAlter
                                (\body ->
                                    case body.what of
                                        Player playerWhat ->
                                            body
                                                |> Physics.speedAlter
                                                    (\speed ->
                                                        speed
                                                            |> (if List.member Key.ArrowLeft state.keysPressed then
                                                                    Vector2d.plus (Vector2d.fromRecord Pixels.pixelsPerSecond { x = -25, y = 0 })

                                                                else
                                                                    identity
                                                               )
                                                            |> (if List.member Key.ArrowRight state.keysPressed then
                                                                    Vector2d.plus (Vector2d.fromRecord Pixels.pixelsPerSecond { x = 25, y = 0 })

                                                                else
                                                                    identity
                                                               )
                                                            |> (if List.member Key.ArrowUp state.keysPressed then
                                                                    Vector2d.plus (Vector2d.fromRecord Pixels.pixelsPerSecond { x = 0, y = 30 })

                                                                else
                                                                    identity
                                                               )
                                                    )
                                                |> Physics.whatReplace
                                                    ({ playerWhat
                                                        | locationTrail =
                                                            (playerWhat.locationTrail |> List.take 5)
                                                                |> (::) (body |> Physics.location)
                                                     }
                                                        |> Player
                                                    )

                                        Obstacle _ ->
                                            body
                                )

                    newSimulatedWorld =
                        updatedWorld |> Physics.simulate sinceLastSimulation
                in
                Reaction.to
                    ({ state
                        | lastSimulationTime = newSimulationTime
                        , world = newSimulatedWorld.world
                        , camera =
                            newSimulatedWorld.world
                                |> Physics.worldBody
                                    (\what ->
                                        case what of
                                            Player playerWhat ->
                                                playerWhat |> Just

                                            _ ->
                                                Nothing
                                    )
                                |> Maybe.map
                                    (\player ->
                                        state.camera
                                            |> Frame2d.moveTo (player |> Physics.location)
                                    )
                                |> Maybe.withDefault state.camera
                        , collideStoneAudioTimes =
                            state.collideStoneAudioTimes
                                |> (case newSimulatedWorld.collisionsWith of
                                        [] ->
                                            identity

                                        _ :: _ ->
                                            (::) newSimulationTime
                                   )
                     }
                        |> Game
                    )

        KeyPressed key ->
            \state ->
                Reaction.to
                    ({ state | keysPressed = state.keysPressed |> (::) key }
                        |> Game
                    )

        KeyReleased key ->
            \state ->
                Reaction.to
                    ({ state
                        | keysPressed =
                            state.keysPressed |> List.filter (\keyPressed -> keyPressed /= key)
                     }
                        |> Game
                    )


subscriptions : State -> Sub Event
subscriptions =
    \state ->
        case state.specific of
            Menu _ ->
                Sub.none

            Game _ ->
                [ Browser.Events.onResize
                    (\width height ->
                        { width = width |> toFloat, height = height |> toFloat }
                            |> GameWindowSized
                    )
                , Time.every (1000.0 / 30.0) FrameTickPassed
                , Browser.Events.onKeyDown (Json.Decode.map KeyPressed Key.decoder)
                , Browser.Events.onKeyUp (Json.Decode.map KeyReleased Key.decoder)
                ]
                    |> Sub.batch
                    |> Sub.map (Game >> Specific)


interpretEffect : Effect -> Reaction.EffectInterpretation Event
interpretEffect =
    \effect ->
        case effect of
            Specific effectSpecific ->
                effectSpecific |> interpretEffectSpecific

            Shared effectShared ->
                effectShared |> interpretEffectShared |> Reaction.effectInterpretationMap Shared


interpretEffectSpecific : EffectSpecific -> Reaction.EffectInterpretation Event
interpretEffectSpecific =
    \effectSpecific ->
        case effectSpecific of
            Menu menuEffect ->
                menuEffect |> menuInterpretEffect |> Reaction.effectInterpretationMap (Menu >> Specific)

            Game gameEffect ->
                gameEffect |> gameInterpretEffect |> Reaction.effectInterpretationMap (Game >> Specific)


interpretEffectShared : EffectShared -> Reaction.EffectInterpretation EventShared
interpretEffectShared =
    \effectShared ->
        case effectShared of
            LoadAudio piece ->
                Reaction.audioCommands
                    [ Audio.loadAudio
                        (\result -> AudioLoaded { result = result, piece = piece })
                        ([ "public/", piece |> audioPieceToName, ".mp3" ] |> String.concat)
                    ]


menuInterpretEffect : MenuEffect -> Reaction.EffectInterpretation MenuEvent
menuInterpretEffect =
    \effect ->
        case effect of
            MenuEffectNoneYet ever ->
                never ever


gameInterpretEffect : GameEffect -> Reaction.EffectInterpretation GameEvent
gameInterpretEffect =
    \effect ->
        case effect of
            RequestInitialTime ->
                Reaction.commands [ Time.now |> Task.perform InitialTimeReceived ]

            GameRequestInitialWindowSize ->
                Reaction.commands
                    [ Browser.Dom.getViewport
                        |> Task.perform
                            (\viewport ->
                                { width = viewport.viewport.width
                                , height = viewport.viewport.height
                                }
                                    |> GameWindowSized
                            )
                    ]


audioPieceToName : AudioPiece -> String
audioPieceToName =
    \audioPiece ->
        case audioPiece of
            AudioCollideStone ->
                "collide-stone"


uiDocument : State -> Browser.Document Event
uiDocument =
    \state ->
        { title = "tet"
        , body =
            state.specific |> ui |> List.singleton
        }


ui : StateSpecific -> Html Event
ui =
    \state ->
        case state of
            Menu menuState ->
                menuState
                    |> menuUi
                    |> Ui.layout
                        [ UiBackground.color (Ui.rgb 0 0 0)
                        , UiFont.color (Ui.rgb 1 1 1)
                        ]
                    |> Html.map (Menu >> Specific)

            Game gameState ->
                Svg.svg
                    [ SvgA.viewBox
                        0
                        0
                        gameState.windowSize.width
                        gameState.windowSize.height
                    ]
                    [ gameState |> backgroundUi
                    , [ gameState |> worldUi ]
                        |> Svg.g
                            [ SvgA.transform
                                [ Svg.Translate
                                    (gameState.windowSize.width / 2)
                                    (gameState.windowSize.height / 2)
                                ]
                            ]
                    ]
                    |> Html.map (Game >> Specific)


menuUi : MenuState -> Ui.Element MenuEvent
menuUi =
    \state ->
        Ui.column
            [ Ui.spacing 70
            , Ui.centerX
            , Ui.centerY
            , UiFont.size 55
            , UiFont.extraBold
            ]
            [ UiInput.button
                [ Ui.paddingXY 50 30
                , UiBackground.color (accentColor |> colorToUi)
                , UiBorder.rounded 1000
                , Ui.width Ui.fill
                ]
                { onPress = GameStartClicked |> Just
                , label = Ui.text "start" |> Ui.el [ Ui.centerX ]
                }
            ]


worldUi :
    { state_
        | world : Physics.World BodyWhat
        , camera : Frame2d Pixels Float { defines : Float }
    }
    -> Svg event_
worldUi =
    \state ->
        Svg.g [ SvgA.transform [ Svg.Scale 1 -1 ] ]
            [ Svg.g
                [ SvgA.transform
                    (state.camera
                        |> cameraToTransform
                    )
                ]
                (state.world |> List.map bodyUi)
            ]


bodyUi : Physics.Body BodyWhat -> Svg event_
bodyUi =
    \body ->
        Svg.g
            [ SvgA.transform
                (body |> Physics.frame |> frameToTransform)
            ]
            [ case body.what of
                Player playerWhat ->
                    body
                        |> Physics.whatReplace playerWhat
                        |> playerUi

                Obstacle obstacleWhat ->
                    body
                        |> Physics.whatReplace obstacleWhat
                        |> obstacleUi
            ]


shapeDebug : Physics.Shape -> Svg event_
shapeDebug =
    \shape ->
        case shape of
            Physics.Circle circle ->
                Svg.circle
                    [ SvgA.r (circle.radius |> Pixels.toFloat |> Svg.px)
                    , SvgA.stroke (Svg.Paint Color.red)
                    ]
                    []

            Physics.Polygon faces ->
                Svg.polygon
                    [ SvgA.points
                        (faces
                            |> List.concatMap
                                (\face ->
                                    [ face |> LineSegment2d.startPoint |> point2dToTupleBoth Pixels.toFloat
                                    , face |> LineSegment2d.endPoint |> point2dToTupleBoth Pixels.toFloat
                                    ]
                                )
                        )
                    , SvgA.stroke (Svg.Paint Color.red)
                    ]
                    []


frameToTransform : Frame2d Pixels Float { defines : Float } -> List Svg.Transform
frameToTransform frame =
    [ Svg.Translate
        (frame |> Frame2d.originPoint |> Point2d.xCoordinate |> Pixels.toFloat)
        (frame |> Frame2d.originPoint |> Point2d.yCoordinate |> Pixels.toFloat)
    ]


cameraToTransform : Frame2d Pixels Float { defines : Float } -> List Svg.Transform
cameraToTransform frame =
    [ Svg.Translate
        (frame |> Frame2d.originPoint |> Point2d.xCoordinate |> Quantity.negate |> Pixels.toFloat)
        (frame |> Frame2d.originPoint |> Point2d.yCoordinate |> Quantity.negate |> Pixels.toFloat)
    ]


accentColor : Color
accentColor =
    Color.rgb 1 0.3 0


backgroundPlainUi : Svg event_
backgroundPlainUi =
    Svg.rect
        [ SvgA.width (Svg.percent 100)
        , SvgA.height (Svg.percent 100)
        , SvgA.fill (Svg.Paint accentColor)
        ]
        []


backgroundUi :
    { state_
        | runStartTime : Time.Posix
        , lastSimulationTime : Time.Posix
    }
    -> Svg event_
backgroundUi =
    \state ->
        Svg.g []
            [ backgroundPlainUi
            , let
                runDuration : Duration
                runDuration =
                    Duration.from state.runStartTime state.lastSimulationTime
              in
              Svg.text_
                [ SvgA.x (Svg.percent 50)
                , SvgA.y (Svg.percent 16)
                , SvgA.fontSize (Svg.percent 400)
                , SvgA.fontWeight Svg.FontWeightBolder
                , SvgA.opacity (Svg.Opacity 0.28)
                , SvgA.textAnchor Svg.AnchorMiddle
                , SvgA.fontFamily [ "monospace" ]
                ]
                [ TypedSvg.Core.text
                    ([ runDuration
                        |> Duration.inMinutes
                        |> floor
                        |> remainderBy 60
                        |> starting0s 2
                     , " "
                     , runDuration
                        |> Duration.inSeconds
                        |> floor
                        |> remainderBy 60
                        |> starting0s 2
                     , " "
                     , runDuration
                        |> Duration.inMilliseconds
                        |> floor
                        |> remainderBy 1000
                        |> starting0s 1
                     ]
                        |> String.concat
                    )
                ]
            ]


starting0s : Int -> Int -> String
starting0s digits =
    \int ->
        if digits == 0 then
            ""

        else
            (int // 10 |> starting0s (digits - 1))
                ++ (case int of
                        0 ->
                            "\u{2002}"

                        intNon0 ->
                            intNon0 |> remainderBy 10 |> String.fromInt
                   )


playerRadius : Quantity Float Pixels
playerRadius =
    Pixels.float 40


playerUi :
    Physics.Body PlayerWhat
    -> Svg event_
playerUi =
    \player ->
        let
            trail =
                case player |> Physics.what |> .locationTrail |> List.reverse of
                    [] ->
                        Svg.g [] []

                    oldestTrailLocation :: _ ->
                        let
                            trailOffset : Vector2d Pixels Float
                            trailOffset =
                                Vector2d.from (player |> Physics.location) oldestTrailLocation

                            trailDirection : Direction2d Float
                            trailDirection =
                                trailOffset
                                    |> Vector2d.direction
                                    |> Maybe.withDefault Direction2d.negativeY

                            left : Point2d Pixels Float
                            left =
                                Point2d.origin
                                    |> Point2d.translateBy
                                        (Vector2d.withLength playerRadius (trailDirection |> Direction2d.rotateCounterclockwise))

                            right : Point2d Pixels Float
                            right =
                                Point2d.origin
                                    |> Point2d.translateBy
                                        (Vector2d.withLength playerRadius (trailDirection |> Direction2d.rotateClockwise))

                            trailEndCenter : Point2d Pixels Float
                            trailEndCenter =
                                Point2d.origin |> Point2d.translateBy trailOffset
                        in
                        Svg.g []
                            [ Svg.polygon
                                [ SvgA.fill (Svg.Paint (Color.rgb 0 0 0))
                                , SvgA.points
                                    ([ right
                                     , right |> Point2d.translateBy trailOffset
                                     , left |> Point2d.translateBy trailOffset
                                     , left
                                     ]
                                        |> List.map (point2dToTupleBoth Pixels.toFloat)
                                    )
                                ]
                                []
                            , Svg.circle
                                [ SvgA.r (Svg.px (playerRadius |> Pixels.toFloat))
                                , SvgA.fill (Svg.Paint (Color.rgb 0 0 0))
                                , SvgA.cx (Svg.px (trailEndCenter |> Point2d.xCoordinate |> Pixels.toFloat))
                                , SvgA.cy (Svg.px (trailEndCenter |> Point2d.yCoordinate |> Pixels.toFloat))
                                ]
                                []
                            ]
        in
        Svg.g
            []
            [ Svg.defs
                []
                [ Svg.filter
                    [ SvgA.id "plyer-blur"
                    ]
                    [ TypedSvg.Filters.gaussianBlur
                        [ TypedSvg.Core.attribute "stdDeviation"
                            ((player
                                |> Physics.speed
                                |> Vector2d.for (Duration.seconds 0.3)
                                |> Vector2d.xComponent
                                |> Pixels.toFloat
                                |> floor
                                |> String.fromInt
                             )
                                ++ " "
                                ++ (player
                                        |> Physics.speed
                                        |> Vector2d.for (Duration.seconds 0.3)
                                        |> Vector2d.yComponent
                                        |> Pixels.toFloat
                                        |> floor
                                        |> String.fromInt
                                   )
                            )

                        -- , TypedSvg.Filters.Attributes.radius 0 10
                        ]
                        []
                    ]
                ]
            , trail
            , Svg.circle
                [ SvgA.r (Svg.px (playerRadius |> Pixels.toFloat))
                , SvgA.fill (Svg.Paint (Color.rgb 1 1 1))
                , SvgA.filter (Svg.Filter "url(#player-blur)")
                ]
                []
            , Svg.circle
                [ SvgA.r (Svg.px 10)
                , SvgA.fill (Svg.Paint (Color.rgb 0 0 0))
                , SvgA.cx (Svg.px -15)
                ]
                []
            , Svg.circle
                [ SvgA.r (Svg.px 10)
                , SvgA.fill (Svg.Paint (Color.rgb 0 0 0))
                , SvgA.cx (Svg.px 15)
                ]
                []
            ]


obstacleWidth : Quantity Float Pixels
obstacleWidth =
    Pixels.float 30


obstacleUi : Physics.Body ObstacleWhat -> Svg event_
obstacleUi =
    \obstacle ->
        let
            attributes =
                [ SvgA.fill (Svg.Paint (Color.rgb 1 1 1))
                , SvgA.strokeWidth (Svg.px (obstacleWidth |> Pixels.toFloat))
                , SvgA.strokeLinecap Svg.StrokeLinecapRound
                , SvgA.strokeLinejoin Svg.StrokeLinejoinRound
                ]
        in
        case obstacle |> Physics.shape of
            Physics.Circle circle ->
                Svg.circle
                    ([ SvgA.r (circle.radius |> Pixels.toFloat |> Svg.px)
                     ]
                        ++ attributes
                    )
                    []

            Physics.Polygon faces ->
                Svg.polygon
                    ([ SvgA.points
                        (faces
                            |> List.concatMap
                                (\face ->
                                    [ face |> LineSegment2d.startPoint |> point2dToTupleBoth Pixels.toFloat
                                    , face |> LineSegment2d.endPoint |> point2dToTupleBoth Pixels.toFloat
                                    ]
                                )
                        )
                     ]
                        ++ attributes
                    )
                    []


point2dToTupleBoth : (Quantity.Quantity Float units -> a) -> Point2d units coordinates_ -> ( a, a )
point2dToTupleBoth quantityChange =
    \point ->
        ( point |> Point2d.xCoordinate |> quantityChange
        , point |> Point2d.yCoordinate |> quantityChange
        )


audio : State -> Audio.Audio
audio =
    \state ->
        case state.specific of
            Menu _ ->
                Audio.silence

            Game gameState ->
                case state.shared.audioCollideStone of
                    Err _ ->
                        Audio.silence

                    Ok moveAudio ->
                        gameState.collideStoneAudioTimes
                            |> List.map
                                (\time -> Audio.audio moveAudio time)
                            |> Audio.group



-- helpers


uiToColor : Ui.Color -> Color
uiToColor =
    \uiColor ->
        Color.fromRgba (uiColor |> Ui.toRgb)


colorToUi : Color -> Ui.Color
colorToUi =
    \color ->
        Ui.fromRgb (color |> Color.toRgba)
