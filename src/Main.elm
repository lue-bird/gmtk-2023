port module Main exposing (main)

import Angle exposing (Angle)
import Audio exposing (AudioData)
import Axis2d exposing (Axis2d)
import Browser
import Browser.Dom
import Browser.Events
import Bytes
import Circle2d exposing (Circle2d)
import Color exposing (Color)
import Direction2d exposing (Direction2d)
import Duration exposing (Duration, Seconds)
import Element as Ui
import Element.Background as UiBackground
import Element.Border as UiBorder
import Element.Font as UiFont
import Element.Input as UiInput
import File
import File.Select
import Forest.Path
import Frame2d exposing (Frame2d)
import Html exposing (Html)
import Html.Events
import Json.Decode
import Json.Encode
import Key
import LineSegment2d exposing (LineSegment2d)
import List.Extra
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate)
import Random
import Random.Extra
import Reaction exposing (Reaction)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Svg as UntypedSvg
import Svg.Lazy
import Task
import Time
import Tree exposing (Tree)
import Tree.Navigate
import Tree.Path exposing (TreePath)
import TypedSvg as Svg
import TypedSvg.Attributes as SvgA
import TypedSvg.Core exposing (Svg)
import TypedSvg.Events
import TypedSvg.Filters
import TypedSvg.Filters.Attributes
import TypedSvg.Types as Svg
import Vector2d exposing (Vector2d)
import VirtualDom


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
    = AudioLoaded { piece : AudioKind, result : Result Audio.LoadError Audio.Source }


type alias EventSpecific =
    MenuOrGame MenuEvent GameEvent


type MenuEvent
    = GameStartClicked


type GameEvent
    = PlantBlossomPressed TreePath
    | MouseMoved { clientX : Float, clientY : Float }
    | MouseReleased
    | FreeBlossomPressed Int
    | GameWindowSized { width : Float, height : Float }
    | InitialRandomSeedReceived Random.Seed
    | InitialTimeReceived Time.Posix
    | FrameTickPassed Time.Posix
    | KeyPressed Key.Key
    | KeyReleased Key.Key


type alias State =
    SpecificAndShared StateSpecific StateShared


type alias StateShared =
    RecordWithoutConstructorFunction
        { audio : EachAudio (Result Audio.LoadError Audio.Source)
        }


type alias StateSpecific =
    MenuOrGame MenuState GameState


type alias MenuState =
    RecordWithoutConstructorFunction
        {}


type alias GameState =
    RecordWithoutConstructorFunction
        { windowSize : { width : Float, height : Float }
        , audioTimes : EachAudio (List Time.Posix)
        , plant : Plant
        , blossomSnappedToMouse : Maybe FreeBlossom
        , freeBlossoms : List FreeBlossom
        , lightRays : List LightRay
        , camera : Frame2d Pixels Float { defines : Float }
        , scrollYSpeed : Quantity Float (Rate Pixels Seconds)
        , highest : Quantity Float Pixels
        , keysPressed : List Key.Key
        , randomSeed : Random.Seed
        , lastTick : Time.Posix
        , initialTime : Time.Posix
        }


type alias Plant =
    Tree PlantSegment


type alias PlantSegment =
    { orientation : Vector2d Pixels Float
    , blossom : Maybe Blossom
    , color : Color
    }


type alias Blossom =
    { color : Color }


type alias FreeBlossom =
    { point : Point2d Pixels Float
    , color : Color
    }


type alias LightRay =
    { axis : HalfLine }


type alias Effect =
    SpecificOrShared EffectSpecific EffectShared


type alias EffectSpecific =
    MenuOrGame MenuEffect GameEffect


type EffectShared
    = LoadAudio AudioKind


type MenuEffect
    = MenuEffectNoneYet Never


type GameEffect
    = RequestInitialRandomSeed
    | RequestInitialTime
    | GameRequestInitialWindowSize


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
        , audio = audio
        , audioPort =
            { toJS = audioPortToJS
            , fromJS = audioPortFromJS
            }
        }


type AudioKind
    = AudioMirrorGrow
    | AudioMirrorPlace
    | AudioMirrorGrab
    | AudioMusic


audioKinds : List AudioKind
audioKinds =
    [ AudioMirrorGrow, AudioMirrorPlace, AudioMirrorGrab, AudioMusic ]


type alias EachAudio perKind =
    { mirrorGrow : perKind
    , mirrorPlace : perKind
    , mirrorGrab : perKind
    , music : perKind
    }


eachAudio : perKind -> EachAudio perKind
eachAudio perKind =
    { mirrorGrow = perKind
    , mirrorPlace = perKind
    , mirrorGrab = perKind
    , music = perKind
    }


alterAudioOfKind : AudioKind -> (a -> a) -> EachAudio a -> EachAudio a
alterAudioOfKind kind f =
    case kind of
        AudioMirrorGrow ->
            \r -> { r | mirrorGrow = r.mirrorGrow |> f }

        AudioMirrorPlace ->
            \r -> { r | mirrorPlace = r.mirrorPlace |> f }

        AudioMirrorGrab ->
            \r -> { r | mirrorGrab = r.mirrorGrab |> f }

        AudioMusic ->
            \r -> { r | music = r.music |> f }


accessAudioOfKind : AudioKind -> EachAudio a -> a
accessAudioOfKind kind =
    case kind of
        AudioMirrorGrow ->
            .mirrorGrow

        AudioMirrorPlace ->
            .mirrorPlace

        AudioMirrorGrab ->
            .mirrorGrab

        AudioMusic ->
            .music


audioPieceToName : AudioKind -> String
audioPieceToName =
    \audioPiece ->
        case audioPiece of
            AudioMirrorGrow ->
                "mirror-grow"

            AudioMirrorPlace ->
                "mirror-place"

            AudioMirrorGrab ->
                "mirror-grab"

            AudioMusic ->
                "music"


init : () -> Reaction State Effect
init () =
    Reaction.to
        { specific = Menu {}
        , shared =
            { audio = eachAudio (Err Audio.UnknownError) }
        }
        |> Reaction.effectsAdd
            (audioKinds |> List.map (\piece -> LoadAudio piece |> Shared))


initGame : Reaction GameState GameEffect
initGame =
    Reaction.to
        { windowSize =
            -- dummy
            { width = 1920, height = 1080 }
        , audioTimes = eachAudio []
        , plant =
            Tree.tree
                { orientation = Vector2d.fromPixels { x = 0, y = 30 }
                , color = Color.rgb 0.5 0.8 0
                , blossom = Just { color = Color.rgb 0 1 1 }
                }
                []
        , blossomSnappedToMouse = Nothing
        , freeBlossoms =
            [ { color = Color.rgb 0 1 1
              , point = Point2d.fromRecord Pixels.float { x = -345, y = 395 }
              }
            ]
        , lightRays =
            [ { axis =
                    { originPoint = Point2d.fromRecord Pixels.float { x = -1000, y = 500 }
                    , direction = Direction2d.fromAngle (Angle.turns -0.02)
                    }
              }
            ]
        , camera = Frame2d.atPoint (Point2d.fromRecord Pixels.float { x = 0, y = -100 })
        , scrollYSpeed = Pixels.float 14 |> Quantity.per Duration.second
        , highest = Pixels.float 30
        , keysPressed = []
        , randomSeed =
            -- dummy
            Random.initialSeed 1635127483
        , initialTime =
            -- dummy
            Time.millisToPosix -1
        , lastTick =
            -- dummy
            Time.millisToPosix -1
        }
        |> Reaction.effectsAdd
            [ RequestInitialRandomSeed
            , RequestInitialTime
            , GameRequestInitialWindowSize
            ]


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
            \state ->
                Reaction.to
                    { state
                        | shared =
                            { audio =
                                state.shared.audio
                                    |> alterAudioOfKind audioLoaded.piece (\_ -> audioLoaded.result)
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
        PlantBlossomPressed pathToSegmentWithBlossom ->
            \state ->
                Reaction.to
                    ({ state
                        | plant =
                            state.plant
                                |> Tree.Navigate.alter pathToSegmentWithBlossom
                                    (\sub -> sub |> Tree.mapLabel (\segment -> { segment | blossom = Nothing }))
                        , blossomSnappedToMouse =
                            state.plant |> separateBlossomAt pathToSegmentWithBlossom
                        , audioTimes =
                            state.audioTimes |> (\r -> { r | mirrorGrab = r.mirrorGrab |> (::) state.lastTick })
                     }
                        |> Game
                    )

        MouseMoved newMousePoint ->
            \state ->
                case state.blossomSnappedToMouse of
                    Nothing ->
                        Reaction.to (state |> Game)

                    Just blossomSnappedToMouse ->
                        Reaction.to
                            ({ state
                                | blossomSnappedToMouse =
                                    { blossomSnappedToMouse
                                        | point =
                                            Point2d.fromRecord Pixels.float
                                                { x =
                                                    newMousePoint.clientX
                                                        - state.windowSize.width
                                                        / 2
                                                        + (state.camera |> Frame2d.originPoint |> Point2d.xCoordinate |> Pixels.toFloat)
                                                , y =
                                                    state.windowSize.height
                                                        - newMousePoint.clientY
                                                        + (state.camera |> Frame2d.originPoint |> Point2d.yCoordinate |> Pixels.toFloat)
                                                }
                                    }
                                        |> Just
                             }
                                |> Game
                            )

        MouseReleased ->
            \state ->
                case state.blossomSnappedToMouse of
                    Nothing ->
                        Reaction.to (state |> Game)

                    Just blossomSnappedToMouse ->
                        Reaction.to
                            ({ state
                                | blossomSnappedToMouse = Nothing
                                , freeBlossoms = blossomSnappedToMouse :: state.freeBlossoms
                                , audioTimes =
                                    state.audioTimes |> (\r -> { r | mirrorPlace = r.mirrorPlace |> (::) state.lastTick })
                             }
                                |> Game
                            )

        FreeBlossomPressed freeBlossomIndex ->
            \state ->
                Reaction.to
                    ({ state
                        | blossomSnappedToMouse =
                            state.freeBlossoms |> List.Extra.getAt freeBlossomIndex
                        , freeBlossoms =
                            state.freeBlossoms
                                |> List.Extra.removeAt freeBlossomIndex
                        , audioTimes =
                            state.audioTimes |> (\r -> { r | mirrorGrab = r.mirrorGrab |> (::) state.lastTick })
                     }
                        |> Game
                    )

        GameWindowSized size ->
            \state -> Reaction.to ({ state | windowSize = size } |> Game)

        InitialRandomSeedReceived initialRandomSeed ->
            \state ->
                Reaction.to
                    ({ state | randomSeed = initialRandomSeed }
                        |> Game
                    )

        InitialTimeReceived initialTime ->
            \state ->
                Reaction.to
                    ({ state
                        | initialTime = initialTime
                        , lastTick = initialTime
                     }
                        |> Game
                    )

        FrameTickPassed newSimulationTime ->
            \state ->
                let
                    sinceLastTick =
                        Duration.from state.lastTick newSimulationTime

                    ( plantProgressed, newSeed ) =
                        Random.step
                            (Random.andThen
                                (\shouldGrowLeaves ->
                                    if shouldGrowLeaves then
                                        state.plant |> plantGrowLeaf

                                    else
                                        state.plant |> Random.constant
                                )
                                (Random.weighted
                                    ( sinceLastTick |> Duration.inSeconds, True )
                                    [ ( 0.06, False ) ]
                                )
                                |> Random.andThen
                                    (\plantGrown ->
                                        Random.andThen
                                            (\shouldAddBlossom ->
                                                if shouldAddBlossom then
                                                    plantGrown |> plantGrowBlossoms

                                                else
                                                    plantGrown |> Random.constant
                                            )
                                            (Random.weighted
                                                ( sinceLastTick |> Duration.inSeconds, True )
                                                [ ( 0.06, False ) ]
                                            )
                                    )
                            )
                            state.randomSeed

                    plantGrowLeaf : Plant -> Random.Generator Plant
                    plantGrowLeaf plant =
                        -- TODO with the parent's absolute location. If hit light mostly only then add random piece
                        Random.map
                            (\subs ->
                                Tree.tree (plant |> Tree.label) subs
                            )
                            (case plant |> Tree.children of
                                [] ->
                                    Random.map (\only -> [ Tree.singleton only ]) plantSegmentRandom

                                [ onlySub ] ->
                                    Random.andThen
                                        (\shouldAddBranch ->
                                            if shouldAddBranch then
                                                Random.map (\new -> [ onlySub, Tree.singleton new ]) plantSegmentRandom

                                            else
                                                Random.map List.singleton (plantGrowLeaf onlySub)
                                        )
                                        (Random.weighted
                                            ( 0.024 {- (1 / ((onlySub |> Tree.count |> toFloat) ^ 2)) * 0.5 -}
                                            , True
                                            )
                                            [ ( 1, False ) ]
                                        )

                                sub0 :: sub1Up ->
                                    Random.andThen
                                        (\index ->
                                            (sub0 :: sub1Up)
                                                |> List.indexedMap
                                                    (\subIndex sub ->
                                                        if subIndex == index then
                                                            plantGrowLeaf sub

                                                        else
                                                            sub |> Random.constant
                                                    )
                                                |> Random.Extra.sequence
                                        )
                                        (let
                                            weighted i =
                                                ( {- ((sub0 :: sub1Up)
                                                       |> List.Extra.getAt i
                                                       |> Maybe.withDefault sub0
                                                       |> Tree.count
                                                       |> toFloat
                                                     )
                                                  -}
                                                  10
                                                    * 2.5
                                                , i
                                                )
                                         in
                                         Random.weighted (0 |> weighted)
                                            (List.range 1 (sub1Up |> List.length) |> List.map weighted)
                                        )
                            )

                    plantGrowBlossoms plant =
                        Random.map2
                            (\blossom subs ->
                                Tree.tree (plant |> Tree.label |> (\r -> { r | blossom = r.blossom |> onJust blossom })) subs
                            )
                            (Random.Extra.frequency ( 0.999, Nothing |> Random.constant )
                                [ ( 0.00025, Random.map Just blossomRandom ) ]
                            )
                            ((plant |> Tree.children)
                                |> Random.Extra.traverse plantGrowBlossoms
                            )

                    -- TODO add sounds for blossoms
                    -- TODO move camera with arrow keys
                in
                Reaction.to
                    ({ state
                        | plant = plantProgressed
                        , highest =
                            Quantity.max
                                state.highest
                                (plantProgressed |> plantHeight)
                        , scrollYSpeed =
                            state.scrollYSpeed
                                |> (if List.member Key.ArrowUp state.keysPressed || List.member Key.W state.keysPressed then
                                        Quantity.plus (Pixels.float 16 |> Quantity.per Duration.second)

                                    else
                                        identity
                                   )
                                |> (if List.member Key.ArrowDown state.keysPressed || List.member Key.S state.keysPressed then
                                        Quantity.plus (Pixels.float -16 |> Quantity.per Duration.second)

                                    else
                                        identity
                                   )
                                |> Quantity.multiplyBy (0.82 ^ (sinceLastTick |> Duration.inSeconds))
                        , camera =
                            state.camera
                                |> Frame2d.translateBy
                                    (Vector2d.fromRecord Pixels.float
                                        { x = 0, y = state.scrollYSpeed |> Quantity.for sinceLastTick |> Pixels.toFloat }
                                    )
                        , lastTick = newSimulationTime
                        , randomSeed = newSeed
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


port audioPortToJS : Json.Encode.Value -> Cmd msg_


port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg


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
            RequestInitialRandomSeed ->
                Reaction.commands [ Random.independentSeed |> Random.generate InitialRandomSeedReceived ]

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


plantHeight : Plant -> Quantity Float Pixels
plantHeight =
    \plant ->
        plant
            |> Tree.Navigate.restructure
                (\step ->
                    step.label.orientation
                        |> Vector2d.yComponent
                        |> Quantity.plus (step.children |> Quantity.maximum |> Maybe.withDefault Quantity.zero)
                )


separateBlossomAt : TreePath -> Plant -> Maybe { color : Color, point : Point2d Pixels Float }
separateBlossomAt path =
    \plant ->
        case path |> Tree.Path.step of
            Nothing ->
                case plant |> Tree.label |> .blossom of
                    Nothing ->
                        Nothing

                    Just blossom ->
                        { point =
                            Point2d.origin
                                |> Point2d.translateBy (plant |> Tree.label |> .orientation |> Vector2d.scaleBy 0.5)
                        , color = blossom.color
                        }
                            |> Just

            Just subPath ->
                case
                    plant
                        |> Tree.children
                        |> List.Extra.getAt (subPath |> Forest.Path.treeIndex)
                        |> Maybe.andThen (separateBlossomAt (subPath |> Forest.Path.pathIntoTreeAtIndex))
                of
                    Nothing ->
                        Nothing

                    Just subBlossom ->
                        { subBlossom
                            | point =
                                subBlossom.point
                                    |> Point2d.translateBy (plant |> Tree.label |> .orientation)
                        }
                            |> Just


plantSegmentRandom : Random.Generator PlantSegment
plantSegmentRandom =
    Random.map2 (\orientation color -> { orientation = orientation, color = color, blossom = Nothing })
        (Random.map2
            (\length angleInTurns ->
                Vector2d.withLength
                    length
                    (Direction2d.fromAngle angleInTurns)
            )
            (Random.map Pixels.float (Random.float 20 50))
            (Random.map Angle.turns (Random.float 0.125 0.375))
        )
        plantColorRandom


plantColorRandom : Random.Generator Color
plantColorRandom =
    Random.map3 Color.rgb
        (Random.float 0 0.35)
        (Random.float 0.7 1)
        (Random.float 0 0.4)


blossomRandom : Random.Generator Blossom
blossomRandom =
    Random.map (\color -> { color = color })
        blossomColorRandom


blossomColorRandom : Random.Generator Color
blossomColorRandom =
    Random.map3 Color.rgb
        (Random.float 0.3 1)
        (Random.float 0.3 0.6)
        (Random.float 0.3 1)
        |> Random.Extra.filter
            (\color ->
                let
                    c =
                        color |> Color.toRgba

                    average =
                        (c.red + c.blue + c.green) / 3

                    isDifferentEnoughFromAverage component =
                        abs (component - average) > 0.2
                in
                List.any isDifferentEnoughFromAverage [ c.red, c.blue, c.green ]
            )


uiDocument : State -> Browser.Document Event
uiDocument =
    \state ->
        { title = "mirror blossom"
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
                    [ SvgA.viewBox 0 0 gameState.windowSize.width gameState.windowSize.height
                    , TypedSvg.Events.on "mousemove"
                        (Json.Decode.map2 (\clientX clientY -> MouseMoved { clientX = clientX, clientY = clientY })
                            (Json.Decode.field "clientX" Json.Decode.float)
                            (Json.Decode.field "clientY" Json.Decode.float)
                            |> VirtualDom.Normal
                        )
                    , TypedSvg.Events.onMouseUp MouseReleased
                    ]
                    [ backgroundUi
                    , [ gameState |> worldUi ]
                        |> Svg.g
                            [ SvgA.transform
                                [ Svg.Translate
                                    (gameState.windowSize.width / 2)
                                    -gameState.windowSize.height
                                ]
                            ]
                        |> List.singleton
                        |> Svg.g
                            [ SvgA.transform [ Svg.Scale 1 -1 ]
                            ]
                    , gameState |> scoreUi
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
                , label = Ui.text "play" |> Ui.el [ Ui.centerX ]
                }
            ]


worldUi :
    { state_
        | plant : Plant
        , lightRays : List LightRay
        , blossomSnappedToMouse : Maybe FreeBlossom
        , freeBlossoms : List FreeBlossom
        , camera : Frame2d Pixels Float { defines : Float }
        , windowSize : { width : Float, height : Float }
    }
    -> Svg GameEvent
worldUi =
    \state ->
        Svg.g
            [ SvgA.transform
                (state.camera |> cameraToTransform)
            ]
            (groundUi
                :: (state.lightRays |> List.map (lightRayUi state))
                ++ [ state.plant |> plantWithoutBlossomsUi, state.plant |> plantBlossomsOnlyUi ]
                ++ (state.freeBlossoms
                        |> List.indexedMap
                            (\i freeBlossom ->
                                freeBlossom |> freeBlossomUi [ TypedSvg.Events.onMouseDown (FreeBlossomPressed i) ]
                            )
                   )
                ++ (case state.blossomSnappedToMouse of
                        Nothing ->
                            []

                        Just snappedBlossom ->
                            [ snappedBlossom |> freeBlossomUi [] ]
                   )
            )


groundUi =
    [ Svg.ellipse
        [ SvgA.rx (Svg.percent 60)
        , SvgA.ry (Svg.px 100)
        , SvgA.cy (Svg.px -100)
        , SvgA.fill (Svg.Paint (Color.rgb 0.3 0.1 0))
        ]
        []
    , Svg.ellipse
        [ SvgA.rx (Svg.percent 40)
        , SvgA.ry (Svg.px 230)
        , SvgA.cy (Svg.px -210)
        , SvgA.cx (Svg.px -210)
        , SvgA.fill (Svg.Paint (Color.rgb 0.25 0.13 0))
        ]
        []
    , plantWithoutBlossomsUi
        (Tree.tree
            { orientation = Vector2d.fromPixels { x = 0, y = 20 }
            , color = Color.rgb 0.4 0.6 0.1
            , blossom = Nothing
            }
            [ Tree.tree
                { orientation = Vector2d.fromPixels { x = 50, y = 20 }
                , color = Color.rgb 0 0.8 0
                , blossom = Nothing
                }
                [ Tree.tree
                    { orientation = Vector2d.fromPixels { x = -20, y = 50 }
                    , color = Color.rgb 0 0.4 0
                    , blossom = Nothing
                    }
                    []
                , Tree.tree
                    { orientation = Vector2d.fromPixels { x = -20, y = 10 }
                    , color = Color.rgb 0 0.2 0
                    , blossom = Nothing
                    }
                    []
                , Tree.tree
                    { orientation = Vector2d.fromPixels { x = 10, y = 60 }
                    , color = Color.rgb 0.3 0.8 0
                    , blossom = Nothing
                    }
                    []
                ]
            , Tree.tree
                { orientation = Vector2d.fromPixels { x = -20, y = 20 }
                , color = Color.rgb 0 0.8 0
                , blossom = Nothing
                }
                [ Tree.tree
                    { orientation = Vector2d.fromPixels { x = -10, y = 50 }
                    , color = Color.rgb 0 0.4 0
                    , blossom = Nothing
                    }
                    []
                , Tree.tree
                    { orientation = Vector2d.fromPixels { x = 20, y = 10 }
                    , color = Color.rgb 0 0.2 0
                    , blossom = Nothing
                    }
                    []
                , Tree.tree
                    { orientation = Vector2d.fromPixels { x = -20, y = 10 }
                    , color = Color.rgb 0.3 0.8 0
                    , blossom = Nothing
                    }
                    []
                ]
            ]
        )
        |> List.singleton
        |> Svg.g
            [ SvgA.transform
                [ Point2d.fromRecord Pixels.float { x = 250, y = -10 }
                    |> pointToTranslateTransform
                ]
            ]
    , [ ( -180, -50 ), ( -100, -80 ), ( -10, -10 ), ( 40, -40 ), ( 240, -60 ) ]
        |> List.map
            (\( x, y ) ->
                Svg.ellipse
                    [ SvgA.cx (Svg.px x)
                    , SvgA.cy (Svg.px y)
                    , SvgA.fill (Svg.Paint (Color.rgba 1 1 0 0.1))
                    , SvgA.rx (Svg.px 30)
                    , SvgA.ry (Svg.px 10)
                    ]
                    []
            )
        |> Svg.g []
    , plantWithoutBlossomsUi
        (Tree.tree
            { orientation = Vector2d.fromPixels { x = -6, y = 20 }
            , color = Color.rgb 0.4 0.6 0.1
            , blossom = Nothing
            }
            [ Tree.tree
                { orientation = Vector2d.fromPixels { x = 10, y = 20 }
                , color = Color.rgb 0 0.8 0.2
                , blossom = Nothing
                }
                [ Tree.tree
                    { orientation = Vector2d.fromPixels { x = -30, y = 50 }
                    , color = Color.rgb 0.3 0.4 0
                    , blossom = Nothing
                    }
                    []
                , Tree.tree
                    { orientation = Vector2d.fromPixels { x = -20, y = 10 }
                    , color = Color.rgb 0 0.2 0.2
                    , blossom = Nothing
                    }
                    []
                , Tree.tree
                    { orientation = Vector2d.fromPixels { x = 18, y = 40 }
                    , color = Color.rgb 0.3 0.8 0.3
                    , blossom = Nothing
                    }
                    []
                ]
            , Tree.tree
                { orientation = Vector2d.fromPixels { x = 20, y = 20 }
                , color = Color.rgb 0.5 0.8 0.2
                , blossom = Nothing
                }
                [ Tree.tree
                    { orientation = Vector2d.fromPixels { x = -10, y = 50 }
                    , color = Color.rgb 0.1 0.3 0.2
                    , blossom = Nothing
                    }
                    []
                , Tree.tree
                    { orientation = Vector2d.fromPixels { x = -20, y = 10 }
                    , color = Color.rgb 0.1 0.5 0.2
                    , blossom = Nothing
                    }
                    []
                , Tree.tree
                    { orientation = Vector2d.fromPixels { x = 20, y = 10 }
                    , color = Color.rgb 0.3 1 0.2
                    , blossom = Nothing
                    }
                    []
                ]
            ]
        )
        |> List.singleton
        |> Svg.g
            [ SvgA.transform
                [ Point2d.fromRecord Pixels.float { x = -150, y = -15 }
                    |> pointToTranslateTransform
                ]
            ]
    ]
        |> Svg.g []


plantWithoutBlossomsUi : Plant -> Svg event_
plantWithoutBlossomsUi =
    \plant ->
        case plant |> Tree.children of
            [] ->
                plant |> Tree.label |> plantSegmentUi

            sub0 :: sub1Up ->
                [ (sub0 :: sub1Up)
                    |> List.map plantWithoutBlossomsUi
                    |> Svg.g
                        [ SvgA.transform
                            [ Point2d.origin
                                |> Point2d.translateBy (plant |> Tree.label |> .orientation)
                                |> pointToTranslateTransform
                            ]
                        ]
                , plant |> Tree.label |> plantSegmentUi
                ]
                    |> Svg.g []


plantSegmentUi : PlantSegment -> Svg event_
plantSegmentUi =
    Svg.Lazy.lazy
        (\label ->
            let
                ( x2, y2 ) =
                    Point2d.origin
                        |> Point2d.translateBy label.orientation
                        |> Point2d.toTuple Pixels.toFloat
            in
            Svg.line
                [ SvgA.x1 (Svg.px 0)
                , SvgA.y1 (Svg.px 0)
                , SvgA.x2 (Svg.px x2)
                , SvgA.y2 (Svg.px y2)
                , SvgA.stroke (Svg.Paint label.color)
                , SvgA.strokeWidth (Svg.px 9)
                , SvgA.strokeLinecap Svg.StrokeLinecapRound
                ]
                []
        )


plantBlossomsOnlyUi : Plant -> Svg GameEvent
plantBlossomsOnlyUi =
    plantBlossomsOnlyUiFrom Tree.Path.atTrunk


plantBlossomsOnlyUiFrom : TreePath -> Plant -> Svg GameEvent
plantBlossomsOnlyUiFrom path =
    \plant ->
        [ plant
            |> Tree.children
            |> List.indexedMap
                (\subIndex sub ->
                    sub |> plantBlossomsOnlyUiFrom (path |> Tree.Path.toChild subIndex)
                )
            |> Svg.g
                [ SvgA.transform
                    [ Point2d.origin
                        |> Point2d.translateBy (plant |> Tree.label |> .orientation)
                        |> pointToTranslateTransform
                    ]
                ]
        , case plant |> Tree.label |> .blossom of
            Nothing ->
                [] |> Svg.g []

            Just blossom ->
                blossomUi path blossom
                    |> List.singleton
                    |> Svg.g
                        [ SvgA.transform
                            [ Point2d.origin
                                |> Point2d.translateBy (plant |> Tree.label |> .orientation |> Vector2d.half)
                                |> pointToTranslateTransform
                            ]
                        ]
        ]
            |> Svg.g []


blossomRadius : Quantity Float Pixels
blossomRadius =
    lightRayRadius |> Quantity.multiplyBy 0.5


blossomUi : TreePath -> Blossom -> Svg GameEvent
blossomUi path =
    \blossom ->
        blossom
            |> blossomUiWith
                [ TypedSvg.Events.onMouseDown (PlantBlossomPressed path) ]


blossomUiWith attrs =
    \blossom ->
        [ Svg.ellipse
            (attrs
                ++ [ SvgA.fill (Svg.Paint blossom.color)
                   , SvgA.rx (Svg.px (0.75 * (blossomRadius |> Pixels.toFloat)))
                   , SvgA.ry (Svg.px (1.7 * (blossomRadius |> Pixels.toFloat)))
                   ]
            )
            []
        , Svg.ellipse
            (attrs
                ++ [ SvgA.fill (Svg.Paint blossom.color)
                   , SvgA.ry (Svg.px (0.75 * (blossomRadius |> Pixels.toFloat)))
                   , SvgA.rx (Svg.px (1.7 * (blossomRadius |> Pixels.toFloat)))
                   ]
            )
            []
        ]
            |> Svg.g []


freeBlossomUi : List (TypedSvg.Core.Attribute event) -> FreeBlossom -> Svg event
freeBlossomUi attrs =
    \freeBlossom ->
        { color = freeBlossom.color }
            |> blossomUiWith attrs
            |> List.singleton
            |> Svg.g
                [ SvgA.transform
                    [ freeBlossom |> .point |> pointToTranslateTransform ]
                ]


withAlpha : Float -> Color -> Color
withAlpha newAlpha =
    \color ->
        let
            oldRgba =
                color |> Color.toRgba
        in
        Color.fromRgba { oldRgba | alpha = newAlpha }


cameraToTransform : Frame2d Pixels Float { defines : Float } -> List Svg.Transform
cameraToTransform frame =
    [ Svg.Translate
        (frame |> Frame2d.originPoint |> Point2d.xCoordinate |> Quantity.negate |> Pixels.toFloat)
        (frame |> Frame2d.originPoint |> Point2d.yCoordinate |> Quantity.negate |> Pixels.toFloat)
    ]


pointToTranslateTransform : Point2d Pixels Float -> Svg.Transform
pointToTranslateTransform =
    \point ->
        Svg.Translate
            (point |> Point2d.xCoordinate |> Pixels.toFloat)
            (point |> Point2d.yCoordinate |> Pixels.toFloat)


accentColor : Color
accentColor =
    Color.rgb 0.05 0.2 0.5


backgroundUi : Svg event_
backgroundUi =
    Svg.rect
        [ SvgA.width (Svg.percent 100)
        , SvgA.height (Svg.percent 100)
        , SvgA.fill (Svg.Paint accentColor)
        ]
        []


scoreUi :
    { state_
        | highest : Quantity Float Pixels
    }
    -> Svg GameEvent
scoreUi =
    \state ->
        [ Svg.rect
            [ SvgA.fill (Svg.Paint (Color.rgba 0 0 0 0.5))
            , SvgA.width (Svg.percent 100)
            , SvgA.height (Svg.percent 14)
            ]
            []
        , Svg.text_
            [ SvgA.x (Svg.percent 50)
            , SvgA.y (Svg.percent 8)
            , SvgA.fontSize (Svg.percent 300)
            , SvgA.fontWeight Svg.FontWeightBolder
            , SvgA.opacity (Svg.Opacity 0.5)
            , SvgA.textAnchor Svg.AnchorMiddle
            , SvgA.fontFamily [ "monospace" ]
            , SvgA.fill (Svg.Paint (Color.rgb 1 1 1))
            ]
            [ TypedSvg.Core.text
                ([ "highest: "
                 , (state.highest |> Pixels.toFloat |> floor)
                    // 30
                    |> String.fromInt
                 , "."
                 , state.highest
                    |> Pixels.toFloat
                    |> floor
                    |> remainderBy 30
                    |> String.fromInt
                 , "m"
                 ]
                    |> String.concat
                )
            ]
        ]
            |> Svg.g []


lightRayRadius : Quantity Float Pixels
lightRayRadius =
    Pixels.float 27


axisToEndPointsInWidth width axis =
    { start =
        axis
            |> Axis2d.intersectionPoint
                (Axis2d.through
                    (Point2d.fromRecord Pixels.float { x = -width / 2, y = 0 })
                    Direction2d.positiveY
                )
            |> Maybe.withDefault Point2d.origin
    , end =
        axis
            |> Axis2d.intersectionPoint
                (Axis2d.through
                    (Point2d.fromRecord Pixels.float { x = width / 2, y = 0 })
                    Direction2d.positiveY
                )
            |> Maybe.withDefault Point2d.origin
    }


lightRayUi :
    { state_
        | windowSize : { width : Float, height : Float }
        , freeBlossoms : List FreeBlossom
        , blossomSnappedToMouse : Maybe FreeBlossom
    }
    -> LightRay
    -> Svg event_
lightRayUi state =
    \lightRay ->
        let
            lightRayInScreen =
                reflect
                    { windowSize = state.windowSize
                    , source = lightRay
                    , mirrors =
                        consJust state.blossomSnappedToMouse
                            state.freeBlossoms
                            |> List.map (\freeBlossom -> freeBlossom.point)
                    }
                    |> List.map (Point2d.toTuple Pixels.toFloat)
        in
        [ Svg.polyline
            [ SvgA.points
                lightRayInScreen
            , SvgA.stroke (Svg.Paint (Color.rgba 1 0.9 0.8 0.25))
            , SvgA.strokeWidth (Svg.px (lightRayRadius |> Quantity.twice |> Pixels.toFloat))
            , SvgA.fill (Svg.Paint (Color.rgba 0 0 0 0))
            , SvgA.strokeLinejoin Svg.StrokeLinejoinRound
            ]
            []
        , Svg.polyline
            [ SvgA.points
                lightRayInScreen
            , SvgA.stroke (Svg.Paint (Color.rgba 1 0.9 0.8 0.1))
            , SvgA.strokeWidth (Svg.px (lightRayRadius |> Quantity.multiplyBy 5 |> Pixels.toFloat))
            , SvgA.fill (Svg.Paint (Color.rgba 0 0 0 0))
            , SvgA.strokeLinejoin Svg.StrokeLinejoinRound
            ]
            []
        , Svg.polyline
            [ SvgA.points
                lightRayInScreen
            , SvgA.stroke (Svg.Paint (Color.rgba 1 0.9 0.8 0.02))
            , SvgA.strokeWidth (Svg.px (lightRayRadius |> Quantity.multiplyBy 15 |> Pixels.toFloat))
            , SvgA.fill (Svg.Paint (Color.rgba 0 0 0 0))
            , SvgA.strokeLinejoin Svg.StrokeLinejoinRound
            ]
            []
        ]
            |> Svg.g []


audio : AudioData -> State -> Audio.Audio
audio audioData =
    \state ->
        case state.specific of
            Menu _ ->
                Audio.silence

            Game gameState ->
                audioWith state.shared.audio.music
                    (\music ->
                        let
                            musicLength =
                                music |> Audio.length audioData

                            -- loop
                            startTime =
                                Duration.addTo
                                    gameState.initialTime
                                    (musicLength |> Quantity.multiplyBy (alreadyCompletedLoops |> toFloat))

                            alreadyCompletedLoops =
                                (Duration.from gameState.initialTime gameState.lastTick
                                    |> Duration.inMilliseconds
                                    |> floor
                                )
                                    // (musicLength |> Duration.inMilliseconds |> floor)
                        in
                        Audio.audio music startTime
                    )
                    :: (audioKinds
                            |> List.map
                                (\audioKind ->
                                    audioWith (state.shared.audio |> accessAudioOfKind audioKind)
                                        (\loadedAudio ->
                                            gameState.audioTimes
                                                |> accessAudioOfKind audioKind
                                                |> List.map
                                                    (\time -> Audio.audio loadedAudio (Duration.addTo time (Duration.seconds 0.07)))
                                                |> Audio.group
                                        )
                                )
                       )
                    |> Audio.group


audioWith source with =
    case source of
        Err _ ->
            Audio.silence

        Ok loadedAudio ->
            with loadedAudio



-- helpers


uiToColor : Ui.Color -> Color
uiToColor =
    \uiColor ->
        Color.fromRgba (uiColor |> Ui.toRgb)


colorToUi : Color -> Ui.Color
colorToUi =
    \color ->
        Ui.fromRgb (color |> Color.toRgba)


onJust : Maybe a -> Maybe a -> Maybe a
onJust ifNothing =
    \maybe ->
        case maybe of
            Nothing ->
                ifNothing

            Just exists ->
                Just exists


consJust : Maybe a -> List a -> List a
consJust maybeHead list =
    case maybeHead of
        Nothing ->
            list

        Just head ->
            head :: list


halfLineToAxis =
    \vector ->
        Axis2d.through vector.originPoint vector.direction


type alias HalfLine =
    { direction : Direction2d Float, originPoint : Point2d Pixels Float }


untilWindowBounds : { width : Float, height : Float } -> HalfLine -> Point2d Pixels Float
untilWindowBounds windowSize =
    \halfLine ->
        halfLine
            |> halfLineToAxis
            |> Axis2d.intersectionPoint
                (Axis2d.y
                    |> Axis2d.translateBy
                        (Vector2d.fromRecord Pixels.float
                            { x =
                                if (halfLine.direction |> Direction2d.xComponent) > 0 then
                                    windowSize.width / 2 + 50

                                else
                                    -windowSize.width / 2 - 50
                            , y = 0
                            }
                        )
                )
            |> Maybe.withDefault halfLine.originPoint


reflectThreshold : Quantity Float Pixels
reflectThreshold =
    lightRayRadius |> Quantity.plus (blossomRadius |> Quantity.multiplyBy 1.45)


reflect :
    { windowSize : { width : Float, height : Float }
    , source : LightRay
    , mirrors : List (Point2d Pixels Float)
    }
    -> List (Point2d Pixels Float)
reflect { windowSize, source, mirrors } =
    let
        sourceEndPoint =
            source.axis |> untilWindowBounds windowSize

        distanceToStart mirror =
            Vector2d.from source.axis.originPoint mirror.pointOnLine
                |> vector2dLengthSquared
                |> Pixels.toFloat

        addHitInfo mirrorPoint =
            let
                hitInfo =
                    onLineSegment2dClosestTo mirrorPoint
                        (LineSegment2d.fromEndpoints ( source.axis.originPoint, sourceEndPoint ))
            in
            { point = mirrorPoint
            , distance = hitInfo.distance
            , pointOnLine = hitInfo.pointOnLine
            }

        isHit mirror =
            mirror.distance |> Quantity.lessThanOrEqualTo reflectThreshold
    in
    case mirrors |> List.map addHitInfo |> List.sortBy distanceToStart |> List.Extra.find isHit of
        Nothing ->
            [ source.axis.originPoint, sourceEndPoint ]

        Just nearestHitMirror ->
            case Vector2d.from nearestHitMirror.pointOnLine nearestHitMirror.point |> Vector2d.direction of
                Nothing ->
                    [ source.axis.originPoint, nearestHitMirror.pointOnLine, source.axis.originPoint ]

                Just reflectedDirection ->
                    [ source.axis.originPoint, nearestHitMirror.pointOnLine ]
                        ++ reflect
                            { windowSize = windowSize
                            , source =
                                { axis =
                                    { originPoint = nearestHitMirror.pointOnLine
                                    , direction =
                                        angleLerp
                                            (source.axis.direction |> Direction2d.toAngle |> Angle.normalize)
                                            (reflectedDirection |> Direction2d.toAngle |> Angle.normalize)
                                            ((nearestHitMirror.distance |> Pixels.toFloat)
                                                / (reflectThreshold |> Pixels.toFloat)
                                            )
                                            |> Direction2d.fromAngle
                                    }
                                }
                            , mirrors = mirrors |> List.filter (\mirror -> mirror /= nearestHitMirror.point)
                            }


angleLerp : Angle -> Angle -> Float -> Angle
angleLerp a b fraction =
    degreesLerp (a |> Angle.inDegrees) (b |> Angle.inDegrees) fraction
        |> Angle.degrees


degreesLerp : Float -> Float -> Float -> Float
degreesLerp a b fraction =
    if abs (b - a) > 180 then
        if b > a then
            degreesLerp (a + 360) b fraction

        else
            degreesLerp a (b + 360) fraction

    else
        (a + ((b - a) * fraction))
            |> floatModBy 360


floatModBy : Int -> Float -> Float
floatModBy divisor =
    \float ->
        let
            floatTruncated =
                truncate float

            floatUntruncated =
                float - (floatTruncated |> toFloat)
        in
        (floatTruncated |> modBy divisor |> toFloat) + floatUntruncated


isLeftOfLineSegment :
    LineSegment2d Pixels Float
    -> Point2d Pixels Float
    -> Bool
isLeftOfLineSegment lineSegment point =
    let
        a =
            lineSegment |> LineSegment2d.startPoint |> Point2d.toRecord Pixels.toFloat

        b =
            lineSegment |> LineSegment2d.endPoint |> Point2d.toRecord Pixels.toFloat

        c =
            point |> Point2d.toRecord Pixels.toFloat
    in
    ((b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)) > 0


onLineSegment2dClosestTo :
    Point2d Pixels Float
    -> LineSegment2d Pixels Float
    ->
        { distance : Quantity Float Pixels
        , pointOnLine : Point2d Pixels Float
        }
onLineSegment2dClosestTo point lineSegment =
    if point |> isLeftOfLineSegment lineSegment then
        onLineSegment2dClosestTo point (LineSegment2d.from (lineSegment |> LineSegment2d.endPoint) (lineSegment |> LineSegment2d.startPoint))

    else
        let
            distance =
                let
                    startToPoint : Vector2d Pixels Float
                    startToPoint =
                        Vector2d.from (lineSegment |> LineSegment2d.startPoint) point

                    lineSegmentVector : Vector2d Pixels Float
                    lineSegmentVector =
                        Vector2d.from (lineSegment |> LineSegment2d.startPoint) (lineSegment |> LineSegment2d.endPoint)

                    fractionAlongLineSegment =
                        (startToPoint |> Vector2d.xComponent)
                            |> Quantity.times (lineSegmentVector |> Vector2d.xComponent)
                            |> Quantity.plus
                                ((startToPoint |> Vector2d.yComponent)
                                    |> Quantity.times (lineSegmentVector |> Vector2d.yComponent)
                                )
                            |> Quantity.over (lineSegmentVector |> vector2dLengthSquared)
                            |> Quantity.clamp (Pixels.float 0) (Pixels.float 1)

                    lineSegmentFraction : Vector2d Pixels Float
                    lineSegmentFraction =
                        lineSegmentVector |> Vector2d.scaleBy (fractionAlongLineSegment |> Pixels.toFloat)

                    fractionEndPoint : Point2d Pixels Float
                    fractionEndPoint =
                        lineSegment |> LineSegment2d.startPoint |> Point2d.translateBy lineSegmentFraction

                    distanceVector : Vector2d Pixels Float
                    distanceVector =
                        Vector2d.from fractionEndPoint point
                in
                distanceVector |> Vector2d.length
        in
        { distance = distance
        , pointOnLine =
            point
                |> Point2d.translateBy
                    (Vector2d.withLength distance
                        (lineSegment
                            |> LineSegment2d.perpendicularDirection
                            |> Maybe.withDefault (Direction2d.fromAngle (Angle.turns 0.25))
                        )
                    )
        }


vector2dLengthSquared : Vector2d Pixels Float -> Quantity Float Pixels
vector2dLengthSquared =
    \vector2d ->
        vector2d
            |> Vector2d.xComponent
            |> Quantity.squared
            |> Quantity.plus (vector2d |> Vector2d.yComponent |> Quantity.squared)
            |> Quantity.over Pixels.pixel
