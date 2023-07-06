module Physics exposing (Body, Circle, Face, Framed, Movement(..), Polygon, Shape(..), World, circle, frame, location, movable, moveTo, polygon, rectangle, shape, simulate, speed, speedAlter, what, whatReplace, worldBody, worldBodyAlter)

import Angle
import Axis2d
import Direction2d exposing (Direction2d)
import Duration exposing (Seconds)
import Frame2d exposing (Frame2d)
import LineSegment2d exposing (LineSegment2d)
import List.Extra
import Maybe.Extra
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Vector2d exposing (Vector2d)


type alias World what =
    List (Body what)


type alias Body what =
    { what : what
    , framedShape : Framed Shape
    , movement : Movement
    }


type alias Framed shape =
    RecordWithoutConstructorFunction
        { shape : shape
        , frame : Frame2d Pixels Float { defines : Float }
        }


type Movement
    = Immovable
    | Movable { speed : Vector2d (Rate Pixels Seconds) Float }


type Shape
    = Circle Circle
    | Polygon Polygon


type alias Circle =
    RecordWithoutConstructorFunction
        { radius : Quantity Float Pixels }


type alias Polygon =
    List Face


type alias Face =
    -- from start to end should face
    --   - rotating counter-clockwise = outward
    --   - rotating clockwise = inward
    LineSegment2d Pixels Float


worldBodyAlter : (Body what -> Body mappedWhat) -> (World what -> World mappedWhat)
worldBodyAlter bodyChange =
    \world ->
        world |> List.map bodyChange


worldBody :
    (what -> Maybe mappedWhat)
    -> (World what -> Maybe (Body mappedWhat))
worldBody whatTryAccess =
    \world ->
        world
            |> List.Extra.findMap
                (\body ->
                    case body.what |> whatTryAccess of
                        Nothing ->
                            Nothing

                        Just mappedWhat ->
                            body |> whatReplace mappedWhat |> Just
                )


rectangle :
    what
    -> { width : Quantity Float Pixels, height : Quantity Float Pixels }
    -> Body what
rectangle what_ rectangleDimensions =
    { what = what_
    , framedShape =
        { shape = rectanglePolygon rectangleDimensions |> Polygon
        , frame = Frame2d.atOrigin
        }
    , movement = Immovable
    }


rectanglePolygon : { width : Quantity Float Pixels, height : Quantity Float Pixels } -> Polygon
rectanglePolygon rectangleDimensions =
    let
        left : Quantity Float Pixels
        left =
            rectangleDimensions.width |> Quantity.half |> Quantity.negate

        bottom : Quantity Float Pixels
        bottom =
            rectangleDimensions.height |> Quantity.half |> Quantity.negate

        bottomLeft : Point2d Pixels Float
        bottomLeft =
            Point2d.xy left bottom

        top : Quantity Float Pixels
        top =
            rectangleDimensions.height |> Quantity.half

        topLeft : Point2d Pixels Float
        topLeft =
            Point2d.xy left top

        right : Quantity Float Pixels
        right =
            rectangleDimensions.width |> Quantity.half

        bottomRight : Point2d Pixels Float
        bottomRight =
            Point2d.xy right bottom

        topRight : Point2d Pixels Float
        topRight =
            Point2d.xy right top
    in
    [ LineSegment2d.from bottomLeft topLeft
    , LineSegment2d.from topLeft topRight
    , LineSegment2d.from topRight bottomRight
    , LineSegment2d.from bottomRight bottomLeft
    ]


polygon :
    what
    -> List (Point2d Pixels Float)
    -> Body what
polygon what_ points =
    { what = what_
    , framedShape =
        { shape = polygonWith points |> Polygon
        , frame = Frame2d.atOrigin
        }
    , movement = Immovable
    }


polygonWith : List (Point2d Pixels Float) -> Polygon
polygonWith points =
    case points of
        [] ->
            []

        point0 :: point1Up ->
            polygonFacesOpen { startPoint = point0 } ( point0, point1Up )


polygonFacesOpen :
    { startPoint : Point2d Pixels Float }
    -> ( Point2d Pixels Float, List (Point2d Pixels Float) )
    -> List Face
polygonFacesOpen config points =
    case points of
        ( lastPoint, [] ) ->
            [ LineSegment2d.from lastPoint config.startPoint ]

        ( point0, point1 :: points2Up ) ->
            LineSegment2d.from point0 point1
                :: polygonFacesOpen config ( point1, points2Up )


circle : what -> { radius : Quantity Float Pixels } -> Body what
circle what_ circle2d =
    { what = what_
    , framedShape =
        { shape = circle2d |> Circle
        , frame = Frame2d.atOrigin
        }
    , movement = Immovable
    }


whatReplace : replacementWhat -> (Body what_ -> Body replacementWhat)
whatReplace whatReplacement =
    \body ->
        { what = whatReplacement
        , framedShape = body.framedShape
        , movement = body.movement
        }


what : Body what -> what
what =
    \body -> body.what


frame : Body what_ -> Frame2d Pixels Float { defines : Float }
frame =
    \body -> body.framedShape.frame


shape : Body what_ -> Shape
shape =
    \body -> body.framedShape.shape


location : Body what_ -> Point2d Pixels Float
location =
    \body -> body |> frame |> Frame2d.originPoint


movable : Body what -> Body what
movable =
    \body ->
        case body.movement of
            Immovable ->
                { body | movement = Movable { speed = Vector2d.zero } }

            Movable _ ->
                body


speedAlter :
    (Vector2d (Rate Pixels Seconds) Float -> Vector2d (Rate Pixels Seconds) Float)
    -> (Body what -> Body what)
speedAlter speedChange =
    \body ->
        { body | movement = Movable { speed = body |> speed |> speedChange } }


speed : Body what_ -> Vector2d (Rate Pixels Seconds) Float
speed =
    \body ->
        case body.movement of
            Immovable ->
                Vector2d.zero

            Movable movement ->
                movement.speed


simulate : Quantity Float Seconds -> (World what -> { world : World what, collisionsWith : List (Body what) })
simulate sinceLastSimulation =
    \world ->
        world
            |> List.foldl
                (\body soFar ->
                    case body.movement of
                        Immovable ->
                            { soFar | world = soFar.world |> (::) body }

                        Movable movement ->
                            let
                                bodyMoved =
                                    body |> translateBy (movement.speed |> Vector2d.for sinceLastSimulation)

                                futureCollisions =
                                    world
                                        |> List.foldl
                                            (\bodyOther collisionsSoFar ->
                                                if bodyOther == body then
                                                    collisionsSoFar

                                                else
                                                    let
                                                        bodyOtherMoved =
                                                            bodyOther |> translateBy (bodyOther |> speed |> Vector2d.for sinceLastSimulation)
                                                    in
                                                    case
                                                        bodyMoved.framedShape
                                                            |> intersect bodyOtherMoved.framedShape
                                                                { towards =
                                                                    movement.speed
                                                                        |> Vector2d.direction
                                                                        |> Maybe.withDefault Direction2d.negativeY
                                                                }
                                                    of
                                                        [] ->
                                                            collisionsSoFar

                                                        collision0 :: collisions1Up ->
                                                            { intersections =
                                                                collisionsSoFar.intersections
                                                                    ++ (collision0 :: collisions1Up)
                                                            , bodies = collisionsSoFar.bodies |> (::) bodyOther
                                                            }
                                            )
                                            { intersections = [], bodies = [] }

                                newSpeed =
                                    case futureCollisions.intersections of
                                        [] ->
                                            movement.speed
                                                |> Vector2d.plus (gravity |> Vector2d.for sinceLastSimulation)

                                        collision0 :: collisions1Up ->
                                            (collision0 :: collisions1Up)
                                                |> List.foldl
                                                    (\collision reflectedSoFar ->
                                                        reflectedSoFar
                                                            |> Vector2d.reverse
                                                            |> Vector2d.mirrorAcross
                                                                (Axis2d.through collision.point collision.reflectionMirrorAxisDirection)
                                                    )
                                                    movement.speed
                            in
                            { world =
                                soFar.world
                                    |> (::)
                                        { body
                                            | movement = { speed = newSpeed } |> Movable
                                            , framedShape =
                                                { shape = body.framedShape.shape
                                                , frame =
                                                    body.framedShape.frame
                                                        |> Frame2d.translateBy
                                                            (newSpeed |> Vector2d.for sinceLastSimulation)
                                                }
                                        }
                            , collisionsWith =
                                soFar.collisionsWith ++ futureCollisions.bodies
                            }
                )
                { world = []
                , collisionsWith = []
                }


gravity : Vector2d (Rate (Rate Pixels Duration.Seconds) Duration.Seconds) Float
gravity =
    Vector2d.fromRecord Pixels.float { x = 0, y = -340 }
        |> Vector2d.per Duration.second
        |> Vector2d.per Duration.second


intersect :
    Framed Shape
    -> { towards : Direction2d Float }
    -> Framed Shape
    ->
        List
            { point : Point2d Pixels Float
            , reflectionMirrorAxisDirection : Direction2d Float
            }
intersect shapeOther towards =
    \shapeThis ->
        case ( shapeThis.shape, shapeOther.shape ) of
            ( Circle circleThis, Circle circleOther ) ->
                { shape = circleThis, frame = shapeThis.frame }
                    |> circleIntersectCircle
                        { shape = circleOther, frame = shapeOther.frame }
                    |> Maybe.Extra.toList

            ( Circle circleThis, Polygon polygonOther ) ->
                { shape = circleThis, frame = shapeThis.frame }
                    |> circleIntersectPolygon
                        { shape = polygonOther, frame = shapeOther.frame }
                        towards

            ( Polygon polygonThis, Polygon polygonOther ) ->
                { shape = polygonThis, frame = shapeThis.frame }
                    |> polygonIntersectPolygon
                        { shape = polygonOther, frame = shapeOther.frame }
                        towards

            ( Polygon polygonThis, Circle circleOther ) ->
                -- TODO
                []


circleIntersectCircle :
    Framed Circle
    -> Framed Circle
    ->
        Maybe
            { point : Point2d Pixels Float
            , reflectionMirrorAxisDirection : Direction2d Float
            }
circleIntersectCircle circleOther =
    \circle_ ->
        let
            distance =
                Vector2d.from
                    (circleOther.frame |> Frame2d.originPoint)
                    (circle_.frame |> Frame2d.originPoint)

            maximumCollisionDistance =
                circle_.shape.radius
                    |> Quantity.plus circleOther.shape.radius
        in
        if distance |> Vector2d.length |> Quantity.lessThanOrEqualTo maximumCollisionDistance then
            { point =
                Point2d.origin
                    |> Point2d.translateBy
                        (distance |> Vector2d.scaleTo circleOther.shape.radius)
            , reflectionMirrorAxisDirection =
                distance |> Vector2d.direction |> Maybe.withDefault Direction2d.positiveY
            }
                |> Just

        else
            Nothing


circleIntersectPolygon :
    Framed Polygon
    -> { towards : Direction2d Float }
    -> Framed Circle
    ->
        List
            { point : Point2d Pixels Float
            , reflectionMirrorAxisDirection : Direction2d Float
            }
circleIntersectPolygon polygonOther towards =
    \circleThis ->
        polygonOther
            |> .shape
            |> List.filterMap
                (\face ->
                    circleThis
                        |> circleIntersectFace
                            (face |> LineSegment2d.placeIn polygonOther.frame)
                            towards
                )


directionFromFaceOutward : Face -> Direction2d Float
directionFromFaceOutward face =
    case face |> LineSegment2d.direction of
        Nothing ->
            Direction2d.negativeY

        Just alongFace ->
            alongFace |> Direction2d.rotateCounterclockwise


circleIntersectFace :
    Face
    -> { towards : Direction2d Float }
    -> Framed Circle
    ->
        Maybe
            { point : Point2d Pixels Float
            , reflectionMirrorAxisDirection : Direction2d Float
            }
circleIntersectFace face towards =
    \circle_ ->
        let
            closest =
                onLineSegment2dClosestTo (circle_.frame |> Frame2d.originPoint) face

            circleIsInside =
                Vector2d.dot
                    (face |> directionFromFaceInward |> Direction2d.toVector)
                    (towards.towards |> Direction2d.toVector)
                    |> Quantity.lessThanZero
        in
        if circleIsInside then
            Nothing

        else if closest.distance |> Quantity.greaterThan circle_.shape.radius then
            Nothing

        else
            { point = closest.pointOnLine
            , reflectionMirrorAxisDirection = face |> directionFromFaceOutward
            }
                |> Just


directionFromFaceInward : Face -> Direction2d Float
directionFromFaceInward face =
    case face |> LineSegment2d.direction of
        Nothing ->
            Direction2d.negativeY

        Just alongFace ->
            alongFace |> Direction2d.rotateClockwise


onLineSegment2dClosestTo :
    Point2d Pixels Float
    -> LineSegment2d Pixels Float
    ->
        { distance : Quantity Float Pixels
        , pointOnLine : Point2d Pixels Float
        }
onLineSegment2dClosestTo point lineSegment =
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


polygonIntersectPolygon :
    Framed Polygon
    -> { towards : Direction2d Float }
    -> Framed Polygon
    ->
        List
            { point : Point2d Pixels Float
            , reflectionMirrorAxisDirection : Direction2d Float
            }
polygonIntersectPolygon polygonOther towards =
    \polygonThis ->
        polygonThis
            |> .shape
            |> List.concatMap
                (\face ->
                    face
                        |> LineSegment2d.placeIn polygonThis.frame
                        |> faceIntersectPolygon polygonOther towards
                )


faceIntersectPolygon :
    Framed Polygon
    -> { towards : Direction2d Float }
    -> Face
    ->
        List
            { point : Point2d Pixels Float
            , reflectionMirrorAxisDirection : Direction2d Float
            }
faceIntersectPolygon polygonOther towards =
    \faceThis ->
        polygonOther
            |> .shape
            |> List.filterMap
                (\polygonFace ->
                    faceThis
                        |> LineSegment2d.placeIn polygonOther.frame
                        |> faceIntersectFace polygonFace towards
                )


faceIntersectFace :
    Face
    -> { towards : Direction2d Float }
    -> Face
    ->
        Maybe
            { point : Point2d Pixels Float
            , reflectionMirrorAxisDirection : Direction2d Float
            }
faceIntersectFace faceOther towards =
    \face ->
        face
            |> LineSegment2d.intersectionPoint faceOther
            |> Maybe.map
                (\intersectionPoint ->
                    { point = intersectionPoint
                    , reflectionMirrorAxisDirection = faceOther |> directionFromFaceOutward
                    }
                )



--


frameAlter :
    (Frame2d Pixels Float { defines : Float }
     -> Frame2d Pixels Float { defines : Float }
    )
    -> (Body what -> Body what)
frameAlter frameChange =
    \body ->
        { body
            | framedShape =
                { shape = body.framedShape.shape
                , frame = body.framedShape.frame |> frameChange
                }
        }


translateBy : Vector2d Pixels Float -> (Body what -> Body what)
translateBy offset =
    \body ->
        body |> frameAlter (Frame2d.translateBy offset)


moveTo : Point2d Pixels Float -> (Body what -> Body what)
moveTo newLocation =
    \body ->
        body |> frameAlter (Frame2d.moveTo newLocation)
