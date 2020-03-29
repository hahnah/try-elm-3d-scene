module Main exposing (main)

import Acceleration
import Angle
import Axis3d
import Block3d
import Browser
import Browser.Events
import Camera3d exposing (Camera3d)
import Color
import Direction3d
import Duration
import Frame3d
import Html exposing (Html)
import Illuminance exposing (lux)
import Length exposing (Meters, meters)
import List
import Luminance
import Mass
import Physics.Body as Body exposing (Body)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.World as World exposing (World)
import Pixels exposing (pixels)
import Point3d
import Scene3d
import Scene3d.Chromaticity as Chromaticity
import Scene3d.Drawable as Drawable exposing (Drawable, Material)
import Scene3d.Exposure as Exposure
import Scene3d.Light as Light exposing (AmbientLighting, Light)
import Scene3d.Mesh as Mesh exposing (Mesh, NoTangents, NoUV, ShadowsDisabled, ShadowsEnabled, Triangles, WithNormals)
import Scene3d.Shape as Shape
import Sphere3d
import Vector3d
import Viewpoint3d


type alias Model =
    { world : World (Drawable BodyCoordinates) }


type Msg
    = Tick Float


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


view : Model -> Html Msg
view model =
    let
        drawables : List (Drawable WorldCoordinates)
        drawables =
            List.map getTransformedDrawable (World.getBodies model.world)
    in
    Scene3d.render []
        { ambientLighting = Just ambientLighting
        , lights = Scene3d.oneLight sunlight { castsShadows = True }
        , camera = camera
        , width = pixels 1024
        , height = pixels 768
        , exposure = Exposure.fromEv100 14
        , whiteBalance = Chromaticity.daylight
        }
        drawables


init : () -> ( Model, Cmd Msg )
init _ =
    ( { world = initialWorld }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { world = World.simulate (Duration.seconds (1 / 60)) model.world }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta Tick


initialWorld : World (Drawable BodyCoordinates)
initialWorld =
    let
        gravity =
            Acceleration.metersPerSecondSquared 9.80665
    in
    World.empty
        |> World.setGravity gravity Direction3d.negativeZ
        |> World.add floor
        |> World.add aluminumSphere
        |> World.add aluminumBlock


floorMesh : Mesh BodyCoordinates (Triangles WithNormals NoUV NoTangents ShadowsDisabled)
floorMesh =
    Shape.block (meters 8) (meters 8) (meters 0.2)


sphereMesh : Mesh BodyCoordinates (Triangles WithNormals NoUV NoTangents ShadowsEnabled)
sphereMesh =
    Shape.sphere { radius = meters 0.8, subdivisions = 72 }
        |> Mesh.enableShadows


blockMesh : Mesh BodyCoordinates (Triangles WithNormals NoUV NoTangents ShadowsEnabled)
blockMesh =
    Shape.block (meters 0.9) (meters 0.9) (meters 0.9)
        |> Mesh.enableShadows


floor : Body (Drawable BodyCoordinates)
floor =
    Drawable.physical whitePlastic floorMesh
        |> Body.plane
        |> Body.translateBy (Vector3d.meters 0 0 -4)


aluminumSphere : Body (Drawable BodyCoordinates)
aluminumSphere =
    Drawable.physical aluminum sphereMesh
        |> Drawable.withShadow sphereMesh
        |> Body.sphere (Sphere3d.atOrigin (meters 0.8))
        |> Body.translateBy (Vector3d.meters 0 0 -0.5)
        |> Body.setBehavior (Body.dynamic (Mass.kilograms 2.5))


aluminumBlock : Body (Drawable BodyCoordinates)
aluminumBlock =
    Drawable.physical aluminum blockMesh
        |> Drawable.withShadow blockMesh
        |> Body.block (Block3d.centeredOn Frame3d.atOrigin ( meters 0.9, meters 0.9, meters 0.9 ))
        |> Body.translateBy (Vector3d.meters 0.1 0 2)
        |> Body.setBehavior (Body.dynamic (Mass.kilograms 5))


aluminum : Material
aluminum =
    { baseColor = Color.rgb255 233 235 236, roughness = 0.6, metallic = True }


whitePlastic : Material
whitePlastic =
    { baseColor = Color.white, roughness = 0.25, metallic = False }


camera : Camera3d Meters WorldCoordinates
camera =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = Point3d.meters 0 0 -2
                , eyePoint = Point3d.meters 10 10 10
                , upDirection = Direction3d.positiveZ
                }
        , verticalFieldOfView = Angle.degrees 30
        , clipDepth = meters 0.1
        }


sunlight : Light WorldCoordinates
sunlight =
    Light.directional
        Chromaticity.daylight
        (lux 20000)
        (Direction3d.negativeZ
            |> Direction3d.rotateAround Axis3d.x (Angle.degrees -30)
        )


ambientLighting : AmbientLighting WorldCoordinates
ambientLighting =
    Light.overcast
        { zenithDirection = Direction3d.positiveZ
        , zenithLuminance = Luminance.nits 3000
        , chromaticity = Chromaticity.daylight
        }

getTransformedDrawable : Body (Drawable BodyCoordinates) -> Drawable WorldCoordinates
getTransformedDrawable body =
    Drawable.placeIn (Body.getFrame3d body) (Body.getData body)
