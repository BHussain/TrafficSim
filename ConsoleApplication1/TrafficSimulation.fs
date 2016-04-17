module TrafficSimulation

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

type Color = 
    | Red of float32
    | Green of float32

type TrafficLight =
    {
        Color : Color
        Position : float32*float32
    }

type MoveableType = 
    | Child
    | Car

type Moveable = 
    {
        Position        : float32*float32
        Waiting         : bool
        MoveableType    : MoveableType
    }

type GameState =
    {
        TrafficLight : TrafficLight
        Moveable1 : Moveable
        Moveable2 : Moveable
    }

let InitialState()=
    {
        Moveable1 = 
            {
                Position = (10.0f, 150.0f)
                Waiting = true
                MoveableType = Car
            }
        Moveable2 = 
            {
                Position = (500.0f, 400.0f)
                Waiting = true
                MoveableType = Child
            }
        TrafficLight = 
            {
                Color = Red(3.0f)
                Position = 400.0f, 220.0f
            }
    }

let UpdateTrafficLight (trafficLight:TrafficLight) (dt:float32) : TrafficLight =
    let newColor = 
        match trafficLight.Color with
        | Red t ->
            if t <= 0.0f then
                Green(3.0f)
            else
                Red(t - dt)
        | Green t ->
            if t <= 0.0f then
                Red(3.0f)
            else
                Green(t - dt)
    { trafficLight with Color = newColor }

let Walking (child:Moveable) (dt:float32) : Moveable =
    {
        child with Position = (fst(child.Position), snd(child.Position) - 1.0f)
    }

let Driving (car:Moveable) (dt:float32) : Moveable =
    {
        car with Position = (fst(car.Position) + 3.0f), snd(car.Position)
    }

let UpdateMoveable (moveable:Moveable) (trafficLight:TrafficLight) (dt:float32) =
    match trafficLight.Color with
    | Red t -> 
        moveable
    | Green t -> 
        match moveable.MoveableType with
        | Car -> Driving moveable dt
        | Child -> Walking moveable dt
   
let UpdateState (dt:float32) (gameState:GameState) =
    {
        gameState with TrafficLight = UpdateTrafficLight gameState.TrafficLight dt
                       Moveable1 = UpdateMoveable gameState.Moveable1 gameState.TrafficLight dt
                       Moveable2 = UpdateMoveable gameState.Moveable2 gameState.TrafficLight dt
    }

type Drawable = 
  {
    Position : float32*float32
    Image    : string
  }

let drawState (gameState:GameState) : seq<Drawable> =
  [
    {
      Drawable.Position = gameState.TrafficLight.Position
      Drawable.Image    = match gameState.TrafficLight.Color with
                          | Red t -> "red.png"
                          | Green t -> "green.png"
    }
    {
      Drawable.Position = gameState.Moveable1.Position
      Drawable.Image    = "car_red.png"
    }
    {
      Drawable.Position = gameState.Moveable2.Position
      Drawable.Image    = "character_black_blue.png"
    }
  ]  |> Seq.ofList