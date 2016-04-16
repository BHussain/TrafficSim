// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
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
        Moveable : Moveable
    }

let InitialState()=
    {
        TrafficLight = 
            {
                Color = Red(3.0f)
                Position = 20.0f, 20.0f
            }

        Moveable = 
            {
                Position = (50.0f, 50.0f)
                Waiting = true
                MoveableType = Car
            }
    }

let UpdateTrafficLight (trafficLight:TrafficLight) (dt:float32) : TrafficLight =
    let newColor = 
        match trafficLight.Color with
        | Red t ->
            if t <= 0.0f then
                Green(1.0f)
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
        child with Position = (fst(child.Position), snd(child.Position) - snd(child.Position) * dt * 10.0f)
    }

let Driving (car:Moveable) (dt:float32) : Moveable =
    {
        car with Position = (fst(car.Position) + fst(car.Position) * dt * 10.0f, snd(car.Position))
    }

let UpdateMoveable (moveable:Moveable) (dt:float32) =
    match moveable.MoveableType with
    | Car -> Driving moveable dt
    | Child -> Walking moveable dt
   
let UpdateState (dt:float32) (gameState:GameState) =
    {
        gameState with TrafficLight = UpdateTrafficLight gameState.TrafficLight dt
                       Moveable = UpdateMoveable gameState.Moveable dt
    }