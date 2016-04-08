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

type Child = 
    {
        Position : float32*float32
        Waiting : bool
    }

type Car = 
    {
        Position:float32*float32
        Waiting:bool
    }

let Walking (dt:float32) (child:Child) : Child =
    {
        child with Position = (fst(child.Position), snd(child.Position) - snd(child.Position) * dt * 10.0f)
    }

let Driving (dt:float32) (car:Car) :Car =
    {
        car with Position = (fst(car.Position) + fst(car.Position) * dt * 10.0f, snd(car.Position))
    }

type MovementStyle = 
    | Walking
    | Driving

type Moveable = 
    {
        Position        : float32*float32
        Waiting         : bool
        MovementStyle   : MovementStyle
    }

type GameState =
    {
        TrafficLight : TrafficLight
        Child : Child
        Car : Car
        Moveable : Moveable
    }

let InitialState()=
    {
        TrafficLight = 
            {
                Color = Red(3.0f)
                Position = 20.0f, 20.0f
            }

        Child = 
            {
                Position = (0.0f, 20.0f)
                Waiting = true
            }

        Car = 
            {
                Position = (50.0f, 0.0f)
                Waiting = true
            }

        Moveable = 
            {
                Position = (50.0f, 0.0f)
                Waiting = true
                MovementStyle = Driving
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


    
let UpdateState (dt:float32) (gameState:GameState) =
    {
        gameState with TrafficLight = UpdateTrafficLight gameState.TrafficLight dt;
                       Moveable = gameState.Moveable.MovementStyle dt gameState.Car
    }
