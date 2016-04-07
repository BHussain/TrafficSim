// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
module TrafficSimulation

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

type Color = 
    | Red of float32
    | Green of float32

type trafficLight =
    {
        Color : Color
        Position : int*int
    }

type gameState =
    {
        trafficLight : trafficLight
    }

let initialState()=
    {
        trafficLight = 
        {
            Color = Red(3.0f)
            Position = 20,20
         }
    }

let updateTrafficLight (trafficLight:trafficLight) (dt:float32) : trafficLight =
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
    
let updateState (dt:float32) (gameState:gameState) =
    {
        gameState with trafficLight = updateTrafficLight gameState.trafficLight dt
    }
