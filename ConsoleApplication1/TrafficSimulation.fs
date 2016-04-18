module TrafficSimulation

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

let r = System.Random()

type List<'a> = 
  | Empty 
  | Node of 'a * List<'a>
let (<<) x xs = Node(x,xs)

let rec toFSharpList l =
  match l with
  | Empty -> []
  | Node(x,xs) -> x :: toFSharpList xs

let rec filter (p:'a->bool) (l:List<'a>) : List<'a> =
  match l with
  | Empty -> Empty
  | Node(x,xs) ->
    if p x then
      Node(x, filter p xs)
    else
      filter p xs

let rec map (f:'a->'b) (l:List<'a>) : List<'b> =
  match l with
  | Empty -> Empty
  | Node(x:'a,xs:List<'a>) -> 
    let y:'b = f x
    let ys:List<'b> = map f xs
    Node(y,ys)

let rec length (l:List<'a>) =
  match l with
  | Empty -> 0
  | Node(x,xs) -> 1 + length xs

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
        Moveables : List<Moveable>
    }

let InitialState()=
    {
        Moveables = Empty
        TrafficLight = 
            {
                Color = Red(3.0f)
                Position = 400.0f, 220.0f
            }
    }

let randomMoveable() =
  {
    Moveable.Position = (10.0f, 150.0f)
    Moveable.Waiting = false;
    Moveable.MoveableType = Car
  }

let Walking (child:Moveable) (dt:float32) : Moveable =
    {
        child with Position = (fst(child.Position), snd(child.Position) - 1.0f)
    }

let Driving (car:Moveable) (dt:float32) : Moveable =
    {
        car with Position = (fst(car.Position) + 3.0f), snd(car.Position)
    }

let UpdateMoveable (dt:float32) (moveable:Moveable) =
    match moveable.MoveableType with
    | Car -> Driving moveable dt
    | Child -> Walking moveable dt

let updateMoveables (dt:float32) (moveables:List<Moveable>) : List<Moveable> =
//  let creationProbability =
//    let l = length moveables
//    if l < 5 then
//      100
//    elif l < 15 then
//      20
//    else
//      5
  let moveables = 
    //let r = new System.Random()
    if  r.Next(0,100) < 2 then
      randomMoveable() << moveables
    else
      moveables
  let moveables = map (UpdateMoveable dt) moveables
  moveables

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
   
let UpdateState (dt:float32) (gameState:GameState) =
    {
        gameState with TrafficLight = UpdateTrafficLight gameState.TrafficLight dt
                       Moveables = updateMoveables dt gameState.Moveables
    }

type Drawable = 
  {
    Position : float32*float32
    Image    : string
  }

 let drawMoveables (m:Moveable) : Drawable =
  {
    Drawable.Position = m.Position
    Drawable.Image    = "car_red.png"
  }

let drawState (gameState:GameState) : seq<Drawable> =
  let listOfDrawableMoveables =
    map drawMoveables gameState.Moveables |> toFSharpList
  [
    {
      Drawable.Position = gameState.TrafficLight.Position
      Drawable.Image    = match gameState.TrafficLight.Color with
                          | Red t -> "red.png"
                          | Green t -> "green.png"
    }
  ] @ listOfDrawableMoveables
    |> Seq.ofList

//let drawState (gameState:GameState) : seq<Drawable> =
//  [
//    {
//      Drawable.Position = gameState.TrafficLight.Position
//      Drawable.Image    = match gameState.TrafficLight.Color with
//                          | Red t -> "red.png"
//                          | Green t -> "green.png"
//    }
//    {
//      Drawable.Position = gameState.Moveable1.Position
//      Drawable.Image    = "car_red.png"
//    }
//    {
//      Drawable.Position = gameState.Moveable2.Position
//      Drawable.Image    = "character_black_blue.png"
//    }
//  ]  |> Seq.ofList