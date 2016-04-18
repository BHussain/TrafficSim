module TrafficSimulation

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

type Spawner =
    | Cooldown of float32
    | Ready

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

//type MoveableType = 
//    | Child
//    | Car

type Child =
    {
        Position        : float32*float32
        Waiting         : bool
        Velocity        : float32
    }

type Car =
    {
        Position        : float32*float32
        Waiting         : bool
        Velocity        : float32
    }

type GameState =
    {
        carSpawner : Spawner
        childSpawner : Spawner
        TrafficLight : TrafficLight
        cars : List<Car>
        children : List<Child>
    }

let InitialState()=
    {
        carSpawner = Spawner.Ready
        childSpawner = Spawner.Ready
        TrafficLight = 
            {
                Color = Red(3.0f)
                Position = 400.0f, 220.0f
            }
        cars = Empty
        children = Empty
    }

let randomCar() =
  {
    Car.Position = (10.0f, 150.0f)
    Car.Waiting = false;
    Car.Velocity = 3.0f;
  }

let randomChild() =
  {
    Child.Position = (500.0f, 400.0f)
    Child.Waiting = false;
    Child.Velocity = 1.0f;
  }

let updateChild  (dt:float32) (child:Child): Child =
    {
        child with Position = (fst(child.Position), snd(child.Position) - child.Velocity)
    }

let updateCar (dt:float32) (car:Car) : Car =
    {
        car with Position = (fst(car.Position) + car.Velocity), snd(car.Position)
    }

let updateSpawner (spawner:Spawner)(dt:float32) :Spawner =
        let r = new System.Random()
        match spawner with
        | Ready ->
            spawner
        | Cooldown t ->
            if t > 0.0f then
                Cooldown(t-dt)
            else
                Ready
    

let updateCars (dt:float32) (cars:List<Car>)(spawnCar:bool) : List<Car> =
  let cars = 
        if  spawnCar = true then
            randomCar() << cars
        else
        cars
  let cars = map (updateCar dt) cars
  cars

let updateChildren (dt:float32) (children:List<Child>)(spawnChild:bool) : List<Child> =
  let children = 
        if  spawnChild = true then
            randomChild() << children
        else
        children
  let children = map (updateChild dt) children
  children

//let updateList (dt:float32) (list:List<'a>)(canSpawn:bool) : List<'a> =
//  let children = 
//        if  canSpawn = true then
//            match list with
//            | Empty -> Empty
//            | Node(x:'a,xs:List<'a>) ->
//                match x.GetType with
//                | Child ->
//                    randomChild() << list
//                | Car ->
//                    randomCar() << list
//            randomChild() << children
//        else
//        children
//  let children = map (updateChild dt) children
//  children 

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
    
    let spawnCar,newCarSpawner = 
        match gameState.carSpawner with
            | Ready ->
                    true, Cooldown 2.0f
            | Cooldown t ->
                if t > 0.0f then
                    false, Cooldown(t-dt)
                else
                    false, Ready

    let spawnChild, newChildSpawner =
        match gameState.carSpawner with
            | Ready ->
                    true, Cooldown 1.0f
            | Cooldown t ->
                if t > 0.0f then
                    false, Cooldown(t-dt)
                else
                    false, Ready

    {
        gameState with  carSpawner = newCarSpawner
                        childSpawner = newChildSpawner
                        TrafficLight = UpdateTrafficLight gameState.TrafficLight dt
                        cars = updateCars dt gameState.cars spawnCar
                        children = updateChildren dt gameState.children spawnChild
    }

type Drawable = 
  {
    Position : float32*float32
    Image    : string
  }

 let drawCars (car:Car) : Drawable =
  {
    Drawable.Position = car.Position
    Drawable.Image    = "car_red.png"
  }

 let drawChildren (child:Child) : Drawable =
   {
    Drawable.Position = child.Position
    Drawable.Image    = "character_black_blue.png"
   }

 let drawState (gameState:GameState) : seq<Drawable> =
  let listOfDrawableCars =
    map drawCars gameState.cars |> toFSharpList
  let listOfDrawableChildren =
    map drawChildren gameState.children |> toFSharpList
  [
    {
      Drawable.Position = gameState.TrafficLight.Position
      Drawable.Image    = match gameState.TrafficLight.Color with
                          | Red t -> "red.png"
                          | Green t -> "green.png"
    }
  ] @ listOfDrawableCars @listOfDrawableChildren
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