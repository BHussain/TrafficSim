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
    Car.Position = (-100.0f, 150.0f)
    Car.Waiting = false;
    Car.Velocity = 5.0f;
  }

let randomChild() =
  {
    Child.Position = (500.0f, 500.0f)
    Child.Waiting = false;
    Child.Velocity = 2.0f;
  }

let rec retrieveCarsRight (car:Car) (cars:List<Car>)  =
    match cars with
    |Empty -> Empty
    |Node(x,xs) ->
        if(fst(car.Position)<fst(x.Position)) then
            Node(x, (retrieveCarsRight car xs))
        else
            retrieveCarsRight car xs

let rec carsRight (car:Car) (cars:List<Car>) : bool =
    match cars with
    |Empty -> false
    |Node(x,xs) ->
        if(fst(car.Position)<fst(x.Position)) then
            true
        else
            carsRight car xs

let rec noCarCollision (car:Car) (cars:List<Car>) : bool =
    match cars with
        |Empty -> true
        |Node(x,xs) ->
            if(fst(x.Position)<>fst(car.Position)) then
                if(abs(fst(x.Position)-fst(car.Position))<150.0f)then
                    false
                else
                    noCarCollision car xs
            else
                noCarCollision car xs

let rec retrieveChildrenTop (child:Child) (children:List<Child>)  =
    match children with
    |Empty -> Empty
    |Node(x,xs) ->
        if(snd(child.Position)>snd(x.Position)) then
            Node(x,(retrieveChildrenTop child xs))
        else
            retrieveChildrenTop child xs

let rec childrenTop (child:Child) (children:List<Child>) : bool =
    match children with
    |Empty -> false
    |Node(x,xs) ->
        if(snd(child.Position)>snd(x.Position)) then
            true
        else
            childrenTop child xs

let rec noChildCollision (child:Child) (children:List<Child>) : bool =
    match children with
        |Empty -> true
        |Node(x,xs) ->
            if(snd(x.Position)<>snd(child.Position)) then
                if(abs(snd(x.Position)-snd(child.Position))<50.0f)then
                    false
                else
                    noChildCollision child xs
            else
                noChildCollision child xs

let updateCar (cars:List<Car>) (trafficLightColor:Color) (dt:float32) (car:Car) : Car =
    let stopPos = 275.0f
    {
        car with Position = 
                            if carsRight car cars then
                                let x = retrieveCarsRight car cars
                                if noCarCollision car x then
                                    if fst(car.Position) > stopPos && fst(car.Position) < (stopPos + 10.0f) then
                                        match trafficLightColor with
                                            | Red t -> 
                                                    car.Position
                                            | Green t ->
                                                    (fst(car.Position) + car.Velocity), snd(car.Position)

                                    else
                                        (fst(car.Position) + car.Velocity), snd(car.Position)
                                else
                                    car.Position
                            elif fst(car.Position) > stopPos && fst(car.Position) < (stopPos + 10.0f) then
                                        match trafficLightColor with
                                            | Red t -> 
                                                    car.Position
                                            | Green t ->
                                                    (fst(car.Position) + car.Velocity), snd(car.Position)
                            else
                                (fst(car.Position) + car.Velocity), snd(car.Position)
    }

let updateChild (children:List<Child>) (trafficLightColor:Color) (dt:float32) (child:Child) : Child =
    let stopPos = 250.0f
    {   
        child with Position = 
                            if childrenTop child children then
                                let x = retrieveChildrenTop child children
                                if noChildCollision child x then
                                    if snd(child.Position) < stopPos && snd(child.Position) > (stopPos - 10.0f) then
                                        match trafficLightColor with
                                        | Red t -> 
                                            child.Position
                                        | Green t ->
                                            (fst(child.Position), snd(child.Position) - child.Velocity)
                                    else
                                    (fst(child.Position), snd(child.Position) - child.Velocity)
                                else
                                    child.Position
                            elif snd(child.Position) < stopPos && snd(child.Position) > (stopPos - 10.0f) then
                                match trafficLightColor with
                                | Red t -> 
                                    child.Position
                                | Green t ->
                                    (fst(child.Position), snd(child.Position) - child.Velocity)
                            else
                                (fst(child.Position), snd(child.Position) - child.Velocity)
    }

let updateSpawner (spawner:Spawner)(dt:float32) :Spawner =
        match spawner with
        | Ready ->
            spawner
        | Cooldown t ->
            if t > 0.0f then
                Cooldown(t-dt)
            else
                Ready    

let updateCars (dt:float32) (cars:List<Car>) (trafficLightColor:Color) (spawnCar:bool) : List<Car> =
  let cars = 
        if  spawnCar = true then
            randomCar() << cars
        else
        cars
  let cars = map (updateCar cars trafficLightColor dt) cars
  let insideScreen (c:Car) : bool =
    fst(c.Position) < 800.0f
  let cars = filter insideScreen cars
  cars

let updateChildren (dt:float32) (children:List<Child>) (trafficLightColor:Color) (spawnChild:bool) : List<Child> =
  let children = 
        if  spawnChild = true then
            randomChild() << children
        else
        children
  let children = map (updateChild children trafficLightColor dt) children
  let insideScreen (c:Child) : bool =
    snd(c.Position) > -20.0f
  let children = filter insideScreen children
  children

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
                if length gameState.cars < 3 then
                    true, Cooldown 2.0f
                else
                    false, Ready
            | Cooldown t ->
                if t > 0.0f then
                    false, Cooldown(t-dt)
                else
                    false, Ready

    let spawnChild, newChildSpawner =
        match gameState.childSpawner with
            | Ready ->
                    if length gameState.children < 3 then
                        true, Cooldown 1.0f
                    else
                        false, Ready
            | Cooldown t ->
                if t > 0.0f then
                    false, Cooldown(t-dt)
                else
                    false, Ready

    {
        gameState with  carSpawner = newCarSpawner
                        childSpawner = newChildSpawner
                        TrafficLight = UpdateTrafficLight gameState.TrafficLight dt
                        cars = updateCars dt gameState.cars gameState.TrafficLight.Color spawnCar
                        children = updateChildren dt gameState.children gameState.TrafficLight.Color spawnChild
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