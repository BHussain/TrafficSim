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
        CarSpawner : Spawner
        ChildSpawner : Spawner
        TrafficLightCars : TrafficLight
        TrafficLightChildren : TrafficLight
        Cars : List<Car>
        Children : List<Child>
    }

let InitialState()=
    {
        CarSpawner = Spawner.Ready
        ChildSpawner = Spawner.Ready
        TrafficLightCars = 
            {
                Color = Red(3.0f)
                Position = 435.0f, 223.0f
            }
        TrafficLightChildren = 
            {
                Color = Green(3.0f)
                Position = 567.0f, 118.0f
            }
        Cars = Empty
        Children = Empty
    }

let randomCar() =
  {
    Car.Position = (-100.0f, 148.0f)
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

let rec retrieveChildrenAbove (child:Child) (children:List<Child>) =
    match children with
    |Empty -> Empty
    |Node(x,xs) ->
        if(snd(child.Position)>snd(x.Position)) then
            Node(x,(retrieveChildrenAbove child xs))
        else
            retrieveChildrenAbove child xs

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
    let stopPos = 285.0f
    {
        car with Position = 
                            if noCarCollision car (retrieveCarsRight car cars) then
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
    }

let updateChild (children:List<Child>) (trafficLightColor:Color) (dt:float32) (child:Child) : Child =
    let stopPos = 250.0f
    {   
        child with Position = 
                            if noChildCollision child (retrieveChildrenAbove child children) then
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

let TurningRed (trafficLight:TrafficLight) : bool =
    match trafficLight.Color with
    | Red t -> false
    | Green t ->
        if t <= 0.0f then
            true
        else
            false

let UpdateTrafficLight (trafficLight:TrafficLight) (dt:float32) : (TrafficLight) =   
    let newColor = 
        match trafficLight.Color with
        | Red t ->
            if t <= 0.0f then
                Green(3.0f)
            else
                Red(t - dt)
        | Green t ->
            Green(t - dt)

    { trafficLight with Color = newColor}
            
let UpdateTrafficLights (trafficLights:TrafficLight*TrafficLight) (dt:float32) : (TrafficLight*TrafficLight) =
    let turningRed1 = TurningRed (fst(trafficLights))
    let turningRed2 = TurningRed (snd(trafficLights))

    if turningRed1 = true && turningRed2 = false then 
        let t1 =  
            {
                Color = Red(5.0f)
                Position = fst(trafficLights).Position
            }
        let t2 =
            {
                Color = Red(2.0f)
                Position = snd(trafficLights).Position
            }
        (t1,t2)
    elif turningRed1 = false && turningRed2 = true then 
        let t1 =  
            {
                Color = Red(2.0f)
                Position = fst(trafficLights).Position
            }
        let t2 =
            {
                Color = Red(5.0f)
                Position = snd(trafficLights).Position
            }
        (t1,t2)
    else
        let t1 = UpdateTrafficLight (fst(trafficLights)) dt
        let t2 = UpdateTrafficLight (snd(trafficLights)) dt
        (t1,t2)
   
let UpdateState (dt:float32) (gameState:GameState) =    
    let spawnCar,newCarSpawner = 
        match gameState.CarSpawner with
            | Ready ->
                if length gameState.Cars < 3 then
                    true, Cooldown 2.0f
                else
                    false, Ready
            | Cooldown t ->
                if t > 0.0f then
                    false, Cooldown(t-dt)
                else
                    false, Ready

    let spawnChild, newChildSpawner =
        match gameState.ChildSpawner with
            | Ready ->
                    if length gameState.Children < 3 then
                        true, Cooldown 1.0f
                    else
                        false, Ready
            | Cooldown t ->
                if t > 0.0f then
                    false, Cooldown(t-dt)
                else
                    false, Ready

    let trafficLights = UpdateTrafficLights (gameState.TrafficLightCars, gameState.TrafficLightChildren) dt
    {
        gameState with  CarSpawner = newCarSpawner
                        ChildSpawner = newChildSpawner
                        TrafficLightCars = fst(trafficLights)
                        TrafficLightChildren = snd(trafficLights)
                        Cars = updateCars dt gameState.Cars gameState.TrafficLightCars.Color spawnCar
                        Children = updateChildren dt gameState.Children gameState.TrafficLightChildren.Color spawnChild
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
    map drawCars gameState.Cars |> toFSharpList
  let listOfDrawableChildren =
    map drawChildren gameState.Children |> toFSharpList
  [
    {
      Drawable.Position = gameState.TrafficLightCars.Position
      Drawable.Image    = match gameState.TrafficLightCars.Color with
                          | Red t -> "red.png"
                          | Green t -> "green.png"
    }
    {
      Drawable.Position = gameState.TrafficLightChildren.Position
      Drawable.Image    = match gameState.TrafficLightChildren.Color with
                          | Red t -> "red.png"
                          | Green t -> "green.png"
    }
  ] @ listOfDrawableCars @listOfDrawableChildren
    |> Seq.ofList