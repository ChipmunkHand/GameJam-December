module Logic
open SDLUtility
open System
open SDLGeometry

let chaos = System.Random(DateTime.Now.Millisecond)
let mutable startTime = DateTime.Now
let screenWidth = 800.0
let screenHeight = 600.0

let public createRect (x:int) (y:int) (w:int) (h:int) = 
    {X = x * 1<px>; Y =  y * 1<px>; Width = w * 1<px>; Height = h * 1<px>}


//let playChomp =  
//    let p  = new System.Media.SoundPlayer(@"..\..\..\..\images\apple.wav")   
//    fun () -> p.Play()

type GameState =
    | Playing
    | GameOver

type Location =
    {
        mutable x : int
        mutable y : int
    }
   
// two types of finishing - one is a straight line 
// where we must create the two rectanlges and find the smallest.
// the other is an explicit rectangle made of three points which is
// always the smallest 
          
//
//let createSplitRectFromLocs (start:Location) (fin:Location) =
//    
//
//    createRect x y w h

let createRectFromLocs (a:Location) (b:Location) (c:Location) (d:Location) =
    // for the explict rect we must find the top left and bottom right corner of it     
    let mutable topLeft = a
    let mutable bottomRight = a
    if b.y <= topLeft.y && b.x <= topLeft.x then topLeft <- b
    if c.y <= topLeft.y && c.x <= topLeft.x then topLeft <- c
    if d.y <= topLeft.y && d.x <= topLeft.x then topLeft <- d

    if b.y >= bottomRight.y && b.x >= bottomRight.x then bottomRight <- b
    if c.y >= bottomRight.y && c.x >= bottomRight.x then bottomRight <- c
    if d.y >= bottomRight.y && d.x >= bottomRight.x then bottomRight <- d

    createRect topLeft.x topLeft.y (abs (bottomRight.x - topLeft.x)) (abs (bottomRight.y - topLeft.y ))
    
         

type Direction = 
    | Up | Down | Left | Right
           
type Game =
    {
        mutable State : GameState
        mutable PlayerLocation : Location;
        mutable Borders : ResizeArray<Rectangle>
        mutable LineSegments : ResizeArray<Location>
        mutable LastDir : Direction
    }



let StartGame() =
    startTime <- DateTime.Now
    let state =     
        {            
            State = Playing
            Borders = ResizeArray<_>()
            LineSegments = ResizeArray<_>()
            PlayerLocation = { x = 0; y = 00}
            LastDir = Up
        }
    state

let overlap(rectA, rectB) =
    let x1 = rectA.X
    let x2 = rectA.X + rectA.Width
    let y1 = rectA.Y
    let y2 = rectA.Y + rectA.Height

    let x1' = rectB.X
    let x2' = rectB.X + rectB.Width
    let y1' = rectB.Y
    let y2' = rectB.Y + rectB.Height

    x2' >= x1 && x1' <= x2 && y2' >= y1 && y1' <= y2

let update (state:Game) (pressed:SDLKeyboard.ScanCode->bool) =
    match state.State with
    | GameOver -> 
        state
    | _ ->
        let onBorder (loc:Location) = 
            loc.x <= 0 || loc.y <= 0 || loc.x >= (int screenWidth) || loc.y >= (int screenHeight)
        let calculateBorder() =
//            if state.LineSegments.Count = 2 then
//                // split, find other two points
                  // determine horiz or vert split
                  
//                ()
//            elif state.LineSegments.Count = 3 then 
//                // corner, find otherp point
//                ()
            //if state.LineSegments.Count = 4 then
                // explict rectangle
                createRectFromLocs state.LineSegments.[0] state.LineSegments.[1] state.LineSegments.[2] state.LineSegments.[3]


        let update oldLocation newLocation changed =
            match onBorder oldLocation, onBorder newLocation, changed with
            | true, false, _ -> 
                // staring new lines
                state.LineSegments.Clear();
                state.LineSegments.Add(oldLocation)
            | false, false, true -> 
                state.LineSegments.Add(oldLocation)
            | false, true, _ -> 
                // segmentds ended, add 
                // assume single for now
                state.LineSegments.Add(newLocation)
                state.Borders.Add(calculateBorder())
            | _ -> ()

        if pressed SDLKeyboard.ScanCode.Right then 
            let oldLoc = {x = state.PlayerLocation.x; y = state.PlayerLocation.y} : Location
            state.PlayerLocation.x <- state.PlayerLocation.x + 10
            update oldLoc state.PlayerLocation (state.LastDir <> Direction.Right) 
            state.LastDir <- Direction.Right
        elif pressed SDLKeyboard.ScanCode.Left then 
            let oldLoc = {x = state.PlayerLocation.x; y = state.PlayerLocation.y} : Location            
            state.PlayerLocation.x <- state.PlayerLocation.x - 10
            update oldLoc state.PlayerLocation (state.LastDir <> Direction.Left)
            state.LastDir <- Direction.Left
        elif pressed SDLKeyboard.ScanCode.Up then 
            let oldLoc = {x = state.PlayerLocation.x; y = state.PlayerLocation.y} : Location            
            state.PlayerLocation.y <- state.PlayerLocation.y - 10
            update oldLoc state.PlayerLocation (state.LastDir <> Direction.Up)
            state.LastDir <- Direction.Up
        elif pressed SDLKeyboard.ScanCode.Down then 
            let oldLoc = {x = state.PlayerLocation.x; y = state.PlayerLocation.y} : Location            
            state.PlayerLocation.y <- state.PlayerLocation.y + 10
            update oldLoc state.PlayerLocation (state.LastDir <> Direction.Down)
            state.LastDir <- Direction.Down
        state
    
