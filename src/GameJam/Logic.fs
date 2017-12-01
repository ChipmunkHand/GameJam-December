module Logic
open SDLUtility
open System
open SDLGeometry

let chaos = System.Random(DateTime.Now.Millisecond)
let mutable startTime = DateTime.Now
let screenWidth = 800.0
let screenHeight = 600.0

//let playChomp =  
//    let p  = new System.Media.SoundPlayer(@"..\..\..\..\images\apple.wav")   
//    fun () -> p.Play()

type GameState =
    | Playing
    | GameOver
           
type Game =
    {
        mutable State : GameState
    }


let StartGame() =
    startTime <- DateTime.Now
    let state = 
    
        {            
            State = Playing
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

let update (state:Game) =
    match state.State with
    | GameOver -> 
        state
    | _ ->
        state
    
