module Treatz_QT
open System
open SDLUtility
open SDLGeometry
open SDLPixel
open SDLRender
open SDLKeyboard
open Logic
let fps = 60.0;
let delay_time = 1000.0 / fps;
let delay_timei = uint32 delay_time

let screenWidth = 800<px>
let screenHeight = 600<px>

let cellWidth = 5
let cellHeight = 5

let mapWidth = 160
let mapHeight = 120

//let music =  
//    let p  = new System.Media.SoundPlayer(@"..\..\..\..\images\music.wav")   
//    fun () -> p.PlayLooping()


type ControllerButton =
   | BUTTON_A = 0
   | BUTTON_B = 1
   | BUTTON_X = 2
   | BUTTON_Y = 3
   | BUTTON_BACK = 4
   | BUTTON_GUIDE = 5
   | BUTTON_START = 6
   | BUTTON_LEFTSTICK = 7
   | BUTTON_RIGHTSTICK = 8
   | BUTTON_LEFTSHOULDER = 9
   | BUTTON_RIGHTSHOULDER = 10
   | BUTTON_DPAD_UP = 11
   | BUTTON_DPAD_DOWN = 12
   | BUTTON_DPAD_LEFT = 13
   | BUTTON_DPAD_RIGHT = 14

//type GameState =
//    TitleScreen 
//    | P1Win
//    | P2Win
//    | Playing
//    | Nope // both Splat (show splat?)
 

let createRect (x:int) (y:int) (w:int) (h:int) = 
    {X = x * 1<px>; Y =  y * 1<px>; Width = w * 1<px>; Height = h * 1<px>}

let inline bresenham fill (x0, y0) (x1, y1) =
    let steep = abs(y1 - y0) > abs(x1 - x0)
    let x0, y0, x1, y1 =
        if steep then y0, x0, y1, x1 else x0, y0, x1, y1
    let x0, y0, x1, y1 =
        if x0 > x1 then x1, y1, x0, y0 else x0, y0, x1, y1
    let dx, dy = x1 - x0, abs(y1 - y0)
    let s = if y0 < y1 then 1 else -1
    let rec loop e x y =
        if x <= x1 then
            if steep then fill y x else fill x y
            if e < dy then
                loop (e-dy+dx) (x+1) (y+s)
            else
                loop (e-dy) (x+1) y
    loop (dx/2) x0 y0
    
type TreatzState =
    { PressedKeys : Set<ScanCode> 
      Chaos : System.Random 
      mutable GameState : Game
      textures : Map<string, SDLTexture.Texture> 
      Controllers : Set<ControllerButton> * Set<ControllerButton>
      Sprites:     Map<byte,Rectangle>; }

let treeDepth = 15
type RenderingContext =
    {Renderer:SDLRender.Renderer;
     Texture:SDLTexture.Texture;
     Surface:SDLSurface.Surface;
     mutable lastFrameTick : uint32 }

let update (state:TreatzState) : TreatzState =
    let pressed (code:ScanCode) = if state.PressedKeys.Contains code then true else false
    let update (scancode, f) state = if pressed scancode then f state else state

    
    match state.GameState.State with
    | GameState.GameOver -> 
        if pressed ScanCode.Return then            
            state.GameState <- StartGame()
            state
        else 
            state

    | _ -> 
        
        state.GameState <- Logic.update state.GameState
        state
    

let rec eventPump (renderHandler:'TState->unit) (eventHandler:SDLEvent.Event->'TState->'TState option) (update:'TState->'TState) (state:'TState) : unit =
    match SDLEvent.pollEvent() with
    | Some event ->
        match state |> eventHandler event with
        | Some newState -> eventPump renderHandler eventHandler update newState
        | None -> ()
    | None -> 
        let state = update state
        state
        |> renderHandler
        eventPump renderHandler eventHandler update state


let handleEvent (event:SDLEvent.Event) (state:TreatzState) : TreatzState option =
    match event with
    | SDLEvent.ControllerButtonDown event  ->
        if event.Which = 0 then 
            Some({ state with Controllers = Set.add (enum<ControllerButton>(int event.Button)) (fst state.Controllers), (snd state.Controllers) } )
        else
            Some({ state with Controllers = (fst state.Controllers), Set.add (enum<ControllerButton>(int event.Button))(snd state.Controllers) } )
    | SDLEvent.ControllerButtonUp event  ->
        if event.Which = 0 then 
            Some({ state with Controllers = Set.remove (enum<ControllerButton>(int event.Button)) (fst state.Controllers), (snd state.Controllers) } )
        else
            Some({ state with Controllers = (fst state.Controllers), Set.remove (enum<ControllerButton>(int event.Button))(snd state.Controllers) } )
    | SDLEvent.KeyDown keyDetails when keyDetails.Keysym.Scancode = ScanCode.Escape ->
        None
    | SDLEvent.Quit _ -> 
        None
    | SDLEvent.KeyDown keyDetails -> 
        Some( { state with PressedKeys = Set.add keyDetails.Keysym.Scancode state.PressedKeys} )
    | SDLEvent.KeyUp keyDetails -> 
        Some( { state with PressedKeys = Set.remove keyDetails.Keysym.Scancode state.PressedKeys} )
    | _ -> Some state
        
    


let render(context:RenderingContext) (state:TreatzState) =
    let blt tex dest =
        context.Renderer |> copy tex None dest |> ignore

    let bltEx tex dest angle flip =
        context.Renderer |> copyEx tex None dest angle 0 |> ignore
        
    let bltf src dest =
        context.Renderer |> copy state.textures.["font"] (Some src) (Some dest) |> ignore

    let drawString (s:string) (x,y) =
        let mutable i = 0
        for c in s do
            bltf (state.Sprites.[byte c]) ({X = (x + (i*16)) * 1<px>; Y = y * 1<px>; Width = 16<px>; Height = 16<px>}) 
            i <- i + 1
             
    // clear screen
    context.Renderer |> SDLRender.setDrawColor (0uy,0uy,0uy,0uy) |> ignore
    context.Renderer |> SDLRender.clear |> ignore
//
//    context.Surface
//    |> SDLSurface.fillRect None {Red=80uy;Green=80uy;Blue=200uy;Alpha=255uy}
//    |> ignore
//    
//    context.Surface
//    |> SDLSurface.fillRect None {Red=80uy;Green=80uy;Blue=200uy;Alpha=255uy}
//    |> ignore
//    
            
    context.Texture
    |> SDLTexture.update None context.Surface
    |> ignore
    context.Renderer |> SDLRender.copy context.Texture None None |> ignore

    match state.GameState.State with
    | Playing -> 
        ()
    | GameOver ->  
        ()

    context.Renderer |> SDLRender.setDrawColor (200uy,255uy,50uy,0uy) |> ignore
    context.Renderer |> SDLRender.drawRect (createRect 0 0 (int screenWidth) (int screenHeight)) |> ignore
    context.Renderer |> SDLRender.drawRect (createRect 1 1 (int screenWidth - 1) (int screenHeight - 1)) |> ignore

   
   
    context.Renderer |> SDLRender.present 
    
    // delay to lock at 60fps (we could do extra work here)
    let frameTime = getTicks() - context.lastFrameTick
    if frameTime < delay_timei then delay(delay_timei - frameTime)
    context.lastFrameTick <- getTicks()    


    


let main() = 
    use system = new SDL.System(SDL.Init.Video ||| SDL.Init.Events ||| SDL.Init.GameController)
    use mainWindow = SDLWindow.create "test" 100<px> 100<px> screenWidth screenHeight (uint32 SDLWindow.Flags.Resizable) // FULLSCREEN!
    //use mainWindow = SDLWindow.create "test" 100<px> 100<px> screenWidth screenHeight (uint32 SDLWindow.Flags.FullScreen) // FULLSCREEN!    
    use mainRenderer = SDLRender.create mainWindow -1 SDLRender.Flags.Accelerated
    use surface = SDLSurface.createRGB (screenWidth,screenHeight,32<bit/px>) (0x00FF0000u,0x0000FF00u,0x000000FFu,0x00000000u)    
    use mainTexture = mainRenderer |> SDLTexture.create SDLPixel.RGB888Format SDLTexture.Access.Streaming (screenWidth,screenHeight)
    mainRenderer |> SDLRender.setLogicalSize (screenWidth,screenHeight) |> ignore
    
    SDLGameController.gameControllerOpen 0
    SDLGameController.gameControllerOpen 1
    
    let context =  { Renderer = mainRenderer; Texture = mainTexture; Surface = surface; lastFrameTick = getTicks() }
    
    // create default state
    let setKey bitmap colour =    
        bitmap
        |> SDLSurface.setColorKey (Some colour)
        |> ignore   

    let state = 
        let magenta = {Red=255uy;Green=0uy;Blue=255uy;Alpha=0uy}
        let loadTex file =
            use bmp = SDLSurface.loadBmp SDLPixel.RGB888Format file
            setKey bmp magenta
            SDLTexture.fromSurface mainRenderer bmp.Pointer
        
        //use tittleScreenBitmap = SDLSurface.loadBmp SDLPixel.RGB888Format @"..\..\..\..\images\title.bmp"
        let tex = 
            [
                ("font", loadTex @"..\..\..\..\images\romfont8x8.bmp")           
            ] |> Map.ofList
        
        use bitmap = SDLSurface.loadBmp SDLPixel.RGB888Format @"..\..\..\..\images\romfont8x8.bmp"

        bitmap
        |> SDLSurface.setColorKey (Some {Red=255uy;Green=255uy;Blue=255uy;Alpha=0uy})
        |> ignore

        
        let sprites = 
            [0uy..255uy]
            |> Seq.map(fun index-> (index, ( {X=8<px>*((index |> int) % 16); Y=8<px>*((index |> int) / 16);Width=8<px>;Height=8<px>})))
            |> Map.ofSeq
   
        {Chaos = System.Random()
         PressedKeys = Set.empty
         Sprites = sprites
         GameState = StartGame()
         textures = tex
         Controllers = Set.empty, Set.empty
         }
    

    eventPump (render context) handleEvent update state

main()