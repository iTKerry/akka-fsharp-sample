namespace ActorsSample

open System
open Akka.FSharp

module Actors =

    type WorkMessage =
        | WorkA
        | WorkB
        | Stop
    
    type ActorMessage =
        | Init
        | Work of WorkMessage
    
    let mainAct (mailbox : Actor<_>) =
        
        // Stopped state
        let rec stopped () = actor {
            match! mailbox.Receive () with
            | _ ->
                printfn "Actor is already stopped!"
                return! stopped ()
        }
        
        // Working state
        let rec working () = actor {
            // Process nested message type
            let proc = function
                | WorkA ->
                    printfn "Matched WorkA"
                    working ()
                | WorkB -> 
                    printfn "Matched WorkB"
                    working ()
                | Stop  ->
                    printfn "Stop received. Switch to stopped state."
                    stopped ()
                
            match! mailbox.Receive() with
            | Work msg ->
                printfn "Received Working message:"
                return! proc msg
            | _ ->
                return! working ()
            
            return! working () 
        }
        
        // Initial state
        let rec initial () = actor {
            match! mailbox.Receive () with
            | Init ->
                printfn "Init received. Switch to working state."
                return! working ()
            | _ ->
                return! initial ()
        }
        
        // Set startup state
        initial ()
            
    
module Program =

    open Actors
    
    [<EntryPoint>]
    let main _ =
        let system = System.create "System" <| Configuration.load ()
        let mainActRef = spawn system "actor" <| mainAct
        
        mainActRef <! Init
        mainActRef <! Work WorkA 
        mainActRef <! Work WorkB
        mainActRef <! Work Stop
        mainActRef <! Init
        
        Console.Read () |> ignore
        0