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
        
        let rec stopped () =
            actor {
                match! mailbox.Receive () with
                | Init ->
                    printfn "Restart stopped actor"
                    return! working ()
                | _ ->
                    printfn "Actor is already stopped!"
                    return! stopped ()
            }
        
        and working () =
            actor {
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
        
        and initial () =
            actor {
                match! mailbox.Receive () with
                | Init ->
                    printfn "Init received. Switch to working state."
                    return! working ()
                | _ ->
                    return! initial ()
            }
        
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
        mainActRef <! Work WorkA
        mainActRef <! Work Stop
        
        Console.Read () |> ignore
        0