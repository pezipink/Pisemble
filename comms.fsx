
open System
open System.IO
open System.IO.Ports

let port = new SerialPort("COM3",115200,Parity.None, 8, StopBits.One)

port.Open()

// in a waiting state, the next byte received determines the state to switch 
// to and in turn what to expect next.

type StateCode = 
    | String = 1
    | Number32 = 2
    | Number64 = 3

type State = 
    | Waiting
    | String // null terminated string
    | Number32 // 32bit number
    | Number64 
    
port.ReadTimeout <- 135000
let mutable state = Waiting
printfn "## listening..."
while true do
    try
        match state with 
        | Waiting -> 
//            printfn "## waiting on control code..."
            let b = (port.ReadByte())
            if b = (int)StateCode.String then 
                //printfn "string incoming"
                state <- String
            elif b = (int)StateCode.Number32 then 
                //printfn "number 32 incoming"
                state <- Number32
            
            elif b = (int)StateCode.Number64 then 
                //printfn "number 64 incoming"
                state <- Number64
            else 
                //printfn $"unknown state code {b}"
                state <-  Waiting
        | String -> 
            let str = new ResizeArray<char>()
            let mutable c = char(port.ReadChar())
            //printfn $"first char {char c} : {(int c):x}"
            while c <> (char 0x0) do
                str.Add c
                c <- char(port.ReadChar())
                //printfn $"read char {char c} : {(int c):x}"
            printf $"{new String(str.ToArray())}"
            state <- Waiting
        | Number32 -> 
            //printfn "## reading number"
            // little endian
            let mutable num = port.ReadByte()
            num <- num ||| (port.ReadByte() <<< 8)
            num <- num ||| (port.ReadByte() <<< 16)
            num <- num ||| (port.ReadByte() <<< 24)       
            printfn "%08x | %A" num num
            state <- Waiting
        | Number64 -> 
            //printfn "## reading number"
            // little endian
            let mutable num = uint64(port.ReadByte())
            num <- num ||| (uint64(port.ReadByte()) <<< 8)
            num <- num ||| (uint64(port.ReadByte())<<< 16)
            num <- num ||| (uint64(port.ReadByte()) <<< 24)
            let mutable hnum = uint64(port.ReadByte())
            hnum <- hnum ||| (uint64(port.ReadByte()) <<< 8)
            hnum <- hnum ||| (uint64(port.ReadByte())<<< 16)
            hnum <- hnum ||| (uint64(port.ReadByte()) <<< 24)
            let final = ((uint64 hnum) <<< 32) ||| (uint64 num)
            printfn "%016x | %A" final final
            state <- Waiting
    with
    | ex -> printfn $"Exception {ex} resetting to Waiting"
            state <- Waiting




//while true do 
//    let c = (uint8 (port.ReadByte()))
//    printfn $"{c} : {c:x}"
//    System.Threading.Thread.Sleep 1