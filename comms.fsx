
open System
open System.IO
open System.IO.Ports

let port = new SerialPort("COM3",115200,Parity.None, 8, StopBits.One)

port.Open()

// in a waiting state, the next byte received determines the state to switch 
// to and in turn what to expect next.

type StateCode = 
    | String = 1
    | Number = 2

type State = 
    | Waiting
    | String // null terminated string
    | Number // 32bit number
    
port.ReadTimeout <- 35000
let mutable state = Waiting

while true do
    try
        match state with 
        | Waiting -> 
            printfn "## waiting on control code..."
            let b = (port.ReadByte())
            if b = (int)StateCode.String then 
                //printfn "string incoming"
                state <- String
            elif b = (int)StateCode.Number then 
                //printfn "number incoming"
                state <- Number
            else 
                printfn $"unknown state code {b}"
                state <-  Waiting
        | String -> 
            let str = new ResizeArray<char>()
            let mutable c = char(port.ReadChar())
            //printfn $"first char {char c} : {(int c):x}"
            str.Add c
            while c <> (char 0x0) do
                c <- char(port.ReadChar())
                //printfn $"read char {char c} : {(int c):x}"
                str.Add c
            printf $"{new String(str.ToArray())}"
            state <- Waiting
        | Number -> 
            //printfn "## reading number"
            // little endian
            let mutable num = port.ReadByte()
            num <- num ||| (port.ReadByte() <<< 8)
            num <- num ||| (port.ReadByte() <<< 16)
            num <- num ||| (port.ReadByte() <<< 24)
            //let mutable hnum = port.ReadByte()
            //hnum <- hnum &&& (port.ReadByte() <<< 8)
            //hnum <- hnum &&& (port.ReadByte() <<< 16)
            //hnum <- hnum &&& (port.ReadByte() <<< 24)
            //let final = ((int64 hnum) <<< 32) ||| (int64 num)
            //printf "%x | %A" final final
            
            printfn "%x | %A" num num
            state <- Waiting
    with
    | ex -> printfn $"Exception {ex} resetting to Waiting"
            state <- Waiting




//while true do 
//    let c = port.ReadChar()
//    printfn $"{c}"
//    System.Threading.Thread.Sleep 1