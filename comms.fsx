#r @"C:\Users\juan\.nuget\packages\system.memory\4.5.5\lib\net461\System.Memory.dll"
#r @"C:\repos\Disarm\Disarm\bin\Debug\netstandard2.0\Disarm.dll"

open System
open System.IO
open System.IO.Ports

open Disarm

//Disassembler.Disassemble(ResizeArray [| 0xa0630158u|], uint64 0)
//Disassembler.Disassemble(ResizeArray [| 0x580163A0u|], uint64 0)

let port = new SerialPort("COM3",115200,Parity.None, 8, StopBits.One)
port.Open()

// in a waiting state, the next byte received determines the state to switch 
// to and in turn what to expect next.

type StateCode = 
    | String = 1
    | Number32 = 2
    | Number64 = 3
    | Disassemble = 4

type State = 
    | Waiting
    | String // null terminated string
    | Number32 // 32bit number
    | Number64 
    | Disassemble // memory location followed by 32bit instructions
    
port.ReadTimeout <- 135000
let mutable state = Waiting
printfn "## listening..."
while true do
    let read32() =
        let mutable num = port.ReadByte()
        num <- num ||| (port.ReadByte() <<< 8)
        num <- num ||| (port.ReadByte() <<< 16)
        num <- num ||| (port.ReadByte() <<< 24)       
        num
    
    let read64() = 
        let mutable num = uint64(port.ReadByte())
        num <- num ||| (uint64(port.ReadByte()) <<< 8)
        num <- num ||| (uint64(port.ReadByte())<<< 16)
        num <- num ||| (uint64(port.ReadByte()) <<< 24)
        let mutable hnum = uint64(port.ReadByte())
        hnum <- hnum ||| (uint64(port.ReadByte()) <<< 8)
        hnum <- hnum ||| (uint64(port.ReadByte())<<< 16)
        hnum <- hnum ||| (uint64(port.ReadByte()) <<< 24)
        let final = ((uint64 hnum) <<< 32) ||| (uint64 num)
        final

     

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
            elif b = (int)StateCode.Disassemble then 
                printfn "Disassemble incoming"
                state <- Disassemble

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
            let num = read32()            
            printfn "%08x | %A" num num
            state <- Waiting
        | Number64 -> 
            //printfn "## reading number"
            // little endian
            let final = read64()        
            printfn "%016x | %A" final final
            state <- Waiting
        | Disassemble -> 
            printfn "## reading disassembly location"            
            let startLocation = read64()
            printfn $"## start location is {startLocation}"
            printfn "## reading disassembly instruction count"
            let instructionCount = read32()
            printfn $"## instruction count {instructionCount}"

            // now read instructions
            let code =
                [|
                    for x in 0.. instructionCount - 1  do                         
                        let num = uint32 (read32())
                        //System.Buffers.Binary.BinaryPrimitives.ReverseEndianness num
                        num
                |]
            printfn $"code len {code.Length}"
            //for c in code do                 
            //    printfn $"{c}"
            let disass = Disarm.Disassembler.Disassemble(ResizeArray code, startLocation, true, true, false);
            for ins in disass.Instructions do
                printfn $"{ins.ToString()}"
            state <- Waiting
            
    with
    | :? System.TimeoutException  -> 
        state <- Waiting
    | ex -> printfn $"Exception {ex} resetting to Waiting"
            state <- Waiting




//while true do 
//    let c = (uint8 (port.ReadByte()))
//    printfn $"{c} : {c:x}"
//    System.Threading.Thread.Sleep 1