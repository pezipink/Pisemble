package main

import (
	"github.com/tarm/serial"
	"log"
)

const StringCode = 1
const Number32Code = 2
const Number64Code = 3

const (
	Waiting  int = 0
	String       = 1
	Number32     = 2
	Number64     = 3
)

func main() {
	config := &serial.Config{
		Name:        "/dev/ttyUSB0",
		Baud:        115200,
		ReadTimeout: 1,
		Size:        8,
		Parity:      serial.ParityNone,
	}
	log.Printf("Starting with config %v", config)
	s, err := serial.OpenPort(config)
	if err != nil {
		log.Fatal(err)
	}

	//buf := make([]byte, 128)
	//n, err := s.Read(buf)
	//if err != nil {
	//	log.Fatal(err)
	//}
	//log.Printf("Reading: %q", buf[:n])
	state := Waiting
	for true {
		//log.Printf("## state:... %v", state)
		switch state {
		case Waiting:
			{
				log.Printf("## waiting on control code...")

				biite := make([]byte, 1)
				_, err := s.Read(biite)
				if err != nil {
					log.Fatal(err)
				}

				switch biite[0] {
				case StringCode:
					state = String
				case Number32Code:
					state = Number32
				case Number64Code:
					state = Number64
				default:
					log.Printf("unknown state code %v", biite[0])
					state = Waiting
				}

			}
		case String:
			{
				// a char slice
				str := []byte{}

				biite := make([]byte, 1)
				_, err := s.Read(biite)

				if err != nil {
					log.Fatal(err)
				}
				//printfn $"first char {char c} : {(int c):x}"

				for biite[0] != 0 {
					str = append(str, biite[0])
					_, err := s.Read(biite)
					if err != nil {
						log.Fatal(err)
					}
				}

				log.Printf("string: %v", string(str[:]))
				state = Waiting
			}
		case Number32:
			{
				biite := make([]byte, 4)
				_, err := s.Read(biite)

				result := int32(biite[0])

				result |= int32(biite[1]) << 8
				result |= int32(biite[2]) << 16
				result |= int32(biite[3]) << 24
				if err != nil {
					log.Fatal(err)
				}
				log.Printf("%08x %v %032b", result, result, result)
				state = Waiting

			}
		case Number64:
			{
				biite := make([]byte, 8)
				_, err := s.Read(biite)

				result := int64(biite[0])

				result |= int64(biite[1]) << 8
				result |= int64(biite[2]) << 16
				result |= int64(biite[3]) << 24
				result |= int64(biite[4]) << 32
				result |= int64(biite[5]) << 40
				result |= int64(biite[6]) << 48
				result |= int64(biite[7]) << 56
				if err != nil {
					log.Fatal(err)
				}
				log.Printf("%016x %v %032b", result, result, result)
				state = Waiting

			}

		}
	}
}
