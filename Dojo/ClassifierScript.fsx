open System
open System.IO

type Image = int[]
type Sample = { Number : int; Image : Image  }
type SampleDistance = { Label : int; Distance : float }

let removeHeader (lines:string[]) =
    lines.[1..]
    
let extractSample (datas:int[]) =
    { Number = datas |> Array.head; Image = datas.[1..]  }
    
let extractSamples (lines:string[]) = 
    lines
    |> removeHeader
    |> Array.map (fun line -> line.Split ',')
    |> Array.map (fun line -> 
        line 
        |> Array.map (fun pixel -> (int)pixel))
    |> Array.map (fun datas -> extractSample datas)

let getValidationSamples = 
    File.ReadAllLines("./Dojo/validationsample.csv")
    |> extractSamples
    
let getTrainingSamples =
    File.ReadAllLines("./Dojo/trainingsample.csv")
    |> extractSamples

let getPixelsDistance p1 p2 =
    pown (p1 - p2) 2

let getImagesDistance (image1:Image) (image2:Image) =
    Array.map2 getPixelsDistance image1 image2
    |> Array.sum
    |> float
    |> sqrt
    
let predictNumber (image:Image) (samples:Sample[]) =
    samples
    |> Array.map (fun sample -> { Label = sample.Number; Distance = getImagesDistance image sample.Image })
    |> Array.minBy (fun sampleDistance -> sampleDistance.Distance)
    |> fun sample -> sample.Label
        
let solve =
    getValidationSamples
    |> Array.map (fun validationSample -> if validationSample.Number = predictNumber validationSample.Image getTrainingSamples then 1.0 else 0.0)
    |> Array.average
    
sprintf "Success rate : %f" solve