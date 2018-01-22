// Copyright (c) 2015,2016 Microsoft Corporation

#if INTERACTIVE
#r @"/home/andru/Liquid/bin/Liquid1.dll"
#r @"blackBox.dll"                     
#else
namespace Microsoft.Research.Liquid // Tell the compiler our namespace
#endif

open System                         // Open any support libraries

open Microsoft.Research.Liquid      // Get necessary Liquid libraries
open Util                           // General utilites
open Operations                     // Basic gates and operations
open Tests                          // Just gets us the RenderTest call for dumping files

module Script =                     // The script module allows for incremental loading

    /// <summary>
    /// Helper function to collect measurement statistics after performing some quantum operation, f
    /// </summary>
    /// <param name="n">Number of qubits</param>    
    /// <param name="f">Quantum operation to be applied on qubits</param>
    /// <param name="indices">Indices of qubits to be measured for statistics</param>    
    let collectStatistics (n:int) (f:Qubits -> unit) (indices:int list) =
        let k = Ket(n)
        let numOut = indices.Length
        let stats = Array.create numOut 0
        let numTrials = 1000

        for i in 1 .. numTrials do
            let qs = k.Reset(n)
            f qs
            for j in 0 .. numOut - 1 do
                M [qs.[indices.[j]]]
                stats.[j] <- stats.[j] + qs.[indices.[j]].Bit.v

        show "Statistics of the form (number of |0> outcomes, number of |1> outcomes)"
        for i in 0 .. numOut - 1 do
            show "Stats for qubit %d: (%d, %d)" indices.[i] (numTrials - stats.[i]) stats.[i]

    /// <summary>
    /// Measuring a qubit in the |+_angle>, |-_angle> basis
    /// </summary>
    /// <param name="qs">Qubits</param>
    /// <param name="angle">Measurement angle</param>    
    let rotatedMeasurement (q:Qubit) (angle:float) =
        HamiltonianGates.Rpauli -angle Z [q]
        H [q]
        M [q]


    let CNOT' (qs:Qubits) =
        CNOT qs

    let H' (qs:Qubits) =
        H qs

    let X' (qs:Qubits) =
        X qs

    let Z' (qs:Qubits) =
        Z qs

    let M' (qs:Qubits) =
        M qs

    let S' (qs:Qubits) =
        S qs


    let IIIXXXX (qs:Qubits) =
        // X 3
        CNOT' [qs.[3]; qs.[7]]
        // X 4
        CNOT' [qs.[4]; qs.[8]]
        // X 5
        CNOT' [qs.[5]; qs.[9]]
        // X 6
        CNOT' [qs.[6]; qs.[10]]
        M >< qs.[7 .. 10]

    let IXXIIXX (qs:Qubits) =
        // X 1
        CNOT' [qs.[1]; qs.[7]]
        // X 2
        CNOT' [qs.[2]; qs.[8]]
        // X 5
        CNOT' [qs.[5]; qs.[9]]
        // X 6
        CNOT' [qs.[6]; qs.[10]]
        M >< qs.[7 .. 10]

    let XIXIXIX (qs:Qubits) =
        // X 0
        CNOT' [qs.[0]; qs.[7]]
        // X 2
        CNOT' [qs.[2]; qs.[8]]
        // X 4
        CNOT' [qs.[4]; qs.[9]]
        // X 6
        CNOT' [qs.[6]; qs.[10]]
        M >< qs.[7 .. 10]


    let IIIZZZZ (qs:Qubits) =
        // Z 3
        H' [qs.[3]]
        CNOT' [qs.[3]; qs.[7]]
        H' [qs.[3]]        
        // Z 4
        H' [qs.[4]]
        CNOT' [qs.[4]; qs.[8]]
        H' [qs.[4]]
        // Z 5
        H' [qs.[5]]
        CNOT' [qs.[5]; qs.[9]]
        H' [qs.[5]]
        // Z 6
        H' [qs.[6]]
        CNOT' [qs.[6]; qs.[10]]
        H' [qs.[6]]
        M >< qs.[7 .. 10]

    let IZZIIZZ (qs:Qubits) =
        // Z 1
        H' [qs.[1]]
        CNOT' [qs.[1]; qs.[7]]
        H' [qs.[1]]
        // Z 2
        H' [qs.[2]]
        CNOT' [qs.[2]; qs.[8]]
        H' [qs.[2]]
        // Z 5
        H' [qs.[5]]
        CNOT' [qs.[5]; qs.[9]]
        H' [qs.[5]]
        // Z 6
        H' [qs.[6]]
        CNOT' [qs.[6]; qs.[10]]
        H' [qs.[6]]
        M >< qs.[7 .. 10]

    let ZIZIZIZ (qs:Qubits) =
        // Z 0
        H' [qs.[0]]
        CNOT' [qs.[0]; qs.[7]]
        H' [qs.[0]]
        // Z 2
        H' [qs.[2]]
        CNOT' [qs.[2]; qs.[8]]
        H' [qs.[2]]
        // Z 4
        H' [qs.[4]]
        CNOT' [qs.[4]; qs.[9]]
        H' [qs.[4]]
        // Z 6
        H' [qs.[6]]
        CNOT' [qs.[6]; qs.[10]]
        H' [qs.[6]]
        M >< qs.[7 .. 10]


    let ZZZIIII (qs:Qubits) =
        // Z 0
        H' [qs.[0]]
        CNOT' [qs.[0]; qs.[7]]
        H' [qs.[0]]
        // Z 1
        H' [qs.[1]]
        CNOT' [qs.[1]; qs.[7]]
        H' [qs.[1]]        
        // Z 2
        H' [qs.[2]]
        CNOT' [qs.[2]; qs.[8]]
        H' [qs.[2]]
        M >< qs.[7 .. 10]


    let checkAndReset (qs:Qubits) (idx:int) (len:int) =
(*        let mutable val = 0
        for i in idx .. (idx + len - 1) do
            if (qs[i].Bit.v = 1) then
                Restore qs.[i]
                X qs.[i]
                val <- val + 1
            else
*)                 
        true

    // program entry point
    [<LQD>]
    let MBQC()    =
        let k = Ket(11)
        let mutable count = 0
        let stabs = [IIIXXXX; IXXIIXX; XIXIXIX; IIIZZZZ; IZZIIZZ; ZIZIZIZ]

        while (count < stabs.Length) do
            let qs = k.Reset(11)
            count <- 0
            for i in 0 .. stabs.Length do
                let stabMeasurement = stabs.[i]
                stabMeasurement qs
                let ancillaOut = checkAndReset qs 7 4
                if (ancillaOut) then
                    show "Success on stabilizer %d" i
                    count <- count + 1

        show "State is %s" (k.ToString())                        

#if INTERACTIVE
do Script.MBQC()        // If interactive, then run the routine automatically
#endif