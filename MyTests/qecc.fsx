// Copyright (c) 2015,2016 Microsoft Corporation

#if INTERACTIVE
#r @"/home/andru/Liquid/bin/Liquid1.dll"           
#else
namespace Microsoft.Research.Liquid // Tell the compiler our namespace
#endif

open System                         // Open any support libraries

open Microsoft.Research.Liquid      // Get necessary Liquid libraries
open Util                           // General utilites
open Operations                     // Basic gates and operations
open Tests                          // Just gets us the RenderTest call for dumping files

module Script =                     // The script module allows for incremental loading


    // p value for depolarization channels
    let p = 0.01

    // single qubit depolarization channel
    let depolarize (p:double) (qs:Qubits) =
        let rand = System.Random()
        let r = rand.NextDouble()
        let idx = rand.Next() % 3
        let errs = [X; Y; Z]
        if (r < p) then
            errs.[idx] qs

    // two qubit depolarization channel
    let depolarize2 (p:double) (qs:Qubits) =
        let rand = System.Random()
        let r = rand.NextDouble()
        let idx1 = rand.Next() % 4
        let idx2 = rand.Next() % 4
        let errs = [I; X; Y; Z]
        if (r < p) then
            errs.[idx1] [qs.[0]]
            errs.[idx2] [qs.[1]]


    // faulty quantum operations
    let CNOT' (qs:Qubits) =
        CNOT qs
        depolarize2 p qs

    let H' (qs:Qubits) =
        H qs
        depolarize p qs

    let X' (qs:Qubits) =
        X qs
        depolarize p qs

    let Z' (qs:Qubits) =
        Z qs
        depolarize p qs

    let S' (qs:Qubits) =
        S qs
        depolarize p qs

    let M' (qs:Qubits) =
        depolarize p qs
        M qs


    // stabilizers for Steane's code
    let IIIZZZZ (qs:Qubits) =
        // Z 3
        CNOT' [qs.[3]; qs.[7]]
        // Z 4
        CNOT' [qs.[4]; qs.[8]]
        // Z 5
        CNOT' [qs.[5]; qs.[9]]
        // Z 6
        CNOT' [qs.[6]; qs.[10]]
        M' >< qs.[7 .. 10]

    let IZZIIZZ (qs:Qubits) =
        // Z 1
        CNOT' [qs.[1]; qs.[7]]
        // Z 2
        CNOT' [qs.[2]; qs.[8]]
        // Z 5
        CNOT' [qs.[5]; qs.[9]]
        // Z 6
        CNOT' [qs.[6]; qs.[10]]
        M' >< qs.[7 .. 10]

    let ZIZIZIZ (qs:Qubits) =
        // Z 0
        CNOT' [qs.[0]; qs.[7]]
        // Z 2
        CNOT' [qs.[2]; qs.[8]]
        // Z 4
        CNOT' [qs.[4]; qs.[9]]
        // Z 6
        CNOT' [qs.[6]; qs.[10]]
        M' >< qs.[7 .. 10]


    let IIIXXXX (qs:Qubits) =
        // X 3
        H' [qs.[3]]
        CNOT' [qs.[3]; qs.[7]]
        H' [qs.[3]]        
        // X 4
        H' [qs.[4]]
        CNOT' [qs.[4]; qs.[8]]
        H' [qs.[4]]
        // X 5
        H' [qs.[5]]
        CNOT' [qs.[5]; qs.[9]]
        H' [qs.[5]]
        // X 6
        H' [qs.[6]]
        CNOT' [qs.[6]; qs.[10]]
        H' [qs.[6]]
        M' >< qs.[7 .. 10]

    let IXXIIXX (qs:Qubits) =
        // X 1
        H' [qs.[1]]
        CNOT' [qs.[1]; qs.[7]]
        H' [qs.[1]]
        // X 2
        H' [qs.[2]]
        CNOT' [qs.[2]; qs.[8]]
        H' [qs.[2]]
        // X 5
        H' [qs.[5]]
        CNOT' [qs.[5]; qs.[9]]
        H' [qs.[5]]
        // X 6
        H' [qs.[6]]
        CNOT' [qs.[6]; qs.[10]]
        H' [qs.[6]]
        M' >< qs.[7 .. 10]

    let XIXIXIX (qs:Qubits) =
        // X 0
        H' [qs.[0]]
        CNOT' [qs.[0]; qs.[7]]
        H' [qs.[0]]
        // X 2
        H' [qs.[2]]
        CNOT' [qs.[2]; qs.[8]]
        H' [qs.[2]]
        // X 4
        H' [qs.[4]]
        CNOT' [qs.[4]; qs.[9]]
        H' [qs.[4]]
        // X 6
        H' [qs.[6]]
        CNOT' [qs.[6]; qs.[10]]
        H' [qs.[6]]
        M' >< qs.[7 .. 10]


    let ZZZIIII (qs:Qubits) =
        // Z 0
        CNOT' [qs.[0]; qs.[7]]
        // Z 1
        CNOT' [qs.[1]; qs.[8]]
        // Z 2
        CNOT' [qs.[2]; qs.[9]]
        M' >< qs.[7 .. 10]


    // resets a list of qubits to |0>
    let resetQubits (qs:Qubits) =
        let b = Bit.Zero
        for i in 0 .. qs.Length - 1 do
            if (qs.[i].Bit = Bit.Unknown) then
                M' [qs.[i]]
            Reset b [qs.[i]]               

    // ancilla initialization procedure
    let initAncilla (qs:Qubits) =
        // prepare and check Shor state until success
        let mutable loop = true
        while (loop) do
            loop <- true
            resetQubits qs
            // prepare Shor state
            H' [qs.[0]]
            for i in 1 .. qs.Length - 2 do
                CNOT' [qs.[i - 1]; qs.[i]]
            CNOT' [qs.[0]; qs.[qs.Length - 1]]
            CNOT' [qs.[qs.Length - 2]; qs.[qs.Length - 1]]
            H' >< qs.[0 .. qs.Length - 2]
            M' [qs.[qs.Length - 1]]
            // check if preparation was successful, repeat if not
            if (qs.[qs.Length - 1].Bit.v = 0) then
                loop <- false

    // stabilizer measurement and resetting of ancilla
    let checkAndReset (qs:Qubits) =
        let mutable parity = 0
        for i in 0 .. qs.Length - 1 do
            if (qs.[i].Bit.v = 1) then
                parity <- parity + 1
        resetQubits qs
        ((parity % 2) = 0)


    // program entry point
    [<LQD>]
    let QECC()    =
        let k = Ket(5)
        let qs = k.Qubits

        // prepare a |0>_L state
        let mutable count = 0
        let mutable attempt = 0
        let stabs = [IIIXXXX; IXXIIXX; XIXIXIX; IIIZZZZ; IZZIIZZ; ZIZIZIZ; ZZZIIII]

        while (count < stabs.Length) do
            attempt <- attempt + 1
            show "Attempt %d" attempt
            let qs = k.Reset(12)
            let ancilla = qs.[7 .. qs.Length - 1]
            count <- 0
            for i in 0 .. stabs.Length - 1 do
                let stabMeasurement = stabs.[i]
                initAncilla ancilla
                stabMeasurement qs
                let ancillaOut = checkAndReset ancilla
                if (ancillaOut) then
                    count <- count + 1
            show "Count is %d" count

        show "State is %s" (k.ToString())                        

#if INTERACTIVE
do Script.QECC()        // If interactive, then run the routine automatically
#endif