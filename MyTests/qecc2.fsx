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

    let rotX (theta:float) (qs:Qubits) =
        let gate (theta:float) =
            let nam     = "Rx" + theta.ToString("F2")
            new Gate(
                Name    = nam,
                Help    = sprintf "Rotate in X by: %f" theta,
                Mat     = (
                    let phi     = theta / 2.0
                    let c       = Math.Cos phi
                    let s       = Math.Sin phi
                    CSMat(2,[0,0,c,0.;0,1,0.,s;1,0,0.,s;1,1,c,0.])),
                Draw    = "\\gate{" + nam + "}"
                )
        (gate theta).Run qs


    let prepFK (qs:Qubits) =
        let gate =
            let nam     = "Simple FK state"
            new Gate(
                Name    = nam,
                Help    = sprintf "Prep FK state",
                Mat     = (
                    let sq3     = 1. / Math.Sqrt(3.)
                    let c       = Math.Cos (Math.PI/8.)
                    let s       = Math.Sin (Math.PI/8.)
                    CSMat(8,[0,0,sq3,0.;3,0,s * sq3,0.;6,0,sq3,0.;7,0,c * sq3,0.])),
                Draw    = "\\gate{" + nam + "}"
                )
        gate.Run qs



    // p value for depolarization channels
    let p = 0.1

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

    let T' (qs:Qubits) =
        T qs
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
    let checkAncilla (qs:Qubits) =
        let mutable parity = 0
        for i in 0 .. qs.Length - 1 do
            if (qs.[i].Bit.v = 1) then
                parity <- parity + 1
        ((parity % 2) = 0)

    // perform syndrome extraction
    let syndromes (qs:Qubits) (ancilla:Qubits) =
        let stabs = [IIIXXXX; IXXIIXX; XIXIXIX; IIIZZZZ; IZZIIZZ; ZIZIZIZ]
        let mutable syns = []

        for stab in stabs do
            initAncilla ancilla
            stab qs
            let ancillaOut = checkAncilla ancilla
            syns <- syns @ [ancillaOut]

        syns

    // function to correct for a single type of error (either X or Z) based on syndromes for that type of error
    let singleErrorCorrection (qs:Qubits) (corr: Qubits -> unit) (syndromes:List<bool>) =
        let syn1 = syndromes.[0]
        let syn2 = syndromes.[1]
        let syn3 = syndromes.[2]
        
        // syndrome is 000 -> correction on qubit 6
        if (not(syn1) && not(syn2) && not(syn3)) then
            corr [qs.[6]]

        // syndrome is 001 -> correction on qubit 5
        if (not(syn1) && not(syn2) && syn3) then
            corr [qs.[5]]

        // syndrome is 010 -> correction on qubit 4
        if (not(syn1) && syn2 && not(syn3)) then
            corr [qs.[4]]

        // syndrome is 011 -> correction on qubit 3
        if (not(syn1) && syn2 && syn3) then
            corr [qs.[3]]

        // syndrome is 100 -> correction on qubit 2
        if (syn1 && not(syn2) && not(syn3)) then
            corr [qs.[2]]

        // syndrome is 101 -> correction on qubit 1
        if (syn1 && not(syn2) && syn3) then
            corr [qs.[1]]

        // syndrome is 110 -> correction on qubit 0
        if (syn1 && syn2 && not(syn3)) then
            corr [qs.[0]]

    // correct for X and Z errors in Steane's code
    let errorCorrection (qs:Qubits) (syns:List<bool>) =
        // correct for Z errors
        let zsyns = syns.[0 .. 2]
        singleErrorCorrection qs Z' zsyns

        // correct for X errors
        let xsyns = syns.[3 .. 5]
        singleErrorCorrection qs X' xsyns

    let test (qs:Qubits) =
        let mutable num = 0

        for i in 1 .. 100 do
            // prepare a |0>_L state
            let mutable count = 0
            let mutable attempt = 0
            let stabs = [IIIXXXX; IXXIIXX; XIXIXIX; IIIZZZZ; IZZIIZZ; ZIZIZIZ; ZZZIIII]

            while (count < stabs.Length) do
                attempt <- attempt + 1
                //show "Attempt %d" attempt
                resetQubits qs
                let ancilla = qs.[7 .. qs.Length - 1]
                count <- 0
                for i in 0 .. stabs.Length - 1 do
                    let stabMeasurement = stabs.[i]
                    initAncilla ancilla
                    stabMeasurement qs
                    let ancillaOut = checkAncilla ancilla
                    if (ancillaOut) then
                        count <- count + 1
                //show "Count is %d" count

            H' >< qs.[0 .. 6]
            let syns = syndromes qs qs.[7 .. qs.Length - 1]
            errorCorrection qs syns
(*
            S' >< qs.[0 .. 6]            
            let syns = syndromes qs qs.[7 .. qs.Length - 1]
            errorCorrection qs syns
*)
            M' >< qs.[0 .. 6]
            let mutable parity = 0
            for i in 0 .. qs.Length - 1 do
                if (qs.[i].Bit.v = 1) then
                    parity <- parity + 1

            show "Measurement outcome %d" (parity % 2)
            num <- num + (parity % 2)
        
        show "Number of ones %d" num
        

    let encode (qs:Qubits) (idx:List<int>) =
        // first layer
        H >< [qs.[idx.[4]]; qs.[idx.[5]]; qs.[idx.[6]]]
        CNOT [qs.[idx.[0]]; qs.[idx.[1]];]
        CNOT [qs.[idx.[0]]; qs.[idx.[2]];]

        // second layer
        CNOT [qs.[idx.[6]]; qs.[idx.[3]]]
        CNOT [qs.[idx.[6]]; qs.[idx.[1]]]
        CNOT [qs.[idx.[6]]; qs.[idx.[0]]]        

        // third layer
        CNOT [qs.[idx.[5]]; qs.[idx.[3]]]
        CNOT [qs.[idx.[5]]; qs.[idx.[2]]]
        CNOT [qs.[idx.[5]]; qs.[idx.[0]]]        

        // fourth layer
        CNOT [qs.[idx.[4]]; qs.[idx.[3]]]
        CNOT [qs.[idx.[4]]; qs.[idx.[2]]]
        CNOT [qs.[idx.[4]]; qs.[idx.[1]]]        
        

    let ex (qs:Qubits) =
//        for i in 1 .. 100 do
//            resetQubits qs
        H' [qs.[0]]
        CNOT' qs
        M' >< [qs.[0]]


    // program entry point
    [<LQD>]
    let QECC()    =
(*
        let k = Ket(3)
        let n = 10000
        let mutable stats = 0
        let rand = System.Random()

        for run in 1 .. n do
            let qs = k.Reset()

            prepFK qs

            let idx = 0
            let mutable res = 0
            M [qs.[idx]]
            show "Run %d, outcome %d" run (qs.[idx].Bit.v)

            stats <- stats + qs.[idx].Bit.v

        show "1 outcomes: %d" stats
*)

        let k = Ket(21)
        let n = 100
        let mutable stats = 0
        let rand = System.Random()

        for run in 1 .. n do
            let qs = k.Reset()

            prepFK [qs.[0]; qs.[7]; qs.[14]]
            encode qs [0 .. 6]
            encode qs [7 .. 13]
            encode qs [14 .. 20]

            for i in 0 .. (qs.Length - 1) do
                depolarize p [qs.[i]]

            let idx = 0

            let mutable res = 0
            M >< qs.[(idx * 7) .. (idx * 7 + 6)]
            for i in (idx * 7) .. (idx * 7 + 6) do
                res <- res + qs.[i].Bit.v

            show "Run %d, outcome %d" run (res % 2)                

            stats <- stats + (res % 2)

        show "1 outcomes: %d" stats

#if INTERACTIVE
do Script.QECC()        // If interactive, then run the routine automatically
#endif