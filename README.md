# prolog-arduino
Running Prolog code on a microcontroller

## Design Decisions
+ Using ESP32 not Arduino
    + Faster processor
    + More RAM
    + More Storage
+ ESP32 hosts WiFi AP and TCP server
    + No messing about with IP addresses
    + Wireless transfer of new code and interaction
+ Data structures have lengths of multiples of 8 bits
    + Easier to write C code
    + Only small amounts of waste
+ Special functionality, e.g. pin interaction implemented as special predicates
    + Already have mechanism for passing arguments to rules
    + Only linker is special
        + Minimal code change
        + Linker just replaces `call` instructions with special instructions
+ Heap and Stack differentiation in Instruction and Value headers
    + Saves on space
    + Minimal code complexity increase
+ Functors used in `is` operation have specific functor ids
    + Operators can easily be mapped to functor of ASCII character id
+ Base Stack Frame contains register state after query runs
    + Registers recovered on success
    + Allows results to be read from the microcontroller
+ Register and Stack value indexes start at 0
    + Translates more naturally into C arrays
+ I insert proceed instructions after deallocate instructions not succeeded by an execute instruction
    + Solves edge case of returning from virtual predicates and cuts
+ Structures' and Lists' bodies are a list of registry entry indices
    + Detaches Structure and contents
    + Allows easier rearrangement of memory
+ Garbage Collection
    + Runs concurrently with instruction execution
        + Paused when all tuples are live
        + Resumed on deallocation, trimming, and backtracking
    + Unified memory space for heap, stack, environments, choice points, trail
        + Trail unwinding controlled by relative position of choice points and trail items
        + No penalty for using heap more than stack or stack more than heap etc
        + Environments have size and capacity
            + Size reduced upon trimming, capacity unchanged
            + When scanning, only permanent variabes with index < size are scanned
                + No space saving in environment, but trimmed variables are swept


## Ideas for Futher Work
+ Multi-Precision Numbers
+ User input request
+ Dynamic Predicates
