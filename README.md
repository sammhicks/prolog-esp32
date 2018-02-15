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
+ Functors used in `is` operation have specific functor ids &lt; 128, others have ids &gt;= 128
    + Operators can easily be mapped to functor of ASCII character id
    + No clash of functor id
    + Minimal wasted space
        + We probably don't need 65280 different functor ids
+ Base Stack Frame contains register state after query runs
    + Registers recovered on success
    + Allows results to be read from the microcontroller
+ Register and Stack value indexes start at 0
    + Translates more naturally into C arrays
