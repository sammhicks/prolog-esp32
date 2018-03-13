# Instruction Set

### Sizes

+ Register Indices are 8 bits wide
+ Stack Indices are 8 bits wide
+ Constants are 16 bits wide
+ Integers are 16 bits wide
+ Storage Addresses are 16 bits wide
+ Stack and Heap values are 32 bits wide

### Pins

#### Modes

+ Input
+ Output
+ Input with pullup resistor
+ Input with pulldown resistor


## Value Representations in Memory

+ Heap Reference
+ Stack Reference
+ Structure
+ List
+ Constant
+ Integer


## Instructions

| instruction                               | octet 0  | octet 1  | octet 2  | octet 3  | octet 4  |
| ----------------------------------------- | -------  | :------: | :------: | :------: | :------: |
| put variable `Xn`, `Ai`                   |   `00`   |   `Xn`   |   `Ai`   |          |          |
| put variable `Yn`, `Ai`                   |   `01`   |   `Yn`   |   `Ai`   |          |          |
| put value `Xn`, `Ai`                      |   `02`   |   `Xn`   |   `Ai`   |          |          |
| put value `Yn`, `Ai`                      |   `03`   |   `Yn`   |   `Ai`   |          |          |
| put structure `f`, `n`, `Ai`              |   `04`   |   `f`    |   `f`    |   `n`    |   `Ai`   |
| put list `Ai`                             |   `05`   |   `Ai`   |          |          |          |
| put constant `c`, `Ai`                    |   `06`   |   `c`    |   `c`    |   `Ai`   |          |
| put integer `i`, `Ai`                     |   `07`   |   `i`    |   `i`    |   `Ai`   |          |
| get variable `Xn`, `Ai`                   |   `10`   |   `Xn`   |   `Ai`   |          |          |
| get variable `Yn`, `Ai`                   |   `11`   |   `Yn`   |   `Ai`   |          |          |
| get value `Xn`, `Ai`                      |   `12`   |   `Xn`   |   `Ai`   |          |          |
| get value `Yn`, `Ai`                      |   `13`   |   `Yn`   |   `Ai`   |          |          |
| get structure `f`, `n`, `Ai`              |   `14`   |   `f`    |   `f`    |   `n`    |   `Ai`   |
| get list `Ai`                             |   `15`   |   `Ai`   |          |          |          |
| get constant `c`, `Ai`                    |   `16`   |   `c`    |   `c`    |   `Ai`   |          |
| get integer `i`, `Ai`                     |   `17`   |   `i`    |   `i`    |   `Ai`   |          |
| set variable `Xn`                         |   `20`   |   `Xn`   |          |          |          |
| set variable `Yn`                         |   `21`   |   `Yn`   |          |          |          |
| set value `Xn`                            |   `22`   |   `Xn`   |          |          |          |
| set value `Yn`                            |   `23`   |   `Yn`   |          |          |          |
| set constant `c`                          |   `26`   |   `c`    |   `c`    |          |          |
| set integer `i`                           |   `27`   |   `i`    |   `i`    |          |          |
| set void `n`                              |   `28`   |   `n`    |          |          |          |
| unify variable `Xn`                       |   `30`   |   `Xn`   |          |          |          |
| unify variable `Yn`                       |   `31`   |   `Yn`   |          |          |          |
| unify value `Xn`                          |   `32`   |   `Xn`   |          |          |          |
| unify value `Yn`                          |   `33`   |   `Yn`   |          |          |          |
| unify constant `c`                        |   `36`   |   `c`    |   `c`    |          |          |
| unify integer `i`                         |   `37`   |   `i`    |   `i`    |          |          |
| unify void `n`                            |   `38`   |   `n`    |          |          |          |
| allocate `N`                              |   `40`   |   `N`    |          |          |          |
| trim `N`                                  |   `41`   |   `N`    |          |          |          |
| deallocate                                |   `42`   |          |          |          |          |
| call `P`                                  |   `43`   |   `P`    |   `P`    |          |          |
| execute `P`                               |   `44`   |   `P`    |   `P`    |          |          |
| proceed                                   |   `45`   |          |          |          |          |
| try me else `P`                           |   `50`   |   `P`    |   `P`    |          |          |
| retry me else `P`                         |   `51`   |   `P`    |   `P`    |          |          |
| trust me                                  |   `52`   |          |          |          |          |
| neck cut                                  |   `53`   |          |          |          |          |
| get level `Yn`                            |   `54`   |   `Yn`   |          |          |          |
| cut `Yn`                                  |   `55`   |   `Yn`   |          |          |          |
| >                                         |   `60`   |          |          |          |          |
| <                                         |   `61`   |          |          |          |          |
| =<                                        |   `62`   |          |          |          |          |
| >=                                        |   `63`   |          |          |          |          |
| =\=                                       |   `64`   |          |          |          |          |
| =:=                                       |   `65`   |          |          |          |          |
| is                                        |   `66`   |          |          |          |          |
| true                                      |   `70`   |          |          |          |          |
| fail                                      |   `71`   |          |          |          |          |
| =                                         |   `72`   |          |          |          |          |
| configure pin `A0` as digital `Mode`      |   `80`   |  `Mode`  |          |          |          |
| digital read pin `A0` to `A1`             |   `81`   |          |          |          |          |
| digital write pin `A0` to `A1`            |   `82`   |          |          |          |          |
| configure pin `A0` as analog mode input   |   `84`   |          |          |          |          |
| configure channel `A0`                    |   `85`   |          |          |          |          |
| configure pin `A0` as analog mode output  |   `86`   |          |          |          |          |
| analog read pin `A0` to `A1`              |   `87`   |          |          |          |          |
| analog write pin `A0` to `A1`             |   `88`   |          |          |          |          |
| read line sensor                          |   `89`   |          |          |          |          |
| get time from start in milliseconds       |   `8A`   |          |          |          |          |
