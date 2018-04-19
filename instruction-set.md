# Instruction Set

### Pins

#### Modes

|           Pin Modes           | Value|
| ----------------------------- | ---- |
| Input                         | `00` |
| Output                        | `01` |
| Input with pullup resistor    | `02` |
| Input with pulldown resistor  | `03` |

## Instructions

| instruction                               | opcode |
| ----------------------------------------- |:------:|
| put variable `Xn`, `Ai`                   |  `00`  |
| put variable `Yn`, `Ai`                   |  `01`  |
| put value `Xn`, `Ai`                      |  `02`  |
| put value `Yn`, `Ai`                      |  `03`  |
| put structure `f`, `n`, `Vn`              |  `04`  |
| put list `Ai`                             |  `05`  |
| put constant `c`, `Ai`                    |  `06`  |
| put integer `i`, `Ai`                     |  `07`  |
| get variable `Xn`, `Ai`                   |  `10`  |
| get variable `Yn`, `Ai`                   |  `11`  |
| get value `Xn`, `Ai`                      |  `12`  |
| get value `Yn`, `Ai`                      |  `13`  |
| get structure `f`, `n`, `Vn`              |  `14`  |
| get list `Ai`                             |  `15`  |
| get constant `c`, `Ai`                    |  `16`  |
| get integer `i`, `Ai`                     |  `17`  |
| set variable `Xn`                         |  `20`  |
| set variable `Yn`                         |  `21`  |
| set value `Xn`                            |  `22`  |
| set value `Yn`                            |  `23`  |
| set constant `c`                          |  `26`  |
| set integer `i`                           |  `27`  |
| set void `n`                              |  `28`  |
| unify variable `Xn`                       |  `30`  |
| unify variable `Yn`                       |  `31`  |
| unify value `Xn`                          |  `32`  |
| unify value `Yn`                          |  `33`  |
| unify constant `c`                        |  `36`  |
| unify integer `i`                         |  `37`  |
| unify void `n`                            |  `38`  |
| allocate `N`                              |  `40`  |
| trim `N`                                  |  `41`  |
| deallocate                                |  `42`  |
| call `P`                                  |  `43`  |
| execute `P`                               |  `44`  |
| proceed                                   |  `45`  |
| try me else `P`                           |  `50`  |
| retry me else `P`                         |  `51`  |
| trust me                                  |  `52`  |
| neck cut                                  |  `53`  |
| get level `Yn`                            |  `54`  |
| cut `Yn`                                  |  `55`  |
| >                                         |  `60`  |
| <                                         |  `61`  |
| =<                                        |  `62`  |
| >=                                        |  `63`  |
| =\=                                       |  `64`  |
| =:=                                       |  `65`  |
| is                                        |  `66`  |
| true                                      |  `70`  |
| fail                                      |  `71`  |
| =                                         |  `72`  |
| configure pin `A0` as digital `Mode`      |  `80`  |
| digital read pin `A0` to `A1`             |  `81`  |
| digital write pin `A0` to `A1`            |  `82`  |
| configure pin `A0` as analog mode input   |  `84`  |
| configure channel `A0`                    |  `85`  |
| configure pin `A0` as analog mode output  |  `86`  |
| analog read pin `A0` to `A1`              |  `87`  |
| analog write pin `A0` to `A1`             |  `88`  |
| read line sensor                          |  `89`  |
| get time from start in milliseconds, `i`  |  `8A`  |
| delay for `i` milliseconds, `i`           |  `8B`  |
