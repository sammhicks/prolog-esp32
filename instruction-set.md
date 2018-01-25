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
+ Servo Motor

### Argument Notation

| Notation | Meaning           |
| -------- | ----------------- |
|   `--`   | Ignored           |
|   `Ai`   | Argument Register |
|   `Xn`   | Register          |
|   `Yn`   | Stack Register    |


## Value Representations in Memory

+ Reference
+ Structure
+ List
+ Constant
+ Integer


## Instructions

| instruction                       | octet 3  | octet 2  | octet 1  | octet 0  |
| --------------------------------- | -------  | :------: | :------: | :------: |
| put variable `Xn`, `Ai`           |   `00`   |   `--`   |   `Xn`   |   `Ai`   |
| put variable `Yn`, `Ai`           |   `01`   |   `--`   |   `Yn`   |   `Ai`   |
| put value `Xn`, `Ai`              |   `02`   |   `--`   |   `Xn`   |   `Ai`   |
| put value `Yn`, `Ai`              |   `03`   |   `--`   |   `Yn`   |   `Ai`   |
| put unsafe value `Yn`, `Ai`       |   `04`   |   `--`   |   `Yn`   |   `Ai`   |
| put structure `f`, `Ai`           |   `06`   |   `f`    |   `f`    |   `Ai`   |
| put list `Ai`                     |   `07`   |   `--`   |   `--`   |   `Ai`   |
| put constant `c`, `Ai`            |   `08`   |   `c`    |   `c`    |   `Ai`   |
| put integer `i`, `Ai`             |   `09`   |   `i`    |   `i`    |   `Ai`   |
| get variable `Xn`, `Ai`           |   `10`   |   `--`   |   `Xn`   |   `Ai`   |
| get variable `Yn`, `Ai`           |   `11`   |   `--`   |   `Yn`   |   `Ai`   |
| get value `Xn`, `Ai`              |   `12`   |   `--`   |   `Xn`   |   `Ai`   |
| get value `Yn`, `Ai`              |   `13`   |   `--`   |   `Yn`   |   `Ai`   |
| get structure `f`, `Ai`           |   `16`   |   `f`    |   `f`    |   `Ai`   |
| get list `Ai`                     |   `17`   |   `--`   |   `--`   |   `Ai`   |
| get constant                      |   `18`   |   `c`    |   `c`    |   `Ai`   |
| get integer `i`, `Ai`             |   `19`   |   `i`    |   `i`    |   `Ai`   |
| set variable `Xn`, `Ai`           |   `20`   |   `--`   |   `Xn`   |   `--`   |
| set variable `Yn`, `Ai`           |   `21`   |   `--`   |   `Yn`   |   `--`   |
| set value `Xn`, `Ai`              |   `22`   |   `--`   |   `Xn`   |   `--`   |
| set value `Yn`, `Ai`              |   `23`   |   `--`   |   `Yn`   |   `--`   |
| set local value `Xn`              |   `24`   |   `--`   |   `Xn`   |   `--`   |
| set local value `Yn`              |   `25`   |   `--`   |   `Yn`   |   `--`   |
| set constant `c`                  |   `28`   |   `c`    |   `c`    |   `--`   |
| set integer `i`                   |   `29`   |   `i`    |   `i`    |   `--`   |
| set void `n`                      |   `2A`   |   `--`   |   `n`    |   `--`   |
| unify variable `Xn`, `Ai`         |   `30`   |   `--`   |   `Xn`   |   `--`   |
| unify variable `Yn`, `Ai`         |   `31`   |   `--`   |   `Yn`   |   `--`   |
| unify value `Xn`, `Ai`            |   `32`   |   `--`   |   `Xn`   |   `--`   |
| unify value `Yn`, `Ai`            |   `33`   |   `--`   |   `Yn`   |   `--`   |
| unify local value `Xn`            |   `34`   |   `--`   |   `Xn`   |   `--`   |
| unify local value `Yn`            |   `35`   |   `--`   |   `Yn`   |   `--`   |
| unify constant `c`                |   `38`   |   `c`    |   `c`    |   `--`   |
| unify integer `i`                 |   `39`   |   `i`    |   `i`    |   `--`   |
| unify void `n`                    |   `3A`   |   `--`   |   `n`    |   `--`   |
| allocate `N`                      |   `40`   |   `--`   |   `--`   |    `N`   |
| deallocate                        |   `41`   |   `--`   |   `--`   |   `--`   |
| call `P`, `N`                     |   `42`   |   `P`    |   `P`    |    `N`   |
| execute `P`                       |   `43`   |   `P`    |   `P`    |   `--`   |
| proceed                           |   `44`   |   `--`   |   `--`   |   `--`   |
| try me else `L`                   |   `48`   |   `L`    |   `L`    |   `L`    |
| retry me else `L`                 |   `49`   |   `L`    |   `L`    |   `L`    |
| trust me                          |   `4A`   |   `--`   |   `--`   |   `--`   |
| neck cut                          |   `4C`   |   `--`   |   `--`   |   `--`   |
| get level `Yn`                    |   `4D`   |   `--`   |   `Yn`   |   `--`   |
| cut `Yn`                          |   `4E`   |   `--`   |   `Yn`   |   `--`   |
| configure pin `A0` as `Mode`      |   `50`   |   `--`   |   `--`   |  `Mode`  |
| digital check pin `A0` is `Value` |   `52`   |   `--`   |   `--`   |  `Value` |
| digital write pin `A0` to `Value` |   `53`   |   `--`   |   `--`   |  `Value` |
| analog read pin `A0` to `A1`      |   `54`   |   `--`   |   `--`   |   `--`   |
| analog write pin `A0` to `A1`     |   `55`   |   `--`   |   `--`   |   `--`   |
| set servo `A0` to `A1`            |   `57`   |   `--`   |   `--`   |   `--`   |