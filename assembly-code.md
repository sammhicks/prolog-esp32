# Assembly Code

## Put Instructions

| instruction    | mnemonic        |
| :------------- | :-------------: |
| `pvar(Vn, Ai)` | put variable    |
| `pval(Vn, Ai)` | put value       |
| `pstr(f, Ai)`  | put structure f |
| `plst(Ai)`     | put list        |
| `pcon(c, Ai)`  | put constant c  |

## Get Instructions

| instruction    | mnemonic        |
| :------------- | :-------------: |
| `gvar(Vn, Ai)` | get variable    |
| `gval(Vn, Ai)` | get value       |
| `gstr(f, Ai)`  | get structure f |
| `glst(Ai)`     | get list        |
| `gcon(c, Ai)`  | get constant c  |

## Set Instructions

| instruction    | mnemonic       |
| :------------- | :------------: |
| `svar(Vn, Ai)` | set variable   |
| `sval(Vn, Ai)` | set value      |
| `scon(c, Ai)`  | set constant c |

## Unify Instructions

| instruction    | mnemonic         |
| :------------- | :--------------: |
| `uvar(Vn, Ai)` | unify variable   |
| `uval(Vn, Ai)` | unify value      |
| `ucon(c, Ai)`  | unify constant c |

## Control Instructions

| instruction   | mnemonic                                  |
| :------------ | :---------------------------------------: |
| `all(N)`      | allocate frame with N permanent variables |
| `dal`         | deallocate frame                          |
| `cal(P, N)`   | call program P/N                          |
| `pro`         | proceed                                   |

## Choice Instructions

| instruction   | mnemonic        |
| :------------ | :-------------: |
| `tme(L)`      | try me else L   |
| `rme(L)`      | retry me else L |
| `tst`         | trust me        |