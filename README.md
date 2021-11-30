# SAT-SOLVER

This is a solver for the temporal logic's algorithm SAT. It takes a model as an
input and it returns the states where the given formula is valid.

## Usage

1) Clone the repository

2) CD to the folder of the repository and run the command `stack setup`.

3) After that, run the command `stack build`.

4) Now the executable is ready. In the folder `test` you will find some model
examples you can use.

5) To execute, run in your terminal the following command: 

    `stack exec TP-Final-exe "<path_to_your_file>.sat"`.

    Make sure the extension of the file is `.sat`

## The grammar.

### Syntax

The syntax used to write the CTL formula in the .sat file is pretty simple:

| Symbol       | Translation        |
| ------------ | ------------------ |
| TOP          | ⊤                  |
| BT           | ⊥                  |
| ! p          | ¬ p                |
| p & q        | p ∧ q              |
| p \| q       | p ∨ q              |
| p -> q       | p → q              |
| AX p         | ∀◯                 | 
| EX p         | ∃◯                 |
| AU p         | ∀ [p U q]          |
| EU p         | ∃ [p U q]          |
| AF p         | ∀◊                 |
| EF p         | ∃◊                 |
| AG p         | ∀□                 |
| EG p         | ∃□                 |

### Precedence

The precedence of the operators is given in the following order, beginning with the 
operators with the most precedence to the ones with the least precedence. The operators
that are on the same item have the same precedence.

- Left associative: &, \|

- Left associative: ->, EU AU

- !, AX, EX, EF, AF, AG, EG

If you are not sure you can always use parentheses to explicit the order you want 
in your formula.


