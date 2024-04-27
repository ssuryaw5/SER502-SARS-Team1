# SARS Compiler and Virtual machine for Programming Language

# SER 502 - Project Team 1

### SARS is a simple programming language developed to compute arithmetic operations, conditions, and loops.

#### Team Members:

1. Sumeet Suryawanshi
2. Akash Rana
3. Rohan Mathur
4. Sadhanand Srinivasan

## ⚙ Tools Used

- SWI-Prolog
- Python3
- VS Code

## Project Video Link

-

## ⚙ How to Execute it

- Clone the git repository into your local machine
- Install python 3.12.3 and SWI-Prolog 9.2.4-1 on your local machine
- SWI-Prolog Download ([click here](https://www.swi-prolog.org/download/stable))
- Python3 Download ([click here](https://www.python.org/downloads/))
- Sample test programs are saved in the 'data' folder with .sars extension
- Open the terminal and execute the below command

bash
swipl

- Enter the path to the .pl file

```bash
?- ['/Users/user/Desktop/SER502-SARS-Team1/src/SARS.pl'].
```

- Run the sample program by parsing tokens through a lexer

### Enter filePath for 2 files [lexerFile, dataFile]

### sars(<Lexer_filePath>,<data_filePath>).

```bash
?- sars('/Users/user/Desktop/SER502-SARS-Team1/src/Lexer.py','/Users/user/Desktop/SER502-SARS-Team1/data/findFactorial.sars').
```
