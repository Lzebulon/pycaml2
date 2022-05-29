# pycaml2
The goal of this project is to create an ocaml with the mandatory indentations of python.
In reality, the goal is to learn how to create a compiler with a bullshit

# Syntax
## Variable declaration 
```
let a = 25;;

let mot = "this is a string";;
```

### Function declaration 
```
let myFunction param1 param2 = param1 + param2;;

let mySecondFunction param1 param2 param3 =
    let temp = 75 in 
    param1 + param2 + param3;;
```

### Function call 

```
let b = myFunction a 75;;

let c = myFunction a (myFunction a b);;
```


# Little Doc


## Lexer

lexer function return a list of token 

