# λ-calculus interpreter

This project aims to build a simple λ-calculus interpreter, following and
expanding some of the rules explained in _Types and Programming Languages_
by Benjamin C. Pierce. These rules are summed up [here](summary_of_rules.pdf).

It has been developed by @xeferz and @anxomm.

## Usage

To compile, just run the Makefile:

```Shell
make
make all
```

and to execute you have two options, the interactive console or passing a file:

```Shell
./top [<file>]
```

If passing a file, the evaluation will stop if an error is found. In this case,
the line where the error is detected will be printed.

---

## Syntax

This section describes how the interpreter shoud be used, what types are
implemented and what operations are allowed. For more exhaustive examples
just have a look at [examples](examples.txt).

1. Multiline expression is allowed

2. Every expression must end with a ';'

3. Variable definitions:

```OCaml
>> x = 5;
x : Nat = 5
```

4. Booleans:

```OCaml
>> true; 
- : Bool = true
>> false;
- : Bool = false
```

5. Naturals: with successor, predecessor and iszero operations:

```OCaml
>> 1;
- : Nat = 1
>> succ 1;
- : Nat = 2
>> pred 1;
- : Nat = 0
>> iszero 1;
- : Bool = false
```

5. If structure: the type of both branches must be the same, and the guard a Bool

```OCaml
>> if iszero x then 1 else 2;
- : Nat = 2
```

6. Function definitions: they must be typed

```OCaml
>> lambda x:Bool. x;
- : (Bool) -> (Bool) = (lambda x:Bool. x)
>> L x:Bool. x;     
- : (Bool) -> (Bool) = (lambda x:Bool. x
```

7. Local expressions: similar to _let ... in_ in functional programming

```OCaml
>> let x = 1 in succ x;
- : Nat = 2
```

8. Recursive expressions:

```OCaml
>> letrec sum : Nat -> Nat -> Nat = lambda n : Nat. lambda m : Nat. if iszero n then m else succ (sum (pred n) m) 
in sum 32 41;
- : Nat = 73
```

9. Strings: '"' and ";" are not allowed inside a String, and the contatenation operation 

```OCaml
>> "a";
- : String = "a"
>> "a" ^ "b";
- : String = "ab"
```

10. Pairs: a pair of elements of any type, and the projection of the _first_ and _second_ element

```OCaml
>> p = {0, false};
p : {Nat, Bool} = {0, false}
>> first p;
- : Nat = 0
>> second p;
- : Bool = false
```

11. Records: sequence of elements of any type with and id, and the projection of the field by the id

```OCaml
>> r = {a = 0, b = { a = 2 }, c = L x:Nat. x};        
r : { a : Nat, b : { a : Nat }, c : (Nat) -> (Nat) } = { a = 0, b = { a = 2 }, c = (lambda x:Nat. x) }
>> r.a;                                        
- : Nat = 0
>> { };                                        
- : {  } = {  }
```

12. Top: root type

```OCaml
>> f = L x:Top. x;    
f : (Top) -> (Top) = (lambda x:Top. x)
>> f 1;
- : Top = 1
>> f {false, 0};
- : Top = {false, 0}
```
---

## Special behaviour

### Subtyping

Subtyping is implemented only for records and functions: 

- A record is a supertype of another if it has at least the same fields (id),
and they are of the same type or a subtype.
- A function is a supertype of another if its inputs are a subtype and its outputs
are a supertype of the other one.

### Debug mode

Executing the command `debug = <Bool>;` you can switch between the default mode
and the debug mode. In this mode, intermediate expressions resulted from an
evaluation of a term are also printed:

```OCaml
>> debug = true;
debug : Bool = true
>> let x = first {succ 1, true} in iszero x;
   let x = 2 in iszero (x)
   iszero (2)
   false
- : Bool = false
```

### Exit

With 'Ctrl+D' you can exit the interpreter.
