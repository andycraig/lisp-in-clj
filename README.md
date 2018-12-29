# lisp-in-clj

A (partial) Lisp interpreter. Made following [Peter Norvig's tutorial on implementing a Lisp in Python](http://norvig.com/lispy.html), but with Clojure instead of Python.

I made it to practice Clojure and interpreter implementation. It's not really of any use beyond that.

## Run

Run with [Leiningen](https://leiningen.org/):

```
lein run
```

## How to use

Functions can be defined as so:

```
>>>(define inc (lambda (x) (add x 1)))
nil
>>>(inc 1)
2
```

Recursive functions work too:

```
>>>(define fac (lambda (x) (if (eq x 1) 1 (mul x (fac (sub x 1))))))
nil
>>>(fac 4)
24
```

## Limitations

Lots. Doesn't implement `car`, `cdr` or `cons`. Very brittle and will crash if, for example, you try to use a function or variable that isn't defined.