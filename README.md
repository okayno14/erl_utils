# Info

Библиотека с erlang-утилитками:

compose.erl - либа для функциональных композиций
curry.erl - либа для каррирования функций
io_lib.erl - работа с вводом/выводом
port_lib.erl - работа с erlang-портами

# curry

Ниже прикреплены примеры использования:

```erlang
F1 = curry:make_curry(fun(A, B) -> A + B end).
(F2(1))(2).
```

```erlang
F1 = curry:make_curry(fun(A, B, C) -> A + B + C end).
((F1(1))(2))(3).
```

```erlang
F1 = curry:make_curry(fun(A, B, C) -> A + B + C end).
curry:run_curry(F1, [1,2,3]).
```

```erlang
F2 = curry:make_curry(fun io:format/3).
F3 = F2(standard_error).
(F3("f~n"))([]).
curry:run_curry(F3, ["f~n", []]).
```

