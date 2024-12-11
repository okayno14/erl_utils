# Info

Библиотека с erlang-утилитками:

* compose.erl - либа для функциональных композиций
* curry.erl - либа для каррирования функций
* io_lib.erl - работа с вводом/выводом
* port_lib.erl - работа с erlang-портами

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
# maybe

Примеры:

```erlang

    Maybe = maybe:maybe(1),
    IncFun = fun(X) -> maybe(X + 1) end.
    Maybe2 = maybe:pipe(Maybe, [IncFun, IncFun, IncFun]),
    ?assertEqual(maybe:extract(Maybe2), 4).

    Maybe = maybe:maybe(1),
    IncFun = fun(X) -> maybe(X + 1) end.
    BindFun = fun maybe:bind/2,
    Maybe2 = BindFun(BindFun(BindFun(Maybe, IncFun), fun(_) -> maybe(undefined) end), IncFun),
    ?assertEqual(maybe:extract(Maybe2), undefined).

    Maybe = maybe:maybe(1),
    IncFun = inc_fun(),
    Maybe2 =
    maybe:pipe(Maybe, [
        fun(X) -> maybe:maybe({dive, maybe:maybe(X), [IncFun || _ <- lists:seq(1, 10)]}) end
    ]),
    ?assertEqual(maybe:extract(Maybe2), 11).
```

