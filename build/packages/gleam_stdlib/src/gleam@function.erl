-module(gleam@function).
-compile(no_auto_import).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2]).

-spec compose(fun((EGB) -> EGC), fun((EGC) -> EGD)) -> fun((EGB) -> EGD).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-spec curry2(fun((EGE, EGF) -> EGG)) -> fun((EGE) -> fun((EGF) -> EGG)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-spec curry3(fun((EGI, EGJ, EGK) -> EGL)) -> fun((EGI) -> fun((EGJ) -> fun((EGK) -> EGL))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-spec curry4(fun((EGN, EGO, EGP, EGQ) -> EGR)) -> fun((EGN) -> fun((EGO) -> fun((EGP) -> fun((EGQ) -> EGR)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-spec curry5(fun((EGT, EGU, EGV, EGW, EGX) -> EGY)) -> fun((EGT) -> fun((EGU) -> fun((EGV) -> fun((EGW) -> fun((EGX) -> EGY))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-spec curry6(fun((EHA, EHB, EHC, EHD, EHE, EHF) -> EHG)) -> fun((EHA) -> fun((EHB) -> fun((EHC) -> fun((EHD) -> fun((EHE) -> fun((EHF) -> EHG)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-spec flip(fun((EHI, EHJ) -> EHK)) -> fun((EHJ, EHI) -> EHK).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-spec identity(EHL) -> EHL.
identity(X) ->
    X.

-spec constant(EHM) -> fun((any()) -> EHM).
constant(Value) ->
    fun(_) -> Value end.

-spec tap(EHO, fun((EHO) -> any())) -> EHO.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.
