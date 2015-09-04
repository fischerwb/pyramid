%% coding: utf-8
%% vim: set et ft=erlang ff=unix ts=4 sw=4 tw=80 cc=80 :

-module(pyramid_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

pyramid() ->
    ?SIZED(N, pyramid(N)).

pyramid(0) -> pyramid:new();
pyramid(N) ->
    ?LAZY(
       ?LETSHRINK({Path, Key, Value, Pyr},
                  {list(term()), term(), term(), pyramid(N-1)},
                  pyramid:insert(Path, Key, Value, Pyr)
                 )
      ).

prop_inserted_can_be_searched() ->
    ?FORALL({Path, Key, Value, Pyramid},
            {list(term()), term(), term(), pyramid()},
            {ok, Value} =:= pyramid:search(Path, Key, pyramid:insert(Path, Key, Value, Pyramid))
           ).

prop_fromlist_tolist_roundtrip() ->
    ?FORALL(Pyramid,
            pyramid(),
            Pyramid =:= pyramid:from_list(pyramid:to_list(Pyramid))
           ).

prop_search_all() ->
    ?FORALL(Pyramid,
            pyramid(),
            lists:all(fun({P,K,V}) ->
                              {ok, V} == pyramid:search(P, K, Pyramid)
                      end,
                      pyramid:to_list(Pyramid))
           ).

eunit_test_() ->
    PT = fun(T) -> {timeout, 60, ?_assert(proper:quickcheck(?MODULE:T(), [long_result]))} end,
    {inparallel,
     [
      PT(prop_inserted_can_be_searched),
      PT(prop_fromlist_tolist_roundtrip),
      PT(prop_search_all)
     ]}.

