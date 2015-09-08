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
    ?LAZY(?LETSHRINK({Path, Key, Value, Pyr},
                     {list(term()), term(), term(), pyramid(N-1)},
                     pyramid:insert(Path, Key, Value, Pyr)
                    )).

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

prop_delete_any() ->
    ?FORALL({Path, Key, Pyramid},
            {list(term()), term(), pyramid()},
            begin
                DA = fun DA([], K, P) -> pyramid:delete([], K, P);
                         DA(L, K, P) -> pyramid:delete(L, K, DA(tl(L), K, P))
                     end,
                P1 = DA(Path, Key, Pyramid),
                error =:= pyramid:search(Path, Key, P1)
            end
           ).

longpath() ->
    ?LETSHRINK({P, T1, T2},
         {list(term()), term(), term()},
         [T1, T2 | P]).

different_vs() ->
    ?SUCHTHAT({V1, V2},
              {term(), term()},
              V1 =/= V2).

prop_path_override_cascade() ->
    ?FORALL({Path, ExtraTerm, Key, {V1, V2}},
            {longpath(), term(), term(), different_vs()},
            begin
                LongPath = Path ++ [ExtraTerm],
                ShortPath = lists:droplast(Path),
                ShortestPath = lists:droplast(ShortPath),
                P1 = pyramid:insert(Path, Key, V1, pyramid:insert(ShortestPath, Key, V2, pyramid:new())),
                {ok, V1} =:= pyramid:search(Path, Key, P1) andalso
                {ok, V1} =:= pyramid:search(LongPath, Key, P1) andalso
                {ok, V2} =:= pyramid:search(ShortPath, Key, P1) andalso
                {ok, V2} =:= pyramid:search(ShortestPath, Key, P1)
            end).

prop_paths() ->
    ?FORALL({Path, Key, Value, Pyramid},
            {list(term()), term(), term(), pyramid()},
            lists:member(Path, pyramid:paths(Path, Key, pyramid:insert(Path, Key, Value, Pyramid)))).

-define(pt(N),
        {timeout, 60, ?_assert(proper:quickcheck(?MODULE:N(), [long_result]))}
       ).

inserted_can_be_searched_test_() ->
    ?pt(prop_inserted_can_be_searched).

fromlist_tolist_roundtrip_test_() ->
    ?pt(prop_fromlist_tolist_roundtrip).

search_all_test_() ->
    ?pt(prop_search_all).

delete_any_test_() ->
    ?pt(prop_delete_any).

path_override_cascade_test_() ->
    ?pt(prop_path_override_cascade).

paths_test_() ->
    ?pt(prop_paths).

