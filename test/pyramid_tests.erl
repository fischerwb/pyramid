%% coding: utf-8
%% vim: set et ft=erlang ff=unix ts=4 sw=4 tw=80 cc=80 :

-module(pyramid_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

pyramid() ->
    ?SIZED(N, pyramid(N)).

pyramid(0) -> ?LET(P, pyramid:new(), P);
pyramid(N) ->
    ?LETSHRINK({Path, Key, Value, Pyr},
               {list(term()), term(), term(), pyramid(N-1)},
               pyramid:insert(Path, Key, Value, Pyr)
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

prop_delete_any() ->
    ?FORALL({Path, Key, Pyramid},
            {list(term()), term(), pyramid()},
            error =:= pyramid:search(Path, Key, pyramid:delete(Path, Key, Pyramid))
           ).

path_gt_1() ->
    ?LET({P, T},
         {list(term()), term()},
         [T | P]).

different_vs() ->
    ?SUCHTHAT({V1, V2},
              {term(), term()},
              V1 =/= V2).

prop_path_override() ->
    ?FORALL({Path, Key, {V1, V2}, Pyramid},
            {path_gt_1(), term(), different_vs(), pyramid()},
            begin
                P1 = pyramid:insert(Path, Key, V1, pyramid:insert(tl(Path), Key, V2, Pyramid)),
                {ok, V1} =:= pyramid:search(Path, Key, P1) andalso
                {ok, V2} =:= pyramid:search(tl(Path), Key, P1)
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

path_override_test_() ->
    ?pt(prop_path_override).

paths_test_() ->
    ?pt(prop_paths).

