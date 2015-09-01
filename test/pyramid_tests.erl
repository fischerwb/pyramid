%% coding: utf-8
%% vim: set et ft=erlang ff=unix ts=4 sw=4 tw=80 cc=80 :

-module(pyramid_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

pyramid() ->
    ?SIZED(Size, pyramid(Size)).

pyramid(0) ->
    pyramid:new();
pyramid(Sz) ->
    pyramid:insert(list(term()), term(), term(), pyramid(Sz-1)).

inserted_can_be_searched() ->
    ?FORALL({Path, Key, Value},
            {pyramid:path(), pyramid:key(), term()},
            {ok, Value} =:= pyramid:search(Path, Key, pyramid:insert(Path, Key, Value, pyramid:new()))
           ).

insert_many_search_all() ->
    ?FORALL(Pyramid,
            pyramid(),
            begin
                {Path, Key, Value} = oneof(Settings),
                {ok, Value} =:= pyramid:search(Path, Key, Pyramid)
            end).


pyramid_test_() ->
    Opts = [verbose, {numtests, 500}, {to_file, user}],
    [
     ?_assert(proper:quickcheck(inserted_can_be_searched(), Opts)),
     ?_assert(proper:quickcheck(insert_many_search_all(), Opts))
    ].
