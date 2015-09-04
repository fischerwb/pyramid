%% coding: utf-8
%% vim: set et ft=erlang ff=unix ts=4 sw=4 tw=80 cc=80 :

-module(pyramid).

%% @doc
%% Hierarchical configuration storage / retrieval.
%% Think CSS for configurataion settings: global settings
%% cascade down until a specific override is made.
%% Each configuration "key" is a path.  Each level in the
%% tree can have a value or not.  When searching, the most
%% specific, deepest, value is usually desired.  Keys at any
%% level in the tree are optional.
%% @end

%% API functions
-export([
         new/0,
         search/3,
         paths/3,
         insert/4,
         to_list/1,
         from_list/1
        ]).

-type level() :: {Keys :: #{}, Levels :: #{}}.  %% Levels :: maps:map(key(), pyramid())
-type key() :: term().
-type path() :: [term()].
-type pyramid() :: level().

-export_type([
              key/0,
              path/0,
              pyramid/0
             ]).

%% @doc
%% Return a new pyramid
%% @end
-spec new() -> pyramid().
new() ->
    {#{}, #{}}.

%% @doc Retrieve the configuration value by searching Path,
%% returning the most specific (deepest) value found.
-spec search(Path::path(), Key::key(), Pyramid::pyramid()) -> {ok, term()} | error.
search([], Key, {Keys,_}) ->
    maps:find(Key, Keys);
search([P | Path], Key, {Keys,Levels}) ->
    case maps:find(P, Levels) of
        error ->
            maps:find(Key, Keys);
        {ok, L} ->
            case search(Path, Key, L) of
                error -> 
                    maps:find(Key, Keys);
                V -> V
            end
    end.

%% Discover which levels in Path have overrides for Key
-spec paths(Path::path(), Key::key(), Pyramid::pyramid()) -> [path()].
paths(Path, Key, {Keys,Levels}) ->
    ok.

-spec insert(Path::path(), Key::key(), Value::term(), Pyramid::pyramid()) -> pyramid().
insert([], Key, Value, {Keys, Levels}) ->
    {maps:put(Key, Value, Keys), Levels};
insert([P | Path], Key, Value, {Keys, Levels}) ->
    Level = case maps:find(P, Levels) of
                error -> new();
                {ok, L} -> L
            end,
    {Keys, maps:put(P, insert(Path, Key, Value, Level), Levels)}.

-spec to_list(pyramid()) -> [{path(), key(), term()}].
to_list({Keys, Levels}) ->
    KeyList = [{[], K, V} || {K,V} <- maps:to_list(Keys)],
    PathList = [[{[K0|P], K, V} || {P,K,V} <- to_list(V0)]
                || {K0,V0} <- maps:to_list(Levels)],
    lists:flatten(PathList, KeyList).

-spec from_list([{Path::path(), Key::key(), Value::term()}]) -> pyramid().
from_list(Paths) ->
    lists:foldl(fun({P, K, V}, Pyr) ->
                        insert(P, K, V, Pyr)
                end,
                new(),
                Paths).

