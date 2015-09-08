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
         delete/3,
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
paths([], Key, {Keys, _Levels}) ->
    case maps:find(Key, Keys) of
        error -> [];
        {ok, _} -> [[]]
    end;
paths([P|Path], Key, {Keys, Levels}) ->
    R = case maps:find(P, Levels) of
            error -> [];
            {ok, L} -> [[P | RP] || RP <- paths(Path, Key, L)]
        end,
    paths([], Key, {Keys, Levels}) ++ R.

%% Visit every level in Pyr along Path, calling F(Keys).
%% Returns value of F from root to tip of path
path_visitor(Path, F, P) ->
    lists:reverse(r_path_visitor(Path, F, P, [])).

r_path_visitor([], F, {Keys, _L}, Acc) ->
    [F(Keys) | Acc];
r_path_visitor([P | Path], F, {Keys, Levels}, Acc) ->
    case maps:find(P, Levels) of
        error -> [];
        {ok, L} -> r_path_visitor(Path, F, L, [F(Keys) | Acc])
    end.

%%
%%
-spec insert(Path::path(), Key::key(), Value::term(), Pyramid::pyramid()) -> pyramid().
insert([], Key, Value, {Keys, Levels}) ->
    {maps:put(Key, Value, Keys), Levels};
insert([P | Path], Key, Value, {Keys, Levels}) ->
    Level = case maps:find(P, Levels) of
                error -> new();
                {ok, L} -> L
            end,
    {Keys, maps:put(P, insert(Path, Key, Value, Level), Levels)}.

-spec delete(Path::path(), Key::key(), Pyramid::pyramid()) -> pyramid().

delete([], Key, {Keys, Levels}) ->
    {maps:remove(Key, Keys), Levels};
delete([P | Path], Key, {Keys, Levels}) ->
    case maps:find(P, Levels) of
        error -> {Keys, Levels};
        {ok, L} -> {Keys, maps:update(P, delete(Path, Key, L), Levels)}
    end.

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

