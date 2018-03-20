%%%-------------------------------------------------------------------
%%% @doc
%%% MySQL Adapter.
%%% @end
%%%-------------------------------------------------------------------
-module(xdb_mysql_adapter).

-behaviour(xdb_adapter).

%% Adapter callbacks
-export([
  child_spec/2,
  insert/4,
  insert_all/4,
  update/5,
  delete/4,
  execute/5
]).

%% Defaults
-define(DEFAULT_POOL_SIZE, erlang:system_info(schedulers_online)).

%%%===================================================================
%%% Adapter callbacks
%%%===================================================================

%% @hidden
child_spec(Repo, Opts) ->
  Name = pool_name(Repo),
  PoolArgs = pool_args(Name, Opts),
  WorkersArgs = parse_opts([{repo, Repo} | Opts]),
  poolboy:child_spec(Name, PoolArgs, WorkersArgs).

%% @hidden
insert(_Repo, _Meta, Fields, _Opts) ->
  {ok, Fields}.

%% @hidden
insert_all(_Repo,_Meta, List, _Opts) ->
  {length(List), List}.

%% @hidden
update(_Repo, _Meta, Fields, _Filters, _Opts) ->
  {ok, Fields}.

%% @hidden
delete(_Repo, _Meta, _Filters, _Opts) ->
  Values = #{id => 1},
  {ok, Values}.

%% @hidden
execute(_Repo, _Op, _Meta, _Query, _Opts) ->
  {1, []}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
pool_name(Repo) ->
  list_to_atom(atom_to_list(Repo) ++ "_pool").

%% @private
pool_args(Name, Opts) ->
  PoolSize = xdb_lib:keyfind(pool_size, Opts, ?DEFAULT_POOL_SIZE),
  MaxOverflow = xdb_lib:keyfind(pool_max_overflow, Opts, PoolSize),

  [
    {name, {local, Name}},
    {worker_module, xdb_mysql_conn},
    {size, PoolSize},
    {max_overflow, MaxOverflow}
  ].

%% @private
parse_opts(Opts) ->
  _ = xdb_lib:keyfind(database, Opts),
  parse_opts(Opts, []) .

%% @private
parse_opts([], Acc) ->
  Acc;
parse_opts([{hostname, Value} | Opts], Acc) ->
  parse_opts(Opts, [{host, Value} | Acc]);
parse_opts([{username, Value} | Opts], Acc) ->
  parse_opts(Opts, [{user, Value} | Acc]);
parse_opts([{ssl_opts, Value} | Opts], Acc) ->
  parse_opts(Opts, [{ssl, Value} | Acc]);
parse_opts([Opt | Opts], Acc) ->
  parse_opts(Opts, [Opt | Acc]).
