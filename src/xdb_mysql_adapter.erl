%%%-------------------------------------------------------------------
%%% @doc
%%% MySQL Adapter.
%%% @end
%%%-------------------------------------------------------------------
-module(xdb_mysql_adapter).

-behaviour(xdb_adapter).

%% Supervisor callbacks
-export([init/1]).

%% Adapter callbacks
-export([
  child_spec/2,
  insert/4,
  insert_all/4,
  update/5,
  delete/4,
  execute/5
]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @hidden
init([]) ->
  {ok, {{one_for_one, 0, 1}, []}}.

%%%===================================================================
%%% Adapter callbacks
%%%===================================================================

%% @hidden
child_spec(_Repo, _Opts) ->
  {
    ?MODULE,
    {supervisor, start_link, [{local, ?MODULE}, ?MODULE, []]},
    permanent,
    infinity,
    supervisor,
    [?MODULE]
  }.

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
  {1, [person:schema(#{id => 1})]}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
