-module(xdb_mysql_adapter_SUITE).

%% Common Test
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

%% Test Cases
-export([
  t_pool/1
]).

%% Test Cases
-include_lib("mixer/include/mixer.hrl").
-mixin([
  {xdb_repo_basic_test, [
    % CT
    init_per_testcase/2,
    end_per_testcase/2

    % Test Cases
    % t_insert/1,
    % t_insert_errors/1,
    % t_insert_on_conflict/1,
    % t_insert_all/1,
    % t_insert_all_on_conflict/1,
    % t_update/1,
    % t_delete/1,
    % t_get/1,
    % t_get_by/1,
    % t_all/1,
    % t_all_with_pagination/1,
    % t_delete_all/1,
    % t_delete_all_with_conditions/1,
    % t_update_all/1,
    % t_update_all_with_conditions/1
  ]}
]).

-define(EXCLUDED_FUNS, [
  module_info,
  all,
  init_per_suite,
  end_per_suite,
  init_per_testcase,
  end_per_testcase
]).

-include_lib("stdlib/include/ms_transform.hrl").

%%%===================================================================
%%% CT
%%%===================================================================

-spec all() -> [atom()].
all() ->
  Exports = ?MODULE:module_info(exports),
  [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

-spec init_per_suite(xdb_ct:config()) -> xdb_ct:config().
init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(cross_db_mysql),
  [{repo, xdb_mysql_repo} | Config].

-spec end_per_suite(xdb_ct:config()) -> ok.
end_per_suite(_) ->
  ok = application:stop(cross_db_mysql).

%%%===================================================================
%%% Test Cases
%%%===================================================================

-spec t_pool(xdb_ct:config()) -> ok.
t_pool(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),
  Pool = list_to_atom(atom_to_list(Repo) ++ "_pool"),
  {ready, 5, 0, 0} = poolboy:status(Pool),
  ok.
