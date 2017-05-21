%%%-------------------------------------------------------------------
%% @doc trade_fsm public API
%% @end
%%%-------------------------------------------------------------------

-module(trade_fsm_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    trade_fsm_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================