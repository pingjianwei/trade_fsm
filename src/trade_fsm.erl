%%%-------------------------------------------------------------------
%%% @author pingjianwei
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 五月 2017 11:43
%%%-------------------------------------------------------------------
-module(trade_fsm).
-author("pingjianwei").

-behaviour(gen_fsm).

%% API
-export([start_link/0]).

%% gen_fsm callbacks
-compile(export_all).
-export([init/1,
  state_name/2,
  state_name/3,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4]).


-define(SERVER, ?MODULE).

-record(state, {
  name="",
  other,
  ownitems=[],
  otheritems=[],
  monitor,
  from
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() -> {ok, pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, StateName :: atom(), StateData :: #state{}} |
  {ok, StateName :: atom(), StateData :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Name]) ->
  {ok, idle, #state{name=Name}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @end
%%--------------------------------------------------------------------
-spec(state_name(Event :: term(), State :: #state{}) ->
  {next_state, NextStateName :: atom(), NextState :: #state{}} |
  {next_state, NextStateName :: atom(), NextState :: #state{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
state_name(_Event, State) ->
  {next_state, state_name, State}.

%%给玩家发送一条通知，可以时发送给玩家进程的一条消息
%%不过在此处时打印在shell中
notice(#state{name=N},Str,Args) ->
  io:format("~s:"++ Str ++"~n",[N|Args]).
%%记录非期望的消息
unexpected(Msg,State) ->
  io:format("~p received unknown event ~p while in state ~p~n",[self(),Msg,State]).
%%======================================idle=============================================
%%接受其他玩家的交易请求
idle({ask_negotiate,OtherPid},S=#state{}) ->
  Ref = monitor(process,OtherPid),
  notice(S,"~p asked for a negotiation",[OtherPid]),
  {next_state,idle_wait,S#state{other = OtherPid,monitor = Ref}};
idle(Event,Data) ->
  unexpected(Event,idle),
  {next_state,idle,Data}.
%%自己请求fsm向另一个玩家发起交易请求
idle({neotiate,OtherPid},From,S=#state{}) ->
  ask_negotiate(OtherPid,self()),
  notice(S,"asking user ~p for a trade",[OtherPid]),
  Ref = monitor(process,OtherPid),
  {next_state,idle_wait,S#state{other = OtherPid,monitor = Ref,from = From}};
idle(Event,_From,Data) ->
  unexpected(Event,idle),
  {next_state,idle,Data}.

%%===============================idle_wait==================================
idle_wait({ask_negotiate,OtherPid},S=#state{other = OtherPid}) ->
  gen:reply(S#state.from ,ok),
  notice(S,"Starting negotiation",[]),
  {next_state,negotiate,S};
idle_wait({accept_negotiate,OtherPid},S=#state{other = OtherPid}) ->
  gen:reply(S#state.from ,ok),
  notice(S,"Starting negotiation",[]),
  {next_state,negotiate,S};
idle_wait(Event,Data) ->
  unexpected(Event,idle),
  {next_state,idle,Data}.


idle_wait(accept_negotiate,_From,S=#state{other = OtherPid}) ->
  accept_negotiate(OtherPid,self()),
  notice(S,"accepting negotiation",[]),
  {reply,ok,negotiate,S};
idle_wait(Event,_From,Data) ->
  unexpected(Event,idle),
  {next_state,idle,Data}.

%%==========================交易物品存储==============================
%%向物品列表中增加一件物品
add(Item,Items) ->
  [Item|Items].
remove(Item,Items) ->
  Items -- [Item].

%%=================================negotiate之make，retract,do,undo =================================
negotiate({make_offer,Item},S=#state{ownitems = OwnItems}) ->
  do_offer(S#state.other,Item),
  notice(S,"offering ~p",[Item]),
  {next_state,negotiate,S#state{ownitems = add(Item,OwnItems)}};
%%本方撤销一件交易物品
negotiate({retract_offer,Item},S=#state{ownitems = OwnItems}) ->
  undo_offer(S#state.other,Item),
  notice(S,"cancelling offer on ~p",[Item]),
  {next_state,negotiate,S#state{ownitems = remove(Item,OwnItems)}};
%%对方提供一件交易物品
negotiate({do_offer,Item},S=#state{otheritems = OtherItems}) ->
  notice(S,"other player offering ~p",[Item]),
  {next_state,negotiate,S#state{otheritems = add(Item,OtherItems)}};
%%对方撤销一件交易物品
negotiate({undo_offer,Item},S=#state{otheritems = OtherItems}) ->
  notice(S,"other player cancelling offer on ~p",[Item]),
  {next_state,negotiate,S#state{otheritems = remove(Item,OtherItems)}};
%%=================================negotiate之are_you_ready =================================
negotiate(are_you_ready,S=#state{other = OtherPid}) ->
  io:format("Other user ready to trade ~n"),
  notice(S,
    "Other user ready to transfer goods; ~n"
    "you get ~p ,The other side get ~p",
    [S#state.ownitems,S#state.ownitems]),
  not_yet(OtherPid);
negotiate(Event,Data) ->
  unexpected(Event,negotiate),
  {next_state,negotiate,Data}.

negotiate(ready,From,S=#state{other = OtherPid}) ->
  are_you_ready(OtherPid),
  io:format("asking if ready,waiting",[]),
  {next_state,wait,S#state{from = From}};

negotiate(Event,_From,S) ->
  unexpected(Event,negotiate),
  {next_state,negotiate,S}.
%%====================================wait:对放添加物品或者撤销物品====================================
%%对方添加物品或者撤销物品时，我方自动回到negotiate状态
wait({do_offer,Item},S=#state{otheritems = OtherItems}) ->
  gen_fsm:reply(S#state.from,offer_changed),
  notice(S,"other side offering ~p",[Item]),
  {next_state,negotiate,S#state{otheritems = add(Item,OtherItems)}};
wait({undo_offer,Item},S=#state{otheritems = OtherItems}) ->
  gen_fsm:reply(S#state.from,offer_changed),
  notice(S,"other side cancelling offer of ~p",[Item]),
  {next_state,negotiate,S#state{otheritems = remove(Item,OtherItems)}};
%%========================================wait:are_you_ready情况===============================================
wait(are_you_ready,S=#state{}) ->
  am_ready(S#state.other),
  notice(S,"asked if ready,and I am.Waiting for same reply ",[]),
  {next_state,wait,S};
wait(am_ready,S=#state{}) ->
  am_ready(S#state.other),
  notice(S,"Other not ready yet",[]),
  {next_state,wait,S};
wait('ready!',S=#state{}) ->
  am_ready(S#state.other),
  ack_trans(S#state.other),
  gen_fsm:reply(S#state.from,ok),
  notice(S,"Other side is ready.Moving to ready state",[]),
  {next_state,wait,S};
wait(Event,Data) ->
  unexpected(Event,negotiate),
  {next_state,wait,Data}.
%%===================================进程标识符比较大小==========================================
priority(OwnPid,OtherPid) when OwnPid >OtherPid -> true;
priority(OwnPid,OtherPid) when OwnPid <OtherPid -> fasle.
%%===================================ready状态==========================================
ready(ack,S=#state{}) ->
  case priority(self(),S#state.other) of
   true ->
     try
         notice(S,"asking for commit",[]),
         ready_commit =ask_commit(S#state.other),
         notice(S,"ording commit",[]),
         ok = do_commit(S#state.other),
         notice(S," commit commiting",[]),
         commit(S),
         {stop,noral,S}
     catch Class:Reason  ->
       %% 退出！ ready_commit 或者 do_commit 失败了
       notice(S,"commit falled",[]),
       {stop,{Class,Reason},S}
     end ;
    false ->
      {nest_state,ready,S}
  end;
ready(Event,Data) ->
  unexpected(Event,negotiate),
  {next_state,ready,Data}.

ready(ask_commit,_From,S) ->
  notice(S,"replying to ask_commit",[]),
  {reply,ready_commit,reay,S};
ready(do_commit,_From,S) ->
  notice(S,"commiting......",[]),
  commit(S),
  {stop,nomal,ok,S};
ready(Event,_From,Data) ->
  unexpected(Event,ready),
  {next_state,ready,Data}.
%%===================================commit函数实现================================
commit(S=#state{}) ->
  io:format("Transaction completed for ~s"
    "This operation shouid hava some atomic save"
    "in a database.~n",
    [S#state.name,S#state.ownitems,S#state.otheritems]
  ).





%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(state_name(Event :: term(), From :: {pid(), term()},
    State :: #state{}) ->
  {next_state, NextStateName :: atom(), NextState :: #state{}} |
  {next_state, NextStateName :: atom(), NextState :: #state{},
    timeout() | hibernate} |
  {reply, Reply, NextStateName :: atom(), NextState :: #state{}} |
  {reply, Reply, NextStateName :: atom(), NextState :: #state{},
    timeout() | hibernate} |
  {stop, Reason :: normal | term(), NewState :: #state{}} |
  {stop, Reason :: normal | term(), Reply :: term(),
    NewState :: #state{}}).
state_name(_Event, _From, State) ->
  Reply = ok,
  {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_event(Event :: term(), StateName :: atom(),
    StateData :: #state{}) ->
  {next_state, NextStateName :: atom(), NewStateData :: #state{}} |
  {next_state, NextStateName :: atom(), NewStateData :: #state{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewStateData :: #state{}}).
%%对方玩家发送了取消事件
%%停止了正在做的的事，终止了进程
handle_event(cancel,_StateName,S=#state{}) ->
  notice(S,"received cancel event",[]),
  {stop,other_canclled,S};
handle_event(Event,StateName,Data) ->
  unexpected(Event,StateName),
  {next_state,StateName,Data}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()},
    StateName :: atom(), StateData :: term()) ->
  {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
  {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {next_state, NextStateName :: atom(), NewStateData :: term()} |
  {next_state, NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
  {stop, Reason :: term(), NewStateData :: term()}).
%%当我们取消交易，不要忘记通知对方玩家
handle_sync_event(cancel, _From, _StateName, S = #state{}) ->
  notify_cancel(S#state.other),
  notice(S,"Cancelling trade,sending cancel event",[]),
  {stop, cancelled,ok ,S};
handle_sync_event(Event, _From, StateName, Data) ->
  unexpected(Event,StateName),
  {next_state,StateName,Data}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: term(), StateName :: atom(),
    StateData :: term()) ->
  {next_state, NextStateName :: atom(), NewStateData :: term()} |
  {next_state, NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {stop, Reason :: normal | term(), NewStateData :: term()}).
handle_info({'down',Ref,process,Pid,Reason},_,S = #state{other = Pid,monitor = Ref}) ->
  notice(S,"Other side dead",[]),
  {stop, {other_down,Reason}, S};
handle_info(Info, StateName, State) ->
  unexpected(Info,StateName),
  {next_state, StateName, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: normal | shutdown | {shutdown, term()}
| term(), StateName :: atom(), StateData :: term()) -> term()).
terminate(normal,ready, S=#state{}) ->
  notice(S,"FSM leaving.",[]);
terminate(_Reason,_StateName, _StateData) ->
  ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
    StateData :: #state{}, Extra :: term()) ->
  {ok, NextStateName :: atom(), NewStateData :: #state{}}).
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%% =============================公共API==========================
start(Name) ->
  gen_fsm:start(?MODULE,[Name],[]).
start_link([Name]) ->
  gen_fsm:start_link(?MODULE,[Name],[]).
%% 请求开始交易会话，当/如果对方接受返回
trade(OwnPid,OtherPid) ->
  gen_fsm:sync_send_event(OwnPid,{negotiate,OtherPid},3000).
%% 接受某个玩家的交易请求
accept_trade(OwnPid) ->
  gen_fsm:sync_send_event(OwnPid,accpt_negotiate).
%% 从物品表中选择一个物品进行交易
make_offer(OwnPid,Item) ->
  gen_fsm:send_event(OwnPid,{make_offer,Item}).
%% 撤销某个交易物品
  retract_offer(OwnPid,Item) ->
    gen_fsm:send_event(OwnPid,{retract_offer,Item}).
%% 宣布自己就绪，当对方宣布自己就绪时，交易就完成了
ready(OwnPid) ->
  gen_fsm:sync_send_event(OwnPid,ready,infinity).
%% 取消交易
cancel(OwnPid) ->
  gen_fsm:sync_send_all_state_event(OwnPid,cancel).
%%==============================公共API=====================================

%%=======================FSM到FSM的函数====================================

%%向另外一个FSM发起交易会话请求
ask_negotiate(OtherPid,OwnPid) ->
  gen_fsm:send_event(OtherPid,{ask_negotiate,OwnPid}).
%%转发玩家交易接受信息
accept_negotiate(OtherPid,OwnPid) ->
  gen_fsm:send_event(OtherPid,{accept_negotiate,OwnPid}).
%%转发玩家交易物品提供信息
do_offer(OtherPid,Item) ->
  gen_fsm:send_event(OtherPid,{do_offer,Item}).
%%转发玩家交易物品撤销信息
undo_offer(OtherPid,Item) ->
  gen_fsm:send_event(OtherPid,{undo_offer,Item}).
%%询问对方是否就绪
are_you_ready(OtherPid) ->
  gen_fsm:send_event(OtherPid,ask_you_ready).

%%回复未就绪，也就说，是不在‘wait’状态
not_yet(OtherPid) ->
  gen_fsm:send_event(OtherPid,not_yet).
%%通知对方玩家处于等待，进入‘ready’状态，状态会迁移到‘ready’
am_ready(OtherPid) ->
  gen_fsm:send_event(OtherPid,'ready!').

%%确认fsm处理ready状态
ack_trans(OtherPid) ->
  gen_fsm:send_event(OtherPid,ack).
%%询问是否可以交易
ask_commit(OtherPid) ->
  gen_fsm:sync_send_event(OtherPid,ask_commit).
%%开始同步提交
do_commit(OtherPid) ->
  gen_fsm:sync_send_event(OtherPid,do_commit).
%%取消提交时，通知对方的fsm
notify_cancel(OtherPid) ->
  gen_fsm:send_all_state_event(OtherPid,cancel).







