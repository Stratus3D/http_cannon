-module(http_cannon_lua).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
          id,
          port,
          mbox
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    Id = random_test,
    Tracelevel = 1,
    {Clean_Id, Host, Lua_Node_Name} = mk_node_name(Id),
    Path = case code:priv_dir(http_cannon) of
        {error, bad_name} -> os:getenv("PATH");
        Folder -> Folder
    end,
    Command = "http_cannon.so",
    lager:info("Starting Lua VM..."),

    lager:info("path = ~s", [Path]),
    case os:find_executable(Command, Path) of
        false ->
            lager:info("Lua command not found", []),
            {stop, lua_not_found};
        Lua ->
            lager:info("Lua: ~w", [Lua]),
            Cmd =  mk_cmdline(Lua, Clean_Id, Host, Tracelevel),
            Port = open_port({spawn, Cmd}, [stream, {line, 100}, stderr_to_stdout, exit_status]),
            wait_for_startup(#state{id=Id, port=Port, mbox={lua, Lua_Node_Name}})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% Wait for the READY signal before confirming that our Lua Server is
% up and running.  Just echo out some of the chit chat coming from the
% Node program.
wait_for_startup(#state{port=Port} = State) ->
    receive
        {Port, {exit_status, N}} ->
            lager:error("error: ~w, state: ~w", [{exit_status, N}, State]),
            {stop, {exit_status, N}};
        {Port, {data, {eol, "READY"}}} ->
            lager:info("status: ~w, state: ~w", [ready, State]),
            {ok, State};
        {Port, {data, {eol, "."}}} ->
            wait_for_startup(State);
        {Port, {data, {eol, S}}} ->
            lager:info("status: ~w, state: ~w", [{startup, S}, State]),
            wait_for_startup(State);
        Err ->
            lager:error("some must have really gone wrong: ~w", [Err])
    end.

mk_node_name(Id) ->
    This_Id = atom_to_list(Id),
    This_Host = string:sub_word(atom_to_list(node()), 2, $@),
    {This_Id, This_Host, list_to_atom(lists:flatten([This_Id, "@", This_Host]))}.

mk_cmdline(Lua, Id, Host, Tracelevel) ->
    lists:flatten([
        Lua,
        quote(Id),
        quote(Host),
        quote(atom_to_list(node())),
        quote(atom_to_list(erlang:get_cookie())),
        quote(integer_to_list(Tracelevel))
    ]).

quote(S) ->
    case ostype() of
        win32 -> [" \"", S, "\""];
        unix -> [" '", S, "'"]
    end.

ostype() ->
    case os:type() of
        {Type, _} -> Type;
        Type -> Type
    end.
