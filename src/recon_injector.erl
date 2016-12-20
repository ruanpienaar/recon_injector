-module(recon_injector).
-export([main/1]).
-mode(compile).
-export([inject_recon/1,
	 purge_recon/1 
]).
-export([inject_module/2,
         purge_module/2
]).
-define(RECON_MODS, [recon, recon_alloc, recon_lib, recon_trace]).
%%---------------------------------------------------------------------------------

main(["inject",NodeStr,CookieStr]) ->
    Node = list_to_atom(NodeStr),
    Cookie = list_to_atom(CookieStr),
    connect_and_doit(Node, Cookie, fun() -> ?MODULE:inject_recon(Node) end);
main(["purge",NodeStr,CookieStr]) ->
    Node = list_to_atom(NodeStr),
    Cookie = list_to_atom(CookieStr),
    connect_and_doit(Node, Cookie, fun() -> ?MODULE:purge_recon(Node) end);
main(_) ->
    usage().

usage() ->
    io:format("./recon_injector [inject/purge] NODE COOKIE\n", []),
    timer:sleep(50).

%%---------------------------------------------------------------------------------

connect_and_doit(Node, Cookie, DoitFun) ->
    ok = application:start(hawk),
    HangingPid = self(),
    {ok,P} = hawk:add_node(
         Node, 
         Cookie, 
         [{recon_injector_connect, fun() -> DoitFun(), HangingPid ! done end}], 
         [{recon_injector_disconnect, fun() -> io:format("Node Disconnected\n") end}]
    ),
    erlang:link(P),
    receive
        R ->
            io:format("Hanging Received ~p ... \n", [R]),
            timer:sleep(50)
	after 1000 ->
            io:format("Timeout ... Closing ...\n"),
            timer:sleep(50)
    end.

inject_recon(Node) ->
    [ok = inject_module(Node, M) || M <- ?RECON_MODS].
    
purge_recon(Node) ->
    [ok = purge_module(Node, M) || M <- ?RECON_MODS].

%%---------------------------------------------------------------------------------

-spec inject_module(Mod :: term(), Node :: atom()) -> ok | {error, Reason :: term()}.
inject_module(Node, Mod) ->
    case code:get_object_code(Mod) of
        {Mod, Bin, File} ->
            case rpc:call(Node, code, load_binary,
                          [Mod, File, Bin]) of
                {module, Mod} ->
                    io:format("Loaded ~p on ~p~n", [Mod, Node]);
                {Error, Reason} when Error =:= error;
                                     Error =:= badrpc ->
                    {error, {load_binary_failed, Reason}}
            end;
        error ->
            {error, {get_object_code_failed, Mod}}
    end.

  purge_module(Node, Mod) ->
    Res = try rpc:call(Node, code, soft_purge, [Mod]) of
              true ->
                  ok;
              false ->
                  hard_purge_module(Node, Mod);
              {badrpc, _} = RPCError ->
                  {error, RPCError}
          catch
              C:E ->
                  {error, {C,E}}
          end,
    case Res of
        ok ->
            case rpc:call(Node, code, delete, [Mod]) of
                true ->
                    io:format("Purged ~p on ~p~n", [Mod, Node]);
                false ->
                    io:format("Could not purged ~p from ~p~n", [Mod, Node])
            end;
        {error, Error} ->
            io:format("Error while purging  ~p from ~p: ~p~n", [Mod, Node, Error])
    end.

hard_purge_module(Node, Mod) ->
    try rpc:call(Node, code, purge, [Mod]) of
        true ->
            ok;
        false ->
           io:format("Could not code:purge ~p~n", [Mod]);
        {badrpc, _} = RPCError ->
            {error, RPCError}
    catch
        C:E ->
            {error, {C,E}}
    end.