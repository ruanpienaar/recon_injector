-module(recon_injector).
-export([main/1]).
-mode(compile).
-define(RECON_MODS, [recon, recon_alloc, recon_lib, recon_trace]).

main(["inject",NodeStr,CookieStr]) ->
    Node = list_to_atom(NodeStr),
    Cookie = list_to_atom(CookieStr),
    spike:connect_and_do(Node, Cookie, inject, ?RECON_MODS);
main(["purge",NodeStr,CookieStr]) ->
    Node = list_to_atom(NodeStr),
    Cookie = list_to_atom(CookieStr),
    spike:connect_and_do(Node, Cookie, purge, ?RECON_MODS);
main(_) ->
    usage().

usage() ->
    io:format("./recon_injector [inject/purge] NODE COOKIE\n", []),
    timer:sleep(50).