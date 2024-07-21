-module(login_manager).
-export([start/0,
        create_account/2,
        close_account/2,
        login/2,
        logout/1,
        online/0]).

start() ->
    register(login_manager, spawn(fun()->loop(#{}) end)).   % #{} <- mapa vazio

rpc(Request) ->
    ?MODULE ! {Request, self()}
    receive {Result, ?MODULE} -> Result end.

create_account(Username, Passwd)->
    rpc(create_account, Username, Passwd).

close_account(Username, Passwd) -> 
    rpc(close_account, Username, Passwd).

login(Username, Passwd) -> 
    rpc(login, Username, Passwd).

logout(Username) -> 
    rpc(logout, Username).

online() -> 
    rpc(online).


loop(Map)->
    receive
        {{create_account, Username, Passwd}, From} -> 
            case maps:is_key(Username, Map) of
                true ->
                    From ! {user_exists, ?MODULE},
                    loop(Map);
                false ->
                    From ! {ok, ?MODULE},
                    loop(maps:put(Username, {Passwd, true}, Map))   % Associa username => {passwd,true}
            end; 

        {{close_account, Username, Passwd}, From} -> 
            case maps:find(Username, Map) of
                {ok, {Passwd, _}} ->
                    From ! {invalid, ?MODULE},
                    loop(Map);
                _ ->
                    From ! {ok, ?MODULE},
                    loop(maps:remove(Username, Map))   
            end; 

        {{login, Username, Passwd}, From} ->
            case maps:find(Username, Map) of
                {ok, {Passwd, false}} ->
                    From ! {ok, ?MODULE},
                    loop(maps:update(Username, {Passwd, true}, Map));
                _ ->
                    From ! {invalid, ?MODULE},
                    loop(Map)   
            end; 

        {{logout, Username}, From} ->
            From ! {ok, ?MODULE},
            case maps:find(Username,Map) of
                {ok, {Passwd, true}} ->
                    loop(maps:update(Username, {Passwd, false}, Map));
                _ ->
                    loop(Map)

        {{online}, From} ->
            Users = {Username || {Username, {_,true}} <- maps:to_list(Map)}
            From ! {Users, ?MODULE},
            loop(Map)
        end
    end.