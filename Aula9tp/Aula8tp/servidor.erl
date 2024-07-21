-module(servidor).
-export([start/1, stop/1, user/2]).

% Inicia o servidor de chat em uma porta específica
start(Port) -> 
    spawn(fun() -> server(Port, #{}) end).

% Para o servidor de chat
stop(Server) -> 
    Server ! stop.

% Função principal do servidor
server(Port, Rooms) ->
    % Abre um socket TCP na porta especificada
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {reuseaddr, true}]),
    
    % Inicia um processo para aceitar conexões
    spawn(fun() -> acceptor(LSock, Rooms) end),

    % Aguarda uma mensagem para parar o servidor
    receive
        stop -> 
            ok
    end.

% Processo para aceitar conexões de clientes
acceptor(LSock, Rooms) ->
    % Aguarda uma conexão
    {ok, Sock} = gen_tcp:accept(LSock),
    
    % Inicia o processo para lidar com o usuário conectado
    spawn(fun() -> user(Sock, Rooms) end),

    % Reinicia o acceptor para aceitar mais conexões
    acceptor(LSock, Rooms).

% Processo para lidar com as interações dos usuários
user(Sock, Rooms) ->
    receive
        % Quando uma linha de texto é recebida do cliente
        {tcp, _, Data} ->
            % Encaminha a mensagem para a sala de chat correspondente
            handle_message(Data, Sock, Rooms),
            user(Sock, Rooms);
        % Quando a conexão TCP é fechada pelo cliente
        {tcp_closed, _} ->
            % Remove o usuário de todas as salas de chat
            RoomsAfterLeave = leave_user(self(), Rooms),
            % Informa às salas de chat que o usuário saiu
            [RoomPid ! {leave, self()} || {_RoomName, RoomPid} <- RoomsAfterLeave],
            ok;
        % Quando ocorre um erro de TCP
        {tcp_error, _, _} ->
            % Remove o usuário de todas as salas de chat
            RoomsAfterLeave = leave_user(self(), Rooms),
            % Informa às salas de chat que o usuário saiu
            [RoomPid ! {leave, self()} || {_RoomName, RoomPid} <- RoomsAfterLeave],
            ok
    end.

% Obtém o nome da sala associada a um usuário
get_user_room(UserPid, Rooms) ->
    [{RoomName, _} || {RoomName, Users} <- maps:to_list(Rooms), lists:member(UserPid, Users)].

% Obtém o processo da sala de chat associado a um nome de sala
get_room(RoomName, Rooms) ->
    case maps:find(RoomName, Rooms) of
        {ok, RoomPid} -> RoomPid;
        error -> spawn(fun() -> room(RoomName, #{}) end)
    end.

% Adiciona um usuário a uma sala de chat
join_room(RoomName, UserPid, Rooms) ->
    Users = case maps:find(RoomName, Rooms) of
                {ok, CurrentUsers} -> CurrentUsers;
                error -> []
            end,
    NewUsers = Users ++ [UserPid],
    NewRooms = maps:put(RoomName, NewUsers, Rooms),
    NewRooms.

% Remove um usuário de uma sala de chat
leave_room(RoomName, UserPid, Rooms) ->
    case maps:find(RoomName, Rooms) of
        {ok, Users} ->
            NewUsers = lists:delete(UserPid, Users),
            NewRooms = case NewUsers of
                           [] -> maps:remove(RoomName, Rooms);
                           _ -> maps:put(RoomName, NewUsers, Rooms)
                      end,
            NewRooms;
        error ->
            Rooms
    end.

% Remove um usuário de todas as salas de chat
leave_user(UserPid, Rooms) ->
    lists:foldl(fun({RoomName, _RoomPid}, Acc) ->
                        leave_room(RoomName, UserPid, Acc)
                 end, Rooms, maps:keys(Rooms)).

% Processo para lidar com as interações dos usuários em uma sala de chat
room(RoomName, Users) ->
    receive
        {enter, UserPid} ->
            io:format("~p entrou na sala ~p~n", [UserPid, RoomName]),
            room(RoomName, Users ++ [UserPid]);
        {line, Message} ->
            io:format("Mensagem recebida na sala ~p: ~p~n", [RoomName, Message]),
            lists:foreach(fun(UserPid) -> UserPid ! {line, Message} end, Users),
            room(RoomName, Users);
        {leave, UserPid} ->
            io:format("~p saiu da sala ~p~n", [UserPid, RoomName]),
            room(RoomName, lists:delete(UserPid, Users))
    end.