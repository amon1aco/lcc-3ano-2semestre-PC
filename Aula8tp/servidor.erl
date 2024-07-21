-module(servidor).
-export([start/1, stop/1, user/2]).

% Inicia o servidor de chat em uma porta específica
start(Port) -> 
    spawn(fun() -> server(Port) end).

% Para o servidor de chat
stop(Server) -> 
    Server ! stop.

% Função principal do servidor
server(Port) ->
    % Abre um socket TCP na porta especificada
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {reuseaddr, true}]),

    % Cria uma sala de chat como um processo
    Room = spawn(fun()-> room([]) end),

    % Inicia um processo para aceitar conexões
    spawn(fun() -> acceptor(LSock, Room) end),

    % Aguarda uma mensagem para parar o servidor
    receive
        stop -> 
            ok
    end.

% Processo para aceitar conexões de clientes
acceptor(LSock, Room) ->
    % Aguarda uma conexão
    {ok, Sock} = gen_tcp:accept(LSock),

    % Inicia um novo processo para aceitar outras conexões
    spawn(fun() -> acceptor(LSock, Room) end),

    % Envia uma mensagem para a sala de chat indicando que um novo usuário entrou
    Room ! {enter, self()},

    % Inicia o processo para lidar com o usuário conectado
    user(Sock, Room).

% Processo para lidar com as interações dos usuários na sala de chat
room(Pids) ->
    receive
        % Quando um usuário entra na sala de chat
        {enter, Pid} ->
            io:format("Usuário entrou~n", []),
            room([Pid | Pids]);

        % Quando uma linha de texto é recebida de um usuário
        {line, Data} = Msg ->
            io:format("Mensagem recebida: ~p~n", [Data]),

            % Reenvia a mensagem para todos os usuários na sala de chat
            [Pid ! Msg || Pid <- Pids],
            room(Pids);

        % Quando um usuário deixa a sala de chat
        {leave, Pid} ->
            io:format("Usuário saiu~n", []),
            
            % Remove o identificador de processo do usuário que saiu
            room(Pids -- [Pid])
    end.

user(Sock, Room) ->
    receive
        % Quando uma linha de texto é recebida do cliente
        {line, Data} ->
            % Envie a linha de texto de volta para o cliente
            gen_tcp:send(Sock, Data),

            % Continue a receber mensagens do cliente
            user(Sock, Room);

        % Quando dados TCP são recebidos do cliente
        {tcp, _, Data} ->
            % Envie os dados para a sala de chat para serem processados
            Room ! {line, Data},

            % Continue a receber mensagens do cliente
            user(Sock, Room);

        % Quando a conexão TCP é fechada pelo cliente
        {tcp_closed, _} ->
            % Informe à sala de chat que o cliente saiu
            Room ! {leave, self()};

        % Quando ocorre um erro de TCP
        {tcp_error, _, _} ->
            % Informe à sala de chat que o cliente saiu
            Room ! {leave, self()}
    end.
