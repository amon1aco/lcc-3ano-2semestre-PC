-module(servidor).
-import(user_manager, [create_account/4, delete_account/4, login/4, logout/4, is_logged_in/3, user_manager/1]).
-import(file_manager, [readContent/1, file_write/2, file_manager/0]).
-import(game, [game/2]).
-export([start/1, servidor/1, stop/1, matchMan/1, acceptor/1, userMan/1]).

% Inicia o servidor na porta especificada
start(Port) -> register(?MODULE, spawn(fun() -> servidor(Port) end)).

% Para o servidor
stop(servidor) -> servidor ! stop.

% Função principal do servidor
servidor(Port) ->
    % Inicia a escuta na porta especificada
    Result = gen_tcp:listen(Port, [binary, {packet, line}]),
    case Result of
        {ok, LSock} ->
            % Inicia o gerenciador do lobby
            register(match_manager, spawn(fun() -> matchMan([]) end)),
            % Carrega contas de usuário do arquivo
            Filename = "file.txt",
            Accounts = file_manager:readContent(Filename),
            % Inicia o gerenciamento de contas
            register(accounts_manager, spawn(fun() -> user_manager(Accounts) end)),
            % Inicia o gerenciamento de arquivos
            register(file_manager, spawn(fun() -> file_manager() end)),
            % Inicia o processo de aceitação de conexões
            acceptor(LSock);
        {error, Reason} ->
            io:fwrite("Error generating socket\n"),
            Reason
    end.

% Processa as conexões de entrada
acceptor(LSock) ->
    case gen_tcp:accept(LSock) of 
        {ok, Socket} ->
            spawn(fun() -> acceptor(LSock) end), % Aceita outras conexões
            userMan(Socket); % Inicia o processo de autenticação do usuário
        {error, closed} ->
            io:fwrite("Closed socket~n");
        {error, system_limit} ->
            io:fwrite("Limit of socket reached");
        {error, _} ->
            io:fwrite("Error listening to socket")
    end.

% Comunicador com o accounts_manager
fazRequest({Request, User, Password}) -> 
    accounts_manager ! {Request, self(), User, Password},
    receive
        {Result, accounts_manager} ->
            Result
    end.

% Lida com a autenticação do usuário
userMan(Sock) ->
    receive
        {tcp, _, Data} ->
            ListOfInfo = re:replace(Data,"\\n|\\r", "",[global,{return,list}]),
            Info = string:split(ListOfInfo, ",", all),
            case Info of

                ["create_account", User, Password] -> % Solicitação para criar uma conta
                    Result = fazRequest({create_account, User, Password}), % Chama a função para criar uma conta
                    case Result of
                        user_exists -> % Se o usuário já existe
                            gen_tcp:send(Sock, "User already exists!\n"), % Envia uma mensagem de erro
                            userMan(Sock); % Reinicia a autenticação do usuário
                        account_created -> % Se a conta foi criada com sucesso
                            gen_tcp:send(Sock, "User created!\n"), % Envia uma mensagem de sucesso
                            accounts_manager ! write_data, % Escreve as alterações no arquivo
                            userMan(Sock) % Reinicia a autenticação do usuário
                    end;

                ["login", User, Password] -> % Solicitação para fazer login
                    Result = fazRequest({login, User, Password}), % Chama a função para fazer login
                    case Result of
                        invalid_user -> % Se o usuário não existe
                            gen_tcp:send(Sock, "User does not exist!\n"); % Envia uma mensagem de erro
                        invalid_pwd -> % Se a senha é inválida
                            gen_tcp:send(Sock, "Invalid password!\n"); % Envia uma mensagem de erro
                        login_sucessfully -> % Se o login foi bem-sucedido
                            gen_tcp:send(Sock, "Logged in with success!\n") % Envia uma mensagem de sucesso
                    end,
                    userMan(Sock); % Reinicia a autenticação do usuário

                ["logout", User, Password] -> % Solicitação para fazer logout
                    Result = fazRequest({logout, User, Password}), % Chama a função para fazer logout
                    case Result of
                        invalid_user -> % Se o usuário não existe
                            gen_tcp:send(Sock, "User does not exist!\n"); % Envia uma mensagem de erro
                        logout_successfully -> % Se o logout foi bem-sucedido
                            gen_tcp:send(Sock, "Logged out with success!\n") % Envia uma mensagem de sucesso
                    end,
                    userMan(Sock);

                ["statistics", _] -> % Solicitação para obter estatísticas
                    accounts_manager ! {statistics, self()}, % Solicita estatísticas ao gerenciador de contas
                    %io:format("pedido de estatisticas~n"),
                    receive
                        Stats -> % Recebe as estatísticas
                            gen_tcp:send(Sock, Stats) % Envia as estatísticas para o cliente
                            %io:format("enviei isto ~p~n",[Stats])
                    end,
                    userMan(Sock); % Reinicia a autenticação do usuário

                ["delete_account", User, Password] -> % Solicitação para excluir uma conta
                    Result = fazRequest({delete_account, User, Password}), % Chama a função para excluir a conta
                    case Result of
                        invalid_user -> % Se o usuário não existe
                            gen_tcp:send(Sock, "User does not exist!\n"); % Envia uma mensagem de erro
                        invalid_Password -> % Se a senha é inválida
                            gen_tcp:send(Sock, "Invalid password!\n"); % Envia uma mensagem de erro
                        account_deleted -> % Se a conta foi excluída com sucesso
                            gen_tcp:send(Sock, "Account deleted with success!\n") % Envia uma mensagem de sucesso
                    end,
                    accounts_manager ! write_data, % Escreve as alterações no arquivo
                    userMan(Sock); % Reinicia a autenticação do usuário

                ["join", User] -> % Solicitação para entrar no matchMan
                    Result = fazRequest({is_logged_in, User, "algo"}), % Verifica se o usuário está logado
                    case Result of
                        true -> % Se o usuário está logado
                            gen_tcp:send(Sock, "User joined the lobby!\n"), % Envia uma mensagem de sucesso
                            accounts_manager ! {user_level, User, self()}, % Solicita o nível do usuário ao gerenciador de contas
                            %io:format("antes~n"),
                            receive
                                Level -> % Recebe o nível do usuário
                                    match_manager ! {join, self(), User, Level}, % Informa ao gerenciador de jogos que um usuário entrou no lobby
                                    Match = gameStart(Sock, User), % Inicializa o jogo para o usuário
                                    gameState(Sock, User, Match) % Inicia o fluxo do jogo para o usuário
                            end,

                            gen_tcp:send(Sock, "User joined the lobby!\n"),
                            userMan(Sock); % Envia uma mensagem de sucesso
                        false -> % Se o usuário não está logado
                            gen_tcp:send(Sock, "User not logged in!\n"), % Envia uma mensagem de erro
                            userMan(Sock) % Reinicia a autenticação do usuário
                    end;

                _ -> io:fwrite("Error!\n") % Se a requisição não é reconhecida, exibe uma mensagem de erro
            end;
        _ -> userMan(Sock) % Se a mensagem não for do tipo tcp, continua esperando
    end.


% Lobby do servidor
% match_manager
matchMan(Pids) ->
    receive
        {join, From, User, UserLevel} -> % Solicitação para entrar no matchMan
            io:format("Pedido de ~p para entrar~n", [User]),
            if
                Pids == [] -> % Se o matchMan está vazio
                    NewPids = [{From, User, UserLevel}], % Adiciona o jogador ao matchMan
                    io:format("Lobby agora: ~p~n", [NewPids]),
                    matchMan(NewPids); % Continua no matchMan
                true ->
                    case lists:member({From, User, UserLevel}, Pids) of % Verifica se o jogador já está no matchMan
                        false -> % Se o jogador não está no matchMan
                            TempPid =[{From, User, UserLevel} | Pids], % Adiciona o jogador ao matchMan
                            case matchMaking(TempPid) of % Verifica se há jogadores para iniciar uma partida
                                {User1, User2} -> % Se há jogadores para iniciar uma partida
                                    {From1, Username1, _} = User1,
                                    {From2, Username2, _} = User2,
                                    
                                    % Inicia o jogo
                                    spawn(fun() -> game({From1, Username1}, {From2, Username2}) end), % Inicia um jogo entre os jogadores

                                    io:format("User 1~p~n", [User1]),
                                    io:format("User 2~p~n", [User2]),
                                    io:format("Lobby~p~n", [TempPid]),

                                    NewPids = lists:delete(User1, Pids), % Remove os jogadores do lobby
                                    NewPids2 = lists:delete(User2, NewPids), % Remove os jogadores do lobby

                                    io:format("Lobby resultante:~p.~n", [NewPids2]),
                                    matchMan(NewPids2); % Continua no lobby
                                false -> % Se não há jogadores suficientes para iniciar uma partida
                                    matchMan([Pids | {From, User, UserLevel}]) % Continua no lobby
                            end;
                        true -> % Se o jogador já está no lobby
                            io:format("User ~w já se encontra no lobby!~n", [User]),
                            matchMan(Pids)
                    end
            end;
        {leave, From, User} -> % Solicitação para sair do matchMan
            NewPids = lists:delete({From, User}, Pids), % Remove o jogador do lobby
            matchMan(NewPids) % Continua no matchMan
    end.


% Função para fazer o matchmaking entre jogadores
matchMaking([]) -> false;
matchMaking([H | T]) ->
    Result = levelDiff(H, T), % Verifica se há jogadores com nível similar
    case Result of
        {true, User1, User2} -> {User1, User2}; % Se há jogadores com nível similar, retorna esses jogadores
        _ -> matchMaking(T) % Se não há jogadores com nível similar, continua o matchmaking
    end.

% Função para verificar se há jogadores com nível similar (diferença máxima de 1)
levelDiff(_, []) -> false;
levelDiff(Elem, [H | T]) -> 
    {_, _, Level} = Elem, % Obtém o nível do jogador atual
    case H of 
        {_, _, Level2} -> % Se o jogador atual tem um nível
            if
                abs(Level - Level2) =< 1 -> % Se a diferença de níveis é menor ou igual a 1
                    {true, Elem, H}; % Retorna true e os jogadores com nível similar
                true ->
                    levelDiff(Elem, T) % Se a diferença de níveis é maior que 1, continua verificando
            end;
        _ -> 
            levelDiff(Elem, []) % Se o jogador atual não tem um nível, continua verificando
    end.

% Inicializa o jogo para um usuário
gameStart(Sock, User) ->
    receive
        {initMatch, Data, MatchPid, match_manager}->
            gen_tcp:send(Sock, "Start\n"), % Envia uma mensagem de início para o cliente
            enviaDataInicial(Sock, Data), % Envia os dados iniciais do jogo para o cliente
            MatchPid; % Retorna o PID do jogo

        _ ->
            io:fwrite("User ~s left the match.~n", [User]), % Exibe uma mensagem se o usuário sair do jogo
            fazRequest({logout, User, "algo"}), % Faz logout do usuário
            match_manager ! {leave, User, self()} % Informa ao gerenciador de jogos que o usuário saiu do jogo
    end.

% Encerra o jogo para um usuário
gameOver(Sock, User, Flag) ->
    if
        Flag == 2 -> % Se o usuário ganhou
            gen_tcp:send(Sock, "You won!\n"), % Envia uma mensagem de vitória para o cliente
            io:format("~p Ganhou!~n",[User]),
            accounts_manager ! {user_win, User}, % Atualiza as estatísticas do usuário
            Result = fazRequest({logout, User, "algo"}), % Chama a função para fazer logout
            case Result of
                invalid_user -> % Se o usuário não existe
                    io:format("ERRO no logout do ~p~n",[User]);
                logout_successfully -> % Se o logout foi bem-sucedido
                    io:format("Logout do ~p~n",[User])
            end,
            accounts_manager ! write_data; % Escreve as alterações no arquivo
            
        Flag == 1 -> % Se o usuário perdeu
            gen_tcp:send(Sock, "You lost!\n"),
            io:format("~p Perdeu!~n",[User]),
            accounts_manager ! {user_lose, User},
            Result = fazRequest({logout, User, "algo"}), % Chama a função para fazer logout
            case Result of
                invalid_user -> % Se o usuário não existe
                    io:format("ERRO no logout do ~p~n",[User]);
                logout_successfully -> % Se o logout foi bem-sucedido
                    io:format("Logout do ~p~n",[User])
            end,
            accounts_manager ! write_data           % Envia uma mensagem de derrota para o cliente
    end.


% Fluxo do jogo para um usuário
gameState(Sock, User, MatchPid) ->
    receive
        {updateInfo, Pid, UpdatedData, MatchPid} -> % Se houver dados atualizados do jogo
            Players = maps:get(players, UpdatedData),
            Player = maps:get(Pid, Players),
            Estado = maps:get(jogo, Player),
            
            if
                Estado == 0 ->
                    enviaData(Sock, UpdatedData), % Envia os dados atualizados para o cliente
                    gameState(Sock, User, MatchPid); % Continua o fluxo do jogo
                true ->
                    gameOver(Sock, User, Estado)
            end;
            
        {tcp, _, Data} -> % Se receber dados do cliente
            Info = re:replace(Data,"\\n|\\r", "",[global,{return,list}]), % Remove caracteres especiais dos dados
            Info1 = string:split(Info,",",all), % Divide os dados recebidos
            case Info1 of
                ["KeyChanged", Key, "True"] -> % Se houve uma mudança de tecla
                    keysManager ! {keyChanged, Key, "true", self()}; % Informa ao gerenciador de chaves
                ["KeyChanged", Key, "False"] -> % Se não houve uma mudança de tecla
                    keysManager ! {keyChanged, Key, "false", self()}; % Informa ao gerenciador de chaves
                _ ->
                    ok % Se não foram recebidos dados relevantes
            end,
            gameState(Sock, User, MatchPid); % Continua o fluxo do jogo
        
        _ ->
            fazRequest({logout, User, "algo"}), % Faz logout do usuário
            match_manager ! {leave, User, self()} % Informa ao gerenciador de jogos que o usuário saiu do jogo
    end.


% Envia os dados dos jogadores para o cliente
enviaPlayersInfo(Sock, Players) ->
    ListOfPlayers = [Player || {_, Player} <- maps:to_list(Players)], % Obtém a lista de jogadores
    enviaPlayer(Sock, ListOfPlayers). % Envia os dados dos jogadores para o cliente


% Envia os dados de um jogador para o cliente
enviaPlayer(Sock, []) -> % Se não houver mais jogadores
    gen_tcp:send(Sock, "\n"); % Envia uma mensagem vazia para o cliente
enviaPlayer(Sock, [H | T]) -> % Se houver jogadores
    User = maps:get(user, H), % Obtém o nome do jogador
    X = maps:get(x, H), % Obtém a posição X do jogador
    Y = maps:get(y, H), % Obtém a posição Y do jogador
    Angle = maps:get(angle, H), % Obtém o ângulo do jogador
    Combustivel = maps:get(combustivel, H),
    Estado = maps:get(jogo, H),
    gen_tcp:send(Sock, io_lib:fwrite("P,~s,~w,~w,~w,~w,~w;", [User, X, Y, Angle, Combustivel, Estado])), % Envia os dados do jogador para o cliente
    enviaPlayer(Sock, T). % Envia os dados dos próximos jogadores para o cliente


% Envia os dados dos itens para o cliente
enviaPlanetasInfo(Sock,[]) -> % Se não houver itens
    gen_tcp:send(Sock, "\n"); % Envia uma mensagem vazia para o cliente
enviaPlanetasInfo(Sock, [H|T]) -> % Se houver itens
    X = maps:get(x,H), % Obtém a posição X do item
    Y = maps:get(y,H), % Obtém a posição Y do item
    Size = maps:get(size,H),
    gen_tcp:send(Sock, io_lib:fwrite("I,~w,~w,~w;", [X, Y, Size])), % Envia os dados do item para o cliente
    enviaPlanetasInfo(Sock, T). % Envia os dados dos próximos itens para o cliente


% Envia os dados atualizados do jogo para o cliente
enviaData(Sock, Data) ->
    case maps:find(players, Data) of % Verifica se há dados atualizados dos jogadores
        {ok, Players} ->
            enviaPlayersInfo(Sock, Players); % Envia os dados dos jogadores para o cliente
        error ->
            pass % Continua
    end,

    case maps:find(items, Data) of % Verifica se há dados atualizados dos itens
        {ok, Items} ->
            enviaPlanetasInfo(Sock, Items); % Envia os dados dos itens para o cliente
        error ->
            pass % Continua
    end.


% Envia os dados iniciais do jogo para o cliente
enviaDataInicial(Sock, Data) ->
    Players = maps:get(players, Data), % Obtém os dados dos jogadores
    Planetas = maps:get(items, Data), % Obtém os dados dos itens
    enviaPlayersInfo(Sock, Players), % Envia os dados dos jogadores para o cliente
    enviaPlanetasInfo(Sock, Planetas). % Envia os dados dos itens para o cliente