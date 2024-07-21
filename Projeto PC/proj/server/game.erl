-module(game).
-export([game/2]).

% Função principal para configurar e iniciar o jogo
game(P1,P2) ->
    % Inicia a configuração do jogo chamando a função setup com os jogadores P1 e P2 e um mapa vazio de jogadores
    setup(P1,P2, #{}).

% Função para configurar o jogo com os jogadores e o mapa de jogadores
setup({Pid1, User1}, {Pid2, User2}, Players) ->

    % Configura as teclas pressionadas inicialmente para o jogador 1
    Keys1 = #{},
    Keys1W = maps:put(w, false, Keys1),
    Keys1A = maps:put(a, false, Keys1W),
    Keys1D = maps:put(d, false, Keys1A),

    % Cria um jogador para o jogador 1 com a posição inicial e teclas pressionadas configuradas
    Player1 = spawnPlayer(User1, Players, 35, Keys1D),

    % Configura as teclas pressionadas inicialmente para o jogador 2
    Keys2 = #{},
    Keys2W = maps:put(w, false, Keys2),
    Keys2A = maps:put(a, false, Keys2W),
    Keys2D = maps:put(d, false, Keys2A),

    % Cria um jogador para o jogador 2 com a posição inicial e teclas pressionadas configuradas
    Player2 = spawnPlayer(User2, Players, 35, Keys2D),

    % Adiciona os jogadores criados ao mapa de jogadores
    Players1 = maps:put(Pid1, Player1, Players),
    Players2 = maps:put(Pid2, Player2, Players1),

    % Regista um processo para gerenciar o início e a execução do jogo
    % Este processo será responsável por enviar dados aos jogadores e monitorar o tempo de jogo
    register(sender,spawn(fun() -> 
        % Inicializa os dados do jogo com um mapa vazio
        Data = #{},

        % Adiciona os jogadores ao mapa de dados
        Data1 = maps:put(players, Players2, Data),

        % Obtém os PIDs dos jogadores
        Pids = maps:keys(Players2),

        % Cria e adiciona itens ao mapa de dados
        Items = spawnPlanets(4, [], 10),
        Data2 = maps:put(items, Items, Data1),

        % Inicia o jogo enviando uma mensagem de inicialização para todos os jogadores
        [Player ! {initMatch, Data2, self(), match_manager} || Player <- Pids],

        % Inicia o envio de dados do jogo
        dataSender(Data2, self())
    end)),

    % Registra um processo para gerenciar as teclas pressionadas pelos jogadores
    register(keysManager, spawn(fun() -> keysManager(Players2) end)).


% Função para criar um novo jogador
spawnPlayer(User, Players, Size, PressedKeys) ->
    % Gera coordenadas aleatórias para o jogador
    X = rand:uniform(1100),
    Y = rand:uniform(700),
    % Verifica se a posição gerada é válida, ou seja, se não colide com outras posições de jogadores
    Valid = validSpawn(X, Y, Size, maps:to_list(Players)),
    % Se a posição gerada for válida, cria um novo jogador com os parâmetros fornecidos
    if
        %Valid ->
        Valid andalso (X =< 200 orelse X >= 1000) ->
            % Cria um mapa representando o jogador com seus atributos iniciais
            Player = #{},
            Player1 = maps:put(user, User, Player),
            Player2 = maps:put(x, X, Player1),
            Player3 = maps:put(y, Y, Player2),
            Player4 = maps:put(pressedKeys, PressedKeys, Player3),
            Player6 = maps:put(speed, 0, Player4),
            Player7 = maps:put(angle, 0, Player6),
            Player8 = maps:put(acceleration, false, Player7),
            Player11 = maps:put(combustivel, 1000, Player8),
            Player12 = maps:put(jogo, 0, Player11),                 % 0 a jogar, 1 perdeu, 2 ganhou
            Player12; % Retorna o jogador criado
        true ->
            % Se a posição não for válida, tenta gerar uma nova posição e criar o jogador novamente
            spawnPlayer(User, Players, Size, PressedKeys)
    end.

% Função para verificar se a posição gerada para um novo jogador é válida
validSpawn(_, _, _, []) -> 
    true; % Se não houver outros jogadores na lista, a posição é sempre válida
validSpawn(X, Y, Size, [{_, Player} | Players]) ->
    % Obtém as coordenadas do jogador na lista
    {ok, X1} = maps:find(x, Player),
    {ok, Y1} = maps:find(y, Player),
    % Calcula a distância entre as coordenadas do jogador gerado e as coordenadas do jogador na lista
    Distance = math:sqrt(math:pow(X1 - X, 2) + math:pow(Y1 - Y, 2)),
    % Verifica se a distância entre os jogadores é maior que uma distância mínima (baseada no tamanho do jogador)
    if
        Distance < (Size * 2) + 1 -> % Se a distância for menor que o limite permitido, as posições colidem
            false; % Retorna falso indicando que a posição não é válida
        true ->
            validSpawn(X, Y, Size, Players) % Chama recursivamente a função para verificar os próximos jogadores na lista
    end.


% Função para gerar uma lista de itens no jogo
spawnPlanets(0, Items, _) ->
    Items; % Se não houver mais itens para gerar, retorna a lista de itens atualizada
spawnPlanets(Num, Items, Size) ->
    % Gera um novo item e adiciona à lista de itens
    Item = spawnPlaneta(Items, Size),
    % Chama recursivamente a função para gerar o próximo item
    spawnPlanets(Num-1, [Item | Items], Size).


% Função para gerar um novo item
spawnPlaneta(Items, Size) ->

    % Gera coordenadas aleatórias para o item
    SemiMajor = rand:uniform(300 - 150 + 1) + 150 - 1,
    SemiMinor = rand:uniform(200 - 100 + 1) + 100 - 1,

    TwoPi = 2 * math:pi(),
    Angle = rand:uniform() * TwoPi,

    X = 640 + SemiMajor * math:cos(Angle),
    Y = 360 + SemiMajor * math:cos(Angle),

    % Verifica se a posição gerada para o item é válida, ou seja, se não colide com outras posições de itens
    Valid = validPlanetSpawn(X, Y, Size, Items),
    % Se a posição gerada for válida, cria o item com o tipo e coordenadas especificadas
    if
        Valid ->
            % Cria um mapa representando o item com seu tipo e coordenadas
            Item = #{},
            Item2 = maps:put(x, X, Item),
            Item3 = maps:put(y, Y, Item2),
            Item4 = maps:put(angle, Angle, Item3),
            Item6 = maps:put(centerX, 640, Item4),
            Item7 = maps:put(centerY, 360, Item6),
            Item8 = maps:put(semiMajor, SemiMajor, Item7),
            Item9 = maps:put(semiMinor, SemiMinor, Item8),
            Item10 = maps:put(size, rand:uniform(10), Item9),
            Item10; % Retorna o item criado

        true ->
            % Se a posição não for válida, tenta gerar uma nova posição e criar o item novamente
            spawnPlaneta(Items, Size)
    end.


% Função para verificar se a posição gerada para um item é válida
validPlanetSpawn(_, _, _, []) -> 
    true; % Se não houver outros itens na lista, a posição é sempre válida
validPlanetSpawn(X, Y, Size, [Item | Items]) ->
    % Obtém as coordenadas do item na lista
    X1 = maps:get(x, Item),
    Y1 = maps:get(y, Item),
    % Calcula a distância entre as coordenadas do item gerado e as coordenadas do item na lista
    Distance = math:sqrt(math:pow(X1 - X, 2) + math:pow(Y1 - Y, 2)),
    % Verifica se a distância entre os itens é maior que uma distância mínima (baseada no tamanho do item)
    if 
        Distance < (Size * 2) + 50 -> % Se a distância for menor que o limite permitido, as posições colidem
            false; % Retorna falso indicando que a posição não é válida
        true ->
            validPlanetSpawn(X, Y, Size, Items) % Chama recursivamente a função para verificar os próximos itens na lista
    end.

% Função para gerenciar a comunicação de dados entre os processos do jogo
dataSender(Data, Pid) ->
    receive
        {quit, From, User} ->
            % Realiza ações de encerramento do jogo quando um jogador sai
            Players = maps:get(players, Data), % Obtém os dados dos jogadores
            Player = maps:get(from, Players),
            surrender(From, Player, Data), % Realiza ações de encerramento do jogo
            %winers(Data),
            match_manager ! {leave, From, User}; % Notifica o gerenciador de partida sobre a saída do jogador
            %success; % Indica sucesso na execução

        {From, PressedKeys} ->
            % Atualiza as teclas pressionadas de um jogador com base na mensagem recebida
            Players = maps:get(players, Data), % Obtém os dados dos jogadores
            Player = maps:get(From, Players), % Obtém os dados do jogador específico
            PlayerUpdated = maps:put(pressedKeys, PressedKeys, Player), % Atualiza as teclas pressionadas do jogador
            PlayersUpdated = maps:put(From, PlayerUpdated, Players), % Atualiza os dados dos jogadores
            DataUpdated = maps:put(players, PlayersUpdated, Data), % Atualiza os dados gerais do jogo
            % Chama recursivamente a função com os dados atualizados
            dataSender(DataUpdated, Pid)
    after
        20 -> % Define um tempo limite para a execução do código seguinte
            % Simula as atualizações e envio de dados a cada iteração do jogo
            DataUpdated = simulate(Data, Pid), % Simula as atualizações dos dados do jogo
            % Chama recursivamente a função com os dados atualizados após a simulação
            dataSender(DataUpdated, Pid)
    end.

calculateSunAttraction(Player, SunX, SunY, SunAttractionForce) ->
    PlayerX = maps:get(x, Player),
    PlayerY = maps:get(y, Player),
    DistanceX = SunX - PlayerX,
    DistanceY = SunY - PlayerY,
    Distance = math:sqrt(math:pow(DistanceX, 2) + math:pow(DistanceY, 2)),
    % Calcular a direção da atração
    DirectionX = DistanceX / Distance,
    DirectionY = DistanceY / Distance,
    % Aplicar a força de atração
    AttrX = SunAttractionForce * DirectionX,
    AttrY = SunAttractionForce * DirectionY,
    % Atualizar a posição do jogador
    NewX = PlayerX + AttrX,
    NewY = PlayerY + AttrY,
    % Retornar o jogador atualizado
    % Retornar o jogador atualizado
    maps:put(x, NewX, maps:put(y, NewY, Player)).

sunAttraction([], PlayersMap) -> PlayersMap;
sunAttraction([{From, Data} | T], PlayersMap) ->
    % Coordenadas do centro do "sol"
    SunX = 640,
    SunY = 360,
    % Constante de atração solar
    SunAttractionForce = 0.9,

    Player2 = calculateSunAttraction(Data, SunX, SunY, SunAttractionForce),

    UpdatedData = maps:put(From, Player2, PlayersMap),

    sunAttraction(T, UpdatedData).



% Simula as atualizações dos dados do jogo e gerencia o envio desses dados aos jogadores
simulate(MatchData, Pid) ->
    % Obtém os dados dos jogadores
    Players = maps:get(players, MatchData),
    % Obtém os PIDs dos jogadores
    Pids = maps:keys(Players),

    %io:format("players: ~w~n",[maps:to_list(Players)]),

    Players1 = sunAttraction(maps:to_list(Players), #{}),

    % Realiza ações com base nas teclas pressionadas pelos jogadores
    {UpdatedPlayers, _} = keyPressedAction(maps:to_list(Players1), #{}, []),

    %io:format("UPDATED Players: ~w~n",[UpdatedPlayers]),

    % Atualiza os dados dos jogadores com base nas ações realizadas
    UpdatedMatchData = maps:update(players, UpdatedPlayers, MatchData),
  
    %%% atualizar posicao dos itens
    NewItems = newPosPlanet(UpdatedMatchData),
    %io:format("MAPA: ~w~n",[NewItems]),
    UpdatedMatchData22 = maps:update(items, NewItems, UpdatedMatchData),
    %io:format("data2: ~p~n",[UpdatedMatchData22]),
    %%% 
    % Verifica e executa ações relacionadas a colisões com itens e jogadores
    UpdatedMatchData1 = eatingKillingActions(UpdatedMatchData22, self()),

    receive 
        {timer} ->  
            MatchFinalData = winers(UpdatedMatchData1),
            MatchFinalData
    
    after 10 ->
        %io:format("data1: ~p~n",[UpdatedMatchData1]),
        % Verifica se houve alterações nos jogadores e itens
        OldPlayers = maps:get(players, MatchData),
        NewItems = maps:get(items, UpdatedMatchData1),
        %io:format("oo: ~w~n",[UpdatedMatchData1]),

        % Prepara os dados a serem enviados aos jogadores
        DataToSend = #{},
        % Verifica se houve alterações nos jogadores
        if
            OldPlayers == UpdatedPlayers ->
                DataToSend11 = maps:put(items, NewItems, DataToSend),
                DataToSend1 = DataToSend11; % Não houve alterações
            true ->
                DataToSend11 = maps:put(items, NewItems, DataToSend),
                DataToSend1 = maps:put(players, UpdatedPlayers, DataToSend11) % Atualiza os dados dos jogadores
        end,
        % Envia os dados atualizados aos jogadores
        % Verifica se há dados para enviar
        case maps:size(DataToSend1) of
            0 ->
                pass; % Não há dados para enviar
            _ ->
                % Envia os dados atualizados a cada jogador
                [PlayerPid ! {updateInfo, PlayerPid, DataToSend1, Pid} || PlayerPid <- Pids]
        end,
        
        UpdatedMatchData1 % Retorna os dados atualizados do jogo
    end.


newPosPlanet(MatchData) ->
    Items = maps:get(items, MatchData),
    UpdatedItems = updatePlanetPositions(Items),
    UpdatedItems.

updatePlanetPositions([]) ->
    []; % Se a lista de itens estiver vazia, retorna uma lista vazia
updatePlanetPositions([Item | RestItems]) ->

    Angle = maps:get(angle, Item),
    CX = maps:get(centerX, Item),
    CY = maps:get(centerY, Item),
    SemiMajor = maps:get(semiMajor, Item),
    SemiMinor = maps:get(semiMinor, Item),

    % Calcula as novas posições usando as funções trigonométricas
    NewX = CX + SemiMajor * math:cos(Angle),
    NewY = CY + SemiMinor * math:sin(Angle),
    NewAngle = Angle + 0.02, % Atualiza o ângulo para a próxima posição

    % Atualiza o mapa do item com as novas coordenadas e ângulo
    Item1 = maps:update(x, NewX, Item),
    Item2 = maps:update(y, NewY, Item1),
    Item3 = maps:update(angle, NewAngle, Item2),

    % Chama recursivamente a função para os itens restantes na lista
    [Item3 | updatePlanetPositions(RestItems)].



% Gerencia as teclas pressionadas pelos jogadores
keysManager(Players) ->
    receive
        % Recebe a mensagem indicando uma mudança na tecla pressionada por um jogador
        {keyChanged, Key, Status, From} ->
            % Obtém os dados do jogador correspondente ao remetente da mensagem
            Player = maps:get(From, Players),
            % Obtém as teclas pressionadas pelo jogador
            PressedKeys = maps:get(pressedKeys, Player),
            % Verifica o status da tecla pressionada
            case Status of
                "true" ->
                    % Se a tecla foi pressionada
                    case Key of
                        "w" ->
                            % Atualiza a tecla 'w' como pressionada
                            UpdatedPressedKeys = maps:put(w, true, PressedKeys),
                            % Envia a nova tecla pressionada ao jogador
                            sender ! {From, UpdatedPressedKeys},
                            % Atualiza os dados do jogador com a nova tecla pressionada
                            PlayerUpdated = maps:put(pressedKeys, UpdatedPressedKeys, Player),
                            % Atualiza os dados dos jogadores com os dados atualizados do jogador
                            PlayersUpdated = maps:put(From, PlayerUpdated, Players),
                            % Chama recursivamente a função para continuar gerenciando as teclas
                            keysManager(PlayersUpdated);
                        "d" ->
                            % Atualiza a tecla 'd' como pressionada (mesmo processo que para a tecla 'w')
                            UpdatedPressedKeys = maps:put(d, true, PressedKeys),
                            sender ! {From, UpdatedPressedKeys},
                            PlayerUpdated = maps:put(pressedKeys, UpdatedPressedKeys, Player),
                            PlayersUpdated = maps:put(From, PlayerUpdated, Players),
                            keysManager(PlayersUpdated);
                        "a" ->
                            % Atualiza a tecla 'a' como pressionada (mesmo processo que para a tecla 'w')
                            UpdatedPressedKeys = maps:put(a, true, PressedKeys),
                            sender ! {From, UpdatedPressedKeys},
                            PlayerUpdated = maps:put(pressedKeys, UpdatedPressedKeys, Player),
                            PlayersUpdated = maps:put(From, PlayerUpdated, Players),
                            keysManager(PlayersUpdated);
                        _ -> 
                            ok % Outras teclas não são tratadas
                    end;
                "false" ->
                    % Se a tecla foi liberada, processo similar ao caso em que ela foi pressionada
                    case Key of
                        "w" ->
                            UpdatedPressedKeys = maps:put(w, false, PressedKeys),
                            sender ! {From, UpdatedPressedKeys},
                            PlayerUpdated = maps:put(pressedKeys, UpdatedPressedKeys, Player),
                            PlayersUpdated = maps:put(From, PlayerUpdated, Players),
                            keysManager(PlayersUpdated);
                        "d" ->
                            UpdatedPressedKeys = maps:put(d, false, PressedKeys),
                            sender ! {From, UpdatedPressedKeys},
                            PlayerUpdated = maps:put(pressedKeys, UpdatedPressedKeys, Player),
                            PlayersUpdated = maps:put(From, PlayerUpdated, Players),
                            keysManager(PlayersUpdated);
                        "a" ->
                            UpdatedPressedKeys = maps:put(a, false, PressedKeys),
                            sender ! {From, UpdatedPressedKeys},
                            PlayerUpdated = maps:put(pressedKeys, UpdatedPressedKeys, Player),
                            PlayersUpdated = maps:put(From, PlayerUpdated, Players),
                            keysManager(PlayersUpdated);
                        _ -> 
                            ok
                    end
            end;
        % Recebe a mensagem indicando que um jogador saiu do jogo
        {leave, User, Pid} ->
            % Notifica o gerenciador do jogo sobre a saída do jogador
            sender ! {quit, User, Pid}
    end.


keysActions(Speed, Angle, Acceleration, Combustivel, []) ->
    {Speed, Angle, Acceleration, Combustivel};
% Atualiza as ações do jogador com base nas teclas pressionadas
keysActions(Speed, Angle, Acceleration, Combustivel, [{Key, Status} | T]) ->
    % Verifica qual tecla foi pressionada e seu status
    case {Key, Status} of
        {w, true} ->
            % Se a tecla 'w' foi pressionada, atualiza a velocidade e a aceleracao do jogador
            UpdatedAcc = true,
            %UpdatedSpeed = SpeedVariation,
            case Combustivel of
                0 ->
                    UpdatedSpeed = 0, % Se o combustível for 0, zera a velocidade
                    %io:format("Combustivel Terminou~n"),
                    UpdatedCombustivel = 0;
                _ ->
                    UpdatedSpeed = 3.5, % Caso contrário, mantém a velocidade
                    %io:format("Menos combustivel~n"),
                    UpdatedCombustivel = Combustivel - 1
            end,

            UpdatedAngle = Angle; % A direcao do jogador permanece a mesma

        {w, false} ->
            % Se a tecla 'w' foi liberada, desativa a aceleracao e zera a velocidade do jogador
            UpdatedAcc = false,
            UpdatedSpeed = 0,
            UpdatedCombustivel = Combustivel,
            UpdatedAngle = Angle; % A direcao do jogador permanece a mesma

        {d, true} -> 
            % Se a tecla 'd' foi pressionada, atualiza a aceleracao e a direcao do jogador
            UpdatedAcc = Acceleration,
            UpdatedSpeed = Speed,
            UpdatedCombustivel = Combustivel,
            Temp = Angle + 0.1, % Incrementa a direcao do jogador
            Pi = math:pi() * 2,
            if
                Temp > Pi ->
                    UpdatedAngle = Temp - Pi; % Se a direcao ultrapassar 2Ï€, subtrai 2Ï€ para mantÃª-la dentro do intervalo
                true ->
                    UpdatedAngle = Temp
            end;
        {d, false} ->
            % Se a tecla 'd' foi liberada, mantÃ©m a aceleracao e a direaao do jogador
            UpdatedAcc = Acceleration,
            UpdatedSpeed = Speed,
            UpdatedCombustivel = Combustivel,
            UpdatedAngle = Angle;
        {a, true} -> 
            % Se a tecla 'a' foi pressionada, atualiza a aceleraaao e a direaao do jogador
            UpdatedAcc = Acceleration,
            UpdatedSpeed = Speed,
            UpdatedCombustivel = Combustivel,
            Temp = Angle - 0.1, % Decrementa a direaao do jogador
            if
                Temp < 0 ->
                    UpdatedAngle = (math:pi() * 2) + Temp; % Se a direaao se tornar negativa, adiciona 2Ï€ para mantÃª-la dentro do intervalo
                true ->
                    UpdatedAngle = Temp
            end;
        {a, false} -> 
            % Se a tecla 'a' foi liberada, mantÃ©m a aceleraaao e a direaao do jogador
            UpdatedAcc = Acceleration,
            UpdatedSpeed = Speed,
            UpdatedCombustivel = Combustivel,
            UpdatedAngle = Angle
    end,
    % Chama recursivamente a funcao para processar as teclas restantes
    keysActions(UpdatedSpeed, UpdatedAngle, UpdatedAcc,  UpdatedCombustivel, T).


% Atualiza as ações de todos os jogadores com base nas teclas pressionadas
keyPressedAction([], Result, PlayersUpdated) ->
    % Se não houver mais jogadores para processar, retorna o resultado final
    {Result, PlayersUpdated};
keyPressedAction([{From, Data} | T], Result, PlayersUpdated) ->
    case maps:get(jogo, Data) of
        0 ->
            % Extrai os dados relevantes do jogador atual
            PressedKeys = maps:get(pressedKeys, Data),
            X = maps:get(x, Data),
            Y = maps:get(y, Data),
            Angle = maps:get(angle, Data),
            Speed = maps:get(speed, Data),
            Acceleration = maps:get(acceleration, Data),
            Combustivel = maps:get(combustivel, Data),

            % Calcula as novas ações do jogador com base nas teclas pressionadas
            {UpdatedSpeed, UpdatedAngle, UpdatedAcc, UpdatedCombustivel} = keysActions(Speed, Angle, Acceleration, Combustivel, maps:to_list(PressedKeys)),

            % Atualiza a posição do jogador com base nas novas ações
            {UpdatedX, UpdatedY} = updatePosition(X, Y, UpdatedSpeed, UpdatedAngle),

            % Verifica se houve alguma mudança nas ações do jogador
            case {UpdatedX, UpdatedY, UpdatedSpeed, UpdatedAngle, UpdatedAcc} of
                {X, Y, Speed, Angle, Acceleration} ->
                    % Se não houve mudança nas ações, o jogador permanece inalterado
                    Update = false,
                    ResultUpdated = maps:put(From, Data, Result);
                _ ->
                    % Se houve mudança nas ações, atualiza os dados do jogador com as novas ações
                    Update = true,
                    DataUpdated = maps:put(x, UpdatedX, Data),
                    DataUpdated1 = maps:put(y, UpdatedY, DataUpdated),
                    DataUpdated2 = maps:put(speed, UpdatedSpeed, DataUpdated1),
                    DataUpdated3 = maps:put(angle, UpdatedAngle, DataUpdated2),
                    DataUpdated4 = maps:put(acceleration, UpdatedAcc, DataUpdated3),
                    DataUpdated5 = maps:put(combustivel, UpdatedCombustivel, DataUpdated4),
                    ResultUpdated = maps:put(From, DataUpdated5, Result)
            end,

            % Adiciona o jogador atual à lista de jogadores atualizados se houve alguma mudança
            if
                Update ->
                    NewPlayersUpdated = [From | PlayersUpdated];
                true ->
                    NewPlayersUpdated = PlayersUpdated
            end;
        _ ->
            ResultUpdated = maps:put(From, Data, Result),
            NewPlayersUpdated = PlayersUpdated
    end,
    % Chama recursivamente a função para processar o próximo jogador na lista
    keyPressedAction(T, ResultUpdated, NewPlayersUpdated).

updatePosition(X, Y, Speed, Angle) ->
    UpdatedX = X + (Speed * math:cos(Angle)),
    UpdatedY = Y + (Speed * math:sin(Angle)),
    {UpdatedX, UpdatedY}.


% Verifica e realiza ações de comer e matar
eatingKillingActions(MatchData, Pid) ->
    % Obtém os jogadores e os itens do estado do jogo
    Players = maps:get(players, MatchData),
    Items = maps:get(items, MatchData),
    Keys = maps:keys(Players),

    % Verifica e atualiza as colisões entre jogadores
    {MapUpdated} = checkPlayersColisions(maps:to_list(Players), maps:to_list(Players), Players, Keys),
    %io:format("MAPA: ~w",[MapUpdated]),
    % Verifica e atualiza as colisões entre jogadores e itens
    {UpdatedPlayers1} = checkPlayersPlanetsColision(maps:to_list(MapUpdated), Items, MapUpdated, Pid),

    % Atualiza o estado do jogo com os jogadores e itens atualizados
    UpdatedMatchData = maps:put(players, UpdatedPlayers1, MapUpdated),
    UpdatedMatchData1 = maps:put(items, Items, UpdatedMatchData),

    UpdatedMatchData1.


% Verifica e atualiza as colisÃµes entre jogadores
checkPlayersColisions([], _, PlayersMap, _) ->
    % Se a lista de jogadores estiver vazia, retorna o mapa de jogadores e os PIDs dos jogadores mortos
    {PlayersMap};
checkPlayersColisions([{PidPredator, Predator} | T], PlayersList, PlayersMap, DeadPids) ->
    % Verifica as colisÃµes do jogador atual com todos os outros jogadores na lista
    Mapa = playerColisions({PidPredator, Predator}, PlayersList, PlayersMap),
    
    % Chama recursivamente a funcao para processar os prÃ³ximos jogadores na lista
    checkPlayersColisions(T, PlayersList, Mapa, DeadPids).

playerColisions(_, [], PlayersMap) ->
    % Se a lista de jogadores estiver vazia, retorna os PIDs dos jogadores mortos
    PlayersMap;
playerColisions({PidPredator, Predator}, [{PidVictim, Victim} | T], PlayersMap) ->
    % Verifica se o jogador predador e a vÃ­tima sao o mesmo jogador
    if
        PidPredator == PidVictim ->
            % Se forem o mesmo jogador, continua para o prÃ³ximo jogador vÃ­tima
            playerColisions({PidPredator, Predator}, T, PlayersMap);
        true ->
            % Se nao forem o mesmo jogador, verifica se houve colisao entre eles
            Bool = colisionPlayerAction(Predator, Victim),
            if
                Bool ->
                    % Se houve colisao
                    {NewPredator, NewVictim} = handleElasticCollision(Predator, Victim),

                    UpdatedPlayersMap1 = maps:put(PidPredator, NewPredator, PlayersMap),
                    UpdatedPlayersMap2 = maps:put(PidVictim, NewVictim, UpdatedPlayersMap1),

                    playerColisions({PidPredator, NewPredator}, T, UpdatedPlayersMap2);
                true ->
                    playerColisions({PidPredator, Predator}, T, PlayersMap)
            end
    end.


% funcao para tratar a colisao elastica
handleElasticCollision(Predator, Victim) ->
    % ObtÃ©m as velocidades e Ã¢ngulos dos jogadores
    {ok, Speed1} = maps:find(speed, Predator),
    {ok, Angle1} = maps:find(angle, Predator),
    {ok, Speed2} = maps:find(speed, Victim),
    {ok, Angle2} = maps:find(angle, Victim),

    % ObtÃ©m as posicÃµes dos jogadores
    {ok, PosX1} = maps:find(x, Predator),
    {ok, PosY1} = maps:find(y, Predator),
    {ok, PosX2} = maps:find(x, Victim),
    {ok, PosY2} = maps:find(y, Victim),

    % Calcula as novas velocidades apÃ³s a colisao elastica
    {NewSpeed1, NewAngle1, NewSpeed2, NewAngle2} = elasticCollision(Speed1, Angle1, PosX1, PosY1, Speed2, Angle2, PosX2, PosY2),

    % Atualiza os estados dos jogadores
    NewPredator = maps:put(speed, NewSpeed1, maps:put(angle, NewAngle1, Predator)),
    NewVictim = maps:put(speed, NewSpeed2, maps:put(angle, NewAngle2, Victim)),
    
    {NewPredator, NewVictim}.


% funcao que calcula as novas velocidades e Ã¢ngulos apÃ³s uma colisao elastica
elasticCollision(Speed1, Angle1, PosX1, PosY1, Speed2, Angle2, PosX2, PosY2) ->
    %io:format("TOU NO ELASTIC~n"),
    DX = PosX2 - PosX1,
    DY = PosY2 - PosY1,

    % Velocidades projetadas ao longo da linha de colisao
    VX1 = Speed1 * math:cos(Angle1),
    VY1 = Speed1 * math:sin(Angle1),
    VX2 = Speed2 * math:cos(Angle2),
    VY2 = Speed2 * math:sin(Angle2),

    % Componentes das velocidades ao longo da linha de colisao
    P1 = VX1 * DX + VY1 * DY,
    P2 = VX2 * DX + VY2 * DY,

    % Troca as velocidades
    NewP1 = P2,
    NewP2 = P1,

    % Velocidades finais em coordenadas X e Y
    NewVX1 = NewP1 * DX,
    NewVY1 = NewP1 * DY,
    NewVX2 = NewP2 * DX,
    NewVY2 = NewP2 * DY,

    % Novas velocidades e Ã¢ngulos
    NewSpeed1 = math:sqrt(math:pow(NewVX1, 2) + math:pow(NewVY1, 2)),
    NewAngle1 = math:atan2(NewVY1, NewVX1),
    NewSpeed2 = math:sqrt(math:pow(NewVX2, 2) + math:pow(NewVY2, 2)),
    NewAngle2 = math:atan2(NewVY2, NewVX2),

    {NewSpeed1, NewAngle1, NewSpeed2, NewAngle2}.


colisionPlayerAction(Predator, Victim) ->
    PiMid = math:pi()/2,
    PredatorDirection = maps:get(angle, Predator),
    
    PredatorX = maps:get(x, Predator),
    PredatorY = maps:get(y, Predator),
    VictimX = maps:get(x, Victim),
    VictimY = maps:get(y, Victim),

    Distance = math:sqrt(math:pow(PredatorX - VictimX, 2) + math:pow(PredatorY - VictimY, 2)),
    Angle = math:atan2(VictimY - PredatorY, VictimX - PredatorX),
    AngleDiff = normalizeAngle(Angle - PredatorDirection),

    EstadoPredator = maps:get(jogo, Predator),
    EstadoVitimia = maps:get(jogo, Victim),

    if
        (Distance < (35*2-1)) and (AngleDiff < PiMid) and (EstadoPredator == 0) and (EstadoVitimia == 0) ->
            true;
        true ->
            false
    end.


checkPlayersPlanetAction(Player, Item) -> 
    PlayerX = maps:get(x, Player),
    PlayerY = maps:get(y, Player),
    ItemX = maps:get(x, Item),
    ItemY = maps:get(y, Item),
    ItemSize = maps:get(size, Item), 
    Distance = math:sqrt(math:pow(PlayerX - ItemX, 2) + math:pow(PlayerY - ItemY, 2)),

    SunDistance = math:sqrt(math:pow(PlayerX - 640, 2) + math:pow(PlayerY - 360, 2)),

    EstadoPlayer = maps:get(jogo, Player),

    if
        (Distance < (35 + ItemSize + 5 - 1)) and (EstadoPlayer == 0) ->         % raio player + raio planeta
            true;
        
        (SunDistance < (35 + 50 - 1)) and (EstadoPlayer == 0) ->                % raio player + raio sol
            true;
        
        % caso o player saia do limite do jogo
        (PlayerX < 0 orelse PlayerX > 1280 orelse PlayerY < 0 orelse PlayerY > 720) and (EstadoPlayer == 0) ->
            true;  

        true ->
            false
    end.


% Função para verificar colisões entre jogadores e itens no jogo
checkPlayersPlanetsColision([], _, PlayersMap, _) ->
    % Se a lista de jogadores estiver vazia, retorna o mapa de jogadores e os itens não comidos
    {PlayersMap};

checkPlayersPlanetsColision(_, [], PlayersMap, _) ->
    % Se a lista de itens estiver vazia, retorna o mapa de jogadores e os itens não comidos
    {PlayersMap};

checkPlayersPlanetsColision([{From, Data} | T], Items, PlayersMap, Pid) ->
    % Para cada jogador na lista de jogadores, verifica a colisão com os itens
    {UpdatedPlayersMap} = playerItemsColisions({From, Data}, Items, PlayersMap, Pid),
    % Chama recursivamente para processar os jogadores restantes
    checkPlayersPlanetsColision(T, Items, UpdatedPlayersMap, Pid).


% Função para verificar colisões entre um jogador e os itens
playerItemsColisions(_, [], PlayersMap, _) ->
    % Se a lista de itens estiver vazia, retorna o mapa de jogadores e os itens não comidos
    {PlayersMap};

playerItemsColisions({From, Player}, [Item | Items], PlayersMap, Pid) ->
    % Verifica a colisão entre o jogador e o item atual
    case checkPlayersPlanetAction(Player, Item) of
        true ->
            io:format("Colisão Player Item~n"),

            UpdatedPlayerMap = surrender(From, Player, PlayersMap),
            
            spawn(fun() -> 
                receive 
                    after 5000 -> Pid ! {timer}
                end 
            end),

            playerItemsColisions({From, Player}, [], UpdatedPlayerMap, Pid);
        false ->
            % Se não houver colisão, atualiza o mapa de jogadores e chama recursivamente
            UpdatedPlayersMap = maps:put(From, Player, PlayersMap),
            playerItemsColisions({From, Player}, Items, UpdatedPlayersMap, Pid)
    end.

surrender(From, Player, PlayersMap) ->
    PlayerLost = maps:update(jogo, 1, Player),
    UpdatedPlayers = maps:update(From, PlayerLost, PlayersMap),

    UpdatedPlayers.

winers(PlayersMap) ->

    Players = maps:get(players, PlayersMap),

    NovosP = maps:map(fun(_, Player) ->
                        case maps:get(jogo, Player) of
                                0 ->
                                    maps:update(jogo, 2, Player);
                                _ ->
                                    Player
                        end
                    end, Players),

    UpdatedMatchData = maps:update(players, NovosP, PlayersMap),
    
    % Retorna o mapa atualizado
    UpdatedMatchData.

normalizeAngle(Angle) ->
    NormalizedAngle = math:fmod(Angle + 2 * math:pi(), 2 * math:pi()),
    NormalizedAngle.