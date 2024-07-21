-module(jogo_impl).
-export([start/0, participa/1, timeout/1]).

-include_lib("kernel/include/logger.hrl"). % Inclui o arquivo de cabeçalho do logger para usar a função de log (opcional).

% Definição do tempo limite em milissegundos para a partida.
-define(TIMEOUT, 60000).

% Registro que define o estado da partida, contendo o número a ser adivinhado, o número de tentativas, e flags para indicar se houve timeout ou se o jogador ganhou.
-record(partida_state, {number = random:uniform(100), tentativas = 0, timeout = false, ganhou = false}).

% Função para iniciar a partida.
start() ->
    % Inicia a partida com um estado inicial vazio.
    Partida = #partida_state{},
    % Inicia um processo separado para lidar com o timeout da partida.
    {ok, spawn(fun() -> timeout(Partida) end), Partida}.

% Função para que o jogador participe da partida.
participa(#partida_state{tentativas = Tentativas} = Partida) ->
    % Incrementa o número de tentativas.
    NovasTentativas = Tentativas + 1,
    % Verifica diferentes condições para determinar o resultado da tentativa do jogador.
    if Tentativas > 100 -> "TENTATIVAS";
       Partida#ganhador.ganhou -> "PERDEU";
       Partida#ganhador.timeout -> "TIMEOUT";
       true -> "MAIOR" end.

% Função para lidar com o timeout da partida.
timeout(Partida) ->
    receive
        % Aguarda uma mensagem de timeout.
        {timeout, PID} ->
            % Aguarda o tempo definido pelo timeout.
            timer:sleep(?TIMEOUT),
            % Atualiza o estado da partida indicando que houve um timeout.
            Partida#ganhador{timeout = true} = Partida,
            % Envia uma mensagem de timeout para o processo que solicitou o timeout.
            PID ! "TIMEOUT"
    end.


////////////////////////////////////////// solucao stor
-module(jogo).
-export([start/0, participa/1, advinha/2]).

start() -> spawn(fun() -> jogo(...) end).

participa(Jogo) -> 
    Jogo ! {participa, self()},
    receive {Partida, Jogo} -> Partida end.

advinha(Partida, Numero) ->
    Partida ! {advinha, Numero, self()},
    receive {Res, Partida} -> Res end.

jogo(Jogadores) when lengh(Jogadores) < 4 -> 
    receive 
        {participa, From} -> 
            jogo([From | Jogadores])
    end;

jogo(Jogadores) -> 
    Numero = rand:uniform(100),
    Partida = spawn(fun() -> partida(...) end),
    [Jogador ! {partida, self()} || Jogador <- Jogadores],
    spawn(fun()-> receive after 60000 -> Partida ! timeout),
    jogo([]).
    
partida(Numero, Tentativas, Timeout, Ganhou) -> 
    receive 
        {advinha, N, From} ->
            Res = 
            if 
                Ganhou -> "PERDEU";
                Timeout -> "TEMPO";
                Tentativas > 100 -> "TENTATIVAS";
                Numero < N -> "MENOR";
                Numero > N -> "MAIOR";
                true -> "GANHOU"
            end,
            From ! {Res, self()},
            partida(Numero, Tentativas + 1, Timeout, Ganhou orelse Res =:= "GANHOU")