-module(file_manager).
-export([readContent/1, parser/2, stringer/1, account_to_string/1, file_write/2, file_manager/0]).

% Função principal que gerencia a escrita de dados em arquivo
file_manager() ->
    receive
        {estatisticas, From, File} ->
            Stats = get_estatisticas(File),
            From ! Stats,
            file_manager();
        {write_data, Data, File} -> % Aguarda uma mensagem com os dados e o nome do arquivo
            file_write(File, Data), % Chama a função de escrita de dados
            file_manager(); % Continua aguardando por mais mensagens
        {remover_conta, User, Password, File} ->
            remover_conta(File, User, Password),
            file_manager()
    end.

% Função para ler o conteúdo de um arquivo
readContent(File) -> 
    Res = file:read_file(File), % Tenta ler o arquivo especificado
    case Res of
        {ok, Conteudo} -> % Se a leitura for bem-sucedida
            Info = string:split(Conteudo, "\n"), % Divide o conteúdo em linhas
            case Info of
                [<<>>] -> #{};             % string vazia
                _Info -> parser(Info, #{}) % Chama a função de análise dos dados para os restantes casos
            end;
        {error, _} -> % Se houver um erro na leitura do arquivo
            io:fwrite("Erro na abertura do ficheiro\n"), % Exibe uma mensagem de erro
            #{} % Retorna um mapa vazio
    end.


% Função para escrever os dados em arquivo
file_write(File, Data) -> 
    file:write_file(File, stringer(maps:to_list(Data))). % Converte os dados e os escreve no arquivo


parser([], Data) -> Data;
parser([H | T], Data) ->
    if 
        H == [<<>>] -> 
            parser(T,Data);
        true ->
            [U | PW] = string:split(H, ","),
            User = parseSingleField(U),
            %io:fwrite("U ~p~n", [User]),
            [P | W] = string:split(PW, ","),
            PasseWord = parseSingleField(P),
            %io:fwrite("P ~p~n", [PasseWord]),
            [Level | R] = string:split(W, ","),
            Nivel = parseSingleField(Level),
            %io:fwrite("Nivel ~p~n", [Nivel]),
            [Wins | R1] = string:split(R, ","),
            Vitorias = parseSingleField(Wins),
            %io:fwrite("V ~p~n", [Vitorias]),
            [Loses | _] = string:split(R1, ","),
            Derrotas = parseSingleField(Loses),
            %io:fwrite("D ~p~n", [Derrotas]),

            if 
                User == [<<>>] ->
                    Data1 = Data;
                true ->
                    Data1 = maps:put(User, {PasseWord, list_to_integer(Nivel), list_to_integer(Vitorias), list_to_integer(Derrotas), false}, Data)
            end,
            if
                T == [] ->
                    Data1;
                true ->
                    parser(string:split(T, "\n"), Data1)
            end
    end.

parseSingleField(Field) ->
    [_ | Value] = string:split(Field, ":"),
    [UserStr] = Value,
    binary_to_list(UserStr).


% Função para converter uma conta em formato de string
account_to_string({User, {Password, Level, Wins, Loses, _}}) ->
    string:join( [string:join(["USERNAME", User], ":"),
                 string:join(["PASSWORD" , Password],":"),
                 string:join(["LEVEL" , integer_to_list(Level)], ":"),
                 string:join(["WINS" , integer_to_list(Wins)], ":"),
                 string:join(["LOSES" , integer_to_list(Loses)], ":")
                ], ",").

% Função para converter os dados em formato de texto
stringer([]) -> ""; % Caso base: retorna uma string vazia
stringer([H | T]) -> % Caso recursivo: processa cada conta
    string:join([account_to_string(H), stringer(T)], "\n"). % Formata a conta e continua com as próximas


% para pedir estatisticas
get_estatisticas(File) ->
    {ok, Data} = file:read_file(File),
    Lines = string:split(binary_to_list(Data), "\n", all),
    Stats = lists:map(fun process_line/1, Lines),
    FilteredStats = lists:filter(fun(X) -> X =/= undefined end, Stats),
    SortedStats = lists:sort(fun compare_by_level/2, FilteredStats),
    Result = format_stats(SortedStats),
    Ret = lists:concat([Result, "\n"]),
    Ret.

process_line(Line) ->
    % Remove trailing newline characters
    CleanLine = string:strip(Line, right, $\n),
    % Split the line into key-value pairs
    Pairs = string:split(CleanLine, ",", all),
    case Pairs of
        [""] -> undefined;
        _ ->
            % Convert to a map
            Map = lists:foldl(fun(Pair, Acc) ->
                                     [Key, Value] = string:split(Pair, ":", all),
                                     maps:put(Key, Value, Acc)
                             end, #{}, Pairs),
            % Remove the password entry
            maps:remove("PASSWORD", Map)
    end.

compare_by_level(Map1, Map2) ->
    Level1 = list_to_integer(maps:get("LEVEL", Map1)),
    Level2 = list_to_integer(maps:get("LEVEL", Map2)),
    case Level1 =:= Level2 of
        true ->
            Wins1 = list_to_integer(maps:get("WINS", Map1)),
            Wins2 = list_to_integer(maps:get("WINS", Map2)),
            case Wins1 =:= Wins2 of
                true ->
                    Loses1 = list_to_integer(maps:get("LOSES", Map1)),
                    Loses2 = list_to_integer(maps:get("LOSES", Map2)),
                    Loses1 =< Loses2; % Menos derrotas vem primeiro
                false ->
                    Wins1 > Wins2 % Mais vitórias vem primeiro
            end;
        false ->
            Level1 > Level2 % Maior nível vem primeiro
    end.

format_stats(Stats) ->
        % Format the list of maps as a string
    FormattedStats = lists:map(fun(Map) ->
        string:join([
            string:join(["USERNAME", maps:get("USERNAME", Map)], ":"),
            string:join(["LEVEL", maps:get("LEVEL", Map)], ":"),
            string:join(["WINS", maps:get("WINS", Map)], ":"),
            string:join(["LOSES", maps:get("LOSES", Map)], ":")
        ], ",")
    end, Stats),
    % Remove the trailing comma from each line
    FormattedLines = lists:map(fun(Line) ->
                                       string:strip(Line, right, $,)
                               end, FormattedStats),
    % Join the formatted lines with newlines
    lists:foldl(fun(Line, Acc) -> Acc ++ Line end, "", FormattedLines).


% para remover registos
remover_conta(File, User, Password) ->
    % Lê o conteúdo do arquivo
    {ok, Bin} = file:read_file(File),
    % Divide o conteúdo em linhas
    Lines = binary:split(Bin, <<"\n">>, [global]),
    % Filtra as linhas que não correspondem ao usuário e senha fornecidos
    FilteredLines = lists:filter(
        fun(Line) -> 
            not is_matching_user(Line, User, Password)
        end, Lines),
    % Junta as linhas filtradas em um binário
    NewContent = join_binaries(FilteredLines, <<"\n">>),
    % Grava o conteúdo filtrado de volta no arquivo
    file:write_file(File, NewContent).

is_matching_user(Line, User, Password) ->
    Components = binary:split(Line, <<",">>, [global]),
    lists:any(fun(Component) -> 
                  case binary:split(Component, <<":">>, [global]) of
                      [<<"USERNAME">>, BinUser] -> BinUser == list_to_binary(User);
                      [<<"PASSWORD">>, BinPassword] -> BinPassword == list_to_binary(Password);
                      _ -> false
                  end
              end, Components).

join_binaries(Binaries, Separator) ->
    lists:flatten(intersperse(Separator, Binaries)).

intersperse(_, []) -> [];
intersperse(_, [Head]) -> [Head];
intersperse(Separator, [Head | Tail]) -> [Head, Separator | intersperse(Separator, Tail)].
