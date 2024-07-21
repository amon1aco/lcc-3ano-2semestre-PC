-module(user_manager).
-export([get_user_level/2, user_manager/1, user_logged_in/3]).

% Função principal para gerenciar contas de usuários
user_manager(Accounts) ->
    receive
        {create_account, From, User, Password} -> % Mensagem para criar uma conta
            NewAccounts = create_account(Accounts, From, User, Password), % Chama a função de criação de conta
            user_manager(NewAccounts); % Chama recursivamente para continuar o gerenciamento de contas
        {login, From, User, Password} -> % Mensagem para fazer login
            NewAccounts = login(Accounts, From, User, Password), % Chama a função de login
            user_manager(NewAccounts); % Chama recursivamente para continuar o gerenciamento de contas
        {logout, From, User, _} -> % Mensagem para fazer logout
            NewAccounts = logout(Accounts, From, User), % Chama a função de logout
            user_manager(NewAccounts); % Chama recursivamente para continuar o gerenciamento de contas
        {user_win, User} ->  % Mensagem para atualizar as vitórias do usuário
            NewAccounts = user_win(Accounts, User), % Chama a função de atualização de pontuação do usuário
            user_manager(NewAccounts); % Chama recursivamente para continuar o gerenciamento de contas
        {user_lose, User} ->  % Mensagem para atualizar as vitórias do usuário
            NewAccounts = user_lose(Accounts, User), % Chama a função de atualização de pontuação do usuário
            user_manager(NewAccounts); % Chama recursivamente para continuar o gerenciamento de contas
        {is_logged_in, From, User, _} -> % Mensagem para verificar se um usuário está logado
            user_logged_in(Accounts, User, From), % Chama a função para verificar se um usuário está logado
            user_manager(Accounts); % Chama recursivamente para continuar o gerenciamento de contas
        {user_level, User, From} -> % Mensagem para obter o nível de um usuário
            From ! get_user_level(Accounts, User), % Envia o nível do usuário de volta ao remetente
            user_manager(Accounts); % Chama recursivamente para continuar o gerenciamento de contas
        {statistics, From} -> % Mensagem para obter estatísticas
            file_manager ! {estatisticas, From, "file.txt"},
            user_manager(Accounts); % Chama recursivamente para continuar o gerenciamento de contas
        {delete_account, From, User, Password} -> % Mensagem para excluir uma conta
            file_manager ! {remover_conta, User, Password, "file.txt"},
            NewAccounts = delete_account(Accounts, From, User, Password), % Chama a função de exclusão de conta
            user_manager(NewAccounts); % Chama recursivamente para continuar o gerenciamento de contas
        {stop, From} -> % Mensagem para parar o gerenciamento de contas
            From ! {stopped, accounts_manager}; % Envia uma mensagem de parada ao remetente
        write_data -> % Mensagem para escrever dados em arquivo
            file_manager ! {write_data, Accounts, "file.txt"}, % Chama o módulo file_manager para escrever dados
            user_manager(Accounts) % Chama recursivamente para continuar o gerenciamento de contas
    end.

% Função para criar uma nova conta de usuário
create_account(Registers, From, User, Password) -> 
    case maps:find(User, Registers)  of
        {ok, _} ->
            From ! {user_exists, accounts_manager}, % Envia uma mensagem informando que o usuário já existe
            Registers_updated = Registers; % Mantém os registros inalterados
        _ ->                                    % passe, nivel, vitorias, derrotas, login ou n
            Registers_updated = maps:put(User, {Password, 1, 0, 0, false}, Registers), % Adiciona uma nova conta aos registros
            From ! {account_created, accounts_manager} % Envia uma mensagem informando que a conta foi criada
    end,
    Registers_updated. % Retorna os registros atualizados

% Função para fazer login de um usuário
login(Registers, From, User, Password) ->
    case maps:find(User, Registers) of
        {ok, {Password, Nivel, Wins, Loses, _}} -> 
            if
                Password == Password ->
                    Registers_updated = maps:update(User, {Password, Nivel, Wins, Loses, true}, Registers), % Atualiza o status de login do usuário
                    From ! {login_sucessfully, accounts_manager}; % Envia uma mensagem informando que o login foi bem-sucedido
                true ->
                    io:fwrite("User ~p: Invalid Password!\n", [User]), % Exibe uma mensagem de erro se a senha for inválida
                    Registers_updated = Registers, % Mantém os registros inalterados
                    From ! {invalid_pwd, accounts_manager} % Envia uma mensagem informando que a senha é inválida
            end;
        _ -> 
            Registers_updated = Registers, % Mantém os registros inalterados
            From ! {invalid_user, accounts_manager}, % Envia uma mensagem informando que o usuário é inválido
            io:fwrite("User ~p does not exist!\n", [User]) % Exibe uma mensagem de erro se o usuário não existir
    end,
    Registers_updated. % Retorna os registros atualizados

% Função para verificar se um usuário está logado
user_logged_in(Accounts, User, From) ->
    Account = maps:get(User, Accounts), % Obtém a conta do usuário
    case Account of
        {_, _, _, _, true}  ->
            From ! {true, accounts_manager}; % Envia uma mensagem informando que o usuário está logado
        _ ->
            From ! {false, accounts_manager} % Envia uma mensagem informando que o usuário não está logado
    end.

% Função para obter o nível de um usuário
get_user_level(Registers, User) ->
    case maps:get(User, Registers) of
        { _, Nivel, _, _, _} -> 
            Nivel;
        _ -> 
            erro
    end.

user_win(Registers, User) ->
    NewRegisters =
        case maps:get(User, Registers) of
            {Password, Nivel, Wins, _, Login} ->
                UpdatedUser =
                    case Wins + 1 == Nivel of
                        true ->
                            {Password, Nivel + 1, 0, 0, Login};
                        false ->
                            {Password, Nivel, Wins + 1, 0, Login}
                    end,
                maps:update(User, UpdatedUser, Registers);
            _ ->
                Registers
        end,
    NewRegisters.

user_lose(Registers, User) ->
    NewRegisters =
        case maps:get(User, Registers) of
            {Password, Nivel, _, Loses, Login} ->
                UpdatedUser =
                    case Loses > (Nivel / 2) of
                        true ->
                            NewNivel = max(1, Nivel - 1),
                            {Password, NewNivel, 0, 0, Login};
                        false ->
                            {Password, Nivel, 0, Loses + 1, Login}
                    end,
                maps:update(User, UpdatedUser, Registers);
            _ ->
                Registers
        end,
    NewRegisters.

% Função para fazer logout de um usuário
logout(Registers, From, User) ->
    case maps:find(User,Registers) of
        {ok, {Password, Nivel, Wins, Loses, _}} ->
            Registers_updated = maps:update(User, {Password, Nivel, Wins, Loses, false}, Registers), 
            From ! {logout_successfully, accounts_manager}; % Envia uma mensagem informando que o logout foi bem-sucedido
        _ ->
            Registers_updated = Registers, % Mantém os registros inalterados
            io:fwrite("User ~p does not exist!\n", [User]), % Exibe uma mensagem de erro se o usuário não existir
            From ! {invalid_user, accounts_manager} % Envia uma mensagem informando que o usuário é inválido
    end,
    Registers_updated. % Retorna os registros atualizados

% Função para excluir uma conta de usuário
delete_account(Registers, From, User, Password) -> 
    case maps:find(User, Registers) of
        {ok, {Password, _, _, _, _}} -> 
            if
                Password == Password ->
                    Registers_updated = maps:remove(User, Registers), % Remove a conta dos registros
                    From ! {account_deleted, accounts_manager}; % Envia uma mensagem informando que a conta foi excluída
                true ->
                    io:fwrite("User ~p: Invalid Password!\n", [User]), % Exibe uma mensagem de erro se a senha for inválida
                    Registers_updated = Registers, % Mantém os registros inalterados
                    From ! {invalid_pwd, accounts_manager} % Envia uma mensagem informando que a senha é inválida
            end;
        _ ->
            io:fwrite("User ~p does not exist!\n", [User]), % Exibe uma mensagem de erro se o usuário não existir
            Registers_updated = Registers, % Mantém os registros inalterados
            From ! {invalid_user, accounts_manager} % Envia uma mensagem informando que o usuário é inválido
    end,
    Registers_updated. % Retorna os registros atualizados