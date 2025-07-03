:- use_module(library(clpfd)).  % Carrega a biblioteca de programação com restrições

% --- PREDICADO PRINCIPAL ---
% makaro(Solucao)
% Encontra e exibe a solução para o quebra-cabeça Makaro pré-definido.
makaro(Solucao) :-
    % Definição do nosso quebra-cabeça 4x4 de exemplo
    definir_puzzle(Dim, Regioes, Setas),

    % Cria a estrutura do tabuleiro com variáveis
    cria_tabuleiro(Dim, Solucao),

    % Pega todas as variáveis do tabuleiro em uma lista única
    flatten(Solucao, Vars),

    % Aplica a restrição de domínio principal.
    % O maior número possível é o tamanho da maior região.
    maplist(length, Regioes, TamanhosRegioes),
    max_list(TamanhosRegioes, MaxN),
    Vars ins 1..MaxN,

    % Aplica as 3 regras do Makaro como restrições
    aplica_restricoes_regioes(Regioes, Solucao),
    aplica_restricoes_adjacencia(Solucao),
    aplica_restricoes_setas(Setas, Solucao),

    % Inicia o processo de busca para encontrar uma solução concreta
    labeling([], Vars),

    % Imprime a solução de forma legível
    imprime_solucao(Solucao).

% --- DEFINIÇÃO DO PUZZLE ---
% Neste exemplo, definimos o puzzle diretamente no código.
definir_puzzle(4, Regioes, Setas) :-
    Regioes = [
        % Região 1 (3 células, usa números 1-3)
        [(1,1), (1,2), (2,1)],
        % Região 2 (3 células, usa números 1-3)
        [(1,3), (1,4), (2,4)],
        % Região 3 (3 células, usa números 1-3)
        [(3,2), (4,1), (4,2)],
        % Região 4 (3 células, usa números 1-3)
        [(3,4), (4,3), (4,4)]
    ],
    % Seta em (2,3) aponta para CIMA, para a célula (1,3)
    % Seta em (3,3) aponta para a ESQUERDA, para a célula (3,2)
    Setas = [
        seta(2,3, 1,3),
        seta(3,3, 3,2)
    ].

% --- LÓGICA DE RESTRIÇÕES ---

% Regra 1: Cada região de N células contém os números de 1 a N.
aplica_restricoes_regioes([], _Tabuleiro).
aplica_restricoes_regioes([Regiao | OutrasRegioes], Tabuleiro) :-
    length(Regiao, N),
    obter_vars_regiao(Regiao, Tabuleiro, VarsRegiao),
    % Os valores devem ser de 1 a N
    VarsRegiao ins 1..N,
    % Todos os valores devem ser diferentes
    all_distinct(VarsRegiao),
    aplica_restricoes_regioes(OutrasRegioes, Tabuleiro).

% Regra 2: Números iguais não podem ser ortogonalmente adjacentes.
aplica_restricoes_adjacencia(Tabuleiro) :-
    length(Tabuleiro, Dim),
    % Itera sobre cada célula do tabuleiro
    forall(between(1, Dim, L),
           forall(between(1, Dim, C),
                  aplica_adjacencia_celula(L, C, Tabuleiro))).

aplica_adjacencia_celula(L, C, Tabuleiro) :-
    obter_var(L, C, Tabuleiro, Var),
    % Vizinho de baixo
    L_baixo is L + 1,
    (obter_var(L_baixo, C, Tabuleiro, Vizinho) -> Var #\= Vizinho ; true),
    % Vizinho da direita
    C_direita is C + 1,
    (obter_var(C_direita, L, Tabuleiro, Vizinho) -> Var #\= Vizinho ; true).
    % Não precisamos checar para cima e para a esquerda, pois a iteração cuidará disso.

% Regra 3: A seta aponta para o maior vizinho ortogonal.
aplica_restricoes_setas([], _Tabuleiro).
aplica_restricoes_setas([Seta | OutrasSetas], Tabuleiro) :-
    % Desestrutura a seta: Seta em (SL,SC) aponta para (AL,AC)
    Seta = seta(SL, SC, AL, AC),
    obter_var(AL, AC, Tabuleiro, VarAlvo),

    % Obtém todos os vizinhos ortogonais da célula da seta
    vizinhos_ortogonais(SL, SC, Tabuleiro, VizinhosCoords),
    
    % Para cada outro vizinho, o alvo deve ser maior
    forall(
        (member((VL, VC), VizinhosCoords), (VL, VC) \= (AL, AC)), % Para cada vizinho QUE NÃO SEJA O ALVO
        (obter_var(VL, VC, Tabuleiro, VarOutroVizinho),
         VarAlvo #> VarOutroVizinho)
    ),
    aplica_restricoes_setas(OutrasSetas, Tabuleiro).


% --- PREDICADOS AUXILIARES ---

% cria_tabuleiro(Dim, Tabuleiro)
cria_tabuleiro(Dim, Tabuleiro) :-
    length(Tabuleiro, Dim),
    maplist(cria_linha(Dim), Tabuleiro).
cria_linha(Dim, Linha) :- length(Linha, Dim).

% obter_var(Linha, Coluna, Tabuleiro, Var)
obter_var(L, C, Tabuleiro, Var) :-
    nth1(L, Tabuleiro, Linha),
    nth1(C, Linha, Var).

% obter_vars_regiao(Coordenadas, Tabuleiro, ListaDeVars)
obter_vars_regiao([], _, []).
obter_vars_regiao([(L,C) | Coords], Tabuleiro, [Var | Vars]) :-
    obter_var(L, C, Tabuleiro, Var),
    obter_vars_regiao(Coords, Tabuleiro, Vars).
    
% vizinhos_ortogonais(L, C, Tabuleiro, ListaDeCoords)
vizinhos_ortogonais(L, C, Tabuleiro, Vizinhos) :-
    length(Tabuleiro, Dim),
    findall((NL, NC), (
        (NL is L, NC is C - 1); % Esquerda
        (NL is L, NC is C + 1); % Direita
        (NL is L - 1, NC is C); % Cima
        (NL is L + 1, NC is C)  % Baixo
    ), Candidatos),
    include(coord_valida(Dim), Candidatos, Vizinhos).

coord_valida(Dim, (L,C)) :- L > 0, C > 0, L =< Dim, C =< Dim.

% --- IMPRESSÃO DA SOLUÇÃO ---
imprime_solucao(Solucao) :-
    nl,
    maplist(imprime_linha, Solucao),
    nl.

imprime_linha(Linha) :-
    write('  '),
    maplist(imprime_celula, Linha),
    nl.

imprime_celula(Celula) :-
    format('~w ', [Celula]).