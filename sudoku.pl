% https://www.janko.at/Raetsel/Sudoku/Vergleich/001.a.htm
% --- Versão Modificada com Nomes e Estrutura Alterados ---

:- use_module(library(clpfd)).

% Predicado principal para iniciar a resolução e mostrar o resultado.
% Basta executar a consulta: ?- iniciar.
iniciar :-
    resolver_puzzle(Grade),
    write('Solucao encontrada:'), nl,
    imprimir_solucao(Grade).

% Encontra a solução para a grade do puzzle.
% A lógica de restrições está toda aqui.
resolver_puzzle(Grade) :-
    % 1. Definir a estrutura da grade com novos nomes de variáveis (C = Célula)
    Grade = [[C11, C12, C13, C14],
             [C21, C22, C23, C24],
             [C31, C32, C33, C34],
             [C41, C42, C43, C44]],

    % 2. Agrupar todas as variáveis em uma única lista
    flatten(Grade, TodasAsCelulas),

    % 3. Restrição de Domínio: todas as células devem ter valores de 1 a 4
    TodasAsCelulas ins 1..4,

    % 4. Restrições Específicas do Puzzle (relações de maior/menor)
    C11 #< C12,                 C13 #< C14,
    C11 #> C21, C12 #< C22,     C13 #< C23, C14 #> C24,
    C21 #< C22,                 C23 #> C24,

    C31 #> C32,                 C33 #< C34,
    C31 #> C41, C32 #< C42,     C33 #< C43, C34 #> C44,
    C41 #> C42,                 C43 #> C44,

    % 5. Restrição de Regiões: sem repetição nos blocos 2x2
    all_distinct([C11, C12, C21, C22]),
    all_distinct([C13, C14, C23, C24]),
    all_distinct([C31, C32, C41, C42]),
    all_distinct([C33, C34, C43, C44]),
    
    % 6. Restrição de Linhas: sem repetição em cada linha
    maplist(all_distinct, Grade),

    % 7. Restrição de Colunas: sem repetição em cada coluna
    transpose(Grade, GradeTransposta),
    maplist(all_distinct, GradeTransposta),

    % 8. Iniciar a busca por uma solução que satisfaça tudo
    label(TodasAsCelulas).

% Predicado para imprimir a grade (matriz) de forma legível.
imprimir_solucao([]). % Caso base: se a lista estiver vazia, não faz nada.
imprimir_solucao([Linha | OutrasLinhas]) :-
    write(Linha), nl, % Imprime a linha atual e pula para a próxima
    imprimir_solucao(OutrasLinhas). % Chama a si mesmo para o resto da matriz.