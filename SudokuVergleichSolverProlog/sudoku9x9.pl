% https://www.janko.at/Raetsel/Sudoku/Vergleich/190.a.htm
% --- Versão Modificada e Comentada ---

% Constraint Logic Programming over Finite Domains. Delimita dominio das variaveis lógicas Cij
:- use_module(library(clpfd)).

% Predicado principal para executar.
% Consulta no SWI-Prolog: ?- iniciar.
iniciar :-
    resolver_sudoku_9x9(Grade),
    write('--- Solucao do Sudoku Comparativo 9x9 ---'), nl,
    imprimir_grade(Grade).

% Imprime a grade de forma legível, linha por linha.
imprimir_grade([]).
imprimir_grade([Linha | Resto]) :-
    write(Linha), nl,
    imprimir_grade(Resto).

% Define todas as restrições e resolve o puzzle. Faz ordenamento via definição da matriz, regras de dominio e execução de fato
resolver_sudoku_9x9(Grade) :-
    % 1. Definir a estrutura da grade 9x9
    Grade = [[C11, C12, C13, C14, C15, C16, C17, C18, C19],
             [C21, C22, C23, C24, C25, C26, C27, C28, C29],
             [C31, C32, C33, C34, C35, C36, C37, C38, C39],
             [C41, C42, C43, C44, C45, C46, C47, C48, C49],
             [C51, C52, C53, C54, C55, C56, C57, C58, C59],
             [C61, C62, C63, C64, C65, C66, C67, C68, C69],
             [C71, C72, C73, C74, C75, C76, C77, C78, C79],
             [C81, C82, C83, C84, C85, C86, C87, C88, C89],
             [C91, C92, C93, C94, C95, C96, C97, C98, C99]],

    % 2. Agrupar todas as 81 células em uma lista simples
    flatten(Grade, TodasAsCelulas),

    % 3. Restrição de Domínio: cada célula deve ser um valor de 1 a 9 (só vale pro cenário atual, a ideia geral é 1 ... n)
    TodasAsCelulas ins 1..9,

    % 4. Restrições de Comparação (Específicas do Puzzle #190)
    C11 #< C12, C12 #> C13,                     C14 #< C15, C15 #< C16,                     C17 #> C18, C18 #> C19,
    C11 #< C21, C12 #> C22, C13 #> C23,         C14 #> C24, C15 #< C25, C16 #< C26,         C17 #> C27, C18 #< C28, C19 #< C29,
    C21 #> C22, C22 #< C23,                     C24 #< C25, C25 #< C26,                     C27 #> C28, C28 #< C29,
    C21 #> C31, C22 #< C32, C23 #< C33,         C24 #< C34, C25 #> C35, C26 #> C36,         C27 #< C37, C28 #< C38, C29 #< C39,
    C31 #> C32, C32 #< C33,                     C34 #> C35, C35 #< C36,                     C37 #> C38, C38 #< C39,

    C41 #< C42, C42 #> C43,                     C44 #> C45, C45 #< C46,                     C47 #< C48, C48 #> C49,
    C41 #> C51, C42 #< C52, C43 #< C53,         C44 #> C54, C45 #> C55, C46 #> C56,         C47 #< C57, C48 #> C58, C49 #< C59,
    C51 #< C52, C52 #> C53,                     C54 #> C55, C55 #< C56,                     C57 #< C58, C58 #> C59,
    C51 #< C61, C52 #> C62, C53 #> C63,         C54 #< C64, C55 #< C65, C56 #> C66,         C57 #> C67, C58 #> C68, C59 #< C69,
    C61 #< C62, C62 #> C63,                     C64 #> C65, C65 #> C66,                     C67 #> C68, C68 #< C69,

    C71 #> C72, C72 #< C73,                     C74 #< C75, C75 #< C76,                     C77 #> C78, C78 #< C79,
    C71 #< C81, C72 #> C82, C73 #> C83,         C74 #< C84, C75 #< C85, C76 #> C86,         C77 #< C87, C78 #< C88, C79 #> C89,
    C81 #> C82, C82 #> C83,                     C84 #> C85, C85 #> C86,                     C87 #< C88, C88 #> C89,
    C81 #> C91, C82 #> C92, C83 #< C93,         C84 #> C94, C85 #< C95, C86 #< C96,         C87 #< C97, C88 #< C98, C89 #< C99,
    C91 #> C92, C92 #> C93,                     C94 #< C95, C95 #> C96,                     C97 #< C98, C98 #< C99,

    % 5. Restrições do Sudoku Padrão
    % 5a. Todas as células em cada linha devem ser diferentes
    maplist(all_distinct, Grade),

    % 5b. Todas as células em cada coluna devem ser diferentes
    transpose(Grade, GradeTransposta),
    maplist(all_distinct, GradeTransposta),

    % 5c. Todas as células em cada bloco 3x3 devem ser diferentes
    all_distinct([C11, C12, C13, C21, C22, C23, C31, C32, C33]),
    all_distinct([C14, C15, C16, C24, C25, C26, C34, C35, C36]),
    all_distinct([C17, C18, C19, C27, C28, C29, C37, C38, C39]),
    all_distinct([C41, C42, C43, C51, C52, C53, C61, C62, C63]),
    all_distinct([C44, C45, C46, C54, C55, C56, C64, C65, C66]),
    all_distinct([C47, C48, C49, C57, C58, C59, C67, C68, C69]),
    all_distinct([C71, C72, C73, C81, C82, C83, C91, C92, C93]),
    all_distinct([C74, C75, C76, C84, C85, C86, C94, C95, C96]),
    all_distinct([C77, C78, C79, C87, C88, C89, C97, C98, C99]),

    % 6. Iniciar a busca pela solução
    label(TodasAsCelulas).