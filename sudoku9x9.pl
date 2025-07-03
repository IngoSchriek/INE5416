% https://www.janko.at/Raetsel/Sudoku/Vergleich/190.a.htm

:- use_module(library(clpfd)).

% predicado para exibir a matriz de solução linha por linha
exibirMatriz([]).
exibirMatriz([L|Ls]) :-
    write(L), nl,
    exibirMatriz(Ls).

solucao(MatrizSolucao) :-
    MatrizSolucao = [[X11, X12, X13,      X14, X15, X16,      X17, X18, X19],
                     [X21, X22, X23,      X24, X25, X26,      X27, X28, X29],
                     [X31, X32, X33,      X34, X35, X36,      X37, X38, X39],

                     [X41, X42, X43,      X44, X45, X46,      X47, X48, X49],
                     [X51, X52, X53,      X54, X55, X56,      X57, X58, X59],
                     [X61, X62, X63,      X64, X65, X66,      X67, X68, X69],
                     
                     [X71, X72, X73,      X74, X75, X76,      X77, X78, X79],
                     [X81, X82, X83,      X84, X85, X86,      X87, X88, X89],
                     [X91, X92, X93,      X94, X95, X96,      X97, X98, X99]],
    
    % transforma a matriz em uma lista contendo todos os elementos da matriz
    flatten(MatrizSolucao, Elementos),
    % elementos devem assumir valores de 1 a 9
    Elementos ins 1..9,

    % tabuleiro do jogo (para resolver outro tabuleiro, alterar somente os operadores)
    X11 #< X12, X12 #> X13,                     X14 #< X15, X15 #< X16,                     X17 #> X18, X18 #> X19,
    X11 #< X21, X12 #> X22, X13 #> X23,         X14 #> X24, X15 #< X25, X16 #< X26,         X17 #> X27, X18 #< X28, X19 #< X29,
    X21 #> X22, X22 #< X23,                     X24 #< X25, X25 #< X26,                     X27 #> X28, X28 #< X29,
    X21 #> X31, X22 #< X32, X23 #< X33,         X24 #< X34, X25 #> X35, X26 #> X36,         X27 #< X37, X28 #< X38, X29 #< X39,
    X31 #> X32, X32 #< X33,                     X34 #> X35, X35 #< X36,                     X37 #> X38, X38 #< X39,

    X41 #< X42, X42 #> X43,                     X44 #> X45, X45 #< X46,                     X47 #< X48, X48 #> X49,
    X41 #> X51, X42 #< X52, X43 #< X53,         X44 #> X54, X45 #> X55, X46 #> X56,         X47 #< X57, X48 #> X58, X49 #< X59,
    X51 #< X52, X52 #> X53,                     X54 #> X55, X55 #< X56,                     X57 #< X58, X58 #> X59,
    X51 #< X61, X52 #> X62, X53 #> X63,         X54 #< X64, X55 #< X65, X56 #> X66,         X57 #> X67, X58 #> X68, X59 #< X69,
    X61 #< X62, X62 #> X63,                     X64 #> X65, X65 #> X66,                     X67 #> X68, X68 #< X69,

    X71 #> X72, X72 #< X73,                     X74 #< X75, X75 #< X76,                     X77 #> X78, X78 #< X79,
    X71 #< X81, X72 #> X82, X73 #> X83,         X74 #< X84, X75 #< X85, X76 #> X86,         X77 #< X87, X78 #< X88, X79 #> X89,
    X81 #> X82, X82 #> X83,                     X84 #> X85, X85 #> X86,                     X87 #< X88, X88 #> X89,
    X81 #> X91, X82 #> X92, X83 #< X93,         X84 #> X94, X85 #< X95, X86 #< X96,         X87 #< X97, X88 #< X98, X89 #< X99,
    X91 #> X92, X92 #> X93,                     X94 #< X95, X95 #> X96,                     X97 #< X98, X98 #< X99,
    
    % verifica se há algum elemento repetido nos blocos 
    all_different([X11, X12, X13, X21, X22, X23, X31, X32, X33]),
    all_different([X14, X15, X16, X24, X25, X26, X34, X35, X36]),
    all_different([X17, X18, X19, X27, X28, X29, X37, X38, X39]),
    all_different([X41, X42, X43, X51, X52, X53, X61, X62, X63]),
    all_different([X44, X45, X46, X54, X55, X56, X64, X65, X66]),
    all_different([X47, X48, X49, X57, X58, X59, X67, X68, X69]),
    all_different([X71, X72, X73, X81, X82, X83, X91, X92, X93]),
    all_different([X74, X75, X76, X84, X85, X86, X94, X95, X96]),
    all_different([X77, X78, X79, X87, X88, X89, X97, X98, X99]),
    
    % verifica se há algum elemento repetido nas linhas
    maplist(all_different, MatrizSolucao),

    % verifica se há algum elemento repetido nas colunas
    transpose(MatrizSolucao, Transposta),
    maplist(all_different, Transposta),

    % busca de soluções
    label(Elementos).