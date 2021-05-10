%-------------------------------------------------------------------------------
%               combinacoes_soma(N, Els, Soma, Combs)
% combinacoes_soma(N, Els, Soma, Combs), em que N eh um inteiro, Els eh uma
% lista de inteiros, e Soma eh um inteiro, significa que Combs eh a lista
% ordenada cujos elementos sao as combinacoes N a N, dos elementos de Els cuja
% soma eh Soma.
%-------------------------------------------------------------------------------
combinacoes_soma(N, Els, Soma, Combs) :-
    setof(Comb, (combinacao(N, Els, Comb), sum_list(Comb, Soma)), Combs).

%-------------------------------------------------------------------------------
%               permutacoes_soma(N, Els, Soma, Perms)
% permutacoes_soma(N, Els, Soma, Perms), em que N eh um inteiro, Els eh uma
% lista de inteiros, e Soma eh um inteiro, significa que Perms eh a lista
% ordenada cujos elementos sao as permutacoes das combinacoes N a N, dos
% elementos de Els cuja soma eh Soma.
%-------------------------------------------------------------------------------
permutacoes_soma(N, Els, Soma, Perms) :-
    combinacoes_soma(N, Els, Soma, Combs),
    findall(Perm, (member(Comb, Combs), permutation(Comb, Perm)), Unsorted),
    sort(Unsorted, Perms).

%-------------------------------------------------------------------------------
%               espaco_fila(Fila, Esp, H_V)
% espaco_fila(Fila, Esp, H_V), em que Fila eh uma fila (linha ou coluna) de um
% puzzle e H_V eh um dos atomos h ou v, conforme se trate de uma fila horizontal
% ou vertical, respectivamente, significa que Esp eh um espaco de Fila.
%-------------------------------------------------------------------------------
espaco_fila(Fila, espaco(H, Esp), h) :- espaco_fila(Fila, [_, H], Esp).
espaco_fila(Fila, espaco(V, Esp), v) :- espaco_fila(Fila, [V, _], Esp).

espaco_fila(Fila, Soma, Posicoes) :-
    append([_, [Soma], Posicoes, [Proxima_soma], _], Fila),
    all(is_list, [Soma, Proxima_soma]),
    Posicoes \= [],
    \+ any(is_list, Posicoes).

espaco_fila(Fila, Soma, Posicoes) :-
    append([_, [Soma], Posicoes], Fila),
    is_list(Soma),
    Posicoes \= [],
    \+ any(is_list, Posicoes).

%-------------------------------------------------------------------------------
%               espacos_fila(H_V, Fila, Espacos)
% espacos_fila(H_V, Fila, Espacos), em que Fila eh uma fila (linha ou coluna) de
% uma grelha e H_V eh um dos atomos h ou v, significa que Espacos eh a lista de
% todos os espacos de Fila, da esquerda para a direita.
%-------------------------------------------------------------------------------
espacos_fila(H_V, Fila, Espacos) :-
    bagof(Esp, Esp^espaco_fila(Fila, Esp, H_V), Espacos), !.

espacos_fila(_, _, []).

%-------------------------------------------------------------------------------
%               espacos_puzzle(Puzzle, Espacos)
% espacos_puzzle(Puzzle, Espacos), em que Puzzle eh um puzzle, significa que
% Espacos eh a lista de espacos de Puzzle.
%-------------------------------------------------------------------------------
espacos_puzzle(Puzzle, Espacos) :-
    maplist(espacos_fila(h), Puzzle, Espacos_h),
    mat_transposta(Puzzle, Transp),
    maplist(espacos_fila(v), Transp, Espacos_v),
    append(Espacos_h, Espacos_v, Esps),
    append(Esps, Espacos).

%-------------------------------------------------------------------------------
%               espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)
% espacos_com_posicoes_comuns(Espacos, Esp, Esps_com), em que Espacos eh uma
% lista de espacos e Esp eh um espaco, significa que Esps_com eh a lista de
% espacos com variaveis em comum com Esp, exceptuando Esp.
%-------------------------------------------------------------------------------
espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
    include(espaco_com_posicoes_comuns(Esp), Espacos, Esps_com).

espaco_com_posicoes_comuns(espaco(_, Posicoes1), espaco(_, Posicoes2)) :-
    Posicoes1 \== Posicoes2,
    \+ findall(Com, (member(Com, Posicoes1), any(==(Com), Posicoes2)), []).

%-------------------------------------------------------------------------------
%               permutacoes_soma_espacos(Espacos, Perms_soma)
% permutacoes_soma_espacos(Espacos, Perms_soma), em que Espacos eh uma lista de
% espacos, significa que Perms_soma eh a lista de listas de 2 elementos, em que
% o primeiro elemento eh um espaco de Espacos e o segundo eh a lista ordenada de
% permutacoes cuja soma eh igual a soma do espaco.
%-------------------------------------------------------------------------------
permutacoes_soma_espacos(Espacos, Perms_soma) :-
    maplist(permutacoes_soma_espaco, Espacos, Perms_soma).

permutacoes_soma_espaco(espaco(Soma, Esp), [espaco(Soma, Esp), Perms]) :-
    numlist(1, 9, Ns),
    length(Esp, Len),
    permutacoes_soma(Len, Ns, Soma, Perms).

%-------------------------------------------------------------------------------
%               permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma)
% permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma), em que Perm eh uma
% permutacao, Esp eh um espaco, Espacos eh uma lista de espacos, e Perms_soma
% eh uma lista de listas tal como obtida pelo predicado permutacoes_soma_espaco,
% significa que Perm eh uma permutacao possivel para o espaco Esp.
%-------------------------------------------------------------------------------
permutacao_possivel_espaco(Perm, espaco(Soma, Esp), Espacos, Perms_soma) :-
    espacos_com_posicoes_comuns(Espacos, espaco(Soma, Esp), Esps_com),
    member([espaco(Soma, Esp), Perms], Perms_soma),
    member(Perm, Perms),
    maplist(permutacao_possivel(Perms_soma), Esp, Perm, Esps_com).

permutacao_possivel(Perms_soma, Pos, Pos, espaco(Soma, Esp)) :-
    member([espaco(Soma, Esp), Perms], Perms_soma),
    % Não interessa com o que unifica apenas se unifica.
    \+ \+ member(Esp, Perms).

%-------------------------------------------------------------------------------
%               numeros_comuns(Lst_Perms, Numeros_comuns)
% numeros_comuns(Lst_Perms, Numeros_comuns), em que Lst_Perms eh uma lista de
% permutacoes, significa que Numeros_comuns eh uma lista de pares (pos, numero),
% significando que todas as listas de Lst_Perms contem o numero numero na
% posicao pos.
%-------------------------------------------------------------------------------
numeros_comuns(Lst_Perms, Numeros_comuns) :-
    findall(Numero, numero_comum(Lst_Perms, Numero), Numeros_comuns).

numero_comum(Lst_Perms, (Pos, Numero)) :-
    all(aux_nth1(Pos, Numero), Lst_Perms).

aux_nth1(Pos, Elem, Lst) :- nth1(Pos, Lst, Elem).

%-------------------------------------------------------------------------------
%               any(Goal, Lista)
%-------------------------------------------------------------------------------
any(Goal, [El | _]) :-
    call(Goal, El), !.

any(Goal, [_ | Resto]) :- any(Goal, Resto).

%-------------------------------------------------------------------------------
%               all(Goal, Lista)
%-------------------------------------------------------------------------------
all(_, []) :- !.

all(Goal, [El | Resto]) :-
    call(Goal, El),
    all(Goal, Resto).
