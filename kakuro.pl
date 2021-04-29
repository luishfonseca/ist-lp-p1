:- [codigo_comum].

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

espaco_fila(Fila, Pos, Esp) :-
    append([_, [Pos | Esp], Fim], Fila),
    posicao(Pos),
    \+length(Esp, 0),
    include(posicao, Esp, Separadores),
    length(Separadores, 0),
    termina(Fim).

posicao([V, H]) :- number(V), number(H).
termina([Pos | _]) :- posicao(Pos).
termina([]).

%-------------------------------------------------------------------------------
%               espacos_fila(H_V, Fila, Espacos)
% espacos_fila(H_V, Fila, Espacos), em que Fila eh uma fila (linha ou coluna) de
% uma grelha e H_V eh um dos atomos h ou v, significa que Espacos eh a lista de
% todos os espacos de Fila, da esquerda para a direita.
%-------------------------------------------------------------------------------
espacos_fila(H_V, Fila, Espacos) :-
    findall(Esp, espaco_fila(Fila, Esp, H_V), Espacos).
