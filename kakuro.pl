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


