%% see: http://www.sics.se/~joe/ubf/site/home.html
%% Written by: Joe Armstrong
-module(ubf).

-compile(export_all).

-export([decode_init/0, decode/1, decode/2, encode/1, encode/2]).
-export([encode_print/1, ubf2term/1, deabstract/1]).

-import(lists, [foldl/3, reverse/1, map/2, seq/2, sort/1]).

bug() ->
    C = decode("{'abc"),
    decode("d'}$", C).

%% Decoding rules
%% {'#S', String} -> String
%% Int            -> Int
%% [ ]            -> List
%% {...}          -> Tuple

%% decode_init() -> Cont
%% decode(Str, Cont) -> {more, Cont'} | {done, Term, Str}
%% encode(Str) -> Bytes

%% intro() -> single line terminated with \n


ubf2term(Str) ->
    {done, Term, _} = decode(Str),
    Term.

decode_init() -> {more, fun(I) -> decode(I, [[]], dict:new()) end}.

decode(Str) -> 
    %% io:format("decode:~s~n",[Str]),
    decode(Str, decode_init()).

decode(S, {more, Fun}) -> 
    %% io:format("decode:~s~n",[S]),
    Fun(S).

decode1(S, Stack, D) ->
    %% io:format("AAAHere:|~s| Stack=~p~n", [S,Stack]),
    decode(S, Stack, D).

decode([$'|T], Stack, Dict) ->
    get_stuff(T, $', [], Stack, Dict);
decode([$~|T], [[Int|Stack]|S1], Dict) when integer(Int), Int >= 0 ->
    collect_binary(Int, T, [], [Stack|S1], Dict);
decode([$~|T], Stack, Dict) ->
    exit(tilde);
decode([$%|T], Stack, Dict) ->
    get_stuff(T, $%, [], Stack, Dict);
decode([$"|T], Stack, Dict) ->
    get_stuff(T, $", [], Stack, Dict);
decode([$`|T], Stack, Dict) ->
    get_stuff(T, $`, [], Stack, Dict);
decode([$-|T], Stack, Dict) ->
    collect_int(T, 0, '-', Stack, Dict);
decode([H|T], Stack, Dict) when $0 =< H, H =< $9 ->
    collect_int(T, H-$0, '+', Stack, Dict);
decode([${|T], Stack, Dict) ->
    decode1(T, [[]|Stack], Dict);
decode([$}|T], [H|Stack], Dict) ->
    decode1(T, push(list_to_tuple(reverse(H)),Stack), Dict);
decode([$&|T], [ [H1,H2|T1] | Stack], Dict) ->
    decode1(T, [[[H1|H2]|T1]|Stack], Dict);
decode([$#|T], Stack, Dict) ->
    decode1(T, push([], Stack), Dict);
decode([$$|T], [[X]], Dict) ->
    {done, X, T};
decode([$>,Key|T], [[Val|R]|Stack], Dict) ->
    decode1(T, [R|Stack], dict:store(Key,Val,Dict));
decode([H|T], Stack, Dict) ->
    case special(H) of
	true ->
	    decode1(T, Stack, Dict);
	false ->
	    decode1(T, push(dict:fetch(H, Dict), Stack), Dict)
    end;
decode([], Stack, Dict) ->
    {more, fun(I) -> decode1(I, Stack, Dict) end};
decode(X, Stack, Dict) ->
    io:format("OOOOO:~p ~p ~p~n",[X, Stack, Dict]),
    exit(aaaaaa).
		   
get_stuff([$\\,H|T], Stop, L, Stack, Dict) -> 
    get_stuff(T, Stop, [H|L], Stack, Dict);
get_stuff([$'|T], $', L, Stack, Dict)  -> 
    decode1(T, push(list_to_atom(reverse(L)),Stack), Dict);
get_stuff([$"|T], $", L, Stack, Dict)  -> 
    decode1(T, push({'#S',reverse(L)},Stack), Dict);
get_stuff([$`|T], $`, L, [Top|Stack], Dict)  -> 
    decode1(T, push({'$TYPE', reverse(L), Top},Stack), Dict);
get_stuff([$%|T], $%, L, Stack, Dict)  -> 
    decode1(T, Stack, Dict);
get_stuff([H|T], Stop, L, Stack, Dict) -> 
    get_stuff(T, Stop, [H|L], Stack, Dict);
get_stuff([], Stop, L, Stack, Dict) ->
    {more, fun(I) ->		   
		   get_stuff(I, Stop, L, Stack, Dict) end}.

collect_binary(0, T, L, Stack, Dict) ->
    expect_tilde(T, push(list_to_binary(L),Stack), Dict);
collect_binary(N, [H|T], L, Stack, Dict) ->
    collect_binary(N-1, T, [H|L], Stack, Dict);
collect_binary(N, [], L, Stack, Dict) ->
    {more, fun(I) -> collect_binary(N, I, L, Stack, Dict) end}.

expect_tilde([$~|T], Stack, Dict) ->
    decode(T, Stack, Dict);
expect_tilde([], Stack, Dict) ->
    {more, fun(I) -> expect_tilde(I, Stack, Dict) end};
expect_tilde([H|_], _, _) ->
    exit({expect_tilde, H}).

push(X, [Top|Rest]) -> 
    [[X|Top]|Rest];
push(X, Y) ->
    io:format("** bad push:~p ~p~n",[X,Y]).

special($ )  -> true;
special(${)  -> true;
special($})  -> true;
special($,)  -> true;
special($#)  -> true;
special($&)  -> true;
special($%)  -> true;
special($>)  -> true;
special($\n) -> true;
special($\r) -> true;
special($\t) -> true;
special($$)  -> true;
special($")  -> true;
special($')  -> true;
special($~)  -> true;
special(_)   -> false.

special_chars() ->    
    " 0123456789{},~%#>\n\r\s\t\"'-&$".

collect_int([H|T], N, Sign, Stack, Dict) when  $0 =< H, H =< $9 ->
    collect_int(T, N*10 + H - $0, Sign, Stack, Dict);
collect_int([], N, Sign, Stack, Dict) ->
    {more, fun(I) -> collect_int(I, N, Sign, Stack, Dict) end};
collect_int(T, N, '+', Stack, Dict) ->
    decode1(T, push(N, Stack), Dict);
collect_int(T, N, '-', Stack, Dict) ->
    decode1(T, push(-N, Stack), Dict).

%%---------------------------------------------------------------------

 
encode_print(X) ->
    io:format("~s~n",[encode(X)]).


encode(X) ->
    element(1, encode(X, dict:new())).


encode(X, Dict0) ->
    {Dict1, L1} = initial_dict(X, Dict0),
    case (catch do_encode(X, Dict1)) of
	{'EXIT', What} ->
	    io:format("What=~p~n",[What]),
	    exit(encode);
	L ->
	    {flatten([L1, L,$$]), Dict1}
    end.

initial_dict(X, Dict0) ->
    Free = seq(32,255) -- special_chars(),
    Most = analyse(X),
    %% io:format("Analysis:~p~n",[Most]),
    load_dict(Most, Free, Dict0, []).

load_dict([{N,X}|T], [Key|T1], Dict0, L) when N > 0->
    load_dict(T, T1, dict:store(X, Key, Dict0), 
	      [encode_obj(X),">",Key|L]);
load_dict(_, _, Dict, L) ->
    {Dict, L}.

analyse(T) ->
    KV = dict:to_list(analyse(T, dict:new())),
    %% The Range is the Number of things times its size
    %% If the size is greater than 0
    KV1 = map(fun rank/1, KV),
    reverse(sort(KV1)).
		     
rank({X, K}) when atom(X) ->
    case length(atom_to_list(X)) of
	N when N > 1, K > 1 ->
	    {(N-1) * K, X};
	_ ->
	    {0, X}
    end;
rank({X, K}) when integer(X) ->
    case length(integer_to_list(X)) of
	N when N > 1, K > 1 ->
	    {(N-1) * K, X};
	_ ->
	    {0, X}
    end;
rank({X, _}) ->
    {0, X}.

analyse({'#S', Str}, Dict) ->
    analyse(Str, Dict);
analyse(T, Dict) when tuple(T) ->
    foldl(fun analyse/2, Dict, tuple_to_list(T)); 
analyse(X, Dict) ->
    case dict:find(X, Dict) of
	{ok, Val} ->
	    dict:store(X, Val+1, Dict);
	error ->
	    dict:store(X, 1, Dict)
    end.

flatten(L) ->
    binary_to_list(list_to_binary(L)).

encode_obj(X) when atom(X) -> encode_atom(X);
encode_obj(X) when integer(X) -> integer_to_list(X);
encode_obj(X) when binary(X) -> encode_binary(X).

encode_string(S) -> [$",add_string(S, $"), $"].
encode_atom(X)   -> [$',add_string(atom_to_list(X), $'), $'].
encode_binary(X) -> [integer_to_list(size(X)), $~,X,$~].
    
do_encode(X, Dict) when atom(X); integer(X); binary(X) ->
    case dict:find(X, Dict) of
	{ok, Y} ->
	    Y;
	error ->
	    encode_obj(X)
    end;
do_encode({'#S', Str}, Dict) ->
    %% This *is* a string
    encode_string(Str);
do_encode([H|T], Dict) ->
    S1  = do_encode(T, Dict),
    S2  = do_encode(H, Dict),
    [S1,S2,$&];
do_encode(T, Dict) when tuple(T) ->
    S1 = encode_tuple(1, T, Dict),
    [${,S1,$}];
do_encode([], Dict) ->
    $#.

encode_list([H|T], Dict, L) ->
    encode_list(T, Dict, [$&,do_encode(H, Dict)|L]);
encode_list([], Dict, L) ->
    reverse(L).

encode_tuple(N, T, Dict) when N > size(T) ->
    "";
encode_tuple(N, T, Dict) ->
    S1 = do_encode(element(N, T), Dict),
    S2 = encode_tuple(N+1, T,  Dict),
    [S1,possible_comma(N, T),S2].

possible_comma(N, T) when N < size(T) -> $,;
possible_comma(_, _)                  -> [].

%% The ascii printables are in the range 32..126 includive

add_string([$\\|T], Quote)   -> [$\\,$\\|add_string(T, Quote)];
add_string([Quote|T], Quote) -> [$\\,Quote|add_string(T, Quote)];
add_string([H|T], Quote) when H >= 0,  H=< 255 -> [H|add_string(T, Quote)];
add_string([H|_], Quote) -> exit({string_character,H});
add_string([], _)            -> [].
    
deabstract({'#S',S}) -> S;
deabstract(T) when tuple(T) ->
    list_to_tuple(map(fun deabstract/1, tuple_to_list(T)));
deabstract([H|T]) -> [deabstract(H)|deabstract(T)];
deabstract(T) -> T.
    
