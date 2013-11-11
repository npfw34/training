-module(myapp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, find_elem/2, delete_elem/2, split_list/2, random_list/1, flatten_list/1,append_list/2, parse_file/1, rle_list/1]).

%%-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  myapp_sup:start_link().

stop(_State) ->
  ok.

%% find k elem of a list
find_elem([],_) ->  error;
find_elem([H|T],1) ->  H;
find_elem([H|T],K) ->  find_elem(T,K-1).

%% delete k elem of a list

delete_elem(L,K) ->  delete_k_elem(L,1,K,[]).

delete_k_elem([],_,_,Acc) ->
  lists:reverse(Acc);
delete_k_elem([H|T],C,K,Acc) ->
  case C == K of true -> delete_k_elem(T,C+1,K,Acc);
    _->  delete_k_elem(T,C+1,K,[H|Acc])
  end.

%% split the list
split_list(L,K) ->  split_two_lists(L,1,K,[]).
split_two_lists([H|T],C,K,Acc)->
  case (C =< K)  of true -> split_two_lists(T,C+1,K,[H|Acc]);
    _-> {lists:reverse(Acc),[H|T]}
  end.

%% list random
random_list(L)->random_list_my(L,[]).

random_list_my([],Acc)->
  Acc;
random_list_my([H|T],Acc)->
  Item = random:uniform(length([H|T])),
  random_list_my(T,[find_elem([H|T],Item)|Acc]).

%% list append & flatten

append_list([H | T], L) -> [H | append_list(T, L)];
append_list([], L) -> L.

flatten_list([[_|_]=H|T]) -> append_list(flatten_list(H), flatten_list(T));
flatten_list([[]|T]) -> flatten_list(T);
flatten_list([H|T]) -> [H|flatten_list(T)];
flatten_list([]) -> [].

%% rle list
rle_list(List) ->
    rle_list(List, [], 1, []).

rle_list([], _, _, Rest) ->
    lists:reverse(Rest);
rle_list([H | T], H, C, Rest) ->
    rle_list([H | T], H, C + 1, Rest);
rle_list([H | T], _X, 1, Rest)->
    rle_list(T, H, 1, [H | Rest]);
rle_list([H | T], X, C, Rest) ->
    rle_list(T, H, 1, [{X, C} | Rest]).

%% tests

%%first_test()->
%%   ?assert(true),
%%   ?assertEqual(flatten_list([[1,[2,[3],[]]], [[[4]]], [5,6]]), [1,2,3,4,5,6] ),
%%   ?assertError(badarg,flatten_list(test) ).

%%prop_delete() ->
%%  ?FORALL({X,L}, {list(integer()),list(integer())},
%%    split_list(X, L)).
%%proper_test() ->
%%  ?assertEqual(
%%    proper:module(?MODULE, [{to_file, user},
%%      {numtests, 1000}]),
%%    []).

parse_file(File)->
  {ok, Binary} = file:read_file(File),
  parse_csv(Binary).

parse_csv(Binary)->
  List1 = [ BinaryList || BinaryList <-binary:split(Binary,[<<"\n">>,<<"\r">>],[global]), BinaryList =/= <<>>],
  List2 = [ binary:split(BinaryList,[<<",">>],[global]) || BinaryList <-List1 ],
  List3 = [parse_triplet(BinaryList) || BinaryList <-List2].

parse_triplet(L)->
  [get_type(Out) || Out <-L].

get_type(<<"\"", QuotStr/binary>>) ->
  [Str, _Rest] = binary:split(QuotStr, [list_to_binary("\"")], [global]),
  binary_to_list(Str);

get_type(Num) ->
  str_to_int_or_float({},Num).

str_to_int_or_float({},Num) ->
  str_to_int_or_float(string:to_float(binary_to_list(Num)),Num);
str_to_int_or_float({error,no_float},Num) ->
  str_to_int_or_float(string:to_integer(binary_to_list(Num)),Num);
str_to_int_or_float({X,[]},Num) ->
  X.





