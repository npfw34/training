-module(myapp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, find_elem/2, delete_elem/2, split_list/2, random_list/1, flatten_list/1,append_list/2]).

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


%% tests

first_test()->
   ?assert(true),
   ?assertEqual(flatten_list([[1,[2,[3],[]]], [[[4]]], [5,6]]), [1,2,3,4,5,6] ),
   ?assertError(badarg,flatten_list(test) ).






