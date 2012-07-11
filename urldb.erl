-module(urldb).
-export([init/2,init/1]).
-export([dbLoop/2]).
-export([test/0]).

init(MaxEntries)-> init(MaxEntries,node()).

init(MaxEntries,Node)->
    Pid=spawn(Node,?MODULE,dbLoop,[MaxEntries,[]]),
    Pid.


len(List)->len(List, 0).
len([_|Xs],Acc)->len(Xs,Acc+1);
len([],Acc)->Acc.
dbLoop(MaxEntries,DB)->
    % Die when MaxEntries count reached
    case len(DB) of 
        MaxEntries -> 
            % database process dies here due to the maximum size limit 
            exit("Maximum size of DB reached, Quitting...");
        _ -> continue
    end,
            
    receive
        {{increment, URL, Amount},Pid}->
            io:fwrite("DB: incrementing ~p by ~p~n",[URL, Amount]),
            Pid ! {incr_ok,self()},
            UpdatedDB=updateDB(DB,URL,Amount);
        {{load_aggregated},Pid}->
            io:fwrite("DB: loading aggregated data",[]),
            Pid ! {db2Json(DB),self()},
            UpdatedDB=DB % for tail-recursion
    end,
    io:fwrite("UPDATED DB ~p ~n ",[UpdatedDB]),
    dbLoop(MaxEntries, UpdatedDB).


update(K,V,KVList)->
    update(K,list_to_integer(atom_to_list(V)),KVList,[]).
update(K,V,[{K2,V2}|KVList],Acc)->
    case K of 
        K2 ->
            lists:flatten([[{K,V+V2}|KVList]|Acc]);
        _ ->
            update(K,V,KVList,[{K2,V2}|Acc])
    end;        
update(K,V,_,Acc)->
    lists:flatten([{K,V}|Acc]).

updateDB(OldState,K,V)->
    update(K,V,OldState).

% 200 OK
% [{ "label": "devtools", "data": 10},{ "label": "fullof.bs", "data": 3},{ "label": "learnyousomeerlang.com", "data": 12},{ "label": "www.erlang.org", "data": 48}]

keyValList2Json([{Key,Val}|KVs],JSN)->
    case KVs of
        [] ->
            Sep="";
        _ ->
            Sep=","
    end,
    KeyStr=atom_to_list(Key),
    ValStr=integer_to_list(Val),
    keyValList2Json(KVs,Sep ++ "{\"label\":\"" ++ KeyStr ++"\" ,\"data\":" ++ValStr ++ "}" ++ JSN);
keyValList2Json(_,JSN)->JSN.


db2Json(DB)->
    "["++keyValList2Json(DB,"]").




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Testing utilities

echo()->
    receive 
        X -> 
            io:format("Received: ~p~n",[X])
    end,
    echo().

test()->
    P2=init(10),
    Echo = spawn(fun echo/0),
    P2 ! {{increment, 'www.ee', '1111'},Echo},
    P2 ! {{increment, 'www.ee', '1111'},Echo},
    P2 ! {{load_aggregated},Echo}.

