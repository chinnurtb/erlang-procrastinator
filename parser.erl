-module(parser).
-export([parse/1]). 

-export([test/0]). 

toKeyValPairs([X|Xs], Sep, Acc)->
    T=string:tokens(X,Sep),
    Val=lists:nth(2,T),
    toKeyValPairs(Xs,Sep, [list_to_atom(Val)|Acc]);
toKeyValPairs(_, _, Acc)-> 
    lists:reverse(Acc).
  
parse(Str)->
    io:fwrite("Parsing string: ~s ~n",[Str]),
    case string:str(Str,"cmd=") of 
        N when N >= 1 -> 
            io:fwrite("Parsing string to command: [~s] ~n",[Str]),
            RawArgs=lists:nth(1,string:tokens(lists:nth(2,string:tokens(Str,"?"))," ")), % cut on the argments part of GET request
            Command=toKeyValPairs(string:tokens(RawArgs,"&"),"=",[]),
            io:fwrite("Command: ~n~p ~n",[Command]),
            Command;
         _ -> 
            [error]
    end.
        
test()->
    parse("GET /?cmd=increment&url=www.foo.com&time=123 HTTP/1.1"),
    parse("GET /?cmd=load_aggregated HTTP/1.1").
    
