-module(my_cache_test).

-include_lib("eunit/include/eunit.hrl").

my_cache_test_()->[
  my_cache:create(table) =:= {ok,table},
  my_cache:insert(table,a,1,10) =:= {ok,insert},
  my_cache:insert(table,b,2,10) =:= {ok,insert},
  my_cache_lookup_test(),
  my_cache:lookup(table,d) =:= []
].

my_cache_lookup_test()->
  [{_Key,_Value,AddTime,TimeToLive}] = ets:lookup(table,a),
  my_cache:lookup(table,a) =:= [{a,1,AddTime,TimeToLive}].