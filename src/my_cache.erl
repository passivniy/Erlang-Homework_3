-module(my_cache).

-export([create/1,insert/4,delete_obsolete/1]).

-export([lookup/2,lookup_all/1]).

-export([get_current_time_in_seconds/0,get_end_time/1,get_time_in_seconds/2]).

create(TableName)->
  Res = ets:new(TableName,[set, public, named_table]),
  {ok,Res}.

insert(TableName, Key, Value, Timer)->
  ets:insert(TableName,{Key,Value,calendar:universal_time(),get_end_time(Timer)}),
  {ok,insert}.

lookup(TableName,Key)->
  ets:lookup(TableName,Key).

lookup_all(TableName)->
  lookup_all(TableName,ets:first(TableName),[]).

delete_obsolete(TableName)->
  NowSeconds = calendar:universal_time(),
  Result = ets:select(TableName,[{{'$1','$2','$3','$4'},[{'>',{const,NowSeconds},('$4')}],['$1']}]),
  lists:foreach(fun(Key)->ets:delete(TableName,Key) end,Result).

%PRIVATE
%=============================================================================
lookup_all(TableName,Key,Acc)->
  case ets:next(TableName,Key) of
    '$end_of_table'-> lists:reverse([my_cache:lookup(TableName,Key)|Acc]);
    _Res -> lookup_all(TableName,_Res,[my_cache:lookup(TableName,Key)|Acc])
  end.
%=============================================================================
%TIME
%=============================================================================
get_end_time(Timer)->
  calendar:gregorian_seconds_to_datetime(get_current_time_in_seconds()+Timer*60).

get_time_in_seconds(Timer,AddDate)->
  (calendar:datetime_to_gregorian_seconds(AddDate))+Timer*60.

get_current_time_in_seconds()->
  calendar:datetime_to_gregorian_seconds(calendar:universal_time()).
%=============================================================================