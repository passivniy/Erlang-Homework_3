-module(my_cache).

-export([create/1,insert/4,delete_obsolete/1]).

-export([lookup/2,lookup_all/1]).

-export([get_current_time_in_seconds/0,get_end_time/1,get_time_in_seconds/2]).

lookup_all(TableName)->
  lookup_all(TableName,ets:first(TableName),[]).

delete_obsolete(TableName)->
  delete_obsolete(TableName,ets:first(TableName),[]).

create(TableName)->
  ets:new(TableName,[set, public, named_table]).

insert(TableName, Key, Value, Timer)->
  ets:insert(TableName,{Key,Value,Timer,calendar:universal_time()}).

lookup(TableName,Key)->
  [{Key,_,Timer,AddDate}] = ets:lookup(TableName,Key),
  case get_current_time_in_seconds()>get_time_in_seconds(Timer,AddDate) of
    true->undefined;
    false-> ets:lookup(TableName,Key)
  end.

lookup_all(TableName,Key,Acc)->
  case ets:next(TableName,Key) of
    '$end_of_table'-> lists:reverse([my_cache:lookup(TableName,Key)|Acc]);
    _Res -> lookup_all(TableName,_Res,[my_cache:lookup(TableName,Key)|Acc])
  end.

delete_obsolete(TableName,_Key,Acc)->
  case ets:next(TableName,_Key) of
    '$end_of_table' -> true;
    _Res ->
      [{Key,Value,Timer,AddDate}] = ets:lookup(TableName,_Res),
      CurrentDate=get_current_time_in_seconds(),
      DataBaseDate=get_time_in_seconds(Timer,AddDate),
      case CurrentDate>=DataBaseDate of
        true->ets:delete(TableName,_Res),delete_obsolete(TableName,_Key,[]);
        false->delete_obsolete(TableName,_Res,[[{Key,Value,Timer,AddDate}]|Acc])
      end
  end.

get_end_time(Timer)->
  calendar:gregorian_seconds_to_datetime(get_current_time_in_seconds()+Timer*60).

get_time_in_seconds(Timer,AddDate)->
  (calendar:datetime_to_gregorian_seconds(AddDate))+Timer*60.

get_current_time_in_seconds()->
  calendar:datetime_to_gregorian_seconds(calendar:universal_time()).