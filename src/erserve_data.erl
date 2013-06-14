%%------------------------------------------------------------------------------
%% @doc This module handles detecting the types of R data received, and parsing
%%      it from the internal format to a more Erlang-style format, e.g. lists
%%      of integers, floats, strings, or proplists for representation of R's
%%      data frames and lists.
%%
%% @author Daniel Eliasson <daniel@danieleliasson.com>
%% @copyright 2012 Daniel Eliasson; Apache 2.0 license -- see LICENSE file
%% @end-------------------------------------------------------------------------
-module(erserve_data).


%%%_* Exports ------------------------------------------------------------------
-export([ dataframe_to_proplist/1
        , parse/1
        , proplist_to_dataframe/1
        , to_r_value/1
        , type/1
        ]).


%%%_* External API -------------------------------------------------------------

-spec type(erserve:r_data()) -> erserve:r_type().
type({xt_has_attr, Data}) ->
  case {is_df(Data), is_r_list(Data)} of
    {true, false}  -> dataframe;
    {false, true}  -> list;
    {false, false} -> unsupported
  end;
type({Type, _Data})       ->
  Type.

-spec parse(erserve:r_data()) -> erserve:untagged_data().
parse({xt_has_attr, _Data} = Rdata) ->
  case type(Rdata) of
    dataframe   -> by_cols(Rdata);
    list        -> by_cols(Rdata);
    unsupported -> unsupported
  end;
parse({xt_vector,   Data})          ->
  lists:map(fun parse/1, Data);
parse({xt_list_tag, Data})          ->
  lists:map(fun({Tag, Val}) ->
                {parse(Tag), parse(Val)}
            end, Data);
parse({_Type,       Data})          ->
  Data.

-spec proplist_to_dataframe([proplist:property()]) -> erserve:r_df().
proplist_to_dataframe(Plist) ->
  lists:foldl(fun({Key, Value0}, Acc) ->
                  {Type, Value} = to_r_value(Value0),
                  case Type of
                    undefined -> Acc;
                    _         -> [{Key, Type, Value}|Acc]
                  end
              end, [], Plist).

-spec dataframe_to_proplist(erserve:r_df()) -> [proplist:property()].
dataframe_to_proplist(Df) -> dataframe_to_proplist_1(parse(Df)).

-spec dataframe_to_proplist_1(erserve:untagged_data()) -> [proplist:property()].
dataframe_to_proplist_1(null) -> [];
dataframe_to_proplist_1(Df)   ->
  lists:foldl(fun(KV0, Acc) ->
                  case from_r_key_values(KV0) of
                    {_Key, unsupported} -> Acc;
                    KV                  -> [KV|Acc]
                  end
              end, [], Df).


%%%_* Internal functions -------------------------------------------------------

-spec to_r_value( integer()
                | [ integer() ]
                | float()
                | [ float() ]
                | [ string() ] ) -> [ erserve:r_data() ].
to_r_value(I)           when is_integer(I) ->
  {xt_array_int, [I]};
to_r_value(Value=[I|_]) when is_integer(I) ->
  case is_string(Value) of
    true  -> {xt_array_str, [Value]};
    false -> {xt_array_int, Value}
  end;
to_r_value(F)           when is_float(F)   ->
  {xt_array_double, [F]};
to_r_value(Value=[F|_]) when is_float(F)   ->
  {xt_array_double, Value};
to_r_value(Value=[S|_]) when is_list(S)    ->
  {xt_array_str, Value};
to_r_value(no_value)                       ->
  {undefined, no_value};
to_r_value(null)                           ->
  {undefined, no_value};
to_r_value(true)                           ->
  {xt_array_bool, [true]};
to_r_value(false)                          ->
  {xt_array_bool, [false]};
to_r_value(A)           when is_atom(A)    ->
  {xt_array_str, [atom_to_list(A)]};
to_r_value([])                             ->
  {xt_array_str, [""]};
to_r_value(B)           when is_binary(B)  ->
  try
    {xt_array_str, [binary_to_list(B)]}
  catch
    _:_ ->
      {undefined, no_value}
  end;
to_r_value(_Other)                         ->
  {undefined, no_value}.

-spec is_string(list()) -> boolean().
is_string(Ints) when is_list(Ints) ->
  lists:all(fun(I) when is_integer(I) ->
                I >= 0 andalso I =< 255
            end, Ints).

-spec from_r_key_values({term(), term()}) -> {atom(), term()}.
from_r_key_values({KeyBin, Value}) when is_binary(KeyBin) ->
  Key = list_to_atom(binary_to_list(KeyBin)),
  from_r_key_values({Key, Value});
from_r_key_values({Key, [Value]})                         ->
  {Key, Value};
from_r_key_values({Key, Value})                           ->
  {Key, Value}.

-spec by_cols(erserve:r_df()) -> erserve:df().
by_cols({xt_has_attr, {{xt_list_tag, Tag}, {xt_vector, Data}}}) ->
  NamedCols = lists:zip(names(Tag), Data),
  lists:map(fun({Name, Col}) ->
                {Name, parse(Col)}
            end, NamedCols).

-spec class(erserve:tag()) -> erserve:r_class() | undefined.
class(Tag) ->
  case proplists:get_value({xt_str, <<"class">>}, Tag) of
    {xt_array_str, [Class]} -> Class;
    undefined               -> undefined
  end.

-spec names(erserve:tag()) -> [ string() ] | undefined.
names(Tag) ->
  case proplists:get_value({xt_str, <<"names">>}, Tag) of
    {xt_array_str, Names} -> Names;
    undefined             -> undefined
  end.

-spec is_df(erserve:r_data()) -> boolean().
is_df({{xt_list_tag, Tag}, {xt_vector, _Data}}) ->
  case class(Tag) of
    <<"data.frame">> -> true;
    _                -> false
  end;
is_df(_Rdata)                                   ->
  false.

-spec is_r_list(erserve:r_data()) -> boolean().
is_r_list({{xt_list_tag, Tag}, {xt_vector, _Data}} = Expr) ->
  is_df(Expr) =:= false andalso names(Tag) =/= undefined;
is_r_list(_Rdata)                                          ->
  false.
