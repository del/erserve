%%%-----------------------------------------------------------------------------
%%% @doc This module handles detecting the types of R data received, and parsing
%%%      it from the internal format to a more Erlang-style format, e.g. lists
%%%      of integers, floats, strings, or proplists for representation of R's
%%%      data frames and lists.
%%%
%%% @author Daniel Eliasson <daniel@danieleliasson.com>
%%% @copyright 2012 Daniel Eliasson; Apache 2.0 license -- see LICENSE file
%%% @end------------------------------------------------------------------------
-module(erserve_data).


%%%_* Exports ------------------------------------------------------------------
-export([ parse/1
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


%%%_* Internal functions -------------------------------------------------------
-spec by_cols(erserve:r_df()) -> erserve:df().
by_cols({xt_has_attr, {{xt_list_tag, Tag}, {xt_vector, Data}}}) ->
  NamedCols = lists:zip(names(Tag), Data),
  lists:map(fun({Name, Col}) ->
                {Name, parse(Col)}
            end, NamedCols).

-spec class(erserve:tag()) -> erserve:r_class() | undefined.
class(Tag) ->
  case proplists:get_value({xt_str, "class"}, Tag) of
    {xt_array_str, [Class]} -> Class;
    undefined               -> undefined
  end.

-spec names(erserve:tag()) -> [ string() ] | undefined.
names(Tag) ->
  case proplists:get_value({xt_str, "names"}, Tag) of
    {xt_array_str, Names} -> Names;
    undefined             -> undefined
  end.

-spec is_df(erserve:r_data()) -> boolean().
is_df({{xt_list_tag, Tag}, {xt_vector, _Data}}) ->
  case class(Tag) of
    "data.frame" -> true;
    _            -> false
  end;
is_df(_Rdata)                                   ->
  false.

-spec is_r_list(erserve:r_data()) -> boolean().
is_r_list({{xt_list_tag, Tag}, {xt_vector, _Data}} = Expr) ->
  is_df(Expr) =:= false andalso names(Tag) =/= undefined;
is_r_list(_Rdata)                                          ->
  false.
