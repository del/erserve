-module(erserve_data).


%%%_* Exports ------------------------------------------------------------------
-export([ df_by_cols/1
        , to_erlang/1
        ]).


%%%_* Types --------------------------------------------------------------------


%%%_* External API -------------------------------------------------------------
-spec df_by_cols(erserve:r_df()) -> {ok, [ erserve:df() ]}.
df_by_cols({xt_has_attr, {{xt_list_tag, Tag}, {xt_vector, Data}}}) ->
  case lists:keyfind({xt_str, "names"}, 1, Tag) of
    {{xt_str, "names"}, {xt_array_str, Names}} ->
      {ok, df_by_cols_1(Names, Data)};
    undefined                                  ->
      error
  end.

-spec to_erlang(erserve:r_data()) -> erserve:untagged_data().
to_erlang({xt_has_attr, {{xt_list_tag, Tag}, {xt_vector, _Data}}} = Df) ->
  case lists:keyfind({xt_str, "class"}, 1, Tag) of
    {{xt_str, "class"}, {xt_array_str, ["data.frame"]}} ->
      df_by_cols(Df);
    undefined                                           ->
      error
  end;
to_erlang({xt_vector,   Data}) ->
  lists:map(fun to_erlang/1, Data);
to_erlang({xt_list_tag, Data}) ->
  lists:map(fun({Tag, Val}) ->
                {to_erlang(Tag), to_erlang(Val)}
            end, Data);
to_erlang({_Type,       Data}) ->
  Data.


%%%_* Internal functions -------------------------------------------------------
df_by_cols_1(Names, Data) ->
  NamedCols = lists:zip(Names, Data),
  lists:map(fun({Name, Col}) ->
                {Name, to_erlang(Col)}
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

-spec type(erserve:r_data()) -> erserve:r_type().
type({xt_has_attr, Data}) ->
  case {is_df(Data), is_r_list(Data)} of
    {true, false}  -> dataframe;
    {false, true}  -> list;
    {false, false} -> unsupported
  end;
type({Type, _Data})       ->
  Type.
