-define(cmd_void_eval,        2).
-define(cmd_eval,             3).
-define(cmd_set_sexp,         32).

-define(resp_ok,              16#10001).
-define(resp_error,           16#10002).

-define(dt_int,               1).   % 01
-define(dt_char,              2).   % 02
-define(dt_double,            3).   % 03
-define(dt_string,            4).   % 04
-define(dt_bytestream,        5).   % 05
-define(dt_sexp,              10).  % 0A
-define(dt_array,             11).  % 0B
-define(dt_large,             64).  % 40

-define(xt_null,              0).   % 00
-define(xt_int,               1).   % 01
-define(xt_double,            2).   % 02
-define(xt_str,               3).   % 03
-define(xt_lang,              4).   % 04
-define(xt_vector,            16).  % 10
-define(xt_list,              17).  % 11
-define(xt_clos,              18).  % 12
-define(xt_symname,           19).  % 13
-define(xt_list_notag,        20).  % 14
-define(xt_list_tag,          21).  % 15
-define(xt_lang_notag,        22).  % 16
-define(xt_lang_tag,          23).  % 17
-define(xt_vector_exp,        26).  % 1A
-define(xt_array_int,         32).  % 20
-define(xt_array_double,      33).  % 21
-define(xt_array_str,         34).  % 22
-define(xt_array_bool,        36).  % 24

-define(xt_large,             64).  % 40
-define(xt_has_attr,          128). % 80

-define(err_auth_failed,      65).
-define(err_conn_broken,      66).
-define(err_inv_cmd,          67).
-define(err_inv_par,          68).
-define(err_r_error,          69).
-define(err_io_error,         70).
-define(err_not_open,         71).
-define(err_access_denied,    72).
-define(err_unsupported_cmd,  73).
-define(err_unknown_cmd,      74).
-define(err_data_overflow,    75).
-define(err_object_too_big,   76).
-define(err_out_of_mem,       77).
-define(err_ctrl_closed,      78).
-define(err_session_busy,     80).
-define(err_detach_failed,    81).

-define(size_int,             4).
-define(size_double,          8).
-define(size_char,            1).
-define(size_bool,            1).
