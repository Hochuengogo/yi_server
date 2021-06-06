-module(proto_100).
-export([pack/3, unpack/3]).


pack(10000, req, {Val_srv_id_1,Val_account_2,Val_timestamp_3,Val_sign_4}) ->
    <<(proto_core:pack_string(Val_srv_id_1))/binary,(proto_core:pack_string(Val_account_2))/binary,Val_timestamp_3:32,(proto_core:pack_string(Val_sign_4))/binary>>;
pack(10000, res, Val_role_info_6) ->
    <<(length(Val_role_info_6)):16,(list_to_binary([<<Val_id_1:32,(proto_core:pack_string(Val_srv_id_2))/binary,(proto_core:pack_string(Val_name_3))/binary,Val_lev_4:16,Val_sex_5:8>>||{Val_id_1,Val_srv_id_2,Val_name_3,Val_lev_4,Val_sex_5}<-Val_role_info_6]))/binary>>;

pack(10001, req, {Val_srv_id_1,Val_account_2,Val_timestamp_3,Val_sign_4}) ->
    <<(proto_core:pack_string(Val_srv_id_1))/binary,(proto_core:pack_string(Val_account_2))/binary,Val_timestamp_3:32,(proto_core:pack_string(Val_sign_4))/binary>>;
pack(10001, res, {Val_srv_id_1}) ->
    <<(proto_core:pack_string(Val_srv_id_1))/binary>>;

pack(10002, req, {Val_srv_id_1,Val_account_2,Val_timestamp_3,Val_id_4}) ->
    <<(proto_core:pack_string(Val_srv_id_1))/binary,(proto_core:pack_string(Val_account_2))/binary,Val_timestamp_3:32,Val_id_4:32>>;
pack(10002, res, {Val_srv_id_1}) ->
    <<(proto_core:pack_string(Val_srv_id_1))/binary>>;

pack(10003, req, {}) ->
    <<>>;
pack(10003, res, {Val_code_1,Val_args_2,Val_desc_3}) ->
    <<Val_code_1:8,(length(Val_args_2)):16,(list_to_binary([<<(proto_core:pack_string(Val__args_2))/binary>>||Val__args_2<-Val_args_2]))/binary,(proto_core:pack_string(Val_desc_3))/binary>>;

pack(10004, req, {}) ->
    <<>>;
pack(10004, res, {Val_timestamp_1,Val_time_zone_2}) ->
    <<Val_timestamp_1:32,Val_time_zone_2:8/signed>>;

pack(Code, _Flag, _Data) ->
    {error, {error_bad_code, Code}}.


unpack(10000, req, Bin) ->
    {Val_srv_id_1,Bin_1} = proto_core:unpack_string(Bin),
    {Val_account_2,Bin_2} = proto_core:unpack_string(Bin_1),
    {Val_timestamp_3,Bin_3} = proto_core:unpack_uint32(Bin_2),
    {Val_sign_4,Bin_4} = proto_core:unpack_string(Bin_3),
    _ = Bin_4,
    {Val_srv_id_1,Val_account_2,Val_timestamp_3,Val_sign_4};
unpack(10000, res, Bin) ->
    {Val_role_info_6,Bin_7} = proto_core:unpack_array(Bin,
    fun(Bin_1) ->
        {Val_id_1,Bin_2} = proto_core:unpack_uint32(Bin_1),
        {Val_srv_id_2,Bin_3} = proto_core:unpack_string(Bin_2),
        {Val_name_3,Bin_4} = proto_core:unpack_string(Bin_3),
        {Val_lev_4,Bin_5} = proto_core:unpack_uint16(Bin_4),
        {Val_sex_5,Bin_6} = proto_core:unpack_uint8(Bin_5),
        {{Val_id_1,Val_srv_id_2,Val_name_3,Val_lev_4,Val_sex_5},Bin_6}
    end),
    _ = Bin_7,
    Val_role_info_6;

unpack(10001, req, Bin) ->
    {Val_srv_id_1,Bin_1} = proto_core:unpack_string(Bin),
    {Val_account_2,Bin_2} = proto_core:unpack_string(Bin_1),
    {Val_timestamp_3,Bin_3} = proto_core:unpack_uint32(Bin_2),
    {Val_sign_4,Bin_4} = proto_core:unpack_string(Bin_3),
    _ = Bin_4,
    {Val_srv_id_1,Val_account_2,Val_timestamp_3,Val_sign_4};
unpack(10001, res, Bin) ->
    {Val_srv_id_1,Bin_1} = proto_core:unpack_string(Bin),
    _ = Bin_1,
    {Val_srv_id_1};

unpack(10002, req, Bin) ->
    {Val_srv_id_1,Bin_1} = proto_core:unpack_string(Bin),
    {Val_account_2,Bin_2} = proto_core:unpack_string(Bin_1),
    {Val_timestamp_3,Bin_3} = proto_core:unpack_uint32(Bin_2),
    {Val_id_4,Bin_4} = proto_core:unpack_uint32(Bin_3),
    _ = Bin_4,
    {Val_srv_id_1,Val_account_2,Val_timestamp_3,Val_id_4};
unpack(10002, res, Bin) ->
    {Val_srv_id_1,Bin_1} = proto_core:unpack_string(Bin),
    _ = Bin_1,
    {Val_srv_id_1};

unpack(10003, req, Bin) ->
    _ = Bin,
    {};
unpack(10003, res, Bin) ->
    {Val_code_1,Bin_1} = proto_core:unpack_uint8(Bin),
    {Val_args_2,Bin_2} = proto_core:unpack_array(Bin_1,fun proto_core:unpack_string/1),
    {Val_desc_3,Bin_3} = proto_core:unpack_string(Bin_2),
    _ = Bin_3,
    {Val_code_1,Val_args_2,Val_desc_3};

unpack(10004, req, Bin) ->
    _ = Bin,
    {};
unpack(10004, res, Bin) ->
    {Val_timestamp_1,Bin_1} = proto_core:unpack_uint32(Bin),
    {Val_time_zone_2,Bin_2} = proto_core:unpack_int8(Bin_1),
    _ = Bin_2,
    {Val_timestamp_1,Val_time_zone_2};

unpack(Code, _Flag, _Bin) ->
    {error, {error_bad_code, Code}}.
