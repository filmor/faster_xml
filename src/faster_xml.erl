-module(faster_xml).

-include("crates.hrl").

-type pattern() :: any().

-export([
    parse/2,
    parse/4,
    parse_file/2
]).

-on_load(init/0).

init() ->
    erlang:load_nif(?crate_faster_xml_nif, 0).

-spec parse(binary(), pattern()) -> {ok, reference()}.
parse(Bin, Pattern) ->
    Ref = make_ref(),
    Pid = self(),
    parse(Pid, Ref, Bin, Pattern),
    {ok, Ref}.

-spec parse(pid(), reference(), binary(), pattern()) -> {ok, reference()}.
parse(_Pid, _Ref, _Bin, _Pattern) ->
    erlang:nif_error(nif_not_loaded).

-spec parse_file(filename:type(), pattern()) -> {ok, reference()}.
parse_file(FName, Pattern) ->
    {ok, Bin} = file:read_file(FName),
    parse(Bin, Pattern).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

epex_test_() ->
    {timeout, 30, ?_assert(epex_case())}.

epias_test_() ->
    {timeout, 30, ?_assert(epias_case())}.

epex_case() ->
    {ok, Ref} = faster_xml:parse_file(
        "epex.xml",
        #{
            <<"PblcTradeConf">> => #{
                <<"@qty">> => int,
                <<"@px">> => int,
                <<"@tradeExecTime">> => timestamp,
                <<"@revisionNo">> => int
            }
        }
    ),

    {N, {Ref, <<"PblcTradeConf">>, Last}} = flush(0, undefined),

    ?assert(N > 0),

    ?assertMatch(
        #{
            <<"@qty">> := _,
            <<"@px">> := _,
            <<"@tradeExecTime">> := _,
            <<"@revisionNo">> := _
        },
        Last
    ),

    true.

epias_case() ->
    {ok, Ref} = faster_xml:parse_file(
        "epias.xml",
        #{
            <<"Teklif">> => #{
                <<"fiyat">> => float,
                <<"miktar">> => int,
                <<"kalanMiktar">> => int
            }
        }
    ),

    {N, {Ref, <<"Teklif">>, Last}} = flush(0, undefined),

    ?assert(N > 0),

    ?assertMatch(
        #{
            <<"fiyat">> := _,
            <<"miktar">> := _,
            <<"kalanMiktar">> := _
        },
        Last
    ),

    true.


flush(N, Last) ->
    receive
        {_, done} ->
            {N, Last};
        Msg ->
            flush(N + 1, Msg)
    after 1000 ->
        {error, no_done, {N, Last}}
    end.

-endif.