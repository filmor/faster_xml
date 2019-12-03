-module(faster_xml).

-include("crates.hrl").

-type pattern() :: any().

-export([
    parse/2,
    parse/3,
    parse_file/2
]).

-on_load(init/0).

init() ->
    erlang:load_nif(?crate_faster_xml_nif, 0).

-spec parse(binary(), pattern()) -> ok.
parse(Bin, Pattern) ->
    parse(self(), Bin, Pattern).

-spec parse(pid(), binary(), pattern()) -> ok.
parse(_Pid, _Bin, _Pattern) ->
    erlang:nif_error(nif_not_loaded).

-spec parse_file(filename:type(), pattern()) -> ok.
parse_file(FName, Pattern) ->
    {ok, Bin} = file:read_file(FName),
    parse(Bin, Pattern).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

epex_test() ->
    ok = parse_file(
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

    flush(),

    ok.

epias_test() ->
    ok = parse_file(
        "epias.xml",
        #{
            <<"Teklif">> => #{
                <<"fiyat">> => float,
                <<"miktar">> => int,
                <<"kalanMiktar">> => int
            }
        }
    ),

    flush(),

    ok.


flush() ->
    receive Msg ->
        io:format("~p~n", [Msg]),
        flush()
    after 1000 ->
        ok
    end.

-endif.