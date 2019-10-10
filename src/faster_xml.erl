-module(faster_xml).

-export([
    parse/1,
    parse_file/1
]).

-on_load(init/0).

init() ->
    {ok, Lib} = find_crate:find_library(?MODULE, "faster_xml_nif"),
    io:format("~s", [Lib]),
    erlang:load_nif(Lib, 0).


parse(Bin) ->
    parse(self(), Bin).

parse(_Pid, _Bin) ->
    erlang:nif_error(nif_not_loaded).

parse_file(FName) ->
    {ok, Bin} = file:read_file(FName),
    parse(Bin).