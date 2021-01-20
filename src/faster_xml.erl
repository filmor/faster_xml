-module(faster_xml).

-include("crates.hrl").

-type spec() :: #{
    Tag :: binary() => element_spec()
}.

% Key may start with "@" to address attributes
-type element_spec() :: #{
    Key :: binary() => item_type()
}.

-type item_type() ::
    int
    | timestamp
    | float
    | string
    | element_spec()
    | {list, item_type()}.

-export([
    parse/2,
    parse/4,
    parse_file/2
]).

-export_type([
    spec/0,
    item_type/0,
    element_spec/0
]).

-on_load(init/0).

init() ->
    ?load_nif_from_crate(faster_xml, ?crate_faster_xml_nif, 0).

-spec parse(binary(), spec()) -> {ok, reference()}.
parse(Bin, Spec) ->
    Ref = make_ref(),
    Pid = self(),
    parse(Pid, Ref, Bin, Spec),
    {ok, Ref}.

-spec parse(pid(), reference(), binary(), spec()) -> {ok, reference()}.
parse(_Pid, _Ref, _Bin, _Spec) ->
    erlang:nif_error(nif_not_loaded).

-spec parse_file(filename:type(), spec()) -> {ok, reference()}.
parse_file(FName, Spec) ->
    {ok, Bin} = file:read_file(FName),
    parse(Bin, Spec).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
