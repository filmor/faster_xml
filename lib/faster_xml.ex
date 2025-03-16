defmodule :faster_xml do
  # @type spec() :: %{
  #         (Tag :: binary()) => element_spec()
  #       }
  # @type element_spec() :: %{
  #         (Key :: binary()) => item_type()
  #       }
  # @type item_type() ::
  #         int
  #         | timestamp
  #         | float
  #         | string
  #         | element_spec()
  #         | {list, item_type()}

    use Rustler,
        otp_app: :faster_xml,
        crate: "faster_xml_nif",
        path: "rust_src/faster_xml_nif/"

  # @spec parse(binary(), spec()) -> {ok, reference()}
  def parse(bin, spec) do
    ref = make_ref()
    pid = self()
    parse(pid, ref, bin, spec)
    {:ok, ref}
  end

  # @spec parse(pid(), reference(), binary(), spec()) -> {ok, reference()}
  def parse(_Pid, _Ref, _Bin, _Spec) do
    :erlang.nif_error(:nif_not_loaded)
  end

  # @spec parse_file(filename:type(), spec()) -> {ok, reference()}
  def parse_file(fname, spec) do
    {:ok, bin} = :file.read_file(fname)
    parse(bin, spec)
  end
end

# -export([
#     parse/2,
#     parse/4,
#     parse_file/2
# ]).
#
# -export_type([
#     spec/0,
#     item_type/0,
#     element_spec/0
# ]).
