-module(r9_config).
-export([load/1,
        get/2]).

-include("r9_dns.hrl").

-record(config_db, {config_attrs}).
-record(config_attr, {key, value}).


load(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    ConfigTable = ets:new(config_table, [{keypos, #config_attr.key}]),
    parse_file(File, ConfigTable),
    #config_db{config_attrs = ConfigTable}.
    
parse_file(File, ConfigTable) ->
    case io:get_line(File, "") of
        eof  -> [];
        Line -> [Key, Value] = string:tokens(string:strip(Line, right, $\n), " "),
                ets:insert(ConfigTable, #config_attr{key = Key, value = Value}),
                parse_file(File, ConfigTable)
    end.

get(#config_db{config_attrs = ConfigTable}, Key) ->
    case ets:lookup(ConfigTable, Key) of
         [] -> {not_found};
         [#config_attr{value = Value}] -> {ok, Value}
     end. 
