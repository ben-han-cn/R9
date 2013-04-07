-module(r9_rdata_struct).
-export([from_wire/3,
        to_wire/1,
        from_string/1,
        to_string/1]).

-include("r9_dns.hrl").

%rdata API
from_string(IP) ->
    #a{ip = r9_util:ipv4_from_string(IP)}.

% A
from_wire(?TYPE_A, WholeMessage, StartPos) ->
    <<_ParsedData:StartPos/bytes, IP:32/bits, _/bits>> = WholeMessage,
    #a{ip = r9_util:ipv4_from_wire(IP)};

% NS
from_wire(?TYPE_NS, WholeMessage, StartPos) ->
    #domain{domain= element(1, r9_wire_name:from_wire(WholeMessage, StartPos))};

% CNAME
from_wire(?TYPE_CNAME, WholeMessage, StartPos) ->
    #domain{domain= element(1, r9_wire_name:from_wire(WholeMessage, StartPos))};

%SOA
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%/                     MNAME                     /
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%/                     RNAME                     /
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%|                    SERIAL                     |
%|                                               |
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%|                    REFRESH                    |
%|                                               |
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%|                     RETRY                     |
%|                                               |
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%|                    EXPIRE                     |
%|                                               |
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%|                    MINIMUM                    |
%|                                               |
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
from_wire(?TYPE_SOA, WholeMessage, StartPos) ->
    {MName, NextPos} = r9_wire_name:from_wire(WholeMessage, StartPos),
    {RName, NNextPos} = r9_wire_name:from_wire(WholeMessage, NextPos),
    <<_ParsedData:NNextPos/bytes, Serial:32/big, Refresh:32/big, Retry:32/big, Expire:32/big, Minimum:32/big, _/bits>> = WholeMessage,
    #soa{mname = MName, 
        rname = RName, 
        serial = Serial,
        refresh = Refresh, 
        retry = Retry,
        expire = Expire,
        minimum = Minimum};

% PTR
from_wire(?TYPE_PTR, WholeMessage, StartPos) ->
    #domain{domain= element(1, r9_wire_name:from_wire(WholeMessage, StartPos))};

% MX
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%|                  PREFERENCE                   |
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%/                   EXCHANGE                    /
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
from_wire(?TYPE_MX, WholeMessage, StartPos) ->
    <<_ParsedData:StartPos/bytes, Preference:16/big, _/bits>> = WholeMessage,
    #mx{preference = Preference, exchange = r9_wire_name:from_wire(WholeMessage, StartPos + 2)};

from_wire(?TYPE_AAAA, WholeMessage, StartPos) ->
    <<_ParsedData:StartPos/bytes, IP:128/bits, _/bits>> = WholeMessage,
    #aaaa{ip = r9_util:ipv6_from_wire(IP)};


%+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
%|                          OPTION-CODE                          |
%+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
%|                         OPTION-LENGTH                         |
%+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
%/                          OPTION-DATA                          /
%+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
from_wire(?TYPE_OPT, _WholeMessage, _StartPos) ->
    %<<_ParsedData:StartPos/bytes, Code:16/big, Len:16/big, Data/bytes>> = WholeMessage,
    #opt{code = 10, data = ""};


from_wire(?TYPE_TXT, WholeMessage, StartPos) ->
    RdlenPos = StartPos - 2,
    <<_ParsedData:RdlenPos/bytes, Rdlen:16/big, Txt:Rdlen/bytes, _/bits>> = WholeMessage,
    #txt{len = Rdlen, text = Txt};

from_wire(UnknownType, _, _) ->
    io:format("unknownn from wire ~p ~n", [UnknownType]),
    throw("unknown type").


% A
to_wire(#a{ip = IP}) ->
    IPBinary = r9_util:ipv4_to_wire(IP),
    {4, <<IPBinary/binary>>};
% NS
to_wire(#domain{domain= DomainName}) ->
    NameBinary = r9_wire_name:to_wire(DomainName),
    {r9_wire_name:len(DomainName), <<NameBinary/binary>>};

%SOA
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%/                     MNAME                     /
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%/                     RNAME                     /
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%|                    SERIAL                     |
%|                                               |
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%|                    REFRESH                    |
%|                                               |
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%|                     RETRY                     |
%|                                               |
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%|                    EXPIRE                     |
%|                                               |
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%|                    MINIMUM                    |
%|                                               |
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
to_wire(#soa{mname = MName, 
             rname = RName, 
             serial = Serial,
             refresh = Refresh, 
             retry = Retry,
             expire = Expire,
             minimum = Minimum}) ->
    MNameBinary = r9_wire_name:to_wire(MName),
    RNameBinary = r9_wire_name:to_wire(RName), 
    {r9_wire_name:len(MName) + r9_wire_name:len(RName) + 20, 
    <<MNameBinary/binary, 
      RNameBinary/binary, 
      Serial:32/big, 
      Refresh:32/big, 
      Retry:32/big, 
      Expire:32/big, 
      Minimum:32/big>>};

% MX
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%|                  PREFERENCE                   |
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%/                   EXCHANGE                    /
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
to_wire(#mx{preference = Preference, 
                        exchange = Exchange}) ->
    ExchangeBinary = r9_wire_name:to_wire(Exchange),
    {2 + r9_wire_name:len(Exchange), 
     <<Preference:16/little, ExchangeBinary/binary>>};

to_wire(#aaaa{ip = IP}) ->
    IPBinary = r9_util:ipv6_to_wire(IP),
    {16, <<IPBinary:128/bits>>};


%+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
%|                          OPTION-CODE                          |
%+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
%|                         OPTION-LENGTH                         |
%+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
%/                          OPTION-DATA                          /
%+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
to_wire(#opt{code = _Code, len = _Len, data = _Data}) ->
    {0, <<>>};

to_wire(#txt{len = Len, text = Text}) ->
    {Len, <<Text/binary>>};

to_wire(UnknownType) ->
    io:format("unknownn from wire ~p ~n", [UnknownType]),
    throw("unknown type").

to_string(#a{ip = IP}) ->
    r9_util:ipv4_to_string(IP);

to_string(#aaaa{ip = IP}) ->
    r9_util:ipv6_to_string(IP);

to_string(#domain{domain = DomainName}) ->
    r9_wire_name:to_string(DomainName);

to_string(#mx{preference= Preference, exchange = Exchange}) ->
    integer_to_list(Preference) ++ r9_wire_name:to_string(Exchange);

to_string(#soa{mname = MName, 
        rname = RName, 
        serial = Serial, 
        refresh = Refresh,
        retry = Retry,
        expire = Expire,
        minimum = Minimum}) ->
    r9_wire_name:to_string(MName) ++ 
    r9_wire_name:to_string(RName) ++ 
    integer_to_list(Serial) ++ 
    integer_to_list(Refresh) ++ 
    integer_to_list(Retry) ++ 
    integer_to_list(Expire) ++ 
    integer_to_list(Minimum) ;

to_string(#opt{code = Code}) ->
    "[opt]" ++ integer_to_list(Code);

to_string(UnknownType) ->
    io:format("unknownn from to string ~p ~n", [UnknownType]),
    throw("unknown type").

