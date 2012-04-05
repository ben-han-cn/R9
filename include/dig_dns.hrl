%% dns message header
%%1  1  1  1  1  1
%%0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
%%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%%|                      ID                       |
%%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%%|QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
%%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%%|                    QDCOUNT                    |
%%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%%|                    ANCOUNT                    |
%%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%%|                    NSCOUNT                    |
%%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%%|                    ARCOUNT                    |
%%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%%
-record(message_header, {id, 
                         qr, 
                         opcode, 
                         aa,
                         tc,
                         rd,
                         ra,
                         ad,
                         cd,
                         rcode,
                         question_sec_count,
                         answer_sec_count,
                         authority_sec_count,
                         additional_sec_count
                         }).

-define(RCODE_STR, ["NOERROR",
                    "FORMERR",
                    "SERVFAIL",
                    "NXDOMAIN",
                    "NOTIMP",
                    "REFUSED",
                    "YXDOMAIN",
                    "YXRRSET",
                    "NXRRSET",
                    "NOTAUTH",
                    "NOTZONE"]).


-define(OPCODE_STR,[ "QUERY",
                     "INVERSE",
                     "STATUS",
                     "UNKNOWN",
                     "NOTIFY",
                     "UPDATE"]).


