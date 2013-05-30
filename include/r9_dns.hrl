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

-record(message, { header,
                   question,
                   answer_section,
                   authority_section,
                   additional_section}).

-record(question, {name,
                   type,
                   class}).

-record(rr, {name,   
        type,
        class,
        ttl = 0,
        rdata}). 

-record(rrset, {name,   
        type,
        class,
        ttl = 0,
        rdatas}). 

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

-define(RCODE_NOERROR, 0).
-define(RCODE_FORMERR, 1).
-define(RCODE_SERVFAIL, 2).
-define(RCODE_NXDOMAIN, 3).
-define(RCODE_NOTIMP, 4).
-define(RCODE_REFUSED, 5).
-define(RCODE_YXDOMAIN, 6).
-define(RCODE_YXRRSET, 7).
-define(RCODE_NXRRSET, 8).
-define(RCODE_NOTAUTH, 9).
-define(RCODE_NOTZONE, 10).

-define(OPCODE_STR,[ "QUERY",
                     "INVERSE",
                     "STATUS",
                     "UNKNOWN",
                     "NOTIFY",
                     "UPDATE"]).

-define(OPCODE_QUERY, 0).
-define(OPCODE_INVERSE, 1).
-define(OPCODE_STATUS, 2).
-define(OPCODE_UNKNOWN, 3).
-define(OPCODE_NOTIFY, 4).
-define(OPCODE_UPDATE,  5).





-define(TYPE_A,1).      % host address
-define(TYPE_NS,2).     % Authoritive Name Server 
-define(TYPE_MD,3).     % Mail Destination (Obsolete)
-define(TYPE_MF,4).     % Mail Forwarder (Obsolete)
-define(TYPE_CNAME,5).  % Canonical Name for an Alias
-define(TYPE_SOA,6).    % Start of Authority
-define(TYPE_MB,7).     % Mailbox Domain Name (Experimental)
-define(TYPE_MG,8).     % Mail Group Member (Experimental)
-define(TYPE_MR,9).     % Mail Rename Domain Name (Experimental)
-define(TYPE_NULL,10).  % NULL RR (Experimental)
-define(TYPE_WKS,11).   % Well Known Service Description
-define(TYPE_PTR,12).   % Domain Name Pointer
-define(TYPE_HINFO,13). % Host Information
-define(TYPE_MINFO,14). % mailbox or mail list information
-define(TYPE_MX,15).    % Mail eXchange
-define(TYPE_TXT,16).   % Text Strings
-define(TYPE_AAAA, 16#1c).   % IPV6
-define(TYPE_OPT, 16#29).   % OPT
-define(TYPE_ANY, 16#ff).   % ANY


-define(CLASS_IN, 1).   % Text Strings


-define(QTYPE_AXFR,252).     % Request a zone transfer
-define(QTYPE_MAILB,253).    % Request mailbox-related records (MB, MR or MR)
-define(QTYPE_MAILA,254).    % Request Mail Agent RR (Obsolete)
-define(QTYPE_WILDCARD,255). % Request All Records
-define(TEST, true).


%rdata struct
-record(a, {ip}).
-record(aaaa, {ip}).
-record(domain, {domain}).
-record(mx, {preference,
             exchange}).
-record(soa, {mname,
              rname,
              serial,
              refresh,
              retry,
              expire,
              minimum}).
-record(opt, {code, len, data}).
-record(txt, {len, text}).


% event
-record(dns_event, {origin_request,
                    current_request,
                    response,
                    ip,
                    port}).

-record(delegation_point, {name,
                           v4_ips,
                           v6_ips}).
                           
