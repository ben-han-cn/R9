-record(host, {ip,
               port}).

-record(request, {id,
                  question,
                  client}).

-record(response, {question,
                   reply_message}).
