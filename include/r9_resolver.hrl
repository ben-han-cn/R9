-record(host, {ip,
               port}).

-record(request, {id,
                  question,
                  client,
                  socket}).

-record(response, {question,
                   reply_message}).
