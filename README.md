## request scoped variables

`rscope` uses `seq_trace` for global variables scoped within the
execution of a single request.

At this point you might say: "Oh, the horror! What kind of twisted use
case could there be for this?" It's very easy, you can set a variable
early in the execution of the request (for example "identity" can be
set during the HTTP handling) and later used when collecting metrics
or logging.

It can work something like this:

1. In your HTTP handler, call request:identity/1 to identify the
   request as "fetch document", "update invoice", etc.

2. In your business logic in another process, call rscope:identity/0
   to retrieve the identity and use it for collecting metrics or
   logging.

3. When the request is complete, ie. you are back in the HTTP handler
   returning the response, call rscope:request_complete/0 to clean up
   the cached request identity.
