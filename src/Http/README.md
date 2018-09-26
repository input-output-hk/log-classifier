# Http module

This contains all the code relating to the _HTTP communication_.

## Queue

The `Queue` module might be a bit obscure, so let us break it down here.
We need a mechanism for limiting the number of _HTTP requests_. See more details
about it [here](https://developer.zendesk.com/rest_api/docs/core/introduction#rate-limits).

![zendek rate limit queue](https://user-images.githubusercontent.com/6264437/45674498-8ab76f80-bb2d-11e8-9712-0fd84b4cc539.png)

We want to have _HTTP requests_ that block if we exceed the number of requests in
a single minute. That allows us to create very simple and composable functions that
we can reason about.

![zendek rate limit](https://user-images.githubusercontent.com/6264437/45674544-a15dc680-bb2d-11e8-8723-cefc9ac78d9e.png)

We have an external thread that resets the number of available _HTTP requests_ every
minute, which allows us to use the rate limiting in a quite simple manner.

Also, if we ever reach a point where we go over the limit, which could happen if someone 
is accessing the Zendesk API at the same time as we are, we get a *HttpTooManyRequests* exception.
That exception is a signal to delay the request for another minute which allows it to be replayed
in the next rate limit request iteration.

