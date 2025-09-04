# stubby

A stub http server and request body recorder.

## Build

    rebar3 compile

## How to use

### Configuration

In rebar.config deps, have:

    {stubby, {git, "git://github.com/yowcow/stubby", {branch, "master}}}

### Testing

In testing, e.g., common tests, have:

```erlang
init_per_suite(Config) ->
    Url = stubby:start(),
    [{url, Url} | Config].

end_per_suite(_) ->
    ok = stubby:stop().
```

See src/stubby.erl for more options starting up a server.

In a testcase, make a request to stubby URL, then get the most recent request to the **specified** path with:

```erlang
{ok, #{
  headers := Headers,
  scheme := Scheme,
  host := Host,
  port := Port,
  path := Path,
  qs := QueryString,
  body := Body
 }} = stubby:get_recent("/path/to/endopoint")
```

When no request is recorded yet, this call blocks until the first request is made.

## Endpoints

By default, a booted stubby serves:

- `/`: always responds with status code 200
- `/blackhole/[...]`: always responds with status code 204

Additional cowboy endpoints can be added as a start option:

```erlang
stubby:start([
   {"/new/endpoint1", new_endpoint1_handler, []},
   {"/new/endpoint2", new_endpoint2_handler, []}
  ])
```

## See also

- <https://github.com/ninenines/cowboy>
- <https://erlang.org/doc/apps/common_test/introduction.html>
- <https://erlang.org/doc/man/common_test.html>
- <https://www.rebar3.org/docs/running-tests#section-common-test>
