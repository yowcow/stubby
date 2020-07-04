[![Build Status](https://travis-ci.com/yowcow/stubby.svg?branch=master)](https://travis-ci.com/yowcow/stubby)

stubby
======

A stub http server and request body recorder.


Build
-----

    $ rebar3 compile


How to use
----------

In rebar.config deps, have:

    {stubby, {git, "git://github.com/yowcow/stubby", {branch, "master}}}

In testing, e.g., common tests, have:

```erlang
init_per_suite(Config) ->
    Url = stubby:start(),
    [{url, Url} | Config].

end_per_suite(_) ->
    ok = stubby:stop().
```

(See src/stubby.erl for more options starting up a server)

By default, a booted stubby serves:

* `/`: always respond with status code 200
* `/blackhole/[...]`: always respond with status code 204

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


See also
--------

* https://github.com/ninenines/cowboy
* https://erlang.org/doc/apps/common_test/introduction.html
* https://erlang.org/doc/man/common_test.html
* https://www.rebar3.org/docs/running-tests#section-common-test
