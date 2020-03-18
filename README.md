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

In a testcase, make a request to stubby url, then get the most recent request body with:

```erlang
{ok, Body} = stubby:get_recent()
```

If the request is expected to arrive after a delay, retry (times) with interval (ms) between each try:

```erlang
{ok, Body} = stubby:get_recent([{retry, 5}, {interval, 100}])
```

See also
--------

* https://github.com/ninenines/cowboy
* https://erlang.org/doc/apps/common_test/introduction.html
* https://erlang.org/doc/man/common_test.html
* https://www.rebar3.org/docs/running-tests#section-common-test
