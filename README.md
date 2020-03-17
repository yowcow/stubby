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
    Url = stubby:start([]),
    [{url, Url} | Config].

end_per_suite(_) ->
    ok = stubby:stop().
```

In a testcase, make a request to stubby url, then get the most recent request body with:

```erlang
{ok, Body} = stubby:get_recent()
```

See also
--------

* https://erlang.org/doc/apps/common_test/introduction.html
* https://erlang.org/doc/man/common_test.html
* https://www.rebar3.org/docs/running-tests#section-common-test
