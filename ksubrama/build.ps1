rel\ksubrama\erts-6.2\bin\epmd.exe -kill
.\rebar get-deps compile
dialyzer --src .\src  -Wunmatched_returns -Werror_handling
.\rebar eunit
