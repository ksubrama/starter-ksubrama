ksubrama\rel\ksubrama\bin\ksubrama console
ksubrama\rel\ksubrama\erts-6.2\bin\epmd.exe -kill
dialyzer --build_plt --apps erts kernel stdlib crypto mnesia sasl common_test eunit ssl reltool -r .\deps\ranch\ebin .\deps\cowboy\ebin .\deps\cowlib\ebin .\ebin
dialyzer --src .\src  -Wunmatched_returns -Werror_handling
