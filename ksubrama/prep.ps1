.\build
dialyzer --build_plt --apps erts kernel stdlib crypto mnesia sasl common_test eunit ssl reltool -r .\deps\ej\ebin .\deps\jsx\ebin .\deps\ranch\ebin .\deps\cowboy\ebin .\deps\cowlib\ebin .\ebin
