{demo program from mp_intro, with Delphi use -cc command line switch}
uses
  mp_types, mp_base, mp_numth;
var
  a: mp_int;
begin
  mp_init(a);
  mp_fib(271,a);
  writeln('Fibonacci(271) = ',mp_decimal(a));
  mp_2expt(a,479999);
  writeln('Number of decimal digits of 2^479999 = ', mp_radix_size(a,10));
  mp_clear(a);
end.
