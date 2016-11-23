{Simple test for Taus113 unit, we Aug.2005}

program t_rnd_50;

{$i STD.INC}

{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  taus113;

begin
  writeln('Simple test for Taus113 unit     (c) 2005 W.Ehrhardt');
  writeln('PRNG selftest: ', taus113_selftest);
end.
