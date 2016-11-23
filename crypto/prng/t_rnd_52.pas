{Simple test for Taus113 unit, we Apr.2005}

program t_rnd_52;

{$i STD.INC}

{$ifdef BIT16}
  {$N+}
{$endif}


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

const
  NMAX = 100000000;
var
  n: longint;
  rng: taus113_ctx;
begin
  writeln('Test for Taus113 unit: ', NMAX, ' taus113_next calls     (c) 2005 W.Ehrhardt');
  taus113_init(rng,1);
  for n:=1 to NMAX do begin
    taus113_next(rng);
  end;
  writeln('Done');
end.
