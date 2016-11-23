{Simple test for Pascal/Delphi random, we May 2005}

program t_rnd_00;

{$i STD.INC}

{$ifdef BIT16}
  {$N+}
{$endif}


{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

{$ifdef WINCRT}
uses wincrt;
{$endif}

const
  NMAX = 100000000;
var
  n: longint;
  w: word;
begin
  writeln('Test for random: ', NMAX, ' random(1) calls     (c) 2005 W.Ehrhardt');
  for n:=1 to NMAX do begin
    w := random(1);
  end;
  writeln('Done');
end.
