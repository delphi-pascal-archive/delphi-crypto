{Simple test for ISAAC unit, we May 2005}

program t_rnd_30;

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
  isaac;
begin
  writeln('Simple test for ISAAC unit     (c) 2005 W.Ehrhardt');
  writeln('ISAAC selftest: ',isaac_selftest);
end.
