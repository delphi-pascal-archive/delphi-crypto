{Simple test for TT800 unit, we May 2005}

program t_rnd_20;

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
  tt800;

begin
  writeln('Simple test for TT800 unit     (c) 2005 W.Ehrhardt');
  writeln('TT800 selftest: ',tt800_selftest);
end.
