{Simple test for MT19937 unit, we May 2005}

program t_rnd_40;

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
  mt19937;
begin
  writeln('Simple test for MT19937 unit     (c) 2005 W.Ehrhardt');
  writeln('MT19937 selftest: ',mt19937_selftest);
end.
