{Simple test for Pascal/Delphi random, we May 2005}

program t_rnd_53;

{$i STD.INC}

{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

{$ifndef FPC}
  {$N+}
{$endif}

{$ifdef BIT16}
  {$define USEDUM}
{$endif}

{$ifdef VirtualPascal}
  {$define USEDUM}
{$endif}

{$ifdef X_OPT}
  {$x+}
{$endif}

uses
  {$ifdef WINCRT}
    wincrt,
  {$endif}
  hrtimer;

const
  LOOPS  = 10;


{---------------------------------------------------------------------------}
function GenerateCycles: longint;
var
  i: integer;
  cyc0, cyc1, cyc2: comp;
  t1,t2,c1,c2: longint;
{$ifdef USEDUM}
  w: integer;
{$endif}
begin
  c1 := MaxLongint;
  c2 := MaxLongint;
  for i:=1 to LOOPS do begin
    ReadTSC(cyc0);
    {$ifdef USEDUM} w := {$endif} random(1);
    ReadTSC(cyc1);
    {$ifdef USEDUM} w := {$endif} random(1);
    {$ifdef USEDUM} w := {$endif} random(1);
    {$ifdef USEDUM} w := {$endif} random(1);
    {$ifdef USEDUM} w := {$endif} random(1);
    {$ifdef USEDUM} w := {$endif} random(1);
    ReadTSC(cyc2);
    t2 := round(cyc2-cyc1);
    t1 := round(cyc1-cyc0);
    if t1<c1 then c1 := t1;
    if t2<c2 then c2 := t2;
  end;
  GenerateCycles := (c2-c1+1) shr 2;
end;


var
  CBlk: longint;
  CPB : double;
begin
  writeln('Test for Pascal/Delphi random  (c) 2005 W.Ehrhardt');
  CBlK := GenerateCycles;
  CPB  := CBlk/4.0;
  writeln('   CPU Frequency: ', CPUFrequency/1E6:1:1);
  writeln('        Generate: ', CBlk);
  writeln('     Cycles/Byte: ', CPB:1:1);
  writeln('            MB/s: ', CPUFrequency/CPB/1E6:1:3);
end.
