{******************************************************************************}
{*  RipeMD.imp.inc                                                            *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
function f128 (x, y, z: LongWord) : LongWord;
begin
    Result := x xor y xor z;
end;

function g128 (x, y, z: LongWord) : LongWord;
begin
    Result := (x and y) or ( (not x) and z );
end;

function h128 (x, y, z: LongWord) : LongWord;
begin
    Result := ( x or (not y) ) xor z;
end;

function i128 (x, y, z: LongWord) : LongWord;
begin
    Result := (x and z) or ( y and (not z) );
end;

function j128 (x, y, z: LongWord) : LongWord;
begin
    Result := x xor ( y or (not z) );
end;

procedure ff128 (var a: LongWord; b, c, d, x, s: LongWord);
begin
    a := a + ( f128 (b, c, d) + x );
    a := rol (a, s);
end;

procedure gg128 (var a: LongWord; b, c, d, x, s: LongWord);
begin
    a := a + ( g128 (b, c, d) + x + $5a827999 );
    a := rol (a, s);
end;

procedure hh128 (var a: LongWord; b, c, d, x, s: LongWord);
begin
    a := a + ( h128 (b, c, d) + x + $6ed9eba1 );
    a := rol (a, s);
end;

procedure ii128 (var a: LongWord; b, c, d, x, s: LongWord);
begin
    a := a + ( i128 (b, c, d) + x + $8f1bbcdc );
    a := rol (a, s);
end;

procedure fff128 (var a: LongWord; b, c, d, x, s: LongWord);
begin
    a := a + ( f128 (b, c, d) + x );
    a := rol (a, s);
end;

procedure ggg128 (var a: LongWord; b, c, d, x, s: LongWord);
begin
    a := a + ( g128 (b, c, d) + x + $6d703ef3 );
    a := rol (a, s);
end;

procedure hhh128 (var a: LongWord; b, c, d, x, s: LongWord);
begin
    a := a + ( h128 (b, c, d) + x + $5c4dd124 );
    a := rol (a, s);
end;

procedure iii128 (var a: LongWord; b, c, d, x, s: LongWord);
begin
    a := a + ( i128 (b, c, d) + x + $50a28be6 );
    a := rol (a, s);
end;

procedure FF160 (var a: LongWord; b: LongWord; var c: LongWord; d, e, x, s: LongWord);
begin
    a := a + ( f128 (b, c, d) + x );
    a := rol (a, s) + e;
    c := rol (c, 10);
end;

procedure gg160 (var a: LongWord; b: LongWord; var c: LongWord; d, e, x, s: LongWord);
begin
    a := a + ( g128 (b, c, d) + x + $5a827999 );
    a := rol (a, s) + e;
    c := rol (c, 10);
end;

procedure hh160 (var a: LongWord; b: LongWord; var c: LongWord; d, e, x, s: LongWord);
begin
    a := a + ( h128 (b, c, d) + x + $6ed9eba1 );
    a := rol (a, s) + e;
    c := rol (c, 10);
end;

procedure ii160 (var a: LongWord; b: LongWord; var c: LongWord; d, e, x, s: LongWord);
begin
    a := a + ( i128 (b, c, d) + x + $8f1bbcdc );
    a := rol (a, s) + e;
    c := rol (c, 10);
end;

procedure jj160 (var a: LongWord; b: LongWord; var c: LongWord; d, e, x, s: LongWord);
begin
    a :=  a + ( j128 (b, c, d) + x + $a953fd4e );
    a := rol (a, s) + e;
    c := rol (c, 10);
end;

procedure fff160 (var a: LongWord; b: LongWord; var c: LongWord; d, e, x, s: LongWord);
begin
    a := a + ( f128 (b, c, d) + x );
    a := rol (a, s) + e;
    c := rol (c, 10);
end;

procedure ggg160(var a: LongWord; b: LongWord; var c: LongWord; d, e, x, s: LongWord);
begin
    a := a + ( g128 (b, c, d) + x + $7a6d76e9 );
    a := rol (a, s) + e;
    c := rol (c, 10);
end;

procedure hhh160(var a: LongWord; b: LongWord; var c: LongWord; d, e, x, s: LongWord);
begin
    a := a + ( h128 (b, c, d) + x + $6d703ef3 );
    a := rol (a, s) + e;
    c := rol (c, 10);
end;

procedure iii160 (var a: LongWord; b: LongWord; var c: LongWord; d, e, x, s: LongWord);
begin
    a := a + ( i128 (b, c, d) + x + $5c4dd124 );
    a := rol(a, s) + e;
    c := rol(c, 10);
end;

procedure jjj160 (var a: LongWord; b: LongWord; var c: LongWord; d, e, x, s: LongWord);
begin
    a := a + ( j128 (b, c, d) + x + $50a28be6 );
    a := rol (a, s) + e;
    c := rol (c, 10);
end;

procedure MDInit128 (var MDBuffer: array of LongWord);
begin
    MDBuffer [0] := $67452301;
    MDBuffer [1] := $efcdab89;
    MDBuffer [2] := $98badcfe;
    MDBuffer [3] := $10325476;
end;

procedure MDInit160 (var MDBuffer: array of LongWord);
begin
    MDInit128 (MDBuffer);
    MDBuffer [4] := $c3d2e1f0;
end;

procedure RMD128Init (var ctx: TRMDCtx);
begin
    ctx.Length := 0;
    ctx.CurrentLength := 0;
    MDInit128 (ctx.MDBuffer);
end;

procedure RMD160Init (var ctx: TRMDCtx);
begin
    ctx.Length := 0;
    ctx.CurrentLength := 0;
    MDInit160 (ctx.MDBuffer);
end;

procedure Compress128 (var MDBuffer: array of LongWord; x: array of LongWord);
var
    aa  : LongWord;
    bb  : LongWord;
    cc  : LongWord;
    dd  : LongWord;

    aaa : LongWord;
    bbb : LongWord;
    ccc : LongWord;
    ddd : LongWord;
begin
    aa := MDBuffer [0];
    bb := MDBuffer [1];
    cc := MDBuffer [2];
    dd := MDBuffer [3];

    aaa := MDBuffer [0];
    bbb := MDBuffer [1];
    ccc := MDBuffer [2];
    ddd := MDBuffer [3];

    { round 0 }
    ff128 ( aa, bb, cc, dd, x [ 0], 11 );
    ff128 ( dd, aa, bb, cc, x [ 1], 14 );
    ff128 ( cc, dd, aa, bb, x [ 2], 15 );
    ff128 ( bb, cc, dd, aa, x [ 3], 12 );
    ff128 ( aa, bb, cc, dd, x [ 4],  5 );
    ff128 ( dd, aa, bb, cc, x [ 5],  8 );
    ff128 ( cc, dd, aa, bb, x [ 6],  7 );
    ff128 ( bb, cc, dd, aa, x [ 7],  9 );
    ff128 ( aa, bb, cc, dd, x [ 8], 11 );
    ff128 ( dd, aa, bb, cc, x [ 9], 13 );
    ff128 ( cc, dd, aa, bb, x [10], 14 );
    ff128 ( bb, cc, dd, aa, x [11], 15 );
    ff128 ( aa, bb, cc, dd, x [12],  6 );
    ff128 ( dd, aa, bb, cc, x [13],  7 );
    ff128 ( cc, dd, aa, bb, x [14],  9 );
    ff128 ( bb, cc, dd, aa, x [15],  8 );
    { round 1 }
    gg128 ( aa, bb, cc, dd, x [ 7],  7 );
    gg128 ( dd, aa, bb, cc, x [ 4],  6 );
    gg128 ( cc, dd, aa, bb, x [13],  8 );
    gg128 ( bb, cc, dd, aa, x [ 1], 13 );
    gg128 ( aa, bb, cc, dd, x [10], 11 );
    gg128 ( dd, aa, bb, cc, x [ 6],  9 );
    gg128 ( cc, dd, aa, bb, x [15],  7 );
    gg128 ( bb, cc, dd, aa, x [ 3], 15 );
    gg128 ( aa, bb, cc, dd, x [12],  7 );
    gg128 ( dd, aa, bb, cc, x [ 0], 12 );
    gg128 ( cc, dd, aa, bb, x [ 9], 15 );
    gg128 ( bb, cc, dd, aa, x [ 5],  9 );
    gg128 ( aa, bb, cc, dd, x [ 2], 11 );
    gg128 ( dd, aa, bb, cc, x [14],  7 );
    gg128 ( cc, dd, aa, bb, x [11], 13 );
    gg128 ( bb, cc, dd, aa, x [ 8], 12 );
    { round 2 }
    hh128 ( aa, bb, cc, dd, x [ 3], 11 );
    hh128 ( dd, aa, bb, cc, x [10], 13 );
    hh128 ( cc, dd, aa, bb, x [14],  6 );
    hh128 ( bb, cc, dd, aa, x [ 4],  7 );
    hh128 ( aa, bb, cc, dd, x [ 9], 14 );
    hh128 ( dd, aa, bb, cc, x [15],  9 );
    hh128 ( cc, dd, aa, bb, x [ 8], 13 );
    hh128 ( bb, cc, dd, aa, x [ 1], 15 );
    hh128 ( aa, bb, cc, dd, x [ 2], 14 );
    hh128 ( dd, aa, bb, cc, x [ 7],  8 );
    hh128 ( cc, dd, aa, bb, x [ 0], 13 );
    hh128 ( bb, cc, dd, aa, x [ 6],  6 );
    hh128 ( aa, bb, cc, dd, x [13],  5 );
    hh128 ( dd, aa, bb, cc, x [11], 12 );
    hh128 ( cc, dd, aa, bb, x [ 5],  7 );
    hh128 ( bb, cc, dd, aa, x [12],  5 );
    { round 3 }
    ii128 ( aa, bb, cc, dd, x [ 1], 11 );
    ii128 ( dd, aa, bb, cc, x [ 9], 12 );
    ii128 ( cc, dd, aa, bb, x [11], 14 );
    ii128 ( bb, cc, dd, aa, x [10], 15 );
    ii128 ( aa, bb, cc, dd, x [ 0], 14 );
    ii128 ( dd, aa, bb, cc, x [ 8], 15 );
    ii128 ( cc, dd, aa, bb, x [12],  9 );
    ii128 ( bb, cc, dd, aa, x [ 4],  8 );
    ii128 ( aa, bb, cc, dd, x [13],  9 );
    ii128 ( dd, aa, bb, cc, x [ 3], 14 );
    ii128 ( cc, dd, aa, bb, x [ 7],  5 );
    ii128 ( bb, cc, dd, aa, x [15],  6 );
    ii128 ( aa, bb, cc, dd, x [14],  8 );
    ii128 ( dd, aa, bb, cc, x [ 5],  6 );
    ii128 ( cc, dd, aa, bb, x [ 6],  5 );
    ii128 ( bb, cc, dd, aa, x [ 2], 12 );
    { parallel round 0 }
    iii128 ( aaa, bbb, ccc, ddd, x [ 5],  8 );
    iii128 ( ddd, aaa, bbb, ccc, x [14],  9 );
    iii128 ( ccc, ddd, aaa, bbb, x [ 7],  9 );
    iii128 ( bbb, ccc, ddd, aaa, x [ 0], 11 );
    iii128 ( aaa, bbb, ccc, ddd, x [ 9], 13 );
    iii128 ( ddd, aaa, bbb, ccc, x [ 2], 15 );
    iii128 ( ccc, ddd, aaa, bbb, x [11], 15 );
    iii128 ( bbb, ccc, ddd, aaa, x [ 4],  5 );
    iii128 ( aaa, bbb, ccc, ddd, x [13],  7 );
    iii128 ( ddd, aaa, bbb, ccc, x [ 6],  7 );
    iii128 ( ccc, ddd, aaa, bbb, x [15],  8 );
    iii128 ( bbb, ccc, ddd, aaa, x [ 8], 11 );
    iii128 ( aaa, bbb, ccc, ddd, x [ 1], 14 );
    iii128 ( ddd, aaa, bbb, ccc, x [10], 14 );
    iii128 ( ccc, ddd, aaa, bbb, x [ 3], 12 );
    iii128 ( bbb, ccc, ddd, aaa, x [12],  6 );
    { parallel round 1 }
    hhh128 ( aaa, bbb, ccc, ddd, x [ 6],  9 );
    hhh128 ( ddd, aaa, bbb, ccc, x [11], 13 );
    hhh128 ( ccc, ddd, aaa, bbb, x [ 3], 15 );
    hhh128 ( bbb, ccc, ddd, aaa, x [ 7],  7 );
    hhh128 ( aaa, bbb, ccc, ddd, x [ 0], 12 );
    hhh128 ( ddd, aaa, bbb, ccc, x [13],  8 );
    hhh128 ( ccc, ddd, aaa, bbb, x [ 5],  9 );
    hhh128 ( bbb, ccc, ddd, aaa, x [10], 11 );
    hhh128 ( aaa, bbb, ccc, ddd, x [14],  7 );
    hhh128 ( ddd, aaa, bbb, ccc, x [15],  7 );
    hhh128 ( ccc, ddd, aaa, bbb, x [ 8], 12 );
    hhh128 ( bbb, ccc, ddd, aaa, x [12],  7 );
    hhh128 ( aaa, bbb, ccc, ddd, x [ 4],  6 );
    hhh128 ( ddd, aaa, bbb, ccc, x [ 9], 15 );
    hhh128 ( ccc, ddd, aaa, bbb, x [ 1], 13 );
    hhh128 ( bbb, ccc, ddd, aaa, x [ 2], 11 );
    { parallel round 2 }
    ggg128 ( aaa, bbb, ccc, ddd, x [15],  9 );
    ggg128 ( ddd, aaa, bbb, ccc, x [ 5],  7 );
    ggg128 ( ccc, ddd, aaa, bbb, x [ 1], 15 );
    ggg128 ( bbb, ccc, ddd, aaa, x [ 3], 11 );
    ggg128 ( aaa, bbb, ccc, ddd, x [ 7],  8 );
    ggg128 ( ddd, aaa, bbb, ccc, x [14],  6 );
    ggg128 ( ccc, ddd, aaa, bbb, x [ 6],  6 );
    ggg128 ( bbb, ccc, ddd, aaa, x [ 9], 14 );
    ggg128 ( aaa, bbb, ccc, ddd, x [11], 12 );
    ggg128 ( ddd, aaa, bbb, ccc, x [ 8], 13 );
    ggg128 ( ccc, ddd, aaa, bbb, x [12],  5 );
    ggg128 ( bbb, ccc, ddd, aaa, x [ 2], 14 );
    ggg128 ( aaa, bbb, ccc, ddd, x [10], 13 );
    ggg128 ( ddd, aaa, bbb, ccc, x [ 0], 13 );
    ggg128 ( ccc, ddd, aaa, bbb, x [ 4],  7 );
    ggg128 ( bbb, ccc, ddd, aaa, x [13],  5 );
    { parallel round 3 }
    fff128 ( aaa, bbb, ccc, ddd, x [ 8], 15 );
    fff128 ( ddd, aaa, bbb, ccc, x [ 6],  5 );
    fff128 ( ccc, ddd, aaa, bbb, x [ 4],  8 );
    fff128 ( bbb, ccc, ddd, aaa, x [ 1], 11 );
    fff128 ( aaa, bbb, ccc, ddd, x [ 3], 14 );
    fff128 ( ddd, aaa, bbb, ccc, x [11], 14 );
    fff128 ( ccc, ddd, aaa, bbb, x [15],  6 );
    fff128 ( bbb, ccc, ddd, aaa, x [ 0], 14 );
    fff128 ( aaa, bbb, ccc, ddd, x [ 5],  6 );
    fff128 ( ddd, aaa, bbb, ccc, x [12],  9 );
    fff128 ( ccc, ddd, aaa, bbb, x [ 2], 12 );
    fff128 ( bbb, ccc, ddd, aaa, x [13],  9 );
    fff128 ( aaa, bbb, ccc, ddd, x [ 9], 12 );
    fff128 ( ddd, aaa, bbb, ccc, x [ 7],  5 );
    fff128 ( ccc, ddd, aaa, bbb, x [10], 15 );
    fff128 ( bbb, ccc, ddd, aaa, x [14],  8 );

    ddd := ddd + cc + MDBuffer [1];             
    MDBuffer [1] := MDBuffer [2] + dd + aaa;
    MDBuffer [2] := MDBuffer [3] + aa + bbb;
    MDBuffer [3] := MDBuffer [0] + bb + ccc;
    MDBuffer [0] := ddd;
end;

procedure Compress160 (var MDBuffer: array of LongWord; x: array of LongWord);
var
    aa  : LongWord;
    bb  : LongWord;
    cc  : LongWord;
    dd  : LongWord;
    ee  : LongWord;

    aaa : LongWord;
    bbb : LongWord;
    ccc : LongWord;
    ddd : LongWord;
    eee : LongWord;
begin
    aa := MDBuffer [0];
    bb := MDBuffer [1];
    cc := MDBuffer [2];
    dd := MDBuffer [3];
    ee := MDBuffer [4];

    aaa := MDBuffer [0];
    bbb := MDBuffer [1];
    ccc := MDBuffer [2];
    ddd := MDBuffer [3];
    eee := MDBuffer [4];

    { round 1 }
    ff160 ( aa, bb, cc, dd, ee, x [ 0], 11 );
    ff160 ( ee, aa, bb, cc, dd, x [ 1], 14 );
    ff160 ( dd, ee, aa, bb, cc, x [ 2], 15 );
    ff160 ( cc, dd, ee, aa, bb, x [ 3], 12 );
    ff160 ( bb, cc, dd, ee, aa, x [ 4],  5 );
    ff160 ( aa, bb, cc, dd, ee, x [ 5],  8 );
    ff160 ( ee, aa, bb, cc, dd, x [ 6],  7 );
    ff160 ( dd, ee, aa, bb, cc, x [ 7],  9 );
    ff160 ( cc, dd, ee, aa, bb, x [ 8], 11 );
    ff160 ( bb, cc, dd, ee, aa, x [ 9], 13 );
    ff160 ( aa, bb, cc, dd, ee, x [10], 14 );
    ff160 ( ee, aa, bb, cc, dd, x [11], 15 );
    ff160 ( dd, ee, aa, bb, cc, x [12],  6 );
    ff160 ( cc, dd, ee, aa, bb, x [13],  7 );
    ff160 ( bb, cc, dd, ee, aa, x [14],  9 );
    ff160 ( aa, bb, cc, dd, ee, x [15],  8 );
    { round 2 }
    gg160 ( ee, aa, bb, cc, dd, x [ 7],  7 );
    gg160 ( dd, ee, aa, bb, cc, x [ 4],  6 );
    gg160 ( cc, dd, ee, aa, bb, x [13],  8 );
    gg160 ( bb, cc, dd, ee, aa, x [ 1], 13 );
    gg160 ( aa, bb, cc, dd, ee, x [10], 11 );
    gg160 ( ee, aa, bb, cc, dd, x [ 6],  9 );
    gg160 ( dd, ee, aa, bb, cc, x [15],  7 );
    gg160 ( cc, dd, ee, aa, bb, x [ 3], 15 );
    gg160 ( bb, cc, dd, ee, aa, x [12],  7 );
    gg160 ( aa, bb, cc, dd, ee, x [ 0], 12 );
    gg160 ( ee, aa, bb, cc, dd, x [ 9], 15 );
    gg160 ( dd, ee, aa, bb, cc, x [ 5],  9 );
    gg160 ( cc, dd, ee, aa, bb, x [ 2], 11 );
    gg160 ( bb, cc, dd, ee, aa, x [14],  7 );
    gg160 ( aa, bb, cc, dd, ee, x [11], 13 );
    gg160 ( ee, aa, bb, cc, dd, x [ 8], 12 );
    { round 3 }
    hh160 ( dd, ee, aa, bb, cc, x [ 3], 11 );
    hh160 ( cc, dd, ee, aa, bb, x [10], 13 );
    hh160 ( bb, cc, dd, ee, aa, x [14],  6 );
    hh160 ( aa, bb, cc, dd, ee, x [ 4],  7 );
    hh160 ( ee, aa, bb, cc, dd, x [ 9], 14 );
    hh160 ( dd, ee, aa, bb, cc, x [15],  9 );
    hh160 ( cc, dd, ee, aa, bb, x [ 8], 13 );
    hh160 ( bb, cc, dd, ee, aa, x [ 1], 15 );
    hh160 ( aa, bb, cc, dd, ee, x [ 2], 14 );
    hh160 ( ee, aa, bb, cc, dd, x [ 7],  8 );
    hh160 ( dd, ee, aa, bb, cc, x [ 0], 13 );
    hh160 ( cc, dd, ee, aa, bb, x [ 6],  6 );
    hh160 ( bb, cc, dd, ee, aa, x [13],  5 );
    hh160 ( aa, bb, cc, dd, ee, x [11], 12 );
    hh160 ( ee, aa, bb, cc, dd, x [ 5],  7 );
    hh160 ( dd, ee, aa, bb, cc, x [12],  5 );
    { round 4 }
    ii160 ( cc, dd, ee, aa, bb, x [ 1], 11 );
    ii160 ( bb, cc, dd, ee, aa, x [ 9], 12 );
    ii160 ( aa, bb, cc, dd, ee, x [11], 14 );
    ii160 ( ee, aa, bb, cc, dd, x [10], 15 );
    ii160 ( dd, ee, aa, bb, cc, x [ 0], 14 );
    ii160 ( cc, dd, ee, aa, bb, x [ 8], 15 );
    ii160 ( bb, cc, dd, ee, aa, x [12],  9 );
    ii160 ( aa, bb, cc, dd, ee, x [ 4],  8 );
    ii160 ( ee, aa, bb, cc, dd, x [13],  9 );
    ii160 ( dd, ee, aa, bb, cc, x [ 3], 14 );
    ii160 ( cc, dd, ee, aa, bb, x [ 7],  5 );
    ii160 ( bb, cc, dd, ee, aa, x [15],  6 );
    ii160 ( aa, bb, cc, dd, ee, x [14],  8 );
    ii160 ( ee, aa, bb, cc, dd, x [ 5],  6 );
    ii160 ( dd, ee, aa, bb, cc, x [ 6],  5 );
    ii160 ( cc, dd, ee, aa, bb, x [ 2], 12 );
    { round 5 }
    jj160 ( bb, cc, dd, ee, aa, x [ 4],  9 );
    jj160 ( aa, bb, cc, dd, ee, x [ 0], 15 );
    jj160 ( ee, aa, bb, cc, dd, x [ 5],  5 );
    jj160 ( dd, ee, aa, bb, cc, x [ 9], 11 );
    jj160 ( cc, dd, ee, aa, bb, x [ 7],  6 );
    jj160 ( bb, cc, dd, ee, aa, x [12],  8 );
    jj160 ( aa, bb, cc, dd, ee, x [ 2], 13 );
    jj160 ( ee, aa, bb, cc, dd, x [10], 12 );
    jj160 ( dd, ee, aa, bb, cc, x [14],  5 );
    jj160 ( cc, dd, ee, aa, bb, x [ 1], 12 );
    jj160 ( bb, cc, dd, ee, aa, x [ 3], 13 );
    jj160 ( aa, bb, cc, dd, ee, x [ 8], 14 );
    jj160 ( ee, aa, bb, cc, dd, x [11], 11 );
    jj160 ( dd, ee, aa, bb, cc, x [ 6],  8 );
    jj160 ( cc, dd, ee, aa, bb, x [15],  5 );
    jj160 ( bb, cc, dd, ee, aa, x [13],  6 );
    { parallel round 0 }
    jjj160 ( aaa, bbb, ccc, ddd, eee, x [ 5],  8 );
    jjj160 ( eee, aaa, bbb, ccc, ddd, x [14],  9 );
    jjj160 ( ddd, eee, aaa, bbb, ccc, x [ 7],  9 );
    jjj160 ( ccc, ddd, eee, aaa, bbb, x [ 0], 11 );
    jjj160 ( bbb, ccc, ddd, eee, aaa, x [ 9], 13 );
    jjj160 ( aaa, bbb, ccc, ddd, eee, x [ 2], 15 );
    jjj160 ( eee, aaa, bbb, ccc, ddd, x [11], 15 );
    jjj160 ( ddd, eee, aaa, bbb, ccc, x [ 4],  5 );
    jjj160 ( ccc, ddd, eee, aaa, bbb, x [13],  7 );
    jjj160 ( bbb, ccc, ddd, eee, aaa, x [ 6],  7 );
    jjj160 ( aaa, bbb, ccc, ddd, eee, x [15],  8 );
    jjj160 ( eee, aaa, bbb, ccc, ddd, x [ 8], 11 );
    jjj160 ( ddd, eee, aaa, bbb, ccc, x [ 1], 14 );
    jjj160 ( ccc, ddd, eee, aaa, bbb, x [10], 14 );
    jjj160 ( bbb, ccc, ddd, eee, aaa, x [ 3], 12 );
    jjj160 ( aaa, bbb, ccc, ddd, eee, x [12],  6 );
    { parallel round 1 }
    iii160 ( eee, aaa, bbb, ccc, ddd, x [ 6],  9 );
    iii160 ( ddd, eee, aaa, bbb, ccc, x [11], 13 );
    iii160 ( ccc, ddd, eee, aaa, bbb, x [ 3], 15 );
    iii160 ( bbb, ccc, ddd, eee, aaa, x [ 7],  7 );
    iii160 ( aaa, bbb, ccc, ddd, eee, x [ 0], 12 );
    iii160 ( eee, aaa, bbb, ccc, ddd, x [13],  8 );
    iii160 ( ddd, eee, aaa, bbb, ccc, x [ 5],  9 );
    iii160 ( ccc, ddd, eee, aaa, bbb, x [10], 11 );
    iii160 ( bbb, ccc, ddd, eee, aaa, x [14],  7 );
    iii160 ( aaa, bbb, ccc, ddd, eee, x [15],  7 );
    iii160 ( eee, aaa, bbb, ccc, ddd, x [ 8], 12 );
    iii160 ( ddd, eee, aaa, bbb, ccc, x [12],  7 );
    iii160 ( ccc, ddd, eee, aaa, bbb, x [ 4],  6 );
    iii160 ( bbb, ccc, ddd, eee, aaa, x [ 9], 15 );
    iii160 ( aaa, bbb, ccc, ddd, eee, x [ 1], 13 );
    iii160 ( eee, aaa, bbb, ccc, ddd, x [ 2], 11 );
    { parallel round 2 }
    hhh160 ( ddd, eee, aaa, bbb, ccc, x [15],  9 );
    hhh160 ( ccc, ddd, eee, aaa, bbb, x [ 5],  7 );
    hhh160 ( bbb, ccc, ddd, eee, aaa, x [ 1], 15 );
    hhh160 ( aaa, bbb, ccc, ddd, eee, x [ 3], 11 );
    hhh160 ( eee, aaa, bbb, ccc, ddd, x [ 7],  8 );
    hhh160 ( ddd, eee, aaa, bbb, ccc, x [14],  6 );
    hhh160 ( ccc, ddd, eee, aaa, bbb, x [ 6],  6 );
    hhh160 ( bbb, ccc, ddd, eee, aaa, x [ 9], 14 );
    hhh160 ( aaa, bbb, ccc, ddd, eee, x [11], 12 );
    hhh160 ( eee, aaa, bbb, ccc, ddd, x [ 8], 13 );
    hhh160 ( ddd, eee, aaa, bbb, ccc, x [12],  5 );
    hhh160 ( ccc, ddd, eee, aaa, bbb, x [ 2], 14 );
    hhh160 ( bbb, ccc, ddd, eee, aaa, x [10], 13 );
    hhh160 ( aaa, bbb, ccc, ddd, eee, x [ 0], 13 );
    hhh160 ( eee, aaa, bbb, ccc, ddd, x [ 4],  7 );
    hhh160 ( ddd, eee, aaa, bbb, ccc, x [13],  5 );
    { parallel round 3 }
    ggg160 ( ccc, ddd, eee, aaa, bbb, x [ 8], 15 );
    ggg160 ( bbb, ccc, ddd, eee, aaa, x [ 6],  5 );
    ggg160 ( aaa, bbb, ccc, ddd, eee, x [ 4],  8 );
    ggg160 ( eee, aaa, bbb, ccc, ddd, x [ 1], 11 );
    ggg160 ( ddd, eee, aaa, bbb, ccc, x [ 3], 14 );
    ggg160 ( ccc, ddd, eee, aaa, bbb, x [11], 14 );
    ggg160 ( bbb, ccc, ddd, eee, aaa, x [15],  6 );
    ggg160 ( aaa, bbb, ccc, ddd, eee, x [ 0], 14 );
    ggg160 ( eee, aaa, bbb, ccc, ddd, x [ 5],  6 );
    ggg160 ( ddd, eee, aaa, bbb, ccc, x [12],  9 );
    ggg160 ( ccc, ddd, eee, aaa, bbb, x [ 2], 12 );
    ggg160 ( bbb, ccc, ddd, eee, aaa, x [13],  9 );
    ggg160 ( aaa, bbb, ccc, ddd, eee, x [ 9], 12 );
    ggg160 ( eee, aaa, bbb, ccc, ddd, x [ 7],  5 );
    ggg160 ( ddd, eee, aaa, bbb, ccc, x [10], 15 );
    ggg160 ( ccc, ddd, eee, aaa, bbb, x [14],  8 );
    { parallel round 4 }
    fff160 ( bbb, ccc, ddd, eee, aaa, x [12],  8 );
    fff160 ( aaa, bbb, ccc, ddd, eee, x [15],  5 );
    fff160 ( eee, aaa, bbb, ccc, ddd, x [10], 12 );
    fff160 ( ddd, eee, aaa, bbb, ccc, x [ 4],  9 );
    fff160 ( ccc, ddd, eee, aaa, bbb, x [ 1], 12 );
    fff160 ( bbb, ccc, ddd, eee, aaa, x [ 5],  5 );
    fff160 ( aaa, bbb, ccc, ddd, eee, x [ 8], 14 );
    fff160 ( eee, aaa, bbb, ccc, ddd, x [ 7],  6 );
    fff160 ( ddd, eee, aaa, bbb, ccc, x [ 6],  8 );
    fff160 ( ccc, ddd, eee, aaa, bbb, x [ 2], 13 );
    fff160 ( bbb, ccc, ddd, eee, aaa, x [13],  6 );
    fff160 ( aaa, bbb, ccc, ddd, eee, x [14],  5 );
    fff160 ( eee, aaa, bbb, ccc, ddd, x [ 0], 15 );
    fff160 ( ddd, eee, aaa, bbb, ccc, x [ 3], 13 );
    fff160 ( ccc, ddd, eee, aaa, bbb, x [ 9], 11 );
    fff160 ( bbb, ccc, ddd, eee, aaa, x [11], 11 );

    ddd := ddd + cc + MDBuffer [1];
    MDBuffer [1] := MDBuffer [2] + dd + eee;
    MDBuffer [2] := MDBuffer [3] + ee + aaa;
    MDBuffer [3] := MDBuffer [4] + aa + bbb;
    MDBuffer [4] := MDBuffer [0] + bb + ccc;
    MDBuffer [0] := ddd;
end;

procedure RMDUpdate (var ctx: TRMDCtx; aBuffer: Pointer; aLength: LongWord; aSize: Word);
var
    x : array [0..15] of LongWord;
begin
    while ( aLength > 0 ) do
    begin
        ctx.Buffer [ ctx.CurrentLength ] := PByte (aBuffer)^;
        Inc (ctx.CurrentLength);
        aBuffer := Ptr ( LongWord (aBuffer) +1 );
        if ( ctx.CurrentLength = 64 ) then
        begin
            Move ( (@ctx.Buffer)^, x, 64 );
            if aSize = 160 then
                Compress160 (ctx.MDBuffer, x)
            else
                Compress128 (ctx.MDBuffer, x);
            Inc (ctx.Length,64);
            ctx.CurrentLength := 0;
        end;
        Dec (aLength);
    end;
end;

procedure RMDfinish (var MDBuffer: array of LongWord; StrPtr: Pointer; lswlen, mswlen: LongWord; aSize: Byte);
var
    x : array [0..15] of LongWord;
    I : LongWord;
begin
    FillChar ( x, SizeOf (x), 0 );
    I := 0;
    while ( I < (lswlen and 63) ) do
    begin
        x [I shr 2] := x [I shr 2] xor (  PByte (StrPtr)^ shl ( 8 * (I and 3) )  );
        Inc (I);
        StrPtr := Ptr ( LongWord (StrPtr) +1 );
    end;
    x [ (lswlen shr 2) and 15 ] := x [ (lswlen shr 2) and 15 ] xor (  LongWord (1) shl ( 8 * (lswlen and 3) + 7 )  );
    if ( (lswlen and 63) > 55 ) then
    begin
        if ( aSize = 160 ) then
            Compress160 (MDBuffer, x)
        else
            Compress128 (MDBuffer, x);
        FillChar ( x, SizeOf (x), 0 );
    end;
    x [14] := lswlen shl 3;
    x [15] := (lswlen shr 29) or (mswlen shl 3);
    if ( aSize = 160 ) then
        Compress160 (MDBuffer, x)
    else
        Compress128 (MDBuffer, x);
end;

function RMDFinal (var ctx: TRMDCtx; aSize: LongWord) : String;
begin
    Inc (ctx.Length, ctx.CurrentLength);
    RMDfinish (ctx.MDBuffer, @ctx.Buffer, ctx.Length, 0, aSize);
    Result := (
        IntToHex ( Endian (ctx.MDBuffer [0]), 8 ) +
        IntToHex ( Endian (ctx.MDBuffer [1]), 8 ) +
        IntToHex ( Endian (ctx.MDBuffer [2]), 8 ) +
        IntToHex ( Endian (ctx.MDBuffer [3]), 8 )
    );
    if ( aSize = 160 ) then
    Result := Result +
        IntToHex ( Endian (ctx.MDBuffer [4]), 8 );
end;

function RipeMD128 (const aValue: String) : String;
var
    ctx    : TRMDCtx;
    Buffer : PChar;
begin
    Result := '';
    try
        RMD128Init (ctx);
        try
            Buffer := PChar (aValue);
            RMDUpdate ( ctx, Buffer, Length (Buffer), 128 );
        finally
            Result := RMDFinal (ctx,128);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_RIPEMD128,E.Message]);
    end;
end;

function RipeMD128 (const aValue: PChar) : String;
var
    ctx : TRMDCtx;
begin
    Result := '';
    try
        RMD128Init (ctx);
        try
            RMDUpdate ( ctx, aValue, Length (aValue), 128 );
        finally
            Result := RMDFinal (ctx,128);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_RIPEMD128,E.Message]);
    end;
end;

function RipeMD128 (const aStream: TStream;
                    const aBlockSize: LongWord = 1024) : String;
var
    ctx    : TRMDCtx;
    Buffer : PChar;
    L      : LongWord;
begin
    Result := '';
    try
        with aStream do
        try
            RMD128Init (ctx);
            Position := 0;
            L := aBlockSize;
            Buffer := AllocMem (L+1);
            while ( Position < Size ) do
            begin
                if ( (Size - Position) < aBlockSize ) then
                begin
                    L := Size - Position;
                    Dispose (Buffer);
                    Buffer := NIL;
                    Buffer := AllocMem (L+1);
                end;
                if ( Read (Buffer^,L) > 0 ) then
                    RMDUpdate (ctx,Buffer,L,128);
            end;
        finally
            Dispose (Buffer);
            Buffer := NIL;
            Result := RMDFinal (ctx,128);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_RIPEMD128,E.Message]);
    end;
end;

function RipeMD128File (const aFileName: String;
                        const aBlockSize: LongWord = 1024) : String;
var
    Stream : TStream;
begin
    Result := '';
    try
        Stream := TFileStream.Create (aFileName,fmOpenRead);
        try
            Result := RipeMD128 (Stream,aBlockSize);
        finally
            FreeAndNil (Stream);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_RIPEMD128,E.Message]);
    end;
end;

function RipeMD160 (const aValue: String) : String;
var
    ctx    : TRMDCtx;
    Buffer : PChar;
begin
    Result := '';
    try
        RMD160Init (ctx);
        try
            Buffer := PChar (aValue);
            RMDUpdate ( ctx, Buffer, Length (Buffer), 160 );
        finally
            Result := RMDFinal (ctx,160);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_RIPEMD160,E.Message]);
    end;
end;

function RipeMD160 (const aValue: PChar) : String;
var
    ctx : TRMDCtx;
begin
    Result := '';
    try
        RMD160Init (ctx);
        try
            RMDUpdate ( ctx, aValue, Length (aValue), 160 );
        finally
            Result := RMDFinal (ctx,160);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_RIPEMD160,E.Message]);
    end;
end;

function RipeMD160 (const aStream: TStream;
                    const aBlockSize: LongWord = 1024) : String;
var
    ctx    : TRMDCtx;
    Buffer : PChar;
    L      : LongWord;
begin
    Result := '';
    try
        with aStream do
        try
            RMD160Init (ctx);
            Position := 0;
            L := aBlockSize;
            Buffer := AllocMem (L+1);
            while ( Position < Size ) do
            begin
                if ( (Size - Position) < aBlockSize ) then
                begin
                    L := Size - Position;
                    Dispose (Buffer);
                    Buffer := NIL;
                    Buffer := AllocMem (L+1);
                end;
                if ( Read (Buffer^,L) > 0 ) then
                    RMDUpdate (ctx,Buffer,L,160);
            end;
        finally
            Dispose (Buffer);
            Buffer := NIL;
            Result := RMDFinal (ctx,160);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_RIPEMD160,E.Message]);
    end;
end;

function RipeMD160File (const aFileName: String;
                        const aBlockSize: LongWord = 1024) : String;
var
    Stream : TStream;
begin
    Result := '';
    try
        Stream := TFileStream.Create (aFileName,fmOpenRead);
        try
            Result := RipeMD160 (Stream,aBlockSize);
        finally
            FreeAndNil (Stream);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_RIPEMD160,E.Message]);
    end;
end;
