{******************************************************************************}
{*  MD.imp.inc                                                                *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
const
    MD_PADDING : array [0..63] of Byte = (
        $80, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00
    );
    
{$IFDEF ASM}
function f4 (x, y, z: LongWord) : LongWord; assembler;
asm
  and   edx,eax
  not   eax
  and   eax,ecx
  or    eax,edx
end;
{$ELSE}
function f4 (x, y, z: LongWord) : LongWord;
begin
    Result := (  (x and y) or ( (not x) and (z) )  );
end;
{$ENDIF ASM}

{$IFDEF ASM}
function g4 (x, y, z: LongWord) : LongWord; assembler;
asm
  push  ecx
  and   ecx,eax
  and   eax,edx
  or    eax,ecx
  pop   ecx
  and   edx,ecx
  or    eax,edx
end;
{$ELSE}
function g4 (x, y, z: LongWord) : LongWord;
begin
    Result := (x and y) or (x and z) or (y and z);
end;
{$ENDIF ASM}

{$IFDEF ASM}
function h4 (x, y, z: LongWord) : LongWord; assembler;
asm
  xor eax,edx
  xor eax,ecx
end;
{$ELSE}
function h4 (x, y, z: LongWord) : LongWord;
begin
    Result := x xor y xor z;
end;
{$ENDIF ASM}

procedure ff4 (var a: LongWord; b, c, d, x, s: LongWord);
begin
    a := a + f4 (b, c, d) + x;
    a := rol (a, s);
end;

procedure gg4 (var a: LongWord; b, c, d, x, s: LongWord);
begin
    a := a + g4 (b, c, d) + x + $5a827999;
    a := rol (a, s);
end;

procedure hh4 (var a: LongWord; b, c, d, x, s: LongWord);
begin
    a := a + h4 (b, c, d) + x + $6ed9eba1;
    a := rol (a, s);
end;

procedure MDInit (ctx: PMD4Ctx);
begin
    ctx^.Count [0] := 0;
    ctx^.Count [1] := 0;
    ctx^.State [0] := $67452301;
    ctx^.State [1] := $efcdab89;
    ctx^.State [2] := $98badcfe;
    ctx^.State [3] := $10325476;
end;

procedure MDEncode (Output, Input: Pointer; Length: LongWord);
var
    I : LongWord;
    J : LongWord;
begin
    I := 0;
    J := 0;
    while ( J < Length ) do
    begin
        PByteArray (Output)^ [J]     := (   PDWordArray (Input)^ [I]          and $ff  );
        PByteArray (Output)^ [J + 1] := ( ( PDWordArray (Input)^ [I] shr 8  ) and $ff  );
        PByteArray (Output)^ [J + 2] := ( ( PDWordArray (Input)^ [I] shr 16 ) and $ff  );
        PByteArray (Output)^ [J + 3] := ( ( PDWordArray (Input)^ [I] shr 24 ) and $ff  );
        Inc (I);
        Inc (J, 4);
    end;
end;

procedure MDDecode (Output, Input: Pointer; Length: LongWord);
var
    I : LongWord;
    J : LongWord;
begin
    I := 0;
    J := 0;
    while ( J < Length ) do
    begin
        PDWordArray (Output)^ [I] := ( PByteArray (Input)^ [J] ) or
                                     ( PByteArray (Input)^ [J + 1] shl 8  ) or
                                     ( PByteArray (Input)^ [J + 2] shl 16 ) or
                                     ( PByteArray (Input)^ [J + 3] shl 24 );
        Inc (I);
        Inc (J, 4);
    end;
end;

procedure MD4Transform (var State: array of LongWord; Block: Pointer);
const
    rounds : array [0..2, 0..3] of Byte = (
        ($03, $07, $0B, $13), { rounds 0 }
        ($03, $05, $09, $0D), { rounds 1 }
        ($03, $09, $0B, $0F)  { rounds 2 }
    );
var
    a, b, c, d : LongWord;
    x          : array [0..15] of LongWord;
begin
    a := State [0];
    b := State [1];
    c := State [2];
    d := State [3];
    MDDecode (@x, Block, 64);
    { rounds 0 }
    ff4 ( a, b, c, d, x [ 0], rounds [0,0] );
    ff4 ( d, a, b, c, x [ 1], rounds [0,1] );
    ff4 ( c, d, a, b, x [ 2], rounds [0,2] );
    ff4 ( b, c, d, a, x [ 3], rounds [0,3] );
    ff4 ( a, b, c, d, x [ 4], rounds [0,0] );
    ff4 ( d, a, b, c, x [ 5], rounds [0,1] );
    ff4 ( c, d, a, b, x [ 6], rounds [0,2] );
    ff4 ( b, c, d, a, x [ 7], rounds [0,3] );
    ff4 ( a, b, c, d, x [ 8], rounds [0,0] );
    ff4 ( d, a, b, c, x [ 9], rounds [0,1] );
    ff4 ( c, d, a, b, x [10], rounds [0,2] );
    ff4 ( b, c, d, a, x [11], rounds [0,3] );
    ff4 ( a, b, c, d, x [12], rounds [0,0] );
    ff4 ( d, a, b, c, x [13], rounds [0,1] );
    ff4 ( c, d, a, b, x [14], rounds [0,2] );
    ff4 ( b, c, d, a, x [15], rounds [0,3] );
    { rounds 1 }
    gg4 ( a, b, c, d, x [ 0], rounds [1,0] );
    gg4 ( d, a, b, c, x [ 4], rounds [1,1] );
    gg4 ( c, d, a, b, x [ 8], rounds [1,2] );
    gg4 ( b, c, d, a, x [12], rounds [1,3] );
    gg4 ( a, b, c, d, x [ 1], rounds [1,0] );
    gg4 ( d, a, b, c, x [ 5], rounds [1,1] );
    gg4 ( c, d, a, b, x [ 9], rounds [1,2] );
    gg4 ( b, c, d, a, x [13], rounds [1,3] );
    gg4 ( a, b, c, d, x [ 2], rounds [1,0] );
    gg4 ( d, a, b, c, x [ 6], rounds [1,1] );
    gg4 ( c, d, a, b, x [10], rounds [1,2] );
    gg4 ( b, c, d, a, x [14], rounds [1,3] );
    gg4 ( a, b, c, d, x [ 3], rounds [1,0] );
    gg4 ( d, a, b, c, x [ 7], rounds [1,1] );
    gg4 ( c, d, a, b, x [11], rounds [1,2] );
    gg4 ( b, c, d, a, x [15], rounds [1,3] );
    { rounds 2 }
    hh4 ( a, b, c, d, x [ 0], rounds [2,0] );
    hh4 ( d, a, b, c, x [ 8], rounds [2,1] );
    hh4 ( c, d, a, b, x [ 4], rounds [2,2] );
    hh4 ( b, c, d, a, x [12], rounds [2,3] );
    hh4 ( a, b, c, d, x [ 2], rounds [2,0] );
    hh4 ( d, a, b, c, x [10], rounds [2,1] );
    hh4 ( c, d, a, b, x [ 6], rounds [2,2] );
    hh4 ( b, c, d, a, x [14], rounds [2,3] );
    hh4 ( a, b, c, d, x [ 1], rounds [2,0] );
    hh4 ( d, a, b, c, x [ 9], rounds [2,1] );
    hh4 ( c, d, a, b, x [ 5], rounds [2,2] );
    hh4 ( b, c, d, a, x [13], rounds [2,3] );
    hh4 ( a, b, c, d, x [ 3], rounds [2,0] );
    hh4 ( d, a, b, c, x [11], rounds [2,1] );
    hh4 ( c, d, a, b, x [ 7], rounds [2,2] );
    hh4 ( b, c, d, a, x [15], rounds [2,3] );

    State [0] := State [0] + a;
    State [1] := State [1] + b;
    State [2] := State [2] + c;
    State [3] := State [3] + d;
end;

procedure MDUpdate (ctx: PMD4Ctx; Input: Pointer; InputLength: LongWord; Transform: TMDTransform);
var
    I          : LongWord;
    Index      : LongWord;
    PartLength : LongWord;
begin
    Index := (ctx^.count [0] shr 3) and $3F;
    ctx^.count [0] := ctx^.count [0] + InputLength shl 3;
    if ( ctx^.count [0] < (InputLength shl 3) ) then
        Inc (ctx^.count [1]);
    ctx^.count [1] := ctx^.count [1] + InputLength shr 29;
    PartLength := 64 - Index;
    if ( InputLength >= PartLength ) then
    begin
        Move ( Input^, ctx^.Buffer [Index], PartLength);
        Transform (ctx^.State, @ctx^.Buffer);
        I := PartLength;
        while ( I + 63 < InputLength ) do
        begin
            Transform (  ctx^.State, Addr ( PByteArray (Input)^ [I] )  );
            Inc (I, 64);
        end;
        Index := 0;
    end
    else
        I := 0;
    Move ( PByteArray (Input)^ [I], ctx^.Buffer [Index], InputLength - I );
end;

function MDFinal (ctx: PMD4Ctx; Transform: TMDTransform) : String;
var
    Digest    : array [0..15] of Char;
    Bits      : array [0..7] of Char;
    Index     : LongWord;
    PadLength : LongWord;
begin
    Result := '';
    MDEncode (@Bits, @ctx^.Count, 8);
    Index := (ctx^.Count [0] shr 3) and $3f;
    if ( Index < 56 ) then
        PadLength := 56 - Index
    else
        PadLength := 120 - Index;

    MDUpdate (ctx, @MD_PADDING, PadLength, Transform);
    MDUpdate (ctx, @Bits, 8, Transform);
    MDEncode (@Digest, @ctx^.State, 16);
    FillChar ( ctx^, 0, SizeOf (TMD4Ctx) );

    for Index := 0 to 15 do
        Result := Result + IntToHex ( Ord (Digest [Index]), 2 );
end;

{$IFDEF ASM}
function g5 (x, y, z: LongWord) : LongWord; assembler;
asm
  push  ecx
  and   ecx,eax
  and   eax,edx
  or    eax,ecx
  pop   ecx
  not   ecx
  and   edx,ecx
  or    eax,edx
end;
{$ELSE}
function g5 (x, y, z: LongWord) : LongWord;
begin
    Result := (x and y) or (x and z) or (y and not z);
end;
{$ENDIF ASM}

{$IFDEF ASM}
function i5 (x, y, z: LongWord) : LongWord; assembler;
asm
  not   ecx
  or    eax,ecx
  xor   eax,edx
end;
{$ELSE}
function i5 (x, y, z: LongWord) : LongWord;
begin
    Result := y xor ( x or (not z) );
end;
{$ENDIF ASM}

procedure ff5 (var a: LongWord; b, c, d, x, s, ac: LongWord);
begin
    a := a + f4 (b, c, d) + x + ac;
    a := rol (a, s);
    a := a + b;
end;

procedure gg5 (var a: LongWord; b, c, d, x, s, ac: LongWord);
begin
    a := a + g5 (b, c, d) + x + ac;
    a := rol (a, s);
    a := a + b;
end;

procedure hh5 (var a: LongWord; b, c, d, x, s, ac: LongWord);
begin
   a := a + h4 (b, c, d) + x + ac;
   a := rol (a, s);
   a := a + b;
end;

procedure ii5 (var a: LongWord; b, c, d, x, s, ac: LongWord);
begin
   a := a + i5 (b, c, d) + x + ac;
   a := rol (a, s);
   a := a + b;  
end;

procedure MD5Transform (var State: array of LongWord; Block: Pointer);
const
    rounds : array [0..3, 0..3] of Byte = (
        ($07, $0C, $11, $16), { rounds 0 }
        ($05, $09, $0E, $14), { rounds 1 }
        ($04, $0B, $10, $17), { rounds 2 }
        ($06, $0A, $0F, $15)  { rounds 3 }
    );
var
    a, b, c, d : LongWord;
    x          : array [0..15] of LongWord;
begin
    a := State [0];
    b := State [1];
    c := State [2];
    d := State [3];
    MDDecode (@x, Block, 64);
    { rounds 0 }
    ff5 ( a, b, c, d, x [ 0], rounds [0,0], $d76aa478 );
    ff5 ( d, a, b, c, x [ 1], rounds [0,1], $e8c7b756 );
    ff5 ( c, d, a, b, x [ 2], rounds [0,2], $242070db );
    ff5 ( b, c, d, a, x [ 3], rounds [0,3], $c1bdceee );
    ff5 ( a, b, c, d, x [ 4], rounds [0,0], $f57c0faf );
    ff5 ( d, a, b, c, x [ 5], rounds [0,1], $4787c62a );
    ff5 ( c, d, a, b, x [ 6], rounds [0,2], $a8304613 );
    ff5 ( b, c, d, a, x [ 7], rounds [0,3], $fd469501 );
    ff5 ( a, b, c, d, x [ 8], rounds [0,0], $698098d8 );
    ff5 ( d, a, b, c, x [ 9], rounds [0,1], $8b44f7af );
    ff5 ( c, d, a, b, x [10], rounds [0,2], $ffff5bb1 );
    ff5 ( b, c, d, a, x [11], rounds [0,3], $895cd7be );
    ff5 ( a, b, c, d, x [12], rounds [0,0], $6b901122 );
    ff5 ( d, a, b, c, x [13], rounds [0,1], $fd987193 );
    ff5 ( c, d, a, b, x [14], rounds [0,2], $a679438e );
    ff5 ( b, c, d, a, x [15], rounds [0,3], $49b40821 );
    { rounds 1 }
    gg5 ( a, b, c, d, x [ 1], rounds [1,0], $f61e2562 );
    gg5 ( d, a, b, c, x [ 6], rounds [1,1], $c040b340 );
    gg5 ( c, d, a, b, x [11], rounds [1,2], $265e5a51 );
    gg5 ( b, c, d, a, x [ 0], rounds [1,3], $e9b6c7aa );
    gg5 ( a, b, c, d, x [ 5], rounds [1,0], $d62f105d );
    gg5 ( d, a, b, c, x [10], rounds [1,1], $02441453 );
    gg5 ( c, d, a, b, x [15], rounds [1,2], $d8a1e681 );
    gg5 ( b, c, d, a, x [ 4], rounds [1,3], $e7d3fbc8 );
    gg5 ( a, b, c, d, x [ 9], rounds [1,0], $21e1cde6 );
    gg5 ( d, a, b, c, x [14], rounds [1,1], $c33707d6 );
    gg5 ( c, d, a, b, x [ 3], rounds [1,2], $f4d50d87 );
    gg5 ( b, c, d, a, x [ 8], rounds [1,3], $455a14ed );
    gg5 ( a, b, c, d, x [13], rounds [1,0], $a9e3e905 );
    gg5 ( d, a, b, c, x [ 2], rounds [1,1], $fcefa3f8 );
    gg5 ( c, d, a, b, x [ 7], rounds [1,2], $676f02d9 );
    gg5 ( b, c, d, a, x [12], rounds [1,3], $8d2a4c8a );
    { rounds 2 }
    hh5 ( a, b, c, d, x [ 5], rounds [2,0], $fffa3942 );
    hh5 ( d, a, b, c, x [ 8], rounds [2,1], $8771f681 );
    hh5 ( c, d, a, b, x [11], rounds [2,2], $6d9d6122 );
    hh5 ( b, c, d, a, x [14], rounds [2,3], $fde5380c );
    hh5 ( a, b, c, d, x [ 1], rounds [2,0], $a4beea44 );
    hh5 ( d, a, b, c, x [ 4], rounds [2,1], $4bdecfa9 );
    hh5 ( c, d, a, b, x [ 7], rounds [2,2], $f6bb4b60 );
    hh5 ( b, c, d, a, x [10], rounds [2,3], $bebfbc70 );
    hh5 ( a, b, c, d, x [13], rounds [2,0], $289b7ec6 );
    hh5 ( d, a, b, c, x [ 0], rounds [2,1], $eaa127fa );
    hh5 ( c, d, a, b, x [ 3], rounds [2,2], $d4ef3085 );
    hh5 ( b, c, d, a, x [ 6], rounds [2,3], $04881d05 );
    hh5 ( a, b, c, d, x [ 9], rounds [2,0], $d9d4d039 );
    hh5 ( d, a, b, c, x [12], rounds [2,1], $e6db99e5 );
    hh5 ( c, d, a, b, x [15], rounds [2,2], $1fa27cf8 );
    hh5 ( b, c, d, a, x [ 2], rounds [2,3], $c4ac5665 );
    { rounds 3 }
    ii5 ( a, b, c, d, x [ 0], rounds [3,0], $f4292244 );
    ii5 ( d, a, b, c, x [ 7], rounds [3,1], $432aff97 );
    ii5 ( c, d, a, b, x [14], rounds [3,2], $ab9423a7 );
    ii5 ( b, c, d, a, x [ 5], rounds [3,3], $fc93a039 );
    ii5 ( a, b, c, d, x [12], rounds [3,0], $655b59c3 );
    ii5 ( d, a, b, c, x [ 3], rounds [3,1], $8f0ccc92 );
    ii5 ( c, d, a, b, x [10], rounds [3,2], $ffeff47d );
    ii5 ( b, c, d, a, x [ 1], rounds [3,3], $85845dd1 );
    ii5 ( a, b, c, d, x [ 8], rounds [3,0], $6fa87e4f );
    ii5 ( d, a, b, c, x [15], rounds [3,1], $fe2ce6e0 );
    ii5 ( c, d, a, b, x [ 6], rounds [3,2], $a3014314 );
    ii5 ( b, c, d, a, x [13], rounds [3,3], $4e0811a1 );
    ii5 ( a, b, c, d, x [ 4], rounds [3,0], $f7537e82 );
    ii5 ( d, a, b, c, x [11], rounds [3,1], $bd3af235 );
    ii5 ( c, d, a, b, x [ 2], rounds [3,2], $2ad7d2bb );
    ii5 ( b, c, d, a, x [ 9], rounds [3,3], $eb86d391 );

    State [0] := State  [0] + a;
    State [1] := State  [1] + b;
    State [2] := State  [2] + c;
    State [3] := State  [3] + d;
end;

function MD4 (const aValue: String) : String;
var
    ctx    : TMD4Ctx;
    Buffer : PChar;
begin
    Result := '';
    try
        MDInit (@ctx);
        try
            Buffer := PChar (aValue);
            MDUpdate ( @ctx, Buffer, Length (Buffer), MD4Transform );
        finally
            Result := MDFinal (@ctx,MD4Transform);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_MD4,E.Message]);
    end;
end;

function MD4 (const aValue: PChar) : String;
var
    ctx : TMD4Ctx;
begin
    Result := '';
    try
        MDInit (@ctx);
        try
            MDUpdate ( @ctx, aValue, Length (aValue), MD4Transform );
        finally
            Result := MDFinal (@ctx,MD4Transform);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_MD4,E.Message]);
    end;
end;

function MD4 (const aStream: TStream;
              const aBlockSize: LongWord = 1024) : String;
var
    ctx    : TMD4Ctx;
    Buffer : PChar;
    L      : LongWord;
begin
    Result := '';
    try
        with aStream do
        try
            MDInit (@ctx);
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
                    MDUpdate (@ctx,Buffer,L,MD4Transform);
            end;
        finally
            Dispose (Buffer);
            Buffer := NIL;
            Result := MDFinal (@ctx,MD4Transform);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_MD4,E.Message]);
    end;
end;

function MD4File (const aFileName: String;
                  const aBlockSize: LongWord = 1024) : String;
var
    Stream : TStream;
begin
    Result := '';
    try
        Stream := TFileStream.Create (aFileName,fmOpenRead);
        try
            Result := MD4 (Stream,aBlockSize);
        finally
            FreeAndNil (Stream);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_MD4,E.Message]);
    end;
end;

function MD5 (const aValue: String) : String;
var
    ctx    : TMD4Ctx;
    Buffer : PChar;
begin
    Result := '';
    try
        MDInit (@ctx);
        try
            Buffer := PChar (aValue);
            MDUpdate ( @ctx, Buffer, Length (Buffer), MD5Transform );
        finally
            Result := MDFinal (@ctx,MD5Transform);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_MD5,E.Message]);
    end;
end;

function MD5 (const aValue: PChar) : String;
var
    ctx : TMD4Ctx;
begin
    Result := '';
    try
        MDInit (@ctx);
        try
            MDUpdate ( @ctx, aValue, Length (aValue), MD5Transform );
        finally
            Result := MDFinal (@ctx,MD5Transform);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_MD5,E.Message]);
    end;
end;

function MD5 (const aStream: TStream;
              const aBlockSize: LongWord = 1024) : String;
var
    ctx    : TMD4Ctx;
    Buffer : PChar;
    L      : LongWord;
begin
    Result := '';
    try
        with aStream do
        try
            MDInit (@ctx);
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
                    MDUpdate (@ctx,Buffer,L,MD5Transform);
            end;
        finally
            Dispose (Buffer);
            Buffer := NIL;
            Result := MDFinal (@ctx,MD5Transform);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_MD5,E.Message]);
    end;
end;

function MD5File (const aFileName: String;
                  const aBlockSize: LongWord = 1024) : String;
var
    Stream : TStream;
begin
    Result := '';
    try
        Stream := TFileStream.Create (aFileName,fmOpenRead);
        try
            Result := MD5 (Stream,aBlockSize);
        finally
            FreeAndNil (Stream);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_MD5,E.Message]);
    end;
end;
