{******************************************************************************}
{*  SHA.imp.inc                                                               *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
const
    K256 : array [0..63] of LongWord = (
        $428A2F98, $71374491, $B5C0FBCF, $E9B5DBA5,
        $3956C25B, $59F111F1, $923F82A4, $AB1C5ED5,
        $D807AA98, $12835B01, $243185BE, $550C7DC3,
        $72BE5D74, $80DEB1FE, $9BDC06A7, $C19BF174,
        $E49B69C1, $EFBE4786, $0FC19DC6, $240CA1CC,
        $2DE92C6F, $4A7484AA, $5CB0A9DC, $76F988DA,
        $983E5152, $A831C66D, $B00327C8, $BF597FC7,
        $C6E00BF3, $D5A79147, $06CA6351, $14292967,
        $27B70A85, $2E1B2138, $4D2C6DFC, $53380D13,
        $650A7354, $766A0ABB, $81C2C92E, $92722C85,
        $A2BFE8A1, $A81A664B, $C24B8B70, $C76C51A3,
        $D192E819, $D6990624, $F40E3585, $106AA070,
        $19A4C116, $1E376C08, $2748774C, $34B0BCB5,
        $391C0CB3, $4ED8AA4A, $5B9CCA4F, $682E6FF3,
        $748F82EE, $78A5636F, $84C87814, $8CC70208,
        $90BEFFFA, $A4506CEB, $BEF9A3F7, $C67178F2
    );
    K512 : array [0..79] of Int64 = (
        $428A2F98D728AE22, $7137449123EF65CD,
        $B5C0FBCFEC4D3B2F, $E9B5DBA58189DBBC,
        $3956C25BF348B538, $59F111F1B605D019,
        $923F82A4AF194F9B, $AB1C5ED5DA6D8118,
        $D807AA98A3030242, $12835B0145706FBE,
        $243185BE4EE4B28C, $550C7DC3D5FFB4E2,
        $72BE5D74F27B896F, $80DEB1FE3B1696B1,
        $9BDC06A725C71235, $C19BF174CF692694,
        $E49B69C19EF14AD2, $EFBE4786384F25E3,
        $0FC19DC68B8CD5B5, $240CA1CC77AC9C65,
        $2DE92C6F592B0275, $4A7484AA6EA6E483,
        $5CB0A9DCBD41FBD4, $76F988DA831153B5,
        $983E5152EE66DFAB, $A831C66D2DB43210,
        $B00327C898FB213F, $BF597FC7BEEF0EE4,
        $C6E00BF33DA88FC2, $D5A79147930AA725,
        $06CA6351E003826F, $142929670A0E6E70,
        $27B70A8546D22FFC, $2E1B21385C26C926,
        $4D2C6DFC5AC42AED, $53380D139D95B3DF,
        $650A73548BAF63DE, $766A0ABB3C77B2A8,
        $81C2C92E47EDAEE6, $92722C851482353B,
        $A2BFE8A14CF10364, $A81A664BBC423001,
        $C24B8B70D0F89791, $C76C51A30654BE30,
        $D192E819D6EF5218, $D69906245565A910,
        $F40E35855771202A, $106AA07032BBD1B8,
        $19A4C116B8D2D0C8, $1E376C085141AB53,
        $2748774CDF8EEB99, $34B0BCB5E19B48A8,
        $391C0CB3C5C95A63, $4ED8AA4AE3418ACB,
        $5B9CCA4F7763E373, $682E6FF3D6B2B8A3,
        $748F82EE5DEFB2FC, $78A5636F43172F60,
        $84C87814A1F0AB72, $8CC702081A6439EC,
        $90BEFFFA23631E28, $A4506CEBDE82BDE9,
        $BEF9A3F7B2C67915, $C67178F2E372532B,
        $CA273ECEEA26619C, $D186B8C721C0C207,
        $EADA7DD6CDE0EB1E, $F57D4F7FEE6ED178,
        $06F067AA72176FBA, $0A637DC5A2C898A6,
        $113F9804BEF90DAE, $1B710B35131C471B,
        $28DB77F523047D84, $32CAAB7B40C72493,
        $3C9EBE0A15C9BEBC, $431D67C49C100D4C,
        $4CC5D4BECB3E42B6, $597F299CFC657E2A,
        $5FCB6FAB3AD6FAEC, $6C44198C4A475817
    );

function Ch (x, y, z: Int64) : Int64;
begin
    Result := (x and y) xor ( (not x) and z );
end;

function Maj (x, y, z: Int64) : Int64;
begin
    Result := (x and y) xor (x and z) xor (y and z);
end;

function Sigma0 (x: Int64) : Int64;
begin
    Result := ror64 (x, 28) xor ror64 (x, 34) xor ror64 (x, 39);
end;

function Sigma1 (x: Int64) : Int64;
begin
    Result := ror64 (x, 14) xor ror64 (x, 18) xor ror64 (x, 41);
end;

function Gamma0 (x: Int64) : Int64;
begin
    Result := ror64 (x, 1) xor ror64 (x, 8) xor (x shr 7);
end;

function Gamma1 (x: Int64) : Int64;
begin
    Result := ror64 (x, 19) xor ror64 (x, 61) xor (x shr 6);
end;

{$IFDEF ASM}
function Ch256 (x, y, z: LongWord) : LongWord; assembler;
asm
  and   edx,eax
  not   eax
  and   eax,ecx
  xor   eax,edx
end;
{$ELSE}
function Ch256 (x, y, z: LongWord) : LongWord;
begin
    Result := (x and y) xor ( (not x) and z );
end;
{$ENDIF ASM}

{$IFDEF ASM}
function Maj256 (x, y, z: LongWord) : LongWord; assembler;
asm
  push  ecx
  and   ecx,eax
  and   eax,edx
  xor   eax,ecx
  pop   ecx
  and   edx,ecx
  xor   eax,edx
end;
{$ELSE}
function Maj256 (x, y, z: LongWord) : LongWord;
begin
    Result := (x and y) xor (x and z) xor (y and z);
end;
{$ENDIF ASM}

function E0256 (x: LongWord) : LongWord;
begin
    Result := ror (x, 2) xor ror (x, 13) xor ror (x, 22);
end;

function E1256 (x: LongWord) : LongWord;
begin
    Result := ror (x, 6) xor ror (x, 11) xor ror (x, 25);
end;

function F0256 (x: LongWord) : LongWord;
begin
    Result := ror (x, 7) xor ror (x, 18) xor (x shr 3);
end;

function F1256 (x: LongWord) : LongWord;
begin
    Result := ror (x, 17) xor ror (x, 19) xor (x shr 10);
end;

function FT1 (t: Byte; x, y, z: LongWord) : LongWord;
begin
    case t of
        0..19  : Result := (x and y) or ((not x) and z);
        20..39 : Result := x xor y xor z;
        40..59 : Result := (x and y) or (x and z) or (y and z);
        else     Result := x xor y xor z;
    end;
end;

{$IFDEF ASM}
function KT1 (t: Byte) : LongWord; assembler;
asm
  cmp   al,19
  jg    @@1
  mov   eax,5a827999h
  jmp   @@end
@@1:
  cmp   al,39
  jg    @@2
  mov   eax,6ed9eba1h
  jmp   @@end
@@2:
  cmp   al,59
  jg    @@3
  mov   eax,8f1bbcdch
  jmp   @@end
@@3:
  mov   eax,0ca62c1d6h
@@end:
end;
{$ELSE}
function KT1 (t: Byte) : LongWord;
begin
    case t of
        0..19  : Result := $5a827999;
        20..39 : Result := $6ed9eba1;
        40..59 : Result := $8f1bbcdc;
        else     Result := $ca62c1d6
    end;
end;
{$ENDIF ASM}

procedure SHA1_Compress (var ctx: TSHA256Ctx);
var
    S : array [0..4] of LongWord;
    W : array [0..79] of LongWord;
    t : LongWord;
    I : Byte;
begin
    Move ( ctx.State, S, SizeOf (S) );
    for I := 0 to 15 do
        W [I] := Endian (  PLongWord ( LongWord (@ctx.Buffer) + I*4 )^  );
    for I := 16 to 79 do
        W [I] := rol (W [I-3] xor W [I-8] xor W [I-14] xor W [I-16], 1);
    for I := 0 to 79 do
    begin
        t := rol (S [0], 5) + FT1 (i, S [1], S [2], S [3]) + S [4] + KT1 (I) + W [I];
        S [4] := S [3];
        S [3] := S [2];
        S [2] := rol (S [1], 30);
        S [1] := S [0];
        S [0] := t;
    end;
    for I := 0 to 4 do
        ctx.State [I] := ctx.State [I] + S [I];
end;

procedure SHA256_Compress (var ctx: TSHA256Ctx);
var
    S      : array [0..7] of LongWord;
    W      : array [0..63] of LongWord;
    t1, t2 : LongWord;
    I      : Byte;
begin
    Move ( ctx.State, S, SizeOf (S) );
    for I := 0 to 15 do
        W [I] := Endian (  PLongWord ( LongWord (@ctx.Buffer) + I*4)^  );
    for I := 16 to 63 do
        W [I] := F1256 (W [I-2]) + W [I-7] + F0256 (W [I-15]) + W [I-16];
    for I := 0 to 63 do
    begin
        t1 := S [7] + E1256(S [4]) + Ch256 (S [4], S [5], S [6]) + K256 [I] + W [I];
        t2 := E0256 (S [0]) + Maj256 (S [0], S [1], S [2]);
        S [7] := S [6];
        S [6] := S [5];
        S [5] := S [4];
        S [4] := S [3] + t1;
        S [3] := S [2];
        S [2] := S [1];
        S [1] := S [0];
        S [0] := t1 + t2;
    end;
    for I := 0 to 7 do
        ctx.State [I] := ctx.State [I] + S [I];
end;

procedure SHA512_Compress (var ctx: TSHA512Ctx);
var
    S      : array [0..7] of Int64;
    W      : array [0..79] of Int64;
    t0, t1 : Int64;
    I      : Byte;
begin
    Move (ctx.State, S, 64);
    for I := 0 to 15 do
        W [I] := Endian64 (  PInt64 ( LongWord (@ctx.Buffer) + I*8 )^  );
    for I := 16 to 79 do
        W [I] := Gamma1 (W [I-2]) + W [I-7] + Gamma0 (W [I-15]) + W [I-16];
    for I := 0 to 79 do
    begin
      t0 := S [7] + Sigma1 (S [4]) + Ch (S [4], S [5], S [6]) + K512 [I] + W [I];
      t1 := Sigma0 (S [0]) + Maj (S [0], S [1], S [2]);
      S [7] := S [6];
      S [6] := S [5];
      S [5] := S [4];
      S [4] := S [3] + t0;
      S [3] := S [2];
      S [2] := S [1];
      S [1] := S [0];
      S [0] := t0 + t1;
    end;
    for I := 0 to 7 do
        Inc (ctx.State [I], S [I]);
end;

procedure SHA1Init (var ctx: TSHA256Ctx);
begin
    ctx.Length := 0;
    ctx.CurrentLength := 0;
    ctx.State [0] := $67452301;
    ctx.State [1] := $efcdab89;
    ctx.State [2] := $98badcfe;
    ctx.State [3] := $10325476;
    ctx.State [4] := $c3d2e1f0;
end;

procedure SHA256Init (var ctx: TSHA256Ctx);
begin
    ctx.Length := 0;
    ctx.CurrentLength := 0;
    ctx.State [0] := $6a09e667;
    ctx.State [1] := $bb67ae85;
    ctx.State [2] := $3c6ef372;
    ctx.State [3] := $a54ff53a;
    ctx.State [4] := $510e527f;
    ctx.State [5] := $9b05688c;
    ctx.State [6] := $1f83d9ab;
    ctx.State [7] := $5be0cd19;
end;

procedure SHA384Init (var ctx: TSHA512Ctx);
begin
    ctx.Length := 0;
    ctx.CurrentLength := 0;
    ctx.State [0] := $cbbb9d5dc1059ed8;
    ctx.State [1] := $629a292a367cd507;
    ctx.State [2] := $9159015a3070dd17;
    ctx.State [3] := $152fecd8f70e5939;
    ctx.State [4] := $67332667ffc00b31;
    ctx.State [5] := $8eb44a8768581511;
    ctx.State [6] := $db0c2e0d64f98fa7;
    ctx.State [7] := $47b5481dbefa4fa4;
end;

procedure SHA512Init (var ctx: TSHA512Ctx);
begin
    ctx.Length := 0;
    ctx.CurrentLength := 0;
    ctx.State [0] := $6a09e667f3bcc908;
    ctx.State [1] := $bb67ae8584caa73b;
    ctx.State [2] := $3c6ef372fe94f82b;
    ctx.State [3] := $a54ff53a5f1d36f1;
    ctx.State [4] := $510e527fade682d1;
    ctx.State [5] := $9b05688c2b3e6c1f;
    ctx.State [6] := $1f83d9abfb41bd6b;
    ctx.State [7] := $5be0cd19137e2179;
end;

procedure SHA256Update (var ctx: TSHA256Ctx; aBuffer: Pointer; aLength: Int64; aSize: Word);
begin
    while ( aLength > 0 ) do
    begin
        ctx.Buffer [ctx.CurrentLength] := PByte (aBuffer)^;
        ctx.CurrentLength := ctx.CurrentLength + 1;
        aBuffer := Ptr ( LongWord (aBuffer) + 1 );
        if (ctx.CurrentLength = 64) then
        begin
            if (aSize = 256) then
                SHA256_Compress (ctx)
            else
                SHA1_Compress (ctx);
            ctx.Length := ctx.Length + 512;
            ctx.CurrentLength := 0;
        end;
        Dec (aLength);
    end;
end;

procedure SHA512Update (var ctx: TSHA512Ctx; aBuffer: Pointer; aLength: Int64);
begin
    while ( aLength > 0 ) do
    begin
        ctx.Buffer [ctx.CurrentLength] := PByte (aBuffer)^;
        ctx.CurrentLength := ctx.CurrentLength + 1;
        aBuffer := Ptr ( LongWord (aBuffer) + 1 );
        if ( ctx.CurrentLength = 128 ) then
        begin
            SHA512_Compress (ctx);
            ctx.Length := ctx.Length + 1024;
            ctx.CurrentLength := 0;
        end;
        Dec (aLength);
    end;
end;

function SHA512Final (var ctx: TSHA512Ctx; aSize: Word) : String;
var
    I : Byte;
begin
    Inc (ctx.Length, ctx.CurrentLength shl 3);
    ctx.Buffer [ctx.CurrentLength] := $80;
    Inc (ctx.CurrentLength);
    if ( ctx.CurrentLength > 112 ) then
    begin
        while ( ctx.CurrentLength < 128 ) do
        begin
            ctx.Buffer [ctx.CurrentLength] := 0;
            Inc (ctx.CurrentLength);
        end;
        SHA512_Compress (ctx);
        ctx.CurrentLength := 0;
    end;
    while ( ctx.CurrentLength < 112 ) do
    begin
        ctx.Buffer [ctx.CurrentLength] := 0;
        Inc (ctx.CurrentLength);
    end;
    for I := 112 to 119 do
        ctx.Buffer [I] := 0;
    for I := 120 to 127 do
        ctx.Buffer [I] := (  ctx.Length shr ( (127 - I) * 8 )  ) and $FF;
    SHA512_Compress (ctx);
    Result := '';
    if ( aSize = 384 ) then
        for I := 0 to 5 do
            Result := Result + IntToHex (ctx.State [I], 16)
    else
        for I := 0 to 7 do
            Result := Result + IntToHex (ctx.State [I], 16);
end;

function SHA256Final (var ctx: TSHA256Ctx; aSize: Word): String;
var
    I : Byte;
begin
    ctx.Length := ctx.Length + ctx.CurrentLength shl 3;
    ctx.Buffer [ctx.CurrentLength] := $80;
    ctx.CurrentLength := ctx.CurrentLength + 1;
    if ( ctx.CurrentLength > 56 ) then
    begin
        while ( ctx.CurrentLength < 64 ) do
        begin
            ctx.Buffer [ctx.CurrentLength] := 0;
            ctx.CurrentLength := ctx.CurrentLength + 1;
        end;
        if (aSize = 256) then
            SHA256_compress (ctx)
        else
            SHA1_compress (ctx);
        ctx.CurrentLength := 0;
    end;
    while ( ctx.CurrentLength < 56 ) do
    begin
        ctx.Buffer [ctx.CurrentLength] := 0;
        ctx.CurrentLength := ctx.CurrentLength + 1;
    end;
    for I := 56 to 63 do
        ctx.Buffer [I] := (  ctx.Length shr ( (63 - I) * 8 )  ) and $FF;
    if aSize = 256 then
        SHA256_Compress (ctx)
    else
        SHA1_Compress (ctx);
    Result := '';
    if ( aSize = 256 ) then
        for I := 0 to 7 do
            Result := Result + IntToHex (ctx.State [I], 8)
    else
        for I := 0 to 4 do
            Result := Result + IntToHex (ctx.State [I], 8);
end;

function SHA1 (const aValue: String) : String;
var
    ctx    : TSHA256Ctx;
    Buffer : PChar;
begin
    Result := '';
    try
        SHA1Init (ctx);
        try
            Buffer := PChar (aValue);
            SHA256Update ( ctx, Buffer, Length (Buffer), 1 );
        finally
            Result := SHA256Final (ctx,1);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_SHA1,E.Message]);
    end;
end;

function SHA1 (const aValue: PChar) : String;
var
    ctx : TSHA256Ctx;
begin
    Result := '';
    try
        SHA1Init (ctx);
        try
            SHA256Update ( ctx, aValue, Length (aValue), 1 );
        finally
            Result := SHA256Final (ctx,1);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_SHA1,E.Message]);
    end;
end;

function SHA1 (const aStream: TStream;
               const aBlockSize: LongWord = 1024) : String;
var
    ctx    : TSHA256Ctx;
    Buffer : PChar;
    L      : LongWord;
begin
    Result := '';
    try
        with aStream do
        try
            SHA1Init (ctx);
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
                    SHA256Update (ctx,Buffer,L,1);
            end;
        finally
            Dispose (Buffer);
            Buffer := NIL;
            Result := SHA256Final (ctx,1);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_SHA1,E.Message]);
    end;
end;

function SHA1File (const aFileName: String;
                   const aBlockSize: LongWord = 1024) : String;
var
    Stream : TStream;
begin
    Result := '';
    try
        Stream := TFileStream.Create (aFileName,fmOpenRead);
        try
            Result := SHA1 (Stream,aBlockSize);
        finally
            FreeAndNil (Stream);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_SHA1,E.Message]);
    end;
end;

function SHA256 (const aValue: String) : String;
var
    ctx    : TSHA256Ctx;
    Buffer : PChar;
begin
    Result := '';
    try
        SHA256Init (ctx);
        try
            Buffer := PChar (aValue);
            SHA256Update ( ctx, Buffer, Length (Buffer), 256 );
        finally
            Result := SHA256Final (ctx,256);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_SHA256,E.Message]);
    end;
end;

function SHA256 (const aValue: PChar) : String;
var
    ctx : TSHA256Ctx;
begin
    Result := '';
    try
        SHA256Init (ctx);
        try
            SHA256Update ( ctx, aValue, Length (aValue), 256 );
        finally
            Result := SHA256Final (ctx,256);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_SHA256,E.Message]);
    end;
end;

function SHA256 (const aStream: TStream;
                 const aBlockSize: LongWord = 1024) : String;
var
    ctx    : TSHA256Ctx;
    Buffer : PChar;
    L      : LongWord;
begin
    Result := '';
    try
        with aStream do
        try
            SHA256Init (ctx);
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
                    SHA256Update (ctx,Buffer,L,256);
            end;
        finally
            Dispose (Buffer);
            Buffer := NIL;
            Result := SHA256Final (ctx,256);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_SHA256,E.Message]);
    end;
end;

function SHA256File (const aFileName: String;
                     const aBlockSize: LongWord = 1024) : String;
var
    Stream : TStream;
begin
    Result := '';
    try
        Stream := TFileStream.Create (aFileName,fmOpenRead);
        try
            Result := SHA256 (Stream,aBlockSize);
        finally
            FreeAndNil (Stream);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_SHA256,E.Message]);
    end;
end;

function SHA384 (const aValue: String) : String;
var
    ctx    : TSHA512Ctx;
    Buffer : PChar;
begin
    Result := '';
    try
        SHA384Init (ctx);
        try
            Buffer := PChar (aValue);
            SHA512Update ( ctx, Buffer, Length (Buffer) );
        finally
            Result := SHA512Final (ctx,384);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_SHA384,E.Message]);
    end;
end;

function SHA384 (const aValue: PChar) : String;
var
    ctx : TSHA512Ctx;
begin
    Result := '';
    try
        SHA384Init (ctx);
        try
            SHA512Update ( ctx, aValue, Length (aValue) );
        finally
            Result := SHA512Final (ctx,384);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_SHA384,E.Message]);
    end;
end;

function SHA384 (const aStream: TStream;
                 const aBlockSize: LongWord = 1024) : String;
var
    ctx    : TSHA512Ctx;
    Buffer : PChar;
    L      : LongWord;
begin
    Result := '';
    try
        with aStream do
        try
            SHA384Init (ctx);
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
                    SHA512Update (ctx,Buffer,L);
            end;
        finally
            Dispose (Buffer);
            Buffer := NIL;
            Result := SHA512Final (ctx,384);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_SHA384,E.Message]);
    end;
end;

function SHA384File (const aFileName: String;
                     const aBlockSize: LongWord = 1024) : String;
var
    Stream : TStream;
begin
    Result := '';
    try
        Stream := TFileStream.Create (aFileName,fmOpenRead);
        try
            Result := SHA384 (Stream,aBlockSize);
        finally
            FreeAndNil (Stream);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_SHA384,E.Message]);
    end;
end;

function SHA512 (const aValue: String) : String;
var
    ctx    : TSHA512Ctx;
    Buffer : PChar;
begin
    Result := '';
    try
        SHA512Init (ctx);
        try
            Buffer := PChar (aValue);
            SHA512Update ( ctx, Buffer, Length (Buffer) );
        finally
            Result := SHA512Final (ctx,512);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_SHA512,E.Message]);
    end;
end;

function SHA512 (const aValue: PChar) : String;
var
    ctx : TSHA512Ctx;
begin
    Result := '';
    try
        SHA512Init (ctx);
        try
            SHA512Update ( ctx, aValue, Length (aValue) );
        finally
            Result := SHA512Final (ctx,512);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_SHA512,E.Message]);
    end;
end;

function SHA512 (const aStream: TStream;
                 const aBlockSize: LongWord = 1024) : String;
var
    ctx    : TSHA512Ctx;
    Buffer : PChar;
    L      : LongWord;
begin
    Result := '';
    try
        with aStream do
        try
            SHA512Init (ctx);
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
                    SHA512Update (ctx,Buffer,L);
            end;
        finally
            Dispose (Buffer);
            Buffer := NIL;
            Result := SHA512Final (ctx,512);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_SHA512,E.Message]);
    end;
end;

function SHA512File (const aFileName: String;
                     const aBlockSize: LongWord = 1024) : String;
var
    Stream : TStream;
begin
    Result := '';
    try
        Stream := TFileStream.Create (aFileName,fmOpenRead);
        try
            Result := SHA512 (Stream,aBlockSize);
        finally
            FreeAndNil (Stream);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_SHA512,E.Message]);
    end;
end;
