{******************************************************************************}
{*  Hex.imp.inc                                                               *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
function IntToHex (Int: Int64; IntSize: Byte) : Hex;
var
    I : LongWord;
begin
    Result := '';
    for I := 0 to IntSize - 1 do
    begin
        Result := HexChars [Int and $F] + Result;
        Int := Int shr $4;
    end;
end;

function HexToInt (const aValue: Hex) : Int64;
begin
    Result := 0;
    if IsHex (aValue) then
        Result := StrToInt ( Format ('$%s',[aValue]) )
    else
        raise Exception.CreateFmt (ERR_HEX_INCORRECT_HEX,[aValue]);
end;

function StrToHex (const aValue: String) : Hex;
var
    I : Integer;
begin
    Result := '';
    for I := 1 to Length (aValue) do
    begin
        Result := Result + IntToHex ( Ord (aValue [I]), 2 );
    end;
end;

function HexToStr (const aValue: Hex) : String;
var
    I      : Integer;
    Buffer : String [3];
begin
    Result := '';
    if IsHex (aValue) then
    begin
        I := 0;
        while ( I < Length (aValue) ) do
        begin
            Inc (I);
            if ( I mod 2 = 0 ) then
            begin
                Buffer := Format ('$%s%s',[ aValue [I-1], aValue [I] ]);
                Result := Result + Char ( StrToInt (Buffer) );
            end;
        end;
    end
    else
        raise Exception.CreateFmt (ERR_HEX_INCORRECT_HEX,[aValue]);
end;

function IsHex (const aValue: String) : Boolean;
var
    I         : Integer;
    J         : Integer;
    IsHexChar : Boolean;
begin
    Result := TRUE;
    for I := 1 to Length (aValue) do
    begin
        IsHexChar := FALSE;
        for J := 0 to 15 do
        begin
            if ( aValue [I] = HexChars [J] ) then
                IsHexChar := TRUE;
        end;
        Result := Result and IsHexChar;
        if not Result then
            Break;
    end;
end;

procedure StreamToHexStream (const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: WORD = 1024);
var
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    Buffer       : String;
    L            : WORD;
begin
    try
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            L := aBlockSize;
            InputBuffer := AllocMem (L+1);
            while ( Position < Size ) do
            begin
                if ( (Size - Position) < aBlockSize ) then
                begin
                    L := Size - Position;
                    Dispose (InputBuffer);
                    InputBuffer := NIL;
                    InputBuffer := AllocMem (L+1);
                end;
                if ( Read (InputBuffer^,L) > 0 ) then
                try
                    Buffer := StrPas (InputBuffer);
                    Buffer := StrToHex (Buffer);
                    OutputBuffer := PChar (Buffer);
                    anOutput.WriteBuffer ( OutputBuffer^, L*2 );
                finally
                    FillChar ( OutputBuffer^, L*2, $00 );
                    _FillChar ( Buffer, Length (Buffer), $00 );
                end;
            end;
        finally
            Dispose (InputBuffer);
            InputBuffer := NIL;
            L := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',[ERR_STREAM_TO_HEX_STREAM,E.Message]);
    end;
end;

procedure HexStreamToStream (const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: WORD = 1024);
var
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    Buffer       : String;
    L            : WORD;
begin
    try
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            L := aBlockSize;
            InputBuffer := AllocMem (L+1);
            while ( Position < Size ) do
            begin
                if ( (Size - Position) < aBlockSize ) then
                begin
                    L := Size - Position;
                    Dispose (InputBuffer);
                    InputBuffer := NIL;
                    InputBuffer := AllocMem (L+1);
                end;
                if ( Read (InputBuffer^,L) > 0 ) then
                try
                    Buffer := StrPas (InputBuffer);
                    if ( L mod 2 <> 0 ) then
                        raise Exception.CreateFmt (ERR_HEX_INCORRECT_HEX,[Buffer]);
                    Buffer := HexToStr (Buffer);
                    OutputBuffer := PChar (Buffer);
                    anOutput.WriteBuffer ( OutputBuffer^, (L div 2) );
                finally
                    FillChar ( OutputBuffer^, (L div 2), $00 );
                    _FillChar ( Buffer, Length (Buffer), $00 );
                end;
            end;
        finally
            Dispose (InputBuffer);
            InputBuffer := NIL;
            L := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',[ERR_HEX_STREAM_TO_STREAM,E.Message]);
    end;
end;

procedure ByteStreamToHexStream (const anInput: TStream;
                                 out anOutput: TStream);
var
    InputBuffer  : Byte;
    OutputBuffer : PChar;
    Buffer       : String;
    L            : WORD;
begin
    try
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            L := 1;
            while ( Position < Size ) do
            begin
                if ( Read (InputBuffer,L) > 0 ) then
                try
                    Buffer := IntToHex (InputBuffer,2);
                    OutputBuffer := PChar (Buffer);
                    anOutput.WriteBuffer ( OutputBuffer^, L*2 );
                finally
                    OutputBuffer := NIL;
                    _FillChar ( Buffer, Length (Buffer), $00 );
                end;
            end;
        finally
            L := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',[ERR_STREAM_TO_HEX_STREAM,E.Message]);
    end;
end;

procedure HexStreamToByteStream (const anInput: TStream;
                                 out anOutput: TStream);
var
    InputBuffer  : PChar;
    OutputBuffer : Byte;
    Buffer       : String;
    L            : WORD;
begin
    try
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            L := 2;
            InputBuffer := AllocMem (L+1);
            while ( Position < Size ) do
            begin
                if ( (Size - Position) < 2 ) then
                begin
                    L := Size - Position;
                    Dispose (InputBuffer);
                    InputBuffer := NIL;
                    InputBuffer := AllocMem (L+1);
                end;
                if ( Read (InputBuffer^,L) > 0 ) then
                try
                    Buffer := StrPas (InputBuffer);
                    if ( L mod 2 <> 0 ) then
                        raise Exception.CreateFmt (ERR_HEX_INCORRECT_HEX,[Buffer]);
                    OutputBuffer := HexToInt (Buffer);
                    anOutput.WriteBuffer ( OutputBuffer, (L div 2) );
                finally
                    OutputBuffer := $00;
                    _FillChar ( Buffer, Length (Buffer), $00 );
                end;
            end;
        finally
            Dispose (InputBuffer);
            InputBuffer := NIL;
            L := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',[ERR_HEX_STREAM_TO_STREAM,E.Message]);
    end;
end;

function IsHexStream (const aStream: TStream;
                      const aBlockSize: WORD = 1024) : Boolean;
var
    Buffer : PChar;
    L      : WORD;
begin
    Result := TRUE;
    try
        with aStream do
        try
            aStream.Position := 0;
            L := aBlockSize;
            Buffer := AllocMem (L+1);
            while ( Position < Size-1 ) do
            begin
                if ( (Size - Position) < aBlockSize ) then
                begin
                    L := Size - Position;
                    Dispose (Buffer);
                    Buffer := NIL;
                    Buffer := AllocMem (L+1);
                end;
                if ( Read (Buffer^,L) > 0 ) then
                try
                    Result := Result and IsHex ( StrPas (Buffer) );
                    if not Result then
                        Exit;
                finally
                    FillChar (Buffer^,L,$00);
                end;
            end;
        finally
            Dispose (Buffer);
            Buffer := NIL;
            L := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',[ERR_IS_HEX_STREAM,E.Message]);
    end;
end;

function DateTimeToHex (const aValue: TDateTime) : Hex;
begin
    Result := '';
    Result := IntToHex (  DateTimeToUnix ( Trunc (aValue) ), 16  );
end;

function HexToDateTime (const aValue: Hex) : TDateTime;
begin
    Result := 0.0;
    Result := UnixToDateTime ( HexToInt (aValue) );
end;

function FloatToHex (const aValue: Extended; Size: Byte) : Hex;
var
    Hi : Cardinal;
    Lo : Cardinal;
begin
    Result := '0';
    try
        FloatToParts (aValue,Hi,Lo);
        Result := Format ('%s%s',[ IntToHex ( Hi, Size div 2 ), IntToHex ( Lo, Size div 2 ) ]);
    finally
        Hi := 0;
        Lo := 0;
    end;
end;

function HexToFloat (const aValue: Hex) : Extended;
var
    Hi    : Cardinal;
    Lo    : Cardinal;
    HiHex : Hex;
    LoHex : Hex;
begin
    Result := 0;
    try
        HiHex := Copy ( aValue, 1, Length (aValue) div 2 );
        Hi := HexToInt (HiHex);
        LoHex := Copy ( aValue, Length (aValue) div 2 + 1, Length (aValue) );
        Lo := HexToInt (LoHex);
        Result := PartsToFloat (Hi,Lo);
    finally
        _FillChar ( HiHex, Length (HiHex), $00 );
        Hi := 0;
        _FillChar ( LoHex, Length (LoHex), $00 );
        Lo := 0;
    end;
end;

function VersionInfoToHex (const aValue: TVersionInfo) : Hex;
begin
    Result := Format ('%s%s%s%s',[ IntToHex (aValue.Major,2),
                                   IntToHex (aValue.Minor,2),
                                   IntToHex (aValue.Release,2),
                                   IntToHex (aValue.Build,2) ]);
end;

function HexToVersionInfo (const aValue: Hex) : TVersionInfo;
begin
    Result := NULL_VERSION;
    try
        Result.Major   := HexToInt ( Copy (aValue,1,2) );
        Result.Minor   := HexToInt ( Copy (aValue,3,2) );
        Result.Release := HexToInt ( Copy (aValue,5,2) );
        Result.Build   := HexToInt ( Copy (aValue,7,2) );
    except
        Result := NULL_VERSION;
    end;
end;
