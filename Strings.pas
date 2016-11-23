unit Strings;
{******************************************************************************}
{*  Unit for working with strings                                             *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: coban & black.rabbit 2010                                     *}
{******************************************************************************}
interface

{$I 'std.inc'}

uses
    Windows, SysUtils, Classes, DateUtils,
    Utils, Versions;

function EscapeString (const aValue: String) : String;

function StrReplace (const Source, Search, Replace: String;
                     const aCaseSensetive: Boolean = TRUE) : String;
function _Trim (const Dest: String; Value: Char) : String; overload;
function _Trim (const Dest: String; Value: Integer) : String; overload;
procedure _FillChar (var Dest: String; count: Integer; Value: Char); overload;
procedure _FillChar (var Dest: String; count: Integer; Value: Integer); overload;
procedure _FillChar (var Dest: ShortString; count: Integer; Value: Char); overload;
procedure _FillChar (var Dest: ShortString; count: Integer; Value: Integer); overload;
function LPad (const Source: String; count: Integer; Value: Char) : String; overload;
function RPad (const Source: String; count: Integer; Value: Char) : String; overload;

function isEmpty (const aValue: String) : Boolean; overload;
function notEmpty (const aValue: String) : Boolean; overload;

function isNull (const aValue: String) : Boolean; overload;
function notNull (const aValue: String) : Boolean; overload;

function Invert (const aValue: String) : String; overload;
function LastPos (const substr: String; const str: String) : Integer; overload;

function _DateTimeToStr (const aDateTime: TDateTime) : String;
function _StrToDateTime (const aValue: String) : TDateTime;
function _DateToStr (const aDate: TDateTime; Long: Boolean = FALSE) : String;
function _StrToDate (const aValue: String) : TDateTime;
function _TimeToStr (const aTime: TDateTime) : String;
function _StrToTime (const aValue: String) : TDateTime;

function _FloatToStr (const aValue: Extended) : String;
function _StrToFloat (const aValue: String) : Extended;
function _TryStrToFloat (const S: string; out Value: Extended) : Boolean;

function _BooleanToStr (const aValue: Boolean; Long: Boolean = TRUE) : String;
function _StrToBoolean (const aValue: String) : Boolean;

procedure _Trim (Dest: TStream; Value: Char); overload;
procedure _Trim (Dest: TStream; Value: Integer); overload;

procedure _TrimW (Dest: TStream; const WSymbols: ShortString); overload;
procedure _TrimB (Dest: TStream; const BSymbols: ShortString); overload;

procedure WriteStrL (aStream: TStream;
                     const aValue: String); overload;
function ReadStrL (const aStream: TStream) : String; overload;

procedure WriteStreamL (aStream: TStream;
                        const anInput: TStream);overload;
procedure ReadStreamL (const aStream: TStream;
                       out anOutput: TStream); overload;

{ функции для работы с 16-ричными данными }
{$I 'Hex.int.inc'}

{$I 'Hex.err.inc'}

implementation

{$I 'Hex.imp.inc'}

function isEmpty (const aValue: String) : Boolean;
begin
    Result := ( aValue = '' );
end;

function notEmpty (const aValue: String) : Boolean;
begin
    Result := ( aValue <> '' );
end;

function isNull (const aValue: String) : Boolean;
begin
    Result := not notNull (aValue);
end;

function notNull (const aValue: String) : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 1 to Length (aValue) do
    begin
        Result := Result and ( aValue [I] <> Char ($00) );
        if not Result then
            Break;
    end;
end;

function _Trim (const Dest: String; Value: Char) : String;
var
    I     : Integer;
    L     : Integer;
    Count : Integer;
begin
    Result := Dest;
    try
        L := Length (Dest);
        Count := 0;
        for I := 1 to L do
            if ( Result [I] = Value ) then
                Inc (Count)
            else
                Break;
        Result := Copy (Result,Count,L);
        L := Length (Dest);
        Count := 0;
        for I := L downto 1 do
            if ( Result [I] = Value ) then
                Inc (Count)
            else
                Break;
        Result := Copy (Result,1,L-Count);
    finally
        L := 0;
    end;
end;

function _Trim (const Dest: String; Value: Integer) : String;
begin
    Result := _Trim ( Dest, Char (Value) );
end;

procedure _Trim (Dest: TStream; Value: Char);
var
    Stream : TStream;
    Buffer : PChar;
    L      : Integer;
begin
    Stream := TMemoryStream.Create;
    Buffer := AllocMem ( SizeOf (Value) + 1 );
    try
        L := Dest.Size;
        Dest.Position := 0;
        while Dest.Position < Dest.Size do
            if not (   (  Dest.Read ( Buffer^, SizeOf (Value) ) > 0  ) and
                       (  Buffer^ = Value  )   ) then
                Break;
        Dest.Position := 0;
        Stream.CopyFrom (Dest,L);
        Dest.Size := 0;
        Stream.Position := 0;
        Dest.CopyFrom (Stream,Stream.Size);
        Dest.Position := Dest.Size;
        L := Dest.Size;
        while Dest.Position > 0 do
        begin
            L := Dest.Position;
            Dest.Seek ( -1 * SizeOf (Value), soCurrent );
            if not (   (  Dest.Read ( Buffer^, SizeOf (Value) ) > 0  ) and
                       (  Dest.Seek ( -1 * SizeOf (Value), soCurrent ) <> 0  ) and
                       (  Buffer^ = Value  )   ) then
                Break;
        end;
        Dest.Position := 0;
        TMemoryStream (Stream).Clear;
        Stream.CopyFrom (Dest,L);
        Dest.Size := 0;
        Stream.Position := 0;
        Dest.CopyFrom (Stream,Stream.Size);
    finally
        if Assigned (Stream) then
            TMemoryStream (Stream).Clear;
        FreeAndNil (Stream);
        ZeroMemory ( Buffer, SizeOf (Value) + 1 );
        Dispose (Buffer);
        L := 0;
    end;
end;

procedure _Trim (Dest: TStream; Value: Integer);
begin
    _Trim ( Dest, Char (Value) );
end;

{ удаляет крайние символы, не входящие в белый список }
procedure _TrimW (Dest: TStream; const WSymbols: ShortString);
var
    Stream : TStream;
    Buffer : PChar;
    L      : Integer;
begin
    Stream := TMemoryStream.Create;
    Buffer := AllocMem ( SizeOf (Char) + 1 );
    try
        L := Dest.Size;
        Dest.Position := 0;
        while Dest.Position < Dest.Size do
            if not (   (  Dest.Read ( Buffer^, SizeOf (Char) ) > 0  ) and
                   not (  Pos (Buffer^,WSymbols) > 0  )   ) then
                Break;
        Dest.Position := 0;
        Stream.CopyFrom (Dest,L);
        Dest.Size := 0;
        Stream.Position := 0;
        Dest.CopyFrom (Stream,Stream.Size);
        Dest.Position := Dest.Size;
        L := Dest.Size;
        while Dest.Position > 0 do
        begin
            L := Dest.Position;
            Dest.Seek ( -1 * SizeOf (Char), soCurrent );
            if not (   (  Dest.Read ( Buffer^, SizeOf (Char) ) > 0  ) and
                       (  Dest.Seek ( -1 * SizeOf (Char), soCurrent ) <> 0  ) and
                   not (  Pos (Buffer^,WSymbols) > 0  )   ) then
                Break;
        end;
        Dest.Position := 0;
        TMemoryStream (Stream).Clear;
        Stream.CopyFrom (Dest,L);
        Dest.Size := 0;
        Stream.Position := 0;
        Dest.CopyFrom (Stream,Stream.Size);
    finally
        if Assigned (Stream) then
            TMemoryStream (Stream).Clear;
        FreeAndNil (Stream);
        ZeroMemory ( Buffer, SizeOf (Char) + 1 );
        Dispose (Buffer);
        L := 0;
    end;
end;

{ удаляет крайние символы, входящие в черный список }
procedure _TrimB (Dest: TStream; const BSymbols: ShortString);
var
    Stream : TStream;
    Buffer : PChar;
    L      : Integer;
begin
    Stream := TMemoryStream.Create;
    Buffer := AllocMem ( SizeOf (Char) + 1 );
    try
        L := Dest.Size;
        Dest.Position := 0;
        while Dest.Position < Dest.Size do
            if not (   (  Dest.Read ( Buffer^, SizeOf (Char) ) > 0  ) and
                       (  Pos (Buffer^,BSymbols) > 0  )   ) then
                Break;
        Dest.Position := 0;
        Stream.CopyFrom (Dest,L);
        Dest.Size := 0;
        Stream.Position := 0;
        Dest.CopyFrom (Stream,Stream.Size);
        Dest.Position := Dest.Size;
        L := Dest.Size;
        while Dest.Position > 0 do
        begin
            L := Dest.Position;
            Dest.Seek ( -1 * SizeOf (Char), soCurrent );
            if not (   (  Dest.Read ( Buffer^, SizeOf (Char) ) > 0  ) and
                       (  Dest.Seek ( -1 * SizeOf (Char), soCurrent ) <> 0  ) and
                       (  Pos (Buffer^,BSymbols) > 0  )   ) then
                Break;
        end;
        Dest.Position := 0;
        TMemoryStream (Stream).Clear;
        Stream.CopyFrom (Dest,L);
        Dest.Size := 0;
        Stream.Position := 0;
        Dest.CopyFrom (Stream,Stream.Size);
    finally
        if Assigned (Stream) then
            TMemoryStream (Stream).Clear;
        FreeAndNil (Stream);
        ZeroMemory ( Buffer, SizeOf (Char) + 1 );
        Dispose (Buffer);
        L := 0;
    end;
end;

procedure _FillChar (var Dest: String; count: Integer; Value: Char);
var
    I : Integer;
begin
    for I := 1 to count-1 do
        Dest [I] := Value;
end; 

procedure _FillChar (var Dest: String; count: Integer; Value: Integer);
begin
    _FillChar ( Dest, count, Char (Value) );
end;

procedure _FillChar (var Dest: ShortString; count: Integer; Value: Char);
var
    I : Integer;
begin
    for I := 1 to count-1 do
        Dest [I] := Value;
end; 

procedure _FillChar (var Dest: ShortString; count: Integer; Value: Integer);
begin
    _FillChar ( Dest, count, Char (Value) );
end;

function LPad (const Source: String; count: Integer; Value: Char) : String;
var
    I : Integer;
    L : Integer;
begin
    Result := Copy (Source,1,count);
    L := Length (Source);
    for I := 1 to count-L do
        Result := Value + Result;
end;

function RPad (const Source: String; count: Integer; Value: Char) : String;
var
    I : Integer;
    L : Integer;
begin
    Result := Copy (Source,1,count);
    L := Length (Source);
    for I := L to count-1 do
        Result := Result + Value;
end;

function StrReplace (const Source, Search, Replace: String;
                     const aCaseSensetive: Boolean = TRUE) : String;
var
    Buf1   : String;
    Buf2   : String;
    Buffer : String;
begin
    Result := Source;
    Buf1 := '';
    Buf2 := Source;
    Buffer := Source;
    while ( Pos (Search, Buf2) > 0 ) do
    begin
        Buf2 := Copy (  Buf2, Pos (Search, Buf2), ( Length (Buf2) - Pos (Search, Buf2) ) + 1  );
        Buf1 := Copy  ( Buffer, 1, Length (Buffer) - Length (Buf2) ) + Replace;
        Delete ( Buf2, Pos (Search, Buf2), Length (Search) );
        Buffer := Buf1 + Buf2;
    end;
    if not aCaseSensetive then
    begin
        Buffer := StrReplace ( Buffer, UpperCase (Search), Replace );
        Buffer := StrReplace ( Buffer, LowerCase (Search), Replace );
    end;
    Result := Buffer;
end;

function EscapeString (const aValue: String) : String;
begin
    Result := Trim (aValue);
    Result := StrReplace (Result,'\','\\');
end;

function _FloatToStr (const aValue: Extended) : String;
begin
    Result := SysUtils.FloatToStr (aValue,FORMAT_SETTINGS);
end;

function _StrToFloat (const aValue: String) : Extended;
begin
    Result := SysUtils.StrToFloat (aValue,FORMAT_SETTINGS);
end;

function _TryStrToFloat (const S: string; out Value: Extended) : Boolean;
begin
    Result := SysUtils.TryStrToFloat (S,Value,FORMAT_SETTINGS);
end;

function DateToStrLong (const aDate: TDateTime) : String;
var
    y : WORD;
    m : WORD;
    d : WORD;
begin
    DecodeDate (aDate,y,m,d);
    Result := Format ('%d %s %d г.',[ d, FORMAT_SETTINGS.LongMonthNames [m], y ]);
end;

function _DateTimeToStr (const aDateTime: TDateTime) : String;
begin
    Result := SysUtils.DateTimeToStr (aDateTime,FORMAT_SETTINGS)
end;

function _StrToDateTime (const aValue: String) : TDateTime;
begin
    Result := SysUtils.StrToDateTime (aValue,FORMAT_SETTINGS);
end;

function _DateToStr (const aDate: TDateTime; Long: Boolean = FALSE) : String;
begin
    if Long then
        Result := DateToStrLong (aDate)
    else
        Result := SysUtils.DateToStr (aDate,FORMAT_SETTINGS);
end;

function _StrToDate (const aValue: String) : TDateTime;
begin
    Result := SysUtils.StrToDate (aValue,FORMAT_SETTINGS);
end;

function _TimeToStr (const aTime: TDateTime) : String;
begin
    Result := SysUtils.TimeToStr (aTime,FORMAT_SETTINGS);
end;

function _StrToTime (const aValue: String) : TDateTime;
begin
    Result := SysUtils.StrToTime (aValue,FORMAT_SETTINGS);
end;

function _BooleanToStr (const aValue: Boolean; Long: Boolean = TRUE) : String;
begin
    if Long then    
        Result := Utils.BooleanToStr (aValue)
    else
        Result := BOOLEAN_CHAR [aValue];
end;

function _StrToBoolean (const aValue: String) : Boolean;
begin
    Result := Utils.StrToBoolean (aValue);
end;

function Invert (const aValue: String) : String;
var
    I : Integer;
    L : Integer;
begin
    Result := '';
    L := Length (aValue);
    SetLength (Result,L);
    for I := 1 to L do
        Result [I] := aValue [L-I+1];
end;

function LastPos (const substr: String; const str: String) : Integer;
var
    P : Integer;
begin
    P := Pos ( Invert (substr), Invert (str) );
    if ( P > 0 ) then
        Result := Length (str) - Length (substr) - P + 2
    else
        Result := 0;
end;

procedure WriteStrL (aStream: TStream;
                     const aValue: String);
var
    Buf  : PChar;
    LBuf : PChar;
    L    : LongWord;
begin
    with aStream do
    begin
        L := Length (aValue);
        LBuf := PChar ( IntToHex (L,4) );
        WriteBuffer (LBuf^,4);
        if ( L > 0 ) then
        begin
            Buf := PChar (aValue);
            WriteBuffer (Buf^,L);
        end;
    end;
end;

function ReadStrL (const aStream: TStream) : String;
var
    Buf  : PChar;
    LBuf : PChar;
    L    : LongInt;
    P    : LongWord;
begin
    Result := '';
    with aStream do
    try
        L := -1;
        try
            P := Position;
            LBuf := AllocMem ( SizeOf (Char) * 4 +1 );
            if ( Read (LBuf^,4) > 0 ) then
                L := HexToInt (LBuf);
            P := Position;
            if ( (Size - Position) < L ) then
                raise Exception.Create (ERR_STREAM_EOF);
            Buf := AllocMem ( SizeOf (Char) * L +1 );
            if ( Read (Buf^,L) > 0 ) then
                Result := StrPas (Buf);
        finally
            Dispose (LBuf);
            Dispose (Buf);
        end;
    except on E: Exception do
        if not Assigned (aStream) then
            raise
        else
            raise Exception.CreateFmt ('%s : Size = %d; Position = %d; L = %d; ',
                                       [ E.Message, aStream.Size, P, L]);
    end;
end;

procedure WriteStreamL (aStream: TStream;
                        const anInput: TStream);
var
    LBuf : PChar;
    L    : LongWord;
begin
    with aStream do
    begin
        L := anInput.Size;
        LBuf := PChar ( IntToHex (L,8) );
        WriteBuffer (LBuf^,8);
        if ( L > 0 ) then
        begin
            anInput.Position := 0;
            CopyFrom (anInput,L);
        end;
    end;
end;

procedure ReadStreamL (const aStream: TStream;
                       out anOutput: TStream);
var
    LBuf : PChar;
    L    : LongInt;
    P    : LongWord;
begin
    with aStream do
    try
        L := -1;
        try
            P := Position;
            LBuf := AllocMem ( SizeOf (Char) * 8 +1 );
            if ( Read (LBuf^,8) > 0 ) then
                L := HexToInt (LBuf);
            if ( L > 0 ) then
            begin
                P := Position;
                anOutput.Position := 0;
                anOutput.CopyFrom (aStream,L);
            end;
        finally
            Dispose (LBuf);
        end;
    except on E: Exception do
        if not Assigned (aStream) then
            raise
        else
            raise Exception.CreateFmt ('%s : Size = %d; Position = %d; L = %d; ',
                                       [ E.Message, aStream.Size, P, L]);
    end;
end;

end.
