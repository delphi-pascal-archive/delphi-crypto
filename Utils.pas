unit Utils;
{******************************************************************************}
{*  Unit with Utils Functions                                                 *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2010                                             *}
{******************************************************************************}
interface

{$I 'std.inc'}

uses
    Windows, SysUtils, DateUtils, Classes,
    zlib,
    {$I 'gzlib/gzlib.uses.inc'}
    Graphics, jpeg, pngimage,
    Versions;

{ Format }
{$I 'months.inc'}
{$I 'days.inc'}

const
    FORMAT_MOTHS_NAMES : array [1..12] of String = (
        MonthJan,
        MonthFeb,
        MonthMar,
        MonthApr,
        MonthMay,
        MonthJun,
        MonthJul,
        MonthAug,
        MonthSep,
        MonthOct,
        MonthNov,
        MonthDec
    );

    FORMAT_DAYS_NAMES : array [1..7] of String = (
        WeekDay1,
        WeekDay2,
        WeekDay3,
        WeekDay4,
        WeekDay5,
        WeekDay6,
        WeekDay7
    );

    FORMAT_SETTINGS : TFormatSettings = (
        ThousandSeparator : ' ';
        DecimalSeparator  : '.';
        DateSeparator     : '.';
        TimeSeparator     : ':';
        ListSeparator     : ',';
        ShortDateFormat   : 'dd.mm.yyyy';
        LongDateFormat    : 'dd mmmm yyyy г.';
        ShortTimeFormat   : 'hh:mm:ss';
        LongTimeFormat    : 'hh:mm:ss';
        ShortMonthNames   : ( Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec);
        LongMonthNames    : ( JanL, FebL, MarL, AprL, MayL, JunL, JulL, AugL, SepL, OctL, NovL, DecL);
        ShortDayNames     : ( WD1, WD2, WD3, WD4, WD5, WD6, WD7 );
        LongDayNames      : ( WeekDay1, WeekDay2, WeekDay3, WeekDay4, WeekDay5, WeekDay6, WeekDay7 );
    );

    UNIX_START_DATE : TDateTime = 25569.0;

resourcestring
    GOOD_NIGHT   = 'Доброй ночи';
    GOOD_MORNING = 'Доброе утро';
    GOOD_DAY     = 'Добрый день';
    GOOD_EVENING = 'Добрый вечер';

function GetGood (const aValue: TDateTime) : String;

{ Float }
type
    FloatInt64 = record
        case Integer of
            0 : ( asInt64 : Int64  );
            1 : ( asFloat : Double );
            2 : ( asParts : packed record
                                Hi : LongWord;
                                Lo : LongWord;
                            end; );
    end;

function FloatToInt64 (const aValue: Extended) : Int64;
function Int64ToFloat (const aValue: Int64) : Extended;

procedure FloatToParts (const aValue: Extended; out Hi, Lo : Cardinal);
function PartsToFloat (const Hi, Lo : Cardinal) : Extended;

{ Boolean }
const
    BOOLEAN_STRING     : array [FALSE..TRUE] of String  = ('FALSE', 'TRUE');
    BOOLEAN_CHAR       : array [FALSE..TRUE] of String  = ('F', 'T');
    SHORT_BOOLEAN_CHAR : array [FALSE..TRUE] of String  = ('0', '1');
    SHORT_BOOLEAN_INT  : array [FALSE..TRUE] of Integer = (0, 1);

function StrToBoolean (const aValue: String) : Boolean;
function BooleanToStr (const aValue: Boolean) : String; 

function IntToBoolean (const aValue: Integer) : Boolean;
function BooleanToInt (const aValue: Boolean) : Integer;

{ Pointer }
function _Dispose (const anAddress: LongWord) : Boolean;
function GetPBoolean (const aValue: Boolean) : PBoolean;
function GetPInteger (const aValue: Integer) : PInteger;
function GetPInt64 (const aValue: Int64) : PInt64;
function GetPExtended (const aValue: Extended) : PExtended;
function GetPDouble (const aValue: Double) : PDouble;
function GetPDateTime (const aValue: TDateTime) : PDateTime;
function GetPVersionInfo (const aValue: TVersionInfo) : PVersionInfo;

{ Stream }
function StreamToStr (const aStream: TStream) : String;
procedure StrToStream (aStream: TStream;
                       const aValue: String);

{ compress }
procedure Compress (const anInput: TStream;
                    out anOutput: TStream); overload;
function Compress (const aValue: String) : String; overload;
procedure Decompress (const anInput: TStream;
                      out anOutput: TStream); overload;
function Decompress (const aValue: String) : String; overload;

{ files }
function GetFileSize (const aFileName: String) : Integer;
function ValidateFileSize (const aFileName: String;
                           const aMaxSize: Integer = 16) : Boolean;
function NormalPath (const aPath: String) : String;
function GetFiles (const aDir: String;
                   out aFiles: TStrings;
                   const doRecurse: Boolean;
                   anExt: array of const) : Integer; overload;
function GetFiles (const aDir: String;
                   out aFiles: TStrings;
                   const doRecurse: Boolean = FALSE) : Integer; overload;

{ images }
function ResizeImage (aBitmap: TBitmap;
                      aWidth, aHeight: Integer;
                      aBorderWidth: Integer = 0;
                      aBkColor: TColor = clNone) : Boolean;

{ ip }
type
    TIPAddress = record
        Oct1, Oct2, Oct3, Oct4 : Byte;
    end;

function StrToIP (const aValue: String) : TIPAddress; overload;
function IPToStr (const anIP: TIPAddress) : String; overload;
function IPToCardinal (const anIP: TIPAddress) : Cardinal; overload;
function CardinalToIP (const aValue: Cardinal) : TIPAddress; overload;
function isIPAddress (const aValue: String) : Boolean; overload;

resourcestring
    ERR_COMPRESS_DATA           = 'Ошибка компрессии данных!';
    ERR_DECOMPRESS_DATA         = 'Ошибка декомпрессии данных!';
    ERR_FILE_NOT_FOUND          = 'Файл ''%s'' не найден!';
    ERR_GET_FILES               = 'Ошибка извлечения списка файлов директории ''%s''!';
    ERR_LARGE_FILE              = 'Размер выбранного файла ( %.2f KB ) превышает лимит %d KB';
    ERR_IMAGE_RESIZE            = 'Ошибка изменения размера изображения: %s';
    ERR_STREAM_EOF              = 'Достигнут конец потока!';
    ERR_INVALID_LIST            = 'Некорректный список!';
    ERR_INVALID_DATA_SIZE       = 'Некорректный размер данных: %d!';
    ERR_INVALID_COMPRESS_DATA   = 'Некорректные буффер компрессора!';
    ERR_INVALID_DECOMPRESS_DATA = 'Некорректные буффер декомпрессора!';

implementation

uses
    VarRecs, Strings;

function GetGood (const aValue: TDateTime) : String;
var
    h : Byte;
begin
    Result := '';
    h := HourOf (aValue);
    case h of
        23..24,
        0..4    : Result := GOOD_NIGHT;
        5..11   : Result := GOOD_MORNING;
        12..16  : Result := GOOD_DAY;
        17..22  : Result := GOOD_EVENING;
    end;
end;

function StrToBoolean (const aValue: String) : Boolean;
var
    Value : String;
begin
    Value := UpperCase (aValue);
    Result := (  ( Value = BOOLEAN_STRING [TRUE] )
              or ( Value = BOOLEAN_CHAR [TRUE] )
              or ( Value = SHORT_BOOLEAN_CHAR [TRUE] )  );
end;

function BooleanToStr (const aValue: Boolean) : String;
begin
    Result := BOOLEAN_STRING [aValue];
end;

function IntToBoolean (const aValue: Integer) : Boolean;
begin
    Result := ( aValue = SHORT_BOOLEAN_INT [TRUE] );
end;

function BooleanToInt (const aValue : Boolean) : Integer;
begin
    Result := SHORT_BOOLEAN_INT [aValue];
end;

{ Float }
function FloatToInt64 (const aValue: Extended) : Int64;
var
    v : FloatInt64;
begin
    result := 0;
    try
        try
            v.asFloat := aValue;
            Result := v.asInt64;
        finally
            v.asInt64 := 0;
        end;
    except
        result := 0;
    end;
end;

function Int64ToFloat (const aValue: Int64) : Extended;
var
    v : FloatInt64;
begin
    result := 0.0;
    try
        try
            v.asInt64 := aValue;
            Result := v.asFloat;
        finally
            v.asInt64 := 0;
        end;
    except
        result := 0.0;
    end;
end;

procedure FloatToParts (const aValue: Extended; out Hi, Lo : Cardinal);
var
    v : FloatInt64;
begin
    Hi := 0;
    Lo := 0;
    try
        try
            v.asFloat := aValue;
            Hi := v.asParts.Hi;
            Lo := v.asParts.Lo;
        finally
            v.asInt64 := 0;
        end;
    except
        Hi := 0;
        Lo := 0;
    end;
end;

function PartsToFloat (const Hi, Lo : Cardinal) : Extended;
var
    v : FloatInt64;
begin
    result := 0.0;
    try
        try
            v.asParts.Hi := Hi;
            v.asParts.Lo := Lo;
            Result := v.asFloat;
        finally
            v.asInt64 := 0;
        end;
    except
        result := 0.0;
    end;
end;

{ Pointer }
function _Dispose (const anAddress: LongWord) : Boolean;
var
    P : Pointer;
begin
    Result := FALSE;
    P := Pointer (anAddress);
    Result := not Assigned (P);
    if not Result then
    try
        try Dispose (P); except end;
        Result := TRUE;
    finally
        P := NIL;
    end;
end;

function GetPBoolean (const aValue: Boolean) : PBoolean;
begin
    Result := AllocMem ( SizeOf (Boolean) );
    Result^ := aValue;
end;

function GetPInteger (const aValue: Integer) : PInteger;
begin
    Result := AllocMem ( SizeOf (Integer) );
    Result^ := aValue;
end;

function GetPInt64 (const aValue: Int64) : PInt64;
begin
    Result := AllocMem ( SizeOf (Int64) );
    Result^ := aValue;
end;

function GetPExtended (const aValue: Extended) : PExtended;
begin
    Result := AllocMem ( SizeOf (Extended) );
    Result^ := aValue;
end;

function GetPDouble (const aValue: Double) : PDouble;
begin
    Result := AllocMem ( SizeOf (Double) );
    Result^ := aValue;
end;

function GetPDateTime (const aValue: TDateTime) : PDateTime;
begin
    Result := AllocMem ( SizeOf (TDateTime) );
    Result^ := aValue;
end;

function GetPVersionInfo (const aValue: TVersionInfo) : PVersionInfo;
begin
    Result := AllocMem ( SizeOf (TVersionInfo) );
    Result^ := aValue;
end;

function StreamToStr (const aStream: TStream) : String;
var
    Reader : TStringStream;
begin
    Result := '';
    Reader := TStringStream.Create ('');
    with Reader do
    try
        aStream.Position := 0;
        Position := 0;
        CopyFrom (aStream,aStream.Size);
        Position := 0;
        Result := ReadString (aStream.Size);
    finally
        FreeAndNil (Reader);
    end;
end;

procedure StrToStream (aStream: TStream;
                       const aValue: String);
var
    Writer : TStringStream;
begin
    Writer := TStringStream.Create (aValue);
    with aStream do
    try
        Writer.Position := 0;
        Position := 0;
        CopyFrom (Writer,Writer.Size);
        Position := 0;
    finally
        FreeAndNil (Writer);
    end;
end;

{$IFNDEF GZIP}

procedure Compress (const anInput: TStream;
                    out anOutput: TStream);
var
    Buf      : Byte;
    InpBuf   : Pointer;
    OutBuf   : Pointer;
    InpBytes : Integer;
    OutBytes : Integer;
begin
    try
        InpBuf := NIL;
        OutBuf := NIL;
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            Buf := $ff;
            anOutput.Write ( Buf, SizeOf (Byte) );
            GetMem (InpBuf,anInput.Size);
            InpBytes := anInput.Read (InpBuf^,anInput.Size);
            if ( InpBytes <= 0 ) then
                raise Exception.CreateFmt (ERR_INVALID_DATA_SIZE,[InpBytes]);
            if not Assigned (InpBuf) then
                raise Exception.Create (ERR_INVALID_DECOMPRESS_DATA);
            CompressBuf (InpBuf,InpBytes,OutBuf,OutBytes);
            if not Assigned (OutBuf) or ( OutBytes <= 0 ) then
                raise Exception.Create (ERR_INVALID_COMPRESS_DATA);
            anOutput.Write (OutBuf^,OutBytes);
            anOutput.Write ( Buf, SizeOf (Byte) );
        finally
            if Assigned (InpBuf) then
                FreeMem (InpBuf);
            if Assigned (OutBuf) then
                FreeMem (OutBuf);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',[ERR_COMPRESS_DATA,E.Message]);
    end;
end;

procedure Decompress (const anInput: TStream;
                      out anOutput: TStream);
var
    Position : LongInt;
    InpBuf   : Pointer;
    OutBuf   : Pointer;
    sz       : Integer;
    OutBytes : Integer;
begin
    try
        InpBuf := NIL;
        OutBuf := NIL;
        try
            //anInput.Position := 0;
            anOutput.Position := 0;
            anInput.Seek ( SizeOf (Byte), soCurrent );
            anInput.Size := anInput.Size - SizeOf (Byte);
            sz := anInput.Size - anInput.Position;
            if ( sz <= 0 ) then
                raise Exception.CreateFmt (ERR_INVALID_DATA_SIZE,[sz]);
            GetMem (InpBuf,sz);
            anInput.Read (InpBuf^,sz);
            if not Assigned (InpBuf) then
                raise Exception.Create (ERR_INVALID_COMPRESS_DATA);
            DecompressBuf (InpBuf,sz,0,OutBuf,OutBytes);
            if not Assigned (OutBuf) or ( OutBytes <= 0 ) then
                raise Exception.Create (ERR_INVALID_DECOMPRESS_DATA);
            anOutput.Write (OutBuf^,OutBytes);
        finally
            if Assigned (InpBuf) then
                FreeMem (InpBuf);
            if Assigned (OutBuf) then
                FreeMem (OutBuf);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',[ERR_DECOMPRESS_DATA,E.Message]);
    end;
end;

{$ELSE GZIP}

procedure Compress (const anInput: TStream;
                    out anOutput: TStream);
var
    Buf : Byte;
begin
    try
        anInput.Position := 0;
        anOutput.Position := 0;
        Buf := $ff;
        anOutput.Write ( Buf, SizeOf (Buf) );
        GZCompressStream (anInput,anOutput);
        anOutput.WriteBuffer ( Buf, SizeOf (Buf) );
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',[ERR_COMPRESS_DATA,E.Message]);
    end;
end;

procedure Decompress (const anInput: TStream;
                      out anOutput: TStream);
begin
    try
        //anInput.Position := 0;
        anInput.Seek ( SizeOf (Byte), soCurrent );
        anInput.Size := anInput.Size - SizeOf (Byte);
        anOutput.Position := 0;
        GZDecompressStream (anInput,anOutput);
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',[ERR_DECOMPRESS_DATA,E.Message]);
    end;
end;

{$ENDIF GZIP}

function Compress (const aValue: String) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    if ( aValue <> '' ) then
    try
        Input := TStringStream.Create (aValue);
        Output := TStringStream.Create ('');
        try
            Compress (Input,Output);
            with TStringStream (Output) do
            begin
                Position := 0;
                Result := ReadString (Size);
            end;
        finally
            FreeAndNil (Output);
            FreeAndNil (Input);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',[ERR_COMPRESS_DATA,E.Message]);
    end;
end;

function Decompress (const aValue: String) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    if ( aValue <> '' ) then
    try
        Input := TStringStream.Create (aValue);
        Output := TStringStream.Create ('');
        try
            Decompress (Input,Output);
            with TStringStream (Output) do
            begin
                Position := 0;
                Result := ReadString (Size);
            end;
        finally
            FreeAndNil (Output);
            FreeAndNil (Input);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',[ERR_DECOMPRESS_DATA,E.Message]);
    end;
end;

function GetFileSize (const aFileName: String) : Integer;
var
    Stream : TStream;
begin
    Result := -1;
    Stream := TFileStream.Create (aFileName,fmOpenRead);
    try
        Result := Stream.Size;
    finally
        FreeAndNIl (Stream);
    end;
end;

function ValidateFileSize (const aFileName: String;
                           const aMaxSize: Integer = 16) : Boolean;
var
    SZ : Integer;
begin
    Result := FALSE;
    if FileExists (aFileName) then
    begin
        SZ := GetFileSize (aFileName);
        if ( SZ > aMaxSize*1024 ) then
            raise Exception.CreateFmt (ERR_LARGE_FILE,[ sz/1024, aMaxSize ]);
    end
    else
        raise Exception.CreateFmt (ERR_FILE_NOT_FOUND,[aFileName]);
    Result := TRUE;
end;

function NormalPath (const aPath: String) : String;
var
    L : Integer;
begin
    L := Length (aPath);
    if ( Length (aPath) > 0 ) then
    begin
        Result := StrReplace (aPath,'/','\');
        if ( Result [L] = '\' ) then
            Result := Copy (Result,1,L-1);
    end;
end;

function GetFiles (const aDir: String;
                   out aFiles: TStrings;
                   const doRecurse: Boolean;
                   anExt: array of const) : Integer;

    function isValidExt (const Ext: String) : Boolean;
    var
        I : Integer;
        S : String;
        P : Integer;
    begin
        Result := FALSE;
        for I := Low (anExt) to High (anExt) do
        begin
            S := Trim ( toString (anExt [I]) );
            P := Pos ('.',S);
            if ( P > 0 ) then
                S := Copy ( S, P, Length (S) );
            S := LowerCase (S);
            Result := ( S = LowerCase (Ext) ) or ( S = '.*' );
            if Result then
                Break;
        end;
    end;

var
    SR  : TSearchRec;
    Dir : String;
begin
    Result := 0;
    try
        if not Assigned (aFiles) then
            raise Exception.Create (ERR_INVALID_LIST);
        Dir := NormalPath (aDir);
        if ( FindFirst (Dir+'\'+'*.*',faAnyFile,SR) = 0 ) then
        try
            repeat
                if ( SR.Name <> '.' ) and ( SR.Name <> '..' ) then
                begin
                    if ( SR.Attr and faDirectory = faDirectory ) and doRecurse then
                        GetFiles (Dir+'\'+SR.Name,aFiles,doRecurse,anExt)
                    else if FileExists (Dir+'\'+SR.Name) and
                            isValidExt ( ExtractFileExt (SR.Name) ) then
                    begin
                        aFiles.Add (Dir+'\'+SR.Name);
                        Inc (Result);
                    end;
                end;
            until ( FindNext (SR) <> 0 );
        finally
            FindClose (SR);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ Format (ERR_GET_FILES,[aDir]), E.Message ]);
    end;
end;

function GetFiles (const aDir: String;
                   out aFiles: TStrings;
                   const doRecurse: Boolean = FALSE) : Integer;
begin
    Result := GetFiles (aDir,aFiles,doRecurse,['*.*']);
end;

function ResizeImage (aBitmap: TBitmap;
                      aWidth, aHeight: Integer;
                      aBorderWidth: Integer = 0;
                      aBkColor: TColor = clNone) : Boolean;
var
    Bmp : TBitmap;
begin
    Result := FALSE;
    try
        Bmp := TBitmap.Create;
        try
            Bmp.SetSize (aWidth,aHeight);
            Bmp.Canvas.Brush.Color := aBkColor;
            Bmp.Canvas.Pen.Color := aBkColor;
            Bmp.Canvas.FillRect ( Rect (0,0,aWidth,aHeight) );
            Bmp.Canvas.StretchDraw ( Rect (aBorderWidth,
                                           aBorderWidth,
                                           aWidth-aBorderWidth,
                                           aHeight-aBorderWidth),
                                     aBitmap );
            aBitmap.Assign (Bmp);
        finally
            FreeAndNil (Bmp);
        end;
        Result := TRUE;
    except on E: Exception do
        raise Exception.CreateFmt (ERR_IMAGE_RESIZE,[E.Message]);
    end;
end;

function StrToIP (const aValue: String) : TIPAddress;
var
    I        : Integer;
    X        : Integer;
    Position : array [1..4] of Integer;
    Oct      : array [1..4] of String;
begin
    X := 0;
    for I := 1 to Length (aValue) do
    begin
        if ( aValue [I] = '.' ) then
        begin
            Inc (X);
            Position [X] := I;
        end
        else
            Oct [X+1] := Oct [X+1] + aValue [I];
    end;
    with Result do
    begin
        Oct1 := StrToInt (Oct [1]);
        Oct2 := StrToInt (Oct [2]);
        Oct3 := StrToInt (Oct [3]);
        Oct4 := StrToInt (Oct [4]);
    end;
end;

function IPToStr (const anIP: TIPAddress) : String;
begin
    Result := '';
    with anIP do
        Result := Format ('%d.%d.%d.%d',[Oct1,Oct2,Oct3,Oct4]);
end;

function IPToCardinal (const anIP: TIPAddress) : Cardinal;
begin
    Result := 0;
    with anIP do
        Result := Oct1*$1000000 +
                  Oct2*$10000 +
                  Oct3*$100 +
                  Oct4;
end;

function CardinalToIP (const aValue: Cardinal) : TIPAddress;
begin
    with Result do
    begin
        Oct1 := aValue div $1000000;
        Oct2 := aValue div $10000;
        Oct3 := aValue div $100;
        Oct4 := aValue mod $100;
    end;
end;

function isIPAddress (const aValue: String) : Boolean;
var
    I        : Integer;
    X        : Integer;
    Position : array [1..4] of Integer;
    Oct      : array [1..4] of String;
begin
    Result := TRUE;
    try
        X := 0;
        for I := 1 to Length (aValue) do
        begin
            Result := Result and ( aValue [I] in ['0'..'9','.'] );
            if not Result then Exit;

            if ( aValue [I] = '.' ) then
            begin
                Inc (X);
                Position [X] := I;
            end
            else
                Oct [X+1] := Oct [X+1] + aValue [I];
        end;
        for X := 1 to 4 do
        begin
            Result := Result and ( StrToInt (Oct [X]) >= 0 ) and ( StrToInt (Oct [X]) <= 255 );
            if not Result then Exit;
        end;
    except
        Result := FALSE;
    end;
end;

initialization
    DecimalSeparator := '.';


end.
