unit Versions;
{******************************************************************************}
{*  Unit for Versions Control                                                 *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2010                                             *}
{******************************************************************************}
interface

{$I 'std.inc'}

uses
    Windows, SysUtils, Classes, Variants, TypInfo;

type
    PVersionInfo = ^TVersionInfo;
    TVersionInfo = packed record
        Major   : WORD;
        Minor   : WORD;
        Release : WORD;
        Build   : WORD;
    end;

const
    NULL_VERSION_STRING = '0.0.0.0';
    NULL_VERSION : TVersionInfo = (
        Major   : 0;
        Minor   : 0;
        Release : 0;
        Build   : 0;
    );

function GetVersionInfo (out Major,
                             Minor,
                             Release,
                             Build: WORD) : Boolean; overload;
function GetVersionInfo : TVersionInfo; overload;

type
    PFileInfo = ^TFileInfo;
    TFileInfo = packed record
        CompanyName      : String;
        FileDescription  : String;
        FileVersion      : String;
        InternalName     : String;
        LegalCopyright   : String;
        LegalTradeMarks  : String;
        OriginalFileName : String;
        ProductName      : String;
        ProductVersion   : String;
        Comments         : String;
    end;

const
    FILE_INFO_COMPANY_NAME       = 0;
    FILE_INFO_FILE_DESCRIPTION   = 1;
    FILE_INFO_FILE_VERSION       = 2;
    FILE_INFO_INTERNAL_NAME      = 3;
    FILE_INFO_LEGAL_COPYRIGHT    = 4;
    FILE_INFO_LEGAL_TRADEMARKS   = 5;
    FILE_INFO_ORIGINAL_FILE_NAME = 6;
    FILE_INFO_PRODUCT_NAME       = 7;
    FILE_INFO_PRODUCT_VERSION    = 8;
    FILE_INFO_COMMENTS           = 9;

function toVersionInfo (const aValue: TVarRec) : TVersionInfo; overload;

function VersionInfoToStr (const aValue: TVersionInfo) : String;
function VersionInfoToInt (const aValue: TVersionInfo) : LongWord;

function GetFileInfo (const aFileName: String) : TFileInfo;
function StrToVersionInfo (const aVersionString: String) : TVersionInfo;
function IntToVersionInfo (const aValue: LongWord) : TVersionInfo;
function GetFileVersionInfo (const aFileName: String) : TVersionInfo;
function GetProductVersionInfo (const aFileName: String) : TVersionInfo;

function ProjectName : String;
function ProductName : String;
function InternalName : String;
function CompanyName : String;
function CopyRight : String;

{ вариантный тип версии }
type
    CVersionVariantType = class of TVersionVariantType;
    PVersionVariantType = ^TVersionVariantType;
    TVersionVariantType = class (TPublishableVariantType, IVarStreamable)
    protected
        function LeftPromotion (const V: TVarData;
                                const Operator: TVarOp;
                                out RequiredVarType: TVarType) : Boolean; override;
        function GetInstance (const V: TVarData) : TObject; override;
    public
        procedure Clear (var V: TVarData); override;
        function IsClear (const V: TVarData) : Boolean; override;
        procedure Copy (var Dest: TVarData;
                        const Source: TVarData;
                        const Indirect: Boolean); override;
        procedure Cast (var Dest: TVarData; const Source: TVarData); override;
        procedure CastTo (var Dest: TVarData;
                          const Source: TVarData;
                          const AVarType: TVarType); override;
        procedure BinaryOp (var Left: TVarData; const Right: TVarData;
                            const Operator: TVarOp); override;
        function CompareOp (const Left: TVarData; const Right: TVarData;
                            const Operator: Integer) : Boolean; override;
        procedure StreamIn (var Dest: TVarData; const Stream: TStream);
        procedure StreamOut (const Source: TVarData; const Stream: TStream);
    end;

var
    VersionVariantType : TVersionVariantType = NIL;

type
    CVersionData = class of TVersionData;
    PVersionData = ^TVersionData;
    TVersionData = class (TPersistent)
    private
        f_Major: WORD;
        f_Minor: WORD;
        f_Release: WORD;
        f_Build: WORD;
    protected
        function GetAsString : String;
        function GetAsInteger : LongWord;
        function GetAsVersion : TVersionInfo;
        procedure SetValue (const aMajor: WORD;
                                  aMinor: WORD = 0;
                                  aRelease: WORD = 0;
                                  aBuild: WORD = 0); overload;
        procedure SetAsInteger (const aValue: LongWord); overload;
        procedure SetAsVersion (const aValue: TVersionInfo); overload;
        procedure SetAsString (const aText: String); overload;
        procedure SetValue (const aData: TVersionData); overload;
    public
        constructor Create (const aMajor: WORD;
                                  aMinor: WORD = 0;
                                  aRelease: WORD = 0;
                                  aBuild: WORD = 0); overload;
        constructor Create (const aValue: TVersionInfo); overload;
        constructor Create (const aText: string); overload;
        constructor Create (const aData: TVersionData); overload;
    public
        function Equals (const aMajor: WORD;
                               aMinor: WORD = 0;
                               aRelease: WORD = 0;
                               aBuild: WORD = 0) : Boolean; overload;
        function Equals (const aValue: TVersionInfo) : Boolean; overload;
        function Equals (const aValue: LongWord) : Boolean; overload;
        function Equals (const aText: string) : Boolean; overload;
        function Equals (const Right: TVersionData) : Boolean; overload;
        function isZero : Boolean;
        function Less (const aValue: TVersionInfo) : Boolean; overload;
        function Less (const aText: string) : Boolean; overload;
        function Less (const aValue: LongWord) : Boolean; overload;
        function Less (const Right: TVersionData) : Boolean; overload;
        function Greater (const aValue: TVersionInfo) : Boolean; overload;
        function Greater (const aText: string) : Boolean; overload;
        function Greater (const aValue: LongWord) : Boolean; overload;
        function Greater (const Right: TVersionData) : Boolean; overload;
    public
        procedure DoAdd (const aMajor: WORD;
                               aMinor: WORD = 0;
                               aRelease: WORD = 0;
                               aBuild: WORD = 0); overload;
        procedure DoAdd (const aValue: TVersionInfo); overload;
        procedure DoAdd (const aText: string); overload;
        procedure DoAdd (const Right: TVersionData); overload;
    public
        procedure DoSubtract (const aMajor: WORD;
                                    aMinor: WORD = 0;
                                    aRelease: WORD = 0;
                                    aBuild: WORD = 0); overload;
        procedure DoSubtract (const aValue: TVersionInfo); overload;
        procedure DoSubtract (const aText: string); overload;
        procedure DoSubtract (const Right: TVersionData); overload;
    public
        property asString: String read GetAsString write SetAsString;
        property asInteger: LongWord read GetAsInteger write SetAsInteger;
        property asVersion: TVersionInfo read GetasVersion write SetAsVersion;
    published
        property Major: WORD read f_Major write f_Major;
        property Minor: WORD read f_Minor write f_Minor;
        property Release: WORD read f_Release write f_Release;
        property Build: WORD read f_Build write f_Build;
    end;

    PVersionVarData = ^TVersionVarData;
    TVersionVarData = packed record
        VType: TVarType;
        Reserved1, Reserved2, Reserved3: Word;
        VVersion: TVersionData;
        Reserved4: LongInt;
    end;

function VarVersionCreate (const aMajor: WORD;
                                 aMinor: WORD = 0;
                                 aRelease: WORD = 0;
                                 aBuild: WORD = 0) : Variant; overload;
function VarVersionCreate (const aValue: TVersionInfo) : Variant; overload;
function VarVersionCreate (const aText: String) : Variant; overload;
function v(const aValue: TVersionInfo) : Variant; overload;
function v(const aValue: String) : Variant; overload;
function VarToVersion (const aValue: Variant) : TVersionInfo;

function VarVersion: TVarType;
function VarIsVersion (const aValue: Variant) : Boolean;
function VarAsVersion (const aValue: Variant) : Variant;
function VarVersionSimplify (const aValue: Variant) : Variant;

{ Versions Errors }
resourcestring
    ERR_GET_FILE_INFO = 'Ошибка получения информации о файле ''%s''!';

var
    vSelf : Variant;

implementation

uses
    Strings;

function toVersionInfo (const aValue: TVarRec) : TVersionInfo;
begin
    Result := NULL_VERSION;
    with aValue do
    try
        case VType of
            vtInteger    : Result.Major := VInteger;
            vtChar       : Result.Major := StrToInt (VChar);
            vtExtended   : begin
                               Result.Major := StrToInt ( Copy (FloatToStr ( Trunc (VExtended^) ),1,2) );
                               Result.Minor := StrToInt ( Copy (FloatToStr ( Frac (VExtended^)*100 ),1,2) );
                           end;
            vtString     : Result := StrToVersionInfo (VString^);
            vtPointer    : Result := TVersionInfo (VPointer^);
            vtPChar      : Result := StrToVersionInfo ( StrPas (VPChar) );
            vtWideChar   : Result := StrToVersionInfo ( Char (VWideChar) );
            vtPWideChar  : Result := StrToVersionInfo ( WideCharToString (VPWideChar) );
            vtAnsiString : Result := StrToVersionInfo ( String (VAnsiString) );
            vtCurrency   : begin
                               Result.Major := StrToInt ( Copy (FloatToStr ( Trunc (VExtended^) ),1,2) );
                               Result.Minor := StrToInt ( Copy (FloatToStr ( Frac (VExtended^)*100 ),1,2) );
                           end;
            vtVariant    : Result := TVersionVarData (VVariant^).VVersion.asVersion;
            vtWideString : Result := StrToVersionInfo ( WideCharToString (VWideString) );
            vtInt64      : Result.Major := Integer (VInt64^);
        end;
    except
        Result := NULL_VERSION;
    end;
end;

function VersionInfoToStr (const aValue: TVersionInfo) : String;
begin
    Result := Format ('%d.%d.%d.%d',[aValue.Major,
                                     aValue.Minor,
                                     aValue.Release,
                                     aValue.Build]);
end;

function VersionInfoToInt (const aValue: TVersionInfo) : LongWord;
begin
    Result := aValue.Build   * 1 +
              aValue.Release * 100 +
              aValue.Minor   * 10000 +
              aValue.Major   * 1000000;
end;

function IntToVersionInfo (const aValue: LongWord) : TVersionInfo;
var
    S : String;
begin
    Result := NULL_VERSION;
    try
        S := IntToStr (aValue);
        try
            S := LPad (S,8,'0');
            S := Format ('%s.%s.%s.%s',[ Copy (S,1,2),
                                         Copy (S,3,2),
                                         Copy (S,5,2),
                                         Copy (S,7,2) ]);
            Result := StrToVersionInfo (S);
        finally
            _FillChar ( S, Length (S), $00 );
        end;
    except
        Result := NULL_VERSION;
    end;
end;

function GetVersionInfo (out Major,
                             Minor,
                             Release,
                             Build: WORD) : Boolean;
type
    VS_VERSION_INFO = packed record
        wLength      : WORD;
        wValueLength : WORD;
        wType        : WORD;
        szKey        : array [0..15] of WCHAR;
    end;
    PVS_VERSION_INFO  = ^VS_VERSION_INFO;
    PVS_FIXEDFILEINFO = ^VS_FIXEDFILEINFO;
var
    hRES : THandle;
    Data : Pointer;
begin
    Result := FALSE;
    try
        hRES := FindResource (hInstance,'#1',RT_VERSION);
        Result := ( hRES > 0 );
        if Result then
        begin
            hRES := LoadResource (hInstance,hRES);
            Result := ( hRES > 0 );
            if Result then
            begin
                Data := LockResource (hRES);
                Result := Assigned (Data);
                if (  Result and ( pVS_VERSION_INFO (Data).wValueLength > 0 )  ) then
                begin
                    Data := Pointer ( Integer (Data) + SizeOf (VS_VERSION_INFO) + 3 );
                    Data := Pointer ( Integer (Data) and (not 3) );
                    Major   := PVS_FIXEDFILEINFO (Data).dwFileVersionMS shr 16;
                    Minor   := PVS_FIXEDFILEINFO (Data).dwFileVersionMS and $FFFF;
                    Release := PVS_FIXEDFILEINFO (Data).dwFileVersionLS shr 16;
                    Build   := PVS_FIXEDFILEINFO (Data).dwFileVersionLS and $FFFF;
                end;
            end;
        end;
    except
        Result := FALSE;
    end;
end;

function GetVersionInfo : TVersionInfo;
begin
    if not ( GetVersionInfo (Result.Major,
                             Result.Minor,
                             Result.Release,
                             Result.Build) ) then
    begin
        Result.Major   := 0;
        Result.Minor   := 0;
        Result.Release := 0;
        Result.Build   := 0;
    end;
end;

const
    FILE_INFO_STRING : array [FILE_INFO_COMPANY_NAME..FILE_INFO_COMMENTS] of String = (
        'CompanyName',
        'FileDescription',
        'FileVersion',
        'InternalName',
        'LegalCopyright',
        'LegalTradeMarks',
        'OriginalFileName',
        'ProductName',
        'ProductVersion',
        'Comments'
    );

function GetFileInfo (const aFileName: String) : TFileInfo;
type
    PDWORD = ^DWORD;
    PLangAndCodePage = ^TLangAndCodePage;
    TLangAndCodePage = packed record
        wLanguage: WORD;
        wCodePage: WORD;
    end;
    PLangAndCodePageArray = ^TLangAndCodePageArray;
    TLangAndCodePageArray = array[0..0] of TLangAndCodePage;
var
    loc_InfoBufSize : DWORD;
    loc_InfoBuf     : PChar;
    loc_VerBufSize  : DWORD;
    loc_VerBuf      : PChar;
    cbTranslate     : DWORD;
    lpTranslate     : PDWORD;
    I               : DWORD;
    InfoType        : Byte;
begin
    with Result do
    begin
        CompanyName := '';
        FileDescription := '';
        FileVersion := '';
        InternalName := '';
        LegalCopyright := '';
        LegalTradeMarks := '';
        OriginalFileName := '';
        ProductName := '';
        ProductVersion := '';
        Comments := '';
    end;
    try
        loc_InfoBufSize := GetFileVersionInfoSize ( PChar (aFileName), loc_InfoBufSize );
        if ( loc_InfoBufSize > 0 ) then
        begin
            loc_VerBuf := NIL;
            loc_InfoBuf := AllocMem (loc_InfoBufSize);
            try
                if (  Windows.GetFileVersionInfo ( PChar (aFileName), 0, loc_InfoBufSize, loc_InfoBuf ) and
                      VerQueryValue ( loc_InfoBuf,
                                      '\\VarFileInfo\\Translation',
                                      Pointer (lpTranslate),
                                      DWORD (cbTranslate)
                                     )
                    ) then
                begin
                    for InfoType := 0 to 9 do
                        for I := 0 to ( cbTranslate div SizeOf (TLangAndCodePage) ) - 1 do
                        begin
                            if VerQueryValue (  loc_InfoBuf,
                                                PChar ( Format ('StringFileInfo\0%x0%x\%s',
                                                                [ PLangAndCodePageArray (lpTranslate) [I].wLanguage,
                                                                  PLangAndCodePageArray (lpTranslate) [I].wCodePage,
                                                                  FILE_INFO_STRING [InfoType] ])
                                                       ),
                                                Pointer (loc_VerBuf),
                                                DWORD (loc_VerBufSize)
                                              ) then
                            begin
                                with Result do
                                case InfoType of
                                    FILE_INFO_COMPANY_NAME       : CompanyName      := loc_VerBuf;
                                    FILE_INFO_FILE_DESCRIPTION   : FileDescription  := loc_VerBuf;
                                    FILE_INFO_FILE_VERSION       : FileVersion      := loc_VerBuf;
                                    FILE_INFO_INTERNAL_NAME      : InternalName     := loc_VerBuf;
                                    FILE_INFO_LEGAL_COPYRIGHT    : LegalCopyright   := loc_VerBuf;
                                    FILE_INFO_LEGAL_TRADEMARKS   : LegalTradeMarks  := loc_VerBuf;
                                    FILE_INFO_ORIGINAL_FILE_NAME : OriginalFileName := loc_VerBuf;
                                    FILE_INFO_PRODUCT_NAME       : ProductName      := loc_VerBuf;
                                    FILE_INFO_PRODUCT_VERSION    : ProductVersion   := loc_VerBuf;
                                    FILE_INFO_COMMENTS           : Comments         := loc_VerBuf;
                                end;
                                Break;
                            end;
                        end;
                end;
            finally
                FreeMem(loc_InfoBuf, loc_InfoBufSize);
            end;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_GET_FILE_INFO,[aFileName]), E.Message ]);
    end;
end;

function StrToVersionInfo (const aVersionString: String) : TVersionInfo;
var
    S        : String;
    Position : Integer;
    I        : Integer;
begin
    with Result do
    try
        Major := 0;
        Minor := 0;
        Release := 0;
        Build := 0;
        S := aVersionString; { aVersionString Format : %Major.%Minor.%Release.%Build }
        I := 0;
        while ( Length (s) > 0 ) do
        begin
            Position := Pos ('.',S);
            if ( Position > 0 ) then
            begin
                Inc (I);
                case I of
                    1   : Major := StrToInt ( copy (S,1,Position-1) );
                    2   : Minor := StrToInt ( copy (S,1,Position-1) );
                    3   : begin
                              Release := StrToInt ( copy (S,1,Position-1) );
                              Build := StrToInt (  copy ( S, Position+1, Length (S) )  );
                          end;
                    else  Break;
                end;
                S := Copy (S, Position+1, Length (S) - Position );
            end
            else
                Break;
        end;
    except
    end;
end;

function GetFileVersionInfo (const aFileName: String) : TVersionInfo;
var
    FileInfo : TFileInfo;
begin
    FileInfo := GetFileInfo (aFileName);
    Result := StrToVersionInfo (FileInfo.FileVersion);
end;

function GetProductVersionInfo (const aFileName: String) : TVersionInfo;
var
    FileInfo : TFileInfo;
begin
    FileInfo := GetFileInfo (aFileName);
    Result := StrToVersionInfo (FileInfo.ProductVersion);
end;

function ProjectName : String;
var
    ProjectVersion : TVersionInfo;
    FileName       : array [0..MAX_PATH] of Char;
begin
    FillChar ( FileName, SizeOf (FileName), #0 );
    GetModuleFileName ( hInstance, FileName, SizeOf (FileName) );
    ProjectVersion := GetVersionInfo;
    Result := Format ('%s v.%d.%d.%d.%d',[ ExtractFileName (FileName),
                                           ProjectVersion.Major,
                                           ProjectVersion.Minor,
                                           ProjectVersion.Release,
                                           ProjectVersion.Build ]);
end;

function ProductName : String;
var
    FileInfo       : TFileInfo;
    ProductVersion : TVersionInfo;
    FileName       : array [0..MAX_PATH] of Char;
begin
    FillChar ( FileName, SizeOf (FileName), #0 );
    GetModuleFileName ( hInstance, FileName, SizeOf (FileName) );
    FileInfo := GetFileInfo (FileName);
    ProductVersion := GetProductVersionInfo (FileName);
    Result := Format ('%s v.%d.%d.%d.%d %s',[FileInfo.ProductName,
                                             ProductVersion.Major,
                                             ProductVersion.Minor,
                                             ProductVersion.Release,
                                             ProductVersion.Build,
                                             FileInfo.Comments]);
end;

function InternalName : String;
var
    FileInfo       : TFileInfo;
    ProductVersion : TVersionInfo;
    FileName       : array [0..MAX_PATH] of Char;
begin
    FillChar ( FileName, SizeOf (FileName), #0 );
    GetModuleFileName ( hInstance, FileName, SizeOf (FileName) );
    FileInfo := GetFileInfo (FileName);
    ProductVersion := GetFileVersionInfo (FileName);
    Result := Format ('%s v.%d.%d.%d.%d %s',[FileInfo.InternalName,
                                             ProductVersion.Major,
                                             ProductVersion.Minor,
                                             ProductVersion.Release,
                                             ProductVersion.Build,
                                             FileInfo.Comments]);
end;

function CompanyName : String;
var
    FileInfo : TFileInfo;
    FileName : array [0..MAX_PATH] of Char;
begin
    FillChar ( FileName, SizeOf (FileName), #0 );
    GetModuleFileName ( hInstance, FileName, SizeOf (FileName) );
    FileInfo := GetFileInfo (FileName);
    Result := FileInfo.CompanyName;
end;

function CopyRight : String;
var
    FileInfo : TFileInfo;
    FileName : array [0..MAX_PATH] of Char;
begin
    FillChar ( FileName, SizeOf (FileName), #0 );
    GetModuleFileName ( hInstance, FileName, SizeOf (FileName) );
    FileInfo := GetFileInfo (FileName);
    Result := FileInfo.LegalCopyright;
end;

{ TVersionVariantType }
procedure TVersionVariantType.BinaryOp (var Left: TVarData;
                                        const Right: TVarData;
                                        const Operator: TVarOp);
begin
    if ( Right.VType = VarType ) then
        case Left.VType of
            varString : case Operator of
                      { + } opAdd      : Variant (Left) := Variant (Left) +
                                         TVersionVarData (Right).VVersion.asString;
                      { - } opSubtract : Variant (Left) := Variant (Left) -
                                         TVersionVarData (Right).VVersion.asString;
                            else         RaiseInvalidOp;
                        end;
            else if ( Left.VType = VarType ) then
                        case Operator of
                      { + } opAdd      : TVersionVarData (Left).VVersion.DoAdd ( TVersionVarData (Right).VVersion );
                      { - } opSubtract : TVersionVarData (Left).VVersion.DoSubtract ( TVersionVarData (Right).VVersion );
                            else         RaiseInvalidOp;
                        end
            else        RaiseInvalidOp;
        end
    else
        RaiseInvalidOp;
end;

procedure TVersionVariantType.Cast (var Dest: TVarData; const Source: TVarData);
var
    LSource, LTemp : TVarData;
begin
    VarDataInit (LSource);
    try
        VarDataCopyNoInd (LSource,Source);
        VarDataClear (Dest);
        if VarDataIsStr (LSource) then
            TVersionVarData (Dest).VVersion := TVersionData.Create ( VarDataToStr (LSource) )
        else if VarDataIsFloat (LSource) then
        begin
            VarDataInit (LTemp);
            try
                VarDataCastTo (LTemp,LSource,varDouble);
                TVersionVarData (Dest).VVersion := TVersionData.Create (  Trunc (LTemp.VDouble),
                                                                          Round ( Frac (LTemp.VDouble) * 100 )  );
            finally
                VarDataClear (LTemp);
            end;
        end
        else
        begin
            VarDataInit (LTemp);
            try
                VarDataCastTo (LTemp,LSource,varInteger);
                TVersionVarData (Dest).VVersion := TVersionData.Create (LTemp.VInteger);
            finally
                VarDataClear (LTemp);
            end;
        end;
        Dest.VType := VarType;
    finally
        VarDataClear (LSource);
    end;
end;

procedure TVersionVariantType.CastTo (var Dest: TVarData;
                                      const Source: TVarData;
                                      const AVarType: TVarType);
var
    LTemp : TVarData;
begin
    if Source.VType = VarType then
        case AVarType of
            varOleStr : VarDataFromOleStr ( Dest, TVersionVarData (Source).VVersion.asString );
            varString : VarDataFromStr ( Dest, TVersionVarData (Source).VVersion.asString );
            else        VarDataInit (LTemp);
                        try
                            LTemp.VType := varDouble;
                            LTemp.VInteger := TVersionVarData (Source).VVersion.Major;
                            VarDataCastTo (Dest,LTemp,AVarType);
                        finally
                            VarDataClear (LTemp);
                        end;
        end
    else
        inherited CastTo (Dest,Source,AVarType);
end;

procedure TVersionVariantType.Clear (var V: TVarData);
begin
    V.VType := varEmpty;
    FreeAndNil ( TVersionVarData (V).VVersion );
end;

function TVersionVariantType.CompareOp (const Left, Right: TVarData;
                                        const Operator: Integer) : Boolean;
begin
    Result := FALSE;
    if ( (Left.VType = VarType) and (Right.VType = VarType) ) then
        case Operator of
      {  = } opCmpEQ : Result := TVersionVarData (Left).VVersion.Equals ( TVersionVarData (Right).VVersion );
      { <> } opCmpNE : Result := not TVersionVarData(Left).VVersion.Equals ( TVersionVarData (Right).VVersion );
      {  < } opCmpLT : Result := TVersionVarData (Left).VVersion.Less ( TVersionVarData (Right).VVersion );
      { <= } opCmpLE : Result := TVersionVarData (Left).VVersion.Less ( TVersionVarData (Right).VVersion ) or
                                 TVersionVarData (Left).VVersion.Equals ( TVersionVarData (Right).VVersion );
      {  > } opCmpGT : Result := TVersionVarData (Left).VVersion.Greater ( TVersionVarData (Right).VVersion );
      { >= } opCmpGE : Result := TVersionVarData (Left).VVersion.Greater ( TVersionVarData (Right).VVersion ) or
                                 TVersionVarData (Left).VVersion.Equals ( TVersionVarData (Right).VVersion );
             else      RaiseInvalidOp;
        end
    else
        RaiseInvalidOp;
end;

procedure TVersionVariantType.Copy (var Dest: TVarData;
                                    const Source: TVarData;
                                    const Indirect: Boolean);
begin
    if Indirect and VarDataIsByRef (Source) then
        VarDataCopyNoInd (Dest,Source)
    else
        with TVersionVarData (Dest) do
        begin
            VType := VarType;
            VVersion := TVersionData.Create ( TVersionVarData (Source).VVersion );
        end;
end;

function TVersionVariantType.GetInstance (const V: TVarData) : TObject;
begin
    Result := TVersionVarData (V).VVersion;
end;

function TVersionVariantType.isClear (const V: TVarData) : Boolean;
begin
    Result := ( TVersionVarData(V).VVersion = NIL ) or
              TVersionVarData(V).VVersion.isZero;
end;

function TVersionVariantType.LeftPromotion (const V: TVarData;
                                            const Operator: TVarOp;
                                            out RequiredVarType: TVarType) : Boolean;
begin
    { TypeX Op Version }
    if (Operator = opAdd) and VarDataIsStr (V) then
        RequiredVarType := varString
    else
        RequiredVarType := VarType;
    Result := TRUE;
end;

procedure TVersionVariantType.StreamIn (var Dest: TVarData;
                                        const Stream: TStream);
begin
    with TReader.Create (Stream,1024) do
    try
        with TVersionVarData (Dest) do
        begin
            VVersion := TVersionData.Create;
            VVersion.Major := ReadInteger;
            VVersion.Minor := ReadInteger;
            VVersion.Release := ReadInteger;
            VVersion.Build := ReadInteger;
        end;
    finally
        Free;
    end;
end;

procedure TVersionVariantType.StreamOut (const Source: TVarData;
                                         const Stream: TStream);
begin
    with TWriter.Create (Stream,1024) do
    try
        with TVersionVarData (Source).VVersion do
        begin
            WriteFloat (Major);
            WriteFloat (Minor);
            WriteFloat (Release);
            WriteFloat (Build);
        end;
    finally
        Free;
    end;
end;

{ TVersionData }
constructor TVersionData.Create (const aMajor: WORD;
                                       aMinor: WORD = 0;
                                       aRelease: WORD = 0;
                                       aBuild: WORD = 0);
begin
    inherited Create;
    f_Major := 0;
    f_Minor := 0;
    f_Release := 0;
    f_Build := 0;
    SetValue (aMajor,aMinor,aRelease,aBuild);
end;

constructor TVersionData.Create (const aValue: TVersionInfo);
begin
    Create (aValue.Major,
            aValue.Minor,
            aValue.Release,
            aValue.Build);
end;

constructor TVersionData.Create (const aText: string);
begin
    inherited Create;
    asString := aText;
end;

constructor TVersionData.Create (const aData: TVersionData);
begin
    inherited Create;
    SetValue (aData);
end;

procedure TVersionData.SetValue (const aMajor: WORD;
                                       aMinor: WORD = 0;
                                       aRelease: WORD = 0;
                                       aBuild: WORD = 0);
begin
    f_Major := aMajor;
    f_Minor := aMinor;
    f_Release := aRelease;
    f_Build := aBuild;
end;

procedure TVersionData.SetAsInteger (const aValue: LongWord);
begin
    SetAsVersion ( IntToVersionInfo (aValue) );
end;

procedure TVersionData.SetAsVersion (const aValue: TVersionInfo);
begin
    SetValue (aValue.Major,
              aValue.Minor,
              aValue.Release,
              aValue.Build);
end;

procedure TVersionData.SetAsString (const aText: String);
begin
    SetAsVersion ( StrToVersionInfo (aText) );
end;

procedure TVersionData.SetValue (const aData: TVersionData);
begin
    if Assigned (aData) then
        SetValue (aData.Major,
                  aData.Minor,
                  aData.Release,
                  aData.Build);
end;

function TVersionData.GetAsString : String;
begin
    Result := Format ('%d.%d.%d.%d',[Major,
                                     Minor,
                                     Release,
                                     Build]);
end;

function TVersionData.GetAsInteger : LongWord;
begin
    Result := Build   * 1 +
              Release * 100 +
              Minor   * 10000 +
              Major   * 1000000;
end;

function TVersionData.GetAsVersion : TVersionInfo;
begin
    Result.Major := Major;
    Result.Minor := Minor;
    Result.Release := Release;
    Result.Build := Build;
end;

procedure TVersionData.DoAdd (const aMajor: WORD;
                                    aMinor: WORD = 0;
                                    aRelease: WORD = 0;
                                    aBuild: WORD = 0);
begin
    SetValue (Major+aMajor,
              Minor+aMinor,
              Release+aRelease,
              Build+aBuild);
end;

procedure TVersionData.DoAdd (const aValue: TVersionInfo);
begin
    DoAdd (aValue.Major,
           aValue.Minor,
           aValue.Release,
           aValue.Build);
end;

procedure TVersionData.DoAdd (const aText: string);
begin
    DoAdd ( StrToVersionInfo (aText) );
end;

procedure TVersionData.DoAdd (const Right: TVersionData);
begin
    if Assigned (Right) then
        DoAdd (Right.Major,
               Right.Minor,
               Right.Release,
               Right.Build);
end;

procedure TVersionData.DoSubtract (const aMajor: WORD;
                                         aMinor: WORD = 0;
                                         aRelease: WORD = 0;
                                         aBuild: WORD = 0);
begin
    SetValue (Major-aMajor,
              Minor-aMinor,
              Release-aRelease,
              Build-aBuild);
end;

procedure TVersionData.DoSubtract (const aValue: TVersionInfo);
begin
    DoSubtract (aValue.Major,
                aValue.Minor,
                aValue.Release,
                aValue.Build);
end;

procedure TVersionData.DoSubtract (const aText: string);
begin
    DoSubtract ( StrToVersionInfo (aText) );
end;

procedure TVersionData.DoSubtract (const Right: TVersionData);
begin
    if Assigned (Right) then
        DoSubtract (Right.Major,
                    Right.Minor,
                    Right.Release,
                    Right.Build);
end;

function TVersionData.Equals (const aMajor: WORD;
                                    aMinor: WORD = 0;
                                    aRelease: WORD = 0;
                                    aBuild: WORD = 0) : Boolean;
begin
    Result := ( Major = aMajor ) and
              ( Minor = aMinor ) and
              ( Release = aRelease ) and
              ( Build = aBuild );
end;

function TVersionData.Equals (const aValue: TVersionInfo) : Boolean;
begin
    Result := ( Major = aValue.Major ) and
              ( Minor = aValue.Minor ) and
              ( Release = aValue.Release ) and
              ( Build = aValue.Build );
end;

function TVersionData.Equals (const aValue: LongWord) : Boolean;
begin
    Result := (  asInteger = VersionInfoToInt ( IntToVersionInfo (aValue) )  );
end;

function TVersionData.Equals (const aText: string) : Boolean;
begin
    Result := (  asString = VersionInfoToStr ( StrToVersionInfo (aText) )  );
end;

function TVersionData.Equals (const Right: TVersionData) : Boolean;
begin
    Result := Assigned (Right) and Equals (Right.Major,
                                           Right.Minor,
                                           Right.Release,
                                           Right.Build);
end;

function TVersionData.isZero : Boolean;
begin
    Result := Equals (0,0,0,0);
end;

function TVersionData.Less (const aValue: TVersionInfo) : Boolean;
begin
    Result :=   (Major   < aValue.Major) or
              ( (Minor   < aValue.Minor)   and (Major = aValue.Major) ) or
              ( (Release < aValue.Release) and (Major = aValue.Major) and (Minor = aValue.Minor) ) or
              ( (Build   < aValue.Build)   and (Major = aValue.Major) and (Minor = aValue.Minor) and (Release = aValue.Release) );
end;

function TVersionData.Less (const aText: string) : Boolean;
begin
    Result := (  asInteger < VersionInfoToInt ( StrToVersionInfo (aText) )  );
end;

function TVersionData.Less (const aValue: LongWord) : Boolean;
begin
    Result := (  asInteger < VersionInfoToInt ( IntToVersionInfo (aValue) )  );
end;

function TVersionData.Less (const Right: TVersionData) : Boolean;
begin
    Result := Assigned (Right) and Less (Right.asVersion);
end;

function TVersionData.Greater (const aValue: TVersionInfo) : Boolean;
begin
    Result :=   (Major   > aValue.Major) or
              ( (Minor   > aValue.Minor)   and (Major = aValue.Major) ) or
              ( (Release > aValue.Release) and (Major = aValue.Major) and (Minor = aValue.Minor) ) or
              ( (Build   > aValue.Build)   and (Major = aValue.Major) and (Minor = aValue.Minor) and (Release = aValue.Release) );
end;

function TVersionData.Greater (const aText: string) : Boolean;
begin
    Result := (  asInteger > VersionInfoToInt ( StrToVersionInfo (aText) )  );
end;

function TVersionData.Greater (const aValue: LongWord) : Boolean;
begin
    Result := (  asInteger > VersionInfoToInt ( IntToVersionInfo (aValue) )  );
end;

function TVersionData.Greater (const Right: TVersionData) : Boolean;
begin
    Result := Assigned (Right) and Greater (Right.asVersion);
end;

function VarVersion : TVarType;
begin
    Result := VersionVariantType.VarType;
end;

function VarIsVersion (const aValue: Variant) : Boolean;
begin
    Result := (  ( TVarData (aValue).VType and varTypeMask ) = varVersion  );
end;

function VarAsVersion (const aValue: Variant) : Variant;
begin
    if not VarIsVersion (aValue) then
        VarCast (Result,aValue,varVersion)
    else
        Result := aValue;
end;

function VarVersionSimplify (const aValue: Variant) : Variant;
begin
    if VarIsVersion (aValue) and
       ( TVersionVarData(AValue).VVersion.Minor = 0 ) and
       ( TVersionVarData(AValue).VVersion.Release = 0 ) and
       ( TVersionVarData(AValue).VVersion.Build = 0 ) then
        Result := TVersionVarData(AValue).VVersion.Major
    else
        Result := aValue;
end;

procedure VarVersionCreateInto (var aDest: Variant;
                                const aVersionData: TVersionData);
begin
    VarClear (aDest);
    TVersionVarData (ADest).VType := varVersion;
    TVersionVarData (ADest).VVersion := aVersionData;
end;

function VarVersionCreate (const aMajor: WORD;
                                 aMinor: WORD = 0;
                                 aRelease: WORD = 0;
                                 aBuild: WORD = 0) : Variant;
begin
    VarVersionCreateInto ( Result,
                           TVersionData.Create (aMajor,
                                                aMinor,
                                                aRelease,
                                                aBuild)
                          );
end;

function VarVersionCreate (const aValue: TVersionInfo) : Variant;
begin
    VarVersionCreateInto ( Result, TVersionData.Create (aValue) );
end;

function VarVersionCreate (const aText: String) : Variant;
begin
    VarVersionCreateInto ( Result, TVersionData.Create (aText) );
end;

function v(const aValue: TVersionInfo) : Variant;
begin
    Result := VarVersionCreate (aValue);
end;

function v(const aValue: String) : Variant;
begin
    Result := VarVersionCreate (aValue);
end;

function VarToVersion (const aValue: Variant) : TVersionInfo;
begin
    Result := TVersionVarData (aValue).VVersion.asVersion;
end;

initialization
    VersionVariantType := TVersionVariantType.Create;
    vSelf := v(GetVersionInfo);

finalization
    FreeAndNil (VersionVariantType);

end.
 