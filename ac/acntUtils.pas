unit acntUtils;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, sConst, IniFiles
  {$IFDEF TNTUNICODE},TntSysUtils, {$IFNDEF D2006}TntWideStrings{$ELSE}WideStrings{$ENDIF}, TntClasses{$ENDIF};

{$IFNDEF NOTFORHELP}
type
{$IFDEF TNTUNICODE}
  TacSearchRec  = TSearchRecW;
  TacStrings    = TWideStrings;
  TacStringList = TTntStringList;
  TacFileStream = TTntFileStream;
  TacWIN32FindData = TWin32FindDataW;
{$else}
  TacSearchRec  = TSearchRec;
  TacStrings    = TStrings;
  TacStringList = TStringList;
  TacFileStream = TFileStream;
  TacWIN32FindData = TWin32FindData;
{$ENDIF}

{ Serviced, used for debugging}
procedure Alert; overload;
procedure Alert(const s : string); overload;
procedure Alert(i : integer); overload;
{ Qustom request}
function CustomRequest(const s : string):boolean;
{ Request for item deleting}
function DeleteRequest:boolean;
{ Show message S with icon mtWarning}
procedure ShowWarning(const S:string);
{ Show message S with icon mtError}
procedure ShowError(const s:string);
function IsNTFamily : boolean;
function MakeCacheInfo(Bmp : TBitmap; x : integer = 0; y : integer = 0) : TCacheInfo;
{ Add N chars C to string S}
function AddChar(C: AnsiChar; const S: AnsiString; N: Integer): AnsiString;
function AddCharR(C: AnsiChar; const S: AnsiString; N: Integer): AnsiString;
{ Returns formated string, represented float value}
function FormatFloatStr(const S: string; Thousands: Boolean): string;
function GetCaptionFontSize : integer;
function CheckLimits(Value, MinValue, MaxValue : integer) : integer; //overload;
//function CheckLimits(Value : real; MinValue, MaxValue : integer) : integer; overload;
function IntToByte(const Value : integer) : byte; register; //Nick

function HexToInt(HexStr : string) : Int64;

procedure GetIniSections(IniList, SectionsList : TStringList);
procedure GetIniSection(const Section: string; Sections, Strings: TStrings);
function ReadIniString(IniList, SectionsList : TStringList; const Section, Ident, Default: string): string;
function ReadIniInteger(IniList, SectionsList : TStringList; const Section, Ident: string; Default: Longint): Longint;

function StringToFloat(S : String) : Extended;
function MakeMessage(Msg, WParam, LParam, Rsult : longint) : TMessage;
{ Returns percent i2 of i1}
function SumTrans(i1, i2 : integer): integer;
{ Returns max value from i1 and i2}
function Maxi(i1, i2 : integer) : integer;
{ Returns min value from i1 and i2}
function Mini(i1, i2 : integer) : integer;
{ Set value to Minvalue or Maxvalue if it not placed between them}
function LimitIt(Value, MinValue, MaxValue : integer): integer;
{ Returns True if Value is valid float}
function IsValidFloat(const Value: string; var RetValue: Extended): Boolean;

function RectIsVisible(const ParentRect, Rect : TRect) : boolean;
function RectInRect(BigRect, SmallRect : TRect) : boolean;
function RotateRect(R : TRect) : TRect;
function RectsAnd(const R1, R2 : TRect) : TRect;
{ Offset point}
function OffsetPoint( p: TPoint; x,y : integer): TPoint;
{ Returns width of rectangle}
function WidthOf(const r: TRect; CheckNegative : boolean = False): integer;
{ Returns height of rectangle}
function HeightOf(const r: TRect; CheckNegative : boolean = False): integer;
{ Returns string s1 if L, else return s2}
function iff(L : boolean; const s1, s2 : string) : string;
{ Returns TObject o1 if L, else return o2}
function iffo(L : boolean; o1, o2 : TObject) : TObject;
{ Returns integer o1 if L, else return o2}
function iffi(L : boolean; i1, i2 : integer) : integer;
{ Returns True if SubString included in s. If CaseInsensitive then function non-Casesensitive}
function SubStrInclude(const SubString, s : string; CaseInsensitive : boolean) : boolean;
{ Returns position of word number N in string S. WordDelims - chars, word delimiters}
function acWordPosition(const N: Integer; const S: acString; const WordDelims: TSysCharSet): Integer;
function WordPosition(const N: Integer; const S: string; const WordDelims: TSysCharSet): Integer;
{ Returns count of words in string S. WordDelims - chars, word delimiters}
function acWordCount(const S: acString; const WordDelims: TSysCharSet): Integer;
function WordCount(const S: string; const WordDelims: TSysCharSet): Integer;
{ Returns number of word W in string S. WordDelims - chars, word delimiters}
function GetWordNumber(const W, S: string; const WordDelims: TSysCharSet): integer;
{ Returns string with length N, filled by character C}
function MakeStr(C: AnsiChar; N: Integer): AnsiString;
{ Replace substring Srch in string S by substring Replace}
function ReplaceStr(const S, Srch, Replace: string): string;
{ Returns False if S include EmptyChars only}
function IsEmptyStr(const S: string; const EmptyChars: TSysCharSet): Boolean;
{ Convert OEM string OemStr to Ansi string}
function OemToAnsiStr(const OemStr: string): string;
{ Returns True if word W included in string S. WordDelims - chars, word delimiters}
function IsWordPresent(const W, S: string; const WordDelims: TSysCharSet): Boolean;
function IsIDERunning: boolean;

{$ENDIF}

{ Returns string with proper cases of first characters in words. WordDelims - chars, word delimiters}
function AnsiProperCase(const S: string; const WordDelims: TSysCharSet): string;
{ Returns True if value placed berween i1 and i2}
function Between(Value, i1, i2 : integer) : boolean;
function BoolToStr(b : boolean) : string;
{ Change values of i1 and i2}
procedure Changei(var i1, i2 : integer);
{ Corrects string for SQL-operations}
function CorrectString(const s : string) : string;
{ Rounds value F up to two chars after a point}
function CurRound(f : real) : real;
{ Delay in milliseconds}
procedure Delay(MSecs: Integer);
{ Returns string with deleted spaces}
function DelRSpace(const S: string): string;
{ Returns string with deleted leading spaces}
function DelBSpace(const S: string): string;
{ Returns string with deleted last spaces}
function DelESpace(const S: string): string;
{ Returns string with deleted chars Chr}
function DelChars(const S: string; Chr: Char): string;
{ Returns substring from position Pos}
function ExtractSubStr(const S: string; var Pos: Integer; const Delims: TSysCharSet): string;
{ Returns word number N from string S. WordDelims - chars, word delimiters}
function ExtractWord(N: Integer; const S: string; const WordDelims: TSysCharSet): string;
function GetCents(Value : Extended) : smallint;
// Properties
function HasProperty(Component : TObject; PropName: String ): Boolean;
function GetIntProp(Component: TObject; PropName: String): Integer;
procedure SetIntProp(Component: TObject; PropName: String; Value: Integer);
function GetObjProp(Component: TObject; PropName: String): TObject;
procedure SetObjProp(Component: TObject; PropName: String; Value: TObject);
function CheckSetProp(Component: TObject; PropName, Value: String): Boolean;
function SetSetPropValue(Component: TObject; PropName, ValueName: String; Value : boolean): Boolean;

{ ************************************** }

{ String-handling procedures and function }

function acSameText(const S1,S2: ACString): Boolean;
procedure acFillString(var S: acString; nCount: Integer; C: acChar);
function acMakeString(C: acChar; nCount: Integer): acString;

{ File-handling procedures and functions }
function GetSystemDir: ACString;
function GetAppName: ACString;
function GetAppPath: ACString;
function NormalDir(const DirName: ACString): ACString;
function GetFileSize(const FileName: ACString): Int64; overload;
procedure CopyFiles(const SrcMask, SrcDir: ACString; DstDir: ACString);
function ClearDir(const Path: ACString; Delete: Boolean): Boolean;
procedure GetDirs(const Path : ACString; Items : TacStrings);

function acCreateDir(const DirName: ACString): Boolean;
function acRemoveDir(const DirName: ACString): Boolean;
function acSetCurrentDir(const DirName: ACString): Boolean;
function acGetCurrentDir: acString;
function acDirExists(const Name: ACString): Boolean;
function acDeleteFile(const FileName: ACString): Boolean;
function acCopyFile(const ExistingFileName, NewFileName: ACString; bFailIfExists: Boolean): Boolean;
function acFileSetAttr(const FileName: ACString; Attr: Cardinal): Integer;
function acFileGetAttr(const FileName: ACString): Cardinal;
function acFindFirst(const Path: ACString; Attr: Integer; var F: TacSearchRec): Integer;
function acFindNext(var F: TacSearchRec): Integer;
procedure acFindClose(var F: TacSearchRec);

function acFindNextFile(hFindFile: THandle; var lpFindFileData: TacWIN32FindData): BOOL;
function acFindFirstFile(lpFileName: PacChar; var lpFindFileData: TacWIN32FindData): THandle;

{ Returns True if FileName is valid}
function ValidFileName(const FileName: ACString): Boolean;
{ Returns long file name from short}
function ShortToLongFileName(const ShortName: ACString): ACString;
{ Returns long path from short}
function ShortToLongPath(ShortName: ACString): ACString;
{ Returns short file name from long}
function LongToShortFileName(const LongName: ACString): ACString;
{ Returns short path from long}
function LongToShortPath(LongName: ACString): ACString;
function GetIconForFile(const FullFileName: ACString; Flag : integer): TIcon;
{$IFDEF TNTUNICODE}
{$MESSAGE WARN 'System.Delete great works with WideString in latest Delphi versions!}
procedure DeleteW(var s : WideString; index , count : integer);
{$ENDIF}

implementation

uses
  {$IFDEF TNTUNICODE} TntSystem, TntWindows, TntWideStrUtils,{$ENDIF}
  {$IFNDEF ALITE}sDialogs, {$ENDIF} Dialogs, Math, ShellAPI, TypInfo;

{$IFDEF TNTUNICODE}
procedure DeleteW(var s : WideString; index , count : integer);
begin
  System.Delete(S, index, count);
end;
{$ENDIF}

function GetCaptionFontSize : integer;
var
  NonClientMetrics: TNonClientMetrics;
begin
  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0)
    then Result := NonClientMetrics.lfCaptionFont.lfHeight
    else Result := 0;
end;

function CheckLimits(Value, MinValue, MaxValue : integer) : integer;
begin
  if Value < MinValue then Result := MinValue else if Value > MaxValue then Result := MaxValue else Result := Value;
end;

function IntToByte(const Value : integer) : byte; register; //Nick
asm
  test eax, $80000000
  jz @q1
  mov al, 0
  ret
  @q1:
  test eax, $FFFFFF00
  jnz @q2
  ret
  @q2:
  mov al, 255
end;

function IsDebuggerPresent(): Boolean; external 'kernel32.dll';

function HexToInt(HexStr : string) : Int64;
var
  i : byte;
begin
  if HexStr = '' then begin
    Result := 0;
    Exit;
  end;
  HexStr := UpperCase(HexStr);
  if HexStr[length(HexStr)] = 'H' then Delete(HexStr,length(HexStr),1);
  Result := 0;
  for i := 1 to length(HexStr) do begin
    Result := Result shl 4;
    if HexStr[i] in ['0'..'9']
      then Result := Result + (byte(HexStr[i]) - 48)
      else if HexStr[i] in ['A'..'F'] then Result := Result + (byte(HexStr[i]) - 55) else begin
        Result := 0;
        break;
      end;
  end;
end;

procedure GetIniSections(IniList, SectionsList : TStringList);
var
  I: Integer;
  S: string;
  Strings: TStrings;
begin
  SectionsList.Clear;
  Strings := nil;
  for I := 0 to IniList.Count - 1 do begin
    S := IniList[I];
    if (S <> '') and (S[1] <> ';') then begin
      if (S[1] = '[') and (S[Length(S)] = ']') then begin
        Strings := TStringList.Create;
        try
          SectionsList.AddObject(Copy(S, 2, Length(S) - 2), Strings);
        except
          Strings.Free;
        end;
      end
      else begin
        if Strings <> nil then Strings.Add(S);
      end;
    end;
  end;
end;

procedure GetIniSection(const Section: string; Sections, Strings: TStrings);
var
  I, J: Integer;
  SectionStrings: TStrings;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    I := Sections.IndexOf(Section);
    if I >= 0 then
    begin
      SectionStrings := TStrings(Sections.Objects[I]);
      for J := 0 to SectionStrings.Count - 1 do
        Strings.Add(SectionStrings.Names[J]);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

function ReadIniString(IniList, SectionsList : TStringList; const Section, Ident, Default: string): string;
var
  I: Integer;
  Strings: TStrings;
begin
  I := SectionsList.IndexOf(Section);
  if I >= 0 then begin
    Strings := TStrings(SectionsList.Objects[I]);
    I := Strings.IndexOfName(Ident);
    if I >= 0 then begin
      Result := Copy(Strings[I], Length(Ident) + 2, Maxint);
      Exit;
    end;
  end;
  Result := Default;
end;

function ReadIniInteger(IniList, SectionsList : TStringList; const Section, Ident: string; Default: Longint): Longint;
var
  IntStr: string;
begin
  IntStr := ReadIniString(IniList, SectionsList, Section, Ident, '');
  if (Length(IntStr) > 2) and (IntStr[1] = '0') and ((IntStr[2] = 'X') or (IntStr[2] = 'x')) then IntStr := '$' + Copy(IntStr, 3, Maxint);
  Result := StrToIntDef(IntStr, Default);
end;

procedure Alert;
begin
  ShowWarning('Alert');
end;

procedure Alert(const s : string); overload;
begin
  ShowWarning(s);
end;

procedure Alert(i : integer); overload;
begin
  ShowWarning(IntToStr(i));
end;

function StringToFloat(S : String) : Extended;
var
  E, i: Integer;
begin
  s := Trim(s);
  for i := 1 to Length(s) do if S[i] = ',' then S[i] := '.';
  Val(S, Result, E);
  if E <> 0 then Result := 0;
end;

function BoolToStr(b : boolean) : string;
begin
  if b then Result := s_TrueStr else Result := s_FalseStr;
end;

function MakeMessage(Msg, WParam, LParam, Rsult : longint) : TMessage;
begin
  Result.Msg := Msg;
  Result.WParam := WParam;
  Result.LParam := LParam;
  Result.Result := RSult;
end;

function GetCents(Value : Extended) : smallint;
var
  e : extended;
begin
  e := Value;
  Result := Round(Frac(e) * 100);
end;

function iff(L : boolean; const s1, s2 : string) : string;
begin
  if l then Result := s1 else Result := s2;
end;

function iffo(L : boolean; o1, o2 : TObject) : TObject;
begin
  if l then Result := o1 else Result := o2;
end;

function iffi(L : boolean; i1, i2 : integer) : integer;
begin
  if l then Result := i1 else Result := i2;
end;

function Between(Value, i1, i2 : integer) : boolean;
begin
  Result := (Value >= i1) and (Value <= i2);
end;

function SumTrans(i1, i2 : integer): integer;
begin
  Result := Round(i2 + (100 - i2) * (i1 / 100));
end;

function Maxi(i1, i2 : integer) : integer;
begin
  if i1 > i2 then Result := i1 else Result := i2;
end;

function Mini(i1, i2 : integer) : integer;
begin
  if i1 > i2 then Result := i2 else Result := i1;
end;

function LimitIt(Value, MinValue, MaxValue : integer): integer;
begin
  if Value < MinValue then Result := MinValue
  else if Value > MaxValue then Result := MaxValue
  else Result := Value;
end;

procedure Changei(var i1, i2 : integer);
var
  i : integer;
begin
  i := i2;
  i2 := i1;
  i1 := i;
end;

function IsValidFloat(const Value: string; var RetValue: Extended): Boolean;
var
  I: Integer;
  Buffer: array[0..63] of Char;
begin
  Result := False;
  for I := 1 to Length(Value) do
    if not (Value[I] in [DecimalSeparator, '-', '+', '0'..'9', 'e', 'E']) then
      Exit;
  Result := TextToFloat(StrPLCopy(Buffer, Value,
    SizeOf(Buffer) - 1), RetValue {$IFDEF WIN32}, fvExtended {$ENDIF});
end;

function FormatFloatStr(const S: string; Thousands: Boolean): string;
var
  I, MaxSym, MinSym, Group: Integer;
  IsSign: Boolean;
begin
  Result := '';
  MaxSym := Length(S);
  IsSign := (MaxSym > 0) and (S[1] in ['-', '+']);
  if IsSign then MinSym := 2
  else MinSym := 1;
  I := Pos(DecimalSeparator, S);
  if I > 0 then MaxSym := I - 1;
  I := Pos('E', AnsiUpperCase(S));
  if I > 0 then MaxSym := Mini(I - 1, MaxSym);
  Result := Copy(S, MaxSym + 1, MaxInt);
  Group := 0;
  for I := MaxSym downto MinSym do begin
    Result := S[I] + Result;
    Inc(Group);
    if (Group = 3) and Thousands and (I > MinSym) then begin
      Group := 0;
      Result := ThousandSeparator + Result;
    end;
  end;
  if IsSign then Result := S[1] + Result;
end;

function RectIsVisible(const ParentRect, Rect : TRect) : boolean;
begin
  Result := (Rect.Bottom > ParentRect.Top) and
              (Rect.Right > ParentRect.Left) and
                (Rect.Left < ParentRect.Right) and
                  (Rect.Top < ParentRect.Bottom) and not IsRectEmpty(Rect);
end;

function RectInRect( BigRect, SmallRect : TRect) : boolean;
begin
  inc(BigRect.Bottom); inc(BigRect.Right);
  Result := PtInRect(BigRect, SmallRect.TopLeft) and PtInRect(BigRect, SmallRect.BottomRight);
end;

function RotateRect(R : TRect) : TRect;
var
  i : integer;
begin
  i := R.left;
  R.left := R.top;
  R.top := i;

  i := R.right;
  R.right := R.bottom;
  R.bottom := i;
  Result := R;
end;

function RectsAnd(const R1, R2 : TRect) : TRect;
begin
  Result.Left := max(R1.Left, R2.Left);
  Result.Top := max(R1.Top, R2.Top);
  Result.Right := min(R1.Right, R2.Right);
  Result.Bottom := min(R1.Bottom, R2.Bottom);
end;

function OffsetPoint(p: TPoint; x,y : integer): TPoint;
begin
  Result := p;
  inc(Result.x, x);
  inc(Result.y, y);
end;

function WidthOf(const r: TRect; CheckNegative : boolean = False): integer;
begin
  Result := r.Right - r.Left;
  if CheckNegative and (Result < 0) then Result := 0;
end;

function HeightOf(const r: TRect; CheckNegative : boolean = False): integer;
begin
  Result := r.Bottom - r.Top;
  if CheckNegative and (Result < 0) then Result := 0;
end;

function SubStrInclude(const SubString, s : string; CaseInsensitive : boolean) : boolean;
begin
  if CaseInsensitive then begin
    Result := pos(UpperCase(SubString), UpperCase(s)) > 0;
  end
  else begin
    Result := pos(SubString, s) > 0;
  end;
end;

function CorrectString(const s : string) : string;
begin
  Result := s;
  Result := ReplaceStr(Result, '''', '`');
  Result := ReplaceStr(Result, '¡', 'I');
  Result := ReplaceStr(Result, #13, ' ');
  Result := ReplaceStr(Result, #10, ' ');
  Result := DelRSpace(Result);
end;

function acCharIn(C: acChar; SysCharSet: TSysCharSet): Boolean;
begin
  Result := (C <= High(AnsiChar)) and (AnsiChar(C) in SysCharSet);
end;

function acWordPosition(const N: Integer; const S: acString; const WordDelims: TSysCharSet): Integer;
var
  Count, I: Integer;
begin
  Count := 0;
  I := 1;
  Result := 0;
  while (I <= Length(S)) and (Count <> N) do begin
    while (I <= Length(S)) and acCharIn(S[I], WordDelims) do Inc(I);
    if I <= Length(S) then Inc(Count);
    if Count <> N then while (I <= Length(S)) and not acCharIn(S[I], WordDelims) do Inc(I)
    else Result := I;
  end;
end;

function WordPosition(const N: Integer; const S: string; const WordDelims: TSysCharSet): Integer;
var
  Count, I: Integer;
begin
  Count := 0;
  I := 1;
  Result := 0;
  while (I <= Length(S)) and (Count <> N) do begin
    while (I <= Length(S)) and (S[I] in WordDelims) do Inc(I);
    if I <= Length(S) then Inc(Count);
    if Count <> N then while (I <= Length(S)) and not (S[I] in WordDelims) do Inc(I)
    else Result := I;
  end;
end;

function ExtractWord(N: Integer; const S: string; const WordDelims: TSysCharSet): string;
var
  I: Integer;
  Len: Integer;
begin
  Len := 0;
  I := WordPosition(N, S, WordDelims);
  if I <> 0 then begin
    while (I <= Length(S)) and not(S[I] in WordDelims) do begin
      Inc(Len);
      SetLength(Result, Len);
      Result[Len] := S[I];
      Inc(I);
    end;
  end;
  SetLength(Result, Len);
end;

function acWordCount(const S: acString; const WordDelims: TSysCharSet): Integer;
var
  SLen, I: Cardinal;
begin
  Result := 0;
  I := 1;
  SLen := Length(S);
  while I <= SLen do begin
    while (I <= SLen) and acCharIn(S[I], WordDelims) do Inc(I);
    if I <= SLen then Inc(Result);
    while (I <= SLen) and not acCharIn(S[I], WordDelims) do Inc(I);
  end;
end;

function WordCount(const S: string; const WordDelims: TSysCharSet): Integer;
var
  SLen, I: Cardinal;
begin
  Result := 0;
  I := 1;
  SLen := Length(S);
  while I <= SLen do begin
    while (I <= SLen) and (S[I] in WordDelims) do Inc(I);
    if I <= SLen then Inc(Result);
    while (I <= SLen) and not(S[I] in WordDelims) do Inc(I);
  end;
end;

function GetWordNumber(const W, S: string; const WordDelims: TSysCharSet): integer;
var
  Count, i: Integer;
begin
  Result := -1;
  Count := WordCount(S, WordDelims);
  for i := 1 to Count do begin
    if ExtractWord(i, S, WordDelims) = W then begin
      Result := i;
      Exit;
    end;
  end;
end;

function AnsiProperCase(const S: string; const WordDelims: TSysCharSet): string;
var
  SLen, I: Cardinal;
begin
  Result := AnsiLowerCase(S);
  I := 1;
  SLen := Length(Result);
  while I <= SLen do begin
    while (I <= SLen) and (Result[I] in WordDelims) do Inc(I);
    if I <= SLen then Result[I] := AnsiUpperCase(Result[I])[1];
    while (I <= SLen) and not (Result[I] in WordDelims) do Inc(I);
  end;
end;

function MakeStr(C: AnsiChar; N: Integer): AnsiString;
begin
  if N < 1 then begin
    Result := ''
  end
  else begin
    SetLength(Result, N);
    FillChar(Result[1], Length(Result), C);
  end;
end;

function DelRSpace(const S: string): string;
begin
  Result := DelBSpace(DelESpace(S));
end;

function DelESpace(const S: string): string;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] = ' ') do Dec(I);
  Result := Copy(S, 1, I);
end;

function DelBSpace(const S: string): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] = ' ') do Inc(I);
  Result := Copy(S, I, MaxInt);
end;

function DelChars(const S: string; Chr: Char): string;
var
  I: Integer;
begin
  Result := S;
  for I := Length(Result) downto 1 do begin
    if Result[I] = Chr then Delete(Result, I, 1);
  end;
end;

function ReplaceStr(const S, Srch, Replace: string): string;
var
  I: Integer;
  Source: string;
begin
  Source := S;
  Result := '';
  repeat
    I := Pos(Srch, Source);
    if I > 0 then begin
      Result := Result + Copy(Source, 1, I - 1) + Replace;
      Source := Copy(Source, I + Length(Srch), MaxInt);
    end
    else begin
      Result := Result + Source;
    end;
  until I <= 0;
end;

function ExtractSubStr(const S: string; var Pos: Integer; const Delims: TSysCharSet): string;
var
  I: Integer;
begin
  I := Pos;
  while (I <= Length(S)) and not (S[I] in Delims) do Inc(I);
  Result := Copy(S, Pos, I - Pos);
  if (I <= Length(S)) and (S[I] in Delims) then Inc(I);
  Pos := I;
end;

function IsEmptyStr(const S: string; const EmptyChars: TSysCharSet): Boolean;
var
  I, SLen: Integer;
begin
  SLen := Length(S);
  I := 1;
  while I <= SLen do begin
    if not (S[I] in EmptyChars) then begin
      Result := False;
      Exit;
    end
    else begin
      Inc(I);
    end;
  end;
  Result := True;
end;

function IsNTFamily : boolean;
begin
  Result := (Win32MajorVersion > 5) or ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1));
end;

function MakeCacheInfo(Bmp : TBitmap; x : integer = 0; y : integer = 0) : TCacheInfo;
begin
  Result.Bmp := Bmp;
  Result.FillColor := clFuchsia;
  Result.X := X;
  Result.Y := Y;
  Result.Ready := True;
end;

function AddChar(C: AnsiChar; const S: AnsiString; N: Integer): AnsiString;
begin
  if Length(S) < N then begin
    Result := MakeStr(C, N - Length(S)) + S
  end
  else begin
    Result := S;
  end;
end;

function AddCharR(C: AnsiChar; const S: AnsiString; N: Integer): AnsiString;
begin
  if Length(S) < N then
    Result := S + MakeStr(C, N - Length(S))
  else Result := S;
end;

function OemToAnsiStr(const OemStr: string): string;
begin
  SetLength(Result, Length(OemStr));
  if Length(Result) > 0 then begin
    OemToCharBuff(POldChar(OemStr), PChar(Result), Length(Result));
  end;
end;

function IsWordPresent(const W, S: string; const WordDelims: TSysCharSet): Boolean;
var
  Count, I: Integer;
begin
  Result := False;
  Count := WordCount(S, WordDelims);
  for I := 1 to Count do begin
    if ExtractWord(I, S, WordDelims) = W then begin
      Result := True;
      Exit;
    end;
  end;
end;

function CurRound(f : real) : real;
begin
  Result := Round((f+0.000001)*100)/100;
end;

////// Dialogs
function DeleteRequest:boolean;
begin
{$IFNDEF ALITE}
  Result := sMessageDlg('Delete this item?', mtConfirmation, [mbYes, mbNo], 0) = mrYes;
{$ELSE}
  Result := MessageDlg('Delete this item?', mtConfirmation, [mbYes, mbNo], 0) = mrYes;
{$ENDIF}
end;

function CustomRequest(const s : string):boolean;
begin
{$IFNDEF ALITE}
  Result := sMessageDlg(s, mtConfirmation, [mbYes, mbNo], 0)=mrYes;
{$ELSE}
  Result := MessageDlg(s, mtConfirmation, [mbYes, mbNo], 0)=mrYes;
{$ENDIF}
end;

procedure ShowWarning(const s:string);
begin
{$IFNDEF ALITE}
  sMessageDlg(s, mtWarning, [mbOk], 0);
{$ELSE}
  MessageDlg(s, mtWarning, [mbOk], 0);
{$ENDIF}
end;

procedure ShowError(const s:string);
begin
{$IFNDEF ALITE}
  sMessageDlg(s, mtError, [mbOk], 0);
{$ELSE}
  MessageDlg(s, mtError, [mbOk], 0);
{$ENDIF}
end;

procedure Delay(MSecs: Integer);
var
  FirstTickCount : DWord;
begin
  FirstTickCount := GetTickCount;
  repeat Application.ProcessMessages until ((GetTickCount - FirstTickCount) >= DWord(MSecs));
end;

function IsIDERunning: boolean;
begin
  Result := IsDebuggerPresent;
end;

// Prop Info
function HasProperty(Component : TObject; PropName: String ): Boolean;
begin
  Result := GetPropInfo(Component.ClassInfo, PropName) <> nil;
end;

function GetObjProp(Component: TObject; PropName: String): TObject;
var
  ptrPropInfo : PPropInfo;
begin
  ptrPropInfo := GetPropInfo(Component.ClassInfo, PropName);
  if ptrPropInfo = nil
    then Result := nil
    else Result := TObject(GetObjectProp(Component, ptrPropInfo, TObject));
end;

procedure SetObjProp(Component: TObject; PropName: String; Value: TObject);
var
  ptrPropInfo : PPropInfo;
begin
  ptrPropInfo := GetPropInfo(Component.ClassInfo, PropName);
  if ptrPropInfo <> nil then SetObjectProp(Component, ptrPropInfo, Value);
end;

function CheckSetProp(Component: TObject; PropName, Value: String): Boolean;
var
  PropInfo : PPropInfo;
  TypeInfo: PTypeInfo;
  i : integer;
  S: TIntegerSet;
begin
  Result := False;
  PropInfo := GetPropInfo(Component.ClassInfo, PropName);
  if PropInfo <> nil then begin
    Integer(S) := GetOrdProp(Component, PropInfo);
    TypeInfo := GetTypeData(PropInfo^.PropType^)^.CompType^;
    for I := 0 to SizeOf(Integer) * 8 - 1 do if I in S then if GetEnumName(TypeInfo, I) = Value then begin
      Result := True;
      Break
    end;
  end;
end;

{$IFNDEF DELPHI6UP}
function SetToString(PropInfo: PPropInfo; Value: Integer): string;
var
  S: TIntegerSet;
  TypeInfo: PTypeInfo;
  I: Integer;
begin
  Result := '';
  Integer(S) := Value;
  TypeInfo := GetTypeData(PropInfo^.PropType^)^.CompType^;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + GetEnumName(TypeInfo, I);
    end;
end;
{$ENDIF}

function SetSetPropValue(Component: TObject; PropName, ValueName: String; Value : boolean): Boolean;
var
  PropInfo : PPropInfo;
  i : integer;
  s : string;
begin
  Result := False;
  PropInfo := GetPropInfo(Component.ClassInfo, PropName);
  if PropInfo <> nil then begin
    Integer(I) := GetOrdProp(Component, PropInfo);
    s := SetToString(PropInfo, I);
    if Value then begin
      if pos(s, ValueName) < 1 then begin
        s := s + ',' + ValueName;
        SetSetProp(Component, PropInfo, s);
      end;
    end
    else begin
      i := pos(',' + ValueName, s);
      if i > 0 then Delete(s, i, Length(',' + ValueName)) else begin
        i := pos(ValueName + ',', s);
        if i > 0 then Delete(s, i, Length(',' + ValueName)) else begin
          i := pos(ValueName, s);
          if i > 0 then Delete(s, i, Length(ValueName)) else Exit;
        end;
      end;
      SetSetProp(Component, PropInfo, s);
    end;
  end;
end;

function GetIntProp(Component: TObject; PropName: String): Integer;
var
  ptrPropInfo : PPropInfo;
begin
  ptrPropInfo := GetPropInfo(Component.ClassInfo, PropName);
  if ptrPropInfo = nil
    then Result := -1
    else Result := Integer(GetOrdProp(Component, ptrPropInfo));
end;

procedure SetIntProp(Component: TObject; PropName: String; Value: Integer);
var
  ptrPropInfo : PPropInfo;
begin
  ptrPropInfo := GetPropInfo(Component.ClassInfo, PropName);
  if ptrPropInfo <> nil
     then SetOrdProp(Component, ptrPropInfo, Value);
end;

{ *************************************** }
{ File-handling procedures and functions  }
{ *************************************** }

function GetSystemDir: ACString;
var
  Buffer: array[0..1023] of ACChar;
begin
{$IFDEF TNTUNICODE}
  SetString(Result, Buffer, Tnt_GetSystemDirectoryW(Buffer, Length(Buffer)));
{$else}
  SetString(Result, Buffer, GetSystemDirectory(Buffer, Length(Buffer)));
{$ENDIF}
end;

function NormalDir(const DirName: ACString): ACString;
begin
  Result := DirName;
  if (Result <> '') and not (Result[Length(Result)] = '\') then begin
    if Result[Length(Result)] = '/'
      then Result[Length(Result)] := '\'
      else begin
        if (Length(Result) = 1)
          then Result := Result + ':\'
          else Result := Result + '\';
      end;
  end;
end;

function GetFileSize(const FileName: ACString): Int64;
var
  SearchRec: TacSearchRec;
{$IFDEF TNTUNICODE}
begin
  if WideFindFirst(WideExpandFileName(FileName), faAnyFile, SearchRec) = 0 then Result := SearchRec.Size else Result := -1;
  WideFindClose(SearchRec);
end;
{$else}
begin
  if FindFirst(ExpandFileName(FileName), faAnyFile, SearchRec) = 0 then begin
    with SearchRec do Result := (Int64(FindData.nFileSizeHigh) shl 32) + FindData.nFileSizeLow;
  end
  else Result := -1;
  FindClose(SearchRec);
end;
{$ENDIF}

function acFindNextFile(hFindFile: THandle; var lpFindFileData: TacWIN32FindData): BOOL;
begin
{$IFDEF TNTUNICODE}
  Result := Tnt_FindNextFileW(hFindFile, lpFindFileData);
{$ELSE}
  Result := FindNextFile(hFindFile, lpFindFileData);
{$ENDIF}
end;

function acFindFirstFile(lpFileName: PacChar; var lpFindFileData: TacWIN32FindData): THandle;
begin
{$IFDEF TNTUNICODE}
  Result := Tnt_FindFirstFileW(lpFileName, lpFindFileData);
{$ELSE}
  Result := FindFirstFile(lpFileName, lpFindFileData);
{$ENDIF}
end;

function ValidFileName(const FileName: ACString): Boolean;
  function HasAny(const Str, Substr: ACString): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 1 to Length(Substr) do begin
      if Pos(Substr[I], Str) > 0 then begin
        Result := True;
        Break;
      end;
    end;
  end;
begin
  Result := (FileName <> '') and (not HasAny(FileName, '<>"|*?/'));
{$IFDEF TNTUNICODE}
  if Result then Result := Pos('\', WideExtractFileName(FileName)) = 0;
{$else}
  if Result then Result := Pos('\', ExtractFileName(FileName)) = 0;
{$ENDIF}
end;

function acDirExists(const Name: ACString): Boolean;
{$IFDEF TNTUNICODE}
var
  Code: Integer;
{$ELSE}
  {$IFNDEF DELPHI7UP}
var
  Code: Integer;
  {$ENDIF}
{$ENDIF}
begin
{$R-}
  {$IFDEF TNTUNICODE}
  Code := Tnt_GetFileAttributesW(PACChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
  {$ELSE}
    {$IFDEF DELPHI7UP}
  Result := DirectoryExists(Name);
    {$ELSE}
  Code := GetFileAttributes(PACChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
    {$ENDIF}
  {$ENDIF}
{$R+}
end;

function ShortToLongFileName(const ShortName: ACString): ACString;
var
{$IFDEF TNTUNICODE}
  Temp: TWin32FindDataW;
{$ELSE}
  Temp: TWin32FindDataA;
{$ENDIF}
  SearchHandle: THandle;
begin
{$IFDEF TNTUNICODE}
  SearchHandle := Tnt_FindFirstFileW(PACChar(ShortName), Temp);
{$ELSE}
  SearchHandle := FindFirstFileA(POldChar(ShortName), Temp);
{$ENDIF}
  if SearchHandle <> INVALID_HANDLE_VALUE then begin
    Result := ACString(Temp.cFileName);
    if Result = '' then Result := ACString(Temp.cAlternateFileName);
  end
  else Result := '';
  Windows.FindClose(SearchHandle);
end;

function LongToShortFileName(const LongName: ACstring): ACString;
var
{$IFDEF TNTUNICODE}
  Temp: TWin32FindDataW;
{$ELSE}
  Temp: TWin32FindDataA;
{$ENDIF}
  SearchHandle: THandle;
begin
{$IFDEF TNTUNICODE}
  SearchHandle := Tnt_FindFirstFileW(PACChar(LongName), Temp);
{$ELSE}
  SearchHandle := FindFirstFileA(POldChar(LongName), Temp);
{$ENDIF}
  if SearchHandle <> INVALID_HANDLE_VALUE then begin
    Result := ACString(Temp.cAlternateFileName);
    if Result = '' then Result := ACString(Temp.cFileName);
  end
  else Result := '';
  Windows.FindClose(SearchHandle);
end;

function ShortToLongPath( ShortName: ACString ): ACString;
var
  LastSlash: PACChar;
  TempPathPtr: PACChar;
begin
  Result := '';
  TempPathPtr := PACChar(ShortName);
  UniqueString (ShortName);
{$IFDEF TNTUNICODE}
  LastSlash := WStrRScan(TempPathPtr, '\');
{$ELSE}
  LastSlash := StrRScan(TempPathPtr, '\');
{$ENDIF}
  while LastSlash <> nil do begin
    Result := '\' + ShortToLongFileName(TempPathPtr) + Result;
    if LastSlash <> nil then begin
      LastSlash^ := #0;
     {$IFDEF TNTUNICODE}
      LastSlash := WStrRScan(TempPathPtr, '\');
     {$ELSE}
      LastSlash := StrRScan(TempPathPtr, '\');
     {$ENDIF}
    end;
  end;
  Result := TempPathPtr + Result;
end;

function LongToShortPath(LongName: ACString): ACString;
var
  LastSlash: PACChar;
  TempPathPtr: PACChar;
begin
  Result := '';
  UniqueString (LongName);
  TempPathPtr := PACChar(LongName);
{$IFDEF TNTUNICODE}
  LastSlash := WStrRScan(TempPathPtr, '\');
{$ELSE}
  LastSlash := StrRScan(TempPathPtr, '\');
{$ENDIF}
  while LastSlash <> nil do begin
    Result := '\' + LongToShortFileName(TempPathPtr) + Result;
    if LastSlash <> nil then begin
      LastSlash^ := #0;
      {$IFDEF TNTUNICODE}
      LastSlash := WStrRScan(TempPathPtr, '\');
      {$ELSE}
      LastSlash := StrRScan(TempPathPtr, '\');
      {$ENDIF}
    end;
  end;
  Result := TempPathPtr + Result;
end;

procedure CopyFiles(const SrcMask, SrcDir: ACString; DstDir: ACString);
var
  FileInfo: TacSearchRec;
  DosCode: Integer;
begin
  if not acDirExists(DstDir) then acCreateDir(DstDir);
  DstDir := NormalDir(DstDir);
  DosCode := acFindFirst(SrcDir + SrcMask, faAnyFile, FileInfo);
  try
    while DosCode = 0 do begin
      if (FileInfo.Name[1] <> '.') and (FileInfo.Attr <> faVolumeID) then begin
        if (FileInfo.Attr and faVolumeID <> faVolumeID) then begin
         acCopyFile(SrcDir + FileInfo.Name, DstDir + FileInfo.Name, False);
        end;
      end;
      DosCode := acFindNext(FileInfo);
    end;
  finally
    acFindClose(FileInfo);
  end;
end;

function acCreateDir(const DirName: ACString): Boolean;
begin
{$IFDEF TNTUNICODE}
  Result := Tnt_CreateDirectoryW(PACChar(DirName), nil);
{$else}
  Result := CreateDirectoryA(POldChar(DirName), nil);
{$ENDIF}
end;

function acRemoveDir(const DirName: ACString): Boolean;
begin
{$IFDEF TNTUNICODE}
  Result := Tnt_RemoveDirectoryW(PACChar(DirName));
{$else}
  Result := RemoveDirectoryA(POldChar(DirName));
{$ENDIF}
end;

function acSetCurrentDir(const DirName: ACString): Boolean;
begin
{$IFDEF TNTUNICODE}
  Result := Tnt_SetCurrentDirectoryW(PACChar(DirName) );
{$else}
  Result := SetCurrentDirectoryA(POldChar(DirName));
{$ENDIF}
end;

function acGetCurrentDir: acString;
var Buf: array[0..MAX_PATH] of acChar;
begin
{$IFDEF TNTUNICODE}
  Tnt_GetCurrentDirectoryW(MAX_PATH, Buf);
{$else}
  GetCurrentDirectory(MAX_PATH, Buf);
{$ENDIF}
  Result := Buf;
end;

function acDeleteFile(const FileName: ACString): Boolean;
begin
{$IFDEF TNTUNICODE}
  Result := Tnt_DeleteFileW(PACChar(FileName));
{$else}
  Result := DeleteFile(PACChar(FileName));
{$ENDIF}
end;

function acCopyFile(const ExistingFileName, NewFileName: ACString; bFailIfExists: Boolean): Boolean;
begin
{$IFDEF TNTUNICODE}
  Result := Tnt_CopyFileW(PACChar(ExistingFileName), PACChar(NewFileName), bFailIfExists);
{$else}
  Result := CopyFile(PACChar(ExistingFileName), PACChar(NewFileName), bFailIfExists);
{$ENDIF}
end;

function acFileSetAttr(const FileName: ACString; Attr: Cardinal): Integer;
begin
  Result := 0;
{$IFDEF TNTUNICODE}
  if not Tnt_SetFileAttributesW(PACChar(FileName), Attr) then
{$ELSE}
  if not SetFileAttributes(PACChar(FileName), Attr) then
{$ENDIF}
    Result := Integer(GetLastError);
end;

function acFileGetAttr(const FileName: ACString): Cardinal;
begin
{$IFDEF TNTUNICODE}
  Result := Tnt_GetFileAttributesW(PACChar(FileName));
{$ELSE}
  Result := GetFileAttributes(PACChar(FileName));
{$ENDIF}
end;

function acFindFirst(const Path: ACString; Attr: Integer; var F: TacSearchRec): Integer;
begin
{$IFDEF TNTUNICODE}
  Result := WideFindFirst(Path, Attr, F);
{$ELSE}
  Result := FindFirst(Path, Attr, F);
{$ENDIF}
end;

function acFindNext(var F: TacSearchRec): Integer;
begin
{$IFDEF TNTUNICODE}
  Result := WideFindNext(F);
{$ELSE}
  Result := FindNext(F);
{$ENDIF}
end;

procedure acFindClose(var F: TacSearchRec);
begin
{$IFDEF TNTUNICODE}
  WideFindClose(F);
{$ELSE}
  FindClose(F);
{$ENDIF}
end;

function ClearDir(const Path: ACString; Delete: Boolean): Boolean;
const
  FileNotFound = 18;
var
  FileInfo: TacSearchRec;
  DosCode: Integer;
begin
  Result := acDirExists(Path);
  if not Result then Exit;
  DosCode := acFindFirst(NormalDir(Path) + '*.*', faAnyFile, FileInfo);
  try
    while DosCode = 0 do begin
      if (FileInfo.Name[1] <> '.') and (FileInfo.Attr <> faVolumeID) then begin
        if (FileInfo.Attr and faDirectory = faDirectory) then begin
          Result := ClearDir(NormalDir(Path) + FileInfo.Name, Delete) and Result
        end
        else if (FileInfo.Attr and faVolumeID <> faVolumeID) then begin
          if (FileInfo.Attr and faReadOnly = faReadOnly) then begin
            acFileSetAttr(NormalDir(Path) + FileInfo.Name, faArchive);
          end;
          Result := acDeleteFile(NormalDir(Path) + FileInfo.Name) and Result;
        end;
      end;
      DosCode := acFindNext(FileInfo);
    end;
  finally
    acFindClose(FileInfo);
  end;
  if Delete and Result and (DosCode = FileNotFound) and not ((Length(Path) = 2) and (Path[2] = ':')) then begin
    Result := acRemoveDir(Path) and Result;
  end;
end;

function GetAppName: ACString;
begin
{$IFDEF TNTUNICODE}
  Result := Tnt_WideLowerCase(WideChangeFileExt( WideExtractFileName(WideParamStr(0)), ''));
{$ELSE}
  Result := AnsiLowerCase(ChangeFileExt( ExtractFileName(ParamStr(0)), ''));
{$ENDIF}
end;

function GetAppPath: ACString;
begin
{$IFDEF TNTUNICODE}
  Result := WideExtractFilePath(WideParamStr(0));
{$ELSE}
  Result := ExtractFilePath(ParamStr(0));
{$ENDIF}
end;

function GetIconForFile(const FullFileName: ACString; Flag : integer): TIcon;
{$IFDEF TNTUNICODE}
var
  SH: TSHFileInfoW;
begin
  Result := nil;
  if WideFileExists(FullFileName) then begin
    FillChar(SH, SizeOf(SH), 0);
    if Tnt_SHGetFileInfoW(PACChar(FullFileName), 0, SH, SizeOf(SH), SHGFI_ICON or Flag) <> 0 then begin
      Result := TIcon.Create;
      Result.Handle := SH.hIcon;
      if Result.Empty then FreeAndNil(Result);
    end;
  end;
end;
{$ELSE}
var
  SH: TSHFileInfo;
begin
  Result := nil;
  if FileExists(FullFileName) then begin
    FillChar(SH, SizeOf(SH), 0);
    if SHGetFileInfo(PACChar(FullFileName), 0, SH, SizeOf(SH), SHGFI_ICON or Flag) <> 0 then begin
      Result := TIcon.Create;
      Result.Handle := SH.hIcon;
      if Result.Empty then FreeAndNil(Result);
    end;
  end;
end;
{$ENDIF}

procedure GetDirs(const Path : ACString; Items : TacStrings);
var
  FileInfo: TacSearchRec;
  DosCode: Integer;
  S: ACString;
begin
  Items.Clear;
  if acDirExists(Path) then begin
    s := Path + '\*.*';
    DosCode := acFindFirst(s, faDirectory, FileInfo);
    try
      while DosCode = 0 do begin
        if (FileInfo.Name[1] <> '.') and (FileInfo.Attr and faDirectory = faDirectory) then begin
          Items.Add(FileInfo.Name);
        end;
        DosCode := acFindNext(FileInfo);
      end;
    finally
      acFindClose(FileInfo);
    end;
  end;
end;

{ String-handling procedures and function }

function acSameText(const S1, S2: ACString): Boolean;
begin
{$IFDEF TNTUNICODE}
  Result := WideSameText(S1, S2);
{$ELSE}
  Result := SameText(S1, S2);
{$ENDIF}
end;

procedure acFillString(var S: acString; nCount: Integer; C: acChar);
{$IFDEF TNTUNICODE}
var i: Integer;
begin
  if nCount < 1 then
    S := ''
  else begin
    if Length(S) <> nCount then
       SetLength(S, nCount);
    for i:=1 to nCount do S[i]:=C;
  end;
end;
{$ELSE}
begin
  if nCount < 1 then
    S := ''
  else begin
    if Length(S) <> nCount then
       SetLength(S, nCount);
    fillchar(S[1], nCount, C);
  end;
end;
{$ENDIF}

function acMakeString(C: acChar; nCount: Integer): acString;
begin
  acFillString(Result, nCount, C);
end;

end.
