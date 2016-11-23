unit VarRecs;
{******************************************************************************}
{*  Unit for Conversion of TVarRec Types                                      *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2010                                             *}
{******************************************************************************}
interface

{$I 'std.inc'}

uses
    Windows, SysUtils, DateUtils,
    Utils;

type
    array_of_const = array of TVarRec;

function toBoolean (const aValue: TVarRec) : Boolean; overload;
function toDateTime (const aValue: TVarRec) : TDateTime; overload;
function toDate (const aValue: TVarRec) : TDateTime; overload;
function toTime (const aValue: TVarRec) : TDateTime; overload;
function toPointer (const aValue: TVarRec) : Pointer; overload;
function toObject (const aValue: TVarRec) : TObject; overload;
function toClass (const aValue: TVarRec) : TClass; overload;
function toInteger (const aValue: TVarRec) : Integer; overload;
function toString (const aValue: TVarRec) : String; overload;
function toExtended (const aValue: TVarRec) : Extended; overload;
function toDouble (const aValue: TVarRec) : Double; overload;
function toInt64 (const aValue: TVarRec) : Int64; overload;
function toArrayOfConst (const aValue: TVarRec) : array_of_const; overload;

function _array_of_const (anArgs: array of const) : array_of_const;
function _(anArgs: array of const) : array_of_const; overload;

function merge (anArgs1, anArgs2: array of const) : array_of_const; overload; 
function _(anArgs1, anArgs2: array of const) : array_of_const; overload;

function isEmpty (const anIndex: Integer; anArgs: array of const) : Boolean; overload;
function notEmpty (const anIndex: Integer; anArgs: array of const) : Boolean; overload;

implementation

{ TVarRec }
function toBoolean (const aValue: TVarRec) : Boolean;
begin
    Result := FALSE;
    with aValue do
    try
        case VType of
            vtInteger    : Result := IntToBoolean (VInteger);
            vtBoolean    : Result := VBoolean;
            vtChar       : Result := StrToBoolean ( VChar );
            vtExtended   : Result := IntToBoolean ( Round (VExtended^) );
            vtString     : Result := StrToBoolean (VString^);
            vtPointer    : Result := StrToBoolean ( StrPas (VPChar) );
            vtPChar      : Result := StrToBoolean ( StrPas (VPChar) );
            vtWideChar   : Result := StrToBoolean ( Char (VWideChar) );
            vtPWideChar  : Result := StrToBoolean ( WideCharToString (VPWideChar) );
            vtAnsiString : Result := StrToBoolean ( String (VAnsiString) );
            vtCurrency   : Result := IntToBoolean ( Round (VExtended^) );
            vtVariant    : Result := Boolean (VVariant^);
            vtWideString : Result := StrToBoolean ( WideCharToString (VWideString) );
            vtInt64      : Result := IntToBoolean ( Integer (VInt64^) );
        end;
    except
        Result := FALSE;
    end;
end;

function toDateTime (const aValue: TVarRec) : TDateTime;
begin
    Result := 0.0;
    with aValue do
    try
        case VType of
            vtInt64      : Result := UnixToDateTime (VInt64^);
            vtInteger    : Result := UnixToDateTime (VInteger);
            vtExtended   : Result := VExtended^;
            vtVariant    : Result := Extended (VVariant^);
            vtString     : Result := StrToDateTime (VString^);
            vtPointer    : Result := StrToDateTime ( StrPas (VPChar) );
            vtPChar      : Result := StrToDateTime ( StrPas (VPChar) );
            vtAnsiString : Result := StrToDateTime ( String (VAnsiString) );
            vtPWideChar  : Result := StrToDateTime ( WideCharToString (VPWideChar) );
            vtWideString : Result := StrToDateTime ( WideCharToString (VWideString) );
        end;
    except
        Result := 0.0;
    end;
end;

function toDate (const aValue: TVarRec) : TDateTime;
begin
    Result := 0.0;
    with aValue do
    try
        case VType of
            vtInt64      : Result := VInt64^;
            vtInteger    : Result := VInteger;
            vtExtended   : Result := Trunc (VExtended^);
            vtVariant    : Result := Trunc ( Extended (VVariant^) );
            vtString     : Result := StrToDate (VString^);
            vtPointer    : Result := StrToDate ( StrPas (VPChar) );
            vtPChar      : Result := StrToDate ( StrPas (VPChar) );
            vtAnsiString : Result := StrToDate ( String (VAnsiString) );
            vtPWideChar  : Result := StrToDate ( WideCharToString (VPWideChar) );
            vtWideString : Result := StrToDate ( WideCharToString (VWideString) );
        end;
    except
        Result := 0.0;
    end;
end;

function toTime (const aValue: TVarRec) : TDateTime;
begin
    Result := 0.0;
    with aValue do
    try
        case VType of
            vtInt64      : Result := Frac ( UnixToDateTime (VInt64^) );
            vtInteger    : Result := Frac ( UnixToDateTime (VInteger) );
            vtExtended   : Result := Frac (VExtended^);
            vtVariant    : Result := Frac ( Extended (VVariant^) );
            vtString     : Result := StrToTime (VString^);
            vtPointer    : Result := StrToTime ( StrPas (VPChar) );
            vtPChar      : Result := StrToTime ( StrPas (VPChar) );
            vtAnsiString : Result := StrToTime ( String (VAnsiString) );
            vtPWideChar  : Result := StrToTime ( WideCharToString (VPWideChar) );
            vtWideString : Result := StrToTime ( WideCharToString (VWideString) );
        end;
    except
        Result := 0.0;
    end;
end;

function toPointer (const aValue: TVarRec) : Pointer;
begin
    Result := NIL;
    with aValue do
    try
        case VType of
            vtPointer : Result := VPointer;
            vtClass   : Result := VClass;
            vtObject  : Result := VObject;
        end;
    except
        Result := NIL;
    end;
end;

function toObject (const aValue: TVarRec) : TObject;
begin
    Result := NIL;
    with aValue do
    try
        case VType of
            vtObject  : Result := VObject;
            {vtPointer : if Assigned (VPointer) and isObject (VPointer) then
                            Result := TObject (VPointer);}
        end;
    except
        Result := NIL;
    end;
end;

function toClass (const aValue: TVarRec) : TClass;
begin
    Result := NIL;
    with aValue do
    try
        case VType of
            vtClass   : Result := VClass;
            vtObject  : Result := VObject.ClassType;
            {vtPointer : if Assigned (VPointer) and isClass (VPointer) then
                            Result := TClass (VPointer);}
        end;
    except
        Result := NIL;
    end;
end;

function toInteger (const aValue: TVarRec) : Integer;
begin
    Result := 0;
    with aValue do
    try
        case VType of
            vtVariant    : Result := Integer (VVariant^);
            vtInteger    : Result := VInteger;
            vtInt64      : Result := Integer (VInt64^);
            vtPointer    : Result := Longint (VPointer);
            vtObject     : Result := Longint (VObject);
            vtClass      : Result := Longint (VClass);
            vtInterface  : Result := Longint (VInterface);
            vtBoolean    : Result := BooleanToInt (VBoolean);
            vtString     : Result := StrToInt (VString^);
            vtChar       : Result := StrToInt (VChar);
            vtPChar      : Result := StrToInt ( StrPas (VPChar) );
            vtAnsiString : Result := StrToInt ( String (VAnsiString) );
            vtWideChar   : Result := StrToInt ( Char (VWideChar) );
            vtPWideChar  : Result := StrToInt ( WideCharToString (VPWideChar) );
            vtWideString : Result := StrToInt ( WideCharToString (VWideString) );
        end;
    except
        Result := 0;
    end;
end;

function toString (const aValue: TVarRec) : String;
begin
    Result := '';
    with aValue do
    try
        case VType of
            vtInteger    : Result := IntToStr (VInteger);
            vtBoolean    : Result := BooleanToStr (VBoolean);
            vtChar       : Result := VChar;
            vtExtended   : Result := FloatToStr (VExtended^);
            vtString     : Result := VString^;
            vtPointer    : Result := IntToStr ( Longint (VPointer) );
            vtPChar      : Result := StrPas (VPChar);
            vtObject     : if Assigned (VObject) then
                               Result := VObject.ClassName;
            vtClass      : if Assigned (VClass) then
                               Result := VClass.ClassName;
            vtAnsiString : Result := String (VAnsiString);
            vtWideChar   : Result := Char (VWideChar);
            vtPWideChar  : Result := WideCharToString (VPWideChar);
            vtWideString : Result := WideCharToString (VWideString);
            vtCurrency   : Result := FloatToStr (VCurrency^);
            vtInt64      : Result := IntToStr (VInt64^);
            vtVariant    : Result := VVariant^;
        end;
    except
        Result := '';
    end;
end;

function toExtended (const aValue: TVarRec) : Extended;
begin
    Result := 0.0;
    with aValue do
    try
        case VType of
            vtExtended   : Result := VExtended^;
            vtCurrency   : Result := VCurrency^;
            vtVariant    : Result := Extended (VVariant^);
            vtInteger    : Result := VInteger;
            vtInt64      : Result := Integer (VInt64^);
            vtPointer    : Result := Longint (VPointer);
            vtObject     : Result := Longint (VObject);
            vtClass      : Result := Longint (VClass);
            vtInterface  : Result := Longint (VInterface);
            vtBoolean    : Result := BooleanToInt (VBoolean);
            vtString     : Result := StrToFloat (VString^);
            vtChar       : Result := StrToFloat (VChar);
            vtPChar      : Result := StrToFloat ( StrPas (VPChar) );
            vtAnsiString : Result := StrToFloat ( String (VAnsiString) );
            vtWideChar   : Result := StrToFloat ( Char (VWideChar) );
            vtPWideChar  : Result := StrToFloat ( WideCharToString (VPWideChar) );
            vtWideString : Result := StrToFloat ( WideCharToString (VWideString) );
        end;
    except
        Result := 0.0;
    end;
end;

function toDouble (const aValue: TVarRec) : Double;
begin
    Result := toExtended (aValue);
end;

function toInt64 (const aValue: TVarRec) : Int64;
begin
    Result := 0;
    with aValue do
    try
        case VType of
            vtInt64      : Result := VInt64^;
            vtInteger    : Result := VInteger;
            vtExtended   : Move ( VExtended^, Result, SizeOf (Int64) );
            vtVariant    : Result := Integer (VVariant^);
            vtPointer    : Result := Longint (VPointer);
            vtObject     : Result := Longint (VObject);
            vtClass      : Result := Longint (VClass);
            vtInterface  : Result := Longint (VInterface);
            vtBoolean    : Result := BooleanToInt (VBoolean);
            vtString     : Result := StrToInt64 (VString^);
            vtChar       : Result := StrToInt64 (VChar);
            vtPChar      : Result := StrToInt64 ( StrPas (VPChar) );
            vtAnsiString : Result := StrToInt64 ( String (VAnsiString) );
            vtWideChar   : Result := StrToInt64 ( Char (VWideChar) );
            vtPWideChar  : Result := StrToInt64 ( WideCharToString (VPWideChar) );
            vtWideString : Result := StrToInt64 ( WideCharToString (VWideString) );
        end;
    except
        Result := 0;
    end;
end;

function toArrayOfConst (const aValue: TVarRec) : array_of_const;
begin
    Result := _([]);
    with aValue do
    try
        case VType of
            vtPointer : Result := VPointer;
        end;
    except
        Result := _([]);
    end;
end;

function _array_of_const (anArgs: array of const) : array_of_const;
var
    I      : WORD;
    Index  : WORD;
    Length : WORD;
begin
    Length := 0;
    if ( High (anArgs) >= 0 ) then
        Length := Length + High (anArgs) - Low (anArgs) +1;
    if ( Length > 0 ) then
    begin
        SetLength (Result,Length);
        Index := 0;
        for I := Low (anArgs) to High (anArgs) do
        begin
            Result [Index] := anArgs [I];
            Inc (Index);
        end;
    end;
end;

function _(anArgs: array of const) : array_of_const;
begin
    Result := _array_of_const (anArgs);
end;

{ merge array of const }
function merge (anArgs1, anArgs2: array of const) : array_of_const;
var
    I      : WORD;
    Index  : WORD;
    Length : WORD;
begin
    Result := _([]);
    try
        Length := 0;
        if ( High (anArgs1) >= 0 ) then
            Length := Length + High (anArgs1) - Low (anArgs1) +1;
        if ( High (anArgs2) >= 0 ) then
            Length := Length + High (anArgs2) - Low (anArgs2) +1;
        if ( Length > 0 ) then
        begin
            SetLength (Result,Length);
            Index := 0;
            if ( High (anArgs1) >= 0 ) then
                for I := Low (anArgs1) to High (anArgs1) do
                begin
                    Result [Index] := anArgs1 [I];
                    Inc (Index);
                end;
            if ( High (anArgs2) >= 0 ) then
                for I := Low (anArgs2) to High (anArgs2) do
                begin
                    Result [Index] := anArgs2 [I];
                    Inc (Index);
                end;
        end;
    except
        Result := _([]);
    end;
end;

function _(anArgs1, anArgs2: array of const) : array_of_const;
begin
    Result := merge (anArgs1,anArgs2);
end;

function isEmpty (const anIndex: Integer; anArgs: array of const) : Boolean;
begin
    Result := (  ( High (anArgs) < anIndex ) or
                 ( toString (anArgs [anIndex]) = 'NULL' )  );
end;

function notEmpty (const anIndex: Integer; anArgs: array of const) : Boolean;
begin
    Result := (  ( High (anArgs) >= anIndex ) and
                 ( toString (anArgs [anIndex]) <> 'NULL' )  );
end;


end.
