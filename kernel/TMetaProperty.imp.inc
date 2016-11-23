{******************************************************************************}
{*  TMetaProperty.imp.inc                                                     *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TMetaProperty }
class function TMetaProperty.toDataType (const aValue: TVarRec) : TDataType;
const
    DATA_TYPE_STRING : array [dtUID..dtText] of String = (
        'dtUID',
        'dtID',
        'dtText'
    );
var
    I : TDataType;
begin
    Result := inherited toDataType (aValue);
    if ( Result = dtUnknown ) then
    try
        { as Integer }
        Result := toInteger (aValue);
        if ( Result < dtUnknown ) or ( Result > High (DATA_TYPE_STRING) ) then
            Result := dtUnknown;
        { as String }
        if ( Result = dtUnknown ) then
            for I := Low (DATA_TYPE_STRING) to High (DATA_TYPE_STRING) do
                if ( toString (aValue) = DATA_TYPE_STRING [I] ) then
                begin
                    Result := I;
                    Break;
                end;
    except
        Result := dtUnknown;
    end;
end;

class function TMetaProperty.toDataValue (const aType: TDataType;
                                          const aValue: TVarRec) : Variant;
begin
    Result := NULL;
    try
        case aType of
            dtUID,
            dtID   : Result := toInt64 (aValue);
            dtText : Result := toString (aValue);
            else     Result := inherited toDataValue (aType,aValue);
        end;
    except
        Result := NULL;
        raise;
    end;
end;

class function TMetaProperty.toDBType (const aType: TDataType) : String;
begin
    Result := '';
    try
        case aType of
            dtBoolean  : Result := 'INTEGER';
            dtInteger  : Result := 'INTEGER';
            dtInt64    : Result := 'INTEGER';
            dtExtended : Result := 'FLOAT';
            dtDouble   : Result := 'FLOAT';
            dtDateTime : Result := 'INTEGER';
            dtDate     : Result := 'INTEGER';
            dtTime     : Result := 'INTEGER';
            dtString   : Result := 'TEXT';
            dtHex      : Result := 'TEXT';
            dtVersion  : Result := 'INTEGER';
            dtStream   : Result := 'BLOB';
            dtCrypto   : Result := 'TEXT';
            dtSafe     : Result := 'TEXT';
            dtUID      : Result := 'INTEGER'+' NOT NULL'{+' UNIQUE'}+' PRIMARY KEY'{+' AUTOINCREMENT'};
            dtID       : Result := 'INTEGER';
            dtText     : Result := 'TEXT';
            else        raise Exception.Create (ERR_TMETAPROPERTY_UNKNOWN_TYPE);
        end;
    except
        Result := '';
        raise;
    end;
end;

function TMetaProperty.GetDBtype : String;
begin
    Result := '';
    try
        Result := toDBType (DataType);
    except on E : Exception do
        _raise (['GetDBtype',ERR_TMETAPROPERTY_GET_DB_TYPE,E],
                ['{27D61DC2-4572-48AC-95BA-E7A0C6635F78}']);
    end;
end;

function TMetaProperty.GetAsID : TID;
begin
    Result := 0;
    try
        case DataType of
            dtUID,
            dtID   : Result := Value;
            else     Result := GetAsInt64;
        end;
    except on E : Exception do
        _raise (['GetAsID',ERR_TMETAPROPERTY_GET_VALUE,E],
                ['{28AA4415-CC81-427B-A439-8884F91190FD}']);
    end;
end;

procedure TMetaProperty.SetAsID (const aValue: TID);
begin
    try
        if not ReadOnly then
            case DataType of
                dtUID,
                dtID   : Value := aValue;
                else     SetAsInt64 (aValue);
            end;
    except on E : Exception do
        _raise (['SetAsID',ERR_TMETAPROPERTY_SET_VALUE,E],
                ['{F18311A6-9644-45AF-BE41-579160B7E8BD}']);
    end;
end;

function TMetaProperty.GetAsText : String;
begin
    Result := '';
    try
        case DataType of
            dtUID,
            dtID   : Result := IntToStr (Value);
            dtText : Result := Value;
            else     Result := GetAsString;
        end;
    except on E : Exception do
        _raise (['GetAsText',ERR_TMETAPROPERTY_GET_VALUE,E],
                ['{E72743E1-3033-46E4-AEDA-F3213A2F8834}']);
    end;
end;

procedure TMetaProperty.SetAsText (const aValue: String);
begin
    try
        if not ReadOnly then
            case DataType of
                dtUID,
                dtID   : Value := StrToInt (aValue);
                dtText : Value := aValue;
                else     SetAsString (aValue);
            end;
    except on E : Exception do
        _raise (['SetAsText',ERR_TMETAPROPERTY_SET_VALUE,E],
                ['{FBB36A68-CBAC-49F5-8C8A-DF5FC345C3E0}']);
    end;
end;

function TMetaProperty.GetAsDBFormat : String;
begin
    Result := '';
    try
        case DataType of
            dtBoolean   : Result := IntToStr ( BooleanToInt (Value) );
            dtInteger   : Result := IntToStr (Value);
            dtInt64     : Result := IntToStr (Value);
            dtExtended,
            dtDouble    : Result := _FloatToStr (Value);
            dtDateTime  : Result := IntToStr ( DateTimeToUnix (Value) );
            dtDate      : Result := IntToStr (  DateTimeToUnix ( Trunc (Value) )  );
            dtTime      : Result := IntToStr (  DateTimeToUnix ( Frac (Value) )  );
            dtString    : Result := Value;
            dtHex       : Result := Value;
            dtVersion   : Result := LPad (  IntToStr ( TVersionVarData (Value).VVersion.asInteger ), 8, '0'  );
            dtCrypto,
            dtSafe      : Result := Value;
            dtUID,
            dtID        : Result := IntToStr (Value);
            dtText      : Result := StrToHex (Value);
            dtStream    : raise Exception.Create (ERR_TMETAPROPERTY_CAN_NOT_READ_STREAM);
            else          raise Exception.Create (ERR_TMETAPROPERTY_UNKNOWN_TYPE);
        end;
    except on E : Exception do
        _raise (['GetAsDBFormat',ERR_TMETAPROPERTY_GET_VALUE,E],
                ['{B6768582-8E04-4C3A-A7D9-AA844BCB5D58}']);
    end;
end;

procedure TMetaProperty.SetAsDBFormat (const aValue: String);
begin
    try
        if not ReadOnly then
            case DataType of
                dtBoolean   : Value := IntToBoolean ( StrToInt (aValue) );
                dtInteger   : Value := StrToInt (aValue);
                dtInt64     : Value := StrToInt (aValue);
                dtExtended,
                dtDouble    : Value := _StrToFloat (aValue);
                dtDateTime  : Value := UnixToDateTime ( StrToInt (aValue) );
                dtDate      : Value := Trunc (  UnixToDateTime ( StrToInt (aValue) )  );
                dtTime      : Value := Frac (  UnixToDateTime ( StrToInt (aValue) )  );
                dtString    : Value := aValue;
                dtHex       : Value := aValue;
                dtVersion   : TVersionVarData (Value).VVersion.asInteger := StrToInt (aValue);
                dtCrypto,
                dtSafe      : Value := aValue;
                dtUID,
                dtID        : Value := StrToInt (aValue);
                dtText      : Value := HexToStr (aValue);
                dtStream    : raise Exception.Create (ERR_TMETAPROPERTY_CAN_NOT_WRITE_STREAM);
                else          raise Exception.Create (ERR_TMETAPROPERTY_UNKNOWN_TYPE);
            end;
    except on E : Exception do
        _raise (['GetAsDBFormat',ERR_TMETAPROPERTY_SET_VALUE,E],
                ['{D140593D-292B-4904-944B-82D647651AE0}']);
    end;
end;

function TMetaProperty.GetAsBoolean : Boolean;
begin
    Result := FALSE;
    try
        case DataType of
            dtUID,
            dtID   : Result := IntToBoolean (Value);
            dtText : Result := _StrToBoolean (Value);
            else     Result := inherited GetAsBoolean;
        end;
    except on E : Exception do
        _raise (['GetAsBoolean',ERR_TMETAPROPERTY_GET_VALUE,E],
                ['{A97DA4AC-70D6-4459-A1D0-9276BB8D9129}']);
    end;
end;

procedure TMetaProperty.SetAsBoolean (const aValue: Boolean);
begin
    try
        if not ReadOnly then
            case DataType of
                dtUID,
                dtID   : Value := BooleanToInt (aValue);
                dtText : Value := _BooleanToStr (aValue);
                else     inherited SetAsBoolean (aValue);
            end;
    except on E : Exception do
        _raise (['SetAsBoolean',ERR_TMETAPROPERTY_SET_VALUE,E],
                ['{4359D31A-3E2A-4E10-ABC2-A573054F636F}']);
    end;
end;

function TMetaProperty.GetAsInteger : Integer;
begin
    Result := 0;
    try
        case DataType of
            dtUID,
            dtID   : Result := Value;
            dtText : Result := StrToInt (Value);
            else     Result := inherited GetAsInteger;
        end;
    except on E : Exception do
        _raise (['GetAsInteger',ERR_TMETAPROPERTY_GET_VALUE,E],
                ['{E39D841C-14FE-4595-98B8-290C621996CD}']);
    end;
end;

procedure TMetaProperty.SetAsInteger (const aValue: Integer);
begin
    try
        if not ReadOnly then
            case DataType of
                dtUID,
                dtID   : Value := aValue;
                dtText : Value := IntToStr (aValue);
                else     inherited SetAsInteger (aValue);
            end;
    except on E : Exception do
        _raise (['SetAsInteger',ERR_TMETAPROPERTY_SET_VALUE,E],
                ['{11987D6F-BFB2-47E6-A995-5DF3E839FB33}']);
    end;
end;

function TMetaProperty.GetAsInt64 : Int64;
begin
    Result := 0;
    try
        case DataType of
            dtUID,
            dtID   : Result := Value;
            dtText : Result := StrToInt (Value);
            else     Result := inherited GetAsInt64;
        end;
    except on E : Exception do
        _raise (['GetAsInt64',ERR_TMETAPROPERTY_GET_VALUE,E],
                ['{3B0300AE-9DB2-49E8-B819-7DAD2B665458}']);
    end;
end;

procedure TMetaProperty.SetAsInt64 (const aValue: Int64);
begin
    try
        if not ReadOnly then
            case DataType of
                dtUID,
                dtID   : Value := aValue;
                dtText : Value := IntToStr (aValue);
                else     inherited SetAsInt64 (aValue);
            end;
    except on E : Exception do
        _raise (['SetAsInt64',ERR_TMETAPROPERTY_SET_VALUE,E],
                ['{EF94DC49-FC51-4343-97A0-68B843F2C821}']);
    end;
end;

function TMetaProperty.GetAsExtended : Extended;
begin
    Result := 0;
    try
        case DataType of
            dtUID,
            dtID   : Result := Value;
            dtText : Result := _StrToFloat (Value);
            else     Result := inherited GetAsExtended;
        end;
    except on E : Exception do
        _raise (['GetAsExtended',ERR_TMETAPROPERTY_GET_VALUE,E],
                ['{5074BC4C-5F0C-4D8C-A69D-0AA31E8CB6CE}']);
    end;
end;

procedure TMetaProperty.SetAsExtended (const aValue: Extended);
begin
    try
        if not ReadOnly then
            case DataType of
                dtUID,
                dtID   : Value := Round (aValue);
                dtText : Value := _FloatToStr (aValue);
                else     inherited SetAsExtended (aValue);
            end;
    except on E : Exception do
        _raise (['SetAsExtended',ERR_TMETAPROPERTY_SET_VALUE,E],
                ['{2DB0AA34-E76B-4341-BD72-A0A41CD1C89B}']);
    end;
end;

function TMetaProperty.GetAsDouble : Double;
begin
    Result := 0;
    try
        case DataType of
            dtUID,
            dtID   : Result := Value;
            dtText : Result := _StrToFloat (Value);
            else     Result := inherited GetAsDouble;
        end;
    except on E : Exception do
        _raise (['GetAsDouble',ERR_TMETAPROPERTY_GET_VALUE,E],
                ['{4C334A93-FCB2-47C9-88EC-7A1D871EBF17}']);
    end;
end;

procedure TMetaProperty.SetAsDouble (const aValue: Double);
begin
    try
        if not ReadOnly then
            case DataType of
                dtUID,
                dtID   : Value := Round (aValue);
                dtText : Value := _FloatToStr (aValue);
                else     inherited SetAsDouble (aValue);
            end;
    except on E : Exception do
        _raise (['SetAsDouble',ERR_TMETAPROPERTY_SET_VALUE,E],
                ['{FA71C964-1C8B-48D6-9B2D-443835FC37A9}']);
    end;
end;

function TMetaProperty.GetAsDateTime : TDateTime;
begin
    Result := 0;
    try
        case DataType of
            dtUID,
            dtID   : Result := UnixToDateTime (Value);
            dtText : Result := _StrToDateTime (Value);
            else     Result := inherited GetAsDateTime;
        end;
    except on E : Exception do
        _raise (['GetAsDateTime',ERR_TMETAPROPERTY_GET_VALUE,E],
                ['{87D61F48-077C-44BA-B364-BCF1CB4F23DA}']);
    end;
end;

procedure TMetaProperty.SetAsDateTime (const aValue: TDateTime);
begin
    try
        if not ReadOnly then
            case DataType of
                dtUID,
                dtID   : Value := DateTimeToUnix (aValue);
                dtText : Value := _DateTimeToStr (aValue);
                else     inherited SetAsDateTime (aValue);
            end;
    except on E : Exception do
        _raise (['SetAsDateTime',ERR_TMETAPROPERTY_SET_VALUE,E],
                ['{401E50A1-E3F2-4200-9150-EC7C3A8DE045}']);
    end;
end;

function TMetaProperty.GetAsDate : TDateTime;
begin
    Result := 0;
    try
        case DataType of
            dtUID,
            dtID   : Result := Trunc ( UnixToDateTime (Value) );
            dtText : Result := _StrToDate (Value);
            else     Result := inherited GetAsDate;
        end;
    except on E : Exception do
        _raise (['GetAsDate',ERR_TMETAPROPERTY_GET_VALUE,E],
                ['{BFCFCA46-8B46-427A-BDF9-5539B1D856B3}']);
    end;
end;

procedure TMetaProperty.SetAsDate (const aValue: TDateTime);
begin
    try
        if not ReadOnly then
            case DataType of
                dtUID,
                dtID   : Value := DateTimeToUnix ( Trunc (aValue) );
                dtText : Value := _DateToStr (aValue);
                else     inherited SetAsDate (aValue);
            end;
    except on E : Exception do
        _raise (['SetAsDate',ERR_TMETAPROPERTY_SET_VALUE,E],
                ['{FDD65CD4-1A15-41EC-8CAC-333BE7E54926}']);
    end;
end;

function TMetaProperty.GetAsTime : TDateTime;
begin
    Result := 0;
    try
        case DataType of
            dtUID,
            dtID   : Result := Frac ( UnixToDateTime (Value) );
            dtText : Result := _StrToTime (Value);
            else     Result := inherited GetAsTime;
        end;
    except on E : Exception do
        _raise (['GetAsTime',ERR_TMETAPROPERTY_GET_VALUE,E],
                ['{D53B0589-8CBF-4B01-B386-77A926AA092E}']);
    end;
end;

procedure TMetaProperty.SetAsTime (const aValue: TDateTime);
begin
    try
        if not ReadOnly then
            case DataType of
                dtUID,
                dtID   : Value := DateTimeToUnix ( Frac (aValue) );
                dtText : Value := _TimeToStr (aValue);
                else     inherited SetAsTime (aValue);
            end;
    except on E : Exception do
        _raise (['SetAsTime',ERR_TMETAPROPERTY_SET_VALUE,E],
                ['{E66FEE6C-BEC1-4479-BE7B-C1FD7A097751}']);
    end;
end;

function TMetaProperty.GetAsString : String;
begin
    Result := '';
    try
        case DataType of
            dtUID,
            dtID   : Result := IntToStr (Value);
            dtText : Result := Value;
            else     Result := inherited GetAsString;
        end;
    except on E : Exception do
        _raise (['GetAsString',ERR_TMETAPROPERTY_GET_VALUE,E],
                ['{224F82B5-0680-42F4-94D4-C4FEDFB01B6E}']);
    end;
end;

procedure TMetaProperty.SetAsString (const aValue: String);
begin
    try
        if not ReadOnly then
            case DataType of
                dtUID,
                dtID   : Value := StrToInt (aValue);
                dtText : Value := aValue;
                else     inherited SetAsString (aValue);
            end;
    except on E : Exception do
        _raise (['SetAsString',ERR_TMETAPROPERTY_SET_VALUE,E],
                ['{495E59ED-483B-4173-AB82-A6C649762031}']);
    end;
end;

function TMetaProperty.GetAsHex : Hex;
begin
    Result := '00';
    try
        case DataType of
            dtUID,
            dtID   : if ( Value > 0 ) then
                         Result := IntToHex (Value,8)
                     else if ( Value < 0 ) then
                         Result := IntToHex (Value,16)
                     else
                         Result := IntToHex (Value,1);
            dtText : Result := Value;
            else     Result := inherited GetAsHex;
        end;
    except on E : Exception do
        _raise (['GetAsHex',ERR_TMETAPROPERTY_GET_VALUE,E],
                ['{0CB4BE67-4211-4C54-AB44-E1F23B29D6E0}']);
    end;
end;

procedure TMetaProperty.SetAsHex (const aValue: Hex);
begin
    try
        if not ReadOnly then
            case DataType of
                dtUID,
                dtID   : Value := HexToInt (aValue);
                dtText : Value := aValue;
                else     inherited SetAsHex (aValue);
            end;
    except on E : Exception do
        _raise (['SetAsHex',ERR_TMETAPROPERTY_SET_VALUE,E],
                ['{F62FD483-F207-4A49-9574-81C17020AB98}']);
    end;
end;

function TMetaProperty.GetAsVersion : TVersionInfo;
begin
    Result := NULL_VERSION;
    try
        case DataType of
            dtUID,
            dtID   : Result := IntToVersionInfo (Value);
            dtText : Result := StrToVersionInfo (Value);
            else     Result := inherited GetAsVersion;
        end;
    except on E : Exception do
        _raise (['GetAsVersion',ERR_TMETAPROPERTY_GET_VALUE,E],
                ['{65E52B0F-1B46-414A-A07D-D1C143C0D046}']);
    end;
end;

procedure TMetaProperty.SetAsVersion (const aValue: TVersionInfo);
begin
    try
        if not ReadOnly then
            case DataType of
                dtUID,
                dtID   : Value := VersionInfoToInt (aValue);
                dtText : Value := VersionInfoToStr (aValue);
                else     inherited SetAsVersion (aValue);
            end;
    except on E : Exception do
        _raise (['SetAsVersion',ERR_TMETAPROPERTY_SET_VALUE,E],
                ['{5DABE7CC-D400-4374-A280-A8EF48A4A1F4}']);
    end;
end;

function TMetaProperty.GetAsStream : TStream;
begin
    Result := NIL;
    try
        case DataType of
            dtUID,
            dtID   : StrToStream ( Result,
                                   IntToStr (Value) );
            dtText : StrToStream (Result,Value);
            else     Result := inherited GetAsStream;
        end;
    except on E : Exception do
        _raise (['GetAsStream',ERR_TMETAPROPERTY_GET_VALUE,E],
                ['{D4737A2F-AF9E-4C50-9A5D-32738DD9F995}']);
    end;
end;

procedure TMetaProperty.SetAsStream (const aValue: TStream);
begin
    try
        if not ReadOnly then
            case DataType of
                dtUID,
                dtID   : Value := StrToInt ( StreamToStr ( TMemoryStream (  Pointer ( LongWord (Value) )  ) ) );
                dtText : Value := StreamToStr ( TMemoryStream (  Pointer ( LongWord (Value) )  ) );
                else     inherited SetAsStream (aValue);
            end;
    except on E : Exception do
        _raise (['SetAsStream',ERR_TMETAPROPERTY_SET_VALUE,E],
                ['{F5C4D515-2C66-4B4C-9FB8-568880CD0596}']);
    end;
end;
