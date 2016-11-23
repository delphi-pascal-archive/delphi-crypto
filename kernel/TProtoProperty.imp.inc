{******************************************************************************}
{*  TProtoProperty.imp.inc                                                    *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TProtoProperty }
class procedure TProtoProperty._raise (anArgs: array of const;
                                       const anEGUID: String = '');
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

class procedure TProtoProperty._raise (anArgs: array of const;
                                       anEGUID: array of const);
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

constructor TProtoProperty.Create (aName: String;
                                   aCaption: String;
                                   aType: TDataType;
                                   aValue: TVarRec;
                                   aReadOnly: Boolean = FALSE;
                                   aVersion: String = NULL_VERSION_STRING;
                                   aMode: TPropertyMode = []);
begin
    try
        inherited Create;
        f_Name := aName;
        f_Caption := aCaption;
        f_DataType := aType;
        f_Value := toDataValue (f_DataType,aValue);
        f_Mode := aMode;
        f_Version := StrToVersionInfo (aVersion);
    except on E: Exception do
        _raise (['Create',ERR_TPROTOPROPERTY_CREATE,E],
                ['{B52F56AC-FEC1-407D-BFEE-98B6C452AFED}']);
    end;
end;

constructor TProtoProperty.Create (anArgs: array of const;
                                   aMode: TPropertyMode = []);
begin
    try
        inherited Create;
        { первый параметр - имя свойства }
        if notEmpty (0,anArgs) then
            f_Name := toString (anArgs [0]);
        { второй параметр - заголовок свойства }
        if notEmpty (1,anArgs) then
            f_Caption := toString (anArgs [1]);
        { третий параметр - тип свойства }
        if notEmpty (2,anArgs) then
            f_DataType := toDataType (anArgs [2]);
        { четвертый параметр - значение свойства }
        if notEmpty (3,anArgs) then
            f_Value := toDataValue (f_DataType,anArgs [3]);
        { пятый параметр - версия свойства }
        if notEmpty (4,anArgs) then
            f_Version := toVersionInfo (anArgs [4]);
        f_Mode := aMode;
    except on E: Exception do
        _raise (['Create',ERR_TPROTOPROPERTY_CREATE,E],
                ['{E7F67675-2498-4FE9-AEBD-91DD95A6F0DE}']);
    end;
end;

destructor TProtoProperty.Destroy;
begin
    try
        case DataType of
            dtStream : begin
                           TMemoryStream (  Pointer ( LongWord (f_Value) )  ).Free;
                           f_Value := LongWord (NIL);
                       end;
        end;
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TPROTOPROPERTY_DESTROY,E],
                ['{B7ADB2F8-DDCF-46EE-BCCE-A45A1B9CC1B9}']);
    end;
end;

procedure TProtoProperty.AssignTo (Dest: TPersistent);
begin
    try
        if Assigned (Dest) and Dest.InheritsFrom (TProtoProperty) then
        with TProtoProperty (Dest) do
        begin
            f_Name := self.f_Name;
            f_Caption := self.f_Caption;
            f_DataType := self.f_DataType;
            if ( DataType = dtStream ) then
            begin
                TMemoryStream ( TProtoProperty (Dest).asStream ).Clear;
                self.asStream.Position := 0;
                TProtoProperty (Dest).asStream.CopyFrom ( self.asStream, self.asStream.Size );
            end
            else
                f_Value := self.f_Value;
            f_ReadOnly := self.f_ReadOnly;
            f_Mode := self.f_Mode;
        end
        else inherited AssignTo (Dest);
    except on E: Exception do
        _raise (['Assign',ERR_TPROTOPROPERTY_ASSIGN,E],
                ['{9F7215C0-E104-48F3-B6AA-1D977AFFF059}']);
    end;
end;

class function TProtoProperty.toDataType (const aValue: TVarRec) : TDataType;
const
    DATA_TYPE_STRING : array [dtUnknown..dtStream] of String = (
        'dtUnknown',
        'dtBoolean',
        'dtInteger',
        'dtInt64',
        'dtExtended',
        'dtDouble',
        'dtDateTime',
        'dtDate',
        'dtTime',
        'dtString',
        'dtHex',
        'dtVersion',
        'dtPointer',
        'dtClass',
        'dtObject',
        'dtStream'
    );
var
    I : TDataType;
begin
    Result := dtUnknown;
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

class function TProtoProperty.toDataValue (const aType: TDataType;
                                           const aValue: TVarRec) : Variant;
begin
    Result := NULL;
    try
        case aType of
            dtBoolean  : Result := toBoolean (aValue);
            dtInteger  : Result := toInteger (aValue);
            dtInt64    : Result := toInt64 (aValue);
            dtExtended : Result := toExtended (aValue);
            dtDouble   : Result := toExtended (aValue);
            dtDateTime : Result := toDateTime (aValue);
            dtDate     : Result := toDate (aValue);
            dtTime     : Result := toTime (aValue);
            dtString   : Result := toString (aValue);
            dtHex      : {$IFDEF HEX_UPPER_CASE}
                         Result := UpperCase ( toString (aValue) );
                         {$ELSE}
                         Result := LowerCase ( toString (aValue) );
                         {$ENDIF HEX_UPPER_CASE}
            dtVersion  : Result := VarVersionCreate ( toVersionInfo (aValue) );
            dtPointer  : Result := LongWord ( toPointer (aValue) );
            dtClass    : Result := LongWord (  Pointer ( toClass (aValue) )  );
            dtObject   : Result := LongWord (  Pointer ( toObject (aValue) )  );
            dtStream   : Result := LongWord (  Pointer ( TMemoryStream.Create )  );
            else         raise Exception.Create (ERR_TPROTOPROPERTY_UNKNOWN_TYPE);
        end;
    except
        Result := NULL;
        raise;
    end;
end;

function TProtoProperty.GetReadOnly : Boolean;
begin
    Result := FALSE;
    try
        Result := prpReadOnly in Mode;
    except on E : Exception do
        _raise (['GetReadOnly',ERR_TPROTOPROPERTY_GET_MODE,E],
                ['{710261B3-F408-49EB-9231-6E29A0B1408A}']);
    end;
end;

procedure TProtoProperty.SetReadOnly (const aValue: Boolean);
begin
    try
        if aValue then
            Mode := Mode + [prpReadOnly]
        else
            Mode := Mode - [prpReadOnly];
    except on E : Exception do
        _raise (['SetReadOnly',ERR_TPROTOPROPERTY_SET_MODE,E],
                ['{E3CA3B9C-852F-4F78-B3C7-5B7DD782FD59}']);
    end;
end;

function TProtoProperty.GetAsVariant : Variant;
begin
    Result := NULL;
    try
        Result := Value;
    except on E : Exception do
        _raise (['GetAsVariant',ERR_TPROTOPROPERTY_GET_VALUE,E],
                ['{2061DE91-7FBA-4BFB-86AF-DC16C3D42EB3}']);
    end;
end;

procedure TProtoProperty.SetAsVariant (const aValue: Variant);
begin
    try
        if not ReadOnly then
            Value := aValue;
    except on E : Exception do
        _raise (['SetAsVariant',ERR_TPROTOPROPERTY_SET_VALUE,E],
                ['{BDCCED27-3239-4E29-A838-BD9203A4F3EE}']);
    end;
end;

function TProtoProperty.GetAsBoolean : Boolean;
begin
    Result := FALSE;
    try
        case DataType of
            dtBoolean  : Result := Value;
            dtInteger,
            dtInt64    : Result := IntToBoolean (Value);
            dtString   : Result := _StrToBoolean (Value);
            dtHex      : Result := IntToBoolean ( HexToInt (Value) );
            dtPointer  : Result := PBoolean ( LongWord (Value) )^;
            dtStream   : Result := _StrToBoolean ( StreamToStr ( TMemoryStream (  Pointer ( LongWord (Value) )  ) ) );
        end;
    except on E : Exception do
        _raise (['GetAsBoolean',ERR_TPROTOPROPERTY_GET_VALUE,E],
                ['{0E8F7931-6DB5-4801-B6DC-8EE7B5B2B9A0}']);
    end;
end;

procedure TProtoProperty.SetAsBoolean (const aValue: Boolean);
begin
    try
        if not ReadOnly then
            case DataType of
                dtBoolean  : Value := aValue;
                dtInteger,
                dtInt64    : Value := BooleanToInt (aValue);
                dtString   : Value := _BooleanToStr (aValue);
                dtHex      : Value := IntToHex ( BooleanToInt (aValue), 1 );
                dtPointer  : if _Dispose (Value) then
                                 Value := LongWord ( GetPBoolean (aValue) );
                dtStream   : StrToStream ( TMemoryStream (  Pointer ( LongWord (Value) )  ),
                                          _BooleanToStr (aValue) );
                else         Value := FALSE;
            end;
    except on E : Exception do
        _raise (['SetAsBoolean',ERR_TPROTOPROPERTY_SET_VALUE,E],
                ['{A0025D0D-4842-4C46-9F42-D07155A69B33}']);
    end;
end;

function TProtoProperty.GetAsInteger : Integer;
begin
    Result := 0;
    try      
        case DataType of
            dtBoolean   : Result := BooleanToInt (Value);
            dtInteger,
            dtInt64     : Result := Value;
            dtExtended,
            dtDouble    : Result := Round (Value);
            dtDateTime  : Result := DateTimeToUnix (Value);
            dtDate      : Result := DateTimeToUnix ( Trunc (Value) );
            dtTime      : Result := DateTimeToUnix ( Frac (Value) );
            dtString    : Result := StrToInt (Value);
            dtHex       : Result := HexToInt (Value);
            dtVersion   : Result := TVersionVarData (Value).VVersion.Major;
            dtPointer   : Result := PInteger ( LongWord (Value) )^;
            dtClass,
            dtObject    : Result := LongWord (Value);
            dtStream    : Result := StrToInt ( StreamToStr ( TMemoryStream (  Pointer ( LongWord (Value) )  ) ) );
        end;
    except on E : Exception do
        _raise (['GetAsInteger',ERR_TPROTOPROPERTY_GET_VALUE,E],
                ['{2F03DEBB-C2FC-4E82-B84C-08500A83F2FE}']);
    end;
end;

procedure TProtoProperty.SetAsInteger (const aValue: Integer);
begin
    try
        if not ReadOnly then
            case DataType of
                dtBoolean   : Value := IntToBoolean (aValue);
                dtInteger,
                dtInt64     : Value := aValue;
                dtExtended,
                dtDouble    : Value := aValue;
                dtDateTime  : Value := UnixToDateTime (aValue);
                dtDate      : Value := Trunc ( UnixToDateTime (aValue) );
                dtTime      : Value := Frac ( UnixToDateTime (aValue) );
                dtString    : Value := IntToStr (aValue);
                dtHex       : if ( aValue = 0 ) then
                                  Value := IntToHex (aValue,1)
                              else if ( aValue > 0 ) then
                                  Value := IntToHex (aValue,8)
                              else
                                  Value := IntToHex (aValue,16);
                dtVersion   : with TVersionVarData (Value).VVersion do
                              begin
                                  Major := aValue;
                                  Minor := 0;
                                  Release := 0;
                                  Build := 0;
                              end;
                dtPointer   : if _Dispose (Value) then
                                  Value := LongWord ( GetPInteger (aValue) );
                dtClass,
                dtObject    : Value := LongWord (aValue);
                dtStream    : StrToStream ( TMemoryStream (  Pointer ( LongWord (Value) )  ),
                                           IntToStr (aValue) );
                else          Value := 0;
            end;
    except on E : Exception do
        _raise (['SetAsInteger',ERR_TPROTOPROPERTY_SET_VALUE,E],
                ['{0B1A0867-30E5-45AD-A4F2-C8FAA19D1035}']);
    end;
end;

function TProtoProperty.GetAsInt64 : Int64;
begin
    Result := 0;
    try
        case DataType of
            dtBoolean  : Result := BooleanToInt (Value);
            dtInteger,
            dtInt64    : Result := Value;
            dtExtended : Result := FloatToInt64 (Value);
            dtDouble   : Result := Round (Value);
            dtDateTime : Result := DateTimeToUnix (Value);
            dtDate     : Result := DateTimeToUnix ( Trunc (Value) );
            dtTime     : Result := DateTimeToUnix ( Frac (Value) );
            dtString   : Result := StrToInt (Value);
            dtHex      : Result := HexToInt (Value);
            dtVersion  : Result := TVersionVarData (Value).VVersion.asInteger;
            dtPointer  : Result := PInt64 ( LongWord (Value) )^;
            dtClass,
            dtObject   : Result := LongWord (Value);
            dtStream   : Result := StrToInt ( StreamToStr ( TMemoryStream (  Pointer ( LongWord (Value) )  ) ) );
        end;
    except on E : Exception do
        _raise (['GetAsInt64',ERR_TPROTOPROPERTY_GET_VALUE,E],
                ['{D5E0D1EB-2678-4B1B-A869-7075691B7CED}']);
    end;
end;

procedure TProtoProperty.SetAsInt64 (const aValue: Int64);
begin
    try
        if not ReadOnly then
            case DataType of
                dtBoolean  : Value := IntToBoolean (aValue);
                dtInteger,
                dtInt64    : Value := aValue;
                dtExtended : Value := Int64ToFloat (aValue);
                dtDouble   : Value := aValue;
                dtDateTime : Value := UnixToDateTime (aValue);
                dtDate     : Value := Trunc ( UnixToDateTime (aValue) );
                dtTime     : Value := Frac ( UnixToDateTime (aValue) );
                dtString   : Value := IntToStr (aValue);
                dtHex      : if ( aValue = 0 ) then
                                 Value := IntToHex (aValue,1)
                             else if ( aValue > 0 ) then
                                 Value := IntToHex (aValue,16)
                             else
                                 Value := IntToHex (aValue,32);
                dtVersion  : TVersionVarData (Value).VVersion.asInteger := aValue;
                dtPointer  : if _Dispose (Value) then
                                 Value := LongWord ( GetPInt64 (aValue) );
                dtClass,
                dtObject   : Value := LongWord (aValue);
                dtStream   : StrToStream ( TMemoryStream (  Pointer ( LongWord (Value) )  ),
                                           IntToStr (aValue) );
                else         Value := 0;
            end;
    except on E : Exception do
        _raise (['SetAsInt64',ERR_TPROTOPROPERTY_SET_VALUE,E],
                ['{31C9E6D2-5B88-4C3F-90AE-D719C0619153}']);
    end;
end;

function TProtoProperty.GetAsExtended : Extended;
begin
    Result := 0.0;
    try
        case DataType of
            dtBoolean   : Result := BooleanToInt (Value);
            dtInteger   : Result := Value;
            dtInt64     : Result := Int64ToFloat (Value);
            dtExtended,
            dtDouble    : Result := Value;
            dtDateTime  : Result := Value;
            dtDate      : Result := Trunc (Value);
            dtTime      : Result := Frac (Value);
            dtString    : Result := _StrToFloat (Value);
            dtHex       : Result := HexToFloat (Value);
            dtVersion   : with TVersionVarData (Value).VVersion do
                              Result := Major + Minor/100;
            dtPointer   : Result := PExtended ( LongWord (Value) )^;
            dtStream    : Result := _StrToFloat ( StreamToStr ( TMemoryStream (  Pointer ( LongWord (Value) )  ) ) );
        end;
    except on E : Exception do
        _raise (['GetAsExtended',ERR_TPROTOPROPERTY_GET_VALUE,E],
                ['{7CB66252-7132-4E41-99FD-56B2D0F1A8A0}']);
    end;
end;

procedure TProtoProperty.SetAsExtended (const aValue: Extended);
begin
    try
        if not ReadOnly then
            case DataType of
                dtBoolean   : Value := IntToBoolean ( Round (aValue) );
                dtInteger   : Value := Round (aValue);
                dtInt64     : Value := FloatToInt64 (aValue);
                dtExtended,
                dtDouble    : Value := aValue;
                dtDateTime  : Value := aValue;
                dtDate      : Value := Trunc (aValue);
                dtTime      : Value := Frac (aValue);
                dtString    : Value := _FloatToStr (aValue);
                dtHex       : if ( aValue > 0 ) then
                                  Value := FloatToHex (aValue,16)
                              else if ( aValue < 0 ) then
                                  Value := FloatToHex (aValue,16)
                              else
                                  Value := FloatToHex (aValue,1);
                dtVersion   : with TVersionVarData (Value).VVersion do
                              begin
                                  Major := Trunc (aValue);
                                  Minor := Round ( Frac (aValue) * 100 );
                                  Release := 0;
                                  Build := 0;
                              end;
                dtPointer   : if _Dispose (Value) then
                                  Value := LongWord ( GetPExtended (aValue) );
                dtStream    : StrToStream ( TMemoryStream (  Pointer ( LongWord (Value) )  ),
                                            _FloatToStr (aValue) );
                else          Value := 0.0;
            end;
    except on E : Exception do
        _raise (['SetAsExtended',ERR_TPROTOPROPERTY_SET_VALUE,E],
                ['{EEFED99A-4666-43C8-94D1-7CD82D26D29C}']);
    end;
end;

function TProtoProperty.GetAsDouble : Double;
begin
    Result := 0.0;
    try
        case DataType of
            dtBoolean   : Result := BooleanToInt (Value);
            dtInteger,
            dtInt64     : Result := Value;
            dtExtended,
            dtDouble    : Result := Value;
            dtDateTime  : Result := Value;
            dtDate      : Result := Trunc (Value);
            dtTime      : Result := Frac (Value);
            dtString    : Result := _StrToFloat (Value);
            dtHex       : Result := HexToInt (Value);
            dtVersion   : with TVersionVarData (Value).VVersion do
                              Result := Major + Minor/100;
            dtPointer   : Result := PDouble ( LongWord (Value) )^;
            dtStream    : Result := _StrToFloat ( StreamToStr ( TMemoryStream (  Pointer ( LongWord (Value) )  ) ) );
        end;
    except on E : Exception do
        _raise (['GetAsDouble',ERR_TPROTOPROPERTY_GET_VALUE,E],
                ['{F8BF8EF4-44E5-4088-A9C0-2A6AAA98A048}']);
    end;
end;

procedure TProtoProperty.SetAsDouble (const aValue: Double);
begin
    try
        if not ReadOnly then
            case DataType of
                dtBoolean   : Value := IntToBoolean ( Round (aValue) );
                dtInteger,
                dtInt64     : Value := Round (aValue);
                dtExtended,
                dtDouble    : Value := aValue;
                dtDateTime  : Value := aValue;
                dtDate      : Value := Trunc (aValue);
                dtTime      : Value := Frac (aValue);
                dtString    : Value := _FloatToStr (aValue);
                dtHex       : if ( aValue > 0 ) then
                                  Value := IntToHex ( Trunc (aValue), 16 )
                              else if ( aValue < 0 ) then
                                  Value := IntToHex ( Trunc (aValue), 16 )
                              else
                                  Value := IntToHex ( Trunc (aValue), 1 );
                dtVersion   : with TVersionVarData (Value).VVersion do
                              begin
                                  Major := Trunc (aValue);
                                  Minor := Round ( Frac (aValue) * 100 );
                                  Release := 0;
                                  Build := 0;
                              end;
                dtPointer   : if _Dispose (Value) then
                                  Value := LongWord ( GetPDouble (aValue) );
                dtStream    : StrToStream ( TMemoryStream (  Pointer ( LongWord (Value) )  ),
                                            _FloatToStr (aValue) );
                else          Value := 0.0;
            end;
    except on E : Exception do
        _raise (['SetAsDouble',ERR_TPROTOPROPERTY_SET_VALUE,E],
                ['{25C86405-6CD6-421D-9113-73F11BC4A463}']);
    end;
end;

function TProtoProperty.GetAsDateTime : TDateTime;
begin
    Result := 0.0;
    try
        case DataType of
            dtInteger,
            dtInt64     : Result := UnixToDateTime (Value);
            dtExtended,
            dtDouble    : Result := Value;
            dtDateTime  : Result := Value;
            dtDate      : Result := Trunc (Value);
            dtTime      : Result := Frac (Value);
            dtString    : Result := _StrToDateTime (Value);
            dtHex       : Result := HexToDateTime (Value);
            dtPointer   : Result := PDateTime ( LongWord (Value) )^;
            dtStream    : Result := _StrToDateTime ( StreamToStr ( TMemoryStream (  Pointer ( LongWord (Value) )  ) ) );
        end;
    except on E : Exception do
        _raise (['GetAsDateTime',ERR_TPROTOPROPERTY_GET_VALUE,E],
                ['{4BED341E-6253-465E-AC1E-44155B8E8F5E}']);
    end;
end;

procedure TProtoProperty.SetAsDateTime (const aValue: TDateTime);
begin
    try
        if not ReadOnly then
            case DataType of
                dtInteger,
                dtInt64     : Value := DateTimeToUnix (aValue);
                dtExtended,
                dtDouble    : Value := aValue;
                dtDateTime  : Value := aValue;
                dtDate      : Value := Trunc (aValue);
                dtTime      : Value := Frac (aValue);
                dtString    : Value := _DateTimeToStr (aValue);
                dtHex       : Value := DateTimeToHex (aValue);
                dtPointer   : if _Dispose (Value) then
                                  Value := LongWord ( GetPDateTime (aValue) );
                dtStream    : StrToStream ( TMemoryStream (  Pointer ( LongWord (Value) )  ),
                                            _DateTimeToStr (aValue) );
                else          Value := 0.0;
            end;
    except on E : Exception do
        _raise (['SetAsDateTime',ERR_TPROTOPROPERTY_SET_VALUE,E],
                ['{2020D894-B194-47E3-B09B-F64185572160}']);
    end;
end;

function TProtoProperty.GetAsDate : TDateTime;
begin
    Result := 0.0;
    try
        Result := Trunc (GetAsDateTime);
    except on E : Exception do
        _raise (['GetAsDate',ERR_TPROTOPROPERTY_GET_VALUE,E],
                ['{A232926C-59D4-47F5-B3E0-0941CB700785}']);
    end;
end;

procedure TProtoProperty.SetAsDate (const aValue: TDateTime);
begin
    try
        if not ReadOnly then
            SetAsDateTime ( Trunc (aValue) );
    except on E : Exception do
        _raise (['SetAsDate',ERR_TPROTOPROPERTY_SET_VALUE,E],
                ['{40CAD977-A20A-4CF6-A8C0-77238C00FBEB}']);
    end;
end;

function TProtoProperty.GetAsTime : TDateTime;
begin
    Result := 0.0;
    try
        Result := Frac (GetAsDateTime);
    except on E : Exception do
        _raise (['GetAsTime',ERR_TPROTOPROPERTY_GET_VALUE,E],
                ['{808A2510-2E39-490A-B5B7-FC706E92E4F3}']);
    end;
end;

procedure TProtoProperty.SetAsTime (const aValue: TDateTime);
begin
    try
        if not ReadOnly then
            SetAsDateTime ( Frac (aValue) );
    except on E : Exception do
        _raise (['SetAsTime',ERR_TPROTOPROPERTY_SET_VALUE,E],
                ['{D491C9EF-6D5F-4C34-85FF-511D96D48D73}']);
    end;
end;

function TProtoProperty.GetAsString : String;
begin
    Result := '';
    try
        case DataType of
            dtBoolean   : Result := _BooleanToStr (Value);
            dtInteger,
            dtInt64     : Result := IntToStr (Value);
            dtExtended,
            dtDouble    : Result := _FloatToStr (Value);
            dtDateTime  : Result := _DateTimeToStr (Value);
            dtDate      : Result := _DateToStr ( Trunc (Value) );
            dtTime      : Result := _TimeToStr ( Frac (Value) );
            dtString    : Result := Value;
            dtHex       : Result := HexToStr (Value);
            dtVersion   : Result := TVersionVarData (Value).VVersion.asString;
            dtPointer   : Result := StrPas (  PChar ( LongWord (Value) )  );
            dtClass     : Result := TClass ( LongWord (Value) ).ClassName;
            dtObject    : Result := TObject ( LongWord (Value) ).ClassName;
            dtStream    : Result := StreamToStr ( TMemoryStream (  Pointer ( LongWord (Value) )  ) );
        end;
    except on E : Exception do
        _raise (['GetAsString',ERR_TPROTOPROPERTY_GET_VALUE,E],
                ['{808A2510-2E39-490A-B5B7-FC706E92E4F3}']);
    end;
end;

procedure TProtoProperty.SetAsString (const aValue: String);
begin
    try
        if not ReadOnly then
            case DataType of
                dtBoolean   : Value := _StrToBoolean (aValue);
                dtInteger,
                dtInt64     : Value := StrToInt (aValue);
                dtExtended,
                dtDouble    : Value := _StrToFloat (aValue);
                dtDateTime  : Value := _StrToDateTime (aValue);
                dtDate      : Value := _StrToDate (aValue);
                dtTime      : Value := _StrToTime (aValue);
                dtString    : Value := aValue;
                dtHex       : Value := StrToHex (aValue);
                dtVersion   : TVersionVarData (Value).VVersion.asString := aValue;
                dtPointer   : if ( Length (aValue) > 0 ) then
                                  Value := LongWord ( PChar (aValue) )
                              else
                                  Value := LongWord (NIL);
                dtStream    : StrToStream ( TMemoryStream (  Pointer ( LongWord (Value) )  ),
                                            aValue );
                else          Value := '';
            end;
    except on E : Exception do
        _raise (['SetAsString',ERR_TPROTOPROPERTY_SET_VALUE,E],
                ['{79F709A8-741A-49F2-AE92-00A1322D9E45}']);
    end;
end;

function TProtoProperty.GetAsHex : Hex;
begin
    Result := '00';
    try
        case DataType of
            dtBoolean  : Result := IntToHex ( BooleanToInt (Value), 1 );
            dtInteger  : if ( Value > 0 ) then
                             Result := IntToHex (Value,8)
                         else if ( Value < 0 ) then
                             Result := IntToHex (Value,16)
                         else
                             Result := IntToHex (Value,1);
            dtInt64    : if ( Value > 0 ) then
                             Result := IntToHex (Value,16)
                         else if ( Value < 0 ) then
                             Result := IntToHex (Value,32)
                         else
                             Result := IntToHex (Value,1);
            dtExtended : if ( Value > 0 ) then
                             Result := FloatToHex (Value,16)
                         else if ( Value < 0 ) then
                             Result := FloatToHex (Value,16)
                         else
                             Result := FloatToHex (Value,1);
            dtDouble   : if ( Value > 0 ) then
                             Result := IntToHex ( Trunc (Value), 16 )
                         else if ( Value < 0 ) then
                             Result := IntToHex ( Trunc (Value), 16 )
                         else
                             Result := IntToHex ( Trunc (Value), 1 );
            dtDateTime : Result := DateTimeToHex (Value);
            dtDate     : Result := DateTimeToHex ( Trunc (Value) );
            dtTime     : Result := DateTimeToHex ( Frac (Value) );
            dtString   : Result := StrToHex (Value);
            dtHex      : Result := Value;
            dtPointer  : Result := StrPas (  PChar ( LongWord (Value) )  );
            dtClass,
            dtObject   : Result := IntToHex ( LongWord (Value), 16 );
            dtStream   : Result := StreamToStr ( TMemoryStream (  Pointer ( LongWord (Value) )  ) );
        end;
    except on E : Exception do
        _raise (['GetAsHex',ERR_TPROTOPROPERTY_GET_VALUE,E],
                ['{4D95E27C-46D4-4315-86C8-AD74F7021755}']);
    end;
end;

procedure TProtoProperty.SetAsHex (const aValue: Hex);
begin
    try
        if not ReadOnly then
            case DataType of
                dtBoolean  : Value := IntToBoolean ( HexToInt (aValue) );
                dtInteger,
                dtInt64    : Value := HexToInt (aValue);
                dtExtended : Value := Int64ToFloat ( HexToInt (aValue) );
                dtDouble   : Value := HexToInt (aValue);
                dtDateTime : Value := HexToDateTime (aValue);
                dtDate     : Value := Trunc ( HexToDateTime (aValue) );
                dtTime     : Value := Frac ( HexToDateTime (aValue) );
                dtString   : Value := HexToStr (aValue);
                dtHex      : Value := aValue;
                dtPointer  : if ( Length (aValue) > 0 ) then
                                 Value := LongWord ( PChar (aValue) )
                             else
                                 Value := LongWord (NIL);
                dtClass,
                dtObject   : Value := HexToInt (aValue);
                dtStream   : StrToStream ( TMemoryStream (  Pointer ( LongWord (Value) )  ),
                                           aValue );
                else         Value := '00';
            end;
    except on E : Exception do
        _raise (['SetAsHex',ERR_TPROTOPROPERTY_SET_VALUE,E],
                ['{D4E6FC72-BC02-4678-93B3-261377A6CFDE}']);
    end;
end;

function TProtoProperty.GetAsVersion : TVersionInfo;
begin
    Result := NULL_VERSION;
    try
        case DataType of
            dtInteger   : with Result do
                          begin
                              Major := Value;
                              Minor := 0;
                              Release := 0;
                              Build := 0;
                          end;
            dtInt64     : Result := IntToVersionInfo (Value);
            dtExtended,
            dtDouble    : with Result do
                          begin
                              Major := Trunc (Value);
                              Minor := Round ( Frac (Value) * 100 );
                              Release := 0;
                              Build := 0;
                          end;
            dtString    : Result := StrToVersionInfo (Value);
            dtHex       : Result := HexToVersionInfo (Value);
            dtVersion   : Result := TVersionVarData (Value).VVersion.asVersion;
            dtPointer   : Result := PVersionInfo ( LongWord (Value) )^;
            dtStream    : Result := StrToVersionInfo ( StreamToStr ( TMemoryStream (  Pointer ( LongWord (Value) )  ) ) );
        end;
    except on E : Exception do
        _raise (['GetAsVersion',ERR_TPROTOPROPERTY_GET_VALUE,E],
                ['{C02D7DE9-F0FC-4220-8294-7736B1FB14DC}']);
    end;
end;

procedure TProtoProperty.SetAsVersion (const aValue: TVersionInfo);
begin
    try
        if not ReadOnly then
            case DataType of
                dtInteger   : Value := aValue.Major;
                dtInt64     : Value := VersionInfoToInt (aValue);
                dtExtended,
                dtDouble    : with aValue do
                                  Value := Major + Minor/100;
                dtString    : Value := VersionInfoToStr (aValue);
                dtHex       : Value := VersionInfoToHex (aValue);
                dtVersion   : TVersionVarData (Value).VVersion.asVersion := aValue;
                dtPointer   : if _Dispose (Value) then
                                  Value := LongWord ( GetPVersionInfo (aValue) );
                dtStream    : StrToStream ( TMemoryStream (  Pointer ( LongWord (Value) )  ),
                                            VersionInfoToStr (aValue) );
                else          TVersionVarData (Value).VVersion.asVersion := NULL_VERSION;
            end;
    except on E : Exception do
        _raise (['SetAsVersion',ERR_TPROTOPROPERTY_SET_VALUE,E],
                ['{A966B418-87D4-4AF2-929E-955DD56F8327}']);
    end;
end;

function TProtoProperty.GetAsPointer : Pointer;
begin
    Result := NIL;
    try
        case DataType of
            dtBoolean  : Result := PBoolean ( LongWord (Value) );
            dtInteger  : Result := PInteger ( LongWord (Value) );
            dtInt64    : Result := PInt64 ( LongWord (Value) );
            dtExtended : Result := PExtended ( LongWord (Value));
            dtDouble   : Result := PDouble ( LongWord (Value) );
            dtString   : Result := PChar ( LongWord (Value) );
            dtHex      : Result := PChar ( LongWord (Value) );
            dtVersion  : Result := PVersionInfo ( LongWord (Value) );
            dtPointer  : Result := Pointer ( LongWord (Value) );
            dtClass    : Result := Pointer (  TClass ( LongWord (Value) )  );
            dtObject   : Result := Pointer (  TObject ( LongWord (Value) )  );
            dtStream   : Result := Pointer (  TStream ( LongWord (Value) )  );
        end;
    except on E : Exception do
        _raise (['GetAsPointer',ERR_TPROTOPROPERTY_GET_VALUE,E],
                ['{68DF2F00-212B-4FEB-B572-19F2F3DDB248}']);
    end;
end;

procedure TProtoProperty.SetAsPointer (const aValue: Pointer);
begin
    try
        if not ReadOnly then
            case DataType of
                dtBoolean  : Value := PBoolean (aValue)^;
                dtInteger  : Value := PInteger (aValue)^;
                dtInt64    : Value := PInt64 (aValue)^;
                dtExtended : Value := PExtended (aValue)^;
                dtDouble   : Value := PDouble (aValue)^;
                dtString   : Value := LongWord ( PChar (aValue) );
                dtHex      : Value := LongWord ( PChar (aValue) );
                dtVersion  : TVersionVarData (Value).VVersion.asVersion := PVersionInfo (aValue)^;
                dtPointer  : Value := LongWord (aValue);
                dtClass    : Value := LongWord ( Addr (TClass (  Pointer ( LongWord (aValue) )  )) );
                dtObject   : Value := LongWord ( Addr (TObject (  Pointer ( LongWord (aValue) )  )) );
                dtStream   : Value := LongWord ( Addr (TStream (  Pointer ( LongWord (aValue) )  )) );
                else         Value := LongWord (NIL);
            end;
    except on E : Exception do
        _raise (['SetAsPointer',ERR_TPROTOPROPERTY_SET_VALUE,E],
                ['{0381D21C-DCEB-42BC-8511-AAFF0A0387D1}']);
    end;
end;

function TProtoProperty.GetAsClass : TClass;
begin
    Result := NIL;
    try
        case DataType of
            dtInt64    : Result := TClass (  PInt64 ( LongWord (Value) )  );
            dtHex      : Result := TClass (  Pointer ( HexToInt ( PChar ( LongWord (Value) ) ) )  );
            dtPointer,
            dtClass    : Result := TClass (  Pointer ( LongWord (Value) )  );
            dtObject   : Result := TObject (  Pointer ( LongWord (Value) )  ).ClassType;
            dtStream   : Result := TStream (  Pointer ( LongWord (Value) )  ).ClassType;
        end;
    except on E : Exception do
        _raise (['GetAsClass',ERR_TPROTOPROPERTY_GET_VALUE,E],
                ['{21878346-C166-4239-AB14-297DCFD3A11E}']);
    end;
end;

procedure TProtoProperty.SetAsClass (const aValue: TClass);
begin
    try
        if not ReadOnly then
            case DataType of
                dtInt64    : Value := LongWord ( Pointer (aValue) );
                dtHex      : Value := LongWord ( PChar (  IntToHex ( LongWord (Addr (aValue)), 8 )  ) );
                dtPointer,
                dtClass    : Value := LongWord ( Pointer (aValue) );
                else         Value := LongWord (NIL);
            end;
    except on E : Exception do
        _raise (['SetAsClass',ERR_TPROTOPROPERTY_SET_VALUE,E],
                ['{E3D949FA-B229-4BC0-80EB-658BD6605787}']);
    end;
end;

function TProtoProperty.GetAsObject : TObject;
begin
    Result := NIL;
    try
        case DataType of
            dtInt64    : Result := TObject (  PInt64 ( LongWord (Value) )  );
            dtHex      : Result := TObject (  Pointer ( HexToInt ( PChar ( LongWord (Value) ) ) )  );
            dtPointer,
            dtObject   : Result := TObject (  Pointer ( LongWord (Value) )  );
            dtStream   : Result := TMemoryStream (  Pointer ( LongWord (Value) )  );
        end;
    except on E : Exception do
        _raise (['GetAsObject',ERR_TPROTOPROPERTY_GET_VALUE,E],
                ['{75582AF9-BDB2-42FA-BB34-E339B89736A9}']);
    end;
end;

procedure TProtoProperty.SetAsObject (const aValue: TObject);
begin
    try
        if not ReadOnly then
            case DataType of
                dtInt64    : Value := LongWord ( Pointer (aValue) );
                dtHex      : Value := LongWord ( PChar (  IntToHex ( LongWord (Addr (aValue)), 8 )  ) );
                dtPointer,
                dtObject   : Value := LongWord ( Pointer (aValue) );
                dtStream   : if aValue is TStream then
                                 Value := LongWord ( Pointer (aValue) );
                else         Value := LongWord (NIL);
            end;
    except on E : Exception do
        _raise (['SetAsObject',ERR_TPROTOPROPERTY_SET_VALUE,E],
                ['{7AE4E749-B638-4FD0-BD5F-553ABC9FC0E1}']);
    end;
end;

function TProtoProperty.GetAsStream : TStream;
begin
    Result := NIL;
    try
        case DataType of
            dtBoolean   : StrToStream ( Result,
                                        _BooleanToStr (Value) );
            dtInteger,
            dtInt64     : StrToStream ( Result,
                                        IntToStr (Value) );
            dtExtended,
            dtDouble    : StrToStream ( Result,
                                        _FloatToStr (Value) );
            dtDateTime  : StrToStream ( Result,
                                        _DateTimeToStr (Value) );
            dtDate      : StrToStream ( Result,
                                        _DateToStr (Value) );
            dtTime      : StrToStream ( Result,
                                        _TimeToStr (Value) );
            dtString    : StrToStream (Result,Value);
            dtHex       : StrToStream (Result,Value);
            dtVersion   : StrToStream ( Result,
                                        TVersionVarData (Value).VVersion.asString );
            dtPointer,
            dtObject,
            dtStream    : Result := TMemoryStream (  Pointer ( LongWord (Value) )  );
        end;
    except on E : Exception do
        _raise (['GetAsStream',ERR_TPROTOPROPERTY_GET_VALUE,E],
                ['{10A924DD-BDDB-4DEC-925A-1BEB9D80989E}']);
    end;
end;

procedure TProtoProperty.SetAsStream (const aValue: TStream);
begin
    try
        if not ReadOnly then
            case DataType of
                dtBoolean   : Value := _StrToBoolean ( StreamToStr ( TMemoryStream (  Pointer ( LongWord (Value) )  ) ) );
                dtInteger,
                dtInt64     : Value := StrToInt ( StreamToStr ( TMemoryStream (  Pointer ( LongWord (Value) )  ) ) );
                dtExtended,
                dtDouble    : Value := _StrToFloat ( StreamToStr ( TMemoryStream (  Pointer ( LongWord (Value) )  ) ) );
                dtDateTime  : Value := _StrToDateTime ( StreamToStr ( TMemoryStream (  Pointer ( LongWord (Value) )  ) ) );
                dtDate      : Value := _StrToDate ( StreamToStr ( TMemoryStream (  Pointer ( LongWord (Value) )  ) ) );
                dtTime      : Value := _StrToTime ( StreamToStr ( TMemoryStream (  Pointer ( LongWord (Value) )  ) ) );
                dtString    : Value := StreamToStr ( TMemoryStream (  Pointer ( LongWord (Value) )  ) );
                dtHex       : Value := StreamToStr ( TMemoryStream (  Pointer ( LongWord (Value) )  ) );
                dtVersion   : TVersionVarData (Value).VVersion.asString := StreamToStr ( TMemoryStream (  Pointer ( LongWord (Value) )  ) );
                dtPointer   : Value := LongWord ( Pointer (aValue) );
                dtClass     : Value := LongWord ( Pointer (aValue.ClassType) );
                dtObject,
                dtStream    : Value := LongWord ( Pointer (aValue) );
                else          Value := LongWord (NIL);
            end;
    except on E : Exception do
        _raise (['SetAsStream',ERR_TPROTOPROPERTY_SET_VALUE,E],
                ['{D934C0E9-0040-4949-AACE-82FAC5A6A03D}']);
    end;
end;
