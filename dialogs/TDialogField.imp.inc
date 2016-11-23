{******************************************************************************}
{*  TDialogField.imp.inc                                                      *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
class function TDialogField.toDataType (const aValue: TVarRec) : TDataType;
const
    DATA_TYPE_STRING : array [dtPassword..dtDirectory] of String = (
        'dtPassword',
        'dtFile',
        'dtDirectory'
    );
var
    I : TDataType;
begin
    Result := inherited toDataType (aValue);
    if ( Result = dtUnknown ) then
    try
        { as Integer }
        Result := toInteger (aValue);
        if ( Result < dtUnknown ) or ( Result > dtDirectory ) then
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

class function TDialogField.toDataValue (const aType: TDataType;
                                         const aValue: TVarRec) : Variant;
begin
    Result := NULL;
    try
        case aType of
            dtText,
            dtPassword,
            dtFile,
            dtDirectory: Result := toString (aValue);
            else         Result := inherited toDataValue (aType,aValue);
        end;
    except
        Result := NULL;
        raise;
    end;
end;

function TDialogField.GetAsString : String;
begin
    Result := '';
    try
        case DataType of
            dtText,
            dtPassword,
            dtFile,
            dtDirectory: Result := Value;
            else         inherited GetAsString;
        end;
    except on E : Exception do
        _raise (['GetAsString',ERR_TDIALOGFIELD_GET_VALUE,E],
                ['{3884D3FE-1B71-4905-BBDA-383CD1CDB289}']);
    end;
end;

procedure TDialogField.SetAsString (const aValue: String);
begin
    try
        if not ReadOnly then
            case DataType of
                dtText,
                dtPassword,
                dtFile,
                dtDirectory: Value := aValue;
                else         inherited SetAsString (aValue);
            end;
    except on E : Exception do
        _raise (['SetAsString',ERR_TDIALOGFIELD_SET_VALUE,E],
                ['{4C9F3F4C-7361-4D2A-8970-BDA0E77371B0}']);
    end;
end;

function TDialogField.GetAsDialogField : Variant;
begin
    Result := '';
    try
        case DataType of
            dtBoolean:  Result := GetAsBoolean;
            dtInteger:  Result := GetAsInteger;
            dtInt64:    Result := GetAsInt64;
            dtExtended: Result := GetAsExtended;
            dtDouble:   Result := GetAsDouble;
            dtDateTime: Result := GetAsDateTime;
            dtDate:     Result := GetAsDate;
            dtTime:     Result := GetAsTime;
            dtString:   Result := GetAsString;
            dtVersion:  Result := GetAsString;
            dtPointer,
            dtClass,
            dtObject:   Result := GetAsString;
            else        Result := Value;
        end;
    except on E : Exception do
        _raise (['GetAsDialogField',ERR_TDIALOGFIELD_GET_VALUE,E],
                ['{A1B77ECC-6B71-4C5D-A1D1-C7DE2B35AF97}']);
    end;
end;

procedure TDialogField.SetAsDialogField (const aValue: Variant);
begin
    try
        if not ReadOnly then
            case DataType of
                dtVersion:  SetAsString (aValue);
                else        Value := aValue;
            end;
    except on E : Exception do
        _raise (['SetAsDialogField',ERR_TDIALOGFIELD_SET_VALUE,E],
                ['{D7601CCD-1D1B-4801-9BD0-0596E83CAB52}']);
    end;
end;
