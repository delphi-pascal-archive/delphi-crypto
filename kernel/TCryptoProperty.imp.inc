{******************************************************************************}
{*  TCryptoProperty.imp.inc                                                   *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TCryptoProperty }
class function TCryptoProperty.toDataType (const aValue: TVarRec) : TDataType;
const
    DATA_TYPE_STRING : array [dtCrypto..dtSafe] of String = (
        'dtCrypto',
        'dtSafe'
    );
var
    I : TDataType;
begin
    Result := inherited toDataType (aValue);
    if ( Result = dtUnknown ) then
    try
        { as Integer }
        Result := toInteger (aValue);
        if ( Result < dtUnknown ) or ( High (DATA_TYPE_STRING) > dtSafe ) then
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

class function TCryptoProperty.toDataValue (const aType: TDataType;
                                            const aValue: TVarRec) : Variant;
begin
    Result := NULL;
    try
        case aType of
            dtCrypto,
            dtSafe   : Result := '';
            else       Result := inherited toDataValue (aType,aValue);
        end;
    except
        Result := NULL;
        raise;
    end;
end;

function TCryptoProperty.GetAsCrypto (aPublicKey: String;
                                      aPrivateKey: String;
                                      aCrypto: TProtoObject) : String;
begin
    Result := '';
    try
        if not ( Assigned (aCrypto) and aCrypto.InheritsFrom (TCrypto) and aCrypto.Check ) then
            raise Exception.Create (ERR_TCRYPTOPROPERTY_INVALID_CRYPTO);
        case DataType of
            dtCrypto : Result := TCrypto (aCrypto).Decrypt (Value,
                                                            aPublicKey,
                                                            aPrivateKey);
            else       Result := TCrypto (aCrypto).Decrypt (GetAsString,
                                                            aPublicKey,
                                                            aPrivateKey);
        end;
    except on E : Exception do
        _raise (['GetAsCrypto',ERR_TCRYPTOPROPERTY_GET_VALUE,E],
                ['{A77A68CC-0600-4927-B323-AF34F2FE6B0D}']);
    end;
end;

procedure TCryptoProperty.SetAsCrypto (aPublicKey: String;
                                       aPrivateKey: String;
                                       aCrypto: TProtoObject;
                                       const aValue: String);
begin
    try
        if not ( Assigned (aCrypto) and aCrypto.InheritsFrom (TCrypto) and aCrypto.Check ) then
            raise Exception.Create (ERR_TCRYPTOPROPERTY_INVALID_CRYPTO);
        if not ReadOnly then
            case DataType of
                dtCrypto : Value := TCrypto (aCrypto).Encrypt (aValue,
                                                               aPublicKey,
                                                               aPrivateKey);
                else       SetAsString ( TCrypto (aCrypto).Encrypt (aValue,
                                                                    aPublicKey,
                                                                    aPrivateKey) );
            end;
    except on E : Exception do
        _raise (['SetAsCrypto',ERR_TCRYPTOPROPERTY_SET_VALUE,E],
                ['{1FBA01A5-2CCC-41AD-B2CF-6C0E9DBE16C8}']);
    end;
end;

function TCryptoProperty.GetAsSafe (aKey: String;
                                    aCrypto: TProtoObject) : String;
begin
    Result := '';
    try
        if not ( Assigned (aCrypto) and aCrypto.InheritsFrom (TCrypto) and aCrypto.Check ) then
            raise Exception.Create (ERR_TCRYPTOPROPERTY_INVALID_CRYPTO);
        case DataType of
            dtSafe : Result := TCrypto (aCrypto).Decrypt (Value,
                                                          aKey);
            else     Result := TCrypto (aCrypto).Decrypt (GetAsString,
                                                          aKey);
        end;
    except on E : Exception do
        _raise (['GetAsSafe',ERR_TCRYPTOPROPERTY_GET_VALUE,E],
                ['{E63695D2-39E9-4CC9-B081-029D94C5E7A4}']);
    end;
end;

procedure TCryptoProperty.SetAsSafe (aKey: String;
                                     aCrypto: TProtoObject;
                                     const aValue: String);
begin
    try
        if not ( Assigned (aCrypto) and aCrypto.InheritsFrom (TCrypto) and aCrypto.Check ) then
            raise Exception.Create (ERR_TCRYPTOPROPERTY_INVALID_CRYPTO);
        if not ReadOnly then
            case DataType of
                dtSafe : Value := TCrypto (aCrypto).Encrypt (aValue,
                                                             aKey);
                else     SetAsString ( TCrypto (aCrypto).Encrypt (aValue,
                                                                  aKey) );
            end;
    except on E : Exception do
        _raise (['SetAsSafe',ERR_TCRYPTOPROPERTY_SET_VALUE,E],
                ['{B81109F1-D18F-4EEC-A4DB-3D51B9F4B4E5}']);
    end;
end;
