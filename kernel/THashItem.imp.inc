{******************************************************************************}
{*  THashItem.imp.inc                                                         *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ THashItem }
class function THashItem.GetClassName : String;
begin
    Result := CLS_THASHITEM_NAME;
end;

class function THashItem.GetClassVersion : TVersionInfo;
begin
    Result := inherited GetClassVersion;
end;

class function THashItem.GetClassProperties : TProtoProperties;
begin
    Result := NIL;
    try
        Result := inherited GetClassProperties;
        with Result do
        begin
            Register ( TMetaProperty, ['key_hash',PRP_THASHITEM_KEY_HASH,dtHex,''],[] );
            Register ( TMetaProperty, ['data_hash',PRP_THASHITEM_DATA_HASH,dtHex,''],[] );
            Register ( TMetaProperty, ['time_stamp_vector',PRP_THASHITEM_TIME_STAMP_VECTOR,dtInt64,0],[] );
        end;
    except on E: Exception do
        _raise (['GetClassProperties',ERR_THASHITEM_GET_ClASS_PROPERTIES,E],
                ['{C2E7D15D-8D96-4DF5-ABED-807E9D450A7C}']);
    end;
end;

function THashItem.RegisterProperty (anArgs: array of const; aMode: TPropertyMode = []) : Integer;
begin
    Result := -1;
    try
        Result := Properties.Register ( TMetaProperty, anArgs, aMode );
    except on E: Exception do
        _raise (['RegisterProperty',ERR_THASHITEM_REGISTER_PROPERTY,E],
                ['{BB920D17-BD84-45BD-BDDB-CAC33F095C9E}']);
    end;
end;

constructor THashItem.Create (anArgs: array of const;
                              aMode: TObjectMode = []);
begin
    try
        inherited Create (anArgs,aMode);
        Mode := Mode - [objCreated];
        { первый параметр - хэш-ключ }
        if notEmpty (0,anArgs) then
            KeyHash := toString (anArgs [0]);
        { второй параметр - хэш данных }
        if notEmpty (1,anArgs) then
            DataHash := toString (anArgs [1]);
        { третий параметр - векторное время }
        if notEmpty (2,anArgs) then
            TimeStampVector := toInt64 (anArgs [2]);
        Mode := Mode + [objCreated];
    except on E: Exception do begin
        Mode := Mode + [objCreateFailed];
        _raise (['Create',ERR_THASHITEM_CREATE,E],
                ['{351CC608-6B21-459F-BBD3-A933841A355A}']);
    end; end;
end;

destructor THashItem.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_THASHITEM_DESTROY,E],
                ['{4CE5320D-8EE6-462F-B072-F771EA536BDE}']);
    end;
end;

function THashItem.Check : Boolean;
begin
    Result := TRUE;
    try
        Result := inherited Check;
        if Result then
        begin
            if not ( Length (KeyHash) > 0 ) then
                Error (ERR_THASHITEM_INVALID_KEY_HASH);
            if not ( Length (DataHash) > 0 ) then
                Error (ERR_THASHITEM_INVALID_DATA_HASH);
            Result := ( Errors.Count = 0 );
        end;
    except on E: Exception do
        _raise (['Check',ERR_THASHITEM_CHECK,E],
                ['{D4AD0B17-3312-4061-8E20-118CAF01CAC9}']);
    end;
end;

function THashItem.GetPropAt (anIndex: Integer) : TMetaProperty;
begin
    Result := NIL;
    try
        //if inherited PropAt [anIndex].InheritsFrom (TMetaProperty) then
            Result := inherited PropAt [anIndex] as TMetaProperty;
    except on E: Exception do
        _raise ([ Format ('GetPropAt [%d]',[anIndex]), ERR_THASHITEM_GET_PROPERTY_AT, E ],
                ['{BCE85F31-CCEC-4254-A3CF-A6C1C2FC8C90}']);
    end;
end;

procedure THashItem.SetPropAt (anIndex: Integer; anItem: TMetaProperty);
begin
    try
        //if inherited PropAt [anIndex].InheritsFrom (TMetaProperty) then
            inherited PropAt [anIndex] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetPropAt [%d]',[anIndex]), ERR_THASHITEM_SET_PROPERTY_AT, E ],
                ['{47742C01-ECE6-4BC5-8E88-04D4F02B35BC}']);
    end;
end;

function THashItem.GetPropOf (aName: String) : TMetaProperty;
begin
    Result := NIL;
    try
        //if inherited PropOf [aName].InheritsFrom (TMetaProperty) then
            Result := inherited PropOf [aName] as TMetaProperty;
    except on E: Exception do
        _raise ([ Format ('GetPropOf [%s]',[aName]), ERR_THASHITEM_GET_PROPERTY_OF, E ],
                ['{DEE2CDA8-3325-4830-AE26-0267D002C065}']);
    end;
end;

procedure THashItem.SetPropOf (aName: String; anItem: TMetaProperty);
begin
    try
        //if inherited PropOf [aName].InheritsFrom (TMetaProperty) then
            inherited PropOf [aName] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetPropOf [%s]',[aName]), ERR_THASHITEM_SET_PROPERTY_OF, E ],
                ['{70201DDE-0243-4BEF-9C5C-78126F4FED76}']);
    end;
end;

function THashItem.GetKeyHash : Hex;
begin
    try
        Result := PropAt [_key_hash].asHex;
    except on E: Exception do
        _raise (['GetKeyHash',ERR_THASHITEM_GET_KEY_HASH,E],
                ['{F5FC9DA3-7E63-4253-BD1A-24326F4734B4}']);
    end;
end;

procedure THashItem.SetKeyHash (const aValue: Hex);
begin
    try
        PropAt [_key_hash].asHex := aValue;
        ObjectName := aValue;
    except on E: Exception do
        _raise (['SetKeyHash',ERR_THASHITEM_SET_KEY_HASH,E],
                ['{A397460E-3842-4064-8C98-069B7CDD8BB4}']);
    end;
end;

function THashItem.GetDataHash : Hex;
begin
    try
        Result := PropAt [_data_hash].asHex;
    except on E: Exception do
        _raise (['GetDataHash',ERR_THASHITEM_GET_DATA_HASH,E],
                ['{0BE2996F-3B27-4552-9930-C34FB35E0353}']);
    end;
end;

procedure THashItem.SetDataHash (const aValue: Hex);
begin
    try
        PropAt [_data_hash].asHex := aValue;
    except on E: Exception do
        _raise (['SetDataHash',ERR_THASHITEM_SET_DATA_HASH,E],
                ['{BFC3DB8D-8EEA-4C65-9E12-2B7A457D673E}']);
    end;
end;

function THashItem.GetTimeStampVector : Int64;
begin
    try
        Result := PropAt [_time_stamp_vector].asInt64;
    except on E: Exception do
        _raise (['GetTimeStampVector',ERR_THASHITEM_GET_TIME_STAMP_VECTOR,E],
                ['{65463EAB-2A9F-4453-A065-6C9DD34193E7}']);
    end;
end;

procedure THashItem.SetTimeStampVector (const aValue: Int64);
begin
    try
        PropAt [_time_stamp_vector].asInt64 := aValue;
    except on E: Exception do
        _raise (['SetTimeStampVector',ERR_THASHITEM_SET_TIME_STAMP_VECTOR,E],
                ['{AF4BEF52-720E-4C23-A7D7-0F86B2A595E4}']);
    end;
end;
