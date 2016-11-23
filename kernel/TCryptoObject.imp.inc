{******************************************************************************}
{*  TCryptoObject.imp.inc                                                     *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TCryptoObject }
class function TCryptoObject.GetClassName : String;
begin
    Result := CLS_TCRYPTOOBJECT_NAME;
end;

class function TCryptoObject.GetClassVersion : TVersionInfo;
begin
    Result := inherited GetClassVersion;
end;

function TCryptoObject.RegisterProperty (anArgs: array of const; aMode: TPropertyMode = []) : Integer;
begin
    Result := -1;
    try
        Result := Properties.Add ( TCryptoProperty.Create (anArgs,aMode) );
    except on E: Exception do
        _raise (['RegisterProperty',ERR_TCRYPTOOBJECT_REGISTER_PROPERTY,E],
                ['{8D59824A-22EE-4045-A945-5298ED862233}']);
    end;
end;

function TCryptoObject.GetPropAt (anIndex: Integer) : TCryptoProperty;
begin
    Result := NIL;
    try
        //if inherited PropAt [anIndex].InheritsFrom (TCryptoProperty) then
            Result := inherited PropAt [anIndex] as TCryptoProperty;
    except on E: Exception do
        _raise ([ Format ('GetPropAt [%d]',[anIndex]), ERR_TCRYPTOOBJECT_GET_PROPERTY_AT, E ],
                ['{1A084153-9762-441D-AD14-2448277A44A4}']);
    end;
end;

procedure TCryptoObject.SetPropAt (anIndex: Integer; anItem: TCryptoProperty);
begin
    try
        //if inherited PropAt [anIndex].InheritsFrom (TCryptoProperty) then
            inherited PropAt [anIndex] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetPropAt [%d]',[anIndex]), ERR_TCRYPTOOBJECT_SET_PROPERTY_AT, E ],
                ['{373A023B-26BB-4929-8033-86F88A84A954}']);
    end;
end;

function TCryptoObject.GetPropOf (aName: String) : TCryptoProperty;
begin
    Result := NIL;
    try
        //if inherited PropOf [aName].InheritsFrom (TCryptoProperty) then
            Result := inherited PropOf [aName] as TCryptoProperty;
    except on E: Exception do
        _raise ([ Format ('GetPropOf [%s]',[aName]), ERR_TCRYPTOOBJECT_GET_PROPERTY_OF, E ],
                ['{BA4333F0-E4BA-4BE5-985D-CBFFE038005F}']);
    end;
end;

procedure TCryptoObject.SetPropOf (aName: String; anItem: TCryptoProperty);
begin
    try
        //if inherited PropOf [aName].InheritsFrom (TCryptoProperty) then
            inherited PropOf [aName] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetPropOf [%s]',[aName]), ERR_TCRYPTOOBJECT_SET_PROPERTY_OF, E ],
                ['{DD9EFF72-93FD-447A-9114-CE6347B44801}']);
    end;
end;