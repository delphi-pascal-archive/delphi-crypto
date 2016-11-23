{******************************************************************************}
{*  TCryptoObjects.imp.inc                                                    *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TCryptoObjects }
function TCryptoObjects.Check (anItem: Pointer) : Boolean;
begin
    Result := FALSE;
    try
        Result := inherited Check (anItem) and
                  TObject (anItem).InheritsFrom (TCryptoObject) and
                  Check ( TCryptoObject (anItem) );
    except on E: Exception do
        _raise (['Check',ERR_TCRYPTOOBJECTS_CHECK,E],
                ['{BADECAC9-57AE-4382-9F1B-DAE7B23B5DE0}']);
    end;
end;

function TCryptoObjects.Check (anItem: TCryptoObject) : Boolean;
begin
    Result := Assigned (anItem) and anItem.Check;
end;

class function TCryptoObjects.ItemClassType : CProtoObject;
begin
    Result := TCryptoObject;
end;

function TCryptoObjects.Add (anItem: TCryptoObject) : Integer;
begin
    Result := -1;
    try
        Result := inherited Add (anItem);
    except on E: Exception do
        _raise (['Add',ERR_TCRYPTOOBJECTS_ADD,E],
                ['{7EB907F8-6061-427F-A797-E995E603DAF9}']);
    end;
end;

function TCryptoObjects.GetItemAt (anIndex: Integer) : TCryptoObject;
begin
    Result := NIL;
    try
        if inherited ItemAt [anIndex].InheritsFrom (TCryptoObject) then
            Result := inherited ItemAt [anIndex] as TCryptoObject;
    except on E: Exception do
        _raise ([ Format ('GetItemAt [%d]',[anIndex]), ERR_TCRYPTOOBJECTS_GET_ITEM_AT, E ],
                ['{E8F77ECB-36D9-4031-B722-1E77005C582D}']);
    end;
end;

procedure TCryptoObjects.SetItemAt (anIndex: Integer; anItem: TCryptoObject);
begin
    try
        if inherited ItemAt [anIndex].InheritsFrom (TCryptoObject) then
            inherited ItemAt [anIndex] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemAt [%d]',[anIndex]), ERR_TCRYPTOOBJECTS_SET_ITEM_AT, E ],
                ['{8F7235CF-3968-4D4E-8E35-C0F2A4B8B33E}']);
    end;
end;

function TCryptoObjects.GetItemOf (aName: String) : TCryptoObject;
begin
    Result := NIL;
    try
        Result := inherited ItemOf [aName] as TCryptoObject;
    except on E: Exception do
        _raise ([ Format ('GetItemOf [%s]',[aName]), ERR_TCRYPTOOBJECTS_GET_ITEM_OF, E ],
                ['{F599FB8B-E1E9-492A-B64A-E19BA4F22088}']);
    end;
end;

procedure TCryptoObjects.SetItemOf (aName: String; anItem: TCryptoObject);
begin
    try
        inherited ItemOf [aName] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemOf [%s]',[aName]), ERR_TCRYPTOOBJECTS_SET_ITEM_OF, E ],
                ['{B7A13944-448C-48E6-83F8-25ED4299E36D}']);
    end;
end;
