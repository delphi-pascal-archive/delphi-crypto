{******************************************************************************}
{*  TProtoObjects.imp.inc                                                     *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TProtoObjects }
constructor TProtoObjects.Create (anArgs: array of const);
var
    I : Integer;
begin
    try
        inherited Create;
        { параметры - список параметров-конструкторов объектов }
        for I := Low (anArgs) to High (anArgs) do
            if notEmpty (I,anArgs) then
                Add ( toArrayOfConst (anArgs [I]) );
    except on E: Exception do
        _raise (['Create',ERR_TPROTOOBJECTS_CREATE,E],
                ['{7A20F0D2-22FD-4544-9A46-90C14F3CB5D1}']);
    end;
end;

destructor TProtoObjects.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TPROTOOBJECTS_DESTROY,E],
                ['{F4D4DE20-2508-4DD9-88A5-06E6BE56F3BB}']);
    end;
end;

function TProtoObjects.Check (anItem: Pointer) : Boolean;
begin
    Result := FALSE;
    try
        Result := inherited Check (anItem) and
                  TObject (anItem).InheritsFrom (TProtoObject) and
                  Check ( TProtoObject (anItem) );
    except on E: Exception do
        _raise (['Check',ERR_TPROTOOBJECTS_CHECK,E],
                ['{38335E8D-54E5-4110-B98F-814440E3EDEC}']);
    end;
end;

function TProtoObjects.Check (anItem: TProtoObject) : Boolean;
begin
    Result := TRUE;
    try
        Result := Assigned (anItem) and anItem.Check;
    except on E: Exception do
        _raise (['Check', ERR_TPROTOOBJECTS_CHECK, E ],
                ['{3F4B2110-487F-4BA9-9A34-996F10B69EB8}']);
    end;
end;

function TProtoObjects.GetItemAt (anIndex: Integer) : TProtoObject;
begin
    Result := NIL;
    try
        Result := TProtoObject (inherited Item [anIndex]);
    except on E: Exception do
        _raise ([ Format ('GetItemAt [%d]',[anIndex]), ERR_TPROTOOBJECTS_GET_ITEM_AT, E ],
                ['{E2DB9716-A9E9-4301-9C25-EC85368CB1D5}']);
    end;
end;

procedure TProtoObjects.SetItemAt (anIndex: Integer; anItem: TProtoObject);
begin
    try
        inherited Item [anIndex] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemAt [%d]',[anIndex]), ERR_TPROTOOBJECTS_SET_ITEM_AT, E ],
                ['{844F407A-65DD-4346-843E-8BC7968DD235}']);
    end;
end;

function TProtoObjects.GetItemOf (aName: String) : TProtoObject;
var
    I : Integer;
begin
    Result := NIL;
    try
        for I := 0 to Count - 1 do
            if ( TProtoObject (Item [I]).ObjectName = aName ) then
            begin
                Result := TProtoObject (Item [I]);
                Break;
            end;
    except on E: Exception do
        _raise ([ Format ('GetItemOf [%s]',[aName]), ERR_TPROTOOBJECTS_GET_ITEM_OF, E ],
                ['{8592E44E-5238-45FA-9B99-53C4A14EFF3D}']);
    end;
end;

procedure TProtoObjects.SetItemOf (aName: String; anItem: TProtoObject);
var
    I : Integer;
begin
    try
        for I := 0 to Count - 1 do
            if ( TProtoObject (Item [I]).ObjectName = aName ) then
            begin
                Item [I] := anItem;
                Break;
            end;
    except on E: Exception do
        _raise ([ Format ('SetItemOf [%s]',[aName]), ERR_TPROTOOBJECTS_SET_ITEM_OF, E ],
                ['{4DE5B1EC-DFB9-4ABD-B389-B28EFF41209B}']);
    end;
end;

class function TProtoObjects.ItemClassType : CProtoObject;
begin
    Result := TProtoObject;
end;

function TProtoObjects.Add (anItem: TProtoObject) : Integer;
begin
    Result := -1;
    try
        Result := inherited Add (anItem);
    except on E: Exception do
        _raise (['Add',ERR_TPROTOOBJECTS_ADD,E],
                ['{9E7C4DED-EB0E-42ED-ABCF-BAD429A2C053}']);
    end;
end;

function TProtoObjects.Add (anArgs: array of const;
                            aMode: TObjectMode = []) : Integer;
var
    OBJ : TProtoObject;
begin
    Result := -1;
    try
        OBJ := ItemClassType.Create (anArgs,aMode);
        try
            Result := Add (OBJ);
        finally
            if ( Result < 0 ) then
                FreeAndNil (OBJ);
        end;
    except on E: Exception do
        _raise (['Add',ERR_TPROTOOBJECTS_ADD,E],
                ['{0CF9AB66-E34D-4FB6-AAA8-316B0DB1EA5E}']);
    end;
end;
