{******************************************************************************}
{*  TParaObjects.imp.inc                                                      *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TParaObjects }
function TParaObjects.Check (anItem: Pointer) : Boolean;
begin
    Result := FALSE;
    try
        Result := inherited Check (anItem) and
                  TObject (anItem).InheritsFrom (TParaObject) and
                  Check ( TParaObject (anItem) );
    except on E: Exception do
        _raise (['Check',ERR_TPARAOBJECTS_CHECK,E],
                ['{FE0D8E22-7D0B-4E1F-A4A2-C32F7CEFA415}']);
    end;
end;

function TParaObjects.Check (anItem: TParaObject) : Boolean;
begin
    Result := Assigned (anItem) and anItem.Check;
end;

function TParaObjects.GetItemAt (anIndex: Integer) : TParaObject;
begin
    Result := NIL;
    try
        if inherited ItemAt [anIndex].InheritsFrom (TParaObject) then
            Result := inherited ItemAt [anIndex] as TParaObject;
    except on E: Exception do
        _raise ([ Format ('GetItemAt [%d]',[anIndex]), ERR_TPARAOBJECTS_GET_ITEM_AT, E ],
                ['{AAE7278A-1E8D-4A36-AD6B-70A1F8BF6F5F}']);
    end;
end;

procedure TParaObjects.SetItemAt (anIndex: Integer; anItem: TParaObject);
begin
    try
        if inherited ItemAt [anIndex].InheritsFrom (TParaObject) then
            inherited ItemAt [anIndex] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemAt [%d]',[anIndex]), ERR_TPARAOBJECTS_SET_ITEM_AT, E ],
                ['{44047CFE-E670-43E0-8E86-F08F1DAEB667}']);
    end;
end;

function TParaObjects.GetItemOf (aName: String) : TParaObject;
begin
    Result := NIL;
    try
        Result := inherited ItemOf [aName] as TParaObject;
    except on E: Exception do
        _raise ([ Format ('GetItemOf [%s]',[aName]), ERR_TPARAOBJECTS_GET_ITEM_OF, E ],
                ['{B260F0D9-7E33-4AE5-81D6-B0FED9ABB1DC}']);
    end;
end;

procedure TParaObjects.SetItemOf (aName: String; anItem: TParaObject);
begin
    try
        inherited ItemOf [aName] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemOf [%s]',[aName]), ERR_TPARAOBJECTS_SET_ITEM_OF, E ],
                ['{11FF032C-4BBA-4CF5-91A9-591F274AF2FA}']);
    end;
end;

function TParaObjects.GetItemOn (anID: TID) : TParaObject;
begin
    Result := NIL;
    try
        Result := inherited ItemOn [anID] as TParaObject;
    except on E: Exception do
        _raise ([ Format ('GetItemOn [%d]',[anID]), ERR_TPARAOBJECTS_GET_ITEM_ON, E ],
                ['{049DAA51-CB3B-43A7-872A-FBAE4E272C87}']);
    end;
end;

procedure TParaObjects.SetItemOn (anID: TID; anItem: TParaObject);
begin
    try
        inherited ItemOn [anID] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemOn [%d]',[anID]), ERR_TPARAOBJECTS_SET_ITEM_ON, E ],
                ['{874368E9-2641-4501-8ABD-204E40EC3BC4}']);
    end;
end;

class function TParaObjects.ItemClassType : CMetaObject;
begin
    Result := TParaObject;
end;

function TParaObjects.Add (anItem: TParaObject) : Integer;
begin
    Result := -1;
    try
        Result := inherited Add (anItem);
    except on E: Exception do
        _raise (['Add',ERR_TPARAOBJECTS_ADD,E],
                ['{5AAB0511-93C6-44B4-85A7-DA0AF89AC21B}']);
    end;
end;

function TParaObjects.AddLoad (const anIDExternal: String) : Integer;
var
    OBJ : TParaObject;
begin
    Result := -1;
    try
        OBJ := TParaObject (ItemClassType).Load (DB,anIDExternal);
        try
            Result := Add (OBJ);
        finally
            if ( Result < 0 ) then
                FreeAndNil (OBJ);
        end;
    except on E: Exception do
        _raise (['AddLoad',ERR_TPARAOBJECTS_ADD_LOAD,E],
                ['{8DA0CEFE-BC28-4C65-B731-2B1DCBCF8F51}']);
    end;
end;

class procedure TParaObjects.Filter (var Query: String;
                                     anArgs: array of const);
var
    OBJ         : TObject;
    arr         : array_of_const;
    I           : Integer;
    Props       : TProtoProperties;
    Prop        : TMetaProperty;
    IDsExternal : String;
begin
    try
        inherited Filter (Query,anArgs);

        Props := ItemClassType.GetClassProperties;
        try
            Prop := TMetaProperty (Props.ItemAt [_id_external]);
            { второй параметр - список пара-идентификаторов }
            IDsExternal := '';
            if notEmpty (1,anArgs) then
            begin
                OBJ := toObject (anArgs [1]);
                if Assigned (OBJ) and OBJ.InheritsFrom (TStringList) then
                begin
                    for I := 0 to TStringList (OBJ).Count - 1 do
                    begin
                        Prop.asString := TStringList (OBJ).Strings [I];
                        if ( I = 0 ) then
                            IDsExternal := Format ('%s="%s"',[ Prop.Name, Prop.asDBFormat ])
                        else
                            IDsExternal := Format ('%s OR %s="%s"',[ IDsExternal,
                                                                     Prop.Name, Prop.asDBFormat ]);
                    end;
                end
                else
                begin
                    arr := toArrayOfConst (anArgs [1]);
                    for I := 0 to High (arr) do
                    begin
                        Prop.asString := toString (arr [I]);
                        if ( I = 0 ) then
                            IDsExternal := Format ('%s="%s"',[ Prop.Name, Prop.asDBFormat ])
                        else
                            IDsExternal := Format ('%s OR %s="%s"',[ IDsExternal,
                                                                     Prop.Name, Prop.asDBFormat ]);
                    end;
                end;
            end;
        finally
            Props.FreeAll;
            FreeAndNil (Props);
        end;
        if ( Length (IDsExternal) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,IDsExternal]);
    except on E: Exception do
        _raise (['Filter',ERR_TPARAOBJECTS_FILTER,E],
                ['{11FB0DF6-B638-403C-9BB3-DDC53F5864EA}']);
    end;
end;

class procedure TParaObjects.OnLoad (const aDB: TSQLiteDatabase;
                                     anObject: TMetaObject;
                                     aTable: TSQLiteTable);
begin
    try
        inherited OnLoad (aDB,anObject,aTable);
        with TParaObject (anObject) do
        try
            Mode := Mode - [objLoaded];
            { ставим в соответствие наименованию объекта его пара-идентификатор }
            Properties.ItemAt [_object_name].asString := PropAt [_id_external].asString;
            Mode := Mode + [objLoaded];
        except
            Mode := Mode + [objLoadFailed];
            raise;
        end;
    except on E: Exception do
        _raise (['OnLoad',ERR_TPARAOBJECTS_LOAD,E],
                ['{BBEBE913-4437-4A54-BD3A-714B2EF21E67}']);
    end;
end;
