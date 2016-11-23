{******************************************************************************}
{*  THypoObjects.imp.inc                                                      *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ THypoObjects }
function THypoObjects.Check (anItem: Pointer) : Boolean;
begin
    Result := FALSE;
    try
        Result := inherited Check (anItem) and
                  TObject (anItem).InheritsFrom (THypoObject) and
                  Check ( THypoObject (anItem) );
    except on E: Exception do
        _raise (['Check',ERR_THYPOOBJECTS_CHECK,E],
                ['{188FB1BF-E986-451A-8CBF-CD61000FFE56}']);
    end;
end;

function THypoObjects.Check (anItem: THypoObject) : Boolean;
begin
    Result := Assigned (anItem) and anItem.Check;
end;

function THypoObjects.GetItemAt (anIndex: Integer) : THypoObject;
begin
    Result := NIL;
    try
        if inherited ItemAt [anIndex].InheritsFrom (THypoObject) then
            Result := inherited ItemAt [anIndex] as THypoObject;
    except on E: Exception do
        _raise ([ Format ('GetItemAt [%d]',[anIndex]), ERR_THYPOOBJECTS_GET_ITEM_AT, E ],
                ['{5307D92E-641B-4CEA-9D46-D980B6659E99}']);
    end;
end;

procedure THypoObjects.SetItemAt (anIndex: Integer; anItem: THypoObject);
begin
    try
        if inherited ItemAt [anIndex].InheritsFrom (THypoObject) then
            inherited ItemAt [anIndex] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemAt [%d]',[anIndex]), ERR_THYPOOBJECTS_SET_ITEM_AT, E ],
                ['{B022D3A5-594A-4186-ACDF-2178D81B92D0}']);
    end;
end;

function THypoObjects.GetItemOf (aName: String) : THypoObject;
begin
    Result := NIL;
    try
        Result := inherited ItemOf [aName] as THypoObject;
    except on E: Exception do
        _raise ([ Format ('GetItemOf [%s]',[aName]), ERR_THYPOOBJECTS_GET_ITEM_OF, E ],
                ['{9DE5EBB7-0A9B-404A-9DC7-803B4F2ADECC}']);
    end;
end;

procedure THypoObjects.SetItemOf (aName: String; anItem: THypoObject);
begin
    try
        inherited ItemOf [aName] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemOf [%s]',[aName]), ERR_THYPOOBJECTS_SET_ITEM_OF, E ],
                ['{E1816F5B-4FBE-4155-A172-B0DEFDFA857E}']);
    end;
end;

function THypoObjects.GetItemOn (anID: TID) : THypoObject;
begin
    Result := NIL;
    try
        Result := inherited ItemOn [anID] as THypoObject;
    except on E: Exception do
        _raise ([ Format ('GetItemOn [%d]',[anID]), ERR_THYPOOBJECTS_GET_ITEM_ON, E ],
                ['{8B12EE86-8E42-4568-BA8F-783A1A1E9D27}']);
    end;
end;

procedure THypoObjects.SetItemOn (anID: TID; anItem: THypoObject);
begin
    try
        inherited ItemOn [anID] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemOn [%d]',[anID]), ERR_THYPOOBJECTS_SET_ITEM_ON, E ],
                ['{4511AA0A-B5FF-4CB1-A49E-D95E7BD187D2}']);
    end;
end;

class function THypoObjects.ItemClassType : CMetaObject;
begin
    Result := THypoObject;
end;

function THypoObjects.Add (anItem: THypoObject) : Integer;
begin
    Result := -1;
    try
        Result := inherited Add (anItem);
    except on E: Exception do
        _raise (['Add',ERR_THYPOOBJECTS_ADD,E],
                ['{FDF22AEC-5CB4-4BF1-9C7C-7F8B63E8F671}']);
    end;
end;

class procedure THypoObjects.Filter (var Query: String;
                                     anArgs: array of const);
var
    OBJ              : TObject;
    arr              : array_of_const;
    I                : Integer;
    Props            : TProtoProperties;
    Prop             : TMetaProperty;
    TimeStampsCreate : String;
    TimeStampsModify : String;
    TimeStampsPublic : String;
    Versions         : String;
begin
    try
        inherited Filter (Query,anArgs);

        Props := ItemClassType.GetClassProperties;
        try
            Prop := TMetaProperty (Props.ItemAt [_hypo_time_stamp_create]);
            { второй параметр - период создания }
            TimeStampsCreate := '';
            if notEmpty (1,anArgs) then
            begin
                arr := toArrayOfConst (anArgs [1]);
                for I := 0 to High (arr) do
                begin
                    Prop.asDateTime := toDateTime (arr [I]);
                    if ( I = 0 ) then
                        TimeStampsCreate := Format ('%s>="%s"',[ Prop.Name, Prop.asDBFormat ])
                    else
                        TimeStampsCreate := Format ('%s AND %s<="%s"',[ TimeStampsCreate,
                                                                        Prop.Name, Prop.asDBFormat ])
                end;
            end;

            Prop := TMetaProperty (Props.ItemAt [_hypo_time_stamp_modify]);
            { третий параметр - период редактирования }
            TimeStampsModify := '';
            if notEmpty (2,anArgs) then
            begin
                arr := toArrayOfConst (anArgs [2]);
                for I := 0 to High (arr) do
                begin
                    Prop.asDateTime := toDateTime (arr [I]);
                    if ( I = 0 ) then
                        TimeStampsModify := Format ('%s>="%s"',[ Prop.Name, Prop.asDBFormat ])
                    else
                        TimeStampsModify := Format ('%s AND %s<="%s"',[ TimeStampsModify,
                                                                        Prop.Name, Prop.asDBFormat ])
                end;
            end;

            Prop := TMetaProperty (Props.ItemAt [_hypo_time_stamp_public]);
            { четвертый параметр - период редактирования }
            TimeStampsPublic := '';
            if notEmpty (3,anArgs) then
            begin
                arr := toArrayOfConst (anArgs [3]);
                for I := 0 to High (arr) do
                begin
                    Prop.asDateTime := toDateTime (arr [I]);
                    if ( I = 0 ) then
                        TimeStampsPublic := Format ('%s>="%s"',[ Prop.Name, Prop.asDBFormat ])
                    else
                        TimeStampsPublic := Format ('%s AND %s<="%s"',[ TimeStampsPublic,
                                                                        Prop.Name, Prop.asDBFormat ])
                end;
            end;

            Prop := TMetaProperty (Props.ItemAt [_hypo_version]);
            { пятый параметр - допустимый период версий }
            Versions := '';
            if notEmpty (3,anArgs) then
            begin
                arr := toArrayOfConst (anArgs [3]);
                for I := 0 to High (arr) do
                begin
                    Prop.asVersion := toVersionInfo (arr [I]);
                    if ( I = 0 ) then
                        Versions := Format ('%s>="%s"',[ Prop.Name, Prop.asDBFormat ])
                    else
                        Versions := Format ('%s AND %s<="%s"',[ Versions,
                                                                Prop.Name, Prop.asDBFormat ])
                end;
            end;
        finally
            Props.FreeAll;
            FreeAndNil (Props);
        end;

        if ( Length (TimeStampsCreate) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,TimeStampsCreate]);
        if ( Length (TimeStampsModify) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,TimeStampsModify]);
        if ( Length (TimeStampsPublic) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,TimeStampsPublic]);
        if ( Length (Versions) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,Versions]);
    except on E: Exception do
        _raise (['Filter',ERR_THYPOOBJECTS_FILTER,E],
                ['{8C32CBEC-6D42-4077-AD47-D8B8E3DD03EB}']);
    end;
end;

class function THypoObjects.Find (const aDB: TSQLiteDatabase;
                                  const aHash: Hex) : TID;
var
    query : String;
    table : TSQLiteTable;
    I     : Integer;
    Props : TProtoProperties;
begin
    Result := 0;
    try
        if not Assigned (aDB) then
            raise Exception.Create (ERR_THYPOOBJECTS_INVALID_DATABASE);
        Props := ItemClassType.GetClassProperties;
        try
            Props.ItemAt [_hypo_key_hash].asHex := aHash;
            
            query := Format ('SELECT %s '+
                             'FROM %s '+
                             'WHERE %s="%s" ',
                             [ Props.ItemAt [_id].Name,
                               ItemClassType.GetClassTable,
                               Props.ItemAt [_hypo_key_hash].Name, TMetaProperty (Props.ItemAt [_hypo_key_hash]).asDBFormat ]);
            //ShowMessage (query);
            table := aDB.GetTable (query);
            with table do
            try
                for I := 0 to table.Count-1 do
                begin
                    if ( I > 0 ) then
                        raise Exception.CreateFmt (ERR_THYPOOBJECT_NOT_UNIQUE_HASH,
                                                   [ Props.ItemAt [_hypo_key_hash].Name,
                                                     TMetaProperty (Props.ItemAt [_hypo_key_hash]).asDBFormat ]);
                    TMetaProperty (Props.ItemAt [_id]).asDBFormat := Fields [  FieldIndex [ Props.ItemAt [_id].Name ]  ];
                    Result := TMetaProperty (Props.ItemAt [_id]).asID;
                    table.Next;
                end;
            finally
                FreeAndNil (table);
            end;
        finally
            Props.FreeAll;
            FreeAndNil (Props);
        end;
    except on E: Exception do
        _raise (['Find',ERR_THYPOOBJECTS_FIND,E],
                ['{C7FFAF99-8B04-4110-B6F2-77E66B2560C2}']);
    end;
end;

class function THypoObjects.GetHashes (const aDB: TSQLiteDatabase;
                                       anArgs: array of const) : THashList;
var
    query    : String;
    table    : TSQLiteTable;
    I        : Integer;
    J        : Integer;
    HashItem : THashItem;
    Props    : TProtoProperties;
begin
    Result := NIL;
    try
        if not Assigned (aDB) then
            raise Exception.Create (ERR_THYPOOBJECTS_INVALID_DATABASE);

        { проверяем наличие таблиц класса }
        ItemClassType.CreateClassTable (aDB);

        Props := ItemClassType.GetClassProperties;
        try
            { формируем запрос }
            query := Format ('SELECT %s, %s, %s '+
                             'FROM %s '+
                             'WHERE ( %s>="0" ) ',
                             [ Props.ItemAt [_hypo_key_hash].Name,
                               Props.ItemAt [_hypo_data_hash].Name,
                               Props.ItemAt [_hypo_time_stamp_vector].Name,
                               ItemClassType.GetClassTable,
                               Props.ItemAt [_id].Name ]);
            { накладываем фильтр }
            Filter (query,anArgs);
            { устанавливаем сортировку }
            Order (query,[_hypo_key_hash]);
            // ShowMessage (query);
            table := aDB.GetTable (query);
            with table do
            try
                Result := THashList.Create ([]);

                for I := 0 to table.Count-1 do
                begin
                    HashItem := THashItem.Create ([],[objLoadFailed]);
                    with HashItem do
                    try
                        Mode := Mode - [objLoaded];
                        for J := 0 to ColCount - 1 do
                            if ( PropOf [ Columns [J] ].DataType = dtStream ) then
                            begin
                                if Assigned ( FieldAsBlob (J) ) and
                                   ( FieldAsBlob (J).Size > 0 ) then
                                begin
                                    FieldAsBlob (J).Position := 0;
                                    PropOf [ Columns [J] ].asStream.Position := 0;
                                    PropOf [ Columns [J] ].asStream.CopyFrom ( FieldAsBlob (J), FieldAsBlob (J).Size );
                                end;
                            end
                            else
                                PropOf [ Columns [J] ].asDBFormat := Fields [J];
                        Mode := Mode + [objLoaded];
                        { добавляем }
                        if ( Result.Add (HashItem) >= 0 ) then
                            HashItem.Mode := HashItem.Mode - [objLoadFailed];
                        table.Next;
                    finally
                        if ( objLoadFailed in HashItem.Mode ) then
                            FreeAndNil (HashItem);
                    end;
                end;
            finally
                FreeAndNil (table);
            end;
        finally
            Props.FreeAll;
            FreeAndNil (Props);
        end;
    except on E: Exception do
        _raise (['GetHashes',ERR_THYPOOBJECTS_GET_HASHES,E],
                ['{980FB883-F85B-4863-B55C-907F9A4EC185}']);
    end;
end;
