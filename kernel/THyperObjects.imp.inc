{******************************************************************************}
{*  THyperObjects.imp.inc                                                     *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ THyperObjects }
function THyperObjects.Check (anItem: Pointer) : Boolean;
begin
    Result := FALSE;
    try
        Result := inherited Check (anItem) and
                  TObject (anItem).InheritsFrom (THyperObject) and
                  Check ( THyperObject (anItem) );
    except on E: Exception do
        _raise (['Check',ERR_THYPEROBJECTS_CHECK,E],
                ['{F1D83766-D1E1-4D02-8452-678D6D361ED6}']);
    end;
end;

function THyperObjects.Check (anItem: THyperObject) : Boolean;
begin
    Result := Assigned (anItem) and anItem.Check;
end;

function THyperObjects.GetItemAt (anIndex: Integer) : THyperObject;
begin
    Result := NIL;
    try
        if inherited ItemAt [anIndex].InheritsFrom (THyperObject) then
            Result := inherited ItemAt [anIndex] as THyperObject;
    except on E: Exception do
        _raise ([ Format ('GetItemAt [%d]',[anIndex]), ERR_THYPEROBJECTS_GET_ITEM_AT, E ],
                ['{A5C9F409-3BB1-4F9E-BC2A-9D7512B68D43}']);
    end;
end;

procedure THyperObjects.SetItemAt (anIndex: Integer; anItem: THyperObject);
begin
    try
        if inherited ItemAt [anIndex].InheritsFrom (THyperObject) then
            inherited ItemAt [anIndex] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemAt [%d]',[anIndex]), ERR_THYPEROBJECTS_SET_ITEM_AT, E ],
                ['{15C33728-20AB-44DC-89D7-60D7FC3E26D6}']);
    end;
end;

function THyperObjects.GetItemOf (aName: String) : THyperObject;
begin
    Result := NIL;
    try
        Result := inherited ItemOf [aName] as THyperObject;
    except on E: Exception do
        _raise ([ Format ('GetItemOf [%s]',[aName]), ERR_THYPEROBJECTS_GET_ITEM_OF, E ],
                ['{A065EED1-2F7E-405C-9789-E979FCBA5DDE}']);
    end;
end;

procedure THyperObjects.SetItemOf (aName: String; anItem: THyperObject);
begin
    try
        inherited ItemOf [aName] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemOf [%s]',[aName]), ERR_THYPEROBJECTS_SET_ITEM_OF, E ],
                ['{DBD69A9D-5830-4166-98CB-E938A90B17D2}']);
    end;
end;

function THyperObjects.GetItemOn (anID: TID) : THyperObject;
begin
    Result := NIL;
    try
        Result := inherited ItemOn [anID] as THyperObject;
    except on E: Exception do
        _raise ([ Format ('GetItemOn [%d]',[anID]), ERR_THYPEROBJECTS_GET_ITEM_ON, E ],
                ['{5DB570E9-7AEB-4F12-96EC-F23AE6D842AC}']);
    end;
end;

procedure THyperObjects.SetItemOn (anID: TID; anItem: THyperObject);
begin
    try
        inherited ItemOn [anID] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemOn [%d]',[anID]), ERR_THYPEROBJECTS_SET_ITEM_ON, E ],
                ['{FC82A242-8099-451B-8212-71033195AC8C}']);
    end;
end;

class function THyperObjects.ItemClassType : CMetaObject;
begin
    Result := THyperObject;
end;

function THyperObjects.Add (anItem: THyperObject) : Integer;
begin
    Result := -1;
    try
        Result := inherited Add (anItem);
    except on E: Exception do
        _raise (['Add',ERR_THYPEROBJECTS_ADD,E],
                ['{383E6315-EFCC-4EB5-8C75-7CAE97205550}']);
    end;
end;

class procedure THyperObjects.Filter (var Query: String;
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
            Prop := TMetaProperty (Props.ItemAt [_hyper_time_stamp_create]);
            { третий параметр - период создания }
            TimeStampsCreate := '';
            if notEmpty (2,anArgs) then
            begin
                arr := toArrayOfConst (anArgs [2]);
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

            Prop := TMetaProperty (Props.ItemAt [_hyper_time_stamp_modify]);
            { четвертый параметр - период редактирования }
            TimeStampsModify := '';
            if notEmpty (3,anArgs) then
            begin
                arr := toArrayOfConst (anArgs [3]);
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

            Prop := TMetaProperty (Props.ItemAt [_hyper_time_stamp_public]);
            { пятый параметр - период редактирования }
            TimeStampsPublic := '';
            if notEmpty (4,anArgs) then
            begin
                arr := toArrayOfConst (anArgs [4]);
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

            Prop := TMetaProperty (Props.ItemAt [_hyper_version]);
            { шестой параметр - допустимый период версий }
            Versions := '';
            if notEmpty (5,anArgs) then
            begin
                arr := toArrayOfConst (anArgs [5]);
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
        _raise (['Filter',ERR_THYPEROBJECTS_FILTER,E],
                ['{815D4427-9F43-4289-8656-42ADFB86617F}']);
    end;
end;

class function THyperObjects.Find (const aDB: TSQLiteDatabase;
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
            raise Exception.Create (ERR_THYPEROBJECTS_INVALID_DATABASE);
        Props := ItemClassType.GetClassProperties;
        try
            Props.ItemAt [_hyper_key_hash].asHex := aHash;
            
            query := Format ('SELECT %s '+
                             'FROM %s '+
                             'WHERE %s="%s" ',
                             [ Props.ItemAt [_id].Name,
                               ItemClassType.GetClassTable,
                               Props.ItemAt [_hyper_key_hash].Name, TMetaProperty (Props.ItemAt [_hyper_key_hash]).asDBFormat ]);
            //ShowMessage (query);
            table := aDB.GetTable (query);
            with table do
            try
                for I := 0 to table.Count-1 do
                begin
                    if ( I > 0 ) then
                        raise Exception.CreateFmt (ERR_THYPEROBJECT_NOT_UNIQUE_HASH,
                                                   [ Props.ItemAt [_hyper_key_hash].Name,
                                                     TMetaProperty (Props.ItemAt [_hyper_key_hash]).asDBFormat ]);
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
        _raise (['Find',ERR_THYPEROBJECTS_FIND,E],
                ['{5063E34C-930F-43C9-8572-2A0967D6B3AE}']);
    end;
end;

class function THyperObjects.GetHashes (const aDB: TSQLiteDatabase;
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
            raise Exception.Create (ERR_THYPEROBJECTS_INVALID_DATABASE);

        { проверяем наличие таблиц класса }
        ItemClassType.CreateClassTable (aDB);

        Props := ItemClassType.GetClassProperties;
        try
            { формируем запрос }
            query := Format ('SELECT %s, %s, %s '+
                             'FROM %s '+
                             'WHERE ( %s>="0" ) ',
                             [ Props.ItemAt [_hyper_key_hash].Name,
                               Props.ItemAt [_hyper_data_hash].Name,
                               Props.ItemAt [_hyper_time_stamp_vector].Name,
                               ItemClassType.GetClassTable,
                               Props.ItemAt [_id].Name ]);
            { накладываем фильтр }
            Filter (query,anArgs);
            { устанавливаем сортировку }
            Order (query,[_hyper_key_hash]);
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
        _raise (['GetHashes',ERR_THYPEROBJECTS_GET_HASHES,E],
                ['{576C6E70-0C42-4867-ACE1-42662FC9595C}']);
    end;
end;
