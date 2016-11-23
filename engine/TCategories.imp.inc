{******************************************************************************}
{*  TCategories.imp.inc                                                       *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TCategories }
function TCategories.Check (anItem: Pointer) : Boolean;
begin
    Result := FALSE;
    try
        Result := inherited Check (anItem) and
                  TObject (anItem).InheritsFrom (TCategorie) and
                  Check ( TCategorie (anItem) );
    except on E: Exception do
        _raise (['Check',ERR_TCATEGORIES_CHECK,E],
                ['{B739C1C4-75FB-4A8C-85AF-B8A4035A5F22}']);
    end;
end;

function TCategories.Check (anItem: TCategorie) : Boolean;
begin
    Result := Assigned (anItem) and anItem.Check;
end;

function TCategories.GetItemAt (anIndex: Integer) : TCategorie;
begin
    Result := NIL;
    try
        if inherited ItemAt [anIndex].InheritsFrom (TCategorie) then
            Result := inherited ItemAt [anIndex] as TCategorie;
    except on E: Exception do
        _raise ([ Format ('GetItemAt [%d]',[anIndex]), ERR_TCATEGORIES_GET_ITEM_AT, E ],
                ['{1FA9E145-4527-47FA-BD45-A1691CE2151A}']);
    end;
end;

procedure TCategories.SetItemAt (anIndex: Integer; anItem: TCategorie);
begin
    try
        if inherited ItemAt [anIndex].InheritsFrom (TCategorie) then
            inherited ItemAt [anIndex] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemAt [%d]',[anIndex]), ERR_TCATEGORIES_SET_ITEM_AT, E ],
                ['{72EA0FDF-BEF0-4FB7-AD9E-496C02B5F58F}']);
    end;
end;

function TCategories.GetItemOf (aName: String) : TCategorie;
begin
    Result := NIL;
    try
        Result := inherited ItemOf [aName] as TCategorie;
    except on E: Exception do
        _raise ([ Format ('GetItemOf [%s]',[aName]), ERR_TCATEGORIES_GET_ITEM_OF, E ],
                ['{46D29FE4-2E73-496E-9A52-5C993A91C155}']);
    end;
end;

procedure TCategories.SetItemOf (aName: String; anItem: TCategorie);
begin
    try
        inherited ItemOf [aName] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemOf [%s]',[aName]), ERR_TCATEGORIES_SET_ITEM_OF, E ],
                ['{C880203A-F03C-4E3A-92F0-B7606DED5B12}']);
    end;
end;

function TCategories.GetItemOn (anID: TID) : TCategorie;
begin
    Result := NIL;
    try
        Result := inherited ItemOn [anID] as TCategorie;
    except on E: Exception do
        _raise ([ Format ('GetItemOn [%d]',[anID]), ERR_TCATEGORIES_GET_ITEM_ON, E ],
                ['{C4A0F624-976D-4D35-BBDA-767F47B60141}']);
    end;
end;

procedure TCategories.SetItemOn (anID: TID; anItem: TCategorie);
begin
    try
        inherited ItemOn [anID] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemOn [%d]',[anID]), ERR_TCATEGORIES_SET_ITEM_ON, E ],
                ['{D79DD549-7B0B-4C48-AED4-E33CCA2CB7DC}']);
    end;
end;

class function TCategories.ItemClassType : CMetaObject;
begin
    Result := TCategorie;
end;

function TCategories.Add (anItem: TCategorie) : Integer;
begin
    Result := -1;
    try
        Result := inherited Add (anItem);
    except on E: Exception do
        _raise (['Add',ERR_TCATEGORIES_ADD,E],
                ['{EFA1B1E5-06DA-4721-A82E-CACC2563F112}']);
    end;
end;

class procedure TCategories.Filter (var Query: String;
                                    anArgs: array of const);
var
    IDs              : TVarRec;
    TimeStampsCreate : TVarRec;
    TimeStampsModify : TVarRec;
    TimeStampsPublic : TVarRec;
    Versions         : TVarRec;
    OBJ              : TObject;
    arr              : array_of_const;
    I                : Integer;
    Props            : TProtoProperties;
    Prop             : TMetaProperty;
    IDsParents       : String;
    IDsAuthors       : String;
    IDsOwners        : String;
    IDsTypes         : String;
    IDsStatuses      : String;
begin
    try
        { первый параметр - список идентификаторов }
        if notEmpty (0,anArgs) then
            IDs := anArgs [0];
        { седьмой параметр - период создания }
        if notEmpty (6,anArgs) then
            TimeStampsCreate := anArgs [6];
        { восьмой параметр - период последнего редактирования }
        if notEmpty (7,anArgs) then
            TimeStampsModify := anArgs [7];
        { девятый параметр - период публикации }
        if notEmpty (8,anArgs) then
            TimeStampsPublic := anArgs [8];
        { десятый параметр - допустимый период версий }
        if notEmpty (9,anArgs) then
            Versions := anArgs [9];
        { накладываем фильтр по алгоритму предка }
        arr := _([ NIL, NIL, NIL, NIL, NIL ]);
        arr [0] := IDs;
        arr [1] := TimeStampsCreate;
        arr [2] := TimeStampsModify;
        arr [3] := TimeStampsPublic;
        arr [4] := Versions;
        inherited Filter (Query,arr);
        { накладываем дополнительный фильтр }
        Props := ItemClassType.GetClassProperties;
        try
            Prop := TMetaProperty (Props.ItemAt [_ctg_id_parent]);
            { второй параметр - список идентификаторов категорий-предков }
            IDsParents := '';
            if notEmpty (1,anArgs) then
            begin
                OBJ := toObject (anArgs [1]);
                if Assigned (OBJ) and OBJ.InheritsFrom (TStringList) then
                begin
                    for I := 0 to TStringList (OBJ).Count - 1 do
                    begin
                        Prop.asString := TStringList (OBJ).Strings [I];
                        if ( Prop.asID >= 0 ) then
                            if ( I = 0 ) then
                                IDsParents := Prop.asDBFormat
                            else
                                IDsParents := Format ('%s,%s',[ IDsParents, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDsParents) > 0 ) then
                        IDsParents := Format ('%s IN (%s) ',[ Prop.Name, IDsParents ]);
                end
                else
                begin
                    arr := toArrayOfConst (anArgs [1]);
                    for I := 0 to High (arr) do
                    begin
                        Prop.asID := toInt64 (arr [I]);
                        if ( Prop.asID >= 0 ) then
                            if ( I = 0 ) then
                                IDsParents := Prop.asDBFormat
                            else
                                IDsParents := Format ('%s,%s',[ IDsParents, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDsParents) > 0 ) then
                        IDsParents := Format ('%s IN (%s) ',[ Prop.Name, IDsParents ]);
                end;
            end;

            Prop := TMetaProperty (Props.ItemAt [_ctg_id_author]);
            { третий параметр - список идентификаторов авторов }
            IDsAuthors := '';
            if notEmpty (2,anArgs) then
            begin
                OBJ := toObject (anArgs [2]);
                if Assigned (OBJ) and OBJ.InheritsFrom (TStringList) then
                begin
                    for I := 0 to TStringList (OBJ).Count - 1 do
                    begin
                        Prop.asString := TStringList (OBJ).Strings [I];
                        if ( Prop.asID >= 0 ) then
                            if ( I = 0 ) then
                                IDsAuthors := Prop.asDBFormat
                            else
                                IDsAuthors := Format ('%s,%s',[ IDsAuthors, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDsAuthors) > 0 ) then
                        IDsAuthors := Format ('%s IN (%s) ',[ Prop.Name, IDsAuthors ]);
                end
                else
                begin
                    arr := toArrayOfConst (anArgs [2]);
                    for I := 0 to High (arr) do
                    begin
                        Prop.asID := toInt64 (arr [I]);
                        if ( Prop.asID >= 0 ) then
                            if ( I = 0 ) then
                                IDsAuthors := Prop.asDBFormat
                            else
                                IDsAuthors := Format ('%s,%s',[ IDsAuthors, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDsAuthors) > 0 ) then
                        IDsAuthors := Format ('%s IN (%s) ',[ Prop.Name, IDsAuthors ]);
                end;
            end;

            Prop := TMetaProperty (Props.ItemAt [_ctg_id_owner]);
            { четвертый параметр - список идентификаторов владельцев }
            IDsOwners := '';
            if notEmpty (3,anArgs) then
            begin
                OBJ := toObject (anArgs [3]);
                if Assigned (OBJ) and OBJ.InheritsFrom (TStringList) then
                begin
                    for I := 0 to TStringList (OBJ).Count - 1 do
                    begin
                        Prop.asString := TStringList (OBJ).Strings [I];
                        if ( Prop.asID >= 0 ) then
                            if ( I = 0 ) then
                                IDsOwners := Prop.asDBFormat
                            else
                                IDsOwners := Format ('%s,%s',[ IDsOwners, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDsOwners) > 0 ) then
                        IDsOwners := Format ('%s IN (%s) ',[ Prop.Name, IDsOwners ]);
                end
                else
                begin
                    arr := toArrayOfConst (anArgs [3]);
                    for I := 0 to High (arr) do
                    begin
                        Prop.asID := toInt64 (arr [I]);
                        if ( Prop.asID >= 0 ) then
                            if ( I = 0 ) then
                                IDsOwners := Prop.asDBFormat
                            else
                                IDsOwners := Format ('%s,%s',[ IDsOwners, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDsOwners) > 0 ) then
                        IDsOwners := Format ('%s IN (%s) ',[ Prop.Name, IDsOwners ]);
                end;
            end;

            Prop := TMetaProperty (Props.ItemAt [_ctg_id_type]);
            { пятый параметр - список идентификаторов типов }
            IDsTypes := '';
            if notEmpty (4,anArgs) then
            begin
                OBJ := toObject (anArgs [4]);
                if Assigned (OBJ) and OBJ.InheritsFrom (TStringList) then
                begin
                    for I := 0 to TStringList (OBJ).Count - 1 do
                    begin
                        Prop.asString := TStringList (OBJ).Strings [I];
                        if ( Prop.asID >= 0 ) then
                            if ( I = 0 ) then
                                IDsTypes := Prop.asDBFormat
                            else
                                IDsTypes := Format ('%s,%s',[ IDsTypes, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDsTypes) > 0 ) then
                        IDsTypes := Format ('%s IN (%s) ',[ Prop.Name, IDsTypes ]);
                end
                else
                begin
                    arr := toArrayOfConst (anArgs [4]);
                    for I := 0 to High (arr) do
                    begin
                        Prop.asID := toInt64 (arr [I]);
                        if ( Prop.asID >= 0 ) then
                            if ( I = 0 ) then
                                IDsTypes := Prop.asDBFormat
                            else
                                IDsTypes := Format ('%s,%s',[ IDsTypes, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDsTypes) > 0 ) then
                        IDsTypes := Format ('%s IN (%s) ',[ Prop.Name, IDsTypes ]);
                end;
            end;

            Prop := TMetaProperty (Props.ItemAt [_ctg_id_status]);
            { шестой параметр - список идентификаторов статусов }
            IDsStatuses := '';
            if notEmpty (5,anArgs) then
            begin
                OBJ := toObject (anArgs [5]);
                if Assigned (OBJ) and OBJ.InheritsFrom (TStringList) then
                begin
                    for I := 0 to TStringList (OBJ).Count - 1 do
                    begin
                        Prop.asString := TStringList (OBJ).Strings [I];
                        if ( Prop.asID >= 0 ) then
                            if ( I = 0 ) then
                                IDsStatuses := Prop.asDBFormat
                            else
                                IDsStatuses := Format ('%s,%s',[ IDsStatuses, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDsStatuses) > 0 ) then
                        IDsStatuses := Format ('%s IN (%s) ',[ Prop.Name, IDsStatuses ]);
                end
                else
                begin
                    arr := toArrayOfConst (anArgs [5]);
                    for I := 0 to High (arr) do
                    begin
                        Prop.asID := toInt64 (arr [I]);
                        if ( Prop.asID >= 0 ) then
                            if ( I = 0 ) then
                                IDsStatuses := Prop.asDBFormat
                            else
                                IDsStatuses := Format ('%s,%s',[ IDsStatuses, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDsStatuses) > 0 ) then
                        IDsStatuses := Format ('%s IN (%s) ',[ Prop.Name, IDsStatuses ]);
                end;
            end;
        finally
            Props.FreeAll;
            FreeAndNil (Props);
        end;
        if ( Length (IDsParents) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,IDsParents]);
        if ( Length (IDsAuthors) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,IDsAuthors]);
        if ( Length (IDsOwners) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,IDsOwners]);
        if ( Length (IDsTypes) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,IDsTypes]);
        if ( Length (IDsStatuses) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,IDsStatuses]);
    except on E: Exception do
        _raise (['Filter',ERR_TCATEGORIES_FILTER,E],
                ['{6606E3E7-F63D-4941-A260-51E97249C4F1}']);
    end;
end;

class procedure TCategories.OnLoad (const aDB: TSQLiteDatabase;
                                    anObject: TMetaObject;
                                    aTable: TSQLiteTable);
begin
    try
        inherited OnLoad (aDB,anObject,aTable);
        if objSimple in anObject.Mode then Exit;
        with TCategorie (anObject) do
        try
            Mode := Mode - [objLoaded];
            if ( IDParent > 0 ) then
            begin
                if Assigned (Parent) then
                begin
                    Parent.ID := IDParent;
                    Parent.Load;
                end
                else
                    Parent := TCategorie.Load (aDB,IDParent,[objSimple]) as TCategorie;
            end;
            if ( IDAuthor > 0 ) then
            begin
                Author.ID := IDAuthor;
                Author.Load;
            end;
            if ( IDType > 0 ) then
            begin
                CtgType.ID := IDType;
                CtgType.Load;
            end;
            if ( IDStatus > 0 ) then
            begin
                CtgStatus.ID := IDStatus;
                CtgStatus.Load;
            end;
            if ( IDPic > 0 ) then
            begin
                Pic.ID := IDPic;
                Pic.Load;
            end;
            Mode := Mode + [objLoaded];
        except
            Mode := Mode + [objLoadFailed];
            raise;
        end;
    except on E: Exception do
        _raise (['OnLoad',ERR_TCATEGORIES_LOAD,E],
                ['{A354D1DD-432C-4743-8FD7-7C75AB8654EE}']);
    end;
end;

{$IFNDEF DELETE_TRIGGER}
class procedure TCategories.DeleteByParent (const aDB: TSQLiteDatabase;
                                            const aParentID: TID);
var
    query                   : String;
    isBeforeTransactionOpen : Boolean;
    Props                   : TProtoProperties;
    MsgProps                : TProtoProperties;
    table                   : TSQLiteTable;
    I                       : Integer;
    IDsChildren             : String;
begin
    try
        if not Assigned (aDB) then
            raise Exception.Create (ERR_TCATEGORIES_INVALID_DATABASE);
        Props := ItemClassType.GetClassProperties;
        MsgProps := TMessage.GetClassProperties;
        try
            TMetaProperty (Props.ItemAt [_ctg_id_parent]).asID := aParentID;
            try
            { при необходимости открываем транзакцию }
                isBeforeTransactionOpen := FALSE;
                if ( not aDB.isTransactionOpen ) then
                    aDB.BeginTransaction
                else
                    isBeforeTransactionOpen := TRUE;
            { извлекаем потомки }
                { формируем запрос }
                query := Format ('SELECT %s '+
                                 'FROM %s '+
                                 'WHERE %s="%s" ',
                                 [ Props.ItemAt [_id].Name,
                                   ItemClassType.GetClassTable,
                                   Props.ItemAt [_ctg_id_parent].Name,
                                   TMetaProperty (Props.ItemAt [_ctg_id_parent]).asDBFormat ]);
                // ShowMessage (query);
                table := aDB.GetTable (query);
                with table do
                try
                    IDsChildren := '';
                    for I := 0 to table.Count-1 do
                    begin
                        { входим в рекурсию - удаляем всех потомков и их сообщения }
                        TMetaProperty (Props.ItemAt [_id]).asDBFormat := Fields [  FieldIndex [ Props.ItemAt [_id].Name ]  ];
                        TCategories.DeleteByParent ( aDB, TMetaProperty (Props.ItemAt [_id]).asID );
                        { формируем список идентификаторов потомков }
                        if ( I = 0 ) then
                            IDsChildren := TMetaProperty (Props.ItemAt [_id]).asDBFormat
                        else
                            IDsChildren := Format ('%s,%s',[ IDsChildren,
                                                             TMetaProperty (Props.ItemAt [_id]).asDBFormat ]);
                        table.Next;
                    end;
                finally
                    FreeAndNil (table);
                end;
                { если есть потомки }
                if ( Length (IDsChildren) > 0 ) then
                begin
                    { удаляем категории-потомки }
                    query := Format ('DELETE '+
                                     'FROM %s '+
                                     'WHERE %s IN (%s) ',
                                     [ ItemClassType.GetClassTable,
                                       Props.ItemAt [_id].Name, IDsChildren ]);
                    // ShowMessage (query);
                    aDB.ExecSQL (query);
                    { удаляем сообщения категорий-потомков }
                    query := Format ('DELETE '+
                                     'FROM %s '+
                                     'WHERE %s IN (%s) ',
                                     [ TMessage.GetClassTable,
                                       MsgProps.ItemAt [_msg_id_categorie].Name, IDsChildren ]);
                    // ShowMessage (query);
                    aDB.ExecSQL (query);
                end;
            { если ошибок не обнаружено - проводим транзакцию }
                if ( not isBeforeTransactionOpen ) then
                    aDB.Commit;
            { в случае возникновения ошибки - откатываем транзакцию }
            except on E: Exception do begin
                if ( not isBeforeTransactionOpen ) then
                    aDB.Rollback;
                raise Exception.CreateFmt (ERR_TCATEGORIES_TRANSACTION,[E.Message]);
            end; end;
        finally
            MsgProps.FreeAll;
            FreeAndNil (MsgProps);
            Props.FreeAll;
            FreeAndNil (Props);
        end;
    except on E: Exception do
        _raise (['DeleteByParent',ERR_TCATEGORIES_DELETE_BY_PARENT,E],
                ['{F1B73E75-302C-4CB0-A488-FA7B8E6EBA76}']);
    end;
end;
{$ENDIF DELETE_TRIGGER}

{$IFNDEF DELETE_TRIGGER}
class procedure TCategories.Delete (const aDB: TSQLiteDatabase;
                                    anArgs: array of const);
var
    query                   : String;
    isBeforeTransactionOpen : Boolean;
    Props                   : TProtoProperties;
    table                   : TSQLiteTable;
    I                       : Integer;
begin
    try
        if not Assigned (aDB) then
            raise Exception.Create (ERR_TCATEGORIES_INVALID_DATABASE);
        Props := ItemClassType.GetClassProperties;
        try
            try
            { при необходимости открываем транзакцию }
                isBeforeTransactionOpen := FALSE;
                if ( not aDB.isTransactionOpen ) then
                    aDB.BeginTransaction
                else
                    isBeforeTransactionOpen := TRUE;
                { формируем запрос }
                query := Format ('SELECT %s '+
                                 'FROM %s '+
                                 'WHERE ( %s>="0" ) ',
                                 [ Props.ItemAt [_id].Name,
                                   ItemClassType.GetClassTable,
                                   Props.ItemAt [_id].Name ]);
                { накладываем фильтр }
                Filter (query,anArgs);
                // ShowMessage (query);
                table := aDB.GetTable (query);
                with table do
                try
                    for I := 0 to table.Count-1 do
                    begin
                        { удаляем категории, всех потомков и их сообщения }
                        TMetaProperty (Props.ItemAt [_id]).asDBFormat := Fields [  FieldIndex [ Props.ItemAt [_id].Name ]  ];
                        TCategorie.Delete ( aDB, TMetaProperty (Props.ItemAt [_id]).asID );
                        table.Next;
                    end;
                finally
                    FreeAndNil (table);
                end;
            { если ошибок не обнаружено - проводим транзакцию }
                if ( not isBeforeTransactionOpen ) then
                    aDB.Commit;
            { в случае возникновения ошибки - откатываем транзакцию }
            except on E: Exception do begin
                if ( not isBeforeTransactionOpen ) then
                    aDB.Rollback;
                raise Exception.CreateFmt (ERR_TCATEGORIES_TRANSACTION,[E.Message]);
            end; end;
        finally
            Props.FreeAll;
            FreeAndNil (Props);
        end;
    except on E: Exception do
        _raise (['Delete',ERR_TCATEGORIES_DELETE,E],
                ['{24EC7702-80DA-45D3-871A-49BED1E075CD}']);
    end;
end;
{$ENDIF DELETE_TRIGGER}

class function TCategories.Find (const aDB: TSQLiteDatabase;
                                 const aHash: String;
                                 const anIDOwner: TID) : TID;
var
    query : String;
    table : TSQLiteTable;
    I     : Integer;
    Props : TProtoProperties;
begin
    Result := 0;
    try
        if not Assigned (aDB) then
            raise Exception.Create (ERR_TCATEGORIES_INVALID_DATABASE);
        Props := ItemClassType.GetClassProperties;
        try
            Props.ItemAt [_ctg_key_hash].asHex := aHash;
            Props.ItemAt [_ctg_id_owner].asInt64 := anIDOwner;

            query := Format ('SELECT %s '+
                             'FROM %s '+
                             'WHERE ( %s="%s" ) '+
                               'AND ( %s="%s" ) ',
                             [ Props.ItemAt [_id].Name,
                               ItemClassType.GetClassTable,
                               Props.ItemAt [_ctg_key_hash].Name, TMetaProperty (Props.ItemAt [_ctg_key_hash]).asDBFormat,
                               Props.ItemAt [_ctg_id_owner].Name, TMetaProperty (Props.ItemAt [_ctg_id_owner]).asDBFormat ]);
            //ShowMessage (query);
            table := aDB.GetTable (query);
            with table do
            try
                for I := 0 to table.Count-1 do
                begin
                    if ( I > 0 ) then
                        raise Exception.CreateFmt (ERR_TCATEGORIE_NOT_UNIQUE_HASH,
                                                   [ Props.ItemAt [_ctg_key_hash].Name,
                                                     TMetaProperty (Props.ItemAt [_ctg_key_hash]).asDBFormat ]);
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
        _raise (['Find',ERR_TCATEGORIES_FIND,E],
                ['{E914BEEE-0C3F-42A9-92FC-2B6731BCEB0F}']);
    end;
end;

class procedure TCategories.ReplacePic (const aDB: TSQLiteDatabase;
                                        const aHashPic: Hex;
                                        const aNewIDPic: TID;
                                        const anIDOwner: TID);
var
    query                   : String;
    isBeforeTransactionOpen : Boolean;
    Props                   : TProtoProperties;
    table                   : TSQLiteTable;
    I                       : Integer;
begin
    try
        if not Assigned (aDB) then
            raise Exception.Create (ERR_TCATEGORIES_INVALID_DATABASE);

        { проверяем наличие таблиц класса }
        ItemClassType.CreateClassTable (aDB);

        Props := ItemClassType.GetClassProperties;
        try
            try
            { при необходимости открываем транзакцию }
                isBeforeTransactionOpen := FALSE;
                if ( not aDB.isTransactionOpen ) then
                    aDB.BeginTransaction
                else
                    isBeforeTransactionOpen := TRUE;
                TMetaProperty (Props.ItemAt [_ctg_hash_pic]).asHex := aHashPic;
                TMetaProperty (Props.ItemAt [_ctg_id_pic]).asID := aNewIDPic;
                TMetaProperty (Props.ItemAt [_ctg_id_owner]).asID := anIDOwner;
                { формируем запрос }
                query := Format ('SELECT %s, %s '+
                                 'FROM %s '+
                                 'WHERE ( %s="%s" ) '+
                                 '  AND ( %s<>"%s" ) '+
                                 '  AND ( %s="%s" ) ',
                                 [ Props.ItemAt [_id].Name,
                                   Props.ItemAt [_ctg_id_pic].Name,
                                   ItemClassType.GetClassTable,
                                   Props.ItemAt [_ctg_hash_pic].Name,
                                   TMetaProperty (Props.ItemAt [_ctg_hash_pic]).asDBFormat,
                                   Props.ItemAt [_ctg_id_pic].Name,
                                   TMetaProperty (Props.ItemAt [_ctg_id_pic]).asDBFormat,
                                   Props.ItemAt [_ctg_id_owner].Name,
                                   TMetaProperty (Props.ItemAt [_ctg_id_owner]).asDBFormat ]);
                // ShowMessage (query);
                table := aDB.GetTable (query);
                with table do
                try
                    for I := 0 to table.Count-1 do
                    begin
                        TMetaProperty (Props.ItemAt [_id]).asDBFormat := Fields [  FieldIndex [ Props.ItemAt [_id].Name ]  ];
                        TMetaProperty (Props.ItemAt [_ctg_id_pic]).asDBFormat := Fields [  FieldIndex [ Props.ItemAt [_ctg_id_pic].Name ]  ];
                        { удаляем старую иконку }
                        TPics.Delete (aDB,[ _([ TMetaProperty (Props.ItemAt [_ctg_id_pic]).asID ]), _([anIDOwner]) ]);
                        { заменяем иконку }
                        TMetaProperty (Props.ItemAt [_ctg_id_pic]).asID := aNewIDPic;
                        query := Format ('UPDATE %s '+
                                         'SET %s="%s" '+
                                         'WHERE %s="%s" ',
                                         [ ItemClassType.GetClassTable,
                                           Props.ItemAt [_ctg_id_pic].Name,
                                           TMetaProperty (Props.ItemAt [_ctg_id_pic]).asDBFormat,
                                           Props.ItemAt [_id].Name,
                                           TMetaProperty (Props.ItemAt [_id]).asDBFormat ]);
                        // ShowMessage (query);
                        aDB.ExecSQL (query);
                        table.Next;
                    end;
                finally
                    FreeAndNil (table);
                end;
            { если ошибок не обнаружено - проводим транзакцию }
                if ( not isBeforeTransactionOpen ) then
                    aDB.Commit;
            { в случае возникновения ошибки - откатываем транзакцию }
            except on E: Exception do begin
                if ( not isBeforeTransactionOpen ) then
                    aDB.Rollback;
                raise Exception.CreateFmt (ERR_TCATEGORIES_TRANSACTION,[E.Message]);
            end; end;
        finally
            Props.FreeAll;
            FreeAndNil (Props);
        end;
    except on E: Exception do
        _raise (['ReplacePic',ERR_TCATEGORIES_REPLACE_PIC,E],
                ['{9CDB30BE-CA89-465B-BC6E-44F7A8ACCE08}']);
    end;
end;
