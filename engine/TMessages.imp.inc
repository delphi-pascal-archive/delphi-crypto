{******************************************************************************}
{*  TMessages.imp.inc                                                         *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TMessages }
function TMessages.Check (anItem: Pointer) : Boolean;
begin
    Result := FALSE;
    try
        Result := inherited Check (anItem) and
                  TObject (anItem).InheritsFrom (TMessage) and
                  Check ( TMessage (anItem) );
    except on E: Exception do
        _raise (['Check',ERR_TMESSAGES_CHECK,E],
                ['{D9FA1DEF-9F4E-417D-B320-84E0C743F93C}']);
    end;
end;

function TMessages.Check (anItem: TMessage) : Boolean;
begin
    Result := Assigned (anItem) and anItem.Check;
end;

function TMessages.GetItemAt (anIndex: Integer) : TMessage;
begin
    Result := NIL;
    try
        if inherited ItemAt [anIndex].InheritsFrom (TMessage) then
            Result := inherited ItemAt [anIndex] as TMessage;
    except on E: Exception do
        _raise ([ Format ('GetItemAt [%d]',[anIndex]), ERR_TMESSAGES_GET_ITEM_AT, E ],
                ['{13EEFC8A-23D3-48DA-81AC-2FC99FEC9489}']);
    end;
end;

procedure TMessages.SetItemAt (anIndex: Integer; anItem: TMessage);
begin
    try
        if inherited ItemAt [anIndex].InheritsFrom (TMessage) then
            inherited ItemAt [anIndex] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemAt [%d]',[anIndex]), ERR_TMESSAGES_SET_ITEM_AT, E ],
                ['{E504844C-5A92-4814-AE5D-71BF363BE636}']);
    end;
end;

function TMessages.GetItemOf (aName: String) : TMessage;
begin
    Result := NIL;
    try
        Result := inherited ItemOf [aName] as TMessage;
    except on E: Exception do
        _raise ([ Format ('GetItemOf [%s]',[aName]), ERR_TMESSAGES_GET_ITEM_OF, E ],
                ['{CD2D5183-64B5-4DEE-88C0-2C80C3C74F8C}']);
    end;
end;

procedure TMessages.SetItemOf (aName: String; anItem: TMessage);
begin
    try
        inherited ItemOf [aName] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemOf [%s]',[aName]), ERR_TMESSAGES_SET_ITEM_OF, E ],
                ['{7F07DEE4-8E71-4322-9BD1-579995AEA136}']);
    end;
end;

function TMessages.GetItemOn (anID: TID) : TMessage;
begin
    Result := NIL;
    try
        Result := inherited ItemOn [anID] as TMessage;
    except on E: Exception do
        _raise ([ Format ('GetItemOn [%d]',[anID]), ERR_TMESSAGES_GET_ITEM_ON, E ],
                ['{28AE466A-0368-44B2-812F-B9C28DAE92CB}']);
    end;
end;

procedure TMessages.SetItemOn (anID: TID; anItem: TMessage);
begin
    try
        inherited ItemOn [anID] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemOn [%d]',[anID]), ERR_TMESSAGES_SET_ITEM_ON, E ],
                ['{C716C235-9D65-40B2-B3E3-E65C76DC0986}']);
    end;
end;

class function TMessages.ItemClassType : CMetaObject;
begin
    Result := TMessage;
end;

function TMessages.Add (anItem: TMessage) : Integer;
begin
    Result := -1;
    try
        Result := inherited Add (anItem);
    except on E: Exception do
        _raise (['Add',ERR_TMESSAGES_ADD,E],
                ['{50D2DE1E-EFB9-4C7C-8711-AD88E15F73EC}']);
    end;
end;

class procedure TMessages.Filter (var Query: String;
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
    IDsCategories    : String;
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
        arr [4] := TimeStampsPublic;
        inherited Filter (Query,arr);
        { накладываем дополнительный фильтр }
        Props := ItemClassType.GetClassProperties;
        try
            Prop := TMetaProperty (Props.ItemAt [_msg_id_categorie]);
            { второй параметр - список идентификаторов категорий }
            IDsCategories := '';
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
                                IDsCategories := Prop.asDBFormat
                            else
                                IDsCategories := Format ('%s,%s',[ IDsCategories, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDsCategories) > 0 ) then
                        IDsCategories := Format ('%s IN (%s) ',[ Prop.Name, IDsCategories ]);
                end
                else
                begin
                    arr := toArrayOfConst (anArgs [1]);
                    for I := 0 to High (arr) do
                    begin
                        Prop.asID := toInt64 (arr [I]);
                        if ( Prop.asID >= 0 ) then
                            if ( I = 0 ) then
                                IDsCategories := Prop.asDBFormat
                            else
                                IDsCategories := Format ('%s,%s',[ IDsCategories, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDsCategories) > 0 ) then
                        IDsCategories := Format ('%s IN (%s) ',[ Prop.Name, IDsCategories ]);
                end;
            end;

            Prop := TMetaProperty (Props.ItemAt [_msg_id_author]);
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

            Prop := TMetaProperty (Props.ItemAt [_msg_id_owner]);
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

            Prop := TMetaProperty (Props.ItemAt [_msg_id_type]);
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

            Prop := TMetaProperty (Props.ItemAt [_msg_id_status]);
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
        if ( Length (IDsCategories) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,IDsCategories]);
        if ( Length (IDsAuthors) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,IDsAuthors]);
        if ( Length (IDsOwners) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,IDsOwners]);
        if ( Length (IDsTypes) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,IDsTypes]);
        if ( Length (IDsStatuses) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,IDsStatuses]);
    except on E: Exception do
        _raise (['Filter',ERR_TMESSAGES_FILTER,E],
                ['{BE65CE99-EEF3-401F-AFCA-0E94ADF8DEE9}']);
    end;
end;

class procedure TMessages.OnLoad (const aDB: TSQLiteDatabase;
                                  anObject: TMetaObject;
                                  aTable: TSQLiteTable);
begin
    try
        inherited OnLoad (aDB,anObject,aTable);
        if objSimple in anObject.Mode then Exit;
        with TMessage (anObject) do
        try
            Mode := Mode - [objLoaded];
            if ( IDCategorie > 0 ) then
            begin
                if Assigned (Categorie) then
                begin
                    Categorie.ID := IDCategorie;
                    Categorie.Load;
                end
                else
                    Categorie := TCategorie.Load (aDB,IDCategorie,[objSimple]) as TCategorie;
            end;
            if ( IDAuthor > 0 ) then
            begin
                Author.ID := IDAuthor;
                Author.Load;
            end;
            if ( IDType > 0 ) then
            begin
                MsgType.ID := IDType;
                MsgType.Load;
            end;
            if ( IDStatus > 0 ) then
            begin
                MsgStatus.ID := IDStatus;
                MsgStatus.Load;
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
        _raise (['OnLoad',ERR_TMESSAGES_LOAD,E],
                ['{27595C2B-CB6C-4F18-917E-F9234FB11CCB}']);
    end;
end;

{$IFNDEF DELETE_TRIGGER}
class procedure TMessages.Delete (const aDB: TSQLiteDatabase;
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
            raise Exception.Create (ERR_TMESSAGES_INVALID_DATABASE);
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
                        { удаляем сообщение и все ключевые слова }
                        TMetaProperty (Props.ItemAt [_id]).asDBFormat := Fields [  FieldIndex [ Props.ItemAt [_id].Name ]  ];
                        TMessage.Delete ( aDB, TMetaProperty (Props.ItemAt [_id]).asID );
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
                raise Exception.CreateFmt (ERR_TMESSAGES_TRANSACTION,[E.Message]);
            end; end;
        finally
            Props.FreeAll;
            FreeAndNil (Props);
        end;
    except on E: Exception do
        _raise (['Delete',ERR_TMESSAGES_DELETE,E],
                ['{4F407D68-EB31-4FE1-A3FB-C05E1CAC66A2}']);
    end;
end;
{$ENDIF DELETE_TRIGGER}

class function TMessages.Find (const aDB: TSQLiteDatabase;
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
            raise Exception.Create (ERR_TMESSAGES_INVALID_DATABASE);
        Props := ItemClassType.GetClassProperties;
        try
            Props.ItemAt [_msg_key_hash].asHex := aHash;
            Props.ItemAt [_msg_id_owner].asInt64 := anIDOwner;

            query := Format ('SELECT %s '+
                             'FROM %s '+
                             'WHERE ( %s="%s" ) '+
                               'AND ( %s="%s" ) ',
                             [ Props.ItemAt [_id].Name,
                               ItemClassType.GetClassTable,
                               Props.ItemAt [_msg_key_hash].Name, TMetaProperty (Props.ItemAt [_msg_key_hash]).asDBFormat,
                               Props.ItemAt [_msg_id_owner].Name, TMetaProperty (Props.ItemAt [_msg_id_owner]).asDBFormat ]);
            //ShowMessage (query);
            table := aDB.GetTable (query);
            with table do
            try
                for I := 0 to table.Count-1 do
                begin
                    if ( I > 0 ) then
                        raise Exception.CreateFmt (ERR_TMESSAGE_NOT_UNIQUE_HASH,
                                                   [ Props.ItemAt [_msg_key_hash].Name,
                                                     TMetaProperty (Props.ItemAt [_msg_key_hash]).asDBFormat ]);
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
        _raise (['Find',ERR_TMESSAGES_FIND,E],
                ['{8318AAD3-7153-455B-A682-098010919962}']);
    end;
end;

class procedure TMessages.ReplacePic (const aDB: TSQLiteDatabase;
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
            raise Exception.Create (ERR_TMESSAGES_INVALID_DATABASE);

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
                TMetaProperty (Props.ItemAt [_msg_hash_pic]).asHex := aHashPic;
                TMetaProperty (Props.ItemAt [_msg_id_pic]).asID := aNewIDPic;
                TMetaProperty (Props.ItemAt [_msg_id_owner]).asID := anIDOwner;
                { формируем запрос }
                query := Format ('SELECT %s, %s '+
                                 'FROM %s '+
                                 'WHERE ( %s="%s" ) '+
                                 '  AND ( %s<>"%s" ) '+
                                 '  AND ( %s="%s" ) ',
                                 [ Props.ItemAt [_id].Name,
                                   Props.ItemAt [_msg_id_pic].Name,
                                   ItemClassType.GetClassTable,
                                   Props.ItemAt [_msg_hash_pic].Name,
                                   TMetaProperty (Props.ItemAt [_msg_hash_pic]).asDBFormat,
                                   Props.ItemAt [_msg_id_pic].Name,
                                   TMetaProperty (Props.ItemAt [_msg_id_pic]).asDBFormat,
                                   Props.ItemAt [_msg_id_owner].Name,
                                   TMetaProperty (Props.ItemAt [_msg_id_owner]).asDBFormat ]);
                // ShowMessage (query);
                table := aDB.GetTable (query);
                with table do
                try
                    for I := 0 to table.Count-1 do
                    begin
                        TMetaProperty (Props.ItemAt [_id]).asDBFormat := Fields [  FieldIndex [ Props.ItemAt [_id].Name ]  ];
                        TMetaProperty (Props.ItemAt [_msg_id_pic]).asDBFormat := Fields [  FieldIndex [ Props.ItemAt [_msg_id_pic].Name ]  ];
                        { удаляем старую иконку }
                        TPics.Delete (aDB,[ _([ TMetaProperty (Props.ItemAt [_msg_id_pic]).asID ]), _([anIDOwner]) ]);
                        { заменяем иконку }
                        TMetaProperty (Props.ItemAt [_msg_id_pic]).asID := aNewIDPic;
                        query := Format ('UPDATE %s '+
                                         'SET %s="%s" '+
                                         'WHERE %s="%s" ',
                                         [ ItemClassType.GetClassTable,
                                           Props.ItemAt [_msg_id_pic].Name,
                                           TMetaProperty (Props.ItemAt [_msg_id_pic]).asDBFormat,
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
                raise Exception.CreateFmt (ERR_TMESSAGES_TRANSACTION,[E.Message]);
            end; end;
        finally
            Props.FreeAll;
            FreeAndNil (Props);
        end;
    except on E: Exception do
        _raise (['ReplacePic',ERR_TMESSAGES_REPLACE_PIC,E],
                ['{B38CDAF5-D6A1-4322-9A65-E74F91E65185}']);
    end;
end;
