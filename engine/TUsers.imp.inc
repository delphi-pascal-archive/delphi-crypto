{******************************************************************************}
{*  TUsers.imp.inc                                                            *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011-2012                                        *}
{******************************************************************************}
{ TUsers }
function TUsers.Check (anItem: Pointer) : Boolean;
begin
    Result := FALSE;
    try
        Result := inherited Check (anItem) and
                  TObject (anItem).InheritsFrom (TUser) and
                  Check ( TUser (anItem) );
    except on E: Exception do
        _raise (['Check',ERR_TUSERS_CHECK,E],
                ['{D761EB79-BD56-4322-8E8A-581639BB1201}']);
    end;
end;

function TUsers.Check (anItem: TUser) : Boolean;
begin
    Result := Assigned (anItem) and anItem.Check;
end;

function TUsers.GetItemAt (anIndex: Integer) : TUser;
begin
    Result := NIL;
    try
        if inherited ItemAt [anIndex].InheritsFrom (TUser) then
            Result := inherited ItemAt [anIndex] as TUser;
    except on E: Exception do
        _raise ([ Format ('GetItemAt [%d]',[anIndex]), ERR_TUSERS_GET_ITEM_AT, E ],
                ['{2AC6770D-636B-47D1-ACD4-D47AD827BCBC}']);
    end;
end;

procedure TUsers.SetItemAt (anIndex: Integer; anItem: TUser);
begin
    try
        if inherited ItemAt [anIndex].InheritsFrom (TUser) then
            inherited ItemAt [anIndex] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemAt [%d]',[anIndex]), ERR_TUSERS_SET_ITEM_AT, E ],
                ['{027BD28E-C883-482D-AC1E-330487169871}']);
    end;
end;

function TUsers.GetItemOf (aName: String) : TUser;
begin
    Result := NIL;
    try
        Result := inherited ItemOf [aName] as TUser;
    except on E: Exception do
        _raise ([ Format ('GetItemOf [%s]',[aName]), ERR_TUSERS_GET_ITEM_OF, E ],
                ['{C422AC79-0B79-4991-B309-F9B34D616B9D}']);
    end;
end;

procedure TUsers.SetItemOf (aName: String; anItem: TUser);
begin
    try
        inherited ItemOf [aName] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemOf [%s]',[aName]), ERR_TUSERS_SET_ITEM_OF, E ],
                ['{603E8ACD-DE38-4E96-A648-AFA40F0F2D1B}']);
    end;
end;

function TUsers.GetItemOn (anID: TID) : TUser;
begin
    Result := NIL;
    try
        Result := inherited ItemOn [anID] as TUser;
    except on E: Exception do
        _raise ([ Format ('GetItemOn [%d]',[anID]), ERR_TUSERS_GET_ITEM_ON, E ],
                ['{C39DE9B3-34AA-43B5-BF56-245478511005}']);
    end;
end;

procedure TUsers.SetItemOn (anID: TID; anItem: TUser);
begin
    try
        inherited ItemOn [anID] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemOn [%d]',[anID]), ERR_TUSERS_SET_ITEM_ON, E ],
                ['{7BD7358A-6CE1-4B13-B8B8-A53778019652}']);
    end;
end;

class function TUsers.ItemClassType : CMetaObject;
begin
    Result := TUser;
end;

function TUsers.Add (anItem: TUser) : Integer;
begin
    Result := -1;
    try
        Result := inherited Add (anItem);
    except on E: Exception do
        _raise (['Add',ERR_TUSERS_ADD,E],
                ['{AD66391A-0622-441B-8BCB-98C3824C2137}']);
    end;
end;

class function TUsers.Find (const aDB: TSQLiteDatabase;
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
            raise Exception.Create (ERR_TUSERS_INVALID_DATABASE);
        Props := ItemClassType.GetClassProperties;
        try
            Props.ItemAt [_usr_key_hash].asHex := aHash;
            Props.ItemAt [_usr_id_owner].asInt64 := anIDOwner;

            query := Format ('SELECT %s '+
                             'FROM %s '+
                             'WHERE ( %s="%s" ) '+
                               'AND ( %s="%s" ) ',
                             [ Props.ItemAt [_id].Name,
                               ItemClassType.GetClassTable,
                               Props.ItemAt [_usr_key_hash].Name, TMetaProperty (Props.ItemAt [_usr_key_hash]).asDBFormat,
                               Props.ItemAt [_usr_id_owner].Name, TMetaProperty (Props.ItemAt [_usr_id_owner]).asDBFormat ]);
            //ShowMessage (query);
            table := aDB.GetTable (query);
            with table do
            try
                for I := 0 to table.Count-1 do
                begin
                    if ( I > 0 ) then
                        raise Exception.CreateFmt (ERR_TUSER_NOT_UNIQUE_HASH,
                                                   [ Props.ItemAt [_usr_key_hash].Name,
                                                     TMetaProperty (Props.ItemAt [_usr_key_hash]).asDBFormat ]);
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
        _raise (['Find',ERR_TUSERS_FIND,E],
                ['{99B2D200-B512-4CA7-99C7-BD93B31332E0}']);
    end;
end;

class procedure TUsers.Filter (var Query: String;
                               anArgs: array of const);
var
    IDs              : TVarRec;
    IDsExternal      : TVarRec;
    TimeStampsCreate : TVarRec;
    TimeStampsModify : TVarRec;
    TimeStampsPublic : TVarRec;
    Versions         : TVarRec;
    OBJ              : TObject;
    arr              : array_of_const;
    I                : Integer;
    Props            : TProtoProperties;
    Prop             : TMetaProperty;
    IDsOwners        : String;
begin
    try
        { первый параметр - список идентификаторов }
        if notEmpty (0,anArgs) then
            IDs := anArgs [0];
        { второй параметр - список пара-идентификаторов (логинов) }
        if notEmpty (1,anArgs) then
            IDsExternal := anArgs [1];
        { четвертый параметр - период создания }
        if notEmpty (3,anArgs) then
            TimeStampsCreate := anArgs [3];
        { пятый параметр - период последнего редактирования }
        if notEmpty (4,anArgs) then
            TimeStampsModify := anArgs [4];
        { шестой параметр - период публикации }
        if notEmpty (5,anArgs) then
            TimeStampsPublic := anArgs [5];
        { седьмой параметр - допустимый период версий }
        if notEmpty (6,anArgs) then
            Versions := anArgs [6];
        { накладываем фильтр по алгоритму предка }
        arr := _([ NIL, NIL, NIL, NIL, NIL, NIL ]);
        arr [0] := IDs;
        arr [1] := IDsExternal;
        arr [2] := TimeStampsCreate;
        arr [3] := TimeStampsModify;
        arr [4] := TimeStampsPublic;
        arr [5] := TimeStampsPublic;
        inherited Filter (Query,arr);
        { накладываем дополнительный фильтр }
        Props := ItemClassType.GetClassProperties;
        try
            Prop := TMetaProperty (Props.ItemAt [_usr_id_owner]);
            { третий параметр - список идентификаторов владельцев }
            IDsOwners := '';
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
                                IDsOwners := Prop.asDBFormat
                            else
                                IDsOwners := Format ('%s,%s',[ IDsOwners, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDsOwners) > 0 ) then
                        IDsOwners := Format ('%s IN (%s) ',[ Prop.Name, IDsOwners ]);
                end
                else
                begin
                    arr := toArrayOfConst (anArgs [2]);
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
        finally
            Props.FreeAll;
            FreeAndNil (Props);
        end;
        if ( Length (IDsOwners) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,IDsOwners]);
    except on E: Exception do
        _raise (['Filter',ERR_TUSERS_FILTER,E],
                ['{176BC17B-0598-4559-876F-FAFAE252FD80}']);
    end;
end;

class procedure TUsers.OnLoad (const aDB: TSQLiteDatabase;
                               anObject: TMetaObject;
                               aTable: TSQLiteTable);
begin
    try
        inherited OnLoad (aDB,anObject,aTable);
        if objSimple in anObject.Mode then Exit;
        with TUser (anObject) do
        try
            Mode := Mode - [objLoaded];
            if ( IDCrypto > 0 ) then
            begin
                Crypto.ID := IDCrypto;
                Crypto.Load;
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
        _raise (['OnLoad',ERR_TUSERS_LOAD,E],
                ['{C1A709CF-DCB6-4816-95B8-FB842B4038E6}']);
    end;
end;

{$IFNDEF DELETE_TRIGGER}
class procedure TUsers.Delete (const aDB: TSQLiteDatabase;
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
            raise Exception.Create (ERR_TUSERS_INVALID_DATABASE);
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
                        { удаляем пользователей и все объекты, принадлежащие им }
                        TMetaProperty (Props.ItemAt [_id]).asDBFormat := Fields [  FieldIndex [ Props.ItemAt [_id].Name ]  ];
                        TUser.Delete ( aDB, TMetaProperty (Props.ItemAt [_id]).asID );
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
                raise Exception.CreateFmt (ERR_TUSERS_TRANSACTION,[E.Message]);
            end; end;
        finally
            Props.FreeAll;
            FreeAndNil (Props);
        end;
    except on E: Exception do
        _raise (['Delete',ERR_TUSERS_DELETE,E],
                ['{C466837B-287B-49B1-B7AA-5605F654023C}']);
    end;
end;
{$ENDIF DELETE_TRIGGER}

class procedure TUsers.ReplacePic (const aDB: TSQLiteDatabase;
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
            raise Exception.Create (ERR_TUSERS_INVALID_DATABASE);

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
                TMetaProperty (Props.ItemAt [_usr_hash_pic]).asHex := aHashPic;
                TMetaProperty (Props.ItemAt [_usr_id_pic]).asID := aNewIDPic;
                TMetaProperty (Props.ItemAt [_usr_id_owner]).asID := anIDOwner;
                { формируем запрос }
                query := Format ('SELECT %s, %s '+
                                 'FROM %s '+
                                 'WHERE ( %s="%s" ) '+
                                 '  AND ( %s<>"%s" ) '+
                                 '  AND ( %s="%s" ) ',
                                 [ Props.ItemAt [_id].Name,
                                   Props.ItemAt [_usr_id_pic].Name,
                                   ItemClassType.GetClassTable,
                                   Props.ItemAt [_usr_hash_pic].Name,
                                   TMetaProperty (Props.ItemAt [_usr_hash_pic]).asDBFormat,
                                   Props.ItemAt [_usr_id_pic].Name,
                                   TMetaProperty (Props.ItemAt [_usr_id_pic]).asDBFormat,
                                   Props.ItemAt [_usr_id_owner].Name,
                                   TMetaProperty (Props.ItemAt [_usr_id_owner]).asDBFormat ]);
                // ShowMessage (query);
                table := aDB.GetTable (query);
                with table do
                try
                    for I := 0 to table.Count-1 do
                    begin
                        TMetaProperty (Props.ItemAt [_id]).asDBFormat := Fields [  FieldIndex [ Props.ItemAt [_id].Name ]  ];
                        TMetaProperty (Props.ItemAt [_usr_id_pic]).asDBFormat := Fields [  FieldIndex [ Props.ItemAt [_usr_id_pic].Name ]  ];
                        { не меняем свойств текущего пользователя }
                        if ( TMetaProperty (Props.ItemAt [_id]).asID <> anIDOwner ) then
                        begin
                            { удаляем старую иконку }
                            TPics.Delete (aDB,[ _([ TMetaProperty (Props.ItemAt [_usr_id_pic]).asID ]), _([anIDOwner]) ]);
                            { заменяем иконку }
                            TMetaProperty (Props.ItemAt [_usr_id_pic]).asID := aNewIDPic;
                            query := Format ('UPDATE %s '+
                                             'SET %s="%s" '+
                                             'WHERE %s="%s" ',
                                             [ ItemClassType.GetClassTable,
                                               Props.ItemAt [_usr_id_pic].Name,
                                               TMetaProperty (Props.ItemAt [_usr_id_pic]).asDBFormat,
                                               Props.ItemAt [_id].Name,
                                               TMetaProperty (Props.ItemAt [_id]).asDBFormat ]);
                            // ShowMessage (query);
                            aDB.ExecSQL (query);
                        end;
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
                raise Exception.CreateFmt (ERR_TUSERS_TRANSACTION,[E.Message]);
            end; end;
        finally
            Props.FreeAll;
            FreeAndNil (Props);
        end;
    except on E: Exception do
        _raise (['ReplacePic',ERR_TUSERS_REPLACE_PIC,E],
                ['{5017C48D-2044-46E2-9F25-05CA68C742BB}']);
    end;
end;
