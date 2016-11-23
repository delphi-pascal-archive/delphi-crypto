{******************************************************************************}
{*  TMetaObjects.imp.inc                                                      *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TMetaObjects }
constructor TMetaObjects.Create (const aDB: TSQLiteDatabase; anArgs: array of const);
begin
    try
        Create (anArgs);
        { обращаемся к объекту БД }
        if Assigned (aDB) then
            f_DB := aDB
        else
            raise Exception.Create (ERR_TMETAOBJECTS_INVALID_DATABASE);
    except on E: Exception do
        _raise (['Create',ERR_TMETAOBJECTS_CREATE,E],
                ['{9D586D30-D97D-4ED3-9445-11369204D530}']);
    end;
end;

destructor TMetaObjects.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TMETAOBJECTS_DESTROY,E],
                ['{13300766-FCC0-4EAF-A0DD-3DC4DEC34F93}']);
    end;
end;

function TMetaObjects.Check (anItem: Pointer) : Boolean;
begin
    Result := FALSE;
    try
        Result := inherited Check (anItem) and
                  TObject (anItem).InheritsFrom (TMetaObject) and
                  Check ( TMetaObject (anItem) );
    except on E: Exception do
        _raise (['Check',ERR_TMETAOBJECTS_CHECK,E],
                ['{FACEA5D5-4BA4-4194-B2A0-7FF85E715B24}']);
    end;
end;

function TMetaObjects.Check (anItem: TMetaObject) : Boolean;
begin
    Result := Assigned (anItem) and anItem.Check;
end;

function TMetaObjects.GetItemAt (anIndex: Integer) : TMetaObject;
begin
    Result := NIL;
    try
        if inherited ItemAt [anIndex].InheritsFrom (TMetaObject) then
            Result := inherited ItemAt [anIndex] as TMetaObject;
    except on E: Exception do
        _raise ([ Format ('GetItemAt [%d]',[anIndex]), ERR_TMETAOBJECTS_GET_ITEM_AT, E ],
                ['{954D33F1-66E1-4DE7-9E4F-E01B963F86FA}']);
    end;
end;

procedure TMetaObjects.SetItemAt (anIndex: Integer; anItem: TMetaObject);
begin
    try
        if inherited ItemAt [anIndex].InheritsFrom (TMetaObject) then
            inherited ItemAt [anIndex] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemAt [%d]',[anIndex]), ERR_TMETAOBJECTS_SET_ITEM_AT, E ],
                ['{32B47DCB-5182-4379-AF18-106F15B60EE4}']);
    end;
end;

function TMetaObjects.GetItemOf (aName: String) : TMetaObject;
begin
    Result := NIL;
    try
        Result := inherited ItemOf [aName] as TMetaObject;
    except on E: Exception do
        _raise ([ Format ('GetItemOf [%s]',[aName]), ERR_TMETAOBJECTS_GET_ITEM_OF, E ],
                ['{9DDC3EA4-F6A9-4A53-9981-DC18D5352C99}']);
    end;
end;

procedure TMetaObjects.SetItemOf (aName: String; anItem: TMetaObject);
begin
    try
        inherited ItemOf [aName] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemOf [%s]',[aName]), ERR_TMETAOBJECTS_SET_ITEM_OF, E ],
                ['{D996E1CD-CB86-4573-ADA5-08FB33548113}']);
    end;
end;

function TMetaObjects.GetItemOn (anID: TID) : TMetaObject;
var
    I : Integer;
begin
    Result := NIL;
    try
        for I := 0 to Count - 1 do
            if Assigned (ItemAt [I]) and ( ItemAt [I].ID = anID ) then
            begin
                Result := ItemAt [I];
                Break;
            end;
    except on E: Exception do
        _raise ([ Format ('GetItemOn [%d]',[anID]), ERR_TMETAOBJECTS_GET_ITEM_ON, E ],
                ['{D8CBCAEC-B80F-4107-B6A3-6AE8393FDD17}']);
    end;
end;

procedure TMetaObjects.SetItemOn (anID: TID; anItem: TMetaObject);
var
    I : Integer;
begin
    try
        for I := 0 to Count - 1 do
            if Assigned (ItemAt [I]) and ( ItemAt [I].ID = anID ) then
            begin
                ItemAt [I] := anItem;
                Break;
            end;
    except on E: Exception do
        _raise ([ Format ('SetItemOn [%d]',[anID]), ERR_TMETAOBJECTS_SET_ITEM_ON, E ],
                ['{89A99D6A-C011-4191-A2A1-42BECF08ADA5}']);
    end;
end;

class function TMetaObjects.ItemClassType : CMetaObject;
begin
    Result := TMetaObject;
end;

function TMetaObjects.Add (anItem: TMetaObject) : Integer;
begin
    Result := -1;
    try
        Result := inherited Add (anItem);
    except on E: Exception do
        _raise (['Add',ERR_TMETAOBJECTS_ADD,E],
                ['{7DFE87EA-BFC1-4DE0-AB15-FEDE4E2F704E}']);
    end;
end;

function TMetaObjects.Add (anArgs: array of const;
                           aMode: TObjectMode = []) : Integer;
var
    OBJ : TMetaObject;
begin
    Result := -1;
    try
        OBJ := ItemClassType.Create (DB,anArgs,aMode);
        try
            Result := Add (OBJ);
        finally
            if ( Result < 0 ) then
                FreeAndNil (OBJ);
        end;
    except on E: Exception do
        _raise (['Add',ERR_TMETAOBJECTS_ADD,E],
                ['{DB779C7B-A5C2-4DCF-96B7-501475F05363}']);
    end;
end;

function TMetaObjects.AddLoad (const anID: TID) : Integer;
var
    OBJ : TMetaObject;
begin
    Result := -1;
    try
        OBJ := ItemClassType.Load (DB,anID);
        try
            Result := Add (OBJ);
        finally
            if ( Result < 0 ) then
                FreeAndNil (OBJ);
        end;
    except on E: Exception do
        _raise (['AddLoad',ERR_TMETAOBJECTS_ADD_LOAD,E],
                ['{948859BE-CD65-48FA-A13D-1BFA066BCDBB}']);
    end;
end;

function TMetaObjects.AddSave (anArgs: array of const;
                               aMode: TObjectMode = []) : Integer;
var
    OBJ : TMetaObject;
begin
    Result := -1;
    try
        OBJ := ItemClassType.Create (DB,anArgs,aMode);
        try
            Result := Add (OBJ);
            Save;
        finally
            if ( Result < 0 ) then
                FreeAndNil (OBJ);
        end;
    except on E: Exception do
        _raise (['AddSave',ERR_TMETAOBJECTS_ADD_SAVE,E],
                ['{B915FCB1-6D19-480E-BF70-A1C7CA2E6ED5}']);
    end;
end;

class procedure TMetaObjects.Filter (var Query: String;
                                     anArgs: array of const);
var
    OBJ   : TObject;
    arr   : array_of_const;
    I     : Integer;
    Props : TProtoProperties;
    Prop  : TMetaProperty;
    IDs   : String;
begin
    try
        Props := ItemClassType.GetClassProperties;
        try
            Prop := TMetaProperty (Props.ItemAt [_id]);
            { первый параметр - список идентификаторов }
            IDS := '';
            if notEmpty (0,anArgs) then
            begin
                OBJ := toObject (anArgs [0]);
                if Assigned (OBJ) and OBJ.InheritsFrom (TStringList) then
                begin
                    for I := 0 to TStringList (OBJ).Count - 1 do
                    begin
                        Prop.asString := TStringList (OBJ).Strings [I];
                        if ( Prop.asID > 0 ) then
                            if ( I = 0 ) then
                                IDs := Prop.asDBFormat
                            else
                                IDs := Format ('%s,%s',[ IDs, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDs) > 0 ) then
                        IDS := Format ('%s IN (%s) ',[ Prop.Name, IDs ]);
                end
                else
                begin
                    arr := toArrayOfConst (anArgs [0]);
                    for I := 0 to High (arr) do
                    begin
                        Prop.asID := toInt64 (arr [I]);
                        if ( Prop.asID > 0 ) then
                            if ( I = 0 ) then
                                IDs := Prop.asDBFormat
                            else
                                IDs := Format ('%s,%s',[ IDs, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDs) > 0 ) then
                        IDs := Format ('%s IN (%s) ',[ Prop.Name, IDs ]);
                end;
            end;
        finally
            Props.FreeAll;
            FreeAndNil (Props);
        end;
        if ( Length (IDs) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,IDs]);
    except on E: Exception do
        _raise (['Filter',ERR_TMETAOBJECTS_FILTER,E],
                ['{C0ED240E-00BC-418F-B056-69F8D86FD674}']);
    end;
end;

class procedure TMetaObjects.Order (var Query: String;
                                    anArgs: array of const);
var
    Props     : TProtoProperties;
    Prop      : TMetaProperty;
    PropIndex : Integer;
    PropName  : String;
    Vector    : String;
begin
    try
        Props := ItemClassType.GetClassProperties;
        try
            PropIndex := _id;
            if notEmpty (0,anArgs) then
            begin
                PropIndex := toInteger (anArgs [0]);
                if ( PropIndex <= 0 ) then
                try
                    PropName := toString (anArgs [0]);
                    PropIndex := Props.Index [ Props.ItemOf [PropName] ];
                except
                    PropIndex := _id;
                end;
            end;
            Prop := TMetaProperty (Props.ItemAt [PropIndex]);
            Vector := 'ASC';
            if notEmpty (1,anArgs) and ( toString (anArgs [1]) = 'DESC' ) then
                Vector := 'DESC';
            query := Format ('%s ORDER BY %s %s ',[query,Prop.Name,Vector]);
        finally
            Props.FreeAll;
            FreeAndNil (Props);
        end;
    except on E: Exception do
        _raise (['Order',ERR_TMETAOBJECTS_ORDER,E],
                ['{2660415B-B2FE-414E-BF2A-E2920886D8D9}']);
    end;
end;

class procedure TMetaObjects.Limit (var Query: String;
                                    anArgs: array of const);
var
    LimitFrom  : TID;
    LimitCount : TID;
begin
    try
        if notEmpty (0,anArgs) and notEmpty (1,anArgs) then
        begin
            LimitFrom  := toInt64 (anArgs [0]);
            LimitCount := toInt64 (anArgs [1]);
            query := Format ('%s LIMIT %d, %d ',[query,LimitFrom,LimitCount]);
        end
        else if notEmpty (0,anArgs) then
        begin
            LimitCount := toInt64 (anArgs [0]);
            query := Format ('%s LIMIT %d ',[query,LimitCount]);
        end;
    except on E: Exception do
        _raise (['Limit',ERR_TMETAOBJECTS_LIMIT,E],
                ['{655D0F45-2956-437F-8100-6584F1322434}']);
    end;
end;

class procedure TMetaObjects.OnLoad (const aDB: TSQLiteDatabase;
                                     anObject: TMetaObject;
                                     aTable: TSQLiteTable);
begin
    try
        { do nothing }
    except on E: Exception do
        _raise (['OnLoad',ERR_TMETAOBJECTS_LOAD,E],
                ['{40AF60E8-D4AB-4D9A-87A3-2099FE4CB6A8}']);
    end;
end;

class function TMetaObjects.GetCount (const aDB: TSQLiteDatabase;
                                      anArgs: array of const) : Integer;
var
    query : String;
    table : TSQLiteTable;
    I     : Integer;
    J     : Integer;
    Props : TProtoProperties;
begin
    Result := 0;
    try
        if not Assigned (aDB) then
            raise Exception.Create (ERR_TMETAOBJECTS_INVALID_DATABASE);

        { проверяем наличие таблиц класса }
        ItemClassType.CreateClassTable (aDB);

        Props := ItemClassType.GetClassProperties;
        try
            { формируем запрос }
            query := Format ('SELECT COUNT (%s) '+
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
                    if ( I > 0 ) then
                        raise Exception.CreateFmt (ERR_TMETAOBJECTS_COUNT_ERROR,[table.Count]);
                    for J := 0 to ColCount - 1 do
                    begin
                        if ( J > 0 ) then
                            raise Exception.CreateFmt (ERR_TMETAOBJECTS_COUNT_ERROR,[ColCount]);
                        Result := StrToInt (Fields [J]);
                    end;
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
        _raise (['GetCount',ERR_TMETAOBJECTS_GET_COUNT,E],
                ['{2031D828-5FDA-4B27-842F-0DB0B7A69046}']);
    end;
end;

class function TMetaObjects.Load (const aDB: TSQLiteDatabase;
                                  anArgs: array of const;
                                  anOrder: array of const;
                                  aLimit: array of const;
                                  aMode: TObjectMode = []) : TMetaObjects;
var
    query : String;
    table : TSQLiteTable;
    I     : Integer;
    J     : Integer;
    OBJ   : TMetaObject;
    Props : TProtoProperties;
begin
    Result := NIL;
    try
        if not Assigned (aDB) then
            raise Exception.Create (ERR_TMETAOBJECTS_INVALID_DATABASE);

        { проверяем наличие таблиц класса }
        ItemClassType.CreateClassTable (aDB);

        Props := ItemClassType.GetClassProperties;
        try
            { формируем запрос }
            query := Format ('SELECT * '+
                             'FROM %s '+
                             'WHERE ( %s>="0" ) ',
                             [ ItemClassType.GetClassTable,
                               Props.ItemAt [_id].Name ]);
            { накладываем фильтр }
            Filter (query,anArgs);
            { устанавливаем сортировку }
            Order (query,anOrder);
            { устанавливаем предел }
            Limit (query,aLimit);
            // ShowMessage (query);
            table := aDB.GetTable (query);
            with table do
            try
                Result := Self.Create (aDB,[]);

                for I := 0 to table.Count-1 do
                try
                    OBJ := ItemClassType.Create (aDB,[],aMode+[objLoadFailed]);
                    with OBJ do
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
                        { выполняем дополнительные действия }
                        OnLoad (aDB,OBJ,table);
                        Mode := Mode + [objLoaded];
                        { добавляем }
                        if ( Result.Add (OBJ) >= 0 ) then
                            OBJ.Mode := OBJ.Mode - [objLoadFailed];
                        table.Next;
                    finally
                        if ( objLoadFailed in OBJ.Mode ) then
                            FreeAndNil (OBJ);
                    end;
                except
                    { ignore invalid object }
                end;
            finally
                FreeAndNil (table);
            end;
        finally
            Props.FreeAll;
            FreeAndNil (Props);
        end;
    except on E: Exception do
        _raise (['Load',ERR_TMETAOBJECTS_LOAD,E],
                ['{227EFDF9-78BA-49B3-8660-E0FA9FFDBB6E}']);
    end;
end;

class function TMetaObjects.Load (const aDB: TSQLiteDatabase;
                                  anArgs: array of const;
                                  anOrder: array of const) : TMetaObjects;
begin
    Result := NIL;
    try
        Result := Load (aDB,anArgs,anOrder,[]);
    except on E: Exception do
        _raise (['Load',ERR_TMETAOBJECTS_LOAD,E],
                ['{659E9636-C332-4D6F-B16E-1DBDEAD8D373}']);
    end;
end;

class function TMetaObjects.Load (const aDB: TSQLiteDatabase;
                                  anArgs: array of const) : TMetaObjects;
begin
    Result := NIL;
    try
        Result := Load (aDB,anArgs,[]);
    except on E: Exception do
        _raise (['Load',ERR_TMETAOBJECTS_LOAD,E],
                ['{07C94DFD-E895-4333-A637-5D36836659E7}']);
    end;
end;

procedure TMetaObjects.Load;
var
    I : Integer;
begin
    try
        for I := 0 to Count - 1 do
            ItemAt [I].Load;
    except on E: Exception do
        _raise (['Load',ERR_TMETAOBJECTS_LOAD,E],
                ['{7D9B37E0-3560-41B6-8B77-4B616765E7F3}']);
    end;
end;

procedure TMetaObjects.Save;
var
    I                       : Integer;
    isBeforeTransactionOpen : Boolean;
begin
    try
        try
        { при необходимости открываем транзакцию }
            isBeforeTransactionOpen := FALSE;
            if ( not DB.isTransactionOpen ) then
                DB.BeginTransaction
            else
                isBeforeTransactionOpen := TRUE;
        { сохраняем мета-объекты }
            for I := 0 to Count - 1 do
                ItemAt [I].Save;
        { если ошибок не обнаружено - проводим транзакцию }
            if ( not isBeforeTransactionOpen ) then
                DB.Commit;
        { в случае возникновения ошибки - откатываем транзакцию }
        except on E: Exception do begin
            if ( not isBeforeTransactionOpen ) then
                DB.Rollback;
            raise Exception.CreateFmt (ERR_TMETAOBJECTS_TRANSACTION,[E.Message]);
        end; end;
    except on E: Exception do
        _raise (['Save',ERR_TMETAOBJECTS_SAVE,E],
                ['{3D9045DE-2CB4-4DA6-9A39-B65EFAA87FCB}']);
    end;
end;

class procedure TMetaObjects.Delete (const aDB: TSQLiteDatabase;
                                     anArgs: array of const);
var
    query                   : String;
    isBeforeTransactionOpen : Boolean;
    Props                   : TProtoProperties;
begin
    try
        if not Assigned (aDB) then
            raise Exception.Create (ERR_TMETAOBJECTS_INVALID_DATABASE);

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
            { удаляем мета-объекты }
                { формируем запрос }
                query := Format ('DELETE '+
                                 'FROM %s '+
                                 'WHERE ( %s>="0" ) ',
                                 [ ItemClassType.GetClassTable,
                                   Props.ItemAt [_id].Name ]);
                { накладываем фильтр }
                Filter (query,anArgs);
                // ShowMessage (query);
                aDB.ExecSQL (query);
            { если ошибок не обнаружено - проводим транзакцию }
                if ( not isBeforeTransactionOpen ) then
                    aDB.Commit;
            { в случае возникновения ошибки - откатываем транзакцию }
            except on E: Exception do begin
                if ( not isBeforeTransactionOpen ) then
                    aDB.Rollback;
                raise Exception.CreateFmt (ERR_TMETAOBJECTS_TRANSACTION,[E.Message]);
            end; end;
        finally
            Props.FreeAll;
            FreeAndNil (Props);
        end;
    except on E: Exception do
        _raise (['Delete',ERR_TMETAOBJECTS_DELETE,E],
                ['{C9684879-0F54-45E0-B264-776B1BC0BC14}']);
    end;
end;

procedure TMetaObjects.Delete;
var
    I                       : Integer;
    isBeforeTransactionOpen : Boolean;
begin
    try
        try
        { при необходимости открываем транзакцию }
            isBeforeTransactionOpen := FALSE;
            if ( not DB.isTransactionOpen ) then
                DB.BeginTransaction
            else
                isBeforeTransactionOpen := TRUE;
        { удаляем мета-объекты }
            for I := 0 to Count - 1 do
                ItemAt [I].Delete;
        { если ошибок не обнаружено - проводим транзакцию }
            if ( not isBeforeTransactionOpen ) then
                DB.Commit;
        { в случае возникновения ошибки - откатываем транзакцию }
        except on E: Exception do begin
            if ( not isBeforeTransactionOpen ) then
                DB.Rollback;
            raise Exception.CreateFmt (ERR_TMETAOBJECTS_TRANSACTION,[E.Message]);
        end; end;
    except on E: Exception do
        _raise (['Delete',ERR_TMETAOBJECTS_DELETE,E],
                ['{9488171C-CE5D-46F3-A890-D1984A231938}']);
    end;
end;

procedure TMetaObjects.LoadFromStream (const aStream: TStream);
var
    Stream : TStream;
    OBJ    : TMetaObject;
begin
    try
        if not Assigned (aStream) then
            raise Exception.Create (ERR_TMETAOBJECTS_INVALID_STREAM);
        aStream.Position := 0;
        Clear (TRUE);
        Stream := TMemoryStream.Create;
        try
            { читаем объекты последовательно из потока }
            if ( aStream.Size > 0 ) then
            repeat
                TMemoryStream (Stream).Clear;
                ReadStreamL (aStream,Stream);
                OBJ := ItemClassType.Create (DB,[],[]);
                try
                    OBJ.LoadFromStream (Stream);
                    if ( Add (OBJ) < 0 ) then
                        FreeAndNil (OBJ);
                except
                    FreeAndNil (OBJ);
                end;
            until not ( aStream.Position < aStream.Size );
        finally
            FreeAndNil (Stream);
        end;
    except on E: Exception do
        _raise (['LoadFromStream',ERR_TMETAOBJECTS_LOAD_FROM_STREAM,E],
                ['{F7F262E8-F648-4F91-8AD7-C05946111D3A}']);
    end;
end;

procedure TMetaObjects.SaveToStream (out aStream: TStream);
var
    Stream : TStream;
    I      : Integer;
begin
    try
        if not Assigned (aStream) then
            raise Exception.Create (ERR_TMETAOBJECTS_INVALID_STREAM);
        aStream.Position := 0;
        Stream := TMemoryStream.Create;
        try
            { записываем объекты последовательно в поток }
            for I := 0 to Count - 1 do
            begin
                TMemoryStream (Stream).Clear;
                ItemAt [I].SaveToStream (Stream);
                WriteStreamL (aStream,Stream);
            end;
        finally
            FreeAndNil (Stream);
        end;
    except on E: Exception do
        _raise (['SaveToStream',ERR_TMETAOBJECTS_SAVE_TO_STREAM,E],
                ['{35C40372-D3E9-4F4C-A2B8-E07A7D71073A}']);
    end;
end;
