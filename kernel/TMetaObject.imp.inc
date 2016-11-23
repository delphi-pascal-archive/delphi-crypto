{******************************************************************************}
{*  TMetaObject.imp.inc                                                       *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TMetaObject }
class function TMetaObject.GetClassName : String;
begin
    Result := CLS_TMETAOBJECT_NAME;
end;

class function TMetaObject.GetClassVersion : TVersionInfo;
begin
    Result := TVersionVarData (vSelf).VVersion.asVersion;
end;

class function TMetaObject.GetClassTable : String;
begin
    Result := 'meta_objects';
end;

class function TMetaObject.GetClassProperties : TProtoProperties;
begin
    Result := NIL;
    try
        Result := inherited GetClassProperties;
        with Result do
        begin
            Register ( TMetaProperty, ['id',PRP_TMETAOBJECT_ID,dtUID,0],[prpStored] );
        end;
    except on E: Exception do
        _raise (['GetClassProperties',ERR_TMETAOBJECT_GET_ClASS_PROPERTIES,E],
                ['{8D0B29FB-BCF9-4AA0-A859-7A6EC7C381AA}']);
    end;
end;

class function TMetaObject.GetClassList : CProtoObjects;
begin
    Result := TMetaObjects;
end;

function TMetaObject.RegisterProperty (anArgs: array of const;
                                       aMode: TPropertyMode = []) : Integer;
begin
    Result := -1;
    try
        Result := Properties.Register ( TMetaProperty, anArgs, aMode );
    except on E: Exception do
        _raise (['RegisterProperty',ERR_TMETAOBJECT_REGISTER_PROPERTY,E],
                ['{D7675818-35C2-4B86-B8B4-16379805A8AC}']);
    end;
end;

constructor TMetaObject.Create (const aDB: TSQLiteDatabase;
                                anArgs: array of const;
                                aMode: TObjectMode = []);
begin
    try
        inherited Create ([],aMode);
        Mode := Mode - [objCreated];
        { обращаемся к объекту БД }
        DB := aDB;
        { создаем таблицу класса, если она еще не создана }
        CreateClassTable;
        { первый параметр - идентификатор }
        if notEmpty (0,anArgs) then
            ID := toInt64 (anArgs [0]);
        Mode := Mode + [objCreated];
    except on E: Exception do begin
        Mode := Mode + [objCreateFailed];
        _raise (['Create',ERR_TMETAOBJECT_CREATE,E],
                ['{CAEE0F9E-B806-47F9-8068-6BF7AF8DE2CE}']);
    end; end;
end;

destructor TMetaObject.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TMETAOBJECT_DESTROY,E],
                ['{48A339DA-45C8-4E2C-8D21-1E6181ABC184}']);
    end;
end;

class procedure TMetaObject.CreateClassTable (const aDB: TSQLiteDatabase);
var
    Props       : TProtoProperties;
    Prop        : TMetaProperty;
    I           : Integer;
    StoredCount : Integer;
    TableFields : String;
    query       : String;
begin
    try
        if not Assigned (aDB) then
            raise Exception.Create (ERR_TMETAOBJECT_INVALID_DATABASE);
        Props := GetClassProperties;
        try
            if ( not aDB.TableExists (GetClassTable) ) then
            begin
                { формируем список полей таблицы }
                StoredCount := 0;
                for I := 0 to Props.Count - 1 do
                begin
                    if Assigned (Props.ItemAt [I]) and ( prpStored in Props.ItemAt [I].Mode ) then
                    begin
                        if (StoredCount = 0) then
                            TableFields := Format ('%s %s',
                                                   [ Props.ItemAt [I].Name, TMetaProperty (Props.ItemAt [I]).DBType ])
                        else
                            TableFields := Format ('%s, ' +
                                                   '%s %s',
                                                   [ TableFields,
                                                     Props.ItemAt [I].Name, TMetaProperty (Props.ItemAt [I]).DBType ]);
                        Inc (StoredCount);
                    end;
                end;
                { создаем таблицу }
                query := Format ('CREATE TABLE %s ( %s )',
                                 [ GetClassTable, TableFields ]);
                // ShowMessage (query);
                aDB.ExecSQL (query);
            end;
            { создаем индекс по идентификатору }
            Prop := TMetaProperty (Props.ItemAt [_id]);
            { автоинкрементальное поле индексируется автоматически
              и стоит первым по счету в таблице }
            if not aDB.IndexExists ( Format ('sqlite_autoindex_%s_%d',[GetClassTable,1]) ) and
               not aDB.IndexExists ( Format ('index_%s_%s',[GetClassTable,Prop.Name]) ) then
            begin
                { создаем индекс }
                query := Format ('CREATE INDEX index_%s_%s '+
                                 'ON %s ( %s )',
                                 [ GetClassTable, Prop.Name,
                                   GetClassTable, Prop.Name ]);
                // ShowMessage (query);
                aDB.ExecSQL (query);
            end;
        finally
            Props.FreeAll;
            FreeAndNil (Props);
        end;
    except on E: Exception do
        _raise (['CreateClassTable',ERR_TMETAOBJECT_CREATE_CLASS_TABLE,E],
                ['{52B43840-FF4D-46B0-AF90-E2A029A5FFFA}']);
    end;
end;

procedure TMetaObject.CreateClassTable;
begin
    try
        CreateClassTable (DB);
    except on E: Exception do
        _raise (['CreateClassTable',ERR_TMETAOBJECT_CREATE_CLASS_TABLE,E],
                ['{C85E6192-F1F3-4F55-A126-E80E48E2E0D4}']);
    end;
end;

procedure TMetaObject.SetDB (const aDB: TSQLiteDatabase);
begin
    try
        if Assigned (aDB) then
            f_DB := aDB
        else
            raise Exception.Create (ERR_TMETAOBJECT_INVALID_DATABASE);
    except on E: Exception do
        _raise (['SetDB',ERR_TMETAOBJECT_SET_DB,E],
                ['{66CD7876-9BBF-49A0-871C-C8023B0F0ECB}']);
    end;
end;

function TMetaObject.Check : Boolean;
begin
    Result := TRUE;
    try
        Result := inherited Check;
        if Result then
        begin
            if not (ID >= 0) then
                Error (ERR_TMETAOBJECT_INVALID_ID);
            Result := ( Errors.Count = 0 );
        end;
    except on E: Exception do
        _raise (['Check',ERR_TMETAOBJECT_CHECK,E],
                ['{38DF28A6-98D6-4D87-94F1-CD5B9092EA87}']);
    end;
end;

procedure TMetaObject.Load;
var
    query : String;
    table : TSQLiteTable;
    I     : Integer;
    J     : Integer;
begin
    try
        if objNoLoad in Mode then Exit;
        Mode := Mode - [objLoadFailed];
        Mode := Mode - [objLoaded];
        { загружаем объект }
        if ( ID > 0 ) then
        begin
            query := Format ('SELECT * '+
                             'FROM %s '+
                             'WHERE %s="%s" ',
                             [ GetClassTable,
                               PropAt [_id].Name, PropAt [_id].asDBFormat ]);
            // ShowMessage (query);
            table := DB.GetTable (query);
            with table do
            try
                if ( table.Count <= 0 ) then
                begin
                    {for I := 0 to Properties.Count - 1 do
                        if Assigned (PropAt [I]) and ( prpStored in PropAt [I].Mode ) then
                            PropAt [I].Value := NULL;}
                    raise Exception.CreateFmt (ERR_TMETAOBJECT_NOT_FOUND_ID,
                                               [ PropAt [_id].Name,
                                                 PropAt [_id].asDBFormat,
                                                 GetClassTable ]);
                end;
                for I := 0 to table.Count-1 do
                begin
                    if ( I > 0 ) then
                        raise Exception.CreateFmt (ERR_TMETAOBJECT_NOT_UNIQUE_ID,
                                                   [ PropAt [_id].Name,
                                                     PropAt [_id].asDBFormat ]);
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
                    table.Next;
                end;
            finally
                FreeAndNil (table);
            end;
        end
        else
            raise Exception.Create (ERR_TMETAOBJECT_INVALID_ID);
        Mode := Mode + [objLoaded];
    except on E: Exception do begin
        Mode := Mode + [objLoadFailed];
        _raise (['Load',ERR_TMETAOBJECT_LOAD,E],
                ['{C582B018-D360-4C12-B7C3-C4DE73C6D1D3}']);
    end; end;
end;

class function TMetaObject.Load (const aDB: TSQLiteDatabase; const anID: TID; aMode: TObjectMode = []) : TMetaObject;
begin
    Result := NIL;
    try
        if not Assigned (aDB) then
            raise Exception.Create (ERR_TMETAOBJECT_INVALID_DATABASE);
        if (anID > 0) then
        begin
            Result := Create (aDB,[anID],aMode);
            with TMetaObject (Result) do
            try
                Load;
            finally
                if Assigned (Result) and ( (ID = 0) or (objLoadFailed in Mode) ) then
                    FreeAndNil (Result);
            end;
        end
        else
            raise Exception.Create (ERR_TMETAOBJECT_INVALID_ID);
    except on E: Exception do
        _raise (['Load',ERR_TMETAOBJECT_LOAD,E],
                ['{088D3965-CD86-4970-A19E-AE6734C46B69}']);
    end;
end;

procedure TMetaObject.Save;
label
    lbInsert;
var
    goto_lbInsert           : Boolean;
    isBeforeTransactionOpen : Boolean;
var
    query       : String;
    sUpdate     : String;
    sFields     : String;
    sValues     : String;
    table       : TSQLiteTable;
    I           : Integer;
    StoredCount : Integer;
begin
    try
        if objNoSave in Mode then Exit;
        Mode := Mode - [objSaveFailed];
        Mode := Mode - [objSaved];
        try
        { при необходимости открываем транзакцию }
            isBeforeTransactionOpen := FALSE;
            if ( not DB.isTransactionOpen ) then
                DB.BeginTransaction
            else
                isBeforeTransactionOpen := TRUE;

            if not Check then
                raise Exception.Create (Errors.Text);

            goto_lbInsert := FALSE;
            { проверяем наличие записи с данным id в БД }
            if ( ID > 0 ) then
            begin
                query := Format ('SELECT %s '+
                                 'FROM %s '+
                                 'WHERE %s="%s" ',
                                 [ PropAt [_id].Name,
                                   GetClassTable,
                                   PropAt [_id].Name, PropAt [_id].asDBFormat ]);
                // ShowMessage (query);
                table := DB.GetTable (query);
                with table do
                try
                    if ( table.Count > 0 ) then
                    begin
                        { формируем список полей для записи в БД }
                        StoredCount := 0;
                        for I := 0 to Properties.Count - 1 do
                            if Assigned (PropAt [I]) and
                               ( prpStored in PropAt [I].Mode ) and
                               not ( PropAt [I].DataType = dtStream ) then
                            begin
                                if ( StoredCount = 0 ) then
                                    sUpdate := Format ('%s="%s"',
                                                       [ PropAt [I].Name, PropAt [I].asDBFormat ])
                                else
                                    sUpdate := Format ('%s, '+
                                                       '%s="%s"',
                                                       [ sUpdate,
                                                         PropAt [I].Name, PropAt [I].asDBFormat ]);
                                Inc (StoredCount);
                            end;
                        { формируем запрос }
                        query := Format ('UPDATE %s '+
                                         'SET %s '+
                                         'WHERE %s="%s" ',
                                         [ GetClassTable,
                                           sUpdate,
                                           PropAt [_id].Name, PropAt [_id].asDBFormat ]);
                        // ShowMessage (query);
                        DB.ExecSQL (query);
                        { записываем BLOB-поля }
                        for I := 0 to Properties.Count - 1 do
                            if Assigned (PropAt [I]) and
                               ( prpStored in PropAt [I].Mode ) and
                               ( PropAt [I].DataType = dtStream ) and
                               Assigned (PropAt [I].asStream) and
                               ( PropAt [I].asStream.Size > 0 ) then
                            begin
                                { формируем запрос }
                                query := Format ('UPDATE %s '+
                                                 'SET %s = ? '+
                                                 'WHERE %s="%s" ',
                                                 [ GetClassTable,
                                                   PropAt [I].Name,
                                                   PropAt [_id].Name, PropAt [_id].asDBFormat ]);
                                // ShowMessage (query);
                                DB.UpdateBlob ( query, PropAt [I].asStream );
                            end;
                    end
                    else
                        goto_lbInsert := TRUE;
                finally
                    FreeAndNil (table);
                end;
                if goto_lbInsert then
                    goto lbInsert;
            end
            else
lbInsert:
            begin
                { формируем список полей для записи в БД }
                StoredCount := 0;
                for I := 0 to Properties.Count - 1 do
                    { при добавлении новой записи в таблицу не нужно указывать идентификатор }
                    if ( I <> _id ) and
                       Assigned (PropAt [I]) and
                       ( prpStored in PropAt [I].Mode ) and
                       not ( PropAt [I].DataType = dtStream ) then
                    begin
                        if ( StoredCount = 0 ) then
                        begin
                            sFields := Format ('%s',
                                               [ PropAt [I].Name ]);
                            sValues := Format ('"%s"',
                                               [ PropAt [I].asDBFormat ]);
                        end
                        else
                        begin
                            sFields := Format ('%s, '+
                                               '%s',
                                               [ sFields,
                                                 PropAt [I].Name ]);
                            sValues := Format ('%s, '+
                                               '"%s"',
                                               [ sValues,
                                                 PropAt [I].asDBFormat ]);
                        end;
                        Inc (StoredCount);
                    end;
                { формируем запрос }
                query := Format ('INSERT INTO %s '+
                                 '( %s ) '+
                                 'VALUES ( %s ) ',
                                 [ GetClassTable,
                                   sFields,
                                   sValues ]);
                // ShowMessage (query);
                DB.ExecSQL (query);
                ID := DB.GetLastInsertRowID;
                if ( ID <= 0 ) then
                    raise Exception.CreateFmt (ERR_TMETAOBJECT_TRANSACTION,[query]);
                { записываем BLOB-поля }
                for I := 0 to Properties.Count - 1 do
                    if Assigned (PropAt [I]) and
                       ( prpStored in PropAt [I].Mode ) and
                       ( PropAt [I].DataType = dtStream ) and
                       Assigned (PropAt [I].asStream) and
                       ( PropAt [I].asStream.Size > 0 ) then
                    begin
                        { формируем запрос }
                        query := Format ('UPDATE %s '+
                                         'SET %s = ? '+
                                         'WHERE %s="%s" ',
                                         [ GetClassTable,
                                           PropAt [I].Name,
                                           PropAt [_id].Name, PropAt [_id].asDBFormat ]);
                        // ShowMessage (query);
                        DB.UpdateBlob ( query, PropAt [I].asStream );
                    end;
            end;
        { если ошибок не обнаружено - проводим транзакцию }
            if ( not isBeforeTransactionOpen ) then
                DB.Commit;
            Mode := Mode + [objSaved];
        { в случае возникновения ошибки - откатываем транзакцию }
        except on E: Exception do begin
            if ( not isBeforeTransactionOpen ) then
                DB.Rollback;
            raise Exception.CreateFmt (ERR_TMETAOBJECT_TRANSACTION,[E.Message]);
        end; end
    except on E: Exception do begin
        Mode := Mode + [objSaveFailed];
        _raise (['Save',ERR_TMETAOBJECT_SAVE,E],
                ['{893F87E3-F7A0-490F-BB16-587C1C3DFDF3}']);
    end; end;
end;

class function TMetaObject.Save (const aDB: TSQLiteDatabase; anArgs: array of const; aMode: TObjectMode = []) : TID;
begin
    Result := 0;
    try
        if not Assigned (aDB) then
            raise Exception.Create (ERR_TMETAOBJECT_INVALID_DATABASE);
        with Create (aDB,anArgs,aMode) do
        try
            Save;
            Result := ID;
        finally
            Free;
        end;
    except on E: Exception do
        _raise (['Save',ERR_TMETAOBJECT_SAVE,E],
                ['{0C17B91F-D479-417B-A9ED-238A2404AABD}']);
    end;
end;

procedure TMetaObject.Delete;
var
    query                   : String;
    isBeforeTransactionOpen : Boolean;
begin
    try
        if objNoDelete in Mode then Exit;
        Mode := Mode - [objDeleteFailed];
        Mode := Mode - [objDeleted];
        if ( ID > 0 ) then
        try
        { при необходимости открываем транзакцию }
            isBeforeTransactionOpen := FALSE;
            if ( not DB.isTransactionOpen ) then
                DB.BeginTransaction
            else
                isBeforeTransactionOpen := TRUE;
        { удаляем мета-объект }
            query := Format ('DELETE '+
                             'FROM %s '+
                             'WHERE %s="%s" ',
                             [ GetClassTable,
                               PropAt [_id].Name, PropAt [_id].asDBFormat ]);
            // ShowMessage (query);
            DB.ExecSQL (query);
        { если ошибок не обнаружено - проводим транзакцию }
            if ( not isBeforeTransactionOpen ) then
                DB.Commit;
        { в случае возникновения ошибки - откатываем транзакцию }
        except on E: Exception do begin
            if ( not isBeforeTransactionOpen ) then
                DB.Rollback;
            raise Exception.CreateFmt (ERR_TMETAOBJECT_TRANSACTION,[E.Message]);
        end; end
        else
            raise Exception.Create (ERR_TMETAOBJECT_INVALID_ID);
        Mode := Mode + [objDeleted];
    except on E: Exception do begin
        Mode := Mode + [objDeleteFailed];
        _raise (['Delete',ERR_TMETAOBJECT_DELETE,E],
                ['{3726EC80-390C-4390-8C10-4594C8DDD865}']);
    end; end;
end;

class procedure TMetaObject.Delete (const aDB: TSQLiteDatabase; const anID: TID; aMode: TObjectMode = []);
begin
    try
        if ( not Assigned (aDB) ) then
            raise Exception.Create (ERR_TMETAOBJECT_INVALID_DATABASE);
        if ( anID > 0 ) then
            with Create (aDB,[anID],aMode) do
            try
                Delete;
            finally
                Free;
            end
        else
            raise Exception.Create (ERR_TMETAOBJECT_INVALID_ID);
    except on E: Exception do
        _raise (['Delete',ERR_TMETAOBJECT_DELETE,E],
                ['{A78C9009-6672-4050-B54B-56340FE50278}']);
    end;
end;

function TMetaObject.GetPropAt (anIndex: Integer) : TMetaProperty;
begin
    Result := NIL;
    try
        if inherited PropAt [anIndex].InheritsFrom (TMetaProperty) then
            Result := inherited PropAt [anIndex] as TMetaProperty;
    except on E: Exception do
        _raise ([ Format ('GetPropAt [%d]',[anIndex]), ERR_TMETAOBJECT_GET_PROPERTY_AT, E ],
                ['{B3006684-8B74-47D9-8F24-05B7624EFA89}']);
    end;
end;

procedure TMetaObject.SetPropAt (anIndex: Integer; anItem: TMetaProperty);
begin
    try
        if inherited PropAt [anIndex].InheritsFrom (TMetaProperty) then
            inherited PropAt [anIndex] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetPropAt [%d]',[anIndex]), ERR_TMETAOBJECT_SET_PROPERTY_AT, E ],
                ['{0843EE1D-A4DB-4A49-B7CE-6789D728E186}']);
    end;
end;

function TMetaObject.GetPropOf (aName: String) : TMetaProperty;
begin
    Result := NIL;
    try
        if inherited PropOf [aName].InheritsFrom (TMetaProperty) then
            Result := inherited PropOf [aName] as TMetaProperty;
    except on E: Exception do
        _raise ([ Format ('GetPropOf [%s]',[aName]), ERR_TMETAOBJECT_GET_PROPERTY_OF, E ],
                ['{523C3254-3BDD-4900-BAF3-D0B1A547043F}']);
    end;
end;

procedure TMetaObject.SetPropOf (aName: String; anItem: TMetaProperty);
begin
    try
        if inherited PropOf [aName].InheritsFrom (TMetaProperty) then
            inherited PropOf [aName] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetPropOf [%s]',[aName]), ERR_TMETAOBJECT_SET_PROPERTY_OF, E ],
                ['{A2B6BC6A-3A63-4C5D-AD1A-B116040D1743}']);
    end;
end;

function TMetaObject.GetID : TID;
begin
    try
        Result := PropAt [_id].asID;
    except on E: Exception do
        _raise (['GetID',ERR_TMETAOBJECT_GET_ID,E],
                ['{CC91EAF6-42F6-4AA4-A973-91E554C28507}']);
    end;
end;

procedure TMetaObject.SetID (const aValue: TID);
begin
    try
        PropAt [_id].asID := aValue;
    except on E: Exception do
        _raise (['SetID',ERR_TMETAOBJECT_SET_ID,E],
                ['{18672F99-689D-4BE6-87B5-1706D7B371E4}']);
    end;
end;

procedure TMetaObject.LoadFromStream (const aStream: TStream);
begin
    try
        Mode := Mode - [objImported];
        if not Assigned (aStream) then
            raise Exception.Create (ERR_TMETAOBJECT_INVALID_STREAM);
        { virtual }
        Mode := Mode + [objImported];
    except on E: Exception do
        _raise (['LoadFromStream',ERR_TMETAOBJECT_LOAD_FROM_STREAM,E],
                ['{118FB7EE-01B8-47C5-A559-6538BB389359}']);
    end;
end;

procedure TMetaObject.SaveToStream (out aStream: TStream);
begin
    try
        Mode := Mode - [objExported];
        if not Assigned (aStream) then
            raise Exception.Create (ERR_TMETAOBJECT_INVALID_STREAM);
        { virtual }
        Mode := Mode + [objExported];
    except on E: Exception do
        _raise (['SaveToStream',ERR_TMETAOBJECT_SAVE_TO_STREAM,E],
                ['{77A49A84-83EA-4F41-BC8C-75D9EDFC31D6}']);
    end;
end;
