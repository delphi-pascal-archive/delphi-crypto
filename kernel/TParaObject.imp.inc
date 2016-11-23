{******************************************************************************}
{*  TParaObject.imp.inc                                                       *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TParaObject }
class function TParaObject.GetClassName : String;
begin
    Result := CLS_TPARAOBJECT_NAME;
end;

class function TParaObject.GetClassVersion : TVersionInfo;
begin
    Result := inherited GetClassVersion;
end;

class function TParaObject.GetClassTable : String;
begin
    Result := 'para_objects';
end;

class function TParaObject.GetClassProperties : TProtoProperties;
begin
    Result := NIL;
    try
        Result := inherited GetClassProperties;
        with Result do
        begin
            Register ( TMetaProperty, ['id_external',PRP_TPARAOBJECT_ID_EXTERNAL,dtString,''],[prpStored] );
        end;
    except on E: Exception do
        _raise (['GetClassProperties',ERR_TPARAOBJECT_GET_ClASS_PROPERTIES,E],
                ['{87304FF4-F1AA-4EF5-A3AD-8C743549C280}']);
    end;
end;

class function TParaObject.GetClassList : CProtoObjects;
begin
    Result := TParaObjects;
end;

constructor TParaObject.Create (const aDB: TSQLiteDatabase;
                                anArgs: array of const;
                                aMode: TObjectMode = []);
begin
    try
        inherited Create (aDB,anArgs,aMode);
        Mode := Mode - [objCreated];
        { второй параметр - внешний идентификатор }
        if notEmpty (1,anArgs) then
            IDExternal := toString (anArgs [1]);
        Mode := Mode + [objCreated];
    except on E: Exception do begin
        Mode := Mode + [objCreateFailed];
        _raise (['Create',ERR_TPARAOBJECT_CREATE,E],
                ['{E1DB2B2E-9985-455E-99B2-3B980A3C59B3}']);
    end; end;
end;

destructor TParaObject.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TPARAOBJECT_DESTROY,E],
                ['{02B5C5E5-41A8-4269-9E05-3E2689B49F8E}']);
    end;
end;

class procedure TParaObject.CreateClassTable (const aDB: TSQLiteDatabase);
var
    Props : TProtoProperties;
    Prop  : TMetaProperty;
    query : String;
begin
    try
        inherited CreateClassTable (aDB);
        begin
            Props := GetClassProperties;
            try
                { создаем индекс по пара-идентификатору }
                Prop := TMetaProperty (Props.ItemAt [_id_external]);
                if not aDB.IndexExists ( Format ('index_%s_%s',[GetClassTable,Prop.Name]) ) then
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
        end;
    except on E: Exception do
        _raise (['CreateClassTable',ERR_TPARAOBJECT_CREATE_CLASS_TABLE,E],
                ['{7934D67B-16AF-4135-9891-04C1BFB40882}']);
    end;
end;

function TParaObject.Check : Boolean;
begin
    Result := TRUE;
    try
        Result := inherited Check;
        if Result then
        begin
            if not ( Length (IDExternal) > 0 ) then
                Error (ERR_TPARAOBJECT_INVALID_ID_EXTERNAL);
            Result := ( Errors.Count = 0 );
        end;
    except on E: Exception do
        _raise (['Check',ERR_TPARAOBJECT_CHECK,E],
                ['{28E2D8FF-C86F-4F58-93FA-0430AEB9873F}']);
    end;
end;

class function TParaObject.Find (const aDB: TSQLiteDatabase;
                                 const anIDExternal: String) : TID;
var
    query : String;
    table : TSQLiteTable;
    I     : Integer;
    Props : TProtoProperties;
begin
    Result := 0;
    try
        if not Assigned (aDB) then
            raise Exception.Create (ERR_TPARAOBJECT_INVALID_DATABASE);
        Props := GetClassProperties;
        try
            Props.ItemAt [_id_external].asString := anIDExternal;

            query := Format ('SELECT %s '+
                             'FROM %s '+
                             'WHERE %s="%s" ',
                             [ Props.ItemAt [_id].Name,
                               GetClassTable,
                               Props.ItemAt [_id_external].Name, TMetaProperty (Props.ItemAt [_id_external]).asDBFormat ]);
            //ShowMessage (query);
            table := aDB.GetTable (query);
            with table do
            try
                for I := 0 to table.Count-1 do
                begin
                    if ( I > 0 ) then
                        raise Exception.CreateFmt (ERR_TPARAOBJECT_NOT_UNIQUE_ID_EXTERNAL,
                                                   [ Props.ItemAt [_id_external].Name,
                                                     TMetaProperty (Props.ItemAt [_id_external]).asDBFormat ]);
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
        _raise (['Find',ERR_TPARAOBJECT_FIND,E],
                ['{EE5C3B0F-9A06-427C-81FB-2F974FB232AD}']);
    end;
end;

function TParaObject.Find : TID;
begin
    Result := 0;
    try
        Result := Find (DB,IDExternal);
    except on E: Exception do
        _raise (['Find',ERR_TPARAOBJECT_FIND,E],
                ['{AD525EAA-40A6-4CD3-823D-771916F4B1B6}']);
    end;
end;

procedure TParaObject.Load;
begin
    try
        if objNoLoad in Mode then Exit;
        Mode := Mode - [objLoadFailed];
        Mode := Mode - [objLoaded];
        { если указан идентификатор,
          то загружаем объект по идентификатору }
        if ( ID > 0 ) then
            inherited Load
        { если не указан идентификатор,
          но указан пара-идентификатор,
          то загружаем объект по пара-идентификатору }
        else if ( ID = 0 ) and ( Length (IDExternal) > 0 ) then
        begin
            ID := Find;
            inherited Load;
        end
        else
            raise Exception.CreateFmt ('%s'#13#10'%s',
                                       [ ERR_TPARAOBJECT_INVALID_ID,
                                         ERR_TPARAOBJECT_INVALID_ID_EXTERNAL ]);
        { ставим в соответствие наименованию объекта его пара-идентификатор }
        Properties.ItemAt [_object_name].asString := PropAt [_id_external].asString;
        Mode := Mode + [objLoaded];
    except on E: Exception do begin
        Mode := Mode + [objLoadFailed];
        _raise (['Load',ERR_TPARAOBJECT_LOAD,E],
                ['{ABA3933D-EDF9-4E93-B982-DAE19453AD84}']);
    end; end;
end;

class function TParaObject.Load (const aDB: TSQLiteDatabase; const anIDExternal: String) : TParaObject;
begin
    Result := NIL;
    try
        if not Assigned (aDB) then
            raise Exception.Create (ERR_TPARAOBJECT_INVALID_DATABASE);
        if ( Length (anIDExternal) > 0 ) then
        begin
            Result := Create (aDB,[0,anIDExternal]);
            with Result do
            try
                Load;
            finally
                if Assigned (Result) and ( (Result.ID = 0) or (objLoadFailed in Mode) ) then
                    FreeAndNil (Result);
            end;
        end
        else
            raise Exception.Create (ERR_TPARAOBJECT_INVALID_ID_EXTERNAL);
    except on E: Exception do
        _raise (['Load',ERR_TPARAOBJECT_LOAD,E],
                ['{6573561E-615D-412C-968A-C2BD7102BEE3}']);
    end;
end;

procedure TParaObject.Save;
var
    isBeforeTransactionOpen : Boolean;
var
    FoundID : TID;
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

            { проверяем наличие записи с данным id_external в БД }
            if ( Length (IDExternal) > 0 ) then
                FoundID := Find;
            if ( FoundID > 0 ) and ( ID = 0 ) then
                ID := FoundID
            else if ( FoundID > 0 ) and ( ID <> FoundID ) then
                raise Exception.CreateFmt (ERR_TPARAOBJECT_NOT_UNIQUE_ID_EXTERNAL,
                                           [ PropAt [_id_external].Name,
                                             PropAt [_id_external].asDBFormat ]);
            { сохраняем }
            inherited Save;
        { если ошибок не обнаружено - проводим транзакцию }
            if ( not isBeforeTransactionOpen ) then
                DB.Commit;
            Mode := Mode + [objSaved];
        { в случае возникновения ошибки - откатываем транзакцию }
        except on E: Exception do begin
            if ( not isBeforeTransactionOpen ) then
                DB.Rollback;
            raise Exception.CreateFmt (ERR_TMETAOBJECT_TRANSACTION,[E.Message]);
        end; end;
    except on E: Exception do begin
        Mode := Mode + [objSaveFailed];
        _raise (['Save',ERR_TPARAOBJECT_SAVE,E],
                ['{12E06E2D-CBAF-4706-983B-6AAF4E6A201D}']);
    end; end;
end;

procedure TParaObject.Delete;
var
    query                   : String;
    isBeforeTransactionOpen : Boolean;
begin
    try
        if objNoDelete in Mode then Exit;
        Mode := Mode - [objDeleteFailed];
        Mode := Mode - [objDeleted];
        { если указан идентификатор,
          то удаляем объект по идентификатору }
        if ( ID > 0 ) then
            inherited Delete
        { если не указан идентификатор,
          но указан пара-идентификатор,
          то удаляем объект по пара-идентификатору }
        else if ( ID = 0 ) and ( Length (IDExternal) > 0 ) then
        try
        { при необходимости открываем транзакцию }
            isBeforeTransactionOpen := FALSE;
            if ( not DB.isTransactionOpen ) then
                DB.BeginTransaction
            else
                isBeforeTransactionOpen := TRUE;
        { удаляем пара-объект }
            query := Format ('DELETE '+
                             'FROM %s '+
                             'WHERE %s="%s" ',
                             [ GetClassTable,
                               PropAt [_id_external].Name, PropAt [_id_external].asDBFormat ]);
            // ShowMessage (query);
            DB.ExecSQL (query);
        { если ошибок не обнаружено - проводим транзакцию }
            if ( not isBeforeTransactionOpen ) then
                DB.Commit;
        { в случае возникновения ошибки - откатываем транзакцию }
        except on E: Exception do begin
            if ( not isBeforeTransactionOpen ) then
                DB.Rollback;
            raise Exception.CreateFmt (ERR_TPARAOBJECT_TRANSACTION,[E.Message]);
        end; end
        else
            raise Exception.CreateFmt ('%s'#13#10'%s',
                                       [ ERR_TPARAOBJECT_INVALID_ID,
                                         ERR_TPARAOBJECT_INVALID_ID_EXTERNAL ]);
        Mode := Mode + [objDeleted];
    except on E: Exception do begin
        Mode := Mode + [objDeleteFailed];
        _raise (['Delete',ERR_TPARAOBJECT_DELETE,E],
                ['{E94EF09C-702D-45FD-A02B-F1252FA819B2}']);
    end; end;
end;

class procedure TParaObject.Delete (const aDB: TSQLiteDatabase;
                                    const anIDExternal: String);
begin
    try
        if ( not Assigned (aDB) ) then
            raise Exception.Create (ERR_TPARAOBJECT_INVALID_DATABASE);
        if ( Length (anIDExternal) > 0 ) then
            with Create (aDB,[0,anIDExternal]) do
            try
                Delete;
            finally
                Free;
            end
        else
            raise Exception.Create (ERR_TPARAOBJECT_INVALID_ID_EXTERNAL);
    except on E: Exception do
        _raise (['Delete',ERR_TMETAOBJECT_DELETE,E],
                ['{474188D1-4AAB-4BA6-B33E-9598F07518E3}']);
    end;
end;

function TParaObject.GetIDExternal : String;
begin
    try
        Result := PropAt [_id_external].asString;
    except on E: Exception do
        _raise (['GetIDExternal',ERR_TPARAOBJECT_GET_ID_EXTERNAL,E],
                ['{AA14990C-8637-48A5-BC10-168368A95D3D}']);
    end;
end;

procedure TParaObject.SetIDExternal (const aValue: String);
begin
    try
        PropAt [_id_external].asString := aValue;
        Properties.ItemAt [_object_name].asString := PropAt [_id_external].asString;
    except on E: Exception do
        _raise (['SetIDExternal',ERR_TPARAOBJECT_SET_ID_EXTERNAL,E],
                ['{506D2840-0305-483C-B9B5-D535EAFF45C2}']);
    end;
end;

procedure TParaObject.SetObjectName (const aValue: String);
begin
    try
        inherited SetObjectName (aValue);
        if not Properties.ItemAt [_id_external].ReadOnly then
            PropAt [_id_external].asString := Properties.ItemAt [_object_name].asString;
    except on E: Exception do
        _raise (['SetObjectName',ERR_TPARAOBJECT_SET_OBJECT_NAME,E],
                ['{B1E846F5-BACC-4BE5-B4E2-E61B6B555CE8}']);
    end;
end;
