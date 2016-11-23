{******************************************************************************}
{*  TCategorieStatus.imp.inc                                                  *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TCategorieStatus }
class function TCategorieStatus.GetClassName : String;
begin
    Result := CLS_TCATEGORIESTATUS_NAME;
end;

class function TCategorieStatus.GetClassVersion : TVersionInfo;
begin
    Result := inherited GetClassVersion;
end;

class function TCategorieStatus.GetClassTable : String;
begin
    Result := 'categorie_statuses';
end;

class function TCategorieStatus.GetClassProperties : TProtoProperties;
begin
    Result := NIL;
    try
        Result := inherited GetClassProperties;
        with Result do
        begin
            ItemAt [_id_external].Name := 'name';
            ItemAt [_id_external].Caption := PRP_TCATEGORIESTATUS_NAME;
            Register ( TMetaProperty, ['description',PRP_TCATEGORIESTATUS_DESCRIPTION,dtText,''],[prpStored] );
        end;
    except on E: Exception do
        _raise (['GetClassProperties',ERR_TCATEGORIESTATUS_GET_ClASS_PROPERTIES,E],
                ['{63036CD4-D137-41BB-8275-161533981D04}']);
    end;
end;

constructor TCategorieStatus.Create (const aDB: TSQLiteDatabase;
                                     anArgs: array of const;
                                     aMode: TObjectMode = []);
begin
    try
        inherited Create (aDB,anArgs,aMode);
        Mode := Mode - [objCreated];
        { третий параметр - описание }
        if notEmpty (2,anArgs) then
            Description := toString (anArgs [2]);
        Mode := Mode + [objCreated];
    except on E: Exception do begin
        Mode := Mode + [objCreateFailed];
        _raise (['Create',ERR_TCATEGORIESTATUS_CREATE,E],
                ['{3D130876-850D-48CD-8894-E5F102A31065}']);
    end; end;
end;

destructor TCategorieStatus.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TCATEGORIESTATUS_DESTROY,E],
                ['{3688E921-516E-4927-AF87-5030175C6A6D}']);
    end;
end;

class procedure TCategorieStatus.CreateClassTable (const aDB: TSQLiteDatabase);
var
    Props    : TProtoProperties;
    CtgProps : TProtoProperties;
    query    : String;
begin
    try
        inherited CreateClassTable (aDB);
        {$IFDEF DELETE_TRIGGER}
        //{ создаем триггер на удаление }
        //if not aDB.TriggerExists ( Format ('trigger_%s_%s',[GetClassTable,'delete']) ) then
        //begin
        //    Props := GetClassProperties;
        //    CtgProps := TCategorie.GetClassProperties;
        //    try
        //        { создаем триггер }
        //        query := Format ('CREATE TRIGGER trigger_%s_%s '+
        //                         'BEFORE DELETE '+
        //                         'ON %s '+
        //                         'FOR EACH ROW '+
        //                         'BEGIN '+
        //                             // delete categories
        //                             'DELETE '+
        //                             'FROM %s '+
        //                             'WHERE %s = OLD.%s; '+
        //                         'END ',
        //                         [ GetClassTable, 'delete',
        //                           GetClassTable,
        //                           // delete categories
        //                           GetClassTable,
        //                           CtgProps.ItemAt [_ctg_id_status].Name, Props.ItemAt [_ctg_status_id].Name ]);
        //        // ShowMessage (query);
        //        aDB.ExecSQL (query);
        //    finally
        //        CtgProps.FreeAll;
        //        FreeAndNil (CtgProps);
        //        Props.FreeAll;
        //        FreeAndNil (Props);
        //    end;
        //end;
        {$ENDIF DELETE_TRIGGER}
    except on E: Exception do
        _raise (['CreateClassTable',ERR_TCATEGORIESTATUS_CREATE_CLASS_TABLE,E],
                ['{1B36863D-EFCE-4881-8110-970807DD7AA7}']);
    end;
end;

function TCategorieStatus.Check : Boolean;
begin
    Result := TRUE;
    try
        Result := inherited Check;
    except on E: Exception do
        _raise (['Check',ERR_TCATEGORIESTATUS_CHECK,E],
                ['{A199C3A5-9385-4DE2-B845-A62FAE2044A7}']);
    end;
end;

{$IFNDEF DELETE_TRIGGER}
procedure TCategorieStatus.Delete;
var
    isBeforeTransactionOpen : Boolean;
begin
    try
        if objNoDelete in Mode then Exit;
        Mode := Mode - [objDeleteFailed];
        Mode := Mode - [objDeleted];
        try
        { при необходимости открываем транзакцию }
            isBeforeTransactionOpen := FALSE;
            if ( not DB.isTransactionOpen ) then
                DB.BeginTransaction
            else
                isBeforeTransactionOpen := TRUE;
            { удаляем статус категории }
            inherited Delete;
            Mode := Mode - [objDeleted];
            { удаляем категории с данным статусом }
            //TCategories.Delete (DB,[ NIL, NIL, NIL, NIL, NIL, _([ID]) ]);
        { если ошибок не обнаружено - проводим транзакцию }
            if ( not isBeforeTransactionOpen ) then
                DB.Commit;
        { в случае возникновения ошибки - откатываем транзакцию }
        except on E: Exception do begin
            if ( not isBeforeTransactionOpen ) then
                DB.Rollback;
            raise Exception.CreateFmt (ERR_TCATEGORIESTATUS_TRANSACTION,[E.Message]);
        end; end;
        Mode := Mode + [objDeleted];
    except on E: Exception do begin
        Mode := Mode + [objDeleteFailed];
        _raise (['Delete',ERR_TCATEGORIESTATUS_DELETE,E],
                ['{E2E967EC-7552-43F7-984A-83B7661AAD11}']);
    end; end;
end;
{$ENDIF DELETE_TRIGGER}

function TCategorieStatus.GetDescription : String;
begin
    Result := '';
    try
        Result := PropAt [_ctg_status_description].asText;
    except on E: Exception do
        _raise (['GetDescription',ERR_TCATEGORIESTATUS_GET_DESCRIPTION,E],
                ['{C6362B72-FBE5-4A31-8EFB-146DBE8E1C21}']);
    end;
end;

procedure TCategorieStatus.SetDescription (const aValue: String);
begin
    try
        PropAt [_ctg_status_description].asText := aValue;
    except on E: Exception do
        _raise (['SetDescription',ERR_TCATEGORIESTATUS_SET_DESCRIPTION,E],
                ['{51F06BEA-C50D-4518-B7D0-95A9E40586A4}']);
    end;
end;