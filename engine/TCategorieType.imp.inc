{******************************************************************************}
{*  TCategorieType.imp.inc                                                    *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TCategorieType }
class function TCategorieType.GetClassName : String;
begin
    Result := CLS_TCATEGORIETYPE_NAME;
end;

class function TCategorieType.GetClassVersion : TVersionInfo;
begin
    Result := inherited GetClassVersion;
end;

class function TCategorieType.GetClassTable : String;
begin
    Result := 'categorie_types';
end;

class function TCategorieType.GetClassProperties : TProtoProperties;
begin
    Result := NIL;
    try
        Result := inherited GetClassProperties;
        with Result do
        begin
            ItemAt [_id_external].Name := 'name';
            ItemAt [_id_external].Caption := PRP_TCATEGORIETYPE_NAME;
            Register ( TMetaProperty, ['description',PRP_TCATEGORIETYPE_DESCRIPTION,dtText,''],[prpStored] );
        end;
    except on E: Exception do
        _raise (['GetClassProperties',ERR_TCATEGORIETYPE_GET_ClASS_PROPERTIES,E],
                ['{3F176902-1ABE-4F5D-97F1-109EBBF0A96B}']);
    end;
end;

constructor TCategorieType.Create (const aDB: TSQLiteDatabase;
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
        _raise (['Create',ERR_TCATEGORIETYPE_CREATE,E],
                ['{19FBFDDA-F400-45DB-8679-9265BE544A78}']);
    end; end;
end;

destructor TCategorieType.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TCATEGORIETYPE_DESTROY,E],
                ['{D4C0FAD6-5B57-4EAB-9636-7BFABD6689A9}']);
    end;
end;

class procedure TCategorieType.CreateClassTable (const aDB: TSQLiteDatabase);
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
        //                           CtgProps.ItemAt [_ctg_id_type].Name, Props.ItemAt [_ctg_type_id].Name ]);
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
        _raise (['CreateClassTable',ERR_TCATEGORIETYPE_CREATE_CLASS_TABLE,E],
                ['{19235546-AB21-44D1-9B99-CDC418334224}']);
    end;
end;

function TCategorieType.Check : Boolean;
begin
    Result := TRUE;
    try
        Result := inherited Check;
    except on E: Exception do
        _raise (['Check',ERR_TCATEGORIETYPE_CHECK,E],
                ['{58B66DCB-5792-46EF-87CD-F2A86606C58B}']);
    end;
end;

{$IFNDEF DELETE_TRIGGER}
procedure TCategorieType.Delete;
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
            { удаляем тип категории }
            inherited Delete;
            Mode := Mode - [objDeleted];
            { удаляем категории данного типа }
            //TCategories.Delete (DB,[ NIL, NIL, NIL, NIL, _([ID]) ]);
        { если ошибок не обнаружено - проводим транзакцию }
            if ( not isBeforeTransactionOpen ) then
                DB.Commit;
        { в случае возникновения ошибки - откатываем транзакцию }
        except on E: Exception do begin
            if ( not isBeforeTransactionOpen ) then
                DB.Rollback;
            raise Exception.CreateFmt (ERR_TCATEGORIETYPE_TRANSACTION,[E.Message]);
        end; end;
        Mode := Mode + [objDeleted];
    except on E: Exception do begin
        Mode := Mode + [objDeleteFailed];
        _raise (['Delete',ERR_TCATEGORIETYPE_DELETE,E],
                ['{2015EB10-3E54-47B0-BBDC-F9D50CFE3095}']);
    end; end;
end;
{$ENDIF DELETE_TRIGGER}

function TCategorieType.GetDescription : String;
begin
    Result := '';
    try
        Result := PropAt [_ctg_type_description].asText;
    except on E: Exception do
        _raise (['GetDescription',ERR_TCATEGORIETYPE_GET_DESCRIPTION,E],
                ['{1581B138-CB94-43DF-8C83-1FFC345B937D}']);
    end;
end;

procedure TCategorieType.SetDescription (const aValue: String);
begin
    try
        PropAt [_ctg_type_description].asText := aValue;
    except on E: Exception do
        _raise (['SetDescription',ERR_TCATEGORIETYPE_SET_DESCRIPTION,E],
                ['{98AF944A-D8A4-494D-A70F-637BFBFB891C}']);
    end;
end;