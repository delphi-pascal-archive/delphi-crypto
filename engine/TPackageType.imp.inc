{******************************************************************************}
{*  TPackageType.imp.inc                                                      *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TPackageType }
class function TPackageType.GetClassName : String;
begin
    Result := CLS_TPACKAGETYPE_NAME;
end;

class function TPackageType.GetClassVersion : TVersionInfo;
begin
    Result := inherited GetClassVersion;
end;

class function TPackageType.GetClassTable : String;
begin
    Result := 'package_types';
end;

class function TPackageType.GetClassProperties : TProtoProperties;
begin
    Result := NIL;
    try
        Result := inherited GetClassProperties;
        with Result do
        begin
            ItemAt [_id_external].Name := 'name';
            ItemAt [_id_external].Caption := PRP_TPACKAGETYPE_NAME;
            Register ( TMetaProperty, ['description',PRP_TPACKAGETYPE_DESCRIPTION,dtText,''],[prpStored] );
        end;
    except on E: Exception do
        _raise (['GetClassProperties',ERR_TPACKAGETYPE_GET_ClASS_PROPERTIES,E],
                ['{7B86C167-9143-4983-B40E-C5751EBF561F}']);
    end;
end;

constructor TPackageType.Create (const aDB: TSQLiteDatabase;
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
        _raise (['Create',ERR_TPACKAGETYPE_CREATE,E],
                ['{02234B8F-E1E4-4253-9F8C-4914FE0CB3AE}']);
    end; end;
end;

destructor TPackageType.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TPACKAGETYPE_DESTROY,E],
                ['{0B7A6FAD-1B5D-48B3-90E3-8D2539466844}']);
    end;
end;

class procedure TPackageType.CreateClassTable (const aDB: TSQLiteDatabase);
var
    Props    : TProtoProperties;
    PckProps : TProtoProperties;
    query    : String;
begin
    try
        inherited CreateClassTable (aDB);
        {$IFDEF DELETE_TRIGGER}
        { создаем триггер на удаление }
        if not aDB.TriggerExists ( Format ('trigger_%s_%s',[GetClassTable,'delete']) ) then
        begin
            Props := GetClassProperties;
            PckProps := TPackage.GetClassProperties;
        //    try
        //        { создаем триггер }
        //        query := Format ('CREATE TRIGGER trigger_%s_%s '+
        //                         'BEFORE DELETE '+
        //                         'ON %s '+
        //                         'FOR EACH ROW '+
        //                         'BEGIN '+
        //                             // delete packages
        //                             'DELETE '+
        //                             'FROM %s '+
        //                             'WHERE %s = OLD.%s; '+
        //                         'END ',
        //                         [ GetClassTable, 'delete',
        //                           GetClassTable,
        //                           // delete packages
        //                           GetClassTable,
        //                           PckProps.ItemAt [_pck_id_type].Name, Props.ItemAt [_pck_type_id].Name ]);
        //        // ShowMessage (query);
        //        aDB.ExecSQL (query);
        //    finally
        //        PckProps.FreeAll;
        //        FreeAndNil (PckProps);
        //        Props.FreeAll;
        //        FreeAndNil (Props);
        //    end;
        //end;
        {$ENDIF DELETE_TRIGGER}
    except on E: Exception do
        _raise (['CreateClassTable',ERR_TPACKAGETYPE_CREATE_CLASS_TABLE,E],
                ['{0ED6E0DC-2215-4C97-BD3B-C4B0A1A82F9B}']);
    end;
end;

function TPackageType.Check : Boolean;
begin
    Result := TRUE;
    try
        Result := inherited Check;
    except on E: Exception do
        _raise (['Check',ERR_TPACKAGETYPE_CHECK,E],
                ['{70B65609-08E6-4FB7-9102-F1FB7D3BFF0A}']);
    end;
end;

{$IFNDEF DELETE_TRIGGER}
procedure TPackageType.Delete;
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
            { удаляем тип пакетов }
            inherited Delete;
            Mode := Mode - [objDeleted];
            { удаляем пакеты данного типа }
            // TPackages.Delete (DB,[ NIL, NIL, NIL, NIL, _([ID]) ]);
        { если ошибок не обнаружено - проводим транзакцию }
            if ( not isBeforeTransactionOpen ) then
                DB.Commit;
        { в случае возникновения ошибки - откатываем транзакцию }
        except on E: Exception do begin
            if ( not isBeforeTransactionOpen ) then
                DB.Rollback;
            raise Exception.CreateFmt (ERR_TPACKAGETYPE_TRANSACTION,[E.Message]);
        end; end;
        Mode := Mode + [objDeleted];
    except on E: Exception do begin
        Mode := Mode + [objDeleteFailed];
        _raise (['Delete',ERR_TPACKAGETYPE_DELETE,E],
                ['{86D49E01-C812-491D-A185-B95705B6D988}']);
    end; end;
end;
{$ENDIF DELETE_TRIGGER}

function TPackageType.GetDescription : String;
begin
    Result := '';
    try
        Result := PropAt [_pck_type_description].asText;
    except on E: Exception do
        _raise (['GetDescription',ERR_TPACKAGETYPE_GET_DESCRIPTION,E],
                ['{6A4A8D56-C008-4112-A4F0-4983A44BE7E8}']);
    end;
end;

procedure TPackageType.SetDescription (const aValue: String);
begin
    try
        PropAt [_pck_type_description].asText := aValue;
    except on E: Exception do
        _raise (['SetDescription',ERR_TPACKAGETYPE_SET_DESCRIPTION,E],
                ['{F43554DC-785B-4379-BF23-AA84F5A1E78C}']);
    end;
end;
