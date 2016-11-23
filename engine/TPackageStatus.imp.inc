{******************************************************************************}
{*  TPackageStatus.imp.inc                                                    *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TPackageStatus }
class function TPackageStatus.GetClassName : String;
begin
    Result := CLS_TPACKAGESTATUS_NAME;
end;

class function TPackageStatus.GetClassVersion : TVersionInfo;
begin
    Result := inherited GetClassVersion;
end;

class function TPackageStatus.GetClassTable : String;
begin
    Result := 'package_statuses';
end;

class function TPackageStatus.GetClassProperties : TProtoProperties;
begin
    Result := NIL;
    try
        Result := inherited GetClassProperties;
        with Result do
        begin
            ItemAt [_id_external].Name := 'name';
            ItemAt [_id_external].Caption := PRP_TPACKAGESTATUS_NAME;
            Register ( TMetaProperty, ['description',PRP_TPACKAGESTATUS_DESCRIPTION,dtText,''],[prpStored] );
        end;
    except on E: Exception do
        _raise (['GetClassProperties',ERR_TPACKAGESTATUS_GET_ClASS_PROPERTIES,E],
                ['{8CC599D6-2A5A-43E0-B7F6-4530607FB67E}']);
    end;
end;

constructor TPackageStatus.Create (const aDB: TSQLiteDatabase;
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
        _raise (['Create',ERR_TPACKAGESTATUS_CREATE,E],
                ['{F5811DB2-59DE-434F-BF84-A44EA9CE1C90}']);
    end; end;
end;

destructor TPackageStatus.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TPACKAGESTATUS_DESTROY,E],
                ['{277B910D-FA70-400B-A228-5A58131747CA}']);
    end;
end;

class procedure TPackageStatus.CreateClassTable (const aDB: TSQLiteDatabase);
var
    Props    : TProtoProperties;
    PckProps : TProtoProperties;
    query    : String;
begin
    try
        inherited CreateClassTable (aDB);
        {$IFDEF DELETE_TRIGGER}
        { создаем триггер на удаление }
        //if not aDB.TriggerExists ( Format ('trigger_%s_%s',[GetClassTable,'delete']) ) then
        //begin
        //    Props := GetClassProperties;
        //    PckProps := TPackage.GetClassProperties;
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
        //                           PckProps.ItemAt [_pck_id_status].Name, Props.ItemAt [_pck_status_id].Name ]);
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
        _raise (['CreateClassTable',ERR_TPACKAGESTATUS_CREATE_CLASS_TABLE,E],
                ['{86EBAEE3-82A8-45A6-913C-C092FD83B687}']);
    end;
end;

function TPackageStatus.Check : Boolean;
begin
    Result := TRUE;
    try
        Result := inherited Check;
    except on E: Exception do
        _raise (['Check',ERR_TPACKAGESTATUS_CHECK,E],
                ['{BC9513AF-7D15-4284-8DAA-97F6BFF552D5}']);
    end;
end;

{$IFNDEF DELETE_TRIGGER}
procedure TPackageStatus.Delete; 
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
            { удаляем статус пакета }
            inherited Delete;
            Mode := Mode - [objDeleted];
            { удаляем пакеты с данным статусом }
            //TPackages.Delete (DB,[ NIL, NIL, NIL, NIL, NIL, _([ID]) ]);
        { если ошибок не обнаружено - проводим транзакцию }
            if ( not isBeforeTransactionOpen ) then
                DB.Commit;
        { в случае возникновения ошибки - откатываем транзакцию }
        except on E: Exception do begin
            if ( not isBeforeTransactionOpen ) then
                DB.Rollback;
            raise Exception.CreateFmt (ERR_TPACKAGESTATUS_TRANSACTION,[E.Message]);
        end; end;
        Mode := Mode + [objDeleted];
    except on E: Exception do begin
        Mode := Mode + [objDeleteFailed];
        _raise (['Delete',ERR_TPACKAGESTATUS_DELETE,E],
                ['{8A820CA4-24DC-4294-85D9-2ACE0572C126}']);
    end; end;
end;
{$ENDIF DELETE_TRIGGER}

function TPackageStatus.GetDescription : String;
begin
    Result := '';
    try
        Result := PropAt [_pck_status_description].asText;
    except on E: Exception do
        _raise (['GetDescription',ERR_TPACKAGESTATUS_GET_DESCRIPTION,E],
                ['{DA68B0DA-107A-4D30-84B5-09930BBE7A63}']);
    end;
end;

procedure TPackageStatus.SetDescription (const aValue: String);
begin
    try
        PropAt [_pck_status_description].asText := aValue;
    except on E: Exception do
        _raise (['SetDescription',ERR_TPACKAGESTATUS_SET_DESCRIPTION,E],
                ['{D32A8C00-44D7-4DF5-B381-F0535F259951}']);
    end;
end;