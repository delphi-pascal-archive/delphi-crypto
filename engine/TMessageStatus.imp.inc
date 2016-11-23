{******************************************************************************}
{*  TMessageStatus.imp.inc                                                    *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TMessageStatus }
class function TMessageStatus.GetClassName : String;
begin
    Result := CLS_TMESSAGESTATUS_NAME;
end;

class function TMessageStatus.GetClassVersion : TVersionInfo;
begin
    Result := inherited GetClassVersion;
end;

class function TMessageStatus.GetClassTable : String;
begin
    Result := 'message_statuses';
end;

class function TMessageStatus.GetClassProperties : TProtoProperties;
begin
    Result := NIL;
    try
        Result := inherited GetClassProperties;
        with Result do
        begin
            ItemAt [_id_external].Name := 'name';
            ItemAt [_id_external].Caption := PRP_TMESSAGESTATUS_NAME;
            Register ( TMetaProperty, ['description',PRP_TMESSAGESTATUS_DESCRIPTION,dtText,''],[prpStored] );
        end;
    except on E: Exception do
        _raise (['GetClassProperties',ERR_TMESSAGESTATUS_GET_ClASS_PROPERTIES,E],
                ['{CA21CD95-6825-4B27-AE7E-6D996BD14F44}']);
    end;
end;

constructor TMessageStatus.Create (const aDB: TSQLiteDatabase;
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
        _raise (['Create',ERR_TMESSAGESTATUS_CREATE,E],
                ['{8A53237D-639D-4EF2-B202-41DE568AFC0E}']);
    end; end;
end;

destructor TMessageStatus.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TMESSAGESTATUS_DESTROY,E],
                ['{E74BF4D3-8D39-4DC8-8E47-1C941F7192C0}']);
    end;
end;

class procedure TMessageStatus.CreateClassTable (const aDB: TSQLiteDatabase);
var
    Props    : TProtoProperties;
    MsgProps : TProtoProperties;
    query    : String;
begin
    try
        inherited CreateClassTable (aDB);
        {$IFDEF DELETE_TRIGGER}
        //{ создаем триггер на удаление }
        //if not aDB.TriggerExists ( Format ('trigger_%s_%s',[GetClassTable,'delete']) ) then
        //begin
        //    Props := GetClassProperties;
        //    MsgProps := TMessage.GetClassProperties;
        //    try
        //        { создаем триггер }
        //        query := Format ('CREATE TRIGGER trigger_%s_%s '+
        //                         'BEFORE DELETE '+
        //                         'ON %s '+
        //                         'FOR EACH ROW '+
        //                         'BEGIN '+
        //                             // delete messages
        //                             'DELETE '+
        //                             'FROM %s '+
        //                             'WHERE %s = OLD.%s; '+
        //                         'END ',
        //                         [ GetClassTable, 'delete',
        //                           GetClassTable,
        //                           // delete messages
        //                           GetClassTable,
        //                           MsgProps.ItemAt [_msg_id_status].Name, Props.ItemAt [_msg_status_id].Name ]);
        //        // ShowMessage (query);
        //        aDB.ExecSQL (query);
        //    finally
        //        MsgProps.FreeAll;
        //        FreeAndNil (MsgProps);
        //        Props.FreeAll;
        //        FreeAndNil (Props);
        //    end;
        //end;
        {$ENDIF DELETE_TRIGGER}
    except on E: Exception do
        _raise (['CreateClassTable',ERR_TMESSAGESTATUS_CREATE_CLASS_TABLE,E],
                ['{16E6B215-A0B5-418C-BEA6-B929B3BE9FE9}']);
    end;
end;

function TMessageStatus.Check : Boolean;
begin
    Result := TRUE;
    try
        Result := inherited Check;
    except on E: Exception do
        _raise (['Check',ERR_TMESSAGESTATUS_CHECK,E],
                ['{D6C770B0-141E-409B-A07F-611DD0914C5C}']);
    end;
end;

procedure TMessageStatus.Delete; 
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
            { удаляем статус сообщения }
            inherited Delete;
            Mode := Mode - [objDeleted];
            { удаляем сообщения с данным статусом }
            //TMessages.Delete (DB,[ NIL, NIL, NIL, NIL, NIL, _([ID]) ]);
        { если ошибок не обнаружено - проводим транзакцию }
            if ( not isBeforeTransactionOpen ) then
                DB.Commit;
        { в случае возникновения ошибки - откатываем транзакцию }
        except on E: Exception do begin
            if ( not isBeforeTransactionOpen ) then
                DB.Rollback;
            raise Exception.CreateFmt (ERR_TMESSAGESTATUS_TRANSACTION,[E.Message]);
        end; end;
        Mode := Mode + [objDeleted];
    except on E: Exception do begin
        Mode := Mode + [objDeleteFailed];
        _raise (['Delete',ERR_TMESSAGESTATUS_DELETE,E],
                ['{4E528DAE-6147-4D34-983C-FC70722C29B2}']);
    end; end;
end;

function TMessageStatus.GetDescription : String;
begin
    Result := '';
    try
        Result := PropAt [_msg_status_description].asText;
    except on E: Exception do
        _raise (['GetDescription',ERR_TMESSAGESTATUS_GET_DESCRIPTION,E],
                ['{548C9826-F36B-46E1-9C29-95E6F66323C2}']);
    end;
end;

procedure TMessageStatus.SetDescription (const aValue: String);
begin
    try
        PropAt [_msg_status_description].asText := aValue;
    except on E: Exception do
        _raise (['SetDescription',ERR_TMESSAGESTATUS_SET_DESCRIPTION,E],
                ['{2CB45F85-6C07-4A1B-A274-799E3A2EA241}']);
    end;
end;