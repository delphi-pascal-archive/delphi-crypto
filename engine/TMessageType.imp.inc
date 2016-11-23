{******************************************************************************}
{*  TMessageType.imp.inc                                                      *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TMessageType }
class function TMessageType.GetClassName : String;
begin
    Result := CLS_TMESSAGETYPE_NAME;
end;

class function TMessageType.GetClassVersion : TVersionInfo;
begin
    Result := inherited GetClassVersion;
end;

class function TMessageType.GetClassTable : String;
begin
    Result := 'message_types';
end;

class function TMessageType.GetClassProperties : TProtoProperties;
begin
    Result := NIL;
    try
        Result := inherited GetClassProperties;
        with Result do
        begin
            ItemAt [_id_external].Name := 'name';
            ItemAt [_id_external].Caption := PRP_TMESSAGETYPE_NAME;
            Register ( TMetaProperty, ['description',PRP_TMESSAGETYPE_DESCRIPTION,dtText,''],[prpStored] );
        end;
    except on E: Exception do
        _raise (['GetClassProperties',ERR_TMESSAGETYPE_GET_ClASS_PROPERTIES,E],
                ['{72A67DC7-5496-4729-9F8E-C7F7258EF4FF}']);
    end;
end;

constructor TMessageType.Create (const aDB: TSQLiteDatabase;
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
        _raise (['Create',ERR_TMESSAGETYPE_CREATE,E],
                ['{4E4C320F-B31E-408C-8187-CA2D14AD1236}']);
    end; end;
end;

destructor TMessageType.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TMESSAGETYPE_DESTROY,E],
                ['{4301E166-0F5B-4EB2-9B17-E727CEF01170}']);
    end;
end;

class procedure TMessageType.CreateClassTable (const aDB: TSQLiteDatabase);
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
        //                           MsgProps.ItemAt [_msg_id_type].Name, Props.ItemAt [_msg_type_id].Name ]);
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
        _raise (['CreateClassTable',ERR_TMESSAGETYPE_CREATE_CLASS_TABLE,E],
                ['{4C79994B-D1BE-4FE1-A3F7-3D311670C81A}']);
    end;
end;

function TMessageType.Check : Boolean;
begin
    Result := TRUE;
    try
        Result := inherited Check;
    except on E: Exception do
        _raise (['Check',ERR_TMESSAGETYPE_CHECK,E],
                ['{5BF5D0B9-9064-417D-B5E2-1B574070D8E5}']);
    end;
end;

{$IFNDEF DELETE_TRIGGER}
procedure TMessageType.Delete;
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
            { удаляем тип сообщения }
            inherited Delete;
            Mode := Mode - [objDeleted];
            { удаляем сообщения данного типа }
            //TMessages.Delete (DB,[ NIL, NIL, NIL, NIL, _([ID]) ]);
        { если ошибок не обнаружено - проводим транзакцию }
            if ( not isBeforeTransactionOpen ) then
                DB.Commit;
        { в случае возникновения ошибки - откатываем транзакцию }
        except on E: Exception do begin
            if ( not isBeforeTransactionOpen ) then
                DB.Rollback;
            raise Exception.CreateFmt (ERR_TMESSAGETYPE_TRANSACTION,[E.Message]);
        end; end;
        Mode := Mode + [objDeleted];
    except on E: Exception do begin
        Mode := Mode + [objDeleteFailed];
        _raise (['Delete',ERR_TMESSAGETYPE_DELETE,E],
                ['{2DF2B420-442B-4F4A-8B9F-A619FED637C5}']);
    end; end;
end;
{$ENDIF DELETE_TRIGGER}

function TMessageType.GetDescription : String;
begin
    Result := '';
    try
        Result := PropAt [_msg_type_description].asText;
    except on E: Exception do
        _raise (['GetDescription',ERR_TMESSAGETYPE_GET_DESCRIPTION,E],
                ['{BFB64B44-7CEC-4600-A265-CD316D3EBD40}']);
    end;
end;

procedure TMessageType.SetDescription (const aValue: String);
begin
    try
        PropAt [_msg_type_description].asText := aValue;
    except on E: Exception do
        _raise (['SetDescription',ERR_TMESSAGETYPE_SET_DESCRIPTION,E],
                ['{21FB202A-C251-4C83-BF8F-3834C2323604}']);
    end;
end;
