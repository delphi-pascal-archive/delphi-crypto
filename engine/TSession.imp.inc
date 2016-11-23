{******************************************************************************}
{*  TSession.imp.inc                                                          *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2012                                             *}
{******************************************************************************}
{ TSession }
class function TSession.GetClassName : String;
begin
    Result := CLS_TSESSION_NAME;
end;

class function TSession.GetClassVersion : TVersionInfo;
begin
    Result := inherited GetClassVersion;
end;

class function TSession.GetClassTable : String;
begin
    Result := 'sessions';
end;

class function TSession.GetClassProperties : TProtoProperties;
begin
    Result := NIL;
    try
        Result := inherited GetClassProperties;
        with Result do
        begin
            Register ( TMetaProperty, ['name',PRP_TSESSION_NAME,dtString,''],[prpStored] );
            Register ( TMetaProperty, ['value',PRP_TSESSION_VALUE,dtSafe,''],[prpStored] );
            if ( ItemAt [_ssn_value].DataType <> dtSafe ) then
                raise Exception.Create ('The value was not encrypted. Hacking attempt!');
            Register ( TMetaProperty, ['id_owner',PRP_TSESSION_ID_OWNER,dtID,0],[prpStored] );
            Register ( TMetaProperty, ['time_stamp_start',PRP_TSESSION_TIME_STAMP_START,dtDateTime,0],[prpStored] );
            Register ( TMetaProperty, ['time_stamp_finish',PRP_TSESSION_TIME_STAMP_FINISH,dtDateTime,0],[prpStored] );
        end;
    except on E: Exception do
        _raise (['GetClassProperties',ERR_TSESSION_GET_ClASS_PROPERTIES,E],
                ['{437856B6-7AC0-47BD-936A-DDBCD797680E}']);
    end;
end;

class function TSession.GetClassList : CProtoObjects;
begin
    Result := TSessions;
end;

constructor TSession.Create (const aDB: TSQLiteDatabase;
                             anArgs: array of const;
                             aMode: TObjectMode = []);
begin
    try
        inherited Create (aDB,[],aMode);
        Mode := Mode - [objCreated];
        { первый параметр - идентификатор }
        if notEmpty (0,anArgs) then
            ID := toInt64 (anArgs [0]);
        { второй параметр - наименование }
        Name := '';
        if notEmpty (1,anArgs) then
            Name := toString (anArgs [1]);
        { третий параметр - идентификатор владельца }
        if notEmpty (2,anArgs) then
            IDOwner := toInt64 (anArgs [2]);
        if Assigned (User) and ( IDOwner = 0 ) then
            IDOwner := USER_ID;
        { четвертый параметр - время начала действия сессии }
        TimeStampStart := now;
        if notEmpty (3,anArgs) then
            TimeStampStart := toDateTime (anArgs [3]);
        { пятый параметр - время окончания действия сессии }
        TimeStampFinish := TimeStampStart;
        if notEmpty (4,anArgs) then
            TimeStampFinish := toDateTime (anArgs [4]);
        { генерируем соль }
        if Assigned (User) and Assigned (User.Crypto) then
            Salt := User.Crypto.GenerateKey;
        Mode := Mode + [objCreated];
    except on E: Exception do begin
        Mode := Mode + [objCreateFailed];
        _raise (['Create',ERR_TSESSION_CREATE,E],
                ['{D970A08C-22FB-4D5D-8022-2A0617A3DDF6}']);
    end; end;
end;

destructor TSession.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TSESSION_DESTROY,E],
                ['{BE0A0991-4E4A-4CE2-81DE-E63212FC31DB}']);
    end;
end;

class procedure TSession.CreateClassTable (const aDB: TSQLiteDatabase);
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
                { создаем индекс по наименованию }
                Prop := TMetaProperty (Props.ItemAt [_ssn_name]);
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
        _raise (['CreateClassTable',ERR_TSESSION_CREATE_CLASS_TABLE,E],
                ['{E4312883-0AEC-44BB-84DE-34C4AFF2DFFF}']);
    end;
end;

function TSession.Check : Boolean;
begin
    Result := TRUE;
    try
        Result := inherited Check;
        if Result then
        begin
            if not ( Length (Name) > 0 ) then
                Error (ERR_TSESSION_INVALID_NAME);
            {if not ( TimeStampStart > 0 ) then
                Error (ERR_TSESSION_INVALID_TIME_STAMP_START);
            if not ( TimeStampFinish > 0 ) then
                Error (ERR_TSESSION_INVALID_TIME_STAMP_FINISH);}
            Result := ( Errors.Count = 0 );
        end;
    except on E: Exception do
        _raise (['Check',ERR_TSESSION_CHECK,E],
                ['{3B7CE618-022E-4BFC-AE8E-4484E79587BB}']);
    end;
end;

function TSession.GetIDOwner : TID;
begin
    Result := 0;
    try
        Result := PropAt [_ssn_id_owner].asID;
    except on E: Exception do
        _raise (['GetIDOwner',ERR_TSESSION_GET_ID_OWNER,E],
                ['{94B509D8-B3BA-4043-B8F6-79377CBD83BB}']);
    end;
end;

procedure TSession.SetIDOwner (const aValue: TID);
begin
    try
        if ( aValue >= 0 ) then
            PropAt [_ssn_id_owner].asID := aValue
        else
            PropAt [_ssn_id_owner].asID := 0;
    except on E: Exception do
        _raise (['SetIDOwner',ERR_TSESSION_SET_ID_OWNER,E],
                ['{8212BBA5-E93C-4E14-A27A-A177D5AF6904}']);
    end;
end;

function TSession.GetTimeStampStart : TDateTime;
begin
    Result := 0.0;
    try
        Result := PropAt [_ssn_time_stamp_start].asDateTime;
    except on E: GetTimeStampStart do
        _raise (['GetTimeStampStart',ERR_TSESSION_GET_TIME_STAMP_START,E],
                ['{E89FE90D-1353-441D-A695-301A91C0049F}']);
    end;
end;

procedure TSession.SetTimeStampStart (const aValue: TDateTime);
begin
    try
        PropAt [_ssn_time_stamp_start].asDateTime := aValue;
    except on E: Exception do
        _raise (['SetTimeStampStart',ERR_TSESSION_SET_TIME_STAMP_START,E],
                ['{666DD6B3-26DE-4D99-9C2F-62E546997939}']);
    end;
end;

function TSession.GetTimeStampFinish : TDateTime;
begin
    Result := 0.0;
    try
        Result := PropAt [_ssn_time_stamp_finish].asDateTime;
    except on E: GetTimeStampFinish do
        _raise (['GetTimeStampFinish',ERR_TSESSION_GET_TIME_STAMP_FINISH,E],
                ['{C20FF3D7-597F-4033-B7BE-8E4A3550770D}']);
    end;
end;

procedure TSession.SetTimeStampFinish (const aValue: TDateTime);
begin
    try
        PropAt [_ssn_time_stamp_finish].asDateTime := aValue;
    except on E: Exception do
        _raise (['SetTimeStampFinish',ERR_TSESSION_SET_TIME_STAMP_FINISH,E],
                ['{E6D1A200-E0A7-4149-9211-7D1A71BDB104}']);
    end;
end;

