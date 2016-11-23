{******************************************************************************}
{*  THyperObject.imp.inc                                                      *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit & rat 2011                                       *}
{******************************************************************************}
{ THyperObject }
class function THyperObject.GetClassName : String;
begin
    Result := CLS_THYPEROBJECT_NAME;
end;

class function THyperObject.GetClassVersion : TVersionInfo;
begin
    Result := inherited GetClassVersion;
end;

class function THyperObject.GetClassTable : String;
begin
    Result := 'hyper_objects';
end;

class function THyperObject.GetClassProperties : TProtoProperties;
begin
    Result := NIL;
    try
        Result := inherited GetClassProperties;
        with Result do
        begin
            Register ( TMetaProperty, ['time_stamp_create',PRP_THYPEROBJECT_TIME_STAMP_CREATE,dtDateTime,0],[prpStored] );
            Register ( TMetaProperty, ['time_stamp_modify',PRP_THYPEROBJECT_TIME_STAMP_MODIFY,dtDateTime,0],[prpStored] );
            Register ( TMetaProperty, ['time_stamp_public',PRP_THYPEROBJECT_TIME_STAMP_PUBLIC,dtDateTime,0],[prpStored] );
            Register ( TMetaProperty, ['time_stamp_vector',PRP_THYPEROBJECT_TIME_STAMP_VECTOR,dtInt64,0],[prpStored] );
            Register ( TMetaProperty, ['version',PRP_THYPEROBJECT_VERSION,dtVersion,'0.0.0.0'],[prpStored] );
            ItemAt [_hyper_version].asVersion := GetClassVersion;
            Register ( TMetaProperty, ['key_hash',PRP_THYPEROBJECT_KEY_HASH,dtHex,''],[prpStored] );
            Register ( TMetaProperty, ['data_hash',PRP_THYPEROBJECT_DATA_HASH,dtHex,''],[prpStored] );
        end;
    except on E: Exception do
        _raise (['GetClassProperties',ERR_THYPEROBJECT_GET_ClASS_PROPERTIES,E],
                ['{B53E76DD-A1ED-4392-8FAA-692D1B62593A}']);
    end;
end;

class function THyperObject.GetClassList : CProtoObjects;
begin
    Result := THyperObjects;
end;

constructor THyperObject.Create (const aDB: TSQLiteDatabase;
                                anArgs: array of const;
                                aMode: TObjectMode = []);
begin
    try
        inherited Create (aDB,anArgs,aMode);
        Mode := Mode - [objCreated];
        { третий параметр - дата и врем€ создани€ }
        TimeStampCreate := now;
        if notEmpty (2,anArgs) then
            TimeStampCreate := toDateTime (anArgs [2]);
        { четвертый параметр - дата и врем€ последнего редактировани€ }
        TimeStampCreate := now;
        if notEmpty (3,anArgs) then
            TimeStampModify := toDateTime (anArgs [3]);
        { п€тый параметр - дата и врем€ публикации }
        if notEmpty (4,anArgs) then
            TimeStampPublic := toDateTime (anArgs [4]);
        { шестой параметр - верси€ }
        if notEmpty (5,anArgs) then
            Version := toVersionInfo (anArgs [5]);

        TimeStampVector := 0;

        Mode := Mode + [objCreated];
    except on E: Exception do begin
        Mode := Mode + [objCreateFailed];
        _raise (['Create',ERR_THYPEROBJECT_CREATE,E],
                ['{6D1A24A8-463A-409D-91C2-184619468EEA}']);
    end; end;
end;

destructor THyperObject.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_THYPEROBJECT_DESTROY,E],
                ['{83AAC9E2-ECA7-466D-95FE-54D792C716E9}']);
    end;
end;

class procedure THyperObject.CreateClassTable (const aDB: TSQLiteDatabase);
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
                { создаем индекс по хэшу }
                Prop := TMetaProperty (Props.ItemAt [_hyper_key_hash]);
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
                { создаем индекс по хэшу данных }
                Prop := TMetaProperty (Props.ItemAt [_hyper_data_hash]);
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
        _raise (['CreateClassTable',ERR_THYPEROBJECT_CREATE_CLASS_TABLE,E],
                ['{AD0BB0C1-285E-422D-B640-47038B31DAF3}']);
    end;
end;

function THyperObject.Check : Boolean;
begin
    Result := TRUE;
    try
        Result := inherited Check;
    except on E: Exception do
        _raise (['Check',ERR_THYPEROBJECT_CHECK,E],
                ['{F139E1B4-AFEA-40FC-97B6-17E686DC9F1F}']);
    end;
end;

procedure THyperObject.Save;
var
    CalculatedDataHash : String;
begin
    try
        if objNoSave in Mode then Exit;
        Mode := Mode - [objSaveFailed];
        Mode := Mode - [objSaved];
        if isEmpty ( PropAt [_hyper_key_hash].asHex ) then
            PropAt [_hyper_key_hash].asHex := CalcKeyHash;
        CalculatedDataHash := CalcDataHash;
        if ( PropAt [_hyper_data_hash].asHex <> CalculatedDataHash ) then
        begin
            PropAt [_hyper_data_hash].asHex := CalculatedDataHash;
            TimeStampVector := TimeStampVector + 1;
            TimeStampModify := now;
        end;
        inherited Save;
    except on E: Exception do begin
        Mode := Mode + [objSaveFailed];
        _raise (['Save',ERR_THYPEROBJECT_SAVE,E],
                ['{2D2296FF-49EA-4D87-9833-AC406B697DEA}']);
    end; end;
end;

function THyperObject.GetTimeStampCreate : TDateTime;
begin
    Result := 0.0;
    try
        Result := PropAt [_hyper_time_stamp_create].asDateTime;
    except on E: Exception do
        _raise (['GetTimeStampCreate',ERR_THYPEROBJECT_GET_TIME_STAMP_CREATE,E],
                ['{0E99F5DC-1755-4997-8268-A699EFAA30DA}']);
    end;
end;

procedure THyperObject.SetTimeStampCreate (const aValue: TDateTime);
begin
    try
        PropAt [_hyper_time_stamp_create].asDateTime := aValue;
    except on E: Exception do
        _raise (['SetTimeStampCreate',ERR_THYPEROBJECT_SET_TIME_STAMP_CREATE,E],
                ['{0B3A5597-CD8F-4434-9449-87B73405BD39}']);
    end;
end;

function THyperObject.GetTimeStampModify : TDateTime;
begin
    Result := 0.0;
    try
        Result := PropAt [_hyper_time_stamp_modify].asDateTime;
    except on E: Exception do
        _raise (['GetTimeStampModify',ERR_THYPEROBJECT_GET_TIME_STAMP_MODIFY,E],
                ['{66BA1FBA-3826-49DC-AE3D-286469224882}']);
    end;
end;

procedure THyperObject.SetTimeStampModify (const aValue: TDateTime);
begin
    try
        PropAt [_hyper_time_stamp_modify].asDateTime := aValue;
    except on E: Exception do
        _raise (['SetTimeStampModify',ERR_THYPEROBJECT_SET_TIME_STAMP_MODIFY,E],
                ['{9716F41F-588F-4442-8363-ED32BACDA2B5}']);
    end;
end;

function THyperObject.GetTimeStampPublic : TDateTime;
begin
    Result := 0.0;
    try
        Result := PropAt [_hyper_time_stamp_public].asDateTime;
    except on E: Exception do
        _raise (['GetTimeStampPublic',ERR_THYPEROBJECT_GET_TIME_STAMP_PUBLIC,E],
                ['{B1304885-593F-4975-AB50-29A1FA7B1FEC}']);
    end;
end;

procedure THyperObject.SetTimeStampPublic (const aValue: TDateTime);
begin
    try
        PropAt [_hyper_time_stamp_public].asDateTime := aValue;
    except on E: Exception do
        _raise (['SetTimeStampPublic',ERR_THYPEROBJECT_SET_TIME_STAMP_PUBLIC,E],
                ['{EE287F3C-E7B1-4AD9-A2E9-DAD1A5A1BB65}']);
    end;
end;

function THyperObject.GetTimeStampVector : Int64;
begin
    Result := 0;
    try
        Result := PropAt [_hyper_time_stamp_vector].asInt64;
    except on E: Exception do
        _raise (['GetTimeStampVector',ERR_THYPEROBJECT_GET_TIME_STAMP_VECTOR,E],
                ['{30AC295E-1924-444C-8EDB-15C29F1B7590}']);
    end;
end;

procedure THyperObject.SetTimeStampVector (const aValue: Int64);
begin
    try
        PropAt [_hyper_time_stamp_vector].asInt64 := aValue;
    except on E: Exception do
        _raise (['SetTimeStampVector',ERR_THYPEROBJECT_SET_TIME_STAMP_VECTOR,E],
                ['{16A34558-315B-47CB-8B99-6E22A16B0F14}']);
    end;
end;

function THyperObject.GetVersion : TVersionInfo;
begin
    Result := NULL_VERSION;
    try
        Result := PropAt [_hyper_version].asVersion;
    except on E: Exception do
        _raise (['GetVersion',ERR_THYPEROBJECT_GET_VERSION,E],
                ['{7D80B9EA-40A8-4B06-9431-1655D3021BA8}']);
    end;
end;

procedure THyperObject.SetVersion (const aValue: TVersionInfo);
begin
    try
        PropAt [_hyper_version].asVersion := aValue;
    except on E: Exception do
        _raise (['SetVersion',ERR_THYPEROBJECT_SET_VERSION,E],
                ['{13EAE80E-3400-4E72-B5D7-176C6D1FBD4E}']);
    end;
end;

function THyperObject.GetKeyHash : Hex;
begin
    Result := '';
    try
        Result := PropAt [_hyper_key_hash].asHex;
    except on E: Exception do
        _raise (['GetKeyHash',ERR_THYPEROBJECT_GET_KEY_HASH,E],
                ['{59EF3567-320D-4BBF-B328-5D8AC7B60B3E}']);
    end;
end;

procedure THyperObject.SetKeyHash (const aValue: Hex);
begin
    try
        PropAt [_hyper_key_hash].asHex := aValue;
    except on E: Exception do
        _raise (['SetKeyHash',ERR_THYPEROBJECT_SET_KEY_HASH,E],
                ['{42515F48-C15F-4049-A2ED-252D910EFBF2}']);
    end;
end;

function THyperObject.GetDataHash : Hex;
begin
    Result := '';
    try
        Result := PropAt [_hyper_data_hash].asHex;
    except on E: Exception do
        _raise (['GetDataHash',ERR_THYPEROBJECT_GET_DATA_HASH,E],
                ['{76796167-1277-418D-BD7D-DE25096DCA05}']);
    end;
end;

function THyperObject.CalcKeyHash : Hex;
begin
    Result := '';
    try
        Result := '';
    except on E: Exception do
        _raise (['CalcKeyHash',ERR_THYPEROBJECT_CALC_KEY_HASH,E],
                ['{3C00B007-0E23-4CFF-9810-8281EFD8F9CB}']);
    end;
end;

function THyperObject.CalcDataHash : Hex;
begin
    Result := '';
    try
        Result := '';
    except on E: Exception do
        _raise (['CalcDataHash',ERR_THYPEROBJECT_CALC_DATA_HASH,E],
                ['{A9639125-97A9-44A9-8EB0-59C8D11E1205}']);
    end;
end;

class function THyperObject.GetHash (const aDB: TSQLiteDatabase;
                                     const anID: TID) : Hex;
var
    lst : THashList;
begin
    Result := '';
    try
        lst := CHyperObjects (GetClassList).GetHashes (aDB,[ _([ anID ]) ]);
        if Assigned (lst) then
        try
            if ( lst.Count > 0 ) then
                Result := lst.ItemAt [0].KeyHash;
        finally
            FreeAndNil (lst);
        end;
    except on E: Exception do
        _raise (['GetHash',ERR_THYPEROBJECT_GET_KEY_HASH,E],
                ['{FE8F7B7B-1896-494D-9EC7-1A748A3D23FF}']);
    end;
end;

procedure THyperObject.LoadFromStream (const aStream: TStream);
begin
    try
        if not Assigned (aStream) then
            raise Exception.Create (ERR_THYPEROBJECT_INVALID_STREAM);
        //aStream.Position := 0;
        Mode := Mode - [objImported];
        with aStream do
        begin
            Version := StrToVersionInfo (  HexToStr ( ReadStrL (aStream) )  );
            KeyHash := ReadStrL (aStream);
            PropAt [_hyper_data_hash].asHex := ReadStrL (aStream);
            IDExternal := HexToStr ( ReadStrL (aStream) );
            TimeStampCreate := _StrToDateTime (  HexToStr ( ReadStrL (aStream) )  );
            TimeStampModify := _StrToDateTime (  HexToStr ( ReadStrL (aStream) )  );
            TimeStampPublic := _StrToDateTime (  HexToStr ( ReadStrL (aStream) )  );
            TimeStampVector := HexToInt ( ReadStrL (aStream) );
        end;
        Mode := Mode + [objImported];
    except on E: Exception do
        _raise (['LoadFromStream',ERR_THYPEROBJECT_LOAD_FROM_STREAM,E],
                ['{ED8EF767-E47B-4452-B181-705DCAE989B2}']);
    end;
end;

procedure THyperObject.SaveToStream (out aStream: TStream);
begin
    try
        if not Assigned (aStream) then
            raise Exception.Create (ERR_THYPEROBJECT_INVALID_STREAM);
        aStream.Position := 0;
        Mode := Mode - [objExported];
        with aStream do
        begin
            WriteStrL (  aStream, StrToHex ( VersionInfoToStr (Version) )  );
            WriteStrL (aStream,KeyHash);
            WriteStrL (aStream,CalcDataHash);
            WriteStrL ( aStream, StrToHex (IDExternal) );
            WriteStrL (  aStream, StrToHex ( _DateTimeToStr (TimeStampCreate) )  );
            WriteStrL (  aStream, StrToHex ( _DateTimeToStr (TimeStampModify) )  );
            WriteStrL (  aStream, StrToHex ( _DateTimeToStr (TimeStampPublic) )  );
            WriteStrL ( aStream, IntToHex (TimeStampVector,8) );
        end;
        Mode := Mode + [objExported];
    except on E: Exception do
        _raise (['SaveToStream',ERR_THYPEROBJECT_SAVE_TO_STREAM,E],
                ['{843E1BC4-2E99-4BCB-975E-7755A762B4F2}']);
    end;
end;
