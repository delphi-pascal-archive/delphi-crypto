{******************************************************************************}
{*  THypoObject.imp.inc                                                       *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit & rat 2011                                       *}
{******************************************************************************}
{ THypoObject }
class function THypoObject.GetClassName : String;
begin
    Result := CLS_THYPOOBJECT_NAME;
end;

class function THypoObject.GetClassVersion : TVersionInfo;
begin
    Result := inherited GetClassVersion;
end;

class function THypoObject.GetClassTable : String;
begin
    Result := 'hypo_objects';
end;

class function THypoObject.GetClassProperties : TProtoProperties;
begin
    Result := NIL;
    try
        Result := inherited GetClassProperties;
        with Result do
        begin
            Register ( TMetaProperty, ['time_stamp_create',PRP_THYPOOBJECT_TIME_STAMP_CREATE,dtDateTime,0],[prpStored] );
            Register ( TMetaProperty, ['time_stamp_modify',PRP_THYPOOBJECT_TIME_STAMP_MODIFY,dtDateTime,0],[prpStored] );
            Register ( TMetaProperty, ['time_stamp_public',PRP_THYPOOBJECT_TIME_STAMP_PUBLIC,dtDateTime,0],[prpStored] );
            Register ( TMetaProperty, ['time_stamp_vector',PRP_THYPOOBJECT_TIME_STAMP_VECTOR,dtInt64,0],[prpStored] );
            Register ( TMetaProperty, ['version',PRP_THYPOOBJECT_VERSION,dtVersion,'0.0.0.0'],[prpStored] );
            ItemAt [_hypo_version].asVersion := GetClassVersion;
            Register ( TMetaProperty, ['key_hash',PRP_THYPOOBJECT_KEY_HASH,dtHex,''],[prpStored] );
            Register ( TMetaProperty, ['data_hash',PRP_THYPOOBJECT_DATA_HASH,dtHex,''],[prpStored] );
        end;
    except on E: Exception do
        _raise (['GetClassProperties',ERR_THYPOOBJECT_GET_ClASS_PROPERTIES,E],
                ['{6EED7FC1-B49B-4FB5-945A-0A79517C118B}']);
    end;
end;

class function THypoObject.GetClassList : CProtoObjects;
begin
    Result := THypoObjects;
end;

constructor THypoObject.Create (const aDB: TSQLiteDatabase;
                                anArgs: array of const;
                                aMode: TObjectMode = []);
begin
    try
        inherited Create (aDB,anArgs,aMode);
        Mode := Mode - [objCreated];
        { второй параметр - дата и врем€ создани€ }
        TimeStampCreate := now;
        if notEmpty (1,anArgs) then
            TimeStampCreate := toDateTime (anArgs [1]);
        { третий параметр - дата и врем€ последнего редактировани€ }
        TimeStampModify := now;
        if notEmpty (2,anArgs) then
            TimeStampModify := toDateTime (anArgs [2]);
        { четвертый параметр - дата и врем€ публикации }
        if notEmpty (3,anArgs) then
            TimeStampPublic := toDateTime (anArgs [3]);
        { п€тый параметр - верси€ }
        if notEmpty (4,anArgs) then
            Version := toVersionInfo (anArgs [4]);

        TimeStampVector := 0;

        Mode := Mode + [objCreated];
    except on E: Exception do begin
        Mode := Mode + [objCreateFailed];
        _raise (['Create',ERR_THYPOOBJECT_CREATE,E],
                ['{8011A3C3-7A81-4267-9C61-083F67964938}']);
    end; end;
end;

destructor THypoObject.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_THYPOOBJECT_DESTROY,E],
                ['{CD0DDE1D-D647-4426-986E-58C63BE9F93A}']);
    end;
end;

class procedure THypoObject.CreateClassTable (const aDB: TSQLiteDatabase);
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
                Prop := TMetaProperty (Props.ItemAt [_hypo_key_hash]);
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
                Prop := TMetaProperty (Props.ItemAt [_hypo_data_hash]);
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
        _raise (['CreateClassTable',ERR_THYPOOBJECT_CREATE_CLASS_TABLE,E],
                ['{8A2F6A05-C7BD-4B4D-A43D-92EBEA66E2E1}']);
    end;
end;

function THypoObject.Check : Boolean;
begin
    Result := TRUE;
    try
        Result := inherited Check;
    except on E: Exception do
        _raise (['Check',ERR_THYPOOBJECT_CHECK,E],
                ['{6BCBC120-270D-4804-96ED-3E3E4F81B10D}']);
    end;
end;

procedure THypoObject.Save;
var
    CalculatedDataHash : String;
begin
    try
        if objNoSave in Mode then Exit;
        Mode := Mode - [objSaveFailed];
        Mode := Mode - [objSaved];
        if isEmpty ( PropAt [_hypo_key_hash].asHex ) then
            PropAt [_hypo_key_hash].asHex := CalcKeyHash;
        CalculatedDataHash := CalcDataHash;
        if ( PropAt [_hypo_data_hash].asHex <> CalculatedDataHash ) then
        begin
            PropAt [_hypo_data_hash].asHex := CalculatedDataHash;
            TimeStampVector := TimeStampVector + 1;
            TimeStampModify := now;
        end;
        inherited Save;
    except on E: Exception do begin
        Mode := Mode + [objSaveFailed];
        _raise (['Save',ERR_THYPOOBJECT_SAVE,E],
                ['{6F3E2553-A7BB-43AA-8378-269E46B28488}']);
    end; end;
end;

function THypoObject.GetTimeStampCreate : TDateTime;
begin
    Result := 0.0;
    try
        Result := PropAt [_hypo_time_stamp_create].asDateTime;
    except on E: Exception do
        _raise (['GetTimeStampCreate',ERR_THYPOOBJECT_GET_TIME_STAMP_CREATE,E],
                ['{1CDCB5BD-10D9-4928-A5EA-06735E02932E}']);
    end;
end;

procedure THypoObject.SetTimeStampCreate (const aValue: TDateTime);
begin
    try
        PropAt [_hypo_time_stamp_create].asDateTime := aValue;
    except on E: Exception do
        _raise (['SetTimeStampCreate',ERR_THYPOOBJECT_SET_TIME_STAMP_CREATE,E],
                ['{FA5E3F81-54AE-43D0-97EE-F0D4A51D3AF9}']);
    end;
end;

function THypoObject.GetTimeStampModify : TDateTime;
begin
    Result := 0.0;
    try
        Result := PropAt [_hypo_time_stamp_modify].asDateTime;
    except on E: Exception do
        _raise (['GetTimeStampModify',ERR_THYPOOBJECT_GET_TIME_STAMP_MODIFY,E],
                ['{171838BE-29AE-4BE8-A5E4-1651CB62D492}']);
    end;
end;

procedure THypoObject.SetTimeStampModify (const aValue: TDateTime);
begin
    try
        PropAt [_hypo_time_stamp_modify].asDateTime := aValue;
    except on E: Exception do
        _raise (['SetTimeStampModify',ERR_THYPOOBJECT_SET_TIME_STAMP_MODIFY,E],
                ['{7B756DF2-A701-4C29-AD43-783B01DC3B81}']);
    end;
end;

function THypoObject.GetTimeStampPublic : TDateTime;
begin
    Result := 0.0;
    try
        Result := PropAt [_hypo_time_stamp_public].asDateTime;
    except on E: Exception do
        _raise (['GetTimeStampPublic',ERR_THYPOOBJECT_GET_TIME_STAMP_PUBLIC,E],
                ['{02F757C2-E8F8-4004-8395-C64DF7440D1F}']);
    end;
end;

procedure THypoObject.SetTimeStampPublic (const aValue: TDateTime);
begin
    try
        PropAt [_hypo_time_stamp_public].asDateTime := aValue;
    except on E: Exception do
        _raise (['SetTimeStampPublic',ERR_THYPOOBJECT_SET_TIME_STAMP_PUBLIC,E],
                ['{E0B67039-5C19-470F-BBA8-F91EE0AB33FF}']);
    end;
end;

function THypoObject.GetTimeStampVector : Int64;
begin
    Result := 0;
    try
        Result := PropAt [_hypo_time_stamp_vector].asInt64;
    except on E: Exception do
        _raise (['GetTimeStampVector',ERR_THYPOOBJECT_GET_TIME_STAMP_VECTOR,E],
                ['{E8B7332F-0E8B-4593-8D30-D0565FC49902}']);
    end;
end;

procedure THypoObject.SetTimeStampVector (const aValue: Int64);
begin
    try
        PropAt [_hypo_time_stamp_vector].asInt64 := aValue;
    except on E: Exception do
        _raise (['SetTimeStampVector',ERR_THYPOOBJECT_SET_TIME_STAMP_VECTOR,E],
                ['{A51452E7-DC63-45FA-99A5-6A8F0F29075B}']);
    end;
end;

function THypoObject.GetVersion : TVersionInfo;
begin
    Result := NULL_VERSION;
    try
        Result := PropAt [_hypo_version].asVersion;
    except on E: Exception do
        _raise (['GetVersion',ERR_THYPOOBJECT_GET_VERSION,E],
                ['{7A24EFE3-D39A-428C-B203-A0A42DA80609}']);
    end;
end;

procedure THypoObject.SetVersion (const aValue: TVersionInfo);
begin
    try
        PropAt [_hypo_version].asVersion := aValue;
    except on E: Exception do
        _raise (['SetVersion',ERR_THYPOOBJECT_SET_VERSION,E],
                ['{30A1A807-0C84-48E8-BE09-54CEFD478AC4}']);
    end;
end;

function THypoObject.GetKeyHash : Hex;
begin
    Result := '';
    try
        Result := PropAt [_hypo_key_hash].asHex;
    except on E: Exception do
        _raise (['GetKeyHash',ERR_THYPOOBJECT_GET_KEY_HASH,E],
                ['{16FD4BED-988B-4755-84A5-F4EAA4F8D0FB}']);
    end;
end;

procedure THypoObject.SetKeyHash (const aValue: Hex);
begin
    try
        PropAt [_hypo_key_hash].asHex := aValue;
    except on E: Exception do
        _raise (['SetKeyHash',ERR_THYPOOBJECT_SET_KEY_HASH,E],
                ['{98AFCC2A-2B3D-42CB-BA58-F21F369E5431}']);
    end;
end;

function THypoObject.GetDataHash : Hex;
begin
    Result := '';
    try
        Result := PropAt [_hypo_data_hash].asHex;
    except on E: Exception do
        _raise (['GetDataHash',ERR_THYPOOBJECT_GET_DATA_HASH,E],
                ['{726727B5-DE1F-4CCF-8D41-BC98C1FC3A91}']);
    end;
end;

function THypoObject.CalcKeyHash : Hex;
begin
    Result := '';
    try
        Result := '';
    except on E: Exception do
        _raise (['CalcKeyHash',ERR_THYPOOBJECT_CALC_KEY_HASH,E],
                ['{D925B8F3-C9BE-4BED-B8D1-A36A7D9BD0E6}']);
    end;
end;

function THypoObject.CalcDataHash : Hex;
begin
    Result := '';
    try
        Result := '';
    except on E: Exception do
        _raise (['CalcDataHash',ERR_THYPOOBJECT_CALC_DATA_HASH,E],
                ['{C811D0A5-40F6-40D7-9383-C9C7DEE0BC3E}']);
    end;
end;

class function THypoObject.GetHash (const aDB: TSQLiteDatabase;
                                    const anID: TID) : Hex;
var
    lst : THashList;
begin
    Result := '';
    try
        lst := CHypoObjects (GetClassList).GetHashes (aDB,[ _([ anID ]) ]);
        if Assigned (lst) then
        try
            if ( lst.Count > 0 ) then
                Result := lst.ItemAt [0].KeyHash;
        finally
            FreeAndNil (lst);
        end;
    except on E: Exception do
        _raise (['GetHash',ERR_THYPOOBJECT_GET_KEY_HASH,E],
                ['{BD185A6E-C510-4E3E-8966-315CAFBCA4CC}']);
    end;
end;

procedure THypoObject.LoadFromStream (const aStream: TStream);
begin
    try
        if not Assigned (aStream) then
            raise Exception.Create (ERR_THYPOOBJECT_INVALID_STREAM);
        aStream.Position := 0;
        Mode := Mode - [objImported];
        with aStream do
        begin
            Version := StrToVersionInfo (  HexToStr ( ReadStrL (aStream) )  );
            KeyHash := ReadStrL (aStream);
            PropAt [_hypo_data_hash].asHex := ReadStrL (aStream);
            TimeStampCreate := _StrToDateTime (  HexToStr ( ReadStrL (aStream) )  );
            TimeStampModify := _StrToDateTime (  HexToStr ( ReadStrL (aStream) )  );
            TimeStampPublic := _StrToDateTime (  HexToStr ( ReadStrL (aStream) )  );
            TimeStampVector := HexToInt ( ReadStrL (aStream) );
        end;
        Mode := Mode + [objImported];
    except on E: Exception do
        _raise (['LoadFromStream',ERR_THYPOOBJECT_LOAD_FROM_STREAM,E],
                ['{0D5A22CB-9C9F-43A4-8AB3-F518C20CB625}']);
    end;
end;

procedure THypoObject.SaveToStream (out aStream: TStream);
begin
    try
        if not Assigned (aStream) then
            raise Exception.Create (ERR_THYPOOBJECT_INVALID_STREAM);
        aStream.Position := 0;
        Mode := Mode - [objExported];
        with aStream do
        begin
            WriteStrL (  aStream, StrToHex ( VersionInfoToStr (Version) )  );
            WriteStrL (aStream,KeyHash);
            WriteStrL (aStream,CalcDataHash);
            WriteStrL (  aStream, StrToHex ( _DateTimeToStr (TimeStampCreate) )  );
            WriteStrL (  aStream, StrToHex ( _DateTimeToStr (TimeStampModify) )  );
            WriteStrL (  aStream, StrToHex ( _DateTimeToStr (TimeStampPublic) )  );
            WriteStrL ( aStream, IntToHex (TimeStampVector,8) );
        end;
        Mode := Mode + [objExported];
    except on E: Exception do
        _raise (['SaveToStream',ERR_THYPOOBJECT_SAVE_TO_STREAM,E],
                ['{86EC113A-ED73-42F5-834B-F03FFA43B1F5}']);
    end;
end;
