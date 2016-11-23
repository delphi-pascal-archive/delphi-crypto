{******************************************************************************}
{*  TPackage.imp.inc                                                          *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011-2012                                        *}
{******************************************************************************}
{ TPackage }
class function TPackage.GetClassName : String;
begin
    Result := CLS_TPACKAGE_NAME;
end;

class function TPackage.GetClassVersion : TVersionInfo;
begin
    Result := inherited GetClassVersion;
end;

class function TPackage.GetClassTable : String;
begin
    Result := 'packages';
end;

class function TPackage.GetClassProperties : TProtoProperties;
begin
    Result := NIL;
    try
        Result := inherited GetClassProperties;
        with Result do
        begin
            Register ( TMetaProperty, ['id_sender',PRP_TPACKAGE_ID_SENDER,dtID,0],[prpStored] );
            Register ( TMetaProperty, ['sender_hash',PRP_TPACKAGE_SENDER_HASH,dtHex,''],[prpStored] );
            Register ( TMetaProperty, ['id_receiver',PRP_TPACKAGE_ID_SENDER,dtID,0],[prpStored] );
            Register ( TMetaProperty, ['receiver_hash',PRP_TPACKAGE_RECEIVER_HASH,dtHex,''],[prpStored] );
            Register ( TMetaProperty, ['time_stamp_create',PRP_TPACKAGE_TIME_STAMP_CREATE,dtDateTime,0],[prpStored] );
            Register ( TMetaProperty, ['time_stamp_send',PRP_TPACKAGE_TIME_STAMP_SEND,dtDateTime,0],[prpStored] );
            Register ( TMetaProperty, ['time_stamp_receive',PRP_TPACKAGE_TIME_STAMP_RECEIVE,dtDateTime,0],[prpStored] );
            Register ( TMetaProperty, ['version',PRP_TPACKAGE_VERSION,dtVersion,'0.0.0.0'],[prpStored] );
            ItemAt [_pck_version].asVersion := GetClassVersion;
            Register ( TMetaProperty, ['key_hash',PRP_TPACKAGE_KEY_HASH,dtHex,''],[prpStored] );
            Register ( TMetaProperty, ['data_hash',PRP_TPACKAGE_DATA_HASH,dtHex,''],[prpStored] );
            Register ( TMetaProperty, ['id_owner',PRP_TPACKAGE_ID_OWNER,dtID,0],[prpStored] );
            Register ( TMetaProperty, ['id_type',PRP_TPACKAGE_ID_TYPE,dtID,0],[prpStored] );
            Register ( TMetaProperty, ['id_status',PRP_TPACKAGE_ID_STATUS,dtID,0],[prpStored] );
            Register ( TMetaProperty, ['meta_class_id',PRP_TPACKAGE_META_CLASS_ID,dtString,''],[prpStored] );
            Register ( TMetaProperty, ['meta_data',PRP_TPACKAGE_META_DATA,dtStream,''],[prpStored] );
        end;
    except on E: Exception do
        _raise (['GetClassProperties',ERR_TPACKAGE_GET_ClASS_PROPERTIES,E],
                ['{B21268BD-F3B8-441B-AE62-68B8573DFDCC}']);
    end;
end;

class function TPackage.GetClassList : CProtoObjects;
begin
    Result := TPackages;
end;

constructor TPackage.Create (const aDB: TSQLiteDatabase;
                             anArgs: array of const;
                             aMode: TObjectMode = []);
begin
    try
        inherited Create (aDB,[],aMode);
        Mode := Mode - [objCreated];
        { первый параметр - идентификатор }
        if notEmpty (0,anArgs) then
            ID := toInt64 (anArgs [0]);
        { второй параметр - идентификатор отправителя }
        f_Sender := TUser.Create (DB,[]);
        if notEmpty (1,anArgs) then
            IDSender := toInt64 (anArgs [1]);
        { третий параметр - идентификатор получателя }
        f_Receiver := TUser.Create (DB,[]);
        if notEmpty (2,anArgs) then
            IDReceiver := toInt64 (anArgs [2]);
        { четвертый параметр - идентификатор владельца }
        if notEmpty (3,anArgs) then
            IDOwner := toInt64 (anArgs [3]);
        if Assigned (User) and ( IDOwner = 0 ) then
            IDOwner := USER_ID;
        { пятый параметр - идентификатор типа }
        f_PckType := TPackageType.Create (aDB,[],aMode);
        if notEmpty (4,anArgs) then
            IDType := toInt64 (anArgs [4]);
        { шестой параметр - идентификатор статуса }
        f_PckStatus := TPackageStatus.Create (aDB,[],aMode);
        IDStatus := PACKAGE_CREATED_STATUS_ID;
        if notEmpty (5,anArgs) then
            IDStatus := toInt64 (anArgs [5]);
        { седьмой параметр - мета-класс или наименование мета-класса }
        f_MetaClass := NIL;
        if notEmpty (6,anArgs) then
        begin
            MetaClass := CMetaObject ( toClass (anArgs [6]) );
            if not Assigned (MetaClass) then
                MetaClassID := toString (anArgs [6]);
        end;
        { восьмой параметр - версия }
        if notEmpty (7,anArgs) then
            Version := toVersionInfo (anArgs [7]);
        Mode := Mode + [objCreated];
    except on E: Exception do begin
        Mode := Mode + [objCreateFailed];
        _raise (['Create',ERR_TPACKAGE_CREATE,E],
                ['{AD383B5F-A863-4141-AFE6-6BC87F04B7E0}']);
    end; end;
end;

destructor TPackage.Destroy;
begin
    try
        if ( Sender <> User ) then
            FreeAndNil (f_Sender);
        if ( Receiver <> User ) then
            FreeAndNil (f_Receiver);
        FreeAndNil (f_PckType);
        FreeAndNil (f_PckStatus);
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TPACKAGE_DESTROY,E],
                ['{FD599445-576F-4A14-9462-E34D4F1CA01F}']);
    end;
end;

class procedure TPackage.CreateClassTable (const aDB: TSQLiteDatabase);
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
                Prop := TMetaProperty (Props.ItemAt [_pck_key_hash]);
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
                Prop := TMetaProperty (Props.ItemAt [_pck_data_hash]);
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
        _raise (['CreateClassTable',ERR_TPACKAGE_CREATE_CLASS_TABLE,E],
                ['{77E5B881-2BA2-4534-B071-45E1C76F9B40}']);
    end;
end;

procedure TPackage.SetDB (const aDB: TSQLiteDatabase);
begin
    try
        inherited SetDB (aDB);
        if Assigned (f_Sender) then
            f_Sender.DB := aDB;
        if Assigned (f_Receiver) then
            f_Receiver.DB := aDB;
        if Assigned (f_PckType) then
            f_PckType.DB := aDB;
        if Assigned (f_PckStatus) then
            f_PckStatus.DB := aDB;
    except on E: Exception do
        _raise (['SetDB',ERR_TPACKAGE_SET_DB,E],
                ['{015EAE12-B15E-4D49-A35E-DFFE41CBDA03}']);
    end;
end;

function TPackage.Check : Boolean;
begin
    Result := TRUE;
    try
        Result := inherited Check;
        if Result then
        begin
            if not ( Length (SenderHash) > 0 ) then
                Error (ERR_TPACKAGE_INVALID_SENDER_HASH);
            if not ( Length (ReceiverHash) > 0 ) then
                Error (ERR_TPACKAGE_INVALID_RECEIVER_HASH);
            if not ( IDType > 0 ) then
                Error (ERR_TPACKAGE_INVALID_ID_TYPE);
            if not ( IDStatus > 0 ) then
                Error (ERR_TPACKAGE_INVALID_ID_STATUS);
            if not Assigned (MetaClass) then
                Error (ERR_TPACKAGE_INVALID_META_CLASS);
            Result := ( Errors.Count = 0 );
        end;
    except on E: Exception do
        _raise (['Check',ERR_TPACKAGE_CHECK,E],
                ['{EADD0213-7B3B-4BE3-BAEE-44B0C3E65E55}']);
    end;
end;

procedure TPackage.Load;
begin
    try
        if objNoLoad in Mode then Exit;
        Mode := Mode - [objLoadFailed];
        Mode := Mode - [objLoaded];
        inherited Load;
        Mode := Mode - [objLoaded];
        { sender }
        if Assigned (Sender) then
        begin
            Sender.ID := PropAt [_pck_id_sender].asID;
            if ( IDSender > 0 ) then
                Sender.Load;
        end
        else if ( PropAt [_pck_id_sender].asID > 0 ) then
            Sender := TUser.Load (DB,PropAt [_pck_id_sender].asID) as TUser
        else if ( Length (PropAt [_pck_sender_hash].asHex) > 0 ) then
            Sender := TUser.Load (  DB, TUsers.Find ( DB, PropAt [_pck_sender_hash].asHex, PropAt [_pck_id_owner].asInt64 )  ) as TUser
        else
            Sender := TUser.Create (DB,[]);
        { receiver }
        if Assigned (Receiver) then
        begin
            Receiver.ID := PropAt [_pck_id_receiver].asID;
            if ( IDReceiver > 0 ) then
                Receiver.Load;
        end
        else if ( PropAt [_pck_id_receiver].asID > 0 ) then
            Receiver := TUser.Load (DB,PropAt [_pck_id_receiver].asID) as TUser
        else if ( Length (PropAt [_pck_receiver_hash].asHex) > 0 ) then
            Receiver := TUser.Load (  DB, TUsers.Find ( DB, PropAt [_pck_receiver_hash].asHex, PropAt [_pck_id_owner].asInt64 )  ) as TUser
        else
            Receiver := TUser.Create (DB,[]);
        { type }
        if Assigned (PckType) then
        begin
            PckType.ID := PropAt [_pck_id_type].asID;
            if ( IDType > 0 ) then
                PckType.Load;
        end
        else if ( PropAt [_pck_id_type].asID > 0 ) then
            PckType := TPackageType.Load (DB,PropAt [_pck_id_type].asID) as TPackageType
        else
            PckType := TPackageType.Create (DB,[]);
        { status }
        if Assigned (PckStatus) then
        begin
            PckStatus.ID := PropAt [_pck_id_status].asID;
            if ( IDStatus > 0 ) then
                PckStatus.Load;
        end
        else if ( PropAt [_pck_id_status].asID > 0 ) then
            PckStatus := TPackageStatus.Load (DB,PropAt [_pck_id_status].asID) as TPackageStatus
        else
            PckStatus := TPackageStatus.Create (DB,[]);
        { meta class }
        MetaClassID := PropAt [_pck_meta_class_id].asString;
        Mode := Mode + [objLoaded];
    except on E: Exception do begin
        Mode := Mode + [objLoadFailed];
        _raise (['Load',ERR_TPACKAGE_LOAD,E],
                ['{BA655A99-58D5-4F21-ABFD-ADE5E680E5AD}']);
    end; end;
end;

procedure TPackage.Save;
var
    CalculatedDataHash : Hex;
begin
    try
        if objNoSave in Mode then Exit;
        Mode := Mode - [objSaveFailed];
        Mode := Mode - [objSaved];
        if isEmpty ( PropAt [_pck_key_hash].asHex ) then
            PropAt [_pck_key_hash].asHex := CalcKeyHash;
        CalculatedDataHash := CalcDataHash;
        if ( PropAt [_pck_data_hash].asHex <> CalculatedDataHash ) then
            PropAt [_pck_data_hash].asHex := CalculatedDataHash;
        inherited Save;
    except on E: Exception do begin
        Mode := Mode + [objSaveFailed];
        _raise (['Save',ERR_TPACKAGE_SAVE,E],
                ['{369B603E-73D1-46FD-86EA-5B4C4B93428B}']);
    end; end;
end;

function TPackage.GetIDSender : TID;
begin
    Result := 0;
    try
        Result := PropAt [_pck_id_sender].asID;
    except on E: Exception do
        _raise (['GetIDSender',ERR_TPACKAGE_GET_ID_SENDER,E],
                ['{D0EBC42A-3FA9-4DED-AF80-3BB4D2A5024C}']);
    end;
end;

procedure TPackage.SetIDSender (const aValue: TID);
begin
    try
        if ( aValue >= 0 ) then
            PropAt [_pck_id_sender].asID := aValue
        else
            PropAt [_pck_id_sender].asID := 0;
        if Assigned (Sender) and ( Sender.ID <> IDSender ) then
            Sender.ID := IDSender
        else if not Assigned (Sender) then
            Sender := TUser.Create (DB,[IDSender]);
    except on E: Exception do
        _raise (['SetIDSender',ERR_TPACKAGE_SET_ID_SENDER,E],
                ['{3A6A7F0D-E859-4EAC-BF48-EC470B515EB0}']);
    end;
end;

function TPackage.GetSender : TUser;
begin
    Result := NIL;
    try
        Result := f_Sender;
    except on E: Exception do
        _raise (['GetSender',ERR_TPACKAGE_GET_SENDER,E],
                ['{82B4C38D-E0AD-4603-9AFE-746C85F389C5}']);
    end;
end;

procedure TPackage.SetSender (const aValue: TUser);
begin
    try
        if Assigned (Sender) and ( Sender <> aValue ) and ( Sender <> User ) then
            FreeAndNil (f_Sender);
        f_Sender := aValue;
        if Assigned (Sender) and ( Sender.ID <> IDSender ) then
        begin
            IDSender := Sender.ID;
            SenderHash := Sender.KeyHash;
        end;
    except on E: Exception do
        _raise (['SetSender',ERR_TPACKAGE_SET_SENDER,E],
                ['{A56AFDFD-6FD0-478E-8ABD-86CB32902EFC}']);
    end;
end;

function TPackage.GetSenderHash : Hex;
begin
    Result := '';
    try
        Result := PropAt [_pck_sender_hash].asHex;
    except on E: Exception do
        _raise (['GetSenderHash',ERR_TPACKAGE_GET_SENDER_HASH,E],
                ['{4BC69FF0-59E7-4606-8E70-6173DC1BBE19}']);
    end;
end;

procedure TPackage.SetSenderHash (const aValue: Hex);
begin
    try
        if isHex (aValue) then
            PropAt [_pck_sender_hash].asHex := aValue
        else
            PropAt [_pck_sender_hash].asHex := '';
        if ( aValue <> USER_KEY_HASH ) then
            IDSender := TUsers.Find (DB,aValue,IDOwner);
    except on E: Exception do
        _raise (['SetSenderHash',ERR_TPACKAGE_SET_SENDER_HASH,E],
                ['{40CD5921-40F4-467A-9D02-1072FA08CB02}']);
    end;
end;

function TPackage.GetIDReceiver : TID;
begin
    Result := 0;
    try
        Result := PropAt [_pck_id_receiver].asID;
    except on E: Exception do
        _raise (['GetIDReceiver',ERR_TPACKAGE_GET_ID_RECEIVER,E],
                ['{8A1A6B55-88D5-425D-B78F-3C3AC04E5967}']);
    end;
end;

procedure TPackage.SetIDReceiver (const aValue: TID);
begin
    try
        if ( aValue >= 0 ) then
            PropAt [_pck_id_receiver].asID := aValue
        else
            PropAt [_pck_id_receiver].asID := 0;
        if Assigned (Receiver) and ( Receiver.ID <> IDReceiver ) then
            Receiver.ID := IDReceiver
        else if not Assigned (Receiver) then
            Receiver := TUser.Create (DB,[IDReceiver]);
    except on E: Exception do
        _raise (['SetIDReceiver',ERR_TPACKAGE_SET_ID_RECEIVER,E],
                ['{8104C8D2-2E86-4457-9B25-3A096D67D61E}']);
    end;
end;

function TPackage.GetReceiver : TUser;
begin
    Result := NIL;
    try
        Result := f_Receiver;
    except on E: Exception do
        _raise (['GetReceiver',ERR_TPACKAGE_GET_RECEIVER,E],
                ['{A118473C-83B4-4EDC-8C97-FD4E679A6231}']);
    end;
end;

procedure TPackage.SetReceiver (const aValue: TUser);
begin
    try
        if Assigned (Receiver) and ( Receiver <> aValue ) and ( Receiver <> User ) then
            FreeAndNil (f_Receiver);
        f_Receiver := aValue;
        if Assigned (Receiver) and ( Receiver.ID <> IDReceiver ) then
        begin
            IDReceiver := Receiver.ID;
            ReceiverHash := Receiver.KeyHash;
        end;
    except on E: Exception do
        _raise (['SetReceiver',ERR_TPACKAGE_SET_RECEIVER,E],
                ['{80C80633-057B-4F4C-8E5C-5B6A84497215}']);
    end;
end;

function TPackage.GetReceiverHash : Hex;
begin
    Result := '';
    try
        Result := PropAt [_pck_receiver_hash].asHex;
    except on E: Exception do
        _raise (['GetReceiverHash',ERR_TPACKAGE_GET_RECEIVER_HASH,E],
                ['{3B299826-A8BB-4616-9FE8-B8ACBE9CB5B3}']);
    end;
end;

procedure TPackage.SetReceiverHash (const aValue: Hex);
begin
    try
        if isHex (aValue) then
            PropAt [_pck_receiver_hash].asHex := aValue
        else
            PropAt [_pck_receiver_hash].asHex := '';
        if ( aValue <> USER_KEY_HASH ) then
            IDReceiver := TUsers.Find (DB,aValue,IDOwner);
    except on E: Exception do
        _raise (['SetReceiverHash',ERR_TPACKAGE_SET_RECEIVER_HASH,E],
                ['{50136AF0-A916-4A2C-B2B9-B502E6494C8B}']);
    end;
end;

function TPackage.GetTimeStampCreate : TDateTime;
begin
    Result := 0.0;
    try
        Result := PropAt [_pck_time_stamp_create].asDateTime;
    except on E: Exception do
        _raise (['GetTimeStampCreate',ERR_TPACKAGE_GET_TIME_STAMP_CREATE,E],
                ['{ACA80E73-8CB0-4955-8E13-E0126755D9AF}']);
    end;
end;

procedure TPackage.SetTimeStampCreate (const aValue: TDateTime);
begin
    try
        PropAt [_pck_time_stamp_create].asDateTime := aValue;
    except on E: Exception do
        _raise (['SetTimeStampCreate',ERR_TPACKAGE_SET_TIME_STAMP_CREATE,E],
                ['{8D6188CB-6F97-4380-9A5A-399CEE2C0B2A}']);
    end;
end;

function TPackage.GetTimeStampSend : TDateTime;
begin
    Result := 0.0;
    try
        Result := PropAt [_pck_time_stamp_send].asDateTime;
    except on E: Exception do
        _raise (['GetTimeStampSend',ERR_TPACKAGE_GET_TIME_STAMP_SEND,E],
                ['{9EA8ED40-923E-44B2-963D-501C59E3515B}']);
    end;
end;

procedure TPackage.SetTimeStampSend (const aValue: TDateTime);
begin
    try
        PropAt [_pck_time_stamp_send].asDateTime := aValue;
    except on E: Exception do
        _raise (['SetTimeStampSend',ERR_TPACKAGE_SET_TIME_STAMP_SEND,E],
                ['{D33B39E2-2C7F-49FC-9A5A-10A179AF29C5}']);
    end;
end;

function TPackage.GetTimeStampReceive : TDateTime;
begin
    Result := 0.0;
    try
        Result := PropAt [_pck_time_stamp_receive].asDateTime;
    except on E: Exception do
        _raise (['GetTimeStampReceive',ERR_TPACKAGE_GET_TIME_STAMP_RECEIVE,E],
                ['{BF2E3E98-C423-4662-9BFE-D173F76B465C}']);
    end;
end;

procedure TPackage.SetTimeStampReceive (const aValue: TDateTime);
begin
    try
        PropAt [_pck_time_stamp_receive].asDateTime := aValue;
    except on E: Exception do
        _raise (['SetTimeStampReceive',ERR_TPACKAGE_SET_TIME_STAMP_RECEIVE,E],
                ['{85F9A61C-325E-4A18-AD33-F96F7E66DE87}']);
    end;
end;

function TPackage.GetIDOwner : TID;
begin
    Result := 0;
    try
        Result := PropAt [_pck_id_owner].asID;
    except on E: Exception do
        _raise (['GetIDOwner',ERR_TPACKAGE_GET_ID_OWNER,E],
                ['{1C325BB7-B3F2-43BC-801E-EDA2426B24CF}']);
    end;
end;

procedure TPackage.SetIDOwner (const aValue: TID);
begin
    try
        if ( aValue >= 0 ) then
            PropAt [_pck_id_owner].asID := aValue
        else
            PropAt [_pck_id_owner].asID := 0;
    except on E: Exception do
        _raise (['SetIDOwner',ERR_TPACKAGE_SET_ID_OWNER,E],
                ['{A33FA33E-85F8-442F-9B7E-D90CC22EF093}']);
    end;
end;

function TPackage.GetIDType : TID;
begin
    Result := 0;
    try
        Result := PropAt [_pck_id_type].asID;
    except on E: Exception do
        _raise (['GetIDType',ERR_TPACKAGE_GET_ID_TYPE,E],
                ['{22ADAF18-3682-4238-8D52-1D339BCBE2EF}']);
    end;
end;

procedure TPackage.SetIDType (const aValue: TID);
begin
    try
        if ( aValue >= 0 ) then
            PropAt [_pck_id_type].asID := aValue
        else
            PropAt [_pck_id_type].asID := 0;
        if Assigned (PckType) and ( PckType.ID <> IDType ) then
            PckType.ID := IDType
        else if not Assigned (PckType) then
            PckType := TPackageType.Create (DB,[IDType]);
    except on E: Exception do
        _raise (['SetIDType',ERR_TPACKAGE_SET_ID_TYPE,E],
                ['{D5D4E25F-DA77-4872-B7F5-6050B425A225}']);
    end;
end;

function TPackage.GetPckType : TPackageType;
begin
    Result := NIL;
    try
        Result := f_PckType;
    except on E: Exception do
        _raise (['GetPckType',ERR_TPACKAGE_GET_TYPE,E],
                ['{38053043-856F-4245-94E8-99C2304A0A82}']);
    end;
end;

procedure TPackage.SetPckType (const aValue: TPackageType);
begin
    try
        f_PckType := aValue;
        if Assigned (PckType) and ( PckType.ID <> IDType ) then
            IDType := PckType.ID;
    except on E: Exception do
        _raise (['SetPckType',ERR_TPACKAGE_SET_TYPE,E],
                ['{0E7F0B88-D226-4AE2-9ACA-2086BF153408}']);
    end;
end;

function TPackage.GetIDStatus : TID;
begin
    Result := 0;
    try
        Result := PropAt [_pck_id_status].asID;
    except on E: Exception do
        _raise (['GetIDStatus',ERR_TPACKAGE_GET_ID_STATUS,E],
                ['{42A47EA8-84BD-41C1-807C-E952E7ADAA69}']);
    end;
end;

procedure TPackage.SetIDStatus (const aValue: TID);
begin
    try
        if ( aValue >= 0 ) then
            PropAt [_pck_id_status].asID := aValue
        else
            PropAt [_pck_id_status].asID := 0;
        if Assigned (PckStatus) and ( PckStatus.ID <> IDStatus ) then
            PckStatus.ID := IDStatus
        else if not Assigned (PckStatus) then
            PckStatus := TPackageStatus.Create (DB,[IDStatus]);
    except on E: Exception do
        _raise (['SetIDStatus',ERR_TPACKAGE_SET_ID_STATUS,E],
                ['{6D5DCDEA-9E33-4256-AFC0-AA827B85261D}']);
    end;
end;

function TPackage.GetPckStatus : TPackageStatus;
begin
    Result := NIL;
    try
        Result := f_PckStatus;
    except on E: Exception do
        _raise (['GetPckStatus',ERR_TPACKAGE_GET_STATUS,E],
                ['{16377407-3A5F-4CE5-A0D4-89E5D47ECEED}']);
    end;
end;

procedure TPackage.SetPckStatus (const aValue: TPackageStatus);
begin
    try
        f_PckStatus := aValue;
        if Assigned (PckStatus) and ( PckStatus.ID <> IDStatus ) then
            IDStatus := PckStatus.ID;
    except on E: Exception do
        _raise (['SetPckStatus',ERR_TPACKAGE_SET_STATUS,E],
                ['{48884FEB-E48E-4F3C-9AEA-F0EC971DB758}']);
    end;
end;

class function TPackage.GetMetaClassByID (const aMetaClassID: String) : CMetaObject;
begin
    Result := NIL;
    try
        if notEmpty (aMetaClassID) then
        begin
            if ( aMetaClassID = TUser.GetClassID ) then
                Result := TUser
            else if ( aMetaClassID = TPic.GetClassID ) then
                Result := TPic
            else if ( aMetaClassID = TCategorie.GetClassID ) then
                Result := TCategorie
            else if ( aMetaClassID = TMessage.GetClassID ) then
                Result := TMessage
            else if ( aMetaClassID = TMetaObject.GetClassID ) then
                Result := TMetaObject
        end;
    except on E: Exception do
        _raise (['GetMetaClassByID',ERR_TPACKAGE_GET_META_CLASS_BY_ID,E],
                ['{2F34B7DF-2E4D-46FD-B382-3E5EE51A18CC}']);
    end;
end;

function TPackage.GetMetaClassID : String;
begin
    Result := '';
    try
        Result := PropAt [_pck_meta_class_id].asString;
    except on E: Exception do
        _raise (['GetMetaClassID',ERR_TPACKAGE_GET_META_CLASS_ID,E],
                ['{F8085079-EA9C-4334-9083-14B4B8CCE8B7}']);
    end;
end;

procedure TPackage.SetMetaClassID (const aValue: String);
begin
    try
        f_MetaClass := GetMetaClassByID (aValue);
        if Assigned (MetaClass) then
            PropAt [_pck_meta_class_id].asString := MetaClass.GetClassID
        else
            PropAt [_pck_meta_class_id].asString := '';
    except on E: Exception do
        _raise (['SetMetaClassID',ERR_TPACKAGE_SET_META_CLASS_ID,E],
                ['{AB80B03D-4508-4A1A-8B84-54E37DAE0D69}']);
    end;
end;

function TPackage.GetMetaClass : CMetaObject;
begin
    Result := NIL;
    try
        Result := f_MetaClass;
    except on E: Exception do
        _raise (['GetMetaClass',ERR_TPACKAGE_GET_META_CLASS,E],
                ['{2147A6FD-CD40-4E31-A18F-FE47FAB2388F}']);
    end;
end;

procedure TPackage.SetMetaClass (const aValue: CMetaObject);
begin
    try
        f_MetaClass := aValue;
        if Assigned (MetaClass) then
            PropAt [_pck_meta_class_id].asString := MetaClass.GetClassID
        else
            PropAt [_pck_meta_class_id].asString := '';
    except on E: Exception do
        _raise (['SetMetaClass',ERR_TPACKAGE_SET_META_CLASS,E],
                ['{B6BA061F-A874-4D66-B50A-6BE53747B837}']);
    end;
end;

function TPackage.GetMetaData : TStream;
begin
    Result := NIL;
    try
        Result := PropAt [_pck_meta_data].asStream;
    except on E: Exception do
        _raise (['GetMetaData',ERR_TPACKAGE_GET_META_DATA,E],
                ['{4F58F655-65D1-42A5-8B45-81E8A68B5944}']);
    end;
end;

procedure TPackage.SetMetaData (const aValue: TStream);
begin
    try
        PropAt [_pck_meta_data].asStream := aValue;
    except on E: Exception do
        _raise (['SetMetaData',ERR_TPACKAGE_SET_META_DATA,E],
                ['{3B88AC88-F75A-4FC7-AA0E-DE90920CC362}']);
    end;
end;

function TPackage.GetVersion : TVersionInfo;
begin
    Result := NULL_VERSION;
    try
        Result := PropAt [_pck_version].asVersion;
    except on E: Exception do
        _raise (['GetVersion',ERR_TPACKAGE_GET_VERSION,E],
                ['{4B2C4108-6730-4272-9B77-56AEF28005FA}']);
    end;
end;

procedure TPackage.SetVersion (const aValue: TVersionInfo);
begin
    try
        PropAt [_pck_version].asVersion := aValue;
    except on E: Exception do
        _raise (['SetVersion',ERR_TPACKAGE_SET_VERSION,E],
                ['{65DC1838-7FC1-4C92-BDB8-40D43592A668}']);
    end;
end;

function TPackage.GetKeyHash : Hex;
begin
    Result := '';
    try
        Result := PropAt [_pck_key_hash].asHex;
    except on E: Exception do
        _raise (['GetKeyHash',ERR_TPACKAGE_GET_KEY_HASH,E],
                ['{317070BE-F1C3-45A1-9D0A-CC7E5F65A462}']);
    end;
end;

procedure TPackage.SetKeyHash (const aValue: Hex);
begin
    try
        PropAt [_pck_key_hash].asHex := aValue;
    except on E: Exception do
        _raise (['SetKeyHash',ERR_TPACKAGE_SET_KEY_HASH,E],
                ['{B14A8668-33B7-4B0F-A776-E46B57FA6BA1}']);
    end;
end;

function TPackage.GetDataHash : Hex;
begin
    Result := '';
    try
        Result := PropAt [_pck_data_hash].asHex;
    except on E: Exception do
        _raise (['GetDataHash',ERR_TPACKAGE_GET_DATA_HASH,E],
                ['{A6F1899E-A93C-498A-80B7-68C9412759BB}']);
    end;
end;

function TPackage.CalcKeyHash : Hex;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto) then
        begin
            Result := User.Crypto.Hash ( Format ('%s_%s',[ CalcDataHash,
                                                           User.Crypto.GenerateKey ]) );
        end;
    except on E: Exception do
        _raise (['CalcKeyHash',ERR_TPACKAGE_CALC_KEY_HASH,E],
                ['{F488DD18-ADDD-4949-843B-9F992FD3B299}']);
    end;
end;

function TPackage.CalcDataHash : Hex;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto) and ( (IDOwner = 0) or (IDOwner = USER_ID) ) then
        begin
            if notEmpty (SenderHash) and notEmpty (ReceiverHash) then
                Result := User.Crypto.Hash ( Format ('%s_%s_%s',[ SenderHash,
                                                                  ReceiverHash,
                                                                  User.Crypto.Hash (MetaData) ]) );
        end;
    except on E: Exception do
        _raise (['CalcDataHash',ERR_TPACKAGE_CALC_DATA_HASH,E],
                ['{43034AB5-891F-46CC-B807-0AD93BA59947}']);
    end;
end;

procedure TPackage.LoadFromStream (const aStream: TStream);
var
    StreamDataHash : Hex;
    Stream         : TStream;
begin
    try
        if not Assigned (aStream) then
            raise Exception.Create (ERR_TPACKAGE_INVALID_STREAM);
        aStream.Position := 0;
        Mode := Mode - [objImported];
        with aStream do
        begin
            Version := StrToVersionInfo (  HexToStr ( ReadStrL (aStream) )  );
            KeyHash := ReadStrL (aStream);
            StreamDataHash := ReadStrL (aStream);
            SenderHash := ReadStrL (aStream);
            ReceiverHash := ReadStrL (aStream);
            TimeStampCreate := _StrToDateTime (  HexToStr ( ReadStrL (aStream) )  );
            TimeStampSend := _StrToDateTime (  HexToStr ( ReadStrL (aStream) )  );
            TimeStampReceive := _StrToDateTime (  HexToStr ( ReadStrL (aStream) )  );
            IDType := StrToInt (  HexToStr ( ReadStrL (aStream) )  );
            MetaClassID := HexToStr ( ReadStrL (aStream) );
            Stream := TMemoryStream.Create;
            try
                ReadStreamL (aStream,Stream);
                Stream.Position := 0;
                MetaData.Position := 0;
                MetaData.CopyFrom (Stream,Stream.Size);
            finally
                FreeAndNil (Stream);
            end;
        end;
        Mode := Mode + [objImported];
    except on E: Exception do
        _raise (['LoadFromStream',ERR_TPACKAGE_LOAD_FROM_STREAM,E],
                ['{56C3D468-9C8B-4B2D-9DA1-89C3B4C7869A}']);
    end;
end;

procedure TPackage.SaveToStream (out aStream: TStream);
var
    CalculatedDataHash : Hex;
begin
    try
        if not Assigned (aStream) then
            raise Exception.Create (ERR_TPACKAGE_INVALID_STREAM);
        aStream.Position := 0;
        Mode := Mode - [objExported];
        with aStream do
        begin
            WriteStrL (  aStream, StrToHex ( VersionInfoToStr (Version) )  );
            WriteStrL (aStream,KeyHash);
            CalculatedDataHash := CalcDataHash;
            WriteStrL (aStream,CalculatedDataHash);
            WriteStrL (aStream,SenderHash);
            WriteStrL (aStream,ReceiverHash);
            WriteStrL (  aStream, StrToHex ( _DateTimeToStr (TimeStampCreate) )  );
            WriteStrL (  aStream, StrToHex ( _DateTimeToStr (TimeStampSend) )  );
            WriteStrL (  aStream, StrToHex ( _DateTimeToStr (TimeStampReceive) )  );
            WriteStrL (  aStream, StrToHex ( IntToStr (IDType) )  );
            WriteStrL ( aStream, StrToHex (MetaClassID) );
            WriteStreamL (aStream,MetaData);
        end;
        Mode := Mode + [objExported];
    except on E: Exception do
        _raise (['SaveToStream',ERR_TPACKAGE_SAVE_TO_STREAM,E],
                ['{1FAB4EA3-22B6-4323-B641-28B664AFC92F}']);
    end;
end;

procedure TPackage.WriteMetaData (const anInput: TStream);
var
    Stream : TStream;
    S1, S2 : String;
begin
    try
        { отправитель - текущий пользователь }
        Sender := User;
        if not (  Assigned (Sender) and
                  Assigned (Sender.Crypto) and
                  ( Length (Sender.PrivateKey) > 0 ) and
                  isHex (Sender.PrivateKey)  ) then
            raise Exception.Create (ERR_TPACKAGE_INVALID_SENDER);
        { загружаем получателя }
        if not ( objLoaded in Receiver.Mode ) or ( objLoadFailed in Receiver.Mode ) then
            Receiver.Load;
        if not (  Assigned (Receiver) and
                  Assigned (Receiver.Crypto) and
                  ( Length (Receiver.PublicKey) > 0 ) and
                  isHex (Receiver.PublicKey)  ) then
            raise Exception.Create (ERR_TPACKAGE_INVALID_RECEIVER);
        { шифруем мета-данные публичным ключом получателя,
          подписываем приватным ключом отправителя;
          используем настройки крипто-системы получателя  }
        Stream := TMemoryStream.Create;
        with TMemoryStream (Stream) do
        try
            anInput.Position := 0;
            Receiver.Crypto.Encrypt (anInput,
                                     Stream,
                                     Receiver.PublicKey,
                                     Sender.Privatekey);
            Position := 0;
            MetaData.Position := 0;
            MetaData.CopyFrom (Stream,Stream.Size);
            MetaData.Position := 0;
        finally
            _FillChar ( S1, Length (S1), $00 );
            _FillChar ( S2, Length (S2), $00 );
            FreeAndNil (Stream);
        end;
    except on E: Exception do
        _raise (['WriteMetaData',ERR_TPACKAGE_WRITE_META_DATA,E],
                ['{9801C5C7-E8A7-4F6C-8756-B2C57AD7BA3F}']);
    end;
end;

procedure TPackage.ReadMetaData (out anOutput: TStream);
var
    Stream : TStream;
    S1, S2 : String;
begin
    try
        { пакет адресован текущему пользователю }
        if ( ReceiverHash = USER_KEY_HASH ) then
        begin
            { получатель - текущий пользователь }
            Receiver := User;
            if not (  Assigned (Receiver) and
                      Assigned (Receiver.Crypto) and
                      ( Length (Receiver.PrivateKey) > 0 ) and
                      isHex (Receiver.PrivateKey)  ) then
                raise Exception.Create (ERR_TPACKAGE_INVALID_RECEIVER);
            { загружаем отправителя }
            if not ( objLoaded in Sender.Mode ) or ( objLoadFailed in Sender.Mode ) then
                Sender.Load;
            if not (  Assigned (Sender) and
                      Assigned (Sender.Crypto) and
                      ( Length (Sender.PublicKey) > 0 ) and
                      isHex (Sender.PublicKey)  ) then
                raise Exception.Create (ERR_TPACKAGE_INVALID_SENDER);
            { дешифруем мета-данные приватным ключом получателя,
              проверяем подпись с помощью публичного ключа отправителя;
              используем настройки крипто-системы получателя  }
            Stream := TMemoryStream.Create;
            with TMemoryStream (Stream) do
            try
                MetaData.Position := 0;
                Receiver.Crypto.Decrypt (MetaData,
                                         Stream,
                                         Sender.PublicKey,
                                         Receiver.Privatekey);
                Position := 0;
                anOutput.Position := 0;
                anOutput.Size := 0;
                anOutput.CopyFrom (Stream,Stream.Size);
            finally
                _FillChar ( S1, Length (S1), $00 );
                _FillChar ( S2, Length (S2), $00 );
                FreeAndNil (Stream);
            end;
        end
        else
            raise Exception.Create (ERR_TPACKAGE_CAN_NOT_RECEIVE);
    except on E: Exception do
        _raise (['ReadMetaData',ERR_TPACKAGE_READ_META_DATA,E],
                ['{B5D9EA74-E40E-4470-BE0F-C24FA2F3FE56}']);
    end;
end;

function TPackage.GetHashList : THashList;
begin
    Result := NIL;
    try
        { проверка мета-класса }
        if not Assigned (MetaClass) then
            raise Exception.Create (ERR_TPACKAGE_INVALID_META_CLASS);
        { получаем хэш-таблицу для соответствующего мета-класса }
        if MetaClass.InheritsFrom (TUser) then
        begin
            Result := CUsers (MetaClass.GetClassList).GetHashes (DB,[ _([]), _([]), _([USER_ID]) ]);
            { включаем в хэш-лист собственного пользователя }
            if Assigned (Result) and not Assigned ( Result.ItemOf [USER_KEY_HASH] ) then
                Result.Add ([ User.KeyHash, User.DataHash, User.TimeStampVector ]);
        end
        else if MetaClass.InheritsFrom (TPic) then
            Result := CPics (MetaClass.GetClassList).GetHashes (DB,[ _([]), _([USER_ID]) ])
        else if MetaClass.InheritsFrom (TCategorie) then
            Result := CCategories (MetaClass.GetClassList).GetHashes (DB,[ _([]), _([]), _([]), _([USER_ID]) ])
        else if MetaClass.InheritsFrom (TMessage) then
            Result := CMessages (MetaClass.GetClassList).GetHashes (DB,[ _([]), _([]), _([]), _([USER_ID]) ])
        else
            raise Exception.CreateFmt ('%s : ''%s''',
                                       [ERR_TPACKAGE_INVALID_META_CLASS,MetaClass]);
    except on E: Exception do
        _raise (['GetHashList',ERR_TPACKAGE_GET_HASH_TABLE,E],
                ['{C082A875-7F39-4EB2-8D92-5F37B2D6F06F}']);
    end;
end;

function TPackage.FindMetaObject (const aHash: Hex) : TMetaObject;
var
    IDMetaObject : TID;
begin
    Result := NIL;
    try
        { проверка хэша мета-объекта }
        if not (  ( Length (aHash) > 0 ) and isHex (aHash)  ) then
            raise Exception.CreateFmt (ERR_TPACKAGE_INVALID_META_OBJECT_HASH,[aHash]);
        { поиск и загрузка мета-объекта по хэшу }
        IDMetaObject := 0;
        if MetaClass.InheritsFrom (TUser) and ( aHash = USER_KEY_HASH ) then
            IDMetaObject := USER_ID
        else if MetaClass.InheritsFrom (TUser) then
            IDMetaObject := CUsers (MetaClass.GetClassList).Find (DB,aHash,IDOwner)
        else if MetaClass.InheritsFrom (TPic) then
            IDMetaObject := CPics (MetaClass.GetClassList).Find (DB,aHash,IDOwner)
        else if MetaClass.InheritsFrom (TCategorie) then
            IDMetaObject := CCategories (MetaClass.GetClassList).Find (DB,aHash,IDOwner)
        else if MetaClass.InheritsFrom (TMessage) then
            IDMetaObject := CMessages (MetaClass.GetClassList).Find (DB,aHash,IDOwner);
        if ( IDMetaObject > 0 ) then
            Result := MetaClass.Load (DB,IDMetaObject);
    except on E: Exception do
        _raise (['FindMetaObject',ERR_TPACKAGE_FIND_META_OBJECT,E],
                ['{EEB35010-2E9C-4FD2-9DBA-B4E042F9BA5D}']);
    end;
end;

class procedure TPackage.WriteMetaObject (const anObject: TMetaObject;
                                          out aStream: TStream);
begin
    try
        { проверка мета-объекта }
        if not Assigned (anObject) then
            raise Exception.Create (ERR_TPACKAGE_INVALID_META_OBJECT);
        { проверка потока }
        if not Assigned (aStream) then
            raise Exception.Create (ERR_TPACKAGE_INVALID_STREAM);
        { запись мета-объекта в поток }
        anObject.SaveToStream (aStream);
    except on E: Exception do
        _raise (['WriteMetaObject',ERR_TPACKAGE_WRITE_META_OBJECT,E],
                ['{3CB9FE49-DBA4-4DDF-BA9C-57E5CFFC69F2}']);
    end;
end;

class procedure TPackage.ReadMetaObject (const aStream: TStream;
                                         out anObject: TMetaObject);
begin
    try
        { проверка мета-объекта }
        if not Assigned (anObject) then
            raise Exception.Create (ERR_TPACKAGE_INVALID_META_OBJECT);
        { проверка потока }
        if not Assigned (aStream) then
            raise Exception.Create (ERR_TPACKAGE_INVALID_STREAM);
        { чтение мета-объекта из потока }
        anObject.LoadFromStream (aStream);
    except on E: Exception do
        _raise (['ReadMetaObject',ERR_TPACKAGE_READ_META_OBJECT,E],
                ['{1EA64E80-E9DC-4227-BA92-EF0C4A2E131D}']);
    end;
end;

procedure TPackage.CreateMetaData (anArgs: array of const);
var
    Stream     : TStream;
    HashList   : THashList;
    MetaHash   : Hex;
    MetaObject : TMetaObject;
    I          : Integer;
begin
    try
        { отправитель - текущий пользователь }
        Sender := User;
        if not (  Assigned (Sender) and
                  Assigned (Sender.Crypto) and
                  ( Length (Sender.PrivateKey) > 0 ) and
                  isHex (Sender.PrivateKey)  ) then
            raise Exception.Create (ERR_TPACKAGE_INVALID_SENDER);
        SenderHash := Sender.KeyHash;
        { загружаем получателя }
        if not ( objLoaded in Receiver.Mode ) or ( objLoadFailed in Receiver.Mode ) then
            Receiver.Load;
        if not (  Assigned (Receiver) and
                  Assigned (Receiver.Crypto) and
                  ( Length (Receiver.PublicKey) > 0 ) and
                  isHex (Receiver.PublicKey)  ) then
            raise Exception.Create (ERR_TPACKAGE_INVALID_RECEIVER);
        ReceiverHash := Receiver.KeyHash;
        { проверка мета-класса }
        if not Assigned (MetaClass) then
            raise Exception.Create (ERR_TPACKAGE_INVALID_META_CLASS);
        { формируем мета-данные }
        Stream := TMemoryStream.Create;
        try
            case IDType of
                { запрашиваем мета-объект по хэшу }
                PACKAGE_GET_TYPE_ID : begin
                    MetaHash := '';
                    if notEmpty (0,anArgs) then
                        MetaHash := toString (anargs [0]);
                    if not (  ( Length (MetaHash) > 0 ) and isHex (MetaHash)  ) then
                        raise Exception.CreateFmt (ERR_TPACKAGE_INVALID_META_OBJECT_HASH,[MetaHash]);
                    { записываем хэш запрашиваемого объекта }
                    WriteStrL (Stream,MetaHash);
                end;
                { отсылаем обновление мета-объекта по хэшу }
                PACKAGE_PUT_TYPE_ID : begin
                    MetaHash := '';
                    if notEmpty (0,anArgs) then
                        MetaHash := toString (anargs [0]);
                    if not (  ( Length (MetaHash) > 0 ) and isHex (MetaHash)  ) then
                        raise Exception.CreateFmt (ERR_TPACKAGE_INVALID_META_OBJECT_HASH,[MetaHash]);
                    MetaObject := FindMetaObject (MetaHash);
                    if Assigned (MetaObject) then
                    try
                        { сохраняем мета-объект в поток }
                        WriteMetaObject (MetaObject,Stream);
                    finally
                        FreeAndNil (MetaObject);
                    end;
                end;
                { просим удалить мета-объект по хэшу }
                PACKAGE_DEL_TYPE_ID : begin
                    MetaHash := '';
                    if notEmpty (0,anArgs) then
                        MetaHash := toString (anargs [0]);
                    if not (  ( Length (MetaHash) > 0 ) and isHex (MetaHash)  ) then
                        raise Exception.CreateFmt (ERR_TPACKAGE_INVALID_META_OBJECT_HASH,[MetaHash]);
                    { записываем хэш удаляемого объекта }
                    WriteStrL (Stream,MetaHash);
                end;
                { обмениваемся хэш-таблицами }
                PACKAGE_UPD_TYPE_ID : begin
                    { получаем хэш-таблицу }
                    HashList := GetHashList;
                    if Assigned (HashList) then
                    try
                        for I := 0 to HashList.Count - 1 do
                        begin
                            { не отправляем информацию о корневом разделе }
                            if not (  MetaClass.InheritsFrom (TCategorie) and
                                      ( HashList.ItemAt [I].KeyHash = ROOT_CATEGORIE_KEY_HASH )  )
                            then
                            begin
                                WriteStrL ( Stream, HashList.ItemAt [I].KeyHash );
                                WriteStrL ( Stream, HashList.ItemAt [I].DataHash );
                                WriteStrL ( Stream, IntToHex (HashList.ItemAt [I].TimeStampVector,8) );
                            end;
                        end;
                    finally
                        HashList.FreeAll;
                        FreeAndNil (HashList);
                    end;
                end;
                { неизвестный тип пакета }
                else
                    raise Exception.Create (ERR_TPACKAGE_UNKNOWN_PACKAGE_TYPE);
            end;
            { записываем поток в поле мета-данных }
            WriteMetaData (Stream);
        finally
            FreeAndNil (Stream);
        end;
        { статус пакета - создан }
        IDStatus := PACKAGE_CREATED_STATUS_ID;
    except on E: Exception do
        _raise (['CreateMetaData',ERR_TPACKAGE_CREATE_META_DATA,E],
                ['{FBD2679E-1B97-4F4C-A52F-7754FF345223}']);
    end;
end;

procedure TPackage.ProcessMetaData;
var
    Stream        : TStream;
    MetaHash      : Hex;
    MetaObject    : TMetaObject;
    LocalObject   : TMetaObject;
    LocalList     : THashList;
    ExternalList  : THashList;
    SubstractList : THashList;
    ExtractList   : THashList;
    Request       : TPackage;
    I             : Integer;
begin
    try
        { пакет адресован текущему пользователю }
        if ( ReceiverHash = USER_KEY_HASH ) then
        begin
            { получатель - текущий пользователь }
            Receiver := User;
            if not (  Assigned (Receiver) and
                      Assigned (Receiver.Crypto) and
                      ( Length (Receiver.PrivateKey) > 0 ) and
                      isHex (Receiver.PrivateKey)  ) then
                raise Exception.Create (ERR_TPACKAGE_INVALID_RECEIVER);
            ReceiverHash := Receiver.KeyHash;
            { загружаем отправителя }
            if not ( objLoaded in Sender.Mode ) or ( objLoadFailed in Sender.Mode ) then
                Sender.Load;
            if not (  Assigned (Sender) and
                      Assigned (Sender.Crypto) and
                      ( Length (Sender.PublicKey) > 0 ) and
                      isHex (Sender.PublicKey)  ) then
                raise Exception.Create (ERR_TPACKAGE_INVALID_SENDER);
            SenderHash := Sender.KeyHash;
            { проверка мета-класса }
            if not Assigned (MetaClass) then
                raise Exception.Create (ERR_TPACKAGE_INVALID_META_CLASS);
            { обрабатываем мета-данные }
            Stream := TMemoryStream.Create;
            try
                { читаем поле мета-данных в поток }
                ReadMetaData (Stream);
                if ( Stream.Size <= 0 ) then
                    raise Exception.Create (ERR_TPACKAGE_INVALID_META_DATA);
                Stream.Position := 0;
                case IDType of
                    { обрабатываем запрос мета-объекта по хэшу }
                    PACKAGE_GET_TYPE_ID : begin
                        MetaHash := ReadStrL (Stream);
                        if not (  ( Length (MetaHash) > 0 ) and isHex (MetaHash)  ) then
                            raise Exception.CreateFmt (ERR_TPACKAGE_INVALID_META_OBJECT_HASH,[MetaHash]);
                        MetaObject := FindMetaObject (MetaHash);
                        if Assigned (MetaObject) then
                        try
                            { не отсылаем информацию о корневом разделе }
                            if not (  MetaClass.InheritsFrom (TCategorie) and
                                      ( MetaObject.PropOf ['key_hash'].asHex = ROOT_CATEGORIE_KEY_HASH )  )
                            then
                            begin
                                { формируем пакет-ответ на запрос объекта по хэшу }
                                Request := TPackage.Create (DB,[0,Receiver.ID,Sender.ID,USER_ID,
                                                                PACKAGE_PUT_TYPE_ID,
                                                                PACKAGE_CREATED_STATUS_ID,
                                                                MetaClass]);
                                Request.CreateMetaData ([ MetaHash ]);
                                Request.Save;
                            end;
                        finally
                            FreeAndNil (MetaObject);
                            FreeAndNil (Request);
                        end;
                    end;
                    { обрабатываем обновление мета-объекта по хэшу }
                    PACKAGE_PUT_TYPE_ID : begin
                        MetaObject := MetaClass.Create (DB,[]);
                        try
                            { загружаем мета-объект из потока }
                            ReadMetaObject (Stream,MetaObject);
                            { не пытаемся импортировать собственного пользователя
                              или его атрибуты, а также корневой раздел }
                            if not (  MetaClass.InheritsFrom (TUser) and
                                      ( MetaObject.PropOf ['key_hash'].asHex = User.KeyHash )  ) and
                               not (  MetaClass.InheritsFrom (TPic) and
                                      ( MetaObject.PropOf ['key_hash'].asHex = User.HashPic )  ) and
                               not (  MetaClass.InheritsFrom (TCategorie) and
                                      ( MetaObject.PropOf ['key_hash'].asHex = ROOT_CATEGORIE_KEY_HASH )  ) and
                               not (  MetaClass.InheritsFrom (TCategorie) and
                                      ( MetaObject.PropOf ['id_type'].asID = CATEGORIE_ROOT_TYPE_ID )  )
                            then
                            begin
                                { пытаемся найти объект с таким хэш-ключом в БД }
                                LocalObject := FindMetaObject ( MetaObject.PropOf ['key_hash'].asHex );
                                { если объект с таким хэшем найден, то
                                  проверяем версию мета-объекта и его хэш данных }
                                if Assigned (LocalObject) then
                                try
                                    { разрешаем редактировать объекты,
                                      созданные только отправителем }
                                    if (  ( LocalObject is TUser ) and
                                          ( LocalObject.PropOf ['key_hash'].asHex = Sender.KeyHash )  ) or
                                       (  ( {(LocalObject is TPic) or}
                                            (LocalObject is TCategorie) or
                                            (LocalObject is TMessage) ) and
                                          ( LocalObject.PropOf ['hash_author'].asHex = Sender.KeyHash )  ) then
                                    begin
                                        { если пришел объект с более поздним векторным временем,
                                          то сохраняем его }
                                        if ( LocalObject.PropOf ['time_stamp_vector'].asInt64 < MetaObject.PropOf ['time_stamp_vector'].asInt64 )
                                        { если пришла информация об отправителе - также пересохраняем }
                                        or (  MetaClass.InheritsFrom (TUser) and
                                              ( MetaObject.PropOf ['key_hash'].asHex = Sender.KeyHash )  )
                                        or (  MetaClass.InheritsFrom (TPic) and
                                              ( MetaObject.PropOf ['key_hash'].asHex = Sender.HashPic )  )
                                        then
                                        begin
                                            { вносим изменения в локальный объект
                                              и пересохраняем его }
                                            Stream.Position := 0;
                                            ReadMetaObject (Stream,LocalObject);
                                            LocalObject.PropOf ['time_stamp_public'].asDateTime := now;
                                            LocalObject.Save;
                                        end
                                        { если пришел объект с тем же векторным временем, но другим хэшем данных,
                                          то это конфликт версий объекта }
                                        else if ( LocalObject.PropOf ['time_stamp_vector'].asInt64 = MetaObject.PropOf ['time_stamp_vector'].asInt64 ) and
                                                ( LocalObject.PropOf ['data_hash'].asHex <> MetaObject.PropOf ['data_hash'].asHex ) then
                                        begin
                                            { сохраняем конфликтную версию объекта? }
                                        end;
                                    end;
                                finally
                                    FreeAndNil (LocalObject);
                                end
                                { если это новый объект,
                                  то сохраняем его }
                                else
                                begin
                                    MetaObject.PropOf ['time_stamp_public'].asDateTime := now;
                                    MetaObject.Save;
                                end;
                            end;
                        finally
                            FreeAndNil (MetaObject);
                        end;
                    end;
                    { обрабатываем удаление мета-объекта по хэшу }
                    PACKAGE_DEL_TYPE_ID : begin
                        MetaHash := ReadStrL (Stream);
                        if not (  ( Length (MetaHash) > 0 ) and isHex (MetaHash)  ) then
                            raise Exception.CreateFmt (ERR_TPACKAGE_INVALID_META_OBJECT_HASH,[MetaHash]);
                        MetaObject := FindMetaObject (MetaHash);
                        if Assigned (MetaObject) then
                        try
                            { не пытаемся удалить собственного пользователя
                              или его атрибуты,
                              а также корневой раздел }
                            if not (  MetaClass.InheritsFrom (TUser) and
                                      ( MetaObject.PropOf ['key_hash'].asHex = User.KeyHash )  ) and
                               not (  MetaClass.InheritsFrom (TPic) and
                                      ( MetaObject.PropOf ['key_hash'].asHex = User.HashPic )  ) and
                               not (  MetaClass.InheritsFrom (TCategorie) and
                                      ( MetaObject.PropOf ['key_hash'].asHex = ROOT_CATEGORIE_KEY_HASH )  )
                            then
                            begin
                                { пытаемся найти объект с таким хэш-ключом в БД }
                                LocalObject := FindMetaObject ( MetaObject.PropOf ['key_hash'].asHex );
                                { если объект с таким хэшем найден }
                                if Assigned (LocalObject) then
                                try
                                    { разрешаем удалять только сообщения,
                                      созданные только отправителем }
                                    if ( LocalObject is TMessage ) and
                                       ( LocalObject.PropOf ['hash_author'].asHex = Sender.KeyHash )
                                    then
                                        LocalObject.Delete;
                                finally
                                    FreeAndNil (LocalObject);
                                end;
                            end;
                        finally
                            FreeAndNil (MetaObject);
                        end;
                    end;
                    { пришел пакет обмена хэш-таблицами }
                    PACKAGE_UPD_TYPE_ID : begin
                        { получаем локальную хэш-таблицу }
                        LocalList := GetHashList;
                        { создаем пустую хэш-таблицу }
                        ExternalList := THashList.Create ([]);
                        try
                            { загружаем хэш-таблицу из потока }
                            ExternalList.LoadFromStream (Stream);
                            { получаем те элементы, которые есть во внешней таблице,
                              но которых нет в локальной (новые для получателя) }
                            SubstractList := THashList.Substract (ExternalList,LocalList);
                            if ( SubstractList.Count > 0 ) then
                            begin
                                { для каждого из новых хэш-ключей
                                  формируем пакет-запрос объекта по хэш-ключу }
                                for I := 0 to SubstractList.Count - 1 do
                                try
                                    Request := TPackage.Create (DB,[0,Receiver.ID,Sender.ID,USER_ID,
                                                                    PACKAGE_GET_TYPE_ID,
                                                                    PACKAGE_CREATED_STATUS_ID,
                                                                    MetaClass]);
                                    Request.CreateMetaData ([ SubstractList.ItemAt [I].KeyHash ]);
                                    Request.Save;
                                finally
                                    FreeAndNil (Request);
                                end;
                            end;
                            { получаем те элементы, у которых тот же хэш-ключ,
                              но более позднее векторное время,
                              или то же векторное время, но другой хэш данных }
                            ExtractList := THashList.Extract (ExternalList,LocalList);
                            if ( ExtractList.Count > 0 ) then
                            begin
                                { для каждого из требующих обновления хэш-ключей
                                  формируем пакет-запрос объекта по хэш-ключу }
                                for I := 0 to ExtractList.Count - 1 do
                                try
                                    Request := TPackage.Create (DB,[0,Receiver.ID,Sender.ID,USER_ID,
                                                                    PACKAGE_GET_TYPE_ID,
                                                                    PACKAGE_CREATED_STATUS_ID,
                                                                    MetaClass]);
                                    Request.CreateMetaData ([ ExtractList.ItemAt [I].KeyHash ]);
                                    Request.Save;
                                finally
                                    FreeAndNil (Request);
                                end;
                            end;
                        finally
                            FreeAndNil (LocalList);
                            FreeAndNil (ExternalList);
                            FreeAndNil (SubstractList);
                            FreeAndNil (ExtractList);
                        end;
                    end;
                    { неизвестный тип пакета }
                    else
                        raise Exception.Create (ERR_TPACKAGE_UNKNOWN_PACKAGE_TYPE);
                end;
            finally
                FreeAndNil (Stream);
            end;
        end
        { пакет предназначен не нам }
        else
            raise Exception.CreateFmt ('%s : ''%s''',
                                       [ ERR_TPACKAGE_INVALID_RECEIVER_HASH,
                                         ReceiverHash ]);
    except on E: Exception do
        _raise (['ProcessMetaData',ERR_TPACKAGE_PROCESS_META_DATA,E],
                ['{CB776EED-E568-42DC-97A2-00EF0CD7D01D}']);
    end;
end;
