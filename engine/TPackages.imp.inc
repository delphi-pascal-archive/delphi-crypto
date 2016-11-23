{******************************************************************************}
{*  TPackages.imp.inc                                                         *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TPackages }
function TPackages.Check (anItem: Pointer) : Boolean;
begin
    Result := FALSE;
    try
        Result := inherited Check (anItem) and
                  TObject (anItem).InheritsFrom (TPackage) and
                  Check ( TPackage (anItem) );
    except on E: Exception do
        _raise (['Check',ERR_TPACKAGES_CHECK,E],
                ['{969C7B1B-59B5-44C0-9F86-EB555A4916FC}']);
    end;
end;

function TPackages.Check (anItem: TPackage) : Boolean;
begin
    Result := Assigned (anItem) and anItem.Check;
end;

function TPackages.GetItemAt (anIndex: Integer) : TPackage;
begin
    Result := NIL;
    try
        if inherited ItemAt [anIndex].InheritsFrom (TPackage) then
            Result := inherited ItemAt [anIndex] as TPackage;
    except on E: Exception do
        _raise ([ Format ('GetItemAt [%d]',[anIndex]), ERR_TPACKAGES_GET_ITEM_AT, E ],
                ['{7E4C4E51-A306-44AF-99A8-1BD2C51CB241}']);
    end;
end;

procedure TPackages.SetItemAt (anIndex: Integer; anItem: TPackage);
begin
    try
        if inherited ItemAt [anIndex].InheritsFrom (TPackage) then
            inherited ItemAt [anIndex] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemAt [%d]',[anIndex]), ERR_TPACKAGES_SET_ITEM_AT, E ],
                ['{564E07DC-D56A-447B-9B00-86ABC4352DA1}']);
    end;
end;

function TPackages.GetItemOf (aName: String) : TPackage;
begin
    Result := NIL;
    try
        Result := inherited ItemOf [aName] as TPackage;
    except on E: Exception do
        _raise ([ Format ('GetItemOf [%s]',[aName]), ERR_TPACKAGES_GET_ITEM_OF, E ],
                ['{8E7C4E7A-884E-4E1C-9BA6-81D210151A5F}']);
    end;
end;

procedure TPackages.SetItemOf (aName: String; anItem: TPackage);
begin
    try
        inherited ItemOf [aName] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemOf [%s]',[aName]), ERR_TPACKAGES_SET_ITEM_OF, E ],
                ['{B71E2611-16BE-4820-AE59-3F1B51FE5600}']);
    end;
end;

function TPackages.GetItemOn (anID: TID) : TPackage;
begin
    Result := NIL;
    try
        Result := inherited ItemOn [anID] as TPackage;
    except on E: Exception do
        _raise ([ Format ('GetItemOn [%d]',[anID]), ERR_TPACKAGES_GET_ITEM_ON, E ],
                ['{285D9861-3077-4EB5-970F-AA4BE62DD46A}']);
    end;
end;

procedure TPackages.SetItemOn (anID: TID; anItem: TPackage);
begin
    try
        inherited ItemOn [anID] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemOn [%d]',[anID]), ERR_TPACKAGES_SET_ITEM_ON, E ],
                ['{9B934C71-4F4B-4415-9CE4-5F66E3B57756}']);
    end;
end;

class function TPackages.ItemClassType : CMetaObject;
begin
    Result := TPackage;
end;

function TPackages.Add (anItem: TPackage) : Integer;
begin
    Result := -1;
    try
        Result := inherited Add (anItem);
    except on E: Exception do
        _raise (['Add',ERR_TPACKAGES_ADD,E],
                ['{56A22A93-E532-4F49-9BA3-A42048E69B79}']);
    end;
end;

class function TPackages.Find (const aDB: TSQLiteDatabase;
                               const aHash: String;
                               const anIDOwner: TID) : TID;
var
    query : String;
    table : TSQLiteTable;
    I     : Integer;
    Props : TProtoProperties;
begin
    Result := 0;
    try
        if not Assigned (aDB) then
            raise Exception.Create (ERR_TPACKAGES_INVALID_DATABASE);
        Props := ItemClassType.GetClassProperties;
        try
            Props.ItemAt [_pck_key_hash].asHex := aHash;
            Props.ItemAt [_pck_id_owner].asInt64 := anIDOwner;

            query := Format ('SELECT %s '+
                             'FROM %s '+
                             'WHERE ( %s="%s" ) '+
                               'AND ( %s="%s" ) ',
                             [ Props.ItemAt [_id].Name,
                               ItemClassType.GetClassTable,
                               Props.ItemAt [_pck_key_hash].Name, TMetaProperty (Props.ItemAt [_pck_key_hash]).asDBFormat,
                               Props.ItemAt [_pck_id_owner].Name, TMetaProperty (Props.ItemAt [_pck_id_owner]).asDBFormat ]);
            //ShowMessage (query);
            table := aDB.GetTable (query);
            with table do
            try
                for I := 0 to table.Count-1 do
                begin
                    if ( I > 0 ) then
                        raise Exception.CreateFmt (ERR_TPACKAGE_NOT_UNIQUE_HASH,
                                                   [ Props.ItemAt [_pck_key_hash].Name,
                                                     TMetaProperty (Props.ItemAt [_pck_key_hash]).asDBFormat ]);
                    TMetaProperty (Props.ItemAt [_id]).asDBFormat := Fields [  FieldIndex [ Props.ItemAt [_id].Name ]  ];
                    Result := TMetaProperty (Props.ItemAt [_id]).asID;
                    table.Next;
                end;
            finally
                FreeAndNil (table);
            end;
        finally
            Props.FreeAll;
            FreeAndNil (Props);
        end;
    except on E: Exception do
        _raise (['Find',ERR_TPACKAGES_FIND,E],
                ['{90DFDD1E-D0A0-45BF-B2B1-36A515D50287}']);
    end;
end;

class procedure TPackages.Filter (var Query: String;
                                  anArgs: array of const);
var
    IDs               : TVarRec;
    OBJ               : TObject;
    arr               : array_of_const;
    I                 : Integer;
    Props             : TProtoProperties;
    Prop              : TMetaProperty;
    SendersHashes     : String;
    ReceiversHashes   : String;
    IDsOwners         : String;
    IDsTypes          : String;
    IDsStatuses       : String;
    MetaClassesIDs    : String;
    TimeStampsCreate  : String;
    TimeStampsSend    : String;
    TimeStampsReceive : String;
    Versions          : String;
begin
    try
        { первый параметр - список идентификаторов }
        if notEmpty (0,anArgs) then
            IDs := anArgs [0];
        { накладываем фильтр по алгоритму предка }
        arr := _([ NIL ]);
        arr [0] := IDs;
        inherited Filter (Query,arr);
        
        { накладываем дополнительный фильтр }
        Props := ItemClassType.GetClassProperties;
        try
            Prop := TMetaProperty (Props.ItemAt [_pck_sender_hash]);
            { второй параметр - список хэшей отправителей }
            SendersHashes := '';
            if notEmpty (1,anArgs) then
            begin
                OBJ := toObject (anArgs [1]);
                if Assigned (OBJ) and OBJ.InheritsFrom (TStringList) then
                begin
                    for I := 0 to TStringList (OBJ).Count - 1 do
                    begin
                        Prop.asHex := TStringList (OBJ).Strings [I];
                        if ( I = 0 ) then
                            SendersHashes := Format ('%s="%s"',[ Prop.Name, Prop.asDBFormat ])
                        else
                            SendersHashes := Format ('%s OR %s="%s"',[ SendersHashes,
                                                                       Prop.Name, Prop.asDBFormat ]);
                    end;
                end
                else
                begin
                    arr := toArrayOfConst (anArgs [1]);
                    for I := 0 to High (arr) do
                    begin
                        Prop.asHex := toString (arr [I]);
                        if ( I = 0 ) then
                            SendersHashes := Format ('%s="%s"',[ Prop.Name, Prop.asDBFormat ])
                        else
                            SendersHashes := Format ('%s OR %s="%s"',[ SendersHashes,
                                                                       Prop.Name, Prop.asDBFormat ]);
                    end;
                end;
            end;

            Prop := TMetaProperty (Props.ItemAt [_pck_receiver_hash]);
            { третий параметр - список хэшей получателей }
            ReceiversHashes := '';
            if notEmpty (2,anArgs) then
            begin
                OBJ := toObject (anArgs [2]);
                if Assigned (OBJ) and OBJ.InheritsFrom (TStringList) then
                begin
                    for I := 0 to TStringList (OBJ).Count - 1 do
                    begin
                        Prop.asHex := TStringList (OBJ).Strings [I];
                        if ( I = 0 ) then
                            ReceiversHashes := Format ('%s="%s"',[ Prop.Name, Prop.asDBFormat ])
                        else
                            ReceiversHashes := Format ('%s OR %s="%s"',[ ReceiversHashes,
                                                                         Prop.Name, Prop.asDBFormat ]);
                    end;
                end
                else
                begin
                    arr := toArrayOfConst (anArgs [2]);
                    for I := 0 to High (arr) do
                    begin
                        Prop.asHex := toString (arr [I]);
                        if ( I = 0 ) then
                            ReceiversHashes := Format ('%s="%s"',[ Prop.Name, Prop.asDBFormat ])
                        else
                            ReceiversHashes := Format ('%s OR %s="%s"',[ ReceiversHashes,
                                                                         Prop.Name, Prop.asDBFormat ]);
                    end;
                end;
            end;

            Prop := TMetaProperty (Props.ItemAt [_pck_id_owner]);
            { четвертый параметр - список идентификаторов владельцев }
            IDsOwners := '';
            if notEmpty (3,anArgs) then
            begin
                OBJ := toObject (anArgs [3]);
                if Assigned (OBJ) and OBJ.InheritsFrom (TStringList) then
                begin
                    for I := 0 to TStringList (OBJ).Count - 1 do
                    begin
                        Prop.asString := TStringList (OBJ).Strings [I];
                        if ( Prop.asID > 0 ) then
                            if ( I = 0 ) then
                                IDsOwners := Prop.asDBFormat
                            else
                                IDsOwners := Format ('%s,%s',[ IDsOwners, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDsOwners) > 0 ) then
                        IDsOwners := Format ('%s IN (%s) ',[ Prop.Name, IDsOwners ]);
                end
                else
                begin
                    arr := toArrayOfConst (anArgs [3]);
                    for I := 0 to High (arr) do
                    begin
                        Prop.asID := toInt64 (arr [I]);
                        if ( Prop.asID > 0 ) then
                            if ( I = 0 ) then
                                IDsOwners := Prop.asDBFormat
                            else
                                IDsOwners := Format ('%s,%s',[ IDsOwners, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDsOwners) > 0 ) then
                        IDsOwners := Format ('%s IN (%s) ',[ Prop.Name, IDsOwners ]);
                end;
            end;

            Prop := TMetaProperty (Props.ItemAt [_pck_id_type]);
            { пятый параметр - список идентификаторов типов }
            IDsTypes := '';
            if notEmpty (4,anArgs) then
            begin
                OBJ := toObject (anArgs [4]);
                if Assigned (OBJ) and OBJ.InheritsFrom (TStringList) then
                begin
                    for I := 0 to TStringList (OBJ).Count - 1 do
                    begin
                        Prop.asString := TStringList (OBJ).Strings [I];
                        if ( Prop.asID > 0 ) then
                            if ( I = 0 ) then
                                IDsTypes := Prop.asDBFormat
                            else
                                IDsTypes := Format ('%s,%s',[ IDsTypes, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDsTypes) > 0 ) then
                        IDsTypes := Format ('%s IN (%s) ',[ Prop.Name, IDsTypes ]);
                end
                else
                begin
                    arr := toArrayOfConst (anArgs [4]);
                    for I := 0 to High (arr) do
                    begin
                        Prop.asID := toInt64 (arr [I]);
                        if ( Prop.asID > 0 ) then
                            if ( I = 0 ) then
                                IDsTypes := Prop.asDBFormat
                            else
                                IDsTypes := Format ('%s,%s',[ IDsTypes, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDsTypes) > 0 ) then
                        IDsTypes := Format ('%s IN (%s) ',[ Prop.Name, IDsTypes ]);
                end;
            end;

            Prop := TMetaProperty (Props.ItemAt [_pck_id_status]);
            { шестой параметр - список идентификаторов статусов }
            IDsStatuses := '';
            if notEmpty (5,anArgs) then
            begin
                OBJ := toObject (anArgs [5]);
                if Assigned (OBJ) and OBJ.InheritsFrom (TStringList) then
                begin
                    for I := 0 to TStringList (OBJ).Count - 1 do
                    begin
                        Prop.asString := TStringList (OBJ).Strings [I];
                        if ( Prop.asID > 0 ) then
                            if ( I = 0 ) then
                                IDsStatuses := Prop.asDBFormat
                            else
                                IDsStatuses := Format ('%s,%s',[ IDsStatuses, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDsStatuses) > 0 ) then
                        IDsStatuses := Format ('%s IN (%s) ',[ Prop.Name, IDsStatuses ]);
                end
                else
                begin
                    arr := toArrayOfConst (anArgs [5]);
                    for I := 0 to High (arr) do
                    begin
                        Prop.asID := toInt64 (arr [I]);
                        if ( Prop.asID > 0 ) then
                            if ( I = 0 ) then
                                IDsStatuses := Prop.asDBFormat
                            else
                                IDsStatuses := Format ('%s,%s',[ IDsStatuses, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDsStatuses) > 0 ) then
                        IDsStatuses := Format ('%s IN (%s) ',[ Prop.Name, IDsStatuses ]);
                end;
            end;

            Prop := TMetaProperty (Props.ItemAt [_pck_meta_class_id]);
            { седьмой параметр - список мета-классов }
            MetaClassesIDs := '';
            if notEmpty (6,anArgs) then
            begin
                OBJ := toObject (anArgs [6]);
                if Assigned (OBJ) and OBJ.InheritsFrom (TStringList) then
                begin
                    for I := 0 to TStringList (OBJ).Count - 1 do
                    begin
                        Prop.asString := TStringList (OBJ).Strings [I];
                        if ( I = 0 ) then
                            MetaClassesIDs := Format ('%s="%s"',[ Prop.Name, Prop.asDBFormat ])
                        else
                            MetaClassesIDs := Format ('%s OR %s="%s"',[ MetaClassesIDs,
                                                                        Prop.Name, Prop.asDBFormat ]);
                    end;
                end
                else
                begin
                    arr := toArrayOfConst (anArgs [6]);
                    for I := 0 to High (arr) do
                    begin
                        if Assigned ( toClass (arr [I]) ) then
                            Prop.asString := CMetaObject ( toClass (arr [I]) ).GetClassID
                        else
                            Prop.asString := toString (arr [I]);
                        if ( I = 0 ) then
                            MetaClassesIDs := Format ('%s="%s"',[ Prop.Name, Prop.asDBFormat ])
                        else
                            MetaClassesIDs := Format ('%s OR %s="%s"',[ MetaClassesIDs,
                                                                        Prop.Name, Prop.asDBFormat ]);
                    end;
                end;
            end;

            Prop := TMetaProperty (Props.ItemAt [_pck_time_stamp_create]);
            { восьмой параметр - период создания }
            TimeStampsCreate := '';
            if notEmpty (7,anArgs) then
            begin
                arr := toArrayOfConst (anArgs [7]);
                for I := 0 to High (arr) do
                begin
                    Prop.asDateTime := toDateTime (arr [I]);
                    if ( I = 0 ) then
                        TimeStampsCreate := Format ('%s>="%s"',[ Prop.Name, Prop.asDBFormat ])
                    else
                        TimeStampsCreate := Format ('%s AND %s<="%s"',[ TimeStampsCreate,
                                                                        Prop.Name, Prop.asDBFormat ])
                end;
            end;

            Prop := TMetaProperty (Props.ItemAt [_pck_time_stamp_send]);
            { девятый параметр - период редактирования }
            TimeStampsSend := '';
            if notEmpty (8,anArgs) then
            begin
                arr := toArrayOfConst (anArgs [8]);
                for I := 0 to High (arr) do
                begin
                    Prop.asDateTime := toDateTime (arr [I]);
                    if ( I = 0 ) then
                        TimeStampsSend := Format ('%s>="%s"',[ Prop.Name, Prop.asDBFormat ])
                    else
                        TimeStampsSend := Format ('%s AND %s<="%s"',[ TimeStampsSend,
                                                                      Prop.Name, Prop.asDBFormat ])
                end;
            end;

            Prop := TMetaProperty (Props.ItemAt [_pck_time_stamp_receive]);
            { десятый параметр - период редактирования }
            TimeStampsReceive := '';
            if notEmpty (9,anArgs) then
            begin
                arr := toArrayOfConst (anArgs [9]);
                for I := 0 to High (arr) do
                begin
                    Prop.asDateTime := toDateTime (arr [I]);
                    if ( I = 0 ) then
                        TimeStampsReceive := Format ('%s>="%s"',[ Prop.Name, Prop.asDBFormat ])
                    else
                        TimeStampsReceive := Format ('%s AND %s<="%s"',[ TimeStampsReceive,
                                                                         Prop.Name, Prop.asDBFormat ])
                end;
            end;

            Prop := TMetaProperty (Props.ItemAt [_pck_version]);
            { одиннадцатый параметр - допустимый период версий }
            Versions := '';
            if notEmpty (10,anArgs) then
            begin
                arr := toArrayOfConst (anArgs [10]);
                for I := 0 to High (arr) do
                begin
                    Prop.asVersion := toVersionInfo (arr [I]);
                    if ( I = 0 ) then
                        Versions := Format ('%s>="%s"',[ Prop.Name, Prop.asDBFormat ])
                    else
                        Versions := Format ('%s AND %s<="%s"',[ Versions,
                                                                Prop.Name, Prop.asDBFormat ])
                end;
            end;

        finally
            Props.FreeAll;
            FreeAndNil (Props);
        end;

        if ( Length (SendersHashes) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,SendersHashes]);
        if ( Length (ReceiversHashes) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,ReceiversHashes]);
        if ( Length (IDsOwners) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,IDsOwners]);
        if ( Length (IDsTypes) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,IDsTypes]);
        if ( Length (IDsStatuses) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,IDsStatuses]);
        if ( Length (MetaClassesIDs) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,MetaClassesIDs]);
        if ( Length (TimeStampsCreate) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,TimeStampsCreate]);
        if ( Length (TimeStampsSend) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,TimeStampsSend]);
        if ( Length (TimeStampsReceive) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,TimeStampsReceive]);
        if ( Length (Versions) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,Versions]);
    except on E: Exception do
        _raise (['Filter',ERR_TPACKAGES_FILTER,E],
                ['{BA6FD641-DD02-412D-9C5E-324D5F9212DB}']);
    end;
end;

class procedure TPackages.OnLoad (const aDB: TSQLiteDatabase;
                                  anObject: TMetaObject;
                                  aTable: TSQLiteTable);
begin
    try
        inherited OnLoad (aDB,anObject,aTable);
        with TPackage (anObject) do
        try
            Mode := Mode - [objLoaded];
            if Assigned (Sender) then
            begin
                Sender.ID := IDSender;
                if ( IDSender > 0 ) then
                    Sender.Load;
            end
            else if ( IDSender > 0 ) then
                Sender := TUser.Load (DB,IDSender) as TUser
            else if ( Length (SenderHash) > 0 ) then
                Sender := TUser.Load ( DB, TUsers.Find (DB,SenderHash,USER_ID) ) as TUser
            else
                Sender := TUser.Create (DB,[]);
            if Assigned (Receiver) then
            begin
                Receiver.ID := IDReceiver;
                if ( IDReceiver > 0 ) then
                    Receiver.Load;
            end
            else if ( IDReceiver > 0 ) then
                Receiver := TUser.Load (DB,IDReceiver) as TUser
            else if ( Length (ReceiverHash) > 0 ) then
                Receiver := TUser.Load ( DB, TUsers.Find (DB,ReceiverHash,USER_ID) ) as TUser
            else
                Receiver := TUser.Create (DB,[]);
            if Assigned (PckType) then
            begin
                PckType.ID := IDType;
                if ( IDType > 0 ) then
                    PckType.Load;
            end
            else if ( IDType > 0 ) then
                PckType := TPackageType.Load (DB,IDType) as TPackageType
            else
                PckType := TPackageType.Create (DB,[]);
            if Assigned (PckStatus) then
            begin
                PckStatus.ID := IDStatus;
                if ( IDStatus > 0 ) then
                    PckStatus.Load;
            end
            else if ( IDStatus > 0 ) then
                PckStatus := TPackageStatus.Load (DB,IDStatus) as TPackageStatus
            else
                PckStatus := TPackageStatus.Create (DB,[]);
            MetaClass := GetMetaClassByID (MetaClassID);
            Mode := Mode + [objLoaded];
        except
            Mode := Mode + [objLoadFailed];
            raise;
        end;
    except on E: Exception do
        _raise (['OnLoad',ERR_TPACKAGES_LOAD,E],
                ['{B8E3D7D0-4183-4B03-B779-CDAA864DFDE1}']);
    end;
end;
