{******************************************************************************}
{*  TUser.imp.inc                                                             *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011-2012                                        *}
{******************************************************************************}
{ TUser }
class function TUser.GetClassName : String;
begin
    Result := CLS_TUSER_NAME;
end;

class function TUser.GetClassVersion : TVersionInfo;
begin
    Result := inherited GetClassVersion;
end;

class function TUser.GetClassTable : String;
begin
    Result := 'users';
end;

class function TUser.GetClassProperties : TProtoProperties;
begin
    Result := NIL;
    try
        Result := inherited GetClassProperties;
        with Result do
        begin
            ItemAt [_id_external].Name := 'login';
            ItemAt [_id_external].Caption := PRP_TUSER_LOGIN;
            ItemAt [_id_external].DataType := dtText;
            Register ( TMetaProperty, ['id_owner',PRP_TUSER_ID_OWNER,dtID,0],[prpStored] );
            Register ( TMetaProperty, ['password',PRP_TUSER_PASSWORD,dtSafe,''],[] );
            if ( ItemAt [_usr_password].DataType <> dtSafe ) then
                raise Exception.Create ('The password was not encrypted. Hacking attempt!');
            if ( prpStored in ItemAt [_usr_password].Mode ) then
                raise Exception.Create ('The password was saved. Hacking attempt!');
            Register ( TMetaProperty, ['salt',PRP_TUSER_SALT,dtHex,''],[prpStored] );
            Register ( TMetaProperty, ['id_crypto',PRP_TUSER_ID_CRYPTO,dtID,0],[prpStored] );
            Register ( TMetaProperty, ['email',PRP_TUSER_EMAIL,dtSafe,''],[prpStored] );
            if ( ItemAt [_usr_email].DataType <> dtSafe ) then
                raise Exception.Create ('The email was not encrypted. Hacking attempt!');
            Register ( TMetaProperty, ['email_password',PRP_TUSER_EMAIL_PASSWORD,dtSafe,''],[prpStored] );
            if ( ItemAt [_usr_email_password].DataType <> dtSafe ) then
                raise Exception.Create ('The email password was not encrypted. Hacking attempt!');
            Register ( TMetaProperty, ['ip',PRP_TUSER_IP,dtSafe,''],[prpStored] );
            if ( ItemAt [_usr_ip].DataType <> dtSafe ) then
                raise Exception.Create ('The ip-address was not encrypted. Hacking attempt!');
            Register ( TMetaProperty, ['port',PRP_TUSER_PORT,dtSafe,''],[prpStored] );
            if ( ItemAt [_usr_port].DataType <> dtSafe ) then
                raise Exception.Create ('The port was not encrypted. Hacking attempt!');
            Register ( TMetaProperty, ['description',PRP_TUSER_DESCRIPTION,dtSafe,''],[prpStored] );
            if ( ItemAt [_usr_description].DataType <> dtSafe ) then
                raise Exception.Create ('The description was not encrypted. Hacking attempt!');
            Register ( TMetaProperty, ['sex',PRP_TUSER_SEX,dtSafe,''],[prpStored] );
            if ( ItemAt [_usr_sex].DataType <> dtSafe ) then
                raise Exception.Create ('The sex was not encrypted. Hacking attempt!');
            Register ( TMetaProperty, ['birthday',PRP_TUSER_BIRTHDAY,dtSafe,''],[prpStored] );
            if ( ItemAt [_usr_birthday].DataType <> dtSafe ) then
                raise Exception.Create ('The birthday was not encrypted. Hacking attempt!');
            Register ( TMetaProperty, ['hash_pic',PRP_TUSER_HASH_PIC,dtHex,''],[prpStored] );
            Register ( TMetaProperty, ['id_pic',PRP_TUSER_ID_PIC,dtID,0],[prpStored] );
            Register ( TMetaProperty, ['public_key',PRP_TUSER_PUBLIC_KEY,dtString,''],[prpStored] );
            Register ( TMetaProperty, ['private_key',PRP_TUSER_PRIVATE_KEY,dtSafe,''],[prpStored] );
            if ( ItemAt [_usr_private_key].DataType <> dtSafe ) then
                raise Exception.Create ('The private key was not encrypted. Hacking attempt!');
            Register ( TMetaProperty, ['timeout',PRP_TUSER_TIMEOUT,dtInteger,3600],[prpStored] );
            Register ( TMetaProperty, ['use_proxy',PRP_TUSER_USE_PROXY,dtBoolean,FALSE],[prpStored] );
            Register ( TMetaProperty, ['proxy_ip',PRP_TUSER_PROXY_IP,dtSafe,''],[prpStored] );
            if ( ItemAt [_usr_proxy_ip].DataType <> dtSafe ) then
                raise Exception.Create ('The proxy ip-address was not encrypted. Hacking attempt!');
            Register ( TMetaProperty, ['proxy_port',PRP_TUSER_PROXY_PORT,dtSafe,''],[prpStored] );
            if ( ItemAt [_usr_proxy_port].DataType <> dtSafe ) then
                raise Exception.Create ('The proxy port was not encrypted. Hacking attempt!');
            Register ( TMetaProperty, ['proxy_login',PRP_TUSER_PROXY_LOGIN,dtSafe,''],[prpStored] );
            if ( ItemAt [_usr_proxy_login].DataType <> dtSafe ) then
                raise Exception.Create ('The proxy login was not encrypted. Hacking attempt!');
            Register ( TMetaProperty, ['proxy_password',PRP_TUSER_PROXY_PASSWORD,dtSafe,''],[prpStored] );
            if ( ItemAt [_usr_proxy_password].DataType <> dtSafe ) then
                raise Exception.Create ('The proxy password was not encrypted. Hacking attempt!');
            Register ( TMetaProperty, ['proxy_protocol',PRP_TUSER_PROXY_PROTOCOL,dtSafe,''],[prpStored] );
            if ( ItemAt [_usr_proxy_protocol].DataType <> dtSafe ) then
                raise Exception.Create ('The proxy protocol was not encrypted. Hacking attempt!');
            Register ( TMetaProperty, ['smtp_host',PRP_TUSER_SMTP_HOST,dtSafe,''],[prpStored] );
            if ( ItemAt [_usr_smtp_host].DataType <> dtSafe ) then
                raise Exception.Create ('The smtp ip-address was not encrypted. Hacking attempt!');
            Register ( TMetaProperty, ['smtp_port',PRP_TUSER_SMTP_PORT,dtSafe,''],[prpStored] );
            if ( ItemAt [_usr_smtp_port].DataType <> dtSafe ) then
                raise Exception.Create ('The smtp port was not encrypted. Hacking attempt!');
            Register ( TMetaProperty, ['pop3_host',PRP_TUSER_POP3_HOST,dtSafe,''],[prpStored] );
            if ( ItemAt [_usr_pop3_host].DataType <> dtSafe ) then
                raise Exception.Create ('The pop3 ip-address was not encrypted. Hacking attempt!');
            Register ( TMetaProperty, ['pop3_port',PRP_TUSER_POP3_PORT,dtSafe,''],[prpStored] );
            if ( ItemAt [_usr_pop3_port].DataType <> dtSafe ) then
                raise Exception.Create ('The pop3 port was not encrypted. Hacking attempt!');
            Register ( TMetaProperty, ['auto_tls',PRP_TUSER_AUTO_TLS,dtBoolean,FALSE],[prpStored] );
            Register ( TMetaProperty, ['full_ssl',PRP_TUSER_FULL_SSL,dtBoolean,FALSE],[prpStored] );
        end;
    except on E: Exception do
        _raise (['GetClassProperties',ERR_TUSER_GET_ClASS_PROPERTIES,E],
                ['{8FFE5843-CC4A-4993-9FDF-67D99060547F}']);
    end;
end;

class function TUser.GetClassList : CProtoObjects;
begin
    Result := TUsers;
end;

constructor TUser.Create (const aDB: TSQLiteDatabase;
                          anArgs: array of const;
                          aMode: TObjectMode = []);
begin
    try
        inherited Create (aDB,[],aMode);
        Mode := Mode - [objCreated];
        { первый параметр - идентификатор }
        if notEmpty (0,anArgs) then
            ID := toInt64 (anArgs [0]);
        { второй параметр - логин }
        if notEmpty (1,anArgs) then
            Login := toString (anArgs [1]);
        { третий параметр - идентификатор владельца }
        if notEmpty (2,anArgs) then
            IDOwner := toInt64 (anArgs [2]);
        if Assigned (User) and ( IDOwner = 0 ) then
            IDOwner := User.ID;
        { четвертый параметр - идентификатор крипто-системы }
        f_Crypto := TCrypto.Create (aDB,[],aMode);
        if notEmpty (3,anArgs) then
            IDCrypto := toInt64 (anArgs [3]);
        { п€тый параметр - иконка }
        f_Pic := TPic.Create (aDB,[],aMode);
        if notEmpty (4,anArgs) then
            IDPic := toInt64 (anArgs [4]);
        { шестой параметр - дата и врем€ создани€ }
        if notEmpty (5,anArgs) then
            TimeStampCreate := toDateTime (anArgs [5]);
        { седьмой параметр - дата и врем€ последнего редактировани€ }
        if notEmpty (6,anArgs) then
            TimeStampModify := toDateTime (anArgs [6]);
        { восьмой параметр - дата и врем€ публикации }
        if notEmpty (7,anArgs) then
            TimeStampPublic := toDateTime (anArgs [7]);
        { дев€тый параметр - верси€ }
        if notEmpty (8,anArgs) then
            Version := toVersionInfo (anArgs [8]);
        Mode := Mode + [objCreated];
    except on E: Exception do begin
        Mode := Mode + [objCreateFailed];
        _raise (['Create',ERR_TUSER_CREATE,E],
                ['{762040D1-E7FF-4B17-AEE4-F378692E371C}']);
    end; end;
end;

destructor TUser.Destroy;
begin
    try
        FreeAndNil (f_Crypto);
        FreeAndNil (f_Pic);
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TUSER_DESTROY,E],
                ['{FD66B04D-A8D3-4788-89AF-C8DA114FDDB8}']);
    end;
end;

class procedure TUser.CreateClassTable (const aDB: TSQLiteDatabase);
var
    Props     : TProtoProperties;
    CrptProps : TProtoProperties;
    PicProps  : TProtoProperties;
    CtgProps  : TProtoProperties;
    MsgProps  : TProtoProperties;
    PckProps  : TProtoProperties;
    query     : String;
begin
    try
        inherited CreateClassTable (aDB);
        {$IFDEF DELETE_TRIGGER}
        { создаем триггер на удаление }
        if not aDB.TriggerExists ( Format ('trigger_%s_%s',[GetClassTable,'delete']) ) then
        begin
            Props := GetClassProperties;
            CrptProps := TCrypto.GetClassProperties;
            PicProps := TPic.GetClassProperties;
            CtgProps := TCategorie.GetClassProperties;
            MsgProps := TMessage.GetClassProperties;
            PckProps := TPackage.GetClassProperties;
            try
                { создаем триггер }
                query := Format ('CREATE TRIGGER trigger_%s_%s '+
                                 'BEFORE DELETE '+
                                 'ON %s '+
                                 'FOR EACH ROW '+
                                 'BEGIN '+
                                     // delete crypto
                                     'DELETE '+
                                     'FROM %s '+
                                     'WHERE %s = OLD.%s; '+
                                     // delete pics
                                     'DELETE '+
                                     'FROM %s '+
                                     'WHERE ( %s = OLD.%s ) OR ( %s = OLD.%s ); '+
                                     // delete categories
                                     'DELETE '+
                                     'FROM %s '+
                                     'WHERE %s = OLD.%s; '+
                                     // delete messages
                                     'DELETE '+
                                     'FROM %s '+
                                     'WHERE %s = OLD.%s; '+
                                     // delete contacts
                                     'DELETE '+
                                     'FROM %s '+
                                     'WHERE %s = OLD.%s; '+
                                     // delete packages
                                     'DELETE '+
                                     'FROM %s '+
                                     'WHERE %s = OLD.%s; '+
                                 'END ',
                                 [ GetClassTable, 'delete',
                                   GetClassTable,
                                   // delete crypto
                                   TCrypto.GetClassTable,
                                   CrptProps.ItemAt [_id].Name, Props.ItemAt [_usr_id_crypto].Name,
                                   // delete pics
                                   TPic.GetClassTable,
                                   PicProps.ItemAt [_pic_id].Name, Props.ItemAt [_usr_id_pic].Name,
                                   PicProps.ItemAt [_pic_id_owner].Name, Props.ItemAt [_usr_id].Name,
                                   // delete categories
                                   TCategorie.GetClassTable,
                                   CtgProps.ItemAt [_ctg_id_owner].Name, Props.ItemAt [_usr_id].Name,
                                   // delete messages
                                   TMessage.GetClassTable,
                                   MsgProps.ItemAt [_msg_id_owner].Name, Props.ItemAt [_usr_id].Name,
                                   // delete contacts
                                   GetClassTable,
                                   Props.ItemAt [_usr_id_owner].Name, Props.ItemAt [_usr_id].Name,
                                   // delete packages
                                   TPackage.GetClassTable,
                                   PckProps.ItemAt [_pck_id_owner].Name, Props.ItemAt [_usr_id].Name ]);
                // ShowMessage (query);
                aDB.ExecSQL (query);
            finally
                PckProps.FreeAll;
                FreeAndNil (PckProps);
                MsgProps.FreeAll;
                FreeAndNil (MsgProps);
                CtgProps.FreeAll;
                FreeAndNil (CtgProps);
                PicProps.FreeAll;
                FreeAndNil (PicProps);
                CrptProps.FreeAll;
                FreeAndNil (CrptProps);
                Props.FreeAll;
                FreeAndNil (Props);
            end;
        end;
        {$ENDIF DELETE_TRIGGER}
    except on E: Exception do
        _raise (['CreateClassTable',ERR_TUSER_CREATE_CLASS_TABLE,E],
                ['{05593C46-55F3-4CB9-A3B8-F20952D0B352}']);
    end;
end;

procedure TUser.SetDB (const aDB: TSQLiteDatabase);
begin
    try
        inherited SetDB (aDB);
        if Assigned (f_Crypto) then
            f_Crypto.DB := aDB;
        if Assigned (f_Pic) then
            f_Pic.DB := aDB;
    except on E: Exception do
        _raise (['SetDB',ERR_TUSER_SET_DB,E],
                ['{EC8CE0FB-5098-4EB5-9A9F-155C124AB591}']);
    end;
end;

function TUser.Check : Boolean;
begin
    Result := TRUE;
    try
        Result := inherited Check;
        if Result then
        begin
            if not ( IDCrypto > 0 ) then
                Error (ERR_TUSER_INVALID_ID_CRYPTO);
            if not ( Length (PublicKey) > 0 ) then
                Error (ERR_TUSER_INVALID_PUBLIC_KEY);
            Result := ( Errors.Count = 0 );
        end;
    except on E: Exception do
        _raise (['Check',ERR_TUSER_CHECK,E],
                ['{B39BC3C3-691A-44D9-9E67-6B8FD5CB594A}']);
    end;
end;

class function TUser.Find (const aDB: TSQLiteDatabase;
                           const aLogin: String;
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
            raise Exception.Create (ERR_TUSER_INVALID_DATABASE);
        Props := GetClassProperties;
        try
            TMetaProperty (Props.ItemAt [_usr_login]).asString := aLogin;
            TMetaProperty (Props.ItemAt [_usr_id_owner]).asID := anIDOwner;

            query := Format ('SELECT %s '+
                             'FROM %s '+
                             'WHERE ( %s="%s" ) '+
                             '  AND ( %s="%s" ) ',
                             [ Props.ItemAt [_id].Name,
                               GetClassTable,
                               Props.ItemAt [_usr_login].Name, TMetaProperty (Props.ItemAt [_usr_login]).asDBFormat,
                               Props.ItemAt [_usr_id_owner].Name, TMetaProperty (Props.ItemAt [_usr_id_owner]).asDBFormat ]);
            //ShowMessage (query);
            table := aDB.GetTable (query);
            with table do
            try
                for I := 0 to table.Count-1 do
                begin
                    if ( I > 0 ) then
                        raise Exception.CreateFmt (ERR_TUSER_NOT_UNIQUE_ID_EXTERNAL,
                                                   [ Props.ItemAt [_usr_login].Name,
                                                     TMetaProperty (Props.ItemAt [_usr_login]).asDBFormat,
                                                     Props.ItemAt [_usr_id_owner].Name,
                                                     TMetaProperty (Props.ItemAt [_usr_id_owner]).asDBFormat ]);
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
        _raise (['Find',ERR_TUSER_FIND,E],
                ['{8A9D1848-58F4-434D-85B0-EC578CA96F40}']);
    end;
end;

function TUser.Find : TID;
begin
    Result := 0;
    try
        Result := Find (DB,Login,IDOwner);
    except on E: Exception do
        _raise (['Find',ERR_TUSER_FIND,E],
                ['{2B58601D-96EE-4B5E-87F7-1A3A17E29C25}']);
    end;
end;

procedure TUser.Load;
begin
    try
        if objNoLoad in Mode then Exit;
        if objSimple in Mode then
        begin
            inherited Load;
            Exit;
        end;
        Mode := Mode - [objLoadFailed];
        Mode := Mode - [objLoaded];
        inherited Load;
        Mode := Mode - [objLoaded];
        { crypto }
        if Assigned (Crypto) then
        begin
            Crypto.ID := PropAt [_usr_id_crypto].asID;
            if ( IDCrypto > 0 ) then
                Crypto.Load;
        end
        else
            Crypto := TCrypto.Load (DB,PropAt [_usr_id_crypto].asID) as TCrypto;
        { pic }
        if Assigned (Pic) then
        begin
            Pic.ID := PropAt [_usr_id_pic].asID;
            if ( IDPic > 0 ) then
                Pic.Load;
        end
        else
            Pic := TPic.Load (DB,PropAt [_usr_id_pic].asID) as TPic;
        Mode := Mode + [objLoaded];
    except on E: Exception do begin
        Mode := Mode + [objLoadFailed];
        _raise (['Load',ERR_TUSER_LOAD,E],
                ['{028DA3A8-EAFF-4BD3-BD5F-0664EB5171AC}']);
    end; end;
end;

procedure TUser.Save;
var
    isBeforeTransactionOpen : Boolean;
    Categories              : TCategories;
    Messages                : TMessages;
    I                       : Integer;
begin
    try
        if objNoSave in Mode then Exit;
        if objSimple in Mode then
        begin
            inherited Save;
            Exit;
        end;
        Mode := Mode - [objSaveFailed];
        Mode := Mode - [objSaved];
        try
        { при необходимости открываем транзакцию }
            isBeforeTransactionOpen := FALSE;
            if ( not DB.isTransactionOpen ) then
                DB.BeginTransaction
            else
                isBeforeTransactionOpen := TRUE;
            { crypto }
            Crypto.Save;
            PropAt [_usr_id_crypto].asID := Crypto.ID;
            { pic }
            if ( Self = User ) then
                Pic.IDOwner := Self.ID;
            if ( Pic.ID > 0 ) and notEmpty (HashPic) and ( Pic.KeyHash <> HashPic ) then
            begin
                Pic.Delete;
                IDPic := TPics.Find (DB,HashPic,IDOwner);
                if ( IDPic > 0 ) then
                    Pic.Load
                else
                    Pic.KeyHash := HashPic;
            end;
            Pic.Save;
            PropAt [_usr_id_pic].asID := Pic.ID;
            PropAt [_usr_hash_pic].asHex := HashPic;
            { перезаписываем KeyHash - он вычисл€етс€ из публичного ключа }
            PropAt [_usr_key_hash].asHex := CalcKeyHash;
            
            inherited Save;
            { прив€зываем все категории без идентификатора автора,
              у которых хэш-ключ автора - это хэш-ключ данного пользовател€ }
            if ( User <> Self ) then
            begin
                Categories := TCategories.Load (DB,[ NIL,
                                                     NIL,
                                                     _([0]),
                                                     _([IDOwner]) ],
                                                   [],
                                                   [],
                                                   [objSimple]) as TCategories;
                try
                    for I := 0 to Categories.Count - 1 do
                        if ( Categories.ItemAt [I].HashAuthor = KeyHash ) then
                        begin
                            Categories.ItemAt [I].IDAuthor := ID;
                            Categories.ItemAt [I].Save;
                        end;
                finally
                    FreeAndNil (Categories);
                end;
            end;
            { прив€зываем все сообщени€ без идентификатора автора,
              у которых хэш-ключ автора - это хэш-ключ данного пользовател€ }
            if ( User <> Self ) then
            begin
                Messages := TMessages.Load (DB,[ NIL,
                                                 NIL,
                                                 _([0]),
                                                 _([IDOwner]) ],
                                               [],
                                               [],
                                               [objSimple]) as TMessages;
                try
                    for I := 0 to Messages.Count - 1 do
                        if ( Messages.ItemAt [I].HashAuthor = KeyHash ) then
                        begin
                            Messages.ItemAt [I].IDAuthor := ID;
                            Messages.ItemAt [I].Save;
                        end;
                finally
                    FreeAndNil (Messages);
                end;
            end;
        { если ошибок не обнаружено - проводим транзакцию }
            if ( not isBeforeTransactionOpen ) then
                DB.Commit;
        { в случае возникновени€ ошибки - откатываем транзакцию }
        except on E: Exception do begin
            if ( not isBeforeTransactionOpen ) then
                DB.Rollback;
            raise Exception.CreateFmt (ERR_TUSER_TRANSACTION,[E.Message]);
        end; end;
    except on E: Exception do begin
        Mode := Mode + [objSaveFailed];
        _raise (['Save',ERR_TUSER_SAVE,E],
                ['{343FF313-C345-41F2-AE50-B93EDB47DF49}']);
    end; end;
end;

{$IFNDEF DELETE_TRIGGER}
procedure TUser.Delete;
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
            { удал€ем пользовател€ }
            inherited Delete;
            Mode := Mode - [objDeleted];
            { удал€ем настройки крипто-системы пользовател€ }
            Crypto.Delete;
            { удал€ем иконку пользовател€ }
            Pic.Delete;
            { удал€ем сообщени€, владельцем которых €вл€етс€ этот пользователь }
            TMessages.Delete (DB,[ NIL, NIL, NIL, _([ID]) ]);
            { удал€ем все категории, владельцем которых €вл€етс€ этот пользователь }
            TCategories.Delete (DB,[ NIL, NIL, NIL, _([ID]) ]);
            { удал€ем контакты пользовател€ }
            TUsers.Delete (DB,[ NIL, NIL, _([ID]) ]);
            { удал€ем пакеты пользовател€ - от него и дл€ него }
            TPackages.Delete (DB,[ NIL, _([ID]), _([])   ]);
            TPackages.Delete (DB,[ NIL, _([]),   _([ID]) ]);
         { если ошибок не обнаружено - проводим транзакцию }
            if ( not isBeforeTransactionOpen ) then
                DB.Commit;
         { в случае возникновени€ ошибки - откатываем транзакцию }
        except on E: Exception do begin
            if ( not isBeforeTransactionOpen ) then
                DB.Rollback;
            raise Exception.CreateFmt (ERR_TUSER_TRANSACTION,[E.Message]);
        end; end;
        Mode := Mode + [objDeleted];
    except on E: Exception do begin
        Mode := Mode + [objDeleteFailed];
        _raise (['Delete',ERR_TUSER_DELETE,E],
                ['{02BECD53-9F8E-4FBF-A75F-71D4C6C67B37}']);
    end; end;
end;
{$ENDIF DELETE_TRIGGER}

function TUser.CheckPassword: Boolean;
var
    Test : String;
    Encr : String;
    Decr : String;
begin
    Result := FALSE;
    try
        if Assigned (Crypto) and ( Password <> '' ) then
        try
            Result := TRUE;
            Crypto.GenerateKey (Test);
            Encr := Crypto.Encrypt (Test,
                                    PublicKey,
                                    PrivateKey);
            Decr := Crypto.Decrypt (Encr,
                                    PublicKey,
                                    PrivateKey);
            Result := ( Decr = Test );
        except
            Result := FALSE;
        end;
    except on E: Exception do
        _raise (['CheckPassword',ERR_TUSER_CHECK_PASSWORD,E],
                ['{66F7EAF0-FC02-4F45-9B64-B164527BD9EA}']);
    end;
end;

function TUser.GetIDOwner : TID;
begin
    Result := 0;
    try
        Result := PropAt [_usr_id_owner].asID;
    except on E: Exception do
        _raise (['GetIDOwner',ERR_TUSER_GET_ID_OWNER,E],
                ['{DE00F1CA-97FE-4628-AAD1-36D5D23313F8}']);
    end;
end;

procedure TUser.SetIDOwner (const aValue: TID);
begin
    try
        if ( aValue >= 0 ) then
            PropAt [_usr_id_owner].asID := aValue
        else
            PropAt [_usr_id_owner].asID := 0;
    except on E: Exception do
        _raise (['SetIDOwner',ERR_TUSER_SET_ID_OWNER,E],
                ['{865F2FA4-6578-4438-9AFF-E4A4539201E5}']);
    end;
end;

function TUser.GetIDCrypto : TID;
begin
    Result := 0;
    try
        Result := PropAt [_usr_id_crypto].asID;
    except on E: Exception do
        _raise (['GetIDCrypto',ERR_TUSER_GET_ID_CRYPTO,E],
                ['{95880C2D-2EF3-4222-9B75-34D9DB06926C}']);
    end;
end;

procedure TUser.SetIDCrypto (const aValue: TID);
begin
    try
        if ( aValue >= 0 ) then
            PropAt [_usr_id_crypto].asID := aValue
        else
            PropAt [_usr_id_crypto].asID := 0;
        if Assigned (Crypto) and ( Crypto.ID <> IDCrypto ) then
            Crypto.ID := IDCrypto
        else if not Assigned (Crypto) then
            Crypto := TCrypto.Create (DB,[IDCrypto]);
    except on E: Exception do
        _raise (['SetIDCrypto',ERR_TUSER_SET_ID_CRYPTO,E],
                ['{EA87445E-0273-466E-892D-ADAB1357A13F}']);
    end;
end;

function TUser.GetCrypto : TCrypto;
begin
    Result := NIL;
    try
        Result := f_Crypto;
    except on E: Exception do
        _raise (['GetCrypto',ERR_TUSER_GET_CRYPTO,E],
                ['{8486988E-68BC-49B0-BB8F-3A2B45C37208}']);
    end;
end;

procedure TUser.SetCrypto (const aValue: TCrypto);
begin
    try
        f_Crypto := aValue;
        if Assigned (Crypto) and ( Crypto.ID <> IDCrypto ) then
            IDCrypto := Crypto.ID;
    except on E: Exception do
        _raise (['SetCrypto',ERR_TUSER_SET_CRYPTO,E],
                ['{DE0B1A35-8BEC-4729-9082-45BFED6C24D5}']);
    end;
end;

function TUser.GetPassword : String;
begin
    Result := '';
    try
        if Assigned (Crypto) then
            Result := PropAt [_usr_password].asSafe [ Crypto.Hash ( Format ('%s_%s',[ Login, IntToHex (LongInt(self),8) ]) ),
                                                      Crypto ];
    except on E: Exception do
        _raise (['GetPassword',ERR_TUSER_GET_PASSWORD,E],
                ['{C0E12181-5F5F-48E5-9D9F-EFC8F67BE990}']);
    end;
end;

procedure TUser.SetPassword (const aValue: String);
begin
    try
        if Assigned (Crypto) then
            PropAt [_usr_password].asSafe [ Crypto.Hash ( Format ('%s_%s',[ Login, IntToHex (LongInt(self),8) ]) ),
                                            Crypto ] := aValue;
    except on E: Exception do
        _raise (['SetPassword',ERR_TUSER_SET_PASSWORD,E],
                ['{6811B03D-5F45-442F-BDBC-DA4271E8C429}']);
    end;
end;

function TUser.GetSalt : Hex;
begin
    Result := '';
    try
        Result := PropAt [_usr_salt].asHex;
    except on E: Exception do
        _raise (['GetSalt',ERR_TUSER_GET_SALT,E],
                ['{8811D09D-5100-4274-8DEA-F087BCFA9BDC}']);
    end;
end;

procedure TUser.SetSalt (const aValue: Hex);
begin
    try
        PropAt [_usr_salt].asHex := aValue;
    except on E: Exception do
        _raise (['SetSalt',ERR_TUSER_SET_SALT,E],
                ['{8FE8A61D-D68A-492F-9D24-EE51160C42F0}']);
    end;
end;

function TUser.GetEMail : String;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            Result := PropAt [_usr_email].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                                   User.Crypto ]
        else if Assigned (Crypto) then
            Result := PropAt [_usr_email].asSafe [ Crypto.Hash (Password,Salt),
                                                   Crypto ];
    except on E: Exception do
        _raise (['GetEMail',ERR_TUSER_GET_EMAIL,E],
                ['{813CB666-FE8A-4ABC-812B-193373345B63}']);
    end;
end;

procedure TUser.SetEMail (const aValue: String);
begin
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            PropAt [_usr_email].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                         User.Crypto ] := aValue
        else if Assigned (Crypto) then
            PropAt [_usr_email].asSafe [ Crypto.Hash (Password,Salt),
                                         Crypto ] := aValue;
    except on E: Exception do
        _raise (['SetEMail',ERR_TUSER_SET_EMAIL,E],
                ['{8FCF2989-1616-4A9E-BD72-2DEDF8EC0404}']);
    end;
end;

function TUser.GetEMailPassword : String;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            Result := PropAt [_usr_email_password].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                                            User.Crypto ]
        else if Assigned (Crypto) then
            Result := PropAt [_usr_email_password].asSafe [ Crypto.Hash (Password,Salt),
                                                            Crypto ];
    except on E: Exception do
        _raise (['GetEMailPassword',ERR_TUSER_GET_EMAIL_PASSWORD,E],
                ['{DA4E0C9A-4D1B-4A67-AA7E-2A00C53159BA}']);
    end;
end;

procedure TUser.SetEMailPassword (const aValue: String);
begin
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            PropAt [_usr_email_password].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                                                 User.Crypto ] := aValue
        else if Assigned (Crypto) then
            PropAt [_usr_email_password].asSafe [ Crypto.Hash (Password,Salt),
                                                                 Crypto ] := aValue;
    except on E: Exception do
        _raise (['SetEMailPassword',ERR_TUSER_SET_EMAIL_PASSWORD,E],
                ['{640C0CBA-D842-4C40-BABA-E7A1ADC05090}']);
    end;
end;

function TUser.GetIP : String;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            Result := PropAt [_usr_ip].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                                User.Crypto ]
        else if Assigned (Crypto) then
            Result := PropAt [_usr_ip].asSafe [ Crypto.Hash (Password,Salt),
                                                Crypto ];
    except on E: Exception do
        _raise (['GetIP',ERR_TUSER_GET_IP,E],
                ['{7D5FFE71-481F-4CDF-8F3C-EE004298424A}']);
    end;
end;

procedure TUser.SetIP (const aValue: String);
begin
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            PropAt [_usr_ip].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                      User.Crypto ] := aValue
        else if Assigned (Crypto) then
            PropAt [_usr_ip].asSafe [ Crypto.Hash (Password,Salt),
                                      Crypto ] := aValue;
    except on E: Exception do
        _raise (['SetIP',ERR_TUSER_SET_IP,E],
                ['{2F560908-2E5E-49AC-8DCE-46FAFD75897F}']);
    end;
end;

function TUser.GetPort : WORD;
begin
    Result := 0;
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            Result := StrToInt ( PropAt [_usr_port].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                                             User.Crypto ] )
        else if Assigned (Crypto) then
            Result := StrToInt ( PropAt [_usr_port].asSafe [ Crypto.Hash (Password,Salt),
                                                             Crypto ] );
    except on E: Exception do
        _raise (['GetPort',ERR_TUSER_GET_PORT,E],
                ['{92109012-4776-447B-99A7-7C04FB317867}']);
    end;
end;

procedure TUser.SetPort (const aValue: WORD);
begin
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            PropAt [_usr_port].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                        User.Crypto ] := IntToStr (aValue)
        else if Assigned (Crypto) then
            PropAt [_usr_port].asSafe [ Crypto.Hash (Password,Salt),
                                        Crypto ] := IntToStr (aValue);
    except on E: Exception do
        _raise (['SetPort',ERR_TUSER_SET_PORT,E],
                ['{17A2A6FC-9070-43B8-BA1E-95923C21F7E2}']);
    end;
end;

function TUser.GetHashPic : Hex;
begin
    Result := '';
    try
        if notEmpty ( PropAt [_usr_hash_pic].asHex ) then
            Result := PropAt [_usr_hash_pic].asHex
        else if Assigned (Pic) and ( Pic.ID > 0 ) then
        begin
            if not ( objLoaded in Pic.Mode ) then
                PropAt [_usr_hash_pic].asHex := TPic.GetHash (DB,IDPic)
            else
                PropAt [_usr_hash_pic].asHex := Pic.KeyHash;
            Result := PropAt [_usr_hash_pic].asHex;
        end;
    except on E: Exception do
        _raise (['GetHashPic',ERR_TUSER_GET_HASH_PIC,E],
                ['{FB14CE80-33DD-41E7-BF4D-17ABA3CA6061}']);
    end;
end;

procedure TUser.SetHashPic (const aValue: Hex);
begin
    try
        if isHex (aValue) then
            PropAt [_usr_hash_pic].asHex := aValue
        else
            PropAt [_usr_hash_pic].asHex := '';
    except on E: Exception do
        _raise (['SetHashPic',ERR_TUSER_SET_HASH_PIC,E],
                ['{DE129898-7B00-4FFA-8E9E-A8C137F4176C}']);
    end;
end;

function TUser.GetIDPic : TID;
begin
    Result := 0;
    try
        Result := PropAt [_usr_id_pic].asID;
    except on E: Exception do
        _raise (['GetIDPic',ERR_TUSER_GET_ID_PIC,E],
                ['{63760A12-CF87-4099-B348-490952E65C4F}']);
    end;
end;

procedure TUser.SetIDPic (const aValue: TID);
begin
    try
        if ( aValue >= 0 ) then
            PropAt [_usr_id_pic].asID := aValue
        else
            PropAt [_usr_id_pic].asID := 0;
        if Assigned (Pic) and ( Pic.ID <> IDPic ) then
            Pic.ID := IDPic
        else if not Assigned (Pic) then
            Pic := TPic.Create (DB,[IDPic]);
    except on E: Exception do
        _raise (['SetIDPic',ERR_TUSER_SET_ID_PIC,E],
                ['{4E7973C4-BE9C-4F36-92A5-DDD067BAA03F}']);
    end;
end;

function TUser.GetPic : TPic;
begin
    Result := NIL;
    try
        Result := f_Pic;
    except on E: Exception do
        _raise (['GetPic',ERR_TUSER_GET_PIC,E],
                ['{D6323C36-5D09-4A75-AB11-28BC69282CDC}']);
    end;
end;

procedure TUser.SetPic (const aValue: TPic);
begin
    try
        f_Pic := aValue;
        if Assigned (Pic) and ( Pic.ID <> IDPic ) then
            IDPic := Pic.ID;
    except on E: Exception do
        _raise (['SetPic',ERR_TUSER_SET_PIC,E],
                ['{BBA26B21-B3B6-4869-AE2D-E5FEAA45CDC8}']);
    end;
end;

function TUser.GetDescription : String;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            Result := Decompress ( PropAt [_usr_description].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                                                      User.Crypto ] )
        else if Assigned (Crypto) then
            Result := Decompress ( PropAt [_usr_description].asSafe [ Crypto.Hash (Password,Salt),
                                                                      Crypto ] );
    except on E: Exception do
        _raise (['GetDescription',ERR_TUSER_GET_DESCRIPTION,E],
                ['{089EF29A-D90C-4937-AC4D-D749A4234F6E}']);
    end;
end;

procedure TUser.SetDescription (const aValue: String);
begin
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            PropAt [_usr_description].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                               User.Crypto ] := Compress (aValue)
        else if Assigned (Crypto) then
            PropAt [_usr_description].asSafe [ Crypto.Hash (Password,Salt),
                                               Crypto ] := Compress (aValue);
    except on E: Exception do
        _raise (['SetDescription',ERR_TUSER_SET_DESCRIPTION,E],
                ['{60CF7C92-6BBE-44BE-8419-401A007ECD68}']);
    end;
end;

function TUser.GetSex : Byte;
begin
    Result := 0;
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            Result := HexToInt ( PropAt [_usr_sex].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                                            User.Crypto ] )
        else if Assigned (Crypto) then
            Result := HexToInt ( PropAt [_usr_sex].asSafe [ Crypto.Hash (Password,Salt),
                                                            Crypto ] );
    except on E: Exception do
        _raise (['GetSex',ERR_TUSER_GET_SEX,E],
                ['{830E08E7-3919-408A-A64C-7484A90A675A}']);
    end;
end;

procedure TUser.SetSex (const aValue: Byte);
begin
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            PropAt [_usr_sex].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                       User.Crypto ] := IntToHex (aValue,2)
        else if Assigned (Crypto) then
            PropAt [_usr_sex].asSafe [ Crypto.Hash (Password,Salt),
                                       Crypto ] := IntToHex (aValue,2);
    except on E: Exception do
        _raise (['SetSex',ERR_TUSER_SET_SEX,E],
                ['{F1CC42F2-7416-4B90-A717-7F19018F2BDD}']);
    end;
end;

function TUser.GetBirthday : TDateTime;
begin
    Result := 0.0;
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            Result := _StrToDate ( PropAt [_usr_birthday].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                                                   User.Crypto ] )
        else if Assigned (Crypto) then
            Result := _StrToDate ( PropAt [_usr_birthday].asSafe [ Crypto.Hash (Password,Salt),
                                                                   Crypto ] );
    except on E: Exception do
        _raise (['GetBirthday',ERR_TUSER_GET_BIRTHDAY,E],
                ['{A18EF3D0-0112-4020-8860-CB028596CE84}']);
    end;
end;

procedure TUser.SetBirthday (const aValue: TDateTime);
begin
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            PropAt [_usr_birthday].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                            User.Crypto ] := _DateToStr ( Trunc (aValue) )
        else if Assigned (Crypto) then
            PropAt [_usr_birthday].asSafe [ Crypto.Hash (Password,Salt),
                                            Crypto ] := _DateToStr ( Trunc (aValue) );
    except on E: Exception do
        _raise (['SetBirthday',ERR_TUSER_SET_BIRTHDAY,E],
                ['{F977C0C9-DED8-4EB0-B263-6581E3D789C5}']);
    end;
end;

function TUser.GetPublicKey : String;
begin
    Result := '';
    try
        Result := PropAt [_usr_public_key].asString;
    except on E: Exception do
        _raise (['GetPublicKey',ERR_TUSER_GET_PUBLIC_KEY,E],
                ['{3004FAFC-8F72-4B0D-BDFE-9CF4B9E3F648}']);
    end;
end;

procedure TUser.SetPublicKey (const aValue: String);
begin
    try
        PropAt [_usr_public_key].asString := aValue;
    except on E: Exception do
        _raise (['SetPublicKey',ERR_TUSER_SET_PUBLIC_KEY,E],
                ['{6731CB9D-C99B-4AFA-9E01-EB0ECEB5385E}']);
    end;
end;

function TUser.GetPrivateKey : String;
begin
    Result := '';
    try
        if Assigned (Crypto) and ( User = Self ) then
            Result := PropAt [_usr_private_key].asSafe [ Crypto.Hash ( Format ('%s_%s',[Password,Salt]) ),
                                                         Crypto ];
    except on E: Exception do
        _raise (['GetPrivateKey',ERR_TUSER_GET_PRIVATE_KEY,E],
                ['{7B224073-7607-4C0C-AFF7-F7F3C00D969F}']);
    end;
end;

procedure TUser.SetPrivateKey (const aValue: String);
begin
    try
        if Assigned (Crypto) and ( User = Self ) then
            PropAt [_usr_private_key].asSafe [ Crypto.Hash ( Format ('%s_%s',[Password,Salt]) ),
                                               Crypto ] := aValue;
    except on E: Exception do
        _raise (['SetPrivateKey',ERR_TUSER_SET_PRIVATE_KEY,E],
                ['{C1531344-7821-48AF-A5DE-876E5D36CE80}']);
    end;
end;

function TUser.GetUseProxy : Boolean;
begin
    Result := FALSE;
    try
        Result := PropAt [_usr_use_proxy].asBoolean;
    except on E: Exception do
        _raise (['GetUseProxy',ERR_TUSER_GET_USE_PROXY,E],
                ['{26933C27-FD66-4F55-B519-F0EBBB94E21D}']);
    end;
end;

procedure TUser.SetUseProxy (const aValue: Boolean);
begin
    try
        PropAt [_usr_use_proxy].asBoolean := aValue;
    except on E: Exception do
        _raise (['SetUseProxy',ERR_TUSER_SET_USE_PROXY,E],
                ['{213ECA94-C0CC-47C5-A875-9119403065A8}']);
    end;
end;

function TUser.GetProxyIP : String;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            Result := PropAt [_usr_proxy_ip].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                                      User.Crypto ]
        else if Assigned (Crypto) then
            Result := PropAt [_usr_proxy_ip].asSafe [ Crypto.Hash (Password,Salt),
                                                      Crypto ];
    except on E: Exception do
        _raise (['GetProxyIP',ERR_TUSER_GET_PROXY_IP,E],
                ['{F9A167AE-8EE1-44BF-BD66-061D3D4C3F0E}']);
    end;
end;

procedure TUser.SetProxyIP (const aValue: String);
begin
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            PropAt [_usr_proxy_ip].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                            User.Crypto ] := aValue
        else if Assigned (Crypto) then
            PropAt [_usr_proxy_ip].asSafe [ Crypto.Hash (Password,Salt),
                                            Crypto ] := aValue;
    except on E: Exception do
        _raise (['SetProxyIP',ERR_TUSER_SET_PROXY_IP,E],
                ['{72C1E224-830C-40FA-A496-9F0F4ACDE69F}']);
    end;
end;

function TUser.GetProxyPort : WORD;
begin
    Result := 0;
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            Result := StrToInt ( PropAt [_usr_proxy_port].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                                                   User.Crypto ] )
        else if Assigned (Crypto) then
            Result := StrToInt ( PropAt [_usr_proxy_port].asSafe [ Crypto.Hash (Password,Salt),
                                                                   Crypto ] );
    except on E: Exception do
        _raise (['GetProxyPort',ERR_TUSER_GET_PROXY_PORT,E],
                ['{3D2C8589-BB51-4EA0-A260-4C4C261ADDF6}']);
    end;
end;

procedure TUser.SetProxyPort (const aValue: WORD);
begin
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            PropAt [_usr_proxy_port].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                              User.Crypto ] := IntToStr (aValue)
        else if Assigned (Crypto) then
            PropAt [_usr_proxy_port].asSafe [ Crypto.Hash (Password,Salt),
                                              Crypto ] := IntToStr (aValue);
    except on E: Exception do
        _raise (['SetProxyPort',ERR_TUSER_SET_PROXY_PORT,E],
                ['{8444C136-17FF-47E5-9882-B21AA6A38DBF}']);
    end;
end;

function TUser.GetProxyLogin : String;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            Result := PropAt [_usr_proxy_login].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                                         User.Crypto ]
        else if Assigned (Crypto) then
            Result := PropAt [_usr_proxy_login].asSafe [ Crypto.Hash (Password,Salt),
                                                         Crypto ];
    except on E: Exception do
        _raise (['GetProxyLogin',ERR_TUSER_GET_PROXY_LOGIN,E],
                ['{8C4D2665-58D0-4021-A5F0-C5B177F0F2F0}']);
    end;
end;

procedure TUser.SetProxyLogin (const aValue: String);
begin
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            PropAt [_usr_proxy_login].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                               User.Crypto ] := aValue
        else if Assigned (Crypto) then
            PropAt [_usr_proxy_login].asSafe [ Crypto.Hash (Password,Salt),
                                               Crypto ] := aValue;
    except on E: Exception do
        _raise (['SetProxyLogin',ERR_TUSER_SET_PROXY_LOGIN,E],
                ['{4290F8FA-8341-4821-A062-36A39CB54193}']);
    end;
end;

function TUser.GetProxyPassword : String;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            Result := PropAt [_usr_proxy_password].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                                            User.Crypto ]
        else if Assigned (Crypto) then
            Result := PropAt [_usr_proxy_password].asSafe [ Crypto.Hash (Password,Salt),
                                                            Crypto ];
    except on E: Exception do
        _raise (['GetProxyPassword',ERR_TUSER_GET_PROXY_PASSWORD,E],
                ['{F24E7ECE-5733-4629-8167-53A62C7E5A5C}']);
    end;
end;

procedure TUser.SetProxyPassword (const aValue: String);
begin
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            PropAt [_usr_proxy_password].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                                  User.Crypto ] := aValue
        else if Assigned (Crypto) then
            PropAt [_usr_proxy_password].asSafe [ Crypto.Hash (Password,Salt),
                                                  Crypto ] := aValue;
    except on E: Exception do
        _raise (['SetProxyPassword',ERR_TUSER_SET_PROXY_PASSWORD,E],
                ['{74CE4BB1-FF2D-4C83-BB08-5DB4DF8D4DEA}']);
    end;
end;

function TUser.GetProxyProtocol : String;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            Result := PropAt [_usr_proxy_protocol].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                                            User.Crypto ]
        else if Assigned (Crypto) then
            Result := PropAt [_usr_proxy_protocol].asSafe [ Crypto.Hash (Password,Salt),
                                                            Crypto ];
    except on E: Exception do
        _raise (['GetProxyProtocol',ERR_TUSER_GET_PROXY_PROTOCOL,E],
                ['{BB98FFAD-7883-4B6B-B74F-C25A061E0861}']);
    end;
end;

procedure TUser.SetProxyProtocol (const aValue: String);
begin
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            PropAt [_usr_proxy_protocol].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                                  User.Crypto ] := aValue
        else if Assigned (Crypto) then
            PropAt [_usr_proxy_protocol].asSafe [ Crypto.Hash (Password,Salt),
                                                  Crypto ] := aValue;
    except on E: Exception do
        _raise (['SetProxyProtocol',ERR_TUSER_SET_PROXY_PROTOCOL,E],
                ['{8837553C-A757-4DA9-8B0F-932E80DD5909}']);
    end;
end;

function TUser.GetTimeOut : WORD;
begin
    Result := 0;
    try
        Result := PropAt [_usr_timeout].asInteger;
    except on E: Exception do
        _raise (['GetTimeOut',ERR_TUSER_GET_TIMEOUT,E],
                ['{937210E8-DB6F-421D-9B14-6E4698AC6A3E}']);
    end;
end;

procedure TUser.SetTimeOut (const aValue: WORD);
begin
    try
        PropAt [_usr_timeout].asInteger := aValue;
    except on E: Exception do
        _raise (['SetTimeOut',ERR_TUSER_SET_TIMEOUT,E],
                ['{215CF373-9CAF-4174-A398-67A7BB4C02A0}']);
    end;
end;

function TUser.GetSMTPHost : String;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            Result := PropAt [_usr_smtp_host].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                                       User.Crypto ]
        else if Assigned (Crypto) then
            Result := PropAt [_usr_smtp_host].asSafe [ Crypto.Hash (Password,Salt),
                                                       Crypto ];
    except on E: Exception do
        _raise (['GetSMTPHost',ERR_TUSER_GET_SMTP_HOST,E],
                ['{BEA0C1E0-D8EB-4388-B487-B16252BEA8B7}']);
    end;
end;

procedure TUser.SetSMTPHost (const aValue: String);
begin
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            PropAt [_usr_smtp_host].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                             User.Crypto ] := aValue
        else if Assigned (Crypto) then
            PropAt [_usr_smtp_host].asSafe [ Crypto.Hash (Password,Salt),
                                             Crypto ] := aValue;
    except on E: Exception do
        _raise (['SetSMTPHost',ERR_TUSER_SET_SMTP_HOST,E],
                ['{217A25B1-D617-492B-BFF5-BC9EEF58793D}']);
    end;
end;

function TUser.GetSMTPPort : WORD;
begin
    Result := 0;
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            Result := StrToInt ( PropAt [_usr_smtp_port].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                                                  User.Crypto ] )
        else if Assigned (Crypto) then
            Result := StrToInt ( PropAt [_usr_smtp_port].asSafe [ Crypto.Hash (Password,Salt),
                                                                  Crypto ] );
    except on E: Exception do
        _raise (['GetSMTPPort',ERR_TUSER_GET_SMTP_PORT,E],
                ['{2E817FD7-423B-4A47-A1D5-CAC589923F11}']);
    end;
end;

procedure TUser.SetSMTPPort (const aValue: WORD);
begin
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            PropAt [_usr_smtp_port].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                             User.Crypto ] := IntToStr (aValue)
        else if Assigned (Crypto) then
            PropAt [_usr_smtp_port].asSafe [ Crypto.Hash (Password,Salt),
                                             Crypto ] := IntToStr (aValue);
    except on E: Exception do
        _raise (['SetSMTPPort',ERR_TUSER_SET_SMTP_PORT,E],
                ['{272B8641-AA4C-4F32-96BF-5BA91E62C2DA}']);
    end;
end;

function TUser.GetPOP3Host : String;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            Result := PropAt [_usr_POP3_host].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                                       User.Crypto ]
        else if Assigned (Crypto) then
            Result := PropAt [_usr_POP3_host].asSafe [ Crypto.Hash (Password,Salt),
                                                       Crypto ];
    except on E: Exception do
        _raise (['GetPOP3Host',ERR_TUSER_GET_POP3_HOST,E],
                ['{E6756359-DFF5-4238-88D3-BDF5AA4CE4E4}']);
    end;
end;

procedure TUser.SetPOP3Host (const aValue: String);
begin
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            PropAt [_usr_POP3_host].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                             User.Crypto ] := aValue
        else if Assigned (Crypto) then
            PropAt [_usr_POP3_host].asSafe [ Crypto.Hash (Password,Salt),
                                             Crypto ] := aValue;
    except on E: Exception do
        _raise (['SetPOP3Host',ERR_TUSER_SET_POP3_HOST,E],
                ['{0F81EBF6-4F7F-4FF6-B088-61E0B7449307}']);
    end;
end;

function TUser.GetPOP3Port : WORD;
begin
    Result := 0;
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            Result := StrToInt ( PropAt [_usr_POP3_port].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                                                  User.Crypto ] )
        else if Assigned (Crypto) then
            Result := StrToInt ( PropAt [_usr_POP3_port].asSafe [ Crypto.Hash (Password,Salt),
                                                                  Crypto ] );
    except on E: Exception do
        _raise (['GetPOP3Port',ERR_TUSER_GET_POP3_PORT,E],
                ['{887C94B4-75A9-4823-BDB7-455CC24AD894}']);
    end;
end;

procedure TUser.SetPOP3Port (const aValue: WORD);
begin
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self ) then
            PropAt [_usr_POP3_port].asSafe [ User.Crypto.Hash (User.Password,User.Salt),
                                             User.Crypto ] := IntToStr (aValue)
        else if Assigned (Crypto) then
            PropAt [_usr_POP3_port].asSafe [ Crypto.Hash (Password,Salt),
                                             Crypto ] := IntToStr (aValue);
    except on E: Exception do
        _raise (['SetPOP3Port',ERR_TUSER_SET_POP3_PORT,E],
                ['{F8FA833C-DBA4-450D-8A86-0A399736CE82}']);
    end;
end;

function TUser.GetAutoTLS : Boolean;
begin
    Result := FALSE;
    try
        Result := PropAt [_usr_auto_tls].asBoolean;
    except on E: Exception do
        _raise (['GetAutoTLS',ERR_TUSER_GET_AUTO_TLS,E],
                ['{93F11D62-6FDA-46BA-9466-3123FE1AD8A0}']);
    end;
end;

procedure TUser.SetAutoTLS (const aValue: Boolean);
begin
    try
        PropAt [_usr_auto_tls].asBoolean := aValue;
    except on E: Exception do
        _raise (['SetAutoTLS',ERR_TUSER_SET_AUTO_TLS,E],
                ['{635A4BB3-CD04-478D-A790-12D570ACB3A9}']);
    end;
end;

function TUser.GetFullSSL : Boolean;
begin
    Result := FALSE;
    try
        Result := PropAt [_usr_full_ssl].asBoolean;
    except on E: Exception do
        _raise (['GetFullSSL',ERR_TUSER_GET_FULL_SSL,E],
                ['{A11B20F2-2BAF-4A47-BBF6-B1A0507A5DCD}']);
    end;
end;

procedure TUser.SetFullSSL (const aValue: Boolean);
begin
    try
        PropAt [_usr_full_ssl].asBoolean := aValue;
    except on E: Exception do
        _raise (['SetFullSSL',ERR_TUSER_SET_FULL_SSL,E],
                ['{D7520156-7BE4-4070-9302-09C44AFECF20}']);
    end;
end;

function TUser.CalcKeyHash : Hex;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self )
           and ( (IDOwner = 0) or (IDOwner = USER_ID) ) then
            Result := User.Crypto.Hash (PublicKey)
        else if Assigned (Crypto)
                and ( (IDOwner = 0) or (IDOwner = ID) ) then
            Result := Crypto.Hash (PublicKey)
        else
            Result := inherited CalcKeyHash;
    except on E: Exception do
        _raise (['CalcKeyHash',ERR_TUSER_CALC_KEY_HASH,E],
                ['{C1456E6F-C4A9-4E45-85CD-CA990C982371}']);
    end;
end;

function TUser.CalcDataHash : Hex;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto) and ( User <> Self )
           and ( (IDOwner = 0) or (IDOwner = USER_ID) ) then
            Result := User.Crypto.Hash ( Format ('%s_%s_%s_%s_%s_%d_%d_%s',
                                                 [ User.Crypto.Hash (PublicKey),
                                                   Login,
                                                   User.Crypto.Hash (Description),
                                                   EMail,
                                                   IP, Port,
                                                   DateTimeToUnix (Birthday),
                                                   HashPic ]) )
        else if Assigned (Crypto)
                and ( (IDOwner = 0) or (IDOwner = ID) ) then
            Result := Crypto.Hash ( Format ('%s_%s_%s_%s_%s_%d_%d_%s',
                                            [ Crypto.Hash (PublicKey),
                                              Login,
                                              Crypto.Hash (Description),
                                              EMail,
                                              IP, Port,
                                              DateTimeToUnix (Birthday),
                                              HashPic ]) )
        else
            Result := inherited CalcDataHash;
    except on E: Exception do
        _raise (['CalcDataHash',ERR_TUSER_CALC_DATA_HASH,E],
                ['{94B4E550-A916-4727-B1BA-06FAAF1820E7}']);
    end;
end;

class function TUser.CheckLogin (const aLogin: String;
                                 var Errors: String) : Boolean;
begin
    Result := FALSE;
    try
        try
            { длина }
            if not ( Length (aLogin) >= 3 ) then
                if ( Errors = '' ) then
                    Errors := Format (ERR_TUSER_LOGIN_LENGTH,[3])
                else
                    Errors := Errors + #13#10 + Format (ERR_TUSER_LOGIN_LENGTH,[3]);
        finally
            Result := ( Errors = '' );
        end;
    except on E: Exception do
        _raise (['CheckLogin',ERR_TUSER_CHECK,E],
                ['{514E108B-9C39-43AD-B80F-DA3BA2E4AC48}']);
    end;
end;

class function TUser.CheckPassword (const aPassword: String;
                                    var Errors: String) : Boolean;
const
{$IF     Defined (SMPL_PASSWORDS) and
     not Defined (NORM_PASSWORDS) and
     not Defined (DIFF_PASSWORDS) }
    MinLength   = 8;
    MinNumerics = 0;
    MinSigns    = 0;
    MinInverse  = 0.00;
{$IFEND}
{$IF not Defined (SMPL_PASSWORDS) and
         Defined (NORM_PASSWORDS) and
     not Defined (DIFF_PASSWORDS) }
    MinLength   = 12; 
    MinNumerics = 2;
    MinSigns    = 0;
    MinInverse  = 0.10;
{$IFEND}
{$IF not Defined (SMPL_PASSWORDS) and
     not Defined (NORM_PASSWORDS) and
         Defined (DIFF_PASSWORDS) }
    MinLength   = 16;
    MinNumerics = 4;
    MinSigns    = 1;
    MinInverse  = 0.25;
{$IFEND}
const
    Numerics    = '0123456789';
    Signs       = '~`!@#$%^&*+-=_|\\/()[]{}<>,.;:?\"\''';
    UpperChars  = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' +
                  'јЅ¬√ƒ≈®∆«»… ЋћЌќѕ–—“”‘’÷„ЎўЏџ№ЁЁёя';
    LowerChars  = 'abcdefghijklmnopqrstuvwxyz' +
                  'абвгдеЄжзийклмнопрстуфхцчшщъыьэю€';
var
    I               : Integer;
    J               : Integer;
    L               : Integer;
    NumericsCount   : Integer;
    SignsCount      : Integer;
    UpperCharsCount : Integer;
    LowerCharsCount : Integer;
    CasePercent     : Double;
begin
    Result := FALSE;
    try
        {$IFDEF CHECK_PASSWORDS}
        try
            { длина }
            L := Length (aPassword);
            if not ( L >= MinLength ) then
                if ( Errors = '' ) then
                    Errors := Format (ERR_TUSER_PASSWORD_LENGTH,[MinLength])
                else
                    Errors := Errors + #13#10 + Format (ERR_TUSER_PASSWORD_LENGTH,[MinLength]);

            NumericsCount := 0;
            SignsCount := 0;
            UpperCharsCount := 0;
            LowerCharsCount := 0;
            for I := 1 to L do
            begin
                for J := 1 to Length (Numerics) do
                begin
                   if ( aPassword [I] = Numerics [J] ) then
                       Inc (NumericsCount);
                end;
                for J := 1 to Length (Signs) do
                begin
                    if ( aPassword [I] = Signs [J] ) then
                        Inc (SignsCount);
                end;
                for J := 1 to Length (UpperChars) do
                begin
                    if ( aPassword [I] = UpperChars [J] ) then
                        Inc (UpperCharsCount);
                end;
                for J := 1 to Length (LowerChars) do
                begin
                    if ( aPassword [I] = LowerChars [J] ) then
                        Inc (LowerCharsCount);
                end;
            end;
            { количество цифр }
            if not ( NumericsCount >= MinNumerics ) then
                if ( Errors = '' ) then
                    Errors := Format (ERR_TUSER_PASSWORD_NUMERIC,[MinNumerics])
                else
                    Errors := Errors + #13#10 + Format (ERR_TUSER_PASSWORD_NUMERIC,[MinNumerics]);
            { спец. символы }
            if not ( SignsCount >= MinSigns ) then
                if ( Errors = '' ) then
                    Errors := Format (ERR_TUSER_PASSWORD_SIGN,[MinSigns])
                else
                    Errors := Errors + #13#10 + Format (ERR_TUSER_PASSWORD_SIGN,[MinSigns]);
            { буквы и регистр }
            if ( LowerCharsCount = 0 ) and ( UpperCharsCount = 0 ) then
                Errors := Errors + #13#10 + ERR_TUSER_PASSWORD_CHAR
            else
            begin
                CasePercent := 0;
                if ( LowerCharsCount > 0 ) and ( LowerCharsCount >= UpperCharsCount ) then
                    CasePercent := UpperCharsCount / LowerCharsCount
                else if ( UpperCharsCount > 0 ) then
                    CasePercent := LowerCharsCount / UpperCharsCount;
                if not ( CasePercent >= MinInverse ) then
                    Errors := Errors + #13#10 + Format (ERR_TUSER_PASSWORD_CASE,[MinInverse*100]);
            end;
        finally
            L := 0;
            NumericsCount := 0;
            SignsCount := 0;
            UpperCharsCount := 0;
            LowerCharsCount := 0;
            CasePercent := 0;

            Result := ( Errors = '' );
        end;
        {$ELSE}
        Result := TRUE;
        {$ENDIF CHECK_PASSWORDS}
    except on E: Exception do
        _raise (['CheckPassword',ERR_TUSER_CHECK,E],
                ['{D7C1D47E-42EC-4D07-84CA-F0EF56A1A34D}']);
    end;
end;

class function TUser.CheckEMail (const anEMail: String;
                                 var Errors: String) : Boolean;

    function CheckAllowed(const S: string): boolean;
    var
        I : Integer;
    begin
        Result := TRUE;
        for I := 1 to Length (S) do
        begin
            Result := Result and
                      ( S [I] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '-', '.'] );
            if not Result then
                Break;
        end;
    end;

var
    I          : Integer;
    NickName   : String;
    DomainName : String;
begin
    Result := FALSE;
    try
        I := Pos ('@',anEMail);
        if ( I = 0 ) then
            if ( Errors = '' ) then
                Errors := ERR_TUSER_EMAIL_DELIMITER
            else
                Errors := Errors + #13#10 + ERR_TUSER_EMAIL_DELIMITER;
        NickName   := Copy (anEMail,1,I-1);
        DomainName := Copy ( anEMail, I+1, Length (anEMail) );
        if ( Length (NickName) = 0 ) then
            if ( Errors = '' ) then
                Errors := ERR_TUSER_EMAIL_NICK
            else
                Errors := Errors + #13#10 + ERR_TUSER_EMAIL_NICK;
        { min: один символ перед точкой, точка и 2 символа после точки }
        if ( ( Length (DomainName) < 4) ) then
            if ( Errors = '' ) then
                Errors := ERR_TUSER_EMAIL_DOMAIN
            else
                Errors := Errors + #13#10 + ERR_TUSER_EMAIL_DOMAIN;
        I := LastPos('.',DomainName);
        if ( I = 0 ) then
            if ( Errors = '' ) then
                Errors := ERR_TUSER_EMAIL_DOMAIN
            else
                Errors := Errors + #13#10 + ERR_TUSER_EMAIL_DOMAIN;
        { min: два символа после точки }
        if ( I > Length (DomainName) - 2 ) then
            if ( Errors = '' ) then
                Errors := ERR_TUSER_EMAIL_DOMAIN
            else
                Errors := Errors + #13#10 + ERR_TUSER_EMAIL_DOMAIN;
        if not CheckAllowed (NickName) then
            if ( Errors = '' ) then
                Errors := ERR_TUSER_EMAIL_DOMAIN
            else
                Errors := Errors + #13#10 + ERR_TUSER_EMAIL_NICK;
        if not CheckAllowed (DomainName) then
            if ( Errors = '' ) then
                Errors := ERR_TUSER_EMAIL_DOMAIN
            else
                Errors := Errors + #13#10 + ERR_TUSER_EMAIL_DOMAIN;
        Result := ( Errors = '' );
    except on E: Exception do
        _raise (['CheckEMail',ERR_TUSER_CHECK,E],
                ['{FEE2C98B-D3E1-4EBD-AA44-F5D30D4D59B8}']);
    end;
end;

class function TUser.CheckIP (const anIP: String;
                              var Errors: String) : Boolean;
begin
    Result := FALSE;
    try
        if not isIPAddress (anIP) then
            if ( Errors = '' ) then
                Errors := ERR_TUSER_INVALID_IP
            else
                Errors := Errors + #13#10 + ERR_TUSER_INVALID_IP;
        Result := ( Errors = '' );
    except on E: Exception do
        _raise (['CheckIP',ERR_TUSER_CHECK,E],
                ['{71977F31-FEBA-431E-80C1-0015E5EB07EE}']);
    end;
end;

procedure TUser.LoadFromStream (const aStream: TStream);
var
    PicStream : TStream;
begin
    try
        if not Assigned (aStream) then
            raise Exception.Create (ERR_TUSER_INVALID_STREAM);
        inherited LoadFromStream (aStream);
        Mode := Mode - [objImported];
        with aStream do
        begin
            Crypto.genRandom := ReadStrL (aStream);
            Crypto.algAsymmetric := ReadStrL (aStream);
            Crypto.algSymmetric := ReadStrL (aStream);
            Crypto.modeSymmetric := ReadStrL (aStream);
            Crypto.algHash := ReadStrL (aStream);
            PublicKey := ReadStrL (aStream);
            EMail := HexToStr ( ReadStrL (aStream) );
            IP := HexToStr ( ReadStrL (aStream) );
            Port := StrToInt (  HexToStr ( ReadStrL (aStream) )  );
            Description := Decompress (  HexToStr ( ReadStrL (aStream) )  );
            Sex := HexToInt ( ReadStrL (aStream) );
            Birthday := _StrToDateTime (  HexToStr ( ReadStrL (aStream) )  );
            HashPic := ReadStrL (aStream);
            PicStream := TMemoryStream.Create;
            try
                ReadStreamL (aStream,PicStream);
                Pic.LoadFromStream (PicStream);
            finally
                FreeAndNil (PicStream);
            end;
        end;
        Mode := Mode + [objImported];
    except on E: Exception do
        _raise (['LoadFromStream',ERR_TUSER_LOAD_FROM_STREAM,E],
                ['{45F8AEA7-CB04-4176-AAB8-E35F349ED774}']);
    end;
end;

procedure TUser.SaveToStream (out aStream: TStream);
var
    PicStream : TStream;
begin
    try
        if not Assigned (aStream) then
            raise Exception.Create (ERR_TUSER_INVALID_STREAM);
        inherited SaveToStream (aStream);
        Mode := Mode - [objExported];
        with aStream do
        begin
            WriteStrL (aStream,Crypto.genRandom);
            WriteStrL (aStream,Crypto.algAsymmetric);
            WriteStrL (aStream,Crypto.algSymmetric);
            WriteStrL (aStream,Crypto.modeSymmetric);
            WriteStrL (aStream,Crypto.algHash);
            WriteStrL (aStream,PublicKey);
            WriteStrL ( aStream, StrToHex (EMail) );
            WriteStrL ( aStream, StrToHex (IP) );
            WriteStrL (  aStream, StrToHex ( IntToStr (Port) )  );
            WriteStrL (  aStream, StrToHex ( Compress (Description) )  );
            WriteStrL ( aStream, IntToHex (Sex,2) );
            WriteStrL (  aStream, StrToHex ( _DateTimeToStr (Birthday) )  );
            WriteStrL (aStream,HashPic);
            PicStream := TMemoryStream.Create;
            try
                Pic.SaveToStream (PicStream);
                WriteStreamL (aStream,PicStream);
            finally
                FreeAndNil (PicStream);
            end;
        end;
        Mode := Mode + [objExported];
    except on E: Exception do
        _raise (['SaveToStream',ERR_TUSER_SAVE_TO_STREAM,E],
                ['{3455E82E-B897-4180-8142-714724E98C19}']);
    end;
end;
