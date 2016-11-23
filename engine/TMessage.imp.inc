{******************************************************************************}
{*  TMessage.imp.inc                                                          *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TMessage }
class function TMessage.GetClassName : String;
begin
    Result := CLS_TMESSAGE_NAME;
end;

class function TMessage.GetClassVersion : TVersionInfo;
begin
    Result := inherited GetClassVersion;
end;

class function TMessage.GetClassTable : String;
begin
    Result := 'messages';
end;

class function TMessage.GetClassProperties : TProtoProperties;
begin
    Result := NIL;
    try
        Result := inherited GetClassProperties;
        with Result do
        begin
            Register ( TMetaProperty, ['hash_categorie',PRP_TMESSAGE_HASH_CATEGORIE,dtHex,''],[prpStored] );
            Register ( TMetaProperty, ['id_categorie',PRP_TMESSAGE_ID_CATEGORIE,dtID,0],[prpStored] );
            Register ( TMetaProperty, ['hash_author',PRP_TMESSAGE_HASH_AUTHOR,dtHex,''],[prpStored] );
            Register ( TMetaProperty, ['id_author',PRP_TMESSAGE_ID_AUTHOR,dtID,0],[prpStored] );
            Register ( TMetaProperty, ['id_owner',PRP_TMESSAGE_ID_OWNER,dtID,0],[prpStored] );
            Register ( TMetaProperty, ['id_type',PRP_TMESSAGE_ID_TYPE,dtID,0],[prpStored] );
            Register ( TMetaProperty, ['id_status',PRP_TMESSAGE_ID_STATUS,dtID,0],[prpStored] );
            Register ( TMetaProperty, ['subject',PRP_TMESSAGE_SUBJECT,dtSafe,''],[prpStored] );
            if ( ItemAt [_msg_subject].DataType <> dtSafe ) then
                raise Exception.Create ('The subject was not encrypted. Hacking attempt!');
            Register ( TMetaProperty, ['text',PRP_TMESSAGE_TEXT,dtSafe,''],[prpStored] );
            if ( ItemAt [_msg_text].DataType <> dtSafe ) then
                raise Exception.Create ('The text was not encrypted. Hacking attempt!');
            Register ( TMetaProperty, ['hash_pic',PRP_TMESSAGE_HASH_PIC,dtHex,''],[prpStored] );
            Register ( TMetaProperty, ['id_pic',PRP_TMESSAGE_ID_PIC,dtID,0],[prpStored] );
            Register ( TMetaProperty, ['index_pic',PRP_TMESSAGE_INDEX_PIC,dtInteger,0],[prpStored] );
            Register ( TMetaProperty, ['salt',PRP_TMESSAGE_SALT,dtHex,''],[prpStored] );
        end;
    except on E: Exception do
        _raise (['GetClassProperties',ERR_TMESSAGE_GET_ClASS_PROPERTIES,E],
                ['{7E43839F-F84A-408C-A79F-2E9425FBF44F}']);
    end;
end;

class function TMessage.GetClassList : CProtoObjects;
begin
    Result := TMessages;
end;

constructor TMessage.Create (const aDB: TSQLiteDatabase;
                             anArgs: array of const;
                             aMode: TObjectMode = []);
begin
    try
        inherited Create (aDB,[],aMode);
        Mode := Mode - [objCreated];
        { первый параметр - идентификатор }
        if notEmpty (0,anArgs) then
            ID := toInt64 (anArgs [0]);
        { второй параметр - идентификатор категории }
        f_Categorie := TCategorie.Create (aDB,[],aMode+[objSimple]);
        if notEmpty (1,anArgs) then
            IDCategorie := toInt64 (anArgs [1]);
        { третий параметр - идентификатор автора }
        f_Author := TUser.Create (aDB,[],aMode+[objSimple]);
        if notEmpty (2,anArgs) then
            IDAuthor := toInt64 (anArgs [2]);
        { четвертый параметр - идентификатор владельца }
        if notEmpty (3,anArgs) then
            IDOwner := toInt64 (anArgs [3]);
        if Assigned (User) and ( IDOwner = 0 ) then
            IDOwner := USER_ID;
        { пятый параметр - идентификатор типа }
        f_MsgType := TMessageType.Create (aDB,[],aMode);
        if notEmpty (4,anArgs) then
            IDType := toInt64 (anArgs [4]);
        { шестой параметр - идентификатор статуса }
        f_MsgStatus := TMessageStatus.Create (aDB,[],aMode);
        if notEmpty (5,anArgs) then
            IDStatus := toInt64 (anArgs [5]);
        { седьмой параметр - идентификатор иконки }
        f_Pic := TPic.Create (aDB,[],aMode);
        if notEmpty (6,anArgs) then
            IDPic := toInt64 (anArgs [6]);
        { восьмой параметр - индекс стандартной иконки }
        if notEmpty (7,anArgs) then
            IndexPic := toInt64 (anArgs [7]);
        { девятый параметр - дата и время создания }
        if notEmpty (8,anArgs) then
            TimeStampCreate := toDateTime (anArgs [8]);
        { десятый параметр - дата и время последнего редактирования }
        if notEmpty (9,anArgs) then
            TimeStampModify := toDateTime (anArgs [9]);
        { одиннадцатый параметр - дата и время публикации }
        if notEmpty (10,anArgs) then
            TimeStampPublic := toDateTime (anArgs [10]);
        { двенадцатый параметр - версия }
        if notEmpty (11,anArgs) then
            Version := toVersionInfo (anArgs [11]);
        { генерируем соль }
        if Assigned (User) and Assigned (User.Crypto) then
            Salt := User.Crypto.GenerateKey;
        Mode := Mode + [objCreated];
    except on E: Exception do begin
        Mode := Mode + [objCreateFailed];
        _raise (['Create',ERR_TMESSAGE_CREATE,E],
                ['{46932062-EF62-4B26-9F12-75ED0EB723CA}']);
    end; end;
end;

destructor TMessage.Destroy;
begin
    try
        FreeAndNil (f_Categorie);
        if ( Author <> User ) then
            FreeAndNil (f_Author);
        FreeAndNil (f_MsgType);
        FreeAndNil (f_MsgStatus);
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TMESSAGE_DESTROY,E],
                ['{304576BB-CA25-4EB8-96BC-188575EB1D57}']);
    end;
end;

class procedure TMessage.CreateClassTable (const aDB: TSQLiteDatabase);
var
    Props    : TProtoProperties;
    KwdProps : TProtoProperties;
    query    : String;
begin
    try
        inherited CreateClassTable (aDB);
        {$IFDEF DELETE_TRIGGER}
        { создаем триггер на удаление }
        if not aDB.TriggerExists ( Format ('trigger_%s_%s',[GetClassTable,'delete']) ) then
        begin
            Props := GetClassProperties;
            KwdProps := TKeyWord.GetClassProperties;
            try
                { создаем триггер }
                query := Format ('CREATE TRIGGER trigger_%s_%s '+
                                 'BEFORE DELETE '+
                                 'ON %s '+
                                 'FOR EACH ROW '+
                                 'BEGIN '+
                                     // delete key words
                                     'DELETE '+
                                     'FROM %s '+
                                     'WHERE %s = OLD.%s; '+
                                 'END ',
                                 [ GetClassTable, 'delete',
                                   GetClassTable,
                                   // delete key words
                                   TKeyWord.GetClassTable,
                                   KwdProps.ItemAt [_kwd_id_message].Name, Props.ItemAt [_msg_id].Name ]);
                // ShowMessage (query);
                aDB.ExecSQL (query);
            finally
                KwdProps.FreeAll;
                FreeAndNil (KwdProps);
                Props.FreeAll;
                FreeAndNil (Props);
            end;
        end;
        {$ENDIF DELETE_TRIGGER}
    except on E: Exception do
        _raise (['CreateClassTable',ERR_TMESSAGE_CREATE_CLASS_TABLE,E],
                ['{7E897789-E173-40F4-89BD-FBAA9065EE85}']);
    end;
end;

procedure TMessage.SetDB (const aDB: TSQLiteDatabase);
begin
    try
        inherited SetDB (aDB);
        if Assigned (f_Categorie) then
            f_Categorie.DB := aDB;
        if Assigned (f_Author) then
            f_Author.DB := aDB;
        if Assigned (f_MsgType) then
            f_MsgType.DB := aDB;
        if Assigned (f_MsgStatus) then
            f_MsgStatus.DB := aDB;
        if Assigned (f_Pic) then
            f_Pic.DB := aDB;
    except on E: Exception do
        _raise (['SetDB',ERR_TMESSAGE_SET_DB,E],
                ['{25EA5365-F5F7-4E72-A96D-E4EF32C78DFB}']);
    end;
end;

function TMessage.Check : Boolean;
begin
    Result := TRUE;
    try
        Result := inherited Check;
        if Result then
        begin
            {if not ( IDCategorie > 0 ) then
                Error (ERR_TMESSAGE_INVALID_ID_CATEGORIE);}
            {if not ( IDAuthor > 0 ) then
                Error (ERR_TMESSAGE_INVALID_ID_AUTHOR);}
            if not ( IDType > 0 ) then
                Error (ERR_TMESSAGE_INVALID_ID_TYPE);
            if not ( IDStatus > 0 ) then
                Error (ERR_TMESSAGE_INVALID_ID_STATUS);
            {if not ( IDPic > 0 ) then
                Error (ERR_TMESSAGE_INVALID_ID_PIC);}
            Result := ( Errors.Count = 0 );
        end;
    except on E: Exception do
        _raise (['Check',ERR_TMESSAGE_CHECK,E],
                ['{9344213F-975D-4D52-B7E7-F18F529DB611}']);
    end;
end;

procedure TMessage.Load;
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
        { categorie }
        if Assigned (Categorie) then
        begin
            Categorie.ID := PropAt [_msg_id_categorie].asID;
            if ( IDCategorie > 0 ) then
                Categorie.Load;
        end
        else if ( PropAt [_msg_id_categorie].asID > 0 ) then
            Categorie := TCategorie.Load (DB,PropAt [_msg_id_categorie].asID,[objSimple]) as TCategorie
        else
            Categorie := TCategorie.Create (DB,[],[objSimple]);
        { author }
        if Assigned (Author) then
        begin
            Author.ID := PropAt [_msg_id_author].asID;
            if ( IDAuthor > 0 ) then
                Author.Load;
        end
        else if ( PropAt [_msg_id_author].asID > 0 ) then
            Author := TUser.Load (DB,PropAt [_msg_id_author].asID,[objSimple]) as TUser
        else
            Author := TUser.Create (DB,[],[objSimple]);
        { type }
        if Assigned (MsgType) then
        begin
            MsgType.ID := PropAt [_msg_id_type].asID;
            if ( IDType > 0 ) then
                MsgType.Load;
        end
        else if ( PropAt [_msg_id_type].asID > 0 ) then
            MsgType := TMessageType.Load (DB,PropAt [_msg_id_type].asID) as TMessageType
        else
            MsgType := TMessageType.Create (DB,[]);
        { status }
        if Assigned (MsgStatus) then
        begin
            MsgStatus.ID := PropAt [_msg_id_status].asID;
            if ( IDStatus > 0 ) then
                MsgStatus.Load;
        end
        else if ( PropAt [_msg_id_status].asID > 0 ) then
            MsgStatus := TMessageStatus.Load (DB,PropAt [_msg_id_status].asID) as TMessageStatus
        else
            MsgStatus := TMessageStatus.Create (DB,[]);
        { pic }
        if Assigned (Pic) then
        begin
            Pic.ID := PropAt [_msg_id_pic].asID;
            if ( IDPic > 0 ) then
                Pic.Load;
        end
        else if ( PropAt [_msg_id_pic].asID > 0 ) then
            Pic := TPic.Load (DB,PropAt [_msg_id_pic].asID) as TPic
        else
            Pic := TPic.Create (DB,[]);
        Mode := Mode + [objLoaded];
    except on E: Exception do begin
        Mode := Mode + [objLoadFailed];
        _raise (['Load',ERR_TMESSAGE_LOAD,E],
                ['{7D1944C7-7774-4A92-8ECB-B47D4D6E57AC}']);
    end; end;
end;

procedure TMessage.Save;
var
    isBeforeTransactionOpen : Boolean;
    KeyWords                : TKeyWords;
    KeyWord                 : TKeyWord;
    Words                   : TStringList;
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
            { categorie }
            PropAt [_msg_hash_categorie].asHex := HashCategorie;
            { author }
            PropAt [_msg_hash_author].asHex := HashAuthor;
            { pic }
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
            PropAt [_msg_id_pic].asID := Pic.ID;
            PropAt [_msg_hash_pic].asHex := HashPic;

            inherited Save;
            { извлекаем ключевые слова }
            Words := TStringList.Create;
            try
                GetKeyWords (Text,Words);
                TKeyWords.Delete (DB,[ NIL, NIL, NIL, _([ ID ]) ]);
                for I := 0 to Words.Count - 1 do
                try
                    KeyWord := TKeyWord.Create (DB,[ 0, Words [I], 0, ID, IDAuthor, IDOwner ]);
                    KeyWord.Save;
                finally
                    FreeAndNil (KeyWord);
                end;
            finally
                FreeAndNil (Words);
            end;
            { привязываем все ключевые слова без идентификатора сообщения,
              у которых хэш-ключ сообщения - это хэш-ключ данной категории }
            KeyWords := TKeyWords.Load (DB,[ NIL,
                                             NIL,
                                             NIL,
                                             _([0]),
                                             NIL,
                                             _([IDOwner]) ],
                                           [],
                                           [],
                                           [objSimple]) as TKeyWords;
            try
                for I := 0 to KeyWords.Count - 1 do
                    if ( KeyWords.ItemAt [I].HashMessage = KeyHash ) then
                    begin
                        KeyWords.ItemAt [I].IDMessage := ID;
                        KeyWords.ItemAt [I].Save;
                    end;
            finally
                FreeAndNil (KeyWords);
            end;
            { если ошибок не обнаружено - проводим транзакцию }
            if ( not isBeforeTransactionOpen ) then
                DB.Commit;
        { в случае возникновения ошибки - откатываем транзакцию }
        except on E: Exception do begin
            if ( not isBeforeTransactionOpen ) then
                DB.Rollback;
            raise Exception.CreateFmt (ERR_TMESSAGE_TRANSACTION,[E.Message]);
        end; end;;
    except on E: Exception do begin
        Mode := Mode + [objSaveFailed];
        _raise (['Save',ERR_TMESSAGE_SAVE,E],
                ['{F9637A76-98BC-4D7A-9611-560A4C2F8396}']);
    end; end;
end;

{$IFNDEF DELETE_TRIGGER}
procedure TMessage.Delete;
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
            { удаляем сообщение }
            inherited Delete;
            Mode := Mode - [objDeleted];
            { удаляем все ключевые слова для данного сообщения }
            TKeyWords.Delete ( DB, [ NIL, NIL, NIL, _([ID]) ] );
            { если ошибок не обнаружено - проводим транзакцию }
            if ( not isBeforeTransactionOpen ) then
                DB.Commit;
        { в случае возникновения ошибки - откатываем транзакцию }
        except on E: Exception do begin
            if ( not isBeforeTransactionOpen ) then
                DB.Rollback;
            raise Exception.CreateFmt (ERR_TMESSAGE_TRANSACTION,[E.Message]);
        end; end;
        Mode := Mode + [objDeleted];
    except on E: Exception do begin
        Mode := Mode + [objDeleteFailed];
        _raise (['Delete',ERR_TMESSAGE_DELETE,E],
                ['{399A3ACC-3B64-4621-A45C-369689E9B825}']);
    end; end;
end;
{$ENDIF DELETE_TRIGGER}

function TMessage.GetHashCategorie : Hex;
begin
    Result := '';
    try
        if notEmpty ( PropAt [_msg_hash_categorie].asHex ) then
            Result := PropAt [_msg_hash_categorie].asHex
        else if Assigned (Categorie) and ( Categorie.ID > 0 ) then
        begin
            if not ( objLoaded in Categorie.Mode ) then
                PropAt [_msg_hash_categorie].asHex := TCategorie.GetHash (DB,IDCategorie)
            else
                PropAt [_msg_hash_categorie].asHex := Categorie.KeyHash;
            Result := PropAt [_msg_hash_categorie].asHex;
        end;
    except on E: Exception do
        _raise (['GetHashCategorie',ERR_TMESSAGE_GET_HASH_CATEGORIE,E],
                ['{05CEC14B-70C9-4535-AFBF-C7F24BFCF389}']);
    end;
end;

procedure TMessage.SetHashCategorie (const aValue: Hex);
begin
    try
        if isHex (aValue) then
        begin
            PropAt [_msg_hash_categorie].asHex := aValue;
            IDCategorie := TCategories.Find (DB,aValue,IDOwner);
        end
        else
            PropAt [_msg_hash_categorie].asHex := '';
    except on E: Exception do
        _raise (['SetHashCategorie',ERR_TMESSAGE_SET_HASH_CATEGORIE,E],
                ['{303BFB7B-2B44-42C5-904C-BDCA6D907A98}']);
    end;
end;

function TMessage.GetIDCategorie : TID;
begin
    Result := 0;
    try
        Result := PropAt [_msg_id_categorie].asID;
    except on E: Exception do
        _raise (['GetIDCategorie',ERR_TMESSAGE_GET_ID_CATEGORIE,E],
                ['{0582532C-CDE9-4351-AF4D-D6F69E0745AB}']);
    end;
end;

procedure TMessage.SetIDCategorie (const aValue: TID);
begin
    try
        if ( aValue >= 0 ) then
            PropAt [_msg_id_categorie].asID := aValue
        else
            PropAt [_msg_id_categorie].asID := 0;
        if Assigned (Categorie) and ( Categorie.ID <> IDCategorie ) then
            Categorie.ID := IDCategorie
        else if not Assigned (Categorie) then
            Categorie := TCategorie.Create (DB,[IDCategorie],[objSimple]);
    except on E: Exception do
        _raise (['SetIDCategorie',ERR_TMESSAGE_SET_ID_CATEGORIE,E],
                ['{ED28BF5E-9C0D-4D4B-BC0C-83AC76A1B5B5}']);
    end;
end;

function TMessage.GetCategorie : TCategorie;
begin
    Result := NIL;
    try
        Result := f_Categorie;
    except on E: Exception do
        _raise (['GetCategorie',ERR_TMESSAGE_GET_CATEGORIE,E],
                ['{3661213D-B3D9-49DC-8555-980754C08F38}']);
    end;
end;

procedure TMessage.SetCategorie (const aValue: TCategorie);
begin
    try
        f_Categorie := aValue;
        if Assigned (Categorie) and ( Categorie.ID <> IDCategorie ) then
            IDCategorie := Categorie.ID;
    except on E: Exception do
        _raise (['SetCategorie',ERR_TMESSAGE_SET_CATEGORIE,E],
                ['{35B678F6-55C0-40A1-BE11-36D8F5F8BFC4}']);
    end;
end;

function TMessage.GetHashAuthor : Hex;
begin
    Result := '';
    try
        if notEmpty ( PropAt [_msg_hash_author].asHex ) then
            Result := PropAt [_msg_hash_author].asHex
        else if Assigned (Author) and ( Author.ID > 0 ) then
        begin
            if not ( objLoaded in Author.Mode ) then
                PropAt [_msg_hash_author].asHex := TUser.GetHash (DB,IDAuthor)
            else
                PropAt [_msg_hash_author].asHex := Author.KeyHash;
            Result := PropAt [_msg_hash_author].asHex;
        end;
    except on E: Exception do
        _raise (['GetHashAuthor',ERR_TMESSAGE_GET_HASH_AUTHOR,E],
                ['{3998FA86-4341-42CE-A864-40ADC2CB5828}']);
    end;
end;

procedure TMessage.SetHashAuthor (const aValue: Hex);
begin
    try
        if isHex (aValue) then
        begin
            PropAt [_msg_hash_author].asHex := aValue;
            if ( aValue = USER_KEY_HASH ) then
                IDAuthor := USER_ID
            else
                IDAuthor := TUsers.Find (DB,aValue,IDOwner);
        end
        else
            PropAt [_msg_hash_author].asHex := '';
    except on E: Exception do
        _raise (['SetHashAuthor',ERR_TMESSAGE_SET_HASH_AUTHOR,E],
                ['{FB53F912-C8AD-403E-8939-C18F63B03FB1}']);
    end;
end;

function TMessage.GetIDAuthor : TID;
begin
    Result := 0;
    try
        Result := PropAt [_msg_id_author].asID;
    except on E: Exception do
        _raise (['GetIDAuthor',ERR_TMESSAGE_GET_ID_AUTHOR,E],
                ['{A446B604-3DD2-463C-B27F-2C0ED1533E8F}']);
    end;
end;

procedure TMessage.SetIDAuthor (const aValue: TID);
begin
    try
        if ( aValue >= 0 ) then
            PropAt [_msg_id_author].asID := aValue
        else
            PropAt [_msg_id_author].asID := 0;
        if Assigned (Author) and ( Author.ID <> IDAuthor ) then
            Author.ID := IDAuthor
        else if not Assigned (Author) then
            Author := TUser.Create (DB,[IDAuthor],[objSimple]);
    except on E: Exception do
        _raise (['SetIDAuthor',ERR_TMESSAGE_SET_ID_AUTHOR,E],
                ['{4BEDC9CD-49D1-4194-9FA2-4421A3A92BE5}']);
    end;
end;

function TMessage.GetAuthor : TUser;
begin
    Result := NIL;
    try
        Result := f_Author;
    except on E: Exception do
        _raise (['GetAuthor',ERR_TMESSAGE_GET_AUTHOR,E],
                ['{BAFB5FB4-1FFF-42B5-8AFD-5DD1750C3C76}']);
    end;
end;

procedure TMessage.SetAuthor (const aValue: TUser);
begin
    try
        if Assigned (Author) and ( Author <> aValue ) and ( Author <> User ) then
            FreeAndNil (f_Author);
        f_Author := aValue;
        if Assigned (Author) and ( Author.ID <> IDAuthor ) then
            IDAuthor := Author.ID;
    except on E: Exception do
        _raise (['SetAuthor',ERR_TMESSAGE_SET_AUTHOR,E],
                ['{1403333A-CB61-4157-A29A-D5EE70B307FE}']);
    end;
end;

function TMessage.GetIDOwner : TID;
begin
    Result := 0;
    try
        Result := PropAt [_msg_id_owner].asID;
    except on E: Exception do
        _raise (['GetIDOwner',ERR_TMESSAGE_GET_ID_OWNER,E],
                ['{3E39E255-769F-4AA3-B774-3D777956850F}']);
    end;
end;

procedure TMessage.SetIDOwner (const aValue: TID);
begin
    try
        if ( aValue >= 0 ) then
            PropAt [_msg_id_owner].asID := aValue
        else
            PropAt [_msg_id_owner].asID := 0;
    except on E: Exception do
        _raise (['SetIDOwner',ERR_TMESSAGE_SET_ID_OWNER,E],
                ['{A4DFE2B8-35C1-4786-B3AA-DD2ADB2A357A}']);
    end;
end;

function TMessage.GetIDType : TID;
begin
    Result := 0;
    try
        Result := PropAt [_msg_id_type].asID;
    except on E: Exception do
        _raise (['GetIDType',ERR_TMESSAGE_GET_ID_TYPE,E],
                ['{84D1CFD3-3428-4388-9C52-E23ECD0235E9}']);
    end;
end;

procedure TMessage.SetIDType (const aValue: TID);
begin
    try
        if ( aValue >= 0 ) then
            PropAt [_msg_id_type].asID := aValue
        else
            PropAt [_msg_id_type].asID := 0;
        if Assigned (MsgType) and ( MsgType.ID <> IDType ) then
            MsgType.ID := IDType
        else if not Assigned (MsgType) then
            MsgType := TMessageType.Create (DB,[IDType]);
    except on E: Exception do
        _raise (['SetIDType',ERR_TMESSAGE_SET_ID_TYPE,E],
                ['{63D75E49-38CE-4650-A340-A3E75C67D068}']);
    end;
end;

function TMessage.GetMsgType : TMessageType;
begin
    Result := NIL;
    try
        Result := f_MsgType;
    except on E: Exception do
        _raise (['GetMsgType',ERR_TMESSAGE_GET_TYPE,E],
                ['{036FA0D1-7053-4DDF-87AC-63DC4D6307FA}']);
    end;
end;

procedure TMessage.SetMsgType (const aValue: TMessageType);
begin
    try
        f_MsgType := aValue;
        if Assigned (MsgType) and ( MsgType.ID <> IDType ) then
            IDType := MsgType.ID;
    except on E: Exception do
        _raise (['SetMsgType',ERR_TMESSAGE_SET_TYPE,E],
                ['{3E33A173-2001-4FAE-B2AF-0897CD18158F}']);
    end;
end;

function TMessage.GetIDStatus : TID;
begin
    Result := 0;
    try
        Result := PropAt [_msg_id_status].asID;
    except on E: Exception do
        _raise (['GetIDStatus',ERR_TMESSAGE_GET_ID_STATUS,E],
                ['{23F9DA36-BD7A-47CA-865E-F962C9A46C9C}']);
    end;
end;

procedure TMessage.SetIDStatus (const aValue: TID);
begin
    try
        if ( aValue >= 0 ) then
            PropAt [_msg_id_status].asID := aValue
        else
            PropAt [_msg_id_status].asID := 0;
        if Assigned (MsgStatus) and ( MsgStatus.ID <> IDStatus ) then
            MsgStatus.ID := IDStatus
        else if not Assigned (MsgStatus) then
            MsgStatus := TMessageStatus.Create (DB,[IDStatus]);
    except on E: Exception do
        _raise (['SetIDStatus',ERR_TMESSAGE_SET_ID_STATUS,E],
                ['{177EB5D4-A637-4E9E-B792-26A3141A9909}']);
    end;
end;

function TMessage.GetMsgStatus : TMessageStatus;
begin
    Result := NIL;
    try
        Result := f_MsgStatus;
    except on E: Exception do
        _raise (['GetMsgStatus',ERR_TMESSAGE_GET_STATUS,E],
                ['{13D5FCC2-E7F8-499F-94F9-035CE6522A78}']);
    end;
end;

procedure TMessage.SetMsgStatus (const aValue: TMessageStatus);
begin
    try
        f_MsgStatus := aValue;
        if Assigned (MsgStatus) and ( MsgStatus.ID <> IDStatus ) then
            IDStatus := MsgStatus.ID;
    except on E: Exception do
        _raise (['SetMsgStatus',ERR_TMESSAGE_SET_STATUS,E],
                ['{68CA364B-601D-467F-9154-7E4BA4F336E1}']);
    end;
end;

function TMessage.GetSubject : String;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto) then
            Result := PropAt [_msg_subject].asSafe [ User.Crypto.Hash (User.Password,Salt),
                                                     User.Crypto ];
    except on E: Exception do
        _raise (['GetSubject',ERR_TMESSAGE_GET_SUBJECT,E],
                ['{D4040582-0177-45BD-A95C-9F893AAA2DBF}']);
    end;
end;

procedure TMessage.SetSubject (const aValue: String);
begin
    try
        if Assigned (User) and Assigned (User.Crypto) then
            PropAt [_msg_subject].asSafe [ User.Crypto.Hash (User.Password,Salt),
                                           User.Crypto ] := aValue;
    except on E: Exception do
        _raise (['SetSubject',ERR_TMESSAGE_SET_SUBJECT,E],
                ['{DB8D4BE3-1D56-48D2-8726-965A404FF7A2}']);
    end;
end;

function TMessage.GetText : String;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto) then
            Result := Decompress ( PropAt [_msg_text].asSafe [ User.Crypto.Hash (User.Password,Salt),
                                                               User.Crypto ] );
    except on E: Exception do
        _raise (['GetText',ERR_TMESSAGE_GET_TEXT,E],
                ['{E6D16339-CB6B-4D33-92F2-57161F8017E5}']);
    end;
end;

procedure TMessage.SetText (const aValue: String);
begin
    try
        if Assigned (User) and Assigned (User.Crypto) then
            PropAt [_msg_text].asSafe [ User.Crypto.Hash (User.Password,Salt),
                                        User.Crypto ] := Compress (aValue);
    except on E: Exception do
        _raise (['SetText',ERR_TMESSAGE_SET_TEXT,E],
                ['{04D14389-2862-454F-8E6A-996930E451DB}']);
    end;
end;

function TMessage.GetHashPic : Hex;
begin
    Result := '';
    try
        if notEmpty ( PropAt [_msg_hash_pic].asHex ) then
            Result := PropAt [_msg_hash_pic].asHex
        else if Assigned (Pic) and ( Pic.ID > 0 ) then
        begin
            if not ( objLoaded in Pic.Mode ) then
                PropAt [_msg_hash_pic].asHex := TPic.GetHash (DB,IDPic)
            else
                PropAt [_msg_hash_pic].asHex := Pic.KeyHash;
            Result := PropAt [_msg_hash_pic].asHex;
        end;
    except on E: Exception do
        _raise (['GetHashPic',ERR_TMESSAGE_GET_HASH_PIC,E],
                ['{E28A706A-D074-414F-B11F-F734AEBCE629}']);
    end;
end;

procedure TMessage.SetHashPic (const aValue: Hex);
begin
    try
        if isHex (aValue) then
            PropAt [_msg_hash_pic].asHex := aValue
        else
            PropAt [_msg_hash_pic].asHex := '';
    except on E: Exception do
        _raise (['SetHashPic',ERR_TMESSAGE_SET_HASH_PIC,E],
                ['{495E2C89-62E6-4697-B17C-A55AB56A0305}']);
    end;
end;

function TMessage.GetIDPic : TID;
begin
    Result := 0;
    try
        Result := PropAt [_msg_id_pic].asID;
    except on E: Exception do
        _raise (['GetIDPic',ERR_TMESSAGE_GET_ID_PIC,E],
                ['{11C409DE-FD48-42D8-B451-4C6CC0BF7CB2}']);
    end;
end;

procedure TMessage.SetIDPic (const aValue: TID);
begin
    try
        if ( aValue >= 0 ) then
            PropAt [_msg_id_pic].asID := aValue
        else
            PropAt [_msg_id_pic].asID := 0;
        if Assigned (Pic) and ( Pic.ID <> IDPic ) then
            Pic.ID := IDPic
        else if not Assigned (Pic) then
            Pic := TPic.Create (DB,[IDPic]);
    except on E: Exception do
        _raise (['SetIDPic',ERR_TMESSAGE_SET_ID_PIC,E],
                ['{4139C162-D1EE-4AB4-95FA-A1044A673D20}']);
    end;
end;

function TMessage.GetPic : TPic;
begin
    Result := NIL;
    try
        Result := f_Pic;
    except on E: Exception do
        _raise (['GetPic',ERR_TMESSAGE_GET_PIC,E],
                ['{B3892533-C204-40B4-A6B8-4B9BF4F49AA6}']);
    end;
end;

procedure TMessage.SetPic (const aValue: TPic);
begin
    try
        f_Pic := aValue;
        if Assigned (Pic) and ( Pic.ID <> IDPic ) then
            IDPic := Pic.ID;
    except on E: Exception do
        _raise (['SetPic',ERR_TMESSAGE_SET_PIC,E],
                ['{6E2C5DCD-E7F2-4632-B921-C116C9E20C93}']);
    end;
end;

function TMessage.GetIndexPic : Integer;
begin
    Result := -1;
    try
        Result := PropAt [_msg_index_pic].asInteger;
    except on E: Exception do
        _raise (['GetIndexPic',ERR_TMESSAGE_GET_INDEX_PIC,E],
                ['{7EA9B8C7-D5B6-404B-BCC9-BCEEC61A1F29}']);
    end;
end;

procedure TMessage.SetIndexPic (const aValue: Integer);
begin
    try
        PropAt [_msg_index_pic].asInteger := aValue;
    except on E: Exception do
        _raise (['SetIndexPic',ERR_TMESSAGE_SET_INDEX_PIC,E],
                ['{0926AE0B-6817-48C0-9134-56789F28CD69}']);
    end;
end;

function TMessage.GetSalt : Hex;
begin
    Result := '';
    try
        Result := PropAt [_msg_salt].asHex;
    except on E: Exception do
        _raise (['GetSalt',ERR_TMESSAGE_GET_SALT,E],
                ['{CED75B19-FAAE-4F9F-BB25-7F41806E6B9C}']);
    end;
end;

procedure TMessage.SetSalt (const aValue: Hex);
begin
    try
        PropAt [_msg_salt].asHex := aValue;
    except on E: Exception do
        _raise (['SetSalt',ERR_TMESSAGE_SET_SALT,E],
                ['{8E48B222-8E35-4E26-8006-15F348AC6A93}']);
    end;
end;

function TMessage.CalcKeyHash : Hex;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto)then
        begin
            Result := User.Crypto.Hash ( Format ('%s_%s',[ CalcDataHash,
                                                           User.Crypto.GenerateKey ]) );
        end
        else
            Result := inherited CalcKeyHash;
    except on E: Exception do
        _raise (['CalcKeyHash',ERR_TMESSAGE_CALC_KEY_HASH,E],
                ['{A155E901-B8AE-4CD2-B36E-EC22D1598F68}']);
    end;
end;

function TMessage.CalcDataHash : Hex;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto) and ( (IDOwner = 0) or (IDOwner = USER_ID) ) then
        begin
            if notEmpty (HashCategorie) then
                Result := User.Crypto.Hash ( Format ('%s_%s_%s',[ Subject,
                                                                  User.Crypto.Hash (Text),
                                                                  HashCategorie ]) )
            else
                Result := User.Crypto.Hash ( Format ('%s_%s_%s',[ Subject,
                                                                  User.Crypto.Hash (Text),
                                                                  User.Crypto.Hash ('') ]) );
        end
        else
            Result := inherited CalcDataHash;
    except on E: Exception do
        _raise (['CalcDataHash',ERR_TMESSAGE_CALC_DATA_HASH,E],
                ['{3CDAA993-659F-46BE-919C-F70694991D7E}']);
    end;
end;

procedure TMessage.LoadFromStream (const aStream: TStream);
var
    PicStream : TStream;
begin
    try
        if not Assigned (aStream) then
            raise Exception.Create (ERR_TMESSAGE_INVALID_STREAM);
        inherited LoadFromStream (aStream);
        Mode := Mode - [objImported];
        with aStream do
        begin
            HashCategorie := ReadStrL (aStream);
            HashAuthor := ReadStrL (aStream);
            IDType := StrToInt (  HexToStr ( ReadStrL (aStream) )  );
            IDStatus := StrToInt (  HexToStr ( ReadStrL (aStream) )  );
            Subject := HexToStr ( ReadStrL (aStream) );
            Text := Decompress (  HexToStr ( ReadStrL (aStream) )  );
            HashPic := ReadStrL (aStream);
            IndexPic := StrToInt (  HexToStr ( ReadStrL (aStream) )  );
            PicStream := TMemoryStream.Create;
            try
                ReadStreamL (aStream,PicStream);
                Pic.LoadFromStream (PicStream);
            finally
                FreeAndNil (PicStream);
            end;
        end;
        Mode := Mode +[objImported];
    except on E: Exception do
        _raise (['LoadFromStream',ERR_TMESSAGE_LOAD_FROM_STREAM,E],
                ['{18399995-E35E-4BA9-96E2-D5F3AB59CE8F}']);
    end;
end;

procedure TMessage.SaveToStream (out aStream: TStream);
var
    PicStream : TStream;
begin
    try
        if not Assigned (aStream) then
            raise Exception.Create (ERR_TMESSAGE_INVALID_STREAM);
        inherited SaveToStream (aStream);
        Mode := Mode - [objExported];
        with aStream do
        begin
            WriteStrL (aStream,HashCategorie);
            WriteStrL (aStream,HashAuthor);
            WriteStrL (  aStream, StrToHex ( IntToStr (IDType) )  );
            WriteStrL (  aStream, StrToHex ( IntToStr (IDStatus) )  );
            WriteStrL ( aStream, StrToHex (Subject) );
            WriteStrL (  aStream, StrToHex ( Compress (Text) )  );
            WriteStrL (aStream,HashPic);
            WriteStrL (  aStream, StrToHex ( IntToStr (IndexPic) )  );
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
        _raise (['SaveToStream',ERR_TMESSAGE_SAVE_TO_STREAM,E],
                ['{42C20FE2-87CD-440C-AEEF-75E330E62277}']);
    end;
end;
