{******************************************************************************}
{*  TCategorie.imp.inc                                                        *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TCategorie }
class function TCategorie.GetClassName : String;
begin
    Result := CLS_TCATEGORIE_NAME;
end;

class function TCategorie.GetClassVersion : TVersionInfo;
begin
    Result := inherited GetClassVersion;
end;

class function TCategorie.GetClassTable : String;
begin
    Result := 'categories';
end;

class function TCategorie.GetClassProperties : TProtoProperties;
begin
    Result := NIL;
    try
        Result := inherited GetClassProperties;
        with Result do
        begin
            Register ( TMetaProperty, ['hash_parent',PRP_TCATEGORIE_HASH_PARENT,dtHex,''],[prpStored] );
            Register ( TMetaProperty, ['id_parent',PRP_TCATEGORIE_ID_PARENT,dtID,0],[prpStored] );
            Register ( TMetaProperty, ['hash_author',PRP_TCATEGORIE_HASH_AUTHOR,dtHex,''],[prpStored] );
            Register ( TMetaProperty, ['id_author',PRP_TCATEGORIE_ID_AUTHOR,dtID,0],[prpStored] );
            Register ( TMetaProperty, ['id_owner',PRP_TCATEGORIE_ID_OWNER,dtID,0],[prpStored] );
            Register ( TMetaProperty, ['id_type',PRP_TCATEGORIE_ID_TYPE,dtID,0],[prpStored] );
            Register ( TMetaProperty, ['id_status',PRP_TCATEGORIE_ID_STATUS,dtID,0],[prpStored] );
            Register ( TMetaProperty, ['name',PRP_TCATEGORIE_NAME,dtSafe,''],[prpStored] );
            if ( ItemAt [_ctg_name].DataType <> dtSafe ) then
                raise Exception.Create ('The name was not encrypted. Hacking attempt!');
            Register ( TMetaProperty, ['description',PRP_TCATEGORIE_DESCRIPTION,dtSafe,''],[prpStored] );
            if ( ItemAt [_ctg_description].DataType <> dtSafe ) then
                raise Exception.Create ('The description was not encrypted. Hacking attempt!');
            Register ( TMetaProperty, ['hash_pic',PRP_TCATEGORIE_HASH_PIC,dtHex,''],[prpStored] );
            Register ( TMetaProperty, ['id_pic',PRP_TCATEGORIE_ID_PIC,dtID,0],[prpStored] );
            Register ( TMetaProperty, ['index_pic',PRP_TCATEGORIE_INDEX_PIC,dtInteger,0],[prpStored] );
            Register ( TMetaProperty, ['salt',PRP_TCATEGORIE_SALT,dtHex,''],[prpStored] );
        end;
    except on E: Exception do
        _raise (['GetClassProperties',ERR_TCATEGORIE_GET_ClASS_PROPERTIES,E],
                ['{DFA96C07-2C5B-451C-BF4A-577188FD5265}']);
    end;
end;

class function TCategorie.GetClassList : CProtoObjects;
begin
    Result := TCategories;
end;

constructor TCategorie.Create (const aDB: TSQLiteDatabase;
                               anArgs: array of const;
                               aMode: TObjectMode = []);
begin
    try
        inherited Create (aDB,[],aMode);
        Mode := Mode - [objCreated];
        { первый параметр - идентификатор }
        if notEmpty (0,anArgs) then
            ID := toInt64 (anArgs [0]);
        { второй параметр - идентификатор категории-предка }
        //f_Parent := TCategorie.Create (aDB,[],aMode+[objSimple]);// no delete this comment, cycled
        if notEmpty (1,anArgs) then
            IDParent := toInt64 (anArgs [1]);
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
        f_CtgType := TCategorieType.Create (aDB,[],aMode);
        if notEmpty (4,anArgs) then
            IDType := toInt64 (anArgs [4]);
        { шестой параметр - идентификатор статуса }
        f_CtgStatus := TCategorieStatus.Create (aDB,[],aMode);
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
        _raise (['Create',ERR_TCATEGORIE_CREATE,E],
                ['{4F858D95-40CE-4813-8859-B832403F8B75}']);
    end; end;
end;

destructor TCategorie.Destroy;
begin
    try
        FreeAndNil (f_Parent);
        if ( Author <> User ) then
            FreeAndNil (f_Author);
        FreeAndNil (f_CtgType);
        FreeAndNil (f_CtgStatus);
        FreeAndNil (f_Pic);
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TCATEGORIE_DESTROY,E],
                ['{E563356C-EBB9-46C4-B6AF-756BC7254BD5}']);
    end;
end;

class procedure TCategorie.CreateClassTable (const aDB: TSQLiteDatabase);
var
    Props    : TProtoProperties;
    MsgProps : TProtoProperties;
    query    : String;
begin
    try
        inherited CreateClassTable (aDB);
        {$IFDEF DELETE_TRIGGER}
        { создаем триггер на удаление }
        if not aDB.TriggerExists ( Format ('trigger_%s_%s',[GetClassTable,'delete']) ) then
        begin
            Props := GetClassProperties;
            MsgProps := TMessage.GetClassProperties;
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
                                     // delete messages
                                     'DELETE '+
                                     'FROM %s '+
                                     'WHERE %s = OLD.%s; '+
                                     // delete children categories
                                     'DELETE '+
                                     'FROM %s '+
                                     'WHERE %s = OLD.%s; '+
                                 'END ',
                                 [ GetClassTable, 'delete',
                                   GetClassTable,
                                   // delete key words
                                   TKeyWord.GetClassTable,
                                   KwdProps.ItemAt [_kwd_id_categorie].Name, Props.ItemAt [_ctg_id].Name,
                                   // delete messages
                                   TMessage.GetClassTable,
                                   MsgProps.ItemAt [_msg_id_categorie].Name, Props.ItemAt [_ctg_id].Name,
                                   // delete children categories
                                   GetClassTable,
                                   Props.ItemAt [_ctg_id_parent].Name, Props.ItemAt [_ctg_id].Name ]);
                // ShowMessage (query);
                aDB.ExecSQL (query);
            finally
                KwdProps.FreeAll;
                FreeAndNil (KwdProps);
                MsgProps.FreeAll;
                FreeAndNil (MsgProps);
                Props.FreeAll;
                FreeAndNil (Props);
            end;
        end;
        {$ENDIF DELETE_TRIGGER}
    except on E: Exception do
        _raise (['CreateClassTable',ERR_TCATEGORIE_CREATE_CLASS_TABLE,E],
                ['{C17EFAA5-DAA1-4B31-A3E6-1F1275E914F2}']);
    end;
end;

procedure TCategorie.SetDB (const aDB: TSQLiteDatabase);
begin
    try
        inherited SetDB (aDB);
        if Assigned (f_Parent) then
            f_Parent.DB := aDB;
        if Assigned (f_Author) then
            f_Author.DB := aDB;
        if Assigned (f_CtgType) then
            f_CtgType.DB := aDB;
        if Assigned (f_CtgStatus) then
            f_CtgStatus.DB := aDB;
        if Assigned (f_Pic) then
            f_Pic.DB := aDB;
    except on E: Exception do
        _raise (['SetDB',ERR_TCATEGORIE_SET_DB,E],
                ['{27B7141C-F12D-478D-966F-BA0BF8708CFB}']);
    end;
end;

function TCategorie.Check : Boolean;
begin
    Result := TRUE;
    try
        Result := inherited Check;
        if Result then
        begin
            {if not ( IDParent >= 0 ) then
                Error (ERR_TCATEGORIE_INVALID_ID_PARENT);}
            {if not ( IDAuthor > 0 ) then
                Error (ERR_TCATEGORIE_INVALID_ID_AUTHOR);}
            if not ( IDOwner > 0 ) then
                Error (ERR_TCATEGORIE_INVALID_ID_OWNER);
            if not ( IDType > 0 ) then
                Error (ERR_TCATEGORIE_INVALID_ID_TYPE);
            if not ( IDStatus > 0 ) then
                Error (ERR_TCATEGORIE_INVALID_ID_STATUS);
            {if not ( IDPic > 0 ) then
                Error (ERR_TCATEGORIE_INVALID_ID_PIC);}
            Result := ( Errors.Count = 0 );
        end;
    except on E: Exception do
        _raise (['Check',ERR_TCATEGORIE_CHECK,E],
                ['{6A5FE91A-EDC7-4ADA-A001-F1B960E75C90}']);
    end;
end;

procedure TCategorie.Load;
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
        { parent }
        if Assigned (Parent) then
        begin
            Parent.ID := PropAt [_ctg_id_parent].asID;
            if ( IDParent > 0 ) then
                Parent.Load;
        end
        else if ( PropAt [_ctg_id_parent].asID > 0 ) then
            Parent := TCategorie.Load (DB,PropAt [_ctg_id_parent].asID,[objSimple]) as TCategorie
        else
            Parent := TCategorie.Create (DB,[],[objSimple]);
        { author }
        if Assigned (Author) then
        begin
            Author.ID := PropAt [_ctg_id_author].asID;
            if ( IDAuthor > 0 ) then
                Author.Load;
        end
        else if ( PropAt [_ctg_id_author].asID > 0 ) then
            Author := TUser.Load (DB,PropAt [_ctg_id_author].asID,[objSimple]) as TUser
        else
            Author := TUser.Create (DB,[],[objSimple]);
        { type }
        if Assigned (CtgType) then
        begin
            CtgType.ID := PropAt [_ctg_id_type].asID;
            if ( IDType > 0 ) then
                CtgType.Load;
        end
        else if ( PropAt [_ctg_id_type].asID > 0 ) then
            CtgType := TCategorieType.Load (DB,PropAt [_ctg_id_type].asID) as TCategorieType
        else
            CtgType := TCategorieType.Create (DB,[]);
        { status }
        if Assigned (CtgStatus) then
        begin
            CtgStatus.ID := PropAt [_ctg_id_status].asID;
            if ( IDStatus > 0 ) then
                CtgStatus.Load;
        end
        else if ( PropAt [_ctg_id_status].asID > 0 ) then
            CtgStatus := TCategorieStatus.Load (DB,PropAt [_ctg_id_status].asID) as TCategorieStatus
        else
            CtgStatus := TCategorieStatus.Create (DB,[]);
        { pic }
        if Assigned (Pic) then
        begin
            Pic.ID := PropAt [_ctg_id_pic].asID;
            if ( IDPic > 0 ) then
                Pic.Load;
        end
        else if ( PropAt [_ctg_id_pic].asID > 0 ) then
            Pic := TPic.Load (DB,PropAt [_ctg_id_pic].asID) as TPic
        else
            Pic := TPic.Create (DB,[]);
        Mode := Mode + [objLoaded];
    except on E: Exception do begin
        Mode := Mode + [objLoadFailed];
        _raise (['Load',ERR_TCATEGORIE_LOAD,E],
                ['{E3A7008F-F983-458F-A5BC-1EDF33F28764}']);
    end; end;
end;

procedure TCategorie.Save;
var
    isBeforeTransactionOpen : Boolean;
    Children                : TCategories;
    Messages                : TMessages;
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
            { parent }
            PropAt [_ctg_hash_parent].asHex := HashParent;
            { author }
            PropAt [_ctg_hash_author].asHex := HashAuthor;
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
            PropAt [_ctg_id_pic].asID := Pic.ID;
            PropAt [_ctg_hash_pic].asHex := HashPic;

            inherited Save;
            { извлекаем ключевые слова }
            Words := TStringList.Create;
            try
                GetKeyWords (Description,Words);
                TKeyWords.Delete (DB,[ NIL, NIL, _([ ID ]) ]);
                for I := 0 to Words.Count - 1 do
                try
                    KeyWord := TKeyWord.Create (DB,[ 0, Words [I], ID, 0, IDAuthor, IDOwner ]);
                    KeyWord.Save;
                finally
                    FreeAndNil (KeyWord);
                end;
            finally
                FreeAndNil (Words);
            end;
            { привязываем все категории без идентификатора предка,
              ( либо с формальным идентификатором предка ),
              у которых хэш-ключ предка - это хэш-ключ данной категории }
            if ( IDType <> CATEGORIE_ROOT_TYPE_ID ) then
            begin
                Children := TCategories.Load (DB,[ NIL,
                                                   _([0,ROOT_CATEGORIE_ID]),
                                                   NIL,
                                                   _([IDOwner]),
                                                   _([CATEGORIE_FORUM_TYPE_ID,
                                                      CATEGORIE_TOPIC_TYPE_ID]) ],
                                                 [],
                                                 [],
                                                 [objSimple]) as TCategories;
                try
                    for I := 0 to Children.Count - 1 do
                        if ( Children.ItemAt [I].HashParent = KeyHash ) then
                        begin
                            Children.ItemAt [I].IDParent := ID;
                            Children.ItemAt [I].Save;
                        end;
                finally
                    FreeAndNil (Children);
                end;
            end;
            { привязываем все сообщения без идентификатора владельца,
              ( либо с формальным идентификатором владельца ),
              у которых хэш-ключ владельца - это хэш-ключ данной категории }
            if ( IDType <> CATEGORIE_ROOT_TYPE_ID ) then
            begin
                Messages := TMessages.Load (DB,[ NIL,
                                                 _([0,ROOT_CATEGORIE_ID]),
                                                 NIL,
                                                 _([IDOwner]) ],
                                               [],
                                               [],
                                               [objSimple]) as TMessages;
                try
                    for I := 0 to Messages.Count - 1 do
                        if ( Messages.ItemAt [I].HashCategorie = KeyHash ) then
                        begin
                            Messages.ItemAt [I].IDCategorie := ID;
                            Messages.ItemAt [I].Save;
                        end;
                finally
                    FreeAndNil (Messages);
                end;
            end;
            { привязываем все ключевые слова без идентификатора категории,
              у которых хэш-ключ категории - это хэш-ключ данной категории }
            if ( IDType <> CATEGORIE_ROOT_TYPE_ID ) then
            begin
                KeyWords := TKeyWords.Load (DB,[ NIL,
                                                 NIL,
                                                 _([0]),
                                                 NIL,
                                                 NIL,
                                                 _([IDOwner]) ],
                                               [],
                                               [],
                                               [objSimple]) as TKeyWords;
                try
                    for I := 0 to KeyWords.Count - 1 do
                        if ( KeyWords.ItemAt [I].HashCategorie = KeyHash ) then
                        begin
                            KeyWords.ItemAt [I].IDCategorie := ID;
                            KeyWords.ItemAt [I].Save;
                        end;
                finally
                    FreeAndNil (KeyWords);
                end;
            end;
            { если ошибок не обнаружено - проводим транзакцию }
            if ( not isBeforeTransactionOpen ) then
                DB.Commit;
        { в случае возникновения ошибки - откатываем транзакцию }
        except on E: Exception do begin
            if ( not isBeforeTransactionOpen ) then
                DB.Rollback;
            raise Exception.CreateFmt (ERR_TCATEGORIE_TRANSACTION,[E.Message]);
        end; end;
    except on E: Exception do begin
        Mode := Mode + [objSaveFailed];
        _raise (['Save',ERR_TCATEGORIE_SAVE,E],
                ['{1B46C83C-864A-4BD4-9F6F-F9D499F17B7A}']);
    end; end;
end;

{$IFNDEF DELETE_TRIGGER}
procedure TCategorie.Delete;
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
            { удаляем все потомки и их сообщения }
            TCategories.DeleteByParent (DB,ID);
            { удаляем категорию }
            inherited Delete;
            Mode := Mode - [objDeleted];
            { удаляем все ключевые слова для данной категории }
            TKeyWords.Delete ( DB, [ NIL, NIL, _([ID]) ] );
            { удаляем все сообщения в данной категории }
            TMessages.Delete ( DB, [ NIL, _([ID]) ] );
            { если ошибок не обнаружено - проводим транзакцию }
            if ( not isBeforeTransactionOpen ) then
                DB.Commit;
        { в случае возникновения ошибки - откатываем транзакцию }
        except on E: Exception do begin
            if ( not isBeforeTransactionOpen ) then
                DB.Rollback;
            raise Exception.CreateFmt (ERR_TCATEGORIE_TRANSACTION,[E.Message]);
        end; end;
        Mode := Mode + [objDeleted];
    except on E: Exception do begin
        Mode := Mode + [objDeleteFailed];
        _raise (['Delete',ERR_TCATEGORIE_DELETE,E],
                ['{7F4D745A-820C-4E6B-BC31-E5262AA6A60C}']);
    end; end;
end;
{$ENDIF DELETE_TRIGGER}

function TCategorie.GetHashParent : Hex;
begin
    Result := '';
    try
        if notEmpty ( PropAt [_ctg_hash_parent].asHex ) then
            Result := PropAt [_ctg_hash_parent].asHex
        else if Assigned (Parent) and ( Parent.ID > 0 ) then
        begin
            if not ( objLoaded in Parent.Mode ) then
                PropAt [_ctg_hash_parent].asHex := TCategorie.GetHash (DB,IDParent)
            else
                PropAt [_ctg_hash_parent].asHex := Parent.KeyHash;
            Result := PropAt [_ctg_hash_parent].asHex;
        end;
    except on E: Exception do
        _raise (['GetHashParent',ERR_TCATEGORIE_GET_HASH_PARENT,E],
                ['{010DD076-577A-4BAC-BBAB-A721563976C4}']);
    end;
end;

procedure TCategorie.SetHashParent (const aValue: Hex);
begin
    try
        if notEmpty (aValue) and isHex (aValue) then
        begin
            PropAt [_ctg_hash_parent].asHex := aValue;
            PropAt [_ctg_id_parent].asID := TCategories.Find (DB,aValue,IDOwner);
        end
        else
            PropAt [_ctg_hash_parent].asHex := '';
        { привязываем "висячие" категории в корень }
        if ( IDType <> CATEGORIE_ROOT_TYPE_ID ) then
        begin
            if ( PropAt [_ctg_id_parent].asID = 0 ) then
                PropAt [_ctg_id_parent].asID := ROOT_CATEGORIE_ID;
            IDParent := PropAt [_ctg_id_parent].asID;
        end;
    except on E: Exception do
        _raise (['SetHashParent',ERR_TCATEGORIE_SET_HASH_PARENT,E],
                ['{C4E968FA-E05C-4AF8-8DD9-D80363E9694E}']);
    end;
end;

function TCategorie.GetIDParent : TID;
begin
    Result := 0;
    try
        Result := PropAt [_ctg_id_parent].asID;
    except on E: Exception do
        _raise (['GetIDParent',ERR_TCATEGORIE_GET_ID_PARENT,E],
                ['{3BE3C340-FE29-4DA3-A1B4-0AD91C1C7EC4}']);
    end;
end;

procedure TCategorie.SetIDParent (const aValue: TID);
begin
    try
        if ( aValue >= 0 ) then
            PropAt [_ctg_id_parent].asID := aValue
        else
            PropAt [_ctg_id_parent].asID := 0;
        if Assigned (Parent) and ( Parent.ID <> IDParent ) then
            Parent.ID := IDParent
        else if not Assigned (Parent) then
            Parent := TCategorie.Create (DB,[IDParent],[objSimple]);
    except on E: Exception do
        _raise (['SetIDParent',ERR_TCATEGORIE_SET_ID_PARENT,E],
                ['{2839DDC2-6A95-4CC6-84E3-F96A521EEF5C}']);
    end;
end;

function TCategorie.GetParent : TCategorie;
begin
    Result := NIL;
    try
        Result := f_Parent;
    except on E: Exception do
        _raise (['GetCategorie',ERR_TCATEGORIE_GET_PARENT,E],
                ['{CD623669-AF39-4F88-968F-ACC2085EC348}']);
    end;
end;

procedure TCategorie.SetParent (const aValue: TCategorie);
begin
    try
        f_Parent := aValue;
        if Assigned (Parent) and ( Parent.ID <> IDParent ) then
            IDParent := Parent.ID;
    except on E: Exception do
        _raise (['SetCategorie',ERR_TCATEGORIE_SET_PARENT,E],
                ['{8CF5E6AD-335B-4989-A664-07782FC5340F}']);
    end;
end;

function TCategorie.GetHashAuthor : Hex;
begin
    Result := '';
    try
        if notEmpty ( PropAt [_ctg_hash_author].asHex ) then
            Result := PropAt [_ctg_hash_author].asHex
        else if Assigned (Author) and ( Author.ID > 0 ) then
        begin
            if not ( objLoaded in Author.Mode ) then
                PropAt [_ctg_hash_author].asHex := TUser.GetHash (DB,IDAuthor)
            else
                PropAt [_ctg_hash_author].asHex := Author.KeyHash;
            Result := PropAt [_ctg_hash_author].asHex;
        end;
    except on E: Exception do
        _raise (['GetHashAuthor',ERR_TCATEGORIE_GET_HASH_AUTHOR,E],
                ['{90A061B9-C54B-4E32-82A0-BC641E211B6E}']);
    end;
end;

procedure TCategorie.SetHashAuthor (const aValue: Hex);
begin
    try
        if isHex (aValue) then
        begin
            PropAt [_ctg_hash_author].asHex := aValue;
            if ( aValue = User.KeyHash ) then
                IDAuthor := USER_ID
            else
                IDAuthor := TUsers.Find (DB,aValue,IDOwner);
        end
        else
            PropAt [_ctg_hash_author].asHex := '';
    except on E: Exception do
        _raise (['SetHashAuthor',ERR_TCATEGORIE_SET_HASH_AUTHOR,E],
                ['{DAB98C20-A7C1-43C1-97C4-BA9E2F5470AA}']);
    end;
end;

function TCategorie.GetIDAuthor : TID;
begin
    Result := 0;
    try
        Result := PropAt [_ctg_id_author].asID;
    except on E: Exception do
        _raise (['GetIDAuthor',ERR_TCATEGORIE_GET_ID_AUTHOR,E],
                ['{124B9093-8004-4F8D-804B-C1D9155D94D5}']);
    end;
end;

procedure TCategorie.SetIDAuthor (const aValue: TID);
begin
    try
        if ( aValue >= 0 ) then
            PropAt [_ctg_id_author].asID := aValue
        else
            PropAt [_ctg_id_author].asID := 0;
        if Assigned (Author) and ( Author.ID <> IDAuthor ) then
            Author.ID := IDAuthor
        else if not Assigned (Author) then
            Author := TUser.Create (DB,[IDAuthor],[objSimple]);
    except on E: Exception do
        _raise (['SetIDAuthor',ERR_TCATEGORIE_SET_ID_AUTHOR,E],
                ['{960B1B1D-E886-426A-A8AC-CF7D368A90CA}']);
    end;
end;

function TCategorie.GetAuthor : TUser;
begin
    Result := NIL;
    try
        Result := f_Author;
    except on E: Exception do
        _raise (['GetAuthor',ERR_TCATEGORIE_GET_AUTHOR,E],
                ['{EFB5C292-8DB8-48AA-A51C-82864429159E}']);
    end;
end;

procedure TCategorie.SetAuthor (const aValue: TUser);
begin
    try
        if Assigned (Author) and ( Author <> aValue ) and ( Author <> User ) then
            FreeAndNil (f_Author);
        f_Author := aValue;
        if Assigned (Author) and ( Author.ID <> IDAuthor ) then
            IDAuthor := Author.ID;
    except on E: Exception do
        _raise (['SetAuthor',ERR_TCATEGORIE_SET_AUTHOR,E],
                ['{B5CE50E2-15E1-4BCC-95E4-C31A61DC9911}']);
    end;
end;

function TCategorie.GetIDOwner : TID;
begin
    Result := 0;
    try
        Result := PropAt [_ctg_id_owner].asID;
    except on E: Exception do
        _raise (['GetIDOwner',ERR_TCATEGORIE_GET_ID_OWNER,E],
                ['{BCCA4BBE-AA69-4907-A258-459B35590B93}']);
    end;
end;

procedure TCategorie.SetIDOwner (const aValue: TID);
begin
    try
        if ( aValue >= 0 ) then
            PropAt [_ctg_id_owner].asID := aValue
        else
            PropAt [_ctg_id_owner].asID := 0;
    except on E: Exception do
        _raise (['SetIDOwner',ERR_TCATEGORIE_SET_ID_OWNER,E],
                ['{31C0F3C0-809D-471C-A9CA-5226EA04FED0}']);
    end;
end;

function TCategorie.GetIDType : TID;
begin
    Result := 0;
    try
        Result := PropAt [_ctg_id_type].asID;
    except on E: Exception do
        _raise (['GetIDType',ERR_TCATEGORIE_GET_ID_TYPE,E],
                ['{BAF00613-3094-4759-A2F9-4ED836D88A9D}']);
    end;
end;

procedure TCategorie.SetIDType (const aValue: TID);
begin
    try
        if ( aValue >= 0 ) then
            PropAt [_ctg_id_type].asID := aValue
        else
            PropAt [_ctg_id_type].asID := 0;
        if Assigned (CtgType) and ( CtgType.ID <> IDType ) then
            CtgType.ID := IDType
        else if not Assigned (CtgType) then
            CtgType := TCategorieType.Create (DB,[IDType]);
    except on E: Exception do
        _raise (['SetIDType',ERR_TCATEGORIE_SET_ID_TYPE,E],
                ['{6F60B71C-69A3-4015-B479-76E9E84102C9}']);
    end;
end;

function TCategorie.GetCtgType : TCategorieType;
begin
    Result := NIL;
    try
        Result := f_CtgType;
    except on E: Exception do
        _raise (['GetCtgType',ERR_TCATEGORIE_GET_TYPE,E],
                ['{9114C2F3-7FE0-4D6E-88C7-EF7371EBDB1E}']);
    end;
end;

procedure TCategorie.SetCtgType (const aValue: TCategorieType);
begin
    try
        f_CtgType := aValue;
        if Assigned (CtgType) and ( CtgType.ID <> IDType ) then
            IDType := CtgType.ID;
    except on E: Exception do
        _raise (['SetCtgType',ERR_TCATEGORIE_SET_TYPE,E],
                ['{75C3949F-2E7B-449D-8CEA-911A748C88A8}']);
    end;
end;

function TCategorie.GetIDStatus : TID;
begin
    Result := 0;
    try
        Result := PropAt [_ctg_id_status].asID;
    except on E: Exception do
        _raise (['GetIDStatus',ERR_TCATEGORIE_GET_ID_STATUS,E],
                ['{497B29F3-E089-4A88-AD34-3D19951E2706}']);
    end;
end;

procedure TCategorie.SetIDStatus (const aValue: TID);
begin
    try
        if ( aValue >= 0 ) then
            PropAt [_ctg_id_status].asID := aValue
        else
            PropAt [_ctg_id_status].asID := 0;
        if Assigned (CtgStatus) and ( CtgStatus.ID <> IDStatus ) then
            CtgStatus.ID := IDStatus
        else if not Assigned (CtgStatus) then
            CtgStatus := TCategorieStatus.Create (DB,[IDStatus]);
    except on E: Exception do
        _raise (['SetIDStatus',ERR_TCATEGORIE_SET_ID_STATUS,E],
                ['{B9648B43-1BB1-4C31-B5AC-F1C6BF19B2DA}']);
    end;
end;

function TCategorie.GetCtgStatus : TCategorieStatus;
begin
    Result := NIL;
    try
        Result := f_CtgStatus;
    except on E: Exception do
        _raise (['GetCtgStatus',ERR_TCATEGORIE_GET_STATUS,E],
                ['{4D25A8F8-AB96-4836-9651-168FF8A6B7AF}']);
    end;
end;

procedure TCategorie.SetCtgStatus (const aValue: TCategorieStatus);
begin
    try
        f_CtgStatus := aValue;
        if Assigned (CtgStatus) and ( CtgStatus.ID <> IDStatus ) then
            IDStatus := CtgStatus.ID;
    except on E: Exception do
        _raise (['SetCtgStatus',ERR_TCATEGORIE_SET_STATUS,E],
                ['{8B7B2483-478D-4C3E-8C97-600657AE3AE4}']);
    end;
end;

function TCategorie.GetName : String;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto) then
            Result := PropAt [_ctg_name].asSafe [ User.Crypto.Hash (User.Password,Salt),
                                                  User.Crypto ];
    except on E: Exception do
        _raise (['GetName',ERR_TCATEGORIE_GET_NAME,E],
                ['{02D8ACC1-136C-487F-ACDB-58FF8452F60D}']);
    end;
end;

procedure TCategorie.SetName (const aValue: String);
begin
    try
        if Assigned (User) and Assigned (User.Crypto) then
            PropAt [_ctg_name].asSafe [ User.Crypto.Hash (User.Password,Salt),
                                        User.Crypto ] := aValue;
    except on E: Exception do
        _raise (['SetName',ERR_TCATEGORIE_SET_NAME,E],
                ['{14013E33-DC07-4CFB-8CAF-E5DAAF9107F9}']);
    end;
end;

function TCategorie.GetDescription : String;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto) then
            Result := Decompress ( PropAt [_ctg_description].asSafe [ User.Crypto.Hash (User.Password,Salt),
                                                                      User.Crypto ] );
    except on E: Exception do
        _raise (['GetDescription',ERR_TCATEGORIE_GET_DESCRIPTION,E],
                ['{AE1B9EB0-B643-4025-BB37-F10AB6803738}']);
    end;
end;

procedure TCategorie.SetDescription (const aValue: String);
begin
    try
        if Assigned (User) and Assigned (User.Crypto) then
            PropAt [_ctg_description].asSafe [ User.Crypto.Hash (User.Password,Salt),
                                               User.Crypto ] := Compress (aValue);
    except on E: Exception do
        _raise (['SetDescription',ERR_TCATEGORIE_SET_DESCRIPTION,E],
                ['{D6B77984-852F-4AE3-BD68-8A59DC554B3B}']);
    end;
end;

function TCategorie.GetHashPic : Hex;
begin
    Result := '';
    try
        if notEmpty ( PropAt [_ctg_hash_pic].asHex ) then
            Result := PropAt [_ctg_hash_pic].asHex
        else if Assigned (Pic) and ( Pic.ID > 0 ) then
        begin
            if not ( objLoaded in Pic.Mode ) then
                PropAt [_ctg_hash_pic].asHex := TPic.GetHash (DB,IDPic)
            else
                PropAt [_ctg_hash_pic].asHex := Pic.KeyHash;
            Result := PropAt [_ctg_hash_pic].asHex;
        end;
    except on E: Exception do
        _raise (['GetHashPic',ERR_TCATEGORIE_GET_HASH_PIC,E],
                ['{926343E9-A72A-4CE7-A016-FCC1AD0DA588}']);
    end;
end;

procedure TCategorie.SetHashPic (const aValue: Hex);
begin
    try
        if isHex (aValue) then
            PropAt [_ctg_hash_pic].asHex := aValue
        else
            PropAt [_ctg_hash_pic].asHex := '';
    except on E: Exception do
        _raise (['SetHashPic',ERR_TCATEGORIE_SET_HASH_PIC,E],
                ['{8BFA0BA2-E62B-460E-999E-534536C015A4}']);
    end;
end;

function TCategorie.GetIDPic : TID;
begin
    Result := 0;
    try
        Result := PropAt [_ctg_id_pic].asID;
    except on E: Exception do
        _raise (['GetIDPic',ERR_TCATEGORIE_GET_ID_PIC,E],
                ['{1B206366-A9C2-4EDF-98D3-5528FAA8EC0A}']);
    end;
end;

procedure TCategorie.SetIDPic (const aValue: TID);
begin
    try
        if ( aValue >= 0 ) then
            PropAt [_ctg_id_pic].asID := aValue
        else
            PropAt [_ctg_id_pic].asID := 0;
        if Assigned (Pic) and ( Pic.ID <> IDPic ) then
            Pic.ID := IDPic
        else if not Assigned (Pic) then
            Pic := TPic.Create (DB,[IDPic]);
    except on E: Exception do
        _raise (['SetIDPic',ERR_TCATEGORIE_SET_ID_PIC,E],
                ['{95A1D2E0-20A8-4873-B8E8-C1276A80F9A9}']);
    end;
end;

function TCategorie.GetPic : TPic;
begin
    Result := NIL;
    try
        Result := f_Pic;
    except on E: Exception do
        _raise (['GetPic',ERR_TCATEGORIE_GET_PIC,E],
                ['{9C7466EC-A771-42F9-8022-A164AB3F3729}']);
    end;
end;

procedure TCategorie.SetPic (const aValue: TPic);
begin
    try
        f_Pic := aValue;
        if Assigned (Pic) and ( Pic.ID <> IDPic ) then
            IDPic := Pic.ID;
    except on E: Exception do
        _raise (['SetPic',ERR_TCATEGORIE_SET_PIC,E],
                ['{3D540B30-401A-4B2E-B364-C180ECA4C945}']);
    end;
end;

function TCategorie.GetIndexPic : Integer;
begin
    Result := -1;
    try
        Result := PropAt [_ctg_index_pic].asInteger;
    except on E: Exception do
        _raise (['GetIndexPic',ERR_TCATEGORIE_GET_INDEX_PIC,E],
                ['{522E24A8-770F-4A4A-95E2-C4959A1BBB09}']);
    end;
end;

procedure TCategorie.SetIndexPic (const aValue: Integer);
begin
    try
        PropAt [_ctg_index_pic].asInteger := aValue;
    except on E: Exception do
        _raise (['SetIndexPic',ERR_TCATEGORIE_SET_INDEX_PIC,E],
                ['{50A7FE26-83BE-4E76-B484-64E88226ABE6}']);
    end;
end;

function TCategorie.GetSalt : Hex;
begin
    Result := '';
    try
        Result := PropAt [_ctg_salt].asHex;
    except on E: Exception do
        _raise (['GetSalt',ERR_TCATEGORIE_GET_SALT,E],
                ['{EBA7D9E7-919C-4CF3-AB16-23961F3C1EE9}']);
    end;
end;

procedure TCategorie.SetSalt (const aValue: Hex);
begin
    try
        PropAt [_ctg_salt].asHex := aValue;
    except on E: Exception do
        _raise (['SetSalt',ERR_TCATEGORIE_SET_SALT,E],
                ['{30FF204D-AB4B-443D-89A2-AB79DD1CAF43}']);
    end;
end;

function TCategorie.CalcKeyHash : Hex;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto) then
        begin
            Result := User.Crypto.Hash ( Format ('%s_%s',[ CalcDataHash,
                                                           User.Crypto.GenerateKey ]) );
        end
        else
            Result := inherited CalcKeyHash;
    except on E: Exception do
        _raise (['CalcKeyHash',ERR_TCATEGORIE_CALC_KEY_HASH,E],
                ['{6F2BA08B-8F62-472E-8B3E-AEEF656FEC9A}']);
    end;
end;

function TCategorie.CalcDataHash : Hex;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto) and ( (IDOwner = 0) or (IDOwner = USER_ID) ) then
        begin
            if notEmpty (HashParent) then
                Result := User.Crypto.Hash ( Format ('%s_%s_%s',[ Name,
                                                                  User.Crypto.Hash (Description),
                                                                  HashParent ]) )
            else
                Result := User.Crypto.Hash ( Format ('%s_%s_%s',[ Name,
                                                                  User.Crypto.Hash (Description),
                                                                  User.Crypto.Hash ('') ]) );
        end
        else
            Result := inherited CalcDataHash;
    except on E: Exception do
        _raise (['CalcDataHash',ERR_TCATEGORIE_CALC_DATA_HASH,E],
                ['{34B4990D-FC19-40E1-A422-BEF18DAF5312}']);
    end;
end;

procedure TCategorie.LoadFromStream (const aStream: TStream);
var
    PicStream : TStream;
begin
    try
        if not Assigned (aStream) then
            raise Exception.Create (ERR_TCATEGORIE_INVALID_STREAM);
        inherited LoadFromStream (aStream);
        Mode := Mode - [objImported];
        with aStream do
        begin
            HashParent := ReadStrL (aStream);
            HashAuthor := ReadStrL (aStream);
            IDType := StrToInt (  HexToStr ( ReadStrL (aStream) )  );
            IDStatus := StrToInt (  HexToStr ( ReadStrL (aStream) )  );
            Name := HexToStr ( ReadStrL (aStream) );
            Description := Decompress (  HexToStr ( ReadStrL (aStream) )  );
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
        Mode := Mode + [objImported];
    except on E: Exception do
        _raise (['LoadFromStream',ERR_TCATEGORIE_LOAD_FROM_STREAM,E],
                ['{EB00AED7-040E-42DB-B0D0-26FA8A204546}']);
    end;
end;

procedure TCategorie.SaveToStream (out aStream: TStream);
var
    PicStream : TStream;
begin
    try
        if not Assigned (aStream) then
            raise Exception.Create (ERR_TCATEGORIE_INVALID_STREAM);
        inherited SaveToStream (aStream);
        Mode := Mode - [objExported];
        with aStream do
        begin
            WriteStrL (aStream,HashParent);
            WriteStrL (aStream,HashAuthor);
            WriteStrL (  aStream, StrToHex ( IntToStr (IDType) )  );
            WriteStrL (  aStream, StrToHex ( IntToStr (IDStatus) )  );
            WriteStrL ( aStream, StrToHex (Name) );
            WriteStrL (  aStream, StrToHex ( Compress (Description) )  );
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
        _raise (['SaveToStream',ERR_TCATEGORIE_SAVE_TO_STREAM,E],
                ['{989101CA-89C4-401F-9040-A8BD67B6C3C0}']);
    end;
end;
