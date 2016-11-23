{******************************************************************************}
{*  TKeyWord.imp.inc                                                          *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2012                                             *}
{******************************************************************************}
{ TKeyWord }
class function TKeyWord.GetClassName : String;
begin
    Result := CLS_TKEYWORD_NAME;
end;

class function TKeyWord.GetClassVersion : TVersionInfo;
begin
    Result := inherited GetClassVersion;
end;

class function TKeyWord.GetClassTable : String;
begin
    Result := 'key_words';
end;

class function TKeyWord.GetClassProperties : TProtoProperties;
begin
    Result := NIL;
    try
        Result := inherited GetClassProperties;
        with Result do
        begin
            Register ( TMetaProperty, ['key_word',PRP_TKEYWORD_KEY_WORD,dtSafe,''],[prpStored] );
            if ( ItemAt [_kwd_key_word].DataType <> dtSafe ) then
                raise Exception.Create ('The key word was not encrypted. Hacking attempt!');
            Register ( TMetaProperty, ['hash',PRP_TKEYWORD_HASH,dtHex,''],[prpStored] );
            Register ( TMetaProperty, ['hash_categorie',PRP_TKEYWORD_HASH_CATEGORIE,dtHex,''],[prpStored] );
            Register ( TMetaProperty, ['id_categorie',PRP_TKEYWORD_ID_CATEGORIE,dtID,0],[prpStored] );
            Register ( TMetaProperty, ['hash_message',PRP_TKEYWORD_HASH_MESSAGE,dtHex,''],[prpStored] );
            Register ( TMetaProperty, ['id_message',PRP_TKEYWORD_ID_MESSAGE,dtID,0],[prpStored] );
            Register ( TMetaProperty, ['hash_author',PRP_TKEYWORD_HASH_AUTHOR,dtHex,''],[prpStored] );
            Register ( TMetaProperty, ['id_author',PRP_TKEYWORD_ID_AUTHOR,dtID,0],[prpStored] );
            Register ( TMetaProperty, ['id_owner',PRP_TKEYWORD_ID_OWNER,dtID,0],[prpStored] );
            Register ( TMetaProperty, ['salt',PRP_TKEYWORD_SALT,dtHex,''],[prpStored] );
        end;
    except on E: Exception do
        _raise (['GetClassProperties',ERR_TKEYWORD_GET_ClASS_PROPERTIES,E],
                ['{72C33213-ED06-4A13-BE44-3C07C8DE3112}']);
    end;
end;

class function TKeyWord.GetClassList : CProtoObjects;
begin
    Result := TKeyWords;
end;

constructor TKeyWord.Create (const aDB: TSQLiteDatabase;
                             anArgs: array of const;
                             aMode: TObjectMode = []);
begin
    try
        inherited Create (aDB,anArgs,aMode);
        Mode := Mode - [objCreated];
        { генерируем соль }
        if Assigned (User) and Assigned (User.Crypto) then
            Salt := User.Crypto.GenerateKey;
        { второй параметр - ключевое слово }
        if notEmpty (1,anArgs) then
            KeyWord := Trim ( toString (anArgs [1]) );
        { третий параметр - идентификатор категории }
        if notEmpty (2,anArgs) then
            IDCategorie := toInt64 (anArgs [2]);
        { четвертый параметр - идентификатор сообщения }
        if notEmpty (3,anArgs) then
            IDMessage := toInt64 (anArgs [3]);
        { пятый параметр - идентификатор автора }
        if notEmpty (4,anArgs) then
            IDAuthor := toInt64 (anArgs [4]);
        { шестой параметр - идентификатор владельца }
        if notEmpty (5,anArgs) then
            IDOwner := toInt64 (anArgs [5]);
        if Assigned (User) and ( IDOwner = 0 ) then
            IDOwner := USER_ID;
        Mode := Mode + [objCreated];
    except on E: Exception do begin
        Mode := Mode + [objCreateFailed];
        _raise (['Create',ERR_TKEYWORD_CREATE,E],
                ['{01A7FF79-B4E0-42DD-99CA-2BD3AF860ACA}']);
    end; end;
end;

destructor TKeyWord.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TKEYWORD_DESTROY,E],
                ['{4D74F0AE-D02F-4DC9-908D-6A170E89D6C1}']);
    end;
end;

class procedure TKeyWord.CreateClassTable (const aDB: TSQLiteDatabase);
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
                Prop := TMetaProperty (Props.ItemAt [_kwd_hash]);
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
        _raise (['CreateClassTable',ERR_TKEYWORD_CREATE_CLASS_TABLE,E],
                ['{5592FC9D-0F73-48A2-BD82-B6E417DB7817}']);
    end;
end;

function TKeyWord.Check : Boolean;
begin
    Result := TRUE;
    try
        Result := inherited Check;
        if Result then
        begin
            {if not ( IDCategorie > 0 ) then
                Error (ERR_TMESSAGE_INVALID_ID_CATEGORIE);}
            {if not ( IDMessage > 0 ) then
                Error (ERR_TMESSAGE_INVALID_ID_MESSAGE);}
            {if not ( IDAuthor > 0 ) then
                Error (ERR_TMESSAGE_INVALID_ID_AUTHOR);}
            Result := ( Errors.Count = 0 );
        end;
    except on E: Exception do
        _raise (['Check',ERR_TKEYWORD_CHECK,E],
                ['{452A200F-6ACB-49E4-84AF-835C855434AD}']);
    end;
end;

procedure TKeyWord.Save;
var
    isBeforeTransactionOpen : Boolean;
begin
    try
        if objNoSave in Mode then Exit;
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
            PropAt [_kwd_hash_categorie].asHex := HashCategorie;
            { message }
            PropAt [_kwd_hash_message].asHex := HashMessage;
            { author }
            PropAt [_kwd_hash_author].asHex := HashAuthor;

            inherited Save;
            { если ошибок не обнаружено - проводим транзакцию }
            if ( not isBeforeTransactionOpen ) then
                DB.Commit;
        { в случае возникновения ошибки - откатываем транзакцию }
        except on E: Exception do begin
            if ( not isBeforeTransactionOpen ) then
                DB.Rollback;
            raise Exception.CreateFmt (ERR_TKEYWORD_TRANSACTION,[E.Message]);
        end; end;;
    except on E: Exception do begin
        Mode := Mode + [objSaveFailed];
        _raise (['Save',ERR_TKEYWORD_SAVE,E],
                ['{23DB1C00-695E-4889-831B-F884A639CE31}']);
    end; end;
end;

function TKeyWord.GetKeyWord : String;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto) then
            Result := PropAt [_kwd_key_word].asSafe [ User.Crypto.Hash (User.Password,Salt),
                                                      User.Crypto ];
    except on E: Exception do
        _raise (['GetKeyWord',ERR_TKEYWORD_GET_KEY_WORD,E],
                ['{DEFBB1BD-0885-4F20-A4D2-05C17B7DEA52}']);
    end;
end;

procedure TKeyWord.SetKeyWord (const aValue: String);
begin
    try
        if Assigned (User) and Assigned (User.Crypto) then
        begin
            PropAt [_kwd_key_word].asSafe [ User.Crypto.Hash (User.Password,Salt),
                                            User.Crypto ] := aValue;
            PropAt [_kwd_hash].asHex := CalcHash (aValue);
        end;
    except on E: Exception do
        _raise (['SetKeyWord',ERR_TKEYWORD_SET_KEY_WORD,E],
                ['{7935CECB-026C-4C19-838E-F22847017C18}']);
    end;
end;

function TKeyWord.GetHash : Hex;
begin
    Result := '';
    try
        Result := PropAt [_kwd_hash].asHex;
    except on E: Exception do
        _raise (['GetHash',ERR_TKEYWORD_GET_HASH,E],
                ['{EBCCD7D9-782F-4CDE-8AD4-EECD728D5BDA}']);
    end;
end;

function TKeyWord.GetHashCategorie : Hex;
begin
    Result := '';
    try
        if notEmpty ( PropAt [_kwd_hash_categorie].asHex ) then
            Result := PropAt [_kwd_hash_categorie].asHex
        else if ( IDCategorie > 0 ) then
        begin
            PropAt [_kwd_hash_categorie].asHex := TCategorie.GetHash (DB,IDCategorie);
            Result := PropAt [_kwd_hash_categorie].asHex;
        end;
    except on E: Exception do
        _raise (['GetHashCategorie',ERR_TKEYWORD_GET_HASH_CATEGORIE,E],
                ['{F10790B8-5C89-488B-BF26-6E407ED10F3F}']);
    end;
end;

procedure TKeyWord.SetHashCategorie (const aValue: Hex);
begin
    try
        if isHex (aValue) then
        begin
            PropAt [_kwd_hash_categorie].asHex := aValue;
            IDCategorie := TCategories.Find (DB,aValue,IDOwner);
        end
        else
            PropAt [_kwd_hash_categorie].asHex := '';
    except on E: Exception do
        _raise (['SetHashCategorie',ERR_TKEYWORD_SET_HASH_CATEGORIE,E],
                ['{70BC3BE1-EC69-4C2C-842B-BFDFDE79DE2A}']);
    end;
end;

function TKeyWord.GetIDCategorie : TID;
begin
    Result := 0;
    try
        Result := PropAt [_kwd_id_categorie].asID;
    except on E: Exception do
        _raise (['GetIDCategorie',ERR_TKEYWORD_GET_ID_CATEGORIE,E],
                ['{74202CBA-C891-42EC-988F-94BD09A1852C}']);
    end;
end;

procedure TKeyWord.SetIDCategorie (const aValue: TID);
begin
    try
        if ( aValue >= 0 ) then
            PropAt [_kwd_id_categorie].asID := aValue
        else
            PropAt [_kwd_id_categorie].asID := 0;
    except on E: Exception do
        _raise (['SetIDCategorie',ERR_TKEYWORD_SET_ID_CATEGORIE,E],
                ['{86ED9497-CD14-4F05-B076-BC9F256FCFF6}']);
    end;
end;

function TKeyWord.GetHashMessage : Hex;
begin
    Result := '';
    try
        if notEmpty ( PropAt [_kwd_hash_message].asHex ) then
            Result := PropAt [_kwd_hash_message].asHex
        else if ( IDMessage > 0 ) then
        begin
            PropAt [_kwd_hash_message].asHex := TMessage.GetHash (DB,IDMessage);
            Result := PropAt [_kwd_hash_message].asHex;
        end;
    except on E: Exception do
        _raise (['GetHashMessage',ERR_TKEYWORD_GET_HASH_MESSAGE,E],
                ['{D99DFB46-1604-4D62-8159-6F154B2A0A74}']);
    end;
end;

procedure TKeyWord.SetHashMessage (const aValue: Hex);
begin
    try
        if isHex (aValue) then
        begin
            PropAt [_kwd_hash_message].asHex := aValue;
            IDMessage := TMessages.Find (DB,aValue,IDOwner);
        end
        else
            PropAt [_kwd_hash_message].asHex := '';
    except on E: Exception do
        _raise (['SetHashMessage',ERR_TKEYWORD_SET_HASH_MESSAGE,E],
                ['{4B075DAC-7DB9-457E-87A9-E0A2BA880DB9}']);
    end;
end;

function TKeyWord.GetIDMessage : TID;
begin
    Result := 0;
    try
        Result := PropAt [_kwd_id_message].asID;
    except on E: Exception do
        _raise (['GetIDMessage',ERR_TKEYWORD_GET_ID_MESSAGE,E],
                ['{1EE5EB9B-C646-48CE-992B-F1ED2DEBBD45}']);
    end;
end;

procedure TKeyWord.SetIDMessage (const aValue: TID);
begin
    try
        if ( aValue >= 0 ) then
            PropAt [_kwd_id_message].asID := aValue
        else
            PropAt [_kwd_id_message].asID := 0;
    except on E: Exception do
        _raise (['SetIDMessage',ERR_TKEYWORD_SET_ID_MESSAGE,E],
                ['{95DC8E4A-5CB5-426B-906B-82CA00A6BCFB}']);
    end;
end;

function TKeyWord.GetHashAuthor : Hex;
begin
    Result := '';
    try
        if notEmpty ( PropAt [_kwd_hash_author].asHex ) then
            Result := PropAt [_kwd_hash_author].asHex
        else if ( IDAuthor > 0 ) then
        begin
            PropAt [_kwd_hash_author].asHex := TUser.GetHash (DB,IDAuthor);
            Result := PropAt [_kwd_hash_author].asHex;
        end;
    except on E: Exception do
        _raise (['GetHashAuthor',ERR_TKEYWORD_GET_HASH_AUTHOR,E],
                ['{28B7DFF4-D305-4843-8D0A-0C0A32B04A04}']);
    end;
end;

procedure TKeyWord.SetHashAuthor (const aValue: Hex);
begin
    try
        if isHex (aValue) then
        begin
            PropAt [_kwd_hash_author].asHex := aValue;
            IDAuthor := TUsers.Find (DB,aValue,IDOwner);
        end
        else
            PropAt [_kwd_hash_author].asHex := '';
    except on E: Exception do
        _raise (['SetHashAuthor',ERR_TKEYWORD_SET_HASH_AUTHOR,E],
                ['{BE213133-15B8-4383-89B7-92ADBDEC9C2D}']);
    end;
end;

function TKeyWord.GetIDAuthor : TID;
begin
    Result := 0;
    try
        Result := PropAt [_kwd_id_author].asID;
    except on E: Exception do
        _raise (['GetIDAuthor',ERR_TKEYWORD_GET_ID_AUTHOR,E],
                ['{469CC0A6-E078-4489-AF53-3C20464C7E77}']);
    end;
end;

procedure TKeyWord.SetIDAuthor (const aValue: TID);
begin
    try
        if ( aValue >= 0 ) then
            PropAt [_kwd_id_author].asID := aValue
        else
            PropAt [_kwd_id_author].asID := 0;
    except on E: Exception do
        _raise (['SetIDAuthor',ERR_TKEYWORD_SET_ID_AUTHOR,E],
                ['{8A0D4D06-794B-466E-9FAC-658110D8E818}']);
    end;
end;

function TKeyWord.GetIDOwner : TID;
begin
    Result := 0;
    try
        Result := PropAt [_kwd_id_owner].asID;
    except on E: Exception do
        _raise (['GetIDOwner',ERR_TKEYWORD_GET_ID_OWNER,E],
                ['{3B99B3BE-4EE4-46BF-A1DB-16DE527D6DDC}']);
    end;
end;

procedure TKeyWord.SetIDOwner (const aValue: TID);
begin
    try
        if ( aValue >= 0 ) then
            PropAt [_kwd_id_owner].asID := aValue
        else
            PropAt [_kwd_id_owner].asID := 0;
    except on E: Exception do
        _raise (['SetIDOwner',ERR_TKEYWORD_SET_ID_OWNER,E],
                ['{FAE97F32-CD5B-4EE0-B542-FA69F666A4F7}']);
    end;
end;

function TKeyWord.GetSalt : Hex;
begin
    Result := '';
    try
        Result := PropAt [_kwd_salt].asHex;
    except on E: Exception do
        _raise (['GetSalt',ERR_TKEYWORD_GET_SALT,E],
                ['{CD60C548-1365-457D-AABB-F3E17DB713DB}']);
    end;
end;

procedure TKeyWord.SetSalt (const aValue: Hex);
begin
    try
        PropAt [_kwd_salt].asHex := aValue;
    except on E: Exception do
        _raise (['SetSalt',ERR_TKEYWORD_SET_SALT,E],
                ['{93A5AD03-7B89-45B7-A1D4-BD2450B6E161}']);
    end;
end;

class function TKeyWord.CalcHash (const aKeyWord: String) : Hex;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto)then
            Result := User.Crypto.Hash (aKeyWord);
    except on E: Exception do
        _raise (['CalcHash',ERR_TKEYWORD_CALC_HASH,E],
                ['{D1D5B37A-C8C4-4C41-80F7-FA182535A417}']);
    end;
end;

procedure TKeyWord.LoadFromStream (const aStream: TStream);
var
    StreamHash : Hex;
begin
    try
        if not Assigned (aStream) then
            raise Exception.Create (ERR_TKEYWORD_INVALID_STREAM);
        aStream.Position := 0;
        Mode := Mode - [objImported];
        with aStream do
        begin
            StreamHash := ReadStrL (aStream);
            HashCategorie := ReadStrL (aStream);
            HashMessage := ReadStrL (aStream);
            HashAuthor := ReadStrL (aStream);
            KeyWord := Decompress (  HexToStr ( ReadStrL (aStream) )  );
            if ( StreamHash <> Hash ) then
                raise Exception.Create (ERR_TKEYWORD_INVALID_HASH);
        end;
        Mode := Mode + [objImported];
    except on E: Exception do
        _raise (['LoadFromStream',ERR_TKEYWORD_LOAD_FROM_STREAM,E],
                ['{E58ABDFD-CFA0-45B8-BAB1-BB7557CCC760}']);
    end;
end;

procedure TKeyWord.SaveToStream (out aStream: TStream);
begin
    try
        if not Assigned (aStream) then
            raise Exception.Create (ERR_TKEYWORD_INVALID_STREAM);
        aStream.Position := 0;
        Mode := Mode - [objExported];
        with aStream do
        begin
            WriteStrL (aStream,Hash);
            WriteStrL (aStream,HashCategorie);
            WriteStrL (aStream,HashMessage);
            WriteStrL (aStream,HashAuthor);
            WriteStrL (  aStream, StrToHex ( Compress (KeyWord) )  );
        end;
        Mode := Mode + [objExported];
    except on E: Exception do
        _raise (['SaveToStream',ERR_TKEYWORD_SAVE_TO_STREAM,E],
                ['{5D709911-BBB0-45E6-B3DA-EA4A5B0B8C26}']);
    end;
end;
