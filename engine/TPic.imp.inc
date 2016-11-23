{******************************************************************************}
{*  TPic.imp.inc                                                              *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TPic }
class function TPic.GetClassName : String;
begin
    Result := CLS_TPIC_NAME;
end;

class function TPic.GetClassVersion : TVersionInfo;
begin
    Result := inherited GetClassVersion;
end;

class function TPic.GetClassTable : String;
begin
    Result := 'pics';
end;

class function TPic.GetClassProperties : TProtoProperties;
begin
    Result := NIL;
    try
        Result := inherited GetClassProperties;
        with Result do
        begin
            Register ( TMetaProperty, ['id_owner',PRP_TPIC_ID_OWNER,dtID,0],[prpStored] );
            Register ( TMetaProperty, ['format',PRP_TPIC_FORMAT,dtString,''],[prpStored] );
            Register ( TMetaProperty, ['data',PRP_TPIC_DATA,dtStream,''],[prpStored] );
        end;
    except on E: Exception do
        _raise (['GetClassProperties',ERR_TPIC_GET_ClASS_PROPERTIES,E],
                ['{9456CDDC-B45B-45F3-BC71-030BE3C4AEA2}']);
    end;
end;

class function TPic.GetClassList : CProtoObjects;
begin
    Result := TPics;
end;

constructor TPic.Create (const aDB: TSQLiteDatabase;
                         anArgs: array of const;
                         aMode: TObjectMode = []);
begin
    try
        inherited Create (aDB,[],aMode);
        Mode := Mode - [objCreated];
        f_Picture := TPicture.Create;
        f_Bitmap := TBitmap.Create;
        { первый параметр - идентификатор }
        if notEmpty (0,anArgs) then
            ID := toInt64 (anArgs [0]);
        { второй параметр - идентификатор владельца }
        if notEmpty (1,anArgs) then
            IDOwner := toInt64 (anArgs [1]);
        if Assigned (User) and ( IDOwner = 0 ) then
            IDOwner := USER_ID;
        { третий параметр - формат данных }
        if notEmpty (2,anArgs) then
            DataFormat := toString (anArgs [2]);
        { четвертый параметр - иконка }
        if notEmpty (3,anArgs) and Assigned (  toObject (anArgs [3])  ) then
        begin
            if ( toObject (anArgs [3]) is TGraphic ) then
                Picture.Graphic.Assign ( TPersistent (toObject (anArgs [3])) )
            else if ( toObject (anArgs [3]) is TPicture ) then
                Picture.Assign (  TPicture ( toObject (anArgs [3]) )  );
        end;
        { пятый параметр - дата и время создания }
        if notEmpty (4,anArgs) then
            TimeStampCreate := toDateTime (anArgs [4]);
        { шестой параметр - дата и время последнего редактирования }
        if notEmpty (5,anArgs) then
            TimeStampModify := toDateTime (anArgs [5]);
        { седьмой параметр - дата и время публикации }
        if notEmpty (6,anArgs) then
            TimeStampPublic := toDateTime (anArgs [6]);
        { седьмой параметр - версия }
        if notEmpty (7,anArgs) then
            Version := toVersionInfo (anArgs [7]);
        Mode := Mode + [objCreated];
    except on E: Exception do begin
        Mode := Mode + [objCreateFailed];
        _raise (['Create',ERR_TPIC_CREATE,E],
                ['{2C385D78-A190-45C1-AE7F-C94FD6F28992}']);
    end; end;
end;

destructor TPic.Destroy;
begin
    try
        FreeAndNil (f_Picture);
        FreeAndNil (f_Bitmap);
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TPIC_DESTROY,E],
                ['{CF4A1775-F71B-4577-93B1-A883FCA3A543}']);
    end;
end;

class procedure TPic.CreateClassTable (const aDB: TSQLiteDatabase);
var
    Props    : TProtoProperties;
    UsrProps : TProtoProperties;
    CtgProps : TProtoProperties;
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
            UsrProps := TUser.GetClassProperties;
            CtgProps := TCategorie.GetClassProperties;
            MsgProps := TMessage.GetClassProperties;
            try
                { создаем триггер }
                query := Format ('CREATE TRIGGER trigger_%s_%s '+
                                 'BEFORE DELETE '+
                                 'ON %s '+
                                 'FOR EACH ROW '+
                                 'BEGIN '+
                                     // update users
                                     'UPDATE %s '+
                                     'SET %s = 0 '+
                                     'WHERE %s = OLD.%s; '+
                                     // update categories
                                     'UPDATE %s '+
                                     'SET %s = 0 '+
                                     'WHERE %s = OLD.%s; '+
                                     // update messages
                                     'UPDATE %s '+
                                     'SET %s = 0 '+
                                     'WHERE %s = OLD.%s; '+
                                 'END ',
                                 [ GetClassTable, 'delete',
                                   GetClassTable,
                                   // update users
                                   TUser.GetClassTable,
                                   UsrProps.ItemAt [_usr_id_pic].Name,
                                   UsrProps.ItemAt [_usr_id_pic].Name, Props.ItemAt [_pic_id].Name,
                                   // update categories
                                   TCategorie.GetClassTable,
                                   CtgProps.ItemAt [_ctg_id_pic].Name,
                                   CtgProps.ItemAt [_ctg_id_pic].Name, Props.ItemAt [_pic_id].Name,
                                   // update messages
                                   TMessage.GetClassTable,
                                   MsgProps.ItemAt [_msg_id_pic].Name,
                                   MsgProps.ItemAt [_msg_id_pic].Name, Props.ItemAt [_pic_id].Name ]);
                // ShowMessage (query);
                aDB.ExecSQL (query);
            finally
                MsgProps.FreeAll;
                FreeAndNil (MsgProps);
                CtgProps.FreeAll;
                FreeAndNil (CtgProps);
                UsrProps.FreeAll;
                FreeAndNil (UsrProps);
                Props.FreeAll;
                FreeAndNil (Props);
            end;
        end;
        {$ENDIF DELETE_TRIGGER}
    except on E: Exception do
        _raise (['CreateClassTable',ERR_TPIC_CREATE_CLASS_TABLE,E],
                ['{52B5D42C-F002-49D8-BA56-F70782E7BCE7}']);
    end;
end;

function TPic.Check : Boolean;
begin
    Result := TRUE;
    try
        Result := inherited Check;
    except on E: Exception do
        _raise (['Check',ERR_TPIC_CHECK,E],
                ['{F8537496-9DE0-4528-BA28-7FBAC95B2362}']);
    end;
end;

class function TPic.GetGraphicClass (const aFormat: String) : TGraphicClass;
begin
    Result := NIL;
    try
        if ( aFormat = 'wmf' ) or ( aFormat = 'emf' )  then
            Result := TMetafile
        else if ( aFormat = 'ico' ) then
            Result := TIcon
        else if ( aFormat = 'bmp' ) then
            Result := TBitmap
        else if ( aFormat = 'jpg' ) or ( aFormat = 'jpeg' ) then
            Result := TJPEGImage
        else if ( aFormat = 'png' ) then
            Result := TPNGObject;
        if not Assigned (Result) then
            raise EInvalidGraphic.CreateFmt (SUnknownExtension,[aFormat]);
    except on E: Exception do
        _raise (['GetGraphicClass',ERR_TPIC_GET_GRAPHIC_CLASS,E],
                ['{4DD4E2F9-A33A-47E2-A3BC-6AAF01735802}']);
    end;
end;

function TPic.GraphicClass : TGraphicClass;
begin
    Result := NIL;
    try
         Result := GetGraphicClass (DataFormat);
    except on E: Exception do
        _raise (['GraphicClass',ERR_TPIC_GET_GRAPHIC_CLASS,E],
                ['{5ED136FB-0499-4A77-850A-596EE66401BC}']);
    end;
end;

class function TPic.GetGraphicFormat (const aClass: TClass) : String;
begin
    Result := '';
    try
        if ( aClass = TMetafile )  then
            Result := 'wmf'
        else if ( aClass = TIcon ) then
            Result := 'ico'
        else if ( aClass = TBitmap ) then
            Result := 'bmp'
        else if ( aClass = TJPEGImage ) then
            Result := 'jpg'
        else if ( aClass = TPNGObject ) then
            Result := 'png';
    except on E: Exception do
        _raise (['GetGraphicFormat',ERR_TPIC_GET_GRAPHIC_FORMAT,E],
                ['{78AF0A6E-A14E-4B64-915A-0B891CC09F0E}']);
    end;
end;

function TPic.GraphicFormat : String;
begin
    Result := '';
    try
        if Assigned (f_Picture) and Assigned (f_Picture.Graphic) then
            Result := GetGraphicFormat (f_Picture.Graphic.ClassType);
    except on E: Exception do
        _raise (['GraphicFormat',ERR_TPIC_GET_GRAPHIC_FORMAT,E],
                ['{41378AA7-0CC7-43E1-BBF6-F7A41C08531F}']);
    end;
end;

procedure TPic.Load;
var
    Graphic : TGraphic;
    Stream  : TStream;
begin
    try
        if objNoLoad in Mode then Exit;
        Mode := Mode - [objLoadFailed];
        Mode := Mode - [objLoaded];
        inherited Load;
        Mode := Mode - [objLoaded];
        if ( Data.Size > 0 ) then
        begin
            Graphic := GraphicClass.Create;
            try
                { распаковываем и загружаем }
                Stream := TMemoryStream.Create;
                try
                    Stream.Position := 0;
                    Data.Position := 0;
                    Decompress (Data,Stream);
                    Data.Position := 0;
                    Stream.Position := 0;
                    Graphic.LoadFromStream (Stream);
                finally
                    FreeAndNil (Stream);
                end;
            except
                FreeAndNil (Graphic);
                raise;
            end;
            f_Bitmap.SetSize (Graphic.Width,Graphic.Height);
            try
                f_Bitmap.Assign (Graphic);
            except on E: Exception do
                { show this exception in debug mode only }
            end;
            f_Picture.Graphic.Free;
            f_Picture.Graphic := Graphic;
        end
        else
        begin
            f_Picture.Bitmap.FreeImage;
            f_Bitmap.FreeImage;
        end;
        Mode := Mode + [objLoaded];
    except on E: Exception do begin
        Mode := Mode + [objLoadFailed];
        _raise (['Load',ERR_TPIC_LOAD,E],
                ['{4C756A80-00A6-4607-8A61-9A6E4C3428FE}']);
    end; end;
end;

procedure TPic.Save;
var
    isBeforeTransactionOpen : Boolean;
    Input                   : TStream;
    Output                  : TStream;
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
            DataFormat := GraphicFormat;
            { сжимаем и сохраняем }
            Input := TMemoryStream.Create;
            Output := TMemoryStream.Create;
            try
                Input.Position := 0;
                if Assigned (f_Picture.Graphic) and
                   ( (f_Picture.Width > 0) or
                     (f_Picture.Height > 0) ) then
                begin
                    f_Picture.Graphic.SaveToStream (Input);
                    Compress (Input,Output);
                end;
                Output.Position := 0;
                Data.Position := 0;
                Data.CopyFrom (Output,Output.Size);
                inherited Save;
            finally
                FreeAndNil (Input);
                FreeAndNil (Output);
            end;
            { привязываем всех пользователей, категории, сообщения,
              у которых данный хэш-ключ иконки }
            TUsers.ReplacePic (DB,KeyHash,ID,IDOwner);
            TCategories.ReplacePic (DB,KeyHash,ID,IDOwner);
            TMessages.ReplacePic (DB,KeyHash,ID,IDOwner);
        { если ошибок не обнаружено - проводим транзакцию }
            if ( not isBeforeTransactionOpen ) then
                DB.Commit;
        { в случае возникновения ошибки - откатываем транзакцию }
        except on E: Exception do begin
            if ( not isBeforeTransactionOpen ) then
                DB.Rollback;
            raise Exception.CreateFmt (ERR_TUSER_TRANSACTION,[E.Message]);
        end; end;
    except on E: Exception do begin
        Mode := Mode + [objSaveFailed];
        _raise (['Save',ERR_TPIC_SAVE,E],
                ['{42C88E13-C605-449C-B4DA-B3B752499764}']);
    end; end;
end;

function TPic.GetIDOwner : TID;
begin
    Result := 0;
    try
        Result := PropAt [_pic_id_owner].asID;
    except on E: Exception do
        _raise (['GetIDOwner',ERR_TPIC_GET_ID_OWNER,E],
                ['{7A4AD508-4E48-43C5-98F3-E207D3C0EC94}']);
    end;
end;

procedure TPic.SetIDOwner (const aValue: TID);
begin
    try
        if ( aValue >= 0 ) then
            PropAt [_pic_id_owner].asID := aValue
        else
            PropAt [_pic_id_owner].asID := 0;
    except on E: Exception do
        _raise (['SetIDOwner',ERR_TPIC_SET_ID_OWNER,E],
                ['{0AB16242-799A-4ADA-8170-0C8610FE4C06}']);
    end;
end;

function TPic.GetData : TStream;
begin
    Result := NIL;
    try
        Result := PropAt [_pic_data].asStream;
    except on E: Exception do
        _raise (['GetData',ERR_TPIC_GET_DATA,E],
                ['{40108C39-535F-4071-947B-7F442FC7B1E5}']);
    end;
end;

procedure TPic.SetData (const aValue: TStream);
begin
    try
        PropAt [_pic_data].asStream := aValue;
    except on E: Exception do
        _raise (['SetData',ERR_TPIC_SET_DATA,E],
                ['{7DF58174-8E6A-47FD-BF56-68E1E57BF9F8}']);
    end;
end;

function TPic.GetFormat : String;
begin
    Result := '';
    try
        Result := PropAt [_pic_format].asString;
    except on E: Exception do
        _raise (['GetFormat',ERR_TPIC_GET_FORMAT,E],
                ['{72484765-15D2-4995-969B-33FD73EF23DC}']);
    end;
end;

procedure TPic.SetFormat (const aValue: String);
begin
    try
        PropAt [_pic_format].asString := StrReplace (aValue,'.','');
    except on E: Exception do
        _raise (['SetFormat',ERR_TPIC_SET_FORMAT,E],
                ['{DBA1DA36-6AD9-4BB2-9DD2-C78997BB840A}']);
    end;
end;

function TPic.CalcKeyHash : Hex;
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
        _raise (['CalcKeyHash',ERR_TPIC_CALC_KEY_HASH,E],
                ['{47B1D7D8-72BC-4C74-9C56-99495495833C}']);
    end;
end;

function TPic.CalcDataHash : Hex;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto) and ( (IDOwner = 0) or (IDOwner = USER_ID) ) then
        begin
            Result := User.Crypto.Hash (  User.Crypto.Hash ( PropAt [_pic_data].asStream )  );
        end
        else
            Result := inherited CalcDataHash;
    except on E: Exception do
        _raise (['CalcDataHash',ERR_TPIC_CALC_DATA_HASH,E],
                ['{59674978-B9A3-4667-A596-D491FED5AEFA}']);
    end;
end;

procedure TPic.LoadFromStream (const aStream: TStream);
var
    Graphic   : TGraphic;
    HexStream : TStream;
    Input     : TStream;
    Stream    : TStream;
begin
    try
        if not Assigned (aStream) then
            raise Exception.Create (ERR_TPIC_INVALID_STREAM);
        inherited LoadFromStream (aStream);
        Mode := Mode - [objImported];
        with aStream do
        begin
            DataFormat := HexToStr ( ReadStrL (aStream) );
        end;
        Input := TMemoryStream.Create;
        HexStream := TMemoryStream.Create;
        try
            ReadStreamL (aStream,HexStream);
            if ( HexStream.Size > 0 ) then
            begin
                HexStreamToByteStream (HexStream,Input);
                Input.Position := 0;
                Data.CopyFrom (Input,Input.Size);
            end;
            if ( Data.Size > 0 ) then
            begin
                FreeAndNil (f_Picture);
                f_Picture := TPicture.Create;
                FreeAndNil (f_Bitmap);
                f_Bitmap := TBitmap.Create;
                Graphic := GraphicClass.Create;
                try
                    Stream := TMemoryStream.Create;
                    try
                        Stream.Position := 0;
                        Data.Position := 0;
                        Decompress (Data,Stream);
                        Data.Position := 0;
                        Stream.Position := 0;
                        Graphic.LoadFromStream (Stream);
                    finally
                        FreeAndNil (Stream);
                    end;
                except
                    FreeAndNil (Graphic);
                    raise;
                end;
                f_Bitmap.SetSize (Graphic.Width,Graphic.Height);
                f_Bitmap.Assign (Graphic);
                f_Picture.Graphic.Free;
                f_Picture.Graphic := Graphic;
            end
            else
            begin
                f_Picture.Bitmap.FreeImage;
                f_Bitmap.FreeImage;
            end;
        finally
            FreeAndNil (Input);
            FreeAndNil (HexStream);
        end;
        Mode := Mode + [objImported];
    except on E: Exception do
        _raise (['LoadFromStream',ERR_TPIC_LOAD_FROM_STREAM,E],
                ['{D096A341-1BC7-4897-B817-9707F4A39E7C}']);
    end;
end;

procedure TPic.SaveToStream (out aStream: TStream);
var
    Input     : TStream;
    Output    : TStream;
    HexStream : TStream;
begin
    try
        if not Assigned (aStream) then
            raise Exception.Create (ERR_TPIC_INVALID_STREAM);
        inherited SaveToStream (aStream);
        Mode := Mode - [objExported];
        with aStream do
        begin
            DataFormat := GraphicFormat;
            WriteStrL ( aStream, StrToHex (DataFormat) );
        end;
        Input := TMemoryStream.Create;
        Output := TMemoryStream.Create;
        HexStream := TMemoryStream.Create;
        try
            Input.Position := 0;
            if Assigned (f_Picture.Graphic) and
               ( (f_Picture.Width > 0) or
                 (f_Picture.Height > 0) )
            then
            begin
                f_Picture.Graphic.SaveToStream (Input);
                Compress (Input,Output);
                ByteStreamToHexStream (Output,HexStream);
            end;
            WriteStreamL (aStream,HexStream);
        finally
            FreeAndNil (Input);
            FreeAndNil (Output);
            FreeAndNil (HexStream);
        end;
        Mode := Mode + [objExported];
    except on E: Exception do
        _raise (['SaveToStream',ERR_TPIC_SAVE_TO_STREAM,E],
                ['{F16D8D3B-9692-4DD1-8B0D-09BBC7570A94}']);
    end;
end;
