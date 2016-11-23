{******************************************************************************}
{*  TPics.imp.inc                                                             *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TPics }
function TPics.Check (anItem: Pointer) : Boolean;
begin
    Result := FALSE;
    try
        Result := inherited Check (anItem) and
                  TObject (anItem).InheritsFrom (TPic) and
                  Check ( TPic (anItem) );
    except on E: Exception do
        _raise (['Check',ERR_TPICS_CHECK,E],
                ['{EE8BF531-DF4A-4391-AB2D-138710B4EAF4}']);
    end;
end;

function TPics.Check (anItem: TPic) : Boolean;
begin
    Result := Assigned (anItem) and anItem.Check;
end;

function TPics.GetItemAt (anIndex: Integer) : TPic;
begin
    Result := NIL;
    try
        if inherited ItemAt [anIndex].InheritsFrom (TPic) then
            Result := inherited ItemAt [anIndex] as TPic;
    except on E: Exception do
        _raise ([ Format ('GetItemAt [%d]',[anIndex]), ERR_TPICS_GET_ITEM_AT, E ],
                ['{D7B9C778-B026-4B30-966E-C656C9C74497}']);
    end;
end;

procedure TPics.SetItemAt (anIndex: Integer; anItem: TPic);
begin
    try
        if inherited ItemAt [anIndex].InheritsFrom (TPic) then
            inherited ItemAt [anIndex] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemAt [%d]',[anIndex]), ERR_TPICS_SET_ITEM_AT, E ],
                ['{DECF9581-4525-45C6-B5B6-79AE9B93AF1E}']);
    end;
end;

function TPics.GetItemOf (aName: String) : TPic;
begin
    Result := NIL;
    try
        Result := inherited ItemOf [aName] as TPic;
    except on E: Exception do
        _raise ([ Format ('GetItemOf [%s]',[aName]), ERR_TPICS_GET_ITEM_OF, E ],
                ['{94FB3B64-872F-45FD-8544-3062B6B5F28D}']);
    end;
end;

procedure TPics.SetItemOf (aName: String; anItem: TPic);
begin
    try
        inherited ItemOf [aName] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemOf [%s]',[aName]), ERR_TPICS_SET_ITEM_OF, E ],
                ['{9ABF4518-4311-4E95-A8E5-63B89CBB78E9}']);
    end;
end;

function TPics.GetItemOn (anID: TID) : TPic;
begin
    Result := NIL;
    try
        Result := inherited ItemOn [anID] as TPic;
    except on E: Exception do
        _raise ([ Format ('GetItemOn [%d]',[anID]), ERR_TPICS_GET_ITEM_ON, E ],
                ['{6816D25B-3305-4DFF-9334-2A530070191D}']);
    end;
end;

procedure TPics.SetItemOn (anID: TID; anItem: TPic);
begin
    try
        inherited ItemOn [anID] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemOn [%d]',[anID]), ERR_TPICS_SET_ITEM_ON, E ],
                ['{B5B90C87-36BD-4E4B-B88F-62994190B8A9}']);
    end;
end;

class function TPics.ItemClassType : CMetaObject;
begin
    Result := TPic;
end;

function TPics.Add (anItem: TPic) : Integer;
begin
    Result := -1;
    try
        Result := inherited Add (anItem);
    except on E: Exception do
        _raise (['Add',ERR_TPICS_ADD,E],
                ['{722450EA-C48A-4E29-ADF8-BEAABD964340}']);
    end;
end;

class function TPics.Find (const aDB: TSQLiteDatabase;
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
            raise Exception.Create (ERR_TPICS_INVALID_DATABASE);
        Props := ItemClassType.GetClassProperties;
        try
            Props.ItemAt [_pic_key_hash].asHex := aHash;
            Props.ItemAt [_pic_id_owner].asInt64 := anIDOwner;

            query := Format ('SELECT %s '+
                             'FROM %s '+
                             'WHERE ( %s="%s" ) '+
                               'AND ( %s="%s" ) ',
                             [ Props.ItemAt [_id].Name,
                               ItemClassType.GetClassTable,
                               Props.ItemAt [_pic_key_hash].Name, TMetaProperty (Props.ItemAt [_pic_key_hash]).asDBFormat,
                               Props.ItemAt [_pic_id_owner].Name, TMetaProperty (Props.ItemAt [_pic_id_owner]).asDBFormat ]);
            //ShowMessage (query);
            table := aDB.GetTable (query);
            with table do
            try
                for I := 0 to table.Count-1 do
                begin
                    if ( I > 0 ) then
                        raise Exception.CreateFmt (ERR_TPIC_NOT_UNIQUE_HASH,
                                                   [ Props.ItemAt [_pic_key_hash].Name,
                                                     TMetaProperty (Props.ItemAt [_pic_key_hash]).asDBFormat ]);
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
        _raise (['Find',ERR_TPICS_FIND,E],
                ['{70B7180C-1FC7-4777-A10E-C11BDE3E0A09}']);
    end;
end;

class procedure TPics.Filter (var Query: String;
                               anArgs: array of const);
var
    IDs              : TVarRec;
    TimeStampsCreate : TVarRec;
    TimeStampsModify : TVarRec;
    TimeStampsPublic : TVarRec;
    Versions         : TVarRec;
    OBJ              : TObject;
    arr              : array_of_const;
    I                : Integer;
    Props            : TProtoProperties;
    Prop             : TMetaProperty;
    IDsOwners        : String;
begin
    try
        { первый параметр - список идентификаторов }
        if notEmpty (0,anArgs) then
            IDs := anArgs [0];
        { третий параметр - период создания }
        if notEmpty (2,anArgs) then
            TimeStampsCreate := anArgs [2];
        { четвертый параметр - период последнего редактирования }
        if notEmpty (3,anArgs) then
            TimeStampsModify := anArgs [3];
        { пятый параметр - период публикации }
        if notEmpty (4,anArgs) then
            TimeStampsPublic := anArgs [4];
        { шестой параметр - допустимый период версий }
        if notEmpty (5,anArgs) then
            Versions := anArgs [5];
        { накладываем фильтр по алгоритму предка }
        arr := _([ NIL, NIL, NIL, NIL, NIL ]);
        arr [0] := IDs;
        arr [1] := TimeStampsCreate;
        arr [2] := TimeStampsModify;
        arr [3] := TimeStampsPublic;
        arr [4] := Versions;
        inherited Filter (Query,arr);
        { накладываем дополнительный фильтр }
        Props := ItemClassType.GetClassProperties;
        try
            Prop := TMetaProperty (Props.ItemAt [_pic_id_owner]);
            { второй параметр - список идентификаторов владельцев }
            IDsOwners := '';
            if notEmpty (1,anArgs) then
            begin
                OBJ := toObject (anArgs [1]);
                if Assigned (OBJ) and OBJ.InheritsFrom (TStringList) then
                begin
                    for I := 0 to TStringList (OBJ).Count - 1 do
                    begin
                        Prop.asString := TStringList (OBJ).Strings [I];
                        if ( Prop.asID >= 0 ) then
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
                    arr := toArrayOfConst (anArgs [1]);
                    for I := 0 to High (arr) do
                    begin
                        Prop.asID := toInt64 (arr [I]);
                        if ( Prop.asID >= 0 ) then
                            if ( I = 0 ) then
                                IDsOwners := Prop.asDBFormat
                            else
                                IDsOwners := Format ('%s,%s',[ IDsOwners, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDsOwners) > 0 ) then
                        IDsOwners := Format ('%s IN (%s) ',[ Prop.Name, IDsOwners ]);
                end;
            end;
        finally
            Props.FreeAll;
            FreeAndNil (Props);
        end;
        if ( Length (IDsOwners) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,IDsOwners]);
    except on E: Exception do
        _raise (['Filter',ERR_TPICS_FILTER,E],
                ['{33AD7C5B-5CBF-48C5-89C7-93310041CFBD}']);
    end;
end;

class procedure TPics.OnLoad (const aDB: TSQLiteDatabase;
                              anObject: TMetaObject;
                              aTable: TSQLiteTable);
var
    Graphic : TGraphic;
    Stream  : TStream;
begin
    try
        inherited OnLoad (aDB,anObject,aTable);
        with TPic (anObject) do
        try
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
                f_Bitmap.Assign (Graphic);
                f_Picture.Graphic.Free;
                f_Picture.Graphic := Graphic;
            end
            else
            begin
                f_Picture.Bitmap.FreeImage;
                f_Bitmap.FreeImage;
            end;
            Mode := Mode + [objLoaded];
        except
            Mode := Mode + [objLoadFailed];
            raise;
        end;
    except on E: Exception do
        _raise (['OnLoad',ERR_TPICS_LOAD,E],
                ['{2C304FDA-1773-4615-8DA8-D0F0CAE0B648}']);
    end;
end;