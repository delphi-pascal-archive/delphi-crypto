{******************************************************************************}
{*  TKeyWords.imp.inc                                                         *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2012                                             *}
{******************************************************************************}
{ TKeyWords }
function TKeyWords.Check (anItem: Pointer) : Boolean;
begin
    Result := FALSE;
    try
        Result := inherited Check (anItem) and
                  TObject (anItem).InheritsFrom (TKeyWord) and
                  Check ( TKeyWord (anItem) );
    except on E: Exception do
        _raise (['Check',ERR_TKEYWORDS_CHECK,E],
                ['{0AFBED13-E658-40AF-A44C-96CF70983760}']);
    end;
end;

function TKeyWords.Check (anItem: TKeyWord) : Boolean;
begin
    Result := Assigned (anItem) and anItem.Check;
end;

function TKeyWords.GetItemAt (anIndex: Integer) : TKeyWord;
begin
    Result := NIL;
    try
        if inherited ItemAt [anIndex].InheritsFrom (TKeyWord) then
            Result := inherited ItemAt [anIndex] as TKeyWord;
    except on E: Exception do
        _raise ([ Format ('GetItemAt [%d]',[anIndex]), ERR_TKEYWORDS_GET_ITEM_AT, E ],
                ['{6517E9BD-173E-4BA2-9949-F42CA528FD81}']);
    end;
end;

procedure TKeyWords.SetItemAt (anIndex: Integer; anItem: TKeyWord);
begin
    try
        if inherited ItemAt [anIndex].InheritsFrom (TKeyWord) then
            inherited ItemAt [anIndex] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemAt [%d]',[anIndex]), ERR_TKEYWORDS_SET_ITEM_AT, E ],
                ['{95CCBAB8-9C55-4350-87BC-0D0A8170C1A0}']);
    end;
end;

function TKeyWords.GetItemOn (anID: TID) : TKeyWord;
begin
    Result := NIL;
    try
        Result := inherited ItemOn [anID] as TKeyWord;
    except on E: Exception do
        _raise ([ Format ('GetItemOn [%d]',[anID]), ERR_TKEYWORDS_GET_ITEM_ON, E ],
                ['{B985FBF8-D857-45E0-AA49-35F2CEEB0DF9}']);
    end;
end;

procedure TKeyWords.SetItemOn (anID: TID; anItem: TKeyWord);
begin
    try
        inherited ItemOn [anID] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemOn [%d]',[anID]), ERR_TKEYWORDS_SET_ITEM_ON, E ],
                ['{09467D94-9895-4AA0-95CB-021DC83AB96B}']);
    end;
end;

class function TKeyWords.ItemClassType : CMetaObject;
begin
    Result := TKeyWord;
end;

function TKeyWords.Add (anItem: TKeyWord) : Integer;
begin
    Result := -1;
    try
        Result := inherited Add (anItem);
    except on E: Exception do
        _raise (['Add',ERR_TKEYWORDS_ADD,E],
                ['{84CFE662-7382-43DE-9F04-013A38D62A10}']);
    end;
end;

class procedure TKeyWords.Filter (var Query: String;
                                  anArgs: array of const);
var
    IDs           : TVarRec;
    OBJ           : TObject;
    arr           : array_of_const;
    I             : Integer;
    Props         : TProtoProperties;
    Prop          : TMetaProperty;
    Hashes        : String;
    IDsCategories : String;
    IDsMessages   : String;
    IDsAuthors    : String;
    IDsOwners     : String;
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
            Prop := TMetaProperty (Props.ItemAt [_kwd_hash]);
            { второй параметр - список ключевых слов }
            Hashes := '';
            if notEmpty (1,anArgs) then
            begin
                OBJ := toObject (anArgs [1]);
                if Assigned (OBJ) and OBJ.InheritsFrom (TStringList) then
                begin
                    for I := 0 to TStringList (OBJ).Count - 1 do
                    begin
                        Prop.asHex := TKeyWord.CalcHash (TStringList (OBJ).Strings [I]);
                        if ( I = 0 ) then
                            Hashes := Format ('%s="%s"',[ Prop.Name, Prop.asDBFormat ])
                        else
                            Hashes := Format ('%s OR %s="%s"',[ Hashes,
                                                                Prop.Name, Prop.asDBFormat ]);
                    end;
                end
                else
                begin
                    arr := toArrayOfConst (anArgs [1]);
                    for I := 0 to High (arr) do
                    begin
                        Prop.asHex := TKeyWord.CalcHash (toString (arr [I]));
                        if ( I = 0 ) then
                            Hashes := Format ('%s="%s"',[ Prop.Name, Prop.asDBFormat ])
                        else
                            Hashes := Format ('%s OR %s="%s"',[ Hashes,
                                                                Prop.Name, Prop.asDBFormat ]);
                    end;
                end;
            end;

            Prop := TMetaProperty (Props.ItemAt [_kwd_id_categorie]);
            { третий параметр - список идентификаторов категорий }
            IDsCategories := '';
            if notEmpty (2,anArgs) then
            begin
                OBJ := toObject (anArgs [2]);
                if Assigned (OBJ) and OBJ.InheritsFrom (TStringList) then
                begin
                    for I := 0 to TStringList (OBJ).Count - 1 do
                    begin
                        Prop.asString := TStringList (OBJ).Strings [I];
                        if ( Prop.asID >= 0 ) then
                            if ( I = 0 ) then
                                IDsCategories := Prop.asDBFormat
                            else
                                IDsCategories := Format ('%s,%s',[ IDsCategories, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDsCategories) > 0 ) then
                        IDsCategories := Format ('%s IN (%s) ',[ Prop.Name, IDsCategories ]);
                end
                else
                begin
                    arr := toArrayOfConst (anArgs [2]);
                    for I := 0 to High (arr) do
                    begin
                        Prop.asID := toInt64 (arr [I]);
                        if ( Prop.asID >= 0 ) then
                            if ( I = 0 ) then
                                IDsCategories := Prop.asDBFormat
                            else
                                IDsCategories := Format ('%s,%s',[ IDsCategories, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDsCategories) > 0 ) then
                        IDsCategories := Format ('%s IN (%s) ',[ Prop.Name, IDsCategories ]);
                end;
            end;

            Prop := TMetaProperty (Props.ItemAt [_kwd_id_message]);
            { четвертый параметр - список идентификаторов сообщений }
            IDsMessages := '';
            if notEmpty (3,anArgs) then
            begin
                OBJ := toObject (anArgs [3]);
                if Assigned (OBJ) and OBJ.InheritsFrom (TStringList) then
                begin
                    for I := 0 to TStringList (OBJ).Count - 1 do
                    begin
                        Prop.asString := TStringList (OBJ).Strings [I];
                        if ( Prop.asID >= 0 ) then
                            if ( I = 0 ) then
                                IDsMessages := Prop.asDBFormat
                            else
                                IDsMessages := Format ('%s,%s',[ IDsMessages, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDsMessages) > 0 ) then
                        IDsMessages := Format ('%s IN (%s) ',[ Prop.Name, IDsMessages ]);
                end
                else
                begin
                    arr := toArrayOfConst (anArgs [3]);
                    for I := 0 to High (arr) do
                    begin
                        Prop.asID := toInt64 (arr [I]);
                        if ( Prop.asID >= 0 ) then
                            if ( I = 0 ) then
                                IDsMessages := Prop.asDBFormat
                            else
                                IDsMessages := Format ('%s,%s',[ IDsMessages, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDsMessages) > 0 ) then
                        IDsMessages := Format ('%s IN (%s) ',[ Prop.Name, IDsMessages ]);
                end;
            end;

            Prop := TMetaProperty (Props.ItemAt [_kwd_id_author]);
            { пятый параметр - список идентификаторов авторов }
            IDsAuthors := '';
            if notEmpty (4,anArgs) then
            begin
                OBJ := toObject (anArgs [4]);
                if Assigned (OBJ) and OBJ.InheritsFrom (TStringList) then
                begin
                    for I := 0 to TStringList (OBJ).Count - 1 do
                    begin
                        Prop.asString := TStringList (OBJ).Strings [I];
                        if ( Prop.asID >= 0 ) then
                            if ( I = 0 ) then
                                IDsAuthors := Prop.asDBFormat
                            else
                                IDsAuthors := Format ('%s,%s',[ IDsAuthors, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDsAuthors) > 0 ) then
                        IDsAuthors := Format ('%s IN (%s) ',[ Prop.Name, IDsAuthors ]);
                end
                else
                begin
                    arr := toArrayOfConst (anArgs [4]);
                    for I := 0 to High (arr) do
                    begin
                        Prop.asID := toInt64 (arr [I]);
                        if ( Prop.asID >= 0 ) then
                            if ( I = 0 ) then
                                IDsAuthors := Prop.asDBFormat
                            else
                                IDsAuthors := Format ('%s,%s',[ IDsAuthors, Prop.asDBFormat ]);
                    end;
                    if ( Length (IDsAuthors) > 0 ) then
                        IDsAuthors := Format ('%s IN (%s) ',[ Prop.Name, IDsAuthors ]);
                end;
            end;

            Prop := TMetaProperty (Props.ItemAt [_kwd_id_owner]);
            { шестой параметр - список идентификаторов владельцев }
            IDsOwners := '';
            if notEmpty (5,anArgs) then
            begin
                OBJ := toObject (anArgs [5]);
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
                    arr := toArrayOfConst (anArgs [5]);
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
        if ( Length (Hashes) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,Hashes]);
        if ( Length (IDsCategories) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,IDsCategories]);
        if ( Length (IDsMessages) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,IDsMessages]);
        if ( Length (IDsAuthors) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,IDsAuthors]);
        if ( Length (IDsOwners) > 0 ) then
            query := Format ('%s AND ( %s ) ',[query,IDsOwners]);
    except on E: Exception do
        _raise (['Filter',ERR_TKEYWORDS_FILTER,E],
                ['{D94199B8-3A3F-4EB4-A198-139D49ED1AC7}']);
    end;
end;
