{******************************************************************************}
{*  THashList.imp.inc                                                         *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011-2012                                        *}
{******************************************************************************}
{ THashList }
function THashList.Check (anItem: Pointer) : Boolean;
begin
    Result := FALSE;
    try
        Result := inherited Check (anItem) and
                  TObject (anItem).InheritsFrom (THashItem) and
                  Check ( THashItem (anItem) );
    except on E: Exception do
        _raise (['Check',ERR_THASHLIST_CHECK,E],
                ['{D8B394A7-9279-4726-836B-B7B3664324FD}']);
    end;
end;

function THashList.Check (anItem: THashItem) : Boolean;
begin
    Result := Assigned (anItem) and anItem.Check;
end;

class function THashList.ItemClassType : CProtoObject;
begin
    Result := THashItem;
end;

function THashList.Add (anItem: THashItem) : Integer;
begin
    Result := -1;
    try
        anItem.ObjectName := anItem.KeyHash;
        Result := inherited Add (anItem);
    except on E: Exception do
        _raise (['Add',ERR_THASHLIST_ADD,E],
                ['{5FB25BF7-E84B-4AF4-9D9A-45C7CE846016}']);
    end;
end;

class function THashList.Compare (anItemA, anItemB: Pointer) : Integer;
begin
    Result := 0;
    try
        Result := inherited Compare (anItemA,anItemB);
        Result := AnsiCompareText ( THashItem (anItemA).KeyHash,
                                    THashItem (anItemB).KeyHash );
    except on E: Exception do
        _raise (['Compare',ERR_THASHLIST_COMPARE,E],
                ['{E0513A0C-9F2C-440A-AFA6-E6AAA9BFC160}']);
    end;
end;

class function THashList.Substract (const aListA, aListB: THashList) : THashList;
var
    I, J  : Integer;
    Found : Boolean;
    OBJ   : TProtoObject;
begin
    Result := NIL;
    try
        if not Assigned (aListA) then
            raise Exception.Create (ERR_THASHLIST_INVALID_LIST);
        if not Assigned (aListB) then
            raise Exception.Create (ERR_THASHLIST_INVALID_LIST);
        { из списка A вычитает список B :
          в результирующем списке будут те элементы,
          хэш-ключи которых есть в A, но нет в B }
        Result := THashList.Create ([]);
        if ( aListA.Count = 0 ) and ( aListB.Count = 0 ) then
            Exit;
        for I := 0 to aListA.Count - 1 do
        begin
            Found := FALSE;
            for J := 0 to aListB.Count - 1 do
            begin
                Found := ( aListB.ItemAt [J].KeyHash = aListA.ItemAt [I].KeyHash );
                if Found then Break;
            end;
            if not Found then
            begin
                OBJ := ItemClassType.Create ([],[]);
                with THashItem (OBJ) do
                begin
                    KeyHash := aListA.ItemAt [I].KeyHash;
                    DataHash := aListA.ItemAt [I].DataHash;
                    TimeStampVector := aListA.ItemAt [I].TimeStampVector;
                end;
                if ( Result.Add (OBJ) < 0 ) then
                    FreeAndNil (OBJ);
            end;
        end;
    except on E: Exception do
        _raise (['Substract',ERR_THASHLIST_SUBSTRACT,E],
                ['{F67CBAF4-605E-47C4-8362-15F86413AF91}']);
    end;
end;

class function THashList.Extract (const aListA, aListB: THashList) : THashList;
var
    I, J  : Integer;
    Found : Boolean;
    OBJ   : TProtoObject;
begin
    Result := NIL;
    try
        if not Assigned (aListA) then
            raise Exception.Create (ERR_THASHLIST_INVALID_LIST);
        if not Assigned (aListB) then
            raise Exception.Create (ERR_THASHLIST_INVALID_LIST);
        { экстрагирует обновления для списка B из списка A :
          в результирующем списке будут те элементы,
          которые имеют те же хэш-ключи, что в B,
          но более позднее векторное время
          или то же векторное время,
          но другой хэш данных ( обновления из списка A ) }
        Result := THashList.Create ([]);
        if ( aListA.Count = 0 ) and ( aListB.Count = 0 ) then
            Exit;
        for I := 0 to aListB.Count - 1 do
        begin
            Found := FALSE;
            for J := 0 to aListA.Count - 1 do
            begin
                Found := ( aListA.ItemAt [J].KeyHash = aListB.ItemAt [I].KeyHash );
                if Found then Break;
            end;
            if Found and
            ( (aListA.ItemAt [J].TimeStampVector > aListB.ItemAt [I].TimeStampVector)
              or
              ( (aListA.ItemAt [J].TimeStampVector = aListB.ItemAt [I].TimeStampVector)
                and
                (aListA.ItemAt [J].DataHash <> aListB.ItemAt [I].DataHash)
              )
            ) then
            begin
                OBJ := ItemClassType.Create ([],[]);
                with THashItem (OBJ) do
                begin
                    KeyHash := aListA.ItemAt [J].KeyHash;
                    DataHash := aListA.ItemAt [J].DataHash;
                    TimeStampVector := aListA.ItemAt [J].TimeStampVector;
                end;
                if ( Result.Add (OBJ) < 0 ) then
                    FreeAndNil (OBJ);
            end;
        end;
    except on E: Exception do
        _raise (['Extract',ERR_THASHLIST_EXTRACT,E],
                ['{FFFEF6C7-F4C8-4540-AF37-6E15700AD15A}']);
    end;
end;

function THashList.GetItemAt (anIndex: Integer) : THashItem;
begin
    Result := NIL;
    try
        if inherited ItemAt [anIndex].InheritsFrom (THashItem) then
            Result := inherited ItemAt [anIndex] as THashItem;
    except on E: Exception do
        _raise ([ Format ('GetItemAt [%d]',[anIndex]), ERR_THASHLIST_GET_ITEM_AT, E ],
                ['{66516380-2D98-4780-BF5C-D8B16BE4DC99}']);
    end;
end;

procedure THashList.SetItemAt (anIndex: Integer; anItem: THashItem);
begin
    try
        if inherited ItemAt [anIndex].InheritsFrom (THashItem) then
            inherited ItemAt [anIndex] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemAt [%d]',[anIndex]), ERR_THASHLIST_SET_ITEM_AT, E ],
                ['{E1C47A7E-6A49-4606-B5FA-A410BA6E91A1}']);
    end;
end;

function THashList.GetItemOf (aName: String) : THashItem;
begin
    Result := NIL;
    try
        Result := inherited ItemOf [aName] as THashItem;
    except on E: Exception do
        _raise ([ Format ('GetItemOf [%s]',[aName]), ERR_THASHLIST_GET_ITEM_OF, E ],
                ['{E58DD4C9-817F-49EF-891B-8C84A315FDFE}']);
    end;
end;

procedure THashList.SetItemOf (aName: String; anItem: THashItem);
begin
    try
        inherited ItemOf [aName] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemOf [%s]',[aName]), ERR_THASHLIST_SET_ITEM_OF, E ],
                ['{7EA77BC3-2D16-4FC8-8483-094DEA28F831}']);
    end;
end;

function THashList.CalcMercleKeyHash : Hex;
var
    lst : String;
    I   : Integer;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto) then
        try
            lst := '';
            for I := 0 to Count - 1 do
                if ( I = 0 ) then
                    lst := ItemAt [I].KeyHash
                else
                    lst := Format ('%s_%s',[ lst, ItemAt [I].KeyHash ]);
            Result := User.Crypto.Hash (lst);
        finally
            _FillChar ( lst, Length (lst), $00 );
        end;
    except on E: Exception do
        _raise (['CalcMercleKeyHash',ERR_THASHLIST_CALC_MERCLE_KEY_HASH,E],
                ['{8C4A58F0-1B22-4FAF-BE7B-3178CCC41B2D}']);
    end;
end;

function THashList.CalcMercleDataHash : Hex;
var
    lst : String;
    I   : Integer;
begin
    Result := '';
    try
        if Assigned (User) and Assigned (User.Crypto) then
        try
            lst := '';
            for I := 0 to Count - 1 do
                if ( I = 0 ) then
                    lst := ItemAt [I].DataHash
                else
                    lst := Format ('%s_%s',[ lst, ItemAt [I].DataHash ]);
            Result := User.Crypto.Hash (lst);
        finally
            _FillChar ( lst, Length (lst), $00 );
        end;
    except on E: Exception do
        _raise (['CalcMercleDataHash',ERR_THASHLIST_CALC_MERCLE_DATA_HASH,E],
                ['{DE8D6977-DDB2-40FF-8F07-F59B93F90E9A}']);
    end;
end;

procedure THashList.LoadFromStream (const aStream: TStream);
var
    Item : THashItem;
begin
    try
        if not Assigned (aStream) then
            raise Exception.Create (ERR_THASHLIST_INVALID_STREAM);
        aStream.Position := 0;
        Clear (TRUE);
        with aStream do
        while ( Position < Size ) do
        begin
            Item := THashItem.Create ([]);
            Item.KeyHash := ReadStrL (aStream);
            Item.DataHash := ReadStrL (aStream);
            Item.TimeStampVector := HexToInt ( ReadStrL (aStream) );
            if ( Add (Item) < 0 ) then
                FreeAndNil (Item);
        end;
    except on E: Exception do
        _raise (['LoadFromStream',ERR_THASHLIST_LOAD_FROM_STREAM,E],
                ['{53D6931D-5259-48E1-BECD-E90D35D8DE4D}']);
    end;
end;

procedure THashList.SaveToStream (out aStream: TStream);
var
    I : Integer;
begin
    try
        if not Assigned (aStream) then
            raise Exception.Create (ERR_THASHLIST_INVALID_STREAM);
        aStream.Position := 0;
        for I := 0 to Count - 1 do
        begin
            with aStream do
            begin
                WriteStrL (aStream, ItemAt [I].KeyHash);
                WriteStrL (aStream, ItemAt [I].DataHash);
                WriteStrL ( aStream, IntToHex (ItemAt [I].TimeStampVector,8) );
            end;
        end;
    except on E: Exception do
        _raise (['SaveToStream',ERR_THASHLIST_SAVE_TO_STREAM,E],
                ['{5048C58D-4AF2-40A9-8A7A-A7639D839708}']);
    end;
end;
