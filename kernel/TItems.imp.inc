{******************************************************************************}
{*  TItems.imp.inc                                                            *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TItems }
class procedure TItems._raise (anArgs: array of const;
                               const anEGUID: String = '');
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

class procedure TItems._raise (anArgs: array of const;
                               anEGUID: array of const);
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

constructor TItems.Create;
begin
    try
        inherited Create;
        f_List := TList.Create;
    except on E: Exception do
        _raise (['Create',ERR_TITEMS_CREATE,E],
                ['{7E7CE5EA-7A03-48FE-B4E4-005603889A7E}']);
    end;
end;

constructor TItems.Create (anArgs: array of const);
var
    I : Integer;
begin
    try
        Create;
        { параметры - список элементов }
        for I := Low (anArgs) to High (anArgs) do
            if notEmpty (I,anArgs) then
                Add ( toPointer (anArgs [I]) );
    except on E: Exception do
        _raise (['Create',ERR_TITEMS_CREATE,E],
                ['{7619CB18-2051-4749-AB5B-764F4D42BDAE}']);
    end;
end;

destructor TItems.Destroy (doFree: Boolean);
begin
    try
        if doFree then
            FreeAll;
        Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TITEMS_DESTROY,E],
                ['{17ACA95F-8CF6-4033-85BB-4B0B1AAF64A2}']);
    end;
end;

destructor TItems.Destroy;
begin
    try
        Clear;
        FreeAndNil (f_List);
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TITEMS_DESTROY,E],
                ['{0930812E-5455-477F-80DE-33615EA06E0E}']);
    end;
end;

function TItems.GetCount : Integer;
begin
    Result := 0;
    try
        if Assigned (f_List) then
            Result := f_List.Count;
    except on E: Exception do
        _raise (['GetCount',ERR_TITEMS_GET_COUNT,E],
                ['{948BB83E-D57E-402B-A781-572419628BF9}']);
    end;
end;

function TItems.Check (anItem: Pointer) : Boolean;
begin
    Result := FALSE;
    try
        Result := Assigned (anItem);
    except on E: Exception do
        _raise (['Check',ERR_TITEMS_CHECK,E],
                ['{008E417A-4D5D-407D-A994-78EE546802F8}']);
    end;
end;

function TItems.GetItem (anIndex: Integer) : Pointer;
begin
    Result := NIL;
    try
        if ( anIndex < 0 ) or ( anIndex > Count - 1 ) or ( Count <= 0 ) then Exit;
        if ( anIndex >= 0 ) then
            Result := f_List [anIndex]
        {else if ( Count + anIndex >= 0 ) then
            Result := f_List [ Count + anIndex ]}
        else
            raise Exception.CreateFmt (ERR_TITEMS_OUT_OF_BOUNDS,[anIndex]);
    except on E: Exception do
        _raise ([ Format ('GetItem [%d]',[anIndex]), ERR_TITEMS_GET_ITEM, E ],
                ['{8905A78C-51AA-48E4-8021-190FAA2ED5FB}']);
    end;
end;

procedure TItems.SetItem (anIndex: Integer; anItem: Pointer);
begin
    try
        if Check (anItem) then
        begin
            if ( anIndex >= 0 ) then
                f_List [anIndex] := anItem
            {else if ( Count + anIndex >= 0 ) then
                f_List [ Count + anIndex ] := anItem}
            else
                raise Exception.CreateFmt (ERR_TITEMS_OUT_OF_BOUNDS,[anIndex]);
        end;
    except on E: Exception do
        _raise ([ Format ('SetItem [%d]',[anIndex]), ERR_TITEMS_SET_ITEM, E ],
                ['{E761586B-C028-4C42-B620-A90E7D0C7389}']);
    end;
end;

function TItems.GetIndex (anItem: Pointer) : Integer;
begin
    Result := -1;
    try
        if Assigned (f_List) then
            Result := f_List.IndexOf (anItem);
    except on E: Exception do
        _raise ([ Format ('GetIndex [$%s]',[ IntToHex (   LongWord (  Pointer ( (anItem) )  ), 8   ) ]),
                  ERR_TITEMS_GET_INDEX, E ],
                ['{63511197-714B-4D86-8494-B757530F731F}']);
    end;
end;

procedure TItems.SetIndex (anItem: Pointer; aNewIndex: Integer);
var
    id : Integer;
begin
    try
        id := f_List.IndexOf (anItem);
        { если такой элемент есть в списке
          и мы хотим изменить его индекс на индекс другого элемента списка,
          то меняем их местами }
        if ( (id >= 0) and (aNewIndex >= 0) and (aNewIndex < Count) ) then
            f_List.Exchange (id,aNewIndex)
        { //если такого элемента нет в списке
          и мы хотим изменить его индекс на индекс другого элемента списка,
          то инжектируем новый элемент по этому индексу, элементы сдвигаются }
        else if ( {(id < 0) and} (aNewIndex >= 0) and (aNewIndex < Count) ) then
            Insert (aNewIndex,anItem)
        { //если такого элемента нет в списке
          и мы хотим изменить его индекс на индекс последнего элемента списка + 1,
          то добавляем этот элемент в конец списка }
        else if ( {(id < 0) and} (aNewIndex = Count) ) then
            Add (anItem)
        else
            raise Exception.CreateFmt (ERR_TITEMS_OUT_OF_BOUNDS,[aNewIndex]);
    except on E: Exception do
        _raise ([ Format ('SetIndex [$%s]',[ IntToHex (   LongWord (  Pointer ( (anItem) )  ), 8   ) ]),
                  ERR_TITEMS_SET_INDEX, E ],
                ['{0D5EFB99-8D7A-4EB7-B1BD-C87C2CB6AE65}']);
    end;
end;

procedure TItems.Assign (aListA: TItems;
                         anOperation: TListAssignOp = laCopy;
                         aListB: TItems = NIL);
begin
    try
        if Assigned (aListB) then
            f_List.Assign (aListA.f_List,anOperation,aListB.f_List)
        else
            f_List.Assign (aListA.f_List,anOperation,NIL);
    except on E: Exception do
        _raise (['Assign',ERR_TITEMS_ASSIGN,E],
                ['{96D1CAB0-32D7-45D5-B4C3-3A1B13559B4B}']);
    end;
end;

class function TItems.Equals (aListA, aListB: TItems) : Boolean;
var
    I : Integer;
begin
    Result := FALSE;
    try
        Result := Assigned (aListA) and Assigned (aListB)
                  and ( aListA.Count = aListB.Count );
        if Result then
            for I := 0 to aListA.Count - 1 do
            begin
                Result := Result and ( aListA.Item [I] = aListB [I] );
                if not Result then
                    Break;
            end;
    except on E: Exception do
        _raise (['Equals',ERR_TITEMS_EQUALS,E],
                ['{086624FC-5BE4-4C60-97C7-84EDF20A3C14}']);
    end;
end;

function TItems.Add (anItem: Pointer) : Integer;
begin
    Result := -1;
    try
        if Check (anItem) then
            Result := f_List.Add (anItem);
    except on E: Exception do
        _raise (['Add',ERR_TITEMS_ADD,E],
                ['{64D2A790-7906-454B-A6DE-D573B99EBF1C}']);
    end;
end;

function TItems.Add (anArgs: array of const) : Integer;
begin
    Result := -1;
    try
        if notEmpty (0,anArgs) then
            Result := Add ( toPointer (anArgs [0]) );
    except on E: Exception do
        _raise (['Add',ERR_TITEMS_ADD,E],
                ['{477A9EE5-59B7-47BA-B038-51E6C994A001}']);
    end;
end;

function TItems.Insert (anIndex: Integer; anItem: Pointer) : Integer;
begin
    Result := -1;
    try
        if Check (anItem) then
        begin
            f_List.Insert (anIndex,anItem);
            Result := anIndex;
        end;
    except on E: Exception do
        _raise ([ Format ('Insert [%d]',[anIndex]), ERR_TITEMS_INSERT, E ],
                ['{B4FB8D67-C7E2-4EAF-9A45-93A1733DFAB6}']);
    end;
end;

function TItems.Insert (anIndex: Integer; anArgs: array of const) : Integer;
begin
    Result := -1;
    try
        if notEmpty (0,anArgs) then
            Result := Insert ( anIndex, toPointer (anArgs [0]) );
    except on E: Exception do
        _raise ([ Format ('Insert [%d]',[anIndex]), ERR_TITEMS_INSERT, E ],
                ['{A2CBBCA1-9782-45AA-A2D0-D51C06738420}']);
    end;
end;

procedure TItems.Move (aCurrentIndex, aNewIndex: Integer);
begin
    try
        f_List.Move (aCurrentIndex,aNewIndex);
    except on E: Exception do
        _raise ([ Format ('Move [ %d --> %d ]',[aCurrentIndex,aNewIndex]), ERR_TITEMS_MOVE, E ],
                ['{2AE07CBB-929E-4048-A05F-AC9618AB1E92}']);
    end;
end;

class function TItems.Compare (anItemA, anItemB: Pointer) : Integer;
begin
    Result := 0;
    try
        if ( LongInt (anItemA) > LongInt (anItemB) ) then
            Result := 1
        else if ( LongInt (anItemA) < LongInt (anItemB) ) then
            Result := -1
        else
            Result := 0;
    except on E: Exception do
        _raise (['Compare',ERR_TITEMS_COMPARE,E],
                ['{3AE3974F-9F07-4FC2-8FF4-C54B011D052D}']);
    end;
end;

procedure TItems.QuickSort (L, R: Integer);
var
    I, J : Integer;
    P, T : Pointer;
begin
    repeat
        I := L;
        J := R;
        P := Item [ (L + R) shr 1 ];
        repeat
            while ( self.Compare (Item [I],P) < 0 ) do
                System.Inc (I);
            while ( self.Compare (Item [J],P) > 0 ) do
                System.Dec (J);
            if ( I <= J ) then
            begin
                T := Item [I];
                Item [I] := Item [J];
                Item [J] := T;
                System.Inc (I);
                System.Dec (J);
            end;
        until ( I > J );
        if ( L < J ) then
            QuickSort (L,J);
        L := I;
    until ( I >= R );
end;

procedure TItems.Sort;
begin
    try
        if ( Count > 0 ) then
            QuickSort (0,Count-1);
    except on E: Exception do
        _raise (['Sort',ERR_TITEMS_SORT,E],
                ['{CE03861E-AB07-4421-9112-AA7626A307A7}']);
    end;
end;

procedure TItems.Delete (anIndex: Integer; doFree: Boolean = FALSE);
begin
    try
        if ( anIndex < 0 ) or ( anIndex > Count - 1 ) or ( Count <= 0 ) then Exit;
        if doFree then
            FreeItem (anIndex);
        f_List.Delete (anIndex);
        f_List.Capacity := f_List.Count;
    except on E: Exception do
        _raise ([ Format ('Delete [%d]',[anIndex]), ERR_TITEMS_DELETE, E ],
                ['{B6609237-ACD8-4293-98D7-77C1D39403DA}']);
    end;
end;

procedure TItems.Clear (doFree: Boolean = FALSE);
begin
    try
        while ( Count > 0 ) do
            Delete (0,doFree);
    except on E: Exception do
        _raise (['Clear',ERR_TITEMS_CLEAR,E],
                ['{2DAA58A1-D305-443C-BEC5-2B9C6B902257}']);
    end;
end;

procedure TItems.FreeItem (anIndex: Integer);
begin
    try
        if ( anIndex < 0 ) or ( anIndex > Count - 1 ) or ( Count <= 0 ) then Exit;
        if Assigned ( TObject (Item [anIndex]) ) then
        begin
            TObject (Item [anIndex]).Free;
            Item [anIndex] := NIL;
        end
        else if Assigned (Item [anIndex]) then
        begin
            Dispose (Item [anIndex]);
            Item [anIndex] := NIL;
        end;
    except on E: Exception do
        _raise ([ Format ('FreeItem [%d]',[anIndex]), ERR_TITEMS_FREE_ITEM, E ],
                ['{24530C6D-C5D2-4C66-9511-8CE95142AAB3}']);
    end;
end;

procedure TItems.FreeAll;
var
    I : Integer;
begin
    try
        for I := 0 to Count - 1 do
            FreeItem (I);
    except on E: Exception do
        _raise (['FreeAll',ERR_TITEMS_FREE_ALL,E],
                ['{6EFD5756-7FDD-442D-8643-B7D64C7A3B74}']);
    end;
end;
