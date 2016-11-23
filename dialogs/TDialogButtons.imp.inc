{******************************************************************************}
{*  TDialogButtons.imp.inc                                                    *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TDialogButtons }
constructor TDialogButtons.Create (const anOwner: TWinControl;
                                   const anImageList: TCustomImageList;
                                   const anOnClick: TNotifyEvent);
begin
    try
        inherited Create;
        f_Owner := anOwner;
        f_ImageList := anImageList;
        f_OnClick := anOnClick;
    except on E: Exception do
        _raise (['Create',ERR_TDIALOGBUTTONS_CREATE,E],
                ['{71EC809A-F902-439E-B9B3-C19D1C55972C}']);
    end;
end;

constructor TDialogButtons.Create (const anOwner: TWinControl;
                                   const anImageList: TCustomImageList;
                                   const anOnClick: TNotifyEvent;
                                   anArgs: array of const);
var
    I : Integer;
begin
    try
        Create (anOwner,anImageList,anOnClick);
        { параметры - список параметров-конструкторов кнопок }
        for I := Low (anArgs) to High (anArgs) do
            if notEmpty (I,anArgs) then
                Add ( toArrayOfConst (anArgs [I]) );
    except on E: Exception do
        _raise (['Create',ERR_TDIALOGBUTTONS_CREATE,E],
                ['{4996C66B-7E9F-4C2B-8054-8EA8B5D6BA79}']);
    end;
end;

destructor TDialogButtons.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TDIALOGBUTTONS_DESTROY,E],
                ['{64A02B3A-2040-4DB9-820F-4FB636C5CF58}']);
    end;
end;

procedure TDialogButtons.FreeItem (anIndex: Integer);
begin
    try
        if ( anIndex < 0 ) or ( anIndex > Count - 1 ) or ( Count <= 0 ) then Exit;
        if Assigned (ItemAt [anIndex]) and not Assigned (ItemAt [anIndex].Owner) then
        begin
            ItemAt [anIndex].Free;
            Item [anIndex] := NIL;
        end;
    except on E: Exception do
        _raise (['FreeItem',ERR_TDIALOGBUTTONS_FREE_ALL,E],
                ['{79F2DE34-92FA-4C08-8331-1CF77565CCE1}']);
    end;
end;

function TDialogButtons.Check (anItem: Pointer) : Boolean;
begin
    Result := FALSE;
    try
        Result := inherited Check (anItem) and
                  TObject (anItem).InheritsFrom (TDialogButton) and
                  Check ( TDialogButton (anItem) );
    except on E: Exception do
        _raise (['Check',ERR_TDIALOGBUTTONS_CHECK,E],
                ['{32E9B325-E06C-4AA6-9188-97E3E11C9ACF}']);
    end;
end;

function TDialogButtons.Check (anItem: TDialogButton) : Boolean;
begin
    Result := TRUE;
end;

function TDialogButtons.GetItemAt (anIndex: Integer) : TDialogButton;
begin
    Result := NIL;
    try
        Result := TDialogButton (inherited Item [anIndex]);
    except on E: Exception do
        _raise ([ Format ('GetItemAt [%d]',[anIndex]), ERR_TDIALOGBUTTONS_GET_ITEM_AT, E ],
                ['{605946E2-DD64-4AB6-A0C8-8FA25D2485C1}']);
    end;
end;

procedure TDialogButtons.SetItemAt (anIndex: Integer; anItem: TDialogButton);
begin
    try
        inherited Item [anIndex] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemAt [%d]',[anIndex]), ERR_TDIALOGBUTTONS_SET_ITEM_AT, E ],
                ['{8A757BD7-8C93-4B50-8675-5FA1A6EEF8AE}']);
    end;
end;

function TDialogButtons.GetItemOf (aName: String) : TDialogButton;
var
    I     : Integer;
    Found : Boolean;
begin
    Result := NIL;
    try
        Found := FALSE;
        for I := 0 to Count - 1 do
            if Assigned ( TDialogButton (Item [I]) ) and
               (  UpperCase ( TDialogButton (Item [I]).Name ) = UpperCase (aName)  ) then
            begin
                Result := TDialogButton (Item [I]);
                Found := TRUE;
                Break;
            end;
        if not Found then
            raise Exception.CreateFmt (ERR_TDIALOGBUTTONS_NOT_FOUND,[aName]);
    except on E: Exception do
        _raise ([ Format ('GetItemOf [%s]',[aName]), ERR_TDIALOGBUTTONS_GET_ITEM_OF, E ],
                ['{446A431B-6898-46D8-9E0C-95B7753988FE}']);
    end;
end;

procedure TDialogButtons.SetItemOf (aName: String; anItem: TDialogButton);
var
    I     : Integer;
    Found : Boolean;
begin
    try
        Found := FALSE;
        for I := 0 to Count - 1 do
            if Assigned ( TDialogButton (Item [I]) ) and
               (  UpperCase ( TDialogButton (Item [I]).Name ) = UpperCase (aName)  ) then
            begin
                Item [I] := anItem;
                Found := TRUE;
                Break;
            end;
        if not Found then
            raise Exception.CreateFmt (ERR_TDIALOGBUTTONS_NOT_FOUND,[aName]);
    except on E: Exception do
        _raise ([ Format ('SetItemOf [%s]',[aName]), ERR_TDIALOGBUTTONS_SET_ITEM_OF, E ],
                ['{0EB470C1-F2AD-4D76-B8A1-79069481BB79}']);
    end;
end;

class function TDialogButtons.ItemClassType : CDialogButton;
begin
    Result := TDialogButton;
end;

function TDialogButtons.Add (anItem: TDialogButton) : Integer;
begin
    Result := -1;
    try
        Result := inherited Add (anItem);
    except on E: Exception do
        _raise (['Add',ERR_TDIALOGBUTTONS_ADD,E],
                ['{1C1350DD-AC51-43D4-98A8-802EAE72D497}']);
    end;
end;

function TDialogButtons.Add (const anOwner: TWinControl;
                             const anImageList: TCustomImageList;
                             const anOnClick: TNotifyEvent;
                             anArgs: array of const) : Integer;
var
    OBJ : TDialogButton;
begin
    Result := -1;
    try
        OBJ := ItemClassType.Create (anOwner,
                                     anImageList,
                                     anOnClick,
                                     anArgs);
        try
            Result := Add (OBJ);
        finally
            if ( Result < 0 ) then
                FreeAndNil (OBJ);
        end;
    except on E: Exception do
        _raise (['Add',ERR_TDIALOGBUTTONS_ADD,E],
                ['{ED7AF98B-7013-4549-9012-F295B93B50ED}']);
    end;
end;

function TDialogButtons.Add (anArgs: array of const) : Integer;
begin
    Result := -1;
    try
        Result := Add (Owner,
                       ImageList,
                       OnClick,
                       anArgs);
    except on E: Exception do
        _raise (['Add',ERR_TDIALOGBUTTONS_ADD,E],
                ['{DC5BBB88-E392-467C-90A0-3A1B24B57BB5}']);
    end;
end;

procedure TDialogButtons.SetOwner (const anOwner: TWinControl);
var
    I : Integer;
begin
    try
        f_Owner := anOwner;
        for I := 0 to Count - 1 do
        begin
            //ItemAt [I].Owner := Owner;
            ItemAt [I].Parent := Owner;
        end;
    except on E: Exception do
        _raise (['SetOwner',ERR_TDIALOGBUTTONS_SET_OWNER,E],
                ['{170B19FA-59AA-41E8-B7E0-038C419D8D67}']);
    end;
end;

procedure TDialogButtons.SetImageList (const anImageList: TCustomImageList);
var
    I : Integer;
begin
    try
        f_ImageList := anImageList;
        for I := 0 to Count - 1 do
            ItemAt [I].Images := ImageList;
    except on E: Exception do
        _raise (['SetImageList',ERR_TDIALOGBUTTONS_SET_IMAGE_LIST,E],
                ['{2BA25563-70E0-49B8-A8A1-42B0C3EC4058}']);
    end;
end;

procedure TDialogButtons.SetOnClick (const anOnClick: TNotifyEvent);
var
    I : Integer;
begin
    try
        f_OnClick := anOnClick;
        for I := 0 to Count - 1 do
            if Assigned (ItemAt [I]) then
                ItemAt [I].OnClick := OnClick;
    except on E: Exception do
        _raise (['SetOnClick',ERR_TDIALOGBUTTONS_SET_ON_CLICK,E],
                ['{8A04A869-4F75-4F9C-A3DD-0833F63074C8}']);
    end;
end;
