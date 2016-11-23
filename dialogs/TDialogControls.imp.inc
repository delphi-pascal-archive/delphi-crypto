{******************************************************************************}
{*  TDialogControls.imp.inc                                                   *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TDialogControls }
constructor TDialogControls.Create (anOwnerControls: TComponent;
                                    anOwnerLabels: TComponent;
                                    aFields: TDialogFields;
                                    aPopupMenu: TPopupMenu = NIL);
var
    I   : Integer;
    OBJ : TDialogControl;
begin
    try
        inherited Create;
        f_Fields := aFields;
        f_OwnerControls := anOwnerControls;
        f_OwnerLabels := anOwnerLabels;
        f_PopupMenu := aPopupMenu;
        for I := 0 to f_Fields.Count - 1 do
        begin
            OBJ := TDialogControl.Create (OwnerControls,
                                          OwnerLabels,
                                          Fields.ItemAt [I],
                                          PopupMenu);
            Add (OBJ,FALSE);
        end;
        Resize;
    except on E: Exception do
        _raise (['Create',ERR_TDIALOGCONTROLS_CREATE,E],
                ['{1D9CB680-F525-43E3-B7CB-61E4F01C3283}']);
    end;
end;

destructor TDialogControls.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TDIALOGCONTROLS_DESTROY,E],
                ['{6FD81B18-E746-4F4E-9579-2640A9B6FF83}']);
    end;
end;

procedure TDialogControls.Resize;
var
    I             : Integer;
    maxLabelWidth : Integer;
begin
    try
        for I := 0 to Count - 1 do
        begin
            ItemAt [I].Control.Left := 8;
            ItemAt [I].Control.Width := ItemAt [I].Control.Parent.Width - 16;
            if ( I > 0 ) then
                ItemAt [I].Control.Top := ItemAt [I-1].Control.Top +
                                          ItemAt [I-1].Control.Height + 8
            else
                ItemAt [I].Control.Top := 8;
            ItemAt [I].LabelControl.Left := 16;
            ItemAt [I].LabelControl.Top := ItemAt [I].Control.Top + 2;
            ItemAt [I].LabelControl.AutoSize := TRUE;
        end;
        maxLabelWidth := 0;
        for I := 0 to Count - 1 do
            if ( ItemAt [I].LabelControl.Width > maxLabelWidth ) then
                maxLabelWidth := ItemAt [I].LabelControl.Width;
        TWinControl (OwnerControls).Height := 6;
        for I := 0 to Count - 1 do
        begin
            ItemAt [I].LabelControl.AutoSize := FALSE;
            ItemAt [I].LabelControl.Width := maxLabelWidth;
            TWinControl (OwnerControls).Height := TWinControl (OwnerControls).Height +
                                                  ItemAt [I].Control.Height + 8;
        end;
        TWinControl (OwnerLabels).Height := TWinControl (OwnerControls).Height;
    except on E: Exception do
        _raise (['Resize',ERR_TDIALOGCONTROLS_RESIZE,E],
                ['{0E775832-BE86-485B-9A70-20E5B91C79CD}']);
    end;
end;

function TDialogControls.Check (anItem: Pointer) : Boolean;
begin
    Result := FALSE;
    try
        Result := inherited Check (anItem) and
                  TObject (anItem).InheritsFrom (TDialogControl) and
                  Check ( TDialogControl (anItem) );
    except on E: Exception do
        _raise (['Check',ERR_TDIALOGCONTROLS_CHECK,E],
                ['{A7453971-F640-4275-8D98-4C3F43C80F2C}']);
    end;
end;

function TDialogControls.Check (anItem: TDialogControl) : Boolean;
begin
    Result := TRUE;
end;

function TDialogControls.GetItemAt (anIndex: Integer) : TDialogControl;
begin
    Result := NIL;
    try
        Result := TDialogControl (inherited Item [anIndex]);
    except on E: Exception do
        _raise ([ Format ('GetItemAt [%d]',[anIndex]), ERR_TDIALOGCONTROLS_GET_ITEM_AT, E ],
                ['{8AAFC71C-86A8-4D8E-9EDA-69F5018D5026}']);
    end;
end;

procedure TDialogControls.SetItemAt (anIndex: Integer; anItem: TDialogControl);
begin
    try
        inherited Item [anIndex] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemAt [%d]',[anIndex]), ERR_TDIALOGCONTROLS_SET_ITEM_AT, E ],
                ['{B6C69164-BCE9-487F-9FD1-2E708D501972}']);
    end;
end;

function TDialogControls.GetItemOf (aName: String) : TDialogControl;
var
    I     : Integer;
    Found : Boolean;
begin
    Result := NIL;
    try
        Found := FALSE;
        for I := 0 to Count - 1 do
            if (  UpperCase ( TDialogControl (Item [I]).Name ) = UpperCase (aName)  ) then
            begin
                Result := TDialogControl (Item [I]);
                Found := TRUE;
                Break;
            end;
        if not Found then
            raise Exception.CreateFmt (ERR_TDIALOGCONTROLS_NOT_FOUND,[aName]);
    except on E: Exception do
        _raise ([ Format ('GetItemOf [%s]',[aName]), ERR_TDIALOGCONTROLS_GET_ITEM_OF, E ],
                ['{0E91CDB9-9233-444D-8C8F-8B602B8E30D7}']);
    end;
end;

procedure TDialogControls.SetItemOf (aName: String; anItem: TDialogControl);
var
    I     : Integer;
    Found : Boolean;
begin
    try
        Found := FALSE;
        for I := 0 to Count - 1 do
            if (  UpperCase ( TDialogControl (Item [I]).Name ) = UpperCase (aName)  ) then
            begin
                Item [I] := anItem;
                Found := TRUE;
                Break;
            end;
        if not Found then
            raise Exception.CreateFmt (ERR_TDIALOGCONTROLS_NOT_FOUND,[aName]);
    except on E: Exception do
        _raise ([ Format ('SetItemOf [%s]',[aName]), ERR_TDIALOGCONTROLS_SET_ITEM_OF, E ],
                ['{A6B0E4A6-CAD1-4544-9571-93AF474E26BE}']);
    end;
end;

function TDialogControls.GetCaptionAt (anIndex: Integer) : String;
begin
    Result := '';
    try
        Result := ItemAt [anIndex].Caption;
    except on E: Exception do
        _raise ([ Format ('GetCaptionAt [%d]',[anIndex]), ERR_TDIALOGCONTROLS_GET_CAPTION_AT, E ],
                ['{0A8DD62B-6111-4A77-A921-4474D59FF850}']);
    end;
end;

procedure TDialogControls.SetCaptionAt (anIndex: Integer; aValue: String);
begin
    try
        ItemAt [anIndex].Caption := aValue;
    except on E: Exception do
        _raise ([ Format ('SetCaptionAt [%d]',[anIndex]), ERR_TDIALOGCONTROLS_SET_CAPTION_AT, E ],
                ['{495D2FE1-F128-4760-9E09-C2B664C60CF9}']);
    end;
end;

function TDialogControls.GetCaptionOf (aName: String) : String;
begin
    Result := '';
    try
        Result := ItemOf [aName].Caption;
    except on E: Exception do
        _raise ([ Format ('GetCaptionOf [%s]',[aName]), ERR_TDIALOGCONTROLS_GET_CAPTION_OF, E ],
                ['{F5CE2A68-66CB-4F81-977A-5F632D3948C1}']);
    end;
end;

procedure TDialogControls.SetCaptionOf (aName: String; aValue: String);
begin
    try
        ItemOf [aName].Caption := aValue;
    except on E: Exception do
        _raise ([ Format ('SetCaptionOf [%s]',[aName]), ERR_TDIALOGCONTROLS_SET_CAPTION_OF, E ],
                ['{3CF6997A-0892-4880-96AF-4F9AD8EF99FF}']);
    end;
end;

function TDialogControls.GetValueAt (anIndex: Integer) : Variant;
begin
    Result := NULL;
    try
        Result := ItemAt [anIndex].Value;
    except on E: Exception do
        _raise ([ Format ('GetValueAt [%d]',[anIndex]), ERR_TDIALOGCONTROLS_GET_VALUE_AT, E ],
                ['{78C3B172-9D70-4910-AFC6-0950A45F14BF}']);
    end;
end;

procedure TDialogControls.SetValueAt (anIndex: Integer; aValue: Variant);
begin
    try
        ItemAt [anIndex].Value := aValue;
    except on E: Exception do
        _raise ([ Format ('SetValueAt [%d]',[anIndex]), ERR_TDIALOGCONTROLS_SET_VALUE_AT, E ],
                ['{FDF8EB06-C1BD-4C75-A0C8-C5CA14BEE8E2}']);
    end;
end;

function TDialogControls.GetValueOf (aName: String) : Variant;
begin
    Result := NULL;
    try
        Result := ItemOf [aName].Value;
    except on E: Exception do
        _raise ([ Format ('GetValueOf [%s]',[aName]), ERR_TDIALOGCONTROLS_GET_VALUE_OF, E ],
                ['{2479A8D3-3368-4CB3-9FA6-5AD8FCBC5886}']);
    end;
end;

procedure TDialogControls.SetValueOf (aName: String; aValue: Variant);
begin
    try
        ItemOf [aName].Value := aValue;
    except on E: Exception do
        _raise ([ Format ('SetValueOf [%s]',[aName]), ERR_TDIALOGCONTROLS_SET_VALUE_OF, E ],
                ['{DE4FF410-5B84-45FB-AA28-05779607F20B}']);
    end;
end;

class function TDialogControls.ItemClassType : CDialogControl;
begin
    Result := TDialogControl;
end;

function TDialogControls.Add (anItem: TDialogControl; doResize: Boolean = TRUE) : Integer;
begin
    Result := -1;
    try
        try
            Result := inherited Add (anItem);
        finally
            if ( Result >= 0 ) then
            begin
                if ( Count > 0 ) then
                    TWinControl (anItem.Control).TabOrder := TWinControl (ItemAt [Count-1].Control).TabOrder + 1;
                if doResize then
                    Resize;
            end;
        end;
    except on E: Exception do
        _raise (['Add',ERR_TDIALOGCONTROLS_ADD,E],
                ['{683EB600-052D-4DCE-BE3C-A1F10175718D}']);
    end;
end;

function TDialogControls.Add (aField: TDialogField; doResize: Boolean = TRUE) : Integer;
var
    OBJ : TDialogControl;
begin
    Result := -1;
    try
        OBJ := TDialogControl.Create (OwnerControls,
                                      OwnerLabels,
                                      aField,
                                      PopupMenu);
        try
            Result := Add (OBJ,doResize);
        finally
            if ( Result < 0 ) then
                FreeAndNil (OBJ);
        end;
    except on E: Exception do
        _raise (['Add',ERR_TDIALOGCONTROLS_ADD,E],
                ['{D6FD427F-9025-41B5-BA3B-79B37DC3B84C}']);
    end;
end;

function TDialogControls.Add (anArgs: array of const; doResize: Boolean = TRUE) : Integer;
var
    OBJ   : TDialogControl;
    Field : TDialogField;
begin
    Result := -1;
    try
        Field := TDialogField.Create (anArgs);
        OBJ := TDialogControl.Create (OwnerControls,
                                      OwnerLabels,
                                      Field,
                                      PopupMenu);
        try
            Result := Add (OBJ,doResize);
        finally
            if ( Result < 0 ) then
            begin
                FreeAndNil (OBJ);
                FreeAndNil (Field);
            end;
        end;
    except on E: Exception do
        _raise (['Add',ERR_TDIALOGCONTROLS_ADD,E],
                ['{CA18B316-DD8F-4A3C-8C8F-EC49E8B8C3E8}']);
    end;
end;

procedure TDialogControls.Delete (anIndex: Integer; doFree: Boolean = FALSE; doResize: Boolean = TRUE);
begin
    try
        inherited Delete (anIndex,doFree);
        if doResize then
            Resize;
    except on E: Exception do
        _raise (['Delete',ERR_TDIALOGCONTROLS_DELETE,E],
                ['{7000C1C2-9071-4ADD-AD26-AA38E89868DA}']);
    end;
end;

procedure TDialogControls.Clear (doFree: Boolean = FALSE; doResize: Boolean = TRUE);
begin
    try
        inherited Clear (doFree);
        if doResize then
            Resize;
    except on E: Exception do
        _raise (['Clear',ERR_TDIALOGCONTROLS_CLEAR,E],
                ['{614A0498-7F33-4CA1-898C-30AC256518AA}']);
    end;
end;
