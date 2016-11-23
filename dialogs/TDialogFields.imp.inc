{******************************************************************************}
{*  TDialogFields.imp.inc                                                     *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TDialogFields }
function TDialogFields.Check (anItem: Pointer) : Boolean;
begin
    Result := FALSE;
    try
        Result := inherited Check (anItem) and
                  TObject (anItem).InheritsFrom (TDialogField) and
                  Check ( TDialogField (anItem) );
    except on E: Exception do
        _raise (['Check',ERR_TDIALOGFIELDS_CHECK,E],
                ['{1EB661CA-BFD6-4D34-8C1D-BDA18D5B1D60}']);
    end;
end;

function TDialogFields.Check (anItem: TDialogField) : Boolean;
begin
    Result := TRUE;
end;

function TDialogFields.GetItemAt (anIndex: Integer) : TDialogField;
begin
    Result := NIL;
    try
        Result := TDialogField (inherited Item [anIndex]);
    except on E: Exception do
        _raise ([ Format ('GetItemAt [%d]',[anIndex]), ERR_TDIALOGFIELDS_GET_ITEM_AT, E ],
                ['{8ABF6994-409D-4B14-9EF0-7FFF7E6F3F67}']);
    end;
end;

procedure TDialogFields.SetItemAt (anIndex: Integer; anItem: TDialogField);
begin
    try
        inherited Item [anIndex] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemAt [%d]',[anIndex]), ERR_TDIALOGFIELDS_SET_ITEM_AT, E ],
                ['{3A63C321-081B-40CC-A6E6-D7F9C4545D9E}']);
    end;
end;

function TDialogFields.GetItemOf (aName: String) : TDialogField;
var
    I     : Integer;
    Found : Boolean;
begin
    Result := NIL;
    try
        Found := FALSE;
        for I := 0 to Count - 1 do
            if (  UpperCase ( TDialogField (Item [I]).Name ) = UpperCase (aName)  ) then
            begin
                Result := TDialogField (Item [I]);
                Found := TRUE;
                Break;
            end;
        if not Found then
            raise Exception.CreateFmt (ERR_TDIALOGFIELDS_NOT_FOUND,[aName]);
    except on E: Exception do
        _raise ([ Format ('GetItemOf [%s]',[aName]), ERR_TDIALOGFIELDS_GET_ITEM_OF, E ],
                ['{06FD1E89-89E7-4E33-B8D4-7C00EB3B59EB}']);
    end;
end;

procedure TDialogFields.SetItemOf (aName: String; anItem: TDialogField);
var
    I     : Integer;
    Found : Boolean;
begin
    try
        Found := FALSE;
        for I := 0 to Count - 1 do
            if (  UpperCase ( TDialogField (Item [I]).Name ) = UpperCase (aName)  ) then
            begin
                Item [I] := anItem;
                Found := TRUE;
                Break;
            end;
        if not Found then
            raise Exception.CreateFmt (ERR_TDIALOGFIELDS_NOT_FOUND,[aName]);
    except on E: Exception do
        _raise ([ Format ('SetItemOf [%s]',[aName]), ERR_TDIALOGFIELDS_SET_ITEM_OF, E ],
                ['{694E6FBA-C747-4EAB-B422-287D01C5C387}']);
    end;
end;

class function TDialogFields.ItemClassType : CProtoProperty;
begin
    Result := TDialogField;
end;

function TDialogFields.Add (anItem: TDialogField) : Integer;
begin
    Result := -1;
    try
        Result := inherited Add (anItem);
    except on E: Exception do
        _raise (['Add',ERR_TDIALOGFIELDS_ADD,E],
                ['{88CA5532-29FB-46F7-A106-084D26A802E4}']);
    end;
end;
