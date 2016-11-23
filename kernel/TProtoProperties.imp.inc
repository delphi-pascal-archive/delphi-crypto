{******************************************************************************}
{*  TProtoProperties.imp.inc                                                  *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TProtoProperties }
constructor TProtoProperties.Create (anArgs: array of const);
var
    I : Integer;
begin
    try
        inherited Create;
        { параметры - список параметров-конструкторов свойств }
        for I := Low (anArgs) to High (anArgs) do
            if notEmpty (I,anArgs) then
                Add ( toArrayOfConst (anArgs [I]) );
    except on E: Exception do
        _raise (['Create',ERR_TPROTOPROPERTIES_CREATE,E],
                ['{7534D664-E1C4-4DFE-BF36-8B38AEBE163E}']);
    end;
end;

destructor TProtoProperties.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TPROTOPROPERTIES_DESTROY,E],
                ['{98C5FF60-A994-49C8-9A30-ABE6DA029945}']);
    end;
end;

function TProtoProperties.Check (anItem: Pointer) : Boolean;
begin
    Result := FALSE;
    try
        Result := inherited Check (anItem) and
                  TObject (anItem).InheritsFrom (TProtoProperty) and
                  Check ( TProtoProperty (anItem) );
    except on E: Exception do
        _raise (['Check',ERR_TPROTOPROPERTIES_CHECK,E],
                ['{413B8202-1E4D-4B9D-9334-EFDAC4852BF3}']);
    end;
end;

function TProtoProperties.Check (anItem: TProtoProperty) : Boolean;
begin
    Result := TRUE;
end;

function TProtoProperties.GetItemAt (anIndex: Integer) : TProtoProperty;
begin
    Result := NIL;
    try
        Result := TProtoProperty (inherited Item [anIndex]);
    except on E: Exception do
        _raise ([ Format ('GetItemAt [%d]',[anIndex]), ERR_TPROTOPROPERTIES_GET_ITEM_AT, E ],
                ['{03DA942B-2819-41CB-93A5-0D0AE20D694C}']);
    end;
end;

procedure TProtoProperties.SetItemAt (anIndex: Integer; anItem: TProtoProperty);
begin
    try
        inherited Item [anIndex] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetItemAt [%d]',[anIndex]), ERR_TPROTOPROPERTIES_SET_ITEM_AT, E ],
                ['{E7DD7EEB-5018-452C-BDB5-3CAA66CFA105}']);
    end;
end;

function TProtoProperties.GetItemOf (aName: String) : TProtoProperty;
var
    I     : Integer;
    Found : Boolean;
begin
    Result := NIL;
    try
        Found := FALSE;
        for I := 0 to Count - 1 do
            if (  UpperCase ( TProtoProperty (Item [I]).Name ) = UpperCase (aName)  ) then
            begin
                Result := TProtoProperty (Item [I]);
                Found := TRUE;
                Break;
            end;
        if not Found then
            raise Exception.CreateFmt (ERR_TPROTOPROPERTIES_NOT_FOUND,[aName]);
    except on E: Exception do
        _raise ([ Format ('GetItemOf [%s]',[aName]), ERR_TPROTOPROPERTIES_GET_ITEM_OF, E ],
                ['{D48C413F-81C9-49C7-8F82-FF70EEEE106D}']);
    end;
end;

procedure TProtoProperties.SetItemOf (aName: String; anItem: TProtoProperty);
var
    I     : Integer;
    Found : Boolean;
begin
    try
        Found := FALSE;
        for I := 0 to Count - 1 do
            if (  UpperCase ( TProtoProperty (Item [I]).Name ) = UpperCase (aName)  ) then
            begin
                Item [I] := anItem;
                Found := TRUE;
                Break;
            end;
        if not Found then
            raise Exception.CreateFmt (ERR_TPROTOPROPERTIES_NOT_FOUND,[aName]);
    except on E: Exception do
        _raise ([ Format ('SetItemOf [%s]',[aName]), ERR_TPROTOPROPERTIES_SET_ITEM_OF, E ],
                ['{0C004EAF-0D35-4323-8409-EAE321243474}']);
    end;
end;

class function TProtoProperties.ItemClassType : CProtoProperty;
begin
    Result := TProtoProperty;
end;

function TProtoProperties.Add (anItem: TProtoProperty) : Integer;
begin
    Result := -1;
    try
        Result := inherited Add (anItem);
    except on E: Exception do
        _raise (['Add',ERR_TPROTOPROPERTIES_ADD,E],
                ['{64D2A790-7906-454B-A6DE-D573B99EBF1C}']);
    end;
end;

function TProtoProperties.Add (anArgs: array of const;
                               aMode: TPropertyMode = []) : Integer;
var
    OBJ : TProtoProperty;
begin
    Result := -1;
    try
        OBJ := ItemClassType.Create (anArgs,aMode);
        try
            Result := Add (OBJ);
        finally
            if ( Result < 0 ) then
                FreeAndNil (OBJ);
        end;
    except on E: Exception do
        _raise (['Add',ERR_TPROTOPROPERTIES_ADD,E],
                ['{74CDE029-8AA9-4960-AB92-D3235E2DEACA}']);
    end;
end;

function TProtoProperties.Register (const anExtraClassType: CProtoProperty;
                                    anArgs: array of const;
                                    aMode: TPropertyMode = []) : Integer;
const
    MaxTryCount = 3;
var
    OBJ : TProtoProperty;
    TryCount : Integer;
begin
    Result := -1;
    try
        if not Assigned (anExtraClassType) then
            raise Exception.Create (ERR_TPROTOPROPERTIES_INVALID_CLASS_TYPE);
        TryCount := 0;
        repeat try
            Inc (TryCount);
            OBJ := anExtraClassType.Create (anArgs,aMode);
            try
                Result := Add (OBJ);
            finally
                if ( Result < 0 ) then
                    FreeAndNil (OBJ);
            end;
        except
            if ( TryCount >= MaxTryCount ) then raise;
        end until ( Result >= 0 );
        if not ( Result >= 0 ) then
            raise Exception.Create (ERR_TPROTOPROPERTIES_REGISTER);
    except on E: Exception do
        _raise (['Register',ERR_TPROTOPROPERTIES_REGISTER,E],
                ['{7CA018FE-953F-4481-8EFF-6FB9C996FA1D}']);
    end;
end;

procedure TProtoProperties.AssignTo (Dest: TPersistent);
var
    I    : Integer;
    Prop : TProtoProperty;
begin
    try
        if Assigned (Dest) and Dest.InheritsFrom (TProtoProperties) then
        begin
            TProtoProperties (Dest).Clear;
            for I := 0 to Count - 1 do
            begin
                Prop := CProtoProperty (ItemAt [I].ClassType).Create ([ ItemAt [I].Name,
                                                                        ItemAt [I].Caption,
                                                                        ItemAt [I].DataType,
                                                                        ItemAt [I].Value ]);
                Prop.Assign (ItemAt [I]);
                if not ( TProtoProperties (Dest).Add (Prop) >= 0 ) then
                    FreeAndNil (Prop);
            end;
        end
        else inherited AssignTo (Dest);
    except on E: Exception do
        _raise (['Assign',ERR_TPROTOOBJECT_ASSIGN,E],
                ['{D0BAE6F0-BCEB-485D-8C7D-8C26F3EBDAB6}']);
    end;
end;
