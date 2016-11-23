{******************************************************************************}
{*  TProtoObject.imp.inc                                                      *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TProtoObject}
class procedure TProtoObject._raise (anArgs: array of const;
                                     const anEGUID: String = '');
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

class procedure TProtoObject._raise (anArgs: array of const;
                                     anEGUID: array of const);
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

class function TProtoObject.GetClassID : ShortString;
var
    S : ShortString;
begin
    S := LowerCase (ClassName);
    // drop T-char
    S := Copy ( S, 2, Length (S) );
    Result := S;
end;

class function TProtoObject.GetClassName : String;
begin
    Result := CLS_TPROTOOBJECT_NAME;
end;

class function TProtoObject.GetClassVersion : TVersionInfo;
begin
    Result := NULL_VERSION;
end;

class function TProtoObject.GetClassProperties : TProtoProperties;
begin
    Result := NIL;
    try
        Result := TProtoProperties.Create ();
        with Result do
        begin
            Add ( TProtoProperty.Create (['object_name',PRP_TPROTOOBJECT_OBJECT_NAME,dtString,'']) );
            Add ( TProtoProperty.Create (['class_name',PRP_TPROTOOBJECT_CLASS_NAME,dtString,GetClassName]) );
            Add ( TProtoProperty.Create (['class_version',PRP_TPROTOOBJECT_CLASS_VERSION,dtVersion,dtVersion,'0.0.0.0']) );
            ItemAt [_class_version].asVersion := GetClassVersion;
        end;
    except on E: Exception do
        _raise (['GetClassProperties',ERR_TPROTOOBJECT_GET_ClASS_PROPERTIES,E],
                ['{0F01EBCA-4EE8-4929-A07B-EA0D3EC203B9}']);
    end;
end;

class function TProtoObject.GetClassList : CProtoObjects;
begin
    Result := TProtoObjects;
end;

function TProtoObject.RegisterProperty (anArgs: array of const;
                                        aMode: TPropertyMode = []) : Integer;
begin
    Result := -1;
    try
        Result := Properties.Add ( TProtoProperty.Create (anArgs,aMode) );
    except on E: Exception do
        _raise (['RegisterProperty',ERR_TPROTOOBJECT_REGISTER_PROPERTY,E],
                ['{E31F8EB6-F815-4E7D-A08F-C39CB2F0CC21}']);
    end;
end;

procedure TProtoObject.RegisterProperties;
var
    Props : TProtoProperties;
    I     : Integer;
begin
    try
        Props := GetClassProperties;
        try
            for I := 0 to Props.Count - 1 do
                RegisterProperty  ([ Props.ItemAt [I].Name,
                                     Props.ItemAt [I].Caption,
                                     Props.ItemAt [I].DataType,
                                     Props.ItemAt [I].Value ],
                                   Props.ItemAt [I].Mode );
        finally
            Props.FreeAll;
            FreeAndNil (Props);
        end;
    except on E: Exception do
        _raise (['RegisterProperties',ERR_TPROTOOBJECT_REGISTER_PROPERTIES,E],
                ['{3D4AF2C7-AF9B-4379-88CB-69A78C08C29D}']);
    end;
end;

constructor TProtoObject.Create (anArgs: array of const;
                                 aMode: TObjectMode = []);
begin
    try
        inherited Create;
        Mode := Mode - [objCreated];
        f_ClassID := GetClassID;
        f_Errors := TStringList.Create;
        f_Mode := aMode;
        f_Properties := TProtoProperties.Create ([]);
        { регистрация свойств }
        RegisterProperties;
        { первый параметр - имя объекта }
        if notEmpty (0,anArgs) then
            ObjectName := toString (anArgs [0]);
        Mode := Mode + [objCreated];
    except on E: Exception do begin
        Mode := Mode + [objCreateFailed];
        _raise (['Create',ERR_TPROTOOBJECT_CREATE,E],
                ['{EAED2373-C7D7-4B75-80F1-1214ED6BE2E5}']);
    end; end;
end;

destructor TProtoObject.Destroy;
begin
    try
        if Assigned (f_Properties) then
        begin
            f_Properties.FreeAll;
            f_Properties.Clear;
        end;
        FreeAndNil (f_Properties);
        FreeAndNil (f_Errors);
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TPROTOOBJECT_DESTROY,E],
                ['{E41DB2F6-94F0-4356-9719-60C3775F2FB7}']);
    end;
end;

function TProtoObject.Check : Boolean;
begin
    Result := TRUE;
    try
        Errors.Clear;
        Result := ( Errors.Count = 0 );
    except on E: Exception do
        _raise (['Check',ERR_TPROTOOBJECT_CHECK,E],
                ['{249E47B0-B0B4-40FC-AE9F-BE47003261C9}']);
    end;
end;

procedure TProtoObject.Error (const aMessage: String);
begin
    try
        Errors.Add (aMessage);
    except on E: Exception do
        _raise (['Error',ERR_TPROTOOBJECT_ERROR,E],
                ['{BDB8F3A9-9990-4A4F-B464-FBFEB5C6EC33}']);
    end;
end;

procedure TProtoObject.ErrorFmt (const aMessage: String; anArgs: array of const);
begin
    try
        Error ( Format (aMessage,anArgs) );
    except on E: Exception do
        _raise (['Error',ERR_TPROTOOBJECT_ERROR,E],
                ['{E5855296-3E21-417B-A037-FEEDA80B31C8}']);
    end;
end;

function TProtoObject.GetPropAt (anIndex: Integer) : TProtoProperty;
begin
    Result := NIL;
    try
        Result := Properties.ItemAt [anIndex];
    except on E: Exception do
        _raise ([ Format ('GetPropAt [%d]',[anIndex]), ERR_TPROTOOBJECT_GET_PROPERTY_AT, E ],
                ['{95334BE5-5D80-413A-B9B9-BDC2A5A22508}']);
    end;
end;

procedure TProtoObject.SetPropAt (anIndex: Integer; anItem: TProtoProperty);
begin
    try
        Properties.ItemAt [anIndex] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetPropAt [%d]',[anIndex]), ERR_TPROTOOBJECT_SET_PROPERTY_AT, E ],
                ['{FE6ADB3A-5B23-4B35-8352-A6A819106B8F}']);
    end;
end;

function TProtoObject.GetPropOf (aName: String) : TProtoProperty;
begin
    Result := NIL;
    try
        Result := Properties.ItemOf [aName];
    except on E: Exception do
        _raise ([ Format ('GetPropOf [%s]',[aName]), ERR_TPROTOOBJECT_GET_PROPERTY_OF, E ],
                ['{48CBD331-8380-42FE-8F4C-5E45B1530C26}']);
    end;
end;

procedure TProtoObject.SetPropOf (aName: String; anItem: TProtoProperty);
begin
    try
        Properties.ItemOf [aName] := anItem;
    except on E: Exception do
        _raise ([ Format ('SetPropOf [%s]',[aName]), ERR_TPROTOOBJECT_SET_PROPERTY_OF, E ],
                ['{FD1D901B-8C0B-4B2E-B5A1-B5FC0DDD8292}']);
    end;
end;

procedure TProtoObject.AssignTo (Dest: TPersistent);
begin
    try
        if Assigned (Dest) and Dest.InheritsFrom (TProtoObject) then
        with TProtoObject (Dest) do
        begin
            f_Properties.AssignTo (self.f_Properties);
            self.f_Errors.Assign (f_Errors);
            f_Mode := self.f_Mode;
        end
        else inherited AssignTo (Dest);
    except on E: Exception do
        _raise (['Assign',ERR_TPROTOOBJECT_ASSIGN,E],
                ['{D0BAE6F0-BCEB-485D-8C7D-8C26F3EBDAB6}']);
    end;
end;

function TProtoObject.GetObjectName : String;
begin
    try
        Result := PropAt [_object_name].asString;
    except on E: Exception do
        _raise (['GetObjectName',ERR_TPROTOOBJECT_GET_OBJECT_NAME,E],
                ['{02F39DE9-D556-463E-84F4-D84E1E03495D}']);
    end;
end;

procedure TProtoObject.SetObjectName (const aValue: String);
begin
    try
        PropAt [_object_name].asString := aValue;
    except on E: Exception do
        _raise (['SetObjectName',ERR_TPROTOOBJECT_SET_OBJECT_NAME,E],
                ['{A5BDBBB3-4DB7-4111-9456-4C9EBFFC22BD}']);
    end;
end;
