{******************************************************************************}
{*  TDialogControl.imp.inc                                                    *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
class procedure TDialogControl._raise (anArgs: array of const;
                                       const anEGUID: String = '');
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

class procedure TDialogControl._raise (anArgs: array of const;
                                       anEGUID: array of const);
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

class function TDialogControl.GetControlClassType (const aType: TDataType) : CControl;
begin
    Result := NIL;
    try
        case aType of
            dtBoolean   : Result := TsCheckBox;
            dtInteger   : Result := TsSpinEdit;
            dtExtended  : Result := TsDecimalSpinEdit;
            dtDouble    : Result := TsCurrencyEdit;
            dtVersion,
            dtString    : Result := TsEdit;
            dtPassword  : Result := TsMaskEdit;
            dtHex,
            dtCrypto,
            dtSafe,
            dtText      : Result := TsRichEdit;
            dtDate      : Result := TsDateEdit;
            dtTime      : Result := TsTimePicker;
            dtFile      : Result := TsFileNameEdit;
            dtDirectory : Result := TsDirectoryEdit;
            else          raise Exception.Create (ERR_TDIALOGFIELD_UNKNOWN_TYPE);
        end;
    except on E: Exception do
        _raise (['GetControlClassType',ERR_TDIALOGCONTROL_GET_CONTROL_CLASS_TYPE,E],
                ['{18505063-86B8-414A-BBEA-862CFED73D22}']);
    end;
end;

function TDialogControl.GetControlClassType : CControl;
begin
    Result := NIL;
    try
        Result := GetControlClassType (Field.DataType);
    except on E: Exception do
        _raise (['GetControlClassType',ERR_TDIALOGCONTROL_GET_CONTROL_CLASS_TYPE,E],
                ['{0B3A599F-6049-479E-BC70-AB0E8A34C1DB}']);
    end;
end;

constructor TDialogControl.Create (anOwnerControl: TComponent;
                                   anOwnerLabel: TComponent;
                                   aField: TDialogField;
                                   aPopupMenu: TPopupMenu = NIL);
begin
    try
        inherited Create;
        f_OwnerControl := anOwnerControl;
        f_OwnerLabel := anOwnerLabel;
        f_Field := aField;
        { control }
        f_Control := GetControlClassType.Create (anOwnerControl);
        Control.Parent := TWinControl (anOwnerControl);
        case Field.DataType of
            dtBoolean:   with TsCheckBox (Control) do
                             Caption := ' ';
            dtPassword:  with TsMaskEdit (Control) do
                         begin
                             PasswordChar := '#';
                             ParentColor := TRUE;
                             ParentFont := TRUE;
                             Font.Name := 'Arial Black';
                             AutoSize := FALSE;
                             Height := 24;
                         end;
            dtHex,
            dtCrypto,
            dtSafe,
            dtText:      with TsRichEdit (Control) do
                             ScrollBars := ssVertical;
            dtDate,
            dtFile,
            dtDirectory: with TsMaskEdit (Control) do
                         begin
                             ParentColor := TRUE;
                             ParentFont := TRUE;
                             AutoSize := FALSE;
                             Height := 24;
                         end;
        end;
        Control.Left := 8;
        Control.Width := Control.Parent.Width - 16;
        if ( anOwnerControl.ComponentCount - 1 > 0 ) then
            Control.Top := TWinControl (anOwnerControl.Components [anOwnerControl.ComponentCount - 2]).Top +
                           TWinControl (anOwnerControl.Components [anOwnerControl.ComponentCount - 2]).Height + 8
        else
            Control.Top := 8;
        { label }
        f_Label := TsLabel.Create (anOwnerLabel);
        LabelControl.Parent := TWinControl (anOwnerLabel);
        with LabelControl do
        begin
            Caption := Field.Caption;
            AlignMent := taRightJustify;
            AutoSize := TRUE;
        end;
        LabelControl.Left := 16;
        LabelControl.Top := Control.Top + 2;
        { etc. }
        SetName (Field.Name);
        SetCaption (Field.Caption);
        SetValue (Field.Value);
        SetReadOnly (Field.ReadOnly);
        if Assigned (aPopupMenu) then
            SetPopupMenu (aPopupMenu);
    except on E: Exception do
        _raise (['Create',ERR_TDIALOGCONTROL_CREATE,E],
                ['{FEFB14E7-E4AD-4640-B280-22577F6FBA1C}']);
    end;
end;

constructor TDialogControl.Create (anArgs: array of const);
begin
    try
        { первый, второй параметр - владельцы }
        { третий параметр - поле }
        if     notEmpty (0,anArgs) and ( toObject (anArgs [0]) is TWinControl )
           and notEmpty (1,anArgs) and ( toObject (anArgs [1]) is TWinControl )
           and notEmpty (2,anArgs) and ( toObject (anArgs [2]) is TDialogField ) then
            Create (  TWinControl ( toObject (anArgs [0]) ),
                      TWinControl ( toObject (anArgs [1]) ),
                      TDialogField ( toObject (anArgs [2]) )  );
        { четвертый параметр - контекстное меню }
        if (  notEmpty (3,anArgs) and ( toObject (anArgs [3]) is TPopupMenu )  ) then
            SetPopupMenu (  TPopupMenu ( toObject (anArgs [3]) )  );
    except on E: Exception do
        _raise (['Create',ERR_TDIALOGCONTROL_CREATE,E],
                ['{96CC1CDA-6DE5-4CE6-8D54-EC0E5C10521A}']);
    end;
end;

destructor TDialogControl.Destroy;
begin
    try
        FreeAndNil (f_Control);
        FreeAndNil (f_Label);
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TDIALOGCONTROL_DESTROY,E],
                ['{2EAA2223-1B55-4CF9-95B7-7958FFA9EF40}']);
    end;
end;

function TDialogControl.GetName : String;
begin
    Result := '';
    try
        Result := Control.Name;
    except on E: Exception do
        _raise (['GetName',ERR_TDIALOGCONTROL_GET_NAME,E],
                ['{A60F069E-9300-4597-98EF-287A65925A0D}']);
    end;
end;

procedure TDialogControl.SetName (const aValue: String);
begin
    try
        Control.Name := aValue;
        LabelControl.Name := Format ('lb%s',[aValue]);
    except on E: Exception do
        _raise (['SetName',ERR_TDIALOGCONTROL_SET_NAME,E],
                ['{C0612271-C1B3-4698-84C8-B817D945BA3A}']);
    end;
end;

function TDialogControl.GetCaption : String;
begin
    Result := '';
    try
        Result := LabelControl.Caption;
    except on E: Exception do
        _raise (['GetCaption',ERR_TDIALOGCONTROL_GET_CAPTION,E],
                ['{39B20E00-A29C-4D01-838B-2CF8D90B4BC9}']);
    end;
end;

procedure TDialogControl.SetCaption (const aValue: String);
begin
    try
        LabelControl.Caption := aValue;
    except on E: Exception do
        _raise (['SetCaption',ERR_TDIALOGCONTROL_SET_CAPTION,E],
                ['{CC98045B-CFD1-4AB4-B3C7-197E2DB813B3}']);
    end;
end;

function TDialogControl.GetValue : Variant;
begin
    Result := NULL;
    try
        case Field.DataType of
            dtBoolean   : Result := TsCheckBox (Control).Checked;
            dtInteger   : Result := TsSpinEdit (Control).Value;
            dtExtended  : Result := TsDecimalSpinEdit (Control).Value;
            dtDouble    : Result := TsCurrencyEdit (Control).Value;
            dtVersion,
            dtString    : Result := TsEdit (Control).Text;
            dtPassword  : Result := TsMaskEdit (Control).Text;
            dtHex,
            dtCrypto,
            dtSafe,
            dtText      : Result := TsRichEdit (Control).Text;
            dtDate      : Result := TsDateEdit (Control).Date;
            dtTime      : Result := TsTimePicker (Control).Time;
            dtFile      : Result := TsFileNameEdit (Control).Text;
            dtDirectory : Result := TsDirectoryEdit (Control).Text;
        end;
    except on E: Exception do
        _raise (['GetValue',ERR_TDIALOGCONTROL_GET_VALUE,E],
                ['{9505DFB0-E0A0-4889-A91D-C182E7DA4175}']);
    end;
end;

procedure TDialogControl.SetValue (const aValue: Variant);
begin
    try
        if not ReadOnly then
            case Field.DataType of
                dtBoolean   : TsCheckBox (Control).Checked := aValue;
                dtInteger   : TsSpinEdit (Control).Value := aValue;
                dtExtended  : TsDecimalSpinEdit (Control).Value := aValue;
                dtDouble    : TsCurrencyEdit (Control).Value := aValue;
                dtVersion,
                dtString    : TsEdit (Control).Text := aValue;
                dtPassword  : TsMaskEdit (Control).Text := aValue;
                dtHex,
                dtCrypto,
                dtSafe,
                dtText      : TsRichEdit (Control).Text := aValue;
                dtDate      : TsDateEdit (Control).Date := aValue;
                dtTime      : TsTimePicker (Control).Time := aValue;
                dtFile      : TsFileNameEdit (Control).Text := aValue;
                dtDirectory : TsDirectoryEdit (Control).Text := aValue;
            end;
    except on E: Exception do
        _raise (['SetValue',ERR_TDIALOGCONTROL_SET_VALUE,E],
                ['{BB97C52E-6673-4C23-A5EF-7D2106AD08B6}']);
    end;
end;

function TDialogControl.GetReadOnly : Boolean;
begin
    Result := FALSE;
    try
        Result := not Control.Enabled;
        case Field.DataType of
            dtBoolean   : Result := Result or TsCheckBox (Control).ReadOnly;
            dtInteger   : Result := Result or TsSpinEdit (Control).ReadOnly;
            dtExtended  : Result := Result or TsDecimalSpinEdit (Control).ReadOnly;
            dtDouble    : Result := Result or TsCurrencyEdit (Control).ReadOnly;
            dtVersion,
            dtString    : Result := Result or TsEdit (Control).ReadOnly;
            dtPassword  : Result := Result or TsMaskEdit (Control).ReadOnly;
            dtHex,
            dtCrypto,
            dtSafe,
            dtText      : Result := Result or TsRichEdit (Control).ReadOnly;
            dtDate      : Result := Result or TsDateEdit (Control).ReadOnly;
            dtTime      : Result := Result or TsTimePicker (Control).ReadOnly;
            dtFile      : Result := Result or TsFileNameEdit (Control).ReadOnly;
            dtDirectory : Result := Result or TsDirectoryEdit (Control).ReadOnly;
        end;
    except on E: Exception do
        _raise (['GetReadOnly',ERR_TDIALOGCONTROL_GET_READ_ONLY,E],
                ['{759CFB2F-233D-477C-B8C3-86D997DD8E73}']);
    end;
end;

procedure TDialogControl.SetReadOnly (const aValue: Boolean);
begin
    try
        //Control.Enabled := not aValue;
        case Field.DataType of
            dtBoolean  : TsCheckBox (Control).ReadOnly := aValue;
            dtInteger  : TsSpinEdit (Control).ReadOnly := aValue;
            dtExtended : TsDecimalSpinEdit (Control).ReadOnly := aValue;
            dtDouble   : TsCurrencyEdit (Control).ReadOnly := aValue;
            dtVersion,
            dtString   : TsEdit (Control).ReadOnly := aValue;
            dtPassword : TsMaskEdit (Control).ReadOnly := aValue;
            dtHex,
            dtCrypto,
            dtSafe,
            dtText      : TsRichEdit (Control).ReadOnly := aValue;
            dtDate      : TsDateEdit (Control).ReadOnly := aValue;
            dtTime      : TsTimePicker (Control).ReadOnly := aValue;
            dtFile      : TsFileNameEdit (Control).ReadOnly := aValue;
            dtDirectory : TsDirectoryEdit (Control).ReadOnly := aValue;
        end;
    except on E: Exception do
        _raise (['SetReadOnly',ERR_TDIALOGCONTROL_SET_READ_ONLY,E],
                ['{2D280EB0-CE71-4421-A3E9-E48AB4F59F00}']);
    end;
end;

function TDialogControl.GetVisible : Boolean;
begin
    Result := FALSE;
    try
        Result := Control.Visible;
    except on E: Exception do
        _raise (['GetVisible',ERR_TDIALOGCONTROL_GET_VISIBLE,E],
                ['{7CA8E0C9-3EA9-42E9-B207-6931F4BFA7A1}']);
    end;
end;

procedure TDialogControl.SetVisible (const aValue: Boolean);
begin
    try
        Control.Visible := aValue;
    except on E: Exception do
        _raise (['SetVisible',ERR_TDIALOGCONTROL_SET_VISIBLE,E],
                ['{2B41F96E-A90A-4477-B756-A262777CC003}']);
    end;
end;

procedure TDialogControl.SetPopupMenu (const aValue: TPopupMenu);
begin
    try
        case Field.DataType of
           {dtBoolean   : with TsCheckBox (Control) do
                              PopupMenu := aValue;}
            dtInteger   : with TsSpinEdit (Control) do
                              PopupMenu := aValue;
            dtExtended  : with TsDecimalSpinEdit (Control) do
                              PopupMenu := aValue;
            dtDouble    : with TsCurrencyEdit (Control) do
                              PopupMenu := aValue;
            dtVersion,
            dtString    : with TsEdit (Control) do
                              PopupMenu := aValue;
            dtPassword  : with TsMaskEdit (Control) do
                              PopupMenu := aValue;
            dtHex,
            dtCrypto,
            dtSafe,
            dtText      : with TsRichEdit (Control) do
                              PopupMenu := aValue;
            dtDate      : with TsMaskEdit (Control) do
                              PopupMenu := aValue;
            dtTime      : with TsTimePicker (Control) do
                              PopupMenu := aValue;
            dtFile      : with TsMaskEdit (Control) do
                              PopupMenu := aValue;
            dtDirectory : with TsMaskEdit (Control) do
                              PopupMenu := aValue;
        end;
    except on E: Exception do
        _raise (['SetPopupMenu',ERR_TDIALOGCONTROL_SET_POPUP_MENU,E],
                ['{D62DC1D4-B718-4360-A1AF-A659C6E0A71B}']);
    end;
end;

