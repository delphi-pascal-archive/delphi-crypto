unit sPropEditors;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, sConst, ExtCtrls, sPanel, sGraphUtils, acntUtils, ImgList,
  Consts, ComStrs, CommCtrl{$IFNDEF ALITE}, sDialogs, sPageControl{$ENDIF} , TypInfo,
  {$IFDEF DELPHI6UP}DesignEditors, DesignIntf, VCLEditors,{$ELSE}dsgnintf,{$ENDIF}
  sVclUtils, ColnEdit;

type
{$IFNDEF ALITE}
  TsHintProperty = class(TCaptionProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetEditLimit: Integer; override;
    procedure Edit; override;
  end;

  TsPageControlEditor = class(TDefaultEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TsFrameBarEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TsTabSheetEditor = class(TDefaultEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TacHintsTemplatesProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TacAlphaHintsEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TsPathDlgEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TacTemplateNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{$ENDIF}

  TacSkinInfoProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TsImageListEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TacImageIndexEditor = class(TIntegerProperty{$IFDEF DELPHI7UP}, ICustomPropertyListDrawing{$ENDIF})
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetMyList:TCustomImageList;
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
  end;

  TacImgListItemsProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TsSkinSectionProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TsSkinNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TsDirProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TsInternalSkinsProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TacThirdPartyProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TacSkinManagerEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

uses sDefaults, sSkinManager, FileCtrl, sMaskData, sSkinProps,
{$IFNDEF ALITE}
  sToolEdit, sComboBoxes, sBitBtn,
  sLabel, sStrEdit, acShellCtrls, acRootEdit, acPathDialog, sFrameBar,
  sMemo, acAlphaHintsEdit, acNotebook, acAlphaHints,
{$ENDIF}
  FiltEdit, sInternalSkins, stdreg, sSpeedButton, sStyleSimply, acAlphaImageList, sImgListEditor
  {$IFNDEF BCB}, Notebreg{$ENDIF}, ac3rdPartyEditor, acSkinInfo;

{$IFNDEF ALITE}

{ TsPageControlEditor }

procedure TsPageControlEditor.ExecuteVerb(Index: Integer);
var
  NewPage: TsTabSheet;
begin
  case Index of
    0: begin
      NewPage := TsTabSheet.Create(Designer.GetRoot);
      NewPage.Parent := (Component as TsPageControl);
      NewPage.PageControl := (Component as TsPageControl);
      NewPage.Caption := Designer.UniqueName('sTabSheet');
      NewPage.Name := NewPage.Caption;
    end;
    1: begin
      NewPage := TsTabSheet((Component as TsPageControl).ActivePage);
      NewPage.Free;
    end;
    2: begin
      (Component as TsPageControl).SelectNextPage(True);
    end;
    3: begin
      (Component as TsPageControl).SelectNextPage(False);
    end;
  end;
  if Designer <> nil then Designer.Modified;
end;

function TsPageControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:  result := 'New Page';
    1:  result := 'Delete Page';
    2:  result := 'Next Page';
    3:  result := 'Previous Page';
  end;
end;

function TsPageControlEditor.GetVerbCount: Integer;
begin
  result := 4;
end;
{$ENDIF}

procedure Register;
begin
{$IFNDEF ALITE}
  RegisterComponentEditor(TsPageControl, TsPageControlEditor);
  RegisterComponentEditor(TsTabSheet, TsTabSheetEditor);
  RegisterComponentEditor(TsFrameBar, TsFrameBarEditor);
  RegisterComponentEditor(TsAlphaHints, TacAlphaHintsEditor);
{$IFNDEF BCB}
  RegisterPropertyEditor(TypeInfo(TStrings), TsNotebook, 'Pages', TPageListProperty);
{$ENDIF}
  RegisterPropertyEditor(TypeInfo(TsSkinSection), TObject, 'TabSkin', TsSkinSectionProperty);
  RegisterPropertyEditor(TypeInfo(TsSkinSection), TObject, 'HeaderSkin', TsSkinSectionProperty);
{$ENDIF}
  RegisterComponentEditor(TsAlphaImageList, TsImageListEditor);
  RegisterPropertyEditor(TypeInfo(TsSkinSection), TObject, 'MenuLineSkin', TsSkinSectionProperty);
  RegisterPropertyEditor(TypeInfo(TsSkinSection), TObject, 'IcoLineSkin', TsSkinSectionProperty);
  RegisterPropertyEditor(TypeInfo(TsSkinSection), TObject, 'SkinSection', TsSkinSectionProperty);
  RegisterPropertyEditor(TypeInfo(TsSkinSection), TObject, 'TitleSkin', TsSkinSectionProperty);
  RegisterPropertyEditor(TypeInfo(TsSkinSection), TObject, 'ButtonSkin', TsSkinSectionProperty);
  RegisterPropertyEditor(TypeInfo(TsSkinSection), TObject, 'CaptionSkin', TsSkinSectionProperty);
  RegisterPropertyEditor(TypeInfo(TsSkinSection), TObject, 'ProgressSkin', TsSkinSectionProperty);
  RegisterPropertyEditor(TypeInfo(TsSkinName), TsSkinManager, 'SkinName', TsSkinNameProperty);
  RegisterPropertyEditor(TypeInfo(TsDirectory), TsSkinManager, 'SkinDirectory', TsDirProperty);
  RegisterPropertyEditor(TypeInfo(TsStoredSkins), TsSkinManager, 'InternalSkins', TsInternalSkinsProperty);
  RegisterPropertyEditor(TypeInfo(ThirdPartyList), TsSkinManager, 'ThirdParty', TacThirdPartyProperty);
  RegisterPropertyEditor(TypeInfo(TsImgListItems), TsAlphaImageList, 'Items', TacImgListItemsProperty);
  RegisterComponentEditor(TsSkinManager, TacSkinManagerEditor);
  RegisterPropertyEditor(TypeInfo(TacSkinInfo), TsSkinManager, 'SkinInfo', TacSkinInfoProperty);

{$IFNDEF ALITE}
  RegisterPropertyEditor(TypeInfo(Integer),TsSpeedButton,'ImageIndex',TAcImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(Integer),TsBitBtn,'ImageIndex',TAcImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(TacStrValue), TsAlphaHints, 'TemplateName', TacTemplateNameProperty);
  RegisterPropertyEditor(TypeInfo(TacHintTemplates), TsAlphaHints, 'Templates', TacHintsTemplatesProperty);

{$IFNDEF TNTUNICODE}
  RegisterPropertyEditor(TypeInfo(TCaption), TObject, 'Caption', TsHintProperty);
  RegisterPropertyEditor(TypeInfo(String), TsMemo, 'Text', TsHintProperty);
{$ENDIF}
  RegisterPropertyEditor(TypeInfo(string), TsFileNameEdit, 'Filter', TFilterProperty);

  // Shell ctrls
  RegisterPropertyEditor(TypeInfo(TacRoot), TsShellListView, 'Root', TacRootProperty);
  RegisterPropertyEditor(TypeInfo(TacRoot), TsShellTreeView, 'Root', TacRootProperty);
  RegisterPropertyEditor(TypeInfo(TacRoot), TsShellComboBox, 'Root', TacRootProperty);
  RegisterPropertyEditor(TypeInfo(TacRoot), TsPathDialog, 'Root', TacRootProperty);
  RegisterPropertyEditor(TypeInfo(TacRoot), TsDirectoryEdit, 'Root', TacRootProperty);
  RegisterComponentEditor(TsPathDialog, TsPathDlgEditor);
{$ENDIF}
end;

{ TsSkinNameProperty }

function TsSkinNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paAutoUpdate];
end;

procedure TsSkinNameProperty.GetValues(Proc: TGetStrProc);
var
  i: integer;
  FileInfo: TSearchRec;
  DosCode: Integer;
  s : string;
begin
  // Internal skins names loading
  if TsSkinManager(GetComponent(0)).InternalSkins.Count > 0 then begin
    for i := 0 to TsSkinManager(GetComponent(0)).InternalSkins.Count - 1 do begin
      Proc(TsSkinManager(GetComponent(0)).InternalSkins[i].Name);
    end;
  end;

  // External skins names loading
  if DirectoryExists(TsSkinManager(GetComponent(0)).GetFullskinDirectory) then begin
    s := TsSkinManager(GetComponent(0)).GetFullskinDirectory + '\*.*';

    DosCode := FindFirst(s, faVolumeID or faDirectory, FileInfo);
    try
      while DosCode = 0 do begin
        if (FileInfo.Name[1] <> '.') then begin
          if {(SkinType in. [stUnpacked, stAllSkins]) and} (FileInfo.Attr and faDirectory = faDirectory) then begin
            Proc(FileInfo.Name);
          end
          else if { (SkinType in [stPacked, stAllSkins]) and} (FileInfo.Attr and faDirectory <> faDirectory) and (ExtractFileExt(FileInfo.Name) = '.' + acSkinExt) then begin
            s := ExtractWord(1, FileInfo.Name, ['.']);
            Proc(s);
          end;
        end;
        DosCode := FindNext(FileInfo);
      end;
    finally
      FindClose(FileInfo);
    end;
  end;
end;

{ TsDirProperty }

function TsDirProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paAutoUpdate];
end;

procedure TsDirProperty.Edit;
var
  s : string;
begin
  s := TsSkinManager(GetComponent(0)).SkinDirectory;

  if not directoryExists(s) then s := '';

{$IFNDEF ALITE}
  if TsSkinManager(GetComponent(0)).SkinData.Active then begin
    PathDialogForm := TPathDialogForm.Create(Application);
    try
      PathDialogForm.sShellTreeView1.Path := s;
      if PathDialogForm.ShowModal = mrOk then begin
        s := PathDialogForm.sShellTreeView1.Path;
        if (s <> '') and DirectoryExists(s) then TsSkinManager(GetComponent(0)).SkinDirectory := s;
      end;
    finally
      FreeAndNil(PathDialogForm);
    end;
  end
  else
{$ENDIF}
    if SelectDirectory('', '', s) then begin
      if (s <> '') and DirectoryExists(s) then TsSkinManager(GetComponent(0)).SkinDirectory := s;
    end;
end;

{ TsInternalSkinsProperty }

procedure TsInternalSkinsProperty.Edit;
var
  i : integer;
begin
  Application.CreateForm(TFormInternalSkins, FormInternalSkins);
  FormInternalSkins.ListBox1.Clear;
  FormInternalSkins.SkinManager := TsSkinManager(GetComponent(0));
  for i := 0 to TsSkinManager(GetComponent(0)).InternalSkins.Count - 1 do begin
    FormInternalSkins.ListBox1.Items.Add(TsSkinManager(GetComponent(0)).InternalSkins.Items[i].Name);
  end;
  if (FormInternalSkins.ShowModal = mrOk) and (Designer <> nil) then Designer.Modified;
  if Assigned(FormInternalSkins) then FreeAndNil(FormInternalSkins);
  inherited;
end;

function TsInternalSkinsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paAutoUpdate, paReadOnly];
end;

{ TacSkinManagerEditor }

procedure TacSkinManagerEditor.ExecuteVerb(Index: Integer);
var
  i : integer;
  sm : TsSkinManager;
begin
  case Index of
    0 : begin
      sm := TsSkinManager(Component);
      Application.CreateForm(TFormInternalSkins, FormInternalSkins);
      FormInternalSkins.ListBox1.Clear;
      FormInternalSkins.SkinManager := sm;
      for i := 0 to sm.InternalSkins.Count - 1 do begin
        FormInternalSkins.ListBox1.Items.Add(sm.InternalSkins.Items[i].Name);
      end;
      FormInternalSkins.ShowModal;
      if Assigned(FormInternalSkins) then FreeAndNil(FormInternalSkins);
      if Designer <> nil then Designer.Modified;
    end;
    1 : begin
      Application.CreateForm(TForm3rdPartyEditor, Form3rdPartyEditor);
      Form3rdPartyEditor.SM := TsSkinManager(Component);
      Form3rdPartyEditor.sListView1.Items.Clear;
      Form3rdPartyEditor.Populate;
      Form3rdPartyEditor.ShowModal;
      if Assigned(Form3rdPartyEditor) then FreeAndNil(Form3rdPartyEditor);
      if Designer <> nil then Designer.Modified;
      inherited;
    end;
  end;
  inherited;
end;

function TacSkinManagerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result := '&Internal skins...';
    1 : Result := '&Third party controls...';
    2 : Result := '-';
  end;
end;

function TacSkinManagerEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{$IFNDEF ALITE}
{ TsTabSheetEditor }

procedure TsTabSheetEditor.ExecuteVerb(Index: Integer);
var
  NewPage: TsTabSheet;
begin
  case Index of
    0: begin
      NewPage := TsTabSheet.Create(Designer.GetRoot);
      NewPage.Parent := TsTabSheet(Component).PageControl;
      NewPage.PageControl := TsTabSheet(Component).PageControl;
      NewPage.Caption := Designer.UniqueName('sTabSheet');
      NewPage.Name := NewPage.Caption;
    end;
    1: begin
      NewPage := TsTabSheet(TsTabSheet(Component).PageControl.ActivePage);
      NewPage.Free;
    end;
    2: begin
      TsTabSheet(Component).PageControl.SelectNextPage(True);
    end;
    3: begin
      TsTabSheet(Component).PageControl.SelectNextPage(False);
    end;
  end;
  if Designer <> nil then Designer.Modified;
end;

function TsTabSheetEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:  result := 'New Page';
    1:  result := 'Delete Page';
    2:  result := 'Next Page';
    3:  result := 'Previous Page';
  end;
end;

function TsTabSheetEditor.GetVerbCount: Integer;
begin
  result := 4;
end;

{ TsHintProperty }

procedure TsHintProperty.Edit;
var
  Temp: string;
  Comp: TPersistent;
  sed : TStrEditDlg;
begin
  sed := TStrEditDlg.Create(Application);
  with sed do try
    Comp := GetComponent(0);
    if Comp is TComponent then Caption := TComponent(Comp).Name + '.' + GetName else Caption := GetName;
    Temp := GetStrValue;
    Memo.Text := Temp;
{$IFNDEF TNTUNICODE}
    Memo.MaxLength := GetEditLimit;
{$ENDIF}
    UpdateStatus(nil);
    if ShowModal = mrOk then begin
      Temp := Memo.Text;
      while (Length(Temp) > 0) and (Temp[Length(Temp)] < ' ') do System.Delete(Temp, Length(Temp), 1);
      SetStrValue(Temp);
      if Designer <> nil then Designer.Modified;
    end;
  finally
    Free;
  end;
end;

function TsHintProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paAutoUpdate];
end;

function TsHintProperty.GetEditLimit: Integer;
begin
  if GetPropType^.Kind = tkString then Result := GetTypeData(GetPropType)^.MaxLength else Result := 1024;
end;

{ TsPathDlgEditor }

procedure TsPathDlgEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  TsPathDialog(Component).Execute;
end;

function TsPathDlgEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result := '&Test dialog...';
    1 : Result := '-';
  end;
end;

function TsPathDlgEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TsFrameBarEditor }

procedure TsFrameBarEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  ShowCollectionEditor(Designer, Component, (Component as TsFrameBar).Items, 'Items');
end;

function TsFrameBarEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result := '&Items editor...';
    1 : Result := '-';
  end;
end;

function TsFrameBarEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TacHintsTemplatesProperty }

procedure TacHintsTemplatesProperty.Edit;
begin
  if EditHints(TsAlphaHints(GetComponent(0))) and (Designer <> nil) then Designer.Modified;
  inherited;
end;

function TacHintsTemplatesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paAutoUpdate, paReadOnly];
end;

{ TacAlphaHintsEditor }

procedure TacAlphaHintsEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0 : if EditHints(TsAlphaHints(Component)) and (Designer <> nil) then Designer.Modified;
  end;
end;

function TacAlphaHintsEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result := '&Hints templates editor...';
  end;
end;

function TacAlphaHintsEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TacTemplateNameProperty }

function TacTemplateNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TacTemplateNameProperty.GetValues(Proc: TGetStrProc);
var
  i : integer;
begin
  inherited;
  for i := 0 to TsAlphaHints(GetComponent(0)).Templates.Count - 1 do begin
    Proc(TsAlphaHints(GetComponent(0)).Templates[i].Name);
  end;
end;

{$ENDIF}

{ TsSkinSectionProperty }

function TsSkinSectionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paAutoUpdate, paMultiSelect];
end;

procedure TsSkinSectionProperty.GetValues(Proc: TGetStrProc);
var
  i, l : integer;
begin
  inherited;
  if Assigned(DefaultManager) and (Length(DefaultManager.gd) > 0) then begin
    l := Length(DefaultManager.gd);
    for i := 0 to l - 1 do if DefaultManager.gd[i].ClassName <> s_GlobalInfo then Proc(DefaultManager.gd[i].ClassName);
  end;
end;

{ TacImageIndexEditor }

function TacImageIndexEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

function TacImageIndexEditor.GetMyList: TCustomImageList;
begin
  Result := TCustomImageList(GetObjectProp(GetComponent(0), 'Images', TObject));
end;

procedure TacImageIndexEditor.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  MyList: TCustomImageList;
begin
  MyList := GetMyList;
  if Assigned(MyList) then
  for i := 0 to MyList.Count-1 do Proc(IntToStr(i));
end;

procedure TacImageIndexEditor.ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  MyLeft: Integer;
  MyList: TCustomImageList;
begin
  ACanvas.FillRect(ARect);
  MyList := GetMyList;
  MyLeft := ARect.Left + 2;
  if Assigned(MyList) then begin
    MyList.Draw(ACanvas,MyLeft, ARect.Top + 2, StrToInt(Value));
    Inc(MyLeft, MyList.Width);
  end;
  ACanvas.TextOut(MyLeft + 2, ARect.Top + 1,Value);
end;

procedure TacImageIndexEditor.ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
var
  MyList: TCustomImageList;
begin
  MyList := GetMyList;
  AHeight := ACanvas.TextHeight(Value) + 2;
  if Assigned(MyList) and (MyList.Height + 4 > AHeight) then AHeight := MyList.Height + 4;
end;

procedure TacImageIndexEditor.ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
var
  MyList: TCustomImageList;
begin
  MyList := GetMyList;
  AWidth := ACanvas.TextWidth(Value) + 4;
  if Assigned(MyList) then Inc(AWidth, MyList.Width);
end;

{ TacThirdPartyProperty }

procedure TacThirdPartyProperty.Edit;
begin
  Application.CreateForm(TForm3rdPartyEditor, Form3rdPartyEditor);
  Form3rdPartyEditor.SM := TsSkinManager(GetComponent(0));
  Form3rdPartyEditor.sListView1.Items.Clear;
  Form3rdPartyEditor.Populate;
  Form3rdPartyEditor.ShowModal;
  if Assigned(Form3rdPartyEditor) then FreeAndNil(Form3rdPartyEditor);
  if Designer <> nil then Designer.Modified;
  inherited;
end;

function TacThirdPartyProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paAutoUpdate, paReadOnly];
end;

{ TacImgListItemsProperty }

procedure TacImgListItemsProperty.Edit;
var
  Form : TFormImgListEditor;
begin
  Application.CreateForm(TFormImgListEditor, Form);
  Form.InitFromImgList(TsAlphaImageList(GetComponent(0)));
  Form.ShowModal;
  FreeAndNil(Form);
  if Designer <> nil then Designer.Modified;
end;

function TacImgListItemsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

{ TacSkinInfoProperty }

procedure TacSkinInfoProperty.Edit;
begin
  if TsSkinManager(GetComponent(0)).SkinData.Active then begin
    SkinInfoForm := TSkinInfoForm.Create(Application);
    SkinInfoForm.sMemo1.Lines.Add('Name : ' + TsSkinManager(GetComponent(0)).SkinName);
    SkinInfoForm.sMemo1.Lines.Add('Version : ' + TsSkinManager(GetComponent(0)).SkinInfo);
    SkinInfoForm.sMemo1.Lines.Add('Author : ' + TsSkinManager(GetComponent(0)).SkinData.Author);
    SkinInfoForm.sMemo1.Lines.Add('Description : ' + TsSkinManager(GetComponent(0)).SkinData.Description);
    if TsSkinManager(GetComponent(0)).SkinData.Version < CompatibleSkinVersion then begin
      SkinInfoForm.Label1.Visible := True
    end;
    try
      SkinInfoForm.ShowModal;
    finally
      FreeAndNil(SkinInfoForm);
    end;
  end
  else MessageDlg('', mtInformation, [mbOK], 0);
end;

function TacSkinInfoProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly, paFullWidthName];
end;

{ TsImageListEditor }

procedure TsImageListEditor.ExecuteVerb(Index: Integer);
var
  Form : TFormImgListEditor;
begin
  case Index of
    0:  begin
      Application.CreateForm(TFormImgListEditor, Form);
      Form.InitFromImgList(Component as TsAlphaImageList);
      Form.ShowModal;
      FreeAndNil(Form);
    end;
  end;
  if Designer <> nil then Designer.Modified;
end;

function TsImageListEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:  result := '&ImageList editor...';
  end;
end;

function TsImageListEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

end.



