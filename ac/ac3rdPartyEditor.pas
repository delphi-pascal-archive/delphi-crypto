unit ac3rdPartyEditor;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, {$IFDEF DELPHI6}Variants, {$ENDIF}Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, sBitBtn, ComCtrls, sListView, sSkinManager,
  sSkinProvider, sSpeedButton, Menus, sAlphaListBox, ExtCtrls, sPanel,
  sCheckListBox, sCheckBox, sLabel;

type
  TForm3rdPartyEditor = class(TForm)
    sListView1: TsListView;
    sBitBtn1: TsBitBtn;
    sBitBtn2: TsSpeedButton;
    sBitBtn3: TsSpeedButton;
    sBitBtn4: TsSpeedButton;
    sSkinProvider1: TsSkinProvider;
    PopupMenu1: TPopupMenu;
    Addnew1: TMenuItem;
    Delete1: TMenuItem;
    Defaultsettings1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    sSpeedButton1: TsSpeedButton;
    sSpeedButton2: TsSpeedButton;
    sSpeedButton3: TsSpeedButton;
    sSpeedButton4: TsSpeedButton;
    sSpeedButton5: TsSpeedButton;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    sSpeedButton6: TsSpeedButton;
    sPanel1: TsPanel;
    sListBox1: TsListBox;
    sPanel2: TsPanel;
    sListBox2: TsCheckListBox;
    Edit1: TMenuItem;
    sCheckBox1: TsCheckBox;
    sCheckBox2: TsCheckBox;
    sLabel1: TsLabel;
    procedure sBitBtn2Click(Sender: TObject);
    procedure sBitBtn1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sBitBtn3Click(Sender: TObject);
    procedure sBitBtn4Click(Sender: TObject);
    procedure sSpeedButton1Click(Sender: TObject);
    procedure sListView1Change(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure sListBox1Click(Sender: TObject);
    procedure sSpeedButton2Click(Sender: TObject);
    procedure sListView1ColumnClick(Sender: TObject; Column: TListColumn);
    procedure sSpeedButton3Click(Sender: TObject);
    procedure sSpeedButton4Click(Sender: TObject);
    procedure sSpeedButton5Click(Sender: TObject);
    procedure sSpeedButton6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure sCheckBox1Click(Sender: TObject);
    procedure sListView1DblClick(Sender: TObject);
  public
    SM : TsSkinManager;
    procedure Populate(ControlRepaint : boolean = True);
    procedure SelectCtrls(TypeIndex : integer);
  end;

var
  Form3rdPartyEditor: TForm3rdPartyEditor;

const
  // Arrary of predefined ctrls
  acCtrlsArray : array [0..13] of string = (
  // 0. Std. VCL
    'TEdit=Edit'#13#10 +
    'TMemo=Edit'#13#10 +
    'TMaskEdit=Edit'#13#10 +
    'TLabeledEdit=Edit'#13#10 +
    'THotKey=Edit'#13#10 +
    'TListBox=Edit'#13#10 +
    'TCheckListBox=Edit'#13#10 +
    'TRichEdit=Edit'#13#10 +
    'TDateTimePicker=Edit'#13#10 +
    'TStringGrid=Grid'#13#10 +
    'TDrawGrid=Grid'#13#10 +
    'TValueListEditor=Grid'#13#10 +
    'TTreeView=TreeView'#13#10 +
    'TListView=ListView'#13#10 +
{.$IFNDEF ALITE}
    'TPanel=Panel'#13#10 +
    'TButton=Button'#13#10 +
    'TBitBtn=BitBtn'#13#10 +
    'TCheckBox=CheckBox'#13#10 +
    'TRadioButton=CheckBox'#13#10 +
    'TGroupButton=CheckBox'#13#10 +
    'TGroupBox=GroupBox'#13#10 +
    'TRadioGroup=GroupBox'#13#10 +
    'TComboBox=ComboBox'#13#10 +
    'TColorBox=ComboBox'#13#10 +
    'TPageControl=PageControl'#13#10 +
    'TTabControl=TabControl'#13#10 +
    'TToolBar=ToolBar'#13#10 +
{.$ENDIF}
    'TStatusBar=StatusBar'#13#10,
  // 1. Std. DB-aware
    'TDBListBox=Edit'#13#10 +
    'TDBMemo=Edit'#13#10 +
    'TDBLookupListBox=Edit'#13#10 +
    'TDBRichEdit=Edit'#13#10 +
    'TDBCtrlGrid=Edit'#13#10 +
    'TDBEdit=Edit'#13#10 +
    'TDBRadioGroup=GroupBox'#13#10 +
    'TDBCtrlPanel=Panel'#13#10 +
    'TDBCheckBox=CheckBox'#13#10 +
    'TDBGrid=Grid'#13#10 +
    'TDBTreeView=TreeView'#13#10 +
    'TDBComboBox=ComboBox'#13#10 +
    'TDBLookupComboBox=WWEdit',
  // 2. TNT Controls
    'TTntEdit=Edit'#13#10 +
    'TTntMemo=Edit'#13#10 +
    'TTntListBox=Edit'#13#10 +
    'TTntCheckListBox=Edit'#13#10 +
    'TTntRichEdit=Edit'#13#10 +
    'TTntDBEdit=Edit'#13#10 +
    'TTntDBMemo=Edit'#13#10 +
    'TTntDBRichEdit=Edit'#13#10 +
    'TTntPanel=Panel'#13#10 +
    'TTntButton=Button'#13#10 +
    'TTntButton=Button'#13#10 +
    'TTntBitBtn=BitBtn'#13#10 +
    'TTntCheckBox=CheckBox'#13#10 +
    'TTntRadioButton=CheckBox'#13#10 +
    'TTntDBCheckBox=CheckBox'#13#10 +
    'TTntDBRadioButton=CheckBox'#13#10 +
    'TTntGroupButton=CheckBox'#13#10 +
    'TTntGroupBox=GroupBox'#13#10 +
    'TTntRadioGroup=GroupBox'#13#10 +
    'TTntDBRadioGroup=GroupBox'#13#10 +
    'TTntStringGrid=Grid'#13#10 +
    'TTntDrawGrid=Grid'#13#10 +
    'TTntDBGrid=Grid'#13#10 +
    'TTntTreeView=TreeView'#13#10 +
    'TTntComboBox=ComboBox'#13#10 +
    'TTntDBComboBox=ComboBox'#13#10 +
    'TTntListView=ListView',
  // 3. Woll2Woll
    'TwwDBGrid=Grid'#13#10 +
    'TwwDBComboBox=wwEdit'#13#10 +
    'TwwDBCustomCombo=wwEdit'#13#10 +
    'TwwDBCustomLookupCombo=wwEdit'#13#10,
  // 4. Virtual controls
    'TVirtualStringTree=VirtualTree'#13#10 +
    'TVirtualStringTreeDB=VirtualTree'#13#10 +
    'TEasyListview=VirtualTree'#13#10 +
    'TVirtualExplorerListview=VirtualTree'#13#10 +
    'TVirtualExplorerTreeview=VirtualTree'#13#10 +
    'TVirtualExplorerTree=VirtualTree'#13#10 +
    'TVirtualExplorerEasyListview=VirtualTree'#13#10 +
    'TVirtualDrawTree=VirtualTree'#13#10,
  // 5. EhLib
    'TDBCheckBoxEh=CheckBox'#13#10 +
    'TDBGridEh=GridEh'#13#10,
  // 6. FastReport
    'TfrxPreviewWorkspace=Edit'#13#10 +
    'TfrxScrollBox=Edit'#13#10 +
    'TfrxTBPanel=Panel'#13#10,
  // 7. RxLib
    '',
  // 8. JvEdits
    '',
  // 9. TMS Edits
    'TAdvStringGrid=Grid'#13#10 +
    'TDBAdvGrid=Grid'#13#10 +
    'TDBLUEdit=Edit'#13#10 +
    'TAdvSpinEdit=Edit'#13#10 +
    'TAdvLUEdit=Edit'#13#10 +
    'TAdvEditBtn=Edit'#13#10 +
    'TUnitAdvEditBtn=Edit'#13#10 +
    'TAdvFileNameEdit=Edit'#13#10 +
    'TAdvDirectoryEdit=Edit'#13#10 +
    'TDBAdvLUEdit=Edit'#13#10 +
    'TDBAdvSpinEdit=Edit'#13#10 +
    'TDBAdvEdit=Edit'#13#10 +
    'TDBAdvMaskEdit=Edit'#13#10 +
    'TEditBtn=Edit'#13#10 +
    'TUnitEditBtn=Edit'#13#10 +
    'TMoneyEdit=Edit'#13#10 +
    'TDBMoneyEdit=Edit'#13#10 +
    'TMaskEditEx=Edit'#13#10 +
    'TEditListBox=Edit'#13#10 +
    'TAdvEdit=Edit'#13#10 +
    'TAdvMaskEdit=Edit'#13#10 +
    'TLUEdit=Edit'#13#10 +
    'TDBAdvEditBtn=Edit'#13#10 +
    'TAdvMoneyEdit=Edit'#13#10 +
    'TDBAdvMoneyEdit=Edit'#13#10 +
    'THTMListBox=Edit'#13#10 +
    'THTMLCheckList=Edit'#13#10 +
    'TParamListBox=Edit'#13#10 +
    'TParamCheckList=Edit'#13#10 +
    'THTMLTreeview=TreeView'#13#10 +
    'TParamTreeview=TreeView'#13#10 +
    'TDBLUCombo=ComboBox'#13#10 +
    'TAdvComboBox=ComboBox'#13#10 +
    'TLUCombo=ComboBox'#13#10 +
    'TAdvDBLookupComboBox=ComboBox'#13#10 +
    'TAdvTreeComboBox=ComboBox'#13#10 +
    'THTMLComboBox=ComboBox'#13#10 +
    'TCheckListEdit=wwEdit'#13#10,
  // 10. SynEdits
    'TSynEdit=Edit'#13#10 +
    'TSynMemo=Edit'#13#10 +
    'TDBSynEdit=Edit'#13#10,
  // 11. mxEdits
    '',
  // 12. RichViews
    'TRichView=Grid'#13#10 +
    'TDBRichViewEdit=Grid'#13#10 +
    'TRichViewEdit=Grid'#13#10 +
    'TDBRichView=Grid'#13#10,
  // 13. Raize
    'TRzTreeView=TreeView'#13#10 +
    'TRzEdit=Edit'#13#10 +
    'TRzHotKeyEdit=Edit'#13#10 +
    'TRzMaskEdit=Edit'#13#10 +
    'TRzNumericEdit=Edit'#13#10 +
    'TRzExpandEdit=Edit'#13#10 +
    'TRzMemo=Edit'#13#10 +
    'TRzListBox=Edit'#13#10 +
    'TRzRankListBox=Edit'#13#10 +
    'TRzTabbedListBox=Edit'#13#10 +
    'TRzCheckList=Edit'#13#10 +
    'TRzEditListBox=Edit'#13#10 +
    'TRzCheckTree=TreeView'#13#10 +
    'TRzRichEdit=Edit'#13#10 +
    'TRzShellTree=TreeView'#13#10 +
    'TRzGroupBox=GroupBox'#13#10 +
    'TRzListView=ListView'#13#10 +
    'TRzShellList=ListView'#13#10 +
    'TRzPanel=Panel'#13#10 +
    'TRzComboBox=ComboBox'#13#10 +
    'TRzImageComboBox=ComboBox'#13#10 +
    'TRzMRUComboBox=ComboBox'#13#10 +
    'TRzShellCombo=ComboBox'#13#10 +
    'TRzDateTimeEdit=wwEdit'
  );

implementation

{$R *.dfm}

uses sDefaults, ac3dNewClass, acntUtils, IniFiles, sStoreUtils;

procedure TForm3rdPartyEditor.Populate(ControlRepaint : boolean = True);
var
  i, j : integer;
begin
  if ControlRepaint then sListView1.Items.BeginUpdate;
  sListView1.Items.Clear;
  for j := 0 to Length(SM.ThirdLists) - 1 do begin
    for i := 0 to SM.ThirdLists[j].Count - 1 do if (SM.ThirdLists[j][i] <> ' ') then begin
      sListView1.Items.Add;
      sListView1.Items[sListView1.Items.Count - 1].Caption := SM.ThirdLists[j][i];
      sListView1.Items[sListView1.Items.Count - 1].SubItems.Add(acThirdCaptions[j]);
      sListView1.Items[sListView1.Items.Count - 1].ImageIndex := j;
    end;
  end;
  if ControlRepaint then begin
    sListView1.Items.EndUpdate;
    RedrawWindow(sListView1.Handle, nil, 0, RDW_UPDATENOW or RDW_ERASE or RDW_INVALIDATE);
  end;
end;

procedure TForm3rdPartyEditor.sBitBtn2Click(Sender: TObject);
begin
  FormNewThirdClass := TFormNewThirdClass.Create(Application);
  FormNewThirdClass.sEdit1.Text := 'T';
  FormNewThirdClass.ShowModal;
  if FormNewThirdClass.ModalResult = mrOk then begin
    SM.ThirdLists[FormNewThirdClass.sComboBox1.ItemIndex].Add(FormNewThirdClass.sEdit1.Text);
    UpdateThirdNames(SM);
    Populate;
  end;
  FreeAndNil(FormNewThirdClass);
end;

procedure TForm3rdPartyEditor.sBitBtn1Click(Sender: TObject);
begin
  Close
end;

procedure TForm3rdPartyEditor.FormShow(Sender: TObject);
begin
  sListView1.Columns[1].Width := 150;
end;

procedure TForm3rdPartyEditor.sBitBtn3Click(Sender: TObject);
var
  i, j : integer;
{$IFDEF DELPHI6UP}
  LastIndex : integer;
{$ENDIF}
begin
{$IFDEF DELPHI6UP}
  LastIndex := sListView1.ItemIndex;
{$ENDIF}
  for i := 0 to sListView1.Items.Count - 1 do if sListView1.Items[i].Selected then begin
    j := 0;
    while j < SM.ThirdLists[sListView1.Items[i].ImageIndex].Count do begin
      if SM.ThirdLists[sListView1.Items[i].ImageIndex][j] = sListView1.Items[i].Caption then begin
        SM.ThirdLists[sListView1.Items[i].ImageIndex].Delete(j);
        if SM.ThirdLists[sListView1.Items[i].ImageIndex].Count = 0 then SM.ThirdLists[sListView1.Items[i].ImageIndex].Text := ' ';
      end
      else inc(j);
    end;
  end;
  UpdateThirdNames(SM);
  Populate;
{$IFDEF DELPHI6UP}
  if LastIndex > sListView1.Items.Count - 1 then sListView1.ItemIndex := sListView1.Items.Count - 1 else sListView1.ItemIndex := LastIndex;
{$ENDIF}
end;

procedure TForm3rdPartyEditor.sBitBtn4Click(Sender: TObject);
begin
  LoadThirdNames(SM, True);
  Populate;
end;

procedure TForm3rdPartyEditor.sCheckBox1Click(Sender: TObject);
var
  i : integer;
begin
  TsCheckBox(Sender).Checked := boolean(TsCheckBox(Sender).Tag);
  for i := 0 to sListBox2.Items.Count - 1 do
    if sListBox2.ItemEnabled[i] then sListBox2.Checked[i] := boolean(TsCheckBox(Sender).Tag);
end;

procedure TForm3rdPartyEditor.sSpeedButton1Click(Sender: TObject);
var
  j, Ndx : integer;
begin
  FormNewThirdClass := TFormNewThirdClass.Create(Application);
  Ndx := {$IFDEF DELPHI6UP}sListView1.ItemIndex{$ELSE}sListView1.Selected.Index{$ENDIF};
  FormNewThirdClass.sEdit1.Text := sListView1.Items[Ndx].Caption;
  FormNewThirdClass.sComboBox1.ItemIndex := FormNewThirdClass.sComboBox1.IndexOf(sListView1.Items[Ndx].SubItems[0]);
  FormNewThirdClass.Caption := 'Edit';
  FormNewThirdClass.ShowModal;
  if FormNewThirdClass.ModalResult = mrOk then begin
    j := 0;
    while j < SM.ThirdLists[sListView1.Items[Ndx].ImageIndex].Count do begin
      if SM.ThirdLists[sListView1.Items[Ndx].ImageIndex][j] = sListView1.Items[Ndx].Caption then begin
        SM.ThirdLists[sListView1.Items[Ndx].ImageIndex].Delete(j);
      end
      else inc(j);
    end;
    SM.ThirdLists[FormNewThirdClass.sComboBox1.ItemIndex].Add(FormNewThirdClass.sEdit1.Text);
    UpdateThirdNames(SM);
    Populate;
  end;
  FreeAndNil(FormNewThirdClass);
end;

procedure TForm3rdPartyEditor.sListView1Change(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  sSpeedButton1.Enabled := sListView1.Selected <> nil;
  Delete1.Enabled := sSpeedButton1.Enabled;
  Edit1.Enabled := sSpeedButton1.Enabled;
end;

procedure TForm3rdPartyEditor.sListBox1Click(Sender: TObject);
  procedure ShowSupportedControls;
  var
    sl : TStringList;
    i, j : integer;
    s1, s2 : string;
  begin
    sListBox2.Items.BeginUpdate;
    sListBox2.Perform(WM_SETREDRAW, 0, 0);
//    sListBox2.SkinData.BeginUpdate;
    sListBox2.Items.Clear;

    sl := TStringList.Create;
    sl.Text := acCtrlsArray[sListBox1.ItemIndex];

    for i := 0 to sl.Count - 1 do begin
      s1 := acntUtils.ExtractWord(1, sl[i], ['=']); // Name of type
      s2 := acntUtils.ExtractWord(2, sl[i], ['=']); // Rule of skinning

      // Add new value
      for j := 0 to Length(acThirdCaptions) - 1 do if acThirdCaptions[j] = s2 then begin
        sListBox2.Items.Add(s1);
{$IFDEF ALITE}
        if sListBox1.ItemIndex <> 0
          then sListBox2.ItemEnabled[sListBox2.Items.Count - 1] := False
          else
            if (s2 <> acThirdCaptions[ord(tpEdit)]) and (s2 <> acThirdCaptions[ord(tpButton)]) and (s2 <> acThirdCaptions[ord(tpGrid)]) and (s2 <> acThirdCaptions[ord(tpListView)]) and (s2 <> acThirdCaptions[ord(tpTreeView)]) and (s2 <> acThirdCaptions[ord(tpStatusBar)])
              then sListBox2.ItemEnabled[sListBox2.Items.Count - 1] := False
              else
{$ENDIF}
        sListBox2.Checked[sListBox2.Items.Count - 1] := True;
        Break;
      end
    end;

    sl.Free;
//    sListBox2.SkinData.EndUpdate;
    sListBox2.Perform(WM_SETREDRAW, 1, 0);
    sListBox2.Items.EndUpdate;
  end;
begin
  ShowSupportedControls;
  sSpeedButton2.Enabled := {$IFNDEF ALITE}sListBox1.ItemIndex >= 0{$ELSE}sListBox1.ItemIndex = 0{$ENDIF};
  Addnew1.Enabled := sSpeedButton2.Enabled;
  sSpeedButton3.Enabled := sSpeedButton2.Enabled;
  SelectCtrls(sListBox1.ItemIndex);
end;

procedure TForm3rdPartyEditor.sSpeedButton2Click(Sender: TObject);
var
  sl : TStringList;
  i, j, k, l : integer;
  s1, s2 : string;
begin
  sl := TStringList.Create;
  sl.Text := acCtrlsArray[sListBox1.ItemIndex];

  for i := 0 to sl.Count - 1 do begin
//    alert(sl[i]);
    s1 := acntUtils.ExtractWord(1, sl[i], ['=']); // Name of type
    s2 := acntUtils.ExtractWord(2, sl[i], ['=']); // Rule of skinning

    // Delete if exists already
    for j := 0 to Length(SM.ThirdLists) - 1 do begin
      k := 0;
      while k < SM.ThirdLists[j].Count do begin
        if (SM.ThirdLists[j][k] = s1)
          then SM.ThirdLists[j].Delete(k)
          else inc(k);
      end;
    end;
    // Add new value
    for j := 0 to Length(acThirdCaptions) - 1 do if acThirdCaptions[j] = s2 then begin
      l := sListBox2.Items.IndexOf(s1);
      if (l > -1) and (sListBox2.Checked[l]) then SM.ThirdLists[j].Add(s1);
      Break;
    end
  end;

  sl.Free;
  UpdateThirdNames(SM);
  Populate;
  SelectCtrls(sListBox1.ItemIndex);
end;

procedure TForm3rdPartyEditor.sListView1ColumnClick(Sender: TObject; Column: TListColumn);
begin
  if Column.Index = 0 then sListView1.SortType := stText else sListView1.SortType := stData;
  Populate;
end;

procedure TForm3rdPartyEditor.sListView1DblClick(Sender: TObject);
begin
  if sSpeedButton1.Enabled then sSpeedButton1.Click;
end;

procedure TForm3rdPartyEditor.sSpeedButton3Click(Sender: TObject);
var
  sl : TStringList;
  i, j, k : integer;
  s1, s2 : string;
begin
  sl := TStringList.Create;
  sl.Text := acCtrlsArray[sListBox1.ItemIndex];

  for i := 0 to sl.Count - 1 do begin
    s1 := acntUtils.ExtractWord(1, sl[i], ['=']); // Name of type
    s2 := acntUtils.ExtractWord(2, sl[i], ['=']); // Rule of skinning

    // Delete if exists already
    for j := 0 to Length(SM.ThirdLists) - 1 do begin
      k := 0;
      while k < SM.ThirdLists[j].Count do begin
        if (SM.ThirdLists[j][k] = s1) then SM.ThirdLists[j].Delete(k) else inc(k);
      end;
    end;
  end;

  sl.Free;
  UpdateThirdNames(SM);
  Populate;
end;

procedure TForm3rdPartyEditor.SelectCtrls(TypeIndex: integer);
var
  sl : TStringList;
  i, j : integer;
  s1 : string;
begin
  sl := TStringList.Create;
  sl.Text := acCtrlsArray[TypeIndex];

  for j := 0 to sListView1.Items.Count - 1 do sListView1.Items[j].Selected := False;

  for i := 0 to sl.Count - 1 do begin
    s1 := acntUtils.ExtractWord(1, sl[i], ['=']); // Name of type
    // Search
    for j := 0 to sListView1.Items.Count - 1 do if sListView1.Items[j].Caption = s1 then begin
      sListView1.Items[j].Selected := True;
      Break;
    end;
  end;

  sl.Free;
end;

const
  s_ThirdParty = 'ThirdParty';

procedure TForm3rdPartyEditor.sSpeedButton4Click(Sender: TObject);
var
  i, j : integer;
  iFile : TMeminiFile;
  s1, s2 : string;
begin
  if SaveDialog1.Execute then begin
    iFile := TMeminiFile.Create(SaveDialog1.FileName);
    for j := 0 to Length(SM.ThirdLists) - 1 do begin
      for i := 0 to SM.ThirdLists[j].Count - 1 do if (SM.ThirdLists[j][i] <> ' ') then begin
        s1 := SM.ThirdLists[j][i];
        s2 := acThirdCaptions[j];
        WriteIniStr(s_ThirdParty, s1, s2, iFile);
      end;
    end;
    iFile.UpdateFile;
    iFile.Free;
  end;
end;

procedure TForm3rdPartyEditor.sSpeedButton5Click(Sender: TObject);
var
  i, j : integer;
  iFile : TMeminiFile;
  s1, s2 : string;
  sl : TStringList;
begin
  if OpenDialog1.Execute then begin
    iFile := TMeminiFile.Create(OpenDialog1.FileName);
    for j := 0 to Length(SM.ThirdLists) - 1 do SM.ThirdLists[j].Clear;
    sl := TStringList.Create;
    iFile.ReadSection(s_ThirdParty, sl);
    for i := 0 to sl.Count - 1 do begin
      s1 := sl[i];
      s2 := ReadIniString(s_ThirdParty, s1, iFile);

      for j := 0 to Length(acThirdCaptions) - 1 do if acThirdCaptions[j] = s2 then begin
        SM.ThirdLists[j].Add(s1);
        Break;
      end;
    end;
    iFile.Free;
  end;
  Populate;
end;

procedure TForm3rdPartyEditor.sSpeedButton6Click(Sender: TObject);
var
  j : integer;
begin
  for j := 0 to Length(SM.ThirdLists) - 1 do SM.ThirdLists[j].Clear;
  Populate;
end;

procedure TForm3rdPartyEditor.FormCreate(Sender: TObject);
{$IFDEF ALITE}
var
  s : string;
{$ENDIF}
begin
{$IFDEF ALITE}
  sBitBtn2.Enabled := False;
  sBitBtn2.Hint := 'Feature is not available for the package  Lite Edition';
  sBitBtn4.Enabled := False;
  sBitBtn4.Hint := sBitBtn2.Hint;
  sSpeedButton4.Enabled := False;
  sSpeedButton4.Hint := sBitBtn2.Hint;
  sSpeedButton5.Enabled := False;
  sSpeedButton5.Hint := sBitBtn2.Hint;

  s := acThirdCaptions[ord(tpEdit)] + ', ';
  s := s + acThirdCaptions[ord(tpButton)] + ', ';
  s := s + acThirdCaptions[ord(tpGrid)] + ', ';
  s := s + acThirdCaptions[ord(tpListView)] + ', ';
  s := s + acThirdCaptions[ord(tpTreeView)] + ', ';
  s := s + acThirdCaptions[ord(tpStatusBar)];
  ShowMessage('List of supported controls for the Lite Edition release is limited by std. controls with such types of skinning : '#13#10 + s);
{$ENDIF}
end;

procedure TForm3rdPartyEditor.Edit1Click(Sender: TObject);
begin
  sSpeedButton1.Click
end;

end.
