unit sInternalSkins;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, 
  StdCtrls, Buttons, sSkinManager, FileCtrl, acntUtils, ActnList, sSkinProvider, 
  ExtCtrls, sPanel, sButton, sAlphaListBox;

type
  TFormInternalSkins = class(TForm)
    ActionList1: TActionList;
    ActionAddNew: TAction;
    ActionDelete: TAction;
    ActionRename: TAction;
    ActionClose: TAction;
    sSkinProvider1: TsSkinProvider;
    ListBox1: TsListBox;
    sBitBtn1: TsButton;
    sPanel1: TsPanel;
    sButton2: TsButton;
    sButton3: TsButton;
    sButton4: TsButton;
    sButton1: TsButton;
    sButton5: TsButton;
    ActionClear: TAction;
    ActionExtract: TAction;
    procedure ActionAddNewExecute(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionRenameExecute(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ActionExtractExecute(Sender: TObject);
    procedure ActionClearExecute(Sender: TObject);
  public
    NewName : string;
    NewDir : string;
    SkinManager : TsSkinManager;
    procedure AddNewSkin;
    procedure UpdateMyActions;
  end;

var
  FormInternalSkins: TFormInternalSkins;

implementation

uses IniFiles, sStrings, sConst
{$IFNDEF ALITE}
  , acSelectSkin
{$ENDIF}
;

{$R *.DFM}

procedure TFormInternalSkins.AddNewSkin;
var
  s : string;
begin
  s := NormalDir(NewDir) + NewName + '.' + acSkinExt;
  if FileExists(s) then begin
    SkinManager.InternalSkins.Add;
    SkinManager.InternalSkins[SkinManager.InternalSkins.Count - 1].Name := NewName + ' ' + LoadStr(s_InternalSkin);
    SkinManager.InternalSkins[SkinManager.InternalSkins.Count - 1].PackedData.LoadFromFile(s);

    ListBox1.Items.Add(SkinManager.InternalSkins[SkinManager.InternalSkins.Count - 1].Name);
  end
  else begin
    s := s + '.' + acSkinExt + ' file with skin data is not exists';
    ShowWarning(s);
  end;
end;

procedure TFormInternalSkins.ActionAddNewExecute(Sender: TObject);
{$IFDEF ALITE}
var
  fod : TOpenDialog;
{$ENDIF}
begin
{$IFNDEF ALITE}
  if SelectSkin(NewName, NewDir, stPacked) then AddNewSkin;
{$ELSE}
  fod := TOpenDialog.Create(Application);
  fod.Filter := 'Packed AlphaSkins|*.' + acSkinExt;
  fod.InitialDir := NewDir;
  if fod.Execute then begin
    NewDir := ExtractFilePath(fod.FileName);
    NewName := ExtractFileName(fod.FileName);
    Delete(NewName, Length(NewName) - 3, 4);
    SkinManager.InternalSkins.Add;
    SkinManager.InternalSkins[SkinManager.InternalSkins.Count - 1].Name := NewName + ' ' + LoadStr(s_InternalSkin);
    SkinManager.InternalSkins[SkinManager.InternalSkins.Count - 1].PackedData.LoadFromFile(fod.FileName);
    ListBox1.Items.Add(SkinManager.InternalSkins[SkinManager.InternalSkins.Count - 1].Name);
  end;
  fod.Free;
{$ENDIF}
  UpdateMyActions;
end;

procedure TFormInternalSkins.ActionCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormInternalSkins.ActionRenameExecute(Sender: TObject);
var
  i : integer;
  s : string;
begin
  s := ListBox1.Items[ListBox1.ItemIndex];
  if InputQuery('String input', 'Enter new name :', s) then begin
    if ListBox1.ItemIndex > -1 then begin
      for i := 0 to SkinManager.InternalSkins.Count - 1 do begin
        if SkinManager.InternalSkins.Items[i].Name = ListBox1.Items[ListBox1.ItemIndex] then begin
          SkinManager.InternalSkins.Items[i].Name := s;
          break;
        end;
      end;
      ListBox1.Items[ListBox1.ItemIndex] := s;
    end;
  UpdateMyActions;
  end;
end;

procedure TFormInternalSkins.ActionDeleteExecute(Sender: TObject);
var
  i : integer;
begin
  if ListBox1.ItemIndex > -1 then begin
    for i := 0 to SkinManager.InternalSkins.Count - 1 do begin
      if SkinManager.InternalSkins.Items[i].Name = ListBox1.Items[ListBox1.ItemIndex] then begin
        SkinManager.InternalSkins.Delete(i);
        break;
      end;
    end;
    ListBox1.Items.Delete(ListBox1.ItemIndex);
  end;
  UpdateMyActions;
end;

procedure TFormInternalSkins.ListBox1Click(Sender: TObject);
begin
  UpdateMyActions;
end;

procedure TFormInternalSkins.FormShow(Sender: TObject);
begin
  NewDir := DefaultManager.SkinDirectory;
  UpdateMyActions;
  ReleaseCapture;
end;

procedure TFormInternalSkins.UpdateMyActions;
begin
  ActionDelete.Enabled := (ListBox1.Items.Count > 0) and (ListBox1.ItemIndex > -1);
  ActionRename.Enabled := (ListBox1.Items.Count > 0) and (ListBox1.ItemIndex > -1);
  ActionExtract.Enabled := (ListBox1.Items.Count > 0) and (ListBox1.ItemIndex > -1);
  ActionClear.Enabled := (ListBox1.Items.Count > 0);
end;

procedure TFormInternalSkins.ActionExtractExecute(Sender: TObject);
var
  s : string;
begin
  s := '\';
  SelectDirectory(s, [], -1);
  if s <> '\' then begin
    SkinManager.ExtractInternalSkin(ListBox1.Items[ListBox1.ItemIndex], s);
  end;
end;

procedure TFormInternalSkins.ActionClearExecute(Sender: TObject);
begin
{$IFNDEF ALITE}
  if Customrequest('Do you really want to delete all internal skins?') then begin
{$ELSE}
  if MessageDlg('Do you really want to delete all internal skins?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
{$ENDIF}
    while SkinManager.InternalSkins.Count > 0 do begin
      SkinManager.InternalSkins.Delete(0);
    end;
    ListBox1.Items.Clear;
    UpdateMyActions;
  end;
end;

end.
