unit acPathDialog;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, sBitBtn, ComCtrls, {sTreeView, }acShellCtrls,
  sSkinProvider, sEdit, sTreeView;

type
  TPathDialogForm = class(TForm)
    sShellTreeView1: TsShellTreeView;
    sBitBtn1: TsBitBtn;
    sBitBtn2: TsBitBtn;
    sSkinProvider1: TsSkinProvider;
    sBitBtn3: TsBitBtn;
    sEdit1: TsEdit;
    procedure sShellTreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure FormCreate(Sender: TObject);
    procedure sBitBtn3Click(Sender: TObject);
    procedure sBitBtn2Click(Sender: TObject);
    procedure sBitBtn1Click(Sender: TObject);
  public
    procedure InitLngCaptions;
  end;

var
  PathDialogForm: TPathDialogForm;

implementation

uses acntUtils, FileCtrl, sConst, Commctrl, sStrings, acSBUtils;

{$R *.DFM}

procedure TPathDialogForm.sShellTreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  sBitBtn1.Enabled := DirectoryExists(sShellTreeView1.Path);
  sBitBtn3.Enabled := DirectoryExists(sShellTreeView1.Path)
//  if sBitBtn1.Enabled then sEdit1.Text := sShellTreeView1.Path else sEdit1.Text := ''
end;

procedure TPathDialogForm.FormCreate(Sender: TObject);
begin
  sShellTreeView1.Anchors := sShellTreeView1.Anchors + [akRight, akBottom];
  sBitBtn1.Anchors := [akRight, akBottom];
  sBitBtn2.Anchors := [akRight, akBottom];
  sBitBtn3.Anchors := [akLeft, akBottom];
  RefreshScrolls(sShellTreeView1.SkinData, sShellTreeView1.ListSW);
end;

procedure TPathDialogForm.sBitBtn3Click(Sender: TObject);
var
  NewName : string;
  TreeNode : TTreeNode;
  i : integer;
  function GetUniName : string;
  var
    i : integer;
  begin
    for i := 1 to maxint - 1 do begin
      Result := NormalDir(sShellTreeView1.SelectedFolder.PathName) + s_NewFolder + IntToStr(i);
      if not acDirExists(Result) then Exit
    end;
    Result := '';
  end;
begin
  NewName := GetUniName;
  CreateDir(NewName);
  if not acDirExists(NewName) then ShowError('Directory ' + NewName + ' can`t be created!') else begin
    sShellTreeView1.Refresh(sShellTreeView1.Selected);
    RefreshScrolls(sShellTreeView1.SkinData, sShellTreeView1.ListSW);

    for i := 0 to sShellTreeView1.Selected.Count - 1 do begin
      TreeNode := sShellTreeView1.Selected.Item[i];
      if sShellTreeView1.Folders[TreeNode.AbsoluteIndex].PathName = NewName then begin
        TreeNode.Selected := True;
        sShellTreeView1.SetFocus;
        TreeNode.Expanded := True;
        TreeNode.EditText;
        sShellTreeView1.Path := NewName;
        Break;
      end;
    end;
  end;
end;

procedure TPathDialogForm.sBitBtn2Click(Sender: TObject);
begin
  if sShellTreeView1.IsEditing then ModalResult := mrNone else ModalResult := mrCancel;
  inherited;
end;

procedure TPathDialogForm.sBitBtn1Click(Sender: TObject);
begin
  if sShellTreeView1.IsEditing then begin
    ModalResult := mrNone;
    sShellTreeView1.Selected.EndEdit(False);
    sShellTreeView1.SetFocus;
  end
  else ModalResult := mrOk;
end;

procedure TPathDialogForm.InitLngCaptions;
begin
  Caption          := LoadStr(s_SelectDir);
  sBitBtn1.Caption := LoadStr(s_MsgDlgOK);
  sBitBtn2.Caption := LoadStr(s_MsgDlgCancel);
  sBitBtn3.Caption := LoadStr(s_Create);
end;

end.
