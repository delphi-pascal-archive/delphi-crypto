unit sStrEdit;
{$I sDefs.inc}
// !!! Based on RxLib package !!! 

interface

uses
  Windows, Classes, Graphics, Forms, Controls, Buttons, Dialogs,
  StdCtrls, ExtCtrls, sSkinProvider, sButton, sMemo, sLabel, sPanel;

type
  TStrEditDlg = class(TForm)
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    OKBtn: TsButton;
    CancelBtn: TsButton;
    HelpBtn: TsButton;
    LoadBtn: TsButton;
    SaveBtn: TsButton;
    sSkinProvider1: TsSkinProvider;
    LineCount: TsLabel;
    Memo: TsMemo;
    procedure FileOpen(Sender: TObject);
    procedure FileSave(Sender: TObject);
    procedure UpdateStatus(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HelpBtnClick(Sender: TObject);
  private
    SingleLine: string;
    MultipleLines: string;
  end;

implementation

{$R *.DFM}

uses SysUtils, LibHelp;

{ TStrListEditDlg }

procedure TStrEditDlg.FileOpen(Sender: TObject);
begin
  with OpenDialog do if Execute then Memo.Lines.LoadFromFile(FileName);
end;

procedure TStrEditDlg.FileSave(Sender: TObject);
begin
  SaveDialog.FileName := OpenDialog.FileName;
  with SaveDialog do if Execute then Memo.Lines.SaveToFile(FileName);
end;

procedure TStrEditDlg.UpdateStatus(Sender: TObject);
var
  Count: Integer;
begin
  Count := Memo.Lines.Count;
  if Count = 1
    then LineCount.Caption := Format('%d %s', [Count, SingleLine])
    else LineCount.Caption := Format('%d %s', [Count, MultipleLines]);
end;

procedure TStrEditDlg.FormCreate(Sender: TObject);
begin
  HelpContext := hcDStringListEditor;
  OpenDialog.HelpContext := hcDStringListLoad;
  SaveDialog.HelpContext := hcDStringListSave;
  SingleLine := 'Line';
  MultipleLines := 'Lines';
end;

procedure TStrEditDlg.MemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then CancelBtn.Click;
end;

procedure TStrEditDlg.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

end.
