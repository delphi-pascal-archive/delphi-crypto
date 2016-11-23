unit acSelectSkin;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, sAlphaListBox, StdCtrls, Buttons, sBitBtn, sSkinProvider,
  ExtCtrls, sPanel, ComCtrls, Mask, sMaskEdit, sSkinManager, sCustomComboEdit, sTooledit;

type
  TFormSkinSelect = class(TForm)
    sListBox1: TsListBox;
    sBitBtn1: TsBitBtn;
    sBitBtn2: TsBitBtn;
    sPanel1: TsPanel;
    sSkinProvider1: TsSkinProvider;
    sDirectoryEdit1: TsDirectoryEdit;
    procedure sListBox1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sDirectoryEdit1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sListBox1DblClick(Sender: TObject);
  public
    sName : string;
    SkinTypes : TacSkinTypes;
  end;

var
  FormSkinSelect: TFormSkinSelect;

function SelectSkin(var SkinName : string; var SkinDir : string; SkinTypes : TacSkinTypes = stAllSkins) : boolean;

implementation

uses acntUtils, acSkinPreview{$IFDEF TNTUNICODE}{$IFNDEF D2006}, TntWideStrings{$ELSE}, WideStrings{$ENDIF}{$ENDIF};

{$R *.dfm}

function SelectSkin(var SkinName : string; var SkinDir : string; SkinTypes : TacSkinTypes = stAllSkins) : boolean;
begin
  Result := False;
  FormSkinSelect := TFormSkinSelect.Create(Application);
  FormSkinSelect.SkinTypes := SkinTypes;
  FormSkinSelect.sDirectoryEdit1.Text := SkinDir;
  FormSkinSelect.sName := SkinName;
  if FormSkinSelect.ShowModal = mrOk then begin
    SkinName := FormSkinSelect.sListBox1.Items[FormSkinSelect.sListBox1.ItemIndex];
    SkinDir := FormSkinSelect.sDirectoryEdit1.Text;
    Result := True;
  end;
  FreeAndNil(FormSkinSelect);
end;

procedure TFormSkinSelect.sListBox1Click(Sender: TObject);
begin
  FormSkinPreview.PreviewManager.SkinName := FormSkinSelect.sListBox1.Items[FormSkinSelect.sListBox1.ItemIndex];
  FormSkinPreview.PreviewManager.Active := True;

  sBitBtn1.Enabled := FormSkinSelect.sListBox1.ItemIndex > -1;
  FormSkinPreview. Visible := True;
end;

procedure TFormSkinSelect.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FormSkinPreview <> nil then FreeAndNil(FormSkinPreview);
end;

procedure TFormSkinSelect.sDirectoryEdit1Change(Sender: TObject);
begin
  if Assigned(FormSkinPreview) and Assigned(FormSkinPreview.PreviewManager) then begin
    FormSkinPreview.PreviewManager.SkinDirectory := sDirectoryEdit1.Text;
    sListBox1.Items.Clear;
{$IFDEF TNTUNICODE}
    FormSkinPreview.PreviewManager.GetExternalSkinNames(TWideStrings(sListBox1.Items), SkinTypes);
{$ELSE}
    FormSkinPreview.PreviewManager.GetExternalSkinNames(sListBox1.Items, SkinTypes);
{$ENDIF}    
    sBitBtn1.Enabled := sListBox1.ItemIndex > -1;
    if not sBitBtn1.Enabled then begin
      FormSkinPreview. Visible := False;
      FormSkinPreview.PreviewManager.Active := False;
    end;
  end;
end;

procedure TFormSkinSelect.FormShow(Sender: TObject);
var
  i : integer;
begin
  if FormSkinPreview = nil then begin
    FormSkinPreview := TFormSkinPreview.Create(Application);
    FormSkinPreview.Visible := False;
    FormSkinPreview.Align := alClient;
  end;
  FormSkinPreview.Parent := sPanel1;
  sDirectoryEdit1.OnChange(sDirectoryEdit1);
  if sName <> '' then begin
    i := sListBox1.Items.IndexOf(sName);
    if i > -1 then sListBox1.ItemIndex := i;
  end;
end;

procedure TFormSkinSelect.sListBox1DblClick(Sender: TObject);
begin
  sBitBtn1.Click
end;

end.
