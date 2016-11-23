unit acAlphaHintsEdit;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, {$IFDEF DELPHI6}Variants, {$ENDIF}Classes, Graphics, Controls, Forms,
  Dialogs, acAlphaHints, sAlphaListBox, StdCtrls, Buttons, sBitBtn,
  sSpeedButton, sEdit, ComCtrls, sPageControl, sLabel, sCheckBox,
  acHintPage, sGroupBox, sSkinProvider;

type
  TAlphaHintsEdit = class(TForm)
    sBitBtn1: TsBitBtn;
    sBitBtn2: TsBitBtn;
    sPageControl1: TsPageControl;
    sEdit1: TsEdit;
    sTabSheet1: TsTabSheet;
    sTabSheet2: TsTabSheet;
    sTabSheet3: TsTabSheet;
    sTabSheet4: TsTabSheet;
    sCheckBox1: TsCheckBox;
    FrameTL: TFrameHintPage;
    Hints: TsAlphaHints;
    sGroupBox1: TsGroupBox;
    sSpeedButton1: TsSpeedButton;
    sSpeedButton2: TsSpeedButton;
    sListBox1: TsListBox;
    sSkinProvider1: TsSkinProvider;
    sSpeedButton3: TsSpeedButton;
    FontDialog1: TFontDialog;
    procedure sCheckBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sListBox1Click(Sender: TObject);
    procedure sEdit1Change(Sender: TObject);
    procedure sPageControl1Change(Sender: TObject);
    procedure sSpeedButton3Click(Sender: TObject);
    procedure sSpeedButton1Click(Sender: TObject);
    procedure sSpeedButton2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    CurrentFrame : TFrameHintPage;
    procedure LoadTemplates;
    procedure InitTemplate(Index : integer);
    procedure UpdateFrame(Sender: TObject);
  end;

var
  AlphaHintsEdit: TAlphaHintsEdit;
  FrameTR : TFrameHintPage = nil;
  FrameBR : TFrameHintPage = nil;
  FrameBL : TFrameHintPage = nil;
  CurrentTemplate : TacHintTemplate;

function EditHints(acHints: TsAlphaHints) : boolean;
procedure ChangeStates(Parent : TWinControl; Tag : integer; Enabled : boolean);

var
  PreviewBmp : TBitmap = nil;

implementation

{$R *.dfm}

var
  acInitializing : boolean = False;

procedure ChangeStates(Parent : TWinControl; Tag : integer; Enabled : boolean);
var
  i : integer;
begin
  for i := 0 to Parent.ControlCount - 1 do begin
    if Parent.Controls[i].Tag = Tag then Parent.Controls[i].Enabled := Enabled;
    if Parent.Controls[i] is TWinControl then ChangeStates(TWinControl(Parent.Controls[i]), Tag, Enabled);
  end;
end;

function EditHints(acHints: TsAlphaHints) : boolean;
begin
  Application.CreateForm(TAlphaHintsEdit, AlphaHintsEdit);
  AlphaHintsEdit.Hints.Assign(acHints);
  AlphaHintsEdit.LoadTemplates;
  AlphaHintsEdit.ShowModal;
  if AlphaHintsEdit.ModalResult = mrOk then begin
    acHints.Assign(AlphaHintsEdit.Hints);
    Result := True;
  end
  else Result := False;
  FreeAndNil(AlphaHintsEdit);
end;

procedure TAlphaHintsEdit.sCheckBox1Click(Sender: TObject);
begin
  if sCheckBox1.Checked then begin
    if FrameTR = nil then begin
      FrameTR := TFrameHintPage.Create(Self);
      FrameTR.Parent := sTabSheet2;
      FrameTR.Left := 2;
      FrameTR.Top := 2;
      FrameTR.Name := 'FrameTR';
      FrameTR.LoadImage(CurrentTemplate.Img_RightTop, False);
    end;
    if FrameBL = nil then begin
      FrameBL := TFrameHintPage.Create(Self);
      FrameBL.Parent := sTabSheet3;
      FrameBL.Left := 2;
      FrameBL.Top := 2;
      FrameBL.Name := 'FrameBL';
      FrameBL.LoadImage(CurrentTemplate.Img_LeftBottom, False);
    end;
    if FrameBR = nil then begin
      FrameBR := TFrameHintPage.Create(Self);
      FrameBR.Parent := sTabSheet4;
      FrameBR.Left := 2;
      FrameBR.Top := 2;
      FrameBR.Name := 'FrameBR';
      FrameBR.LoadImage(CurrentTemplate.Img_RightBottom, False);
    end;
    ChangeStates(sPageControl1, 1, True);
  end
  else begin
    if FrameTR <> nil then begin
      FreeAndNil(FrameTR);
    end;
    if FrameBR <> nil then begin
      FreeAndNil(FrameBR);
    end;
    if FrameBL <> nil then begin
      FreeAndNil(FrameBL);
    end;
  end;
  sTabSheet2.TabVisible := sCheckBox1.Checked;
  sTabSheet3.TabVisible := sCheckBox1.Checked;
  sTabSheet4.TabVisible := sCheckBox1.Checked;
end;

procedure TAlphaHintsEdit.FormCreate(Sender: TObject);
begin
  FrameTR := nil;
  FrameBR := nil;
  FrameBL := nil;
  CurrentTemplate := nil;
  Hints.OnChange := UpdateFrame;
end;

procedure TAlphaHintsEdit.LoadTemplates;
var
  i, DefIndex : integer;
begin
  CurrentFrame := FrameTL;
  sListBox1.Clear;
  DefIndex := -1;
  for i := 0 to Hints.Templates.Count - 1 do begin
    sListBox1.Items.Add(Hints.Templates[i].Name);
    if (DefIndex = -1) and (Hints.Templates[i].Name = Hints.TemplateName) then DefIndex := i;
  end;
  if DefIndex <> -1 then begin
    sListBox1.ItemIndex := DefIndex;
    InitTemplate(DefIndex);
  end;
end;

procedure TAlphaHintsEdit.sListBox1Click(Sender: TObject);
begin
  inherited;
  if sListBox1.ItemIndex > -1 then begin
    if sEdit1.Text <> sListBox1.Items[sListBox1.ItemIndex] then InitTemplate(sListBox1.ItemIndex);
  end
  else FrameTL.LoadImage(nil);
end;

procedure TAlphaHintsEdit.InitTemplate(Index: integer);
begin
  acInitializing := True;
  CurrentTemplate := Hints.Templates[Index];
  sEdit1.Text := CurrentTemplate.Name;
  FrameTL.LoadImage(CurrentTemplate.ImageDefault);

  if not CurrentTemplate.Img_LeftBottom.Image.Empty or not CurrentTemplate.Img_RightBottom.Image.Empty or not CurrentTemplate.Img_RightTop.Image.Empty then begin
    sCheckBox1.Checked := True;
  end
  else sCheckBox1.Checked := False;
  ChangeStates(Self, 1, True);
  acInitializing := False;
end;

procedure TAlphaHintsEdit.sEdit1Change(Sender: TObject);
begin
  if acInitializing then Exit;
  CurrentTemplate.Name := sEdit1.Text;
  sListBox1.Items[sListBox1.ItemIndex] := sEdit1.Text;
end;

procedure TAlphaHintsEdit.sPageControl1Change(Sender: TObject);
begin
  case sPageControl1.ActivePageIndex of
    0 : CurrentFrame := FrameTL;
    1 : CurrentFrame := FrameTR;
    2 : CurrentFrame := FrameBL;
    3 : CurrentFrame := FrameBR;
  end;
  CurrentFrame.UpdateData;//.RefreshPaintBox;
end;

procedure TAlphaHintsEdit.UpdateFrame(Sender: TObject);
begin
  if (CurrentFrame <> nil) and (CurrentFrame.Image <> nil) then CurrentFrame.UpdateData;
end;

procedure TAlphaHintsEdit.sSpeedButton3Click(Sender: TObject);
begin
  FontDialog1.Font.Assign(CurrentTemplate.Font);
  if FontDialog1.Execute then begin
    CurrentTemplate.Font.Assign(FontDialog1.Font);
    UpdateFrame(Hints);
  end;
end;

procedure TAlphaHintsEdit.sSpeedButton1Click(Sender: TObject);
begin
  CurrentTemplate := TacHintTemplate(Hints.Templates.Add);
  CurrentTemplate.Name := 'New template';
  sListBox1.Items.Add(CurrentTemplate.Name);
  sListBox1.ItemIndex := sListBox1.Items.Count - 1;
  InitTemplate(sListBox1.ItemIndex);
  sEdit1.SetFocus;
  sEdit1.SelectAll;
end;

procedure TAlphaHintsEdit.sSpeedButton2Click(Sender: TObject);
begin
  CurrentTemplate := nil;
  Hints.Templates.Delete(sListBox1.ItemIndex);
  sListBox1.Items.Delete(sListBox1.ItemIndex);
  sListBox1.ItemIndex := sListBox1.Items.Count - 1;
  sListBox1.OnClick(sListBox1);
//  if sListBox1.ItemIndex < 0 then sSpeedButton2.Enabled := False;
end;

procedure TAlphaHintsEdit.FormDestroy(Sender: TObject);
begin
  if PreviewBmp <> nil then FreeAndNil(PreviewBmp)
end;

end.
