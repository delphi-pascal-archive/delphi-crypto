unit sHintDesigner;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, sHintManager, ImgList, ExtDlgs, StdCtrls, Buttons, ExtCtrls, ComCtrls,
  sEdit, sBitBtn, sSpeedButton, sPanel, sCheckBox, sGroupBox, sTrackBar,
  sComboBox, sLabel, sSkinProvider, sColorSelect, sDialogs;

type
  THintDesigner = class(TForm)
    Label1: TsLabel;
    Label3: TsLabel;
    Label5: TsLabel;
    Label6: TsLabel;
    Label7: TsLabel;
    Label8: TsLabel;
    Label9: TsLabel;
    Label10: TsLabel;
    sComboBox4: TsComboBox;
    sComboBox1: TsComboBox;
    sTrackBar1: TsTrackBar;
    sTrackBar2: TsTrackBar;
    sTrackBar3: TsTrackBar;
    sTrackBar4: TsTrackBar;
    sTrackBar5: TsTrackBar;
    sTrackBar21: TsTrackBar;
    GroupBox1: TsGroupBox;
    Label12: TsLabel;
    Label13: TsLabel;
    Label14: TsLabel;
    sCheckBox1: TsCheckBox;
    sTrackBar9: TsTrackBar;
    sTrackBar10: TsTrackBar;
    sTrackBar11: TsTrackBar;
    GroupBox2: TsGroupBox;
    GroupBox3: TsGroupBox;
    BitBtn1: TsBitBtn;
    Panel1: TsPanel;
    sSpinEdit2: TsEdit;
    FontDialog1: TFontDialog;
    PopupMenu1: TPopupMenu;
    Gotohomesite1: TMenuItem;
    Feedback1: TMenuItem;
    sTrackBar12: TsTrackBar;
    Label17: TsLabel;
    Label2: TsLabel;
    Panel4: TsPanel;
    PaintBox1: TPaintBox;
    TrackBar1: TsTrackBar;
    Label11: TsLabel;
    BitBtn2: TsBitBtn;
    BitBtn3: TsBitBtn;
    OpenPictureDialog1: TOpenPictureDialog;
    TrackBar2: TsTrackBar;
    Label15: TsLabel;
    PaintBox2: TPaintBox;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    sSkinProvider1: TsSkinProvider;
    sColorSelect2: TsColorSelect;
    sColorSelect3: TsColorSelect;
    sColorSelect4: TsColorSelect;
    sBitBtn1: TsBitBtn;
    sBitBtn2: TsBitBtn;
    procedure sComboBox4Change(Sender: TObject);
    procedure sComboBox1Change(Sender: TObject);
    procedure sTrackBar1Change(Sender: TObject);
    procedure sTrackBar2Change(Sender: TObject);
    procedure sTrackBar3Change(Sender: TObject);
    procedure sTrackBar4Change(Sender: TObject);
    procedure sTrackBar5Change(Sender: TObject);
    procedure sTrackBar21Change(Sender: TObject);
    procedure sSpinEdit2Change(Sender: TObject);
    procedure sCheckBox1Click(Sender: TObject);
    procedure sTrackBar9Change(Sender: TObject);
    procedure sTrackBar10Change(Sender: TObject);
    procedure sTrackBar11Change(Sender: TObject);
    procedure sTrackBar12Change(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure PaintBox2Paint(Sender: TObject);
    procedure PaintBox2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure sColorSelect2Change(Sender: TObject);
    procedure sColorSelect3Change(Sender: TObject);
    procedure sColorSelect4Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sBitBtn1Click(Sender: TObject);
    procedure sBitBtn2Click(Sender: TObject);
    procedure OpenDialog1SelectionChange(Sender: TObject);
  public
    sHintManager1 : TsHintManager;
    PreviewWnd : TsCustomHintWindow;
    procedure SetPanelColor(Panel : TPanel; c : TColor);
    procedure InitControls(Manager : TsHintManager; Sender : TObject);
    procedure LoadPreserved(i : integer);
    procedure SetcustomStyle;
    procedure PrepareMainBmp;
    procedure LoadFromFile(const FileName : string);
    procedure WndProc (var Message: TMessage); override;
    procedure InitLngCaptions;
  end;

var
  HintDesigner: THintDesigner;
  MainBmp : TBitmap;

procedure CreateEditorForm;
procedure OpenDesigner;
procedure KillForm;

implementation

uses sConst, ShellApi, sGraphUtils, acntUtils, sGradient, sGradBuilder, sStoreUtils,
  sStyleSimply, sMessages, IniFiles, sSkinManager, sStrings, sVCLUtils;

const
  sSection = 'Properties';

var
  Changing : boolean = False;

{$R *.DFM}

procedure CreateEditorForm;
begin
  Application.CreateForm(THintDesigner, HintDesigner);
  HintDesigner.InitLngCaptions;
//  SkinForm(HintDesigner);
end;

procedure OpenDesigner;
begin
  HintDesigner.ShowModal;
end;

procedure KillForm;
begin
//  UnSkinform(HintDesigner);
  if Assigned(HintDesigner) then HintDesigner.Release;
end;

procedure THintDesigner.sComboBox4Change(Sender: TObject);
begin
  if sComboBox4.ItemIndex < sComboBox4.Items.Count - 1
    then sHintManager1.PreDefinitions := asHintsPredefinitions[sComboBox4.ItemIndex];
  sHintManager1.UpdateProperties;
  InitControls(sHintManager1, sComboBox4);
end;

procedure THintDesigner.sComboBox1Change(Sender: TObject);
begin
  sHintManager1.HintKind.Style := aHintStyles[sComboBox1.ItemIndex];
  SetcustomStyle
end;

procedure THintDesigner.sTrackBar1Change(Sender: TObject);
begin
  sHintManager1.HintKind.BevelWidth := sTrackBar1.Position;
  SetcustomStyle;
end;

procedure THintDesigner.sTrackBar2Change(Sender: TObject);
begin
  sHintManager1.HintKind.ExOffset := sTrackBar2.Position;
  SetcustomStyle
end;

procedure THintDesigner.sTrackBar3Change(Sender: TObject);
begin
  sHintManager1.HintKind.MarginH := sTrackBar3.Position;
  SetcustomStyle
end;

procedure THintDesigner.sTrackBar4Change(Sender: TObject);
begin
  sHintManager1.HintKind.MarginV := sTrackBar4.Position;
  SetcustomStyle
end;

procedure THintDesigner.sTrackBar5Change(Sender: TObject);
begin
  sHintManager1.HintKind.Radius := sTrackBar5.Position;
  SetcustomStyle
end;

procedure THintDesigner.sTrackBar21Change(Sender: TObject);
begin
  Label9.Caption := LoadStr(s_HintDsgnMaxWidth) + ' = ' + IntToStr(sTrackBar21.Position);
  sHintManager1.HintKind.MaxWidth := sTrackBar21.Position;
  SetcustomStyle
end;

procedure THintDesigner.sSpinEdit2Change(Sender: TObject);
begin
  if sSpinEdit2.Text = '' then sSpinEdit2.Text := '0';
  try
    sHintManager1.PauseHide := StrToInt(sSpinEdit2.Text);
  except
    sSpinEdit2.Text := '5000';
  end;
end;

procedure THintDesigner.sCheckBox1Click(Sender: TObject);
begin
  sHintManager1.HintKind.ShadowEnabled := sCheckBox1.Checked;
  SetcustomStyle
end;

procedure THintDesigner.sTrackBar9Change(Sender: TObject);
begin
  sHintManager1.HintKind.ShadowBlur := sTrackBar9.Position;
  SetcustomStyle
end;

procedure THintDesigner.sTrackBar10Change(Sender: TObject);
begin
  sHintManager1.HintKind.ShadowOffset := sTrackBar10.Position;
  SetcustomStyle
end;

procedure THintDesigner.sTrackBar11Change(Sender: TObject);
begin
  sHintManager1.HintKind.ShadowTransparency := sTrackBar11.Position;
  SetcustomStyle
end;

procedure THintDesigner.sTrackBar12Change(Sender: TObject);
begin
  sHintManager1.HintKind.Transparency := sTrackBar12.Position;
  SetcustomStyle
end;

procedure THintDesigner.BitBtn1Click(Sender: TObject);
begin
  Close;
end;

procedure THintDesigner.SpeedButton1Click(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'open', PChar('http://www.alphaskins.com'), nil, nil, ord(soDefault));
end;

{procedure THintDesigner.ChangeColor(Button: TPanel);
var
  c : TColor;
begin
  ColorDialog1.Color := Button.Color;
  if ColorDialog1.Execute then begin
    c := ColorToRGB(ColorDialog1.Color);
    if (GetRValue(C) + GetGValue(C) + GetBValue(C)) / 3 > 129 then begin
      Button.Font.Color := clBlack;
    end
    else begin
      Button.Font.Color := clWhite;
    end;
    Button.Color := C;
  end;
end;}

procedure THintDesigner.InitControls(Manager: TsHintManager; Sender : TObject);
begin
  if Changing then Exit;
  Changing := True;
  if Manager.Skinned then begin
    TrackBar1.Enabled := False;
    TrackBar2.Enabled := False;
    sTrackBar2.Enabled := False;
    sTrackBar5.Enabled := False;
    sTrackBar9.Enabled := False;
    sTrackBar10.Enabled := False;
    sTrackBar11.Enabled := False;
    sTrackBar12.Enabled := False;
    sBitBtn1.Enabled := False;
    sBitBtn2.Enabled := False;
    BitBtn2.Enabled := False;
    BitBtn3.Enabled := False;
    sColorSelect2.Enabled := False;
    sColorSelect3.Enabled := False;
    sColorSelect4.Enabled := False;
    sCheckBox1.Enabled := False;
  end;
{  if Manager.ShadowMode <> hmCustom then begin
    sColorSelect1.Enabled := False;
    sCheckBox1.Enabled := False;
    sTrackBar9.Enabled := False;
    sTrackBar10.Enabled := False;
    sTrackBar11.Enabled := False;
    GroupBox1.Caption := ' Custom shadow disabled ';
  end;}

  if sComboBox4 <> Sender then sComboBox4.ItemIndex := ord(Manager.Predefinitions);
  sComboBox1.ItemIndex := ord(Manager.HintKind.Style);
  sTrackBar1.Position := Manager.HintKind.BevelWidth;
  sTrackBar2.Position := Manager.HintKind.ExOffset;
  sTrackBar3.Position := Manager.HintKind.MarginH;
  sTrackBar4.Position := Manager.HintKind.MarginV;
  sTrackBar21.Position := Manager.HintKind.MaxWidth;
  sSpinEdit2.Text := IntToStr(Manager.PauseHide);
  sTrackBar5.Position := Manager.HintKind.Radius;

  sColorSelect4.ColorValue := Manager.HintKind.Color;

  sTrackBar12.Position := Manager.HintKind.Transparency;

  sColorSelect2.ColorValue := Manager.HintKind.ColorBorderTop;
  sColorSelect3.ColorValue := Manager.HintKind.ColorBorderBottom;

  sTrackBar9.Position := Manager.HintKind.ShadowBlur;
  sTrackBar10.Position := Manager.HintKind.ShadowOffset;
  sTrackBar11.Position := Manager.HintKind.ShadowTransparency;
  sCheckBox1.Checked := Manager.HintKind.ShadowEnabled;

  TrackBar1.Position := Manager.HintKind.TexturePercent;
  TrackBar2.Position := Manager.HintKind.GradientPercent;

  FontDialog1.Font.Assign(Manager.HintKind.Font);

  PaintBox1.Repaint;
  PaintBox2.Repaint;
  Changing := False;
end;

procedure THintDesigner.LoadPreserved(i: integer);
begin
  sHintManager1.PreDefinitions := asHintsPredefinitions[i];
  InitControls(sHintManager1, nil);
end;

procedure THintDesigner.SetPanelColor(Panel: TPanel; c: TColor);
var
  nc : TColor;
begin
  nc := ColorToRGB(C);
  Panel.Color := nc;
  if (GetRValue(nC) + GetGValue(nC) + GetBValue(nC)) / 3 > 129 then begin
    Panel.Font.Color := clBlack;
  end
  else begin
    Panel.Font.Color := clWhite;
  end;
end;

procedure THintDesigner.SetcustomStyle;
begin
  if Changing then Exit;
  sComboBox4.ItemIndex := 9;
  sHintManager1.Predefinitions := shCustom;
  if sHintManager1.HintKind.Style in [hsNone, hsStandard] then PaintBox1.Visible := False;
  if PaintBox1.Visible = False then PaintBox1.Visible := True;
  PaintBox1.Repaint
end;

procedure THintDesigner.FormShow(Sender: TObject);
begin
  acHintsInEditor := True;
  InitControls(sHintManager1, nil);
//  StartUrgentPainting(Self); !!!
end;

procedure THintDesigner.PaintBox1Paint(Sender: TObject);
var
  FTempHint : TsCustomHintWindow;
  MaskBmp, BodyBmp: TBitmap;
  OffsetX, OffsetY : integer;
  HintRect : TRect;
  SavedDC : hdc;
begin
  if (MainBmp = nil) then PrepareMainBmp;

  Case sHintManager1.HintKind.Style of
    hsBalloon: FTempHint := TsBalloonHintWindow.Create(Self);
    hsComics: FTempHint := TsComicsHintWindow.Create(Self);
    hsEllipse: FTempHint := TsEllipseHintWindow.Create(Self);
    hsSimply: FTempHint := TsSimplyHintWindow.Create(Self)
    else begin
      if Assigned(DefaultManager) and DefaultManager.SkinData.Active
        then BitBlt(PaintBox1.Canvas.Handle, 0, 0, PaintBox1.Width, PaintBox1.Height, Panel4.SkinData.FCacheBmp.Canvas.Handle, 4, 4, SRCCOPY)
        else FillDC(PaintBox1.Canvas.Handle, PaintBox1.ClientRect, ColorToRGB(clBtnFace));//Gray);
      PaintBox1.Canvas.Brush.Style := bsClear;
      PaintBox1.Canvas.TextOut((PaintBox1.Width - PaintBox1.Canvas.TextWidth(LoadStr(s_HintDsgnNoPicture))) div 2,
                               (PaintBox1.Height - PaintBox1.Canvas.TextHeight(LoadStr(s_HintDsgnNoPicture))) div 2, LoadStr(s_HintDsgnNoPicture));
      Exit;
    end;
  end;

  FTempHint.Caption := LoadStr(s_PreviewHint);
  HintRect := FTempHint.CalcHintRect(sHintManager1.HintKind.MaxWidth, FTempHint.Caption, nil);
  FTemphint.Width := WidthOf(HintRect);
  FTemphint.Height := HeightOf(HintRect);
  sHintManager1.FCacheBmp.Width := FTemphint.Width;
  sHintManager1.FCacheBmp.Height := FTemphint.Height;
  if sHintManager1.FCacheBmp = nil then sHintManager1.FCacheBmp := CreateBmp24(FTemphint.Width, FTemphint.Height);
  OffsetX := (MainBmp.Width - FTemphint.Width) div 2;
  OffsetY := (MainBmp.Height - FTemphint.Height) div 2;
  FTemphint.Left := 0;
  FTemphint.Top := 0;
  BitBlt(sHintManager1.FCacheBmp.Canvas.Handle, 0, 0, sHintManager1.FCacheBmp.Width, sHintManager1.FCacheBmp.Height, MainBmp.Canvas.Handle, OffsetX, OffsetY, SRCCOPY);
  if sHintManager1.HintKind.ShadowEnabled then FTempHint.PaintShadow;

  MaskBmp := FTempHint.GetMask;
  if Assigned(MaskBmp) then begin
    SumByMask(sHintManager1.FCacheBmp, MainBmp, MaskBmp, Rect(0, 0, MaskBmp.Width, MaskBmp.Height));
  end;
  BodyBmp := FTempHint.GetBody;
  try
    if Assigned(BodyBmp) and Assigned(MaskBmp) then SumByMask(sHintManager1.FCacheBmp, BodyBmp, MaskBmp, FTempHint.ClientRect);

    SavedDC := SaveDC(PaintBox1.Canvas.Handle);
    ExcludeClipRect(PaintBox1.Canvas.Handle, OffsetX, OffsetY, sHintManager1.FCacheBmp.Width + OffsetX, sHintManager1.FCacheBmp.Height + OffsetY);
    BitBlt(PaintBox1.Canvas.Handle, 0, 0, MainBmp.Width, MainBmp.Height, MainBmp.Canvas.Handle, 0, 0, SRCCOPY);
    RestoreDC(PaintBox1.Canvas.Handle, SavedDC);

    BitBlt(PaintBox1.Canvas.Handle, OffsetX, OffsetY, sHintManager1.FCacheBmp.Width, sHintManager1.FCacheBmp.Height, sHintManager1.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);

  finally
    if Assigned(MaskBmp) then FreeAndNil(MaskBmp);
    if Assigned(BodyBmp) then FreeAndNil(BodyBmp);
    FreeAndNil(FTempHint);
  end;
end;

procedure THintDesigner.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(MainBmp);
  acHintsInEditor := False
end;

procedure THintDesigner.TrackBar1Change(Sender: TObject);
begin
  if TrackBar2.Position > 100 - TrackBar1.Position then TrackBar2.Position := 100 - TrackBar1.Position;
  sHintManager1.HintKind.TexturePercent := TrackBar1.Position;
  if sHintManager1.HintKind.TexturePercent = 0 then sHintManager1.HintKind.Texture.Assign(nil);
  SetcustomStyle
end;

procedure THintDesigner.TrackBar2Change(Sender: TObject);
begin
  if TrackBar1.Position > 100 - TrackBar2.Position then TrackBar1.Position := 100 - TrackBar2.Position;
  sHintManager1.HintKind.GradientPercent := TrackBar2.Position;
  SetCustomStyle
end;

procedure THintDesigner.PaintBox2Paint(Sender: TObject);
const
  np = 'No gradient available';
var
  Bmp : TBitmap;
begin
  if sHintManager1.HintKind.GradientData <> '' then begin
    Bmp := TBitmap.Create;
    Bmp.Width := PaintBox2.Width;
    Bmp.Height := PaintBox2.Height;
    Bmp.PixelFormat := pf24bit;
    sGradient.PaintGrad(Bmp, PaintBox2.ClientRect, sHintManager1.HintKind.GradientData);
    BitBlt(PaintBox2.Canvas.Handle, 0, 0, PaintBox2.Width, PaintBox2.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    FreeAndNil(Bmp);
  end
  else begin
    FillDC(PaintBox2.Canvas.Handle, PaintBox2.ClientRect, ColorToRGB(clBtnFace));
    PaintBox2.Canvas.Brush.Color := ColorToRGB(clBtnFace);
    PaintBox2.Canvas.TextOut((PaintBox2.Width - PaintBox2.Canvas.TextWidth(np)) div 2,
                               (PaintBox2.Height - PaintBox2.Canvas.TextHeight(np)) div 2, np);
  end;
end;

procedure THintDesigner.PaintBox2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  GradArray : TsGradArray;
begin
  if not Manager.Skinned then begin
    sGradBuilder.CreateEditorForm;
    PrepareGradArray(sHintManager1.HintKind.GradientData, GradArray);
    GradBuilder.LoadFromArray(GradArray);
    GradBuilder.ShowModal;
    case GradBuilder.ModalResult of
      mrOk : begin
        sHintManager1.HintKind.GradientData := GradBuilder.AsString;
        SetCustomStyle;
      end;
      mrNone : begin
        sHintManager1.HintKind.GradientData := '';
        SetCustomStyle;
      end
    end;
    sGradBuilder.KillForm;
    PaintBox2.Repaint;
  end;
end;

procedure THintDesigner.BitBtn3Click(Sender: TObject);
var
  iFile : TMemIniFile;
begin
  if SaveDialog1.Execute then begin
    iFile := TMemIniFile.Create(SaveDialog1.FileName);
    sStoreUtils.WriteIniStr(sSection, 'Style', IntToStr(ord(sHintManager1.HintKind.Style)), iFile);
    sStoreUtils.WriteIniStr(sSection, 'BevelWidth', IntToStr(sHintManager1.HintKind.BevelWidth), iFile);
    sStoreUtils.WriteIniStr(sSection, 'ArrowLength', IntToStr(sHintManager1.HintKind.ExOffset), iFile);
    sStoreUtils.WriteIniStr(sSection, 'MarginH', IntToStr(sHintManager1.HintKind.MarginH), iFile);
    sStoreUtils.WriteIniStr(sSection, 'MarginV', IntToStr(sHintManager1.HintKind.MarginV), iFile);
    sStoreUtils.WriteIniStr(sSection, 'CornRadius', IntToStr(sHintManager1.HintKind.Radius), iFile);
    sStoreUtils.WriteIniStr(sSection, 'MaxWidth', IntToStr(sHintManager1.HintKind.MaxWidth), iFile);
    sStoreUtils.WriteIniStr(sSection, 'PauseHide', IntToStr(sHintManager1.PauseHide), iFile);
    sStoreUtils.WriteIniStr(sSection, 'BorderTop', IntToStr(sHintManager1.HintKind.ColorBorderTop), iFile);
    sStoreUtils.WriteIniStr(sSection, 'BorderBottom', IntToStr(sHintManager1.HintKind.ColorBorderBottom), iFile);

    sStoreUtils.WriteIniStr(sSection, 'ShadowEnabled', IntToStr(integer(sHintManager1.HintKind.ShadowEnabled)), iFile);
    sStoreUtils.WriteIniStr(sSection, 'ShadowColor', IntToStr(sHintManager1.HintKind.ShadowColor), iFile);
    sStoreUtils.WriteIniStr(sSection, 'ShadowBlur', IntToStr(sHintManager1.HintKind.ShadowBlur), iFile);
    sStoreUtils.WriteIniStr(sSection, 'ShadowOffset', IntToStr(sHintManager1.HintKind.ShadowOffset), iFile);
    sStoreUtils.WriteIniStr(sSection, 'ShadowTransparency', IntToStr(sHintManager1.HintKind.ShadowTransparency), iFile);

    sStoreUtils.WriteIniStr(sSection, 'Color', IntToStr(sHintManager1.HintKind.Color), iFile);
    sStoreUtils.WriteIniStr(sSection, 'Transparency', IntToStr(sHintManager1.HintKind.Transparency), iFile);
    sStoreUtils.WriteIniStr(sSection, 'Texture', sHintManager1.HintKind.TextureFile, iFile);
    sStoreUtils.WriteIniStr(sSection, 'TexturePercent', IntToStr(sHintManager1.HintKind.TexturePercent), iFile);
    sStoreUtils.WriteIniStr(sSection, 'GradientPercent', IntToStr(sHintManager1.HintKind.GradientPercent), iFile);
    sStoreUtils.WriteIniStr(sSection, 'GradientData', sHintManager1.HintKind.GradientData, iFile);

    sStoreUtils.WriteIniFont(sSection, 'Font', sHintManager1.HintKind.Font, iFile);

    iFile.UpdateFile;
  end;
end;

procedure THintDesigner.BitBtn2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then LoadFromFile(OpenDialog1.FileName)
end;

procedure THintDesigner.sColorSelect2Change(Sender: TObject);
begin
  sHintManager1.HintKind.ColorBorderTop := sColorSelect2.ColorValue;
  SetcustomStyle
end;

procedure THintDesigner.sColorSelect3Change(Sender: TObject);
begin
  sHintManager1.HintKind.ColorBorderBottom := sColorSelect3.ColorValue;
  SetcustomStyle
end;

procedure THintDesigner.sColorSelect4Change(Sender: TObject);
begin
  sHintManager1.HintKind.Color := sColorSelect4.ColorValue;
  SetcustomStyle
end;

procedure THintDesigner.PrepareMainBmp;
const
  s = 'preview ';
var
  x, y, w, h, wd2 : integer;
  b : boolean;
begin
  if Panel4.SkinData.Updating and Panel4.SkinData.Skinned then Exit;
  // Prepare of the PaintBox BG
  MainBmp := CreateBmp24(PaintBox1.Width, PaintBox1.Height);
  MainBmp.Canvas.Brush.Color := clSilver;
  MainBmp.Canvas.Font.Name := 'Small fonts';
  MainBmp.Canvas.Font.Size := 7;
  if Panel4.SkinData.Skinned
    then BitBlt(MainBmp.Canvas.Handle, 0, 0, PaintBox1.Width, PaintBox1.Height, Panel4.SkinData.FCacheBmp.Canvas.Handle, 4, 4, SRCCOPY)
    else FillDC(MainBmp.Canvas.Handle, PaintBox1.ClientRect, ColorToRGB(clBtnFace));
  MainBmp.Canvas.Brush.Style := bsClear;
  w := MainBmp.Canvas.TextWidth(s);
  wd2 := w div 2;
  h := MainBmp.Canvas.TextHeight(s);
  b := False;
  for y := 0 to 30 do begin
    b := not b;
    for x := 0 to 30 do MainBmp.Canvas.TextOut(x * w - integer(b) * wd2, y * h, s);
  end;
end;

procedure THintDesigner.FormCreate(Sender: TObject);
begin
  PaintBox1.ControlStyle := PaintBox1.ControlStyle + [csOpaque] - [csReplicatable];
end;

procedure THintDesigner.WndProc(var Message: TMessage);
begin
  inherited;
  if (Message.Msg = cardinal(SM_ALPHACMD)) and (Message.WParamHi = AC_REFRESH) then PrepareMainBmp;
end;

procedure THintDesigner.sBitBtn1Click(Sender: TObject);
begin
  if FontDialog1.Execute then begin
    sHintManager1.HintKind.Font.Assign(FontDialog1.Font);
    SetcustomStyle
  end;
end;

procedure THintDesigner.sBitBtn2Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then begin
    sHintManager1.HintKind.TextureFile := OpenPictureDialog1.FileName;
    sGraphUtils.LoadJpegOrBmp(sHintManager1.HintKind.Texture, OpenPictureDialog1.FileName, False);
    if TrackBar1.Position = 0 then TrackBar1.Position := 1;
    SetcustomStyle
  end;
end;

procedure THintDesigner.LoadFromFile(const FileName: string);
var
  iFile : TMemIniFile;
begin
  if (pos('*', FileName) > 0) or not FileExists(FileName) then Exit;
  iFile := TMemIniFile.Create(FileName);
  sHintManager1.HintKind.Style := aHintStyles[sStoreUtils.ReadIniInteger(sSection, 'Style', 0, iFile)];
  sHintManager1.HintKind.BevelWidth := sStoreUtils.ReadIniInteger(sSection, 'BevelWidth', 1, iFile);
  sHintManager1.HintKind.ExOffset := sStoreUtils.ReadIniInteger(sSection, 'ArrowLength', 0, iFile);

  sHintManager1.HintKind.MarginH := sStoreUtils.ReadIniInteger(sSection, 'MarginH', 4, iFile);
  sHintManager1.HintKind.MarginV := sStoreUtils.ReadIniInteger(sSection, 'MarginV', 4, iFile);
  sHintManager1.HintKind.Radius := sStoreUtils.ReadIniInteger(sSection, 'CornRadius', 1, iFile);
  sHintManager1.HintKind.MaxWidth := sStoreUtils.ReadIniInteger(sSection, 'MaxWidth', 200, iFile);
  sHintManager1.PauseHide := sStoreUtils.ReadIniInteger(sSection, 'PauseHide', 5000, iFile);
  sHintManager1.HintKind.ColorBorderTop := sStoreUtils.ReadIniInteger(sSection, 'BorderTop', 1, iFile);
  sHintManager1.HintKind.ColorBorderBottom := sStoreUtils.ReadIniInteger(sSection, 'BorderBottom', 1, iFile);

  sHintManager1.HintKind.ShadowEnabled := boolean(sStoreUtils.ReadIniInteger(sSection, 'ShadowEnabled', 0, iFile));
  sHintManager1.HintKind.ShadowBlur := sStoreUtils.ReadIniInteger(sSection, 'ShadowBlur', 4, iFile);
  sHintManager1.HintKind.ShadowOffset := sStoreUtils.ReadIniInteger(sSection, 'ShadowOffset', 6, iFile);
  sHintManager1.HintKind.ShadowTransparency := sStoreUtils.ReadIniInteger(sSection, 'ShadowTransparency', 50, iFile);

  sHintManager1.HintKind.Color := sStoreUtils.ReadIniInteger(sSection, 'Color', ColorToRGB(clWhite), iFile);
  sHintManager1.HintKind.Transparency := sStoreUtils.ReadIniInteger(sSection, 'Transparency', 0, iFile);
  sHintManager1.HintKind.TextureFile := sStoreUtils.ReadIniString(sSection, 'TextureFile', iFile);
  if FileExists(sHintManager1.HintKind.TextureFile) then sHintManager1.HintKind.Texture.LoadFromFile(sHintManager1.HintKind.TextureFile);
  sHintManager1.HintKind.TexturePercent := sStoreUtils.ReadIniInteger(sSection, 'TexturePercent', 0, iFile);
//  sHintManager1.HintKind.GradientPercent := sStoreUtils.ReadIniInteger(sSection, 'GradientPercent', 0, iFile);
  sHintManager1.HintKind.GradientData := sStoreUtils.ReadIniString(sSection, 'GradientData', iFile);

  sStoreUtils.ReadIniFont(sSection, 'Font', sHintManager1.HintKind.Font, iFile);
  SetcustomStyle;

  InitControls(sHintManager1, nil);
end;

procedure THintDesigner.OpenDialog1SelectionChange(Sender: TObject);
begin
  LoadFromFile(OpenDialog1.FileName);
end;

procedure THintDesigner.InitLngCaptions;
begin
  Caption         := LoadStr(s_HintDsgnTitle);
  Label1.Caption  := LoadStr(s_HintDsgnPreserved);
  Label2.Caption  := LoadStr(s_HintDsgnStyle);
  Label3.Caption  := LoadStr(s_HintDsgnBevelWidth);
//  Label4.Caption  := LoadStr(s_Blur);
  Label5.Caption  := LoadStr(s_HintDsgnArrowLength);
  Label6.Caption  := LoadStr(s_HintDsgnHorizMargin);
  Label7.Caption  := LoadStr(s_HintDsgnVertMargin);
  Label8.Caption  := LoadStr(s_HintDsgnRadius);
  Label9.Caption  := LoadStr(s_HintDsgnMaxWidth);
  Label10.Caption := LoadStr(s_HintDsgnPauseHide);
  Label11.Caption := LoadStr(s_Percent);
  Label12.Caption := LoadStr(s_Blur);
  Label13.Caption := LoadStr(s_HintDsgnOffset);
  Label14.Caption := LoadStr(s_HintDsgnTransparency);
  Label15.Caption := LoadStr(s_Percent);
  Label17.Caption := LoadStr(s_HintDsgnTransparency);

//  Panel4.Caption  := LoadStr(s_HintDsgnNoPicture);

  sBitBtn1.Caption      := LoadStr(s_Font);
  sBitBtn2.Caption      := LoadStr(s_Texture);
  BitBtn1.Caption       := LoadStr(s_CloseStr);
  BitBtn2.Caption       := LoadStr(s_HintDsgnLoad);
  BitBtn3.Caption       := LoadStr(s_HintDsgnSave);
  sColorSelect2.Caption := LoadStr(s_HintDsgnBorderTop);
  sColorSelect3.Caption := LoadStr(s_HintDsgnBorderBottom);
  sColorSelect4.Caption := LoadStr(s_HintDsgnColor);
  GroupBox1.Caption     := LoadStr(s_Shadow);
  GroupBox2.Caption     := LoadStr(s_Background);
  GroupBox3.Caption     := LoadStr(s_Gradient);

end;

end.
