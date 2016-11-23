unit sColorDialog;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  sSkinProvider, StdCtrls, Buttons, sBitBtn, ExtCtrls, sPanel, Mask,
  sMaskEdit, sCurrencyEdit, sLabel, sConst, sSpeedButton, sGraphUtils,
  sCustomComboEdit, sCurrEdit;

type
  TColorArray = array of TColor;

  TsColorDialogForm = class(TForm)
    sSkinProvider1: TsSkinProvider;
    sBitBtn1: TsBitBtn;
    sBitBtn2: TsBitBtn;
    ColorPanel: TsPanel;
    GradPanel: TsPanel;
    SelectedPanel: TsPanel;
    sREdit: TsCurrencyEdit;
    sGEdit: TsCurrencyEdit;
    sBEdit: TsCurrencyEdit;
    sBitBtn3: TsBitBtn;
    sBitBtn4: TsBitBtn;
    sLabel1: TsLabel;
    sLabel2: TsLabel;
    sHEdit: TsCurrencyEdit;
    sSEdit: TsCurrencyEdit;
    sVEdit: TsCurrencyEdit;
    MainPal: TsColorsPanel;
    AddPal: TsColorsPanel;
    sEditDecimal: TsCurrencyEdit;
    sEditHex: TsMaskEdit;
    sLabel4: TsLabel;
    sLabel5: TsLabel;
    sLabel6: TsLabel;
    sBitBtn5: TsBitBtn;
    sSpeedButton1: TsSpeedButton;
    procedure sBitBtn2Click(Sender: TObject);
    procedure sBitBtn1Click(Sender: TObject);
    procedure CreateExtBmp;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure ColorPanelPaint(Sender: TObject; Canvas: TCanvas);
    procedure ColorPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ColorPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ColorPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    procedure GradPanelPaint(Sender: TObject; Canvas: TCanvas);
    procedure GradPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GradPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GradPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    procedure MainPalChange(Sender: TObject);
    procedure sEditHexKeyPress(Sender: TObject; var Key: Char);
    function GetColorCoord(C : TsColor) : integer;
    procedure FormPaint(Sender: TObject);
    procedure sEditHexChange(Sender: TObject);
    procedure sEditDecimalChange(Sender: TObject);
    procedure sHEditChange(Sender: TObject);
    procedure sREditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sBitBtn4Click(Sender: TObject);
    procedure sBitBtn5Click(Sender: TObject);
    procedure sBitBtn3Click(Sender: TObject);
    procedure sSpeedButton1Click(Sender: TObject);
    procedure PickFormPaint(Sender: TObject);
    procedure PickFormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PickFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PickFormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MainPalDblClick(Sender: TObject);
    procedure AddPalDblClick(Sender: TObject);
  public
    TmpBmp : TBitmap;
    ExtBmp : TBitmap;
    TopColors : TColorArray;
    Owner : TColorDialog;
    PickPanel : TPanel;
    procedure SetMarker;
    procedure PaintMarker(mX, mY : integer);
    procedure PaintCursor(mX, mY : integer; Canvas : TCanvas);
    function GradToColor(Coord : integer) : TColor;
    procedure SetCurrentColor(c : TColor);
    procedure SetInternalColor(h : integer; s : real);
    procedure SetColorFromEdit(Color : TColor; var Flag : boolean);
    procedure ExitPanels;
    procedure InitLngCaptions;
    function MarkerRect : TRect;
  end;

const
  bWidth = 0;
  dblWidth = bWidth * 0;
  ArrowSize = 5;

var
  sColorDialogForm: TsColorDialogForm;
  InternalColor, PickColor : TColor;
  SelectedHsv : TsHSV;
  ColorCoord : TPoint;
  ExPressed, GradPressed : boolean;
  j, GradY, CurrCustomIndex : integer;
  UseCoords : boolean;
  ColorChanging, HexChanging, DecChanging, HsvChanging, RgbChanging : boolean;
  b : TBitmap;

implementation

uses math, sCommonData, acntUtils, sDialogs, sStrings;

{$R *.DFM}

procedure Marker(DC : hdc; Pos : TPoint);
var
  x, y : integer;
  C : TColor;
begin
  if (sColorDialogForm <> nil) and sColorDialogForm.sSkinProvider1.SkinData.Skinned then begin
    C := sColorDialogForm.sSkinProvider1.SkinData.SkinManager.GetGlobalFontColor
  end
  else C := clBlack;//clBtnText;

  for y := 0 to ArrowSize do for x := 0 to ArrowSize do begin
    if x > y then SetPixel(DC, Pos.X + X, Pos.Y + Y, C);
    if x > ArrowSize - y then SetPixel(DC, Pos.X + X, Pos.Y + Y - ArrowSize, C);
  end;
end;

procedure SkinMarker(Form : TsColorDialogForm);
var
  CI : TCacheInfo;
begin
  with Form do if not sSkinProvider1.SkinData.BGChanged then begin
    CI := GetParentCache(sBitBtn1.SkinData);
    if (TmpBmp = nil) then begin
      TmpBmp := CreateBmp24(10, GradPanel.Height + ArrowSize * 2);
      BitBlt(TmpBmp.Canvas.Handle, 0, 0, TmpBmp.Width, TmpBmp.Height, CI.Bmp.Canvas.Handle,
               GradPanel.Left + GradPanel.Width + CI.X, GradPanel.Top - ArrowSize + CI.Y, SRCCOPY);
    end;
    BitBlt(CI.Bmp.Canvas.Handle, GradPanel.Left + GradPanel.Width + CI.X, GradPanel.Top - ArrowSize + CI.Y, 20, TmpBmp.Height, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);
    Marker(CI.Bmp.Canvas.Handle, Point(GradPanel.Left + GradPanel.Width + CI.X, GradPanel.Top - ArrowSize + CI.Y + GradY + ArrowSize));
  end;
end;

procedure RepaintRect(Form : TsColorDialogForm);
var
  UR : TRect;
begin
  if (Form.sSkinProvider1.SkinData.SkinManager = nil) or not Form.sSkinProvider1.SkinData.SkinManager.Active then begin
    UR := Form.MarkerRect;
    RedrawWindow(Form.Handle, @UR, 0, RDW_INVALIDATE or RDW_ERASE or RDW_UPDATENOW);
  end;
end;

procedure TsColorDialogForm.sBitBtn2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TsColorDialogForm.sBitBtn1Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TsColorDialogForm.ColorPanelPaint(Sender: TObject; Canvas: TCanvas);
begin
  BitBlt(Canvas.Handle, 0, 0, ExtBmp.Width, ExtBmp.Height, ExtBmp.Canvas.Handle, 0, 0, SRCCOPY);
  if ExPressed then exit;
  if UseCoords
    then PaintCursor(ColorCoord.x, ColorCoord.y, Canvas)
    else PaintCursor(SelectedHsv.h * ColorPanel.Width div 360, Round((1 - SelectedHsv.s) * ColorPanel.Height), Canvas)
end;

procedure TsColorDialogForm.CreateExtBmp;
var
  x, y : integer;
  ImgWidth, ImgHeight : integer;
begin
  ImgWidth := ColorPanel.Width - dblWidth;
  ImgHeight := ColorPanel.Height - dblWidth;

  ExtBmp := CreateBmp24(ImgWidth, ImgHeight);

  for y := 0 to ImgHeight - 1 do
    for x := 0 to ImgWidth - 1 do ExtBmp.Canvas.Pixels[x, y] := Hsv2Rgb(x * 360 / ImgWidth, 1 - y / ImgHeight, 1 - y / (ImgHeight * 3)).C;
end;

procedure TsColorDialogForm.FormShow(Sender: TObject);
begin
  SetCurrentColor(ColorToRGB(TsColorDialog(Owner).Color));
  TsColorDialog(Owner).DoShow
end;

procedure TsColorDialogForm.ColorPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R : TRect;
  P : TPoint;
begin
  ColorPanel.SetFocus;
  ExPressed := True;
  SetInternalColor(Round(x * 360 / ColorPanel.Width), 1 - y / ColorPanel.Height);
  ColorPanel.SkinData.BGChanged := True;
  ColorPanel.Repaint;

  P := ColorPanel.ClientToScreen(Point(0, 0));
  R := Rect(0, 0, ColorPanel.Width, ColorPanel.Height);
  OffsetRect(R, P.x, P.y);
  ClipCursor(@R);
end;

procedure TsColorDialogForm.GradPanelPaint(Sender: TObject; Canvas: TCanvas);
var
  x, y : integer;
  RStep, GStep, BStep : real;
  R, G, B : real;
  c : TsColor;
  ImgWidth, ImgHeight : integer;
begin
  c.C := InternalColor;
  R := 255; G := 255; B := 255;
  ImgWidth := GradPanel.Width - dblWidth;
  ImgHeight := GradPanel.Height - dblWidth;
  y := 0;
  RStep := (255 - c.R) / (ImgHeight div 2); GStep := (255 - c.G) / (ImgHeight div 2); BStep := (255 - c.B) / (ImgHeight div 2);
  while y < (ImgHeight - 1) div 2 do begin
    R := R - RStep; G := G - GStep; B := B - BStep;
    c.R := max(min(round(R), 255), 0); c.G := max(min(round(G), 255), 0); c.B := max(min(round(B), 255), 0);
    for x := 0 to ImgWidth - 1 do Canvas.Pixels[x, y] := c.C;
    inc(y)
  end;
  RStep := c.R / (ImgHeight div 2); GStep := c.G / (ImgHeight div 2); BStep := c.B / (ImgHeight div 2);
  while y < ImgHeight do begin
    R := R - RStep; G := G - GStep; B := B - BStep;
    c.R := max(min(round(R), 255), 0); c.G := max(min(round(G), 255), 0); c.B := max(min(round(B), 255), 0);
    for x := 0 to ImgWidth - 1 do Canvas.Pixels[x, y] := c.C;
    inc(y)
  end;
end;

procedure TsColorDialogForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(TmpBmp);
  FreeAndNil(ExtBmp);
end;

procedure TsColorDialogForm.ColorPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if not ExPressed then Exit;
  SetInternalColor(Round(x * 360 / ColorPanel.Width), 1 - y / ColorPanel.Height);
end;

procedure TsColorDialogForm.GradPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R : TRect;
  P : TPoint;
  UR : TRect;
begin
  GradPanel.SetFocus;
  GradPressed := True;
  GradY := Y;
  UseCoords := True;
  SetCurrentColor(GradToColor(y));
  UseCoords := False;

  P := GradPanel.ClientToScreen(Point(0, 0));
  R := Rect(0, 0, 8, GradPanel.Height - 1);
  OffsetRect(R, P.x + GradPanel.Width, P.y);
  UR := MarkerRect;
  RedrawWindow(Handle, @UR, 0, RDW_INVALIDATE or RDW_ERASE or RDW_UPDATENOW);
  ClipCursor(@R);
end;

procedure TsColorDialogForm.SetMarker;
var
  CI : TCacheInfo;
  c : TsColor;
begin
  GradPanel.SkinData.BGChanged := True;
  GradPanel.Repaint;
  CI := GetParentCache(sBitBtn1.SkinData);
  if CI.Ready then begin
    BitBlt(Canvas.Handle, GradPanel.Left + GradPanel.Width, GradPanel.Top - 5, 20, GradPanel.Height + 10, sSkinProvider1.SkinData.FCacheBmp.Canvas.Handle, GradPanel.Left + GradPanel.Width + CI.X, GradPanel.Top + CI.Y - 5, SRCCOPY);
  end
  else FillDC(Canvas.Handle, Rect(GradPanel.Left + GradPanel.Width, GradPanel.Top - 5, GradPanel.Left + GradPanel.Width + 20, GradPanel.Top + GradPanel.Height + 5), CI.FillColor);
  c.C := SelectedPanel.Color;
  GradY := GetColorCoord(c);
  PaintMarker(GradPanel.Left + GradPanel.Width, GradPanel.top + GradY);
end;

procedure TsColorDialogForm.PaintMarker(mX, mY: integer);
const
  ArrowSize = 5;
var
  CI : TCacheInfo;
begin
  CI := GetParentCache(sBitBtn1.SkinData);

  if sSkinProvider1.SkinData.Skinned and CI.Ready then begin
    SkinMarker(Self);
  end
  else begin
    Marker(Canvas.Handle, Point(mX, mY));
  end;
end;

procedure TsColorDialogForm.GradPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ClipCursor(nil);
  GradPressed := False;
  RepaintRect(Self);
end;

procedure TsColorDialogForm.GradPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if not GradPressed then Exit;
  RepaintRect(Self);
  GradY := Y;
  UseCoords := True;
  SetCurrentColor(GradToColor(y));
  UseCoords := False;
end;

procedure TsColorDialogForm.ColorPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ClipCursor(nil);
  ExPressed := False;
  ColorPanel.SkinData.BGChanged := True;
  ColorPanel.Repaint;
end;

function TsColorDialogForm.GradToColor(Coord: integer): TColor;
var
  y : integer;
  RStep, GStep, BStep : real;
  R, G, B, id2 : real;
  c : TsColor;
  ImgHeight : integer;
begin
  Result := clWhite;
  c.C := InternalColor;
  R := 255; G := 255; B := 255;
  ImgHeight := GradPanel.Height - dblWidth;
  id2 := ImgHeight / 2;

  RStep := (255 - c.R) / id2; GStep := (255 - c.G) / id2; BStep := (255 - c.B) / id2;

  y := 0;
  while y < (ImgHeight - 1) div 2 do begin
    R := R - RStep; G := G - GStep; B := B - BStep;
    c.R := round(R); c.G := round(G); c.B := round(B);
    if y = Coord then begin
      if c.R < 3 then c.R := 0; if c.G < 3 then c.G := 0; if c.B < 3 then c.B := 0;
      if c.R > 253 then c.R := 255; if c.G > 253 then c.G := 255; if c.B > 253 then c.B := 255;
      Result := c.C;
      Exit;
    end;
    inc(y)
  end;

  RStep := c.R / id2; GStep := c.G / id2; BStep := c.B / id2;
  while y < ImgHeight - 1 do begin
    R := R - RStep; G := G - GStep; B := B - BStep;
    c.R := round(R); c.G := round(G); c.B := round(B);
    if y = Coord then begin
      Result := c.C;
      Exit;
    end;
    inc(y)
  end;
end;

procedure TsColorDialogForm.SetCurrentColor(c: TColor);
var
  sColor : TsColor;
  TempValue : real;
begin
  ColorChanging := True;
  SelectedPanel.Color := ColorToRGB(c);
//  SelectedPanel.SkinData.Invalidate;
  sColor.C := c;
  if not UseCoords then begin
    SelectedHsv := Rgb2Hsv(sColor);
    InternalColor := max(Hsv2Rgb(SelectedHsv.h, SelectedHsv.s, 1).C, 0);
  end;
  if not HsvChanging then begin
    sHEdit.Value := Round(SelectedHsv.H);
    if not UseCoords then begin
      sSEdit.Value := SelectedHsv.S * 100;
      sVEdit.Value := SelectedHsv.V * 100
    end
    else begin
      if GradY < GradPanel.Height div 2 then begin
        TempValue := SelectedHsv.S * 100;
        sSEdit.Value := (TempValue * 2) / GradPanel.Height * (GradY + 1);

      end else sVEdit.Value := (1 - abs(GradPanel.Height div 2 - GradY) / (GradPanel.Height div 2)) * 100;
    end;
  end;

  if not RgbChanging then begin
    sREdit.Value := sColor.R;
    sGEdit.Value := sColor.G;
    sBEdit.Value := sColor.B;
  end;

  if not DecChanging then sEditDecimal.Text := IntToStr(sColor.C);
  if not HexChanging then sEditHex.Text := IntToHex(SwapColor(sColor.C), 6);

  SelectedPanel.Color := sColor.C;
//  SelectedPanel.SkinData.Invalidate;

  if not UseCoords then ColorCoord := Point(SelectedHsv.h * ColorPanel.Width div 360, Round((1 - SelectedHsv.s) * ColorPanel.Height));
  SetMarker;
  RepaintRect(Self);
  ExitPanels;
  ColorChanging := False;
end;

procedure TsColorDialogForm.PaintCursor(mX, mY: integer; Canvas: TCanvas);
begin
  Canvas.Ellipse(mX - 5, mY - 5, mX + 5, mY + 5);
end;

procedure TsColorDialogForm.MainPalChange(Sender: TObject);
begin
  if (TsColorsPanel(Sender).ItemIndex <> -1) and not ((Sender = AddPal) and (AddPal.Colors[AddPal.ItemIndex]='FFFFFF')) then begin
    SetCurrentColor(TsColorsPanel(Sender).ColorValue);
    ColorPanel.SkinData.BGChanged := True;
    ColorPanel.Repaint;
  end;
end;

procedure TsColorDialogForm.sEditHexKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key in ['0'..'9', 'A'..'F', 'a'..'f', Chr(27), Chr(13)]) then inherited else Key := #0;
end;

function TsColorDialogForm.GetColorCoord(C: TsColor): integer;
var
  y : integer;
  RStep, GStep, BStep : real;
  R, G, B : real;
  ImgHeight : integer;
begin
  R := 255; G := 255; B := 255;
  ImgHeight := GradPanel.Height - dblWidth;
  InternalColor := Max(0, InternalColor);
  RStep := (255 - GetRValue(InternalColor)) / (ImgHeight div 2); GStep := (255 - GetGValue(InternalColor)) / (ImgHeight div 2); BStep := (255 - GetBValue(InternalColor)) / (ImgHeight div 2);

  y := 0;
  Result := 0;
  while y < (ImgHeight - 1) div 2 do begin
    R := R - RStep; G := G - GStep; B := B - BStep;
    if (abs(c.R - R) < 2) and (abs(c.G - G) < 2) and (abs(c.B - B) < 2) then begin
      Result := y;
      Exit;
    end;
    inc(y)
  end;

  RStep := GetRValue(InternalColor) / (ImgHeight div 2); GStep := GetGValue(InternalColor) / (ImgHeight div 2); BStep := GetBValue(InternalColor) / (ImgHeight div 2);
  while y < ImgHeight do begin
    R := R - RStep; G := G - GStep; B := B - BStep;
    if (abs(c.R - R) < 2) and (abs(c.G - G) < 2) and (abs(c.B - B) < 2) then begin
      Result := y;
      Exit;
    end;
    inc(y)
  end;
end;

procedure TsColorDialogForm.SetInternalColor(h : integer; s: real);
var
  sColor, sC : TsColor;
begin
  ColorChanging := True;
  SelectedHsv.h := h;
  SelectedHsv.s := s;
  sColor := Hsv2Rgb(h, s, 1);
  InternalColor := max(sColor.C, 0);

  sHEdit.Value := Round(SelectedHsv.H);
  sSEdit.Value := SelectedHsv.S * 100;
  if GradY < GradPanel.Height div 2
    then sVEdit.Value := 100
    else sVEdit.Value := (1 - abs(GradPanel.Height div 2 - GradY) / (GradPanel.Height div 2)) * 100;

  GradPanel.SkinData.BGChanged := True;
  GradPanel.Repaint;
  sC.C := GradToColor(GradY);

  sREdit.Value := sC.R;
  sGEdit.Value := sC.G;
  sBEdit.Value := sC.B;

  sEditDecimal.Text := IntToStr(sC.C);
  sEditHex.Text := IntToHex(SwapColor(sC.C), 6);

  SelectedPanel.Color := sC.C;
//  SelectedPanel.SkinData.Invalidate;

  ColorCoord := Point(SelectedHsv.h * ColorPanel.Width div 360, Round((1 - SelectedHsv.s) * ColorPanel.Height));

  ExitPanels;
  ColorChanging := False;
end;

procedure TsColorDialogForm.FormPaint(Sender: TObject);
var
  CI : TCacheInfo;
begin
  CI := GetParentCache(sBitBtn1.SkinData);
  if sSkinProvider1.SkinData.Skinned and CI.Ready then begin
    SkinMarker(Self);
    Marker(Canvas.Handle, Point(GradPanel.Left + GradPanel.Width, GradPanel.Top + GradY));
  end
  else begin
    Marker(Canvas.Handle, Point(GradPanel.Left + GradPanel.Width, GradPanel.Top + GradY));
  end;
end;

procedure TsColorDialogForm.sEditHexChange(Sender: TObject);
begin
  SetColorFromEdit(SwapColor(HexToInt(ExtractWord(1, sEditHex.Text, [' ']))), HexChanging);
end;

procedure TsColorDialogForm.sEditDecimalChange(Sender: TObject);
begin
  SetColorFromEdit(sEditDecimal.AsInteger, DecChanging);
end;

procedure TsColorDialogForm.sHEditChange(Sender: TObject);
begin
  SetColorFromEdit(Hsv2Rgb(sHEdit.asInteger, sSEdit.Value / 100, sVEdit.Value / 100).C, HsvChanging);
end;

procedure TsColorDialogForm.SetColorFromEdit(Color: TColor; var Flag: boolean);
begin
  if not ColorChanging then begin
    Flag := True;
    SetCurrentColor(Color);
    MainPal.ItemIndex := -1;
//    ColorPanel.Perform(WM_PAINT, 0, 0);
    ColorPanel.SkinData.BGChanged := True;
    ColorPanel.Repaint;
    Flag := False;
  end;
end;

procedure TsColorDialogForm.sREditChange(Sender: TObject);
var c : TsColor; begin
  c.R := sREdit.AsInteger; c.G := sGEdit.AsInteger; c.B := sBEdit.AsInteger;
  SetColorFromEdit(c.C, RgbChanging);
end;

procedure TsColorDialogForm.FormCreate(Sender: TObject);
begin
  CurrCustomIndex := 0;
  CreateExtBmp;
end;

procedure TsColorDialogForm.sBitBtn4Click(Sender: TObject);
begin
  Width := Constraints.MaxWidth;
  sBitBtn4.Enabled := False;
end;

procedure TsColorDialogForm.sBitBtn5Click(Sender: TObject);
begin
  Application.HelpContext(Owner.Helpcontext);
end;

procedure TsColorDialogForm.sBitBtn3Click(Sender: TObject);
begin
  if AddPal.ItemIndex <> -1 then AddPal.Colors[AddPal.ItemIndex] := sEditHex.Text else begin
    AddPal.Colors[CurrCustomIndex] := sEditHex.Text;
    if CurrCustomIndex < AddPal.Colors.Count - 1 then inc(CurrCustomIndex)
  end;
  AddPal.GenerateColors;
  AddPal.SkinData.Invalidate;
end;

procedure TsColorDialogForm.ExitPanels;
begin
  if MainPal.ItemIndex <> - 1 then if MainPal.ColorValue <> SelectedPanel.Color then MainPal.ItemIndex := -1;
  if AddPal.ItemIndex <> - 1 then if AddPal.ColorValue <> SelectedPanel.Color then begin
    CurrCustomIndex := AddPal.ItemIndex;
    AddPal.ItemIndex := -1;
  end;
end;

procedure TsColorDialogForm.sSpeedButton1Click(Sender: TObject);
var
  DC, SavedDC : hdc;
  PickForm : TForm;
  p : TPoint;
begin
  b := TBitmap.Create;
//  b.Width := Screen.Width;
//  b.Height := Screen.Height;
  b.Width := Monitor.Width;
  b.Height := Monitor.Height;
  DC := GetDC(0);
  SavedDC := SaveDC(DC);
  try
    BitBlt(b.Canvas.Handle, 0, 0, b.width, b.height, dc, Monitor.Left, Monitor.Top, SRCCOPY);
    PickForm := TForm.Create(Application);
    PickForm.Tag := ExceptTag;
    PickForm.Visible := False;
    PickForm.DoubleBuffered := True;
    PickPanel := TPanel.Create(PickForm);
    PickPanel.BorderStyle := bsSingle;
    PickPanel.BevelOuter := bvNone;
    PickPanel.ParentCtl3D := False;
    PickPanel.Ctl3D := False;
    PickPanel.Width := SelectedPanel.Width;
    PickPanel.Height := SelectedPanel.Height;
    PickPanel.Color := SelectedPanel.Color;
    p := SelectedPanel.ClientToScreen(Point(0, 0));
    PickPanel.Left := p.x - 1;
    PickPanel.Top := p.y - 1;
    PickPanel.Parent := PickForm;
    PickPanel.Visible := True;

    PickForm.OnMouseMove := PickFormMouseMove;

    PickForm.Cursor := crHandPoint;
    PickForm.Cursor := crCross;
    PickForm.BorderStyle := bsNone;
    PickForm.WindowState := wsMaximized;
    PickForm.OnPaint := PickFormPaint;
    PickForm.OnMouseDown := PickFormMouseDown;
    PickForm.OnKeyDown := PickFormKeyDown;
    PickForm.ShowModal;
    FreeAndNil(PickPanel);
    FreeAndNil(PickForm);
  finally
    RestoreDC(DC, SavedDC);
    FreeAndNil(b);
  end;
end;

procedure TsColorDialogForm.PickFormPaint(Sender: TObject);
begin
  BitBlt(TForm(Sender).Canvas.Handle, 0, 0, b.width, b.height, b.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TsColorDialogForm.PickFormMouseDown(Sender: TObject;  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PickColor := b.Canvas.Pixels[x, y];
  SetCurrentColor(PickColor);
  TForm(Sender).Close;
end;

procedure TsColorDialogForm.PickFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then TForm(Sender).Close
end;

procedure TsColorDialogForm.PickFormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if PickPanel <> nil then begin
    PickColor := b.Canvas.Pixels[x, y];
    PickPanel.Color := PickColor;
//    SetCurrentColor(PickColor);
  end;
end;

procedure TsColorDialogForm.InitLngCaptions;
begin
  sBitBtn1.Caption := LoadStr(s_MsgDlgOK);
  sBitBtn2.Caption := LoadStr(s_MsgDlgCancel);
  sBitBtn3.Caption := LoadStr(s_ColorDlgAdd);
  sBitBtn4.Caption := LoadStr(s_ColorDlgDefine);
  sBitBtn5.Caption := LoadStr(s_MsgDlgHelp);

  sLabel1.Caption  :=  LoadStr(s_ColorDlgMainPal);
  sLabel2.Caption  :=  LoadStr(s_ColorDlgAddPal);

  Caption                   := LoadStr(s_ColorDlgTitle);
  sREdit.BoundLabel.Caption := LoadStr(s_ColorDlgRed);
  sGEdit.BoundLabel.Caption := LoadStr(s_ColorDlgGreen);
  sBEdit.BoundLabel.Caption := LoadStr(s_ColorDlgBlue);
  sEditHex.BoundLabel.Caption := LoadStr(s_ColorDlgHex);
  sEditDecimal.BoundLabel.Caption := LoadStr(s_ColorDlgDecimal);
end;

function TsColorDialogForm.MarkerRect: TRect;
begin
  Result := Rect(GradPanel.Left + GradPanel.Width, 0, Width, GradPanel.Top + GradPanel.Height + 4);
end;

procedure TsColorDialogForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R : TRect;
begin
  R := MarkerRect;
  inc(R.Top, GradPanel.Top);
  R.Bottom := GradPanel.Top + GradPanel.Height;
  if PtInRect(R, Point(x, y)) then begin
    GradPanel.OnMouseDown(GradPanel, Button, Shift, X - GradPanel.Left - GradPanel.Width, Y - GradPanel.Top);
  end;
end;

procedure TsColorDialogForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  R : TRect;
begin
  R := MarkerRect;
  inc(R.Top, GradPanel.Top);
  R.Bottom := GradPanel.Top + GradPanel.Height;
  if PtInRect(R, Point(x, y)) then begin
    GradPanel.OnMouseMove(GradPanel, Shift, X - GradPanel.Left - GradPanel.Width, Y - GradPanel.Top);
  end;
end;

procedure TsColorDialogForm.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R : TRect;
begin
  R := MarkerRect;
  inc(R.Top, GradPanel.Top);
  R.Bottom := GradPanel.Top + GradPanel.Height;
  if PtInRect(R, Point(x, y)) then begin
    GradPanel.OnMouseUp(GradPanel, Button, Shift, X - GradPanel.Left - GradPanel.Width, Y - GradPanel.Top);
  end;
end;

procedure TsColorDialogForm.MainPalDblClick(Sender: TObject);
begin
  sBitBtn1.Click
end;

procedure TsColorDialogForm.AddPalDblClick(Sender: TObject);
begin
  sBitBtn1.Click
end;

end.
