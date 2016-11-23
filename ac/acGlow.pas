unit acGlow;
{$I sDefs.inc}

interface

uses Windows, Controls, Classes, Forms, SysUtils, Graphics, sSkinManager, Messages;

type
  TacGlowEffect = class(TObject)
  public
    SkinManager : TsSkinManager;
    MaskIndex : integer;
    Margin : integer;

    Wnd : TForm;
    AlphaBmp : TBitmap;
    OldWndProc : TWndMethod;
    constructor Create; virtual;
    procedure CreateAlphaBmp(const Width, Height : integer);
    destructor Destroy; override;
    function IntBorderWidth : integer;
    procedure Show(const R, RealRect : TRect; const Alpha : integer; WndHandle : HWND);
    procedure NewWndProc(var Message: TMessage);
  end;

  TacGlowEffects = array of TacGlowEffect;

function ShowGlow(const R, RealRect : TRect; const SkinSection, Name : string; const Margin, Alpha : integer; WndHandle : HWND; SkinManager : TsSkinManager = nil) : integer;
procedure HideGlow(const ID : integer);
procedure ClearGlows;

var
  FBlend: TBlendFunction;
  acgEffects : TacGlowEffects;

implementation

uses acntUtils, sGraphUtils, sAlphaGraph, sConst;

function ShowGlow(const R, RealRect : TRect; const SkinSection, Name : string; const Margin, Alpha : integer; WndHandle : HWND; SkinManager : TsSkinManager = nil) : integer;
var
  NewGlow : TacGlowEffect;
  mi : integer;
begin
  Result := -1;
  if acLayered then begin
    NewGlow := TacGlowEffect.Create;
    if SkinManager = nil then NewGlow.SkinManager := DefaultManager else NewGlow.SkinManager := SkinManager;
    mi := NewGlow.SkinManager.GetMaskIndex(SkinSection, Name + '0');
    if mi > -1 then begin
      Result := Length(acgEffects);
      SetLength(acgEffects, Result + 1);
      acgEffects[Result] := NewGlow;
      NewGlow.Margin := Margin;
      NewGlow.MaskIndex := mi;
      NewGlow.Show(R, RealRect, Alpha, WndHandle)
    end
    else begin
      Result := -1;
      NewGlow.Free;
    end;
  end;
end;

procedure HideGlow(const ID : integer);
begin
  if (Length(acgEffects) > ID) and (acgEffects[ID] <> nil) then begin
    acgEffects[ID].Free;
    acgEffects[ID] := nil;
  end;
end;

procedure ClearGlows;
var
  i, l : integer;
begin
  l := Length(acgEffects) - 1;
  for i := 0 to l do if acgEffects[i] <> nil then acgEffects[i].Free;
  SetLength(acgEffects, 0);
end;

{ TacGlowEffect }

constructor TacGlowEffect.Create;
begin
  Wnd := nil;
end;

procedure TacGlowEffect.CreateAlphaBmp(const Width, Height: integer);
begin
  if AlphaBmp <> nil then FreeAndNil(AlphaBmp);
  AlphaBmp := CreateBmp32(Width, Height);
  FillDC(AlphaBmp.Canvas.Handle, Rect(0, 0, Width, Height), 0);

  if MaskIndex > -1 then
    DrawSkinRect(AlphaBmp, Rect(0, 0, Width, Height), False, EmptyCI, SkinManager.ma[MaskIndex], 0, False, SkinManager);
end;

destructor TacGlowEffect.Destroy;
begin
  if Wnd <> nil then begin
    Wnd.WindowProc := OldWndProc;
    Wnd.Release;
    Wnd := nil;
  end;
  inherited;
end;

function TacGlowEffect.IntBorderWidth: integer;
begin
  Result := 2;
end;

procedure TacGlowEffect.NewWndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_NCHITTEST : begin
      Message.Result := HTTRANSPARENT;
      Exit;
    end;
  end;
  OldWndProc(Message);
end;

procedure TacGlowEffect.Show(const R, RealRect : TRect; const Alpha: integer; WndHandle : HWND);
var
  DC : hdc;
  FBmpSize: TSize;
  FBmpTopLeft: TPoint;
  dw, dh, dx1, dx2, dy1, dy2 : integer;
  TmpBmp : TBitmap;
  h : HWND;
begin
  if Wnd = nil then begin
    Wnd := TForm.Create(nil);
//    Wnd.Enabled := False;
    OldWndProc := Wnd.WindowProc;
    Wnd.WindowProc := NewWndProc;

    Wnd.ControlStyle := Wnd.ControlStyle - [csCaptureMouse];
    Wnd.BorderStyle := bsNone;
    Wnd.Visible := False;
    FBmpSize.cx := WidthOf(R) + SkinManager.MaskWidthLeft(MaskIndex) + SkinManager.MaskWidthRight(MaskIndex) - 2 * Margin;
    FBmpSize.cy := HeightOf(R) + SkinManager.MaskWidthTop(MaskIndex) + SkinManager.MaskWidthBottom(MaskIndex) - 2 * Margin;

    CreateAlphaBmp(FBmpSize.cx, FBmpSize.cy);

    dw := WidthOf(RealRect);
    dh := HeightOf(RealRect);
    if (dw <> WidthOf(R)) or (dh <> HeightOf(R)) then begin
      if RealRect.Left <> 0 then dx1 := RealRect.Left + SkinManager.MaskWidthLeft(MaskIndex) - Margin else dx1 := 0;
      if RealRect.Top <> 0 then dy1 := RealRect.Top + SkinManager.MaskWidthLeft(MaskIndex) - Margin else dy1 := 0;

      if RealRect.Right <> WidthOf(R) then begin
        if dx1 = 0
          then dx2 := WidthOf(R) - dw + SkinManager.MaskWidthRight(MaskIndex) - Margin
          else dx2 := FBmpSize.cx - dx1 - dw;
      end else dx2 := 0;
      if RealRect.Bottom <> HeightOf(R) then begin
        if dy1 = 0
          then dy2 := HeightOf(R) - dh + SkinManager.MaskWidthBottom(MaskIndex) - Margin
          else dy2 := FBmpSize.cy - dy1 - dh;
      end else dy2 := 0;

      FBmpSize.cx := FBmpSize.cx - dx1 - dx2;
      FBmpSize.cy := FBmpSize.cy - dy1 - dy2;

      if (dx1 <> 0) or (dy1 <> 0) then begin
        TmpBmp := CreateBmp32(FBmpSize.cx, FBmpSize.cy);

        BitBlt(TmpBmp.Canvas.Handle, 0, 0, FBmpSize.cx, FBmpSize.cy, AlphaBmp.Canvas.Handle, dx1, dy1, SRCCOPY);
        AlphaBmp.Assign(TmpBmp);

        TmpBmp.Free;
      end;
    end
    else begin
      dx1 := 0;
      dy1 := 0;
    end;
    Wnd.Width := FBmpSize.cx;
    Wnd.Height := FBmpSize.cy;                             
    if GetWindowLong(WndHandle, GWL_EXSTYLE) and WS_EX_TOPMOST = WS_EX_TOPMOST then begin
      Wnd.FormStyle := fsStayOnTop;
      h := HWND_TOPMOST
    end
    else h := 0;
    SetWindowPos(Wnd.Handle, h,
                 R.Left - SkinManager.MaskWidthLeft(MaskIndex) + Margin + dx1,
                 R.Top - SkinManager.MaskWidthTop(MaskIndex) + Margin + dy1,
                 FBmpSize.cx, FBmpSize.cy, SWP_NOACTIVATE or SWP_NOZORDER);
    FBmpTopLeft := Point(0, 0);
    DC := GetDC(0);
    SetWindowLong(Wnd.Handle, GWL_EXSTYLE, GetWindowLong(Wnd.Handle, GWL_EXSTYLE) or WS_EX_LAYERED or WS_EX_TOOLWINDOW);
    UpdateLayeredWindow(Wnd.Handle, DC, nil, @FBmpSize, AlphaBmp.Canvas.Handle, @FBmpTopLeft, 0, @FBlend, ULW_ALPHA);
    ShowWindow(Wnd.Handle, SW_SHOWNOACTIVATE);
    ReleaseDC(0, DC);
    AlphaBmp.Free;
  end;
end;

initialization
  with FBlend do begin
    BlendOp := AC_SRC_OVER;
    BlendFlags := 0;
    AlphaFormat := AC_SRC_ALPHA;
    SourceConstantAlpha := 255;
  end;

finalization
  ClearGlows;

end.
