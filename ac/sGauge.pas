unit sGauge;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  sConst, sGraphUtils, sVclUtils, acntUtils, sCommonData, ExtCtrls;

type
  TsGaugeKind = (gkText, gkHorizontalBar, gkVerticalBar, gkPie, gkNeedle);

  TsGauge = class(TGraphicControl)
{$IFNDEF NOTFORHELP}
  private
    FMinValue: Longint;
    FMaxValue: Longint;
    FCurValue: Longint;
    FKind: TsGaugeKind;
    FShowText: Boolean;
    FOnChange : TNotifyEvent;
    FSuffix: string;
    FCommonData: TsCommonData;
    FForeColor: TColor;
    FBorderStyle: TBorderStyle;
    FBackColor: TColor;
    FProgressSkin: TsSkinSection;
    FCalcPercents: boolean;
    FAnimated: boolean;
    FLongTime : integer;
    FAnimPos : integer;
    Light : TBitmap;
    procedure PaintBackground(AnImage: TBitmap);
    procedure PaintAsText(AnImage: TBitmap; PaintRect: TRect);
    procedure PaintAsNothing(AnImage: TBitmap; PaintRect: TRect);
    procedure PaintAsBar(AnImage: TBitmap; PaintRect: TRect);
    procedure PaintAsPie(AnImage: TBitmap; PaintRect: TRect);
    procedure PaintAsNeedle(AnImage: TBitmap; PaintRect: TRect);

    procedure SkinPaintAsText(aRect: TRect);
    procedure SkinPaintAsBar(aRect: TRect);
    procedure SkinPaintAsPie(aRect: TRect);
    procedure SkinPaintAsNeedle(aRect: TRect);
    procedure SkinPaintBody(aRect: TRect);

    procedure SetGaugeKind(Value: TsGaugeKind);
    procedure SetShowText(Value: Boolean);
    procedure SetMinValue(Value: Longint);
    procedure SetMaxValue(Value: Longint);
    procedure SetProgress(Value: Longint);
    function GetPercentDone: Longint;
    procedure WMEraseBkGND (var Message: TWMPaint); message WM_ERASEBKGND;
    procedure SetSuffix(const Value: string);
    procedure SetForeColor(const Value: TColor);
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetBackColor(const Value: TColor);
    procedure SetProgressSkin(const Value: TsSkinSection);
    procedure SetCalcPercents(const Value: boolean);
    procedure SetAnimated(const Value: boolean);
  protected
    Timer : TTimer;
    procedure WndProc (var Message: TMessage); override;
    procedure TimerAction(Sender : TObject);
    function AnimSize : integer;
    function AnimStep : integer;
    procedure UpdateLighting;
  public
    procedure Paint; override;
    procedure PrepareCache;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddProgress(Value: Longint);
    procedure AfterConstruction; override;
    procedure Loaded; override;
    property PercentDone: Longint read GetPercentDone;
    property Color;
  published
    property Align;
    property Anchors;
    property Constraints;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property Kind: TsGaugeKind read FKind write SetGaugeKind default gkHorizontalBar;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property CalcPercents : boolean read FCalcPercents write SetCalcPercents default True;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
{$ENDIF} // NOTFORHELP
    property Animated : boolean read FAnimated write SetAnimated default True;
    property SkinData : TsCommonData read FCommonData write FCommonData;
    property BackColor: TColor read FBackColor write SetBackColor default clWhite;
    property ForeColor : TColor read FForeColor write SetForeColor;
    property MinValue: Longint read FMinValue write SetMinValue default 0;
    property MaxValue: Longint read FMaxValue write SetMaxValue default 100;
    property Progress: Longint read FCurValue write SetProgress default 47;
    property ProgressSkin : TsSkinSection read FProgressSkin write SetProgressSkin;
    property ShowText: Boolean read FShowText write SetShowText default True;
    property Suffix : string read FSuffix write SetSuffix;
  end;

var
  AnimLongDelay : integer = 2222;

implementation

uses Consts, sStyleSimply, sMaskData, sBorders, sSkinProps, sAlphaGraph,
  sMessages, sSKinManager, acPNG, math;

const
  AnimMinHeight = 22;
  AnimShortDelay = 22;

{ This function solves for x in the equation "x is y% of z". }
function SolveForX(Y, Z: Longint): Longint;
begin
  Result := Longint(Trunc( Z * (Y * 0.01) ));
end;

{ This function solves for y in the equation "x is y% of z". }
function SolveForY(X, Z: Longint): Longint;
begin
  if Z = 0 then Result := 0 else Result := Longint(Trunc( (X * 100.0) / Z ));
end;

{ TsGauge }

constructor TsGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Timer := TTimer.Create(Self);
  Timer.Enabled := False;
  Timer.Interval := AnimShortDelay;
  Timer.OnTimer := TimerAction;
  FLongTime := AnimLongDelay + 1;
  FAnimPos := 9999;
  FCommonData := TsCommonData.Create(Self, False);
  FCommonData.COC := COC_TsGauge;
  ControlStyle := ControlStyle + [csOpaque];

  FMinValue := 0;
  FMaxValue := 100;
  FCurValue := 47;
  FAnimated := True;
  FCalcPercents := True;
  FKind := gkHorizontalBar;
  FShowText := True;
  FBorderStyle := bsSingle;
  FForeColor := clBlack;
  FBackColor := clWhite;
  Width := 120;
  Height := 30;
  FSuffix := '%';
end;

function TsGauge.GetPercentDone: Longint;
begin
  Result := SolveForY(FCurValue - FMinValue, FMaxValue - FMinValue);
end;

procedure TsGauge.Paint;
var
  TheImage: TBitmap;
  OverlayImage: TBitmap;
  PaintRect: TRect;
begin
  if (Width < 1) or (Height < 1) then Exit;
  if FCommonData.Skinned then begin
    if FCommonData.Updating then begin
      SetPixel(Canvas.Handle, 0, 0, clFuchsia);
      Exit
    end;

    PrepareCache;
    UpdateCorners(FCommonData, 0);

    BitBlt(Canvas.Handle, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
    FreeAndNil(FCommonData.FCacheBmp);
  end
  else with Canvas do begin
    TheImage := CreateBmp24(Width, Height);
    try
      PaintBackground(TheImage);
      PaintRect := ClientRect;
      if FBorderStyle = bsSingle then InflateRect(PaintRect, -1, -1);
      OverlayImage := CreateBmpLike(TheImage);
      OverlayImage.Canvas.Brush.Color := clWindowFrame;
      OverlayImage.Canvas.Brush.Style := bsSolid;
      OverlayImage.Canvas.FillRect(Rect(0, 0, Width, Height));
      try
        PaintBackground(OverlayImage);
        case FKind of
          gkText: PaintAsNothing(OverlayImage, PaintRect);
          gkHorizontalBar, gkVerticalBar: PaintAsBar(OverlayImage, PaintRect);
          gkPie: PaintAsPie(OverlayImage, PaintRect);
          gkNeedle: PaintAsNeedle(OverlayImage, PaintRect);
        end;
        TheImage.Canvas.CopyMode := cmSrcInvert;
        TheImage.Canvas.Draw(0, 0, OverlayImage);
        TheImage.Canvas.CopyMode := cmSrcCopy;
        if FShowText then PaintAsText(TheImage, PaintRect);
      finally
        OverlayImage.Free;
      end;
      Canvas.CopyMode := cmSrcCopy;
      Canvas.Draw(0, 0, TheImage);
    finally
      TheImage.Destroy;
    end;
  end;
end;

procedure TsGauge.SkinPaintAsText(aRect: TRect);
var
  S: string;
begin
  S := Format('%d%', [PercentDone]) + FSuffix;
  FCommonData.FCacheBmp.Canvas.Font.Assign(Font);
  FCommonData.FCacheBmp.Canvas.Pen.Style := psInsideFrame;
  FCommonData.FCacheBmp.Canvas.Brush.Style := bsClear;

  WriteTextEx(FCommonData.FCacheBmp.Canvas, PChar(s), Enabled, aRect, GetStringFlags(Self, taCenter) or DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE, FCommonData, False)
end;

procedure TsGauge.SkinPaintAsBar(aRect: TRect);
var
  FillSize: Longint;
  W, H, X, RgnIndex: Integer;
  TempBmp : TBitmap;
  index : integer;
  CI : TCacheInfo;
  bRect : TRect;
  pSkinSection, s : string;
  R1, R2, RR : TRect;
  MaskBmp : TBitmap;
  SrcBmp : TBitmap;
  LightBmp : TBitmap;
  RtSrc : TBitmap;
  IsVert : boolean;
begin
  CI := MakeCacheInfo(FCommonData.FCacheBmp);
  bRect := aRect;
  IsVert := Kind = gkVerticalBar;
  if ProgressSkin <> '' then pSkinSection := ProgressSkin else if not IsVert or Animated then pSkinSection := s_ProgressH else pSkinSection := s_ProgressV;
  Index := FCommonData.SkinManager.GetSkinIndex(pSkinSection);
  RtSrc := nil;
  if not IsVert or Animated or (Index < 0) then begin
    if IsVert then begin
      if Index < 0 then begin
        pSkinSection := s_ProgressH;
        Index := FCommonData.SkinManager.GetSkinIndex(pSkinSection);
      end;
      W := HeightOf(aRect);
      H := WidthOf(aRect);
      RtSrc := CreateBmp24(H, W);
      FillSize := min(SolveForX(PercentDone, W), W);
      BitBlt(RtSrc.Canvas.Handle, 0, 0, RtSrc.Width, RtSrc.Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
      RtSrc := MakeRotated90(RtSrc, True, True);
      CI.Bmp := RtSrc;
    end
    else begin
      W := WidthOf(aRect);
      H := HeightOf(aRect);
      FillSize := min(SolveForX(PercentDone, W), W);
    end;
    TempBmp := CreateBmp24(W, H);
    if FillSize > 0 then if FCommonData.SkinManager.IsValidSkinIndex(Index) then begin
      bRect.Right := FillSize;
      bRect.Bottom := H;
      TempBmp.Width := WidthOf(bRect);

      if Animated and (FAnimPos < Fillsize + AnimSize) then begin // Prepare mask of region
        if (Light = nil) or (AnimSize <> Light.Width) or (iffi(Kind = gkVerticalBar, Width, Height) <> Light.Height)
          then UpdateLighting;
        R1 := Rect(0, 0, TempBmp.Width, TempBmp.Height);
        R2 := Rect(0, 0, Light.Width, Light.Height);
        OffsetRect(R2, FAnimPos, 0);
        RR := RectsAnd(R1, R2);
        RgnIndex := FCommonData.SkinManager.GetMaskIndex(Index, pSkinSection, s_LightRegion);
        if (WidthOf(RR) > 0) and (RgnIndex > -1) then begin
          CI.Ready := False;
          CI.FillColor := clWhite;
          DrawSkinRect(TempBmp, Rect(0, 0, TempBmp.Width, TempBmp.Height), True, CI, FCommonData.SkinManager.ma[RgnIndex], 0, False, FCommonData.SkinManager);
          MaskBmp := CreateBmp24(WidthOf(RR), HeightOf(RR));
          BitBlt(MaskBmp.Canvas.Handle, 0, 0, MaskBmp.Width, MaskBmp.Height, TempBmp.Canvas.Handle, RR.Left, RR.Top, SRCCOPY);
          CI.Ready := True;
        end;
      end;

      PaintItem(Index, pSkinSection, CI, True, 0, Rect(0, 0, FillSize, H), Point(0, 0), TempBmp, FCommonData.SkinManager);

      if Animated and (FAnimPos < Fillsize + AnimSize) then begin
        if (WidthOf(RR) > 0) and (RgnIndex > -1) then begin
          SrcBmp := CreateBmp24(WidthOf(RR), HeightOf(RR));
          LightBmp := CreateBmp32(WidthOf(RR), HeightOf(RR));
          if R2.Left < 0 then X := -R2.Left else X := 0;
          BitBlt(SrcBmp.Canvas.Handle, 0, 0, SrcBmp.Width, SrcBmp.Height, TempBmp.Canvas.Handle, RR.Left, RR.Top, SRCCOPY);
          BitBlt(LightBmp.Canvas.Handle, 0, 0, LightBmp.Width, LightBmp.Height, Light.Canvas.Handle, X, RR.Top, SRCCOPY);
          SumByMaskWith32(SrcBmp, LightBmp, MaskBmp, Rect(0, 0, WidthOf(RR), HeightOf(RR)));
          BitBlt(TempBmp.Canvas.Handle, RR.Left, RR.Top, SrcBmp.Width, SrcBmp.Height, SrcBmp.Canvas.Handle, 0, 0, SRCCOPY);
          SrcBmp.Free;
          LightBmp.Free;
          MaskBmp.Free;
        end
        else begin
          if R2.Left < 0 then R2.Left := -R2.Left else R2.Left := 0;
          R2.Right := R2.Left + WidthOf(RR);
          CopyBmp32(RR, R2, TempBmp, Light, EmptyCI, False, clNone, 0, False);
        end;
      end;

      if RtSrc <> nil then begin
        TempBmp := MakeRotated90(TempBmp, False, True);
        if FShowText then begin
          S := Format('%d%', [iffi(FCalcPercents, PercentDone, FCurValue)]) + FSuffix;
          TempBmp.Canvas.Font.Assign(Font);
          TempBmp.Canvas.Pen.Style := psInsideFrame;
          TempBmp.Canvas.Brush.Style := bsClear;
          OffsetRect(aRect, 0, FillSize - Height);
          WriteTextEx(TempBmp.Canvas, PChar(s), Enabled, aRect, GetStringFlags(Self, taCenter) or DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE, Index, False, SkinData.SkinManager);
        end;
        BitBlt(FCommonData.FCacheBmp.Canvas.Handle, 0, Height - FillSize, TempBmp.Width, FillSize, TempBmp.Canvas.Handle, 0, 0, SRCCOPY);
      end
      else begin
        if FShowText then begin
          S := Format('%d%', [iffi(FCalcPercents, PercentDone, FCurValue)]) + FSuffix;
          TempBmp.Canvas.Font.Assign(Font);
          TempBmp.Canvas.Pen.Style := psInsideFrame;
          TempBmp.Canvas.Brush.Style := bsClear;
          WriteTextEx(TempBmp.Canvas, PChar(s), Enabled, aRect, GetStringFlags(Self, taCenter) or DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE, Index, False, SkinData.SkinManager);
        end;
        BitBlt(FCommonData.FCacheBmp.Canvas.Handle, aRect.Left, aRect.Top, TempBmp.Width, H, TempBmp.Canvas.Handle, 0, 0, SRCCOPY);
      end;
    end;
    if RtSrc <> nil then FreeAndNil(RtSrc);
  end
  else begin
    W := WidthOf(aRect);
    H := HeightOf(aRect);
    TempBmp := CreateBmp24(W, H);
    FillSize := SolveForX(PercentDone, H);
    if FillSize >= H then FillSize := H;
    if FillSize > 0 then if FCommonData.SkinManager.IsValidSkinIndex(Index) then begin
      bRect.Top := Height - FillSize;
      H := HeightOf(bRect);
      TempBmp.Height := H;
      PaintItem(Index, pSkinSection, CI, True, 0, Rect(0, 0, W, H), Point(bRect.Left, bRect.Top), TempBmp, FCommonData.SkinManager);

      if FShowText then begin
        S := Format('%d%', [iffi(FCalcPercents, PercentDone, FCurValue)]) + FSuffix;
        TempBmp.Canvas.Font.Assign(Font);
        TempBmp.Canvas.Pen.Style := psInsideFrame;
        FCommonData.FCacheBmp.Canvas.Brush.Style := bsClear;
        OffsetRect(aRect, 0, -bRect.Top);
        WriteTextEx(TempBmp.Canvas, PChar(s), Enabled, aRect, GetStringFlags(Self, taCenter) or DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE, Index, False, SkinData.SkinManager);
      end;

      BitBlt(FCommonData.FCacheBmp.Canvas.Handle, bRect.Left, bRect.Top, W, H, TempBmp.Canvas.Handle, 0, 0, SRCCOPY);
    end;
  end;
  FreeAndNil(TempBmp);
end;

procedure TsGauge.SkinPaintAsPie(aRect: TRect);
var
  MiddleX, MiddleY: Integer;
  Angle: Double;
  W, H: Integer;
begin
  W := WidthOf(aRect);
  H := HeightOf(aRect);

  FCommonData.FCacheBmp.Canvas.Pen.Width := 1;
  FCommonData.FCacheBmp.Canvas.Brush.Style := bsSolid;
  FCommonData.FCacheBmp.Canvas.Pen.Style := psSolid;
  FCommonData.FCacheBmp.Canvas.Pen.Color := ForeColor;
  FCommonData.FCacheBmp.Canvas.Ellipse(aRect.Left, aRect.Top, W, H);
  FCommonData.FCacheBmp.Canvas.Pen.Style := psSolid;
  FCommonData.FCacheBmp.Canvas.Brush.Color := ForeColor;
  if PercentDone > 0 then begin
    MiddleX := W div 2;
    MiddleY := H div 2;
    Angle := (Pi * ((PercentDone / 50) + 0.5));
    FCommonData.FCacheBmp.Canvas.Pie(aRect.Left, aRect.Top, W, H,
      Integer(Round(MiddleX * (1 - Cos(Angle)))),
      Integer(Round(MiddleY * (1 - Sin(Angle)))), MiddleX, 0);
  end;
end;

procedure TsGauge.SkinPaintAsNeedle(aRect: TRect);
var
  MiddleX: Integer;
  Angle: Double;
  X, Y, W, H: Integer;
begin

  X := aRect.Left;
  Y := aRect.Top;
  W := WidthOf(aRect);
  H := HeightOf(aRect);

  FCommonData.FCacheBmp.Canvas.Brush.Style := bsClear;
  FCommonData.FCacheBmp.Canvas.Pen.Width := 1;
  FCommonData.FCacheBmp.Canvas.Pie(X, Y, W, H * 2 - 1, X + W, aRect.Bottom - 1, X, aRect.Bottom - 1);
  FCommonData.FCacheBmp.Canvas.MoveTo(X, aRect.Bottom);
  FCommonData.FCacheBmp.Canvas.LineTo(X + W, aRect.Bottom);
  FCommonData.FCacheBmp.Canvas.Pen.Color := ForeColor;
  FCommonData.FCacheBmp.Canvas.Pen.Style := psSolid;
  FCommonData.FCacheBmp.Canvas.Pie(X, Y, W, H * 2 - 1, X + W, aRect.Bottom - 1, X, aRect.Bottom - 1);
  if PercentDone > 0 then begin
    MiddleX := Width div 2;
    FCommonData.FCacheBmp.Canvas.MoveTo(MiddleX, aRect.Bottom - 1);
    Angle := (Pi * ((PercentDone / 100)));
    FCommonData.FCacheBmp.Canvas.LineTo(Integer(Round(MiddleX * (1 - Cos(Angle)))),
      Integer(Round((aRect.Bottom - 1) * (1 - Sin(Angle)))));
  end;
end;

procedure TsGauge.SetGaugeKind(Value: TsGaugeKind);
begin
  if Value <> FKind then begin
    FKind := Value;
    Refresh;
  end;
end;

procedure TsGauge.SetShowText(Value: Boolean);
begin
  if Value <> FShowText then begin
    FShowText := Value;
    Refresh;
  end;
end;

procedure TsGauge.SetMinValue(Value: Longint);
begin
  if Value <> FMinValue then begin
    if Value > FMaxValue then begin
      if not (csLoading in ComponentState) then begin
        raise EInvalidOperation.CreateFmt(SOutOfRange, [-MaxInt, FMaxValue - 1]);
      end;
    end;
    FMinValue := Value;
    if FCurValue < Value then FCurValue := Value;
    Refresh;
  end;
end;

procedure TsGauge.SetMaxValue(Value: Longint);
begin
  if Value <> FMaxValue then begin
    if Value < FMinValue then begin
      if not (csLoading in ComponentState) then begin
        raise EInvalidOperation.CreateFmt(SOutOfRange, [FMinValue + 1, MaxInt]);
      end;
    end;
    FMaxValue := Value;
    if FCurValue > Value then FCurValue := Value;
    Refresh;
  end;
end;

procedure TsGauge.SetProgress(Value: Longint);
var
  TempPercent: Longint;
begin
  TempPercent := GetPercentDone;
  Value := LimitIt(Value, FMinValue, FMaxValue);
  if FCurValue <> Value then begin
    FCurValue := Value;
    if TempPercent <> GetPercentDone then begin
      if not RestrictDrawing then FCommonData.BGChanged := True;
      Refresh;
    end;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TsGauge.AddProgress(Value: Longint);
begin
  Progress := FCurValue + Value;
  Refresh;
end;

destructor TsGauge.Destroy;
begin
  FreeAndNil(FCommonData);
  FreeAndNil(Timer);
  if Light <> nil then FreeAndNil(Light);
  inherited Destroy;
end;

procedure TsGauge.WndProc(var Message: TMessage);
begin
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_CTRLHANDLED : begin Message.Result := 1; Exit end;
    AC_GETAPPLICATION : begin Message.Result := longint(Application); Exit end;
    AC_REMOVESKIN : if LongWord(Message.LParam) = LongWord(SkinData.SkinManager) then begin
      if Animated then Timer.Enabled := False;
      CommonWndProc(Message, FCommonData);
      Repaint;
      Exit
    end;
    AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FCommonData);
      Repaint;
      if not (csDesigning in ComponentState) and Animated then Timer.Enabled := True;
      Exit
    end;
    AC_SETNEWSKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin CommonWndProc(Message, FCommonData); exit end;
    AC_ENDPARENTUPDATE : if FCommonData.Updating then begin
      FCommonData.Updating := False;
      Repaint; Exit
    end;
  end;
  if not ControlIsReady(Self) or not Assigned(FCommonData) or not FCommonData.Skinned then inherited else begin
    if not CommonWndProc(Message, FCommonData) then inherited;
  end;
end;

procedure TsGauge.SkinPaintBody(aRect: TRect);
begin
  if ShowText then SkinPaintAsText(aRect);
  case Kind of
    gkHorizontalBar, gkVerticalBar: SkinPaintAsBar(aRect);
    gkPie: SkinPaintAsPie(aRect);
    gkNeedle: SkinPaintAsNeedle(aRect);
  end;
end;

procedure TsGauge.TimerAction(Sender: TObject);
begin
  if SkinData.Skinned then begin
    if FLongTime > AnimLongDelay then begin
      FAnimPos := - AnimSize;
      FLongTime := 0;
    end;
    inc(FLongTime, AnimShortDelay);
    if FAnimPos < iffi(Kind = gkVerticalBar, Height, Width) then begin
      if Visible and (Progress > 0) then begin
        PrepareCache;
        UpdateCorners(FCommonData, 0);
        BitBlt(Canvas.Handle, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
      end;
      inc(FAnimPos, AnimStep);
    end;
  end
  else Timer.Enabled := False;
end;

procedure TsGauge.UpdateLighting;
var
  Png : TPNGGraphic;
  dy : integer;
  rs : TResourceStream;
begin
  Png := TPNGGraphic.Create;
  rs := TResourceStream.Create(hInstance, 'ACGL', RT_RCDATA);
  Png.LoadFromStream(rs);
  rs.Free;
  if Light = nil then Light := CreateBmp32(Animsize, iffi(Kind = gkVerticalBar, Width, Height)) else begin
    Light.Width := AnimSize;
    Light.Height := iffi(Kind = gkVerticalBar, Width, Height);
  end;
  if Height < Png.Height then begin
    dy := (Png.Height - iffi(Kind = gkVerticalBar, Width, Height)) div 2 + 1;
    if Light.Height - 2 * dy < 0 then dy := 0;
  end
  else dy := 0;
  StretchBlt(Light.Canvas.Handle, 0, 0, Light.Width, Light.Height, Png.Canvas.Handle, 0, dy, Png.Width, Png.Height - dy, SRCCOPY);
  Png.Free;
end;

procedure TsGauge.WMEraseBkGND(var Message: TWMPaint);
begin
  if not Skindata.Skinned then inherited;
end;

procedure TsGauge.SetSuffix(const Value: string);
begin
  if FSuffix <> Value then begin
    FSuffix := Value;
    FCommonData.Invalidate;
  end;
end;

procedure TsGauge.AfterConstruction;
begin
  inherited;
  FCommonData.Loaded;
end;

function TsGauge.AnimSize: integer;
begin
  Result := 300;
end;

function TsGauge.AnimStep: integer;
begin
  Result := max(Round(iffi(Kind = gkVerticalBar, Height, Width) / (AnimLongDelay / (2 * AnimShortDelay))), 15);
end;

procedure TsGauge.Loaded;
begin
  inherited;
  FCommonData.Loaded;
  if not (csDesigning in ComponentState) and FAnimated and not Timer.Enabled then Timer.Enabled := True;  
end;

procedure TsGauge.SetForeColor(const Value: TColor);
begin
  if FForeColor <> Value then begin
    FForeColor := Value;
    Repaint;
  end;
end;

procedure TsGauge.PaintBackground(AnImage: TBitmap);
var
  ARect: TRect;
begin
  with AnImage.Canvas do begin
    CopyMode := cmBlackness;
    ARect := Rect(0, 0, Width, Height);
    CopyRect(ARect, Animage.Canvas, ARect);
    CopyMode := cmSrcCopy;
  end;
end;

procedure TsGauge.PrepareCache;
begin
  FCommonData.InitCacheBmp;
  PaintItem(FCommonData, GetParentCache(FCommonData), True, 0, Rect(0, 0, width, Height), Point(Left, Top), FCommonData.FCacheBMP, False);
  SkinPaintBody(Rect(0, 0, Width, Height));
  FCommonData.BGChanged := False;
end;

procedure TsGauge.SetBorderStyle(const Value: TBorderStyle);
begin
  if Value <> FBorderStyle then begin
    FBorderStyle := Value;
    Refresh;
  end;
end;

procedure TsGauge.SetCalcPercents(const Value: boolean);
begin
  if FCalcPercents <> Value then begin
    FCalcPercents := Value;
    FCommonData.Invalidate
  end;
end;

procedure TsGauge.PaintAsBar(AnImage: TBitmap; PaintRect: TRect);
var
  FillSize: Longint;
  W, H: Integer;
begin
  W := PaintRect.Right - PaintRect.Left + 1;
  H := PaintRect.Bottom - PaintRect.Top + 1;
  with AnImage.Canvas do begin
    Brush.Color := BackColor;
    FillRect(PaintRect);
    Pen.Color := ForeColor;
    Pen.Width := 1;
    Brush.Color := ForeColor;
    case FKind of
      gkHorizontalBar: begin
        FillSize := SolveForX(PercentDone, W);
        if FillSize > W then FillSize := W;
        if FillSize > 0 then FillRect(Rect(PaintRect.Left, PaintRect.Top, FillSize, H));
      end;
      gkVerticalBar: begin
        FillSize := SolveForX(PercentDone, H);
        if FillSize >= H then FillSize := H - 1;
        FillRect(Rect(PaintRect.Left, H - FillSize, W, H));
      end;
    end;
  end;
end;

procedure TsGauge.PaintAsNeedle(AnImage: TBitmap; PaintRect: TRect);
var
  MiddleX: Integer;
  Angle: Double;
  X, Y, W, H: Integer;
begin
  with PaintRect do begin
    X := Left;
    Y := Top;
    W := Right - Left;
    H := Bottom - Top;
    if FBorderStyle = bsSingle then begin
      Inc(W);
      Inc(H);
    end;
  end;
  with AnImage.Canvas do begin
    Brush.Color := Color;
    FillRect(PaintRect);
    Brush.Color := BackColor;
    Pen.Color := ForeColor;
    Pen.Width := 1;
    Pie(X, Y, W, H * 2 - 1, X + W, PaintRect.Bottom - 1, X, PaintRect.Bottom - 1);
    MoveTo(X, PaintRect.Bottom);
    LineTo(X + W, PaintRect.Bottom);
    if PercentDone > 0 then begin
      Pen.Color := ForeColor;
      MiddleX := Width div 2;
      MoveTo(MiddleX, PaintRect.Bottom - 1);
      Angle := (Pi * ((PercentDone / 100)));
      LineTo(Integer(Round(MiddleX * (1 - Cos(Angle)))),
        Integer(Round((PaintRect.Bottom - 1) * (1 - Sin(Angle)))));
    end;
  end;
end;

procedure TsGauge.PaintAsNothing(AnImage: TBitmap; PaintRect: TRect);
begin
  with AnImage do begin
    Canvas.Brush.Color := BackColor;
    Canvas.FillRect(PaintRect);
  end;
end;

procedure TsGauge.PaintAsPie(AnImage: TBitmap; PaintRect: TRect);
var
  MiddleX, MiddleY: Integer;
  Angle: Double;
  W, H: Integer;
begin
  W := PaintRect.Right - PaintRect.Left;
  H := PaintRect.Bottom - PaintRect.Top;
  if FBorderStyle = bsSingle then begin
    Inc(W);
    Inc(H);
  end;
  with AnImage.Canvas do begin
    Brush.Color := Color;
    FillRect(PaintRect);
    Brush.Color := BackColor;
    Pen.Color := ForeColor;
    Pen.Width := 1;
    Ellipse(PaintRect.Left, PaintRect.Top, W, H);
    if PercentDone > 0 then begin
      Brush.Color := ForeColor;
      MiddleX := W div 2;
      MiddleY := H div 2;
      Angle := (Pi * ((PercentDone / 50) + 0.5));
      Pie(PaintRect.Left, PaintRect.Top, W, H,
        Integer(Round(MiddleX * (1 - Cos(Angle)))),
        Integer(Round(MiddleY * (1 - Sin(Angle)))), MiddleX, 0);
    end;
  end;
end;

procedure TsGauge.PaintAsText(AnImage: TBitmap; PaintRect: TRect);
var
  S: string;
  X, Y: Integer;
  OverRect: TBitmap;
begin
  OverRect := CreateBmpLike(AnImage);
  OverRect.Canvas.Brush.Color := clWindowFrame;
  OverRect.Canvas.Brush.Style := bsSolid;
  OverRect.Canvas.FillRect(Rect(0, 0, Width, Height));
  try
    PaintBackground(OverRect);
    S := Format('%d%', [iffi(FCalcPercents, PercentDone, FCurValue)]) + FSuffix;
    with OverRect.Canvas do begin
      Brush.Style := bsClear;
      Font := Self.Font;
      Font.Color := clWhite;
      with PaintRect do begin
        X := (Right - Left + 1 - TextWidth(S)) div 2;
        Y := (Bottom - Top + 1 - TextHeight(S)) div 2;
      end;
      TextRect(PaintRect, X, Y, S);
    end;
    AnImage.Canvas.CopyMode := cmSrcInvert;
    AnImage.Canvas.Draw(0, 0, OverRect);
  finally
    OverRect.Free;
  end;
end;

procedure TsGauge.SetAnimated(const Value: boolean);
begin
  if FAnimated <> Value then begin
    FAnimated := Value;
    if not (csDesigning in ComponentState) then Timer.Enabled := Value;
  end;
end;

procedure TsGauge.SetBackColor(const Value: TColor);
begin
  if Value <> FBackColor then begin
    FBackColor := Value;
    Refresh;
  end;
end;

procedure TsGauge.SetProgressSkin(const Value: TsSkinSection);
begin
  if FProgressSkin <> Value then begin
    FProgressSkin := Value;
    FCommonData.BGChanged := True;
    Repaint;
  end;
end;

end.
