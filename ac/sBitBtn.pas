unit sBitBtn;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Imglist,
  StdCtrls, Buttons, sCommonData, sConst, sDefaults, sFade
  {$IFDEF LOGGED}, sDebugMsgs{$ENDIF} {$IFDEF TNTUNICODE}, TntButtons{$ENDIF};

type
{$IFDEF TNTUNICODE}
  TsBitBtn = class(TTntBitBtn)
{$ELSE}
  TsBitBtn = class(TBitBtn)
{$ENDIF}
{$IFNDEF NOTFORHELP}
  private
    FOldSpacing : integer;
    FCommonData: TsCommonData;
    FMouseClicked : boolean;
    FDown: boolean;
    RegionChanged : boolean;
    FFocusMargin: integer;
    FDisabledKind: TsDisabledKind;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FadeTimer : TsFadeTimer;
    FDisabledGlyphKind: TsDisabledGlyphKind;
    FGrayed: boolean;
    FBlend: integer;
    FOffset: Integer;
    FImageIndex: integer;
    FImages: TCustomImageList;
    FShowCaption: boolean;
    FShowFocus: boolean;
    FAlignment: TAlignment;
    FDrawOverBorder: boolean;
    FOnPaint: TBmpPaintEvent;
    FTextAlignment: TAlignment;
    FAnimatEvents: TacAnimatEvents;
    LastRect : TRect;
    FReflected : boolean;
{$IFNDEF DELPHI7UP}
    FWordWrap : boolean;
    procedure SetWordWrap(const Value: boolean);
{$ENDIF}
    procedure SetDown(const Value: boolean);
    procedure SetFocusMargin(const Value: integer);
    procedure SetDisabledKind(const Value: TsDisabledKind);
    procedure WMKeyUp (var Message: TWMKey); message WM_KEYUP;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure SetDisabledGlyphKind(const Value: TsDisabledGlyphKind);
    procedure SetBlend(const Value: integer);
    procedure SetGrayed(const Value: boolean);
    procedure SetOffset(const Value: Integer);
    procedure SetImageIndex(const Value: integer);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetShowCaption(const Value: boolean);
    procedure SetShowFocus(const Value: boolean);
    procedure SetAlignment(const Value: TAlignment);
    function GetDown: boolean;
    procedure SetDrawOverBorder(const Value: boolean);
    procedure SetReflected(const Value: boolean);
    procedure SetTextAlignment(const Value: TAlignment);
  protected
    IsFocused : boolean;
    FRegion : hrgn;
    OldLayout : TButtonLayout;
    procedure StdDrawItem(const DrawItemStruct: TDrawItemStruct);
    procedure MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;

    procedure OurPaintHandler(aDC : hdc);
    procedure DoDrawText(var Rect: TRect; Flags: Cardinal);
    procedure DrawCaption(Canvas : TCanvas = nil);
    function CaptionRect : TRect;
    function TextRectSize : TSize;
    function GlyphWidth : integer;
    function GlyphHeight : integer;
    function GenMargin : integer;
    procedure PrepareCache;
  public
    constructor Create(AOwner:TComponent); override;
    function CurrentState : integer; virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Loaded; override;
    function ImgRect : TRect;
    procedure Invalidate; override;
    procedure WndProc (var Message: TMessage); override;
    procedure SetButtonStyle(ADefault: Boolean); override;
  published
    property OnPaint : TBmpPaintEvent read FOnPaint write FOnPaint;
{$ENDIF} // NOTFORHELP
    property OnMouseEnter : TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave : TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property AnimatEvents : TacAnimatEvents read FAnimatEvents write FAnimatEvents default [aeGlobalDef];
    property Alignment : TAlignment read FAlignment write SetAlignment default taCenter;
    property Blend : integer read FBlend write SetBlend default 0;
    property SkinData : TsCommonData read FCommonData write FCommonData;
    property DisabledGlyphKind : TsDisabledGlyphKind read FDisabledGlyphKind write SetDisabledGlyphKind default DefDisabledGlyphKind;
    property DisabledKind : TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property Down : boolean read GetDown write SetDown default False;
    property FocusMargin : integer read FFocusMargin write SetFocusMargin default 1;
    property Grayed : boolean read FGrayed write SetGrayed default False;
    property ImageIndex : integer read FImageIndex write SetImageIndex default -1;
    property Images : TCustomImageList read FImages write SetImages;
    property Reflected : boolean read FReflected write SetReflected default False;
    property ShowCaption: boolean read FShowCaption write SetShowCaption default True;
    property ShowFocus : boolean read FShowFocus write SetShowFocus default True;
    property DrawOverBorder : boolean read FDrawOverBorder write SetDrawOverBorder default True;
    property TextOffset : Integer read FOffset write SetOffset default 0;       
    property TextAlignment : TAlignment read FTextAlignment write SetTextAlignment default taCenter;
{$IFNDEF DELPHI7UP}
    property WordWrap : boolean read FWordWrap write SetWordWrap default True;
{$ELSE}
    property WordWrap default True;
{$ENDIF}
  end;

implementation

uses sVCLUtils, sMessages, acntUtils, sGraphUtils, sAlphaGraph, acGlow, sBorders, ActnList, sButton, sThirdParty, sSkinManager, sStyleSimply, acPNG
{$IFDEF DELPHI7UP}
  , Themes
{$ENDIF};

var
  bFocusChanging : boolean = False;

{ TsBitBtn }

function IsImgListDefined(Btn : TsBitBtn) : boolean;
begin
  Result := Assigned(Btn.Images) and (GetImageCount(Btn.Images) > 0) and (Btn.ImageIndex >= 0) and (Btn.ImageIndex < GetImageCount(Btn.Images))
end;

function MaxCaptionWidth(Button : TsBitBtn) : integer;
begin
  with Button do begin
    if ShowCaption and (Caption <> '') then begin
      Result := Width - 2 * Margin;
      case Layout of
        blGlyphLeft, blGlyphRight : Result := Result - (Spacing + GlyphWidth) * integer(GlyphWidth <> 0);
      end;
    end
    else Result := 0
  end;
end;

procedure TsBitBtn.SetButtonStyle(ADefault: Boolean);
begin
  inherited;
  if ADefault <> IsFocused then begin
    IsFocused := ADefault;
  end;
  SkinData.Invalidate
end;

procedure TsBitBtn.AfterConstruction;
begin
  inherited;
  FCommonData.FCacheBmp.Canvas.Font.Assign(Font);
  FCommonData.Loaded;
end;

function TsBitBtn.CaptionRect : TRect;
var
  GlyphPos: TPoint;
begin
  CalcButtonLayout(ClientRect, Point(GlyphWidth, GlyphHeight), TextRectSize, Layout, Alignment, Margin, Spacing, GlyphPos, Result, DrawTextBiDiModeFlags(0));
end;

constructor TsBitBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csOpaque];
  FCommonData := TsCommonData.Create(Self, True);
  FCommonData.COC := COC_TsBitBtn;
  FFocusMargin := 1;
  FadeTimer := nil;
  FRegion := 1;
  FDown := False;

  FImageIndex := -1;
  FGrayed := False;
  FBlend := 0;
  FDisabledGlyphKind := DefDisabledGlyphKind;
  FDisabledKind := DefDisabledKind;
  FDrawOverBorder := True;
  FOffset := 0;
  FAlignment := taCenter;
  FShowCaption := True;
  FTextAlignment := taCenter;
  FShowFocus := True;
  FAnimatEvents := [aeGlobalDef];
{$IFNDEF DELPHI7UP}
  FWordWrap := True;
{$ELSE}
  WordWrap := True;
{$ENDIF}
  RegionChanged := True;
//  GlowID := -1;
end;

function TsBitBtn.CurrentState: integer;
begin
  if ((SendMessage(Handle, BM_GETSTATE, 0, 0) and BST_PUSHED = BST_PUSHED) or fGlobalFlag) and (SkinData.FMouseAbove or not (csLButtonDown in ControlState)) or FDown
    then Result := 2
    else if IsFocused or ((GetWindowLong(Handle, GWL_STYLE) and $000F) = BS_DEFPUSHBUTTON) or ControlIsActive(FCommonData) or ((csDesigning in ComponentState) and Default)
      then Result := 1
      else Result := 0
end;

destructor TsBitBtn.Destroy;
begin
  StopFading(FadeTimer, FCommonData);
  if Assigned(FCommonData) then FreeAndNil(FCommonData);
  inherited Destroy;
end;

procedure TsBitBtn.DoDrawText(var Rect: TRect; Flags: Cardinal);
begin
  Flags := DrawTextBiDiModeFlags(Flags);
  acWriteTextEx(FCommonData.FCacheBMP.Canvas, PacChar(Caption), True, Rect, Flags, FCommonData, CurrentState <> 0);
end;

procedure TsBitBtn.DrawCaption(Canvas : TCanvas = nil);
var
  R : TRect;
  DrawStyle : Cardinal;
begin
  if Canvas = nil then Canvas := FCommonData.FCacheBmp.Canvas;
  Canvas.Font.Assign(Font);
  Canvas.Brush.Style := bsClear;
  R := CaptionRect;
  if CurrentState = 2 then OffsetRect(R, 1, 1);
  DrawStyle := DT_EXPANDTABS or GetStringFlags(Self, FTextAlignment);
  if WordWrap then DrawStyle := DrawStyle or DT_WORDBREAK;
  if SkinData.Skinned then begin
    DoDrawText(R, DrawStyle);

    if Enabled and Focused and (Caption <> '') and FCommonData.SkinManager.gd[FCommonData.SkinIndex].ShowFocus and ShowFocus and ShowCaption then begin
      InflateRect(R, FocusMargin, FocusMargin);
      FocusRect(Canvas, R);
    end;
  end
  else begin
    Canvas.Brush.Style := bsClear;
    if not Enabled then begin
      OffsetRect(R, 1, 1);
      Canvas.Font.Color := clBtnHighlight;
      acDrawText(Canvas.Handle, PacChar(Caption), R, DrawStyle);
      OffsetRect(R, -1, -1);
      Canvas.Font.Color := clBtnShadow;
      acDrawText(Canvas.Handle, PacChar(Caption), R, DrawStyle);
    end
    else acDrawText(Canvas.Handle, PacChar(Caption), R, DrawStyle);
  end;
end;

function TsBitBtn.GenMargin: integer;
begin
  if Margin < 0 then Result := 0 else Result := Margin + 3;
end;

function TsBitBtn.GetDown: boolean;
begin
  Result := FDown;
end;

function TsBitBtn.GlyphHeight: integer;
begin
  if IsImgListDefined(Self) then begin
    Result := Images.Height;
  end
  else
  if (Glyph <> nil) and (Glyph.Height > 0) then begin
    Result := Glyph.Height;
  end
  else Result := 0;
end;

function TsBitBtn.GlyphWidth: integer;
begin
  if IsImgListDefined(Self) then begin
    Result := Images.Width div NumGlyphs;
  end
  else
  if (Glyph <> nil) and (Glyph.Width > 0) then begin
    Result := Glyph.Width div NumGlyphs;
  end
  else Result := 0;
end;

function TsBitBtn.ImgRect: TRect;
var
  x, y : integer;
  dh, dw : integer;
begin
  x := 0;
  y := 0;
  Result := Rect(0, 0, 0, 0);
  dw := (Width - GlyphWidth - Spacing * integer(ShowCaption and (GlyphWidth > 0) and (Caption <> '')) - TextRectSize.cx) div 2 - GenMargin;
  dh := (Height - GlyphHeight - Spacing * integer(ShowCaption and (GlyphWidth > 0) and (Caption <> '')) - TextRectSize.cy) div 2 - GenMargin;
  case Layout of
    blGlyphLeft : case Alignment of
      taLeftJustify : begin
        x := GenMargin;
        y := (Height - GlyphHeight) div 2;
      end;
      taCenter : begin
        x := GenMargin + dw;
        y := (Height - GlyphHeight) div 2;
      end;
      taRightJustify : begin
        x := GenMargin + 2 * dw;
        y := (Height - GlyphHeight) div 2;
      end;
    end;
    blGlyphRight : case Alignment of
      taLeftJustify : begin
        x := Width - GenMargin - 2 * dw - Spacing * integer(ShowCaption and (GlyphWidth > 0) and (Caption <> '')) - GlyphWidth;
        y := (Height - GlyphHeight) div 2;
      end;
      taCenter : begin
        x := (Width - GlyphWidth + Spacing * integer(ShowCaption and (GlyphWidth > 0) and (Caption <> '')) + TextRectSize.cx) div 2;
        y := (Height - GlyphHeight) div 2;
      end;
      taRightJustify : begin
        x := Width - GlyphWidth - GenMargin;
        y := (Height - GlyphHeight) div 2;
      end;
    end;
    blGlyphTop : begin
      x := (Width - GlyphWidth) div 2 + 1;
      y := GenMargin + dh;
    end;
    blGlyphBottom : begin
      x := (Width - GlyphWidth) div 2 + 1;
      y := Height - GenMargin - dh - GlyphHeight;
    end;
  end;
  inc(x, integer(CurrentState = 2));
  inc(y, integer(CurrentState = 2));
  Result := Rect(x, y, x + GlyphWidth, y + GlyphHeight);
  if Reflected then OffsetRect(Result, 0, - WidthOf(Result) div 6); 
end;

procedure TsBitBtn.Invalidate;
begin
  if (OldLayout <> Layout) then begin
    OldLayout := Layout;
    FCommonData.BGChanged := True;
  end;
  inherited;
end;

procedure TsBitBtn.Loaded;
begin
  inherited;
  FCommonData.FCacheBmp.Canvas.Font.Assign(Font);
  FCommonData.Loaded;
  if IsImgListDefined(Self) then Glyph.Assign(nil);
end;                        

procedure TsBitBtn.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FCommonData.Skinned and Enabled and not (csDesigning in ComponentState) then begin
    FCommonData.Updating := False;
    if (Button = mbLeft) and not ShowHintStored then begin
      AppShowHint := Application.ShowHint;
      Application.ShowHint := False;
      ShowHintStored := True;
    end;
    FMouseClicked := True;
    if (Button = mbLeft) then begin
      if not Down then begin
        RegionChanged := True;
        FCommonData.Updating := FCommonData.Updating;
        FCommonData.BGChanged := False;
        DoChangePaint(FadeTimer, FCommonData, True, EventEnabled(aeMouseDown, FAnimatEvents));
      end;
    end;
    if ShowHintStored then begin;
      Application.ShowHint := AppShowHint;
      ShowHintStored := False
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TsBitBtn.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (FCommonData <> nil) and FCommonData.Skinned and Enabled and not (csDesigning in ComponentState) then begin
    if Button = mbLeft then begin
      Application.ShowHint := AppShowHint;
      ShowHintStored := False;
    end;

    if FMouseClicked and not (csDestroying in ComponentState) then begin
      FMouseClicked := False;
      if (Button = mbLeft) and Enabled then begin
        if (FadeTimer <> nil) and (FadeTimer.FadeLevel < FadeTimer.Iterations) then begin
          FadeTimer.Enabled := False;
          FCommonData.BGChanged := True;
          fGlobalFlag := True;
          RedrawWindow(Handle, nil, 0, RDW_UPDATENOW or RDW_INVALIDATE);
          fGlobalFlag := False;
          Sleep(30);
        end;
        FCommonData.Updating := False;
        try
          if (Self <> nil) and not (csDestroying in ComponentState) then begin
            RegionChanged := True;
            FCommonData.BGChanged := False;
            if Assigned(FCommonData) then DoChangePaint(FadeTimer, FCommonData, True, EventEnabled(aeMouseUp, FAnimatEvents), fdUp);
          end;
        except
        end;
      end;
    end
    else if (FadeTimer <> nil) then FreeAndNil(FadeTimer);
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TsBitBtn.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Images) then Images := nil;
end;

procedure TsBitBtn.OurPaintHandler;
var
  DC, SavedDC : hdc;
  PS : TPaintStruct;
  b : boolean;
  R : TRect;
begin
  if InAnimationProcess and ((aDC <> acPrintDC) or (aDC = 0)) then Exit;
  if aDC = 0 then DC := GetDC(Handle) else DC := aDC;

  if not InanimationProcess then BeginPaint(Handle, PS);
  try
    FCommonData.Updating := FCommonData.Updating;
    if not FCommonData.Updating and not (Assigned(FadeTimer) and FadeTimer.Enabled) then begin
      FCommonData.BGChanged := FCommonData.BGChanged or FCommonData.HalfVisible or GetBoolMsg(Parent, AC_GETHALFVISIBLE);
      FCommonData.HalfVisible := not RectInRect(Parent.ClientRect, BoundsRect);

      if not FCommonData.BGChanged then begin
        if (FOldSpacing <> Spacing) then begin
          FCommonData.BGChanged := True;
          FOldSpacing := Spacing;
        end;
      end;

      b := (FRegion = 1) or aSkinChanging;
      FRegion := 0;
      if RegionChanged then FCommonData.BGChanged := True;
      if (FCommonData.BGChanged or (csDesigning in ComponentState {for glyph changing})) then begin
        PrepareCache;
      end;
      if RegionChanged then begin
        UpdateCorners(FCommonData, CurrentState);
        if FCommonData.BorderIndex > 0 then begin
          BitBlt(DC, 0, 0, FCommonData.SkinManager.MaskWidthLeft(FCommonData.BorderIndex), FCommonData.SkinManager.MaskWidthTop(FCommonData.BorderIndex), FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
          BitBlt(DC, 0, Height - FCommonData.SkinManager.MaskWidthBottom(FCommonData.BorderIndex), Width, FCommonData.SkinManager.MaskWidthTop(FCommonData.BorderIndex), FCommonData.FCacheBmp.Canvas.Handle, 0, Height - FCommonData.SkinManager.MaskWidthBottom(FCommonData.BorderIndex), SRCCOPY);
          BitBlt(DC, Width - FCommonData.SkinManager.MaskWidthRight(FCommonData.BorderIndex), Height - FCommonData.SkinManager.MaskWidthBottom(FCommonData.BorderIndex), FCommonData.SkinManager.MaskWidthRight(FCommonData.BorderIndex), FCommonData.SkinManager.MaskWidthTop(FCommonData.BorderIndex), FCommonData.FCacheBmp.Canvas.Handle, Width - FCommonData.SkinManager.MaskWidthRight(FCommonData.BorderIndex), Height - FCommonData.SkinManager.MaskWidthBottom(FCommonData.BorderIndex), SRCCOPY);
          BitBlt(DC, Width - FCommonData.SkinManager.MaskWidthRight(FCommonData.BorderIndex), 0, FCommonData.SkinManager.MaskWidthRight(FCommonData.BorderIndex), FCommonData.SkinManager.MaskWidthTop(FCommonData.BorderIndex), FCommonData.FCacheBmp.Canvas.Handle, Width - FCommonData.SkinManager.MaskWidthRight(FCommonData.BorderIndex), 0, SRCCOPY);
        end; 
        if (DC <> acPrintDC) and not (csDesigning in ComponentState) and not (csAlignmentNeeded in ControlState) then begin // Set region
          if FRegion <> 0 then begin
            SetWindowRgn(Handle, FRegion, b); // Speed increased if repainting is disabled
            if (Width < WidthOf(LastRect)) or (Height < HeightOf(LastRect)) then begin
              if not GetParentCache(SkinData).Ready then begin
                R := Rect(LastRect.Right - SkinData.SkinManager.ma[SkinData.BorderIndex].WR, LastRect.Top, LastRect.Right, LastRect.Top + SkinData.SkinManager.ma[SkinData.BorderIndex].WT);
                InvalidateRect(Parent.Handle, @R, True); // Top-right
                R := Rect(LastRect.Right - SkinData.SkinManager.ma[SkinData.BorderIndex].WR, LastRect.Bottom - SkinData.SkinManager.ma[SkinData.BorderIndex].WB, LastRect.Right, LastRect.Bottom);
                InvalidateRect(Parent.Handle, @R, True); // Bottom-right
                R := Rect(LastRect.Left, LastRect.Bottom - SkinData.SkinManager.ma[SkinData.BorderIndex].WB, LastRect.Left + SkinData.SkinManager.ma[SkinData.BorderIndex].WL, LastRect.Bottom);
                InvalidateRect(Parent.Handle, @R, True); // Left-bottom
              end;
            end;
            LastRect := BoundsRect;
            FRegion := 0;
          end;
        end;
      end;
      SavedDC := SaveDC(DC);
      try
        BitBlt(DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
      finally
        RestoreDC(DC, SavedDC);
        if aDC = 0 then ReleaseDC(Handle, DC);
      end;
    end;
  finally
    if not InanimationProcess then EndPaint(Handle, PS);
  end;
end;

procedure TsBitBtn.PrepareCache;
var
  CI : TCacheInfo;
  BGInfo : TacBGInfo;
begin
  BGInfo.PleaseDraw := False;
  GetBGInfo(@BGInfo, Parent);
  CI := BGInfoToCI(@BGInfo);

  InitCacheBmp(SkinData);

  PaintItemBG(FCommonData, CI, CurrentState, Rect(0, 0, Width, Height), Point(Left, Top), FCommonData.FCacheBMP, integer(CurrentState = 2), integer(CurrentState = 2));
  if RegionChanged then begin FRegion := 0; FRegion := CreateRectRgn(0, 0, Width, Height); end; // Empty region
  if FDrawOverBorder and FCommonData.SkinManager.IsValidImgIndex(FCommonData.BorderIndex) then
    PaintRgnBorder(FCommonData.FCacheBmp, FRegion, True, FCommonData.SkinManager.ma[FCommonData.BorderIndex], CurrentState);
  if ShowCaption then DrawCaption;
  DrawBtnGlyph(Self);
  if not FDrawOverBorder and FCommonData.SkinManager.IsValidImgIndex(FCommonData.BorderIndex) then
    PaintRgnBorder(FCommonData.FCacheBmp, FRegion, True, FCommonData.SkinManager.ma[FCommonData.BorderIndex], CurrentState);

  if not Enabled or ((Action <> nil) and not TAction(Action).Enabled) then begin
    CI := GetParentCache(FCommonData);
    BmpDisabledKind(FCommonData.FCacheBmp, FDisabledKind, Parent, CI, Point(Left, Top));
  end;
  if Assigned(FOnPaint) then FOnPaint(Self, FCommonData.FCacheBmp);
  FCommonData.BGChanged := False;
end;

procedure TsBitBtn.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    FCommonData.Invalidate;
  end;
end;

procedure TsBitBtn.SetBlend(const Value: integer);
begin
  if FBlend <> Value then begin
    if Value < 0 then FBlend := 0 else if Value > 100 then FBlend := 100 else FBlend := Value;
    FCommonData.Invalidate;
  end;
end;

procedure TsBitBtn.SetDisabledGlyphKind(const Value: TsDisabledGlyphKind);
begin
  if FDisabledGlyphKind <> Value then begin
    FDisabledGlyphKind := Value;
    FCommonData.Invalidate;
  end;
end;

procedure TsBitBtn.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FCommonData.Invalidate;
  end;
end;

{$IFNDEF DELPHI7UP}
procedure TsBitBtn.SetWordWrap(const Value: boolean);
begin
  if FWordWrap <> Value then begin
    FWordWrap := Value;
    FCommonData.BGChanged := True;
    Repaint;
  end;
end;
{$ENDIF}

procedure TsBitBtn.SetDown(const Value: boolean);
begin
  if FDown <> Value then begin
    FDown := Value;
    RegionChanged := True;
    FCommonData.BGChanged := True;
    Repaint;
  end;
end;

procedure TsBitBtn.SetFocusMargin(const Value: integer);
begin
  if (FFocusMargin <> Value) then begin
    FFocusMargin := Value;
  end;
end;

procedure TsBitBtn.SetGrayed(const Value: boolean);
begin
  if FGrayed <> Value then begin
    FGrayed := Value;
    FCommonData.Invalidate;
  end;
end;

procedure TsBitBtn.SetImageIndex(const Value: integer);
begin
  if FImageIndex <> Value then begin
    FImageIndex := Value;
    FCommonData.Invalidate;
  end;
end;

procedure TsBitBtn.SetImages(const Value: TCustomImageList);
begin
  if Images <> Value then begin
    FImages := Value;
    FCommonData.Invalidate;
  end;
end;

procedure TsBitBtn.SetOffset(const Value: Integer);
begin
  if (FOffset <> Value) then begin
    FOffset := Value;
    if Visible or (csDesigning in ComponentState) then begin
      FCommonData.Invalidate;
    end;
  end;
end;

procedure TsBitBtn.SetShowCaption(const Value: boolean);
begin
  if FShowCaption <> Value then begin
    FShowCaption := Value;
    FCommonData.Invalidate;
  end;
end;

procedure TsBitBtn.SetShowFocus(const Value: boolean);
begin
  if FShowFocus <> Value then begin
    FShowFocus := Value;
    FCommonData.Invalidate;
  end;
end;

procedure TsBitBtn.SetDrawOverBorder(const Value: boolean);
begin
  if FDrawOverBorder <> Value then begin
    FDrawOverBorder := Value;
    FCommonData.Invalidate;
  end;
end;

procedure TsBitBtn.SetReflected(const Value: boolean);
begin
  if FReflected <> Value then begin
    FReflected := Value;
    FCommonData.Invalidate;
  end;
end;

function TsBitBtn.TextRectSize: TSize;
var
  R : TRect;
  DrawStyle : longint;
begin
  R := Rect(0, 0, MaxCaptionWidth(Self), 0);
  DrawStyle := DT_EXPANDTABS or GetStringFlags(Self, FTextAlignment) or DT_CALCRECT;
  if WordWrap then DrawStyle := DrawStyle or DT_WORDBREAK;
  FCommonData.FCacheBMP.Canvas.Font.Assign(Font);
{$IFDEF TNTUNICODE}
  DrawTextW(FCommonData.FCacheBMP.Canvas.Handle, PWideChar(Caption), Length(Caption), R, DrawStyle);
{$ELSE}
  DrawText(FCommonData.FCacheBMP.Canvas.Handle, PChar(Caption), Length(Caption), R, DrawStyle);
{$ENDIF}
  Result.cy := HeightOf(R);
  Result.cx := WidthOf(R);
end;

procedure TsBitBtn.WMKeyUp(var Message: TWMKey);
begin
  inherited;
  if Assigned(FCommonData) and FCommonData.Skinned and (Message.CharCode = 32) then begin
    RegionChanged := True;
    FCommonData.BGChanged := True;
    Repaint;
  end;
end;

procedure TsBitBtn.WndProc(var Message: TMessage);
//var
//  R : TRect;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  if (Message.Msg = WM_KILLFOCUS) and (csDestroying in ComponentState) then Exit;
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_CTRLHANDLED : begin Message.Result := 1; Exit end; // AlphaSkins is supported
    AC_GETAPPLICATION : begin Message.Result := longint(Application); Exit end;
    AC_SETNEWSKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      RegionChanged := True;
      StopFading(FadeTimer, FCommonData);
      CommonWndProc(Message, FCommonData);
      exit
    end;
    AC_REMOVESKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) and not (csDestroying in ComponentState) then begin
      StopFading(FadeTimer, FCommonData);
      CommonWndProc(Message, FCommonData);
      FRegion := 0;
      SetWindowRgn(Handle, 0, False);
      Repaint;
      Exit;
    end;
    AC_REFRESH : if LongWord(Message.LParam) = LongWord(SkinData.SkinManager) then begin
      StopFading(FadeTimer, FCommonData);
      CommonWndProc(Message, FCommonData);
      RegionChanged := True;
      if acPrintDC = 0 then Repaint;
      exit
    end;
    AC_PREPARECACHE : PrepareCache;
    AC_DRAWANIMAGE : if FRegion <> 0 then DeleteObject(FRegion);
    AC_STOPFADING : begin StopFading(FadeTimer, FCommonData); Exit end;
  end;
  if not ControlIsReady(Self) or not FCommonData.Skinned(True) then begin
    case Message.Msg of
      CM_MOUSEENTER : begin
        SkinData.FMouseAbove := True;
        if Assigned(FOnMouseEnter) and Enabled and not (csDesigning in ComponentState) then FOnMouseEnter(Self);
      end;
      CM_MOUSELEAVE : begin
        SkinData.FMouseAbove := False;
        if Assigned(FOnMouseLeave) and Enabled and not (csDesigning in ComponentState) then FOnMouseLeave(Self);
      end;
    end;
    inherited
  end
  else begin
    if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
      AC_ENDPARENTUPDATE : if FCommonData.Updating then begin
        FCommonData.Updating := False;
        Repaint;
        Exit
      end;
      AC_URGENTPAINT : begin
        if FCommonData.UrgentPainting then PrepareCache;
        CommonWndProc(Message, FCommonData);
        Exit
      end;
    end
    else case Message.Msg of
      WM_WINDOWPOSCHANGING : RegionChanged := True;
      WM_WINDOWPOSCHANGED : begin
        RegionChanged := True;
        SkinData.BGChanged := True;
      end;
      CM_UIACTIVATE : SkinData.Updating := False;
      CM_DIALOGCHAR : if (Enabled and Focused and (TCMDialogChar(Message).CharCode = VK_SPACE)) then begin
        StopFading(FadeTimer, FCommonData);
        RegionChanged := True;
        FCommonData.BGChanged := True;
        if (SkinData.GlowID <> -1) then begin
          HideGlow(SkinData.GlowID);
          SkinData.GlowID := -1;
        end;
        Repaint;
      end;
      CM_MOUSEENTER : if Enabled and not (csDesigning in ComponentState) then begin
        if not FCommonData.FMouseAbove and not MouseForbidden then begin
          if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
          FCommonData.FMouseAbove := True;
          FCommonData.BGChanged := False;
          DoChangePaint(FadeTimer, FCommonData, False, EventEnabled(aeMouseEnter, FAnimatEvents));
        end;
      end;
      CM_MOUSELEAVE : if Enabled and not (csDesigning in ComponentState) then begin
        if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
        FCommonData.FMouseAbove := False;
        FCommonData.BGChanged := False;
        DoChangePaint(FadeTimer, FCommonData, False, EventEnabled(aeMouseLeave, FAnimatEvents));
      end;
      WM_SIZE : begin
        RegionChanged := True;
      end;
      WM_UPDATEUISTATE, WM_ERASEBKGND : if Visible or (csDesigning in ComponentState) then begin
        Message.Result := 0;
        Exit;
      end;
      WM_SETTEXT : if Showing then begin
        if FadeTimer <> nil then StopFading(FadeTimer, FCommonData);
        Perform(WM_SETREDRAW, 0, 0);
        inherited;
        Perform(WM_SETREDRAW, 1, 0);
        Exit;
      end;
      CM_TEXTCHANGED : if not (csDestroying in ComponentState) and (Visible or (csDesigning in ComponentState)) then begin
        FCommonData.Invalidate;
        Exit;
      end;
      WM_PRINT : begin
        RegionChanged := True;
        OurPaintHandler(TWMPaint(Message).DC);
        Exit;
      end;
      WM_PAINT : if Visible or (csDesigning in ComponentState) then begin
        if (Parent = nil) then Exit;
        OurPaintHandler(TWMPaint(Message).DC);
        if not (csDesigning in ComponentState) then Exit;
      end;
      CN_DRAWITEM : begin Message.WParam := 0; Message.LParam := 0; Message.Result := 1; Exit; end;
      WM_MOVE : if (FCommonData.SkinManager.gd[FCommonData.SkinIndex].Transparency > 0) or ((FCommonData.SkinManager.gd[FCommonData.SkinIndex].HotTransparency > 0) and ControlIsActive(FCommonData)) then begin
        FCommonData.BGChanged := True;
        Repaint
      end;
      WM_SETFOCUS, CM_ENTER : if not (csDesigning in ComponentState) and Visible then begin
        if Enabled and not (csDestroying in ComponentState) and not bFocusChanging then begin
          Perform(WM_SETREDRAW, 0, 0);
          bFocusChanging := True;
          inherited;
          Perform(WM_SETREDRAW, 1, 0);
          bFocusChanging := False;
          if FadeTimer <> nil then FadeTimer.Change {Fast repaint} else FCommonData.Invalidate;
        end
        else inherited;
        Exit;
      end;
      WM_KILLFOCUS, CM_EXIT: if not (csDesigning in ComponentState) and Visible then begin
        if Enabled and not (csDestroying in ComponentState) then begin
          if FadeTimer <> nil then StopFading(FadeTimer, FCommonData);
          Perform(WM_SETREDRAW, 0, 0);
          inherited;
          Perform(WM_SETREDRAW, 1, 0);
          if FCommonData.Skinned then begin
            FCommonData.FFocused := False;
            FCommonData.Invalidate; 
          end;
        end
        else inherited;
        Exit
      end;
      CM_FOCUSCHANGED : if Visible then begin
        if not bFocusChanging then Perform(WM_SETREDRAW, 0, 0);
        inherited;
        if not bFocusChanging then Perform(WM_SETREDRAW, 1, 0);
      end;     // Disabling of blinking
      WM_ENABLE : Exit;                      // Disabling of blinking when switched
    end;
    CommonWndProc(Message, FCommonData);
    inherited;
    case Message.Msg of
      CM_ENABLEDCHANGED : if (Visible or (csDesigning in ComponentState)) and not (csDestroying in ComponentState) then begin
        FCommonData.Updating := False;
        FCommonData.Invalidate;
      end;
      CM_VISIBLECHANGED : if not (csDestroying in ComponentState) and Visible then begin
        FCommonData.BGChanged := True;
        FCommonData.Updating := False;
        if Visible or (csDesigning in ComponentState) then Repaint;
      end;
      WM_SETFONT : if Visible or (csDesigning in ComponentState) then begin
        FCommonData.Updating := False;
        FCommonData.Invalidate;
      end;
      CM_ACTIONUPDATE : begin
        if (Action <> nil) then Enabled := TCustomAction(Action).Enabled;
      end;
    end;
  end;
end;

procedure TsBitBtn.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  Images := TCustomAction(Sender).ActionList.Images;
  ImageIndex := TCustomAction(Sender).ImageIndex;
//  if Sender is TCustomAction then DoActionChanging(Self, TCustomAction(Sender));
  if not (csDestroying in ComponentState) and not (csLoading in ComponentState) then FCommonData.Invalidate;
end;

procedure TsBitBtn.SetTextAlignment(const Value: TAlignment);
begin
  if FTextAlignment <> Value then begin
    FTextAlignment := Value;
    FCommonData.Invalidate
  end;
end;

procedure TsBitBtn.CNDrawItem(var Message: TWMDrawItem);
begin
  if not SkinData.Skinned and (IsImgListDefined(Self) or (Glyph.PixelFormat = pf32bit))// is TPNGGraphic))
    then StdDrawItem(Message.DrawItemStruct^)
    else inherited;
end;

procedure TsBitBtn.StdDrawItem(const DrawItemStruct: TDrawItemStruct);
var
  IsDown, IsDefault: Boolean;
//  State: TButtonState;
  R: TRect;
  Flags: Longint;
{$IFDEF DELPHI7UP}
  Details: TThemedElementDetails;
  Button: TThemedButton;
  Offset: TPoint;
{$ENDIF}
  Canvas : TCanvas;
begin
  Canvas := TCanvas.Create;
  Canvas.Handle := DrawItemStruct.hDC;
  R := ClientRect;

  with DrawItemStruct do begin
    Canvas.Handle := hDC;
    Canvas.Font := Self.Font;
    IsDown := itemState and ODS_SELECTED <> 0;
    IsDefault := itemState and ODS_FOCUS <> 0;
  end;

{$IFDEF DELPHI7UP}
  if ThemeServices.ThemesEnabled then
  begin
    if not Enabled then
      Button := tbPushButtonDisabled
    else
      if IsDown then
        Button := tbPushButtonPressed
      else
        if SkinData.FMouseAbove{ MouseInControl }then
          Button := tbPushButtonHot
        else
          if IsFocused or IsDefault then
            Button := tbPushButtonDefaulted
          else
            Button := tbPushButtonNormal;

    Details := ThemeServices.GetElementDetails(Button);
    // Parent background.
    ThemeServices.DrawParentBackground(Handle, DrawItemStruct.hDC, @Details, True);
    // Button shape.
    ThemeServices.DrawElement(DrawItemStruct.hDC, Details, DrawItemStruct.rcItem);
    R := ThemeServices.ContentRect(Canvas.Handle, Details, DrawItemStruct.rcItem);

    if Button = tbPushButtonPressed then
      Offset := Point(1, 0)
    else
      Offset := Point(0, 0);

    if IsFocused and IsDefault then
    begin
      Canvas.Pen.Color := clWindowFrame;
      Canvas.Brush.Color := clBtnFace;
      DrawFocusRect(Canvas.Handle, R);
    end;
  end
  else
{$ENDIF}
  begin
    R := ClientRect;

    Flags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
    if IsDown then Flags := Flags or DFCS_PUSHED;
    if DrawItemStruct.itemState and ODS_DISABLED <> 0 then
      Flags := Flags or DFCS_INACTIVE;

    { DrawFrameControl doesn't allow for drawing a button as the
        default button, so it must be done here. }
    if IsFocused or IsDefault then
    begin
      Canvas.Pen.Color := clWindowFrame;
      Canvas.Pen.Width := 1;
      Canvas.Brush.Style := bsClear;
      Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);

      { DrawFrameControl must draw within this border }
      InflateRect(R, -1, -1);
    end;
    
    { DrawFrameControl does not draw a pressed button correctly }
    if IsDown then
    begin
      Canvas.Pen.Color := clBtnShadow;
      Canvas.Pen.Width := 1;
      Canvas.Brush.Color := clBtnFace;
      Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      InflateRect(R, -1, -1);
    end
    else
      DrawFrameControl(DrawItemStruct.hDC, R, DFC_BUTTON, Flags);

    if IsFocused then
    begin
      R := ClientRect;
      InflateRect(R, -1, -1);
    end;

    Canvas.Font := Self.Font;
    if IsDown then OffsetRect(R, 1, 1);

    if IsFocused and IsDefault then
    begin
      R := ClientRect;
      InflateRect(R, -4, -4);
      Canvas.Pen.Color := clWindowFrame;
      Canvas.Brush.Color := clBtnFace;
      DrawFocusRect(Canvas.Handle, R);
    end;
  end;
  DrawCaption(Canvas);
  DrawBtnGlyph(Self, Canvas);

  Canvas.Handle := 0;
  Canvas.Free;
end;
    
end.
