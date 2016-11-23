unit sSpeedButton;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Imglist,
  Buttons, sCommonData, sConst, sDefaults, sFade, comctrls, menus
  {$IFDEF TNTUNICODE}, TntButtons{$ENDIF};

type

{$IFDEF TNTUNICODE}
  TsSpeedButton = class(TTntSpeedButton)
{$ELSE}
  TsSpeedButton = class(TSpeedButton)
{$ENDIF}
  private
{$IFNDEF NOTFORHELP}
    FOldNumGlyphs : integer;
    FOldSpacing : integer;

    FStoredDown : boolean;
    FCommonData: TsCommonData;
    FDisabledKind: TsDisabledKind;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FadeTimer : TsAnimTimer;
    FDisabledGlyphKind: TsDisabledGlyphKind;
    FGrayed: boolean;
    FBlend: integer;
    FOffset: Integer;
    FImageIndex: integer;
    FImages: TCustomImageList;
    FShowCaption: boolean;
    FAlignment: TAlignment;
    FTextLayout : integer;
    FButtonStyle: TToolButtonStyle;
    FDropdownMenu: TPopupMenu;
    FDrawOverBorder: boolean;
    FOnPaint: TBmpPaintEvent;
    FTextAlignment: TAlignment;
    FAnimatEvents: TacAnimatEvents;

    procedure SetDisabledKind(const Value: TsDisabledKind);
    procedure SetDisabledGlyphKind(const Value: TsDisabledGlyphKind);
    procedure SetBlend(const Value: integer);
    procedure SetGrayed(const Value: boolean);
    procedure SetOffset(const Value: Integer);
    procedure SetImageIndex(const Value: integer);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetShowCaption(const Value: boolean);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetButtonStyle(const Value: TToolButtonStyle);
    procedure SetDropdownMenu(const Value: TPopupMenu);
    procedure SetDrawOverBorder(const Value: boolean);
    procedure SetTextAlignment(const Value: TAlignment);
  private
    FReflected: boolean;
    procedure SetReflected(const Value: boolean);
  protected
    DroppedDown : boolean;
    OldOnChange: TNotifyEvent;
    OldLayout : TButtonLayout;
    OldCaption : acString;
    procedure SetFakeCaption;

    function ArrowWidth : integer;
    procedure DoDrawText(var Rect: TRect; Flags: Longint); virtual;
    procedure DrawCaption; virtual;
    function TextRectSize : TSize; virtual;
    procedure DrawGlyph; virtual;
    function GlyphWidth : integer; virtual;
    function GlyphHeight : integer; virtual;
    function GenMargin : integer;

    procedure PrepareCache; virtual;

    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure Ac_CMMouseEnter(var Message : TMessage);
    procedure Ac_CMMouseLeave(var Message : TMessage);
    procedure StdPaint(PaintButton : boolean = True); virtual;
    procedure Paint; override;
    procedure GlyphChanged(Sender: TObject);
    procedure MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    function CurrentState : integer; virtual;
    function CaptionRect : TRect; virtual;
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure GraphRepaint;
    procedure AfterConstruction; override;
    function ImgRect : TRect; virtual;
    procedure Invalidate; override;
    procedure Loaded; override;
    procedure WndProc (var Message: TMessage); override;
    property Canvas;
{$ENDIF} // NOTFORHELP
  published
{$IFNDEF NOTFORHELP}
    property Align;
    property OnPaint : TBmpPaintEvent read FOnPaint write FOnPaint;
{$ENDIF} // NOTFORHELP
    property OnMouseEnter : TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave : TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property AnimatEvents : TacAnimatEvents read FAnimatEvents write FAnimatEvents default [aeGlobalDef];
    property Alignment : TAlignment read FAlignment write SetAlignment default taCenter;
    property Blend : integer read FBlend write SetBlend default 0;
    property ButtonStyle : TToolButtonStyle read FButtonStyle write SetButtonStyle default tbsButton;
    property SkinData : TsCommonData read FCommonData write FCommonData;
    property DisabledGlyphKind : TsDisabledGlyphKind read FDisabledGlyphKind write SetDisabledGlyphKind default DefDisabledGlyphKind;
    property DisabledKind : TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property DropdownMenu: TPopupMenu read FDropdownMenu write SetDropdownMenu;
    property Grayed : boolean read FGrayed write SetGrayed default False;
    property ImageIndex : integer read FImageIndex write SetImageIndex default -1;
    property Images : TCustomImageList read FImages write SetImages;
    property Reflected : boolean read FReflected write SetReflected default False;
    property ShowCaption: boolean read FShowCaption write SetShowCaption default True;
    property DrawOverBorder : boolean read FDrawOverBorder write SetDrawOverBorder default True;
    property TextOffset : Integer read FOffset write SetOffset default 0; 
    property TextAlignment : TAlignment read FTextAlignment write SetTextAlignment default taCenter;
  end;

{$IFNDEF NOTFORHELP}
  TsTimerSpeedButton = class(TsSpeedButton)
  private
    FAllowTimer: boolean;
  public
    constructor Create (AOwner: TComponent); override;
  published
    property AllowTimer: boolean read FAllowTimer write FAllowTimer default True;
  end;
{$ENDIF} // NOTFORHELP

implementation

uses sGraphUtils, sVCLUtils, sMessages, acntUtils, sMAskData, sAlphaGraph, sStyleSimply, sSkinProps,
  sBitBtn, sThirdParty{$IFDEF LOGGED}, sDebugMsgs{$ENDIF}, ActnList, sSkinManager, sBorders{$IFDEF DELPHI7UP}, Themes{$ENDIF};

const AddedWidth = 16;

var
  MenuVisible : boolean = False;

function IsImgListDefined(Btn : TsSpeedButton) : boolean;
begin
  Result := Assigned(Btn.Images) and (GetImageCount(Btn.Images) > 0) and (Btn.ImageIndex >= 0) and (Btn.ImageIndex < GetImageCount(Btn.Images))
end;

procedure PaintParentControls(Button : TsSpeedButton; aCanvas : TCanvas);
var
  i : integer;
  R, cR : TRect;
  SavedDC : hdc;
begin
  with Button do begin
    R := BoundsRect;
    for i := 0 to Parent.ControlCount - 1 do if Parent.Controls[i] <> Button then begin
      cR := Parent.Controls[i].BoundsRect;
      if PtInRect(R, cR.TopLeft) or PtInRect(R, cR.BottomRight) then begin
        SavedDC := SaveDC(aCanvas.Handle);
        try
          aCanvas.Lock;
          Parent.Controls[i].ControlState := Parent.Controls[i].ControlState + [csPaintCopy];
          MoveWindowOrg(aCanvas.Handle, cR.Left - R.Left, cr.Top - R.Top);
          IntersectClipRect(aCanvas.Handle, 0, 0, Parent.Controls[i].Width, Parent.Controls[i].Height);
          Parent.Controls[i].Perform(WM_PAINT, longint(aCanvas.Handle), 0);
          Parent.Controls[i].ControlState := Parent.Controls[i].ControlState - [csPaintCopy];
          aCanvas.UnLock;
        finally
          RestoreDC(aCanvas.Handle, SavedDC);
        end;
      end;
    end;
  end;
end;

procedure StartFade(Button : TsSpeedButton; Clicked : boolean = False);
var
  i : integer;
begin
  with Button do if not (csDesigning in ComponentState) then begin
    if FadeTimer <> nil then begin
      i := FadeTimer.Iterations - FadeTimer.FadeLevel;
      FreeAndNil(FadeTimer);
    end
    else i := 1;
    if FCommonData.SkinManager.gd[FCommonData.SkinIndex].FadingEnabled and not FadingForbidden then begin
      if FadeTimer = nil then begin
        FadeTimer := TsAnimTimer.Create(nil);
        FadeTimer.Enabled := False;
        FadeTimer.OwnerData := FCommonData;
        if Clicked then FadeTimer.Iterations := FadeTimer.Iterations div 2;
        FadeTimer.FadeLevel := i;
        PaintParentControls(Button, FCommonData.FCacheBmp.Canvas);
        FadeTimer.BmpFrom.Assign(FCommonData.FCacheBmp); // Save current image
      end;
      PrepareCache;
      PaintParentControls(Button, Button.SkinData.FCacheBmp.Canvas);
      UpdateCorners(FCommonData, 1);
      FadeTimer.Enabled := True;
      FadeTimer.DoFade;
    end;
  end;
end;

procedure StopFading(Button : TsSpeedButton);
begin
  with Button do if not (csDestroying in ComponentState) and not (csDesigning in ComponentState) then begin
    if (FadeTimer <> nil) and not (csDestroying in FadeTimer.ComponentState) then begin
      FadeTimer.Enabled := False;
      if FadeTimer.FadeLevel <> 0 then begin
        FCommonData.BGChanged := True;
        Repaint;
//        GraphRepaint
      end;
    end;
  end;
  if Assigned(Button.FadeTimer)
    then FreeAndNil(Button.FadeTimer);
end;

procedure DoChangePaint(Button : TsSpeedButton; Clicked : boolean; AllowAnimation : boolean; Direction : TFadeDirection = fdUp);
begin
  if AllowAnimation and not aSkinchanging and Button.FCommonData.Skinned and Button.FCommonData.SkinManager.gd[Button.FCommonData.SkinIndex].FadingEnabled and
       not FadingForbidden and not Button.FCommonData.BGChanged then begin
    Button.FCommonData.BGChanged := True;
    StartFade(Button, Clicked)
  end
  else begin
    Button.FCommonData.BGChanged := True;
    if (Button.FadeTimer <> nil) and (Button.FadeTimer.Enabled) then StopFading(Button);
    Button.GraphRepaint;
  end;
  ShowGlowingIfNeeded(Button.FCommonData, Clicked);
end;

{ TsSpeedButton }

function MaxCaptionWidth(Button : TsSpeedButton) : integer;
begin
  with Button do begin
    if ShowCaption and (Caption <> '') then Result := Width - ArrowWidth - 2 * Margin - (Spacing + GlyphWidth) * integer(GlyphWidth <> 0) else Result := 0
  end;
end;

function TsSpeedButton.ArrowWidth: integer;
begin
  Result := AddedWidth * integer(ButtonStyle = tbsDropDown);
end;

procedure TsSpeedButton.AfterConstruction;
begin
  inherited;
  FCommonData.Loaded;
  if FCommonData.Skinned then ControlStyle := ControlStyle + [csOpaque]; 
end;

function TsSpeedButton.CaptionRect: TRect;
var
  l, t, r, b : integer;
  dh, dw : integer;
  Size : TSize;
begin
  l := 0; t := 0; r := 0; b := 0;
  Size := TextRectSize;
  case Layout of
    blGlyphLeft : begin
      dw := (Width - ArrowWidth - GlyphWidth - Spacing * integer((GlyphWidth > 0) and (Caption <> '')) - Size.cx) div 2 - GenMargin;
      t := (Height - Size.cy) div 2;
      b := Height - t;
      case Alignment of
        taLeftJustify : begin
          l := Margin + GlyphWidth + Spacing * integer(GlyphWidth > 0);
          r := Width - ArrowWidth - GenMargin - dw * 2;
        end;
        taCenter : begin
          l := GenMargin + dw + GlyphWidth + Spacing * integer(GlyphWidth > 0);
          r := Width - ArrowWidth - GenMargin - dw;
        end;
        taRightJustify : begin
          l := GenMargin + 2 * dw + GlyphWidth + Spacing * integer(GlyphWidth > 0);
          r := Width - ArrowWidth - GenMargin;
        end;
      end;
      FTextLayout := DT_LEFT;
    end;
    blGlyphRight : begin
      dw := (Width - ArrowWidth - GlyphWidth - Spacing * integer((GlyphWidth > 0) and (Caption <> '')) - Size.cx) div 2 - GenMargin;
      t := (Height - Size.cy) div 2;
      b := Height - t;
      case Alignment of
        taLeftJustify : begin
          l := GenMargin;
          r := GenMargin + Size.cx
        end;
        taCenter : begin
          l := GenMargin + dw;
          r := GenMargin + dw + Size.cx
        end;
        taRightJustify : begin
          l := GenMargin + 2 * dw;
          r := GenMargin + 2 * dw + Size.cx
        end;
      end;
      FTextLayout := DT_RIGHT;
    end;
    blGlyphTop : begin
      dh := (Height - GlyphHeight - Spacing * integer((GlyphHeight > 0) and (Caption <> '')) - Size.cy) div 2 - GenMargin;
      l := (Width - Size.cx - ArrowWidth) div 2;
      t := (GenMargin + dh + GlyphHeight + Spacing * integer((GlyphHeight > 0) and (Caption <> '')));
      r := Width - (Width - Size.cx - ArrowWidth) div 2 - ArrowWidth;
      b := Height - dh - GenMargin;
      FTextLayout := DT_CENTER;
    end;
    blGlyphBottom : begin
      dh := (Height - GlyphHeight - Spacing * integer((GlyphHeight > 0) and (Caption <> '')) - Size.cy) div 2 - GenMargin;
      l := (Width - Size.cx - ArrowWidth) div 2;
      t := GenMargin + dh;
      r := Width - (Width - Size.cx - ArrowWidth) div 2 - ArrowWidth;
      b := Height - dh - GenMargin - GlyphHeight - Spacing * integer((GlyphHeight > 0) and (Caption <> ''));
      FTextLayout := DT_CENTER;
    end;
  end;
  Result := Rect(l - 1 + FOffset, t, r + 2 + FOffset, b);
  if CurrentState = 2 then OffsetRect(Result, 1, 1);
end;

constructor TsSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommonData := TsCommonData.Create(Self, True);
  FCommonData.COC := COC_TsSPEEDBUTTON;
  FadeTimer := nil;

  FButtonStyle := tbsButton;
  FImageIndex := -1;
  FGrayed := False;
  FBlend := 0;
  FDisabledGlyphKind := DefDisabledGlyphKind;
  FDisabledKind := DefDisabledKind;
  FOffset := 0;
  FAlignment := taCenter;
  FShowCaption := True;
  FDrawOverBorder := True;
  FTextAlignment := taCenter;
  OldOnChange := Glyph.OnChange;
  Glyph.OnChange := GlyphChanged;
  FAnimatEvents := [aeGlobalDef];
end;

function TsSpeedButton.CurrentState: integer;
begin
  if FState in [bsDown, bsExclusive] then Result := 2 else if ControlIsActive(FCommonData) then Result := 1 else Result := 0
end;

destructor TsSpeedButton.Destroy;
begin
  StopFading(Self);
  if Assigned(FCommonData) then FreeAndNil(FCommonData);
  inherited Destroy;
end;

procedure TsSpeedButton.DoDrawText(var Rect: TRect; Flags: Integer);
begin
  Flags := DrawTextBiDiModeFlags(Flags) and not DT_SINGLELINE;
  if SkinData.Skinned then begin
    FCommonData.FCacheBMP.Canvas.Font.Assign(Font);
    acWriteTextEx(FCommonData.FCacheBMP.Canvas, PacChar(Caption), True, Rect, Flags, FCommonData, CurrentState <> 0);
  end
  else begin
    Canvas.Brush.Style := bsClear;
    if not Enabled then begin
      OffsetRect(Rect, 1, 1);
      Canvas.Font.Color := clBtnHighlight;
      acDrawText(Canvas.Handle, PacChar(Caption), Rect, Flags);
      OffsetRect(Rect, -1, -1);
      Canvas.Font.Color := clBtnShadow;
      acDrawText(Canvas.Handle, PacChar(Caption), Rect, Flags);
    end
    else acDrawText(Canvas.Handle, PacChar(Caption), Rect, Flags);
  end;
end;

procedure TsSpeedButton.DrawCaption;
var
  R, CalcRect : TRect;
begin
  if ShowCaption then begin
    FCommonData.FCacheBmp.Canvas.Font.Assign(Font);
    FCommonData.FCacheBMP.Canvas.Brush.Style := bsClear;
    R := CaptionRect;
    { Calculate vertical layout }
    CalcRect := R;
    DoDrawText(R, DT_EXPANDTABS or DT_WORDBREAK or GetStringFlags(Self, FTextAlignment));
  end;
end;

procedure TsSpeedButton.DrawGlyph;
begin
  if SkinData.Skinned
    then DrawBtnGlyph(Self, SkinData.FCacheBmp.Canvas)
    else DrawBtnGlyph(Self, Canvas);
end;

function TsSpeedButton.GenMargin: integer;
begin
  if Margin < 0 then Result := 0 else Result := Margin + 3;
end;

function TsSpeedButton.GlyphHeight: integer;
begin
  if IsImgListDefined(Self) then begin
    Result := Images.Height;
  end
  else if (Glyph <> nil) and (Glyph.Height > 0) then begin
    Result := Glyph.Height;
  end
  else Result := 0;
end;

function TsSpeedButton.GlyphWidth: integer;
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

procedure TsSpeedButton.GraphRepaint;
begin
  if (csCreating in ControlState) or (csDestroying in ComponentState) or (csLoading in ComponentState) or not Assigned(Parent) or not Visible then Exit;
  if Parent.HandleAllocated then begin
    if Skindata.Skinned and not (csDesigning in ComponentState) then begin
      if FCommonData.BGChanged then begin
        PrepareCache;
        if not (csPaintCopy in ControlState)
          then PaintParentControls(Self, FCommonData.FCacheBmp.Canvas)
          else Alert;
      end;

      if not ((FadeTimer <> nil) and FadeTimer.Enabled)
        then BitBlt(Canvas.Handle, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY)
        else BitBlt(Canvas.Handle, 0, 0, Width, Height, FadeTimer.TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);
    end
    else begin
      Repaint
    end;
{
    SendMessage(Parent.Handle, SM_ALPHACMD, MakeWParam(0, AC_SETGRAPHCONTROL), longint(Self));
    Repaint;
    SendMessage(Parent.Handle, SM_ALPHACMD, MakeWParam(0, AC_SETGRAPHCONTROL), 0);
}
  end;
end;

function TsSpeedButton.ImgRect: TRect;
var
  x, y : integer;
  dh, dw : integer;
begin
  x := 0;
  y := 0;
  Result := Rect(0, 0, 0, 0);
  dw := (Width - ArrowWidth - GlyphWidth - Spacing * integer(ShowCaption and (GlyphWidth > 0) and (Caption <> '')) - TextRectSize.cx) div 2 - GenMargin;
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
        x := Width - ArrowWidth - GenMargin - 2 * dw - Spacing * integer(ShowCaption and (GlyphWidth > 0) and (Caption <> '')) - GlyphWidth;
        y := (Height - GlyphHeight) div 2;
      end;
      taCenter : begin
        x := (Width - ArrowWidth - GlyphWidth + Spacing * integer(ShowCaption and (GlyphWidth > 0) and (Caption <> '')) + TextRectSize.cx) div 2;
        y := (Height - GlyphHeight) div 2;
      end;
      taRightJustify : begin
        x := Width - ArrowWidth - GlyphWidth - GenMargin;
        y := (Height - GlyphHeight) div 2;
      end;
    end;
    blGlyphTop : begin
      x := (Width - ArrowWidth - GlyphWidth) div 2 + 1;
      y := GenMargin + dh;
    end;
    blGlyphBottom : begin
      x := (Width - ArrowWidth - GlyphWidth) div 2 + 1;
      y := Height - GenMargin - dh - GlyphHeight;
    end;
  end;
  if CurrentState = 2 then begin
    inc(x);
    inc(y);
  end;
  Result := Rect(x, y, x + GlyphWidth, y + GlyphHeight);
  if Reflected then OffsetRect(Result, 0, - WidthOf(Result) div 6);
end;

procedure TsSpeedButton.Loaded;
begin
  inherited;
  FCommonData.Loaded;
  if FCommonData.Skinned then ControlStyle := ControlStyle + [csOpaque];
  if IsImgListDefined(Self) then Glyph.Assign(nil);
end;

procedure TsSpeedButton.Paint;
begin
  if Assigned(FCommonData.SkinManager) and FCommonData.SkinManager.Active and (FCommonData.SkinIndex < 1) then FCommonData.UpdateIndexes;
  if not FCommonData.Skinned then begin
    StdPaint;
  end
  else begin
    if (width < 1) or (height < 1) then Exit;
    if Assigned(FadeTimer) and FadeTimer.Enabled and Assigned(FadeTimer.TmpBmp) and (FadeTimer.TmpBmp.Width = Width) then begin
      BitBlt(Canvas.Handle, 0, 0, Width, Height, FadeTimer.TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);
    end
    else begin
{$IFDEF D2005}
      if CurrentState > 0 then FCommonData.BGChanged := True else
{$ENDIF}
      FCommonData.BGChanged := (FStoredDown <> Down) or FCommonData.BGChanged or FCommonData.HalfVisible;
      FStoredDown := Down;
      FCommonData.HalfVisible := SkinData.RepaintIfMoved;
      if not FCommonData.BGChanged then begin
        if (FOldNumGlyphs <> NumGlyphs) then begin
          FCommonData.BGChanged := True;
          FOldNumGlyphs := NumGlyphs;
        end
        else if (FOldSpacing <> Spacing) then begin
          FCommonData.BGChanged := True;
          FOldSpacing := Spacing;
        end;
      end
      else begin
        FOldNumGlyphs := NumGlyphs;
        FOldSpacing := Spacing;
      end;
      if FCommonData.BGChanged and not FCommonData.UrgentPainting then PrepareCache;
      UpdateCorners(FCommonData, 0);
      BitBlt(Canvas.Handle, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
    end;
  end;
end;

procedure TsSpeedButton.PrepareCache;
var
  CI : TCacheInfo;
  ParentBG : TacBGInfo;
  si, mi, w : integer;
  Mode, x, y : integer;
  R : TRect;
begin
  if Self = nil then Exit;
  InitCacheBmp(SkinData);
  FCommonData.FCacheBmp.Canvas.Font.Assign(Font);

  ParentBG.BgType := btUnknown;
  ParentBG.DrawDC := 0;
  ParentBG.PleaseDraw := False;
  GetBGInfo(@ParentBG, Parent);
  CI := BGInfoToCI(@ParentBG);
  if CI.Ready and (CI.Bmp.Width = 0) then Exit;

  case FButtonStyle of
    tbsDivider: begin
      if CI.Ready
        then BitBlt(FCommonData.FCacheBMP.Canvas.Handle, 0, 0, Width, Height, CI.Bmp.Canvas.Handle, Left + CI.X, Top + CI.Y, SRCCOPY)
        else FillDC(FCommonData.FCacheBMP.Canvas.Handle, ClientRect, CI.FillColor);
      si := FCommonData.SkinManager.GetSkinIndex(s_Divider);
      if FCommonData.SkinManager.IsValidSkinIndex(si) then begin
        mi := FCommonData.SkinManager.GetMaskIndex(si, s_Divider, s_BordersMask);
        if Assigned(FCommonData.SkinManager) and FCommonData.SkinManager.IsValidImgIndex(mi) then begin
          w := FCommonData.SkinManager.MaskSize(mi).cx;
          DrawSkinRect(FCommonData.FCacheBmp, Rect((Width - w) div 2, 0, (Width + w) div 2, Height), True, CI, FCommonData.SkinManager.ma[mi], 0, False);
        end;
      end
      else begin
      end;
    end;
    tbsSeparator: begin
      if CI.Ready
        then BitBlt(FCommonData.FCacheBMP.Canvas.Handle, 0, 0, Width, Height, CI.Bmp.Canvas.Handle, Left, Top, SRCCOPY)
        else FillDC(FCommonData.FCacheBMP.Canvas.Handle, ClientRect, CI.FillColor);
    end
    else begin
      if not FDrawOverBorder then begin
        PaintItemBG(FCommonData, ci, CurrentState, Rect(0, 0, Width - ArrowWidth, Height), Point(Left, Top), FCommonData.FCacheBMP, integer(Down), integer(Down));
        DrawCaption;
        DrawGlyph;
        Mode := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinIndex, FCommonData.SkinSection, s_BordersMask);
        inc(ci.X, Left);
        inc(ci.Y, Top);
        if FCommonData.SkinManager.IsValidImgIndex(Mode) then DrawSkinRect(FCommonData.FCacheBMP, Rect(0, 0, Width - ArrowWidth, Height), True, ci, FCommonData.SkinManager.ma[Mode], CurrentState, False);
      end
      else begin
        PaintItem(FCommonData, CI, True, CurrentState, Rect(0, 0, Width - ArrowWidth, Height), Point(Left, Top), FCommonData.FCacheBMP, True, integer(Down), integer(Down));
        UpdateCorners(SkinData, CurrentState);
        DrawCaption;
        DrawGlyph;
      end;
      if FButtonStyle = tbsDropDown then begin
        if ((Assigned(DropDownMenu) and DroppedDown) or Down or (FState in [bsDown, bsExclusive])) then Mode := 2 else if ControlIsActive(FCommonData) then Mode := 1 else Mode := 0;
        if not FDrawOverBorder then begin
          PaintItemBG(FCommonData, ci, Mode, Rect(Width - ArrowWidth, 0, Width, Height), Point(Left + Width - ArrowWidth, Top), FCommonData.FCacheBMP, integer(Down), integer(Down));
          inc(ci.X, Left);
          inc(ci.Y, Top);
          w := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinIndex, FCommonData.SkinSection, s_BordersMask);
          if FCommonData.SkinManager.IsValidImgIndex(Mode) then DrawSkinRect(FCommonData.FCacheBMP, Rect(Width - ArrowWidth, 0, Width, Height), True, ci, FCommonData.SkinManager.ma[w], Mode, False);
        end
        else begin
          PaintItem(FCommonData, CI, True, Mode, Rect(Width - ArrowWidth, 0, Width, Height), Point(Left + Width - ArrowWidth, Top), FCommonData.FCacheBMP, True, integer(Down), integer(Down));
        end;
        if FCommonData.SkinManager.ConstData.MaskArrowBottom > -1 then with FCommonData.SkinManager do begin

          R.Left := Width - ArrowWidth;
          R.Right := Width;

          x := R.Left + (ArrowWidth - WidthOf(FCommonData.SkinManager.ma[ConstData.MaskArrowBottom].R) div FCommonData.SkinManager.ma[ConstData.MaskArrowBottom].ImageCount) div 2 + 1;// + 2;
          y := (Height - HeightOf(ma[ConstData.MaskArrowBottom].R) div (1 + ma[ConstData.MaskArrowBottom].MaskType)) div 2;

          DrawSkinGlyph(FCommonData.FCacheBmp, Point(x, y), Mode, 1, ma[ConstData.MaskArrowBottom], MakeCacheInfo(FCommonData.FCacheBmp));
        end;
      end;
      if not Enabled then begin
        CI := GetParentCache(FCommonData);
        BmpDisabledKind(FCommonData.FCacheBmp, FDisabledKind, Parent, CI, Point(Left, Top));
      end;
      if Assigned(FOnPaint) then FOnPaint(Self, FCommonData.FCacheBmp);
    end;
  end;
  FCommonData.BGChanged := False;
end;

procedure TsSpeedButton.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    if Visible or (csDesigning in ComponentState) then begin
      FCommonData.BGChanged := True;
      GraphRepaint;
    end;
  end;
end;

procedure TsSpeedButton.SetBlend(const Value: integer);
begin
  if FBlend <> Value then begin
    if Value < 0 then FBlend := 0 else if Value > 100 then FBlend := 100 else FBlend := Value;
    if (Visible or (csDesigning in ComponentState)) and (SkinData.CtrlSkinState and ACS_LOCKED <> ACS_LOCKED) then begin
      FCommonData.BGChanged := True;
      GraphRepaint;
    end;
  end;
end;

procedure TsSpeedButton.SetButtonStyle(const Value: TToolButtonStyle);
begin
  if FButtonStyle <> Value then begin
    if not (csLoading in ComponentState) then begin
      if Value = tbsDropDown then Width := Width + AddedWidth else if FButtonStyle = tbsDropdown then Width := Width - AddedWidth;
    end;
    FButtonStyle := Value;
    if Visible or (csDesigning in ComponentState) then begin
      FCommonData.BGChanged := True;
      GraphRepaint;
    end;
  end;
end;

procedure TsSpeedButton.SetDisabledGlyphKind(const Value: TsDisabledGlyphKind);
begin
  if FDisabledGlyphKind <> Value then begin
    FDisabledGlyphKind := Value;
    if Visible or (csDesigning in ComponentState) then begin
      FCommonData.BGChanged := True;
      GraphRepaint;
    end;
  end;
end;

procedure TsSpeedButton.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    if Visible or (csDesigning in ComponentState) then begin
      FCommonData.BGChanged := True;
      GraphRepaint;
    end;
  end;
end;

procedure TsSpeedButton.SetGrayed(const Value: boolean);
begin
  if FGrayed <> Value then begin
    FGrayed := Value;
    if (Visible or (csDesigning in ComponentState)) and (SkinData.CtrlSkinState and ACS_LOCKED <> ACS_LOCKED) then begin
      FCommonData.BGChanged := True;
      GraphRepaint;
    end;
  end;
end;

procedure TsSpeedButton.SetImageIndex(const Value: integer);
begin
  if FImageIndex <> Value then begin
    FImageIndex := Value;
    if (Visible or (csDesigning in ComponentState)) and (SkinData.CtrlSkinState and ACS_LOCKED <> ACS_LOCKED) then begin
      FCommonData.BGChanged := True;
      GraphRepaint;
    end;
  end;
end;

procedure TsSpeedButton.SetImages(const Value: TCustomImageList);
begin
  if Images <> Value then begin
    FImages := Value;
    if (Visible or (csDesigning in ComponentState)) and (SkinData.CtrlSkinState and ACS_LOCKED <> ACS_LOCKED) then begin
      FCommonData.BGChanged := True;
      GraphRepaint;
    end;
  end;
end;

procedure TsSpeedButton.SetOffset(const Value: Integer);
begin
  if (FOffset <> Value) then begin
    FOffset := Value;
    if Visible or (csDesigning in ComponentState) then begin
      FCommonData.BGChanged := True;
      GraphRepaint;
    end;
  end;
end;

procedure TsSpeedButton.SetShowCaption(const Value: boolean);
begin
  if FShowCaption <> Value then begin
    FShowCaption := Value;
    SetFakeCaption;
    if Visible or (csDesigning in ComponentState) then begin
      FCommonData.BGChanged := True;
      GraphRepaint;
    end;
  end;
end;

function TsSpeedButton.TextRectSize: TSize;
var
  R : TRect;
begin
  R := Rect(0, 0, MaxCaptionWidth(Self), 0);
  acDrawText(FCommonData.FCacheBMP.Canvas.Handle, Caption, R, DT_EXPANDTABS or DT_WORDBREAK or DT_CALCRECT);
  Result.cy := HeightOf(R);
  Result.cx := WidthOf(R);
end;

procedure TsSpeedButton.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_CTRLHANDLED : begin Message.Result := 1; Exit end;
    AC_GETAPPLICATION : begin Message.Result := longint(Application); Exit end;
    AC_SETNEWSKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FCommonData);
      if FCommonData.Skinned then ControlStyle := ControlStyle + [csOpaque];
      Exit;
    end;
    AC_REMOVESKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) and not (csDestroying in ComponentState) then begin
      CommonWndProc(Message, FCommonData);
      ControlStyle := ControlStyle - [csOpaque];
      if Visible then Repaint;
    end;
    AC_ENDPARENTUPDATE : if FCommonData.Updating then begin
      FCommonData.Updating := False;
      GraphRepaint;
      Exit;
    end;
    AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FCommonData);
      if Visible then RePaint;
      exit
    end;
    AC_INVALIDATE : begin
      FCommonData.FUpdating := False;
      FCommonData.BGChanged := True;
      GraphRepaint;
    end;
  end;
  if not ControlIsReady(Self) or not FCommonData.Skinned then begin
    case Message.Msg of
      CM_MOUSEENTER : if Enabled and not (csDesigning in ComponentState) then begin
        FCommonData.FMouseAbove := True;
        if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
      end;
      CM_MOUSELEAVE : if Enabled and not (csDesigning in ComponentState) then begin
        FCommonData.FMouseAbove := False;
        if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
        if bsDown = FState then FState := bsUp;
      end;
    end;
    inherited
  end
  else begin
    if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
      AC_UPDATESECTION : begin GraphRepaint; Exit end;
      AC_STOPFADING : begin StopFading(Self); Exit end;
      AC_PREPARING : begin
        Message.Result := integer(FCommonData.FUpdating);
        Exit;
      end;
    end
    else case Message.Msg of
      CM_MOUSEENTER : if Enabled and not MouseInControl and not (csDesigning in ComponentState) then begin
        Ac_CMMouseEnter(Message);
      end;
      CM_MOUSELEAVE : if Enabled then begin
        Ac_CMMouseLeave(Message);
      end;
      WM_ERASEBKGND : Exit;
{$IFNDEF DYNAMICCACHE}
      CM_TEXTCHANGED : if ShowCaption then begin
        FCommonData.BGChanged := True;
        GraphRepaint;
        Exit;
      end;
{$ENDIF}
      WM_WINDOWPOSCHANGED, WM_SIZE : begin
        if Visible then FCommonData.BGChanged := True;
      end;
      WM_LBUTTONUP : if not (csDesigning in ComponentState) then begin
        if Assigned(FadeTimer) and FadeTimer.Enabled and Assigned(FadeTimer.TmpBmp) and (FadeTimer.TmpBmp.Width = Width) then begin
          StopFading(Self);
          PrepareCache;
          GraphRepaint; // Fast repainting if clicked quickly
        end;
      end;
    end;
    CommonWndProc(Message, FCommonData);
    inherited;
    case Message.Msg of
      CM_BUTTONPRESSED : begin // If changed when GroupIndex <> 0
        FCommonData.BGChanged := True;
        GraphRepaint;
      end;
      CM_VISIBLECHANGED : begin
        SkinData.FMouseAbove := False
      end;
      WM_LBUTTONDBLCLK, WM_LBUTTONDOWN : if not (csDesigning in ComponentState) then begin
        DoChangePaint(Self, True, EventEnabled(aeMouseDown, FAnimatEvents));
      end;
      WM_LBUTTONUP : if not (csDesigning in ComponentState) and not (csDestroying in ComponentState) then begin
        DoChangePaint(Self, True, EventEnabled(aeMouseUp, FAnimatEvents));
      end
      else begin
        Message.Result := 1;
      end;
      CM_ENABLEDCHANGED : if (Visible or (csDesigning in ComponentState)) then begin
        if Visible then FCommonData.BGChanged := True;
        GraphRepaint;
        Exit;
      end;
      WM_MOVE : begin
        if (csDesigning in ComponentState) and not SkinData.Updating then Repaint
      end;
      WM_SIZE, WM_WINDOWPOSCHANGED : if (csDesigning in ComponentState) and not SkinData.Updating then begin
        GraphRepaint;
      end;
    end;
  end;
end;

procedure TsSpeedButton.SetDropdownMenu(const Value: TPopupMenu);
begin
  if Value <> FDropdownMenu then begin
    FDropdownMenu := Value;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;

procedure TsSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  p : TPoint;
  c : TMouse;
begin
  if not ShowHintStored then begin
    AppShowHint := Application.ShowHint;
    Application.ShowHint := False;
    ShowHintStored := True;
  end;

  if (Button = mbLeft) and Enabled then begin
    if (ButtonStyle = tbsDropDown) and Assigned(DropDownMenu) and ((X > Width - AddedWidth) or not Assigned(OnCLick)) then begin
      TempControl := pointer(Self);

      c := nil;
      StopFading(Self);

      if not MenuVisible then begin
        MenuVisible := True;
        DroppedDown := True;
        FCommonData.BGChanged := True;
        if not Assigned(OnCLick) then FState := bsDown;
        GraphRepaint;
        p := ClientToScreen(Point(0, Height + 1));
        DropDownMenu.PopupComponent := Self;
        DropDownMenu.Popup(p.X, p.Y);
        DroppedDown := False;
        MenuVisible := False;
        TempControl := nil;
        if not PtInRect(Rect(p.x, p.y - Height - 1, p.x + Width, p.y - 1), c.CursorPos) then begin
          Perform(CM_MOUSELEAVE, 0, 0);
        end;
        if not Assigned(OnCLick) then FState := bsUp;
      end;
    end
    else inherited;
  end else inherited;
end;

procedure TsSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Application.ShowHint := AppShowHint;
  ShowHintStored := False;
  if (Button = mbLeft)  and Enabled and (ButtonStyle = tbsDropDown) then begin
    DroppedDown := False;
    TempControl := nil;
    inherited;
  end
  else inherited;
end;

procedure TsSpeedButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Images) then Images := nil;
end;

procedure TsSpeedButton.SetDrawOverBorder(const Value: boolean);
begin
  if FDrawOverBorder <> Value then begin
    FDrawOverBorder := Value;
    if Visible or (csDesigning in ComponentState) then begin
      FCommonData.BGChanged := True;
      GraphRepaint;
    end;
  end;
end;

procedure TsSpeedButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  Images := TCustomAction(Sender).ActionList.Images;
  ImageIndex := TCustomAction(Sender).ImageIndex;
  SetFakeCaption;
  FCommonData.BGChanged := True;
  if not (csDestroying in ComponentState) and not (csLoading in ComponentState) then Repaint;
end;

procedure TsSpeedButton.GlyphChanged(Sender: TObject);
begin
  if Assigned(OldOnChange) then OldOnChange(Glyph);
  if not (csLoading in ComponentState) and not (csDestroying in ComponentState) and not (csCreating in ControlState)
    then FCommonData.Invalidate;
end;

procedure TsSpeedButton.SetTextAlignment(const Value: TAlignment);
begin
  if FTextAlignment <> Value then begin
    FTextAlignment := Value;
    Repaint
  end;
end;

procedure TsSpeedButton.Ac_CMMouseEnter(var Message: TMessage);
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
  if not FCommonData.FMouseAbove then begin
    FCommonData.FMouseAbove := True;
    DoChangePaint(Self, False, EventEnabled(aeMouseEnter, FAnimatEvents));
  end;
end;

procedure TsSpeedButton.Ac_CMMouseLeave(var Message: TMessage);
begin
  if FCommonData.FMouseAbove then begin
    if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
    FCommonData.FMouseAbove := False;
    if bsDown = FState then FState := bsUp;
    if Visible then DoChangePaint(Self, False, EventEnabled(aeMouseLeave, FAnimatEvents));
  end;
end;

procedure TsSpeedButton.Invalidate;
begin
  if (OldLayout <> Layout) then begin
    OldLayout := Layout;
    FCommonData.BGChanged := True;
  end;
  inherited;
end;

procedure TsSpeedButton.SetFakeCaption;
begin
  if Caption <> '' then OldCaption := Caption;
  if not FShowCaption then Caption := '' else Caption := OldCaption;
end;

procedure TsSpeedButton.StdPaint(PaintButton : boolean = True);
const
  DownStyles: array[Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
  FillStyles: array[Boolean] of Integer = (BF_MIDDLE, 0);
var
  PaintRect: TRect;
  DrawFlags: Integer;
  Offset: TPoint;
{$IFDEF DELPHI7UP}
  ArrowPoints: array of TPoint;
  Button: TThemedButton;
  ToolButton: TThemedToolBar;
  Details: TThemedElementDetails;
{$ENDIF}
begin
  if not Enabled then begin
    FState := bsDisabled;
  end
  else if FState = bsDisabled then if Down and (GroupIndex <> 0) then FState := bsExclusive else FState := bsUp;
  Canvas.Font := Self.Font;
  if PaintButton then begin
{$IFDEF DELPHI7UP}
    if ButtonStyle = tbsDropDown then begin
      PaintRect := ClientRect;
      PaintRect.Right := PaintRect.Right - ArrowWidth;
    end
    else begin
      PaintRect := ClientRect;
    end;
    if ThemeServices.ThemesEnabled then begin
      PerformEraseBackground(Self, Canvas.Handle);

      if not Enabled then Button := tbPushButtonDisabled else if FState in [bsDown, bsExclusive] then Button := tbPushButtonPressed else
          if MouseInControl then Button := tbPushButtonHot else Button := tbPushButtonNormal;
      ToolButton := ttbToolbarDontCare;
      if Flat then case Button of
        tbPushButtonDisabled: Toolbutton := ttbButtonDisabled;
        tbPushButtonPressed:  Toolbutton := ttbButtonPressed;
        tbPushButtonHot:      Toolbutton := ttbButtonHot;
        tbPushButtonNormal:   Toolbutton := ttbButtonNormal;
      end;
      if ToolButton = ttbToolbarDontCare then begin
        Details := ThemeServices.GetElementDetails(Button);
        ThemeServices.DrawElement(Canvas.Handle, Details, PaintRect);
        PaintRect := ThemeServices.ContentRect(Canvas.Handle, Details, PaintRect);
      end
      else begin
        Details := ThemeServices.GetElementDetails(ToolButton);
        ThemeServices.DrawElement(Canvas.Handle, Details, PaintRect);
        PaintRect := ThemeServices.ContentRect(Canvas.Handle, Details, PaintRect);
        if ButtonStyle = tbsDropDown then begin
          PaintRect := ClientRect;
          PaintRect.Left := PaintRect.Right - ArrowWidth;
          ThemeServices.DrawElement(Canvas.Handle, Details, PaintRect);
          SetLength(ArrowPoints, 3);
          ArrowPoints[0] := Point((WidthOf(PaintRect) - 5) div 2 + PaintRect.Left + integer(Toolbutton = ttbButtonPressed), (HeightOf(PaintRect) - 5) div 2 + PaintRect.Top + integer(Toolbutton = ttbButtonPressed));
          ArrowPoints[1] := Point(ArrowPoints[0].X + 5, ArrowPoints[0].Y);
          ArrowPoints[2] := Point(ArrowPoints[0].X + 2, ArrowPoints[0].Y + 5);
          Canvas.Brush.Style := bsSolid;
          Canvas.Brush.Color := clBtnText;
          Canvas.Pen.Color := clBtnText;
          Canvas.Polygon(ArrowPoints);
        end
      end;

      if Button = tbPushButtonPressed then begin
        // A pressed speed button has a white text. This applies however only to flat buttons.
        if ToolButton <> ttbToolbarDontCare then Canvas.Font.Color := clHighlightText;
        Offset := Point(1, 0);
      end
      else Offset := Point(0, 0);
    end
    else
{$ENDIF}
    begin
      PaintRect := Rect(0, 0, Width, Height);
      if not Flat then begin
        DrawFlags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
        if FState in [bsDown, bsExclusive] then DrawFlags := DrawFlags or DFCS_PUSHED;
        DrawFrameControl(Canvas.Handle, PaintRect, DFC_BUTTON, DrawFlags);
      end
      else begin
        if (FState in [bsDown, bsExclusive]) or (MouseInControl and (FState <> bsDisabled)) or (csDesigning in ComponentState)
          then DrawEdge(Canvas.Handle, PaintRect, DownStyles[FState in [bsDown, bsExclusive]], FillStyles[Transparent] or BF_RECT)
          else if not Transparent then begin
            Canvas.Brush.Color := Color;
            Canvas.FillRect(PaintRect);
        end;
        InflateRect(PaintRect, -1, -1);
      end;
      if FState in [bsDown, bsExclusive] then begin
        if (FState = bsExclusive) and (not Flat or not MouseInControl) then begin
          Canvas.Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight);
          Canvas.FillRect(PaintRect);
        end;
        Offset := Point(1, 1);
      end
      else Offset := Point(0, 0);
    end;
  end;

  DrawCaption;
  DrawGlyph;
end;

procedure TsSpeedButton.SetReflected(const Value: boolean);
begin
  if FReflected <> Value then begin
    FReflected := Value;
    FCommonData.BGChanged := True;
    GraphRepaint;
  end;
end;

{ TsTimerSpeedButton }

constructor TsTimerSpeedButton.Create(AOwner: TComponent);
begin
  inherited;
  Width := Height - 4;
end;

end.
