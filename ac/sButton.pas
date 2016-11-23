unit sButton;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, sCommonData, Buttons, sConst, sDefaults, sFade{$IFDEF LOGGED}, sDebugMsgs{$ENDIF}
  {$IFDEF TNTUNICODE}, TntStdCtrls {$ENDIF};

type
{$IFDEF TNTUNICODE}
  TsButton = class(TTntButton)
{$ELSE}
  TsButton = class(TButton)
{$ENDIF}
  private
{$IFNDEF NOTFORHELP}
    FCommonData: TsCommonData;
    FMouseClicked : boolean;
    FDown: boolean;
    RegionChanged : boolean;
    FFocusMargin: integer;
    FDisabledKind: TsDisabledKind;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FadeTimer : TsFadeTimer;
    FAnimatEvents: TacAnimatEvents;
    LastRect : TRect;
    FShowFocus: boolean;
{$IFNDEF DELPHI7UP}
    FWordWrap : boolean;
    procedure SetWordWrap(const Value: boolean);
{$ENDIF}
    procedure SetDown(const Value: boolean);
    procedure SetFocusMargin(const Value: integer);
    procedure SetDisabledKind(const Value: TsDisabledKind);
    procedure WMKeyUp (var Message: TWMKey); message WM_KEYUP;
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    function GetDown: boolean;
    procedure SetShowFocus(const Value: boolean);
  protected
    IsFocused : boolean;
    FRegion : hrgn;

    procedure SetButtonStyle(ADefault: Boolean); override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure OurPaintHandler(aDC : hdc);
    procedure DoDrawText(var Rect: TRect; Flags: Longint); dynamic;
    procedure DrawCaption; dynamic;
    function CaptionRect : TRect; dynamic;
    function TextRectSize : TSize;
    function CurrentState : integer;

    procedure PrepareCache;
  public
    Active: Boolean;
    constructor Create(AOwner:TComponent); override;

    procedure CreateParams(var Params: TCreateParams); override;

    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Loaded; override;
    procedure WndProc (var Message: TMessage); override;
  published
{$ENDIF} // NOTFORHELP
    property OnMouseEnter : TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave : TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property AnimatEvents : TacAnimatEvents read FAnimatEvents write FAnimatEvents default [aeGlobalDef];
    property SkinData : TsCommonData read FCommonData write FCommonData;
    property ShowFocus : boolean read FShowFocus write SetShowFocus default True;
    property DisabledKind : TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property Down : boolean read GetDown write SetDown default False;
    property FocusMargin : integer read FFocusMargin write SetFocusMargin default 1;
{$IFNDEF DELPHI7UP}
    property WordWrap : boolean read FWordWrap write SetWordWrap default True;
{$ELSE}
    property WordWrap default True;
{$ENDIF}
  end;

implementation

uses sVCLUtils, sMessages, acntUtils, sGraphUtils, sAlphaGraph, sBitBtn, sBorders, ActnList, sSkinManager,
  sStyleSimply, acGlow;

{ TsButton }

var
  bFocusChanging : boolean = False;

function MaxCaptionWidth(Button : TsButton) : integer;
begin
  with Button do if (Caption <> '') then Result := Width - 2 else Result := 0
end;

procedure TsButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then Self.Enabled := TCustomAction(Sender).Enabled;
end;

procedure TsButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if (SkinData <> nil) and (SkinData.SkinManager <> nil) and SkinData.SkinManager.Active and (SkinData.SkinManager.SkinName <> '') {$IFNDEF SKININDESIGN}and not ((csDesigning in ComponentState) and (GetOwnerFrame(Self) <> nil)){$ENDIF}  
    then Params.Style := Params.Style or BS_OWNERDRAW
end;

procedure TsButton.CNMeasureItem(var Message: TWMMeasureItem);
begin
  with Message.MeasureItemStruct^ do begin
    itemWidth := Width;
    itemHeight := Height;
  end;
end;

procedure TsButton.AfterConstruction;
begin
  inherited;
  FCommonData.FCacheBmp.Canvas.Font.Assign(Font);
  FCommonData.Loaded;
end;

procedure TsButton.SetButtonStyle(ADefault: Boolean);
begin
  if ADefault <> IsFocused then IsFocused := ADefault;
  SkinData.Invalidate
end;

function TsButton.CaptionRect: TRect;
var
  l, t, r, b : integer;
  Size : TSize;
begin
  Size := TextRectSize;
  l := (Width - Size.cx) div 2;
  t := (Height - Size.cy) div 2;
  b := Height - t;
  r := Width - l;
  Result := Rect(l - 1, t, r + 2, b);
  if CurrentState = 2 then OffsetRect(Result, 1, 1);
end;

constructor TsButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csOpaque, csDoubleClicks];
  FCommonData := TsCommonData.Create(Self, True);
  FCommonData.COC := COC_TsBUTTON;
  FDisabledKind := DefDisabledKind;
  FFocusMargin := 1;
  FadeTimer := nil;
  FDown := False;
  FRegion := 1;
  FAnimatEvents := [aeGlobalDef];
  FShowFocus := True;
{$IFNDEF DELPHI7UP}
  FWordWrap := True;
{$ELSE}
  WordWrap := True;
{$ENDIF}
  RegionChanged := True;
end;

function TsButton.CurrentState: integer;
begin
  if ((SendMessage(Handle, BM_GETSTATE, 0, 0) and BST_PUSHED = BST_PUSHED) or fGlobalFlag) and (SkinData.FMouseAbove or not (csLButtonDown in ControlState)) or FDown
    then Result := 2
    else if IsFocused or ((GetWindowLong(Handle, GWL_STYLE) and $000F) = BS_DEFPUSHBUTTON) or ControlIsActive(FCommonData) or ((csDesigning in ComponentState) and Default)
      then Result := 1
      else Result := 0
end;

destructor TsButton.Destroy;
begin
  StopFading(FadeTimer, FCommonData);
  if Assigned(FCommonData) then FreeAndNil(FCommonData);
  inherited Destroy;
end;

procedure TsButton.DoDrawText(var Rect: TRect; Flags: Integer);
begin
  Flags := DrawTextBiDiModeFlags(Flags);
  acWriteTextEx(FCommonData.FCacheBMP.Canvas, PacChar(Caption), True, Rect, Flags, FCommonData, CurrentState <> 0);
end;

procedure TsButton.DrawCaption;
var
  R : TRect;
  DrawStyle: Longint;
begin
  FCommonData.FCacheBmp.Canvas.Font.Assign(Font);
  FCommonData.FCacheBMP.Canvas.Brush.Style := bsClear;
  R := CaptionRect;
  { Calculate vertical layout }

  DrawStyle := DT_EXPANDTABS or DT_CENTER;
  if WordWrap then DrawStyle := DrawStyle or DT_WORDBREAK;

  DoDrawText(R, DrawStyle);

  if Enabled and Focused and (Caption <> '') and FCommonData.SkinManager.gd[FCommonData.SkinIndex].ShowFocus and ShowFocus then begin
    InflateRect(R, FocusMargin, FocusMargin);
    FocusRect(FCommonData.FCacheBMP.Canvas, R);
  end;
end;

function TsButton.GetDown: boolean;
begin
  Result := FDown;
end;
 
procedure TsButton.Loaded;
begin
  inherited;
  FCommonData.FCacheBmp.Canvas.Font.Assign(Font);
  FCommonData.Loaded;
end;

procedure TsButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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
//        FDown := True;
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

procedure TsButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FCommonData.Skinned and Enabled and not (csDesigning in ComponentState) {and FDown} then begin
    if (Button = mbLeft) and ShowHintStored then begin
      Application.ShowHint := AppShowHint;
      ShowHintStored := False;
    end;

    if not FMouseClicked or (csDestroying in ComponentState) then Exit;
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

      if (Self <> nil) and not (csDestroying in ComponentState) then begin
        RegionChanged := True;
        FCommonData.BGChanged := False;
        if Assigned(FCommonData) then DoChangePaint(FadeTimer, FCommonData, True, EventEnabled(aeMouseUp, FAnimatEvents), fdUp);
      end;
    end;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TsButton.OurPaintHandler;
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
      b := (FRegion = 1) or aSkinChanging;
      FRegion := 0;
      if RegionChanged then FCommonData.BGChanged := True;
      if (FCommonData.BGChanged) then begin
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

procedure TsButton.PrepareCache;
var
  CI : TCacheInfo;
  BGInfo : TacBGInfo;
begin
  BGInfo.PleaseDraw := False;
  GetBGInfo(@BGInfo, Parent);
  CI := BGInfoToCI(@BGInfo);

  InitCacheBmp(SkinData);

  PaintItemBG(FCommonData, CI, CurrentState, Rect(0, 0, Width, Height), Point(Left, Top), FCommonData.FCacheBMP, 0, 0);
  if RegionChanged then begin FRegion := 0; FRegion := CreateRectRgn(0, 0, Width, Height); end; // Empty region
  if FCommonData.SkinManager.IsValidImgIndex(FCommonData.BorderIndex) then
    PaintRgnBorder(FCommonData.FCacheBmp, FRegion, True, FCommonData.SkinManager.ma[FCommonData.BorderIndex], CurrentState);
  DrawCaption;

  if not Enabled or ((Action <> nil) and not TAction(Action).Enabled) then begin
    CI := GetParentCache(FCommonData);
    BmpDisabledKind(FCommonData.FCacheBmp, FDisabledKind, Parent, CI, Point(Left, Top));
  end;

  FCommonData.BGChanged := False;
end;

procedure TsButton.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    if not (csLoading in ComponentState) then FCommonData.Invalidate;
  end;
end;

{$IFNDEF DELPHI7UP}
procedure TsButton.SetWordWrap(const Value: boolean);
begin
  if FWordWrap <> Value then begin
    FWordWrap := Value;
    if not (csLoading in ComponentState) then FCommonData.Invalidate;
  end;
end;
{$ENDIF}

procedure TsButton.SetDown(const Value: boolean);
begin
  if FDown <> Value then begin
    FDown := Value;
    RegionChanged := True;
    if not (csLoading in ComponentState) then FCommonData.Invalidate;
  end;
end;

procedure TsButton.SetFocusMargin(const Value: integer);
begin
  if (FFocusMargin <> Value) then begin
    FFocusMargin := Value;
  end;
end;

procedure TsButton.SetShowFocus(const Value: boolean);
begin
  if FShowFocus <> Value then begin
    FShowFocus := Value;
    FCommonData.Invalidate;
  end;
end;

function TsButton.TextRectSize: TSize;
var
  R : TRect;
  DrawStyle: Longint;
begin
  R := Rect(0, 0, MaxCaptionWidth(Self), 0);
  DrawStyle := DT_EXPANDTABS or DT_CENTER or DT_CALCRECT;
  if WordWrap
    then DrawStyle := DrawStyle or DT_WORDBREAK
    else DrawStyle := DrawStyle or DT_SINGLELINE;
  AcDrawText(FCommonData.FCacheBMP.Canvas.Handle, Caption, R, DrawStyle);
  Result.cy := HeightOf(R);
  Result.cx := WidthOf(R);
end;

procedure TsButton.WMKeyUp(var Message: TWMKey);
begin
  inherited;
  if Assigned(FCommonData) and FCommonData.Skinned and (Message.CharCode = 32) then begin
//    FDown := False;
    RegionChanged := True;
    FCommonData.BGChanged := True;
    Repaint;
  end;
end;

procedure TsButton.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  if (Message.Msg = WM_KILLFOCUS) and (csDestroying in ComponentState) then Exit;
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_CTRLHANDLED : begin Message.Result := 1; Exit end; // AlphaSkins supported
    AC_GETAPPLICATION : begin Message.Result := longint(Application); Exit end;
    AC_SETNEWSKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      RegionChanged := True;    
      {$IFNDEF SKININDESIGN}if not ((csDesigning in ComponentState) and (GetOwnerFrame(Self) <> nil)) then{$ENDIF}
      SendMessage(Handle, BM_SETSTYLE, (GetWindowLong(Handle, GWL_STYLE) or BS_OWNERDRAW), 1);
      StopFading(FadeTimer, FCommonData);

      CommonWndProc(Message, FCommonData);
      exit
    end;
    AC_REMOVESKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) and not (csDestroying in ComponentState) then begin
      StopFading(FadeTimer, FCommonData);
      CommonWndProc(Message, FCommonData);
      SendMessage(Handle, BM_SETSTYLE, (GetWindowLong(Handle, GWL_STYLE) and not BS_OWNERDRAW), 1);
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
      CM_MOUSEENTER : if Assigned(FOnMouseEnter) and Enabled and not (csDesigning in ComponentState) then FOnMouseEnter(Self);
      CM_MOUSELEAVE : if Assigned(FOnMouseLeave) and Enabled and not (csDesigning in ComponentState) then FOnMouseLeave(Self);
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
        ClearGlows;
      end;
      WM_UPDATEUISTATE, WM_ERASEBKGND : if Visible or (csDesigning in ComponentState) then begin
        Message.Result := 1;
        Exit;
      end;
      CM_TEXTCHANGED : if not (csDestroying in ComponentState) then begin
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
        end else inherited;
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
            RegionChanged := True;
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
      WM_ENABLE : Exit; // Disabling of blinking when switched
    end;
    CommonWndProc(Message, FCommonData);
    inherited;
    case Message.Msg of
      CM_ENABLEDCHANGED : if (Visible or (csDesigning in ComponentState)) and not (csDestroying in ComponentState) then begin
        FCommonData.Updating := False;
        FCommonData.Invalidate;
      end;
      CM_VISIBLECHANGED : if not (csDestroying in ComponentState) then begin
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

end.
