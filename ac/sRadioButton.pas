unit sRadioButton;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  {$IFDEF TNTUNICODE}TntControls, TntActnList, TntForms, TntClasses, {$ENDIF}
  StdCtrls, sCommonData, sConst, sDefaults, sFade{$IFDEF LOGGED}, sDebugMsgs{$ENDIF};

type
  TsRadioButton = class(TRadioButton)
{$IFNDEF NOTFORHELP}
  private
    FCommonData: TsCommonData;
    FDisabledKind: TsDisabledKind;
    FGlyphUnChecked: TBitmap;
    FGlyphChecked: TBitmap;
    FTextIndent: integer;
    FPressed : boolean;
    FShowFocus: Boolean;
    FMargin: integer;
    FadeTimer : TsFadeTimer;
    FAnimatEvents: TacAnimatEvents;
{$IFNDEF DELPHI7UP}
    FWordWrap : boolean;
    procedure SetWordWrap(const Value: boolean);
{$ENDIF}
    procedure SetDisabledKind(const Value: TsDisabledKind);
    procedure SetGlyphChecked(const Value: TBitmap);
    procedure SetGlyphUnChecked(const Value: TBitmap);
    procedure SetTextIndent(const Value: integer);
    procedure SetShowFocus(const Value: Boolean);
    procedure SetMargin(const Value: integer);
    function GetReadOnly: boolean; virtual;
    procedure SetReadOnly(const Value: boolean);
{$IFDEF TNTUNICODE}
    function GetCaption: TWideCaption;
    procedure SetCaption(const Value: TWideCaption);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsCaptionStored: Boolean;
    function IsHintStored: Boolean;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
{$ENDIF}
  protected
    FReadOnly: boolean;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure PaintHandler(M : TWMPaint);
    procedure PaintControl(DC : HDC);
    procedure DrawCheckText;
    procedure DrawCheckArea;
    procedure DrawSkinGlyph(i : integer);
    procedure PaintGlyph(Bmp : TBitmap);
    function SkinGlyphWidth(i : integer) : integer;
    function SkinGlyphHeight(i : integer) : integer;
    function SkinCheckRect(i : integer): TRect;

    function CheckRect: TRect;
    function GlyphWidth : integer;
    function GlyphHeight : integer;

    function GlyphMaskIndex(Checked : boolean) : smallint;
    procedure PrepareCache;
{$IFDEF TNTUNICODE}
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
{$ENDIF}
  public
    function GetControlsAlignment: TAlignment; override;
    procedure AfterConstruction; override;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    procedure Loaded; override;
    procedure WndProc(var Message: TMessage); override;
  published
{$IFDEF TNTUNICODE}
    property Caption: TWideCaption read GetCaption write SetCaption stored IsCaptionStored;
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
{$ENDIF}
    property AutoSize default True;
    property Margin : integer read FMargin write SetMargin default 2;//0;
{$ENDIF} // NOTFORHELP
    property AnimatEvents : TacAnimatEvents read FAnimatEvents write FAnimatEvents default [aeGlobalDef];
    property SkinData : TsCommonData read FCommonData write FCommonData;
    property DisabledKind : TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property GlyphChecked : TBitmap read FGlyphChecked write SetGlyphChecked;
    property GlyphUnChecked : TBitmap read FGlyphUnChecked write SetGlyphUnChecked;
    property ReadOnly : boolean read GetReadOnly write SetReadOnly default False;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default True;
    property TextIndent : integer read FTextIndent write SetTextIndent default 0;
{$IFNDEF DELPHI7UP}
    property WordWrap : boolean read FWordWrap write SetWordWrap default False;
{$ELSE}
    property WordWrap default False;
{$ENDIF}
  end;

implementation

uses sGraphUtils, acntUtils, sAlphaGraph, sVclUtils, sMaskData, sStylesimply, sSkinProps, ExtCtrls, sGroupBox,
  Math, sMessages, sSKinManager{$IFDEF CHECKXP}, UxTheme, Themes{$ENDIF};

{ TsRadioButton }

{$IFDEF TNTUNICODE}
procedure TsRadioButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;
{$ENDIF}

procedure TsRadioButton.AfterConstruction;
begin
  inherited;
  SkinData.Loaded;
end;

function TsRadioButton.GetControlsAlignment: TAlignment;
begin
  if not UseRightToLeftAlignment
    then Result := Alignment
    else if Alignment = taRightJustify
      then Result := taLeftJustify
      else Result := taRightJustify;
end;

function TsRadioButton.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
var
  ss : TSize;
  R : TRect;
  w, h : integer;
begin
  Result := False;
  if FCommonData.Skinned then begin
    if csLoading in ComponentState then Exit;
    if AutoSize then begin
      ss := GetStringSize(Font.Handle, Caption);
      R := CheckRect;
      NewWidth := WidthOf(R) + 2 * Margin + (ss.cx + FTextIndent + 8) * integer(Caption <> '');
      NewHeight := Max(HeightOf(R), 2 * Margin + ss.cy * integer(Caption <> '')) + 2;
      Result := True;
      w := NewWidth; h := NewHeight;
    end;
  end
  else begin
    if AutoSize then begin
      ss := GetStringSize(Font.Handle, Caption);
      NewWidth := ss.cx + 20;
      NewHeight := max(ss.cy + 4, 20);
    end
    else begin
      w := NewWidth; h := NewHeight;
      Result := inherited CanAutoSize(w, h);
      NewWidth := w; NewHeight := h;
    end;
  end;
end;

function TsRadioButton.CheckRect: TRect;
var
  i : integer;
begin
  if FGlyphChecked.Width > 0 then begin
    if GetControlsAlignment = taRightJustify
      then Result := Rect(Margin, (Height - GlyphHeight) div 2, Margin + GlyphWidth, GlyphHeight + (Height - GlyphHeight) div 2)
      else Result := Rect(Width - GlyphWidth - Margin, (Height - GlyphHeight) div 2, Width - Margin, GlyphHeight + (Height - GlyphHeight) div 2)
  end
  else begin
    i := GlyphMaskIndex(Checked);
    if SkinData.SkinManager.IsValidImgIndex(i) then Result := SkinCheckRect(i) else Result := Rect(0, 0, 16, 16);
  end;
end;

{$IFDEF TNTUNICODE}
procedure TsRadioButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsWideCharAccel(Message.CharCode, Caption)
    and CanFocus then
    begin
      SetFocus;
      Result := 1;
    end else
      Broadcast(Message);
end;
{$ENDIF}

constructor TsRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommonData := TsCommonData.Create(Self, False);
  FCommonData.COC := COC_TsRadioButton;
  FCommonData.FOwnerControl := Self;
  FadeTimer := nil;
  FMargin := 2;
  FShowFocus := True;
  FTextIndent := 0;
  FDisabledKind := DefDisabledKind;
  FGlyphChecked := TBitmap.Create;
  FGlyphUnChecked := TBitmap.Create;
  FPressed := False;
  AutoSize := True;
  FAnimatEvents := [aeGlobalDef];
{$IFNDEF DELPHI7UP}
  FWordWrap := False;
{$ELSE}
  WordWrap := False;
{$ENDIF}
end;

{$IFDEF TNTUNICODE}
procedure TsRadioButton.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, 'BUTTON');
end;

procedure TsRadioButton.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;
{$ENDIF}

destructor TsRadioButton.Destroy;
begin
  StopFading(FadeTimer, FCommonData);
  if Assigned(FCommonData) then FreeAndNil(FCommonData);
  if Assigned(FGlyphChecked) then FreeAndNil(FGlyphChecked);
  if Assigned(FGlyphUnchecked) then FreeAndNil(FGlyphUnChecked);
  inherited Destroy;
end;

procedure TsRadioButton.DrawCheckArea;
var
  CheckArea: TRect;
  i : integer;
begin
  if FGlyphChecked.Width > 0 then begin
    CheckArea := CheckRect;
    if Checked then begin
      PaintGlyph(FGlyphChecked);
    end
    else if not Checked then begin
      if (FGlyphUnChecked.Width > 0) then PaintGlyph(FGlyphUnChecked);
    end;
  end
  else if SkinData.SkinManager.IsValidSkinIndex(FCommonData.SkinIndex) then begin
    i := GlyphMaskIndex(Checked);
    if SkinData.SkinManager.IsValidImgIndex(i) then DrawSkinGlyph(i);
  end;
end;

procedure TsRadioButton.DrawCheckText;
var
  rText: TRect;
  Fmt: integer;
  t, b, w, h : integer;
begin
  if Caption <> '' then begin
    w := Width - (WidthOf(CheckRect) + FTextIndent + 2 * Margin + 2);

    rText := Rect(0, 0, w, 0);
    Fmt := DT_CALCRECT;
    if WordWrap
      then Fmt := Fmt or DT_WORDBREAK
      else Fmt := Fmt or DT_SINGLELINE;
    AcDrawText(FCommonData.FCacheBMP.Canvas.Handle, Caption, rText, Fmt);
    h := HeightOf(rText);
    t := Max((Height - h) div 2, Margin);
    b := t + h;

    Fmt := 0;
    if GetControlsAlignment = taRightJustify then begin
      rText := Rect(Width - w - Margin + 2, t, Width - Margin, b);
      if not WordWrap then Fmt := DT_LEFT;
    end
    else begin
      rText := Rect(Margin, t, w + Margin, b);
    end;
    OffsetRect(rText, -integer(WordWrap), -1);
    if WordWrap
      then Fmt := Fmt or DT_WORDBREAK or DT_TOP or DT_CENTER
      else Fmt := Fmt or DT_SINGLELINE or DT_TOP;

    acWriteTextEx(FCommonData.FCacheBmp.Canvas, PacChar(Caption), True, rText, Fmt, FCommonData, ControlIsActive(FCommonData) and not ReadOnly);

    FCommonData.FCacheBmp.Canvas.Pen.Style := psClear;
    FCommonData.FCacheBmp.Canvas.Brush.Style := bsSolid;
    if Focused and ShowFocus then begin
      dec(rText.Bottom, {1 + }integer(not WordWrap));
      inc(rText.Top);
      InflateRect(rText, 1, 1);
      FocusRect(FCommonData.FCacheBmp.Canvas, rText);
    end;
  end;
end;

procedure TsRadioButton.DrawSkinGlyph(i: integer);
var
  R : TRect;
  Mode : integer;
begin
  if FCommonData.FCacheBmp.Width < 1 then exit;
  R := SkinCheckRect(i);
  if FPressed then Mode := 2 else if ControlIsActive(FCommonData) and not ReadOnly then Mode := 1 else Mode := 0;
  sAlphaGraph.DrawSkinGlyph(FCommonData.FCacheBmp, R.TopLeft, Mode, 1, FCommonData.SkinManager.ma[i], MakeCacheInfo(SkinData.FCacheBmp));
end;

{$IFDEF TNTUNICODE}
function TsRadioButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

function TsRadioButton.GetCaption: TWideCaption;
begin
  Result := TntControl_GetText(Self)
end;

function TsRadioButton.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;
{$ENDIF}

function TsRadioButton.GetReadOnly: boolean;
begin
  if (Parent is TsRadioGroup) then begin
     Result := not TsRadioGroup(Parent).CanModify(TsRadioGroup(Parent).FButtons.IndexOf(Self))
  end
  else Result := FReadOnly;
end;

function TsRadioButton.GlyphHeight: integer;
begin
  Result := GlyphChecked.Height div 2;
end;

function TsRadioButton.GlyphMaskIndex(Checked : boolean): smallint;
begin
  if Checked
    then Result := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinManager.ConstData.IndexGLobalInfo, s_GLobalInfo, s_RadioButtonChecked)
    else Result := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinManager.ConstData.IndexGLobalInfo, s_GLobalInfo, s_RadioButtonUnChecked);
end;

function TsRadioButton.GlyphWidth: integer;
begin
  Result := GlyphChecked.Width div 3;
end;

procedure TsRadioButton.Invalidate;
begin
  inherited;
  if AutoSize then WordWrap := False;
end;

{$IFDEF TNTUNICODE}
function TsRadioButton.IsCaptionStored: Boolean;
begin
  Result := TntControl_IsCaptionStored(Self);
end;

function TsRadioButton.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self)
end;
{$ENDIF}

procedure TsRadioButton.Loaded;
begin
  inherited;
  SkinData.Loaded;
  AdjustSize;
end;

procedure TsRadioButton.PaintControl(DC: HDC);
begin
  if not FCommonData.Updating and not (Assigned(FadeTimer) and FadeTimer.Enabled) then begin
    PrepareCache;
    UpdateCorners(FCommonData, 0);
    BitBlt(DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
  end;
end;

procedure TsRadioButton.PaintGlyph(Bmp: TBitmap);
var
  R : TRect;
  function CurrentMaskRect : TRect; begin
    if FPressed then begin
      Result := Rect(2 * GlyphWidth, 0, 3 * GlyphWidth, GlyphHeight);
    end
    else if ControlIsActive(FCommonData) and not ReadOnly then begin
      Result := Rect(GlyphWidth, 0, 2 * GlyphWidth, GlyphHeight);
    end
    else begin
      Result := Rect(0, 0, GlyphWidth, GlyphHeight);
    end;
  end;
begin
  if FCommonData.FCacheBmp.Width < 1 then exit;
  Bmp.PixelFormat := pf24bit;
  R := CheckRect;
  CopyByMask(Rect(R.Left, R.Top, R.Right, R.Bottom), CurrentMaskRect, FCommonData.FCacheBmp, Bmp, EmptyCI, True);
end;

procedure TsRadioButton.PaintHandler(M: TWMPaint);
var
  PS: TPaintStruct;
  DC : hdc;
  SavedDC: hdc;
begin
  DC := M.DC;
  if DC = 0 then begin
    BeginPaint(Handle, PS);
    DC := GetDC(Handle);
  end;
  SavedDC := SaveDC(DC);
  try
    if not FCommonData.Updating then PaintControl(DC) else FCommonData.Updating := True;
  finally
    RestoreDC(DC, SavedDC);
    if M.DC = 0 then begin
      ReleaseDC(Handle, DC);
      EndPaint(Handle, PS);
    end;
  end;
end;

procedure TsRadioButton.PrepareCache;
var
  BGInfo : TacBGInfo;
begin
  FCommonData.InitCacheBmp;
  FCommonData.FCacheBmp.Canvas.Font.Assign(Font);
  FCommonData.FCacheBmp.Canvas.Lock;
  BGInfo.DrawDC := FCommonData.FCacheBmp.Canvas.Handle;
  BGInfo.PleaseDraw := True;
  BGInfo.Offset := Point(Left, Top);
  BGInfo.R := Rect(0, 0, Width, Height);
  GetBGInfo(@BGInfo, Parent);
  if BGInfo.BgType = btUnknown then begin // If parent is not AlphaControl
    BGInfo.Bmp := FCommonData.FCacheBmp;
    BGInfo.BgType := btCache;
  end;
  FCommonData.FCacheBmp.Canvas.Unlock;
  PaintItem(FCommonData, BGInfoToCI(@BGInfo), True, integer(ControlIsActive(FCommonData) and not ReadOnly),
              Rect(0, 0, FCommonData.FCacheBmp.Width, Height), Point(Left, Top), FCommonData.FCacheBmp, False);
  DrawCheckText;
  DrawCheckArea;
  if not Enabled then BmpDisabledKind(FCommonData.FCacheBmp, FDisabledKind, Parent, BGInfoToCI(@BGInfo), Point(Left, Top));
  FCommonData.BGChanged := False
end;

{$IFDEF TNTUNICODE}
procedure TsRadioButton.SetCaption(const Value: TWideCaption);
begin
  TntControl_SetText(Self, Value);
end;
{$ENDIF}

{$IFNDEF DELPHI7UP}
procedure TsRadioButton.SetWordWrap(const Value: boolean);
begin
  if FWordWrap <> Value then begin
    FWordWrap := Value;
    FCommonData.BGChanged := True;
    if AutoSize then AutoSize := False;
    Repaint;
  end;
end;
{$ENDIF}

procedure TsRadioButton.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FCommonData.Invalidate;
  end;
end;

procedure TsRadioButton.SetGlyphChecked(const Value: TBitmap);
begin
  FGlyphChecked.Assign(Value);
  if AutoSize then AdjustSize;
  FCommonData.Invalidate;
end;

procedure TsRadioButton.SetGlyphUnChecked(const Value: TBitmap);
begin
  FGlyphUnChecked.Assign(Value);
  if AutoSize then AdjustSize;
  Invalidate;
end;

{$IFDEF TNTUNICODE}
procedure TsRadioButton.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;
{$ENDIF}

procedure TsRadioButton.SetMargin(const Value: integer);
begin
  if FMargin <> Value then begin
    FMargin := Value;
    if AutoSize then AdjustSize;
    Invalidate;
  end;
end;

procedure TsRadioButton.SetReadOnly(const Value: boolean);
begin
  FReadOnly := Value;
end;

procedure TsRadioButton.SetShowFocus(const Value: Boolean);
begin
  if FShowFocus <> Value then begin
    FShowFocus := Value;
    Invalidate;
  end;
end;

procedure TsRadioButton.SetTextIndent(const Value: integer);
begin
  if FTextIndent <> Value then begin
    FTextIndent := Value;
    if AutoSize then AdjustSize;
    Invalidate;
  end;
end;

function TsRadioButton.SkinCheckRect(i: integer): TRect;
var
  h, w, hdiv : integer;
begin
  h := SkinGlyphHeight(i);
  w := SkinGlyphWidth(i);
  hdiv := (Height - h) div 2;
  if GetControlsAlignment = taRightJustify then begin
    Result := Rect(Margin, hdiv, Margin + w, h + hdiv);
  end
  else begin
    Result := Rect(Width - w - Margin, hdiv, Width - Margin, h + hdiv);
  end;
end;

function TsRadioButton.SkinGlyphHeight(i: integer): integer;
begin
  if Assigned(FCommonData.SkinManager.ma[i].Bmp) then Result := FCommonData.SkinManager.ma[i].Bmp.Height div 2 else Result := HeightOf(FCommonData.SkinManager.ma[i].R) div (FCommonData.SkinManager.ma[i].MaskType + 1);
end;

function TsRadioButton.SkinGlyphWidth(i: integer): integer;
begin
  if Assigned(FCommonData.SkinManager.ma[i].Bmp) then Result := FCommonData.SkinManager.ma[i].Bmp.Width div 3 else Result := WidthOf(FCommonData.SkinManager.ma[i].R) div FCommonData.SkinManager.ma[i].ImageCount;
end;

procedure TsRadioButton.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_CTRLHANDLED : begin Message.Result := 1; Exit end; // AlphaSkins supported
    AC_GETAPPLICATION : begin Message.Result := longint(Application); Exit end;
    AC_REMOVESKIN : if LongWord(Message.LParam) = LongWord(SkinData.SkinManager) then begin
      StopFading(FadeTimer, FCommonData);
      CommonWndProc(Message, FCommonData);
      if HandleAllocated then SendMessage(Handle, BM_SETCHECK, Integer(Checked), 0);
      if not (csDesigning in ComponentState) and (@Ac_SetWindowTheme <> nil) then Ac_SetWindowTheme(Handle, nil, nil);
      Repaint;
      exit
    end;
    AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      SetClassLong(Handle, GCL_STYLE, GetClassLong(Handle, GCL_STYLE) and not CS_VREDRAW and not CS_HREDRAW);
      StopFading(FadeTimer, FCommonData);
      CommonWndProc(Message, FCommonData);
      AdjustSize; // v5.32
      Repaint;
      exit
    end;
    AC_PREPARECACHE : PrepareCache;
    AC_STOPFADING : begin StopFading(FadeTimer, FCommonData); Exit end;
    AC_SETNEWSKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      StopFading(FadeTimer, FCommonData);
      CommonWndProc(Message, FCommonData);
      exit
    end
  end;
  if (FCommonData <> nil) and FCommonData.Skinned(True) then case Message.Msg of
    CM_MOUSEENTER : if Enabled and not (csDesigning in ComponentState) and not FCommonData.FMouseAbove then begin
      FCommonData.FMouseAbove := True;
      DoChangePaint(FadeTimer, FCommonData, False, EventEnabled(aeMouseEnter, FAnimatEvents));
    end;
    CM_MOUSELEAVE : if Enabled and not (csDesigning in ComponentState) then begin
      FCommonData.FMouseAbove := False;
      FPressed := False;
      DoChangePaint(FadeTimer, FCommonData, False, EventEnabled(aeMouseLeave, FAnimatEvents));
    end;
    WM_SETFOCUS, CM_ENTER : if not (csDesigning in ComponentState) then begin
      if Enabled then begin
        inherited;
        FCommonData.BGChanged := True;
        if FadeTimer = nil then Repaint else FadeTimer.Change; // Fast repaint
      end;
      Exit;
    end;
    WM_KILLFOCUS, CM_EXIT: if not (csDesigning in ComponentState) then begin
      if Enabled then begin
        if FadeTimer <> nil then StopFading(FadeTimer, FCommonData);
        Perform(WM_SETREDRAW, 0, 0);
        inherited;
        Perform(WM_SETREDRAW, 1, 0);
        FCommonData.FFocused := False;
        FCommonData.FMouseAbove := False;
        FCommonData.Invalidate;
        Exit
      end;
    end;
  end;
  if not ControlIsReady(Self) then inherited else begin
    CommonWndProc(Message, FCommonData);
    if FCommonData.Skinned(True) then begin
      if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
        AC_ENDPARENTUPDATE : if FCommonData.Updating or FCommonData.HalfVisible then begin
          FCommonData.Updating := False;
          PaintHandler(TWMPaint(MakeMessage(WM_PAINT, 0, 0, 0)));
        end
      end
      else case Message.Msg of
        WM_ENABLE, WM_NCPAINT : Exit; // Disabling of blinking when switched
{$IFDEF CHECKXP}
        WM_UPDATEUISTATE : begin
          if SkinData.Skinned and UseThemes and not (csDesigning in ComponentState) and (@Ac_SetWindowTheme <> nil)
            then Ac_SetWindowTheme(Handle, ' ', ' ');
          Exit;
        end;
{$ENDIF}
        CM_ENABLEDCHANGED : begin inherited; Repaint; Exit end;
        BM_SETSTATE : begin
          Exit;
        end;
        BM_SETCHECK : begin
          inherited; 
          FCommonData.BGChanged := True;
          if (FadeTimer <> nil) and (FadeTimer.FadeLevel < FadeTimer.Iterations) then begin
            FadeTimer.Enabled := False;
            Repaint;
          end;
          case Message.WParam of
            0 : Checked := False;
            1 : Checked := True;
          end;
          if not (csDesigning in ComponentState) then begin
            if Checked then DoChangePaint(FadeTimer, FCommonData, True, EventEnabled(aeMouseUp, FAnimatEvents), fdUp) else DoChangePaint(FadeTimer, FCommonData, True, EventEnabled(aeMouseUp, FAnimatEvents));
          end
          else FCommonData.Invalidate;
          Exit;
        end;
        WM_ERASEBKGND : begin
          Message.Result := 1;
          Exit;
        end;
        WM_PRINT : begin
          SkinData.Updating := False;
          PaintHandler(TWMPaint(Message));
        end;
        WM_PAINT : begin
          PaintHandler(TWMPaint(Message));
          if not (csDesigning in ComponentState) then Exit;
        end;
        CM_TEXTCHANGED : begin
          if AutoSize then AdjustSize;
          Repaint;
          Exit;
        end;
        WM_KEYDOWN : if Enabled and not (csDesigning in ComponentState) and (TWMKey(Message).CharCode = VK_SPACE) then begin
          if ReadOnly then Exit;
          FPressed := True;
          if not Focused then begin
            ClicksDisabled := True;
            Windows.SetFocus(Handle);
            ClicksDisabled := False;
          end;
          Repaint;
          if Assigned(OnKeyDown) then OnKeydown(Self, TWMKeyDown(Message).CharCode, KeysToShiftState(word(TWMKeyDown(Message).KeyData)));
          Exit;
        end;
        WM_LBUTTONDBLCLK, WM_LBUTTONDOWN : if not (csDesigning in ComponentState) and Enabled and (DragMode = dmManual) then begin
          if ReadOnly then Exit;
          FPressed := True;
          DoChangePaint(FadeTimer, FCommonData, True, EventEnabled(aeMouseDown, FAnimatEvents));

          if not Focused then begin
            ClicksDisabled := True;
            Windows.SetFocus(Handle);
            ClicksDisabled := False;
          end;

          if WM_LBUTTONDBLCLK = Message.Msg then begin
            if Assigned(OnDblClick) then OnDblClick(Self)
          end
          else if Assigned(OnMouseDown) then OnMouseDown(Self, mbLeft, KeysToShiftState(TWMMouse(Message).Keys), TWMMouse(Message).XPos, TWMMouse(Message).YPos);
          Exit;
        end;
        WM_KEYUP : if not (csDesigning in ComponentState) and Enabled then begin
          if ReadOnly then Exit;
          if FPressed then begin
            FPressed := False;
            Checked := True;
          end;
          Repaint;
          if Assigned(OnKeyUp) then OnKeyUp(Self, TWMKey(Message).CharCode, KeysToShiftState(TWMKey(Message).KeyData));
          Exit;
        end;
        WM_LBUTTONUP : if not (csDesigning in ComponentState) and Enabled then begin
          if ReadOnly then Exit;
          if FPressed then begin
            FPressed := False;
            Checked := True; 
          end;
          Repaint;
          if Assigned(OnMouseUp) then OnMouseUp(Self, mbLeft, KeysToShiftState(TWMMouse(Message).Keys), TWMMouse(Message).XPos, TWMMouse(Message).YPos);
          Exit;
        end;
      end
    end
    else case Message.Msg of
      WM_KEYDOWN, WM_LBUTTONDOWN : FPressed := True;
      WM_KEYUP, WM_LBUTTONUP : FPressed := False;
      WM_LBUTTONDBLCLK : if ReadOnly then Exit;
      BM_SETSTATE, BM_SETCHECK : if not (csCreating in ControlState) and FPressed and ReadOnly then Exit;
    end;
    inherited;
  end;
end;

end.
