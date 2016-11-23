unit sCheckBox;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, sFade,
  {$IFDEF TNTUNICODE}TntControls, TntActnList, TntForms, TntClasses, {$ENDIF}
  StdCtrls, sCommonData, sConst, sDefaults, imglist{$IFDEF LOGGED}, sDebugMsgs{$ENDIF};

type
{$IFNDEF NOTFORHELP}
  TsImageIndex = integer;
{$ENDIF} // NOTFORHELP

  TsCheckBox = class(TCustomCheckBox)
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
    FImages: TCustomImageList;
    FImgChecked: TsImageIndex;
    FImgUnchecked: TsImageIndex;
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
    procedure SetReadOnly(const Value: boolean);
    procedure SetImageChecked(const Value: TsImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetImageUnChecked(const Value: TsImageIndex);
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

    function GetReadOnly: boolean; virtual;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure SetChecked(Value: Boolean); override;

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

    function GlyphMaskIndex(State : TCheckBoxState) : smallint;
    procedure PrepareCache;
{$IFDEF TNTUNICODE}
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
{$ENDIF}
  public
    function GetControlsAlignment: TAlignment; override;
    procedure AfterConstruction; override;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure WndProc(var Message: TMessage); override;
  published
{$IFDEF TNTUNICODE}
    property Caption: TWideCaption read GetCaption write SetCaption stored IsCaptionStored;
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
{$ELSE}
    property Caption;
{$ENDIF}
    property Action;
    property Align;
    property Alignment;
    property AllowGrayed;
    property Anchors;
    property AutoSize default True;
    property BiDiMode;
    property Checked;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property State;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property Margin : integer read FMargin write SetMargin default 2;
{$ENDIF} // NOTFORHELP
    property AnimatEvents : TacAnimatEvents read FAnimatEvents write FAnimatEvents default [aeGlobalDef];
    property SkinData : TsCommonData read FCommonData write FCommonData;
    property DisabledKind : TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property GlyphChecked : TBitmap read FGlyphChecked write SetGlyphChecked;
    property GlyphUnChecked : TBitmap read FGlyphUnChecked write SetGlyphUnChecked;
    property ImgChecked : TsImageIndex read FImgChecked write SetImageChecked;
    property ImgUnchecked : TsImageIndex  read FImgUnchecked write SetImageUnChecked;
    property Images : TCustomImageList read FImages write SetImages;
    property ReadOnly : boolean read GetReadOnly write SetReadOnly default False;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default True;
    property TextIndent : integer read FTextIndent write SetTextIndent default 0;
{$IFNDEF DELPHI7UP}
    property WordWrap : boolean read FWordWrap write SetWordWrap default False;
{$ELSE}
    property WordWrap default False;
{$ENDIF}
  end;

var
  PaintState : integer = -1;

implementation

uses sGraphUtils, acntUtils, sAlphaGraph, sVclUtils, sStylesimply, sSkinProps,
  Math, sMessages, sSKinManager{$IFDEF CHECKXP}, UxTheme, Themes{$ENDIF};

{ TsCheckBox }

procedure TsCheckBox.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
{$IFDEF TNTUNICODE}
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
{$ENDIF}
  FCommonData.BGChanged := True;
  inherited;
  Repaint;
end;

procedure TsCheckBox.AfterConstruction;
begin
  inherited;
  SkinData.Loaded;
end;

function TsCheckBox.GetControlsAlignment: TAlignment;
begin
  if not UseRightToLeftAlignment
    then Result := Alignment
    else if Alignment = taRightJustify
      then Result := taLeftJustify
      else Result := taRightJustify;
end;

function TsCheckBox.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
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

function TsCheckBox.CheckRect: TRect;
var
  i : integer;
begin
  if Assigned(Images) and (ImgChecked > -1) and (ImgUnChecked > -1) then begin
    if GetControlsAlignment = taRightJustify
      then Result := Rect(Margin, (Height - GlyphHeight) div 2, Margin + GlyphWidth, GlyphHeight + (Height - GlyphHeight) div 2)
      else Result := Rect(Width - GlyphWidth - Margin, (Height - GlyphHeight) div 2, Width - Margin, GlyphHeight + (Height - GlyphHeight) div 2)
  end
  else if FGlyphChecked.Width > 0 then begin
    if GetControlsAlignment = taRightJustify
      then Result := Rect(Margin, (Height - GlyphHeight) div 2, Margin + GlyphWidth, GlyphHeight + (Height - GlyphHeight) div 2)
      else Result := Rect(Width - GlyphWidth - Margin, (Height - GlyphHeight) div 2, Width - Margin, GlyphHeight + (Height - GlyphHeight) div 2)
  end
  else begin
    i := GlyphMaskIndex(cbChecked);
    if FCommonData.SkinManager.IsValidImgIndex(i) then Result := SkinCheckRect(i) else Result := Rect(0, 0, 16, 16);
  end;
end;

{$IFDEF TNTUNICODE}
procedure TsCheckBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do if IsWideCharAccel(Message.CharCode, Caption) and CanFocus then begin
    SetFocus;
    if Focused then Toggle;
    Result := 1;
  end
  else Broadcast(Message);
end;
{$ENDIF}

constructor TsCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommonData := TsCommonData.Create(Self, False);
  FCommonData.COC := COC_TsCheckBox;
  FCommonData.FOwnerControl := Self;
  FadeTimer := nil;
  FMargin := 2;
  FShowFocus := True;
  FTextIndent := 0;
  FDisabledKind := DefDisabledKind;
  FGlyphChecked := TBitmap.Create;
  FGlyphUnChecked := TBitmap.Create;
  FAnimatEvents := [aeGlobalDef];
{$IFNDEF DELPHI7UP}
  FWordWrap := False;
{$ELSE}
  WordWrap := False;
{$ENDIF}
  FPressed := False;
  AutoSize := True;
end;

{$IFDEF TNTUNICODE}
procedure TsCheckBox.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, 'BUTTON');
end;

procedure TsCheckBox.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;
{$ENDIF}

destructor TsCheckBox.Destroy;
begin
  StopFading(FadeTimer, FCommonData);
  if Assigned(FCommonData) then FreeAndNil(FCommonData);
  if Assigned(FGlyphChecked) then FreeAndNil(FGlyphChecked);
  if Assigned(FGlyphUnchecked) then FreeAndNil(FGlyphUnChecked);
  if Assigned(FadeTimer) then FreeAndNil(FadeTimer);
  inherited Destroy;
end;

const
  CheckBoxStates : array[0..2] of TCheckBoxState = (cbUnchecked, cbChecked, cbGrayed);

procedure TsCheckBox.DrawCheckArea;
var
  CheckArea: TRect;
  i : integer;
  TempBmp : TBitmap;
begin
  if Assigned(Images) and (ImgChecked > -1) and (ImgUnChecked > -1) then begin
    TempBmp := TBitmap.Create;
    TempBmp.Width := GlyphWidth;
    TempBmp.Height := GlyphHeight;
    TempBmp.PixelFormat := pf24bit;

    Images.GetBitmap(iffi(Checked, ImgChecked, ImgUnChecked), TempBmp);

    PaintGlyph(TempBmp);

    FreeAndNil(TempBmp);
  end
  else if FGlyphChecked.Width > 0 then begin
    CheckArea := CheckRect;
    if Checked then begin
      PaintGlyph(FGlyphChecked);
    end
    else if not Checked then begin
      if (FGlyphUnChecked.Width > 0) then PaintGlyph(FGlyphUnChecked);
    end;
  end
  else begin
    if PaintState <> - 1
      then i := GlyphMaskIndex(CheckBoxStates[PaintState])
      else i := GlyphMaskIndex(State);
    if SkinData.SkinManager.IsValidImgIndex(i) then DrawSkinGlyph(i);
  end;
end;

procedure TsCheckBox.DrawCheckText;
var
  rText: TRect;
  Fmt: integer;
  t, b, w, h, dx : integer;
begin
  if Caption <> '' then begin
    w := Width - (WidthOf(CheckRect) + FTextIndent + 2 * Margin + 2);

    rText := Rect(0, 0, w, 0);
    Fmt := DT_CALCRECT;
    if WordWrap then Fmt := Fmt or DT_WORDBREAK else Fmt := Fmt or DT_SINGLELINE;
    AcDrawText(FCommonData.FCacheBMP.Canvas.Handle, Caption, rText, Fmt);
    h := HeightOf(rText);
    dx := WidthOf(rText);
    t := Max((Height - h) div 2, Margin);
    b := t + h;

    Fmt := 0;
    if GetControlsAlignment = taRightJustify then begin
      rText := Rect(Width - w - Margin + 2, t, Width - w - Margin + 2 + dx, b);
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

procedure TsCheckBox.DrawSkinGlyph(i: integer);
var
  R : TRect;
  Mode : integer;
begin
  if FCommonData.FCacheBmp.Width < 1 then exit;
  R := SkinCheckRect(i);
  if FPressed then Mode := 2 else if ControlIsActive(FCommonData) and not ReadOnly then Mode := 1 else Mode := 0;
  sAlphaGraph.DrawSkinGlyph(FCommonData.FCacheBmp, R.TopLeft, Mode, 1, FCommonData.SkinManager.ma[i], MakeCacheInfo(SkinData.FCacheBmp))
end;

{$IFDEF TNTUNICODE}
function TsCheckBox.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

function TsCheckBox.GetCaption: TWideCaption;
begin
  Result := TntControl_GetText(Self)
end;

function TsCheckBox.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;
{$ENDIF}

function TsCheckBox.GetReadOnly: boolean;
begin
  Result := FReadOnly;
end;

function TsCheckBox.GlyphHeight: integer;
begin
  if Assigned(Images) and (ImgChecked > -1) and (ImgUnChecked > -1) then begin
    Result := Images.Height div 2;
  end
  else Result := GlyphChecked.Height div 2;
end;

function TsCheckBox.GlyphMaskIndex(State: TCheckBoxState): smallint;
begin
  case State of
    cbChecked : Result := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinManager.ConstData.IndexGLobalInfo, s_GLobalInfo, s_CheckBoxChecked);
    cbUnchecked : Result := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinManager.ConstData.IndexGLobalInfo, s_GLobalInfo, s_CheckBoxUnChecked)
    else Result := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinManager.ConstData.IndexGLobalInfo, s_GLobalInfo, s_CheckBoxGrayed);
  end;
end;

function TsCheckBox.GlyphWidth: integer;
begin
  if Assigned(Images) and (ImgChecked > -1) and (ImgUnChecked > -1) then begin
    Result := Images.Width div 3;
  end
  else Result := GlyphChecked.Width div 3;
end;

{$IFDEF TNTUNICODE}
function TsCheckBox.IsCaptionStored: Boolean;
begin
  Result := TntControl_IsCaptionStored(Self)
end;

function TsCheckBox.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self)
end;
{$ENDIF}

procedure TsCheckBox.Loaded;
begin
  inherited;
  SkinData.Loaded;
  AdjustSize;
end;

procedure TsCheckBox.PaintControl(DC : HDC);
begin
  if not FCommonData.Updating and not (Assigned(FadeTimer) and FadeTimer.Enabled {and (FadeTimer.Iterations > FadeTimer.FadeLevel)}) then begin
    PrepareCache;
    UpdateCorners(FCommonData, 0);
    BitBlt(DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
  end;
end;

procedure TsCheckBox.PaintGlyph(Bmp: TBitmap);
var
  R : TRect;
  function CurrentMaskRect : TRect; begin
    if FPressed
      then Result := Rect(2 * GlyphWidth, 0, 3 * GlyphWidth, GlyphHeight)
      else if ControlIsActive(FCommonData) and not ReadOnly
        then Result := Rect(GlyphWidth, 0, 2 * GlyphWidth, GlyphHeight)
        else Result := Rect(0, 0, GlyphWidth, GlyphHeight);
  end;
begin
  if FCommonData.FCacheBmp.Width < 1 then exit;
  Bmp.PixelFormat := pf24bit;
  R := CheckRect;
  CopyByMask(Rect(R.Left, R.Top, R.Right, R.Bottom),
             CurrentMaskRect,
             FCommonData.FCacheBmp,
             Bmp, EmptyCI, True);
end;

procedure TsCheckBox.PaintHandler(M: TWMPaint);
var
  PS: TPaintStruct;
  DC : hdc;
  SavedDC: hdc;
begin
  DC := M.DC;
  if DC = 0 then DC := BeginPaint(Handle, PS);
  SavedDC := SaveDC(DC);
  try
    if not FCommonData.Updating then PaintControl(DC) else FCommonData.Updating := True;
  finally
    RestoreDC(DC, SavedDC);
    if M.DC = 0 then EndPaint(Handle, PS);
  end;
end;

procedure TsCheckBox.PrepareCache;
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
procedure TsCheckBox.SetCaption(const Value: TWideCaption);
begin
  TntControl_SetText(Self, Value);
end;
{$ENDIF}

procedure TsCheckBox.SetChecked(Value: Boolean);
begin
  if not (csLoading in ComponentState) then begin
    if (Value <> Checked) then FCommonData.BGChanged := True;
    inherited;
    if FCommonData.BGChanged then Repaint;
  end;
end;

{$IFNDEF DELPHI7UP}
procedure TsCheckBox.SetWordWrap(const Value: boolean);
begin
  if FWordWrap <> Value then begin
    FWordWrap := Value;
    FCommonData.BGChanged := True;
    if AutoSize then AutoSize := False;
    Repaint;
  end;
end;
{$ENDIF}

procedure TsCheckBox.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FCommonData.Invalidate;
  end;
end;

procedure TsCheckBox.SetGlyphChecked(const Value: TBitmap);
begin
  FGlyphChecked.Assign(Value);
  if AutoSize then AdjustSize;
  FCommonData.Invalidate;
end;

procedure TsCheckBox.SetGlyphUnChecked(const Value: TBitmap);
begin
  FGlyphUnChecked.Assign(Value);
  if AutoSize then AdjustSize;
  Invalidate;
end;

{$IFDEF TNTUNICODE}
procedure TsCheckBox.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;
{$ENDIF}

procedure TsCheckBox.SetImageChecked(const Value: TsImageIndex);
begin
  if FImgChecked <> Value then begin
    FImgChecked := Value;
    if AutoSize then AdjustSize;
    if Checked then SkinData.Invalidate;
  end;
end;

procedure TsCheckBox.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then begin
    FImages := Value;
    if AutoSize then AdjustSize;
    SkinData.Invalidate;
  end;
end;

procedure TsCheckBox.SetImageUnChecked(const Value: TsImageIndex);
begin
  if FImgUnchecked <> Value then begin
    FImgUnchecked := Value;
    if AutoSize then AdjustSize;
    if not Checked then SkinData.Invalidate;
  end;
end;

procedure TsCheckBox.SetMargin(const Value: integer);
begin
  if FMargin <> Value then begin
    FMargin := Value;
    if AutoSize then AdjustSize;
    Invalidate;
  end;
end;

procedure TsCheckBox.SetReadOnly(const Value: boolean);
begin
  FReadOnly := Value;
end;

procedure TsCheckBox.SetShowFocus(const Value: Boolean);
begin
  if FShowFocus <> Value then begin
    FShowFocus := Value;
    Invalidate;
  end;
end;

procedure TsCheckBox.SetTextIndent(const Value: integer);
begin
  if FTextIndent <> Value then begin
    FTextIndent := Value;
    if AutoSize then AdjustSize;
    Invalidate;
  end;
end;

function TsCheckBox.SkinCheckRect(i: integer): TRect;
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

function TsCheckBox.SkinGlyphHeight(i: integer): integer;
begin
  with FCommonData.SkinManager do if Assigned(ma[i].Bmp) then Result := ma[i].Bmp.Height div 2 else Result := HeightOf(ma[i].R) div (ma[i].MaskType + 1);
end;

function TsCheckBox.SkinGlyphWidth(i: integer): integer;
begin
  with FCommonData.SkinManager do if Assigned(ma[i].Bmp) then Result := ma[i].Bmp.Width div 3 else Result := WidthOf(ma[i].R) div ma[i].ImageCount;
end;

procedure TsCheckBox.WndProc(var Message: TMessage);
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
      if HandleAllocated then SendMessage(Handle, BM_SETCHECK, Integer(State), 0);
      if not (csDesigning in ComponentState) and (@Ac_SetWindowTheme <> nil) then Ac_SetWindowTheme(Handle, nil, nil);
      Repaint;
      exit
    end;
    AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      StopFading(FadeTimer, FCommonData);
      CommonWndProc(Message, FCommonData);
      AdjustSize;
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
          if not (csDesigning in ComponentState) then Repaint;
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
        CM_ENABLEDCHANGED : begin
          inherited;
          Repaint;
          Exit
        end;
        CM_CHANGED : begin 
          if not (csDesigning in ComponentState) then begin
            if Checked
              then DoChangePaint(FadeTimer, FCommonData, True, EventEnabled(aeMouseUp, FAnimatEvents), fdUp)
              else DoChangePaint(FadeTimer, FCommonData, True, EventEnabled(aeMouseUp, FAnimatEvents));
          end
          else FCommonData.Invalidate;
        end;
        BM_SETCHECK : begin
          if (FadeTimer <> nil) and (FadeTimer.FadeLevel < FadeTimer.Iterations) then StopFading(FadeTimer, FCommonData);
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
            Toggle;
          end else FPressed := False;
          if Assigned(OnKeyUp) then OnKeyUp(Self, TWMKey(Message).CharCode, KeysToShiftState(TWMKey(Message).KeyData));
          Exit;
        end;
        WM_LBUTTONUP : if not (csDesigning in ComponentState) and Enabled then begin
          if ReadOnly then Exit;
          if FPressed then begin
            FPressed := False;
            Toggle;
          end
          else FPressed := False;
          if Assigned(OnMouseUp) then OnMouseUp(Self, mbLeft, KeysToShiftState(TWMMouse(Message).Keys), TWMMouse(Message).XPos, TWMMouse(Message).YPos);
          Exit;
        end;
      end
    end else case Message.Msg of
      WM_KEYDOWN, WM_LBUTTONDOWN : FPressed := True;
      WM_KEYUP, WM_LBUTTONUP : FPressed := False;
      WM_LBUTTONDBLCLK : if ReadOnly then Exit;
      BM_SETSTATE, BM_SETCHECK : if not (csCreating in ControlState) and FPressed and ReadOnly then Exit;
    end;
    inherited;
  end;
end;

end.

