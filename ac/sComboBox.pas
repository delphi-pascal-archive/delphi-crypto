unit sComboBox;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  {$IFDEF TNTUNICODE}TntControls, TntClasses, TntActnList, TntStdCtrls, TntGraphics, {$ENDIF}
  StdCtrls, sConst, sDefaults, acSBUtils,
  sCommonData{$IFDEF LOGGED}, sDebugMsgs{$ENDIF};

type
{$IFNDEF NOTFORHELP}
{$IFDEF TNTUNICODE}
  TsCustomComboBox = class(TTntCustomComboBox)
{$ELSE}
  TsCustomComboBox = class(TCustomComboBox)
{$ENDIF}
  private
    FAlignment : TAlignment;
    FReadOnly: boolean;
    FDisabledKind: TsDisabledKind;
    FCommonData: TsCommonData;
    FBoundLabel: TsBoundLabel;
    FShowButton: boolean;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetReadOnly(const Value: boolean);
    procedure SetDisabledKind(const Value: TsDisabledKind);
    procedure SetShowButton(const Value: boolean);
  protected
    lboxhandle : hwnd;
    ListSW : TacScrollWnd;
    OldDropcountValue : integer;
    procedure PrepareCache;
    procedure PaintText; virtual;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure OurPaintHandler(iDC : hdc);
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure DrawSkinItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);

    procedure WndProc (var Message: TMessage); override;
    procedure ComboWndProc(var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer); override;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TMessage); message WM_LBUTTONDBLCLK;
  public
    FChildHandle: HWND;
    FDefListProc: Pointer;
    FDropDown : boolean;

    bFormHandle : hwnd;
    bFormDefProc: Pointer;

    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    function IndexOf(const s : string) : integer;
    procedure Invalidate; override;

    function ButtonRect: TRect;
    procedure PaintButton;
    function ButtonHeight : integer;

    procedure AfterConstruction; override;
    procedure Loaded; override;
    function Focused: Boolean; override;
    property ShowButton : boolean read FShowButton write SetShowButton default True;
  published
{$IFDEF D2009}
    property AutoCloseUp;
    property AutoDropDown;
{$ENDIF}
    property Align;
    property Anchors;
    property Alignment : TAlignment read FAlignment write SetAlignment;       //KJS
{$IFDEF D2005}
    property AutoCompleteDelay;
{$ENDIF}
    property BoundLabel : TsBoundLabel read FBoundLabel write FBoundLabel;
    property DisabledKind : TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property DropDownCount default 16;
    property SkinData : TsCommonData read FCommonData write FCommonData;
    property ReadOnly : boolean read FReadOnly write SetReadOnly default False;
  end;
{$ENDIF} // NOTFORHELP

  TsComboBox = class(TsCustomComboBox)
{$IFNDEF NOTFORHELP}
    property Style; {Must be published before Items}
{$IFDEF DELPHI7UP}
    property AutoComplete;
{$ENDIF}
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ItemIndex;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
{$IFDEF TNTUNICODE}
    property SelText;
    property SelStart;
    property SelLength;
{$ENDIF}
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
{$IFDEF DELPHI6UP}
    property OnCloseUp;
    property OnSelect;
{$ENDIF}
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDock;
    property OnStartDrag;
    property Items; { Must be published after OnMeasureItem }
{$ENDIF} // NOTFORHELP
    property BoundLabel;
    property DisabledKind;
    property SkinData;
    property ReadOnly;
  end;


implementation

uses sStyleSimply, sSkinProps, sVCLUtils, sMessages, sAlphaGraph, acntUtils, sGraphUtils, sSkinManager, acGlow;

var
  bFlag : boolean = False;

function IsOwnerDraw(Ctrl : TsCustomComboBox) : boolean;
begin
  Result := (Ctrl.Style in [csOwnerDrawFixed, csOwnerDrawVariable]) and Assigned(Ctrl.OnDrawItem)
end;

{ TsCustomComboBox }

procedure TsCustomComboBox.AfterConstruction;
begin
  inherited AfterConstruction;
  FCommonData.Loaded;
end;

function TsCustomComboBox.ButtonHeight: integer;
begin
  if FCommonData.Skinned and (FCommonData.SkinManager.ConstData.ComboGlyph > -1)
    then Result := HeightOf(FCommonData.SkinManager.ma[FCommonData.SkinManager.ConstData.ComboGlyph].R) div (1 + FCommonData.SkinManager.ma[FCommonData.SkinManager.ConstData.ComboGlyph].MaskType)
    else Result := 16;
end;

function TsCustomComboBox.ButtonRect: TRect;
const
  iMargin = 2;
var
  w : integer;
begin
  if (Style <> csSimple) and FShowButton then w := GetSystemMetrics(SM_CXVSCROLL) else w := 0;
  if UseRightToLeftAlignment then Result.Left := iMargin else Result.Left := Width - w - iMargin - 1;
  Result.Top := iMargin;
  Result.Right := Result.Left + w;
  Result.Bottom := Height - iMargin;
end;

procedure TsCustomComboBox.CNDrawItem(var Message: TWMDrawItem);
var
  b1, b2 : boolean;
  ds : TDrawItemStruct;
begin
  ds :=  Message.DrawItemStruct^;
  b1 := (ds.itemState and ODS_COMBOBOXEDIT = 0);
  b2 := ((ds.itemState and ODS_FOCUS <> 0) and not DroppedDown);
  if not SkinData.Skinned or b1 or b2 or DroppedDown then inherited else begin
    with Message.DrawItemStruct^ do BitBlt(hDC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
  end;
end;

procedure TsCustomComboBox.ComboWndProc(var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer);
var
  ps : TPaintStruct;
begin
{$IFDEF LOGGED}
//  AddToLog(Message);
{$ENDIF}
  if ReadOnly then begin
    case Message.Msg of
      WM_KEYDOWN, WM_CHAR, WM_KEYUP, WM_SYSKEYUP, CN_KEYDOWN, CN_CHAR, CN_SYSKEYDOWN,
        CN_SYSCHAR, WM_PASTE, WM_CUT, WM_CLEAR, WM_UNDO: Exit
      else
{$IFDEF TNTUNICODE}
//      if not TntCombo_ComboWndProc(Self, Message, ComboWnd, ComboProc, DoEditCharMsg) then
{$ENDIF}
    end
  end;
{$IFDEF TNTUNICODE}
//  if not TntCombo_ComboWndProc(Self, Message, ComboWnd, ComboProc, DoEditCharMsg) then
{$ENDIF}
  begin
    if FCommonData.Skinned then case Message.Msg of
      WM_ERASEBKGND, WM_NCPAINT : if (Style <> csSimple) and (not (Focused or FCommonData.FFocused) or not Enabled or ReadOnly) then begin
        Message.Result := 1;
        Exit;
      end;
      WM_PAINT : if (Style <> csSimple) and (not (Focused or FCommonData.FFocused) or not Enabled or ReadOnly) then begin
        BeginPaint(ComboWnd, PS);
{        if not FCommonData.BGChanged then begin
          DC := GetWindowDC(ComboWnd);
          if DroppedDown then
            BitBlt(DC, 0, 0, Width - 6, FCommonData.FCacheBmp.Height - 6, FCommonData.FCacheBmp.Canvas.Handle, 3, 3, SRCCOPY);
          ReleaseDC(ComboWnd, DC);
        end;}
        EndPaint(ComboWnd, PS);
        Exit;
      end;
    end;
    inherited;
  end;
end;

constructor TsCustomComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DropDownCount := 16;
  FCommonData := TsCommonData.Create(Self, True);
  FCommonData.COC := COC_TsEdit;
  if FCommonData.SkinSection = '' then FCommonData.SkinSection := s_ComboBox;
  FDisabledKind := DefDisabledKind;

  FBoundLabel := TsBoundLabel.Create(Self, FCommonData);
  FDropDown := False;
//  Canvas.Handle := 0;

  FReadOnly := False;
  FShowButton := True;

  FDefListProc := nil;
end;

destructor TsCustomComboBox.Destroy;
begin
  if lBoxHandle <> 0 then begin
    SetWindowLong(lBoxHandle, GWL_STYLE, GetWindowLong(lBoxHandle, GWL_STYLE) and not WS_THICKFRAME or WS_BORDER);
    UninitializeACScroll(lBoxHandle, True, False, ListSW);
    lBoxHandle := 0;
  end;
  FreeAndNil(FBoundLabel);
  if Assigned(FCommonData) then FreeAndNil(FCommonData);
  inherited Destroy;
end;

procedure TsCustomComboBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Offset : integer;
begin
  if FCommonData.Skinned and (odComboBoxEdit in State) then begin
    Canvas.Font.Assign(Font);
    if not IsOwnerDraw(Self) then BitBlt(Canvas.Handle, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
    if not (Enabled or IsOwnerDraw(Self)) or ReadOnly then Exit;
    if DroppedDown then begin
      if (odComboBoxEdit in State) then begin
        Canvas.Font.Color := Font.Color;
        Canvas.Brush.Style := bsClear;
      end
      else begin
        Canvas.Brush.Style := bsSolid;
        if not DroppedDown then begin
          Canvas.Font.Color := clHighLightText;
          Canvas.Brush.Color := clHighLight;
          Canvas.FillRect(Rect);
        end
        else begin
          Canvas.Font.Color := Font.Color;
          if FCommonData.Skinned or not (odComboBoxEdit in State) then Canvas.Brush.Style := bsClear else begin
            Canvas.Brush.Color := ColorToRGB(Color);
            Canvas.FillRect(Rect);
          end;
        end;
      end;
      if Assigned(OnDrawItem) then OnDrawItem(Self, Index, Rect, State) else begin
        TControlCanvas(Canvas).UpdateTextFlags;
        if Text <> '' then begin
          Offset := integer(Style <> csDropDown);
          acTextRect(Canvas, Rect, Rect.Left + Offset, Rect.Top + Offset, Items[Index]);
        end;
      end;
      if odFocused in State then Canvas.DrawFocusRect(Rect);
    end
    else begin
      Canvas.Brush.Style := bsSolid;
      if IsOwnerDraw(Self) then begin
        if (odFocused in State) then begin
          Canvas.Font.Color := clHighLightText;
          Canvas.Brush.Color := clHighLight;
        end
        else begin
          Canvas.Font.Color := Font.Color;
          Canvas.Brush.Color := Color;
        end;
        if Index > -1 then OnDrawItem(Self, Index, Rect, State);
      end
      else begin
        if not ((odSelected in State) or (odFocused in State) or FCommonData.FFocused or Focused) then Exit;
        Canvas.Font.Color := clHighLightText;
        Canvas.Brush.Color := clHighLight;

        Canvas.FillRect(Rect);

        TControlCanvas(Canvas).UpdateTextFlags;
        if Text <> '' then begin
          Offset := integer(Style <> csDropDown);
          acTextRect(Canvas, Rect, Rect.Left + Offset, Rect.Top + Offset, Items[Index]);
        end;
      end;
    end;
  end
  else begin
    Canvas.Font.Assign(Font);
    if ((odSelected in State) or (odFocused in State)) then begin
      Canvas.Font.Color := clHighLightText;
      Canvas.Brush.Color := clHighLight;
      Canvas.Brush.Style := bsSolid;
      if not IsOwnerDraw(Self) then Canvas.FillRect(Rect);
    end
    else begin
      if Enabled then Canvas.Font.Color := Font.Color else Canvas.Font.Color := clBtnShadow;
      Canvas.Brush.Color := ColorToRGB(Color);
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(Rect);
    end;
    if Assigned(OnDrawItem) then OnDrawItem(Self, Index, Rect, State) else begin
      TControlCanvas(Canvas).UpdateTextFlags;
      if Text <> '' then begin
        Offset := integer(Style <> csDropDown);
        acTextRect(Canvas, Rect, Rect.Left + Offset, Rect.Top + Offset, Items[Index]);
      end;
    end;
  end;
end;

procedure TsCustomComboBox.DrawSkinItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  R : TRect;
  Flags : Cardinal;
begin
  FCommonData.FCacheBmp.Canvas.Font.Assign(Font);
  if (odFocused in State) then begin
    FCommonData.FCacheBmp.Canvas.Font.Color := ColorToRGB(clHighlightText);
    FCommonData.FCacheBmp.Canvas.Brush.Color := ColorToRGB(clHighlight);
    FCommonData.FCacheBmp.Canvas.Brush.Style := bsSolid;
    FCommonData.FCacheBmp.Canvas.FillRect(Rect);
  end
  else begin
    FCommonData.FCacheBmp.Canvas.Font.Color := Font.Color;
    FCommonData.FCacheBmp.Canvas.Brush.Color := Color;
    FCommonData.FCacheBmp.Canvas.Brush.Style := bsClear;
  end;
  if Text <> '' then begin
    Flags := DT_NOPREFIX or DT_SINGLELINE or GetStringFlags(Self, Alignment);
    R := Classes.Rect(Rect.Left + integer(Style <> csDropDown), Rect.Top, Rect.Right, Rect.Bottom - integer(Style = csDropDown));
    acDrawText(FCommonData.FCacheBMP.Canvas.Handle, Text, R, Flags);
  end;
  if odFocused in State then DrawFocusRect(FCommonData.FCacheBmp.Canvas.Handle, Rect);
end;

function TsCustomComboBox.Focused: Boolean;
var
  FocusedWnd: HWND;
begin
  Result := False;
  if HandleAllocated then begin
    FocusedWnd := GetFocus;
    Result := (FocusedWnd <> 0) and ((FocusedWnd = EditHandle) or (FocusedWnd = ListHandle)) or FCommonData.FFocused;
  end;
end;

function TsCustomComboBox.IndexOf(const s: string): integer;
var
  i : integer;
begin
  Result := -1;
  for i := 0 to Items.Count - 1 do if Items[i] = s then begin
    Result := i;
    Exit;
  end;
end;

procedure TsCustomComboBox.Invalidate;
begin
  if Focused then FCommonData.FFocused := True;
  inherited Invalidate;
end;

procedure TsCustomComboBox.Loaded;
begin
  inherited Loaded;
  FCommonData.Loaded;
  if FCommonData.Skinned then begin
    if not FCommonData.CustomColor then Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].Color;
    if not FCommonData.CustomFont then Font.Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].FontColor[1];
  end;
end;

procedure TsCustomComboBox.OurPaintHandler(iDC : hdc);
const
  BordWidth = 3;
var
  DC : hdc;
  R : TRect;
  State : TOwnerDrawState;
begin
  if not Showing then Exit;
  if iDC = 0 then DC := GetDC(Handle) else DC := iDC;
  R := ButtonRect;
  try
    FCommonData.Updating := FCommonData.Updating;
    if not (InAnimationProcess and (DC <> acPrintDC)) and not FCommonData.Updating then begin
      FCommonData.BGChanged := FCommonData.BGChanged or FCommonData.HalfVisible or GetBoolMsg(Parent, AC_GETHALFVISIBLE) or IsOwnerDraw(Self);
      FCommonData.HalfVisible := not RectInRect(Parent.ClientRect, BoundsRect);

      if FCommonData.BGChanged then PrepareCache;
      UpdateCorners(FCommonData, 0);

      if Enabled or IsOwnerDraw(Self) then case Style of
        csSimple : begin
          BitBltBorder(DC, 0, 0, Width, FCommonData.FCacheBmp.Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, BordWidth);
        end;
        csDropDown : begin
          if Focused then begin
            BitBltBorder(DC, 0, 0, Width, FCommonData.FCacheBmp.Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, BordWidth);
            R := ButtonRect;
            BitBlt(DC, R.Left, R.Top, WidthOf(R), HeightOf(R), FCommonData.FCacheBmp.Canvas.Handle, R.Left, R.Top, SRCCOPY);
          end
          else BitBlt(DC, 0, 0, Width, FCommonData.FCacheBmp.Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
        end;
        csDropDownList : begin
          BitBlt(DC, 0, 0, Width, FCommonData.FCacheBmp.Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
        end;
        csOwnerDrawFixed, csOwnerDrawVariable : begin
//          R := ButtonRect;
          R := Rect(BordWidth, BordWidth, ButtonRect.Left, FCommonData.FCacheBmp.Height - BordWidth);
          State := [odComboBoxEdit];
          if (Focused or SkinData.FFocused) and not (Style in [csDropDown, csSimple]) then State := State + [odFocused, odSelected];
          Canvas.Handle := FCommonData.FCacheBmp.Canvas.Handle;
          FCommonData.FCacheBmp.Canvas.Lock;
          DrawItem(ItemIndex, R, State);
          FCommonData.FCacheBmp.Canvas.Unlock;
          if not Enabled then BmpDisabledKind(FCommonData.FCacheBmp, FDisabledKind, Parent, GetParentCache(FCommonData), Point(Left, Top));
          BitBlt(DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
          Canvas.Handle := 0;
        end;
      end
      else begin
        BitBlt(DC, 0, 0, Width, FCommonData.FCacheBmp.Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
      end;

{$IFDEF DYNAMICCACHE}
    if Assigned(FCommonData.FCacheBmp) then FreeAndNil(FCommonData.FCacheBmp);
{$ENDIF}
    end;
  finally
    if iDC = 0 then ReleaseDC(Handle, DC);
  end;
end;

procedure TsCustomComboBox.PaintButton;
var
  R : TRect;
  Mode : integer;
  c : TsColor;
  glIndex : integer;
  TmpBtn : TBitmap;
begin
  if FDropDown then Mode := 2 else if ControlIsActive(FCommonData) then Mode := 1 else Mode := 0;
  R := ButtonRect;

  if FCommonData.SkinManager.ConstData.ComboBtnIndex > -1 then begin
    TmpBtn := CreateBmpLike(FCommonData.FCacheBmp);
    BitBlt(TmpBtn.Canvas.Handle, 0, 0, TmpBtn.Width, TmpBtn.Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
    PaintItem(FCommonData.SkinManager.ConstData.ComboBtnIndex, s_ComboBtn, MakeCacheInfo(FCommonData.FCacheBmp),
      True, Mode, R, Point(0, 0){R.TopLeft}, FCommonData.FCacheBmp, FCommonData.SkinManager, FCommonData.SkinManager.ConstData.ComboBtnBG, FCommonData.SkinManager.ConstData.ComboBtnBGHot);
    FreeAndNil(TmpBtn);
  end;
  glIndex := FCommonData.SkinManager.ConstData.ComboGlyph;
  if glIndex > -1 then begin
    if ControlIsActive(FCommonData)
      then c.C := FCommonData.SkinManager.gd[FCommonData.SkinIndex].HotColor
      else c.C := FCommonData.SkinManager.gd[FCommonData.SkinIndex].Color;

    DrawSkinGlyph(FCommonData.FCacheBmp,
      Point(R.Left + (WidthOf(R) - WidthOf(FCommonData.SkinManager.ma[glIndex].R) div FCommonData.SkinManager.ma[glIndex].ImageCount) div 2,
            (Height - ButtonHeight) div 2), Mode, 1, FCommonData.SkinManager.ma[FCommonData.SkinManager.ConstData.ComboGlyph], MakeCacheInfo(SkinData.FCacheBmp));
  end;
end;

procedure TsCustomComboBox.PaintText;
begin
end;

procedure TsCustomComboBox.PrepareCache;
const
  BordWidth = 3;
var
  R : TRect;
  State : TOwnerDrawState;
begin
  FCommonData.InitCacheBmp;
  if Style <> csSimple then begin
    PaintItem(FCommonData,
                GetParentCache(FCommonData), True,
                integer(ControlIsActive(FCommonData)),
                Rect(0, 0, Width, Height),
                Point(Left, top), FCommonData.FCacheBmp, False);
    if FShowButton then PaintButton;

    if not IsOwnerDraw(Self) then begin
      if not UseRightToLeftAlignment
        then R := Rect(BordWidth, BordWidth, ButtonRect.Left - 1, Height - BordWidth)
        else R := Rect(WidthOf(ButtonRect) + BordWidth + 1, BordWidth, Width - BordWidth + 1, Height - BordWidth);
      State := [odComboBoxEdit];
      if (Focused or SkinData.FFocused) and (Style = csDropDownList) and Enabled
        then State := State + [odFocused, odSelected];
      DrawSkinItem(ItemIndex, R, State);
    end;
  end
  else begin
    FCommonData.FCacheBmp.Height := ItemHeight + 8;
    PaintItem(FCommonData,
                GetParentCache(FCommonData), True,
                integer(ControlIsActive(FCommonData)),
                Rect(0, 0, Width, FCommonData.FCacheBmp.Height),
                Point(Left, top), FCommonData.FCacheBmp, False);
  end;
  if not Enabled and not IsOwnerDraw(Self) then BmpDisabledKind(FCommonData.FCacheBmp, FDisabledKind, Parent, GetParentCache(FCommonData), Point(Left, Top));
  FCommonData.BGChanged := False;
end;

procedure TsCustomComboBox.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    FCommonData.Invalidate;
  end;
end;

procedure TsCustomComboBox.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FCommonData.Invalidate;
  end;
end;

procedure TsCustomComboBox.SetReadOnly(const Value: boolean);
begin
  if FReadOnly <> Value then begin
    FReadOnly := Value;
  end;
end;

procedure TsCustomComboBox.WMLButtonDblClk(var Message: TMessage);
begin
  if FReadOnly then begin
    SetFocus;
    if Assigned(OnDblClick) then OnDblClick(Self);
  end
  else inherited;
end;

procedure TsCustomComboBox.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if FReadOnly then SetFocus else inherited
end;

procedure TsCustomComboBox.SetShowButton(const Value: boolean);
begin
  if FShowButton <> Value then begin
    FShowButton := Value;
    FCommonData.Invalidate;
  end;
end;

procedure TsCustomComboBox.WndProc(var Message: TMessage);
var
  PS : TPaintStruct;
  DC : hdc;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_CTRLHANDLED : begin Message.Result := 1; Exit end; // AlphaSkins supported
    AC_GETAPPLICATION : begin Message.Result := longint(Application); Exit end;
    AC_REMOVESKIN : if LongWord(Message.LParam) = LongWord(SkinData.SkinManager) then begin
      CommonWndProc(Message, FCommonData);
      if not FCommonData.CustomColor then Color := clWindow;
      if not FCommonData.CustomFont then Font.Color := clWindowText;
      if Assigned(ListSW) then begin
        FreeAndNil(ListSW);
        lBoxHandle := 0;
      end;
      RecreateWnd;
      exit
    end;
    AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FCommonData);
      if FCommonData.Skinned then begin
        if not FCommonData.CustomColor then Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].Color;
        if not FCommonData.CustomFont
          then Font.Color := ColorToRGB(FCommonData.SkinManager.gd[FCommonData.SkinIndex].FontColor[1]);
      end;
      Repaint;
      exit
    end;
    AC_SETNEWSKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FCommonData);
      if ListSW <> nil then ListSW.acWndProc(Message);
      exit
    end;
    AC_ENDPARENTUPDATE : if FCommonData.Updating then begin
      FCommonData.Updating := False;
      Repaint;
      Exit
    end;
  end
  else case Message.Msg of
    WM_SYSCHAR, WM_SYSKEYDOWN, CN_SYSCHAR, CN_SYSKEYDOWN, WM_KEYDOWN, CN_KEYDOWN : case TWMKey(Message).CharCode of
      VK_SPACE..VK_DOWN, $39..$39, $41..$5A :
      if ReadOnly then Exit;
    end;
    WM_CHAR : if ReadOnly then Exit;
    WM_COMMAND, CN_COMMAND : if (Message.WParam = CBN_DROPDOWN) and ReadOnly then Exit;
  end;
  if not ControlIsReady(Self) or not FCommonData.Skinned then inherited else begin
    case Message.Msg of
      CM_COLORCHANGED, CM_MOUSEWHEEL : FCommonData.BGChanged := True;
      CN_COMMAND : case TWMCommand(Message).NotifyCode of
        CBN_CLOSEUP : begin
          FDropDown := False;
          FCommonData.BGChanged := True;
          OurPaintHandler(0);
        end;
        CBN_DROPDOWN : FDropDown := True;
      end;
      WM_SETFOCUS, CM_ENTER : if CanFocus then begin
        FCommonData.FFocused := True;
        FCommonData.FMouseAbove := False;
        FCommonData.BGChanged := True;
        inherited;
        Exit
      end;
      WM_KILLFOCUS, CM_EXIT: begin
        DroppedDown := False;
        FCommonData.FFocused := False;
        FCommonData.FMouseAbove := False;
        FCommonData.BGChanged := True;
        Repaint;
        inherited;
        Exit
      end;
      WM_NCPAINT : begin
        if InanimationProcess then OurPaintHandler(0);
        Exit
      end;
      WM_PAINT : begin
//        if Focused {and DroppedDown {v5.01} then inherited;
        BeginPaint(Handle, PS);
        if not InAnimationProcess then begin
          if TWMPaint(Message).DC = 0 then DC := GetDC(Handle) else DC := TWMPaint(Message).DC;
          OurPaintHandler(DC);
          if TWMPaint(Message).DC = 0 then ReleaseDC(Handle, DC);
        end;
        EndPaint(Handle, PS);
        Exit
      end;
      CM_MOUSEENTER, CM_MOUSELEAVE : if not DroppedDown then begin
        FCommonData.FMouseAbove := Message.Msg = CM_MOUSEENTER;
        FCommonData.BGChanged := True;
        bFlag := True;
        Repaint;
        inherited;
        if FCommonData.FMouseAbove then begin
          if SkinData.GlowID <> -1 then HideGlow(SkinData.GlowID);
          ShowGlowingIfNeeded(SkinData)
        end
        else ClearGlows;
        bFlag := False;
        exit;
      end;
      WM_COMMAND : if ReadOnly then Exit else begin
        FDropDown := False;
        OurPaintHandler(0);
      end;
{removed by dia $IFNDEF TNTUNICODE}
      WM_CTLCOLORLISTBOX : if not (csLoading in ComponentState) and (lBoxHandle = 0) then begin
        if Items.Count > DropDownCount then begin
          lBoxHandle := hwnd(Message.LParam);
          ListSW := TacComboListWnd.Create(lboxhandle, nil, SkinData.SkinManager, s_Edit);
          if Style = csSimple then TacComboListWnd(ListSW).SimplyBox := True else TacComboListWnd(ListSW).SimplyBox := False;
        end;
      end;
{removed by dia $ENDIF}
      CM_VISIBLECHANGED, CM_ENABLEDCHANGED, WM_SETFONT : begin
        FCommonData.BGChanged := True;
      end;
      WM_PRINT : begin
        try
          SkinData.Updating := False;
//          if SkinData.BGChanged then PrepareCache;
//          UpdateCorners(SkinData, 0);
          try
//            bw := (1 + integer(Ctl3d));
//            BitBltBorder(DC, 0, 0, SkinData.FCacheBmp.Width, SkinData.FCacheBmp.Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, bw);
            OurPaintHandler(TWMPaint(Message).DC);
          finally
          end;
          Exit;
        except
        end;
      end;
    end;
    CommonWndProc(Message, FCommonData);
    inherited;
    if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
      AC_DROPPEDDOWN : Message.Result := integer(DroppedDown);
    end
    else case Message.Msg of
      CN_COMMAND : case TWMCommand(Message).NotifyCode of
        CBN_CLOSEUP : begin
        end;
      end;
      CB_SETCURSEL : begin
        FCommonData.BGChanged := True;
        Repaint
      end;
      CM_CHANGED, CM_TEXTCHANGED : begin
        FCommonData.BGChanged := True;
        Repaint;
      end;
    end;
  end;
  // Aligning of the bound label
  if Assigned(BoundLabel) and Assigned(BoundLabel.FtheLabel) then case Message.Msg of
    WM_SIZE, WM_WINDOWPOSCHANGED : begin BoundLabel.AlignLabel end;
    CM_VISIBLECHANGED : begin BoundLabel.FtheLabel.Visible := Visible; BoundLabel.AlignLabel end;
    CM_ENABLEDCHANGED : begin BoundLabel.FtheLabel.Enabled := Enabled; BoundLabel.AlignLabel end;
    CM_BIDIMODECHANGED : begin BoundLabel.FtheLabel.BiDiMode := BiDiMode; BoundLabel.AlignLabel end;
  end;
end;

end.


