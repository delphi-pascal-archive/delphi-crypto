unit sScrollBar;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  Consts, sPanel, acntUtils, sConst, extctrls, sCommonData, sDefaults, sSkinManager{$IFDEF DELPHI6UP}, Types{$ENDIF};

type

  TsScrollBar = class(TScrollBar)
  private
    FBtn1Rect : TRect;
    FBtn2Rect : TRect;
    FBar1Rect : TRect;
    FBar2Rect : TRect;
    FSliderRect : TRect;
    Timer : TTimer;
    FBtn1State: integer;
    FBar2State: integer;
    FBtn2State: integer;
    FBar1State: integer;
    FSliderState : integer;
    FCommonData: TsCommonData;
    FDisabledKind: TsDisabledKind;
    MustBeRecreated : boolean;
    FSI : TScrollInfo;
    FCurrPos : integer;
    FBeginTrack : boolean;
    function NotRightToLeft: Boolean;

    procedure CNHScroll(var Message: TWMHScroll); message CN_HSCROLL;
    procedure CNVScroll(var Message: TWMVScroll); message CN_VSCROLL;

    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure SetInteger(Index : integer; Value: integer);
    procedure SetDisabledKind(const Value: TsDisabledKind);
    function GetSkinManager: TsSkinManager;
    procedure SetSkinManager(const Value: TsSkinManager);
  protected
    CI : TCacheInfo;
    AppShowHint : boolean;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WndProc(var Message: TMessage); override;

    procedure Paint(MsgDC : hdc);
    procedure PlaceToLinked;

    procedure InitDontChange;
    procedure ClearDontChange;

    procedure DrawBtnTop(b : TBitmap);
    procedure DrawBtnLeft(b : TBitmap);
    procedure DrawBtnRight(b : TBitmap);
    procedure DrawBtnBottom(b : TBitmap);
    procedure DrawSlider(b : TBitmap);

    function Bar1Rect : TRect;
    function Bar2Rect : TRect;
    function Btn1Rect : TRect;
    function Btn2Rect : TRect;
    function Btn1DRect : TRect;
    function Btn2DRect : TRect;
    function WorkSize : integer;
    function SliderRect : TRect;
    function SliderSize : integer;
    function Btn1SkinIndex : integer;
    function Btn2SkinIndex : integer;
    function CoordToPoint(p : TPoint) : TPoint;
    function CoordToPosition(p : TPoint) : integer;
    function PositionToCoord : integer;
    function FirstPoint : integer;
    function BarIsHot : boolean;
    procedure PrepareTimer;
    procedure PrepareBtnTimer;
    procedure PrepareBarTimer;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure IncPos(Offset : integer);
    procedure SetPos(Pos : integer);
  public
    ScrollCode : integer;
    RepaintNeeded : boolean;
    MouseOffset : integer;
    DrawingForbidden : boolean;
    LinkedControl : TWinControl;
    DontChange : boolean;
    DoSendChanges : boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Loaded; override;
    procedure UpdateBar;
    procedure OnTimer(Sender : TObject);
    procedure OnBtnTimer(Sender : TObject);
    procedure OnBarTimer(Sender : TObject);
    property Btn1State : integer index 0 read FBtn1State write SetInteger;
    property Btn2State : integer index 1 read FBtn2State write SetInteger;
    property Bar1State : integer index 2 read FBar1State write SetInteger;
    property Bar2State : integer index 3 read FBar2State write SetInteger;
    property SliderState : integer index 4 read FSliderState write SetInteger;
    property SkinData : TsCommonData read FCommonData write FCommonData;
  published
    property DisabledKind : TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property SkinManager : TsSkinManager read GetSkinManager write SetSkinManager;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  function UpdateControlScrollBar(Control : TWinControl; var ScrollBar : TsScrollBar; Kind : TScrollBarKind; Free : boolean = true) : boolean;

var
  Log : TStrings;

implementation

uses sGraphUtils, sBorders, sSkinProps, math{$IFDEF LOGGED}, sDebugMsgs{$ENDIF},
  sMessages, commctrl, sMaskData, sStyleSimply, sVclUtils{$IFDEF CHECKXP}, UxTheme, Themes{$ENDIF},
  sAlphaGraph;

var
  p : TPoint;
  SkinnedRecreate : boolean = False;

type

{$HINTS OFF}
  TScrollBar_ = class(TWinControl)
  private
    FPageSize: Integer;
    FRTLFactor: Integer;
  end;
{$HINTS ON}

function Skinned(sb : TsScrollBar): boolean;
begin
  if not Assigned(sb.SkinData.SkinManager) then sb.SkinData.SkinManager := DefaultManager;
  if Assigned(sb.SkinData.SkinManager) and sb.SkinData.SkinManager.SkinData.Active
    then Result := True
    else Result := False;
end;

function UpdateControlScrollBar(Control : TWinControl; var ScrollBar : TsScrollBar; Kind : TScrollBarKind; Free : boolean = true) : boolean;
const
  SysConsts: array[TScrollBarKind] of Integer = (SM_CXHSCROLL, SM_CXVSCROLL);
  Kinds: array[TScrollBarKind] of DWORD = (SB_HORZ, SB_VERT);
var
  SI : TScrollInfo;
  function HaveScroll(Handle : hwnd; fnBar : integer) : boolean;
  var
    Style : UINT;
  begin
    Style := GetWindowLong(Handle, GWL_STYLE);
    case fnBar of
      SB_VERT : Result := (Style and WS_VSCROLL) <> 0;
      SB_HORZ : Result := (Style and WS_HSCROLL) <> 0;
      SB_BOTH : Result := ((Style and WS_VSCROLL) <> 0) and ((Style and WS_HSCROLL) <> 0)
      else Result := False
    end;
  end;
  function GetScrollInfo(Handle: HWND; Kind: Integer; Mask : Cardinal; var ScrollInfo: TScrollInfo): boolean;
  begin
    Result := HaveScroll(Handle, Kind);
    if Result then begin
      ScrollInfo.cbSize := SizeOf(TScrollInfo);
      ScrollInfo.fMask := Mask;
      Result := Windows.GetScrollInfo(Handle, Kind, ScrollInfo);
    end;
  end;
begin
  result := false;
  if Control.Visible then begin
    if GetScrollInfo(Control.Handle, Kinds[Kind], SIF_ALL, SI) then begin
      if ScrollBar = nil then begin
        ScrollBar := TsScrollBar.Create(Control);
        ScrollBar.Visible          := False;
        ScrollBar.LinkedControl    := Control;
        ScrollBar.DoSendChanges    := true;
        ScrollBar.DrawingForbidden := True;
        ScrollBar.TabStop          := False;
        ScrollBar.Kind             := Kind;
        ScrollBar.Parent           := Control.Parent;
      end;
      result := true;
    end else begin
      if Assigned(ScrollBar) and Free then FreeAndNil(ScrollBar);
    end;
  end
  else begin
    if Assigned(ScrollBar) then FreeAndNil(ScrollBar);
  end;
end;

{ TsScrollBar }

procedure TsScrollBar.AfterConstruction;
var
  OldPos : integer;
begin
  inherited;
  FCommonData.Loaded;
{$IFDEF CHECKXP}
  if UseThemes and not (SkinData.Skinned and SkinData.SkinManager.SkinData.Active) then begin
    ControlStyle := ControlStyle - [csParentBackground]; // Patching of bug with TGraphicControls repainting when XPThemes used
  end;
{$ENDIF}
  if MustBeRecreated then begin // Control must be recreated for the skinned mode using without std blinking
    MustBeRecreated := False;
    SkinnedRecreate := True;
    OldPos := Position;
    RecreateWnd;
    Position := OldPos;
    SkinnedRecreate := False;
  end;
end;

function TsScrollBar.Btn1Rect: TRect;
begin
  FBtn1Rect.Left := 0;
  FBtn1Rect.Top := 0;
  if Kind = sbHorizontal then begin
    FBtn1Rect.Right := GetSystemMetrics(SM_CXHSCROLL);
    FBtn1Rect.Bottom := Height;
    if WidthOf(FBtn1Rect) > Width div 2 then FBtn1Rect.Right := Width div 2;
  end
  else begin
    FBtn1Rect.Right := Width;
    FBtn1Rect.Bottom := GetSystemMetrics(SM_CYVSCROLL);
    if HeightOf(FBtn1Rect) > Height div 2 then FBtn1Rect.Bottom := Height div 2;
  end;
  Result := FBtn1Rect;
end;

function TsScrollBar.Btn1SkinIndex: integer;
begin
  if Kind = sbHorizontal
    then Result := FCommonData.SkinManager.ConstData.IndexScrollLeft
    else Result := FCommonData.SkinManager.ConstData.IndexScrollTop;
end;

function TsScrollBar.Btn2Rect: TRect;
begin
  if Kind = sbHorizontal then begin
    FBtn2Rect.Left := Width - GetSystemMetrics(SM_CXHSCROLL);
    FBtn2Rect.Top := 0;
    FBtn2Rect.Right := Width;
    FBtn2Rect.Bottom := Height;
    if WidthOf(FBtn2Rect) > Width div 2 then FBtn2Rect.Left := Width div 2;
  end
  else begin
    FBtn2Rect.Left := 0;
    FBtn2Rect.Top := Height - GetSystemMetrics(SM_CYVSCROLL);
    FBtn2Rect.Right := Width;
    FBtn2Rect.Bottom := Height;
    if HeightOf(FBtn2Rect) > Height div 2 then FBtn2Rect.Top := Height div 2;
  end;
  Result := FBtn2Rect;
end;

function TsScrollBar.Btn2SkinIndex: integer;
begin
  if Kind = sbHorizontal
    then Result := FCommonData.SkinManager.ConstData.IndexScrollRight
    else Result := FCommonData.SkinManager.ConstData.IndexScrollBottom;
end;

function TsScrollBar.CoordToPoint(p: TPoint): TPoint;
begin
  Result := ScreenToClient(P);
end;

function TsScrollBar.CoordToPosition(p: TPoint): integer;
begin
  if Enabled then begin
    if Kind = sbHorizontal
      then Result := Round((p.x - GetSystemMetrics(SM_CXHSCROLL) - SliderSize / 2) * (FSI.nMax - FSI.nMin- Math.Max(Integer(FSI.nPage) -1,0)) / (Width - 2 * GetSystemMetrics(SM_CXHSCROLL) - SliderSize))
      else Result := Round((p.y - GetSystemMetrics(SM_CYVSCROLL) - SliderSize / 2) * (FSI.nMax - FSI.nMin- Math.Max(Integer(FSI.nPage) -1,0)) / (Height - 2 * GetSystemMetrics(SM_CYVSCROLL) - SliderSize));
  end
  else Result := 0;
end;

constructor TsScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommonData := TsCommonData.Create(TWinControl(Self), True);
  CI.Bmp := FCommonData.FCacheBmp;
  CI.Ready := True;
  CI.x := 0;
  CI.Y := 0;
  FCommonData.COC := COC_TsScrollBar;
  FSI.nMax := FSI.nMin - 1;

  Btn1State := 0;
  Btn2State := 0;
  Bar1State := 0;
  Bar2State := 0;

  FBtn1Rect.Right := 0;
  FBtn2Rect.Right := 0;
  FDisabledKind := DefDisabledKind;
end;

procedure TsScrollBar.CreateParams(var Params: TCreateParams);
  procedure DefCreateParams(var Params: TCreateParams);
  var
    FText : string;
    FLeft,
    FTop,
    FWidth,
    FHeight : integer;
  begin
    FillChar(Params, SizeOf(Params), 0);
    FText := Text;
    FLeft := Left;
    FTop  := Top;
    FWidth:= Width;
    FHeight:= Height;
    with Params do begin
      Caption := PChar(FText);
      Style := WS_CHILD or WS_CLIPSIBLINGS;
      AddBiDiModeExStyle(ExStyle);
      if csAcceptsControls in ControlStyle then begin
        Style := Style or WS_CLIPCHILDREN;
        ExStyle := ExStyle or WS_EX_CONTROLPARENT;
      end;
      if not (csDesigning in ComponentState) and not Enabled then Style := Style or WS_DISABLED;
      if TabStop then Style := Style or WS_TABSTOP;
      X := FLeft;
      Y := FTop;
      Width := FWidth;
      Height := FHeight;
      if Parent <> nil then WndParent := Parent.Handle else WndParent := ParentWindow;
      WindowClass.style := CS_VREDRAW + CS_HREDRAW + CS_DBLCLKS;
      WindowClass.lpfnWndProc := @DefWindowProc;
      WindowClass.hCursor := LoadCursor(0, IDC_ARROW);
      WindowClass.hbrBackground := 0;
      WindowClass.hInstance := HInstance;
      StrPCopy(WinClassName, ClassName);
    end;
  end;
begin
  if SkinnedRecreate then begin
    DefCreateParams(Params);
    if NotRightToLeft then TScrollBar_(Self).FRTLFactor := 1 else TScrollBar_(Self).FRTLFactor := -1;
  end
  else begin
    inherited CreateParams(Params);
    if Skinned(Self) then MustBerecreated := True;
  end;
end;

destructor TsScrollBar.Destroy;
begin
  if Assigned(Timer) then begin
    Timer.Enabled := False;
    FreeAndNil(Timer);
  end;
  if Assigned(FCommonData) then FreeAndNil(FCommonData);
  inherited Destroy;
end;

procedure TsScrollBar.DrawBtnBottom(b: TBitmap);
begin
  with FCommonData.SkinManager.ConstData do begin
    Ci.Bmp := b;
    PaintItemFast(IndexScrollBottom, MaskScrollBottom, IndexBGScrollBottom, IndexBGHotScrollBottom, s_ScrollBtnBottom, Ci, True,
      Btn2State, Btn2DRect, Point(0, 0), b, SkinData.SkinManager);
    Ci.Bmp := FCommonData.FCacheBmp;

    if FCommonData.SkinManager.IsValidImgIndex(MaskArrowBottom) then
      with FCommonData.SkinManager do begin
      if ma[MaskArrowBottom].Bmp = nil then begin
        p.x := FBtn2Rect.Left + (WidthOf(FBtn2Rect) - WidthOf(ma[MaskArrowBottom].R) div ma[MaskArrowBottom].ImageCount) div 2;// + integer(Btn2State = 2);
        p.y := FBtn2Rect.Top + (HeightOf(FBtn2Rect) - HeightOf(ma[MaskArrowBottom].R) div (1 + ma[MaskArrowBottom].MaskType)) div 2;// + integer(Btn2State = 2);
      end
      else if (ma[MaskArrowBottom].Bmp.Height div 2 < HeightOf(FBtn2Rect)) then begin
        p.x := FBtn2Rect.Left + (WidthOf(FBtn2Rect) - ma[MaskArrowBottom].Bmp.Width div 3) div 2;// + integer(Btn2State = 2);
        p.y := FBtn2Rect.Top + (HeightOf(FBtn2Rect) - ma[MaskArrowBottom].Bmp.Height div 2) div 2;// + integer(Btn2State = 2);
      end;
      if (p.x < 0) or (p.y < 0) then Exit;
      DrawSkinGlyph(b, p, Btn2State, 1, ma[MaskArrowBottom], CI);
    end;
  end;
end;

procedure TsScrollBar.DrawBtnLeft(b: TBitmap);
begin
  with FCommonData.SkinManager.ConstData do begin
    Ci.Bmp := b;
    PaintItemFast(IndexScrollLeft, MaskScrollLeft, IndexBGScrollLeft, IndexBGHotScrollLeft, s_ScrollBtnLeft, Ci, True,
      Btn1State, Btn1DRect, Point(0, 0), b, SkinData.SkinManager);
    Ci.Bmp := FCommonData.FCacheBmp;

    if Assigned(FCommonData.SkinManager) and FCommonData.SkinManager.IsValidImgIndex(MaskArrowLeft) then
      with FCommonData.SkinManager do begin
      if ma[MaskArrowLeft].Bmp = nil then begin
        p.x := FBtn1Rect.Left + (WidthOf(FBtn1Rect) - WidthOf(ma[MaskArrowLeft].R) div ma[MaskArrowLeft].ImageCount) div 2;// + integer(Btn1State = 2);
        p.y := FBtn1Rect.Top + (HeightOf(FBtn1Rect) - HeightOf(ma[MaskArrowLeft].R) div (1 + ma[MaskArrowLeft].MaskType)) div 2;// + integer(Btn1State = 2);
      end
      else if (ma[MaskArrowLeft].Bmp.Width div 3 < WidthOf(FBtn1Rect)) then begin
        p.x := FBtn1Rect.Left + (WidthOf(FBtn1Rect) - ma[MaskArrowLeft].Bmp.Width div 3) div 2;// + integer(Btn1State = 2);
        p.y := FBtn1Rect.Top + (HeightOf(FBtn1Rect) - ma[MaskArrowLeft].Bmp.Height div 2) div 2;// + integer(Btn1State = 2);
      end;
      if (p.x < 0) or (p.y < 0) then Exit;
      DrawSkinGlyph(b, p, Btn1State, 1, ma[MaskArrowLeft], CI);
    end;
  end;
end;

procedure TsScrollBar.DrawBtnRight(b: TBitmap);
begin
  with FCommonData.SkinManager.ConstData do begin
    Ci.Bmp := b;
    PaintItemFast(IndexScrollRight, MaskScrollRight, IndexBGScrollRight, IndexBGHotScrollRight, s_ScrollBtnRight, Ci, True,
      Btn2State, Btn2DRect, Point(0, 0), b, SkinData.SkinManager);
    Ci.Bmp := FCommonData.FCacheBmp;

    if Assigned(FCommonData.SkinManager) and FCommonData.SkinManager.IsValidImgIndex(MaskArrowRight) then
      with FCommonData.SkinManager do begin
      if ma[MaskArrowRight].Bmp = nil then begin
        p.x := FBtn2Rect.Left + (WidthOf(FBtn2Rect) - WidthOf(ma[MaskArrowRight].R) div ma[MaskArrowRight].ImageCount) div 2;// + integer(Btn2State = 2);
        p.y := FBtn2Rect.Top + (HeightOf(FBtn2Rect) - HeightOf(ma[MaskArrowRight].R) div (1 + ma[MaskArrowRight].MaskType)) div 2;// + integer(Btn2State = 2);
      end
      else if (ma[MaskArrowRight].Bmp.Width div 3 < WidthOf(FBtn1Rect)) then begin
        p.x := FBtn2Rect.Left + (WidthOf(FBtn2Rect) - ma[MaskArrowRight].Bmp.Width div 3) div 2;// + integer(Btn2State = 2);
        p.y := FBtn2Rect.Top + (HeightOf(FBtn2Rect) - ma[MaskArrowRight].Bmp.Height div 2) div 2;// + integer(Btn2State = 2);
      end;
      if (p.x < 0) or (p.y < 0) then Exit;
      DrawSkinGlyph(b, p, Btn2State, 1, ma[MaskArrowRight], CI);
    end;
  end;
end;

procedure TsScrollBar.DrawBtnTop(b: TBitmap);
begin
  with FCommonData.SkinManager.ConstData do begin
    Ci.Bmp := b;
    PaintItemFast(IndexScrollTop, MaskScrollTop, IndexBGScrollTop, IndexBGHotScrollTop, s_ScrollBtnTop, Ci, True,
      Btn1State, Btn1DRect, Point(0, 0), b, SkinData.SkinManager);
    Ci.Bmp := FCommonData.FCacheBmp;

    if Assigned(FCommonData.SkinManager) and FCommonData.SkinManager.IsValidImgIndex(MaskArrowTop) then
      with FCommonData.SkinManager do begin
      if ma[MaskArrowTop].Bmp = nil then begin
        p.x := FBtn1Rect.Left + (WidthOf(FBtn1Rect) - WidthOf(ma[MaskArrowTop].R) div ma[MaskArrowTop].ImageCount) div 2;// + integer(Btn1State = 2);
        p.y := FBtn1Rect.Top + (HeightOf(FBtn1Rect) - HeightOf(ma[MaskArrowTop].R) div (1 + ma[MaskArrowTop].MaskType)) div 2;// + integer(Btn1State = 2);
      end
      else if (ma[MaskArrowTop].Bmp.Height div 2 < HeightOf(FBtn1Rect)) then begin
        p.x := FBtn1Rect.Left + (WidthOf(FBtn1Rect) - ma[MaskArrowTop].Bmp.Width div 3) div 2;
        p.y := FBtn1Rect.Top + (HeightOf(FBtn1Rect) - ma[MaskArrowTop].Bmp.Height div 2) div 2;
      end;
      if (p.x < 0) or (p.y < 0) then Exit;
      DrawSkinGlyph(b, p, Btn1State, 1, ma[MaskArrowTop], CI);
    end;
  end;
end;

function TsScrollBar.FirstPoint: integer;
begin
  if Kind = sbHorizontal
    then Result := GetSystemMetrics(SM_CXHSCROLL)
    else Result := GetSystemMetrics(SM_CYVSCROLL);
end;

procedure TsScrollBar.Loaded;
var
  OldPos : integer;
begin
  inherited;
  FCommonData.Loaded;
{$IFDEF CHECKXP}
  if UseThemes and not (SkinData.Skinned and SkinData.SkinManager.SkinData.Active) then begin
    ControlStyle := ControlStyle - [csParentBackground]; // Patching of bug with TGraphicControls repainting when XPThemes used
  end;
{$ENDIF}
  if MustBeRecreated then begin // Control must be recreated for the skinned mode using without std blinking
    MustBeRecreated := False;
    SkinnedRecreate := True;
    OldPos := Position;
    RecreateWnd;
    Position := OldPos;
    SkinnedRecreate := False;
  end;
  if (csDesigning in ComponentState) and (FSI.nMax = FSI.nMin - 1) {If FSI is not initialized} then begin
    FSI.nMax := Max;
    FSI.nMin := Min;
    FSI.nPos := Position;
    FSI.nTrackPos := Position;
  end;
end;

procedure TsScrollBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i : integer;
begin
  if not Skinned(Self) or not Enabled or not (Button = mbLeft) then inherited else begin
    if not ControlIsReady(Self) then Exit;
    AppShowHint := Application.ShowHint;
    Application.ShowHint := False;
    MouseOffset := 0;
    if CanFocus then SetFocus;
    // If Button1 pressed...
    if PtInRect(Btn1Rect, Point(x,y)) then begin
      if Btn1State <> 2 then begin
        if Kind = sbVertical then ScrollCode := SB_LINEUP else ScrollCode := SB_LINELEFT;
        Btn1State := 2;
        DrawingForbidden := True;
        IncPos(-1);
        PrepareBtnTimer;
      end;
    end
    // If Button2 pressed...
    else if PtInRect(Btn2Rect, Point(x,y)) then begin
      if Btn2State <> 2 then begin
        Btn2State := 2;
        if Kind = sbVertical then ScrollCode := SB_LINEDOWN else ScrollCode := SB_LINERIGHT;
        DrawingForbidden := True;
        IncPos(1);
        PrepareBtnTimer;
      end;
    end
    // If slider pressed...
    else if PtInRect(SliderRect, Point(x,y)) then begin
      ScrollCode := SB_THUMBTRACK;
      InitDontChange;
      if SliderState <> 2 then begin
        i := CoordToPosition(Point(x, y));
        MouseOffset := i - FCurrPos;
        SliderState := 2;
        FBeginTrack := true;
        IncPos(0);
        PrepareTimer;
      end;
    end
    else begin
      if PtInRect(Bar1Rect, Point(x,y)) then begin
        if Kind = sbVertical then ScrollCode := SB_PAGEUP else ScrollCode := SB_PAGELEFT;
        if Bar1State <> 2 then begin
          Bar1State := 2;
          Bar2State := integer(BarIsHot);
          DrawingForbidden := True;
          IncPos(-Math.Max(Integer(FSI.nPage),1));
          PrepareBarTimer;
        end;
      end
      else begin
        if Kind = sbVertical then ScrollCode := SB_PAGEDOWN else ScrollCode := SB_PAGERIGHT;
        if Bar2State <> 2 then begin
          Bar1State := integer(BarIsHot);
          Bar2State := 2;
          DrawingForbidden := True;
          IncPos(Math.Max(Integer(FSI.nPage),1));
          PrepareBarTimer;
        end;
      end;
    end;
    UpdateBar;
    inherited;
  end;
end;

procedure TsScrollBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not Skinned(Self) or not Enabled then inherited else begin
    if not ControlIsReady(Self) then Exit;
    if Assigned(Timer) then begin
      Timer.Enabled := False;
      if Assigned(Timer) then FreeAndNil(Timer);
    end;
    if PtInRect(SliderRect, Point(X, Y)) or (SliderState = 2) then begin
      ScrollCode := SB_THUMBPOSITION;
      Bar1State := integer(BarIsHot);
      Bar2State := Bar1State;
      if SliderState = 2 then begin
        DrawingForbidden := True;
        IncPos(0);
        if PtInRect(SliderRect, Point(X, Y)) then begin
          SliderState := 1;
        end
        else begin
          SliderState := 0;
        end;
        ClearDontChange;
      end
      else
    end
    else
    if PtInRect(Btn1Rect, Point(X, Y)) and (Btn1State = 2) then begin
      Btn1State := 1;
    end
    else if PtInRect(Btn2Rect, Point(X, Y)) and (Btn2State = 2) then begin
      Btn2State := 1;
    end
    else if (Bar1State = 2) then begin
      Bar1State := integer(BarIsHot);
    end
    else if (Bar2State = 2) then begin
      Bar2State := integer(BarIsHot);
    end;
    UpdateBar;
    ReleaseCapture;
    inherited;
    ScrollCode := SB_ENDSCROLL;
    IncPos(0);
    Application.ShowHint := AppShowHint;
  end;
end;

function TsScrollBar.NotRightToLeft: Boolean;
begin
  Result := not IsRightToLeft or (Kind = sbVertical);
end;

procedure TsScrollBar.OnTimer(Sender: TObject);
begin
  if not Assigned(Timer) or not ControlIsReady(Self) or (csDestroying in Timer.ComponentState) or FCommonData.FMouseAbove then Exit;
  SetPos(CoordToPosition(ScreenToClient(acMousePos)) - MouseOffset);
  SetCapture(Handle);
end;

procedure TsScrollBar.Paint(MsgDC : hdc);
var
  DC, SavedDC : hdc;
  bmp : TBitmap;
  lCI : TCacheInfo;
  LocalState : integer;
  c : TsColor;
  PS : TPaintStruct;
  BG : TacBGInfo;
begin
  bmp := nil;
  BeginPaint(Handle, PS);
  if MsgDC = 0 then DC := GetWindowDC(Handle) else DC := MsgDC;
  SavedDC := SaveDC(DC);
  try
    if DrawingForbidden or not ControlIsReady(Self) or RestrictDrawing or (csDestroying in ComponentState) or (csLoading in ComponentState) or FCommonData.Updating then else begin
      RepaintNeeded := False;
      FCommonData.InitCacheBmp;

      if not Enabled
        then bmp := CreateBmpLike(FCommonData.FCacheBmp)
        else bmp := FCommonData.FCacheBmp;

      BG.PleaseDraw := False;
      if (LinkedControl <> nil) and (LinkedControl is TWinControl) then begin
        GetBGInfo(@BG, LinkedControl);
        lCI := BGInfoToCI(@BG);
        if not (LinkedControl is TCustomForm) then begin
          dec(lCI.X, LinkedControl.Left);
          dec(lCI.Y, LinkedControl.Top);
        end;
      end
      else begin
        GetBGInfo(@BG, Parent);
        lCI := BGInfoToCI(@BG);
      end;

      with FCommonData.SkinManager.ConstData do begin
        Bar1Rect;
        if (HeightOf(FBar1Rect) > 0) and (WidthOf(FBar1Rect) > 0) then begin
          LocalState := Bar1State;
          if LocalState = 0 then LocalState := integer(BarIsHot);
          LocalState := LocalState * integer(Enabled);
          if Kind = sbHorizontal then begin
            if not Assigned(FCommonData.SkinManager) or not FCommonData.SkinManager.IsValidSkinIndex(IndexScrollBar1H) then Exit;
            PaintItemFast(IndexScrollBar1H, MaskScrollBar1H, BGScrollBar1H, BGHotScrollBar1H,
                          s_ScrollBar1H, lCi, True, LocalState, FBar1Rect, Point(Left, Top), FCommonData.FCacheBmp, SkinData.SkinManager);
          end
          else begin
            if not Assigned(FCommonData.SkinManager) or not FCommonData.SkinManager.IsValidSkinIndex(IndexScrollBar1V) then Exit;
            PaintItemFast(IndexScrollBar1V, MaskScrollBar1V, BGScrollBar1V, BGHotScrollBar1V,
                          s_ScrollBar1V, lCi, True, LocalState, FBar1Rect, Point(Left, Top), FCommonData.FCacheBmp, SkinData.SkinManager);
          end;
        end;
        Bar2Rect;
        if (HeightOf(FBar2Rect) > 0) and (WidthOf(FBar2Rect) > 0) then begin
          LocalState := Bar2State;
          if LocalState = 0 then LocalState := integer(BarIsHot);
          LocalState := LocalState * integer(Enabled);
          if Kind = sbHorizontal then begin
            PaintItemFast(IndexScrollBar2H, MaskScrollBar2H,
                          BGScrollBar2H, BGHotScrollBar2H,
                          s_ScrollBar2H, lCi, True, LocalState, FBar2Rect, Point(Left + FBar2Rect.Left, Top + FBar2Rect.Top), FCommonData.FCacheBmp, SkinData.SkinManager);
          end
          else begin
            PaintItemFast(IndexScrollBar2V, MaskScrollBar2V,
                          BGScrollBar2V, BGHotScrollBar2V,
                          s_ScrollBar2V, lCi, True, LocalState, FBar2Rect, Point(Left + FBar2Rect.Left, Top + FBar2Rect.Top), FCommonData.FCacheBmp, SkinData.SkinManager);
          end;
        end;
      end;
      BitBlt(bmp.Canvas.Handle, 0, 0, bmp.Width, bmp.Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);

      if Kind = sbHorizontal then begin
        DrawBtnLeft(bmp);
        DrawBtnRight(bmp);
      end else begin
        DrawBtnTop(bmp);
        DrawBtnBottom(bmp);
      end;

      if (LinkedControl = nil) or Enabled or not LinkedControl.Enabled then DrawSlider(bmp);

      BG.PleaseDraw := False;
      if not Enabled then begin
        if (LinkedControl <> nil) then begin
          GetBGInfo(@BG, LinkedControl);
          lCI := BGInfoToCI(@BG);
          if lCI.Ready then begin
            BmpDisabledKind(bmp, FDisabledKind, Parent, lCI, Point(Left - LinkedControl.Left, Top - LinkedControl.Top));
          end
          else begin
            c.C := lCI.FillColor;
            FadeBmp(bmp, Rect(0, 0, bmp.Width + 1, bmp.Height + 1), 60, c, 0, 0);
          end;
        end
        else begin
          lCI := GetParentCache(FCommonData);
          BmpDisabledKind(bmp, FDisabledKind, Parent, lCI, Point(Left, Top));
        end;
      end;

      BitBlt(DC, 0, 0, bmp.Width, bmp.Height, bmp.Canvas.Handle, 0, 0, SRCCOPY);
    end;
    if not Enabled and Assigned(bmp) then FreeAndNil(bmp);
  finally
    RestoreDC(DC, SavedDC);
    if MsgDC = 0 then ReleaseDC(Handle, DC);
    EndPaint(Handle, PS);
  end;
end;

procedure TsScrollBar.Preparetimer;
begin
  if Assigned(Timer) then FreeAndNil(Timer);
  SetCapture(Handle);
  Timer := TTimer.Create(Self);
  Timer.OnTimer := OnTimer;
  Timer.Interval := 50; // {KJS} more smooth scrolling
  Timer.Enabled := True;
end;

function TsScrollBar.SliderRect: TRect;
begin
  if Kind = sbHorizontal then begin
    FSliderRect.Left := PositionToCoord - SliderSize div 2;
    FSliderRect.Top := 0;
    FSliderRect.Right := FSliderRect.Left + SliderSize;
    FSliderRect.Bottom := Height;
  end
  else begin
    FSliderRect.Left := 0;
    FSliderRect.Top := PositionToCoord - SliderSize div 2;
    FSliderRect.Right := Width;
    FSliderRect.Bottom := FSliderRect.Top + SliderSize;
  end;
  Result := FSliderRect;
end;

function TsScrollBar.SliderSize : integer;
const
  MinSize = 14;
begin
  if FSI.nPage = 0 then
    Result := MinSize
  else
    Result := math.max(MinSize, Round(FSI.nPage * (WorkSize / (FSI.nMax - Math.Max(Integer(FSI.nPage) - 1,0) + integer(FSI.nPage) - FSI.nMin))));
end;

procedure TsScrollBar.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if not DrawingForbidden and not InAnimationProcess then DefaultHandler(Message);
end;

procedure TsScrollBar.WndProc(var Message: TMessage);
var
  OldPos : integer;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  if (Message.Msg = SM_ALPHACMD) and not (csDestroying in ComponentState) then case Message.WParamHi of
    AC_CTRLHANDLED : begin Message.Result := 1; Exit end; 
    AC_GETAPPLICATION : begin Message.Result := longint(Application); Exit end;
    AC_REMOVESKIN : if LongWord(Message.LParam) = LongWord(SkinData.SkinManager) then begin
      CommonWndProc(Message, FCommonData);
      if not SkinnedRecreate then begin
        OldPos := Position;
        RecreateWnd;
        Position := OldPos;
      end;
      exit
    end;
    AC_SETNEWSKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FCommonData);
      exit
    end;
    AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FCommonData);
      FCommonData.BGChanged := True;
      if not SkinnedRecreate then begin
        SkinnedRecreate := True;
        OldPos := Position;
        RecreateWnd;
        Position := OldPos;
        SkinnedRecreate := False;
      end;
      exit
    end;
    AC_ENDPARENTUPDATE : {if FCommonData.Updating then} begin
      FCommonData.Updating := False;
      Repaint;
      Exit
    end
  end;
  if Assigned(FCommonData) then begin
    case Message.Msg of
      WM_PRINT : if (DefaultManager <> nil) and DefaultManager.Active then begin
        SendMessage(Handle, WM_PAINT, Message.WParam, Message.LParam);
        Perform(WM_NCPAINT, Message.WParam, Message.LParam);
        Exit;
      end;
      WM_PAINT, WM_NCHITTEST : if MustBeRecreated and not InAnimationProcess then begin // Control must be recreated for the skinned mode using without std blinking
        MustBeRecreated := False;
        SkinnedRecreate := True;
        OldPos := Position;
        RecreateWnd;
        Position := OldPos;
        SkinnedRecreate := False;
      end;
    end;
    CommonWndProc(Message, FCommonData);
  end;
  if Assigned(FCommonData) and Skinned(Self) then
    case Message.Msg of
      SBM_SETSCROLLINFO :
        begin
          with PScrollInfo(Message.LParam)^ do begin
            if Boolean(fMask and SIF_PAGE) and (FSI.nPage <> nPage) then begin
              FSI.nPage := nPage;
              RepaintNeeded := LongBool(Message.WParam);
            end;
            if Boolean(fMask and SIF_POS) and (FSI.nPos <> nPos) then begin
              FSI.nPos := nPos;
              RepaintNeeded := LongBool(Message.WParam);
            end;
            if Boolean(fMask and SIF_RANGE) and ((FSI.nMin <> nMin) or (FSI.nMax <> nMax)) then begin
              if (nMax - nMin) < 0 then begin
                FSI.nMin := 0;
                FSI.nMax := 0;
                RepaintNeeded := LongBool(Message.WParam);
              end
              else begin
                FSI.nMin := nMin;
                FSI.nMax := nMax;
                RepaintNeeded := LongBool(Message.WParam);
              end;
            end;
            if integer(FSI.nPage) < 0 then
              FSI.nPage := 0
            else if integer(FSI.nPage) > (FSI.nMax - FSI.nMin + 1) then
              FSI.nPage := (FSI.nMax - FSI.nMin + 1);
            if FSI.nPos < FSI.nMin then
              FSI.nPos := FSI.nMin
            else if FSI.nPos > (FSI.nMax - Math.Max(Integer(FSI.nPage) - 1, 0)) then
              FSI.nPos := (FSI.nMax - Math.Max(Integer(FSI.nPage) - 1, 0));
            if (ScrollCode <> SB_THUMBTRACK) then FCurrPos := FSI.nPos;
          end;
          UpdateBar;
        end;
      SBM_GETSCROLLINFO : begin
          with PScrollInfo(Message.LParam)^ do begin
            if Boolean(fMask and SIF_PAGE) then begin
              nPage := FSI.nPage;
            end;
            if Boolean(fMask and SIF_POS) then begin
              nPos := FSI.nPos;
            end;
            if Boolean(fMask and SIF_TRACKPOS) and (cbSize = SizeOf(TScrollInfo)) then begin
              nTrackPos := FSI.nTrackPos;
            end;
            if Boolean(fMask and SIF_RANGE) then begin
              nMin := FSI.nMin;
              nMax := FSI.nMax;
            end;
          end;
        end;
    end;
  inherited WndProc(Message);
end;

procedure TsScrollBar.DrawSlider(b: TBitmap);
var
  R : TRect;
  i1 : integer;
  TmpBmp : TBitmap;
begin
  R := SliderRect;
  if (Kind = sbVertical) then begin
    if HeightOf(R) > Height - HeightOf(FBtn1Rect) - HeightOf(FBtn2Rect) then Exit
  end
  else if WidthOf(R) > Width - WidthOf(FBtn1Rect) - WidthOf(FBtn2Rect) then Exit;

  TmpBmp := CreateBmp24(WidthOf(R), HeightOf(R));
  BitBlt(TmpBmp.Canvas.Handle, 0, 0, TmpBmp.Width, TmpBmp.Height, FCommonData.FCacheBmp.Canvas.Handle, R.Left, R.Top, SRCCOPY);
  Ci := MakeCacheInfo(FCommonData.FCacheBmp);
  with FCommonData.SkinManager.ConstData do begin
    if Kind = sbHorizontal then begin
      PaintItemFast(IndexSliderHorz, MaskSliderHorz, ScrollSliderBGHorz, ScrollSliderBGHotHorz, s_ScrollSliderH,
          Ci, True, SliderState, Rect(0, 0, TmpBmp.Width, TmpBmp.Height), R.TopLeft, TmpBmp, SkinData.SkinManager);
      i1 := MaskSliderGlyphHorz;
    end
    else begin
      PaintItemFast(IndexSliderVert, MaskSliderVert, ScrollSliderBGVert, ScrollSliderBGHotVert, s_ScrollSliderV,
          Ci, True, SliderState, Rect(0, 0, TmpBmp.Width, TmpBmp.Height), R.TopLeft, TmpBmp, SkinData.SkinManager);
      i1 := MaskSliderGlyphVert;
    end;
  end;
  BitBlt(b.Canvas.Handle, R.Left, R.Top, TmpBmp.Width, TmpBmp.Height, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);
  FreeAndNil(TmpBmp);
  Ci.Bmp := FCommonData.FCacheBmp;

  if Assigned(FCommonData.SkinManager) and FCommonData.SkinManager.IsValidImgIndex(i1) then
    with FCommonData.SkinManager do begin
    if FCommonData.SkinManager.ma[i1].Bmp = nil then begin
      p.x := FSliderRect.Left + (WidthOf(FSliderRect) - WidthOf(ma[i1].R) div ma[i1].ImageCount) div 2 + integer(SliderState = 2);
      p.y := FSliderRect.Top + (HeightOf(FSliderRect) - HeightOf(ma[i1].R) div (1 + ma[i1].MaskType)) div 2 + integer(SliderState = 2);
    end
    else if (((Kind = sbVertical) and (ma[i1].Bmp.Height div 2 < HeightOf(FSliderRect))) or
         ((Kind = sbHorizontal) and (ma[i1].Bmp.Width div 2 < WidthOf(FSliderRect)))) then begin
      p.x := FSliderRect.Left + (WidthOf(FSliderRect) - ma[i1].Bmp.Width div 3) div 2 + integer(SliderState = 2);
      p.y := FSliderRect.Top + (HeightOf(FSliderRect) - ma[i1].Bmp.Height div 2) div 2 + integer(SliderState = 2);
    end;
    DrawSkinGlyph(b, p, SliderState, 1, ma[i1], CI);
  end;
end;

procedure TsScrollBar.WMNCHitTest(var Message: TWMNCHitTest);
var
  i : integer;
begin
  if Skinned(Self) and Enabled and (not (csDesigning in ComponentState) {or AlwaysLive}) then begin
    if not ControlIsReady(Self) then Exit;
    if PtInRect(SliderRect, CoordToPoint(SmallPointToPoint(Message.Pos))) or (SliderState = 2) then begin
      if SliderState <> 2 then SliderState := 1 else begin
        i := CoordToPosition(CoordToPoint(Point(Message.Pos.X, Message.Pos.Y))) - MouseOffset;
        if FCurrPos <> i then begin
          DrawingForbidden := True;
          SetPos(i);
        end;
      end;
    end
    else
    if PtInRect(Btn1Rect, CoordToPoint(SmallPointToPoint(Message.Pos))) then begin
      if Btn1State <> 2 then Btn1State := 1;
    end
    else if PtInRect(Btn2Rect, CoordToPoint(SmallPointToPoint(Message.Pos))) then begin
      if Btn2State <> 2 then Btn2State := 1;
    end
    else if (SliderState = 2) then begin
      i := CoordToPosition(CoordToPoint(SmallPointToPoint(Message.Pos)));
      if FCurrPos <> i then begin
        DrawingForbidden := True;
        SetPos(i);
      end;
    end
    else begin
      SliderState := 0;
      Btn1State := 0;
      Btn2State := 0;
    end;
    if Self <> nil then UpdateBar;
  end;
  inherited;
end;

procedure TsScrollBar.OnBtnTimer(Sender: TObject);
begin
  if not Assigned(Timer) or (csDestroying in Timer.ComponentState) then Exit;
  if Btn1State = 2 then begin
    IncPos(-1);
  end
  else
  if Btn2State = 2 then begin
    IncPos(1);
  end
  else begin
    if Assigned(Timer) then FreeAndNil(Timer);
  end;
  if assigned(Timer) and (Timer.Interval > 50) then Timer.Interval := 50;        //KJS
end;

procedure TsScrollBar.PrepareBtnTimer;
begin
  if Assigned(Timer) then FreeAndNil(Timer);
  Timer := TTimer.Create(Self);
  Timer.OnTimer := OnBtnTimer;
  Timer.Interval := 500;
  Timer.Enabled := True;
end;

function TsScrollBar.PositionToCoord: integer;
begin
  if Enabled then begin
    if (FSI.nMax - FSI.nMin - Math.Max(Integer(FSI.nPage) -1, 0)) <> 0 then
      if Kind = sbHorizontal then begin
        Result := FirstPoint + SliderSize div 2 + Round((FCurrPos - FSI.nMin) * ((Width - 2 * FirstPoint - SliderSize) / (FSI.nMax - FSI.nMin - Math.Max(Integer(FSI.nPage) -1,0))));
      end
      else begin
        Result := FirstPoint + SliderSize div 2 + Round((FCurrPos - FSI.nMin) * ((Height - 2 * FirstPoint - SliderSize) / (FSI.nMax - FSI.nMin - Math.Max(Integer(FSI.nPage) -1,0))));
      end
    else Result := 0;
  end
  else begin
    if Kind = sbHorizontal then Result := Width div 2 else Result := Height div 2;
  end;
end;

procedure TsScrollBar.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited;
end;

procedure TsScrollBar.WMPaint(var Msg: TWMPaint);
begin
  if DrawingForbidden or (not (csCreating in Controlstate) and Assigned(SkinData.SkinManager) and SkinData.SkinManager.SkinData.Active and not (csDestroying in Componentstate)) then begin
    Self.FCommonData.Updating := Self.FCommonData.Updating;
    Paint(Msg.DC);
  end
  else inherited;
end;

procedure TsScrollBar.CMMouseLeave(var Msg: TMessage);
begin
  if Skinned(Self) then begin
    Btn1State := 0;
    Btn2State := 0;
    if SliderState <> 2 then begin
      SliderState := 0;
      Bar1State := 0;
      Bar2State := 0;
    end;
    UpdateBar;
  end
  else inherited;
end;

procedure TsScrollBar.PrepareBarTimer;
begin
  if Assigned(Timer) then FreeAndNil(Timer);
  Timer := TTimer.Create(Self);
  Timer.OnTimer := OnBarTimer;
  Timer.Interval := 500;
  Timer.Enabled := True;
end;

procedure TsScrollBar.OnBarTimer(Sender: TObject);
begin
  if not Assigned(Timer) or (csDestroying in Timer.ComponentState) then Exit;
  if (Bar1State = 2) and (FCurrPos > CoordToPosition(ScreenToClient(acMousePos))) then begin
    IncPos(-Math.Max(Integer(FSI.nPage),1));
  end
  else
  if (Bar2State = 2) and (FCurrPos < CoordToPosition(ScreenToClient(acMousePos))) then begin
    IncPos(Math.Max(Integer(FSI.nPage),1));
  end
  else begin
    if Assigned(Timer) then FreeAndNil(Timer);
  end;
  if assigned(Timer) and (Timer.Interval > 50) then Timer.Interval := 50;        
end;

function TsScrollBar.Bar1Rect: TRect;
begin
  FBar1Rect.Left := 0;
  FBar1Rect.Top := 0;
  if Kind = sbHorizontal then begin
    FBar1Rect.Right := PositionToCoord;
    FBar1Rect.Bottom := Height;
  end
  else begin
    FBar1Rect.Right := Width;
    FBar1Rect.Bottom := PositionToCoord;
  end;
  Result := FBar1Rect;
end;

function TsScrollBar.Bar2Rect: TRect;
begin
  if Kind = sbHorizontal then begin
    FBar2Rect.Left := PositionToCoord;
    FBar2Rect.Top := 0;
    FBar2Rect.Right := Width;
    FBar2Rect.Bottom := Height;
  end
  else begin
    FBar2Rect.Left := 0;
    FBar2Rect.Top := PositionToCoord;
    FBar2Rect.Right := Width;
    FBar2Rect.Bottom := Height;
  end;
  Result := FBar2Rect;
end;

procedure TsScrollBar.CMMouseEnter(var Msg: TMessage);
begin
  if Skinned(Self) then begin
    Bar1State := 1;
    Bar2State := 1;
    UpdateBar;
  end
  else inherited;
end;

function TsScrollBar.Btn1DRect: TRect;
begin
  Result := Btn1Rect;
  with FCommonData.SkinManager.ConstData, FCommonData.SkinManager do begin
    if Kind = sbHorizontal then begin
      if (IndexScrollLeft > -1) and gd[IndexScrollLeft].ReservedBoolean and (MaskScrollLeft > -1) then begin
        if ma[MaskScrollLeft].Bmp = nil
          then Result.Right := math.max(GetSystemMetrics(SM_CXHSCROLL), WidthOfImage(ma[MaskScrollLeft]))
          else Result.Right := math.max(GetSystemMetrics(SM_CXHSCROLL), ma[MaskScrollLeft].Bmp.Width div 3);
      end;
    end
    else begin
      if (IndexScrollTop > -1) and gd[ConstData.IndexScrollTop].ReservedBoolean and (MaskScrollTop > -1) then begin
        if ma[ConstData.MaskScrollTop].Bmp = nil
          then Result.Bottom := math.max(GetSystemMetrics(SM_CYVSCROLL), HeightOfImage(ma[MaskScrollTop]))
          else Result.Bottom := math.max(GetSystemMetrics(SM_CYVSCROLL), ma[MaskScrollTop].Bmp.Height div 2);
      end;
    end;
  end;
end;

procedure TsScrollBar.UpdateBar;
begin
  DrawingForbidden := False;
  if RepaintNeeded then Paint(0);
end;

procedure TsScrollBar.SetInteger(Index, Value: integer);
begin
  case Index of
    0 : if FBtn1State <> Value then begin
      RepaintNeeded := True;
      FBtn1State := Value;
      case Value of
        1, 2 : begin
          FBtn2State := 0;
          FSliderState := 0;
          FBar1State := 1;
          FBar2State := 1;
        end;
      end;
    end;
    1 : if FBtn2State <> Value then begin
      RepaintNeeded := True;
      FBtn2State := Value;
      case Value of
        1, 2 : begin
          FBtn1State := 0;
          FSliderState := 0;
          FBar1State := 1;
          FBar2State := 1;
        end;
      end;
    end;
    2 : if FBar1State <> Value then begin
      RepaintNeeded := True;
      FBar1State := Value;
      case Value of
        1, 2 : begin
          FBtn1State := 0;
          FBtn2State := 0;
          FSliderState := 0;
          FBar2State := 1;
        end;
      end;
    end;
    3 : if FBar2State <> Value then begin
      RepaintNeeded := True;
      FBar2State := Value;
      case Value of
        1, 2 : begin
          FBtn1State := 0;
          FBtn2State := 0;
          FSliderState := 0;
          FBar1State := 1;
        end;
      end;
    end;
    4 : if FSliderState <> Value then begin
      RepaintNeeded := True;
      FSliderState := Value;
      case Value of
        1, 2 : begin
          FBtn1State := 0;
          FBtn2State := 0;
          FBar1State := 1;
          FBar2State := 1;
        end;
      end;
    end;
  end;
end;

function TsScrollBar.Btn2DRect: TRect;
begin
  Result := Btn2Rect;
  with FCommonData.SkinManager.ConstData, FCommonData.SkinManager do begin
    if Kind = sbHorizontal then begin
      if (IndexScrollRight > -1) and gd[IndexScrollRight].ReservedBoolean and (MaskScrollRight > -1) then begin
        if ma[MaskScrollRight].Bmp = nil
          then Result.Left := width - math.max(GetSystemMetrics(SM_CXHSCROLL), WidthOf(ma[MaskScrollRight].R) div ma[MaskScrollRight].ImageCount)
          else Result.Left := width - math.max(GetSystemMetrics(SM_CXHSCROLL), ma[MaskScrollRight].Bmp.Width div 3);
      end;
    end
    else begin
      if (IndexScrollBottom > -1) and gd[IndexScrollBottom].ReservedBoolean and (MaskScrollBottom > -1) then begin
        if ma[MaskScrollBottom].Bmp = nil
          then Result.Top := height - math.max(GetSystemMetrics(SM_CYVSCROLL), HeightOf(ma[MaskScrollBottom].R) div (1 + ma[MaskScrollBottom].MaskType))
          else Result.Top := height - math.max(GetSystemMetrics(SM_CYVSCROLL), ma[MaskScrollBottom].Bmp.Height div 2);
      end;
    end;
  end;
end;

function TsScrollBar.BarIsHot: boolean;
begin
  Result := ControlIsActive(FCommonData);
end;

function TsScrollBar.WorkSize: integer;
begin
  if Kind = sbHorizontal then Result := Width - 2 * GetSystemMetrics(SM_CXHSCROLL) else Result := Height - 2 * GetSystemMetrics(SM_CYVSCROLL);
end;

procedure TsScrollBar.ClearDontChange;
begin
  DontChange := False;
end;

procedure TsScrollBar.InitDontChange;
begin
  DontChange := True;
end;

procedure TsScrollBar.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FCommonData.Invalidate;
  end;
end;

procedure TsScrollBar.PlaceToLinked;
begin
end;

procedure TsScrollBar.IncPos(Offset: integer);
begin
  SetPos(FCurrPos+ Offset);
end;

procedure TsScrollBar.SetPos(Pos: integer);
const
  Kinds: array[TScrollBarKind] of DWORD = (SB_HORZ, SB_VERT);
  Styles: array[TScrollBarKind] of DWORD = (WM_HSCROLL, WM_VSCROLL);
var
  m : TWMScroll;

begin
  FCurrPos := Pos;

  if FCurrPos < FSI.nMin
    then FCurrPos := FSI.nMin
    else if FCurrPos > (FSI.nMax - Math.Max(Integer(FSI.nPage) - 1, 0))
      then FCurrPos := (FSI.nMax - Math.Max(Integer(FSI.nPage) - 1, 0));

  m.Msg := Styles[Kind];
  m.ScrollBar := Handle;
  m.ScrollCode := SmallInt(ScrollCode);

  if (m.ScrollCode = SB_THUMBTRACK) then begin
    if (FSI.nTrackPos = FCurrPos) and (not FBeginTrack) then exit;
    FBeginTrack := false;
    FSI.nTrackPos := FCurrPos
  end
  else FSI.nPos := FCurrPos;

  if m.ScrollCode in [SB_THUMBTRACK,SB_THUMBPOSITION] then m.Pos := FCurrPos else m.Pos := 0;

  SendMessage(Handle, M.Msg, TMessage(M).WParam, TMessage(M).LParam);

  RepaintNeeded := true;
  UpdateBar;

  if DoSendChanges and Assigned(LinkedControl) and LinkedControl.HandleAllocated
    then SendMessage(LinkedControl.Handle, M.Msg, TMessage(M).WParam, TMessage(M).LParam);
end;

procedure TsScrollBar.CNHScroll(var Message: TWMHScroll);
begin
  if not (DoSendChanges and Assigned(LinkedControl)) then inherited;
end;

procedure TsScrollBar.CNVScroll(var Message: TWMVScroll);
begin
  if not (DoSendChanges and Assigned(LinkedControl)) then inherited;
end;

function TsScrollBar.GetSkinManager: TsSkinManager;
begin
  Result := SkinData.SkinManager
end;

procedure TsScrollBar.SetSkinManager(const Value: TsSkinManager);
begin
  SkinData.SkinManager := Value;
end;

end.
