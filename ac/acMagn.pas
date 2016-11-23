unit acMagn;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, Graphics, Forms, Menus, Classes, Controls,
  ExtCtrls{$IFDEF LOGGED}, sDebugMsgs{$ENDIF};

const
  MaxSize = 800;
  MinSize = 150;

type
  TPosChangingEvent = procedure(var X : integer; var Y : integer) of object;
  TMagnSize = MinSize .. MaxSize;
  TacSizingMode = (asmNone, asmFreeAspectRatio, asmFixedAspectRatio);

  TsMagnifier = class(TComponent)
{$IFNDEF NOTFORHELP}
  private
{$IFDEF D2007}
    MainFormOnTaskBar : boolean;
{$ENDIF}
    FScaling: integer;
    FPopupMenu: TPopupMenu;
    FOnMouseUp: TMouseEvent;
    FOnMouseDown: TMouseEvent;
    FOnPosChanging: TPosChangingEvent;
    FOnDblClick: TNotifyEvent;
    FHeight: TMagnSize;
    FWidth: TMagnSize;
    FSizingMode: TacSizingMode;
    procedure SetScaling(const Value: integer);
  public
    IsModal : boolean;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
{$ENDIF}
    procedure Execute(x : integer = -1; y : integer = -1);
    procedure Hide;
    function  IsVisible : Boolean;
    function  GetPosition : TPoint;
    procedure Refresh;
  published
    property PopupMenu : TPopupMenu read FPopupMenu write FPopupMenu;
    property Scaling : integer read FScaling write SetScaling default 2;
    property Width : TMagnSize read FWidth write FWidth default 250;
    property Height : TMagnSize read FHeight write FHeight default 250;
    property SizingMode : TacSizingMode read FSizingMode write FSizingMode default asmFreeAspectRatio;

    property OnDblClick : TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnMouseDown : TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp : TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnPosChanging : TPosChangingEvent read FOnPosChanging write FOnPosChanging;
  end;

{$IFNDEF NOTFORHELP}
  TacMagnForm = class(TForm)
    PopupMenu1: TPopupMenu;
    N1x1: TMenuItem;
    N2x1: TMenuItem;
    N8x1: TMenuItem;
    N1: TMenuItem;
    Close1: TMenuItem;
    N16x1: TMenuItem;
    Timer1: TTimer;
    procedure Close1Click(Sender: TObject);
    procedure Zoom1x1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormActivate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  protected
    procedure WMPosChanging (var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    function WMNCHitTest(var Message : TWMNCHitTest) : integer;
    procedure UpdateThumbPos;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    Caller : TsMagnifier;
    FTempBmp : TBitMap;
    AlphaBmp : TBitmap;
    Scale : Smallint;
    MagnBmp : TBitmap;
    procedure FormCreateInit;
    function MagnSize : TSize;
    function MClientRect : TRect;
    destructor Destroy; override;
    procedure EstablishAspectRatio(Side : word; var Rect : TRect);
    procedure WndProc(var Message : TMessage); override;
    procedure SetZooming(k : integer);
    procedure ShowGlass(x, y : integer);
    procedure CreateAlphaBmp;
  end;

var
  Closing      : boolean = False;
  Showed       : boolean = False;
  acIsDragging : boolean = False;

const
  wL = 50; wR = wL; wT = wL; wB = wL; // Borders widths
  cL = 24; cR = 26; cT = 21; cB = 29; // Content margins

{$ENDIF}

implementation

{$R *.DFM}

uses sGraphUtils, sConst, SysUtils, sAlphaGraph, sSkinManager, sVclUtils, sMessages, acntUtils, acPNG, sSkinProvider, acThumbForm, math, sSkinMenus;

var
  ThumbWnd : TThumbForm = nil;

procedure TacMagnForm.ShowGlass(x, y : integer);
var
  DC : hdc;
  FBmpSize: TSize;
  FBmpTopLeft: TPoint;
  FBlend: TBlendFunction;
  w, h, XOffs, YOffs, i, p, StepCount : integer;
begin
  if (DefaultManager <> nil) then DefaultManager.SkinableMenus.HookPopupMenu(PopupMenu, (DefaultManager.Active and (DefaultManager.SkinName <> '')));

  w := WidthOf(MClientRect);
  h := HeightOf(MClientRect);

  XOffs := Round(X + cL + (w - w / Scale) / 2);
  YOffs := Round(Y + cT + (h - h / Scale) / 2);

  if ThumbWnd = nil then begin
    FTempBmp.Width := w;
    FTempBmp.Height := h;
    DC := GetDC(0); // Copy image from screen
    StretchBlt(FTempBmp.Canvas.Handle, 0, 0, w, h, DC, XOffs, YOffs, w div Scale, h div Scale, SrcCopy);
    ReleaseDC(0, DC);
  end;

  FBmpSize.cx := MagnSize.cx;
  FBmpSize.cy := MagnSize.cy;
  FBmpTopLeft := Point(0, 0);

  with FBlend do begin
    BlendOp := AC_SRC_OVER;
    BlendFlags := 0;
    AlphaFormat := $01;
    SourceConstantAlpha := MaxByte;
  end;

  CreateAlphaBmp;

  DC := GetDC(0);
  //  Animation is disabled
  if not AeroIsEnabled and not Showed and (DefaultManager <> nil) and DefaultManager.AnimEffects.DialogShow.Active then begin
    StepCount := 20;//0 div 10;
    if StepCount > 0 then begin
      p := MaxByte div StepCount;
      i := 0;
      while i <= StepCount do begin
        FBlend.SourceConstantAlpha := i * p;
        UpdateLayeredWindow(Handle, DC, nil, @FBmpSize, AlphaBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA);
        inc(i);
        if (i > StepCount) then Break;
        if StepCount > 0 then Sleep(10);
      end;
    end;
  end
  else UpdateLayeredWindow(Handle, DC, nil, @FBmpSize, AlphaBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA);
  Showed := True;
  ReleaseDC(0, DC);
end;

procedure TacMagnForm.Timer1Timer(Sender: TObject);
begin
  if (ThumbWnd <> nil) then UpdateThumbPos else ShowGlass(Left, Top);
end;

procedure TacMagnForm.UpdateThumbPos;
begin
  ThumbWnd.ScaleFactor := Scale;
  ThumbWnd.BoundsRect := Rect(Left + cL, Top + cT, Left + cL + WidthOf(MClientRect), Top + cT + HeightOf(MClientRect));
  ThumbWnd.UpdateThumbnail;
end;

{$R magn.res}

procedure TacMagnForm.FormCreateInit;
var
  Bmp : PRGBAArray;
  X, Y : integer;
  rgba : TsColor_;
  pg : TPNGGraphic;
  s : TResourceStream;
begin
  if AeroIsEnabled then begin
    ThumbWnd := TThumbForm.Create(nil);
    ThumbWnd.ScaleFactor := Scale;
  end
  else FTempBmp := CreateBmp24(WidthOf(MClientRect), HeightOf(MClientRect));
  Timer1.Enabled := True;

  s := TResourceStream.Create(hInstance, 'MAGN', RT_RCDATA);
  pg := TPNGGraphic.Create;
  pg.LoadFromStream(s);
  MagnBmp := TBitmap.Create;
  MagnBmp.Assign(pg); // Main magnifier template

  rgba.I := 0;
  for Y := 0 to MagnBmp.Height - 1 do begin
    Bmp := MagnBmp.ScanLine[Y];
    for X := 0 to MagnBmp.Width - 1 do if Bmp[X].A = 0 then Bmp[X] := rgba
  end;

  pg.Free;
  s.Free;
end;

procedure TacMagnForm.Close1Click(Sender: TObject);
begin
  Close;
end;

procedure TacMagnForm.SetZooming(k: integer);
begin
  Scale := k;
  ShowGlass(Left, Top);
  if ThumbWnd <> nil then UpdateThumbPos;
end;

procedure TacMagnForm.Zoom1x1Click(Sender: TObject);
begin
  Caller.Scaling := TMenuItem(Sender).Tag;
  TMenuItem(Sender).Checked := True;
end;

procedure TacMagnForm.WMPosChanging(var Message: TWMWindowPosChanging);
var
  w, h, l, r, t, b : integer;
  function DesktopLeft : integer;
  var
    i : integer;
  begin
    Result := Monitor.Left;
    for i := 0 to Screen.MonitorCount - 1 do if Screen.Monitors[i].Left < Result then Result := Screen.Monitors[i].Left;
  end;
  function DesktopRight : integer;
  var
    i : integer;
  begin
    Result := Monitor.Left + Monitor.Width;
    for i := 0 to Screen.MonitorCount - 1 do if Screen.Monitors[i].Left + Screen.Monitors[i].Width > Result then Result := Screen.Monitors[i].Left + Screen.Monitors[i].Width;
  end;
begin
  if not Showed or Closing or (csloading in ComponentState) or (csCreating in ControlState) or (csDestroying in ComponentState) or (csDestroying in Application.ComponentState) then Exit;

  if Assigned(TsMagnifier(Caller).OnPosChanging) and (Message.WindowPos^.cx <> 0) and (Message.WindowPos^.cy <> 0) then begin
    TsMagnifier(Caller).OnPosChanging(Message.WindowPos^.X, Message.WindowPos^.Y)
  end
  else begin
    w := WidthOf(MClientRect) div 2;
    h := HeightOf(MClientRect) div 2;
    l := DesktopLeft - w - cL;
    r := DesktopRight - w - cL;
    t := Screen.DesktopTop - h - cT;
    b := Screen.DesktopHeight - h - cB;

    if Message.WindowPos^.X < l then Message.WindowPos^.X := l else if Message.WindowPos^.X > r  then Message.WindowPos^.X := r;
    if Message.WindowPos^.Y < t then Message.WindowPos^.Y := t else if Message.WindowPos^.Y > b then Message.WindowPos^.Y := b;
  end;

  if (Message.WindowPos^.X = 0) and (Message.WindowPos^.Y = 0) then ShowGlass(Left, Top) else ShowGlass(Message.WindowPos^.X, Message.WindowPos^.Y);
  if ThumbWnd <> nil then UpdateThumbPos;
end;

procedure TacMagnForm.EstablishAspectRatio(Side: word; var Rect: TRect);
var
  OldW, OldH, i, NewH, NewW : integer;
  AspRatio : real;
begin
  OldH := max(Height - cL - cR, 1);
  OldW := max(Width - cT - cB, 1);
  NewH := HeightOf(Rect);
  if NewH < MinSize then begin
    NewH := MinSize;
    Rect.Bottom := Rect.Top + NewH;
  end
  else if NewH > MaxSize then begin
    NewH := MaxSize;
    Rect.Bottom := Rect.Top + NewH;
  end;
  NewW := WidthOf(Rect);
  if NewW < MinSize then begin
    NewW := MinSize;
    Rect.right := Rect.Left + NewW;
  end
  else if NewW > MaxSize then begin
    NewW := MaxSize;
    Rect.right := Rect.Left + NewW;
  end;

  AspRatio := OldH / OldW;
  with Rect do case Side of
    WMSZ_BOTTOMRIGHT, WMSZ_BOTTOM : begin
      i := Round(Left + (NewH - cT - cB) / AspRatio + cL + cR);
      if i - Left < MinSize then begin
        i := Left + MinSize;
        Bottom := Round(Top + (i - Left - cL - cR) * AspRatio + cT + cB);
      end
      else if i - Left > MaxSize then begin
        i := Left + MaxSize;
        Bottom := Round(Top + (i - Left - cL - cR) * AspRatio + cT + cB);
      end;
      Rect.Right := i;
    end;                                                      

    WMSZ_TOPLEFT, WMSZ_TOP : begin
      i := Round(Right - (NewH - cT - cB) / AspRatio - cL - cR);
      if Right - i < MinSize then begin
        i := Right - MinSize;
        Top := Round(Bottom - (Right - i - cL - cR) * AspRatio - cT - cB);
      end
      else if Right - i > MaxSize then begin
        i := Right - MaxSize;
        Top := Round(Bottom - (Right - i - cL - cR) * AspRatio - cT - cB);
      end;
      Left := i
    end;

    WMSZ_BOTTOMLEFT, WMSZ_RIGHT : begin
      i := Round(Top + (NewW - cL - cR) * AspRatio + cT + cB);
      if i - Top < MinSize then begin
        i := Top + MinSize;
        Right := Round(Left + (i - Top - cL - cR) / AspRatio + cL + cR);
      end
      else if i - Top > MaxSize then begin
        i := Top + MaxSize;
        Right := Round(Left + (i - Top - cL - cR) / AspRatio + cL + cR);
      end;
      Bottom := i;
    end;

    WMSZ_TOPRIGHT, WMSZ_LEFT : begin
      i := Round(Bottom - (WidthOf(Rect) - cL - cR) * AspRatio - cT - cB);
      if Bottom - i < MinSize then begin
        i := Bottom - MinSize;
        Left := Round(Right - (Bottom - i - cT - cB) / AspRatio - cL - cR);
      end
      else if Bottom - i > MaxSize then begin
        i := Bottom - MaxSize;
        Left := Round(Right - (Bottom - i - cT - cB) / AspRatio - cL - cR);
      end;
      Top := i;
    end;
  end
end;

procedure TacMagnForm.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_ERASEBKGND, WM_NCPAINT : Exit;
    WM_SIZING : begin
      case Caller.SizingMode of
        asmFixedAspectRatio : EstablishAspectRatio(Message.wParam, PRect(Message.lParam)^);
        asmFreeAspectRatio : inherited
      end;
      Message.Result := 0;
      exit
    end;
    WM_NCHITTEST : begin
      Message.Result := WMNCHitTest(TWMNCHitTest(Message));
      Exit;
    end;
  end;
  inherited;
  case Message.Msg of
    WM_SIZE, WM_ACTIVATE : if not (csDestroying in ComponentState) and not (csDestroying in Application.ComponentState) and not (csLoading in ComponentState) and Visible then begin
      ShowGlass(Left, Top);
    end;
  end;
end;

procedure TacMagnForm.FormActivate(Sender: TObject);
begin
  if ThumbWnd <> nil then UpdateThumbPos;
end;

procedure TacMagnForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Showed := False;
  Closing := True;
  if Application.MainForm <> Self then begin
    Action := caFree;
    acMagnForm := nil;
  end;
  if ThumbWnd <> nil then FreeAndNil(ThumbWnd);
end;

procedure TacMagnForm.CreateAlphaBmp;
var
  FastDst, FastSrc : TacFast32;
  x, y : integer;
  CSrc, CDst : TsColor_;
  Bmp : TBitmap;
  mRect : TRect;
  Dst, Src : PRGBAArray;
begin
  mRect := MClientRect;
  FastDst := TacFast32.Create;
  FastSrc := TacFast32.Create;

  if AlphaBmp = nil then AlphaBmp := CreateBmp32(MagnSize.cx, MagnSize.cy) else begin
    AlphaBmp.Width := MagnSize.cx;
    AlphaBmp.Height := MagnSize.cy;
  end;

  Bmp := CreateBmp32(AlphaBmp.Width, AlphaBmp.Height);
  PaintControlByTemplate(Bmp, MagnBmp, Rect(0, 0, Bmp.Width, Bmp.Height), Rect(0, 0, MagnBmp.Width, MagnBmp.Height), Rect(wL, wT, wR, wB), Rect(1, 1, 1, 1), True);
  Bmp.Canvas.Lock;
  AlphaBmp.Canvas.Lock;
  Bmp.Modified := False;
  BitBlt(AlphaBmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
  // Copy content
  if ThumbWnd = nil then begin
    BitBlt(AlphaBmp.Canvas.Handle, mRect.Left, mRect.Top, WidthOf(mRect), HeightOf(mRect), FTempBmp.Canvas.Handle, 0, 0, SRCCOPY);

    if FastDst.Attach(AlphaBmp) and FastSrc.Attach(Bmp) then for y := mRect.Top to mRect.Bottom do begin
      Dst := AlphaBmp.ScanLine[y];
      Src := Bmp.ScanLine[y];
      for x := mRect.Left to mRect.Right do begin
        CSrc := Src[x];
        CDst := Dst[x];
        CDst.R := (((CSrc.R - CDst.R) * CSrc.A + CDst.R shl 8) shr 8) and MaxByte;
        CDst.G := (((CSrc.G - CDst.G) * CSrc.A + CDst.G shl 8) shr 8) and MaxByte;
        CDst.B := (((CSrc.B - CDst.B) * CSrc.A + CDst.B shl 8) shr 8) and MaxByte;
        CDst.A := MaxByte;
        Dst[x] := CDst;
      end;
    end;
  end
  else if FastDst.Attach(AlphaBmp) then for y := mRect.Top to mRect.Bottom do begin
    Dst := AlphaBmp.ScanLine[y];
    for x := mRect.Left to mRect.Right do begin
      CDst := Dst[x];
      if CDst.A = 0 then CDst.A := 1;
      Dst[x] := CDst;
    end;
  end;
  Bmp.Canvas.UnLock;
  AlphaBmp.Canvas.UnLock;

  Bmp.Free;

  FreeAndnil(FastSrc);
  FreeAndnil(FastDst);
end;

procedure TacMagnForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
end;

{ TsMagnifier }

constructor TsMagnifier.Create(AOwner: TComponent);
begin
  inherited;
{$IFDEF D2007}
  MainFormOnTaskBar := Application.MainFormOnTaskBar;
{$ENDIF}
  FScaling := 2;
  IsModal := False;
  FHeight := 250;
  FWidth := FHeight;
  FSizingMode := asmFreeAspectRatio;
end;

destructor TsMagnifier.Destroy;
begin
  if Assigned(acMagnForm) then FreeAndNil(acMagnForm);
  inherited;
end;

procedure ChangeAppWindow(const Handle: THandle; const SetAppWindow, RestoreVisibility: Boolean);
var
  Style: Longint;
  WasVisible, WasIconic: Boolean;
begin
  Style := GetWindowLong(Handle, GWL_EXSTYLE);
  if (SetAppWindow and (Style and WS_EX_APPWINDOW = 0)) or
     (not SetAppWindow and (Style and WS_EX_APPWINDOW = WS_EX_APPWINDOW)) then begin
    WasIconic := Windows.IsIconic(Handle);
    WasVisible := IsWindowVisible(Handle);
    if WasVisible or WasIconic then ShowWindow(Handle, SW_HIDE);
    if SetAppWindow then SetWindowLong(Handle, GWL_EXSTYLE, Style or WS_EX_APPWINDOW) else SetWindowLong(Handle, GWL_EXSTYLE, Style and not WS_EX_APPWINDOW);
    if (RestoreVisibility and WasVisible) or WasIconic then begin
      if WasIconic then ShowWindow(Handle, SW_MINIMIZE) else ShowWindow(Handle, SW_SHOW);
    end;
  end;
end;

procedure TsMagnifier.Execute(x : integer = -1; y : integer = -1);
var
  i : integer;
begin
  if acMagnForm = nil then begin
    acMagnForm := TacMagnForm.Create(nil);

{$IFDEF D2007}
    if AeroIsEnabled and Application.MainFormOnTaskBar and (Application.MainForm <> nil) and Application.MainForm.Visible then begin
      acTaskBarChanging := True;
      SendMessage(Application.MainForm.Handle, WM_SETREDRAW, 0, 0);
      ChangeAppWindow(Application.MainForm.Handle, False, True);
      ChangeAppWindow(Application.Handle, True, False);
      Application.MainFormOnTaskBar := False;
      SendMessage(Application.MainForm.Handle, WM_SETREDRAW, 1, 0);
      acTaskBarChanging := False;
    end;
{$ENDIF}
    acMagnForm.Visible := False;
    TacMagnForm(acMagnForm).Caller := Self;
    TacMagnForm(acMagnForm).Width := Width;
    TacMagnForm(acMagnForm).Height := Height;
    TacMagnForm(acMagnForm).FormCreateInit;

    TacMagnForm(acMagnForm).Scale := FScaling;

    if FPopupMenu <> nil then TacMagnForm(acMagnForm).PopupMenu := FPopupMenu else begin
      for i := 0 to TacMagnForm(acMagnForm).PopupMenu1.Items.Count - 1 do if TacMagnForm(acMagnForm).PopupMenu1.Items[i].Tag = FScaling then begin
        TacMagnForm(acMagnForm).PopupMenu1.Items[i].Checked := True;
        Break;
      end;
    end;
    if (x <> -1) or (y <> -1) then begin
      TacMagnForm(acMagnForm).Position := poDesigned;
      acMagnForm.Left := x;
      acMagnForm.Top := y;
    end;

    SetWindowLong(TacMagnForm(acMagnForm).Handle, GWL_EXSTYLE, GetWindowLong(acMagnForm.Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
    if IsModal then TacMagnForm(acMagnForm).ShowModal else TacMagnForm(acMagnForm).Show;
  end
  else TacMagnForm(acMagnForm).BringToFront
end;

procedure TacMagnForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then Close;
end;

procedure TacMagnForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Assigned(TsMagnifier(Caller).OnMouseDown) then TsMagnifier(Caller).OnMouseDown(Caller, Button, Shift, X, Y);
  if (mbLeft = Button) then begin
    acIsDragging := True;
    ReleaseCapture;
    Perform(WM_SYSCOMMAND, $F012, 0);
  end;
end;

procedure TacMagnForm.FormResize(Sender: TObject);
begin
  if ThumbWnd <> nil then UpdateThumbPos;
end;

function TsMagnifier.IsVisible: Boolean;
begin
  if acMagnForm <> nil
    then Result := TForm(acMagnForm).Visible
    else Result := False;
end;

function TsMagnifier.GetPosition: TPoint;
begin
  if acMagnForm <> nil then begin
    Result.X := TForm(acMagnForm).Left;
    Result.Y := TForm(acMagnForm).Top ;
  end
  else begin
    Result.X := -1;
    Result.Y := -1;
  end;
end;

procedure TsMagnifier.Hide;
begin
  if acMagnForm <> nil then TForm(acMagnForm).Close
end;

procedure TsMagnifier.SetScaling(const Value: integer);
begin
  if FScaling = Value then Exit;
  if Value < 2 then FScaling := 2 else if Value > 16 then FScaling := 16 else FScaling := Value;
  if acMagnForm <> nil then TacMagnForm(acMagnForm).SetZooming(FScaling);
end;

procedure TacMagnForm.FormShow(Sender: TObject);
begin
  Closing := False;
  if ThumbWnd <> nil then begin
    ThumbWnd.UpdateThumbnail;
    SetWindowPos(ThumbWnd.Handle, Handle, Left + cL, Top + wT, WidthOf(MClientrect), HeightOf(MClientRect), SWP_SHOWWINDOW or SWP_NOACTIVATE);
  end;
end;

procedure TacMagnForm.Image1DblClick(Sender: TObject);
begin
  if Assigned(TsMagnifier(Caller).OnDblClick) then TsMagnifier(Caller).OnDblClick(Caller);
end;

procedure TacMagnForm.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
{$IFDEF DELPHI7UP}
  if not Mouse.IsDragging and acIsDragging then
{$ENDIF}
  begin
    if Assigned(TsMagnifier(Caller).OnMouseUp) then TsMagnifier(Caller).OnMouseUp(TObject(Caller), mbLeft, Shift, X, Y);
    acIsDragging := False;
  end
end;

destructor TacMagnForm.Destroy;
begin
{$IFDEF D2007}
  if AeroIsEnabled and Caller.MainFormOnTaskBar and not Application.MainFormOnTaskBar and (Application.MainForm <> nil) and Application.MainForm.Visible then begin
    acTaskBarChanging := True;
    SendMessage(Application.MainForm.Handle, WM_SETREDRAW, 0, 0);
    ChangeAppWindow(Application.Handle, False, False);
    ChangeAppWindow(Application.MainForm.Handle, True, True);
    Application.MainFormOnTaskBar := True;
    SendMessage(Application.MainForm.Handle, WM_SETREDRAW, 1, 0);
    acTaskBarChanging := False;
  end;
{$ENDIF}
  if Assigned(MagnBmp) then FreeAndNil(MagnBmp);

  if Assigned(FTempBmp) then FreeAndNil(FTempBmp);
  if Assigned(AlphaBmp) then FreeAndNil(AlphaBmp);
  inherited;
end;

procedure TsMagnifier.Refresh;
begin
  if Assigned(acMagnForm) then SendMessage(acMagnForm.Handle, SM_ALPHACMD, MakeWParam(0, AC_REFRESH), 0);
end;

function TacMagnForm.WMNCHitTest(var Message: TWMNCHitTest) : integer;
const
  bWidth = 8;
  HitArrayLT : array[boolean, boolean] of Cardinal = ((HTCLIENT, HTTOP), (HTLEFT, HTTOPLEFT));
  HitArrayBR : array[boolean, boolean] of Cardinal = ((HTCLIENT, HTBOTTOM), (HTRIGHT, HTBOTTOMRIGHT));
var
  p : TPoint;
  mRect : TRect;
  R1, R2 : integer;
begin
  p := Point(Message.Pos.x, Message.Pos.y);
  p := ScreenToClient(p);
  mRect := MClientRect;
  InflateRect(mRect, 4, 4);
  if PTInRect(mRect, p) then begin
    if Caller.SizingMode <> asmNone then begin
      R1 := HitArrayLT[PtInRect(Rect(mRect.Left, mRect.Top, mRect.Left + bWidth, mRect.Bottom), p), PtInRect(Rect(mRect.Left, mRect.Top, mRect.Right, mRect.Top + bWidth), p)];
      R2 := HitArrayBR[PtInRect(Rect(mRect.Right - bWidth, mRect.Top, mRect.Right, mRect.Bottom), p), PtInRect(Rect(mRect.Left, mRect.Bottom - bWidth, mRect.Right, mRect.Bottom), p)];
      if R1 = HTCLIENT then Result := R2 else begin
        if (R1 = HTTOP) and (R2 = HTRIGHT)
          then Result := HTTOPRIGHT
          else if (R1 = HTLEFT) and (R2 = HTBOTTOM)
            then Result := HTBOTTOMLEFT
            else Result := R1;
      end;
    end
    else Result := HTCLIENT;
  end
  else Result := HTTRANSPARENT;
end;

function TacMagnForm.MClientRect: TRect;
begin
  Result := Rect(cL, cT, MagnSize.cx - cR, MagnSize.cy - cB);
end;

function TacMagnForm.MagnSize: TSize;
begin
  Result.cx := max(MinSize, acMagnForm.Width);
  Result.cy := max(MinSize, acMagnForm.Height);
  Result.cx := min(MaxSize, Result.cx);
  Result.cy := min(MaxSize, Result.cy);
end;

end.
