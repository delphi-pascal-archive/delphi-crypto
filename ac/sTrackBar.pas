unit sTrackBar;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, comctrls,
  commctrl, consts, sConst, acntUtils, sGraphUtils, ExtCtrls, sDefaults, sCommonData, {$IFNDEF DELPHI5}types,{$ENDIF}
  sFade{$IFDEF LOGGED}, sDebugMsgs{$ENDIF};

type

  TsTrackBar = class;
{$IFNDEF NOTFORHELP}
  TAPoint = array of TPoint;
{$ENDIF}

  TsTrackBar = class(TTrackBar)
{$IFNDEF NOTFORHELP}
  private
    FDisabledKind: TsDisabledKind;
    FOnUserChange: TNotifyEvent;
    FCommonData: TsCommonData;

    FCanvas: TCanvas;
    FAnimatEvents: TacAnimatEvents;
    FadeTimer : TsFadeTimer;
    FShowFocus: boolean;
    FOnSkinPaint: TPaintEvent;
    procedure SetDisabledKind(const Value: TsDisabledKind);
    procedure SetShowFocus(const Value: boolean);
  protected
    AppShowHint : boolean;
    procedure PaintWindow(DC: HDC); override;
    property Canvas: TCanvas read FCanvas;

    procedure WndProc (var Message: TMessage); override;
    procedure UserChanged;
  public
    TickHeight : integer;
    iStep : real;
    Thumb : TBitmap;

    procedure PaintBody;
    procedure PaintBar; virtual;
    procedure PaintTicksHor;
    procedure PaintTicksVer;
    procedure PaintTick(P : TPoint; Horz : boolean);

    procedure PaintThumb(i: integer);
    function ThumbRect: TRect;
    function ChannelRect: TRect;
    function TickPos(i: integer): integer;
    function TickCount : integer;
    function TicksArray : TAPoint;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint;
    procedure PrepareCache;
    procedure AfterConstruction; override;
    procedure Loaded; override;
    function Mode : integer;

  published
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property ThumbLength default 23;
{$ENDIF} // NOTFORHELP
    property AnimatEvents : TacAnimatEvents read FAnimatEvents write FAnimatEvents default [aeGlobalDef];
    property DisabledKind : TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property SkinData : TsCommonData read FCommonData write FCommonData;
    property ShowFocus : boolean read FShowFocus write SetShowFocus default False;
{$IFNDEF NOTFORHELP}
    property OnUserChange: TNotifyEvent read FOnUserChange write FOnUserChange; // KJS
    property OnSkinPaint : TPaintEvent read FOnSkinPaint write FOnSkinPaint;
{$ENDIF} // NOTFORHELP
  end;

implementation

uses sBorders, sStyleSimply, sMaskData, sSkinProps, sAlphaGraph, sVCLUtils,
  sMessages, math, sSkinManager;

{ TsTrackBar }

constructor TsTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommonData := TsCommonData.Create(Self, True);
  FCommonData.COC := COC_TsTrackBar;

  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;

  Thumb := TBitmap.Create;
  Thumb.PixelFormat := pf24Bit;

//v4.66
  ControlStyle := ControlStyle - [csOpaque];

  TickHeight := 4;
  ThumbLength := 23;

  FDisabledKind := DefDisabledKind;
  FAnimatEvents := [aeGlobalDef];
end;

destructor TsTrackBar.Destroy;
begin
  StopFading(FadeTimer, FCommonData);
  if Assigned(FCommonData) then FreeAndNil(FCommonData);

  if Assigned(Thumb) then FreeAndNil(Thumb);
  FCanvas.Free;
  inherited Destroy;
end;

procedure TsTrackBar.WndProc(var Message: TMessage);
var
  DC, SavedDC : hdc;
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
      RecreateWnd;
      exit
    end;
    AC_SETNEWSKIN, AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      StopFading(FadeTimer, FCommonData);
      CommonWndProc(Message, FCommonData);
      Repaint;
      exit
    end;
    AC_PREPARECACHE : PrepareCache;
    AC_STOPFADING : begin StopFading(FadeTimer, FCommonData); Exit end;
    AC_DRAWANIMAGE : begin
      Message.Result := 0;
      if Message.LParam <> 0 then try
        DC := GetWindowDC(Handle);
        SavedDC := SaveDC(DC);
        try
          BitBlt(DC, 0, 0, Width, Height, TBitmap(Message.LParam).Canvas.Handle, 0, 0, SRCCOPY);
        finally
          RestoreDC(DC, SavedDC);
          ReleaseDC(Handle, DC);
        end;
      finally
        Message.Result := 1;
      end;
      Exit;
    end;
    AC_ENDPARENTUPDATE : if FCommonData.Updating then begin
      FCommonData.Updating := False;
      Repaint;
    end
  end;
  if not ControlIsReady(Self) or not FCommonData.Skinned(True) then inherited else begin
    case Message.Msg of
      WM_PRINT : begin
        PaintWindow(TWMPaint(Message).DC);
      end;
      WM_PAINT : begin
        ControlState := ControlState + [csCustomPaint];
      end;
      WM_ERASEBKGND : Exit;
      WM_SETFOCUS, CM_ENTER : if not (csDesigning in ComponentState) then begin
        inherited;
        if Enabled then begin
          if FadeTimer = nil then Repaint else FadeTimer.Change; // Fast repaint
        end;
        Exit;
      end;
      WM_KILLFOCUS, CM_EXIT: if not (csDesigning in ComponentState) then begin
        inherited;
        if Enabled then begin
          if FadeTimer <> nil then StopFading(FadeTimer, FCommonData);
          Exit
        end;
      end;
      WM_LBUTTONUP : if not (csDesigning in ComponentState) and Enabled then begin
        Application.ShowHint := AppShowHint;
        ShowHintStored := False;
        if PtInRect(ThumbRect, SmallPointToPoint(TWMMouse(Message).Pos)) then begin
          ControlState := ControlState - [csLButtonDown];
          DoChangePaint(FadeTimer, FCommonData, True, EventEnabled(aeMouseUp, FAnimatEvents), fdUp);
        end
        else if FadeTimer <> nil then StopFading(FadeTimer, FCommonData);
      end;
      WM_LBUTTONDBLCLK, WM_LBUTTONDOWN : if not (csDesigning in ComponentState) and Enabled then begin
        if not ShowHintStored then begin
          AppShowHint := Application.ShowHint;
          Application.ShowHint := False;
          ShowHintStored := True;
        end;
        if PtInRect(ThumbRect, SmallPointToPoint(TWMMouse(Message).Pos)) then begin
          ControlState := ControlState + [csLButtonDown];
          Skindata.BGChanged := False;
          DoChangePaint(FadeTimer, FCommonData, True, EventEnabled(aeMouseDown, FAnimatEvents));
        end
        else if FadeTimer <> nil then StopFading(FadeTimer, FCommonData);
      end;
      CN_HSCROLL, CN_VSCROLL : {if not PtInRect(ThumbRect, ScreenToClient(acMousePos)) and (FadeTimer <> nil) then} begin
        StopFading(FadeTimer, FCommonData);
        Repaint;
      end;
    end;
    CommonWndProc(Message, FCommonData);
    inherited;
    case Message.Msg of
      WM_MOVE : if csDesigning in ComponentState then Repaint;
      WM_PAINT : ControlState := ControlState - [csCustomPaint];
      CM_MOUSEENTER : if not (csDesigning in ComponentState) and not (csLButtonDown in ControlState) then begin
        FCommonData.FMouseAbove := True;
        FCommonData.BGChanged := False;
        DoChangePaint(FadeTimer, FCommonData, False, EventEnabled(aeMouseEnter, FAnimatEvents));
      end;
      CM_MOUSELEAVE : if not (csDesigning in ComponentState) and not (csLButtonDown in ControlState) then begin
        FCommonData.FMouseAbove := False;
        FCommonData.BGChanged := False;
        DoChangePaint(FadeTimer, FCommonData, False, EventEnabled(aeMouseLeave, FAnimatEvents));
      end;
    end;
  end;
  case Message.Msg of
    CN_HSCROLL, CN_VSCROLL : UserChanged;
  end;
end;

procedure TsTrackBar.PaintBody;
var
  R : TRect;
begin
  R := ClientRect;
  PaintItem(FCommonData, GetParentCache(FCommonData), True, integer(ControlIsActive(FCommonData)),
    R, Point(Left, Top), FCommonData.FCacheBmp, False);
  if FShowFocus and (Focused or (csLButtonDown in ControlState)) then begin
    InflateRect(R, -1, -1);
    FocusRect(FCommonData.FCacheBMP.Canvas, R);
  end;
  PaintBar;

  if Assigned(FOnSkinPaint) then FOnSkinPaint(Self, FCommonData.FCacheBMP.Canvas);

  PaintThumb(Position);
end;

procedure TsTrackBar.PaintBar;
var
  w, h, i : integer;
  aRect : TRect;
  CI : TCacheInfo;
begin
  aRect := ChannelRect;
  i := SkinData.SkinManager.GetMaskIndex(FCommonData.SkinIndex, FCommonData.SkinSection, s_SliderChannelMask);
  if SkinData.SkinManager.IsValidImgIndex(i) then begin
    case Orientation of
      trHorizontal: begin
        h := SkinData.SkinManager.MaskSize(i).cy - 1 {v5.05};
        w := HeightOf(aRect);
        aRect.Top := aRect.Top + (w - h) div 2;
        aRect.Bottom := aRect.Top + h;
      end;
      trVertical: begin
        h := SkinData.SkinManager.MaskSize(i).cx - 1 {v5.05};
        w := WidthOf(aRect);
        aRect.Left := aRect.Left + (w - h) div 2;
        aRect.Right := aRect.Left + h;
      end;
    end;
{    if (SkinData.FOwnerControl <> nil) and (SkinData.FOwnerControl.Parent <> nil) then begin
      CtrlParentColor := ColorToRGB(TsHackedControl(SkinData.FOwnerControl.Parent).Color);
    end;}
    CI := MakeCacheInfo(FCommonData.FCacheBmp);
    DrawSkinRect(FCommonData.FCacheBmp, aRect, True, CI, SkinData.SkinManager.ma[i], integer(ControlIsActive(FCommonData)), True);
  end;
  if Orientation = trHorizontal then PaintTicksHor else PaintTicksVer;
end;

const
  SelSize = 3;

procedure TsTrackBar.PaintTicksHor;
var
  i, mh : integer;
  pa : TAPoint;
  cr : TRect;
  ArrowPoints: array of TPoint;
  dw : real;
begin
  pa := nil;
  if TickStyle <> tsNone then begin
    pa := TicksArray;
    cr := ChannelRect;
    mh := (HeightOf(ThumbRect) - HeightOf(cr)) div 2 + 2;
    if TickMarks in [tmTopLeft, tmBoth] then for i := 0 to High(pa) do begin
      if ((SelStart = 0) and (SelEnd = 0)) or ((i <> SelStart) and (i <> SelEnd)) then PaintTick(Point(pa[i].x, cr.Top - mh - TickHeight), True);
    end;
    if TickMarks in [tmBottomRight, tmBoth] then for i := 0 to High(pa) do begin
      if ((SelStart = 0) and (SelEnd = 0)) or ((i <> SelStart) and (i <> SelEnd)) then PaintTick(Point(pa[i].x, cr.Bottom + mh), True);
    end;
  end;
  if (SelStart > 0) or (SelEnd > 0) then begin
    dw := (WidthOf(ChannelRect) - WidthOf(ThumbRect)) / (TickCount - 1);
    SetLength(ArrowPoints, 3);
    FCommonData.FCacheBmp.Canvas.Brush.Style := bsSolid;
    FCommonData.FCacheBmp.Canvas.Brush.Color := FCommonData.SkinManager.GetGlobalFontColor;
    FCommonData.FCacheBmp.Canvas.Pen.Color := FCommonData.SkinManager.GetGlobalFontColor;
    if TickMarks in [tmTopLeft, tmBoth] then begin
      // SelStart
      i := Round(dw * (SelStart + 1));
      ArrowPoints[0] := Point(i, cr.Top - mh - TickHeight);
      ArrowPoints[1] := Point(ArrowPoints[0].X, ArrowPoints[0].Y - SelSize);
      ArrowPoints[2] := Point(ArrowPoints[0].X - SelSize, ArrowPoints[0].Y - SelSize);
      FCommonData.FCacheBmp.Canvas.Polygon(ArrowPoints);
      // SelEnd
      i := Round(dw * (SelEnd + 1));
      ArrowPoints[0] := Point(i, cr.Top - mh - TickHeight);
      ArrowPoints[1] := Point(ArrowPoints[0].X, ArrowPoints[0].Y - SelSize);
      ArrowPoints[2] := Point(ArrowPoints[0].X + SelSize, ArrowPoints[0].Y - SelSize);
      FCommonData.FCacheBmp.Canvas.Polygon(ArrowPoints);
    end;
    if TickMarks in [tmBottomRight, tmBoth] then begin
      // SelStart
      i := Round(dw * (SelStart + 1));
      ArrowPoints[0] := Point(i, cr.Bottom + mh);
      ArrowPoints[1] := Point(ArrowPoints[0].X, ArrowPoints[0].Y + SelSize);
      ArrowPoints[2] := Point(ArrowPoints[0].X - SelSize, ArrowPoints[0].Y + SelSize);
      FCommonData.FCacheBmp.Canvas.Polygon(ArrowPoints);
      // SelEnd
      i := Round(dw * (SelEnd + 1));
      ArrowPoints[0] := Point(i, cr.Bottom + mh);
      ArrowPoints[1] := Point(ArrowPoints[0].X, ArrowPoints[0].Y + SelSize);
      ArrowPoints[2] := Point(ArrowPoints[0].X + SelSize, ArrowPoints[0].Y + SelSize);
      FCommonData.FCacheBmp.Canvas.Polygon(ArrowPoints);
    end
  end
end;

procedure TsTrackBar.PaintThumb(i: integer);
var
  aRect, NewRect : TRect;
  Bmp : TBitmap;
  md : TsMaskData;
  function NotStretched : boolean; begin
    Result := False;
    with SkinData.SkinManager do begin
      if Orientation = trHorizontal then begin
        if (HeightOf(aRect) = HeightOf(ma[i].R) div (ma[i].MaskType + 1)) and (WidthOf(aRect) = WidthOf(ma[i].R) div ma[i].ImageCount) then Result := True;
        if (HeightOf(aRect) = 23) and (HeightOf(ma[i].R) div (ma[i].MaskType + 1) = 21) and (WidthOf(aRect) = 11) then Result := True;
      end
      else begin
        if (HeightOf(aRect) = HeightOf(ma[i].R) div (ma[i].MaskType + 1)) and (WidthOf(aRect) = WidthOf(ma[i].R) div ma[i].ImageCount) then Result := True;
        if (WidthOf(aRect) = 23) and (WidthOf(ma[i].R) div ma[i].ImageCount = 21) and (HeightOf(aRect) = 11) then Result := True;
      end;
    end;
  end;
  procedure RotateBmp180(Bmp : TBitmap; Horz : boolean);
  var
    x, y : integer;
    c : TColor;
  begin
    if not Horz then begin
      for x := 0 to Bmp.Width - 1 do for y := 0 to (Bmp.Height - 1) div 2 do begin
        c := Bmp.Canvas.Pixels[x, y];
        Bmp.Canvas.Pixels[x, y] := Bmp.Canvas.Pixels[x, Bmp.Height - y - 1];
        Bmp.Canvas.Pixels[x, Bmp.Height - y - 1] := c
      end;
    end
    else begin
      for y := 0 to Bmp.Height - 1 do for x := 0 to (Bmp.Width - 1) div 2 do begin
        c := Bmp.Canvas.Pixels[x, y];
        Bmp.Canvas.Pixels[x, y] := Bmp.Canvas.Pixels[Bmp.Width - x - 1, y];
        Bmp.Canvas.Pixels[Bmp.Width - x - 1, y] := c
      end;
    end;
  end;
begin
  aRect := ThumbRect;

  if Orientation = trHorizontal
    then i := SkinData.SkinManager.GetMaskIndex(FCommonData.SkinIndex, FCommonData.SkinSection, s_SliderHorzMask)
    else i := SkinData.SkinManager.GetMaskIndex(FCommonData.SkinIndex, FCommonData.SkinSection, s_SliderVertMask);
  if SkinData.SkinManager.IsValidImgIndex(i) then with SkinData.SkinManager do begin
    if NotStretched then begin
      if TickMarks = tmTopLeft then begin
        Bmp := CreateBmp24(WidthOf(aRect), HeightOf(aRect));
        try
          BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, FCommonData.FCacheBmp.Canvas.Handle, aRect.Left, aRect.Top, SRCCOPY);
          RotateBmp180(Bmp, Orientation <> trHorizontal);
          DrawSkinGlyph(Bmp, Point(0, 0), Mode, 1, ma[i], MakeCacheInfo(Bmp));
          RotateBmp180(Bmp, Orientation <> trHorizontal);
          BitBlt(FCommonData.FCacheBmp.Canvas.Handle, aRect.Left, aRect.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
        finally
          Bmp.Free;
        end
      end
      else DrawSkinGlyph(FCommonData.FCacheBmp, point(aRect.Left, aRect.Top), Mode, 1, ma[i], MakeCacheInfo(FCommonData.FCacheBmp))
    end
    else if MasterBitmap <> nil then begin
      NewRect := Rect(0, 0, WidthOf(aRect), HeightOf(aRect)); NewRect.Right := NewRect.Right * 3; NewRect.Bottom := NewRect.Bottom * (ma[i].MaskType + 1);
      Bmp := CreateBmp24(WidthOf(NewRect), HeightOf(NewRect));
      if ma[i].MaskType = 0 then begin
        StretchBlt(Bmp.Canvas.Handle, 0, 0, WidthOf(NewRect),
                   HeightOf(NewRect), MasterBitmap.Canvas.Handle,
                   ma[i].R.Left, ma[i].R.Top, WidthOf(ma[i].R), HeightOf(ma[i].R), SRCCOPY);
      end
      else begin
        StretchBlt(Bmp.Canvas.Handle,
                   0, 0, WidthOf(NewRect), HeightOf(aRect), MasterBitmap.Canvas.Handle,
                   ma[i].R.Left, ma[i].R.Top, WidthOf(ma[i].R), HeightOf(ma[i].R) div 2, SRCCOPY);

        StretchBlt(Bmp.Canvas.Handle,
                   0, HeightOf(aRect), WidthOf(NewRect), Bmp.Height, MasterBitmap.Canvas.Handle,
                   ma[i].R.Left, ma[i].R.Top + HeightOf(ma[i].R) div 2, WidthOf(ma[i].R), HeightOf(ma[i].R), SRCCOPY);
      end;
      md.Bmp := Bmp;
      md.BorderWidth := 0;
      md.MaskType := ma[i].MaskType;
      md.ImageCount := ma[i].ImageCount;
      md.R := Rect(0, 0, Bmp.Width, Bmp.Height);

      DrawSkinGlyph(FCommonData.FCacheBmp, point(aRect.Left, aRect.Top), Mode, 1, md, MakeCacheInfo(FCommonData.FCacheBmp));

      FreeAndNil(Bmp);
    end;
  end;
end;

function TsTrackBar.ThumbRect: TRect;
begin
  Result := Rect(0, 0, 1, 1);
  SendMessage(Handle, TBM_GETTHUMBRECT, 0, longint(@Result));
end;

function TsTrackBar.ChannelRect: TRect;
begin
  Result := Rect(0, 0, 1, 1);
  SendMessage(Handle, TBM_GETCHANNELRECT, 0, longint(@Result));
  if Orientation = trVertical then begin
    Changei(Result.Left, Result.Top);
    Changei(Result.Right, Result.Bottom);
  end;
end;

function TsTrackBar.TickPos(i: integer): integer;
var
  Value : longint;
begin
  Value := longint(i);
  Result := SendMessage(Handle, TBM_GETTICPOS, Value, 0);
//  Result := Value;
end;

function TsTrackBar.TickCount: integer;
begin
  Result := SendMessage(Handle, TBM_GETNUMTICS, 0, 0);
end;

function TsTrackBar.TicksArray: TAPoint;
var
  i, w, c : integer;
  ChRect, ThRect : TRect;
begin
  Result := nil;
  ChRect := ChannelRect;
  ThRect := ThumbRect;
  c := TickCount;
  SetLength(Result, c);
  if TickStyle = tsAuto then begin
    if Orientation = trVertical then begin
      iStep := (HeightOf(ChRect) - HeightOf(ThRect)) / (TickCount - 1);
      w := HeightOf(ThRect) div 2;
      for i := 0 to c - 1 do Result[i] := Point(0, Round(ChRect.Top + i * iStep + w));
    end
    else begin
      iStep := (WidthOf(ChRect) - WidthOf(ThRect)) / (TickCount - 1);
      w := WidthOf(ThRect) div 2;
      for i := 0 to c - 1 do Result[i] := Point(Round(ChRect.Left + i * iStep + w), 0);
    end;
  end
  else begin
    if Orientation = trVertical then begin
      Result[0] := Point(0, ChRect.Top + HeightOf(ThRect) div 2);
      for i := 0 to c - 3 do Result[i + 1] := Point(0, TickPos(i));
      Result[c - 1] := Point(0, ChRect.Bottom - HeightOf(ThRect) div 2);
    end
    else begin
      Result[0] := Point(ChRect.Left + WidthOf(ThRect) div 2, 0);
      for i := 0 to c - 3 do Result[i + 1] := Point(TickPos(i), 0);
      Result[c - 1] := Point(ChRect.Right - WidthOf(ThRect) div 2, 0);
    end;
  end;
end;

procedure TsTrackBar.PaintTicksVer;
var
  i, mh : integer;
  pa : TAPoint;
  cr : TRect;
  ArrowPoints: array of TPoint;
  dh : real;
begin
  if TickStyle <> tsNone then begin
    pa := TicksArray;
    cr := ChannelRect;
    mh := (WidthOf(ThumbRect) - WidthOf(cr)) div 2 + 2;
    if TickMarks in [tmTopLeft, tmBoth] then for i := 0 to High(pa) do begin
      if ((SelStart = 0) and (SelEnd = 0)) or ((i <> SelStart) and (i <> SelEnd)) then PaintTick(Point(cr.Left - mh - TickHeight, pa[i].y), False);
    end;
    if TickMarks in [tmBottomRight, tmBoth] then for i := 0 to High(pa) do begin
      if ((SelStart = 0) and (SelEnd = 0)) or ((i <> SelStart) and (i <> SelEnd)) then PaintTick(Point(cr.Right + mh, pa[i].y), False);
    end;
  end
  else pa := nil;
  if (SelStart > 0) or (SelEnd > 0) then begin
    dh := (HeightOf(ChannelRect) - HeightOf(ThumbRect)) / (TickCount - 1);
    SetLength(ArrowPoints, 3);
    FCommonData.FCacheBmp.Canvas.Brush.Style := bsSolid;
    FCommonData.FCacheBmp.Canvas.Brush.Color := FCommonData.SkinManager.GetGlobalFontColor;
    FCommonData.FCacheBmp.Canvas.Pen.Color := FCommonData.SkinManager.GetGlobalFontColor;
    if TickMarks in [tmTopLeft, tmBoth] then begin
      // SelStart
      i := Round(dh * (SelStart + 1));
      ArrowPoints[0] := Point(cr.Left - mh - TickHeight, i);
      ArrowPoints[1] := Point(ArrowPoints[0].X - SelSize, ArrowPoints[0].Y - SelSize);
      ArrowPoints[2] := Point(ArrowPoints[0].X - SelSize, ArrowPoints[0].Y);
      FCommonData.FCacheBmp.Canvas.Polygon(ArrowPoints);
      // SelEnd
      i := Round(dh * (SelEnd + 1));
      ArrowPoints[0] := Point(cr.Left - mh - TickHeight, i);
      ArrowPoints[1] := Point(ArrowPoints[0].X - SelSize, ArrowPoints[0].Y);
      ArrowPoints[2] := Point(ArrowPoints[0].X - SelSize, ArrowPoints[0].Y + SelSize);
      FCommonData.FCacheBmp.Canvas.Polygon(ArrowPoints);
    end;
    if TickMarks in [tmBottomRight, tmBoth] then begin
      // SelStart
      i := Round(dh * (SelStart + 1));
      ArrowPoints[0] := Point(cr.Right + mh, i);
      ArrowPoints[1] := Point(ArrowPoints[0].X + SelSize, ArrowPoints[0].Y - SelSize);
      ArrowPoints[2] := Point(ArrowPoints[0].X + SelSize, ArrowPoints[0].Y);
      FCommonData.FCacheBmp.Canvas.Polygon(ArrowPoints);
      // SelEnd
      i := Round(dh * (SelEnd + 1));
      ArrowPoints[0] := Point(cr.Right + mh, i);
      ArrowPoints[1] := Point(ArrowPoints[0].X + SelSize, ArrowPoints[0].Y);
      ArrowPoints[2] := Point(ArrowPoints[0].X + SelSize, ArrowPoints[0].Y + SelSize);
      FCommonData.FCacheBmp.Canvas.Polygon(ArrowPoints);
    end
  end
end;

procedure TsTrackBar.Paint;
begin
  if (csDestroying in ComponentState) or not FCommonData.Skinned then inherited else begin
    FCommonData.Updating := FCommonData.Updating;
    if not FCommonData.Updating and not (Assigned(FadeTimer) and FadeTimer.Enabled) then begin
      PrepareCache;
      if FCommonData.SkinIndex > -1 then UpdateCorners(FCommonData, 0);
      BitBlt(Canvas.Handle, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
    end;
  end;
end;

procedure TsTrackBar.AfterConstruction;
begin
  inherited;
  FCommonData.Loaded;
end;

procedure TsTrackBar.Loaded;
begin
  inherited;
  FCommonData.Loaded;
end;

procedure TsTrackBar.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FCommonData.Invalidate;
  end;
end;

procedure TsTrackBar.UserChanged;
begin
  if Assigned(FOnUserChange) then FOnUserChange(Self);
end;

procedure TsTrackBar.PaintWindow(DC: HDC);
begin
  if FCommonData.Skinned then begin
    if not Assigned(FadeTimer) or (FadeTimer.FDirection = fdNone) then begin
      FCanvas.Lock;
      if DC <> 0 then FCanvas.Handle := DC else GetWindowDC(FCanvas.Handle); 
      try
        TControlCanvas(FCanvas).UpdateTextFlags;
        Paint;
      finally
        if DC = 0 then ReleaseDC(Handle, FCanvas.Handle);
        FCanvas.Handle := 0;
        FCanvas.Unlock;
      end;
    end
  end
  else inherited;
end;

procedure TsTrackBar.PrepareCache;
var
  CI : TCacheInfo;
begin
  FCommonData.InitCacheBmp;
  PaintBody;
  if not Enabled then begin
    CI := GetParentCache(FCommonData);
    BmpDisabledKind(FCommonData.FCacheBmp, FDisabledKind, Parent, CI, Point(Left, Top));
  end;
end;

procedure TsTrackBar.PaintTick(P : TPoint; Horz: boolean);
var
  GlyphIndex : integer;
  w : integer;
  R : TRect;
begin
  if Horz
    then GlyphIndex := SkinData.SkinManager.GetMaskIndex(SkinData.SkinIndex, SkinData.SkinSection, s_TICKHORZ)
    else GlyphIndex := SkinData.SkinManager.GetMaskIndex(SkinData.SkinIndex, SkinData.SkinSection, s_TICKVERT);
  if GlyphIndex <> -1 then begin
    if Horz
      then dec(P.x, WidthOf(SkinData.SkinManager.ma[GlyphIndex].R) div (2 * SkinData.SkinManager.ma[GlyphIndex].ImageCount))
      else dec(P.y, HeightOf(SkinData.SkinManager.ma[GlyphIndex].R) div (2 * (1 + SkinData.SkinManager.ma[GlyphIndex].MaskType)));
    DrawSkinGlyph(SkinData.FCacheBmp, P, Mode, 1, SkinData.SkinManager.ma[GlyphIndex], MakeCacheInfo(FCommonData.FCacheBmp))
  end
  else begin
    if Horz
      then R := Rect(P.x, P.y, P.x + 2, P.Y + TickHeight)
      else R := Rect(P.x, P.y, P.x + TickHeight, P.Y + 2);
    w := 1;
    DrawRectangleOnDC(FCommonData.FCacheBmp.Canvas.Handle, R, ColorToRGB(clBtnShadow), ColorToRGB(clWhite), w);
  end;
end;

function TsTrackBar.Mode: integer;
begin
  if (csLButtonDown in ControlState) //and PtInRect(ThumbRect, ScreenToClient(acMousePos))
    then Result := 2
    else if ControlIsActive(FCommonData) then Result := 1 else Result := 0;
end;

procedure TsTrackBar.SetShowFocus(const Value: boolean);
begin
  FShowFocus := Value;
  if FShowFocus <> Value then FCommonData.Invalidate;
end;

end.
