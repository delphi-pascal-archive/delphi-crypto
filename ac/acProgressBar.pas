unit acProgressBar;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, sCommonData, sConst, ExtCtrls;

type
{$IFNDEF D2009}
  TProgressBarStyle = (pbstNormal, pbstMarquee);
{$ENDIF}

  TsProgressBar = class(TProgressBar)
{$IFNDEF NOTFORHELP}
  private
    FOldCount : integer;
    Timer : TTimer;
    FCommonData: TsCommonData;
    FProgressSkin: TsSkinSection;
    FMarqPos : integer;
    FMarqSize : integer;
    FMarqStep : integer;
    FOrient : integer;
{$IFNDEF D2009}
    FSavedPosition: Integer;
    FStyle: TProgressBarStyle;
    FMarqueeInterval: Integer;
{$ENDIF}
    procedure PrepareCache;
    function ProgressRect : TRect;
    function ItemSize : TSize;
    function ClRect : TRect;
    procedure SetProgressSkin(const Value: TsSkinSection);
{$IFNDEF D2009}
    procedure SetStyle(const Value: TProgressBarStyle);
    procedure SetMarqueeInterval(const Value: Integer);
{$ENDIF}
    procedure TimerAction(Sender : TObject);
  public
    procedure Paint(DC : hdc);
    constructor Create(AOwner: TComponent); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Loaded; override;
    procedure WndProc (var Message: TMessage); override;
  published
{$ENDIF}
    property ProgressSkin : TsSkinSection read FProgressSkin write SetProgressSkin;
    property SkinData : TsCommonData read FCommonData write FCommonData;
{$IFNDEF D2009}
    property Style: TProgressBarStyle read FStyle write SetStyle default pbstNormal;
    property MarqueeInterval: Integer read FMarqueeInterval write SetMarqueeInterval default 10;
{$ENDIF}
  end;

implementation

uses sMessages, sVclUtils, sGraphUtils, acntUtils, sAlphaGraph, sSkinProps, CommCtrl
{$IFDEF DELPHI7UP}
  , Themes
{$ENDIF};

const
  iNdent = 2;

{ TsProgressBar }

procedure TsProgressBar.AfterConstruction;
begin
  inherited;
  FCommonData.Loaded;
end;

function TsProgressBar.ClRect: TRect;
begin
  Result := Rect(0, 0, Width, Height);
  InflateRect(Result, -1, -1);
end;

constructor TsProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMarqSize := 40;
  FMarqStep := 6;
  FOldCount := -1;
{$IFNDEF D2009}
  FMarqueeInterval := 10;
  FStyle := pbstNormal;
{$ENDIF}
  FCommonData := TsCommonData.Create(Self, False);
  FCommonData.COC := COC_TsGauge;
  ControlStyle := ControlStyle + [csOpaque];
  Timer := TTimer.Create(Self);
  Timer.Enabled := False;
  Timer.Interval := 30;
  Timer.OnTimer := TimerAction;
  FOrient := 1;
end;

destructor TsProgressBar.Destroy;
begin
  FreeAndNil(FCommonData);
  FreeAndNil(Timer);
  inherited Destroy;
end;

function TsProgressBar.ItemSize: TSize;
const
  prop = 0.66;
begin
  if Style = pbstMarquee then begin
    if Orientation = pbVertical then begin
      Result.cx := WidthOf(clRect) - BorderWidth * 2;
      Result.cy := 9;
    end
    else begin
      Result.cy := HeightOf(clRect) - BorderWidth * 2;
      Result.cx := 9;
    end;
  end
  else begin
    if Orientation = pbVertical then begin
      Result.cx := WidthOf(clRect) - BorderWidth * 2;
      if Smooth
        then Result.cy := ProgressRect.Bottom
        else Result.cy := Round(Result.cx * prop) - Indent;
    end
    else begin
      Result.cy := HeightOf(clRect) - BorderWidth * 2;
      if Smooth
        then Result.cx := ProgressRect.Right
        else Result.cx := Round(Result.cy * prop) - Indent;
    end;
  end
end;

procedure TsProgressBar.Loaded;
begin
  inherited;
  FCommonData.Loaded;
  if not (csDesigning in ComponentState) and (Style = pbstMarquee) and not Timer.Enabled then Timer.Enabled := True;  
end;

procedure TsProgressBar.Paint;
var
  NewDC, SavedDC : hdc;
begin
  if (Width < 1) or (Height < 1) then Exit;
  if DC = 0 then NewDC := GetWindowDC(Handle) else NewDC := DC;
  SavedDC := SaveDC(NewDC);
  try
    FCommonData.Updating := FCommonData.Updating;
    if not FCommonData.Updating then begin
      FCommonData.BGChanged := FCommonData.BGChanged or FCommonData.HalfVisible or GetBoolMsg(Parent, AC_GETHALFVISIBLE);
      FCommonData.HalfVisible := not RectInRect(Parent.ClientRect, BoundsRect);

      if FCommonData.BGChanged then PrepareCache;

      if FCommonData.FCacheBmp <> nil then begin
        UpdateCorners(FCommonData, 0);

        CopyWinControlCache(Self, FCommonData, Rect(0, 0, 0, 0), Rect(0, 0, Width, Height), NewDC, True);

        sVCLUtils.PaintControls(NewDC, Self, True, Point(0, 0));
      end;
    end;
  finally
    RestoreDC(NewDC, SavedDC);
    if DC = 0 then ReleaseDC(Handle, NewDC);
  end;
end;

procedure TsProgressBar.PrepareCache;
var
  si, i, d, c, value, w, h : integer;
  s : string;
  ci : TCacheInfo;
  Bmp : TBitmap;
  prRect : TRect;
  iSize : TSize;
begin
  if (Style <> pbstMarquee) and not Smooth then begin
    if (FCommonData.FCacheBmp <> nil) and (FCommonData.FCacheBmp.Width = Width) and (FCommonData.FCacheBmp.Height = Height) then begin
      iSize := ItemSize;
      if Orientation = pbHorizontal then begin
        w := WidthOf(clRect) - iNdent;
        c := w div (iSize.cx + iNdent);
      end
      else begin
        h := HeightOf(clRect) - iNdent;
        c := h div (iSize.cy + iNdent);
      end;
      if c > 1 then value := Round(c / Max * Position) else value := 0;
      if value = FOldCount then Exit else FOldCount := value;
    end;
  end;
  FCommonData.InitCacheBmp;
  PaintItem(FCommonData, GetParentCache(FCommonData), True, 0, Rect(0, 0, width, Height), Point(Left, Top), FCommonData.FCacheBMP, False);
  if Max <= Min then Exit;

  if (ProgressSkin <> '') then s := ProgressSkin else begin
    if Orientation = pbVertical then s := s_ProgressV else s := s_ProgressH;
  end;
  si := FCommonData.SkinManager.GetSkinIndex(s);
  ci := MakeCacheInfo(FCommonData.FCacheBmp);
  prRect := ProgressRect;
  if (prRect.Right <= prRect.Left) or (prRect.Bottom <= prRect.Top) then Exit;

  iSize := ItemSize;
  if (iSize.cx < 2) or (iSize.cy < 2) then Exit;
  Bmp := CreateBmp24(iSize.cx, iSize.cy);

  if Style = pbstMarquee then begin
    d := 1;
    if Orientation = pbHorizontal then for i := 0 to 4 do begin
      c := prRect.Left + i * (iSize.cx + d);
      if c > Width then c := c - Width;
      PaintItem(si, s, ci, True, 0, Rect(0, 0, Bmp.Width, Bmp.Height), Point(c, prRect.Top), BMP, FCommonData.SkinManager);

      BitBlt(FCommonData.FCacheBmp.Canvas.Handle, c, prRect.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    end
    else for i := 0 to 4 do begin
      c := prRect.Bottom - i * (iSize.cy + d) - iSize.cy;
      if c < 0 then c := Height + c;
      PaintItem(si, s, ci, True, 0, Rect(0, 0, Bmp.Width, Bmp.Height), Point(prRect.Left, c), BMP, FCommonData.SkinManager);
      BitBlt(FCommonData.FCacheBmp.Canvas.Handle, prRect.Left, c, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    end;
  end
  else begin
    if Orientation = pbHorizontal then begin
      if Smooth then begin
        Bmp.Width := WidthOf(prRect);
        PaintItem(si, s, ci, True, 0, Rect(0, 0, Bmp.Width, Bmp.Height), Point(prRect.Left, prRect.Top), BMP, FCommonData.SkinManager);
        BitBlt(FCommonData.FCacheBmp.Canvas.Handle, prRect.Left, prRect.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
      end
      else begin
        w := WidthOf(clRect) - iNdent;
        c := w div (iSize.cx + iNdent);
        if c > 1 then begin
          d := (w - c * iSize.cx) div (c - 1);
          value := Round(c / Max * Position);
          for i := 0 to value - 1 do begin
            PaintItem(si, s, ci, True, 0, Rect(0, 0, Bmp.Width, Bmp.Height), Point(prRect.Left + i * (iSize.cx + d), prRect.Top), BMP, FCommonData.SkinManager);
            BitBlt(FCommonData.FCacheBmp.Canvas.Handle, prRect.Left + i * (iSize.cx + d), prRect.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
          end;
          if (Value > 0) and (Position = Max) and (w - (Value - 1) * (iSize.cx + d) - iSize.cx > 3) then begin
            Bmp.Width := w - (Value - 1) * (iSize.cx + d) - iSize.cx;
            PaintItem(si, s, ci, True, 0, Rect(0, 0, Bmp.Width, Bmp.Height), Point(prRect.Left + Value * (iSize.cx + d), prRect.Top), BMP, FCommonData.SkinManager);
            BitBlt(FCommonData.FCacheBmp.Canvas.Handle, prRect.Left + (Value * (iSize.cx + d)), prRect.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
          end;
        end;
      end;
    end
    else begin
      if Smooth then begin
        Bmp.Height := HeightOf(prRect);
        PaintItem(si, s, ci, True, 0, Rect(0, 0, Bmp.Width, Bmp.Height), Point(prRect.Left, prRect.Top), BMP, FCommonData.SkinManager);
        BitBlt(FCommonData.FCacheBmp.Canvas.Handle, prRect.Left, prRect.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
      end
      else begin
        h := HeightOf(clRect) - iNdent;
        c := h div (iSize.cy + iNdent);
        if c > 1 then begin
          d := (h - c * iSize.cy) div (c - 1);
          value := Round(c / Max * Position);
          for i := 0 to value - 1 do begin
            PaintItem(si, s, ci, True, 0, Rect(0, 0, Bmp.Width, Bmp.Height), Point(prRect.Left, prRect.Bottom - i * (iSize.cy + d) - iSize.cy), BMP, FCommonData.SkinManager);
            BitBlt(FCommonData.FCacheBmp.Canvas.Handle, prRect.Left, prRect.Bottom - i * (iSize.cy + d) - iSize.cy, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
          end;
          if (Value > 0) and (Position = Max) and (h - (Value - 1) * (iSize.cy + d) - iSize.cy > 3) then begin
            Bmp.Height := HeightOf(clRect) - Value * (iSize.cy + d);
            PaintItem(si, s, ci, True, 0, Rect(0, 0, Bmp.Width, Bmp.Height), Point(prRect.Left, prRect.Top), BMP, FCommonData.SkinManager);
            BitBlt(FCommonData.FCacheBmp.Canvas.Handle, prRect.Left, prRect.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
          end;
        end;
      end;
    end;
  end;

  FreeAndNil(Bmp);
end;

function TsProgressBar.ProgressRect: TRect;
begin
  Result := clRect;
  InflateRect(Result, -BorderWidth, -BorderWidth);
  if Style = pbstMarquee then begin
    if Orientation = pbVertical then begin
      Result.Bottom := Height - FMarqPos;
      Result.Top := Result.Bottom - FMarqSize;
(*      inc(FMarqPos, FOrient * FMarqStep);

{$IFDEF D2009}
      if State = pbsError then begin
        if (FMarqPos >= Height - FMarqSize - BorderWidth - FMarqStep) or (FMarqPos <= BorderWidth) then FOrient := -1 * FOrient;
      end
      else
{$ENDIF}
        if (FMarqPos >= Height - FMarqStep) then FMarqPos := BorderWidth;*)
    end
    else begin
      Result.Left := Result.Left + FMarqPos;
      Result.Right := Result.Left + FMarqSize;
(*
      inc(FMarqPos, FOrient * FMarqStep);
{$IFDEF D2009}
      if State = pbsError then begin
        if (FMarqPos >= Width - FMarqSize - BorderWidth - FMarqStep) or (FMarqPos <= BorderWidth) then FOrient := -1 * FOrient;
      end
      else
{$ENDIF}
        if (FMarqPos >= Width - FMarqStep) then FMarqPos := 0;
*)
    end;
  end
  else begin
    if Orientation = pbVertical then begin
      Result.Top := Result.Bottom - Round(((Height - 2 * Result.Left) / (Max - Min)) * (Position));
      if Position = Max
        then Result.Top := Result.Left
        else Result.Top := Result.Bottom - Round(((Height - 2 * Result.Left) / (Max - Min)) * (Position));
    end
    else begin
      if Position = Max
        then Result.Right := Width - Result.Left
        else Result.Right := Round(((Width - 2 * Result.Left) / (Max - Min)) * (Position));
    end;
  end;
end;

procedure TsProgressBar.SetProgressSkin(const Value: TsSkinSection);
begin
  if FProgressSkin <> Value then begin
    FProgressSkin := Value;
    FCommonData.Invalidate;
  end;
end;

{$IFNDEF D2009}
procedure TsProgressBar.SetMarqueeInterval(const Value: Integer);
{$IFDEF DELPHI7UP}
var
  MarqueeEnabled: Boolean;
{$ENDIF}
begin
  FMarqueeInterval := Value;
{$IFNDEF D2009}
{$IFDEF DELPHI7UP}
  if (FStyle = pbstMarquee) and ThemeServices.ThemesEnabled and CheckWin32Version(5, 1) and HandleAllocated then
  begin
    MarqueeEnabled := FStyle = pbstMarquee;
    SendMessage(Handle, WM_USER + 10{PBM_SETMARQUEE}, WPARAM(MarqueeEnabled), LPARAM(FMarqueeInterval));
  end;
{$ELSE}
//    MarqueeEnabled := FStyle = pbstMarquee;
{$ENDIF}
{$ENDIF}
end;

procedure TsProgressBar.SetStyle(const Value: TProgressBarStyle);
{$IFDEF DELPHI7UP}
var
  MarqueeEnabled: Boolean;
{$ENDIF}
begin
  if FStyle <> Value then begin
    FStyle := Value;
    if FStyle = pbstMarquee then begin
      FSavedPosition := Position;
      DoubleBuffered := False;
    end;
    Timer.Enabled := SkinData.Skinned and (FStyle = pbstMarquee);
{$IFDEF DELPHI7UP}
    if ThemeServices.ThemesEnabled and CheckWin32Version(5, 1) and HandleAllocated then begin
      MarqueeEnabled := FStyle = pbstMarquee;
      SendMessage(Handle, WM_USER + 10{PBM_SETMARQUEE}, WPARAM(THandle(MarqueeEnabled)), LPARAM(FMarqueeInterval));
    end;
{$ENDIF}
    RecreateWnd;
    if FStyle = pbstNormal then Position := FSavedPosition;
  end;
end;
{$ENDIF}

procedure TsProgressBar.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    PBM_SETPOS : begin
      if Style = pbstMarquee then Perform(WM_SETREDRAW, 0, 0);
      inherited;
      if Style = pbstMarquee then Perform(WM_SETREDRAW, 1, 0);
      Exit
    end;
  end;
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_CTRLHANDLED : begin Message.Result := 1; Exit end; // AlphaSkins supported
    AC_GETAPPLICATION : begin Message.Result := longint(Application); Exit end;
    AC_REMOVESKIN : if LongWord(Message.LParam) = LongWord(SkinData.SkinManager) then begin
      if Style = pbstMarquee then Timer.Enabled := False;
      CommonWndProc(Message, FCommonData);
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_FRAME or RDW_UPDATENOW);
      Exit
    end;
    AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FCommonData);
      Repaint;
      FOldCount := -1;
      Exit
    end;
    AC_SETNEWSKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FCommonData);
      if not (csDesigning in ComponentState) and (Style = pbstMarquee) then Timer.Enabled := True;
      FOldCount := -1;
      exit
    end;
    AC_ENDPARENTUPDATE : if FCommonData.Updating then begin
      FCommonData.Updating := False;
      Repaint; Exit
    end;
    AC_GETBG : begin
      PacBGInfo(Message.LParam)^.Offset := Point(0, 0);
      PacBGInfo(Message.LParam).Bmp := FCommonData.FCacheBmp;
      PacBGInfo(Message.LParam)^.BgType := btCache;
      Exit;
    end;
  end;
  if Assigned(FCommonData) and ControlIsReady(Self) and FCommonData.Skinned then begin
    case Message.Msg of
      WM_PRINT : begin
        SkinData.Updating := False;
        Paint(TWMPaint(Message).DC);
        Exit;
      end;
      WM_PAINT : begin
        BeginPaint(Handle, acGlobalPS);
        Paint(TWMPaint(Message).DC);
        EndPaint(Handle, acGlobalPS);
        Message.Result := 0;
        Exit;
      end;
      WM_SIZE, WM_MOVE, WM_WINDOWPOSCHANGED : FOldCount := -1; 
      WM_NCPAINT : begin
        Message.Result := 0;
        Exit;
      end;
      WM_ERASEBKGND : begin
        Message.Result := 1;
        Exit;
      end;
    end;
    CommonWndProc(Message, FCommonData);
  end;
  inherited;
end;

procedure TsProgressBar.CreateParams(var Params: TCreateParams);
begin
  inherited;
{$IFNDEF D2009}
{$IFDEF DELPHI7UP}
  if (FStyle = pbstMarquee) and ThemeServices.ThemesEnabled and CheckWin32Version(5, 1)
    then Params.Style := Params.Style or 8{PBS_MARQUEE};
{$ENDIF}
{$ENDIF}
end;

procedure TsProgressBar.CreateWnd;
begin
  inherited;
{$IFDEF DELPHI7UP}
{$IFNDEF D2009}
  if ThemeServices.ThemesEnabled and CheckWin32Version(5, 1) and HandleAllocated then
  begin
    SendMessage(Handle, WM_USER + 10{PBM_SETMARQUEE}, WPARAM(THandle(FStyle = pbstMarquee)), LPARAM(FMarqueeInterval));
  end;
{$ENDIF}  
{$ENDIF}
end;

procedure TsProgressBar.TimerAction(Sender: TObject);
var
  DC : hdc;
begin
  if SkinData.Skinned then begin
    if Visible then begin
      PrepareCache;
      DC := GetWindowDC(Handle);
      BitBlt(DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
      ReleaseDC(Handle, DC);

      inc(FMarqPos, FOrient * FMarqStep);
{$IFDEF D2009}
      if State = pbsError then begin
        if (FMarqPos >= Width - FMarqSize - BorderWidth - FMarqStep) or (FMarqPos <= BorderWidth) then FOrient := -1 * FOrient;
      end
      else
{$ENDIF}
        if (FMarqPos >= Width - FMarqStep) then FMarqPos := 0;
    end;
  end
  else Timer.Enabled := False;
end;

end.
