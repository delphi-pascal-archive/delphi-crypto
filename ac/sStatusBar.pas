unit sStatusBar;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, sConst,
  ComCtrls, sCommonData{$IFDEF TNTUNICODE}, TntComCtrls{$ENDIF};

type
{$IFDEF TNTUNICODE}
  TsStatusBar = class(TTntStatusBar)
{$ELSE}
  TsStatusBar = class(TStatusBar)
{$ENDIF}
{$IFNDEF NOTFORHELP}
  private
    FCommonData: TsCommonData;
  protected
    procedure PaintGrip;
    procedure PaintPanels;
    function PanelOffset(k: integer) : integer;

{$IFDEF TNTUNICODE}
    procedure DrawPanel(Panel: TTntStatusPanel; const Rect: TRect); reintroduce;
    procedure InternalDrawPanel(Panel: TTntStatusPanel; Text: WideString; Rect: TRect);
{$ELSE}
    procedure DrawPanel(Panel: TStatusPanel; const Rect: TRect); reintroduce;
    procedure InternalDrawPanel(Panel: TStatusPanel; Text: string; Rect: TRect);
{$ENDIF}
    procedure DoDrawText(const Text : acString; var Rect: TRect; Flags: Longint);

    procedure PrepareCache;
    procedure OurPaintHandler(MsgDC: hdc);
    procedure WndProc (var Message: TMessage); override;
    procedure PaintWindow(DC: HDC); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure AfterConstruction; override;
  published
{$ENDIF} // NOTFORHELP
    property SkinData : TsCommonData read FCommonData write FCommonData;
  end;

implementation

uses sMessages, sMaskData, acntUtils, sVCLUtils, sGraphUtils, sStyleSimply,
  sSkinManager, sSkinProps, sAlphaGraph, math;

{ TsStatusBar }

procedure TsStatusBar.AfterConstruction;
begin
  inherited;
  FCommonData.Loaded;
end;

constructor TsStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommonData := TsCommonData.Create(Self, True);
  FCommonData.COC := COC_TsStatusBar;
  ControlStyle := ControlStyle + [csOpaque, csAcceptsControls];
end;

destructor TsStatusBar.Destroy;
begin
  if Assigned(FCommonData) then FreeAndNil(FCommonData);
  inherited Destroy;
end;

procedure TsStatusBar.DoDrawText(const Text : acString; var Rect: TRect; Flags: Integer);
begin
  Flags := DrawTextBiDiModeFlags(Flags) or DT_SINGLELINE or DT_VCENTER;
  FCommonData.FCacheBMP.Canvas.Font.Assign(Font);
  acWriteTextEx(FCommonData.FCacheBMP.Canvas, PacChar(Text), True, Rect, Flags, FCommonData, ControlIsActive(FCommonData));
end;

{$IFDEF TNTUNICODE}
procedure TsStatusBar.DrawPanel(Panel: TTntStatusPanel; const Rect: TRect);
{$ELSE}
procedure TsStatusBar.DrawPanel(Panel: TStatusPanel; const Rect: TRect);
{$ENDIF}
var
  aRect: TRect;
begin
  if FCommonData.Skinned then begin
    aRect := Rect;
    InflateRect(aRect, -1, -1);
    Canvas.Handle := FCommonData.FCacheBmp.Canvas.Handle;
    InternalDrawPanel(Panel, '', aRect);
    Canvas.Handle := 0;
  end
  else inherited DrawPanel(Panel, Rect);
end;

{$IFDEF TNTUNICODE}
procedure TsStatusBar.InternalDrawPanel(Panel: TTntStatusPanel; Text: WideString; Rect: TRect);
{$ELSE}
procedure TsStatusBar.InternalDrawPanel(Panel: TStatusPanel; Text: string; Rect: TRect);
{$ENDIF}
var
  aRect: TRect;
  index, si, mi, dx: integer;
  s : acString;
  TempBmp : TBitmap;
  CI : TCacheInfo;
begin
  aRect := Rect;
  InflateRect(aRect, -1, -1);

  index := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinIndex, FCommonData.SkinSection, s_StatusPanelBordersMask);
  if FCommonData.SkinManager.IsValidSkinIndex(FCommonData.SkinIndex) then begin
    if FCommonData.SkinManager.IsValidImgIndex(index) then begin
      if SimplePanel or (Panels.Count = 0) then begin
        DrawSkinRect(FCommonData.FCacheBmp, Classes.Rect(0, 0, width, Height), True, EmptyCI, FCommonData.SkinManager.ma[index], 0, True);
      end
      else begin
        DrawSkinRect(FCommonData.FCacheBmp, Rect, True, EmptyCI, FCommonData.SkinManager.ma[index], 0, True);
      end;
    end
    else begin
      if not (SimplePanel or (Panels.Count = 0) or (Panel.Index = Panels.Count - 1)) then begin;
        si := FCommonData.SkinManager.GetSkinIndex(s_Divider);
        if FCommonData.SkinManager.IsValidskinIndex(si) then begin
          mi := FCommonData.SkinManager.GetMaskIndex(si, s_Divider, s_BordersMask);
          if FCommonData.SkinManager.IsValidImgIndex(mi) then begin
            TempBmp := CreateBmp24(max(2, (FCommonData.SkinManager.ma[mi].WL + FCommonData.SkinManager.ma[mi].WR)), Height - 2);
            dx := TempBmp.Width div 2;

            CI := MakeCacheInfo(FCommonData.FCacheBmp);
            PaintItem(si, s_Divider, CI, True, 0, Classes.Rect(0, 0, TempBmp.Width, TempBmp.Height), Point(Rect.Right - dx - 1, 1), TempBmp, SkinData.SkinManager);
            BitBlt(FCommonData.FCacheBmp.Canvas.Handle, Rect.Right - dx - 1, 1, TempBmp.Width, TempBmp.Height, TempBmp.Canvas.Handle, 0, 0, SRCCOPY);
            FreeAndNil(TempBmp);
          end;
        end;
      end;
    end
  end;
  if Assigned(Panel) and (Panel.Style = psOwnerDraw) and Assigned(OnDrawPanel) then begin
    Self.OnDrawPanel(TStatusBar(Self), Panel, Rect)
  end
  else begin
    dec(aRect.Bottom, 1);
    inc(aRect.Left, 2);
    dec(aRect.Right, 4);
    FCommonData.FCacheBmp.Canvas.Font.Assign(Font);

{$IFDEF TNTUNICODE}
    if Assigned(Panel) then begin
      s := Panel.Text;
      DoDrawText(s, aRect, GetStringFlags(Self, Panel.Alignment) or DT_WORD_ELLIPSIS);
    end
    else begin
      s := Text;
      DoDrawText(s, aRect, GetStringFlags(Self, taLeftJustify) or DT_WORD_ELLIPSIS);
    end;
{$ELSE}
    if Assigned(Panel) then begin
      s := CutText(FCommonData.FCacheBmp.Canvas, Panel.Text, WidthOf(aRect));
      DoDrawText(s, aRect, GetStringFlags(Self, Panel.Alignment));
    end
    else begin
      s := CutText(FCommonData.FCacheBmp.Canvas, Text, WidthOf(aRect));
      DoDrawText(s, aRect, GetStringFlags(Self, taLeftJustify));
    end;
{$ENDIF}
  end;
end;

procedure TsStatusBar.Loaded;
begin
  inherited;
  FCommonData.Loaded;
end;

procedure TsStatusBar.OurPaintHandler(MsgDC: hdc);
var
  DC, SavedDC : hdc;
  PS : TPaintStruct;
  bWidth : integer;
begin
  BeginPaint(Handle, PS);
  if MsgDC = 0 then DC := GetDC(Handle) else DC := MsgDC;
  SavedDC := SaveDC(DC);
  try
    FCommonData.Updating := FCommonData.Updating;
    if not FCommonData.Updating then begin

      PrepareCache;
      bWidth := BorderWidth;
      CopyWinControlCache(Self, FCommonData, Rect(bWidth, bWidth, 0, 0), Rect(0, 0, Width, Height), DC, True);

      sVCLUtils.PaintControls(DC, Self, FCommonData.BGChanged, Point(0, 0));
      SetParentUpdated(Self);

      FCommonData.BGChanged := False;
    end;
  finally
    RestoreDC(DC, SavedDC);
    if MsgDC = 0 then ReleaseDC(Handle, DC);
    EndPaint(Handle, PS);
  end;
end;

procedure TsStatusBar.PaintGrip;
var
  i : integer;
  function GripPos : TPoint;
  begin
    if FCommonData.SkinManager.ma[i].Bmp = nil
      then Result := point(Width - WidthOf(FCommonData.SkinManager.ma[i].R) div FCommonData.SkinManager.ma[i].ImageCount {- 1} - BorderWidth, Height - HeightOf(FCommonData.SkinManager.ma[i].R) div (1 + FCommonData.SkinManager.ma[i].MaskType) {- 1} - BorderWidth)
      else Result := point(Width - FCommonData.SkinManager.ma[i].Bmp.Width div 3 {- 1} - BorderWidth, Height - FCommonData.SkinManager.ma[i].Bmp.Height div 2 {- 1} - BorderWidth);
  end;
begin
  if FCommonData.SkinManager = nil then Exit;
  i := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinManager.ConstData.IndexGLobalInfo, s_GlobalInfo, s_GripImage);
  if i = -1 then i := FCommonData.SkinManager.GetMaskIndex(FCommondata.SkinIndex, s_Form, s_GripImage);

  if Assigned(FCommonData.SkinManager) and FCommonData.SkinManager.IsValidImgIndex(i)
    then DrawSkinGlyph(FCommonData.FCacheBmp, GripPos, 0, 1, FCommonData.SkinManager.ma[i], MakeCacheInfo(FCommonData.FCacheBmp));
end;

procedure TsStatusBar.PaintPanels;
var
  i: integer;
begin
  if SimplePanel or (Panels.Count = 0)
    then InternalDrawPanel(nil, SimpleText, Rect(0, 1, Width - 1, Height - 1))
    else for i := 0 to Panels.Count - 1 do begin
      DrawPanel(Panels[i], Rect(PanelOffset(i), 0, iffi(i <> Panels.Count - 1, PanelOffset(i) + Panels[i].Width, Width), Height));
    end
end;

procedure TsStatusBar.PaintWindow(DC: HDC);
begin
  inherited;
  if FCommonData.Skinned then OurPaintHandler(DC);
end;

function TsStatusBar.PanelOffset(k: integer): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Panels.Count - 1 do begin
    if i = k then break;
    inc(Result, Panels[i].Width);
  end;
end;

procedure TsStatusBar.PrepareCache;
var
  CI : TCacheInfo;
  b : boolean;
begin
  // If transparent and form resizing processed
  b := FCommonData.BGChanged or FCommonData.HalfVisible or GetBoolMsg(Parent, AC_GETHALFVISIBLE);
  FCommonData.HalfVisible := not (PtInRect(Parent.ClientRect, Point(Left, Top)) and
                         PtInRect(Parent.ClientRect, Point(Left + Width, Top + Height)));
  if b and not FCommonData.UrgentPainting then begin
    CI := GetParentCache(FCommonData);
    FCommonData.InitCacheBmp;
    PaintItem(FCommonData, CI, False, 0, Rect(0, 0, width, Height), Point(Left, Top), FCommonData.FCacheBMP, False);

    PaintPanels;
    if SizeGrip then PaintGrip;
    FCommonData.BGChanged := False;
  end;
end;

procedure TsStatusBar.WndProc(var Message: TMessage);
var
  SaveIndex: Integer;
  DC: HDC;
  cr : TRect;
begin
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_CTRLHANDLED : begin Message.Result := 1; Exit end; 
    AC_GETAPPLICATION : begin Message.Result := longint(Application); Exit end;
    AC_REMOVESKIN : if LongWord(Message.LParam) = LongWord(SkinData.SkinManager) then begin
      SetBkMode(Canvas.Handle, OPAQUE);
      CommonWndProc(Message, FCommonData);
      RecreateWnd;
      AlphaBroadCast(Self, Message);
      exit
    end;
    AC_SETNEWSKIN, AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      SetBkMode(Canvas.Handle, TRANSPARENT);
      AlphaBroadCast(Self, Message);
      CommonWndProc(Message, FCommonData);
      if Message.WParamHi = AC_REFRESH then begin
      Repaint;
      Perform(WM_NCPAINT, 0, 0); end;
      exit
    end;
    AC_ENDPARENTUPDATE : if FCommonData.FUpdating then begin
      FCommonData.FUpdating := False;
      Repaint;
      Perform(WM_NCPAINT, 0, 0);
      Exit
    End;
  end;
  if not ControlIsReady(Self) or not FCommonData.Skinned then inherited else begin
    case Message.Msg of
      WM_PAINT : begin
        if InAnimationProcess then Exit;              
        ControlState := ControlState + [csCustomPaint];
        OurPaintHandler(TWMPaint(Message).DC);
      end;
      WM_ERASEBKGND : begin
        SkinData.FUpdating := SkinData.Updating;
        if SkinData.FUpdating then Exit;
        if not FCommonData.BGChanged then begin
          CopyWinControlCache(Self, FCommonData, Rect(BorderWidth, BorderWidth, 0, 0), Rect(0, 0, Width, Height), TWMPaint(Message).DC, True);
          sVCLUtils.PaintControls(TWMPaint(Message).DC, Self, FCommonData.BGChanged, Point(0, 0));
        end;
        Exit;
      end;
      WM_MOVE : if (FCommonData.SkinManager.gd[FCommonData.SkinIndex].Transparency > 0) or ((FCommonData.SkinManager.gd[FCommonData.SkinIndex].HotTransparency > 0)) then begin
        FCommonData.BGChanged := True;
        if csDesigning in ComponentState then begin
          Repaint;
          Perform(WM_NCPAINT, 0, 0);
        end;
      end;
      WM_PRINT : begin
        if (not Visible and not (csDesigning in ComponentState)) then exit;
        DC := TWMPaint(Message).DC;
        cr := Rect(1, 1, 1, 1);
        GetClipBox(DC, cR);

        FCommonData.FUpdating := FCommonData.Updating;
        if FCommonData.FUpdating then Exit;

        PrepareCache;

        if (BorderWidth <> 0) then BitBltBorder(DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, BorderWidth);
        MoveWindowOrg(DC, BorderWidth, BorderWidth);
        cR := GetClientRect;
        IntersectClipRect(DC, 0, 0, WidthOf(cR), HeightOf(cR));
        OurPaintHandler(DC);
        Exit;
      end;
      WM_NCPAINT : begin
        if (BorderWidth = 0) or (not Visible and not (csDesigning in ComponentState)) then exit;
        FCommonData.FUpdating := FCommonData.Updating;
        if InAnimationProcess or FCommonData.FUpdating then Exit;

        PrepareCache;
        DC := GetWindowDC(Handle);
        SaveIndex := SaveDC(DC);
        try
          BitBltBorder(DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, BorderWidth);
        finally
          RestoreDC(DC, SaveIndex);
          ReleaseDC(Handle, DC)
        end;
        Exit;
      end;
    end;
    CommonWndProc(Message, FCommonData);
    inherited;
    case Message.Msg of
      DM_SETDEFID, 1033, 1034 : SkinData.BGChanged := True;
      WM_PAINT : ControlState := ControlState - [csCustomPaint];
      WM_SIZE : if Visible and (BorderWidth <> 0) then Perform(WM_NCPAINT, 0, 0);
    end;
  end
end;

end.
