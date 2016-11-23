unit sToolBar;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ToolWin, ComCtrls, sCommonData, sConst, sDefaults{$IFDEF TNTUNICODE}, TntComCtrls{$ENDIF};

type
{$IFDEF TNTUNICODE}
  TsToolBar = class(TTntToolBar)
{$ELSE}
  TsToolBar = class(TToolBar)
{$ENDIF}
{$IFNDEF NOTFORHELP}
  private
    HotButtonIndex : integer;
    FCommonData: TsCommonData;
    FDisabledKind: TsDisabledKind;
    procedure WMNCPaint (var Message: TWMNCPaint); message WM_NCPAINT;
    procedure SetDisabledKind(const Value: TsDisabledKind);
  protected
    DroppedButton : TToolButton;
    procedure WndProc (var Message: TMessage); override;
    function OffsetX : integer;
    function OffsetY : integer;

    procedure PrepareCache;
    procedure OurAdvancedCustomDraw(Sender: TToolBar; const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure OurAdvancedCustomDrawButton(Sender: TToolBar; Button: TToolButton; State: TCustomDrawState; Stage: TCustomDrawStage; var Flags: TTBCustomDrawFlags; var DefaultDraw: Boolean);

    function GetButtonRect(Index : integer) : TRect;
    function IndexByMouse(MousePos : TPoint) : integer;
    procedure RepaintButton(Index : integer);
{$IFDEF DELPHI6UP}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Loaded; override;
    procedure UpdateEvents;
  published
    property Flat;
{$ENDIF} // NOTFORHELP
    property BtnDisabledKind : TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property SkinData : TsCommonData read FCommonData write FCommonData;
  end;

implementation

uses acntUtils, sStyleSimply, sVCLUtils, sMessages, sMaskData, sGraphUtils,
  sSKinProps, sAlphaGraph, CommCtrl, ImgList, sSKinManager {$IFDEF LOGGED}, sDebugMsgs{$ENDIF}, sThirdParty, Menus;

{ TsToolBar }

procedure PaintSeparator(const DC : hdc; const R : TRect; const ToolBar : TsToolBar; IsDivider : boolean);
var
  Bmp : TBitmap;
  si, mi : integer;
  nRect : TRect;
  BGInfo : TacBGInfo;
begin
  si := ToolBar.SkinData.SkinManager.GetSkinIndex(s_Divider);
  mi := ToolBar.SkinData.SkinManager.GetMaskIndex(si, s_Divider, s_BordersMask);
  if ToolBar.SkinData.SkinManager.IsValidImgIndex(mi) then begin
    Bmp := CreateBmp24(WidthOf(R), HeightOf(R));
    BGInfo.BgType := btUnknown;
    BGInfo.Offset.X := 0;
    BGInfo.Offset.Y := 0;
    BGInfo.Bmp := nil;
    BGInfo.R := Rect(0, 0, Bmp.Width, Bmp.Height);
    BGInfo.PleaseDraw := False;
    GetBGInfo(@BGInfo, ToolBar.Handle, False);
    if BGInfo.BgType = btCache
      then BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, BGInfo.Bmp.Canvas.Handle, BGInfo.Offset.X + R.Left, BGInfo.Offset.Y + R.Top, SRCCOPY)
      else FillDC(Bmp.Canvas.Handle, Rect(0, 0, Bmp.Width, Bmp.Height), BGInfo.Color);

    nRect.Top := 2;
    nRect.Left := Bmp.Width div 2 - 2;
    nRect.Bottom := Bmp.Height - 2;
    nRect.Right := nRect.Left + 4;

    if IsDivider then
      PaintItem(si, s_Divider, BGInfoToCI(@BGInfo), True, 0, nRect, Point(R.Left + nRect.Left, R.Top + nRect.Top), Bmp, ToolBar.SkinData.SkinManager);

    BitBlt(DC, ToolBar.OffsetX + R.Left, ToolBar.OffsetY + R.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    FreeAndNil(Bmp);
  end;
end;

function TsToolBar.OffsetX: integer;
begin
  Result := (integer(ebLeft in EdgeBorders) + BorderWidth) * integer(EdgeInner <> esNone) + (integer(ebLeft in EdgeBorders) + BorderWidth) * integer(EdgeOuter <> esNone)
end;

function TsToolBar.OffsetY: integer;
begin
  Result := (integer(ebTop in EdgeBorders) + BorderWidth) * integer(EdgeInner <> esNone) + (integer(ebTop in EdgeBorders) + BorderWidth) * integer(EdgeOuter <> esNone)
end;

procedure TsToolBar.AfterConstruction;
begin
  inherited;
  FCommonData.Loaded;
  UpdateEvents;
end;

{$IFDEF DELPHI6UP}
procedure TsToolBar.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if not (csDesigning in ComponentState) and Assigned(FCommonData) and FCommonData.Skinned then begin
    if (AComponent is TPopupMenu) then begin
      if Assigned(DefaultManager) then DefaultManager.SkinableMenus.HookPopupMenu(TPopupMenu(AComponent), Operation = opInsert);
    end;
  end;
end;
{$ENDIF}

constructor TsToolBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommonData := TsCommonData.Create(Self, True);
  FCommonData.COC := COC_TsToolBar;
  FDisabledKind := DefDisabledKind;
  HotButtonIndex := -1;
end;

destructor TsToolBar.Destroy;
begin
  if Assigned(FCommonData) then FreeAndNil(FCommonData);
  inherited Destroy;
end;

function TsToolBar.GetButtonRect(Index: integer): TRect;
begin
  Perform(TB_GETITEMRECT, Index, Longint(@Result))
end;

function TsToolBar.IndexByMouse(MousePos: TPoint): integer;
var
  i : integer;
begin
  Result := -1;
  for i := 0 to ButtonCount - 1 do begin
    if PtInRect(GetButtonRect(i), MousePos) then begin
      if (TControl(Buttons[I]) is TToolButton) and (Buttons[i].Style in [tbsButton, tbsCheck, tbsDropDown]) then Result := i;
      Exit;
    end;
  end;
end;

procedure TsToolBar.Loaded;
begin
  inherited;
  FCommonData.Loaded;
  UpdateEvents;
end;

procedure CopyCache(Control : TWinControl; SkinData : TsCommonData; SrcRect, DstRect : TRect; DstDC : HDC);
var
  SaveIndex : HDC;
  i : integer;
  Designing : boolean;
begin
  sAlphaGraph.UpdateCorners(SkinData, 0);
  SaveIndex := SaveDC(DstDC);
  IntersectClipRect(DstDC, DstRect.Left, DstRect.Top, DstRect.Right, DstRect.Bottom);
  Designing := csDesigning in Control.ComponentState;
  try
    for i := 0 to Control.ControlCount - 1 do begin
      if (Control.Controls[i] is TToolButton) and (csDesigning in Control.ComponentState) then Continue;
      if (Control.Controls[i] is TGraphicControl) and StdTransparency then Continue;
      if not Control.Controls[i].Visible or not RectIsVisible(DstRect, Control.Controls[i].BoundsRect) then Continue;
      if ((csOpaque in Control.Controls[i].ControlStyle) or (Control.Controls[i] is TGraphicControl) or Designing) then begin
        ExcludeClipRect(DstDC, Control.Controls[i].Left, Control.Controls[i].Top,
                          Control.Controls[i].Left + Control.Controls[i].Width,
                          Control.Controls[i].Top + Control.Controls[i].Height);
      end;
    end;
    BitBlt(DstDC, DstRect.Left, DstRect.Top, WidthOf(DstRect), HeightOf(DstRect), SkinData.FCacheBmp.Canvas.Handle, SrcRect.Left, SrcRect.Top, SRCCOPY); 
  finally
    RestoreDC(DstDC, SaveIndex);
  end;
end;

procedure TsToolBar.OurAdvancedCustomDraw(Sender: TToolBar; const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var
  RC, RW: TRect;
begin
  if FCommonData.Skinned then begin
    FCommonData.FUpdating := FCommonData.Updating;
    if not (Stage in [cdPrePaint]) {or FCommonData.Updating v6.12}then begin DefaultDraw := False; Exit end;
    FCommonData.FCacheBMP.Canvas.Font.Assign(Font);
    if FCommonData.BGChanged then PrepareCache;

    Windows.GetClientRect(Handle, RC);
    GetWindowRect(Handle, RW);
    MapWindowPoints(0, Handle, RW, 2);
    OffsetRect(RC, -RW.Left, -RW.Top);

    CopyCache(Self, FCommonData, RC, ARect, Canvas.Handle);
    sVCLUtils.PaintControls(Canvas.Handle, Self, True, Point(0, 0));
    SetParentUpdated(Self);
    if (RC.Left > 0) or (RC.Top > 0) or (RC.Right <> Width) or (RC.Bottom <> Height) then SendMessage(Handle, WM_NCPAINT, 0, 0);
  end
  else begin
    DefaultDraw := True;
    inherited;
  end
end;

procedure TsToolBar.OurAdvancedCustomDrawButton(Sender: TToolBar; Button: TToolButton; State: TCustomDrawState; Stage: TCustomDrawStage; var Flags: TTBCustomDrawFlags; var DefaultDraw: Boolean);
var
  Mode, SkinIndex, BorderIndex : integer;
  ci : TCacheInfo;
  R, iR : TRect;
  BtnBmp : TBitmap;
  bWidth, bHeight : integer;
  function AddedWidth : integer;
  begin
    if (Button.Style = tbsDropDown) then Result := GetSystemMetrics(SM_CXVSCROLL) - 2 else Result := 0
  end;
  function IntButtonWidth : integer;
  begin
    Result := Button.Width - AddedWidth;
  end;
  function ButtonWidth : integer;
  begin
    Result := Button.Width;
  end;
  function ImgRect : TRect; begin
    if not List then begin
      Result.Left := (IntButtonWidth - Images.Width) div 2 + 1;
      Result.Top := (Button.Height - Images.Height - integer(ShowCaptions) * (FCommonData.FCacheBMP.Canvas.TextHeight('A') + 3)) div 2;
      Result.Right := Result.Left + Images.Width;
      Result.Bottom := Result.Bottom + Images.Height;
    end
    else begin
      Result.Left := 5;
      Result.Top := (Button.Height - Images.Height) div 2;
      Result.Right := Result.Left + Images.Width;
      Result.Bottom := Result.Bottom + Images.Height;
    end;
    if Mode = 2 then OffsetRect(Result, 1, 1);
  end;
  procedure DrawBtnCaption;
  var
    cRect : TRect;
    function CaptionRect : TRect; var l, t, r, b, dh : integer; begin
      if not List then begin
        l := (IntButtonWidth - FCommonData.FCacheBMP.Canvas.TextWidth(Button.Caption)) div 2;
        if Assigned(Images) then begin
          dh := (Button.Height - Images.Height - FCommonData.FCacheBMP.Canvas.TextHeight('A') - 3) div 2;
          t := dh + Images.Height + 3;
        end
        else begin
          dh := (Button.Height - FCommonData.FCacheBMP.Canvas.TextHeight('A')) div 2;
          t := dh;
        end;
        r := IntButtonWidth - l;
        b := Button.Height - dh;
        Result := Rect(l - 1, t, r + 2, b);
      end
      else begin
        if Assigned(Images) then Result.Left := 6 + Images.Width else Result.Left := 1;
        Result.Right := IntButtonWidth - 2;
        Result.Top := 2;
        Result.Bottom := Button.Height - 2;
      end;
      OffsetRect(Result, integer(Mode = 2), integer(Mode = 2));
    end;
  begin
    if ShowCaptions then begin
      cRect := CaptionRect;
{$IFDEF TNTUNICODE}
      if Button is TTntToolButton
        then WriteTextExW(BtnBMP.Canvas, PWideChar(TTntToolButton(Button).Caption), True, cRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE, GetFontIndex(Button, SkinIndex, SkinData.SkinManager), Mode > 0)
        else
{$ENDIF}
      acWriteTextEx(BtnBMP.Canvas, PacChar(Button.Caption), True, cRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE, GetFontIndex(Button, SkinIndex, SkinData.SkinManager), Mode > 0);
    end;
  end;
  procedure DrawBtnGlyph;
  begin
    if Assigned(Images) and (Button.ImageIndex > -1) and (Button.ImageIndex < Images.Count) then begin
      sThirdParty.CopyToolBtnGlyph(Self, Button, State, Stage, Flags, BtnBmp);
    end;
  end;
  procedure DrawArrow;
  var
    Mode : integer;
    x, y : integer;
  begin
    if FCommonData.SkinManager.ConstData.MaskArrowBottom > -1 then begin
      if ((DroppedButton = Button) and Assigned(Button.DropDownMenu) or Button.Down) then Mode := 2 else if cdsHot in State then Mode := 1 else Mode := 0;

      R.Left := IntButtonWidth;
      R.Right := Button.Width;
      BorderIndex := FCommonData.SkinManager.GetMaskIndex(SkinIndex, s_ToolButton, s_BordersMask);
      if FCommonData.SkinManager.IsValidImgIndex(BorderIndex) then DrawSkinRect(BtnBmp, R, True, ci, FCommonData.SkinManager.ma[BorderIndex], Mode, True);

      if (FCommonData.SkinManager.ConstData.MaskArrowBottom > -1) and (FCommonData.SkinManager.ConstData.MaskArrowBottom < High(FCommonData.SkinManager.ma)) then begin
        x := IntButtonWidth + (AddedWidth - 3 - WidthOf(FCommonData.SkinManager.ma[FCommonData.SkinManager.ConstData.MaskArrowBottom].R) div FCommonData.SkinManager.ma[FCommonData.SkinManager.ConstData.MaskArrowBottom].ImageCount) div 2 + 2;
        y := (ButtonHeight - HeightOf(FCommonData.SkinManager.ma[FCommonData.SkinManager.ConstData.MaskArrowBottom].R) div (1 + FCommonData.SkinManager.ma[FCommonData.SkinManager.ConstData.MaskArrowBottom].MaskType)) div 2;

        DrawSkinGlyph(BtnBmp, Point(x, y), Mode, 1, FCommonData.SkinManager.ma[FCommonData.SkinManager.ConstData.MaskArrowBottom], MakeCacheInfo(BtnBmp));
      end;
    end;
  end;
begin
  if FCommonData.Skinned then begin
    DefaultDraw := False;
    if not (Stage in [cdPrePaint]) or FCommonData.Updating then begin DefaultDraw := False; Exit end;
    if Stage in [cdPrePaint] then begin
      if not Flat and not (csDesigning in ComponentState) and (HotButtonIndex = Button.Index) then State := State + [cdsHot];
      Flags := Flags + [tbNoEtchedEffect, tbNoEdges];

      iR := GetButtonRect(Button.Index);
      bWidth := WidthOf(iR);
      bHeight := HeightOf(iR);

      BtnBmp := CreateBmp24(bWidth, bHeight);
      BtnBmp.Canvas.Font.Assign(Font);

      if not Button.Marked and not Button.Indeterminate and ((State = []) or (State = [cdsDisabled])) 
        then Mode := 0
        else if (cdsSelected in State) or (cdsChecked in State) or Button.Marked or Button.Indeterminate
          then Mode := 2
          else Mode := 1;
      SkinIndex := FCommonData.SkinManager.GetSkinIndex(s_TOOLBUTTON);
      ci := MakeCacheInfo(FCommonData.FCacheBmp,
                          BorderWidth * 2 + integer(ebLeft in EdgeBorders) * (integer(EdgeInner <> esNone) + integer(EdgeOuter <> esNone)),
                          BorderWidth * 2 + integer(ebTop in EdgeBorders) * (integer(EdgeInner <> esNone) + integer(EdgeOuter <> esNone)));
      R := Rect(0, 0, bWidth, Button.Height);
      OffsetRect(R, ClientRect.Left, ClientRect.Top);

      PaintItemBg(SkinIndex, s_ToolButton, ci, Mode, R, Point(Button.Left, Button.Top), BtnBmp, FCommonData.SkinManager);
      R.Right := bWidth - AddedWidth;

      ci.X := ci.X + Button.Left;
      ci.Y := ci.Y + Button.Top;
      BorderIndex := FCommonData.SkinManager.GetMaskIndex(SkinIndex, s_ToolButton, s_BordersMask);
      if BorderIndex > -1 then DrawSkinRect(BtnBmp, R, True, ci, FCommonData.SkinManager.ma[BorderIndex], Mode, True);

      DrawBtnCaption;
      DrawBtnGlyph;
      if Button.Style = tbsDropDown then DrawArrow;
      if not Button.Enabled or Button.Indeterminate then BmpDisabledKind(BtnBmp, FDisabledKind, Parent, CI, Point(0, 0));

      BitBlt(Canvas.Handle, Button.Left, Button.Top, bWidth, bHeight, BtnBmp.Canvas.Handle, 0, 0, SRCCOPY);
      FreeAndNil(BtnBmp);
    end
  end
  else begin
    DefaultDraw := True;
    inherited;
  end;
end;

procedure TsToolBar.PrepareCache;
begin
  FCommonData.InitCacheBmp;
  PaintItem(FCommonData, GetParentCache(FCommonData), False, 0, Rect(0, 0, WIdth, Height), Point(Left, Top), FCommonData.FCacheBmp, False);
  FCommonData.BGChanged := False;
end;

procedure TsToolBar.RepaintButton(Index : integer);
var
  Flags : TTBCustomDrawFlags;
  Def : boolean;
  DC, SavedDC: HDC;
  RC, RW: TRect;
begin
  if (Index > -1) and Buttons[Index].Visible then begin
    Flags := [tbNoEtchedEffect, tbNoEdges];
    Def := False;
    DC := GetWindowDC(Handle);
    SavedDC := SaveDC(DC);
    try
      Windows.GetClientRect(Handle, RC);
      GetWindowRect(Handle, RW);
      MapWindowPoints(0, Handle, RW, 2);
      OffsetRect(RC, -RW.Left, -RW.Top);
      MoveWindowOrg(DC, -RW.Left, -RW.Top);

      Canvas.Handle := DC;
      OurAdvancedCustomDrawButton(Self, Buttons[Index], [], cdPrePaint, Flags, Def)
    finally
      Canvas.Handle := 0;
      RestoreDC(DC, SavedDC);
      ReleaseDC(Handle, DC);
    end;
  end;
end;

procedure TsToolBar.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    Repaint;
  end;
end;

procedure TsToolBar.UpdateEvents;
begin
  if SkinData.Skinned then begin
    inherited OnAdvancedCustomDraw := OurAdvancedCustomDraw;
    if not (csDesigning in ComponentState) then inherited OnAdvancedCustomDrawButton := OurAdvancedCustomDrawButton;
  end
  else begin
    inherited OnAdvancedCustomDraw := nil;
    if not (csDesigning in ComponentState) then inherited OnAdvancedCustomDrawButton := nil;
  end;
end;

procedure TsToolBar.WMNCPaint(var Message: TWMNCPaint);
var
  DC, SavedDC: HDC;
  RC, RW: TRect;
  w, h : integer;
begin
  if FCommonData.Skinned then begin
    FCommonData.Updating := FCommonData.Updating;
    if (csDestroying in ComponentState) or FCommonData.Updating then Exit;
    if FCommonData.BGChanged and (csDesigning in ComponentState) then PrepareCache;
    DC := GetWindowDC(Handle);
    SavedDC := SaveDC(DC);
    try
      Windows.GetClientRect(Handle, RC);
      GetWindowRect(Handle, RW);
      MapWindowPoints(0, Handle, RW, 2);
      OffsetRect(RC, -RW.Left, -RW.Top);
      ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
      { Draw borders in non-client area }

      w := WidthOf(Rc);
      h := HeightOf(Rc);
      // Top
      BitBlt(DC, 0, 0, Width, Rc.Top, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
      // Left
      BitBlt(DC, 0, Rc.Top, Rc.Left, h, SkinData.FCacheBmp.Canvas.Handle, 0, Rc.Top, SRCCOPY);
      // Bottom
      BitBlt(DC, 0, Rc.Bottom, Width, Height - h - Rc.Top, SkinData.FCacheBmp.Canvas.Handle, 0, Rc.Bottom, SRCCOPY);
      // Right
      BitBlt(DC, Rc.Right, Rc.Top, Width - Rc.Left - w, h, SkinData.FCacheBmp.Canvas.Handle, Rc.Right, Rc.Top, SRCCOPY);

      IntersectClipRect(DC, RW.Left, RW.Top, RW.Right, RW.Bottom);
    finally
      RestoreDC(DC, SavedDC);
      ReleaseDC(Handle, DC);
    end;
  end
  else inherited;
end;

procedure TsToolBar.WndProc(var Message: TMessage);
var
  i, OldIndex, w, h : integer;
  rc{, R} : TRect;
  DC : hdc;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_CTRLHANDLED : begin Message.Result := 1; Exit end; 
    AC_GETAPPLICATION : begin Message.Result := longint(Application); Exit end;
    AC_SETNEWSKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      if not (csDesigning in ComponentState) and (@Ac_SetWindowTheme <> nil) then Ac_SetWindowTheme(Handle, ' ', ' ');
      AlphaBroadCast(Self, Message);
      CommonWndProc(Message, FCommonData);
      exit
    end;
    AC_ENDPARENTUPDATE : if FCommonData.Updating then begin
      FCommonData.Updating := False;
      FCommonData.BGChanged := True;
      RedrawWindow(Handle, nil, 0, RDW_UPDATENOW or RDW_ERASE or RDW_INVALIDATE or RDW_FRAME);
      Repaint;
      Exit
    end;
    AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      DroppedButton := nil;
      CommonWndProc(Message, FCommonData);
      AlphaBroadcast(Self, Message);
      UpdateEvents;
      Repaint;
      SendMessage(Handle, WM_NCPAINT, 0, 0);
      exit
    end;
    AC_REMOVESKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      DroppedButton := nil;
      CommonWndProc(Message, FCommonData);
      if not (csDesigning in ComponentState) and (@Ac_SetWindowTheme <> nil) then Ac_SetWindowTheme(Handle, nil, nil);
      AlphaBroadcast(Self, Message);
      UpdateEvents;
      Repaint;
      SendMessage(Handle, WM_NCPAINT, 0, 0);
      exit
    end
  end;
  if not ControlIsReady(Self) or not FCommonData.Skinned then inherited else begin
    if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
      AC_GETBG : begin
        PacBGInfo(Message.LParam)^.Offset := Point(0, 0);
        InitBGInfo(FCommonData, PacBGInfo(Message.LParam), 0);
        PacBGInfo(Message.LParam)^.Offset.X := PacBGInfo(Message.LParam)^.Offset.X + OffsetX;
        PacBGInfo(Message.LParam)^.Offset.Y := PacBGInfo(Message.LParam)^.Offset.Y + OffsetY;
        Exit;
      end;
    end
    else case Message.Msg of
      WM_PRINT : begin
        FCommonData.Updating := False;
        try
          RC := ACClientRect(Handle);
          w := WidthOf(Rc);
          h := HeightOf(Rc);
          if FCommonData.BGChanged then PrepareCache;
          // Top
          BitBlt(TWMPaint(Message).DC, 0, 0, Width, Rc.Top, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
          // Left
          BitBlt(TWMPaint(Message).DC, 0, Rc.Top, Rc.Left, h, SkinData.FCacheBmp.Canvas.Handle, 0, Rc.Top, SRCCOPY);
          // Bottom
          BitBlt(TWMPaint(Message).DC, 0, Rc.Bottom, Width, Height - h - Rc.Top, SkinData.FCacheBmp.Canvas.Handle, 0, Rc.Bottom, SRCCOPY);
          // Right
          BitBlt(TWMPaint(Message).DC, Rc.Right, Rc.Top, Width - Rc.Left - w, h, SkinData.FCacheBmp.Canvas.Handle, Rc.Right, Rc.Top, SRCCOPY);


          MoveWindowOrg(TWMPaint(Message).DC, Rc.Left, Rc.Top);
          IntersectClipRect(TWMPaint(Message).DC, 0, 0, w, h);
          SendMessage(Handle, WM_PAINT, Message.WParam, 0);
          MoveWindowOrg(TWMPaint(Message).DC, -Rc.Left, -Rc.Top);

          for i := 0 to ButtonCount - 1 do if (Buttons[i].Style in [tbsSeparator, tbsDivider]) and Buttons[i].Visible then begin
            rc := Buttons[i].BoundsRect;
            PaintSeparator(TWMPaint(Message).DC, rc, Self, Flat or (Buttons[i].Style = tbsDivider));
          end;
        finally
        end;
        Exit;
      end;
      WM_ERASEBKGND : if not (csDesigning in ComponentState) then begin
        FCommonData.FUpdating := FCommonData.Updating;
        Message.Result := 1;
        Exit;
      end;
      CM_COLORCHANGED : Exit;
      WM_SIZE : SkinData.BGChanged := True;
      CM_TEXTCHANGED : begin Repaint; Exit end;
      CN_NOTIFY : with TWMNotify(Message) do case NMHdr^.code of
        TBN_DROPDOWN: with PNMToolBar(NMHdr)^ do if Perform(TB_GETBUTTON, iItem, Longint(@tbButton)) <> 0 then begin
          DroppedButton := TToolButton(tbButton.dwData);
          DroppedButton.Repaint
        end;
        TBN_DELETINGBUTTON : if HotButtonIndex >= ButtonCount then HotButtonIndex := -1;
      end;
      WM_PAINT : if InAnimationProcess and (0 = acPrintDC) then Exit;
    end;
    if CommonWndProc(Message, FCommonData) then Exit;
    inherited;
    case Message.Msg of
      WM_PAINT : if not InAnimationProcess then begin
        if TWMPAint(Message).DC = 0 then DC := GetWindowDC(Handle) else DC := TWMPAint(Message).DC;
        w := SkinData.SkinManager.GetSkinIndex(s_Divider);
        if (w >-1) then begin
          for i := 0 to ButtonCount - 1 do if (Buttons[i].Style in [tbsSeparator, tbsDivider]) and Buttons[i].Visible then begin
            if not Buttons[i].Wrap then begin
              rc := Buttons[i].BoundsRect;
              PaintSeparator(DC, rc, Self, Flat or (Buttons[i].Style = tbsDivider));
            end;
          end;
        end;
        if TWMPAint(Message).DC = 0 then ReleaseDC(Handle, DC);
      end;
      CN_DROPDOWNCLOSED : begin
        if DroppedButton <> nil then begin
          HotButtonIndex := -1;
          i := DroppedButton.Index;
          DroppedButton := nil;
          RepaintButton(i);
        end;
      end;
      CM_MOUSELEAVE : if not Flat and not (csDesigning in ComponentState) then begin
        OldIndex := HotButtonIndex;
        HotButtonIndex := -1;
        if (OldIndex > -1) and not Buttons[OldIndex].Down then RepaintButton(OldIndex);
        HotButtonIndex := -1;
      end;
      WM_MOUSEMOVE : if not Flat and not (csDesigning in ComponentState) then with TWMMouse(Message) do begin
        i := IndexByMouse(Point(TWMMouse(Message).XPos, TWMMouse(Message).YPos));
        if (i <> HotButtonIndex) then begin
          if (i > -1) and not Buttons[i].Enabled then Exit; // v4.50
          OldIndex := HotButtonIndex;
          HotButtonIndex := i;
          if (OldIndex > -1) and not Buttons[OldIndex].Down then RepaintButton(OldIndex);
          if (HotButtonIndex > -1) and not Buttons[HotButtonIndex].Down then RepaintButton(HotButtonIndex);
        end;
      end;
      WM_SIZE, CM_VISIBLECHANGED, CM_ENABLEDCHANGED : begin
        FCommonData.BGChanged := True;
        InvalidateRect(Handle, nil, False);
        Exit;
      end;
    end;
  end;
end;

end.
