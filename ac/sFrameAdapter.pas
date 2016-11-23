unit sFrameAdapter;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  sConst, sCommondata, sPanel, acSBUtils{$IFDEF LOGGED}, sDebugMsgs{$ENDIF};

type
  TsFrameAdapter = class(TComponent)
{$IFNDEF NOTFORHELP}
  protected
    FCommonData: TsCommonData;
    procedure PrepareCache;
    procedure OurPaintHandler(aDC : hdc);
    procedure AC_WMPaint(aDC: hdc);
  public
    Frame : TFrame;
    OldWndProc: TWndMethod;
    ListSW : TacScrollWnd;
    procedure Loaded; override;
    procedure AfterConstruction; override;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure NewWndProc(var Message: TMessage);
{$ENDIF} // NOTFORHELP
  published
    property SkinData : TsCommonData read FCommonData write FCommonData;
  end;

implementation

uses math, menus, sVclUtils, sBorders, sGraphUtils, sSkinProps, sSkinManager,
  sMaskData{$IFDEF CHECKXP}, UxTheme, Themes{$ENDIF}, acntUtils, sMessages, sStyleSimply,
  sAlphaGraph, sStrings, sSpeedButton;

const
  sWinControlForm = 'TWinControlForm';

{ TsFrameAdapter }

procedure TsFrameAdapter.AC_WMPaint(aDC: hdc);
var
  DC, SavedDC : hdc;
  PS : TPaintStruct;
begin
  if not (csDestroying in Frame.ComponentState) and (Frame.Parent <> nil) and not IsCached(FCommonData) then begin
    InvalidateRect(Frame.Handle, nil, True); // Background update (for repaint of graphic controls and for frame refreshing)
  end;
  BeginPaint(Frame.Handle, PS);
  if aDC = 0 then DC := GetDC(Frame.Handle) else DC := aDC;
  SavedDC := SaveDC(DC);
  try
    if IsCached(FCommonData) then begin
      FCommonData.FUpdating := FCommonData.Updating;
      if not FCommonData.FUpdating then OurPaintHandler(DC);
    end
  finally
    RestoreDC(DC, SavedDC);
    if aDC = 0 then ReleaseDC(Frame.Handle, DC);
    EndPaint(Frame.Handle, PS);
  end;
end;

procedure TsFrameAdapter.AfterConstruction;
begin
  inherited;
  if Assigned(Frame) and GetBoolMsg(Frame, AC_CTRLHANDLED) then begin
    SkinData.UpdateIndexes;
  end;
end;

constructor TsFrameAdapter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCommonData := TsCommonData.Create(Self, True);
  if (FCommonData.SkinSection = ClassName) or (FCommonData.SkinSection = '') then FCommonData.SkinSection := s_GroupBox;
  FCommonData.COC := COC_TsFrameAdapter;

  if AOwner is TFrame then begin
    Frame := TFrame(AOwner);
    FCommonData.FOwnerControl := TControl(AOwner);
  end
  else begin
    Frame := nil;
    ShowError(LoadStr(s_FrameAdapterError1));
  end;

  if Frame <> nil then begin
    OldWndProc := Frame.WindowProc;
    Frame.WindowProc := NewWndProc;
  end;
end;

destructor TsFrameAdapter.Destroy;
begin
  if ListSW <> nil then FreeAndNil(ListSW);
  if (Frame <> nil) and Assigned(OldWndProc) then Frame.WindowProc := OldWndProc;
  Frame := nil;
  FreeAndNil(FCommonData);
  inherited Destroy;
end;

procedure TsFrameAdapter.Loaded;
var
  i : integer;
begin
  inherited Loaded;
  if Assigned(Frame) and GetBoolMsg(Frame, AC_CTRLHANDLED) and Assigned(SkinData) and Assigned(SkinData.SkinManager) then begin
    SkinData.UpdateIndexes;
    if not SkinData.SkinManager.SkinData.Active or (csDesigning in ComponentState) then Exit;
    if (csDesigning in Frame.ComponentState) and // Updating of form color in design-time
      (Frame.Parent.ClassName = sWinControlForm) and FCommonData.SkinManager.IsValidSkinIndex(FCommonData.SkinManager.ConstData.IndexGlobalInfo)
         then TsHackedControl(Frame.Parent).Color := SkinData.SkinManager.gd[FCommonData.SkinManager.ConstData.IndexGlobalInfo].Color;
    // Popups initialization
    for i := 0 to Frame.ComponentCount - 1 do begin
      if (Frame.Components[i] is TPopupMenu) and SkinData.SkinManager.SkinnedPopups then SkinData.SkinManager.SkinableMenus.HookPopupMenu(TPopupMenu(Frame.Components[i]), True);
    end;
    if SkinData.Skinned and (srThirdParty in SkinData.SkinManager.SkinningRules) then AddToAdapter(Frame);
  end;
end;

procedure TsFrameAdapter.NewWndProc(var Message: TMessage);
var
  i : integer;
  m : TMessage;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  if csDesigning in ComponentState then begin
    OldWndProc(Message);
    Exit;
  end;
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_CTRLHANDLED : begin Message.Result := 1; Exit end;
    AC_GETAPPLICATION : begin Message.Result := longint(Application); Exit end;
    AC_SETNEWSKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      AlphaBroadCast(Frame, Message);
      CommonWndProc(Message, FCommonData);
      if not Assigned(SkinData.SkinManager) then Exit;
      for i := 0 to Frame.ComponentCount - 1 do
        if (Frame.Components[i] is TPopupMenu) and SkinData.SkinManager.SkinnedPopups
          then SkinData.SkinManager.SkinableMenus.HookPopupMenu(TPopupMenu(Frame.Components[i]), True);
      if (csDesigning in Frame.ComponentState) and // Updating of form color in design-time
           (Frame.Parent.ClassName = sWinControlForm) and (FCommonData.SkinManager.ConstData.IndexGlobalInfo > -1)
             then TsHackedControl(Frame.Parent).Color := SkinData.SkinManager.gd[FCommonData.SkinManager.ConstData.IndexGlobalInfo].Color;
      exit
    end;
    AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FCommonData);
      FCommonData.UpdateIndexes;
      if not Assigned(FCommonData.SkinManager) then Exit;
      if (csDesigning in Frame.ComponentState) and // Updating of form color in design-time
           (Frame.Parent.ClassName = sWinControlForm) and (FCommonData.SkinManager.ConstData.IndexGlobalInfo > -1)
             then TsHackedControl(Frame.Parent).Color := SkinData.SkinManager.gd[FCommonData.SkinManager.ConstData.IndexGlobalInfo].Color;
      AlphaBroadcast(Frame, Message);
      RedrawWindow(Frame.Handle, nil, 0, RDW_ERASE or RDW_UPDATENOW or RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_FRAME);
      RefreshScrolls(SkinData, ListSW);
      exit
    end;
    AC_STOPFADING : AlphaBroadcast(Frame, Message);
    AC_BEFORESCROLL : if GetBoolMsg(Frame, AC_CHILDCHANGED) or FCommonData.RepaintIfMoved then begin
      SendMessage(Frame.Handle, WM_SETREDRAW, 0, 0);
    end;
    AC_AFTERSCROLL : if GetBoolMsg(Frame, AC_CHILDCHANGED) or FCommonData.RepaintIfMoved then begin
      SendMessage(Frame.Handle, WM_SETREDRAW, 1, 0);
      RedrawWindow(Frame.Handle, nil, 0, RDW_UPDATENOW or RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_FRAME);
    end;
    AC_REMOVESKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      if ListSW <> nil then FreeAndNil(ListSW);
      CommonWndProc(Message, FCommonData);
      if (csDesigning in Frame.ComponentState) and // Updating of form color in design-time
           Assigned(Frame.Parent) and
           (Frame.Parent.ClassName = sWinControlForm)
             then TsHackedControl(Frame.Parent).Color := clBtnFace;
      AlphaBroadcast(Frame, Message);
      SetWindowPos(Frame.Handle, 0, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_FRAMECHANGED);
      RedrawWindow(Frame.Handle, nil, 0, RDW_ERASE or RDW_UPDATENOW or RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_FRAME);
      exit
    end;
    AC_GETBG : if (FCommonData <> nil) then begin
      InitBGInfo(FCommonData, PacBGInfo(Message.LParam), 0);
      Exit;
    end;
    AC_GETSKININDEX : begin
      Message.Result := FCommonData.SkinIndex + 1;
      Exit;
    end;
  end;
  if (csDestroying in ComponentState) or (csDestroying in Frame.ComponentState) or not FCommonData.Skinned or not SkinData.SkinManager.SkinData.Active then begin
    OldWndProc(Message);
  end
  else begin
    if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
      AC_SETTRANSBGCHANGED : if BgIsTransparent(SkinData) then FCOmmonData.BGChanged := True;
      AC_CHILDCHANGED : begin
        if (SkinData.SkinIndex < 0) or not Assigned(SkinData.SkinManager)
          then Message.LParam := 0
          else Message.LParam := integer((SkinData.SkinManager.gd[SkinData.SkinIndex].GradientPercent + SkinData.SkinManager.gd[SkinData.SkinIndex].ImagePercent > 0) or SkinData.RepaintIfMoved);
        Message.Result := Message.LParam;
        Exit;
      end;
      AC_PREPARING : begin
        Message.Result := integer((SkinData.CtrlSkinState and ACS_FAST <> ACS_FAST) and ({FCommonData.BGChanged or} FCommonData.FUpdating));
        Exit
      end;
      AC_UPDATING : begin
        FCommonData.Updating := Message.WParamLo = 1;
        for i := 0 to Frame.ControlCount - 1 do Frame.Controls[i].Perform(Message.Msg, Message.WParam, Message.LParam);
        Exit;
      end;
      AC_ENDPARENTUPDATE : begin
        if FCommonData.FUpdating {$IFDEF D2006} and not (csRecreating in Frame.ControlState) and not (csAligning in Frame.ControlState) {$ENDIF} then begin
          FCommonData.Updating := False;
          RedrawWindow(Frame.Handle, nil, 0, RDW_ALLCHILDREN or RDW_INVALIDATE or RDW_UPDATENOW or RDW_ERASE)
        end;
        Exit;
      end;
      AC_GETCONTROLCOLOR : begin
        if SkinData.Skinned then begin
          if not IsCached(SkinData) then begin
            case SkinData.SkinManager.gd[SkinData.Skinindex].Transparency of
              0 : Message.Result := longint(SkinData.SkinManager.gd[SkinData.Skinindex].Color);
              100 : begin if Frame.Parent <> nil
                then begin
                  Message.Result := SendMessage(Frame.Parent.Handle, SM_ALPHACMD, MakeWParam(0, AC_GETCONTROLCOLOR), 0);
                  if Message.Result = clFuchsia {if AlphaMessage not supported} then Message.Result := longint(TsHackedControl(Frame.Parent).Color)
                end
                else Message.Result := longint(ColorToRGB(Frame.Color));
              end
              else begin
                if Frame.Parent <> nil
                  then Message.Result := SendMessage(Frame.Parent.Handle, SM_ALPHACMD, MakeWParam(0, AC_GETCONTROLCOLOR), 0)
                  else Message.Result := longint(ColorToRGB(Frame.Color));
                // Mixing of colors
                C1.C := TColor(Message.Result);
                C2.C := SkinData.SkinManager.gd[SkinData.Skinindex].Color;
                C1.R := IntToByte(((C1.R - C2.R) * SkinData.SkinManager.gd[SkinData.Skinindex].Transparency + C2.R shl 8) shr 8);
                C1.G := IntToByte(((C1.G - C2.G) * SkinData.SkinManager.gd[SkinData.Skinindex].Transparency + C2.G shl 8) shr 8);
                C1.B := IntToByte(((C1.B - C2.B) * SkinData.SkinManager.gd[SkinData.Skinindex].Transparency + C2.B shl 8) shr 8);
                Message.Result := longint(C1.C);
              end
            end;
          end
          else Message.LParam := longint(clFuchsia);
        end
        else if Assigned(Frame) then Message.LParam := longint(ColorToRGB(TsHackedControl(Frame).Color));
      end
      else begin
        CommonWndProc(Message, FCommonData);
        Exit;
      end;
    end
    else case Message.Msg of
      CM_MOUSEENTER : if not (csDesigning in ComponentState) then begin
        OldWndProc(Message);
        for i := 0 to Frame.ControlCount - 1 do begin
          if (Frame.Controls[i] is TsSpeedButton) and (Frame.Controls[i] <> Pointer(Message.LParam)) and TsSpeedButton(Frame.Controls[i]).SkinData.FMouseAbove then begin
            Frame.Controls[i].Perform(CM_MOUSELEAVE, 0, 0) // !!!!
          end;
        end;
        if DefaultManager <> nil then DefaultManager.ActiveControl := Frame.Handle;
      end;
      CM_VISIBLECHANGED : begin
        FCommonData.BGChanged := True;
        OldWndProc(Message);
        if Assigned(SkinData.SkinManager) then SendMessage(Frame.Handle, SM_ALPHACMD, MakeWParam(0, AC_REFRESH), LongWord(SkinData.SkinManager));
      end;
      WM_SIZE, WM_MOVE : begin
        FCommonData.BGChanged := FCommonData.BGChanged or (Message.Msg = WM_SIZE) or FCommonData.RepaintIfMoved;
        if FCommonData.BGChanged then begin
          m := MakeMessage(SM_ALPHACMD, MakeWParam(1, AC_SETBGCHANGED), 0, 0);
          Frame.BroadCast(m);
        end;
        if Message.Msg = WM_SIZE then begin
//          PrepareCache;
          InvalidateRect(Frame.Handle, nil, True);
        end; 
        OldWndProc(Message);
      end;
      WM_PARENTNOTIFY : if ((Message.WParam and $FFFF = WM_CREATE) or (Message.WParam and $FFFF = WM_DESTROY)) and not (csLoading in ComponentState) and not (csCreating in Frame.ControlState) then begin
        OldWndProc(Message);
        UpdateScrolls(ListSW, False);
        if (Message.WParamLo = WM_CREATE) and (srThirdParty in SkinData.SkinManager.SkinningRules) then AddToAdapter(Frame);
      end
      else OldWndProc(Message);
      CM_SHOWINGCHANGED : begin
        OldWndProc(Message);
        RefreshScrolls(SkinData, ListSW);
        if (srThirdParty in SkinData.SkinManager.SkinningRules) then AddToAdapter(Frame);
      end;
      WM_PAINT : begin                    
        if (csDesigning in Frame.ComponentState) or (Frame.Parent = nil) or (csDestroying in ComponentState) then OldWndProc(Message) else begin
          AC_WMPaint(TWMPaint(Message).DC);
          Message.Result := 0;
        end;
      end;
      WM_PRINT : if FCommonData.Skinned then begin
        FCommonData.Updating := False;
        if ControlIsReady(Frame) then begin
          OurPaintHandler(TWMPaint(Message).DC);
          Ac_NCPaint(ListSW, Frame.Handle, Message.wParam, Message.lParam, -1, TWMPaint(Message).DC); // Scrolls painting
//          MoveWindowOrg(DC, 0, 0);
        end;
      end
      else OldWndProc(Message);
      WM_NCPAINT : if csDesigning in Frame.ComponentState then OldWndProc(Message) else begin
        FCommonData.FUpdating := FCommonData.Updating;
        Message.Result := 0;
      end;
      WM_ERASEBKGND : if (csDesigning in Frame.ComponentState) or not Frame.Showing then OldWndProc(Message) else begin
        if not InAnimationProcess then begin
          FCommonData.FUpdating := FCommonData.Updating;
          if not FCommonData.FUpdating then begin
            if not IsCached(FCommonData) or not FCommonData.BGChanged then OurPaintHandler(TWMPaint(Message).DC);
          end;
          Message.Result := 1;
        end;
      end
      else OldWndProc(Message);
    end;
  end;
end;

procedure TsFrameAdapter.OurPaintHandler(aDC : hdc);
var
  Changed : boolean;
  DC, SavedDC : hdc;
  R : TRect;
  i : integer;
  procedure FillBG(aRect : TRect);
  begin
    FillDC(DC, aRect, GetBGColor(SkinData, 0))
  end;
begin
  if (aDC <> 0) and (not InAnimationProcess or (aDC = acPrintDC)) then begin
    DC := aDC;
    try
      if IsCached(FCommonData) then begin
        SavedDC := SaveDC(DC);
        FCommonData.Updating := FCommonData.Updating;
        if not FCommonData.Updating then begin
          FCommonData.BGChanged := FCommonData.BGChanged or FCommonData.HalfVisible;
          if SkinData.RepaintIfMoved and (Frame.Parent <> nil) then begin
            FCommonData.HalfVisible := not (PtInRect(Frame.Parent.ClientRect, Point(Frame.Left + 1, Frame.Top + 1)));
            FCommonData.HalfVisible := FCommonData.HalfVisible or not PtInRect(Frame.Parent.ClientRect, Point(Frame.Left + Frame.Width - 1, Frame.Top + Frame.Height - 1));
          end
          else FCommonData.HalfVisible := False;
          Changed := FCommonData.BGChanged;
          if Changed and not FCommonData.UrgentPainting then PrepareCache;

          CopyWinControlCache(Frame, FCommonData, Rect(0, 0, 0, 0), Rect(0, 0, Frame.Width, Frame.Height), DC, True);

          sVCLUtils.PaintControls(DC, Frame, Changed, Point(0, 0));
          SetParentUpdated(Frame);
        end;
        RestoreDC(DC, SavedDC);
      end
      else begin
        FCommonData.Updating := False;
        i := SkinBorderMaxWidth(FCommonData);
        R := Rect(0, 0, Frame.Width, Frame.Height);
        if aDC <> acPrintDC then DC := GetDC(Frame.Handle);
        try
          SavedDC := SaveDC(DC);
          ExcludeControls(DC, Frame, actGraphic, 0, 0);
          if i = 0 then FillBG(R) { Just fill BG} else begin
            if FCommonData.FCacheBmp = nil then FCommonData.FCacheBmp := CreateBmp24(Frame.Width, Frame.Height);
            PaintBorderFast(DC, R, i, FCommonData, 0);
            InflateRect(R, i, i);
            FillBG(R);
          end;
          RestoreDC(DC, SavedDC);
          if i > 0 then BitBltBorder(DC, 0, 0, Frame.Width, Frame.Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, i);
          PaintControls(DC, Frame, True, Point(0, 0));
          if FCommonData.FCacheBmp <> nil then FreeAndNil(FCommonData.FCacheBmp);
        finally
          if aDC <> acPrintDC then ReleaseDC(Frame.Handle, DC);
        end;
      end;
    finally
    end;
  end;
end;

procedure TsFrameAdapter.PrepareCache;
begin
  if IsCached(SkinData) then begin
    SkinData.InitCacheBmp;
    SkinData.FCacheBmp.Width := Frame.Width;
    SkinData.FCacheBmp.Height := Frame.Height;
    SkinData.FCacheBMP.Canvas.Font.Assign(Frame.Font);
    PaintItem(SkinData, GetParentCache(SkinData), False, 0, Rect(0, 0, Frame.Width, Frame.Height), Point(Frame.Left, Frame.Top), SkinData.FCacheBMP, False);
  end;
  SkinData.BGChanged := False;
end;

end.

