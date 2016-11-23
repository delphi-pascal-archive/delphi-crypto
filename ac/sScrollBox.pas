unit sScrollBox;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  sLabel, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, sCommonData, StdCtrls, acSBUtils;

type
  TsPaintEvent = procedure (ControlBmp : TBitmap) of object;

  TsScrollBox = class(TScrollingWinControl)
  private
{$IFNDEF NOTFORHELP}
    FCommonData : TsCommonData;
    FOnPaint: TsPaintEvent;
    FOnBeforeScroll: TNotifyEvent;
    FOnAfterScroll: TNotifyEvent;
    FCanvas : TControlCanvas;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    function GetCanvas: TCanvas;
    procedure SetBorderStyle(const Value: TBorderStyle);
  protected
    FBorderStyle: TBorderStyle;
    FAutoFrameSize: boolean;
    procedure WMEraseBkGnd(var Message: TWMPaint); message WM_ERASEBKGND;
    procedure WMNCPaint(var Message: TWMPaint); message WM_NCPAINT;
    procedure WMPrint(var Message: TWMPaint); message WM_PRINT;
    procedure SetParent(AParent: TWinControl); override;
    procedure WMNCHitTest(var Message: TMessage); message WM_NCHITTEST;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
  public
    ListSW : TacScrollWnd;
    constructor Create(AOwner: TComponent); override;
    procedure CreateParams(var Params: TCreateParams); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Loaded; override;

    procedure ScrollBy(DeltaX, DeltaY: Integer);

    procedure PrepareCache; virtual;
    procedure Paint(aDC : hdc = 0; SendUpdated : boolean = True); virtual;
    procedure PaintWindow(DC: HDC); override;
    procedure WndProc(var Message: TMessage); override;
{$ENDIF} // NOTFORHELP
  published
    {:@event}
    property OnPaint : TsPaintEvent read FOnPaint write FOnPaint;
{$IFNDEF NOTFORHELP}
    {:@event}
    property OnAfterScroll : TNotifyEvent read FOnAfterScroll write FOnAfterScroll;
    {:@event}
    property OnBeforeScroll : TNotifyEvent read FOnBeforeScroll write FOnBeforeScroll;
    property OnMouseEnter : TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave : TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property Align;
    property Anchors;
    property AutoScroll default True;
    property BiDiMode;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Canvas : TCanvas read GetCanvas;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Color;
    property Ctl3D;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
{$ENDIF} // NOTFORHELP
    property SkinData : TsCommonData read FCommonData write FCommonData;
  end;

{$IFNDEF NOTFORHELP}
procedure SkinScrollInView(AControl: TControl; ScrollBox : TsScrollBox); // For compatibility
{$ENDIF} // NOTFORHELP

implementation

uses sGraphUtils{$IFDEF CHECKXP}, UxTheme, Themes{$ENDIF}, sConst, sMaskData, sVCLUtils, acntUtils, sStyleSimply, math,
  sMessages{$IFDEF LOGGED}, sDebugMsgs{$ENDIF}, sAlphaGraph, sSkinManager, FlatSB, sBorders;

procedure SkinScrollInView(AControl: TControl; ScrollBox : TsScrollBox);
begin
  ScrollBox.ScrollInView(AControl);
end;

{ TsScrollBox }

procedure TsScrollBox.AfterConstruction;
begin
  inherited AfterConstruction;
  FCommonData.Loaded;
end;

procedure TsScrollBox.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then RecreateWnd;
  inherited;
end;

constructor TsScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoFrameSize := False;
  FCommonData := TsCommonData.Create(Self, True);
  FCommonData.COC := COC_TsScrollBox;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  Width := 185;
  Height := 41;
  AutoScroll := True;

  FBorderStyle := bsSingle;
end;

procedure TsScrollBox.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do begin
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then  begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

destructor TsScrollBox.Destroy;
begin
  if ListSW <> nil then FreeAndNil(ListSW);
  if Assigned(FCommonData) then FreeAndNil(FCommonData);
  if Assigned(FCanvas) then FreeAndNil(FCanvas);
  inherited Destroy;
end;

function TsScrollBox.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

procedure TsScrollBox.Loaded;
begin
  inherited Loaded;
  FCommonData.Loaded;
  if not FCommonData.Skinned then Exit;
end;

procedure TsScrollBox.Paint(aDC : hdc = 0; SendUpdated : boolean = True);
var
  DC, SavedDC : hdc;
  R : TRect;
  bWidth, i : integer;
  ParentBG : TacBGInfo;
begin
  bWidth := BorderWidth + 2 * integer(BorderStyle = bsSingle);
  if aDC <> 0 then DC := aDC else DC := GetDC(Handle);
  if IsCached(FCommonData) and not SkinData.CustomColor or Assigned(FOnPaint) then begin
    FCommonData.Updating := FCommonData.Updating;
    if not FCommonData.Updating then begin
        // If transparent and parent is resized
        b := FCommonData.HalfVisible or FCommonData.BGChanged;

        if SkinData.RepaintIfMoved then begin
          GetClipBox(DC, R);
          FCommonData.HalfVisible := (WidthOf(R) <> Width) or (HeightOf(R) <> Height)
        end
        else FCommonData.HalfVisible := False;

        if b then PrepareCache;
        UpdateCorners(FCommonData, 0);
        CopyWinControlCache(Self, FCommonData, Rect(bWidth, bWidth, 0, 0), Rect(0, 0, Width - 2 * bWidth, Height - 2 * bWidth), DC, False);

        sVCLUtils.PaintControls(DC, Self, b and SkinData.RepaintIfMoved, Point(0, 0));
        MoveWindowOrg(DC, bWidth, bWidth);
    end;
  end
  else begin
    FCommonData.Updating := False;
    i := SkinBorderMaxWidth(FCommonData);
    R := Rect(0, 0, Width, Height);
    SavedDC := SaveDC(DC);
    ExcludeControls(DC, Self, actGraphic, 0, 0);
    MoveWindowOrg(DC, -bWidth, -bWidth);
    ParentBG.PleaseDraw := False;
    if not SkinData.CustomColor then GetBGInfo(@ParentBG, Self) else begin
      ParentBG.Color := ColorToRGB(Color);
      ParentBG.BgType := btFill;
    end;
    if (FCommonData.SkinManager.gd[FCommonData.SkinIndex].Transparency = 100) and (ParentBG.BgType = btCache) then begin
      FCommonData.Updating := FCommonData.Updating;
      if not FCommonData.Updating then begin
        if i = 0 then begin
          BitBlt(DC, 0, 0, Width, Height, ParentBG.Bmp.Canvas.Handle, ParentBG.Offset.X, ParentBG.Offset.Y, SRCCOPY);
        end
        else begin
          if FCommonData.FCacheBmp = nil then FCommonData.FCacheBmp := CreateBmp24(Width, Height);
          R := PaintBorderFast(DC, R, i, FCommonData, 0);
          if FCommonData.SkinManager.gd[FCommonData.SkinIndex].Transparency = 100
            then FillDC(DC, R, ParentBG.Color)
            else FillDC(DC, R, GetBGColor(SkinData, 0));
          if i > 0 then BitBltBorder(DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, i);
          BitBlt(DC, i, i, Width - 2 * i, Height - 2 * i, ParentBG.Bmp.Canvas.Handle, ParentBG.Offset.X + i, ParentBG.Offset.Y + i, SRCCOPY);
        end;
      end;
    end
    else begin
      if i = 0 then FillDC(DC, R, ParentBG.Color) else begin
        if FCommonData.FCacheBmp = nil then FCommonData.FCacheBmp := CreateBmp24(Width, Height);
        R := PaintBorderFast(DC, R, i, FCommonData, 0);
        if FCommonData.SkinManager.gd[FCommonData.SkinIndex].Transparency = 100
          then FillDC(DC, R, ParentBG.Color)
          else FillDC(DC, R, GetBGColor(SkinData, 0));
        if i > 0 then BitBltBorder(DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, i);
      end;
    end;
    R := ClientRect;
    i := BorderWidth + integer(BevelInner <> bvNone) * BevelWidth + integer(BevelOuter <> bvNone) * BevelWidth;
    InflateRect(R, -i, -i);

    RestoreDC(DC, SavedDC);
    sVCLUtils.PaintControls(DC, Self, True, Point(0, 0));
    if FCommonData.FCacheBmp <> nil then FreeAndNil(FCommonData.FCacheBmp);
  end;
  if aDC = 0 then ReleaseDC(Handle, DC);
  SetParentUpdated(Self);
end;

procedure TsScrollBox.PrepareCache;
begin
  FCommonData.InitCacheBmp;
  PaintSkinControl(FCommonData, Parent, False, 0, Rect(0, 0, width, Height), Point(Left, Top), FCommonData.FCacheBMP, True);
  SkinData.BGChanged := False;
  if Assigned(OnPaint) then OnPaint(FCommonData.FCacheBmp);
end;

procedure TsScrollBox.ScrollBy(DeltaX, DeltaY: Integer);
begin
  SendAMessage(Handle, AC_BEFORESCROLL);
  inherited ScrollBy(DeltaX, DeltaY);
  SendAMessage(Handle, AC_AFTERSCROLL);
end;

procedure TsScrollBox.SetBorderStyle(const Value: TBorderStyle);
begin
  if Value <> FBorderStyle then begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TsScrollBox.SetParent(AParent: TWinControl);
begin
  inherited;
  if (Parent = nil) then Exit;
  FCommonData.Loaded;
end;

procedure TsScrollBox.WMNCHitTest(var Message: TMessage);
begin
  DefaultHandler(Message);
end;

procedure TsScrollBox.WMNCPaint(var Message: TWMPaint);
var
  DC, SavedDC : hdc;
  bWidth : integer;
begin
  if FCommonData.Skinned and (BorderStyle <> bsNone) and Visible then begin
    if InAnimationProcess then Exit;
    
    if csDesigning in ComponentState then inherited;
    if IsCached(FCommonData) then begin
      FCommonData.Updating := FCommonData.Updating;

      if ControlIsReady(Self) and not FCommonData.Updating then begin
        if SkinData.BGChanged then PrepareCache;
        UpdateCorners(FCommonData, 0);

        bWidth := 2 * integer(BorderStyle = bsSingle) + BorderWidth;
        DC := GetWindowDC(Handle);
        SavedDC := SaveDC(DC);

        BitBltBorder(DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, bWidth);
        if Assigned(ListSW) and Assigned(ListSW.sBarVert) then Ac_NCPaint(ListSW, Handle, 1, 0, -1, DC);
        RestoreDC(DC, SavedDC);
        ReleaseDC(Handle, DC);
      end;
    end
    else begin
      DC := GetWindowDC(Handle);
      SavedDC := SaveDC(DC);
      PaintBorderFast(DC, Rect(0, 0, Width, Height), 2 * integer(BorderStyle = bsSingle) + BorderWidth, SkinData, 0);
      if FCommonData.FCacheBmp <> nil then FreeAndNil(FCommonData.FCacheBmp);
      RestoreDC(DC, SavedDC);
      ReleaseDC(Handle, DC);
    end;
  end else inherited;
end;

procedure TsScrollBox.WMEraseBkGnd(var Message: TWMPaint);
begin
  if SkinData.Skinned then begin
    if InAnimationProcess and (Message.DC <> acPrintDC) then Exit; // Prevent of BG drawing in Aero
    if (FCommonData.SkinManager.gd[FCommonData.SkinIndex].Transparency = 0) or
         (SendAMessage(SkinData.FOwnerControl.Parent, AC_GETSKINSTATE) and ACS_FAST = ACS_FAST) then Paint(Message.DC);
  end
  else inherited;
end;

procedure TsScrollBox.WMPrint;
var
  bWidth : integer;
  cR : TRect;
begin
  if FCommonData.Skinned then begin
    FCommonData.Updating := False;
    if ControlIsReady(Self) then begin
      PrepareCache;
      if Assigned(OnPaint) then OnPaint(FCommonData.FCacheBmp);
      UpdateCorners(FCommonData, 0);
      bWidth := BorderWidth + 2 * integer(BorderStyle = bsSingle);

      BitBltBorder(Message.DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, bWidth);
      Ac_NCPaint(ListSW, Handle, longint(Message.DC), 0, -1, Message.DC);

      MoveWindowOrg(Message.DC, bWidth, bWidth);

      cR := GetClientRect;
      IntersectClipRect(Message.DC, 0, 0, WidthOf(cR), HeightOf(cR));

      FCommonData.HalfVisible := False;
      Paint(TWMPaint(Message).DC);
      MoveWindowOrg(Message.DC, -bWidth, -bWidth);
    end;
  end
  else inherited;
end;

procedure TsScrollBox.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_CTRLHANDLED : begin Message.Result := 1; Exit end; // AlphaSkins supported
    AC_GETAPPLICATION : begin Message.Result := longint(Application); Exit end;
    AC_SETNEWSKIN : begin
      if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
        AlphaBroadCast(Self, Message);
        CommonWndProc(Message, FCommonData);
      end
      else AlphaBroadCast(Self, Message);
      exit
    end;
    AC_REFRESH : begin
      if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
        CommonWndProc(Message, FCommonData);
        SkinData.Updating := SkinData.Updating;
        if not SkinData.Updating and not InAnimationProcess then begin
          RedrawWindow(Handle, nil, 0, RDW_FRAME + RDW_INVALIDATE + RDW_UPDATENOW + RDW_ERASE);
        end
        else RedrawWindow(Handle, nil, 0, RDW_FRAME + RDW_INVALIDATE + RDW_ERASE);
        RefreshScrolls(SkinData, ListSW);
      end;
      AlphaBroadCast(Self, Message);
      exit
    end;
    AC_REMOVESKIN : begin
      if LongWord(Message.LParam) = LongWord(SkinData.SkinManager) then begin
        if ListSW <> nil then begin
          FreeAndNil(ListSW);
          InitializeFlatSB(Handle);
        end;
        CommonWndProc(Message, FCommonData);
        if not (csDestroying in ComponentState) then begin
          FCommonData.BorderIndex := -1;
          FCommonData.SkinIndex := -1;
          ControlStyle := ControlStyle - [csOpaque];
          RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_FRAME);
        end;
      end;
      AlphaBroadCast(Self, Message);
      exit
    end;
    AC_INVALIDATE : begin
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_FRAME);
    end;
    AC_BEFORESCROLL : begin
      if Assigned(FOnBeforeScroll) then FOnBeforeScroll(Self);
      if (Message.LParamHi = WM_HSCROLL) and not HorzScrollBar.Tracking or (Message.LParamHi = WM_VSCROLL) and not VertScrollBar.Tracking then begin
      end
      else begin
        SendMessage(Handle, WM_SETREDRAW, 0, 0);
      end;
    end;
    AC_AFTERSCROLL : begin
      if (Message.LParamHi = WM_HSCROLL) and not HorzScrollBar.Tracking or (Message.LParamHi = WM_VSCROLL) and not VertScrollBar.Tracking then begin
        RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_UPDATENOW);
      end
      else begin
        SendMessage(Handle, WM_SETREDRAW, 1, 0);
        RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_FRAME or RDW_UPDATENOW);
      end;
      if Assigned(FOnAfterScroll) then FOnAfterScroll(Self);
    end;
    AC_ENDPARENTUPDATE : if FCommonData.Updating {IsNT or (not IsNT and FCommonData.Updating)} {v4.83 for Win9x} then begin
      RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE);// or RDW_ERASE);
      FCommonData.Updating := False;
      Exit;
    end
    else begin
      CommonMessage(Message, FCommonData);
      Exit;
    end;
  end;
  case Message.Msg of
    CM_MOUSEENTER : begin
      if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
    end;
    CM_MOUSELEAVE : begin
      if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
    end;
  end;
  if not ControlIsReady(Self) then begin
    inherited
  end
  else begin
    if FCommonData.Skinned then begin
      if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
        AC_PREPARING : begin
          Message.Result := integer(IsCached(FCommonData) and (FCommonData.FUpdating));
          Exit;
        end;
        AC_GETBG : begin
          if FCommonData.FCacheBmp = nil
            then Message.Result := 0;
          InitBGInfo(FCommonData, PacBGInfo(Message.LParam), 0);
          if (PacBGInfo(Message.LParam)^.BgType = btCache) and not PacBGInfo(Message.LParam)^.PleaseDraw then begin
            PacBGInfo(Message.LParam)^.Offset.X := PacBGInfo(Message.LParam)^.Offset.X + 2 * integer(BorderStyle = bsSingle) + BorderWidth;
            PacBGInfo(Message.LParam)^.Offset.Y := PacBGInfo(Message.LParam)^.Offset.Y + 2 * integer(BorderStyle = bsSingle) + BorderWidth;
          end;
          Exit;
        end
        else begin
          CommonMessage(Message, FCommonData);
          Exit;
        end;
      end
      else case Message.Msg of
        CM_VISIBLECHANGED : FCommonData.BGChanged := True;
        CM_ENTER, CM_EXIT : begin
          FCommonData.BeginUpdate;
          inherited;
          FCommonData.EndUpdate;
          Exit;
        end;
        WM_PAINT : begin
          if IsCached(FCommonData) then begin
            acGlobalDC := BeginPaint(Handle, acGlobalPS);
            Paint(TWMPaint(Message).DC);
            Message.LParam := 1;
            EndPaint(Handle, acGlobalPS);
          end
          else begin
            InvalidateRect(Handle, nil, True); // v6.10
            acGlobalDC := BeginPaint(Handle, acGlobalPS);
            sVclUtils.PaintControls(acGlobalDC, Self, True, Point(0, 0));
            EndPaint(Handle, acGlobalPS);
          end;
          Exit;
        end;
      end;
    end;
    CommonWndProc(Message, FCommonData);
    inherited;
    if FCommonData.Skinned then case Message.Msg of
      WM_SIZE : begin
        RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_FRAME or RDW_ERASE);
      end;
      CM_FOCUSCHANGED : UpdateScrolls(ListSW, True);
      CM_SHOWINGCHANGED : RefreshScrolls(SkinData, ListSW);
      WM_PARENTNOTIFY: if (Message.WParam and $FFFF = WM_CREATE) or (Message.WParam and $FFFF = WM_DESTROY) then begin
        if AutoScroll then UpdateScrolls(ListSW, True);
      end;
      WM_MOUSEWHEEL, WM_WINDOWPOSCHANGING, CM_CONTROLLISTCHANGE, CM_CONTROLCHANGE : if not SkinData.Updating then begin
        if AutoScroll then UpdateScrolls(ListSW, True);
      end;
    end;
  end;
end;

procedure TsScrollBox.PaintWindow(DC: HDC);
begin
//  Empty for fixing of Borland bug
end;

end.
