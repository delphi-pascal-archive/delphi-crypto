unit sListView;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, sConst, ComCtrls, {$IFNDEF DELPHI5}types,{$ENDIF}
  Commctrl, sCommonData, sMessages, acSBUtils{$IFDEF LOGGED}, sDebugMsgs{$ENDIF}
  {$IFDEF TNTUNICODE}, TntComCtrls{$ENDIF};

{$I sDefs.inc}

type
{$IFDEF TNTUNICODE}
  TsCustomListView = class(TTntCustomListView)
{$ELSE}
  TsCustomListView = class(TCustomListView)
{$ENDIF}
{$IFNDEF NOTFORHELP}
  private
    Loading          : boolean;
    FhWndHeader      : HWnd;
    FhHeaderProc     : Pointer;
    FhDefHeaderProc  : Pointer;
    FPressedColumn   : Integer;
    FCommonData: TsCommonData;
    HoverColIndex : integer;
    FBoundLabel: TsBoundLabel;
    FHighlightHeaders: boolean;
    FOldAdvancedCustomDraw: TLVAdvancedCustomDrawEvent;
    FFlag: Boolean;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure WMHitTest(var Message: TMessage); message WM_NCHITTEST;
    procedure WMParentNotify(var Message: TWMParentNotify); message WM_PARENTNOTIFY;
    procedure NewAdvancedCustomDraw(Sender: TCustomListView; const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);

    procedure PrepareCache;
    function GetHeaderColumnRect(Index: Integer): TRect;
    procedure ColumnSkinPaint(ControlRect : TRect; cIndex : Integer);
    procedure PaintHeader;
  protected
    ListSW : TacScrollWnd;
    procedure WndProc (var Message: TMessage); override;
    procedure HeaderWndProc(var Message: TMessage);
    function AllColWidth : integer;
    function FullRepaint : boolean;
    property BorderStyle;
    procedure InvalidateSmooth(Always : boolean);
  public
    ListLineHeight : Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AfterConstruction; override;
    procedure Loaded; override;
  published
{$ENDIF} // NOTFORHELP
{$IFDEF D2009}
    property Action;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelEdges;
    property DoubleBuffered;
    property Groups;
    property GroupView default False;
    property GroupHeaderImages;
{$ENDIF}
    property BoundLabel : TsBoundLabel read FBoundLabel write FBoundLabel;
    property SkinData : TsCommonData read FCommonData write FCommonData;
    property HighlightHeaders : boolean read FHighlightHeaders write FHighlightHeaders default True;
  end;

  TsListView = class(TsCustomListView)
{$IFNDEF NOTFORHELP}
  published
    property Align;
    property AllocBy;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Checkboxes;
    property Color;
    property Columns;
    property ColumnClick;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FlatScrollBars;
    property FullDrag;
    property GridLines;
    property HideSelection;
    property HotTrack;
    property HotTrackStyles;
    property HoverTime;
    property IconOptions;
    property Items;
    property LargeImages;
    property MultiSelect;
    property OwnerData;
    property OwnerDraw;
    property ReadOnly default False;
    property RowSelect;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowColumnHeaders;
    property ShowWorkAreas;
    property ShowHint;
    property SmallImages;
    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property ViewStyle;
    property Visible;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnColumnClick;
    property OnColumnDragged;
    property OnColumnRightClick;
    property OnCompare;
    property OnContextPopup;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnCustomDrawSubItem;
    property OnData;
    property OnDataFind;
    property OnDataHint;
    property OnDataStateChange;
    property OnDblClick;
    property OnDeletion;
    property OnDrawItem;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSubItemImage;
    property OnDragDrop;
    property OnDragOver;
    property OnInfoTip;
    property OnInsert;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnSelectItem;
    property OnStartDock;
    property OnStartDrag;
    property BoundLabel;
    property SkinData;
{$ENDIF} // NOTFORHELP
  end;

{$IFNDEF NOTFORHELP}
  TsHackedListItems = class({$IFDEF TNTUNICODE}TTntListItems{$ELSE}TListItems{$ENDIF})
  public
    FNoRedraw: Boolean;
  end;
{$ENDIF} // NOTFORHELP

implementation

uses sStyleSimply, acntUtils, sVclUtils, sMaskData, sGraphUtils, sSkinProps,
  sAlphaGraph, sSkinManager, math;

var
  LocalMsg : TMessage;
  LocalFlag : boolean;

constructor TsCustomListView.Create(AOwner: TComponent);
begin
  FhWndHeader     := 0;
  FhDefHeaderProc := nil;
  FPressedColumn  := -1;
  Loading := True;

  inherited Create(AOwner);
  FCommonData := TsCommonData.Create(Self, True);
  FCommonData.COC := COC_TsListView;
//  FCommonData.Updating := False;
  SkinData.BGChanged := True;
  FBoundLabel := TsBoundLabel.Create(Self, FCommonData);
  ListLineHeight := Font.Size;
  FHighlightHeaders := True;
  HoverColIndex := -2;

  if Assigned(OnAdvancedCustomDraw) then FOldAdvancedCustomDraw := OnAdvancedCustomDraw else FOldAdvancedCustomDraw := nil;
  OnAdvancedCustomDraw := NewAdvancedCustomDraw;
  try
    FhHeaderProc := MakeObjectInstance(HeaderWndProc);
  except
    Application.HandleException(Self);
  end;
end;

destructor TsCustomListView.Destroy;
begin
  if ListSW <> nil then FreeAndNil(ListSW);
  SmallImages := nil;
  LargeImages := nil;
  if FhWndHeader <> 0 then begin
    SetWindowLong(FhWndHeader, GWL_WNDPROC, LongInt(FhDefHeaderProc));
  end;
  if FhHeaderProc <> nil then begin
    FreeObjectInstance(FhHeaderProc);
  end;
  FreeAndNil(FBoundLabel);
  if Assigned(FCommonData) then FreeAndNil(FCommonData);
  OnAdvancedCustomDraw := FOldAdvancedCustomDraw;
  inherited Destroy;
end;

procedure TsCustomListView.AfterConstruction;
begin
  Loading := True;
  inherited AfterConstruction;
  try
    FCommonData.Loaded;
  except
    Application.HandleException(Self);
  end;
end;

procedure TsCustomListView.WndProc(var Message: TMessage);
var
  R : TRect;
  SavedDC : hdc;
  DstPos, Delta : integer;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_CTRLHANDLED : begin Message.Result := 1; Exit end; // AlphaSkins supported
    AC_GETAPPLICATION : begin Message.Result := longint(Application); Exit end;
    AC_REMOVESKIN : if LongWord(Message.LParam) = LongWord(SkinData.SkinManager) then begin
      Items.BeginUpdate;
      CommonWndProc(Message, FCommonData);
      if not FCommonData.CustomColor then Color := clWindow;
      if not FCommonData.CustomFont then Font.Color := clWindowText;
      if ListSW <> nil then FreeAndNil(ListSW);
      Items.EndUpdate;
      RedrawWindow(Handle, nil, 0, RDW_FRAME + RDW_INVALIDATE + RDW_UPDATENOW);
      Exit
    end;
    AC_SETNEWSKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FCommonData);
      exit
    end;
    AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      Items.BeginUpdate;
      CommonWndProc(Message, FCommonData);
      if FCommonData.Skinned and not Loading {v4.66} then begin
        if not FCommonData.CustomColor then Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].Color;
        if not FCommonData.CustomFont then Font.Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].FontColor[1];
        if HandleAllocated then UninitializeFlatSB(Handle); // v5.05
        RefreshEditScrolls(SkinData, ListSW);
        RedrawWindow(Handle, nil, 0, RDW_FRAME + RDW_INVALIDATE + RDW_UPDATENOW);
        HeaderWndProc(LocalMsg);
      end;
      Items.EndUpdate;
      Exit;
    end;
    AC_ENDPARENTUPDATE : begin
      acPrintDC := 0;
      PaintHeader;
      Exit
    end;
    AC_PREPARING       : begin
      Message.Result := integer(SkinData.FUpdating);
      Exit;
    end;
    AC_BEFORESCROLL : begin
      Message.Result := 1;
      Exit;
    end;
  end;
  if (csCreating in ControlState) or (FCommonData = nil) or not FCommonData.Skinned then inherited else begin // <- csLoading state is damaged (enabled always)???
    case Message.Msg of
      LVM_SETCOLUMN, LVM_INSERTCOLUMN : with PLVColumn(Message.LParam)^ do begin
        if iImage = - 1 then Mask := Mask and not LVCF_IMAGE;
      end;
      WM_PRINT : begin
        inherited;
        if (ViewStyle = vsReport) and (ListSW <> nil) then begin
          SavedDC := SaveDC(TWMPaint(Message).DC);
          MoveWindowOrg(TWMPaint(Message).DC, ListSW.cxLeftEdge, ListSW.cxLeftEdge);
          IntersectClipRect(TWMPaint(Message).DC, 0, 0,
                            SkinData.FCacheBmp.Width - 2 * ListSW.cxLeftEdge - integer(ListSW.sBarVert.fScrollVisible) * GetScrollMetric(ListSW.sBarVert, SM_CXVERTSB),
                            SkinData.FCacheBmp.Height - 2 * ListSW.cxLeftEdge - integer(ListSW.sBarHorz.fScrollVisible) * GetScrollMetric(ListSW.sBarHorz, SM_CYHORZSB));
          acPrintDC := TWMPaint(Message).DC;
          HeaderWndProc(Message);
          RestoreDC(TWMPaint(Message).DC, SavedDC);
        end;
        Exit;
      end;
      WM_SETFOCUS, WM_KILLFOCUS : begin
        SendMessage(Handle, WM_SETREDRAW, 0, 0);
        inherited;
        SendMessage(Handle, WM_SETREDRAW, 1, 0);
        if Selected <> nil then ListView_RedrawItems(Handle, Selected.Index, Selected.Index);
        Exit;
      end;
      WM_ERASEBKGND : if (acPrintDC <> 0) then begin
        TWMPaint(Message).DC := acPrintDC;
        inherited;
      end
      else if FCommonData.Updating then Exit;
      WM_VSCROLL : begin
        if Message.WParamLo = SB_THUMBTRACK then begin
          if Message.LParam <> 0 then DstPos := Message.LParam else DstPos := Message.WParamHi;
          if nLastSBPos <> DstPos then begin // If CurPos is changed
            Delta := DstPos - nLastSBPos;
            if ViewStyle = vsReport then begin
              ListView_GetItemRect(Handle, 0, R, LVIR_BOUNDS);
              Delta := Delta * HeightOf(R);
            end;
            ListView_Scroll(Handle, 0, Delta);
          end
        end
        else begin
          Message.LParam := 0;
          inherited;
        end;
        Exit;
      end;
      WM_HSCROLL : case Message.WParamLo of
        SB_THUMBTRACK : begin
          if Message.LParam <> 0 then DstPos := Message.LParam else DstPos := Message.WParamHi;
          Delta := DstPos - nLastSBPos;
          if ViewStyle = vsList then begin
            ListView_GetItemRect(Handle, 0, R, LVIR_BOUNDS);
            Delta := Delta * WidthOf(R);
          end;
          ListView_Scroll(Handle, Delta, 0);
          InvalidateSmooth(False);
          PaintHeader;
          Exit;
        end;
        SB_LINELEFT, SB_LINERIGHT : begin
          inherited;
          InvalidateSmooth(False);
          Exit;
        end;
      end;
    end;
    CommonWndProc(Message, FCommonData);
    inherited;
    if FCommonData.Skinned then case Message.Msg of
      CM_MOUSEWHEEL, WM_MOUSEWHEEL : if (TWMMouseWheel(Message).Keys = 0) then begin
        InvalidateSmooth(False);
      end;
      CN_KEYDOWN, CN_KEYUP : case TWMKey(Message).CharCode of VK_PRIOR..VK_DOWN : InvalidateSmooth(False) end;
      CM_SHOWINGCHANGED : begin
        if HandleAllocated then UninitializeFlatSB(Handle); // v5.05
        RefreshEditScrolls(SkinData, ListSW);
      end;
      WM_STYLECHANGED : if not (csReadingState in ControlState) then begin
        ListView_Scroll(Handle, 0, 0);
        UpdateScrolls(ListSW, True);
      end;
      LVM_DELETEITEM, LVM_REDRAWITEMS,
      LVM_INSERTITEMA : if not FCommonData.Updating then UpdateScrolls(ListSW, True);
      WM_NCPAINT: begin
        PaintHeader;
      end;
      CM_VISIBLECHANGED, CM_ENABLEDCHANGED, WM_MOVE, WM_SIZE, WM_WINDOWPOSCHANGED : if FCommonData.Skinned and not (csDestroying in ComponentState) then begin
        Perform(WM_NCPAINT, 0, 0);
        LocalFlag := True;
        InvalidateSmooth(True);
        LocalFlag := False;
        case Message.Msg of
          WM_MOVE, WM_SIZE : begin
            if FullRepaint then SendMessage(Handle, WM_NCPAINT, 0, 0) // Scrollbars repainting if transparent
          end;
        end;
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

procedure TsCustomListView.Loaded;
begin
  Loading := True;
  inherited Loaded;
  try
    FCommonData.Loaded;
  except
    Application.HandleException(Self);
  end;
  if FCommonData.Skinned then begin
    if not FCommonData.CustomColor then Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].Color;
    if not FCommonData.CustomFont then Font.Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].FontColor[1];
  end;
  Loading := False;
end;

procedure TsCustomListView.CMMouseLeave(var Msg: TMessage);
var
  p : TPoint;
  r : TRect;
begin
  if FCommonData.Skinned and (ViewStyle = vsReport) then begin
    p := ClientToScreen(Point(Left, Top));
    r := Rect(p.x, p.y, p.x + Width, p.y + Height);

    if not PtInRect(r, acMousePos) then inherited;

    if (HoverColIndex >= 0) then begin
      HoverColIndex := -2;
      PaintHeader;
    end;
  end;
  inherited;
end;

procedure TsCustomListView.HeaderWndProc(var Message: TMessage);
var
  Info : THDHitTestInfo;
  CurIndex, w : integer;
  function MouseToColIndex(p : TSmallPoint) : integer;
  var
    ltPoint : TPoint;
    i, c : integer;
    rc : TRect;
  begin
    w := 0;
    if Assigned(ListSW.sBarHorz) then w := ListSW.sBarHorz.ScrollInfo.nPos else w := 0;
    ltPoint := ScreenToClient(Point(p.x + w, p.y));
    Result := -2;
    c := (Header_GetItemCount(FhWndHeader) - 1);
    for i := 0 to c do begin
      rc := GetHeaderColumnRect(i);
      if PtInRect(rc, ltPoint) then begin
        Result := i;
        exit;
      end;
    end;
  end;
begin
  if (ViewStyle = vsReport) and Assigned(FCommonData) and FCommonData.Skinned then begin
    try
      with Message do begin
        case Msg of
          WM_NCHITTEST : if ColumnClick then begin
            Result := CallWindowProc(FhDefHeaderProc, FhWndHeader, Msg, WParam, LParam);
            if FCommonData.Skinned and FHighLightHeaders then begin
              CurIndex := MouseToColIndex(TWMNCHitTest(Message).Pos);
              if HoverColIndex <> CurIndex then begin
                HoverColIndex := CurIndex;
                PaintHeader;
              end;
            end;
          end;
          WM_LBUTTONUP: if ColumnClick then begin
            FPressedColumn := -1;
            FFlag := False;
          end;
          WM_PRINT : begin
            PaintHeader
          end;
          WM_PAINT: if FCommonData.Skinned then begin
            PaintHeader;
            Exit;
          end;
          WM_ERASEBKGND: Exit;
          WM_NCDESTROY: begin
            Result := CallWindowProc(FhDefHeaderProc, FhWndHeader, Msg, WParam, LParam);
            FhWndHeader := 0;
            FhDefHeaderProc := nil;
            Exit;
          end;
        end;
        Result := CallWindowProc(FhDefHeaderProc, FhWndHeader, Msg, WParam, LParam);
        case Msg of
          WM_LBUTTONDOWN: if ColumnClick then begin
            FFlag := True;
            Info.Point.X := TWMMouse(Message).XPos;
            Info.Point.Y := TWMMouse(Message).YPos;
            SendMessage(FhWndHeader, HDM_HITTEST, 0, Integer(@Info));

            if (Info.Flags and HHT_ONDIVIDER = 0) and (Info.Flags and HHT_ONDIVOPEN = 0) then begin
              FPressedColumn := {a[}Info.Item//] else FPressedColumn := -1;
            end
            else FPressedColumn := -1;
            RedrawWindow(FhWndHeader, nil, 0, RDW_INVALIDATE);
          end;
          WM_MOUSEMOVE : begin
            if FFlag then UpdateScrolls(ListSW, True)
          end;
        end;
      end;
    except
      Application.HandleException(Self);
    end;
  end
  else with Message do
    Result := CallWindowProc(FhDefHeaderProc, FhWndHeader, Msg, WParam, LParam);
end;

procedure TsCustomListView.WMParentNotify(var Message: TWMParentNotify);
var
  WndName : string;
begin
  try
    with Message do begin
      SetLength(WndName, 96);
      SetLength(WndName, GetClassName(ChildWnd, PChar(WndName), Length(WndName)));
      if (Event = WM_CREATE) and (WndName = 'SysHeader32') then begin
        if (FhWndHeader <> 0) then begin
          SetWindowLong(FhWndHeader, GWL_WNDPROC, LongInt(FhDefHeaderProc));
          FhWndHeader := 0;
        end;
        if (FhWndHeader = 0) then begin
          FhWndHeader := ChildWnd;
          FhDefHeaderProc := Pointer(GetWindowLong(FhWndHeader, GWL_WNDPROC));
          SetWindowLong(FhWndHeader, GWL_WNDPROC, LongInt(FhHeaderProc));
        end;
      end else
      if (Event = WM_DESTROY) and (WndName = 'SysHeader32') then begin
        if (FhWndHeader <> 0) then begin
          SetWindowLong(FhWndHeader, GWL_WNDPROC, LongInt(FhDefHeaderProc));
          FhWndHeader := 0;
        end;
        if (FhWndHeader = 0) then begin
          FhWndHeader := ChildWnd;
          FhDefHeaderProc := Pointer(GetWindowLong(FhWndHeader, GWL_WNDPROC));
          SetWindowLong(FhWndHeader, GWL_WNDPROC, LongInt(FhHeaderProc));
        end;
      end;
    end;
  except
    Application.HandleException(Self);
  end;
  inherited;
end;

procedure TsCustomListView.PaintHeader;
var
  i, count, RightPos : Integer;
  rc, HeaderR : TRect;
  PS : TPaintStruct;
begin
  BeginPaint(FhWndHeader, PS);
  try
    if not FCommonData.FCacheBmp.Empty then begin
      RightPos := 0;
      count := Header_GetItemCount(FhWndHeader) - 1;
      if count > -1 then begin
        // Draw Columns Headers
        for i := 0 to count do begin
          rc := GetHeaderColumnRect(i);
          if not IsRectEmpty(rc) then begin
            ListLineHeight := HeightOf(rc);
            ColumnSkinPaint(rc, i);
          end;
          if RightPos < rc.Right then RightPos := rc.Right;
        end;
      end
      else begin
        rc := GetHeaderColumnRect(0);
        ListLineHeight := HeightOf(rc);
      end;

      // Draw background section
      if Windows.GetWindowRect(FhWndHeader, HeaderR) then begin
        rc := Rect(RightPos, 0, WidthOf(HeaderR), HeightOf(HeaderR));
        if not IsRectEmpty(rc) then begin ColumnSkinPaint(rc, -1); end;
      end;
    end;
  finally
    EndPaint(FhWndHeader, PS);
  end;
end;

function TsCustomListView.GetHeaderColumnRect(Index: Integer): TRect;
var
  SectionOrder : array of Integer;
  rc : TRect;
begin
  if FhWndHeader <> 0 then begin
    if Self.FullDrag then begin
      SetLength(SectionOrder, Columns.Count);
      Header_GetOrderArray(FhWndHeader, Columns.Count, PInteger(SectionOrder));
      Header_GETITEMRECT(FhWndHeader, SectionOrder[Index] , @rc);
    end
    else begin
      Header_GETITEMRECT(FhWndHeader, Index, @rc);
    end;
    Result := rc;
  end
  else Result := Rect(0, 0, 0, 0);
end;

procedure TsCustomListView.ColumnSkinPaint(ControlRect : TRect; cIndex : Integer);
const
  HDF_SORTDOWN = $0200;
  HDF_SORTUP = $0400;
var
  R, TextRC   : TRect;
  tmpdc : HDC;
  TempBmp : Graphics.TBitmap;
  State, si : integer;
  Flags : integer;
  bf : HFont;
{$IFDEF TNTUNICODE}
  Item: THDItemW;
{$ELSE}
  Item: THDItem;
{$ENDIF}
  Buf: array[0..128] of acChar;
  ws : acString;
  ts, ArrowSize : TSize;
  ArrowIndex : integer;
  CI : TCacheInfo;
  gWidth : integer;
begin
  try
    TempBmp := CreateBmp24(WidthOf(ControlRect), HeightOf(ControlRect));
    R := Rect(0, 0, TempBmp.Width, TempBmp.Height);
    if FPressedColumn >= 0 then State := iffi(FPressedColumn = cIndex, 2, 0) else if HoverColIndex = cIndex then State := 1 else State := 0;
    CI.Ready := False;
    CI.FillColor := Color;
    si := PaintSection(TempBmp, s_ColHeader, s_Button, State, SkinData.SkinManager, ControlRect.TopLeft, CI.FillColor);
    bf := LongWord(SendMessage(Handle, WM_GETFONT, 0, 0));
    SelectObject(TempBmp.Canvas.Handle, bf);
    TextRC := R;
    InflateRect(TextRC, -4, -1);
    TempBmp.Canvas.Brush.Style := bsClear;
    FillChar(Item, SizeOf(Item), 0);
    FillChar(Buf, SizeOf(Buf), 0);
    Item.pszText := PacChar(@Buf);
    Item.cchTextMax := SizeOf(Buf);
    Item.Mask := HDI_TEXT or HDI_FORMAT or HDI_IMAGE or HDI_BITMAP;
    if (cIndex >= 0) and bool(SendMessage(FHwndHeader, {$IFDEF TNTUNICODE}HDM_GETITEMW{$ELSE}HDM_GETITEM{$ENDIF}, cIndex, Longint(@Item))) then begin
      ws := acString(Item.pszText);
      Flags := DT_END_ELLIPSIS or DT_EXPANDTABS or DT_SINGLELINE or DT_VCENTER;
      if (SmallImages = nil) or (Item.fmt and (LVCFMT_IMAGE or LVCFMT_COL_HAS_IMAGES) = 0) then begin
        Item.iImage := -1;
        gWidth := 0;
      end
      else gWidth := SmallImages.Width + 4;

      if item.fmt and HDF_SORTDOWN = HDF_SORTDOWN
        then ArrowIndex := SkinData.SkinManager.GetMaskIndex(SkinData.SkinManager.ConstData.IndexScrollBottom, s_ScrollBtnBottom, s_ItemGlyph)
        else if item.fmt and HDF_SORTUP = HDF_SORTUP
          then ArrowIndex := SkinData.SkinManager.GetMaskIndex(SkinData.SkinManager.ConstData.IndexScrollTop, s_ScrollBtnTop, s_ItemGlyph)
          else ArrowIndex := -1;
      if ArrowIndex <> -1 then begin
        ArrowSize.cx := WidthOfImage(SkinData.SkinManager.ma[ArrowIndex]) + 6;
        ArrowSize.cy := HeightOfImage(SkinData.SkinManager.ma[ArrowIndex]);
      end
      else begin
        ArrowSize.cx := 0;
        ArrowSize.cy := 0;
      end;

{$IFDEF TNTUNICODE}
      GetTextExtentPoint32W(TempBmp.Canvas.Handle, PacChar(ws), Length(ws), ts);
{$ELSE}
      GetTextExtentPoint32(TempBmp.Canvas.Handle, PacChar(ws), Length(ws), ts);
{$ENDIF}
      inc(ts.cx, 5);
      case (Item.fmt and $0ff) of
        HDF_CENTER : begin
          TextRc.Left := (WidthOf(TextRc) - ts.cx - ArrowSize.cx - gWidth) div 2 + TextRc.Left + gWidth;
          TextRc.Right := TextRc.Left + ts.cx;
        end;
        HDF_RIGHT : begin
          TextRc.Right := TextRc.Right - ArrowSize.cx;
          TextRc.Left := TextRc.Right - ts.cx;
        end
        else begin
          TextRc.Left := TextRc.Left + gWidth;
          TextRc.Right := TextRc.Left + ts.cx;
        end
      end;
      if ArrowIndex <> -1 then DrawSkinGlyph(TempBmp, Point(TextRc.Right + 6, (HeightOf(TextRc) - ArrowSize.cy) div 2), State, 1, SkinData.SkinManager.ma[ArrowIndex], MakeCacheInfo(TempBmp));

      if (State = 2) then OffsetRect(TextRc, 1, 1);
      acWriteTextEx(TempBmp.Canvas, PacChar(ws), True, TextRc, Flags, si, (State <> 0), SkinData.SkinManager);
      if (item.iImage <> -1)
        then SmallImages.Draw(TempBmp.Canvas, TextRc.Left - gWidth, (HeightOf(TextRc) - SmallImages.Height) div 2 + integer(State = 2), Columns[cIndex].ImageIndex, Enabled);
    end;

    if acPrintDC = 0 then tmpdc := GetDC(FhWndHeader) else tmpdc := acPrintDC;
    try
      BitBlt(tmpdc, ControlRect.Left, ControlRect.Top, R.Right, R.Bottom, TempBmp.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      if acPrintDC = 0 then ReleaseDC(FhWndHeader, tmpdc);
    end;
    FreeAndNil(TempBmp);
  except
    Application.HandleException(Self);
  end;
{
const
  HDF_SORTDOWN = $0200;
  HDF_SORTUP = $0400;
var
  CI : TCacheInfo;
  R, TextRC   : TRect;
  Flags : integer;
  Item: THDItem;
  ts, ArrowSize : TSize;
  ArrowIndex : integer;
  tmpdc : HDC;
  TempBmp : Graphics.TBitmap;
  State, si, bWidth : integer;
  sci : TScrollInfo;
begin
  try
    TempBmp := CreateBmp24(WidthOf(ControlRect), HeightOf(ControlRect));
    bWidth := 1 + integer(BorderStyle) * (1 + integer(Ctl3D));
    CI := MakeCacheInfo(FCommonData.FCacheBmp, ControlRect.Left + bWidth, ControlRect.Top + bWidth);
    R := Rect(0, 0, TempBmp.Width, TempBmp.Height);

    if FPressedColumn >= 0 then begin
      State := iffi(FPressedColumn = cIndex, 2, 0);
    end
    else begin
      if HoverColIndex = cIndex then State := 1 else State := 0;
    end;

    si := FCommonData.SkinManager.GetSkinIndex(s_ColHeader);
    if (ListSW <> nil) and (ListSW.sBarHorz <> nil) and ListSW.sBarHorz.fScrollVisible then begin
      sci.cbSize := SizeOf(TScrollInfo);
      sci.fMask := SIF_ALL;
      GetScrollInfo(Handle, SB_HORZ, sci);
    end
    else sci.npos := 0;
    if FCommonData.SkinManager.IsValidSkinIndex(si) then begin
      PaintItem(si, s_ColHeader, Ci, True, State, r, Point(-sci.npos, 0), TempBmp)
    end
    else begin
      si := FCommonData.SkinManager.GetSkinIndex(s_Button);
      PaintItem(si, s_Button, Ci, True, State, r, Point(-sci.npos, 0), TempBmp);
    end;

    TempBmp.Canvas.Font.Assign(Font);
    TextRC := R;
    InflateRect(TextRC, 0, -1);
    TextRc.Left := TextRc.Left + 4 + integer(State = 2);
    TextRc.Right := TextRc.Right - TextRc.Left - 4 + integer(State = 2);
    TextRc.Top := TextRc.Top + integer(State = 2);
    TextRc.Bottom := TextRc.Bottom + integer(State = 2);

    TempBmp.Canvas.Brush.Style := bsClear;
    if cIndex >= 0 then begin
      if (Length(Columns[cIndex].Caption) > 0) or (Columns[cIndex].ImageIndex >= 0) then begin
        if Assigned(SmallImages) and (Columns[cIndex].ImageIndex >= 0) then begin
          if Columns[cIndex].Alignment <> taRightJustify then OffsetRect(TextRc, SmallImages.Width, 0);
          acWriteTextEx(TempBmp.Canvas, PacChar(Columns[cIndex].Caption), True, TextRc,
             DrawTextBiDiModeFlags(DT_EXPANDTABS or DT_WORDBREAK or GetStringFlags(Self, Columns[cIndex].Alignment)),
             Si, (State <> 0), FCommonData.SkinManager);
          SmallImages.Draw(TempBmp.Canvas, 4 + integer(State = 2), integer(State = 2), Columns[cIndex].ImageIndex, Enabled);
        end
        else begin
          acWriteTextEx(TempBmp.Canvas, PacChar(Columns[cIndex].Caption), True, TextRc,
             DrawTextBiDiModeFlags(DT_EXPANDTABS or DT_WORDBREAK or GetStringFlags(Self, Columns[cIndex].Alignment)),
             Si, (State <> 0), FCommonData.SkinManager);
        end;
      end;
    end;

    if acPrintDC = 0 then tmpdc := GetDC(FhWndHeader) else tmpdc := acPrintDC;
    try
      BitBlt(tmpdc, ControlRect.Left, ControlRect.Top, R.Right, R.Bottom, TempBmp.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      if acPrintDC = 0 then ReleaseDC(FhWndHeader, tmpdc);
    end;
    TempBmp.Free
  except
    Application.HandleException(Self);
  end;
}  
end;

procedure TsCustomListView.PrepareCache;
begin
  FCommonData.InitCacheBmp;
  PaintItem(FCommonData, GetParentCache(FCommonData), False, 0, Rect(0, 0, Width, Height), Point(Left, Top), FCommonData.FCacheBmp, False);
  FCommonData.BGChanged := False;
end;

procedure TsCustomListView.WMHitTest(var Message: TMessage);
begin
  inherited;
  if FCommonData.Skinned and (HoverColIndex > -1) and FHighLightHeaders then begin
    HoverColIndex := -2;
    PaintHeader;
  end;
end;

function TsCustomListView.AllColWidth: integer;
var
  i, w, c : integer;
begin
  Result := 0;
  c := Columns.Count - 1;
  for i := 0 to c do begin
    try
      w := integer(ListView_GetColumnWidth(Handle, i));
      if abs(w) > 999999 then Exit;
      Result := integer(Result + w);
    except
    end;
  end
end;

procedure TsCustomListView.NewAdvancedCustomDraw(Sender: TCustomListView; const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var
  SavedDC : hdc;
  i, TopIndex, LastIndex : integer; // v4.71
  R : TRect;
begin
  // inherite...
  if not (csDesigning in ComponentState) and Assigned(FOldAdvancedCustomDraw) then FOldAdvancedCustomDraw(Sender, Arect, Stage, DefaultDraw) else begin
    if (Stage in [cdPreErase, cdPrePaint]) then begin
      FCommonData.Updating := FCommonData.Updating;
      if FCommonData.Updating then Exit;
      if SkinData.BGChanged then PrepareCache;
      if FullRepaint then begin
        SavedDC := SaveDC(Canvas.Handle);
        if (Stage in [cdPrePaint]) and LocalFlag then begin
          if not (ViewStyle in [vsSmallIcon, vsIcon]) then TopIndex := ListView_GetTopIndex(Handle) else TopIndex := 0;
          if ViewStyle in [vsReport, vsList] then LastIndex:= TopIndex + ListView_GetCountPerPage(Handle) -1 else LastIndex := Items.Count - 1;
          for i := TopIndex to LastIndex do begin
            if ListView_GetItemRect(Handle, i, R, LVIR_ICON) then ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
            if ListView_GetItemRect(Handle, i, R, LVIR_LABEL) then ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
          end;
        end;
        BitBlt(Canvas.Handle, 0, 0, ClientWidth, ClientHeight, FCommonData.FCacheBmp.Canvas.Handle,
                        integer(BorderStyle = bsSingle) * 2, integer(BorderStyle = bsSingle) * 2, SRCCOPY);
        RestoreDC(Canvas.Handle, SavedDC);
        if (Stage in [cdPrePaint]) and not SkinData.CustomColor then begin
          // Ensure that the items are drawn transparently
          SetBkMode(Canvas.Handle, TRANSPARENT);
          ListView_SetTextBkColor(Handle, CLR_NONE);
          ListView_SetBKColor(Handle, CLR_NONE);
        end;
      end
      else if not SkinData.CustomColor then begin
        Color := GetBGColor(SkinData, 0);
{        SendMessage(Handle, SM_ALPHACMD, MakeWParam(0, AC_GETCONTROLCOLOR), 0);
        if (ParentCenterColor <> clFuchsia) and (ParentCenterColor <> Color) then Color := ColorToRGB(ParentCenterColor);
        ParentCenterColor := clFuchsia;}
      end;
      if Stage = cdPreErase then DefaultDraw := False
    end else if Stage = cdPostErase then DefaultDraw := False
  end
end;

function TsCustomListView.FullRepaint: boolean;
begin
  Result := False;
end;

procedure TsCustomListView.InvalidateSmooth(Always : boolean);
begin
  if FullRepaint then begin
    if Always then InvalidateRect(Handle, nil, False) else case ViewStyle of
      vsList : begin
        if (ListSW.sBarHorz.ScrollInfo.nPos < ListSW.sBarHorz.ScrollInfo.nMax - 1) and
          (ListSW.sBarHorz.ScrollInfo.nPos > ListSW.sBarHorz.ScrollInfo.nMin) then InvalidateRect(Handle, nil, False);
      end;
      vsReport : begin
        GetScrollInfo(Handle, SB_VERT, ListSW.sBarVert.ScrollInfo);
        if (ListSW.sBarVert.ScrollInfo.nPos < ListSW.sBarVert.ScrollInfo.nMax - Font.Size - 3) and
          (ListSW.sBarVert.ScrollInfo.nPos > ListSW.sBarVert.ScrollInfo.nMin) then InvalidateRect(Handle, nil, False);
      end;
    end;
  end;
end;

end.
