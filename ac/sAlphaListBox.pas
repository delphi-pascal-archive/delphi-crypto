unit sAlphaListBox;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses StdCtrls, controls, classes, forms, graphics, messages, windows, sysutils, consts, sCommonData,
  sConst, sDefaults, commctrl, acSBUtils{$IFNDEF DELPHI5}, types{$ENDIF};

type

{$IFNDEF NOTFORHELP}
{$IFNDEF DELPHI6UP}
  TListBoxStyle = (lbStandard, lbOwnerDrawFixed, lbOwnerDrawVariable, lbVirtual, lbVirtualOwnerDraw);
  TLBGetDataEvent = procedure(Control: TWinControl; Index: Integer; var Data: string) of object;
  TLBGetDataObjectEvent = procedure(Control: TWinControl; Index: Integer; var DataObject: TObject) of object;
  TLBFindDataEvent = function(Control: TWinControl; FindString: string): Integer of object;
{$ENDIF}
{$ENDIF} // NOTFORHELP

  TsAlphaListBox = class(TWinControl)
{$IFNDEF NOTFORHELP}
  private
    FCount: Integer;
    FOldCount: Integer;
    FFilter: String;
    FLastTime: DWord;
    FItems: TStrings;
    FBorderStyle: TBorderStyle;
    FCanvas: TCanvas;
    FColumns: Integer;
    FItemHeight: Integer;
    FStyle: TListBoxStyle;
    FIntegralHeight: Boolean;
    FMultiSelect: Boolean;
    FSorted: Boolean;
    FExtendedSelect: Boolean;
    FTabWidth: Integer;
    FSaveItems: TStringList;
    FSaveTopIndex: Integer;
    FSaveItemIndex: Integer;
    FOnDrawItem: TDrawItemEvent;
    FOnMeasureItem: TMeasureItemEvent;
    FCommonData: TsCommonData;
    FOnVScroll: TNotifyEvent;
    FDisabledKind: TsDisabledKind;
    FBoundLabel: TsBoundLabel;
    FOnDataFind: TLBFindDataEvent;
    FOnData: TLBGetDataEvent;
    FOnDataObject: TLBGetDataObjectEvent;
    FAutoComplete: Boolean;
    FAutoHideScroll: boolean;
    FAutoCompleteDelay: Word;
    function GetItemHeight: Integer;
    function GetItemIndex: Integer;
    function GetSelCount: Integer;
    function GetSelected(Index: Integer): Boolean;
    function GetTopIndex: Integer;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetColumnWidth;
    procedure SetColumns(Value: Integer);
    procedure SetExtendedSelect(Value: Boolean);
    procedure SetIntegralHeight(Value: Boolean);
    procedure SetItemHeight(Value: Integer);
    procedure SetItems(Value: TStrings);
    procedure SetItemIndex(Value: Integer);
    procedure SetMultiSelect(Value: Boolean);
    procedure SetSelected(Index: Integer; Value: Boolean);
    procedure SetSorted(Value: Boolean);
    procedure SetStyle(Value: TListBoxStyle);
    procedure SetTabWidth(Value: Integer);
    procedure SetTopIndex(Value: Integer);
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMNCCalcSize (var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMEraseBkgnd (var Message: TWMPaint); message WM_ERASEBKGND;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TMessage); message WM_LBUTTONUP;
    procedure SetDisabledKind(const Value: TsDisabledKind);
    procedure WMPrint (var Message: TWMPaint); message WM_PRINT;

    procedure LBGetText(var Message: TMessage); message LB_GETTEXT;
    procedure LBGetTextLen(var Message: TMessage); message LB_GETTEXTLEN;
    function GetCount: Integer;
    procedure SetCount(const Value: Integer);
    procedure SetAutoHideScroll(const Value: boolean);
    function GetScrollWidth: Integer;
    procedure SetScrollWidth(const Value: Integer);
  protected
    FMoving: Boolean;
    FTopIndex : integer;
    ListSW : TacScrollWnd;

    function DoGetData(const Index: Integer): String;
    function DoGetDataObject(const Index: Integer): TObject;
    function DoFindData(const Data: String): Integer;
    procedure KeyPress(var Key: Char); override;

    procedure PrepareCache; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure DragCanceled; override;
    procedure MeasureItem(Index: Integer; var Height: Integer); virtual;
    function InternalGetItemData(Index: Integer): Longint; dynamic;
    procedure InternalSetItemData(Index: Integer; AData: Longint); dynamic;
    function GetItemData(Index: Integer): LongInt; dynamic;
    function VisibleRows : integer;
    procedure SetItemData(Index: Integer; AData: LongInt); dynamic;
    procedure ResetContent; dynamic;
    procedure DeleteString(Index: Integer); dynamic;
    property ExtendedSelect: Boolean read FExtendedSelect write SetExtendedSelect default True;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); virtual;
    procedure WndProc(var Message: TMessage); override;
  public
    procedure AfterConstruction; override;
    procedure AddItem(Item: String; AObject: TObject); virtual;
    procedure DeleteSelected;
    procedure ClearSelection;
    procedure Loaded; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function ItemAtPos(Pos: TPoint; Existing: Boolean): Integer;
    function ItemRect(Index: Integer): TRect;
    procedure RepaintItem(Index : Integer);
    property Canvas: TCanvas read FCanvas;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property SelCount: Integer read GetSelCount;
    property Selected[Index: Integer]: Boolean read GetSelected write SetSelected;
    property TopIndex: Integer read GetTopIndex write SetTopIndex;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property ScrollWidth: Integer read GetScrollWidth write SetScrollWidth default 0;
{$ENDIF} // NOTFORHELP
    property Count: Integer read GetCount write SetCount;
  published
    property AutoCompleteDelay : Word read FAutoCompleteDelay write FAutoCompleteDelay;
    property AutoComplete: Boolean read FAutoComplete write FAutoComplete default True;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property BoundLabel : TsBoundLabel read FBoundLabel write FBoundLabel;
    property Columns: Integer read FColumns write SetColumns default 0;
    property SkinData : TsCommonData read FCommonData write FCommonData;
    property DisabledKind : TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property IntegralHeight: Boolean read FIntegralHeight write SetIntegralHeight default False;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property Items: TStrings read FItems write SetItems;
    property Sorted: Boolean read FSorted write SetSorted default False;
    property Style: TListBoxStyle read FStyle write SetStyle default lbStandard;
    property TabWidth: Integer read FTabWidth write SetTabWidth default 0;
{$IFNDEF NOTFORHELP}
    property AutoHideScroll : boolean read FAutoHideScroll write SetAutoHideScroll default True;
    property OnData: TLBGetDataEvent read FOnData write FOnData;
    property OnDataObject: TLBGetDataObjectEvent read FOnDataObject write FOnDataObject;
    property OnDataFind: TLBFindDataEvent read FOnDataFind write FOnDataFind;
    property OnDrawItem: TDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnMeasureItem: TMeasureItemEvent read FOnMeasureItem write FOnMeasureItem;
    property OnVScroll : TNotifyEvent read FOnVScroll write FOnVScroll;
    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
{$ENDIF} // NOTFORHELP
  end;

  TsListBox = class(TsAlphaListBox)
  published
{$IFNDEF NOTFORHELP}
    property MultiSelect;
    property ExtendedSelect;
{$ENDIF} // NOTFORHELP
  end;

{$IFNDEF NOTFORHELP}
var
  mPressed : boolean = False;
  ScrollsUpdating : boolean = False;
{$ENDIF}


implementation

uses sVCLUtils, sGraphUtils, math, sMessages, sStyleSimply, sSkinProps, sAlphaGraph, sStrings
  {$IFDEF DELPHI6UP}, RTLConsts{$ENDIF} {$IFDEF LOGGED}, sDebugMsgs{$ENDIF};

const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);

type
  TListBoxStrings = class(TStrings)
  private
    ListBox: TsAlphaListBox;
  protected
    procedure Put(Index: Integer; const S: string); override;
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
    procedure Update;
  public
    function Add(const S: string): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function IndexOf(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
  end;

{ TsAlphaListBox }
procedure TsAlphaListBox.AddItem(Item: String; AObject: TObject);
var
  S: String;
begin
  SetString(S, PChar(Item), StrLen(PChar(Item)));
  Items.AddObject(S, AObject);
end;

procedure TsAlphaListBox.AfterConstruction;
begin
  inherited AfterConstruction;
  SkinData.Loaded;
end;

procedure TsAlphaListBox.Clear;
begin
  FItems.Clear;
end;

procedure TsAlphaListBox.ClearSelection;
var
  I: Integer;
begin
  if MultiSelect then for I := 0 to Items.Count - 1 do Selected[I] := False else ItemIndex := -1;
end;

procedure TsAlphaListBox.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) and not (csLoading in ComponentState) then RecreateWnd;
  inherited;
end;

procedure TsAlphaListBox.CMEnabledChanged(var Message: TMessage);
begin
  FCommonData.BGChanged := True;
  Perform(WM_PAINT, 0, 0);
  Perform(WM_NCPAINT, 0, 0);
  inherited;
end;

procedure TsAlphaListBox.CMFontChanged(var Message: TMessage);
var
  R : TRect;
begin
  inherited;
  if not (csLoading in ComponentState) then begin
    SkinData.BGChanged := True;
    Repaint;
    if HandleAllocated and (FStyle = lbStandard) then begin
      Perform(LB_GETITEMRECT, 0, Longint(@R));
      ItemHeight := R.Bottom - R.Top;
    end;
  end;
end;

procedure TsAlphaListBox.CNCommand(var Message: TWMCommand);
begin
  case Message.NotifyCode of
    LBN_SELCHANGE: begin
      inherited Changed;
      Click;
    end;
    LBN_DBLCLK: DblClick;
  end;
end;

procedure TsAlphaListBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  FCommonData.Updating := FCommonData.Updating;
  if FCommonData.Updating and SkinData.Skinned then Exit;
  if Items.Count < 1 then Exit;
  with Message.DrawItemStruct^ do begin
    State := TOwnerDrawState(LongRec(itemState).Lo);
    FCanvas.Handle := hDC;
    FCanvas.Font := Font;
    FCanvas.Brush := Brush;
    if not ((Style in [lbOwnerDrawFixed, lbOwnerDrawVariable, lbVirtualOwnerDraw]) and Assigned(OnDrawItem)) and (Integer(itemID) >= 0) and ((odSelected in State) or ((integer(itemID) = ItemIndex) and not Focused)) then begin
      FCanvas.Brush.Color := clHighlight;
      FCanvas.Font.Color := clHighlightText
    end
    else if not FCommonData.CustomFont then begin
      if not Enabled then FCanvas.Font.Color := AverageColor(Font.Color, Color) else FCanvas.Font.Color := Font.Color;
    end;

    DrawItem(Message.DrawItemStruct^.itemID, Message.DrawItemStruct^.rcItem, State);
    FCanvas.Handle := 0;
  end;
end;

procedure TsAlphaListBox.CNMeasureItem(var Message: TWMMeasureItem);
begin
  with Message.MeasureItemStruct^ do begin
    itemHeight := FItemHeight;
    if FStyle = lbOwnerDrawVariable then MeasureItem(itemID, Integer(itemHeight));
  end;
end;

constructor TsAlphaListBox.Create(AOwner: TComponent);
const
  ListBoxStyle = [csSetCaption, csDoubleClicks];
begin
  inherited Create(AOwner);
  if NewStyleControls then ControlStyle := ListBoxStyle else ControlStyle := ListBoxStyle + [csFramed, csOpaque];
  Width := 121;
  Height := 97;
  TabStop := True;
  ParentColor := False;
  FItems := TListBoxStrings.Create;
  TListBoxStrings(FItems).ListBox := Self;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FItemHeight := 16;
  FAutoComplete := True;
  FBorderStyle := bsSingle;
  FExtendedSelect := True;
  FAutoHideScroll := True;
  FCommonData := TsCommonData.Create(Self, True);
  FCommonData.COC := COC_TsAlphaListBox;
  if FCommonData.SkinSection = '' then FCommonData.SkinSection := s_Edit;
  FTopIndex := 0;
  FDisabledKind := DefDisabledKind;
  FBoundLabel := TsBoundLabel.Create(Self, FCommonData);
  FOldCount := -1;
  FAutoCompleteDelay := 500;
  DoubleBuffered := False;
end;

procedure TsAlphaListBox.CreateParams(var Params: TCreateParams);
type
  PSelects = ^TSelects;
  TSelects = array[Boolean] of DWORD;
const
  Styles: array[TListBoxStyle] of DWORD = (0, LBS_OWNERDRAWFIXED, LBS_OWNERDRAWVARIABLE, LBS_OWNERDRAWFIXED, LBS_OWNERDRAWFIXED);
  Sorteds: array[Boolean] of DWORD = (0, LBS_SORT);
  MultiSelects: array[Boolean] of DWORD = (0, LBS_MULTIPLESEL);
  ExtendSelects: array[Boolean] of DWORD = (0, LBS_EXTENDEDSEL);
  IntegralHeights: array[Boolean] of DWORD = (LBS_NOINTEGRALHEIGHT, 0);
  MultiColumns: array[Boolean] of DWORD = (0, LBS_MULTICOLUMN);
  TabStops: array[Boolean] of DWORD = (0, LBS_USETABSTOPS);
  CSHREDRAW: array[Boolean] of DWORD = (CS_HREDRAW, 0);
  Data: array[Boolean] of DWORD = (LBS_HASSTRINGS, LBS_NODATA);
var
  Selects: PSelects;
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'LISTBOX');
  with Params do begin
    Selects := @MultiSelects;
    if FExtendedSelect then Selects := @ExtendSelects;

    Style := Style or ({WS_HSCROLL or }WS_VSCROLL or Data[Self.Style in [lbVirtual, lbVirtualOwnerDraw]] or
      LBS_NOTIFY) or Styles[FStyle] or Sorteds[FSorted] or Selects^[FMultiSelect] or IntegralHeights[FIntegralHeight] or
      MultiColumns[FColumns <> 0] or BorderStyles[FBorderStyle] or TabStops[FTabWidth <> 0];

    if FColumns <> 0 then Style := Style or WS_HSCROLL;

    if not FAutoHideScroll then Style := Style or LBS_DISABLENOSCROLL;

    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;

    WindowClass.style := WindowClass.style and not (CSHREDRAW[UseRightToLeftAlignment] or CS_VREDRAW);

  end;
end;

procedure TsAlphaListBox.CreateWnd;
var
  W, H: Integer;
begin
  W := Width;
  H := Height;
  inherited CreateWnd;
  SetWindowPos(Handle, 0, Left, Top, W, H, SWP_NOZORDER or SWP_NOACTIVATE);
  if FTabWidth <> 0 then SendMessage(Handle, LB_SETTABSTOPS, 1, Longint(@FTabWidth));
  SetColumnWidth;
  if (FOldCount <> -1) or Assigned(FSaveItems) then begin
    if (Style in [lbVirtual, lbVirtualOwnerDraw]) then
      Count := FOldCount;
    if FSaveItems <> nil then begin
      FItems.Assign(FSaveItems);
      FreeAndNil(FSaveItems);
    end;
    SetTopIndex(FSaveTopIndex);
    SetItemIndex(FSaveItemIndex);
    FOldCount := -1;
  end;
end;

procedure TsAlphaListBox.DeleteSelected;
var
  I: Integer;
begin
  if MultiSelect then begin
    for I := Items.Count - 1 downto 0 do if Selected[I] then Items.Delete(I);
  end
  else if ItemIndex <> -1 then Items.Delete(ItemIndex);
end;

procedure TsAlphaListBox.DeleteString(Index: Integer);
begin
  SendMessage(Handle, LB_DELETESTRING, Index, 0);
end;

destructor TsAlphaListBox.Destroy;
begin
  if ListSW <> nil then FreeAndNil(ListSW);
  FreeAndNil(FBoundLabel);
  if Assigned(FCommonData) then FreeAndNil(FCommonData);
  inherited Destroy;
  FCanvas.Free;
  FItems.Free;
  FSaveItems.Free;
end;

procedure TsAlphaListBox.DestroyWnd;
begin
  if (FItems.Count > 0) then begin
    if (Style in [lbVirtual, lbVirtualOwnerDraw]) then
      FOldCount := FItems.Count
    else begin
      FSaveItems := TStringList.Create;
      FSaveItems.Assign(FItems);
    end;
    FSaveTopIndex := GetTopIndex;
    FSaveItemIndex := GetItemIndex;
  end;
  inherited DestroyWnd;
end;

function TsAlphaListBox.DoFindData(const Data: String): Integer;
begin
  if Assigned(FOnDataFind) then Result := FOnDataFind(Self, Data) else Result := -1;
end;

function TsAlphaListBox.DoGetData(const Index: Integer): String;
begin
  if Assigned(FOnData) then FOnData(Self, Index, Result);
end;

function TsAlphaListBox.DoGetDataObject(const Index: Integer): TObject;
begin
  if Assigned(FOnDataObject) then FOnDataObject(Self, Index, Result);
end;

procedure TsAlphaListBox.DragCanceled;
var
  M: TWMMouse;
  MousePos: TPoint;
begin
  with M do begin
    Msg := WM_LBUTTONDOWN;
    GetCursorPos(MousePos);
    Pos := PointToSmallPoint(ScreenToClient(MousePos));
    Keys := 0;
    Result := 0;
  end;
  DefaultHandler(M);
  M.Msg := WM_LBUTTONUP;
  DefaultHandler(M);
end;

procedure TsAlphaListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Flags: Longint;
//  TempBmp : Graphics.TBitmap;
//  R : TRect;
//  CI : TCacheInfo;
  Data : string;
begin
  if Index < 0 then Exit;
(*  if FCommonData.Skinned then begin
//    if FCommonData.BGChanged then PrepareCache;
{    if FCommonData.Skinned then if Self.ClientHeight = Height then begin
      Perform(CM_RECREATEWND, 0, 0); // Fixing of error in CalcSize..
      Perform(CM_INVALIDATE, 0, 0);
      Exit;
    end;}
    TempBmp := Graphics.TBitmap.Create;
    TempBmp.PixelFormat := pf24Bit;
    TempBmp.Width := WidthOf(Rect);
    TempBmp.Height := HeightOf(Rect);
    TempBmp.Canvas.Font.Assign(Font);
    try
      R := Classes.Rect(0, 0, TempBmp.Width, TempBmp.Height);
      if (odSelected in State) or ((Index = ItemIndex) and not Focused) then begin
        TempBmp.Canvas.Brush.Color := clHighlight;
        TempBmp.Canvas.Brush.Style := bsSolid;
        TempBmp.Canvas.FillRect(R);
        TempBmp.Canvas.Font.Color := clHighlightText;
      end
      else begin
        BitBlt(TempBmp.Canvas.Handle, 0, 0, TempBmp.Width, TempBmp.Height, SkinData.FCacheBmp.Canvas.Handle, Rect.Left + 3, Rect.Top + 3, SRCCOPY);
        State := [];
        TempBmp.Canvas.Brush.Color := clWhite;
        TempBmp.Canvas.Brush.Style := bsClear;
        TempBmp.Canvas.Font.Color := Font.Color;
      end;
      if not Assigned(FOnDrawItem) then {FOnDrawItem(Self, Index, Rect, State) else }begin
        R.Left := 2;
        if (Style in [lbVirtual, lbVirtualOwnerDraw]) then Data := DoGetData(Index) else Data := Items[Index];
        if (odSelected in State) or ((Index = ItemIndex) and not Focused) then begin
          WriteText(TempBmp.Canvas, PChar(Data), True, R, DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
        end
        else begin
          DrawText(TempBmp.Canvas.Handle, PChar(Data), Length(Data), R, DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
        end;
        R := Classes.Rect(0, 0, TempBmp.Width, TempBmp.Height);
        if odFocused in State then DrawFocusRect(TempBmp.Canvas.Handle, R);
      end;
      if not Enabled then begin
        CI.Bmp := SkinData.FCacheBmp;
        CI.X := 0;
        CI.Y := 0;
        CI.Ready := True;
        BmpDisabledKind(TempBmp, FDisabledKind, Parent, CI, Point(Rect.Left + 3, Rect.Top + 3));
      end;
      BitBlt(Canvas.Handle, Rect.Left, Rect.Top, TempBmp.Width, TempBmp.Height, TempBmp.Canvas.Handle, 0, 0, SRCCOPY);
      if Assigned(FOnDrawItem) then FOnDrawItem(Self, Index, Rect, State);
    finally
      FreeAndNil(TempBmp);
    end;
  end
  else*) begin
    if not ((Style in [lbOwnerDrawFixed, lbOwnerDrawVariable, lbVirtualOwnerDraw]) and Assigned(OnDrawItem)) then FCanvas.FillRect(Rect);
    if Assigned(FOnDrawItem) then begin
      FCanvas.FillRect(Rect);
      FOnDrawItem(Self, Index, Rect, State);
      if odFocused in State then FCanvas.DrawFocusRect(Rect);
    end
    else if (Index < Items.Count) and (Index > -1) then begin
      Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
      if not UseRightToLeftAlignment then Inc(Rect.Left, 2) else Dec(Rect.Right, 2);
      if (Style in [lbVirtual, lbVirtualOwnerDraw]) then Data := DoGetData(Index) else Data := Items[Index];
      DrawText(FCanvas.Handle, PChar(Data), Length(Data), Rect, Flags);
//      acDrawText(FCanvas.Handle, Data, Rect, Flags);
      if not UseRightToLeftAlignment then Dec(Rect.Left, 2) else Inc(Rect.Right, 2);
      if odFocused in State then DrawFocusRect(FCanvas.Handle, Rect);
    end;
  end;
end;

function TsAlphaListBox.GetCount: Integer;
begin
  if Style in [lbVirtual, lbVirtualOwnerDraw] then Result := FCount else Result := Items.Count;
end;

function TsAlphaListBox.GetItemData(Index: Integer): LongInt;
begin
  Result := SendMessage(Handle, LB_GETITEMDATA, Index, 0);
end;

function TsAlphaListBox.GetItemHeight: Integer;
var
  R: TRect;
begin
  Result := FItemHeight;
  if HandleAllocated and (FStyle = lbStandard) then begin
    Perform(LB_GETITEMRECT, 0, Longint(@R));
    Result := R.Bottom - R.Top;
  end;
end;

function TsAlphaListBox.GetItemIndex: Integer;
begin
  if MultiSelect
    then Result := SendMessage(Handle, LB_GETCARETINDEX, 0, 0)
    else Result := SendMessage(Handle, LB_GETCURSEL, 0, 0);
end;

function TsAlphaListBox.GetScrollWidth: Integer;
begin
  Result := SendMessage(Handle, LB_GETHORIZONTALEXTENT, 0, 0);
end;

function TsAlphaListBox.GetSelCount: Integer;
begin
  Result := SendMessage(Handle, LB_GETSELCOUNT, 0, 0);
end;

function TsAlphaListBox.GetSelected(Index: Integer): Boolean;
var
  R: Longint;
begin
  R := SendMessage(Handle, LB_GETSEL, Index, 0);
  Result := LongBool(R);
end;

function TsAlphaListBox.GetTopIndex: Integer;
begin
  Result := SendMessage(Handle, LB_GETTOPINDEX, 0, 0);
end;

function TsAlphaListBox.InternalGetItemData(Index: Integer): Longint;
begin
  Result := GetItemData(Index);
end;

procedure TsAlphaListBox.InternalSetItemData(Index, AData: Integer);
begin
  SetItemData(Index, AData);
end;

function TsAlphaListBox.ItemAtPos(Pos: TPoint; Existing: Boolean): Integer;
var
  Count: Integer;
  ItemRect: TRect;
begin
  if PtInRect(ClientRect, Pos) then begin
    Result := TopIndex;
    Count := Items.Count;
    while Result < Count do begin
      Perform(LB_GETITEMRECT, Result, Longint(@ItemRect));
      if PtInRect(ItemRect, Pos) then Exit;
      Inc(Result);
    end;
    if not Existing then Exit;
  end;
  Result := -1;
end;

function TsAlphaListBox.ItemRect(Index: Integer): TRect;
var
  Count: Integer;
begin
  Count := Items.Count;
  if (Index = 0) or (Index < Count) then
    Perform(LB_GETITEMRECT, Index, Longint(@Result))
  else if Index = Count then begin
    Perform(LB_GETITEMRECT, Index - 1, Longint(@Result));
    OffsetRect(Result, 0, Result.Bottom - Result.Top);
  end else FillChar(Result, SizeOf(Result), 0);
end;

procedure TsAlphaListBox.KeyPress(var Key: Char);            
  procedure FindString;
  var
    Idx: Integer;
  begin
    if Style in [lbVirtual, lbVirtualOwnerDraw]
      then Idx := DoFindData(FFilter)
      else Idx := SendMessage(Handle, LB_FINDSTRING, -1, LongInt(PChar(FFilter)));
    if Idx <> LB_ERR then begin
      if MultiSelect then begin
        ClearSelection;
        SendMessage(Handle, LB_SELITEMRANGE, 1, MakeLParam(Idx, Idx))
      end;
      ItemIndex := Idx;
      Click;
    end;
    if not (Ord(Key) in [VK_RETURN, VK_BACK, VK_ESCAPE]) then Key := #0;  // Clear so that the listbox's default search mechanism is disabled
  end;
var
  Msg: TMsg;
begin
  inherited KeyPress(Key);
  if not FAutoComplete then exit;
//  if GetTickCount - FLastTime >= 500 then FFilter := '';
  if GetTickCount - FLastTime >= FAutoCompleteDelay then FFilter := '';
  FLastTime := GetTickCount;

  if Ord(Key) <> VK_BACK then begin
    if Key in LeadBytes then  begin
      if PeekMessage(Msg, Handle, WM_CHAR, WM_CHAR, PM_REMOVE) then begin
        FFilter := FFilter + Key + Chr(Msg.wParam);
        Key := #0;
      end;
    end
    else FFilter := FFilter + Key;
  end
  else begin
    while ByteType(FFilter, Length(FFilter)) = mbTrailByte do Delete(FFilter, Length(FFilter), 1);
    Delete(FFilter, Length(FFilter), 1);
  end;

  if Length(FFilter) > 0 then FindString else begin
    ItemIndex := 0;
    Click;
  end;
end;

procedure TsAlphaListBox.LBGetText(var Message: TMessage);
var
  S: string;
begin
  if Style in [lbVirtual, lbVirtualOwnerDraw] then begin
    if Assigned(FOnData) and (Message.WParam > -1) and (Message.WParam < Count) then begin
      S := '';
      OnData(Self, Message.wParam, S);
      StrCopy(PChar(Message.lParam), PChar(S));
      Message.Result := Length(S);
    end
    else Message.Result := LB_ERR;
  end
  else inherited;
end;

procedure TsAlphaListBox.LBGetTextLen(var Message: TMessage);
var
  S: string;
begin
  if Style in [lbVirtual, lbVirtualOwnerDraw] then begin
    if Assigned(FOnData) and (Message.WParam > -1) and (Message.WParam < Count) then begin
      S := '';
      OnData(Self, Message.wParam, S);
      Message.Result := Length(S);
    end
    else Message.Result := LB_ERR;
  end
  else inherited
end;

procedure TsAlphaListBox.Loaded;
begin
  inherited Loaded;
  FCommonData.Loaded;
  if FCommonData.Skinned then begin
    if not FCommonData.CustomColor then Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].Color;
    if not FCommonData.CustomFont then Font.Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].FontColor[1];
  end;
end;

procedure TsAlphaListBox.MeasureItem(Index: Integer; var Height: Integer);
begin
  if Assigned(FOnMeasureItem) then FOnMeasureItem(Self, Index, Height)
end;

procedure TsAlphaListBox.PrepareCache;
var
  CI : TCacheInfo;
begin
  FCommonData.InitCacheBmp;
  CI := GetParentCache(FCommonData);
  PaintItem(SkinData, Ci, False, integer(ControlIsActive(SkinData)),
               Rect(0, 0, Width, Height),
               Point(Left, Top), SkinData.FCacheBmp, False);
  if not Enabled then BmpDisabledKind(FCommonData.FCacheBmp, FDisabledKind, Parent, CI, Point(Left, Top));
  FCommonData.BGChanged := False
end;

procedure TsAlphaListBox.RepaintItem(Index: Integer);
var
  DC, SavedDC : hdc;
  procedure PaintItem;
  var
    DrawItemMsg: TWMDrawItem;
    DrawItemStruct: TDrawItemStruct;
  begin
    DrawItemMsg.Msg := CN_DRAWITEM;
    DrawItemMsg.DrawItemStruct := @DrawItemStruct;
    DrawItemMsg.Ctl := Handle;
    DrawItemStruct.CtlType := ODT_LISTBOX;
    DrawItemStruct.itemAction := ODA_DRAWENTIRE;
    DrawItemStruct.itemState := integer(ItemIndex = Index);
    DrawItemStruct.hDC := DC;
    DrawItemStruct.CtlID := Handle;
    DrawItemStruct.hwndItem := Handle;
    DrawItemStruct.itemID := Index;

    DrawItemStruct.rcItem := ItemRect(Index);
    DrawItemStruct.itemState := 0;

    if MultiSelect then begin
      if Selected[Index] then DrawItemStruct.itemState := DrawItemStruct.itemState or ODS_SELECTED;
    end;
    if Focused and (Index = ItemIndex) then DrawItemStruct.itemState := DrawItemStruct.itemState or ODS_FOCUS or ODS_SELECTED;

    Dispatch(DrawItemMsg);
  end;
begin
  if acPrintDC <> 0 then DC := acPrintDC else DC := GetDC(Handle);
  if RectVisible(DC, ItemRect(Index)) then begin
    SavedDC := SaveDC(DC);
    try
      Canvas.Lock;
      Canvas.Handle := DC;
      PaintItem;
      Canvas.Handle := 0;
      Canvas.Unlock;
    finally
      RestoreDC(DC, SavedDC);
      if acPrintDC = 0 then ReleaseDC(Handle, DC);
    end;
  end;
end;

procedure TsAlphaListBox.ResetContent;
begin
  if Style in [lbVirtual, lbVirtualOwnerDraw] then exit;
  SendMessage(Handle, LB_RESETCONTENT, 0, 0);
end;

procedure TsAlphaListBox.SetAutoHideScroll(const Value: boolean);
begin
  if FAutoHideScroll <> Value then begin
    FAutoHideScroll := Value;
    if not (csLoading in ComponentState) then RecreateWnd;
  end;
end;

procedure TsAlphaListBox.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then begin
    FBorderStyle := Value;
    if not (csLoading in ComponentState) then RecreateWnd;
  end;
end;

procedure TsAlphaListBox.SetColumns(Value: Integer);
begin
  if FColumns <> Value then begin
    if (FColumns = 0) or (Value = 0) then begin
      FColumns := Value;
    if not (csLoading in ComponentState) then RecreateWnd;
    end else begin
      FColumns := Value;
      if HandleAllocated then SetColumnWidth;
    end;
  end;
end;

procedure TsAlphaListBox.SetColumnWidth;
var
  ColWidth: Integer;
begin
  if (FColumns > 0) and (Width > 0) then begin
    ColWidth := (Width + FColumns - 3) div FColumns;
    if ColWidth < 1 then ColWidth := 1;
    SendMessage(Handle, LB_SETCOLUMNWIDTH, ColWidth, 0);
  end;
end;

procedure TsAlphaListBox.SetCount(const Value: Integer);
var
  Error: Integer;
begin
  if Style in [lbVirtual, lbVirtualOwnerDraw] then begin
    // Limited to 32767 on Win95/98 as per Win32 SDK
    Error := SendMessage(Handle, LB_SETCOUNT, Value, 0);
    if (Error <> LB_ERR) and (Error <> LB_ERRSPACE) then FCount := Value else raise Exception.CreateFmt(LoadStr(S_ErrorSettingCount), [Name]);
  end
  else raise Exception.CreateFmt(LoadStr(S_ListBoxMustBeVirtual), [Name]);
end;

procedure TsAlphaListBox.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FCommonData.Invalidate;
  end;
end;

procedure TsAlphaListBox.SetExtendedSelect(Value: Boolean);
begin
  if Value <> FExtendedSelect then begin
    FExtendedSelect := Value;
    if not (csLoading in ComponentState) then RecreateWnd;
  end;
end;

procedure TsAlphaListBox.SetIntegralHeight(Value: Boolean);
begin
  if Value <> FIntegralHeight then begin
    FIntegralHeight := Value;
    if not (csLoading in ComponentState) then RecreateWnd;
    RequestAlign;
  end;
end;

procedure TsAlphaListBox.SetItemData(Index, AData: Integer);
begin
  SendMessage(Handle, LB_SETITEMDATA, Index, AData);
end;

procedure TsAlphaListBox.SetItemHeight(Value: Integer);
begin
  if (FItemHeight <> Value) and (Value > 0) then begin
    FItemHeight := Value;
    if not (csLoading in ComponentState) then RecreateWnd;
  end;
end;

procedure TsAlphaListBox.SetItemIndex(Value: Integer);
var
  OldItem : Integer;
begin
  OldItem := ItemIndex;
  if GetItemIndex <> Value
    then if MultiSelect
      then SendMessage(Handle, LB_SETCARETINDEX, Value, 0)
      else SendMessage(Handle, LB_SETCURSEL, Value, 0);
  if OldItem > -1 then RepaintItem(OldItem);
end;

procedure TsAlphaListBox.SetItems(Value: TStrings);
begin
  if Style in [lbVirtual, lbVirtualOwnerDraw] then
    case Style of
      lbVirtual: Style := lbStandard;
      lbVirtualOwnerDraw: Style := lbOwnerDrawFixed;
    end;
  Items.Assign(Value);
end;

procedure TsAlphaListBox.SetMultiSelect(Value: Boolean);
begin
  if FMultiSelect <> Value then begin
    FMultiSelect := Value;
    RecreateWnd;
  end;
end;

procedure TsAlphaListBox.SetScrollWidth(const Value: Integer);
begin
  if Value <> ScrollWidth then
    SendMessage(Handle, LB_SETHORIZONTALEXTENT, Value, 0);
end;

procedure TsAlphaListBox.SetSelected(Index: Integer; Value: Boolean);
begin
  if FMultiSelect then begin
    if SendMessage(Handle, LB_SETSEL, Longint(Value), Index) = LB_ERR
      then raise EListError.CreateResFmt(@SListIndexError, [Index])
  end
  else begin
    ItemIndex := Index;
    Repaint;
  end;
end;

procedure TsAlphaListBox.SetSorted(Value: Boolean);
begin
  if Style in [lbVirtual, lbVirtualOwnerDraw] then exit;
  if FSorted <> Value then begin
    FSorted := Value;
    if not (csLoading in ComponentState) then RecreateWnd;
  end;
end;

procedure TsAlphaListBox.SetStyle(Value: TListBoxStyle);
begin
  if FStyle <> Value then begin
    if Value in [lbVirtual, lbVirtualOwnerDraw] then begin
      Items.Clear;
      Sorted := False;
    end;
    FStyle := Value;
    RecreateWnd;
  end;
end;

procedure TsAlphaListBox.SetTabWidth(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FTabWidth <> Value then begin
    FTabWidth := Value;
    if not (csLoading in ComponentState) then RecreateWnd;
  end;
end;

procedure TsAlphaListBox.SetTopIndex(Value: Integer);
begin
  if GetTopIndex <> Value then SendMessage(Handle, LB_SETTOPINDEX, Value, 0);
end;

function TsAlphaListBox.VisibleRows: integer;
begin
  Result := (Height - 6) div ItemHeight;
end;

procedure TsAlphaListBox.WMEraseBkgnd(var Message: TWMPaint);
//var
//  SavedDC : hdc;
begin
{  if (Message.DC <> 0) and FCommonData.Skinned then begin
    if (csDestroying in ComponentState) or (csLoading in ComponentState) or (ListSW = nil) or (FCommonData = nil) then Exit;
    FCommonData.Updating := FCommonData.Updating;
    if FCommonData.Updating then Exit;
    if FCommonData.BGChanged then PrepareCache;
    SavedDC := SaveDC(Message.DC);
    try
      CopyWinControlCache(Self, FCommonData, Rect(ListSW.cxLeftEdge, ListSW.cxLeftEdge, Width - ListSW.cxLeftEdge, Height - ListSW.cxLeftEdge), Rect(0, 0, Width - 2 * ListSW.cxLeftEdge, Height - 2 * ListSW.cxLeftEdge), Message.DC, True);
    finally
      RestoreDC(Message.DC, SavedDC);
    end;
    Message.Result := 1;
  end else}
  inherited;
end;

procedure TsAlphaListBox.WMLButtonDown(var Message: TWMLButtonDown);
var
  ItemNo : Integer;
  ShiftState: TShiftState;
begin
  mPressed := True;
  ShiftState := KeysToShiftState(Message.Keys);
  if (DragMode = dmAutomatic) and FMultiSelect then begin
    if not (ssShift in ShiftState) or (ssCtrl in ShiftState) then begin
      ItemNo := ItemAtPos(SmallPointToPoint(Message.Pos), True);
      if (ItemNo >= 0) and (Selected[ItemNo]) then begin
        BeginDrag (False);
        Exit;
      end;
    end;
  end;
  inherited;
  if (DragMode = dmAutomatic) and not (FMultiSelect and ((ssCtrl in ShiftState) or (ssShift in ShiftState))) then BeginDrag(False);
end;

procedure TsAlphaListBox.WMLButtonUp(var Message: TMessage);
begin
  mPressed := False;
  inherited;
end;

procedure TsAlphaListBox.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
end;

procedure TsAlphaListBox.WMPrint(var Message: TWMPaint);
const
  cxLeftEdge = 2;
var
  DC, SavedDC : hdc;
  i : integer;
begin
  if acPrintDC = 0 then inherited else begin
    DC := hdc(TWMPaint(Message).DC);
    SavedDC := SaveDC(DC);
    try

      MoveWindowOrg(DC, cxLeftEdge, cxLeftEdge);
      IntersectClipRect(DC, 0, 0,
                        SkinData.FCacheBmp.Width - 2 * cxLeftEdge - integer(ListSW.sBarVert.fScrollVisible) * GetScrollMetric(ListSW.sBarVert, SM_CXVERTSB),
                        SkinData.FCacheBmp.Height - 2 * cxLeftEdge - integer(ListSW.sBarHorz.fScrollVisible) * GetScrollMetric(ListSW.sBarHorz, SM_CYHORZSB));

      for i := 0 to Items.Count - 1 do RepaintItem(i);

    finally
      RestoreDC(DC, SavedDC);
    end;
    Message.Result := 1;
  end;
end;

procedure TsAlphaListBox.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_CTRLHANDLED : begin Message.Result := 1; Exit end; // AlphaSkins supported
    AC_GETAPPLICATION : begin Message.Result := longint(Application); Exit end;
    AC_REMOVESKIN : if LongWord(Message.LParam) = LongWord(SkinData.SkinManager) then begin
      if ListSW <> nil then FreeAndNil(ListSW);
      CommonWndProc(Message, FCommonData);
      if not FCommonData.CustomColor then Color := clWindow;
      if not FCommonData.CustomFont then Font.Color := clWindowText;
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE);
      exit
    end;
    AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FCommonData);
      RefreshEditScrolls(SkinData, ListSW);
      if FCommonData.Skinned then begin
        if not FCommonData.CustomColor then Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].Color;
        if not FCommonData.CustomFont then Font.Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].FontColor[1];
      end;
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE);
      exit
    end;
    AC_SETNEWSKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FCommonData);
      exit
    end;
    AC_ENDPARENTUPDATE : if FCommonData.Updating then begin
      FCommonData.Updating := False;
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE);
      Exit;
    end;
  end;
  if not ControlIsReady(Self) or not FCommonData.Skinned then inherited else begin
    CommonWndProc(Message, FCommonData);
    {for auto drag mode, let listbox handle itself, instead of TControl}
    if not (csDesigning in ComponentState) and ((Message.Msg = WM_LBUTTONDOWN) or (Message.Msg = WM_LBUTTONDBLCLK)) and not Dragging then begin
      if DragMode = dmAutomatic then begin
        if IsControlMouseMsg(TWMMouse(Message)) then Exit;
        ControlState := ControlState + [csLButtonDown];
        Dispatch(Message);
        Exit;
      end;
    end;
    case Message.Msg of
      WM_SIZE, WM_WINDOWPOSCHANGING : if not ((csLoading in ComponentState) or (csCreating in ControlState)) then begin
        FCommonData.BGChanged := True;
      end;
      WM_PRINTCLIENT, WM_PAINT : if inAnimationProcess then {inc(IntLevel);} Exit; // Patch for Vista
    end;
    inherited WndProc(Message);
    case Message.Msg of
      WM_WINDOWPOSCHANGING : begin
        if FCommonData.BGChanged then RedrawWindow(Handle, nil, 0, RDW_UPDATENOW or RDW_ERASENOW or RDW_ERASE or RDW_INVALIDATE);
      end;
//      WM_SETFOCUS, WM_KILLFOCUS : SkinData.Invalidate;
      CM_SHOWINGCHANGED : RefreshEditScrolls(SkinData, ListSW);
      WM_SIZE, CM_ENABLEDCHANGED : if not ((csLoading in ComponentState) or (csCreating in ControlState)) then begin
        SetColumnWidth;
      end;
      LB_SETCURSEL : UpdateScrolls(ListSW, True);
      CM_COLORCHANGED : begin
        if not FCommonData.CustomColor then Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].Color;
        if not FCommonData.CustomFont then Font.Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].FontColor[1];
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

{ TListBoxStrings }

function TListBoxStrings.GetCount: Integer;
begin
  Result := SendMessage(ListBox.Handle, LB_GETCOUNT, 0, 0);
end;

function TListBoxStrings.Get(Index: Integer): string;
var
  Len: Integer;
begin
  if ListBox.Style in [lbVirtual, lbVirtualOwnerDraw] then
    Result := ListBox.DoGetData(Index)
  else
  begin
    Len := SendMessage(ListBox.Handle, LB_GETTEXTLEN, Index, 0);
    if Len = LB_ERR then Error(SListIndexError, Index);
    SetLength(Result, Len);
    if Len <> 0 then
    begin
      Len := SendMessage(ListBox.Handle, LB_GETTEXT, Index, Longint(PChar(Result)));
      SetLength(Result, Len);  // LB_GETTEXTLEN isn't guaranteed to be accurate
    end;
  end;
end;

function TListBoxStrings.GetObject(Index: Integer): TObject;
begin
  if ListBox.Style in [lbVirtual, lbVirtualOwnerDraw] then
    Result := ListBox.DoGetDataObject(Index)
  else
  begin
    Result := TObject(ListBox.GetItemData(Index));
    if Longint(Result) = LB_ERR then Error(SListIndexError, Index);
  end;
end;

procedure TListBoxStrings.Put(Index: Integer; const S: string);
var
  I: Integer;
  TempData: Longint;
begin
  I := ListBox.ItemIndex;
  TempData := ListBox.InternalGetItemData(Index);
  // Set the Item to 0 in case it is an object that gets freed during Delete
  ListBox.InternalSetItemData(Index, 0);
  Delete(Index);
  InsertObject(Index, S, nil);
  ListBox.InternalSetItemData(Index, TempData);
  ListBox.ItemIndex := I;
end;

procedure TListBoxStrings.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index <> -1) and not (ListBox.Style in [lbVirtual, lbVirtualOwnerDraw]) then
    ListBox.SetItemData(Index, LongInt(AObject));
end;

function TListBoxStrings.Add(const S: string): Integer;
begin
  Result := -1;
  if ListBox.Style in [lbVirtual, lbVirtualOwnerDraw] then exit;
  Result := SendMessage(ListBox.Handle, LB_ADDSTRING, 0, Longint(PChar(S)));
  if Result < 0 then raise EOutOfResources.Create(SInsertLineError);
end;

procedure TListBoxStrings.Insert(Index: Integer; const S: string);
begin
  if ListBox.Style in [lbVirtual, lbVirtualOwnerDraw] then exit;
  if SendMessage(ListBox.Handle, LB_INSERTSTRING, Index,
    Longint(PChar(S))) < 0 then
    raise EOutOfResources.Create(SInsertLineError);
end;

procedure TListBoxStrings.Delete(Index: Integer);
begin
  ListBox.DeleteString(Index);
end;

procedure TListBoxStrings.Exchange(Index1, Index2: Integer);
var
  TempData: Longint;
  TempString: string;
begin
  if ListBox.Style in [lbVirtual, lbVirtualOwnerDraw] then exit;
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempData := ListBox.InternalGetItemData(Index1);
    Strings[Index1] := Strings[Index2];
    ListBox.InternalSetItemData(Index1, ListBox.InternalGetItemData(Index2));
    Strings[Index2] := TempString;
    ListBox.InternalSetItemData(Index2, TempData);
    if ListBox.ItemIndex = Index1 then
      ListBox.ItemIndex := Index2
    else if ListBox.ItemIndex = Index2 then
      ListBox.ItemIndex := Index1;
  finally
    EndUpdate;
  end;
end;

procedure TListBoxStrings.Clear;
begin
  ListBox.ResetContent;
end;

procedure TListBoxStrings.SetUpdateState(Updating: Boolean);
begin
//  SendMessage(ListBox.Handle, WM_SETREDRAW, Ord(not Updating), 0);
  if not Updating then begin ListBox.Refresh; Update; end;
end;

function TListBoxStrings.IndexOf(const S: string): Integer;
begin
  if ListBox.Style in [lbVirtual, lbVirtualOwnerDraw] then
    Result := ListBox.DoFindData(S)
  else
    Result := SendMessage(ListBox.Handle, LB_FINDSTRINGEXACT, -1, LongInt(PChar(S)));
end;

procedure TListBoxStrings.Move(CurIndex, NewIndex: Integer);
var
  TempData: Longint;
  TempString: string;
begin
  if ListBox.Style in [lbVirtual, lbVirtualOwnerDraw] then exit;
  BeginUpdate;
  ListBox.FMoving := True;
  try
    if CurIndex <> NewIndex then
    begin
      TempString := Get(CurIndex);
      TempData := ListBox.InternalGetItemData(CurIndex);
      ListBox.InternalSetItemData(CurIndex, 0);
      Delete(CurIndex);
      Insert(NewIndex, TempString);
      ListBox.InternalSetItemData(NewIndex, TempData);
    end;
  finally
    ListBox.FMoving := False;
    EndUpdate;
  end;
end;

procedure TListBoxStrings.Update;
begin
end;

end.
