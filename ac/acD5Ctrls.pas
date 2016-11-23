unit acD5Ctrls;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ToolWin, ExtCtrls, sConst, acntUtils, sGraphUtils{$IFNDEF DELPHI5}, types{$ENDIF},
  {sGlyphUtils,} math, sCommonData, ImgList, sDefaults, sComboBox, acSBUtils,
  sDialogs{$IFDEF LOGGED}, sDebugMsgs{$ENDIF};

Type
{$IFNDEF NOTFORHELP}
  TsCommonCombo = class;

  TsCustomComboBoxStrings = class(TStrings)
  private
    FComboBox: TsCommonCombo;
  protected
    function GetCount: Integer; override;
    function Get(Index: Integer): string; override;
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
    property ComboBox: TsCommonCombo read FComboBox write FComboBox;
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function IndexOf(const S: string): Integer; override;
  end;

  TsCustomComboBoxStringsClass = class of TsCustomComboBoxStrings;
  TsComboBoxStrings = class(TsCustomComboBoxStrings)
  public
    function Add(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
  end;

  TsCustomListControl = class(TWinControl)
  private
    FBoundLabel: TsBoundLabel;
  protected
    FCommonData: TsCommonData;
    function GetCount: Integer; virtual; abstract;
    function GetItemIndex: Integer; virtual; abstract;
    procedure SetItemIndex(const Value: Integer); overload; virtual; abstract;
    procedure Loaded; override;
  public
    procedure AfterConstruction; override;
    procedure AddItem(Item: String; AObject: TObject); virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure ClearSelection; virtual; abstract;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure WndProc (var Message: TMessage); override;

    procedure CopySelection(Destination: TsCustomListControl); virtual; abstract;
    procedure DeleteSelected; virtual; abstract;
    procedure MoveSelection(Destination: TsCustomListControl); virtual;
    procedure SelectAll; virtual; abstract;
    property SkinData : TsCommonData read FCommonData write FCommonData;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
  published
    property BoundLabel : TsBoundLabel read FBoundLabel write FBoundLabel;
  end;

  TsCommonCombo = class(TsCustomListControl)
  private
    FCanvas: TCanvas;
    FMaxLength: Integer;
    FDropDownCount: Integer;
    FItemIndex: Integer;
    FOnChange: TNotifyEvent;
    FOnSelect: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    FOnCloseUp: TNotifyEvent;
    FItemHeight: Integer;
    FItems: TStrings;
    FOnMeasureItem: TMeasureItemEvent;
    FShowButton: boolean;
    procedure WMCreate(var Message: TWMCreate); message WM_CREATE;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WMDrawItem(var Message: TWMDrawItem); message WM_DRAWITEM;
    procedure WMDeleteItem(var Message: TWMDeleteItem); message WM_DELETEITEM;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure MeasureItem(Index: Integer; var Height: Integer); virtual;
    procedure SetShowButton(const Value: boolean);
  protected
    lboxhandle : hwnd;
    ListSW : TacScrollWnd;
    //
    FListHandle: HWnd;
    FDropHandle: HWnd;
    FEditInstance: Pointer;
    FDefEditProc: Pointer;
    FListInstance: Pointer;
    FDefListProc: Pointer;
    FDroppingDown: Boolean;
    FFocusChanged: Boolean;
    FIsFocused: Boolean;
    FSaveIndex: Integer;
    procedure AdjustDropDown; virtual;
    procedure ComboWndProc(var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer); virtual;
    procedure EditWndProc(var Message: TMessage);
    function GetItemsClass: TsCustomComboBoxStringsClass; virtual; abstract;
    function GetItemHt: Integer; virtual; abstract;
    procedure SetItemHeight(Value: Integer); virtual;
    function GetCount: Integer; override;
    function GetItemCount: Integer; virtual; abstract;
    function GetItemIndex: Integer; override;
    function GetDroppedDown: Boolean;
    function GetSelLength: Integer;
    function GetSelStart: Integer;
    procedure CreateWnd; override;
    procedure ListWndProc(var Message: TMessage);
    procedure Change; dynamic;
    procedure Select; dynamic;
    procedure DropDown; dynamic;
    procedure CloseUp; dynamic;
    procedure DestroyWindowHandle; override;
    procedure SetDroppedDown(Value: Boolean);
    procedure SetSelLength(Value: Integer);
    procedure SetSelStart(Value: Integer);
    procedure SetMaxLength(Value: Integer);
    procedure SetDropDownCount(const Value: Integer); virtual;
    procedure SetItemIndex(const Value: Integer); override;
    procedure SetItems(const Value: TStrings); virtual;
    procedure UpdateMargins; virtual;
    procedure Loaded; override;
    property DropDownCount: Integer read FDropDownCount write SetDropDownCount default 14;
    property ItemCount: Integer read GetItemCount;
    property ItemHeight: Integer read GetItemHt write SetItemHeight default 16;
    property ListHandle: HWnd read FListHandle;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
    property ParentColor default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnMeasureItem: TMeasureItemEvent read FOnMeasureItem write FOnMeasureItem;
  public
    FEditHandle: HWnd;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WndProc(var Message: TMessage); override;
    procedure AddItem(Item: String; AObject: TObject); override;
    procedure Clear; override;
    procedure ClearSelection; override;
    procedure CopySelection(Destination: TsCustomListControl); override;
    procedure DeleteSelected; override;
    function Focused: Boolean; override;
    procedure PaintButton; virtual;
    function ButtonRect : TRect;
    procedure SelectAll; override;
    property EditHandle: HWnd read FEditHandle;
    property Canvas: TCanvas read FCanvas;
    property DroppedDown: Boolean read GetDroppedDown write SetDroppedDown;
    property Items: TStrings read FItems write SetItems;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property TabStop default True;
    property Height default 22;
    property ShowButton : boolean read FShowButton write SetShowButton default True;
  end;

  TsCommonComboBox = class(TsCommonCombo)
  private
    FAutoComplete: Boolean;
    FAutoDropDown: Boolean;
    FLastTime: Cardinal;
    FFilter: String;
    FCharCase: TEditCharCase;
    FSorted: Boolean;
    FStyle: TComboBoxStyle;
    FSaveItems: TStringList;
    FOnDrawItem: TDrawItemEvent;
    FOnMeasureItem: TMeasureItemEvent;
    FDisabledKind: TsDisabledKind;
    FReadOnly: boolean;
    procedure SetCharCase(Value: TEditCharCase);
    procedure SetSelText(const Value: string);
    procedure SetSorted(Value: Boolean);
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
//    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MeasureItem;
//    procedure WMFontChange(var Message: TMessage); message WM_SETFONT;
    procedure WMLButtonDblClk(var Message: TMessage); message WM_LBUTTONDBLCLK;
    procedure SkinPaint(DC : HDC); virtual;
    procedure SetDisabledKind(const Value: TsDisabledKind);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); virtual;
    procedure DrawSkinItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); virtual;
    function  GetItemHt: Integer; override;
    function GetItemsClass: TsCustomComboBoxStringsClass; override;
    function GetSelText: string;
    procedure KeyPress(var Key: Char); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    function SelectItem(const AnItem: String): Boolean;
    procedure SetStyle(Value: TComboBoxStyle); virtual;
    property Sorted: Boolean read FSorted write SetSorted default False;
    property OnDrawItem: TDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnMeasureItem: TMeasureItemEvent read FOnMeasureItem write FOnMeasureItem;
    function GetItemCount: Integer; override;
  public
    property Align;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WndProc(var Message: TMessage); override;
    property Style: TComboBoxStyle read FStyle write SetStyle default csDropDown;
    property AutoComplete: Boolean read FAutoComplete write FAutoComplete default True;
    property AutoDropDown: Boolean read FAutoDropDown write FAutoDropDown default False;
    property CharCase: TEditCharCase read FCharCase write SetCharCase default ecNormal;
    property SelText: string read GetSelText write SetSelText;
    property DisabledKind : TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
  published
    property ReadOnly : boolean read FReadOnly write FReadOnly default False;
  end;

  TsColorBoxStyles = (cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors);
  TsColorBoxStyle = set of TsColorBoxStyles;
{$ENDIF} // NOTFORHELP

{$IFNDEF NOTFORHELP}
  TsCustomColorBox = class;
  TGetColorsEvent = procedure(Sender: TsCustomColorBox; Items: TStrings) of object;
  TsCustomColorBox = class(TsCommonComboBox)
  private
    FStyle: TsColorBoxStyle;
    FNeedToPopulate: Boolean;
    FListSelected: Boolean;
    FDefaultColorColor: TColor;
    FNoneColorColor: TColor;
    FSelectedColor: TColor;
    FShowColorName: boolean;
    FMargin: integer;
    FOnGetColors: TGetColorsEvent;
    function GetColor(Index: Integer): TColor;
    function GetColorName(Index: Integer): string;
    function GetSelected: TColor;
    procedure SetSelected(const AColor: TColor);
    procedure ColorCallBack(const AName: string);
    procedure SetDefaultColorColor(const Value: TColor);
    procedure SetNoneColorColor(const Value: TColor);
    procedure SetShowColorName(const Value: boolean);
    procedure SetMargin(const Value: integer);
  protected
    procedure CloseUp; override;
    function ColorRect(SourceRect : TRect; State: TOwnerDrawState) : TRect;
    procedure CreateWnd; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure DrawSkinItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function PickCustomColor: Boolean; virtual;
    procedure PopulateList;
    procedure Select; override;
    procedure SetStyle(AStyle: TsColorBoxStyle); reintroduce;
  public
    constructor Create(AOwner: TComponent); override;
    property ColorNames[Index: Integer]: string read GetColorName;
    property Colors[Index: Integer]: TColor read GetColor;
  published
    property Style: TsColorBoxStyle read FStyle write SetStyle default [cbStandardColors, cbExtendedColors, cbSystemColors];
    property Margin : integer read FMargin write SetMargin default 0;
    property Selected: TColor read GetSelected write SetSelected default clBlack;
    property ShowColorName : boolean read FShowColorName write SetShowColorName default True;
    property DefaultColorColor: TColor read FDefaultColorColor write SetDefaultColorColor default clBlack;
    property NoneColorColor: TColor read FNoneColorColor write SetNoneColorColor default clBlack;
    property OnGetColors: TGetColorsEvent read FOnGetColors write FOnGetColors;
  end;
{$ENDIF} // NOTFORHELP
(*
  TsColorBox = class(TsCustomColorBox)
{$IFNDEF NOTFORHELP}
  published
    property AutoComplete;
    property AutoDropDown;
    property DisabledKind;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
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
    property OnChange;
    property OnCloseUp;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseUp;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
{$ENDIF} // NOTFORHELP
    property Style;
    property Margin;
    property Selected;
    property ShowColorName;
    property DefaultColorColor;
    property NoneColorColor;
    property SkinData;
  end;
*)
{$IFNDEF NOTFORHELP}
  TsCustomComboBoxEx = class;
  TsComboItem = class;

  TsComboItems = class(TCollection)
  private
    FOwner: TsCustomComboBoxEx;
    function GetItem(Index: Integer): TsComboItem;
    procedure SetItem(Index: Integer; Value: TsComboItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    function Add: TsComboItem;
    function AddItem(const Caption: String; const ImageIndex, SelectedImageIndex, OverlayImageIndex, Indent: Integer; Data: Pointer): TsComboItem;
    constructor Create(AOwner: TsCustomComboBoxEx);
    destructor Destroy; override;
    property Items[Index: Integer]: TsComboItem read GetItem write SetItem; default;
  end;

  TsComboItem = class(TCollectionItem)
  private
    FCaption: string;
    FSelectedImageIndex: TImageIndex;
    FImageIndex: TImageIndex;
    FOverlayImageIndex: TImageIndex;
    FIndend: integer;
    FData: Pointer;
    procedure SetCaption(const Value: string);
    procedure SetData(const Value: Pointer);
  public
    property Data: Pointer read FData write SetData;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetDisplayName: string; override;
  published
    property Caption : string read FCaption write SetCaption;
    property ImageIndex : TImageIndex read FImageIndex write FImageIndex default -1;
    property Indend : integer read FIndend write FIndend default -1;
    property OverlayImageIndex : TImageIndex read FOverlayImageIndex write FOverlayImageIndex default -1;
    property SelectedImageIndex : TImageIndex read FSelectedImageIndex write FSelectedImageIndex default -1;
  end;
{$ENDIF} // NOTFORHELP

{$IFNDEF NOTFORHELP}
  TsCustomComboBoxEx = class(TsCommonComboBox)
  private
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FItemsEx: TsComboItems;
    NeedToUpdate : boolean;
    procedure ImageListChange(Sender: TObject);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetItemsEx(const Value: TsComboItems);
    function GetSelectedItem: TsComboItem;
  protected
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure DrawSkinItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    function ImgRect(Item : TsComboItem; State: TOwnerDrawState) : TRect;
    function CurrentImage(Item : TsComboItem; State: TOwnerDrawState) : integer;
  public
    procedure Clear; override;
    procedure UpdateList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateMargins; override;
    property Images : TCustomImageList read FImages write SetImages;
    property ItemsEx : TsComboItems read FItemsEx write SetItemsEx;
    property SelectedItem : TsComboItem read GetSelectedItem;
  end;
{$ENDIF} // NOTFORHELP
(*
  TsComboBoxEx = class(TsCustomComboBoxEx)
{$IFNDEF NOTFORHELP}
  public
    property SelectedItem;
  published
    property Action;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DisabledKind;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
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

    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property DropDownCount;

    property OnChange;
    property OnCloseUp;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;

    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
{$ENDIF} // NOTFORHELP
    property MaxLength;
    property Images;
    property ItemsEx;
    property SkinData;
    property Text;
  end;
*)

implementation

uses sStyleSimply, sMaskData, sSkinProps, sVclUtils, Consts, sMessages, sBorders, commctrl, sAlphaGraph, sThirdParty, sSKinManager, sComboBoxes;

const
  StandardColorsCount = 16;
  ExtendedColorsCount = 4;
  NoColorSelected = TColor($FF000000);

type
  TSelection = record
    StartPos, EndPos: Integer;
  end;

function HasPopup(Control: TControl): Boolean;
begin
  Result := True;
  while Control <> nil do
    if TsHackedControl(Control).PopupMenu <> nil then Exit else Control := Control.Parent;
  Result := False;
end;

{ TsCustomListControl }

procedure TsCustomListControl.AfterConstruction;
begin
  inherited;
  SkinData.Loaded;
end;

constructor TsCustomListControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommonData := TsCommonData.Create(Self, True);
  FCommonData.COC := COC_TsCustom;
  if FCommonData.SkinSection = '' then FCommonData.SkinSection := s_ComboBOx;
  FBoundLabel := TsBoundLabel.Create(Self, FCommonData);
end;

destructor TsCustomListControl.Destroy;
begin
  FreeAndNil(FBoundLabel);
  if Assigned(FCommonData) then FreeAndNil(FCommonData);
  inherited Destroy;
end;

procedure TsCustomListControl.Loaded;
begin
  inherited;
  SkinData.Loaded;
  if FCommonData.Skinned then begin
    if not FCommonData.CustomColor then Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].Color;
    if not FCommonData.CustomFont then Font.Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].FontColor[1];
  end;
end;

procedure TsCustomListControl.MoveSelection(Destination: TsCustomListControl);
begin
  CopySelection(Destination);
  DeleteSelected;
end;

procedure TsCustomListControl.WndProc(var Message: TMessage);
var
  DC : hdc;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_CTRLHANDLED : begin Message.Result := 1; Exit end; // AlphaSkins supported
    AC_GETAPPLICATION : begin Message.Result := longint(Application); Exit end;
    AC_REMOVESKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FCommonData);
      if not FCommonData.CustomColor then Color := clWindow;
      if not FCommonData.CustomFont then Font.Color := clWindowText;
      exit
    end;
    AC_SETNEWSKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FCommonData);
//      Repaint;
      exit
    end;
    AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FCommonData);
      if FCommonData.Skinned then begin
        if not FCommonData.CustomColor then Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].Color;
        if not FCommonData.CustomFont then Font.Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].FontColor[1];
      end;
      Repaint;
      exit
    end;
    AC_ENDPARENTUPDATE : if FCommonData.Updating then begin
      FCommonData.Updating := False;
      Repaint;
      Exit
    End; 
  end;
  if not ControlIsReady(Self) then inherited else begin
    if Assigned(FCommonData) then begin
      if CommonWndProc(Message, FCommonData) then Exit;
      if FCommonData.Skinned then case Message.Msg of
        WM_NCCALCSIZE, WM_WINDOWPOSCHANGED, CM_VISIBLECHANGED, WM_SIZE, CM_ENABLEDCHANGED, WM_MOUSEWHEEL, WM_MOVE : begin
          FCommonData.BGChanged := True;
        end;
        WM_VSCROLL : begin
          exit;
        end;
        WM_PRINT : begin
          try
            DC := TWMPaint(Message).DC;
            SkinData.FUpdating := False;
            if SkinData.BGChanged then PrepareCache(SkinData, Handle);
            UpdateCorners(SkinData, 0);
            BitBlt(DC, 0, 0, SkinData.FCacheBmp.Width, SkinData.FCacheBmp.Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
            SendMessage(Handle, WM_PAINT, longint(DC), 0);
            Exit;
          except
          end;
        end;
      end;
    end;
    inherited;
    if FCommonData.Skinned then case Message.Msg of
      WM_WINDOWPOSCHANGING, WM_WINDOWPOSCHANGED, CM_VISIBLECHANGED, WM_SIZE, CM_ENABLEDCHANGED, WM_MOUSEWHEEL, WM_MOVE : begin
  //        Repaint;
        SendMessage(Handle, WM_NCPAINT, 0, 0);
      end;
      WM_SETFOCUS, CM_ENTER, WM_KILLFOCUS, CM_EXIT: begin
        FCommonData.FFocused := (Message.Msg = CM_ENTER) or (Message.Msg = WM_SETFOCUS);
        FCommonData.FMouseAbove := False;
        FCommonData.BGChanged := True;
        Repaint;
        SendMessage(Handle, WM_NCPAINT, 0, 0);
      end;
      CM_MOUSELEAVE, CM_MOUSEENTER : begin
        if not FCommonData.FFocused and not(csDesigning in ComponentState) then begin
          FCommonData.FMouseAbove := Message.Msg = CM_MOUSEENTER;
          FCommonData.BGChanged := True;
          SendMessage(Handle, WM_NCPAINT, 0, 0);
          Repaint;
        end;
      end;
      WM_SETFONT : begin
        FCommonData.BGChanged := True;
        SendMessage(Handle, WM_NCPAINT, 0, 0);
        Repaint;
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

{ TsCommonCombo }

procedure TsCommonCombo.AddItem(Item: String; AObject: TObject);
begin
  Items.AddObject(Item, AObject);
end;

procedure TsCommonCombo.AdjustDropDown;
var
  Count, h: Integer;
begin
  Count := ItemCount;
  if Count > DropDownCount then Count := DropDownCount;
  if Count < 1 then Count := 1;
  FDroppingDown := True;
  try
    h := ItemHeight * Count + Height + 2;
    SetWindowPos(FDropHandle, 0, 0, 0, Width, h, SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_NOREDRAW or SWP_HIDEWINDOW);
  finally
    FDroppingDown := False;
  end;
  SetWindowPos(FDropHandle, 0, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_NOREDRAW or SWP_SHOWWINDOW);
end;

function TsCommonCombo.ButtonRect: TRect;
const
  bWidth = 3;
var
  w : integer;
begin
  if FShowButton then w := GetSystemMetrics(SM_CXVSCROLL) + 1 else w := 0;
  if UseRightToLeftAlignment then Result.Left := 2 else Result.Left := Width - w - 2;
  Result.Top := 2;
  Result.Right := Result.Left + w;
  Result.Bottom := Height - 2;
//  if UseRightToLeftAlignment then Result := Rect(bWidth, bWidth, GetSystemMetrics(SM_CXVSCROLL) + bWidth, Height - bWidth) else Result := Rect(Width - GetSystemMetrics(SM_CXVSCROLL) - bWidth, bWidth, Width - bWidth, Height - bWidth);
end;

procedure TsCommonCombo.Change;
var
  R : TRect;
begin
  if csLoading in ComponentState then Exit;
  inherited Changed;
  UpdateMargins;
  if Assigned(FCommonData.SkinManager) and FCommonData.SkinManager.IsValidSkinIndex(FCommonData.SkinIndex) then begin
    R := Classes.Rect(3, 3, Width - 3, Height - 3);
    InvalidateRect(Handle, @R, False);
  end;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TsCommonCombo.Clear;
begin
  SetTextBuf('');
  FItems.Clear;
  FSaveIndex := -1;
end;

procedure TsCommonCombo.ClearSelection;
begin
  ItemIndex := -1;
end;

procedure TsCommonCombo.CloseUp;
begin
  if Assigned(FOnCloseUp) then FOnCloseUp(Self);
end;

procedure TsCommonCombo.CMCancelMode(var Message: TCMCancelMode);
begin
  if Message.Sender <> Self then Perform(CB_SHOWDROPDOWN, 0, 0);
end;

procedure TsCommonCombo.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls then RecreateWnd;
  inherited;
end;

procedure TsCommonCombo.CNCommand(var Message: TWMCommand);
begin
  case Message.NotifyCode of
    CBN_DBLCLK : DblClick;
    CBN_EDITCHANGE : Change;
    CBN_DROPDOWN: begin
      FFocusChanged := False;
      DropDown;
      AdjustDropDown;
      if FFocusChanged then begin
        PostMessage(Handle, WM_CANCELMODE, 0, 0);
        if not FIsFocused then PostMessage(Handle, CB_SHOWDROPDOWN, 0, 0);
      end;
    end;
    CBN_SELCHANGE: begin
      Text := Items[ItemIndex];
      try
        Click;
      except
      end;
      Select;
    end;
    CBN_CLOSEUP: begin
      FCommonData.BGChanged := True;
      Repaint;
      CloseUp;
    end;
    CBN_SETFOCUS : begin
      FIsFocused := True;
      FCommonData.FFocused := True;
      FFocusChanged := True;
      SetIme;
    end;
    CBN_KILLFOCUS : begin
      FIsFocused := False;
      FCommonData.FFocused := False;
      FFocusChanged := True;
      ResetIme;
    end;
  end;
end;

procedure TsCommonCombo.CNMeasureItem(var Message: TWMMeasureItem);
begin
  with Message.MeasureItemStruct^ do begin
    itemHeight := FItemHeight;
//    if FStyle = csOwnerDrawVariable then
//      MeasureItem(itemID, Integer(itemHeight));
  end;
end;

procedure TsCommonCombo.ComboWndProc(var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer);
var
  Point: TPoint;
  Form: TCustomForm;
begin
  try
    with Message do begin
      case Msg of
        WM_SETFOCUS : begin
          Form := GetParentForm(Self);
          if (Form <> nil) and not Form.SetFocusedControl(Self) then Exit;
        end;
        WM_KILLFOCUS :
          if csFocusing in ControlState then Exit;
        WM_KEYDOWN, WM_SYSKEYDOWN:
          if (ComboWnd <> FListHandle) and DoKeyDown(TWMKey(Message)) then Exit;
        WM_CHAR : begin
          if DoKeyPress(TWMKey(Message)) then Exit;
          if ((TWMKey(Message).CharCode = VK_RETURN) or
               (TWMKey(Message).CharCode = VK_ESCAPE)) and DroppedDown then begin
            DroppedDown := False;
            Exit;
          end;
        end;
        WM_KEYUP, WM_SYSKEYUP : if DoKeyUp(TWMKey(Message)) then Exit;
        WM_MOUSEMOVE : Application.HintMouseMessage(Self, Message);
        WM_RBUTTONUP : if HasPopup(Self) then begin
          with TWMRButtonUp(Message) do begin
            Point.X := Pos.X;
            Point.Y := Pos.Y;
            MapWindowPoints(ComboWnd, Handle, Point, 1);
            Pos.X := Point.X;
            Pos.Y := Point.Y;
          end;
          WndProc(Message);
          Exit;
        end;
        WM_GETDLGCODE : if DroppedDown then begin
          Result := DLGC_WANTALLKEYS;
          Exit;
        end;
        WM_NCHITTEST : if csDesigning in ComponentState then begin
          Result := HTTRANSPARENT;
          Exit;
        end;
        CN_KEYDOWN, CN_CHAR, CN_SYSKEYDOWN, CN_SYSCHAR : begin
          WndProc(Message);
          Exit;
        end;
      end;
      Result := CallWindowProc(ComboProc, ComboWnd, Msg, WParam, LParam);
      if (Msg = WM_LBUTTONDBLCLK) and (csDoubleClicks in ControlStyle) then DblClick;
    end;
  except
    Application.HandleException(Self);
  end;
end;

procedure TsCommonCombo.CopySelection(Destination: TsCustomListControl);
begin
  if ItemIndex <> -1 then Destination.AddItem(Items[ItemIndex], Items.Objects[ItemIndex]);
end;

constructor TsCommonCombo.Create(AOwner: TComponent);
const
  ComboBoxStyle = [csCaptureMouse, csSetCaption, csDoubleClicks, csFixedHeight, csReflector, csOpaque];
begin
  inherited Create(AOwner);

  if NewStyleControls then ControlStyle := ComboBoxStyle else ControlStyle := ComboBoxStyle + [csFramed];

  Width := 145;
  TabStop := True;
  ParentColor := False;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FItemHeight := 16;
  FEditInstance := {$IFDEF D2007}Classes.{$ENDIF}MakeObjectInstance(EditWndProc);
  FListInstance := {$IFDEF D2007}Classes.{$ENDIF}MakeObjectInstance(ListWndProc);
  FDropDownCount := 14;
  FItemIndex := -1;
  FSaveIndex := -1;
  FShowButton := True;
  Height := 22;
end;

procedure TsCommonCombo.CreateWnd;
begin
  inherited CreateWnd;
  SendMessage(Handle, CB_LIMITTEXT, FMaxLength, 0);
  FEditHandle := 0;
  FListHandle := 0;
end;

procedure TsCommonCombo.DeleteSelected;
begin
  if ItemIndex <> -1 then Items.Delete(ItemIndex);
end;

destructor TsCommonCombo.Destroy;
begin
  if lBoxHandle <> 0 then begin
    UninitializeACScroll(lBoxHandle, True, False, ListSW);
    lBoxHandle := 0;
  end;
  if HandleAllocated then DestroyWindowHandle;
  {$IFDEF D2007}Classes.{$ENDIF}FreeObjectInstance(FListInstance);
  {$IFDEF D2007}Classes.{$ENDIF}FreeObjectInstance(FEditInstance);
  FCanvas.Free;
  inherited Destroy;
end;

procedure TsCommonCombo.DestroyWindowHandle;
begin
  inherited DestroyWindowHandle;
  {
    must be cleared after the main handle is destroyed as messages are sent
    to our internal WndProcs when the main handle is destroyed and we should not
    have NULL handles when we receive those messages.
  }
  FEditHandle := 0;
  FListHandle := 0;
  FDropHandle := 0;
end;

procedure TsCommonCombo.DropDown;
begin
  if Assigned(FOnDropDown) then FOnDropDown(Self);
end;

procedure TsCommonCombo.EditWndProc(var Message: TMessage);
var
  P: TPoint;
  Form: TCustomForm;
begin
  if Message.Msg = WM_SYSCOMMAND then begin
    WndProc(Message);
    Exit;
  end
  else if (Message.Msg >= WM_KEYFIRST) and (Message.Msg <= WM_KEYLAST) then begin
    Form := GetParentForm(Self);
    if (Form <> nil) and Form.WantChildKey(Self, Message) then Exit;
  end;
  ComboWndProc(Message, FEditHandle, FDefEditProc);
  case Message.Msg of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK : begin
      if DragMode = dmAutomatic then begin
        GetCursorPos(P);
        P := ScreenToClient(P);
        SendMessage(FEditHandle, WM_LBUTTONUP, 0,Longint(PointToSmallPoint(P)));
        BeginDrag(False);
      end;
    end;

    WM_SETFONT : if NewStyleControls then begin
      SendMessage(FEditHandle, EM_SETMARGINS, EC_LEFTMARGIN or EC_RIGHTMARGIN, 0);
    end;
  end;
end;

function TsCommonCombo.Focused: Boolean;
var
  FocusedWnd: HWND;
begin
  Result := False;
  if HandleAllocated then begin
    FocusedWnd := GetFocus;
    Result := (FocusedWnd <> 0) and ((FocusedWnd = FEditHandle) or (FocusedWnd = FListHandle));
  end;
end;

function TsCommonCombo.GetCount: Integer;
begin
  Result := GetItemCount;
end;

function TsCommonCombo.GetDroppedDown: Boolean;
begin
  Result := LongBool(SendMessage(Handle, CB_GETDROPPEDSTATE, 0, 0));
end;

function TsCommonCombo.GetItemIndex: Integer;
begin
  if csLoading in ComponentState then begin
    Result := FItemIndex
  end
  else begin
    Result := SendMessage(Handle, CB_GETCURSEL, 0, 0);
  end;
end;

function TsCommonCombo.GetSelLength: Integer;
var
  Selection: TSelection;
begin
  SendMessage(Handle, CB_GETEDITSEL, Longint(@Selection.StartPos), Longint(@Selection.EndPos));
  Result := Selection.EndPos - Selection.StartPos;
end;

function TsCommonCombo.GetSelStart: Integer;
begin
  SendMessage(Handle, CB_GETEDITSEL, Longint(@Result), 0);
end;

procedure TsCommonCombo.ListWndProc(var Message: TMessage);
begin
  ComboWndProc(Message, FListHandle, FDefListProc);
end;

procedure TsCommonCombo.Loaded;
begin
  inherited Loaded;
  if FItemIndex <> -1 then SetItemIndex(FItemIndex);
end;

procedure TsCommonCombo.MeasureItem(Index: Integer; var Height: Integer);
begin
  if Assigned(FOnMeasureItem) then FOnMeasureItem(Self, Index, Height)
end;

procedure TsCommonCombo.PaintButton;
var
  R : TRect;
  Mode : integer;
  c : TsColor;
  glIndex : integer;
  ButtonHeight : integer;
  TmpBtn : TBitmap;
begin
  if DroppedDown then Mode := 2 else if ControlIsActive(FCommonData) then Mode := 1 else Mode := 0;
  R := ButtonRect;

  if FCommonData.SkinManager.ConstData.ComboBtnIndex > -1 then begin
    TmpBtn := CreateBmpLike(FCommonData.FCacheBmp);
    BitBlt(TmpBtn.Canvas.Handle, 0, 0, TmpBtn.Width, TmpBtn.Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
    PaintItem(FCommonData.SkinManager.ConstData.ComboBtnIndex, s_ComboBtn, MakeCacheInfo(FCommonData.FCacheBmp),
      True, Mode, R, Point(0, 0), FCommonData.FCacheBmp, FCommonData.SkinManager, FCommonData.SkinManager.ConstData.ComboBtnBG, FCommonData.SkinManager.ConstData.ComboBtnBGHot);
    FreeAndNil(TmpBtn);
  end;
  glIndex := FCommonData.SkinManager.ConstData.ComboGlyph;
  if glIndex > -1 then begin
    if ControlIsActive(FCommonData)
      then c.C := FCommonData.SkinManager.gd[FCommonData.SkinIndex].HotColor
      else c.C := FCommonData.SkinManager.gd[FCommonData.SkinIndex].Color;

    ButtonHeight := HeightOf(FCommonData.SkinManager.ma[glIndex].R) div (1 + FCommonData.SkinManager.ma[glIndex].MaskType);

    DrawSkinGlyph(FCommonData.FCacheBmp,
      Point(R.Left + (WidthOf(R) - WidthOf(FCommonData.SkinManager.ma[glIndex].R) div FCommonData.SkinManager.ma[glIndex].ImageCount) div 2,
            (Height - ButtonHeight) div 2), Mode, 1, FCommonData.SkinManager.ma[FCommonData.SkinManager.ConstData.ComboGlyph], MakeCacheInfo(SkinData.FCacheBmp));
  end;
end;

procedure TsCommonCombo.Select;
begin
  if Assigned(FOnSelect) then begin
    FOnSelect(Self)
  end
  else begin
    Change;
  end;
end;

procedure TsCommonCombo.SelectAll;
begin
  SendMessage(Handle, CB_SETEDITSEL, 0, Integer($FFFF0000));
end;

procedure TsCommonCombo.SetDropDownCount(const Value: Integer);
begin
  FDropDownCount := Value;
end;

procedure TsCommonCombo.SetDroppedDown(Value: Boolean);
var
  R: TRect;
begin
  SendMessage(Handle, CB_SHOWDROPDOWN, Longint(Value), 0);
  R := ClientRect;
  InvalidateRect(Handle, @R, True);
end;

procedure TsCommonCombo.SetItemHeight(Value: Integer);
begin
  if Value > 0 then begin
    FItemHeight := Value;
    if HandleAllocated then SendMessage(Handle, CB_SETITEMHEIGHT, 0, Value);
    RecreateWnd;
  end;
end;

procedure TsCommonCombo.SetItemIndex(const Value: Integer);
begin
  if csLoading in ComponentState then
    FItemIndex := Value
  else
    if GetItemIndex <> Value then begin
      SendMessage(Handle, CB_SETCURSEL, Value, 0);
      if Assigned(FCommonData.SkinManager) and FCommonData.SkinManager.IsValidSkinIndex(FCommonData.SkinIndex) then Repaint;
    end;
end;

procedure TsCommonCombo.SetItems(const Value: TStrings);
begin
  if Assigned(FItems) then
    FItems.Assign(Value)
  else
    FItems := Value;
end;

procedure TsCommonCombo.SetMaxLength(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FMaxLength <> Value then begin
    FMaxLength := Value;
    if HandleAllocated then SendMessage(Handle, CB_LIMITTEXT, Value, 0);
  end;
end;

procedure TsCommonCombo.SetSelLength(Value: Integer);
var
  Selection: TSelection;
begin
  SendMessage(Handle, CB_GETEDITSEL, Longint(@Selection.StartPos), Longint(@Selection.EndPos));
  Selection.EndPos := Selection.StartPos + Value;
  SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam(Word(Selection.StartPos), Word(Selection.EndPos)));
end;

procedure TsCommonCombo.SetSelStart(Value: Integer);
var
  Selection: TSelection;
begin
  Selection.StartPos := Value;
  Selection.EndPos := Value;
  SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam(Word(Selection.StartPos), Word(Selection.EndPos)));
end;

procedure TsCommonCombo.SetShowButton(const Value: boolean);
begin
  if FShowButton <> Value then begin
    FShowButton := Value;
    FCommonData.Invalidate;
  end;
end;

procedure TsCommonCombo.UpdateMargins;
begin
end;

procedure TsCommonCombo.WMCreate(var Message: TWMCreate);
begin
  inherited;
  if WindowText <> nil then SetWindowText(Handle, WindowText);
end;

procedure TsCommonCombo.WMDeleteItem(var Message: TWMDeleteItem);
begin
  DefaultHandler(Message);
end;

procedure TsCommonCombo.WMDrawItem(var Message: TWMDrawItem);
begin
  DefaultHandler(Message);
end;

procedure TsCommonCombo.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  if DroppedDown then Message.Result := Message.Result or DLGC_WANTALLKEYS;
end;

procedure TsCommonCombo.WndProc(var Message: TMessage);
var
  h : hdc;
begin
    {for auto drag mode, let listbox handle itself, instead of TControl}
  if not (csDesigning in ComponentState) and
       ((Message.Msg = WM_LBUTTONDOWN) or (Message.Msg = WM_LBUTTONDBLCLK)) and
         not Dragging then begin
    if DragMode = dmAutomatic then begin
      if IsControlMouseMsg(TWMMouse(Message)) then Exit;
      ControlState := ControlState + [csLButtonDown];
      Dispatch(Message);  {overrides TControl's BeginDrag}
      Exit;
    end;
  end;
  with Message do begin
    case Msg of
      WM_SIZE : begin
        if FDroppingDown then begin
          DefaultHandler(Message);
          Exit;
        end;
      end;
      WM_CTLCOLORMSGBOX..WM_CTLCOLORSTATIC : begin
        {$R-}
        h := hdc(WParam);
        {$R+}
        if (Message.Msg = WM_CTLCOLORLISTBOX) and SkinData.Skinned and not (csLoading in ComponentState) and (lBoxHandle = 0) then begin
          if Items.Count > DropDownCount then begin
            lBoxHandle := hwnd(Message.LParam);
//            SetWindowLong(lBoxHandle, GWL_STYLE, GetWindowLong(lBoxHandle, GWL_STYLE) and not WS_BORDER or WS_THICKFRAME);
            ListSW := TacComboListWnd.CreateEx(lboxhandle, nil, SkinData.SkinManager, s_Edit, True, False);
          end;
        end;
        SetTextColor(h, ColorToRGB(Font.Color));
        SetBkColor(h, ColorToRGB(Brush.Color));
        Result := integer(Brush.Handle);
        Exit;
      end;
      WM_CHAR : begin
        if DoKeyPress(TWMKey(Message)) then Exit;
        if ((TWMKey(Message).CharCode = VK_RETURN) or (TWMKey(Message).CharCode = VK_ESCAPE)) and DroppedDown then begin
          DroppedDown := False;
          Exit;
        end;
      end;
    end;
  end;
  inherited WndProc(Message);
  case Message.Msg of
    CN_COMMAND : case TWMCommand(Message).NotifyCode of
      CBN_CLOSEUP : begin
        if ListSW <> nil then ListSW.SkinData.BGChanged := True;
      end;
    end;
  end;
end;

{ TsCommonComboBox }

procedure TsCommonComboBox.CMParentColorChanged(var Message: TMessage);
begin
  inherited;
  if not NewStyleControls and (Style < csDropDownList) then Invalidate;
end;

procedure TsCommonComboBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
  ds : TDrawItemStruct;
begin
  ds := Message.DrawItemStruct^;
  if ds.hDC = 0 then Exit;
  State := TOwnerDrawState(LongRec(ds.itemState).Lo);
  FCanvas.Handle := ds.hDC;
  FCanvas.Font := Font;
  FCanvas.Brush := Brush;
  if (ds.itemState and ODS_DEFAULT) <> 0 then Include(State, odDefault);

  if FCommonData.Skinned then begin
    if ds.itemState and ODS_COMBOBOXEDIT <> 0 then begin
      Exit;
      Include(State, odComboBoxEdit);
    end;
    if Integer(ds.itemID) >= 0 then DrawSkinItem(ds.itemID, ds.rcItem, State)
  end
  else begin
    if ds.itemState and ODS_COMBOBOXEDIT <> 0 then Include(State, odComboBoxEdit);
    if (Integer(ds.itemID) >= 0) and (odSelected in State) then begin
      FCanvas.Brush.Color := clHighlight;
      FCanvas.Font.Color := clHighlightText
    end
    else begin
      FCanvas.Brush.Color := Color;
      FCanvas.Font.Color := Font.Color;
    end;
    if Integer(ds.itemID) >= 0
      then DrawItem(ds.itemID, ds.rcItem, State)
      else FCanvas.FillRect(ds.rcItem);
    if odFocused in State then DrawFocusRect(ds.hDC, ds.rcItem);
  end;
  FCanvas.Handle := 0;
end;


{procedure TsCommonComboBox.CNMeasureItem(var Message: TWMMeasureItem);
var
  mi : TMeasureItemStruct;
begin
  mi := Message.MeasureItemStruct^;
  mi.itemHeight := FItemHeight;
  if FStyle = csOwnerDrawVariable then MeasureItem(mi.itemID, Integer(mi.itemHeight));
end;}

constructor TsCommonComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TsComboBoxStrings.Create;
  TsComboBoxStrings(FItems).ComboBox := Self;
  FStyle := csDropDown;
  FLastTime := 0;
  FAutoComplete := True;
  FDisabledKind := DefDisabledKind;
  FReadOnly := False;
end;

procedure TsCommonComboBox.CreateParams(var Params: TCreateParams);
const
  ComboBoxStyles: array[TComboBoxStyle] of DWORD = (CBS_DROPDOWN, CBS_SIMPLE, CBS_DROPDOWNLIST,
                    CBS_DROPDOWNLIST or CBS_OWNERDRAWFIXED, CBS_DROPDOWNLIST or CBS_OWNERDRAWVARIABLE);
  CharCases: array[TEditCharCase] of DWORD = (0, CBS_UPPERCASE, CBS_LOWERCASE);
  Sorts: array[Boolean] of DWORD = (0, CBS_SORT);
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'COMBOBOX');
  Params.Style := Params.Style or (WS_VSCROLL or CBS_HASSTRINGS or CBS_AUTOHSCROLL) or
                  ComboBoxStyles[FStyle] or Sorts[FSorted] or CharCases[FCharCase];
end;

procedure TsCommonComboBox.CreateWnd;
var
  ChildHandle: THandle;
begin
  inherited CreateWnd;
  FDropHandle := Handle;
  if FSaveItems <> nil then begin
    FItems.Assign(FSaveItems);
    FSaveItems.Free;
    FSaveItems := nil;
    if FSaveIndex <> -1 then begin
      if FItems.Count < FSaveIndex then FSaveIndex := Items.Count;
      SendMessage(Handle, CB_SETCURSEL, FSaveIndex, 0);
    end;
  end;
  if FStyle in [csDropDown, csSimple] then begin
    ChildHandle := GetWindow(Handle, GW_CHILD);
    if ChildHandle <> 0 then begin
      if FStyle = csSimple then begin
        FListHandle := ChildHandle;
        FDefListProc := Pointer(GetWindowLong(FListHandle, GWL_WNDPROC));
        SetWindowLong(FListHandle, GWL_WNDPROC, Longint(FListInstance));
        ChildHandle := GetWindow(ChildHandle, GW_HWNDNEXT);
      end;
      FEditHandle := ChildHandle;
      FDefEditProc := Pointer(GetWindowLong(FEditHandle, GWL_WNDPROC));
      SetWindowLong(FEditHandle, GWL_WNDPROC, Longint(FEditInstance));
    end;
  end;
  if NewStyleControls and (FEditHandle <> 0) then
    SendMessage(FEditHandle, EM_SETMARGINS, EC_LEFTMARGIN or EC_RIGHTMARGIN, 0);
end;

destructor TsCommonComboBox.Destroy;
begin
  FItems.Free;
  FSaveItems.Free;
  inherited Destroy;
end;

procedure TsCommonComboBox.DestroyWnd;
begin
  if FItems.Count > 0 then begin
    FSaveIndex := ItemIndex;
    FSaveItems := TStringList.Create;
    FSaveItems.Assign(FItems);
  end;
  inherited DestroyWnd;
end;

procedure TsCommonComboBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  TControlCanvas(FCanvas).UpdateTextFlags;
  if Assigned(FOnDrawItem) then FOnDrawItem(Self, Index, Rect, State)
  else begin
    FCanvas.FillRect(Rect);
    FCanvas.TextOut(Rect.Left + 2, Rect.Top, Items[Index]);
  end;
end;

procedure TsCommonComboBox.DrawSkinItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin

end;

function TsCommonComboBox.GetItemCount: Integer;
begin
  Result := FItems.Count;// - 2;
end;

function TsCommonComboBox.GetItemHt: Integer;
begin
  if FStyle in [csOwnerDrawFixed, csOwnerDrawVariable]
    then Result := FItemHeight
    else Result := Perform(CB_GETITEMHEIGHT, 0, 0);
end;

function TsCommonComboBox.GetItemsClass: TsCustomComboBoxStringsClass;
begin
  Result := TsComboBoxStrings;
end;

function TsCommonComboBox.GetSelText: string;
begin
  Result := '';
  if FStyle < csDropDownList then Result := Copy(Text, GetSelStart + 1, GetSelLength);
end;

procedure TsCommonComboBox.KeyPress(var Key: Char);

  function HasSelectedText(var StartPos, EndPos: DWORD): Boolean;
  begin
    SendMessage(Handle, CB_GETEDITSEL, Integer(@StartPos), Integer(@EndPos));
    Result := EndPos > StartPos;
  end;

  procedure DeleteSelectedText;
  var
    StartPos, EndPos: DWORD;
    OldText: String;
  begin
    OldText := Text;
    SendMessage(Handle, CB_GETEDITSEL, Integer(@StartPos), Integer(@EndPos));
    Delete(OldText, StartPos + 1, EndPos - StartPos);
    SendMessage(Handle, CB_SETCURSEL, -1, 0);
    Text := OldText;
    SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam(Word(StartPos), Word(StartPos)));
  end;

var
  StartPos: DWORD;
  EndPos: DWORD;
  OldText: String;
  SaveText: String;
begin
  inherited KeyPress(Key);
  if not AutoComplete then exit;
  if Style in [csDropDown, csSimple] then
    FFilter := Text
  else
  begin
   if GetTickCount - FLastTime >= 500 then
      FFilter := '';
    FLastTime := GetTickCount;
  end;
  case Ord(Key) of
    VK_ESCAPE: exit;
    VK_TAB:
      if FAutoDropDown and DroppedDown then
        DroppedDown := False;
    VK_BACK:
      begin
        if HasSelectedText(StartPos, EndPos) then
          DeleteSelectedText
        else
          if (Style in [csDropDown, csSimple]) and (Length(Text) > 0) then
          begin
            SaveText := Text;
            OldText := Copy(SaveText, 1, StartPos - 1);
            SendMessage(Handle, CB_SETCURSEL, -1, 0);
            Text := OldText + Copy(SaveText, EndPos + 1, MaxInt);
            SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam(Word(StartPos - 1), Word(StartPos - 1)));
            FFilter := Text;
          end
          else
            Delete(FFilter, Length(FFilter), 1);
        Key := #0;
        Change;
      end;
  else
    if FAutoDropDown and not DroppedDown then
      DroppedDown := True;
    if HasSelectedText(StartPos, EndPos) then
    begin
      if SelectItem(Copy(FFilter, 1, StartPos) + Key) then
        Key := #0
    end
    else
      if SelectItem(FFilter + Key) then
        Key := #0;
  end;
end;

procedure TsCommonComboBox.MeasureItem(Index: Integer; var Height: Integer);
begin
  if Assigned(FOnMeasureItem) then FOnMeasureItem(Self, Index, Height)
end;

function TsCommonComboBox.SelectItem(const AnItem: String): Boolean;
var
  Idx: Integer;
  ValueChange: Boolean;
begin
  if Length(AnItem) = 0 then begin
    Result := False;
    ItemIndex := -1;
    Change;
    exit;
  end;
  Idx := SendMessage(Handle, CB_FINDSTRING, -1, LongInt(PChar(AnItem)));
  Result := (Idx <> CB_ERR);
  if not Result then exit;
  ValueChange := Idx <> ItemIndex;
  SendMessage(Handle, CB_SETCURSEL, Idx, 0);
  if (Style in [csDropDown, csSimple]) then begin
    Text := AnItem + Copy(Items[Idx], Length(AnItem) + 1, MaxInt);
    SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam(Word(Length(AnItem)), Word(Length(Text))));
  end
  else begin
    ItemIndex := Idx;
    FFilter := AnItem;
  end;
  if ValueChange then begin
    Click;
    Select;
  end;
end;

procedure TsCommonComboBox.SetCharCase(Value: TEditCharCase);
begin
  if FCharCase <> Value then begin
    FCharCase := Value;
    RecreateWnd;
  end;
end;

procedure TsCommonComboBox.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FCommonData.Invalidate;
  end;
end;

procedure TsCommonComboBox.SetSelText(const Value: string);
begin
  if FStyle < csDropDownList then begin
    HandleNeeded;
    SendMessage(FEditHandle, EM_REPLACESEL, 0, Longint(PChar(Value)));
  end;
end;

procedure TsCommonComboBox.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then begin
    FSorted := Value;
    RecreateWnd;
  end;
end;

procedure TsCommonComboBox.SetStyle(Value: TComboBoxStyle);
begin
  if FStyle <> Value then begin
    FStyle := Value;
    RecreateWnd;
  end;
end;

procedure TsCommonComboBox.SkinPaint(DC: HDC);
var
  CI : TCacheInfo;
  R : TRect;
  State : TOwnerDrawState;
begin
  InitCacheBmp(SkinData);
  CI.Ready := False;
  CI := GetParentCache(FCommonData);

  PaintItem(FCommonData, Ci,
    False, integer(ControlIsActive(FCommonData)),
    Rect(0, 0, Width, Height),
    Point(Left, Top),
    SkinData.FCacheBmp, False
  );
  UpdateCorners(FCommonData, 0);
  if FShowButton then PaintButton;
  FCommonData.BGChanged := False;

  if not Enabled then begin
    BmpDisabledKind(FCommonData.FCacheBmp, FDisabledKind, Parent, CI, Point(Left, Top));
  end;

  BitBlt(DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);

  R := ClientRect;
  InflateRect(R, -3, -3);

  State := [odComboBoxEdit];
  if FCommonData.FFocused
    then State := State + [odFocused];
  Canvas.Handle := DC;
  DrawSkinItem(ItemIndex, R, State);
  Canvas.Handle := 0;
end;

procedure TsCommonComboBox.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if not FCommonData.Skinned then FillDC(Message.DC, ClientRect, Color);
end;

procedure TsCommonComboBox.WMLButtonDblClk(var Message: TMessage);
begin
  if FReadOnly then begin
    SetFocus;
    if Assigned(OnDblClick) then OnDblClick(Self);
  end
  else inherited;
end;

procedure TsCommonComboBox.WMLButtonDown(var Message: TWMLButtonDown);
var
  Form: TCustomForm;
begin
  if FReadOnly then SetFocus else begin
    if (DragMode = dmAutomatic) and (Style = csDropDownList) and
         (Message.XPos < (Width - GetSystemMetrics(SM_CXHSCROLL))) then begin
      SetFocus;
      BeginDrag(False);
      Exit;
    end;
    inherited;
    if MouseCapture then begin
      Form := GetParentForm(Self);
      if (Form <> nil) and (Form.ActiveControl <> Self) then MouseCapture := False;
    end;
  end;
end;

procedure TsCommonComboBox.WMPaint(var Message: TWMPaint);
const
  InnerStyles: array[TBevelCut] of Integer = (0, BDR_SUNKENINNER, BDR_RAISEDINNER, 0);
  OuterStyles: array[TBevelCut] of Integer = (0, BDR_SUNKENOUTER, BDR_RAISEDOUTER, 0);
  EdgeStyles: array[TBevelKind] of Integer = (0, 0, BF_SOFT, BF_FLAT);
  Ctl3DStyles: array[Boolean] of Integer = (BF_MONO, 0);
var
  EdgeSize: Integer;
  WinStyle: LongInt;
  C: TControlCanvas;
  R: TRect;
  PS: TPaintStruct;
  DC : hdc;
  SavedDC: hdc;
begin
  if FCommonData.Skinned then begin
    if not SkinData.CustomColor then Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].HotColor;
    Brush.Style := bsClear;
    DC := Message.DC;
    BeginPaint(Handle, PS);
    if DC = 0 then begin DC := GetWindowDC(Handle); end;
    SavedDC := SaveDC(DC);
    try
      SkinData.FUpdating := SkinData.Updating;
      if not SkinData.FUpdating
        then SkinPaint(DC);
    finally
      RestoreDC(DC, SavedDC);
      if Message.DC = 0 then ReleaseDC(Handle, DC);
      EndPaint(Handle, PS);
    end;
  end
  else begin
    inherited;
    if BevelKind = bkNone then Exit;
    C := TControlCanvas.Create;
    try
    C.Control := Self;
    R := ClientRect;
    C.Brush.Color := Color;
    C.FillRect(R);
    C.FrameRect(R);
    InflateRect(R,-1,-1);
    C.FrameRect(R);
    if BevelKind <> bkNone then begin
      EdgeSize := 0;
      if BevelInner <> bvNone then Inc(EdgeSize, BevelWidth);
      if BevelOuter <> bvNone then Inc(EdgeSize, BevelWidth);
      if EdgeSize = 0 then begin
        R := ClientRect;
        C.Brush.Color := Color;
        C.FrameRect(R);
        InflateRect(R,-1,-1);
        C.FrameRect(R);
      end;
      R := BoundsRect;
      with R do begin
        WinStyle := GetWindowLong(Handle, GWL_STYLE);
        if beLeft in BevelEdges then Dec(Left, EdgeSize);
        if beTop in BevelEdges then Dec(Top, EdgeSize);
        if beRight in BevelEdges then Inc(Right, EdgeSize);
        if (WinStyle and WS_VSCROLL) <> 0 then Inc(Right, GetSystemMetrics(SM_CYVSCROLL));
        if beBottom in BevelEdges then Inc(Bottom, EdgeSize);
        if (WinStyle and WS_HSCROLL) <> 0 then Inc(Bottom, GetSystemMetrics(SM_CXHSCROLL));
      end;
      if not EqualRect(BoundsRect, R) then BoundsRect := R;
      R := ClientRect;
      DrawEdge(C.Handle, R, InnerStyles[BevelInner] or OuterStyles[BevelOuter], Byte(BevelEdges) or EdgeStyles[BevelKind] or Ctl3DStyles[Ctl3D] or BF_ADJUST);
      R.Left := R.Right - GetSystemMetrics(SM_CXHTHUMB) - 1;
      if DroppedDown then begin
        DrawFrameControl(C.Handle, R, DFC_SCROLL, DFCS_FLAT or DFCS_SCROLLCOMBOBOX)
      end
      else begin
        DrawFrameControl(C.Handle, R, DFC_SCROLL, DFCS_FLAT or DFCS_SCROLLCOMBOBOX);
      end;
      SendMessage(Handle, WM_NCPAINT, 0, 0);
    end;
    finally
      C.Free;
    end;
  end;
end;

procedure TsCommonComboBox.WndProc(var Message: TMessage);
begin
  if ReadOnly then begin
    case Message.Msg of
      WM_KEYDOWN, WM_CHAR, WM_KEYUP, WM_SYSKEYUP, CN_KEYDOWN, CN_CHAR, CN_SYSKEYDOWN,
        CN_SYSCHAR, WM_PASTE, WM_CUT, WM_CLEAR, WM_UNDO: Exit
      else
    end
  end;
  case Message.Msg of
    CN_CTLCOLORMSGBOX..CN_CTLCOLORSTATIC : if not NewStyleControls and (Style < csDropDownList) then begin
      Message.Result := Parent.Brush.Handle;
      Exit;
    end;
    WM_COMMAND, CN_COMMAND : if ReadOnly then Exit;
  end;
  inherited WndProc(Message);
end;

{ TsCustomColorBox }

procedure TsCustomColorBox.CloseUp;
begin
  inherited CloseUp;
  FListSelected := True;
end;

procedure TsCustomColorBox.ColorCallBack(const AName: string);
var
  I, LStart: Integer;
  LColor: TColor;
  LName: string;
begin
  LColor := StringToColor(AName);
  if cbPrettyNames in Style then begin
    if Copy(AName, 1, 2) = 'cl'
      then LStart := 3
        else LStart := 1;
    LName := '';
    for I := LStart to Length(AName) do begin
      case AName[I] of
        'A'..'Z': if LName <> '' then LName := LName + ' ';
      end;
      LName := LName + AName[I];
    end;
  end
  else LName := AName;
  Items.AddObject(LName, TObject(LColor));
end;

function TsCustomColorBox.ColorRect(SourceRect : TRect; State: TOwnerDrawState): TRect;
begin
  Result := SourceRect;
  if ShowColorName
    then Result.Right := Result.Bottom - Result.Top + Result.Left
    else Result.Right := Result.Right - WidthOf(ButtonRect) - 3 * integer(FShowButton); 
  if odComboBoxEdit in State
    then InflateRect(Result, - 1 - FMargin, - 1 - FMargin)
    else InflateRect(Result, - 1, - 1);
end;

constructor TsCustomColorBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited Style := csOwnerDrawFixed;
  FStyle := [cbStandardColors, cbExtendedColors, cbSystemColors];
  FSelectedColor := clBlack;
  FDefaultColorColor := clBlack;
  FShowColorName := True;
  FNoneColorColor := clBlack;
  FCommonData.COC := COC_TsColorBox;
  PopulateList;
end;

procedure TsCustomColorBox.CreateWnd;
begin
  inherited CreateWnd;
  if FNeedToPopulate then PopulateList;
end;

procedure TsCustomColorBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
  function ColorToBorderColor(AColor: TColor): TColor;
  type
    TColorQuad = record
      Red,
      Green,
      Blue,
      Alpha: Byte;
    end;
  begin
    if (TColorQuad(AColor).Red > 192) or
       (TColorQuad(AColor).Green > 192) or
       (TColorQuad(AColor).Blue > 192) then
      Result := clBlack
    else if odSelected in State then
      Result := clWhite
    else
      Result := AColor;
  end;
var
  LRect: TRect;
  LBackground: TColor;
begin
  with Canvas do begin
    FillRect(Rect);
    LBackground := Brush.Color;

    LRect := Rect;
    if FShowColorName or not (odComboBoxEdit in State)
      then LRect.Right := LRect.Bottom - LRect.Top + LRect.Left
      else LRect.Right := Rect.Right - WidthOf(ButtonRect);
    InflateRect(LRect, -1, -1);


    Brush.Color := Colors[Index];
    if Brush.Color = clDefault then Brush.Color := DefaultColorColor else if Brush.Color = clNone then Brush.Color := NoneColorColor;
    FillRect(LRect);
    Brush.Color := ColorToBorderColor(ColorToRGB(Brush.Color));
    FrameRect(LRect);

    Brush.Color := LBackground;
    Rect.Left := LRect.Right + 5;

{KJS ADDED}
    if FShowColorName or not (odComboBoxEdit in State) then {KJS END ADD} TextRect(Rect, Rect.Left, Rect.Top + (Rect.Bottom - Rect.Top - TextHeight(Items[Index])) div 2, Items[Index]);
  end;
end;

procedure TsCustomColorBox.DrawSkinItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  R, aRect : TRect;
  Bmp : TBitmap;
  CI : TCacheInfo;
  function ColorToBorderColor(AColor: TColor): TColor; begin
    if (TsColor(AColor).R > 192) or (TsColor(AColor).G > 192) or (TsColor(AColor).B > 192) then
      Result := clBlack
    else if odSelected in State then
      Result := clWhite
    else
      Result := AColor;
  end;
begin
  R := Rect;
  aRect := Rect;
  Canvas.Brush.Style := bsSolid;
  if odComboBoxEdit in State then begin // if editor window ...
    OffsetRect(R, - R.Left, - R.Top);
    OffsetRect(Rect, - Rect.Left, - Rect.Top);
    R.Right := R.Right - WidthOf(ButtonRect) - 3 * integer(FShowButton);
    Bmp := CreateBmp32(WidthOf(R), HeightOf(R));

    BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, FCommonData.FCacheBmp.Canvas.Handle, aRect.Left, aRect.Top, SRCCOPY);

    with Bmp do begin
      if odFocused in state then begin
        if FCommonData.SkinManager.gd[FCommonData.SkinIndex].ShowFocus then begin
          if ShowcolorName then begin
            Bmp.Canvas.Brush.Color := clHighLight;
            Bmp.Canvas.Font.Color := clHighlightText;
            R := Classes.Rect({ColorRect(Rect, State).Right + }3, R.Top + 1, R.Right - 3, R.Bottom - 1);
            Bmp.Canvas.FillRect(R);
          end;
          DrawFocusRect(Bmp.Canvas.Handle, R);
        end;
      end
      else begin
        InflateRect(R, 3, 3);
        BitBlt(Bmp.Canvas.Handle, R.Left, R.Top, WidthOf(R), HeightOf(R), FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
      end;
      Bmp.Canvas.Brush.Color := Colors[Index];
      if ShowcolorName then begin
        R := ColorRect(Rect, State);

        if Bmp.Canvas.Brush.Color = clDefault
          then Bmp.Canvas.Brush.Color := DefaultColorColor
          else if Bmp.Canvas.Brush.Color = clNone then Bmp.Canvas.Brush.Color := NoneColorColor;

        Bmp.Canvas.FillRect(R);
        Bmp.Canvas.Brush.Color := ColorToBorderColor(ColorToRGB(Brush.Color));
        Bmp.Canvas.FrameRect(R);

        Rect.Left := R.Right + 5;

        Bmp.Canvas.Brush.Style := bsClear;
        if (odFocused in state) and FCommonData.SkinManager.gd[FCommonData.SkinIndex].ShowFocus then begin
          Bmp.Canvas.TextRect(Rect, Rect.Left,
            Rect.Top + (Rect.Bottom - Rect.Top - Bmp.Canvas.TextHeight(Items[Index])) div 2,
            Items[Index]);
        end
        else begin
          acWriteTextEx(Bmp.Canvas, PacChar(Items[Index]), Enabled,
                 Rect, DT_NOPREFIX, FCommonData, ControlIsActive(FCommonData));
        end;
      end
      else begin
        R := ColorRect(Rect, State);

        if Bmp.Canvas.Brush.Color = clDefault then begin
          Bmp.Canvas.Brush.Color := DefaultColorColor
        end
        else if Bmp.Canvas.Brush.Color = clNone then begin
          Bmp.Canvas.Brush.Color := NoneColorColor;
        end;
        Bmp.Canvas.FillRect(R);
        Bmp.Canvas.Brush.Color := ColorToBorderColor(ColorToRGB(Brush.Color));
        Bmp.Canvas.FrameRect(R);
      end;
    end;

    if not Enabled then begin
      CI := MakeCacheinfo(SkinData.FCacheBmp);
      BmpDisabledKind(Bmp, DisabledKind, Parent, CI, Point(aRect.Left, aRect.Top));
    end;
    BitBlt(Canvas.Handle, aRect.Left, aRect.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    FreeAndNil(Bmp);
  end
  else begin
    if odFocused in state then begin
      Canvas.Brush.Color := clHighLight;
      Canvas.Font.Color := clHighlightText;
      Canvas.FillRect(Classes.Rect(R.Left, R.Top, R.Right, R.Bottom));
      DrawFocusRect(Canvas.Handle, Classes.Rect(R.Left, R.Top, R.Right, R.Bottom));
    end
    else begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(Rect);
      Canvas.Font.Color := Font.Color;
    end;

    R := Rect;
    R.Right := R.Bottom - R.Top + R.Left;
    InflateRect(R, -1, -1);

    Canvas.Brush.Color := Colors[Index];
    if Canvas.Brush.Color = clDefault then begin
      Canvas.Brush.Color := DefaultColorColor
    end
    else if Canvas.Brush.Color = clNone then begin
      Canvas.Brush.Color := NoneColorColor;
    end;
{KJS ADDED}
//    if not fShowcolorName then
//       OffsetRect(R,2,0);
{KJS END ADD}

    Canvas.FillRect(R);
    Canvas.Brush.Color := ColorToBorderColor(ColorToRGB(Brush.Color));
    Canvas.FrameRect(R);

    Rect.Left := R.Right + 5;

    Canvas.Brush.Style := bsClear;
 {KJS ADDED}
//    if fShowcolorName then {KJS END ADDED}
   Canvas.TextRect(Rect, Rect.Left,
      Rect.Top + (Rect.Bottom - Rect.Top - Canvas.TextHeight(Items[Index])) div 2,
      Items[Index]);
  end;
end;

function TsCustomColorBox.GetColor(Index: Integer): TColor;
begin
  if Index < 0 then begin
    Result := clNone;
    Exit;
  end;
  Result := TColor(Items.Objects[Index]);
end;

function TsCustomColorBox.GetColorName(Index: Integer): string;
begin
  Result := Items[Index];
end;

function TsCustomColorBox.GetSelected: TColor;
begin
  if HandleAllocated then
    if ItemIndex <> -1 then
      Result := Colors[ItemIndex]
    else
      Result := NoColorSelected
  else
    Result := FSelectedColor;
end;

procedure TsCustomColorBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  FListSelected := False;
  inherited KeyDown(Key, Shift);
end;

procedure TsCustomColorBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (cbCustomColor in Style) and (Key = #13) and (ItemIndex = 0) then begin
    PickCustomColor;
    Key := #0;
  end;
end;

function TsCustomColorBox.PickCustomColor: Boolean;
var
  LColor: TColor;
begin
  if ColDlg = nil then ColDlg := TsColorDialog.Create(nil);// else ColDlg := TColorDialog.Create(nil);
  LColor := ColorToRGB(TColor(Items.Objects[0]));
  ColDlg.Color := LColor;
//  ColDlg.CustomColors.Text := Format('ColorA=%.8x', [LColor]);
  Result := ColDlg.Execute;
  if Result then begin
    Items.Objects[0] := TObject(ColDlg.Color);
    Self.Invalidate;
  end;
end;

procedure TsCustomColorBox.PopulateList;
  procedure DeleteRange(const AMin, AMax: Integer); var I: Integer; begin
    for I := AMax downto AMin do Items.Delete(I);
  end;
  procedure DeleteColor(const AColor: TColor); var I: Integer; begin
    I := Items.IndexOfObject(TObject(AColor));
    if I <> -1 then Items.Delete(I);
  end;
var
  LSelectedColor, LCustomColor: TColor;
begin
  if HandleAllocated then begin
    Items.BeginUpdate;
    try
      LCustomColor := clBlack;
      if (cbCustomColor in Style) and (Items.Count > 0) then LCustomColor := TColor(Items.Objects[0]);
      LSelectedColor := FSelectedColor;
      Items.Clear;
      GetColorValues(ColorCallBack);
      if not (cbIncludeNone in Style) then DeleteColor(clNone);
      if not (cbIncludeDefault in Style) then DeleteColor(clDefault);
      if not (cbSystemColors in Style) then DeleteRange(StandardColorsCount + ExtendedColorsCount, Items.Count - 1);
      if not (cbExtendedColors in Style) then DeleteRange(StandardColorsCount, StandardColorsCount + ExtendedColorsCount - 1);
      if not (cbStandardColors in Style) then DeleteRange(0, StandardColorsCount - 1);
      if cbCustomColor in Style then Items.InsertObject(0, 'Custom...', TObject(LCustomColor));
      if (cbCustomColors in Style) and Assigned(OnGetColors) then OnGetColors(Self, Items);
      Selected := LSelectedColor;
    finally
      Items.EndUpdate;
      Self.FNeedToPopulate := False;
    end;
  end
  else FNeedToPopulate := True;
end;

procedure TsCustomColorBox.Select;
begin
  if FListSelected then begin
    FListSelected := False;
    if (cbCustomColor in Style) and
         (ItemIndex = 0) and
           not PickCustomColor then
      Exit;
  end;
  inherited Select;
end;

procedure TsCustomColorBox.SetDefaultColorColor(const Value: TColor);
begin
  if Value <> FDefaultColorColor then begin
    FDefaultColorColor := Value;
    FCommonData.Invalidate;
  end;
end;

procedure TsCustomColorBox.SetMargin(const Value: integer);
begin
  if FMargin <> Value then begin
    FMargin := Value;
    FMargin := min(FMargin, Height div 2);
    FCommonData.Invalidate;
  end;
end;

procedure TsCustomColorBox.SetNoneColorColor(const Value: TColor);
begin
  if Value <> FNoneColorColor then
  begin
    FNoneColorColor := Value;
    FCommonData.Invalidate;
  end;
end;

procedure TsCustomColorBox.SetSelected(const AColor: TColor);
var
  I: Integer;
begin
  if HandleAllocated then begin
    I := Items.IndexOfObject(TObject(AColor));
    if AColor <> 0 then begin
      if (I = -1) and (cbCustomColor in Style) and (AColor <> NoColorSelected) then begin
        Items.Objects[0] := TObject(AColor);
        I := 0;
      end;
      ItemIndex := I;
    end
    else begin
      if (cbCustomColor in Style) then begin
        if (I = -1) and ((AColor <> NoColorSelected)) then begin
          Items.Objects[0] := TObject(AColor);
          I := 0;
        end;
        if (I = 0) and (Items.Objects[0] = TObject(AColor)) then begin
          ItemIndex := 0;
          for I := 1 to Items.Count - 1 do begin
            if Items.Objects[0] = TObject(AColor) then begin
              ItemIndex := I;
              Break;
            end;
          end;
        end
        else ItemIndex := I;
      end
      else ItemIndex := I;
    end;
  end;
  FSelectedColor := AColor;
end;

procedure TsCustomColorBox.SetShowColorName(const Value: boolean);
begin
  if FShowColorName <> Value then begin
    FShowColorName := Value;
    FCommonData.Invalidate;
  end;
end;

procedure TsCustomColorBox.SetStyle(AStyle: TsColorBoxStyle);
begin
  if AStyle <> Style then begin
    FStyle := AStyle;
    Enabled := ([cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor] * FStyle) <> [];
    PopulateList;
//    if (Items.Count > 0) and (ItemIndex = -1) then ItemIndex := 0;
  end;
end;

{ TsCustomComboBoxStrings }

procedure TsCustomComboBoxStrings.Clear;
var
  S: string;
begin
  S := ComboBox.Text;
  SendMessage(ComboBox.Handle, CB_RESETCONTENT, 0, 0);
  ComboBox.Text := S;
  ComboBox.Update;
end;

procedure TsCustomComboBoxStrings.Delete(Index: Integer);
begin
  SendMessage(ComboBox.Handle, CB_DELETESTRING, Index, 0);
end;

function TsCustomComboBoxStrings.Get(Index: Integer): string;
var
  Text: array[0..4095] of Char;
  Len: Integer;
begin
  Len := SendMessage(ComboBox.Handle, CB_GETLBTEXT, Index, Longint(@Text));
  if Len = CB_ERR then Len := 0;
  SetString(Result, Text, Len);
end;

function TsCustomComboBoxStrings.GetCount: Integer;
begin
  Result := SendMessage(ComboBox.Handle, CB_GETCOUNT, 0, 0);
end;

function TsCustomComboBoxStrings.GetObject(Index: Integer): TObject;
begin
  Result := TObject(SendMessage(ComboBox.Handle, CB_GETITEMDATA, Index, 0));
  if Longint(Result) = CB_ERR then
    Error('List index out of bounds', Index);
end;

function TsCustomComboBoxStrings.IndexOf(const S: string): Integer;
begin
  Result := SendMessage(ComboBox.Handle, CB_FINDSTRINGEXACT, -1, LongInt(PChar(S)));
end;

procedure TsCustomComboBoxStrings.PutObject(Index: Integer;
  AObject: TObject);
begin
  SendMessage(ComboBox.Handle, CB_SETITEMDATA, Index, Longint(AObject));
end;

procedure TsCustomComboBoxStrings.SetUpdateState(Updating: Boolean);
begin
  SendMessage(ComboBox.Handle, WM_SETREDRAW, Ord(not Updating), 0);
  if not Updating then ComboBox.Refresh;
end;

{ TsComboBoxStrings }

function TsComboBoxStrings.Add(const S: string): Integer;
begin
  Result := SendMessage(ComboBox.Handle, CB_ADDSTRING, 0, Longint(PChar(S)));
  if Result < 0 then
    raise EOutOfResources.Create(SInsertLineError);
end;

procedure TsComboBoxStrings.Insert(Index: Integer; const S: string);
begin
  if SendMessage(ComboBox.Handle, CB_INSERTSTRING, Index, Longint(PChar(S))) < 0 then raise EOutOfResources.Create(SInsertLineError);
end;

{ TsCustomComboBoxEx }

procedure TsCustomComboBoxEx.Clear;
begin
  inherited;
  FItemsEx.Clear;
end;

constructor TsCustomComboBoxEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  Style := csDropDown;
  Style := csOwnerDrawFixed;//Variable;
  FItemsEx := TsComboItems.Create(Self);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  UpdateList;
end;

procedure TsCustomComboBoxEx.CreateWnd;
begin
  inherited CreateWnd;
  if NeedToUpdate then UpdateList;
  UpdateMargins;
end;

function TsCustomComboBoxEx.CurrentImage(Item : TsComboItem; State: TOwnerDrawState): integer;
begin
  Result := -1;
  if (Images = nil) or (Item = nil) then Exit;
  if odComboBoxEdit in State then begin
    Result := Item.ImageIndex;
  end
  else if odSelected in State then begin
    Result := Item.SelectedImageIndex;
    if Result < 0 then Result := Item.ImageIndex;
  end
  else begin
    Result := Item.ImageIndex;
  end;
end;

destructor TsCustomComboBoxEx.Destroy;
begin
  try
    if Images <> nil then Images.UnRegisterChanges(FImageChangeLink);
    FImageChangeLink.OnChange := nil;
    FreeAndNil(FImageChangeLink);
  except
  end;
  if Assigned(FItemsEX) then FreeAndNil(FItemsEx);
  inherited Destroy;
end;

procedure TsCustomComboBoxEx.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  R, rText : TRect;
  i : integer;
  function ColorToBorderColor(AColor: TColor): TColor; begin
    if (TsColor(AColor).R > 192) or (TsColor(AColor).G > 192) or (TsColor(AColor).B > 192) then
      Result := clBlack
    else if odSelected in State then
      Result := clWhite
    else
      Result := AColor;
  end;
begin
  R := Rect;
  Canvas.Brush.Style := bsSolid;
  if odComboBoxEdit in State then begin // if editor window ...
//v4.83    R.Right := R.Right - WidthOf(ButtonRect);
    R.Right := R.Right - 1;
    if (SkinData.Ffocused or Focused or (odFocused in state)) and not DroppedDown then begin
      Canvas.Brush.Color := clHighLight;
      Canvas.FillRect(R);
    end
    else begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(R);
    end;
    R.Right := R.Right - 3;
    if Index > -1 then begin
      R := ImgRect(SelectedItem, State);
      i := CurrentImage(ItemsEx[Index], State);
      if i > -1 then begin
        Images.Draw(FCanvas, R.Left, R.Top, i, Enabled);
      end else R.Bottom := Rect.bottom;
    end;
    // Text out
    rText := R;
    rText.Left := R.Right + 5;
    rText.Right := Width - WidthOf(ButtonRect) - 3 * integer(FShowButton);

    Canvas.Brush.Style := bsClear;
    Canvas.Font.Assign(Font);
    if (odFocused in state) then begin
      Canvas.Font.Color := clHighLightText;
    end;
    Canvas.TextRect(rText, rText.Left,
        rText.Top + (rText.Bottom - rText.Top - Canvas.TextHeight(Items[Index])) div 2,
        Items[Index]);
//    Rect := rText;
  end
  else begin

    if odSelected in state then begin
      Canvas.Brush.Color := clHighLight;
      Canvas.Font.Color := clHighlightText;
      Canvas.FillRect(Rect);
      DrawFocusRect(Canvas.Handle, Rect);
    end
    else begin
      Canvas.Brush.Color := Color;// clWindow; v4.43
      Canvas.FillRect(Rect);
      Canvas.Font.Color := Font.Color;// clWindowText; v4.43
    end;

    R := ImgRect(ItemsEx[Index], State);
    R.Top := Rect.Top + (HeightOf(Rect) - HeightOf(R)) div 2;
//    R.Top := Rect.Top;
    R.Bottom := Rect.Bottom;

    if WidthOf(R) > 0 then begin
      i := CurrentImage(ItemsEx[Index], State);
      if i > -1 then begin
        Images.Draw(FCanvas, R.Left, R.Top, i, True);
      end;
    end;

    rText := Rect;
    rText.Left := R.Right + 5;
    Canvas.Brush.Style := bsClear;
    Canvas.TextRect(rText, rText.Left,
            rText.Top + (rText.Bottom - rText.Top - Canvas.TextHeight(Items[Index])) div 2,
            Items[Index]);
  end;
end;

procedure TsCustomComboBoxEx.DrawSkinItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  R, rText : TRect;
  i : integer;
begin
  R := Rect;
  Canvas.Brush.Style := bsSolid;
  if odComboBoxEdit in State then begin // if editor window ...
    Canvas.Font.Assign(Font);
    if Index > -1 then begin
      i := CurrentImage(ItemsEx[Index], State);
      if i > -1 then begin
        R := ImgRect(SelectedItem, State);
      end
      else begin
        R := Rect;
        R.Right := R.Left - 3;
        R.Bottom := Rect.bottom;
      end;
    end else R.Right := R.Left - 3;
//    rText := R;
    rText.Left := R.Right + 3;
    rText.Right := Width - WidthOf(ButtonRect) - 3 - 2 * integer(FShowButton);
    rText.Top := Rect.Top;
    rText.Bottom := Rect.Bottom;
    if (SkinData.Ffocused or Focused or (odFocused in state)) and not DroppedDown then begin
      Canvas.Brush.Color := clHighLight;
      Canvas.Font.Color := clHighlightText;
      Canvas.FillRect(Classes.Rect(Rect.Left, Rect.Top, RText.Right, Rect.Bottom));
      DrawFocusRect(Canvas.Handle, Classes.Rect(Rect.Left, Rect.Top, RText.Right, Rect.Bottom));
    end;
    if Index > -1 then begin
      i := CurrentImage(ItemsEx[Index], State);
      if i > -1 then Images.Draw(FCanvas, R.Left, R.Top, i, True)
    end;
    // Text out

    Canvas.Brush.Style := bsClear;
    rText.Left := rText.Left + 2;
    if (SkinData.Ffocused or Focused or (odFocused in state)) and not DroppedDown then begin
      Canvas.TextRect(rText, rText.Left, rText.Top + (rText.Bottom - rText.Top - Canvas.TextHeight(Items[Index])) div 2, Items[Index]);
    end
    else begin
      WriteTextEx(Canvas, PChar(Items[Index]), Enabled, rText, DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE, FCommonData, ControlIsActive(FCommonData));
    end;
  end
  else begin
    if Index > ItemsEx.Count - 1 then Exit;
    if Index < 0 then Exit;

    Canvas.Brush.Color := FCommonData.SkinManager.gd[SkinData.SkinIndex].Color;
    Canvas.FillRect(Rect);
    Canvas.Font.Color := FCommonData.SkinManager.gd[SkinData.SkinIndex].FontColor[1];

    R := ImgRect(ItemsEx[Index], State);
    R.Top := Rect.Top + (HeightOf(Rect) - HeightOf(R)) div 2;
    R.Bottom := Rect.Bottom;

    rText := Rect;
    rText.Left := R.Right + 5;

    if odSelected in State then begin
      Canvas.Brush.Color := clHighLight;
      Canvas.Font.Color := clHighlightText;

      rText.Right := min(rText.Left + Canvas.TextWidth(Items[Index]) + 6, rText.Right);
      rText.Left := rText.Left - 2;
      Canvas.FillRect(Rect);
      DrawFocusRect(Canvas.Handle, Rect);
      rText.Left := rText.Left + 2;
    end;

    if WidthOf(R) > 0 then begin
      i := CurrentImage(ItemsEx[Index], State);
      if i > -1 then Images.Draw(FCanvas, R.Left, R.Top, i, True);
    end;

    Canvas.Brush.Style := bsClear;
    Canvas.TextRect(rText, rText.Left,
            rText.Top + (rText.Bottom - rText.Top - Canvas.TextHeight(Items[Index])) div 2,Items[Index]);
  end;
end;

function TsCustomComboBoxEx.GetSelectedItem: TsComboItem;
begin
  if ItemIndex > -1 then begin
    Result := ItemsEx[ItemIndex];
  end
  else Result := nil
end;

procedure TsCustomComboBoxEx.ImageListChange(Sender: TObject);
begin
  if Sender <> nil then begin
    if HandleAllocated then Perform(CBEM_SETIMAGELIST, 0, TCustomImageList(Sender).Handle);
  end
  else Images := nil;
end;

function TsCustomComboBoxEx.ImgRect(Item : TsComboItem; State: TOwnerDrawState): TRect;
begin
  if (Images <> nil) and (Item <> nil) then begin
    Result.Top := (Height - Images.Height) div 2;
    Result.Left := Result.Top;
    Result.Right := Result.Left + Images.Width;
    Result.Bottom := Result.top + Images.Height;
    if not (odComboBoxEdit in State) and (Item.Indend > 0) then begin
      OffsetRect(Result, Item.Indend * 16{Canvas.TextWidth('w')}, 0);
    end;
  end
  else begin
    Result := Rect(0, 0, 0, 0);
  end;
end;

procedure TsCustomComboBoxEx.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove{opInsert} then begin
    if AComponent = Images then begin
      FImages := nil;
    end;
  end;
end;

procedure TsCustomComboBoxEx.SetImages(const Value: TCustomImageList);
begin
  if Images <> nil then Images.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if Images <> nil then begin
    Images.RegisterChanges(FImageChangeLink);
    Images.FreeNotification(Self);
    if HandleAllocated then
      PostMessage(Handle, CBEM_SETIMAGELIST, 0, integer(Images.Handle));
    if ItemHeight < Images.Height then ItemHeight := Images.Height;
  end
  else begin
    if HandleAllocated then begin
      Perform(CBEM_SETIMAGELIST, 0, 0);
      RecreateWnd;
    end;
  end;
  UpdateMargins;
end;

procedure TsCustomComboBoxEx.SetItemsEx(const Value: TsComboItems);
begin
  FItemsEx.Assign(Value);
end;

procedure TsCustomComboBoxEx.UpdateList;
var
  i : integer;
begin
  if HandleAllocated then begin
    Items.BeginUpdate;
    try
      Items.Clear;
      for i := 0 to ItemsEx.Count - 1 do begin
        Items.Insert(ItemsEx[i].Index, ItemsEx[i].Caption);
      end;
    finally
      Items.EndUpdate;
      NeedToUpdate := False;
    end;
  end
  else NeedToUpdate := True;
end;

procedure TsCustomComboBoxEx.UpdateMargins;
begin
  if Self = nil then Exit;
  if (Images <> nil) and (ItemIndex > -1) then begin
    SendMessage(EditHandle, EM_SETMARGINS, EC_LEFTMARGIN, MakeLong(Images.Width, 0));
  end
  else begin
    SendMessage(EditHandle, EM_SETMARGINS, EC_LEFTMARGIN, 0);
    Invalidate;
  end;
end;

{ TsComboItems }

function TsComboItems.Add: TsComboItem;
begin
  Result := TsComboItem(inherited Add);
end;

function TsComboItems.AddItem(const Caption: String; const ImageIndex,
  SelectedImageIndex, OverlayImageIndex, Indent: Integer;
  Data: Pointer): TsComboItem;
begin
  Result := Add;
  Result.Caption := Caption;
  Result.ImageIndex := ImageIndex;
  Result.SelectedImageIndex := SelectedImageIndex;
  Result.OverlayImageIndex := OverlayImageIndex;
  Result.Indend := Indent;
  Result.Data := Data;
end;

constructor TsComboItems.Create(AOwner: TsCustomComboBoxEx);
begin
  inherited Create(TsComboItem);
  FOwner := AOwner;
end;

destructor TsComboItems.Destroy;
begin
  FOwner := nil;
  inherited Destroy;
end;

function TsComboItems.GetItem(Index: Integer): TsComboItem;
begin
  Result := TsComboItem(inherited GetItem(Index))
end;

function TsComboItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TsComboItems.SetItem(Index: Integer; Value: TsComboItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TsComboItems.Update(Item: TCollectionItem);
begin
  inherited;
end;

{ TsComboItem }

procedure TsComboItem.Assign(Source: TPersistent);
begin
  inherited;
end;

constructor TsComboItem.Create(Collection: TCollection);
begin
  inherited;
  ImageIndex := -1;
  Indend := -1;
  OverlayImageIndex := -1;
  SelectedImageIndex := -1;
end;

destructor TsComboItem.Destroy;
begin
  inherited;
end;

function TsComboItem.GetDisplayName: string;
begin
  if FCaption <> '' then begin
    Result := FCaption;
  end
  else Result := inherited GetDisplayName;
end;

procedure TsComboItem.SetCaption(const Value: string);
begin
  if FCaption <> Value then begin
    FCaption := Value;
    DisplayName := Value;
  end;
end;

procedure TsComboItem.SetData(const Value: Pointer);
begin
  FData := Value;
end;

initialization
  ColDlg := nil;

finalization
  if Assigned(ColDlg) then FreeAndNil(ColDlg);

end.
