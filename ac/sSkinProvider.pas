unit sSkinProvider;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms, Dialogs, sDefaults, menus, sSkinMenus, sSkinManager,
  sConst, sCommondata, Controls, acSBUtils{$IFDEF TNTUNICODE}, TntWideStrUtils, TntMenus, TntStdCtrls{$ENDIF};

type

{$IFNDEF NOTFORHELP}
  TacCtrlAdapter = class;
  TacAdapterItem = class(TPersistent)
  public
    Ctrl : TWinControl;
    SkinData : TsCommonData;
    OldFontColor : integer;
    Adapter : TacCtrlAdapter;
    ScrollWnd : TacMainWnd;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure DoHook(Control : TWinControl); virtual;
  end;
{$ENDIF} // NOTFORHELP

  TAddItemEvent = procedure(Item: TComponent; var CanBeAdded: boolean; var SkinSection: string) of object;
  TsSkinProvider = class;

  TsGripMode = (gmNone, gmRightBottom);
  TsResizeMode = (rmStandard, rmBorder);

{$IFNDEF NOTFORHELP}
  TsSystemMenu = class;
  TsCaptionButton = record
    State : integer;
    ImageIndex : integer;
    Rect : TRect;
    HaveAlignment : boolean;
    GlowID : integer;
  end;
{$ENDIF} // NOTFORHELP

  TsTitleButton = class(TCollectionItem)
{$IFNDEF NOTFORHELP}
  private
    FUseSkinData: boolean;
    FName: string;
    FGlyph: TBitmap;
    FOnMouseUp: TMouseEvent;
    FOnMouseDown: TMouseEvent;
    FEnabled: boolean;
    procedure SetGlyph(const Value: TBitmap);
    procedure SetName(const Value: string);
    procedure MouseDown(BtnIndex : integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(BtnIndex : integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  public
    BtnData : TsCaptionButton;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function GetDisplayName: string; override;
  published
{$ENDIF} // NOTFORHELP
    property Enabled : boolean read FEnabled write FEnabled default True;
    property Glyph : TBitmap read FGlyph write SetGlyph;
    property Name : string read FName write SetName;
    property UseSkinData : boolean read FUseSkinData write FUseSkinData default True;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
  end;

  TsTitleButtons = class(TCollection)
{$IFNDEF NOTFORHELP}
  private
    FOwner: TsSkinProvider;
    function GetItem(Index: Integer): TsTitleButton;
    procedure SetItem(Index: Integer; Value: TsTitleButton);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TsSkinProvider);
    destructor Destroy; override;
{$ENDIF} // NOTFORHELP
    property Items[Index: Integer]: TsTitleButton read GetItem write SetItem; default;
  end;

  TsTitleIcon = class (TPersistent)
{$IFNDEF NOTFORHELP}
  private
    FGlyph: TBitmap;
    FHeight: integer;
    FWidth: integer;
    procedure SetGlyph(const Value: TBitmap);
    procedure SetHeight(const Value: integer);
    procedure SetWidth(const Value: integer);
  public
    constructor Create;
    destructor Destroy; override;
  published
{$ENDIF} // NOTFORHELP
    property Glyph : TBitmap read FGlyph write SetGlyph;
    property Height : integer read FHeight write SetHeight default 0;
    property Width : integer read FWidth write SetWidth default 0;
  end;

  TsSkinProvider = class(TComponent)
{$IFNDEF NOTFORHELP}
  private
    ArOR : TAOR;
    RgnChanged : boolean;
    CurrentHT : integer;
    TempBmp : TBitmap;

    FMakeSkinMenu: boolean;
    FShowAppIcon: boolean;
    ControlsChanged : boolean;
    HaveSysMenu : boolean;

    FCaptionAlignment: TAlignment;
    FTitleIcon: TsTitleIcon;
    FTitleButtons: TsTitleButtons;
    FGripMode: TsGripMode;
    FCommonData: TsCommonData;
    FResizeMode: TsResizeMode;
    FirstInitialized : boolean;
    FScreenSnap: boolean;
    FSnapBuffer: integer;
    FUseGlobalColor: boolean;
    FTitleSkin: TsSkinSection;
    FMenuLineSkin: TsSkinSection;
    UserBtnIndex : integer;
    FOnSkinItem: TAddItemEvent;
    FDrawNonClientArea: boolean;

    procedure SetCaptionAlignment(const Value: TAlignment);
    procedure SetShowAppIcon(const Value: boolean);
    procedure SetTitleButtons(const Value: TsTitleButtons);
    procedure SetUseGlobalColor(const Value: boolean);
    function GetLinesCount : integer; // Returns a count of menu lines
    procedure SetTitleSkin(const Value: TsSkinSection);
    procedure SetMenuLineSkin(const Value: TsSkinSection);
    procedure SetDrawNonClientArea(const Value: boolean);
  protected
    DwmInitialized : boolean;
    ClearButtons : boolean;
    MenusInitialized : boolean;
    RegionChanged : boolean;
    CaptChanged : boolean;
    CaptRgnChanged : boolean;

    ButtonMin : TsCaptionButton;
    ButtonMax : TsCaptionButton;
    ButtonClose : TsCaptionButton;
    ButtonHelp : TsCaptionButton;
    MDIMin : TsCaptionButton;
    MDIMax : TsCaptionButton;
    MDIClose : TsCaptionButton;

    LastClientRect : TRect;

    procedure AdapterRemove;
    procedure AdapterCreate; virtual;
    procedure SendToAdapter(Message : TMessage);
    // Painting procedures <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    procedure PaintAll;
    procedure PaintForm(DC : hdc; SendUpdated : boolean = True);
    procedure PaintCaption(dc : hdc);
    procedure PrepareCaption(CI : TCacheInfo; R : TRect);
    procedure PaintBorderIcons;
    procedure RepaintButton(i : integer);
    procedure RepaintMenuItem(mi : TMenuItem; R : TRect; State : TOwnerDrawState);
    procedure MakeTitleBG;
    procedure SaveBGForBtns(Full : boolean = False);
    procedure RestoreBtnsBG;

    procedure OurPaintHandler(const Msg : TWMPaint);
    procedure AC_WMEraseBkGnd(const aDC: hdc);
    procedure AC_WMNCPaint;
    procedure AC_WMNCCalcSize(Message : TWMNCCalcSize);
//    procedure AC_WMWindowPosChanged(const Msg : TWMWindowPosMsg);

    // Special calculations <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    function HTProcess(Message : TWMNCHitTest) : integer;
    function AboveBorder(Message : TWMNCHitTest) : boolean;
    function CursorToPoint(x, y : integer) : TPoint;
    function MDIButtonsNeeded : boolean;
    function RBGripPoint(ImgIndex : integer) : TPoint;
    function IconRect : TRect;
    function FormLeftTop : TPoint;
    function SysButtonsCount : integer;
    function SmallButtonWidth : integer;
    function ButtonHeight : integer;
    function SmallButtonHeight : integer;

    function SysButtonWidth(Btn : TsCaptionButton) : integer;
    function TitleBtnsWidth : integer;
    function UserButtonWidth(Btn : TsTitleButton) : integer;
    function BarWidth(i : integer) : integer;
    // >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    procedure UpdateIconsIndexes;

    procedure StartMove(X, Y: Integer);                             // MarkB
    procedure StopMove(X, Y: Integer);                              // MarkB
    procedure DrawFormBorder(X, Y: Integer);                        // MarkB

    procedure SetHotHT(i : integer; Repaint : boolean = True);
    procedure SetPressedHT(i : integer);

    function FormChanged : boolean;
    function IconVisible : boolean;
    function TitleSkinSection : string;
    procedure CheckSysMenu(const Skinned : boolean);
  public
    AeroInvalidate : boolean;
    InMenu : boolean;
    ShowAction : TShowAction;

    fAnimating : boolean;
    ListSW : TacScrollWnd;
    Adapter : TacCtrlAdapter;

    RgnChanging : boolean;

    MenuChanged : boolean;
    OldWndProc: TWndMethod;
    MDIForm : TObject;
    FormActive : boolean;

    MenuLineBmp : TBitmap;

    Form : TForm;
    FLinesCount : integer;
    SystemMenu : TsSystemMenu;
    TitleBG : TBitmap;
    CaptForm : TForm;
    OldCaptFormProc : TWndMethod;
{$IFDEF CHECKXP}
//    StdBgIsUsed : boolean;
{$ENDIF}

    constructor Create(AOwner : TCOmponent); override;
    procedure DropSysMenu(x, y : integer);
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Loaded; override;
    procedure PrepareForm;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function OffsetX : integer;
    function OffsetY : integer;

    procedure NewWndProc(var Message: TMessage); // Main window procedure hook
    procedure DsgnWndProc(var Message: TMessage); // Main window procedure hook for design-time
    procedure HookMDI(Active : boolean = True);
    function HeaderHeight : integer;  // Height of the header + menu lines
    function SysBorderWidth : integer;
    function SysBorderHeight : integer;
    function BorderHeight: integer;
    function BorderWidth: integer;
    function CaptionHeight : integer;
    function CaptionWidth : integer;
    function MenuHeight : integer;
    function MenuPresent : boolean;
    function FormColor : TColor;

    procedure MdiIcoFormPaint(Sender : TObject);
    procedure CaptFormPaint(Sender : TObject);
    procedure NewCaptFormProc(var Message: TMessage);

    function UpdateMenu : boolean;
    procedure InitMenuItems(A: boolean);
    procedure RepaintMenu;
    property LinesCount : integer read GetLinesCount;
  published
{$ENDIF} // NOTFORHELP
    property CaptionAlignment : TAlignment read FCaptionAlignment write SetCaptionAlignment default taLeftJustify;
    property DrawNonClientArea : boolean read FDrawNonClientArea write SetDrawNonClientArea default True;
    property SkinData : TsCommonData read FCommonData write FCommonData;
    property GripMode : TsGripMode read FGripMode write FGripMode default gmNone;
    property MakeSkinMenu : boolean read FMakeSkinMenu write FMakeSkinMenu default DefMakeSkinMenu;
    property MenuLineSkin : TsSkinSection read FMenuLineSkin write SetMenuLineSkin;
    property ResizeMode : TsResizeMode read FResizeMode write FResizeMode default rmStandard; // MarkB
    property ScreenSnap : boolean read FScreenSnap write FScreenSnap default False;
    property SnapBuffer : integer read FSnapBuffer write FSnapBuffer default 10;
    property ShowAppIcon : boolean read FShowAppIcon write SetShowAppIcon default True;
    property TitleButtons : TsTitleButtons read FTitleButtons write SetTitleButtons;
    property TitleIcon : TsTitleIcon read FTitleIcon write FTitleIcon;
    property TitleSkin : TsSkinSection read FTitleSkin write SetTitleSkin;
    property UseGlobalColor : boolean read FUseGlobalColor write SetUseGlobalColor default True;
    property OnSkinItem: TAddItemEvent read FOnSkinItem write FOnSkinItem;
  end;

{$IFNDEF NOTFORHELP}
{$IFDEF TNTUNICODE}
  TsSystemMenu = class(TTntPopupMenu)
{$ELSE}
  TsSystemMenu = class(TPopupMenu)
{$ENDIF}
  public
    ExtItemsCount : integer;
    FOwner : TsSkinProvider;
    FForm : TCustomForm;
    ItemRestore : TMenuItem;
    ItemMove : TMenuItem;
    ItemSize : TMenuItem;
    ItemMinimize : TMenuItem;
    ItemMaximize : TMenuItem;
    ItemClose : TMenuItem;

    constructor Create(AOwner : TComponent); override;
    procedure Generate;
    procedure UpdateItems(Full : boolean = False);
    procedure UpdateGlyphs;
    procedure MakeSkinItems;

    function VisibleRestore : boolean;
    function VisibleSize : boolean;
    function VisibleMin : boolean;
    function VisibleMax : boolean;
    function VisibleClose : boolean;

    function EnabledRestore : boolean;
    function EnabledMove    : boolean;
    function EnabledSize    : boolean;
    function EnabledMin     : boolean;
    function EnabledMax     : boolean;

    procedure RestoreClick(Sender: TObject);
    procedure MoveClick(Sender: TObject);
    procedure SizeClick(Sender: TObject);
    procedure MinClick(Sender: TObject);
    procedure MaxClick(Sender: TObject);
    procedure CloseClick(Sender: TObject);
    procedure SkinSelect(Sender: TObject);
    procedure ExtClick(Sender: TObject);
  end;

{$IFNDEF NOTFORHELP}
  TacAdapterItems = array of TacAdapterItem;
{$ENDIF}

  TacCtrlAdapter = class(TPersistent)
{$IFNDEF NOTFORHELP}
  public
    CtrlClass : TsCtrlClass;
    DefaultSection : string;
    Items : TacAdapterItems;
    Provider : TsSkinProvider;
    function IsControlSupported(Control : TComponent) : boolean; virtual;
    function Count : integer;
    constructor Create(AProvider: TsSkinProvider);
    destructor Destroy; override;
    function GetItem(Index : integer) : TacAdapterItem; virtual;
    function GetCommonData(Index : integer) : TsCommonData; virtual;
    function IndexOf(Ctrl : TWinControl) : integer;
    procedure AfterConstruction; override;
{$ENDIF} // NOTFORHELP
    procedure AddAllItems(OwnerCtrl : TWinControl = nil);
    procedure AddNewItem(Ctrl : TWinControl); overload; virtual;
    procedure AddNewItem(Ctrl : TWinControl; const SkinSection : string); overload; virtual;
    procedure RemoveItem(Index : integer); virtual;
    procedure RemoveAllItems;
    procedure CleanItems;
    procedure WndProc(var Message: TMessage); virtual;
  end;

const
  UserButtonsOffset = 8;
  ScrollWidth = 18;
  IconicHeight = 26;
  HTUDBTN = 1000;
{
  FormTitleHeight = 52;
  FormLeftBorderWidth = 4;
  FormRightBorderWidth = 4;
  FormBottomBorderWidth = 4;
}
var
  acM : TMessage;
  Style : LongInt;
  HotItem : TMenuItemData;
  SelectedMenuItem : TMenuItem;
{$IFNDEF ALITE}
  acTaskBarChanging : boolean = False;
{$ENDIF}

  // <<<<<<<<<<<<<<< MarkB <<<<<<<<<<<<<<<<<<<<<<
  bInProcess  : boolean = False;
  DoStartMove : boolean = False;
  bCapture    : Boolean = False;
  bFlag       : boolean = False;
  bRemoving   : boolean = False;
  bMode       : Boolean; //True - move, False - size
  deskwnd     : HWND;
  formDC      : HDC;
  ntop, nleft, nbottom, nright, nX, nY, nDirection, nMinHeight, nMinWidth, nDC : Integer;
  // >>>>>>>>>>>>>>>>> MarkB >>>>>>>>>>>>>>>>>>
  hDWMAPI: HMODULE = 0;

//function IsGripVisible(sp : TsSkinProvider) : boolean;
function AeroIsEnabled : boolean;
function UseAero : boolean;
procedure InitDwmApi;
procedure UseDwm(Handle : THandle; Active : boolean; Refresh : boolean = True);
function BigButtons(sp : TsSkinProvider) : boolean;
function IsBorderUnchanged(const BorderIndex : integer; const sm : TsSkinManager) : boolean;
function IsGripVisible(const sp : TsSkinProvider) : boolean;
procedure PaintGrip(const aDC : hdc; const sp : TsSkinProvider);
function CtrlIsReadyForHook(const Ctrl : TWinControl) : boolean;
procedure UpdateRgn(sp : TsSkinProvider; Repaint : boolean = True);
procedure FillArOR(sp : TsSkinProvider);
function GetRgnFromArOR(sp : TsSkinProvider; X : integer = 0; Y : integer = 0) : hrgn;
procedure UpdateSkinCaption(SkinProvider : TsSkinProvider);
function GetSkinProvider(Cmp : TComponent) : TsSkinProvider;
procedure DrawAppIcon(SkinProvider : TsSkinProvider);
function GetWindowWidth(Handle : hwnd) : integer;
function GetClientWidth(Handle : hwnd) : integer;
function GetWindowHeight(Handle : hwnd) : integer;
function GetClientHeight(Handle : hwnd) : integer;
procedure ForbidDrawing(sp : TsSkinProvider; MDIAlso : boolean = False);
procedure PermitDrawing(sp : TsSkinProvider; MDIAlso : boolean = False);
function HaveBorder(sp : TsSkinProvider) : boolean;
procedure UpdateMainForm;
{$ENDIF} // NOTFORHELP

implementation

uses math, sVclUtils, sBorders, sGraphUtils, sSkinProps, sGradient, sLabel, FlatSB, StdCtrls,
  sMaskData, acntUtils, sMessages, sStyleSimply, sStrings, {$IFDEF LOGGED} sDebugMsgs,{$ENDIF}
  sMDIForm{$IFDEF CHECKXP}, UxTheme, Themes{$ENDIF}, sAlphaGraph, ComCtrls, Grids, acDials, acGlow,
  ExtCtrls, sSpeedButton, Buttons, CommCtrl{$IFNDEF ALITE}, sFrameAdapter{$ENDIF};

var
  biClicked : boolean = False;
  MDICreating : boolean = False;
  ChildProvider : TsSkinProvider = nil;
  MDIIconsForm : TForm = nil;

_DwmSetWindowAttribute: function(hwnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer; cbAttribute: DWORD): HResult; stdcall;
_DwmIsCompositionEnabled: function (out pfEnabled: BOOL): HResult; stdcall;

const
  ModName = 'DWMAPI.DLL';

procedure InitDwmApi;
begin
  if hDWMAPI = 0 then hDWMAPI := LoadLibrary(ModName);
end;

function DwmSetWindowAttribute(hwnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer; cbAttribute: DWORD): HResult;
begin
  if Assigned(_DwmSetWindowAttribute) then Result := _DwmSetWindowAttribute(hwnd, dwAttribute, pvAttribute, cbAttribute) else begin
    InitDwmApi;
    Result := E_NOTIMPL;
    if hDWMAPI > 0 then begin
      _DwmSetWindowAttribute := GetProcAddress(hDWMAPI, 'DwmSetWindowAttribute');
      if Assigned(_DwmSetWindowAttribute) then
        Result := _DwmSetWindowAttribute(hwnd, dwAttribute, pvAttribute, cbAttribute);
    end;
  end;
end;

function AeroIsEnabled : boolean;
var
  b : Longbool;
begin
  Result := False;
  if (Win32MajorVersion >= 6) then begin
    b := False;
    if Assigned(_DwmIsCompositionEnabled) then Result := _DwmIsCompositionEnabled(b) = S_OK else begin
      InitDwmApi;
      if hDWMAPI > 0 then begin
        _DwmIsCompositionEnabled := GetProcAddress(hDWMAPI, 'DwmIsCompositionEnabled');
        if Assigned(_DwmIsCompositionEnabled) then Result := _DwmIsCompositionEnabled(b) = S_OK;
      end
    end;
  end;
  Result := Result and b;
end;

function UseAero : boolean;
begin
  Result := acAeroShadows and AeroIsEnabled;
end;

procedure UseDwm(Handle : THandle; Active : boolean; Refresh : boolean = True);
var
  Value : Longbool;
  Policy : Longint;
begin
  if not acAeroShadows or IsZoomed(Handle) then Exit;
  Value := Active;
  if Active then begin
//    Policy := 1; // DWMNCRP_DISABLED
//    DwmSetWindowAttribute(Handle, 1{DWMWA_NCRENDERING_POLICY}, @Policy, Sizeof(Policy));

    Policy := 2; // DWMNCRP_ENABLED
    DwmSetWindowAttribute(Handle, 2{DWMWA_NCRENDERING_POLICY}, @Policy, Sizeof(Policy));
    
    DwmSetWindowAttribute(Handle, 4{DWMWA_ALLOW_NCPAINT}, @Value, Sizeof(Value));
  end
  else begin
    DwmSetWindowAttribute(Handle, 4{DWMWA_ALLOW_NCPAINT}, @Value, Sizeof(Value));
    Policy := 0; // DWMNCRP_USEWINDOWSTYLE
    DwmSetWindowAttribute(Handle, 1{DWMWA_NCRENDERING_POLICY}, @Policy, Sizeof(Policy));
    SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_NOOWNERZORDER or SWP_FRAMECHANGED); // ?
  end;
end;

function BigButtons(sp : TsSkinProvider) : boolean;
begin
  Result := sp.Form.BorderStyle in [bsSingle, bsSizeable]
end;

function IsBorderUnchanged(const BorderIndex : integer; const sm : TsSkinManager) : boolean;
begin
  Result := (BorderIndex < 0) or (sm.ma[BorderIndex].ImageCount = 1)
end;

function IsGripVisible(const sp : TsSkinProvider) : boolean;
begin
  Result := (sp.GripMode = gmRightBottom) and (sp.Form.WindowState <> wsMaximized)
end;

procedure PaintGrip(const aDC : hdc; const sp : TsSkinProvider);
var
  i, w1, w2, dx, h1, h2, dy : integer;
  Bmp : TBitmap;
  p : TPoint;
  BG : TacBGInfo;
begin
  i := sp.FCommonData.SkinManager.GetMaskIndex(sp.FCommonData.SkinManager.ConstData.IndexGLobalInfo, s_GlobalInfo, s_GripImage);
  if sp.FCommonData.SkinManager.IsValidImgIndex(i) then begin
    Bmp := CreateBmp24(WidthOf(sp.FCommonData.SkinManager.ma[i].R) div sp.FCommonData.SkinManager.ma[i].ImageCount,
                       HeightOf(sp.FCommonData.SkinManager.ma[i].R) div (sp.FCommonData.SkinManager.ma[i].MaskType + 1));
    p := sp.RBGripPoint(i);
    p.X := p.X - sp.OffsetX;
    p.Y := p.Y - sp.OffsetY;


    BG.DrawDC := Bmp.Canvas.Handle;
    BG.Offset := p;
    BG.BgType := btUnknown;
    BG.R := Rect(0, 0, Bmp.Width, Bmp.Height);

    sp.FCommonData.FCacheBmp.Canvas.Lock;
    BG.PleaseDraw := False;
    GetBGInfo(@BG, sp.Form, True);
    if BG.BgType = btFill then begin
      w1 := sp.SkinData.SkinManager.MaskWidthRight(sp.SkinData.BorderIndex);
      h1 := sp.SkinData.SkinManager.MaskWidthBottom(sp.SkinData.BorderIndex);
      w2 := sp.SysBorderWidth;
      h2 := sp.SysBorderHeight;
      dx := w1 - w2;
      dy := h1 - h2;
      if (dx > 0) and (dy > 0) then
        // Right border
        BitBlt(Bmp.Canvas.Handle, Bmp.Width - dx, 0, dx, Bmp.Height,
                     sp.SkinData.FCacheBmp.Canvas.Handle, sp.SkinData.FCacheBmp.Width - w1, sp.SkinData.FCacheBmp.Height - h2 - Bmp.Height, SRCCOPY);
        // Bottom border
        BitBlt(Bmp.Canvas.Handle, 0, Bmp.Height - dy, Bmp.Width, dy,
                     sp.SkinData.FCacheBmp.Canvas.Handle, sp.SkinData.FCacheBmp.Width - w2 - Bmp.Width, sp.SkinData.FCacheBmp.Height - h1, SRCCOPY);
    end;
    sp.FCommonData.FCacheBmp.Canvas.UnLock;

    DrawSkinGlyph(Bmp, Point(0, 0), 0, 1, sp.FCommonData.SkinManager.ma[i], MakeCacheInfo(Bmp));

    BitBlt(aDC, p.X + sp.OffsetX, p.Y + sp.OffsetY, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    FreeAndNil(Bmp);
  end;
end;

function CtrlIsReadyForHook(const Ctrl : TWinControl) : boolean;
begin
  Result := Ctrl.HandleAllocated{$IFDEF TNTUNICODE} and Ctrl.Visible and (Ctrl.Parent <> nil){$ENDIF} // Showing is False when Parent changed
end;

procedure MakeCaptForm(sp : TsSkinProvider; Full : boolean = False);
var
  p : TPoint;
begin
  if not sp.FDrawNonClientArea then Exit;
  if sp.CaptForm = nil then begin
    sp.CaptForm := TForm.Create(Application);
    sp.CaptForm.Tag := ExceptTag;
    sp.CaptForm.OnPaint := sp.CaptFormPaint;
    sp.OldCaptFormProc := sp.CaptForm.WindowProc;
    sp.CaptForm.WindowProc := sp.NewCaptFormProc;
    sp.CaptForm.BorderStyle := bsNone;
  end;
  sp.CaptForm.Visible := False;
  if (sp.Form.FormStyle = fsMDIChild) then begin
    p := TsSkinProvider(MDISkinProvider).Form.ClientToScreen(Point(sp.Form.Left + GetAlignShift(TsSkinProvider(MDISkinProvider).Form, alLeft, True) + 2, sp.Form.Top + GetAlignShift(TsSkinProvider(MDISkinProvider).Form, alTop, True) + 2));
    SetWindowPos(sp.CaptForm.Handle, 0, p.x, p.y, sp.Form.Width, sp.HeaderHeight, SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOREDRAW or SWP_NOSENDCHANGING or SWP_NOOWNERZORDER);
  end
  else begin
    if GetWindowLong(sp.Form.Handle, GWL_EXSTYLE) and WS_EX_TOPMOST = WS_EX_TOPMOST // sp.Form.FormStyle = fsStayOnTop
      then SetWindowPos(sp.CaptForm.Handle, HWND_TOPMOST, sp.Form.Left, sp.Form.Top, sp.Form.Width, iffi(Full, sp.Form.Height, sp.HeaderHeight),
        SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOREDRAW or SWP_NOSENDCHANGING or SWP_NOOWNERZORDER)
      else SetWindowPos(sp.CaptForm.Handle, 0, sp.Form.Left, sp.Form.Top, sp.Form.Width, iffi(Full, sp.Form.Height, sp.HeaderHeight),
        SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOREDRAW or SWP_NOSENDCHANGING or SWP_NOOWNERZORDER);
  end;
end;

procedure KillCaptForm(sp : TsSkinProvider);
begin
  if sp.CaptForm <> nil then begin
    sp.CaptForm.WindowProc := sp.OldCaptFormProc;
    FreeAndNil(sp.CaptForm);
  end;
end;

procedure FillArOR(sp : TsSkinProvider);
var
  i : integer;
begin
  SetLength(sp.ArOR, 0);
  if sp.SkinData.SkinManager.IsValidImgIndex(sp.SkinData.BorderIndex) then begin
    // TopBorderRgn
    AddRgn(sp.ArOR, sp.CaptionWidth, sp.SkinData.SkinManager.ma[sp.SkinData.BorderIndex], 0, False);
    // BottomBorderRgn
    AddRgn(sp.ArOR, sp.CaptionWidth, sp.SkinData.SkinManager.ma[sp.SkinData.BorderIndex], sp.Form.Height - sp.SkinData.SkinManager.ma[sp.SkinData.BorderIndex].WB, True);
  end;

  // TitleRgn
  i := sp.SkinData.SkinManager.GetSkinIndex(sp.TitleSkinSection);
  if sp.SkinData.SkinManager.IsValidSkinIndex(i) then begin
    i := sp.SkinData.SkinManager.GetMaskIndex(i, sp.TitleSkinSection, s_BordersMask);
    if sp.SkinData.SkinManager.IsValidImgIndex(i) then AddRgn(sp.ArOR, sp.CaptionWidth, sp.SkinData.SkinManager.ma[i], 0, False);
  end;
end;

procedure UpdateRgn(sp : TsSkinProvider; Repaint : boolean = True);
const
  BE_ID = $41A2;
  CM_BEWAIT = CM_BASE + $0C4D;
var
  rgn : HRGN;
  R : TRect;
begin
  if sp.DrawNonClientArea and not sp.InMenu and (HaveBorder(sp) or IsIconic(sp.Form.Handle)) then with sp do begin
    if not FirstInitialized then if SendMessage(Form.Handle, CM_BEWAIT, BE_ID, 0) = BE_ID then Exit; // BE compatibility
    if ((sp.Form.Parent = nil) or (TForm(sp.Form).DragKind <> dkDock)) {regions changing disabled when docking used} then begin
      RgnChanging := True;
      if (sp.Form.WindowState = wsMaximized) and (sp.Form.Constraints.MaxWidth = 0) and (sp.Form.Constraints.MaxHeight = 0) and (Form.FormStyle <> fsMDIChild) then begin // v6.02
        R := Rect(0, 0, sp.Form.Width, sp.Form.Height);
        InflateRect(R, - sp.SysBorderWidth, - sp.SysBorderHeight);
        rgn := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
      end
      else rgn := GetRgnFromArOR(sp);
      sp.RgnChanged := True;
      SetWindowRgn(Form.Handle, rgn, Repaint); // True - repainting required
    end
    else SetWindowRgn(Form.Handle, 0, False);
  end;
end;

function GetRgnFromArOR(sp : TsSkinProvider; X : integer = 0; Y : integer = 0) : hrgn;
var
  l, i : integer;
  subrgn : HRGN;
begin
  l := Length(sp.ArOR);
  Result := CreateRectRgn(X, Y, sp.CaptionWidth + X, sp.Form.Height + Y);
  if l > 0 then for i := 0 to l - 1 do begin
    subrgn := CreateRectRgn(sp.ArOR[i].Left + X, sp.ArOR[i].Top + Y, sp.ArOR[i].Right + X, sp.ArOR[i].Bottom + Y);
    CombineRgn(Result, Result, subrgn, RGN_DIFF);
    DeleteObject(subrgn);
  end;
end;

procedure RefreshFormScrolls(SkinProvider : TsSkinProvider; var ListSW : TacScrollWnd; Repaint : boolean);
begin
  if not (csDestroying in SkinProvider.ComponentState) and SkinProvider.Form.HandleAllocated and TForm(SkinProvider.Form).AutoScroll {v5.01} then begin
    if SkinProvider.SkinData.Skinned then begin
      UninitializeFlatSB(SkinProvider.Form.Handle);
      if (ListSW <> nil) and ListSW.Destroyed then FreeAndNil(ListSW);
      if ListSW = nil then begin
        ListSW := TacScrollWnd.Create(SkinProvider.Form.Handle, SkinProvider.SkinData, SkinProvider.SkinData.SkinManager, '', False);
      end;
    end
    else begin
      if ListSW <> nil then FreeAndNil(ListSW);
      InitializeFlatSB(SkinProvider.Form.Handle);
    end;
  end;
end;

procedure ForbidDrawing(sp : TsSkinProvider; MDIAlso : boolean = False);
begin
  sp.SkinData.BeginUpdate;
  sp.Form.Perform(WM_SETREDRAW, 0, 0);
  if MDIAlso and (TForm(sp.Form).FormStyle = fsMDIChild) and Assigned(MDISkinProvider) then begin
    TsSkinProvider(MDISkinProvider).SkinData.BeginUpdate;
    TsSkinProvider(MDISkinProvider).Form.Perform(WM_SETREDRAW, 0, 0);
  end;
end;

procedure PermitDrawing(sp : TsSkinProvider; MDIAlso : boolean = False);
begin
  sp.SkinData.EndUpdate;
  sp.Form.Perform(WM_SETREDRAW, 1, 0);
  if MDIAlso and (TForm(sp.Form).FormStyle = fsMDIChild) and Assigned(MDISkinProvider) then begin
    TsSkinProvider(MDISkinProvider).SkinData.EndUpdate;
    TsSkinProvider(MDISkinProvider).Form.Perform(WM_SETREDRAW, 1, 0);
  end;
end;

function HaveBorder(sp : TsSkinProvider) : boolean;
begin
  Result := (sp.Form.BorderStyle <> bsNone) or (TForm(sp.Form).FormStyle = fsMDIChild)
end;

procedure UpdateSkinCaption(SkinProvider : TsSkinProvider);
var
  DC, SavedDC : hdc;
begin
  if InAnimationProcess then Exit;
  with SkinProvider do
  if (TForm(Form).FormStyle = fsMDIChild) and (Form.WindowState = wsMaximized) then begin
    TsSkinProvider(MDISkinProvider).FCommonData.BGChanged := True;
    DC := GetWindowDC(TsSkinProvider(MDISkinProvider).Form.Handle);
    SavedDC := SaveDC(DC);
    try
      TsSkinProvider(MDISkinProvider).PaintCaption(DC);
    finally
      RestoreDC(DC, SavedDC);
      ReleaseDC(TsSkinProvider(MDISkinProvider).Form.Handle, DC);
    end;
  end
  else begin
    FCommonData.BGChanged := True;
    DC := GetWindowDC(Form.Handle);
    SavedDC := SaveDC(DC);
    try
      PaintCaption(DC);
    finally
      RestoreDC(DC, SavedDC);
      ReleaseDC(Form.Handle, DC);
    end;
  end;
end;

function IsSizeBox(Handle : hWnd) : boolean;
var
  Style: LongInt;
begin
  Style := GetWindowLong(Handle, GWL_STYLE);
  Result := Style and WS_SIZEBOX = WS_SIZEBOX;
end;

function GetNearestSize(Max : integer) : integer;
begin
  case Max of
    0..8 : Result := 0;
    9..16 : Result := 8;
    17..27 : Result := 16;
    28..32 : Result := 32
    else Result := 32;
  end;
end;

function GetSkinProvider(Cmp : TComponent) : TsSkinProvider;
var
  c : TComponent;
  sp : integer;
begin
  Result := nil;
  c := Cmp;
  while Assigned(c) and not (c is TCustomForm) do c := c.Owner;
  if (c is TCustomForm) then begin
    sp := SendMessage(TCustomForm(c).Handle, SM_ALPHACMD, MakeWParam(0, AC_GETPROVIDER), 0);
    if sp <> 0 then Result := TsSkinProvider(sp);
  end;
end;

function TitleIconWidth(SP : TsSkinProvider) : integer;
begin
  if SP.IconVisible then begin
    if SP.TitleIcon.Width <> 0 then Result := SP.TitleIcon.Width else Result := GetNearestSize(SP.CaptionHeight);
  end
  else Result := 0;
end;

function TitleIconHeight(SP : TsSkinProvider) : integer;
begin
  if SP.IconVisible then begin
    if SP.TitleIcon.Height <> 0 then Result := SP.TitleIcon.Height else Result := GetNearestSize(SP.CaptionHeight);
  end
  else Result := 0;
end;

procedure DrawAppIcon(SkinProvider : TsSkinProvider);
var
  iW, iH : integer;
begin
  with SkinProvider do if IconVisible then begin
    if not TitleIcon.Glyph.Empty then begin
      TitleIcon.Glyph.Transparent := True;
      TitleIcon.Glyph.TransparentColor := clFuchsia;
      iW := iffi(TitleIcon.Width = 0, GetNearestSize(HeaderHeight - 2), TitleIcon.Width);
      iH := iffi(TitleIcon.Width = 0, GetNearestSize(HeaderHeight - 2), TitleIcon.Height);
      if TitleIcon.Glyph.PixelFormat = pf32bit then begin
        CopyByMask(Rect(IconRect.Left, IconRect.Top, IconRect.Left + TitleIcon.Glyph.Width, IconRect.Left + TitleIcon.Glyph.Height),
                   Rect(0, 0, TitleIcon.Glyph.Width, TitleIcon.Glyph.Height),
                   FCommonData.FCacheBmp,
                   TitleIcon.Glyph,
                   EmptyCI, False);
      end
      else begin
        FCommonData.FCacheBmp.Canvas.StretchDraw(
             Rect(IconRect.Left, IconRect.Top,
                  IconRect.Left + iW,
                  IconRect.Top + iH
                 ), TitleIcon.Glyph);
      end;
    end
    else if TForm(Form).Icon.Handle <> 0 then begin
           DrawIconEx(FCommonData.FCacheBmp.Canvas.Handle,
                 IconRect.Left, IconRect.Top,
                 TForm(Form).Icon.Handle,
                 TitleIconWidth(SkinProvider),
                 TitleIconHeight(SkinProvider), 0, 0, DI_NORMAL);
        end
        else if Application.Icon.Handle <> 0 then begin
           DrawIconEx(FCommonData.FCacheBmp.Canvas.Handle,
                 IconRect.Left, IconRect.Top,
                 Application.Icon.Handle,
                 TitleIconWidth(SkinProvider),
                 TitleIconHeight(SkinProvider), 0, 0, DI_NORMAL);
        end
        else begin
          iW := iffi(TitleIcon.Width = 0, CaptionHeight - IconRect.Top, TitleIcon.Width);
          iH := iffi(TitleIcon.Height = 0, CaptionHeight - IconRect.Top, TitleIcon.Height);
          if (iH > 16) and (AppIconLarge <> nil)
            then DrawIconEx(FCommonData.FCacheBmp.Canvas.Handle,
                            IconRect.Left, IconRect.Top,
                            AppIconLarge.Handle,
                            iW,
                            iH,
                            0, 0, di_Normal)
            else if (AppIcon <> nil) then DrawIconEx(FCommonData.FCacheBmp.Canvas.Handle,
                            IconRect.Left, IconRect.Top,
                            AppIcon.Handle,
                            iW,
                            iH,
                            0, 0, di_Normal)
           else DrawIconEx(FCommonData.FCacheBmp.Canvas.Handle,
                            IconRect.Left, IconRect.Top,
                            LoadIcon(0, IDI_APPLICATION),
                            TitleIconWidth(SkinProvider),
                            TitleIconHeight(SkinProvider),
                            0, 0, di_Normal);
        end;
  end;
end;

function GetWindowWidth(Handle : hwnd) : integer;
var
  R : TRect;
begin
  GetWindowRect(Handle, R);
  Result := WidthOf(R)
end;

function GetClientWidth(Handle : hwnd) : integer;
var
  R : TRect;
begin
  GetClientRect(Handle, R);
  Result := WidthOf(R)
end;

function GetWindowHeight(Handle : hwnd) : integer;
var
  R : TRect;
begin
  GetWindowRect(Handle, R);
  Result := HeightOf(R)
end;

function GetClientHeight(Handle : hwnd) : integer;
var
  R : TRect;
begin
  GetClientRect(Handle, R);
  Result := HeightOf(R)
end;

{ TsSkinProvider }

procedure TsSkinProvider.AfterConstruction;
begin
  inherited;
  if Assigned(FCommonData.SkinManager) and FCommonData.SkinManager.Active and not (csDesigning in ComponentState) then begin
    PrepareForm;
  end;
end;

function TsSkinProvider.BarWidth(i : integer): integer;
begin
  if Assigned(FCommonData.SkinManager.ma[i].Bmp) then begin
    Result := (FCommonData.SkinManager.ma[i].Bmp.Width div 9) * 2 + TitleBtnsWidth;
  end
  else begin
    Result := (WidthOf(FCommonData.SkinManager.ma[i].R) div (3 * FCommonData.SkinManager.ma[i].ImageCount)) * 2 + TitleBtnsWidth;
  end;
end;

function TsSkinProvider.BorderHeight: integer;
begin
  Result := SysBorderHeight + Form.BorderWidth
end;

function TsSkinProvider.BorderWidth: integer;
begin
  Result := SysBorderHeight + Form.BorderWidth
end;

function TsSkinProvider.ButtonHeight: integer;
begin
  if FCommonData.SkinManager.IsValidImgIndex(ButtonClose.ImageIndex) then begin
    if FCommonData.SkinManager.ma[ButtonClose.ImageIndex].Bmp = nil
      then Result := HeightOf(FCommonData.SkinManager.ma[ButtonClose.ImageIndex].R) div (1 + FCommonData.SkinManager.ma[ButtonClose.ImageIndex].MaskType)
      else Result := FCommonData.SkinManager.ma[ButtonClose.ImageIndex].Bmp.Height div 2;
  end
  else Result := 21;
end;

function TsSkinProvider.SysButtonsCount: integer;
begin
  Result := 0;
  if SystemMenu.VisibleClose{(biSystemMenu in Form.BorderIcons)} and Assigned(SystemMenu) then begin
    inc(Result);
    if SystemMenu.VisibleMax then inc(Result);
    if SystemMenu.VisibleMin then inc(Result);
    if (biHelp in Form.BorderIcons) then inc(Result);
  end;
end;

function TsSkinProvider.SysButtonWidth(Btn : TsCaptionButton): integer;
begin
  if FCommonData.SkinManager.IsValidImgIndex(Btn.ImageIndex) then begin
    if FCommonData.SkinManager.ma[Btn.ImageIndex].Bmp = nil
     then Result := WidthOf(FCommonData.SkinManager.ma[Btn.ImageIndex].R) div FCommonData.SkinManager.ma[Btn.ImageIndex].ImageCount
     else Result := FCommonData.SkinManager.ma[Btn.ImageIndex].Bmp.Width div FCommonData.SkinManager.ma[Btn.ImageIndex].ImageCount;
  end
  else Result := 21;
end;

function TsSkinProvider.CaptionHeight: integer;
begin
  if HaveBorder(Self) and (GetWindowLong(Form.Handle, GWL_STYLE) and WS_CAPTION = WS_CAPTION) or IsIconic(Form.Handle)
    then begin
      if Form.BorderStyle in [bsToolWindow, bsSizeToolWin]
        then Result := GetSystemMetrics(SM_CYSMCAPTION)
        else Result := GetSystemMetrics(SM_CYCAPTION)
    end
    else Result := 0;
end;

constructor TsSkinProvider.Create(AOwner: TCOmponent);
var
  i : integer;
begin
  inherited Create(AOwner);
  FDrawNonClientArea := True;
  Form := TForm(GetOwnerForm(Self));
  DwmInitialized := False;

  if AOwner is TCustomFrame then ShowWarning('TsSkinProvider component must be used with forms only!'#13#10'For frames skinning may be used TsFrameAdapter component.');
  fAnimating := False;
  HaveSysMenu := False;
  InMenu := False;
  ShowAction := saIgnore;

  FCommonData := TsCommonData.Create(Self, False);
  FCommonData.SkinSection := s_Form;
  FCommonData.COC := COC_TsSkinProvider;
  if Form.ControlState <> [] then FCommonData.Updating := True;

  FResizeMode := rmStandard;
  FUseGlobalColor := True;
  MDIForm := nil;
  AeroInvalidate := False;

  MenuChanged := True;
  FMakeSkinMenu := DefMakeSkinMenu;
  MenusInitialized := False;
  RgnChanged := True;
  RgnChanging := False;
  FScreenSnap := False;
  FSnapBuffer := 10;
{$IFDEF CHECKXP}
//  StdBgIsUsed := True;
{$ENDIF}

  FShowAppIcon := True;
  FCaptionAlignment := taLeftJustify;
  FTitleIcon := TsTitleIcon.Create;
  FTitleButtons := TsTitleButtons.Create(Self);
  FGripMode := gmNone;
  ClearButtons := False;
  OldCaptFormProc := nil;

  for i := 0 to Form.ComponentCount - 1 do begin
    if (Form.Components[i] is TsSkinProvider) and (Form.Components[i] <> Self) then begin
      Form := nil;
      if (csDesigning in ComponentState) then ShowWarning('Only one instance of the TsSkinProvider component is allowed!');
      break;
    end;
  end;

  if not (csDesigning in ComponentState) and (Form <> nil) then begin
    Form.DoubleBuffered := False;
    TempBmp := TBitmap.Create;
    MenuLineBmp := CreateBmp24(0, 0);

    ClearButtons := True;

    SetLength(ArOR, 0);

    FLinesCount := -1;

    OldWndProc := Form.WindowProc;
    Form.WindowProc := NewWndProc;
    IntSkinForm(Form);

    FormActive := True;
  end;
end;

destructor TsSkinProvider.Destroy;
begin
  if not (csDesigning in ComponentState) then begin
    if Form <> nil then begin
      IntUnskinForm(Form);
      Form.WindowProc := OldWndProc;
      if (Form.FormStyle = fsMDIChild) and Assigned(Form.Menu) then begin
        if Assigned(MDISkinProvider) and
             not (csDestroying in TsSkinProvider(MDISkinProvider).ComponentState) and
             not (csDestroying in TsSkinProvider(MDISkinProvider).Form.ComponentState)
               then begin
          TsSkinProvider(MDISkinProvider).FCommonData.BGChanged := True;
          TsSkinProvider(MDISkinProvider).FLinesCount := -1;
          SendMessage(TsSkinProvider(MDISkinProvider).Form.Handle, WM_NCPAINT, 0, 0);
        end;
      end;
      if MDISkinProvider = Self then begin
        if MDIForm <> nil then HookMDI(False);
        MDISkinProvider := nil;
      end;
    end;
    if Assigned(SystemMenu) then FreeAndNil(SystemMenu);
    if Assigned(TempBmp) then FreeAndnil(TempBmp);
    if Assigned(MenuLineBmp) then FreeAndNil(MenuLineBmp);
    if TitleBG <> nil then FreeAndNil(TitleBG);
  end;

  if ChildProvider = Self then ChildProvider := nil;

  if Assigned(FTitleIcon) then FreeAndNil(FTitleIcon);
  if Assigned(FTitleButtons) then FreeAndNil(FTitleButtons);

  if Assigned(Adapter) then FreeAndNil(Adapter);
  if ListSW <> nil then FreeAndNil(ListSW);
  if FCommonData <> nil then FreeAndNil(FCommonData);
  inherited Destroy;
end;

procedure TsSkinProvider.RepaintButton(i: integer);
var
  DC, SavedDC : hdc;
  CurButton : ^TsCaptionButton;
  cx, ind, x, y : integer;
  BtnDisabled : boolean;
  CI : TCacheInfo;
  R : TRect;
begin
  CurButton := nil;
  case i of
    HTCLOSE      : CurButton := @ButtonClose;
    HTMAXBUTTON  : CurButton := @ButtonMax;
    HTMINBUTTON  : CurButton := @ButtonMin;
    HTHELP       : CurButton := @ButtonHelp;
    HTCHILDCLOSE : CurButton := @MDIClose;
    HTCHILDMAX   : CurButton := @MDIMax;
    HTCHILDMIN   : CurButton := @MDIMin
    else if Between(i, HTUDBTN, (HTUDBTN + TitleButtons.Count - 1))
           then CurButton := @TitleButtons.Items[i - HTUDBTN].BtnData;
  end;
  if (CurButton <> nil) and (CurButton^.State <> -1) then begin
    BtnDisabled := False;
    if CurButton^.Rect.Left <= IconRect.Right then Exit;
    cx := CaptionWidth - CurButton^.Rect.Left;
    BitBlt(FCommonData.FCacheBmp.Canvas.Handle, // Restore a button BG
           CurButton^.Rect.Left, CurButton^.Rect.Top, SysButtonwidth(CurButton^), ButtonHeight,
           TempBmp.Canvas.Handle, TempBmp.Width - cx, CurButton^.Rect.Top, SRCCOPY);
    // if Max btn and form is maximized then Norm btn
    if (i = HTMAXBUTTON) and (Form.WindowState = wsMaximized) then ind := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinIndex, s_GlobalInfo, s_BorderIconNormalize)
    else case i of
      HTCHILDMIN : begin
        ind := CurButton^.ImageIndex;
        if ChildProvider <> nil then BtnDisabled := not ChildProvider.SystemMenu.EnabledMin;
        if BtnDisabled then Exit;
      end;
      HTCHILDMAX : begin // Correction of the Maximize button (may be Normalize)
        if Assigned(Form.ActiveMDIChild) and (Form.ActiveMDIChild.WindowState = wsMaximized) then begin
          ind := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinManager.ConstData.IndexGlobalInfo, s_GlobalInfo, s_SmallIconNormalize);
          if ind < 0 then ind := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinIndex, s_GlobalInfo, s_BorderIconNormalize) // For compatibility
        end
        else ind := CurButton^.ImageIndex;
        if ChildProvider <> nil then BtnDisabled := not ChildProvider.SystemMenu.EnabledRestore;
      end
      else if IsIconic(Form.Handle) then begin
        case i of
          HTMINBUTTON : begin
            ind := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinManager.ConstData.IndexGlobalInfo, s_GlobalInfo, s_BorderIconNormalize);
            if ind < 0 then ind := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinIndex, FCommonData.SkinSection, s_BorderIconNormalize); // For compatibility
          end;
          HTMAXBUTTON : begin
            ind := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinManager.ConstData.IndexGlobalInfo, s_GlobalInfo, s_BorderIconMaximize);
            if ind < 0 then ind := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinIndex, FCommonData.SkinSection, s_BorderIconMaximize); // For compatibility
            if not SystemMenu.EnabledMax then BtnDisabled := True;
          end
          else ind := CurButton^.ImageIndex;
        end
      end else ind := CurButton^.ImageIndex;
    end;
    if FCommonData.SkinManager.IsValidImgIndex(ind) then begin // Drawing of the button from skin
      if i < HTUDBTN // if not user defined
        then DrawSkinGlyph(FCommonData.FCacheBmp, Point(CurButton^.Rect.Left, CurButton^.Rect.Top),
               CurButton^.State, 1 + integer(not FormActive or BtnDisabled) * integer(not (CurButton^.State > 0) or BtnDisabled), FCommonData.SkinManager.ma[ind], MakeCacheInfo(SkinData.FCacheBmp))
        else if (TitleButtons.Items[i - HTUDBTN].UseSkinData)
             then DrawSkinGlyph(FCommonData.FCacheBmp, Point(CurButton^.Rect.Left, CurButton^.Rect.Top),
               CurButton^.State, 1 + integer(not FormActive) * integer(not (CurButton^.State > 0)), FCommonData.SkinManager.ma[ind], MakeCacheInfo(SkinData.FCacheBmp));
    end;
    // If user Glyph is defined
    if (i >= HTUDBTN) and Assigned(TitleButtons.Items[i - HTUDBTN].Glyph) then begin
      if TitleButtons.Items[i - HTUDBTN].Glyph.PixelFormat = pf32bit then begin
        x := CurButton^.Rect.Left + integer(CurButton^.State = 2) + (WidthOf(CurButton^.Rect) - TitleButtons.Items[i - HTUDBTN].Glyph.Width) div 2;
        y := CurButton^.Rect.Top + integer(CurButton^.State = 2) + (HeightOf(CurButton^.Rect) - TitleButtons.Items[i - HTUDBTN].Glyph.Height) div 2;
        CI := MakeCacheInfo(FCommonData.FCacheBmp, x, y);
        CopyByMask(Rect(x, y, x + TitleButtons.Items[i - HTUDBTN].Glyph.Width, y + TitleButtons.Items[i - HTUDBTN].Glyph.Height),
                   Rect(0, 0, TitleButtons.Items[i - HTUDBTN].Glyph.Width, TitleButtons.Items[i - HTUDBTN].Glyph.Height),
                   FCommonData.FCacheBmp,
                   TitleButtons.Items[i - HTUDBTN].Glyph,
                   CI, True);
      end
      else begin
        TitleButtons.Items[i - HTUDBTN].Glyph.PixelFormat := pf24bit;
        CopyTransBitmaps(FCommonData.FCacheBmp, TitleButtons.Items[i - HTUDBTN].Glyph,
             CurButton^.Rect.Left + integer(CurButton^.State = 2) + (WidthOf(CurButton^.Rect) - TitleButtons.Items[i - HTUDBTN].Glyph.Width) div 2,
             CurButton^.Rect.Top + integer(CurButton^.State = 2) + (HeightOf(CurButton^.Rect) - TitleButtons.Items[i - HTUDBTN].Glyph.Height) div 2,
             ColorToSColor(TitleButtons.Items[i - HTUDBTN].Glyph.Canvas.Pixels[0, TitleButtons.Items[i - HTUDBTN].Glyph.Height - 1]));
      end;
    end;
    // Copying to form
    DC := GetWindowDC(Form.Handle);
    SavedDC := SaveDC(DC);
    try
      BitBlt(DC, CurButton^.Rect.Left, CurButton^.Rect.Top, WidthOf(CurButton^.Rect), HeightOf(CurButton^.Rect),
        FCommonData.FCacheBmp.Canvas.Handle, CurButton^.Rect.Left, CurButton^.Rect.Top, SRCCOPY);
      if (CurButton^.State = 1) and (i in [HTCLOSE, HTMAXBUTTON, HTMINBUTTON]) then begin
        case i of
          HTCLOSE      : x := FCommonData.SkinManager.SkinData.BICloseGlow;
          HTMAXBUTTON  : x := FCommonData.SkinManager.SkinData.BIMaxGlow;
          HTMINBUTTON  : x := FCommonData.SkinManager.SkinData.BIMinGlow;
        end;
        if x > 0 then begin
          case i of
            HTCLOSE      : y := FCommonData.SkinManager.SkinData.BICloseGlowMargin;
            HTMAXBUTTON  : y := FCommonData.SkinManager.SkinData.BIMaxGlowMargin;
            HTMINBUTTON  : y := FCommonData.SkinManager.SkinData.BIMinGlowMargin;
          end;
          GetWindowRect(Form.Handle, R);
          OffsetRect(R, CurButton^.Rect.Left, CurButton^.Rect.Top);
          R.Right := R.Left + WidthOf(CurButton^.Rect);
          R.Bottom := R.Top + HeightOf(CurButton^.Rect);

          if SkinData.SkinManager.AllowGlowing and (Form.Parent = nil) and (Form.FormStyle <> fsMDIChild) then
            CurButton^.GlowID := ShowGlow(R, R, s_GlobalInfo, FCommonData.SkinManager.ma[CurButton.ImageIndex].PropertyName + s_Glow, y, 255, Form.Handle, FCommonData.SkinManager);
        end;
      end
      else if CurButton^.GlowID <> -1 then begin
        HideGlow(CurButton^.GlowID);
        CurButton^.GlowID := -1;
      end;
    finally
      RestoreDC(DC, SavedDC);
      ReleaseDC(Form.Handle, DC);
    end;
  end
  else if (CurButton <> nil) and (CurButton^.GlowID <> -1) then begin
    HideGlow(CurButton^.GlowID);
    CurButton^.GlowID := -1;
  end;
end;

function TsSkinProvider.HTProcess(Message : TWMNCHitTest): integer;
const
  BtnSpacing = 1;
var
  p : TPoint;
  cy1, cy2 : integer;
  i, SysBtnCount, BtnIndex : integer;
  GripVisible : boolean;
  R, hrect, vrect : TRect;
  function GetBtnIndex(x : integer) : integer;
  var
    i, c : integer;
  begin
    Result := 0;
    c := 0;
    if SystemMenu.VisibleClose and Assigned(SystemMenu) then begin
      inc(c);
      if Between(x, ButtonClose.Rect.Left, ButtonClose.Rect.Right) then begin
        Result := c;
        Exit;
      end;
      if SystemMenu.VisibleMax then begin
        inc(c);
        if Between(x, ButtonMax.Rect.Left, ButtonMax.Rect.Right) then begin
          Result := c;
          Exit;
        end;
      end;
      if SystemMenu.VisibleMin then begin
        inc(c);
        if Between(x, ButtonMin.Rect.Left, ButtonMin.Rect.Right) then begin
          Result := c;
          Exit;
        end;
      end;
      if (biHelp in Form.BorderIcons) then begin
        inc(c);
        if Between(x, ButtonHelp.Rect.Left, ButtonHelp.Rect.Right) then begin
          Result := c;
          Exit;
        end;
      end;
    end;       
    for i := 0 to TitleButtons.Count - 1 do begin
      inc(c);
      if Between(x, TitleButtons[i].BtnData.Rect.Left, TitleButtons[i].BtnData.Rect.Right) then begin
        Result := c;
        Exit;
      end;
    end;
  end;
begin
  p := CursorToPoint(Message.XPos, Message.YPos);
  if (FCommonData.SkinManager.SkinData.BIVAlign = 1) and (Form.BorderStyle in [bsSingle, bsSizeable]) then begin
    cy1 := 1;
    cy2 := cy1 + ButtonHeight - 1;
  end
  else begin
    cy1 := (CaptionHeight - ButtonHeight + SysBorderHeight) div 2;    
    cy2 := cy1 + ButtonHeight;
  end;

  if Between(p.y, cy1, cy2) then begin // If in buttons
    if Between(p.x, SysBorderWidth, SysBorderWidth + WidthOf(IconRect)) // If system menu icon
      then begin SetHotHT(HTSYSMENU); Result := HTSYSMENU; Exit; end;
    // Title button?
    SysBtnCount := 0;
    if SystemMenu.VisibleClose then begin
      inc(SysBtnCount);
      if SystemMenu.VisibleMax then inc(SysBtnCount);
      if SystemMenu.VisibleMin or IsIconic(Form.Handle) then inc(SysBtnCount);
      if biHelp in Form.BorderIcons then inc(SysBtnCount);
    end;
    BtnIndex := GetBtnIndex(p.x);

    if (BtnIndex > 0) and (BtnIndex <= SysBtnCount) then begin // If system button
      case BtnIndex of
        1 : if SystemMenu.VisibleClose then begin
          SetHotHT(HTCLOSE); Result := HTCLOSE; Exit;
        end;
        2 : begin
          if SystemMenu.VisibleMax then begin
            if (SystemMenu.EnabledMax or (SystemMenu.EnabledRestore and not IsIconic(Form.Handle))) then begin
              SetHotHT(HTMAXBUTTON); Result := HTMAXBUTTON; Exit;
            end
            else begin
              SetHotHT(HTCAPTION); Result := HTCAPTION; Exit;
            end;
          end
          else if (SystemMenu.VisibleMin) or IsIconic(Form.Handle) then begin
            if SystemMenu.EnabledMin then begin
              SetHotHT(HTMINBUTTON); Result := HTMINBUTTON; Exit;
            end
            else begin
              SetHotHT(HTCAPTION); Result := HTCAPTION; Exit;
            end;
          end
          else if (biHelp in Form.BorderIcons) then begin
            SetHotHT(HTHELP); Result := HTHELP; EXIT;
          end;
        end;
        3 : begin
          if (SystemMenu.VisibleMin) or IsIconic(Form.Handle) then begin
            if not IsIconic(Form.Handle) then begin
              if SystemMenu.EnabledMin then begin
                SetHotHT(HTMINBUTTON); Result := HTMINBUTTON; Exit;
              end
              else begin
                SetHotHT(HTCAPTION); Result := HTCAPTION; Exit;
              end;
            end
            else begin
              SetHotHT(HTMINBUTTON); Result := HTMINBUTTON; Exit;
            end;
          end
          else if (biHelp in Form.BorderIcons) then begin
            SetHotHT(HTHELP); Result := HTHELP; EXIT;
          end;
        end;
        4 : if (biHelp in Form.BorderIcons) and SystemMenu.VisibleMax then begin
          SetHotHT(HTHELP); Result := HTHELP; EXIT;
        end;
      end;
    end
    else if (BtnIndex > SysBtnCount) and (BtnIndex <= TitleButtons.Count + SysBtnCount) then begin // UDF button
      BtnIndex := BtnIndex - SysBtnCount - 1;
      if TitleButtons.Items[BtnIndex].Enabled then begin
        Result := HTUDBTN + BtnIndex;
        SetHotHT(Result);
      end
      else begin
        Result := HTCAPTION;
      end;
      Exit;
    end
    else begin
      Result := HTCAPTION;
      Exit;
    end;
  end
  else begin
    if (GripMode = gmRightBottom) then begin
      GripVisible := True;
    end
    else if Assigned(ListSW) and Assigned(ListSW.sbarVert) and ListSW.sbarVert.fScrollVisible and ListSW.sbarHorz.fScrollVisible then begin
      Ac_GetHScrollRect(ListSW, Form.Handle, hrect);
      Ac_GetVScrollRect(ListSW, Form.Handle, vrect);
      GetWindowRect(Form.Handle, R);
      GripVisible := PtInRect(Rect(hrect.Right - R.Left, hrect.Top - R.Top, vrect.Right - R.Left, hrect.Bottom - R.Top), p)
    end
    else GripVisible := False;
    if GripVisible then begin
      i := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinManager.ConstData.IndexGLobalInfo, s_GlobalInfo, s_GripImage);
      if FCommonData.SkinManager.IsValidImgIndex(i) then begin
        if (p.y > RBGripPoint(i).y) and (p.x > RBGripPoint(i).x) then begin
          Result := HTBOTTOMRIGHT; Exit;
        end;
      end;
    end;
    // MDI child buttons
    if MDIButtonsNeeded then begin
      if PtInRect(MDICLose.Rect, p) then begin
        SetHotHT(HTCHILDCLOSE); Result := HTCHILDCLOSE; Exit;
      end else
      if PtInRect(MDIMax.Rect, p) then begin
        SetHotHT(HTCHILDMAX); Result := HTCHILDMAX; Exit;
      end else
      if PtInRect(MDIMin.Rect, p) then begin
        SetHotHT(HTCHILDMIN); Result := HTCHILDMIN; Exit;
      end;
    end else SetHotHT(0);
  end;
  if (Form.WindowState = wsMaximized) and AboveBorder(Message) then Result := HTTRANSPARENT else Result := Message.Result;
end;

procedure UpdateMainForm;
begin
  if Assigned(MDISkinProvider) then begin
    TsSkinProvider(MDISkinProvider).FCommonData.BeginUpdate;
    if Assigned(TsSkinProvider(MDISkinProvider).MDIForm) then TsMDIForm(TsSkinProvider(MDISkinProvider).MDIForm).UpdateMDIIconItem;
    TsSkinProvider(MDISkinProvider).FCommonData.BGChanged := True;
    TsSkinProvider(MDISkinProvider).MenuChanged := True;
    TsSkinProvider(MDISkinProvider).FLinesCount := -1;
    TsSkinProvider(MDISkinProvider).FCommonData.EndUpdate;
    RedrawWindow(TsSkinProvider(MDISkinProvider).Form.ClientHandle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INTERNALPAINT or RDW_INVALIDATE or RDW_UPDATENOW);
    RedrawWindow(TsSkinProvider(MDISkinProvider).Form.Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE);
  end
end;

procedure HandleEdge(var Edge: Integer; SnapToEdge: Integer; ASnapBuffer : integer; SnapDistance: Integer = 0);
begin
  if (Abs(Edge + SnapDistance - SnapToEdge) < ASnapBuffer) then Edge := SnapToEdge - SnapDistance;
end;

function DoLayered(Form : TForm; Layered : boolean) : boolean;
var
  DC : hdc;
  FBmpSize: TSize;
  FBmpTopLeft: TPoint;
  FBlend: TBlendFunction;
  Bmp : TBitmap;
begin
  Result := False;
  if Layered then begin
    if GetWindowLong(Form.Handle, GWL_EXSTYLE) and WS_EX_LAYERED <> WS_EX_LAYERED then begin
      DC := GetDC(0);
      FBlend.BlendOp := AC_SRC_OVER;
      FBlend.BlendFlags := 0;
      FBlend.AlphaFormat := AC_SRC_ALPHA;
      FBlend.SourceConstantAlpha := 10;
      FBmpSize.cx := 0;
      FBmpSize.cy := 0;
      FBmpTopLeft.X := 0;
      FBmpTopLeft.Y := 0;
      Bmp := TBitmap.Create;

      SetWindowLong(Form.Handle, GWL_EXSTYLE, GetWindowLong(Form.Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
      UpdateLayeredWindow(Form.Handle, DC, nil, @FBmpSize, Bmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA);

      Bmp.Free;
      ReleaseDC(0, DC);
      Result := True;
    end;
  end
  else begin
    SetWindowLong(Form.Handle, GWL_EXSTYLE, GetWindowLong(Form.Handle, GWL_EXSTYLE) and not WS_EX_LAYERED);
  end;
end;

procedure TsSkinProvider.NewWndProc(var Message: TMessage);
var
  DC, SavedDC : hdc;
  mi :  TMenuItem;
  X, Y, i : integer;
  p : TPoint;
  UpdateClient, lInted : boolean;
  cR : TRect;
  PS : TPaintStruct;
  AFlags: integer;
begin
{$IFDEF LOGGED}
  if Assigned(Form) and (Form.Name = 'Frm_Undo') {and (IntLevel > 0){(Form.FormStyle <> fsMdiChild) }then begin
    AddToLog(Message);
  end;
{$ENDIF}
{$IFDEF LOGGED}
//  if Form.Tag = 1 then begin
//    if Form.Parent <> nil then AddToLog(Message, 'Docked  - ') else AddToLog(Message, 'Undocked  - ');
//  end;
//  AddToLog(Message);
{$ENDIF}
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_REMOVESKIN : begin
      if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
        UseDwm(Form.Handle, False);
        DwmInitialized := False;
        if ListSW <> nil then FreeAndNil(ListSW);
        AdapterRemove;
        CommonMessage(Message, FCommonData);
        FLinesCount := -1;
        AlphaBroadCast(Form, Message);
        if Assigned(SkinData.SkinManager) then begin
          DeleteUnusedBmps(True);
        end;
        if (Form <> nil) and not (csDestroying in Form.ComponentState) then begin
          if Assigned(SkinData.SkinManager) then begin
            InitMenuItems(False);
            CheckSysMenu(False);
            if (Form.FormStyle = fsMDIForm) and (MDIForm <> nil) then HookMDI(False);
            if HaveBorder(Self) and not InMenu then SetWindowRgn(Form.Handle, 0, True); // Return standard kind
            UpdateMenu;
            RedrawWindow(Form.ClientHandle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ERASE or RDW_ALLCHILDREN);
          end;
          if UseGlobalColor and not SkinData.CustomColor then Form.Color := clBtnFace;
          if FCommonData.FCacheBmp <> nil then begin
            FreeAndNil(FCommonData.FCacheBmp);
          end;
        end;
      end
      else AlphaBroadCast(Form, Message);
      Exit
    end;
    AC_SETNEWSKIN : if not (csDestroying in Form.ComponentState) and Assigned(SkinData) then begin
      if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) and not Assigned(SystemMenu) then PrepareForm
    end;
    AC_GETAPPLICATION : begin Message.Result := longint(Application); Exit end;
    AC_PARENTCLOFFSET : begin Message.Result := MakeLong(OffsetX, OffsetY); Exit end;
    AC_CTRLHANDLED : begin Message.Result := 1; Exit end;
    AC_GETBG : if (FCommonData <> nil) then begin
      if FCommonData.BGChanged and not FCommonData.FUpdating and IsCached(SkinData) then begin
        PaintAll;
      end;
      if PacBGInfo(Message.LParam)^.PleaseDraw then begin
        inc(PacBGInfo(Message.LParam)^.Offset.X, OffsetX);
        inc(PacBGInfo(Message.LParam)^.Offset.Y, OffsetY);
      end;
      InitBGInfo(FCommonData, PacBGInfo(Message.LParam), integer(FormActive));
      if (PacBGInfo(Message.LParam)^.BgType = btCache) then begin
        if not PacBGInfo(Message.LParam)^.PleaseDraw then begin
          if PacBGInfo(Message.LParam)^.Bmp = nil then begin
            PaintAll;
            PacBGInfo(Message.LParam)^.Bmp := FCommonData.FCacheBmp;
          end;
          PacBGInfo(Message.LParam)^.Offset := Point(OffsetX, OffsetY);
        end;
      end;
      Exit;
    end;
    AC_GETSKINSTATE : begin
      Message.Result := FCommonData.CtrlSkinState;
      Exit;
    end;
  end;

  if (csDestroying in Form.ComponentState) or not FCommonData.Skinned(True) then begin
    if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
      AC_GETPROVIDER : begin Message.Result := longint(Self); Exit end; // Used for menuline init
      AC_SETNEWSKIN : if not (csDestroying in Form.ComponentState) and Assigned(SkinData) then begin
        if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
          DeleteUnusedBmps(True);
          FCommonData.UpdateIndexes;
//          if not IsCacheRequired(FCommonData) then FCommonData.CtrlSkinState := FCommonData.CtrlSkinState or ACS_FAST else FCommonData.CtrlSkinState := FCommonData.CtrlSkinState and not ACS_FAST;

          if (SkinData.SkinManager <> nil) then begin
            UpdateIconsIndexes;
            CheckSysMenu(True);
            if (Form.FormStyle = fsMDIForm) and (Screen.ActiveForm = Form.ActiveMDIChild) then FormActive := True;
            // Menus skinning
            if not (csLoading in SkinData.SkinManager.ComponentState) then
              InitMenuItems(True); // Update after skinning in run-time
            // Menu Line refresh
            FCommonData.BGChanged := True;
            FLinesCount := -1;
            if (TForm(Form).FormStyle = fsMDIForm) then begin
              if not Assigned(MDIForm) then HookMDI;
//              if MDIForm <> nil then TsMDIForm(MDIForm).ConnectToClient;
            end;
            if UseGlobalColor and not SkinData.CustomColor then Form.Color := SkinData.SkinManager.GetGlobalColor;
          end;
          if Adapter = nil then AdapterCreate else Adapter.AddAllItems;
        end;
//        AlphaBroadCast(Form, Message);
        Exit;
      end;
    end;
    OldWndProc(Message);
    if (Message.Msg = WM_NCDESTROY) and (Form.FormStyle = fsMDIChild) then begin
      if Assigned(MDISkinProvider) and Assigned(TsSkinProvider(MDISkinProvider).MDIForm) and not (csDestroying in TsSkinProvider(MDISkinProvider).ComponentState) and
          not (csDestroying in TsSkinProvider(MDISkinProvider).Form.ComponentState) and TsSkinProvider(MDISkinProvider).SkinData.Skinned(True) then begin
        TsSkinProvider(MDISkinProvider).RepaintMenu;
        if TsSkinProvider(MDISkinProvider).Form.ActiveMDIChild = nil
          then RedrawWindow(TsSkinProvider(MDISkinProvider).Form.ClientHandle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INTERNALPAINT or RDW_INVALIDATE or RDW_UPDATENOW);
      end;
    end;
  end
  else begin
    if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
      AC_CONTROLLOADED : if (Form <> nil) and not (csLoading in Form.ComponentState) then begin
        if Adapter <> nil then begin
          if Message.LParam <> 0 then Adapter.AddAllItems(TWinControl(Message.LParam)) else begin
            Adapter.AddAllItems(Form)
          end;
        end;
      end;
//      AC_SETGRAPHCONTROL : FCommonData.GraphControl := pointer(Message.LParam);
      AC_PREPARING : if (FCommonData <> nil) then begin
        Message.Result := integer(not InAnimationProcess{LionDev} and IsCached(FCommonData) and (FCommonData.FUpdating or FormChanged));
        Exit
      end;
      AC_UPDATING : begin
        FCommonData.Updating := Message.WParamLo = 1;
        if FCommonData.Updating then FCommonData.BGChanged := True;
      end;
      AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
        if Adapter = nil then AdapterCreate else Adapter.AddAllItems;
        if FDrawNonClientArea then RefreshFormScrolls(Self, ListSW, False);

        FLinesCount := -1;
        if Assigned(TitleBG) then FreeAndNil(TitleBG);
        if not (csLoading in SkinData.SkinManager.ComponentState) then begin
          UpdateMenu;
          InitMenuItems(SkinData.Skinned);
        end;
        if HaveBorder(Self) then RgnChanged := True;
        if not Form.Visible then Exit;

        FCommonData.BGChanged := True;
        if not InAnimationProcess then begin
          if (Form.FormStyle = fsMDIForm) and Assigned(Form.ActiveMDIChild) and (Form.ActiveMDIChild.WindowState = wsMaximized) then begin
            SendMessage(Form.Handle, WM_NCPAINT, 0, 0);
            InvalidateRect(Form.ActiveMDIChild.Handle, nil, True);
          end
          else begin
            RedrawWindow(Form.Handle, nil, 0, RDW_INVALIDATE or RDW_ERASE or RDW_UPDATENOW or RDW_FRAME or RDW_NOCHILDREN);
            if Form.FormStyle = fsMDIForm then begin
              RedrawWindow(Form.ClientHandle, nil, 0, RDW_INVALIDATE or RDW_ERASE or RDW_UPDATENOW or RDW_FRAME or RDW_NOCHILDREN);
            end;
          end;
        end;
        if UseGlobalColor and not SkinData.CustomColor then Form.Color := SkinData.SkinManager.GetGlobalColor;
        if MDIForm <> nil then TsMDIForm(MDIForm).ConnectToClient;
        FCommonData.Updating := False;                               // Form image is prepared now
        if SystemMenu <> nil then SystemMenu.UpdateGlyphs;
        SendToAdapter(Message);                                      // Sending message to all child adapted controls
      end;
      AC_CHILDCHANGED : begin
        Message.LParam := integer((FCommonData.SkinManager.gd[SkinData.SkinIndex].GradientPercent + FCommonData.SkinManager.gd[SkinData.SkinIndex].ImagePercent > 0));
        Message.Result := Message.LParam;
        Exit;
      end;
      AC_GETCONTROLCOLOR : begin
        Message.Result := FormColor;
        Exit;
      end;
      AC_GETPROVIDER : begin
        Message.Result := longint(Self);
        Exit
      end; // Used for menuline init
      AC_SETNEWSKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
        DeleteUnusedBmps(True);
        FCommonData.UpdateIndexes;
//        if not IsCacheRequired(FCommonData) then FCommonData.CtrlSkinState := FCommonData.CtrlSkinState or ACS_FAST else FCommonData.CtrlSkinState := FCommonData.CtrlSkinState and not ACS_FAST;
        if (SkinData.SkinManager <> nil) then begin
          UpdateIconsIndexes;

          DeleteUnusedBmps(True);
          CheckSysMenu(FDrawNonClientArea);
          SkinData.SkinManager.SkinableMenus.UpdateMenus;

          // Menu Line refresh
          FCommonData.BGChanged := True;
          FLinesCount := -1;
          SendMessage(Form.Handle, WM_NCPaint, 0, 0);

          if (TForm(Form).FormStyle = fsMDIForm) then begin
            if not Assigned(MDIForm) then HookMDI;
//            TsMDIForm(MDIForm).ConnectToClient
          end;
        end;
      end;
      AC_BEFORESCROLL : begin
        if FCommonData.RepaintIfMoved then SendMessage(Form.Handle, WM_SETREDRAW, 0, 0);
      end;
      AC_AFTERSCROLL : if FCommonData.RepaintIfMoved then begin
        SendMessage(Form.Handle, WM_SETREDRAW, 1, 0);
        RedrawWindow(Form.Handle, nil, 0, {RDW_NOERASE or }RDW_UPDATENOW or RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_FRAME or RDW_ERASE);
        if MDISkinProvider = Self then RedrawWindow(Form.ClientHandle, nil, 0, RDW_UPDATENOW or RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_FRAME);
      end;
      AC_URGENTPAINT : begin
        CommonWndProc(Message, FCommonData);
        if FCommonData.UrgentPainting then PaintAll;
        Exit;
      end;
    end
    else case Message.Msg of
      WM_NCCALCSIZE : begin
        AC_WMNCCalcSize(TWMNCCalcSize(Message));
      end;
      WM_INITMENUPOPUP : begin
        if Message.LParam <> 0 then acCanHookMenu := True; // Menu skinning is supported
        OldWndProc(Message);
      end;
      787 : begin
        DropSysMenu(Mouse.CursorPos.X, Mouse.CursorPos.Y) ;
      end;
      CM_SHOWINGCHANGED : begin
        if FCommonData.BGChanged and {not AeroIsEnabled and }Form.Showing and FDrawNonClientArea then begin // If first showing
          InitMenuItems(SkinData.Skinned);
          if Adapter <> nil then Adapter.AddAllItems(Form);
          fAnimating := (Form.FormStyle <> fsMDIChild) and (Form.Parent = nil) and
                        (not (Form.BorderStyle in [bsSizeable, bsSingle]) and SkinData.SkinManager.AnimEffects.DialogShow.Active or
                        (Form.BorderStyle in [bsSizeable, bsSingle]) and SkinData.SkinManager.AnimEffects.FormShow.Active);

          lInted := False;
          if fAnimating then begin
            FCommonData.Updating := True; // Don't draw child controls
            InAnimationProcess := True;
            if AeroIsEnabled then begin
              lInted := DoLayered(Form, True);
              UpdateRgn(Self, False);
              RgnChanging := False;
            end;
            for i := 0 to Form.ControlCount - 1 do if Form.Controls[i].Visible and not (Form.Controls[i] is TCoolBar) {TCoolBar must have Painting processed for buttons aligning} then Form.Controls[i].Perform(WM_SETREDRAW, 0, 0);
          end;
          OldWndProc(Message);
          RefreshFormScrolls(Self, ListSW, False);
          if fAnimating then begin
            if AeroIsEnabled then begin
              if lInted then DoLayered(Form, False);
            end;
            for i := 0 to Form.ControlCount - 1 do if Form.Controls[i].Visible then Form.Controls[i].Perform(WM_SETREDRAW, 1, 0);
          end;
          if fAnimating then begin
            fAnimating := False;
            if not (Form.BorderStyle in [bsSizeable, bsSingle])
              then i := SkinData.SkinManager.AnimEffects.DialogShow.Time
              else i := SkinData.SkinManager.AnimEffects.FormShow.Time;
            AnimShowControl(Form, i);
            UpdateRgn(Self, False);
            InAnimationProcess := False;
            if IsCached(FCommonData) or UseAero
              then RedrawWindow(Form.Handle, nil, 0, RDW_NOERASE or RDW_UPDATENOW or RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_FRAME)
              else RedrawWindow(Form.Handle, nil, 0, RDW_ERASE or RDW_UPDATENOW or RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_FRAME);
            RgnChanging := False;
          end;
          if (Form.FormStyle = fsMDIChild) and (Form.WindowState = wsMaximized) then TsSkinProvider(MDISkinProvider).RepaintMenu;
        end
        else begin // Backgrounds must be drawn by all controls for a blinking prevent
          OldWndProc(Message);
        end;
      end;
      WM_SHOWWINDOW : begin
        if (Form.FormStyle <> fsMDIChild) then begin

          if (Message.WParam = 1) and AeroIsEnabled then begin
            if not InAnimationProcess and (GetWindowLong(Form.Handle, GWL_EXSTYLE) and WS_EX_LAYERED <> WS_EX_LAYERED) then begin
              SetWindowLong(Form.Handle, GWL_STYLE, GetWindowLong(Form.Handle, GWL_STYLE) or WS_VISIBLE);
              RedrawWindow(Form.Handle, nil, 0, RDW_FRAME or RDW_ALLCHILDREN or RDW_INVALIDATE);
            end;
          end;

          if TWMShowWindow(Message).Status = SW_PARENTCLOSING then begin
            if IsIconic(Form.Handle) then begin
              ShowAction := saMinimize
            end
            else begin
              if IsZoomed(Form.Handle) then ShowAction := saMaximize else ShowAction := saRestore;
            end;
          end;
        end;
        OldWndProc(Message);
        if (Form.FormStyle = fsMDIChild) then begin
          if (Form.WindowState = wsMaximized) then TsSkinProvider(MDISkinProvider).RepaintMenu;
        end;
        if (Form.FormStyle = fsMDIForm) then begin
          for i := 0 to Form.MDIChildCount - 1 do
            if not GetBoolMsg(Form.MDIChildren[i].Handle, AC_CTRLHANDLED)
              then AddSupportedForm(Form.MDIChildren[i].Handle);
        end;
      end;
      WM_SETTEXT : if Form.Showing and not (csCreating in Form.ControlState) and FDrawNonClientArea {and not ((Form.FormStyle = fsMdiChild) and (Form.WindowState = wsMaximized))} then begin
        if (Form.FormStyle = fsMdiChild) and (Form.WindowState = wsMaximized) then begin
          TsSkinProvider(MDISkinProvider).FCommonData.BGChanged := True;
          MakeCaptForm(TsSkinProvider(MDISkinProvider));
          OldWndProc(Message);
          KillCaptForm(TsSkinProvider(MDISkinProvider));
        end
        else if not AeroIsEnabled then begin
          FCommonData.BGChanged := True;
          MakeCaptForm(Self);
          OldWndProc(Message);
          KillCaptForm(Self);
        end
        else begin
//          Form.Perform(WM_SETREDRAW, 0, 0);
          OldWndProc(Message);
//          Form.Perform(WM_SETREDRAW, 1, 0); // Don't work correctly under Aero
          UpdateSkinCaption(Self);
        end;
      end else OldWndProc(Message);
      CM_ENABLEDCHANGED : begin
        OldWndProc(Message);
        if FDrawNonClientArea then begin
          SkinData.BGChanged := True;
          SendMessage(Form.Handle, WM_NCPAINT, 0, 0);
        end;
      end;
      WM_SYSCOLORCHANGE : begin
        OldWndProc(Message);
        UpdateMenu;
      end;
      WM_CHILDACTIVATE : begin
        OldWndProc(Message);
        UpdateMenu;
        if (MDISkinProvider <> nil) and (TsSkinProvider(MDISkinProvider).Form.ActiveMDIChild <> nil)
          then if TsSkinProvider(MDISkinProvider).Form.ActiveMDIChild.WindowState = wsMaximized then begin
            TsMDIForm(TsSkinProvider(MDISkinProvider).MDIForm).UpdateMDIIconItem;
        end
      end;
      WM_SIZE : begin
        if IsGripVisible(Self) then begin
          i := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinManager.ConstData.IndexGLobalInfo, s_GlobalInfo, s_GripImage);
          if FCommonData.SkinManager.IsValidImgIndex(i) then begin // If grip image is defined in skin
            X := WidthOf(FCommonData.SkinManager.ma[i].R) div FCommonData.SkinManager.ma[i].ImageCount;
            Y := HeightOf(FCommonData.SkinManager.ma[i].R) div (FCommonData.SkinManager.ma[i].MaskType + 1);

            if ((Form.ClientWidth > WidthOf(LastClientRect)) or (Form.ClientHeight > HeightOf(LastClientRect))) then begin
              if not IsCached(SkinData) then begin // Refresh rect where Grip was drawn
                if (WidthOf(LastClientRect) <> 0) then begin

                  cR.Left := LastClientRect.Right - X;
                  cR.Top := LastClientRect.Bottom - Y;
                  cR.Bottom := LastClientRect.Bottom;
                  cR.Right := LastClientRect.Right;

                  InvalidateRect(Form.Handle, @cR, not IsCached(FCommonData));
                end;
              end;
            end;
            if ((Form.ClientWidth < WidthOf(LastClientRect)) or (Form.ClientHeight < HeightOf(LastClientRect))) then begin
              cR.Left := Form.ClientWidth - X;
              cR.Top := Form.ClientHeight - Y;
              cR.Bottom := Form.ClientHeight;
              cR.Right := Form.ClientWidth;

              InvalidateRect(Form.Handle, @cR, not IsCached(FCommonData));
            end;

          end;
        end;
        if ((Form.ClientWidth < WidthOf(LastClientRect)) or (Form.ClientHeight < HeightOf(LastClientRect)) or (WidthOf(LastClientRect) = 0)) then begin
          i := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinIndex, FCommonData.SkinSection, s_ImgTopRight);
          if i > -1 then begin
            X := WidthOf(FCommonData.SkinManager.ma[i].R) div FCommonData.SkinManager.ma[i].ImageCount;
            Y := HeightOf(FCommonData.SkinManager.ma[i].R) div (FCommonData.SkinManager.ma[i].MaskType + 1);
            cR.Left := LastClientRect.Right - X;
            cR.Top := LastClientRect.Bottom - Y;
            cR.Bottom := LastClientRect.Bottom;
            cR.Right := LastClientRect.Right;
            InvalidateRect(Form.Handle, @cR, not IsCached(FCommonData));
          end;
          i := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinIndex, FCommonData.SkinSection, s_ImgBottomRight);
          if i > -1 then begin
            X := WidthOf(FCommonData.SkinManager.ma[i].R) div FCommonData.SkinManager.ma[i].ImageCount;
            Y := HeightOf(FCommonData.SkinManager.ma[i].R) div (FCommonData.SkinManager.ma[i].MaskType + 1);
            cR.Left := LastClientRect.Right - X;
            cR.Top := LastClientRect.Bottom - Y;
            cR.Bottom := LastClientRect.Bottom;
            cR.Right := LastClientRect.Right;
            InvalidateRect(Form.Handle, @cR, not IsCached(FCommonData));
          end;

          if FCommonData.SkinManager.gd[FCommonData.SkinIndex].GradientPercent > 0 then InvalidateRect(Form.Handle, nil, True);
        end;
        LastClientRect := Rect(0, 0, Form.ClientWidth, Form.ClientHeight);

        OldWndProc(Message);
        if not InAnimationProcess then begin
          if Form.Showing then begin
            if FormChanged and (not (IsIconic(Form.Handle) and AeroIsEnabled)) then begin
              RgnChanged := True;
              FCommonData.BGChanged := True;
              FLinesCount := -1;
              if FCommonData.FCacheBmp <> nil
                then UpdateClient := IsCached(FCommonData) and ((FCommonData.FCacheBmp.Width > Form.Width) or (FCommonData.FCacheBmp.Height > Form.Height))
                else UpdateClient := True;
              if FDrawNonClientArea then SendMessage(Form.Handle, WM_NCPAINT, 0, 0);
              if UpdateClient and not AeroIsEnabled
                then InvalidateRect(Form.Handle, nil, False); // v6.10
            end
            else if (Form.FormStyle = fsMDIForm) and IsCached(SkinData) then begin
              SendMessage(Form.Handle, WM_NCPAINT, 0, 0);
            end
            else begin
              case Message.WParam of
                SIZE_MAXIMIZED : if (Form.FormStyle = fsMDIChild) and (Form.WindowState = wsMaximized) then begin // Repaint MDI child buttons
                  TsSkinProvider(MDISkinProvider).FCommonData.BGChanged := True;
                  TsSkinProvider(MDISkinProvider).MenuChanged := True;
                  RedrawWindow(Form.Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN);
                end;
              end;
            end;
          end
          else FCommonData.BGChanged := True;
        end;
      end;
      WM_ENTERMENULOOP : begin
        if (Form.FormStyle = fsMDIForm) and not InMenu and (Form.ActiveMDIChild <> nil) and (Form.ActiveMDIChild.WindowState = wsMaximized) then begin
          InMenu := True;
          if MDIIconsForm = nil then MDIIconsForm := TForm.Create(Application);
          MDIIconsForm.Visible := False;
          MDIIconsForm.Tag := ExceptTag;
          MDIIconsForm.Name := 'acMDIIcons';
          MDIIconsForm.OnPaint := MdiIcoFormPaint;

          MDIIconsForm.BorderStyle := bsNone;
          SetWindowPos(MDIIconsForm.Handle, 0, Form.BoundsRect.Right - 60, Form.Top + CaptionHeight, 60, GetSystemMetrics(SM_CYMENU) - 1 + 4, SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOREDRAW);
        end;
        OldWndProc(Message);
      end;
      WM_EXITMENULOOP : begin
        InMenu := False;
        if Assigned(MDIIconsForm) then FreeAndNil(MDIIconsForm);
        OldWndProc(Message);
      end;
      WM_NCHITTEST : begin
        if (CaptionHeight = 0) or not FDrawNonClientArea then OldWndProc(Message) else begin
          Message.Result := HTProcess(TWMNCHitTest(Message));
          case Message.Result of
            Windows.HTCAPTION : begin
              OldWndProc(Message);
              SetHotHT(0);
            end;
            Windows.HTNOWHERE : begin
              OldWndProc(Message);
              SetHotHT(0);
            end;
          end;
          if (ResizeMode = rmBorder) and { MarkB } (nDirection = 0) and (Message.Result in [HTCAPTION, HTBOTTOM, HTBOTTOMLEFT, HTBOTTOMRIGHT, HTLEFT,
               HTRIGHT, HTTOP, HTTOPLEFT, HTTOPRIGHT]) then nDirection := Message.Result;
        end;
      end;
      WM_SIZING : begin
        OldWndProc(Message);
{       if FormChanged then begin
          FCommonData.BGChanged := True;
          FLinesCount := -1;
        end;}
      end;
      WM_MOUSELEAVE, CM_MOUSELEAVE : begin
        SetHotHT(0);
        OldWndProc(Message);
      end;
      CM_MOUSEENTER : begin
        OldWndProc(Message);
        for i := 0 to Form.ControlCount - 1 do begin
          if (Form.Controls[i] is TsSpeedButton) and (Form.Controls[i] <> Pointer(Message.LParam)) and TsSpeedButton(Form.Controls[i]).SkinData.FMouseAbove then begin
            Form.Controls[i].Perform(CM_MOUSELEAVE, 0, 0)
          end;
        end;
      end;
      WM_NCLBUTTONDOWN : begin
        if FDrawNonClientArea then case TWMNCLButtonDown(Message).HitTest of
          HTCLOSE, HTMAXBUTTON, HTMINBUTTON, HTHELP, HTCHILDCLOSE..HTCHILDMIN : begin
            if Assigned(ChildProvider) then begin
              if ((TWMNCLButtonDown(Message).HitTest = HTCHILDMIN) and not ChildProvider.SystemMenu.EnabledMin) or
                   ((TWMNCLButtonDown(Message).HitTest = HTCHILDMAX) and not ChildProvider.SystemMenu.EnabledRestore)
                      then Exit;
            end;
            SetPressedHT(TWMNCLButtonDown(Message).HitTest);
          end;
          HTSYSMENU : begin
            SetHotHT(0);
            if Form.FormStyle = fsMDIChild then begin
              if Assigned(MDISkinProvider) then begin
                p := FormLeftTop;
                DropSysMenu(p.x, p.y + CaptionHeight + 4);
              end;
            end
            else DropSysMenu(FormLeftTop.x + SysBorderWidth, FormLeftTop.y + BorderHeight + HeightOf(IconRect));
          end
          else if BetWeen(TWMNCLButtonDown(Message).HitTest, HTUDBTN, HTUDBTN + TitleButtons.Count - 1) then begin
            SetPressedHT(TWMNCLButtonDown(Message).HitTest);
            TitleButtons.Items[TWMNCHitMessage(Message).HitTest - HTUDBTN].MouseDown(TWMNCHitMessage(Message).HitTest - HTUDBTN, mbLeft, [], TWMNCHitMessage(Message).XCursor, TWMNCHitMessage(Message).YCursor);
          end
          else begin
            if IsIconic(Form.Handle) then Form.Perform(WM_SYSCOMMAND, $F012, 0) else begin
              SetHotHT(0);
              if (Form.WindowState <> wsMaximized) or (CursorToPoint(0, TWMNCLButtonDown(Message).YCursor).y > SysBorderHeight + CaptionHeight) then begin
                if (ResizeMode = rmBorder) and not (TWMNCLButtonDown(Message).HitTest in [HTMENU]) then begin // MarkB
                  // If caption pressed then activate form (standard procedure)
                  if (TWMNCLButtonDown(Message).HitTest in [HTMENU, HTCAPTION]) then OldWndProc(Message);
                  bMode := not (TWMNCLButtonDown(Message).HitTest in [HTRIGHT, HTLEFT, HTBOTTOM, HTTOP, HTTOPLEFT, HTTOPRIGHT, HTBOTTOMLEFT, HTBOTTOMRIGHT]);
                  p := Point(TWMNCLButtonDown(Message).XCursor, TWMNCLButtonDown(Message).YCursor);
                  if bMode then SendMessage(Form.Handle, WM_SYSCOMMAND, SC_MOVE, 0) else SendMessage(Form.Handle, WM_SYSCOMMAND, SC_SIZE, 0);
                  StartMove(p.X, p.Y);
                end
                else OldWndProc(Message);
              end
              else if not Form.Active then Form.SetFocus; // Focusing if maximized
            end;
          end
        end
        else begin
          // Skinned sysmenu
          case TWMNCLButtonDown(Message).HitTest of
            HTSYSMENU : begin
              SetHotHT(0);
              if Form.FormStyle = fsMDIChild then begin
                if Assigned(MDISkinProvider) then begin
                  p := FormLeftTop;
                  DropSysMenu(p.x, p.y + CaptionHeight + 4);
                end;
              end
              else DropSysMenu(FormLeftTop.x + SysBorderWidth, FormLeftTop.y + BorderHeight + HeightOf(IconRect));
            end
            else OldWndProc(Message);
          end                 
//          OldWndProc(Message);
        end;
      end;// else OldWndProc(Message);
      WM_SETTINGCHANGE : begin
        FCommonData.BGChanged := True;
        if TitleBG <> nil then FreeAndNil(TitleBG);
        OldWndProc(Message);
      end;
      WM_MOUSEMOVE : begin // MarkB
        OldWndProc(Message);
        if (DefaultManager <> nil) and not (csDesigning in DefaultManager.ComponentState) then DefaultManager.ActiveControl := 0;
        if ResizeMode = rmBorder then begin
          p := Form.ClientToScreen(Point(TWMMouseMove(Message).XPos, TWMMouseMove(Message).YPos));
          X := p.X;
          Y := p.Y;
          if bMode then begin //Move section
            if bCapture then bCapture := False;
            if bInProcess then begin
              DrawFormBorder(nleft, ntop);
              nleft := nleft + (X - nX);
              ntop := ntop + (Y - nY);
              nY := Y;
              nX := X;
              DrawFormBorder(nleft, ntop);
            end;
          end
          else begin
            //Size section
            if bCapture then begin
              nX := X;
              nY := Y;
              bCapture := False;
            end;
            if bInProcess then begin
              DrawFormBorder(0, 0);
              case nDirection of
                HTLEFT : begin
                  nleft := Form.left + X - nX;
                  if nright - nleft < nMinWidth then nleft := nright - nMinWidth;
                end;
                HTRIGHT : begin
                  nright := Form.left + Form.width + X - nX;
                  if nright - nleft < nMinWidth then nright := Form.left + nMinWidth;
                end;
                HTBOTTOM : begin
                  nbottom := Form.top + Form.height + Y - nY;
                  if nbottom - ntop < nMinHeight then nbottom := Form.top + nMinHeight;
                end;
                HTTOP : begin
                  ntop := Form.top + Y - nY;
                  if nbottom - ntop < nMinHeight then ntop := nbottom - nMinHeight;
                end;
                HTBOTTOMLEFT : begin
                  nbottom := Form.top + Form.height + Y - nY;
                  if nbottom - ntop < nMinHeight then nbottom := Form.top + nMinHeight;
                  nleft := Form.left + X - nX;
                  if nright - nleft < nMinWidth then nleft := nright - nMinWidth;
                end;
                HTTOPRIGHT : begin
                  ntop := Form.top + Y - nY;
                  if nbottom - ntop < nMinHeight then ntop := nbottom - nMinHeight;
                  nright := Form.left + Form.width + X - nX;
                  if nright - nleft < nMinWidth then nright := Form.left + nMinWidth;
                end;
                HTTOPLEFT : begin
                  ntop := Form.top + Y - nY;
                  if nbottom - ntop < nMinHeight then ntop := nbottom - nMinHeight;
                  nleft := Form.left + X - nX;
                  if nright - nleft < nMinWidth then nleft := nright - nMinWidth;
                end
                else begin
                  nbottom := Form.top + Form.height + Y - nY;
                  if nbottom - ntop < nMinHeight then nbottom := Form.top + nMinHeight;
                  nright := Form.left + Form.width + X - nX;
                  if nright - nleft < nMinWidth then nright := Form.left + nMinWidth;
                end;
              end;
              DrawFormBorder(0, 0);
            end;
          end;
        end;
      end;
      WM_NCRBUTTONDOWN : if not (TWMNCLButtonUp(Message).HitTest in [HTCAPTION, HTSYSMENU]) then OldWndProc(Message);
      WM_NCRBUTTONUP : begin
        if {FDrawNonClientArea Skinned sysmenu and} HaveBorder(Self){SkinData.SkinManager.SkinnedPopups} then begin
          case TWMNCLButtonUp(Message).HitTest of
            HTCAPTION, HTSYSMENU : begin
              SetHotHT(0);
              DropSysMenu(TWMNCLButtonUp(Message).XCursor, TWMNCLButtonUp(Message).YCursor);
            end
          end;
          Exit;
        end
        else OldWndProc(Message);
      end;
      WM_NCLBUTTONUP, WM_LBUTTONUP: if FDrawNonClientArea then begin
        case TWMNCHitMessage(Message).HitTest of
          HTCLOSE : if biClicked then begin
            ButtonClose.State := 0;
            SendMessage(Form.Handle, WM_SYSCOMMAND, SC_CLOSE, 0);
          end;
          HTMAXBUTTON : if (SystemMenu.EnabledMax or (Form.WindowState = wsMaximized) and SystemMenu.EnabledRestore) then begin
            if biClicked then begin
              if Form.FormStyle = fsMDIChild then TsSkinProvider(MDISkinProvider).FCommonData.BeginUpdate;
              SetHotHT(0);
              if Form.WindowState = wsMaximized then begin
                if Form.FormStyle <> fsMDIChild then Form.WindowState := wsNormal else ChildProvider := nil;
                SendMessage(Form.Handle, WM_SYSCOMMAND, SC_RESTORE, 0);
                Form.Repaint
              end
              else begin
                if Form.FormStyle = fsMDIChild then ChildProvider := nil;
                SendMessage(Form.Handle, WM_SYSCOMMAND, SC_MAXIMIZE, 0);
              end;
              SystemMenu.UpdateItems;
            end
            else begin
              SetHotHT(0);
              OldWndProc(Message);
            end;
          end;
          HTMINBUTTON : if biClicked then begin
            p := CursorToPoint(TWMMouse(Message).XPos, TWMMouse(Message).YPos);
            if PtInRect(ButtonMin.Rect, p) then begin
              SetHotHT(0);
              if (Application.MainForm = Form) then begin
//                Application.Minimize; !!!
                SendMessage(Form.Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0); 
              end
              else if IsIconic(Form.Handle) then begin
                SendMessage(Form.Handle, WM_SYSCOMMAND, SC_RESTORE, 0);
              end
              else SendMessage(Form.Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
              FCommonData.BGChanged := True;
              RegionChanged := True;
            end
            else OldWndProc(Message);
          end
          else begin
            SetHotHT(0);
            OldWndProc(Message);
          end;
          HTHELP : if biClicked then begin
            SendMessage(Form.Handle, WM_SYSCOMMAND, SC_CONTEXTHELP, 0);
            SetHotHT(0);
            SystemMenu.UpdateItems;
            Form.Perform(WM_NCPAINT, 0, 0);
          end else SetHotHT(0);
          // MDI child buttons
          HTCHILDCLOSE : if biClicked then begin
            SendMessage(Form.ActiveMDIChild.Handle, WM_SYSCOMMAND, SC_CLOSE, 0);
            SetHotHT(0, False);
          end;
          HTCHILDMAX : begin
            if biClicked then SendMessage(Form.ActiveMDIChild.Handle, WM_SYSCOMMAND, SC_RESTORE, 0);
            SetHotHT(0, False);
          end;
          HTCHILDMIN : begin
            if biClicked then begin
              TsSkinProvider(MDISkinProvider).FCommonData.BeginUpdate;
              SendMessage(Form.ActiveMDIChild.Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
            end;
            SetHotHT(0);
          end
          else if BetWeen(TWMNCHitMessage(Message).HitTest, HTUDBTN, HTUDBTN + TitleButtons.Count - 1) then begin
            if biClicked then TitleButtons.Items[TWMNCHitMessage(Message).HitTest - HTUDBTN].MouseUp(TWMNCHitMessage(Message).HitTest - HTUDBTN, mbLeft, [], TWMNCHitMessage(Message).XCursor, TWMNCHitMessage(Message).YCursor);
            SetHotHT(0);
          end
          else begin
            if IsIconic(Form.Handle) then begin
              if Assigned(MDISkinProvider) then DropSysMenu(acMousePos.x + FormLeftTop.X, acMousePos.y) else OldWndProc(Message);
            end
            else begin
              if (ResizeMode = rmBorder) and (Form.WindowState <> wsMaximized) and bInProcess then begin // MarkB
                //Common section
                p := Form.ClientToScreen(Point(TWMNCLButtonDown(Message).XCursor, TWMNCLButtonDown(Message).YCursor));
                StopMove(p.x, p.y);
                ReleaseCapture;
              end else OldWndProc(Message);
            end;
          end
        end;
      end
      else OldWndProc(Message);
      WM_ENTERIDLE : begin
        Message.Result := 0;
        Exit;
      end;
      CM_FLOAT : begin
        OldWndProc(Message);
        if ListSW <> nil then FreeAndNil(ListSW);
        SendAMessage(Form.Handle, AC_REFRESH, Cardinal(SkinData.SkinManager));
      end;
      CM_MENUCHANGED : begin
        if not (fsCreating in Form.FormState) and Form.Visible then MenuChanged := True; // Menu may be invisible after form opening ????
        OldWndProc(Message);
      end;
      WM_PRINT : begin
        if not Form.Visible or (csLoading in ComponentState) or (fsCreating in Form.FormState) or (csDestroying in Form.ComponentState) then Exit;
        if {FCommonData.Updating or} ((Form.FormStyle = fsMDIChild) and (MDISkinProvider <> nil) and
                Assigned(TsSkinProvider(MDISkinProvider).Form.ActiveMDIChild) and
                  (TsSkinProvider(MDISkinProvider).Form.ActiveMDIChild.WindowState = wsMaximized)
                    and (Form <> TsSkinProvider(MDISkinProvider).Form.ActiveMDIChild)) then Exit;
        if not MenusInitialized then begin
          if UpdateMenu then MenusInitialized := True;
        end;
        DC := TWMPaint(Message).DC;
        FCommonData.Updating := False;
        if FDrawNonClientArea then begin
          if not HaveBorder(Self) and IsSizeBox(Form.Handle) then begin
            if FCommonData.BGChanged then begin
              PaintAll;
            end;
            i := Form.BorderWidth + 3;
            BitBlt(DC, 0, 0, Form.Width, i, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY); // Title and menu line update
            BitBlt(DC, 0, i, i, FCommonData.FCacheBmp.Height, FCommonData.FCacheBmp.Canvas.Handle, 0, i, SRCCOPY); // Left border update
            BitBlt(DC, i, Form.Height - i, Form.Width - i, i, FCommonData.FCacheBmp.Canvas.Handle, i, Form.Height - i, SRCCOPY); // Bottom border update
            BitBlt(DC, FCommonData.FCacheBmp.Width - i, i, i, FCommonData.FCacheBmp.Height, FCommonData.FCacheBmp.Canvas.Handle, FCommonData.FCacheBmp.Width - i, i, SRCCOPY); // Right border update
          end
          else PaintCaption(DC);
        end;
        MoveWindowOrg(DC, OffsetX, OffsetY);
        GetClientRect(Form.Handle, cR);
        IntersectClipRect(DC, 0, 0, WidthOf(cR), HeightOf(cR));

        if SkinData.CtrlSkinState and ACS_FAST <> ACS_FAST then PaintForm(DC) else begin
          SavedDC := SaveDC(DC);
          ExcludeControls(DC, Form, actGraphic, 0, 0);
          PaintForm(DC);
          RestoreDC(DC, SavedDC);
          if (Form.FormStyle <> fsMDIForm) then PaintControls(DC, Form, True, Point(0, 0));
        end;

        if Form.FormStyle = fsMDIForm then SendMessage(Form.ClientHandle, WM_PRINT, longint(DC), 0);

        if Assigned(Form.OnPaint) then begin
          Form.Canvas.Handle := DC;
          Form.OnPaint(Form);
          Form.Canvas.Handle := 0;
        end;
      end;
      WM_PRINTCLIENT : ;
      CM_INVALIDATE, 147, 45132, 48205 : Message.Result := 0;
      WM_STYLECHANGED : begin
        OldWndProc(Message);
      end;
      WM_NCPAINT : if DrawNonClientArea and not (InAnimationProcess and (Form.FormStyle = fsMDIChild){liondev}) then begin
        if IsIconic(Form.Handle) then FCommonData.BGChanged := True;
        if fAnimating or not Form.Showing or (csLoading in ComponentState) or (SystemMenu = nil{not initialized}) then Exit;

{        if AeroIsEnabled and (Tag = -97) then begin // If WM_PAINT was not performed (when TsSkinProvider wasn't placed before form showing)
          Tag := 0;
          if Assigned(Adapter) then Adapter.AddAllItems else AdapterCreate;
          InvalidateRect(Form.Handle, nil, True);
          RedrawWindow(Form.Handle, nil, 0, RDW_ALLCHILDREN or RDW_ERASE or RDW_INVALIDATE or RDW_UPDATENOW);
        end;}

        AC_WMNCPaint;
        Message.Result := 0;
      end
      else OldWndProc(Message);
      WM_ERASEBKGND : if Form.Showing then begin
        if not (csPaintCopy in Form.ControlState) then begin
          if not IsCached(FCommonData) or ((Form.FormStyle = fsMDIForm) and (Form.ActiveMDIChild <> nil) and (Form.ActiveMDIChild.WindowState = wsMaximized)) then begin
            if (Form.FormStyle = fsMDIChild) and (FCommonData.FCacheBmp <> nil) then begin
              GetClientRect(TsSkinProvider(MDISkinProvider).Form.Handle, cR);
              if (PtInRect(cR, Form.BoundsRect.TopLeft) and PtInRect(cR, Form.BoundsRect.BottomRight)) then begin
                FCommonData.FCacheBmp.Height := min(FCommonData.FCacheBmp.Height, CaptionHeight + SysBorderHeight + LinesCount * MenuHeight + 1);
                FCommonData.BGChanged := True;
              end
            end;
            if not AeroIsEnabled then AC_WMEraseBkGnd(TWMPaint(Message).DC) else begin
              if (GetClipBox(TWMPaint(Message).DC, cR) = NULLREGION) or (WidthOf(cR) = 0) or (HeightOf(cR) = 0) then begin
                DC := GetDC(Form.Handle);
              end
              else DC := TWMPaint(Message).DC;
              AC_WMEraseBkGnd(DC);
            end;
            if MDICreating then begin
              MDICreating := False;
              if Form.FormStyle = fsMDIChild then begin
                ChildProvider := Self;
              end;
            end;
{            if not FCommonData.FUpdating then begin
              // If have child control with Align = clClient // If client area is not visible // v6.12
              if (GetClipBox(TWMPaint(Message).DC, cR) = NULLREGION) or (WidthOf(cR) = 0) or (HeightOf(cR) = 0) then begin
                SetParentUpdated(Form);
                Exit;
              end;
            end}
          end
          else begin
            if not FCommonData.FUpdating then begin
              // If have child control with Align = clClient // If client area is not visible // v6.12
              if (GetClipBox(TWMPaint(Message).DC, cR) = NULLREGION) or (WidthOf(cR) = 0) or (HeightOf(cR) = 0) then begin
                SetParentUpdated(Form);
                Exit;
              end;
            end
          end;
        end
        else if (Message.LParam <> 0) and (Message.LParam = Message.WParam) then begin // From PaintTo proc // DevEx
          if not FCommonData.BGChanged then begin
            if IsCached(FCommonData)
              then CopyWinControlCache(Form, FCommonData, Rect(SysBorderWidth + Form.BorderWidth, OffsetY, 0, 0), Rect(0, 0, Form.Width, Form.Height), TWMPaint(Message).DC, False)
              else FillDC(TWMPaint(Message).DC, Rect(0, 0, Form.Width, Form.Height), GetControlColor(Form.Handle));
          end;
        end;
        Message.Result := 1;
        {$IFDEF CHECKXP}
//        if UseThemes and ThemeServices.ThemesEnabled {and StdBgIsUsed} and not InAnimationProcess then OldWndProc(Message) // Draw BG for controls with ParentBackground if exists
        {$ENDIF}
      end
      else OldWndProc(Message);
      WM_PAINT : begin
        if not DwmInitialized then begin
          DwmInitialized := True;
          UseDwm(Form.Handle, SkinData.Skinned);
        end;
        if IsCached(FCommonData) {!!!and not AeroIsEnabled} then begin
          if Form.Showing then begin
            OurPaintHandler(TWMPaint(Message));
            if MDICreating then begin
              MDICreating := False;
              if Form.FormStyle = fsMDIChild then begin
                SendMessage(Form.Handle, WM_SYSCOMMAND, SC_MAXIMIZE, 0);
//                Form.WindowState := wsMaximized;
              end;
            end;
            if (Form.FormStyle = fsMDIForm) then begin
              if TWMPaint(Message).DC = 0 then TWMPaint(Message).DC := GetDC(Form.Handle);
              PaintControls(TWMPaint(Message).DC, Form, True, Point(0, 0));
            end;
            Message.Result := 0;
          end
          else{$IFDEF DELPHI7UP} if Form.AlphaBlend then {$ENDIF}OldWndProc(Message);
        end
        else begin
          if Form.Showing then begin
{            if not (csDestroying in ComponentState) and AeroInvalidate then begin // Background update in desight-time
              AeroInvalidate := False;
              InvalidateRect(Form.Handle, nil, True); // Background update (for repaint of graphic controls)
            end;}
            BeginPaint(Form.Handle, PS);
            EndPaint(Form.Handle, PS);
            Message.Result := 0;
          end
          else OldWndProc(Message);
        end;
        if not InAnimationProcess and Assigned(Form.OnPaint) then Form.OnPaint(Form);
      end;
{$IFNDEF ALITE}
{$IFDEF D2005}
      CM_RECREATEWND : if acTaskBarChanging then Exit;
{$ENDIF}
{$ENDIF}
      WM_NCACTIVATE : if not (csDestroying in ComponentState) then begin
        FormActive := TWMNCActivate(Message).Active;
        if not FormActive and (ShowAction = saMaximize) then begin // <<<<< Caption blinking removing
          ShowAction := saIgnore;
        end
        else if not (ShowAction = saMaximize) then begin
          FCommonData.BGChanged := True;
          FCommonData.Updating := False;
        end
        else FCommonData.Updating := True;                         // >>>>> Caption blinking removing

        if Form.Showing then begin
          if FormActive <> (TWMActivate(Message).Active <> WA_INACTIVE) then FCommonData.BGChanged := True;
          if AeroIsEnabled then begin
            AFlags := GetWindowLong(Form.Handle, GWL_STYLE);
            SetWindowLong(Form.Handle, GWL_STYLE, AFlags and not WS_VISIBLE);
            if fAnimating
              then SendMessage(Form.Handle, WM_SETREDRAW, 0, 0)
          end
          else begin
            if fAnimating
              then SendMessage(Form.Handle, WM_SETREDRAW, 0, 0)
              else MakeCaptForm(Self);
          end;

          OldWndProc(Message);

          if AeroIsEnabled then begin
            if fAnimating
              then SendMessage(Form.Handle, WM_SETREDRAW, 1, 0);
            SetWindowLong(Form.Handle, GWL_STYLE, AFlags);
          end
          else begin
            if fAnimating
              then SendMessage(Form.Handle, WM_SETREDRAW, 1, 0)
              else KillCaptForm(Self);
          end;
        end
        else OldWndProc(Message);
        if Assigned(Form) and (Form.Visible) and (Form.FormStyle = fsMDIChild) and (fsCreatedMDIChild in Form.FormState) and
                   (fsshowing in Form.FormState) and Assigned(MDISkinProvider) and Assigned(TsSkinProvider(MDISkinProvider).MDIForm) then begin

          if SystemMenu = nil then PrepareForm; // If not intitialized (occurs when Scaled = False)
          TsSkinProvider(MDISkinProvider).FCommonData.BGChanged := True;
          TsSkinProvider(MDISkinProvider).FLinesCount := -1;
          TsMDIForm(TsSkinProvider(MDISkinProvider).MDIForm).UpdateMDIIconItem;

          SendMessage(TsSkinProvider(MDISkinProvider).Form.Handle, WM_NCPAINT, 0, 0);
          SendMessage(TsSkinProvider(MDISkinProvider).Form.ClientHandle, WM_NCPAINT, 0, 0);
        end;
        if GetWindowLong(Form.Handle, GWL_EXSTYLE) and $00080000 = $00080000
          then SendMessage(Form.Handle, WM_NCPAINT, 0, 0); // WS_EX_LAYERED

        if (csCreating in Form.ControlState) then Exit;

        if HaveBorder(Self) then begin
          if MDIButtonsNeeded then begin
            if (TWMActivate(Message).Active <> WA_INACTIVE) or (Form.ActiveMDIChild.Active) then begin
              FormActive := (TWMActivate(Message).Active <> WA_INACTIVE) or (Form.ActiveMDIChild.Active);
              FLinesCount := -1;
            end;
          end
          else if FormActive <> (TWMActivate(Message).Active <> WA_INACTIVE) then begin
            FormActive := (TWMActivate(Message).Active <> WA_INACTIVE);
            FLinesCount := -1;
          end;
        end;

//        if Form.FormStyle = fsMDIChild then begin// System default proc calling enables blinking
        SendMessage(Form.Handle, WM_NCPAINT, 0, 0);
//        end;

        if not (csDesigning in SkinData.SkinManager.ComponentState) then InitMenuItems(SkinData.Skinned); // Update after skinning in run-time
        if not MenusInitialized then begin
          if UpdateMenu then MenusInitialized := True;
        end;
        if FCommonData.Skinned and (Adapter = nil) then AdapterCreate;
{        if IsIconic(Form.Handle) and not AeroIsEnabled then begin // Aero
//        if IsIconic(Form.Handle) then begin
          FCommonData.BGChanged := True;
          SendMessage(Form.Handle, WM_NCPAINT, 0, 0);
        end;}
//        if IsCached(FCommonData) and not FCommonData.BGChanged then SetParentUpdated(Form); // Updating of frames which are not refreshed
      end
      else OldWndProc(Message);
      WM_MDICREATE : begin
        OldWndProc(Message);
{
        if (Form.FormStyle = fsMDIForm) then begin
          for i := 0 to Form.MDIChildCount - 1 do
            if not GetBoolMsg(Form.MDIChildren[i].Handle, AC_CTRLHANDLED)
              then AddSupportedForm(Form.MDIChildren[i].Handle);
        end;
}
      end;
      WM_MDIACTIVATE, WM_MDIDESTROY : begin
        OldWndProc(Message);
        if (Form.FormStyle = fsMDIChild) and (Form.WindowState <> wsMaximized) and Assigned(MDISkinProvider)
              and not (csDestroying in TsSkinProvider(MDISkinProvider).ComponentState) then begin
          MenusInitialized := False;
          TsSkinProvider(MDISkinProvider).Menuchanged := True;
          TsSkinProvider(MDISkinProvider).SkinData.BGChanged := True;
          TsSkinProvider(MDISkinProvider).FLinesCount := -1;
          SendMessage(TsSkinProvider(MDISkinProvider).Form.Handle, WM_NCPAINT, 0, 0);
        end;
        ChildProvider := Self;
      end;
      WM_ACTIVATE : if Form.Showing then begin
        {if AeroIsEnabled then begin
          AFlags := GetWindowLong(Form.Handle, GWL_STYLE);
          SetWindowLong(Form.Handle, GWL_STYLE, AFlags and not WS_VISIBLE);
        end;v6.21}
        OldWndProc(Message);
        if (ListSW = nil) and FDrawNonClientArea then RefreshFormScrolls(Self, ListSW, False);
//        if AeroIsEnabled then SetWindowLong(Form.Handle, GWL_STYLE, AFlags or WS_VISIBLE);
        if not (csCreating in Form.ControlState) then begin
          FLinesCount := -1;
          if Form.FormStyle = fsMDIChild then begin// !! System calling enables blinking
            for i := 0 to TsSkinProvider(MDISkinProvider).Form.MDIChildCount - 1 do begin
              if (TsSkinProvider(MDISkinProvider).Form.MDIChildren[i] <> Form) and (TsSkinProvider(MDISkinProvider).Form.MDIChildren[i].Visible)
                then SendMessage(TsSkinProvider(MDISkinProvider).Form.MDIChildren[i].Handle, WM_NCPAINT, 0, 0);
            end;
            TsSkinProvider(MDISkinProvider).FCommonData.BGChanged := True;
          end;
          if not (csDesigning in SkinData.SkinManager.ComponentState) then InitMenuItems(SkinData.Skinned); // Update after skinning in run-time
          if Assigned(SystemMenu) then SystemMenu.UpdateItems;
          if (Form.BorderStyle = bsNone) then SetParentUpdated(Form);
        end;
      end else OldWndProc(Message);
      CM_VISIBLECHANGED : begin
        OldWndProc(Message);
        if (Message.WParam = 0) then begin
          if Assigned(SkinData) and Assigned(SkinData.FCacheBmp) and not (csDestroying in Form.ComponentState) then begin
            SkinData.FCacheBmp.Width := 0;
            SkinData.FCacheBmp.Height := 0;
          end;
        end
        else begin
          if Form.Parent <> nil then SetParentUpdated(Form); // Updating of controls which are not refreshed
        end;
      end;
      WM_NCLBUTTONDBLCLK : begin
        if (ResizeMode = rmBorder) and bInProcess then begin        // MarkB
          p := Form.ClientToScreen(Point(TWMMouse(Message).XPos, TWMMouse(Message).YPos));
          StopMove(p.x, p.y);
          ReleaseCapture;
          bInProcess := False;
        end;
        DoStartMove := False;
        case TWMNCHitMessage(Message).HitTest of
          HTSYSMENU : begin Form.Close; end;
          HTCAPTION : begin
            if SystemMenu.VisibleClose and (SystemMenu.EnabledMax or SystemMenu.EnabledRestore) or not HaveBorder(Self) and IsIconic(Form.Handle) then begin
              if (Form.WindowState = wsMaximized) or IsIconic(Form.Handle) then begin
                SendMessage(Form.Handle, WM_SYSCOMMAND, SC_RESTORE, 0);
              end
              else begin
                SendMessage(Form.Handle, WM_SYSCOMMAND, SC_MAXIMIZE, 0);
              end;
              SystemMenu.UpdateItems;
            end;
            TWMNCHitMessage(Message).HitTest := 0;
          end;
        end;
      end;
      WM_MEASUREITEM : begin
        if (Form.FormStyle = fsMDIForm) {and not (csDesigning in ComponentState)} and (MDISkinProvider = Self) then begin
          MDISkinProvider := Self;
        end;
        if not (Form.FormStyle = fsMDIChild) and Assigned(Form.Menu) and Assigned(SkinData.SkinManager) and (PMeasureItemStruct(Message.LParam)^.CtlType = ODT_MENU) then begin
          mi := Form.Menu.FindItem(PMeasureItemStruct(Message.LParam)^.itemID, fkCommand);
          if mi <> nil then SkinData.SkinManager.SkinableMenus.InitItem(mi, True);
        end;
        OldWndProc(Message);
      end;
      WM_SYSCOMMAND : begin
        if Message.WParamLo = $F012 then Form.Repaint;               // Faster switching between a forms (avoid of system delay)
        if (ResizeMode = rmBorder) and not bInProcess then begin     // MarkB
          if ($FFF0 and Message.WParam = SC_MOVE) then begin         // Move section
            SetCapture(Form.handle);
            bCapture := True; bMode := True;
          end
          else if ($FFF0 and Message.WParam = SC_SIZE) then begin    // Size section
            SetCapture(Form.handle);
            nDirection := 0;
            bCapture := True; bMode := False;
          end;
        end;

        case Form.FormStyle of
          fsMDIChild : begin
            UpdateClient := (Form.WindowState = wsMaximized) and (Message.WParam <> SC_MAXIMIZE) and (Form.WindowState <> wsNormal);
            if UpdateClient then TsSkinProvider(MDISkinProvider).FCommonData.BeginUpdate; // Speed rising
            case $FFF0 and Message.WParam of
              SC_CLOSE : begin

                if (MDISkinProvider <> nil) and Assigned(TsSkinProvider(MDISkinProvider).MDIForm) then begin
                  if UpdateClient then UpdateMainForm;
                end; // If CloseQuery used then must be repainted before

                TsSkinProvider(MDISkinProvider).InMenu := True;
                OldWndProc(Message);
                SetHotHT(0, False);
                TsSkinProvider(MDISkinProvider).InMenu := False;

                if (MDISkinProvider <> nil) and Assigned(TsSkinProvider(MDISkinProvider).MDIForm) then if UpdateClient then UpdateMainForm;
                Exit;
              end;
              SC_KEYMENU : ;
              SC_RESTORE : begin
                if Form.WindowState = wsMinimized then UpdateClient := True;
                if MDICreating then Exit;
                ForbidDrawing(Self, True);
                OldWndProc(Message);
                SkinData.BGChanged := True;

                SetWindowLong(Form.Handle, GWL_STYLE, GetWindowLong(Form.Handle, GWL_STYLE) and not WS_SYSMENU); // v5.03
                PermitDrawing(Self, True);
                RedrawWindow(Form.Handle, nil, 0, RDW_FRAME or RDW_ERASE or RDW_INVALIDATE or RDW_UPDATENOW);

                // Updating of childs
                for i := 0 to TsSkinProvider(MDISkinProvider).Form.MDIChildCount - 1 do if TsSkinProvider(MDISkinProvider).Form.MDIChildren[i] <> Form then begin
                  if TsSkinProvider(MDISkinProvider).Form.MDIChildren[i].WindowState = wsMaximized then TsSkinProvider(MDISkinProvider).Form.MDIChildren[i].WindowState := wsNormal;
                  RedrawWindow(TsSkinProvider(MDISkinProvider).Form.MDIChildren[i].Handle, nil, 0, RDW_FRAME or RDW_INTERNALPAINT or RDW_ERASE or RDW_UPDATENOW);
                end;
                if UpdateClient then UpdateMainForm;
                SystemMenu.UpdateItems(True);
                Exit;
              end;
              SC_MINIMIZE : begin
                OldWndProc(Message);
                if UpdateClient then UpdateMainForm;
                SystemMenu.UpdateItems(True);
                FreeAndNil(TitleBG);
                FCommonData.BGChanged := True;
                UpdateMainForm;
                Exit;
              end;
              SC_MAXIMIZE : begin
                ChildProvider := Self;
                ForbidDrawing(Self, True);
                OldWndProc(Message);
                PermitDrawing(Self, True);
                RedrawWindow(Form.Handle, nil, 0, RDW_FRAME or RDW_INTERNALPAINT or RDW_ERASE or RDW_UPDATENOW);

                if Assigned(TsSkinProvider(MDISkinProvider).MDIForm) then UpdateMainForm;
                SystemMenu.UpdateItems(True);
                Exit;
              end
              else OldWndProc(Message);
            end;
            if MDISkinProvider <> nil then TsSkinProvider(MDISkinProvider).FCommonData.EndUpdate;
          end;
          fsMDIForm : begin
            OldWndProc(Message);
            case Message.WParam of
              SC_MAXIMIZE, SC_RESTORE, SC_MINIMIZE : begin
                TsSkinProvider(MDISkinProvider).FCommonData.BGChanged := True;
                TsSkinProvider(MDISkinProvider).FLinesCount := -1;
                SendMessage(TsSkinProvider(MDISkinProvider).Form.Handle, WM_NCPAINT, 0, 0);
              end;
            end;
          end
          else begin
            OldWndProc(Message);
            case Message.WParam of
              SC_KEYMENU : if (TWMSysCommand(Message).Key = VK_SPACE) and SystemMenu.VisibleClose{(bisystemMenu in Form.BorderIcons)} and HaveBorder(Self)
                then DropSysMenu(FormLeftTop.x + SysBorderWidth, FormLeftTop.y + BorderHeight + HeightOf(IconRect));
              SC_MAXIMIZE, SC_RESTORE : begin
                if SystemMenu.VisibleMax then CurrentHT := HTMAXBUTTON;
                SetHotHT(0);
                if (Message.WParam = SC_RESTORE) then begin
                  if not HaveBorder(Self) then begin
                    FCommonData.BGChanged := True;
                    RegionChanged := True;
                    SetWindowRgn(Form.Handle, 0, True);
                  end
                  else begin
                    FCommonData.BGChanged := True;
                    RegionChanged := True;
                    RedrawWindow(Form.Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE);
                  end
                end;
              end;
              SC_MINIMIZE : if Assigned(FCommonData.FCacheBmp) then begin
                FCommonData.FCacheBmp.Width := 0;
                FCommonData.FCacheBmp.Height := 0;
//                if (Application.MainForm = Form) then Application.Minimize; v6.10
              end;
            end;
          end;
        end;
      end;
      WM_WINDOWPOSCHANGING : begin
        if not RgnChanging then begin
          if not (wsMaximized = Form.WindowState) and FScreenSnap and ((TWMWindowPosChanging(Message).WindowPos^.X <> 0) or (TWMWindowPosChanging(Message).WindowPos^.Y <> 0)) then begin
            with TWMWindowPosChanging(Message).WindowPos^ do begin
{$IFDEF DELPHI6UP}
              HandleEdge(x, Form.Monitor.WorkareaRect.Right, FSnapBuffer, Form.Width);
              HandleEdge(y, Form.Monitor.WorkareaRect.Bottom, FSnapBuffer, Form.Height);
              HandleEdge(x, Form.Monitor.WorkareaRect.Left, FSnapBuffer, 0);
              HandleEdge(y, Form.Monitor.WorkareaRect.Top, FSnapBuffer, 0);
{$ELSE}
              HandleEdge(x, WorkRect.Right, FSnapBuffer, Form.Width);
              HandleEdge(y, WorkRect.Bottom, FSnapBuffer, Form.Height);
              HandleEdge(x, WorkRect.Left, FSnapBuffer, Form.Monitor.Left);
              HandleEdge(y, WorkRect.Top, FSnapBuffer, Form.Monitor.Top);
{$ENDIF}
            end;
          end;
          OldWndProc(Message);
        end;
      end;
      CM_COLORCHANGED : if SkinData.CustomColor then begin
        if not (csCreating in Form.ControlState) then FCommonData.BGChanged := True;
        OldWndProc(Message);
      end;
      WM_CREATE : begin
        if Assigned(TitleBG) then FreeAndNil(TitleBG);

        if (Form.FormStyle = fsMDIChild) and (MDISkinProvider = nil) then AddSupportedForm(Application.MainForm.Handle);

        if (Form.FormStyle = fsMDIChild) and
             ((TsSkinProvider(MDISkinProvider).Form.ActiveMDIChild) <> nil) and
               (TsSkinProvider(MDISkinProvider).Form.ActiveMDIChild.WindowState = wsMaximized) then begin
          MDICreating := True;
        end else MDICreating := False;
        OldWndProc(Message);
        if (Form.FormStyle <> fsMDIChild) then PrepareForm; // Problem with MDI menu solving
      end;
      WM_PARENTNOTIFY : if not (csLoading in ComponentState) and not (csLoading in Form.ComponentState) and ((Message.WParam and $FFFF = WM_CREATE) or (Message.WParam and $FFFF = WM_DESTROY)) then begin
        OldWndProc(Message);
        if Assigned(Adapter) and (Message.WParamLo = WM_CREATE) then Adapter.AddAllItems;
        acM := MakeMessage(SM_ALPHACMD, MakeWParam(0, AC_GETSKINSTATE), 1, 0);
        AlphaBroadCast(Form, acM);
        if FDrawNonClientArea
          then UpdateScrolls(ListSW, True);
      end
      else OldWndProc(Message);
      WM_NOTIFY : begin
        OldWndProc(Message);
        case TWMNotify(Message).NMHdr^.code of
          TCN_SELCHANGE : if Adapter <> nil then Adapter.AddAllItems;
        end;
      end;
      CM_CONTROLLISTCHANGE : begin
        if (TCMControlListChange(Message).Control <> nil) then begin
          OldWndProc(Message);
          if (TCMControlListChange(Message).Control is TWinControl) then begin
            if Adapter <> nil then Adapter.AddNewItem(TWinControl(TCMControlListChange(Message).Control));
            acM := MakeMessage(SM_ALPHACMD, MakeWParam(0, AC_GETSKINSTATE), 1, 0);
            AlphaBroadCast(TWinControl(TCMControlListChange(Message).Control), acM);
          end;
          TCMControlListChange(Message).Control.Perform(SM_ALPHACMD, MakeWParam(0, AC_GETSKINSTATE), 1);
        end
        else OldWndProc(Message);
      end
      else OldWndProc(Message);
    end;
  end;
end;

procedure TsSkinProvider.PaintBorderIcons;
var
  i, b, index, mi, Offset, x, y : integer;
  CI : TCacheInfo;
  procedure PaintButton(var Btn : TsCaptionButton; var Index : integer; SkinIndex : integer; BtnEnabled : boolean; UserBtn : TsTitleButton = nil);
  var
    w : integer;
  begin
    if UserBtn = nil then w := SysButtonWidth(Btn) else w := UserButtonWidth(UserBtn);
    Btn.Rect.Left := Btn.Rect.Right - w;
    if Btn.HaveAlignment then begin // If not user button and not small
      case FCommonData.SkinManager.SkinData.BIVAlign of
        -1, 0 : begin  // Center vert layout
          Btn.Rect.Top := (CaptionHeight - ButtonHeight + SysBorderHeight) div 2;
          if ButtonHeight < 16 then inc(Btn.Rect.Top, 2);
          Btn.Rect.Bottom := Btn.Rect.Top + ButtonHeight;
        end;
        1 : begin  // Top layout
          Btn.Rect.Top := iffi(Form.WindowState = wsMaximized, SysBorderHeight - 1, 0);
          Btn.Rect.Bottom := Btn.Rect.Top + ButtonHeight;
        end;
      end;
    end
    else begin
      Btn.Rect.Top := (CaptionHeight - ButtonHeight + SysBorderHeight) div 2;
      if BigButtons(Self) and (ButtonHeight < 16) then inc(Btn.Rect.Top, 2);
      Btn.Rect.Bottom := Btn.Rect.Top + ButtonHeight;
    end;

    if SkinIndex > -1 then DrawSkinGlyph(FCommonData.FCacheBmp, Point(Btn.Rect.Left, Btn.Rect.Top),
      Btn.State, 1 + integer(not FormActive or not BtnEnabled), FCommonData.SkinManager.ma[SkinIndex], MakeCacheInfo(SkinData.FCacheBmp));
    inc(Index);
  end;
begin
  if not HaveBorder(Self) then Exit;
  b := 1;
  Offset := CaptionWidth - SysBorderWidth - integer(BigButtons(Self)) * FCommonData.SkinManager.SkinData.BISpacing - FCommonData.SkinManager.SkinData.BIRightMargin;
  RestoreBtnsBG;
  if SystemMenu.VisibleClose then begin // Accommodation of a buttons in a special order...
    if FCommonData.SkinManager.IsValidImgIndex(ButtonClose.ImageIndex) then begin
      ButtonClose.Rect.Right := Offset;
      PaintButton(ButtonClose, b, ButtonClose.ImageIndex, True);
      Offset := ButtonClose.Rect.Left - integer(BigButtons(Self)) * FCommonData.SkinManager.SkinData.BISpacing;
    end;
    if SystemMenu.VisibleMax then begin
      if Form.WindowState <> wsMaximized then begin
        if FCommonData.SkinManager.IsValidImgIndex(ButtonMax.ImageIndex) then begin
          ButtonMax.Rect.Right := Offset;
          PaintButton(ButtonMax, b, ButtonMax.ImageIndex, SystemMenu.EnabledMax);
          Offset := ButtonMax.Rect.Left - integer(BigButtons(Self)) * FCommonData.SkinManager.SkinData.BISpacing;
        end;
      end
      else begin
        i := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinManager.ConstData.IndexGLobalInfo, s_GlobalInfo, s_BorderIconNormalize);
        if i < 0 then i := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinIndex, FCommonData.SkinSection, s_BorderIconNormalize); // For compatibility
        if i > -1 then begin
          ButtonMax.Rect.Right := Offset;
          PaintButton(ButtonMax, b, i, SystemMenu.EnabledRestore);
          Offset := ButtonMax.Rect.Left - integer(BigButtons(Self)) * FCommonData.SkinManager.SkinData.BISpacing;
        end;
      end;
    end;
    if SystemMenu.VisibleMin then begin
      if Form.WindowState = wsMinimized then begin // If form is minimized then changing to Normalize
        i := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinManager.ConstData.IndexGLobalInfo, s_GlobalInfo, s_BorderIconNormalize);
        if i < 0 then i := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinIndex, FCommonData.SkinSection, s_BorderIconNormalize);
        if FCommonData.SkinManager.IsValidImgIndex(i) then begin
          ButtonMin.Rect.Right := Offset;
          PaintButton(ButtonMin, b, i, SystemMenu.EnabledRestore); // For compatibility
          Offset := ButtonMin.Rect.Left - integer(BigButtons(Self)) * FCommonData.SkinManager.SkinData.BISpacing;
        end;
      end
      else begin
        if FCommonData.SkinManager.IsValidImgIndex(ButtonMin.ImageIndex) then begin
          ButtonMin.Rect.Right := Offset;
          PaintButton(ButtonMin, b, ButtonMin.ImageIndex, SystemMenu.EnabledMin);
          Offset := ButtonMin.Rect.Left - integer(BigButtons(Self)) * FCommonData.SkinManager.SkinData.BISpacing;
        end;
      end;
    end;
    if biHelp in Form.BorderIcons then begin
      if FCommonData.SkinManager.IsValidImgIndex(ButtonHelp.ImageIndex) then begin
        ButtonHelp.Rect.Right := Offset;
        PaintButton(ButtonHelp, b, ButtonHelp.Imageindex, True);
        Offset := ButtonHelp.Rect.Left - integer(BigButtons(Self)) * FCommonData.SkinManager.SkinData.BISpacing;
      end;
    end;
  end; 

  if TitleButtons.Count > 0 then begin
    mi := UserBtnIndex;
    Offset := Offset - UserButtonsOffset;
    for index := 0 to TitleButtons.Count - 1 do begin
      if TitleButtons.Items[index].UseSkinData and FCommonData.SkinManager.IsValidImgIndex(mi) then begin
        TitleButtons.Items[index].BtnData.ImageIndex := mi;
        TitleButtons.Items[index].BtnData.Rect.Right := Offset;
        PaintButton(TitleButtons.Items[index].BtnData, b, mi, TitleButtons.Items[index].Enabled, TitleButtons.Items[index]);
        Offset := TitleButtons.Items[index].BtnData.Rect.Left - integer(BigButtons(Self)) * FCommonData.SkinManager.SkinData.BISpacing;
      end
      else begin
        TitleButtons.Items[index].BtnData.ImageIndex := -1;
        TitleButtons.Items[index].BtnData.Rect.Right := Offset;
        PaintButton(TitleButtons.Items[index].BtnData, b, -1, TitleButtons.Items[index].Enabled, TitleButtons.Items[index]);
        Offset := TitleButtons.Items[index].BtnData.Rect.Left - integer(BigButtons(Self)) * FCommonData.SkinManager.SkinData.BISpacing;
      end;
      if Assigned(TitleButtons.Items[index].Glyph) then begin
        if TitleButtons.Items[index].Glyph.PixelFormat = pf32bit then begin
          x := TitleButtons.Items[index].BtnData.Rect.Left + (UserButtonWidth(TitleButtons.Items[index]) - TitleButtons.Items[index].Glyph.Width) div 2;
          y := TitleButtons.Items[index].BtnData.Rect.Top + (ButtonHeight - TitleButtons.Items[index].Glyph.Height) div 2;
          CI := MakeCacheInfo(FCommonData.FCacheBmp, x, y);
          CopyByMask(Rect(x, y, x + TitleButtons.Items[index].Glyph.Width, y + TitleButtons.Items[index].Glyph.Height),
                     Rect(0, 0, TitleButtons.Items[index].Glyph.Width, TitleButtons.Items[index].Glyph.Height),
                     FCommonData.FCacheBmp,
                     TitleButtons.Items[index].Glyph,
                     CI, True);
        end
        else begin
          TitleButtons.Items[index].Glyph.PixelFormat := pf24bit;
          CopyTransBitmaps(FCommonData.FCacheBmp, TitleButtons.Items[index].Glyph,
                 TitleButtons.Items[index].BtnData.Rect.Left + (UserButtonWidth(TitleButtons.Items[index]) - TitleButtons.Items[index].Glyph.Width) div 2,
                 TitleButtons.Items[index].BtnData.Rect.Top + (ButtonHeight - TitleButtons.Items[index].Glyph.Height) div 2,
                 ColorToSColor(TitleButtons.Items[index].Glyph.Canvas.Pixels[0, TitleButtons.Items[index].Glyph.Height - 1]));
        end;
      end;
    end;
  end;

  // Drawing of MDI child buttons if maximized
  if MDIButtonsNeeded then begin
    if FCommonData.SkinManager.IsValidImgIndex(MDIMin.ImageIndex) then begin
      MDIMin.Rect := Bounds(
                          CaptionWidth - SysBorderWidth - 3 * (SmallButtonWidth + 1),
                          HeaderHeight - (MenuHeight - SmallButtonHeight) div 2 - SmallButtonHeight - 1,
                          SmallButtonWidth, SmallButtonHeight
                         );

      DrawSkinGlyph(FCommonData.FCacheBmp, Point(MDIMin.Rect.Left, MDIMin.Rect.Top),
                MDIMin.State, 1 + integer(not FormActive or not ChildProvider.SystemMenu.EnabledMin), FCommonData.SkinManager.ma[MDIMin.ImageIndex], MakeCacheInfo(SkinData.FCacheBmp));
    end;
    if Assigned(TsSkinProvider(MDISkinProvider).Form.ActiveMDIChild) and // Drawing of norm. button when maximized
          (TsSkinProvider(MDISkinProvider).Form.ActiveMDIChild.WindowState <> wsMaximized) then begin
      i := MDIMax.ImageIndex
    end
    else begin
      i := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinManager.ConstData.IndexGlobalInfo, s_GlobalInfo, s_SmallIconNormalize);
      if i < 0 then i := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinIndex, s_GlobalInfo, s_BorderIconNormalize);
    end;
    MDIMax.Rect := Bounds(
                          Form.Width - SysBorderWidth - 2 * (SmallButtonWidth + 1),
                          HeaderHeight - (MenuHeight - SmallButtonHeight) div 2 - SmallButtonHeight - 1,
                          SmallButtonWidth, SmallButtonHeight
                         );
    if FCommonData.SkinManager.IsValidImgIndex(i) then begin
      DrawSkinGlyph(FCommonData.FCacheBmp, Point(MDIMax.Rect.Left, MDIMax.Rect.Top),
              MDIMax.State, 1 + integer(not FormActive or not ChildProvider.SystemMenu.EnabledRestore), FCommonData.SkinManager.ma[i], MakeCacheInfo(SkinData.FCacheBmp));
    end;
    if FCommonData.SkinManager.IsValidImgIndex(MDIClose.ImageIndex) then begin
      MDIClose.Rect := Bounds(Form.Width - SysBorderWidth - SmallButtonWidth - 1,
                              HeaderHeight - (MenuHeight - SmallButtonHeight) div 2 - SmallButtonHeight - 1,
                              SmallButtonWidth, SmallButtonHeight);

      DrawSkinGlyph(FCommonData.FCacheBmp, Point(MDIClose.Rect.Left, MDIClose.Rect.Top),
                MDIClose.State, 1 + integer(not FormActive), FCommonData.SkinManager.ma[MDIClose.ImageIndex], MakeCacheInfo(SkinData.FCacheBmp));
    end;
  end;
end;

procedure TsSkinProvider.PaintCaption(DC : hdc);
var
  h, hmnu : integer;
begin
  h := SysBorderHeight + CaptionHeight;
  if IsIconic(Form.Handle) then inc(h);
  if FCommonData.BGChanged or not IsCached(FCommonData) then begin
    if MenuChanged or IsBorderUnchanged(FCommonData.BorderIndex, FCommonData.SkinManager) or (TitleBG = nil) or (TitleBG.Width <> CaptionWidth) or (FCommonData.FCacheBmp.Height <> Form.Height) or not HaveBorder(Self) then begin// if ready caption BG
      ControlsChanged := not FirstInitialized;
    end;
    PaintAll;
    FCommonData.BGChanged := False;
    FCommonData.Updating := False;
  end;
  hmnu := integer(MenuPresent) * LinesCount * MenuHeight;

  // Title and menu line update
  BitBlt(DC, 0, 0, Form.Width, HeaderHeight, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
//  FillDC(DC, Rect(0, 0, Form.Width, HeaderHeight), clYellow);
  // Left border update
  BitBlt(DC, 0, h + hmnu + TForm(Form).BorderWidth, SysBorderWidth + TForm(Form).BorderWidth, FCommonData.FCacheBmp.Height, FCommonData.FCacheBmp.Canvas.Handle, 0, h + hmnu + TForm(Form).BorderWidth, SRCCOPY);
  // Bottom border update
  BitBlt(DC, BorderWidth, Form.Height - BorderHeight, Form.Width - BorderWidth, BorderHeight,
             FCommonData.FCacheBmp.Canvas.Handle,
             SysBorderwidth + TForm(Form).BorderWidth,
             Form.Height - SysBorderHeight - TForm(Form).BorderWidth, SRCCOPY);
  // Right border update
  BitBlt(DC, FCommonData.FCacheBmp.Width - SysBorderWidth - TForm(Form).BorderWidth, h + hmnu + TForm(Form).BorderWidth , SysBorderHeight + TForm(Form).BorderWidth, FCommonData.FCacheBmp.Height, FCommonData.FCacheBmp.Canvas.Handle, FCommonData.FCacheBmp.Width - SysBorderwidth - TForm(Form).BorderWidth, h + hmnu + TForm(Form).BorderWidth, SRCCOPY);
//  if IsGripVisible(Self) then PaintGrip(DC, Self);
// v6.0  if Assigned(acMagnForm) then SendMessage(acMagnForm.Handle, SM_ALPHACMD, MakeWParam(0, AC_REFRESH), 0);
end;

procedure TsSkinProvider.PaintForm(DC : hdc; SendUpdated : boolean = True);
var
  Changed : boolean;
  i : integer;
  SaveIndex : hdc;
  TempBmp : TBitmap;
  R : TRect;
begin
  R := Form.ClientRect;
//  if not FDrawNonClientArea then inc(R.Top, MenuHeight);
  if SkinData.CtrlSkinState and ACS_FAST = ACS_FAST then begin
    FillDC(DC, R, FormColor);
  end
  else begin
    if FCommonData.GraphControl <> nil then begin
      for i := 0 to Form.ControlCount - 1 do if Form.Controls[i] = FCommonData.GraphControl then begin
        if csPaintCopy in Form.ControlState then Form.Controls[i].ControlState := Form.Controls[i].ControlState + [csPaintCopy];
        SaveIndex := SaveDC(DC);
        MoveWindowOrg(DC, Form.Controls[i].Left, Form.Controls[i].Top);
        IntersectClipRect(DC, 0, 0, Form.Controls[i].Width, Form.Controls[i].Height);

        if not (csOpaque in Form.Controls[i].ControlStyle) then begin
          TempBmp := CreateBmp24(Form.Controls[i].Width, Form.Controls[i].Height);
          BitBlt(TempBmp.Canvas.Handle, 0, 0, Form.Controls[i].Width, Form.Controls[i].Height, SkinData.FCacheBmp.Canvas.Handle, Form.Controls[i].Left + OffsetX, Form.Controls[i].Top + OffsetY, SRCCOPY);
          Form.Controls[i].Perform(WM_PAINT, longint(TempBmp.Canvas.Handle), 0);
          BitBlt(DC, 0, 0, Form.Controls[i].Width, Form.Controls[i].Height, TempBmp.Canvas.Handle, 0, 0, SRCCOPY);
          FreeAndNil(TempBmp);
        end
        else Form.Controls[i].Perform(WM_PAINT, longint(DC), 0);

        RestoreDC(DC, SaveIndex);
        Form.Controls[i].ControlState := Form.Controls[i].ControlState - [csPaintCopy];
        Break;
      end;
      Exit;
    end;
    Changed := FCommonData.BGChanged;
    if not FCommonData.UrgentPainting
      then PaintAll;
    if (Form.FormStyle <> fsMDIForm) then begin
      CopyWinControlCache(Form, FCommonData, Rect(SysBorderWidth + Form.BorderWidth, OffsetY, 0, 0), Rect(0, 0, Form.Width, Form.Height), DC, False);
      PaintControls(DC, Form, ControlsChanged or Changed, Point(0, 0));
    end;
    if SendUpdated then begin
      SetParentUpdated(Form);
      SendToAdapter(MakeMessage(SM_ALPHACMD, MakeWParam(0, AC_ENDPARENTUPDATE), 0, 0));
    end;
  end;
  ControlsChanged := False;
end;

procedure TsSkinProvider.SetHotHT(i: integer; Repaint : boolean = True);
begin
  if not FDrawNonClientArea then Exit;
  if HotItem.Item <> nil then begin
    RepaintMenuItem(HotItem.Item, HotItem.R, []);
    HotItem.Item := nil;
  end;
  if (CurrentHT = i) then Exit;
  if (CurrentHT <> 0) then begin
    case CurrentHT of
      HTCLOSE : ButtonClose.State := 0;
      HTMAXBUTTON : ButtonMax.State := 0;
      HTMINBUTTON : ButtonMin.State := 0;
      HTHELP : ButtonHelp.State := 0;
      HTCHILDCLOSE : MDIClose.State := 0;
      HTCHILDMAX : MDIMax.State := 0;
      HTCHILDMIN : MDIMin.State := 0
      else if BetWeen(CurrentHT, HTUDBTN, HTUDBTN + TitleButtons.Count - 1) then begin
        TitleButtons.Items[CurrentHT - HTUDBTN].BtnData.State := 0;
      end;
    end;
    if Repaint then RepaintButton(CurrentHT);
  end;
  CurrentHT := i;
  case CurrentHT of
    HTCLOSE : ButtonClose.State := 1;
    HTMAXBUTTON : ButtonMax.State := 1;
    HTMINBUTTON : ButtonMin.State := 1;
    HTHELP : ButtonHelp.State := 1;
    HTCHILDCLOSE : MDIClose.State := 1;
    HTCHILDMAX : MDIMax.State := 1;
    HTCHILDMIN : MDIMin.State := 1
    else if BetWeen(CurrentHT, HTUDBTN, HTUDBTN + TitleButtons.Count - 1) then begin
      TitleButtons.Items[CurrentHT - HTUDBTN].BtnData.State := 1;
    end;
  end;
  biClicked := False;
  if Repaint then RepaintButton(CurrentHT);
end;

procedure TsSkinProvider.SetPressedHT(i: integer);
begin
  if (CurrentHT <> i) and (CurrentHT <> 0) then begin
    case CurrentHT of
      HTCLOSE : ButtonClose.State := 0;
      HTMAXBUTTON : ButtonMax.State := 0;
      HTMINBUTTON : ButtonMin.State := 0;
      HTHELP : ButtonHelp.State := 0;
      HTCHILDCLOSE : MDIClose.State := 0;
      HTCHILDMAX : MDIMax.State := 0;
      HTCHILDMIN : MDIMin.State := 0
      else if BetWeen(CurrentHT, HTUDBTN, HTUDBTN + TitleButtons.Count - 1) then begin
        TitleButtons.Items[CurrentHT - HTUDBTN].BtnData.State := 0;
      end;
    end;
    RepaintButton(CurrentHT);
  end;
  CurrentHT := i;
  case CurrentHT of
    HTCLOSE : ButtonClose.State := 2;
    HTMAXBUTTON : if SystemMenu.EnabledMax or ((Form.WindowState = wsMaximized) and SystemMenu.EnabledRestore) then ButtonMax.State := 2 else begin
      CurrentHT := 0;
      Exit;
    end;
    HTMINBUTTON : ButtonMin.State := 2;
    HTHELP : ButtonHelp.State := 2;
    HTCHILDCLOSE : MDIClose.State := 2;
    HTCHILDMAX : if SystemMenu.EnabledMax then MDIMax.State := 2;
    HTCHILDMIN : MDIMin.State := 2
    else if BetWeen(CurrentHT, HTUDBTN, HTUDBTN + TitleButtons.Count - 1) then begin
      TitleButtons.Items[CurrentHT - HTUDBTN].BtnData.State := 2;
    end;
  end;
  biClicked := True;
  RepaintButton(CurrentHT);
end;

type
  TsHackedItem = class(TMenuItem)end;

procedure TsSkinProvider.PaintAll;
var
  VisIndex, Index, i, CY : integer;
  x, w, h, y : integer;
  s : acString;
  r, rForm, rC : TRect;
  ci : TCacheInfo;
  CurrentItem : TMenuItem;
  ItemProcessed : integer;
  ChangedIndex : integer;
  Iconic : boolean;
  SavedCanvas, SavedDC : hdc;
  ts : TSize;
  function ProcessMerged(CurrentIndex : integer) : boolean;
  var
    i, j, VisJ, w : integer;
    LocalItem : TMenuItem;
  begin
    Result := False;
    if Assigned(Form.ActiveMDIChild) and Assigned(Form.ActiveMDIChild.Menu) then begin
      for i := ItemProcessed to Form.ActiveMDIChild.Menu.Items.Count - 1 do if Form.ActiveMDIChild.Menu.Items[i].Visible then begin
        LocalItem := Form.ActiveMDIChild.Menu.Items[i];

        // If MDI form and included other
        if (LocalItem.GroupIndex > ChangedIndex) and (LocalItem.GroupIndex <= CurrentIndex) then begin

          if not Assigned(LocalItem.OnMeasureItem) or not Assigned(LocalItem.OnAdvancedDrawItem) then Continue;

          Result := (LocalItem.GroupIndex >= CurrentIndex);
          ChangedIndex := LocalItem.GroupIndex;

          j := i;
          VisJ := j;
          while (j < Form.ActiveMDIChild.Menu.Items.Count) do begin
            LocalItem := Form.ActiveMDIChild.Menu.Items[j];
            if (LocalItem.GroupIndex > CurrentIndex{v5.46}) and (Index <= Form.Menu.Items.Count - 1) then Exit;
            GetMenuItemRect(Form.ActiveMDIChild.Handle, Form.ActiveMDIChild.Menu.Handle, VisJ, R);
            w := WidthOf(R);
            ChangedIndex := LocalItem.GroupIndex;

            if x + w > Form.Width - 2 * SysBorderWidth - 2 * TForm(Form).BorderWidth then begin
              x := SysBorderWidth;
              inc(y, MenuHeight);
            end;

            r := Rect(x, y, x + w, y + MenuHeight);
            LocalItem.OnAdvancedDrawItem(LocalItem, FCommonData.FCacheBmp.Canvas, R, []);
            x := r.Right;
            ItemProcessed := i + 1;
            inc(j);
            inc(VisIndex);
            inc(VisJ);
          end;
        end;
      end;
    end;
  end;
begin
  if (csCreating in Form.ControlState) then Exit;
  if FCommonData.FCacheBmp = nil then begin // If first loading
    InitCacheBmp(FCommonData);
    UpdateSkinState(FCommonData, False);
    acM := MakeMessage(SM_ALPHACMD, MakeWParam(0, AC_GETSKINSTATE), 1, 0); // Initialization of all child states
    if not (csLoading in Form.ComponentState)
      then AlphaBroadCast(Form, acM);
  end;
  if (Form.FormStyle = fsMDIForm) and (Form.ActiveMDIChild <> nil) and (Form.ActiveMDIChild.WindowState = wsMaximized) then begin
    if (fsShowing in Form.ActiveMDIChild.FormState) and MenuChanged then Exit;
  end;
  Iconic := IsIconic(Form.Handle);
  CY := SysBorderHeight;
  h := 2 * CY + CaptionHeight;
  ItemProcessed := 0;
  if Iconic
    then rForm := Rect(0, 0, CaptionWidth, h - CY + 1)
    else rForm := Rect(0, 0, CaptionWidth, Form.Height);
//  if not IsCached(FCommonData) and (Form.Height <> FCommonData.FCacheBmp.Height) then FCommonData.BGChanged := True;
  if FCommonData.BGChanged then begin
    if not MenuChanged and IsBorderUnchanged(FCommonData.BorderIndex, FCommonData.SkinManager) and (TitleBG <> nil) and (TitleBG.Width = CaptionWidth) and (FCommonData.FCacheBmp.Height = Form.Height) and (CaptionHeight <> 0) then begin // if ready caption BG
      // Drawn caption only
      if FDrawNonClientArea then begin
        ci := MakeCacheInfo(FCommonData.FCacheBmp);
        BitBlt(FCommonData.FCacheBmp.Canvas.Handle, 0, 0, TitleBG.Width, TitleBG.Height, TitleBG.Canvas.Handle, 0, 0, SRCCOPY);
        PrepareCaption(CI, Rect(0, 0, TitleBG.Width, TitleBG.Height));
      end;
    end
    else begin
      RgnChanged := True;
      ci.Ready := False;
      FCommonData.FCacheBmp.Width := CaptionWidth;
      FCommonData.FCacheBmp.Height := Form.Height;
      if FCommonData.SkinManager.IsValidSkinIndex(FCommonData.SkinIndex) then begin
        // Paint body
        if SkinData.CtrlSkinState and ACS_FAST <> ACS_FAST then begin
          if {(CaptionHeight <> 0) or v5.50}HaveBorder(Self)
            then PaintItem(FCommonData, EmptyCI, False, integer(FormActive), rForm, Point(0, 0), FCommonData.FCacheBmp, False)
            else PaintItemBG(FCommonData.SkinIndex, FCommonData.SkinSection, EmptyCI, integer(FormActive), rForm, Point(0, 0), FCommonData.FCacheBmp, FCommonData.SkinManager);
        end
        else begin
          FillDC(FCommonData.FCacheBmp.Canvas.Handle, Rect(0, 0, FCommonData.FCacheBmp.Width, CaptionHeight + BorderWidth + LinesCount * MenuHeight + 1), FormColor);
          PaintBorderFast(FCommonData.FCacheBmp.Canvas.Handle, rForm, BorderWidth, FCommonData, integer(FormActive));
        end;

        if IsGripVisible(Self) and IsCached(FCommonData)then begin
          FCommonData.BGChanged := False;
          PaintGrip(FCommonData.FCacheBmp.Canvas.Handle, Self);
        end;

        if IsBorderUnchanged(FCommonData.BorderIndex, FCommonData.SkinManager) and ((TitleBG = nil) or (TitleBG.Width <> CaptionWidth)) then MakeTitleBG;
        ci := MakeCacheInfo(FCommonData.FCacheBmp, OffsetX, OffsetY); // Prepare cache info
        if (CaptionHeight <> 0) and FDrawNonClientArea then begin // Paint title
          i := FCommonData.SkinManager.GetSkinIndex(TitleSkinSection);
          if FCommonData.SkinManager.IsValidSkinIndex(i) then if Iconic
            then PaintItem(i, TitleSkinSection, ci, True, integer(FormActive), Rect(0, 0, rForm.Right, rForm.Bottom), Point(0, 0), FCommonData.FCacheBmp, FCommonData.SkinManager)
            else PaintItem(i, TitleSkinSection, ci, True, integer(FormActive), Rect(0, 0, rForm.Right, h - CY), Point(0, 0), FCommonData.FCacheBmp, FCommonData.SkinManager);
          DrawAppIcon(Self); // Draw app icon
          if (SysButtonsCount > 0) then begin // Paint title toolbar if exists
            i := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinIndex, FCommonData.SkinSection, s_NormalTitleBar);
            if FCommonData.SkinManager.IsValidImgIndex(i)
              then DrawSkinRect(FCommonData.FCacheBmp, Rect(CaptionWidth - BarWidth(i), 0, FCommonData.FCacheBmp.Width, h - CY),
                                 True, EmptyCI, FCommonData.SkinManager.ma[i], integer(FormActive), True);
          end;
        end;

        if (Form.Menu <> nil) and MenuPresent and (MenuHeight > 0) then begin // Menu line drawing
          i := -1;
          if FMenuLineSkin <> '' then i := FCommonData.SkinManager.GetSkinIndex(FMenuLineSkin);
          if i < 0 then begin
            i := FCommonData.SkinManager.GetSkinIndex(s_MenuLine); // Paint menu bar
            if FCommonData.SkinManager.IsValidSkinIndex(i) then PaintItem(i, s_MenuLine, ci, True, integer(FormActive),
                      Rect(0, CaptionHeight + SysBorderHeight, FCommonData.FCacheBmp.Width, CaptionHeight + SysBorderHeight + LinesCount * MenuHeight),
                      Point(0, CaptionHeight + SysBorderHeight), FCommonData.FCacheBmp, FCommonData.SkinManager);
          end
          else begin
            if FCommonData.SkinManager.IsValidSkinIndex(i) then PaintItem(i, FMenuLineSkin, ci, True, integer(FormActive),
                      Rect(0, CaptionHeight + SysBorderHeight, FCommonData.FCacheBmp.Width, CaptionHeight + SysBorderHeight + LinesCount * MenuHeight),
                      Point(0, CaptionHeight + SysBorderHeight), FCommonData.FCacheBmp, FCommonData.SkinManager);
          end;
          MenuLineBmp.Width := CaptionWidth; // Store bg for Menu line
          MenuLineBmp.Height := LinesCount * MenuHeight;
          BitBlt(MenuLineBmp.Canvas.Handle, 0, 0, MenuLineBmp.Width, MenuLineBmp.Height, FCommonData.FCacheBmp.Canvas.Handle, 0, SysBorderHeight + CaptionHeight{ + Form.BorderWidth}, SRCCOPY);
          // Draw maximized child form system icon on menuline
          if ChildIconPresent and (MDISkinProvider = Self) then begin
            if Form.ActiveMDIChild.Icon.Handle <> 0
              then DrawIconEx(FCommonData.FCacheBmp.Canvas.Handle, SysBorderWidth + 1, CaptionHeight + SysBorderHeight + 1, Form.ActiveMDIChild.Icon.Handle, MenuHeight - 2, MenuHeight - 2, 0, 0, di_Normal)
              else if AppIcon <> nil
                then DrawIconEx(FCommonData.FCacheBmp.Canvas.Handle, SysBorderWidth + 1, CaptionHeight + SysBorderHeight + 1, AppIcon.Handle, MenuHeight - 2, MenuHeight - 2, 0, 0, di_Normal)
                else DrawIconEx(FCommonData.FCacheBmp.Canvas.Handle, SysBorderWidth + 1, CaptionHeight + SysBorderHeight + 1, LoadIcon(0, IDI_APPLICATION), MenuHeight - 2, MenuHeight - 2, 0, 0, di_Normal);
          end;
          /////// Painting of menu items ///////
          x := SysBorderWidth;
          y := SysBorderHeight + CaptionHeight;
          ChangedIndex := -1;
          Index := 0;
          VisIndex := 0;

          if AeroIsEnabled and (Form.Menu <> nil)
            then FCommonData.SkinManager.SkinableMenus.InitMenuLine(Form.Menu, FDrawNonCLientArea);

          while Index < Form.Menu.Items.Count do begin
            if (x = SysBorderWidth) and (MDISkinProvider = Self) and ChildIconPresent then begin // Skip Item with child icon
              GetMenuItemRect(Form.Handle, Form.Menu.Handle, 0, R);
              inc(x, WidthOf(R));
              inc(VisIndex);
              Continue;
            end;
            CurrentItem := Form.Menu.Items[Index];
            if ((CurrentItem.GroupIndex = ChangedIndex) or ProcessMerged(CurrentItem.GroupIndex)) then begin
              inc(Index); continue;
            end
            else begin
              if not Assigned(CurrentItem.OnMeasureItem) or not Assigned(CurrentItem.OnAdvancedDrawItem) or not CurrentItem.Visible then begin
                inc(Index); Continue;
              end;
              GetMenuItemRect(Form.Handle, Form.Menu.Handle, VisIndex, R);
              w := WidthOf(R);
              OffsetRect(R, -Form.Left, -Form.Top);
              CurrentItem.OnAdvancedDrawItem(CurrentItem, FCommonData.FCacheBmp.Canvas, R, [odNoAccel, odReserved1]);
              inc(x, w);
              inc(Index);
              inc(VisIndex);
            end;
          end;
          ProcessMerged(99999);
        end;
        if (CaptionHeight <> 0) and FDrawNonClientArea then begin
          SaveBGForBtns(True);
          // Paint buttons
          PaintBorderIcons;
          // Out the title text
          FCommonData.FCacheBmp.Canvas.Font.Assign(Form.Font);
          FCommonData.FCacheBmp.Canvas.Font.Style := FCommonData.FCacheBmp.Canvas.Font.Style + [fsBold];
          FCommonData.FCacheBmp.Canvas.Font.Height := GetCaptionFontSize;
          R := Rect(SysBorderWidth + integer(IconVisible) * WidthOf(IconRect) + 4 + SkinData.SkinManager.SkinData.BILeftMargin,
                    CY, rForm.Right - TitleBtnsWidth - 6, CaptionHeight);
          if not IsRectEmpty(R) then begin
{$IFDEF TNTUNICODE}
            s := WideString(GetWndText(Form.Handle));
            GetTextExtentPoint32W(FCommonData.FCacheBmp.Canvas.Handle, PWideChar(s), Length(s), ts);
{$ELSE}
            s := Form.Caption;
            GetTextExtentPoint32(FCommonData.FCacheBmp.Canvas.Handle, PChar(s), Length(s), ts);
{$ENDIF}
            R.Top := R.Top + (HeightOf(R) - ts.cy) div 2;
            R.Bottom := R.Top + ts.cy;
            acWriteTextEx(FCommonData.FCacheBmp.Canvas, PacChar(s), Form.Enabled, R,
              GetStringFlags(Form, FCaptionAlignment) or DT_END_ELLIPSIS or DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX,
              FCommonData.SkinManager.GetSkinIndex(TitleSkinSection), FormActive, FCommonData.SkinManager)
          end;
        end;
        // Paint MDIArea
        if (Form.FormStyle = fsMDIForm) and Assigned(MDIForm) then begin
          rC.Left := BorderWidth + GetAlignShift(Form, alLeft);
          rC.Top := SysBorderHeight * 2 + CaptionHeight + LinesCount * MenuHeight * integer(MenuPresent) + Form.BorderWidth - 2 + GetAlignShift(Form, alTop);
          rC.Right := Form.Width - BorderWidth - GetAlignShift(Form, alRight);
          rC.Bottom := Form.Height - BorderWidth - GetAlignShift(Form, alBottom);
          CI := MakeCacheInfo(FCommonData.FCacheBmp);
          i := FCommonData.SkinManager.GetSkinIndex(s_MDIArea);
          PaintItem(i, s_MDIArea, CI, False, 0, Rect(rC.Left, rC.Top, rC.Right, rC.Bottom), rC.TopLeft, FCommonData.FCacheBmp.Canvas.Handle, FCommonData.SkinManager);
        end;
      end;
      MenuChanged := False;
      FCommonData.BGChanged := False;
      if Assigned(Form.OnPaint) and IsCached(SkinData) then begin
        SavedCanvas := Form.Canvas.Handle;
        Form.Canvas.Handle := SkinData.FCacheBmp.Canvas.Handle;
        SavedDC := SaveDC(Form.Canvas.Handle);
        MoveWindowOrg(Form.Canvas.Handle, OffsetX, OffsetY);
        Form.Canvas.Lock;
        Form.OnPaint(Form);
        Form.Canvas.Unlock;
        RestoreDC(Form.Canvas.Handle, SavedDC);
        Form.Canvas.Handle := SavedCanvas;
      end;
    end;
    FirstInitialized := True;
    FCommonData.BGChanged := False;
  end;
  FCommonData.Updating := False;
end;

function TsSkinProvider.FormLeftTop: TPoint;
var
  p : TPoint;
  R : TRect;
begin
  if TForm(Form).FormStyle = fsMDIChild then begin
    p := Point(0, 0);
    p := Form.ClientToScreen(p);
    Result.x := p.x - SysBorderWidth;
    Result.y := p.y - SysBorderHeight - integer(not IsIconic(Form.Handle)) * CaptionHeight;
  end
  else begin
    GetWindowRect(Form.Handle, R);
    Result.x := R.Left;
    Result.y := R.Top;
  end;
end;

function TsSkinProvider.IconRect: TRect;
begin
  Result.Left := SysBorderWidth + SkinData.SkinManager.SkinData.BILeftMargin;
  Result.Right := Result.Left + TitleIconWidth(Self);
  Result.Top := (CaptionHeight + SysBorderHeight - TitleIconHeight(Self)) div 2;
  Result.Bottom := Result.Top + TitleIconHeight(Self);
end;

procedure TsSkinProvider.DropSysMenu(x, y : integer);
var
  mi : TMenuitem;
  SysMenu: HMENU;
  SelItem: DWORD;
  procedure EnableCommandItem(uIDEnableItem : UINT; Enable : Boolean);
  begin
    if Enable
      then EnableMenuItem(SysMenu, uIDEnableItem, MF_BYCOMMAND or MF_ENABLED)
      else EnableMenuItem(SysMenu, uIDEnableItem, MF_BYCOMMAND or MF_GRAYED or MF_DISABLED);
  end;
begin
  SystemMenu.UpdateItems(SystemMenu.ExtItemsCount > 0);
  if FMakeSkinMenu then begin
    if SystemMenu.Items[0].Name = s_SkinSelectItemName then begin
      while SystemMenu.Items[0].Count > 0 do begin
        mi := SystemMenu.Items[0].Items[0];
        SystemMenu.Items[0].Delete(0);
        FreeAndNil(mi);
      end;
      mi := SystemMenu.Items[0];
      SystemMenu.Items.Delete(0);
      FreeAndNil(mi);
    end;
    SystemMenu.MakeSkinItems;
  end;
  SkinData.SkinManager.SkinableMenus.HookPopupMenu(SystemMenu, True);

  if SkinData.SkinManager.SkinnedPopups then
  begin
    SkinData.SkinManager.SkinableMenus.HookPopupMenu(SystemMenu, True);
    SystemMenu.WindowHandle := Form.Handle;
    SystemMenu.Popup(x, y);
    exit;
  end;

  // Prevent of painting by system (white line)
  SetWindowLong(Form.Handle, GWL_STYLE, GetWindowLong(Form.Handle, GWL_STYLE) and not WS_VISIBLE);
  //get a modifiable copy of the original sysmenu
  SysMenu := GetSystemMenu(Form.Handle, False);
  //read and emulate states from skin SystemMenu
  with SystemMenu do begin
    EnableCommandItem(SC_RESTORE , VisibleRestore And EnabledRestore);
    EnableCommandItem(SC_MOVE    , EnabledMove and not IsIconic(Form.Handle));
    EnableCommandItem(SC_SIZE    , VisibleSize And EnabledSize And (Form.WindowState = wsNormal));
    EnableCommandItem(SC_MINIMIZE, VisibleMin And EnabledMin);
    EnableCommandItem(SC_MAXIMIZE, VisibleMax And EnabledMax);
    EnableCommandItem(SC_CLOSE   , VisibleClose);
  end;
  SetWindowLong(Form.Handle, GWL_STYLE, GetWindowLong(Form.Handle, GWL_STYLE) or WS_VISIBLE);
  //Get menuselection from menu, do not send it on automatically
  SelItem := LongWord(Windows.TrackPopupMenu( SysMenu, TPM_LEFTBUTTON or TPM_RIGHTBUTTON or TPM_RETURNCMD, x, y, 0, Form.Handle, nil) );
  //If the sysmenu tracking resulted in a selection, post it as a WM_SYSCOMMAND
  if SelItem > 0 then PostMessage(Form.Handle, WM_SYSCOMMAND, SelItem, 0);
end;

function TsSkinProvider.CursorToPoint(x, y: integer): TPoint;
begin
  Result := FormLeftTop;
  Result.x := x - Result.x;
  Result.y := y - Result.y;
end;

function TsSkinProvider.MenuPresent: boolean;
var
 i, VisibleItems : integer;
begin
  Result := False;
  if (Form.BorderStyle = bsDialog) or (Form.FormStyle = fsMDIChild) then begin
    Exit
  end
  else begin
    if Form.Menu <> nil then begin
      VisibleItems := 0;
      for i := 0 to Form.Menu.Items.Count - 1 do begin
        if Form.Menu.Items[i].Visible then begin
          inc(VisibleItems);
          Break;
        end;
      end;
      if (Form.FormStyle = fsMDIForm) and Assigned(Form.ActiveMDIChild) and Assigned(Form.ActiveMDIChild.Menu) then begin
        for i := 0 to Form.ActiveMDIChild.Menu.Items.Count - 1 do begin
          if Form.ActiveMDIChild.Menu.Items[i].Visible then begin
            inc(VisibleItems);
            Break;
          end;
        end;
      end;
      Result := VisibleItems > 0;
    end;
  end;
end;

function TsSkinProvider.OffsetX: integer;
var
  i : integer;
begin
  if Assigned(ListSW) and Assigned(ListSW.sBarVert) and ListSW.sBarVert.fScrollVisible then i := GetScrollMetric(ListSW.sBarVert, SM_CXVERTSB) else i := 0;
  Result := (GetWindowWidth(Form.Handle) - GetClientWidth(Form.Handle) - i) div 2
end;

function TsSkinProvider.OffsetY: integer;
var
  i : integer;
begin
  if Assigned(ListSW) and Assigned(ListSW.sBarHorz) and ListSW.sBarHorz.fScrollVisible then i := GetScrollMetric(ListSW.sBarHorz, SM_CYHORZSB) else i := 0;
  Result := GetWindowHeight(Form.Handle) - GetClientHeight(Form.Handle) - BorderWidth * integer(CaptionHeight <> 0) - i;
end;

function TsSkinProvider.GetLinesCount: integer;
var
  VisIndex, Index, x, w, GlobalResult : integer;
  CurrentItem : TMenuItem;
  ItemProcessed : integer;
  ChangedIndex : integer;
  R : TRect;
  function ProcessMerged(CurrentIndex : integer) : boolean;
  var
    i, j, VisJ, w : integer;
    LocalItem : TMenuItem;
  begin
    Result := False;
    if Assigned(Form.ActiveMDIChild) and Assigned(Form.ActiveMDIChild.Menu) then begin
      for i := ItemProcessed to Form.ActiveMDIChild.Menu.Items.Count - 1 do begin
        LocalItem := Form.ActiveMDIChild.Menu.Items[i];

        // If MDI form and included other
        if (LocalItem.GroupIndex > ChangedIndex) and (LocalItem.GroupIndex <= CurrentIndex) then begin

          if not Assigned(LocalItem.OnMeasureItem) or not Assigned(LocalItem.OnAdvancedDrawItem) or not LocalItem.Visible then Continue;

          Result := (LocalItem.GroupIndex >= CurrentIndex);
          ChangedIndex := LocalItem.GroupIndex;

          j := i;
          VisJ := j;
          while (j <= Form.ActiveMDIChild.Menu.Items.Count - 1) do begin
            LocalItem := Form.ActiveMDIChild.Menu.Items[j];
            if (LocalItem.GroupIndex > ChangedIndex) and (Index <= Form.Menu.Items.Count - 1) then Exit;
            GetMenuItemRect(Form.ActiveMDIChild.Handle, Form.ActiveMDIChild.Menu.Handle, VisJ, R);
            w := WidthOf(R);
            ChangedIndex := LocalItem.GroupIndex;

            if x + w > Form.Width - 2 * SysBorderWidth - 2 * TForm(Form).BorderWidth then begin
              x := SysBorderWidth;
              inc(GlobalResult);
            end;

            inc(x, w);
            ItemProcessed := i + 1;
            inc(j);
            inc(VisJ);
          end;
        end;
      end;
    end;
  end;
begin
  if FLinesCount <> -1 then Result := FLinesCount else begin
    GlobalResult := 1;
    x := SysBorderWidth;
    ItemProcessed := 0;
    if Form.Menu <> nil then begin
      ChangedIndex := -1;
      Index := 0;
      VisIndex := 0;
      while Index <= Form.Menu.Items.Count - 1 do begin
        if (x = SysBorderWidth) and (MDISkinProvider = Self) and ChildIconPresent then begin
          GetMenuItemRect(Form.Handle, Form.Menu.Handle, 0, R);
          inc(x, WidthOf(R));
          inc(VisIndex);
          Continue;
        end;
        CurrentItem := Form.Menu.Items[Index];
        if (CurrentItem.GroupIndex = ChangedIndex) or ProcessMerged(CurrentItem.GroupIndex) then begin
          inc(Index);
          continue;
        end
        else begin
          if not Assigned(CurrentItem.OnMeasureItem) or not Assigned(CurrentItem.OnAdvancedDrawItem) or
               not CurrentItem.Visible then begin
            inc(Index);
            Continue;
          end;

          GetMenuItemRect(Form.Handle, Form.Menu.Handle, VisIndex, R);
          w := WidthOf(R);

          if x + w > Form.Width - 2 * SysBorderWidth - 2 * TForm(Form).BorderWidth then begin
            inc(GlobalResult);
            x := SysBorderWidth;
          end;

          inc(x, w);
          inc(Index);
          inc(VisIndex);
        end;
      end;
      ProcessMerged(99999);
    end;
    Result := GlobalResult;
    FLinesCount := Result;
  end;
end;

procedure TsSkinProvider.Loaded;
var
  mi, i : integer;
begin
  inherited;
  if Assigned(FCommonData.SkinManager) and not (csDesigning in ComponentState) and FCommonData.Skinned(True) then begin
    if Adapter = nil then AdapterCreate;
    PrepareForm;
    if Assigned(SystemMenu) then SystemMenu.UpdateItems;

    mi := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinManager.ConstData.IndexGLobalInfo, s_GlobalInfo, s_TitleButtonMask);
    if mi < 0 then mi := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinIndex, FCommonData.SkinSection, s_TitleButtonMask); // For compatibility
    for i := 0 to TitleButtons.Count - 1 do
      if TitleButtons.Items[i].UseSkinData
        then TitleButtons.Items[i].BtnData.ImageIndex := mi
        else TitleButtons.Items[i].BtnData.ImageIndex := -1;

    if Assigned(MDIForm) then begin
//      Application.ProcessMessages; // For Scrollbars updating
      TsMDIForm(MDIForm).ConnectToClient;
    end;
  end;
end;

procedure TsSkinProvider.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent is TsSkinManager) then case Operation of
    opInsert : if not Assigned(SkinData.SkinManager) then SkinData.SkinManager := TsSkinManager(AComponent);
    opRemove : if AComponent = SkinData.SkinManager then SkinData.SkinManager := nil;
  end
  else if (AComponent is TWinControl) then case Operation of
    opInsert : if Assigned(Adapter) then begin
      if Adapter.IsControlSupported(TWincontrol(AComponent)) then Adapter.AddNewItem(TWincontrol(AComponent));
    end;
  end;
end;

function TsSkinProvider.FormChanged: boolean;
begin
  Result := (FCommonData.FCacheBmp = nil) or (CaptionWidth <> FCommonData.FCacheBmp.Width) or (Form.Height <> FCommonData.FCacheBmp.Height)
end;

procedure TsSkinProvider.RepaintMenuItem(mi: TMenuItem; R: TRect; State : TOwnerDrawState);
var
  DC, SavedDC : hdc;
begin
  SavedDC := 0;
  if MenuPresent and (Form.Menu.FindItem(mi.Handle, fkHandle) <> nil) then begin
    mi.OnAdvancedDrawItem(mi, FCommonData.FCacheBmp.Canvas, R, State);
    DC := GetWindowDC(Form.Handle);
    try
      SavedDC := SaveDC(DC);
      BitBlt(DC, R.Left, R.Top, WidthOf(R), HeightOf(R), FCommonData.FCacheBmp.Canvas.Handle, R.Left, R.Top, SRCCOPY);
    finally
      RestoreDC(DC, SavedDC);
      ReleaseDC(Form.Handle, DC);
    end;
  end;
end;

function TsSkinProvider.SmallButtonWidth: integer;
begin
  if FCommonData.SkinManager.IsValidImgIndex(MDIClose.ImageIndex) then begin
    if Assigned(FCommonData.SkinManager.ma[MDIClose.ImageIndex].Bmp)
      then Result := FCommonData.SkinManager.ma[MDIClose.ImageIndex].Bmp.Width div 3
      else Result := WidthOf(FCommonData.SkinManager.ma[MDIClose.ImageIndex].R) div FCommonData.SkinManager.ma[MDIClose.ImageIndex].ImageCount;
  end
  else Result := 16;
end;

function TsSkinProvider.SmallButtonHeight: integer;
begin
  if FCommonData.SkinManager.IsValidImgIndex(MDIClose.ImageIndex) then begin
    if Assigned(FCommonData.SkinManager.ma[MDIClose.ImageIndex].Bmp)
      then Result := FCommonData.SkinManager.ma[MDIClose.ImageIndex].Bmp.Height div 2
      else Result := HeightOf(FCommonData.SkinManager.ma[MDIClose.ImageIndex].R) div (FCommonData.SkinManager.ma[MDIClose.ImageIndex].MaskType + 1);
  end
  else Result := 16;
end;

function TsSkinProvider.HeaderHeight: integer;
begin
  if CaptionHeight = 0
    then Result := GetWindowHeight(Form.Handle) - GetClientHeight(Form.Handle)
    else Result := GetWindowHeight(Form.Handle) - GetClientHeight(Form.Handle) - SysBorderHeight - Form.BorderWidth {v5.32};
  if Result < 0 then Result := 0;
  if IsIconic(Form.Handle) then inc(Result, SysBorderHeight);
  if SkinData.Skinned and Assigned(ListSW) and Assigned(ListSW.sBarHorz) and ListSW.sBarHorz.fScrollVisible then begin
    dec(Result, GetScrollMetric(ListSW.sBarHorz, SM_CYHORZSB));
  end;
end;

function TsSkinProvider.MDIButtonsNeeded: boolean;
begin
  Result := (ChildProvider <> nil) and (Form.FormStyle = fsMDIForm) and Assigned(Form.ActiveMDIChild) and
              (Form.ActiveMDIChild.WindowState = wsMaximized) and (Form.Menu <> nil) and (biSystemMenu in Form.ActiveMDIChild.BorderIcons);
end;

function TsSkinProvider.MenuHeight: integer;
begin
  if Form.Menu <> nil then Result := GetSystemMetrics(SM_CYMENU) - 1 else Result := 0;
end;

function TsSkinProvider.AboveBorder(Message: TWMNCHitTest): boolean;
var
  p : TPoint;
begin
  p := CursorToPoint(Message.XPos, Message.YPos);
  Result := not PtInRect(Rect(2, 2, Form.Width - 4, Form.Height - 4), p);
  if Result then SetHotHT(0);
end;

type
  TAccessMenuItem = class(TMenuItem);

function TsSkinProvider.UpdateMenu : boolean;
begin
  Result := False;
  if not fGlobalFlag then begin
    fGlobalFlag := True;
    if (Form.Menu <> nil) and (Form.Menu.Items.Count > 0) and (Form.Menu.Items[0] <> nil) then begin
      TAccessMenuItem(Form.Menu.Items[0]).MenuChanged(True);
      Result := True;
    end;
    fGlobalFlag := False;
  end;
end;

function TsSkinProvider.IconVisible: boolean;
begin
  Result := ((Form.BorderStyle = bsSizeable) or (Form.BorderStyle = bsSingle)) and FShowAppIcon;
end;

procedure TsSkinProvider.MakeTitleBG;
begin
  if TitleBG <> nil then FreeAndNil(TitleBG);
  TitleBG := TBitmap.Create;
  TitleBG.Width := FCommonData.FCacheBmp.Width;
  TitleBG.Height := CaptionHeight + SysBorderHeight;
  TitleBG.PixelFormat := pf24bit;
  BitBlt(TitleBG.Canvas.Handle, 0, 0, TitleBG.Width, TitleBG.Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TsSkinProvider.PrepareCaption(CI: TCacheInfo; R : TRect);
var
  i : integer;
  s : acString;
  sRect : TRect;
  ts : TSize;
begin
  i := FCommonData.SkinManager.GetSkinIndex(TitleSkinSection); // Paint title
  PaintItem(i, TitleSkinSection, ci, True, integer(FormActive), Rect(0, 0, R.Right, R.Bottom), Point(0, 0), FCommonData.FCacheBmp, FCommonData.SkinManager);

  DrawAppIcon(Self);
  if (SysButtonsCount > 0) then begin
    // Paint title toolbar if exists
    i := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinIndex, FCommonData.SkinSection, s_NormalTitleBar);
    if FCommonData.SkinManager.IsValidImgIndex(i) then begin
      DrawSkinRect(FCommonData.FCacheBmp,
             Rect(R.Right - BarWidth(i), 0, R.Right, R.Bottom),
             True, EmptyCI, FCommonData.SkinManager.ma[i], integer(FormActive), True);
    end;
  end;
  SaveBGForBtns; 
  // Paint buttons
  PaintBorderIcons;

  // Out title text
  FCommonData.FCacheBmp.Canvas.Font.Assign(Form.Font);
  FCommonData.FCacheBmp.Canvas.Font.Height := GetCaptionFontSize;
  FCommonData.FCacheBmp.Canvas.Font.Style := FCommonData.FCacheBmp.Canvas.Font.Style + [fsBold];

  sRect := Rect(
             SysBorderWidth + integer(IconVisible) * WidthOf(IconRect) + 4 + SkinData.SkinManager.SkinData.BILeftMargin,
             R.Top + SysBorderHeight,
             R.Right - TitleBtnsWidth - 6,
             CaptionHeight
           );
  if not IsRectEmpty(sRect) then begin
{$IFDEF TNTUNICODE}
    s := WideString(GetWndText(Form.Handle));
    GetTextExtentPoint32W(FCommonData.FCacheBmp.Canvas.Handle, PWideChar(s), Length(s), ts);
    sRect.Top := sRect.Top + (HeightOf(sRect) - ts.cy) div 2;
    sRect.Bottom := sRect.Top + ts.cy;
    WriteTextExW(FCommonData.FCacheBmp.Canvas, PWideChar(s), Form.Enabled, sRect,
      GetStringFlags(Form, FCaptionAlignment) or DT_END_ELLIPSIS or DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX, FCommonData.SkinManager.GetSkinIndex(TitleSkinSection),
      FormActive, FCommonData.SkinManager)
{$ELSE}
    s := Form.Caption;
    GetTextExtentPoint32(FCommonData.FCacheBmp.Canvas.Handle, PacChar(s), Length(s), ts);
    sRect.Top := sRect.Top + (HeightOf(sRect) - ts.cy) div 2;
    sRect.Bottom := sRect.Top + ts.cy;
    acWriteTextEx(FCommonData.FCacheBmp.Canvas, PChar(s), Form.Enabled, sRect,
      GetStringFlags(Form, FCaptionAlignment) or DT_END_ELLIPSIS or DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX, FCommonData.SkinManager.GetSkinIndex(TitleSkinSection),
      FormActive, FCommonData.SkinManager);
{$ENDIF}
  end;
end;

procedure TsSkinProvider.SetCaptionAlignment(const Value: TAlignment);
begin
  if FCaptionAlignment <> Value then begin
    FCaptionAlignment := Value;
    FCommonData.BGChanged := True;
    if Form.Visible then SendMessage(Form.Handle, WM_NCPAINT, 0, 0);
  end;
end;

procedure TsSkinProvider.SetShowAppIcon(const Value: boolean);
begin
  if FShowAppIcon <> Value then begin
    FShowAppIcon := Value;
    FCommonData.BGChanged := True;
    if Form.Visible then SendMessage(Form.Handle, WM_NCPAINT, 0, 0);
  end;
end;

procedure TsSkinProvider.SetTitleButtons(const Value: TsTitleButtons);
begin
  FTitleButtons.Assign(Value);
end;

function TsSkinProvider.RBGripPoint(ImgIndex : integer): TPoint;
begin
// v6.03 if FCommonData.SkinManager.ma[ImgIndex].Bmp = nil then begin
  Result := Point(FCommonData.FCacheBmp.Width - WidthOf(FCommonData.SkinManager.ma[ImgIndex].R) div FCommonData.SkinManager.ma[ImgIndex].ImageCount - SysBorderWidth,
                  Form.Height - HeightOf(FCommonData.SkinManager.ma[ImgIndex].R) div (1 + FCommonData.SkinManager.ma[ImgIndex].MaskType) - SysBorderHeight);
end;

procedure TsSkinProvider.InitMenuItems(A: boolean);
var
  i : integer;
  procedure ProcessComponent(c: TComponent);
  var
    i: integer;
  begin
    if (c <> nil) then begin
      if (c is TMainMenu) then begin
        FCommonData.SkinManager.SkinableMenus.InitMenuLine(TMainMenu(c), A and FDrawNonCLientArea);
        for i := 0 to TMainMenu(c).Items.Count - 1 do
          FCommonData.SkinManager.SkinableMenus.HookItem(TMainMenu(c).Items[i], A and FCommonData.SkinManager.SkinnedPopups);
      end
      else if (c is TPopupMenu) then begin
        FCommonData.SkinManager.SkinableMenus.HookPopupMenu(TPopupMenu(c), A and FCommonData.SkinManager.SkinnedPopups);
      end
      else if (c is TMenuItem) then if not (TMenuItem(c).GetParentMenu is TMainMenu) then begin
        FCommonData.SkinManager.SkinableMenus.HookItem(TMenuItem(c), A and FCommonData.SkinManager.SkinnedPopups);
      end
      else
        for i := 0 to c.ComponentCount - 1 do ProcessComponent(c.Components[i]);
    end;
  end;
begin
  if (csDesigning in Form.ComponentState) or (FCommonData.SkinManager.SkinableMenus = nil) or not FCommonData.SkinManager.IsDefault then Exit;
//  FCommonData.SkinManager.SkinableMenus.InitItems(A);
  for i := 0 to Form.ComponentCount - 1 do ProcessComponent(Form.Components[i]);
end;

procedure TsSkinProvider.StartMove(X, Y: Integer);
begin
  if ResizeMode = rmBorder then begin
    //Common section
    bInProcess := TRUE;
    deskwnd    := GetDesktopWindow();
    formDC     := GetWindowDC(deskwnd);
    nDC        := SaveDC(formDC);
    ntop       := Form.Top;
    nleft      := Form.Left;
    SetROP2(formDC, R2_NOT);

    if bMode then begin //Move section
      nX := X;
      nY := Y;
      DrawFormBorder(nleft, ntop);
    end
    else begin //Size section
      nMinHeight := Form.Constraints.MinHeight;
      nMinWidth  := Form.Constraints.MinWidth;
      nbottom    := Form.top + Form.height;
      nright     := Form.left + Form.width;
      DrawFormBorder(0, 0);
    end;
  end;
end;

procedure TsSkinProvider.StopMove(X, Y: Integer);
begin
  if ResizeMode = rmBorder then begin
    //Common section
    ReleaseCapture;
    bInProcess := FALSE;

    if bMode then begin //Move section
      DrawFormBorder(nleft, ntop);
      RestoreDC(formDC, nDC);
      ReleaseDC(deskwnd, formDC);
      MoveWindow(Form.handle, nleft, ntop, Form.width, Form.height, TRUE)
    end
    else begin //Size section
      DrawFormBorder(0,0);
      RestoreDC(formDC, nDC);
      ReleaseDC(deskwnd, formDC);
      if not bCapture then MoveWindow(Form.handle, nleft, ntop, nright - nleft, nbottom - ntop, TRUE);
      bCapture := FALSE;
    end;
  end;
end;

procedure TsSkinProvider.DrawFormBorder(X, Y: Integer);
var
  pts : array [1..5] of TPoint;
  incX, incY : integer;
begin
  if ResizeMode = rmBorder then begin
    if Form.FormStyle = fsMDIChild then with TsSkinProvider(MDISkinProvider) do begin
      incX := Form.Left + SysBorderWidth + Form.BorderWidth + 1;
      incY := Form.Top + SysBorderHeight * 2 + CaptionHeight + LinesCount * MenuHeight * integer(MenuPresent) + Form.BorderWidth - 2;
      X := X + incX;
      Y := Y + incY;
    end
    else begin
      incX := 0;
      incY := 0;
    end;
    if bMode then begin //Move section
      pts[1] := point(X, Y);
      pts[2] := point(X, Y + Form.Height);
      pts[3] := point(X + Form.Width, Y + Form.Height);
      pts[4] := point(X + Form.Width, Y);
      pts[5] := point(X, Y);
      PolyLine(formDC, pts, 5);
    end
    else begin //Size section
      pts[1].X := nleft + incX;
      pts[1].Y := ntop + incY;
      pts[2].X := nleft + incX;
      pts[2].Y := nbottom + incY;
      pts[3].X := nright + incX;
      pts[3].Y := nbottom + incY;
      pts[4].X := nright + incX;
      pts[4].Y := ntop + incY;
      pts[5].X := nleft + incX;
      pts[5].Y := ntop + incY;
      PolyLine(formDC, pts, 5);
    end;
  end;
end;

procedure TsSkinProvider.SetUseGlobalColor(const Value: boolean);
begin
  if FUseGlobalColor <> Value then begin
    FUseGlobalColor := Value;
    if FCommonData.Skinned and Assigned(Form) and Value and not SkinData.CustomColor then Form.Color := FCommonData.SkinManager.GetGlobalColor else Form.Color := clBtnFace;
  end;
end;

procedure TsSkinProvider.RepaintMenu;
begin
  SkinData.BGChanged := True;
  MenuChanged := True;
  FLinesCount := -1;
  SendMessage(Form.Handle, WM_NCPAINT, 0, 0);
end;

function TsSkinProvider.CaptionWidth: integer;
var
  R : TRect;
begin
  if IsIconic(Form.Handle) then begin
    GetWindowRect(Form.Handle, R);
    Result := WidthOf(R)
  end
  else Result := Form.Width;
end;

function TsSkinProvider.SysBorderHeight: integer;
begin
  Result := 0;
  if not HaveBorder(Self) then Exit;
  if Form.FormStyle = fsMDIChild then begin
    if Form.BorderStyle in [bsToolWindow, bsSingle]
      then Result := GetSystemMetrics(SM_CYFIXEDFRAME)
      else Result := GetSystemMetrics(SM_CYSIZEFRAME);
  end
  else begin
    if Form.BorderStyle in [bsToolWindow, bsSingle, bsDialog]
      then Result := GetSystemMetrics(SM_CYFIXEDFRAME)
      else Result := GetSystemMetrics(SM_CYSIZEFRAME);
  end;
end;

function TsSkinProvider.SysBorderWidth: integer;
begin
  Result := 0;
  if not HaveBorder(Self) then Exit;
  if Form.FormStyle = fsMDIChild then begin
    if Form.BorderStyle in [bsToolWindow, bsSingle]
      then Result := GetSystemMetrics(SM_CXFIXEDFRAME)
      else Result := GetSystemMetrics(SM_CXSIZEFRAME);
  end
  else begin
    if Form.BorderStyle in [bsToolWindow, bsSingle, bsDialog]
      then Result := GetSystemMetrics(SM_CXFIXEDFRAME)
      else Result := GetSystemMetrics(SM_CXSIZEFRAME);
  end;
end;

procedure TsSkinProvider.UpdateIconsIndexes;
begin
  if FCommonData.SkinManager.IsValidSkinIndex(FCommonData.SkinManager.ConstData.IndexGlobalInfo) then begin
//    ButtonClose.ImageIndex := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinManager.ConstData.IndexGlobalInfo, s_GlobalInfo, s_BorderIconClose);
//    if ButtonClose.ImageIndex > -1 then
    with FCommonData.SkinManager do begin // For compatibility with skins with version < 4.33
      if BigButtons(Self) then begin
        ButtonMin.ImageIndex    := GetMaskIndex(ConstData.IndexGlobalInfo, s_GlobalInfo, s_BorderIconMinimize);
        ButtonMax.ImageIndex    := GetMaskIndex(ConstData.IndexGlobalInfo, s_GlobalInfo, s_BorderIconMaximize);
        ButtonClose.ImageIndex  := GetMaskIndex(ConstData.IndexGlobalInfo, s_GlobalInfo, s_BorderIconClose);
        ButtonHelp.ImageIndex   := GetMaskIndex(ConstData.IndexGlobalInfo, s_GlobalInfo, s_BorderIconHelp);
        ButtonMin.HaveAlignment   := True;
        ButtonMax.HaveAlignment   := True;
        ButtonClose.HaveAlignment := True;
        ButtonHelp.HaveAlignment  := True;
      end
      else begin
        ButtonClose.ImageIndex  := GetMaskIndex(ConstData.IndexGlobalInfo, s_GlobalInfo, s_SmallIconClose);
        ButtonMin.ImageIndex    := GetMaskIndex(ConstData.IndexGlobalInfo, s_GlobalInfo, s_SmallIconMinimize);
        ButtonMax.ImageIndex    := GetMaskIndex(ConstData.IndexGlobalInfo, s_GlobalInfo, s_SmallIconMaximize);
        ButtonHelp.ImageIndex   := GetMaskIndex(ConstData.IndexGlobalInfo, s_GlobalInfo, s_SmallIconHelp);
        if ButtonHelp.ImageIndex < 0 then ButtonHelp.ImageIndex := GetMaskIndex(ConstData.IndexGlobalInfo, s_GlobalInfo, s_BorderIconHelp);
        ButtonMin.HaveAlignment   := False;
        ButtonMax.HaveAlignment   := False;
        ButtonClose.HaveAlignment := False;
        ButtonHelp.HaveAlignment  := False;
      end;
      if ButtonClose.ImageIndex < 0 then begin
        ButtonClose.ImageIndex  := GetMaskIndex(ConstData.IndexGlobalInfo, s_GlobalInfo, s_BorderIconClose);
        ButtonMin.ImageIndex    := GetMaskIndex(ConstData.IndexGlobalInfo, s_GlobalInfo, s_BorderIconMinimize);
        ButtonMax.ImageIndex    := GetMaskIndex(ConstData.IndexGlobalInfo, s_GlobalInfo, s_BorderIconMaximize);
        ButtonHelp.ImageIndex   := GetMaskIndex(ConstData.IndexGlobalInfo, s_GlobalInfo, s_BorderIconHelp);
        ButtonMin.HaveAlignment   := True;
        ButtonMax.HaveAlignment   := True;
        ButtonClose.HaveAlignment := True;
        ButtonHelp.HaveAlignment  := True;

        if (Form.FormStyle = fsMDIForm) and not (csDesigning in ComponentState) then begin
          MDIMin.ImageIndex       := GetMaskIndex(ConstData.IndexGlobalInfo, s_GlobalInfo, s_BorderIconMinimize);
          MDIMax.ImageIndex       := GetMaskIndex(ConstData.IndexGlobalInfo, s_GlobalInfo, s_BorderIconMaximize);
          MDIClose.ImageIndex     := GetMaskIndex(ConstData.IndexGlobalInfo, s_GlobalInfo, s_BorderIconClose);
        end;
        MDIMin.HaveAlignment   := True;
        MDIMax.HaveAlignment   := True;
        MDIClose.HaveAlignment := True;
      end
      else begin
        if (Form.FormStyle = fsMDIForm) and not (csDesigning in ComponentState) then begin
          MDIMin.ImageIndex       := GetMaskIndex(ConstData.IndexGlobalInfo, s_GlobalInfo, s_SmallIconMinimize);
          MDIMax.ImageIndex       := GetMaskIndex(ConstData.IndexGlobalInfo, s_GlobalInfo, s_SmallIconMaximize);
          MDIClose.ImageIndex     := GetMaskIndex(ConstData.IndexGlobalInfo, s_GlobalInfo, s_SmallIconClose);
          MDIMin.HaveAlignment   := False;
          MDIMax.HaveAlignment   := False;
          MDIClose.HaveAlignment := False;
        end;
        if MDIMin.ImageIndex < 0 then begin // Leaved for compatibility, should be removed later
          MDIMin.ImageIndex       := ButtonMin.ImageIndex;
          MDIMax.ImageIndex       := ButtonMax.ImageIndex;
          MDIClose.ImageIndex     := ButtonClose.ImageIndex;
          MDIMin.HaveAlignment   := True;
          MDIMax.HaveAlignment   := True;
          MDIClose.HaveAlignment := True;
        end;
      end;
{    end
    else with FCommonData.SkinManager do begin
      ButtonMin.ImageIndex    := GetMaskIndex(FCommonData.SkinIndex, s_Form, s_BorderIconMinimize);
      ButtonMax.ImageIndex    := GetMaskIndex(FCommonData.SkinIndex, s_Form, s_BorderIconMaximize);
      ButtonClose.ImageIndex  := GetMaskIndex(FCommonData.SkinIndex, s_Form, s_BorderIconClose);
      ButtonHelp.ImageIndex   := GetMaskIndex(FCommonData.SkinIndex, s_Form, s_BorderIconHelp);
      if (Form.FormStyle = fsMDIForm) and not (csDesigning in ComponentState) then begin
        MDIMin.ImageIndex       := GetMaskIndex(FCommonData.SkinIndex, s_Form, s_BorderIconMinimize);
        MDIMax.ImageIndex       := GetMaskIndex(FCommonData.SkinIndex, s_Form, s_BorderIconMaximize);
        MDIClose.ImageIndex     := GetMaskIndex(FCommonData.SkinIndex, s_Form, s_BorderIconClose);
      end;}
    end;

    UserBtnIndex := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinManager.ConstData.IndexGlobalInfo, s_GlobalInfo, s_TitleButtonMask);
  end;
end;

procedure TsSkinProvider.SetTitleSkin(const Value: TsSkinSection);
begin
  if FTitleSkin <> Value then begin
    FTitleSkin := Value;
    FCommonData.BGChanged := True;
    if Form.Visible then SendMessage(Form.Handle, WM_NCPAINT, 0, 0);
  end;
end;

function TsSkinProvider.TitleSkinSection: string;
begin
  if FTitleSkin = '' then Result := s_FormTitle else Result := FTitleSkin
end;

procedure TsSkinProvider.SetMenuLineSkin(const Value: TsSkinSection);
begin
  if (FMenuLineSkin <> Value) then begin
    FMenuLineSkin := Value;
    if (csDesigning in ComponentState) then SkinData.Invalidate;
  end;
end;

procedure TsSkinProvider.PrepareForm;
begin
  if (FCommonData.SkinManager = nil) or (Form = nil) or not Form.HandleAllocated then Exit;
  FCommonData.Loaded;
  CheckSysMenu(FCommonData.SkinManager.SkinData.Active);
//  if not FDrawNonClientArea then SetWindowLong(Form.Handle, GWL_STYLE, GetWindowLong(Form.Handle, GWL_STYLE) or WS_SYSMENU);
  if SystemMenu = nil then begin
    SystemMenu := TsSystemMenu.Create(Self);
    SystemMenu.FForm := Form;
    SystemMenu.UpdateItems;
  end;

  if ClearButtons and FCommonData.SkinManager.SkinData.Active then begin
    ClearButtons := False;
    if (Form.FormStyle = fsMDIForm) and not (csDesigning in ComponentState) then HookMDI;
  end;
  FCommonData.UpdateIndexes;
  UpdateIconsIndexes;

  RegionChanged := True;
  CaptChanged := True;
  CaptRgnChanged :=True;

  // If form is MDIChild and menus are merged then
  if (Form.FormStyle = fsMDIChild) then begin
    if Assigned(MDISkinProvider) and
         not (csDestroying in TsSkinProvider(MDISkinProvider).ComponentState) and
           not (csDestroying in TsSkinProvider(MDISkinProvider).Form.ComponentState) and SkinData.Skinned
             then begin
      TsSkinProvider(MDISkinProvider).FCommonData.BGChanged := True;
      TsSkinProvider(MDISkinProvider).FLinesCount := -1;
    end;
  end;
  if (Form <> nil) and not (csCreating in Form.ControlState) and not (csReadingState in Form.ControlState) and
       not (csLoading in componentState) and (SkinData.SkinManager <> nil) and UseGlobalColor and not SkinData.CustomColor
    then Form.Color := SkinData.SkinManager.GetGlobalColor;
  InitMenuItems(SkinData.Skinned);
  if not MenusInitialized then begin
    if UpdateMenu then MenusInitialized := True;
  end;
  if SystemMenu <> nil then SystemMenu.UpdateGlyphs;
end;

procedure TsSkinProvider.HookMDI(Active: boolean);
begin
  if Active then begin
    if not Assigned(MDIForm) then begin
      MDISkinProvider := Self;
      TsMDIForm(MDIForm) := TsMDIForm.Create(Self);
      if MDIForm <> nil then TsMDIForm(MDIForm).ConnectToClient;
    end;
  end
  else begin
    if Assigned(MDIForm) then FreeAndNil(MDIForm);
  end;
end;

procedure TsSkinProvider.DsgnWndProc(var Message: TMessage);
begin
  if ChangeFormsInDesign and (csDesigning in ComponentState) and
       Assigned(SkinData) and
         Assigned(SkinData.SkinManager) and
           Assigned(Form) and
             UseGlobalColor and
               not SkinData.CustomColor and
                 (Message.WParamHi in [AC_SETNEWSKIN, AC_REFRESH, AC_REMOVESKIN]) and
                   (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_SETNEWSKIN, AC_REFRESH : Form.Color := SkinData.SkinManager.GetGlobalColor;
    AC_REMOVESKIN : Form.Color := clBtnFace;
  end;
end;

function TsSkinProvider.TitleBtnsWidth: integer;
var
  i : integer;
begin
  Result := FCommonData.SkinManager.SkinData.BIRightMargin;
  if SystemMenu.VisibleClose and Assigned(SystemMenu) then begin
    inc(Result, SysButtonWidth(ButtonClose) + integer(BigButtons(Self)) * FCommonData.SkinManager.SkinData.BISpacing);
    if SystemMenu.VisibleMax then inc(Result, SysButtonWidth(ButtonMax) + integer(BigButtons(Self)) * FCommonData.SkinManager.SkinData.BISpacing);
    if SystemMenu.VisibleMin then inc(Result, SysButtonWidth(ButtonMin) + integer(BigButtons(Self)) * FCommonData.SkinManager.SkinData.BISpacing);
    if (biHelp in Form.BorderIcons) then inc(Result, SysButtonWidth(ButtonHelp) + integer(BigButtons(Self)) * FCommonData.SkinManager.SkinData.BISpacing);
  end;

  if TitleButtons.Count > 0 then inc(Result, UserButtonsOffset);
  for i := 0 to TitleButtons.Count - 1 do begin
    inc(Result, UserButtonWidth(TitleButtons[i]) + integer(BigButtons(Self)) * FCommonData.SkinManager.SkinData.BISpacing);
  end;
end;

function TsSkinProvider.UserButtonWidth(Btn: TsTitleButton): integer;
begin
  if Assigned(Btn.Glyph) then Result := Btn.Glyph.Width + 2 else Result := 0;
  if FCommonData.SkinManager.IsValidImgIndex(UserBtnIndex)
    then Result := max(Result, WidthOf(FCommonData.SkinManager.ma[UserBtnIndex].R) div FCommonData.SkinManager.ma[UserBtnIndex].ImageCount)
    else Result := max(Result, 21);
end;

procedure TsSkinProvider.AdapterCreate;
begin
  if not (csDesigning in ComponentState) and FCommonData.Skinned then begin
    Adapter := TacCtrlAdapter.Create(Self);
    Adapter.AddAllItems;
  end;
end;

procedure TsSkinProvider.AdapterRemove;
begin
  if not (csDesigning in ComponentState) then begin
    SendToAdapter(MakeMessage(SM_ALPHACMD, MakeWParam(0, AC_REMOVESKIN), LongWord(SkinData.SkinManager), 0));
    FreeAndNil(Adapter);
  end;
end;

procedure TsSkinProvider.SendToAdapter(Message: TMessage);
begin
  try
    if not (csDesigning in ComponentState) and Assigned(Adapter) then Adapter.WndProc(Message)
  except
  end
end;

procedure TsSkinProvider.MdiIcoFormPaint(Sender: TObject);
begin
  with TForm(Sender) do BitBlt(Canvas.Handle, 0, 0,
      60, GetSystemMetrics(SM_CYMENU) - 1 + 4, SkinData.FCacheBmp.Canvas.Handle,
      Form.Width - 60, CaptionHeight, SRCCOPY);
end;

procedure TsSkinProvider.CaptFormPaint(Sender: TObject);
begin
  if (CaptForm <> nil) and not (csDestroying in CaptForm.ComponentState)
    then BitBlt(CaptForm.Canvas.Handle, 0, 0, CaptForm.Width, CaptForm.Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TsSkinProvider.NewCaptFormProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_ERASEBKGND : Exit;
  end;
  OldCaptFormProc(Message);
end;

procedure TsSkinProvider.SaveBGForBtns(Full : boolean = False);
begin
  TempBmp.Width := TitleBtnsWidth + SysBorderWidth + 10;
  TempBmp.Height := HeaderHeight;
  BitBlt(TempBmp.Canvas.Handle, 0, 0, TempBmp.Width, iffi(Full, TempBmp.Height, CaptionHeight + SysBorderHeight),
         FCommonData.FCacheBmp.Canvas.Handle, CaptionWidth - TempBmp.Width - 1, 0, SRCCOPY);
end;

procedure TsSkinProvider.RestoreBtnsBG;
begin
  if Assigned(TempBmp) then BitBlt(FCommonData.FCacheBmp.Canvas.Handle, CaptionWidth - TempBmp.Width - 1, 0, TempBmp.Width, TempBmp.Height, TempBmp.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TsSkinProvider.AC_WMEraseBkGnd(const aDC: hdc);
var
  DC, SavedDC : hdc;
begin
  if aDC = 0 then DC := GetDC(Form.Handle) else DC := aDC;
  try
    FCommonData.FUpdating := False;
    if not fAnimating and not (csDestroying in Form.ComponentState) and not ((Form.FormStyle = fsMDIChild) and (MDISkinProvider <> nil) and not MDICreating and
         Assigned(TsSkinProvider(MDISkinProvider).Form.ActiveMDIChild) and (TsSkinProvider(MDISkinProvider).Form.ActiveMDIChild.WindowState = wsMaximized)
           and (Form <> TsSkinProvider(MDISkinProvider).Form.ActiveMDIChild)) then begin
      SavedDC := SaveDC(DC);
      ExcludeControls(DC, Form, actGraphic, 0, 0);
      PaintForm(DC);

      if IsGripVisible(Self) then begin
        MoveWindowOrg(DC, -OffsetX, -OffsetY);
        PaintGrip(DC, Self);
      end;
      RestoreDC(DC, SavedDC);
      PaintControls(DC, Form, True, Point(0, 0));
    end;
  finally
    if aDC = 0 then ReleaseDC(Form.Handle, DC);
  end;
end;

procedure TsSkinProvider.AC_WMNCPaint;
var
  DC, SavedDC : hdc;
  i : integer;
begin
  if MDICreating or not FDrawNonClientArea then Exit; // If maximized mdi child was created
  if (ResizeMode = rmBorder) and AeroIsEnabled then ResizeMode := rmStandard;
  if (Form.FormStyle = fsMDIChild) then begin
    if (MDISkinProvider <> nil) and
          Assigned(TsSkinProvider(MDISkinProvider).Form.ActiveMDIChild) and
            (TsSkinProvider(MDISkinProvider).Form.ActiveMDIChild.WindowState = wsMaximized)
              and (Form <> TsSkinProvider(MDISkinProvider).Form.ActiveMDIChild) then begin
      Exit;
    end;
  end;
{  if not IsCached(FCommonData) and (FCommonData.FCacheBmp <> nil) then begin
    FCommonData.FCacheBmp.Height := Form.Height;
    FCommonData.BGChanged := True;
  end;}

  if not MenusInitialized then begin
    if UpdateMenu then MenusInitialized := True;
  end;

  if not RgnChanging {!!!and not UseAero} and RgnChanged and (HaveBorder(Self) or IsSizeBox(Form.Handle) or IsIconic(Form.Handle)) then begin
    FillArOR(Self);
    RgnChanged := False;
    if not RgnChanging then begin
      if (fsShowing in Form.FormState) and (Form.Position <> poDefault) and (Form.WindowState <> wsMaximized) then UpdateRgn(Self, False) else begin
        UpdateRgn(Self, True);
        Exit
      end;
    end;
  end;
  FCommonData.Updating := False; // v5.41 MenuChanged

  DC := GetWindowDC(Form.Handle);
  SavedDC := SaveDC(DC);

  try
    if not HaveBorder(Self) and IsSizeBox(Form.Handle) and not IsIconic(Form.Handle) then begin
      if FCommonData.BGChanged
        then PaintAll;

      i := Form.BorderWidth + 3;
      BitBlt(DC, 0, 0, Form.Width, i, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY); // Title and menu line update
      BitBlt(DC, 0, i, i, FCommonData.FCacheBmp.Height, FCommonData.FCacheBmp.Canvas.Handle, 0, i, SRCCOPY); // Left border update
      BitBlt(DC, i, Form.Height - i, Form.Width - i, i, FCommonData.FCacheBmp.Canvas.Handle, i, Form.Height - i, SRCCOPY); // Bottom border update
      BitBlt(DC, FCommonData.FCacheBmp.Width - i, i, i, FCommonData.FCacheBmp.Height, FCommonData.FCacheBmp.Canvas.Handle, FCommonData.FCacheBmp.Width - i, i, SRCCOPY); // Right border update
    end
    else PaintCaption(DC);

  finally
    RestoreDC(DC, SavedDC);
    ReleaseDC(Form.Handle, DC);
  end;

  RgnChanging := False;

//  if (Form.FormStyle = fsMDIForm) and Assigned(MDIForm) then TsMDIForm(MDIForm).ConnectToClient;
//  if not IsCached(FCommonData) and (FCommonData.FCacheBmp <> nil) then FCommonData.FCacheBmp.Height := min(1000 + FCommonData.FCacheBmp.Height, CaptionHeight + SysBorderHeight + LinesCount * MenuHeight + 64);
end;
{
procedure TsSkinProvider.AC_WMWindowPosChanged(const Msg: TWMWindowPosMsg);
var
  wp : TWindowPos;
begin
  wp := Msg.WindowPos^;
  if (wp.flags and SWP_NOREDRAW = SWP_NOREDRAW) or (wp.flags and SWP_FRAMECHANGED = SWP_FRAMECHANGED)
    then AeroInvalidate := False
    else AeroInvalidate := True;
end;
}
function TsSkinProvider.FormColor: TColor;
begin
  if FCOmmonData.Skinned and not SkinData.CustomColor then begin
    if FormActive
      then Result := FCommonData.SkinManager.gd[FCommonData.Skinindex].HotColor
      else Result := FCommonData.SkinManager.gd[FCommonData.Skinindex].Color
  end
  else Result := ColorToRGB(Form.Color);
end;

procedure TsSkinProvider.OurPaintHandler(const Msg: TWMPaint);
var
  SavedDC : hdc;
  PS : TPaintStruct;
begin
  if InAnimationProcess and (Msg.DC = 0) then Exit;
  if not InAnimationProcess then BeginPaint(Form.Handle, PS);
  SavedDC := SaveDC(Form.Canvas.Handle);
  try
    Form.Canvas.Lock;
    FCommonData.Updating := False;
    if not fAnimating and not (csDestroying in Form.ComponentState) and not ((Form.FormStyle = fsMDIChild) and (MDISkinProvider <> nil) and not MDICreating and
         Assigned(TsSkinProvider(MDISkinProvider).Form.ActiveMDIChild) and (TsSkinProvider(MDISkinProvider).Form.ActiveMDIChild.WindowState = wsMaximized)
           and (Form <> TsSkinProvider(MDISkinProvider).Form.ActiveMDIChild)) then begin
      PaintForm(Form.Canvas.Handle);
    end;
  finally
    Form.Canvas.UnLock;
    RestoreDC(Form.Canvas.Handle, SavedDC);
    Form.Canvas.Handle := 0;
    if not InAnimationProcess then EndPaint(Form.Handle, PS);
  end;
end;

procedure TsSkinProvider.CheckSysMenu(const Skinned: boolean);
begin
  if Skinned then begin
    if (GetWindowLong(Form.Handle, GWL_STYLE) and WS_SYSMENU = WS_SYSMENU) or HaveSysMenu then begin
      if FDrawNonClientArea
        then SetWindowLong(Form.Handle, GWL_STYLE, GetWindowLong(Form.Handle, GWL_STYLE) and not WS_SYSMENU)
        else SetWindowLong(Form.Handle, GWL_STYLE, GetWindowLong(Form.Handle, GWL_STYLE) or WS_SYSMENU);
      HaveSysMenu := True;
    end
    else HaveSysMenu := False;
  end
  else begin
    if HaveSysMenu then SetWindowLong(Form.Handle, GWL_STYLE, GetWindowLong(Form.Handle, GWL_STYLE) or WS_SYSMENU);
    HaveSysMenu := False;
  end;
end;

procedure TsSkinProvider.SetDrawNonClientArea(const Value: boolean);
begin
  if (FDrawNonClientArea <> Value) then begin
    FDrawNonClientArea := Value;
    if (csDesigning in ComponentState) then Exit;
    if Value then begin
      CheckSysMenu(True);

      if not (csDesigning in ComponentState) and (Form <> nil) and Form.Showing and SkinData.Skinned then begin
        SkinData.BGChanged := True;
        FreeAndNil(TitleBG);
        RedrawWindow(Form.Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW);
        RefreshFormScrolls(Self, ListSW, False);
      end;
    end
    else begin
      if (@Ac_SetWindowTheme <> nil) then Ac_SetWindowTheme(Form.Handle, nil, nil);
      if ListSW <> nil then FreeAndNil(ListSW);
//      InitializeFlatSB(Form.Handle);
      if HaveSysMenu then SetWindowLong(Form.Handle, GWL_STYLE, GetWindowLong(Form.Handle, GWL_STYLE) or WS_SYSMENU);
      if Form.Showing then SetWindowRgn(Form.Handle, 0, True);
    end;
  end;
end;

procedure TsSkinProvider.AC_WMNCCalcSize(Message: TWMNCCalcSize);
//var
//  R : TRect;
begin
//  R := Message.CalcSize_Params.rgrc[0];
  OldWndProc(TMessage(Message));
{  Message.CalcSize_Params.rgrc[0].Top := R.Top + FormTitleHeight;
  Message.CalcSize_Params.rgrc[0].Left := R.Left + FormLeftBorderWidth;
  Message.CalcSize_Params.rgrc[0].Right := R.Right - FormRightBorderWidth;
  Message.CalcSize_Params.rgrc[0].Bottom := R.Bottom - FormBottomBorderWidth;}
end;

{ TsSystemMenu }

procedure TsSystemMenu.CloseClick(Sender: TObject);
begin
  Sendmessage(FForm.Handle, WM_SYSCOMMAND, SC_CLOSE, 0);
end;

constructor TsSystemMenu.Create(AOwner: TComponent);
begin
  FOwner := TsSkinProvider(AOwner);
  FForm := FOwner.Form;
  inherited Create(AOwner);
  Name := 'SysMenu';
  Generate
end;

function TsSystemMenu.EnabledMax: boolean;
begin
  Result := ((TForm(FForm).FormStyle = fsMDIChild) or
      ((FForm.WindowState <> wsMaximized) and (FForm.BorderStyle in [bsSingle, bsSizeable]))) and
      (biMaximize in FOwner.Form.BorderIcons);
end;

function TsSystemMenu.EnabledMin: boolean;
begin
  Result := (biMinimize in FOwner.Form.BorderIcons) and (FForm.WindowState <> wsMinimized);
end;

function TsSystemMenu.EnabledMove: boolean;
begin
  Result := (FForm.WindowState <> wsMaximized);
end;

function TsSystemMenu.EnabledRestore: boolean;
begin
  Result := ((biMaximize in FOwner.Form.BorderIcons) and (FForm.WindowState <> wsNormal)) or
    (FForm.WindowState = wsMinimized);
end;

function TsSystemMenu.EnabledSize: boolean;
begin
  Result := (FForm.BorderStyle <> bsSingle) and not IsIconic(FForm.Handle);
end;

procedure TsSystemMenu.ExtClick(Sender: TObject);
begin
  PostMessage(FForm.Handle, WM_SYSCOMMAND, TComponent(Sender).Tag, 0);
end;

procedure TsSystemMenu.Generate;
var
  Menu : HMENU;
  i, j : integer;
  s : acString;
  sCut : string;
  TmpItem : TMenuItem;
  function CreateSystemItem(const Caption : acString; const Name : string; EventProc : TNotifyEvent) : TMenuItem; begin
{$IFDEF TNTUNICODE}
    Result := TTntMenuItem.Create(Self);
{$ELSE}
    Result := TMenuItem.Create(Self);
{$ENDIF}
    Result.Caption := Caption;
    Result.OnClick := EventProc;
    Result.Name := Name;
  end;
  function GetItemText(ID : Cardinal; var Caption : acString; var ShortCut : String; uFlag : Cardinal) : boolean;
  var
    Text: array[0..255] of acChar;
{$IFDEF TNTUNICODE}
    ws : WideString;
{$ENDIF}
    P : integer;
  begin
{$IFDEF TNTUNICODE}
    Result := GetMenuStringW(Menu, ID, Text, 256, uFlag) <> 0;
{$ELSE}
    Result := GetMenuString(Menu, ID, Text, 256, uFlag) <> 0;
{$ENDIF}
    if Result then begin
      P := Pos(#9, Text);
      if P = 0 then ShortCut := '' else begin
        ShortCut := Copy(Text, P + 1, Length(Text) - P);
{$IFDEF TNTUNICODE}
        ws := Text;
        ws := Copy(ws, 1, P - 1);
        Caption := ws;
        Exit;
{$ELSE}
        StrLCopy(Text, Text, P - 1);
{$ENDIF}
      end;
      Caption := Text;
    end;
  end;
begin
  Items.Clear;
  ExtItemsCount := 0;
  Menu := GetSystemMenu(FForm.Handle, False);

  if (Menu = 0) or not GetItemText(SC_RESTORE, s, sCut, MF_BYCOMMAND) then s := LoadStr(s_RestoreStr);
  ItemRestore := CreateSystemItem(s, 'acIR', RestoreClick);
  ItemRestore.Tag := SC_RESTORE;
  Self.Items.Add(ItemRestore);

  if (Menu = 0) or not GetItemText(SC_MOVE, s, sCut, MF_BYCOMMAND) then s := LoadStr(s_MoveStr);
  ItemMove := CreateSystemItem(s, 'acIM', MoveClick);
  Self.Items.Add(ItemMove);
  ItemMove.Tag := SC_MOVE;

  if (Menu = 0) or not GetItemText(SC_SIZE, s, sCut, MF_BYCOMMAND) then s := LoadStr(s_SizeStr);
  ItemSize := CreateSystemItem(s, 'acIS', SizeClick);
  Self.Items.Add(ItemSize);
  ItemSize.Tag := SC_SIZE;

  if (Menu = 0) or not GetItemText(SC_MINIMIZE, s, sCut, MF_BYCOMMAND) then s := LoadStr(s_MinimizeStr);
  ItemMinimize := CreateSystemItem(s, 'acIN', MinClick);
  Self.Items.Add(ItemMinimize);
  ItemMinimize.Tag := SC_MINIMIZE;

  if (Menu = 0) or not GetItemText(SC_MAXIMIZE, s, sCut, MF_BYCOMMAND) then s := LoadStr(s_MaximizeStr);
  ItemMaximize := CreateSystemItem(s, 'acIX', MaxClick);
  Self.Items.Add(ItemMaximize);
  ItemMaximize.Tag := SC_MAXIMIZE;

  Self.Items.InsertNewLineAfter(ItemMaximize);

  i := GetMenuItemCount(Menu);
  TmpItem := nil;
  for i := 0 to i - 1 do begin
    j := GetMenuItemID(Menu, i);
    if (j < $F000) and GetItemText(i, s, sCut, MF_BYPOSITION) then begin // If some external menuitems are exists
{$IFDEF TNTUNICODE}
      TmpItem := TTntMenuItem.Create(Self);
{$ELSE}
      TmpItem := TMenuItem.Create(Self);
{$ENDIF}
      TmpItem.Caption := s;
      TmpItem.Tag := j;
      if sCut <> '' then TmpItem.ShortCut := TextToShortCut(sCut);
      TmpItem.OnClick := ExtClick;
      Self.Items.Add(TmpItem);
      inc(ExtItemsCount);
    end;
  end;
  if ExtItemsCount > 0 then Self.Items.InsertNewLineAfter(TmpItem);

  if (Menu = 0) or not GetItemText(SC_CLOSE, s, sCut, MF_BYCOMMAND) then s := LoadStr(s_CloseStr);
  ItemClose := CreateSystemItem(s, 'acIC', CloseClick);
  if sCut <> '' then ItemClose.ShortCut := TextToShortCut(sCut);
  Self.Items.Add(ItemClose);
  ItemClose.Tag := SC_CLOSE;
end;

procedure TsSystemMenu.MakeSkinItems;
var
  sl : TacStringList;
  i : integer;
  SkinItem, TempItem : TMenuItem;
begin
  if Assigned(FOwner.SkinData.SkinManager) then begin
    sl := TacStringList.Create;
    FOwner.SkinData.SkinManager.GetSkinNames(sl);

    if sl.Count > 0 then begin
{$IFDEF TNTUNICODE}
      SkinItem := TTntMenuItem.Create(Self);
{$ELSE}
      SkinItem := TMenuItem.Create(Self);
{$ENDIF}
      SkinItem.Caption := LoadStr(s_AvailSkins);
      SkinItem.Name := s_SkinSelectItemName;
      Self.Items.Insert(0, SkinItem);
      Self.Items.InsertNewLineAfter(SkinItem);
      for i := 0 to sl.Count - 1 do begin
{$IFDEF TNTUNICODE}
        TempItem := TTntMenuItem.Create(Self);
{$ELSE}
        TempItem := TMenuItem.Create(Self);
{$ENDIF}
        TempItem.Caption := sl[i];
        TempItem.OnClick := SkinSelect;
        TempItem.Name := s_SkinSelectItemName + IntToStr(i);
        TempItem.RadioItem := True;
        if TempItem.Caption = FOwner.SkinData.SkinManager.SkinName then TempItem.Checked := True;
        if (i <> 0) and (i mod 20 = 0) then TempItem.Break := mbBreak;
        SkinItem.Add(TempItem);
      end;
    end;
    FreeAndNil(sl);
  end;
end;

procedure TsSystemMenu.MaxClick(Sender: TObject);
begin
  Sendmessage(FForm.Handle, WM_SYSCOMMAND, SC_MAXIMIZE, 0);
  UpdateItems;
end;

procedure TsSystemMenu.MinClick(Sender: TObject);
begin
  if (Application.MainForm = FForm) then begin
    Application.Minimize;
    FOwner.SystemMenu.UpdateItems;
  end
  else begin
    SendMessage(FOwner.Form.Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
  end;
end;

procedure TsSystemMenu.MoveClick(Sender: TObject);
begin
  Sendmessage(FForm.Handle, WM_SYSCOMMAND, SC_MOVE, 0);
end;

procedure TsSystemMenu.RestoreClick(Sender: TObject);
begin
  Sendmessage(FForm.Handle, WM_SYSCOMMAND, SC_RESTORE, 0);
  UpdateItems;
  if (FOwner.Form.FormStyle = fsMDIChild) then begin
  end;
end;

procedure TsSystemMenu.SizeClick(Sender: TObject);
begin
  Sendmessage(FForm.Handle, WM_SYSCOMMAND, SC_SIZE, 0);
end;

procedure TsSystemMenu.SkinSelect(Sender: TObject);
begin
  if Assigned(FOwner.SkinData.SkinManager) then begin
    FOwner.SkinData.SkinManager.SkinName := DelChars(TMenuItem(Sender).Caption, '&');
  end;
end;

procedure TsSystemMenu.UpdateGlyphs;
begin
// 
end;

procedure TsSystemMenu.UpdateItems(Full : boolean = False);
begin
  if Full then begin
    Generate;
  end;
  if Assigned(Self) and Assigned(FForm) then begin
    ItemRestore.Visible  := VisibleRestore;
    ItemMove.Visible     := True;
    ItemSize.Visible     := VisibleSize;
    ItemMinimize.Visible := VisibleMin;
    ItemMaximize.Visible := VisibleMax;
    ItemClose.Visible    := VisibleClose;

    ItemRestore.Enabled  := EnabledRestore;
    ItemMove.Enabled     := EnabledMove;
    ItemSize.Enabled     := EnabledSize;
    ItemMinimize.Enabled := EnabledMin;
    ItemMaximize.Enabled := EnabledMax;
    ItemClose.Enabled    := True;
  end;
end;

function TsSystemMenu.VisibleClose: boolean;
begin
  Result := FOwner.HaveSysMenu;
{  Result := (biSystemMenu in FOwner.Form.BorderIcons);
  Result := GetWindowLong(FOwner.Form.Handle, GWL_STYLE) and WS_SYSMENU = WS_SYSMENU;}
end;

function TsSystemMenu.VisibleMax: boolean;
begin
  Result := False;
  if (Self = nil) or not VisibleClose then Exit;

  Result := IsIconic(Self.FForm.Handle) or (TForm(FForm).FormStyle = fsMDIChild) or
    ((FForm.BorderStyle <> bsDialog) and
    ((FForm.BorderStyle <> bsSingle) or (biMaximize in FOwner.Form.BorderIcons)) and
    (FForm.BorderStyle <> bsNone) and
    (FForm.BorderStyle <> bsSizeToolWin) and
    (FForm.BorderStyle <> bsToolWindow) and VisibleClose) and (biMaximize in FOwner.Form.BorderIcons);
end;

function TsSystemMenu.VisibleMin: boolean;
begin
  Result := False;
  if (Self = nil) or not VisibleClose then Exit;
  if IsIconic(FForm.Handle) or (TForm(FForm).FormStyle = fsMDIChild) then begin
    Result := True
  end
  else begin
    Result :=
      (FForm.BorderStyle <> bsDialog) and
      (FForm.BorderStyle <> bsNone) and
      ((FForm.BorderStyle <> bsSingle) or (biMinimize in FOwner.Form.BorderIcons)) and
      (FForm.BorderStyle <> bsSizeToolWin) and
      (FForm.BorderStyle <> bsToolWindow) and (biMinimize in FOwner.Form.BorderIcons) and VisibleClose;
  end;
end;

function TsSystemMenu.VisibleRestore: boolean;
begin
  Result := False;
  if (Self = nil) or not VisibleClose then Exit;
  Result := (TForm(FForm).FormStyle = fsMDIChild) or
    ((FForm.BorderStyle <> bsDialog) and
    (FForm.BorderStyle <> bsNone) and
    (FForm.BorderStyle <> bsSizeToolWin) and
    (FForm.BorderStyle <> bsToolWindow)) and VisibleClose;
end;

function TsSystemMenu.VisibleSize: boolean;
begin
  Result := False;
  if Self = nil then Exit;
  Result := (TForm(FForm).FormStyle = fsMDIChild) or
    ((FForm.BorderStyle <> bsDialog) and
    (FForm.BorderStyle <> bsNone) and
    (FForm.BorderStyle <> bsToolWindow));
end;

{ TsTitleIcon }

constructor TsTitleIcon.Create;
begin
  FGlyph := TBitmap.Create;
  FHeight := 0;
  FWidth := 0;
end;

destructor TsTitleIcon.Destroy;
begin
  FreeAndNil(FGlyph);
  inherited Destroy; 
end;

procedure TsTitleIcon.SetGlyph(const Value: TBitmap);
begin
  FGlyph.Assign(Value);
end;

procedure TsTitleIcon.SetHeight(const Value: integer);
begin
  FHeight := Value;
end;

procedure TsTitleIcon.SetWidth(const Value: integer);
begin
  FWidth := Value;
end;

{ TsTitleButtons }

constructor TsTitleButtons.Create(AOwner: TsSkinProvider);
begin
  inherited Create(TsTitleButton);
  FOwner := AOwner;
end;

destructor TsTitleButtons.Destroy;
begin
  FOwner := nil;
  inherited Destroy;
end;

function TsTitleButtons.GetItem(Index: Integer): TsTitleButton;
begin
  Result := TsTitleButton(inherited GetItem(Index))
end;

function TsTitleButtons.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TsTitleButtons.SetItem(Index: Integer; Value: TsTitleButton);
begin
  inherited SetItem(Index, Value);
end;

procedure TsTitleButtons.Update(Item: TCollectionItem);
begin
  inherited;
end;

{ TsTitleButton }

constructor TsTitleButton.Create(Collection: TCollection);
begin
  FGlyph := TBitmap.Create;
  FUseSkinData := True;
  FEnabled := True;
  inherited Create(Collection);
  if FName = '' then FName := ClassName;
end;

destructor TsTitleButton.Destroy;
begin
  FreeAndNil(FGlyph);
  inherited Destroy;
end;

function TsTitleButton.GetDisplayName: string;
begin
  Result := Name;
end;

procedure TsTitleButton.MouseDown(BtnIndex : integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TsTitleButton.MouseUp(BtnIndex : integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then FOnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TsTitleButton.SetGlyph(const Value: TBitmap);
begin
  FGlyph.Assign(Value);
end;

procedure TsTitleButton.SetName(const Value: string);
begin
  if FName <> Value then FName := Value;
end;

{ TacCtrlAdapter }

procedure TacCtrlAdapter.AddNewItem(Ctrl: TWinControl);
begin
  AddNewItem(Ctrl, DefaultSection);
end;

procedure TacCtrlAdapter.AddNewItem(Ctrl: TWinControl; const SkinSection: string);
var
  i, Index, l : integer;
  Found : boolean;
  CanAdd : boolean;
  s : string;
begin
  if (Ctrl = nil) or (Ctrl.Tag = ExceptTag) then Exit;
  if (Ctrl.Parent = nil) or GetBoolMsg(Ctrl, AC_CTRLHANDLED) then Exit;
  CanAdd := True;
  s := SkinSection;
  if Assigned(Provider.OnSkinItem) then Provider.FOnSkinItem(Ctrl, CanAdd, s);
  if not CanAdd then Exit;

{$IFNDEF ALITE}
  if Ctrl is TFrame then begin
    with TsFrameAdapter.Create(Ctrl) do SkinData.SkinSection := s_GroupBox;//s_CheckBox;
    Exit;
  end;
{$ENDIF}

  Index := -1;
  l := Length(Items);
  Found := False;
  for i := 0 to l - 1 do if (Items[i].Ctrl = Ctrl) then begin // If added in list already, then go to Exit
    Index := i;
    Found := True;
    Break;
  end;
  if Index = -1 then begin
    SetLength(Items, l + 1);
    l := Length(Items);
    Index := l - 1;
    Items[Index] := TacAdapterItem.Create;
    Items[Index].Adapter := Self;
    Items[Index].Ctrl := Ctrl;
  end;
  Items[Index].SkinData.SkinSection := s;
  if Found and Assigned(Items[Index].ScrollWnd) and Items[Index].ScrollWnd.Destroyed then begin
    FreeAndNil(Items[Index].ScrollWnd);
    if (Items[Index].Ctrl.Parent <> nil) then Items[Index].DoHook(Ctrl);
  end
  else if not Found then begin
    if (Items[Index].Ctrl.Parent <> nil) then Items[Index].DoHook(Ctrl);
    if Items[Index].ScrollWnd = nil then begin
      FreeAndNil(Items[Index]);
      SetLength(Items, Index);
    end;
  end;
end;

type
  TacAccessWinCtrl = class(TWinControl);

procedure TacCtrlAdapter.AddAllItems(OwnerCtrl : TWinControl = nil);
var
  i : integer;
  sSection : string;
  Owner : TWinControl;
begin
{$IFDEF CHECKXP}
{  Provider.StdBgIsUsed := False;
  for i := 0 to Provider.Form.ControlCount - 1 do if (Provider.Form.Controls[i] is TWinControl) and TacAccessWinCtrl(Provider.Form.Controls[i]).ParentBackground then begin
    Provider.StdBgIsUsed := True;
    Break
  end;}
{$ENDIF}

  if bFlag or not (srThirdParty in Provider.SkinData.SkinManager.SkinningRules) then Exit;
  bFlag := True;
  if OwnerCtrl = nil then Owner := Provider.Form else Owner := OwnerCtrl;
  if Owner = nil then Exit;
  CleanItems;
  for i := 0 to Owner.ComponentCount - 1 do begin
    if IsControlSupported(Owner.Components[i]) then begin
      if (Owner.Components[i] is TWinControl) then begin
        sSection := s_Edit;
        AddNewItem(TWinControl(Owner.Components[i]), sSection);
      end
      else if (Owner.Components[i] is TCustomLabel) and not (Owner.Components[i] is TsCustomLabel) then begin
        TLabel(Owner.Components[i]).Transparent := True;
        TLabel(Owner.Components[i]).Font.Color := DefaultManager.GetGlobalFontColor;
      end;
    end;
    if (Owner.Components[i] is TWinControl) and TWinControl(Owner.Components[i]).HandleAllocated and (TWinControl(Owner.Components[i]).Parent <> nil) then begin
      bFlag := False;
      AddAllItems(TWinControl(Owner.Components[i])); // Recursion
    end;
  end;
  for i := 0 to Owner.ControlCount - 1 do begin
    if IsControlSupported(Owner.Controls[i]) then begin
      if (Owner.Controls[i] is TWinControl) then begin
        sSection := s_Edit;
        AddNewItem(TWinControl(Owner.Controls[i]), sSection);
      end
      else if (Owner.Controls[i] is TLabel) then begin
        TLabel(Owner.Controls[i]).Transparent := True;
        TLabel(Owner.Controls[i]).Font.Color := DefaultManager.GetGlobalFontColor;
      end;
    end;
    if (Owner.Controls[i] is TWinControl) and TWinControl(Owner.Controls[i]).HandleAllocated and (TWinControl(Owner.Controls[i]).Parent <> nil) then begin
      bFlag := False;
      AddAllItems(TWinControl(Owner.Controls[i])); // Recursion
    end;
  end;
  bFlag := False;
end;

function TacCtrlAdapter.Count: integer;
begin
  Result := Length(Items);
end;

destructor TacCtrlAdapter.Destroy;
begin
  CleanItems;
  RemoveAllItems;
  inherited Destroy;
end;

function TacCtrlAdapter.GetCommonData(Index: integer): TsCommonData;
begin
  Result := nil;
end;

function TacCtrlAdapter.GetItem(Index: integer) : TacAdapterItem;
begin
  if (Index > -1) and (Index < Count) then Result := Items[Index] else Result := nil;
end;

function TacCtrlAdapter.IndexOf(Ctrl : TWinControl): integer;
var
  i : integer;
begin
  Result := -1;
  for i := 0 to Length(Items) - 1 do if Items[i].Ctrl = Ctrl then begin
    Result := i;
    Exit;
  end;
end;

procedure TacCtrlAdapter.RemoveItem(Index: integer);
var
  l : integer;
begin
  l := Count;
  if (Index < l) and (l > 0) then begin
    if Items[Index] <> nil then FreeAndNil(Items[Index]);
    Items[Index] := Items[l - 1];
    SetLength(Items, l - 1);
  end;
end;

function TacCtrlAdapter.IsControlSupported(Control: TComponent): boolean;
var
  i, j : integer;
  CanAdd : boolean;
  s : string;
begin
  Result := False;
  if (Control.Tag = ExceptTag) or ((Control is TWinControl) and not CtrlIsReadyForHook(TWinControl(Control))) then Exit;

  if (Control is TWinControl) and GetBoolMsg(TWinControl(Control), AC_CTRLHANDLED) then Exit else if Control is TCustomLabel then begin
    CanAdd := True;
    s := '';
    if Assigned(Provider.OnSkinItem) then Provider.FOnSkinItem(Control, CanAdd, s);
    Result := CanAdd;
    Exit;
  end
  else if not (Control is TGraphicControl) then begin
    if Control is TFrame then begin
      CanAdd := True;
      s := '';
      if Assigned(Provider.OnSkinItem) then Provider.FOnSkinItem(Control, CanAdd, s);
      Result := CanAdd;
      Exit;
    end
    else for j := 0 to Length(Provider.SkinData.SkinManager.ThirdLists) - 1 do begin
      for i := 0 to Provider.SkinData.SkinManager.ThirdLists[j].Count - 1 do if Provider.SkinData.SkinManager.ThirdLists[j][i] = Control.ClassName then begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

procedure TacCtrlAdapter.RemoveAllItems;
var
  l : integer;
begin
  if bRemoving then Exit;
  bRemoving := True;
  while Length(Items) > 0 do begin
    l := Length(Items);
    FreeAndNil(Items[l - 1]);
    SetLength(Items, l - 1);
  end;
  bRemoving := False;
end;

procedure TacCtrlAdapter.WndProc(var Message: TMessage);
var
  i : integer;
begin
  try
    for i := 0 to Length(Items) - 1 do if Items[i].ScrollWnd <> nil then
      if not Items[i].ScrollWnd.Destroyed and (Items[i].Ctrl.Parent <> nil)
        then SendMessage(Items[i].Ctrl.Handle, Message.Msg, Message.WParam, Message.LParam)
  except
  end;
end;

procedure TacCtrlAdapter.AfterConstruction;
begin
  inherited;
end;

constructor TacCtrlAdapter.Create(AProvider: TsSkinProvider);
begin
  Provider := AProvider;
  Items := nil;
end;

procedure TacCtrlAdapter.CleanItems;
var
  i, j : integer;
begin
  i := 0;
  while i < Length(Items) do begin
    if (Items[i] = nil) or (Items[i].ScrollWnd = nil) or Items[i].ScrollWnd.Destroyed then begin
      if Items[i] <> nil then FreeAndNil(Items[i]);
      for j := i to Length(Items) - 2 do begin
        Items[j] := Items[j + 1];
        Items[j + 1] := nil;
      end;
      SetLength(Items, Length(Items) - 1);
    end;
    inc(i)
  end;
end;

{ TacAdapterItem }
constructor TacAdapterItem.Create;
begin
  inherited Create;
  OldFontColor := -1;
  SkinData := TsCommonData.Create(Self, True);
  SkinData.COC := COC_TsAdapter;
  ScrollWnd := nil;
end;

destructor TacAdapterItem.Destroy;
begin
  if (ScrollWnd <> nil) then FreeAndNil(ScrollWnd);
  FreeAndNil(SkinData);
  inherited Destroy;
end;

procedure TacAdapterItem.DoHook(Control: TWinControl);
var
  i, j : integer;
begin
  if (Control.TabOrder = ExceptTag) or not CtrlIsReadyForHook(Control) then Exit;
  if (Control.Parent <> nil) then if GetBoolMsg(Control, AC_CTRLHANDLED) then Exit;

  Self.Ctrl := Control;
  SkinData.FOwnerControl := Control;
  SkinData.FOwnerObject := TObject(Control);
  if (Control.Parent <> nil) then begin
    for j := 0 to Length(SkinData.SkinManager.ThirdLists) - 1 do begin
      for i := 0 to SkinData.SkinManager.ThirdLists[j].Count - 1 do if SkinData.SkinManager.ThirdLists[j][i] = Control.ClassName then begin
        case j of
          ord(tpEdit) : begin
            ScrollWnd := TacEditWnd.Create(Control.Handle, SkinData, SkinData.SkinManager, s_Edit);
            Exit;
          end;
          ord(tpButton) : begin
            SkinData.SkinSection := s_Button;
            ScrollWnd := TacBtnWnd.Create(Control.Handle, SkinData, SkinData.SkinManager, s_Button);
            Exit;
          end;
          ord(tpBitBtn) : begin
            SkinData.SkinSection := s_Button;
            ScrollWnd := TacBitBtnWnd.Create(Control.Handle, SkinData, SkinData.SkinManager, s_Button);
            Exit;
          end;
          ord(tpCheckBox) : begin
            SkinData.SkinSection := s_CheckBox;
            ScrollWnd := TacCheckBoxWnd.Create(Control.Handle, SkinData, SkinData.SkinManager, '');
            Exit;
          end;
          ord(tpComboBox) : begin
            ScrollWnd := TacComboBoxWnd.Create(Control.Handle, SkinData, SkinData.SkinManager, s_Edit);
            Exit;
          end;
          ord(tpGrid) : begin
            ScrollWnd := TacGridWnd.Create(Control.Handle, SkinData, SkinData.SkinManager, s_Edit);
            Exit;
          end;
          ord(tpGroupBox) : begin
            SkinData.SkinSection := s_GroupBox;
            ScrollWnd := TacGroupBoxWnd.Create(Control.Handle, SkinData, SkinData.SkinManager, '');
            Exit;
          end;
          ord(tpListView) : begin
            ScrollWnd := TacListViewWnd.Create(Control.Handle, SkinData, SkinData.SkinManager, s_Edit);
            Exit;
          end;
          ord(tpPanel) : begin
            SkinData.SkinSection := s_Panel;
            ScrollWnd := TacPanelWnd.Create(Control.Handle, SkinData, SkinData.SkinManager, s_Panel);
            Exit;
          end;
          ord(tpTreeView) : begin
            ScrollWnd := TacTreeViewWnd.Create(Control.Handle, SkinData, SkinData.SkinManager, s_Edit);
            Exit;
          end;
          ord(tpwwEdit) : begin
            ScrollWnd := TacWWComboBoxWnd.Create(Control, SkinData, SkinData.SkinManager, s_Edit);
            Exit;
          end;
          ord(tpGridEh) : begin
            ScrollWnd := TacGridEhWnd.Create(Control.Handle, SkinData, SkinData.SkinManager, s_Edit);
            Exit;
          end;
          ord(tpVirtualTree) : begin
            ScrollWnd := TacVirtualTreeViewWnd.Create(Control.Handle, SkinData, SkinData.SkinManager, s_Edit);
            Exit;
          end;
          ord(tpPageControl) : begin
            SkinData.SkinSection := s_PageControl;
            ScrollWnd := TacPageControlWnd.Create(Control.Handle, SkinData, SkinData.SkinManager, s_PageControl);
            Exit;
          end;
          ord(tpTabControl) : begin
            SkinData.SkinSection := s_PageControl;
            ScrollWnd := TacTabControlWnd.Create(Control.Handle, SkinData, SkinData.SkinManager, s_PageControl);
            Exit;
          end;
          ord(tpToolBar) : begin
            SkinData.SkinSection := s_ToolBar;
            ScrollWnd := TacToolBarVCLWnd.Create(Control.Handle, SkinData, SkinData.SkinManager, s_ToolBar);
            Exit;
          end;
          ord(tpStatusBar) : begin
            SkinData.SkinSection := s_StatusBar;
            ScrollWnd := TacStatusBarWnd.Create(Control.Handle, SkinData, SkinData.SkinManager, s_StatusBar);
            Exit;
          end;
        end;
      end;
    end;
  end;
end;

initialization

finalization
  if hDWMAPI > 0 then FreeLibrary(hDWMAPI);

end.

