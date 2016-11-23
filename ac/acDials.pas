unit acDials;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Controls, Graphics, Messages, SysUtils, Classes, Forms, sSkinProvider, acSBUtils, sCommonData, sSkinManager,
  sConst, menus;

type
  TacBorderStyle = (acbsDialog, acbsSingle, acbsNone, acbsSizeable, acbsToolWindow, acbsSizeToolWin);
  TacDialogWnd = class;

  TacSystemMenu = class(TPopupMenu)
  public
    FOwner : TacDialogWnd;

    ItemRestore : TMenuItem;
    ItemMove : TMenuItem;
    ItemSize : TMenuItem;
    ItemMinimize : TMenuItem;
    ItemMaximize : TMenuItem;
    ItemClose : TMenuItem;

    constructor Create(AOwner : TComponent); override;
    function EnabledMove : boolean;
    function EnabledSize : boolean;
    function VisibleSize : boolean;
    procedure UpdateItems;

    procedure RestoreClick(Sender: TObject);
    procedure MoveClick(Sender: TObject);
    procedure SizeClick(Sender: TObject);
    procedure MinClick(Sender: TObject);
    procedure MaxClick(Sender: TObject);
    procedure CloseClick(Sender: TObject);
  end;

  TacProvider = class;

  TacDialogWnd = class(TacScrollWnd)
  protected
    ArOR : TAOR;
    CurrentHT : integer;
    FFormActive : boolean;
    Initialized : boolean;
    DwmInitialized : boolean;
  public
    ButtonMin : TsCaptionButton;
    ButtonMax : TsCaptionButton;
    ButtonClose : TsCaptionButton;
    ButtonHelp : TsCaptionButton;

    TitleGlyph : TBitmap;
    TitleIcon : HIcon;
    TitleFont : TFont;
    dwStyle: LongInt;
    dwExStyle: LongInt;
    RgnChanged : boolean;
    WndRect : TRect;
    WndSize : TSize;
    BorderStyle : TacBorderStyle;
    TitleBG : TBitmap;
    TempBmp : TBitmap;
    Adapter : TacCtrlAdapter;
    SystemMenu : TacSystemMenu;
    Provider : TacProvider;
    procedure AdapterRemove;
    procedure AdapterCreate;
    procedure SendToAdapter(Message : TMessage);
    // Drawing
    procedure MakeTitleBG;
    procedure PaintAll;
    procedure PaintBorderIcons;
    procedure PaintCaption(dc : hdc);
    procedure PaintForm(var DC : hdc; SendUpdated : boolean = True);
    procedure PrepareTitleGlyph;
    procedure RepaintButton(i : integer);

    procedure acWndProc(var Message: TMessage); override;
    constructor Create(AHandle : hwnd; ASkinData : TsCommonData; ASkinManager : TsSkinManager; const SkinSection : string; Repaint : boolean = True); override;
    destructor Destroy; override;
    procedure InitParams;
    procedure UpdateIconsIndexes;

    // Messages
    procedure Ac_WMPaint(var Msg : TWMPaint);
    procedure Ac_WMNCPaint(var Message : TMessage);
    procedure Ac_WMNCHitTest(var Message : TMessage);
    procedure Ac_WMNCLButtonDown(var Message : TWMNCLButtonDown);
    procedure Ac_WMLButtonUp(var Message : TMessage);
    procedure Ac_WMActivate(var Message : TMessage);
    procedure Ac_WMNCActivate(var Message : TMessage);
    procedure Ac_DrawStaticItem(var Message : TWMDrawItem);
    function HTProcess(var Message : TWMNCHitTest) : integer;
    procedure SetHotHT(i : integer; Repaint : boolean = True);
    procedure SetPressedHT(i : integer);
    procedure DropSysMenu(x, y : integer);

    // Calculations
    function AboveBorder(Message : TWMNCHitTest) : boolean;
    function BarWidth(i : integer) : integer;
    function BorderHeight: integer;
    function BorderWidth: integer;
    function ButtonHeight(Index : integer) : integer;
    function CaptionHeight : integer;
    function CursorToPoint(x, y : integer) : TPoint;
    function FormActive : integer;
    function HeaderHeight : integer;
    function OffsetX : integer;
    function OffsetY : integer;
//    function RBGripPoint(ImgIndex : integer) : TPoint;
    function SysBorderWidth : integer;
    function SysBorderHeight : integer;
    function SysButtonWidth(Btn : TsCaptionButton) : integer;
    function TitleBtnsWidth : integer;

    function VisibleMax : boolean;
    function VisibleMin : boolean;
    function VisibleHelp : boolean;
    function VisibleClose : boolean;
    function VisibleRestore : boolean;

    function EnabledMax : boolean;
    function EnabledMin : boolean;
    function EnabledClose : boolean;
    function EnabledRestore : boolean;
  end;

  TacProvider = class(TComponent)
  protected
    FForm: TForm;
  public
    BiDiLeft : boolean;
    CtrlHandle : THandle;
    sp : TsSkinProvider;
    ListSW : TacDialogWnd;

    acSkinnedCtrls : TList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitForm(Form: TCustomForm);
    function InitSkin(aHandle : hwnd) : boolean;
    function InitHwndControls(hWnd : hwnd) : boolean;
    function AddControl(aHwnd : hwnd) : boolean;
    function FindCtrlInList(hwnd: THandle): TObject;
  end;

{$IFNDEF NOMNUHOOK}
  TacMnuArray = array of TacMnuWnd;
{$ENDIF}

var
  HookCallback, WndCallBack, WndCallRet : HHOOK;
  acSupportedList : TList;
  fRect : TRect;
  DlgLeft : integer = -1;
  DlgTop : integer = -1;
{$IFNDEF NOMNUHOOK}
  MnuArray : TacMnuArray;
{$ENDIF}

function VisibleDlgCount : integer;
function ControlExists(CtrlHandle : hwnd; const Name : string) : boolean;
function AddSupportedForm(hwnd: THandle): boolean;
function SkinHookCBT(code: integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
function GetWndClassName(hwnd: THandle): string;
function FindFormInList(hwnd: THandle): TObject;
function FindFormOnScreen(hwnd: THandle): TCustomForm;
procedure InitDialog(hwnd: THandle; var ListSW : TacDialogWnd);
procedure DrawAppIcon(ListSW : TacDialogWnd);
function GetWndText(hwnd: THandle): WideString;
procedure FillArOR(ListSW : TacDialogWnd);
procedure UpdateRgn(ListSW : TacDialogWnd; Repaint : boolean = True);
function GetRgnFromArOR(ListSW : TacDialogWnd; X : integer = 0; Y : integer = 0) : hrgn;
procedure BroadCastHwnd(const hWnd: hwnd; Message: TMessage);
procedure ClearMnuArray;
procedure CleanArray;

implementation

uses
  sVclUtils, sMessages, acntUtils, FlatSB, sSkinProps{$IFDEF LOGGED}, sDebugMsgs{$ENDIF},
  sGraphUtils, sAlphaGraph, sStrings, sStyleSimply, Commctrl, IniFiles, sSkinMenus, sDefaults, acGlow;

const
  rsfName = '#32770';
  s_TMessageForm = 'TMessageForm';

var
  biClicked : boolean = False;
  RgnChanging : boolean = False;

procedure BroadCastHwnd(const hWnd: hwnd; Message: TMessage);
var
  hCtrl : THandle;
begin
  hCtrl := GetTopWindow(hWnd);
  while hCtrl <> 0 do begin
    SendMessage(hCtrl, Message.Msg, Message.WParam, Message.LParam);
    hCtrl := GetNextWindow(hCtrl, GW_HWNDNEXT);
  end;
end;

function VisibleDlgCount : integer;
var
  i: integer;
  ap: TacProvider;
begin
  Result := 0;
  for i := 0 to acSupportedList.Count - 1 do begin
    ap := TacProvider(acSupportedList[i]);
    if (ap <> nil) and (ap.ListSW <> nil) and IsWindowVisible(ap.ListSW.CtrlHandle) then inc(Result);
  end;
end;

function ControlExists(CtrlHandle : hwnd; const Name : string) : boolean;
var
  hCtrl : THandle;
  s : string;
begin
  Result := False;
  hCtrl := GetTopWindow(CtrlHandle);
  while hCtrl <> 0 do begin
    s := LowerCase(GetWndClassName(hCtrl));
    if (s = Name) then begin
      Result := True;
      Exit;
    end
    else if ControlExists(hCtrl, Name) then begin
      Result := True;
      Exit;
    end;
    hCtrl := GetNextWindow(hCtrl, GW_HWNDNEXT);
  end;
end;

function SkinHookCBT(code: integer; wParam: WPARAM; lParam: LPARAM): LRESULT;
var
  wHandle : THandle;
  i : integer;
  s : string;
begin
{.$IFDEF LOGGED
  AddToLog(MakeMessage(code, wParam, lParam, 0));
$ENDIF}
  Result := CallNextHookEx(HookCallback, Code, wParam, lParam);
  case code of
{$IFNDEF NOMNUHOOK}
    HCBT_CREATEWND : if not AeroIsEnabled then begin
      wHandle := Thandle(wParam);
      s := GetWndClassName(wHandle);
      if (s = '#32768') then if not GetBoolMsg(wHandle, AC_CTRLHANDLED) then begin
        i := Length(MnuArray);
        SetLength(MnuArray, i + 1);
        MnuArray[i] := TacMnuWnd.Create(wHandle, nil, DefaultManager, s_MainMenu, False);
        MnuArray[i].CtrlHandle := wHandle;
//        if AeroIsEnabled then RedrawWindow(wHandle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW);
      end;
    end;
{$ENDIF}
    HCBT_ACTIVATE : AddSupportedForm(THandle(wParam));
    HCBT_DESTROYWND : CleanArray;
  end;
end;

function AddSupportedForm(hwnd: THandle): boolean;
var
  ap : TacProvider;
  Form : TCustomForm;
  s : string;
  b : boolean;
  i : integer;
begin
  Result := false;
  if GetBoolMsg(hwnd, AC_CTRLHANDLED) then Exit;
  if (DefaultManager = nil) then Exit;
  if FindFormInList(hwnd) = nil then begin
    Form := FindFormOnScreen(hwnd);

    s := GetWndClassName(hwnd); // Optimize !!
    if (s = 'TApplication') or (s = 'TQRStandardPreview') {$IFDEF DEVEX}or (s = 'TcxGridFilterPopup'){$ENDIF} then exit;

    if Form <> nil then begin
      b := s = s_TMessageForm;
      if b then begin
        if not (srStdDialogs in DefaultManager.SkinningRules) then Exit;
      end
      else begin
        if not (srStdForms in DefaultManager.SkinningRules) or (Form.Tag = ExceptTag) then Exit;
      end;
      ap := TacProvider.Create(Form);
      acSupportedList.add(ap);
      ap.InitForm(Form);
      if b and Assigned(ap.sp) then ap.sp.MakeSkinMenu := False;
      // Add MDIChild which haven't a SkinProvider
      if TForm(Form).FormStyle = fsMDIForm then begin
        for i := 0 to TForm(Form).MDIChildCount - 1 do begin
          if not GetBoolMsg(TForm(Form).MDIChildren[i].Handle, AC_CTRLHANDLED) then begin
            ap := TacProvider.Create(TForm(Form).MDIChildren[i]);
            acSupportedList.add(ap);
            ap.InitForm(TForm(Form).MDIChildren[i]);
          end;
        end;
      end;
    end
    else begin
      if not (srStdDialogs in DefaultManager.SkinningRules) then Exit;
      if (VisibleDlgCount > 0) and not ControlExists(hwnd, 'toolbarwindow32') then begin
        Result := False;
        Exit;
      end;
      ap := TacProvider.Create(Form);
      acSupportedList.add(ap);
      if not ap.InitSkin(hwnd) then FreeAndNil(ap);
    end;
    Result := true;
  end;
end;

function GetWndClassName(Hwnd: THandle): string;
var
  Buf: array[0..128] of char;
begin
  GetClassName(Hwnd, Buf, 128);
  result := StrPas(Buf);
end;

function FindFormInList(hwnd: THandle): TObject;
var
  i: integer;
  ap: TacProvider;
begin
  Result := nil;
  for i := 0 to acSupportedList.Count - 1 do begin
    ap := TacProvider(acSupportedList[i]);
    if (ap <> nil) and (ap.CtrlHandle = hwnd) then begin
      Result := ap;
      Break;
    end;
  end;
end;

function FindFormOnScreen(hwnd: THandle): TCustomForm;
var
  i, j : integer;
  f : TCustomForm;
begin
  Result := nil;
  for i := 0 to Screen.CustomFormCount - 1 do begin
    f := Screen.CustomForms[i];
    if f.Handle = hwnd then begin
      Result := Screen.CustomForms[i];
      exit;
    end;
    if TForm(Screen.CustomForms[i]).FormStyle = fsMDIForm then begin
      for j := 0 to TForm(Screen.CustomForms[i]).MDIChildCount - 1 do begin
        if TForm(Screen.CustomForms[i]).MDIChildren[j].Handle = hwnd then begin
          Result := TForm(Screen.CustomForms[i]).MDIChildren[j];
          exit;
        end;
      end;
    end;
  end;
end;

{ TacProvider }

function TacProvider.AddControl(aHwnd: hwnd) : boolean;
var
  st : dword;
  Style : LongInt;
  s: string;
  Wnd : TacMainWnd;
  R : TRect;
begin
  Result := True;
  if GetBoolMsg(aHwnd, AC_CTRLHANDLED) then exit;
  if (ahWnd = 0) then Exit;

  acDlgMode := True;

  s := LowerCase(GetWndClassName(aHwnd));

  Style := GetWindowLong(aHwnd, GWL_STYLE);
  Wnd := nil;
  if s = 'static' then begin
    GetWindowRect(aHwnd, R);
    st := Style and SS_TYPEMASK;
    if (Style and SS_ICON = SS_ICON) then begin // Icon
      Wnd := TacIconWnd.Create(aHwnd, nil, ListSW.SkinData.SkinManager, s_CheckBox)
    end
    else if (Style and SS_BITMAP = SS_BITMAP) then begin // Bitmap
      Wnd := TacIconWnd.Create(aHwnd, nil, ListSW.SkinData.SkinManager, s_CheckBox)
    end
    else if (Style and SS_OWNERDRAW = SS_OWNERDRAW) then begin // Bitmap
    end
    else if st in [SS_SIMPLE, SS_GRAYRECT, SS_WHITERECT] then begin // Bitmap
    end
    else begin
      Wnd := TacStaticWnd.Create(aHwnd, nil, ListSW.SkinData.SkinManager, s_CheckBox)
    end;
  end
  else if (s = rsfName) then begin
    Wnd := TacTransPanelWnd.Create(aHwnd, nil, ListSW.SkinData.SkinManager, s_CheckBox)
  end
  else if (s = 'tpanel') then begin
    acSkinnedCtrls.Add(TacDlgPanelWnd.Create(aHwnd, nil, ListSW.SkinData.SkinManager, s_Panel))
  end
  else if (s = 'tsilentpaintpanel') then begin
    acSkinnedCtrls.Add(TacDlgPanelWnd.Create(aHwnd, nil, ListSW.SkinData.SkinManager, s_Panel))
  end
  else if (s = 'button') then begin
    if Style and BS_GROUPBOX = BS_GROUPBOX then begin
      Wnd := TacGroupBoxWnd.Create(aHwnd, nil, ListSW.SkinData.SkinManager, s_GroupBox)
    end
    else if (Style and BS_AUTOCHECKBOX = BS_AUTOCHECKBOX) or (Style and BS_CHECKBOX = BS_CHECKBOX) or (Style and BS_3STATE = BS_3STATE) then begin
      Wnd := TacCheckBoxWnd.Create(aHwnd, nil, ListSW.SkinData.SkinManager, s_CheckBox)
    end
    else if (Style and BS_AUTORADIOBUTTON = BS_AUTORADIOBUTTON) or (Style and BS_RADIOBUTTON = BS_RADIOBUTTON) then begin
      Wnd := TacCheckBoxWnd.Create(aHwnd, nil, ListSW.SkinData.SkinManager, s_CheckBox)
    end
    else Wnd := TacBtnWnd.Create(aHwnd, nil, ListSW.SkinData.SkinManager, s_Button)
  end
  else if (s = 'combobox') then begin
    if (GetWindowLong(aHwnd, GWL_STYLE) and CBS_OWNERDRAWFIXED = CBS_OWNERDRAWFIXED) then begin
      Wnd := TacComboBoxWnd.Create(aHwnd, nil, ListSW.SkinData.SkinManager, s_ComboBox)
    end
    else begin
      Wnd := TacComboBoxWnd.Create(aHwnd, nil, ListSW.SkinData.SkinManager, s_ComboBox)
    end;
    TacEditWnd(Wnd).DlgMode := True;
  end
  else if (s = 'combolbox') then begin
//    inc(lBoxCount);
    Wnd := TacEditWnd.Create(aHwnd, nil, ListSW.SkinData.SkinManager, s_ComboBox);
  end
  else if (s = 'comboboxex32') then begin
    Wnd := TacComboBoxWnd.Create(aHwnd, nil, ListSW.SkinData.SkinManager, s_ComboBox)
  end
  else if (s = 'scrollbar') then begin
    Wnd := TacSizerWnd.Create(aHwnd, nil, ListSW.SkinData.SkinManager, s_CheckBox)
   end
  else if (s = 'systabcontrol32') then begin
    if ((Style and TCS_OWNERDRAWFIXED) <> TCS_OWNERDRAWFIXED) then begin
      Wnd := TacTabControlWnd.Create(aHwnd, nil, ListSW.SkinData.SkinManager, s_PageControl);
    end;
//    GetWindowRect(aHwnd, R);
    if (WidthOf(R) = 470) and (HeightOf(R) = 349) then Result := False; // if Printer properties Dlg !!!
  end
  else if (s = 'syslistview32') then begin
    Wnd := TacListViewWnd.Create(aHwnd, nil, ListSW.SkinData.SkinManager, s_Edit);
    TacEditWnd(Wnd).DlgMode := True;
  end
  else if (s = 'msctls_updown32') then begin
    Wnd := TacSpinWnd.Create(aHwnd, nil, ListSW.SkinData.SkinManager, s_SpeedButton_Small);
    TacSpinWnd(Wnd).lOffset := 2;
  end
{  else if (s = 'shelldll_defview') then begin
    acSkinnedCtrls.Add(TacBtnWnd.Create(aHwnd, nil, ListSW.SkinData.SkinManager, s_DlgEdit));
  end}
  else if (s = 'listbox') then begin
    Wnd := TacEditWnd.Create(aHwnd, nil, ListSW.SkinData.SkinManager, s_Edit);
    TacEditWnd(Wnd).DlgMode := True;
  end
  else if (s = 'link window') or (s = 'syslink') then begin
    Wnd := TacLinkWnd.Create(aHwnd, nil, ListSW.SkinData.SkinManager, s_CheckBox);
  end
  else if (s = 'toolbarwindow32') then begin
    if GetWindowLong(aHwnd, GWL_STYLE) and TBSTYLE_WRAPABLE = TBSTYLE_WRAPABLE
      then Wnd := TacToolBarWnd.Create(aHwnd, nil, ListSW.SkinData.SkinManager, s_Bar)
      else Wnd := TacToolBarWnd.Create(aHwnd, nil, ListSW.SkinData.SkinManager, s_CheckBox);
  end
  else if (s = 'edit') then begin
    Wnd := TacEditWnd.Create(aHwnd, nil, ListSW.SkinData.SkinManager, s_Edit);
    TacEditWnd(Wnd).DlgMode := True;
  end
  else if (s = 'systreeview32') then begin
    Wnd := TacTreeViewWnd.Create(aHwnd, nil, ListSW.SkinData.SkinManager, s_Edit);
    TacEditWnd(Wnd).DlgMode := True;
  end
  else if (pos('shbrowseforfolder', s) > 0) then begin
    Wnd := TacTransPanelWnd.Create(aHwnd, nil, ListSW.SkinData.SkinManager, s_CheckBox)
  end;
  if Wnd <> nil then begin
    acSkinnedCtrls.Add(Wnd);
    InitCtrlData(CtrlHandle, Wnd.ParentWnd, Wnd.WndRect, Wnd.ParentRect, Wnd.WndSize, Wnd.WndPos, Wnd.Caption);
  end;
  acDlgMode := False;
end;

constructor TacProvider.Create(AOwner: TComponent);
begin
  inherited;
  BiDiLeft := False;
end;

destructor TacProvider.Destroy;
var
  i : integer;
begin
  if (sp = nil) then begin
    if acSkinnedCtrls <> nil then begin
      for i := 0 to acSkinnedCtrls.Count - 1 do if acSkinnedCtrls[i] <> nil then TObject(acSkinnedCtrls[i]).Free;
      FreeAndNil(acSkinnedCtrls);
    end;
    if (ListSW <> nil) then FreeAndNil(ListSW);
  end
  else for i := 0 to acSupportedList.Count - 1 do begin
    if acSupportedList[i] = Self then begin
      acSupportedList[i] := nil;
      Break;
    end;
  end;
  inherited;
end;

function TacProvider.FindCtrlInList(hwnd: THandle): TObject;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to acSkinnedCtrls.Count - 1 do begin
    if TacScrollWnd(acSkinnedCtrls[i]).CtrlHandle = hwnd then begin
      Result := TacScrollWnd(acSkinnedCtrls[i]);
      Break;
    end;
  end;
end;

procedure TacProvider.InitForm(Form: TCustomForm);
begin
  sp := TsSkinProvider.Create(Form);
  sp.Form := TForm(Form);
  if AeroIsEnabled then begin // Special initialization is required later
    SetWindowLong(sp.Form.Handle, GWL_STYLE, GetWindowLong(sp.Form.Handle, GWL_STYLE) or WS_VISIBLE);
    RedrawWindow(sp.Form.Handle, nil, 0, RDW_FRAME or RDW_ALLCHILDREN or RDW_INVALIDATE or RDW_ERASE or RDW_UPDATENOW);
  end;
  sp.MakeSkinMenu := (Form.BorderStyle in [bsSizeable, bsSingle]) and DefMakeSkinMenu;
end;

function TacProvider.InitHwndControls(hWnd : hwnd) : boolean;
var
  hCtrl : THandle;
begin
  Result := True;
  hCtrl := GetTopWindow(hWnd);
  while hCtrl <> 0 do begin
    if not InitHwndControls(hCtrl) then begin
      Result := False;
      Exit;
    end;
    if (GetWindowLong(hCtrl, GWL_STYLE) and WS_CHILD) = WS_CHILD then if not AddControl(hCtrl) then begin
      Result := False;
      Exit;
    end;
    hCtrl := GetNextWindow(hCtrl, GW_HWNDNEXT);
  end;
end;

function TacProvider.InitSkin(aHandle : hwnd) : boolean;
var
  xStyle: LongInt;
  WndClassName : string;
  Manager : TsSkinManager;
  i : integer;
begin
  Result := True;
  CtrlHandle := aHandle;
  acSkinnedCtrls := TList.Create;
  Manager := DefaultManager;
  if (ListSW = nil) and (Manager <> nil) and Manager.Active then begin
    style := GetWindowLong(CtrlHandle, GWL_STYLE);
    xStyle := GetWindowLong(CtrlHandle, GWL_EXSTYLE);
    BiDiLeft := (xStyle and WS_EX_LEFTSCROLLBAR) > 0;
    WndClassName := GetWndClassName(CtrlHandle);
    if (WndClassName = rsfName) or (WndClassName = s_TMessageForm) then begin
      InitDialog(CtrlHandle, ListSW);
      ListSW.Provider := Self;
      if (DlgLeft <> -1) or (DlgTop <> -1) then SetWindowPos(CtrlHandle, 0, DlgLeft, DlgTop, 0, 0, SWP_NOOWNERZORDER or SWP_NOREDRAW or SWP_NOSIZE);
      if InitHwndControls(CtrlHandle) then begin
        ListSW.InitParams;
        SendAMessage(CtrlHandle, AC_SETNEWSKIN, longword(DefaultManager));
      end
      else begin
        for i := 0 to acSupportedList.Count - 1 do begin
          if acSupportedList[i] = Self then acSupportedList[i] := nil;
        end;
        Result := False;
      end;
    end;
  end;
end;

procedure InitDialog(hwnd: THandle; var ListSW : TacDialogWnd);
begin
  if Assigned(DefaultManager) and DefaultManager.Active then begin
    UninitializeFlatSB(hwnd);
    if (ListSW <> nil) and ListSW.Destroyed then FreeAndNil(ListSW);
    if ListSW = nil then begin
      ListSW := TacDialogWnd.Create(hwnd, nil, DefaultManager, s_Dialog, False);
      ListSW.CtrlHandle := hwnd;
    end;
  end
  else begin
    if ListSW <> nil then FreeAndNil(ListSW);
    InitializeFlatSB(hwnd);
  end;
end;

procedure DrawAppIcon(ListSW : TacDialogWnd);
var
  SmallIcon: HIcon;
  IcoSize : TSize;
  x, y : integer;
begin
  if ListSW.TitleIcon <> 0 then begin
    IcoSize.cx := GetSystemMetrics(SM_CXSMICON);
    IcoSize.cy := GetSystemMetrics(SM_CYSMICON);
    x := ListSW.SysBorderWidth + ListSW.SkinData.SkinManager.SkinData.BILeftMargin;
    y := (ListSW.CaptionHeight + ListSW.SysBorderHeight - IcoSize.cy) div 2;
    SmallIcon := Windows.CopyImage(ListSW.TitleIcon, IMAGE_ICON, IcoSize.cx, IcoSize.cy, LR_COPYFROMRESOURCE);
    if SmallIcon <> 0 then begin
      DrawIconEx(ListSW.SkinData.FCacheBmp.Canvas.Handle, x, y, SmallIcon, IcoSize.cx, IcoSize.cy, 0, 0, DI_NORMAL);
      DestroyIcon(SmallIcon);
    end
    else DrawIconEx(ListSW.SkinData.FCacheBmp.Canvas.Handle, x, y, LoadIcon(0, IDI_APPLICATION), IcoSize.cx, IcoSize.cy, 0, 0, DI_NORMAL);
  end
end;

function GetWndText(hwnd: THandle): WideString;
var
  buf: array[0..1000] of char;
begin
  Result := '';
  if Win32Platform = VER_PLATFORM_WIN32_NT then begin
    SetLength(Result, GetWindowTextLengthW(hwnd) + 1);
    GetWindowTextW(hwnd, PWideChar(Result), Length(Result));
    SetLength(Result, Length(Result) - 1);
  end
  else begin
    SendMessage(hwnd, WM_GETTEXT, 1000, integer(@buf));
    Result := StrPas(buf);
  end;
end;

procedure FillArOR(ListSW : TacDialogWnd);
var
  i : integer;
begin
  SetLength(ListSW.ArOR, 0);
  if ListSW.SkinData.SkinManager.IsValidImgIndex(ListSW.SkinData.BorderIndex) then begin
    // TopBorderRgn
    AddRgn(ListSW.ArOR, ListSW.WndSize.cx, ListSW.SkinData.SkinManager.ma[ListSW.SkinData.BorderIndex], 0, False);
    // BottomBorderRgn
    AddRgn(ListSW.ArOR, ListSW.WndSize.cx, ListSW.SkinData.SkinManager.ma[ListSW.SkinData.BorderIndex], ListSW.WndSize.cy - ListSW.SkinData.SkinManager.ma[ListSW.SkinData.BorderIndex].WB, True);
  end;

  // TitleRgn
  i := ListSW.SkinData.SkinManager.GetSkinIndex(s_FormTitle);
  if ListSW.SkinData.SkinManager.IsValidSkinIndex(i) then begin
    i := ListSW.SkinData.SkinManager.GetMaskIndex(i, s_FormTitle, s_BordersMask);
    if ListSW.SkinData.SkinManager.IsValidImgIndex(i) then AddRgn(ListSW.ArOR, ListSW.WndSize.cx, ListSW.SkinData.SkinManager.ma[i], 0, False);
  end;
end;

procedure UpdateRgn(ListSW : TacDialogWnd; Repaint : boolean = True);
const
  BE_ID = $41A2;
  CM_BEWAIT = CM_BASE + $0C4D;
var
  rgn : HRGN;
begin
  if (ListSW.BorderStyle <> acbsNone) then with ListSW do begin
//    if not FirstInitialized then
    if SendMessage(CtrlHandle, CM_BEWAIT, BE_ID, 0) = BE_ID then Exit; // BE compatibility
    RgnChanging := True;
    rgn := GetRgnFromArOR(ListSW);
    SetWindowRgn(CtrlHandle, rgn, Repaint); // True - repainting required
  end;
end;

function GetRgnFromArOR(ListSW : TacDialogWnd; X : integer = 0; Y : integer = 0) : hrgn;
var
  l, i : integer;
  subrgn : HRGN;
begin
  l := Length(ListSW.ArOR);
  Result := CreateRectRgn(X, Y, ListSW.WndSize.cx + X, ListSW.WndSize.cy + Y);
  if l > 0 then for i := 0 to l - 1 do begin
    subrgn := CreateRectRgn(ListSW.ArOR[i].Left + X, ListSW.ArOR[i].Top + Y, ListSW.ArOR[i].Right + X, ListSW.ArOR[i].Bottom + Y);
    CombineRgn(Result, Result, subrgn, RGN_DIFF);
    DeleteObject(subrgn);
  end;
end;

{ TacDialogWnd }

procedure TacDialogWnd.acWndProc(var Message: TMessage);
var
  sw : TacMainWnd;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    WM_DESTROY, WM_NCDESTROY: begin
      if ((OldProc <> nil) or Assigned(OldWndProc)) then begin
        sw := Self;
        UninitializeACWnd(CtrlHandle, False, False, sw);
        Message.Result := CallPrevWndProc(CtrlHandle, Message.Msg, Message.WParam, Message.LParam);
      end;
      Exit;
    end;
  end;
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_SETNEWSKIN : begin
      if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
        SkinData.UpdateIndexes;
        if (SkinData.SkinManager <> nil) then UpdateIconsIndexes;
        if Adapter = nil then AdapterCreate;
        BroadCastHwnd(CtrlHandle, Message);
      end;
      Exit;
    end;
    AC_REFRESH : begin
      SkinData.UpdateIndexes;
      SkinData.Invalidate;
//      BroadCastHwnd(CtrlHandle, Message);
//      RedrawWindow(CtrlHandle, nil, 0, {RDW_ERASE or }RDW_FRAME or {RDW_INTERNALPAINT or }RDW_INVALIDATE {or RDW_UPDATENOW }or RDW_ALLCHILDREN);// or RDW_ERASENOW);
      Exit;
    end;
    AC_PREPARING : if (SkinData <> nil) then begin
      Message.Result := integer({SkinData.BGChanged or }SkinData.FUpdating);
      Exit;
    end;
    AC_UPDATING : begin
      SkinData.Updating := Message.WParamLo = 1;
      if SkinData.Updating then SkinData.BGChanged := True;
    end;
    AC_GETCONTROLCOLOR : begin
      Message.Result := GetBGColor(SkinData, 0);
      Exit;
    end;
    AC_GETBG : begin
      InitBGInfo(SkinData, PacBGInfo(Message.LParam), 0);
      if (PacBGInfo(Message.LParam)^.BgType = btCache) and not PacBGInfo(Message.LParam)^.PleaseDraw then begin
        PacBGInfo(Message.LParam)^.Offset.X := PacBGInfo(Message.LParam)^.Offset.X + OffsetX;
        PacBGInfo(Message.LParam)^.Offset.Y := PacBGInfo(Message.LParam)^.Offset.Y + OffsetY;
      end;
      Exit;
    end;
    AC_UPDATECHILDREN : Provider.InitHwndControls(CtrlHandle); // SysListView re-init
    AC_CHILDCHANGED : begin
      Message.Result := integer((SkinData.SkinManager.gd[SkinData.SkinIndex].GradientPercent + SkinData.SkinManager.gd[SkinData.SkinIndex].ImagePercent > 0));
      Exit;
    end;
    AC_PARENTCLOFFSET : begin Message.Result := MakeLong(Word(OffsetX), Word(OffsetY)); Exit end;
  end
  else case Message.Msg of
    WM_GETDLGCODE : Exit;
    WM_MOUSEMOVE : begin
      if IsWindowEnabled(CtrlHandle) then DefaultManager.ActiveControl := 0;
    end;
    WM_NCHITTEST : begin
      Ac_WMNCHitTest(Message);
      Exit;
    end;
    WM_MOUSELEAVE : SetHotHT(0);
    WM_NCLBUTTONDOWN : begin
      Ac_WMNCLButtonDown(TWMNCLButtonDown(Message));
      Exit;
    end;
    WM_NCRBUTTONDOWN : begin
      if not (TWMNCLButtonUp(Message).HitTest in [HTCAPTION, HTSYSMENU]) then begin
        inherited
      end
      else Exit;
    end;
    WM_CTLCOLORDLG : begin
      Message.Result := LongInt(CreateSolidBrush(DefaultManager.GetGlobalColor)); // 5.21
      Exit;
    end;
    WM_DRAWITEM : begin
      case TWMDrawItem(Message).DrawItemStruct.CtlType of
        ODT_COMBOBOX : begin end;
        ODT_STATIC : begin
          Ac_DrawStaticItem(TWMDrawItem(Message));
        end;
      end
    end;
    WM_NCRBUTTONUP : begin
      case TWMNCLButtonUp(Message).HitTest of
        HTCAPTION, HTSYSMENU : begin
          SetHotHT(0);
          DropSysMenu(TWMNCLButtonUp(Message).XCursor, TWMNCLButtonUp(Message).YCursor);
        end
      end;
      Exit;
    end;
    WM_NCLBUTTONUP, WM_LBUTTONUP: begin
      Ac_WMLButtonUp(Message);
      Exit;
    end;
    WM_NCLBUTTONDBLCLK : begin
      case TWMNCHitMessage(Message).HitTest of
        HTSYSMENU : SendMessage(CtrlHandle, WM_SYSCOMMAND, SC_CLOSE, 0);
        HTCAPTION : begin
          if EnabledMax or EnabledRestore then begin
            if IsZoomed(CtrlHandle) or IsIconic(CtrlHandle)
              then SendMessage(CtrlHandle, WM_SYSCOMMAND, SC_RESTORE, 0)
              else SendMessage(CtrlHandle, WM_SYSCOMMAND, SC_MAXIMIZE, 0);
            SystemMenu.UpdateItems;
          end;
          TWMNCHitMessage(Message).HitTest := 0;
        end;
      end;
      Exit;
    end;

    WM_ERASEBKGND : begin
      InitCtrlData(CtrlHandle, ParentWnd, WndRect, ParentRect, WndSize, WndPos, Caption);
      if not Initialized then begin
        Initialized := True;
        Provider.InitHwndControls(CtrlHandle); // Rezerved searching of controls
      end;
      Ac_WMPaint(TWMPaint(Message));
      Message.Result := 1;
      Exit;
    end;

    WM_PAINT : begin
      if not DwmInitialized then begin
        DwmInitialized := True;
        UseDwm(CtrlHandle, Skindata.Skinned);
      end;
      Message.Result := CallWindowProc(OldProc, CtrlHandle, Message.Msg, Message.WParam, Message.LParam);
      Exit;
    end;

    WM_NCPAINT : if IsWindowVisible(CtrlHandle) then begin
      InitCtrlData(CtrlHandle, ParentWnd, WndRect, ParentRect, WndSize, WndPos, Caption);
      Caption := GetWndText(CtrlHandle);
      Ac_WMNCPaint(Message);
      Message.Result := 1;
      Exit;
    end;

    WM_WINDOWPOSCHANGED : begin
      SkinData.BGChanged := True;
      RgnChanged := True;
      Message.Result := CallWindowProc(OldProc, CtrlHandle, Message.Msg, Message.WParam, Message.LParam); 
      Exit;
    end;

    WM_SIZE : begin
      SkinData.BGChanged := True;
      RgnChanged := True;
      Message.Result := CallWindowProc(OldProc, CtrlHandle, Message.Msg, Message.WParam, Message.LParam);
      RedrawWindow(CtrlHandle, nil, 0, RDW_ERASE or RDW_UPDATENOW or RDW_INVALIDATE);
      Exit;
    end;
    WM_SETTEXT : if IsWindowVisible(CtrlHandle) then begin
//      SendMessage(CtrlHandle, WM_SETREDRAW, 0, 0);
      Message.Result := CallWindowProc(OldProc, CtrlHandle, Message.Msg, Message.WParam, Message.LParam);
//      SendMessage(CtrlHandle, WM_SETREDRAW, 1, 0);
      SkinData.BGChanged := True;
      SendMessage(CtrlHandle, WM_NCPAINT, 0, 0);
      Exit;
    end;
    WM_ENABLE : if IsWindowVisible(CtrlHandle) then begin
//      SendMessage(CtrlHandle, WM_SETREDRAW, 0, 0);
      Message.Result := CallWindowProc(OldProc, CtrlHandle, Message.Msg, Message.WParam, Message.LParam);
//      SendMessage(CtrlHandle, WM_SETREDRAW, 1, 0);
      SkinData.BGChanged := True;
      SendMessage(CtrlHandle, WM_NCPAINT, 0, 0);
      Exit;
    end;
    WM_NCACTIVATE : if IsWindowVisible(CtrlHandle) then begin
      UseDwm(CtrlHandle, Skindata.Skinned);
      SendMessage(CtrlHandle, WM_SETREDRAW, 0, 0);
      Message.Result := CallWindowProc(OldProc, CtrlHandle, Message.Msg, Message.WParam, Message.LParam);
      FFormActive := (TWMActivate(Message).Active <> WA_INACTIVE);
      SendMessage(CtrlHandle, WM_SETREDRAW, 1, 0);
      Ac_WMNCActivate(Message);
      Exit;
    end;
{    WM_ACTIVATE : if IsWindowVisible(CtrlHandle) then begin
//      SendMessage(CtrlHandle, WM_SETREDRAW, 0, 0);
      Message.Result := CallWindowProc(OldProc, CtrlHandle, Message.Msg, Message.WParam, Message.LParam);
      FFormActive := (TWMActivate(Message).Active <> WA_INACTIVE);
//      SendMessage(CtrlHandle, WM_SETREDRAW, 1, 0);
      Ac_WMActivate(Message);
      Exit;
    end;   }
    1326 : begin
      Message.Result := CallWindowProc(OldProc, CtrlHandle, Message.Msg, Message.WParam, Message.LParam);
      Provider.InitHwndControls(CtrlHandle); // SysListView re-init
      RedrawWindow(CtrlHandle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW);
      Exit;
    end;
  end;
  inherited;
  case Message.Msg of
    WM_SYSCOMMAND : begin
//      if Message.WParamLo = $F012 then RedrawWindow(CtrlHandle, nil, 0, RDW_INTERNALPAINT or RDW_INVALIDATE or RDW_UPDATENOW);
      case Message.WParam of
        SC_MAXIMIZE, SC_RESTORE : begin
          if VisibleMax then CurrentHT := HTMAXBUTTON;
          SetHotHT(0);
        end;
      end;
    end;
  end;
end;

function TacDialogWnd.BarWidth(i: integer): integer;
begin
  if Assigned(SkinData.SkinManager.ma[i].Bmp) then begin
    Result := (SkinData.SkinManager.ma[i].Bmp.Width div 9) * 2 + TitleBtnsWidth;
  end
  else begin
    Result := (WidthOf(SkinData.SkinManager.ma[i].R) div (3 * SkinData.SkinManager.ma[i].ImageCount)) * 2 + TitleBtnsWidth;
  end;
end;

function TacDialogWnd.BorderWidth: integer;
begin
  Result := SysBorderHeight
end;

function TacDialogWnd.CaptionHeight: integer;
begin
  if (BorderStyle <> acbsNone) and (GetWindowLong(CtrlHandle, GWL_STYLE) and WS_CAPTION = WS_CAPTION) then begin
    if BorderStyle in [acbsToolWindow, acbsSizeToolWin]
      then Result := GetSystemMetrics(SM_CYSMCAPTION)
      else Result := GetSystemMetrics(SM_CYCAPTION)
  end
  else Result := 0;
end;

constructor TacDialogWnd.Create(AHandle: hwnd; ASkinData: TsCommonData; ASkinManager: TsSkinManager; const SkinSection : string; Repaint: boolean);
begin
  inherited;
  Initialized := False;
  DwmInitialized := False;
  RgnChanged := True;
  BorderStyle := acbsSingle;
  TempBmp := TBitmap.Create;
  FFormActive := True;

  SystemMenu := TacSystemMenu.Create(nil);
  SystemMenu.FOwner := Self;
  SystemMenu.UpdateItems;
  SkinData.Updating := True;
  if (DefaultManager <> nil) and DefaultManager.SkinnedPopups then DefaultManager.SkinableMenus.HookPopupMenu(SystemMenu, True);
end;

destructor TacDialogWnd.Destroy;
begin
  if Assigned(Adapter) then FreeAndNil(Adapter);
  if Assigned(TitleGlyph) then FreeAndNil(TitleGlyph);
  if Assigned(TitleBG) then FreeAndNil(TitleBG);
  if Assigned(TempBmp) then FreeAndnil(TempBmp);
  FreeAndNil(SystemMenu);
  if TitleFont <> nil then FreeAndnil(TitleFont);
  ClearGlows;
  inherited;
end;

procedure TacDialogWnd.Ac_WMPaint(var Msg: TWMPaint);
var
  DC, SavedDC : hdc;
begin
  if Msg.DC = 0 then DC := GetDC(CtrlHandle) else DC := Msg.DC;
  SavedDC := SaveDC(DC);
  try
    SkinData.Updating := False;
    PaintForm(DC);
  finally
    RestoreDC(DC, SavedDC);
    if Msg.DC = 0 then ReleaseDC(CtrlHandle, DC);
  end;
end;

function TacDialogWnd.EnabledClose: boolean;
begin
  Result := VisibleClose and (GetClassLong(CtrlHandle, GCL_STYLE) and CS_NOCLOSE <> CS_NOCLOSE)
end;

function TacDialogWnd.EnabledMax: boolean;
begin
  Result := VisibleMax and not IsZoomed(CtrlHandle) and (BorderStyle in [acbsSingle, acbsSizeable, acbsSizeToolWin]);
end;

function TacDialogWnd.EnabledMin: boolean;
begin
  Result := VisibleMin and not IsIconic(CtrlHandle);
end;

function TacDialogWnd.EnabledRestore: boolean;
begin
  Result := VisibleMax and (IsIconic(CtrlHandle) or IsZoomed(CtrlHandle));
end;

function TacDialogWnd.FormActive: integer;
begin
  Result := integer(FFormActive);
end;

function TacDialogWnd.HeaderHeight: integer;
begin
  if GetWindowLong(CtrlHandle, GWL_STYLE) and WS_CAPTION <> WS_CAPTION
    then Result := WndSize.cy - GetClientHeight(CtrlHandle)
    else Result := WndSize.cy - GetClientHeight(CtrlHandle) - SysBorderHeight;
  if Result < 0 then Result := 0;
  if IsIconic(CtrlHandle) then inc(Result, SysBorderHeight);
  if Assigned(sBarHorz) and sBarHorz.fScrollVisible then begin
    dec(Result, GetScrollMetric(sBarHorz, SM_CYHORZSB));
  end;
end;

procedure TacDialogWnd.InitParams;
var
  NonClientMetrics: TNonClientMetrics;
  f : HFONT;
begin
  dwStyle := GetWindowLong(CtrlHandle, GWL_STYLE);
  dwExStyle := GetWindowLong(CtrlHandle, GWL_EXSTYLE);
  BorderStyle := acbsSizeable;
  if ((dwStyle and WS_POPUP) = WS_POPUP) and ((dwStyle and WS_CAPTION) <> WS_Caption)
    then BorderStyle := acbsNone
    else if ((dwStyle and WS_THICKFRAME) = WS_THICKFRAME) or ((dwStyle and WS_SIZEBOX) = WS_SIZEBOX)
      then BorderStyle := acbsSizeable
      else if ((dwStyle and DS_MODALFRAME) = DS_MODALFRAME) then BorderStyle := acbsDialog;
  PrepareTitleGlyph;
  if TitleFont <> nil then FreeAndNil(TitleFont);
  TitleFont := TFont.Create;
  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then begin
    f := CreateFontIndirect(NonClientMetrics.lfCaptionFont);
    if f <> 0 then TitleFont.Handle := f
  end;
  SetWindowLong(CtrlHandle, GWL_STYLE, GetWindowLong(CtrlHandle, GWL_STYLE) and not WS_SYSMENU);
end;

procedure TacDialogWnd.MakeTitleBG;
begin
  if TitleBG <> nil then FreeAndNil(TitleBG);
  TitleBG := TBitmap.Create;
  TitleBG.Width := SkinData.FCacheBmp.Width;
  TitleBG.Height := CaptionHeight + SysBorderHeight;
  TitleBG.PixelFormat := pf24bit;
  BitBlt(TitleBG.Canvas.Handle, 0, 0, TitleBG.Width, TitleBG.Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
end;

function TacDialogWnd.OffsetX: integer;
var
  i : integer;
begin
  if Assigned(sBarVert) and sBarVert.fScrollVisible then i := GetScrollMetric(sBarVert, SM_CXVERTSB) else i := 0;
  Result := (GetWindowWidth(CtrlHandle) - GetClientWidth(CtrlHandle) - i) div 2
end;

function TacDialogWnd.OffsetY: integer;
var
  i : integer;
begin
  if Assigned(sBarHorz) and sBarHorz.fScrollVisible then i := GetScrollMetric(sBarHorz, SM_CYHORZSB) else i := 0;
  Result := GetWindowHeight(CtrlHandle) - GetClientHeight(CtrlHandle) - BorderWidth * integer(CaptionHeight <> 0) - i;
end;

procedure TacDialogWnd.PaintAll;
var
  i, CY : integer;
  h : integer;
  s : acString;
  r : TRect;
  ci : TCacheInfo;
  ts : TSize;
begin
  CY := SysBorderHeight;
  h := 2 * CY + CaptionHeight;

  if SkinData.BGChanged then begin
    if False {!!!} and
        IsBorderUnchanged(SkinData.BorderIndex, SkinData.SkinManager) and
          (TitleBG <> nil) and (TitleBG.Width = WndSize.cx) and (SkinData.FCacheBmp.Height = WndSize.cy) {and (CaptionHeight <> 0)} then begin
      // Drawn only caption
      ci := MakeCacheInfo(SkinData.FCacheBmp);
      BitBlt(SkinData.FCacheBmp.Canvas.Handle, 0, 0, TitleBG.Width, TitleBG.Height, TitleBG.Canvas.Handle, 0, 0, SRCCOPY);
    end
    else begin
      RgnChanged := True;
      ci.Ready := False;
      SkinData.FCacheBmp.Width := WndSize.cx;
      SkinData.FCacheBmp.Height := WndSize.cy;
      if SkinData.SkinManager.IsValidSkinIndex(SkinData.SkinIndex) then begin
        // Paint body
        PaintItem(SkinData, EmptyCI, False, integer(FormActive), Rect(0, 0, WndSize.cx, WndSize.cy), Point(0, 0), SkinData.FCacheBmp, False);

{        if (BorderStyle = acbsSizeable) or (BorderStyle = acbsSizeToolWin) then begin // PaintGrip
          i := SkinData.SkinManager.GetMaskIndex(SkinData.SkinManager.ConstData.IndexGLobalInfo, s_GlobalInfo, s_GripImage);
          if SkinData.SkinManager.IsValidImgIndex(i)
            then DrawSkinGlyph(SkinData.FCacheBmp, RBGripPoint(i), 0, 1, SkinData.SkinManager.ma[i], MakeCacheInfo(SkinData.FCacheBmp));
        end;}

        ci := MakeCacheInfo(SkinData.FCacheBmp, OffsetX, OffsetY); // Prepare cache info
        if (CaptionHeight <> 0) then begin // Paint title
          i := SkinData.SkinManager.GetSkinIndex(s_FormTitle);
          if SkinData.SkinManager.IsValidSkinIndex(i) then if IsIconic(CtrlHandle)
            then PaintItem(i, s_FormTitle, ci, True, integer(FormActive),
              Rect(0, 0, WndSize.cx, WndSize.cy), Point(0, 0), SkinData.FCacheBmp, SkinData.SkinManager)
            else PaintItem(i, s_FormTitle, ci, True, integer(FormActive),
              Rect(0, 0, WndSize.cx, h - CY), Point(0, 0), SkinData.FCacheBmp, SkinData.SkinManager);
          DrawAppIcon(Self); // Draw app icon
          if dwStyle and WS_SYSMENU = WS_SYSMENU then begin // Paint title toolbar if exists
            i := SkinData.SkinManager.GetMaskIndex(SkinData.SkinIndex, SkinData.SkinSection, s_NormalTitleBar);
            if SkinData.SkinManager.IsValidImgIndex(i) then DrawSkinRect(SkinData.FCacheBmp,
                        Rect(WndSize.cx - BarWidth(i), 0, SkinData.FCacheBmp.Width, h - CY),
                        True, EmptyCI, SkinData.SkinManager.ma[i], integer(FormActive), True);
          end;
          // Store bg for buttons
          TempBmp.Width := TitleBtnsWidth + SysBorderWidth;
          TempBmp.Height := HeaderHeight;
          BitBlt(TempBmp.Canvas.Handle, 0, 0, TempBmp.Width, TempBmp.Height, SkinData.FCacheBmp.Canvas.Handle, WndSize.cx - TempBmp.Width - 1, 0, SRCCOPY);
          // Paint buttons
          PaintBorderIcons;
          // Out the title text
          SkinData.FCacheBmp.Canvas.Font.Assign(TitleFont);
          R := Rect(SysBorderWidth + GetSystemMetrics(SM_CXSMICON) * integer(TitleIcon <> 0) + 4 + SkinData.SkinManager.SkinData.BILeftMargin, CY, WndSize.cx - TitleBtnsWidth - 6, CaptionHeight);
          if not IsRectEmpty(R) then begin
{$IFDEF TNTUNICODE}
            s := Caption;
{$ELSE}
            s := CutText(SkinData.FCacheBmp.Canvas, Caption, WndSize.cx - GetSystemMetrics(SM_CXSMICON) - SysBorderWidth - TitleBtnsWidth - 6);
{$ENDIF}
            GetTextExtentPoint32(SkinData.FCacheBmp.Canvas.Handle, PChar(s), Length(s), ts);
            R.Top := R.Top + (HeightOf(R) - ts.cy) div 2;
            R.Bottom := R.Top + ts.cy;
            acWriteTextEx(SkinData.FCacheBmp.Canvas, PacChar(s), True, R, DT_LEFT or DT_SINGLELINE or DT_VCENTER, SkinData.SkinManager.GetSkinIndex(s_FormTitle), boolean(FormActive));
          end;
        end;
        if IsBorderUnchanged(SkinData.BorderIndex, SkinData.SkinManager) and ((TitleBG = nil) or (TitleBG.Width <> WndSize.cx))
          then MakeTitleBG;
      end;
    end;
    SkinData.BGChanged := False
  end;
end;

procedure TacDialogWnd.PaintBorderIcons;
var
  i, b, Offset : integer;
  procedure PaintButton(var Btn : TsCaptionButton; var Index : integer; SkinIndex : integer; BtnEnabled : boolean; UserBtn : TsTitleButton = nil);
  var
    w, btnH : integer;
  begin
    w := SysButtonWidth(Btn);
    Btn.Rect.Left := Btn.Rect.Right - w;
    if Btn.HaveAlignment then begin // If not user button and not small
      case SkinData.SkinManager.SkinData.BIVAlign of
        -1, 0 : begin  // Center vert layout
          Btn.Rect.Top := (CaptionHeight - ButtonHeight(Btn.ImageIndex) + SysBorderHeight) div 2;
          if ButtonHeight(Btn.ImageIndex) < 16 then inc(Btn.Rect.Top, 2);
          Btn.Rect.Bottom := Btn.Rect.Top + ButtonHeight(Btn.ImageIndex);
        end;
        1 : begin  // Top layout
          Btn.Rect.Top := iffi(IsZoomed(CtrlHandle), SysBorderHeight - 1, 0);
          Btn.Rect.Bottom := Btn.Rect.Top + ButtonHeight(Btn.ImageIndex);
        end;
      end;
    end
    else begin
      dec(Btn.Rect.Left, 2);
      btnH := ButtonHeight(Btn.ImageIndex);
      Btn.Rect.Top := (CaptionHeight - btnH + SysBorderHeight) div 2;
//      if btnH < 16 then inc(Btn.Rect.Top, 2);
      Btn.Rect.Bottom := Btn.Rect.Top + btnH;
    end;
    if SkinIndex > -1 then DrawSkinGlyph(SkinData.FCacheBmp, Point(Btn.Rect.Left, Btn.Rect.Top),
      Btn.State, 1 + integer(not boolean(FormActive) or not BtnEnabled), SkinData.SkinManager.ma[SkinIndex], MakeCacheInfo(SkinData.FCacheBmp));
    inc(Index);
  end;
begin
  b := 1;
  Offset := WndSize.cx - SysBorderWidth - SkinData.SkinManager.SkinData.BIRightMargin;
  if (dwStyle and WS_SYSMENU = WS_SYSMENU) then begin // Accommodation of a buttons in a special order...
    if SkinData.SkinManager.IsValidImgIndex(ButtonClose.ImageIndex) then begin
      ButtonClose.Rect.Right := Offset;
      PaintButton(ButtonClose, b, ButtonClose.ImageIndex, EnabledClose);
      Offset := ButtonClose.Rect.Left;
    end;
    if VisibleMax then begin
      if not IsZoomed(CtrlHandle) then begin
        if SkinData.SkinManager.IsValidImgIndex(ButtonMax.ImageIndex) then begin
          ButtonMax.Rect.Right := Offset;
          PaintButton(ButtonMax, b, ButtonMax.ImageIndex, EnabledMax);
          Offset := ButtonMax.Rect.Left;
        end;
      end
      else begin
        i := SkinData.SkinManager.GetMaskIndex(SkinData.SkinManager.ConstData.IndexGLobalInfo, s_GlobalInfo, s_BorderIconNormalize);
        if i < 0 then i := SkinData.SkinManager.GetMaskIndex(SkinData.SkinIndex, SkinData.SkinSection, s_BorderIconNormalize); // For compatibility
        if i > -1 then begin
          ButtonMax.Rect.Right := Offset;
          PaintButton(ButtonMax, b, i, EnabledRestore);
          Offset := ButtonMax.Rect.Left;
        end;
      end;
    end;
    if VisibleMin then begin
      if IsIconic(CtrlHandle) then begin // If form is minimized then changing to Normalize
        i := SkinData.SkinManager.GetMaskIndex(SkinData.SkinManager.ConstData.IndexGLobalInfo, s_GlobalInfo, s_BorderIconNormalize);
        if i < 0 then i := SkinData.SkinManager.GetMaskIndex(SkinData.SkinIndex, SkinData.SkinSection, s_BorderIconNormalize);
        if SkinData.SkinManager.IsValidImgIndex(i) then begin
          ButtonMin.Rect.Right := Offset;
          PaintButton(ButtonMin, b, i, EnabledRestore); // For compatibility
          Offset := ButtonMin.Rect.Left;
        end;
      end
      else begin
        if SkinData.SkinManager.IsValidImgIndex(ButtonMin.ImageIndex) then begin
          ButtonMin.Rect.Right := Offset;
          PaintButton(ButtonMin, b, ButtonMin.ImageIndex, EnabledMin);
          Offset := ButtonMin.Rect.Left;
        end;
      end;
    end;
    if VisibleHelp then begin
      if SkinData.SkinManager.IsValidImgIndex(ButtonHelp.ImageIndex) then begin
        ButtonHelp.Rect.Right := Offset;
        PaintButton(ButtonHelp, b, ButtonHelp.Imageindex, True);
      end;
    end;
  end;
end;

procedure TacDialogWnd.PaintForm(var DC: hdc; SendUpdated : boolean = True);
begin
  PaintAll;
  BitBlt(DC, 0, 0, WndSize.cx, WndSize.cy, SkinData.FCacheBmp.Canvas.Handle, BorderWidth, HeaderHeight, SRCCOPY);
  SetParentUpdated(CtrlHandle);
end;

procedure TacDialogWnd.PrepareTitleGlyph;
var
  SmallIcon: HIcon;
  cx, cy: Integer;
  Bmp : TBitmap;
begin
  Bmp := TBitmap.Create;
  cx := GetSystemMetrics(SM_CXSMICON);
  cy := GetSystemMetrics(SM_CYSMICON);
  Bmp.Width := cx;
  Bmp.Height := cy;
  Bmp.Canvas.Brush.Color := clFuchsia;
  TitleIcon := hIcon(SendMessage(CtrlHandle, WM_GETICON, ICON_SMALL, 0));
  if TitleIcon = 0 then TitleIcon := hIcon(SendMessage(CtrlHandle, WM_GETICON, ICON_BIG, 0));

  if TitleIcon <> 0 then begin
    SmallIcon := Windows.CopyImage(TitleIcon, IMAGE_ICON, cx, cy, LR_COPYFROMRESOURCE);
    DrawIconEx(Bmp.Canvas.Handle, 0, 0, SmallIcon, cx, cy, 0, 0, DI_NORMAL);
    DestroyIcon(SmallIcon);
    if TitleGlyph = nil then TitleGlyph := TBitmap.Create;
    TitleGlyph.Assign(Bmp);
  end;
  FreeAndNil(Bmp);
end;

{function TacDialogWnd.RBGripPoint(ImgIndex: integer): TPoint;
begin
  if SkinData.SkinManager.ma[ImgIndex].Bmp = nil then begin
    Result := Point(SkinData.FCacheBmp.Width - WidthOf(SkinData.SkinManager.ma[ImgIndex].R) div SkinData.SkinManager.ma[ImgIndex].ImageCount - SysBorderWidth,
                    SkinData.FCacheBmp.Height - HeightOf(SkinData.SkinManager.ma[ImgIndex].R) div (1 + SkinData.SkinManager.ma[ImgIndex].MaskType) - SysBorderHeight
              );
  end
  else begin
    Result := Point(SkinData.FCacheBmp.Width - SkinData.SkinManager.ma[ImgIndex].Bmp.Width div 3 - SysBorderWidth,
                    SkinData.FCacheBmp.Height - SkinData.SkinManager.ma[ImgIndex].Bmp.Height div 2 - SysBorderHeight
              );
  end;
end;}

function TacDialogWnd.SysBorderHeight: integer;
begin
  Result := 0;
  if BorderStyle = acbsnone then Exit;
  if BorderStyle in [acbsToolWindow, acbsSingle, acbsDialog]
    then Result := GetSystemMetrics(SM_CYFIXEDFRAME)
    else Result := GetSystemMetrics(SM_CYSIZEFRAME);
end;

function TacDialogWnd.SysBorderWidth: integer;
begin
  Result := 0;
  if BorderStyle = acbsnone then Exit;
  if BorderStyle in [acbsToolWindow, acbsSingle, acbsDialog]
    then Result := GetSystemMetrics(SM_CXFIXEDFRAME)
    else Result := GetSystemMetrics(SM_CXSIZEFRAME);
end;

function TacDialogWnd.SysButtonWidth(Btn: TsCaptionButton): integer;
begin
  if SkinData.SkinManager.IsValidImgIndex(Btn.ImageIndex) then begin
    if SkinData.SkinManager.ma[Btn.ImageIndex].Bmp = nil
     then Result := WidthOf(SkinData.SkinManager.ma[Btn.ImageIndex].R) div SkinData.SkinManager.ma[Btn.ImageIndex].ImageCount
     else Result := SkinData.SkinManager.ma[Btn.ImageIndex].Bmp.Width div SkinData.SkinManager.ma[Btn.ImageIndex].ImageCount;
  end
  else Result := 21;
end;

function TacDialogWnd.TitleBtnsWidth: integer;
begin
  Result := 0;
  if VisibleClose then begin
    inc(Result, SysButtonWidth(ButtonClose));
    if VisibleMax then inc(Result, SysButtonWidth(ButtonMax));
    if VisibleMin then inc(Result, SysButtonWidth(ButtonMin));
    if VisibleHelp then inc(Result, SysButtonWidth(ButtonHelp));
  end;
end;

function TacDialogWnd.VisibleClose: boolean;
begin
  Result := dwStyle and WS_SYSMENU = WS_SYSMENU                
end;

function TacDialogWnd.VisibleHelp: boolean;
begin
  Result := dwExStyle and WS_EX_CONTEXTHELP = WS_EX_CONTEXTHELP
end;

function TacDialogWnd.VisibleMax: boolean;
begin
  Result := dwStyle and WS_MAXIMIZEBOX = WS_MAXIMIZEBOX

end;

function TacDialogWnd.VisibleMin: boolean;
begin
  Result := dwStyle and WS_MINIMIZEBOX = WS_MINIMIZEBOX
end;

procedure TacDialogWnd.Ac_WMNCPaint(var Message: TMessage);
var
  DC, SavedDC : hdc;
  i : integer;
begin
  Provider.InitHwndControls(CtrlHandle);

  GetWindowRect(CtrlHandle, WndRect);
  WndSize.cx := WidthOf(WndRect);
  WndSize.cy := HeightOf(WndRect);

  if not RgnChanging and RgnChanged and ((BorderStyle <> acbsNone) or (dwStyle and WS_SIZEBOX = WS_SIZEBOX)) then begin
    FillArOR(Self);
    RgnChanged := False;
    if not RgnChanging then UpdateRgn(Self);
  end;

  DC := GetWindowDC(CtrlHandle);
  SavedDC := SaveDC(DC);
  try
    if (BorderStyle = acbsNone) and (dwStyle and WS_SIZEBOX = WS_SIZEBOX) then begin
      if SkinData.BGChanged then begin
        PaintAll;
        SkinData.BGChanged := False;
        SkinData.Updating := False;
      end;
      i := 3;
      // Title and menu line update
      BitBlt(DC, 0, 0, WndSize.cx, i, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
      // Left border update
      BitBlt(DC, 0, i, i, SkinData.FCacheBmp.Height, SkinData.FCacheBmp.Canvas.Handle, 0, i, SRCCOPY);
      // Bottom border update
      BitBlt(DC, i, WndSize.cy - i, WndSize.cx - i, i, SkinData.FCacheBmp.Canvas.Handle, i, WndSize.cy - i, SRCCOPY);
      // Right border update
      BitBlt(DC, SkinData.FCacheBmp.Width - i, i, i, SkinData.FCacheBmp.Height, SkinData.FCacheBmp.Canvas.Handle, SkinData.FCacheBmp.Width - i, i, SRCCOPY);
    end
    else PaintCaption(DC);
  finally
    RestoreDC(DC, SavedDC);
    ReleaseDC(CtrlHandle, DC);
  end;
  SkinData.Updating := False;//SkinData.Updating;

  RgnChanging := False;
end;

procedure TacDialogWnd.PaintCaption(dc: hdc);
var
  h, bw, bh : integer;
begin
  h := SysBorderHeight + CaptionHeight;
  if IsIconic(CtrlHandle) then inc(h);
  if SkinData.BGChanged then begin
    PaintAll;
    SkinData.BGChanged := False;
    SkinData.Updating := False;
  end;

  bw := BorderWidth;
  bh := BorderHeight;
  // Title update
  BitBlt(DC, 0, 0, WndSize.cx, HeaderHeight, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
  // Left border update
  BitBlt(DC, 0, h, SysBorderwidth, SkinData.FCacheBmp.Height, SkinData.FCacheBmp.Canvas.Handle, 0, h, SRCCOPY);

  // Bottom border update
  BitBlt(DC, bw, WndSize.cy - bh, WndSize.cx - bw, bh,
             SkinData.FCacheBmp.Canvas.Handle,
             SysBorderwidth,
             WndSize.cy - SysBorderHeight, SRCCOPY);
  // Right border update
  BitBlt(DC, SkinData.FCacheBmp.Width - SysBorderWidth, h{ + BorderWidth}, SysBorderHeight, SkinData.FCacheBmp.Height, SkinData.FCacheBmp.Canvas.Handle, SkinData.FCacheBmp.Width - SysBorderwidth, h, SRCCOPY);

// v6.0  if Assigned(acMagnForm) then SendMessage(acMagnForm.Handle, SM_ALPHACMD, MakeWParam(0, AC_REFRESH), 0);
end;

function TacDialogWnd.BorderHeight: integer;
begin
  Result := SysBorderHeight
end;

procedure TacDialogWnd.UpdateIconsIndexes;
begin
  if SkinData.SkinManager.IsValidSkinIndex(SkinData.SkinManager.ConstData.IndexGlobalInfo) then with SkinData.SkinManager do begin
    if BorderStyle in [acbsSingle, acbsSizeable] then begin
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
    end;
  end;
end;

function TacDialogWnd.ButtonHeight(Index: integer): integer;
begin
  if SkinData.SkinManager.IsValidImgIndex(Index) then begin
    if SkinData.SkinManager.ma[Index].Bmp = nil
      then Result := HeightOf(SkinData.SkinManager.ma[Index].R) div (1 + SkinData.SkinManager.ma[Index].MaskType)
      else Result := SkinData.SkinManager.ma[Index].Bmp.Height div 2;
  end
  else Result := 21;
end;

procedure TacDialogWnd.AdapterCreate;
begin
end;

procedure TacDialogWnd.AdapterRemove;
begin
  SendToAdapter(MakeMessage(SM_ALPHACMD, MakeWParam(0, AC_REMOVESKIN), LongWord(SkinData.SkinManager), 0));
  FreeAndNil(Adapter);
end;

procedure TacDialogWnd.SendToAdapter(Message: TMessage);
begin
  if Assigned(Adapter) then Adapter.WndProc(Message)
end;

function TacDialogWnd.VisibleRestore: boolean;
begin
  Result := not (BorderStyle in [acbsDialog, acbsNone, acbsSizeToolWin, acbsToolWindow]) and VisibleClose;
end;

procedure TacDialogWnd.Ac_WMNCHitTest(var Message: TMessage);
begin
  Message.Result := HTProcess(TWMNCHitTest(Message));
  case Message.Result of
    Windows.HTCAPTION, Windows.HTNOWHERE : begin
      Message.Result := CallWindowProc(OldProc, CtrlHandle, Message.Msg, Message.WParam, Message.LParam);
      SetHotHT(0);
    end;
  end;
end;

function TacDialogWnd.HTProcess(var Message: TWMNCHitTest): integer;
const
  BtnSpacing = 1;
var
  p : TPoint;
  cy1, cy2 : integer;
  SysBtnCount, BtnIndex : integer;
  b : boolean;
  function GetBtnIndex(x : integer) : integer;
  var
    c : integer;
  begin
    Result := 0;
    c := 0;
    if VisibleClose then begin
      inc(c);
      if Between(x, ButtonClose.Rect.Left, ButtonClose.Rect.Right) then begin
        Result := c;
        Exit;
      end;
      if VisibleMax then begin
        inc(c);
        if Between(x, ButtonMax.Rect.Left, ButtonMax.Rect.Right) then begin
          Result := c;
          Exit;
        end;
      end;
      if VisibleMin then begin
        inc(c);
        if Between(x, ButtonMin.Rect.Left, ButtonMin.Rect.Right) then begin
          Result := c;
          Exit;
        end;
      end;
      if VisibleHelp then begin
        inc(c);
        if Between(x, ButtonHelp.Rect.Left, ButtonHelp.Rect.Right) then begin
          Result := c;
          Exit;
        end;
      end;
    end;
  end;
begin
  p := CursorToPoint(Message.XPos, Message.YPos);
  cy1 := (CaptionHeight - ButtonHeight(ButtonClose.ImageIndex) + SysBorderHeight) div 2;
  cy2 := cy1 + ButtonHeight(ButtonClose.ImageIndex);

  if Between(p.y, cy1, cy2) then begin // If in buttons
    if Between(p.x, SysBorderWidth, SysBorderWidth + GetSystemMetrics(SM_CXSMICON)) // If system menu icon
      then begin SetHotHT(HTSYSMENU); Result := HTSYSMENU; Exit; end;
    // Title button?
    SysBtnCount := 0;
    if VisibleClose then inc(SysBtnCount);
    if VisibleMax then inc(SysBtnCount);
    if VisibleMin or IsIconic(CtrlHandle) then inc(SysBtnCount);
    if VisibleHelp then inc(SysBtnCount);
    BtnIndex := GetBtnIndex(p.x);

    if (BtnIndex > 0) and (BtnIndex <= SysBtnCount) then begin // If system button
      case BtnIndex of
        1 : if VisibleClose then begin
          SetHotHT(HTCLOSE); Result := HTCLOSE; Exit;
        end;
        2 : begin
          if VisibleMax then begin
            if (EnabledMax or EnabledRestore) then begin
              SetHotHT(HTMAXBUTTON); Result := HTMAXBUTTON; Exit;
            end
            else begin
              SetHotHT(HTCAPTION); Result := HTCAPTION; Exit;
            end;
          end
          else if VisibleMin or IsIconic(CtrlHandle) then begin
            if EnabledMin then begin
              SetHotHT(HTMINBUTTON); Result := HTMINBUTTON; Exit;
            end
            else begin
              SetHotHT(HTCAPTION); Result := HTCAPTION; Exit;
            end;
          end
          else if VisibleHelp then begin
            SetHotHT(HTHELP); Result := HTHELP; EXIT;
          end;
        end;
        3 : begin
          if (VisibleMin) or IsIconic(CtrlHandle) then begin
            if not IsIconic(CtrlHandle) then begin
              if EnabledMin then begin
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
          else if VisibleHelp then begin
            SetHotHT(HTHELP); Result := HTHELP; EXIT;
          end;
        end;
        4 : if VisibleHelp and VisibleMax then begin
          SetHotHT(HTHELP); Result := HTHELP; EXIT;
        end;
      end;
    end
    else begin
      Result := HTCAPTION;
      Exit;
    end;
  end(*
  else begin
    if (BorderStyle = acbsSizeable) or (BorderStyle = acbsSizeToolWin) then begin
      i := SkinData.SkinManager.GetMaskIndex(SkinData.SkinManager.ConstData.IndexGLobalInfo, s_GlobalInfo{SkinData.SkinSection}, s_GripImage);
      if SkinData.SkinManager.IsValidImgIndex(i) then begin
        if (p.y > RBGripPoint(i).y) and (p.x > RBGripPoint(i).x) then begin
          Result := HTBOTTOMRIGHT; Exit;
        end;
      end;
    end;
  end*);
  b := IsZoomed(CtrlHandle);
  if b and AboveBorder(Message) then Result := HTTRANSPARENT else Result := Message.Result;
end;

procedure TacDialogWnd.SetHotHT(i: integer; Repaint: boolean);
begin
  if (CurrentHT = i) then Exit;
  if (CurrentHT <> 0) then begin
    case CurrentHT of
      HTCLOSE : ButtonClose.State := 0;
      HTMAXBUTTON : ButtonMax.State := 0;
      HTMINBUTTON : ButtonMin.State := 0;
      HTHELP : ButtonHelp.State := 0;
    end;
    if Repaint then RepaintButton(CurrentHT);
  end;
  CurrentHT := i;
  case CurrentHT of
    HTCLOSE : ButtonClose.State := 1;
    HTMAXBUTTON : ButtonMax.State := 1;
    HTMINBUTTON : ButtonMin.State := 1;
    HTHELP : ButtonHelp.State := 1;
  end;
  biClicked := False;
  if Repaint then RepaintButton(CurrentHT);
end;

function TacDialogWnd.CursorToPoint(x, y: integer): TPoint;
begin
  GetWindowRect(CtrlHandle, WndRect);
  Result := WndRect.TopLeft;
  Result.x := x - Result.x;
  Result.y := y - Result.y;
end;

function TacDialogWnd.AboveBorder(Message: TWMNCHitTest): boolean;
var
  p : TPoint;
begin
  p := CursorToPoint(Message.XPos, Message.YPos);
  Result := not PtInRect(Rect(2, 2, WndSize.cx - 4, WndSize.cy - 4), p);
  if Result then SetHotHT(0);
end;

procedure TacDialogWnd.RepaintButton(i: integer);
var
  DC, SavedDC : hdc;
  CurButton : ^TsCaptionButton;
  cx, ind, x, y : integer;
  BtnDisabled : boolean;
  R : TRect;
begin
  CurButton := nil;
  case i of
    HTCLOSE      : CurButton := @ButtonClose;
    HTMAXBUTTON  : CurButton := @ButtonMax;
    HTMINBUTTON  : CurButton := @ButtonMin;
    HTHELP       : CurButton := @ButtonHelp;
  end;
  if (CurButton <> nil) and (CurButton^.State <> -1) then begin
    BtnDisabled := False;
    if CurButton^.Rect.Left <= GetSystemMetrics(SM_CXSMICON) + BorderWidth then Exit;
    cx := SkinData.FCacheBmp.Width - CurButton^.Rect.Left;
    BitBlt(SkinData.FCacheBmp.Canvas.Handle, // Restore a button BG
           CurButton^.Rect.Left, CurButton^.Rect.Top, SysButtonwidth(CurButton^), ButtonHeight(CurButton^.ImageIndex),
           TempBmp.Canvas.Handle, TempBmp.Width - cx, CurButton^.Rect.Top, SRCCOPY);
    // if Max btn and form is maximized then Norm btn
    if (i = HTMAXBUTTON) and IsZoomed(CtrlHandle) then ind := SkinData.SkinManager.GetMaskIndex(SkinData.SkinIndex, s_GlobalInfo, s_BorderIconNormalize)
    else if IsIconic(CtrlHandle) then begin
      case i of
        HTMINBUTTON : begin
          ind := SkinData.SkinManager.GetMaskIndex(SkinData.SkinManager.ConstData.IndexGlobalInfo, s_GlobalInfo, s_BorderIconNormalize);
          if ind < 0 then ind := SkinData.SkinManager.GetMaskIndex(SkinData.SkinIndex, SkinData.SkinSection, s_BorderIconNormalize); // For compatibility
        end;
        HTMAXBUTTON : begin
          ind := SkinData.SkinManager.GetMaskIndex(SkinData.SkinManager.ConstData.IndexGlobalInfo, s_GlobalInfo, s_BorderIconMaximize);
          if ind < 0 then ind := SkinData.SkinManager.GetMaskIndex(SkinData.SkinIndex, SkinData.SkinSection, s_BorderIconMaximize); // For compatibility
          if not EnabledMax then BtnDisabled := True;
        end
        else ind := CurButton^.ImageIndex;
      end
    end else ind := CurButton^.ImageIndex;
    if SkinData.SkinManager.IsValidImgIndex(ind) then begin // Drawing of the button from skin
      if i < HTUDBTN // if not user defined
        then DrawSkinGlyph(SkinData.FCacheBmp, Point(CurButton^.Rect.Left, CurButton^.Rect.Top),
               CurButton^.State, 1 + integer(not boolean(FormActive) or BtnDisabled) * integer(not (CurButton^.State > 0) or BtnDisabled), SkinData.SkinManager.ma[ind], MakeCacheInfo(SkinData.FCacheBmp));
    end;
    // Copying to form
    DC := GetWindowDC(CtrlHandle);
    SavedDC := SaveDC(DC);
    try
      BitBlt(DC, CurButton^.Rect.Left, CurButton^.Rect.Top, WidthOf(CurButton^.Rect), HeightOf(CurButton^.Rect),
        SkinData.FCacheBmp.Canvas.Handle, CurButton^.Rect.Left, CurButton^.Rect.Top, SRCCOPY);
      if (CurButton^.State = 1) and (i in [HTCLOSE, HTMAXBUTTON, HTMINBUTTON]) then begin
        case i of
          HTCLOSE      : x := SkinData.SkinManager.SkinData.BICloseGlow;
          HTMAXBUTTON  : x := SkinData.SkinManager.SkinData.BIMaxGlow;
          HTMINBUTTON  : x := SkinData.SkinManager.SkinData.BIMinGlow;
        end;
        if x > 0 then begin
          case i of
            HTCLOSE      : y := SkinData.SkinManager.SkinData.BICloseGlowMargin;
            HTMAXBUTTON  : y := SkinData.SkinManager.SkinData.BIMaxGlowMargin;
            HTMINBUTTON  : y := SkinData.SkinManager.SkinData.BIMinGlowMargin;
          end;
          GetWindowRect(CtrlHandle, R);
          OffsetRect(R, CurButton^.Rect.Left, CurButton^.Rect.Top);
          R.Right := R.Left + WidthOf(CurButton^.Rect);
          R.Bottom := R.Top + HeightOf(CurButton^.Rect);

          if SkinData.SkinManager.AllowGlowing then
            CurButton^.GlowID := ShowGlow(R, R, s_GlobalInfo, SkinData.SkinManager.ma[CurButton.ImageIndex].PropertyName + s_Glow, y, 255, CtrlHandle, SkinData.SkinManager);
        end;
      end
      else if CurButton^.GlowID <> -1 then begin
        HideGlow(CurButton^.GlowID);
        CurButton^.GlowID := -1;
      end;
    finally
      RestoreDC(DC, SavedDC);
      ReleaseDC(CtrlHandle, DC);
    end;
  end
  else if (CurButton <> nil) and (CurButton^.GlowID <> -1) then begin
    HideGlow(CurButton^.GlowID);
    CurButton^.GlowID := -1;
  end;
end;

procedure TacDialogWnd.Ac_WMNCLButtonDown(var Message: TWMNCLButtonDown);
begin
  case TWMNCLButtonDown(Message).HitTest of
    HTCLOSE, HTMAXBUTTON, HTMINBUTTON, HTHELP, HTCHILDCLOSE..HTCHILDMIN : begin
      SetPressedHT(TWMNCLButtonDown(Message).HitTest);
    end;
    HTSYSMENU : begin
      if True{SkinData.SkinManager.SkinnedPopups} then begin
        SetHotHT(0);
        DropSysMenu(WndRect.Left + SysBorderWidth, WndRect.Top + BorderHeight + GetSystemMetrics(SM_CYSMICON));
      end
      else Message.Result := CallWindowProc(OldProc, CtrlHandle, Message.Msg, TMessage(Message).WParam, TMessage(Message).LParam);
    end
    else begin
      if IsIconic(CtrlHandle) then begin
        SendMessage(CtrlHandle, WM_SYSCOMMAND, $F012, 0);
      end
      else begin
        SetHotHT(0);
        if not IsZoomed(CtrlHandle) or (CursorToPoint(0, TWMNCLButtonDown(Message).YCursor).y > SysBorderHeight + CaptionHeight) then begin
          Message.Result := CallWindowProc(OldProc, CtrlHandle, Message.Msg, TMessage(Message).WParam, TMessage(Message).LParam);
        end
        else if FormActive = 0 then SetFocus(CtrlHandle); 
      end;
    end
  end;
end;

procedure TacDialogWnd.SetPressedHT(i: integer);
begin
  if (CurrentHT <> i) and (CurrentHT <> 0) then begin
    case CurrentHT of
      HTCLOSE : ButtonClose.State := 0;
      HTMAXBUTTON : ButtonMax.State := 0;
      HTMINBUTTON : ButtonMin.State := 0;
      HTHELP : ButtonHelp.State := 0;
    end;
    RepaintButton(CurrentHT);
  end;
  CurrentHT := i;
  case CurrentHT of
    HTCLOSE : ButtonClose.State := 2;
    HTMAXBUTTON : if EnabledMax or (IsZoomed(CtrlHandle) and EnabledRestore) then ButtonMax.State := 2;

    HTMINBUTTON : ButtonMin.State := 2;
    HTHELP : ButtonHelp.State := 2;
  end;
  biClicked := True;
  RepaintButton(CurrentHT);
end;

procedure TacDialogWnd.DropSysMenu(x, y: integer);
begin
  SystemMenu.WindowHandle := CtrlHandle;
  SystemMenu.Popup(x, y);
end;

procedure TacDialogWnd.Ac_WMLButtonUp(var Message: TMessage);
var
  p : TPoint;
begin
  case TWMNCHitMessage(Message).HitTest of
    HTCLOSE : if biClicked then begin
      ButtonClose.State := 0;
      SendMessage(CtrlHandle, WM_SYSCOMMAND, SC_CLOSE, 0);
    end;
    HTMAXBUTTON : if not IsIconic(CtrlHandle) or (IsIconic(CtrlHandle) and EnabledMax) then begin
      if biClicked then begin
        SetHotHT(0);
        if IsZoomed(CtrlHandle) then begin
          SendMessage(CtrlHandle, WM_SYSCOMMAND, SC_RESTORE, 0);
        end
        else begin
          SendMessage(CtrlHandle, WM_SYSCOMMAND, SC_MAXIMIZE, 0);
        end;
        Application.ProcessMessages;
        SystemMenu.UpdateItems;
      end
      else SetHotHT(0);
    end;
    HTMINBUTTON : if biClicked then begin
      p := CursorToPoint(TWMMouse(Message).XPos, TWMMouse(Message).YPos);
      if PtInRect(ButtonMin.Rect, p) then begin
        SetHotHT(0);
        if IsIconic(CtrlHandle) then begin
          SendMessage(CtrlHandle, WM_SYSCOMMAND, SC_RESTORE, 0);
        end
        else SendMessage(CtrlHandle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
      end
      else Message.Result := CallWindowProc(OldProc, CtrlHandle, Message.Msg, Message.WParam, Message.LParam);
    end else SetHotHT(0);
    HTHELP : if biClicked then begin
      SendMessage(CtrlHandle, WM_SYSCOMMAND, SC_CONTEXTHELP, 0);
      SetHotHT(0);
      SystemMenu.UpdateItems;
      SendMessage(CtrlHandle, WM_NCPAINT, 0, 0);
    end else SetHotHT(0);
    // MDI child buttons
    else begin
      Message.Result := CallWindowProc(OldProc, CtrlHandle, Message.Msg, Message.WParam, Message.LParam);
    end
  end;
  TWMNCHitMessage(Message).HitTest := 0;
end;

procedure TacDialogWnd.Ac_WMNCActivate(var Message: TMessage);
begin
//  if SkinData.CtrlSkinState and ACS_FAST = ACS_FAST then
  SkinData.BGChanged := True;
  RedrawWindow(CtrlHandle, nil, 0, RDW_ALLCHILDREN or RDW_FRAME or RDW_INVALIDATE);
end;

procedure TacDialogWnd.Ac_WMActivate(var Message: TMessage);
begin
  SkinData.BGChanged := True;
  RedrawWindow(CtrlHandle, nil, 0, RDW_ALLCHILDREN or RDW_FRAME or RDW_INVALIDATE or RDW_ERASE);
  if Assigned(SystemMenu) then SystemMenu.UpdateItems;
end;

procedure TacDialogWnd.Ac_DrawStaticItem(var Message: TWMDrawItem); 
begin
  SetBkMode(TWMDrawItem(Message).DrawItemStruct.hDC, TRANSPARENT);
end;

{ TacSystemMenu }

procedure TacSystemMenu.CloseClick(Sender: TObject);
begin
  Sendmessage(FOwner.CtrlHandle, WM_SYSCOMMAND, SC_CLOSE, 0);
end;

constructor TacSystemMenu.Create(AOwner: TComponent);
  function CreateSystemItem(const Caption, Name : string; EventProc : TNotifyEvent) : TMenuItem; begin
    Result := TMenuItem.Create(Self);
    Result.Caption := Caption;
    Result.OnClick := EventProc;
    Result.Name := Name;
  end;
begin
  inherited Create(AOwner);

  ItemRestore := CreateSystemItem(LoadStr(s_RestoreStr), 'miRestore', RestoreClick); Self.Items.Add(ItemRestore);
  ItemMove := CreateSystemItem(LoadStr(s_MoveStr), 'miMove', MoveClick);             Self.Items.Add(ItemMove);
  ItemSize := CreateSystemItem(LoadStr(s_SizeStr), 'miSize', SizeClick);             Self.Items.Add(ItemSize);
  ItemMinimize := CreateSystemItem(LoadStr(s_MinimizeStr), 'miMinimize', MinClick);  Self.Items.Add(ItemMinimize);
  ItemMaximize := CreateSystemItem(LoadStr(s_MaximizeStr), 'miMaximize', MaxClick);  Self.Items.Add(ItemMaximize);

  Self.Items.InsertNewLineAfter(ItemMaximize);

  ItemClose := CreateSystemItem(LoadStr(s_CloseStr), 'miClose', CloseClick);         Self.Items.Add(ItemClose);
  ItemClose.ShortCut := scAlt + 115;
end;

function TacSystemMenu.EnabledMove: boolean;
begin
  Result := not IsZoomed(FOwner.CtrlHandle);
end;

function TacSystemMenu.EnabledSize: boolean;
begin
  Result := (FOwner.BorderStyle <> acbsSingle) and not IsIconic(FOwner.CtrlHandle);
end;

procedure TacSystemMenu.MaxClick(Sender: TObject);
begin
  Sendmessage(FOwner.CtrlHandle, WM_SYSCOMMAND, SC_MAXIMIZE, 0);
  UpdateItems;
end;

procedure TacSystemMenu.MinClick(Sender: TObject);
begin
  SendMessage(FOwner.CtrlHandle, WM_SYSCOMMAND, SC_MINIMIZE, 0);    
end;

procedure TacSystemMenu.MoveClick(Sender: TObject);
begin
  Sendmessage(FOwner.CtrlHandle, WM_SYSCOMMAND, SC_MOVE, 0);
end;

procedure TacSystemMenu.RestoreClick(Sender: TObject);
begin
  Sendmessage(FOwner.CtrlHandle, WM_SYSCOMMAND, SC_RESTORE, 0);
  UpdateItems;
end;

procedure TacSystemMenu.SizeClick(Sender: TObject);
begin
  Sendmessage(FOwner.CtrlHandle, WM_SYSCOMMAND, SC_SIZE, 0);
end;

procedure TacSystemMenu.UpdateItems;
begin
  try
    ItemRestore.Visible  := FOwner.VisibleRestore;
    ItemMove.Visible     := True;
    ItemSize.Visible     := VisibleSize;
    ItemMinimize.Visible := FOwner.VisibleMin;
    ItemMaximize.Visible := FOwner.VisibleMax;
    ItemClose.Visible    := FOwner.VisibleClose;

    ItemRestore.Enabled  := FOwner.EnabledRestore;
    ItemMove.Enabled     := EnabledMove;
    ItemSize.Enabled     := EnabledSize;
    ItemMinimize.Enabled := FOwner.EnabledMin;
    ItemMaximize.Enabled := FOwner.EnabledMax;
    ItemClose.Enabled    := True;
  except
  end;
end;

function TacSystemMenu.VisibleSize: boolean;
begin
  Result := not (FOwner.BorderStyle in [acbsDialog, acbsNone, acbsToolWindow]);
end;

procedure ClearMnuArray;
begin
{$IFNDEF NOMNUHOOK}
  SetLength(MnuArray, 0);
{$ENDIF}
end;

procedure CleanArray;
var
  i: integer;
  ap: TacProvider;
begin
  if acSupportedList <> nil then for i := 0 to acSupportedList.Count - 1 do begin
    ap := TacProvider(acSupportedList[i]);
    if (ap <> nil) and (ap.ListSW <> nil) and ap.ListSW.Destroyed then begin
      if (ap.ListSW.BorderStyle <> acbsDialog) then begin // Non-sizeable dialog will be freed with application closing (UnInstallHook)
        ap.ListSW.Destroyed := False;
        FreeAndNil(ap);
        acSupportedList[i] := nil;
      end;
    end;
  end;
end;

initialization

finalization
  ClearMnuArray;
  if acSupportedList <> nil then begin
    while acSupportedList.Count > 0 do begin
      TObject(acSupportedList[0]).Free;
      acSupportedList.Delete(0);
    end;
    FreeAndNil(acSupportedList);
  end;

end.
