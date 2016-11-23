unit sVclUtils;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Classes, Controls, SysUtils, StdCtrls, windows,
  Dialogs, Graphics, Forms, Messages, extctrls, //sScrollBar,
  comctrls, sConst, Menus, inifiles, registry, acntUtils, sCommonData,
{$IFNDEF ALITE}
  sEdit, sMemo, sComboBox, sToolEdit, sCurrEdit, sDateUtils,
  sCustomComboEdit, sRadioButton, sMonthCalendar,
{$ENDIF}
  {$IFDEF USEDB}db, dbgrids, dbCtrls, {$ENDIF}
  sCheckBox, sGraphUtils, buttons{$IFDEF LOGGED}, sDebugMsgs{$ENDIF};

const
  AlignToInt: array[TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);

function acMousePos : TPoint;
function LeftToRight(Control : TControl; NormalAlignment : boolean = True) : boolean;
procedure AddToAdapter(Frame : TWinControl);
//procedure RemoveFromAdapter(Frame : TWinControl);
procedure BroadCastMsg(Ctrl : hwnd; Message : TMessage);

procedure SkinPaintTo(Bmp : TBitmap; Ctrl : TControl; Left : integer = 0; Top : integer = 0);
procedure PrepareForAnimation(Ctrl : TWinControl);
procedure AnimShowControl(Ctrl : TWinControl; wTime : word = 0);

function WorkRect : TRect;
procedure SetParentUpdated(wc : TWinControl); overload;
procedure SetParentUpdated(pHwnd : hwnd); overload
//procedure InitParentColor(Control : TWinControl);
function GetControlColor(Control : TControl) : TColor; overload;
function GetControlColor(Handle : THandle) : TColor; overload;

procedure PaintControls(DC: HDC; OwnerControl : TWinControl; ChangeCache : boolean; Offset : TPoint; AHandle : THandle = 0);

function SendAMessage(Handle : hwnd; Cmd : Integer; LParam : longword = 0) : longint; overload; // may be removed later
function SendAMessage(Control : TControl; Cmd : Integer; LParam : longword = 0) : longint; overload;
procedure SetBoolMsg(Handle : hwnd; Cmd : Cardinal; Value : boolean);
function GetBoolMsg(Control : TWinControl; Cmd : Cardinal) : boolean; overload;
function GetBoolMsg(CtrlHandle : hwnd; Cmd : Cardinal) : boolean; overload;
procedure RepaintShadows(Control : TWinControl; BGBmp : graphics.TBitmap);
procedure RepaintsGraphicControls(WinControl : TWinControl);
function ControlIsReady(Control : TControl) : boolean;
function GetOwnerForm(Component: TComponent) : TCustomForm;
function GetOwnerFrame(Component: TComponent) : TCustomFrame;
//function GetParentFrame(Control: TControl) : TCustomFrame;
procedure SetPanelFocus(Panel : TWinControl);
procedure SetControlsEnabled(Parent:TWinControl; Value: boolean);
function CheckPanelFilled(Panel:TCustomPanel):boolean;
{$IFDEF USEDB}
procedure ComboBoxFilling(ComboBox:TComboBox; DataSet:TDataSet; const CodeField, NameField:string; CountSymb:integer; FromDOSToWIN: boolean);
procedure FillsComboBox(sC : TCustomComboBox; CharsInCode: smallint; sD: TDataSet);
{$ENDIF}
function GetStringFlags(Control: TControl; al: TAlignment): longint;
procedure RepaintsControls(Owner: TWinControl; BGChanged : boolean);
function GetControlByName(ParentControl : TWinControl; const CtrlName : string) : TControl;
procedure AlphaBroadCast(Control : TWinControl; var Message); overload;
procedure AlphaBroadCast(Handle : hwnd; var Message); overload;
procedure SendToProvider(Form : TCustomform; var Message);
function GetCtrlRange(Ctl : TWinControl; nBar : integer) : integer;
function ACClientRect(Handle : hwnd): TRect;
function GetAlignShift(Ctrl : TWinControl; Align : TAlign; GraphCtrlsToo : boolean = False) : integer;
function GetParentFormHandle(const CtrlHandle: hwnd): hwnd;

type
  TOutputWindow = class(TCustomControl)
  private
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCPaint(var Message: TWmEraseBkgnd); message WM_NCPAINT;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Canvas;
  end;

var
  ow : TOutPutwindow = nil;
  acPrintDC : hdc = 0;
  acSrcBmp : TBitmap = nil;
  InAnimationProcess : boolean = False;

  uxthemeLib : Cardinal;
  Ac_SetWindowTheme : function(hwnd: HWND; pszSubAppName: LPCWSTR; pszSubIdList: LPCWSTR): HRESULT; stdcall;

implementation

uses
  {$IFNDEF ALITE} sStatusBar, sPageControl, sSpinEdit, sGroupBox, sGauge,
  sScrollBox, sComboBoxes, sSplitter,{$ENDIF} sPanel, sStyleSimply, sSkinProvider,
  sMessages, sMaskData, math, ShellAPI, sSkinManager, sThirdParty;

function acMousePos : TPoint;
begin
  GetCursorPos(Result);
end;

function LeftToRight(Control : TControl; NormalAlignment : boolean = True) : boolean;
begin
  if NormalAlignment
    then Result := (Control.BidiMode = bdLeftToRight) or not SysLocale.MiddleEast
    else Result := (Control.BidiMode <> bdLeftToRight) and SysLocale.MiddleEast;
end;

procedure AddToAdapter(Frame : TWinControl);
var
  c : TWinControl;
begin
  if (csDesigning in Frame.ComponentState) or (csLoading in Frame.ComponentState) then Exit;
  c := GetParentForm(Frame);
  if c <> nil then SendMessage(c.Handle, SM_ALPHACMD, MakeWParam(0, AC_CONTROLLOADED), longword(Frame));
end;

procedure BroadCastMsg(Ctrl : hwnd; Message : TMessage);
var
  hCtrl : THandle;
begin
  hCtrl := GetTopWindow(Ctrl);
  while hCtrl <> 0 do begin
    if (GetWindowLong(hCtrl, GWL_STYLE) and WS_CHILD) = WS_CHILD then SendMessage(hCtrl, Message.Msg, Message.WParam, Message.LParam);
    hCtrl := GetNextWindow(hCtrl, GW_HWNDNEXT);
  end;
end;

procedure SkinPaintTo(Bmp : TBitmap; Ctrl : TControl; Left : integer = 0; Top : integer = 0);
var
  I: Integer;
  SaveIndex : hdc;
  cR : TRect;
  DC : hdc;
{$IFDEF LOGGED} // Speed test
  lw : Longword;
{$ENDIF}
begin
  if not Ctrl.Visible then Exit;
  GetWindowRect(TWinControl(Ctrl).Handle, cR);
  DC := Bmp.Canvas.Handle;
  IntersectClipRect(DC, 0, 0, Ctrl.Width, Ctrl.Height);
  if Ctrl is TWinControl then begin

    if (Ctrl is TForm) and (TForm(Ctrl).FormStyle = fsMDIForm) then for I := 0 to TForm(Ctrl).MDIChildCount - 1 do begin
      SaveIndex := SaveDC(DC);
      MoveWindowOrg(DC, TForm(Ctrl).MDIChildren[i].Left, TForm(Ctrl).MDIChildren[i].Top);
      SkinPaintTo(Bmp, TForm(Ctrl).MDIChildren[i], TForm(Ctrl).MDIChildren[i].Left, TForm(Ctrl).MDIChildren[i].Top);
      RestoreDC(DC, SaveIndex);
    end;

    if (Ctrl is TTabsheet) and (TTabSheet(Ctrl).BorderWidth <> 0) then begin
      MoveWindowOrg(DC, TTabSheet(Ctrl).BorderWidth, TTabSheet(Ctrl).BorderWidth);
    end;

{$IFDEF LOGGED} // Speed test
  lw := GetTickCount;
{$ENDIF}

    if GetBoolMsg(TWinControl(Ctrl), AC_CTRLHANDLED)
      then SendMessage(TWinControl(Ctrl).Handle, WM_PRINT, longint(DC), 0)
      else TWinControl(Ctrl).PaintTo(DC, 0, 0);

{$IFDEF LOGGED} // Speed test
  LogLines.Add(Ctrl.Name + ' - ' + IntToStr(GetTickCount - lw));
{$ENDIF}

    for I := 0 to TWinControl(Ctrl).ControlCount - 1 do if (TWinControl(Ctrl).Controls[I] is TWinControl) and TWinControl(Ctrl).Controls[I].Visible then begin
      SaveIndex := SaveDC(DC);
      if not (TWinControl(Ctrl).Controls[I] is TCustomForm) or (TWinControl(Ctrl).Controls[I].Parent <> nil) then begin
        MoveWindowOrg(DC, TWinControl(Ctrl).Controls[I].Left, TWinControl(Ctrl).Controls[I].Top);
      end;
      SkinPaintTo(Bmp, TWinControl(Ctrl).Controls[I], TWinControl(Ctrl).Controls[I].Left, TWinControl(Ctrl).Controls[I].Top);
      RestoreDC(DC, SaveIndex);
    end;

    if (Ctrl is TTabsheet) and (TTabSheet(Ctrl).BorderWidth <> 0) then begin
      MoveWindowOrg(DC, -TTabSheet(Ctrl).BorderWidth, -TTabSheet(Ctrl).BorderWidth);
    end;
  end
  else Ctrl.Perform(WM_PRINT, longint(DC), 0);
end;

type
  TacWinControl = class(TWinControl);

procedure SetChildOrderAfter(Child: TWinControl; Control: TControl);
var
  i: Integer;
begin
  for i := 0 to Child.Parent.ControlCount do if Child.Parent.Controls[i] = Control then begin
    TacWinControl(Child.Parent).SetChildOrder(Child, i + 1);
    break;
  end;
end;

procedure PrepareForAnimation(Ctrl : TWinControl);
var
  Flags : dword;
  R : TRect;
  ScrDC : hdc;
begin
  GetWindowRect(Ctrl.Handle, R);
  if acSrcBmp = nil then begin
    acSrcBmp := CreateBmp24(Ctrl.width, Ctrl.Height);
    ScrDC := GetWindowDC(0);
    BitBlt(acSrcBmp.Canvas.Handle, 0, 0, Ctrl.width, Ctrl.Height, ScrDC, R.Left, R.Top, SRCCOPY);
    ReleaseDC(Ctrl.Handle, ScrDC);
  end;

  if ow = nil then ow := TOutPutWindow.Create(Ctrl);
  if Ctrl.Parent <> nil then begin
    ow.Parent := Ctrl.Parent;
    if ow = nil then Exit;
    SetChildOrderAfter(ow, Ctrl);
    ow.BoundsRect := Ctrl.BoundsRect;
  end
  else begin
    ow.BoundsRect := R;
  end;
  if ow.Parent = nil then begin
    Flags := SWP_NOZORDER or SWP_SHOWWINDOW or SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE; // or SWP_NOREDRAW or SWP_NOCOPYBITS;
    SetWindowPos(ow.Handle, GetWindow(TWinControl(Ctrl).Handle, GW_HWNDPREV), 0, 0, 0, 0, Flags);
  end
  else ShowWindow(ow.Handle, SW_SHOWNA);
end;

procedure AnimShowControl(Ctrl : TWinControl; wTime : word = 0);
const
  DelayValue = 8;
var
  NewBmp : TBitmap;
  DC : hdc;
  i, StepCount : integer;
  Percent, p : integer;
  sp : longint;
  acDstBmp : TBitmap;
begin
  InAnimationProcess := True;
  if ow = nil then PrepareForAnimation(Ctrl);
  if ow = nil then Exit;

  acDstBmp := CreateBmp24(Ctrl.width, Ctrl.Height);

  acDstBmp.Canvas.Lock;
  acPrintDC := acDstBmp.Canvas.Handle;
  SkinPaintTo(acDstBmp, Ctrl);
  if acDstBmp = nil then Exit;
  acPrintDC := 0;
  acDstBmp.Canvas.UnLock;

  NewBmp := CreateBmp32(Ctrl.width, Ctrl.Height);
  StepCount := wTime div DelayValue;

  if Ctrl is TCustomForm then sp := SendMessage(Ctrl.Handle, SM_ALPHACMD, MakeWParam(0, AC_GETPROVIDER), 0) else sp := 0;
  if (sp <> 0) then begin
    FillArOR(TsSkinProvider(sp));
    if ow = nil then Exit;
    SetWindowRgn(ow.Handle, sSkinProvider.GetRgnFromArOR(TsSkinProvider(sp)), False);
  end;
//  DC := GetWindowDC(Ctrl.Handle);
  DC := GetWindowDC(ow.Handle);

  if StepCount > 0 then begin
    p := 255 div StepCount;
    Percent := 255;
    i := 0;
    while i <= StepCount do begin
      SumBitmapsByMask(NewBmp, acSrcBmp, acDstBmp, nil, max(0, Percent));
      BitBlt(DC, 0, 0, Ctrl.Width, Ctrl.Height, NewBmp.Canvas.Handle, 0, 0, SRCCOPY);
      if Assigned(acMagnForm) then SendMessage(acMagnForm.Handle, SM_ALPHACMD, MakeWParam(0, AC_REFRESH), 0);
      inc(i);
      dec(Percent, p);
      if (i > StepCount) then Break;
      if StepCount > 0 then Sleep(DelayValue);
    end;
  end;
  BitBlt(DC, 0, 0, Ctrl.width, Ctrl.Height, acDstBmp.Canvas.Handle, 0, 0, SRCCOPY);

  if Assigned(acMagnForm) then SendMessage(acMagnForm.Handle, SM_ALPHACMD, MakeWParam(0, AC_REFRESH), 0);
  InAnimationProcess := False;

  SendMessage(Ctrl.Handle, WM_SETREDRAW, 1, 0); // Vista
  if Win32MajorVersion >= 6
    then RedrawWindow(Ctrl.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_ALLCHILDREN or RDW_INVALIDATE or RDW_UPDATENOW);
  if not (Ctrl is TCustomForm) or (DWord(GetWindowLong(Ctrl.Handle, GWL_EXSTYLE)) and $00080000 <> $00080000)
    then SetWindowPos(ow.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or SWP_HIDEWINDOW or SWP_NOREDRAW or SWP_NOCOPYBITS or SWP_NOACTIVATE);
  ReleaseDC(ow.Handle, DC);
  FreeAndnil(ow);
  FreeAndNil(NewBmp);
// Vista RedrawWindow(Ctrl.Handle, nil, 0, RDW_ALLCHILDREN or RDW_UPDATENOW);
  acPrintDC := 0;
  FreeAndNil(acSrcBmp);
  FreeAndNil(acDstBmp);
end;

procedure SetParentUpdated(wc : TWinControl); overload;
var
  i : integer;
begin
  if not InAnimationProcess then begin
    i := 0;
    while i < wc.ControlCount do begin
      if not (wc.Controls[i] is TGraphicControl) and not (csDestroying in wc.Controls[i].ComponentState) then begin
        if wc.Controls[i] is TWinControl then begin
          if TWinControl(wc.Controls[i]).HandleAllocated and TWinControl(wc.Controls[i]).Showing then SendAMessage(TWinControl(wc.Controls[i]).Handle, AC_ENDPARENTUPDATE)
        end
        else if wc.Controls[i] is TControl then SendAMessage(wc.Controls[i], AC_ENDPARENTUPDATE);
      end;
      inc(i);
    end;
  end;
end;

procedure SetParentUpdated(pHwnd : hwnd); overload;
var
  hCtrl : THandle;
begin
  hCtrl := GetTopWindow(pHwnd);
  while hCtrl <> 0 do begin
    if (GetWindowLong(hCtrl, GWL_STYLE) and WS_CHILD) = WS_CHILD then SendAMessage(hCtrl, AC_ENDPARENTUPDATE);
    hCtrl := GetNextWindow(hCtrl, GW_HWNDNEXT);
  end;
end;
(*
procedure InitParentColor(Control : TWinControl);
var
  BGInfo : TacBGInfo;
begin
  ParentCenterColor := clFuchsia;
  if Control = nil then Exit;
  GetBGInfo(@BGInfo, Control);
  if BGInfo.BgType = btCache then begin
    ParentCenterColor := clfuchsia;
    CtrlParentColor := clfuchsia;
  end
{  SendMessage(Control.Handle, SM_ALPHACMD, MakeWParam(0, AC_GETCONTROLCOLOR), 0);
  if ParentCenterColor = clFuchsia then ParentCenterColor := TsHackedControl(Control).Color;
  ParentCenterColor := ColorToRGB(ParentCenterColor);
  CtrlParentColor := ParentCenterColor;}
end;
*)
function GetControlColor(Control : TControl) : TColor;
begin
  Result := clFuchsia;
  if Control = nil then Exit;
  if SendAMessage(Control, AC_CTRLHANDLED) = 1
    then Result := ColorToRGB(Control.Perform(SM_ALPHACMD, MakeWParam(0, AC_GETCONTROLCOLOR), Result))
    else Result := ColorToRGB(TsHackedControl(Control).Color); // message is not supported by parent control
end;

function GetControlColor(Handle : THandle) : TColor; overload;
begin
  Result := clFuchsia;
  if Handle = 0 then Exit;
  Result := ColorToRGB(SendMessage(Handle, SM_ALPHACMD, MakeWParam(0, AC_GETCONTROLCOLOR), Result));
end;

function WorkRect: TRect;
begin
  SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0);
end;

function GetControlByName(ParentControl : TWinControl; const CtrlName : string) : TControl;
var
  i, j : integer;
  FrameName, cName : string;
  cf : TCustomFrame;
begin
  Result := nil;
  if ParentControl = nil then Exit;
  if pos('.', CtrlName) < 1 then for i := 0 to ParentControl.ComponentCount - 1 do begin
    if UpperCase(ParentControl.Components[i].Name) = UpperCase(CtrlName) then begin
      Result := TControl(ParentControl.Components[i]);
      Exit;
    end;
  end
  else begin
    FrameName := ExtractWord(1, CtrlName, ['.']);
    cName := ExtractWord(2, CtrlName, ['.']);
    if (FrameName = '') or (cName = '') then Exit;
    for i := 0 to ParentControl.ComponentCount - 1 do if (UpperCase(ParentControl.Components[i].Name) = UpperCase(FrameName)) then begin
      if (ParentControl.Components[i] is TCustomFrame) then begin
        cf := TCustomFrame(ParentControl.Components[i]);
        for j := 0 to cf.ComponentCount - 1 do if UpperCase(cf.Components[j].Name) = UpperCase(cName) then begin
          Result := TControl(cf.Components[j]);
          Exit;
        end
      end;
      Exit
    end;
  end;
end;

procedure PaintControls(DC: HDC; OwnerControl : TWinControl; ChangeCache : boolean; Offset : TPoint; AHandle : THandle = 0);
var
  SaveIndex : hdc;
  I, J, Count : Integer;
  R : TRect;
  tDC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  MemDCExists : boolean;
  BGInfo : TacBGInfo;
  function ControlIsReady(Control : TControl) : boolean; begin
    Result := (Control.Visible or (csDesigning in Control.ComponentState)) and (Control is TGraphicControl) and (Control.Width > 0) and (Control.Height > 0) and
           not (csNoDesignVisible in Control.ControlStyle) and not (csDestroying in Control.ComponentState) and
             not ((Control is TToolButton) and (TToolButton(Control).Style in [tbsCheck, tbsButton, tbsDropDown])) and
               RectVisible(DC, Control.BoundsRect);
  end;
begin
  MemDCExists := False;
  MemDC := 0;
  MemBitmap := 0;
  OldBitmap := 0;
  if (OwnerControl.Visible or (csDesigning in OwnerControl.ComponentState)) and (OwnerControl.ControlCount > 0) then try
    if (GetClipBox(DC, R) = NULLREGION) or (WidthOf(R) = 0) or (HeightOf(R) = 0) then begin
      SendAMessage(OwnerControl.Handle, AC_SETHALFVISIBLE);
      Exit;
    end;        

    BGInfo.BgType := btUnknown;
    BGInfo.PleaseDraw := False;
    GetBGInfo(@BGInfo, OwnerControl);
    
    I := 0; Count := OwnerControl.ControlCount;

    while I < Count do begin
      if ControlIsReady(OwnerControl.Controls[I]) then begin
        if (OwnerControl is TForm) and (TForm(OwnerControl).FormStyle = fsMDIForm) and
              (OwnerControl.Controls[I].Align <> alNone) and (OwnerControl.Controls[I] is TGraphicControl) then begin

          OwnerControl.Controls[I].Perform(SM_ALPHACMD, MakeWParam(0, AC_INVALIDATE), 0);
{          SendMessage(OwnerControl.Handle, SM_ALPHACMD, MakeWParam(0, AC_SETGRAPHCONTROL), longint(OwnerControl.Controls[I]));
          OwnerControl.Controls[I].Repaint;
          SendMessage(OwnerControl.Handle, SM_ALPHACMD, MakeWParam(0, AC_SETGRAPHCONTROL), 0);}
        end;

        if not MemDCExists then begin
          tDC := GetDC(0);
          MemBitmap := CreateCompatibleBitmap(tDC, OwnerControl.Width, OwnerControl.Height);
          ReleaseDC(0, tDC);
          MemDC := CreateCompatibleDC(0); OldBitmap := SelectObject(MemDC, MemBitmap);
          MemDCExists := True;
          for j := 0 to Count - 1 do // Copy parent BG
            if ControlIsReady(OwnerControl.Controls[J]) then begin
              if not (csOpaque in OwnerControl.Controls[J].ControlStyle) {???or (OwnerControl.Controls[J] is TGraphicControl)} then begin
                if BGInfo.BgType = btFill // If without cache
                  then FillDC(MemDC, OwnerControl.Controls[J].BoundsRect, BGInfo.Color)
                  else BitBlt(MemDC, OwnerControl.Controls[J].Left, OwnerControl.Controls[J].Top, OwnerControl.Controls[J].Width, OwnerControl.Controls[J].Height, BGInfo.Bmp.Canvas.Handle, OwnerControl.Controls[J].Left + BGInfo.Offset.X, OwnerControl.Controls[J].Top + BGInfo.Offset.Y, SRCCOPY)
              end
              else FillDC(MemDC, OwnerControl.Controls[J].BoundsRect, GetControlColor(OwnerControl.Controls[J]));
            end;
        end;

        SaveIndex := SaveDC(MemDC);

        if not RectVisible(DC, OwnerControl.Controls[I].BoundsRect) then begin
          SendAMessage(OwnerControl.Controls[I], AC_SETHALFVISIBLE);
        end;

        MoveWindowOrg(MemDC, OwnerControl.Controls[I].Left, OwnerControl.Controls[I].top);
        IntersectClipRect(MemDC, 0, 0, OwnerControl.Controls[I].Width, OwnerControl.Controls[I].Height);

        if csPaintCopy in OwnerControl.ControlState then begin
          OwnerControl.Controls[I].ControlState := OwnerControl.Controls[I].ControlState + [csPaintCopy];
        end;
        OwnerControl.Controls[I].Perform(WM_PAINT, longint(MemDC), 0);
        if csPaintCopy in OwnerControl.ControlState then begin
          OwnerControl.Controls[I].ControlState := OwnerControl.Controls[I].ControlState - [csPaintCopy];
        end;

        MoveWindowOrg(MemDC, - OwnerControl.Controls[I].Left, - OwnerControl.Controls[I].Top);

        RestoreDC(MemDC, SaveIndex);
      end;
      Inc(i);
    end;
    if MemDCExists then begin
      J := 0;
      while J < Count do begin // Copy graphic controls
        if ControlIsReady(OwnerControl.Controls[J]) then begin
          if GetPixel(MemDC, OwnerControl.Controls[J].Left + Offset.X, OwnerControl.Controls[J].Top + Offset.Y) <> DWord(clFuchsia) then
            BitBlt(DC, OwnerControl.Controls[J].Left + Offset.X, OwnerControl.Controls[J].Top + Offset.Y, OwnerControl.Controls[J].Width, OwnerControl.Controls[J].Height,
                   MemDC, OwnerControl.Controls[J].Left, OwnerControl.Controls[J].Top, SRCCOPY);
        end;
        inc(J);
      end;
    end;
  finally if MemDCExists then begin
    SelectObject(MemDC, OldBitmap);
    DeleteDC(MemDC);
    DeleteObject(MemBitmap);
  end; end;
end;

function SendAMessage(Handle : hwnd; Cmd : Integer; LParam : longword = 0) : longint; overload;
begin
  Result := SendMessage(Handle, SM_ALPHACMD, MakeWParam(0, Cmd), LParam);
end;

function SendAMessage(Control : TControl; Cmd : Integer; LParam : longword = 0) : longint; overload;
begin
  Result := 0;
  if (Control is TWinControl) then begin
    if not (csDestroying in Control.ComponentState) and TWinControl(Control).HandleAllocated
      then Result := SendMessage(TWinControl(Control).Handle, SM_ALPHACMD, MakeWParam(0, Cmd), LParam)
  end
  else Result := Control.Perform(SM_ALPHACMD, MakeWParam(0, Cmd), LParam)
end;

procedure SetBoolMsg(Handle : hwnd; Cmd : Cardinal; Value : boolean);
var
  m : TMessage;
begin
  m.Msg := SM_ALPHACMD;
  m.WParam := MakeWParam(Word(Value), Cmd);
  m.Result := 0;
  SendMessage(Handle, m.Msg, m.wParam, m.lParam);
end;

function GetBoolMsg(Control : TWinControl; Cmd : Cardinal) : boolean;
begin
  Result := boolean(SendAMessage(Control, Cmd));
end;

function GetBoolMsg(CtrlHandle : hwnd; Cmd : Cardinal) : boolean; overload;
var
  LParam : cardinal;
begin
  LParam := 0;
  if SendMessage(CtrlHandle, SM_ALPHACMD, MakeWParam(0, Cmd), LParam) = 1 then Result := True else Result := LParam = 1;
end;

procedure RepaintShadows(Control : TWinControl; BGBmp : graphics.TBitmap);
begin
end;

procedure RepaintsGraphicControls(WinControl : TWinControl);
var
  i : integer;
begin
  for i := 0 to WinControl.ControlCount - 1 do
    if (WinControl.Controls[i] is TGraphicControl) then
      if ControlIsReady(WinControl.Controls[i]) then WinControl.Controls[i].Repaint;
end;

function ControlIsReady(Control : TControl) : boolean;
begin
  Result := False;
  if (Control = nil) or ((Control is TWinControl) and not TWinControl(Control).HandleAllocated) then Exit;

  Result := not (csCreating in Control.ControlState) and
              not (csReadingState in Control.ControlState) and //not (csAlignmentNeeded in Control.ControlState) and
                not (csLoading in Control.ComponentState) and not (csDestroying in Control.ComponentState) and
                  (Control.Parent <> nil);
end;

function GetOwnerForm(Component: TComponent) : TCustomForm;
var
  c: TComponent;
begin
  Result := nil;
  c := Component;
  while Assigned(c) and not (c is TCustomForm) do c := c.Owner;
  if (c is TCustomForm) then Result := TCustomForm(c);
end;

function GetOwnerFrame(Component: TComponent) : TCustomFrame;
var
  c: TComponent;
begin
  Result := nil;
  c := Component;
  while Assigned(c) and not (c is TCustomFrame) do c := c.Owner;
  if (c is TCustomFrame) then Result := TCustomFrame(c);
end;
{
function GetParentFrame(Control: TControl) : TCustomFrame;
begin
  if Control.Parent = nil then Result := nil else begin
    if Control.Parent is TCustomFrame
      then Result := TCustomFrame(Control.Parent)
      else GetParentFrame(Control.Parent);
  end;
end;
}
procedure SetPanelFocus(Panel : TWinControl);
var
  List : TList;
  i : integer;
begin
  List := TList.Create;
  Panel.GetTabOrderList(List);
  if List.Count > 0 then for i:=0 to List.Count-1 do begin
    if TWinControl(List[i]).Enabled and TWinControl(List[i]).TabStop then begin
      TWinControl(List[i]).SetFocus;
      Break;
    end;
  end;
  List.Free;
end;

procedure SetControlsEnabled(Parent:TWinControl; Value: boolean);
var
   i:integer;
begin
  for i:=0 to Parent.ControlCount-1 do begin
    if not (Parent.Controls[i] is TCustomPanel) then Parent.Controls[i].Enabled := Value;
  end;
end;

function CheckPanelFilled(Panel:TCustomPanel):boolean;
var
   i:integer;
begin
  Result:=False;
  for i:=0 to Panel.ControlCount-1 do begin
    if (Panel.Controls[i] is TEdit) and (TEdit(Panel.Controls[i]).Text='') then begin exit; end;
    if (Panel.Controls[i] is TComboBox) and (TComboBox(Panel.Controls[i]).Text='') then begin exit; end;
  end;
  Result:=True;
end;

{$IFDEF USEDB}
procedure ComboBoxFilling(ComboBox:TComboBox; DataSet:TDataSet; const CodeField, NameField:string; CountSymb:integer; FromDOSToWIN: boolean);
begin
  with DataSet do begin
    DisableControls;
    open;
    first;
    while not eof do begin
      if CodeField<>'' then begin
       if FieldByName(CodeField).AsInteger < CountSymb then begin
        if FromDOSToWIN
          then ComboBox.Items.Add('0' + FieldByName(CodeField).AsString + ' - ' + OEMToAnsiStr(FieldByName(NameField).AsString))
          else ComboBox.Items.Add('0' + FieldByName(CodeField).AsString + ' - ' + FieldByName(NameField).AsString);
       end
       else begin
        if FromDOSToWIN
          then ComboBox.Items.Add(FieldByName(CodeField).AsString + ' - ' + OEMToAnsiStr(FieldByName(NameField).AsString))
          else ComboBox.Items.Add(FieldByName(CodeField).AsString + ' - ' + FieldByName(NameField).AsString);
       end;
      end
      else begin
        if FromDOSToWIN
          then ComboBox.Items.Add(OEMToAnsiStr(FieldByName(NameField).AsString))
          else ComboBox.Items.Add(FieldByName(NameField).AsString);
      end;
      next;
    end;
    EnableControls;
  end;
end;

procedure FillsComboBox(sC : TCustomComboBox; CharsInCode: smallint; sD: TDataSet);
begin
  if not sD.Active then sD.Open;
  sC.Items.Clear;
  while not sD.Eof do begin
    if (CharsInCode = 0) then begin
      sC.Items.Add(sD.Fields[0].asString);
    end
    else begin
      sC.Items.Add(AddChar('0', sD.Fields[0].asString, CharsInCode) +
                   ' - ' + sD.Fields[1].asString);
    end;
    sD.Next;
  end;
  sD.Close;
  sC.ItemIndex := 0;
end;
{$ENDIF}

function GetStringFlags(Control: TControl; al: TAlignment): longint;
begin
  Result := Control.DrawTextBiDiModeFlags(DT_EXPANDTABS or DT_VCENTER or AlignToInt[al]);
end;

procedure RepaintsControls(Owner: TWinControl; BGChanged : boolean);
var
  i: Integer;
begin
  i := 0;
  while i <= Owner.ControlCount - 1 do begin
    if ControlIsReady(Owner.Controls[i]) then begin
      if not (Owner.Controls[i] is TGraphicControl) then Owner.Controls[i].Invalidate;
    end;
    inc(i);
  end;
end;

procedure AlphaBroadCast(Control : TWinControl; var Message);
var
  i : integer;
begin
  i := 0;
  while i < Control.ControlCount do begin
    if (i >= Control.ControlCount) or (csDestroying in Control.Controls[i].ComponentState) then Exit;
    if (Control.Controls[i] is TWincontrol) then begin
      if not TWinControl(Control.Controls[i]).HandleAllocated then begin
        Control.Controls[i].Perform(TMessage(Message).Msg, TMessage(Message).Wparam, TMessage(Message).LParam);
      end
      else if GetBoolMsg(TWinControl(Control.Controls[i]), AC_CTRLHANDLED) then begin
        SendMessage(TWinControl(Control.Controls[i]).Handle, TMessage(Message).Msg, TMessage(Message).WParam, TMessage(Message).LParam)
      end
      else if not CheckDevEx(Control.Controls[i]) then AlphaBroadCast(TWinControl(Control.Controls[i]), Message);
    end
    else Control.Controls[i].Perform(TMessage(Message).Msg, TMessage(Message).Wparam, TMessage(Message).LParam);
    inc(i);
  end;
end;

type
  TacMsgInfo = record
    Sender : hwnd;
    Message : TMessage;
  end;

  PacMsgInfo = ^TacMsgInfo;

function SendToChildren(Child: HWND; Data: LParam): BOOL; stdcall;
var
  MsgI : TacMsgInfo;
begin
  MsgI := PacMsgInfo(Data)^;
  if GetParent(Child) = MsgI.Sender then begin
    SendMessage(Child, MsgI.Message.Msg, MsgI.Message.WParam, MsgI.Message.LParam);
  end;
  Result := True;
end;

procedure AlphaBroadCast(Handle : hwnd; var Message); overload;
var
  MsgI : TacMsgInfo;
begin
  MsgI.Sender := Handle;
  MsgI.Message := TMessage(Message);
  EnumChildWindows(Handle, @SendToChildren, LPARAM(@MsgI));
end;

procedure SendToProvider(Form : TCustomform; var Message);
var
  i : integer;
//  ap : TacProvider;
begin
  i := 0;
  while i < Form.ComponentCount do begin
    if i >= Form.ComponentCount then Exit;
    if (Form.Components[i] is TsSkinProvider) and not (csDestroying in Form.Components[i].ComponentState) then begin
      TsSkinProvider(Form.Components[i]).DsgnWndProc(TMessage(Message));
      exit
    end;
    inc(i);
  end;
{
  for i := 0 to acSupportedList.Count - 1 do begin
    ap := TacProvider(acSupportedList[i]);
    if (ap <> nil) and (ap.ListSW <> nil) and IsWindowVisible(ap.ListSW.CtrlHandle)
      then SendMessage(ap.ListSW.CtrlHandle, TMessage(Message).Msg, TMessage(Message).WParam, TMessage(Message).LParam);
  end;
}
end;

function GetCtrlRange(Ctl : TWinControl; nBar : integer) : integer;
var
  i, iMin, iMax : integer;
begin
  iMax := 0;
  iMin := 0;
  case nBar of
    SB_VERT : begin
      iMin := Ctl.Height;
      for i := 0 to Ctl.ControlCount - 1 do begin
        iMin := Min(iMin, Ctl.Controls[i].Top);
        iMax := Max(iMax, Ctl.Controls[i].Top + Ctl.Controls[i].Height);
      end;
    end;
    SB_HORZ : begin
      iMin := Ctl.Width;
      for i := 0 to Ctl.ControlCount - 1 do begin
        iMin := Min(iMin, Ctl.Controls[i].Left);
        iMax := Max(iMax, Ctl.Controls[i].Left + Ctl.Controls[i].Width);
      end;
    end;
  end;
  if iMin < iMax then Result := iMax - iMin else Result := 0
end;

function ACClientRect(Handle : hwnd): TRect;
var
  R: TRect;
begin
  Windows.GetClientRect(Handle, Result);
  GetWindowRect(Handle, R);
  MapWindowPoints(0, Handle, R, 2);
  OffsetRect(Result, -R.Left, -R.Top);
end;

{ TOutputWindow }

constructor TOutputWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := 'acOutputWindow';
  Visible := False;
  Color   := clPurple;
end;

procedure TOutputWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do begin
    if (Parent = nil) and (ParentWindow = 0) then begin
      Params.Style := WS_POPUP;
      if(Owner is TWinControl) and ((DWord(GetWindowLong(TWinControl(Owner).Handle, GWL_EXSTYLE)) and WS_EX_TOPMOST) <> 0)
        then Params.ExStyle := ExStyle or WS_EX_TOPMOST;
      WndParent := Application.Handle;
    end;
  end;
end;

procedure TOutputWindow.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TOutputWindow.WMNCPaint(var Message: TWmEraseBkgnd);
begin
  Message.Result := 0;
end;

function GetAlignShift(Ctrl : TWinControl; Align : TAlign; GraphCtrlsToo : boolean = False) : integer;
var
  i : integer;
begin
  Result := 0;
  for i := 0 to Ctrl.ControlCount - 1 do if Ctrl.Controls[i].Visible and (Ctrl.Controls[i].Align = Align) and (GraphCtrlsToo or not (Ctrl.Controls[i] is TGraphicControl)) then begin
    case Align of
      alLeft, alRight : inc(Result, Ctrl.Controls[i].Width);
      alTop, alBottom : inc(Result, Ctrl.Controls[i].Height);
    end;
  end;
end;

function GetParentFormHandle(const CtrlHandle: hwnd): hwnd;
var
  ph : hwnd;
begin
  ph := GetParent(CtrlHandle);
  if ph = 0 then Result := CtrlHandle else Result := GetParentFormHandle(ph);
end;

initialization
  uxthemeLib := LoadLibrary('UXTHEME');
  try
    if uxthemeLib <> 0
      then Ac_SetWindowTheme := GetProcAddress(uxthemeLib, 'SetWindowTheme')
      else @Ac_SetWindowTheme := nil
  finally
  end;

finalization
  FreeLibrary(uxthemeLib);

end.

