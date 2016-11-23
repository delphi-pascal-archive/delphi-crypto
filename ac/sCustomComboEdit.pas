unit sCustomComboEdit;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, acntUtils, buttons, menus,
{$IFNDEF ALITE}
  sCalcUnit,
{$ENDIF}
  sConst, sSpeedButton,
  sGraphUtils, sCommonData, sDefaults, sMaskEdit, sSkinProps, sBitBtn,
  sGlyphUtils{$IFDEF DELPHI6}, Variants{$ENDIF};

type
{$IFNDEF NOTFORHELP}
  TCloseUpEvent = procedure (Sender: TObject; Accept: Boolean) of object;
  TsCustomComboEdit = class;

  TsEditButton = class(TsSpeedButton)
  private
    FOwner : TsCustomComboEdit;
  public
    procedure BeginInitGlyph;
    procedure EndInitGlyph;
    constructor Create(AOwner: TComponent); override;
    procedure PaintTo(DC : hdc; R : TPoint);
    procedure PrepareCache; override;
    procedure Paint; override;
    function GlyphWidth : integer; override;
    function GlyphHeight : integer; override;
  end;
{$ENDIF} // NOTFORHELP

  TsCustomComboEdit = class(TsMaskEdit)
{$IFNDEF NOTFORHELP}
  private
    FButton: TsEditButton;
    FClickKey: TShortCut;
    FReadOnly: Boolean;
    FDirectInput: Boolean;
    FAlignment: TAlignment;
    FPopupWindowAlign: TPopupWindowAlign;
    FGlyphMode: TsGlyphMode;
    FPopupWidth: integer;
    FDisabledKind: TsDisabledKind;
    FShowButton: boolean;
    procedure EditButtonClick(Sender: TObject);
    procedure EditButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function GetDroppedDown: Boolean;
    procedure SetDirectInput(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure SetAlignment(Value: TAlignment);
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure WMCut(var Message: TWMCut); message WM_CUT;
    procedure CMFocuseChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure SetPopupWidth(const Value: integer);
    procedure SetDisabledKind(const Value: TsDisabledKind);
    procedure SetShowButton(const Value: boolean);
  protected
    FOnButtonClick: TNotifyEvent;
    procedure SetEditRect; override;
    function IsActive : boolean; override;
    procedure PaintBorder(DC : hdc); override;
    procedure PaintText; override;
    procedure OurPaintHandler(DC : hdc); override;

    function GetReadOnly: Boolean; virtual;
    procedure KeyPress(var Key: Char); override;

    procedure PopupWindowShow; virtual;
    procedure PopupWindowClose; virtual;
    procedure CreateParams(var Params: TCreateParams); override;

    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure ButtonClick; dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property PopupAlign: TPopupWindowAlign read FPopupWindowAlign write FPopupWindowAlign default pwaRight;
  public
    FDefBmpID : integer;
    FPopupWindow: TWinControl;
    constructor Create(AOwner: TComponent); override;
    procedure CreateWnd; override;
    destructor Destroy; override;
    procedure Invalidate; override;
    procedure AfterConstruction; override;
    procedure Loaded; override;

    procedure WndProc (var Message: TMessage); override;
    procedure DoClick;
    procedure SelectAll; virtual;
    property Button: TsEditButton read FButton;
    property DroppedDown: Boolean read GetDroppedDown;
    property PopupWidth : integer read FPopupWidth write SetPopupWidth default 197;
{$ENDIF} // NOTFORHELP
  published
{$IFNDEF NOTFORHELP}
    property Align;             // KJS
    property Anchors;           // KJS
    property AutoSelect;
    property DragCursor;
    property DragMode;
    property EditMask;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnContextPopup;
{$ENDIF} // NOTFORHELP
    {:@event}
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property ShowButton : boolean read FShowButton write SetShowButton default True;
    property ClickKey: TShortCut read FClickKey write FClickKey default scAlt + vk_Down;
    property DisabledKind : TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property GlyphMode : TsGlyphMode read FGlyphMode write FGlyphMode;
    property DirectInput: Boolean read FDirectInput write SetDirectInput default True;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;

implementation

uses sStyleSimply,
{$IFNDEF ALITE}
  sCurrencyEdit,
{$ENDIF}
{$IFDEF CHECKXP}
  UxTheme, Themes,
{$ENDIF}
  sMaskData, sVCLUtils, sMessages, sAlphaGraph, sSkinManager, math
  {$IFDEF LOGGED}, sDebugMsgs{$ENDIF};

procedure TsEditButton.BeginInitGlyph;
begin
  if (csLoading in ComponentState) or (csCreating in ControlState) then Exit;
  SkinData.CtrlSkinState := SkinData.CtrlSkinState or ACS_LOCKED;
  if Assigned(FOwner.GlyphMode.Images) and (FOwner.GlyphMode.ImageIndex > -1) and (FOwner.GlyphMode.ImageIndex < FOwner.GlyphMode.Images.Count) then begin
    Blend := FOwner.GlyphMode.Blend;
    Grayed := FOwner.GlyphMode.Grayed;
    Images := FOwner.GlyphMode.Images;
    case CurrentState of
      0 : ImageIndex := FOwner.GlyphMode.ImageIndex;
      1 : ImageIndex := FOwner.GlyphMode.ImageIndexHot;
      2 : ImageIndex := FOwner.GlyphMode.ImageIndexPressed;
    end;
    if (ImageIndex < 0) or (ImageIndex > FOwner.GlyphMode.Images.Count - 1) then ImageIndex := FOwner.GlyphMode.ImageIndex;
    NumGlyphs := 1;
  end
  else begin
    Images := acResImgList;
    ImageIndex := FOwner.FDefBmpID;
    NumGlyphs := 3;
  end;
  SkinData.CtrlSkinState := SkinData.CtrlSkinState and not ACS_LOCKED;
end;

constructor TsEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwner := TsCustomComboEdit(AOwner);
  SkinData.SkinSection := s_SPEEDBUTTON_SMALL;
{$IFDEF CHECKXP}
  Flat := True;
{$ENDIF}
  Cursor := crArrow;
  Align := alRight;
  Width := 22; 
  ShowCaption := False;
  AnimatEvents := [aeMouseEnter, aeMouseLeave];
end;

{ TsCustomComboEdit }

constructor TsCustomComboEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SkinData.COC := COC_TsComboEdit;
  FDisabledKind := DefDisabledKind;

  FDefBmpID := -1;

  AutoSize := False;
  FDirectInput := True;
  FClickKey := scAlt + vk_Down;
  FPopupWindowAlign := pwaRight;
  FShowButton := True;

  FButton := TsEditButton.Create(Self);
  FButton.Parent := Self;
  FButton.Visible := True;
  FButton.OnClick := EditButtonClick;
  FButton.OnMouseDown := EditButtonMouseDown;
  FButton.OnMouseUp := EditButtonMouseUp;

  FGlyphMode := TsGlyphMode.Create(Self);
  FGlyphMode.Btn := FButton;

  Height := 21;
  FPopupWidth := 197;
end;

destructor TsCustomComboEdit.Destroy;
begin
  OnKeyDown := nil;
  if Assigned(FGlyphMode) then FreeAndNil(FGlyphMode);
  if Assigned(FButton) then FButton.OnClick := nil;
  inherited Destroy;
end;

procedure TsCustomComboEdit.PopupWindowShow;
var
  P: TPoint;
  Y: Integer;
  Form : TCustomForm;
begin
  if (FPopupWindow <> nil) and not (ReadOnly or DroppedDown) then begin
    FPopupWindow.Visible := False;
    FPopupWindow.Width := FPopupWidth;
    P := Parent.ClientToScreen(Point(Left, Top));
    Y := P.Y + Height;

    if Y + FPopupWindow.Height > Screen.DesktopHeight then Y := P.Y - FPopupWindow.Height;
    case FPopupWindowAlign of
      pwaRight: begin
        Dec(P.X, FPopupWindow.Width - Width);
        if P.X < Screen.DesktopLeft then Inc(P.X, FPopupWindow.Width - Width);
      end;
      pwaLeft: begin
        if P.X + FPopupWindow.Width > Screen.DesktopWidth then Dec(P.X, FPopupWindow.Width - Width);
      end;
    end;
    if P.X < Screen.DesktopLeft then P.X := Screen.Desktopleft else if P.X + FPopupWindow.Width > Screen.DesktopWidth then P.X := Screen.DesktopWidth - FPopupWindow.Width;

    Form := GetParentForm(Self);
    if Form <> nil then begin
      if (FPopupWindow is TForm) and (TForm(Form).FormStyle = fsStayOnTop) then TForm(FPopupWindow).FormStyle := fsStayOnTop;
      if Application.MainForm <> nil
        then SetWindowPos(FPopupWindow.Handle, Application.MainForm.Handle, P.X, Y, FPopupWindow.Width, FPopupWindow.Height, SWP_SHOWWINDOW)
        else SetWindowPos(FPopupWindow.Handle, Form.Handle, P.X, Y, FPopupWindow.Width, FPopupWindow.Height, SWP_SHOWWINDOW);
    end;
    FPopupWindow.Visible := True;
  end;
end;

procedure TsCustomComboEdit.Change;
begin
  if not DroppedDown then inherited Change;
end;

procedure TsCustomComboEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (FClickKey = ShortCut(Key, Shift)) and (GlyphMode.Width > 0) then begin
    EditButtonClick(Self);
    Key := 0;
  end;
  SetEditRect
end;

procedure TsCustomComboEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not RestrictDrawing then SkinData.BGChanged := True;
  inherited MouseDown(Button, Shift, X, Y);
  if DroppedDown then PopupWindowClose;
  SetEditRect
end;

procedure TsCustomComboEdit.SetEditRect;
var
  Loc: TRect;
begin
  if Parent = nil then Exit;
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));
  Loc.Bottom := ClientHeight;
  Loc.Right := ClientWidth - integer(FShowButton) * FButton.Width;
  Loc.Top := 0;
  Loc.Left := 0;
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));  {debug}
end;

function TsCustomComboEdit.GetDroppedDown : Boolean;
begin
  Result := (FPopupWindow <> nil) and FPopupWindow.Visible;
end;

procedure TsCustomComboEdit.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and
       (Message.Sender <> FPopupWindow) and
         (Message.Sender <> FButton) and
           ((FPopupWindow <> nil) and
             not FPopupWindow.ContainsControl(Message.Sender)) then PopupWindowClose;
end;

procedure TsCustomComboEdit.EditButtonClick(Sender: TObject);
begin
  if not FReadOnly then ButtonClick;
end;

procedure TsCustomComboEdit.DoClick;
begin
  EditButtonClick(Self);
end;

procedure TsCustomComboEdit.ButtonClick;
begin
//  if CanFocus then SetFocus;
  if Assigned(FOnButtonClick) then FOnButtonClick(Self);
  if DroppedDown then PopupWindowClose else begin
    PopupWindowShow;
  end;
end;


procedure TsCustomComboEdit.SelectAll;
begin
  if {DirectInput and} (Text <> '') then //inherited SelectAll;
    SendMessage(Handle, EM_SETSEL, 0, -1);//Length(Text)); // v4.44
end;

procedure TsCustomComboEdit.SetDirectInput(Value: Boolean);
begin
  inherited ReadOnly := not Value or FReadOnly;
  FDirectInput := Value;
end;

procedure TsCustomComboEdit.WMPaste(var Message: TWMPaste);
begin
  if not FDirectInput or ReadOnly then Exit;
  inherited;
end;

procedure TsCustomComboEdit.WMCut(var Message: TWMCut);
begin
  if not FDirectInput or ReadOnly then Exit;
  inherited;
end;

procedure TsCustomComboEdit.SetReadOnly(Value: Boolean);
begin
  if Value <> FReadOnly then begin
    FReadOnly := Value;
    inherited ReadOnly := Value or not FDirectInput;
//     if Assigned(Button) then Button.Enabled := not FReadOnly; for future???
  end;
end;

procedure TsCustomComboEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    SkinData.Invalidate;
  end;
end;

procedure TsCustomComboEdit.WndProc(var Message: TMessage);
var
  DC : hdc;
  b : boolean;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  if Message.MSG = SM_ALPHACMD then case Message.WParamHi of
    AC_GETBG : begin
      InitBGInfo(SkinData, PacBGInfo(Message.LParam), 0);
      PacBGInfo(Message.LParam)^.Offset.X := BorderWidth;
      PacBGInfo(Message.LParam)^.Offset.Y := PacBGInfo(Message.LParam)^.Offset.X;
      Exit;
    end;
  end
  else case Message.Msg of
    WM_ERASEBKGND : if SkinData.Skinned and IsWindowVisible(Handle) then begin
      SkinData.Updating := SkinData.Updating;
      if SkinData.Updating then Exit;
    end;
    WM_PRINT : if SkinData.Skinned then begin
      SkinData.Updating := False;
      DC := TWMPaint(Message).DC;
      if SkinData.BGChanged then PrepareCache;
      UpdateCorners(SkinData, 0);

      b := Button.Visible;
      Button.Visible := False;
      OurPaintHandler(DC);
      BitBltBorder(DC, 0, 0, SkinData.FCacheBmp.Width, SkinData.FCacheBmp.Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, BorderWidth);
      if ShowButton then Button.PaintTo(DC, Point(FButton.Left + 2, FButton.Top + 2));
      Button.Visible := b;
      Exit;
    end;
  end;
  inherited;
  if Message.MSG = SM_ALPHACMD then case Message.WParamHi of
    AC_REMOVESKIN, AC_SETNEWSKIN, AC_REFRESH : if LongWord(Message.LParam) = LongWord(SkinData.SkinManager) then begin
      AlphaBroadcast(Self, Message);
      SetEditRect
    end;
    AC_PREPARING : begin
      Message.Result := 0;
      Exit;
    end;
  end
  else case Message.Msg of
    WM_SETFOCUS : if AutoSelect then SelectAll;
    WM_SIZE, CM_FONTCHANGED : SetEditRect;
    CM_EXIT : Repaint;
  end
end;

procedure TsCustomComboEdit.Invalidate;
begin
  inherited;
  if Assigned(Button) then begin
    if not RestrictDrawing then Button.SkinData.BGChanged := True;
    if FShowButton then begin
      Button.Width := FGlyphMode.Width;
      if not Button.Visible then Button.Visible := True
    end
    else begin
      if Button.Visible then begin
        Button.Visible := False;
        Button.Width := 0
      end
    end;
    GlyphMode.Invalidate;
//    Button.Invalidate
  end;
end;

procedure TsCustomComboEdit.PopupWindowClose;
begin
  if Assigned(FPopupWindow) and TForm(FPopupWindow).Visible then TForm(FPopupWindow).Close;
end;

procedure TsCustomComboEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of Longword = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or Alignments[FAlignment] or WS_CLIPCHILDREN;
end;

procedure TsCustomComboEdit.KeyPress(var Key: Char);
begin
  if (Key = Char(VK_RETURN)) or (Key = Char(VK_ESCAPE)) then begin
    { must catch and remove this, since is actually multi-line }
    GetParentForm(Self).Perform(CM_DIALOGKEY, Byte(Key), 0);
    if Key = Char(VK_RETURN) then begin
      inherited KeyPress(Key);
      Key := #0;
      Exit;
    end;
  end;
  inherited KeyPress(Key);
end;

procedure TsCustomComboEdit.AfterConstruction;
begin
  inherited;
  SkinData.Loaded;
end;

procedure TsCustomComboEdit.Loaded;
begin
  inherited;
  SkinData.Loaded;
  SetEditRect;
end;

procedure TsCustomComboEdit.SetPopupWidth(const Value: integer);
begin
  FPopupWidth := Value;
end;

procedure TsCustomComboEdit.EditButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TsCustomComboEdit.EditButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

function TsCustomComboEdit.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TsCustomComboEdit.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    SkinData.Invalidate;
  end;
end;

procedure TsCustomComboEdit.OurPaintHandler(DC : hdc);
var
  NewDC, SavedDC : hdc;
  PS : TPaintStruct;
begin
  if not InAnimationProcess then BeginPaint(Handle, PS);
  if DC = 0 then NewDC := GetWindowDC(Handle) else NewDC := DC;
  SavedDC := SaveDC(NewDC);
  try
    if SkinData.Skinned then begin
      SkinData.Updating := SkinData.Updating;
      if not SkinData.Updating then begin
        SkinData.BGChanged := SkinData.BGChanged or SkinData.HalfVisible or GetBoolMsg(Parent, AC_GETHALFVISIBLE);
        SkinData.HalfVisible := not RectInRect(Parent.ClientRect, BoundsRect);

        if SkinData.BGChanged then PrepareCache;

        UpdateCorners(SkinData, 0);
        BitBlt(NewDC, 0, 0, Width, Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
        if (not ControlIsActive(SkinData) or (acPrintDC = NewDC)) and FButton.Visible
          then FButton.PaintTo(NewDC, Point(FButton.Left + 2, FButton.Top + 2));
{$IFDEF DYNAMICCACHE}
      if Assigned(SkinData.FCacheBmp) then FreeAndNil(SkinData.FCacheBmp);
{$ENDIF}
      end;
    end
    else begin
      PrepareCache;
      BitBlt(NewDC, 3, 3, Width - 6, Height - 6, SkinData.FCacheBmp.Canvas.Handle, 3, 3, SRCCOPY);
      if FButton.Visible then FButton.Paint;
    end;
  finally
    RestoreDC(NewDC, SavedDC);
    if DC = 0 then ReleaseDC(Handle, NewDC);
    if not InAnimationProcess then EndPaint(Handle, PS);
  end;
end;

procedure TsCustomComboEdit.PaintBorder(DC : hdc);
const
  BordWidth = 2;
var
  NewDC, SavedDC: HDC;
begin
  if not Assigned(Parent) or not Visible or not Parent.Visible or (csCreating in ControlState) or SkinData.Updating then exit;
  if DC = 0 then NewDC := GetWindowDC(Handle) else NewDC := DC;
  SavedDC := SaveDC(NewDC);
  try
    if SkinData.BGChanged {and ControlIsActive(SkinData) }then PrepareCache;
    UpdateCorners(SkinData, 0);
    BitBltBorder(NewDC, 0, 0, Width, Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, BordWidth);
    if ControlIsActive(SkinData) and FButton.Visible
      then FButton.PaintTo(NewDC, Point(FButton.Left + BordWidth, FButton.Top + BordWidth));
{$IFDEF DYNAMICCACHE}
    if Assigned(SkinData.FCacheBmp) then FreeAndNil(SkinData.FCacheBmp);
{$ENDIF}
  finally
    RestoreDC(NewDC, SavedDC);
    if DC = 0 then ReleaseDC(Handle, NewDC);
  end;
end;

procedure TsEditButton.EndInitGlyph;
begin
  if (csLoading in ComponentState) or (csCreating in ControlState) then Exit;
  SkinData.CtrlSkinState := SkinData.CtrlSkinState or ACS_LOCKED;
  Images := nil;
  SkinData.CtrlSkinState := SkinData.CtrlSkinState and not ACS_LOCKED;
end;

function TsEditButton.GlyphHeight: integer;
begin
  Result := FOwner.GlyphMode.Height;
end;

function TsEditButton.GlyphWidth: integer;
begin
  Result := FOwner.GlyphMode.Width;
end;

procedure TsEditButton.Paint;
begin
  if not FOwner.SkinData.Skinned then begin
    BeginInitGlyph;
    StdPaint(False);
    EndInitGlyph;
  end
  else if not (InAnimationProcess and (Canvas.Handle <> acPrintDC)) then inherited; // Animation under Vista fix
end;

procedure TsEditButton.PaintTo(DC: hdc; R: TPoint);
begin
  if not FOwner.SkinData.Skinned then begin
    Paint;
  end
  else begin
    PrepareCache;
    BitBlt(DC, R.x, R.y, SkinData.FCacheBmp.Width, SkinData.FCacheBmp.Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
  end;
end;

procedure TsCustomComboEdit.PaintText;
var
  R : TRect;
  bw : integer;
  aText : acString;
begin
  SkinData.FCacheBMP.Canvas.Font.Assign(Font);
  bw := BorderWidth;
  aText := EditText;
  R := Rect(bw, bw, Width - bw - integer(FShowButton) * Button.Width, Height - bw);

  if PasswordChar = #0 then begin
    acWriteTextEx(SkinData.FCacheBMP.Canvas, PacChar(aText), Enabled or SkinData.Skinned, R, DT_TOP or GetStringFlags(Self, Alignment) or DT_NOPREFIX, SkinData, ControlIsActive(SkinData))
  end
  else begin
    acFillString(aText, Length(aText), acChar(PasswordChar));
    acWriteTextEx(SkinData.FCacheBMP.Canvas, PacChar(aText), Enabled or SkinData.Skinned, R, DT_TOP or GetStringFlags(Self, Alignment) or DT_NOPREFIX,
              SkinData, ControlIsActive(SkinData));
  end;
end;

procedure TsEditButton.PrepareCache;
begin
  BeginInitGlyph;
  SkinData.InitCacheBmp;
  BitBlt(SkinData.FCacheBmp.Canvas.Handle, 0, 0, Width, Height, FOwner.SkinData.FCacheBmp.Canvas.Handle, Left + 2, Top + 2, SRCCOPY);
  DrawGlyph;
  SkinData.BGChanged := False;
  EndInitGlyph;
end;

function TsCustomComboEdit.IsActive: boolean;
begin
  Result := inherited IsActive or FButton.SkinData.FMouseAbove or ControlIsActive(FButton.SkinData);
end;

procedure TsCustomComboEdit.CMFocuseChanged(var Message: TCMFocusChanged);
begin
  if DroppedDown and (Message.Sender <> Self) then begin
    PopupWindowClose
  end;
  inherited;
end;

procedure TsCustomComboEdit.SetShowButton(const Value: boolean);
begin
  if FShowButton <> Value then begin
    FShowButton := Value;
    Invalidate
  end;
end;

procedure TsCustomComboEdit.CreateWnd;
begin
  inherited;
  SetEditRect
end;

procedure TsCustomComboEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (FGlyphMode <> nil) and (AComponent = FGlyphMode.Images) then FGlyphMode.Images := nil;
end;

end.
