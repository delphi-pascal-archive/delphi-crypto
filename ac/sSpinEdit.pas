unit sSpinEdit;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses Windows, Classes, StdCtrls, ExtCtrls, Controls, Messages, SysUtils,
  Forms, Graphics, Menus, sEdit, acntUtils, sConst, buttons, sGraphUtils, sSpeedButton;

{$IFNDEF NOTFORHELP}
const
  InitRepeatPause = 400;  { pause before repeat timer (ms) }
  RepeatPause     = 100;  { pause before hint window displays (ms)}
{$ENDIF} // NOTFORHELP

type
{$IFNDEF NOTFORHELP}
  TNumGlyphs = Buttons.TNumGlyphs;

  TsTimerSpeedButton = class;
  TsSpinEdit = class;

{ TsSpinButton }
  TsSpinButton = class(TWinControl)
  private
    FOwner : TsSpinEdit;
    FUpButton: TsTimerSpeedButton;
    FDownButton: TsTimerSpeedButton;
    FFocusedButton: TsTimerSpeedButton;
    FFocusControl: TWinControl;
    FOnUpClick: TNotifyEvent;
    FOnDownClick: TNotifyEvent;
    function CreateButton: TsTimerSpeedButton;
    function GetUpGlyph: TBitmap;
    function GetDownGlyph: TBitmap;
    procedure SetUpGlyph(Value: TBitmap);
    procedure SetDownGlyph(Value: TBitmap);
    procedure BtnClick(Sender: TObject);
    procedure BtnMouseDown (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SetFocusBtn (Btn: TsTimerSpeedButton);
    procedure AdjustSize(var W, H: Integer); reintroduce;
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WndProc (var Message: TMessage); override;
  public
    procedure PaintTo(DC : hdc; P : TPoint);
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Align;
    property Anchors;
    property Constraints;
    property Ctl3D;
    property DownGlyph: TBitmap read GetDownGlyph write SetDownGlyph;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusControl: TWinControl read FFocusControl write FFocusControl;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UpGlyph: TBitmap read GetUpGlyph write SetUpGlyph;
    property Visible;
    property OnDownClick: TNotifyEvent read FOnDownClick write FOnDownClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDock;
    property OnStartDrag;
    property OnUpClick: TNotifyEvent read FOnUpClick write FOnUpClick;
  end;
{$ENDIF} // NOTFORHELP

{ TsSpinEdit }
{KJS ADDED new base class for decimal spin edit}
  TsBaseSpinEdit = class(TsEdit)
{$IFNDEF NOTFORHELP}
   private
    FButton: TsSpinButton;
    FEditorEnabled: Boolean;
    FOnUpClick: TNotifyEvent;
    FOnDownClick: TNotifyEvent;
    function GetMinHeight: Integer;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMPaste(var Message: TWMPaste);  message WM_PASTE;
    procedure WMCut(var Message: TWMCut);  message WM_CUT;
  protected
    procedure SetEditRect;
    function IsValidChar(Key: Char): Boolean; virtual;
    procedure UpClick (Sender: TObject); virtual;
    procedure DownClick (Sender: TObject); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure PrepareCache; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    property Button: TsSpinButton read FButton;
    property CharCase;
    procedure Invalidate; override;
    procedure AfterConstruction; override;
    procedure Loaded; override;
    procedure WndProc (var Message: TMessage); override;
  published
{$ENDIF} // NOTFORHELP
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property OnDownClick: TNotifyEvent read FOnDownClick write FOnDownClick;
    property OnUpClick: TNotifyEvent read FOnUpClick write FOnUpClick;
  end;


  TsSpinEdit = class(TsBaseSpinEdit)
{$IFNDEF NOTFORHELP}
  private
    FMinValue: LongInt;
    FMaxValue: LongInt;
    FIncrement: LongInt;
    function GetValue: LongInt;
    function CheckValue (NewValue: LongInt): Longint;
    procedure SetValue (NewValue: LongInt);
    procedure CMExit(var Message: TCMExit);  message CM_EXIT;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
  protected
    function IsValidChar(Key: Char): Boolean; override;
    procedure UpClick (Sender: TObject); override;
    procedure DownClick (Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
{$ENDIF} // NOTFORHELP
    property Increment: LongInt read FIncrement write FIncrement default 1;
    property MaxValue: LongInt read FMaxValue write FMaxValue;
    property MinValue: LongInt read FMinValue write FMinValue;
    property Value: LongInt read GetValue write SetValue;
  end;

  TsDecimalSpinEdit = class(TsBaseSpinEdit)
{$IFNDEF NOTFORHELP}
  private
    FMinValue: Extended;
    FMaxValue: Extended;
    FIncrement: Extended;
    fDecimalPlaces:Integer;
    FUseSystemDecSeparator: boolean;
    function GetValue: Extended;
    function CheckValue (NewValue: Extended): Extended;
    procedure SetValue (NewValue: Extended);
    procedure SetDecimalPlaces(New:Integer);
    procedure CMExit(var Message: TCMExit);  message CM_EXIT;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
  protected
    function IsValidChar(Key: Char): Boolean; override;
    procedure UpClick (Sender: TObject); override;
    procedure DownClick (Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
{$ENDIF} // NOTFORHELP
    property Increment: Extended read FIncrement write FIncrement;
    property MaxValue: Extended read FMaxValue write FMaxValue;
    property MinValue: Extended read FMinValue write FMinValue;
    property Value: Extended read GetValue write SetValue;
    property DecimalPlaces:Integer read fDecimalPlaces write SetDecimalPlaces default 2;
    property UseSystemDecSeparator : boolean read FUseSystemDecSeparator write FUseSystemDecSeparator default True;
  end;

{$IFNDEF NOTFORHELP}
{ TsTimerSpeedButton }

  TTimeBtnState = set of (tbFocusRect, tbAllowTimer);

  TsTimerSpeedButton = class(TsSpeedButton)
  private
    FRepeatTimer: TTimer;
    FTimeBtnState: TTimeBtnState;
    procedure TimerExpired(Sender: TObject);
  protected
    procedure Paint; override;
    procedure PrepareCache; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PaintTo(DC : hdc; P : TPoint);
    property TimeBtnState: TTimeBtnState read FTimeBtnState write FTimeBtnState;
  end;

  TsTimePicker = class(TsBaseSpinEdit)
  private
    fHour, fMin, fSec: integer;
    fCursor: DWord;
    function GetValue: TDateTime;
    procedure SetValue (NewValue: TDateTime);
    function CheckValue (NewValue: TDateTime): TDateTime;
    procedure CMExit(var Message: TCMExit);  message CM_EXIT;
  protected
    function IsValidChar(Key: Char): Boolean; override;
    procedure UpClick (Sender: TObject); override;
    procedure DownClick (Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Value: TDateTime read GetValue write SetValue;
    property Date: TDateTime read GetValue write SetValue;
    property Time: TDateTime read GetValue write SetValue;
  end;
{$ENDIF} // NOTFORHELP

implementation

uses sStyleSimply, Math, sSkinProps, sMessages, sCommonData, sSkinManager, sVCLUtils, sDefaults, sAlphaGraph
  {$IFDEF LOGGED}, sDebugMsgs{$ENDIF};

{ TsSpinButton }

constructor TsSpinButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption];
  FOwner := TsSpinEdit(AOwner);

  FUpButton := CreateButton;
  FDownButton := CreateButton;
  UpGlyph := nil;
  DownGlyph := nil;
  Width := 18;
  FFocusedButton := FUpButton;
end;

function TsSpinButton.CreateButton: TsTimerSpeedButton;
begin
  Result := TsTimerSpeedButton.Create(Self);
  Result.SkinData.SkinSection := s_SpeedButton_Small;
  Result.Parent := Self;
  Result.OnClick := BtnClick;
  Result.NumGlyphs := 1;
  Result.OnMouseDown := BtnMouseDown;
  Result.Visible := True;
  Result.Enabled := True;
  Result.Flat := False;
  Result.Transparent := True;
  Result.TimeBtnState := [tbAllowTimer];
end;

procedure TsSpinButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then FFocusControl := nil;
end;

procedure TsSpinButton.AdjustSize(var W, H: Integer);
begin
  if (FUpButton = nil) or (csLoading in ComponentState) or (H = 0) then Exit;
  if W < 15 then W := 15;
  FUpButton.SetBounds(0, 0, W, H div 2);
  FDownButton.SetBounds(0, FUpButton.Height, W, H - FUpButton.Height);
end;

procedure TsSpinButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  AdjustSize (W, H);
  inherited SetBounds (ALeft, ATop, W, H);
end;

procedure TsSpinButton.WMSize(var Message: TWMSize);
var
  W, H: Integer;
begin
  inherited;
  W := Width;
  H := Height;
  AdjustSize(W, H);
  if (W <> Width) or (H <> Height) then inherited SetBounds(Left, Top, W, H);
  Message.Result := 0;
end;

procedure TsSpinButton.WMSetFocus(var Message: TWMSetFocus);
begin
  FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState + [tbFocusRect];
  FFocusedButton.Invalidate;
end;

procedure TsSpinButton.WMKillFocus(var Message: TWMKillFocus);
begin
  FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState - [tbFocusRect];
  FFocusedButton.Invalidate;
end;

procedure TsSpinButton.BtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then begin
    SetFocusBtn (TsTimerSpeedButton (Sender));
    if (FFocusControl <> nil) and FFocusControl.TabStop and FFocusControl.CanFocus and (GetFocus <> FFocusControl.Handle)
      then FFocusControl.SetFocus
      else if TabStop and (GetFocus <> Handle) and CanFocus then SetFocus;
  end;
end;

procedure TsSpinButton.BtnClick(Sender: TObject);
begin
  if Sender = FUpButton then begin
    if Assigned(FOnUpClick) then FOnUpClick(Self);
  end
  else if Assigned(FOnDownClick) then FOnDownClick(Self);
end;

procedure TsSpinButton.SetFocusBtn(Btn: TsTimerSpeedButton);
begin
  if TabStop and CanFocus and  (Btn <> FFocusedButton) then begin
    FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState - [tbFocusRect];
    FFocusedButton := Btn;
    if (GetFocus = Handle) then begin
      FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState + [tbFocusRect];
      Invalidate;
    end;
  end;
end;

procedure TsSpinButton.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TsSpinButton.Loaded;
var
  W, H: Integer;
begin
  inherited Loaded;
  W := Width;
  H := Height;
  AdjustSize (W, H);
  if (W <> Width) or (H <> Height) then inherited SetBounds (Left, Top, W, H);
  FUpButton.SkinData.SkinManager := FOwner.SkinData.FSkinManager;
  FDownButton.SkinData.SkinManager := FOwner.SkinData.FSkinManager;
end;

function TsSpinButton.GetUpGlyph: TBitmap;
begin
  Result := FUpButton.Glyph;
end;

procedure TsSpinButton.SetUpGlyph(Value: TBitmap);
begin
  if Value <> nil then FUpButton.Glyph := Value else begin
    FUpButton.Glyph.Width := 9;
    FUpButton.Glyph.Height := 5;
    FUpButton.Glyph.PixelFormat := pf24bit;

    FUpButton.Glyph.Canvas.Brush.Style := bsSolid;
    FUpButton.Glyph.Canvas.Brush.Color := clFuchsia;
    FUpButton.Glyph.Canvas.FillRect(Rect(0, 0, FUpButton.Glyph.Width, FUpButton.Glyph.Height));

    FUpButton.Glyph.Canvas.Pen.Style := psSolid;
    FUpButton.Glyph.canvas.Brush.Color := clBlack;

    if FUpButton.SkinData.SkinIndex > -1
      then FUpButton.Glyph.canvas.Pen.Color := ColorToRGB(FUpButton.SkinData.SkinManager.gd[FUpButton.SkinData.SkinIndex].FontColor[1])
      else FUpButton.Glyph.canvas.Pen.Color := ColorToRGB(Font.Color);

    FUpButton.Glyph.canvas.Polygon([Point(1, 4), Point(7, 4), Point(4, 1)]);
  end;
end;

function TsSpinButton.GetDownGlyph: TBitmap;
begin
  Result := FDownButton.Glyph;
end;

procedure TsSpinButton.SetDownGlyph(Value: TBitmap);
begin
  if Value <> nil then FDownButton.Glyph := Value else begin
    FDownButton.Glyph.Width := 9;
    FDownButton.Glyph.Height := 5;
    FDownButton.Glyph.PixelFormat := pf24bit;

    FDownButton.Glyph.Canvas.Brush.Style := bsSolid;
    FDownButton.Glyph.Canvas.Brush.Color := clFuchsia;
    FDownButton.Glyph.Canvas.FillRect(Rect(0, 0, FDownButton.Glyph.Width, FDownButton.Glyph.Height));

    FDownButton.Glyph.Canvas.Pen.Style := psSolid;
    FDownButton.Glyph.canvas.Brush.Color := clBlack;
    if FDownButton.SkinData.SkinIndex > -1
      then FDownButton.Glyph.canvas.Pen.Color := ColorToRGB(FDownButton.SkinData.SkinManager.gd[FDownButton.SkinData.SkinIndex].FontColor[1])
      else FDownButton.Glyph.canvas.Pen.Color := ColorToRGB(Font.Color);

    FDownButton.Glyph.canvas.Polygon([Point(1, 1), Point(7, 1), Point(4, 4)]);
  end;
end;

procedure TsSpinButton.WndProc(var Message: TMessage);
begin
  if Message.MSG = SM_ALPHACMD then case Message.WParamHi of
    AC_GETBG : begin
      if Assigned(FOwner.SkinData) and Assigned(FOwner.SkinData.FCacheBmp) then begin
        PacBGInfo(Message.LParam).BgType := btCache;
        PacBGInfo(Message.LParam).Bmp := FOwner.SkinData.FCacheBmp;
        PacBGInfo(Message.LParam).Offset := Point(Left + 2, Top + 2);
      end
      else PacBGInfo(Message.LParam).BgType := btFill;
      PacBGInfo(Message.LParam).Color := SendMessage(FOwner.Handle, SM_ALPHACMD, MakeWParam(0, AC_GETCONTROLCOLOR), 0); //FOwner.Color;
      Exit
    end;
    AC_PREPARING : begin
      Message.Result := integer(FOwner.SkinData.FUpdating);
      Exit
    end;
  end
  else case Message.Msg of
    WM_ERASEBKGND : Exit;
//    WM_PAINT : {if not Parent.Visible and (Message.LParam <> 1) then} Exit;
  end;
  inherited;
  case Message.Msg of
    CM_ENABLEDCHANGED : begin
      SetUpGlyph(nil);
      SetDownGlyph(nil);
//      FUpButton.Enabled := Enabled;
//      FDownButton.Enabled := Enabled;
    end;
  end;
end;

procedure TsSpinButton.PaintTo(DC: hdc; P: TPoint);
begin
  FUpButton.PaintTo(DC, P);
  inc(P.Y, FUpButton.Height);
  FDownButton.PaintTo(DC, P);
end;

{ TsSpinEdit }

constructor TsBaseSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption] + [csAcceptsControls];
  FButton := TsSpinButton.Create(Self);
  FButton.Visible := False;
  FButton.Parent := Self;
  FButton.FocusControl := Self;
  FButton.OnUpClick := UpClick;
  FButton.OnDownClick := DownClick;
  FButton.SetUpGlyph(nil);
  FButton.SetDownGlyph(nil);
  FButton.Visible := True;
  ControlStyle := ControlStyle - [csAcceptsControls];
  FEditorEnabled := True;
end;

destructor TsBaseSpinEdit.Destroy;
begin
  FButton := nil;
  inherited Destroy;
end;

procedure TsBaseSpinEdit.GetChildren(Proc: TGetChildProc; Root: TComponent); begin end;

procedure TsBaseSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP:   UpClick (Self);
    VK_DOWN: DownClick (Self);
  end;
  inherited KeyDown(Key, Shift);
  SetEditRect
end;

procedure TsBaseSpinEdit.KeyPress(var Key: Char);
begin
  if not IsValidChar(Key) then begin
    Key := #0;
    MessageBeep(0)
  end;
  if Key <> #0 then inherited KeyPress(Key);
end;

function TsBaseSpinEdit.IsValidChar(Key: Char): Boolean;
begin
  Result := (Key in [DecimalSeparator, '+', '-', '0'..'9']) or ((Key < #32) and (Key <> Chr(VK_RETURN)));
  if not FEditorEnabled and Result and ((Key >= #32) or (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE))) then Result := False;
end;

procedure TsBaseSpinEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style and not WS_BORDER;
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TsBaseSpinEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TsBaseSpinEdit.SetEditRect;
var
  Loc: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));
  Loc.Bottom := ClientHeight + 1;  {+1 is workaround for windows paint bug}
  Loc.Right := Width - FButton.Width - 3;
  Loc.Top := 0;
  Loc.Left := 0;
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));  {debug}
end;

procedure TsBaseSpinEdit.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
begin
  inherited;
  MinHeight := GetMinHeight;
  if Height < MinHeight then Height := MinHeight else if FButton <> nil then begin
    FButton.SetBounds(Width - FButton.Width - 4, 0, FButton.Width, Height - 4);//6);
    SetEditRect;
  end;
end;

function TsBaseSpinEdit.GetMinHeight: Integer;
begin
  Result := 0;
end;

procedure TsBaseSpinEdit.UpClick (Sender: TObject);
begin
//
end;

procedure TsBaseSpinEdit.DownClick (Sender: TObject);
begin
//
end;

procedure TsBaseSpinEdit.WMPaste(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TsBaseSpinEdit.WMCut(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TsBaseSpinEdit.Invalidate;
begin
  inherited;
//  if not ((csDestroying in ComponentState) or (csLoading in ComponentState)) then FButton.Invalidate;
end;

procedure TsBaseSpinEdit.AfterConstruction;
begin
  inherited;
end;

procedure TsBaseSpinEdit.Loaded;
begin
  inherited;
  FButton.SetUpGlyph(nil);
  FButton.SetDownGlyph(nil);
  SetEditRect;
end;

constructor tsSpinEdit.Create(AOwner:TComponent);
begin
  inherited create(AOwner);
  FIncrement := 1;
end;

procedure TsSpinEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  if CheckValue(Value) <> Value then SetValue(Value);
end;

procedure TsSpinEdit.UpClick (Sender: TObject);
begin
  inherited;
  if ReadOnly then MessageBeep(0) else begin
    if Assigned(FOnUpClick)
      then FOnUpClick(Self)
      else Value := Value + FIncrement;
  end;
end;

procedure TsSpinEdit.DownClick (Sender: TObject);
begin
  inherited;
  if ReadOnly then MessageBeep(0) else begin
    if Assigned(FOnDownClick)
      then FOnDownClick(Self)
      else Value := Value - FIncrement;
  end;
end;


function TsSpinEdit.GetValue: LongInt;
begin
{KJS Changed from Result=-1}
  Result := fMinValue;
  if Text <> '' then begin
    try
      Result := StrToInt(Text);
    except
      Result := FMinValue;
    end;
  end;
end;

procedure TsSpinEdit.SetValue (NewValue: LongInt);
begin
  Text := IntToStr (CheckValue (NewValue));
end;

function TsSpinEdit.CheckValue (NewValue: LongInt): LongInt;
begin
  Result := NewValue;
  if (FMaxValue <> FMinValue) then begin
    if NewValue < FMinValue then Result := FMinValue else if NewValue > FMaxValue then Result := FMaxValue;
  end;
end;

function TsSpinEdit.IsValidChar(Key: Char): Boolean;
begin
  Result := (Key in ['0'..'9']) or ((Key < #32) and (Key <> Chr(VK_RETURN)));
  if not FEditorEnabled and Result and ((Key >= #32) or (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE))) then Result := False;
end;

procedure TsSpinEdit.CMMouseWheel(var Message: TCMMouseWheel);
begin
  inherited;
  if not ReadOnly then begin
    Value := Value + Increment * (Message.WheelDelta div 120);
    Message.Result := 1
  end;
end;

{tsDecimalSpinEdit}

constructor tsDecimalSpinEdit.Create(AOwner:TComponent);
begin
   inherited create(AOwner);
   FUseSystemDecSeparator := True;
   FIncrement := 1.0;
   fDecimalPlaces:=2;
end;

procedure tsDecimalSpinEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  if CheckValue(Value) <> Value then SetValue(Value);
end;

procedure tsDecimalSpinEdit.UpClick (Sender: TObject);
begin
  inherited;
  if ReadOnly then MessageBeep(0) else begin
    if Assigned(FOnUpClick)
      then FOnUpClick(Self)
      else Value := Value + FIncrement;
  end;
end;

procedure tsDecimalSpinEdit.DownClick (Sender: TObject);
begin
  inherited;
  if ReadOnly then MessageBeep(0) else begin
    if Assigned(FOnDownClick)
      then FOnDownClick(Self)
      else Value := Value - FIncrement;
  end;
end;


function tsDecimalSpinEdit.GetValue: Extended;
begin
{KJS Changed from Result=-1}
  Result := fMinValue;
  if Text <> '' then begin
    try
      Result := StringToFloat(Text);
    except
      Result := FMinValue;
    end;
  end;
end;

procedure tsDecimalSpinEdit.SetDecimalPlaces(New : Integer);
begin
  if fDecimalPlaces <> New then begin
    fDecimalPlaces := New;
    Value := CheckValue(Value);
  end;
end;

procedure tsDecimalSpinEdit.SetValue (NewValue: Extended);
var
   NewText : String;
begin
  NewText := FloatToStrF(CheckValue (NewValue), ffFixed, 18, fDecimalPlaces);
  if not (csLoading in ComponentState)
    then Text := NewText;
end;

function tsDecimalSpinEdit.CheckValue (NewValue: Extended): Extended;
begin
  Result := NewValue;
  if (FMaxValue <> FMinValue) then begin
    if NewValue < FMinValue then Result := FMinValue else if NewValue > FMaxValue then Result := FMaxValue;
  end;
end;

function tsDecimalSpinEdit.IsValidChar(Key: Char): Boolean;
var
  bIsDecSeparator : boolean;
begin
  if FUseSystemDecSeparator then bIsDecSeparator := Key = DecimalSeparator else bIsDecSeparator := Key in ['.', ','];

  Result := bIsDecSeparator or (Key in ['+', '-', '0'..'9']) or ((Key < #32) and (Key <> Chr(VK_RETURN)));

  if (bIsDecSeparator and (DecimalPlaces <= 0)) or
  (bIsDecSeparator and (Pos(Key, Text) <> 0)) or
  (bIsDecSeparator and ((Pos('+', Text)-1 >= SelStart) or (Pos('-', Text)-1 >= SelStart))) or
  ((Key in ['+', '-']) and ((SelStart <> 0) or (Pos('+', Text) <> 0) or (Pos('-', Text) <> 0)))
    then Result := false;

  if not FEditorEnabled and Result and ((Key >= #32) or (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE))) then Result := False;
end;

procedure TsDecimalSpinEdit.CMMouseWheel(var Message: TCMMouseWheel);
begin
  inherited;
  if not ReadOnly then begin
    Value := Value + Increment * (Message.WheelDelta div 120);
    Message.Result := 1
  end;
end;

{TsTimerSpeedButton}

destructor TsTimerSpeedButton.Destroy;
begin
  if FRepeatTimer <> nil then FRepeatTimer.Free;
  inherited Destroy;
end;

procedure TsTimerSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  Down := True;
  if tbAllowTimer in FTimeBtnState then begin
    if FRepeatTimer = nil then FRepeatTimer := TTimer.Create(Self);

    FRepeatTimer.OnTimer := TimerExpired;
    FRepeatTimer.Interval := InitRepeatPause;
    FRepeatTimer.Enabled  := True;
  end;
end;

procedure TsTimerSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Down := False;
  inherited MouseUp (Button, Shift, X, Y);
  if FRepeatTimer <> nil then FRepeatTimer.Enabled  := False;
end;

procedure TsTimerSpeedButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if MouseCapture then begin
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;

procedure TsTimerSpeedButton.Paint;
var
  R: TRect;
begin
  if (csDestroying in ComponentState) or (csLoading in ComponentState) then Exit;
  inherited Paint;
  if tbFocusRect in FTimeBtnState then begin
    R := Bounds(0, 0, Width, Height);
    InflateRect(R, -3, -3);
    if Down then OffsetRect(R, 1, 1);
    DrawFocusRect(Canvas.Handle, R);
  end;
end;

constructor TsTimerSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TsBaseSpinEdit.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  inherited;
  if Message.MSG = SM_ALPHACMD then case Message.WParamHi of
    AC_REMOVESKIN, AC_SETNEWSKIN, AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      FButton.FUpButton.Perform(Message.Msg, Message.WParam, Message.LParam);
      FButton.FdownButton.Perform(Message.Msg, Message.WParam, Message.LParam);
      if Message.WParamHi in [AC_REFRESH, AC_REMOVESKIN] then begin
        FButton.FUpButton.Invalidate;
        FButton.FDownButton.Invalidate;
      end
      else begin
        FButton.FUpButton.Enabled := True;
        FButton.FDownButton.Enabled := True;
      end;
      FButton.SetUpGlyph(nil);
      FButton.SetDownGlyph(nil);
      SetEditRect;
    end;
    AC_ENDPARENTUPDATE : FButton.Repaint;
    AC_GETBG : InitBGInfo(SkinData, PacBGInfo(Message.LParam), 0);
    AC_GETCONTROLCOLOR : begin
      Message.Result := GetBGColor(SkinData, 0);
      if not Enabled then begin
        TColor(Message.Result) := MixColors(GetControlColor(Parent), TColor(Message.Result), 0.5);
      end;
    end;
  end
  else case Message.MSG of
    CM_MOUSEENTER, CM_MOUSELEAVE : begin
      FButton.FUpButton.SkinData.FMouseAbove := False;
      FButton.FUpButton.Perform(SM_ALPHACMD, MakeWParam(0, AC_STOPFADING), 0);
      FButton.FUpButton.SkinData.Invalidate;
      FButton.FDownButton.SkinData.FMouseAbove := False;
      FButton.FDownButton.Perform(SM_ALPHACMD, MakeWParam(0, AC_STOPFADING), 0);
      FButton.FDownButton.SkinData.Invalidate;
    end;
    CM_ENABLEDCHANGED : begin
      FButton.Enabled := Enabled;
      if SkinData.Skinned then FButton.Visible := Enabled else begin
        FButton.FUpButton.Enabled := Enabled;
        FButton.FDownButton.Enabled := Enabled;
      end
    end;
    WM_PAINT : if SkinData.Skinned then begin
      SkinData.Updating := SkinData.Updating;
      if not SkinData.Updating and Enabled then begin
        Button.FUpButton.Perform(SM_ALPHACMD, MakeWParam(0, AC_STOPFADING), 0);
        Button.FUpButton.SkinData.BGChanged := True;
        Button.FUpButton.Repaint;            
        Button.FDownButton.Perform(SM_ALPHACMD, MakeWParam(0, AC_STOPFADING), 0);
        Button.FDownButton.SkinData.BGChanged := True;
        Button.FDownButton.Repaint;
      end;
    end;
    WM_SIZE, CM_FONTCHANGED : SetEditRect;
    WM_SETFOCUS : if AutoSelect then begin
      Self.SelectAll
    end
  end;
end;

procedure TsTimerSpeedButton.PrepareCache;
begin
  if CurrentState <> 0 then inherited else begin
    SkinData.InitCacheBmp;
    FillDC(SkinData.FCacheBmp.Canvas.Handle, Rect(0, 0, SkinData.FCacheBmp.Width, SkinData.FCacheBmp.Height), TsHackedControl(Parent.Parent).Color);
    DrawGlyph;
  end;
end;

procedure TsTimerSpeedButton.PaintTo(DC: hdc; P: TPoint);
begin
  PrepareCache;
  BitBlt(DC, P.X, P.Y, Width, Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
end;

{ TsSpinButton }

constructor TsTimePicker.Create(AOwner:TComponent);
begin
  inherited create(AOwner);
  fHour := 0;
  fMin := 0;
  fSec := 0;
  fCursor := 0;
  Text := '00:00:00';
end;

procedure TsTimePicker.CMExit(var Message: TCMExit);
begin
  inherited;
  if CheckValue(Value) <> Value then SetValue(Value);
end;

procedure TsTimePicker.UpClick (Sender: TObject);
var s: string;
begin
  inherited;
  if ReadOnly then MessageBeep(0) else if length(Text)=8 then begin
    fCursor := dWord(SendMessage(Handle,EM_GETSEL,0,0)) mod $10000;
    s := Text;
    fHour := StrToInt(copy(s,1,Pos(':',s)-1));
    s := copy(s,Pos(':',s)+1,length(s));
    fMin := StrToInt(copy(s,1,Pos(':',s)-1));
    s := copy(s,Pos(':',s)+1,length(s));
    fSec := StrToInt(s);
    if fCursor in [0,1,2] then inc(fHour);
    if fCursor in [3,4,5] then inc(fMin);
    if fCursor in [6,7,8] then inc(fSec);
    if fHour>24 then fHour := 24;
    if fMin>60 then fMin := 60;
    if fSec>60 then fSec := 60;
    s := Format('%0.2d:%0.2d:%0.2d',[fHour,fMin,fSec]);
    if (not (csLoading in ComponentState)) and (Text<>s) then begin
      Text := s;
      SendMessage(Handle,EM_SETSEL,fCursor,fCursor);
    end;
  end;
end;


procedure TsTimePicker.DownClick (Sender: TObject);
var s: string;
begin
  inherited;
  if ReadOnly then MessageBeep(0) else if length(Text)=8 then begin
    fCursor := dWord(SendMessage(Handle,EM_GETSEL,0,0)) mod $10000;
    s := Text;
    fHour := StrToInt(copy(s,1,Pos(':',s)-1));
    s := copy(s,Pos(':',s)+1,length(s));
    fMin := StrToInt(copy(s,1,Pos(':',s)-1));
    s := copy(s,Pos(':',s)+1,length(s));
    fSec := StrToInt(s);
    if fCursor in [0,1,2] then dec(fHour);
    if fCursor in [3,4,5] then dec(fMin);
    if fCursor in [6,7,8] then dec(fSec);
    if fHour<4 then fHour := 0;
    if fMin<0 then fMin := 0;
    if fSec<0 then fSec := 0;
    s := Format('%0.2d:%0.2d:%0.2d',[fHour,fMin,fSec]);
    if (not (csLoading in ComponentState)) and (Text<>s) then begin
      Text := s;
      SendMessage(Handle,EM_SETSEL,fCursor,fCursor);
    end;
  end;
end;

function TsTimePicker.GetValue: TDateTime;
var
  s: string;
begin
  Result := 0;
  if length(Text) = 8 then begin
    try
      s := Text;
      fHour := StrToInt(copy(s, 1, Pos(':', s) - 1));
      s := copy(s, Pos(':', s) + 1,length(s));
      fMin := StrToInt(copy(s, 1, Pos(':', s) - 1));
      s := copy(s,Pos(':', s) + 1, length(s));
      fSec := StrToInt(s);
      Result := EncodeTime(fHour, fMin, fSec, 0)
    except
      Result := 0;
    end;
  end;
end;

procedure TsTimePicker.SetValue (NewValue: TDateTime);
var NewText: String;
    dHour,dMin,dSec,dMSec: Word;
begin
  DecodeTime(NewValue, dHour, dMin, dSec, dMSec);
  NewText := Format('%0.2d:%0.2d:%0.2d',[dHour,dMin,dSec]);
  if not (csLoading in ComponentState) then begin
    Text := NewText;
  end;
end;

function TsTimePicker.CheckValue (NewValue: TDateTime): TDateTime;
begin
  if NewValue<0 then Result := 0 else Result := NewValue;
end;

function TsTimePicker.IsValidChar(Key: Char): Boolean;
begin
  Result := (Key in [':', '0'..'9']) or ((Key < #32) and (Key <> Chr(VK_RETURN)));
  //if Result and ((Key >= #32) or (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE))) then Result := False;
end;

procedure TsBaseSpinEdit.PrepareCache;
var
  bw : integer;
begin
  SkinData.InitCacheBmp;
  PaintItem(SkinData,
            GetParentCache(SkinData), True,
            integer(ControlIsActive(SkinData)),
            Rect(0, 0, Width, Height),
            Point(Left, top), SkinData.FCacheBmp, False);
  PaintText;

  if not Enabled then begin
    bw := integer(BorderStyle <> bsNone) * (1 + integer(Ctl3d));
    FButton.PaintTo(SkinData.FCacheBmp.Canvas.Handle, Point(FButton.Left + bw, FButton.Top + bw));
    BmpDisabledKind(SkinData.FCacheBmp, DisabledKind, Parent, GetParentCache(SkinData), Point(Left, Top));
  end;
  SkinData.BGChanged := False;       
end;

end.


