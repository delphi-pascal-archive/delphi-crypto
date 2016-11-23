unit sSplitter;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  sCommonData, Extctrls{$IFDEF LOGGED}, sDebugMsgs{$ENDIF};

type

  TsSplitter = class(TSplitter)
{$IFNDEF NOTFORHELP}
  private
    FCommonData: TsCommonData;
    FSizing: Boolean;
  protected
    procedure Paint; override;
    procedure WndProc (var Message: TMessage); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Loaded; override;
  published
    property Enabled;
    property Color;
    property ParentColor;
    property Hint;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    property Width default 6;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
{$ENDIF} // NOTFORHELP
    property SkinData : TsCommonData read FCommonData write FCommonData;
  end;

implementation

uses sConst, sVclUtils, sMaskData, sMessages, sStyleSimply, sGraphUtils, acntUtils, sSKinManager;

{ TsSplitter }

procedure TsSplitter.AfterConstruction;
begin
  inherited;
  SkinData.Loaded;
end;

constructor TsSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommonData := TsCommonData.Create(Self, False);
  FCommonData.COC := COC_TsSplitter;

  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents, csOpaque, csDoubleClicks];
  Width := 6;
  FSizing := False;
end;

destructor TsSplitter.Destroy;
begin
  if Assigned(FCommonData) then FreeAndNil(FCommonData);
  inherited Destroy;
end;

procedure TsSplitter.Loaded;
begin
  inherited Loaded;
  SkinData.Loaded;
end;

procedure TsSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i : integer;
begin
  if not ShowHintStored then begin
    AppShowHint := Application.ShowHint;
    Application.ShowHint := False;
    ShowHintStored := True;
  end;
  if AutoSnap and (Parent <> nil) then begin // Hack for standard bug with Realign procedure removing
    for i := 0 to Parent.ControlCount - 1 do begin
      case Align of
        alLeft : if Parent.Controls[i].Width = 0 then Parent.Controls[i].Left := Left;
        alTop : if Parent.Controls[i].Height = 0 then Parent.Controls[i].Top := Top;
      end;
    end;
  end;
  inherited;
  FSizing := True;
  if (ResizeStyle = rsUpdate) then Repaint;
end;

procedure TsSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FSizing := False;
  if (ResizeStyle = rsUpdate) then Repaint;
  Application.ShowHint := AppShowHint;
  ShowHintStored := False;
end;

procedure TsSplitter.Paint;
const
  XorColor = $00FFD8CE;
var
  R: TRect;
  State : integer;
  ParentBG : TacBGInfo;
begin
  if not FCommonData.Skinned then inherited else begin

    R := ClientRect;
    FCommonData.InitCacheBmp;
    State := integer(ControlIsActive(FCommonData));
    if FSizing and (ResizeStyle = rsUpdate) then State := 2;
    ParentBG.PleaseDraw := False;
    GetBGInfo(@ParentBG, Parent);
    PaintItem(FCommonData, BGInfoToCI(@ParentBG), True, State, R, Point(Left, Top), FCommonData.FCacheBmp, True);
    BitBlt(Canvas.Handle, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0,0, SRCCOPY);
    if Assigned(FCommonData.FCacheBmp) then FreeAndNil(FCommonData.FCacheBmp);

    if csDesigning in ComponentState then with Canvas do begin
      Pen.Style := psDot;
      Pen.Mode := pmXor;
      Pen.Color := XorColor;
      Brush.Style := bsClear;
      Rectangle(0, 0, ClientWidth, ClientHeight);
    end;
    if Assigned(OnPaint) then OnPaint(Self);
  end;
end;

procedure TsSplitter.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_CTRLHANDLED : begin Message.Result := 1; Exit end; // AlphaSkins supported
    AC_GETAPPLICATION : begin Message.Result := longint(Application); Exit end;
    AC_SETNEWSKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FCommonData);
      exit
    end;
    AC_REMOVESKIN : if LongWord(Message.LParam) = LongWord(SkinData.SkinManager) then begin
      CommonWndProc(Message, FCommonData);
      Repaint;
      exit
    end;
    AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FCommonData);
      Repaint;
      exit
    end
  end;
  if not ControlIsReady(Self) or not FCommonData.Skinned then inherited else begin
    if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
      AC_ENDPARENTUPDATE : if FCommonData.Updating then begin
        FCommonData.Updating := False;
        Perform(WM_PAINT, 0, 0);
        Exit
      end;
    end
    else case Message.Msg of
      WM_NCPAINT, WM_ERASEBKGND : begin
        Message.Result := 1;
        Exit;
      end;
      CM_VISIBLECHANGED, WM_SIZE, CM_ENABLEDCHANGED, WM_MOUSEWHEEL, WM_MOVE : FCommonData.BGChanged := True;
    end;
    CommonWndProc(Message, FCommonData);
    inherited;
    case Message.Msg of
      WM_SETFOCUS, CM_ENTER, WM_KILLFOCUS, CM_EXIT: begin
        FCommonData.FFocused := (Message.Msg = CM_ENTER) or (Message.Msg = WM_SETFOCUS);
        FCommonData.FMouseAbove := False;
        FCommonData.BGChanged := True;
        Repaint;
      end;
      CM_MOUSELEAVE, CM_MOUSEENTER : begin
        if not FCommonData.FFocused and not(csDesigning in ComponentState) then begin
          FCommonData.FMouseAbove := Message.Msg = CM_MOUSEENTER;
          FCommonData.BGChanged := True;
          Repaint;
        end;
      end;
    end;
  end;
end;

end.
