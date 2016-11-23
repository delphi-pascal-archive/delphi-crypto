unit sFade;
{$I sDefs.inc}

interface
{$R+}

uses
  Windows, Messages, SysUtils, Classes, sCommonData, sConst, ExtCtrls, Graphics, sSkinManager;

type
  TsFadeTimer = class(TTimer)
  private
    procedure SetDirection(const Value: TFadeDirection);
    procedure SetOwnerData(const Value: TsCommonData);
    procedure TimerAction(Sender : TObject);
  public
    FDirection : TFadeDirection;
    FOwnerData: TsCommonData;
    Iterations : integer;
    FadeLevel : integer;
    BmpFrom : TBitmap;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Change;
    procedure Timer; override;
    function ToEnd : boolean;
    property Direction : TFadeDirection read FDirection write SetDirection;
    property OwnerData : TsCommonData read FOwnerData write SetOwnerData;
  end;

  TsAnimTimer = class(TTimer)
  private
    FadeInProcess : boolean;
    procedure SetOwnerData(const Value: TsCommonData);
    procedure TimerAction(Sender : TObject);
  public
    BmpFrom : TBitmap;
    TmpBmp : TBitmap;
    FOwnerData : TsCommonData;
    Iterations : integer;
    FadeLevel : integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Enabled default False;
    procedure DoFade;
    procedure Repaint;
    procedure Timer; override;
    function ToEnd : boolean;
    property OwnerData : TsCommonData read FOwnerData write SetOwnerData;
  end;

procedure AnimChange(var Timer : TsFadeTimer; CommonData : TsCommonData; Clicked : boolean; Direction : TFadeDirection = fdUp);
procedure DoChangePaint(var Timer : TsFadeTimer; CommonData : TsCommonData; Clicked : boolean; AllowAnimation : boolean; Direction : TFadeDirection = fdUp);
procedure StopFading(var Timer : TsFadeTimer; CommonData : TsCommonData);
function EventEnabled(Event : TacAnimatEvent; CurrentProperty : TacAnimatEvents; SkinManager : TsSkinManager = nil) : boolean;

implementation

uses sGraphUtils, Forms, Controls, sMaskData, sMessages, math, acntUtils, sStyleSimply, acGlow, sSkinProps, sSpeedButton;

function EventEnabled(Event : TacAnimatEvent; CurrentProperty : TacAnimatEvents; SkinManager : TsSkinManager = nil) : boolean;
begin
  if SkinManager = nil then SkinManager := DefaultManager;
  Result := (Event in CurrentProperty) or ((aeGlobalDef in CurrentProperty) and (acBtnEvents[Event] in SkinManager.AnimEffects.Buttons.Events));
end;

procedure AnimChange(var Timer : TsFadeTimer; CommonData : TsCommonData; Clicked : boolean; Direction : TFadeDirection = fdUp);
begin
  if CommonData.SkinManager.gd[CommonData.SkinIndex].FadingEnabled then begin
    if Timer = nil then begin
      Timer := TsFadeTimer.Create(CommonData.FOwnerControl);
      Timer.Enabled := False;
      Timer.FadeLevel := 1;
    end
    else Timer.FadeLevel := CommonData.SkinManager.gd[CommonData.SkinIndex].FadingIterations;
    Timer.OwnerData := CommonData;
    if Clicked then Timer.Iterations := Timer.Iterations div 2;

    if (Timer.FadeLevel >= Timer.Iterations) or (Timer.FadeLevel < 1) then Timer.FadeLevel := 1;
    CommonData.FOwnerControl.Perform(SM_ALPHACMD, MakeWParam(0, AC_PREPARECACHE), 0);
    Timer.Direction := Direction;
  end;
end;

procedure DoChangePaint(var Timer : TsFadeTimer; CommonData : TsCommonData; Clicked : boolean; AllowAnimation : boolean; Direction : TFadeDirection = fdUp);
begin
  if not CommonData.Skinned then Exit;
  if not aSkinChanging and AllowAnimation and CommonData.SkinManager.gd[CommonData.SkinIndex].FadingEnabled and
     not FadingForbidden and (CommonData.FCacheBmp <> nil) and not CommonData.BGChanged then begin
    CommonData.BGChanged := True;
    AnimChange(Timer, CommonData, Clicked, Direction)
  end
  else begin
    CommonData.BGChanged := True;
    if Timer <> nil then StopFading(Timer, CommonData);
    CommonData.FOwnerControl.Repaint;
  end;

  ShowGlowingIfNeeded(CommonData, Clicked);
end;

procedure StopFading(var Timer : TsFadeTimer; CommonData : TsCommonData);
begin
  if Assigned(Timer) then begin
    if not (csDestroying in Timer.ComponentState) then begin
      Timer.Enabled := False;
      Timer.Direction := fdNone;
      FreeAndNil(Timer);
    end;
  end;
end;

{ TsFadeTimer }

procedure TsFadeTimer.Change;
var
  b : TBitmap;
  c : TsColor;
  ControlDC, SavedDC : hdc;
begin
  if (FOwnerData.FOwnerControl = nil) or (csDestroying in FOwnerData.FOwnerControl.ComponentState) or (Iterations = 0) then Exit;
  if not FOwnerData.FOwnerControl.Enabled then begin {v4.83} Enabled := False; Iterations := 0; FOwnerData.Invalidate; Exit end;
  b := TBitmap.Create;
  c.R := IntToByte(255 - (FadeLevel shl 8) div Iterations); c.G := c.R; c.B := c.R; c.A := 0;
  b.Assign(BmpFrom);
  b.PixelFormat := pf24bit;
  b.HandleType := bmDIB;
  try
    SumBitmaps(b, FOwnerData.FCacheBmp, c);
    ControlDC := GetDC(TWinControl(FOwnerData.FOwnerControl).Handle);
    SavedDC := SaveDC(ControlDC);
    if (FOwnerData.FOwnerControl = nil) or (csDestroying in FOwnerData.FOwnerControl.ComponentState) or (Iterations = 0) then Exit;
    try
      if FOwnerData.FOwnerControl.Perform(SM_ALPHACMD, MakeWParam(0, AC_DRAWANIMAGE), longint(b)) = 0
        // if not processed by control
        then BitBlt(ControlDC, 0, 0, b.Width, b.Height, b.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      if (FOwnerData.FOwnerControl <> nil) and not (csDestroying in FOwnerData.FOwnerControl.ComponentState) then begin
        RestoreDC(ControlDC, SavedDC);
        ReleaseDC(TWinControl(FOwnerData.FOwnerControl).Handle, ControlDC);
      end;
    end;
    if Assigned(acMagnForm) then SendMessage(acMagnForm.Handle, SM_ALPHACMD, MakeWParam(0, AC_REFRESH), 0);
  finally
    FreeAndNil(b);
    inc(FadeLevel);
  end;
end;

constructor TsFadeTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BmpFrom := TBitmap.Create;
  Direction := fdNone;
  OnTimer := TimerAction;
end;

destructor TsFadeTimer.Destroy;
begin
  FreeAndNil(BmpFrom);
  inherited Destroy;
end;

procedure TsFadeTimer.SetDirection(const Value: TFadeDirection);
begin
  FDirection := Value;
  if FadeLevel < 0 then FadeLevel := 1;
  Enabled := Value <> fdNone;
end;

procedure TsFadeTimer.SetOwnerData(const Value: TsCommonData);
begin
  FOwnerData := Value;
  BmpFrom.Assign(FOwnerData.FCacheBmp);
  Interval := 10;
  Iterations := FOwnerData.SkinManager.gd[FOwnerData.SkinIndex].FadingIterations;
  case FDirection of
    fdDown : Dec(Iterations, FadeLevel);
    fdUp : Iterations := FadeLevel;
  end;
end;

procedure TsFadeTimer.Timer;
begin
  if Assigned(Self) and Assigned(FOwnerData) and not (csDestroying in ComponentState) and not ToEnd then case FDirection of
    fdUp, fdDown : Change;
  end;
end;

procedure TsFadeTimer.TimerAction(Sender: TObject);
begin
end;

function TsFadeTimer.ToEnd : boolean;
begin
  Result := False;
  try
    if (FOwnerData = nil) or (FOwnerData.FOwnerControl = nil) or (csDestroying in FOwnerData.FOwnerControl.ComponentState) or Application.Terminated then begin
      FDirection := fdNone;
    end
    else if (FadeLevel > Iterations) or (FadeLevel < 0) then begin
      FOwnerData.FOwnerControl.Perform(SM_ALPHACMD, MakeWParam(0, AC_STOPFADING), 0);
      Result := True;
    end;
  except
  end
end;

{ TsAnimTimer }

constructor TsAnimTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BmpFrom := TBitmap.Create;
  TmpBmp := TBitmap.Create;
  Enabled := False;
  FadeInProcess := False;
  OnTimer := TimerAction;
end;

destructor TsAnimTimer.Destroy;
begin
  if Assigned(BmpFrom) then FreeAndNil(BmpFrom);
  if Assigned(TmpBmp) then FreeAndNil(TmpBmp);
  inherited Destroy;
end;

procedure TsAnimTimer.DoFade;
var
  c : TsColor;
begin
  try
    if FadeInProcess or (Self.FOwnerData.FOwnerControl = nil) or (csDestroying in FOwnerData.FOwnerControl.ComponentState) or (Iterations = 0) or (FadeLevel < 0) then Exit;
    FadeInProcess := True;
    c.R := IntToByte(255 - (FadeLevel shl 8) div Iterations); 
    TmpBmp.Assign(BmpFrom);
    SumBitmaps(TmpBmp, FOwnerData.FCacheBmp, c);
    Repaint;
    if Assigned(acMagnForm) then SendMessage(acMagnForm.Handle, SM_ALPHACMD, MakeWParam(0, AC_REFRESH), 0);
  finally
    inc(FadeLevel);
  end;
  FadeInProcess := False;
end;

procedure TsAnimTimer.Repaint;
begin
  FOwnerData.FOwnerControl.Perform(SM_ALPHACMD, MakeWParam(0, AC_UPDATING), 0);
  FOwnerData.FOwnerControl.Perform(SM_ALPHACMD, MakeWParam(0, AC_INVALIDATE), 0);
end;

procedure TsAnimTimer.SetOwnerData(const Value: TsCommonData);
begin
  FOwnerData := Value;
  Interval := 10;
  Iterations := FOwnerData.SkinManager.gd[FOwnerData.SkinIndex].FadingIterations;
end;

procedure TsAnimTimer.Timer;
begin
  if not Assigned(Self) or (csDestroying in ComponentState) then Exit;
  if not ToEnd then DoFade;
end;

procedure TsAnimTimer.TimerAction(Sender: TObject);
begin
end;

function TsAnimTimer.ToEnd : boolean;
begin
  Result := True;
  if (Self = nil) or (csDestroying in ComponentState) then Exit;
  if (Self.FOwnerData.FOwnerControl = nil) or (csDestroying in FOwnerData.FOwnerControl.ComponentState) or Application.Terminated
    then else if (FadeLevel < 0) or (FadeLevel > Iterations) then begin
      FOwnerData.FOwnerControl.Perform(SM_ALPHACMD, MakeWParam(0, AC_STOPFADING), 0);
    end else Result := False;
end;

end.
