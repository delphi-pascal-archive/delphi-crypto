unit sCalculator;
{$I sDefs.inc}

interface

uses Windows, SysUtils, sCalcUnit,
  Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Menus,
  ExtCtrls, Buttons, Clipbrd;

{$IFNDEF NOTFORHELP}
const
  ButtonChars = '0123456789_./*-+Q%R='#8'C';
{$ENDIF} // NOTFORHELP

type
{ TsCalculator }
  TsCalculator = class(TComponent)
{$IFNDEF NOTFORHELP}
  private
    FCalc: TsCalcForm;
    FValue: Double;
    FMemory: Double;
    FCaption: String;
    FPrecision: Byte;
    FBeepOnError: Boolean;
    FHelpContext: THelpContext;
    FOnChange: TNotifyEvent;
    FOnCalcKey: TKeyPressEvent;
    FOnDisplayChange: TNotifyEvent;
    function GetDisplay: Double;
  protected
    procedure Change; dynamic;
    procedure CalcKey(var Key: Char); dynamic;
    procedure DisplayChange; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CalcDisplay: Double read GetDisplay;
    property Memory: Double read FMemory;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDisplayChange: TNotifyEvent read FOnDisplayChange write FOnDisplayChange;
{$ENDIF} // NOTFORHELP
    function Execute(LeftPos : integer = -1; TopPos : integer = -1): Boolean;
  published
    property BeepOnError: Boolean read FBeepOnError write FBeepOnError default True;
    property Precision: Byte read FPrecision write FPrecision default 24;
    property Caption: string read FCaption write FCaption;
    property Value: Double read FValue write FValue;
{$IFNDEF NOTFORHELP}
    property HelpContext: THelpContext read FHelpContext write FHelpContext default 0;
{$ENDIF} // NOTFORHELP
  end;

implementation

uses sVclUtils, sMessages, sSkinManager, acntUtils;

type
  TCalcPanelLayout = (clDialog, clPopup);

{ TsCalculator }

constructor TsCalculator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPrecision := 24;
  FBeepOnError := True;
  if FCaption = '' then FCaption := 'Calculator';
end;

destructor TsCalculator.Destroy;
begin
  FOnChange := nil;
  FOnDisplayChange := nil;
  inherited Destroy;
end;

function TsCalculator.GetDisplay: Double;
begin
  if Assigned(FCalc)
    then Result := FCalc.GetDisplay
    else Result := FValue;
end;

procedure TsCalculator.CalcKey(var Key: Char);
begin
  if Assigned(FOnCalcKey) then FOnCalcKey(Self, Key);
end;

procedure TsCalculator.DisplayChange;
begin
  if Assigned(FOnDisplayChange) then FOnDisplayChange(Self);
end;

procedure TsCalculator.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TsCalculator.Execute(LeftPos : integer = -1; TopPos : integer = -1): Boolean;
var
  M : TMessage;
begin
  Result := False;
  if FCalc = nil then FCalc := TsCalcForm.Create(Self);
  FCalc.Caption := Self.Caption;
  FCalc.sDragBar1.Caption := Self.Caption;
  FCalc.FormStyle := fsStayOnTop;
  FCalc.FMemory := Self.FMemory;
  FCalc.UpdateMemoryLabel;
  FCalc.FPrecision := Maxi(2, Self.Precision);
  FCalc.FBeepOnError := Self.BeepOnError;
  if Self.FValue <> 0 then begin
    FCalc.DisplayValue := Self.FValue;
    FCalc.FStatus := csFirst;
    FCalc.FOperator := '=';
  end;
  if LeftPos <> -1 then begin
    FCalc.Position := poDesigned;
    FCalc.Left := LeftPos;
  end;
  if TopPos <> -1 then begin
    FCalc.Position := poDesigned;
    FCalc.Top := TopPos;
  end;

  M.Msg := SM_ALPHACMD;
  M.LParam := longint(DefaultManager);
  M.Result := 0;
  M.WParam := MakeWParam(0, AC_SETNEWSKIN);
  AlphaBroadCast(FCalc, M);
  SendToProvider(FCalc, M);
  M.WParam := MakeWParam(0, AC_REFRESH);
  AlphaBroadCast(FCalc, M);
  SendToProvider(FCalc, M);
  FCalc.Show;
end;

end.

