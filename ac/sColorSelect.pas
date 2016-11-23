unit sColorSelect;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Buttons, sSpeedButton;

type
  TsColorSelect = class(TsSpeedButton)
{$IFNDEF NOTFORHELP}
  private
    FColorValue : TColor;
    FOnChange : TNotifyEvent;
    FImgWidth: integer;
    FImgHeight: integer;
    FStandardDlg: boolean;
    procedure SetColorValue(const Value: TColor);
    procedure SetImgHeight(const Value: integer);
    procedure SetImgWidth(const Value: integer);
  public
    ColorDialog : TColorDialog;
    procedure AfterConstruction; override;
    constructor Create (AOwner: TComponent); override;
    procedure Click; override;
    procedure Loaded; override;
    procedure WndProc (var Message: TMessage); override;
    procedure UpdateGlyph;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
{$ENDIF} // NOTFORHELP
    property ColorValue : TColor read FColorValue write SetColorValue;
    property ImgWidth : integer read FImgWidth write SetImgWidth default 15;
    property ImgHeight : integer read FImgHeight write SetImgHeight default 15;
    property StandardDlg : boolean read FStandardDlg write FStandardDlg default False; 
  end;

implementation

uses sDialogs, sCommonData, sThirdParty, sGraphUtils, sMessages;

{ TsColorSelect }

procedure TsColorSelect.AfterConstruction;
begin
  inherited;
  UpdateGlyph;
end;

procedure TsColorSelect.Click;
var
  cd : TColorDialog;
begin
  if ColorDialog = nil then begin
    if FStandardDlg then cd := TColorDialog.Create(Self) else cd := TsColorDialog.Create(Self);
  end
  else cd := ColorDialog;
  cd.Color := ColorValue;
  if cd.Execute then begin
    ColorValue := cd.Color;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
  if ColorDialog <> cd then FreeAndNil(cd);
  inherited Click;
end;

constructor TsColorSelect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImgHeight := 15;
  FImgWidth := 15;
  FStandardDlg := False;
end;

procedure TsColorSelect.Loaded;
begin
  inherited;
  UpdateGlyph;
end;

procedure TsColorSelect.SetColorValue(const Value: TColor);
begin
  if FColorValue <> Value then begin
    FColorValue := Value;
    Glyph.Assign(nil);
    UpdateGlyph;
    if not (csLoading in ComponentState) then SkinData.Invalidate;
  end;
end;

procedure TsColorSelect.SetImgHeight(const Value: integer);
begin
  if FImgHeight <> Value then begin
    FImgHeight := Value;
    Glyph.Assign(nil);
    UpdateGlyph;
    if not (csLoading in ComponentState) then SkinData.Invalidate;
  end;
end;

procedure TsColorSelect.SetImgWidth(const Value: integer);
begin
  if FImgWidth <> Value then begin
    FImgWidth := Value;
    Glyph.Assign(nil);
    UpdateGlyph;
    if not (csLoading in ComponentState) then SkinData.Invalidate;
  end;
end;

procedure TsColorSelect.UpdateGlyph;
begin
  if (csLoading in ComponentState) then Exit;
  if (FImgHeight <> 0) and (FImgWidth <> 0) and (Glyph.Canvas.Pixels[FImgWidth div 2, FImgHeight div 2] <> ColorValue) then begin
    if SkinData.Skinned then Glyph.OnChange := nil;
    Glyph.PixelFormat := pf24bit;
    Glyph.Width := FImgWidth;
    Glyph.Height := FImgHeight;
    Glyph.Canvas.Brush.Color := ColorValue;
    Glyph.Canvas.FillRect(Rect(0, 0, FImgWidth, FImgHeight));

    // Transparent pixels in corners
    Glyph.Canvas.Pixels[0, FImgHeight - 1] := ColorToRGB(clFuchsia);
    Glyph.Canvas.Pixels[0, 0] := ColorToRGB(clFuchsia);
    Glyph.Canvas.Pixels[FImgWidth - 1, FImgHeight - 1] := ColorToRGB(clFuchsia);
    Glyph.Canvas.Pixels[FImgWidth - 1, 0] := ColorToRGB(clFuchsia);
  end;
end;

procedure TsColorSelect.WndProc(var Message: TMessage);
begin
  inherited;
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      UpdateGlyph;
    end;
  end;
end;

end.
