unit sMaskEdit;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, sCommonData, sConst, sDefaults{$IFDEF LOGGED}, sDebugMsgs{$ENDIF};

type

  TsMaskEdit = class(TMaskEdit)
{$IFNDEF NOTFORHELP}
  private
    FCommonData: TsCommonData;
    FDisabledKind: TsDisabledKind;
    FBoundLabel: TsBoundLabel;
    procedure SetDisabledKind(const Value: TsDisabledKind);
  protected
    procedure SetEditRect; virtual;
    procedure Change; override;
    procedure PaintBorder(DC : hdc); virtual;
    procedure PrepareCache;
    procedure PaintText; virtual;
    procedure OurPaintHandler(DC : hdc); virtual;
    function IsActive : boolean; virtual;
    function BorderWidth : integer;
  public
    procedure AfterConstruction; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure WndProc (var Message: TMessage); override;
  published
    property Align;
{$ENDIF} // NOTFORHELP
    property BoundLabel : TsBoundLabel read FBoundLabel write FBoundLabel;
    property DisabledKind : TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property SkinData : TsCommonData read FCommonData write FCommonData;
  end;

implementation

uses sVCLUtils, sMessages, acntUtils, sGraphUtils, sAlphaGraph, sMaskData, sSkinProps, sSkinManager;

{ TsMaskEdit }

procedure TsMaskEdit.AfterConstruction;
begin
  inherited AfterConstruction;
  FCommonData.Loaded;
end;

constructor TsMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  FCommonData := TsCommonData.Create(Self, True);
  FCommonData.COC := COC_TsEdit;
  if FCommonData.SkinSection = '' then FCommonData.SkinSection := s_Edit;
  FDisabledKind := DefDisabledKind;
  FBoundLabel := TsBoundLabel.Create(Self, FCommonData);
end;

destructor TsMaskEdit.Destroy;
begin
  FreeAndNil(FBoundLabel);
  if Assigned(FCommonData) then FreeAndNil(FCommonData);
  inherited Destroy;
end;

function TsMaskEdit.IsActive: boolean;
begin
  Result := ControlIsActive(FCommonData);
end;

procedure TsMaskEdit.Loaded;
begin
  inherited Loaded;
  FCommonData.Loaded;
end;

procedure TsMaskEdit.OurPaintHandler(DC : hdc);
var
  NewDC, SavedDC : hdc;
  PS : TPaintStruct;
begin
  if not InAnimationProcess then BeginPaint(Handle, PS);
  SavedDC := 0;
  if DC = 0 then begin
    NewDC := GetWindowDC(Handle);
    SavedDC := SaveDC(NewDC);
  end
  else NewDC := DC;
  try
    FCommonData.Updating := FCommonData.Updating;
    if not FCommonData.Updating then begin
      FCommonData.BGChanged := FCommonData.BGChanged or FCommonData.HalfVisible or GetBoolMsg(Parent, AC_GETHALFVISIBLE);
      FCommonData.HalfVisible := not RectInRect(Parent.ClientRect, BoundsRect);

      if FCommonData.BGChanged then PrepareCache;

      UpdateCorners(FCommonData, 0);
      BitBlt(NewDC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
    end;
  finally
    if DC = 0 then begin
      RestoreDC(NewDC, SavedDC);
      ReleaseDC(Handle, NewDC);
    end;
    if not InAnimationProcess then EndPaint(Handle, PS);
  end;
end;

procedure TsMaskEdit.PaintBorder(DC : hdc);
const
  BordWidth = 2;
var
  NewDC, SavedDC: HDC;
begin
  if not Assigned(Parent) or not Visible or not Parent.Visible or (csCreating in ControlState) or (BorderStyle = bsNone) then exit;
  if SkinData.Updating then Exit;
  if DC = 0 then NewDC := GetWindowDC(Handle) else NewDC := DC;
  SavedDC := SaveDC(NewDC);
  try
    if FCommonData.BGChanged then PrepareCache;
    UpdateCorners(FCommonData, 0);
    BitBltBorder(NewDC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, BordWidth);
  finally
    RestoreDC(NewDC, SavedDC);
    if DC = 0 then ReleaseDC(Handle, NewDC);
  end;
end;

procedure TsMaskEdit.PaintText;
var
  R : TRect;
  aText : acString;
  bw : integer;
begin
  aText := EditText;
  if aText = '' then Exit;

  FCommonData.FCacheBMP.Canvas.Font.Assign(Font);
  bw := BorderWidth;

  R := Rect(bw, bw, Width - bw, bw + acTextHeight(FCommonData.FCacheBMP.Canvas, aText) + 2);
//  if Ctl3d and (BorderStyle = bsSingle) then dec(R.Top);

  if PasswordChar = #0 then begin
    acWriteTextEx(FCommonData.FCacheBMP.Canvas, PacChar(aText), True, R, DT_NOPREFIX or DT_TOP or DT_EXTERNALLEADING, FCommonData, IsActive);
  end
  else begin
    acFillString(aText, Length(aText), acChar(PasswordChar));
    acWriteTextEx(FCommonData.FCacheBMP.Canvas, PacChar(aText), True, R, DT_NOPREFIX or DT_TOP or GetStringFlags(Self, taLeftJustify), FCommonData, IsActive);
  end;
end;

procedure TsMaskEdit.PrepareCache;
begin
  if SkinData.Skinned then begin
    FCommonData.InitCacheBmp;
    if BorderStyle = bsSingle
      then PaintItem(FCommonData, GetParentCache(FCommonData), True, integer(IsActive), Rect(0, 0, Width, Height), Point(Left, top), FCommonData.FCacheBmp, False)
      else PaintItemBG(FCommonData, GetParentCache(FCommonData), 0, Rect(0, 0, Width, Height), Point(Left, top), FCommonData.FCacheBmp, 0, 0);

    PaintText;

    if not Enabled then BmpDisabledKind(FCommonData.FCacheBmp, FDisabledKind, Parent, GetParentCache(FCommonData), Point(Left, Top));
    SkinData.BGChanged := False;
  end
  else begin
    FCommonData.InitCacheBmp;
    FillDC(FCommonData.FCacheBmp.Canvas.Handle, Rect(0, 0, Width, Height), Color);
    PaintText;
  end;
end;

procedure TsMaskEdit.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FCommonData.Invalidate;
  end;
end;

procedure TsMaskEdit.WndProc(var Message: TMessage);
var
  DC, SavedDC : hdc;
  bw : integer;
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
    AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FCommonData);
      SendMessage(Handle, WM_NCPaint, 0, 0);
      Repaint;
      exit
    end;
    AC_REMOVESKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FCommonData);
      Invalidate;
      exit
    end;
    AC_ENDPARENTUPDATE : if FCommonData.Updating then begin
      FCommonData.Updating := False;
      InvalidateRect(Handle, nil, True);
      SendMessage(Handle, WM_NCPAINT, 0, 0);
      Exit
    End;
  end;
  if not FCommonData.Skinned or not ControlIsReady(Self) then inherited else begin
    case Message.Msg of
      WM_ERASEBKGND, CN_DRAWITEM : begin
        if InAnimationProcess then Exit;
        SkinData.Updating := SkinData.Updating;
        if SkinData.Updating then Exit;
        inherited;
        Exit;
      end;
      WM_NCPAINT : begin
        if InAnimationProcess then Exit;
        FCommonData.Updating := FCommonData.Updating;
        if FCommonData.Updating then Exit;
        DC := GetWindowDC(Handle);
        SavedDC := SaveDC(DC);
        try
          PaintBorder(DC);
        finally
          RestoreDC(DC, SavedDC);
          ReleaseDC(Handle, DC);
        end;
        Exit;
      end;
      WM_PRINT : begin
        SkinData.Updating := False;
        DC := TWMPaint(Message).DC;
        if SkinData.BGChanged then PrepareCache;
        UpdateCorners(SkinData, 0);

        bw := BorderWidth;
        OurPaintHandler(DC);
        BitBltBorder(DC, 0, 0, SkinData.FCacheBmp.Width, SkinData.FCacheBmp.Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, bw);
        Exit;
      end;
      WM_PAINT : begin
        if not ControlIsActive(SkinData) then OurPaintHandler(TWMPaint(Message).DC) else begin
          inherited;
          PaintBorder(0);
        end;
        Exit;
      end;
      CM_COLORCHANGED, CM_CHANGED : FCommonData.BGChanged := True;
      WM_SETTEXT : if (csDesigning in ComponentState) then FCommonData.BGChanged := True;
    end;
    CommonWndProc(Message, FCommonData);
    inherited;
    case Message.Msg of
      CM_VISIBLECHANGED, CM_ENABLEDCHANGED, WM_SETFONT : FCommonData.Invalidate;
      WM_SETFOCUS : begin
        Invalidate;
      end
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

procedure TsMaskEdit.Change;
begin
  if not (csLoading in ComponentState) then begin
    inherited;
  end;
end;

procedure TsMaskEdit.SetEditRect;
begin
//
end;

function TsMaskEdit.BorderWidth: integer;
begin
  Result := integer(BorderStyle <> bsNone) * (2 + 1 * integer(Ctl3d));
end;

end.
