unit sEdit;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  {$IFDEF TNTUNICODE}TntControls, TntActnList, TntStdCtrls, TntClasses, {$ENDIF}
  StdCtrls, sCommonData, sConst, sDefaults{$IFDEF LOGGED}, sDebugMsgs{$ENDIF};

type
{$IFDEF TNTUNICODE}
  TsEdit = class(TTntEdit)
{$ELSE}
  TsEdit = class(TEdit)
{$ENDIF}
{$IFNDEF NOTFORHELP}
  private
    FCommonData: TsCommonData;
    FDisabledKind: TsDisabledKind;
    FBoundLabel: TsBoundLabel;
    procedure SetDisabledKind(const Value: TsDisabledKind);
  protected
    procedure PaintBorder;
    procedure PrepareCache; virtual;
    procedure PaintText; virtual;
    procedure OurPaintHandler(aDC : hdc = 0);
  public
    procedure AfterConstruction; override;
    constructor Create(AOwner: TComponent); override;
    procedure CreateParams(var Params: TCreateParams); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure WndProc (var Message: TMessage); override;
  published
    property Align;
{$ENDIF} // NOTFORHELP
    property DisabledKind : TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property SkinData : TsCommonData read FCommonData write FCommonData;
    property BoundLabel : TsBoundLabel read FBoundLabel write FBoundLabel;
  end;

implementation

uses sStyleSimply, sMaskData, sVCLUtils, sMessages, sGraphUtils, sAlphaGraph, acntUtils, sSKinProps, sSkinManager;

{ TsEdit }

procedure TsEdit.AfterConstruction;
begin
  inherited AfterConstruction;
  FCommonData.Loaded;
end;

constructor TsEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  ControlStyle := ControlStyle + [csOpaque];
  FCommonData := TsCommonData.Create(Self, {$IFDEF DYNAMICCACHE} False {$ELSE} True {$ENDIF});
  FCommonData.COC := COC_TsEdit;
  FDisabledKind := DefDisabledKind;
  FBoundLabel := TsBoundLabel.Create(Self, FCommonData);
end;

destructor TsEdit.Destroy;
begin
  FreeAndNil(FBoundLabel);
  if Assigned(FCommonData) then FreeAndNil(FCommonData);
  inherited Destroy;
end;

procedure TsEdit.Loaded;
begin
  inherited Loaded;
  FCommonData.Loaded;
end;

procedure TsEdit.OurPaintHandler(aDC : hdc = 0);
var
  DC, SavedDC : hdc;
  PS : TPaintStruct;
begin
  if not InAnimationProcess then BeginPaint(Handle, PS);
  SavedDC := 0;
  if aDC = 0 then begin
    DC := GetWindowDC(Handle);
    SavedDC := SaveDC(DC);
  end
  else DC := aDC;
  FCommonData.Updating := FCommonData.Updating;
  try
    if not FCommonData.Updating then begin
      FCommonData.BGChanged := FCommonData.BGChanged or FCommonData.HalfVisible or GetBoolMsg(Parent, AC_GETHALFVISIBLE);
      FCommonData.HalfVisible := not RectInRect(Parent.ClientRect, BoundsRect);

      if FCommonData.BGChanged and not FCommonData.UrgentPainting
        then PrepareCache;
      UpdateCorners(FCommonData, 0);
      BitBlt(DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
    end;
  finally
    if aDC = 0 then begin
      RestoreDC(DC, SavedDC);
      ReleaseDC(Handle, DC);
    end;
    if not InAnimationProcess then EndPaint(Handle, PS);
  end;
end;

procedure TsEdit.PrepareCache;
begin
  FCommonData.InitCacheBmp;
  PaintItem(FCommonData,
            GetParentCache(SkinData), True,
            integer(ControlIsActive(FCommonData)),
            Rect(0, 0, Width, Height),
            Point(Left, top), FCommonData.FCacheBmp, False);
  PaintText;

  if not Enabled then BmpDisabledKind(FCommonData.FCacheBmp, FDisabledKind, Parent, GetParentCache(FCommonData), Point(Left, Top));
  FCommonData.BGChanged := False;
end;

procedure TsEdit.PaintBorder;
var
  DC, SavedDC: HDC;
  BordWidth : integer;
begin
  if InAnimationProcess then exit; 
  FCommonData.Updating := FCommonData.Updating;
  if SkinData.Updating then Exit;
  DC := GetWindowDC(Handle);
  SavedDC := SaveDC(DC);
  try
    if FCommonData.BGChanged then PrepareCache;
    BordWidth := integer(BorderStyle <> bsNone) * (1 + integer(Ctl3d));
{$IFDEF DELPHI7UP}
    if BordWidth = 0 then begin
      if BevelInner <> bvNone then inc(BordWidth);
      if BevelOuter <> bvNone then inc(BordWidth);
    end;
{$ENDIF}
    UpdateCorners(FCommonData, 0);
    BitBltBorder(DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, BordWidth);
{$IFDEF DYNAMICCACHE}
    if Assigned(FCommonData.FCacheBmp) then FreeAndNil(FCommonData.FCacheBmp);
{$ENDIF}
  finally
    RestoreDC(DC, SavedDC);
    ReleaseDC(Handle, DC);
  end;
end;

procedure TsEdit.PaintText;
var
  R : TRect;
  s : acString;
  i : integer;
  BordWidth : integer;
begin
  FCommonData.FCacheBMP.Canvas.Font.Assign(Font);
  if BorderStyle <> bsNone then BordWidth := 1 + integer(Ctl3D) else BordWidth := 0;
  BordWidth := BordWidth {$IFDEF DELPHI7UP} + integer(BevelKind <> bkNone) * (integer(BevelOuter <> bvNone) + integer(BevelInner <> bvNone)) {$ENDIF};
  R := Rect(BordWidth + 1, BordWidth + 1, Width - BordWidth, Height - BordWidth);
{$IFDEF TNTUNICODE}
  if PasswordChar <> #0 then begin
    for i := 1 to Length(Text) do s := s + PasswordChar;
  end
  else s := Text;
  dec(R.Bottom);
  dec(R.Top);
  sGraphUtils.WriteUniCode(FCommonData.FCacheBmp.Canvas, s, True, R, DT_TOP or DT_NOPREFIX, FCommonData, ControlIsActive(FCommonData) and not ReadOnly);
{$ELSE}
  if PasswordChar <> #0 then begin
    for i := 1 to Length(Text) do s := s + PasswordChar;
  end
  else s := Text;
  acWriteTextEx(FCommonData.FCacheBMP.Canvas, PacChar(s), True, R, DT_TOP or DT_SINGLELINE or DT_WORDBREAK or DT_NOPREFIX, FCommonData, ControlIsActive(FCommonData));
{$ENDIF}
end;

procedure TsEdit.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FCommonData.Invalidate;
  end;
end;

procedure TsEdit.WndProc(var Message: TMessage);
var
  DC : hdc;
  bw : integer;
  PS: TPaintStruct;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_CTRLHANDLED : begin Message.Result := 1; Exit end; 
    AC_GETAPPLICATION : begin Message.Result := longint(Application); Exit end;
    AC_REMOVESKIN : if LongWord(Message.LParam) = LongWord(SkinData.SkinManager) then begin
      CommonWndProc(Message, FCommonData);
      exit
    end;
    AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FCommonData);
      if not InAnimationProcess then RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_ERASE or RDW_FRAME);
      exit
    end;
    AC_SETNEWSKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FCommonData);
      exit
    end;
    AC_CHILDCHANGED : begin
      Message.LParam := 0; // Internal buttons not required in the repainting
      Exit;
    end;
    AC_GETCONTROLCOLOR : if not FCommonData.Skinned then begin
      Message.Result := ColorToRGB(Color);
      Exit
    end;
  end;
  if not ControlIsReady(Self) or not FCommonData.Skinned then inherited else begin
    case Message.Msg of
      WM_ERASEBKGND, CN_DRAWITEM : begin
        if InAnimationProcess then exit; 
        SkinData.Updating := SkinData.Updating;
        if SkinData.Updating then Exit;
        if Enabled then inherited;
        Exit;
      end;
      WM_NCPAINT : begin
        if not InAnimationProcess then PaintBorder;
        Exit;
      end;
      WM_PAINT : begin
        FCommonData.Updating := FCommonData.Updating;
        if InAnimationProcess or FCommonData.Updating then begin // Exit if parent is not ready yet
          BeginPaint(Handle, PS);
          EndPaint(Handle, PS);
          Exit;
        end;
        if ControlIsActive(FCommonData) then begin
          if not FCommonData.CustomColor and (Color <> FCommonData.SkinManager.gd[FCommonData.SkinIndex].HotColor) then Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].HotColor;
          if not FCommonData.CustomFont and (Font.Color <> FCommonData.SkinManager.gd[FCommonData.SkinIndex].HotFontColor[1]) then Font.Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].HotFontColor[1];
        end
        else begin
          if not FCommonData.CustomColor and (Color <> FCommonData.SkinManager.gd[FCommonData.SkinIndex].Color) then Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].Color;
          if not FCommonData.CustomFont and (Font.Color <> FCommonData.SkinManager.gd[FCommonData.SkinIndex].FontColor[1]) then Font.Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].FontColor[1];
        end;
        if Enabled then inherited else OurPaintHandler(TWMPaint(Message).DC);
        exit;
      end;
      WM_PRINT : begin
        SkinData.Updating := False;
        DC := TWMPaint(Message).DC;
        PrepareCache;
        UpdateCorners(SkinData, 0);
        try
          bw := integer(BorderStyle <> bsNone) * (1 + integer(Ctl3d));
          BitBltBorder(DC, 0, 0, SkinData.FCacheBmp.Width, SkinData.FCacheBmp.Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, bw);
          OurPaintHandler(DC);
          MoveWindowOrg(DC, bw, bw);
          IntersectClipRect(DC, 0, 0, SkinData.FCacheBmp.Width - 2 * bw, SkinData.FCacheBmp.Height - 2 * bw);
        finally
        end;
      end;
      CM_COLORCHANGED: if FCommonData.CustomColor then FCommonData.BGChanged := True; 
    end;
    if CommonWndProc(Message, FCommonData) then Exit;
    inherited;
    if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
      AC_ENDPARENTUPDATE : if FCommonData.Updating then begin
        FCommonData.Updating := False;
        Repaint;
        SendMessage(Handle, WM_NCPAINT, 0, 0);
      end;
    end
    else case Message.Msg of
      WM_KILLFOCUS, CM_EXIT: begin
        FCommonData.FFocused := False;
        FCommonData.FMouseAbove := False;
        FCommonData.BGChanged := True;
        if Visible then Repaint;
      end;
      WM_SETTEXT, CM_TEXTCHANGED, CM_VISIBLECHANGED, CM_ENABLEDCHANGED, WM_SETFONT : if not (csLoading in ComponentState) and not InAnimationProcess then begin
        FCommonData.Invalidate;
        SendMessage(Handle, WM_NCPAINT, 0, 0);
      end;
      WM_SIZE : begin
        SendMessage(Handle, WM_NCPAINT, 0, 0);
      end;
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

procedure TsEdit.CreateParams(var Params: TCreateParams);
begin
  inherited;
//  Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end;


end.
