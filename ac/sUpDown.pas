unit sUpDown;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, sConst, sDefaults;

type
{$IFNDEF NOTFORHELP}
  TsDrawingState = (dsDefault, dsPrevUp, dsNextUp, dsPrevDown, dsNextDown);
  TsBtnKind = (sbkTop, sbkLeft, sbkBottom, sbkRight);
{$ENDIF} // NOTFORHELP

  TsUpDown = class(TCustomUpDown)
{$IFNDEF NOTFORHELP}
  private
    FShowInaccessibility: boolean;
    FDisabledKind: TsDisabledKind;
    FDrawingState: TsDrawingState;
    FButtonSkin: TsSkinSection;
    procedure SetShowInaccessibility(const Value: boolean);
    procedure SetDisabledKind(const Value: TsDisabledKind);
    procedure SetDrawingState(const Value: TsDrawingState);
    procedure SetSkinSection(const Value: TsSkinSection);
  protected
    Pressed : boolean;
    function BtnRect : TRect;
    procedure WndProc (var Message: TMessage); override;
  public
    procedure DrawBtn(Btn : TBitmap; Kind : TsBtnKind);
    constructor Create (AOwner: TComponent); override;
    property DrawingState : TsDrawingState read FDrawingState write SetDrawingState default dsDefault;
  published
    property Align;
    property AlignButton;
    property Anchors;
    property Associate;
    property ArrowKeys;
    property Enabled;
    property Hint;
    property Min;
    property Max;
    property Increment;
    property Constraints;
    property Orientation;
    property ParentShowHint;
    property PopupMenu;
    property Position;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Thousands;
    property Visible;
    property Wrap;
    property OnChanging;
    property OnChangingEx;
    property OnContextPopup;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$ENDIF} // NOTFORHELP
    property ButtonSkin : TsSkinSection read FButtonSkin write SetSkinSection;
    property DisabledKind : TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property ShowInaccessibility : boolean read FShowInaccessibility write SetShowInaccessibility default True;
  end;

implementation

uses sStyleSimply, sPageControl, sMessages, sGraphUtils, sSkinProps, sMaskData, acntUtils, sAlphaGraph,
  sVclUtils, sSkinManager, sCommonData;

{ TsUpDown }

function TsUpDown.BtnRect: TRect;
begin
  if Orientation = udVertical then begin
    Result := Rect(0, 0, Width, Height div 2);
  end
  else begin
    Result := Rect(0, 0, Width div 2, Height);
  end;
end;

constructor TsUpDown.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Pressed := False;
  FDrawingState := dsDefault;
  FShowInaccessibility := True;
  FDisabledKind := DefDisabledKind;
  ControlStyle := ControlStyle - [csDoubleClicks];
end;

procedure TsUpDown.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    Repaint;
  end;
end;

procedure TsUpDown.SetDrawingState(const Value: TsDrawingState);
begin
  if FDrawingState <> Value then begin
    FDrawingState := Value;
    Repaint;
  end;
end;

procedure TsUpDown.SetShowInaccessibility(const Value: boolean);
begin
  if FShowInaccessibility <> Value then begin FShowInaccessibility := Value; Repaint; end;
end;

procedure TsUpDown.WndProc(var Message: TMessage);
var
  PS : TPaintStruct;
  SaveIndex, DC : hdc;
  Btn : TBitmap;
  h : integer;
  R : TRect;
  function BtnPrevDisabled : boolean; begin
    if Orientation = udVertical then Result := Position = Max else Result := Position = Min;
  end;
  function BtnNextDisabled : boolean; begin
    if Orientation = udVertical then Result := Position = Min else Result := Position = Max;
  end;
begin
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_CTRLHANDLED : begin Message.Result := 1; Exit end;
    AC_GETAPPLICATION : begin Message.Result := longint(Application); Exit end;
    AC_REMOVESKIN : if Message.LParam = LongInt(DefaultManager) then begin
      Repaint;
      exit
    end;
    AC_REFRESH : if Message.LParam = LongInt(DefaultManager) then begin
      Repaint;
      exit
    end
  end;
  Case Message.Msg of
    WM_LBUTTONDBLCLK, WM_NCLBUTTONDBLCLK : begin
      inherited;
      Pressed := True;
      if (DrawingState = dsPrevUp) and (Position < Max) then begin
        DrawingState := dsPrevDown;
      end
      else if (DrawingState = dsNextUp) and (Position > Min) then begin
        DrawingState := dsNextDown;
      end;
    end;
    WM_NCHITTEST : begin
      inherited;
      if csDesigning in ComponentState then Exit;
      R := BtnRect;
      if PtInRect(R, ScreenToClient(Point(TWMMouse(Message).XPos, TWMMouse(Message).YPos))) then begin
        if (FShowInaccessibility and BtnPrevDisabled) then begin DrawingState := dsDefault; Exit end else
        if (DrawingState <> dsPrevUp) and not Pressed then begin
          DrawingState := dsPrevUp;
          Repaint
        end
      end
      else begin
        if (FShowInaccessibility and BtnNextDisabled) then begin DrawingState := dsDefault; Exit end else
        if (DrawingState <> dsNextUp) and not Pressed then begin
          DrawingState := dsNextUp;
          Repaint
        end
      end;
//      Repaint;
    end;
    WM_LBUTTONUP : begin
      inherited;
      if csDesigning in ComponentState then Exit;
      if Pressed then begin
        Pressed := False;
        if DrawingState = dsPrevDown then begin
          if (FShowInaccessibility and (Position = Max)) then Exit;
          DrawingState := dsPrevUp
        end else begin
          if (FShowInaccessibility and (Position = Min)) then Exit;
          DrawingState := dsNextUp;
        end;
        Message.Result := 1;
      end;
    end;
    WM_LBUTTONDOWN : begin
      inherited;
      if csDesigning in ComponentState then Exit;
      Pressed := True;
      if (DrawingState = dsPrevUp) and (Position < Max) then begin
        DrawingState := dsPrevDown;
      end
      else if (DrawingState = dsNextUp) and (Position > Min) then begin
        DrawingState := dsNextDown;
      end;
      Message.Result := 1;
    end;
    CM_MOUSELEAVE : begin
      inherited;
      if csDesigning in ComponentState then Exit;
      Pressed := False;
      DrawingState := dsDefault;
    end;
    WM_NCPAINT, WM_ERASEBKGND : if Assigned(DefaultManager) and DefaultManager.Active then Exit else inherited;
    WM_PRINT : begin
      SendMessage(Handle, WM_PAINT, Message.WParam, Message.LParam);
    end;
    WM_PAINT : begin
      if Assigned(DefaultManager) and DefaultManager.SkinData.Active then with DefaultManager.ConstData do begin
        if Orientation = udVertical then begin
          if (IndexScrollTop > -1) and (IndexScrollBottom > -1) then begin
            DC := TWMPaint(Message).DC; if DC = 0 then DC := BeginPaint(Handle, PS);
            SaveIndex := SaveDC(DC);
            try
              Btn := TBitmap.Create;
              h := Height div 2;
              Btn.Height := h; Btn.Width := Width; Btn.PixelFormat := pf24bit;
              try
                DrawBtn(Btn, sbkTop);
                BitBlt(DC, 0, 0, Btn.Width, Btn.Height, Btn.Canvas.Handle, 0, 0, SRCCOPY);
                DrawBtn(Btn, sbkBottom);
                BitBlt(DC, 0, h, Btn.Width, Btn.Height, Btn.Canvas.Handle, 0, 0, SRCCOPY);
              finally
                FreeAndNil(Btn);
              end;
            finally
              RestoreDC(DC, SaveIndex);
              if TWMPaint(Message).DC = 0 then EndPaint(Handle, PS);
            end;
          end else inherited;
        end
        else begin
          if (DefaultManager.ConstData.IndexScrollLeft > -1) and (DefaultManager.ConstData.IndexScrollRight > -1) then begin
            DC := TWMPaint(Message).DC;
            SaveIndex := 0;
            if DC = 0 then begin
              DC := BeginPaint(Handle, PS);
              SaveIndex := SaveDC(DC);
            end;
            try
              Btn := TBitmap.Create;
              h := Width div 2;
              Btn.Height := Height; Btn.Width := h; Btn.PixelFormat := pf24bit;
              try
                DrawBtn(Btn, sbkLeft);
                BitBlt(DC, 0, 0, Btn.Width, Btn.Height, Btn.Canvas.Handle, 0, 0, SRCCOPY);
                DrawBtn(Btn, sbkRight);
                BitBlt(DC, h, 0, Btn.Width, Btn.Height, Btn.Canvas.Handle, 0, 0, SRCCOPY);
              finally
                FreeAndNil(Btn);
              end;
            finally
              if TWMPaint(Message).DC = 0 then begin
                RestoreDC(DC, SaveIndex);
                EndPaint(Handle, PS);
              end;
            end;
          end else inherited;
        end;
      end
      else inherited;
    end
    else inherited;
  end;
end;

procedure TsUpDown.SetSkinSection(const Value: TsSkinSection);
begin
  if FButtonSkin <> Value then begin
    FButtonSkin := Value;
    Invalidate;
  end;
end;

procedure TsUpDown.DrawBtn(Btn: TBitmap; Kind: TsBtnKind);
var
  CI : TCacheInfo;
  p : TPoint;
  State : integer;
  c : TsColor;
  R : TRect;
  sSkinIndex, {sMaskIndex, sBGIndex, sBGHotIndex, }sArrowMask, sLimPosition, XOffset, YOffset : integer;
  sSkinSection : string;
  SkinManager : TsSkinManager;
begin
  if Parent is TsPageControl then begin
    SkinManager := TsPageControl(Parent).SkinData.SkinManager;
    CI.Ready := False;
  end
  else begin
    SkinManager := DefaultManager;
    CI := GetParentCacheHwnd(Parent.Handle);
  end;
  if not Assigned(SkinManager) then Exit;
  sSkinIndex := -1;
  if ButtonSkin <> '' then begin
    sSkinSection := ButtonSkin;
    sSkinIndex := SkinManager.GetSkinIndex(sSkinSection);
  end;
  with SkinManager.ConstData do begin
    case Kind of
      sbkTop : begin
        if sSkinIndex < 0 then begin
          sSkinIndex := IndexScrollTop;
          sSkinSection := s_ScrollBtnTop;
        end;
        sArrowMask := MaskArrowTop;
        sLimPosition := Max;
        case DrawingState of dsPrevUp : State := 1; dsPrevDown : State := 2 else State := 0; end;
        XOffset := 0;
        YOffset := 0;
      end;
      sbkBottom : begin
        Btn.Height := Height - Btn.Height;
        if sSkinIndex < 0 then begin
          sSkinIndex := IndexScrollBottom;
          sSkinSection := s_ScrollBtnBottom;
        end;
        sArrowMask := MaskArrowBottom;
        sLimPosition := Min;
        case DrawingState of dsNextUp : State := 1; dsNextDown : State := 2 else State := 0 end;
        XOffset := 0;
        YOffset := Height - Btn.Height;
      end;
      sbkLeft : begin
        if sSkinIndex < 0 then begin
          sSkinIndex := IndexScrollLeft;
          sSkinSection := s_ScrollBtnLeft;
        end;
        sArrowMask := MaskArrowLeft;
        sLimPosition := Min;
        case DrawingState of dsPrevUp : State := 1; dsPrevDown : State := 2 else State := 0; end;
        XOffset := 0;
        YOffset := 0;
      end
      else begin
        Btn.Width := Width - Btn.Width;
        if sSkinIndex < 0 then begin
          sSkinIndex := IndexScrollRight;
          sSkinSection := s_ScrollBtnRight;
        end;
        sArrowMask := MaskArrowRight;
        sLimPosition := Max;
        case DrawingState of dsNextUp : State := 1; dsNextDown : State := 2 else State := 0 end;
        YOffset := 0;
        XOffset := Width - Btn.Width;
      end;
    end;

    if Assigned(SkinManager) then begin
      R := Rect(0, 0, Btn.Width, Btn.Height);
      CI := GetParentCacheHwnd(Parent.Handle);;
      PaintItem(sSkinIndex, sSkinSection, CI, True, State, R, Point(Left + XOffset, Top + YOffset), Btn, SkinManager);
    end;

    Ci.Bmp := Btn;
    CI.Ready := True;

    if (sArrowMask > -1) then begin
      if SkinManager.ma[sArrowMask].Bmp = nil then begin
        p.x := (Btn.Width - WidthOf(SkinManager.ma[sArrowMask].R) div SkinManager.ma[sArrowMask].ImageCount) div 2;
        p.y := (Btn.Height - HeightOf(SkinManager.ma[sArrowMask].R) div (1 + SkinManager.ma[sArrowMask].MaskType)) div 2;
      end
      else if (SkinManager.ma[sArrowMask].Bmp.Height div 2 < Btn.Height) then begin
        p.x := (Btn.Width - SkinManager.ma[sArrowMask].Bmp.Width div 3) div 2;
        p.y := (Btn.Height - SkinManager.ma[sArrowMask].Bmp.Height div 2) div 2;
      end;
      if (p.x < 0) or (p.y < 0) then Exit;
      DrawSkinGlyph(Btn, p, State, 1, SkinManager.ma[sArrowMask], CI);
    end;

    if not Enabled or (FShowInaccessibility and (Position = sLimPosition)) then begin
      CI := GetParentCacheHwnd(Parent.Handle);
      if not CI.Ready then begin
        c.C := ColorToRGB(TsHackedControl(Parent).Color);
        FadeBmp(Btn, Rect(0, 0, Btn.Width + 1, Btn.Height + 1), 60, c, 0, 0);
      end
      else BmpDisabledKind(Btn, FDisabledKind, Parent, CI, Point(Left + XOffset, Top + YOffset));
    end;
  end;
end;

end.
