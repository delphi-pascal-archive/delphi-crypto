unit sCheckListBox;
{$I sDefs.inc}

{$T-,H+,X+}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, {$IFDEF TNTUNICODE}sListBox,{$ELSE}sAlphaListBox,{$ENDIF}sConst;

type
  TsCheckListBox = class(TsListBox)
{$IFNDEF NOTFORHELP}
  private
    FAllowGrayed: Boolean;
    FOnClickCheck: TNotifyEvent;
    FSaveStates: TList;
    FHeaderColor: TColor;
    FHeaderBackgroundColor: TColor;
    FHeaderSkin: TsSkinSection;
    procedure DrawCheck(R: TRect; AState: TCheckBoxState; AEnabled: Boolean; Bmp : TBitmap; CI : TCacheInfo); overload;
    procedure DrawCheck(R: TRect; AState: TCheckBoxState; AEnabled: Boolean; C : TCanvas); overload;
    procedure SetChecked(Index: Integer; Checked: Boolean);
    function GetChecked(Index: Integer): Boolean;
    procedure SetState(Index: Integer; AState: TCheckBoxState);
    function GetState(Index: Integer): TCheckBoxState;
    procedure ToggleClickCheck(Index: Integer);
    procedure InvalidateCheck(Index: Integer);
    function CreateWrapper(Index: Integer): TObject;
    function ExtractWrapper(Index: Integer): TObject;
    function GetWrapper(Index: Integer): TObject;
    function HaveWrapper(Index: Integer): Boolean;
    procedure WMDestroy(var Msg : TWMDestroy);message WM_DESTROY;
    function GetItemEnabled(Index: Integer): Boolean;
    procedure SetItemEnabled(Index: Integer; const Value: Boolean);
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    function GetHeader(Index: Integer): Boolean;
    procedure SetHeader(Index: Integer; const Value: Boolean);
    procedure SetHeaderBackgroundColor(const Value: TColor);
    procedure SetHeaderColor(const Value: TColor);
    procedure SetHeaderSkin(const Value: TsSkinSection);
  protected
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    function InternalGetItemData(Index: Integer): Longint; override;
    procedure InternalSetItemData(Index: Integer; AData: Longint); override;
    procedure SetItemData(Index: Integer; AData: LongInt); override;
    function GetItemData(Index: Integer): LongInt; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyPress(var Key: Char); override;
    procedure ResetContent; override;
    procedure DeleteString(Index: Integer); override;
    procedure ClickCheck; dynamic;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    function GetCheckWidth: Integer;
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
{$ENDIF} // NOTFORHELP
    property Checked[Index: Integer]: Boolean read GetChecked write SetChecked;
    property ItemEnabled[Index: Integer]: Boolean read GetItemEnabled write SetItemEnabled;
    property State[Index: Integer]: TCheckBoxState read GetState write SetState;
    property Header[Index: Integer]: Boolean read GetHeader write SetHeader;
  published
    {:@event}
    property OnClickCheck: TNotifyEvent read FOnClickCheck write FOnClickCheck;

    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    property HeaderColor: TColor read FHeaderColor write SetHeaderColor default clInfoText;
    property HeaderBackgroundColor: TColor read FHeaderBackgroundColor write SetHeaderBackgroundColor default clInfoBk;
    property HeaderSkin : TsSkinSection read FHeaderSkin write SetHeaderSkin;
{$IFNDEF NOTFORHELP}
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle default bsNone;
    property Color;
    property Columns;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight;
    property Items;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
{$ENDIF} // NOTFORHELP
  end;

implementation

uses Consts, sVclUtils, acntUtils, sMessages, sGraphUtils, sAlphaGraph, sCommonData,
  sStyleSimply {$IFDEF DELPHI6UP}, RTLConsts{$ENDIF};

type
  TsCheckListBoxDataWrapper = class
  private
    FData: LongInt;
    FState: TCheckBoxState;
    FDisabled: Boolean;
    FHeader: Boolean;
    procedure SetChecked(Check: Boolean);
    function GetChecked: Boolean;
  public
    class function GetDefaultState: TCheckBoxState;
    property Checked: Boolean read GetChecked write SetChecked;
    property State: TCheckBoxState read FState write FState;
    property Disabled: Boolean read FDisabled write FDisabled;
    property Header: Boolean read FHeader write FHeader;
  end;

var
  FCheckWidth, FCheckHeight: Integer;

function CheckWidth(cb : TsCheckListBox) : integer;
begin
  if Assigned(cb.SkinData.SkinManager) and cb.SkinData.SkinManager.IsValidImgIndex(cb.SkinData.SkinManager.ConstData.SmallCheckBoxChecked) then begin
    Result := WidthOf(cb.SkinData.SkinManager.ma[cb.SkinData.SkinManager.ConstData.SmallCheckBoxChecked].R) div cb.SkinData.SkinManager.ma[cb.SkinData.SkinManager.ConstData.SmallCheckBoxChecked].ImageCount;
  end
  else Result := FCheckWidth;
end;

function CheckHeight(cb : TsCheckListBox) : integer;
begin
  if Assigned(cb.SkinData.SkinManager) and cb.SkinData.SkinManager.IsValidImgIndex(cb.SkinData.SkinManager.ConstData.SmallCheckBoxChecked) then begin
    Result := HeightOf(cb.SkinData.SkinManager.ma[cb.SkinData.SkinManager.ConstData.SmallCheckBoxChecked].R) div (cb.SkinData.SkinManager.ma[cb.SkinData.SkinManager.ConstData.SmallCheckBoxChecked].MaskType + 1);
  end
  else Result := FCheckHeight;
end;

procedure GetCheckSize;
begin
  with TBitmap.Create do
  try
    Handle := LoadBitmap(0, PChar(OBM_CHECKBOXES));
    FCheckWidth := Width div 4;
    FCheckHeight := Height div 3;
  finally
    Free;
  end;
end;

function MakeSaveState(State: TCheckBoxState; Disabled: Boolean): TObject;
begin
  Result := TObject((Byte(State) shl 16) or Byte(Disabled));
end;

function GetSaveState(AObject: TObject): TCheckBoxState;
begin
  Result := TCheckBoxState(Integer(AObject) shr 16);
end;

function GetSaveDisabled(AObject: TObject): Boolean;
begin
  Result := Boolean(Integer(AObject) and $FF);
end;

{ TsCheckListBoxDataWrapper }

function TsCheckListBoxDataWrapper.GetChecked: Boolean;
begin
  Result := FState = cbChecked;
end;

class function TsCheckListBoxDataWrapper.GetDefaultState: TCheckBoxState;
begin
  Result := cbUnchecked;
end;

procedure TsCheckListBoxDataWrapper.SetChecked(Check: Boolean);
begin
  if Check then FState := cbChecked else FState := cbUnchecked;
end;

{ TsCheckListBox }

procedure TsCheckListBox.ClickCheck;
begin
  if Assigned(FOnClickCheck) then FOnClickCheck(Self);
end;

procedure TsCheckListBox.CNDrawItem(var Message: TWMDrawItem);
begin
  if not SkinData.Skinned then
    with Message.DrawItemStruct^ do
      if not Header[itemID] then
        if not UseRightToLeftAlignment
          then rcItem.Left := rcItem.Left + GetCheckWidth
          else rcItem.Right := rcItem.Right - GetCheckWidth;
  inherited;
end;

constructor TsCheckListBox.Create(AOwner: TComponent);
begin
  inherited;
  FHeaderColor := clInfoText;
  FHeaderBackgroundColor := clInfoBk;
end;

procedure TsCheckListBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do if Style and (LBS_OWNERDRAWFIXED or LBS_OWNERDRAWVARIABLE) = 0 then Style := Style or LBS_OWNERDRAWFIXED;
end;

procedure TsCheckListBox.CreateWnd;
begin
  inherited CreateWnd;
  if FSaveStates <> nil then begin
    FreeAndNil(FSaveStates);
  end;
end;

function TsCheckListBox.CreateWrapper(Index: Integer): TObject;
begin
  Result := TsCheckListBoxDataWrapper.Create;
  inherited SetItemData(Index, LongInt(Result));
end;

procedure TsCheckListBox.DeleteString(Index: Integer);
begin
  if HaveWrapper(Index) then GetWrapper(Index).Free;
  inherited;
end;

destructor TsCheckListBox.Destroy;
begin
  FreeAndNil(FSaveStates);
  inherited;
end;

procedure TsCheckListBox.DestroyWnd;
var
  I: Integer;
begin
  if Items.Count > 0 then begin
    FSaveStates := TList.Create;
    for I := 0 to Items.Count - 1 do FSaveStates.Add(MakeSaveState(State[I], not ItemEnabled[I]));
  end;
  inherited DestroyWnd;
end;

procedure TsCheckListBox.DrawCheck(R: TRect; AState: TCheckBoxState; AEnabled: Boolean; Bmp : TBitmap; CI : TCacheInfo);
var
  DrawState: Integer;
  DrawRect: TRect;
  SkinnedGlyph : boolean;
  C : TsColor;
begin
  SkinnedGlyph := False;
  case AState of
    cbChecked : if SkinData.SkinManager.IsValidImgIndex(SkinData.SkinManager.ConstData.SmallCheckBoxChecked) then SkinnedGlyph := True;
    cbUnChecked : if SkinData.SkinManager.IsValidImgIndex(SkinData.SkinManager.ConstData.SmallCheckBoxUnChecked) then SkinnedGlyph := True;
    cbGrayed : if SkinData.SkinManager.IsValidImgIndex(SkinData.SkinManager.ConstData.SmallCheckBoxGrayed) then SkinnedGlyph := True;
  end;

  DrawRect := R;
  DrawState := 0;
  if not SkinnedGlyph then begin
    OffsetRect(DrawRect, - DrawRect.Left, - DrawRect.Top);
    BitBlt(Bmp.Canvas.Handle, DrawRect.Left, DrawRect.Top, CheckWidth(Self) + 2, HeightOf(R), SkinData.FCacheBmp.Canvas.Handle, R.Left + 1, R.Top + 1, SRCCOPY);
    case AState of
      cbChecked: DrawState := DFCS_BUTTONCHECK or DFCS_CHECKED;
      cbUnchecked: DrawState := DFCS_BUTTONCHECK;
      else DrawState := DFCS_BUTTON3STATE or DFCS_CHECKED;
    end;
    if not AEnabled then DrawState := DrawState or DFCS_INACTIVE;
  end;
  DrawRect.Left := DrawRect.Left + (DrawRect.Right - DrawRect.Left - CheckWidth(Self)) div 2;
  DrawRect.Top := DrawRect.Top + (DrawRect.Bottom - DrawRect.Top - CheckHeight(Self)) div 2;
  DrawRect.Right := DrawRect.Left + CheckWidth(Self);
  DrawRect.Bottom := DrawRect.Top + CheckHeight(Self);

  if SkinnedGlyph then begin
    if not UseRightToLeftAlignment
      then OffsetRect(DrawRect, - DrawRect.Left, - DrawRect.Top + (HeightOf(R) - CheckHeight(Self)) div 2)
      else OffsetRect(DrawRect, 0, - DrawRect.Top + (HeightOf(R) - CheckHeight(Self)) div 2);
    case AState of 
      cbChecked : if SkinData.SkinManager.IsValidImgIndex(SkinData.SkinManager.ConstData.SmallCheckBoxChecked)
        then sAlphaGraph.DrawSkinGlyph(Bmp, DrawRect.TopLeft, 0, 1, SkinData.SkinManager.ma[SkinData.SkinManager.ConstData.SmallCheckBoxChecked], CI);
      cbUnChecked : if SkinData.SkinManager.IsValidImgIndex(SkinData.SkinManager.ConstData.SmallCheckBoxUnChecked)
        then sAlphaGraph.DrawSkinGlyph(Bmp, DrawRect.TopLeft, 0, 1, SkinData.SkinManager.ma[SkinData.SkinManager.ConstData.SmallCheckBoxUnChecked], CI);
      cbGrayed : if SkinData.SkinManager.IsValidImgIndex(SkinData.SkinManager.ConstData.SmallCheckBoxGrayed)
        then sAlphaGraph.DrawSkinGlyph(Bmp, DrawRect.TopLeft, 0, 1, SkinData.SkinManager.ma[SkinData.SkinManager.ConstData.SmallCheckBoxGrayed], CI);
    end;
    if not AEnabled then begin
      C.A := 255;
      OffsetRect(R, CI.X, CI.Y);
      BlendTransRectangle(Bmp, 0, 0, CI.Bmp, R, 0.4, C);
    end;
  end
  else DrawFrameControl(Bmp.Canvas.Handle, DrawRect, DFC_BUTTON, DrawState);
end;

procedure TsCheckListBox.DrawCheck(R: TRect; AState: TCheckBoxState; AEnabled: Boolean; C : TCanvas);
var
  DrawState: Integer;
  DrawRect: TRect;
  OldBrushColor: TColor;
  OldBrushStyle: TBrushStyle;
  OldPenColor: TColor;
  Rgn, SaveRgn: HRgn;
begin
  DrawRect := R;
  DrawRect.Left := DrawRect.Left + (DrawRect.Right - DrawRect.Left - CheckWidth(Self)) div 2;
  DrawRect.Top := DrawRect.Top + (DrawRect.Bottom - DrawRect.Top - CheckHeight(Self)) div 2;
  DrawRect.Right := DrawRect.Left + CheckWidth(Self);
  DrawRect.Bottom := DrawRect.Top + CheckHeight(Self);
  case AState of
    cbChecked: DrawState := DFCS_BUTTONCHECK or DFCS_CHECKED;
    cbUnchecked: DrawState := DFCS_BUTTONCHECK;
    else DrawState := DFCS_BUTTON3STATE or DFCS_CHECKED;
  end;
  if not AEnabled then DrawState := DrawState or DFCS_INACTIVE;

  SaveRgn := CreateRectRgn(0, 0, 0, 0);
  GetClipRgn(C.Handle, SaveRgn);
  with DrawRect do Rgn := CreateRectRgn(Left + 2, Top + 2, Right - 2, Bottom - 2);
  SelectClipRgn(C.Handle, Rgn);
  DeleteObject(Rgn);

  DrawFrameControl(C.Handle, DrawRect, DFC_BUTTON, DrawState);

  SelectClipRgn(C.Handle, SaveRgn);
  DeleteObject(SaveRgn);
  OldBrushStyle := C.Brush.Style;
  OldBrushColor := C.Brush.Color;
  OldPenColor := C.Pen.Color;
  C.Brush.Style := bsClear;
  C.Pen.Color := clBtnShadow;
  C.Rectangle(DrawRect.Left + 1, DrawRect.Top + 1, DrawRect.Right - 1, DrawRect.Bottom - 1);
  C.Brush.Style := OldBrushStyle;
  C.Brush.Color := OldBrushColor;
  C.Pen.Color := OldPenColor;
end;

procedure TsCheckListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Enable: Boolean;
  ACheckWidth: Integer;
  TempBmp : Graphics.TBitmap;
  R : TRect;
  CI : TCacheInfo;
  Flags : word;
begin
  if (Index < 0) or (Index > Items.Count - 1) then Exit;
  if SkinData.BGChanged then SendAMessage(Handle, AC_PREPARECACHE);
  ACheckWidth := GetCheckWidth;
  if SkinData.Skinned then begin
    TempBmp := CreateBmp24(WidthOf(Rect), HeightOf(Rect));
    TempBmp.Canvas.Font.Assign(Font);
    try
      if not UseRightToLeftAlignment
        then R := Classes.Rect(ACheckWidth, 0, TempBmp.Width, TempBmp.Height)
        else R := Classes.Rect(0, 0, TempBmp.Width - ACheckWidth - 2, TempBmp.Height);
//      SendAMessage(Handle, AC_GETCACHE);
      FillDC(TempBmp.Canvas.Handle, Classes.Rect(0, 0, TempBmp.Width, TempBmp.Height), Color);
//      BitBlt(TempBmp.Canvas.Handle, 0, 0, TempBmp.Width, TempBmp.Height, SkinData.FCacheBmp.Canvas.Handle, Rect.Left + ListSW.cxLeftEdge, Rect.Top + ListSW.cxLeftEdge, SRCCOPY);
      if (odSelected in State) then begin // If selected
        TempBmp.Canvas.Brush.Color := clHighlight;
        TempBmp.Canvas.Brush.Style := bsSolid;
        TempBmp.Canvas.FillRect(R);
        TempBmp.Canvas.Font.Color := clHighlightText;
      end
      else begin
        TempBmp.Canvas.Brush.Color := Color;
        TempBmp.Canvas.Brush.Style := bsClear;
        TempBmp.Canvas.Font.Color := Font.Color;
      end;
      R := Rect;
      if not UseRightToLeftAlignment
        then begin R.Left := 0; R.Right := ACheckWidth end
        else begin R.Left := Rect.Right - ACheckWidth; R.Right := Rect.Right end;
      Enable := Self.Enabled and GetItemEnabled(Index);
      if not Header[Index] then begin
        DrawCheck(R, GetState(Index), Enable, TempBmp, MakeCacheInfo(SkinData.FCacheBmp, Rect.Left + 2, Rect.Top + 2));
        Flags := DT_VCENTER or DT_NOPREFIX;
        if not UseRightToLeftAlignment then begin
          R := Classes.Rect(ACheckWidth, 0, TempBmp.Width, TempBmp.Height);
          R.Left := ACheckWidth + 2;
        end
        else begin
          R := Classes.Rect(1, 1, TempBmp.Width - ACheckWidth - 3, TempBmp.Height);
          Flags := Flags or DT_RIGHT;
//          R.Left := ACheckWidth + 2;
        end;

        if not Assigned(OnDrawItem) then begin
          if State = []
            then acWriteTextEx(TempBmp.Canvas, PacChar(Items[Index]), True, R, Flags, SkinData, ControlIsActive(SkinData))
            else acWriteText(TempBmp.Canvas, PacChar(Items[Index]), True, R, Flags);
        end;
        if not UseRightToLeftAlignment then begin
          R := Classes.Rect(ACheckWidth, 0, TempBmp.Width, TempBmp.Height);
        end
        else begin
          R := Classes.Rect(0, 0, TempBmp.Width - ACheckWidth - 2, TempBmp.Height);
        end;
        if odFocused in State then DrawFocusRect(TempBmp.Canvas.Handle, R);
      end
      else begin
        R := Classes.Rect(0, 0, TempBmp.Width, TempBmp.Height);
        if SkinData.CustomColor then begin
          Canvas.Font.Color := HeaderColor;
          Canvas.Brush.Color := HeaderBackgroundColor;
          inherited;
          exit
        end
        else
        if HeaderSkin <> '' then begin
          ACheckWidth := SkinData.SkinManager.GetSkinIndex(HeaderSkin);
          if ACheckWidth > -1 then begin
            CI := MakeCacheInfo(TempBmp);
            PaintItem(ACheckWidth, HeaderSkin, CI, True, 1, R, Point(0, 0), TempBmp);
            TempBmp.Canvas.Font.Color := SkinData.SkinManager.gd[ACheckWidth].HotFontColor[1];
          end;
          if not Assigned(OnDrawItem) then begin
            acWriteText(TempBmp.Canvas, PacChar(Items[Index]), True, R, DT_VCENTER or DT_NOPREFIX)
          end;
        end
        else begin
          if SkinData.SkinManager.ConstData.IndexGlobalInfo > -1 then begin
            TempBmp.Canvas.Brush.Color := SkinData.SkinManager.gd[SkinData.SkinManager.ConstData.IndexGlobalInfo].Color;
            TempBmp.Canvas.Font.Color := SkinData.SkinManager.GetGlobalFontColor;
          end
          else TempBmp.Canvas.Brush.Color := Color;
          TempBmp.Canvas.Brush.Style := bsSolid;
          TempBmp.Canvas.FillRect(R);
          if not Assigned(OnDrawItem) then begin
            acWriteText(TempBmp.Canvas, PacChar(Items[Index]), True, R, DT_VCENTER or DT_NOPREFIX{, SkinData, ControlIsActive(SkinData)})
          end;
        end;
        R := Classes.Rect(ACheckWidth, 0, TempBmp.Width, TempBmp.Height);
        R := Classes.Rect(0, 0, TempBmp.Width, TempBmp.Height);
        if (odFocused in State) then DrawFocusRect(TempBmp.Canvas.Handle, R);
      end;

      if not Enabled then begin
        CI := MakeCacheInfo(SkinData.FCacheBmp);
        BmpDisabledKind(TempBmp, DisabledKind, Parent, CI, Point(Rect.Left + 3, Rect.Top + 3));
      end;
      BitBlt(Canvas.Handle, Rect.Left, Rect.Top, TempBmp.Width, TempBmp.Height, TempBmp.Canvas.Handle, 0, 0, SRCCOPY);
      if Assigned(OnDrawItem) then begin
        R := Rect;
        R.Left := ACheckWidth + 2;
        OnDrawItem(Self, Index, R, State);
      end;
    finally
      FreeAndNil(TempBmp);
    end;
  end
  else begin
    Canvas.FillRect(Rect);
    if Header[Index] then begin
      Canvas.Font.Color := HeaderColor;
      Canvas.Brush.Color := HeaderBackgroundColor;
      inherited;
    end
    else begin
      inherited;
      Enable := False;
//      if Assigned(OnDrawItem) then OnDrawItem(Self, Index, Rect, State) else
      if (Index < Items.Count) and (Index > -1) then begin
        R := Rect;
        if not UseRightToLeftAlignment then begin
          R.Right := Rect.Left; R.Left := R.Right - ACheckWidth;
        end
        else begin
          R.Left := Rect.Right; R.Right := R.Left + ACheckWidth;
        end;
        Enable := Self.Enabled and GetItemEnabled(Index);
        DrawCheck(R, GetState(Index), Enable, Canvas);
      end;
      if not Enable then Canvas.Font.Color := clGrayText;
    end;
  end;
end;

function TsCheckListBox.ExtractWrapper(Index: Integer): TObject;
begin
  if Index < 0 then begin
    Result := nil;
    Exit;
  end;
  Result := TsCheckListBoxDataWrapper(inherited GetItemData(Index));
  if LB_ERR = Integer(Result) then
    raise EListError.CreateResFmt(@SListIndexError, [Index]);
  if (Result <> nil) and (not (Result is TsCheckListBoxDataWrapper)) then Result := nil;
end;

function TsCheckListBox.GetChecked(Index: Integer): Boolean;
begin
  if HaveWrapper(Index)
    then Result := TsCheckListBoxDataWrapper(GetWrapper(Index)).GetChecked
    else Result := False;
end;

function TsCheckListBox.GetCheckWidth: Integer;
begin
  Result := CheckWidth(Self) + 2;
end;

function TsCheckListBox.GetHeader(Index: Integer): Boolean;
begin
  if HaveWrapper(Index) then
    Result := TsCheckListBoxDataWrapper(GetWrapper(Index)).Header
  else
    Result := False;
end;

function TsCheckListBox.GetItemData(Index: Integer): LongInt;
begin
  Result := 0;
  if HaveWrapper(Index) then
    Result := TsCheckListBoxDataWrapper(GetWrapper(Index)).FData;
end;

function TsCheckListBox.GetItemEnabled(Index: Integer): Boolean;
begin
  Result := False;
  if (Index = -1) or (Index > Items.Count - 1) then Exit;
  if HaveWrapper(Index)
    then Result := not TsCheckListBoxDataWrapper(GetWrapper(Index)).Disabled
    else Result := True;
end;

function TsCheckListBox.GetState(Index: Integer): TCheckBoxState;
begin
  if HaveWrapper(Index)
    then Result := TsCheckListBoxDataWrapper(GetWrapper(Index)).State
    else Result := TsCheckListBoxDataWrapper.GetDefaultState;
end;

function TsCheckListBox.GetWrapper(Index: Integer): TObject;
begin
  Result := ExtractWrapper(Index);
  if Result = nil then Result := CreateWrapper(Index);
end;

function TsCheckListBox.HaveWrapper(Index: Integer): Boolean;
begin
  Result := ExtractWrapper(Index) <> nil;
end;

function TsCheckListBox.InternalGetItemData(Index: Integer): Longint;
begin
  Result := inherited GetItemData(Index);
end;

procedure TsCheckListBox.InternalSetItemData(Index, AData: Integer);
begin
  inherited SetItemData(Index, AData);
end;

procedure TsCheckListBox.InvalidateCheck(Index: Integer);
var
  R: TRect;
begin
  if not Header[Index] then begin
    R := ItemRect(Index);
    if not UseRightToLeftAlignment
      then R.Right := R.Left + GetCheckWidth
      else R.Left := R.Right - GetCheckWidth;
    InvalidateRect(Handle, @R, not (csOpaque in ControlStyle));
    UpdateWindow(Handle);
  end;
end;

procedure TsCheckListBox.KeyPress(var Key: Char);
begin
  if (Key = ' ') then ToggleClickCheck(ItemIndex) else inherited;
end;

procedure TsCheckListBox.ResetContent;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do if HaveWrapper(I) then GetWrapper(I).Free;
  inherited;
end;

procedure TsCheckListBox.SetChecked(Index: Integer; Checked: Boolean);
begin
  if Checked <> GetChecked(Index) then begin
    TsCheckListBoxDataWrapper(GetWrapper(Index)).SetChecked(Checked);
    InvalidateCheck(Index);
  end;
end;

procedure TsCheckListBox.SetHeader(Index: Integer; const Value: Boolean);
begin
  if Value <> GetHeader(Index) then begin
    TsCheckListBoxDataWrapper(GetWrapper(Index)).Header := Value;
    SkinData.Invalidate;
  end;
end;

procedure TsCheckListBox.SetHeaderBackgroundColor(const Value: TColor);
begin
  if Value <> HeaderBackgroundColor then begin
    FHeaderBackgroundColor := Value;
    SkinData.Invalidate;
  end;
end;

procedure TsCheckListBox.SetHeaderColor(const Value: TColor);
begin
  if Value <> HeaderColor then begin
    FHeaderColor := Value;
    SkinData.Invalidate;
  end;
end;

procedure TsCheckListBox.SetHeaderSkin(const Value: TsSkinSection);
begin
  if FHeaderSkin <> Value then begin
    FHeaderSkin := Value;
    SkinData.Invalidate
  end;
end;

procedure TsCheckListBox.SetItemData(Index, AData: Integer);
var
  Wrapper: TsCheckListBoxDataWrapper;
  SaveState: TObject;
begin
  Wrapper := TsCheckListBoxDataWrapper(GetWrapper(Index));
  Wrapper.FData := AData;
  if FSaveStates <> nil then
    if FSaveStates.Count > 0 then begin
      SaveState := FSaveStates[0];
      Wrapper.FState := GetSaveState(SaveState);
      Wrapper.FDisabled := GetSaveDisabled(SaveState);
      FSaveStates.Delete(0);
    end;
end;

procedure TsCheckListBox.SetItemEnabled(Index: Integer; const Value: Boolean);
begin
  if Value <> GetItemEnabled(Index) then begin
    TsCheckListBoxDataWrapper(GetWrapper(Index)).Disabled := not Value;
    InvalidateCheck(Index);
  end;
end;

procedure TsCheckListBox.SetState(Index: Integer; AState: TCheckBoxState);
begin
  if AState <> GetState(Index) then begin
    TsCheckListBoxDataWrapper(GetWrapper(Index)).State := AState;
    InvalidateCheck(Index);
  end;
end;

procedure TsCheckListBox.ToggleClickCheck(Index: Integer);
var
  State: TCheckBoxState;
begin
  if (Index >= 0) and (Index < Items.Count) and GetItemEnabled(Index) then begin
    State := Self.State[Index];
    case State of
      cbUnchecked: if AllowGrayed then State := cbGrayed else State := cbChecked;
      cbChecked: State := cbUnchecked;
      cbGrayed: State := cbChecked;
    end;
    Self.State[Index] := State;
    ClickCheck;
  end;
end;

procedure TsCheckListBox.WMDestroy(var Msg: TWMDestroy);
var
  i: Integer;
begin
  if Items <> nil then for i := 0 to Items.Count -1 do ExtractWrapper(i).Free;
  inherited;
end;

procedure TsCheckListBox.WMLButtonDown(var Message: TWMLButtonDown);
var
  Index: Integer;
begin
  inherited;
  Index := ItemAtPos(Point(Message.XPos,Message.YPos), True);
  if (Index <> -1) and GetItemEnabled(Index) then begin
    if not UseRightToLeftAlignment then begin
      if Message.XPos - ItemRect(Index).Left < GetCheckWidth then ToggleClickCheck(Index)
    end
    else begin
      Dec(Message.XPos, ItemRect(Index).Right - GetCheckWidth);
      if (Message.XPos > 0) and (Message.XPos < GetCheckWidth) then ToggleClickCheck(Index)
    end;
  end;
end;

initialization
  GetCheckSize;

finalization

end.
