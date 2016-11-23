unit sFontCtrls;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
 Windows, Messages, Classes, Graphics, Controls, StdCtrls, sAlphaListBox, sComboBox;

type
{$IFNDEF NOTFORHELP}
  FontTypes = (PS, TTF, RASTER, UNKNOWN);

  TFontClass = class
    FntName : string;
    FntType : FontTypes;
  end;

  TFontsArray = array of TFontClass;

  TBitmapArray = array [0..3] of TBitmap;
  TFilterOption = (ShowTrueType, ShowPostScript, ShowRaster);
  TFilterOptions = set of TFilterOption;
  EValidateFont = procedure (Sender: TObject; Font: TFontClass; var accept:Boolean) of object;
{$ENDIF} // NOTFORHELP

  TsFontListBox = Class(TsAlphaListBox)
{$IFNDEF NOTFORHELP}
  private
    FFilterOptions : TFilterOptions;
    FOnValidateFont : EValidateFont;
    FDrawFont: boolean;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure SetDrawFont(const Value: boolean);
  protected
    procedure SetFilterOptions(Value : TFilterOptions);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
  published
    property Align;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property IntegralHeight;
    property ItemHeight;
    property Items stored False;
    property MultiSelect;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style default lbOwnerDrawVariable; 
    property TabOrder;
    property TabWidth;
    property Visible;
    property OnValidateFont:EValidateFont read FOnValidateFont write FOnValidateFont;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
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
    property OnStartDrag;
{$ENDIF} // NOTFORHELP
    property DrawFont : boolean read FDrawFont write SetDrawFont default True;
    property FilterOptions: TFilterOptions read FFilterOptions write SetFilterOptions default [ShowTrueType, ShowPostScript, ShowRaster];
  end;

  TsFontComboBox = Class(TsCustomComboBox)
{$IFNDEF NOTFORHELP}
  private
    FFilterOptions : TFilterOptions;
    FOnValidateFont : EValidateFont;
    FDrawFont: boolean;
    procedure SetDrawFont(const Value: boolean);
  protected
    procedure SetFilterOptions(Value : TFilterOptions);
    procedure PaintText; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    property Style default csOwnerDrawVariable;
  published
    property Align;
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property Items stored False;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnValidateFont : EValidateFont read FOnValidateFont write FOnValidateFont;
    property OnChange;
    property OnClick;
{$IFDEF DELPHI6UP}
    property OnCloseUp;
{$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDrag;
{$ENDIF} // NOTFORHELP
    property DrawFont : boolean read FDrawFont write SetDrawFont default True;
    property FilterOptions: TFilterOptions read FFilterOptions write SetFilterOptions default [ShowTrueType, ShowPostScript, ShowRaster];
  end;

implementation

uses SysUtils, acntUtils, sGraphUtils, sVclUtils, sCommonData, Forms, sMessages{$IFDEF LOGGED}, sDebugMsgs{$ENDIF};

var
  iC : integer;
  FBitmaps : TBitmapArray;
  fa : TFontsArray;

function EnumFontFamProc(var LogFont : TLogFont; var TextMetric : TTextMetric; FontType : Integer; Data : Pointer): Integer; StdCall;
var
  FontClass : TFontClass;
  s : string;
  i, l : integer;
begin
  Result := 1;
  s := LogFont.lfFaceName;
  l := Length(fa);
  for i := 0 to l - 1 do if (AnsiCompareText(fa[i].FntName, s) = 0) or (s = '') then Exit; // Skip duplicates Synchronize
  FontClass := TFontClass.Create;
  with FontClass do begin
    FntName := LogFont.lfFaceName;
    case FontType of
      1 : FntType := RASTER;
      2 : FntType := PS;
      4 : FntType := TTF;
      else FntType := UNKNOWN;
    end;
  end;
  SetLength(fa, Length(fa) + 1);
  fa[Length(fa) - 1] := FontClass;
end;

procedure GetFonts(Sender : TControl);
var
  cont: Boolean;
  i : integer;
  fc : TFontClass;
begin
  if Sender is TsFontListBox then with Sender as TsFontListBox do begin
    Items.BeginUpdate;
    Items.Clear;
    for i := 0 to Length(fa) - 1 do begin
      Cont := True;
      if Assigned(FOnValidateFont) then FOnValidateFont(Sender, fa[i], Cont);
      fc := fa[i];
      if Cont then with fa[i] do Case FntType of
        PS     : if ShowPostScript in TsFontListBox(Sender).FFilterOptions then Items.AddObject(FntName, fa[i]);
        TTF    : if ShowTrueType in FFilterOptions then TsFontListBox(Sender).Items.AddObject(FntName, fa[i]);
        RASTER : if ShowRaster in FFilterOptions then Items.AddObject(FntName, fc);
        else Items.AddObject(FntName, fa[i]);
      end;
    end;
    Items.EndUpdate;
  end
  else with Sender as TsFontComboBox do begin
    Items.BeginUpdate;
    Items.Clear;
    for i := 0 to Length(fa) - 1 do begin
      Cont := True;
      if Assigned(FOnValidateFont) then FOnValidateFont(Sender, fa[i], Cont);
      if Cont then with fa[i] do begin
        Case FntType of
          PS     : if ShowPostScript in FFilterOptions then Items.AddObject(FntName, fa[i]);
          TTF    : if ShowTrueType in FFilterOptions then Items.AddObject(FntName, fa[i]);
          RASTER : if ShowRaster in FFilterOptions then Items.AddObject(FntName, fa[i]);
          else Items.AddObject(FntName, fa[i]);
        end;
{$IFDEF LOGGED}
  LogLines.Add(FntName);
{$ENDIF}
      end;
    end;
    Items.EndUpdate;
  end;
end;

procedure GetAllInstalledScreenFonts;
var
  DC : HDC;
begin
  DC := GetDC(0);
  EnumFontFamilies(DC, nil, @EnumFontFamProc, 0);
  ReleaseDC(0, DC);
end;

Constructor TsFontListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Sorted     := True;
  ItemHeight := 16;
  FFilterOptions := [ShowTrueType, ShowPostScript, ShowRaster];
  FDrawFont := True;
  Style := lbOwnerDrawVariable;
end;

Constructor TsFontComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Sorted     := True;
  Style      := csOwnerDrawVariable;
  FFilterOptions  := [ShowTrueType, ShowPostScript, ShowRaster];
  FDrawFont := True;
end;

procedure TsFontListBox.SetFilterOptions(Value: TFilterOptions);
begin
  if FFilterOptions <> Value then begin
    FFilterOptions := Value;
    if not (csLoading in ComponentState) then GetFonts(Self);
  end;
end;

procedure TsFontComboBox.SetFilterOptions(Value: TFilterOptions);
begin
  if FFilterOptions <> Value then begin
    FFilterOptions := Value;
    if not (csLoading in ComponentState) then GetFonts(Self);
  end;
end;

procedure TsFontListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  tmpbitmap1, tmpbitmap2   : TBitmap;
  ARect,BRect  : TRect;
  TmpColor     : TColor;
  TmpFontClass : TFontClass;
  ts : TSize;
begin
  if (Items.Objects[Index] <> nil) then begin
    TmpFontClass := TFontClass(Items.Objects[Index]);
    tmpbitmap1 := TBitmap.Create;
    tmpbitmap1.Assign(FBitmaps[ord(TmpFontClass.FntType)]);

    if odSelected in State then TmpColor := clHighLight else TmpColor := Color;

    arect.left   := 0;
    arect.top    := 0;
    arect.bottom := tmpbitmap1.Height;
    arect.right  := tmpbitmap1.Width;

    Canvas.FillRect(Rect);
    tmpbitmap2 := TBitmap.Create;

    tmpbitmap2.Height      := tmpbitmap1.Height;
    tmpbitmap2.Width       := tmpbitmap1.Width;
    tmpbitmap2.canvas.Brush.Color := TmpColor;
    tmpbitmap2.canvas.BrushCopy(ARect, tmpbitmap1, ARect, clWhite); // white is transparent in bitmap

    brect.left   := rect.left;
    brect.top    := rect.top;
    brect.bottom := rect.bottom;
    brect.right  := rect.bottom - rect.top + rect.left;

    Canvas.CopyRect(bRect, TmpBitmap2.Canvas, ARect);
    Rect.Left := Rect.Left + brect.right;
    if DrawFont then Canvas.Font.Name := TmpFontClass.FntName;

    GetTextExtentPoint32(Canvas.Handle, PChar(TmpFontClass.FntName), Length(TmpFontClass.FntName), ts);

    Canvas.Brush.Style := bsClear;
    Canvas.TextOut(Rect.Left + 2, Rect.Top + (HeightOf(Rect) - ts.cy) div 2, TmpFontClass.FntName);

    FreeAndNil(tmpbitmap2);
    FreeAndNil(tmpbitmap1);
  end;
end;

procedure TsFontComboBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  tmpbitmap1, tmpbitmap2   : TBitmap;
  ARect,BRect  : TRect;
  TmpColor     : TColor;
  TmpFontClass : TFontClass;
  ts : TSize;
begin
  if (Index >= 0) and (Index < Items.Count) and (Items.Objects[Index] <> nil) then begin
    TmpFontClass := TFontClass(Items.Objects[Index]);
    tmpbitmap1 := TBitmap.Create;
    tmpbitmap1.Assign(FBitmaps[ord(TmpFontClass.FntType)]);
    if odSelected in State then TmpColor := ColorToRGB(clHighLight) else TmpColor := ColorToRGB(Color);

    arect.left   := 0;
    arect.top    := 0;
    arect.bottom := tmpbitmap1.Height;
    arect.right  := tmpbitmap1.Width;

    Canvas.Brush.Color := TmpColor;
    Canvas.FillRect(Rect);
    tmpbitmap2 := TBitmap.Create;
    tmpbitmap2.Height      := tmpbitmap1.Height;
    tmpbitmap2.Width       := tmpbitmap1.Width;
    tmpbitmap2.canvas.Brush.Color := TmpColor;
    tmpbitmap2.canvas.BrushCopy(ARect, tmpbitmap1, ARect, clWhite); // white is transparent in bitmap

    brect.left   := rect.left;
    brect.top    := rect.top;
    brect.bottom := rect.bottom;
    brect.right  := rect.bottom - rect.top + rect.left;

    Canvas.CopyRect(bRect, TmpBitmap2.Canvas, ARect);
    Rect.Left := Rect.Left + brect.right;
    if DrawFont then Canvas.Font.Name := TmpFontClass.FntName;
    if odSelected in State then Canvas.Font.Color := ColorToRGB(clHighlightText) else Canvas.Font.Color := Font.Color;

    GetTextExtentPoint32(Canvas.Handle, PChar(TmpFontClass.FntName), Length(TmpFontClass.FntName), ts);
    Canvas.Brush.Style := bsClear;
    Canvas.TextRect(Rect, Rect.Left + 1, Rect.Top + (HeightOf(Rect) - ts.cy) div 2, TmpFontClass.FntName);

    FreeAndNil(tmpbitmap2);
    FreeAndNil(tmpbitmap1);
  end
  else begin
    if odSelected in State then TmpColor := clHighLight else TmpColor := Color;
    FillDC(Canvas.Handle, Rect, ColorToRGB(TmpColor));
  end;
end;

procedure TsFontListBox.CMFontChanged(var Message: TMessage);
begin
  SkinData.FCacheBMP.Canvas.Font.Assign(Font);
  ItemHeight := acTextWidth(SkinData.FCacheBMP.Canvas, 'Wy');
  inherited;
end;

procedure TsFontListBox.SetDrawFont(const Value: boolean);
begin
  if FDrawFont <> Value then begin
    FDrawFont := Value;
    if not (csLoading in ComponentState) then Invalidate;
  end;
end;

procedure TsFontComboBox.SetDrawFont(const Value: boolean);
begin
  if FDrawFont <> Value then begin
    FDrawFont := Value;
    if not (csLoading in ComponentState) then Invalidate;
  end;
end;

procedure TsFontComboBox.PaintText;
{var
  tmpbitmap1,
  tmpbitmap2   : TBitmap;
  ARect,BRect  : TRect;
  TmpColor     : TColor;
  TmpFontClass : TFontClass;
  R : TRect;}
begin
{  TmpColor := 0; // !!!
  if Focused then inherited else begin
    R := Rect(3, 3, Width - 3 - GetSystemMetrics(SM_CXVSCROLL), Height - 3);
    if itemIndex < 0 then Exit;
    TmpFontClass := TFontClass(Items.Objects[ItemIndex]);
    tmpbitmap1 := TBitmap.Create;
    tmpbitmap1.Assign(FBitmaps[ord(TmpFontClass.FntType)]);
    with arect do begin
      left   := 0;
      top    := 0;
      bottom := tmpbitmap1.Height;
      right  := tmpbitmap1.Width;
    end;
    tmpbitmap2 := TBitmap.Create;
    with tmpbitmap2, canvas do begin
      Height      := tmpbitmap1.Height;
      Width       := tmpbitmap1.Width;
      Brush.Color := TmpColor;
      BrushCopy(ARect, tmpbitmap1, ARect, clLime);
    end;
    with brect do begin
      left   := r.left;
      top    := r.top;
      bottom := r.bottom;
      right  := r.bottom - r.top + r.left;
    end;
    with SkinData.FCacheBmp.Canvas do begin
      CopyRect(bRect, TmpBitmap2.Canvas, ARect);
      R.Left := R.Left + brect.right + 2;
      SkinData.FCacheBMP.Canvas.Font.Assign(Self.Font);
      if DrawFont then SkinData.FCacheBMP.Canvas.Font.Name := TmpFontClass.FntName;
      WriteTextEx(SkinData.FCacheBMP.Canvas, PChar(Text), True, R, DT_NOPREFIX or DT_TOP or DT_SINGLELINE or GetStringFlags(Self, Alignment),
                  SkinData, ControlIsActive(SkinData));
  //    TextRect(R, R.Left + 2, R.Top + 1, TmpFontClass.FntName);
    end;
    FreeAndNil(tmpbitmap2);
    FreeAndNil(tmpbitmap1);
  end;
}
end;

procedure TsFontListBox.MeasureItem(Index: Integer; var Height: Integer);
var
  f : TFont;
begin
  inherited MeasureItem(Index, Height);
Exit; // v5.44
  if (Items.Count < 1) or (Items.Objects[Index] = nil) then Exit;
  if DrawFont and (Index > -1) then begin
    f := TFont.Create;
    f.Name := Items[Index];
    f.Size := Font.Size;
    Height := GetFontHeight(f.Handle);
    FreeAndNil(f);
  end;
end;

procedure TsFontComboBox.MeasureItem(Index: Integer; var Height: Integer);
var
  f : TFont;
begin
  inherited MeasureItem(Index, Height);
Exit; // v5.44
  if DrawFont and (Index > -1) then begin
    f := TFont.Create;
    f.Name := Items[Index];
    f.Size := Font.Size;
    Height := GetFontHeight(f.Handle);
    FreeAndNil(f);
  end;
end;

procedure TsFontListBox.Loaded;
begin
  inherited;
  GetFonts(Self);
end;

procedure TsFontComboBox.Loaded;
begin
  inherited;
  GetFonts(Self);
end;

initialization
  GetAllInstalledScreenFonts;
//  CreateThread(nil, 128, @GetAllInstalledScreenFonts, nil, 0, ThreadID);
  for iC := 0 to 3 do FBitmaps[iC]:= TBitmap.Create;
  FBitmaps[0].Handle := LoadBitmap(hinstance, 'PS');
  FBitmaps[1].Handle := LoadBitmap(hinstance, 'TTF');
  FBitmaps[2].Handle := LoadBitmap(hinstance, 'RASTER');
  FBitmaps[3].Handle := LoadBitmap(hinstance, 'UNKNOWN');

finalization
{$WARNINGS OFF}
  for iC := 0 to Length(fa) - 1 do TFontClass(fa[iC]).Free;
  SetLength(fa, 0);
  for iC := 0 to 3 do FreeAndNil(FBitmaps[iC]);

end.



