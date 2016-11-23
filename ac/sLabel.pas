unit sLabel;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, sConst, sMessages
  {$IFDEF TNTUNICODE}, TntControls, TntStdCtrls, TntGraphics, TntWindows{$ENDIF};

type
  TsShadowMode = (smNone, smCustom, smSkin1);
  TsKindType = (ktStandard, ktCustom, ktSkin);

  TsShadow = class(TPersistent)
{$IFNDEF NOTFORHELP}
  private
    FBuffered: Boolean;
    FBlurCount: Integer;
    FDistance: Integer;
    FColor: TColor;
    ParentControl : TControl;
    FMode: TsShadowMode;
    procedure SetBlurCount(const Value: Integer);
    procedure SetDistance(const Value: Integer);
    procedure SetColor(const Value: TColor);
    procedure SetMode(const Value: TsShadowMode);
  public
    sr, sg, sb : Integer;
    constructor Create(AOwner: TComponent; Control : TControl);
    destructor Destroy; override;
    property ShadowBuffered : Boolean read FBuffered write FBuffered default False;
  published
{$ENDIF} // NOTFORHELP
    property Mode : TsShadowMode read FMode write SetMode default smSkin1;
    property Color : TColor read FColor write SetColor default clBlack;
    property BlurCount : Integer read FBlurCount write SetBlurCount default 4;
    property Distance : Integer read FDistance write SetDistance default 1;
  end;

{$IFNDEF NOTFORHELP}
  TsCustomLabel = class(TCustomLabel)
  private
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FSkinSection: string;
{$IFDEF TNTUNICODE}
    function GetCaption: TWideCaption;
    function IsCaptionStored: Boolean;
    procedure SetCaption(const Value: TWideCaption);
    function GetHint: WideString;
    function IsHintStored: Boolean;
    procedure SetHint(const Value: WideString);
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    function GetLabelText: WideString; reintroduce; virtual;
{$ENDIF}
    procedure SetSkinSection(const Value: string);
  protected
    function GetCurrentFont : TFont; virtual;
    procedure WndProc (var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure DoDrawText(var Rect: TRect; Flags: Longint); override;
    property Font;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property SkinSection : string read FSkinSection write SetSkinSection;
{$IFDEF TNTUNICODE}
    property Caption: TWideCaption read GetCaption write SetCaption stored IsCaptionStored;
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
{$ELSE}
    property Caption;
{$ENDIF}
{$IFDEF D2005}
    property EllipsisPosition;
{$ENDIF}
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusControl;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent default True;
    property Layout;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TsClassSkinData = record
    CustomColor : boolean;
    CustomFont : boolean;
  end;
{$ENDIF} // NOTFORHELP

  TsLabel = class(TsCustomLabel)
{$IFNDEF NOTFORHELP}
  private
    FUseSkinColor: boolean;
  protected
    function GetCurrentFont : TFont; override;
    procedure WndProc (var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Font;
    property UseSkinColor : boolean read FUseSkinColor write FUseSkinColor default True;
{$ENDIF} // NOTFORHELP
  end;

{$IFNDEF NOTFORHELP}
  TsEditLabel = class(TsLabel)
  public
    BoundLabel : TObject;
    constructor InternalCreate(AOwner: TComponent; BoundStruct : TObject);
    destructor Destroy; override;
  end;
{$ENDIF} // NOTFORHELP

  TsWebLabel = class(TsCustomLabel)
{$IFNDEF NOTFORHELP}
  private
    FNormalFont: TFont;
    procedure SetHoverFont(const Value: TFont);
    procedure SetNormalFont(const Value: TFont);
  protected
    FHoverFont : TFont;
    FURL : string;
    FShowMode: TsWindowShowMode;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMEraseBkGnd(var Message: TWMLButtonDown); message WM_ERASEBKGND; 
    function GetCurrentFont: TFont; override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  public
    MouseAbove: boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
  published
    property Font: TFont read FNormalFont write SetNormalFont; 
    property Cursor default crHandPoint;
{$ENDIF} // NOTFORHELP
    property HoverFont: TFont read FHoverFont write SetHoverFont;
    property ShowMode : TsWindowShowMode read FShowMode write FShowMode default soDefault;
    property URL : string read FURL write FURL;
  end;

  TsKind = class(TPersistent)
{$IFNDEF NOTFORHELP}
  private
    FOwner : TControl;
    FKindType: TsKindType;
    FColor: TColor;
    constructor Create(AOwner: TControl);
    procedure SetKindType(const Value: TsKindType);
    procedure SetColor(const Value: TColor);
  public
    destructor Destroy; override;
  published
{$ENDIF} // NOTFORHELP
    property KindType : TsKindType read FKindType write SetKindType default ktSkin;
    property Color : TColor read FColor write SetColor default clWhite;
  end;

//  Used shading algorithm by Gennady Ermakovets (gen@godeltech.com)
  TsLabelFX = class(TsCustomLabel)
{$IFNDEF NOTFORHELP}
  private
    FMask : TBitmap;
    FMaskBits : Pointer;
    FMaskBitsSize : Integer;
    FShadow : TsShadow;
    FNeedInvalidate : Boolean;
    OffsTopLeft, OffsRightBottom : Integer;
    FKind: TsKind;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoDrawText(var Rect: TRect; Flags: Longint); override;
  published
    property Font;
{$ENDIF} // NOTFORHELP
    property Kind : TsKind read FKind write FKind;
    property Shadow : TsShadow read FShadow write FShadow;
  end;

(* ///////////////////////////////////////////////////////////////////////
TsStickyLabel component was based on the TGMSStickyLabel control code

GMSStickyLabel v1.1 July/5/97        by Glenn Shukster & Jacques Scoatarin

  GMS COMPUTING INC.                 Phone         (905)771-6458
  53 COLVIN CRES.                    Fax                   -6819
  THORNHILL, ONT.                    Compuserve:       72734,123
  CANADA  L4J 2N7                    InternetId:Gms@Shaw.wave.ca
                                     http://members.tor.shaw.wave.ca/~gms/

  Jacques Scoatarin                  Phone (357)2-492591
  52 Athalassis Ave, (flat 202)      InternetId:j.scoatarin@cytanet.com.cy
  Nicosia, Cyprus
/////////////////////////////////////////////////////////////////////// *)
  TAlignTo = (altLeft, altTop, altBottom,  altRight);
  TsStickyLabel = class(TsLabel)//CustomLabel) v4.54
{$IFNDEF NOTFORHELP}
  private
    FAttachTo: TWinControl;
    FAlignTo: TAlignTo;
    FGap : Integer;
    FOldWinProc: TWndMethod;
    FRealigning: Boolean;
    Procedure SetGap(Value: Integer);
    procedure SetAttachTo(Value: TWinControl);
    Procedure SetAlignTo(Value: TAlignTo);
    procedure NewWinProc(var Message: TMessage);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Adjust(MoveLabel: boolean);
  published
    property Font;
{$ENDIF} // NOTFORHELP
    property AlignTo: TAlignTo read FAlignTo write SetAlignTo default altLeft;
    property AttachTo: TWinControl read FAttachTo write SetAttachTo;
    Property Gap : Integer Read FGap write SetGap Default 2;
  end;

{$IFNDEF NOTFORHELP}
function GetParentCache(Control : TControl) : TCacheInfo;
{$ENDIF} // NOTFORHELP

implementation

uses ShellAPI, {$IFNDEF ALITE}sPageControl, {$ENDIF}sStyleSimply, //sMessages,
  sVCLUtils, sGraphUtils, acntUtils, sCommonData, math, sMaskData, sDefaults,
  sSkinManager;

var
  FontChanging : boolean;

function GetParentCache(Control : TControl) : TCacheInfo;
var
  ParentBG : TacBGInfo;
begin
  ParentBG.PleaseDraw := False;
  GetBGInfo(@ParentBG, Control.Parent);
  Result := BGInfoToCI(@ParentBG);
end;

{ TsCustomLabel }

constructor TsCustomLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Transparent := True;
end;

{$IFDEF TNTUNICODE}
procedure TsCustomLabel.DoDrawText(var Rect: TRect; Flags: Integer);
begin
  if not TntLabel_DoDrawText(Self, Rect, Flags, GetLabelText) then inherited;
end;
{$ELSE}
procedure TsCustomLabel.DoDrawText(var Rect: TRect; Flags: Integer);
var
  Text: string;
begin
  if Enabled or not Assigned(DefaultManager) or not DefaultManager.SkinData.Active then inherited else begin
    Text := GetLabelText;
    if (Flags and DT_CALCRECT <> 0) and ((Text = '') or ShowAccelChar and
      (Text[1] = '&') and (Text[2] = #0)) then Text := Text + ' ';
    if not ShowAccelChar then Flags := Flags or DT_NOPREFIX;
    Flags := DrawTextBiDiModeFlags(Flags);
    Canvas.Font := Font;
    Canvas.Font.Color := MixColors(ColorToRGB(Font.Color), GetControlColor(Parent), DefDisabledBlend);
    DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
  end;
end;
{$ENDIF}

function TsCustomLabel.GetCurrentFont: TFont;
var
  c : TColor;
  Ndx : integer;
begin
  Result := inherited Font;
  if Assigned(DefaultManager) and DefaultManager.SkinData.Active then begin
    Ndx := GetFontIndex(Self, -1, DefaultManager);
    if Ndx < 0
      then c := DefaultManager.GetGlobalFontColor
      else c := DefaultManager.gd[Ndx].FontColor[1];

    if (c <> clFuchsia) and (Result.Color <> c) then Result.Color := c;
  end;
end;

{$IFDEF TNTUNICODE}
function TsCustomLabel.GetCaption: TWideCaption;
begin
  Result := TntControl_GetText(Self);
end;

function TsCustomLabel.IsCaptionStored: Boolean;
begin
  Result := TntControl_IsCaptionStored(Self)
end;

procedure TsCustomLabel.SetCaption(const Value: TWideCaption);
begin
  TntControl_SetText(Self, Value);
end;

function TsCustomLabel.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;

function TsCustomLabel.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self)
end;

procedure TsCustomLabel.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TsCustomLabel.CMDialogChar(var Message: TCMDialogChar);
begin
  TntLabel_CMDialogChar(Self, Message, Caption);
end;

procedure TsCustomLabel.CMHintShow(var Message: TMessage);
begin
  ProcessCMHintShowMsg(Message);
  inherited;
end;

function TsCustomLabel.GetLabelText: WideString;
begin
  Result := Caption;
end;

{$ENDIF}

procedure TsCustomLabel.Paint;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  f : TFont;
  si : integer;
begin
  if not FontChanging then begin
    FontChanging := True;
    f := GetCurrentFont;
    if inherited Font <> f then inherited Font.Assign(f);
    if (SkinSection <> '') and (DefaultManager <> nil) and DefaultManager.Active then begin
      si := DefaultManager.GetSkinIndex(SkinSection);
      if (si > 0) and (Font.Color <> DefaultManager.gd[si].FontColor[1]) then begin
        Font.Color := DefaultManager.gd[si].FontColor[1]; // 5.40
      end;
    end;
    FontChanging := False;
  end;
  if (SkinSection <> '') and (DefaultManager <> nil) and DefaultManager.Active then begin
    si := DefaultManager.GetSkinIndex(SkinSection);
    if si > 0 then begin
      PaintItem(si, SkinSection, GetParentCache(Self), True, 0, Rect(0, 0, Width, Height), Point(Left, Top), Canvas.Handle);
    end;
  end;
  inherited Paint
end;

procedure TsCustomLabel.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    CM_FONTCHANGED : begin
      if not FontChanging and not (csDestroying in ComponentState) then begin
        if AutoSize then AdjustBounds;
        Invalidate;
      end;
      Exit
    end;
    CM_MOUSEENTER : if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
  end;
  inherited;
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_REMOVESKIN : if not (csDestroying in ComponentState) and (Message.LParam = LongInt(DefaultManager)) and (Font.Color <> clWindowText) then Font.Color := clWindowText;
    AC_REFRESH : if (csDesigning in ComponentState) and not (csDestroying in ComponentState) then Invalidate;
  end
  else case Message.Msg of
    CM_MOUSELEAVE : if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
  end;
end;

procedure TsCustomLabel.SetSkinSection(const Value: string);
begin
  if FSkinSection <> Value then begin
    FSkinSection := Value;
    Repaint;
  end;
end;

{ TsWebLabel }

procedure TsWebLabel.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  MouseAbove := True;
  if AutoSize then begin
    Invalidate;
    Update;
    AdjustBounds;
  end;
  RePaint;
end;

procedure TsWebLabel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  MouseAbove := False;
  if AutoSize then begin
    Invalidate;
    Update;
    AdjustBounds;
  end;
  RePaint;
end;

constructor TsWebLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShowMode := soDefault;

  FHoverFont := TFont.Create;
  FNormalFont := TFont.Create;
  Cursor := crHandPoint;
  ControlStyle := ControlStyle + [csOpaque];
  Transparent := True;
end;

destructor TsWebLabel.Destroy;
begin
  if Assigned(FHoverFont) then FreeAndNil(FHoverFont);
  if Assigned(FNormalFont) then FreeAndNil(FNormalFont);
  inherited Destroy;
end;

function TsWebLabel.GetCurrentFont: TFont;
begin
  if MouseAbove then Result := FHoverFont else Result := FNormalFont;
end;

procedure TsWebLabel.Loaded;
begin
  inherited Loaded;
  inherited Font.Assign(FNormalFont); // v4.41
end;

procedure TsWebLabel.SetHoverFont(const Value: TFont);
begin
  FHoverFont.Assign(Value);
end;

procedure TsWebLabel.SetNormalFont(const Value: TFont);
begin
  inherited Font.Assign(Value); // v4.41
  FNormalFont.Assign(Value);
  Paint;
end;

procedure TsWebLabel.WMEraseBkGnd(var Message: TWMLButtonDown);
begin
end;

procedure TsWebLabel.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
  if FURL <> '' then ShellExecute(Application.Handle, 'open', PChar(FURL), nil, nil, ord(FShowMode));
end;

{ TsEditLabel }

destructor TsEditLabel.Destroy;
begin
  TsBoundLabel(BoundLabel).FActive := False;
  inherited Destroy;
end;

constructor TsEditLabel.InternalCreate(AOwner: TComponent; BoundStruct: TObject);
begin
  inherited Create(AOwner);
  BoundLabel := BoundStruct;
end;

{ TsStickyLabel }

procedure TsStickyLabel.Adjust(MoveLabel: boolean);
var
  iNewTop, iNewLeft : Integer;
  MoveRelativeTo : TControl;
  Mover : TControl;
  Alignment : TAlignTo;
begin
  FRealigning := True;
  if FAttachTo <> nil then begin
    if MoveLabel then begin
      MoveRelativeTo := FAttachTo;
      Mover := Self;
      Alignment := FAlignTo;
    end
    else begin
      MoveRelativeTo := Self;
      Mover := FAttachTo;
      Alignment := altRight;
      case FAlignTo of
        altTop:    Alignment := altBottom;
        altRight:  Alignment := altLeft;
        altBottom: Alignment := altTop;
      end;
    end;

    Case Alignment of
       altLeft : begin
         iNewTop :=  MoveRelativeTo.Top + (MoveRelativeTo.Height - Mover.Height) div 2;
         iNewLeft := MoveRelativeTo.Left - Mover.Width - FGap;
       end;
       altRight : begin
         iNewTop :=  MoveRelativeTo.Top + (MoveRelativeTo.Height - Mover.Height) div 2;
         iNewLeft := MoveRelativeTo.Left + MoveRelativeTo.Width + FGap;
       end;
       altTop: begin
         iNewTop := MoveRelativeTo.Top - Mover.Height - FGap;
         iNewLeft := MoveRelativeTo.Left;
       end;
       else {alBottom:} begin
         iNewTop := MoveRelativeTo.Top + MoveRelativeTo.Height + FGap;
         iNewLeft := MoveRelativeTo.Left;
       end;
    end;
    { Set all propertied in one call to avoid multiple re-drawing & pos changes }
    Mover.SetBounds(iNewLeft, iNewTop, Mover.Width, Mover.Height);
  end;
  FRealigning := False;
end;

constructor TsStickyLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGap := 2;
  FRealigning := False;
end;

destructor TsStickyLabel.Destroy;
begin
  SetAttachTo(nil);
  inherited Destroy;
end;

procedure TsStickyLabel.NewWinProc(var Message: TMessage);
begin
  if not (csDestroying in ComponentState) then begin
    if Assigned(FAttachTo) and (not FRealigning) then begin
      FRealigning := True;
      try
        case(Message.Msg) of
          CM_ENABLEDCHANGED : Enabled := FAttachTo.Enabled;
          CM_VISIBLECHANGED : Visible := FAttachTo.Visible;
          WM_SIZE, WM_MOVE, WM_WINDOWPOSCHANGED : Adjust(Message.Msg <> WM_SIZE);
        end;
      finally
        FRealigning := FALSE;
      end;
    end;
  end;
  if Assigned(FOldWinProc) then FOldWinProc(Message);
end;

procedure TsStickyLabel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FAttachTo) then SetAttachTo(nil);
  inherited Notification(AComponent, Operation);
end;

procedure TsStickyLabel.SetAlignTo(Value: TAlignTo);
begin
  if (FAlignTo <> Value) then begin
    FAlignTo := Value;
    Adjust(True);
  end;
end;

procedure TsStickyLabel.SetAttachTo(Value: TWinControl);
begin
  if(Value <> FAttachTo) then begin
    if (Assigned(FAttachTo)) then FAttachTo.WindowProc := FOldWinProc;
    FAttachTo := Value;
    if (Assigned(Value)) then begin
      Adjust(True);
      Enabled := FAttachTo.Enabled;
      Visible := FAttachTo.Visible;
      FOldWinProc := FAttachTo.WindowProc;
      FAttachTo.WindowProc := NewWinProc;
    end;
  end;
end;

procedure TsStickyLabel.SetGap(Value: Integer);
begin
  if (FGap <> Value) then begin
    FGap := Value;
    Adjust(True);
  end;
end;

procedure TsStickyLabel.WndProc(var Message: TMessage);
begin
  if not (csDestroying in ComponentState) and Assigned(FAttachTo) and not FRealigning then begin
    FRealigning := True;
    try
      if Message.Msg = WM_WINDOWPOSCHANGED then Adjust(False);
   finally
      FRealigning := False;
    end;
  end;
  inherited WndProc(Message);
end;

{ TsShadow }

constructor TsShadow.Create(AOwner: TComponent; Control : TControl);
begin
  FColor := clBlack;
  FBlurCount := 4;
  FDistance := 1;
  FMode := smSkin1;
  ParentControl := Control;
end;

destructor TsShadow.Destroy;
begin
  inherited;
end;

procedure TsShadow.SetBlurCount(const Value: Integer);
begin
  if FBlurCount <> Value then begin
    FBlurCount := Value;
    ParentControl.Invalidate
  end;
end;

procedure TsShadow.SetDistance(const Value: Integer);
begin
  if FDistance <> Value then begin
    FDistance := Value;
    ParentControl.Invalidate;
  end;
end;

procedure TsShadow.SetColor(const Value: TColor);
var
  rgb : Integer;
begin
  if FColor <> Value then begin
    FColor := Value;
    rgb := ColorToRGB(Value);
    sr := rgb and 255;
    sg := (rgb shr 8) and 255;
    sb := (rgb shr 16) and 255;
    ParentControl.Invalidate;
  end;
end;

procedure TsShadow.SetMode(const Value: TsShadowMode);
begin
  if FMode <> Value then begin
    FMode := Value;
    ParentControl.Invalidate
  end;
end;

{ TsLabelFX }

constructor TsLabelFX.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShadow := TsShadow.Create(AOwner, Self);
  FKind := TsKind.Create(Self);       
  FMask := CreateBmp32(0, 0);
  FMaskBits := nil;
  FNeedInvalidate := True;
end;

destructor TsLabelFX.Destroy;
begin
  FreeAndNil(FShadow);
  FreeAndNil(FMask);
  FreeAndNil(FKind);
  if FMaskBits <> nil then FreeMem(FMaskBits);
  inherited;
end;

procedure TsLabelFX.DoDrawText(var Rect: TRect; Flags: Integer);
const
  LB_BORDER = 3;
var
  Text: acString;
  x, y :Integer;
  i : Integer;
  oRect : TRect;
  MaskOffs, pb : PByte;
  W, H : Integer;
  offs_North, offs_South, offs_West, offs_East : PByte;
  invert : byte;
  cr, cg, cb : Integer;
  ShColor, ShOffset, ShBlur : integer;
  rgb : Integer;
  procedure AddMask;
  var
    y, x : Integer;
    MaskOffs, pb : PByte;
  begin // Fill mask
    Integer(MaskOffs) := Integer(FMaskBits) + W + 1;
    for y := 0 to FMask.Height - 1 do begin
      pb := FMask.ScanLine[y];
      for x := 0 to FMask.Width - 1 do begin
        if pb^ <> 0 then MaskOffs^ := 255;
        Integer(pb) := Integer(pb) + 4;
        Integer(MaskOffs) := Integer(MaskOffs) + 1;
      end;
      Integer(MaskOffs) := Integer(MaskOffs) + 2;
    end;             
  end;
begin
  if FShadow.Mode <> smNone then begin // If not standard kind
    ShBlur := 0;
    ShColor := 0;
    ShOffset := 0;
    case FShadow.Mode of
      smCustom : begin
        ShColor := FShadow.Color;
        ShBlur := FShadow.BlurCount;
        ShOffset := FShadow.FDistance;
      end;
      smSkin1 : begin
        if Assigned(DefaultManager) and DefaultManager.SkinData.Active and (DefaultManager.SkinData.Shadow1Blur <> -1) then begin
          ShColor := DefaultManager.SkinData.Shadow1Color;
          ShBlur := DefaultManager.SkinData.Shadow1Blur;
          ShOffset := DefaultManager.SkinData.Shadow1Offset;
        end
        else begin
          ShColor := FShadow.Color;
          ShBlur := FShadow.BlurCount;
          ShOffset := FShadow.FDistance;
        end;
        rgb := ColorToRGB(ShColor);
        Shadow.sr := rgb and 255;
        Shadow.sg := (rgb shr 8) and 255;
        Shadow.sb := (rgb shr 16) and 255;
      end;
    end;
    Text := GetLabelText;
    if (Flags and DT_CALCRECT <> 0) and ((Text = '') or ShowAccelChar and (Text[1] = '&') and (Text[2] = #0)) then Text := Text + ' ';
    if not ShowAccelChar then Flags := Flags or DT_NOPREFIX;
    Flags := DrawTextBiDiModeFlags(Flags);
    Canvas.Font.Assign(Font);
    if not Enabled then begin
      OffsetRect(Rect, 1, 1);
      Canvas.Font.Color := clBtnHighlight;
{$IFDEF TNTUNICODE}
      Tnt_DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), Rect, Flags);
      OffsetRect(Rect, -1, -1);
      Canvas.Font.Color := clBtnShadow;
      Tnt_DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), Rect, Flags);
{$ELSE}
      DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
      OffsetRect(Rect, -1, -1);
      Canvas.Font.Color := clBtnShadow;
      DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
{$ENDIF}
    end
    else begin
      case FKind.KindType of
        ktStandard : Canvas.Font.Color := ColorToRGB(Color);
        ktCustom : Canvas.Font.Color := ColorToRGB(Kind.Color);
        ktSkin : begin
          if Assigned(DefaultManager) and DefaultManager.SkinData.Active then begin
            if (DefaultManager.ConstData.IndexGlobalInfo > -1) and
                 (DefaultManager.ConstData.IndexGlobalInfo <= Length(DefaultManager.gd) - 1) and
                   (DefaultManager.gd[DefaultManager.ConstData.IndexGlobalInfo].FontColor[2] <> -1)
              then Canvas.Font.Color := ColorToRGB(DefaultManager.gd[DefaultManager.ConstData.IndexGlobalInfo].FontColor[2])
              else Canvas.Font.Color := ColorToRGB(Kind.Color)
          end
          else Canvas.Font.Color := ColorToRGB(Kind.Color);
        end;
      end;

      if (Flags and DT_CALCRECT <> DT_CALCRECT) and (ShColor <> clNone) and (ShBlur <> 0) then begin
        if (FNeedInvalidate) or (not FShadow.FBuffered) then begin
          FMask.Width := WidthOf(Rect, True);
          FMask.Height := HeightOf(Rect, True);
          FMask.Canvas.Brush.Color := 0;
          FMask.Canvas.FillRect(Classes.Rect(0, 0, FMask.Width, FMask.Height));
          FMask.Canvas.Font := Canvas.Font;
          FMask.Canvas.Font.Color := clWhite;
          //draw text
(*!!!*)
          oRect := Rect;
          dec(Rect.Left, OffsTopLeft);
          dec(Rect.Top, OffsTopLeft);
          dec(Rect.Right, OffsRightBottom);
          dec(Rect.Bottom, OffsRightBottom);

          OffsetRect(Rect, ShOffset, ShOffset);
{$IFDEF TNTUNICODE}
          Tnt_DrawTextW(FMask.Canvas.Handle, PWideChar(Text), Length(Text), Rect, Flags);
{$ELSE}
          DrawText(FMask.Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
{$ENDIF}
          Rect := oRect;

          W := FMask.Width + 2;
          H := FMask.Height + 2;
          if FMaskBitsSize < W * H * 2 then begin
            FMaskBitsSize := W * H * 2;
            ReallocMem(FMaskBits, FMaskBitsSize);
          end;
          FillChar(PChar(FMaskBits)^, W * H * 2, 0);

          //Blur Mask
          for i := 1 to ShBlur do begin
            Integer(MaskOffs) := Integer(FMaskBits) + W + 1;

            AddMask;
            Integer(offs_North) := Integer(MaskOffs) - W;
            Integer(offs_South) := Integer(MaskOffs) + W;
            Integer(offs_West) := Integer(MaskOffs) - 1;
            Integer(offs_East) := Integer(MaskOffs) + 1;

            for y := 0 to H - 3 do begin
              for x := 0 to W - 3 do begin
                MaskOffs^ := (offs_North^ + offs_South^ + offs_West^ + offs_East^)shr 2;
                Integer(MaskOffs) := Integer(MaskOffs) + 1;
                Integer(offs_North) := Integer(offs_North) + 1;
                Integer(offs_South) := Integer(offs_South) + 1;
                Integer(offs_West) := Integer(offs_West) + 1;
                Integer(offs_East) := Integer(offs_East) + 1;
              end;
              Integer(MaskOffs) := Integer(MaskOffs) + 2;
              Integer(offs_North) := Integer(offs_North) + 2;
              Integer(offs_South) := Integer(offs_South) + 2;
              Integer(offs_West) := Integer(offs_West) + 2;
              Integer(offs_East) := Integer(offs_East) + 2;
            end;       
          end;

          Integer(MaskOffs) := Integer(FMaskBits) + FMask.Width + 3;

          if Transparent then begin
            // GetBackground
            BitBlt(FMask.Canvas.Handle, 0, 0, FMask.Width, FMask.Height, Canvas.Handle, 0, 0, SRCCOPY);
//            BitBlt(FMask.Canvas.Handle, Rect.Left, Rect.Top, FMask.Width, FMask.Height, Canvas.Handle, Rect.Left, Rect.Top, SRCCOPY);
            // setAlpha
            for y := 0 to FMask.Height - 1  do begin
              pb := FMask.ScanLine[y];
              for x := 0 to FMask.Width - 1 do begin
                invert := not MaskOffs^; // 255 - MaskOffs^
                pb^ := (pb^ * invert + FShadow.sb * MaskOffs^) shr 8;
                Integer(pb) := Integer(pb) + 1;
                pb^ := (pb^ * invert + FShadow.sg * MaskOffs^) shr 8;
                Integer(pb) := Integer(pb) + 1;
                pb^ := (pb^ * invert + FShadow.sr * MaskOffs^) shr 8;
                Integer(pb) := Integer(pb) + 2;
                Integer(MaskOffs) := Integer(MaskOffs) + 1;
              end;
              Integer(MaskOffs) := Integer(MaskOffs) + 2;
            end;
          end
          else begin
            // setAlpha
            i := ColorToRGB(Color);

            cr := i and 255;
            cg := (i shr 8) and 255;
            cb := (i shr 16) and 255;

            for y := 0 to FMask.Height - 1  do begin
              pb := FMask.ScanLine[y];
              for x := 0 to FMask.Width - 1 do begin
                invert := not MaskOffs^; // 255 - MaskOffs^
                pb^ := (cb * invert + FShadow.sb * MaskOffs^) shr 8;
                Integer(pb) := Integer(pb) + 1;
                pb^ := (cg * invert + FShadow.sg * MaskOffs^) shr 8;
                Integer(pb) := Integer(pb) + 1;
                pb^ := (cr * invert + FShadow.sr * MaskOffs^) shr 8;
                Integer(pb) := Integer(pb) + 2;
                Integer(MaskOffs) := Integer(MaskOffs) + 1;
              end;
              Integer(MaskOffs) := Integer(MaskOffs) + 2;
            end;
          end;//*)
          FNeedInvalidate := False;
        end; // Need Invalidate

        BitBlt(Canvas.Handle, 0{Rect.Left}, 0{Rect.Top v5.11}, FMask.Width, FMask.Height, FMask.Canvas.Handle, 0, 0, SRCCOPY);
        oRect := Rect;
        dec(Rect.Left, OffsTopLeft);
        dec(Rect.Top, OffsTopLeft);
        dec(Rect.Right, OffsRightBottom);
        dec(Rect.Bottom, OffsRightBottom);

{$IFDEF TNTUNICODE}
        Tnt_DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), Rect, Flags);
{$ELSE}
        DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
{$ENDIF}

        Rect := oRect;
      end
      else begin
{$IFDEF TNTUNICODE}
        Rect := Classes.Rect(0, 0, WideCanvasTextWidth(Canvas, Text), WideCanvasTextHeight(Canvas, Text));
        Tnt_DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), Rect, Flags);
{$ELSE}
        DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
{$ENDIF}
      end;

      if (Flags and DT_CALCRECT = DT_CALCRECT) and (ShColor <> clNone) and (ShBlur <> 0) then begin
        OffsTopLeft := Min(0, ShOffset - ShBlur);
        OffsRightBottom := Max(0, ShOffset + ShBlur);
        inc(Rect.Right, OffsRightBottom - OffsTopLeft);
        inc(Rect.Bottom, OffsRightBottom - OffsTopLeft);
      end;
    end;
  end
  else inherited;
end;

{ TsKind }

constructor TsKind.Create(AOwner: TControl);
begin
  FKindType := ktSkin;
  FColor := clWhite;
  FOwner := AOwner
end;

destructor TsKind.Destroy;
begin
  inherited;
end;

procedure TsKind.SetColor(const Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    FOwner.Invalidate
  end
end;

procedure TsKind.SetKindType(const Value: TsKindType);
begin
  if FKindType <> Value then begin
    FKindType := Value;
    FOwner.Invalidate
  end
end;

{ TsLabel }

constructor TsLabel.Create(AOwner: TComponent);
begin
  inherited;
  FUseSkinColor := True;
end;

function TsLabel.GetCurrentFont: TFont;
var
  c : TColor;
  Ndx : integer;
begin
  Result := inherited Font;
  if not (csDestroying in ComponentState) and Assigned(DefaultManager) and DefaultManager.SkinData.Active and UseSkinColor {$IFNDEF SKININDESIGN}and not ((csDesigning in ComponentState) and (GetOwnerFrame(Self) <> nil)){$ENDIF} then begin
    Ndx := GetFontIndex(Self, -1, DefaultManager);
    if Ndx < 0
      then c := DefaultManager.GetGlobalFontColor
      else c := DefaultManager.gd[Ndx].FontColor[1];
    if (c <> clFuchsia) and (Result.Color <> c) then Result.Color := c;
  end;
end;

procedure TsLabel.WndProc(var Message: TMessage);
begin
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_REMOVESKIN : begin
      if not (csDestroying in ComponentState) and (Message.LParam = LongInt(DefaultManager)) and UseSkinColor {$IFNDEF SKININDESIGN}and not (csDestroying in ComponentState) and not ((csDesigning in ComponentState) and (GetOwnerFrame(Self) <> nil)){$ENDIF} and (Font.Color <> clWindowText)
        then Font.Color := clWindowText;
      Exit;
    end;
  end;           
  inherited;
end;

end.
