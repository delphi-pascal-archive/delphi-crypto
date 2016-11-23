unit acAlphaHints;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, sGraphUtils, sConst, acntUtils, sHtmlParse, acPNG;

const
  DefAnimationTime = 100;

type
  TacMousePosition = (mpLeftTop, mpLeftBottom, mpRightTop, mpRightBottom);
  TacBorderDrawMode = (dmRepeat, dmStretch);
  TsAlphaHints = class;
  TacHintTemplate = class;
  TacHintTemplates = class;
  TacCustomHintWindow = class;
  TacHintImage = class;

  TacShowHintEvent = procedure (var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo; var Frame : TFrame) of object;

  TacBorderDrawModes = class(TPersistent)
  private
    procedure SetDrawMode(const Index: Integer; const Value: TacBorderDrawMode);
  protected
    FBottom: TacBorderDrawMode;
    FLeft: TacBorderDrawMode;
    FTop: TacBorderDrawMode;
    FRight: TacBorderDrawMode;
    FCenter: TacBorderDrawMode;
    FOwner : TacHintImage;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TacHintImage);
  published
    property Top : TacBorderDrawMode index 0 read FTop write SetDrawMode default dmStretch;
    property Left : TacBorderDrawMode index 1 read FLeft write SetDrawMode default dmStretch;
    property Bottom : TacBorderDrawMode index 2 read FBottom write SetDrawMode default dmStretch;
    property Right : TacBorderDrawMode index 3 read FRight write SetDrawMode default dmStretch;
    property Center : TacBorderDrawMode index 4 read FCenter write SetDrawMode default dmStretch;
  end;

  TacBordersSizes = class(TPersistent)
  private
    function GetInteger(const Index: Integer): integer;
  protected
    FTop : integer;
    FLeft : integer;
    FBottom : integer;
    FRight : integer;
    FOwner : TacHintImage;
    procedure SetInteger(Index : integer; Value: integer);
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TacHintImage);
  published
    property Top : integer index 0 read GetInteger write SetInteger default 0;
    property Left : integer index 1 read GetInteger write SetInteger default 0;
    property Bottom : integer index 2 read GetInteger write SetInteger default 0;
    property Right : integer index 3 read GetInteger write SetInteger default 0;
  end;

  TacHintImage = class(TPersistent)
  private
    FOwner : TacHintTemplate;
    FBordersWidths: TacBordersSizes;
    FClientMargins: TacBordersSizes;
    FBorderDrawModes: TacBorderDrawModes;
    function GetImgHeight: integer;
    function GetImgWidth: integer;
    procedure SetBordersWidths(const Value: TacBordersSizes);
    procedure SetImage(const Value: TPngGraphic);
    procedure SetClientMargins(const Value: TacBordersSizes);
    procedure SetImgHeight(const Value: integer);
    procedure SetImgWidth(const Value: integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    FImage: TPngGraphic;
    constructor Create(AOwner: TacHintTemplate);
    destructor Destroy; override;
    procedure ImageChanged;
  published
    property ImageHeight : integer read GetImgHeight write SetImgHeight;
    property ImageWidth : integer read GetImgWidth write SetImgWidth;
    property Image : TPngGraphic read FImage write SetImage;
    property ClientMargins : TacBordersSizes read FClientMargins write SetClientMargins;
    property BorderDrawModes : TacBorderDrawModes read FBorderDrawModes write FBorderDrawModes;
    property BordersWidths : TacBordersSizes read FBordersWidths write SetBordersWidths;
  end;

  TacHintTemplate = class(TCollectionItem)
  private
    FImageDefault : TacHintImage;
    FImageLeftBottom : TacHintImage;
    FImageRightBottom : TacHintImage;
    FImageRightTop : TacHintImage;
    FName: acString;
    FFont: TFont;
    procedure SetHintImage(const Index: Integer; const Value: TacHintImage);
    procedure SetFont(const Value: TFont);
  protected
    FOwner : TacHintTemplates;
    procedure AssignTo(Dest: TPersistent); override;
  public
    destructor Destroy; override;
    constructor Create(Collection: TCollection); override;
  published
    property ImageDefault : TacHintImage index 0 read FImageDefault write SetHintImage;
    property Img_LeftBottom : TacHintImage index 1 read FImageLeftBottom write SetHintImage;
    property Img_RightBottom : TacHintImage index 2 read FImageRightBottom write SetHintImage;
    property Img_RightTop : TacHintImage index 3 read FImageRightTop write SetHintImage;
    property Font : TFont read FFont write SetFont;
    property Name : acString read FName write FName;
  end;

  TacHintTemplates = class(TCollection)
  protected
    FOwner: TsAlphaHints;
    function GetItem(Index: Integer): TacHintTemplate;
    procedure SetItem(Index: Integer; Value: TacHintTemplate);
    function GetOwner: TPersistent; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner : TsAlphaHints);
    destructor Destroy; override;
    property Items[Index: Integer]: TacHintTemplate read GetItem write SetItem; default;
  end;

  TsAlphaHints = class(TComponent)
  private
    FPauseHide: integer;
    FHTMLMode : boolean;
    FOnShowHint: TacShowHintEvent;
    FDefaultMousePos: TacMousePosition;
    FAnimated: boolean;
    FHintPos: TPoint;
{$IFNDEF ACHINTS}
    FSkinSection: TsSkinSection;
    FUseSkinData: boolean;
    FActive: boolean;
    FTemplates: TacHintTemplates;
    FTemplateIndex: integer;
    FTemplateName: TacStrValue;
    FOnChange: TNotifyEvent;
{$ENDIF}
    function GetAnimated: boolean;
{$IFNDEF ACHINTS}
    procedure SetSkinData(const Value: boolean);
    procedure SetPauseHide(const Value: integer);
    procedure SetActive(const Value: boolean);
{$ENDIF}
    procedure UpdateHWClass;
    procedure SetTemplates(const Value: TacHintTemplates);
    procedure SetTemplateName(const Value: TacStrValue);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    FTempHint: TacCustomHintWindow;
    FCacheBmp : TBitmap;
    procedure OnShowHintApp(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Changed;
    procedure AfterConstruction; override;
    procedure ShowHint(TheControl: TControl; HintText: String);
    procedure HideHint;
    function Skinned : boolean;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property DefaultMousePos : TacMousePosition read FDefaultMousePos write FDefaultMousePos default mpLeftTop;
  published
    property Active : boolean read FActive write SetActive default True;
    property OnShowHint: TacShowHintEvent read FOnShowHint write FOnShowHint;
    property Animated : boolean read GetAnimated write FAnimated default True;
    property Templates : TacHintTemplates read FTemplates write SetTemplates;
    property TemplateName : TacStrValue read FTemplateName write SetTemplateName;
    property HTMLMode : boolean read FHTMLMode write FHTMLMode default False;
    property PauseHide : integer read FPauseHide write SetPauseHide default 5000;
{$IFNDEF ACHINTS}
    property SkinSection : TsSkinSection read FSkinSection write FSkinSection;
    property UseSkinData : boolean read FUseSkinData write SetSkinData default False;
{$ENDIF}
  end;

  TacCustomHintWindow = class(THintWindow)
  private
    FHintLocation: TPoint;
    procedure WMEraseBkGND (var Message: TWMPaint); message WM_ERASEBKGND;
    procedure WMNCPaint (var Message: TWMPaint); message WM_NCPaint;
    procedure PrepareMask;
  protected
    SkinIndex, BorderIndex, BGIndex : integer;
    rgn : hrgn;
    FMousePos : TacMousePosition;
    procedure CreateParams(var Params: TCreateParams); override;
    function GetMousePosition : TPoint; virtual;
    function MainRect: TRect; dynamic;
    procedure WndProc(var Message: TMessage); override;
    function SkinMargin(Border : byte): integer;
  public
    BodyBmp: TBitmap;
    MaskBmp : TBitmap; // for debug
    AlphaBmp : TBitmap;
    function GetMask : TBitmap; dynamic;
    function GetBody : TBitmap; dynamic;
    property HintLocation: TPoint read FHintLocation write FHintLocation;
    procedure CreateAlphaBmp(const Width, Height : integer); virtual;
    procedure TextOut(Bmp: TBitmap); dynamic;
    function GetArrowPos(var Rect : TRect; const mPos : TPoint) : TacMousePosition;
    function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect; override;
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
  end;

  TacPngHintWindow = class(TacCustomHintWindow)
  public
    constructor Create(AOwner:TComponent); override;
    procedure CreateAlphaBmp(const Width, Height : integer); override;
    function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect; override;
  end;

var
  Manager : TsAlphaHints;

procedure CopyChannel32(const DstBmp, SrcBmp : TBitmap; const Channel : integer);

{$IFDEF ACHINTS}
procedure Register;
{$ENDIF}

implementation

uses {$IFNDEF ACHINTS}sVclUtils, sMessages, sSkinProps, sSkinManager, {$ENDIF}
  sGradient, math, sStyleSimply, sAlphaGraph{$IFDEF LOGGED}, sDebugMsgs{$ENDIF};

{$R acHints.res}

const
  NCS_DROPSHADOW = $20000;
  DelayValue = 8;
  SkinBorderWidth = 4;

var
  FBlend: TBlendFunction;
  HintFrame : TFrame = nil;
  DefaultTemplate : TacHintTemplate = nil;
  Template : TacHintTemplate = nil;
  Image : TacHintImage = nil;
  acLocalUpd : boolean = False; 

{$IFDEF ACHINTS}
procedure Register;
begin
  RegisterComponents('AlphaExtra', [TsAlphaHints]);
end;
{$ENDIF}

{ TsAlphaHints }

constructor TsAlphaHints.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTemplateIndex := -1;

  FCacheBmp := CreateBmp24(0, 0);

  FHTMLMode := False;
  FPauseHide := 5000;
  FDefaultMousePos := mpLeftTop;
  FTemplates := TacHintTemplates.Create(Self);

  FAnimated := True;
  FActive := True;
end;

destructor TsAlphaHints.Destroy;
begin
  Application.OnShowHint := nil;
  HintWindowClass := THintWindow;
  UpdateHWClass;
  FreeAndNil(FTemplates);
  FreeAndNil(FCacheBmp);
  inherited;
end;

procedure TsAlphaHints.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) and (Tag <> ExceptTag) then begin
    Application.HintHidePause := FPauseHide;
    FHintPos := Point(-1, -1);
    Application.HintPause := 500;
    Application.HintShortPause := 200;
    Application.OnShowHint := OnShowHintApp;
  end;
{$IFNDEF ACHINTS}
  if FSkinSection = '' then FSkinSection := s_Hint;
{$ENDIF}
  UpdateHWClass;
end;

procedure TsAlphaHints.OnShowHintApp(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
begin
  FHintPos := HintInfo.HintPos;
  if Assigned(FOnShowHint) then FOnShowHint(HintStr, CanShow, HintInfo, HintFrame) else inherited;
  if (FHintPos.x <> HintInfo.HintPos.x) and (FHintPos.y <> HintInfo.HintPos.y) then FHintPos := HintInfo.HintPos;
end;

procedure TsAlphaHints.ShowHint(TheControl: TControl; HintText: String); // Added by Matthew Bieschke
Var
  HL: TPoint; // Hint location
  HintRect: TRect;
begin
  FTempHint := nil;
  // Is control valid ?
  If Assigned(TheControl) Then Begin
    HL.X := 0;
    HL.Y := 0;
  End;
  // Does hint need to be created ?
  If not Assigned(FTempHint) Then begin
    FTempHint := TacPngHintWindow.Create(Self);
  end;
  // Was hint creation successful ?
  If Assigned(FTempHint) Then With FTempHint Do Begin
    HintRect := CalcHintRect(120 {MaxWidth}, HintText, NIL);
    HintLocation := TheControl.ClientToScreen(HL);
    HintLocation := Point(HintLocation.x + TheControl.Width div 2, HintLocation.Y + TheControl.Height div 3);
    ActivateHint(HintRect, HintText);
  End;
end;

procedure TsAlphaHints.HideHint;
begin
  FreeAndNil(FTempHint);
end;

procedure TsAlphaHints.AfterConstruction;
begin
  inherited;
{$IFNDEF ACHINTS}
  if FSkinSection = '' then FSkinSection := s_Hint;
{$ENDIF}
end;

function TsAlphaHints.GetAnimated: boolean;
begin
  Result := FAnimated;
end;

function TsAlphaHints.Skinned: boolean;
begin
{$IFNDEF ACHINTS}
  Result := Assigned(Manager) and Assigned(DefaultManager) and UseSkinData;
  if Result then Result := (Manager.SkinSection <> '') and (DefaultManager.GetSkinIndex(Manager.SkinSection) > -1);
{$ELSE}
  Result := False
{$ENDIF}
end;

{$IFNDEF ACHINTS}
procedure TsAlphaHints.SetSkinData(const Value: boolean);
begin
  FUseSkinData := Value;
end;
{$ENDIF}

procedure TsAlphaHints.SetPauseHide(const Value: integer);
begin
  if FPauseHide <> Value then begin
    FPauseHide := Value;
    if not (csDesigning in ComponentState) and (Tag <> ExceptTag) then begin
      Application.HintHidePause := FPauseHide;
    end;
  end;
end;

procedure TsAlphaHints.SetActive(const Value: boolean);
begin
  FActive := Value;
  UpdateHWClass;
end;

procedure TsAlphaHints.UpdateHWClass;
begin
  if not (csDesigning in ComponentState) and (Tag <> - 98) {Edit mode for component in editor} then begin
    if FActive and acLayered and not (csDestroying in ComponentState) then begin
      Manager := Self;
      HintWindowClass := TacPngHintWindow
    end
    else begin
      Manager := nil;
      HintWindowClass := THintWindow;
    end;
  end;
end;

procedure TsAlphaHints.SetTemplates(const Value: TacHintTemplates);
begin
  FTemplates.Assign(Value);
end;

procedure TsAlphaHints.SetTemplateName(const Value: TacStrValue);
var
  i : integer;
begin
  FTemplateIndex := -1;
  FTemplateName := Value;
  for i := 0 to Templates.Count - 1 do begin
    if UpperCase(Templates[i].Name) = UpperCase(Value) then begin
      FTemplateIndex := i;
      Break;
    end;
  end;
end;

procedure TsAlphaHints.AssignTo(Dest: TPersistent);
var
  DstCmp : TsAlphaHints;
begin
  if Dest <> nil then begin
    DstCmp := TsAlphaHints(Dest);
    DstCmp.Templates.Clear;
    Templates.AssignTo(DstCmp.Templates);
    DstCmp.TemplateName := TemplateName;
    DstCmp.Animated := Animated;
    DstCmp.DefaultMousePos := DefaultMousePos;
    DstCmp.HTMLMode := HTMLMode;
    DstCmp.PauseHide := PauseHide;
    DstCmp.SkinSection := SkinSection; 
    DstCmp.UseSkinData := UseSkinData;
  end
  else inherited;
end;

procedure TsAlphaHints.Changed;
begin
  if not (csLoading in ComponentState) and Assigned(FOnChange) then FOnChange(Self);
end;

{ TacCustomHintWindow }

procedure TacCustomHintWindow.ActivateHint(Rect: TRect; const AHint: string);
var
{$IFNDEF ACHINTS}
  DC: HDC;
  w, h, i : integer;
  p : TPoint;

  FBmpSize: TSize;
  FBmpTopLeft: TPoint;
  StepB, Blend : real;
  StepCount : integer;
{$ENDIF}
begin
{$IFNDEF ACHINTS}
  if not Assigned(Manager) then exit;

  if Manager.Skinned and acLayered
    then SetClassLong(Handle, GCL_STYLE, GetClassLong(Handle, GCL_STYLE) or NCS_DROPSHADOW)
    else SetClassLong(Handle, GCL_STYLE, GetClassLong(Handle, GCL_STYLE) and not NCS_DROPSHADOW);

  Caption := AHint;
  if (FHintLocation.X = 0) or (FHintLocation.Y = 0) then p := GetMousePosition else p := FHintLocation;
  w := WidthOf(Rect);
  h := HeightOf(Rect);
  OffsetRect(Rect, p.x - Rect.Left, p.y - Rect.Top);
  UpdateBoundsRect(Rect);

  FMousePos := GetArrowPos(Rect, p);
  if not Manager.Skinned and (FMousePos in [mpLeftBottom, mpRightBottom]) then dec(Rect.Top, 16);

  Rect.Right := Rect.Left + w;
  Rect.Bottom := Rect.Top + h;

  Manager.FCacheBmp.Width := w;
  Manager.FCacheBmp.Height := h;

  if acLayered then begin
    SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, w, h, SWP_NOACTIVATE);
    FBmpSize.cx := w;
    FBmpSize.cy := h;
    FBmpTopLeft := Point(0, 0);
    CreateAlphaBmp(w, h);
    DC := GetDC(0);
    SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
    UpdateLayeredWindow(Handle, DC, nil, @FBmpSize, AlphaBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA);
    // Show window with hint
    if Manager.Animated and IsNTFamily then begin
      if Manager.Skinned then i := DefaultManager.gd[SkinIndex].Transparency else
      i := 0;//Manager.HintKind.Transparency;
      i := Max(0, Min(100, i));
      if not (csDestroying in ComponentState) then begin
        StepCount := max(DefAnimationTime div DelayValue, 1);
        StepB := Round((100 - i) * 2.55) / StepCount;
        Blend := 0;
        FBlend.SourceConstantAlpha := 0;
        UpdateLayeredWindow(Handle, DC, nil, @FBmpSize, AlphaBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA);
        ShowWindow(Handle, SW_SHOWNOACTIVATE);
        RedrawWindow(Handle, nil, 0, RDW_NOERASE or RDW_UPDATENOW or RDW_ALLCHILDREN);
        for i := 0 to StepCount - 1 do begin
          Blend := Blend + StepB;
          FBlend.SourceConstantAlpha := Round(Blend);
          if not (csDestroying in ComponentState)
            then UpdateLayeredWindow(Handle, DC, nil, @FBmpSize, AlphaBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA)
            else break;
          Sleep(DelayValue);
        end;
      end;
    end
    else begin
      if IsNTFamily then begin
        FBlend.SourceConstantAlpha := Round((100{ - Manager.HintKind.Transparency}) * 2.55);
        UpdateLayeredWindow(Handle, DC, nil, @FBmpSize, AlphaBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA);
        ShowWindow(Handle, SW_SHOWNOACTIVATE);
      end
      else SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, w, h, SWP_SHOWWINDOW or SWP_NOACTIVATE);
    end;
    ReleaseDC(0, DC);
    if AlphaBmp <> nil then FreeAndNil(AlphaBmp);
  end;
  Manager.FHintPos.x := -1;
{$ELSE}
  inherited;
{$ENDIF}
end;

constructor TacCustomHintWindow.Create(AOwner: TComponent);
begin
  inherited;
//  dx := 0;
//  dy := 0;
  FHintLocation.X := 0;
  FHintLocation.Y := 0;
  BorderWidth := 0;
  SkinIndex := -1;
  BorderIndex := -1;
  BGIndex := -1;
  with FBlend do begin
    BlendOp := AC_SRC_OVER;
    BlendFlags := 0;
    AlphaFormat := AC_SRC_ALPHA;
  end;
end;

procedure TacCustomHintWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style and not WS_BORDER or WS_EX_TRANSPARENT;
end;

function TacCustomHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect;
var
  sHTML : TsHtml;
begin
  if Assigned(Manager) then begin
{$IFNDEF ACHINTS}
    if Manager.Skinned and Assigned(DefaultManager) and DefaultManager.Active then begin
      SkinIndex := DefaultManager.GetSkinIndex(Manager.SkinSection);
      if SkinIndex > -1 then begin
        BorderIndex := DefaultManager.GetMaskIndex(Manager.SkinSection, s_BordersMask);
        BGIndex := DefaultManager.GetTextureIndex(SkinIndex, Manager.SkinSection, s_Pattern);
      end
      else begin
        SkinIndex := DefaultManager.GetSkinIndex(s_Edit);
        BorderIndex := DefaultManager.GetMaskIndex(s_Edit, s_BordersMask);
        BGIndex := DefaultManager.GetTextureIndex(SkinIndex, s_Edit, s_Pattern);
      end;
    end;
{$ENDIF}
    if HintFrame <> nil then begin
      Result := Rect(0, 0, HintFrame.Width, HintFrame.Height);
    end
    else begin
      Result := Rect(0, 0, 120{Manager.HintKind.FMaxWidth}, 0);
  {$IFNDEF ACHINTS}
      if Manager.Skinned then Manager.FCacheBmp.Canvas.Font.Assign(Screen.HintFont);
  {$ENDIF}
      if Manager.FHTMLMode then begin
        sHTML := TsHtml.Create;
        sHTML.Init(Manager.FCacheBmp, aHint, Result);
        Result := sHTML.HtmlText;
        FreeAndNil(sHTML);
      end
      else DrawText(Manager.FCacheBmp.Canvas.Handle, PChar(AHint), -1, Result,
        DT_CALCRECT or DT_CENTER or DT_VCENTER or DT_WORDBREAK or DT_NOPREFIX or DrawTextBiDiModeFlagsReadingOnly);
    end;
    if Manager.Skinned then begin
      Inc(Result.Right, SkinMargin(0) + SkinMargin(2) + SkinBorderWidth * 2);
      Inc(Result.Bottom, SkinMargin(1) + SkinMargin(3) + SkinBorderWidth * 2);
    end;
  end;
end;

procedure TacCustomHintWindow.WMEraseBkGND(var Message: TWMPaint);
begin
  Message.Result := 1;
end;

procedure TacCustomHintWindow.WMNCPaint(var Message: TWMPaint);
begin
  if Assigned(Manager) then PrepareMask;
  Message.Result := 1;
end;

procedure TacCustomHintWindow.TextOut(Bmp: TBitmap);
var
  R : TRect;
  SaveIndex : hdc;
  sHTML : TsHtml;
  TempBmp : TBitmap;
{$IFNDEF ACHINTS}
  Flags: Integer;
{$ENDIF}
begin
  R := MainRect;
  if HintFrame <> nil then begin
    HintFrame.Visible := False;
    HintFrame.Left := R.Left;
    HintFrame.Top := R.Top;
    HintFrame.Parent := Self;
    TempBmp := CreateBmp24(HintFrame.Width, HintFrame.Height);

    if (DefaultManager <> nil) and DefaultManager.Active then begin
      HintFrame.Visible := True;
      SaveIndex := SaveDC(Bmp.Canvas.Handle);
      TempBmp.Canvas.Lock;
      SkinPaintTo(TempBmp, HintFrame);
      TempBmp.Canvas.UnLock;
      RestoreDC(TempBmp.Canvas.Handle, SaveIndex);
    end;

    BitBlt(Bmp.Canvas.Handle, R.Left, R.Top, HintFrame.Width, HintFrame.Height, TempBmp.Canvas.Handle, 0, 0, SRCCOPY);
    FreeAndNil(TempBmp);
    if Assigned(HintFrame) then FreeAndNil(HintFrame);
  end
  else begin
    if Manager.Skinned then begin
      R.Left := R.Left + SkinMargin(0) + SkinBorderWidth;
      R.Top := R.Top + SkinMargin(1) + SkinBorderWidth;
      R.Right := R.Right - SkinMargin(2) - SkinBorderWidth;
      R.Bottom := R.Bottom - SkinMargin(3) - SkinBorderWidth;
    end
    else begin
      inc(R.Left, Image.ClientMargins.Left);
      inc(R.Top, Image.ClientMargins.Top);
      dec(R.Right, Image.ClientMargins.Right);
      dec(R.Bottom, Image.ClientMargins.Bottom);
    end;
    Bmp.Canvas.Brush.Style := bsClear;
    Bmp.Canvas.Pen.Style := psSolid;

  {$IFNDEF ACHINTS}
    if Manager.Skinned then Bmp.Canvas.Font.Assign(Screen.HintFont);
  {$ENDIF}
    if Manager.FHTMLMode then begin
  {$IFNDEF ACHINTS}
      if Manager.Skinned then Bmp.Canvas.Font.Color := DefaultManager.gd[SkinIndex].Fontcolor[1];
  {$ENDIF}
      sHTML := TsHtml.Create;
      sHTML.Init(Bmp, Caption, R);
      sHTML.HtmlText;
      FreeAndNil(sHTML);
    end
    else begin
  {$IFNDEF ACHINTS}
      if Manager.Skinned then begin
        Flags := DT_EXPANDTABS or DT_WORDBREAK or DT_CENTER;
        WriteTextEx(BMP.Canvas, PChar(Text), True, R, Flags, SkinIndex, False, DefaultManager);
      end else
  {$ENDIF}
        DrawText(Bmp.Canvas.Handle, PChar(Caption), -1, R, DT_CENTER or DT_NOPREFIX or DT_WORDBREAK or DrawTextBiDiModeFlagsReadingOnly);
    end;
  end;
end;

function TacCustomHintWindow.MainRect: TRect;
begin
  Result := Rect(0, 0, Width, Height);
end;

procedure TacCustomHintWindow.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_GETBG : begin
      PacBGInfo(Message.LParam)^.Bmp := BodyBmp;
      PacBGInfo(Message.LParam)^.Offset := Point(0, 0);
      PacBGInfo(Message.LParam)^.BgType := btCache;
      Exit;
    end;
    AC_CHILDCHANGED : Message.LParam := 1;
  end;
  inherited;
end;

destructor TacCustomHintWindow.Destroy;
begin
  if Assigned(AlphaBmp) then FreeAndNil(AlphaBmp);
  if Assigned(MaskBmp) then FreeAndNil(MaskBmp);
  if Assigned(BodyBmp) then FreeAndNil(BodyBmp);
  inherited;
end;

function TacCustomHintWindow.GetMousePosition: TPoint;
begin
  if (Manager.FHintPos.x = -1)// and (Manager.FHintPos.y = -1)
    then Result := acMousePos
    else Result := Manager.FHintPos;
end;

procedure TacCustomHintWindow.PrepareMask;
begin
  rgn := 0;
  FreeAndNil(MaskBmp);  
  MaskBmp := GetMask;
{$IFNDEF ACHINTS}
  if Assigned(MaskBmp) and Manager.Skinned then begin // Defining window region by MaskBmp
    GetRgnFromBmp(rgn, MaskBmp, clwhite);
    SetWindowRgn(Handle, rgn, False);
  end
  else SetWindowRgn(Handle, 0, False);
{$ENDIF}
end;

function TacCustomHintWindow.GetMask: TBitmap;
{$IFNDEF ACHINTS}
var
  x, y, h, w : integer;
  White, Black : TsColor;
  CI : TCacheInfo;
{$ENDIF}
begin
{$IFNDEF ACHINTS}
  CI.FillColor := ColorToRGB(clFuchsia) - 1;
  CI.Ready := False;
  White.C := clWhite;
  Black.I := 0;
  Result := CreateBmpLike(Manager.FCacheBmp);
  PaintItemFast(SkinIndex, BorderIndex, BGIndex, BGIndex, '', CI, True, 0, Rect(0, 0, Width, Height), Point(0, 0), Result, DefaultManager);

  if Fast24Src.Attach(Result) then begin
    h := Result.Height - 1;
    w := Result.Width - 1;
    for y := 0 to h do
      for x := 0 to w do
        if Fast24Src.Pixels[x, y].I = CI.FillColor
          then Fast24Src.Pixels[x, y] := White
          else Fast24Src.Pixels[x, y] := Black;
  end;
{$ENDIF}
end;

function TacCustomHintWindow.GetBody: TBitmap;
{$IFNDEF ACHINTS}
var
  CI : TCacheInfo;
  R : TRect;
{$ENDIF}
begin
{$IFNDEF ACHINTS}
  CI.Ready := False;
  Result := CreateBmpLike(Manager.FCacheBmp);
  if HintFrame <> nil then BodyBmp := Result;
  R := ClientRect;
  PaintItemFast(SkinIndex, BorderIndex, BGIndex, BGIndex, '', CI, True, 0, Rect(0, 0, Width, Height), Point(0, 0), Result, DefaultManager);
  TextOut(Result);
{$ENDIF}
end;

function TacCustomHintWindow.SkinMargin(Border: byte): integer;
begin
{$IFNDEF ACHINTS}
  if BorderIndex > -1 then begin
    if DefaultManager.ma[BorderIndex].BorderWidth > 0 then begin
      Result := DefaultManager.ma[BorderIndex].BorderWidth;
    end
    else case Border of
      0 : begin
        if DefaultManager.ma[BorderIndex].WL > 0
          then Result := DefaultManager.ma[BorderIndex].WL
          else Result := WidthOf(DefaultManager.ma[BorderIndex].R) div (DefaultManager.ma[BorderIndex].ImageCount * 3)
      end;
      1 : begin
        if DefaultManager.ma[BorderIndex].WT > 0
          then Result := DefaultManager.ma[BorderIndex].WT
          else Result := HeightOf(DefaultManager.ma[BorderIndex].R) div ((DefaultManager.ma[BorderIndex].MaskType + 1) * 3)
      end;
      2 : begin
        if DefaultManager.ma[BorderIndex].WR > 0
          then Result := DefaultManager.ma[BorderIndex].WR
          else Result := WidthOf(DefaultManager.ma[BorderIndex].R) div (DefaultManager.ma[BorderIndex].ImageCount * 3)
      end
      else {3} begin
        if DefaultManager.ma[BorderIndex].WB > 0
          then Result := DefaultManager.ma[BorderIndex].WB
          else Result := HeightOf(DefaultManager.ma[BorderIndex].R) div ((DefaultManager.ma[BorderIndex].MaskType + 1) * 3)
      end;
    end
  end else
{$ENDIF}
  Result := 0;
end;

procedure TacCustomHintWindow.CreateAlphaBmp;
var
  x, y : integer;
  FastDst : TacFast32;
  FastShadow : TacFast24;
  FastMask : TacFast24;
  FastBody : TacFast24;
  c : TsColor;
begin
  FBlend.SourceConstantAlpha := Round(255{ - 2.55 * Manager.HintKind.Transparency});
  FreeAndNil(AlphaBmp);
  AlphaBmp := CreateBmp32(Width, Height);

  FastDst := TacFast32.Create;
  FastShadow := TacFast24.Create;
  FastMask := TacFast24.Create;
  FastBody := TacFast24.Create;

  PrepareMask;
  FreeAndNil(BodyBmp);
  BodyBmp := GetBody;
  FillDC(Manager.FCacheBmp.Canvas.Handle, Classes.Rect(0, 0, Width, Height), clWhite);

  AlphaBmp.PixelFormat := pf32bit;
  if FastDst.Attach(AlphaBmp) and FastShadow.Attach(Manager.FCacheBmp) and FastMask.Attach(MaskBmp) and FastBody.Attach(BodyBmp) then begin
    for y := 0 to Height - 1 do for x := 0 to Width - 1 do begin
      if FastMask.Pixels[x, y].R = 255 then begin
        c.I := 0;
        c.A := 255 - FastShadow.Pixels[x, y].R;
      end
      else begin
        c := FastBody.Pixels[x, y];
        c.A := 255
      end;
      FastDst[x, y] := c;
    end;
  end;
//  AlphaBmp.SaveToFile('c:\AlphaBmp.bmp');
  FreeAndnil(FastDst);
  FreeAndnil(FastMask);
  FreeAndnil(FastShadow);
  FreeAndnil(FastBody);
  if Assigned(BodyBmp) then FreeAndNil(BodyBmp);
end;

function TacCustomHintWindow.GetArrowPos(var Rect : TRect; const mPos : TPoint): TacMousePosition;
const
  pArray : array[boolean, boolean] of TacMousePosition = ((mpRightBottom, mpLeftBottom), (mpRightTop, mpLeftTop));
var
  t, l, Auto : boolean;
  h, w : integer;
begin
  FMousePos := Manager.FDefaultMousePos;
  w := WidthOf(Rect);
  h := HeightOf(Rect);
  t := not (FMousePos in [mpLeftBottom, mpRightBottom]);
  l := not (FMousePos in [mpRightTop, mpRightBottom]);
  if FMousePos in [mpLeftBottom, mpRightBottom] then OffsetRect(Rect, 0, - h);
  if FMousePos in [mpRightTop, mpRightBottom] then OffsetRect(Rect, - w, 0);
  Auto := False; // Calc arrow position
  if Rect.Bottom > Screen.Height then begin Rect.Top := mPos.y - h; t := False; Auto := True end;
  if Rect.Top < 0 then begin Rect.Top := mPos.y; t := True; Auto := True end;
  if Rect.Right > Screen.Width then begin Rect.Left := mPos.x - w; l := False; Auto := True end;
  if Rect.Left < 0 then begin Rect.Left := mPos.x; l := True; Auto := True end;
  if Auto then Result := pArray[t, l];
end;

{ TacPngHintWindow }

function TacPngHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect;
var
  DefRect : TRect;
  p : TPoint;
  procedure UpdateHintRect;
  begin
    if not Manager.Skinned then begin
      Inc(Result.Right, Image.ClientMargins.Left + Image.ClientMargins.Right);
      Inc(Result.Bottom, Image.ClientMargins.Top + Image.ClientMargins.Bottom);
      if WidthOf(Result) < Image.ImageWidth then Result.Right := Result.Left + Image.ImageWidth;
      if HeightOf(Result) < Image.ImageHeight then Result.Bottom := Result.Top + Image.ImageHeight;
    end;
  end;
begin
  if not Manager.Skinned then begin
    if (Manager.FTemplateIndex > -1) and (Manager.FTemplateIndex < Manager.Templates.Count)
      then Template := Manager.Templates[Manager.FTemplateIndex]
      else Template := DefaultTemplate;
    Manager.FCacheBmp.Canvas.Font.Assign(Template.FFont);
  end;
  DefRect := inherited CalcHintRect(MaxWidth, AHint, AData);
  Result := DefRect;
  if not Manager.Skinned then begin
    Image := Template.FImageDefault;
    UpdateHintRect;
    if (FHintLocation.X = 0) or (FHintLocation.Y = 0) then p := GetMousePosition else p := FHintLocation;
    OffsetRect(Result, p.x, p.y);
    FMousePos := GetArrowPos(Result, p);
    Result := DefRect;
    case FMousePos of
      mpLeftTop : ;
      mpLeftBottom : if not Template.FImageLeftBottom.Image.Empty then Image := Template.FImageLeftBottom;
      mpRightTop : if not Template.FImageRightTop.Image.Empty then Image := Template.FImageRightTop;
      mpRightBottom : if not Template.FImageRightBottom.Image.Empty then Image := Template.FImageRightBottom;
    end;
    UpdateHintRect;
  end;
end;

constructor TacPngHintWindow.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure CopyChannel32(const DstBmp, SrcBmp : TBitmap; const Channel : integer);
var
  Dst, Src : PByteArray;
  X, Y : integer;
begin
  for Y := 0 to DstBmp.Height - 1 do begin
    Dst := DstBmp.ScanLine[Y];
    Src := SrcBmp.ScanLine[Y];
    for X := 0 to DstBmp.Width - 1 do Dst[X * 4 + Channel] := Src[X * 4 + Channel];
  end;
end;

procedure TacPngHintWindow.CreateAlphaBmp(const Width, Height: integer);
var
  SrcBmp, TempBmp : TBitmap;
begin
  if Manager.Skinned then inherited else begin
    SetWindowRgn(Handle, 0, False);
    AlphaBmp := CreateBmp32(Width, Height);
    if (Template <> nil) and not Image.FImage.Empty then begin
      SrcBmp := Image.FImage;
      PaintControlByTemplate(AlphaBmp, SrcBmp, Rect(0, 0, AlphaBmp.Width, AlphaBmp.Height), Rect(0, 0, SrcBmp.Width, SrcBmp.Height),
        Rect(Image.BordersWidths.Left, Image.BordersWidths.Top, Image.BordersWidths.Right, Image.BordersWidths.Bottom),
        Rect(ord(Image.BorderDrawModes.Left), ord(Image.BorderDrawModes.Top), ord(Image.BorderDrawModes.Right), ord(Image.BorderDrawModes.Bottom)), boolean(ord(Image.BorderDrawModes.Center)));
    end;
    FBlend.SourceConstantAlpha := 255;
    TempBmp := TBitmap.Create;
    TempBmp.Assign(AlphaBmp);
    AlphaBmp.Canvas.Font.Assign(Template.FFont);
    TextOut(AlphaBmp);
    CopyChannel32(AlphaBmp, TempBmp, 3);
    TempBmp.Free;
  end;
end;

{ TacHintTemplate }

procedure TacHintTemplate.AssignTo(Dest: TPersistent);
begin
  if Dest <> nil then begin
    ImageDefault.AssignTo(TacHintTemplate(Dest).ImageDefault);
    Img_LeftBottom.AssignTo(TacHintTemplate(Dest).Img_LeftBottom);
    Img_RightBottom.AssignTo(TacHintTemplate(Dest).Img_RightBottom);
    Img_RightTop.AssignTo(TacHintTemplate(Dest).Img_RightTop);
    TacHintTemplate(Dest).Font.Assign(Font);
    TacHintTemplate(Dest).Name := Name;
  end
  else inherited;
end;

constructor TacHintTemplate.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FOwner := TacHintTemplates(Collection);
  FImageDefault := TacHintImage.Create(Self);
  FImageLeftBottom := TacHintImage.Create(Self);
  FImageRightBottom := TacHintImage.Create(Self);
  FImageRightTop := TacHintImage.Create(Self);
  FFont := TFont.Create;
end;

destructor TacHintTemplate.Destroy;
begin
  FreeAndNil(FImageDefault);
  FreeAndNil(FImageLeftBottom);
  FreeAndNil(FImageRightBottom);
  FreeAndNil(FImageRightTop);
  FreeAndNil(FFont);
  inherited Destroy;         
end;

procedure TacHintTemplate.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  FOwner.FOwner.Changed;
end;

procedure TacHintTemplate.SetHintImage(const Index: Integer; const Value: TacHintImage);
begin
  case Index of
    0 : FImageDefault.Assign(Value);
    1 : FImageLeftBottom.Assign(Value);
    2 : FImageRightBottom.Assign(Value);
    3 : FImageRightTop.Assign(Value);
  end;
end;

{ TacHintTemplates }

procedure TacHintTemplates.AssignTo(Dest: TPersistent);
var
  i : integer;
begin
  if Dest <> nil then begin
    TacHintTemplates(Dest).Clear;
    for i := 0 to Count - 1 do begin
      TacHintTemplates(Dest).Add;
      Items[i].AssignTo(TacHintTemplates(Dest).Items[i]);
    end;
  end
  else inherited;
end;

constructor TacHintTemplates.Create(AOwner: TsAlphaHints);
begin
  FOwner := AOwner;
  inherited Create(TacHintTemplate);
end;

destructor TacHintTemplates.Destroy;
begin
  inherited Destroy;
  FOwner := nil;
end;

function TacHintTemplates.GetItem(Index: Integer): TacHintTemplate;
begin
  Result := TacHintTemplate(inherited GetItem(Index));
end;

function TacHintTemplates.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TacHintTemplates.SetItem(Index: Integer; Value: TacHintTemplate);
begin
  inherited SetItem(Index, Value);
end;

{ TacBordersSizes }

procedure TacBordersSizes.AssignTo(Dest: TPersistent);
begin
  if Dest <> nil then begin
    TacBordersSizes(Dest).Top := Top;
    TacBordersSizes(Dest).Left := Left;
    TacBordersSizes(Dest).Bottom := Bottom;
    TacBordersSizes(Dest).Right := Right;
  end
  else inherited;
end;

constructor TacBordersSizes.Create(AOwner: TacHintImage);
begin
  FTop := 0;
  FLeft := 0;
  FBottom := 0;
  FRight := 0;
  FOwner := AOwner;
end;

function TacBordersSizes.GetInteger(const Index: Integer): integer;
begin
  case Index of
    0 : Result := FTop;
    1 : Result := FLeft;
    2 : Result := FBottom;
    3 : Result := FRight;
  end;
  if Result < 0 then Result := 0;
end;

procedure TacBordersSizes.SetInteger(Index, Value: integer);
const
  CenterPiece = 1;
begin
  case Index of
    0 : begin
      FTop := min(Value, FOwner.ImageHeight - CenterPiece);
      FBottom := min(FBottom, FOwner.ImageHeight - FTop - CenterPiece);
    end;
    1 : begin
      FLeft := min(Value, FOwner.ImageWidth - CenterPiece);
      FRight := min(FRight, FOwner.ImageWidth - FLeft - CenterPiece);
    end;
    2 : begin
      FBottom := min(Value, FOwner.ImageHeight - CenterPiece);
      FTop := min(FTop, FOwner.ImageHeight - FBottom - CenterPiece);
    end;
    3 : begin
      FRight := min(Value, FOwner.ImageWidth - CenterPiece);
      FLeft := min(FLeft, FOwner.ImageWidth - FRight - CenterPiece);
    end;
  end;
  if not acLocalUpd and (FOwner.FOwner.FOwner <> nil) then begin
    acLocalUpd := True;
    FOwner.FOwner.FOwner.FOwner.Changed;
    acLocalUpd := False;
  end;
end;

{ TacHintImage }

constructor TacHintImage.Create(AOwner: TacHintTemplate);
begin
  FImage := TPngGraphic.Create;
  FOwner := AOwner;
  FBordersWidths := TacBordersSizes.Create(Self);
  FClientMargins := TacBordersSizes.Create(Self);
  FBorderDrawModes := TacBorderDrawModes.Create(Self);
end;

destructor TacHintImage.Destroy;
begin
  FreeAndNil(FBordersWidths);
  FreeAndNil(FBorderDrawModes);
  FreeAndNil(FClientMargins);
  FreeAndNil(FImage);
  inherited Destroy;
end;

function TacHintImage.GetImgHeight: integer;
begin
  if FImage.Empty then Result := 0 else Result := Image.Height;
end;

function TacHintImage.GetImgWidth: integer;
begin
  if FImage.Empty then Result := 0 else Result := Image.Width;
end;

procedure TacHintImage.SetClientMargins(const Value: TacBordersSizes);
begin
  FClientMargins.Assign(Value);
{
  if FClientMargins.Left > ImageWidth - BordersWidths.Left - BordersWidths.Right then FClientMargins.Left := ImageWidth - BordersWidths.Left - BordersWidths.Right;
  if FClientMargins.Top > ImageHeight - BordersWidths.Top - BordersWidths.Bottom then FClientMargins.Top := ImageHeight - BordersWidths.Top - BordersWidths.Bottom;

  FClientMargins.Right := min(FClientMargins.Right, ImageWidth - BordersWidths.Right);
  FClientMargins.Bottom := min(FClientMargins.Bottom, ImageHeight - BordersWidths.Bottom);

  if FClientMargins.Right < FClientMargins.Left then FClientMargins.Right := FClientMargins.Left;
  if FClientMargins.Bottom < FClientMargins.Top then FClientMargins.Bottom := FClientMargins.Top;
}
end;

procedure TacHintImage.SetImage(const Value: TPngGraphic);
begin
  FImage.Assign(Value);
  ImageChanged;
  FOwner.FOwner.FOwner.Changed;
end;

procedure TacHintImage.SetImgHeight(const Value: integer); begin end;

procedure TacHintImage.SetImgWidth(const Value: integer); begin end;

procedure TacHintImage.SetBordersWidths(const Value: TacBordersSizes);
begin
  FBordersWidths.Assign(Value);
end;

procedure TacHintImage.ImageChanged;
var
  Bmp : PRGBAArray;
  X, Y, h, w : integer;
  rgba : TsColor_;
begin
  w := FImage.Width - 1;
  h := FImage.Height - 1;

  BordersWidths.FLeft  := w div 2;
  BordersWidths.FRight := BordersWidths.FLeft;
  ClientMargins.FLeft  := BordersWidths.FLeft;
  ClientMargins.FRight := BordersWidths.FLeft;

  BordersWidths.FTop    := h div 2;
  BordersWidths.FBottom := BordersWidths.FTop;
  ClientMargins.FTop    := BordersWidths.FTop;
  ClientMargins.FBottom := BordersWidths.FTop;

  rgba.A := 0;
  rgba.R := 0;
  rgba.G := 0;
  rgba.B := 0;
  for Y := 0 to h do begin
    Bmp := FImage.ScanLine[Y];
    for X := 0 to w do if Bmp[X].A = 0 then Bmp[X] := rgba
  end;
end;

procedure TacHintImage.AssignTo(Dest: TPersistent);
begin
  if Dest <> nil then begin
    TacHintImage(Dest).Image.Assign(Image);
    TacHintImage(Dest).ClientMargins.Assign(ClientMargins);
    TacHintImage(Dest).BorderDrawModes.Assign(BorderDrawModes);
    TacHintImage(Dest).BordersWidths.Assign(BordersWidths);
  end
  else inherited;
end;

{ TacBorderDrawModes }

procedure TacBorderDrawModes.AssignTo(Dest: TPersistent);
begin
  if Dest <> nil then begin
    TacBorderDrawModes(Dest).Top := Top;
    TacBorderDrawModes(Dest).Left := Left;
    TacBorderDrawModes(Dest).Bottom := Bottom;
    TacBorderDrawModes(Dest).Right := Right;
    TacBorderDrawModes(Dest).Center := Center;
  end
  else inherited;
end;

constructor TacBorderDrawModes.Create(AOwner: TacHintImage);
begin
  FBottom := dmStretch;
  FLeft := dmStretch;
  FTop := dmStretch;
  FRight := dmStretch;
  FCenter := dmStretch;
  FOwner := AOwner;
end;

procedure TacBorderDrawModes.SetDrawMode(const Index: Integer; const Value: TacBorderDrawMode);
begin
  case Index of
    0 : FTop := Value;
    1 : FLeft := Value;
    2 : FBottom := Value;
    3 : FRight := Value;
    4 : FCenter := Value;
  end;
  if FOwner.FOwner.FOwner <> nil then FOwner.FOwner.FOwner.FOwner.Changed;
end;

var
  HintStream : TResourceStream;

initialization
  DefaultTemplate := TacHintTemplate.Create(nil);
  HintStream := TResourceStream.Create(hInstance, 'ACHINT', RT_RCDATA);
  DefaultTemplate.ImageDefault.Image.LoadFromStream(HintStream);
  DefaultTemplate.ImageDefault.BordersWidths.Left := 7;
  DefaultTemplate.ImageDefault.BordersWidths.Top := 7;
  DefaultTemplate.ImageDefault.BordersWidths.Right := 15;
  DefaultTemplate.ImageDefault.BordersWidths.Bottom := 15;
  DefaultTemplate.ImageDefault.ClientMargins.Left := 11;
  DefaultTemplate.ImageDefault.ClientMargins.Top := 7;
  DefaultTemplate.ImageDefault.ClientMargins.Right := 15;
  DefaultTemplate.ImageDefault.ClientMargins.Bottom := 14;
  DefaultTemplate.ImageDefault.BorderDrawModes.Left := dmStretch;
  DefaultTemplate.ImageDefault.BorderDrawModes.Top := dmStretch;
  DefaultTemplate.ImageDefault.BorderDrawModes.Right := dmStretch;
  DefaultTemplate.ImageDefault.BorderDrawModes.Bottom := dmStretch;
  DefaultTemplate.ImageDefault.BorderDrawModes.Center := dmStretch;

  FreeAndNil(HintStream);              

finalization
  FreeAndNil(DefaultTemplate);

end.



