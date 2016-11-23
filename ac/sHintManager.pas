unit sHintManager;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  sGraphUtils, sConst, acntUtils, ExtCtrls, IniFiles, sHtmlParse;

{$IFNDEF NOTFORHELP}
const
  DefStyle = hsEllipse;
  DefAnimationTime = 250;
{$ENDIF} // NOTFORHELP

type
{$IFNDEF NOTFORHELP}
  TsMousePosition = (mpLeftTop, mpLeftBottom, mpRightTop, mpRightBottom);
  TsHintsEffectsMode = (hmNone, hmSystem, hmCustom);
  TsHintManager = class;
  TsCustomHintWindow = class;

  TsHintKind = class(TPersistent)
  private
    FOwner : TsHintManager;
    FShadowEnabled: boolean;
    FMarginV: integer;
    FTexturePercent: integer;
    FShadowBlur: integer;
    FTransparency: integer;
    FGradientPercent: integer;
    FMarginH: integer;
    FExOffset: integer;
    FShadowTransparency: integer;
    FShadowOffset: integer;
    FBevelWidth: integer;
    FMaxWidth: integer;
    FRadius: integer;
    FBevel: integer;
    FGradientData: string;
    FTextureFile: string;
    FColorBorderTop: TColor;
    FColorBorderBottom: TColor;
    FColor: TColor;
    FShadowColor: TColor;
    FFont: TFont;
    FTexture: TPicture;
    FStyle: TsHintStyle;
    FBlur: integer;
    procedure SetFont(const Value: TFont);
    procedure SetStyle(const Value: TsHintStyle);
    procedure SetTexture(const Value: TPicture);
    function GetShadowEnabled: boolean;
    function GetShadowBlur: integer;
    function GetShadowOffset: integer;
    function GetShadowTransparency: integer;
    procedure SetBlur(const Value: integer);
    procedure SetShadowColor(const Value: TColor);
  public
    constructor Create (AOwner: TsHintManager);
    destructor Destroy; override;
  published
    property Style: TsHintStyle read FStyle write SetStyle default DefStyle;
    property Radius : integer read FRadius write FRadius default 20;
    property BevelWidth : integer read FBevelWidth write FBevelWidth default 1;
    property Blur : integer read FBlur write SetBlur Default 0; // Leaved for compatibility, should be removed later
    property ExOffset : integer read FExOffset write FExOffset default 32;
    property GradientData : string read FGradientData write FGradientData;
    property Texture : TPicture read FTexture write SetTexture;
    property TextureFile : string read FTextureFile write FTextureFile;
    property GradientPercent : integer read FGradientPercent write FGradientPercent default 0;
    property TexturePercent : integer read FTexturePercent write FTexturePercent default 0;
    property Bevel : integer read FBevel write FBevel;
    property Color : TColor read FColor write FColor;
    property ColorBorderTop : TColor read FColorBorderTop write FColorBorderTop;
    property ColorBorderBottom : TColor read FColorBorderBottom write FColorBorderBottom;
    property Transparency : integer read FTransparency write FTransparency;

    property ShadowBlur : integer read GetShadowBlur write FShadowBlur;
    property ShadowColor : TColor read FShadowColor write SetShadowColor default clBlack; // Leaved for compatibility, should be removed later
    property ShadowEnabled : boolean read GetShadowEnabled write FShadowEnabled;
    property ShadowOffset : integer read GetShadowOffset write FShadowOffset;
    property ShadowTransparency : integer read GetShadowTransparency write FShadowTransparency;
    property MarginH : integer read FMarginH write FMarginH default 15;
    property MarginV : integer read FMarginV write FMarginV default 10;
    property MaxWidth : integer read FMaxWidth write FMaxWidth default 200;
    property Font: TFont read FFont write SetFont;
  end;

{$ENDIF}
  TacShowHintEvent = procedure (var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo; var Frame : TFrame) of object;

  TsHintManager = class(TComponent)
{$IFNDEF NOTFORHELP}
  private
    FPauseHide: integer;
    FHTMLMode : boolean;
    FPredefinitions: TsHintsPredefinitions;
    FOnShowHint: TacShowHintEvent;
    FHintKind: TsHintKind;
    FDefaultMousePos: TsMousePosition;
    FAnimated: boolean;
    FHintPos: TPoint;
{$IFNDEF ACHINTS}
    FSkinSection: TsSkinSection;
    FUseSkinData: boolean;
{$ENDIF}
    procedure SetNewStyle(hs: TsHintStyle);
    procedure SetPredefinitions(const Value: TsHintsPredefinitions);
    function GetAnimated: boolean;
{$IFNDEF ACHINTS}
    procedure SetSkinData(const Value: boolean);
    procedure SetPauseHide(const Value: integer);
{$ENDIF}
  public
    FTempHint: TsCustomHintWindow;
    FCacheBmp : TBitmap;
    procedure OnShowHintApp(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure AfterConstruction; override;
    procedure Invalidate;
    procedure PaintBG(BGBmp : TBitmap; R : TRect; ci : TCacheInfo);
    procedure ShowHint(TheControl: TControl; HintText: String);
    procedure HideHint;
    procedure UpdateProperties;
    function Skinned : boolean;
  published
{$ENDIF} // NOTFORHELP
    property OnShowHint: TacShowHintEvent read FOnShowHint write FOnShowHint;
    property Animated : boolean read GetAnimated write FAnimated default True;
    property DefaultMousePos : TsMousePosition read FDefaultMousePos write FDefaultMousePos default mpLeftTop;
    property HintKind: TsHintKind read FHintKind write FHintKind;
    property HTMLMode : boolean read FHTMLMode write FHTMLMode default False;
    property PauseHide : integer read FPauseHide write SetPauseHide default 5000;
    property Predefinitions : TsHintsPredefinitions read FPredefinitions write SetPredefinitions default shEllipse;
{$IFNDEF ACHINTS}
    property SkinSection : TsSkinSection read FSkinSection write FSkinSection;
    property UseSkinData : boolean read FUseSkinData write SetSkinData default False;
{$ENDIF}
  end;

{$IFNDEF NOTFORHELP}
  TsCustomHintWindow = class(THintWindow)
  private
    FHintLocation: TPoint;
    procedure WMEraseBkGND (var Message: TWMPaint); message WM_ERASEBKGND;
    procedure WMNCPaint (var Message: TWMPaint); message WM_NCPaint;
    procedure PrepareMask;
  protected
    SkinIndex, BorderIndex, BGIndex : integer;
    rgn : hrgn;
    dx, dy : integer;
    FMousePos : TsMousePosition;
    procedure CreateParams(var Params: TCreateParams); override;
    function GetMousePosition : TPoint; virtual;
    function MainRect: TRect; dynamic;
    function ShadowTransparency : integer;
    procedure WndProc(var Message: TMessage); override;
    function SkinMargin(Border : byte): integer;
  public
    BodyBmp: TBitmap;
    MaskBmp : TBitmap; // for debug
    ScreenBmp : TBitmap; // for debug
    AlphaBmp : TBitmap;
    function GetMask : TBitmap; dynamic;
    function GetBody : TBitmap; dynamic;
    procedure PaintShadow(Bmp : TBitmap = nil); virtual;
    property HintLocation: TPoint read FHintLocation write FHintLocation;
    procedure Paint; override;
    procedure PaintBG(Bmp: TBitmap; aRect: TRect); dynamic;
    procedure PaintBorder(Bmp: TBitmap); dynamic;
    procedure TextOut(Bmp: TBitmap); dynamic;
    function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect; override;
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
  end;

  { Hint window type for style hsSimply}
  TsSimplyHintWindow = class(TsCustomHintWindow)
  public
    procedure PaintShadow(Bmp : TBitmap = nil); override;
    function GetMask : TBitmap; override;
    function GetBody : TBitmap; override;
    function GetMousePosition : TPoint; override;
    procedure PaintBorder(Bmp: TBitmap); override;
  end;

  { Hint window type for style hsComics}
  TsComicsHintWindow = class(TsCustomHintWindow)
  public
    function MainRect: TRect; override;
    procedure PaintShadow(Bmp : TBitmap = nil); override;
    function GetMask : TBitmap; override;
    function GetArrowPosition : TPoint;
    function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect; override;
    function GetBody : TBitmap; override;
    procedure PaintBorder(Bmp: TBitmap); override;
  end;

  { Hint window type for style hsEllipse}
  TsEllipseHintWindow = class(TsCustomHintWindow)
  public
    function GetArrowPosition : TPoint;
    function GetBody : TBitmap; override;
    function GetMask : TBitmap; override;
    function MainRect: TRect; override;
    procedure PaintShadow(Bmp : TBitmap = nil); override;
    function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect; override;
    procedure PaintBorder(Bmp: TBitmap); override;
  end;

  { Hint window type for style hsBalloon}
  TsBalloonHintWindow = class(TsCustomHintWindow)
  public
    BalloonCount : integer;
    Divid : integer;
    procedure PaintShadow(Bmp : TBitmap = nil); override;
    function GetMask : TBitmap; override;
    function GetCustMask(Shadow : boolean) : TBitmap;
    function GetBody : TBitmap; override;
    function GetArrowPosition : TPoint;
    function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect; override;
    constructor Create(AOwner:TComponent); override;
    function MainRect: TRect; override;
    procedure PaintBorder(Bmp: TBitmap); override;
  end;

var
  Manager : TsHintManager;
  acHintsInEditor : boolean = False;

{$IFDEF ACHINTS}
procedure Register;
{$ENDIF}
{$ENDIF} // NOTFORHELP

implementation

uses {$IFNDEF ACHINTS}sVclUtils, sMessages, sSkinProps, sSkinManager, {$ENDIF}
  sGradient, math, sStyleSimply, sAlphaGraph{$IFDEF LOGGED}, sDebugMsgs{$ENDIF};

const
  NCS_DROPSHADOW = $20000;
  DelayValue = 8;
  SkinBorderWidth = 4;

var
  FBlend: TBlendFunction;
  HintFrame : TFrame = nil;

{$IFDEF ACHINTS}
procedure Register;
begin
  RegisterComponents('AlphaTools', [TsHintManager]);
end;
{$ENDIF}

{ TsHintManager }

procedure BorderByMask(SrcBmp, MskBmp: TBitmap; ColorTop, ColorBottom: TsColor);
var
  S1, S2, S2t, S2b : PRGBArray;
  t, b : boolean;
  X, Y, sw, sh: Integer;
  function BlackPoint(c: TsRGB) : boolean;
  begin
    Result := c.R + c.G + c.B < 765;
  end;
begin
  S2t := nil;
  S2b := nil;
  sh := SrcBmp.Height - 1;
  sw := SrcBmp.Width - 1;
  if SrcBmp.Height <> MskBmp.Height then Exit;
  if SrcBmp.Width <> MskBmp.Width then Exit;
  if SrcBmp.Height < 1 then Exit;
  if SrcBmp.Width < 1 then Exit;
  for Y := 0 to sh do begin
    S1 := SrcBmp.ScanLine[Y];
    S2 := MskBmp.ScanLine[Y];
    if Y > 0 then begin
      S2t := MskBmp.ScanLine[Y - 1];
      t := True;
    end
    else t := False;
    if Y < SrcBmp.Height - 1 then begin
      S2B := MskBmp.ScanLine[Y + 1];
      b := True;
    end
    else b := False;

    for X := 0 to sw do begin
      if BlackPoint(S2[X]) then begin
        if ((X > 0) and not BlackPoint(S2[X - 1])) or (X = 0) or (t and not BlackPoint(S2t[X])) or not t then begin
          S1[X].R := ColorTop.R;
          S1[X].G := ColorTop.G;
          S1[X].B := ColorTop.B;
        end
        else
        if ((X < SrcBmp.Width - 1) and not BlackPoint(S2[X + 1])) or (X = SrcBmp.Width - 1) or (b and not BlackPoint(S2b[X])) or not b then begin
          S1[X].R := ColorBottom.R;
          S1[X].G := ColorBottom.G;
          S1[X].B := ColorBottom.B;
        end;
      end;
    end
  end;
END;

constructor TsHintManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  FForm := GetParentForm(TControl(AOwner));
  FHintKind := TsHintKind.Create(Self);

  FCacheBmp := CreateBmp24(0, 0);

  FHTMLMode := False;
  FPauseHide := 5000;
  FDefaultMousePos := mpLeftTop;
  FHintPos := Point(-1, -1);

  Application.HintPause := 500;
  Application.HintShortPause := 200;

  FPreDefinitions := shEllipse;
  FAnimated := True;
{$IFNDEF ACHINTS}
  FUseSkinData := False;
{$ENDIF}
  if not (csDesigning in ComponentState) then Application.OnShowHint := OnShowHintApp;
end;

destructor TsHintManager.Destroy;
begin
  HintWindowClass := THintWindow;
  Application.OnShowHint := nil;
  FreeAndNil(FCacheBmp);
  FreeAndNil(FHintKind);
  inherited;
end;

procedure TsHintManager.Invalidate;
begin
  PreDefinitions := FPreDefinitions;
  HintKind.Style := HintKind.Style;
end;

procedure TsHintManager.Loaded;
begin
  inherited;
  Application.HintHidePause := FPauseHide;
{$IFNDEF ACHINTS}
  if FSkinSection = '' then FSkinSection := s_Hint;
{$ENDIF}
  SetNewStyle(HintKind.Style);
  if not (csDesigning in ComponentState) then UpdateProperties;
end;

procedure TsHintManager.OnShowHintApp(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
begin
  FHintPos := HintInfo.HintPos;
  if Assigned(FOnShowHint) then FOnShowHint(HintStr, CanShow, HintInfo, HintFrame) else inherited;
  if (FHintPos.x <> HintInfo.HintPos.x) and (FHintPos.y <> HintInfo.HintPos.y) then FHintPos := HintInfo.HintPos else begin
  end;
end;

procedure TsHintManager.PaintBG(BGBmp: TBitmap; R: TRect; ci: TCacheInfo);
var
  aRect: TRect;
  TransColor : TsColor;
  Transparency : integer;
  iDrawed : boolean;
  TempBmp : TBitmap;

  GradientArray : TsGradArray;

  procedure FillCanvas(bmp : TBitmap); begin
    BMP.Canvas.Pen.Style := psClear;
    BMP.Canvas.Brush.Style := bsSolid;
    BMP.Canvas.Brush.Color := ColortoRGB(HintKind.Color);
    BMP.Canvas.Rectangle(aRect.Left, aRect.Top, aRect.Right + 1, aRect.Bottom + 1);
  end;
  procedure PaintAddons(var aBmp : TBitmap);
  var bmp : TBitmap;
  begin
    iDrawed := False;
    // BGImage painting
    if (HintKind.TexturePercent > 0) then begin
      TileBitmap(aBmp.Canvas, Rect(aRect.Left, aRect.Top, aRect.Right, aRect.Bottom), HintKind.Texture.Graphic);
      iDrawed := True;
    end
    else begin
      FillCanvas(aBmp);
    end;
    // BGGradient painting
    if (HintKind.GradientPercent > 0) then begin
      if iDrawed then begin
        bmp := TBitmap.Create;
        try
          bmp.PixelFormat := pf24bit;
          bmp.Width := WidthOf(aRect);
          bmp.Height := HeightOf(aRect);

          if Length(HintKind.GradientData) > 0 then PaintGrad(Bmp, Rect(0, 0, Bmp.Width, Bmp.Height), GradientArray) else FillCanvas(Bmp);

          TransColor.A := 0;
          TransColor.R := IntToByte(HintKind.TexturePercent shl 8 div 100); TransColor.G := TransColor.R; TransColor.B := TransColor.R;
          SumBmpRect(aBmp, Bmp, TransColor, Rect(0, 0, Bmp.Width - 1, Bmp.Height{ - 1}), Point(aRect.Left, aRect.Top));
        finally
          FreeAndNil(Bmp);
        end;
      end
      else begin
        if Length(HintKind.GradientData) > 0 then begin
          PaintGrad(aBmp, aRect, GradientArray);
        end
        else begin
          FillCanvas(aBmp);
        end;
      end;
    end;
    case HintKind.GradientPercent + HintKind.TexturePercent of
      1..99 : begin
        BlendColorRect(aBmp, aRect, (HintKind.GradientPercent + HintKind.TexturePercent),
                         HintKind.Color);
      end;
      100 : begin end
      else begin
        aBMP.Canvas.Pen.Style := psClear;
        aBMP.Canvas.Brush.Style := bsSolid;
        aBMP.Canvas.Brush.Color := ColortoRGB(HintKind.Color);
        Rectangle(aBMP.Canvas.Handle, aRect.Left, aRect.Top, aRect.Right + 1, aRect.Bottom + 1);
      end;
    end;
  end;
begin
  if not acHintsInEditor then Transparency := 0 else Transparency := HintKind.Transparency;
  aRect := R;
  // Properties definition from skin file
  if HintKind.GradientPercent > 0 then begin
    PrepareGradArray(HintKind.GradientData, GradientArray);
    if Length(GradientArray) = 0 then HintKind.GradientPercent := 0;
  end;

  if ci.Ready and (Transparency = 100) then begin
    if BGBmp <> ci.Bmp then begin
      BitBlt(BGBmp.Canvas.Handle, aRect.Left, aRect.Top, WidthOf(aRect), HeightOf(aRect),
        ci.Bmp.Canvas.Handle, ci.X, ci.Y, SRCCOPY);
    end;
  end
  else if not ci.Ready or (Transparency = 0) then begin
    PaintAddons(BGBmp);
  end
  else if ci.Ready and (Transparency > 0) then begin
    TempBmp := TBitmap.Create;

    try
      TempBmp.Width := WidthOf(aRect);
      TempBmp.Height := HeightOf(aRect);
      TempBmp.PixelFormat := pf24bit;
      OffsetRect(aRect, - aRect.Left, - aRect.Top);
      PaintAddons(TempBmp);
      aRect := R;
      TransColor.A := 0;
      TransColor.R := IntToByte(Transparency shl 8 div 100);
      TransColor.G := TransColor.R;
      TransColor.B := TransColor.R;

      if ci.Bmp <> BGBmp then begin
        BitBlt(BGBmp.Canvas.Handle, aRect.Left, aRect.Top, WidthOf(aRect), HeightOf(aRect),
             ci.Bmp.Canvas.Handle, ci.X, ci.Y, SRCCOPY);
      end;
      SumBmpRect(BGBmp, TempBmp, TransColor, Rect(0, 0, WidthOf(aRect), HeightOf(aRect)), Point(aRect.Left, aRect.Top));

    finally
      FreeAndNil(TempBmp);
    end;
  end;
end;

procedure TsHintManager.SetNewStyle(hs: TsHintStyle);
begin
  if not (csDesigning in ComponentState) then begin
    case hs of
      hsNone : begin
        Manager := nil;
        HintWindowClass := TsSimplyHintWindow;
      end;
      hsStandard : begin
        Manager := nil;
        HintWindowClass := THintWindow;
      end;
      hsSimply : begin
        Manager := Self;
        HintWindowClass := TsSimplyHintWindow;
      end;
      hsComics : begin
        Manager := Self;
        HintWindowClass := TsComicsHintWindow;
      end;
      hsEllipse : begin
        Manager := Self;
        HintWindowClass := TsEllipseHintWindow;
      end;
      hsBalloon : begin
        Manager := Self;
        HintWindowClass := TsBalloonHintWindow;
      end;
    end;
  end;
end;

procedure TsHintManager.SetPredefinitions(const Value: TsHintsPredefinitions);
begin
//  if csLoading in ComponentState then Exit;
  FPredefinitions := Value;
end;

procedure TsHintManager.ShowHint(TheControl: TControl; HintText: String); // Added by Matthew Bieschke
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
  If not Assigned(FTempHint) Then Case HintKind.Style of
    hsBalloon: FTempHint := TsBalloonHintWindow.Create(Self);
    hsComics: FTempHint := TsComicsHintWindow.Create(Self);
    hsEllipse: FTempHint := TsEllipseHintWindow.Create(Self);
    hsSimply: FTempHint := TsSimplyHintWindow.Create(Self);
  end;
  // Was hint creation successful ?
  If Assigned(FTempHint) Then With FTempHint Do Begin
    HintRect := CalcHintRect(HintKind.MaxWidth, HintText, NIL);
    HintLocation := TheControl.ClientToScreen(HL);
    HintLocation := Point(HintLocation.x + TheControl.Width div 2, HintLocation.Y + TheControl.Height div 3); 
    ActivateHint(HintRect, HintText);
  End;
end;

procedure TsHintManager.HideHint;
begin
  FreeAndNil(FTempHint);
end;

procedure TsHintManager.UpdateProperties;
begin
  case FPredefinitions of
    shSimply : begin // Simply with shadow
      HintKind.BevelWidth := 1;
      HintKind.MarginH := 4;
      HintKind.MarginV := 2;
      HintKind.MaxWidth := 240;
      HintKind.Color := clInfoBk;
      HintKind.Transparency := 0;
      HintKind.ColorBorderTop := 0;
      HintKind.ColorBorderBottom := 0;
      HintKind.Bevel := 1;
      HintKind.GradientPercent := 0;
      HintKind.GradientData := '';
      HintKind.ShadowBlur := 3;
      HintKind.ShadowOffset := 6;
      HintKind.ShadowTransparency := 40;
      HintKind.ShadowEnabled := True;
      HintKind.Font.Assign(Screen.HintFont);
      HintKind.Font.Style := [];
      HintKind.Font.Color := clBlack;
      HintKind.Style := hsSimply;
    end;
    shGradient : begin // Gradient
      HintKind.BevelWidth := 1;
      HintKind.MarginH := 6;
      HintKind.MarginV := 3;
      HintKind.MaxWidth := 240;
      HintKind.Color := 11992314;
      HintKind.Transparency := 10;
      HintKind.ColorBorderTop := clWhite;
      HintKind.ColorBorderBottom := 32896;
      HintKind.Bevel := 1;
      HintKind.GradientPercent := 100;
      HintKind.GradientData := '167827;6865090;24;2;0;6865090;6865090;24;2;0;6865090;15400959;24;2;0;15400959;6865090;24;2;0;6865090;6865090;0;2;0';
      HintKind.ShadowBlur := 5;
      HintKind.ShadowOffset := 6;
      HintKind.ShadowTransparency := 50;
      HintKind.ShadowEnabled := True;
      HintKind.Font.Assign(Screen.HintFont);
      HintKind.Font.Style := [];
      HintKind.Font.Color := clBlack;
      HintKind.Style := hsSimply;
    end;
    shTransparent : begin // Simply transparent 2
      HintKind.BevelWidth := 0;
      HintKind.MarginH := 13;
      HintKind.MarginV := 5;
      HintKind.MaxWidth := 240;
      HintKind.Color := 11992314;
      HintKind.Transparency := 20;
      HintKind.ColorBorderTop := 16777215;
      HintKind.ColorBorderBottom := clOlive;
      HintKind.Bevel := 1;
      HintKind.GradientPercent := 0;
      HintKind.GradientData := '';
      HintKind.ShadowBlur := 3;
      HintKind.ShadowOffset := 10;
      HintKind.ShadowTransparency := 70;
      HintKind.ShadowEnabled := True;
      HintKind.Font.Assign(Screen.HintFont);
      HintKind.Font.Style := [];
      HintKind.Font.Color := clBlack;
      HintKind.Style := hsSimply;
    end;
    shEllipse : begin // Ellipse transparent
      HintKind.BevelWidth := 1;
      HintKind.ExOffset := 16;
      HintKind.MarginH := 6;
      HintKind.MarginV := 12;
      HintKind.MaxWidth := 240;
      HintKind.Color := 12778748;
      HintKind.Transparency := 5;
      HintKind.ColorBorderTop := 32896;
      HintKind.ColorBorderBottom := clBlack;
      HintKind.Bevel := 1;
      HintKind.GradientPercent := 100;
      HintKind.GradientData := '7728127;16777215;99;0;0;16777215;16777215;0;0;0';
      HintKind.ShadowBlur := 4;
      HintKind.ShadowOffset := 16;
      HintKind.ShadowTransparency := 75;
      HintKind.ShadowEnabled := True;
      HintKind.Font.Assign(Screen.HintFont);
      HintKind.Font.Style := [];
      HintKind.Font.Color := clBlack;
      HintKind.Style := hsEllipse;
    end;
    shBalloon : begin // Baloon
      HintKind.BevelWidth := 6;
      HintKind.ExOffset := 18;
      HintKind.MarginH := 6;
      HintKind.MarginV := 6;
      HintKind.MaxWidth := 240;
      HintKind.Color := 12778748;
      HintKind.Transparency := 10;
      HintKind.ColorBorderTop := 4227200;
      HintKind.ColorBorderBottom := 4227200;
      HintKind.Bevel := 1;
      HintKind.GradientPercent := 0;
      HintKind.GradientData := '';
      HintKind.ShadowBlur := 6;
      HintKind.ShadowOffset := 8;
      HintKind.ShadowTransparency := 70;
      HintKind.ShadowEnabled := True;
      HintKind.Font.Assign(Screen.HintFont);
      HintKind.Font.Style := [];
      HintKind.Font.Color := clBlack;
      HintKind.Style := hsBalloon;
    end;
    shComicsGradient : begin // Comics gradient
      HintKind.BevelWidth := 1;
      HintKind.ExOffset := 24;
      HintKind.MarginH := 15;
      HintKind.MarginV := 7;
      HintKind.MaxWidth := 180;
      HintKind.Radius := 21;
      HintKind.Color := 16757839;
      HintKind.Transparency := 8;
      HintKind.ColorBorderTop := 4194304;
      HintKind.ColorBorderBottom := 4194304;
      HintKind.Bevel := 1;
      HintKind.GradientPercent := 100;
      HintKind.GradientData := '16753188;16777215;99;0;0;16777215;16777215;0;0;0';
      HintKind.ShadowBlur := 10;
      HintKind.ShadowOffset := 7;
      HintKind.ShadowTransparency := 28;
      HintKind.ShadowEnabled := True;
      HintKind.Font.Assign(Screen.HintFont);
      HintKind.Font.Style := [];
      HintKind.Font.Color := clBlack;
      HintKind.Style := hsComics;
    end;
    shComicsTransparent : begin // Comics transparent
      HintKind.BevelWidth := 1;
      HintKind.ExOffset := 19;
      HintKind.MarginH := 15;
      HintKind.MarginV := 8;
      HintKind.MaxWidth := 180;
      HintKind.Radius := 13;
      HintKind.Color := 12778748;
      HintKind.Transparency := 10;
      HintKind.ColorBorderTop := clBlack;
      HintKind.ColorBorderBottom := clBlack;
      HintKind.Bevel := 1;
      HintKind.GradientPercent := 0;
      HintKind.GradientData := '';
      HintKind.ShadowBlur := 4;
      HintKind.ShadowOffset := 7;
      HintKind.ShadowTransparency := 45;
      HintKind.ShadowEnabled := True;
      HintKind.Font.Assign(Screen.HintFont);
      HintKind.Font.Style := [];
      HintKind.Font.Color := clBlack;
      HintKind.Style := hsComics;
    end;
    shStandard : HintKind.Style := hsStandard;
    shNone : HintKind.Style := hsNone;
  end;
end;

procedure TsHintManager.AfterConstruction;
begin
  inherited;
{$IFNDEF ACHINTS}
  if FSkinSection = '' then FSkinSection := s_Hint;
{$ENDIF}
  SetNewStyle(HintKind.Style);
end;

function TsHintManager.GetAnimated: boolean;
begin
  Result := FAnimated;
end;

function TsHintManager.Skinned: boolean;
begin
{$IFNDEF ACHINTS}
  Result := Assigned(Manager) and Assigned(DefaultManager) and UseSkinData;
  if Result then Result := (Manager.SkinSection <> '') and (DefaultManager.GetSkinIndex(Manager.SkinSection) > -1);
{$ELSE}
  Result := False
{$ENDIF}
end;

{$IFNDEF ACHINTS}
procedure TsHintManager.SetSkinData(const Value: boolean);
begin
  FUseSkinData := Value;
  if not Value and not (csDesigning in ComponentState) then begin
    SetNewStyle(HintKind.Style);
    UpdateProperties;
  end;
end;
{$ENDIF}

procedure TsHintManager.SetPauseHide(const Value: integer);
begin
  if FPauseHide <> Value then begin
    FPauseHide := Value;
    Application.HintHidePause := FPauseHide;
  end;
end;

{ TsCustomHintWindow }

procedure TsCustomHintWindow.ActivateHint(Rect: TRect; const AHint: string);
var
  DC: HDC;
  w, h, i : integer;
  t, l : boolean;
  p : TPoint;
  Auto : boolean;

  FBmpSize: TSize;
  FBmpTopLeft: TPoint;
  StepB, Blend : real;
  StepCount : integer;
  procedure CreateAlphaBmp;
  var
    x, y : integer;
    FastDst : TacFast32;
    FastShadow : TacFast24;
    FastMask : TacFast24;
    FastBody : TacFast24;
    c : TsColor;
  begin
    FBlend.SourceConstantAlpha := Round(255 - 2.55 * Manager.HintKind.Transparency);
    FreeAndNil(AlphaBmp);  { MemoryLeak : ONT }
    AlphaBmp := CreateBmp32(w, h);
    FBmpSize.cx := w;
    FBmpSize.cy := h;
    FBmpTopLeft := Point(0, 0);

    FastDst := TacFast32.Create;
    FastShadow := TacFast24.Create;
    FastMask := TacFast24.Create;
    FastBody := TacFast24.Create;

    PrepareMask;
    FreeAndNil(BodyBmp);  { MemoryLeak : ONT }
    BodyBmp := GetBody;
    FillDC(Manager.FCacheBmp.Canvas.Handle, Classes.Rect(0, 0, w, h), clWhite);
    PaintShadow(Manager.FCacheBmp);

    AlphaBmp.PixelFormat := pf32bit;
    if FastDst.Attach(AlphaBmp) and FastShadow.Attach(Manager.FCacheBmp) and FastMask.Attach(MaskBmp) and FastBody.Attach(BodyBmp) then begin
      for y := 0 to h - 1 do for x := 0 to w - 1 do begin
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
    FreeAndnil(FastDst);
    FreeAndnil(FastMask);
    FreeAndnil(FastShadow); // v4.82
    FreeAndnil(FastBody); // v4.82
    if Assigned(BodyBmp) then FreeAndNil(BodyBmp);
  end;
begin
  if not Assigned(Manager) or (Manager.HintKind.Style = hsNone) {or not HandleAllocated }then exit;

  if Manager.Skinned and acLayered
    then SetClassLong(Handle, GCL_STYLE, GetClassLong(Handle, GCL_STYLE) or NCS_DROPSHADOW)
    else SetClassLong(Handle, GCL_STYLE, GetClassLong(Handle, GCL_STYLE) and not NCS_DROPSHADOW);

  Caption := AHint;
  if (FHintLocation.X = 0) or (FHintLocation.Y = 0) then p := GetMousePosition else p := FHintLocation;
  w := WidthOf(Rect);
  h := HeightOf(Rect);
  OffsetRect(Rect, p.x - Rect.Left, p.y - Rect.Top);
  UpdateBoundsRect(Rect);
  FMousePos := Manager.FDefaultMousePos;
  t := not (FMousePos in [mpLeftBottom, mpRightBottom]);
  l := not (FMousePos in [mpRightTop, mpRightBottom]);
  if FMousePos in [mpLeftBottom, mpRightBottom] then OffsetRect(Rect, 0, - h);
  if FMousePos in [mpRightTop, mpRightBottom] then OffsetRect(Rect, -w, 0);
  Auto := False; // Calc arrow position
  if Rect.Bottom > Screen.DesktopHeight then begin Rect.Top := p.y - h; t := False; Auto := True end;
  if Rect.Top < Screen.DesktopTop then begin Rect.Top := p.y; t := True; Auto := True end;
  if Rect.Right > Screen.DesktopWidth then begin Rect.Left := p.x - w; l := False; Auto := True end;
  if Rect.Left < Screen.DesktopLeft then begin Rect.Left := p.x; l := True; Auto := True end;
  if Auto then begin if t then begin if l then FMousePos := mpLeftTop else FMousePos := mpRightTop end else if l then FMousePos := mpLeftBottom else FMousePos := mpRightBottom end;
  Rect.Right := Rect.Left + w;
  Rect.Bottom := Rect.Top + h;

  Manager.FCacheBmp.Width := w;
  Manager.FCacheBmp.Height := h;

  if acLayered then begin
    SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, w, h, SWP_NOACTIVATE);
    CreateAlphaBmp;
    DC := GetDC(0);
    SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
    UpdateLayeredWindow(Handle, DC, nil, @FBmpSize, AlphaBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA);
    // Show window with hint
    if Manager.Animated and IsNTFamily then begin
{$IFNDEF ACHINTS}
      if Manager.Skinned then i := DefaultManager.gd[SkinIndex].Transparency else
{$ENDIF}
      i := Manager.HintKind.Transparency;
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
        FBlend.SourceConstantAlpha := Round((100 - Manager.HintKind.Transparency) * 2.55);
        UpdateLayeredWindow(Handle, DC, nil, @FBmpSize, AlphaBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA);
        ShowWindow(Handle, SW_SHOWNOACTIVATE);
      end
      else SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, w, h, SWP_SHOWWINDOW or SWP_NOACTIVATE);
    end;
    ReleaseDC(0, DC);
    if AlphaBmp <> nil then FreeAndNil(AlphaBmp);
  end
  else begin
    DC := GetDC(0); // grabbing
    if DC = 0 then begin {$IFNDEF ACHINTS}ShowError('GDI error (out of resources)');{$ENDIF} Exit end;
    if not Assigned(ScreenBmp) then ScreenBmp := CreateBmp24(w, h) else begin ScreenBmp.Width := w; ScreenBmp.Height := h end;
    BitBlt(ScreenBmp.Canvas.Handle, 0, 0, w, h, DC, Rect.Left, Rect.Top, SrcCopy);
    ReleaseDC(0, DC);
    if Assigned(ScreenBmp) then BitBlt(Manager.FCacheBmp.Canvas.Handle, 0, 0, w, h, ScreenBmp.Canvas.Handle, 0, 0, SrcCopy);
    SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, w, h, SWP_SHOWWINDOW or SWP_NOACTIVATE);
  end;
  Manager.FHintPos.x := -1; // v5.27
end;

constructor TsCustomHintWindow.Create(AOwner: TComponent);
begin
  inherited;
  dx := 0;
  dy := 0;
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

procedure TsCustomHintWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style and not WS_BORDER or WS_EX_TRANSPARENT;
end;

function TsCustomHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect;
var
  sHTML : TsHtml;
begin
  if HintFrame <> nil then begin
    Result := Rect(0, 0, HintFrame.Width, HintFrame.Height);
    Inc(Result.Right, iffi(Manager.HintKind.ShadowEnabled, Manager.HintKind.ShadowOffset, 0));
    Inc(Result.Bottom, iffi(Manager.HintKind.ShadowEnabled, Manager.HintKind.ShadowOffset, 0));
  end
  else if Assigned(Manager) then begin
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
    Result := Rect(0, 0, Manager.HintKind.FMaxWidth, 0);
{$IFNDEF ACHINTS}
    if Manager.Skinned then Manager.FCacheBmp.Canvas.Font.Assign(Screen.HintFont) else
{$ENDIF}
    Manager.FCacheBmp.Canvas.Font.Assign(Manager.HintKind.Font);
    if Manager.FHTMLMode then begin
      sHTML := TsHtml.Create;
      sHTML.Init(Manager.FCacheBmp, aHint, Result);
      Result := sHTML.HtmlText;
      FreeAndNil(sHTML);
    end
    else DrawText(Manager.FCacheBmp.Canvas.Handle, PChar(AHint), -1, Result,
      DT_CALCRECT or DT_CENTER or DT_VCENTER or DT_WORDBREAK or DT_NOPREFIX or DrawTextBiDiModeFlagsReadingOnly);
    if ((Manager.HintKind.Style = hsBalloon) or (Manager.HintKind.Style = hsEllipse)) and (WidthOf(Result) < 50) then Result.Right := Result.Left + 50;

    if Manager.Skinned then begin
      Inc(Result.Right, SkinMargin(0) + SkinMargin(2) + SkinBorderWidth * 2);
      Inc(Result.Bottom, SkinMargin(1) + SkinMargin(3) + SkinBorderWidth * 2);
    end
    else begin
      Inc(Result.Right, (Manager.HintKind.MarginH + Manager.HintKind.FBevelWidth) * 2);
      Inc(Result.Bottom, (Manager.HintKind.MarginV + Manager.HintKind.FBevelWidth) * 2);
    end;

    Inc(Result.Right, iffi(Manager.HintKind.ShadowEnabled, Manager.HintKind.ShadowOffset, 0));
    Inc(Result.Bottom, iffi(Manager.HintKind.ShadowEnabled, Manager.HintKind.ShadowOffset, 0));
  end;
end;

procedure TsCustomHintWindow.WMEraseBkGND(var Message: TWMPaint);
begin
  Message.Result := 1;
end;

procedure TsCustomHintWindow.WMNCPaint(var Message: TWMPaint);
begin
  if Assigned(Manager) then PrepareMask;
  Message.Result := 1;
end;

procedure TsCustomHintWindow.Paint;
begin
  if Assigned(Manager) then with Manager do begin
    if HintKind.ShadowEnabled then PaintShadow;
    FreeAndNil(BodyBmp);  { MemoryLeak : ONT }
//    BodyBmp := nil;
    BodyBmp := GetBody;
    try
      if not Assigned(MaskBmp) then PrepareMask;
      if Assigned(MaskBmp) and Assigned(BodyBmp) then SumByMask(FCacheBmp, BodyBmp, MaskBmp, ClientRect);
      BitBlt(Canvas.Handle, 0, 0, FCacheBmp.Width, FCacheBmp.Height, FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      if Assigned(MaskBmp) then FreeAndNil(MaskBmp);
      if Assigned(BodyBmp) then FreeAndNil(BodyBmp);
    end;
  end;
end;

procedure TsCustomHintWindow.PaintBG(Bmp: TBitmap; aRect: TRect);
var
  ci : TCacheInfo;
begin
  ci.Bmp := Manager.FCacheBmp; ci.X := 0; ci.Y := 0; ci.Ready := True;
  Manager.PaintBG(Bmp, aRect, ci);
end;

procedure TsCustomHintWindow.TextOut(Bmp: TBitmap);
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
//      MoveWindowOrg(Bmp.Canvas.Handle, R.Left, R.Top);
//      IntersectClipRect(Bmp.Canvas.Handle, 0, 0, HintFrame.Width, HintFrame.Height);
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
      InflateRect(R, - Manager.HintKind.MarginH - Manager.HintKind.BevelWidth - dx div 2,
                     - Manager.HintKind.MarginV - Manager.HintKind.BevelWidth - dy div 2);
    end;
    Bmp.Canvas.Brush.Style := bsClear;
    Bmp.Canvas.Pen.Style := psSolid;

  {$IFNDEF ACHINTS}
    if Manager.Skinned then Bmp.Canvas.Font.Assign(Screen.HintFont) else
  {$ENDIF}
    Bmp.Canvas.Font.Assign(Manager.HintKind.Font);
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

function TsCustomHintWindow.MainRect: TRect;
var
  ShadowOffset : integer;
begin
  ShadowOffset := iffi(Manager.HintKind.ShadowEnabled, Manager.HintKind.ShadowOffset, 0);
  Result.Left  := 0;
  Result.Right := Width - ShadowOffset;
  Result.Top  := 0;
  Result.Bottom := Height - ShadowOffset;
end;

procedure TsCustomHintWindow.PaintBorder(Bmp: TBitmap);
var
  R: TRect;
begin
  if Manager.HintKind.FBevelWidth > 0 then begin
    Bmp.Canvas.Pen.Style := psSolid;
    R := MainRect;
    inc(R.Left);
    inc(R.Top);
  end;
end;

function TsCustomHintWindow.ShadowTransparency: integer;
begin
  if acHintsInEditor then Result := Manager.HintKind.ShadowTransparency else begin
    Result := Manager.HintKind.Transparency * integer(Manager.HintKind.Transparency > 0);
    Result := SumTrans(Result, Manager.HintKind.ShadowTransparency);
  end
end;

procedure TsCustomHintWindow.WndProc(var Message: TMessage);
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
//    AC_GETCACHE : GlobalCacheInfo := MakeCacheInfo(BodyBmp);
    AC_CHILDCHANGED : Message.LParam := 1;
  end;
  inherited;
end;

destructor TsCustomHintWindow.Destroy;
begin
  FreeAndNil(ScreenBmp);  { No need to : ONT }
  FreeAndNil(AlphaBmp);   { MemoryLeak : ONT }
  FreeAndNil(MaskBmp);    { MemoryLeak : ONT }
  FreeAndNil(BodyBmp);    { MemoryLeak : ONT }
  inherited;                                        
end;

function TsCustomHintWindow.GetMousePosition: TPoint;
begin
  if Manager.FHintPos.x = -1 then Result := acMousePos else Result := Manager.FHintPos;
end;

procedure TsCustomHintWindow.PrepareMask;
begin
  rgn := 0;
  FreeAndNil(MaskBmp);  { MemoryLeak : ONT }
  MaskBmp := GetMask;
{$IFNDEF ACHINTS}
  if Assigned(MaskBmp) and Manager.Skinned then begin // Defining window region by MaskBmp
    GetRgnFromBmp(rgn, MaskBmp, clwhite);
    SetWindowRgn(Handle, rgn, False);
  end
  else SetWindowRgn(Handle, 0, False);
{$ENDIF}
end;

function TsCustomHintWindow.GetMask: TBitmap;
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

function TsCustomHintWindow.GetBody: TBitmap;
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
  if Assigned(ScreenBmp) then begin
    CI := MakeCacheInfo(ScreenBmp);
    PaintItemFast(SkinIndex, BorderIndex, BGIndex, BGIndex, '', CI, True, 0, Rect(0, 0, Width, Height), Point(0, 0), Result, DefaultManager)
  end
  else PaintItemFast(SkinIndex, BorderIndex, BGIndex, BGIndex, '', CI, True, 0, Rect(0, 0, Width, Height), Point(0, 0), Result, DefaultManager);
  TextOut(Result);
{$ENDIF}
end;

procedure TsCustomHintWindow.PaintShadow(Bmp : TBitmap = nil);
{$IFNDEF ACHINTS}
var
  tr: integer;
  R: TRect;
  ShadowOffset : integer;
  c : TsColor;
{$ENDIF}
begin
{$IFNDEF ACHINTS}
  if Manager.Skinned and acHintsInEditor then begin
    if Bmp = nil then Bmp := Manager.FCacheBmp;
    R := ClientRect;
    FillDC(Bmp.Canvas.Handle, R, clWhite);
    ShadowOffset := iffi(Manager.HintKind.ShadowEnabled, Manager.HintKind.ShadowOffset, 0);
    tr := ShadowTransparency;
    c.I := 0;
    // Painting
    FadeBmp(Bmp,
            Classes.Rect(R.Left + ShadowOffset,
            R.Top + ShadowOffset, R.Right, R.Bottom),
            tr, c, Manager.HintKind.ShadowBlur, 0);
  end;
{$ENDIF}
end;

function TsCustomHintWindow.SkinMargin(Border: byte): integer;
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

{ TsSimplyHintWindow }
function TsSimplyHintWindow.GetBody: TBitmap;
var
  R : TRect;
  ShadowOffset : integer;
begin
  if Manager.Skinned then Result := inherited GetBody else begin
    Result := CreateBmpLike(Manager.FCacheBmp);
    if HintFrame <> nil then BodyBmp := Result;
    R := ClientRect;
    ShadowOffset := iffi(Manager.HintKind.ShadowEnabled, Manager.HintKind.ShadowOffset, 0);
    PaintBG(Result, Classes.Rect(R.Left, R.Top, R.Right - ShadowOffset, R.Bottom - ShadowOffset));
    TextOut(Result);
    PaintBorder(Result);
  end;
end;

function TsSimplyHintWindow.GetMask: TBitmap;
var
  c : TsColor;
begin
  if Manager.Skinned then Result := inherited GetMask else begin
    Result := CreateBmpLike(Manager.FCacheBmp);
    Result.Canvas.Pen.Style := psClear;
    Result.Canvas.Brush.Color := clWhite;
    Result.Canvas.FillRect(Rect(0, 0, Width, Height));
    c.I := 0;
    // Painting
    Result.Canvas.Brush.Color := clBlack;
    Result.Canvas.FillRect(MainRect);
  end;
end;

function TsSimplyHintWindow.GetMousePosition: TPoint;
begin
  Result := inherited GetMousePosition;
  if not Manager.Skinned then inc(Result.y, 16);
end;

procedure TsSimplyHintWindow.PaintBorder(Bmp: TBitmap);
var
  R: TRect;
begin
  if Manager.Skinned then inherited else begin
    if Manager.HintKind.FBevelWidth > 0 then begin
      Bmp.Canvas.Pen.Style := psSolid;
      R := MainRect;
      Frame3d(Bmp.Canvas, R,
              ColorToRGB(Manager.HintKind.ColorBorderTop),
              ColorToRGB(Manager.HintKind.ColorBorderBottom), Manager.HintKind.FBevelWidth);
    end;
  end;
end;

procedure TsSimplyHintWindow.PaintShadow;
var
  tr: integer;
  R: TRect;
  ShadowOffset : integer;
  c : TsColor;
begin
  if Manager.Skinned then inherited else begin
    if Bmp = nil then Bmp := Manager.FCacheBmp;
    R := ClientRect;
    ShadowOffset := iffi(Manager.HintKind.ShadowEnabled, Manager.HintKind.ShadowOffset, 0);
    tr := ShadowTransparency;
    c.I := 0;
    // Painting
    FadeBmp(Bmp,
            Classes.Rect(R.Left + ShadowOffset,
            R.Top + ShadowOffset, R.Right, R.Bottom),
            tr, c, Manager.HintKind.ShadowBlur, 0);
  end;
end;

{ TsComicsHintWindow }
function TsComicsHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect;
begin
  Result := inherited CalcHintRect(MaxWidth, AHint, AData);
  if not Manager.Skinned then Inc(Result.Bottom, Manager.HintKind.ExOffset);
end;

function TsComicsHintWindow.GetArrowPosition: TPoint;
const
  Offs = 3;
begin
  case FMousePos of
    mpLeftTop: begin
      Result := Point(Offs, Offs);
    end;
    mpRightTop: begin
      Result := Point(Width - Offs, Offs);
    end;
    mpLeftBottom: begin
      Result := Point(Offs, Height - Offs);
    end;
    mpRightBottom: begin
      Result := Point(Width - Offs, Height - Offs);
    end;
  end;
end;

function TsComicsHintWindow.GetBody: TBitmap;
var
  R: TRect;
begin
  if Manager.Skinned then Result := inherited GetBody else begin
    Result := CreateBmpLike(Manager.FCacheBmp);
    if HintFrame <> nil then BodyBmp := Result;
    R := ClientRect;
    PaintBg(Result, ClientRect);
    TextOut(Result);
    PaintBorder(Result);
  end;
end;

function TsComicsHintWindow.GetMask: TBitmap;
var
  R: TRect;
  RValue, tr, i, xcenter : integer;
  pMouse : TPoint;
  delta : real;
  c : TsColor;
begin
  if Manager.Skinned then Result := inherited GetMask else begin
    Result := CreateBmpLike(Manager.FCacheBmp);

    Result.Canvas.Pen.Style := psClear;
    Result.Canvas.Brush.Color := clWhite;
    Result.Canvas.FillRect(ClientRect);

    R := MainRect;

    if not acLayered or acHintsInEditor
      then tr := Manager.HintKind.Transparency * integer(Manager.HintKind.Transparency > 0)
      else tr := 0;

    RValue := 255 * tr div 100;

    delta := 255 - RValue;

    // Prepare mask
    TColor(c) := clWhite;
    Result.Canvas.Pen.Style := psClear;
    Result.Canvas.Brush.Style := bsSolid;
    Result.Canvas.Brush.Color := clWhite;

    pMouse := GetArrowPosition;

//    k := 0;
    i := 0;

//    for i := 0 to 1 do begin
      c.R := max(0, min(255, RValue + Round(delta * ( - 1))));
      c.G := c.R;
      c.B := c.R;
      Result.Canvas.Brush.Color := TColor(c);

      Result.Canvas.RoundRect(R.Left + i,
                               R.Top + i,
                               R.Right - i + 1,
                               R.Bottom - i + 1,
                               Manager.HintKind.Radius,
                               Manager.HintKind.Radius);

      xcenter := R.Left + WidthOf(R) div 2;// + Manager.Blur;
      tr := WidthOf(R) div 8;// - Round(i * k * 2);
      if tr < 0 then tr := 0;
      case FMousePos of
        mpLeftTop, mpRightTop : begin
//          k := (R.Top + HeightOf(R) / 2) / (R.Top + 1);
          Result.Canvas.Polygon([pMouse,
                           Point(xcenter - tr, R.Top + HeightOf(R) div 2),
                           Point(xcenter + tr, R.Top + HeightOf(R) div 2),
                           pMouse
                          ]);
        end
        else begin
//          k := (Height - (R.Bottom - HeightOf(R) / 2)) / (Height - R.Bottom + 1);
          Result.Canvas.Polygon([pMouse,
                           Point(xcenter - tr, R.Bottom - HeightOf(R) div 2),
                           Point(xcenter + tr, R.Bottom - HeightOf(R) div 2)
                          ]);
        end;
      end;
//    end;
  end;
end;

function TsComicsHintWindow.MainRect: TRect;
var
  ShadowOffset : integer;
begin
  if Manager.Skinned then Result := inherited MainRect else begin
    ShadowOffset := iffi(Manager.HintKind.ShadowEnabled, Manager.HintKind.ShadowOffset, 0);
    Result.Left  := 0;
    Result.Right := Width - ShadowOffset;
    case FMousePos of
      mpLeftTop, mpRightTop: begin
        Result.Top := Manager.HintKind.ExOffset;
        Result.Bottom := Height - ShadowOffset;
      end;
      mpLeftBottom, mpRightBottom: begin
        Result.Top := 0;
        Result.Bottom := Height - ShadowOffset - Manager.HintKind.ExOffset;
      end;
    end;
  end;
end;

procedure TsComicsHintWindow.PaintBorder(Bmp: TBitmap);
var
  MskBmp : TBitmap;
begin
  if Manager.Skinned then inherited else begin
    if Manager.HintKind.FBevelWidth > 0 then begin
      MskBmp := GetMask;
      try
        Bmp.Canvas.Pen.Style := psSolid;
        BorderByMask(Bmp, MskBmp, TsColor(ColorToRGB(Manager.HintKind.ColorBorderTop)), TsColor(ColorToRGB(Manager.HintKind.ColorBorderBottom)));
      finally
        if Assigned(MskBmp) then FreeAndNil(MskBmp);
      end;
    end;
  end;
end;

procedure TsComicsHintWindow.PaintShadow;
var
  R: TRect;
  {ShadowOffset,} RValue, tr, i, xcenter : integer;
  pMouse : TPoint;
  MaskBmp, TempBmp : TBitmap;
  delta, k: real;
  c : TsColor;
begin
  if Manager.Skinned then inherited else begin
    if Bmp = nil then Bmp := Manager.FCacheBmp;
    MaskBmp := CreateBmpLike(Bmp);
    TempBmp := CreateBmpLike(Bmp);

    try
    MaskBmp.Canvas.Pen.Style := psClear;
    MaskBmp.Canvas.Brush.Color := clWhite;
    MaskBmp.Canvas.FillRect(ClientRect);

    R := MainRect;
    OffsetRect(R, Manager.HintKind.ShadowOffset, Manager.HintKind.ShadowOffset);

    tr := ShadowTransparency;

    RValue := 255 * tr div 100;

    bitBlt(TempBmp.Canvas.Handle, 0, 0, TempBmp.Width, TempBmp.Height, Bmp.Canvas.Handle, 0, 0, SrcCopy);

    delta := (255 - RValue) / (Manager.HintKind.ShadowBlur + 1);

    // Prepare mask
    TColor(c) := clWhite;
    MaskBmp.Canvas.Pen.Style := psClear;
    MaskBmp.Canvas.Brush.Style := bsSolid;
    MaskBmp.Canvas.Brush.Color := clWhite;

    pMouse := GetArrowPosition;

    k := 0;

    for i := 0 to Manager.HintKind.ShadowBlur do begin
      c.R := max(0, min(255, RValue + Round(delta * (Manager.HintKind.ShadowBlur - i))));
      c.G := c.R;
      c.B := c.R;
      MaskBmp.Canvas.Brush.Color := TColor(c);

      MaskBmp.Canvas.RoundRect(R.Left + i,
                               R.Top + i,
                               R.Right - i + 1,
                               R.Bottom - i + 1,
                               Manager.HintKind.Radius,
                               Manager.HintKind.Radius);

      xcenter := R.Left + WidthOf(R) div 2 + Manager.HintKind.ShadowBlur;
      tr := WidthOf(R) div 8 - Round(i * k * 2);
      if tr < 0 then tr := 0;
      case FMousePos of
        mpLeftTop, mpRightTop : begin
          k := (R.Top + HeightOf(R) / 2) / (R.Top + 1);
          MaskBmp.Canvas.Polygon([pMouse,
                           Point(xcenter - tr, R.Top + HeightOf(R) div 2),
                           Point(xcenter + tr, R.Top + HeightOf(R) div 2),
                           pMouse
                          ]);
        end
        else begin
          k := (Height - (R.Bottom - HeightOf(R) / 2)) / (Height - R.Bottom + 1);
          MaskBmp.Canvas.Polygon([pMouse,
                           Point(xcenter - tr, R.Bottom - HeightOf(R) div 2),
                           Point(xcenter + tr, R.Bottom - HeightOf(R) div 2)
                          ]);
        end;
      end;
    end;
    c.I := 0;
    BlendBmpByMask(TempBmp, MaskBmp, c);

    // Copy back
    BitBlt(Bmp.Canvas.Handle, 0, 0, TempBmp.Width, MaskBmp.Height, TempBmp.Canvas.Handle, 0, 0, SRCCOPY);

    finally
    if Assigned(MaskBmp) then FreeAndNil(MaskBmp);
    if Assigned(Tempbmp) then FreeAndNil(TempBmp);
    end;
  end;
end;

{ TsEllipseHintWindow }

function TsEllipseHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect;
var
  s, l : real;
begin
  Result := inherited CalcHintRect(MaxWidth, AHint, AData);
  if not Manager.Skinned then begin
    Inc(Result.Bottom, Manager.HintKind.ExOffset);
    if (HintFrame <> nil) then Exit;
    l := Sqrt(Sqr(WidthOf(Result)) + Sqr(HeightOf(Result)));
    s := l - WidthOf(Result);
    dx := Round(s * WidthOf(Result) / HeightOf(Result));
    dy := Round(s * HeightOf(Result) / WidthOf(Result));
    Inc(Result.Right, dx);
    Inc(Result.Bottom, dy);
  end;
end;

function TsEllipseHintWindow.GetArrowPosition: TPoint;
const
  Offs = 3;
begin
  case FMousePos of
    mpLeftTop: begin
      Result := Point(Offs, Offs);
    end;
    mpRightTop: begin
      Result := Point(Width - Offs, Offs);
    end;
    mpLeftBottom: begin
      Result := Point(Offs, Height - Offs);
    end;
    mpRightBottom: begin
      Result := Point(Width - Offs, Height - Offs);
    end;
  end;
end;

function TsEllipseHintWindow.GetBody: TBitmap;
var
  R: TRect;
begin
  if Manager.Skinned then Result := inherited GetBody else begin
    Result := CreateBmpLike(Manager.FCacheBmp);
    if HintFrame <> nil then BodyBmp := Result;
    R := ClientRect;
    PaintBg(Result, ClientRect);
    TextOut(Result);
    PaintBorder(Result);
  end;
end;

function TsEllipseHintWindow.GetMask: TBitmap;
var
  R: TRect;
  RValue, tr, i, xcenter : integer;
  pMouse : TPoint;
  delta: real;
  c : TsColor;
begin
  if Manager.Skinned then Result := inherited GetMask else begin
    Result := CreateBmpLike(Manager.FCacheBmp);
    Result.Canvas.Pen.Style := psClear;
    Result.Canvas.Brush.Color := clWhite;
    Result.Canvas.FillRect(ClientRect);

    R := MainRect;

    if not acLayered or acHintsInEditor
      then tr := Manager.HintKind.Transparency * integer(Manager.HintKind.Transparency > 0)
      else tr := 0;

    RValue := 255 * tr div 100;

    delta := 255 - RValue;

    // Prepare mask
    TColor(c) := clWhite;
    Result.Canvas.Pen.Style := psClear;
    Result.Canvas.Brush.Style := bsSolid;
    Result.Canvas.Brush.Color := clWhite;

    pMouse := GetArrowPosition;

    i := 0;

      c.R := max(0, min(255, RValue - Round(delta)));
      c.G := c.R;
      c.B := c.R;
      Result.Canvas.Brush.Color := TColor(c);

      Result.Canvas.Ellipse(R.Left + i,
                             R.Top + i,
                             R.Right - i + 1,
                             R.Bottom - i + 1);

      xcenter := R.Left + WidthOf(R) div 2;// + Manager.Blur;
      tr := WidthOf(R) div 8;
      if tr < 0 then tr := 0;
      case FMousePos of
        mpLeftTop, mpRightTop : begin
          Result.Canvas.Polygon([pMouse,
                           Point(xcenter - tr, R.Top + HeightOf(R) div 2),
                           Point(xcenter + tr, R.Top + HeightOf(R) div 2),
                           pMouse
                          ]);
        end
        else begin
          Result.Canvas.Polygon([pMouse,
                           Point(xcenter - tr, R.Bottom - HeightOf(R) div 2),
                           Point(xcenter + tr, R.Bottom - HeightOf(R) div 2)
                          ]);
        end;
      end;
  end;
end;

function TsEllipseHintWindow.MainRect: TRect;
var
  ShadowOffset : integer;
begin
  if Manager.Skinned then Result := inherited MainRect else begin
    ShadowOffset := iffi(Manager.HintKind.ShadowEnabled, Manager.HintKind.ShadowOffset, 0);
    Result.Left  := 0;
    Result.Right := Width - ShadowOffset;
    case FMousePos of
      mpLeftTop, mpRightTop: begin
        Result.Top := Manager.HintKind.ExOffset;
        Result.Bottom := Height - ShadowOffset;
      end;
      mpLeftBottom, mpRightBottom: begin
        Result.Top := 0;
        Result.Bottom := Height - ShadowOffset - Manager.HintKind.ExOffset;
      end;
    end;
  end;
  if Result.Top < 0 then Result.Top := 0;
  if Result.Bottom > Height then Result.Bottom := Height;
end;

procedure TsEllipseHintWindow.PaintBorder(Bmp: TBitmap);
var
  MskBmp : TBitmap;
begin
  if Manager.Skinned then inherited else begin
    if Manager.HintKind.FBevelWidth > 0 then begin
      MskBmp := GetMask;
      try
        Bmp.Canvas.Pen.Style := psSolid;
        BorderByMask(Bmp, MskBmp, TsColor(ColorToRGB(Manager.HintKind.ColorBorderTop)), TsColor(ColorToRGB(Manager.HintKind.ColorBorderBottom)));
      finally
        if Assigned(MskBmp) then FreeAndNil(MskBmp);
      end;
    end;
  end;
end;

procedure TsEllipseHintWindow.PaintShadow;
var
  R: TRect;
  RValue, tr, i, xcenter : integer;
  pMouse : TPoint;
  MaskBmp, TempBmp : TBitmap;
  delta, k: real;
  c : TsColor;
begin
  if Manager.Skinned then inherited else begin
    if Bmp = nil then Bmp := Manager.FCacheBmp;
    MaskBmp := CreateBmpLike(Bmp);
    TempBmp := CreateBmpLike(Bmp);

    try

    MaskBmp.Canvas.Pen.Style := psClear;
    MaskBmp.Canvas.Brush.Color := clWhite;
    MaskBmp.Canvas.FillRect(ClientRect);

    R := MainRect;
    OffsetRect(R, Manager.HintKind.ShadowOffset, Manager.HintKind.ShadowOffset);

    tr := ShadowTransparency;

    RValue := 255 * tr div 100;

    bitBlt(TempBmp.Canvas.Handle, 0, 0, TempBmp.Width, TempBmp.Height, Bmp.Canvas.Handle, 0, 0, SrcCopy);

    delta := (255 - RValue) / (Manager.HintKind.ShadowBlur + 1);

    // Prepare mask
    TColor(c) := clWhite;
    MaskBmp.Canvas.Pen.Style := psClear;
    MaskBmp.Canvas.Brush.Style := bsSolid;
    MaskBmp.Canvas.Brush.Color := clWhite;

    pMouse := GetArrowPosition;

    k := 0;

    for i := 0 to Manager.HintKind.ShadowBlur do begin
      c.R := max(0, min(255, RValue + Round(delta * (Manager.HintKind.ShadowBlur - i))));
      c.G := c.R;
      c.B := c.R;
      MaskBmp.Canvas.Brush.Color := TColor(c);

      MaskBmp.Canvas.Ellipse(R.Left + i,
                             R.Top + i,
                             R.Right - i + 1,
                             R.Bottom - i + 1);

      xcenter := R.Left + WidthOf(R) div 2 + Manager.HintKind.ShadowBlur;
      tr := WidthOf(R) div 8 - Round(i * k * 2);
      if tr < 0 then tr := 0;
      case FMousePos of
        mpLeftTop, mpRightTop : begin
          k := (R.Top + HeightOf(R) / 2) / (R.Top + 1);
          MaskBmp.Canvas.Polygon([pMouse,
                           Point(xcenter - tr, R.Top + HeightOf(R) div 2),
                           Point(xcenter + tr, R.Top + HeightOf(R) div 2),
                           pMouse
                          ]);
        end
        else begin
          k := (Height - (R.Bottom - HeightOf(R) / 2)) / (Height - R.Bottom + 1);
          MaskBmp.Canvas.Polygon([pMouse,
                           Point(xcenter - tr, R.Bottom - HeightOf(R) div 2),
                           Point(xcenter + tr, R.Bottom - HeightOf(R) div 2)
                          ]);
        end;
      end;
    end;
    c.I := 0;
    BlendBmpByMask(TempBmp, MaskBmp, c);
    // Copy back
    BitBlt(Bmp.Canvas.Handle, 0, 0, TempBmp.Width, MaskBmp.Height, TempBmp.Canvas.Handle, 0, 0, SRCCOPY);

    finally
      if Assigned(Maskbmp) then FreeAndNil(MaskBmp);
      if Assigned(TempBmp) then FreeAndNil(TempBmp);
    end;
  end;
end;

{ TsBalloonHintWindow }

function TsBalloonHintWindow.CalcHintRect(MaxWidth: Integer;
  const AHint: string; AData: Pointer): TRect;
var
  s, {z,} l : real;
begin
  if not Assigned(Manager) then Exit;
  Result := inherited CalcHintRect(MaxWidth, AHint, AData);
  if not Manager.Skinned then begin
    Inc(Result.Bottom, Manager.HintKind.ExOffset);
    l := Sqrt(Sqr(WidthOf(Result)) + Sqr(HeightOf(Result)));
    s := l - WidthOf(Result);
    dx := Round(s * WidthOf(Result) / HeightOf(Result));
    dy := Round(s * HeightOf(Result) / WidthOf(Result));
    Inc(Result.Right, dx);
    Inc(Result.Bottom, dy);
  end;
end;

constructor TsBalloonHintWindow.Create(AOwner: TComponent);
begin
  inherited;
  BalloonCount := 5;
  Divid := 4;
end;

function TsBalloonHintWindow.GetArrowPosition: TPoint;
const
  Offs = 3;
begin
  case FMousePos of
    mpLeftTop: Result := Point(Offs, Offs);
    mpRightTop: Result := Point(Width - Offs, Offs);
    mpLeftBottom: Result := Point(Offs, Height - Offs);
    mpRightBottom: Result := Point(Width - Offs, Height - Offs);
  end;
end;

function TsBalloonHintWindow.GetBody: TBitmap;
var
  R: TRect;
begin
  if Manager.Skinned then Result := inherited GetBody else begin
    Result := CreateBmpLike(Manager.FCacheBmp);
    if HintFrame <> nil then BodyBmp := Result;
    R := ClientRect;
    PaintBg(Result, ClientRect);
    TextOut(Result);
    PaintBorder(Result);
  end;
end;

function TsBalloonHintWindow.GetMask: TBitmap;
begin
  if Manager.Skinned then Result := inherited GetMask else Result := GetCustMask(False);
end;

function TsBalloonHintWindow.GetCustMask(Shadow: boolean): TBitmap;
var
  R, {mRect,} bRect : TRect;
  RValue, tr, bl, xcenter, w, h : integer;
  pMouse{, bCenter} : TPoint;
  delta: real;
  c : TsColor;
  bSize : TSize;
  j, deltaY, deltaX : integer;
  Blur : integer;
begin
  Result := CreateBmpLike(Manager.FCacheBmp);
  FillDC(Result.Canvas.Handle, ClientRect, clWhite);

  R := MainRect;
  w := WidthOf(R);
  h := HeightOf(R);
  xcenter := R.Left + w div 2;
  if Shadow then begin
    tr := ShadowTransparency;
    blur := Manager.HintKind.ShadowBlur;
  end
  else begin
    if not acLayered or acHintsInEditor
      then tr := Manager.HintKind.Transparency * integer(Manager.HintKind.Transparency > 0)
      else tr := 0;
    blur := 0;
  end;
  pMouse := GetArrowPosition;
  RValue := (tr shl 8) div 100;
  delta := (255 - RValue) / (Blur + 1);

  Result.Canvas.Pen.Style := psClear;
  for bl := 0 to Blur do begin
    c.R := max(0, min(255, RValue + Round(delta * (Blur - bl)))); c.G := c.R; c.B := c.R;
    Result.Canvas.Brush.Color := c.C;
    Result.Canvas.Ellipse(R.Left + bl, R.Top + bl, R.Right - bl + 1, R.Bottom - bl + 1);

    // Arrow painting
    bSize.cx := w div Divid;
    bSize.cy := h div Divid;

    case FMousePos of
      mpLeftTop: begin
        DeltaX := (w div 2 - bSize.cx) div (BalloonCount + 1);
        DeltaY := (R.Top + h div 6 - bSize.cy) div (BalloonCount + 1);
        bRect.Left := xcenter - bSize.cx;
        bRect.Top := R.Top + h div 6 - bSize.cy;
      end;
      mpRightTop: begin
        DeltaX := - ((w + Manager.HintKind.ShadowOffset) div 2 - bSize.cx) div (BalloonCount) * 2;
        DeltaY := (R.Top + h div 6 - bSize.cy) div (BalloonCount + 1);
        bRect.Left := xcenter + DeltaX;
        bRect.Top := R.Top + h div 6 - bSize.cy;
      end;
      mpLeftBottom: begin
        DeltaX := (w div 2 - bSize.cx) div (BalloonCount - 1);
        DeltaY := - (Height - R.Bottom {+ h div 6 - bSize.cy}) div (BalloonCount - 1);
        bRect.Left := xcenter - DeltaX div 2;// div 2;//bSize.cx;
        bRect.Top := R.Bottom {+ h div 6} {- bSize.cy} + DeltaY + DeltaY div 2;
      end
      else {mpRightBottom} begin
        DeltaX := - (Width - xcenter + Manager.HintKind.ShadowOffset - bSize.cx div 2) div (BalloonCount - 1);
        DeltaY := - (R.Bottom + Manager.HintKind.ShadowOffset + h div 6 - bSize.cy) div (BalloonCount - 1);
        bRect.Left := xcenter + DeltaX div 2;
        bRect.Top := R.Bottom {+ h div 6} {- bSize.cy} + DeltaY + DeltaY div 2;
      end;
    end;
    for j := 1 to BalloonCount do begin
      bSize.cx := w div (Divid * j);
      bSize.cy := h div (Divid * j);

      bRect.Left := bRect.Left - DeltaX;
      bRect.Top := bRect.top - DeltaY;
      bRect.Right := bRect.Left + bSize.cx;
      bRect.Bottom := bRect.Top + bSize.cy;
      if (bRect.Bottom > bRect.Top + bl * 2) and (bRect.Right > bRect.Left + bl * 2) then
        Result.Canvas.Ellipse(bRect.Left + bl, bRect.Top + bl, bRect.Right - bl + 1, bRect.Bottom - bl + 1);
    end;
  end;
end;

function TsBalloonHintWindow.MainRect: TRect;
var
  ShadowOffset : integer;
begin
  if Manager.Skinned then Result := inherited MainRect else begin
    ShadowOffset := iffi(Manager.HintKind.ShadowEnabled, Manager.HintKind.ShadowOffset, 0);
    Result.Left  := 0;
    Result.Right := Width - ShadowOffset;
    case FMousePos of
      mpLeftTop, mpRightTop: begin
        Result.Top := Manager.HintKind.ExOffset;
        Result.Bottom := Height - ShadowOffset;
      end;
      mpLeftBottom, mpRightBottom: begin
        Result.Top := 0;
        Result.Bottom := Height - ShadowOffset - Manager.HintKind.ExOffset;
      end;
    end;
  end;
end;

procedure TsBalloonHintWindow.PaintBorder(Bmp: TBitmap);
var
  MskBmp : TBitmap;
begin
  if Manager.Skinned then inherited else begin
    if Manager.HintKind.FBevelWidth > 0 then begin
      MskBmp := GetMask;
      Bmp.Canvas.Pen.Style := psSolid;
      BorderByMask(Bmp, MskBmp, TsColor(ColorToRGB(Manager.HintKind.ColorBorderTop)), TsColor(ColorToRGB(Manager.HintKind.ColorBorderBottom)));
      FreeAndNil(MskBmp);
    end;
  end;
end;

procedure TsBalloonHintWindow.PaintShadow;
var
  R : TRect;
  MaskBmp, TempBmp, BodyBmp : TBitmap;
  c : TsColor;
begin
  if Manager.Skinned then inherited else begin
    if Bmp = nil then Bmp := Manager.FCacheBmp;
    MaskBmp := CreateBmpLike(Bmp);
    BodyBmp := GetCustMask(True);
    TempBmp := CreateBmpLike(Bmp);
    R := ClientRect;
    FillDC(MaskBmp.Canvas.Handle, R, clWhite);

    inc(R.Left, Manager.HintKind.ShadowOffset);
    inc(R.Top, Manager.HintKind.ShadowOffset);
    StretchBlt(MaskBmp.Canvas.Handle, R.Left, R.Top, WidthOf(R), HeightOf(R),
               BodyBmp.Canvas.Handle, 0, 0, Width - Manager.HintKind.ShadowOffset, Height - Manager.HintKind.ShadowOffset, srcCopy);
    c.I := 0;
    BlendBmpByMask(Bmp, MaskBmp, c);
    FreeAndNil(MaskBmp);
    FreeAndNil(TempBmp);
    FreeAndNil(BodyBmp);
  end;
end;

{ TsHintKind }

constructor TsHintKind.Create(AOwner: TsHintManager);
begin
  FBlur := 0;
  FShadowColor := clBlack;
  FOwner := AOwner;
  FFont := TFont.Create;
  FTexture := TPicture.Create;
  FRadius := 20;
  FBevelWidth := 1;
  FExOffset := 32;
  FMarginH := 15;
  FMarginV := 10;
  FMaxWidth := 200;
  FStyle := DefStyle;
  FGradientPercent := 0;
  FTexturePercent := 0;
end;

destructor TsHintKind.Destroy;
begin
  FreeAndNil(FFont);
  FreeAndNil(FTexture);
  inherited Destroy;
end;

function TsHintKind.GetShadowBlur: integer;
begin
  Result := FShadowBlur;
end;

function TsHintKind.GetShadowEnabled: boolean;
begin
  Result := FShadowEnabled and not FOwner.Skinned;
end;

function TsHintKind.GetShadowOffset: integer;
begin
  Result := FShadowOffset;
end;

function TsHintKind.GetShadowTransparency: integer;
begin
  Result := FShadowTransparency;
end;

procedure TsHintKind.SetBlur(const Value: integer); begin end;

procedure TsHintKind.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TsHintKind.SetShadowColor(const Value: TColor); begin end;

procedure TsHintKind.SetStyle(const Value: TsHintStyle);
begin
  FStyle := Value;
  FOwner.SetNewStyle(Value);
end;

procedure TsHintKind.SetTexture(const Value: TPicture);
begin
  FTexture.Assign(Value);
end;

end.



