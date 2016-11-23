unit sGraphUtils;
{$I sDefs.inc}
{$DEFINE DEBAGASM8} // Improving of blending
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  {$IFDEF TNTUNICODE}TntGraphics, {$ENDIF}
  ComCtrls, sConst, ExtCtrls, Jpeg, acntUtils, math, Buttons{$IFDEF LOGGED}, sDebugMsgs{$ENDIF}
  {$IFNDEF ACHINTS}, imglist, sMaskData, sCommonData{$ENDIF};

{$IFNDEF NOTFORHELP}
type
  TsHSV = record h : integer; s : real; v : real end;

{$ENDIF} // NOTFORHELP

function SwapColor(i : integer) : integer;
function ColorToSColor(Color : TColor) : TsColor;
// Paint tiled TGraphic on bitmap
procedure TileBitmap(Canvas: TCanvas; aRect: TRect; Graphic: TGraphic); overload;

{$IFNDEF ACHINTS}
procedure RGBToHSV (const R, G, B: Real; var H, S, V: Real);
procedure HSVtoRGB (const H,S,V: Real; var R,G,B: real);
procedure CopyImage(Glyph : TBitmap; ImageList: TCustomImageList; Index: Integer);
procedure PaintItemBG(SkinData : TsCommonData; ci : TCacheInfo; State : integer; R : TRect; pP : TPoint; ItemBmp : TBitmap; OffsetX : integer = 0; OffsetY : integer = 0); overload;
procedure PaintItem(SkinData : TsCommonData; ci : TCacheInfo; Filling : boolean; State : integer; R : TRect; pP : TPoint; ItemBmp : TBitmap; UpdateCorners : boolean; OffsetX : integer = 0; OffsetY : integer = 0); overload;
function PaintSection(const Bmp : TBitmap; Section : string; const SecondSection : string; const State : integer; const Manager : TObject; const ParentOffset : TPoint; const BGColor : TColor; ParentDC : hdc = 0) : integer;

procedure PaintControlByTemplate(const DstBmp, SrcBmp : TBitmap; const DstRect, SrcRect, BorderWidths : TRect; const DrawModes : TRect; const StretchCenter : boolean);//TacBorderDrawModes);
procedure PaintSkinControl(const SkinData : TsCommonData; const Parent : TControl; const Filling : boolean; const State : integer; const R : TRect; const pP : TPoint; const ItemBmp : TBitmap; const UpdateCorners : boolean; const OffsetX : integer = 0; const OffsetY : integer = 0);
procedure CopyChannel32(const DstBmp, SrcBmp : TBitmap; const Channel : integer);
procedure CopyChannel(const Bmp32, Bmp8 : TBitmap; const Channel : integer; const From32To8 : boolean);

procedure DrawGlyphEx(Glyph, DstBmp : TBitmap; R : TRect; NumGlyphs : integer; Enabled : boolean; DisabledGlyphKind : TsDisabledGlyphKind; State, Blend : integer; Down : boolean = False);
{$ENDIF}
// Fills rectangle on device context by Color
procedure FillDC(const DC: HDC; const aRect: TRect; const Color: TColor);
procedure FillDCBorder(const DC: HDC; const aRect: TRect; const wl, wt, wr, wb : integer; const Color: TColor);
procedure BitBltBorder(const DestDC: HDC; const X, Y, Width, Height: Integer; const SrcDC: HDC; const XSrc, YSrc: Integer; const BorderWidth : integer);
procedure ExcludeControls(const DC : hdc; const Ctrl : TWinControl; const CtrlType : TacCtrlType; const OffsetX : integer; const OffsetY : integer);
// Grayscale bitmap
procedure GrayScale(Bmp: TBitmap);
procedure GrayScaleTrans(Bmp: TBitmap; const TransColor : TsColor);
// Function CutText get text with ellipsis if no enough place
function CutText(Canvas: TCanvas; const Text: string; MaxLength : integer): string;
// Writes text on Canvas on custom rectangle by Flags
procedure WriteText(Canvas: TCanvas; Text: PChar; Enabled: boolean; var aRect : TRect; Flags: Cardinal);
procedure SumBitmaps(SrcBmp, MskBmp: Graphics.TBitMap; Color : TsColor);
procedure SumBmpRect(const DstBmp, SrcBmp: Graphics.TBitMap; const Color : TsColor; SrcRect : TRect; DstPoint : TPoint);
// Alpha-blending of rectangle on bitmap by Blend, excluding pixels with color TransColor
// if TransColor.A = MaxByte then TransColor is not used
procedure BlendTransRectangle(Dst: TBitmap; X, Y: integer; Src: TBitmap; aRect: TRect; Blend: real; TransColor: TsColor);
procedure BlendTransBitmap(Bmp: TBitmap; Blend: real; Color, TransColor: TsColor);
// Alpha-blending of rectangle on bitmap custom transparency, color, blur and radius
procedure FadeBmp(FadedBmp: TBitMap; aRect: TRect;Transparency: integer; Color: TsColor; Blur, Radius : integer);
// Copying alpha-blended rectangle from CanvasSrc to CanvasDst
procedure FadeRect(CanvasSrc: TCanvas; RSrc: TRect; CanvasDst: HDC; PDst: TPoint; Transparency: integer; Color: TColor; Blur : integer; Shape: TsShadowingShape); overload;
procedure FadeRect(CanvasSrc: TCanvas; RSrc: TRect; CanvasDst: HDC; PDst: TPoint; Transparency: integer; Color: TColor; Blur : integer; Shape: TsShadowingShape; Radius : integer); overload;
// Sum two bitmaps where Color used as mask
procedure BlendBmpByMask(SrcBmp, MskBmp: Graphics.TBitMap; BlendColor : TsColor);
// Copying rectangle from SrcBmp to DstBmp
procedure CopyRect(DstBmp, SrcBmp: Graphics.TBitMap; X, Y : integer; aRect: TRect; TransColor : TColor);{$IFDEF WARN_DEPRECATED} deprecated; {$ENDIF}
// Copying bitmap SrcBmp to DstBmp, excluding pixels with color TransColor
procedure CopyTransBitmaps(DstBmp, SrcBmp: Graphics.TBitMap; X, Y : integer; TransColor : TsColor);
// Sum two bitmaps by mask MskBmp
procedure SumByMaskWith32(const Src1, Src2, MskBmp: Graphics.TBitMap; const aRect: TRect);
procedure SumByMask(var Src1, Src2, MskBmp: Graphics.TBitMap; aRect: TRect);
// Fills bitmap by custom properties of Gradient
procedure GradientBmp(Bmp: Graphics.TBitMap; aRect : TRect; Color1, Color2 : TsColor; Layout : TGradientTypes; Percent1, Percent2 : TPercent; Width : integer);{$IFDEF WARN_DEPRECATED} deprecated; {$ENDIF}
function MakeRotated90(const Bmp : TBitmap; CW : boolean; KillSource : boolean = True) : TBitmap;
// Returns color as ColorBegin -  (ColorBegin - ColorEnd) * i
function ChangeColor(ColorBegin, ColorEnd : TColor; i : real) : TColor;
// Returns color as (ColorBegin + ColorEnd) / 2
function AverageColor(ColorBegin, ColorEnd : TsColor) : TsColor; overload;
function AverageColor(ColorBegin, ColorEnd : TColor) : TColor; overload;
function MixColors(Color1, Color2 : TColor; PercentOfColor1 : real) : TColor;
// Draws rectangle on device context
procedure DrawRectangleOnDC(DC: HDC; var R: TRect; ColorTop, ColorBottom: TColor; var Width: integer);
procedure CalcButtonLayout(const Client: TRect; const GlyphSize: TPoint; const TextRectSize: TSize; Layout: TButtonLayout;
            Alignment: TAlignment; Margin, Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect; BiDiFlags: LongInt);      {RL}
// Returns height of font
function GetFontHeight(hFont : HWnd): integer;
// Returns width of text
function GetStringSize(hFont : hgdiobj; const Text : acString): TSize;
// Loads to Image TJpegImage or TBitmap from FileName
function LoadJpegOrBmp(Image: TPicture; const FileName: string; Gray: boolean):boolean;
procedure FocusRect(Canvas : TCanvas; R : TRect);

{$IFNDEF NOTFORHELP}
{$IFNDEF ACHINTS}
procedure TileBitmap(Canvas: TCanvas; var aRect: TRect; Graphic: TGraphic; MaskData : TsMaskData; FillMode : TacFillMode = fmTiled); overload;
procedure TileBitmapFromOther(Canvas: TCanvas; aRect: TRect; MaskData : TsMaskData); {$IFDEF WARN_DEPRECATED} deprecated; {$ENDIF}
procedure TileMasked(Bmp: TBitmap; var aRect: TRect; CI : TCacheInfo; MaskData : TsMaskData; FillMode : TacFillMode = fmDisTiled);
procedure AddRgn(var AOR : TAOR; Width : integer; MaskData : TsMaskData; VertOffset : integer; Bottom : boolean);
function GetRgnForMask(MaskIndex, Width, Height : integer; SkinManager : TObject) : hrgn;
procedure GetRgnFromBmp(var rgn : hrgn; MaskBmp : TBitmap; TransColor : TColor);
function GetBGInfo(const BGInfo : PacBGInfo; const Handle : THandle; PleaseDraw : boolean = False) : boolean; overload;
function GetBGInfo(const BGInfo : PacBGInfo; const Control : TControl; PleaseDraw : boolean = False) : boolean; overload;
function BGInfoToCI(const BGInfo : PacBGInfo) : TCacheInfo;
procedure AddRgnBmp(var AOR : TAOR; MaskBmp : TBitmap; TransColor : TsColor);
{$ENDIF}
procedure SumBitmapsByMask(var ResultBmp, Src1, Src2: Graphics.TBitMap; MskBmp: Graphics.TBitMap; Percent : word = 0);
// Copy Bmp with AlphaMask if Bmp2 is not MasterBitmap
procedure CopyByMask(R1, R2 : TRect; const Bmp1, Bmp2 : TBitmap; const CI : TCacheInfo; const UpdateTrans : boolean); overload;
procedure CopyByMask(R1, R2 : TRect; const Bmp1, Bmp2 : TBitmap; const CI : TCacheInfo; const UpdateTrans : boolean; const MaskData : TsMaskData); overload;
procedure CopyBmp32(R1, R2 : TRect; const Bmp1, Bmp2 : TBitmap; const CI : TCacheInfo; const UpdateTrans : boolean; const GrayedColor : TColor; const Blend : integer; const Reflected : boolean);
//procedure acCopyByMask(const R1, R2 : PRect; const Bmp1, Bmp2 : TBitmap; const CI : TCacheInfo; const UpdateTrans : boolean; const MaskData : TsMaskData);
// Copying rectangle from SrcBmp to DstBmp, excluding pixels with color TransColor (get trans pixels from parent)
procedure CopyTransRect(DstBmp, SrcBmp: Graphics.TBitMap; X, Y : integer; SrcRect: TRect; TransColor : TColor; CI : TCacheInfo; UpdateTrans : boolean);
// Skip transarent part
procedure CopyTransRectA(DstBmp, SrcBmp: Graphics.TBitMap; X, Y : integer; SrcRect: TRect; TransColor : TColor; CI : TCacheInfo);
// Creates bitmap like Bmp
function CreateBmpLike(const Bmp: TBitmap): TBitmap;
function CreateBmp24(const Width, Height : integer) : TBitmap;
function CreateBmp32(const Width, Height : integer) : TBitmap;
procedure InitCI(var CI : TCacheInfo; Bmp : TBitmap; X : integer = 0; y : integer = 0);{$IFDEF WARN_DEPRECATED} deprecated; {$ENDIF}
procedure WriteTextOnDC(DC: hdc; Text: PChar; Enabled: boolean; var aRect : TRect; Flags: Cardinal);
function acDrawText(hDC: HDC; const Text: ACString; var lpRect: TRect; uFormat: Cardinal): Integer;
function acTextWidth(const Canvas: TCanvas; const Text: ACString): Integer;
function acTextHeight(const Canvas: TCanvas; const Text: ACString): Integer;
function acTextExtent(const Canvas: TCanvas; const Text: ACString): TSize;
procedure acTextRect(const Canvas : TCanvas; const Rect: TRect; X, Y: Integer; const Text: ACString);

procedure acWriteTextEx(const Canvas: TCanvas; Text: PacChar; Enabled: boolean; var aRect : TRect; Flags: Cardinal; SkinData : TsCommonData; Hot : boolean; SkinManager : TObject = nil); overload;
procedure acWriteTextEx(const Canvas: TCanvas; Text: PacChar; Enabled: boolean; var aRect : TRect; Flags: Cardinal; SkinIndex : integer; Hot : boolean; SkinManager : TObject = nil); overload;
procedure acWriteText(const Canvas: TCanvas; Text: PacChar; Enabled: boolean; var aRect : TRect; Flags: Cardinal);

{$IFNDEF ACHINTS}
procedure WriteTextEx(const Canvas: TCanvas; Text: PChar; Enabled: boolean; var aRect : TRect; Flags: Cardinal; SkinIndex : integer; Hot : boolean; SkinManager : TObject = nil); overload;
procedure WriteTextEx(const Canvas: TCanvas; Text: PChar; Enabled: boolean; var aRect : TRect; Flags: Cardinal; SkinData : TsCommonData; Hot : boolean); overload;
{$IFDEF TNTUNICODE}
procedure WriteUnicode(const Canvas: TCanvas; const Text: WideString; Enabled: boolean; var aRect : TRect; Flags: Cardinal; SkinData : TsCommonData; Hot : boolean); overload;
procedure WriteTextExW(const Canvas: TCanvas; Text: PWideChar; Enabled: boolean; var aRect : TRect; Flags: Cardinal; SkinData : TsCommonData; Hot : boolean); overload;
// replace function of Canvas.TextRect
procedure TextRectW(const Canvas : TCanvas; var Rect: TRect; X, Y: Integer; const Text: WideString);
procedure WriteTextExW(const Canvas: TCanvas; Text: PWideChar; Enabled: boolean; var aRect : TRect; Flags: Cardinal; SkinIndex : integer; Hot : boolean; SkinManager : TObject = nil); overload;
{$ENDIF}

procedure PaintItemBG(SkinIndex : integer; const SkinSection : string; ci : TCacheInfo; State : integer; R : TRect; pP : TPoint; ItemBmp : TBitmap; SkinManager : TObject = nil; TextureIndex : integer = -1; HotTextureIndex : integer = -1; CustomColor : TColor = clFuchsia); overload;
procedure PaintItemBGFast(SkinIndex, BGIndex, BGHotIndex : integer; const SkinSection : string; ci : TCacheInfo; State : integer; R : TRect; pP : TPoint; ItemBmp : TBitmap; SkinManager : TObject = nil);
procedure PaintItemFast(SkinIndex, MaskIndex, BGIndex, BGHotIndex : integer; const SkinSection : string; var ci : TCacheInfo; Filling : boolean; State : integer; R : TRect; pP : TPoint; ItemBmp : TBitmap; SkinManager : TObject = nil); overload;
procedure PaintSmallItem(SkinIndex : integer; const SkinSection : string; ci : TCacheInfo; Filling : boolean; State : integer; R : TRect; pP : TPoint; ItemBmp : TBitmap; SkinManager : TObject = nil); overload;
procedure PaintItem(SkinIndex : integer; const SkinSection : string; ci : TCacheInfo; Filling : boolean; State : integer; R : TRect; pP : TPoint; ItemBmp : TBitmap; SkinManager : TObject = nil; BGIndex : integer = -1; BGHotIndex : integer = -1); overload;
procedure PaintItem(SkinIndex : integer; const SkinSection : string; ci : TCacheInfo; Filling : boolean; State : integer; R : TRect; pP : TPoint; DC : HDC; SkinManager : TObject = nil); overload;

//function ChangeHue(Delta : integer; C : TsColor) : TsColor; overload;
function ChangeBrightness(Color : TColor; Delta : integer) : TColor;
function ChangeSaturation(Color : TColor; Delta : integer) : TColor;
function ChangeHue(Delta : integer; Color : TColor) : TColor; overload;
//function ChangeHLS(Color : TColor; dH, dL, dS : integer) : TColor; overload;
function Hsv2Rgb(h, s, v : real) : TsColor;
function Rgb2Hsv(C : TsColor) : TsHSV;
function acLayered: Boolean;
{$ENDIF}

var
  User32Lib: Cardinal = 0;

  UpdateLayeredWindow: function (Handle: THandle; hdcDest: HDC; pptDst: PPoint; _psize: PSize;
    hdcSrc: HDC; pptSrc: PPoint; crKey: COLORREF; pblend: PBLENDFUNCTION; dwFlags: DWORD): Boolean; stdcall;

{$ENDIF} // NOTFORHELP
implementation

uses {$IFNDEF ACHINTS}sStyleSimply, sSkinProps, sSkinManager, sBorders, sSkinProvider, sVCLUtils, sMessages,
  {$ENDIF}sGradient, sAlphaGraph{$IFDEF TNTUNICODE}, TntWideStrUtils, TntWindows{$ENDIF}, sDefaults
{$IFNDEF ALITE}
  , sSplitter
{$ENDIF}
  ;

function acLayered: Boolean;
begin
  Result := @UpdateLayeredWindow <> nil;
end;

function SwapColor(i : integer) : integer;
var
  r, g, b : integer;
begin
  r := i mod 256;
  i := i shr 8;
  g := i mod 256;
  b := i shr 8;
  Result := r shl 16 + g shl 8 + b;
end;

{$IFNDEF ACHINTS}
function IsNAN(const d: double): boolean;
var
  Overlay: Int64 absolute d;
begin
  Result := ((Overlay and $7FF0000000000000) = $7FF0000000000000) and ((Overlay and $000FFFFFFFFFFFFF) <> $0000000000000000)
end;

function ChangeBrightness(Color : TColor; Delta : integer) : TColor;
var
  C : TsColor;
  dR, dG, dB : real;
begin
  Result := Color;
  if Delta = 0 then Exit;
  C.C := Color;

  if Delta > 0 then begin
    dR := (MaxByte - C.R) / 100;
    dG := (MaxByte - C.G) / 100;
    dB := (MaxByte - C.B) / 100;
  end
  else begin
    dR := C.R / 100;
    dG := C.G / 100;
    dB := C.B / 100;
  end;

  C.R := max(min(Round(C.R + Delta * dR), MaxByte), 0);
  C.G := max(min(Round(C.G + Delta * dG), MaxByte), 0);
  C.B := max(min(Round(C.B + Delta * dB), MaxByte), 0);
  Result := C.C;
end;

function ChangeSaturation(Color : TColor; Delta : integer) : TColor;
var
  C : TsColor;
  dR, dG, dB, Gray : real;
begin
  Result := Color;
  if Delta = 0 then Exit;
  C.C := Color;

  Gray := (C.R + C.G + C.B) / 3;

  dR := (Gray - C.R) / 100;
  dG := (Gray - C.G) / 100;
  dB := (Gray - C.B) / 100;

  Result := Rgb(max(min(Round(C.R + Delta * dR), MaxByte), 0), max(min(Round(C.G + Delta * dG), MaxByte), 0), max(min(Round(C.B + Delta * dB), MaxByte), 0));
end;

function Hsv2Rgb(h, s, v : real) : TsColor;
var
  I : integer;
  F, M, N, K : real;
begin
  Result.A := 0;
  if S = 0 then begin Result.R := IntToByte(Round(V * MaxByte)); Result.G := Result.R; Result.B := Result.R end else begin
    if H = 360 then H := 0 else H := H / 60;
    I := Round(Int(H));
    F := (H - I);

    V := V * MaxByte;
    M := V * (1 - S);
    N := V * (1 - S * F);
    K := V * (1 - S * (1 - F));

    M := max(min(M, MaxByte), 0);
    N := max(min(N, MaxByte), 0);
    K := max(min(K, MaxByte), 0);

    Result.A := 0;
    case I of
      0: begin Result.R := Round(V); Result.G := Round(K); Result.B := Round(M) end;
      1: begin Result.R := Round(N); Result.G := Round(V); Result.B := Round(M) end;
      2: begin Result.R := Round(M); Result.G := Round(V); Result.B := Round(K) end;
      3: begin Result.R := Round(M); Result.G := Round(N); Result.B := Round(V) end;
      4: begin Result.R := Round(K); Result.G := Round(M); Result.B := Round(V) end
      else begin Result.R := Round(V); Result.G := Round(M); Result.B := Round(N) end
    end;
  end
end;

function Rgb2Hsv(C : TsColor) : TsHSV;
var
  Rt, Gt, Bt : real;
  H, S, V : real;
  d, max, min : integer;
begin
  C.A := 0;
  max := math.Max(math.Max(c.R, c.G), c.B);
  min := math.Min(math.Min(c.R, c.G), c.B);
  d := max - min;
  V := max;
  if (max <> 0) then S := d / max else S := 0;
  if S = 0 then begin
    Result.H := 0;
  end
  else begin
    rt := max - c.R * 60 / d;
    gt := max - c.G * 60 / d;
    bt := max - c.B * 60 / d;
    if c.R = max then H := bt - gt else if c.G = max then H := 120 + rt - bt else H := 240 + gt - rt;
    if H < 0 then H := H + 360;
    Result.H := Round(H);
  end;
  Result.S := S;
  Result.V := V / MaxByte;//0;
end;

function ChangeHue(Delta : integer; Color : TColor) : TColor; overload;
var
  Rt, Gt, Bt : real;
  H, S, V, r : real;
  d, max, min : integer;
  I : integer;
  F, M, N, K : real;
  C : TsColor;
begin
  C.C := ColorToRGB(Color);
  C.A := 0;
  max := math.Max(math.Max(c.R, c.G), c.B);
  min := math.Min(math.Min(c.R, c.G), c.B);
  d := max - min;
  V := max;
  if (max <> 0) then S := d / max else S := 0;
  if S = 0 then H := 0 else begin
    r := 60 / d;
    rt := max - c.R * r;
    gt := max - c.G * r;
    bt := max - c.B * r;
    if c.R = max then H := bt - gt else if c.G = max then H := 120 + rt - bt else H := 240 + gt - rt;
    if H < 0 then H := H + 360;
  end;

  H := round(H + Delta) mod 360;

  if S = 0 then begin C.R := Round(V) end else begin
    H := H / 60;
    I := Round(Int(H));
    F := (H - I);

    M := V * (1 - S);
    N := V * (1 - S * F);
    K := V * (1 - S * (1 - F));

    M := Math.max(Math.min(M, MaxByte), 0);
    N := Math.max(Math.min(N, MaxByte), 0);
    K := Math.max(Math.min(K, MaxByte), 0);

    case I of
      0: begin C.R := Round(V); C.G := Round(K); C.B := Round(M) end;
      1: begin C.R := Round(N); C.G := Round(V); C.B := Round(M) end;
      2: begin C.R := Round(M); C.G := Round(V); C.B := Round(K) end;
      3: begin C.R := Round(M); C.G := Round(N); C.B := Round(V) end;
      4: begin C.R := Round(K); C.G := Round(M); C.B := Round(V) end
    else begin C.R := Round(V); C.G := Round(M); C.B := Round(N) end
    end;
  end;
  C.A := 0;
  Result := C.C
end;

procedure  HSVtoRGB (const H,S,V: Real; var R,G,B: real);
var
  f    :  Real;
  i    :  Integer;
  hTemp:  Real;              // since H is const parameter
  p, q, t:  Real;
begin
  if   (ABS(S-0.0001) <= 0.0001)  OR
       IsNan(H)                 // color is on black-and-white center line
  then begin
    if   IsNaN(H)
    then begin
      R := V;                   // achromatic:  shades of gray
      G := V;
      B := V
    end
  end

  else begin                    // chromatic color
    if   H = 360.0              // 360 degrees same as 0 degrees
    then hTemp := 0.0
    else hTemp := H;

    hTemp := hTemp / 60;        // h is now IN [0,6)
    i := TRUNC(hTemp);          // largest integer <= h
    f := hTemp - i;             // fractional part of h

    p := V * (1.0 - S);
    q := V * (1.0 - (S * f));
    t := V * (1.0 - (S * (1.0 - f)));

    CASE i OF
      0:  begin R := V;  G := t;  B := p  end;
      1:  begin R := q;  G := V;  B := p  end;
      2:  begin R := p;  G := V;  B := t  end;
      3:  begin R := p;  G := q;  B := V  end;
      4:  begin R := t;  G := p;  B := V  end;
      5:  begin R := V;  G := p;  B := q  end
    end
  end
end {HSVtoRGB};

procedure RGBToHSV (const R, G, B: Real; var H, S, V: Real);
var
  Delta:  Real;
  Min  :  Real;
begin
  Min := MinValue( [R, G, B] );
  V   := MaxValue( [R, G, B] );

  Delta := V - Min;

  // Calculate saturation:  saturation is 0 if r, g and b are all 0
  if V = 0.0 then S := 0 else S := Delta / V;

  if S = 0.0
  then {H := NAN} // Achromatic:  When s = 0, h is undefined
  else begin    // Chromatic
    if R = V
    then // between yellow and magenta [degrees]
      H := 60.0 * (G - B) / Delta
    else if G = V
      then // between cyan and yellow
        H := 120.0 + 60.0 * (B - R) / Delta
      else if B = V
        then // between magenta and cyan
          H := 240.0 + 60.0 * (R - G) / Delta;
    if H < 0.0 then H := H + 360.0
  end
end {RGBtoHSV};

procedure GetRgnFromBmp(var rgn : hrgn; MaskBmp : TBitmap; TransColor : TColor);
var
  ArOR : TAOR;
  subrgn : hrgn;
  i, l : integer;
begin
  SetLength(ArOR, 0);
  AddRgnBmp(ArOR, MaskBmp, ColorToSColor(TransColor));

  l := Length(ArOR);
  rgn := CreateRectRgn(0, 0, MaskBmp.Width, MaskBmp.Height);
  if l > 0 then begin
    for i := 0 to l - 1 do begin
      subrgn := CreateRectRgn(ArOR[i].Left, ArOR[i].Top, ArOR[i].Right, ArOR[i].Bottom);
      CombineRgn(rgn, rgn, subrgn, RGN_DIFF);
      DeleteObject(subrgn);
    end
  end
end;

function GetBGInfo(const BGInfo : PacBGInfo; const Handle : THandle; PleaseDraw : boolean = False) : boolean;
var
  b : boolean;
  FSaveIndex : hdc;
  P: TPoint;
begin
  b := BGInfo^.PleaseDraw;// and (DefaultManager <> nil) and not (csDesigning in DefaultManager.ComponentState);
  BGInfo^.BgType := btUnknown;
  BGInfo^.PleaseDraw := PleaseDraw;
  SendMessage(Handle, SM_ALPHACMD, MakeWParam(0, AC_GETBG), longint(BGInfo));
  if BGInfo^.BgType <> btUnknown then Result := True else begin
    if b then begin // If real parent bg is required
      FSaveIndex := SaveDC(BGInfo^.DrawDC);
      GetViewportOrgEx(BGInfo^.DrawDC, P);
      SetViewportOrgEx(BGInfo^.DrawDC, P.X - BGInfo^.Offset.X, P.Y - BGInfo^.Offset.Y, nil);
      OffsetRect(BGInfo^.R, BGInfo^.Offset.X, BGInfo^.Offset.Y);
      IntersectClipRect(BGInfo^.DrawDC, BGInfo^.R.Left, BGInfo^.R.Top, BGInfo^.R.Right, BGInfo^.R.Bottom);

      SendMessage(Handle, WM_ERASEBKGND, Longint(BGInfo^.DrawDC), 0);
      SendMessage(Handle, WM_PAINT, Longint(BGInfo^.DrawDC), 0);
      RestoreDC(BGInfo^.DrawDC, FSaveIndex);
    end
    else begin
      if DefaultManager <> nil then BGInfo^.Color := DefaultManager.GetGlobalColor else BGInfo^.Color := clBtnFace;
      BGInfo^.BgType := btFill;
      Result := False;
    end;
  end;
//  if (BGInfo^.BgType = btFill) and (BGInfo^.Color = clFuchsia) then Alert; // For debuging
end;

function BGInfoToCI(const BGInfo : PacBGInfo) : TCacheInfo;
begin
  if BGInfo^.BgType = btCache then begin
    Result := MakeCacheInfo(BGInfo^.Bmp, BGInfo^.Offset.X, BGInfo^.Offset.Y);
  end
  else begin
    Result.Ready := False;
    Result.FillColor := ColorToRGB(BGInfo^.Color);
    Result.Bmp := nil;
  end;
//  if not Result.Ready and (Result.FillColor = clFuchsia) then Alert;       // For debuging
end;

function GetBGInfo(const BGInfo : PacBGInfo; const Control : TControl; PleaseDraw : boolean = False) : boolean; overload;
var
  b : boolean;
  FSaveIndex : hdc;
  P: TPoint;
begin
  b := BGInfo^.PleaseDraw;// and (DefaultManager <> nil) and not (csDesigning in DefaultManager.ComponentState);
  BGInfo^.BgType := btUnknown;
  BGInfo^.PleaseDraw := PleaseDraw;
  if (Control is TWinControl) and (TWinControl(Control)).HandleAllocated
    then SendMessage(TWinControl(Control).Handle, SM_ALPHACMD, MakeWParam(0, AC_GETBG), longint(BGInfo))
    else Control.Perform(SM_ALPHACMD, MakeWParam(0, AC_GETBG), longint(BGInfo));
  if BGInfo^.BgType <> btUnknown then Result := True else begin
    if b then begin // If real parent bg is required
      FSaveIndex := SaveDC(BGInfo^.DrawDC);
      GetViewportOrgEx(BGInfo^.DrawDC, P);
      SetViewportOrgEx(BGInfo^.DrawDC, P.X - BGInfo^.Offset.X, P.Y - BGInfo^.Offset.Y, nil);
      OffsetRect(BGInfo^.R, BGInfo^.Offset.X, BGInfo^.Offset.Y);
      IntersectClipRect(BGInfo^.DrawDC, BGInfo^.R.Left, BGInfo^.R.Top, BGInfo^.R.Right, BGInfo^.R.Bottom);

      Control.Perform(WM_ERASEBKGND, Longint(BGInfo^.DrawDC), 0);
      Control.Perform(WM_PAINT, Longint(BGInfo^.DrawDC), 0);
      RestoreDC(BGInfo^.DrawDC, FSaveIndex);
    end
    else begin
      Result := False;
      BGInfo^.BgType := btFill;
      BGInfo^.Color := Control.Perform(SM_ALPHACMD, MakeWParam(0, AC_GETCONTROLCOLOR), 0);
      if BGInfo^.Color = 0 then BGInfo^.Color := ColorToRGB(TsHackedControl(Control).Color);
      if BGInfo^.PleaseDraw and (BGInfo^.Bmp <> nil) then FillDC(BGInfo^.DrawDC, BGInfo^.R, BGInfo^.Color);
    end
  end;
//  if (BGInfo^.BgType = btFill) and (BGInfo^.Color = clFuchsia) then Alert; // For debuging
end;

procedure AddRgnBmp(var AOR : TAOR; MaskBmp : TBitmap; TransColor : TsColor);
var
  X, Y, h, w, l: Integer;
  c : TsColor;
  RegRect : TRect;
begin
  h := MaskBmp.Height - 1;
  w := MaskBmp.Width - 1;
  RegRect := Rect(-1, 0, 0, 0);
  TransColor.A := 0;
  c.A := 0;
  l := Length(AOR);
  try
    if Fast24Src.Attach(MaskBmp) then for Y := 0 to h do begin
      for X := 0 to w do begin
        c := Fast24Src.Pixels[x, y];
        if c.C = TransColor.C then begin
          if RegRect.Left <> -1 then inc(RegRect.Right) else begin
            RegRect.Left := X;
            RegRect.Right := RegRect.Left + 1;
            RegRect.Top := Y;
            RegRect.Bottom := RegRect.Top + 1;
          end;
        end
        else if RegRect.Left <> -1 then begin
          SetLength(aOR, l + 1);
          AOR[l] := RegRect;
          inc(l);
          RegRect.Left := -1;
        end;
      end;
      if RegRect.Left <> -1 then begin
        SetLength(AOR, l + 1);
        AOR[l] := RegRect;
        inc(l);
        RegRect.Left := -1;
      end;
    end;
  except
  end;
end;

procedure AddRgn(var AOR : TAOR; Width : integer; MaskData : TsMaskData; VertOffset : integer; Bottom : boolean);
var
  S : PRGBArray;
  X, Y, h, w, l, w2, cx: Integer;
  c, ct : TsColor;
  cur : TsRGB;
  RegRect : TRect;
  Bmp : TBitmap;
  XOffs, YOffs, MaskOffs : integer;
begin
  if MaskData.Manager = nil then Exit;
  if MaskData.Bmp = nil then Bmp := TsSkinManager(MaskData.Manager).MasterBitmap else Bmp := MaskData.Bmp;

  if Bottom then h := MaskData.WB else h := MaskData.WT;
  w := MaskData.WL;
  MaskOffs := integer(Bottom) * (HeightOf(MaskData.R) div (1 + MaskData.MaskType) - MaskData.WB);
  XOffs := MaskData.R.Left; YOffs := MaskData.R.Top;

  if Bmp = nil then Exit;
  inc(YOffs, MaskOffs);
  RegRect := Rect(-1, 0, 0, 0);
  ct.C := clFuchsia;
  l := Length(AOR);
  dec(h); dec(w);
  if h + YOffs > Bmp.Height then Exit;
  c.A := 0;
  try
    for Y := 0 to h do begin
      S := Bmp.ScanLine[Y + YOffs];
      for X := 0 to w do begin
        cur := S[X + XOffs];
        if (cur.R = ct.R) and (cur.G = ct.G) and (cur.B = ct.B) then begin
          if RegRect.Left <> -1 then inc(RegRect.Right) else begin
            RegRect.Left := X;
            RegRect.Right := RegRect.Left + 1;
            RegRect.Top := Y + VertOffset;
            RegRect.Bottom := RegRect.Top + 1;
          end;
        end
        else if RegRect.Left <> -1 then begin
          SetLength(aOR, l + 1);
          AOR[l] := RegRect;
          inc(l);
          RegRect.Left := -1;
        end;
      end;
      if RegRect.Left <> -1 then begin
        SetLength(AOR, l + 1);
        AOR[l] := RegRect;
        inc(l);
        RegRect.Left := -1;
      end;
    end;

    w2 := WidthOf(MaskData.R) div MaskData.ImageCount - 1;      //x2
    w := WidthOf(MaskData.R) div MaskData.ImageCount - MaskData.WR;
    cx := Width - WidthOf(MaskData.R) div MaskData.ImageCount;  //First pixel on control
    for Y := 0 to h do begin
      S := Bmp.ScanLine[Y + YOffs];
      for X := w to w2 do begin
        cur := S[X + XOffs];
        if (cur.R = ct.R) and (cur.G = ct.G) and (cur.B = ct.B) then begin
          if RegRect.Left <> -1 then inc(RegRect.Right) else begin
            RegRect.Left := cx + X;
            RegRect.Right := RegRect.Left + 1;
            RegRect.Top := Y + VertOffset;
            RegRect.Bottom := RegRect.Top + 1;
          end;
        end
        else if RegRect.Left <> -1 then begin
          SetLength(aOR, l + 1);
          AOR[l] := RegRect;
          inc(l);
          RegRect.Left := -1;
        end;
      end;
      if RegRect.Left <> -1 then begin
        SetLength(AOR, l + 1);
        AOR[l] := RegRect;
        inc(l);
        RegRect.Left := -1;
      end;
    end;
  except
  end;
end;

function GetRgnForMask(MaskIndex, Width, Height : integer; SkinManager : TObject) : hrgn;
var
  ArOR : TAOR;
  SubRgn : hrgn;
  i, l : integer;
begin
  Result := 0;
  SetLength(ArOR, 0);
  if TsSkinManager(SkinManager).IsValidImgIndex(MaskIndex) then begin
    AddRgn(ArOR, Width, TsSkinManager(SkinManager).ma[MaskIndex], 0, False);
    if TsSkinManager(SkinManager).ma[MaskIndex].Bmp = nil
      then AddRgn(ArOR, Width, TsSkinManager(SkinManager).ma[MaskIndex], Height - (HeightOf(TsSkinManager(SkinManager).ma[MaskIndex].R) div (3 * (1 + TsSkinManager(SkinManager).ma[MaskIndex].MaskType))), True)
      else AddRgn(ArOR, Width, TsSkinManager(SkinManager).ma[MaskIndex], Height - TsSkinManager(SkinManager).ma[MaskIndex].Bmp.Height div 6, True);

    l := Length(ArOR);
    if (l > 0) then begin
      Result := CreateRectRgn(0, 0, Width, Height);
      for i := 0 to l - 1 do begin
        SubRgn := CreateRectRgn(ArOR[i].Left, ArOR[i].Top, ArOR[i].Right, ArOR[i].Bottom);
        CombineRgn(Result, Result, SubRgn, RGN_DIFF);
        DeleteObject(SubRgn);
      end;
    end
  end;
end;

procedure CopyImage(Glyph : TBitmap; ImageList: TCustomImageList; Index: Integer);
begin
  with Glyph do begin
    Width := ImageList.Width;
    Height := ImageList.Height;
    if ImageList.BkColor = clNone then Canvas.Brush.Color := clFuchsia else Canvas.Brush.Color := ImageList.BkColor;//! for lack of a better color
    Canvas.FillRect(Rect(0,0, Width, Height));
{$IFDEF DELPHI7UP}
    ImageList.Draw(Canvas, 0, 0, Index, dsTransparent, itImage);
{$ELSE}
    ImageList.Draw(Canvas, 0, 0, Index, True);
{$ENDIF}
  end;
end;

procedure PaintItemBG(SkinIndex : integer; const SkinSection : string; ci : TCacheInfo; State : integer; R : TRect; pP : TPoint; ItemBmp : TBitmap; SkinManager : TObject = nil; TextureIndex : integer = -1; HotTextureIndex : integer = -1; CustomColor : TColor = clFuchsia);
var
  aRect: TRect;
  TransColor : TsColor;
  iDrawed : boolean;
  TempBmp : TBitmap;

  ImagePercent, GradientPercent : integer;
  PatternIndex, Transparency : integer;
  GradientData : string;
  GradientArray : TsGradArray;
  Color : TColor;
  Isjpg : boolean;
  md : TsMaskData;

  procedure PaintAddons(var aBmp : TBitmap);
  var
    bmp : TBitmap;
    R : TRect;
  begin
    iDrawed := False;
    R := aRect;
    // BGImage painting
    if (ImagePercent > 0) then with TsSkinManager(SkinManager) do begin
      if IsJpg then begin
        if (PatternIndex > -1) and (PatternIndex < Length(pa)) then begin
          TileBitmap(aBmp.Canvas, R, pa[PatternIndex].Img, md);
          iDrawed := True;
        end;
      end
      else if (PatternIndex > -1) and (PatternIndex < Length(ma)) then begin
        if boolean(ma[PatternIndex].MaskType)
          then TileMasked(aBmp, R, CI, ma[PatternIndex], acFillModes[ma[PatternIndex].DrawMode])
          else TileBitmap(aBmp.Canvas, R, ma[PatternIndex].Bmp, ma[PatternIndex], acFillModes[ma[PatternIndex].DrawMode]);
        iDrawed := True;
      end;
      if R.Right <> -1 then FillDC(aBmp.Canvas.Handle, R, Color);
    end;
    // BGGradient painting
    if (GradientPercent > 0) then begin
      if iDrawed then begin
        bmp := CreateBmp24(WidthOf(aRect), HeightOf(aRect));
        try
          if Length(GradientData) > 0
            then PaintGrad(Bmp, Rect(0, 0, Bmp.Width, Bmp.Height), GradientArray)
            else FillDC(Bmp.Canvas.Handle, aRect, Color);

          TransColor.A := 0;
          TransColor.R := IntToByte((ImagePercent * MaxByte) div 100); TransColor.G := TransColor.R; TransColor.B := TransColor.R;
          SumBmpRect(aBmp, Bmp, TransColor, Rect(0, 0, Bmp.Width - 1, Bmp.Height{ - 1}), Point(aRect.Left, aRect.Top));
        finally
          FreeAndNil(Bmp);
        end;
      end
      else if Length(GradientData) > 0 then PaintGrad(aBmp, aRect, GradientArray) else FillDC(aBmp.Canvas.Handle, aRect, Color);
    end;
    case GradientPercent + ImagePercent of
      1..99 : BlendColorRect(aBmp, aRect, GradientPercent + ImagePercent, Color);
      0 : if not ci.Ready and (Transparency <> 0)
        then FillDC(aBmp.Canvas.Handle, aRect, ci.FillColor)
        else FillDC(aBmp.Canvas.Handle, aRect, Color);
    end;
  end;
begin
  if SkinManager = nil then SkinManager := DefaultManager;
  if not Assigned(DefaultManager) or not TsSkinManager(SkinManager).IsValidSkinIndex(SkinIndex) then Exit;
  with TsSkinManager(SkinManager) do begin {SeeLater}
    aRect := R;
    IsJpg := False;
    if CustomColor = clFuchsia then begin
      case State of
        0 : begin
          Color := gd[SkinIndex].Color;
          Transparency := gd[SkinIndex].Transparency;
          if Transparency <> 100 then begin
            ImagePercent := gd[SkinIndex].ImagePercent;
            GradientPercent := gd[SkinIndex].GradientPercent;
            if (ImagePercent > 0) then begin
              if TextureIndex <> -1 then begin
                if ma[TextureIndex].MaskType = 0 then PatternIndex := TextureIndex
              end
              else PatternIndex := GetMaskIndex(SkinIndex, SkinSection, s_Pattern);
              if not TsSkinManager(SkinManager).IsValidImgIndex(PatternIndex) then begin PatternIndex := GetPatternIndex(SkinIndex, SkinSection, s_Pattern); IsJpg := PatternIndex > -1 end;
            end;
            if GradientPercent <> 0 then begin GradientData := gd[SkinIndex].GradientData; GradientArray := gd[SkinIndex].GradientArray end;
          end
        end
        else begin
          Color := gd[SkinIndex].HotColor;
          Transparency := gd[SkinIndex].HotTransparency;
          if Transparency <> 100 then begin
            ImagePercent := TsSkinManager(SkinManager).gd[SkinIndex].HotImagePercent;
            GradientPercent := gd[SkinIndex].HotGradientPercent;
            if (ImagePercent > 0) then begin
              if HotTextureIndex <> -1 then begin
                if TsSkinManager(SkinManager).ma[HotTextureIndex].MaskType = 0 then PatternIndex := HotTextureIndex
              end
              else PatternIndex := GetMaskIndex(SkinIndex, SkinSection, s_HotPattern);
              if not TsSkinManager(SkinManager).IsValidImgIndex(PatternIndex) then begin PatternIndex := GetPatternIndex(SkinIndex, SkinSection, s_HotPattern); IsJpg := PatternIndex > -1 end;
            end;
            if GradientPercent <> 0 then begin GradientData := gd[SkinIndex].HotGradientData; GradientArray := gd[SkinIndex].HotGradientArray end;
          end
        end;
      end;
    end
    else begin
      Color := CustomColor;
      ImagePercent := 0;
      GradientPercent := 0;
      Transparency := 0;
    end;
    if ci.Ready or (ci.FillColor <> clFuchsia) then case Transparency of
      100 : begin
        if ci.Ready then begin
          if ItemBmp <> ci.Bmp then begin
            BitBlt(ItemBmp.Canvas.Handle, aRect.Left, aRect.Top, WidthOf(aRect), HeightOf(aRect), ci.Bmp.Canvas.Handle, ci.X + pP.X, ci.Y + pP.Y, SRCCOPY)
          end;
        end
        else FillDC(ItemBmp.Canvas.Handle, aRect, ci.FillColor);
      end;
      0 : PaintAddons(ItemBmp);
      else begin
        TempBmp := CreateBmp24(WidthOf(aRect), HeightOf(aRect));
        try
          OffsetRect(aRect, - aRect.Left, - aRect.Top);
          PaintAddons(TempBmp);
          aRect := R;
          TransColor.A := 0; TransColor.R := IntToByte(Transparency * MaxByte div 100); TransColor.G := TransColor.R; TransColor.B := TransColor.R;

          if ci.Ready then begin
            if ItemBmp <> ci.Bmp then BitBlt(ItemBmp.Canvas.Handle, R.Left, R.Top, WidthOf(R), HeightOf(R), ci.Bmp.Canvas.Handle, ci.X + pP.X, ci.Y + pP.y, SRCCOPY)
          end
          else FillDC(ItemBmp.Canvas.Handle, R, ci.FillColor);
          SumBmpRect(ItemBmp, TempBmp, TransColor, Rect(0, 0, WidthOf(aRect), HeightOf(aRect)), Point(aRect.Left, aRect.Top));
        finally
          FreeAndNil(TempBmp);
        end;
      end;
    end
    else PaintAddons(ItemBmp);

    case State of
      0 : if (TextureIndex <> -1) and (ma[TextureIndex].MaskType > 0) then begin
        if (ma[TextureIndex].DrawMode in [ord(fmDisTiled)]) then begin
          inc(ci.X, pP.X);
          inc(ci.Y, pP.Y);
          TileMasked(ItemBmp, R, ci, ma[TextureIndex], acFillModes[ma[TextureIndex].DrawMode]);
          dec(ci.X, pP.X);
          dec(ci.Y, pP.Y);
        end;
      end
      else if (HotTextureIndex <> -1) and (ma[HotTextureIndex].MaskType > 0) then begin
        if (ma[HotTextureIndex].DrawMode in [ord(fmDisTiled)]) then begin
          inc(ci.X, pP.X);
          inc(ci.Y, pP.Y);
          TileMasked(ItemBmp, R, ci, ma[HotTextureIndex], acFillModes[ma[HotTextureIndex].DrawMode]);
          dec(ci.X, pP.X);
          dec(ci.Y, pP.Y);
        end;
      end;
    end;    
  end;
end;

procedure PaintItemBG(SkinData : TsCommonData; ci : TCacheInfo; State : integer; R : TRect; pP : TPoint; ItemBmp : TBitmap; OffsetX : integer = 0; OffsetY : integer = 0); overload;
var
  CustomColor : TColor;
begin
  if SkinData.CustomColor then begin // If custom color used
    if SkinData.FOwnerObject is TsSkinProvider
      then CustomColor := ColorToRGB(TsHackedControl(TsSkinProvider(SkinData.FOwnerObject).Form).Color)
      else if (SkinData.FOwnerControl <> nil)
        then CustomColor := ColorToRGB(TsHackedControl(SkinData.FOwnerControl).Color)
        else CustomColor := clFuchsia;
  end
  else CustomColor := clFuchsia;
  PaintItemBG(SkinData.SkinIndex, SkinData.SkinSection, ci, State, R, pP, ItemBmp, SkinData.SkinManager, SkinData.Texture, SkinData.HotTexture, CustomColor);
end;

procedure PaintItemBGFast(SkinIndex, BGIndex, BGHotIndex : integer; const SkinSection : string; ci : TCacheInfo; State : integer; R : TRect; pP : TPoint; ItemBmp : TBitmap; SkinManager : TObject = nil);
var
  aRect: TRect;
  TransColor : TsColor;
  iDrawed : boolean;
  TempBmp : TBitmap;

  ImagePercent, GradientPercent : integer;
  PatternIndex, Transparency : integer;
  GradientData : string;
  GradientArray : TsGradArray;
  Color : TColor;
  Isjpg : boolean;
  md : TsMaskData;

  procedure PaintAddons(var aBmp : TBitmap);
  var
    bmp : TBitmap;
    R : TRect;
  begin
    iDrawed := False;
    R := aRect;
    // BGImage painting
    if (ImagePercent > 0) then with TsSkinManager(SkinManager) do begin
      if IsJpg then begin
        if (PatternIndex > -1) and (PatternIndex < Length(pa)) then begin
          TileBitmap(aBmp.Canvas, R, pa[PatternIndex].Img, md);
          iDrawed := True;
        end;
      end
      else if (PatternIndex > -1) and (PatternIndex < Length(ma)) then begin
        if boolean(ma[PatternIndex].MaskType)
          then TileMasked(aBmp, R, CI, ma[PatternIndex], acFillModes[ma[PatternIndex].DrawMode])
          else TileBitmap(aBmp.Canvas, R, ma[PatternIndex].Bmp, ma[PatternIndex], acFillModes[ma[PatternIndex].DrawMode]);
        iDrawed := True;
      end;
      if R.Right <> -1 then FillDC(aBmp.Canvas.Handle, R, Color);
    end;
    // BGGradient painting
    if (GradientPercent > 0) then begin
      if iDrawed then begin
        bmp := CreateBmp24(WidthOf(aRect), HeightOf(aRect));
        try                                                
          if Length(GradientData) > 0
            then PaintGrad(Bmp, Rect(0, 0, Bmp.Width, Bmp.Height), GradientArray)
            else FillDC(Bmp.Canvas.Handle, aRect, Color);

          TransColor.A := 0;
          TransColor.R := ImagePercent * 256 div 100; TransColor.G := TransColor.R; TransColor.B := TransColor.R;
          SumBmpRect(aBmp, Bmp, TransColor, Rect(0, 0, Bmp.Width - 1, Bmp.Height{ - 1}), Point(aRect.Left, aRect.Top));
        finally
          FreeAndNil(Bmp);
        end;
      end
      else begin
        if Length(GradientData) > 0 then begin
          PaintGrad(aBmp, aRect, GradientArray);
        end
        else FillDC(aBmp.Canvas.Handle, aRect, Color);
      end;
    end;
    case GradientPercent + ImagePercent of
      1..99 : BlendColorRect(aBmp, aRect, GradientPercent + ImagePercent, Color);
      100 :
      else begin
        if not CI.Ready and (Transparency = 100)
          then FillDC(aBmp.Canvas.Handle, aRect, CI.FillColor)
          else FillDC(aBmp.Canvas.Handle, aRect, Color);
      end;
    end;
  end;
begin
  if SkinManager = nil then SkinManager := DefaultManager;
  if not Assigned(DefaultManager) or not TsSkinManager(SkinManager).IsValidSkinIndex(SkinIndex) then Exit;
  with TsSkinManager(SkinManager) do begin

    aRect := R;
    IsJpg := False;
    // Properties definition from skin file
    case State of
      0 : begin
        Color := gd[SkinIndex].Color;
        ImagePercent := gd[SkinIndex].ImagePercent;
        GradientPercent := gd[SkinIndex].GradientPercent;
        PatternIndex := BGIndex;
        if GradientPercent <> 0 then begin
          GradientData := gd[SkinIndex].GradientData;
          GradientArray := gd[SkinIndex].GradientArray;
        end;
        Transparency := gd[SkinIndex].Transparency;
      end
      else begin
        Color := gd[SkinIndex].HotColor;
        ImagePercent := gd[SkinIndex].HotImagePercent;
        GradientPercent := gd[SkinIndex].HotGradientPercent;
        PatternIndex := BGHotIndex;
        if GradientPercent <> 0 then begin
          GradientData := gd[SkinIndex].HotGradientData;
          GradientArray := gd[SkinIndex].HotGradientArray;
        end;
        Transparency := gd[SkinIndex].HotTransparency;
      end;
    end;

    if ci.Ready and (Transparency = 100) then begin
      if ItemBmp <> ci.Bmp then begin
        BitBlt(ItemBmp.Canvas.Handle, aRect.Left, aRect.Top, WidthOf(aRect), HeightOf(aRect),
          ci.Bmp.Canvas.Handle, ci.X + pP.X, ci.Y + pP.Y, SRCCOPY);
      end;
    end
    else if not ci.Ready or (Transparency = 0) then begin
      PaintAddons(ItemBmp);
    end
    else if ci.Ready and (Transparency > 0) then begin
      TempBmp := CreateBmp24(WidthOf(aRect), HeightOf(aRect));
      try
        OffsetRect(aRect, - aRect.Left, - aRect.Top);
        PaintAddons(TempBmp);
        aRect := R;
        TransColor.A := 0;
        TransColor.R := IntToByte(Transparency * MaxByte div 100); TransColor.G := TransColor.R; TransColor.B := TransColor.R;
        if ci.Ready and (ci.Bmp <> nil) and (ci.Bmp <> ItemBmp) then begin
          BitBlt(ItemBmp.Canvas.Handle, aRect.Left, aRect.Top, WidthOf(aRect), HeightOf(aRect),
               ci.Bmp.Canvas.Handle, ci.X + pP.X, ci.Y + pP.y, SRCCOPY);
        end;
        SumBmpRect(ItemBmp, TempBmp, TransColor, Rect(0, 0, WidthOf(aRect), HeightOf(aRect)), Point(aRect.Left, aRect.Top));
      finally
        FreeAndNil(TempBmp);
      end;
    end;
  end;
end;

procedure PaintItem(SkinIndex : integer; const SkinSection : string; ci : TCacheInfo; Filling : boolean; State : integer; R : TRect; pP : TPoint; DC : HDC; SkinManager : TObject = nil); overload;
var
  TempBmp : TBitmap;
  SavedDC : HDC;
begin
  if (SkinManager = nil) then SkinManager := DefaultManager;
  if not Assigned(SkinManager) or not TsSkinManager(SkinManager).IsValidSkinIndex(SkinIndex) or (R.Left < 0) or (R.Top < 0) or (WidthOf(r) < 1) or (HeightOf(r) < 1) then Exit;
  SavedDC := SaveDC(DC);
  TempBmp := CreateBmp24(WidthOf(r), HeightOf(r));
  try
    PaintItem(SkinIndex, SkinSection, ci, Filling, State, Rect(0, 0, TempBmp.Width, TempBmp.Height), pP, TempBmp, SkinManager);
    BitBlt(DC, r.Left, r.top, WidthOf(r), HeightOf(r), TempBmp.Canvas.Handle, 0, 0, SRCCOPY);
  finally
    FreeAndNil(TempBmp);
    RestoreDC(DC, SavedDC);
  end;
end;

procedure PaintItem(SkinIndex : integer; const SkinSection : string; ci : TCacheInfo; Filling : boolean; State : integer; R : TRect; pP : TPoint; ItemBmp : TBitmap; SkinManager : TObject = nil; BGIndex : integer = -1; BGHotIndex : integer = -1); overload;
var
  i : integer;
begin
  if (ItemBmp = nil) or (R.Left >= R.Right) or (R.Top >= R.Bottom) then Exit;
  if (SkinManager = nil) then SkinManager := DefaultManager;
  if not Assigned(SkinManager) or not TsSkinManager(SkinManager).IsValidSkinIndex(SkinIndex) or (R.Bottom > ItemBmp.Height) or (R.Right > ItemBmp.Width) or (R.Left < 0) or (R.Top < 0) then Exit;
  PaintItemBG(SkinIndex, SkinSection, ci, State, R, pP, ItemBmp, SkinManager, BGIndex, BGHotIndex);

  with TsSkinManager(SkinManager).gd[SkinIndex] do begin
    if ImgTL > -1 then DrawSkinGlyph(ItemBmp, Point(R.Left, R.Top), State, 1, TsSkinManager(SkinManager).ma[ImgTL], MakeCacheInfo(ItemBmp));
    if ImgTR > -1 then DrawSkinGlyph(ItemBmp, Point(R.Right - WidthOfImage(TsSkinManager(SkinManager).ma[ImgTR]), R.Top), State, 1, TsSkinManager(SkinManager).ma[ImgTR], MakeCacheInfo(ItemBmp));
    if ImgBL > -1 then DrawSkinGlyph(ItemBmp, Point(0, R.Bottom - HeightOfImage(TsSkinManager(SkinManager).ma[ImgBL])), State, 1, TsSkinManager(SkinManager).ma[ImgBL], MakeCacheInfo(ItemBmp));
    if ImgBR > -1 then DrawSkinGlyph(ItemBmp, Point(R.Right - WidthOfImage(TsSkinManager(SkinManager).ma[ImgBR]), R.Bottom - HeightOfImage(TsSkinManager(SkinManager).ma[i])), State, 1, TsSkinManager(SkinManager).ma[ImgBR], MakeCacheInfo(ItemBmp));

    inc(ci.X, pP.X);
    inc(ci.Y, pP.Y);
    if TsSkinManager(SkinManager).IsValidImgIndex(BorderIndex) then DrawSkinRect(ItemBmp, R, Filling, ci, TsSkinManager(SkinManager).ma[BorderIndex], State, True);
  end;
end;

procedure PaintItemFast(SkinIndex, MaskIndex, BGIndex, BGHotIndex : integer; const SkinSection : string; var ci : TCacheInfo; Filling : boolean; State : integer; R : TRect; pP : TPoint; ItemBmp : TBitmap; SkinManager : TObject = nil); overload;
begin
  if SkinManager = nil then SkinManager := DefaultManager;
  if not Assigned(SkinManager) or not TsSkinManager(SkinManager).IsValidSkinIndex(SkinIndex) then Exit;
  if (R.Bottom > ItemBmp.Height) or (R.Right > ItemBmp.Width) or (R.Left < 0) or (R.Top < 0) then Exit;
  PaintItemBGFast(SkinIndex, BGIndex, BGHotIndex, SkinSection, ci, State, R, pP, ItemBmp, SkinManager);
  inc(ci.X, pP.X);
  inc(ci.Y, pP.Y);
  if TsSkinManager(SkinManager).IsValidImgIndex(MaskIndex) then DrawSkinRect(ItemBmp, R, Filling, ci, TsSkinManager(SkinManager).ma[MaskIndex], State, True, TsSkinManager(SkinManager));
  dec(ci.X, pP.X);
  dec(ci.Y, pP.Y);
end;

procedure PaintSmallItem(SkinIndex : integer; const SkinSection : string; ci : TCacheInfo; Filling : boolean; State : integer; R : TRect; pP : TPoint; ItemBmp : TBitmap; SkinManager : TObject = nil); overload;
var
  i : integer;
begin
  if SkinManager = nil then SkinManager := DefaultManager;
  if not Assigned(SkinManager) or not TsSkinManager(SkinManager).IsValidSkinIndex(SkinIndex) or (R.Bottom > ItemBmp.Height) or (R.Right > ItemBmp.Width) or (R.Left < 0) or (R.Top < 0) then Exit;
  PaintItemBG(SkinIndex, SkinSection, ci, State, R, pP, ItemBmp, SkinManager);
  i := TsSkinManager(SkinManager).GetMaskIndex(SkinIndex, SkinSection, s_BordersMask);
  inc(ci.X, pP.X);
  inc(ci.Y, pP.Y);
  if TsSkinManager(SkinManager).IsValidImgIndex(i) then DrawSmallSkinRect(ItemBmp, R, Filling, ci, TsSkinManager(SkinManager).ma[i], State);
end;

function PaintSection(const Bmp : TBitmap; Section : string; const SecondSection : string; const State : integer; const Manager : TObject; const ParentOffset : TPoint; const BGColor : TColor; ParentDC : hdc = 0) : integer;
var
  CI : TCacheInfo;
begin
  with TsSkinManager(Manager) do begin
    Result := GetSkinIndex(Section);
    if not IsValidSkinIndex(Result) then begin
      Section := SecondSection;
      Result := GetSkinIndex(Section);
    end;
    if IsValidSkinIndex(Result) then begin
      CI.FillColor := BGColor;//GetGlobalColor;
      if ParentDC = 0 then CI.Ready := False else begin
        BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, ParentDC, ParentOffset.x, ParentOffset.y, SRCCOPY);
        CI.Bmp := Bmp;
        CI.Ready := True;
      end;
      PaintItem(Result, Section, CI, True, State, Rect(0, 0, Bmp.Width, Bmp.Height), Point(0, 0), Bmp, DefaultManager);
    end;
  end;
end;

procedure PaintItem(SkinData : TsCommonData; ci : TCacheInfo; Filling : boolean; State : integer; R : TRect; pP : TPoint; ItemBmp : TBitmap; UpdateCorners : boolean; OffsetX : integer = 0; OffsetY : integer = 0); overload;
var
  i : integer;
begin
  if (ItemBmp = nil) or not Assigned(SkinData.SkinManager) or not SkinData.SkinManager.IsValidSkinIndex(SkinData.SkinIndex) or
    (R.Bottom > ItemBmp.Height) or (R.Right > ItemBmp.Width) or (R.Left < 0) or (R.Top < 0) then Exit;

  PaintItemBG(SkinData, ci, State, R, pP, ItemBmp, OffsetX, OffsetY);
  inc(ci.X, pP.X);
  inc(ci.Y, pP.Y);
  if Assigned(SkinData.SkinManager) and SkinData.SkinManager.IsValidImgIndex(SkinData.BorderIndex) and not ((State = 0) and (SkinData.SkinManager.ma[SkinData.BorderIndex].DrawMode and BDM_ACTIVEONLY = BDM_ACTIVEONLY)) 
    then DrawSkinRect(ItemBmp, R, Filling, ci, SkinData.SkinManager.ma[SkinData.BorderIndex], State, UpdateCorners);

  with TsSkinManager(SkinData.SkinManager).gd[SkinData.SkinIndex] do begin
    if ImgTL > -1 then DrawSkinGlyph(ItemBmp, Point(R.Left, R.Top), State, 1, TsSkinManager(SkinData.SkinManager).ma[ImgTL], MakeCacheInfo(ItemBmp));
    if ImgTR > -1 then DrawSkinGlyph(ItemBmp, Point(R.Right - WidthOfImage(TsSkinManager(SkinData.SkinManager).ma[ImgTR]), R.Top), State, 1, TsSkinManager(SkinData.SkinManager).ma[ImgTR], MakeCacheInfo(ItemBmp));
    if ImgBL > -1 then DrawSkinGlyph(ItemBmp, Point(0, R.Bottom - HeightOfImage(TsSkinManager(SkinData.SkinManager).ma[ImgBL])), State, 1, TsSkinManager(SkinData.SkinManager).ma[ImgBL], MakeCacheInfo(ItemBmp));
    if ImgBR > -1 then DrawSkinGlyph(ItemBmp, Point(R.Right - WidthOfImage(TsSkinManager(SkinData.SkinManager).ma[ImgBR]), R.Bottom - HeightOfImage(TsSkinManager(SkinData.SkinManager).ma[i])), State, 1, TsSkinManager(SkinData.SkinManager).ma[ImgBR], MakeCacheInfo(ItemBmp));
  end;
end;

procedure PaintSkinControl(const SkinData : TsCommonData; const Parent : TControl; const Filling : boolean; const State : integer; const R : TRect; const pP : TPoint; const ItemBmp : TBitmap; const UpdateCorners : boolean; const OffsetX : integer = 0; const OffsetY : integer = 0);
var
  i : integer;
  BG : TacBGInfo;
  CI : TCacheInfo;
begin
  if (ItemBmp = nil) or not Assigned(SkinData.SkinManager) or not SkinData.SkinManager.IsValidSkinIndex(SkinData.SkinIndex) or
    (R.Bottom > ItemBmp.Height) or (R.Right > ItemBmp.Width) or (R.Left < 0) or (R.Top < 0) then Exit;

  BG.PleaseDraw := False;  
  GetBGInfo(@BG, Parent);
  CI := BGInfoToCI(@BG);

  PaintItemBG(SkinData, ci, State, R, pP, ItemBmp, OffsetX, OffsetY);
  inc(ci.X, pP.X);
  inc(ci.Y, pP.Y);
  if Assigned(SkinData.SkinManager) and SkinData.SkinManager.IsValidImgIndex(SkinData.BorderIndex) and not ((State = 0) and (SkinData.SkinManager.ma[SkinData.BorderIndex].DrawMode and BDM_ACTIVEONLY = BDM_ACTIVEONLY))
    then DrawSkinRect(ItemBmp, R, Filling, ci, SkinData.SkinManager.ma[SkinData.BorderIndex], State, UpdateCorners);

  with TsSkinManager(SkinData.SkinManager).gd[SkinData.SkinIndex] do begin
    if ImgTL > -1 then DrawSkinGlyph(ItemBmp, Point(R.Left, R.Top), State, 1, TsSkinManager(SkinData.SkinManager).ma[ImgTL], MakeCacheInfo(ItemBmp));
    if ImgTR > -1 then DrawSkinGlyph(ItemBmp, Point(R.Right - WidthOfImage(TsSkinManager(SkinData.SkinManager).ma[ImgTR]), R.Top), State, 1, TsSkinManager(SkinData.SkinManager).ma[ImgTR], MakeCacheInfo(ItemBmp));
    if ImgBL > -1 then DrawSkinGlyph(ItemBmp, Point(0, R.Bottom - HeightOfImage(TsSkinManager(SkinData.SkinManager).ma[ImgBL])), State, 1, TsSkinManager(SkinData.SkinManager).ma[ImgBL], MakeCacheInfo(ItemBmp));
    if ImgBR > -1 then DrawSkinGlyph(ItemBmp, Point(R.Right - WidthOfImage(TsSkinManager(SkinData.SkinManager).ma[ImgBR]), R.Bottom - HeightOfImage(TsSkinManager(SkinData.SkinManager).ma[i])), State, 1, TsSkinManager(SkinData.SkinManager).ma[ImgBR], MakeCacheInfo(ItemBmp));
  end;
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

procedure CopyChannel(const Bmp32, Bmp8 : TBitmap; const Channel : integer; const From32To8 : boolean);
var
  Dst, Src : PByteArray;
  X, Y : integer;
begin
  if From32To8 then for Y := 0 to Bmp32.Height - 1 do begin
    Dst := Bmp8.ScanLine[Y];
    Src := Bmp32.ScanLine[Y];
    for X := 0 to Bmp32.Width - 1 do begin
      Dst[X] := Src[X * 4 + Channel];
    end;
  end
  else for Y := 0 to Bmp32.Height - 1 do begin
    Dst := Bmp32.ScanLine[Y];
    Src := Bmp8.ScanLine[Y];
    for X := 0 to Bmp32.Width - 1 do Dst[X * 4 + Channel] := Src[X];
  end;
end;

procedure PaintControlByTemplate(const DstBmp, SrcBmp : TBitmap; const DstRect, SrcRect, BorderWidths : TRect; const DrawModes : TRect; const StretchCenter : boolean);//TacBorderDrawModes);
var
  X, Y, w, h : integer;
begin
  // Copy corners
  BitBlt(DstBmp.Canvas.Handle, DstRect.Left, DstRect.Top, BorderWidths.Left, BorderWidths.Top,                             // LeftTop
         SrcBmp.Canvas.Handle, SrcRect.Left, SrcRect.Top, SRCCOPY);
  BitBlt(DstBmp.Canvas.Handle, DstRect.Left, DstRect.Bottom - BorderWidths.Bottom, BorderWidths.Left, BorderWidths.Bottom, // LeftBottom
         SrcBmp.Canvas.Handle, SrcRect.Left, SrcRect.Bottom - BorderWidths.Bottom, SRCCOPY);
  BitBlt(DstBmp.Canvas.Handle, DstRect.Right - BorderWidths.Right, DstRect.Top, BorderWidths.Right, BorderWidths.Top,      // RightTop
         SrcBmp.Canvas.Handle, SrcRect.Right - BorderWidths.Right, SrcRect.Top, SRCCOPY);
  BitBlt(DstBmp.Canvas.Handle, DstRect.Right - BorderWidths.Right, DstRect.Bottom - BorderWidths.Bottom, BorderWidths.Right, BorderWidths.Bottom, // RightBottom
         SrcBmp.Canvas.Handle, SrcRect.Right - BorderWidths.Right, SrcRect.Bottom - BorderWidths.Bottom, SRCCOPY);

  w := WidthOf(SrcRect) - BorderWidths.Right - BorderWidths.Left;
  h := HeightOf(SrcRect) - BorderWidths.Bottom - BorderWidths.Top;
  // Left border
  case DrawModes.Left of
    0 : begin
      X := DstRect.Left;
      Y := DstRect.Top + BorderWidths.Top;
      while Y < DstRect.Bottom - BorderWidths.Bottom - h do begin
        BitBlt(DstBmp.Canvas.Handle, X, Y, BorderWidths.Left, h, SrcBmp.Canvas.Handle, SrcRect.Left, SrcRect.Top + BorderWidths.Top, SRCCOPY);
        inc(Y, h);
      end;
      if Y < DstRect.Bottom - BorderWidths.Bottom then begin
        BitBlt(DstBmp.Canvas.Handle, X, Y, BorderWidths.Left, DstRect.Bottom - BorderWidths.Bottom - Y, SrcBmp.Canvas.Handle, SrcRect.Left, SrcRect.Top + BorderWidths.Top, SRCCOPY);
      end;
    end;
    1 : begin
      StretchBlt(DstBmp.Canvas.Handle, DstRect.Left, DstRect.Top + BorderWidths.Top, BorderWidths.Left, DstRect.Bottom - DstRect.Top - BorderWidths.Top - BorderWidths.Bottom,
                 SrcBmp.Canvas.Handle, SrcRect.Left, SrcRect.Top + BorderWidths.Top, BorderWidths.Left, SrcRect.Bottom - SrcRect.Top - BorderWidths.Top - BorderWidths.Bottom, SRCCOPY);
    end;
  end;
  // Top border
  case DrawModes.Top of
    0 : begin
      X := DstRect.Left + BorderWidths.Left;
      Y := DstRect.Top;
      while X < DstRect.Right - BorderWidths.Right - w do begin
        BitBlt(DstBmp.Canvas.Handle, X, Y, w, BorderWidths.Top, SrcBmp.Canvas.Handle, SrcRect.Left + BorderWidths.Left, SrcRect.Top, SRCCOPY);
        inc(X, w);
      end;
      if X < DstRect.Right - BorderWidths.Right then begin
        BitBlt(DstBmp.Canvas.Handle, X, Y, DstRect.Right - BorderWidths.Right - X, BorderWidths.Top, SrcBmp.Canvas.Handle, SrcRect.Left + BorderWidths.Left, SrcRect.Top, SRCCOPY);
      end;
    end;
    1 : begin
      StretchBlt(DstBmp.Canvas.Handle, DstRect.Left + BorderWidths.Left, DstRect.Top, DstRect.Right - DstRect.Top - BorderWidths.Left - BorderWidths.Right, BorderWidths.Top,
                 SrcBmp.Canvas.Handle, SrcRect.Left + BorderWidths.Left, SrcRect.Top, SrcRect.Right - SrcRect.Top - BorderWidths.Left - BorderWidths.Right, BorderWidths.Top, SRCCOPY);
    end;
  end;
  // Right border
  case DrawModes.Right of
    0 : begin
      X := DstRect.Right - BorderWidths.Right;
      Y := DstRect.Top + BorderWidths.Top;
      while Y < DstRect.Bottom - BorderWidths.Bottom - h do begin
        BitBlt(DstBmp.Canvas.Handle, X, Y, BorderWidths.Right, h, SrcBmp.Canvas.Handle, SrcRect.Right - BorderWidths.Right, SrcRect.Top + BorderWidths.Top, SRCCOPY);
        inc(Y, h);
      end;
      if Y < DstRect.Bottom - BorderWidths.Bottom then begin
        BitBlt(DstBmp.Canvas.Handle, X, Y, BorderWidths.Right, DstRect.Bottom - BorderWidths.Bottom - Y, SrcBmp.Canvas.Handle, SrcRect.Right - BorderWidths.Right, SrcRect.Top + BorderWidths.Top, SRCCOPY);
      end;
    end;
    1 : begin
      StretchBlt(DstBmp.Canvas.Handle, DstRect.Right - BorderWidths.Right, DstRect.Top + BorderWidths.Top, BorderWidths.Right, DstRect.Bottom - DstRect.Top - BorderWidths.Bottom - BorderWidths.Top,
                 SrcBmp.Canvas.Handle, SrcRect.Right - BorderWidths.Right, SrcRect.Top + BorderWidths.Top, BorderWidths.Right, SrcRect.Bottom - SrcRect.Top - BorderWidths.Bottom - BorderWidths.Top, SRCCOPY);
    end;
  end;
  // Bottom border
  case DrawModes.Bottom of
    0 : begin
      X := DstRect.Left + BorderWidths.Left;
      Y := DstRect.Bottom - BorderWidths.Bottom;
      while X < DstRect.Right - BorderWidths.Right - w do begin
        BitBlt(DstBmp.Canvas.Handle, X, Y, w, BorderWidths.Bottom, SrcBmp.Canvas.Handle, SrcRect.Left + BorderWidths.Left, SrcRect.Bottom - BorderWidths.Bottom, SRCCOPY);
        inc(X, w);
      end;
      if X < DstRect.Right - BorderWidths.Right then begin
        BitBlt(DstBmp.Canvas.Handle, X, Y, DstRect.Right - BorderWidths.Right - X, BorderWidths.Bottom, SrcBmp.Canvas.Handle, SrcRect.Left + BorderWidths.Left, SrcRect.Bottom - BorderWidths.Bottom, SRCCOPY);
      end;
    end;
    1 : begin
      StretchBlt(DstBmp.Canvas.Handle, DstRect.Left + BorderWidths.Left, DstRect.Bottom - BorderWidths.Bottom, DstRect.Right - DstRect.Left - BorderWidths.Right - BorderWidths.Left, BorderWidths.Bottom,
                 SrcBmp.Canvas.Handle, SrcRect.Left + BorderWidths.Left, SrcRect.Bottom - BorderWidths.Bottom, SrcRect.Right - SrcRect.Left - BorderWidths.Right - BorderWidths.Left, BorderWidths.Bottom, SRCCOPY);
    end;
  end;
  // Center
  case StretchCenter of
    False : begin
      X := DstRect.Left + BorderWidths.Left;
      while X < DstRect.Right - BorderWidths.Right - w do begin
        Y := DstRect.Top + BorderWidths.Top;
        while Y < DstRect.Bottom - BorderWidths.Bottom - h do begin
          BitBlt(DstBmp.Canvas.Handle, X, Y, w, h, SrcBmp.Canvas.Handle, SrcRect.Left + BorderWidths.Left, SrcRect.Top + BorderWidths.Top, SRCCOPY);
          inc(Y, h);
        end;
        if Y < DstRect.Bottom - BorderWidths.Bottom then begin
          BitBlt(DstBmp.Canvas.Handle, X, Y, w, DstRect.Bottom - BorderWidths.Bottom - Y, SrcBmp.Canvas.Handle, SrcRect.Left + BorderWidths.Left, SrcRect.Top + BorderWidths.Top, SRCCOPY);
        end;
        inc(X, w);
      end;
      if X < DstRect.Right - BorderWidths.Right then begin
        Y := DstRect.Top + BorderWidths.Top;
        while Y < DstRect.Bottom - BorderWidths.Bottom - h do begin
          BitBlt(DstBmp.Canvas.Handle, X, Y, DstRect.Right - BorderWidths.Right - X, h, SrcBmp.Canvas.Handle, SrcRect.Left + BorderWidths.Left, SrcRect.Top + BorderWidths.Top, SRCCOPY);
          inc(Y, h);
        end;
        if Y < DstRect.Bottom - BorderWidths.Bottom then begin
          BitBlt(DstBmp.Canvas.Handle, X, Y, DstRect.Right - BorderWidths.Right - X, DstRect.Bottom - BorderWidths.Bottom - Y, SrcBmp.Canvas.Handle, SrcRect.Left + BorderWidths.Left, SrcRect.Top + BorderWidths.Top, SRCCOPY);
        end;
      end;
    end;
    True : begin
      StretchBlt(DstBmp.Canvas.Handle, DstRect.Left + BorderWidths.Left, DstRect.Top + BorderWidths.Top, DstRect.Right - DstRect.Left - BorderWidths.Right - BorderWidths.Left, DstRect.Bottom - DstRect.Top - BorderWidths.Bottom - BorderWidths.Top,
                 SrcBmp.Canvas.Handle, SrcRect.Left + BorderWidths.Left, SrcRect.Top + BorderWidths.Top, SrcRect.Right - SrcRect.Left - BorderWidths.Right - BorderWidths.Left, SrcRect.Bottom - SrcRect.Top - BorderWidths.Bottom - BorderWidths.Top, SRCCOPY);
    end;
  end;
end;

procedure DrawGlyphEx(Glyph, DstBmp : TBitmap; R : TRect; NumGlyphs : integer; Enabled : boolean; DisabledGlyphKind : TsDisabledGlyphKind; State, Blend : integer; Down : boolean = False);
var
  Bmp, TmpGlyph : TBitmap;
  MaskColor: TsColor;
  w, GlyphIndex : integer;
  CI : TCacheInfo;
begin
  TmpGlyph := TBitmap.Create;
  TmpGlyph.Assign(Glyph);
  TmpGlyph.PixelFormat := pf24bit;
  case NumGlyphs of
    1 : begin
      Bmp := TBitmap.Create;
      Bmp.Assign(TmpGlyph);
      Bmp.PixelFormat := pf24bit;
      Bmp.TransparentColor := Bmp.Canvas.Pixels[0, Bmp.Height - 1];
      try if not Enabled then begin
        if dgGrayed in DisabledGlyphKind then GrayScale(Bmp);
        if dgBlended in DisabledGlyphKind then begin
          MaskColor := TsColor(Bmp.Canvas.Pixels[0, Bmp.Height - 1]);
          BlendTransRectangle(DstBmp, R.Left, R.Top, Bmp, Rect(0, 0, WidthOf(R), HeightOf(R)), 0.5, MaskColor);
        end
        else begin
          MaskColor := TsColor(Bmp.Canvas.Pixels[0, Bmp.Height - 1]);
          CopyTransBitmaps(DstBmp, Bmp, R.Left, R.Top, MaskColor);
        end;
      end
      else begin
        MaskColor := TsColor(Bmp.Canvas.Pixels[0, Bmp.Height - 1]);

        if (State = 0) and (Blend > 0)
          then BlendTransRectangle(DstBmp, R.Left, R.Top, Bmp, Rect(0, 0, WidthOf(R), HeightOf(R)), Blend / 100, MaskColor)
          else CopyTransBitmaps(DstBmp, Bmp, R.Left, R.Top, MaskColor);
      end;
      finally
        FreeAndNil(Bmp);
      end;
    end
    else begin
      if not Enabled then GlyphIndex := min(NumGlyphs, 1) else begin
        case State of
          0 : GlyphIndex := 0;
          1 : if NumGlyphs > 4 then GlyphIndex := 4 else GlyphIndex := 0;
          2 : begin
            if Down and (NumGlyphs > 3) then GlyphIndex := 3 else if (NumGlyphs > 2) then GlyphIndex := 2 else GlyphIndex := 0;
          end;
        end;
      end;
      w := TmpGlyph.Width div NumGlyphs;
      CI := MakeCacheInfo(DstBmp);
      if Enabled then begin
        CopyTransRectA(DstBmp, TmpGlyph, R.Left, R.Top, Rect(w * GlyphIndex, 0, (GlyphIndex + 1) * w - 1, TmpGlyph.Height - 1), TmpGlyph.Canvas.Pixels[GlyphIndex * w, TmpGlyph.Height - 1], CI);
      end
      else begin
        if (State = 0) and (Blend > 0) then begin
          MaskColor := TsColor(TmpGlyph.Canvas.Pixels[0, TmpGlyph.Height - 1]);
          BlendTransRectangle(DstBmp, R.Left, R.Top, TmpGlyph, Rect(w * GlyphIndex, 0, (GlyphIndex + 1) * w - 1, TmpGlyph.Height - 1), Blend / 100, MaskColor);
        end
        else begin
          CopyTransRectA(DstBmp, TmpGlyph, R.Left, R.Top, Rect(w * GlyphIndex, 0, (GlyphIndex + 1) * w - 1, TmpGlyph.Height - 1), TmpGlyph.Canvas.Pixels[0, TmpGlyph.Height - 1], CI);
        end;
      end;
    end;
  end;
  FreeAndNil(TmpGlyph);
end;
{$ENDIF}

procedure FillDC(const DC: HDC; const aRect: TRect; const Color: TColor);
var
  OldBrush, NewBrush : hBrush;
  SavedDC : hWnd;
begin
  SavedDC := SaveDC(DC);
  NewBrush := CreateSolidBrush(cardinal(ColorToRGB(Color)));
  OldBrush := SelectObject(dc, NewBrush);
  try
    FillRect(DC, aRect, NewBrush);
  finally
    SelectObject(dc, OldBrush);
    DeleteObject(NewBrush);
    RestoreDC(DC, SavedDC);
  end;
end;

procedure FillDCBorder(const DC: HDC; const aRect: TRect; const wl, wt, wr, wb : integer; const Color: TColor);
var
  OldBrush, NewBrush : hBrush;
  SavedDC : hWnd;
begin
  SavedDC := SaveDC(DC);
  NewBrush := CreateSolidBrush(Cardinal(ColorToRGB(Color)));
  OldBrush := SelectObject(dc, NewBrush);
  try
    FillRect(DC, Rect(aRect.Left, aRect.Top, aRect.Right, aRect.Top + wt), NewBrush);
    FillRect(DC, Rect(aRect.Left, aRect.Top + wt, aRect.Left + wl, aRect.Bottom), NewBrush);
    FillRect(DC, Rect(aRect.Left + wl, aRect.Bottom - wb, aRect.Right, aRect.Bottom), NewBrush);
    FillRect(DC, Rect(aRect.Right - wr, aRect.Top + wt, aRect.Right, aRect.Bottom - wb), NewBrush);
  finally
    SelectObject(dc, OldBrush);
    DeleteObject(NewBrush);
    RestoreDC(DC, SavedDC);
  end;
end;

procedure BitBltBorder(const DestDC: HDC; const X, Y, Width, Height: Integer; const SrcDC: HDC; const XSrc, YSrc: Integer; const BorderWidth : integer);
begin
  BitBlt(DestDC, X, Y, BorderWidth, Height, SrcDC, XSrc, YSrc, SRCCOPY);
  BitBlt(DestDC, X + BorderWidth, Y, Width, BorderWidth, SrcDC, XSrc + BorderWidth, YSrc, SRCCOPY);
  BitBlt(DestDC, Width - BorderWidth, Y + BorderWidth, Width, Height, SrcDC, XSrc + Width - BorderWidth, YSrc + BorderWidth, SRCCOPY);
  BitBlt(DestDC, X + BorderWidth, Height - BorderWidth, Width - BorderWidth, Height, SrcDC, XSrc + BorderWidth, YSrc + Height - BorderWidth, SRCCOPY);
end;

procedure ExcludeControls(const DC : hdc; const Ctrl : TWinControl; const CtrlType : TacCtrlType; const OffsetX : integer; const OffsetY : integer);
var
  i : integer;
  Child : TControl;
begin
  for i := 0 to Ctrl.ControlCount - 1 do begin
    Child := Ctrl.Controls[i];
    if (Child is TGraphicControl) and StdTransparency {$IFNDEF ALITE} or (Child is TsSplitter) {$ENDIF} then Continue;
    if Child.Visible then begin
      if (Child is TGraphicControl) then begin
        ExcludeClipRect(DC, Child.Left + OffsetX, Child.Top + OffsetY, Child.Left + Child.Width + OffsetX, Child.Top + Child.Height + OffsetY);
      end;
    end;
  end;
end;

procedure GrayScale(Bmp: TBitmap);
var
  p : PByteArray;
  Gray, x, y, w, h : integer;
begin
  h := Bmp.Height - 1;
  w := Bmp.Width - 1;
  for y := 0 to h do begin
    p := Bmp.scanline[y];
    for x := 0 to w do begin
      Gray := (p[x * 3] + p[x * 3 + 1] + p[x * 3 + 2]) div 3;
      p[x * 3 + 0] := Gray; p[x * 3 + 1] := Gray; p[x * 3 + 2] := Gray
    end
  end;
end;

procedure GrayScaleTrans(Bmp: TBitmap; const TransColor : TsColor);
var
  S1 : PRGBArray;
  Gray, x, y, w, h : integer;
begin
  h := Bmp.Height - 1;
  w := Bmp.Width - 1;
  for Y := 0 to h do begin
    S1 := Bmp.ScanLine[Y];
    for X := 0 to w do begin
      if (S1[X].B <> TransColor.B) or (S1[X].G <> TransColor.G) or (S1[X].R <> TransColor.R) then begin
        Gray := (S1[X].R + S1[X].G + S1[X].B) div 3;
        S1[X].R := Gray; S1[X].G := Gray; S1[X].B := Gray
      end;
    end
  end;
end;

function CutText(Canvas: TCanvas; const Text: string; MaxLength : integer): string;
begin
  if MaxLength < 1 then Result := '' else Result := Text;
  if (Canvas.TextWidth(Result) > MaxLength) and (Result <> '') then begin
    while (Result <> '') and (Canvas.TextWidth(Result + '...') > MaxLength) do Delete(Result, Length(Result), 1);
    if Result <> '' then Result := Result + '...';
  end;
end;

procedure WriteText(Canvas: TCanvas; Text: PChar; Enabled: boolean; var aRect : TRect; Flags: Cardinal);
var
  R, Rd: TRect;
  x, y : integer;
  ts: TSize;
begin
  R := aRect;

  if Flags or DT_WORDBREAK <> Flags then begin // If no multiline

    GetTextExtentPoint32(Canvas.Handle, Text, Length(Text), ts);
    R.Right := R.Left + ts.cx;
    R.Bottom := R.Top + ts.cy;

    if Flags or DT_CENTER = Flags then begin
      y := (HeightOf(R) - HeightOf(aRect)) div 2;
      x := (WidthOf(R) - WidthOf(aRect)) div 2;
      InflateRect(aRect, x, y);
    end
    else if Flags or DT_RIGHT = Flags then begin
      y := (HeightOf(R) - HeightOf(aRect)) div 2;
      dec(aRect.Top, y);
      inc(aRect.Bottom, y);
      inc(aRect.Left, WidthOf(aRect) - WidthOf(R));
    end
    else if Flags or DT_LEFT = Flags then begin
      y := (HeightOf(R) - HeightOf(aRect)) div 2;
      dec(aRect.Top, y);
      inc(aRect.Bottom, y);
      inc(aRect.Right, WidthOf(R) - WidthOf(aRect));
    end;


    R := aRect;// := R;
    InflateRect(aRect, 1, 1);
  end;

  Canvas.Brush.Style := bsClear;
  if Text <> ''then
  if Enabled then begin
    DrawText(Canvas.Handle, Text, Length(Text), R, Flags);
  end
  else begin
    Rd := Rect(R.Left + 1, R.Top + 1, R.Right + 1, R.Bottom + 1);
    Canvas.Font.Color := ColorToRGB(clBtnHighlight);
    DrawText(Canvas.Handle, Text, Length(Text), Rd, Flags);

    Canvas.Font.Color := ColorToRGB(clBtnShadow);
    DrawText(Canvas.Handle, Text, Length(Text), R, Flags);
  end;
end;

procedure WriteTextOnDC(DC: hdc; Text: PChar; Enabled: boolean; var aRect : TRect; Flags: Cardinal);
var
  R, Rd: TRect;
  x, y : integer;
  ts: TSize;
begin
  R := aRect;
  SetBkMode(DC, TRANSPARENT);

  if Flags or DT_WORDBREAK <> Flags then begin // If no multiline

    GetTextExtentPoint32(DC, Text, Length(Text), ts);
    R.Right := R.Left + ts.cx;
    R.Bottom := R.Top + ts.cy;

    if Flags or DT_CENTER = Flags then begin
      y := (HeightOf(R) - HeightOf(aRect)) div 2;
      x := (WidthOf(R) - WidthOf(aRect)) div 2;
      InflateRect(aRect, x, y);
    end
    else if Flags or DT_RIGHT = Flags then begin
      y := (HeightOf(R) - HeightOf(aRect)) div 2;
      dec(aRect.Top, y);
      inc(aRect.Bottom, y);
      inc(aRect.Left, WidthOf(aRect) - WidthOf(R));
    end
    else if Flags or DT_LEFT = Flags then begin
      y := (HeightOf(R) - HeightOf(aRect)) div 2;
      dec(aRect.Top, y);
      inc(aRect.Bottom, y);
      inc(aRect.Right, WidthOf(R) - WidthOf(aRect));
    end;


    R := aRect;
    InflateRect(aRect, 1, 1);
  end;

  if Text <> ''then
  if Enabled then begin
    DrawText(DC, Text, Length(Text), R, Flags);
  end
  else begin
    Rd := Rect(R.Left + 1, R.Top + 1, R.Right + 1, R.Bottom + 1);
    DrawText(DC, Text, Length(Text), Rd, Flags);
    DrawText(DC, Text, Length(Text), R, Flags);
  end;
end;

function acDrawText(hDC: HDC; const Text: ACString; var lpRect: TRect; uFormat: Cardinal): Integer;
begin
{$IFDEF TNTUNICODE}
  Result := Tnt_DrawTextW(hDC, PACChar(Text), Length(Text), lpRect, uFormat);
{$else}
  Result := DrawText(hDC, PACChar(Text), Length(Text), lpRect, uFormat);
{$ENDIF}
end;

function acTextWidth(const Canvas: TCanvas; const Text: ACString): Integer;
begin
{$IFDEF TNTUNICODE}
  Result := WideCanvasTextExtent(Canvas, Text).cx;
{$ELSE}
  Result := Canvas.TextExtent(Text).cx;
{$ENDIF}
end;

function acTextHeight(const Canvas: TCanvas; const Text: ACString): Integer;
begin
{$IFDEF TNTUNICODE}
  Result := WideCanvasTextExtent(Canvas, Text).cy;
{$ELSE}
  Result := Canvas.TextExtent(Text).cy;
{$ENDIF}
end;

function acTextExtent(const Canvas: TCanvas; const Text: ACString): TSize;
begin
{$IFDEF TNTUNICODE}
  Result := WideCanvasTextExtent(Canvas, Text);
{$ELSE}
  Result := Canvas.TextExtent(Text);
{$ENDIF}
end;

procedure acTextRect(const Canvas : TCanvas; const Rect: TRect; X, Y: Integer; const Text: ACString);
begin
{$IFDEF TNTUNICODE}
  WideCanvasTextRect(Canvas, Rect, X, Y, Text);
{$ELSE}
  Canvas.TextRect(Rect, X, Y, Text);
{$ENDIF}
end;

procedure acWriteTextEx(const Canvas: TCanvas; Text: PacChar; Enabled: boolean; var aRect : TRect; Flags: Cardinal; SkinData : TsCommonData; Hot : boolean; SkinManager : TObject = nil);
begin
{$IFDEF TNTUNICODE}
  WriteTextExW(Canvas, Text, Enabled, aRect, Flags, SkinData, Hot);
{$ELSE}
  WriteTextEx(Canvas, Text, Enabled, aRect, Flags, SkinData, Hot);
{$ENDIF}
end;

procedure acWriteTextEx(const Canvas: TCanvas; Text: PacChar; Enabled: boolean; var aRect : TRect; Flags: Cardinal; SkinIndex : integer; Hot : boolean; SkinManager : TObject = nil);
begin
{$IFDEF TNTUNICODE}
  WriteTextExW(Canvas, Text, Enabled, aRect, Flags, SkinIndex, Hot, SkinManager);
{$ELSE}
  WriteTextEx(Canvas, Text, Enabled, aRect, Flags, SkinIndex, Hot, SkinManager);
{$ENDIF}
end;

procedure acWriteText(const Canvas: TCanvas; Text: PacChar; Enabled: boolean; var aRect : TRect; Flags: Cardinal);
begin
{$IFDEF TNTUNICODE}
  DrawTextW(Canvas.Handle, Text, Length(Text), aRect, Flags);
{$ELSE}
  DrawText(Canvas.Handle, Text, Length(Text), aRect, Flags);
{$ENDIF}
end;

{$IFNDEF ACHINTS}
procedure WriteTextEx(const Canvas: TCanvas; Text: PChar; Enabled: boolean; var aRect : TRect; Flags: Cardinal; SkinIndex : integer; Hot : boolean; SkinManager : TObject = nil); overload;
var
  R, Rd: TRect;
  nLength: Integer;
begin
  nLength := StrLen(Text);
  if SkinManager = nil then SkinManager := DefaultManager;
  if not Assigned(DefaultManager) or not TsSkinManager(SkinManager).IsValidSkinIndex(SkinIndex) then Exit;
  with TsSkinManager(SkinManager) do begin
    Canvas.Brush.Style := bsClear;
    R := aRect;
    if Text <> '' then
      if Enabled then begin
        if IsValidSkinIndex(SkinIndex) then begin
          // Left
          if Hot then Canvas.Font.Color := gd[SkinIndex].HotFontColor[2] else Canvas.Font.Color := gd[SkinIndex].FontColor[2];
          if Canvas.Font.Color <> -1 then begin
            Rd := Rect(R.Left - 1, R.Top, R.Right - 1, R.Bottom);
            DrawText(Canvas.Handle, Text, nLength, Rd, Flags);
          end;
          // Top
          if Hot then Canvas.Font.Color := gd[SkinIndex].HotFontColor[3] else Canvas.Font.Color := gd[SkinIndex].FontColor[3];
          if Canvas.Font.Color <> -1 then begin
            Rd := Rect(R.Left, R.Top - 1, R.Right, R.Bottom - 1);
            DrawText(Canvas.Handle, Text, nLength, Rd, Flags);
          end;
          // Right
          if Hot then Canvas.Font.Color := gd[SkinIndex].HotFontColor[4] else Canvas.Font.Color := gd[SkinIndex].FontColor[4];
          if Canvas.Font.Color <> -1 then begin
            Rd := Rect(R.Left + 1, R.Top, R.Right + 1, R.Bottom);
            DrawText(Canvas.Handle, Text, nLength, Rd, Flags);
          end;
          // Bottom
          if Hot then Canvas.Font.Color := gd[SkinIndex].HotFontColor[5] else Canvas.Font.Color := gd[SkinIndex].FontColor[5];
          if Canvas.Font.Color <> -1 then begin
            Rd := Rect(R.Left, R.Top + 1, R.Right, R.Bottom + 1);
            DrawText(Canvas.Handle, Text, nLength, Rd, Flags);
          end;
          // Center
          if Hot then Canvas.Font.Color := gd[SkinIndex].HotFontColor[1] else Canvas.Font.Color := gd[SkinIndex].FontColor[1];
          DrawText(Canvas.Handle, Text, nLength, R, Flags);
        end
        else DrawText(Canvas.Handle, Text, nLength, R, Flags);
      end
      else begin
        Rd := R;
        Canvas.Font.Color := MixColors(gd[SkinIndex].FontColor[1], gd[SkinIndex].Color, DefDisabledBlend);
        DrawText(Canvas.Handle, Text, nLength, Rd, Flags);
      end;
  end
end;
{$ENDIF}

procedure WriteTextEx(const Canvas: TCanvas; Text: PChar; Enabled: boolean; var aRect : TRect; Flags: Cardinal; SkinData : TsCommonData; Hot : boolean); overload;
var
  R, Rd: TRect;
  SavedDC : hdc;
  nLength : Integer;
  SkinIndex : integer;
  Transparency : integer;
begin
  nLength := StrLen(Text);

  R := aRect;
  if Assigned(SkinData.FOwnerControl) then Canvas.Font.Assign(TsHackedControl(SkinData.FOwnerControl).Font);
  if Hot and (SkinData.SkinSection = s_WebBtn)
    then Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];

  SavedDC := SaveDC(Canvas.Handle);
  try
    IntersectClipRect(Canvas.Handle, aRect.Left, aRect.Top, aRect.Right, aRect.Bottom);

    if Text <> '' then begin
      if (SkinData.FOwnerControl <> nil) and ((SkinData.COC in [COC_TsGroupBox, COC_TsCheckBox, COC_TsRadioButton]) or (SkinData.FOwnerControl is TGraphicControl)) and
            TsHackedControl(SkinData.FOwnerControl).ParentFont and (SkinData.FOwnerControl.Parent <> nil) then begin
        if Hot then Transparency := SkinData.SkinManager.gd[SkinData.SkinIndex].HotTransparency else Transparency := SkinData.SkinManager.gd[SkinData.SkinIndex].Transparency;
        if Transparency > 0 then begin
          SkinIndex := GetFontIndex(SkinData.FOwnerControl, SkinData.SkinIndex, SkinData.SkinManager);
        end
        else SkinIndex := SkinData.SkinIndex;
      end
      else SkinIndex := SkinData.SkinIndex;
      Canvas.Brush.Style := bsClear;
      if Enabled then begin
        if Assigned(SkinData.SkinManager) and SkinData.SkinManager.IsValidSkinIndex(SkinIndex) then begin
          // Left contur
          if not SkinData.CustomFont then begin
            if Hot then Canvas.Font.Color := SkinData.SkinManager.gd[SkinIndex].HotFontColor[2] else Canvas.Font.Color := SkinData.SkinManager.gd[SkinIndex].FontColor[2];
            if Canvas.Font.Color <> -1 then begin
              Rd := Rect(R.Left - 1, R.Top, R.Right - 1, R.Bottom);
              DrawText(Canvas.Handle, Text, nLength, Rd, Flags);
            end;
            // Top
            if Hot then Canvas.Font.Color := SkinData.SkinManager.gd[SkinIndex].HotFontColor[3] else Canvas.Font.Color := SkinData.SkinManager.gd[SkinIndex].FontColor[3];
            if Canvas.Font.Color <> -1 then begin
              Rd := Rect(R.Left, R.Top - 1, R.Right, R.Bottom - 1);
              DrawText(Canvas.Handle, Text, nLength, Rd, Flags);
            end;
            // Right
            if Hot then Canvas.Font.Color := SkinData.SkinManager.gd[SkinIndex].HotFontColor[4] else Canvas.Font.Color := SkinData.SkinManager.gd[SkinIndex].FontColor[4];
            if Canvas.Font.Color <> -1 then begin
              Rd := Rect(R.Left + 1, R.Top, R.Right + 1, R.Bottom);
              DrawText(Canvas.Handle, Text, nLength, Rd, Flags);
            end;
            // Bottom
            if Hot then Canvas.Font.Color := SkinData.SkinManager.gd[SkinIndex].HotFontColor[5] else Canvas.Font.Color := SkinData.SkinManager.gd[SkinIndex].FontColor[5];
            if Canvas.Font.Color <> -1 then begin
              Rd := Rect(R.Left, R.Top + 1, R.Right, R.Bottom + 1);
              DrawText(Canvas.Handle, Text, nLength, Rd, Flags);
            end;
          end;
          // Center
          if not SkinData.CustomFont then begin
            if Hot then Canvas.Font.Color := SkinData.SkinManager.gd[SkinIndex].HotFontColor[1] else Canvas.Font.Color := SkinData.SkinManager.gd[SkinIndex].FontColor[1];
          end;
          DrawText(Canvas.Handle, Text, nLength, R, Flags or DT_NOCLIP);
        end
        else DrawText(Canvas.Handle, Text, nLength, R, Flags or DT_NOCLIP);
      end
      else if SkinIndex > -1 then begin
        try
          Canvas.Font.Color := MixColors(SkinData.SkinManager.gd[SkinIndex].FontColor[1], SkinData.SkinManager.GetGlobalColor, DefDisabledBlend);
          DrawText(Canvas.Handle, Text, nLength, R, Flags);
        except
        end;
      end
      else begin
        DrawText(Canvas.Handle, Text, nLength, R, Flags);
      end;
//      aRect := R;
    end;
  finally
    RestoreDC(Canvas.Handle, SavedDC);
  end;
end;

{$IFDEF TNTUNICODE}
procedure WriteTextExW(const Canvas: TCanvas; Text: PWideChar; Enabled: boolean; var aRect : TRect; Flags: Cardinal; SkinData : TsCommonData; Hot : boolean); overload;
var
  R, Rd: TRect;
//  ts: TSize;
  SavedDC : hdc;
  nLength: Integer;
begin
  {$IFDEF D2005}
  nLength := Length(Text);
  {$ELSE}
  nLength := WStrLen(Text);
  {$ENDIF}

  R := aRect;
  if Assigned(SkinData.FOwnerControl) then Canvas.Font.Assign(TsHackedControl(SkinData.FOwnerControl).Font);
  if Hot and (SkinData.SkinSection = s_WebBtn) then Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];

  SavedDC := SaveDC(Canvas.Handle);
  try
  IntersectClipRect(Canvas.Handle, aRect.Left, aRect.Top, aRect.Right, aRect.Bottom); // v4.40
{
  if (Flags or DT_WORDBREAK <> Flags) and (Flags or DT_END_ELLIPSIS <> Flags) then begin // If no multiline

    GetTextExtentPoint32W(Canvas.Handle, Text, nLength, ts);
    R.Right := R.Left + ts.cx;
    R.Bottom := R.Top + ts.cy;

    if Flags or DT_CENTER = Flags then begin
      y := (HeightOf(R) - HeightOf(aRect)) div 2;
      x := (WidthOf(R) - WidthOf(aRect)) div 2;
      InflateRect(aRect, x, y);
    end
    else if Flags or DT_RIGHT = Flags then begin
      y := (HeightOf(R) - HeightOf(aRect)) div 2;
      dec(aRect.Top, y);
      inc(aRect.Bottom, y);
      inc(aRect.Left, WidthOf(aRect) - WidthOf(R));
    end
    else if Flags or DT_LEFT = Flags then begin
      y := (HeightOf(R) - HeightOf(aRect)) div 2;
      dec(aRect.Top, y);
      inc(aRect.Bottom, y);
      inc(aRect.Right, WidthOf(R) - WidthOf(aRect));
    end;

    R := aRect;// := R;
    InflateRect(aRect, 1, 1);
  end;
}
  Canvas.Brush.Style := bsClear;
  if Text <> '' then
    if Enabled then begin
      if Assigned(SkinData.SkinManager) and SkinData.SkinManager.IsValidSkinIndex(SkinData.SkinIndex) then begin
        // Left contur
        if not SkinData.CustomFont then begin
          if Hot then Canvas.Font.Color := SkinData.SkinManager.gd[SkinData.SkinIndex].HotFontColor[2] else Canvas.Font.Color := SkinData.SkinManager.gd[SkinData.SkinIndex].FontColor[2];
          if Canvas.Font.Color <> -1 then begin
            Rd := Rect(R.Left - 1, R.Top, R.Right - 1, R.Bottom);
            Tnt_DrawTextW(Canvas.Handle, Text, nLength, Rd, Flags);
          end;
          // Top
          if Hot then Canvas.Font.Color := SkinData.SkinManager.gd[SkinData.SkinIndex].HotFontColor[3] else Canvas.Font.Color := SkinData.SkinManager.gd[SkinData.SkinIndex].FontColor[3];
          if Canvas.Font.Color <> -1 then begin
            Rd := Rect(R.Left, R.Top - 1, R.Right, R.Bottom - 1);
            Tnt_DrawTextW(Canvas.Handle, Text, nLength, Rd, Flags);
          end;
          // Right
          if Hot then Canvas.Font.Color := SkinData.SkinManager.gd[SkinData.SkinIndex].HotFontColor[4] else Canvas.Font.Color := SkinData.SkinManager.gd[SkinData.SkinIndex].FontColor[4];
          if Canvas.Font.Color <> -1 then begin
            Rd := Rect(R.Left + 1, R.Top, R.Right + 1, R.Bottom);
            Tnt_DrawTextW(Canvas.Handle, Text, nLength, Rd, Flags);
          end;
          // Bottom
          if Hot then Canvas.Font.Color := SkinData.SkinManager.gd[SkinData.SkinIndex].HotFontColor[5] else Canvas.Font.Color := SkinData.SkinManager.gd[SkinData.SkinIndex].FontColor[5];
          if Canvas.Font.Color <> -1 then begin
            Rd := Rect(R.Left, R.Top + 1, R.Right, R.Bottom + 1);
            Tnt_DrawTextW(Canvas.Handle, Text, nLength, Rd, Flags);
          end;
        end;
        // Center
        if not SkinData.CustomFont then begin
          if Hot then Canvas.Font.Color := SkinData.SkinManager.gd[SkinData.SkinIndex].HotFontColor[1] else Canvas.Font.Color := SkinData.SkinManager.gd[SkinData.SkinIndex].FontColor[1];
        end;
        Tnt_DrawTextW(Canvas.Handle, Text, nLength, R, Flags or DT_NOCLIP);
      end
      else
        Tnt_DrawTextW(Canvas.Handle, Text, nLength, R, Flags);
    end
    else begin
      Rd := Rect(R.Left + 1, R.Top + 1, R.Right + 1, R.Bottom + 1);
      Canvas.Font.Color := ColorToRGB(clBtnHighlight);
      Tnt_DrawTextW(Canvas.Handle, Text, nLength, Rd, Flags);
      Canvas.Font.Color := ColorToRGB(clBtnShadow);
      Tnt_DrawTextW(Canvas.Handle, Text, nLength, R, Flags);
    end;
  finally
    RestoreDC(Canvas.Handle, SavedDC);
  end;
end;

procedure WriteUnicode(const Canvas: TCanvas; const Text: WideString; Enabled: boolean; var aRect : TRect; Flags: Cardinal; SkinData : TsCommonData; Hot : boolean); overload;
var
  R, Rd: TRect;
  x, y : integer;
  ts: TSize;
  SavedDC : hdc;
  nLength: Integer;
begin
  nLength := Length(Text);

  R := aRect;
  if Assigned(SkinData.FOwnerControl) then Canvas.Font.Assign(TsHackedControl(SkinData.FOwnerControl).Font);
  if Hot and (SkinData.SkinSection = 'WEBBUTTON') then Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];

  SavedDC := SaveDC(Canvas.Handle);
  try
  IntersectClipRect(Canvas.Handle, aRect.Left, aRect.Top, aRect.Right, aRect.Bottom); // v4.40

  if Flags or DT_WORDBREAK <> Flags then begin // If no multiline

    {
    ts.cx := WideCanvasTextWidth(Canvas, Text);
    ts.cy := WideCanvasTextHeight(Canvas, Text);
    }
    ts := WideCanvasTextExtent(Canvas, Text);

    R.Right := R.Left + ts.cx;
    R.Bottom := R.Top + ts.cy;

    if Flags or DT_CENTER = Flags then begin
      y := (HeightOf(R) - HeightOf(aRect)) div 2;
      x := (WidthOf(R) - WidthOf(aRect)) div 2;
      InflateRect(aRect, x, y);
    end
    else if Flags or DT_RIGHT = Flags then begin
      y := (HeightOf(R) - HeightOf(aRect)) div 2;
      dec(aRect.Top, y);
      inc(aRect.Bottom, y);
      inc(aRect.Left, WidthOf(aRect) - WidthOf(R));
    end
    else if Flags or DT_LEFT = Flags then begin
      y := (HeightOf(R) - HeightOf(aRect)) div 2;
      dec(aRect.Top, y);
      inc(aRect.Bottom, y);
      inc(aRect.Right, WidthOf(R) - WidthOf(aRect));
    end;

    R := aRect;// := R;
    InflateRect(aRect, 1, 1);
  end;

  Canvas.Brush.Style := bsClear;
  Flags := ETO_CLIPPED or Flags;// or DT_NOCLIP;
  if Text <> '' then
    if Enabled then begin
      if Assigned(SkinData.SkinManager) and SkinData.SkinManager.IsValidSkinIndex(SkinData.SkinIndex) then begin
        // Left contur
        if not SkinData.CustomFont then begin
          if Hot then Canvas.Font.Color := SkinData.SkinManager.gd[SkinData.SkinIndex].HotFontColor[2] else Canvas.Font.Color := SkinData.SkinManager.gd[SkinData.SkinIndex].FontColor[2];
          if Canvas.Font.Color <> -1 then begin
            Rd := Rect(R.Left - 1, R.Top, R.Right - 1, R.Bottom);
            Windows.ExtTextOutW(Canvas.Handle, Rd.Left, Rd.Top, Flags, @Rd, PWideChar(Text), nLength, nil)
          end;
          // Top
          if Hot then Canvas.Font.Color := SkinData.SkinManager.gd[SkinData.SkinIndex].HotFontColor[3] else Canvas.Font.Color := SkinData.SkinManager.gd[SkinData.SkinIndex].FontColor[3];
          if Canvas.Font.Color <> -1 then begin
            Rd := Rect(R.Left, R.Top - 1, R.Right, R.Bottom - 1);
            Windows.ExtTextOutW(Canvas.Handle, Rd.Left, Rd.Top, Flags, @Rd, PWideChar(Text), nLength, nil)
          end;
          // Right
          if Hot then Canvas.Font.Color := SkinData.SkinManager.gd[SkinData.SkinIndex].HotFontColor[4] else Canvas.Font.Color := SkinData.SkinManager.gd[SkinData.SkinIndex].FontColor[4];
          if Canvas.Font.Color <> -1 then begin
            Rd := Rect(R.Left + 1, R.Top, R.Right + 1, R.Bottom);
            Windows.ExtTextOutW(Canvas.Handle, Rd.Left, Rd.Top, Flags, @Rd, PWideChar(Text), nLength, nil)
          end;
          // Bottom
          if Hot then Canvas.Font.Color := SkinData.SkinManager.gd[SkinData.SkinIndex].HotFontColor[5] else Canvas.Font.Color := SkinData.SkinManager.gd[SkinData.SkinIndex].FontColor[5];
          if Canvas.Font.Color <> -1 then begin
            Rd := Rect(R.Left, R.Top + 1, R.Right, R.Bottom + 1);
            Windows.ExtTextOutW(Canvas.Handle, Rd.Left, Rd.Top, Flags, @Rd, PWideChar(Text), nLength, nil)
          end;
        end;
        // Center
        if not SkinData.CustomFont then begin
          if Hot then Canvas.Font.Color := SkinData.SkinManager.gd[SkinData.SkinIndex].HotFontColor[1] else Canvas.Font.Color := SkinData.SkinManager.gd[SkinData.SkinIndex].FontColor[1];
        end;
//        WideCanvasTextRect(Canvas, R, R.Left, R.Top, Text);
        Windows.ExtTextOutW(Canvas.Handle, R.Left, R.Top, Flags, @R, PWideChar(Text), nLength, nil)
      end
      else
        Windows.ExtTextOutW(Canvas.Handle, R.Left, R.Top, Flags, @R, PWideChar(Text), nLength, nil)
    end
    else begin
      Canvas.Font.Color := MixColors(SkinData.SkinManager.gd[SkinData.SkinIndex].FontColor[1], SkinData.SkinManager.gd[SkinData.SkinIndex].Color, DefDisabledBlend);
      Windows.ExtTextOutW(Canvas.Handle, R.Left, R.Top, Flags, @R, PWideChar(Text), nLength, nil)
    end;
  finally
    RestoreDC(Canvas.Handle, SavedDC);
  end;
end;

procedure TextRectW(const Canvas : TCanvas; var Rect: TRect; X, Y: Integer; const Text: WideString);
begin
  WideCanvasTextRect(Canvas, Rect, X, Y, Text);
end;

procedure WriteTextExW(const Canvas: TCanvas; Text: PWideChar; Enabled: boolean; var aRect : TRect; Flags: Cardinal; SkinIndex : integer; Hot : boolean; SkinManager : TObject = nil);
var
  R, Rd: TRect;
  x, y : integer;
  ts: TSize;
  nLength: Integer;
begin
  nLength := {$IFNDEF D2006}WStrLen(Text){$ELSE}Length(Text){$ENDIF};

  if SkinManager = nil then SkinManager := DefaultManager;
  if not Assigned(DefaultManager) or not TsSkinManager(SkinManager).IsValidSkinIndex(SkinIndex) then Exit;
  with TsSkinManager(SkinManager) do begin {SeeLater}

    R := aRect;

    if (Flags or DT_WORDBREAK <> Flags) and (Flags or DT_END_ELLIPSIS <> Flags) then begin // If no multiline

      GetTextExtentPoint32W(Canvas.Handle, Text, nLength, ts);
      R.Right := R.Left + ts.cx;
      R.Bottom := R.Top + ts.cy;

      if Flags or DT_CENTER = Flags then begin
        y := (HeightOf(R) - HeightOf(aRect)) div 2;
        x := (WidthOf(R) - WidthOf(aRect)) div 2;
        InflateRect(aRect, x, y);
      end
      else if Flags or DT_RIGHT = Flags then begin
        y := (HeightOf(R) - HeightOf(aRect)) div 2;
        dec(aRect.Top, y);
        inc(aRect.Bottom, y);
        inc(aRect.Left, WidthOf(aRect) - WidthOf(R));
      end
      else if Flags or DT_LEFT = Flags then begin
        y := (HeightOf(R) - HeightOf(aRect)) div 2;
        dec(aRect.Top, y);
        inc(aRect.Bottom, y);
        inc(aRect.Right, WidthOf(R) - WidthOf(aRect));
      end;

      R := aRect;
      InflateRect(aRect, 1, 1);
    end;

    Canvas.Brush.Style := bsClear;
    if Text <> '' then begin
      if Enabled then begin
        if IsValidSkinIndex(SkinIndex) then begin
          // Left contur
          if Hot then Canvas.Font.Color := gd[SkinIndex].HotFontColor[2] else Canvas.Font.Color := gd[SkinIndex].FontColor[2];
          if Canvas.Font.Color <> -1 then begin
            Rd := Rect(R.Left - 1, R.Top, R.Right - 1, R.Bottom);
            Tnt_DrawTextW(Canvas.Handle, Text, nLength, Rd, Flags);
          end;
          // Top
          if Hot then Canvas.Font.Color := gd[SkinIndex].HotFontColor[3] else Canvas.Font.Color := gd[SkinIndex].FontColor[3];
          if Canvas.Font.Color <> -1 then begin
            Rd := Rect(R.Left, R.Top - 1, R.Right, R.Bottom - 1);
            Tnt_DrawTextW(Canvas.Handle, Text, nLength, Rd, Flags);
          end;
          // Right
          if Hot then Canvas.Font.Color := gd[SkinIndex].HotFontColor[4] else Canvas.Font.Color := gd[SkinIndex].FontColor[4];
          if Canvas.Font.Color <> -1 then begin
            Rd := Rect(R.Left + 1, R.Top, R.Right + 1, R.Bottom);
            Tnt_DrawTextW(Canvas.Handle, Text, nLength, Rd, Flags);
          end;
          // Bottom
          if Hot then Canvas.Font.Color := gd[SkinIndex].HotFontColor[5] else Canvas.Font.Color := gd[SkinIndex].FontColor[5];
          if Canvas.Font.Color <> -1 then begin
            Rd := Rect(R.Left, R.Top + 1, R.Right, R.Bottom + 1);
            Tnt_DrawTextW(Canvas.Handle, Text, nLength, Rd, Flags);
          end;
          // Center
          if Hot then Canvas.Font.Color := gd[SkinIndex].HotFontColor[1] else Canvas.Font.Color := gd[SkinIndex].FontColor[1];
          Tnt_DrawTextW(Canvas.Handle, Text, nLength, R, Flags);
        end
        else Tnt_DrawTextW(Canvas.Handle, Text, nLength, R, Flags);
      end
      else begin
//        Rd := R;
        Canvas.Font.Color := MixColors(gd[SkinIndex].FontColor[1], gd[SkinIndex].Color, DefDisabledBlend);
        Tnt_DrawTextW(Canvas.Handle, Text, nLength, R, Flags);
      end;
//      aRect := R;
    end;
  end
end;

{$ENDIF}

procedure FadeRect(CanvasSrc: TCanvas; RSrc: TRect; CanvasDst: HDC; PDst: TPoint; Transparency: integer; Color: TColor; Blur : integer; Shape: TsShadowingShape); overload;
begin
  FadeRect(CanvasSrc, RSrc, CanvasDst, PDst, Transparency, Color, Blur, Shape, 0);
end;

procedure FadeRect(CanvasSrc: TCanvas; RSrc: TRect; CanvasDst: HDC; PDst: TPoint; Transparency: integer; Color: TColor; Blur : integer; Shape: TsShadowingShape; Radius : integer); overload;
var
  Bmp, TempBmp : TBitmap;
  delta: real;
  RValue,
  i{, j} : integer;
  c : TsColor;
  SavedBmp, SavedSrc, SavedDst: longint;
begin
  SavedSrc := SaveDC(CanvasSrc.Handle);
  SavedDst := SaveDC(CanvasDst);
  Color := ColorToRGB(Color);
  try

  case Transparency of
    100: begin
      BitBlt(CanvasDst,
             PDst.x, PDst.y, WidthOf(RSrc), HeightOf(RSrc), CanvasSrc.Handle,
             RSrc.Left, RSrc.Top, SRCCOPY);
    end;
    0: begin
      FillDC(CanvasDst, Rect(PDst.x, PDst.y, PDst.x + WidthOf(RSrc), PDst.y + HeightOf(RSrc)), Color);
    end
    else begin
      Bmp := CreateBmp24(WidthOf(rSrc), HeightOf(rSrc));
      TempBmp := CreateBmp24(Bmp.Width, Bmp.Height);
      Blur := Mini(Mini(TempBmp.Width div 2, TempBmp.Height div 2), Blur);

      RValue := MaxByte * Transparency div 100;
      SavedBmp := SaveDC(Bmp.Canvas.Handle);
      try

        bitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, CanvasSrc.Handle, RSrc.Left, RSrc.Top, SrcCopy);

        delta := (MaxByte - RValue) / (Blur + 1);
        // Prepare mask
        TColor(c) := clWhite;
        TempBmp.Canvas.Pen.Style := psClear;
        TempBmp.Canvas.Brush.Style := bsSolid;
        TempBmp.Canvas.Brush.Color := clWhite;
        case Shape of
          ssRectangle: begin
            for i := 0 to Blur do begin
              c.R := RValue + Round(delta * (Blur - i));
              c.G := c.R;
              c.B := c.R;
              TempBmp.Canvas.Brush.Color := TColor(c);
              TempBmp.Canvas.RoundRect(i, i, TempBmp.Width - i + 1, TempBmp.Height - i + 1, Blur + Radius, Blur + Radius);
            end;
          end;
          ssEllipse: begin
            for i := 0 to Blur do begin
              c.R := RValue + Round(delta * (Blur - i));
              c.G := c.R;
              c.B := c.R;
              TempBmp.Canvas.Brush.Color := TColor(c);
              TempBmp.Canvas.Ellipse(Rect(i, i, TempBmp.Width - i, TempBmp.Height - i));
            end;
          end;
        end;

        BlendBmpByMask(Bmp, TempBmp, TsColor(Color));

        // Copy back
        BitBlt(CanvasDst, PDst.x, PDst.y, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);

      finally
        RestoreDC(Bmp.Canvas.Handle, SavedBmp);

        FreeAndNil(Bmp);
        FreeAndNil(TempBmp);
      end
    end;
  end;
  finally
    RestoreDC(CanvasSrc.Handle, SavedSrc);
    RestoreDC(CanvasDst, SavedDst);
  end;
end;

procedure FadeBmp(FadedBmp: TBitMap; aRect: TRect; Transparency: integer; Color: TsColor; Blur, Radius : integer);
var
  Bmp, TempBmp : Graphics.TBitmap;
  r: TRect;
  delta: real;
  RValue, i : integer;
  c : TsColor;
begin
  Bmp := CreateBmp24(aRect.Right - aRect.Left, aRect.Bottom - aRect.Top);
  TempBmp := CreateBmp24(Bmp.Width, Bmp.Height);
  Blur := Mini(Mini(TempBmp.Width div 2, TempBmp.Height div 2), Blur);
  Radius := Mini(Mini(TempBmp.Width div 2, TempBmp.Height div 2), Radius);

  RValue := MaxByte * Transparency div 100;

  // Copy faded area in Ftb
  bitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, FadedBmp.Canvas.Handle, aRect.Left, aRect.Top, SrcCopy);

  TempBmp.Canvas.Pen.Style := psClear;
  TempBmp.Canvas.Brush.Style := bsSolid;
  TempBmp.Canvas.Brush.Color := clWhite;

  delta := (MaxByte - RValue) / (Blur + 1);
  // Prepare template
  TColor(c) := clWhite;
  for i := 0 to Blur do begin

    r := Rect(i, i, TempBmp.Width - i, TempBmp.Height - i);
    TempBmp.Canvas.Brush.Color := TColor(c);
    TempBmp.Canvas.RoundRect(i, i, TempBmp.Width - i, TempBmp.Height - i, Radius, Radius);

    c.R := RValue + Round(delta * (Blur - i));
    c.G := c.R;
    c.B := c.R;
  end;
  r := Rect(Blur, Blur, TempBmp.Width - Blur, TempBmp.Height - Blur);


  TempBmp.Canvas.Pen.Style := psClear;
  TempBmp.Canvas.Brush.Style := bsSolid;
  TempBmp.Canvas.Brush.Color := TColor(c);
  TempBmp.Canvas.RoundRect(r.Left, R.Top, R.Right, R.Bottom, Blur, Blur);

  BlendBmpByMask(Bmp, TempBmp, Color);

  // Copy back
  BitBlt(FadedBmp.Canvas.Handle, aRect.Left, aRect.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
  FreeAndNil(Bmp);
  FreeAndNil(TempBmp);
end;

procedure BlendTransRectangle(Dst: TBitmap; X, Y: integer; Src: TBitmap; aRect: TRect; Blend: real; TransColor: TsColor);
var
  oldleft, oldtop, dx, dy, h, w, width, height, curX, nextX : integer;
  S, D : PRGBArray;
begin
  if aRect.Top < 0 then begin
    oldtop := aRect.Top;
    aRect.Top := 0
  end
  else oldtop := 0;
  if aRect.Left < 0 then begin
    oldleft := aRect.Left;
    aRect.Left := 0
  end
  else oldleft := 0;
  if aRect.Bottom > Src.Height - 1 then aRect.Bottom := Src.Height - 1;
  if aRect.Right > Src.Width - 1 then aRect.Right := Src.Width - 1;

  try
    h := HeightOf(aRect);
    w := WidthOf(aRect);
    width := Dst.Width - 1;
    height := Dst.Height - 1;

    S := nil;
    D := nil;
    for dy := 0 to h do begin

      if (dy + Y > height) then break else if dy + Y < 0 then Continue;

      if dy + aRect.Top >= 0 then S := Src.ScanLine[dy + aRect.Top] else Continue;

      if dy + Y - oldtop < 0 then Continue else if dy + Y - oldtop > Dst.Height - 1 then Break else D := Dst.ScanLine[dy + Y - oldtop];

      for dx := 0 to w do begin
        nextX := dx + X - oldleft;
        if (nextX > Width) then Break;
        if (nextX < 0) then Continue;
        CurX := dX + aRect.Left;
        if CurX > Src.Width - 1 then Continue;

        if TransColor.A <> MaxByte then begin // ??? What is it
          if (S[CurX].B <> TransColor.B) or (S[CurX].G <> TransColor.G) or (S[CurX].R <> TransColor.R) then begin
            D[nextX].R := round(S[CurX].R - Blend * (S[CurX].R - D[nextX].R));
            D[nextX].G := round(S[CurX].G - Blend * (S[CurX].G - D[nextX].G));
            D[nextX].B := round(S[CurX].B - Blend * (S[CurX].B - D[nextX].B));
          end;
        end
        else begin
          D[nextX].R := round(S[CurX].R - Blend * (S[CurX].R - D[nextX].R));
          D[nextX].G := round(S[CurX].G - Blend * (S[CurX].G - D[nextX].G));
          D[nextX].B := round(S[CurX].B - Blend * (S[CurX].B - D[nextX].B));
        end;
      end;
    end;
  except
  end;
end;

procedure BlendTransBitmap(Bmp: TBitmap; Blend: real; Color, TransColor: TsColor);
var
  dx, dy : integer;
  S : PRGBArray;
  w, h : integer;
begin
  w := Bmp.Width - 1;
  h := Bmp.Height - 1;
  try
    for dy := 0 to h do begin
      S := Bmp.ScanLine[dy];
      for dx := 0 to w do begin
        if (S[dX].B <> TransColor.B) or (S[dX].G <> TransColor.G) or (S[dX].R <> TransColor.R) then begin
          S[dX].R := round((S[dX].R - Color.R) * Blend + Color.R);
          S[dX].G := round((S[dX].G - Color.G) * Blend + Color.G);
          S[dX].B := round((S[dX].B - Color.B) * Blend + Color.B);
        end;
      end;
    end;
  except
  end;
end;

procedure BlendBmpByMask(SrcBmp, MskBmp: Graphics.TBitMap; BlendColor : TsColor);
var
  S1, S2 : PRGBArray;
  X, Y: Integer;
  minW, minH : integer;
  r, g, b : integer;
begin
  if (SrcBmp.Width <> MskBmp.Width) or (SrcBmp.Height <> MskBmp.Height) then Exit;
  minH := SrcBmp.Height - 1;
  minW := SrcBmp.Width - 1;
  r := BlendColor.R shl 8;
  g := BlendColor.G shl 8;
  b := BlendColor.B shl 8;
  for Y := 0 to minH do begin
    S1 := SrcBmp.ScanLine[Y];
    S2 := MskBmp.ScanLine[Y];
    for X := 0 to minW do begin
      S1[X].R := (((S1[X].R - BlendColor.R) * S2[X].R + r) shr 8) and MaxByte;
      S1[X].G := (((S1[X].G - BlendColor.G) * S2[X].G + g) shr 8) and MaxByte;
      S1[X].B := (((S1[X].B - BlendColor.B) * S2[X].B + b) shr 8) and MaxByte;
    end
  end;
end;

procedure SumBitmaps(SrcBmp, MskBmp: Graphics.TBitMap; Color : TsColor);
begin
  if (SrcBmp.Width = MskBmp.Width) and (SrcBmp.Height = MskBmp.Height) then with FastSum24 do if Attach(MskBmp, SrcBmp) then begin
    FastSum24.Alpha := Color.R;
    DstX1 := 0;
    DstY1 := 0;
    DstX2 := DstX1 + Mskbmp.Width;
    DstY2 := DstY1 + Mskbmp.Height;

    SrcX1 := 0;
    SrcY2 := 0;
    SrcX2 := SrcX1 + SrcBmp.Width;
    SrcY2 := SrcY1 + SrcBmp.Height;

    BlendBitmapsRect;
  end
end;

procedure SumBmpRect(const DstBmp, SrcBmp: Graphics.TBitMap; const Color : TsColor; SrcRect : TRect; DstPoint : TPoint);
begin
  // Coordinates correcting
  if DstPoint.x < 0 then begin
    inc(SrcRect.Left, - DstPoint.x);
    DstPoint.x := 0;
  end;
  if DstPoint.y < 0 then begin
    inc(SrcRect.Top, - DstPoint.y);
    DstPoint.y := 0;
  end;
  if SrcRect.Left < 0 then begin
    inc(DstPoint.x, SrcRect.Left);
    SrcRect.Left := 0;
  end;
  if SrcRect.Top < 0 then begin
    inc(DstPoint.y, SrcRect.Top);
    SrcRect.Top := 0;
    if SrcRect.Bottom < SrcRect.Top then Exit;    
  end;
  if SrcRect.Right > SrcBmp.Width then SrcRect.Right := SrcBmp.Width;
  if SrcRect.Bottom > SrcBmp.Height then SrcRect.Bottom := SrcBmp.Height;
  if DstPoint.x + WidthOf(SrcRect) > DstBmp.Width then SrcRect.Right := SrcRect.Left + (DstBmp.Width - DstPoint.x);
  if DstPoint.y + HeightOf(SrcRect) > DstBmp.Height then SrcRect.Bottom := SrcRect.Top + (DstBmp.Height - DstPoint.y);

  with FastSum24 do if Attach(SrcBmp, DstBmp) then begin
    FastSum24.Alpha := Color.R;
    DstX1 := DstPoint.x;
    DstY1 := DstPoint.y;
    DstX2 := DstX1 + WidthOf(SrcRect);
    DstY2 := DstY1 + HeightOf(SrcRect);

    SrcX1 := SrcRect.Left;
    SrcY2 := SrcRect.Top;
    SrcX2 := SrcX1 + WidthOf(SrcRect);
    SrcY2 := SrcY1 + HeightOf(SrcRect);

    BlendBitmapsRect;
  end;
end;

procedure CopyByMask(R1, R2 : TRect; const Bmp1, Bmp2 : TBitmap; const CI : TCacheInfo; const UpdateTrans : boolean); overload;
var
  S1, S2, M : PRGBArray;
  S3 : PRGBAArray;
  X, Y, h, w, dX1, dX2: Integer;
  col : TsColor;
begin
  if Bmp2 = nil then Exit;
  h := Min(HeightOf(R1), HeightOf(R2));
  h := Min(h, Bmp1.Height - R1.Top);
  h := Min(h, Bmp2.Height - R2.Top) - 1;
  if h < 0 then Exit;
  w := Min(WidthOf(R1), WidthOf(R2));
  w := Min(w, Bmp1.Width - R1.Left);
  w := Min(w, Bmp2.Width - R2.Left) - 1;
  if w < 0 then Exit;
  if R1.Left < R2.Left then begin
    if (R1.Left < 0) then begin
      inc(R2.Left, - R1.Left);
      dec(h, - R1.Left);
      R1.Left := 0;
    end;
  end
  else begin
    if (R2.Left < 0) then begin
      inc(R1.Left, - R2.Left);
      dec(h, - R2.Left);
      R2.Left := 0;
    end;
  end;
  if R1.Top < R2.Top then begin
    if (R1.Top < 0) then begin
      inc(R2.Top, - R1.Top);
      dec(h, - R1.Top);
      R1.Top := 0;
    end;
  end
  else begin
    if (R2.Top < 0) then begin
      inc(R1.Top, - R2.Top);
      dec(h, - R2.Top);
      R2.Top := 0;
    end;
  end;

//  try
    if Bmp2.PixelFormat = pf24Bit then begin
      if not CI.Ready then begin
        for Y := 0 to h do begin
          S1 := Bmp1.ScanLine[R1.Top + Y];
          S2 := Bmp2.ScanLine[R2.Top + Y];
          M  := Bmp2.ScanLine[R2.Top + Bmp2.Height div 2 + Y];
          for X := 0 to w do begin
            dX1 := R1.Left + X;
            dX2 := R2.Left + X;
            S1[dX1].R := (((S1[dX1].R - S2[dX2].R) * M[dX2].R + S2[dX2].R shl 8) shr 8) and MaxByte;
            S1[dX1].G := (((S1[dX1].G - S2[dX2].G) * M[dX2].G + S2[dX2].G shl 8) shr 8) and MaxByte;
            S1[dX1].B := (((S1[dX1].B - S2[dX2].B) * M[dX2].B + S2[dX2].B shl 8) shr 8) and MaxByte;
          end;
        end;
      end
      else begin
        for Y := 0 to h do begin
          S1 := Bmp1.ScanLine[R1.Top + Y];
          S2 := Bmp2.ScanLine[R2.Top + Y];
          M  := Bmp2.ScanLine[R2.Top + Bmp2.Height div 2 + Y];
          for X := 0 to w do begin
            col.R := S2[R2.Left + X].R; col.B := S2[R2.Left + X].B; col.G := S2[R2.Left + X].G;
            if col.C <> clFuchsia then begin
              dX1 := R1.Left + X;
              dX2 := R2.Left + X;
              S1[dX1].R := (((S1[dX1].R - S2[dX2].R) * M[dX2].R + S2[dX2].R shl 8) shr 8) and MaxByte;
              S1[dX1].G := (((S1[dX1].G - S2[dX2].G) * M[dX2].G + S2[dX2].G shl 8) shr 8) and MaxByte;
              S1[dX1].B := (((S1[dX1].B - S2[dX2].B) * M[dX2].B + S2[dX2].B shl 8) shr 8) and MaxByte;
            end
            else if UpdateTrans then begin
              if (CI.Bmp.Height <= ci.Y + R1.Top + Y) then Continue;
              if (CI.Bmp.Width <= ci.X + R1.Left + X) then Break;
              if ci.Y + R1.Top + Y < 0 then Break;
              if ci.X + R1.Left + X < 0 then Continue;
              col.C := ci.Bmp.Canvas.Pixels[ci.X + R1.Left + X, ci.Y + R1.Top + Y];
              S1[R1.Left + X].R := col.R;
              S1[R1.Left + X].G := col.G;
              S1[R1.Left + X].B := col.B;
            end;
          end;
        end;
      end;
    end
    else if Bmp2.PixelFormat = pf32Bit then begin
      if not CI.Ready then begin
        for Y := 0 to h do begin
          S1 := Bmp1.ScanLine[R1.Top + Y];
          S3 := Bmp2.ScanLine[R2.Top + Y];
          for X := 0 to w do begin
            dX1 := R1.Left + X;
            dX2 := R2.Left + X;
            S1[dX1].R := (((S3[dX2].R - S1[dX1].R) * S3[dX2].A + S1[dX1].R shl 8) shr 8) and MaxByte;
            S1[dX1].G := (((S3[dX2].G - S1[dX1].G) * S3[dX2].A + S1[dX1].G shl 8) shr 8) and MaxByte;
            S1[dX1].B := (((S3[dX2].B - S1[dX1].B) * S3[dX2].A + S1[dX1].B shl 8) shr 8) and MaxByte;
          end;
        end;
      end
      else begin
        for Y := 0 to h do begin
          S1 := Bmp1.ScanLine[R1.Top + Y];
          S3 := Bmp2.ScanLine[R2.Top + Y];
          for X := 0 to w do begin
            col.R := S3[R2.Left + X].R; col.B := S3[R2.Left + X].B; col.G := S3[R2.Left + X].G;
            if col.C <> clFuchsia then begin
              dX1 := R1.Left + X;
              dX2 := R2.Left + X;
              S1[dX1].R := (((S3[dX2].R - S1[dX1].R) * S3[dX2].A + S1[dX1].R shl 8) shr 8) and MaxByte;
              S1[dX1].G := (((S3[dX2].G - S1[dX1].G) * S3[dX2].A + S1[dX1].G shl 8) shr 8) and MaxByte;
              S1[dX1].B := (((S3[dX2].B - S1[dX1].B) * S3[dX2].A + S1[dX1].B shl 8) shr 8) and MaxByte;
            end
            else if UpdateTrans then begin
              if (CI.Bmp.Height <= ci.Y + R1.Top + Y) then Continue;
              if (CI.Bmp.Width <= ci.X + R1.Left + X) then Break;
              if ci.Y + R1.Top + Y < 0 then Break;
              if ci.X + R1.Left + X < 0 then Continue;
              col.C := ci.Bmp.Canvas.Pixels[ci.X + R1.Left + X, ci.Y + R1.Top + Y];
              S1[R1.Left + X].R := col.R;
              S1[R1.Left + X].G := col.G;
              S1[R1.Left + X].B := col.B;
            end;
          end;
        end;
      end;
    end;
//  except
//  end;
end;

procedure CopyBmp32(R1, R2 : TRect; const Bmp1, Bmp2 : TBitmap; const CI : TCacheInfo; const UpdateTrans : boolean; const GrayedColor : TColor; const Blend : integer; const Reflected : boolean);
var
  S1 : PRGBArray;
  S3 : PRGBAArray;
  X, Y, h, w, dX1, dX2: Integer;
  col : TsColor;
  Col_ : TsColor_;
  MaskValue : byte;
  dR, dG, dB : real;
begin
  if Bmp2 = nil then Exit else Bmp2.PixelFormat := pf32Bit;
  h := Min(HeightOf(R1, True), HeightOf(R2, True));
  h := Min(h, Bmp1.Height - R1.Top);
  h := Min(h, Bmp2.Height - R2.Top) - 1;
  if h < 0 then Exit;
  w := Min(WidthOf(R1, True), WidthOf(R2, True));
  w := Min(w, Bmp1.Width - R1.Left);
  w := Min(w, Bmp2.Width - R2.Left) - 1;
  if w < 0 then Exit;
  if R1.Left < R2.Left then begin
    if (R1.Left < 0) then begin
      inc(R2.Left, - R1.Left);
      dec(h, - R1.Left);
      R1.Left := 0;
      w := Min(w, Bmp2.Width - R2.Left) - 1;
    end;
  end
  else begin
    if (R2.Left < 0) then begin
      inc(R1.Left, - R2.Left);
      dec(h, - R2.Left);
      R2.Left := 0;
      w := Min(w, Bmp1.Width - R1.Left) - 1;
    end;
  end;
  if R1.Top < R2.Top then begin
    if (R1.Top < 0) then begin
      inc(R2.Top, - R1.Top);
      dec(h, - R1.Top);
      R1.Top := 0;
      h := Min(h, Bmp2.Height - R2.Top) - 1;
    end;
  end
  else begin
    if (R2.Top < 0) then begin
      inc(R1.Top, - R2.Top);
      dec(h, - R2.Top);
      R2.Top := 0;
      h := Min(h, Bmp1.Height - R1.Top) - 1;
    end;
  end;

  if GrayedColor <> clNone then begin
    Col.C := GrayedColor;
    dR := Col.R / MaxByte;
    dG := Col.G / MaxByte;
    dB := Col.B / MaxByte;

    for Y := 0 to h do begin
      S3 := Bmp2.ScanLine[R2.Top + Y];
      for X := 0 to w do begin
        col_ := S3[X];
        if col_.C <> sFuchsia.C then begin
          MaskValue := (col_.R + col_.G + col_.B) div 3;
          col_.R := min(round(MaskValue * dR), MaxByte);
          col_.G := min(round(MaskValue * dG), MaxByte);
          col_.B := min(round(MaskValue * dB), MaxByte);
          S3[X] := col_;
        end;
      end;
    end;
  end;

  if Blend <> 0 then begin
    if not CI.Ready then begin
      for Y := 0 to h do begin
        S1 := Bmp1.ScanLine[R1.Top + Y];
        S3 := Bmp2.ScanLine[R2.Top + Y];
        for X := 0 to w do begin
          dX1 := R1.Left + X;
          dX2 := R2.Left + X;
          col_ := S3[dX2];
          MaskValue := col_.A * (100 - Blend) div 100;
          S1[dX1].R := (((col_.R - S1[dX1].R) * MaskValue + S1[dX1].R shl 8) shr 8) and MaxByte;
          S1[dX1].G := (((col_.G - S1[dX1].G) * MaskValue + S1[dX1].G shl 8) shr 8) and MaxByte;
          S1[dX1].B := (((col_.B - S1[dX1].B) * MaskValue + S1[dX1].B shl 8) shr 8) and MaxByte;
        end;
      end;
    end
    else begin
      for Y := 0 to h do begin
        S1 := Bmp1.ScanLine[R1.Top + Y];
        S3 := Bmp2.ScanLine[R2.Top + Y];
        dX1 := R1.Left;
        dX2 := R2.Left;
        for X := 0 to w do begin
          Col_ := S3[dX2];
          if Col_.C <> clFuchsia then begin
            MaskValue := Col_.A * (100 - Blend) div 100;
            S1[dX1].R := (((Col_.R - S1[dX1].R) * MaskValue + S1[dX1].R shl 8) shr 8) and MaxByte;
            S1[dX1].G := (((Col_.G - S1[dX1].G) * MaskValue + S1[dX1].G shl 8) shr 8) and MaxByte;
            S1[dX1].B := (((Col_.B - S1[dX1].B) * MaskValue + S1[dX1].B shl 8) shr 8) and MaxByte;
          end
          else if UpdateTrans then begin
            if (CI.Bmp.Height <= ci.Y + R1.Top + Y) then Continue;
            if (CI.Bmp.Width <= ci.X + dX1) then Break;
            if ci.Y + R1.Top + Y < 0 then Break;
            if ci.X + dX1 < 0 then Continue;
            col.C := ci.Bmp.Canvas.Pixels[ci.X + dX1, ci.Y + R1.Top + Y];
            S1[dX1].R := col.R;
            S1[dX1].G := col.G;
            S1[dX1].B := col.B;
          end;
          inc(dX1);
          inc(dX2);
        end;
      end;
    end;
  end
  else begin
    if not CI.Ready then begin
      for Y := 0 to h do begin
        S1 := Bmp1.ScanLine[R1.Top + Y];
        S3 := Bmp2.ScanLine[R2.Top + Y];
        dX1 := R1.Left;
        dX2 := R2.Left;
        for X := 0 to w do begin
        try
          S1[dX1].R := (((S3[dX2].R - S1[dX1].R) * S3[dX2].A + S1[dX1].R shl 8) shr 8) and MaxByte;
        except
        end;
          S1[dX1].G := (((S3[dX2].G - S1[dX1].G) * S3[dX2].A + S1[dX1].G shl 8) shr 8) and MaxByte;
          S1[dX1].B := (((S3[dX2].B - S1[dX1].B) * S3[dX2].A + S1[dX1].B shl 8) shr 8) and MaxByte;
          inc(dX1);
          inc(dX2);
        end;
      end;
    end
    else begin
      for Y := 0 to h do begin
        S1 := Bmp1.ScanLine[R1.Top + Y];
        S3 := Bmp2.ScanLine[R2.Top + Y];
        dX1 := R1.Left;
        dX2 := R2.Left;
        for X := 0 to w do begin
          Col_ := S3[dX2];
          if Col_.C <> clFuchsia then begin
            S1[dX1].R := (((S3[dX2].R - S1[dX1].R) * S3[dX2].A + S1[dX1].R shl 8) shr 8) and MaxByte;
            S1[dX1].G := (((S3[dX2].G - S1[dX1].G) * S3[dX2].A + S1[dX1].G shl 8) shr 8) and MaxByte;
            S1[dX1].B := (((S3[dX2].B - S1[dX1].B) * S3[dX2].A + S1[dX1].B shl 8) shr 8) and MaxByte;
          end
          else if UpdateTrans then begin
            if (CI.Bmp.Height <= ci.Y + R1.Top + Y) then Continue;
            if (CI.Bmp.Width <= ci.X + dX1) then Break;
            if ci.Y + R1.Top + Y < 0 then Break;
            if ci.X + dX1 < 0 then Continue;
            col.C := ci.Bmp.Canvas.Pixels[ci.X + R1.Left + X, ci.Y + R1.Top + Y];
            S1[dX1].R := col.R;
            S1[dX1].G := col.G;
            S1[dX1].B := col.B;
          end;
          inc(dX1);
          inc(dX2);
        end;
      end;
    end;
  end;

  if Reflected then begin
    h := min(Bmp2.Height div 2 - 1, Bmp1.Height - R1.Bottom - 1);
    Col.A := MaxByte div (Bmp2.Height div 2); // Step
    Col.R := MaxByte;
    for Y := 1 to h do begin
      S1 := Bmp1.ScanLine[R1.Bottom + Y];
      S3 := Bmp2.ScanLine[R2.Bottom - Y - 1];
      dX1 := R1.Left;
      dX2 := R2.Left;
      for X := 0 to w do begin
        Col_ := S3[dX2];
        if Col_.C <> clFuchsia then begin
          if Blend = 0
            then MaskValue := max(((Col_.A div 3) * Col.R) div MaxByte, 0)
            else MaskValue := max(((Col_.A * (100 - Blend) div 300) * Col.R) div MaxByte, 0);
          S1[dX1].R := (((Col_.R - S1[dX1].R) * MaskValue + S1[dX1].R shl 8) shr 8) and MaxByte;
          S1[dX1].G := (((Col_.G - S1[dX1].G) * MaskValue + S1[dX1].G shl 8) shr 8) and MaxByte;
          S1[dX1].B := (((Col_.B - S1[dX1].B) * MaskValue + S1[dX1].B shl 8) shr 8) and MaxByte;
        end;

        inc(dX1);
        inc(dX2);
      end;
      dec(Col.R, Col.A);
    end;
  end;
end;

procedure CopyByMask(R1, R2 : TRect; const Bmp1, Bmp2 : TBitmap; const CI : TCacheInfo; const UpdateTrans : boolean; const MaskData : TsMaskData); overload;
var
  S1, S2, M : PRGBArray;
  X, Y, h, w, ch, cw, dX1, dX2, hdiv2, k1, k2: Integer;
  col : TsColor;
  prgb : TsRGB;
  C : TsColor;
  Cur : TsColor_;
  C1 : TsColor;
begin
{$IFDEF NOSLOWDETAILS} Exit;{$ENDIF}
  h := R1.Bottom - R1.Top;
  if h > R2.Bottom - R2.Top then h := R2.Bottom - R2.Top;
  if h > Bmp1.Height - R1.Top then h := Bmp1.Height - R1.Top;
  if h > Bmp2.Height - R2.Top then h := Bmp2.Height - R2.Top - 1 else h := h - 1;
  if h < 0 then Exit;

  w := R1.Right - R1.Left;
  if w > R2.Right - R2.Left then w := R2.Right - R2.Left;
  if w > Bmp1.Width - R1.Left then w := Bmp1.Width - R1.Left;
  if w > Bmp2.Width - R2.Left then w := Bmp2.Width - R2.Left - 1 else w := w - 1;
  if w < 0 then Exit;

  if R1.Left < R2.Left then begin
    if (R1.Left < 0) then begin
      inc(R2.Left, - R1.Left);
      dec(h, - R1.Left);
      R1.Left := 0;
    end;
  end
  else begin
    if (R2.Left < 0) then begin
      inc(R1.Left, - R2.Left);
      dec(h, - R2.Left);
      R2.Left := 0;
    end;
  end;
  if R1.Top < R2.Top then begin
    if (R1.Top < 0) then begin
      inc(R2.Top, - R1.Top);
      dec(h, - R1.Top);
      R1.Top := 0;
    end;
  end
  else begin
    if (R2.Top < 0) then begin
      inc(R1.Top, - R2.Top);
      dec(h, - R2.Top);
      R2.Top := 0;
    end;
  end;
  Cur.A := 0;
  C1.A := 0;

  hdiv2 := (MaskData.R.Bottom - MaskData.R.Top) div 2;
  k1 := R2.Top + hdiv2;
  k2 := ci.X + R1.Left; //  v6.15

  if not CI.Ready then begin
    if UpdateTrans then begin
      prgb.R := TsColor(CI.FillColor).R;
      prgb.G := TsColor(CI.FillColor).G;
      prgb.B := TsColor(CI.FillColor).B;
      for Y := 0 to h do begin
        S1 := Bmp1.ScanLine[R1.Top + Y];
        S2 := Bmp2.ScanLine[R2.Top + Y];
        M  := Bmp2.ScanLine[k1 + Y];
        dX1 := R1.Left;
        dX2 := R2.Left;
        for X := 0 to w do begin
          cur.sBGR := S2[dX2];
          if cur.C <> sFuchsia.C then begin
            S1[dX1].R := (((S1[dX1].R - cur.R) * M[dX2].R + cur.R shl 8) shr 8) and MaxByte;
            S1[dX1].G := (((S1[dX1].G - cur.G) * M[dX2].G + cur.G shl 8) shr 8) and MaxByte;
            S1[dX1].B := (((S1[dX1].B - cur.B) * M[dX2].B + cur.B shl 8) shr 8) and MaxByte;
          end
          else S1[dX1] := prgb;
          inc(dX1);
          inc(dX2);
        end;
      end;
    end
    else for Y := 0 to h do begin
      S1 := Bmp1.ScanLine[R1.Top + Y];
      S2 := Bmp2.ScanLine[R2.Top + Y];
      M  := Bmp2.ScanLine[k1 + Y];
      dX1 := R1.Left;
      dX2 := R2.Left;
      for X := 0 to w do begin
        cur.sBGR := S2[dX2];
        if cur.C <> sFuchsia.C then begin
          S1[dX1].R := (((S1[dX1].R - cur.R) * M[dX2].R + cur.R shl 8) shr 8) and MaxByte;
          S1[dX1].G := (((S1[dX1].G - cur.G) * M[dX2].G + cur.G shl 8) shr 8) and MaxByte;
          S1[dX1].B := (((S1[dX1].B - cur.B) * M[dX2].B + cur.B shl 8) shr 8) and MaxByte;
        end;
        inc(dX1);
        inc(dX2);
      end;
    end;
  end
  else begin
    ch := CI.Bmp.Height;
    cw := CI.Bmp.Width;
    if UpdateTrans then begin
      if CI.Bmp.PixelFormat = pf24bit then begin
        if Fast24Src.Attach(CI.Bmp) then for Y := 0 to h do begin
          S1 := Bmp1.ScanLine[R1.Top + Y];
          S2 := Bmp2.ScanLine[R2.Top + Y];
          M  := Bmp2.ScanLine[k1 + Y];
          dX1 := R1.Left;
          dX2 := R2.Left;
          for X := 0 to w do begin
            cur.sBGR := S2[dX2];
            if cur.C <> sFuchsia.C then begin
              S1[dX1].R := (((S1[dX1].R - cur.R) * M[dX2].R + cur.R shl 8) shr 8) and MaxByte;
              S1[dX1].G := (((S1[dX1].G - cur.G) * M[dX2].G + cur.G shl 8) shr 8) and MaxByte;
              S1[dX1].B := (((S1[dX1].B - cur.B) * M[dX2].B + cur.B shl 8) shr 8) and MaxByte;
            end
            else begin
              if (ch <= ci.Y + R1.Top + Y) then Continue;
              if (cw <= k2 + X) then Break;
              if ci.Y + R1.Top + Y < 0 then Break;
              if k2 + X < 0 then Continue;
              col := Fast24Src.Pixels[k2 + X, ci.Y + R1.Top + Y];
              S1[dX1].R := col.R;
              S1[dX1].G := col.G;
              S1[dX1].B := col.B;
            end;
            inc(dX1);
            inc(dX2);
          end;
        end;
      end
      else
      if CI.Bmp.PixelFormat = pf32bit then begin
        if Fast32Src.Attach(CI.Bmp) then for Y := 0 to h do begin
          S1 := Bmp1.ScanLine[R1.Top + Y];
          S2 := Bmp2.ScanLine[R2.Top + Y];
          M  := Bmp2.ScanLine[k1 + Y];
          dX1 := R1.Left;
          dX2 := R2.Left;
          for X := 0 to w do begin
            cur.sBGR := S2[dX2];
            if cur.C <> sFuchsia.C then begin
              S1[dX1].R := (((S1[dX1].R - cur.R) * M[dX2].R + cur.R shl 8) shr 8) and MaxByte;
              S1[dX1].G := (((S1[dX1].G - cur.G) * M[dX2].G + cur.G shl 8) shr 8) and MaxByte;
              S1[dX1].B := (((S1[dX1].B - cur.B) * M[dX2].B + cur.B shl 8) shr 8) and MaxByte;
            end
            else begin
              if (ch <= ci.Y + R1.Top + Y) then Continue;
              if (cw <= k2 + X) then Break;
              if ci.Y + R1.Top + Y < 0 then Break;
              if k2 + X < 0 then Continue;
              C := Fast32Src.Pixels[k2 + X, ci.Y + R1.Top + Y];
              S1[dX1].R := C.R;
              S1[dX1].G := C.G;
              S1[dX1].B := C.B;
            end;
            inc(dX1);
            inc(dX2);
          end;
        end;
      end
    end
    else for Y := 0 to h do begin
      S1 := Bmp1.ScanLine[R1.Top + Y];
      S2 := Bmp2.ScanLine[R2.Top + Y];
      M  := Bmp2.ScanLine[k1 + Y];
      dX1 := R1.Left;
      dX2 := R2.Left;
      for X := 0 to w do begin
        S1[dX1].R := (((S1[dX1].R - S2[dX2].R) * M[dX2].R + S2[dX2].R shl 8) shr 8) and MaxByte;
        S1[dX1].G := (((S1[dX1].G - S2[dX2].G) * M[dX2].G + S2[dX2].G shl 8) shr 8) and MaxByte;
        S1[dX1].B := (((S1[dX1].B - S2[dX2].B) * M[dX2].B + S2[dX2].B shl 8) shr 8) and MaxByte;
        inc(dX1);
        inc(dX2);
      end;
    end;
  end;
end;

procedure CopyTransBitmaps(DstBmp, SrcBmp: Graphics.TBitMap; X, Y : integer; TransColor : TsColor);
var
  Dst, Src : PRGBArray;
  sX, sY, sw, sh, dh, dw: Integer;
  C : TsColor_;
begin
  sw := SrcBmp.Width - 1;
  sh := SrcBmp.Height - 1;
  dw := DstBmp.Width - 1;
  dh := DstBmp.Height - 1;
  C.A := 0;
  for sY := 0 to sh do begin
    if not ((sY + Y > dh) or (sY + Y < 0)) then begin
      Dst := DstBmp.ScanLine[sY + Y];
      Src := SrcBmp.ScanLine[sY];
      for sX := 0 to sw do if not ((sX + X > dw) or (sX + X < 0)) then begin
        if (Src[sX].B <> TransColor.B) or (Src[sX].G <> TransColor.G) or (Src[sX].R <> TransColor.R) then begin
          Dst[sX + X] := Src[sX];
        end;
      end;
    end;
  end;
end;

procedure CopyTransRect(DstBmp, SrcBmp: Graphics.TBitMap; X, Y : integer; SrcRect: TRect; TransColor : TColor; CI : TCacheInfo; UpdateTrans : boolean);
var
  Dst, Src : PRGBArray;
  DstA, SrcA : PRGBAArray;
  sX, sY, DstX, DstY, SrcX, SrcY : Integer;
  Cur, MaskColor : TsColor_;
  h, w, ch, cw, dh, dw : integer;
  col : TsColor;
  ParentRGBA : TsColor_;
begin
  MaskColor.B := TsColor(TransColor).R;
  MaskColor.G := TsColor(TransColor).G;
  MaskColor.R := TsColor(TransColor).B;
  MaskColor.A := 0;

  if SrcRect.Top < 0 then SrcRect.Top := 0;
  if SrcRect.Bottom > SrcBmp.Height - 1 then SrcRect.Bottom := SrcBmp.Height - 1;
  if SrcRect.Left < 0 then SrcRect.Left := 0;
  if SrcRect.Right > SrcBmp.Width - 1 then SrcRect.Right := SrcBmp.Width - 1;

  h := HeightOf(SrcRect);
  w := WidthOf(SrcRect);
  dh := DstBmp.Height - 1;
  dw := DstBmp.Width - 1;
  Cur.A := 0;

  if UpdateTrans and CI.Ready and (CI.Bmp <> nil) then begin
    ch := CI.Bmp.Height;
    cw := CI.Bmp.Width;
    if (CI.Bmp.PixelFormat = pf24bit) and Fast24Src.Attach(CI.Bmp) then begin

      if DstBmp <> CI.Bmp then begin
        DstY := Y;
        SrcY := SrcRect.Top;
        for sY := 0 to h do begin
          if (DstY <= dh) and (DstY >= 0) then begin
            Dst := DstBmp.ScanLine[DstY];
            Src := SrcBmp.ScanLine[SrcY];
            DstX := X;
            SrcX := SrcRect.Left;
            for sX := 0 to w do begin
              if (DstX <= dw) and (DstX >= 0) then begin
                Cur.sBGR := Src[SrcX];
                if Cur.C <> MaskColor.C then Dst[DstX] := Cur.sBGR else begin
                  if (ch <= ci.Y + DstY) then Continue;
                  if (cw <= ci.X + DstX) then Break;
                  if ci.Y + DstY < 0 then Break;
                  if ci.X + DstX < 0 then Continue;
                  col := Fast24Src.Pixels[ci.X + DstX, ci.Y + DstY];
                  Dst[DstX].R := col.R;
                  Dst[DstX].G := col.G;
                  Dst[DstX].B := col.B;
                end;
              end;
              inc(DstX);
              inc(SrcX);
            end
          end;
          inc(DstY);
          inc(SrcY);
        end
      end
      else begin
        DstY := Y;
        SrcY := SrcRect.Top;
        for sY := 0 to h do begin
          if (DstY <= dh) and (DstY >= 0) then begin
            Dst := DstBmp.ScanLine[DstY];
            Src := SrcBmp.ScanLine[SrcY];
            DstX := X;
            SrcX := SrcRect.Left;
            for sX := 0 to w do begin
              if (DstX <= dw) and (DstX >= 0) then begin
                Cur.sBGR := Src[SrcX];
                if Cur.C <> MaskColor.C then Dst[DstX] := Cur.sBGR
              end;
              inc(DstX);
              inc(SrcX);
            end
          end;
          inc(DstY);
          inc(SrcY);
        end
      end;
    end;
  end
  else begin
    if SrcBmp.PixelFormat = pf24bit then begin
      if not CI.Ready then begin // If color for transparent pixels is defined
        ParentRGBA.C := CI.FillColor;
        ParentRGBA.A := ParentRGBA.R; // Swap
        ParentRGBA.R := ParentRGBA.B;
        ParentRGBA.B := ParentRGBA.A;
        ParentRGBA.A := 0;
        DstY := Y;
        SrcY := SrcRect.Top;
        for sY := 0 to h do begin
          if (DstY <= dh) and (DstY >= 0) then begin
            Dst := DstBmp.ScanLine[DstY];
            Src := SrcBmp.ScanLine[SrcY];
            DstX := X;
            SrcX := SrcRect.Left;
            for sX := 0 to w do begin
              if (DstX <= dw) and (DstX >= 0) then begin
                Cur.sBGR := Src[SrcX];
                if Cur.C <> MaskColor.C then Dst[DstX] := Cur.sBGR else Dst[DstX] := ParentRGBA.sBGR
              end;
              inc(DstX);
              inc(SrcX);
            end
          end;
          inc(dstY);
          inc(SrcY);
        end
      end
      else begin
        DstY := Y;
        SrcY := SrcRect.Top;
        for sY := 0 to h do begin
          if (DstY <= dh) and (DstY >= 0) then begin
            Dst := DstBmp.ScanLine[DstY];
            Src := SrcBmp.ScanLine[SrcY];
            DstX := X;
            SrcX := SrcRect.Left;
            for sX := 0 to w do begin
              if (DstX <= dw) and (DstX >= 0) then begin
                Cur.sBGR := Src[SrcX];
                if (Cur.C <> MaskColor.C) then Dst[DstX] := Cur.sBGR
              end;
              inc(DstX);
              inc(SrcX);
            end
          end;
          inc(DstY);
          inc(SrcY)
        end;
      end
    end
    else if SrcBmp.PixelFormat = pf32bit then begin
      if not CI.Ready then begin // If color for transparent pixels is defined
        ParentRGBA.C := CI.FillColor;
        DstY := Y;
        SrcY := SrcRect.Top;
        for sY := 0 to h do begin
          if (DstY <= dh) and (DstY >= 0) then begin
            DstA := DstBmp.ScanLine[DstY];
            SrcA := SrcBmp.ScanLine[SrcY];
            DstX := X;
            SrcX := SrcRect.Left;
            for sX := 0 to w do begin
              if (DstX <= dw) and (DstX >= 0) then begin
                if (SrcA[SrcX].C <> MaskColor.C) then DstA[DstX] := SrcA[SrcX] else DstA[DstX] := ParentRGBA
              end;
              inc(DstX);
              inc(SrcX);
            end
          end;
          inc(DstY);
          inc(SrcY);
        end
      end
      else begin
        DstY := Y;
        SrcY := SrcRect.Top;
        for sY := 0 to h do begin
          if (DstY <= dh) and (DstY >= 0) then begin
            DstA := DstBmp.ScanLine[DstY];
            SrcA := SrcBmp.ScanLine[SrcY];
            DstX := X;
            SrcX := SrcRect.Left;
            for sX := 0 to w do begin
              if (DstX <= dw) and (DstX >= 0) then begin
                if (SrcA[SrcX].C <> MaskColor.C) then DstA[DstX] := SrcA[SrcX];
              end;
              inc(DstX);
              inc(SrcX);
            end
          end;
          inc(DstY);
          inc(SrcY);
        end;
      end
    end;
  end;
end;

procedure CopyTransRectA(DstBmp, SrcBmp: Graphics.TBitMap; X, Y : integer; SrcRect: TRect; TransColor : TColor; CI : TCacheInfo);
var
  Dst, Src : PRGBArray;
  sX, sY, SrcX, DstX, DstY : Integer;
  h, w, dh, dw : integer;
  C, M : TsColor_;
begin
  M.B := TsColor_(TransColor).R;
  M.G := TsColor_(TransColor).G;
  M.R := TsColor_(TransColor).B;
  M.A := 0;

  if SrcRect.Top < 0 then SrcRect.Top := 0;
  if SrcRect.Bottom > SrcBmp.Height - 1 then SrcRect.Bottom := SrcBmp.Height - 1;
  if SrcRect.Left < 0 then SrcRect.Left := 0;
  if SrcRect.Right > SrcBmp.Width - 1 then SrcRect.Right := SrcBmp.Width - 1;

  h := HeightOf(SrcRect);
  w := WidthOf(SrcRect);
  C.A := 0;

  DstY := Y;
  dh := DstBmp.Height - 1;
  dw := DstBmp.Width - 1;
  for sY := 0 to h do begin
    if (DstY <= dh) and (DstY >= 0) then begin
      Dst := DstBmp.ScanLine[DstY];
      Src := SrcBmp.ScanLine[sY + SrcRect.Top];
      DstX := X;
      SrcX := SrcRect.Left;
      for sX := 0 to w do if (DstX <= dw) and (DstX >= 0) then begin
        C.sBGR := Src[SrcX];
        if C.C <> M.C then Dst[DstX] := C.sBGR;
        inc(SrcX);
        inc(dstX);
      end;
    end;
    inc(DstY);
  end;
end;

procedure CopyRect(DstBmp, SrcBmp: Graphics.TBitMap; X, Y : integer; aRect: TRect; TransColor : TColor);
var
  Dst, Src : PRGBArray;
  sX, sY: Integer;
  sh, sw, dh, dw : integer;
begin
  sh := SrcBmp.Height - 1;
  sw := SrcBmp.Width - 1;
  if aRect.Top < 0 then aRect.Top := 0;
  if aRect.Bottom > sh then aRect.Bottom := sh;
  if aRect.Left < 0 then aRect.Left := 0;
  if aRect.Right > sw then aRect.Right := sw;

  sh := HeightOf(aRect) - 1;
  sw := WidthOf(aRect) - 1;
  dh := DstBmp.Height - 1;
  dw := DstBmp.Width - 1;
  for sY := 0 to sh do begin
    if sY + Y > dh then Break;
    Dst := DstBmp.ScanLine[sY + Y];
    Src := SrcBmp.ScanLine[sY + aRect.Top];
    for sX := 0 to sw do begin
      if sX + X > dw then Break;
      Dst[sX + X] := Src[sX + aRect.Left];
    end;
  end;
end;

procedure SumBitmapsByMask(var ResultBmp, Src1, Src2: Graphics.TBitMap; MskBmp: Graphics.TBitMap; Percent : word = 0);
var
  S1, S2, M : PRGBArray;
  R : PRGBAArray;
  X, Y, w, h: Integer;
begin
  if (Src1.Width <> Src2.Width) or (Src1.Height <> Src2.Height) then Exit;
  w := Src1.Width - 1;
  h := Src1.Height - 1;
  if MskBmp = nil then for Y := 0 to h do begin
    S1 := Src1.ScanLine[Y];
    S2 := Src2.ScanLine[Y];
    R  := ResultBmp.ScanLine[Y];
    for X := 0 to w do begin
      R[X].R := (((S1[X].R - S2[X].R) * Percent + S2[X].R shl 8) shr 8) and MaxByte;
      R[X].G := (((S1[X].G - S2[X].G) * Percent + S2[X].G shl 8) shr 8) and MaxByte;
      R[X].B := (((S1[X].B - S2[X].B) * Percent + S2[X].B shl 8) shr 8) and MaxByte;
    end
  end
  else for Y := 0 to h do begin
    S1 := Src1.ScanLine[Y];
    S2 := Src2.ScanLine[Y];
    R  := ResultBmp.ScanLine[Y];
    M  := MskBmp.ScanLine[Y];
    for X := 0 to w do begin
      R[X].R := (((S1[X].R - S2[X].R) * M[X].R + S2[X].R shl 8) shr 8) and MaxByte;
      R[X].G := (((S1[X].G - S2[X].G) * M[X].G + S2[X].G shl 8) shr 8) and MaxByte;
      R[X].B := (((S1[X].B - S2[X].B) * M[X].B + S2[X].B shl 8) shr 8) and MaxByte;
    end
  end
end;

procedure SumByMask(var Src1, Src2, MskBmp: Graphics.TBitMap; aRect: TRect);
var
  S1, S2, M : PRGBArray;
  X, Y, B, R: Integer;
begin
  if Src1.Width < WidthOf(aRect) then Exit;
  if Src1.Height < HeightOf(aRect) then Exit;
  B := aRect.Bottom - 1;
  R := aRect.Right - 1;
  for Y := aRect.Top to B do begin
    S1 := Src1.ScanLine[Y];
    S2 := Src2.ScanLine[Y];
    M  := MskBmp.ScanLine[Y];
    for X := aRect.Left to R do begin
      S1[X].R := (((S1[X].R - S2[X].R) * M[X].R + S2[X].R shl 8) shr 8) and MaxByte;
      S1[X].G := (((S1[X].G - S2[X].G) * M[X].G + S2[X].G shl 8) shr 8) and MaxByte;
      S1[X].B := (((S1[X].B - S2[X].B) * M[X].B + S2[X].B shl 8) shr 8) and MaxByte;
    end
  end;
end;

procedure SumByMaskWith32(const Src1, Src2, MskBmp: Graphics.TBitMap; const aRect: TRect);
var
  S24, M24 : PRGBArray;
  S32 : PRGBAArray;
  X, Y, B, R: Integer;
begin
  if Src1.Width < WidthOf(aRect) then Exit;
  if Src1.Height < HeightOf(aRect) then Exit;
  B := aRect.Bottom - 1;
  R := aRect.Right - 1;
  for Y := aRect.Top to B do begin
    S24 := Src1.ScanLine[Y];
    S32 := Src2.ScanLine[Y];
    M24  := MskBmp.ScanLine[Y];
    for X := aRect.Left to R do begin
      case M24[X].R of
        0 : begin
          S24[X].R := (((S32[X].R - S24[X].R) * S32[X].A + S24[X].R shl 8) shr 8) and MaxByte;
          S24[X].G := (((S32[X].G - S24[X].G) * S32[X].A + S24[X].G shl 8) shr 8) and MaxByte;
          S24[X].B := (((S32[X].B - S24[X].B) * S32[X].A + S24[X].B shl 8) shr 8) and MaxByte;
        end;
        MaxByte : begin
          // skip
        end
        else begin
          S24[X].R := (((S32[X].R - S24[X].R) * S32[X].A + S24[X].R shl 8) shr 8) and MaxByte;
          S24[X].G := (((S32[X].G - S24[X].G) * S32[X].A + S24[X].G shl 8) shr 8) and MaxByte;
          S24[X].B := (((S32[X].B - S24[X].B) * S32[X].A + S24[X].B shl 8) shr 8) and MaxByte;
        end;
      end;
    end
  end;
end;

procedure GradientBmp(Bmp: Graphics.TBitMap; aRect : TRect; Color1, Color2 : TsColor; Layout : TGradientTypes; Percent1, Percent2 : TPercent; Width : integer);
var
  SSrc : PRGBArray;
  X, Y, i, w, h, dx, dy: Integer;
  R, G, B, pr, rr : real;
  RStep1, GStep1, BStep1 : real;
  RStep2, GStep2, BStep2 : real;
  SavedDC : hWnd;
  FirstLim, SecondLim : integer;
  AvColor, CurrentColor : TsColor;
begin
  Color1.C := ColorToRGB(TColor(Color1));
  Color2.C := ColorToRGB(TColor(Color2));
  if aRect.Right > Bmp.Width then aRect.Right := Bmp.Width;
  if aRect.Bottom > Bmp.Height then aRect.Bottom := Bmp.Height;
  if aRect.Left < 0 then aRect.Left := 0;
  if aRect.Top < 0 then aRect.Top := 0;

  FirstLim  := Maxi((Width * Percent1) div 100, 1);
  SecondLim := Maxi((Width * Percent2) div 100, 1);
  case Layout of
    gtLeftRight: begin
      FirstLim  := Mini(FirstLim , WidthOf(aRect));
      SecondLim := Mini(SecondLim, WidthOf(aRect));
    end
    else begin
      FirstLim  := Mini(FirstLim , HeightOf(aRect));
      SecondLim := Mini(SecondLim, HeightOf(aRect));
    end;
  end;
  if FirstLim < 1 then FirstLim := 1;
  if SecondLim < 1 then SecondLim := 1;

  SavedDC := SaveDC(Bmp.Canvas.Handle);
  try
    R := Color1.R;
    G := Color1.G;
    B := Color1.B;
    AvColor := AverageColor(Color1, Color2);

    Bmp.PixelFormat := pf24bit;
    w := WidthOf(aRect) + aRect.Left;
    h := HeightOf(aRect) + aRect.Top;
    pr := SecondLim / FirstLim;
    rr := FirstLim / SecondLim;
    case Layout of
      gtLeftRight : begin
        RStep1 := (AvColor.R - Color1.R) / FirstLim;
        GStep1 := (AvColor.G - Color1.G) / FirstLim;
        BStep1 := (AvColor.B - Color1.B) / FirstLim;

        RStep2 := (Color2.R - AvColor.R) / SecondLim;
        GStep2 := (Color2.G - AvColor.G) / SecondLim;
        BStep2 := (Color2.B - AvColor.B) / SecondLim;

        for Y := aRect.Top to h - 1 do begin
          SSrc := Bmp.ScanLine[Y];
          R := Color1.R;
          G := Color1.G;
          B := Color1.B;
          if FirstLim = aRect.Left + 1 then begin
            R := R + RStep1;
            G := G + GStep1;
            B := B + BStep1;
            SSrc[aRect.Left].R := Round(R);
            SSrc[aRect.Left].G := Round(G);
            SSrc[aRect.Left].B := Round(B);
          end
          else for X := aRect.Left to FirstLim - 1 do begin
            SSrc[X].R := Round(R);
            SSrc[X].G := Round(G);
            SSrc[X].B := Round(B);
            R := R + RStep1;
            G := G + GStep1;
            B := B + BStep1;
          end;

          for X := FirstLim to w - SecondLim - 1 do begin
            SSrc[X].R := SSrc[X - 1].R;
            SSrc[X].G := SSrc[X - 1].G;
            SSrc[X].B := SSrc[X - 1].B;
          end;

          for X := w - SecondLim to w - 1 do begin
            SSrc[X].R := Round(R);
            SSrc[X].G := Round(G);
            SSrc[X].B := Round(B);
            R := R + RStep2;
            G := G + GStep2;
            B := B + BStep2;
          end;
        end;
      end;
      gtTopBottom : begin
        RStep1 := (AvColor.R - Color1.R) / FirstLim;
        GStep1 := (AvColor.G - Color1.G) / FirstLim;
        BStep1 := (AvColor.B - Color1.B) / FirstLim;

        RStep2 := (Color2.R - AvColor.R) / SecondLim;
        GStep2 := (Color2.G - AvColor.G) / SecondLim;
        BStep2 := (Color2.B - AvColor.B) / SecondLim;

        for Y := aRect.Top to FirstLim - 1 do begin
          SSrc := Bmp.ScanLine[Y];
          CurrentColor.R := Round(R);
          CurrentColor.G := Round(G);
          CurrentColor.B := Round(B);

          for X := aRect.Left to w - 1 do begin
            SSrc[X].R := CurrentColor.R;
            SSrc[X].G := CurrentColor.G;
            SSrc[X].B := CurrentColor.B;
          end;
          R := R + RStep1;
          G := G + GStep1;
          B := B + BStep1;
        end;

        CurrentColor.R := Round(R);
        CurrentColor.G := Round(G);
        CurrentColor.B := Round(B);
        for Y := FirstLim to h - SecondLim - 1 do begin
          SSrc := Bmp.ScanLine[Y];
          for X := aRect.Left to w - 1 do begin
            SSrc[X].R := CurrentColor.R;
            SSrc[X].G := CurrentColor.G;
            SSrc[X].B := CurrentColor.B;
          end;
        end;

        for Y := h - SecondLim to h - 1 do begin
          SSrc := Bmp.ScanLine[Y];
          for X := aRect.Left to w - 1 do begin
            SSrc[X].R := Round(R);
            SSrc[X].G := Round(G);
            SSrc[X].B := Round(B);
          end;
          R := R + RStep2;
          G := G + GStep2;
          B := B + BStep2;
        end;
      end;
      gtAsBorder : begin
        RStep1 := (AvColor.R - Color1.R) / FirstLim;
        GStep1 := (AvColor.G - Color1.G) / FirstLim;
        BStep1 := (AvColor.B - Color1.B) / FirstLim;

        RStep2 := (Color2.R - AvColor.R) / SecondLim;
        GStep2 := (Color2.G - AvColor.G) / SecondLim;
        BStep2 := (Color2.B - AvColor.B) / SecondLim;

        for Y := aRect.Top to FirstLim - 1 + aRect.Top do begin
          SSrc := Bmp.ScanLine[Y];
          CurrentColor.R := Round(R);
          CurrentColor.G := Round(G);
          CurrentColor.B := Round(B);
          dy := Y - aRect.Top;

          for X := aRect.Left to w - 1 do begin
            dx := X - aRect.Left;

            if dx < dy then begin                               // LeftTop
              SSrc[X].R := Round(Color1.R + RStep1 * dx);
              SSrc[X].G := Round(Color1.G + GStep1 * dx);
              SSrc[X].B := Round(Color1.B + BStep1 * dx);
            end
            else if X > w - dy * pr then begin                  // RightTop
              SSrc[X].R := Round(AvColor.R + RStep2 * (SecondLim - w + X));
              SSrc[X].G := Round(AvColor.G + GStep2 * (SecondLim - w + X));
              SSrc[X].B := Round(AvColor.B + BStep2 * (SecondLim - w + X));
            end
            else begin                                          // CenterTop
              SSrc[X].R := CurrentColor.R;
              SSrc[X].G := CurrentColor.G;
              SSrc[X].B := CurrentColor.B;
            end
          end;
          R := R + RStep1;
          G := G + GStep1;
          B := B + BStep1;
        end;

        CurrentColor.R := Round(R);
        CurrentColor.G := Round(G);
        CurrentColor.B := Round(B);
        for Y := FirstLim + aRect.Top to h - SecondLim - 1 do begin
          SSrc := Bmp.ScanLine[Y];
          for X := aRect.Left to w - 1 do begin
            dx := X - aRect.Left;
            if dx < FirstLim then begin            // Leftcenter
              SSrc[X].R := Round(Color1.R + RStep1 * dx);
              SSrc[X].G := Round(Color1.G + GStep1 * dx);
              SSrc[X].B := Round(Color1.B + BStep1 * dx);
            end
            else if X > W - SecondLim then begin // RightCenter
              SSrc[X].R := Round(AvColor.R + RStep2 * (SecondLim - w + X));
              SSrc[X].G := Round(AvColor.G + GStep2 * (SecondLim - w + X));
              SSrc[X].B := Round(AvColor.B + BStep2 * (SecondLim - w + X));
            end
            else begin  // CenterTop
              SSrc[X].R := CurrentColor.R;
              SSrc[X].G := CurrentColor.G;
              SSrc[X].B := CurrentColor.B;
            end
          end;
        end;

        for Y := h - SecondLim to h - 1 do begin
          SSrc := Bmp.ScanLine[Y];
          for X := aRect.Left to w - 1 do begin
            dx := X - aRect.Left;
            if dX < (h - Y) * rr  then begin // Leftbottom
              SSrc[X].R := Round(Color1.R + RStep1 * dX);
              SSrc[X].G := Round(Color1.G + GStep1 * dX);
              SSrc[X].B := Round(Color1.B + BStep1 * dX);
            end
            else if X > W - (H - Y) then begin // RightTop
              SSrc[X].R := Round(AvColor.R + RStep2 * (SecondLim - w + X));
              SSrc[X].G := Round(AvColor.G + GStep2 * (SecondLim - w + X));
              SSrc[X].B := Round(AvColor.B + BStep2 * (SecondLim - w + X));
            end
            else begin  // CenterTop
              SSrc[X].R := Round(R);
              SSrc[X].G := Round(G);
              SSrc[X].B := Round(B);
            end
          end;
          R := R + RStep2;
          G := G + GStep2;
          B := B + BStep2;
        end;
      end;
    end;
  finally
    RestoreDC(Bmp.Canvas.Handle, SavedDC);
  end;
end;

function MakeRotated90(const Bmp : TBitmap; CW : boolean; KillSource : boolean = True) : TBitmap;
var
  X, Y, w, h : integer;
  Dst, Src : TacFast32;
  Dst24, Src24 : TacFast24;
begin
  w := Bmp.Width - 1;
  h := Bmp.Height - 1;
  if Bmp.PixelFormat = pf32bit then begin
    Result := CreateBmp32(h + 1, w + 1);
    Src := TacFast32.Create;
    Dst := TacFast32.Create;
    if Src.Attach(Bmp) and Dst.Attach(Result) then begin
      if CW
        then for Y := 0 to h do for X := 0 to w do Dst24[h - Y, X] := Src24[X, Y]
        else for Y := 0 to h do for X := 0 to w do Dst24[Y, X] := Src24[w - X, Y];
    end;
    Src.Free;
    Dst.Free;
  end
  else if Bmp.PixelFormat = pf24bit then begin
    Result := CreateBmp24(h + 1, w + 1);
    Src24 := TacFast24.Create;
    Dst24 := TacFast24.Create;
    if Src24.Attach(Bmp) and Dst24.Attach(Result) then begin
      if CW
        then for Y := 0 to h do for X := 0 to w do Dst24[h - Y, X] := Src24[X, Y]
        else for Y := 0 to h do for X := 0 to w do Dst24[Y, X] := Src24[w - X, Y];
    end;
    Src24.Free;
    Dst24.Free;
  end;
  if KillSource then Bmp.Free;
end;

function CreateBmpLike(const Bmp: TBitmap): TBitmap;
begin
  Result := TBitmap.Create;
  Result.Width := Bmp.Width;
  Result.Height := Bmp.Height;
  Result.PixelFormat := Bmp.PixelFormat
end;

function CreateBmp24(const Width, Height : integer) : TBitmap;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.HandleType := bmDIB;
  Result.Width  := Width;
  Result.Height := Height;
end;

function CreateBmp32(const Width, Height : integer) : TBitmap;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf32bit;
  Result.HandleType := bmDIB;
  Result.Width  := Width;
  Result.Height := Height;
end;

procedure InitCI(var CI : TCacheInfo; Bmp : TBitmap; X : integer = 0; y : integer = 0);
begin
  CI.Bmp := bmp;
  CI.X := X;
  CI.Y := Y;
  CI.Ready := True;
end;

function ChangeColor(ColorBegin, ColorEnd : TColor; i : real) : TColor;
var
  r, g, b : integer;
begin
  r := Round(GetRValue(ColorBegin) - (GetRValue(ColorBegin) - GetRValue(ColorEnd)) * i);
  g := Round(GetGValue(ColorBegin) - (GetGValue(ColorBegin) - GetGValue(ColorEnd)) * i);
  b := Round(GetBValue(ColorBegin) - (GetBValue(ColorBegin) - GetBValue(ColorEnd)) * i);
  Result := RGB(iffi(r > MaxByte, MaxByte, r), iffi(g > MaxByte, MaxByte, g), iffi(b > MaxByte, MaxByte, b));
end;

function ColorToSColor(Color : TColor) : TsColor;
begin
  Result.A := 0;
  Result.C := Color;
end;

function AverageColor(ColorBegin, ColorEnd : TsColor) : TsColor;
begin
  Result.R := ((ColorBegin.R - ColorEnd.R) * 127 + ColorEnd.R shl 8) shr 8;
  Result.G := ((ColorBegin.G - ColorEnd.G) * 127 + ColorEnd.G shl 8) shr 8;
  Result.B := ((ColorBegin.B - ColorEnd.B) * 127 + ColorEnd.B shl 8) shr 8;
  Result.A := 0;
end;

function AverageColor(ColorBegin, ColorEnd : TColor) : TColor; overload;
var
  c1, c2 : TsColor;
begin
  c1.C := ColorBegin;
  c2.C := ColorEnd;
  Result := AverageColor(c1, c2).C;
end;

function MixColors(Color1, Color2 : TColor; PercentOfColor1 : real) : TColor;
var
  c1, c2 : TsColor;
begin
  c1.C := Color1;
  c2.C := Color2;
  c1.R := Round((c1.R * PercentOfColor1 + c2.R * (1 - PercentOfColor1)));
  c1.G := Round((c1.G * PercentOfColor1 + c2.G * (1 - PercentOfColor1)));
  c1.B := Round((c1.B * PercentOfColor1 + c2.B * (1 - PercentOfColor1)));
  Result := c1.C;
end;

procedure DrawRectangleOnDC(DC: HDC; var R: TRect; ColorTop, ColorBottom: TColor; var Width: integer);
var
  PenTop, PenBottom, OldPen : hPen;
  OldBrush : hBrush;
  Points : Array [0..2] of TPoint;
  procedure DoRect;
  var
    TopRight, BottomLeft: TPoint;
  begin
    with R do begin
      TopRight.X := Right;
      TopRight.Y := Top;
      BottomLeft.X := Left;
      BottomLeft.Y := Bottom;

      Points[0] := BottomLeft;
      Points[1] := TopLeft;
      Points[2] := TopRight;
      SelectObject(dc, PenTop);
      PolyLine(DC, PPoints(@Points)^, 3);

      Dec(BottomLeft.X);

      Points[0] := TopRight;
      Points[1] := BottomRight;
      Points[2] := BottomLeft;
      SelectObject(dc, PenBottom);
      PolyLine(DC, PPoints(@Points)^, 3);
    end;
  end;
begin
  PenTop := CreatePen(PS_SOLID, 1, ColorToRGB(ColorTop));
  PenBottom := CreatePen(PS_SOLID, 1, ColorBottom);
  OldPen := SelectObject(dc, PenTop);
  OldBrush := SelectObject(dc, GetStockObject(NULL_BRUSH));

  Dec(R.Bottom);
  Dec(R.Right);

  while Width > 0 do begin
    Dec(Width);
    DoRect;
    InflateRect(R, -1, -1);
  end;
  Inc(R.Bottom); Inc(R.Right);

  SelectObject(dc, OldPen);
  SelectObject(dc, OldBrush);
  DeleteObject(PenTop);
  DeleteObject(PenBottom);
end;

procedure TileBitmap(Canvas: TCanvas; aRect: TRect; Graphic: TGraphic);
var
  X, Y, cx, cy, w, h: Integer;
begin
{$IFDEF NOSLOWDETAILS} Exit;{$ENDIF}
  if Graphic = nil then Exit;

  w := Graphic.Width;
  h := Graphic.Height;
  if (w = 0) or (h = 0) then Exit;

  if Graphic is TBitmap then begin
    x := aRect.Left;
    while x < aRect.Right - w do begin
      y := aRect.Top;
      while y < aRect.Bottom - h do begin
        BitBlt(Canvas.Handle, x, y, w, h, TBitmap(Graphic).Canvas.Handle, 0, 0, SRCCOPY);
        inc(y, h);
      end;
      BitBlt(Canvas.Handle, x, y, w, aRect.Bottom - y, TBitmap(Graphic).Canvas.Handle, 0, 0, SRCCOPY);
      inc(x, w);
    end;
    y := aRect.Top;
    while y < aRect.Bottom - h do begin
      BitBlt(Canvas.Handle, x, y, aRect.Right - x, h, TBitmap(Graphic).Canvas.Handle, 0, 0, SRCCOPY);
      inc(y, h);
    end;
    BitBlt(Canvas.Handle, x, y, aRect.Right - x, aRect.Bottom - y, TBitmap(Graphic).Canvas.Handle, 0, 0, SRCCOPY);
  end
  else if Graphic is TJpegImage then begin
    IntersectClipRect(Canvas.Handle, aRect.Left, aRect.Top, aRect.Right, aRect.Bottom);
    cx := WidthOf(aRect) div w;
    cy := HeightOf(aRect) div h;
    for X := 0 to cx do for Y := 0 to cy do
      Canvas.Draw(aRect.Left + X * w, aRect.Top + Y * h, Graphic);
  end;
end;

{$IFNDEF ACHINTS}
procedure TileBitmap(Canvas: TCanvas; var aRect: TRect; Graphic: TGraphic; MaskData : TsMaskData; FillMode : TacFillMode = fmTiled);
var
  X, Y, w, h, NewX, NewY, Tmp: Integer;
begin
{$IFDEF NOSLOWDETAILS} Exit;{$ENDIF}
  if (Graphic = nil) or Graphic.Empty then begin // If bitmap in the MaskData
    if MaskData.Manager = nil then Exit;
    w := WidthOf(MaskData.R);
    h := HeightOf(MaskData.R);
    Tmp := 0;
    case FillMode of
      fmStretched : begin
        StretchBlt(Canvas.Handle, aRect.Left, aRect.Top, WidthOf(aRect), HeightOf(aRect),
                   TsSkinManager(MaskData.Manager).MasterBitmap.Canvas.Handle,
                   MaskData.R.Left, MaskData.R.Top, w, h, SRCCOPY);
        aRect := Rect(-1, -1, -1, -1);
      end;
      fmStretchHorz : begin
        StretchBlt(Canvas.Handle, aRect.Left, aRect.Top, WidthOf(aRect), h,
                   TsSkinManager(MaskData.Manager).MasterBitmap.Canvas.Handle,
                   MaskData.R.Left, MaskData.R.Top, w, h, SRCCOPY);
        aRect := Rect(aRect.Left, aRect.Top + h, aRect.Right, aRect.Bottom);
      end;
      fmStretchVert : begin
        StretchBlt(Canvas.Handle, aRect.Left, aRect.Top, w, HeightOf(aRect),
                   TsSkinManager(MaskData.Manager).MasterBitmap.Canvas.Handle,
                   MaskData.R.Left, MaskData.R.Top, w, h, SRCCOPY);
        aRect := Rect(aRect.Left + w, aRect.Top, aRect.Right, aRect.Bottom);
      end;
      fmDisTiled : begin
        x := aRect.Left;
        NewX := aRect.Right - w;
        NewY := aRect.Bottom - h;
        while x < NewX do begin
          y := aRect.Top;
          while y < NewY do begin
            BitBlt(Canvas.Handle, x, y, w, h, TsSkinManager(MaskData.Manager).MasterBitmap.Canvas.Handle, MaskData.R.Left, MaskData.R.Top, SRCCOPY);
            inc(y, h);
          end;
          inc(x, w);
        end;
        y := aRect.Top;
        while y < NewY do begin
          BitBlt(Canvas.Handle, x, y, aRect.Right - x, h, TsSkinManager(MaskData.Manager).MasterBitmap.Canvas.Handle, MaskData.R.Left, MaskData.R.Top, SRCCOPY);
          inc(y, h);
        end;
        aRect := Rect(-1, -1, -1, -1);
      end
      else begin
        x := aRect.Left;
        case FillMode of
          fmTiled : begin
            NewX := aRect.Right - w;
            NewY := aRect.Bottom - h;
          end;
          fmTiledHorz : begin
            NewX := aRect.Right - w;
            NewY := aRect.Top;
          end;
          fmTiledVert : begin
            NewX := aRect.Left;
            NewY := aRect.Bottom - h;
          end;
          fmTileHorBtm : begin
            Tmp := aRect.Top;
            aRect.Top := aRect.Bottom - h;
            NewX := aRect.Right - w;
            NewY := aRect.Top;
          end
          else begin
            Tmp := aRect.Left;
            aRect.Left := aRect.Right - w;
            NewX := aRect.Left;
            NewY := aRect.Bottom - h;
          end;
        end;
        while x < NewX do begin
          y := aRect.Top;
          while y < NewY do begin
            BitBlt(Canvas.Handle, x, y, w, h, TsSkinManager(MaskData.Manager).MasterBitmap.Canvas.Handle, MaskData.R.Left, MaskData.R.Top, SRCCOPY);
            inc(y, h);
          end;
          BitBlt(Canvas.Handle, x, y, w, aRect.Bottom - y, TsSkinManager(MaskData.Manager).MasterBitmap.Canvas.Handle, MaskData.R.Left, MaskData.R.Top, SRCCOPY);
          inc(x, w);
        end;
        y := aRect.Top;
        while y < NewY do begin
          BitBlt(Canvas.Handle, x, y, aRect.Right - x, h, TsSkinManager(MaskData.Manager).MasterBitmap.Canvas.Handle, MaskData.R.Left, MaskData.R.Top, SRCCOPY);
          inc(y, h);
        end;
        BitBlt(Canvas.Handle, x, y, aRect.Right - x, aRect.Bottom - y, TsSkinManager(MaskData.Manager).MasterBitmap.Canvas.Handle, MaskData.R.Left, MaskData.R.Top, SRCCOPY);

        case FillMode of
          fmTiled : aRect := Rect(-1, -1, -1, -1);
          fmTiledHorz : aRect := Rect(aRect.Left, aRect.Top + h, aRect.Right, aRect.Bottom);
          fmTiledVert : aRect := Rect(aRect.Left + w, aRect.Top, aRect.Right, aRect.Bottom);
          fmTileHorBtm : aRect := Rect(aRect.Left, Tmp, aRect.Right, aRect.Bottom - h);
          fmTileVertRight : aRect := Rect(Tmp, arect.Top, aRect.Right - w, aRect.Bottom);
        end;
      end
    end
  end
  else begin
    TileBitmap(Canvas, aRect, Graphic);
    aRect := Rect(-1, -1, -1, -1);
  end;
end;

procedure TileMasked(Bmp: TBitmap; var aRect: TRect; CI : TCacheInfo; MaskData : TsMaskData; FillMode : TacFillMode = fmDisTiled);
var
  X, Y, w, h, NewX, NewY: Integer;
  mr : TRect;
begin
  if MaskData.Manager = nil then Exit;
  w := WidthOf(MaskData.R) div MaskData.ImageCount;
  h := HeightOf(MaskData.R) div 2;
  case FillMode of
    fmTiled : begin
      x := aRect.Left;
      NewX := aRect.Right - w;
      NewY := aRect.Bottom - h;
      mr := MaskData.R;
      while x < NewX do begin
        y := aRect.Top;
        while y < NewY do begin
          CopyMasterRect(Rect(x, y, x + w, y + h), mr, Bmp, CI, MaskData);
          inc(y, h);
        end;
//        BitBlt(Canvas.Handle, x, y, w, aRect.Bottom - y, TsSkinManager(MaskData.Manager).MasterBitmap.Canvas.Handle, MaskData.R.Left, MaskData.R.Top, SRCCOPY);
        inc(x, w);
      end;
      y := aRect.Top;
      while y < NewY do begin
        CopyMasterRect(Rect(x, y, aRect.Right, y + h), mr, Bmp, CI, MaskData);
        inc(y, h);
      end;
//      BitBlt(Canvas.Handle, x, y, aRect.Right - x, aRect.Bottom - y, TsSkinManager(MaskData.Manager).MasterBitmap.Canvas.Handle, MaskData.R.Left, MaskData.R.Top, SRCCOPY);
      aRect := Rect(-1, -1, -1, -1);
    end;
    fmStretched : begin
{        StretchBlt(Canvas.Handle, aRect.Left, aRect.Top, WidthOf(aRect), HeightOf(aRect),
                 TsSkinManager(MaskData.Manager).MasterBitmap.Canvas.Handle,
                 MaskData.R.Left, MaskData.R.Top, w, h, SRCCOPY);
      aRect := Rect(-1, -1, -1, -1);}
    end;
    fmStretchHorz : begin
{        StretchBlt(Canvas.Handle, aRect.Left, aRect.Top, WidthOf(aRect), h,
                 TsSkinManager(MaskData.Manager).MasterBitmap.Canvas.Handle,
                 MaskData.R.Left, MaskData.R.Top, w, h, SRCCOPY);
      aRect := Rect(aRect.Left, aRect.Top + h, aRect.Right, aRect.Bottom);}
    end;
    fmStretchVert : begin
{        StretchBlt(Canvas.Handle, aRect.Left, aRect.Top, w, HeightOf(aRect),
                 TsSkinManager(MaskData.Manager).MasterBitmap.Canvas.Handle,
                 MaskData.R.Left, MaskData.R.Top, w, h, SRCCOPY);
      aRect := Rect(aRect.Left + w, aRect.Top, aRect.Right, aRect.Bottom);}
    end;
    fmDisTiled : begin
      x := aRect.Left;
      NewX := aRect.Right - w;
      NewY := aRect.Bottom - h;
      mr := MaskData.R;
      y := 0;
      while x < NewX do begin
        y := aRect.Top;
        while y < NewY do begin
          CopyMasterRect(Rect(x, y, x + w, y + h), mr, Bmp, CI, MaskData);
//          BlendGlyphByMask(Rect(x, y, x + w, y + h), mr, Bmp, TsSkinManager(MaskData.Manager).MasterBitmap, clFuchsia, 1, MaskData);
          inc(y, h);
        end;
        inc(x, w);
      end;
      if CI.Ready then begin
        BitBlt(Bmp.Canvas.Handle, aRect.Left, y, WidthOf(aRect), aRect.Bottom - y, CI.Bmp.Canvas.Handle, aRect.Left + CI.X, Y + CI.Y, SRCCOPY);
        BitBlt(Bmp.Canvas.Handle, x, aRect.Top, aRect.Right - x, HeightOf(aRect), CI.Bmp.Canvas.Handle, x + CI.X, aRect.Top + CI.Y, SRCCOPY);
      end
      else begin
        FillDC(Bmp.Canvas.Handle, Rect(aRect.Left, y, aRect.Right, aRect.Bottom), CI.FillColor);
        FillDC(Bmp.Canvas.Handle, Rect(x, aRect.Top, aRect.Right, aRect.Bottom), CI.FillColor);
      end;
      aRect := Rect(-1, -1, -1, -1);
    end
  end
end;

procedure TileBitmapFromOther(Canvas: TCanvas; aRect: TRect; MaskData : TsMaskData);
var
  X, Y, w, h: Integer;
begin
  w := WidthOf(MaskData.R);
  h := HeightOf(MaskData.R);
  x := aRect.Left;
  while x < aRect.Right - w do begin
    y := aRect.Top;
    while y < aRect.Bottom - h do begin
      BitBlt(Canvas.Handle, x, y, w, h, MaskData.Bmp.Canvas.Handle, MaskData.R.Left, MaskData.R.Top, SRCCOPY);
      inc(y, h);
    end;
    BitBlt(Canvas.Handle, x, y, w, aRect.Bottom - y, MaskData.Bmp.Canvas.Handle, MaskData.R.Left, MaskData.R.Top, SRCCOPY);
    inc(x, w);
  end;
  y := aRect.Top;
  while y < aRect.Bottom - h do begin
    BitBlt(Canvas.Handle, x, y, aRect.Right - x, h, MaskData.Bmp.Canvas.Handle, MaskData.R.Left, MaskData.R.Top, SRCCOPY);
    inc(y, h);
  end;
  BitBlt(Canvas.Handle, x, y, aRect.Right - x, aRect.Bottom - y, MaskData.Bmp.Canvas.Handle, MaskData.R.Left, MaskData.R.Top, SRCCOPY);
end;
{$ENDIF}

procedure CalcButtonLayout(const Client: TRect; const GlyphSize: TPoint; const TextRectSize: TSize; Layout: TButtonLayout;
            Alignment: TAlignment; Margin, Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect; BiDiFlags: LongInt);      {RL}
var
  TextPos, ClientSize, TextSize, TotalSize: TPoint;
  dh : integer;
begin
  if (BiDiFlags and DT_RIGHT) = DT_RIGHT then
    if Layout = blGlyphLeft then Layout := blGlyphRight else if Layout = blGlyphRight then Layout := blGlyphLeft;
  { calculate the item sizes }
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom - Client.Top);

  TextBounds := Rect(0, 0, TextRectSize.cx, TextRectSize.cy);
  TextSize := Point(TextRectSize.cx,TextRectSize.cy);

  { If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if Layout in [blGlyphLeft, blGlyphRight] then begin
    GlyphPos.Y := (ClientSize.Y - GlyphSize.Y + 1) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.Y + 1) div 2;
  end
  else begin
    GlyphPos.X := (ClientSize.X - GlyphSize.X + 1) div 2;
    TextPos.X := (ClientSize.X - TextSize.X + 1) div 2;
  end;

  { if there is no text or no bitmap, then Spacing is irrelevant }
  if (TextSize.X = 0) or (GlyphSize.X = 0) then Spacing := 0;

  if Margin = -1 then begin // adjust Margin and Spacing
    if Spacing < 0 then begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then Margin := (ClientSize.X - TotalSize.X) div 3 else Margin := (ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end
    else begin
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y + Spacing + TextSize.Y);
      if Alignment = taCenter then begin
        if Layout in [blGlyphLeft, blGlyphRight]
          then Margin := (ClientSize.X - TotalSize.X + 1) div 2
          else Margin := (ClientSize.Y - TotalSize.Y + 1) div 2;
      end
      else Margin := 2;
    end;
  end
  else begin
    if Spacing < 0 then begin
      TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y - (Margin + GlyphSize.Y));
      if Layout in [blGlyphLeft, blGlyphRight] then Spacing := (TotalSize.X - TextSize.X) div 2 else Spacing := (TotalSize.Y - TextSize.Y) div 2;
    end;
  end;

  case Layout of
    blGlyphLeft: begin
      case Alignment of
        taLeftJustify: begin
          GlyphPos.X := Margin;
          TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
          TextBounds.Right := TextRectSize.cx;
        end;
        taCenter: begin
          Margin := (ClientSize.X - TextSize.X - Spacing - GlyphSize.X) div 2;
          GlyphPos.X := Margin;
          TextPos.X := GlyphPos.X + Spacing + GlyphSize.X;
        end;
        taRightJustify: begin
          TextPos.X := ClientSize.X - Margin - TextSize.X;
          GlyphPos.X := TextPos.X - Spacing - GlyphSize.X;
        end;
      end;
    end;
    blGlyphRight: begin
      case Alignment of
        taLeftJustify: begin
          GlyphPos.X := Margin + TextSize.X + Spacing;
          TextPos.X := GlyphPos.X - Spacing - TextRectSize.cx;
        end;
        taCenter: begin
          Margin := (ClientSize.X - TextSize.X - Spacing - GlyphSize.X) div 2;
          TextPos.X := Margin;
          GlyphPos.X := TextPos.X + Spacing + TextSize.X;
        end;
        taRightJustify: begin
          GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
          TextPos.X := GlyphPos.X - Spacing - TextRectSize.cx;
        end;
      end;
    end;
    blGlyphTop: begin
      dh := (ClientSize.y - GlyphSize.Y - Spacing - TextRectSize.cy) div 2 - Margin;
      GlyphPos.Y := Margin + dh;
      TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
    end;
    blGlyphBottom: begin
      dh := (ClientSize.y - GlyphSize.Y - Spacing - TextRectSize.cy) div 2 - Margin;
      GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y - dh;
      TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
    end;
  end;

  { fixup the result variables }
  with GlyphPos do begin
    Inc(X, Client.Left);
    Inc(Y, Client.Top);
  end;

  OffsetRect(TextBounds, TextPos.X + Client.Left, TextPos.Y + Client.Top);
end;

function GetFontHeight(hFont : HWnd): integer;
var
  DC: HDC;
  SaveFont: LongInt;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  try
    SaveFont := SelectObject(DC, hFont);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
    Result := Metrics.tmHeight + 6;
  finally
    ReleaseDC(0, DC);
  end;
end;

function GetStringSize(hFont : hgdiobj; const Text : acString): TSize;
var
  DC: HDC;
  SaveFont: LongInt;
begin
  DC := GetDC(0);
  try
    SaveFont := SelectObject(DC, hFont);
{$IFDEF TNTUNICODE}
    if not GetTextExtentPoint32W(DC, PWideChar(Text), Length(Text), Result) then begin
{$ELSE}
    if not GetTextExtentPoint32(DC, PChar(Text), Length(Text), Result) then begin
{$ENDIF}
      Result.cx := 0;
      Result.cy := 0;
    end;
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;
end;                             

function LoadJpegOrBmp(Image: TPicture; const FileName: string; Gray: boolean):boolean;
var
  s: string;
  j: TJpegImage;
begin
  Result := False;
  if FileExists(FileName) then begin
    s := ExtractFileExt(FileName);
    if (UpperCase(s)='.JPG') or (UpperCase(s)='.JPEG') then begin
      j := TJPEGImage.Create;
      try
      j.LoadFromFile(FileName);
      j.Grayscale := Gray;
      Image.Assign(TGraphic(j));
      finally
        FreeAndNil(j);
      end;
    end
    else if (UpperCase(s)='.BMP') then begin
      Image.LoadFromFile(FileName);
      Image.Bitmap.PixelFormat := pf24Bit;
      if Gray then begin
        GrayScale(Image.Bitmap);
      end;
    end;
  end
  else begin
    Image.Assign(nil);
  end;
end;

procedure FocusRect(Canvas : TCanvas; R : TRect);
var
  x, y, dx, dy : integer;
begin
  dx := WidthOf(R) div 3;
  dy := HeightOf(R) div 3;
  for x := 0 to dx do begin
    Canvas.Pixels[R.Left + 3 * x, R.Top] := clBlack;
    Canvas.Pixels[R.Left + 3 * x, R.Bottom] := clBlack;

    Canvas.Pixels[R.Left + 3 * x - 1, R.Top - 1] := clWhite;
    Canvas.Pixels[R.Left + 3 * x - 1, R.Bottom - 1] := clWhite;
  end;
  for y := 0 to dy do begin
    Canvas.Pixels[R.Left, R.Top + 3 * y] := clBlack;
    Canvas.Pixels[R.Right, R.Top + 3 * y] := clBlack;

    Canvas.Pixels[R.Left - 1, R.Top + 3 * y - 1] := clWhite;
    Canvas.Pixels[R.Right - 1, R.Top + 3 * y - 1] := clWhite;
  end;
end;

initialization
  if @UpdateLayeredWindow = nil then begin
    User32Lib := LoadLibrary('USER32');
    try
      if User32Lib <> 0 then UpdateLayeredWindow := GetProcAddress(User32Lib, 'UpdateLayeredWindow') else @UpdateLayeredWindow := nil;
    finally
    end;
  end;

finalization
  if User32Lib <> 0 then FreeLibrary(User32Lib);

end.

end.



