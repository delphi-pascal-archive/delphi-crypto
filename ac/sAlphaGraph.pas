unit sAlphaGraph;
{$I sDefs.inc}

interface
// universal formula for blending // Peter Result := ((Src1 - Src2) * PercentOfSrc1 + Src2 / 256) shr 8;
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    ComCtrls, sConst, ExtCtrls, Jpeg, math{$IFNDEF ACHINTS}, sMaskData, sCommonData{$ENDIF};

{$IFNDEF NOTFORHELP}
type
  TsCorner = (scLeftTop, scLeftBottom, scRightTop, scRightBottom);
  TsCorners = set of TsCorner;

  TacFast24 = class
  private
    FBitmap: TBitmap;
    FWidth, FHeight: Integer;
    FDelta: Integer;
    function GetPixel(X, Y: Integer): TsColor;
    procedure SetPixel(X, Y: Integer; const Value: TsColor);
  public
    FStart, FLength, DstStart : longint;
    function Attach(Bmp: TBitmap) : boolean;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Pixels[X, Y: Integer]: TsColor read GetPixel write SetPixel; default;
  end;

  TacFast32 = class
  private
    FBitmap: TBitmap;
    FWidth, FHeight: Integer;
    FDelta: Integer;
    function GetPixel(X, Y: Integer): TsColor;
    procedure SetPixel(X, Y: Integer; const Value: TsColor);
  public
    FStart, FLength, DstStart : longint;
    function Attach(Bmp: TBitmap) : boolean;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Pixels[X, Y: Integer]: TsColor read GetPixel write SetPixel; default;
  end;

  TacFastSum24 = class
  private
    FBmpSrc, FBmpDst: TBitmap;
    FWidthDst, FHeightDst: Integer;
    FStartSrc, FStartDst : longint;
    FDeltaSrc, FDeltaDst: Integer;
  public
    Alpha : byte;
    DstX1, DstY1, DstX2, DstY2, SrcX1, SrcY1, SrcX2, SrcY2 : longint;
    procedure BlendBitmaps; pascal;
    procedure BlendBitmapsRect; pascal;
    function Attach(BmpSrc, BmpDst: TBitmap) : boolean;
  end;

var
  Fast24Src, Fast24Dst : TacFast24;
  FastSum24 : TacFastSum24;
  Fast32Src : TacFast32;

{$ENDIF} // NOTFORHELP
procedure BlendColorRect(Bmp: TBitmap; R: TRect; Transparency: TPercent; Color: TColor);

{$IFNDEF ACHINTS}
// Copy with AlphaMask from MasterBmp (get transp. pixels from parent)
procedure CopyMasterRect(R1, R2 : TRect; Bmp : TBitmap; CI : TCacheInfo; MaskData : TsMaskData);
procedure DrawSmallSkinRect(Bmp : TBitmap; R : TRect; Filling : boolean; ci : TCacheInfo; MaskData : TsMaskData; State : integer);
// Skinned rectangle with transparent corners
procedure DrawSkinRect(const Bmp : TBitmap; const R : TRect; const Filling : boolean; const ci : TCacheInfo; const MaskData : TsMaskData; const State : integer; const UpdateCorners : boolean; SkinManager : TObject = nil);
// Skip transparent part of corners
procedure DrawSkinGlyph(Bmp : TBitmap; P : TPoint; State, AddedTransparency : integer; MaskData : TsMaskData; const CI : TCacheInfo);
// Transparency - percent of the bmp visibility
procedure BmpDisabledKind(Bmp : TBitmap; DisabledKind : TsDisabledKind; Parent : TControl; CI : TCacheInfo; Offset : TPoint);
{$IFNDEF NOTFORHELP}
// Skip transparent pixels
procedure CopyMasterRectA(R1, R2 : TRect; Bmp : TBitmap; CI : TCacheInfo; MaskData : TsMaskData);
procedure BlendGlyphByMask(R1, R2 : TRect; Bmp1, Bmp2 : TBitmap; TransColor : TColor; AddedTransparency : integer; MaskData : TsMaskData);
procedure UpdateCorners(SkinData : TsCommonData; State : integer); overload;
procedure UpdateCorners(SkinData : TsCommonData; State : integer; Corners : TsCorners); overload;
//procedure PaintTransPixels(const DstBmp : TBitmap; R: TRect; const MaskData : TsMaskData; CI : TCacheInfo; State : integer; SkinManager : TObject);

{$ENDIF} // NOTFORHELP
{$ENDIF}
implementation

Uses acntUtils, sGraphUtils{$IFNDEF ACHINTS}, sDefaults, sVclUtils, sSkinManager{$ENDIF};

procedure BlendColorRect(Bmp: TBitmap; R: TRect; Transparency: TPercent; Color: TColor);
var
  BmpLine : PRGBArray;
  BlendColor : TsColor;
  x, y : integer;
  rT, tR : real;
begin
  if R.Left < 0 then R.Left := 0;
  if R.Top < 0 then R.Top := 0;
  if R.Right > Bmp.Width - 1 then R.Right := Bmp.Width - 1;
  if R.Bottom > Bmp.Height - 1 then R.Bottom := Bmp.Height - 1;
  BlendColor.C := ColorToRGB(Color);
  rT := Transparency / 100;
  tR := 1 - rT;
  for y := R.Top to R.Bottom do begin
    BmpLine := Bmp.ScanLine[y];
    for x := R.Left to R.Right do begin
      BmpLine[X].R := Round(BmpLine[X].R * rT + BlendColor.R * tR);
      BmpLine[X].G := Round(BmpLine[X].G * rT + BlendColor.G * tR);
      BmpLine[X].B := Round(BmpLine[X].B * rT + BlendColor.B * tR);
    end;
  end;
end;

{$IFNDEF ACHINTS}
procedure BmpDisabledKind(Bmp : TBitmap; DisabledKind : TsDisabledKind; Parent : TControl; CI : TCacheInfo; Offset : TPoint);
var
  C : TsColor;
  R : TRect;
begin
  if (dkGrayed in DisabledKind) then begin
    GrayScale(Bmp);
  end;
  if (dkBlended in DisabledKind) then begin
    R := Rect(0, 0, Bmp.Width, Bmp.Height);
    if CI.Ready then begin
      C.A := 255;
      OffsetRect(R, CI.X + Offset.X, CI.Y + Offset.Y);
      BlendTransRectangle(Bmp, 0, 0, CI.Bmp, R, DefDisabledBlend, C);
    end
    else begin
      if CI.FillColor <> clFuchsia
        then C.C := CI.FillColor
        else C.C := GetControlColor(Parent);
      BlendColorRect(Bmp, R, 50, C.C);
    end;
  end;
end;

procedure DrawSmallSkinRect(Bmp : TBitmap; R : TRect; Filling : boolean; ci : TCacheInfo; MaskData : TsMaskData; State : integer);
var
  x, y : integer;
  w, h : integer;
  dw, dh : integer;
  mw, mh, minhp, minwp, minh, minw : integer;
begin
  if MaskData.Bmp = nil then with TsSkinManager(MaskData.Manager) do begin
    if MaskData.Manager = nil then Exit;
    if (WidthOf(R) < 2) or (HeightOf(R) < 2) then Exit;

    if MaskData.ImageCount = 0 then exit;
    if State >= MaskData.ImageCount then State := MaskData.ImageCount - 1;
    dw := State * WidthOf(MaskData.R) div (MaskData.ImageCount);        // Width of mask
    dh := HeightOf(MaskData.R) div (1 + MaskData.MaskType);             // Height of mask

    w := WidthOf(MaskData.R) div (3 * MaskData.ImageCount);
    h := HeightOf(MaskData.R) div (3 * (1 + MaskData.MaskType));

    if WidthOf(R) < w * 2 then mw := WidthOf(R) div 2 else mw := 0;
    if HeightOf(R) < h * 2 then mh := HeightOf(R) div 2 else mh := 0;
    
    if mh > 0 then begin
      minh := mh;
      if HeightOf(R) mod 2 <> 0 then minhp := minh + 1 else minhp := minh;
    end else begin
      minh := h;
      minhp := h;
    end;
    if mw > 0 then begin
      minw := mw;
      if WidthOf(R) mod 2 <> 0 then minwp := minw + 1 else minwp := minw;
    end else begin
      minw := w;
      minwp := w;
    end;

    if MaskData.MaskType = 0 then begin
      // left - top
      CopyTransRect(Bmp, MasterBitmap, R.Left, R.Top,
                    Rect(MaskData.R.Left + dw,
                          MaskData.R.Top,
                          MaskData.R.Left + dw + minw,
                          MaskData.R.Top + minh),
                    clFuchsia, CI, True);
      // left - middle
      y := R.Top + h;
      if MaskData.DrawMode and BDM_STRETCH = 0 then begin
        while y < R.Bottom - h do begin
          BitBlt(Bmp.Canvas.Handle, R.Left, y, minw, h, MasterBitmap.Canvas.Handle, MaskData.R.Left + dw, MaskData.R.Top + h, SRCCOPY);
          inc(y, h);
        end;
        if y < R.Bottom - h then BitBlt(Bmp.Canvas.Handle, R.Left, y, minw, h,
               MasterBitmap.Canvas.Handle, MaskData.R.Left + dw, MaskData.R.Top + h, SRCCOPY);
      end
      else StretchBlt(Bmp.Canvas.Handle,
                 R.Left, y, R.Left + minw, R.Bottom - minh - y,
                 MasterBitmap.Canvas.Handle,
                 MaskData.R.Left + dw, MaskData.R.Top + h, minw, h,
                 SRCCOPY);

      // top - middle
      x := R.Left + minw;
      if MaskData.DrawMode and BDM_STRETCH = 0 then begin
        while x < R.Right - w - minw do begin
          BitBlt(Bmp.Canvas.Handle, x, R.Top, w, minh, MasterBitmap.Canvas.Handle, MaskData.R.Left + dw + w, MaskData.R.Top, SRCCOPY);
          inc(x, w);
        end;
        if x < R.Right - minw then BitBlt(Bmp.Canvas.Handle, x, R.Top, R.Right - minw - x, minh, MasterBitmap.Canvas.Handle, MaskData.R.Left + dw + w, MaskData.R.Top, SRCCOPY);
      end
      else StretchBlt(Bmp.Canvas.Handle,
                 x, R.Top, R.Right - minw - x, minh,
                 MasterBitmap.Canvas.Handle,
                 MaskData.R.Left + dw + w, MaskData.R.Top, w, minh,
                 SRCCOPY); // v4.52

      // left - bottom
      CopyTransRect(Bmp, MasterBitmap, R.Left, R.Bottom - minhp, Rect(MaskData.R.Left + dw, MaskData.R.Bottom - minhp, MaskData.R.Left + dw + minw - 1,
                         MaskData.R.Bottom - 1), clFuchsia, CI, True);
      // bottom - middle
      x := R.Left + minw;
      if MaskData.DrawMode and BDM_STRETCH = 0 then begin
        while x < R.Right - w - minw do begin
          BitBlt(Bmp.Canvas.Handle, x, R.Bottom - minh, w, minh,
               MasterBitmap.Canvas.Handle, MaskData.R.Left + dw + w, MaskData.R.Bottom - minh, SRCCOPY);
          inc(x, w);
        end;
        if x < R.Right - minw then BitBlt(Bmp.Canvas.Handle, x, R.Bottom - minh, R.Right - minw - x, minh,
                                          MasterBitmap.Canvas.Handle, MaskData.R.Left + dw + w, MaskData.R.Bottom - minh, SRCCOPY);
      end
      else StretchBlt(Bmp.Canvas.Handle, x, R.Bottom - minh, R.Right - minw - x, minh, MasterBitmap.Canvas.Handle,
                 MaskData.R.Left + dw + w, MaskData.R.Bottom - minh, w, minh, SRCCOPY);
      // right - bottom
      CopyTransRect(Bmp, MasterBitmap, R.Right - minwp, R.Bottom - minhp,
                    Rect(MaskData.R.Left + dw + 3 * w - minwp, MaskData.R.Bottom - minhp, MaskData.R.Left + dw + 3 * w - 1, MaskData.R.Bottom - 1),
                    clFuchsia, CI, True);
      // right - top
      CopyTransRect(Bmp, MasterBitmap, R.Right - minwp, R.Top,
                    Rect(MaskData.R.Left + dw + 3 * w - minwp, MaskData.R.Top, MaskData.R.Left + dw + 3 * w - 1, MaskData.R.Top + minh - 1),
                    clFuchsia, CI, True);
      // right - middle
      y := R.Top + h;
      while y < R.Bottom - h do begin
        BitBlt(Bmp.Canvas.Handle, R.Right - minwp, y, minwp, h,
             MasterBitmap.Canvas.Handle, MaskData.R.Left + dw + 3 * w - minwp, MaskData.R.Top + h, SRCCOPY);
        inc(y, h);
      end;
      if y < R.Bottom - h then begin
        BitBlt(Bmp.Canvas.Handle, R.Right - minwp, y, minwp, R.Bottom - h - y,
             MasterBitmap.Canvas.Handle, MaskData.R.Left + dw + 3 * w - minwp, MaskData.R.Top + h, SRCCOPY);
      end;

      // Fill
      if Filling and (MaskData.DrawMode and BDM_FILL = BDM_FILL) then begin
        y := R.Top + h;
        while y < R.Bottom - 2 * h do begin
          x := R.Left + w;
          while x < R.Right - 2 * w do begin
            BitBlt(Bmp.Canvas.Handle, x, y, w, h,
                 MasterBitmap.Canvas.Handle, MaskData.R.Left + dw + w, MaskData.R.Top + h, SRCCOPY);
            inc(x, w);
          end;
          if x < R.Right - w then begin
            BitBlt(Bmp.Canvas.Handle, x, y, R.Right - w - x,  R.Bottom - h - y,
                 MasterBitmap.Canvas.Handle, MaskData.R.Left + dw + w, MaskData.R.Top + h, SRCCOPY);
          end;
          inc(y, h);
        end;
        x := R.Left + w;
        if y < R.Bottom - h then begin
          while x < R.Right - 2 * w do begin
            BitBlt(Bmp.Canvas.Handle, x, y, w,  R.Bottom - h - y,
                 MasterBitmap.Canvas.Handle, MaskData.R.Left + dw + w, MaskData.R.Top + h, SRCCOPY);
            inc(x, w);
          end;
          if x < R.Right - w then begin
            BitBlt(Bmp.Canvas.Handle, x, y, R.Right - w - x, R.Bottom - h - y,
                 MasterBitmap.Canvas.Handle, MaskData.R.Left + dw + w, MaskData.R.Top + h, SRCCOPY);
          end
        end;
      end;

    end
    else begin
      CopyMasterRect(Rect(R.Left, R.Top, R.Left + minw + 1, R.Top + minh + 1),
                       Rect(MaskData.R.Left + dw, MaskData.R.Top, MaskData.R.Left + dw + minw, MaskData.R.Top + minh),
                       Bmp, ci, MaskData);
      // left - middle
      y := R.Top + h;
      while y < R.Bottom - h do begin
        CopyMasterRect(Rect(R.Left, y, R.Left + minw + 1, y + h + 1),
                       Rect(MaskData.R.Left + dw, MaskData.R.Top + h, MaskData.R.Left + dw + minw, MaskData.R.Top + 2 * h),
                       Bmp, CI, MaskData);
        inc(y, h);
      end;

      if y < R.Bottom - h then begin
        CopyMasterRect(Rect(R.Left, y, R.Left + minw, R.Bottom - h), Rect(MaskData.R.Left + dw, MaskData.R.Top + h, MaskData.R.Left + dw + minw, MaskData.R.Top + dh - h), Bmp, CI, MaskData);
      end;
      // top - middle
      x := R.Left + w;
      while x < R.Right - 2 * w do begin
        CopyMasterRect(Rect(x, R.Top, x + w, R.Top + minh), Rect(MaskData.R.Left + dw + w, MaskData.R.Top, MaskData.R.Left + dw + 2 * w, MaskData.R.Top + minh), Bmp, CI, MaskData);
        inc(x, w);
      end;
      if x < R.Right - w then begin
        CopyMasterRect(Rect(x, R.Top, R.Right - w, R.Top + minh), Rect(MaskData.R.Left + dw + w, MaskData.R.Top, MaskData.R.Left + dw + 2 * w, MaskData.R.Top + minh), Bmp, CI, MaskData);
      end;
      // left - bottom
      CopyMasterRect(Rect(R.Left, R.Bottom - minhp, R.Left + minw, R.Bottom), Rect(MaskData.R.Left + dw, MaskData.R.Top + dh - minhp, MaskData.R.Left + dw + minw, MaskData.R.Top + dh), Bmp, CI, MaskData);
      // bottom - middle
      x := R.Left + w;
      while x < R.Right - 2 * w do begin
        CopyMasterRect(Rect(x, R.Bottom - minhp, x + w, R.Bottom), Rect(MaskData.R.Left + dw + w, MaskData.R.Top + dh - minhp, MaskData.R.Left + dw + 2 * w, MaskData.R.Top + dh), Bmp, CI, MaskData);
        inc(x, w);
      end;
      if x < R.Right - w then begin
        CopyMasterRect(Rect(x, R.Bottom - minhp, R.Right - w, R.Bottom), Rect(MaskData.R.Left + dw + w, MaskData.R.Top + dh - minhp, MaskData.R.Left + dw + 2 * w, MaskData.R.Top + dh), Bmp, CI, MaskData);
      end;
      // right - bottom
      CopyMasterRect(Rect(R.Right - minwp, R.Bottom - minhp, R.Right, R.Bottom), Rect(MaskData.R.Left + dw + 3 * w - minwp, MaskData.R.Top + dh - minhp, MaskData.R.Left + dw + 3 * w, MaskData.R.Top + dh), Bmp, CI, MaskData);
      // right - top
      CopyMasterRect(Rect(R.Right - minwp, R.Top, R.Right, R.Top + minh), Rect(MaskData.R.Left + dw + 3 * w - minwp, MaskData.R.Top, MaskData.R.Left + dw + 3 * w, MaskData.R.Top + minh), Bmp, CI, MaskData);
      // right - middle
      y := R.Top + h;
      while y < R.Bottom - h do begin
        CopyMasterRect(Rect(R.Right - minwp, y, R.Right, y + h), Rect(MaskData.R.Left + dw + 3 * w - minwp, MaskData.R.Top + h, MaskData.R.Left + dw + 3 * w, MaskData.R.Top + 2 * h), Bmp, CI, MaskData);
        inc(y, h);
      end;
      if y < R.Bottom - h then begin
        CopyMasterRect(Rect(R.Right - minwp, y, R.Right, R.Bottom - h), Rect(MaskData.R.Left + dw + 3 * w - minwp, MaskData.R.Top + h, MaskData.R.Left + dw + 3 * w, MaskData.R.Top + 2 * h), Bmp, CI, MaskData);
      end;
      // Fill
      if Filling and (MaskData.DrawMode and BDM_FILL = BDM_FILL) then begin
        y := R.Top + h;
        while y < R.Bottom - h do begin
          x := R.Left + w;
          while x < R.Right - 2 * w do begin
            CopyMasterRect(Rect(x, y, x + w, y + h), Rect(MaskData.R.Left + dw + w, MaskData.R.Top + h, MaskData.R.Left + dw + 2 * w, MaskData.R.Top + 2 * h), Bmp, EmptyCI, MaskData);
            inc(x, w);
          end;
          if x < R.Right - w then begin
            CopyMasterRect(Rect(x, y, R.Right - w, y + h), Rect(MaskData.R.Left + dw + w, MaskData.R.Top + h, MaskData.R.Left + dw + 2 * w, MaskData.R.Top + 2 * h), Bmp, EmptyCI, MaskData);
          end;
          inc(y, h);
        end;
        x := R.Left + w;
        if y < R.Bottom - h then begin
          while x < R.Right - 2 * w do begin
            CopyMasterRect(Rect(x, y, x + w, R.Bottom - h), Rect(MaskData.R.Left + dw + w, MaskData.R.Top + h, MaskData.R.Left + dw + 2 * w, MaskData.R.Top + 2 * h), Bmp, EmptyCI, MaskData);
            inc(x, w);
          end;
          if x < R.Right - w then begin
            CopyMasterRect(Rect(x, y, R.Right - w, R.Bottom - h), Rect(MaskData.R.Left + dw + w, MaskData.R.Top + h, MaskData.R.Left + dw + 2 * w, MaskData.R.Top + 2 * h), Bmp, EmptyCI, MaskData);
          end
        end;
      end;
    end;
  end;
end;

procedure DrawSkinRect(const Bmp : TBitmap; const R : TRect; const Filling : boolean; const ci : TCacheInfo; const MaskData : TsMaskData; const State : integer; const UpdateCorners : boolean; SkinManager : TObject = nil);
var
  x, y : integer;
  w, h : integer;
  dw, dh : integer;
  wl, wt, wr, wb : integer;
  BmpSrc : TBitmap;
  NewState : integer;
begin
  if (State = 0) and (MaskData.DrawMode and BDM_ACTIVEONLY = BDM_ACTIVEONLY) then Exit;
  if (WidthOf(R) < 2) or (HeightOf(R) < 2) or (MaskData.Manager = nil) then Exit;
  wl := MaskData.WL; wt := MaskData.WT; wr := MaskData.WR; wb := MaskData.WB;
  if wl + wr > WidthOf(R) then begin
    x := ((wl + wr) - WidthOf(R)) div 2;
    dec(wl, x); dec(wr, x); if WidthOf(R) mod 2 > 0 then dec(wr);
    if wl < 0 then wl := 0;
    if wr < 0 then wr := 0;
  end;
  if wt + wb > HeightOf(R) then begin
    x := ((wt + wb) - HeightOf(R)) div 2;
    dec(wt, x); dec(wb, x); if HeightOf(R) mod 2 > 0 then dec(wb);
    if wt < 0 then wt := 0;
    if wb < 0 then wb := 0;
  end;
  if State >= MaskData.ImageCount then NewState := MaskData.ImageCount - 1 else NewState := State;
  if MaskData.ImageCount = 0 then begin
    Exit;
  end;
  dw := WidthOfImage(MaskData);                  // Width of mask
  dh := HeightOfImage(MaskData);                 // Height of mask

  w := dw - wl - wr; if w < 0 then w := 0;                            // Width of middle piece
  h := dh - wt - wb; if h < 0 then h := 0;                            // Height of middle piece
  dw := dw * NewState;                                                   // Offset of mask

  if MaskData.Bmp <> nil then BmpSrc := MaskData.Bmp else BmpSrc := TsSkinManager(MaskData.Manager).MasterBitmap;
  if MaskData.MaskType = 0 then begin // Copy without mask
    // left - top
    CopyTransRect(Bmp, BmpSrc, R.Left, R.Top,
                  Rect(MaskData.R.Left + dw,
                       MaskData.R.Top,
                       MaskData.R.Left + dw + wl - 1,
                       MaskData.R.Top + wt - 1),
                  clFuchsia, CI, UpdateCorners);
    y := R.Top + wt;    
    // left - middle
    if MaskData.DrawMode and BDM_STRETCH = 0 then begin
      if h > 0 then while y < R.Bottom - h - wb do begin
        BitBlt(Bmp.Canvas.Handle, R.Left, y, wl, h, BmpSrc.Canvas.Handle, MaskData.R.Left + dw, MaskData.R.Top + wt, SRCCOPY);
        inc(y, h);
      end;
      if y < R.Bottom - wb then BitBlt(Bmp.Canvas.Handle, R.Left, y, wl, R.Bottom - wb - y, BmpSrc.Canvas.Handle, MaskData.R.Left + dw, MaskData.R.Top + wt, SRCCOPY);
    end
    else begin
      SetStretchBltMode(Bmp.Canvas.Handle, HALFTONE);
      StretchBlt(Bmp.Canvas.Handle, R.Left, y, wl, R.Bottom - wb - y, BmpSrc.Canvas.Handle, MaskData.R.Left + dw, MaskData.R.Top + wt, wl, h, SRCCOPY);
    end;
    // top - middle
    x := R.Left + wl;
    if MaskData.DrawMode and BDM_STRETCH = 0 then begin
      if w > 0 then while x < R.Right - w - wr do begin
        BitBlt(Bmp.Canvas.Handle, x, R.Top, w, wt, BmpSrc.Canvas.Handle, MaskData.R.Left + dw + wl, MaskData.R.Top, SRCCOPY);
        inc(x, w);
      end;
      if x < R.Right - wr then BitBlt(Bmp.Canvas.Handle, x, R.Top, R.Right - wr - x, wt, BmpSrc.Canvas.Handle, MaskData.R.Left + dw + wl, MaskData.R.Top, SRCCOPY);
    end
    else StretchBlt(Bmp.Canvas.Handle, x, R.Top, R.Right - wr - x, wt, BmpSrc.Canvas.Handle, MaskData.R.Left + dw + wl, MaskData.R.Top, w, wt, SRCCOPY);
    // left - bottom
    CopyTransRect(Bmp, BmpSrc, R.Left, R.Bottom - wb, Rect(MaskData.R.Left + dw, MaskData.R.Bottom - wb, MaskData.R.Left + dw + wl - 1, MaskData.R.Bottom - 1), clFuchsia, CI, UpdateCorners);
    // bottom - middle
    x := R.Left + wl;
    if MaskData.DrawMode and BDM_STRETCH = 0 then begin
      if w > 0 then while x < R.Right - w - wr do begin
        BitBlt(Bmp.Canvas.Handle, x, R.Bottom - wb, w, wb, BmpSrc.Canvas.Handle, MaskData.R.Left + dw + wl, MaskData.R.Bottom - wb, SRCCOPY);
        inc(x, w);
      end;
      if x < R.Right - wr then BitBlt(Bmp.Canvas.Handle, x, R.Bottom - wb, R.Right - wr - x, wb, BmpSrc.Canvas.Handle, MaskData.R.Left + dw + wl, MaskData.R.Bottom - wb, SRCCOPY);
    end
    else StretchBlt(Bmp.Canvas.Handle, x, R.Bottom - wb, R.Right - wr - x, wb, BmpSrc.Canvas.Handle, MaskData.R.Left + dw + wl, MaskData.R.Bottom - wb, w, wb, SRCCOPY);
    // right - bottom
    CopyTransRect(Bmp, BmpSrc, R.Right - wr, R.Bottom - wb, Rect(MaskData.R.Left + dw + w + wl, MaskData.R.Bottom - wb, MaskData.R.Left + dw + w + wl + wr - 1, MaskData.R.Bottom - 1), clFuchsia, CI, UpdateCorners);
    // right - top
    CopyTransRect(Bmp, BmpSrc, R.Right - wr, R.Top, Rect(MaskData.R.Left + dw + w + wl, MaskData.R.Top, MaskData.R.Left + dw + w + wl + wr - 1, MaskData.R.Top + wt - 1), clFuchsia, CI, UpdateCorners);
    y := R.Top + wt;
    // right - middle
    if MaskData.DrawMode and BDM_STRETCH = 0 then begin
      if h > 0 then while y < R.Bottom - h - wb do begin
        BitBlt(Bmp.Canvas.Handle, R.Right - wr, y, wr, h, BmpSrc.Canvas.Handle, MaskData.R.Left + dw + wl + w, MaskData.R.Top + wt, SRCCOPY);
        inc(y, h);
      end;
      if y < R.Bottom - wb then BitBlt(Bmp.Canvas.Handle, R.Right - wr, y, wr, R.Bottom - wb - y, BmpSrc.Canvas.Handle, MaskData.R.Left + dw + w + wl, MaskData.R.Top + wt, SRCCOPY);
    end
    else StretchBlt(Bmp.Canvas.Handle, R.Right - wr, y, wr, R.Bottom - wb - y, BmpSrc.Canvas.Handle, MaskData.R.Left + dw + wl + w, MaskData.R.Top + wt, wr, h, SRCCOPY);
    // Fill
    if Filling and (MaskData.DrawMode and BDM_FILL = BDM_FILL) then begin
      if MaskData.DrawMode and BDM_STRETCH = 0 then begin
        y := R.Top + wt;
        if h > 0 then while y < R.Bottom - h - wb do begin
          x := R.Left + wl;
          if w > 0 then while x < R.Right - w - wr do begin
            BitBlt(Bmp.Canvas.Handle, x, y, w, h, BmpSrc.Canvas.Handle, MaskData.R.Left + dw + wl, MaskData.R.Top + wt, SRCCOPY);
            inc(x, w);
          end;
          if x < R.Right - wr then BitBlt(Bmp.Canvas.Handle, x, y, R.Right - wr - x,  R.Bottom - wb - y, BmpSrc.Canvas.Handle, MaskData.R.Left + dw + wl, MaskData.R.Top + wt, SRCCOPY);
          inc(y, h);
        end;
        x := R.Left + wl;
        if y < R.Bottom - wb then begin
          if w > 0 then while x < R.Right - w - wr do begin
            BitBlt(Bmp.Canvas.Handle, x, y, w, R.Bottom - wb - y, BmpSrc.Canvas.Handle, MaskData.R.Left + dw + wl, MaskData.R.Top + wt, SRCCOPY);
            inc(x, w);
          end;
          if x < R.Right - wr then BitBlt(Bmp.Canvas.Handle, x, y, R.Right - wr - x, R.Bottom - wb - y, BmpSrc.Canvas.Handle, MaskData.R.Left + dw + wl, MaskData.R.Top + wt, SRCCOPY);
        end;
      end
      else begin
        y := R.Top + wt;
        x := R.Left + wl;
        StretchBlt(Bmp.Canvas.Handle, x, y, R.Right - wr - x, R.Bottom - wb - y, BmpSrc.Canvas.Handle, MaskData.R.Left + dw + wl, MaskData.R.Top + wt, w, h, SRCCOPY);
      end;
    end;
  end
  else begin
    // left - top
    CopyByMask(Rect(R.Left, R.Top, R.Left + wl, R.Top + wt), Rect(MaskData.R.Left + dw, MaskData.R.Top, MaskData.R.Left + dw + wl, MaskData.R.Top + wt), Bmp, BmpSrc, CI, UpdateCorners, MaskData);
    y := R.Top + wt;
    // left - middle
    while y < R.Bottom - h - wb do begin
      CopyByMask(Rect(R.Left, y, R.Left + wl, y + h), Rect(MaskData.R.Left + dw, MaskData.R.Top + wt, MaskData.R.Left + dw + wl, MaskData.R.Top + h + wt), Bmp, BmpSrc, EmptyCI, False, MaskData);
      inc(y, h);
    end;
    if y < R.Bottom - wb then begin
      CopyByMask(Rect(R.Left, y, R.Left + wl, R.Bottom - wb), Rect(MaskData.R.Left + dw, MaskData.R.Top + wt, MaskData.R.Left + dw + wl, MaskData.R.Top + wt + h), Bmp, BmpSrc, EmptyCI, False, MaskData);
    end;
    // top - middle
    x := R.Left + wl;
    while x < R.Right - w - wr do begin
      CopyByMask(Rect(x, R.Top, x + w, R.Top + wt), Rect(MaskData.R.Left + dw + wl, MaskData.R.Top, MaskData.R.Left + dw + w + wl, MaskData.R.Top + wt), Bmp, BmpSrc, EmptyCI, False, MaskData);
      inc(x, w);
    end;
    if x < R.Right - wr then begin
      CopyByMask(Rect(x, R.Top, R.Right - wr, R.Top + wt), Rect(MaskData.R.Left + dw + wl, MaskData.R.Top, MaskData.R.Left + dw + w + wl, MaskData.R.Top + wt), Bmp, BmpSrc, EmptyCI, False, MaskData);
    end;
    // left - bottom
    CopyByMask(Rect(R.Left, R.Bottom - wb, R.Left + wl, R.Bottom), Rect(MaskData.R.Left + dw, MaskData.R.Top + dh - wb, MaskData.R.Left + dw + wl, MaskData.R.Top + dh), Bmp, BmpSrc, CI, UpdateCorners, MaskData);
    // bottom - middle
    x := R.Left + wl;
    while x < R.Right - w - wr do begin
      CopyByMask(Rect(x, R.Bottom - wb, x + w, R.Bottom), Rect(MaskData.R.Left + dw + wl, MaskData.R.Top + dh - wb, MaskData.R.Left + dw + w + wl, MaskData.R.Top + dh), Bmp, bmpSrc, EmptyCI, False, MaskData);
      inc(x, w);
    end;
    if x < R.Right - wr then begin
      CopyByMask(Rect(x, R.Bottom - wb, R.Right - wr, R.Bottom), Rect(MaskData.R.Left + dw + wl, MaskData.R.Top + dh - wb, MaskData.R.Left + dw + w + wl, MaskData.R.Top + dh), Bmp, BmpSrc, EmptyCI, False, MaskData);
    end;
    // right - bottom
    CopyByMask(Rect(R.Right - wr, R.Bottom - wb, R.Right, R.Bottom), Rect(MaskData.R.Left + dw + w + wl, MaskData.R.Top + dh - wb, MaskData.R.Left + dw + w + wl + wr, MaskData.R.Top + dh), Bmp, BmpSrc, CI, UpdateCorners, MaskData);
    // right - top
    CopyByMask(Rect(R.Right - wr, R.Top, R.Right, R.Top + wt), Rect(MaskData.R.Left + dw + w + wl, MaskData.R.Top, MaskData.R.Left + dw + w + wl + wr, MaskData.R.Top + wt), Bmp, BmpSrc, CI, UpdateCorners, MaskData);
    // right - middle
    y := R.Top + wt;
    while y < R.Bottom - h - wb do begin
      CopyByMask(Rect(R.Right - wr, y, R.Right, y + h), Rect(MaskData.R.Left + dw + w + wl, MaskData.R.Top + wt, MaskData.R.Left + dw + w + wl + wr, MaskData.R.Top + h + wt), Bmp, BmpSrc, EmptyCI, False, MaskData);
      inc(y, h);
    end;
    if y < R.Bottom - wb then begin
      CopyByMask(Rect(R.Right - wr, y, R.Right, R.Bottom - wb), Rect(MaskData.R.Left + dw + w + wl, MaskData.R.Top + wt, MaskData.R.Left + dw + w + wl + wr, MaskData.R.Top + h + wt), Bmp, BmpSrc, EmptyCI, False, MaskData);
    end;
    // Fill
    if Filling and (MaskData.DrawMode and BDM_FILL = BDM_FILL) then begin
      y := R.Top + wt;
      while y < R.Bottom - h - wb do begin
        x := R.Left + wl;
        while x < R.Right - w - wr do begin
          CopyByMask(Rect(x, y, x + w, y + h), Rect(MaskData.R.Left + dw + wl, MaskData.R.Top + wt, MaskData.R.Left + dw + w + wl, MaskData.R.Top + h + wt), Bmp, BmpSrc, EmptyCI, False, MaskData);
          inc(x, w);
        end;
        if x < R.Right - wr then begin
          CopyByMask(Rect(x, y, R.Right - wr, y + h), Rect(MaskData.R.Left + dw + wl, MaskData.R.Top + wt, MaskData.R.Left + dw + w + wl, MaskData.R.Top + h + wt), Bmp, BmpSrc, EmptyCI, False, MaskData);
        end;
        inc(y, h);
      end;
      x := R.Left + wl;
      if y < R.Bottom - wb then begin
        while x < R.Right - w - wr do begin
          CopyByMask(Rect(x, y, x + w, R.Bottom - wb), Rect(MaskData.R.Left + dw + wl, MaskData.R.Top + wt, MaskData.R.Left + dw + w + wl, MaskData.R.Top + h + wt), Bmp, BmpSrc, EmptyCI, False, MaskData);
          inc(x, w);
        end;
        if x < R.Right - wr then begin
          CopyByMask(Rect(x, y, R.Right - wr, R.Bottom - wb), Rect(MaskData.R.Left + dw + wl, MaskData.R.Top + wt, MaskData.R.Left + dw + w + wl, MaskData.R.Top + h + wt), Bmp, BmpSrc, EmptyCI, False, MaskData);
        end
      end;
    end;
  end;
end;

procedure CopyMasterRect(R1, R2 : TRect; Bmp : TBitmap; CI : TCacheInfo; MaskData : TsMaskData);
var
  S1, S2, M : PRGBArray;
  X, Y, h, w: Integer;
  col : TsColor;
begin
  if MaskData.Manager = nil then Exit;
  with TsSkinManager(MaskData.Manager) do begin
    h := Min(HeightOf(R1), HeightOf(R2));
    h := Min(h, Bmp.Height - R1.Top);
    h := Min(h, TsSkinManager(MaskData.Manager).MasterBitmap.Height - R2.Top) - 1;
    if h < 0 then Exit;
    w := Min(WidthOf(R1), WidthOf(R2));
    w := Min(w, Bmp.Width - R1.Left);
    w := Min(w, MasterBitmap.Width - R2.Left) - 1;
    if w < 0 then Exit;
    if R1.Left < R2.Left then begin
      if (R1.Left < 0) then begin
        inc(R2.Left, - R1.Left);
        dec(h, - R1.Left);
        R1.Left := 0;
      end;
    end
    else
      if (R2.Left < 0) then begin
        inc(R1.Left, - R2.Left);
        dec(h, - R2.Left);
        R2.Left := 0;
      end;
    if R1.Top < R2.Top then begin
      if (R1.Top < 0) then begin
        inc(R2.Top, - R1.Top);
        dec(h, - R1.Top);
        R1.Top := 0;
      end;
    end
    else
      if (R2.Top < 0) then begin
        inc(R1.Top, - R2.Top);
        dec(h, - R2.Top);
        R2.Top := 0;
      end;
    col.C := CI.FillColor;
    if not CI.Ready then begin
      for Y := 0 to h do begin
        S1 := Bmp.ScanLine[R1.Top + Y];
        S2 := MasterBitmap.ScanLine[R2.Top + Y];
        M  := MasterBitmap.ScanLine[R2.Top + HeightOf(MaskData.R) div 2 + Y];
        for X := 0 to w do begin
          if (S2[R2.Left + X].R <> sFuchsia.R) or (S2[R2.Left + X].G <> sFuchsia.G) or (S2[R2.Left + X].B <> sFuchsia.B) then begin
            S1[R1.Left + X].R := IntToByte(((S1[R1.Left + X].R - S2[R2.Left + X].R) * M[R2.Left + X].R + S2[R2.Left + X].R shl 8) shr 8);
            S1[R1.Left + X].G := IntToByte(((S1[R1.Left + X].G - S2[R2.Left + X].G) * M[R2.Left + X].G + S2[R2.Left + X].G shl 8) shr 8);
            S1[R1.Left + X].B := IntToByte(((S1[R1.Left + X].B - S2[R2.Left + X].B) * M[R2.Left + X].B + S2[R2.Left + X].B shl 8) shr 8);
          end
          else if CI.FillColor <> clFuchsia then begin
            S1[R1.Left + X].R := col.R;
            S1[R1.Left + X].G := col.G;
            S1[R1.Left + X].B := col.B;
          end;
        end;
      end;
    end
    else begin
      if ci.Bmp.PixelFormat = pf24bit then begin
        if Fast24Src.Attach(ci.Bmp) then for Y := 0 to h do begin
          S1 := Bmp.ScanLine[R1.Top + Y];
          S2 := MasterBitmap.ScanLine[R2.Top + Y];
          M  := MasterBitmap.ScanLine[R2.Top + HeightOf(MaskData.R) div 2 + Y];
          for X := 0 to w do begin
            if (S2[R2.Left + X].R <> sFuchsia.R) or (S2[R2.Left + X].G <> sFuchsia.G) or (S2[R2.Left + X].B <> sFuchsia.B) then begin
              S1[R1.Left + X].R := IntToByte(((S1[R1.Left + X].R - S2[R2.Left + X].R) * M[R2.Left + X].R + S2[R2.Left + X].R shl 8) shr 8);
              S1[R1.Left + X].G := IntToByte(((S1[R1.Left + X].G - S2[R2.Left + X].G) * M[R2.Left + X].G + S2[R2.Left + X].G shl 8) shr 8);
              S1[R1.Left + X].B := IntToByte(((S1[R1.Left + X].B - S2[R2.Left + X].B) * M[R2.Left + X].B + S2[R2.Left + X].B shl 8) shr 8);
            end
            else if not CI.Ready then begin
              S1[R1.Left + X].R := col.R;
              S1[R1.Left + X].G := col.G;
              S1[R1.Left + X].B := col.B;
            end
            else begin
              if (CI.Bmp.Height <= R1.Top + ci.Y + Y) then Continue;
              if (CI.Bmp.Width <= R1.Left + ci.X + X) then Break;
              if R1.Top + ci.Y + Y < 0 then Continue;
              if R1.Left + ci.X + X < 0 then Continue;
              col := Fast24Src.Pixels[R1.Left + ci.X + X, R1.Top + ci.Y + Y];
              S1[R1.Left + X].R := col.R;
              S1[R1.Left + X].G := col.G;
              S1[R1.Left + X].B := col.B;
            end;
          end;
        end;
      end
      else if ci.Bmp.PixelFormat = pf32bit then begin
        if Fast32Src.Attach(ci.Bmp) then for Y := 0 to h do begin
          S1 := Bmp.ScanLine[R1.Top + Y];
          S2 := MasterBitmap.ScanLine[R2.Top + Y];
          M  := MasterBitmap.ScanLine[R2.Top + HeightOf(MaskData.R) div 2 + Y];
          for X := 0 to w do begin
            if (S2[R2.Left + X].R <> sFuchsia.R) or (S2[R2.Left + X].G <> sFuchsia.G) or (S2[R2.Left + X].B <> sFuchsia.B) then begin
              S1[R1.Left + X].R := IntToByte(((S1[R1.Left + X].R - S2[R2.Left + X].R) * M[R2.Left + X].R + S2[R2.Left + X].R shl 8) shr 8);
              S1[R1.Left + X].G := IntToByte(((S1[R1.Left + X].G - S2[R2.Left + X].G) * M[R2.Left + X].G + S2[R2.Left + X].G shl 8) shr 8);
              S1[R1.Left + X].B := IntToByte(((S1[R1.Left + X].B - S2[R2.Left + X].B) * M[R2.Left + X].B + S2[R2.Left + X].B shl 8) shr 8);
            end
            else if CI.FillColor <> clFuchsia then begin
              S1[R1.Left + X].R := col.R;
              S1[R1.Left + X].G := col.G;
              S1[R1.Left + X].B := col.B;
            end
            else begin
              if (CI.Bmp.Height <= R1.Top + ci.Y + Y) then Continue;
              if (CI.Bmp.Width <= R1.Left + ci.X + X) then Break;
              if R1.Top + ci.Y + Y < 0 then Continue;
              if R1.Left + ci.X + X < 0 then Continue;
              col := Fast32Src.Pixels[R1.Left + ci.X + X, R1.Top + ci.Y + Y];
              S1[R1.Left + X].R := col.R;
              S1[R1.Left + X].G := col.G;
              S1[R1.Left + X].B := col.B;
            end;
          end;
        end;
      end
    end;
  end;
end;

procedure CopyMasterRectA(R1, R2 : TRect; Bmp : TBitmap; CI : TCacheInfo; MaskData : TsMaskData);
var
  S1, S2, M : PRGBArray;
  X, Y, h, w: Integer;
  col : TsColor;
begin
  if MaskData.Manager = nil then Exit;
  with TsSkinManager(MaskData.Manager) do begin
    h := Min(HeightOf(R1), HeightOf(R2));
    h := Min(h, Bmp.Height - R1.Top);
    h := Min(h, MasterBitmap.Height - R2.Top) - 1;
    if h < 0 then Exit;
    w := Min(WidthOf(R1), WidthOf(R2));
    w := Min(w, Bmp.Width - R1.Left);
    w := Min(w, MasterBitmap.Width - R2.Left) - 1;
    if w < 0 then Exit;
    if R1.Left < R2.Left then begin
      if (R1.Left < 0) then begin
        inc(R2.Left, - R1.Left);
        dec(h, - R1.Left);
        R1.Left := 0;
      end;
    end
    else
      if (R2.Left < 0) then begin
        inc(R1.Left, - R2.Left);
        dec(h, - R2.Left);
        R2.Left := 0;
      end;
    if R1.Top < R2.Top then begin
      if (R1.Top < 0) then begin
        inc(R2.Top, - R1.Top);
        dec(h, - R1.Top);
        R1.Top := 0;
      end;
    end
    else
      if (R2.Top < 0) then begin
        inc(R1.Top, - R2.Top);
        dec(h, - R2.Top);
        R2.Top := 0;
      end;
    col.C := CI.FillColor;
    if not CI.Ready then for Y := 0 to h do begin
      S1 := Bmp.ScanLine[R1.Top + Y];
      S2 := MasterBitmap.ScanLine[R2.Top + Y];
      M  := MasterBitmap.ScanLine[R2.Top + HeightOf(MaskData.R) div 2 + Y];
      for X := 0 to w do if (S2[R2.Left + X].R <> sFuchsia.R) or (S2[R2.Left + X].G <> sFuchsia.G) or (S2[R2.Left + X].B <> sFuchsia.B) then begin // v4
        S1[R1.Left + X].R := IntToByte(((S1[R1.Left + X].R - S2[R2.Left + X].R) * M[R2.Left + X].R + S2[R2.Left + X].R shl 8) shr 8);
        S1[R1.Left + X].G := IntToByte(((S1[R1.Left + X].G - S2[R2.Left + X].G) * M[R2.Left + X].G + S2[R2.Left + X].G shl 8) shr 8);
        S1[R1.Left + X].B := IntToByte(((S1[R1.Left + X].B - S2[R2.Left + X].B) * M[R2.Left + X].B + S2[R2.Left + X].B shl 8) shr 8);
      end;
    end
    else for Y := 0 to h do begin
      S1 := Bmp.ScanLine[R1.Top + Y];
      S2 := MasterBitmap.ScanLine[R2.Top + Y];
      M  := MasterBitmap.ScanLine[R2.Top + HeightOf(MaskData.R) div 2 + Y];
      for X := 0 to w do if (S2[R2.Left + X].R <> sFuchsia.R) or (S2[R2.Left + X].G <> sFuchsia.G) or (S2[R2.Left + X].B <> sFuchsia.B) then begin
        S1[R1.Left + X].R := IntToByte(((S1[R1.Left + X].R - S2[R2.Left + X].R) * M[R2.Left + X].R + S2[R2.Left + X].R shl 8) shr 8);
        S1[R1.Left + X].G := IntToByte(((S1[R1.Left + X].G - S2[R2.Left + X].G) * M[R2.Left + X].G + S2[R2.Left + X].G shl 8) shr 8);
        S1[R1.Left + X].B := IntToByte(((S1[R1.Left + X].B - S2[R2.Left + X].B) * M[R2.Left + X].B + S2[R2.Left + X].B shl 8) shr 8);
      end;
    end;
  end;
end;

procedure DrawSkinGlyph(Bmp : TBitmap; P : TPoint; State, AddedTransparency : integer; MaskData : TsMaskData; const CI : TCacheInfo);
var
  w, h, cy, cx : integer;
  dw : integer;
begin
  w := WidthOfImage(MaskData);
  h := HeightOfImage(MaskData);

  if State > MaskData.ImageCount - 1 then State := MaskData.ImageCount - 1;
  dw := State * w;
  if p.y < 0 then cy := - p.y else cy := 0;
  if p.x < 0 then cx := - p.x else cx := 0;

  if MaskData.Bmp = nil then with TsSkinManager(MaskData.Manager) do begin
    if MaskData.Manager = nil then Exit;
    if MaskData.ImageCount = 0 then Exit;

    if (MaskData.MaskType > 0) then begin
      BlendGlyphByMask(Rect(p.x + cx, p.y + cy, p.x + w - 1 + cx, p.y + h + cy - 1),
               Rect(dw + cx + MaskData.R.Left, cy + MaskData.R.Top, dw + w - 1 + cx + MaskData.R.Left, h - 1 + cy + MaskData.R.Top),
               Bmp, MasterBitmap, clFuchsia, AddedTransparency, MaskData);
    end
    else begin
      if AddedTransparency <> 1 then begin
        BlendTransRectangle(Bmp, p.x + cx, p.y + cy, MasterBitmap,
          Rect(dw + cx + MaskData.R.Left, cy + MaskData.R.Top, dw + cx + MaskData.R.Left + w - 1, cy + MaskData.R.Top + h - 1),
          0.5, sFuchsia);
      end
      else
        CopyTransRect(Bmp, MasterBitmap, p.x + cx, p.y + cy,
          Rect(dw + cx + MaskData.R.Left, cy + MaskData.R.Top, dw + cx + MaskData.R.Left + w - 1, cy + MaskData.R.Top + h - 1),
          clFuchsia, CI, True);
    end;
  end
  else begin
    if (MaskData.R.Right > MaskData.Bmp.Width) or (MaskData.R.Bottom > MaskData.Bmp.Height) then Exit;
//    if p.y + h + cy - 1 > Bmp.Height then Exit;

    BlendGlyphByMask(Rect(p.x + cx, p.y + cy, p.x + w - 1 + cx, p.y + h + cy - 1),
               Rect(dw + cx + MaskData.R.Left, cy + MaskData.R.Top, dw + w - 1 + cx + MaskData.R.Left, h - 1 + cy + MaskData.R.Top),
               Bmp, MaskData.Bmp, clFuchsia, AddedTransparency, MaskData);

  end;
end;

procedure BlendGlyphByMask(R1, R2 : TRect; Bmp1, Bmp2 : TBitmap; TransColor : TColor; AddedTransparency : integer; MaskData : TsMaskData);
var
  S1, S2, M : PRGBArray;
  X, Y, h, w, hdiv2, dx1, dy1, dx2, dy2: Integer;
  c : TsColor;
  RegRect : TRect;
begin
  hdiv2 := HeightOf(MaskData.R) div (MaskData.MaskType + 1);
  h := Min(HeightOf(R1), HeightOf(R2));
  h := min(h, Bmp1.Height - R1.Top - 1);
  h := min(h, Bmp2.Height - R2.Top - hdiv2 - 1);

  if MaskData.ImageCount < 1 then h := min(h, hdiv2 - R2.Top - 1);
  w := Min(WidthOf(R1), WidthOf(R2));
  w := min(w, Bmp1.Width - R1.Left - 1);

  if MaskData.ImageCount < 1 then w := min(w, Bmp2.Width - R2.Left - 1);
  RegRect := Rect(-1, 0, 0, 0);

  if MaskData.MaskType = 0 then CopyTransRectA(Bmp1, Bmp2, R1.Left, R1.Top, R2, clFuchsia, EmptyCI) else begin
    c.A := 0;
    if R2.Top + h > Bmp2.Height then Exit;
    dy1 := R1.Top;
    dy2 := R2.Top;
    for Y := 0 to h do begin
      S1 := Bmp1.ScanLine[dy1];
      S2 := Bmp2.ScanLine[dy2];
      M  := Bmp2.ScanLine[dy2 + hdiv2];
      dx1 := R1.Left;
      dx2 := R2.Left;
      for X := 0 to w do begin
        c.sBGR := S2[R2.Left + X];
        // If not transparent..
        if c.C <> sFuchsia.C then begin
          if AddedTransparency <> 1 then begin
            S1[dx1].R := IntToByte(((S1[dx1].R - S2[dx2].R) * min(M[dx2].R + 100, 255) + S2[dx2].R shl 8) shr 8);
            S1[dx1].G := IntToByte(((S1[dx1].G - S2[dx2].G) * min(M[dx2].G + 100, 255) + S2[dx2].G shl 8) shr 8);
            S1[dx1].B := IntToByte(((S1[dx1].B - S2[dx2].B) * min(M[dx2].B + 100, 255) + S2[dx2].B shl 8) shr 8);
          end
          else begin
            S1[dx1].R := IntToByte(((S1[dx1].R - S2[dx2].R) * M[dx2].R + S2[dx2].R shl 8) shr 8);
            S1[dx1].G := IntToByte(((S1[dx1].G - S2[dx2].G) * M[dx2].G + S2[dx2].G shl 8) shr 8);
            S1[dx1].B := IntToByte(((S1[dx1].B - S2[dx2].B) * M[dx2].B + S2[dx2].B shl 8) shr 8);
          end;
        end;
        inc(dx1);
        inc(dx2);
      end;
      inc(dy1);
      inc(dy2);
    end;
  end;    
end;

procedure UpdateCorners(SkinData : TsCommonData; State : integer);
begin
  UpdateCorners(SkinData, State, [scLeftTop, scLeftBottom, scRightTop, scRightBottom]);
end;

procedure UpdateCorners(SkinData : TsCommonData; State : integer; Corners : TsCorners);
var
  w, Width, Height : integer;
  dw, dh : integer;
  MaskData : TsMaskData;
  BGInfo : TacBGInfo;
  ParentRGB : TsRGB;
  ParentColor : TsColor;
  SrcBmp : TBitmap;
  wl, wt, wr, wb : integer;
  procedure CopyTransCorner(SrcBmp: Graphics.TBitMap; X, Y : integer; SrcRect: TRect);
  var
    Dst, Src : PRGBArray;
    sX, sY, SrcX, DstX, DstY : Integer;
    NewColor : TsRGB;
    h, w : integer;
    col : TsColor;
  begin

    if SrcRect.Top < 0 then SrcRect.Top := 0;
    if SrcRect.Bottom > SrcBmp.Height - 1 then SrcRect.Bottom := SrcBmp.Height - 1;
    if SrcRect.Left < 0 then SrcRect.Left := 0;
    if SrcRect.Right > SrcBmp.Width - 1 then SrcRect.Right := SrcBmp.Width - 1;

    h := HeightOf(SrcRect);
    w := WidthOf(SrcRect);
    if (BGInfo.BgType = btCache) and (BGInfo.Bmp <> nil) then begin
      if BGInfo.Bmp.PixelFormat = pf24bit then begin
        if Fast24Src.Attach(BGInfo.Bmp) then for sY := 0 to h do begin
          DstY := sY + Y;
          if (DstY <= SkinData.FCacheBmp.Height - 1) and (DstY >= 0) then begin
            Dst := SkinData.FCacheBmp.ScanLine[DstY];
            Src := SrcBmp.ScanLine[sY + SrcRect.Top];
            for sX := 0 to w do begin
              DstX := sX + X;
              if (DstX <= SkinData.FCacheBmp.Width - 1) and (DstX >= 0) then begin
                SrcX := sX + SrcRect.Left;
                if (Src[SrcX].B = sFuchsia.B) and (Src[SrcX].G = sFuchsia.G) and (Src[SrcX].R = sFuchsia.R) {if transparent pixel}
                  then begin
//                    if BGInfo.bgType = btFill then Dst[DstX] := ParentRGB else if BGInfo.BgType = btCache then begin
                      if (BGInfo.Offset.Y + DstY >= BGInfo.Bmp.Height) or (BGInfo.Offset.X + DstX >= BGInfo.Bmp.Width) or (BGInfo.Offset.Y + DstY < 0) or (BGInfo.Offset.X + DstX < 0) then continue;
                      try
                        col := Fast24Src.Pixels[BGInfo.Offset.X + DstX, BGInfo.Offset.Y + DstY];
                      finally
                        NewColor.R := col.R;
                        NewColor.G := col.G;
                        NewColor.B := col.B;
                        Dst[DstX] := NewColor
                      end
//                    end
                end;
              end;
            end
          end;
        end
      end
      else begin
        if (BGInfo.Bmp.PixelFormat = pf32bit) and Fast32Src.Attach(BGInfo.Bmp) then for sY := 0 to h do begin
          DstY := sY + Y;
          if (DstY <= SkinData.FCacheBmp.Height - 1) and (DstY >= 0) then begin
            Dst := SkinData.FCacheBmp.ScanLine[DstY];
            Src := SrcBmp.ScanLine[sY + SrcRect.Top];
            for sX := 0 to w do begin
              DstX := sX + X;
              if (DstX <= SkinData.FCacheBmp.Width - 1) and (DstX >= 0) then begin
                SrcX := sX + SrcRect.Left;
                if (Src[SrcX].B = sFuchsia.B) and (Src[SrcX].G = sFuchsia.G) and (Src[SrcX].R = sFuchsia.R) {if transparent pixel} then begin
                  if BGInfo.Color <> clFuchsia then Dst[DstX] := ParentRGB else if BGInfo.BgType = btCache then begin
                    if (BGInfo.Offset.Y + DstY >= BGInfo.Bmp.Height) or (BGInfo.Offset.X + DstX >= BGInfo.Bmp.Width) or (BGInfo.Offset.Y + DstY < 0) or (BGInfo.Offset.X + DstX < 0) then continue;
                    try
                      col := Fast32Src.Pixels[BGInfo.Offset.X + DstX, BGInfo.Offset.Y + DstY];
                    finally
                      NewColor.R := col.R;
                      NewColor.G := col.G;
                      NewColor.B := col.B;
                      Dst[DstX] := NewColor
                    end
                  end
                end;
              end;
            end
          end
        end
      end;
    end
    else if ParentColor.C <> clFuchsia then for sY := 0 to h do begin
      DstY := sY + Y;
      if (DstY <= SkinData.FCacheBmp.Height - 1) and (DstY >= 0) then begin
        Dst := SkinData.FCacheBmp.ScanLine[DstY];
        Src := SrcBmp.ScanLine[sY + SrcRect.Top];
        for sX := 0 to w do begin
          DstX := sX + X;
          if (DstX <= SkinData.FCacheBmp.Width - 1) and (DstX >= 0) then begin
            SrcX := sX + SrcRect.Left;
            if (Src[SrcX].B = sFuchsia.B) and (Src[SrcX].G = sFuchsia.G) and (Src[SrcX].R = sFuchsia.R) then Dst[DstX] := ParentRGB
          end;
        end
      end;
    end;
  end;
  procedure CopyMasterCorner(R1, R2 : TRect; Bmp : TBitmap);
  var
    S1, S2 : PRGBArray;
    X, Y, h, w: Integer;
    col : TsColor;
  begin
    h := Min(HeightOf(R1), HeightOf(R2));
    h := Min(h, SkinData.FCacheBmp.Height - R1.Top);
    h := Min(h, Bmp.Height - R2.Top) - 1;
    if h < 0 then Exit;
    w := Min(WidthOf(R1), WidthOf(R2));
    w := Min(w, SkinData.FCacheBmp.Width - R1.Left);
    w := Min(w, Bmp.Width - R2.Left) - 1;
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
    col.C := BGInfo.Color;
    if BGInfo.BgType <> btCache then begin
      for Y := 0 to h do begin
        S1 := SkinData.FCacheBmp.ScanLine[R1.Top + Y];
        S2 := Bmp.ScanLine[R2.Top + Y];
        for X := 0 to w do begin
          if (S2[R2.Left + X].R = sFuchsia.R) and (S2[R2.Left + X].G = sFuchsia.G) and (S2[R2.Left + X].B = sFuchsia.B) then begin
            S1[R1.Left + X].R := col.R;
            S1[R1.Left + X].G := col.G;
            S1[R1.Left + X].B := col.B;
          end;
        end;
      end;
    end
    else begin
      if BGInfo.Bmp.PixelFormat = pf24bit then begin
        if Fast24Src.Attach(BGInfo.Bmp) then for Y := 0 to h do begin
          S1 := SkinData.FCacheBmp.ScanLine[R1.Top + Y];
          S2 := Bmp.ScanLine[R2.Top + Y];
          if BGInfo.BgType <> btCache then begin
            for X := 0 to w do begin
              if (S2[R2.Left + X].R = sFuchsia.R) and (S2[R2.Left + X].G = sFuchsia.G) and (S2[R2.Left + X].B = sFuchsia.B) then begin
                S1[R1.Left + X].R := col.R;
                S1[R1.Left + X].G := col.G;
                S1[R1.Left + X].B := col.B;
              end
            end;
          end
          else begin
            for X := 0 to w do begin
              if (S2[R2.Left + X].R = sFuchsia.R) and (S2[R2.Left + X].G = sFuchsia.G) and (S2[R2.Left + X].B = sFuchsia.B) then begin
                if (BGInfo.Bmp.Height <= R1.Top + BGInfo.Offset.Y + Y) then Continue;
                if (BGInfo.Bmp.Width <= R1.Left + BGInfo.Offset.X + X) then Break;
                if R1.Top + BGInfo.Offset.Y + Y < 0 then Break;
                if R1.Left + BGInfo.Offset.X + X < 0 then Continue;
                col := Fast24Src.Pixels[R1.Left + BGInfo.Offset.X + X, R1.Top + BGInfo.Offset.Y + Y];
                S1[R1.Left + X].R := col.R;
                S1[R1.Left + X].G := col.G;
                S1[R1.Left + X].B := col.B;
              end
            end;
          end;
        end;
      end
      else begin
        // !!!
      end;
    end;
  end;
begin
  if not Assigned(Skindata.SkinManager) then Exit;
  if (SkinData.BorderIndex < 0) or not Skindata.SkinManager.IsValidImgIndex(SkinData.BorderIndex) or
       not Assigned(SkinData.FCacheBmp) or
       (SkinData.FOwnerControl = nil) or not (SkinData.FOwnerControl is TControl) or
         (SkinData.FOwnerControl.Width < 2) or (SkinData.FOwnerControl.Height < 2) then Exit;

  BGInfo.PleaseDraw := False;
  BGInfo.DrawDC := 0;
  GetBGInfo(@BGInfo, SkinData.FOwnerControl.Parent);

  MaskData := SkinData.SkinManager.ma[SkinData.BorderIndex];
  Width := SkinData.FOwnerControl.Width;
  Height := SkinData.FOwnerControl.Height;
  wl := MaskData.WL; wt := MaskData.WT; wr := MaskData.WR; wb := MaskData.WB;

  if (MaskData.ImageCount = 0) and (MaskData.Bmp <> nil) then begin // if external
    MaskData.MaskType := 1;
    MaskData.ImageCount := 3;
    MaskData.R := Rect(0, 0, MaskData.Bmp.Width, MaskData.Bmp.Height);
  end;

  if BGInfo.BgType <> btCache then begin
    ParentColor.C := ColorToRGB(BGInfo.Color);
    ParentRGB.R := ParentColor.R;
    ParentRGB.G := ParentColor.G;
    ParentRGB.B := ParentColor.B;
  end
  else begin
    inc(BGInfo.Offset.X, SkinData.FOwnerControl.Left);
    inc(BGInfo.Offset.Y, SkinData.FOwnerControl.Top);
  end;

  if State >= MaskData.ImageCount then State := MaskData.ImageCount - 1;
  dw := State * WidthOf(MaskData.R) div (MaskData.ImageCount);        // Width of mask
  dh := HeightOf(MaskData.R) div (1 + MaskData.MaskType);             // Height of mask

  w := WidthOfImage(MaskData) - wl - wr;             // Width of piece of mask

  if MaskData.Bmp <> nil then SrcBmp := MaskData.Bmp else SrcBmp := SkinData.SkinManager.MasterBitmap;
  if MaskData.MaskType = 0 then begin // Copy without mask
    if (scLeftTop in Corners) then CopyTransCorner(SrcBmp, 0, 0, Rect(MaskData.R.Left + dw, MaskData.R.Top, MaskData.R.Left + dw + wl - 1, MaskData.R.Top + wt - 1));
    if (scLeftBottom in Corners) then CopyTransCorner(SrcBmp, 0, Height - wb, Rect(MaskData.R.Left, MaskData.R.Bottom - wb, MaskData.R.Left + wl - 1, MaskData.R.Bottom - 1));
    if (scRightBottom in Corners) then CopyTransCorner(SrcBmp, Width - wr, Height - wb, Rect(MaskData.R.Left + dw + wl + w, MaskData.R.Bottom - wb, MaskData.R.Left + dw + wl + w + wr - 1, MaskData.R.Bottom - 1));
    if (scRightTop in Corners) then CopyTransCorner(SrcBmp, Width - wr, 0, Rect(MaskData.R.Left + dw + wl + w, MaskData.R.Top, MaskData.R.Left + dw + wl + w + wr - 1, MaskData.R.Top + wb - 1));
  end
  else begin
    if (scLeftTop in Corners) then CopyMasterCorner(Rect(0, 0, wl + 1, wt + 1), Rect(MaskData.R.Left + dw, MaskData.R.Top, MaskData.R.Left + dw + wl, MaskData.R.Top + wt), SrcBmp);
    if (scLeftBottom in Corners) then CopyMasterCorner(Rect(0, Height - wb, wl, Height), Rect(MaskData.R.Left + dw, MaskData.R.Top + dh - wb, MaskData.R.Left + dw + wl, MaskData.R.Top + dh), SrcBmp);
    if (scRightBottom in Corners) then CopyMasterCorner(Rect(Width - wr, Height - wb, Width, Height), Rect(MaskData.R.Left + dw + wl + w, MaskData.R.Top + dh - wb, MaskData.R.Left + dw + wl + w + wr, MaskData.R.Top + dh), SrcBmp);
    if (scRightTop in Corners) then CopyMasterCorner(Rect(Width - wr, 0, Width, wt), Rect(MaskData.R.Left + dw + wl + w, MaskData.R.Top, MaskData.R.Left + dw + wl + w + wr, MaskData.R.Top + wt), SrcBmp);
  end;
end;
(*
procedure PaintTransPixels(const DstBmp : TBitmap; R: TRect; const MaskData : TsMaskData; CI : TCacheInfo; State : integer; SkinManager : TObject);
var
  dw, dh, w : integer;
  NewColor : TsRGB;
  Col : TsColor;
  SrcBmp : TBitmap;
  procedure CopyTransPixels(SrcBmp : TBitmap; X, Y : integer; SrcR : TRect);
  var
    Dst, Src : PRGBArray;
    sX, sY, SrcX, DstX, DstY : Integer;
    h, w, mOffset : integer;
  begin
    h := HeightOf(SrcR);
    w := WidthOf(SrcR);
    if CI.Ready then begin
      if (CI.Bmp.PixelFormat = pf24bit) and Fast24Src.Attach(CI.Bmp) then begin
        if MaskData.MaskType = 0 then begin
          for sY := 0 to h do begin
            DstY := sY + Y;
            if (DstY <= DstBmp.Height - 1) and (DstY >= 0) then begin
              Dst := DstBmp.ScanLine[DstY];
              Src := SrcBmp.ScanLine[sY + SrcR.Top];
              for sX := 0 to w do begin
                DstX := sX + X;
                if (DstX <= DstBmp.Width - 1) and (DstX >= 0) then begin
                  SrcX := sX + SrcR.Left;
                  if (Src[SrcX].B = sFuchsia.B) and (Src[SrcX].G = sFuchsia.G) and (Src[SrcX].R = sFuchsia.R) {if transparent pixel} then begin
                    if (CI.Y + DstY >= CI.Bmp.Height) or (CI.X + DstX >= CI.Bmp.Width) or (CI.Y + DstY < 0) or (CI.X + DstX < 0) then Continue;
                    try
                      col := Fast24Src.Pixels[CI.X + DstX, CI.Y + DstY];
                    finally
                      Dst[DstX] := col.sBGR;
                    end
                  end;
                end;
              end
            end;
          end
        end
        else begin
          mOffset := HeightOfImage(MaskData);
          for sY := 0 to h do begin
            DstY := sY + Y;
            if (DstY <= DstBmp.Height - 1) and (DstY >= 0) then begin
              Dst := DstBmp.ScanLine[DstY];
              Src := SrcBmp.ScanLine[sY + SrcR.Top + mOffset];
              for sX := 0 to w do begin
                DstX := sX + X;
                if (DstX <= DstBmp.Width - 1) and (DstX >= 0) then begin
                  SrcX := sX + SrcR.Left;
                  if (Src[SrcX].B = 255) and (Src[SrcX].G = 255) and (Src[SrcX].R = 255) {if transparent pixel} then begin
                    if (CI.Y + DstY >= CI.Bmp.Height) or (CI.X + DstX >= CI.Bmp.Width) or (CI.Y + DstY < 0) or (CI.X + DstX < 0) then Continue;
                    try
                      col := Fast24Src.Pixels[CI.X + DstX, CI.Y + DstY];
                    finally
                      Dst[DstX] := col.sBGR;
                    end
                  end;
                end;
              end
            end;
          end
        end;
      end;
    end
    else if CI.FillColor <> clFuchsia then for sY := 0 to h do begin
      DstY := sY + Y;
      if (DstY <= DstBmp.Height - 1) and (DstY >= 0) then begin
        Dst := DstBmp.ScanLine[DstY];
        Src := SrcBmp.ScanLine[sY + SrcR.Top];
        for sX := 0 to w do begin
          DstX := sX + X;
          if (DstX <= DstBmp.Width - 1) and (DstX >= 0) then begin
            SrcX := sX + SrcR.Left;
            if (Src[SrcX].B = sFuchsia.B) and (Src[SrcX].G = sFuchsia.G) and (Src[SrcX].R = sFuchsia.R) then Dst[DstX] := NewColor
          end;
        end
      end;
    end;
  end;
begin
  if MaskData.Bmp = nil then SrcBmp := TsSkinManager(SkinManager).MasterBitmap else SrcBmp := MaskData.Bmp;
  if State >= MaskData.ImageCount then State := MaskData.ImageCount - 1;
  dw := State * WidthOfImage(MaskData);      // Width of mask
  dh := HeightOfImage(MaskData);             // Height of mask

  w := WidthOfImage(MaskData) - MaskData.wl - MaskData.wr;             // Width of piece of mask
  Col.C := CI.FillColor;
  NewColor.R := Col.R;
  NewColor.G := Col.G;
  NewColor.B := Col.B;

  with MaskData do begin
    CopyTransPixels(SrcBmp, 0, 0, Rect(MaskData.R.Left + dw, MaskData.R.Top, MaskData.R.Left + dw + wl - 1, MaskData.R.Top + wt - 1));
//    CopyTransPixels(SrcBmp, 0, R.Bottom - wb, Rect(MaskData.R.Left, MaskData.R.Bottom - wb, MaskData.R.Left + wl - 1, MaskData.R.Bottom - 1));
//    CopyTransPixels(SrcBmp, R.Right - wr, R.Bottom - wb, Rect(MaskData.R.Left + dw + wl + w, MaskData.R.Bottom - wb, MaskData.R.Left + dw + wl + w + wr - 1, MaskData.R.Bottom - 1));
//    CopyTransPixels(SrcBmp, R.Right - wr, 0, Rect(MaskData.R.Left + dw + wl + w, MaskData.R.Top, MaskData.R.Left + dw + wl + w + wr - 1, MaskData.R.Top + wb - 1));
  end;
end;*)

{$ENDIF}

{ TacFast24 }

function TacFast24.Attach(Bmp: TBitmap) : boolean;
begin
  if (Bmp <> nil) and (Bmp.Height > 1) and (Bmp.Width > 1) then begin
    FBitmap := Bmp;
    FWidth := FBitmap.Width;
    FHeight := FBitmap.Height;
    Assert(FBitmap.PixelFormat = pf24bit, 'Bitmap format is not supported');
    FStart := Integer(FBitmap.Scanline[0]);
    FDelta := Integer(FBitmap.Scanline[1]) - FStart;
    Result := True;
  end
  else begin
    FWidth := 0;
    Result := False;
  end;
end;

procedure TacFast24.SetPixel(X, Y: Integer; const Value: TsColor);
asm
  push eax
  push ebx
  push ecx
  push edx
  imul ecx,[eax].FDelta
  add ecx,[eax].FStart
  lea edx,[edx+edx*2]
  mov eax,[ebp+8]  //Value
  bswap eax
  shr eax, 8
  mov [ecx+edx],ax
  shr eax, 16
  mov [ecx+edx+2],al
  pop edx
  pop ecx
  pop ebx
  pop eax
end;

function TacFast24.GetPixel(X, Y: Integer): TsColor;
asm
  push ebx
  push ecx
  push edx
  imul ecx,[eax].FDelta
  add ecx,[eax].FStart
  add ecx,edx
  movzx eax,WORD PTR [ecx+2*edx]
  bswap eax
  shr eax,8
  movzx ecx, BYTE PTR [ecx+2*edx+2]
  or eax,ecx
  pop edx
  pop ecx
  pop ebx
end;

{ TacFastSum24 }

function TacFastSum24.Attach(BmpSrc, BmpDst: TBitmap) : boolean;
begin
  if (BmpSrc.Height > 1) and (BmpDst.Height > 1) and (BmpSrc.Width > 1) and (BmpDst.Width > 1) then begin
    FBmpSrc := BmpSrc;
    FBmpDst := BmpDst;

    FWidthDst := min(FBmpDst.Width, FBmpSrc.Width);
    FHeightDst := min(FBmpDst.Height, FBmpSrc.Height);
    FStartSrc := longint(FBmpSrc.Scanline[0]);
    FStartDst := longint(FBmpDst.Scanline[0]);
    FDeltaDst := longint(FBmpDst.Scanline[1]) - FStartDst;
    FDeltaSrc := longint(FBmpSrc.Scanline[1]) - FStartSrc;
    Result := True;
  end
  else Result := False;
end;

procedure TacFastSum24.BlendBitmaps;
var
  x, y : integer;
  SrcOffset : integer;
  a, b : byte;
asm
  mov cl, [eax].Alpha;
  mov a, cl                 // Alpha value

  mov ch, 255
  sub ch, cl
  mov b, ch                 // Beta value

  mov ecx, 0
  mov ecx, [eax].FHeightDst
  dec ecx                   // Max Y receiving
@DecY:
  mov Y, ecx

  mov ebx, [eax].FWidthDst
  jz @Next2
  imul ebx, 3
@Next2:
  dec ebx
  mov X, ebx                // Max X receiving

  mov edx, Y
  imul edx, [eax].FDeltaSrc
  add edx, [eax].FStartSrc  // Src offset

  mov SrcOffset, edx

  mov edx, Y
  imul edx, [eax].FDeltaDst
  add edx, [eax].FStartDst  // Dst offset

  mov ecx, X
@DecX:

  mov ebx, 0
  mov bl, BYTE PTR [edx + ecx] // EBX - destination byte
  push edx
  push eax

  mov edx, SrcOffset
  // (C2 * alpha + beta * C1 + 127 ) / 255
  mov eax, 0
  mov al, a
  imul ebx, eax

  push ebx

  mov ebx, 0
  mov bl, b
  mov eax, 0
  mov al, BYTE PTR [edx + ecx]  // EAX - source byte

  imul eax, ebx

  pop ebx

  add eax, ebx
  add eax, 127

  mov bl, ah                    // shl al, 8 Result byte - in BL

  pop eax
  pop edx
  mov BYTE PTR [edx + ecx], bl  // Moving the result byte

  dec ecx
  cmp ecx, 0
  jns @DecX

  mov ecx, Y
  dec ecx
  cmp ecx, 0

  jns @DecY
end;

procedure TacFastSum24.BlendBitmapsRect;
var
  x, y, minX, maxX, SrcXOffset, SrcYOffset : integer;
  SrcOffset : integer;
  a, b : byte;
asm
  mov cl, [eax].Alpha;
  mov a, cl                 // Alpha value
  mov ch, 255
  sub ch, cl
  mov b, ch                 // Beta value


  mov ebx, [eax].DstX1


  mov edx, ebx
  sub edx, [eax].SrcX1
  imul edx, 3

  mov SrcXOffset, edx

  imul ebx, 3
  mov MinX, ebx             // Max X in MaxX variable

  mov ebx, [eax].DstX2
  imul ebx, 3
  dec ebx
  mov MaxX, ebx             // Max X in MaxX variable

  xor ecx, ecx
  mov ecx, [eax].DstY2

  mov ebx, ecx
  sub ebx, [eax].SrcY2
//  imul ebx, [eax].FDeltaSrc
  mov SrcYOffset, ebx

  dec ecx
@DecY:
  mov Y, ecx
  mov ebx, MaxX
  mov X, ebx                // Min DstX2 receiving

  mov edx, Y
  sub edx, SrcYOffset
  imul edx, [eax].FDeltaSrc
  add edx, [eax].FStartSrc  // Src offset
  mov SrcOffset, edx

  mov edx, Y
  imul edx, [eax].FDeltaDst
  add edx, [eax].FStartDst  // Dst offset

  mov ecx, X
@DecX:
  xor ebx, ebx
  mov bl, BYTE PTR [edx + ecx] // EBX - destination byte
  push edx
  push eax

  mov edx, SrcOffset
  // (C2 * alpha + beta * C1 + 127 ) / 255
  xor eax, eax
  mov al, a
  imul ebx, eax

  push ebx

  xor ebx, ebx
  mov bl, b
  xor eax, eax
  sub edx, SrcXOffset
  mov al, BYTE PTR [edx + ecx]  // EAX - source byte

  imul eax, ebx

  pop ebx

  add eax, ebx
  add eax, 127

  mov bl, ah                    // shl al, 8 and result byte - in BL

  pop eax
  pop edx
  mov BYTE PTR [edx + ecx], bl  // Moving the result byte

  dec ecx
  cmp ecx, MinX
  jns @DecX

  mov ecx, Y
  dec ecx
  cmp ecx, [eax].DstY1

  jns @DecY
end;

{ TacFast32 }

function TacFast32.Attach(Bmp: TBitmap): boolean;
begin
  if (Bmp.Height > 1) and (Bmp.Width > 1) then begin
    FBitmap := Bmp;
    FWidth := FBitmap.Width;
    FHeight := FBitmap.Height;
    Assert(FBitmap.PixelFormat = pf32bit, 'Bitmap format is not supported');
    FStart := longint(FBitmap.Scanline[0]);
    FDelta := longint(FBitmap.Scanline[1]) - FStart;
    Result := True;
  end
  else begin
    FWidth := 0;
    Result := False;
  end;
end;

function TacFast32.GetPixel(X, Y: Integer): TsColor;
asm
  push ebx
  push ecx
  push edx
  imul ecx, [eax].FDelta
  add ecx, [eax].FStart
  mov eax, [ecx + 4 * edx]
  bswap eax

  shr eax, 8
  xor ebx, ebx
  mov bl, al
  shl ebx, 24
//  shr eax, 8 v5.33
  add eax, ebx
  pop edx
  pop ecx
  pop ebx
end;

procedure TacFast32.SetPixel(X, Y: Integer; const Value: TsColor);
asm
  push eax
  push ebx
  push ecx
  push edx
  imul ecx, [eax].FDelta
  add ecx, [eax].FStart
  mov eax, Value
  bswap eax

  xor ebx, ebx
  mov bl, al
  shl ebx, 24
  shr eax, 8
  add eax, ebx
  mov [ecx + 4 * edx], eax
  pop edx
  pop ecx
  pop ebx
  pop eax
end;

initialization
  Fast24Src := TacFast24.Create;
  Fast24Dst := TacFast24.Create; 
  FastSum24 := TacFastSum24.Create;
  Fast32Src := TacFast32.Create;

finalization
  FreeAndNil(Fast24Src);
  FreeAndNil(Fast24Dst);
  FreeAndNil(FastSum24);
  FreeAndNil(Fast32Src);
end.




