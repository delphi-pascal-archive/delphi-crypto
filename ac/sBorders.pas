unit sBorders;
{$I sDefs.inc}

interface

uses Windows, Graphics, Classes, SysUtils, sMaskData, sCommonData, sGraphUtils;

function PaintBorderFast(DC : hdc; aRect : TRect; MinBorderWidth : integer; SkinData : TsCommonData; State : integer) : TRect; // New rect for filling is returned
//procedure PaintSkinBorderFast(Bmp : TBitmap; R : TRect; Filling : boolean; ci : TCacheInfo; MaskData : TsMaskData; State : integer; UpdateCorners : boolean; SkinManager : TObject = nil);

procedure PaintRgnBorder(Bmp : TBitmap; Region : hrgn; Filling : boolean; MaskData : TsMaskData; State : integer);
procedure CopyMask(DstX1, DstY1, DstX2, DstY2, SrcX1, SrcY1, SrcX2, SrcY2 : integer; Bmp : TBitmap; Region : hrgn; MaskData : TsMaskData; FillRgn : boolean = False);

implementation

uses acntUtils, Math, sSkinManager, sConst, sMessages, sVclUtils, sAlphaGraph, sStylesimply;

function PaintBorderFast(DC : hdc; aRect : TRect; MinBorderWidth : integer; SkinData : TsCommonData; State : integer) : TRect;
var
  Color : TColor;
  l, t, r, b : integer;
  sm : TsSkinManager;
  ParentBG : TacBGInfo;
begin
  Color := GetBGColor(SkinData, State); // Control BG color

  ParentBG.BgType := btUnknown;
  if (SkinData.FOwnerControl <> nil) and (SkinData.FOwnerControl.Parent <> nil) then begin // Get info about parent
    ParentBG.PleaseDraw := False;
    GetBGInfo(@ParentBG, SkinData.FOwnerControl.Parent);
    inc(ParentBG.Offset.x, SkinData.FOwnerControl.Left); // v6.04
    inc(ParentBG.Offset.y, SkinData.FOwnerControl.Top);
  end; // Init parent BG data
  if SkinData.BorderIndex > -1 then begin // if borders exists
    sm := SkinData.SkinManager;
    InitCacheBmp(SkinData);

    if SkinData.SkinManager.ma[SkinData.BorderIndex].MaskType = 1 then begin
      if ParentBG.BgType = btCache then begin
        BitBltBorder(SkinData.FCacheBmp.Canvas.Handle, aRect.Left, aRect.Top, WidthOf(aRect), HeightOf(aRect),
                     ParentBG.Bmp.Canvas.Handle, ParentBG.Offset.X + SkinData.FOwnerControl.Left + aRect.Left, ParentBG.Offset.Y + SkinData.FOwnerControl.Top + aRect.Top, SRCCOPY);
      end
      else begin
        FillDCBorder(SkinData.FCacheBmp.Canvas.Handle, aRect, sm.ma[SkinData.BorderIndex].WL, sm.ma[SkinData.BorderIndex].WT, sm.ma[SkinData.BorderIndex].WR, sm.ma[SkinData.BorderIndex].WB, Color);
      end;
    end;
    DrawSkinRect(SkinData.FCacheBmp, aRect, False, BGInfoToCI(@ParentBG), SkinData.SkinManager.ma[SkinData.BorderIndex], State, True);

    Result := Rect(aRect.Left + sm.ma[SkinData.BorderIndex].WL,
                   aRect.Top + sm.ma[SkinData.BorderIndex].WT,
                   aRect.Right - sm.ma[SkinData.BorderIndex].WR,
                   aRect.Bottom - sm.ma[SkinData.BorderIndex].WB);

    if MinBorderWidth <> 0 then begin

      l := max(min(MinBorderWidth, WidthOf(aRect) - sm.ma[SkinData.BorderIndex].WR) - sm.ma[SkinData.BorderIndex].WL, 0);
      t := max(min(MinBorderWidth, HeightOf(aRect) - sm.ma[SkinData.BorderIndex].WB) - sm.ma[SkinData.BorderIndex].WT, 0);
      r := max(min(MinBorderWidth, WidthOf(aRect) - sm.ma[SkinData.BorderIndex].WL) - sm.ma[SkinData.BorderIndex].WR, 0);
      b := max(min(MinBorderWidth, HeightOf(aRect) - sm.ma[SkinData.BorderIndex].WT) - sm.ma[SkinData.BorderIndex].WB, 0);

      FillDCBorder(SkinData.FCacheBmp.Canvas.Handle, Result, l, t, r, b, Color); // Fill skipped parts
      Result := Rect(aRect.Left + MinBorderWidth,
                     aRect.Top + MinBorderWidth,
                     aRect.Right - MinBorderWidth,
                     aRect.Bottom - MinBorderWidth);
      if Result.Right < Result.Left then Result.Right := Result.Left;
      if Result.Bottom < Result.Top then Result.Bottom := Result.Top;
    end;
    if SkinData.FCacheBmp.Canvas.Handle <> DC then begin
      if MinBorderWidth <> 0 then begin
        BitBltBorder(DC, aRect.Left, aRect.Top, WidthOf(aRect), HeightOf(aRect), SkinData.FCacheBmp.Canvas.Handle, aRect.Left, aRect.Top, MinBorderWidth);
      end;
    end;
  end
  else if MinBorderWidth <> 0 then begin
    FillDCBorder(DC, aRect, MinBorderWidth, MinBorderWidth, MinBorderWidth, MinBorderWidth, Color);
    InflateRect(aRect, MinBorderWidth, MinBorderWidth);
  end;
end;

procedure PaintSkinBorderFast(Bmp : TBitmap; R : TRect; Filling : boolean; ci : TCacheInfo; MaskData : TsMaskData; State : integer; UpdateCorners : boolean; SkinManager : TObject = nil);
var
  x, y : integer;
  w, h : integer;
  dw, dh : integer;
  wl, wt, wr, wb : integer;
  BmpSrc : TBitmap;
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
  if State >= MaskData.ImageCount then State := MaskData.ImageCount - 1;
  dw := WidthOf(MaskData.R) div MaskData.ImageCount;                  // Width of mask
  dh := HeightOf(MaskData.R) div (1 + MaskData.MaskType);             // Height of mask

  w := dw - wl - wr; if w < 0 then w := 0;                            // Width of middle piece
  h := dh - wt - wb; if h < 0 then h := 0;                            // Height of middle piece
  dw := dw * State;                                                   // Offset of mask

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
      StretchBlt(Bmp.Canvas.Handle,
               R.Left, y, wl, R.Bottom - wb - y,
               BmpSrc.Canvas.Handle,
               MaskData.R.Left + dw, MaskData.R.Top + wt, wl, h,
               SRCCOPY);
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
    else StretchBlt(Bmp.Canvas.Handle,
               x, R.Top, R.Right - wr - x, wt,
               BmpSrc.Canvas.Handle,
               MaskData.R.Left + dw + wl, MaskData.R.Top, w, wt,
               SRCCOPY);
    // left - bottom
    CopyTransRect(Bmp, BmpSrc, R.Left, R.Bottom - wb,
                  Rect(MaskData.R.Left + dw,
                       MaskData.R.Bottom - wb,
                       MaskData.R.Left + dw + wl - 1,
                       MaskData.R.Bottom - 1),
                  clFuchsia, CI, UpdateCorners);
    // bottom - middle
    x := R.Left + wl;
    if MaskData.DrawMode and BDM_STRETCH = 0 then begin
      if w > 0 then while x < R.Right - w - wr do begin
        BitBlt(Bmp.Canvas.Handle, x, R.Bottom - wb, w, wb,
             BmpSrc.Canvas.Handle, MaskData.R.Left + dw + wl, MaskData.R.Bottom - wb, SRCCOPY);
        inc(x, w);
      end;
      if x < R.Right - wr then BitBlt(Bmp.Canvas.Handle, x, R.Bottom - wb, R.Right - wr - x, wb,
                                        BmpSrc.Canvas.Handle, MaskData.R.Left + dw + wl, MaskData.R.Bottom - wb, SRCCOPY);
    end
    else StretchBlt(Bmp.Canvas.Handle,
               x, R.Bottom - wb, R.Right - wr - x, wb,
               BmpSrc.Canvas.Handle,
               MaskData.R.Left + dw + wl, MaskData.R.Bottom - wb, w, wb,
               SRCCOPY);
    // right - bottom
    CopyTransRect(Bmp, BmpSrc, R.Right - wr, R.Bottom - wb,
                  Rect(MaskData.R.Left + dw + w + wl,
                       MaskData.R.Bottom - wb,
                       MaskData.R.Left + dw + w + wl + wr - 1,
                       MaskData.R.Bottom - 1),
                  clFuchsia, CI, UpdateCorners);
    // right - top
    CopyTransRect(Bmp, BmpSrc, R.Right - wr, R.Top,
                  Rect(MaskData.R.Left + dw + w + wl,
                       MaskData.R.Top,
                       MaskData.R.Left + dw + w + wl + wr - 1,
                       MaskData.R.Top + wt - 1),
                  clFuchsia, CI, UpdateCorners);
    y := R.Top + wt;
    // right - middle
    if MaskData.DrawMode and BDM_STRETCH = 0 then begin
      if h > 0 then while y < R.Bottom - h - wb do begin
        BitBlt(Bmp.Canvas.Handle, R.Right - wr, y, wr, h,
          BmpSrc.Canvas.Handle, MaskData.R.Left + dw + wl + w, MaskData.R.Top + wt, SRCCOPY);
        inc(y, h);
      end;
      if y < R.Bottom - wb then BitBlt(Bmp.Canvas.Handle, R.Right - wr, y, wr, R.Bottom - wb - y,
                                         BmpSrc.Canvas.Handle, MaskData.R.Left + dw + w + wl, MaskData.R.Top + wt, SRCCOPY);
    end
    else StretchBlt(Bmp.Canvas.Handle,
               R.Right - wr, y, wr, R.Bottom - wb - y,
               BmpSrc.Canvas.Handle,
               MaskData.R.Left + dw + wl + w, MaskData.R.Top + wt, wr, h,
               SRCCOPY);
    // Fill
    if Filling and (MaskData.DrawMode and BDM_FILL = BDM_FILL) then begin
      if MaskData.DrawMode and BDM_STRETCH = 0 then begin
        y := R.Top + wt;
        if h > 0 then while y < R.Bottom - h - wb do begin
          x := R.Left + wl;
          if w > 0 then while x < R.Right - w - wr do begin
            BitBlt(Bmp.Canvas.Handle, x, y, w, h, BmpSrc.Canvas.Handle,
                             MaskData.R.Left + dw + wl, MaskData.R.Top + wt, SRCCOPY);
            inc(x, w);
          end;
          if x < R.Right - wr then BitBlt(Bmp.Canvas.Handle, x, y, R.Right - wr - x,  R.Bottom - wb - y,
                             BmpSrc.Canvas.Handle, MaskData.R.Left + dw + wl, MaskData.R.Top + wt, SRCCOPY);
          inc(y, h);
        end;
        x := R.Left + wl;
        if y < R.Bottom - wb then begin
          if w > 0 then while x < R.Right - w - wr do begin
            BitBlt(Bmp.Canvas.Handle, x, y, w, R.Bottom - wb - y,
                 BmpSrc.Canvas.Handle, MaskData.R.Left + dw + wl, MaskData.R.Top + wt, SRCCOPY);
            inc(x, w);
          end;
          if x < R.Right - wr then BitBlt(Bmp.Canvas.Handle, x, y, R.Right - wr - x, R.Bottom - wb - y,
                                            BmpSrc.Canvas.Handle, MaskData.R.Left + dw + wl, MaskData.R.Top + wt, SRCCOPY);
        end;
      end
      else begin
        y := R.Top + wt;
        x := R.Left + wl;
        StretchBlt(Bmp.Canvas.Handle,
                 x, y, R.Right - wr - x, R.Bottom - wb - y,
                 BmpSrc.Canvas.Handle,
                 MaskData.R.Left + dw + wl, MaskData.R.Top + wt, w, h,
                 SRCCOPY);
      end;
    end;
  end;
end;

procedure CopyMask(DstX1, DstY1, DstX2, DstY2, SrcX1, SrcY1, SrcX2, SrcY2 : integer; Bmp : TBitmap; Region : hrgn; MaskData : TsMaskData; FillRgn : boolean = False);
var
  S1, S2, M : PRGBArray;
  X, Y, h, w, MaskH, MaskHd2, MaskBmpHeight: Integer;
  c : TsColor;
  RegRect : TRect;
  fr : hrgn;
  BmpSrc : TBitmap;
  src, dst, msk : TsRGB;
begin
  if (DstX1 < 0) or (DstX2 < 0) or (DstY1 < 0) or (DstY2 < 0) or (Bmp.Height < 2) or (Bmp.Width < 2) then Exit;
  if MaskData.Manager = nil then Exit;
  if MaskData.Bmp = nil then BmpSrc := TsSkinManager(MaskData.Manager).MasterBitmap else BmpSrc := MaskData.Bmp;
  if MaskData.MaskType = 1 then begin
    MaskH := HeightOf(MaskData.R);
    MaskBmpHeight := BmpSrc.Height - 1;
    MaskHd2 := MaskH div 2;

    h := Min(DstY2 - DstY1, Bmp.Height - DstY1);
    h := Min(h, MaskBmpHeight - SrcY1);
    h := Min(SrcY2 - SrcY1, h);
    w := Min(DstX2 - DstX1, SrcX2 - SrcX1);
    RegRect := Rect(-1, 0, 0, 0);
    c.A := 0;
    try
      if FillRgn then for Y := 0 to h do begin
        S1 := Bmp.ScanLine[DstY1 + Y];
        S2 := BmpSrc.ScanLine[SrcY1 + Y];
        M  := BmpSrc.ScanLine[SrcY1 + MaskHd2 + Y];
        for X := 0 to w do begin
          src := S2[SrcX1 + X];
          c.R := src.R;
          c.G := src.G;
          c.B := src.B;
          if c.C = sFuchsia.C then begin // If transparent pixel..
            if (Region <> 0) then begin
              if RegRect.Left <> -1 then inc(RegRect.Right) else begin
                RegRect.Left := DstX1 + X;
                RegRect.Right := RegRect.Left + 1;
                RegRect.Top := DstY1 + Y;
                RegRect.Bottom := RegRect.Top + 1;
              end;
            end;
          end
          else begin
            if (RegRect.Left <> -1) and (Region <> 0) then begin
              fr := CreateRectRgn(RegRect.Left, RegRect.Top, RegRect.Right, RegRect.Bottom);
              CombineRgn(Region, Region, fr, RGN_XOR);
              DeleteObject(fr);
              RegRect.Left := -1;
            end;
            dst := S1[DstX1 + X];
            msk := M[SrcX1 + X];
            dst.R := IntToByte(((dst.R - src.R) * msk.R + src.R shl 8) shr 8);
            dst.G := IntToByte(((dst.G - src.G) * msk.G + src.G shl 8) shr 8);
            dst.B := IntToByte(((dst.B - src.B) * msk.B + src.B shl 8) shr 8);
            S1[DstX1 + X] := dst;
          end;
        end;
        if (RegRect.Left <> -1) and (Region <> 0) then begin
          fr := CreateRectRgn(RegRect.Left, RegRect.Top, RegRect.Right, RegRect.Bottom);
          CombineRgn(Region, Region, fr, RGN_DIFF);
          DeleteObject(fr);
          RegRect.Left := -1;
        end;
      end
      else for Y := 0 to h do begin
        S1 := Bmp.ScanLine[DstY1 + Y];
        S2 := BmpSrc.ScanLine[SrcY1 + Y];
        M  := BmpSrc.ScanLine[SrcY1 + MaskHd2 + Y];
        for X := 0 to w do begin
          dst := S1[DstX1 + X];
          src := S2[SrcX1 + X];
          msk := M[SrcX1 + X];
          dst.R := IntToByte(((dst.R - src.R) * msk.R + src.R shl 8) shr 8);
          dst.G := IntToByte(((dst.G - src.G) * msk.G + src.G shl 8) shr 8);
          dst.B := IntToByte(((dst.B - src.B) * msk.B + src.B shl 8) shr 8);
          S1[DstX1 + X] := dst;
        end;
      end;
    except
    end;
  end
  else begin
    h := Min(DstY2 - DstY1, Bmp.Height - DstY1);
    h := Min(SrcY2 - SrcY1, h);
    w := Min(DstX2 - DstX1, SrcX2 - SrcX1);
    RegRect := Rect(-1, 0, 0, 0);
    c.A := 0;
    try
      if FillRgn then for Y := 0 to h do begin
        S1 := Bmp.ScanLine[DstY1 + Y];
        S2 := BmpSrc.ScanLine[SrcY1 + Y];
        for X := 0 to w do begin
          src := S2[SrcX1 + X];
          c.R := src.R;
          c.G := src.G;
          c.B := src.B;
          if c.C = sFuchsia.C then begin // If transparent pixel..
            if (Region <> 0) then begin
              if RegRect.Left <> -1 then inc(RegRect.Right) else begin
                RegRect.Left := DstX1 + X;
                RegRect.Right := RegRect.Left + 1;
                RegRect.Top := DstY1 + Y;
                RegRect.Bottom := RegRect.Top + 1;
              end;
            end;
          end
          else begin
            if (RegRect.Left <> -1) and (Region <> 0) then begin
              fr := CreateRectRgn(RegRect.Left, RegRect.Top, RegRect.Right, RegRect.Bottom);
              CombineRgn(Region, Region, fr, RGN_XOR);
              DeleteObject(fr);
              RegRect.Left := -1;
            end;
            S1[DstX1 + X] := src;
          end;
        end;
        if (RegRect.Left <> -1) and (Region <> 0) then begin
          fr := CreateRectRgn(RegRect.Left, RegRect.Top, RegRect.Right, RegRect.Bottom);
          CombineRgn(Region, Region, fr, RGN_DIFF);
          DeleteObject(fr);
          RegRect.Left := -1;
        end;
      end
      else for Y := 0 to h do begin
        S1 := Bmp.ScanLine[DstY1 + Y];
        S2 := BmpSrc.ScanLine[SrcY1 + Y];
        for X := 0 to w do S1[DstX1 + X] := S2[SrcX1 + X];
      end;
    except
    end;
  end;
end;

procedure PaintRgnBorder(Bmp : TBitmap; Region : hrgn; Filling : boolean; MaskData : TsMaskData; State : integer);
var
  x, y : integer;
  w, h : integer;
  dw, dh, pw : integer;
  wl, wt, wr, wb : integer;
  BmpSrc : TBitmap;
begin
  if (Bmp.Width < 2) or (Bmp.Height < 2) or (MaskData.Manager = nil) then Exit;
//  if (Region = 0) then Region := CreateRectRgn(0, 0, Bmp.Width, Bmp.Height);
  wl := MaskData.WL; wt := MaskData.WT; wr := MaskData.WR; wb := MaskData.WB;
  if wl + wr > Bmp.Width then begin
    x := ((wl + wr) - Bmp.Width) div 2;
    dec(wl, x); dec(wr, x); if Bmp.Width mod 2 > 0 then dec(wr);
    if wl < 0 then wl := 0;
    if wr < 0 then wr := 0;
  end;
  if wt + wb > Bmp.Height then begin
    x := ((wt + wb) - Bmp.Height) div 2;
    dec(wt, x); dec(wb, x); if Bmp.Height mod 2 > 0 then dec(wb);
    if wt < 0 then wt := 0;
    if wb < 0 then wb := 0;
  end;
  if MaskData.ImageCount < 1 then begin
    MaskData.ImageCount := 1;
    MaskData.R := Rect(0, 0, MaskData.Bmp.Width, MaskData.Bmp.Height);
  end;
  if State >= MaskData.ImageCount then State := MaskData.ImageCount - 1;
  pw := WidthOf(MaskData.R) div MaskData.ImageCount;                  // Width of mask
  dh := HeightOf(MaskData.R) div (1 + MaskData.MaskType);             // Height of mask

  w := pw - MaskData.WL - MaskData.WR; if w < 0 then w := 0;          // Width of middle piece
  h := dh - MaskData.WT - MaskData.WB; if h < 0 then h := 0;          // Height of middle piece
  dw := MaskData.R.Left + pw * State;                                 // Offset of mask
  if MaskData.Bmp = nil then BmpSrc := TsSkinManager(MaskData.Manager).MasterBitmap else BmpSrc := MaskData.Bmp;

  // left - top
  CopyMask(0, 0, wl - 1, wt - 1, dw, MaskData.R.Top, dw + wl - 1, MaskData.R.Top + wt - 1, Bmp, Region, MaskData, True);
  // left - bottom
  CopyMask(0, Bmp.Height - wb, wl - 1, Bmp.Height - 1, dw, MaskData.R.Top + dh - wb, dw + MaskData.WL - 1, MaskData.R.Top + dh - 1, Bmp, Region, MaskData, True);
  // left - middle
  y := wt;
  if (MaskData.DrawMode and BDM_STRETCH = 0) or (MaskData.MaskType = 1) then begin
    while y < Bmp.Height - h - wb do begin
      CopyMask(0, y, wl - 1, y + h - 1, dw, MaskData.R.Top + wt, MaskData.R.Left + dw + wl - 1, MaskData.R.Top + wt + h - 1, Bmp, Region, MaskData, False);
      inc(y, h);
    end;
    if y < Bmp.Height - wb then CopyMask(0, y, wl - 1, Bmp.Height - wb - 1,  dw, MaskData.R.Top + wt, dw + wl - 1, MaskData.R.Top + wt + h - 1, Bmp, Region, MaskData, False);
  end
  else begin
    SetStretchBltMode(Bmp.Canvas.Handle, HALFTONE);
    StretchBlt(Bmp.Canvas.Handle,
             0, y, wl, Bmp.Height - wb - wt,
             BmpSrc.Canvas.Handle,
             dw, MaskData.R.Top + wt, wl, h,
             SRCCOPY);
  end;
  // top - middle
  x := wl;
  if (MaskData.DrawMode and BDM_STRETCH = 0) or (MaskData.MaskType = 1) then begin
    while x < Bmp.Width - w - wr do begin
      CopyMask(x, 0, x + w - 1, wt - 1, dw + wl, MaskData.R.Top, dw + w + wl - 1, MaskData.R.Top + wt - 1, Bmp, Region, MaskData, False);
      inc(x, w);
    end;
    if x < Bmp.Width - wr then CopyMask(x, 0, Bmp.Width - wr - 1, wt - 1, dw + wl, MaskData.R.Top, dw + w + wl - 1, MaskData.R.Top + wt - 1, Bmp, Region, MaskData, False);
  end
  else StretchBlt(Bmp.Canvas.Handle,
             x, 0, Bmp.Width - wr - x, wt,
             BmpSrc.Canvas.Handle,
             dw + wl, MaskData.R.Top, w, wt,
             SRCCOPY);
  // bottom - middle
  x := wl;
  if (MaskData.DrawMode and BDM_STRETCH = 0) or (MaskData.MaskType = 1) then begin
    while x < Bmp.Width - w - wr do begin
      CopyMask(x, Bmp.Height - wb, x + w - 1, Bmp.Height - 1, dw + wl, MaskData.R.Top + dh - wb, dw + w + wl - 1, MaskData.R.Top + dh - 1, Bmp, Region, MaskData, False);
      inc(x, w);
    end;
    if x < Bmp.Width - wr then CopyMask(x, Bmp.Height - wb, Bmp.Width - wr - 1, Bmp.Height - 1, dw + wl, MaskData.R.Top + dh - wb, dw + w + wl - 1, MaskData.R.Top + dh - 1, Bmp, Region, MaskData, False);
  end
  else StretchBlt(Bmp.Canvas.Handle,
             x, Bmp.Height - wb, Bmp.Width - wr - x, wb,
             BmpSrc.Canvas.Handle,
             dw + wl, MaskData.R.Top + dh - wb, w, wb,
             SRCCOPY);
  // right - middle
  y := wt;
  if (MaskData.DrawMode and BDM_STRETCH = 0) or (MaskData.MaskType = 1) then begin
    while y < Bmp.Height - h - wb do begin
      CopyMask(Bmp.Width - wr, y, Bmp.Width - 1, y + h - 1, dw + pw - wr, MaskData.R.Top + wt, dw + pw - 1, MaskData.R.Top + h + wt - 1, Bmp, Region, MaskData, False);
      inc(y, h);
    end;
    if y < Bmp.Height - wb then CopyMask(Bmp.Width - wr, y, Bmp.Width - 1, Bmp.Height - wb - 1, dw + pw - wr, MaskData.R.Top + wt, dw + pw - 1, MaskData.R.Top + h + wt - 1, Bmp, Region, MaskData, False);
  end
  else StretchBlt(Bmp.Canvas.Handle,
             Bmp.Width - wr, y, wr, Bmp.Height - wb - y,
             BmpSrc.Canvas.Handle,
             dw + w + wl, MaskData.R.Top + wt, wr, h,
             SRCCOPY);
  // right - bottom
  CopyMask(Bmp.Width - wr, Bmp.Height - wb, Bmp.Width - 1, Bmp.Height - 1, dw + pw - wr, MaskData.R.Top + dh - wb, dw + pw - 1, MaskData.R.Top + dh - 1, Bmp, Region, MaskData, True);
  // right - top
  CopyMask(Bmp.Width - wr, 0, Bmp.Width - 1, wt - 1, dw + pw - wr, MaskData.R.Top, dw + pw - 1, MaskData.R.Top + wt - 1, Bmp, Region, MaskData, True);
  // Fill
  if Filling and (MaskData.DrawMode and BDM_FILL = BDM_FILL) then begin //???
    y := wt;
    if (MaskData.DrawMode and BDM_STRETCH = 0) or (MaskData.MaskType = 1) then begin
      while y < Bmp.Height - h - wb do begin
        x := wl;
        while x < Bmp.Width - w - wr do begin
          CopyMask(x, y, x + w - 1, y + h - 1, dw + wl, MaskData.R.Top + wt, dw + w + wl - 1, MaskData.R.Top + h + wl - 1, Bmp, Region, MaskData, False);
          inc(x, w);
        end;
        if x < Bmp.Width - wr then CopyMask(x, y, Bmp.Width - wr - 1, y + h, dw + wl, MaskData.R.Top + wt, dw + w + wl - 1, MaskData.R.Top + h + wt - 1, Bmp, Region, MaskData, False);
        inc(y, h);
      end;
      x := wl;
      if y < Bmp.Height - wb then begin
        while x < Bmp.Width - w - wr do begin
          CopyMask(x, y, x + w - 1, Bmp.Height - wb - 1, dw + wl, MaskData.R.Top + wt, dw + w + wl - 1, MaskData.R.Top + h + wt - 1, Bmp, Region, MaskData, False);
          inc(x, w);
        end;
        if x < Bmp.Width - wr then CopyMask(x, y, Bmp.Width - wr - 1, Bmp.Height - wb - 1, dw + wl, MaskData.R.Top + wt, dw + w + wl - 1, MaskData.R.Top + h + wt - 1, Bmp, Region, MaskData, False);
      end;
    end
    else StretchBlt(Bmp.Canvas.Handle,
             wl, wt, Bmp.Width - wr - wl, Bmp.Height - wb - wt,
             BmpSrc.Canvas.Handle,
             dw + wl, MaskData.R.Top + wt, w, h,
             SRCCOPY);
  end;
end;

end.
