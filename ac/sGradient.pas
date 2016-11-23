unit sGradient;
{$I sDefs.inc}

interface

uses
  windows, Graphics, Classes, Controls, SysUtils, StdCtrls, sConst, math,
  Dialogs, Forms, Messages, extctrls, IniFiles;

type

  TsGradFillMode = (fmSolid, fmTransparent);

  TsGradPie = record
    Color1 : TColor;
    Color2 : TColor;
    Percent : TPercent;
    Mode1 : integer;
    Mode2 : integer;
  end;
  TRIVERTEX = packed record
    X, Y : DWORD;
    Red, Green, Blue, Alpha : Word;
  end;

  TsGradArray = array of TsGradPie;

// Fills bitmap by custom properties of Gradient
procedure PaintGrad(Bmp: TBitMap; const aRect : TRect; const Gradient : string); overload;
procedure PaintGrad(Bmp: TBitMap; aRect : TRect; const Data : TsGradArray; OffsetX : integer = 0; OffsetY : integer = 0); overload;
//procedure PaintGradV(Bmp: TBitMap; aRect : TRect; Data : TsGradArray);
procedure PrepareGradArray(const GradientStr : string; var GradArray : TsGradArray);

implementation

uses acntUtils;

{$IFNDEF BCB6}
function GradientFillAC(DC : hDC; pVertex : Pointer; dwNumVertex : DWORD; pMesh : Pointer; dwNumMesh, dwMode: DWORD): DWORD; stdcall; external 'msimg32.dll' name 'GradientFill';
{$ENDIF}
{.$NODEFINE GradientFillAC}

procedure PaintGrad(Bmp: TBitMap; const aRect : TRect; const Gradient : string);
var
  ga : TsGradArray;
begin
  PrepareGradArray(Gradient, ga);
  PaintGrad(Bmp, aRect, ga);
end;

procedure PaintGrad(Bmp: TBitMap; aRect : TRect; const Data : TsGradArray; OffsetX : integer = 0; OffsetY : integer = 0);
const C_HALF : double = 0.5;
var
  SSrc : PRGBArray;
  i, w, dX, dY: Integer;
  R, G, B : single;
  RStep, GStep, BStep, p : real;
  CurrentColor, Color1, Color2 : TsRGB;
  Count, Percent, CurrentX, MaxX, CurrentY, MaxY : integer;
  Y, X : integer;
{$IFNDEF BCB6}
  vert : array[0..4] of TRIVERTEX;
  gRect: array[0..3] of GRADIENT_TRIANGLE;
  c : TsColor;
{$ENDIF}
  function sRGB(Color : TColor) : TsRGB; begin
    Result.R := GetRValue(Color);
    Result.G := GetGValue(Color);
    Result.B := GetBValue(Color);
  end;
begin
  if aRect.Right > Bmp.Width then aRect.Right := Bmp.Width;
  if aRect.Bottom > Bmp.Height then aRect.Bottom := Bmp.Height;
  if aRect.Left < 0 then aRect.Left := 0;
  if aRect.Top < 0 then aRect.Top := 0;

  Count := Length(Data); if Count = 0 then Exit;

  case Data[0].Mode1 of
    0 : begin
      try
        MaxY := aRect.Top + OffsetY;
        for i := 0 to Count - 1 do begin
          Color1 := sRGB(Data[i].Color1);
          Color2 := sRGB(Data[i].Color2);
          Percent := Data[i].Percent;
          CurrentY := MaxY;
          MaxY := CurrentY + ((HeightOf(aRect) + OffsetY) * Percent) div 100;
          if i = (Count - 1) then MaxY := min(aRect.Bottom, Bmp.Height) - 1 else MaxY := min(MaxY, min(aRect.Bottom, Bmp.Height) - 1);
          if MaxY - CurrentY > 0 then begin

            R := Color1.R;
            G := Color1.G;
            B := Color1.B;

            if (i = (Count - 1)) or (MaxY > bmp.Height - 1) then begin
              MaxY := min(aRect.Bottom - 1, bmp.Height - 1);
            end;

            dY := MaxY - CurrentY;
            if dY = 0 then Exit;
            w := min(WidthOf(aRect) + aRect.Left, bmp.Width) - 1;

            RStep := (Color2.R - Color1.R) / dY;
            GStep := (Color2.G - Color1.G) / dY;
            BStep := (Color2.B - Color1.B) / dY;

            for Y := CurrentY to MaxY do begin
              SSrc := Bmp.ScanLine[Y];

              CurrentColor.R := Round(R);
              CurrentColor.G := Round(G);
              CurrentColor.B := Round(B);

              for X := aRect.Left to w do SSrc[X] := CurrentColor;

              R := R + RStep;
              G := G + GStep;
              B := B + BStep;

            end;
          end;
        end;
      finally
      end;
    end;
    1 : begin
      try
        p := WidthOf(aRect) / 100;
        for CurrentY := aRect.Top to aRect.Bottom - 1 do begin

          SSrc := Bmp.ScanLine[CurrentY];
          MaxX := aRect.Left;

          for i := 0 to Count - 1 do begin
            Color1 := sRGB(Data[i].Color1);
            Color2 := sRGB(Data[i].Color2);
            Percent := Data[i].Percent;
            CurrentX := MaxX;
            MaxX := Round(CurrentX + (p * Percent));
            if i = (Count - 1) then MaxX := min(aRect.Right, Bmp.Width) - 1 else MaxX := min(MaxX, min(aRect.Right, Bmp.Width) - 1);
            if MaxX - CurrentX > 0 then begin
              dX := MaxX - CurrentX;

              R := Color1.R;
              G := Color1.G;
              B := Color1.B;

              RStep := (Color2.R - Color1.R) / dX;
              GStep := (Color2.G - Color1.G) / dX;
              BStep := (Color2.B - Color1.B) / dX;

              for X := CurrentX to MaxX do begin
                CurrentColor.R := Round(R);
                CurrentColor.G := Round(G);
                CurrentColor.B := Round(B);
                SSrc[X] := CurrentColor;
                R := R + RStep;
                G := G + GStep;
                B := B + BStep;
              end;
            end;
          end;
        end;
      except
      end;
    end;
{$IFNDEF BCB6}
    2 : begin // Triangles

      if Count > 0 then c.C := Data[0].Color1 else c.C := 0;
      // Left-top
      vert[0].Alpha := 0;
      vert[0].x := aRect.Left;
      vert[0].y:= aRect.Top;
      vert[0].Red := c.R shl 8;
      vert[0].Green := c.G shl 8;
      vert[0].Blue := c.B shl 8;

      if Count > 1 then c.C := Data[1].Color1;
      // Center
      vert[1].Alpha := 0;
      vert[1].x := aRect.Left + WidthOf(aRect) div 2;
      vert[1].y:= aRect.Top + HeightOf(aRect) div 2;
      vert[1].Red := c.R shl 8;
      vert[1].Green := c.G shl 8;
      vert[1].Blue := c.B shl 8;

      if Count > 2 then c.C := Data[2].Color1;
      // Right-top
      vert[2].Alpha := 0;
      vert[2].x := aRect.Right;
      vert[2].y:= aRect.Top;
      vert[2].Red := c.R shl 8;
      vert[2].Green := c.G shl 8;
      vert[2].Blue := c.B shl 8;

      if Count > 3 then c.C := Data[3].Color1;
      // Right-bottom
      vert[3].Alpha := 0;
      vert[3].x := aRect.Right;
      vert[3].y:= aRect.Bottom;
      vert[3].Red := c.R shl 8;
      vert[3].Green := c.G shl 8;
      vert[3].Blue := c.B shl 8;

      if Count > 4 then c.C := Data[4].Color1;
      // Left-bottom
      vert[4].Alpha := 0;
      vert[4].x := aRect.Left;
      vert[4].y:= aRect.Bottom;
      vert[4].Red := c.R shl 8;
      vert[4].Green := c.G shl 8;
      vert[4].Blue := c.B shl 8;

      gRect[0].Vertex1 := 0; // Top
      gRect[0].Vertex2 := 1;
      gRect[0].Vertex3 := 2;

      gRect[1].Vertex1 := 1; // Right
      gRect[1].Vertex2 := 2;
      gRect[1].Vertex3 := 3;

      gRect[2].Vertex1 := 0; // Left
      gRect[2].Vertex2 := 1;
      gRect[2].Vertex3 := 4;

      gRect[3].Vertex1 := 4; // Bottom
      gRect[3].Vertex2 := 1;
      gRect[3].Vertex3 := 3;

      GradientFillAC(Bmp.Canvas.Handle, @vert, 5, @gRect, 4, GRADIENT_FILL_TRIANGLE);
    end;
{$ENDIF}
  end;
end;
{
procedure PaintGradV(Bmp: TBitMap; aRect : TRect; Data : TsGradArray);
var
  SSrc : PRGBArray;
  i, w, dY: Integer;
  R, G, B : real;
  RStep, GStep, BStep : real;
  SavedDC : longint;
  CurrentColor, Color1, Color2 : TsRGB;
  Count, Percent, CurrentY, MaxY : integer;

  function GetRGB(Color : TColor) : TsRGB; begin
    Result.R := GetRValue(Color);
    Result.G := GetGValue(Color);
    Result.B := GetBValue(Color);
  end;

  procedure Paint(Color1, Color2 : TsRGB; cY, mY : integer; Last : boolean);
  var
    Y, X : integer;
  begin
    R := Color1.R;
    G := Color1.G;
    B := Color1.B;

    if Last or (my > bmp.Height - 1) then begin
      mY := min(aRect.Bottom - 1, bmp.Height - 1);
    end;

    dY := mY - cY;
    if dY = 0 then Exit;
    w := min(WidthOf(aRect) + aRect.Left, bmp.Width);

    RStep := (Color2.R - Color1.R) / dY;
    GStep := (Color2.G - Color1.G) / dY;
    BStep := (Color2.B - Color1.B) / dY;

    for Y := cY to mY do begin
      SSrc := Bmp.ScanLine[Y];
      CurrentColor.R := Round(R);
      CurrentColor.G := Round(G);
      CurrentColor.B := Round(B);

      for X := aRect.Left to w - 1 do begin
        SSrc[X] := CurrentColor;
      end;

      R := R + RStep;
      G := G + GStep;
      B := B + BStep;
    end;
  end;
begin
  if aRect.Right > Bmp.Width then
    aRect.Right := Bmp.Width;
  if aRect.Bottom > Bmp.Height then
    aRect.Bottom := Bmp.Height;
  if aRect.Left < 0 then aRect.Left := 0;
  if aRect.Top < 0 then aRect.Top := 0;

  Count := Length(Data); if Count = 0 then Exit;

  SavedDC := SaveDC(Bmp.Canvas.Handle);
  try

    MaxY := aRect.Top;

    for i := 0 to Count - 1 do begin
      Color1 := GetRGB(Data[i].Color1);
      Color2 := GetRGB(Data[i].Color2);
      Percent := Data[i].Percent;
      CurrentY := MaxY;
      MaxY := CurrentY + (HeightOf(aRect) * Percent) div 100;
      if i = (Count - 1) then
          MaxY := Bmp.Height - 1;
      if MaxY - CurrentY > 0 then begin
        Paint(Color1, Color2, CurrentY, MaxY, i = (Count - 1));
      end;
    end;

  finally
    RestoreDC(Bmp.Canvas.Handle, SavedDC);
  end;
end;
}
procedure PrepareGradArray(const GradientStr : string; var GradArray : TsGradArray);
var
  Count, i : integer;
begin
  SetLength(GradArray, 0);
  if GradientStr = '' then Exit;

  Count := WordCount(GradientStr, [';']) div 5;
  SetLength(GradArray, Count);
  for i := 0 to Count - 1 do begin
    GradArray[i].Color1 := StrToInt(ExtractWord(i * 5 + 1, GradientStr, [';']));
    GradArray[i].Color2 := StrToInt(ExtractWord(i * 5 + 2, GradientStr, [';']));
    GradArray[i].Percent := min(100, StrToInt(ExtractWord(i * 5 + 3, GradientStr, [';'])));
    GradArray[i].Mode1 := StrToInt(ExtractWord(i * 5 + 4, GradientStr, [';']));
  end;
end;

end.


