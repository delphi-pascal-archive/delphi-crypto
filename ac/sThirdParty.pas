unit sThirdParty;
{$I sDefs.inc}

interface

uses
  Messages, SysUtils, Classes, Windows, Graphics, Controls, Forms, Dialogs, ActnList, imglist,
    comctrls, StdCtrls, sCommonData, sConst, sBitBtn, sSpeedButton{$IFNDEF ALITE}, sToolBar{$ENDIF}
    {$IFDEF USEPNG}, PngImageList, PngFunctions, PngImage{$ENDIF};

procedure InitDevEx(const Active : boolean);
function CheckDevEx(const Control : TControl) : boolean;
function GetImageCount(ImgList : TCustomImageList) : integer;
procedure DrawBtnGlyph(Button : TControl; Canvas : TCanvas = nil);
{$IFNDEF ALITE}
procedure CopyToolBtnGlyph(ToolBar : TsToolBar; Button : TToolButton; State: TCustomDrawState; Stage: TCustomDrawStage; var Flags: TTBCustomDrawFlags; BtnBmp : TBitmap);
{$ENDIF}

implementation

uses sGraphUtils, acAlphaImageList, CommCtrl, acntUtils, acPNG, sAlphaGraph
{$IFDEF DEVEX}
  , cxLookAndFeels, cxLookAndFeelPainters, acLFPainter, cxControls, dxSkinInfo
{$ENDIF};

{$IFDEF DEVEX}
type
  TAcesscxControl = class(TcxControl);
const
  s_AlphaSkins = 'AlphaSkins';
var
  OldDXSkin : string;
{$ENDIF}

procedure InitDevEx(const Active : boolean);
{$IFDEF DEVEX}
var
 vPainter: TcxCustomLookAndFeelPainterClass;
begin
 if Active then
 begin
   if not GetExtendedStylePainters.GetPainterByName(s_AlphaSkins, vPainter) then
   begin
     GetExtendedStylePainters.Register(s_AlphaSkins, TcxACLookAndFeelPainter, TdxSkinInfo.Create(nil));
     RootLookAndFeel.SkinName := s_AlphaSkins;
   end
 end else
   if GetExtendedStylePainters.GetPainterByName(s_AlphaSkins, vPainter) then
   begin
     RootLookAndFeel.SkinName := '';
     GetExtendedStylePainters.Unregister(s_AlphaSkins);
   end
end;
{$ELSE}
begin
end;
{$ENDIF}

function CheckDevEx(const Control : TControl) : boolean;
begin
{$IFDEF DEVEX}
  if (RootLookAndFeel.SkinName = s_AlphaSkins) then begin
    if Control.ClassName = 'TcxGrid' then begin
      TAcesscxControl(Control).Loaded;
      Result := True;
    end
    else if (Control.ClassName = 'TcxPivotGrid') or (Control.ClassName = 'TcxDBPivotGrid') then begin
      TAcesscxControl(Control).FontChanged;
      Result := True;
    end
    else if Control.ClassName = 'TcxScheduler' then begin
      TAcesscxControl(Control).Loaded;
      Result := True;
    end
    else if (Control.ClassName = 'TcxVerticalGrid') or (Control.ClassName = 'TcxVirtualVerticalGrid') or (Control.ClassName = 'TcxDBVerticalGrid') then begin
      TAcesscxControl(Control).Loaded;
      Result := True;
    end
    else if Control is TcxControl then begin
      TAcesscxControl(Control).Invalidate;
      Result := True;
    end
    else Result := False;
  end
  else
{$ENDIF}
  Result := False;
end;

function GetImageCount(ImgList : TCustomImageList) : integer;
begin
  Result := 0;
  if ImgList = nil then Exit;
{$IFDEF USEPNG}
  if ImgList is TPngImageList then begin
    Result := TPngImageList(ImgList).PngImages.Count;
  end
  else
{$ENDIF}
  Result := ImgList.Count;
end;

procedure MakeMask32(const Bmp : TBitmap);
var
  MaskColor: TsColor;
  TransColor: TsColor;
  X, Y : integer;
begin
  TransColor.C := Bmp.Canvas.Pixels[0, Bmp.Height - 1];
  TransColor.A := 255;
  if Fast32Src.Attach(Bmp) then begin
    for X := 0 to Bmp.Width - 1 do for Y := 0 to Bmp.Height - 1 do begin
      MaskColor := Fast32Src[X, Y];
      if MaskColor.C <> TransColor.C then begin
        MaskColor.A := 255;
        Fast32Src[X, Y] := MaskColor;
      end
      else Fast32Src[X, Y] := sFuchsia;
    end;
  end;
end;

procedure DrawBtnGlyph(Button : TControl; Canvas : TCanvas = nil);
var
  IRect : TRect;
  Bmp : TBitmap;
  MaskColor: TsColor;
  TmpPng : TPNGGraphic;

  Images : TCustomImageList;
  Glyph : TBitmap;
  ImageIndex : integer;
  SkinData : TsCommonData;
  ImgRect : TRect;
  NumGlyphs : integer;
  Enabled : boolean;
  DisabledGlyphKind : TsDisabledGlyphKind;
  Blend : integer;
  Down : boolean;
  CurrentState : integer;
  Grayed, Reflected : boolean;
{$IFDEF USEPNG}
  PngCopy: TPNGObject;
{$ENDIF}
  procedure PrepareGlyph; begin
    Bmp.Width := Images.Width;
    Bmp.Height := Images.Height;
    Bmp.PixelFormat := pf24bit;
    if Images.BkColor <> clNone then MaskColor.C := Images.BkColor else MaskColor.C := clFuchsia;
    Bmp.Canvas.Brush.Color := MaskColor.C;
    Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));
    Images.GetBitmap(ImageIndex, Bmp);
  end;
begin
  if Button is TsBitBtn then begin
    Images     := TsBitBtn(Button).Images;
    Glyph      := TsBitBtn(Button).Glyph;
    ImageIndex := TsBitBtn(Button).ImageIndex;
    SkinData   := TsBitBtn(Button).SkinData;
    ImgRect    := TsBitBtn(Button).ImgRect;
    NumGlyphs  := TsBitBtn(Button).NumGlyphs;
    Enabled    := TsBitBtn(Button).Enabled;
    Blend      := TsBitBtn(Button).Blend;
    Down       := TsBitBtn(Button).Down;
    Grayed     := TsBitBtn(Button).Grayed;
    CurrentState := TsBitBtn(Button).CurrentState;
    DisabledGlyphKind := TsBitBtn(Button).DisabledGlyphKind;
    Reflected := TsBitBtn(Button).Reflected;
  end
  else if Button is TsSpeedButton then begin
    Images     := TsSpeedButton(Button).Images;
    Glyph      := TsSpeedButton(Button).Glyph;
    ImageIndex := TsSpeedButton(Button).ImageIndex;
    SkinData   := TsSpeedButton(Button).SkinData;
    ImgRect    := TsSpeedButton(Button).ImgRect;
    NumGlyphs  := TsSpeedButton(Button).NumGlyphs;
    Enabled    := TsSpeedButton(Button).Enabled;
    Blend      := TsSpeedButton(Button).Blend;
    Down       := TsSpeedButton(Button).Down;
    Grayed     := TsSpeedButton(Button).Grayed;
    CurrentState := TsSpeedButton(Button).CurrentState;
    DisabledGlyphKind := TsSpeedButton(Button).DisabledGlyphKind;
    Reflected := TsSpeedButton(Button).Reflected;
  end
  else Exit;
  if Canvas = nil then Canvas := SkinData.FCacheBmp.Canvas;

  if not Glyph.Empty then begin
    if {(Glyph is TPNGGraphic) and }(NumGlyphs = 1) and (Blend = 0) and (Glyph.PixelFormat = pf32bit) then begin // Patch if Png, don't work in std. mode
      TmpPng := TPNGGraphic.Create;
      TmpPng.PixelFormat := pf32bit;
      TmpPng.Width := Glyph.Width;
      TmpPng.Height := Glyph.Height;
      BitBlt(TmpPng.Canvas.Handle, 0, 0, Glyph.Width, Glyph.Height, Glyph.Canvas.Handle, 0, 0, SRCCOPY);
      TmpPng.Reflected := Reflected;
      if SkinData.FCacheBmp.Canvas = Canvas then SkinData.FCacheBmp.Canvas.Draw(ImgRect.Left, ImgRect.Top, TmpPng) else begin
        Bmp := CreateBmp24(Button.Width, Button.Height);
        BitBlt(Bmp.Canvas.Handle, 0, 0, Button.Width, Button.Height, Canvas.Handle, 0, 0, SRCCOPY);
        Bmp.Canvas.Draw(ImgRect.Left, ImgRect.Top, TmpPng);
        BitBlt(Canvas.Handle, 0, 0, Button.Width, Button.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
        Bmp.Free;
      end;
      TmpPng.Free;
{ May be optimized !!!
        Bmp := CreateBmp24(Glyph.Width, Glyph.Height + Integer(Reflected) * (Glyph.Height div 2));
        BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, Canvas.Handle, ImgRect.Left, ImgRect.Top, SRCCOPY);
        CopyBmp32(Rect(0, 0, Glyph.Width, Glyph.Height), Rect(0, 0, Glyph.Width, Glyph.Height), Bmp, Glyph, EmptyCI, False, clNone, 0, Reflected);
        BitBlt(Canvas.Handle, ImgRect.Left, ImgRect.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
        Bmp.Free;
}
    end
    else begin
      if Canvas <> SkinData.FCacheBmp.Canvas then begin
        Bmp := CreateBmp24(Button.Width, Button.Height);
        BitBlt(Bmp.Canvas.Handle, 0, 0, Button.Width, Button.Height, Canvas.Handle, 0, 0, SRCCOPY);

        if Reflected then begin
          Glyph.PixelFormat := pf32bit;
          MakeMask32(Glyph);             
          CopyBmp32(Classes.Rect(0, 0, Glyph.Width, Glyph.Height), Classes.Rect(0, 0, Glyph.Width, Glyph.Height), Bmp, Glyph, EmptyCI, False, clNone, 0, Reflected);
        end
        else sGraphUtils.DrawGlyphEx(Glyph, Bmp, ImgRect, NumGlyphs, Enabled, DisabledGlyphKind, integer(ControlIsActive(SkinData)), Blend, Down);
        BitBlt(Canvas.Handle, 0, 0, Button.Width, Button.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
        Bmp.Free;
      end
      else sGraphUtils.DrawGlyphEx(Glyph, SkinData.FCacheBmp, ImgRect, NumGlyphs, Enabled, DisabledGlyphKind, integer(ControlIsActive(SkinData)), Blend, Down);
    end;
  end
  else if Assigned(Images) and (ImageIndex > -1) and (GetImageCount(Images) > ImageIndex) then begin
    IRect := ImgRect;
{$IFDEF USEPNG}
    if Images is TPngImageList then begin
      PngCopy := nil;
      if Enabled then begin
        if ControlIsActive(SkinData) or ((Blend = 0) and not Grayed) then begin
          PngCopy := TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[ImageIndex]).PngImage;
          if SkinData.Skinned
            then PngCopy.Draw(SkinData.FCacheBmp.Canvas, IRect)
            else PngCopy.Draw(Canvas, IRect);
        end
        else begin
          if Blend > 0 then begin
            PngCopy := TPNGObject.Create;
            PngCopy.Assign(TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[ImageIndex]).PngImage);
            MakeImageBlended(PngCopy);
          end;
          if Grayed then begin
            if PngCopy = nil then begin
              PngCopy := TPNGObject.Create;
              PngCopy.Assign(TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[ImageIndex]).PngImage);
            end;
            MakeImageGrayscale(PngCopy);
          end;
          if SkinData.Skinned
            then PngCopy.Draw(SkinData.FCacheBmp.Canvas, IRect)
            else PngCopy.Draw(Canvas, IRect);
          FreeAndNil(PngCopy);
        end;
      end
      else begin
        if dgBlended in DisabledGlyphKind then begin
          PngCopy := TPNGObject.Create;
          PngCopy.Assign(TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[ImageIndex]).PngImage);
          MakeImageBlended(PngCopy);
        end;
        if dgGrayed in DisabledGlyphKind then begin
          if PngCopy = nil then begin
            PngCopy := TPNGObject.Create;
            PngCopy.Assign(TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[ImageIndex]).PngImage);
          end;
          MakeImageGrayscale(PngCopy);
        end;
        if PngCopy = nil then begin
          PngCopy := TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[ImageIndex]).PngImage;
          if SkinData.Skinned then PngCopy.Draw(SkinData.FCacheBmp.Canvas, IRect) else PngCopy.Draw(Canvas, IRect);
        end
        else begin
          if SkinData.Skinned then PngCopy.Draw(SkinData.FCacheBmp.Canvas, IRect) else PngCopy.Draw(Canvas, IRect);
          FreeAndNil(PngCopy);
        end;
      end;
    end
    else
{$ENDIF}
    if (Images is TsAlphaImageList) or not SkinData.Skinned then begin
      if SkinData.Skinned
        then DrawAlphaImgList(Images, SkinData.FCacheBmp, IRect.Left, IRect.Top, ImageIndex, iffi(CurrentState = 0, Blend, 0), iffi((CurrentState = 0) and Grayed, SkinData.SkinManager.gd[SkinData.SkinIndex].Color, clNone), CurrentState, NumGlyphs, Reflected)
        else DrawAlphaImgListDC(Images, Canvas.Handle, IRect.Left, IRect.Top, ImageIndex, iffi(CurrentState = 0, Blend, 0), iffi((CurrentState = 0) and Grayed, ColorToRGB(clBtnFace), clNone), CurrentState, NumGlyphs, Reflected);
    end
    else begin
      Bmp := TBitmap.Create;
      try
        PrepareGlyph;
        if not Enabled then begin
          if dgGrayed in DisabledGlyphKind then begin
            GrayScaleTrans(Bmp, ColorToSColor(Bmp.Canvas.Pixels[0, 0]));
          end;
          if dgBlended in DisabledGlyphKind then begin
            BlendTransRectangle(SkinData.FCacheBmp, IRect.Left, IRect.Top, Bmp,
                                  Rect(0, 0, Bmp.Width, Bmp.Height), 0.5, MaskColor);
          end
          else begin
            CopyTransBitmaps(SkinData.FCacheBmp, Bmp, IRect.Left, IRect.Top, MaskColor);
          end;
        end
        else begin
          if not ControlIsActive(SkinData) and Grayed then GrayScaleTrans(Bmp, ColorToSColor(Bmp.Canvas.Pixels[0, 0]));
          if not ControlIsActive(SkinData) and (Blend > 0)
            then BlendTransRectangle(SkinData.FCacheBmp, IRect.Left, IRect.Top, Bmp, Rect(0, 0, Bmp.Width, Bmp.Height), Blend / 100, MaskColor)
            else CopyTransBitmaps(SkinData.FCacheBmp, Bmp, IRect.Left, IRect.Top, MaskColor);
        end;
      finally
        FreeAndNil(Bmp);
      end;
    end;
  end;
end;

procedure DrawSpeedButtonGlyph(Button : TsSpeedButton);
var
  IRect : TRect;
  Bmp : TBitmap;
  MaskColor: TsColor;
{$IFDEF USEPNG}
  PngCopy: TPNGObject;
{$ENDIF}
  procedure PrepareGlyph; begin
    with Button do begin
      Bmp.Width := Images.Width;
      Bmp.Height := Images.Height;
      Bmp.PixelFormat := pf24bit;
      if Button.Images.BkColor <> clNone then MaskColor.C := Button.Images.BkColor else MaskColor.C := clFuchsia;
      Bmp.Canvas.Brush.Color := MaskColor.C;
      Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));
      Images.GetBitmap(ImageIndex, Bmp);
    end;
  end;
begin
  with Button do begin
    if not Glyph.Empty then begin
      sGraphUtils.DrawGlyphEx(Glyph, SkinData.FCacheBmp, ImgRect, NumGlyphs, Enabled, DisabledGlyphKind, Button.CurrentState, Blend, Button.Down);
    end
    else if Assigned(Button.Images) and (Button.ImageIndex > -1) and (GetImageCount(Images) > ImageIndex) then begin
      IRect := ImgRect;
  {$IFDEF USEPNG}
      if Images is TPngImageList then begin
        PngCopy := nil;
        if Enabled then begin
          if ControlIsActive(SkinData) or ((Blend = 0) and not Grayed) then begin
            PngCopy := TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[ImageIndex]).PngImage;
            PngCopy.Draw(SkinData.FCacheBmp.Canvas, IRect);
          end
          else begin
            if Blend > 0 then begin
              PngCopy := TPNGObject.Create;
              PngCopy.Assign(TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[ImageIndex]).PngImage);
              MakeImageBlended(PngCopy);
            end;
            if Grayed then begin
              if PngCopy = nil then begin
                PngCopy := TPNGObject.Create;
                PngCopy.Assign(TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[ImageIndex]).PngImage);
              end;
              MakeImageGrayscale(PngCopy);
            end;
            PngCopy.Draw(SkinData.FCacheBmp.Canvas, IRect);
            FreeAndNil(PngCopy);
          end;
        end
        else begin
          if dgBlended in DisabledGlyphKind then begin
            PngCopy := TPNGObject.Create;
            PngCopy.Assign(TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[ImageIndex]).PngImage);
            MakeImageBlended(PngCopy);
          end;
          if dgGrayed in DisabledGlyphKind then begin
            if PngCopy = nil then begin
              PngCopy := TPNGObject.Create;
              PngCopy.Assign(TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[ImageIndex]).PngImage);
            end;
            MakeImageGrayscale(PngCopy);
          end;
          if PngCopy = nil then begin
            PngCopy := TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[ImageIndex]).PngImage;
            PngCopy.Draw(SkinData.FCacheBmp.Canvas, IRect);
          end
          else begin
            PngCopy.Draw(SkinData.FCacheBmp.Canvas, IRect);
            FreeAndNil(PngCopy);
          end;
        end;
      end
      else
  {$ENDIF}
      if (Images is TsAlphaImageList) or not SkinData.Skinned then begin
        if SkinData.Skinned
          then DrawAlphaImgList(Images, SkinData.FCacheBmp, IRect.Left, IRect.Top, ImageIndex, iffi(CurrentState = 0, Blend, 0), iffi((CurrentState = 0) and Grayed, clNone, clNone), CurrentState, NumGlyphs, Reflected)
          else DrawAlphaImgListDC(Images, Canvas.Handle, IRect.Left, IRect.Top, ImageIndex, iffi(CurrentState = 0, Blend, 0), iffi((CurrentState = 0) and Grayed, clNone, clNone), CurrentState, NumGlyphs, Reflected);
      end
      else begin
        Bmp := TBitmap.Create;
        try
          PrepareGlyph;
          if not Enabled then begin
            if dgGrayed in DisabledGlyphKind then GrayScaleTrans(Bmp, ColorToSColor(Bmp.Canvas.Pixels[0, Bmp.Height - 1]));
            if dgBlended in DisabledGlyphKind
              then BlendTransRectangle(SkinData.FCacheBmp, IRect.Left, IRect.Top, Bmp, Rect(0, 0, Bmp.Width, Bmp.Height), 0.5, MaskColor)
              else CopyTransBitmaps(SkinData.FCacheBmp, Bmp, IRect.Left, IRect.Top, MaskColor);
          end
          else begin
            if not ControlIsActive(SkinData) and Grayed then GrayScaleTrans(Bmp, ColorToSColor(Bmp.Canvas.Pixels[0, Bmp.Height - 1]));

            if not ControlIsActive(SkinData) and (Blend > 0)
              then BlendTransRectangle(SkinData.FCacheBmp, IRect.Left, IRect.Top, Bmp, Rect(0, 0, Bmp.Width, Bmp.Height), Blend / 100, MaskColor)
              else CopyTransBitmaps(SkinData.FCacheBmp, Bmp, IRect.Left, IRect.Top, MaskColor);
          end;
        finally
          FreeAndNil(Bmp);
        end;
      end;
    end;
  end;
end;

{$IFNDEF ALITE}
procedure CopyToolBtnGlyph(ToolBar : TsToolBar; Button : TToolButton; State: TCustomDrawState; Stage: TCustomDrawStage; var Flags: TTBCustomDrawFlags; BtnBmp : TBitmap);
var
  IRect : TRect;
  Mode : integer;
  Bmp : TBitmap;
  MaskColor : TsColor;
{$IFDEF USEPNG}
  PngCopy: TPNGObject;
{$ENDIF}
  function AddedWidth : integer; begin
    Result := integer(Button.Style = tbsDropDown) * 8;
  end;
  function ImgRect : TRect;
  begin
    with ToolBar do begin
      if not List then begin
        Result.Left := (Button.Width - Images.Width) div 2 + 1 - AddedWidth;
        Result.Top := (Button.Height - Images.Height - integer(ShowCaptions) * (SkinData.FCacheBMP.Canvas.TextHeight('A') + 3)) div 2;
        Result.Right := Result.Left + Images.Width;
        Result.Bottom := Result.Top + Images.Height;
      end
      else begin
        Result.Left := 5;
        Result.Top := (Button.Height - Images.Height) div 2;
        Result.Right := Result.Left + Images.Width;
        Result.Bottom := Result.Top + Images.Height;
      end;
      if (Mode = 2) then OffsetRect(Result, 1, 1);
    end;
  end;
  function Imges : TCustomImageList; begin
    with ToolBar do
    if (Mode <> 0) and Assigned(HotImages) and (Button.ImageIndex < HotImages.Count) then begin
      Result := HotImages;
    end
    else Result := Images;
  end;
  procedure PrepareGlyph;
  begin
    Bmp.Width := Imges.Width;
    Bmp.Height := Imges.Height;
    Bmp.PixelFormat := pf24bit;
    if ToolBar.Images.BkColor <> clNone then MaskColor.C := ToolBar.Images.BkColor else MaskColor.C := clFuchsia;
    Bmp.Canvas.Brush.Color := MaskColor.C;
    Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));
    Imges.GetBitmap(Button.ImageIndex, Bmp);
  end;
begin
  with ToolBar do begin
    if (State = []) or (State = [cdsDisabled]) then Mode := 0 else if (cdsSelected in State) or (cdsChecked in State) then Mode := 2 else Mode := 1;
    IRect := ImgRect;
{$IFDEF USEPNG}
    if Imges is TPngImageList then begin
      PngCopy := nil;
      if Enabled then begin
        PngCopy := TPngImageCollectionItem(TPngImageList(Imges).PngImages.Items[Button.ImageIndex]).PngImage;
        PngCopy.Draw(BtnBmp.Canvas, IRect);
      end
      else begin
        PngCopy := TPNGObject.Create;
        PngCopy.Assign(TPngImageCollectionItem(TPngImageList(Imges).PngImages.Items[Button.ImageIndex]).PngImage);
        MakeImageBlended(PngCopy);
        if PngCopy = nil then begin
          PngCopy := TPngImageCollectionItem(TPngImageList(Imges).PngImages.Items[Button.ImageIndex]).PngImage;
          PngCopy.Draw(BtnBmp.Canvas, IRect);
        end
        else begin
          PngCopy.Draw(BtnBmp.Canvas, IRect);
          FreeAndNil(PngCopy);
        end;
      end;
    end
    else
{$ENDIF}
    if (Images is TsAlphaImageList) or not SkinData.Skinned then begin
      IRect := ImgRect;
      Images.Draw(BtnBmp.Canvas, IRect.Left, IRect.Top, Button.ImageIndex)
    end
    else begin
      Bmp := TBitmap.Create;
      try
        PrepareGlyph;
        CopyTransBitmaps(BtnBmp, Bmp, ImgRect.left, ImgRect.Top, MaskColor);
      finally
        FreeAndNil(Bmp);
      end;
    end;
  end;
end;
{$ENDIF}

end.
