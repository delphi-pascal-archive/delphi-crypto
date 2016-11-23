unit sGlyphUtils;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  sConst, buttons, acntUtils, sGraphUtils, sDefaults, ImgList, acAlphaImageList, sSpeedButton;

type

  TsGlyphMode = class(TPersistent)
  private
    FOwner : TWinControl;
    FImages: TCustomImageList;
    FImageIndex: integer;
    FImageIndexHot: integer;
    FImageIndexPressed: integer;
    procedure SetBlend(const Value: integer);
    function GetHint: string;
    procedure SetHint(const Value: string);
    procedure SetGrayed(const Value: boolean);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetImageIndex(const Value: integer);
    procedure SetImageIndexHot(const Value: integer);
    procedure SetImageIndexPressed(const Value: integer);
    function ReadBlend: integer;
    function ReadGrayed: boolean;
    function BtnIsReady : boolean;
  public
    Btn : TsSpeedButton;
    constructor Create(AOwner: TWinControl);
    procedure Invalidate;
    function ImageCount : integer;
    function Width: Integer;
    function Height: Integer;
  published
    property Blend : integer read ReadBlend write SetBlend;
    property Grayed: boolean read ReadGrayed write SetGrayed;
    property Hint: string read GetHint write SetHint;
    property Images : TCustomImageList read FImages write SetImages;
    property ImageIndex : integer read FImageIndex write SetImageIndex default -1;
    property ImageIndexHot : integer read FImageIndexHot write SetImageIndexHot default -1;
    property ImageIndexPressed : integer read FImageIndexPressed write SetImageIndexPressed default -1;
  end;

const
  iBTN_OPENFILE   = 0;
  iBTN_OPENFOLDER = 1;
  iBTN_DATE       = 2;
  iBTN_ELLIPSIS   = 3;
  iBTN_CALC       = 4;

  acGlyphsResNames : array [0..4] of string = ('SF', 'SR', 'SD', 'SE', 'SC');

var
  acResImgList : TsAlphaImageList;

implementation

uses sCustomComboEdit, sComboBox,
{$IFNDEF ALITE}
  sCurrencyEdit,
{$ENDIF}
  sMessages, acPNG, CommCtrl {$IFDEF UNICODE}, PngImage{$ENDIF};

{ TsGlyphMode }

constructor TsGlyphMode.Create(AOwner: TWinControl);
begin
  Btn := nil;
  FOwner := AOwner;
  FImageIndex := -1;
  FImageIndexHot := -1;
  FImageIndexPressed := -1;
end;

function TsGlyphMode.GetHint: string;
begin
  if (FOwner is TsCustomComboEdit) and (TsCustomComboEdit(FOwner).Button <> nil) then begin
    Result := TsCustomComboEdit(FOwner).Button.Hint;
  end
  else Result := TsComboBox(FOwner).Hint;
end;

procedure TsGlyphMode.SetBlend(const Value: integer);
begin
  if BtnIsReady then Btn.Blend := Value;
end;

procedure TsGlyphMode.SetHint(const Value: string);
begin
  if (FOwner is TsCustomComboEdit) and (TsCustomComboEdit(FOwner).Button <> nil) then begin
    TsCustomComboEdit(FOwner).Button.Hint := Value;
  end;
end;

procedure TsGlyphMode.SetGrayed(const Value: boolean);
begin
  if BtnIsReady then Btn.Grayed := Value;
end;

procedure TsGlyphMode.Invalidate;
begin
  if FOwner is TsCustomComboEdit then begin
    TsCustomComboEdit(FOwner).Button.Width := Width + 2;
    TsCustomComboEdit(FOwner).Button.NumGlyphs := ImageCount;
    TsCustomComboEdit(FOwner).Button.Invalidate;
  end;
end;

function TsGlyphMode.Width: Integer;
begin
  if FOwner is TsCurrencyEdit then begin
    Result := 0;
    Exit;
  end;
  if Assigned(FImages) and (ImageIndex > -1) and (ImageIndex < FImages.Count) then begin
    Result := FImages.Width div ImageCount;
  end
  else begin
    Result := acResImgList.Width div ImageCount;
  end;
end;

function TsGlyphMode.Height: Integer;
begin
  if Assigned(FImages) and (ImageIndex > -1) and (ImageIndex < FImages.Count) then begin
    Result := FImages.Height
  end
  else begin
    Result := acResImgList.Height
  end;
end;

function TsGlyphMode.ImageCount: integer;
var
  w, h : integer;
begin
  if Assigned(FImages) and (ImageIndex > -1) and (ImageIndex < FImages.Count) then begin
    w := FImages.Width;
    h := FImages.Height
  end
  else begin
    w := acResImgList.Width;
    h := acResImgList.Height
  end;
  if w mod h = 0 then Result := w div h else Result := 1;
end;

procedure TsGlyphMode.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then begin
    FImages := Value;
    Invalidate;
  end;
end;

procedure TsGlyphMode.SetImageIndex(const Value: integer);
begin
  if FImageIndex <> Value then begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

procedure TsGlyphMode.SetImageIndexHot(const Value: integer);
begin
  if FImageIndexHot <> Value then begin
    FImageIndexHot := Value;
    Invalidate;
  end;
end;

procedure TsGlyphMode.SetImageIndexPressed(const Value: integer);
begin
  if FImageIndexPressed <> Value then begin
    FImageIndexPressed := Value;
    Invalidate;
  end;
end;

function TsGlyphMode.ReadBlend: integer;
begin
  if BtnIsReady then Result := Btn.Blend else Result := 0;
end;

function TsGlyphMode.ReadGrayed: boolean;
begin
  if BtnIsReady then Result := Btn.Grayed else Result := False;
end;

function TsGlyphMode.BtnIsReady: boolean;
begin
  Result := (Btn <> nil) and not (csLoading in Btn.ComponentState) and not (csDestroying in Btn.ComponentState)
end;
{
procedure Ico2Bmp(Icon : HIcon; Bmp : TBitmap);
var
  SDC, DDC: HDC;
  hBMP: HBitmap;
  TheBitmap:Pbitmap;
  iINFO: TICONINFO;
begin
  GetIconInfo(Icon, iinfo);
  SDC := CreateCompatibleDC(0);
  DDC := CreateCompatibleDC(0);
  SelectObject(DDC, iinfo.hbmColor);
  hBMP := SelectObject(SDC, iinfo.hbmMask);
  BitBlt(DDC, 0, 0, 32, 32, SDC, 0, 0, SRCPAINT);
  Bmp.handle := SelectObject(DDC, hBMP);
  DeleteDC(DDC);
  DeleteDC(SDC);
end;
}

{$IFDEF UNICODE}
function MakeIconFromPng(Png: TPngImage): HICON;
var
  IconInfo: TIconInfo;
  MaskBitmap: TBitmap;
  ImgBitmap : TBitmap;
begin
  MaskBitmap := TBitmap.Create;
  ImgBitmap := TBitmap.Create;
  try
    MaskBitmap.Width := Png.Width;
    MaskBitmap.Height := Png.Height;

    MaskBitmap.PixelFormat := pf1bit;
    MaskBitmap.Canvas.Brush.Color := clBlack;
    MaskBitmap.Canvas.FillRect(Rect(0, 0, MaskBitmap.Width, MaskBitmap.Height));

    IconInfo.fIcon := True;
    Png.AssignTo(ImgBitmap);
    IconInfo.hbmColor := ImgBitmap.Handle;
    IconInfo.hbmMask := MaskBitmap.Handle;

    Result := CreateIconIndirect(IconInfo);
  finally
    ImgBitmap.Free;
    MaskBitmap.Free;
  end;
end;
{$ENDIF}

var
  s : TResourceStream;
  it : integer;
  Ico : hIcon;
  pg : {$IFDEF UNICODE}TPngImage{$ELSE}TPNGGraphic{$ENDIF};

initialization

  acResImgList := TsAlphaImageList.Create(nil);
  acResImgList.Width := 48;
  acResImgList.Height := 16;

  for it := 0 to 4 do begin
    s := TResourceStream.Create(hInstance, acGlyphsResNames[it], RT_RCDATA);
{$IFDEF UNICODE}
    pg := TPngImage.Create;
    pg.LoadFromStream(s);
    Ico := MakeIconFromPng(pg);
{$ELSE}
    pg := TPNGGraphic.Create;
    pg.LoadFromStream(s);
    Ico := MakeIcon32(pg);
{$ENDIF}

    if Ico <> 0 then begin
      ImageList_AddIcon(acResImgList.Handle, Ico);
//      DestroyIcon(Ico); destroyed by system
    end;
    pg.Free;
    s.Free;

  end;

finalization
  FreeAndNil(acResImgList);

end.
