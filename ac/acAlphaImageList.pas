unit acAlphaImageList;
{$I sDefs.inc}

interface

uses
  Windows, Classes, SysUtils, Controls, Graphics, CommCtrl, ImgList, ComCtrls, sConst, acPNG, acntUtils;

type
  TsAlphaImageList = class;
  TsImgListItems = class;

  TsImageFormat = (ifPNG, ifICO);//, ifBMP);

  TsImgListItem = class(TCollectionItem)
  private
    FImageFormat: TsImageFormat;
    FPixelFormat: TPixelFormat;
  protected
    FOwner : TsImgListItems;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Reader: TStream);
    procedure WriteData(Writer: TStream);
  public
    ImgData : TMemoryStream;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    destructor Destroy; override;
    constructor Create(Collection: TCollection); override;
  published
    property ImageFormat : TsImageFormat read FImageFormat write FImageFormat;
    property PixelFormat : TPixelFormat read FPixelFormat write FPixelFormat default pf32bit;
  end;

  TsImgListItems = class(TCollection)
  protected
    FOwner: TsAlphaImageList;
    function GetItem(Index: Integer): TsImgListItem;
    procedure SetItem(Index: Integer; Value: TsImgListItem);
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner : TsAlphaImageList);
    destructor Destroy; override;
    property Items[Index: Integer]: TsImgListItem read GetItem write SetItem; default;
  end;

  TsAlphaImageList = class(TImageList)
  private
    FItems: TsImgListItems;
    StdListIsGenerated : boolean;
    AcChanging : boolean;
//    FShowReflections: boolean;
    procedure SetItems(const Value: TsImgListItems);
//    procedure SetShowReflections(const Value: boolean);
  protected
    procedure CreateImgList;
    procedure Change; override;
    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean = True); override;
    procedure KillImgList;
    function IsDuplicated : boolean;
    function TryLoadFromFile(const FileName : acString) : boolean;
{$IFDEF DELPHI7UP}
    procedure ReadData(Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
{$ENDIF}
    procedure ItemsClear;
  public
    DoubleData : boolean;
    procedure AcBeginUpdate;
    procedure AcEndUpdate;
    procedure AfterConstruction; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure CopyImages(const ImgList : TsAlphaImageList);
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure GenerateStdList;
    function GetBitmap32(Index: Integer; Image: TBitmap): Boolean;
    procedure Loaded; override;
    procedure LoadFromFile(const FileName : acString);
    procedure SetNewDimensions(Value: HImageList);
  published
    property Items : TsImgListItems read FItems write SetItems;
//    property ShowReflections : boolean read FShowReflections write SetShowReflections default False;
  end;

function GetImageFormat(const FileName : acString; var ImageFormat : TsImageFormat) : boolean;
function DrawAlphaImgList(const ImgList : TCustomImageList; const DestBmp : TBitmap; const Left : integer; const Top : integer;
  const ImageIndex : integer; const Blend : integer; const GrayedColor : TColor; State : integer; const NumGlyphs : integer; const Reflected : boolean) : TSize;
procedure DrawAlphaImgListDC(const ImgList : TCustomImageList; const DC : hdc; const Left : integer; const Top : integer;
  const ImageIndex : integer; const Blend : integer; const GrayedColor : TColor; const State : integer; const NumGlyphs : integer; const Reflected : boolean);
function HaveMagic(const FileName : string; const Magic: Pointer; const Size : integer): Boolean;

implementation

uses math, ShellAPI, sGraphUtils, Dialogs, Forms, sAlphaGraph;

{ TsAlphaImageList }

function GetBpp : integer;
var
  ScreenDC : hdc;
begin
  ScreenDC := GetDC(0);
  try
    Result := GetDeviceCaps(ScreenDC, BITSPIXEL);
  finally
    ReleaseDC(0, ScreenDC)
  end;
end;

function GetImageFormat(const FileName : acString; var ImageFormat : TsImageFormat) : boolean;
const
  IcoMagic: array[0..1] of Byte = (0, 0);
var
  s : string;
begin
  Result := False;
  // Check format
  if HaveMagic(FileName, @PNGMagic, 8) then begin // Png
    ImageFormat := ifPNG;
    Result := True;
  end
  else if HaveMagic(FileName, @IcoMagic, 2) then begin // Ico
    s := UpperCase(ExtractFileExt(FileName));
    System.Delete(s, 1, 1);
    if s = 'ICO' then begin
      ImageFormat := ifICO;
      Result := True;
    end;
  end;
end;

function DrawAlphaImgList(const ImgList : TCustomImageList; const DestBmp : TBitmap; const Left : integer; const Top : integer;
  const ImageIndex : integer; const Blend : integer; const GrayedColor : TColor; State : integer; const NumGlyphs : integer; const Reflected : boolean) : TSize;
var
  Bmp : TBitmap;
  w, Count : integer;
  R1, R2 : TRect;
begin
  if (DestBmp.Width = 0) or not ImgList.HandleAllocated then Exit;
//  if ImgList.Width mod ImgList.Height = 0 then Count := ImgList.Width div ImgList.Height else Count := 1;
  Count := NumGlyphs;
  w := ImgList.Width div Count;
  if State >= Count then State := Count - 1;
  R1 := Rect(Left, Top, Left + w, Top + ImgList.Height);
  R2 := Rect(0, 0, w, ImgList.Height);
  Result.cx := w;
  Result.cy := ImgList.Height;
  OffsetRect(R2, w * State, 0);

  if ImgList is TsAlphaImageList then begin
    Bmp := CreateBmp32(ImgList.Width, ImgList.Height);
    if TsAlphaImageList(ImgList).GetBitmap32(ImageIndex, Bmp) then begin
      CopyBmp32(R1, R2, DestBmp, Bmp, EmptyCI, False, GrayedColor, Blend, Reflected) // TsAlphaImageList(ImgList).ShowReflections)
    end;
  end
  else begin
    Bmp := CreateBmp24(ImgList.Width, ImgList.Height);
    if True{(Blend = 0) and (GrayedColor = clNone) not supported in standard imagelist std mode v6} then begin
      BitBlt(Bmp.Canvas.Handle, R2.Left, R2.Top, WidthOf(R2), HeightOf(R2), DestBmp.Canvas.Handle, R1.Left, R1.Top, SRCCOPY);
      ImgList.Draw(Bmp.Canvas, 0, 0, ImageIndex, True);
      BitBlt(DestBmp.Canvas.Handle, R1.Left, R1.Top, WidthOf(R2), HeightOf(R2), Bmp.Canvas.Handle, R2.Left, R2.Top, SRCCOPY);
    end
    else begin
{      if Bmp.PixelFormat = pf32bit then begin
        CopyBmp32(R1, R2, DestBmp, Bmp, EmptyCI, False, GrayedColor, Blend);
      end
      else begin
        Bmp.Transparent := True;
        Bmp.TransparentColor := clFuchsia;
        CopyTransRect(DestBmp, Bmp, R1.Left, R1.Top, R2, clFuchsia, EmptyCI, False);
      end; not supported in standard imagelist std mode v6}
    end;
  end;
  if Bmp <> nil then Bmp.Free;
end;

procedure DrawAlphaImgListDC(const ImgList : TCustomImageList; const DC : hdc; const Left : integer; const Top : integer;
  const ImageIndex : integer; const Blend : integer; const GrayedColor : TColor; const State : integer; const NumGlyphs : integer; const Reflected : boolean);
var
  Bmp : TBitmap;
  Size : TSize;
begin
  Bmp := CreateBmp24(ImgList.Width, ImgList.Height + Integer(Reflected) * ImgList.Height div 2);
  BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, DC, Left, Top, SRCCOPY);
  Size := DrawAlphaImgList(ImgList, Bmp, 0, 0, ImageIndex, Blend, GrayedColor, State, NumGlyphs, Reflected);
  BitBlt(DC, Left, Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
  Bmp.Free;
end;

function HaveMagic(const FileName : string; const Magic: Pointer; const Size : integer): Boolean;
var
  MagicBuf: array[0..7] of Byte;
  Stream: TFileStream;
  len: integer;
begin
  FillChar(MagicBuf, 8, #0);
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    len := min(Size, SizeOf(MagicBuf));
    Result := (Stream.Size - Stream.Position) > len;
    if Result then begin
      Stream.ReadBuffer(MagicBuf, len);
      Result := CompareMem(@MagicBuf, Magic, len);
    end;
  finally
    Stream.Free;
  end;
end;

procedure TsAlphaImageList.AfterConstruction;
begin
  inherited;
  if not (csLoading in ComponentState) then begin
    if not HandleAllocated then CreateImgList;
    if not StdListIsGenerated then GenerateStdList;
  end;
end;

function GetColor(Value: DWORD): TColor;
begin
  case Value of
    CLR_NONE: Result := clNone;
    CLR_DEFAULT: Result := clDefault;
  else
    Result := TColor(Value);
  end;
end;

procedure TsAlphaImageList.Assign(Source: TPersistent);
var
  ImageList: TsAlphaImageList;
begin
  if Source = nil then KillImgList else if Source is TsAlphaImageList then begin
    Clear;
    ImageList := TsAlphaImageList(Source);
    Masked := ImageList.Masked;
    ImageType := ImageList.ImageType;
    DrawingStyle := ImageList.DrawingStyle;
    ShareImages := ImageList.ShareImages;
    SetNewDimensions(ImageList.Handle);
    KillImgList;
    if not HandleAllocated then CreateImgList else ImageList_SetIconSize(Handle, Width, Height);
    BkColor := GetColor(ImageList_GetBkColor(ImageList.Handle));
    BlendColor := ImageList.BlendColor;
    CopyImages(ImageList);
  end
  else inherited Assign(Source);
end;

procedure TsAlphaImageList.AssignTo(Dest: TPersistent);
var
  ImageList: TsAlphaImageList;
begin
  if Dest is TsAlphaImageList then begin
    ImageList := TsAlphaImageList(Dest);
    ImageList.Masked := Masked;
    ImageList.ImageType := ImageType;
    ImageList.DrawingStyle := DrawingStyle;
    ImageList.ShareImages := ShareImages;
    ImageList.BlendColor := BlendColor;
    with ImageList do begin
      Clear;
      ImageList.KillImgList;
      SetNewDimensions(Self.Handle);
      if not HandleAllocated then CreateImgList else ImageList_SetIconSize(Handle, Width, Height);
      BkColor := GetColor(ImageList_GetBkColor(Self.Handle));
      ImageList.CopyImages(Self);
    end;
  end
  else inherited AssignTo(Dest);
end;

procedure TsAlphaImageList.CopyImages(const ImgList: TsAlphaImageList);
var
  i : integer;
  Ico : hIcon;
begin
  if not HandleAllocated then Exit;
  ImageList_SetBkColor(ImgList.Handle, CLR_NONE);

  if IsDuplicated then begin
    Items.Clear;
    for i := 0 to ImgList.Items.Count - 1 do begin
      with TsImgListItem(Items.Add) do begin
        ImageFormat := ImgList.Items[i].ImageFormat;
        PixelFormat := ImgList.Items[i].PixelFormat;
        ImgData.LoadFromStream(ImgList.Items[i].ImgData);
      end;
    end;
    GenerateStdList;
  end
  else begin
    Clear;
    ImageList_SetBkColor(Handle, CLR_NONE);
    for i := 0 to ImgList.Count - 1 do begin
      Ico := ImageList_GetIcon(ImgList.Handle, i, ILD_TRANSPARENT);
      ImageList_AddIcon(Handle, Ico);
      DestroyIcon(Ico);
    end;
  end;
end;

constructor TsAlphaImageList.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TsImgListItems.Create(Self);
  DoubleData := False;
end;

procedure TsAlphaImageList.CreateImgList;
begin
  Handle := ImageList_Create(Width, Height, ILC_COLOR32 or ILC_MASK, AllocBy, AllocBy);
  ImageList_SetBkColor(Handle, CLR_NONE);
end;

destructor TsAlphaImageList.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TsAlphaImageList.DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean);
var
  Ico : hIcon;
//  Bmp, TmpBmp : TBitmap;
begin
  if HandleAllocated then begin
{    if IsDuplicated then begin
      // Experimental code
      Bmp := CreateBmp32(Width, Height);
      if GetBitmap32(Index, Bmp) then begin
        TmpBmp := CreateBmp24(Width, Height);
        BitBlt(TmpBmp.Canvas.Handle, 0, 0, Width, Height, Canvas.Handle, X, Y, SRCCOPY);
        CopyBmp32(Rect(0, 0, Width, Height), Rect(0, 0, Width, Height), TmpBmp, Bmp, MakeCacheInfo(TmpBmp), False, clNone, 0);
        BitBlt(Canvas.Handle, X, Y, Width, Height, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);
        TmpBmp.Free;
      end;
      Bmp.Free;
    end
    else begin}
      ImageList_SetBkColor(Handle, CLR_NONE);
      Ico := ImageList_GetIcon(Handle, Index, ILD_TRANSPARENT);
      if (Ico > 0) then begin
        DrawIconEx(Canvas.Handle, X, Y, Ico, Width, Height, 0, 0, DI_NORMAL);
        DestroyIcon(Ico);
      end;
//    end;
  end;
end;

procedure TsAlphaImageList.GenerateStdList;
var
  i : integer;
  Png : TPNGGraphic;
  Icon : TIcon;
  Ico : hIcon;
begin
  if not HandleAllocated then Exit;
  Clear;
  for i := 0 to Items.Count - 1 do begin
    case Items[i].ImageFormat of
      ifPNG : begin
        Png := TPNGGraphic.Create;
        Png.LoadFromStream(Items[i].ImgData);
        Ico := MakeIcon32(Png);
        ImageList_AddIcon(Handle, Ico);
        DestroyIcon(Ico);
        Png.Free;
      end;
      ifICO : begin
        Icon := TIcon.Create;
        Icon.LoadFromStream(Items[i].ImgData);
        ImageList_AddIcon(Handle, Icon.Handle);
        Icon.Free;
      end;
    end;
  end;
  if Items.Count > 0 then begin
    StdListIsGenerated := True;
    if not IsDuplicated then Items.Clear;
  end;
end;

function TsAlphaImageList.GetBitmap32(Index: Integer; Image: TBitmap): Boolean;
var
  iInfo: TIconInfo;
  Ico : hIcon;
  TmpBmp : TBitmap;
//  Bmp : TBitmap;
//  Png : TPNGGraphic;
//  Icon : TIcon;
  x, y : integer;
  Color : TsColor;
  TransColor : TColor;
  S : PRGBAArray;
  C : TsColor_;
begin
  Result := False;
  if HandleAllocated and (Image <> nil) and (Index >= 0) and (Index < Count) then begin
    TmpBmp := CreateBmp32(Image.Width, Image.Height);

{    if IsDuplicated and (Index < Items.Count) then begin
      // Experimental code
      case Items[Index].ImageFormat of
        ifPNG : if (Items[Index].ImgData.Size > 0) then begin
          Png := TPNGGraphic.Create;
          Items[Index].ImgData.Seek(0, 0);
          Png.LoadFromStream(Items[Index].ImgData);
          Image.Assign(Png);
          Png.Free;
        end;
        ifICO : if (Items[Index].ImgData.Size > 0) then begin
          Icon := TIcon.Create;
          Ico := ImageList_GetIcon(Handle, Index, ILD_NORMAL);
          Icon.Handle := Ico;
          GetIconInfo(Ico, iInfo);
          TmpBmp.Handle := iInfo.hbmColor;
          TmpBmp.HandleType := bmDIB;

          Image.Assign(TmpBmp);

          DeleteObject(iInfo.hbmColor);
          DeleteObject(iInfo.hbmMask);
          DestroyIcon(Ico);
          Icon.Free;
        end;
      end;
    end
    else begin}
      Ico := ImageList_GetIcon(Handle, Index, ILD_NORMAL);
      GetIconInfo(Ico, iInfo);
      TmpBmp.Handle := iInfo.hbmColor;
      TmpBmp.HandleType := bmDIB;
      TmpBmp.PixelFormat := pf32bit;

      if (Win32MajorVersion < 6) and (GetBpp < 32) {and (Items.Count > Index) and (Items[Index].ImageFormat = ifICO) }then begin // Update alpha channel
        TransColor := clFuchsia;
        for Y := 0 to TmpBmp.Height - 1 do begin // Check if alphachannel if fully clear
          S := tmpBmp.ScanLine[Y];
          for X := 0 to TmpBmp.Width - 1 do begin
            C := S[X];
            if C.A <> 0 then begin
              TransColor := clNone;
              Break;
            end;
          end;
          if TransColor = clNone then Break;
        end;
        if TransColor = clFuchsia then begin
          TransColor := TmpBmp.Canvas.Pixels[0, TmpBmp.Height - 1];
          if Fast32Src.Attach(TmpBmp) then begin
            for X := 0 to TmpBmp.Width - 1 do for Y := 0 to TmpBmp.Height - 1 do begin
              Color := Fast32Src[X, Y];
              if Color.C <> TransColor then begin
                Color.A := 255;
                Fast32Src[X, Y] := Color;
              end;
            end;
          end;
        end;
      end;

      Image.Assign(TmpBmp);

      DeleteObject(iInfo.hbmColor);
      DeleteObject(iInfo.hbmMask);
      DestroyIcon(Ico);
//    end;

    TmpBmp.Free;
    Result := True;
  end;
end;

function TsAlphaImageList.IsDuplicated: boolean;
begin
  Result := DoubleData or (csDesigning in ComponentState);// or (Win32MajorVersion < 6) and (GetBpp < 32);
end;

procedure TsAlphaImageList.KillImgList;
begin
  if HandleAllocated and not ShareImages then ImageList_Destroy(Handle);
  Handle := 0;
  Change;
end;

procedure TsAlphaImageList.Loaded;
begin
  inherited;
  if not HandleAllocated then CreateImgList;
  if not StdListIsGenerated then GenerateStdList;
end;

procedure TsAlphaImageList.LoadFromFile(const FileName: acString);
begin
  if not TryLoadFromfile(FileName) then MessageDlg('Cannot load ' + FileName + #13#10 + 'Invalid or unexpected image format.', mtError, [mbOk], 0);
end;

{$IFDEF DELPHI7UP}
procedure TsAlphaImageList.ReadData(Stream: TStream);
begin
// All data is in Items
end;

procedure TsAlphaImageList.WriteData(Stream: TStream);
begin
// All data is in Items
end;
{$ENDIF}

procedure TsAlphaImageList.SetItems(const Value: TsImgListItems);
begin
  FItems.Assign(Value);
end;

procedure TsAlphaImageList.SetNewDimensions(Value: HImageList);
var
  AHeight, AWidth: Integer;
begin
  AWidth := Width;
  AHeight := Height;
  ImageList_GetIconSize(Value, AWidth, AHeight);
  Width := AWidth;
  Height := AHeight;
end;

function TsAlphaImageList.TryLoadFromfile(const FileName: acString) : boolean;
var
  Ico: HICON;
  iInfo : TIconInfo;
  Png : TPNGGraphic;
  iFormat : TsImageFormat;
  Bmp : TBitmap;
  X, Y : integer;
  BmpLine : PRGBAArray;
begin
  Result := False;
  Ico := 0;
  if not HandleAllocated or not GetImageFormat(FileName, iFormat) then Exit;

  if IsDuplicated then begin // If double data used
    with TsImgListItem(Items.Add) do begin
      ImgData.LoadFromFile(FileName);
      ImageFormat := iFormat;
      case iFormat of
        ifPNG : begin
          PixelFormat := pf32bit;
          Png := TPNGGraphic.Create;
          Png.LoadFromStream(ImgData);
          Ico := MakeIcon32(Png);
          Png.Free;
        end;
        ifICO : begin
{$IFDEF TNTUNICODE}
          Ico := ExtractIconW(hInstance, PacChar(FileName), 0);
{$ELSE}
          Ico := ExtractIcon(hInstance, PacChar(FileName), 0);
{$ENDIF}
          GetIconInfo(Ico, iInfo);
          Bmp := TBitmap.Create;
          Bmp.Handle := iInfo.hbmColor;
          Bmp.HandleType := bmDIB;
          PixelFormat := pf24bit;

          if (Bmp.PixelFormat = pf32bit) then begin // Check the alpha channel
            for Y := 0 to Bmp.Height - 1 do begin
              BmpLine := Bmp.ScanLine[Y];
              for X := 0 to Bmp.Width - 1 do begin
                if BmpLine[X].A <> 0 then begin
                  PixelFormat := pf32bit;
                  Break;
                end;
              end;
              if PixelFormat = pf32bit then Break;
            end;
          end;

          Bmp.Free;
          DeleteObject(iInfo.hbmColor);
          DeleteObject(iInfo.hbmMask);
        end;
      end;
    end;
  end
  else begin
    case iFormat of
      ifPNG : begin
        Png := TPNGGraphic.Create;
        Png.LoadFromFile(FileName);
        Ico := MakeIcon32(Png);
        Png.Free;
      end;
      ifICO : begin
{$IFDEF TNTUNICODE}
        Ico := ExtractIconW(hInstance, PacChar(FileName), 0);
{$ELSE}
        Ico := ExtractIcon(hInstance, PacChar(FileName), 0);
{$ENDIF}
      end;
    end;
  end;

  if Ico <> 0 then begin
    Result := ImageList_AddIcon(Handle, Ico) > -1;
    DestroyIcon(Ico);
  end;
//  if not IsDuplicated then
  Change;
end;

procedure TsAlphaImageList.ItemsClear;
var
  i : integer;
begin
  for i := 0 to Items.Count - 1 do Items[i].ImgData.Clear;
end;

procedure TsAlphaImageList.Change;
var
  Ico, NewIco: HICON;
  iInfo : TIconInfo;
  Bmp : TBitmap;
  X, Y : integer;
  BmpLine : PRGBAArray;
  i, c, h, w : integer;
  b : boolean;
begin
  if AcChanging then Exit;
  inherited;
  if HandleAllocated and not (csLoading in ComponentState) and StdListIsGenerated {(UpdateCount < 1) }then begin
    if not (csDesigning in ComponentState) then begin
      if IsDuplicated then Exit;
      c := ImageList_GetImageCount(Handle) - 1;
      if c > -1 then begin
        Bmp := TBitmap.Create;

        for i := 0 to c do begin
          Ico := ImageList_GetIcon(Handle, i, ILD_NORMAL);
          GetIconInfo(Ico, iInfo);
          Bmp.Handle := iInfo.hbmColor;
          Bmp.HandleType := bmDIB;
          b := False;
          h := Bmp.Height - 1;
          w := Bmp.Width - 1;
          Bmp.PixelFormat := pf32bit;
          for Y := 0 to h do begin // Check if AlphaChannel is empty
            BmpLine := Bmp.ScanLine[Y];
            for X := 0 to w do begin
              if BmpLine[X].A <> 0 then begin
                b := True;
                Break;
              end;
            end;
            if b then Break;
          end;
          if not b then begin
            for Y := 0 to h do begin
              BmpLine := Bmp.ScanLine[Y];
              for X := 0 to w do if BmpLine[X].C <> sFuchsia.C then BmpLine[X].A := 255;
            end;
            iInfo.hbmColor := Bmp.Handle;
            NewIco := CreateIconIndirect(iInfo);
            DeleteObject(Ico);
            ImageList_ReplaceIcon(Handle, i, NewIco);
          end;

          DeleteObject(iInfo.hbmColor);
          DeleteObject(iInfo.hbmMask);
        end;

        Bmp.Free;
      end;
    end;
  end;
end;

procedure TsAlphaImageList.AcBeginUpdate;
begin
  AcChanging := True;
end;

procedure TsAlphaImageList.AcEndUpdate;
begin
  AcChanging := False;
  Change;
end;
{
procedure TsAlphaImageList.SetShowReflections(const Value: boolean);
begin
  if FShowReflections <> Value then begin
    FShowReflections := Value;
    Change;
  end;
end;
}
{ TsImgListItems }

constructor TsImgListItems.Create(AOwner: TsAlphaImageList);
begin
  inherited Create(TsImgListItem);
  FOwner := AOwner;
end;

destructor TsImgListItems.Destroy;
begin
  inherited Destroy;
  FOwner := nil;
end;

function TsImgListItems.GetItem(Index: Integer): TsImgListItem;
begin
  Result := TsImgListItem(inherited GetItem(Index));
end;

function TsImgListItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TsImgListItems.SetItem(Index: Integer; Value: TsImgListItem);
begin
  inherited SetItem(Index, Value);
end;

{ TsImgListItem }

procedure TsImgListItem.Assign(Source: TPersistent);
begin
  if Source <> nil then begin
    ImageFormat := TsImgListItem(Source).ImageFormat;
    PixelFormat := TsImgListItem(Source).PixelFormat;
    ImgData.LoadFromStream(TsImgListItem(Source).ImgData);
  end
  else inherited;
end;

procedure TsImgListItem.AssignTo(Dest: TPersistent);
begin
  if Dest <> nil then begin
    TsImgListItem(Dest).ImageFormat := ImageFormat;
    TsImgListItem(Dest).PixelFormat := PixelFormat;
    TsImgListItem(Dest).ImgData.LoadFromStream(ImgData);
  end
  else inherited;
end;

constructor TsImgListItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FOwner := TsImgListItems(Collection);
  ImgData := TMemoryStream.Create;
  FPixelFormat := pf32bit;
end;

procedure TsImgListItem.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('ImgData', ReadData, WriteData, True);
end;

destructor TsImgListItem.Destroy;
begin
  FreeAndNil(ImgData);
  inherited Destroy;
end;

procedure TsImgListItem.ReadData(Reader: TStream);
begin
  ImgData.LoadFromStream(Reader);
end;

procedure TsImgListItem.WriteData(Writer: TStream);
begin
  ImgData.SaveToStream(Writer);
end;

end.
