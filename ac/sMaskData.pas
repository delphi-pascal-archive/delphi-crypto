unit sMaskData;
{$I sDefs.inc}

interface

uses Windows, Graphics, sConst, sGradient, jpeg;

type

  TsMaskData = record
    Bmp          : TBitmap;
    ClassName    : string;
    PropertyName : string;
    R            : TRect;             // Rectangle of the image piece in MasterBitmap
    ImageCount   : smallint;          // Count of States, allowed for control (count of images in piece)
    MaskType     : smallint;          // Type of used mask (0 - not used, 1 - AlphaMask, 2... - reserved)
    BorderWidth  : smallint;
    DrawMode     : smallint;          // Fill type if ImgType is texture
    ImgType      : TacImgType;        // itisaBorder, itisaTexture, itisaGlyph //, itisanOldType (default)
    Manager      : TObject;

    WL           : smallint;          // Border width / Left
    WT           : smallint;          // Top
    WR           : smallint;          // Right
    WB           : smallint;          // Bottom
  end;

  TsPatternData = record
    Img          : TJPegImage;
    ClassName    : string;
    PropertyName : string;
  end;

  TsGeneralData = record
    ParentClass : string;
    ClassName : string;
    Color : TColor;
    FontColor : array [1..5] of integer;
    HotFontColor : array [1..5] of integer;
    GiveOwnFont : boolean;                 // Gives own font color for transparent child
    GlowCount : integer;
    GlowMargin : integer;

    ReservedBoolean : boolean;

    Transparency : integer;
    GradientPercent : integer;
    GradientData : string;
    GradientArray : TsGradArray;
    ImagePercent : integer;

    ShowFocus : boolean;
    //                                     // Fading properties will be common for all controls in the v6
    FadingEnabled : boolean;
    FadingIterations : integer;

    HotColor : TColor;
    HotTransparency : integer;
    HotGradientPercent : integer;
    HotGradientData : string;
    HotGradientArray : TsGradArray;
    HotImagePercent : integer;

    // Initialized values
    BorderIndex : integer;
    ImgTL : integer;
    ImgTR : integer;
    ImgBL : integer;
    ImgBR : integer;
  end;

  TsMaskArray = array of TsMaskData;
  TsPatternArray = array of TsPatternData;
  TsGeneralDataArray = array of TsGeneralData;

function WidthOfImage(const md : TsMaskData) : integer;
function HeightOfImage(const md : TsMaskData) : integer;

implementation

uses acntUtils;

function WidthOfImage(const md : TsMaskData) : integer;
begin
  Result := WidthOf(md.R) div md.ImageCount;
end;

function HeightOfImage(const md : TsMaskData) : integer;
begin
  Result := HeightOf(md.R) div (md.MaskType + 1);
end;

end.
