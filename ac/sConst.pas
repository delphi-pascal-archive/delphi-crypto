unit sConst;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses Messages, Graphics, Windows, comctrls, ExtCtrls, controls, classes, Buttons,
  StdCtrls, Consts, Dialogs;

{$R SRES.RES}

{$IFNDEF NOTFORHELP}
const
  CompatibleSkinVersion = 6.00;     // Version of supported skins in current version of the package.
  MaxCompSkinVersion = 6.29;

  ExceptTag = -98;                  // Tag value for 3rd-party controls which will not be skinned automatically

  // Data chars for skins
  TexChar   = '~';
  ZeroChar  = '0';
  CharQuest = '?';
  CharDiez  = '#';
  CharExt   = '!';
  CharGlyph = '@';
  CharMask  = '^';

  // States of control
  ACS_FAST     = 1; // Cache is notused (when control is not alphablended and when childs do not must have image of this control)
  ACS_LOCKED   = 2048; // Drawing of control is blocked

  // Background types
  BGT_NONE          = 0;
  BGT_GRADIENTHORZ  = 1;
  BGT_GRADIENTVERT  = 2;
  BGT_TEXTURE       = 4;
  BGT_TEXTURELEFT   = 16;
  BGT_TEXTURETOP    = 32;
  BGT_TEXTURERIGHT  = 64;
  BGT_TEXTUREBOTTOM = 128;
  BTG_TOPLEFT       = 256;
  BGT_TOPRIGHT      = 512;
  BGT_BOTTOMLEFT    = 1024;
  BGT_BOTTOMRIGHT   = 2048;

type
{$IFDEF UNICODE}
  ACString = String;
  ACChar   = Char;
  PACChar  = PChar;
{$ELSE} // UNICODE
{$IFDEF TNTUNICODE}
  ACString = WideString;
  ACChar   = WideChar;
  PACChar  = PWideChar;
{$ELSE}
  ACString = AnsiString;
  ACChar   = AnsiChar;
  PACChar  = PAnsiChar;
{$ENDIF}
{$ENDIF} // UNICODE
  OldChar = AnsiChar;
  POldChar = PAnsiChar;
  OldString = AnsiString;
  POldString = PAnsiString;

  PACString = ^ACString;

  TAOR = array of Windows.TRect;
  TPaintEvent = procedure (Sender: TObject; Canvas: TCanvas) of object;
  TBmpPaintEvent = procedure (Sender: TObject; Bmp: Graphics.TBitmap) of object;

  TsSkinName = string;
  TsDirectory = string;
  TsSkinSection = string;
  TacRoot = type string;
  TacStrValue = String;

  TFadeDirection = (fdNone, fdUp, fdDown);

  TacBtnEvent = (beMouseEnter, beMouseLeave, beMouseDown, beMouseUp); // Change to TacAnimatEvent in the AC v6 !!!
  TacBtnEvents = set of TacBtnEvent;

  TacCtrlType = (actGraphic);

  TacAnimatEvent = (aeMouseEnter, aeMouseLeave, aeMouseDown, aeMouseUp, aeGlobalDef);
  TacAnimatEvents = set of TacAnimatEvent;
  TacImgType = (itisaBorder, itisaTexture, itisaGlyph, itisaGlow);//, itisanOldType);
  TacFillMode = (fmTiled, fmStretched, fmTiledHorz, fmTiledVert, fmStretchHorz, fmStretchVert,
    fmTileHorBtm, fmTileVertRight, fmStretchHorBtm, fmStretchVertRight, fmDisTiled, fmDiscHorTop,
    fmDiscVertLeft, fmDiscHorBottom, fmDiscVertRight
  );

  TsCtrlClass = class of TObject;

  TsHackedControl = class(TControl)
  public
    property ParentColor;
    property Color;
    property ParentFont;
    property PopupMenu;
    property Font;
    property WindowText;
  end;

  PacBGInfo = ^TacBGInfo;
  TacBGType = (btUnknown, btFill, btCache); // Returned by control type of BG
  TacBGInfo = record
    Bmp : Graphics.TBitmap;
    Color : TColor;         // Color returned if btFill is used
    Offset : TPoint;        // Offset of bg, used with Cache
    R : TRect;              // Rectangle used if PlsDraw is True
    BgType : TacBGType;     // btUnknown, btFill, btCache
    PleaseDraw : boolean;   // Parent must fill rectangle(R) 
    DrawDC : hdc;           // Device context for drawing, if PleaseDraw is True
  end;

  TCacheInfo = record
    Bmp : Graphics.TBitmap;
    X : integer;
    Y : integer;
    FillColor : TColor; 
    Ready : boolean;
  end;
  { Pointer to @link(TPoints)}
  PPoints = ^TPoints;
  { Array of TPoint}
  TPoints = array of TPoint;

  { Set of 1..100}
  TPercent = 0..100;
  { Set of controls codes (1..255)}
  TsCodes = set of 1..255;
  // Border styles for SStyle controls
  TsControlBevel = (cbNone, cbRaisedSoft, cbLoweredSoft, cbRaisedHard, cbLoweredHard);
  { Styles of hints - (hsSimply, hsExtended, hsEllipse, hsStandard, hsNone)}
  TsHintStyle = (hsSimply, hsComics, hsEllipse, hsBalloon, hsStandard, hsNone);
  TsHintsPredefinitions = (shSimply, shGradient, shTransparent, shEllipse, shBalloon, shComicsGradient, shComicsTransparent, shStandard, shNone, shCustom);
  { Types of the gradients painting - (gtTopBottom, gtLeftRight, gtAsBorder)}
  TGradientTypes = (gtTopBottom, gtLeftRight, gtAsBorder);
  { Shapes of the shadows (ssRectangle, ssEllipse). Used in TsStyle pack}
  TsShadowingShape = (ssRectangle, ssEllipse);
  { Set of window_show types. Used in TsStyle pack}
  TsWindowShowMode = (soHide, soNormal, soShowMinimized, soMaximize, soShowNoActivate,
                  soShow, soMinimize, soShowMinNoActive, soShowNA, soRestore, soDefault);
  TsRGB = packed record
    B: Byte;
    G: Byte;
    R: Byte;
  end;

  TsRGBA = packed record
    B: Byte;
    G: Byte;
    R: Byte;
    A: Byte;
  end;

  TsColor = record
    case integer of
      0  : (C : TColor);
      1  : (R, G, B, A : Byte);
      2  : (I : integer);
      3  : (sBGR : TsRGB; T : byte);
      4  : (sBGRA : TsRGBA);
    end;

  TsColor_ = record // Bytes inverted (for fast calcs)
    case integer of
      0  : (C : TColor);
      1  : (B, G, R, A : Byte);
      2  : (I : integer);
      3  : (sBGR : TsRGB; T : byte);
      4  : (sBGRA : TsRGBA);
    end;

  PRGBArray = ^TRGBArray;
  TRGBArray = array[0..100000] of TsRGB;

  PRGBAArray = ^TRGBAArray;
  TRGBAArray = array[0..100000] of TsColor_;

  TsDisabledGlyphKind = set of (dgBlended, dgGrayed);
  TsDisabledKind = set of (dkBlended, dkGrayed);

// Scrollbars HitTest results
const
  MasterBmpName = 'Master.bmp';
  OptionsDatName = 'Options.dat';
  acSkinExt = 'asz';

  s_TrueStr = 'TRUE';
  s_FalseStr = 'FALSE';
  s_NewFolder = 'New folder';
  s_SkinSelectItemName = 'SkinSelectItem';

  // Borders draw modes
  BDM_STRETCH = 1;
  BDM_ACTIVEONLY = 2; 
  BDM_FILL = 4; 

  HTSB_LEFT_BUTTON = 100;
  HTSB_RIGHT_BUTTON = 101;
  HTSB_TOP_BUTTON = 102;
  HTSB_BOTTOM_BUTTON = 103;
  HTSB_H_SCROLL = 104;
  HTSB_HB_SCROLL = 105;
  HTSB_V_SCROLL = 106;
  HTSB_VB_SCROLL = 107;

  { WM_NCHITTEST and MOUSEHOOKSTRUCT Mouse Position Codes for MDI form}
  HTCHILDCLOSE       = 101;
  HTCHILDMAX         = 102;
  HTCHILDMIN         = 103;

  EmptyRgn = 0;

  acImgTypes : array [0..3] of TacImgType = (itisaBorder, itisaTexture, itisaGlyph, itisaGlow);
  acFillModes : array [0..14] of TacFillMode = (fmTiled, fmStretched, fmTiledHorz, fmTiledVert, fmStretchHorz, fmStretchVert,
    fmTileHorBtm, fmTileVertRight, fmStretchHorBtm, fmStretchVertRight, fmDisTiled, fmDiscHorTop,
    fmDiscVertLeft, fmDiscHorBottom, fmDiscVertRight
  );
  aScrollCodes : array [0..8] of TScrollCode = (scLineUp, scLineDown, scPageUp, scPageDown, scPosition, scTrack, scTop, scBottom, scEndScroll);
  // Array of @link(TsHintStyle)
  aHintStyles : array [0..5] of TsHintStyle = (hsSimply, hsComics, hsEllipse, hsBalloon, hsStandard, hsNone);
  acBtnEvents : array [TacAnimatEvent] of TacBtnEvent = (beMouseEnter, beMouseLeave, beMouseDown, beMouseUp, beMouseUp);

  COC_TsCustom              = 1;

  COC_TsSpinEdit            = 2;
  COC_TsEdit                = 3;
  COC_TsCustomMaskEdit      = 4;
  COC_TsMemo                = 7;
  COC_TsCustomListBox       = 8;
  COC_TsListBox             = 8;
  COC_TsColorBox            = 9;
  COC_TsListView            = 10;
  COC_TsCustomComboBox      = 11;
  COC_TsComboBox            = 13;
  COC_TsComboBoxEx          = 18;

  COC_TsFrameBar            = 19;
  COC_TsBarTitle            = 20;
  COC_TsCheckBox            = 32;
  COC_TsDBCheckBox          = 32;
  COC_TsRadioButton         = 33;

  COC_TsCurrencyEdit        = 41;

  COC_TsPanel               = 51;
  COC_TsPanelLow            = 52;
  COC_TsCoolBar             = 53;
  COC_TsToolBar             = 54;
  COC_TsDragBar             = 56;
  COC_TsTabSheet            = 57;
  COC_TsScrollBox           = 58;
  COC_TsMonthCalendar       = 59;
  COC_TsDBNavigator         = 60;
  COC_TsCustomPanel         = 68;
  COC_TsGrip                = 73;
  COC_TsGroupBox            = 74;
  COC_TsSplitter            = 75;
  // DB-aware controls
  COC_TsDBEdit              = 76;
  COC_TsDBMemo              = 78;
  COC_TsDBComboBox          = 81;
  COC_TsDBLookupComboBox    = 82;
  COC_TsDBListBox           = 83;
  COC_TsDBLookupListBox     = 84;
  // -------------- >>
  COC_TsSpeedButton         = 92;
  COC_TsButton              = 93;
  COC_TsBitBtn              = 94;
  COC_TsColorSelect         = 95;
  COC_TsTreeView            = 96;
  COC_TsAlphaListBox        = 97;
  COC_TsNavButton           = 98;
  COC_TsBevel               = 110;
  COC_TsCustomComboEdit     = 131;
  COC_TsFileDirEdit         = 132;
  COC_TsFilenameEdit        = 133;
  COC_TsDirectoryEdit       = 134;
  COC_TsCustomDateEdit      = 137;
  COC_TsComboEdit           = 138;
  COC_TsDateEdit            = 140;
  COC_TsPageControl         = 141;
  COC_TsScrollBar           = 142;
  COC_TsTabControl          = 143;
  COC_TsStatusBar           = 151;
  COC_TsHeaderControl       = 152;
  COC_TsGauge               = 161;
  COC_TsTrackBar            = 165;
  COC_TsHintManager         = 211;
  COC_TsSkinProvider        = 224;
  COC_TsMDIForm             = 225;
  COC_TsFrameAdapter        = 226;
  COC_TsAdapter             = 227;

  COC_Unknown               = 250;

  // Codes of components, who don't catch mouse events
  sForbidMouse : TsCodes = [COC_TsFrameBar, COC_TsPanel..COC_TsGroupBox, COC_TsBevel, COC_TsPageControl..COC_TsGauge];

  // Contols that can have one state only
  sCanNotBeHot : TsCodes = [COC_TsPanel, COC_TsPanelLow, COC_TsToolBar, COC_TsDragBar, COC_TsTabSheet,
                            COC_TsScrollBox, COC_TsMonthCalendar, COC_TsDBNavigator, COC_TsCustomPanel,
                            COC_TsGrip, COC_TsGroupBox, COC_TsBevel, COC_TsPageControl, COC_TsTabControl,
                            COC_TsStatusBar, COC_TsGauge, COC_TsFrameAdapter];

  sEditCtrls : TsCodes = [COC_TsSpinEdit..COC_TsComboBoxEx, COC_TsCurrencyEdit, COC_TsDBEdit..COC_TsDBLookupListBox,
                          COC_TsTreeView, COC_TsAlphaListBox, COC_TsCustomComboEdit..COC_TsDateEdit];

var
  acAeroShadows : boolean = {$IFDEF USEAERO}True{$ELSE}False{$ENDIF};
  ac_v5PaintMode : boolean = False; // Different algoritms of controls painting, if disabled then painting is faster, variable maybe used for compatibility
{$IFDEF LOGGED}
  acDebugCount : integer = 0;
{$ENDIF}

  acScrollBtnLength : integer = 16;
  AppShowHint : boolean;
  ShowHintStored : boolean = False;
  EmptyCI : TCacheInfo;
  FadingForbidden : boolean = False;
  TempControl : pointer;

  LargeShellImages, SmallShellImages : TImageList;
  StdTransparency : boolean = False; // True; // Note : Set this variable to True for standard mechanism of TGraphicControls repainting
  MouseForbidden : boolean = False;
  fGlobalFlag : boolean = False;
  acMagnForm : TWinControl;
  sFuchsia : TsColor;
  acGlobalDisabledKind : TsDisabledKind;
  acGlobalPS : TPaintStruct;
  acGlobalDC : hdc;

procedure InitShellImageLists(Large, Small : boolean);
{$ENDIF} // NOTFORHELP

type
  { Layouts for controls captions - (sclLeft, sclTopLeft, sclTopCenter, sclTopRight). Used in TsStyle pack}
  TsCaptionLayout = (sclLeft, sclTopLeft, sclTopCenter, sclTopRight);
  { Set of days of week. Used in TsStyle pack}
  TDaysOfWeek = set of TCalDayOfWeek;
  { Order of date representation - (doMDY, doDMY, doYMD). Used in TsStyle pack}
  TDateOrder = (doMDY, doDMY, doYMD);
  { Set of popup window alignes - (pwaRight, pwaLeft). Used in TsStyle pack}
  TPopupWindowAlign = (pwaRight, pwaLeft);

var
  DrawSkinnedMDIWall : boolean = True;
  DrawSkinnedMDIScrolls : boolean = True;

const
  asHintsPredefinitions : array [0..8] of TsHintsPredefinitions = (shSimply, shGradient, shTransparent, shEllipse, shBalloon, shComicsGradient, shComicsTransparent, shStandard, shNone{, shCustom});

{$IFDEF DELPHI5}
  WS_EX_LAYERED = $00080000;
  LWA_ALPHA = $00000002;
  ULW_ALPHA = $00000002;
  AC_SRC_ALPHA = $01;

  CS_DROPSHADOW = $20000;
{$ENDIF}

{$IFDEF RUNIDEONLY}
const
  sIsRUNIDEONLYMessage = 'This version of component is trial, for purchasing the fully functional version please visit www.alphaskins.com. Thanks!';
{$ENDIF}

implementation

uses SysUtils, Forms, ShellAPI;

procedure InitShellImageLists(Large, Small : boolean);
var
  sfi: TSHFileInfo;
begin
  if not Assigned(LargeShellImages) then begin
    LargeShellimages := TImageList.Create(nil);
    LargeShellImages.Handle := SHGetFileInfo('', 0, sfi, SizeOf(TSHFileInfo), SHGFI_SYSICONINDEX or SHGFI_LARGEICON or SHGFI_SHELLICONSIZE);
    LargeShellImages.ShareImages := TRUE;
    LargeShellImages.Name := 'sShellLargeImages';
  end;
  if not Assigned(SmallShellImages) then begin
    SmallShellImages := TImageList.Create(nil);
    SmallShellImages.Handle := SHGetFileInfo('', 0, sfi, SizeOf(TSHFileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
    SmallShellImages.ShareImages := TRUE;
    SmallShellImages.Name := 'sShellSmallImages';
  end;
end;

initialization
  EmptyCI.Ready := False; EmptyCI.X := -99; EmptyCI.X := -99; EmptyCI.Bmp := nil;
  sFuchsia.C := $FF00FF;
  acScrollBtnLength := GetSystemMetrics(SM_CXHSCROLL);

finalization
  if Assigned(SmallShellImages) then FreeAndNil(SmallShellImages);
  if Assigned(LargeShellImages) then FreeAndNil(LargeShellImages);

end.




