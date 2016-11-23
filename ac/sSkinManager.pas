unit sSkinManager;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, sDefaults,
  sConst, IniFiles, sMaskData, sSkinMenus, jpeg, sStyleSimply, acSkinPack, menus, acntUtils
  {$IFDEF LOGGED}, sDebugMsgs{$ENDIF};

{$IFNDEF NOTFORHELP}
const
  CurrentVersion = '6.21';

var
  NonAutoUpdate : boolean = False;
{$ENDIF} // NOTFORHELP

type
  TacSkinTypes = (stUnpacked, stPacked, stAllSkins);

  TacGetExtraLineData = procedure
    (FirstItem: TMenuItem; var SkinSection : string; var Caption : string; var Glyph : TBitmap; var LineVisible : boolean) of object;

{$IFNDEF NOTFORHELP}
  TsSkinManager = class;
  TsStoredSkin = class;
  TacSkinInfo = type string;

  TacBtnEffects = class(TPersistent)
  private
    FEvents: TacBtnEvents;
//    procedure SetEvents(const Value: TacBtnEvents);
//    function GetEvents: TacBtnEvents;
  public
    Manager : TsSkinManager;
    constructor Create;
  published
    property Events : TacBtnEvents read FEvents write FEvents default [beMouseEnter, beMouseLeave, beMouseDown, beMouseUp];
  end;

  TacFormAnimation = class(TPersistent)
  private
    FTime: word;
    FActive: boolean;
  public
    constructor Create; virtual;
  published
    property Active : boolean read FActive write FActive default True;
    property Time : word read FTime write FTime default 0;
  end;

  TacFormShow = class(TacFormAnimation);
  TacPageChange = class(TacFormAnimation);

  TacDialogShow = class(TacFormAnimation)
  public
    constructor Create; override;
  published
    property Time default 0;
  end;

  TacSkinChanging = class(TacFormAnimation)
  public
    constructor Create; override;
  published
    property Time default 100;
  end;

  TacAnimEffects = class(TPersistent)
  private
    FButtons: TacBtnEffects;
    FDialogShow: TacDialogShow;
    FFormShow: TacFormShow;
    FSkinChanging: TacSkinChanging;
    FPageChange: TacPageChange;
  public
    Manager : TsSkinManager;
    constructor Create;
    destructor Destroy; override;
  published
    property Buttons : TacBtnEffects read FButtons write FButtons;
    property DialogShow : TacDialogShow read FDialogShow write FDialogShow;
    property FormShow : TacFormShow read FFormShow write FFormShow;
    property PageChange : TacPageChange read FPageChange write FPageChange;
    property SkinChanging : TacSkinChanging read FSkinChanging write FSkinChanging;
  end;

  TsSkinGeneral = class(TCollectionItem)
  private
    FName: string;
    FShowFocus: boolean;
    FFadingEnabled: boolean;
    FHotImagePercent: integer;
    FTransparency: integer;
    FHotTransparency: integer;
    FFadingIterations: integer;
    FHotFontColor: string;
    FHotGradientPercent: integer;
    FGradientPercent: integer;
    FImagePercent: integer;
    FHotGradientData: string;
    FGradientData: string;
    FParentClass: string;
    FHotColor: TColor;
    FColor: TColor;
    FSectionName: string;
    FFontColor: string;
    FReservedBoolean: boolean;
    FGiveOwnFont : boolean;
    FGlowCount: integer;
    FGlowMargin: integer;
    procedure SetName(const Value: string);
    procedure SetFadingEnabled(const Value: boolean);
    procedure SetFadingIterations(const Value: integer);
    procedure SetGradientData(const Value: string);
    procedure SetGradientPercent(const Value: integer);
    procedure SetHotGradientData(const Value: string);
    procedure SetHotGradientPercent(const Value: integer);
    procedure SetHotImagePercent(const Value: integer);
    procedure SetHotColor(const Value: TColor);
    procedure SetHotTransparency(const Value: integer);
    procedure SetImagePercent(const Value: integer);
    procedure SetColor(const Value: TColor);
    procedure SetTransparency(const Value: integer);
    procedure SetParentClass(const Value: string);
    procedure SetShowFocus(const Value: boolean);
    procedure SetSectionName(const Value: string);
    procedure SetHotFontColor(const Value: string);
    procedure SetFontColor(const Value: string);
    procedure SetReservedBoolean(const Value: boolean);
    procedure SetGiveOwnFont(const Value: boolean);
    procedure SetGlowCount(const Value: integer);
    procedure SetGlowMargin(const Value: integer);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property SectionName : string read FSectionName write SetSectionName;
    property ParentClass : string read FParentClass write SetParentClass;
    property Color : TColor read FColor write SetColor;
    property ReservedBoolean : boolean read FReservedBoolean write SetReservedBoolean;
    property FontColor : string read FFontColor write SetFontColor;
    property HotFontColor : string read FHotFontColor write SetHotFontColor;
    property Transparency : integer read FTransparency write SetTransparency;
    property GlowCount : integer read FGlowCount write SetGlowCount;
    property GlowMargin : integer read FGlowMargin write SetGlowMargin;
    property GradientPercent : integer read FGradientPercent write SetGradientPercent;
    property GradientData : string read FGradientData write SetGradientData;
    property ImagePercent : integer read FImagePercent write SetImagePercent;
    property ShowFocus : boolean read FShowFocus write SetShowFocus;
    property FadingEnabled : boolean read FFadingEnabled write SetFadingEnabled;
    property FadingIterations : integer read FFadingIterations write SetFadingIterations;
    property HotColor : TColor read FHotColor write SetHotColor;
    property HotTransparency : integer read FHotTransparency write SetHotTransparency;
    property HotGradientPercent : integer read FHotGradientPercent write SetHotGradientPercent;
    property HotGradientData : string read FHotGradientData write SetHotGradientData;
    property HotImagePercent : integer read FHotImagePercent write SetHotImagePercent;
    property Name : string read FName write SetName;
    property GiveOwnFont : boolean read FGiveOwnFont write SetGiveOwnFont;
  end;

  TsSkinGenerals = class(TCollection)
  private
    FOwner: TsStoredSkin;
    function GetItem(Index: Integer): TsSkinGeneral;
    procedure SetItem(Index: Integer; Value: TsSkinGeneral);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TsStoredSkin);
    destructor Destroy; override;
    property Items[Index: Integer]: TsSkinGeneral read GetItem write SetItem; default;
  end;

  TsSkinImage = class(TCollectionItem)
  private
    FName: string;
    FImage: TBitmap;
    FClassName: string;
    FPropertyName: string;
    FMaskType: integer;
    FImageCount: integer;
    FTop: integer;
    FLeft: integer;
    FRight: integer;
    FBottom: integer;
    FBorderWidth: integer;
    FStretchMode: integer;
    FImgType: TacImgType;
    FWR: integer;
    FWT: integer;
    FWL: integer;
    FWB: integer;
    procedure SetName(const Value: string);
    procedure SetImage(const Value: TBitmap);
    procedure SetClassName(const Value: string);
    procedure SetPropertyName(const Value: string);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property SectionName : string read FClassName write SetClassName;
    property Image : TBitmap read FImage write SetImage;
    property Name : string read FName write SetName;
    property PropertyName : string read FPropertyName write SetPropertyName;
    property Left : integer read FLeft write FLeft;
    property Top : integer read FTop write FTop;
    property Right : integer read FRight write FRight;
    property Bottom : integer read FBottom write FBottom;
    property ImageCount : integer read FImageCount write FImageCount;
    property MaskType : integer read FMaskType write FMaskType;
    property BorderWidth : integer read FBorderWidth write FBorderWidth;
    property StretchMode : integer read FStretchMode write FStretchMode;
    property ImgType : TacImgType read FImgType write FImgType;
    property WL : integer read FWL write FWL;
    property WR : integer read FWR write FWR;
    property WT : integer read FWT write FWT;
    property WB : integer read FWB write FWB;
  end;

  TsSkinPattern = class(TCollectionItem)
  private
    FName: string;
    FImage: TJpegImage;
    FClassName: string;
    FPropertyName: string;
    procedure SetName(const Value: string);
    procedure SetImage(const Value: TJpegImage);
    procedure SetClassName(const Value: string);
    procedure SetPropertyName(const Value: string);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property SectionName : string read FClassName write SetClassName;
    property Image : TJpegImage read FImage write SetImage;
    property Name : string read FName write SetName;
    property PropertyName : string read FPropertyName write SetPropertyName;
  end;

  TsSkinImages = class(TCollection)
  private
    FOwner: TsStoredSkin;
    function GetItem(Index: Integer): TsSkinImage;
    procedure SetItem(Index: Integer; Value: TsSkinImage);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TsStoredSkin);
    destructor Destroy; override;
    property Items[Index: Integer]: TsSkinImage read GetItem write SetItem; default;
  end;

  TsSkinPatterns = class(TCollection)
  private
    FOwner: TsStoredSkin;
    function GetItem(Index: Integer): TsSkinPattern;
    procedure SetItem(Index: Integer; Value: TsSkinPattern);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TsStoredSkin);
    destructor Destroy; override;
    property Items[Index: Integer]: TsSkinPattern read GetItem write SetItem; default;
  end;

  TsStoredSkin = class(TCollectionItem)
  private
    FImages: TsSkinImages;
    FName: string;
    FGeneralData: TsSkinGenerals;
    FPatterns: TsSkinPatterns;
    FMasterBitmap: TBitmap;
    FVersion: real;
    FDescription: string;
    FAuthor: string;
    FShadow1Offset: integer;
    FShadow1Color: TColor;
    FShadow1Blur: integer;
    FShadow1Transparency: integer;
    FBorderColor: TColor;
    procedure SetImages(const Value: TsSkinImages);
    procedure SetName(const Value: string);
    procedure SetGeneralData(const Value: TsSkinGenerals);
    procedure SetPatterns(const Value: TsSkinPatterns);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Reader: TStream);
    procedure WriteData(Writer: TStream);
  public
    PackedData: TMemoryStream;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure LoadSkin(sf : TMemIniFile);
    procedure LoadFromIni(gd : TsSkinGenerals; sf: TMemIniFile);
  published
    property Name : string read FName write SetName;
    property GeneralData : TsSkinGenerals read FGeneralData write SetGeneralData;
    property Images : TsSkinImages read FImages write SetImages;
    property Patterns : TsSkinPatterns read FPatterns write SetPatterns;
    property MasterBitmap : TBitmap read FMasterBitmap write FMasterBitmap;

    property Shadow1Color : TColor read FShadow1Color write FShadow1Color;
    property Shadow1Offset : integer read FShadow1Offset write FShadow1Offset;
    property Shadow1Blur : integer read FShadow1Blur write FShadow1Blur default -1;
    property Shadow1Transparency : integer read FShadow1Transparency write FShadow1Transparency;

    property BorderColor : TColor read FBorderColor write FBorderColor default clFuchsia;

    property Version : real read FVersion write FVersion;
    property Author : string read FAuthor write FAuthor;
    property Description : string read FDescription write FDescription;
  end;

  TsStoredSkins = class(TCollection)
  private
    FOwner: TsSkinManager;
    function GetItem(Index: Integer): TsStoredSkin;
    procedure SetItem(Index: Integer; Value: TsStoredSkin);
  protected
    function GetOwner: TPersistent; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TsSkinManager);
    destructor Destroy; override;
    property Items[Index: Integer]: TsStoredSkin read GetItem write SetItem; default;
    function IndexOf(const SkinName : string) : integer;
  end;
{$ENDIF} // NOTFORHELP

  ThirdPartyList = class(TPersistent)
  private
    FThirdEdits        : string;
    FThirdButtons      : string;
    FThirdBitBtns      : string;
    FThirdCheckBoxes   : string;
    FThirdGroupBoxes   : string;
    FThirdListViews    : string;
    FThirdPanels       : string;
    FThirdGrids        : string;
    FThirdTreeViews    : string;
    FThirdComboBoxes   : string;
    FThirdWWEdits      : string;
    FThirdVirtualTrees : string;
    FThirdGridEh       : string;
    FThirdPageControl  : string;
    FThirdTabControl   : string;
    FThirdToolBar      : string;
    FThirdStatusBar    : string;
    function GetString(const Index: Integer): string;
    procedure SetString(const Index: Integer; const Value: string);
  published
    property ThirdEdits        : string index ord(tpEdit       ) read GetString write SetString stored True;
    property ThirdButtons      : string index ord(tpButton     ) read GetString write SetString stored True;
    property ThirdBitBtns      : string index ord(tpBitBtn     ) read GetString write SetString stored True;
    property ThirdCheckBoxes   : string index ord(tpCheckBox   ) read GetString write SetString stored True;
    property ThirdGroupBoxes   : string index ord(tpGroupBox   ) read GetString write SetString stored True;
    property ThirdListViews    : string index ord(tpListView   ) read GetString write SetString stored True;
    property ThirdPanels       : string index ord(tpPanel      ) read GetString write SetString stored True;
    property ThirdGrids        : string index ord(tpGrid       ) read GetString write SetString stored True;
    property ThirdTreeViews    : string index ord(tpTreeView   ) read GetString write SetString stored True;
    property ThirdComboBoxes   : string index ord(tpComboBox   ) read GetString write SetString stored True;
    property ThirdWWEdits      : string index ord(tpWWEdit     ) read GetString write SetString stored True;
    property ThirdVirtualTrees : string index ord(tpVirtualTree) read GetString write SetString stored True;
    property ThirdGridEh       : string index ord(tpGridEh     ) read GetString write SetString stored True;
    property ThirdPageControl  : string index ord(tpPageControl) read GetString write SetString stored True;
    property ThirdTabControl   : string index ord(tpTabControl ) read GetString write SetString stored True;
    property ThirdToolBar      : string index ord(tpToolBar    ) read GetString write SetString stored True;
    property ThirdStatusBar    : string index ord(tpStatusBar  ) read GetString write SetString stored True;
  end;

  TacSkinningRule = (srStdForms, srStdDialogs, srThirdParty);
  TacSkinningRules = set of TacSkinningRule;

  TsSkinManager = class(TComponent)
  private
{$IFNDEF NOTFORHELP}
    FGroupIndex: integer;
    FSkinName: TsSkinName;
    FSkinDirectory: TsDirectory;
    FActive: boolean;
    FBuiltInSkins: TsStoredSkins;
    FSkinableMenus: TsSkinableMenus;
    FOnAfterChange: TNotifyEvent;
    FOnBeforeChange: TNotifyEvent;
    FSkinnedPopups: boolean;
    FCommonSections: TStringList;
    FIsDefault: boolean;
    FOnGetPopupLineData: TacGetExtraLineData;
    FMenuSupport: TacMenuSupport;
    FAnimEffects: TacAnimEffects;
    FActiveControl: hwnd;
    GlobalHookInstalled : boolean;
    FSkinningRules: TacSkinningRules;
    FThirdParty: ThirdPartyList;
    FAllowGlowing: boolean;
    procedure SetSkinName(const Value: TsSkinName);
    procedure SetSkinDirectory(const Value: string);
    procedure SetActive(const Value: boolean);
    procedure SetBuiltInSkins(const Value: TsStoredSkins);
    procedure SetSkinnedPopups(const Value: boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetSkinInfo: TacSkinInfo;
    procedure SetSkinInfo(const Value: TacSkinInfo);
    procedure SetHueOffset(const Value: integer);
    procedure SetSaturation(const Value: integer);
    procedure SetIsDefault(const Value: boolean);
    function GetIsDefault: boolean;
    function MainWindowHook(var Message: TMessage): boolean;
    procedure SetActiveControl(const Value: hwnd);
    procedure SetFSkinningRules(const Value: TacSkinningRules);
  protected
    procedure SendNewSkin(AllowAnimation : boolean = True);
    procedure SendRemoveSkin;

    procedure LoadAllMasks;
    procedure LoadAllPatterns;
    procedure FreeBitmaps;
    procedure FreeJpegs;
{$ENDIF} // NOTFORHELP
  public

    SkinData : TsSkinData;
{$IFNDEF NOTFORHELP}
    ma : TsMaskArray;
    pa : TsPatternArray;
    gd : TsGeneralDataArray;
    ConstData : TConstantSkinData;
    MasterBitmap : TBitmap;
    SkinIsPacked : boolean;

    FHueOffset: integer;
    FSaturation: integer;
    ThirdLists : array of TStringList;

    procedure InitConstantIndexes;

    procedure LoadAllGeneralData;
    procedure InitMaskIndexes;
    procedure SetCommonSections(const Value: TStringList);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Loaded; override;
    procedure SaveToIni(Index : integer; sf : TMemIniFile);
    procedure ReloadSkin;
    procedure ReloadPackedSkin;
    procedure InstallHook;
    procedure UnInstallHook;
    procedure CheckVersion;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateSkinSection(const SectionName : string);
    property GroupIndex : integer read FGroupIndex write FGroupIndex;
    property SkinableMenus : TsSkinableMenus read FSkinableMenus write FSkinableMenus;
    property ActiveControl : hwnd read FActiveControl write SetActiveControl;
    procedure RepaintForms;
    function MaskSize(MaskIndex : integer) : TSize; {$IFDEF WARN_DEPRECATED} deprecated; {$ENDIF}
    function GetSkinIndex(const SkinSection : string) : integer;
    function GetMaskIndex(SkinIndex : integer; const SkinSection, mask : string) : integer; overload;
    function GetMaskIndex(const SkinSection, mask : string) : integer; overload;
    function GetTextureIndex(SkinIndex : integer; const SkinSection, PropName : string) : integer;
    function GetPatternIndex(SkinIndex : integer; const SkinSection, pattern : string) : integer;
{$ENDIF} // NOTFORHELP

    function GetFullSkinDirectory : string;
    function GetSkinNames(sl: TacStrings; SkinType : TacSkinTypes = stAllSkins) : acString;
    function GetExternalSkinNames(sl: TacStrings; SkinType : TacSkinTypes = stAllSkins) : acString;
    procedure GetSkinSections(sl: TStrings);
    procedure ExtractInternalSkin(const NameOfSkin, DestDir : string);
    procedure ExtractByIndex(Index : integer; const DestDir : string);
    procedure UpdateSkin;

    function GetGlobalColor : TColor;
    function GetGlobalFontColor : TColor;
    function GetActiveEditColor : TColor;
    function GetActiveEditFontColor : TColor;
    function GetHighLightColor : TColor;
    function GetHighLightFontColor : TColor;

{$IFNDEF NOTFORHELP}
    function MaskWidthTop(MaskIndex : integer) : integer;
    function MaskWidthLeft(MaskIndex : integer) : integer;
    function MaskWidthBottom(MaskIndex : integer) : integer;
    function MaskWidthRight(MaskIndex : integer) : integer;

    function IsValidImgIndex(ImageIndex : integer) : boolean;
    function IsValidSkinIndex(SkinIndex : integer) : boolean;
{$ENDIF} // NOTFORHELP
  published
    property SkinnedPopups : boolean read FSkinnedPopups write SetSkinnedPopups default True;
    property AllowGlowing : boolean read FAllowGlowing write FAllowGlowing default True;
    property AnimEffects : TacAnimEffects read FAnimEffects write FAnimEffects;
    property IsDefault : boolean read GetIsDefault write SetIsDefault default True;
    property Active : boolean read FActive write SetActive default True;
    property CommonSections : TStringList read FCommonSections write SetCommonSections;
    property Saturation : integer read FSaturation write SetSaturation default 0;
    property HueOffset : integer read FHueOffset write SetHueOffset default 0;
    property InternalSkins : TsStoredSkins read FBuiltInSkins write SetBuiltInSkins;
    property MenuSupport : TacMenuSupport read FMenuSupport write FMenuSupport;
    property SkinDirectory : TsDirectory read FSkinDirectory write SetSkinDirectory;
    property SkinName : TsSkinName read FSkinName write SetSkinName;
    property SkinInfo : TacSkinInfo read GetSkinInfo write SetSkinInfo;
    property SkinningRules : TacSkinningRules read FSkinningRules write SetFSkinningRules default [srStdForms, srStdDialogs, srThirdParty];
    property ThirdParty : ThirdPartyList read FThirdParty write FThirdParty;
    property Version : string read GetVersion write SetVersion stored False;
    property OnAfterChange : TNotifyEvent read FOnAfterChange write FOnAfterChange;
    property OnBeforeChange : TNotifyEvent read FOnBeforeChange write FOnBeforeChange;
    property OnGetMenuExtraLineData : TacGetExtraLineData read FOnGetPopupLineData write FOnGetPopupLineData;
  end;

{$IFNDEF NOTFORHELP}
var
  DefaultManager : TsSkinManager;
  SkinFile : TMemIniFile;
  OSVersionInfo: TOSVersionInfo;
  IsNT : boolean;
  sc : TacSkinConvertor;
  UnPackedFirst : boolean = False;

procedure UpdateCommonDlgs(sManager : TsSkinManager);
function ChangeImageInSkin(const SkinSection, PropName, FileName : string; sm : TsSkinManager) : boolean;

procedure ChangeSkinSaturation(sManager : TsSkinManager; Value : integer);
procedure ChangeSkinHue(sManager : TsSkinManager; Value : integer);
procedure ChangeSkinBrightness(sManager : TsSkinManager; Value : integer);

procedure LoadThirdNames(sm : TsSkinManager; Overwrite : boolean = False);
procedure UpdateThirdNames(sm : TsSkinManager);
{$ENDIF} // NOTFORHELP

implementation

uses sMessages, sStoreUtils, sVclUtils, sCommonData, acPNG, acGlow, sThirdParty,
  sSkinProps, acDials, FileCtrl, sGraphUtils, sGradient, sSkinProvider, math;

procedure UpdateCommonDlgs(sManager : TsSkinManager);
begin
{$IFDEF D2007}
  if (DefaultManager = sManager) then UseLatestCommonDialogs := not (srStdDialogs in sManager.SkinningRules) or not sManager.Active;
{$ENDIF}
end;

function ChangeImageInSkin(const SkinSection, PropName, FileName : string; sm : TsSkinManager) : boolean;
var
  i, l : integer;
  s : string;
begin
  with sm do begin

  Result := False;
  if not SkinData.Active then Exit;
  if (SkinSection = '') or (PropName='') or not FileExists(FileName) then Exit;

  s := UpperCase(PropName);
  // If property is Background texture
  if (s = s_Pattern) or ( s = s_HotPattern) then begin
    // If loaded file is Bitmap
    if pos('.BMP', UpperCase(FileName)) > 0 then begin
      l := Length(ma);
      // ma - is array of records with image description
      if l > 0 then begin
        // search of the required image in the massive
        for i := 0 to l - 1 do begin
          if (UpperCase(ma[i].PropertyName) = s) and (UpperCase(ma[i].ClassName) = UpperCase(skinSection))  then begin
            // If found then we must define new Bmp
            if ma[i].Bmp = nil then ma[i].Bmp := TBitmap.Create;
            ma[i].Bmp.LoadFromFile(FileName);
            // To exit
            Result := True;
            Break;
          end;
        end;
      end;

      // If not found we must to add new image
      if not Result then begin
        l := Length(ma) + 1;
        SetLength(ma, l);
        ma[l - 1].PropertyName := '';
        ma[l - 1].ClassName := '';
        try
          ma[l - 1].Bmp := TBitmap.Create;
          ma[l - 1].Bmp.LoadFromFile(FileName);
        finally
          ma[l - 1].PropertyName := s;
          ma[l - 1].ClassName := UpperCase(skinSection);
          ma[l - 1].Manager := sm;
          ma[l - 1].R := Rect(0, 0, ma[l - 1].Bmp.Width, ma[l - 1].Bmp.Height);
          ma[l - 1].ImageCount := 1;
          ma[l - 1].ImgType := itisaTexture;
        end;
        if ma[l - 1].Bmp.Width < 1 then begin
          FreeAndNil(ma[l - 1].Bmp);
          SetLength(ma, l - 1);
        end;

        l := Length(pa);
        if l > 0 then for i := 0 to l - 1 do if (pa[i].PropertyName = s) and (pa[i].ClassName = UpperCase(skinSection)) then begin
          FreeAndNil(pa[i].Img);

          l := Length(pa) - 1;
          if l <> i then begin
            pa[i].Img          := pa[l].Img         ;
            pa[i].ClassName    := pa[l].ClassName   ;
            pa[i].PropertyName := pa[l].PropertyName;
          end;
          SetLength(pa, l);
          Break;
        end;
        Result := True;
      end;
    end
    // If loaded image is Jpeg, then working with massive of JPegs
    else begin
      l := Length(pa);
      if l > 0 then for i := 0 to l - 1 do if (pa[i].PropertyName = s) and (pa[i].ClassName = UpperCase(skinSection)) then begin
        if not Assigned(pa[i].Img) then pa[i].Img := TJpegImage.Create;
        pa[i].Img.LoadFromFile(FileName);
        Result := True;
        Break;
      end;
      if not Result then begin
        l := Length(pa) + 1;
        SetLength(pa, l);
        try
          pa[l - 1].Img := TJpegImage.Create;
          pa[l - 1].Img.LoadFromFile(FileName);
        finally
          pa[l - 1].PropertyName := s;
          pa[l - 1].ClassName := UpperCase(SkinSection);
        end;
        if pa[l - 1].Img.Width < 1 then begin
          FreeAndNil(pa[l - 1].Img);
          SetLength(pa, l - 1);
        end;
        l := Length(ma);
        if l > 0 then begin
          for i := 0 to l - 1 do begin
            if (ma[i].PropertyName = s) and (ma[i].ClassName = UpperCase(skinSection))  then begin
              FreeAndNil(ma[i].Bmp);

              l := Length(ma) - 1;
              if l <> i then begin
                ma[i].Bmp          := ma[l].Bmp         ;
                ma[i].BorderWidth  := ma[l].BorderWidth ;
                ma[i].ClassName    := ma[l].ClassName   ;
                ma[i].DrawMode     := ma[l].DrawMode    ;
                ma[i].ImageCount   := ma[l].ImageCount  ;
                ma[i].Manager      := ma[l].Manager     ;
                ma[i].MaskType     := ma[l].MaskType    ;
                ma[i].PropertyName := ma[l].PropertyName;
                ma[i].R            := ma[l].R           ;
                ma[i].WT           := ma[l].WT          ;
                ma[i].WL           := ma[l].WL          ;
                ma[i].WR           := ma[l].WR          ;
                ma[i].WB           := ma[l].WB          ;
              end;
              SetLength(ma, l);
              Break;
            end;
          end;
        end;
      end;
    end;
  end
  // If property is not background texture
  else begin
    if pos('.BMP', FileName) > 0 then begin
      l := Length(ma);
      if l > 0 then for i := 0 to l - 1 do if (ma[i].PropertyName = s) and (ma[i].ClassName = UpperCase(skinSection)) then begin
        ma[i].Bmp.LoadFromFile(FileName);
        Result := True;
        Exit
      end;
    end;
  end;

  end;
end;

procedure ChangeSkinSaturation(sManager : TsSkinManager; Value : integer);
var
  S1 : PRGBArray;
  SA : PRGBAArray;
  i, l, j, w, h, x, y : integer;
  Col : TsColor;
begin
  if Value = 0 then Exit;
  Value := - Value mod 101;
  with sManager do begin

    if Assigned(MasterBitmap) then begin
      h := MasterBitmap.Height - 1;
      w := MasterBitmap.Width - 1;
      for y := 0 to h do begin
        S1 := MasterBitmap.ScanLine[y];
        for x := 0 to w do with S1[X] do begin
          Col.R := R; Col.G := G; Col.B := B;
          if (Col.C = clFuchsia) or ((Col.R = Col.G) and (Col.R = Col.B)) then Continue;
          Col.C := ChangeSaturation(Col.C, Value);
          R := Col.R; G := Col.G; B := Col.B
        end
      end;
    end;

    l := Length(ma);
    for i := 0 to l - 1 do if Assigned(ma[i].Bmp) then begin
      if ma[i].Bmp.PixelFormat = pf32bit then begin
        h := ma[i].Bmp.Height - 1;
        w := ma[i].Bmp.Width - 1;
        for y := 0 to h do begin
          SA := ma[i].Bmp.ScanLine[y];
          for x := 0 to w do with SA[X] do begin
            Col.R := R; Col.G := G; Col.B := B;
            if (Col.C = clFuchsia) or ((Col.R = Col.G) and (Col.R = Col.B)) then Continue;
            Col.C := ChangeSaturation(Col.C, Value);
            R := Col.R; G := Col.G; B := Col.B
          end
        end;
      end
      else begin
        h := ma[i].Bmp.Height - 1;
        w := ma[i].Bmp.Width - 1;
        ma[i].Bmp.PixelFormat := pf24bit;
        for y := 0 to h do begin
          S1 := ma[i].Bmp.ScanLine[y];
          for x := 0 to w do with S1[X] do begin
            Col.R := R; Col.G := G; Col.B := B;
            if (Col.C = clFuchsia) or ((Col.R = Col.G) and (Col.R = Col.B)) then Continue;
            Col.C := ChangeSaturation(Col.C, Value);
            R := Col.R; G := Col.G; B := Col.B
          end
        end;
      end
    end;

    l := Length(gd);
    for i := 0 to l - 1 do begin

      gd[i].Color := ChangeSaturation(gd[i].Color, Value);
      gd[i].HotColor := ChangeSaturation(gd[i].HotColor, Value);

      if gd[i].FontColor[1] <> -1 then gd[i].FontColor[1] := ChangeSaturation(gd[i].FontColor[1], Value);
      if gd[i].FontColor[2] <> -1 then gd[i].FontColor[2] := ChangeSaturation(gd[i].FontColor[2], Value);
      if gd[i].FontColor[3] <> -1 then gd[i].FontColor[3] := ChangeSaturation(gd[i].FontColor[3], Value);
      if gd[i].FontColor[4] <> -1 then gd[i].FontColor[4] := ChangeSaturation(gd[i].FontColor[4], Value);
      if gd[i].FontColor[5] <> -1 then gd[i].FontColor[5] := ChangeSaturation(gd[i].FontColor[5], Value);

      if gd[i].HotFontColor[1] <> -1 then gd[i].HotFontColor[1] := ChangeSaturation(gd[i].HotFontColor[1], Value);
      if gd[i].HotFontColor[2] <> -1 then gd[i].HotFontColor[2] := ChangeSaturation(gd[i].HotFontColor[2], Value);
      if gd[i].HotFontColor[3] <> -1 then gd[i].HotFontColor[3] := ChangeSaturation(gd[i].HotFontColor[3], Value);
      if gd[i].HotFontColor[4] <> -1 then gd[i].HotFontColor[4] := ChangeSaturation(gd[i].HotFontColor[4], Value);
      if gd[i].HotFontColor[5] <> -1 then gd[i].HotFontColor[5] := ChangeSaturation(gd[i].HotFontColor[5], Value);

      w := WordCount(gd[i].GradientData, [';']) div 5;
      for j := 0 to w - 1 do begin
        gd[i].GradientArray[j].Color1 := ChangeSaturation(gd[i].GradientArray[j].Color1, Value);
        gd[i].GradientArray[j].Color2 := ChangeSaturation(gd[i].GradientArray[j].Color2, Value);
      end;

      w := Length(gd[i].HotGradientArray);
      for j := 0 to w - 1 do begin
        gd[i].HotGradientArray[j].Color1 := ChangeSaturation(gd[i].HotGradientArray[j].Color1, Value);
        gd[i].HotGradientArray[j].Color2 := ChangeSaturation(gd[i].HotGradientArray[j].Color2, Value);
      end;
    end;
    sManager.SkinData.Shadow1Color := ChangeSaturation(sManager.SkinData.Shadow1Color, Value);
    sManager.SkinData.BorderColor := ChangeSaturation(sManager.SkinData.BorderColor, Value);
  end;
end;

procedure ChangeSkinBrightness(sManager : TsSkinManager; Value : integer);
var
  S1 : PRGBArray;
  i, l, j, w, h, x, y : integer;
  C : TsColor;
begin
  if Value = 0 then Exit;
  with sManager do begin

    if Assigned(MasterBitmap) then begin
      h := MasterBitmap.Height - 1;
      w := MasterBitmap.Width - 1;
      for y := 0 to h do begin
        S1 := MasterBitmap.ScanLine[y];
        for x := 0 to w do with S1[X] do begin
          C.R := R; C.G := G; C.B := B;
          if (C.C = clFuchsia) then Continue;
          C.C := ChangeBrightness(C.C, Value);
          R := C.R; G := C.G; B := C.B
        end
      end;
    end;

    l := Length(ma);
    for i := 0 to l - 1 do if Assigned(ma[i].Bmp) then begin
      h := ma[i].Bmp.Height - 1;
      w := ma[i].Bmp.Width - 1;
      ma[i].Bmp.PixelFormat := pf24bit;
      for y := 0 to h do begin
        S1 := ma[i].Bmp.ScanLine[y];
        for x := 0 to w do with S1[X] do begin
          C.R := R; C.G := G; C.B := B;
          if (C.C = clFuchsia) then Continue;
          C.C := ChangeBrightness(C.C, Value);
          R := C.R; G := C.G; B := C.B
        end
      end;
    end;

    l := Length(gd);
    for i := 0 to l - 1 do begin
      gd[i].Color := ChangeBrightness(gd[i].Color, Value);
      gd[i].HotColor := ChangeBrightness(gd[i].HotColor, Value);

      for j := 1 to 5 do begin
        if gd[i].FontColor[j] <> -1 then gd[i].FontColor[j] := ChangeBrightness(gd[i].FontColor[j], Value);
        if gd[i].HotFontColor[j] <> -1 then gd[i].HotFontColor[j] := ChangeBrightness(gd[i].HotFontColor[j], Value);
      end;

      w := WordCount(gd[i].GradientData, [';']) div 5;
      for j := 0 to w - 1 do begin
        gd[i].GradientArray[j].Color1 := ChangeBrightness(gd[i].GradientArray[j].Color1, Value);
        gd[i].GradientArray[j].Color2 := ChangeBrightness(gd[i].GradientArray[j].Color2, Value);
      end;

      w := Length(gd[i].HotGradientArray);
      for j := 0 to w - 1 do begin
        gd[i].HotGradientArray[j].Color1 := ChangeBrightness(gd[i].HotGradientArray[j].Color1, Value);
        gd[i].HotGradientArray[j].Color2 := ChangeBrightness(gd[i].HotGradientArray[j].Color2, Value);
      end;
    end;
  end;
end;

procedure ChangeSkinHue(sManager : TsSkinManager; Value : integer);
var
  S1 : PRGBArray;
  SA : PRGBAArray;
  i, l, j, w, h, x, y : integer;
  Col : TsColor;
begin
  if Value = 0 then Exit;
  with sManager do begin

    if Assigned(MasterBitmap) then begin
      h := MasterBitmap.Height - 1;
      w := MasterBitmap.Width - 1;
      for y := 0 to h do begin
        S1 := MasterBitmap.ScanLine[y];
        for x := 0 to w do with S1[X] do begin
          Col.R := R; Col.G := G; Col.B := B;
          if (Col.C = clFuchsia) or ((Col.R = Col.G) and (Col.R = Col.B)) then Continue;
          Col.C := ChangeHue(Value, Col.C);
          R := Col.R; G := Col.G; B := Col.B
        end
      end;
    end;

    l := Length(ma);
    for i := 0 to l - 1 do if Assigned(ma[i].Bmp) then begin
      if ma[i].Bmp.PixelFormat = pf32bit then begin
        h := ma[i].Bmp.Height - 1;
        w := ma[i].Bmp.Width - 1;
        for y := 0 to h do begin
          SA := ma[i].Bmp.ScanLine[y];
          for x := 0 to w do with SA[X] do begin
            Col.R := R; Col.G := G; Col.B := B;
            if (Col.C = clFuchsia) or ((Col.R = Col.G) and (Col.R = Col.B)) then Continue;
            Col.C := ChangeHue(Value, Col.C);
            R := Col.R; G := Col.G; B := Col.B
          end
        end;
      end
      else begin
        h := ma[i].Bmp.Height - 1;
        w := ma[i].Bmp.Width - 1;
        ma[i].Bmp.PixelFormat := pf24bit;
        for y := 0 to h do begin
          S1 := ma[i].Bmp.ScanLine[y];
          for x := 0 to w do with S1[X] do begin
            Col.R := R; Col.G := G; Col.B := B;
            if (Col.C = clFuchsia) or ((Col.R = Col.G) and (Col.R = Col.B)) then Continue;
            Col.C := ChangeHue(Value, Col.C);
            R := Col.R; G := Col.G; B := Col.B
          end
        end;
      end;
    end;

    l := Length(gd);
    for i := 0 to l - 1 do begin
      gd[i].Color := ChangeHue(Value, gd[i].Color);
      gd[i].HotColor := ChangeHue(Value, gd[i].HotColor);

      for j := 1 to 5 do begin
        if gd[i].FontColor[j] <> -1 then gd[i].FontColor[j] := ChangeHue(Value, gd[i].FontColor[j]);
        if gd[i].HotFontColor[j] <> -1 then gd[i].HotFontColor[j] := ChangeHue(Value, gd[i].HotFontColor[j]);
      end;

      w := WordCount(gd[i].GradientData, [';']) div 5;
      for j := 0 to w - 1 do begin
        gd[i].GradientArray[j].Color1 := ChangeHue(Value, gd[i].GradientArray[j].Color1);
        gd[i].GradientArray[j].Color2 := ChangeHue(Value, gd[i].GradientArray[j].Color2);
      end;

      w := Length(gd[i].HotGradientArray);
      for j := 0 to w - 1 do begin
        gd[i].HotGradientArray[j].Color1 := ChangeHue(Value, gd[i].HotGradientArray[j].Color1);
        gd[i].HotGradientArray[j].Color2 := ChangeHue(Value, gd[i].HotGradientArray[j].Color2);
      end;
    end;

    sManager.SkinData.Shadow1Color := ChangeHue(Value, sManager.SkinData.Shadow1Color);
    sManager.SkinData.BorderColor := ChangeHue(Value, sManager.SkinData.BorderColor);
  end;
end;

procedure LoadThirdNames(sm : TsSkinManager; Overwrite : boolean = False);
var
  i : integer;
begin
  for i := 0 to High(acThirdNames) do begin
    if Overwrite or (sm.ThirdParty.GetString(i) = '') then sm.ThirdParty.SetString(i, acThirdNames[i]);
    sm.ThirdLists[i].Text := sm.ThirdParty.GetString(i);
  end;
end;

procedure UpdateThirdNames(sm : TsSkinManager);
var
  i : integer;
begin
  for i := 0 to High(acThirdNames) do sm.ThirdParty.SetString(i, sm.ThirdLists[i].Text);
end;

{ TsSkinManager }

procedure TsSkinManager.AfterConstruction;
begin
  inherited;
  LoadThirdNames(Self);
  if FSkinDirectory = '' then begin
    FSkinDirectory := DefSkinsDir;
  end;
  if not (csLoading in ComponentState) and not (csReading in ComponentState) then InitDevEx(Active and (SkinName <> ''));
end;

constructor TsSkinManager.Create(AOwner: TComponent);
var
  i, l : integer;
begin
  inherited Create(AOwner);
  FThirdParty := ThirdPartyList.Create;

  if (DefaultManager = nil) then FIsDefault := True;

  l := High(acThirdNames);
  SetLength(ThirdLists, l + 1);
  for i := 0 to l do ThirdLists[i] := TStringList.Create;

  SkinData := TsSkinData.Create;
  SkinData.Active := False;
  FBuiltInSkins := TsStoredSkins.Create(Self);
  FCommonSections := TStringList.Create;
  FSkinnedPopups := True;
  FHueOffset := 0;
  FAllowGlowing := True;
  FMenuSupport := TacMenuSupport.Create;
  FAnimEffects := TacAnimEffects.Create;
  FAnimEffects.Manager := Self;
  FAnimEffects.Buttons.Manager := Self;
  GlobalHookInstalled := False;
  FSkinningRules := [srStdForms, srStdDialogs, srThirdParty];
  if (DefaultManager = nil) then begin
    DefaultManager := Self;
    if IsNT and not (csDesigning in ComponentState) then Application.HookMainWindow(MainWindowHook);
  end;
  FActive := True;
  FSkinableMenus := TsSkinableMenus.Create(Self);
  SetLength(gd, 0);
  SetLength(ma, 0);
  SetLength(pa, 0);
end;

destructor TsSkinManager.Destroy;
var
  i : integer;
begin
  Active := False;
  FreeAndNil(FAnimEffects);
  if Assigned(FBuiltInSkins) then FreeAndNil(FBuiltInSkins);
  if Assigned(FSkinableMenus) then FreeAndNil(FSkinableMenus);
  FreeAndNil(FCommonSections);
  if Assigned(SkinData) then SkinData.Free;
  FreeAndNil(FMenuSupport);
  FreeJpegs;
  FreeBitmaps;
  if (DefaultManager = Self) then begin
    if IsNT and not (csDesigning in ComponentState) then Application.UnHookMainWindow(MainWindowHook);
    DefaultManager := nil;
  end;

  UpdateThirdNames(Self);

  for i := 0 to Length(ThirdLists) - 1 do if ThirdLists[i] <> nil then FreeAndNil(ThirdLists[i]);
  SetLength(ThirdLists, 0);

  FreeAndNil(FThirdParty);

  inherited Destroy;
end;

procedure TsSkinManager.ExtractByIndex(Index: integer; const DestDir: string);
var
  DirName : string;
begin
  DirName := NormalDir(DestDir);
  if not DirectoryExists(DirName) then begin
    if not CreateDir(DirName) then begin
{$IFNDEF ALITE}
      ShowError('Directory ' + DirName + ' creation error.');
{$ENDIF}
      Exit;
    end;
  end;
  if InternalSkins[Index].PackedData <> nil then InternalSkins[Index].PackedData.SaveToFile(DirName + InternalSkins[Index].Name + ' extracted.asz');
end;

procedure TsSkinManager.ExtractInternalSkin(const NameOfSkin, DestDir: string);
var
  i : integer;
  Executed : boolean;
begin
  Executed := False;
  for i := 0 to InternalSkins.Count - 1 do begin
    if InternalSkins[i].Name = NameOfskin then begin
      if DirectoryExists(Destdir) then begin
        ExtractByIndex(i, Destdir);
{$IFNDEF ALITE}
      end
      else begin
        ShowError('Directory with such name do not exists.');
{$ENDIF}
      end;
      Executed := True;
    end;
  end;
  if not Executed then begin
{$IFNDEF ALITE}
    ShowError('Skin with such name do not exists.');
{$ENDIF}
  end;
end;

function TsSkinManager.GetExternalSkinNames(sl: TacStrings; SkinType : TacSkinTypes = stAllSkins): acString;
var
  FileInfo: TacSearchRec;
  DosCode: Integer;
  s : acString;
  SkinPath : acString;
  stl : TacStringList;
begin
  Result := '';
  SkinPath := GetFullskinDirectory;
  sl.Clear;
  stl := TacStringList.Create;

  // External skins names loading
  if DirectoryExists(SkinPath) then begin
    s := SkinPath + '\*.*';
    DosCode := acFindFirst(s, faDirectory, FileInfo);
    try
      while DosCode = 0 do begin
        if (FileInfo.Name[1] <> '.') then begin
          if (SkinType in [stUnpacked, stAllSkins]) and (FileInfo.Attr and faDirectory = faDirectory) and FileExists(SkinPath + '\' + FileInfo.Name + '\' + OptionsDatName) then begin
            stl.Add(FileInfo.Name);
            if Result = '' then Result := FileInfo.Name;
          end
          else if (SkinType in [stPacked, stAllSkins]) and (FileInfo.Attr and faDirectory <> faDirectory) and (ExtractFileExt(FileInfo.Name) = '.' + acSkinExt) then begin
            s := ExtractWord(1, FileInfo.Name, ['.']);
            stl.Add(s);
            if Result = '' then Result := s;
          end;
        end;
        DosCode := acFindNext(FileInfo);
      end;
    finally
      acFindClose(FileInfo);
    end;
  end;
  stl.Sort;
  sl.Assign(stl);
  FreeAndNil(stl);
end;

function TsSkinManager.GetFullSkinDirectory: string;
var
  s : string;
begin
  Result := SkinDirectory;
  if (pos('..', Result) = 1) then begin
    s := GetAppPath;
    Delete(s, Length(s), 1);
    while (s[Length(s)] <> '/') and (s[Length(s)] <> '\') do begin
      Delete(s, Length(s), 1);
    end;
    Delete(Result, 1, 3);
    Result := s + Result;
  end
  else if (pos('.\', Result) = 1) or (pos('./', Result) = 1) then begin
    Delete(Result, 1, 2);
    Result := GetAppPath + Result;
  end
  else if (pos(':', Result) < 1) and (pos('\\', Result) < 1) then begin
    Result := GetAppPath + Result;
  end;
  NormalDir(Result);
end;

function TsSkinManager.GetGlobalColor: TColor;
begin
  if (ConstData.IndexGlobalInfo > -1) and (ConstData.IndexGlobalInfo <= Length(gd) - 1) then Result := ColorToRGB(gd[ConstData.IndexGlobalInfo].Color) else Result := ColorToRGB(clBtnFace);
end;

function TsSkinManager.GetGlobalFontColor: TColor;
begin
  if (ConstData.IndexGlobalInfo > -1) and (ConstData.IndexGlobalInfo <= Length(gd) - 1) then Result := ColorToRGB(gd[ConstData.IndexGlobalInfo].FontColor[1]) else Result := clFuchsia;
end;

function TsSkinManager.GetSkinNames(sl: TacStrings; SkinType : TacSkinTypes = stAllSkins) : acString;
var
  FileInfo: TacSearchRec;
  DosCode: Integer;
  s : acString;
  SkinPath : acString;
  stl : TacStringList;
begin
  Result := '';
  SkinPath := GetFullskinDirectory;
  sl.Clear;
  stl := TacStringList.Create;

  // Internal skins names loading
  if InternalSkins.Count > 0 then begin
    for DosCode := 0 to InternalSkins.Count - 1 do begin
      stl.Add(InternalSkins[DosCode].Name);
      if Result = '' then Result := InternalSkins[DosCode].Name;
    end;
  end;

  // External skins names loading
  if DirectoryExists(SkinPath) then begin
    s := SkinPath + '\*.*';
    DosCode := acFindFirst(s, faDirectory, FileInfo);
    try
      while DosCode = 0 do begin
        if (FileInfo.Name[1] <> '.') then begin
          if (SkinType in [stUnpacked, stAllSkins]) and (FileInfo.Attr and faDirectory = faDirectory) and FileExists(SkinPath + '\' + FileInfo.Name + '\' + OptionsDatName) then begin
            stl.Add(FileInfo.Name);
            if Result = '' then Result := FileInfo.Name;
          end
          else if (SkinType in [stPacked, stAllSkins]) and (FileInfo.Attr and faDirectory <> faDirectory) and (ExtractFileExt(FileInfo.Name) = '.' + acSkinExt) then begin
            s := ExtractWord(1, FileInfo.Name, ['.']);
            stl.Add(s);
            if Result = '' then Result := s;
          end;
        end;
        DosCode := acFindNext(FileInfo);
      end;
    finally
      acFindClose(FileInfo);
    end;
  end;
  stl.Sort;
  sl.Assign(stl);
  FreeAndNil(stl);
end;

procedure TsSkinManager.GetSkinSections(sl: TStrings);
var
  i : integer;
begin
  sl.Clear;
  if SkinData.Active
    then for i := Low(gd) to High(gd) do sl.Add(gd[i].ClassName);
end;

function TsSkinManager.GetSkinInfo: TacSkinInfo;
var
  s : char;
begin
  if SkinData.Active then begin
    s := DecimalSeparator;
    DecimalSeparator := '.';
    Result := FloatToStr(SkinData.Version);
    DecimalSeparator := s;
  end
  else Result := 'N/A';
end;

function TsSkinManager.GetVersion: string;
begin
  Result := CurrentVersion {$IFDEF RUNIDEONLY} + ' Trial'{$ENDIF};
end;

procedure TsSkinManager.InitConstantIndexes;
begin
  with ConstData do begin
    IndexGlobalInfo := GetSkinIndex(s_GlobalInfo);
    if IndexGlobalInfo > -1 then begin
      // Global data
      CheckBoxChecked := GetMaskIndex(IndexGlobalInfo, s_GlobalInfo, s_CheckBoxChecked);
      CheckBoxUnChecked := GetMaskIndex(IndexGlobalInfo, s_GlobalInfo, s_CheckBoxUnChecked);
      CheckBoxGrayed := GetMaskIndex(IndexGlobalInfo, s_GlobalInfo, s_CheckBoxGrayed);
      RadioButtonChecked := GetMaskIndex(IndexGlobalInfo, s_GlobalInfo, s_RadioButtonChecked);
      RadioButtonUnChecked := GetMaskIndex(IndexGlobalInfo, s_GlobalInfo, s_RadioButtonUnChecked);
      RadioButtonGrayed := GetMaskIndex(IndexGlobalInfo, s_GlobalInfo, s_RadioButtonGrayed);

      SmallCheckBoxChecked := GetMaskIndex(IndexGlobalInfo, s_GlobalInfo, s_SmallBoxChecked);
      SmallCheckBoxUnChecked := GetMaskIndex(IndexGlobalInfo, s_GlobalInfo, s_SmallBoxUnChecked);
      SmallCheckBoxGrayed := GetMaskIndex(IndexGlobalInfo, s_GlobalInfo, s_SmallBoxGrayed);
    end
    else begin
      CheckBoxChecked        := -1;
      CheckBoxUnChecked      := -1;
      CheckBoxGrayed         := -1;
      RadioButtonChecked     := -1;
      RadioButtonUnChecked   := -1;
      RadioButtonGrayed      := -1;

      SmallCheckBoxChecked   := -1;
      SmallCheckBoxUnChecked := -1;
      SmallCheckBoxGrayed    := -1;
    end;

    // ComboBox
    ComboBtnIndex := GetSkinIndex(s_ComboBtn);
    ComboBtnBorder := GetMaskIndex(s_ComboBtn, s_BordersMask);
    ComboBtnBG := GetTextureIndex(ComboBtnIndex, s_ComboBtn, s_Pattern);
    ComboBtnBGHot := GetTextureIndex(ComboBtnIndex, s_ComboBtn, s_HotPattern);
    ComboGlyph := GetMaskIndex(s_ComboBox, s_ItemGlyph);

    // Tabs
    IndexTabTop := GetSkinIndex(s_TABTOP);
    IndexTabBottom := GetSkinIndex(s_TABBOTTOM);
    IndexTabLeft := GetSkinIndex(s_TABLEFT);
    IndexTabRight := GetSkinIndex(s_TABRIGHT);
    MaskTabTop := GetMaskIndex(IndexTabTop, s_TABTOP, s_BordersMask);
    MaskTabBottom := GetMaskIndex(IndexTabTop, s_TABBOTTOM, s_BordersMask);
    MaskTabLeft := GetMaskIndex(IndexTabTop, s_TABLEFT, s_BordersMask);
    MaskTabRight := GetMaskIndex(IndexTabTop, s_TABRIGHT, s_BordersMask);

    IndexScrollTop := GetSkinIndex(s_SCROLLBTNTOP);
    IndexScrollBottom := GetSkinIndex(s_SCROLLBTNBOTTOM);
    IndexScrollLeft := GetSkinIndex(s_SCROLLBTNLEFT);
    IndexScrollRight := GetSkinIndex(s_SCROLLBTNRIGHT);
    IndexSliderVert := GetSkinIndex(s_SCROLLSLIDERV);
    IndexSliderHorz := GetSkinIndex(s_SCROLLSLIDERH) ;

    MaskScrollTop := GetMaskIndex(IndexScrollTop, s_SCROLLBTNTOP, s_BordersMask);
    if IndexScrollTop > -1 then MaskArrowTop := GetMaskIndex(IndexScrollTop, s_ScrollBtntop, s_ItemGlyph);

    MaskScrollBottom := GetMaskIndex(IndexScrollBottom, s_SCROLLBTNBOTTOM, s_BordersMask);
    if IndexScrollBottom > -1 then MaskArrowBottom := GetMaskIndex(IndexScrollBottom, s_ScrollBtnBottom, s_ItemGlyph);

    MaskScrollLeft := GetMaskIndex(IndexScrollLeft, s_SCROLLBTNLEFT, s_BordersMask);
    if IndexScrollLeft > -1 then MaskArrowLeft := GetMaskIndex(IndexScrollLeft, s_ScrollBtnLeft, s_ItemGlyph);

    MaskScrollRight := GetMaskIndex(IndexScrollRight, s_SCROLLBTNRIGHT, s_BordersMask);
    if IndexScrollRight > -1 then MaskArrowRight := GetMaskIndex(IndexScrollRight, s_ScrollBtnRight, s_ItemGlyph);

    MaskSliderVert := GetMaskIndex(IndexSliderVert, s_SCROLLSLIDERV, s_BordersMask);
    if IndexSLiderVert > -1 then MaskSliderGlyphVert := GetMaskIndex(IndexSLiderVert, s_ScrollSLiderV, s_ItemGlyph);

    MaskSliderHorz := GetMaskIndex(IndexSliderHorz, s_SCROLLSLIDERH, s_BordersMask);
    if IndexSLiderHorz > -1 then MaskSliderGlyphHorz := GetMaskIndex(IndexSLiderHorz, s_ScrollSLiderH, s_ItemGlyph);
    
    IndexBGScrollTop := GetMaskIndex(IndexScrollTop, s_ScrollBtnTop, s_Pattern);
    IndexBGHotScrollTop := GetMaskIndex(IndexScrollTop, s_ScrollBtnTop, s_HotPattern);
    IndexBGScrollBottom := GetMaskIndex(IndexScrollBottom, s_ScrollBtnBottom, s_Pattern);
    IndexBGHotScrollBottom := GetMaskIndex(IndexScrollBottom, s_ScrollBtnBottom, s_HotPattern);
    IndexBGScrollLeft := GetMaskIndex(IndexScrollLeft, s_ScrollBtnLeft, s_Pattern);
    IndexBGHotScrollLeft := GetMaskIndex(IndexScrollLeft, s_ScrollBtnLeft, s_HotPattern);
    IndexBGScrollRight := GetMaskIndex(IndexScrollRight, s_ScrollBtnRight, s_Pattern);
    IndexBGHotScrollRight := GetMaskIndex(IndexScrollRight, s_ScrollBtnRight, s_HotPattern);

    ScrollSliderBGHorz := GetMaskIndex(IndexSLiderHorz, s_ScrollSLiderH, s_Pattern);
    ScrollSliderBGHotHorz := GetMaskIndex(IndexSLiderHorz, s_ScrollSLiderH, s_HotPattern);
    ScrollSliderBGVert := GetMaskIndex(IndexSLiderVert, s_ScrollSLiderV, s_Pattern);
    ScrollSliderBGHotVert := GetMaskIndex(IndexSLiderVert, s_ScrollSLiderV, s_HotPattern);

    //ScrollBars
    IndexScrollBar1H := GetSkinIndex(s_ScrollBar1H);
    IndexScrollBar1V := GetSkinIndex(s_ScrollBar1V);
    IndexScrollBar2H := GetSkinIndex(s_ScrollBar2H);
    IndexScrollBar2V := GetSkinIndex(s_ScrollBar2V);
    MaskScrollBar1H := GetMaskIndex(IndexScrollBar1H, s_ScrollBar1H, s_BordersMask);
    MaskScrollBar1V := GetMaskIndex(IndexScrollBar1V, s_ScrollBar1V, s_BordersMask);
    MaskScrollBar2H := GetMaskIndex(IndexScrollBar2H, s_ScrollBar2H, s_BordersMask);
    MaskScrollBar2V := GetMaskIndex(IndexScrollBar2V, s_ScrollBar2V, s_BordersMask);
    BGScrollBar1H := GetMaskIndex(IndexScrollBar1H, s_ScrollBar1H, s_Pattern);
    BGScrollBar1V := GetMaskIndex(IndexScrollBar1V, s_ScrollBar1V, s_Pattern);
    BGScrollBar2H := GetMaskIndex(IndexScrollBar2H, s_ScrollBar2H, s_Pattern);
    BGScrollBar2V := GetMaskIndex(IndexScrollBar2V, s_ScrollBar2V, s_Pattern);

    BGHotScrollBar1H := GetMaskIndex(IndexScrollBar1H, s_ScrollBar1H, s_HotPattern);
    BGHotScrollBar1V := GetMaskIndex(IndexScrollBar1V, s_ScrollBar1V, s_HotPattern);
    BGHotScrollBar2H := GetMaskIndex(IndexScrollBar2H, s_ScrollBar2H, s_HotPattern);
    BGHotScrollBar2V := GetMaskIndex(IndexScrollBar2V, s_ScrollBar2V, s_HotPattern);
  end;
end;

procedure TsSkinManager.InitMaskIndexes;
var
  i : integer;
begin
  for i := 0 to Length(gd) - 1 do if i <> ConstData.IndexGlobalInfo then begin
    gd[i].BorderIndex := GetMaskIndex(i, gd[i].ClassName, s_BordersMask);
    gd[i].ImgTL := GetMaskIndex(i, gd[i].ClassName, s_ImgTopLeft);
    gd[i].ImgTR := GetMaskIndex(i, gd[i].ClassName, s_ImgTopRight);
    gd[i].ImgBL := GetMaskIndex(i, gd[i].ClassName, s_ImgBottomLeft);
    gd[i].ImgBR := GetMaskIndex(i, gd[i].ClassName, s_ImgBottomRight);
  end;
end;

procedure TsSkinManager.Loaded;
begin
  inherited;
  if FSkinDirectory = '' then FSkinDirectory := DefSkinsDir;
  if FMenuSupport.IcoLineSkin = '' then FMenuSupport.IcoLineSkin := s_MenuIcoLine;
  LoadThirdNames(Self);
  if Active and (SkinName <> '') then begin
    SendNewSkin(False);
  end;
  UpdateCommonDlgs(Self);
  if not (csLoading in ComponentState) and not (csReading in ComponentState) then InitDevEx(Active and (SkinName <> ''));
end;

procedure TsSkinManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation); 
end;

procedure TsSkinManager.SaveToIni(Index: integer; sf: TMemIniFile);
var
  gd : TsSkinGeneral;
  i : integer;
  s, SectionName : string;
begin
  for i := 0 to InternalSkins[Index].GeneralData.Count - 1 do begin
    gd := InternalSkins[Index].GeneralData.Items[i];
    if gd.SectionName = '' then Continue;
    SectionName := gd.SectionName;

    WriteIniStr(SectionName, s_ParentClass, gd.ParentClass, sf);

    WriteIniStr(SectionName, s_Color, IntToStr(gd.Color), sf);
    WriteIniStr(SectionName, s_ReservedBoolean, iff(gd.ReservedBoolean, s_TrueStr, s_FalseStr), sf);
    WriteIniStr(SectionName, s_GiveOwnFont, iff(gd.GiveOwnFont, s_TrueStr, s_FalseStr), sf);

    WriteIniStr(SectionName, s_Glow, IntToStr(gd.GlowCount), sf);
    WriteIniStr(SectionName, s_GlowMargin, IntToStr(gd.GlowMargin), sf);

    s := ExtractWord(1, gd.FontColor, [' ']);
    WriteIniStr(SectionName, s_FontColor, s, sf);
    s := ExtractWord(2, gd.FontColor, [' ']);
    WriteIniStr(SectionName, s_TCLeft, s, sf);
    s := ExtractWord(3, gd.FontColor, [' ']);
    WriteIniStr(SectionName, s_TCTop, s, sf);
    s := ExtractWord(4, gd.FontColor, [' ']);
    WriteIniStr(SectionName, s_TCRight, s, sf);
    s := ExtractWord(5, gd.FontColor, [' ']);
    WriteIniStr(SectionName, s_TCBottom, s, sf);
    s := ExtractWord(1, gd.HotFontColor, [' ']);
    WriteIniStr(SectionName, s_HotFontColor, s, sf);
    s := ExtractWord(2, gd.HotFontColor, [' ']);
    WriteIniStr(SectionName, s_HotTCLeft, s, sf);
    s := ExtractWord(3, gd.HotFontColor, [' ']);
    WriteIniStr(SectionName, s_HotTCTop, s, sf);
    s := ExtractWord(4, gd.HotFontColor, [' ']);
    WriteIniStr(SectionName, s_HotTCRight, s, sf);
    s := ExtractWord(5, gd.HotFontColor, [' ']);
    WriteIniStr(SectionName, s_HotTCBottom, s, sf);
    s := IntToStr(gd.Transparency);
    WriteIniStr(SectionName, s_Transparency, s, sf);
    s := IntToStr(gd.GradientPercent);
    WriteIniStr(SectionName, s_GradientPercent, s, sf);
    s := IntToStr(gd.ImagePercent);
    WriteIniStr(SectionName, s_ImagePercent, s, sf);
    s := (gd.GradientData);
    WriteIniStr(SectionName, s_GradientData, s, sf);
    s := iff(gd.ShowFocus, s_TrueStr, s_FalseStr);
    WriteIniStr(SectionName, s_ShowFocus, s, sf);
    s := iff(gd.FadingEnabled, s_TrueStr, s_FalseStr);      
    WriteIniStr(SectionName, s_FadingEnabled, s, sf);
    s := IntToStr(gd.FadingIterations);
    WriteIniStr(SectionName, s_FadingIterations, s, sf);
    s := IntToStr(gd.HotColor);
    WriteIniStr(SectionName, s_HotColor, s, sf);
    s := IntToStr(gd.HotTransparency);
    WriteIniStr(SectionName, s_HotTransparency, s, sf);
    s := IntToStr(gd.HotGradientPercent);
    WriteIniStr(SectionName, s_HotGradientPercent, s, sf);
    s := gd.HotGradientData;
    WriteIniStr(SectionName, s_HotGradientData, s, sf);
    s := IntToStr(gd.HotImagePercent);
    WriteIniStr(SectionName, s_HotImagePercent, s, sf);
  end;
end;

procedure TsSkinManager.SendNewSkin(AllowAnimation : boolean = True);
var
  M : TMessage;
  i : integer;
begin
  if (csLoading in ComponentState) or (csReading in ComponentState) then Exit;
  ClearGlows;
  if not (csDesigning in ComponentState) and AllowAnimation then LockForms(Self);

  if SkinableMenus <> nil then begin
    SkinableMenus.SkinBorderWidth := -1;
  end;
  SkinData.Active := False;
  RestrictDrawing := True;

  InitConstantIndexes;

  M.Msg := SM_ALPHACMD;
  M.WParam := MakeWParam(0, AC_SETNEWSKIN);
  M.LParam := longint(Self);
  M.Result := 0;
  if csDesigning in ComponentState
    then for i := 0 to Screen.FormCount - 1 do begin
      if (Screen.Forms[i].Name = '') or (Screen.Forms[i].Name = 'AppBuilder') or (Screen.Forms[i].Name = 'PropertyInspector') then Continue;
      SendToProvider(Screen.Forms[i], M);
      AlphaBroadCast(Screen.Forms[i], M);
      SendToHooked(M);
    end
    else AppBroadCastS(M);
  RestrictDrawing := False;
  SkinData.Active := True;

  if (DefaultManager = Self) and not GlobalHookInstalled then InstallHook;

  if AllowAnimation then RepaintForms;
  InitDevEx(True);
end;

procedure TsSkinManager.SendRemoveSkin;
var
  M : TMessage;
  i : integer;
begin
  InitDevEx(False);
  aSkinRemoving := True;
  ClearGlows;
  UninstallHook;
  SkinData.Active := False;
  M.Msg := SM_ALPHACMD;
  M.WParam := MakeWParam(0, AC_REMOVESKIN);
  M.LParam := longint(Self);
  M.Result := 0;
  if csDesigning in ComponentState then begin
    for i := 0 to Screen.FormCount - 1 do begin
      if (Screen.Forms[i].Name = '') or
         (Screen.Forms[i].Name = 'AppBuilder') or
         (pos('EditWindow_', Screen.Forms[i].Name)> 0) or
         (pos('DockSite', Screen.Forms[i].Name)> 0) or
         (Screen.Forms[i].Name = 'PropertyInspector') then Continue;
      SendToProvider(Screen.Forms[i], M);
      AlphaBroadCast(Screen.Forms[i], M);
      SendToHooked(M);
    end;
  end
  else begin
    AppBroadCastS(M);
  end;
  FreeBitmaps;
  FreeJpegs;
  SetLength(gd, 0);
  aSkinRemoving := False;
end;

procedure TsSkinManager.SetActive(const Value: boolean);
begin
  if FActive <> Value then begin
    FActive := Value;
    if not Value then begin
      if not (csLoading in ComponentState) then SendRemoveSkin;
      InitConstantIndexes;
      UpdateCommonDlgs(Self);
    end
    else begin
      SkinName := FSkinName;
    end;
  end;
end;

procedure TsSkinManager.SetBuiltInSkins(const Value: TsStoredSkins);
begin
  FBuiltInSkins.Assign(Value);
end;

procedure TsSkinManager.SetCommonSections(const Value: TStringList);
var
  i : integer;
  s : string;
begin
  FCommonSections.Assign(Value);
  for i := 0 to FCommonSections.Count - 1 do begin
    s := FCommonSections[i];
    if (s <> '') and (s[1] <> ';') then FCommonSections[i] := acntUtils.DelChars(s, ' ');
  end;
  SkinName := SkinName;
end;

procedure TsSkinManager.SetSkinDirectory(const Value: string);
begin
  if FSkinDirectory <> Value then begin
    FSkinDirectory := Value;
    SkinData.SkinPath := GetFullSkinDirectory;
  end;
end;

procedure TsSkinManager.SetSkinName(const Value: TsSkinName);
var
  s : string;
begin
  FSkinName := Value;
  if FActive then begin
    if Assigned(FOnBeforeChange) then FOnBeforeChange(Self);
    aSkinChanging := True;

    SkinData.Active := False;

    s := NormalDir(SkinDirectory) + Value + '.' + acSkinExt;
    SkinIsPacked := False;
    if UnPackedFirst and DirectoryExists(NormalDir(SkinDirectory) + Value) then SkinIsPacked := False else SkinIsPacked := FileExists(s);

    if SkinIsPacked then ReloadPackedSkin else ReloadSkin;

    if FActive then begin
      if not NonAutoUpdate then SendNewSkin;
    end
    else SendRemoveSkin;
    aSkinChanging := False;
    if Assigned(FOnAfterChange) then FOnAfterChange(Self);
  end;
  UpdateCommonDlgs(Self);
end;

procedure TsSkinManager.SetSkinnedPopups(const Value: boolean);
begin
  if FSkinnedPopups <> Value then begin
    FSkinnedPopups := Value;
    if not (csDesigning in ComponentState) and FSkinnedPopups and (SkinableMenus <> nil) and IsDefault then SkinableMenus.UpdateMenus;
  end;
end;

procedure TsSkinManager.SetSkinInfo(const Value: TacSkinInfo); begin end;

procedure TsSkinManager.SetVersion(const Value: string); begin end;

procedure TsSkinManager.UpdateSkin;
begin
  if Active then SendNewSkin;
end;

procedure TsSkinManager.UpdateSkinSection(const SectionName: string);
var
  M : TMessage;
  i : integer;
begin
  GlobalSectionName := UpperCase(SectionName);

  M.Msg := SM_ALPHACMD;
  M.WParamHi := AC_UPDATESECTION;
  M.Result := 0;
  if csDesigning in ComponentState then begin
    for i := 0 to Screen.FormCount - 1 do begin
      AlphaBroadCast(Screen.Forms[i], M);
    end;
  end
  else begin
    AppBroadCastS(M);
  end;
end;

procedure TsSkinManager.RepaintForms;
var
  M : TMessage;
  i : integer;
  ap : TacProvider;
begin
  M.Msg := SM_ALPHACMD;
  M.LParam := longint(Self);

  if not (csDesigning in ComponentState) then begin
    M.WParam := MakeWParam(0, AC_STOPFADING);
    M.Result := 0;
    AppBroadCastS(M);
  end;

  M.WParam := MakeWParam(0, AC_REFRESH);
  M.Result := 0;
  if csDesigning in ComponentState then for i := 0 to Screen.FormCount - 1 do begin
    if (Screen.Forms[i].Name = '') or (Screen.Forms[i].Name = 'AppBuilder') or (Screen.Forms[i].Name = 'PropertyInspector') then Continue;
    AlphaBroadCast(Screen.Forms[i], M);
    SendToProvider(Screen.Forms[i], M);
    SendToHooked(M);
  end
  else begin
    if not (csLoading in ComponentState) {and (Application.MainForm <> nil) Changing in DLL}then LockForms(Self);
    AppBroadCastS(M);
    if not (csLoading in ComponentState) {and (Application.MainForm <> nil) Changing in DLL }then UnLockForms(Self);
  end;
  if Assigned(acMagnForm) then SendMessage(acMagnForm.Handle, M.Msg, M.WParam, M.LParam);
  // Repaint dialogs
  if acSupportedList <> nil then begin
    for i := 0 to acSupportedList.Count - 1 do begin
      ap := TacProvider(acSupportedList[i]);
      if (ap <> nil) and (ap.ListSW <> nil) and IsWindowVisible(ap.ListSW.CtrlHandle) then
        RedrawWindow(ap.ListSW.CtrlHandle, nil, 0, RDW_ERASE or RDW_FRAME or {RDW_INTERNALPAINT or }RDW_INVALIDATE or RDW_UPDATENOW or RDW_ALLCHILDREN);// or RDW_ERASENOW);
    end;
  end;
end;

procedure TsSkinManager.SetHueOffset(const Value: integer);
var
  s : string;
begin
  if FHueOffset <> Value then begin
    FHueOffset := Value;

    if SkinData.Active then begin
      aSkinChanging := True;
      s := NormalDir(SkinDirectory) + SkinName + '.' + acSkinExt;
      SkinIsPacked := FileExists(s);

      if SkinIsPacked then ReloadPackedSkin else ReloadSkin;
      aSkinChanging := False;
      if not (csLoading in ComponentState) and not (csReading in ComponentState) then RepaintForms;
    end
  end;
end;

procedure TsSkinManager.SetSaturation(const Value: integer);
var
  s : string;
begin
  if FSaturation <> Value then begin
    FSaturation := Value;
    if SkinData.Active then begin
      aSkinChanging := True;
      s := NormalDir(SkinDirectory) + SkinName + '.' + acSkinExt;
      SkinIsPacked := FileExists(s);

      if SkinIsPacked then ReloadPackedSkin else ReloadSkin;
      aSkinChanging := False;
      if not (csLoading in ComponentState) and not (csReading in ComponentState) then RepaintForms;
    end
  end;
end;

function TsSkinManager.GetActiveEditColor: TColor;
begin
  if (ConstData.IndexGlobalInfo > -1) and (ConstData.IndexGlobalInfo <= Length(gd) - 1)
    then Result := ColorToRGB(gd[ConstData.IndexGlobalInfo].HotColor)
    else Result := ColorToRGB(clWindow);
end;

function TsSkinManager.GetActiveEditFontColor: TColor;
var
  i : integer;
begin
  i := GetSkinIndex(s_Edit);
  if (i > -1) //and (i <= Length(gd) - 1)
    then Result := ColorToRGB(gd[i].HotFontColor[1])
    else Result := clFuchsia;
end;

function TsSkinManager.GetHighLightColor: TColor;
var
  i : integer;
begin
  i := GetSkinIndex(s_MENUITEM);
  if IsValidSkinIndex(i) and (gd[i].HotColor <> -1) and (gd[i].HotColor <> clFuchsia) and (gd[i].HotColor <> clWhite) then begin
    Result := ColorToRGB(gd[i].HotColor)
  end
  else begin
    Result := clHighLight
  end;
end;

function TsSkinManager.GetHighLightFontColor: TColor;
var
  i : integer;
begin
  i := GetSkinIndex(s_MENUITEM);
  if IsValidSkinIndex(i) and (gd[i].HotFontColor[1] <> -1) and (gd[i].HotFontColor[1] <> clFuchsia) and (gd[i].HotFontColor[1] <> clWhite) and (gd[i].HotFontColor[1] <> 0) and (gd[i].HotColor <> -1) and (gd[i].HotColor <> clFuchsia) and (gd[i].HotColor <> clWhite) then begin
    Result := ColorToRGB(gd[i].HotFontColor[1])
  end
  else begin
    Result := clHighLightText
  end;
end;

function TsSkinManager.IsValidImgIndex(ImageIndex: integer): boolean;
begin
  Result := (ImageIndex > -1) and (ImageIndex < Length(ma));
end;

function TsSkinManager.IsValidSkinIndex(SkinIndex: integer): boolean;
begin
  Result := (SkinData <> nil) and (SkinIndex > -1) and (SkinIndex < Length(gd));
end;

procedure TsSkinManager.LoadAllMasks;
var
  sf : TMemIniFile;
  Sections, Values : TStringList;
  SkinIndex, i, j, l : integer;
  s, subs, s1 : string;
  TempBmp : TBitmap;
  b : boolean;
  Png : TPNGGraphic;
begin
  FreeBitmaps;
  if SkinFile <> nil then begin
    sf := SkinFile;
    // Reading of the MasterBitmap if exists
    s := sf.ReadString(s_GLOBALINFO, s_MASTERBITMAP, '');
    MasterBitmap := TBitmap.Create;
    if SkinIsPacked then begin
      for i := 0 to sc.ImageCount - 1 do begin
        if UpperCase(sc.Files[i].FileName) = s then begin
          sc.Files[i].FileStream.Seek(0, 0);
          MasterBitmap.LoadFromStream(sc.Files[i].FileStream);
          Break
        end;
      end;
    end
    else begin
      if (pos(':', s) < 1) then s := SkinData.SkinPath + s;
      if (s <> '') and FileExists(s) then MasterBitmap.LoadFromFile(s);
    end;

    MasterBitmap.PixelFormat := pf24bit;
    MasterBitmap.Transparent := True;
    MasterBitmap.TransparentColor := clFuchsia;
    MasterBitmap.HandleType := bmDIB;

    Sections := TStringList.Create;
    Values := TStringList.Create;
    try sf.ReadSections(Sections);
    for i := 0 to Sections.Count - 1 do begin
      sf.ReadSection(Sections[i], Values);
      for j := 0 to Values.Count - 1 do begin
        if (Sections[i] = s_GLOBALINFO) then begin // Check for MASTERBITMAP property
          if (Values[j] = s_MasterBitmap) then Continue;
        end;
        s := sf.ReadString(Sections[i], Values[j], '');
        if (s <> '') then begin
          case s[1] of
            TexChar : begin
              l := Length(ma);
              SetLength(ma, l + 1);
              ma[l].PropertyName := Values[j];
              ma[l].ClassName := Sections[i];
              ma[l].Manager := Self;
              ma[l].ImgType := itisaTexture;
              ma[l].R := Rect(StrToInt(Copy(s, 2, 4)), StrToInt(Copy(s, 7, 4)), StrToInt(Copy(s, 12, 4)), StrToInt(Copy(s, 17, 4)));
              ma[l].ImageCount := 1;
              ma[l].DrawMode := StrToInt(Copy(s, 22, 2));
              if Length(s) > 24
                then ma[l].MaskType := StrToInt(Copy(s, 25, 1))
                else ma[l].MaskType := 0;
            end;
            CharGlyph : begin
              l := Length(ma);
              SetLength(ma, l + 1);
              ma[l].PropertyName := Values[j];
              ma[l].ClassName := Sections[i];
              ma[l].Manager := Self;
              ma[l].ImgType := itisaGlyph;
              ma[l].R := Rect(StrToInt(Copy(s, 2, 4)), StrToInt(Copy(s, 7, 4)), StrToInt(Copy(s, 12, 4)), StrToInt(Copy(s, 17, 4)));
              ma[l].ImageCount := StrToInt(Copy(s, 22, 1));
              ma[l].MaskType := StrToInt(Copy(s, 24, 1));
            end;
            CharMask : begin
              l := Length(ma);
              SetLength(ma, l + 1);
              ma[l].PropertyName := Values[j];
              ma[l].ClassName := Sections[i];
              ma[l].Manager := Self;
              ma[l].ImgType := itisaBorder;
              ma[l].R := Rect(StrToInt(Copy(s, 2, 4)), StrToInt(Copy(s, 7, 4)), StrToInt(Copy(s, 12, 4)), StrToInt(Copy(s, 17, 4)));
              ma[l].WL := StrToInt(Copy(s, 22, 4));
              ma[l].WT := StrToInt(Copy(s, 27, 4));
              ma[l].WR := StrToInt(Copy(s, 32, 4));
              ma[l].WB := StrToInt(Copy(s, 37, 4));
              ma[l].ImageCount := StrToInt(Copy(s, 42, 1));
              ma[l].MaskType := StrToInt(Copy(s, 44, 1));
              ma[l].DrawMode := StrToInt(Copy(s, 46, 1));
              ma[l].BorderWidth := StrToInt(Copy(s, 48, 1));
              if ma[l].WL + ma[l].WT + ma[l].WR + ma[l].WB = 0 then begin // If BorderWidths are not defined
                if ma[l].BorderWidth <> 0 then begin
                  ma[l].WL := ma[l].BorderWidth;
                  ma[l].WT := ma[l].BorderWidth;
                  ma[l].WR := ma[l].BorderWidth;
                  ma[l].WB := ma[l].BorderWidth;
                end
                else begin
                  ma[l].WL := WidthOf(ma[l].R) div (ma[l].ImageCount * 3);
                  ma[l].WT := HeightOf(ma[l].R) div ((1 + ma[l].MaskType) * 3);
                  ma[l].WR := ma[l].WL;
                  ma[l].WB := ma[l].WT;
                end;
              end;
            end;
            CharExt : begin
              s1 := ExtractWord(1, s, [CharExt]);
              b := pos('.BMP', s1) > 0;
              if b or (pos('.PNG', s1) > 0) then begin                              // Else if bitmap assigned
                TempBmp := nil;
                if SkinIsPacked then begin
                  for l := 0 to sc.ImageCount - 1 do if UpperCase(sc.Files[l].FileName) = s1 then begin
                    TempBmp := TBitmap.Create;
                    sc.Files[l].FileStream.Seek(0, 0);
                    if b then begin // If is bitmap
                      TempBmp.LoadFromStream(sc.Files[l].FileStream);
                    end
                    else begin // If is PNG
                      Png := TPNGGraphic.Create;
                      Png.LoadFromStream(sc.Files[l].FileStream);
                      TempBmp.Assign(Png);
                      UpdateTransPixels(TempBmp);
                      Png.Free;
                    end;
                    break;
                  end;
                end
                else begin
                  if (pos(':', s1) < 1) then s1 := SkinData.SkinPath + s1;
                  if FileExists(s1) then begin
                    TempBmp := TBitmap.Create;
                    if b then begin // If is bitmap
                      TempBmp.LoadFromFile(s1);
                    end
                    else begin // If is PNG
                      Png := TPNGGraphic.Create;
                      Png.LoadFromFile(s1);
                      TempBmp.Assign(Png);
                      UpdateTransPixels(TempBmp);
                      Png.Free;
                    end;
                  end;
                end;
                if (TempBmp <> nil) and (TempBmp.Width > 0) then begin
                  l := Length(ma);
                  SetLength(ma, l + 1);
                  try
                    ma[l].Bmp := TempBmp;
                    ma[l].ImgType := acImgTypes[Min(StrToInt(ExtractWord(4, s, [CharExt])), Length(acImgTypes) - 1)];
                    if b and not (ma[l].ImgType in [itisaTexture]) then ma[l].Bmp.PixelFormat := pf24bit;
                  finally
                    ma[l].PropertyName := Values[j];
                    ma[l].ClassName := Sections[i];
                    ma[l].Manager := Self;
                    ma[l].ImageCount := StrToInt(ExtractWord(2, s, [CharExt]));
                    ma[l].MaskType := StrToInt(ExtractWord(3, s, [CharExt]));
                    ma[l].DrawMode := BDM_FILL;
                    ma[l].R := Rect(0, 0, ma[l].Bmp.Width, ma[l].Bmp.Height);
                    if WordCount(s, [CharExt]) > 4 then begin // if border widthes are defined
                      ma[l].WL := StrToInt(ExtractWord(5, s, [CharExt]));
                      ma[l].WT := StrToInt(ExtractWord(6, s, [CharExt]));
                      ma[l].WR := StrToInt(ExtractWord(7, s, [CharExt]));
                      ma[l].WB := StrToInt(ExtractWord(8, s, [CharExt]));
                    end
                    else with ma[l] do begin
                      ma[l].WL := WidthOf(ma[l].R) div (ImageCount * 3);
                      ma[l].WT := HeightOf(ma[l].R) div ((1 + MaskType) * 3);
                      ma[l].WR := ma[l].WL;
                      ma[l].WB := ma[l].WT;
                    end;
                  end;
                end;
              end;
            end
            else begin
              if (pos(CharDiez, s) > 0) then begin       // Reading of the MasterBitmap item
                if s = CharDiez then Continue;
                l := Length(ma) + 1;
                SetLength(ma, l);

                ma[l - 1].PropertyName := UpperCase(Values[j]);
                ma[l - 1].ClassName := UpperCase(Sections[i]);
                ma[l - 1].Manager := Self;
                subs := ExtractWord(2, s, [')', '(', ' ']);
                ma[l - 1].R := Rect(StrToInt(ExtractWord(1, subs, [',', ' '])),
                                    StrToInt(ExtractWord(2, subs, [',', ' '])),
                                    StrToInt(ExtractWord(3, subs, [',', ' '])),
                                    StrToInt(ExtractWord(4, subs, [',', ' '])));

                subs := ExtractWord(3, s, [')', '(', ' ']);
                ma[l - 1].ImageCount := StrToInt(ExtractWord(1, subs, [',', ' ']));
                if ma[l - 1].ImageCount < 1 then ma[l - 1].ImageCount := 1;
                ma[l - 1].MaskType := StrToInt(ExtractWord(2, subs, [',', ' ']));
                if ma[l - 1].MaskType < 0 then ma[l - 1].MaskType := 0;
                // BorderWidth
                s1 := ExtractWord(3, subs, [',', ' ']);
                if s1 <> '' then begin
                  ma[l - 1].BorderWidth := StrToInt(s1);
                  if ma[l - 1].BorderWidth < 0 then ma[l - 1].BorderWidth := 0;
                end
                else ma[l - 1].BorderWidth := 0;
                // StretchMode
                s1 := ExtractWord(4, subs, [',', ' ']);
                if s1 <> ''
                  then ma[l - 1].DrawMode := StrToInt(s1)
                  else ma[l - 1].DrawMode := 1; // Stretching of borders is allowed if possible
                with ma[l - 1] do begin
                  if WL + WT + WR + WB = 0 then begin // If BorderWidths are not defined
                    if BorderWidth <> 0 then begin
                      WL := BorderWidth;
                      WT := BorderWidth;
                      WR := BorderWidth;
                      WB := BorderWidth;
                    end
                    else begin
                      WL := WidthOf(ma[l - 1].R) div (ImageCount * 3);
                      WT := HeightOf(ma[l - 1].R) div ((1 + MaskType) * 3);
                      WR := WL;
                      WB := WT;
                    end;
                  end;
                end;
                Continue;
              end;
              s := AnsiUpperCase(s);
              if (pos('.BMP', s) > 0) then begin                              // Else if bitmap assigned
                TempBmp := nil;
                if SkinIsPacked then begin
                  for l := 0 to sc.ImageCount - 1 do if UpperCase(sc.Files[l].FileName) = s then begin
                    TempBmp := TBitmap.Create;
                    sc.Files[l].FileStream.Seek(0, 0);
                    TempBmp.LoadFromStream(sc.Files[l].FileStream);
                    break;
                  end;
                end
                else begin
                  if (pos(':', s) < 1) then s := SkinData.SkinPath + s;
                  if FileExists(s) then begin
                    TempBmp := TBitmap.Create;
                    TempBmp.LoadFromFile(s);
                  end;
                end;

                if (TempBmp <> nil) and (TempBmp.Width > 0) then begin
                  l := Length(ma) + 1;
                  SetLength(ma, l);
                  ma[l - 1].Bmp := TempBmp;
                  ma[l - 1].PropertyName := '';
                  ma[l - 1].ClassName := '';
                  ma[l - 1].Manager := Self;
                  try
                    ma[l - 1].Bmp.PixelFormat := pf24bit;
                  finally
                    ma[l - 1].PropertyName := UpperCase(Values[j]);
                    ma[l - 1].ClassName := UpperCase(Sections[i]);
                    ma[l - 1].MaskType := 1;
                    ma[l - 1].ImageCount := 3;
                    ma[l - 1].R := Rect(0, 0, ma[l - 1].Bmp.Width, ma[l - 1].Bmp.Height);
                    ma[l - 1].WL := WidthOf(ma[l - 1].R) div (ma[l - 1].ImageCount * 3);
                    ma[l - 1].WT := HeightOf(ma[l - 1].R) div ((1 + ma[l - 1].MaskType) * 3);
                    ma[l - 1].WR := ma[l - 1].WL;
                    ma[l - 1].WB := ma[l - 1].WT;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;

    finally
      if Assigned(Values) then FreeAndNil(Values);
      if Assigned(Sections) then FreeAndNil(Sections);
      sf := nil;
    end;
  end
  else begin
    SkinIndex := -1;
    for i := 0 to InternalSkins.Count - 1 do if InternalSkins[i].Name = SkinName then begin
      SkinIndex := i;
      break;
    end;
    if SkinIndex < 0 then Exit;
    if Assigned(InternalSkins[SkinIndex].MasterBitmap) then begin
      if Assigned(MasterBitmap) then FreeAndnil(MasterBitmap);
      MasterBitmap := TBitmap.Create;
      MasterBitmap.Assign(InternalSkins[SkinIndex].MasterBitmap);
      MasterBitmap.Transparent := True;
      MasterBitmap.TransparentColor := clFuchsia;
      MasterBitmap.PixelFormat := pf24bit;
      MasterBitmap.HandleType := bmDIB;
    end;
    for i := 0 to InternalSkins[SkinIndex].Images.Count - 1 do begin
      l := Length(ma) + 1;
      SetLength(ma, l);
      if (InternalSkins[SkinIndex].Images[i].Image <> nil) and (InternalSkins[SkinIndex].Images[i].Name <> '') then begin
        ma[l - 1].Bmp := TBitmap.Create;
        ma[l - 1].Bmp.Assign(InternalSkins[SkinIndex].Images[i].Image);
        ma[l - 1].Bmp.PixelFormat := pf24bit;
        ma[l - 1].R := Rect(0, 0, ma[l - 1].Bmp.Width, ma[l - 1].Bmp.Height);
      end
      else ma[l - 1].R := Rect(InternalSkins[SkinIndex].Images[i].Left,
                               InternalSkins[SkinIndex].Images[i].Top,
                               InternalSkins[SkinIndex].Images[i].Right,
                               InternalSkins[SkinIndex].Images[i].Bottom);
      ma[l - 1].PropertyName := UpperCase(InternalSkins[SkinIndex].Images[i].PropertyName);
      ma[l - 1].ClassName := UpperCase(InternalSkins[SkinIndex].Images[i].SectionName);
      ma[l - 1].ImageCount := InternalSkins[SkinIndex].Images[i].ImageCount;
      ma[l - 1].MaskType := InternalSkins[SkinIndex].Images[i].MaskType;
      if ma[l - 1].ImageCount = 0 then ma[l - 1].MaskType := 1; // if 0 then external picture
      if ma[l - 1].ImageCount = 0 then ma[l - 1].ImageCount := 3;
      ma[l - 1].BorderWidth := InternalSkins[SkinIndex].Images[i].BorderWidth;
      ma[l - 1].DrawMode := InternalSkins[SkinIndex].Images[i].StretchMode;
      ma[l - 1].ImgType := InternalSkins[SkinIndex].Images[i].ImgType;
      ma[l - 1].WL := InternalSkins[SkinIndex].Images[i].WL;
      ma[l - 1].WT := InternalSkins[SkinIndex].Images[i].WT;
      ma[l - 1].WR := InternalSkins[SkinIndex].Images[i].WR;
      ma[l - 1].WB := InternalSkins[SkinIndex].Images[i].WB;
      with ma[l - 1] do begin
        if WL + WT + WR + WB = 0 then begin // If BorderWidths are not defined
          if BorderWidth <> 0 then begin
            WL := BorderWidth;
            WT := BorderWidth;
            WR := BorderWidth;
            WB := BorderWidth;
          end
          else begin
            WL := WidthOf(ma[l - 1].R) div (ImageCount * 3);
            WT := HeightOf(ma[l - 1].R) div ((1 + MaskType) * 3);
            WR := WL;
            WB := WT;
          end;
        end;
      end;

      ma[l - 1].Manager := Self;
    end;
  end;

  // CommonSection property in TsSkinManager
  if CommonSections.Count > 0 then begin
    sf := TMemInifile.Create('1.tmp');
    sf.SetStrings(CommonSections);

    Sections := TStringList.Create;
    Values := TStringList.Create;
    try

    sf.ReadSections(Sections);
    sf.SetStrings(CommonSections);
    for i := 0 to Sections.Count - 1 do begin
      if UpperCase(Sections[i]) = s_GLOBALINFO then Continue;
      sf.ReadSection(Sections[i], Values);
      for j := 0 to Values.Count - 1 do begin
        s := sf.ReadString(Sections[i], Values[j], '-');


        if (MasterBitmap <> nil) and (pos(CharDiez, s) > 0) then begin       // Reading of the MasterBitmap item
          if s = CharDiez then Continue;
          l := Length(ma) + 1;
          SetLength(ma, l);

          ma[l - 1].PropertyName := UpperCase(Values[j]);
          ma[l - 1].ClassName := UpperCase(Sections[i]);
          ma[l - 1].Manager := Self;

          subs := ExtractWord(2, s, [')', '(', ' ']);
          ma[l - 1].R := Rect(StrToInt(ExtractWord(1, subs, [',', ' '])),
                              StrToInt(ExtractWord(2, subs, [',', ' '])),
                              StrToInt(ExtractWord(3, subs, [',', ' '])),
                              StrToInt(ExtractWord(4, subs, [',', ' '])));

          subs := ExtractWord(3, s, [')', '(', ' ']);
          ma[l - 1].ImageCount := StrToInt(ExtractWord(1, subs, [',', ' ']));
          if ma[l - 1].ImageCount < 1 then ma[l - 1].ImageCount := 1;
          ma[l - 1].MaskType := StrToInt(ExtractWord(2, subs, [',', ' ']));
          if ma[l - 1].MaskType < 0 then ma[l - 1].MaskType := 0;

          // BorderWidth
          s1 := ExtractWord(3, subs, [',', ' ']);
          if s1 <> '' then begin
            ma[l - 1].BorderWidth := StrToInt(s1);
            if ma[l - 1].BorderWidth < 0 then ma[l - 1].BorderWidth := 0;
          end
          else ma[l - 1].BorderWidth := 0;

          // StretchMode
          s1 := ExtractWord(4, subs, [',', ' ']);
          if s1 <> ''
            then ma[l - 1].DrawMode := StrToInt(s1)
            else ma[l - 1].DrawMode := 1;
          with ma[l - 1] do begin
            if WL + WT + WR + WB = 0 then begin // If BorderWidths are not defined
              if BorderWidth <> 0 then begin
                WL := BorderWidth;
                WT := BorderWidth;
                WR := BorderWidth;
                WB := BorderWidth;
              end
              else begin
                WL := WidthOf(ma[l - 1].R) div (ImageCount * 3);
                WT := HeightOf(ma[l - 1].R) div ((1 + MaskType) * 3);
                WR := WL;
                WB := WT;
              end;
            end;
          end;
          Continue;
        end;
        s := AnsiUpperCase(s);
        if (pos('.BMP', s) > 0) then begin                              // Else if bitmap assigned
          if (pos(':', s) < 1) then begin
            s := SkinData.SkinPath + s;
          end;
          if FileExists(s) then begin
            l := Length(ma) + 1;
            SetLength(ma, l);
            ma[l - 1].PropertyName := '';
            ma[l - 1].ClassName := '';
            ma[l - 1].Manager := Self;
            try
              ma[l - 1].Bmp := TBitmap.Create;
              ma[l - 1].Bmp.LoadFromFile(s);
              ma[l - 1].Bmp.PixelFormat := pf24bit;
            finally
              ma[l - 1].PropertyName := UpperCase(Values[j]);
              ma[l - 1].ClassName := UpperCase(Sections[i]);
            end;
            if (ma[l - 1].Bmp.Width < 1) then begin
              if Assigned(ma[l - 1].bmp) then FreeAndNil(ma[l - 1].Bmp);
              SetLength(ma, l - 1);
            end
          end
        end
      end
    end;
    finally
      if Assigned(Values) then FreeAndNil(Values);
      FreeAndNil(sf);
      FreeAndNil(Sections);
    end;
  end;
end;

procedure TsSkinManager.LoadAllPatterns;
var
  sf : TMemIniFile;
  Sections, Values : TStringList;
  SkinIndex, i, j, l, n : integer;
  s : string;
  TempJpg : TJpegImage;
begin
  FreeJpegs;

  if SkinFile <> nil then begin // Loading from external skin

    sf := SkinFile;
    Sections := TStringList.Create;
    Values := TStringList.Create;
    try

    sf.ReadSections(Sections);
    for i := 0 to Sections.Count - 1 do begin
      sf.ReadSection(Sections[i], Values);
      for j := 0 to Values.Count - 1 do begin
        s := sf.ReadString(Sections[i], Values[j], '-');
        s := AnsiUpperCase(s);

        if (pos('.JPG', s) > 0) or (pos('.JPEG', s) > 0) then begin
          TempJpg := nil;
          if SkinIsPacked then begin
            for l := 0 to sc.ImageCount - 1 do if UpperCase(sc.Files[l].FileName) = s then begin
              TempJpg := TJpegImage.Create;
              sc.Files[l].FileStream.Seek(0, 0);
              TempJpg.LoadFromStream(sc.Files[l].FileStream);
              break;
            end;
          end
          else begin
            if (pos(':', s) < 1) then s := SkinData.SkinPath + s;
            if FileExists(s) then begin
              TempJpg := TJpegImage.Create;
              TempJpg.LoadFromFile(s);
            end;
          end;

          if (TempJpg <> nil) and (TempJpg.Width > 0) then begin

            l := Length(pa) + 1;
            SetLength(pa, l);
            pa[l - 1].PropertyName := '';
            pa[l - 1].ClassName := '';

            pa[l - 1].Img := TempJpg;

            if pa[l - 1].Img.Width * pa[l - 1].Img.Height < 90000 then begin // convert to bitmap
              n := Length(ma) + 1;
              SetLength(ma, n);
              ma[n - 1].Bmp := TBitmap.Create;
              ma[n - 1].Bmp.Width := pa[l - 1].Img.Width;
              ma[n - 1].Bmp.Height := pa[l - 1].Img.Height;
              ma[n - 1].Bmp.Canvas.Draw(0, 0, pa[l - 1].Img);
              ma[n - 1].PropertyName := UpperCase(Values[j]);
              ma[n - 1].ClassName := UpperCase(Sections[i]);
              ma[n - 1].ImgType := itisaTexture;
              ma[n - 1].ImageCount := 1;
              ma[l - 1].Manager := Self;
              if Assigned(pa[l - 1].Img) then FreeAndNil(pa[l - 1].Img);
              SetLength(pa, l - 1);
            end
            else begin // big image stored as Jpeg
              pa[l - 1].PropertyName := UpperCase(Values[j]);
              pa[l - 1].ClassName := UpperCase(Sections[i]);
            end;
          end;
        end;
      end;
    end;
    finally
      if Assigned(Values) then FreeAndNil(Values);
      if Assigned(Sections) then FreeAndNil(Sections);
    end;
  end
  else begin // Loading from internal skin
    SkinIndex := -1;
    for i := 0 to InternalSkins.Count - 1 do if InternalSkins[i].Name = SkinName then begin
      SkinIndex := i;
      break;
    end;
    if SkinIndex < 0 then Exit;
    for i := 0 to InternalSkins[SkinIndex].Patterns.Count - 1 do begin
      begin // big image stored as Jpeg
        l := Length(pa) + 1;
        SetLength(pa, l);
        pa[l - 1].Img := TJpegImage.Create;
        if not InternalSkins[SkinIndex].Patterns[i].Image.Empty
          then  pa[l - 1].Img.Assign(InternalSkins[SkinIndex].Patterns[i].Image)
          else begin
          for j := 0 to InternalSkins[SkinIndex].Patterns.Count - 1 do begin
            if (InternalSkins[SkinIndex].Patterns[j].FName = InternalSkins[SkinIndex].Patterns[i].FName) and not InternalSkins[SkinIndex].Patterns[j].Image.Empty then begin
              pa[l - 1].Img.Assign(InternalSkins[SkinIndex].Patterns[j].Image);
              Break;
            end;
          end;
        end;
        pa[l - 1].PropertyName := UpperCase(InternalSkins[SkinIndex].Patterns[i].PropertyName);
        pa[l - 1].ClassName := UpperCase(InternalSkins[SkinIndex].Patterns[i].SectionName);
      end;
    end;
  end;

  // CommonSection property in TsSkinManager
  if CommonSections.Count > 0 then begin
    sf := TMemInifile.Create('2.tmp');
    sf.SetStrings(CommonSections);
    Sections := TStringList.Create;
    Values := TStringList.Create;
    try
    sf.ReadSections(Sections);
    for i := 0 to Sections.Count - 1 do begin
      sf.ReadSection(Sections[i], Values);
      for j := 0 to Values.Count - 1 do begin
        s := sf.ReadString(Sections[i], Values[j], '-');
        s := AnsiUpperCase(s);

        if (pos('.JPG', s) > 0) or (pos('.JPEG', s) > 0) then begin

          if (pos(':', s) < 1) then begin
            s := SkinData.SkinPath + s;
          end;
          if FileExists(s) then begin //Break;
            l := Length(pa) + 1;
            SetLength(pa, l);
            pa[l - 1].PropertyName := '';
            pa[l - 1].ClassName := '';
            try
              pa[l - 1].Img := TJpegImage.Create;
              pa[l - 1].Img.LoadFromFile(s);
            finally
              pa[l - 1].PropertyName := UpperCase(Values[j]);
              pa[l - 1].ClassName := UpperCase(Sections[i]);
            end;
            if pa[l - 1].Img.Width < 1 then begin
              if Assigned(pa[l - 1].Img) then FreeAndNil(pa[l - 1].Img);
              SetLength(pa, l - 1);
            end;
          end;
        end;
      end;
    end;
    finally
      if Assigned(Values) then FreeAndNil(Values);
      if Assigned(Sections) then FreeAndNil(Sections);
      FreeAndNil(sf);
    end;
  end;
end;

procedure TsSkinManager.FreeBitmaps;
begin
  while Length(ma) > 0 do begin
    if Assigned(ma[Length(ma) - 1].Bmp) then FreeAndNil(ma[Length(ma) - 1].Bmp);
    SetLength(ma, Length(ma) - 1);
  end;
  if Assigned(MasterBitmap) then FreeAndNil(MasterBitmap);
end;

procedure TsSkinManager.FreeJpegs;
begin
  while Length(pa) > 0 do begin
    if Assigned(pa[Length(pa) - 1].Img) then FreeAndNil(pa[Length(pa) - 1].Img);
    SetLength(pa, Length(pa) - 1);
  end;
end;

procedure TsSkinManager.LoadAllGeneralData;
var
  sf : TMemIniFile;
  gData : TsGeneralData;
  Sections, Ini : TStringList;
  i, j, l, SkinIndex, ParentIndex, int : integer;
  sg : TsSkinGenerals;
  s : string;
  OldSeparator : char;

  function FindString(const ClassName, PropertyName, DefaultValue : string) : string; var s : string; begin
    Result := sf.ReadString(ClassName, PropertyName, CharQuest);
    if Result = CharQuest then begin
      s := sf.ReadString(ClassName, s_ParentClass, CharQuest);
      if (s <> '') and (s <> CharQuest) and (s <> ClassName) then begin
        Result := FindString(s, PropertyName, CharQuest);
      end;
      if Result = CharQuest then Result := DefaultValue;
    end;
  end;
  function FindInteger(const ClassName, PropertyName : string; DefaultValue : integer) : integer; var s : string; begin
    Result := sf.ReadInteger(ClassName, PropertyName, -1);
    if Result = -1 then begin
      s := sf.ReadString(ClassName, s_ParentClass, CharQuest);
      if (s <> '') and (s <> CharQuest) and (s <> ClassName) then begin
        Result := FindInteger(s, PropertyName, -1);
      end;
      if Result = -1 then Result := DefaultValue;
    end;
  end;
begin
  if SkinFile <> nil then begin // If external skin

    sf := SkinFile;

    // Global info
    OldSeparator := DecimalSeparator;
    DecimalSeparator := '.';
    SkinData.Version := sf.ReadFloat(s_GlobalInfo, s_Version, 0);
    DecimalSeparator := OldSeparator;
    SkinData.Author := sf.ReadString(s_GlobalInfo, s_Author, '');
    SkinData.Description := sf.ReadString(s_GlobalInfo, s_Description, '');

    SkinData.Shadow1Color := sf.ReadInteger(s_GlobalInfo, s_Shadow1Color, 0);
    SkinData.Shadow1Offset := StrToInt(sf.ReadString(s_GlobalInfo, s_Shadow1Offset, '0'));
    SkinData.Shadow1Blur := StrToInt(sf.ReadString(s_GlobalInfo, s_Shadow1Blur, '-1'));
    SkinData.Shadow1Transparency := StrToInt(sf.ReadString(s_GlobalInfo, s_Shadow1Transparency, '0'));

    SkinData.BISpacing := sf.ReadInteger(s_GlobalInfo, s_BISpacing, 0);
    SkinData.BIVAlign := sf.ReadInteger(s_GlobalInfo, s_BIVAlign, 0);
    SkinData.BIRightMargin := sf.ReadInteger(s_GlobalInfo, s_BIRightMargin, 0);
    SkinData.BILeftMargin := sf.ReadInteger(s_GlobalInfo, s_BILeftMargin, 0);
    SkinData.BorderColor := sf.ReadInteger(s_GlobalInfo, s_BorderColor, clBlack);

    SkinData.BICloseGlow := StrToInt(sf.ReadString(s_GlobalInfo, s_BorderIconClose + s_Glow, '0'));
    SkinData.BICloseGlowMargin := StrToInt(sf.ReadString(s_GlobalInfo, s_BorderIconClose + s_GlowMargin, '0'));
    SkinData.BIMaxGlow := StrToInt(sf.ReadString(s_GlobalInfo, s_BorderIconMaximize + s_Glow, '0'));
    SkinData.BIMaxGlowMargin := StrToInt(sf.ReadString(s_GlobalInfo, s_BorderIconMaximize + s_GlowMargin, '0'));
    SkinData.BIMinGlow := StrToInt(sf.ReadString(s_GlobalInfo, s_BorderIconMinimize + s_Glow, '0'));
    SkinData.BIMinGlowMargin := StrToInt(sf.ReadString(s_GlobalInfo, s_BorderIconMinimize + s_GlowMargin, '0'));

    CheckVersion;

    Sections := TStringList.Create;
    try

    SetLength(gd, 0);
    sf.ReadSections(Sections);

    for i := 0 to Sections.Count - 1 do begin
      l := Length(gd) + 1;
      SetLength(gd, l);
//General data
      gd[i].ClassName := Sections[i];
      gd[i].ParentClass := sf.ReadString(Sections[i], s_ParentClass, '');

      gd[i].Color := ColorToRGB(FindInteger(Sections[i], s_Color, clWhite));
      gd[i].ReservedBoolean := UpperCase(FindString(Sections[i], s_ReservedBoolean, s_FalseStr)) = s_TrueStr;
      gd[i].GiveOwnFont := UpperCase(FindString(Sections[i], s_GiveOwnFont, s_FalseStr)) = s_TrueStr;

      gd[i].GlowCount := FindInteger(Sections[i], s_Glow, 0);
      gd[i].GlowMargin := FindInteger(Sections[i], s_GlowMargin, 0);

      gd[i].ImgTL := -1;
      gd[i].ImgTR := -1;
      gd[i].ImgBL := -1;
      gd[i].ImgBR := -1;

      gd[i].FontColor[1] := FindInteger(Sections[i], s_FontColor, clBlack);
      gd[i].FontColor[2] := FindInteger(Sections[i], s_TCLeft  , -1);
      gd[i].FontColor[3] := FindInteger(Sections[i], s_TCTop   , -1);
      gd[i].FontColor[4] := FindInteger(Sections[i], s_TCRight , -1);
      gd[i].FontColor[5] := FindInteger(Sections[i], s_TCBottom, -1);

      gd[i].HotFontColor[1] := FindInteger(Sections[i], s_HotFontColor, clBlack);
      gd[i].HotFontColor[2] := FindInteger(Sections[i], s_HotTCLeft  , -1);
      gd[i].HotFontColor[3] := FindInteger(Sections[i], s_HotTCTop   , -1);
      gd[i].HotFontColor[4] := FindInteger(Sections[i], s_HotTCRight , -1);
      gd[i].HotFontColor[5] := FindInteger(Sections[i], s_HotTCBottom, -1);

//Panels data
      gd[i].Transparency := FindInteger(Sections[i], s_Transparency, 0);
      gd[i].GradientPercent := FindInteger(Sections[i], s_GradientPercent, 0);
      gd[i].ImagePercent := FindInteger(Sections[i], s_ImagePercent, 0);
      gd[i].GradientData := FindString(Sections[i], s_GradientData, ' ');
      if gd[i].GradientData <> ' ' then PrepareGradArray(gd[i].GradientData, gd[i].GradientArray);
// Buttons data
      gd[i].ShowFocus := UpperCase(FindString(Sections[i], s_ShowFocus, s_FalseStr)) = s_TrueStr;
// ---- BtnEffects ----
      gd[i].FadingEnabled := UpperCase(FindString(Sections[i], s_FadingEnabled, s_FalseStr)) = s_TrueStr;
      gd[i].FadingIterations := FindInteger(Sections[i], s_FadingIterations, 5);

// ---- PaintingOptions -----
      gd[i].HotColor := TColor(FindInteger(Sections[i], s_HotColor, clWhite));
      gd[i].HotTransparency := FindInteger(Sections[i], s_HotTransparency, 0);
      gd[i].HotGradientPercent := FindInteger(Sections[i], s_HotGradientPercent, 0);
      gd[i].HotGradientData := FindString(Sections[i], s_HotGradientData, '');
      if gd[i].HotGradientData <> ' ' then PrepareGradArray(gd[i].HotGradientData, gd[i].HotGradientArray);
      gd[i].HotImagePercent := FindInteger(Sections[i], s_HotImagePercent, 0);
    end;

    finally
      if Assigned(Sections) then FreeAndNil(Sections);
    end;
  end
  else begin // If internal skin
    SkinIndex := -1;
    for i := 0 to InternalSkins.Count - 1 do
      if InternalSkins[i].Name = SkinName then begin
        SkinIndex := i;
        break;
      end;
    if SkinIndex = -1 then Exit;

    SetLength(gd, 0);
    sg := InternalSkins[SkinIndex].GeneralData;

    // Global info
    SkinData.Version := InternalSkins[SkinIndex].Version;
    SkinData.Author := InternalSkins[SkinIndex].Author;
    SkinData.Description := InternalSkins[SkinIndex].Description;

    SkinData.Shadow1Color := InternalSkins[SkinIndex].Shadow1Color;
    SkinData.Shadow1Offset := InternalSkins[SkinIndex].Shadow1Offset;
    SkinData.Shadow1Blur := InternalSkins[SkinIndex].Shadow1Blur;
    SkinData.Shadow1Transparency := InternalSkins[SkinIndex].Shadow1Transparency;

    SkinData.BorderColor := InternalSkins[SkinIndex].BorderColor;

    CheckVersion;
    for i := 0  to sg.Count - 1 do begin
      l := Length(gd) + 1;
      SetLength(gd, l);

//General data
      gd[i].ClassName                   := sg[i].SectionName;

      gd[i].ParentClass                 := sg[i].ParentClass;

      gd[i].Color                       := sg[i].Color;
      gd[i].ReservedBoolean             := sg[i].ReservedBoolean;
      gd[i].GiveOwnFont                 := sg[i].GiveOwnFont;
      gd[i].GlowCount                   := sg[i].GlowCount;
      gd[i].GlowMargin                  := sg[i].GlowMargin;

//Panels data
      gd[i].Transparency                := sg[i].Transparency;
      gd[i].GradientPercent             := sg[i].GradientPercent;
      gd[i].ImagePercent                := sg[i].ImagePercent;
      gd[i].GradientData                := sg[i].GradientData;
      PrepareGradArray(sg[i].GradientData, gd[i].GradientArray);
// Buttons data
      gd[i].ShowFocus                   := sg[i].ShowFocus;
// ---- BtnEffects ----
      gd[i].FadingEnabled               := sg[i].FadingEnabled;
      gd[i].FadingIterations            := sg[i].FadingIterations;

// ---- PaintingOptions -----
      gd[i].HotColor                    := sg[i].HotColor;
      gd[i].HotTransparency             := sg[i].HotTransparency;
      gd[i].HotGradientPercent          := sg[i].HotGradientPercent;
      gd[i].HotGradientData             := sg[i].HotGradientData;
      PrepareGradArray(sg[i].HotGradientData, gd[i].HotGradientArray);
      gd[i].HotImagePercent             := sg[i].HotImagePercent;

      if (sg[i].HotFontColor <> '') and (WordCount(sg[i].HotFontColor, [' ']) = 5) then begin
        gd[i].HotFontColor[1]           := StrToInt(ExtractWord(1, sg[i].HotFontColor, [' ']));
        gd[i].HotFontColor[2]           := StrToInt(ExtractWord(2, sg[i].HotFontColor, [' ']));
        gd[i].HotFontColor[3]           := StrToInt(ExtractWord(3, sg[i].HotFontColor, [' ']));
        gd[i].HotFontColor[4]           := StrToInt(ExtractWord(4, sg[i].HotFontColor, [' ']));
        gd[i].HotFontColor[5]           := StrToInt(ExtractWord(5, sg[i].HotFontColor, [' ']));
      end
      else begin
        gd[i].HotFontColor[1]           := 0;
        gd[i].HotFontColor[2]           := -1;
        gd[i].HotFontColor[3]           := -1;
        gd[i].HotFontColor[4]           := -1;
        gd[i].HotFontColor[5]           := -1;
      end;
      if (sg[i].FontColor <> '') and (WordCount(sg[i].FontColor, [' ']) = 5) then begin
        gd[i].FontColor[1]              := StrToInt(ExtractWord(1, sg[i].FontColor, [' ']));
        gd[i].FontColor[2]              := StrToInt(ExtractWord(2, sg[i].FontColor, [' ']));
        gd[i].FontColor[3]              := StrToInt(ExtractWord(3, sg[i].FontColor, [' ']));
        gd[i].FontColor[4]              := StrToInt(ExtractWord(4, sg[i].FontColor, [' ']));
        gd[i].FontColor[5]              := StrToInt(ExtractWord(5, sg[i].FontColor, [' ']));
      end
      else begin
        gd[i].FontColor[1]             := 0;
        gd[i].FontColor[2]             := -1;
        gd[i].FontColor[3]             := -1;
        gd[i].FontColor[4]             := -1;
        gd[i].FontColor[5]             := -1;
      end;
    end;
  end;

  // CommonSection property in TsSkinManager
  if CommonSections.Count > 0 then begin
    Sections := TStringList.Create;
    GetIniSections(CommonSections, Sections);
    try for i := 0 to Sections.Count - 1 do begin
      l := Length(gd);
      gData.ClassName := '';
      SkinIndex := -1;
      for ParentIndex := 0 to l - 1 do begin
        if gd[ParentIndex].ClassName = Sections[i] then begin
          gData := gd[ParentIndex];
          SkinIndex := ParentIndex;
          break;
        end;
      end;
      Ini := nil;
      if gData.ClassName = '' then begin
        l := Length(gd) + 1;
        SetLength(gd, l);
        gData.ClassName := Sections[i];
        Ini := CommonSections;
        gData.ParentClass := UpperCase(acntUtils.ReadIniString(Ini, Sections, gData.ClassName, s_ParentClass, '-1'));

        ParentIndex := -1;
        for j := 0 to Length(gd) - 1 do begin
          if UpperCase(gd[j].ClassName) = gData.ParentClass then begin
            ParentIndex := j;
            break;
          end;
        end;
      end;
      // General data
      if Ini <> nil then begin
        int := acntUtils.ReadIniInteger(Ini, Sections, Sections[i], s_Color, -1);
        if int = -1 then if ParentIndex > -1 then gData.Color := gd[ParentIndex].Color else gData.Color := clWhite else gData.Color := int;
        s := UpperCase(acntUtils.ReadIniString(Ini, Sections, Sections[i], s_ReservedBoolean, CharQuest));
        if s = CharQuest then if ParentIndex > -1 then gData.ReservedBoolean := gd[ParentIndex].ReservedBoolean else gData.ReservedBoolean := False else gData.ReservedBoolean := s = s_TrueStr;
        s := UpperCase(acntUtils.ReadIniString(Ini, Sections, Sections[i], s_GiveOwnFont, CharQuest));
        if s = CharQuest then if ParentIndex > -1 then gData.GiveOwnFont := gd[ParentIndex].GiveOwnFont else gData.GiveOwnFont := False else gData.GiveOwnFont := s = s_TrueStr;

        int := acntUtils.ReadIniInteger(Ini, Sections, Sections[i], s_Glow, 0);
        if int = -1 then if ParentIndex > -1 then gData.GlowCount := gd[ParentIndex].GlowCount else gData.GlowCount := 0 else gData.GlowCount := int;
        int := acntUtils.ReadIniInteger(Ini, Sections, Sections[i], s_GlowMargin, 0);
        if int = -1 then if ParentIndex > -1 then gData.GlowMargin := gd[ParentIndex].GlowMargin else gData.GlowMargin := 0 else gData.GlowMargin := int;

        int := acntUtils.ReadIniInteger(Ini, Sections, Sections[i], s_FontColor, -1);
        if int = -1 then if ParentIndex > -1 then gData.FontColor[1] := gd[ParentIndex].FontColor[1] else gData.FontColor[1] := 0 else gData.FontColor[1] := int;
        int := acntUtils.ReadIniInteger(Ini, Sections, Sections[i], s_TCLeft, -1);
        if int = -1 then if ParentIndex > -1 then gData.FontColor[2] := gd[ParentIndex].FontColor[2] else gData.FontColor[2] := -1 else gData.FontColor[2] := int;
        int := acntUtils.ReadIniInteger(Ini, Sections, Sections[i], s_TCTop, -1);
        if int = -1 then if ParentIndex > -1 then gData.FontColor[3] := gd[ParentIndex].FontColor[3] else gData.FontColor[3] := -1 else gData.FontColor[3] := int;
        int := acntUtils.ReadIniInteger(Ini, Sections, Sections[i], s_TCRight, -1);
        if int = -1 then if ParentIndex > -1 then gData.FontColor[4] := gd[ParentIndex].FontColor[4] else gData.FontColor[4] := -1 else gData.FontColor[4] := int;
        int := acntUtils.ReadIniInteger(Ini, Sections, Sections[i], s_TCBottom, -1);
        if int = -1 then if ParentIndex > -1 then gData.FontColor[5] := gd[ParentIndex].FontColor[5] else gData.FontColor[5] := -1 else gData.FontColor[5] := int;

        int := acntUtils.ReadIniInteger(Ini, Sections, Sections[i], s_HotFontColor, -1);
        if int = -1 then if ParentIndex > -1 then gData.HotFontColor[1] := gd[ParentIndex].HotFontColor[1] else gData.HotFontColor[1] := 0 else gData.HotFontColor[1] := int;
        int := acntUtils.ReadIniInteger(Ini, Sections, Sections[i], s_HotTCLeft, -1);
        if int = -1 then if ParentIndex > -1 then gData.HotFontColor[2] := gd[ParentIndex].HotFontColor[2] else gData.HotFontColor[2] := -1 else gData.HotFontColor[2] := int;
        int := acntUtils.ReadIniInteger(Ini, Sections, Sections[i], s_HotTCTop, -1);
        if int = -1 then if ParentIndex > -1 then gData.HotFontColor[3] := gd[ParentIndex].HotFontColor[3] else gData.HotFontColor[3] := -1 else gData.HotFontColor[3] := int;
        int := acntUtils.ReadIniInteger(Ini, Sections, Sections[i], s_HotTCRight, -1);
        if int = -1 then if ParentIndex > -1 then gData.HotFontColor[4] := gd[ParentIndex].HotFontColor[4] else gData.HotFontColor[4] := -1 else gData.HotFontColor[4] := int;
        int := acntUtils.ReadIniInteger(Ini, Sections, Sections[i], s_HotTCBottom, -1);
        if int = -1 then if ParentIndex > -1 then gData.HotFontColor[5] := gd[ParentIndex].HotFontColor[5] else gData.HotFontColor[5] := -1 else gData.HotFontColor[5] := int;

  //Panels data
        int := acntUtils.ReadIniInteger(Ini, Sections, Sections[i], s_Transparency, -1);
        if int = -1 then if ParentIndex > -1 then gData.Transparency := gd[ParentIndex].Transparency else gData.Transparency := 0 else gData.Transparency := int;
        int := acntUtils.ReadIniInteger(Ini, Sections, Sections[i], s_GradientPercent, -1);
        if int = -1 then if ParentIndex > -1 then gData.GradientPercent := gd[ParentIndex].GradientPercent else gData.GradientPercent := 0 else gData.GradientPercent := int;
        int := acntUtils.ReadIniInteger(Ini, Sections, Sections[i], s_ImagePercent, -1);
        if int = -1 then if ParentIndex > -1 then gData.ImagePercent := gd[ParentIndex].ImagePercent else gData.ImagePercent := 0 else gData.ImagePercent := int;

        s := acntUtils.ReadIniString(Ini, Sections, Sections[i], s_GradientData, CharQuest);
        if s = CharQuest then if ParentIndex > -1 then gData.GradientData := gd[ParentIndex].GradientData else gData.GradientData := ' ' else gData.GradientData := s;
        if gData.GradientData <> ' ' then PrepareGradArray(gData.GradientData, gData.GradientArray);

  // Buttons data
        s := UpperCase(acntUtils.ReadIniString(Ini, Sections, Sections[i], s_ShowFocus, CharQuest));
        if s = CharQuest then begin
          if ParentIndex > -1 then gData.ShowFocus := gd[ParentIndex].ShowFocus else gData.ShowFocus := False
        end
        else gData.ShowFocus := s = s_TrueStr;
  // ---- BtnEffects ----
        s := UpperCase(acntUtils.ReadIniString(Ini, Sections, Sections[i], s_FadingEnabled, CharQuest));
        if s = CharQuest then begin
          if ParentIndex > -1 then gData.FadingEnabled := gd[ParentIndex].FadingEnabled else gData.FadingEnabled := False
        end else gData.FadingEnabled := s = s_TrueStr;
        int := acntUtils.ReadIniInteger(Ini, Sections, Sections[i], s_FadingIterations, -1);
        if int = -1 then if ParentIndex > -1 then gData.FadingIterations := gd[ParentIndex].FadingIterations else gData.FadingIterations := 5 else gData.FadingIterations := int;

  // ---- PaintingOptions -----
        int := acntUtils.ReadIniInteger(Ini, Sections, Sections[i], s_HotColor, -1);
        if int = -1 then if ParentIndex > -1 then gData.HotColor := gd[ParentIndex].HotColor else gData.HotColor := clWhite else gData.HotColor := int;
        int := acntUtils.ReadIniInteger(Ini, Sections, Sections[i], s_HotTransparency, -1);
        if int = -1 then if ParentIndex > -1 then gData.HotTransparency := gd[ParentIndex].HotTransparency else gData.HotTransparency := 0 else gData.HotTransparency := int;
        int := acntUtils.ReadIniInteger(Ini, Sections, Sections[i], s_HotGradientPercent, -1);
        if int = -1 then if ParentIndex > -1 then gData.HotGradientPercent := gd[ParentIndex].HotGradientPercent else gData.HotGradientPercent := 0 else gData.HotGradientPercent := int;
        s := acntUtils.ReadIniString(Ini, Sections, Sections[i], s_HotGradientData, CharQuest);
        if s = CharQuest then if ParentIndex > -1 then gData.HotGradientData := gd[ParentIndex].HotGradientData else gData.HotGradientData := '' else gData.HotGradientData := s;
        if gData.HotGradientData <> ' ' then PrepareGradArray(gData.HotGradientData, gData.HotGradientArray);
        int := acntUtils.ReadIniInteger(Ini, Sections, Sections[i], s_HotImagePercent, -1);
        if int = -1 then if ParentIndex > -1 then gData.HotImagePercent := gd[ParentIndex].HotImagePercent else gData.HotImagePercent := 0 else gData.HotImagePercent := int;

        if gData.ClassName <> '' then begin
          if SkinIndex > -1 then gd[SkinIndex] := gData else gd[l - 1] := gData;
          gData.ClassName := '';
        end;
      end;
    end;

    finally
      if Assigned(Sections) then begin
        while Sections.Count > 0 do begin
          if Sections.Objects[0] <> nil then TStringList(Sections.Objects[0]).Free;
          Sections.Delete(0);
        end;
        Sections.Free;
      end;
    end;
  end;
  ChangeSkinHue(Self, HueOffset);
  ChangeSkinSaturation(Self, Saturation);

  InitMaskIndexes;
end;

function TsSkinManager.MaskSize(MaskIndex: integer): TSize;
begin
  if ma[MaskIndex].Bmp = nil then begin
    Result.cx := WidthOfImage(ma[MaskIndex]);
    Result.cy := HeightOfImage(ma[MaskIndex]);
  end
  else begin // Deprecated code
    Result.cx := ma[MaskIndex].Bmp.Width div 3; 
    Result.cy := ma[MaskIndex].Bmp.Height div 2;
  end;
end;

function TsSkinManager.GetSkinIndex(const SkinSection: string): integer;
var
  i, l : integer;
begin
  Result := -1;
  if Self = nil then Exit;
  l := Length(gd);
  if l > 0 then for i := 0 to l - 1 do if (gd[i].ClassName = SkinSection) then begin
    Result := i;
    Exit;
  end;
end;

function TsSkinManager.GetMaskIndex(SkinIndex: integer; const SkinSection, mask: string): integer;
var
  i, l : integer;
  s : string;
begin
  Result := -1;
  if SkinIndex < 0 then Exit;
  if (SkinSection <> '') then begin
    l := Length(ma);
    for i := 0 to l - 1 do if (ma[i].ClassName = SkinSection) and (ma[i].PropertyName = mask) then begin
      Result := i;
      Exit;
    end;
    if SkinIndex > -1 then begin
      s := gd[SkinIndex].ParentClass;
      if (s <> '') and (SkinSection <> s) then begin
        i := GetSkinIndex(s);
        if i > -1 then Result := GetMaskIndex(i, s, mask);
      end;
    end;
  end;
end;

function TsSkinManager.GetPatternIndex(SkinIndex: integer; const SkinSection, pattern: string): integer;
var
  i, l : integer;
  s : string;
begin
  Result := -1;
  try
    if SkinIndex > -1 then begin
      l := Length(pa);
      if (l > 0) and (SkinSection <> '') then begin
        for i := 0 to l - 1 do if (pa[i].PropertyName = pattern) and (pa[i].ClassName = skinSection) then begin
          Result := i;
          Exit;
        end;
        s := gd[SkinIndex].ParentClass;
        if (s <> '') and (SkinSection <> s) then begin
          i := GetSkinIndex(s);
          if i > -1 then Result := GetPatternIndex(i, s, pattern);
        end;
      end;
    end;
  except
  end;
end;

procedure TsSkinManager.SetIsDefault(const Value: boolean);
begin
  if Value or (DefaultManager = nil) then begin
    FIsDefault := True;
    DefaultManager := Self;
    if Active then begin
      SendNewSkin;

      RepaintForms;
    end
    else SendRemoveSkin;
  end
  else FIsDefault := DefaultManager = Self;
end;

function TsSkinManager.GetIsDefault: boolean;
begin
  Result := DefaultManager = Self;
end;

function TsSkinManager.GetTextureIndex(SkinIndex: integer; const SkinSection, PropName: string): integer;
var
  i, l : integer;
begin
  Result := -1;
  try
    if SkinIndex > -1 then begin
      l := Length(ma);
      if (l > 0) and (SkinSection <> '') then begin
        for i := 0 to l - 1 do if (ma[i].ImgType = itisaTexture) and (ma[i].PropertyName = PropName) and (ma[i].ClassName = SkinSection) then begin
          Result := i;
          Exit;
        end;
        if (gd[SkinIndex].ParentClass <> '') and (SkinSection <> gd[SkinIndex].ParentClass) then begin
          i := GetSkinIndex(gd[SkinIndex].ParentClass);
          if i > -1 then Result := GetTextureIndex(i, gd[SkinIndex].ParentClass, PropName);
        end;
      end;
    end;
  except
  end;
end;

function TsSkinManager.GetMaskIndex(const SkinSection, mask: string): integer;
var
  i, l : integer;
begin
  Result := -1;
  if (SkinSection <> '') then begin
    l := Length(ma);
    for i := 0 to l - 1 do if (ma[i].ClassName = SkinSection) then begin
      if (ma[i].PropertyName = mask) then begin
        Result := i;
        Exit;
      end
    end;
  end;
end;

function TsSkinManager.MainWindowHook(var Message: TMessage): boolean;
var
  FMenuItem : TMenuItem;
  R : TRect;
  Wnd : hwnd;
  mi : TacMenuInfo;
//  sp : TsSkinProvider;
begin
{$IFDEF LOGGED}
  AddToLog(Message, BoolToStr(fGlobalFlag));
{$ENDIF}
  Result := False;
  if not SkinnedPopups then Exit;
  case Message.Msg of
    WM_DRAWMENUBORDER : {if (mi.Bmp <> nil) then }begin
      FMenuItem := TMenuItem(Message.LParam);
      if Assigned(FMenuItem) then begin
        if GetMenuItemRect(0, FMenuItem.Parent.Handle, 0{FMenuItem.MenuIndex}, R) or
             GetMenuItemRect(PopupList.Window, FMenuItem.Parent.Handle, 0{FMenuItem.MenuIndex}, R) then begin
          Wnd := WindowFromPoint(Point(r.Left + WidthOf(r) div 2, r.Top + HeightOf(r) div 2));
          if (Wnd <> 0) then begin
            mi := SkinableMenus.GetMenuInfo(FMenuItem, 0, 0, Wnd);
            if (mi.Bmp <> nil)
              then SkinableMenus.DrawWndBorder(Wnd, mi.Bmp);
          end;
        end;
      end;
      Result := True;
    end;
    WM_DRAWMENUBORDER2 : begin
      Wnd := HWND(Message.LParam);
      if (Wnd <> 0) then begin
        mi := SkinableMenus.GetMenuInfo(nil, 0, 0, Wnd);
        if (mi.Bmp <> nil) then SkinableMenus.DrawWndBorder(Wnd, mi.Bmp);
      end;
      Result := True;
    end;
    $031A{ <- WM_THEMECHANGED} : Result := True;
{    787 : if Application.MainForm <> nil then begin{
      try
        sp := TsSkinProvider(SendAMessage(Application.MainForm.Handle, AC_GETPROVIDER));
      except
        sp := nil;
      end;
      if sp <> nil then begin
        sp.DropSysMenu(Mouse.CursorPos.X, Mouse.CursorPos.Y) ;
        Result := True;
      end;
    end;}
  end;
end;

procedure TsSkinManager.ReloadSkin;
var
  s : string;
  sl : TStringList;
  i : integer;
begin
  if FActive then begin
    aSkinChanging := True;

    if Assigned(SkinFile) then FreeAndNil(SkinFile);
    s := GetFullSkinDirectory + '\' + SkinName + '\' + OptionsDatName;
    if FileExists(s) then begin // If used external skins
      SkinIsPacked := False;
      SkinFile := TMemIniFile.Create(s);
      SkinData.SkinPath := GetFullSkinDirectory + '\' + SkinName + '\';
    end
    else begin // If used internal skins
      SkinData.SkinPath := '';
      i := InternalSkins.IndexOf(FSkinName);
      if (i = -1) and (InternalSkins.Count > 0) then begin
        FSkinName := InternalSkins.Items[0].Name;
        i := 0;
      end
      else if (InternalSkins.Count < 1) then begin
        FActive := False;
        Exit;
      end;
      if InternalSkins.Items[i].PackedData.Size > 0 then begin // if packed
        if Assigned(sc) then FreeAndNil(sc);
        SkinIsPacked := True;
        sc := TacSkinConvertor.Create;
        sc.PackedData := InternalSkins.Items[i].PackedData;
        ExtractPackedData(sc);
        sc.PackedData := nil;

        sc.Options.Seek(0, 0);
        sl := TStringList.Create;
        sl.LoadFromStream(sc.Options);
        SkinFile := TMemIniFile.Create('');
        SkinFile.SetStrings(sl);
        FreeAndNil(sl);
      end
      else SkinIsPacked := False;
    end;
    LoadAllMasks;
    LoadAllPatterns;
    LoadAllGeneralData;
    if Assigned(SkinFile) then FreeAndNil(SkinFile);
    if Assigned(sc) then FreeAndNil(sc);
  end;
end;

function TsSkinManager.MaskWidthBottom(MaskIndex: integer): integer;
begin
  if ma[MaskIndex].WB > 0 then begin
    Result := ma[MaskIndex].WB;
  end
  else if ma[MaskIndex].BorderWidth > 0 then begin
    Result := ma[MaskIndex].BorderWidth;
  end
  else begin
    Result := HeightOf(ma[MaskIndex].R) div ((1 + ma[MaskIndex].MaskType) * 3);
  end
end;

function TsSkinManager.MaskWidthLeft(MaskIndex: integer): integer;
begin
  if ma[MaskIndex].WL > 0 then begin
    Result := ma[MaskIndex].WL;
  end
  else if ma[MaskIndex].BorderWidth > 0 then begin
    Result := ma[MaskIndex].BorderWidth;
  end
  else begin
    if ma[MaskIndex].ImageCount = 0 then ma[MaskIndex].ImageCount := 1;
    Result := WidthOf(ma[MaskIndex].R) div (ma[MaskIndex].ImageCount * 3);
  end
end;

function TsSkinManager.MaskWidthRight(MaskIndex: integer): integer;
begin
  if ma[MaskIndex].WR > 0 then begin
    Result := ma[MaskIndex].WR;
  end
  else if ma[MaskIndex].BorderWidth > 0 then begin
    Result := ma[MaskIndex].BorderWidth;
  end
  else begin
    Result := WidthOf(ma[MaskIndex].R) div (ma[MaskIndex].ImageCount * 3);
  end
end;

function TsSkinManager.MaskWidthTop(MaskIndex: integer): integer;
begin
  if ma[MaskIndex].WT > 0 then begin
    Result := ma[MaskIndex].WT;
  end
  else if ma[MaskIndex].BorderWidth > 0 then begin
    Result := ma[MaskIndex].BorderWidth;
  end
  else begin
    Result := HeightOf(ma[MaskIndex].R) div ((1 + ma[MaskIndex].MaskType) * 3);
  end
end;

procedure TsSkinManager.SetActiveControl(const Value: hwnd);
var
  OldHwnd : hwnd;
begin
  if FActiveControl <> Value then begin
    OldHwnd := FActiveControl;
    FActiveControl := Value;
    if OldHwnd <> 0 then SendAMessage(OldHwnd, AC_MOUSELEAVE, LongWord(Self));
    if FActiveControl <> 0 then SendAMessage(FActiveControl, AC_MOUSEENTER, LongWord(Self));
  end;
end;

procedure TsSkinManager.InstallHook;
var
  dwThreadID: DWORD;
begin
  if (csDesigning in ComponentState) or (DefaultManager <> Self) then Exit;
  if not GlobalHookInstalled then begin
    GlobalHookInstalled := True;
    if acSupportedList = nil then acSupportedList := TList.Create;
    dwThreadID := GetCurrentThreadId;
    HookCallback := SetWindowsHookEx(WH_CBT, SkinHookCBT, 0, dwThreadID);
  end;
end;

procedure TsSkinManager.UnInstallHook;
var
  i : integer;
begin
  if (csDesigning in ComponentState) or (DefaultManager <> Self) then Exit;
  if GlobalHookInstalled then begin
    ClearMnuArray;
    if HookCallBack <> 0 then UnhookWindowsHookEx(HookCallback);
    if acSupportedList <> nil then begin
      for i := 0 to acSupportedList.Count - 1 do if acSupportedList[i] <> nil then TObject(acSupportedList[i]).Free;
      FreeAndNil(acSupportedList);
    end;
    GlobalHookInstalled := False;
    HookCallback := 0;
  end;
end;

procedure TsSkinManager.ReloadPackedSkin;
var
  sl : TStringList;
begin
  if FActive then begin
    aSkinChanging := True;
    if Assigned(SkinFile) then FreeAndNil(SkinFile);
    sc := nil;
    LoadSkinFromFile(NormalDir(SkinDirectory) + SkinName + '.' + acSkinExt, sc);
    sc.Options.Seek(0, 0);
    sl := TStringList.Create;
    sl.LoadFromStream(sc.Options);
    SkinFile := TMemIniFile.Create('');
    SkinFile.SetStrings(sl);
    FreeAndNil(sl);

    SkinData.SkinPath := GetFullSkinDirectory + '\';

    LoadAllMasks;
    LoadAllPatterns;
    LoadAllGeneralData;
    if Assigned(SkinFile) then FreeAndNil(SkinFile);
    if Assigned(sc) then FreeAndNil(sc);
  end;
end;

procedure TsSkinManager.SetFSkinningRules(const Value: TacSkinningRules);
begin
  FSkinningRules := Value;
  UpdateCommonDlgs(Self);
end;

procedure TsSkinManager.CheckVersion;
begin
  if (csDesigning in ComponentState) and (SkinData.Version < CompatibleSkinVersion) then begin
    ShowMessage('You use an old version of "' + SkinName + '" skin, please update a skins to latest or link with AlphaControls support for upgrading of existing skin.'#13#10#13#10'This notification occurs in design-time only for your information and will not occur in real-time.');
  end
  else if SkinData.Version > MaxCompSkinVersion
    then ShowMessage('This version of skin have not complete supported by the AlphaControls package.'#13#10'Components must be updated to latest version for using this skin.');
end;

{ TsStoredSkins }

procedure TsStoredSkins.Assign(Source: TPersistent);
begin
end;

constructor TsStoredSkins.Create(AOwner: TsSkinManager);
begin
  inherited Create(TsStoredSkin);
  FOwner := AOwner;
end;

destructor TsStoredSkins.Destroy;
begin
  FOwner := nil;
  inherited Destroy;
end;

function TsStoredSkins.GetItem(Index: Integer): TsStoredSkin;
begin
  Result := TsStoredSkin(inherited GetItem(Index))
end;

function TsStoredSkins.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TsStoredSkins.IndexOf(const SkinName: string): integer;
var
  i : integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do begin
    if Items[i].Name = SkinName then begin
      Result := i;
      Exit;
    end;
  end;
end;

procedure TsStoredSkins.SetItem(Index: Integer; Value: TsStoredSkin);
begin
  inherited SetItem(Index, Value);
end;

{ TsStoredSkin }

constructor TsStoredSkin.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  FMasterBitmap := TBitmap.Create;
  FImages := TsSkinImages.Create(Self);
  FPatterns := TsSkinPatterns.Create(Self);
  FGeneralData := TsSkinGenerals.Create(Self);

  PackedData := TMemoryStream.Create;

  FShadow1Blur := -1;
  FBorderColor := clFuchsia;
end;

procedure TsStoredSkin.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, True);
end;

destructor TsStoredSkin.Destroy;
begin
  if Assigned(FImages) then FreeAndNil(FImages);
  if Assigned(FPatterns) then FreeAndNil(FPatterns);
  if Assigned(FGeneralData) then FreeAndNil(FGeneralData);
  if Assigned(FMasterBitmap) then FreeAndNil(FMasterBitmap);
  FreeAndNil(PackedData);
  inherited Destroy;
end;

procedure TsStoredSkin.LoadFromIni(gd : TsSkinGenerals; sf: TMemIniFile);
var
  Sections : TStringList;
  i, len : integer;
  OldSeparator : char;

  function FindString(const ClassName, PropertyName, DefaultValue : string) : string; var s : string; begin
    Result := sf.ReadString(ClassName, PropertyName, CharQuest);
    if Result = CharQuest then begin
      s := sf.ReadString(ClassName, s_ParentClass, CharQuest);
      if (s <> CharQuest) and (s <> '') and (s <> ClassName) then Result := FindString(s, PropertyName, CharQuest);
    end;
    if Result = CharQuest then Result := DefaultValue;
  end;

  function FindInteger(const ClassName, PropertyName : string; DefaultValue : integer) : integer; var s : string; begin
    Result := sf.ReadInteger(ClassName, PropertyName, -1);
    if Result = -1 then begin
      s := sf.ReadString(ClassName, s_ParentClass, CharQuest);
      if (s <> CharQuest) and (s <> '') and (s <> ClassName) then Result := FindInteger(s, PropertyName, -1);
    end;
    if Result = -1 then Result := DefaultValue;
  end;
begin
  if sf <> nil then begin
    OldSeparator := DecimalSeparator;
    DecimalSeparator := '.';
    TsStoredSkins(Collection).FOwner.SkinData.Version := sf.ReadFloat(s_GlobalInfo, s_Version, 0);
    Version := sf.ReadFloat(s_GlobalInfo, s_Version, 0);
    DecimalSeparator := OldSeparator;
    Author := sf.ReadString(s_GlobalInfo, s_Author, '');
    Description := sf.ReadString(s_GlobalInfo, s_Description, '');

    Shadow1Color := StrToInt(sf.ReadString(s_GlobalInfo, s_Shadow1Color, '0'));
    Shadow1Offset := StrToInt(sf.ReadString(s_GlobalInfo, s_Shadow1Offset, '1'));
    Shadow1Blur := StrToInt(sf.ReadString(s_GlobalInfo, s_Shadow1Blur, '-1'));
    Shadow1Transparency := StrToInt(sf.ReadString(s_GlobalInfo, s_Shadow1Transparency, '50'));

    BorderColor := sf.ReadInteger(s_GlobalInfo, s_BorderColor, clBlack);

    Sections := TStringList.Create;
    try
    sf.ReadSections(Sections);
    len := Sections.Count - 1;
    for i := 0 to len do begin
      gd.Add;
//General data
      gd[i].SectionName := Sections[i];
      gd[i].ParentClass := sf.ReadString(Sections[i], s_ParentClass, '');

      gd[i].Color := ColorToRGB(FindInteger(Sections[i], s_Color, clWhite));
      gd[i].ReservedBoolean := UpperCase(FindString(Sections[i], s_ReservedBoolean, s_FalseStr)) = s_TrueStr;
      gd[i].GiveOwnFont := UpperCase(FindString(Sections[i], s_GiveOwnFont, s_FalseStr)) = s_TrueStr;
      gd[i].GlowCount := FindInteger(Sections[i], s_Glow, 0);
      gd[i].GlowMargin := FindInteger(Sections[i], s_GlowMargin, 0);

      gd[i].FontColor := FindString(Sections[i], s_FontColor, '0') + ' ' +
                         FindString(Sections[i], s_TCLeft  , '-1') + ' ' +
                         FindString(Sections[i], s_TCTop   , '-1') + ' ' +
                         FindString(Sections[i], s_TCRight , '-1') + ' ' +
                         FindString(Sections[i], s_TCBottom, '-1');

      gd[i].HotFontColor := FindString(Sections[i], s_HotFontColor, '0') + ' ' +
                            FindString(Sections[i], s_HotTCLeft  , '-1') + ' ' +
                            FindString(Sections[i], s_HotTCTop   , '-1') + ' ' +
                            FindString(Sections[i], s_HotTCRight , '-1') + ' ' +
                            FindString(Sections[i], s_HotTCBottom, '-1');


      gd[i].Transparency := FindInteger(Sections[i], s_Transparency, 0);
      gd[i].GradientPercent := FindInteger(Sections[i], s_GradientPercent, 0);
      gd[i].ImagePercent := FindInteger(Sections[i], s_ImagePercent, 0);
      gd[i].GradientData := FindString(Sections[i], s_GradientData, ' ');
      gd[i].ShowFocus := UpperCase(FindString(Sections[i], s_ShowFocus, s_FalseStr)) = s_TrueStr;
      gd[i].FadingEnabled := UpperCase(FindString(Sections[i], s_FadingEnabled, s_FalseStr)) = s_TrueStr;
      gd[i].FadingIterations := FindInteger(Sections[i], s_FadingIterations, 5);

      gd[i].HotColor := ColorToRGB(TColor(FindInteger(Sections[i], s_HotColor, clWhite)));
      gd[i].HotTransparency := FindInteger(Sections[i], s_HotTransparency, 0);

      gd[i].HotGradientPercent := FindInteger(Sections[i], s_HotGradientPercent, 0);
      gd[i].HotGradientData := FindString(Sections[i], s_HotGradientData, ' ');
      gd[i].HotImagePercent := FindInteger(Sections[i], s_HotImagePercent, 0);

    end;

    finally
      if Assigned(Sections) then FreeAndNil(Sections);
    end;
  end
end;

procedure TsStoredSkin.LoadSkin(sf : TMemIniFile);
var
  Sections, Values : TStringList;
  i, j, n : integer;
  s, subs, st : string;
  WasFound : boolean;
begin
  GeneralData.Add;
  LoadFromIni(GeneralData, sf);
  Images.Clear;

  // Reading of the MasterBitmap if exists
  s := sf.ReadString(s_GLOBALINFO, s_MASTERBITMAP, '');
  if (pos(':', s) < 1) then s := ExtractFilePath(sf.FileName) + s;
  if (s <> '') and FileExists(s) then FMasterBitmap.LoadFromFile(s);

  Sections := TStringList.Create;
  Values := TStringList.Create;
  try
    sf.ReadSections(Sections);
    for i := 0 to Sections.Count - 1 do begin
      sf.ReadSection(Sections[i], Values);
      for j := 0 to Values.Count - 1 do begin
        if (Sections[i] = s_GLOBALINFO) then begin // Check for MASTERBITMAP property
          s := Values[j];
          if (s = s_MasterBitmap) then Continue;
        end; // v4.66
        s := sf.ReadString(Sections[i], Values[j], '-');
        if (s <> '') then case s[1] of // New format v4.66
          TexChar : begin
            Images.Add;

            Images[Images.Count - 1].PropertyName := UpperCase(Values[j]);
            Images[Images.Count - 1].SectionName := UpperCase(Sections[i]);

            Images[Images.Count - 1].Left  := StrToInt(Copy(s, 2, 4));
            Images[Images.Count - 1].Top   := StrToInt(Copy(s, 7, 4));
            Images[Images.Count - 1].Right := StrToInt(Copy(s, 12, 4));
            Images[Images.Count - 1].Bottom:= StrToInt(Copy(s, 17, 4));

            Images[Images.Count - 1].StretchMode := StrToInt(Copy(s, 22, 2));
            Images[Images.Count - 1].ImgType := itisaTexture;
            Images[Images.Count - 1].ImageCount := 1;
            if Length(s) > 24
              then Images[Images.Count - 1].MaskType := StrToInt(Copy(s, 25, 1))
              else Images[Images.Count - 1].MaskType := 0;
            Continue;
          end;
          CharGlyph : begin
            Images.Add;

            Images[Images.Count - 1].PropertyName := Values[j];
            Images[Images.Count - 1].SectionName := Sections[i];
            Images[Images.Count - 1].ImgType := itisaGlyph;
            Images[Images.Count - 1].Left  := StrToInt(Copy(s, 2, 4));
            Images[Images.Count - 1].Top   := StrToInt(Copy(s, 7, 4));
            Images[Images.Count - 1].Right := StrToInt(Copy(s, 12, 4));
            Images[Images.Count - 1].Bottom:= StrToInt(Copy(s, 17, 4));

            Images[Images.Count - 1].ImageCount := StrToInt(Copy(s, 22, 1));
            Images[Images.Count - 1].MaskType := StrToInt(Copy(s, 24, 1));
          end;
          CharMask : begin
            Images.Add;

            Images[Images.Count - 1].PropertyName := Values[j];
            Images[Images.Count - 1].SectionName := Sections[i];
            Images[Images.Count - 1].Left  := StrToInt(Copy(s, 2, 4));
            Images[Images.Count - 1].Top   := StrToInt(Copy(s, 7, 4));
            Images[Images.Count - 1].Right := StrToInt(Copy(s, 12, 4));
            Images[Images.Count - 1].Bottom:= StrToInt(Copy(s, 17, 4));

            Images[Images.Count - 1].ImgType := itisaBorder;
            Images[Images.Count - 1].WL := StrToInt(Copy(s, 22, 4));
            Images[Images.Count - 1].WT := StrToInt(Copy(s, 27, 4));
            Images[Images.Count - 1].WR := StrToInt(Copy(s, 32, 4));
            Images[Images.Count - 1].WB := StrToInt(Copy(s, 37, 4));
            Images[Images.Count - 1].ImageCount := StrToInt(Copy(s, 42, 1));
            Images[Images.Count - 1].MaskType := StrToInt(Copy(s, 44, 1));
            Images[Images.Count - 1].StretchMode := StrToInt(Copy(s, 46, 1));
            Images[Images.Count - 1].BorderWidth := StrToInt(Copy(s, 48, 1));
            with Images[Images.Count - 1] do begin
              if WL + WT + WR + WB = 0 then begin // If BorderWidths are not defined
                if BorderWidth <> 0 then begin
                  WL := BorderWidth;
                  WT := BorderWidth;
                  WR := BorderWidth;
                  WB := BorderWidth;
                end
                else begin
                  WL := (Right - Left) div (ImageCount * 3);
                  WT := (Bottom - Top) div ((1 + MaskType) * 3);
                  WR := WL;
                  WB := WT;
                end;
              end;
            end;
          end;
          CharExt : begin
            subs := ExtractWord(1, s, [CharExt]);
            if (pos('.BMP', subs) > 0) then begin    // Else if bitmap assigned  Need for optimize when several similar bitmaps loaded
              if (pos(':', subs) < 1) then subs := ExtractFilePath(sf.FileName) + subs;
              if FileExists(subs) then begin
                Images.Add;

                Images[Images.Count - 1].Name := ExtractFileName(subs);
                try
                  Images[Images.Count - 1].Image.LoadFromFile(subs);
                finally
                  Images[Images.Count - 1].PropertyName := Values[j];
                  Images[Images.Count - 1].SectionName := Sections[i];

                  Images[Images.Count - 1].ImgType := acImgTypes[StrToInt(ExtractWord(4, s, [CharExt]))];
                  Images[Images.Count - 1].ImageCount := StrToInt(ExtractWord(2, s, [CharExt]));
                  Images[Images.Count - 1].MaskType := StrToInt(ExtractWord(3, s, [CharExt]));
                  Images[Images.Count - 1].Left := 0;
                  Images[Images.Count - 1].Top := 0;
                  Images[Images.Count - 1].Right := Images[Images.Count - 1].Image.Width;
                  Images[Images.Count - 1].Bottom := Images[Images.Count - 1].Image.Height;

                  if WordCount(s, [CharExt]) > 4 then begin // if border widths are defined
                    Images[Images.Count - 1].WL := StrToInt(ExtractWord(5, s, [CharExt]));
                    Images[Images.Count - 1].WT := StrToInt(ExtractWord(6, s, [CharExt]));
                    Images[Images.Count - 1].WR := StrToInt(ExtractWord(7, s, [CharExt]));
                    Images[Images.Count - 1].WB := StrToInt(ExtractWord(8, s, [CharExt]));
                  end
                  else begin
                    Images[Images.Count - 1].WL := (Images[Images.Count - 1].Right - Images[Images.Count - 1].Left) div (Images[Images.Count - 1].ImageCount * 3);
                    Images[Images.Count - 1].WT := (Images[Images.Count - 1].Bottom - Images[Images.Count - 1].Top) div ((1 + Images[Images.Count - 1].MaskType) * 3);
                    Images[Images.Count - 1].WR := Images[Images.Count - 1].WL;
                    Images[Images.Count - 1].WB := Images[Images.Count - 1].WT;
                  end;
                end;
                if Images[Images.Count - 1].Image.Width < 1 then begin
                  Images.Delete(Images.Count - 1);
                end;
              end;
            end;
          end
          else begin
            if (pos(CharDiez, s) > 0) then begin       // Reading of the MasterBitmap item
              if s = CharDiez then Continue;
              Images.Add;

              Images[Images.Count - 1].PropertyName := UpperCase(Values[j]);
              Images[Images.Count - 1].SectionName := UpperCase(Sections[i]);

              subs := ExtractWord(2, s, [')', '(', ' ']);
              Images[Images.Count - 1].Left  := StrToInt(ExtractWord(1, subs, [',', ' ']));
              Images[Images.Count - 1].Top   := StrToInt(ExtractWord(2, subs, [',', ' ']));
              Images[Images.Count - 1].Right := StrToInt(ExtractWord(3, subs, [',', ' ']));
              Images[Images.Count - 1].Bottom:= StrToInt(ExtractWord(4, subs, [',', ' ']));

              subs := ExtractWord(3, s, [')', '(', ' ']);
              Images[Images.Count - 1].ImageCount := StrToInt(ExtractWord(1, subs, [',', ' ']));
              if Images[Images.Count - 1].ImageCount < 1 then Images[Images.Count - 1].ImageCount := 1;
              Images[Images.Count - 1].MaskType := StrToInt(ExtractWord(2, subs, [',', ' ']));
              if Images[Images.Count - 1].MaskType < 0 then Images[Images.Count - 1].MaskType := 0;

              // BorderWidth
              st := ExtractWord(3, subs, [',', ' ']);
              if st <> '' then begin
                Images[Images.Count - 1].BorderWidth := StrToInt(st);
                if Images[Images.Count - 1].BorderWidth < 0 then Images[Images.Count - 1].BorderWidth := 0;
              end;

              // StretchMode
              st := ExtractWord(4, subs, [',', ' ']);
              if st <> ''
                then Images[Images.Count - 1].StretchMode := StrToInt(st)
                else Images[Images.Count - 1].StretchMode := 1;
              Continue;
            end;
            s := AnsiUpperCase(s);
            if (pos('.BMP', s) > 0) then begin
              if (pos(':', s) < 1) then begin
                s := ExtractFilePath(sf.FileName) + s;
              end;
              if FileExists(s) then begin
                Images.Add;       
                Images[Images.Count - 1].PropertyName := '';
                Images[Images.Count - 1].SectionName := '';
                Images[Images.Count - 1].Name := ExtractFileName(s);
                Images[Images.Count - 1].ImageCount := 0;
                try
                  Images[Images.Count - 1].Image.LoadFromFile(s);
                finally
                  Images[Images.Count - 1].PropertyName := Values[j];
                  Images[Images.Count - 1].SectionName := Sections[i];
                end;
                if Images[Images.Count - 1].Image.Width < 1 then begin
                  Images.Delete(Images.Count - 1);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
    // Read patterns
    Sections.Clear;
    Values.Clear;
    sf.ReadSections(Sections);
    for i := 0 to Sections.Count - 1 do begin
      sf.ReadSection(Sections[i], Values);
      for j := 0 to Values.Count - 1 do begin
        s := sf.ReadString(Sections[i], Values[j], '-');
        s := AnsiUpperCase(s);

        if (pos('.JPG', s) > 0) or (pos('.JPEG', s) > 0) then begin
          if (pos(':', s) < 1) then s := ExtractFilePath(sf.FileName) + s;
          if FileExists(s) then begin
            Patterns.Add;
            Patterns[Patterns.Count - 1].Name := ExtractFileName(s);
            try
              WasFound := False;
              if Patterns.Count > 1 then for n := 0 to Patterns.Count - 1 do begin
                if (Patterns[n].Name = Patterns[Patterns.Count - 1].Name) and not Patterns[n].Image.Empty then begin
                  WasFound := True;
                  Break
                end;
              end;
              if not Wasfound then begin
                Patterns[Patterns.Count - 1].Image.LoadFromFile(s);
              end;
            finally
              Patterns[Patterns.Count - 1].PropertyName := Values[j];
              Patterns[Patterns.Count - 1].SectionName := Sections[i];
            end;
          end;
        end;
      end;
    end;
  finally
    if Assigned(Values) then FreeAndNil(Values);
    if Assigned(Sections) then FreeAndNil(Sections);
  end;
end;

procedure TsStoredSkin.ReadData(Reader: TStream);
begin
  PackedData.LoadFromStream(Reader);
end;

procedure TsStoredSkin.SetGeneralData(const Value: TsSkinGenerals);
begin
  FGeneralData.Assign(Value);
end;

procedure TsStoredSkin.SetImages(const Value: TsSkinImages);
begin
  FImages.Assign(Value);
end;

procedure TsStoredSkin.SetName(const Value: string);
begin
  if FName <> Value then begin
    FName := Value;
  end;
end;

procedure TsStoredSkin.SetPatterns(const Value: TsSkinPatterns);
begin
  FPatterns.Assign(Value);
end;

procedure TsStoredSkin.WriteData(Writer: TStream);
begin
  PackedData.SaveToStream(Writer);
end;

{ TsSkinImages }

constructor TsSkinImages.Create(AOwner: TsStoredSkin);
begin
  inherited Create(TsSkinImage);
  FOwner := AOwner;
end;

destructor TsSkinImages.Destroy;
begin
  FOwner := nil;
  inherited Destroy;
end;

function TsSkinImages.GetItem(Index: Integer): TsSkinImage;
begin
  Result := TsSkinImage(inherited GetItem(Index))
end;

function TsSkinImages.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TsSkinImages.SetItem(Index: Integer; Value: TsSkinImage);
begin
  inherited SetItem(Index, Value);
end;

procedure TsSkinImages.Update(Item: TCollectionItem);
begin
  inherited;
end;

{ TsSkinImage }

constructor TsSkinImage.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FImage := TBitmap.Create;
end;

destructor TsSkinImage.Destroy;
begin
  if Assigned(FImage) then FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TsSkinImage.SetClassName(const Value: string);
begin
  FClassName := Value;
end;

procedure TsSkinImage.SetImage(const Value: TBitmap);
begin
  FImage.Assign(Value);
end;

procedure TsSkinImage.SetName(const Value: string);
begin
  if FName <> Value then begin
    FName := Value;
  end;
end;

procedure TsSkinImage.SetPropertyName(const Value: string);
begin
  FPropertyName := Value;
end;

{ TsSkinGeneral }

procedure TsSkinGeneral.Assign(Source: TPersistent);
begin
  inherited;
end;

constructor TsSkinGeneral.Create(Collection: TCollection);
begin
  inherited;
end;

destructor TsSkinGeneral.Destroy;
begin
  inherited;
end;

procedure TsSkinGeneral.SetFadingEnabled(const Value: boolean);
begin
  FFadingEnabled := Value;
end;

procedure TsSkinGeneral.SetFadingIterations(const Value: integer);
begin
  FFadingIterations := Value;
end;

procedure TsSkinGeneral.SetFontColor(const Value: string);
begin
  FFontColor := Value;
end;

procedure TsSkinGeneral.SetGradientData(const Value: string);
begin
  FGradientData := Value;
end;

procedure TsSkinGeneral.SetGradientPercent(const Value: integer);
begin
  FGradientPercent := Value;
end;

procedure TsSkinGeneral.SetHotFontColor(const Value: string);
begin
  FHotFontColor := Value;
end;

procedure TsSkinGeneral.SetHotGradientData(const Value: string);
begin
  FHotGradientData := Value;
end;

procedure TsSkinGeneral.SetHotGradientPercent(const Value: integer);
begin
  FHotGradientPercent := Value;
end;

procedure TsSkinGeneral.SetHotImagePercent(const Value: integer);
begin
  FHotImagePercent := Value;
end;

procedure TsSkinGeneral.SetHotColor(const Value: TColor);
begin
  FHotColor := Value;
end;

procedure TsSkinGeneral.SetHotTransparency(const Value: integer);
begin
  FHotTransparency := Value;
end;

procedure TsSkinGeneral.SetImagePercent(const Value: integer);
begin
  FImagePercent := Value;
end;

procedure TsSkinGeneral.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TsSkinGeneral.SetColor(const Value: TColor);
begin
  FColor := Value;
end;

procedure TsSkinGeneral.SetTransparency(const Value: integer);
begin
  FTransparency := Value;
end;

procedure TsSkinGeneral.SetParentClass(const Value: string);
begin
  FParentClass := Value;
end;

procedure TsSkinGeneral.SetReservedBoolean(const Value: boolean);
begin
  FReservedBoolean := Value;
end;

procedure TsSkinGeneral.SetSectionName(const Value: string);
begin
  FSectionName := Value;
end;

procedure TsSkinGeneral.SetShowFocus(const Value: boolean);
begin
  FShowFocus := Value;
end;

procedure TsSkinGeneral.SetGiveOwnFont(const Value: boolean);
begin
  FGiveOwnFont := Value;
end;

procedure TsSkinGeneral.SetGlowCount(const Value: integer);
begin
  FGlowCount := Value;
end;

procedure TsSkinGeneral.SetGlowMargin(const Value: integer);
begin
  FGlowMargin := Value;
end;

{ TsSkinGenerals }

constructor TsSkinGenerals.Create(AOwner: TsStoredSkin);
begin
  inherited Create(TsSkinGeneral);
  FOwner := AOwner;
end;

destructor TsSkinGenerals.Destroy;
begin
  FOwner := nil;
  inherited Destroy;
end;

function TsSkinGenerals.GetItem(Index: Integer): TsSkinGeneral;
begin
  Result := TsSkinGeneral(inherited GetItem(Index))
end;

function TsSkinGenerals.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TsSkinGenerals.SetItem(Index: Integer; Value: TsSkinGeneral);
begin
  inherited SetItem(Index, Value);
end;

procedure TsSkinGenerals.Update(Item: TCollectionItem);
begin
  inherited;
end;

{ TsSkinPattern }

constructor TsSkinPattern.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FImage := TJpegImage.Create;
end;

destructor TsSkinPattern.Destroy;
begin
  if Assigned(FImage) then FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TsSkinPattern.SetClassName(const Value: string);
begin
  FClassName := Value;
end;

procedure TsSkinPattern.SetImage(const Value: TJpegImage);
begin
  FImage.Assign(Value);
end;

procedure TsSkinPattern.SetName(const Value: string);
begin
  if FName <> Value then begin
    FName := Value;
  end;
end;

procedure TsSkinPattern.SetPropertyName(const Value: string);
begin
  FPropertyName := Value;
end;

{ TsSkinPatterns }

constructor TsSkinPatterns.Create(AOwner: TsStoredSkin);
begin
  inherited Create(TsSkinPattern);
  FOwner := AOwner;
end;

destructor TsSkinPatterns.Destroy;
begin
  FOwner := nil;
  inherited Destroy;
end;

function TsSkinPatterns.GetItem(Index: Integer): TsSkinPattern;
begin
  Result := TsSkinPattern(inherited GetItem(Index))
end;

function TsSkinPatterns.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TsSkinPatterns.SetItem(Index: Integer; Value: TsSkinPattern);
begin
  inherited SetItem(Index, Value);
end;

procedure TsSkinPatterns.Update(Item: TCollectionItem);
begin
  inherited;
end;

{ TacAnimEffects }

constructor TacAnimEffects.Create;
begin
  FButtons := TacBtnEffects.Create;
  FButtons.Manager := Manager;
  FDialogShow := TacDialogShow.Create;
  FFormShow := TacFormShow.Create;
  FPageChange := TacPageChange.Create;
  FSkinChanging := TacSkinChanging.Create;
end;

destructor TacAnimEffects.Destroy;
begin
  FreeAndNil(FButtons);
  FreeAndNil(FDialogShow);
  FreeAndNil(FFormShow);
  FreeAndNil(FPageChange);
  FreeAndNil(FSkinChanging);
  inherited;
end;

{ TacBtnEffects }

constructor TacBtnEffects.Create;
begin
  FEvents := [beMouseEnter, beMouseLeave, beMouseDown, beMouseUp]
end;
{
function TacBtnEffects.GetEvents: TacBtnEvents;
begin
  if Manager.IsDefault then begin
    Result := [];
    if aeMouseEnter in GlobalAnimateEvents then Result := Result + [beMouseEnter];
    if aeMouseLeave in GlobalAnimateEvents then Result := Result + [beMouseLeave];
    if aeMouseDown  in GlobalAnimateEvents then Result := Result + [beMouseDown];
    if aeMouseUp    in GlobalAnimateEvents then Result := Result + [beMouseUp];
  end
  else Result := FEvents;
end;}
{
procedure TacBtnEffects.SetEvents(const Value: TacBtnEvents);
begin
  if FEvents <> Value then begin
    FEvents := Value;
    if Manager.IsDefault then begin
      GlobalAnimateEvents := [];
      if beMouseEnter in FEvents then GlobalAnimateEvents := GlobalAnimateEvents + [aeMouseEnter];
      if beMouseLeave in FEvents then GlobalAnimateEvents := GlobalAnimateEvents + [aeMouseLeave];
      if beMouseDown  in FEvents then GlobalAnimateEvents := GlobalAnimateEvents + [aeMouseDown];
      if beMouseUp    in FEvents then GlobalAnimateEvents := GlobalAnimateEvents + [aeMouseUp];
    end;
  end;
end;}

{ TacFormAnimation }

constructor TacFormAnimation.Create;
begin
  FActive := True;
  FTime := 0;
end;

{ TacDialogShow }

constructor TacDialogShow.Create;
begin
  inherited;
  FTime := 0;
end;

{ TacSkinChanging }

constructor TacSkinChanging.Create;
begin
  inherited;
  FTime := 100;
end;

{ ThirdPartyList }

function ThirdPartyList.GetString(const Index: Integer): string;
begin
  case Index of
    ord(tpEdit)        : Result := FThirdEdits       ;
    ord(tpButton)      : Result := FThirdButtons     ;
    ord(tpBitBtn)      : Result := FThirdBitBtns     ;
    ord(tpCheckBox)    : Result := FThirdCheckBoxes  ;
    ord(tpGroupBox)    : Result := FThirdGroupBoxes  ;
    ord(tpListView)    : Result := FThirdListViews   ;
    ord(tpPanel)       : Result := FThirdPanels      ;
    ord(tpGrid)        : Result := FThirdGrids       ;
    ord(tpTreeView)    : Result := FThirdTreeViews   ;
    ord(tpComboBox)    : Result := FThirdComboBoxes  ;
    ord(tpwwEdit)      : Result := FThirdWWEdits     ;
    ord(tpVirtualTree) : Result := FThirdVirtualTrees;
    ord(tpGridEh)      : Result := FThirdGridEh      ;
    ord(tpPageControl) : Result := FThirdPageControl ;
    ord(tpTabControl)  : Result := FThirdTabControl  ;
    ord(tpToolBar)     : Result := FThirdToolBar     ;
    ord(tpStatusBar)   : Result := FThirdStatusBar   ;
  end
end;

procedure ThirdPartyList.SetString(const Index: Integer; const Value: string);
begin
  case Index of
    ord(tpEdit)        :   FThirdEdits         := Value;
    ord(tpButton)      :   FThirdButtons       := Value;
    ord(tpBitBtn)      :   FThirdBitBtns       := Value;
    ord(tpCheckBox)    :   FThirdCheckBoxes    := Value;
    ord(tpGroupBox)    :   FThirdGroupBoxes    := Value;
    ord(tpListView)    :   FThirdListViews     := Value;
    ord(tpPanel)       :   FThirdPanels        := Value;
    ord(tpGrid)        :   FThirdGrids         := Value;
    ord(tpTreeView)    :   FThirdTreeViews     := Value;
    ord(tpComboBox)    :   FThirdComboBoxes    := Value;
    ord(tpwwEdit)      :   FThirdWWEdits       := Value;
    ord(tpVirtualTree) :   FThirdVirtualTrees  := Value;
    ord(tpGridEh)      :   FThirdGridEh        := Value;
    ord(tpPageControl) :   FThirdPageControl   := Value;
    ord(tpTabControl)  :   FThirdTabControl    := Value;
    ord(tpToolBar)     :   FThirdToolBar       := Value;
    ord(tpStatusBar)   :   FThirdStatusBar     := Value;
  end
end;

initialization
  OSVersionInfo.dwOSVersionInfoSize := sizeof(OSVersionInfo);
  GetVersionEx(OSVersionInfo);
  IsNT := OSVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT;

finalization

end.
