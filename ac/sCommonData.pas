unit sCommonData;
{$I sDefs.inc}

interface

uses
  windows, Graphics, Classes, Controls, SysUtils, StdCtrls,  Dialogs, sSkinManager, acntUtils,
  Forms, Messages, sConst, extctrls, IniFiles, sLabel;

type

  TsCommonData = class(TPersistent)
{$IFNDEF NOTFORHELP}
  private
    FSkinSection: TsSkinSection;
    FCustomFont: boolean;
    FCustomColor: boolean;
    procedure SetSkinSection(const Value: string);
    function GetUpdating: boolean;
    procedure SetUpdating(const Value: boolean);
    procedure SetCustomColor(const Value: boolean);
    procedure SetCustomFont(const Value: boolean);
    function GetSkinManager: TsSkinManager;
    procedure SetSkinManager(const Value: TsSkinManager);
  public
    GlowID : integer;
    FUpdating : boolean;
    FSkinManager : TsSkinManager;
    BorderIndex : integer;
    SkinIndex : integer;
    Texture : integer;
    HotTexture : integer;
    GraphControl : pointer;

    UrgentPainting : boolean;
    BGChanged : boolean;
    HalfVisible : boolean;

    FOwnerControl : TControl;
    FOwnerObject : TObject;
    FCacheBmp : TBitmap;
    FRegion : hrgn;

    COC : integer;
    FFocused : boolean;
    FMouseAbove: Boolean;

    CtrlSkinState : word;
    BGType : word;
    property Updating : boolean read GetUpdating write SetUpdating default False;
    procedure InitCacheBmp;
    constructor Create(AOwner : TObject; CreateCacheBmp : boolean);
    destructor Destroy; override;
    procedure UpdateIndexes;
    procedure Loaded;

    function RepaintIfMoved : boolean;
    function ParentTextured : boolean;
    function ManagerStored : boolean;
{$ENDIF} // NOTFORHELP
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Invalidate;
    function Skinned(CheckSkinActive : boolean = False) : boolean;
  published
    property CustomColor : boolean read FCustomColor write SetCustomColor default False;
    property CustomFont : boolean read FCustomFont write SetCustomFont default False;
    property SkinManager : TsSkinManager read GetSkinManager write SetSkinManager stored ManagerStored;
    property SkinSection : TsSkinSection read FSkinSection write SetSkinSection;
  end;

  TsBoundLabel = class(TPersistent)
{$IFNDEF NOTFORHELP}
  private
    FMaxWidth: integer;
    FText: acString; // gralex
    FLayout: TsCaptionLayout;
    FFont: TFont;
    FIndent: integer;
    procedure SetActive(const Value: boolean);
    procedure SetLayout(const Value: TsCaptionLayout);
    procedure SetMaxWidth(const Value: integer);
    procedure SetText(const Value: acString); // gralex
    procedure SetFont(const Value: TFont);
    procedure SetIndent(const Value: integer);
    function GetFont: TFont;
    procedure UpdateAlignment;
    function GetUseSkin: boolean;
    procedure SetUseSkin(const Value: boolean);
  public
    FActive: boolean;
    FTheLabel : TsEditLabel;
    FCommonData : TsCommonData;
    procedure AlignLabel;
    constructor Create(AOwner : TObject; CommonData : TsCommonData);
    destructor Destroy; override;
  published
{$ENDIF} // NOTFORHELP
    property Active : boolean read FActive write SetActive default False;
    property Caption : acString read FText write SetText; // gralex
    property Indent : integer read FIndent write SetIndent;
    property Font : TFont read GetFont write SetFont;
    property Layout : TsCaptionLayout read FLayout write SetLayout;
    property MaxWidth: integer read FMaxWidth write SetMaxWidth;
    property UseSkinColor : boolean read GetUseSkin write SetUseSkin;
  end;

{$IFNDEF NOTFORHELP}
var
  b : boolean;
  C1, C2 : TsColor;
  RestrictDrawing : boolean = False;

function IsCached(SkinData : TsCommonData) : boolean;
function IsCacheRequired(SkinData : TsCommonData) : boolean;
procedure InitBGInfo(const SkinData : TsCommonData; const PBGInfo : PacBGInfo; const State : integer; Handle : THandle = 0);
function GetBGColor(const SkinData : TsCommonData; const State : integer; Handle : THandle = 0) : TColor;
function GetFontIndex(const Ctrl : TControl; const DefSkinIndex : integer; const SkinManager : TsSkinManager) : integer;
procedure ShowGlowingIfNeeded(const SkinData : TsCommonData; Clicked : boolean = False; CtrlHandle : HWND = 0);

procedure InitCacheBmp(SkinData : TsCommonData);
function SkinBorderMaxWidth(SkinData : TsCommonData) : integer;

procedure UpdateData(SkinData : TsCommonData);
procedure UpdateSkinState(const SkinData : TsCommonData; UpdateChildren : boolean = True);
procedure AlignShadow(SkinData : TsCommonData);
function ControlIsActive(SkinData : TsCommonData): boolean;
function BgIsTransparent(CommonData : TsCommonData) : boolean;
procedure CopyWinControlCache(Control : TWinControl; SkinData : TsCommonData; SrcRect, DstRect : TRect; DstDC : HDC; UpdateCorners : boolean; OffsetX : integer = 0; OffsetY : integer = 0); overload;
procedure CopyHwndCache(hwnd : THandle; SkinData : TsCommonData; SrcRect, DstRect : TRect; DstDC : HDC; UpdateCorners : boolean; OffsetX : integer = 0; OffsetY : integer = 0); overload;

function CommonMessage(var Message: TMessage; SkinData : TsCommonData) : boolean;
function CommonWndProc(var Message: TMessage; SkinData : TsCommonData) : boolean;
{$ENDIF} // NOTFORHELP
function GetParentCache(SkinData : TsCommonData) : TCacheInfo;
function GetParentCacheHwnd(cHwnd : hwnd) : TCacheInfo;

implementation

uses sStyleSimply, sSkinProps, sMaskData, sMessages, sButton, sBitBtn, Math, ComCtrls, acGlow,
  {$IFNDEF ALITE} sPageControl, sSplitter, sCustomComboEdit, {$ENDIF} sVclUtils{$IFDEF CHECKXP}, UxTheme, Themes{$ENDIF},
  sGraphUtils, sAlphaGraph, sSkinProvider, sSpeedButton;

{$IFDEF RUNIDEONLY}
var
  sTerminated : boolean = False;
{$ENDIF}

function IsCached(SkinData : TsCommonData) : boolean;
begin
  Result := InAnimationProcess or ac_v5PaintMode or (SkinData.CtrlSkinState and ACS_FAST <> ACS_FAST) or ControlIsActive(SkinData);
end;

procedure InitBGType(SkinData : TsCommonData);
begin
  SkinData.BGType := 0;
  if (SkinData.SkinManager <> nil) and (SkinData.SkinIndex > -1) then begin

    if (SkinData.SkinManager.gd[SkinData.SkinIndex].ImagePercent <> 0) then SkinData.BGType := SkinData.BGType or BGT_TEXTURE;
    if (SkinData.SkinManager.gd[SkinData.SkinIndex].GradientPercent <> 0) and (Length(SkinData.SkinManager.gd[SkinData.SkinIndex].GradientArray) > 0) then begin
      case SkinData.SkinManager.gd[SkinData.SkinIndex].GradientArray[0].Mode1 of
        0 : SkinData.BGType := SkinData.BGType or BGT_GRADIENTVERT;
        1 : SkinData.BGType := SkinData.BGType or BGT_GRADIENTHORZ;
        2 : SkinData.BGType := SkinData.BGType or BGT_GRADIENTHORZ or BGT_GRADIENTVERT;
      end;
    end;
{
    if (SkinData.Texture > -1) and (SkinData.SkinManager.ma[SkinData.Texture].DrawMode > -1) then begin
      try
      case acFillModes[SkinData.SkinManager.ma[SkinData.Texture].DrawMode] of
        fmStretchHorz, fmTiledHorz, fmDiscHorTop : SkinData.BGType := SkinData.BGType or BGT_TEXTURETOP;
        fmStretchVert, fmTiledVert, fmDiscVertLeft : SkinData.BGType := SkinData.BGType or BGT_TEXTURELEFT;
        fmTileVertRight, fmDiscVertRight, fmStretchVertRight : SkinData.BGType := SkinData.BGType or BGT_TEXTURERIGHT;
        fmTileHorBtm, fmDiscHorBottom, fmStretchHorBtm : SkinData.BGType := SkinData.BGType or BGT_TEXTUREBOTTOM;
      end;
      except
      end;
    end;  }
  end
end;

function IsCacheRequired(SkinData : TsCommonData) : boolean; // Used for non-active controls only, active controls have cache always
//  function BorderIsTooBig : boolean;
begin
  if ac_v5PaintMode then begin
    Result := True;
    if SkinData.FCacheBmp = nil then SkinData.FCacheBmp := CreateBmp24(0, 0);
    Exit;
  end
  else begin
    Result := True;
    if (SkinData.SkinManager <> nil) and (SkinData.SkinIndex > -1) then begin
      with SkinData.SkinManager.gd[SkinData.SkinIndex] do begin
        if (SkinData.BorderIndex > -1) // Border Is Too Big
          and ((SkinData.SkinManager.ma[SkinData.BorderIndex].WL + SkinData.SkinManager.ma[SkinData.BorderIndex].WR > 30) or
          (SkinData.SkinManager.ma[SkinData.BorderIndex].WT + SkinData.SkinManager.ma[SkinData.BorderIndex].WB > 30)) then Exit;

        if (Transparency = 100) then begin
          if Assigned(SkinData.FOwnerControl) and Assigned(SkinData.FOwnerControl.Parent){ and SkinData.Skinned} then begin
            Result := SendAMessage(SkinData.FOwnerControl.Parent, AC_GETSKINSTATE) and ACS_FAST <> ACS_FAST;
            if Result then Exit;
          end
          else Exit;
        end
        else if (Transparency <> 0) then Exit;
        if (SkinData.SkinManager.gd[SkinData.SkinIndex].ImagePercent <> 0) then Exit;
        if (SkinData.SkinManager.gd[SkinData.SkinIndex].GradientPercent <> 0) then Exit;
        Result := False;
      end;
    end
    else Result := False;
  end;
end;

procedure InitBGInfo(const SkinData : TsCommonData; const PBGInfo : PacBGInfo; const State : integer; Handle : THandle = 0);
var
  iTransparency, iGradient, iTexture : integer;
begin
  if SkinData.Skinned and not SkinData.CustomColor then begin
    if State = 0 then begin
      iTransparency := Skindata.SkinManager.gd[SkinData.SkinIndex].Transparency;
      iGradient := Skindata.SkinManager.gd[SkinData.SkinIndex].GradientPercent;
      iTexture := Skindata.SkinManager.gd[SkinData.SkinIndex].ImagePercent;
    end
    else begin
      iTransparency := Skindata.SkinManager.gd[SkinData.SkinIndex].HotTransparency;
      iGradient := Skindata.SkinManager.gd[SkinData.SkinIndex].HotGradientPercent;
      iTexture := Skindata.SkinManager.gd[SkinData.SkinIndex].HotImagePercent;
    end;
    case iTransparency of
      0 : begin
        if (iGradient > 0) or (iTexture > 0) then begin
          if SkinData.FCacheBmp = nil then begin
            PBGInfo^.Color := clFuchsia; // Debug
            PBGInfo^.BgType := btFill;
            Exit;
          end;
          PBGInfo^.BgType := btCache;
          if PBGInfo^.PleaseDraw then begin
            BitBlt(PBGInfo^.DrawDC, PBGInfo^.R.Left, PBGInfo^.R.Top, WidthOf(PBGInfo^.R), HeightOf(PBGInfo^.R),
                     SkinData.FCacheBmp.Canvas.Handle, PBGInfo^.Offset.X, PBGInfo^.Offset.Y, SRCCOPY);
          end
          else begin
            PBGInfo^.Bmp := SkinData.FCacheBmp;
            PBGInfo^.Offset := Point(0, 0);
          end;
        end
        else begin
          PBGInfo^.BgType := btFill;
          PBGInfo^.Bmp := SkinData.FCacheBmp;
          PBGInfo^.Color := GetBGColor(SkinData, State);
          if PBGInfo^.PleaseDraw then FillDC(PBGInfo^.DrawDC, PBGInfo^.R, PBGInfo^.Color);
        end
      end;
      100 : begin
        if (SkinData.BorderIndex > -1) // Border Is Too Big
          and ((SkinData.SkinManager.ma[SkinData.BorderIndex].WL + SkinData.SkinManager.ma[SkinData.BorderIndex].WR > 24) or (SkinData.SkinManager.ma[SkinData.BorderIndex].WT + SkinData.SkinManager.ma[SkinData.BorderIndex].WB > 24)) then begin
           if SkinData.FCacheBmp <> nil then begin
             PBGInfo^.BgType := btCache;
             if PBGInfo^.PleaseDraw then begin
               BitBlt(PBGInfo^.DrawDC, PBGInfo^.R.Left, PBGInfo^.R.Top, WidthOf(PBGInfo^.R), HeightOf(PBGInfo^.R),
                        SkinData.FCacheBmp.Canvas.Handle, PBGInfo^.Offset.X, PBGInfo^.Offset.Y, SRCCOPY);
             end
             else begin
               PBGInfo^.Bmp := SkinData.FCacheBmp;
               PBGInfo^.Offset := Point(0, 0);
             end;
           end
           else begin
             PBGInfo^.BgType := btFill; 
             if (SkinData.FOwnerControl <> nil) and (SkinData.FOwnerControl.Parent <> nil)
               then PBGInfo^.Color := GetControlColor(SkinData.FOwnerControl.Parent)
               else PBGInfo^.Color := DefaultManager.GetGlobalColor
           end;                                                  
           Exit;
        end;

        if (SkinData.FOwnerControl <> nil) and (SkinData.FOwnerControl.Parent <> nil) then begin
          GetBGInfo(PBGInfo, SkinData.FOwnerControl.Parent, PBGInfo^.PleaseDraw);
          if PBGInfo^.BgType = btCache then begin
            inc(PBGInfo^.Offset.X, SkinData.FOwnerControl.Left);
            inc(PBGInfo^.Offset.Y, SkinData.FOwnerControl.Top);
          end;
        end
        else if Handle <> 0 then begin
          GetBGInfo(PBGInfo, GetParent(Handle));
  { // Add own offset if placed on transparent control
          if PBGInfo^.BgType = btCache then begin
            inc(PBGInfo^.Offset.X, SkinData.FOwnerControl.Left);
            inc(PBGInfo^.Offset.Y, SkinData.FOwnerControl.Top);
          end;
  }
        end
        else begin
          PBGInfo^.BgType := btFill;
          PBGInfo^.Color := DefaultManager.GetGlobalColor
        end;
      end
      else begin
        if SkinData.FCacheBmp = nil then begin
          PBGInfo^.Color := clFuchsia; // Debug
          PBGInfo^.BgType := btFill;
          Exit;
        end;
        PBGInfo^.BgType := btCache;
        if PBGInfo^.PleaseDraw then begin
          BitBlt(PBGInfo^.DrawDC, PBGInfo^.R.Left, PBGInfo^.R.Top, WidthOf(PBGInfo^.R), HeightOf(PBGInfo^.R), SkinData.FCacheBmp.Canvas.Handle, PBGInfo^.Offset.X, PBGInfo^.Offset.Y, SRCCOPY);
        end
        else begin
          PBGInfo^.Bmp := SkinData.FCacheBmp;
          PBGInfo^.Offset := Point(0, 0);
        end;
      end;
    end;
  end
  else begin
    PBGInfo^.BgType := btFill;
    if SkinData.FOwnerControl <> nil then PBGInfo^.Color := TsHackedControl(SkinData.FOwnerControl).Color else begin
      if (SkinData.FOwnerObject <> nil) and (SkinData.FOwnerObject is TsSkinProvider) then PBGInfo^.Color := TsSkinProvider(SkinData.FOwnerObject).Form.Color;
    end;
  end;
end;

function GetBGColor(const SkinData : TsCommonData; const State : integer; Handle : THandle = 0) : TColor;
var
  i, C : integer;
  function StdColor : TColor;
  begin
    if SkinData.FOwnerControl <> nil
      then Result := TsHackedControl(SkinData.FOwnerControl).Color
      else if SkinData.FOwnerObject is TsSkinProvider
        then Result := TsSkinProvider(SkinData.FOwnerObject).Form.Color
        else Result := clBtnFace;
  end;
begin
  if SkinData.Skinned then begin
    if State = 0 then begin
      i := SkinData.SkinManager.gd[SkinData.SkinIndex].Transparency;
      c := SkinData.SkinManager.gd[SkinData.SkinIndex].Color
    end
    else begin
      i := SkinData.SkinManager.gd[SkinData.SkinIndex].HotTransparency;
      c := SkinData.SkinManager.gd[SkinData.SkinIndex].HotColor
    end;
    Result := 0;

    case i of
      0 : begin
        if SkinData.Skinned and not SkinData.CustomColor then begin
          Result := C;
        end
        else begin
          Result := StdColor;
        end   
      end;
      100 : begin
        if SkinData.FOwnerControl <> nil
          then Result := GetControlColor(SkinData.FOwnerControl.Parent)
          else if Handle <> 0 then Result := GetControlColor(GetParent(Handle)) else Result := clBtnFace;
      end
      else begin
        if SkinData.FOwnerControl <> nil then Result := MixColors(c, GetControlColor(SkinData.FOwnerControl.Parent), i / 100);
      end;
    end
  end
  else Result := StdColor;
end;

function GetFontIndex(const Ctrl : TControl; const DefSkinIndex : integer; const SkinManager : TsSkinManager) : integer;
var
  Ndx : integer;
begin
  Result := DefSkinIndex;
  if Ctrl = nil then Exit;
  Ndx := SendMessage(Ctrl.Parent.Handle, SM_ALPHACMD, MakeWParam(0, AC_GETSKININDEX), 0) - 1;
  if Ndx >= 0 then begin
    if (SkinManager.gd[Ndx].Transparency > 50) and (Ctrl.Parent <> nil) then begin
      Result := GetFontIndex(Ctrl.Parent, DefSkinIndex, SkinManager);
    end
    else if SkinManager.gd[Ndx].GiveOwnFont then Result := Ndx else Result := DefSkinIndex;
  end;
end;

procedure ShowGlowingIfNeeded(const SkinData : TsCommonData; Clicked : boolean = False; CtrlHandle : HWND = 0);
var
  GlowCount : integer;
  R, RBox : TRect;
  DC : hdc;
  ParentForm : TCustomForm;
  WndHandle : HWND;
begin
  if SkinData.SkinManager.AllowGlowing and (SkinData.SkinIndex > -1) then begin
    GlowCount := SkinData.SkinManager.gd[SkinData.SkinIndex].GlowCount;
    if GlowCount < 1 then Exit;

    if (SkinData.FOwnerControl <> nil) then begin
      if SkinData.FMouseAbove and not (csLButtonDown in SkinData.FOwnerControl.ControlState) and (not Clicked or (SkinData.GlowID = -1)) then begin
        ParentForm := GetParentForm(SkinData.FOwnerControl);
        if ParentForm = nil then Exit;
        WndHandle := ParentForm.Handle;
        if (SkinData.FOwnerControl is TWinControl) then begin
          GetWindowRect(TWinControl(SkinData.FOwnerControl).Handle, R);

          DC := GetWindowDC(TWinControl(SkinData.FOwnerControl).Handle);
          GetClipBox(DC, RBox);
          ReleaseDC(TWinControl(SkinData.FOwnerControl).Handle, DC);

          SkinData.GlowID := ShowGlow(R, RBox, SkinData.SkinSection, s_Glow, SkinData.SkinManager.gd[SkinData.SkinIndex].GlowMargin, 255, WndHandle, SkinData.SkinManager);
        end
        else begin
          R.TopLeft := SkinData.FOwnerControl.ClientToScreen(Point(0, 0));
          R.BottomRight := Point(R.Left + SkinData.FOwnerControl.Width, R.Top + SkinData.FOwnerControl.Height);

          DC := GetDC(SkinData.FOwnerControl.Parent.Handle);
          GetClipBox(DC, RBox);
          ReleaseDC(SkinData.FOwnerControl.Parent.Handle, DC);

          if SkinData.FOwnerControl.Left < 0 then RBox.Left := -SkinData.FOwnerControl.Left else RBox.Left := 0;
          if SkinData.FOwnerControl.Top < 0 then RBox.Top := -SkinData.FOwnerControl.Top else RBox.Top := 0;
          if SkinData.FOwnerControl.Left + SkinData.FOwnerControl.Width > RBox.Right
            then RBox.Right := RBox.Right - SkinData.FOwnerControl.Left
            else RBox.Right := SkinData.FOwnerControl.Width;
          if SkinData.FOwnerControl.Top + SkinData.FOwnerControl.Height > RBox.Bottom
            then RBox.Bottom := RBox.Bottom - SkinData.FOwnerControl.Top
            else RBox.Bottom := SkinData.FOwnerControl.Height;

          SkinData.GlowID := ShowGlow(R, RBox, SkinData.SkinSection, s_Glow, SkinData.SkinManager.gd[SkinData.SkinIndex].GlowMargin, 255, WndHandle, SkinData.SkinManager);
        end;
      end
      else if (SkinData.GlowID <> -1) then begin
        HideGlow(SkinData.GlowID);
        SkinData.GlowID := -1;
      end;
    end
    else if CtrlHandle <> 0 then begin
      GetWindowRect(CtrlHandle, R);

      DC := GetWindowDC(CtrlHandle);
      GetClipBox(DC, RBox);
      ReleaseDC(CtrlHandle, DC);

      SkinData.GlowID := ShowGlow(R, RBox, SkinData.SkinSection, s_Glow, SkinData.SkinManager.gd[SkinData.SkinIndex].GlowMargin, 255, GetParentFormHandle(CtrlHandle), SkinData.SkinManager);
    end;
  end;
end;

procedure InitCacheBmp(SkinData : TsCommonData);
begin
  with SkinData do begin
    if not Assigned(FCacheBmp) then FCacheBmp := TBitmap.Create;
    if FCacheBmp.PixelFormat <> pf24bit then FCacheBmp.PixelFormat := pf24bit;
    if FCacheBmp.HandleType <> bmDIB then FCacheBmp.HandleType := bmDIB;
    if Assigned(FOwnerControl) then begin
      if FCacheBmp.Width <> FOwnerControl.Width then FCacheBmp.Width := FOwnerControl.Width;
      if FCacheBmp.Height <> FOwnerControl.Height then FCacheBmp.Height := FOwnerControl.Height;
    end
  end;
end;

function SkinBorderMaxWidth(SkinData : TsCommonData) : integer;
begin
  Result := 0;
  if (SkinData.BorderIndex > -1) and (SkinData.SkinManager.ma[SkinData.BorderIndex].DrawMode and BDM_ACTIVEONLY <> BDM_ACTIVEONLY) then with SkinData.SkinManager.ma[SkinData.BorderIndex] do begin
    if WL > WT then Result := WL else Result := WT;
    if WR > Result then Result := WR;
    if WB > Result then Result := WB;
  end
end;

function GetParentCache(SkinData : TsCommonData) : TCacheInfo;
var
  BGInfo : TacBGInfo;
begin
  if SkinData.Skinned and Assigned(SkinData.FOwnerControl) and Assigned(SkinData.FOwnerControl.Parent) then begin
    BGInfo.DrawDC := 0;
    BGInfo.PleaseDraw := False;
    GetBGInfo(@BGInfo, SkinData.FOwnerControl.Parent);
    Result := BGInfoToCI(@BGInfo);
  end
  else begin
    Result.FillColor := GetBGColor(SkinData, 0);
    Result.Ready := False;
  end;
end;

function GetParentCacheHwnd(cHwnd : hwnd) : TCacheInfo;
var
  pHwnd : hwnd;
  BGInfo : TacBGInfo;
begin
  Result.Ready := False;
  pHwnd := GetParent(cHwnd);
  if pHwnd <> 0 then begin
    BGInfo.PleaseDraw := False;
    GetBGInfo(@BGInfo, pHwnd);
    Result := BGInfoToCI(@BGInfo);
  end;
end;

procedure UpdateData(SkinData : TsCommonData);
begin
  with SkinData do if SkinSection = '' then case COC of
    COC_TsSpinEdit..COC_TsListBox, COC_TsCurrencyEdit, COC_TsDBEdit, COC_TsDBMemo, COC_TsDBListBox, COC_TsAlphaListBox,
    COC_TsDBLookupListBox, COC_TsTreeView, COC_TsCustomComboEdit, COC_TsDateEdit, COC_TsAdapter, COC_TsListView : SkinSection := s_Edit;
    COC_TsCustomComboBox..COC_TsComboBoxEx, COC_TsDBComboBox, COC_TsDBLookupComboBox : SkinSection := s_ComboBox;
    COC_TsButton, COC_TsBitBtn : SkinSection := s_Button;
    COC_TsPanel, COC_TsCustomPanel, COC_TsMonthCalendar, COC_TsGrip : SkinSection := s_Panel;
    COC_TsPanelLow : SkinSection := s_PanelLow;
    COC_TsStatusBar : SkinSection := s_StatusBar;
    COC_TsTabControl : SkinSection := s_PageControl;
    COC_TsTabSheet : SkinSection := s_TabSheet;
    COC_TsDBNavigator, COC_TsToolBar, COC_TsCoolBar : SkinSection := s_ToolBar;
    COC_TsNavButton : SkinSection := s_ToolButton;
    COC_TsDragBar : SkinSection := s_DragBar;
    COC_TsScrollBox : SkinSection := s_PanelLow;
    COC_TsSplitter : SkinSection := s_Splitter;
    COC_TsGroupBox : SkinSection := s_GroupBox;
    COC_TsGauge : SkinSection := s_Gauge;
    COC_TsCheckBox, COC_TsHeaderControl : SkinSection := s_CheckBox;
    COC_TsRadioButton : SkinSection := s_RadioButton;
    COC_TsFrameAdapter : SkinSection := s_GroupBox;
    COC_TsTrackBar : SkinSection := s_TrackBar;
    COC_TsPageControl : SkinSection := s_PageControl;
    COC_TsFrameBar : SkinSection := s_Bar;
    COC_TsBarTitle : SkinSection := s_BarTitle;
    COC_TsSpeedButton, COC_TsColorSelect : SkinSection := s_SpeedButton;
    else SkinSection := FOwnerObject.ClassName;
  end
  else UpdateIndexes;
end;

procedure UpdateSkinState(const SkinData : TsCommonData; UpdateChildren : boolean = True);
var
  i : integer;
begin
  SkinData.CtrlSkinState := SkinData.CtrlSkinState and not ACS_FAST;

  if SkinData.FOwnerControl <> nil then begin
    if (SkinData.FOwnerControl is TFrame) or (SkinData.FOwnerControl is TPanel) or (SkinData.FOwnerControl is TScrollingWinControl) or (SkinData.FOwnerControl is TPageControl) then begin
      if not IsCacheRequired(SkinData) then SkinData.CtrlSkinState := SkinData.CtrlSkinState or ACS_FAST else SkinData.CtrlSkinState := SkinData.CtrlSkinState and not ACS_FAST;
    end;
    if UpdateChildren and (SkinData.FOwnerControl is TWinControl) then with TWinControl(SkinData.FOwnerControl) do begin
      for i := 0 to ControlCount - 1
        do SendAMessage(Controls[i], AC_GETSKINSTATE, 1);
    end;
  end
  else if SkinData.FOwnerObject is TsSkinProvider then begin
    if not IsCacheRequired(SkinData) then SkinData.CtrlSkinState := SkinData.CtrlSkinState or ACS_FAST else SkinData.CtrlSkinState := SkinData.CtrlSkinState and not ACS_FAST;
  end
end;

function ControlIsActive(SkinData : TsCommonData): boolean;
begin
  Result := False;
  with SkinData do begin
    if not Assigned(FOwnerControl) or (csDestroying in FOwnerControl.ComponentState) then Exit;
    if FOwnerControl.Enabled and not (csDesigning in FOwnerControl.ComponentState) then begin
      if FFocused
        then Result := True
        else if (FOwnerControl is TWinControl) and TWinControl(FOwnerControl).Focused
               then Result := True
               else if SkinData.FMouseAbove then Result := not (SkinData.COC in sForbidMouse);
    end;
  end;
end;

function BgIsTransparent(CommonData : TsCommonData) : boolean;
begin
  Result := False;
  if CommonData.SkinIndex < 0 then Exit;
  Result := CommonData.SkinManager.gd[CommonData.SkinIndex].Transparency > 0;
end;

procedure AlignShadow(SkinData : TsCommonData);
begin
end;

{ TsCommonData }

procedure TsCommonData.BeginUpdate;
begin
  FUpdating := True;
  if FOwnerControl <> nil then begin
    FOwnerControl.Perform(SM_ALPHACMD, MakeWParam(0, AC_BEGINUPDATE), 0)
  end
  else if (FOwnerObject <> nil) and (FOwnerObject is TsSkinProvider) then SendMessage(TsSkinProvider(FOwnerObject).Form.Handle, SM_ALPHACMD, MakeWParam(0, AC_BEGINUPDATE), 0);
end;

constructor TsCommonData.Create(AOwner : TObject; CreateCacheBmp : boolean);
begin
  SkinIndex := -1;
  BorderIndex := -1;
  GlowID := -1;
  Texture := -1;
  HotTexture := -1;
  if AOwner is TControl then FOwnerControl := TControl(AOwner) else FOwnerControl := nil;
  FOwnerObject := AOwner;
  FFocused := False;
  FMouseAbove := False;
  FUpdating := False;
  BGChanged := True;
  GraphControl := nil;
  HalfVisible := True;
  FSkinManager := nil;
  FCacheBmp := nil;
  CtrlSkinState := 0;
  BGType := 0;

  if CreateCacheBmp then FCacheBmp := CreateBmp24(0, 0); // Must be moved to the UpdateIndexes procedure !!

{$IFDEF RUNIDEONLY}
  if not IsIDERunning and not ((FOwnerObject is TComponent) and (csDesigning in TComponent(FOwnerObject).ComponentState)) and not sTerminated then begin
    sTerminated := True;
    ShowWarning(sIsRUNIDEONLYMessage);
  end;
{$ENDIF}
end;

destructor TsCommonData.Destroy;
begin
  SkinIndex := -1;
  Texture := -1;
  HotTexture := -1;
  FOwnerControl := nil;
  FOwnerObject := nil;
  FSkinManager := nil;
  if Assigned(FCacheBmp) then FreeAndNil(FCacheBmp);
  if GlowID <> -1 then HideGlow(GlowID);
  inherited Destroy;
end;

procedure TsCommonData.EndUpdate;
begin
  FUpdating := False;
  if FOwnerControl <> nil then begin
    FOwnerControl.Perform(SM_ALPHACMD, MakeWParam(0, AC_ENDUPDATE), 0)
  end
  else if (FOwnerObject <> nil) and (FOwnerObject is TsSkinProvider) then SendMessage(TsSkinProvider(FOwnerObject).Form.Handle, SM_ALPHACMD, MakeWParam(0, AC_ENDUPDATE), 0);
end;

function TsCommonData.GetSkinManager: TsSkinManager;
begin
  if Assigned(FSkinManager)
    then Result := FSkinManager
    else if Assigned(DefaultManager) then Result := DefaultManager else Result := nil;
end;

function TsCommonData.GetUpdating: boolean;
begin
  if not Assigned(SkinManager) {or not SkinManager.SkinData.Active {or InAnimationProcess }then begin
    Result := False;//True;
    Exit
  end
  else
  if csDesigning in SkinManager.ComponentState then begin
    Result := False;
    Exit
  end
  else
  if not IsCached(Self) then begin
    Result := FUpdating;
    Exit
  end
  else Result := FUpdating;
  if (Self = nil) or (FOwnerControl = nil) or (csDestroying in FOwnerControl.ComponentState) then Exit; 
  if not Self.FUpdating and (FOwnerControl <> nil) and (FOwnerControl.Parent <> nil)
    then Result := GetBoolMsg(FOwnerControl.Parent, AC_PREPARING)
    else Result := True
end;

procedure TsCommonData.InitCacheBmp;
begin
  if not Assigned(FCacheBmp) then FCacheBmp := TBitmap.Create;
  if FCacheBmp.PixelFormat <> pf24bit then FCacheBmp.PixelFormat := pf24bit;
  if FCacheBmp.HandleType <> bmDIB then FCacheBmp.HandleType := bmDIB;
  if Assigned(FOwnerControl) then begin
    if FCacheBmp.Width <> FOwnerControl.Width then FCacheBmp.Width := FOwnerControl.Width;
    if FCacheBmp.Height <> FOwnerControl.Height then FCacheBmp.Height := FOwnerControl.Height;
  end
end;

procedure TsCommonData.Invalidate;
begin
  if Assigned(FOwnerControl) and not ((csDestroying in FOwnerControl.ComponentState) or (csLoading in FOwnerControl.ComponentState)) then begin
    BGChanged := True;
    if ControlIsReady(FOwnerControl) then FOwnerControl.Invalidate
  end;
end;

procedure TsCommonData.Loaded;
begin
  UpdateData(Self);
  if Skinned and Assigned(FOwnerControl) and Assigned(FOwnerControl.Parent) and not (csLoading in FOwnerControl.ComponentState) then begin
    if FOwnerControl is TWinControl then begin
      AddToAdapter(TWinControl(FOwnerControl))
    end;
    if ((FOwnerControl is TCustomEdit) or (COC = COC_TsAlphaListBox)) then begin
      if not CustomColor and (TsHackedControl(FOwnerControl).Color <> SkinManager.gd[SkinIndex].Color) then TsHackedControl(FOwnerControl).Color := SkinManager.gd[SkinIndex].Color;
      if not CustomFont and (TsHackedControl(FOwnerControl).Font.Color <> SkinManager.gd[SkinIndex].FontColor[1]) then TsHackedControl(FOwnerControl).Font.Color := SkinManager.gd[SkinIndex].FontColor[1];
    end;
  end;
end;

function TsCommonData.ManagerStored: boolean;
begin
  Result := (FSkinManager <> nil)
end;

function TsCommonData.ParentTextured: boolean;
begin
  if Assigned(FOwnerControl) and Assigned(FOwnerControl.Parent) and FOwnerControl.Parent.HandleAllocated
    then Result := GetBoolMsg(FOwnerControl.Parent, AC_CHILDCHANGED)
    else Result := False;
end;

function TsCommonData.RepaintIfMoved: boolean;
begin
  if (Self = nil) or UrgentPainting or not Skinned or{$IFNDEF ALITE}(FOwnerControl is TsTabSheet) or{$ENDIF} (FOwnerControl = nil) or
       (csCreating in FOwnerControl.ControlState) or not FOwnerControl.Visible then begin
    Result := True;
    Exit;
  end;
  if (SkinIndex < 0) or (SkinIndex > High(SkinManager.gd)) then { if is not skinned } Result := False else begin
    Result := (FOwnerControl <> nil) and not FOwnerControl.Enabled;
    if not Result then Result := (SkinManager.gd[SkinIndex].Transparency > 0);
    if Result and Assigned(FOwnerControl) and Assigned(FOwnerControl.Parent) and FOwnerControl.Parent.HandleAllocated then begin
      Result := GetBoolMsg(FOwnerControl.Parent, AC_CHILDCHANGED);
    end;
  end;
end;

procedure TsCommonData.SetCustomColor(const Value: boolean);
begin
  if FCustomColor <> Value then begin
    FCustomColor := Value;
    if Skinned and Assigned(FOwnerControl) and (FOwnerControl is TCustomEdit) and not (csLoading in FOwnerControl.ComponentState) then begin
      if not CustomColor and (TsHackedControl(FOwnerControl).Color <> SkinManager.gd[SkinIndex].Color) then TsHackedControl(FOwnerControl).Color := SkinManager.gd[SkinIndex].Color;
      if not CustomFont and (TsHackedControl(FOwnerControl).Font.Color <> SkinManager.gd[SkinIndex].FontColor[1]) then TsHackedControl(FOwnerControl).Font.Color := SkinManager.gd[SkinIndex].FontColor[1];
      Invalidate
    end;
  end;
end;

procedure TsCommonData.SetCustomFont(const Value: boolean);
begin
  if FCustomFont <> Value then begin
    FCustomFont := Value;
    Invalidate
  end;
end;

procedure TsCommonData.SetSkinManager(const Value: TsSkinManager);
var
  m : TMEssage;
begin
  if FSkinManager <> Value then begin
    if Value <> DefaultManager then FSkinManager := Value else FSkinManager := nil;
    if (FOwnerControl <> nil) and (csLoading in FOwnerControl.ComponentState) then Exit;
    if Assigned(Value) and (Length(Value.gd) > 0) then UpdateIndexes else SkinIndex := -1;
    if Assigned(FOwnerObject) and (FOwnerObject is TsSkinProvider) then begin
      if Assigned(Value) and (Length(Value.gd) > 0) and Value.IsValidSkinIndex(SkinIndex)
        then m.WParam := MakeWParam(0, AC_REFRESH) else m.WParam := MakeWParam(0, AC_REMOVESKIN);
      m.Msg := SM_ALPHACMD;
      m.LParam := LongWord(Value);
      TsSkinProvider(FOwnerObject).DsgnWndProc(m);
    end
    else if (FOwnerControl <> nil) and (FOwnerControl.Parent <> nil) and not (csDestroying in FOwnerControl.ComponentState) then begin
      try
      if Assigned(Value) and (Length(Value.gd) > 0) and Value.IsValidSkinIndex(SkinIndex)
        then FOwnerControl.Perform(SM_ALPHACMD, MakeWParam(0, AC_REFRESH), LongWord(Value))
        else FOwnerControl.Perform(SM_ALPHACMD, MakeWParam(0, AC_REMOVESKIN), LongWord(Value));
      except
      end;
    end;
  end;
end;

procedure TsCommonData.SetSkinSection(const Value: string);
begin
  if FSkinSection <> Value then begin
    if Value = '' then FSkinSection := s_Unknown else FSkinSection := UpperCase(Value);

    if Assigned(SkinManager) and (Length(SkinManager.gd) > 0) then UpdateIndexes else SkinIndex := -1;
    if (FOwnerControl <> nil) and not (csLoading in FOwnerControl.ComponentState) and not (csReading in FOwnerControl.ComponentState) and
         (FOwnerControl.Parent <> nil) and not (csDestroying in FOwnerControl.ComponentState) then begin

      if Assigned(SkinManager) and (Length(SkinManager.gd) > 0) and SkinManager.IsValidSkinIndex(SkinIndex) then begin
        FOwnerControl.Perform(SM_ALPHACMD, MakeWParam(0, AC_REFRESH), LongWord(SkinManager));
//        FOwnerControl.Perform(SM_ALPHACMD, MakeWParam(0, AC_INVALIDATE), 0);
      end
      else FOwnerControl.Perform(SM_ALPHACMD, MakeWParam(0, AC_REMOVESKIN), LongWord(SkinManager));
    end
    else if (FOwnerObject <> nil) and (FOwnerObject is TsSkinProvider) then begin
      BGChanged := True;
      FreeAndNil(TsSkinProvider(Self.FOwnerObject).TitleBG);
    end;
  end;
end;

procedure TsCommonData.SetUpdating(const Value: boolean);
begin
  FUpdating := Value;
end;

function TsCommonData.Skinned(CheckSkinActive : boolean = False): boolean;
begin
  if (Self = nil) or (SkinManager = nil) or (SkinIndex < 0) or 
       ((Self.FOwnerObject <> nil) and (csDestroying in TComponent(Self.FOwnerObject).ComponentState)) or
         (csDestroying in SkinManager.ComponentState)
     then Result := False
     else Result := (not CheckSkinActive or SkinManager.SkinData.Active) and SkinManager.IsValidSkinIndex(SkinIndex);
end;

function CommonMessage(var Message: TMessage; SkinData : TsCommonData) : boolean;
var
  i : integer;
begin
  Result := False;
  if SkinData <> nil then with SkinData do if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_SETTRANSBGCHANGED : begin
      if BgIsTransparent(SkinData) and (Message.WParamLo = 1) then BGChanged := True;
    end;
    AC_UPDATESECTION : if UpperCase(SkinSection) = GlobalSectionName then begin
      RestrictDrawing := False;
      Invalidate;
    end;
//    AC_SETGRAPHCONTROL : GraphControl := pointer(Message.LParam);
    AC_PREPARING       : begin
      Message.Result := integer(BGChanged or FUpdating);
      Result := True;
      Exit;
    end;
    AC_UPDATING        : FUpdating := Message.WParamLo = 1;
    AC_GETHALFVISIBLE  : begin
      Message.Result := integer(HalfVisible);
      Result := True;
      Exit;
    end;
    AC_URGENTPAINT     : UrgentPainting := Message.WParamLo = 1;
    AC_CHILDCHANGED : if (SkinData.SkinIndex > -1) then begin
      if (SkinData.SkinManager.gd[SkinData.SkinIndex].Transparency = 100) and (FOwnerControl <> nil) and (FOwnerControl.Parent <> nil)
        then Message.LParam := integer(ParentTextured)
        else Message.LParam := integer((SkinData.SkinManager.gd[SkinData.SkinIndex].GradientPercent + SkinData.SkinManager.gd[SkinData.SkinIndex].ImagePercent > 0) or SkinData.RepaintIfMoved);
      Message.Result := Message.LParam;
      Result := True;
    end;
    AC_SETNEWSKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      UpdateIndexes;
      UpdateSkinState(SkinData, False);
      RestrictDrawing := False;
    end;
    AC_SETBGCHANGED : BGChanged := True;
    AC_SETHALFVISIBLE : HalfVisible := True;
    AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      BGChanged := True;
      Updating := False;
      if Skinned and Assigned(FOwnerControl) then begin
        if (FOwnerControl is TCustomEdit) then begin
          if not CustomColor then TsHackedControl(FOwnerControl).Color := SkinData.SkinManager.gd[SkinIndex].Color;
          if not CustomFont then TsHackedControl(FOwnerControl).Font.Color := SkinData.SkinManager.gd[SkinIndex].FontColor[1];
          RedrawWindow(TCustomEdit(FOwnerControl).Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE);
        end;
        if (FOwnerControl is TWinControl) then AddToAdapter(TWinControl(FOwnerControl))
      end;
      UpdateSkinState(SkinData, False);
    end;
    AC_REMOVESKIN : if LongWord(Message.LParam) = LongWord(SkinData.SkinManager) then begin
      if (SkinData.SkinManager <> nil) and (csDestroying in SkinData.SkinManager.ComponentState) then FSkinManager := nil;
      BorderIndex := -1;
      SkinIndex := -1;
      Texture := -1;
      HotTexture := -1;
      FUpdating := True;
      if Assigned(FCacheBmp) then begin
        FCacheBmp.Width := 0;
        FCacheBmp.Height := 0;
      end;
      if Assigned(FOwnerControl) and (FOwnerControl is TCustomEdit) then begin
        if not CustomColor then TsHackedControl(FOwnerControl).Color := clWindow;
        if not CustomFont then TsHackedControl(FOwnerControl).Font.Color := clWindowText;
        RedrawWindow(TCustomEdit(FOwnerControl).Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE);
      end
{$IFDEF CHECKXP}
//      if UseThemes and (SkinData.FOwnerControl <> nil) and (SkinData.FOwnerControl is TWinControl) then begin
//        SetWindowTheme(TWinControl(SkinData.FOwnerControl).Handle, nil, nil);
//      end;
{$ENDIF}
    end;
    AC_GETCONTROLCOLOR : if Assigned(FOwnerControl) then begin
      if SkinData.Skinned then begin
        case SkinData.SkinManager.gd[SkinData.Skinindex].Transparency of
          0 : Message.Result := SkinData.SkinManager.gd[SkinData.Skinindex].Color;
          100 : begin if FOwnerControl.Parent <> nil
            then begin
              Message.Result := SendMessage(FOwnerControl.Parent.Handle, SM_ALPHACMD, MakeWParam(0, AC_GETCONTROLCOLOR), 0);
              if Message.Result = clFuchsia {if AlphaMessage is not supported} then Message.Result := TsHackedControl(FOwnerControl.Parent).Color
            end
            else Message.Result := ColorToRGB(TsHackedControl(FOwnerControl).Color);
          end
          else begin
            if FOwnerControl.Parent <> nil
              then Message.Result := SendMessage(FOwnerControl.Parent.Handle, SM_ALPHACMD, MakeWParam(0, AC_GETCONTROLCOLOR), 0)
              else Message.Result := ColorToRGB(TsHackedControl(FOwnerControl).Color);
            // Mixing of colors
            C1.C := Message.Result;
            C2.C := SkinData.SkinManager.gd[SkinData.Skinindex].Color;
            C1.R := IntToByte(((C1.R - C2.R) * SkinData.SkinManager.gd[SkinData.Skinindex].Transparency + C2.R shl 8) shr 8);
            C1.G := IntToByte(((C1.G - C2.G) * SkinData.SkinManager.gd[SkinData.Skinindex].Transparency + C2.G shl 8) shr 8);
            C1.B := IntToByte(((C1.B - C2.B) * SkinData.SkinManager.gd[SkinData.Skinindex].Transparency + C2.B shl 8) shr 8);
            Message.Result := C1.C;
          end
        end;
      end
      else Message.Result := ColorToRGB(TsHackedControl(FOwnerControl).Color);
      Result := True;
    end;
    AC_SETCHANGEDIFNECESSARY : begin
      b := SkinData.RepaintIfMoved;
      SkinData.BGChanged := SkinData.BGChanged or b;
      if (SkinData.FOwnerControl <> nil) and (SkinData.FOwnerControl is TWinControl) then begin
        if (Message.WParamLo = 1) then RedrawWindow(TWinControl(SkinData.FOwnerControl).Handle, nil, 0, RDW_NOERASE + RDW_NOINTERNALPAINT + RDW_INVALIDATE + RDW_ALLCHILDREN);
        if b and Assigned(FOwnerControl) then for i := 0 to TWinControl(SkinData.FOwnerControl).ControlCount - 1 do begin
          if (i < TWinControl(SkinData.FOwnerControl).ControlCount) and not (csDestroying in TWinControl(SkinData.FOwnerControl).Controls[i].ComponentState)
            then SendAMessage(TWinControl(FOwnerControl).Controls[i], AC_SETCHANGEDIFNECESSARY)
        end
      end;
    end;
    AC_GETAPPLICATION : begin Message.Result := longint(Application); Exit end;
    AC_CTRLHANDLED : begin
      Message.Result := 1;
      Result := True;
    end;
    AC_GETBG : begin
      InitBGInfo(SkinData, PacBGInfo(Message.LParam), 0);
    end;
    AC_GETSKININDEX : begin
      Message.Result := SkinIndex + 1;
      Result := True
    end;
    AC_GETSKINSTATE : begin
      if Message.LParam = 1 then begin
        UpdateSkinState(SkinData);
      end
      else begin
        Message.Result := CtrlSkinState;
      end;
      Result := True
    end;
  end;
end;

procedure TsCommonData.UpdateIndexes;
begin
{$IFNDEF SKININDESIGN}
  if (FOwnerObject <> nil) and (FOwnerObject is TComponent) and (csDesigning in TComponent(FOwnerObject).ComponentState) and (GetOwnerFrame(TComponent(FOwnerObject)) <> nil) then begin
    SkinIndex := -1;
    Texture := -1;
    HotTexture := -1;
    Exit;
  end;
{$ENDIF}  
  BGChanged := True;
  if Assigned(SkinManager) then SkinIndex := SkinManager.GetSkinIndex(SkinSection) else SkinIndex := -1;

  InitBGType(Self);

  CtrlSkinState := 0;
  if SkinIndex > -1 then begin
    BorderIndex := SkinManager.gd[SkinIndex].BorderIndex;// GetMaskIndex(SkinIndex, SkinSection, s_BordersMask);
    Texture := SkinManager.GetTextureIndex(SkinIndex, SkinSection, s_Pattern);
    HotTexture := SkinManager.GetTextureIndex(SkinIndex, SkinSection, s_HotPattern);

    UpdateSkinState(Self, False);

{    if FOwnerControl <> nil then begin
      if (FOwnerControl is TFrame) or (FOwnerControl is TPanel) or (FOwnerControl is TScrollingWinControl) then
        if not IsCacheRequired(Self)
          then CtrlSkinState := ACS_FAST;
    end
    else if FOwnerObject is TsSkinProvider then begin
      if not IsCacheRequired(Self) then CtrlSkinState := ACS_FAST;
    end;}
  end
  else begin
    Texture := -1;
    HotTexture := -1;
  end;
end;

function CommonWndProc(var Message: TMessage; SkinData : TsCommonData) : boolean;
var
  i : integer;
begin
  Result := False;
  if SkinData <> nil then with SkinData do begin
    if Message.Msg = SM_ALPHACMD then begin // Common messages for all components
      Result := CommonMessage(Message, SkinData);
    end
    else case Message.Msg of
{$IFDEF CHECKXP}
      WM_UPDATEUISTATE : if SkinData.Skinned
        then Result := True
        else if UseThemes and (SkinData.FOwnerControl <> nil) and (SkinData.FOwnerControl is TWinControl)
          then SetWindowTheme(TWinControl(SkinData.FOwnerControl).Handle, nil, nil);
{$ENDIF}
      CM_VISIBLECHANGED : if Assigned(SkinData) and Assigned(SkinData.FCacheBmp) then begin
        if Message.WParam = 0 then begin
          SkinData.FCacheBmp.Width := 0;
          SkinData.FCacheBmp.Height := 0;
        end
        else begin
          SkinData.Updating := False;
          SkinData.BGChanged := True
        end;
      end;
      WM_PARENTNOTIFY : if Assigned(FOwnerControl) and (FOwnerControl is TWinControl) and
                            (Message.WParam and $FFFF = WM_CREATE) or (Message.WParam and $FFFF = WM_DESTROY) then begin
        if Message.WParamLo = WM_CREATE then AddToAdapter(TWinControl(FOwnerControl))
      end;
      WM_SETFOCUS: if Assigned(FOwnerControl) and (FOwnerControl is TWinControl) and TWinControl(FOwnerControl).CanFocus and TWinControl(FOwnerControl).TabStop then begin
        BGChanged := True;
        FFocused := True;
        if (COC in sEditCtrls) then RedrawWindow(TWinControl(FOwnerControl).Handle, nil, 0, RDW_INVALIDATE or RDW_FRAME);
      end;
      WM_KILLFOCUS : if Assigned(FOwnerControl) and (FOwnerControl is TWinControl) and TWinControl(FOwnerControl).CanFocus and TWinControl(FOwnerControl).TabStop then begin
        BGChanged := True;
        FFocused := False;
        if (COC in sEditCtrls) and FOwnerControl.Visible then RedrawWindow(TWinControl(FOwnerControl).Handle, nil, 0, RDW_INVALIDATE or RDW_FRAME);
      end;
      CM_ENABLEDCHANGED, WM_FONTCHANGE: begin
        FMouseAbove := False;
        FFocused := False;
        if Assigned(FOwnerControl) and not FOwnerControl.Enabled and (SkinData.GlowID <> -1) then begin
          HideGlow(SkinData.GlowID);
          SkinData.GlowID := -1;
        end;
      end;
      CM_MOUSEENTER : if not (csDesigning in FOwnerControl.ComponentState) then begin
        if (FOwnerControl <> nil) and (FOwnerControl is TWinControl) and (DefaultManager <> nil) then begin
          for i := 0 to TWinControl(FOwnerControl).ControlCount - 1 do begin
            if (TWinControl(FOwnerControl).Controls[i] is TsSpeedButton) and (TWinControl(FOwnerControl).Controls[i] <> FOwnerControl) and (TWinControl(FOwnerControl).Controls[i] <> Pointer(Message.LParam)) and TsSpeedButton(TWinControl(FOwnerControl).Controls[i]).SkinData.FMouseAbove then begin
              TWinControl(FOwnerControl).Controls[i].Perform(CM_MOUSELEAVE, 0, 0)
            end;
          end;
          if not (COC in sForbidMouse) then FMouseAbove := True;
          DefaultManager.ActiveControl := TWinControl(FOwnerControl).Handle;
          if not (COC in sCanNotBeHot) and not (COC in [COC_TsButton, COC_TsBitBtn, COC_TsSpeedButton]) then ShowGlowingIfNeeded(SkinData);
          if (Skindata.COC in sEditCtrls) then begin
            BGChanged := True;
            RedrawWindow(TWinControl(FOwnerControl).Handle, nil, 0, RDW_INVALIDATE or RDW_FRAME or RDW_UPDATENOW);
          end;
        end else if not (COC in sForbidMouse) then FMouseAbove := True;
      end;
      CM_MOUSELEAVE : if not (csDesigning in FOwnerControl.ComponentState) then begin
        if not (COC in sForbidMouse) then FMouseAbove := False;
        if not (COC in sCanNotBeHot) and not (COC in [COC_TsButton, COC_TsBitBtn, COC_TsSpeedButton]) then ClearGlows;
        if FOwnerControl.Visible and (Skindata.COC in sEditCtrls) then begin
          BGChanged := True;
          RedrawWindow(TWinControl(FOwnerControl).Handle, nil, 0, RDW_INVALIDATE or RDW_FRAME or RDW_UPDATENOW);
        end;
      end;
      WM_SIZE : if Skinned then begin
        BGChanged := True;
        if (FOwnerControl <> nil) and (FOwnerControl is TWinControl) then begin
          if Skinned and (Length(SkinData.SkinManager.gd) > SkinIndex) and (SkinData.SkinManager.gd[SkinIndex].GradientPercent > 0) then begin
            for i := 0 to TWinControl(SkinData.FOwnerControl).ControlCount - 1 do
              if (i < TWinControl(SkinData.FOwnerControl).ControlCount) and
                not (csDestroying in TWinControl(SkinData.FOwnerControl).Controls[i].ComponentState) and
                  not ((akRight in TWinControl(SkinData.FOwnerControl).Controls[i].Anchors) or
                    (akBottom in TWinControl(SkinData.FOwnerControl).Controls[i].Anchors))
              then TWinControl(FOwnerControl).Controls[i].Perform(SM_ALPHACMD, MakeWParam(1, AC_SETCHANGEDIFNECESSARY), 0);
          end;
        end;
        if (SkinData.GlowID <> -1) then begin
          HideGlow(SkinData.GlowID);
          SkinData.GlowID := -1;
        end;
      end;
      WM_MOVE : if SkinData.RepaintIfMoved then begin
        SkinData.BGChanged := True;
        if (SkinData.FOwnerControl <> nil) then SkinData.FOwnerControl.Perform(SM_ALPHACMD, MakeWParam(1, AC_SETCHANGEDIFNECESSARY), 0);
        if (SkinData.GlowID <> -1) then begin
          HideGlow(SkinData.GlowID);
          SkinData.GlowID := -1;
        end;
      end;
    end;
  end;
end;

procedure CopyWinControlCache(Control : TWinControl; SkinData : TsCommonData; SrcRect, DstRect : TRect; DstDC : HDC; UpdateCorners : boolean; OffsetX : integer = 0; OffsetY : integer = 0);
var
  SaveIndex : HDC;
  i : integer;
  Child : TControl;
begin
  if SkinData.FCacheBmp = nil then Exit;
  if UpdateCorners then sAlphaGraph.UpdateCorners(SkinData, 0);
  SaveIndex := SaveDC(DstDC);
  IntersectClipRect(DstDC, DstRect.Left, DstRect.Top, DstRect.Right, DstRect.Bottom);
  try
    for i := 0 to Control.ControlCount - 1 do begin
      Child := Control.Controls[i];
      if (Control.Controls[i] is TGraphicControl) and StdTransparency {$IFNDEF ALITE}or (Control.Controls[i] is TsSplitter){$ENDIF} then Continue;
      if Child.Visible then begin
        if (Control.Controls[i].Left < DstRect.Right) and (Control.Controls[i].Top < DstRect.Bottom) and
           (Control.Controls[i].Left + Control.Controls[i].Width > DstRect.Left) and (Control.Controls[i].top + Control.Controls[i].Height > DstRect.Top) then begin

          if (csDesigning in Control.Controls[i].ComponentState) or
               (csOpaque in Control.Controls[i].ControlStyle) or
               (Control.Controls[i] is TGraphicControl) then begin
            ExcludeClipRect(DstDC, Control.Controls[i].Left + OffsetX, Control.Controls[i].Top + OffsetY,
                            Control.Controls[i].Left + Control.Controls[i].Width + OffsetX,
                            Control.Controls[i].Top + Control.Controls[i].Height + OffsetY);
          end;
        end;
      end;
    end;
    BitBlt(DstDC, DstRect.Left, DstRect.Top, WidthOf(DstRect), HeightOf(DstRect), SkinData.FCacheBmp.Canvas.Handle, SrcRect.Left, SrcRect.Top, SRCCOPY);
  finally
    RestoreDC(DstDC, SaveIndex);
  end;
end;

procedure CopyHwndCache(hwnd : THandle; SkinData : TsCommonData; SrcRect, DstRect : TRect; DstDC : HDC; UpdateCorners : boolean; OffsetX : integer = 0; OffsetY : integer = 0); overload;
var
  SaveIndex : HDC;
  hctrl : THandle;
  R, hR : TRect;
  Index : integer;
begin
  if UpdateCorners then sAlphaGraph.UpdateCorners(SkinData, 0);
  SaveIndex := SaveDC(DstDC);
  IntersectClipRect(DstDC, DstRect.Left, DstRect.Top, DstRect.Right, DstRect.Bottom);
  try        
    hctrl := GetTopWindow(hwnd);
    GetWindowRect(hwnd, R);

    while (hctrl <> 0) do begin
      if IsWindowVisible(hctrl) then begin
        GetWindowRect(hctrl, hR);
        OffsetRect(hR, -R.Left - OffsetX, -R.Top - OffsetY);
        if (GetWindowLong(hctrl, GWL_STYLE) and BS_GROUPBOX = BS_GROUPBOX) and (GetWindowLong(hctrl, GWL_EXSTYLE) and WS_EX_CLIENTEDGE <> WS_EX_CLIENTEDGE) {Prevent of Treeview filling} then begin
          if DefaultManager <> nil then begin
            Index := DefaultManager.GetSkinIndex(s_GroupBox);
            Index := DefaultManager.GetMaskIndex(Index, s_GroupBox, s_BordersMask);
            ExcludeClipRect(DstDC, hR.Left, hR.Top, hR.Right, hR.Top + SendAMessage(hctrl, AC_GETSERVICEINT));
            ExcludeClipRect(DstDC, hR.Left, hR.Top, hR.Left + DefaultManager.MaskWidthLeft(Index), hR.Bottom);
            ExcludeClipRect(DstDC, hR.Right - DefaultManager.MaskWidthRight(Index), hR.Top, hR.Right, hR.Bottom);
            ExcludeClipRect(DstDC, hR.Left, hR.Bottom - DefaultManager.MaskWidthBottom(Index), hR.Right, hR.Bottom);
          end;
        end
        else ExcludeClipRect(DstDC, hR.Left, hR.Top, hR.Right, hR.Bottom);
      end;
      hctrl := GetNextWindow(hctrl, GW_HWNDNEXT);
    end;
    BitBlt(DstDC, DstRect.Left, DstRect.Top, WidthOf(DstRect), HeightOf(DstRect), SkinData.FCacheBmp.Canvas.Handle, SrcRect.Left, SrcRect.Top, SRCCOPY);
  finally
    RestoreDC(DstDC, SaveIndex);
  end;
end;

{ TsBoundLabel }

procedure TsBoundLabel.AlignLabel;
begin
  if Assigned(FTheLabel) and FTheLabel.Visible then begin
    FTheLabel.AutoSize := True;
    case Layout of
      sclLeft : begin
        FTheLabel.Left := FTheLabel.FocusControl.Left - FTheLabel.Width - 4 - Indent;
        FTheLabel.Top := FTheLabel.FocusControl.Top + (FTheLabel.FocusControl.Height - FTheLabel.Height) div 2 - 1;
      end;
      sclTopLeft : begin
        FTheLabel.Left := FTheLabel.FocusControl.Left;
        FTheLabel.Top := FTheLabel.FocusControl.Top - FTheLabel.Height - Indent;
      end;
      sclTopCenter : begin
        FTheLabel.Left := FTheLabel.FocusControl.Left + (FTheLabel.FocusControl.Width - FTheLabel.Width) div 2;
        FTheLabel.Top := FTheLabel.FocusControl.Top - FTheLabel.Height - Indent;
      end;
      sclTopRight : begin
        FTheLabel.Left := FTheLabel.FocusControl.Left + FTheLabel.FocusControl.Width - FTheLabel.Width;
        FTheLabel.Top := FTheLabel.FocusControl.Top - FTheLabel.Height - Indent;
      end;
    end;
    if FMaxWidth <> 0 then begin
      FTheLabel.AutoSize := False;
      FTheLabel.Width := FMaxWidth;
      FTheLabel.WordWrap := True;
      FTheLabel.Height := Max(FTheLabel.Height, FTheLabel.FocusControl.Height);
    end;
  end;
end;

constructor TsBoundLabel.Create(AOwner: TObject; CommonData : TsCommonData);
begin
  FCommonData := CommonData;
  FFont := TFont.Create;
  FActive := False;
end;

destructor TsBoundLabel.Destroy;
begin
  FreeAndNil(FFont);
  if Assigned(FTheLabel) then FreeAndNil(FTheLabel);
  inherited Destroy;
end;

function TsBoundLabel.GetFont: TFont;
begin
  if Assigned(FTheLabel) and Assigned(FTheLabel.Font) then Result := FTheLabel.Font else Result := FFont;
end;

function TsBoundLabel.GetUseSkin: boolean;
begin
  if Assigned(FTheLabel) then Result := FTheLabel.UseSkinColor else Result := True;
end;

procedure TsBoundLabel.SetActive(const Value: boolean);
begin
  if FActive = Value then Exit;
  if Value then begin
    FActive := True;
    if FTheLabel = nil then FTheLabel := TsEditLabel.InternalCreate(FCommonData.FOwnerControl, Self);
    FTheLabel.Visible := False;
    FTheLabel.Parent := FCommonData.FOwnerControl.Parent;
    FTheLabel.FocusControl := TWinControl(FCommonData.FOwnerControl);

    UpdateAlignment;
    FTheLabel.Name := FCommonData.FOwnerControl.Name + 'Label';
    if FText = '' then FText := FCommonData.FOwnerControl.Name;
    FTheLabel.Caption := FText;
    FTheLabel.Visible := FCommonData.FOwnerControl.Visible or (csDesigning in FTheLabel.ComponentState);
    FTheLabel.Enabled := FCommonData.FOwnerControl.Enabled;
    AlignLabel;
  end
  else begin
    if Assigned(FTheLabel) then FreeAndNil(FTheLabel);
    FActive := False;
  end;
end;

procedure TsBoundLabel.SetFont(const Value: TFont);
begin
  FTheLabel.Font.Assign(Value);
  FTheLabel.ParentFont := False;
  AlignLabel;
end;

procedure TsBoundLabel.SetIndent(const Value: integer);
begin
  if FIndent <> Value then begin
    FIndent := Value;
    if Active then AlignLabel;
  end;
end;

procedure TsBoundLabel.SetLayout(const Value: TsCaptionLayout);
begin
  if FLayout <> Value then begin
    FLayout := Value;
    UpdateAlignment;
    if Active then AlignLabel;
  end;
end;

procedure TsBoundLabel.SetMaxWidth(const Value: integer);
begin
  if FMaxWidth <> Value then begin
    FMaxWidth := Value;
    if Active then AlignLabel;
  end;
end;

procedure TsBoundLabel.SetText(const Value: acString); // gralex
begin
  if FText <> Value then begin
    FText := Value;
    if Active then begin
      FTheLabel.Caption := Value;
      AlignLabel;
    end;
  end;
end;

procedure TsBoundLabel.SetUseSkin(const Value: boolean);
begin
  if Assigned(FTheLabel) then FTheLabel.UseSkinColor := Value;
end;

procedure TsBoundLabel.UpdateAlignment;
begin
  if FTheLabel <> nil then case FLayout of
    sclTopLeft : TsLabel(FTheLabel).Alignment := taLeftJustify;
    sclTopCenter : TsLabel(FTheLabel).Alignment := taCenter;
    sclTopRight, sclLeft : TsLabel(FTheLabel).Alignment := taRightJustify;
  end;
end;

end.

