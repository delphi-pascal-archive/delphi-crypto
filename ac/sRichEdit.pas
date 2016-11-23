unit sRichEdit;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, sConst, sCommonData, sDefaults, acSBUtils
  {$IFDEF TNTUNICODE}, TntComCtrls {$ENDIF} ;

type
{$IFDEF TNTUNICODE}
  TsRichEdit = class(TTntRichEdit)
{$ELSE}
  TsRichEdit = class(TRichEdit)
{$ENDIF}
{$IFNDEF NOTFORHELP}
  private
    FCommonData: TsCommonData;
    FDisabledKind: TsDisabledKind;
    FBoundLabel: TsBoundLabel;
    procedure SetDisabledKind(const Value: TsDisabledKind);
  protected
    procedure PrepareCache;
  public
    ListSW : TacScrollWnd;
    procedure AfterConstruction; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure WndProc (var Message: TMessage); override;
  published
    property Text;
{$ENDIF} // NOTFORHELP
    property BoundLabel : TsBoundLabel read FBoundLabel write FBoundLabel;
    property SkinData : TsCommonData read FCommonData write FCommonData;
    property DisabledKind : TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
  end;

implementation

uses sStyleSimply, sVCLUtils, sMessages, sMaskData, acntUtils, sGraphUtils, sAlphaGraph,
  sSkinProps, RichEdit {$IFDEF LOGGED}, sDebugMsgs{$ENDIF}, CommCtrl;

{ TsRichEdit }

procedure TsRichEdit.AfterConstruction;
begin
  inherited AfterConstruction;
  UpdateData(FCommonData);
end;

constructor TsRichEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommonData := TsCommonData.Create(Self, {$IFDEF DYNAMICCACHE} False {$ELSE} True {$ENDIF});
  FCommonData.COC := COC_TsMemo;
  if FCommonData.SkinSection = '' then FCommonData.SkinSection := s_Edit;
  FDisabledKind := DefDisabledKind;
  FBoundLabel := TsBoundLabel.Create(Self, FCommonData);
  Perform(WM_USER + 53{EM_EXLIMITTEXT}, 0, $7FFFFFF0);
end;

destructor TsRichEdit.Destroy;
begin
  if ListSW <> nil then FreeAndNil(ListSW);
  FreeAndNil(FBoundLabel);
  if Assigned(FCommonData) then FreeAndNil(FCommonData);
  inherited Destroy;
end;

procedure TsRichEdit.Loaded;
begin
  inherited Loaded;
  FCommonData.Loaded;
end;

procedure TsRichEdit.PrepareCache;
begin
  FCommonData.InitCacheBmp;
  PaintItem(FCommonData,
                GetParentCache(FCommonData), True,
                integer(ControlIsActive(FCommonData)), Rect(0, 0, Width, Height),
                Point(Left, top), FCommonData.FCacheBmp, False);
  if not Enabled then BmpDisabledKind(FCommonData.FCacheBmp, FDisabledKind, Parent, GetParentCache(FCommonData), Point(Left, Top));
end;

procedure TsRichEdit.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FCommonData.Invalidate;
  end;
end;

procedure TsRichEdit.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_CTRLHANDLED : begin Message.Result := 1; Exit end; // AlphaSkins supported
    AC_GETAPPLICATION : begin Message.Result := longint(Application); Exit end;
    AC_REMOVESKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) and not (csDestroying in ComponentState) then begin
      if ListSW <> nil then FreeAndNil(ListSW);
      CommonWndProc(Message, FCommonData);
      if not FCommonData.CustomFont then begin
        DefAttributes.Color := Font.Color;
      end;
      RecreateWnd;
      exit
    end;
    AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) and Visible then begin
      CommonWndProc(Message, FCommonData);
      if FCommonData.Skinned then begin
        if not FCommonData.CustomFont and (DefAttributes.Color <> FCommonData.SkinManager.gd[FCommonData.SkinIndex].FontColor[1]) then begin
//v5.43          SkinData.Updating := True;
          DefAttributes.Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].FontColor[1];
//v5.43          SkinData.Updating := False;
        end;
      end;
      RefreshEditScrolls(SkinData, ListSW);
      SendMessage(Handle, WM_NCPaint, 0, 0);
      exit
    end;
    AC_SETNEWSKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      BorderStyle := bsSingle;
      Ctl3D := True;
      CommonWndProc(Message, FCommonData);
      exit
    end;
    AC_GETDISKIND : acGlobalDisabledKind := DisabledKind;
  end;
  if not ControlIsReady(Self) or not Assigned(FCommonData) or not FCommonData.Skinned then inherited else begin
    if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
      AC_ENDPARENTUPDATE : if FCommonData.Updating then begin
        FCommonData.Updating := False;
        Perform(WM_NCPAINT, 0, 0); Exit
      end;
    end
    else case Message.Msg of
      WM_ENABLE : Exit;
(*//v5.43      WM_USER + 68{EM_SETCHARFORMAT} : begin // Prevent of OnChange event calling in RichEdit
        if SkinData.Updating then Exit;
      end;*)
    end;
    CommonWndProc(Message, FCommonData);
    inherited;
    case Message.Msg of
      TB_SETANCHORHIGHLIGHT, WM_SIZE : SendMessage(Handle, WM_NCPAINT, 0, 0);
      WM_PRINT : begin
        ControlState := ControlState + [csPaintCopy];
        SendMessage(Handle, WM_PAINT, Message.WParam, 0);
        ControlState := ControlState - [csPaintCopy];
      end;
      CM_SHOWINGCHANGED : RefreshEditScrolls(SkinData, ListSW);
      CM_ENABLEDCHANGED : begin
        if not FCommonData.CustomColor then Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].Color;
        if not FCommonData.CustomFont then begin
          if not Enabled then begin
            Font.Color := AverageColor(FCommonData.SkinManager.gd[FCommonData.SkinIndex].FontColor[1], Color);
            DefAttributes.Color := Font.Color;
          end
          else begin
            Font.Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].FontColor[1];
            DefAttributes.Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].FontColor[1];
          end;
        end;
      end;
    end;
  end;
  // Aligning of the bound label
  if Assigned(BoundLabel) and Assigned(BoundLabel.FtheLabel) then case Message.Msg of
    WM_SIZE, WM_WINDOWPOSCHANGED : begin BoundLabel.AlignLabel end;
    CM_VISIBLECHANGED : begin BoundLabel.FtheLabel.Visible := Visible; BoundLabel.AlignLabel end;
    CM_ENABLEDCHANGED : begin BoundLabel.FtheLabel.Enabled := Enabled; BoundLabel.AlignLabel end;
    CM_BIDIMODECHANGED : begin BoundLabel.FtheLabel.BiDiMode := BiDiMode; BoundLabel.AlignLabel end;
  end;
end;

end.
