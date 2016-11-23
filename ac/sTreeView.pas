unit sTreeView;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, sConst, sCommonData,
  sMessages, Commctrl, acSBUtils {$IFDEF TNTUNICODE}, TntComCtrls{$ENDIF};

type
{$IFDEF TNTUNICODE}
  TsTreeView = class(TTntTreeView)
{$ELSE}
  TsTreeView = class(TTreeView)
{$ENDIF}
{$IFNDEF NOTFORHELP}
  private
    FCommonData: TsCommonData;
    FBoundLabel: TsBoundLabel;
  protected
    procedure WndProc (var Message: TMessage); override;
    procedure Loaded; override;
  public
    ListSW : TacScrollWnd;
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
  published
{$ENDIF} // NOTFORHELP
    property BoundLabel : TsBoundLabel read FBoundLabel write FBoundLabel;
    property SkinData : TsCommonData read FCommonData write FCommonData;
  end;

implementation

uses sMaskData, sVclUtils, sStyleSimply, acntUtils, sGraphUtils, math, sAlphaGraph,
  sSkinProps{$IFDEF LOGGED}, sDebugMsgs{$ENDIF}, sSKinManager;

{ TsTreeView }

procedure TsTreeView.AfterConstruction;
begin
  inherited;
  SkinData.Loaded;
end;

constructor TsTreeView.Create(AOwner: TComponent);
begin
  inherited;
  FCommonData := TsCommonData.Create(Self, True);
  FCommonData.COC := COC_TsTreeView;
  if FCommonData.SkinSection = '' then FCommonData.SkinSection := s_Edit;
  FBoundLabel := TsBoundLabel.Create(Self, FCommonData);
end;

destructor TsTreeView.Destroy;
begin
  if ListSW <> nil then FreeAndNil(ListSW);
  FreeAndNil(FBoundLabel);
  if Assigned(FCommonData) then FreeAndNil(FCommonData);
  inherited Destroy;
end;
{
function TsTreeView.GetNodeFromItem(const Item: TTVItem): TTreeNode;
begin
  Result := nil;
  if Items <> nil then
    with Item do
      if (state and TVIF_PARAM) <> 0 then Result := Pointer(lParam)
      else Result := Items.GetNode(hItem);
end;
}
procedure TsTreeView.Loaded;
begin
  inherited Loaded;
  FCommonData.Loaded;
  if FCommonData.Skinned then begin
    if not FCommonData.CustomColor then Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].Color;
    if not FCommonData.CustomFont then Font.Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].FontColor[1];
  end;
  RefreshTreeScrolls(SkinData, ListSW);
end;

procedure TsTreeView.WndProc(var Message: TMessage);
begin
{  case Message.Msg of
    TVM_GETNEXTITEM, TVM_GETITEMA, TVM_SETITEMA, TVM_INSERTITEM, TVM_GETITEMRECT : begin
      inherited; exit;
    end;
  end;}
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_CTRLHANDLED : begin Message.Result := 1; Exit end; // AlphaSkins supported
    AC_GETAPPLICATION : begin Message.Result := longint(Application); Exit end;
    AC_REMOVESKIN : if LongWord(Message.LParam) = LongWord(SkinData.SkinManager) then begin
      if ListSW <> nil then FreeAndNil(ListSW);
      CommonWndProc(Message, FCommonData);
      if not FCommonData.CustomColor then Color := clWindow;
      if not FCommonData.CustomFont then Font.Color := clWindowText;
      exit
    end;
    AC_SETNEWSKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FCommonData);
      exit
    end;
    AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FCommonData);
      if FCommonData.Skinned then begin
        if not FCommonData.CustomColor then Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].Color;
        if not FCommonData.CustomFont then Font.Color := FCommonData.SkinManager.gd[FCommonData.SkinIndex].FontColor[1];
      end;
      RefreshTreeScrolls(SkinData, ListSW);
      exit
    end;
    AC_ENDPARENTUPDATE : if FCommonData.Updating then begin
      FCommonData.Updating := False;
      Perform(WM_NCPAINT, 0, 0);
      Exit
    end;
  end;
  if not ControlIsReady(Self) then inherited else begin
    if Assigned(FCommonData) and FCommonData.Skinned then begin
      case Message.Msg of
        CM_VISIBLECHANGED, CM_ENABLEDCHANGED, WM_MOVE : begin
          FCommonData.BGChanged := True;
        end;
//        WM_ERASEBKGND : Exit;
{        WM_PRINT : begin
          Perform(WM_PAINT, Message.WParam, Message.LParam);
          Perform(WM_NCPAINT, Message.WParam, Message.LParam);
          Exit;
        end;}
      end;
    end;
    CommonWndProc(Message, FCommonData);
    inherited;
    case Message.Msg of
      CM_SHOWINGCHANGED : RefreshTreeScrolls(SkinData, ListSW);
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
