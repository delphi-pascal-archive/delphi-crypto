unit sMemo;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, sConst, sCommonData
  {$IFDEF TNTUNICODE}, TntStdCtrls, TntClasses, TntSysUtils, TntActnList, TntControls{$ENDIF},
  sDefaults, acSBUtils{$IFDEF LOGGED}, sDebugMsgs{$ENDIF};

type
{$IFDEF TNTUNICODE}
  TsMemo = class(TTntMemo)
{$ELSE}
  TsMemo = class(TMemo)
{$ENDIF}
{$IFNDEF NOTFORHELP}
  private
    FCommonData: TsCommonData;
    FDisabledKind: TsDisabledKind;
    FOnVScroll: TNotifyEvent;
    FOnScrollCaret: TNotifyEvent;
    FBoundLabel: TsBoundLabel;
    procedure SetDisabledKind(const Value: TsDisabledKind);
  public
    ListSW : TacScrollWnd;
    procedure AfterConstruction; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure WndProc (var Message: TMessage); override;
  published
    property Text;
    property CharCase;
    Property OnScrollCaret : TNotifyEvent read FOnScrollCaret write FOnScrollCaret;
    Property OnVScroll : TNotifyEvent read FOnVScroll write FOnVScroll;
{$ENDIF} // NOTFORHELP
    property BoundLabel : TsBoundLabel read FBoundLabel write FBoundLabel;
    property SkinData : TsCommonData read FCommonData write FCommonData;
    property DisabledKind : TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
{$IFDEF TNTUNICODE}
    property SelText;
    property SelStart;
    property SelLength;
{$ENDIF}
  end;

implementation

uses sVCLUtils, sMessages, sGraphUtils, sAlphaGraph, sSkinProps;

{ TsMemo }

procedure TsMemo.AfterConstruction;
begin
  inherited AfterConstruction;
  FCommonData.Loaded;
end;


constructor TsMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csOpaque];
  FCommonData := TsCommonData.Create(Self, {$IFDEF DYNAMICCACHE} False {$ELSE} True {$ENDIF});
  FCommonData.COC := COC_TsMemo;
  if FCommonData.SkinSection = '' then FCommonData.SkinSection := s_Edit;
  FDisabledKind := DefDisabledKind;
  FBoundLabel := TsBoundLabel.Create(Self, FCommonData);
end;

destructor TsMemo.Destroy;
begin
  if ListSW <> nil then FreeAndNil(ListSW);
  FreeAndNil(FBoundLabel);
  if Assigned(FCommonData) then FreeAndNil(FCommonData);
  inherited Destroy;
end;


procedure TsMemo.Loaded;
begin
  inherited Loaded;
  FCommonData.Loaded;
  RefreshEditScrolls(SkinData, ListSW);
end;

procedure TsMemo.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FCommonData.Invalidate;
  end;
end;


procedure TsMemo.WndProc(var Message: TMessage);
var
  PS : TPaintStruct;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  if (Message.Msg = CM_FONTCHANGED) and Showing and HandleAllocated then RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_FRAME);
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of
    AC_CTRLHANDLED : begin Message.Result := 1; Exit end; // AlphaSkins supported
    AC_GETAPPLICATION : begin Message.Result := longint(Application); Exit end;
    AC_SETNEWSKIN : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FCommonData);
      exit
    end;
    AC_REMOVESKIN : if LongWord(Message.LParam) = LongWord(SkinData.SkinManager) then begin
      if ListSW <> nil then FreeAndNil(ListSW);
      CommonWndProc(Message, FCommonData);
      RecreateWnd;
      exit
    end;
    AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      CommonWndProc(Message, FCommonData);
      RefreshEditScrolls(SkinData, ListSW);
      RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_ERASE or RDW_FRAME);
      exit
    end
  end;
  if not ControlIsReady(Self) or not FCommonData.Skinned then inherited else begin
    case Message.Msg of
      WM_PAINT : begin
        FCommonData.Updating := FCommonData.Updating;
        if FCommonData.Updating then begin // Exit if parent is not ready yet
          BeginPaint(Handle, PS);
          EndPaint(Handle, PS);
          Exit;
        end;
        inherited;
        exit;
      end;
    end;
    CommonWndProc(Message, FCommonData);
    inherited;
    case Message.Msg of
      CM_SHOWINGCHANGED : RefreshEditScrolls(SkinData, ListSW);
      CM_VISIBLECHANGED, CM_ENABLEDCHANGED, WM_SETFONT : begin
        FCommonData.Invalidate;
      end;
      CM_TEXTCHANGED, CM_CHANGED : if Assigned(ListSW) then UpdateScrolls(ListSW, True);
      EM_SETSEL : if Assigned(FOnScrollCaret) then FOnScrollCaret(Self);
      WM_HSCROLL, WM_VSCROLL : begin
        if (Message.Msg = WM_VSCROLL) and Assigned(FOnVScroll) then begin
          FOnVScroll(Self);
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

