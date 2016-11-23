unit sFrameBar;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, sSpeedButton, sScrollBox, ImgList, Menus;

type
{$IFNDEF NOTFORHELP}
  TsTitleItem = class;
  TsTitles = class;
  TsTitleState = (stClosed, stOpened, stClosing, stOpening);
{$ENDIF} // NOTFORHELP

  TsFrameBar = class(TsScrollBox)
{$IFNDEF NOTFORHELP}
  private
    FItems: TsTitles;
    FTitleHeight: integer;
    FAnimation: boolean;
    FImages: TCustomImageList;
    FSpacing: integer;
    FAllowAllClose: boolean;
    FAllowAllOpen: boolean;
    FAutoFrameSize: boolean;
    FBorderWidth: integer;
    procedure SetItems(const Value: TsTitles);
    procedure SetTitleHeight(const Value: integer);
    procedure SetImages(const Value: TCustomImageList);
    function Offset : integer;
    procedure UpdateWidths;
    procedure SetSpacing(const Value: integer);
    function CalcClientRect : TRect;
    function CreateDefaultFrame : TFrame;
    function UpdateFrame(i, y, h, w : integer) : boolean;
    procedure SetAutoFrameSize(const Value: boolean);
    procedure SetAllowAllOpen(const Value: boolean);
    procedure SetBorderWidth(const Value: integer);
  public
    Arranging : boolean;
    Sizing : boolean;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure WndProc (var Message: TMessage); override;
{$ENDIF} // NOTFORHELP
    procedure ArrangeTitles;
    procedure ChangeSize(Index : integer; AllowAnimation : boolean; Height:integer);
    procedure OpenItem(Index : integer; AllowAnimation : boolean);
    procedure CloseItem(Index : integer; AllowAnimation : boolean);
    procedure ExpandAll(AllowAnimation : boolean);
    procedure CollapseAll(AllowAnimation : boolean);
    procedure Rearrange;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
{$IFNDEF NOTFORHELP}
    property Align default alLeft;
    property BorderStyle;
    property BorderWidth : integer read FBorderWidth write SetBorderWidth default 2;
{$ENDIF} // NOTFORHELP
    property AllowAllClose : boolean read FAllowAllClose write FAllowAllClose default False;
    property AllowAllOpen : boolean read FAllowAllOpen write SetAllowAllOpen default False;
    property Animation : boolean read FAnimation write FAnimation default True;
    property AutoFrameSize : boolean read FAutoFrameSize write SetAutoFrameSize;
    property Images : TCustomImageList read FImages write SetImages;
    property Items : TsTitles read FItems write SetItems;
    property TitleHeight : integer read FTitleHeight write SetTitleHeight default 28;
    property Spacing : integer read FSpacing write SetSpacing default 2;
  end;

{$IFNDEF NOTFORHELP}
  TsTitles = class(TCollection)
  private
    FOwner: TsFrameBar;
  protected
    function GetItem(Index: Integer): TsTitleItem;
    procedure SetItem(Index: Integer; Value: TsTitleItem);
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner : TsFrameBar);
    destructor Destroy; override;
    property Items[Index: Integer]: TsTitleItem read GetItem write SetItem; default;
  end;

  TsTitleButton = class(TsSpeedButton)
  protected
    Active : boolean;
    constructor InternalCreate(AOwner : TsFrameBar; Index : integer);
  public
    TitleItem : TsTitleItem; // v4.65
    function CurrentState : integer; override;
    property OnClick;
  end;
{$ENDIF} // NOTFORHELP

  TCreateFrameEvent = procedure (Sender: TObject; var Frame: TCustomFrame) of object;
  TFrameDestroyEvent = procedure (Sender: TObject; var Frame: TCustomFrame; var CanDestroy: boolean) of object;

  TsTitleItem = class(TCollectionItem)
{$IFNDEF NOTFORHELP}
  private
    FOwner: TsTitles;
    FCaption: string;
    FVisible: boolean;
    FOnCreateFrame: TCreateFrameEvent;
    FImageIndex: integer;
    FOnFrameDestroy: TFrameDestroyEvent;
    FOnClick: TNotifyEvent;
    FTag: Longint;
    procedure SetCaption(const Value: string);
    procedure SetVisible(const Value: boolean);
    procedure TitleButtonClick(Sender: TObject);
    function GetSkinSection: string;
    procedure SetSkinSection(const Value: string);
    procedure SetImageIndex(const Value: integer);
    function GetMargin: integer;
    function GetSpacing: integer;
    procedure SetMargin(const Value: integer);
    procedure SetSpacing(const Value: integer);
    function GetPopupMenu: TPopupMenu;
    procedure SetPopupMenu(const Value: TPopupMenu);
  public
{$ENDIF} // NOTFORHELP
    TitleButton : TsTitleButton;
    Frame : TCustomFrame;
    State : TsTitleState;
{$IFNDEF NOTFORHELP}
    FrameSize : integer;
    Closing : boolean;
    destructor Destroy; override;
    constructor Create(Collection: TCollection); override;
{$ENDIF} // NOTFORHELP
  published
    property Caption : string read FCaption write SetCaption;
    property ImageIndex : integer read FImageIndex write SetImageIndex default -1;
    property SkinSection : string read GetSkinSection write SetSkinSection;
    property Margin : integer read GetMargin write SetMargin default 5;
    property Spacing : integer read GetSpacing write SetSpacing default 8;
    property Tag : Longint read FTag write FTag default 0;
    property Visible : boolean read FVisible write SetVisible default True;
    property PopupMenu : TPopupMenu read GetPopupMenu write SetPopupMenu;
    property OnCreateFrame: TCreateFrameEvent read FOnCreateFrame write FOnCreateFrame;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnFrameDestroy: TFrameDestroyEvent read FOnFrameDestroy write FOnFrameDestroy;
  end;

implementation

uses sConst, sMessages, sSkinProps, sVCLUtils, sFrameAdapter, sLabel, stdctrls, acntUtils, acSBUtils;

{ TsTitles }
var
  DontAnim : boolean;

constructor TsTitles.Create(AOwner: TsFrameBar);
begin
  inherited Create(TsTitleItem);
  FOwner := AOwner;
end;

destructor TsTitles.Destroy;
begin
  inherited Destroy;
  FOwner := nil;
end;

function TsTitles.GetItem(Index: Integer): TsTitleItem;
begin
  Result := TsTitleItem(inherited GetItem(Index));
end;

function TsTitles.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TsTitles.SetItem(Index: Integer; Value: TsTitleItem);
begin
  inherited SetItem(Index, Value);
end;

{ TsFrameBar }
procedure TsFrameBar.ArrangeTitles;
const
  StepsCount = 3;
  DelayValue = 10;
var
  i, ii, sHeight, cWidth, AutoHeight : integer;
  cRect : TRect;
  Steps, sDiv : integer;
  CanDestroy : boolean;
  procedure SetActive(Index : integer; Active : boolean);
  begin
    if (Items[Index].TitleButton.Active <> Active) and (Items[Index].State in [stClosed, stOpened]) then begin
      Items[Index].TitleButton.Active := Active;
      Items[Index].TitleButton.SkinData.Invalidate;
    end;
  end;
begin
  if not visible or Arranging or (csReading in ComponentState) or (Items.Count = 0) then Exit;
  if not DontAnim and not (csDesigning in ComponentState) and FAnimation and Visible and not (csLoading in ComponentState) then Steps := StepsCount else Steps := 0;

  cRect := CalcClientRect;
  Arranging := True;
  sHeight := 0;
  AutoHeight := -1;
  if not ShowHintStored then begin
    AppShowHint := Application.ShowHint;
    Application.ShowHint := False;
    ShowHintStored := True;
  end;
  FadingForbidden := True;
  MouseForbidden := True;
  if AutoFrameSize then begin
    AutoScroll := False;
    sHeight := cRect.Top;
    for i := 0 to Items.Count - 1 do if Items[i].TitleButton.Visible and Items[i].Visible then begin
      inc(sHeight, FTitleHeight);
      if (Items[i].State in [stOpened, stOpening]) then inc(sHeight, BorderWidth);
      inc(sHeight, BorderWidth);
    end;
    AutoHeight := HeightOf(cRect) - sHeight;
  end;
  for ii := 0 to Steps do begin
    SkinData.BeginUpdate;
    Perform(WM_SETREDRAW, 0, 0);
    sHeight := cRect.Top;
    cWidth := WidthOf(cRect);
    for i := 0 to Items.Count - 1 do if Items[i].TitleButton.Visible and Items[i].Visible then begin
      Items[i].TitleButton.SetBounds(cRect.Left, sHeight - Offset, cWidth, FTitleHeight);
      if Items[i].TitleButton.Parent <> Self then Items[i].TitleButton.Parent := Self;
      inc(sHeight, FTitleHeight);
      sDiv := Items[i].FrameSize;
      if (sDiv = 0) and (Items[i].State = stOpening) and not Animation then Items[i].State := stOpened;
      case Items[i].State of
        stOpening : begin
          inc(sHeight, FSpacing);
          if (ii = Steps) and (AutoHeight <> -1) then begin
            sDiv := AutoHeight;
            Items[i].FrameSize := AutoHeight;
            if Items[i].Frame <> nil then Items[i].Frame.Height := AutoHeight
          end;
          if Steps <> 0 then sDiv := Round((sDiv / Steps) * ii);
          if UpdateFrame(i, sHeight - Offset, sDiv, cWidth) then begin
            if (ii = Steps) then begin
              Items[i].State := stOpened;
            end;
            if Steps > 0 then Sleep(DelayValue);
          end;
        end;
        stClosing : begin
        try
          if Steps = 0 then sDiv := 0 else sDiv := Round((sDiv / Steps) * (Steps - ii));
          if (ii = Steps) then begin
            Items[i].Closing := False;
            CanDestroy := True;

            if Assigned(Items[i].FOnFrameDestroy) then Items[i].FOnFrameDestroy(Self, Items[i].Frame, CanDestroy);
            if CanDestroy then FreeAndNil(Items[i].Frame);
            Items[i].FrameSize := 0;
            sDiv := 0;

            inc(sHeight, BorderWidth);
            Items[i].State := stClosed;
            SetActive(i, False);
            if Items[i].Frame <> nil then UpdateFrame(i, sHeight - Offset, sDiv, cWidth);
            Continue;
          end;
          UpdateFrame(i, sHeight - Offset, sDiv, cWidth);
          if Steps > 0 then Sleep(DelayValue);
        except
        end;
        end;
        stOpened : begin
          if AutoHeight <> -1 then begin
            sDiv := AutoHeight;
            Items[i].FrameSize := AutoHeight;
            if Items[i].Frame <> nil then Items[i].Frame.Height := AutoHeight
          end;
          UpdateFrame(i, sHeight - Offset, -1, cWidth);
          if (sDiv = 0) and (Items[i].Frame <> nil) then begin
            sDiv := Items[i].Frame.Height
          end;
        end;
        stClosed : begin
          if Items[i].Frame <> nil then begin
            CanDestroy := True;
            if Assigned(Items[i].FOnFrameDestroy) then Items[i].FOnFrameDestroy(Self, Items[i].Frame, CanDestroy);
            if CanDestroy then FreeAndNil(Items[i].Frame);
            Items[i].FrameSize := 0;
            sDiv := 0;
            if Items[i].Frame <> nil then UpdateFrame(i, sHeight - Offset, sDiv, cWidth);
            Items[i].FrameSize := 0;
          end
        end;
      end;
      if (Items[i].Frame <> nil) and (Items[i].State in [stOpened, stOpening, stClosing]) then begin
        if Items[i].Frame.Parent = nil then Items[i].Frame.Parent := Self;
        inc(sHeight, sDiv + BorderWidth);
      end;
      if (Items[i].Frame <> nil) and (Items[i].State = stOpened) then begin
        SetWindowRgn(Items[i].Frame.Handle, 0, False);
      end;
      inc(sHeight, BorderWidth);
      SetActive(i, Items[i].State in [stOpened, stOpening]);
    end;
    Perform(WM_SETREDRAW, 1, 0);
    SkinData.EndUpdate;
    if Showing then begin
      RedrawWindow(Handle, nil, 0, RDW_ALLCHILDREN or RDW_INVALIDATE or RDW_ERASE or RDW_FRAME);
      SetParentUpdated(Self);
    end;
    if Assigned(acMagnForm) then SendMessage(acMagnForm.Handle, SM_ALPHACMD, MakeWParam(0, AC_REFRESH), 0);
    if Steps > 0 then Application.ProcessMessages;
  end;
  FadingForbidden := False;
  inc(sHeight, BorderWidth + 2 * integer(BorderStyle = bsSingle));
  if VertScrollBar.Range <> sHeight then VertScrollBar.Range := sHeight;
  Arranging := False;
  UpdateWidths;
  if Showing then
      RedrawWindow(Handle, nil, 0, RDW_ALLCHILDREN or RDW_INVALIDATE or RDW_ERASE or RDW_FRAME);
  if Assigned(acMagnForm) then SendMessage(acMagnForm.Handle, SM_ALPHACMD, MakeWParam(0, AC_REFRESH), 0);
  MouseForbidden := False;
  Application.ShowHint := AppShowHint;
  ShowHintStored := False;
end;

function TsFrameBar.CalcClientRect: TRect;
begin
  Result := Rect(0, 0, Width - 4 * integer(BorderStyle = bsSingle), Height);
  InflateRect(Result,  - BorderWidth - 2 * integer(BorderStyle = bsSingle), - BorderWidth - 2 * integer(BorderStyle = bsSingle));
  if Parent = nil then Exit;
  if GetWindowLong(Handle, GWL_STYLE) and WS_VSCROLL = WS_VSCROLL then dec(Result.Right, GetSystemMetrics(SM_CXVSCROLL));
end;

procedure TsFrameBar.ChangeSize(Index: integer; AllowAnimation: boolean; Height: integer);
begin

  if Assigned(Items[Index].Frame) then begin
    Items[Index].FrameSize := Height;
    Items[Index].Frame.Height := Height;
  end;
  Items[Index].FrameSize := Height;

  if AllowAnimation then Items[Index].State := stOpening else Items[Index].State := stOpened;

  DontAnim := not AllowAnimation;
  ArrangeTitles;
  DontAnim := False;
end;

procedure TsFrameBar.CloseItem(Index: integer; AllowAnimation: boolean);
begin

  if AllowAnimation then Items[Index].State := stClosing else Items[Index].State := stClosed;
  DontAnim := not AllowAnimation;
  ArrangeTitles;
  DontAnim := False;
end;

procedure TsFrameBar.CollapseAll(AllowAnimation : boolean);
var
  i : integer;
begin

  for i := 0 to Items.Count - 1 do if AllowAnimation then Items[i].State := stClosing else Items[i].State := stClosed;
  ArrangeTitles;
end;

constructor TsFrameBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SkinData.COC := COC_TsFrameBar;
  FItems := TsTitles.Create(Self);

  Caption := ' ';
  Align := alLeft;
  BevelOuter := bvLowered;
  FTitleHeight := 28;
  VertScrollBar.Tracking := True;
  HorzScrollBar.Visible := False;
  FBorderWidth := 2;
  FAnimation := True;
  FAllowAllClose := False;
  FAllowAllOpen := False;
end;

function TsFrameBar.CreateDefaultFrame: TFrame;
begin

  Result := TFrame.Create(Self);
  Result.Height := 150;
  with TsFrameAdapter.Create(Result) do begin
    SkinData.SkinManager := Self.SkinData.FSkinManager;
    SkinData.SkinSection := s_BarPanel;
  end;
  with TsLabel.Create(Result) do begin
    Align := alClient;
    Caption := 'Frame creation'#13#10'event has not been defined.';
    Alignment := taCenter;
    Layout := tlCenter;
    WordWrap := True;
    Font.color := clRed;
    Parent := Result;
  end;
end;

destructor TsFrameBar.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TsFrameBar.ExpandAll(AllowAnimation : boolean);
var
  i : integer;
begin

  for i := 0 to Items.Count - 1 do if AllowAnimation then Items[i].State := stOpening else Items[i].State := stOpened;
  ArrangeTitles;
end;

procedure TsFrameBar.Loaded;
var
  i : integer;
begin
  inherited;
  for i := 0 to Items.Count - 1 do Items[i].TitleButton.SkinData.SkinManager := SkinData.FSkinManager;
  if Visible then Rearrange
end;

procedure TsFrameBar.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Images) then Images := nil;
end;

function TsFrameBar.Offset: integer;
begin
  if Assigned(ListSW) and (ListSW.sBarVert <> nil) and ListSW.sBarVert.fScrollVisible
    then Result := ListSW.sBarVert.ScrollInfo.nPos else Result := 0
end;

procedure TsFrameBar.OpenItem(Index: integer; AllowAnimation: boolean);
var
  i : integer;
begin

  if AllowAnimation then Items[Index].State := stOpening else Items[Index].State := stOpened;
  if not AllowAllOpen then begin
    for i := 0 to Items.Count - 1 do if Items[i].State = stOpened then Items[i].State := stClosing;
    Items[Index].State := stOpened;
  end;
  DontAnim := not AllowAnimation;
  ArrangeTitles;
  DontAnim := False;
end;

procedure TsFrameBar.Rearrange;
begin
  DontAnim := True;
  ArrangeTitles;
  DontAnim := False;
end;

procedure TsFrameBar.SetAllowAllOpen(const Value: boolean);
begin
  if FAllowAllOpen <> Value then begin
    if Value and FAutoFrameSize then FAutoFrameSize := False;
    FAllowAllOpen := Value;
    if not (csLoading in ComponentState) then Rearrange;
  end;
end;

procedure TsFrameBar.SetAutoFrameSize(const Value: boolean);
begin
  if FAutoFrameSize <> Value then begin
    if Value then begin
      if AllowAllOpen then AllowAllOpen := False;
      AutoScroll := False;
    end;
    FAutoFrameSize := Value;
    if not (csLoading in ComponentState) then Rearrange;
  end;
end;

procedure TsFrameBar.SetBorderWidth(const Value: integer);
begin
  if FBorderWidth <> Value then begin
    FBorderWidth := Value;
    RecreateWnd;
    if not (csLoading in ComponentState) then Rearrange;
  end;
end;

procedure TsFrameBar.SetImages(const Value: TCustomImageList);
var
  i : integer;
begin
  if FImages <> Value then begin
    FImages := Value;
    for i := 0 to Items.Count - 1 do if Items[i].TitleButton.Visible then Items[i].TitleButton.Images := Images;
  end;
end;

procedure TsFrameBar.SetItems(const Value: TsTitles);
begin
  FItems.Assign(Value);
end;

procedure TsFrameBar.SetSpacing(const Value: integer);
begin

  if FSpacing <> Value then begin
    FSpacing := Value;
    if not (csLoading in ComponentState) then Rearrange;
  end;
end;

procedure TsFrameBar.SetTitleHeight(const Value: integer);
begin

  if FTitleHeight <> Value then begin
    FTitleHeight := Value;
    if not (csLoading in ComponentState) then Rearrange;
  end;
end;

function TsFrameBar.UpdateFrame(i, y, h, w : integer) : boolean;
var
  rgn : hrgn;
begin
  Result := False;
  if Items.Count <= i then Exit;
  if (Items[i].Frame = nil) and not (csDesigning in ComponentState) then begin
    if Assigned(Items[i].OnCreateFrame)
      then Items[i].OnCreateFrame(Items[i], Items[i].Frame)
      else Items[i].Frame := CreateDefaultFrame;
  end;
  if (Items[i].Frame <> nil) then begin
    if (Items[i].FrameSize = 0) then Items[i].FrameSize := Items[i].Frame.Height;
    if h = -1 then begin
      h := Items[i].FrameSize; // if frame has not been created
      Items[i].Frame.Height := Items[i].FrameSize;
    end;
    if h = 0 then begin
      rgn := CreateRectRgn(-1, -1, -1, -1);
      SetWindowRgn(Items[i].Frame.Handle, rgn, False);
      Items[i].Frame.Visible := False;
    end
    else if h = Items[i].Frame.Height then begin
      rgn := CreateRectRgn(0, 0, Items[i].Frame.Width, Items[i].Frame.Height);
      SetWindowRgn(Items[i].Frame.Handle, rgn, False);
      Items[i].Frame.Visible := True;
    end
    else begin
      rgn := CreateRectRgn(0, Items[i].Frame.Height - h, w, Items[i].Frame.Height);
      SetWindowRgn(Items[i].Frame.Handle, rgn, False);
      Items[i].Frame.Visible := True;
    end;
    Items[i].Frame.SetBounds(Items[i].TitleButton.Left, y - (Items[i].Frame.Height - h), w, Items[i].Frame.Height);
    Result := True
  end
  else Result := False;
end;

procedure TsFrameBar.UpdateWidths;
var
  i, cWidth : integer;
begin
  Arranging := True;
  cWidth := WidthOf(CalcClientRect);
  for i := 0 to Items.Count - 1 do if Items[i].TitleButton.Visible and Items[i].Visible then begin
    if Items[i].TitleButton.Width <> cWidth then begin
      Items[i].TitleButton.SkinData.BGChanged := True;
      Items[i].TitleButton.Width := cWidth;
    end;
    if (Items[i].Frame <> nil) and (Items[i].Frame.Width <> cWidth) then begin
      Items[i].Frame.Width := cWidth;
    end;
  end;
  Arranging := False;
  if AutoScroll then UpdateScrolls(ListSW);
end;

procedure TsFrameBar.WndProc(var Message: TMessage);
var
  i : integer;
begin
  inherited;
  case Message.Msg of
    WM_SIZE : if Showing then begin
      if AutoFrameSize then Rearrange else begin
        UpdateWidths;
        SendMessage(Handle, WM_NCPAINT, 0, 0);
      end;
    end;
    CM_VISIBLECHANGED : if Showing then begin
      Rearrange;
    end;
    CM_ENABLEDCHANGED: begin
      for i := 0 to Items.Count - 1 do begin
        Items[i].TitleButton.Enabled := Enabled;
        if Items[i].Frame <> nil then Items[i].Frame.Enabled := Enabled;
      end;
      Repaint
    end;
  end;
  if Message.Msg = cardinal(SM_ALPHACMD) then case Message.WParamHi of
    AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) and SkinData.Skinned then UpdateWidths
  end;
end;

{ TsTitleItem }

constructor TsTitleItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  FTag := 0;
  FOwner := TsTitles(Collection);
  TitleButton := TsTitleButton.InternalCreate(FOwner.FOwner, Index);
  TitleButton.TitleItem := Self;
  TitleButton.OnClick := TitleButtonClick;
  FVisible := True;
  DontAnim := True;
  FOwner.FOwner.ArrangeTitles;
  DontAnim := False;
  FImageIndex := -1;
  State := stClosed;
end;

destructor TsTitleItem.Destroy;
begin
  if not (csDestroying in FOwner.FOwner.ComponentState) and (TitleButton <> nil) then begin
    TitleButton.Visible := False;
    TitleButton.Free;
    TitleButton := nil;
    if Frame <> nil then FreeAndNil(Frame);
  end;
  inherited Destroy;
  if not (csDestroying in FOwner.FOwner.ComponentState) then FOwner.FOwner.ArrangeTitles;
end;

function TsTitleItem.GetMargin: integer;
begin
  Result := TitleButton.Margin; 
end;

function TsTitleItem.GetPopupMenu: TPopupMenu;
begin
  if TitleButton <> nil then Result := TitleButton.PopupMenu else Result := nil
end;

function TsTitleItem.GetSkinSection: string;
begin
  Result := TitleButton.SkinData.SkinSection;
end;

function TsTitleItem.GetSpacing: integer;
begin
  if Result <> TitleButton.Spacing then begin
    Result := TitleButton.Spacing;
    if csDesigning in TitleButton.ComponentState then TitleButton.SkinData.Invalidate;
  end;
end;

procedure TsTitleItem.SetCaption(const Value: string);
begin
  TitleButton.Caption := Value;
  FCaption := Value;
end;

procedure TsTitleItem.SetImageIndex(const Value: integer);
begin
  if FImageIndex <> Value then begin
    FImageIndex := Value;
    TitleButton.ImageIndex := Value;
    if TitleButton.Images <> FOwner.FOwner.Images then TitleButton.Images := FOwner.FOwner.Images
  end;
end;

procedure TsTitleItem.SetMargin(const Value: integer);
begin
  if TitleButton.Margin <> Value then begin
    TitleButton.Margin := Value;
    if csDesigning in TitleButton.ComponentState then TitleButton.SkinData.Invalidate;
  end;
end;

procedure TsTitleItem.SetPopupMenu(const Value: TPopupMenu);
begin
  if TitleButton <> nil then TitleButton.PopupMenu := Value;
end;

procedure TsTitleItem.SetSkinSection(const Value: string);
begin

  TitleButton.SkinData.SkinSection := Value
end;

procedure TsTitleItem.SetSpacing(const Value: integer);
begin
  TitleButton.Spacing := Value;
end;

procedure TsTitleItem.SetVisible(const Value: boolean);
begin
  if FVisible <> Value then begin
    FVisible := Value;
    if Value then begin
      TitleButton.SkinData.UpdateIndexes;
      TitleButton.Parent := FOwner.FOwner;
    end
    else TitleButton.Parent := nil;
    FOwner.FOwner.ArrangeTitles;
  end;
end;

procedure TsTitleItem.TitleButtonClick;
var
  i : integer;
begin
  if (csDesigning in FOwner.FOwner.ComponentState) then Exit;
  if Assigned(TitleButton) and Assigned(FOnClick) then FOnClick(TitleButton);
  case State of
    stClosed : begin
      State := stOpening;
      if not FOwner.FOwner.AllowAllOpen
        then for i := 0 to FOwner.Count - 1 do if FOwner[i].State = stOpened then FOwner[i].State := stClosing;
    end;
    stOpened : if FOwner.FOwner.AllowAllClose then FOwner[Index].State := stClosing;
  end;
  FOwner.FOwner.ArrangeTitles;
end;

{ TsTitleButton }
function TsTitleButton.CurrentState: integer;
begin
  Result := inherited CurrentState;
  if (Result = 0) and Active then Result := 1;
end;

constructor TsTitleButton.InternalCreate(AOwner: TsFrameBar; Index: integer);
var
  i : Integer;
begin
  inherited Create(AOwner);
  SkinData.COC := COC_TsBarTitle;
  i := 0;
  repeat
    inc(i);
    if AOwner.FindComponent('sTitleButton' + IntToStr(i)) = nil then begin
      Name := 'sTitleButton' + IntToStr(i);
      break;
    end;
  until False;
  Alignment := taLeftJustify;
  Spacing := 8;
  Margin := 5;
end;

end.
