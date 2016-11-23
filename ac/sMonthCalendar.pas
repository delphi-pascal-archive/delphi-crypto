unit sMonthCalendar;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses Windows, Classes, Controls, SysUtils, Graphics, buttons, grids,
  messages, acntUtils, sPanel, sGraphUtils, sConst, StdCtrls, forms, comctrls,
  sMessages, sDateUtils, sSpeedButton, sDefaults, Menus;

type
{$IFNDEF NOTFORHELP}
  TGetCellParams = procedure(Sender: TObject; Date: TDatetime; AFont: TFont; var Background: TColor) of object;
  TsMonthCalendar = class;

  TsCalendGrid = class(TDrawGrid)
  private
    FOwner: TsMonthCalendar;
    Clicked : boolean;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    procedure Click; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure Paint; override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure AfterConstruction; override;
    constructor Create(AOwner: TComponent); override;
    procedure MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
    property GridLineWidth;
    property DefaultColWidth;
    property DefaultRowHeight;
  end;
{$ENDIF} // NOTFORHELP

{ TsMonthCalendar }

  TsMonthCalendar = class(TsPanel)
{$IFNDEF NOTFORHELP}
  private
    FDate: TDateTime;
    FMonthOffset: Integer;
    FOnChange: TNotifyEvent;
    FReadOnly: Boolean;
    FStartOfWeek: TCalDayOfWeek;
    FUpdating: Boolean;
    FUseCurrentDate: Boolean;
    FWeekends: TDaysOfWeek;
    FWeekendColor: TColor;
    FShowTitle: boolean;
    FOnGetCellParams: TGetCellParams;
    FShowCurrentDate: boolean;
    FOnDrawDay: TGetCellParams;
    FShowWeeks: boolean;
    FShowTodayBtn: boolean;
    function GetCellText(ACol, ARow: Integer): string;
    function GetDateElement(Index: Integer): Integer;
    procedure SetCalendarDate(Value: TDateTime);
    procedure SetDateElement(Index: Integer; Value: Integer);
    procedure SetStartOfWeek(Value: TCalDayOfWeek);
    procedure SetUseCurrentDate(Value: Boolean);
    procedure SetWeekendColor(Value: TColor);
    procedure SetWeekends(Value: TDaysOfWeek);
    function IsWeekend(ACol, ARow: Integer): Boolean;
    procedure CalendarUpdate(DayOnly: Boolean);
    function StoreCalendarDate: Boolean;
    function FirstDay: integer;
    procedure TopPanelDblClick(Sender: TObject);
    procedure SetShowTitle(const Value: boolean);
    procedure SetShowCurrentDate(const Value: boolean);
    procedure SetShowWeeks(const Value: boolean);
    procedure SetShowTodayBtn(const Value: boolean);
  protected
    PopMenu : TPopupMenu;
    procedure Change; dynamic;
    procedure ChangeMonth(Delta: Integer);
    function DaysThisMonth: Integer;
    procedure PrevMonthBtnClick(Sender: TObject);
    procedure NextMonthBtnClick(Sender: TObject);
    procedure PrevYearBtnClick(Sender: TObject);
    procedure NextYearBtnClick(Sender: TObject);
    procedure MenuClick(Sender: TObject);
    procedure TodayClick(Sender: TObject);
    procedure TitleClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MakePopup;
    procedure UpdateProps;
  public
    FGrid: TsCalendGrid;
    FDragBar : TsDragBar;
    FTodayBtn : TsSpeedButton;
    FBtns: array[0..3] of TsTimerSpeedButton;
    constructor Create(AOwner: TComponent); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure NextMonth;
    procedure NextYear;
    procedure PrevMonth;
    procedure PrevYear;
    procedure UpdateCalendar; virtual;
    property CellText[ACol, ARow: Integer]: string read GetCellText;
    procedure WndProc(var Message: TMessage); override;
    property OnDrawDay : TGetCellParams read FOnDrawDay write FOnDrawDay;
{$ENDIF} // NOTFORHELP
  published
{$IFNDEF NOTFORHELP}
    property Align;
    property BorderWidth default 0;
    property BevelWidth default 5;
    property Width default 178;
    property Height default 139;
{$ENDIF} // NOTFORHELP
    {:@event}
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    {:@event}
    property OnGetCellParams : TGetCellParams read FOnGetCellParams write FOnGetCellParams;
    property CalendarDate: TDateTime read FDate write SetCalendarDate stored StoreCalendarDate;
    property Day: Integer index 3  read GetDateElement write SetDateElement stored False;
    property Month: Integer index 2  read GetDateElement write SetDateElement stored False;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property StartOfWeek : TCalDayOfWeek read FStartOfWeek write SetStartOfWeek default dowLocaleDefault;
    property ShowCurrentDate : boolean read FShowCurrentDate write SetShowCurrentDate default True;
    property ShowTitle : boolean read FShowTitle write SetShowTitle default True;
    property ShowTodayBtn : boolean read FShowTodayBtn write SetShowTodayBtn default False;
    property ShowWeeks : boolean read FShowWeeks write SetShowWeeks default False;
    property UseCurrentDate: Boolean read FUseCurrentDate write SetUseCurrentDate default True;
    property WeekendColor: TColor read FWeekendColor write SetWeekendColor default clRed;
    property Weekends: TDaysOfWeek read FWeekends write SetWeekends default DefWeekends;
    property Year: Integer index 1  read GetDateElement write SetDateElement stored False;
  end;

function WeekNum(const TDT:TDateTime) : Word;

implementation

uses Consts, ExtCtrls, sStyleSimply, sSkinProps, sSkinManager, sPopupClndr, sVclUtils, math
  {$IFDEF LOGGED}, sDebugMsgs{$ENDIF};

{ TsMonthCalendar }

var
  acTo : acString;

function WeekNum(const TDT:TDateTime) : Word;
var
  Y, M, D : Word;
  dtTmp : TDateTime;
begin
  DecodeDate(TDT, Y, M, D) ;
  dtTmp := EnCodeDate(Y, 1, 1) ;
  Result := (Trunc(TDT - dtTmp) + (DayOfWeek(dtTmp) - 1)) div 7 + 1;
  if Result <> 0 then Result := Result - 1;
end;

constructor TsMonthCalendar.Create(AOwner: TComponent);
var
  i : integer;
begin
  inherited Create(AOwner);
  SkinData.COC := COC_TsMonthCalendar;
  SkinData.Updating := False;

  FShowTitle := True;
  FShowTodayBtn := False;
  PopMenu := nil;

  FDragBar := TsDragBar.Create(Self);
  FDragBar.SkinData.SkinSection := s_DragBar;
  FDragBar.Color := clActiveCaption;
  FDragBar.OnDblClick := TopPanelDblClick;
  FDragBar.SkinData.Updating := False;
  FDragBar.OnMouseDown := TitleClick;

  FDragBar.Align := alTop;
  FDragBar.Top := -1;

  with TsPanel.Create(Self) do begin
    Align := alTop;
    Height := 3;
    Skindata.SkinSection := s_CheckBox;
    Caption := '';
    BevelOuter := bvNone;
    Parent := Self;
  end;         

  FGrid := TsCalendGrid.Create(Self);
  FGrid.Parent := Self;
  FGrid.ParentColor := True;
  FGrid.OnDblClick := OnDblClick;
  FGrid.OnClick := OnClick;
  BorderWidth := 3;
  BevelWidth := 1;
  Caption := ' ';
  FShowCurrentDate := True;

  FUseCurrentDate := True;
  FStartOfWeek := dowLocaleDefault;
  FWeekends := DefWeekends;
  FWeekendColor := clRed;
  FDate := Date;
  Width  := 178;
  Height := 139;

  for i := 0 to 3 do begin
    FBtns[i] := TsTimerSpeedButton.Create(Self);
    FBtns[i].Parent := FDragBar;
    FBtns[i].Flat := True;
    FBtns[i].SkinData.SkinSection := s_WebBtn;
  end;
  FBtns[0].Align := alLeft;
  FBtns[0].Caption := '<<';
  FBtns[0].OnClick := PrevYearBtnClick;
  FBtns[1].Align := alLeft;
  FBtns[1].Caption := '<';
  FBtns[1].OnClick := PrevMonthBtnClick;
  FBtns[2].Align := alRight;
  FBtns[2].Caption := '>';
  FBtns[2].OnClick := NextMonthBtnClick;
  FBtns[3].Align := alRight;
  FBtns[3].Caption := '>>';
  FBtns[3].OnClick := NextYearBtnClick;

  FDragBar.Parent := Self;

  FTodayBtn := TsSpeedButton.Create(Self);
  FTodayBtn.Visible := False;
  FTodayBtn.Align := alBottom;
  FTodayBtn.Height := 18;
  FTodayBtn.Flat := True;
  FTodayBtn.AnimatEvents := [];
  FTodayBtn.SkinData.SkinSection := s_ToolButton;//s_WebBtn;
  if acTo = ''
    then FTodayBtn.Caption := FormatDateTime('"Today: " mmmm d, yyyy', Date)
    else FTodayBtn.Caption := acTo + FormatDateTime(' mmmm d, yyyy', Date);
  FTodayBtn.Font.Style := [fsBold];
  FTodayBtn.OnClick := TodayClick;
  FTodayBtn.Parent := Self;
end;

procedure TsMonthCalendar.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TsMonthCalendar.DaysThisMonth: Integer;
begin
  Result := DaysPerMonth(Year, Month);
end;

function TsMonthCalendar.GetCellText(ACol, ARow: Integer): string;
var
  DayNum: Integer;
  Y, M : Word;
  PrevYear : boolean;
begin
  if ARow = 0 then begin
    if ShowWeeks and (ACol = 0) then Result := '¹/w' else Result := ShortDayNames[(FirstDay + ACol - Integer(ShowWeeks) + 1) mod 7 + 1]
  end
  else if ShowWeeks and (ACol = 0) then begin
    if ARow > 6 then Exit;
    PrevYear := False;
    Y := Year;
    M := Month;
    if ARow = 1
      then DayNum := FMonthOffset + 7 - Integer(ShowWeeks)
      else DayNum := FMonthOffset + 1 - Integer(ShowWeeks) + (ARow - 1) * 7 + 1;

    if FirstDay = 0 then dec(DayNum); // If Monday - first
    if DayNum <= 0 then begin
      if M = 1 then begin
        PrevYear := True;
        M := 12;
        dec(Y);
      end
      else dec(M);
      DayNum := DaysPerMonth(Y, M);
    end
    else if DayNum > DaysThisMonth then begin
      DayNum := DayNum - DaysThisMonth;
      inc(M);
      if M > 12 then begin
        inc(Y);
        M := 1;
      end;
    end;
    if PrevYear
      then Result := IntToStr(WeekNum(EncodeDate(Y, M, 1)) + 1)
      else Result := IntToStr(WeekNum(EncodeDate(Y, M, DayNum)) + 1);
  end
  else begin
    DayNum := FMonthOffset + ACol - Integer(ShowWeeks) + (ARow - 1) * 7;
    if (DayNum < 1) or (DayNum > DaysThisMonth)
      then Result := ''
      else Result := IntToStr(DayNum);
  end;
end;

procedure TsMonthCalendar.SetCalendarDate(Value: TDateTime);
{$IFDEF DELPHI7UP}
var
  Y, M, D : Word;
{$ENDIF}
begin
  if FDate <> Value then begin
{$IFDEF DELPHI7UP}
    DecodeDate(Value, Y, M, D);
    if not TryEncodeDate(Y, M, D, FDate) then Exit;
{$ENDIF}
    FDate := Value;
    UpdateCalendar;
    Change;
  end;
end;

function TsMonthCalendar.StoreCalendarDate: Boolean;
begin
  Result := not FUseCurrentDate;
end;

function TsMonthCalendar.GetDateElement(Index: Integer): Integer;
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(FDate, AYear, AMonth, ADay);
  case Index of
    1: Result := AYear;
    2: Result := AMonth;
    3: Result := ADay;
    else Result := -1;
  end;
end;

procedure TsMonthCalendar.SetDateElement(Index: Integer; Value: Integer);
var
  AYear, AMonth, ADay: Word;
begin
  if Value > 0 then begin
    DecodeDate(FDate, AYear, AMonth, ADay);
    case Index of
      1: if AYear <> Value then AYear := Value else Exit;
      2: if (Value <= 12) and (Value <> AMonth) then begin
        AMonth := Value;
        if ADay > DaysPerMonth(Year, Value) then ADay := DaysPerMonth(Year, Value);
      end
      else Exit;
      3: if (Value <= DaysThisMonth) and (Value <> ADay) then ADay := Value else Exit;
      else Exit;
    end;
{$IFDEF DELPHI7UP}
    if not TryEncodeDate(AYear, AMonth, ADay, FDate) then Exit;
{$ELSE}
    FDate := EncodeDate(AYear, AMonth, ADay);
{$ENDIF}
    FUseCurrentDate := False;
    CalendarUpdate(Index = 3);
    Change;
  end;
end;

procedure TsMonthCalendar.SetWeekendColor(Value: TColor);
begin
  if Value <> FWeekendColor then begin
    FWeekendColor := Value;
    SkinData.Invalidate;
  end;
end;

procedure TsMonthCalendar.SetWeekends(Value: sConst.TDaysOfWeek);
begin
  if Value <> FWeekends then begin
    FWeekends := Value;
    UpdateCalendar;
  end;
end;

function TsMonthCalendar.IsWeekend(ACol, ARow: Integer): Boolean;
begin
  Result := TCalDayOfWeek((FirstDay + ACol - Integer(ShowWeeks)) mod 7) in FWeekends;
end;

procedure TsMonthCalendar.SetStartOfWeek(Value: TCalDayOfWeek);
begin
  if Value <> FStartOfWeek then begin
    FStartOfWeek := Value;
    UpdateCalendar;
  end;
end;

procedure TsMonthCalendar.SetUseCurrentDate(Value: Boolean);
begin
  if Value <> FUseCurrentDate then begin
    FUseCurrentDate := Value;
    if Value then begin
      FDate := Date;
      UpdateCalendar;
    end;
  end;
end;

procedure TsMonthCalendar.ChangeMonth(Delta: Integer);
var
  AYear, AMonth, ADay: Word;
  NewDate: TDateTime;
  CurDay: Integer;
begin
  DecodeDate(FDate, AYear, AMonth, ADay);
  CurDay := ADay;
  if Delta > 0 then ADay := DaysPerMonth(AYear, AMonth) else ADay := 1;
  NewDate := EncodeDate(AYear, AMonth, ADay);
  NewDate := NewDate + Delta;
  DecodeDate(NewDate, AYear, AMonth, ADay);
  if DaysPerMonth(AYear, AMonth) > CurDay then ADay := CurDay else ADay := DaysPerMonth(AYear, AMonth);
{$IFDEF DELPHI7UP}
  if TryEncodeDate(AYear, AMonth, ADay, NewDate) then
{$ENDIF}
    CalendarDate := EncodeDate(AYear, AMonth, ADay);
end;

procedure TsMonthCalendar.PrevMonth;
begin
  ChangeMonth(-1);
end;

procedure TsMonthCalendar.NextMonth;
begin
  ChangeMonth(1);
end;

procedure TsMonthCalendar.NextYear;
{$IFDEF DELPHI7UP}
var
  NewDate : TDateTime;
{$ENDIF}
begin
  if IsLeapYear(Year) and (Month = 2) and (Day = 29) then Day := 28;
{$IFDEF DELPHI7UP}
  if TryEncodeDate(Year + 1, Month, Day, NewDate) then
{$ENDIF}
    Year := Year + 1;
end;

procedure TsMonthCalendar.PrevYear;
begin
  if IsLeapYear(Year) and (Month = 2) and (Day = 29) then Day := 28;
  Year := Year - 1;
end;

procedure TsMonthCalendar.CalendarUpdate(DayOnly: Boolean);
var
  AYear, AMonth, ADay: Word;
  FirstDate: TDateTime;
  d1, d2 : integer;
begin
  FUpdating := True;
  try
    DecodeDate(FDate, AYear, AMonth, ADay);
{$IFDEF DELPHI7UP}
    if not TryEncodeDate(AYear, AMonth, 1, FirstDate) then Exit;
{$ELSE}
    FirstDate := EncodeDate(AYear, AMonth, 1);
{$ENDIF}
    FMonthOffset := 2 - ((DayOfWeek(FirstDate) - (FirstDay + 1) + 7) mod 7);
    if FMonthOffset = 2 then FMonthOffset := -5;
    d1 := (ADay - FMonthOffset) mod 7 + Integer(ShowWeeks);
    d2 := (ADay - FMonthOffset) div 7 + 1;
    FGrid.MoveColRow(d1, d2, False, False);
    if not DayOnly then begin
      FDragBar.Caption := LongMonthNames[AMonth] + ' ' + IntToStr(AYear);
      FGrid.Invalidate;
    end;
  finally
    FUpdating := False;
  end;
end;

procedure TsMonthCalendar.UpdateCalendar;
begin
  CalendarUpdate(False);
end;

function TsMonthCalendar.FirstDay: integer;
var
  A: array[0..1] of char;
begin
  if FStartOfWeek = dowLocaleDefault then begin
    GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IFIRSTDAYOFWEEK, A, SizeOf(A));
    Result := Ord(A[0]) - Ord('0');
  end
  else Result := Ord(FStartOfWeek);
end;

procedure TsMonthCalendar.PrevYearBtnClick(Sender: TObject);
begin
  PrevYear;
end;

procedure TsMonthCalendar.NextYearBtnClick(Sender: TObject);
begin
  NextYear;
end;

procedure TsMonthCalendar.PrevMonthBtnClick(Sender: TObject);
begin
  PrevMonth;
end;

procedure TsMonthCalendar.NextMonthBtnClick(Sender: TObject);
begin
  NextMonth;
end;

procedure TsMonthCalendar.TopPanelDblClick(Sender: TObject);
begin
  CalendarDate := Date;
end;

procedure TsMonthCalendar.WndProc(var Message: TMessage);
begin
  inherited;
  if Message.MSG = SM_ALPHACMD then case Message.WParamHi of
    AC_REMOVESKIN, AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      if (PopMenu <> nil) and SkinData.SkinManager.SkinnedPopups then Skindata.SkinManager.SkinableMenus.HookPopupMenu(PopMenu, Message.WParamHi = AC_REFRESH);
      UpdateCalendar;
      FGrid.Repaint;
      if ShowTitle then SendMessage(FDragBar.Handle, Message.Msg, Message.WParam, Message.LParam);
    end;
  end;
end;

procedure TsMonthCalendar.SetShowTitle(const Value: boolean);
begin
  if FShowTitle <> Value then begin
    FShowTitle := Value;
    FDragBar.Visible := Value;
    if Value then FDragBar.Parent := Self else FDragBar.Parent := nil;
  end;
end;

procedure TsMonthCalendar.Loaded;
var
  i : integer;
begin
  inherited;
  if Assigned(FDragBar) and FDragBar.Visible then begin
    FDragBar.SkinData.FSkinManager := SkinData.FSkinManager;
    for i := 0 to 3 do FBtns[i].SkinData.FSkinManager := SkinData.FSkinManager;
  end;
end;

procedure TsMonthCalendar.SetShowCurrentDate(const Value: boolean);
begin
  if FShowCurrentDate <> Value then begin
    FShowCurrentDate := Value;
    Invalidate;
  end;
end;

procedure TsMonthCalendar.SetShowWeeks(const Value: boolean);
begin
  if FShowWeeks <> Value then begin
    FShowWeeks := Value;
    FGrid.DefaultColWidth := FGrid.Width div (7 + integer(ShowWeeks));
    UpdateProps;
    Invalidate;
  end;
end;

procedure TsMonthCalendar.CreateWnd;
begin
  inherited;
  if FShowtodayBtn then FTodayBtn.Visible := True;
  UpdateProps;
end;

procedure TsMonthCalendar.MakePopup;
var
  i, j : integer;
  mi, si : TMenuItem;
begin
  if PopMenu = nil then begin
    if PopMenu <> nil then PopMenu.Free;
    PopMenu := TPopupMenu.Create(Self);
    for i := Year - 5 to Year + 5 do begin
      mi := TMenuItem.Create(Self);
      mi.Tag := i;
      mi.Caption := IntToStr(i);
      if i = Year then begin
        mi.Default := True;
        mi.Checked := True;
      end;
      for j := 1 to 12 do begin
        si := TMenuItem.Create(Self);
        si.Caption := LongMonthNames[j];
        if mi.Default and (j = Month) then begin
          si.Default := True;
          si.Checked := True;
        end;
        si.OnClick := MenuClick;
        mi.Add(si);
      end;
      PopMenu.Items.Add(mi);
    end;
  end;
  if SkinData.Skinned and SkinData.SkinManager.SkinnedPopups then Skindata.SkinManager.SkinableMenus.HookPopupMenu(PopMenu, True);
end;

procedure TsMonthCalendar.MenuClick(Sender: TObject);
var
  mi : TMenuItem;
begin
  mi := TMenuItem(Sender);
  Year := mi.Parent.Tag;
  Month := mi.MenuIndex + 1;
end;

procedure TsMonthCalendar.TitleClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P : TPoint;
begin
  MakePopup;
  P := acMousePos;
  P.Y := max(0, P.Y - 120);
  PopMenu.Popup(P.X, P.Y);
end;

procedure TsMonthCalendar.TodayClick(Sender: TObject);
begin
  CalendarDate := Date;
  if Parent is TsPopupCalendar then begin
    TsPopupCalendar(Parent).CalendarClick;
  end;
end;

procedure TsMonthCalendar.SetShowTodayBtn(const Value: boolean);
begin
  if FShowTodayBtn <> Value then begin
    FShowTodayBtn := Value;
    if Value then FTodayBtn.Parent := Self else FTodayBtn.Parent := nil;
    CalendarUpdate(False);
  end;
end;

procedure TsMonthCalendar.UpdateProps;
begin
  if FShowTodayBtn then FTodayBtn.Parent := Self else FTodayBtn.Parent := nil;
  if FGrid <> nil then begin
    FGrid.ColCount := 7 + integer(ShowWeeks);
    FGrid.FixedCols := integer(ShowWeeks);
    UpdateCalendar;
  end;
end;

{ TsCalendGrid }

procedure TsCalendGrid.AfterConstruction;
begin
  inherited;
end;

procedure TsCalendGrid.Click;
var
  TheCellText: string;
begin
  inherited Click;
  TheCellText := FOwner.CellText[Col, Row];
  if TheCellText <> '' then FOwner.Day := StrToInt(TheCellText);
  if Assigned(FOwner.OnClick) then FOwner.OnClick(FOwner);
end;

constructor TsCalendGrid.Create(AOwner: TComponent);
begin
  inherited;
  FOwner := TsMonthCalendar(AOwner);
  Ctl3D := False;
  BorderStyle := bsNone;
  FixedCols := 0;
  FixedRows := 1;
  ColCount := 7;
  RowCount := 8;
  ScrollBars := ssNone;
  Options := [];
  Align := alClient;
  Tag := ExceptTag;
end;

procedure TsCalendGrid.CreateWnd;
begin
  inherited;
  DefaultDrawing := False;
  Clicked := False;
end;

procedure TsCalendGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
var
  TheText: string;
  R : TRect;
  d : TDateTime;
  c : TColor;
  BGInfo : TacBGInfo;
  function IsToday: boolean; begin
    Result := (ARow <> 0) and (TheText <> '') and (EncodeDate(FOwner.Year, FOwner.Month, StrToInt(TheText)) = Date)
  end;
begin
  TheText := FOwner.CellText[ACol, ARow];
  R := ARect;
  if ACol = 6 + Integer(FOwner.ShowWeeks)
    then R.Right := Width;
  if ARow = 6 then R.Bottom := Height;

  BGInfo.PleaseDraw := False;
  BGInfo.Offset := Point(0, 0);
  GetBGInfo(@BGInfo, FOwner);
  if BGInfo.BgType = btCache
    then BitBlt(Canvas.Handle, R.Left, R.Top, WidthOf(R), HeightOf(R), BGInfo.Bmp.Canvas.Handle, Left + R.Left + BGInfo.Offset.X, Top + R.Top + BGInfo.Offset.Y, SRCCOPY)
    else FillDC(Canvas.Handle, R, BGInfo.Color);
  R := ARect;

  Canvas.Font.Assign(Font);
  Canvas.Brush.Color := Color;
  if FOwner.ShowCurrentDate and (gdSelected in AState) then begin
    Canvas.Font.Style := [fsBold];
    Canvas.Font.Color := clBlack;
  end
  else begin
    Canvas.Font.Style := [];
  end;
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Style := [];
  if not (FOwner.ShowWeeks and (ACol = 0)) and FOwner.ShowCurrentDate and IsToday and (ACol <> 7 + Integer(FOwner.ShowWeeks)) then begin
    inc(R.Left);
    inc(R.Bottom);
    Canvas.Brush.Style := bsSolid;
    Canvas.Pen.Style := psClear;
    if FOwner.SkinData.Skinned
      then Canvas.Brush.Color := MixColors(FOwner.WeekendColor, FOwner.SkinData.SkinManager.GetGlobalColor, 0.3)
      else Canvas.Brush.Color := MixColors(FOwner.WeekendColor, ColortoRGB(clBtnFace), 0.3);
    Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, 8, 8);
    Canvas.Font.Color := clWhite;
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Style := [fsBold];
  end
  else if FOwner.IsWeekend(ACol, ARow) then begin
    Canvas.Font.Color := FOwner.WeekendColor;
  end
  else if FOwner.ShowWeeks and (ACol = 0) then begin
    if ARow = 0 then begin
      Canvas.Font.Name := 'Small Fonts';
      Canvas.Font.Size := 6;
    end
    else Canvas.Font.Style := [fsBold];
    if FOwner.SkinData.Skinned and Assigned(FOwner.SkinData.SkinManager)
      then Canvas.Font.Color := MixColors(FOwner.SkinData.SkinManager.GetGlobalFontColor, FOwner.SkinData.SkinManager.GetGlobalColor, 0.3)
      else Canvas.Font.Color := clGrayText;

    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := Canvas.Font.Color;
    Canvas.MoveTo(ARect.Right - 1, ARect.Top);
    Canvas.LineTo(ARect.Right - 1, ARect.Bottom);
    dec(ARect.Right, 3)
  end
  else begin
    if FOwner.SkinData.Skinned and Assigned(FOwner.SkinData.SkinManager) and (FOwner.SkinData.SkinManager.GetGlobalFontColor <> clFuchsia)
      then Canvas.Font.Color := FOwner.SkinData.SkinManager.GetGlobalFontColor
      else Canvas.Font.Color := clWindowText;
  end;
  if (gdSelected in AState) then begin
    if TheText = '' then Exit;
    Canvas.Font.Style := [fsBold];
    InflateRect(R, -2, -2);
    dec(R.Top, 1);
    inc(R.Left);
    Canvas.Brush.Style := bsSolid;
    if not IsToday then begin
      if FOwner.SkinData.Skinned then begin
        Canvas.Brush.Color := FOwner.SkinData.SkinManager.GetHighLightColor;
        Canvas.Font.Color := FOwner.SkinData.SkinManager.GetHighLightFontColor;
//        if Focused then InflateRect(ARect, 1, 1);
      end
      else begin
        Canvas.Brush.Color := clHighlight;
        Canvas.Font.Color := clHighlightText;
      end;
      Canvas.FillRect(R);
    end;
    Canvas.Brush.Color := Color;
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Style := psClear;
  end;
  if Assigned(FOwner.FOnDrawDay) or Assigned(FOwner.FOnGetCellParams) then begin
    if (ARow > 0) and (TheText <> '') then begin
      d := EncodeDate(FOwner.Year, FOwner.Month, StrToInt(Thetext));
      c := clFuchsia;
      if Assigned(FOwner.FOnDrawDay)
        then FOwner.FOnDrawDay(TsPopupCalendar(FOwner.Parent).FEditor, d, Canvas.Font, c)
        else FOwner.FOnGetCellParams(Self, d, Canvas.Font, c);
      if c <> clFuchsia then begin
        Canvas.Brush.Color := c;
        Canvas.Brush.Style := bsSolid
      end;
    end;
  end;
  Canvas.TextRect(ARect, ARect.Left + (ARect.Right - ARect.Left - Canvas.TextWidth(TheText)) div 2,
                  ARect.Top + (ARect.Bottom - ARect.Top - Canvas.TextHeight(TheText)) div 2, TheText);

  if (gdSelected in AState) and (Canvas.Brush.Style <> bsSolid) and Focused then begin
{    if FOwner.SkinData.Skinned then begin
      FocusRect(Canvas, ARect);
    end
    else begin}
      InflateRect(R, 1, 1);
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Style := psClear;
      DrawFocusRect(Canvas.Handle, R); // standard
//    end;
  end;
end;

procedure TsCalendGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP, VK_DOWN : Shift := Shift - [ssCtrl];
    VK_LEFT, VK_SUBTRACT: if Shift = [] then begin
      if (FOwner.Day > 1)
        then FOwner.Day := FOwner.Day - 1
        else FOwner.CalendarDate := FOwner.CalendarDate - 1;
      Exit;
    end;
    VK_RIGHT, VK_ADD: if Shift = [] then begin
      if (FOwner.Day < FOwner.DaysThisMonth)
        then FOwner.Day := FOwner.Day + 1
        else FOwner.CalendarDate := FOwner.CalendarDate + 1;
      Exit;
    end;
    VK_NEXT: begin
      if ssCtrl in Shift
        then FOwner.NextYear
        else FOwner.NextMonth;
      Exit;
    end;
    VK_PRIOR: begin
      if ssCtrl in Shift
        then FOwner.PrevYear
        else FOwner.PrevMonth;
      Exit;
    end;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TsCalendGrid.KeyPress(var Key: Char);
begin
  if Key in ['T', 't'] then begin
    FOwner.CalendarDate := Trunc(Now);
    Key := #0;
  end;
  inherited KeyPress(Key);
end;

procedure TsCalendGrid.Loaded;
begin
  inherited;
end;

procedure TsCalendGrid.MouseToCell(X, Y: Integer; var ACol, ARow: Integer);
var
  Coord: TGridCoord;
begin
  Coord := MouseCoord(X, Y);
  ACol := Coord.X;
  ARow := Coord.Y;
end;

procedure TsCalendGrid.Paint;
begin
  if (csDestroying in ComponentState) or (csLoading in ComponentState) then Exit;
  inherited;
end;

function TsCalendGrid.SelectCell(ACol, ARow: Integer): Boolean;
begin
  if ((not FOwner.FUpdating) and FOwner.FReadOnly) or (FOwner.CellText[ACol, ARow] = '')
    then Result := False
    else Result := inherited SelectCell(ACol, ARow);
end;

procedure TsCalendGrid.WMSize(var Message: TWMSize);
begin
  DefaultColWidth  := Message.Width  div (7 + integer(FOwner.ShowWeeks));
  DefaultRowHeight := Message.Height div 7;
end;

procedure TsCalendGrid.WndProc(var Message: TMessage);
var
  SaveIndex: Integer;
  DC: HDC;
  BGInfo : TacBGInfo;
  R : TRect;
begin
  case Message.Msg of
    WM_ERASEBKGND, CM_MOUSEENTER, CM_MOUSELEAVE, WM_MOUSEMOVE :
    else inherited;
  end;
  case Message.Msg of
    WM_LBUTTONDBLCLK : if Assigned(FOwner.OnDblClick) and (csDoubleClicks in FOwner.ControlStyle) and (TsPopupCalendar(FOwner.Parent).FEditor = nil)
      then FOwner.OnDblClick(FOwner);
    WM_PAINT : begin
      inherited;
      // Filling the right line
      if TWMPAINT(Message).DC = 0 then DC := GetDC(Handle) else DC := TWMPAINT(Message).DC;
      SaveIndex := SaveDC(DC);
      try
        BGInfo.PleaseDraw := False;
        GetBGInfo(@BGInfo, FOwner);
        R := Rect((ColCount) * DefaultColWidth, 0, Width, Height);

        if BGInfo.BgType = btCache
          then BitBlt(DC, R.Left, R.Top, WidthOf(R), HeightOf(R), BGInfo.Bmp.Canvas.Handle, Left + R.Left + BGInfo.Offset.X, Top + R.Top + BGInfo.Offset.Y, SRCCOPY)
          else FillDC(DC, R, BGInfo.Color);
      finally
        RestoreDC(DC, SaveIndex);
        if TWMPAINT(Message).DC = 0 then ReleaseDC(Handle, DC);
      end;
    end;
    WM_LBUTTONUP : if (FOwner.Parent is TsPopupCalendar) and FOwner.Parent.Visible then begin
      if Clicked then Exit;
      Clicked := True;
      TsPopupCalendar(FOwner.Parent).CalendarClick;
    end
    else inherited;
  end;
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
end;

var
  Lib : Cardinal;
  ResStringRec : TResStringRec;

initialization

  Lib := LoadLibrary('Comctl32');
  if Lib <> 0 then begin
    ResStringRec.Module := @Lib;
    ResStringRec.Identifier := 4163;
    acTo := LoadResString(@ResStringRec);
  end;

finalization

end.
