unit sTooledit;
{$I sDefs.inc}

interface

uses Windows, Classes, sConst, StdCtrls, Controls, Messages, SysUtils, Forms, Graphics, Menus, Buttons, sDialogs,
  Mask, sDateUtils, sCustomComboEdit, FileCtrl, comctrls, sPopupClndr, sStrings, Dialogs, sMonthCalendar,
  acntUtils, sDefaults {$IFDEF DELPHI6}, Variants{$ENDIF};

type
  TExecOpenDialogEvent = procedure(Sender: TObject; var Name: string; var Action: Boolean) of object;
  
  TsFileDirEdit = class(TsCustomComboEdit)
{$IFNDEF NOTFORHELP}
  private
    FAcceptFiles: Boolean;
    FOnDropFiles: TNotifyEvent;
    FOnBeforeDialog: TExecOpenDialogEvent;  
    FOnAfterDialog: TExecOpenDialogEvent;   
    procedure SetDragAccept(Value: Boolean);
    procedure SetAcceptFiles(Value: Boolean);
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
  protected
    FMultipleDirs: Boolean;
    procedure CreateHandle; override;
    procedure DestroyWindowHandle; override;
    function GetLongName: string; virtual; abstract;
    function GetShortName: string; virtual; abstract;
    procedure DoAfterDialog(var FileName: string; var Action: Boolean);dynamic;
    procedure DoBeforeDialog(var FileName: string; var Action: Boolean);dynamic;
    procedure ReceptFileDir(const AFileName: acString); virtual; abstract;
    procedure ClearFileList; virtual;
    property MaxLength default 255;
  public
    constructor Create(AOwner: TComponent); override;
{$ENDIF} // NOTFORHELP
    property LongName: string read GetLongName;
    property ShortName: string read GetShortName;
    property AcceptFiles: Boolean read FAcceptFiles write SetAcceptFiles default False;
  published
{$IFNDEF NOTFORHELP}
    property OnBeforeDialog: TExecOpenDialogEvent read FOnBeforeDialog write FOnBeforeDialog;
    property OnAfterDialog: TExecOpenDialogEvent read FOnAfterDialog   write FOnAfterDialog;
    property OnDropFiles: TNotifyEvent read FOnDropFiles write FOnDropFiles;
{$ENDIF} // NOTFORHELP
  end;

{ TsFilenameEdit }

  TFileDialogKind = (dkOpen, dkSave, dkOpenPicture, dkSavePicture);

  TsFilenameEdit = class(TsFileDirEdit)
{$IFNDEF NOTFORHELP}
  private
    FDialog: TOpenDialog;
    FDialogKind: TFileDialogKind;
    procedure CreateEditDialog;
    function GetFileName: string;
    function GetDefaultExt: string;
    function GetFileEditStyle: TFileEditStyle;
    function GetFilter: string;
    function GetFilterIndex: Integer;
    function GetInitialDir: string;
    function GetHistoryList: TStrings;
    function GetOptions: TOpenOptions;
    function GetDialogTitle: string;
    function GetDialogFiles: TStrings;
    procedure SetDialogKind(Value: TFileDialogKind);
    procedure SetFileName(const Value: string);
    procedure SetDefaultExt(Value: string);
    procedure SetFileEditStyle(Value: TFileEditStyle);
    procedure SetFilter(const Value: string);
    procedure SetFilterIndex(Value: Integer);
    procedure SetInitialDir(const Value: string);
    procedure SetHistoryList(Value: TStrings);
    procedure SetOptions(Value: TOpenOptions);
    procedure SetDialogTitle(const Value: string);
    function IsCustomTitle: Boolean;
    function IsCustomFilter: Boolean;
  protected
    procedure ButtonClick; override;
    procedure ReceptFileDir(const AFileName: acString); override;
    procedure ClearFileList; override;
    function GetLongName: string; override;
    function GetShortName: string; override;
    property FileEditStyle: TFileEditStyle read GetFileEditStyle write SetFileEditStyle default fsEdit;
  public
    constructor Create(AOwner: TComponent); override;
    property Dialog: TOpenDialog read FDialog;
{$ENDIF} // NOTFORHELP
    property DialogFiles: TStrings read GetDialogFiles;
    property DialogTitle: string read GetDialogTitle write SetDialogTitle stored IsCustomTitle;
  published
    property AcceptFiles;
{$IFNDEF NOTFORHELP}
    property DefaultExt: string read GetDefaultExt write SetDefaultExt;
    property FilterIndex: Integer read GetFilterIndex write SetFilterIndex default 1;
    property InitialDir: string read GetInitialDir write SetInitialDir;
{$ENDIF} // NOTFORHELP

    property DialogKind: TFileDialogKind read FDialogKind write SetDialogKind default dkOpen;
    property FileName: string read GetFileName write SetFileName stored False;
    property Filter: string read GetFilter write SetFilter stored IsCustomFilter;
    property HistoryList: TStrings read GetHistoryList write SetHistoryList;
    property DialogOptions: TOpenOptions read GetOptions write SetOptions default [ofHideReadOnly, ofEnableSizing];
  end;

{ TsDirectoryEdit }

  TsDirectoryEdit = class(TsFileDirEdit)
{$IFNDEF NOTFORHELP}
  private
    FOptions: TSelectDirOpts;
    FInitialDir: string;
    FDialogText: string;
    FRoot: TacRoot;
    FNoChangeDir: boolean;
  protected
    procedure ButtonClick; override;
    procedure ReceptFileDir(const AFileName: acString); override;
    function GetLongName: string; override;
    function GetShortName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    property DialogText: string read FDialogText write FDialogText;
  published
    property AcceptFiles;
    property InitialDir: string read FInitialDir write FInitialDir;
{$ENDIF} // NOTFORHELP
    property DialogOptions: TSelectDirOpts read FOptions write FOptions default [sdAllowCreate, sdPerformCreate, sdPrompt];
    property MultipleDirs: Boolean read FMultipleDirs write FMultipleDirs default False;
    property NoChangeDir : boolean read FNoChangeDir write FNoChangeDir default False;
    property Root: TacRoot read FRoot write FRoot;
  end;

{ TsCustomDateEdit }

  TYearDigits = (dyDefault, dyFour, dyTwo);
  TOnAcceptDate = procedure(Sender: TObject; var aDate: TDateTime; var CanAccept: Boolean) of object;

  TsCustomDateEdit = class(TsCustomComboEdit)
{$IFNDEF NOTFORHELP}
  private
    FTitle: POldString;
    FOnAcceptDate: TOnAcceptDate;
    FDefaultToday: Boolean;
    FHooked: Boolean;
    FCheckOnExit: Boolean;
    FBlanksChar: Char;
    FCalendarHints: TStrings;
    FStartOfWeek: TCalDayOfWeek;
    FWeekends: sConst.TDaysOfWeek;
    FWeekendColor: TColor;
    FYearDigits: TYearDigits;
    FDateFormat: string[10];
    FFormatting: Boolean;
    FMinDate: TDateTime;
    FMaxDate: TDateTime;
    FOnGetCellParams: TGetCellParams;
    FShowCurrentDate: boolean;
    FOnDrawDay: TGetCellParams;
    FShowWeeks: boolean;
    FShowTodayBtn: boolean;
    function GetDate: TDateTime;
    procedure SetDate(Value: TDateTime);
    procedure SetYearDigits(Value: TYearDigits);
    function GetDialogTitle: string;
    procedure SetDialogTitle(const Value: string);
    function IsCustomTitle: Boolean;
    procedure SetCalendarHints(Value: TStrings);
    procedure CalendarHintsChanged(Sender: TObject);
    procedure SetWeekendColor(Value: TColor);
    procedure SetWeekends(Value: sConst.TDaysOfWeek);
    procedure SetStartOfWeek(Value: TCalDayOfWeek);
    procedure SetBlanksChar(Value: Char);
    function TextStored: Boolean;
    function FourDigitYear: Boolean;
    function FormatSettingsChange(var Message: TMessage): Boolean;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure SetMinDate(const Value: TDateTime);
    procedure SetMaxDate(const Value: TDateTime);
    procedure SetShowCurrentDate(const Value: boolean);
  protected
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DestroyWindowHandle; override;
    function GetDateFormat: string;
    function DateIsStored : boolean;
    procedure ApplyDate(Value: TDateTime); virtual;
    procedure UpdateFormat;
    procedure UpdatePopup;
    procedure PopupWindowShow; override;
    property Formatting: Boolean read FFormatting;
    property EditMask stored False;
    property DialogTitle: string read GetDialogTitle write SetDialogTitle stored IsCustomTitle;
{$ENDIF} // NOTFORHELP
    property BlanksChar: Char read FBlanksChar write SetBlanksChar default ' ';
    property CalendarHints: TStrings read FCalendarHints write SetCalendarHints;
    property CheckOnExit: Boolean read FCheckOnExit write FCheckOnExit default False;
    property DefaultToday: Boolean read FDefaultToday write FDefaultToday default False;
    property MaxLength stored False;
    property StartOfWeek: TCalDayOfWeek read FStartOfWeek write SetStartOfWeek default dowLocaleDefault;
    property Weekends: sConst.TDaysOfWeek read FWeekends write SetWeekends default DefWeekends;
    property WeekendColor: TColor read FWeekendColor write SetWeekendColor default clRed;
    property YearDigits: TYearDigits read FYearDigits write SetYearDigits default dyFour;
    {:@event}
    property OnAcceptDate: TOnAcceptDate read FOnAcceptDate write FOnAcceptDate;
{$IFNDEF NOTFORHELP}
  public
    procedure Loaded; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CheckValidDate : boolean;
    function GetDateMask: string;
    procedure WndProc (var Message: TMessage); override;
    procedure UpdateMask; virtual;
{$ENDIF} // NOTFORHELP
    property Date: TDateTime read GetDate write SetDate;
    property Text stored DateIsStored;
  published
    property MinDate : TDateTime read FMinDate write SetMinDate;
    property MaxDate : TDateTime read FMaxDate write SetMaxDate;
    property ShowCurrentDate : boolean read FShowCurrentDate write SetShowCurrentDate default True;
    property ShowWeeks : boolean read FShowWeeks write FShowWeeks default False;
    property ShowTodayBtn : boolean read FShowTodayBtn write FShowTodayBtn default True;
    property OnDrawDay : TGetCellParams read FOnDrawDay write FOnDrawDay;
    property OnGetCellParams : TGetCellParams read FOnGetCellParams write FOnGetCellParams;
  end;

{ TsDateEdit }

  TsDateEdit = class(TsCustomDateEdit)
{$IFNDEF NOTFORHELP}
  public
    constructor Create(AOwner: TComponent); override;
    property EditMask;
  published
    property BlanksChar;
    property CalendarHints;
    property CheckOnExit;
    property ClickKey;
    property Date;
    property DefaultToday;
    property DialogTitle;
    property MaxDate;
    property MinDate;
    property PopupAlign;
    property PopupWidth;
    property StartOfWeek;
    property Text;
    property Weekends;
    property WeekendColor;
    property YearDigits;
    {:@event}
    property OnAcceptDate;
    property OnButtonClick;
    property OnChange;
    property OnContextPopup;
{$ENDIF} // NOTFORHELP
  end;

{$IFNDEF NOTFORHELP}
procedure DateFormatChanged;
function StrToDateFmt(const DateFormat, S: string): TDateTime;
function StrToDateFmtDef(const DateFormat, S: string; Default: TDateTime): TDateTime;
{$ENDIF} // NOTFORHELP

implementation

uses ShellAPI, Consts, sMessages, sStyleSimply, acPathDialog, acShellCtrls, sSkinManager, sGlyphUtils, ExtDlgs;

{ TsFileDirEdit }

procedure TsFileDirEdit.DoBeforeDialog(var FileName: string; var Action: Boolean);
begin
  if Assigned(FOnBeforeDialog) then FOnBeforeDialog(Self, FileName, Action);
end;

procedure TsFileDirEdit.DoAfterDialog(var FileName: string; var Action: Boolean);
begin
  if Assigned(FOnAfterDialog) then FOnAfterDialog(Self, FileName, Action);
end;

constructor TsFileDirEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SkinData.COC := COC_TsFileDirEdit;
  MaxLength := MaxByte;
end;

procedure TsFileDirEdit.CreateHandle;
begin
  inherited CreateHandle;
  if FAcceptFiles then SetDragAccept(True);
end;

procedure TsFileDirEdit.DestroyWindowHandle;
begin
  SetDragAccept(False);
  inherited DestroyWindowHandle;
end;

procedure TsFileDirEdit.SetDragAccept(Value: Boolean);
begin
  if not (csDesigning in ComponentState) and (Handle <> 0) then DragAcceptFiles(Handle, Value);
end;

procedure TsFileDirEdit.SetAcceptFiles(Value: Boolean);
begin
  if FAcceptFiles <> Value then begin
    SetDragAccept(Value);
    FAcceptFiles := Value;
  end;
end;

procedure TsFileDirEdit.WMDropFiles(var Msg: TWMDropFiles);
const
  maxlen = 254;
var
  FileName: acString;
  i, Num: Cardinal;
  pchr : array [0..maxlen] of acChar;
begin
  Msg.Result := 0;
  Num := DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0);
  if Num > 0 then begin
    ClearFileList;
    for i := 0 to Num - 1 do begin
{$IFDEF TNTUNICODE}
      DragQueryFileW(Msg.Drop, i, pchr, maxlen);
{$ELSE}
      DragQueryFile(Msg.Drop, i, pchr, maxlen);
{$ENDIF}
      FileName := acString(pchr);
      ReceptFileDir(FileName);
      if not FMultipleDirs then Break;
    end;
    if Assigned(FOnDropFiles) then FOnDropFiles(Self);
  end;
  DragFinish(Msg.Drop);
end;

procedure TsFileDirEdit.ClearFileList;
begin
end;

{ TsFilenameEdit }

constructor TsFilenameEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SkinData.COC := COC_TsFilenameEdit;
  FDefBmpID := iBTN_OPENFILE;
  CreateEditDialog;
end;

procedure TsFilenameEdit.CreateEditDialog;
var
  NewDialog: TOpenDialog;
begin
  case FDialogKind of
    dkOpen: begin
      NewDialog := TsOpenDialog.Create(Self);
      TsOpenDialog(NewDialog).ZipShowing := zsAsFile;
    end;
    dkOpenPicture: begin
      NewDialog := TOpenPictureDialog.Create(Self);
    end;
    dkSave: begin
      NewDialog := TsSaveDialog.Create(Self);
    end;
    dkSavePicture: begin
      NewDialog := TSavePictureDialog.Create(Self);
    end
    else NewDialog := nil;
  end;
  if FDialog <> nil then begin
    NewDialog.DefaultExt := FDialog.DefaultExt;
    NewDialog.FileEditStyle := FDialog.FileEditStyle;
    NewDialog.FileName := FDialog.FileName;
    NewDialog.Filter := FDialog.Filter;
    NewDialog.FilterIndex := FDialog.FilterIndex;
    NewDialog.InitialDir := FDialog.InitialDir;
    NewDialog.HistoryList := FDialog.HistoryList;
    NewDialog.Files.Assign(FDialog.Files);
    NewDialog.Options := FDialog.Options;
    FDialog.Free;
  end
  else begin
    NewDialog.Filter := SDefaultFilter;
    NewDialog.Options := [ofHideReadOnly, ofEnableSizing];
  end;
  FDialog := NewDialog;
end;

function TsFilenameEdit.IsCustomTitle: Boolean;
begin
  Result := CompareStr(LoadStr(s_FileOpen), FDialog.Title) <> 0;
end;

function TsFilenameEdit.IsCustomFilter: Boolean;
begin
  Result := CompareStr(sDefaultFilter, FDialog.Filter) <> 0;
end;

procedure TsFilenameEdit.ButtonClick;
var
  Temp: string;
  Flag: Boolean;
begin
  inherited;
  Temp := inherited Text;
  Flag := True;
  DoBeforeDialog(Temp, Flag);  
  if not Flag then Exit;
  if ValidFileName(Temp) then begin
    if DirectoryExists(ExtractFilePath(Temp)) then SetInitialDir(ExtractFilePath(Temp));
    if (ExtractFileName(Temp) = '') or not ValidFileName(ExtractFileName(Temp)) then Temp := '';
    FDialog.FileName := Temp;
  end;
  FDialog.HelpContext := Self.HelpContext;
  Flag := FDialog.Execute;
  if Flag then Temp := FDialog.FileName;
  if CanFocus then SetFocus;
  DoAfterDialog(Temp, Flag);  
  if Flag then begin
    inherited Text := Temp;
    SetInitialDir(ExtractFilePath(FDialog.FileName));
  end;
end;

function TsFilenameEdit.GetFileName: string;
begin
  Result := inherited Text;
end;

procedure TsFilenameEdit.SetFileName(const Value: string);
begin
  if (Value = '') or ValidFileName(Value) then begin
    inherited Text := Value;
    ClearFileList;
  end
  else raise Exception.CreateFmt('Invalid file name', [Value]);
end;

function TsFilenameEdit.GetLongName: string;
begin
  Result := FileName;
end;

function TsFilenameEdit.GetShortName: string;
begin
  Result := FileName;
end;

procedure TsFilenameEdit.ClearFileList;
begin
  FDialog.Files.Clear;
end;

procedure TsFilenameEdit.ReceptFileDir(const AFileName: acString);
begin
  if FMultipleDirs then begin
    if FDialog.Files.Count = 0 then SetFileName(AFileName);
    FDialog.Files.Add(AFileName);
  end
  else SetFileName(AFileName);
end;

function TsFilenameEdit.GetDialogFiles: TStrings;
begin
  Result := FDialog.Files;
end;

function TsFilenameEdit.GetDefaultExt: string;
begin
  Result := FDialog.DefaultExt;
end;

function TsFilenameEdit.GetFileEditStyle: TFileEditStyle;
begin
  Result := FDialog.FileEditStyle;
end;

function TsFilenameEdit.GetFilter: string;
begin
  Result := FDialog.Filter;
end;

function TsFilenameEdit.GetFilterIndex: Integer;
begin
  Result := FDialog.FilterIndex;
end;

function TsFilenameEdit.GetInitialDir: string;
begin
  Result := FDialog.InitialDir;
end;

function TsFilenameEdit.GetHistoryList: TStrings;
begin
  Result := FDialog.HistoryList;
end;

function TsFilenameEdit.GetOptions: TOpenOptions;
begin
  Result := FDialog.Options;
end;

function TsFilenameEdit.GetDialogTitle: string;
begin
  Result := FDialog.Title;
end;

procedure TsFilenameEdit.SetDialogKind(Value: TFileDialogKind);
begin
  if FDialogKind <> Value then begin
    FDialogKind := Value;
    CreateEditDialog;
  end;
end;

procedure TsFilenameEdit.SetDefaultExt(Value: string);
begin
  FDialog.DefaultExt := Value;
end;

procedure TsFilenameEdit.SetFileEditStyle(Value: TFileEditStyle);
begin
  FDialog.FileEditStyle := Value;
end;

procedure TsFilenameEdit.SetFilter(const Value: string);
begin
  FDialog.Filter := Value;
end;

procedure TsFilenameEdit.SetFilterIndex(Value: Integer);
begin
  FDialog.FilterIndex := Value;
end;

procedure TsFilenameEdit.SetInitialDir(const Value: string);
begin
  FDialog.InitialDir := Value;
end;

procedure TsFilenameEdit.SetHistoryList(Value: TStrings);
begin
  FDialog.HistoryList := Value;
end;

procedure TsFilenameEdit.SetOptions(Value: TOpenOptions);
begin
  if Value <> FDialog.Options then begin
    FMultipleDirs := ofAllowMultiSelect in Value;
    FDialog.Options := Value;
    if not FMultipleDirs then ClearFileList;
  end;
end;

procedure TsFilenameEdit.SetDialogTitle(const Value: string);
begin
  FDialog.Title := Value;
end;

{ TsDirectoryEdit }

constructor TsDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SkinData.COC := COC_TsDirectoryEdit;
  FDefBmpID := iBTN_OPENFOLDER;
  FOptions := [sdAllowCreate, sdPerformCreate, sdPrompt];
  FRoot := SRFDesktop;
end;

procedure TsDirectoryEdit.ButtonClick;
var
  s: string;
  Flag: Boolean;
begin
  inherited;
  s := Text;
  Flag := True;
  DoBeforeDialog(s, Flag);  
  if not Flag then Exit;
  if (s = '') then begin
    if (InitialDir <> '') then s := InitialDir else s := '';
  end;
  if DirectoryExists(s) then begin
    if not NoChangeDir then ChDir(s);
  end else s := '';

  if SkinData.Skinned then begin
    PathDialogForm := TPathDialogForm.Create(Application);
    PathDialogForm.InitLngCaptions;

    PathDialogForm.sBitBtn3.Visible := sdAllowCreate in DialogOptions;
    PathDialogForm.sShellTreeView1.BoundLabel.Caption := DialogText;

    try
      PathDialogForm.sShellTreeView1.Root := FRoot;
      if (s <> '') and DirectoryExists(s) then PathDialogForm.sShellTreeView1.Path := s;
      if PathDialogForm.ShowModal = mrOk then begin
        s := PathDialogForm.sShellTreeView1.Path;
        if (s <> '') and DirectoryExists(s) then begin
          InitialDir := s;
          Text := s;
        end;
      end;
    finally
      FreeAndNil(PathDialogForm);
    end;
  end
  else begin
    if SelectDirectory('', FRoot, s) then begin
      if (Text = '') or not MultipleDirs then Text := s else Text := s + ';' + s;
      if (s <> '') and DirectoryExists(s) then InitialDir := s;
    end;
    if CanFocus then SetFocus;
  end;
  DoAfterDialog(s, Flag);
end;

procedure TsDirectoryEdit.ReceptFileDir(const AFileName: acString);
var
  s: string;
begin
  if FileExists(AFileName)
    then s := ExtractFilePath(AFileName)
    else s := AFileName;
  if (Text = '') or not MultipleDirs
    then Text := s
    else Text := Text + ';' + s;
end;

function TsDirectoryEdit.GetLongName: string;
var
  s: string;
  Pos: Integer;
begin
  if not MultipleDirs then Result := ShortToLongPath(Text)
  else begin
    Result := '';
    Pos := 1;
    while Pos <= Length(Text) do begin
      s := ShortToLongPath(ExtractSubstr(Text, Pos, [';']));
      if (Result <> '') and (s <> '') then Result := Result + ';';
      Result := Result + s;
    end;
  end;
end;

function TsDirectoryEdit.GetShortName: string;
var
  s: string;
  Pos: Integer;
begin
  if not MultipleDirs then Result := LongToShortPath(Text)
  else begin
    Result := '';
    Pos := 1;
    while Pos <= Length(Text) do begin
      s := LongToShortPath(ExtractSubstr(Text, Pos, [';']));
      if (Result <> '') and (s <> '') then Result := Result + ';';
      Result := Result + s;
    end;
  end;
end;

{ TsCustomDateEdit }

function NvlDate(DateValue, DefaultValue: TDateTime): TDateTime;
begin
  if DateValue = NullDate then Result := DefaultValue else Result := DateValue;
end;

constructor TsCustomDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SkinData.COC := COC_TsCustomDateEdit;

  FBlanksChar := ' ';
  FTitle := NewStr('Date select');
  FStartOfWeek := dowLocaleDefault;
  FWeekends := DefWeekends;
  FWeekendColor := clRed;
  FYearDigits := dyFour;
  FCalendarHints := TStringList.Create;
  TStringList(FCalendarHints).OnChange := CalendarHintsChanged;
  FDefBmpID := iBTN_DATE;
  FShowCurrentDate := True;
  FShowWeeks := False;
  FShowTodayBtn := True;

  ControlState := ControlState + [csCreating];
  Width := 86;
  try
    UpdateFormat;
  finally
    ControlState := ControlState - [csCreating];
  end;
end;

destructor TsCustomDateEdit.Destroy;
begin
  if FHooked then begin
    Application.UnhookMainWindow(FormatSettingsChange);
    FHooked := False;
  end;
  if Assigned(FPopupwindow) then FPopupWindow := nil;
  TStringList(FCalendarHints).OnChange := nil;
  if Assigned(FCalendarHints) then FreeAndNil(FCalendarHints);
  DisposeStr(FTitle);
  inherited Destroy;
end;

procedure TsCustomDateEdit.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);
  if Handle <> 0 then begin
    UpdateMask;
    if not (csDesigning in ComponentState) and not (IsLibrary or FHooked) then begin
      Application.HookMainWindow(FormatSettingsChange);
      FHooked := True;
    end;
  end;
end;

procedure TsCustomDateEdit.DestroyWindowHandle;
begin
  if FHooked then begin
    Application.UnhookMainWindow(FormatSettingsChange);
    FHooked := False;
  end;
  inherited DestroyWindowHandle;
end;

procedure TsCustomDateEdit.UpdateFormat;
begin
  FDateFormat := DefDateFormat(FourDigitYear);
end;

function TsCustomDateEdit.GetDateFormat: string;
begin
  Result := FDateFormat;
end;

function TsCustomDateEdit.TextStored: Boolean;
begin
  Result := not IsEmptyStr(Text, [#0, ' ', DateSeparator, FBlanksChar]);
end;

function TsCustomDateEdit.CheckValidDate : boolean;
{$IFDEF DELPHI7UP}
var
  D : TDateTime;
{$ENDIF}
begin
  Result := False;
  if TextStored then try
    FFormatting := True;
    try
{$IFDEF DELPHI7UP}
      Result := TryStrToDate(Text, D);
      if Result then SetDate(D) else Raise EConvertError.CreateFmt('Invalid Date', [Text]);
{$ELSE}
      SetDate(StrToDateFmt(FDateFormat, Text));
      Result := True;
{$ENDIF}
    finally
      FFormatting := False;
    end;
  except
    if CanFocus then SetFocus;
    raise;
  end;
end;

procedure TsCustomDateEdit.Change;
begin
  if not FFormatting then inherited Change;
end;

procedure TsCustomDateEdit.CMExit(var Message: TCMExit);
begin
  if not (csDesigning in ComponentState) and CheckOnExit and CheckValidDate then begin
    if (FMaxDate <> 0) and (Date > FMaxDate)
      then Date := FMaxDate
      else if (FMinDate <> 0) and (Date < FMinDate) then Date := FMinDate;
  end;
  inherited;
end;

procedure TsCustomDateEdit.SetBlanksChar(Value: Char);
begin
  if Value <> FBlanksChar then begin
    if (Value < ' ') then Value := ' ';
    FBlanksChar := Value;
    UpdateMask;
  end;
end;

procedure TsCustomDateEdit.UpdateMask;
var
  DateValue: TDateTime;
  OldFormat: string[10];
begin
  DateValue := GetDate;
  OldFormat := FDateFormat;
  UpdateFormat;
  if (GetDateMask <> EditMask) or (OldFormat <> FDateFormat) then begin
    { force update }
    EditMask := '';
    EditMask := GetDateMask;
  end;
  UpdatePopup;
  SetDate(DateValue);
end;

function TsCustomDateEdit.FormatSettingsChange(var Message: TMessage): Boolean;
begin
  Result := False;
  if (Message.Msg = WM_WININICHANGE) and Application.UpdateFormatSettings then UpdateMask;
end;

function TsCustomDateEdit.FourDigitYear: Boolean;
begin
  Result := (FYearDigits = dyFour);
  Result := Result or ((FYearDigits = dyDefault) and sDateUtils.NormalYears);
end;

function TsCustomDateEdit.GetDateMask: string;
begin
  Result := DefDateMask(FBlanksChar, FourDigitYear);
end;

function TsCustomDateEdit.GetDate: TDateTime;
begin
  if DefaultToday and not (csDesigning in ComponentState)
    then Result := SysUtils.Date
    else Result := NullDate;
  Result := StrToDateFmtDef(FDateFormat, Text, Result);
end;

procedure TsCustomDateEdit.SetDate(Value: TDateTime);
var
  D: TDateTime;
begin
  if not ValidDate(Value) or (Value = NullDate) then begin
    if DefaultToday and not (csDesigning in ComponentState) then Value := SysUtils.Date else Value := NullDate;
  end;
  D := Self.Date;
  if Value = NullDate then Text := '' else begin
    TEdit(Self).Text := FormatDateTime(FDateFormat, Value);
  end;
  Modified := D <> Date;
end;

procedure TsCustomDateEdit.ApplyDate(Value: TDateTime);
begin
  SetDate(Value);
  SelectAll;
end;

function TsCustomDateEdit.GetDialogTitle: string;
begin
  Result := FTitle^;
end;

procedure TsCustomDateEdit.SetDialogTitle(const Value: string);
begin
  AssignStr(FTitle, Value);
end;

function TsCustomDateEdit.IsCustomTitle: Boolean;
begin
  Result := (CompareStr('Date select', DialogTitle) <> 0) and (FTitle <> {$IFDEF UNICODE}NullAnsiStr{$ELSE}NullStr{$ENDIF});
end;

procedure TsCustomDateEdit.UpdatePopup;
var
  i : integer;
begin
  if (FPopupWindow <> nil) and (TsPopupCalendar(FPopupWindow).FCalendar <> nil) then begin
    TsPopupCalendar(FPopupWindow).FCalendar.StartOfWeek := FStartOfWeek;
    TsPopupCalendar(FPopupWindow).FCalendar.Weekends := FWeekends;
    TsPopupCalendar(FPopupWindow).FCalendar.WeekendColor := FWeekendColor;
    TsPopupCalendar(FPopupWindow).FFourDigitYear := FourDigitYear;
    TsPopupCalendar(FPopupWindow).sMonthCalendar1.ShowWeeks := FShowWeeks;
    TsPopupCalendar(FPopupWindow).sMonthCalendar1.ShowTodayBtn := FShowTodayBtn;
    FPopupWindow.Height := FormHeight + Integer(FShowTodayBtn) * 16;

    if Assigned(FOnGetCellParams)
      then TsPopupCalendar(FPopupWindow).sMonthCalendar1.OnGetCellParams := FOnGetCellParams
      else TsPopupCalendar(FPopupWindow).sMonthCalendar1.OnGetCellParams := nil;

    if Assigned(FOnDrawDay)
      then TsPopupCalendar(FPopupWindow).sMonthCalendar1.OnDrawDay := FOnDrawDay
      else TsPopupCalendar(FPopupWindow).sMonthCalendar1.OnDrawDay := nil;

    if SkinData.Skinned and (DefaultManager <> nil)
      then TsPopupCalendar(FPopupWindow).Color := DefaultManager.GetGlobalColor
      else TsPopupCalendar(FPopupWindow).Color := clBtnFace;
    for i := 0 to CalendarHints.Count -1 do begin
      TsPopupCalendar(FPopupWindow).FCalendar.FBtns[i].Hint := CalendarHints[i];
      TsPopupCalendar(FPopupWindow).FCalendar.FBtns[i].ShowHint := ShowHint;
      if i = 3 then break;
    end;
  end;
end;

procedure TsCustomDateEdit.SetYearDigits(Value: TYearDigits);
begin
  if FYearDigits <> Value then begin
    FYearDigits := Value;
    UpdateMask;
  end;
end;

procedure TsCustomDateEdit.SetCalendarHints(Value: TStrings);
begin
  FCalendarHints.Assign(Value);
end;

procedure TsCustomDateEdit.CalendarHintsChanged(Sender: TObject);
begin
  TStringList(FCalendarHints).OnChange := nil;
  try
    while (FCalendarHints.Count > 4) do
      FCalendarHints.Delete(FCalendarHints.Count - 1);
  finally
    TStringList(FCalendarHints).OnChange := CalendarHintsChanged;
  end;
  if not (csDesigning in ComponentState) then UpdatePopup;
end;

procedure TsCustomDateEdit.SetWeekendColor(Value: TColor);
begin
  if Value <> FWeekendColor then begin
    FWeekendColor := Value;
    UpdatePopup;
  end;
end;

procedure TsCustomDateEdit.SetWeekends(Value: sConst.TDaysOfWeek);
begin
  if Value <> FWeekends then begin
    FWeekends := Value;
    UpdatePopup;
  end;
end;

procedure TsCustomDateEdit.SetStartOfWeek(Value: TCalDayOfWeek);
begin
  if Value <> FStartOfWeek then begin
    FStartOfWeek := Value;
    UpdatePopup;
  end;
end;

procedure TsCustomDateEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_PRIOR, VK_NEXT, VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN, VK_ADD, VK_SUBTRACT]) and DroppedDown then begin
    TsPopupCalendar(FPopupWindow).FCalendar.FGrid.KeyDown(Key, Shift);
    Key := 0;
  end
  else if (Shift = []) and DirectInput then case Key of
    VK_ADD: begin
      ApplyDate(NvlDate(Date, Now) + 1);
      Key := 0;
    end;
    VK_SUBTRACT: begin
      ApplyDate(NvlDate(Date, Now) - 1);
      Key := 0;
    end;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TsCustomDateEdit.KeyPress(var Key: Char);
begin
  if (Key in ['T', 't', '+', '-']) and DroppedDown then begin
    TsPopupCalendar(FPopupWindow).FCalendar.FGrid.KeyPress(Key);
    Key := #0;
  end
  else if DirectInput and not ReadOnly then begin
    case Key of
      'T', 't': begin
        ApplyDate(Trunc(Now));
        Key := #0;
      end;
      '+', '-': begin
        Key := #0;
      end;
    end;
  end;
  inherited KeyPress(Key);
end;

procedure TsCustomDateEdit.PopupWindowShow;
begin
  if FPopupWindow = nil then FPopupWindow := TsPopupCalendar.Create(Self);
  if Self.Date <> NullDate
    then TsPopupCalendar(FPopupWindow).FCalendar.CalendarDate := Self.Date
    else TsPopupCalendar(FPopupWindow).FCalendar.CalendarDate := SysUtils.Date;

  TsPopupCalendar(FPopupWindow).FCalendar.ControlStyle := TsPopupCalendar(FPopupWindow).FCalendar.ControlStyle - [csDoubleClicks];
  TsPopupCalendar(FPopupWindow).FEditor := Self;
  UpdatePopup;
  inherited;
end;

procedure TsCustomDateEdit.Loaded;
begin
  inherited;
  Self.UpdateMask;
end;

procedure TsCustomDateEdit.WndProc(var Message: TMessage);
begin
  if Message.Msg = SM_ALPHACMD then case Message.WParamHi of 
    AC_REMOVESKIN : if LongWord(Message.LParam) = LongWord(SkinData.SkinManager) then begin
      if Assigned (FPopupWindow) then FPopupWindow.BroadCast(Message);
      UpdateFormat;
      UpdateMask;
    end;
    AC_SETNEWSKIN, AC_REFRESH : if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
      if Assigned (FPopupWindow) then FPopupWindow.BroadCast(Message);
      UpdateFormat;
      UpdateMask;
    end;
  end;
  inherited;
end;

procedure TsCustomDateEdit.SetMinDate(const Value: TDateTime);
begin
  if (FMaxDate <> NullDate) and (Value > FMaxDate) then Exit;
  if (FMinDate <> Value) then begin
    FMinDate := Value;
    if Date < FMinDate then Date := FMinDate;
  end;
end;

procedure TsCustomDateEdit.SetMaxDate(const Value: TDateTime);
begin
  if (FMaxDate <> Value) and (Value >= FMinDate) then begin
    FMaxDate := Value;
    if Date > FMaxDate then Date := FMaxDate;
  end;
end;

function TsCustomDateEdit.DateIsStored: boolean;
begin
  Result := not DefaultToday
end;

procedure TsCustomDateEdit.SetShowCurrentDate(const Value: boolean);
begin
  if FShowCurrentDate <> Value then begin
    FShowCurrentDate := Value;
  end;
end;

{ TsDateEdit }

constructor TsDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SkinData.COC := COC_TsDateEdit;
  EditMask := '!90/90/0000;1; ';
  UpdateMask;
end;

{ Utility routines }

procedure DateFormatChanged;
  procedure IterateControls(AControl: TWinControl);
  var
    I: Integer;
  begin
    with AControl do for I := 0 to ControlCount - 1 do
      if Controls[I] is TsCustomDateEdit
        then TsCustomDateEdit(Controls[I]).UpdateMask
        else if Controls[I] is TWinControl then IterateControls(TWinControl(Controls[I]));
  end;
var
  I: Integer;
begin
  if Screen <> nil then for I := 0 to Screen.FormCount - 1 do IterateControls(Screen.Forms[I]);
end;

function StrToDateFmt(const DateFormat, S: string): TDateTime;
begin
  if not InternalStrToDate(DateFormat, S, Result) then Raise EConvertError.CreateFmt('Invalid Date', [S]);
end;

function StrToDateFmtDef(const DateFormat, S: string; Default: TDateTime): TDateTime;
begin
  if not InternalStrToDate(DateFormat, S, Result) then Result := Trunc(Default);
end;

end.
