unit sPopupClndr;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, sMonthCalendar, comctrls, sConst, sPanel, acntUtils
  {$IFDEF DELPHI6UP}, Variants{$ENDIF}, sCustomComboEdit, sSkinProvider,
  Menus;

const
  FormHeight = 144;

type
  TsPopupCalendar = class(TForm)
    sMonthCalendar1: TsMonthCalendar;
    PopupMenu1: TPopupMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    OK1: TMenuItem;
    Cancel1: TMenuItem;
    procedure sToolButton3Click(Sender: TObject);
    procedure sToolButton1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure CalendarClick;
    procedure FormDeactivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  protected
    FCloseUp: TCloseUpEvent;
    procedure KeyPress(var Key: Char); override;
    function GetValue: Variant;
    procedure SetValue(const Value: Variant);
    procedure CloseUp(Accept: Boolean); virtual;
    procedure FillArOR;
    function GetRgnFromArOR : hrgn;
  public
    FFourDigitYear: Boolean;
    FEditor: TsCustomComboEdit;
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WndProc (var Message: TMessage); override;
    property FCalendar: TsMonthCalendar read sMonthCalendar1 write sMonthCalendar1;
    property OnCloseUp: TCloseUpEvent read FCloseUp write FCloseUp;
  end;

var
  sPopupCalendar: TsPopupCalendar;

implementation

{$R *.dfm}

uses sToolEdit, sStyleSimply, sSkinManager, sGraphUtils;

{ TsPopupCalendar }

var
  ArOR : sConst.TAOR;

function TsPopupCalendar.GetValue: Variant;
begin
  if (csDesigning in ComponentState) then
    Result := VarFromDateTime(SysUtils.Date)
  else
    Result := VarFromDateTime(FCalendar.CalendarDate);
end;

procedure TsPopupCalendar.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (FCalendar <> nil) and (Key <> #0) then FCalendar.FGrid.KeyPress(Key);
end;

procedure TsPopupCalendar.SetValue(const Value: Variant);
begin
  if not (csDesigning in ComponentState) then begin
    try
      if (Trim(ReplaceStr(VarToStr(Value), DateSeparator, '')) = '') or
        VarIsNull(Value) or VarIsEmpty(Value) then
        FCalendar.CalendarDate := VarToDateTime(SysUtils.Date)
      else FCalendar.CalendarDate := VarToDateTime(Value);
    except
      FCalendar.CalendarDate := VarToDateTime(SysUtils.Date);
    end;
  end;
end;

procedure TsPopupCalendar.sToolButton3Click(Sender: TObject);
begin
  Application.Minimize;
end;

procedure TsPopupCalendar.sToolButton1Click(Sender: TObject);
begin
  Close;
end;

procedure TsPopupCalendar.CloseUp(Accept: Boolean);
begin
  if Assigned(FCloseUp) then FCloseUp(Self, Accept);
end;

procedure TsPopupCalendar.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  CanAccept : boolean;
  d : TDateTime;
begin
  inherited;
  case Key of
    VK_RETURN: begin
      if FEditor <> nil then begin
        d := sMonthCalendar1.CalendarDate;
        CanAccept := True;
        if Assigned(TsDateEdit(FEditor).OnAcceptDate) then TsDateEdit(FEditor).OnAcceptDate(FEditor, d, CanAccept);
        if CanAccept then begin
          TsCustomDateEdit(FEditor).Date := d;
          if Assigned(TsCustomDateEdit(FEditor).OnChange) then TsCustomDateEdit(FEditor).OnChange(TsCustomDateEdit(FEditor));
        end;
        FEditor.SetFocus;
        if FEditor.AutoSelect then FEditor.SelectAll;
      end;
      Close;
    end;
    VK_ESCAPE: begin
      FEditor.SetFocus;
      Close;
    end;
  end;
end;

procedure TsPopupCalendar.FormShow(Sender: TObject);
var
  rgn : hrgn;
begin
  sMonthCalendar1.FDragBar.Cursor := crDefault;
//  Height := FormHeight;
//  if FEditor.Sh

  if (FEditor.SkinData.SkinManager <> nil) and FEditor.SkinData.SkinManager.Active then begin
    FillArOR;
    rgn := GetRgnFromArOR;
    SetWindowRgn(Handle, rgn, True);
  end;

  if (DefaultManager <> nil) and DefaultManager.Active then Color := DefaultManager.GetGlobalColor else Color := clBtnFace;
  if Assigned(FEditor) then sMonthCalendar1.ShowCurrentDate := TsDateEdit(FEditor).ShowCurrentDate;
end;

procedure TsPopupCalendar.CalendarClick;
var
  CanAccept : boolean;
  d : TDateTime;
begin
  CanAccept := True;
  if FEditor <> nil then begin
    d := sMonthCalendar1.CalendarDate;
    if Assigned(TsDateEdit(FEditor).OnAcceptDate) then TsDateEdit(FEditor).OnAcceptDate(FEditor, d, CanAccept);
    if CanAccept then begin
      TsCustomDateEdit(FEditor).Date := d;
      if Assigned(TsCustomDateEdit(FEditor).OnChange) then TsCustomDateEdit(FEditor).OnChange(TsCustomDateEdit(FEditor));
      Visible := False;
      if Assigned(FEditor) and FEditor.Visible and FEditor.Enabled then begin
        FEditor.SetFocus;
        if FEditor.AutoSelect then FEditor.SelectAll;
      end;
    end;
  end;
  if CanAccept then Close;
end;

procedure TsPopupCalendar.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TsPopupCalendar.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  TsDateEdit(FEditor).FPopupWindow := nil;
  Inherited;
end;

procedure TsPopupCalendar.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_CLIPSIBLINGS;
end;

procedure TsPopupCalendar.CreateWnd;
begin
  inherited;
  SetClassLong(Handle, GCL_STYLE, GetClassLong(Handle, GCL_STYLE) or $20000);
end;

procedure TsPopupCalendar.FillArOR;
begin
  SetLength(ArOR, 0);
  if FEditor.SkinData.SkinManager.IsValidImgIndex(sMonthCalendar1.SkinData.BorderIndex) then begin
    AddRgn(ArOR, Width, FEditor.SkinData.SkinManager.ma[sMonthCalendar1.SkinData.BorderIndex], 0, False);
    AddRgn(ArOR, Width, FEditor.SkinData.SkinManager.ma[sMonthCalendar1.SkinData.BorderIndex], Height - FEditor.SkinData.SkinManager.ma[sMonthCalendar1.SkinData.BorderIndex].WB, True);
  end;
end;

function TsPopupCalendar.GetRgnFromArOR: hrgn;
var
  l, i : integer;
  subrgn : HRGN;
begin
  l := Length(ArOR);
  Result := CreateRectRgn(0, 0, Width, Height);
  if l > 0 then for i := 0 to l - 1 do begin
    subrgn := CreateRectRgn(ArOR[i].Left, ArOR[i].Top, ArOR[i].Right, ArOR[i].Bottom);
    CombineRgn(Result, Result, subrgn, RGN_DIFF);
    DeleteObject(subrgn);
  end;
end;

procedure TsPopupCalendar.WndProc(var Message: TMessage);
var
  rgn : hrgn;
begin
  case Message.Msg of
    WM_ERASEBKGND : if Assigned(DefaultManager) and DefaultManager.Active then Exit;
    WM_NCPAINT : if (FEditor <> nil) and (TsCustomDateEdit(FEditor).SkinData.SkinManager <> nil) and TsCustomDateEdit(FEditor).SkinData.SkinManager.Active then begin
      FillArOR;
      rgn := GetRgnFromArOR;
      SetWindowRgn(Handle, rgn, False);
    end;
  end;

  inherited;
end;

end.
