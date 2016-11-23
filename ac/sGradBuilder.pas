unit sGradBuilder;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus, sGradient, StdCtrls, Buttons, sSkinProvider, sPanel, sLabel,
  sButton, sComboBox, ComCtrls, sRadioButton, sDialogs, sPageControl, sColorSelect,
  sSpeedButton, sGroupBox;

type
  TGradPoint = class(TPanel)
  public
    Number : integer;
    constructor Create(AOwner: TComponent); override;
  end;

  TPointArray = array of TGradPoint;

  TGradBuilder = class(TForm)
    PopupMenu1: TPopupMenu;
    Changecolor1: TMenuItem;
    Delete1: TMenuItem;
    sSkinProvider1: TsSkinProvider;
    BitBtn1: TsButton;
    sButton1: TsButton;
    sButton2: TsButton;
    sRadioButton1: TsRadioButton;
    sRadioButton2: TsRadioButton;
    sRadioButton3: TsRadioButton;
    sGroupBox1: TsGroupBox;
    PaintPanel: TsPanel;
    PaintBox1: TPaintBox;
    Panel2: TsPanel;
    TemplatePanel: TsPanel;
    sGroupBox2: TsGroupBox;
    sPanel1: TsPanel;
    PaintBox2: TPaintBox;
    sColorSelect5: TsColorSelect;
    sColorSelect3: TsColorSelect;
    sColorSelect4: TsColorSelect;
    sColorSelect2: TsColorSelect;
    sColorSelect1: TsColorSelect;
    procedure Panel2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TemplatePanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TemplatePanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Changecolor1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure TemplatePanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TemplatePanelDblClick(Sender: TObject);
    procedure sButton1Click(Sender: TObject);
    procedure sButton2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sRadioButton1Click(Sender: TObject);
    procedure sColorSelect1Change(Sender: TObject);
  private
    function GetDirection: integer;
    procedure SetDirection(const Value: integer);
    procedure MakefirstPoints;
    procedure InitTriangles;
  public
    a : TPointArray;
    g : TsGradArray;
    LastPoint : TGradPoint;
    ModalResult : TModalResult;
    CurrentArray : TsGradArray;
    ColorDialog1 : TColorDialog;
    function AsString : string;
    procedure LoadFromArray(NewArray : TsGradArray);
    procedure ReCalcData;
    procedure NewPoint(Owner : TsPanel; y : integer; Color : TColor);
    procedure DeleteFromArray(p : TGradPoint);
    procedure Reinit;
    property Direction : integer read GetDirection write SetDirection;
  end;

const
  PointHeight = 6;

var
  GradBuilder : TGradBuilder;
  NoMouse : boolean;
  ColorDialog : TColorDialog;

procedure CreateEditorForm; overload;
procedure CreateEditorForm(CustomDlg : TColorDialog); overload;
procedure KillForm;

implementation

uses math, acntUtils, sConst, sStyleSimply, sGraphUtils;

const

  acs_FirstPoint = 'FirstPoint';
  acs_LastPoint  = 'LastPoint';

{$R *.DFM}

procedure CreateEditorForm;
begin
  Application.CreateForm(TGradBuilder, GradBuilder);
end;

procedure CreateEditorForm(CustomDlg : TColorDialog); overload;
begin
  CreateEditorForm;
  ColorDialog := CustomDlg;
  GradBuilder.ColorDialog1 := CustomDlg;
  GradBuilder.sColorSelect1.ColorDialog := CustomDlg;
  GradBuilder.sColorSelect2.ColorDialog := CustomDlg;
  GradBuilder.sColorSelect3.ColorDialog := CustomDlg;
  GradBuilder.sColorSelect4.ColorDialog := CustomDlg;
  GradBuilder.sColorSelect5.ColorDialog := CustomDlg;
end;

procedure KillForm;
begin
  if Assigned(GradBuilder) then GradBuilder.Release;
end;

procedure TGradBuilder.sRadioButton1Click(Sender: TObject);
begin
  Direction := TsRadioButton(Sender).Tag;
end;

procedure TGradBuilder.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (ColorDialog = nil) and (ColorDialog1 <> nil) then FreeAndNil(ColorDialog1) else ColorDialog1 := nil;
end;

procedure TGradBuilder.ReCalcData;
var
  i, c, n : integer;
  CurPoint, PrevPoint : TGradPoint;
  function GetMinPoint(top : integer) : TGradPoint;
  var
    i : integer;
  begin
    Result := a[1];
    for i := 0 to c - 1 do begin
      if (a[i].Top > Top) and (a[i].Top <= Result.Top) then begin
        Result := a[i];
      end;
    end;
  end;
begin
  if Direction <> 2 then begin
    SetLength(g, 0);
    c := Length(a);
    PrevPoint := a[0];
    for i := 0 to c - 1 do begin
      CurPoint := GetMinPoint(PrevPoint.Top);

      SetLength(g, Length(g) + 1);

      g[Length(g) - 1].Color1 := ColorToRGB(PrevPoint.Color);
      g[Length(g) - 1].Color2 := ColorToRGB(CurPoint.Color);
      n := Round((CurPoint.Top - PrevPoint.Top) / 2.2);//* 100 / (PaintPanel.Height - PointHeight));
      g[Length(g) - 1].Percent :=  min(n, 100);
      g[Length(g) - 1].Mode1 := max(Direction, 0);

      PrevPoint := CurPoint;
    end;
    GradBuilder.PaintBox1.Repaint;
  end
  else begin
    c := Length(g);
    if c > 0 then g[0].color1 := sColorSelect1.ColorValue;
    if c > 1 then g[1].color1 := sColorSelect5.ColorValue;
    if c > 2 then g[2].color1 := sColorSelect2.ColorValue;
    if c > 3 then g[3].color1 := sColorSelect4.ColorValue;
    if c > 4 then g[4].color1 := sColorSelect3.ColorValue;
    GradBuilder.PaintBox2.Repaint;
  end;
end;

procedure TGradBuilder.NewPoint(Owner : TsPanel; y : integer; Color : TColor);
var
  c : integer;
begin
  c := Length(a);
  a[c - 1] := TGradPoint.Create(Owner);
  a[c - 1].Parent := Owner;
  a[c - 1].Left := 5;
  a[c - 1].Width := Owner.Width - 10;
  a[c - 1].Top := y;
  a[c - 1].Visible := True;
  a[c - 1].Caption := ' ';
  a[c - 1].Color := ColorToRGB(Color);
  a[c - 1].PopupMenu := GradBuilder.PopupMenu1;
  a[c - 1].onMouseDown := GradBuilder.TemplatePanel.OnMouseDown;
  a[c - 1].onMouseUp := GradBuilder.TemplatePanel.OnMouseUp;
  a[c - 1].onMouseMove := GradBuilder.TemplatePanel.OnMouseMove;
  a[c - 1].onDblClick := GradBuilder.TemplatePanel.OnDblClick;
  a[c - 1].Number := Length(a) - 1;
end;

procedure TGradBuilder.DeleteFromArray(p : TGradPoint);
var
  i : integer;
begin
  for i := p.Number to Length(a) - 2 do begin
    a[i] := a[i + 1];
  end;
  SetLength(a, Length(a) - 1);
  p.PopupMenu := nil;
  p.onMouseDown := nil;
  p.onMouseMove := nil;
  p.onMouseUp := nil;
  if Assigned(p) then FreeAndNil(p);
end;

{ TGradPoint }

constructor TGradPoint.Create(AOwner: TComponent);
begin
  inherited;
  Tag := ExceptTag; // Preventing of automatic skinning
  Parent := TWinControl(AOwner);
  Visible := False;
  Height := PointHeight;
end;

{ TGradBuilder }

procedure TGradBuilder.Panel2Click(Sender: TObject);
var
  m : TMouse;
  p : TPoint;
begin
  m := TMouse.Create;
  p := m.CursorPos;
  p := Panel2.ScreenToClient(p);
  if Assigned(m) then FreeAndNil(m);

  SetLength(a, Length(a) + 1);
  NewPoint(Panel2, p.y, clWhite);
  ReCalcData;
end;

procedure TGradBuilder.FormCreate(Sender: TObject);
begin
  MakefirstPoints
end;

procedure TGradBuilder.TemplatePanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  t : integer;
begin
  if not NoMouse then begin
    LastPoint := TGradPoint(Sender);
    if (Button = mbLeft) and (LastPoint.Name <> acs_FirstPoint) and (LastPoint.Name <> acs_LastPoint) then begin
      t := LastPoint.Top;
      ReleaseCapture;
      LastPoint.Perform(WM_SYSCOMMAND, $F012, 0);
      if t <> LastPoint.Top then begin
        ReCalcData;
      end;
    end;
  end;
  NoMouse := False;
end;

procedure TGradBuilder.TemplatePanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  TPanel(Sender).Left := 5;
  TPanel(Sender).Width := Panel2.Width - 10;
end;

procedure TGradBuilder.Changecolor1Click(Sender: TObject);
begin
  if ColorDialog1 = nil then ColorDialog1 := TsColorDialog.Create(Application);
  ColorDialog := ColorDialog1;
  ColorDialog.Color := LastPoint.Color;
  if ColorDialog1.Execute then begin
    LastPoint.Color := ColorToRGB(ColorDialog1.Color);
    ReCalcData;
  end;
end;

procedure TGradBuilder.PopupMenu1Popup(Sender: TObject);
begin
  if LastPoint = nil then Exit;
  Delete1.Enabled := (LastPoint.Name <> acs_FirstPoint) and (LastPoint.Name <> acs_LastPoint);
end;

procedure TGradBuilder.Delete1Click(Sender: TObject);
begin
  if LastPoint = nil then Exit;
  DeleteFromArray(LastPoint);
  ReCalcData;
end;

procedure TGradBuilder.PaintBox1Paint(Sender: TObject);
var
  b : TBitMap;
begin
  b := CreateBmp24(TPaintBox(Sender).Width, TPaintBox(Sender).Height);
  try
    PaintGrad(b, Rect(0, 0, b.Width, b.Height), g);
    TPaintBox(Sender).Canvas.Draw(0, 0, b);
  finally
    FreeAndNil(b);
  end;
end;

procedure TGradBuilder.FormShow(Sender: TObject);
begin
  ReCalcData;
  if (CurrentArray <> nil) and (Length(CurrentArray) > 0)
    then Direction := max(CurrentArray[0].Mode1, 0)
    else Direction := 0;
end;

procedure TGradBuilder.BitBtn1Click(Sender: TObject);
begin
  ModalResult := mrCancel;
  Close;
end;

function TGradBuilder.AsString: string;
var
  i : integer;
begin
  Result := '';
  for i := 0 to Length(g) - 1 do begin
    Result := Result + IntToStr(ColorToRGB(g[i].Color1)) + ';' +
                       IntToStr(ColorToRGB(g[i].Color2)) + ';' +
                       IntToStr(g[i].Percent) + ';' +
                       IntToStr(g[i].Mode1) + ';' +
                       IntToStr(g[i].Mode2) + ';';
  end;
  Delete(Result, Length(Result), 1);
end;

procedure TGradBuilder.LoadFromArray(NewArray: TsGradArray);
var
  i, c : integer;
begin
  CurrentArray := NewArray;
  c := Length(NewArray) - 1;
  if c < 1 then Exit;
  SetLength(g, c + 1);
  for i := 0 to c do begin
    g[i].Color1 := NewArray[i].Color1;
    g[i].Color2 := NewArray[i].Color2;
    g[i].Percent := NewArray[i].Percent;
    g[i].Mode1 := NewArray[i].Mode1;
    g[i].Mode2 := NewArray[i].Mode2;
  end;
  Direction := NewArray[0].Mode1;
end;

procedure TGradBuilder.TemplatePanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ReCalcData;
end;

procedure TGradBuilder.TemplatePanelDblClick(Sender: TObject);
begin
  if ColorDialog1 = nil then ColorDialog1 := TsColorDialog.Create(Application);
  LastPoint := TGradPoint(Sender);
  NoMouse := True;
  ColorDialog1.Color := LastPoint.Color;
  if ColorDialog1.Execute then begin
    LastPoint.Color := ColorToRGB(ColorDialog1.Color);
    ReCalcData;
  end;
end;

procedure TGradBuilder.sButton1Click(Sender: TObject);
begin
  ModalResult := mrOk;
  Close;
end;

procedure TGradBuilder.sButton2Click(Sender: TObject);
begin
  if
{$IFNDEF ALITE}
  Customrequest('Are you sure that want to clear a current gradient information??')
{$ELSE}
  MessageDlg('Are you sure that want to clear a current gradient information?', mtConfirmation, [mbYes, mbNo], 0) = mrYes
{$ENDIF}
   then begin
    ModalResult := mrNone;
    Close;
  end;
end;

function TGradBuilder.GetDirection: integer;
begin
  if sRadioButton1.Checked then Result := 0 else if sRadioButton2.Checked then Result := 1 else Result := 2;
end;

procedure TGradBuilder.SetDirection(const Value: integer);
var
  i : integer;
begin
  case Value of
    0 : begin
      sRadioButton1.Checked := True;
      sGroupBox1.BringToFront;
      MakefirstPoints
    end;
    1 : begin
      sRadioButton2.Checked := True;
      sGroupBox1.BringToFront;
      MakefirstPoints
    end;
    2 : begin
      sRadioButton3.Checked := True;
      sGroupBox2.BringToFront;
    end;
  end;
  for i := 0 to Length(g) - 1 do g[i].Mode1 := Value;
  Reinit;
end;

procedure TGradBuilder.sColorSelect1Change(Sender: TObject);
begin
  SetLength(g, 5);
  g[TsColorSelect(Sender).Tag].Color1 := ColorToRGB(TsColorSelect(Sender).ColorValue);
  InitTriangles;
  Reinit;
end;

procedure TGradBuilder.Reinit;
var
  i, c : integer;
  h : integer;
begin
  c := Length(g);
  if Direction < 2 then begin
    SetLength(a, 0);
    while Panel2.ComponentCount > 0 do Panel2.Components[0].Destroy;
    MakefirstPoints;
    h := 0;
    for i := 0 to Panel2.ComponentCount - 1 do begin
      if Panel2.Components[i].Name = acs_FirstPoint
        then TGradPoint(Panel2.Components[i]).Color := ColorToRGB(g[0].Color1)
        else if Panel2.Components[i].Name = acs_LastPoint then TGradPoint(Panel2.Components[i]).Color := ColorToRGB(g[c - 1].Color2);
    end;
    for i := 1 to c - 2 do begin
      inc(h, Round(g[i - 1].Percent * 2.2));
      SetLength(a, Length(a) + 1);
      NewPoint(Panel2, h, ColorToRGB(g[i].Color1));
    end;
  end
  else begin
    SetLength(a, 0);
    while Panel2.ComponentCount > 0 do Panel2.Components[0].Destroy;

    if c > 0 then sColorSelect1.ColorValue := g[0].color1 else sColorSelect1.ColorValue := 0;
    if c > 1 then sColorSelect5.ColorValue := g[1].color1 else sColorSelect5.ColorValue := sColorSelect1.ColorValue;
    if c > 2 then sColorSelect2.ColorValue := g[2].color1 else sColorSelect2.ColorValue := sColorSelect5.ColorValue;
    if c > 3 then sColorSelect4.ColorValue := g[3].color1 else sColorSelect4.ColorValue := sColorSelect2.ColorValue;
    if c > 4 then sColorSelect3.ColorValue := g[4].color1 else sColorSelect3.ColorValue := sColorSelect4.ColorValue;
  end;
  ReCalcData;
end;

procedure TGradBuilder.MakefirstPoints;
begin
  if Panel2.ComponentCount > 0 then Exit;
  // FirstPoint
  SetLength(a, 1);
  NewPoint(Panel2, 0, clWhite);
  a[Length(a) - 1].Name := acs_FirstPoint;

  //LastPoint
  SetLength(a, 2);
  NewPoint(Panel2, Panel2.Height - PointHeight, clBtnShadow);
  a[Length(a) - 1].Name := acs_LastPoint;
end;

procedure TGradBuilder.InitTriangles;
begin
  if Length(g) < 5 then SetLength(g, 5);
  g[0].Color1 := sColorSelect1.ColorValue;
  g[0].Color2 := sColorSelect5.ColorValue;
  g[0].Percent := 24;
  g[1].Color1 := sColorSelect5.ColorValue;
  g[1].Color2 := sColorSelect2.ColorValue;
  g[1].Percent := 24;
  g[2].Color1 := sColorSelect2.ColorValue;
  g[2].Color2 := sColorSelect4.ColorValue;
  g[2].Percent := 24;
  g[3].Color1 := sColorSelect4.ColorValue;
  g[3].Color2 := sColorSelect3.ColorValue;
  g[3].Percent := 24;
  g[4].Color1 := sColorSelect3.ColorValue;
  g[4].Color2 := sColorSelect3.ColorValue;
  g[4].Percent := 0;
end;

end.

