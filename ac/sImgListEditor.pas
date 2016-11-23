unit sImgListEditor;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, ActnList, Buttons, ImgList, ExtDlgs,
  sPanel, sSpeedButton, acAlphaImageList, sSkinProvider, sListView,
  sBitBtn, sButton;

type
  TFormImgListEditor = class(TForm)
    sBitBtn5: TsBitBtn;
    sBitBtn6: TsBitBtn;
    sBitBtn7: TsBitBtn;
    ImageList1: TsAlphaImageList;
    OpenPictureDialog1: TOpenPictureDialog;
    ListView1: TsListView;
    SpeedButton1: TsSpeedButton;
    SpeedButton2: TsSpeedButton;
    SpeedButton3: TsSpeedButton;
    SpeedButton4: TsSpeedButton;
    sSkinProvider1: TsSkinProvider;
    sAlphaImageList1: TsAlphaImageList;
    SaveDialog1: TSaveDialog;
    SpeedButton5: TsSpeedButton;
    procedure ListView1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ListView1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure sBitBtn5Click(Sender: TObject);
    procedure sBitBtn6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sBitBtn7Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure CheckScroll(Sender: TObject);
    procedure ListView1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    sImageList : TsAlphaImageList;
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DropFiles;
    procedure ListViewRefresh;
    procedure InitFromImgList(ImgList : TsAlphaImageList);
  end;

var
  FormImgListEditor: TFormImgListEditor;
  ScrollTimer : TTimer = nil;

implementation

uses acntUtils, CommCtrl, ShellAPI, sConst, acPNG, sVCLUtils;

{$R *.DFM}

{ TFormImgListEditor }

const
  scBorder = 8;

procedure StartScroll;
begin
  if ScrollTimer = nil then begin
    ScrollTimer := TTimer.Create(FormImgListEditor);
    ScrollTimer.OnTimer := FormImgListEditor.CheckScroll;
    ScrollTimer.Interval := 10;
  end;
end;

procedure EndScroll;
begin
  if ScrollTimer <> nil then FreeAndNil(ScrollTimer);
end;

procedure DoScroll(X, Y : integer);
var
  dx{, dy} : integer;
begin
  dx := 0;
//  dy := dx;
  if X > FormImgListEditor.ListView1.Width - scBorder then begin
    dx := 10;
  end
  else if X < scBorder then begin
    dx := -10;
{  end
  else if Y < scBorder then begin
    dy := -10;
  end
  else if Y > FormImgListEditor.ListView1.Height - scBorder then begin
    dx := 10;}
  end;
  if (dx <> 0){ or (dy <> 0)} then begin
    FormImgListEditor.ListView1.Scroll(dx, 0);
    FormImgListEditor.ListView1.Invalidate;
  end;
end;

procedure TFormImgListEditor.InitFromImgList(ImgList: TsAlphaImageList);
begin
  ImageList1.DoubleData := True;
  ImageList1.Clear;
  ImageList1.Items.Clear;

  sImageList := ImgList;
  Caption := ImgList.Owner.Name + '.' + ImgList.GetNamePath;

  ImageList1.Width := ImgList.Width;
  ImageList1.Height := ImgList.Height;

  ImageList_SetBkColor(ImageList1.Handle, ColorToRGB(ListView1.Color));

  ImageList1.CopyImages(ImgList);

  ListViewRefresh;
end;

procedure TFormImgListEditor.ListView1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  li : TListItem;
begin
  li := ListView1.GetItemAt(X, Y);
  Accept := (li <> nil) and (li <> ListView1.ItemFocused);

  if (X < scBorder) or (X > ListView1.Width - scBorder) {or (Y < scBorder) or (Y > ListView1.Height - scBorder) }then StartScroll

end;

procedure TFormImgListEditor.ListView1DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  li : TListItem;
begin
  li := ListView1.GetItemAt(X, Y);

  ImageList1.Items[ListView1.ItemFocused.Index].Index := li.Index;
  ImageList1.Move(ListView1.ItemFocused.Index, li.Index);
  EndScroll;
end;

procedure TFormImgListEditor.ListViewRefresh;
var
  i : integer;
begin
  SendMessage(ListView1.Handle, WM_SETREDRAW, 0, 0);
  ListView1.Items.Clear;
  for i := 0 to ImageList1.Count - 1 do begin
    ListView1.Items.Add.Selected;
    ListView1.Items.Item[i].ImageIndex := i;
    ListView1.Items.Item[i].Caption := IntToStr(i);
  end;
  SendMessage(ListView1.Handle, WM_SETREDRAW, 1, 0);
  SpeedButton3.Enabled := ImageList1.Count > 0;
end;

procedure TFormImgListEditor.FormShow(Sender: TObject);
begin
  ListView1.Anchors := ListView1.Anchors + [akBottom];
  sBitBtn5.Anchors := sBitBtn5.Anchors + [akBottom, akRight] - [akTop, akLeft];
  sBitBtn6.Anchors := sBitBtn6.Anchors + [akBottom, akRight] - [akTop, akLeft];
  sBitBtn7.Anchors := sBitBtn7.Anchors + [akBottom, akRight] - [akTop, akLeft];
  Height := 110 + ImageList1.Height + GetSystemMetrics(SM_CYVSCROLL);
end;

procedure TFormImgListEditor.ListView1Click(Sender: TObject);
begin
{$IFDEF DELPHI7UP}
  SpeedButton2.Enabled := ListView1.ItemIndex > -1;
{$ELSE}
  SpeedButton2.Enabled := ListView1.Selected <> nil;
{$ENDIF}
  SpeedButton3.Enabled := ImageList1.Count > 0;
  SpeedButton4.Enabled := SpeedButton2.Enabled;
  SpeedButton5.Enabled := SpeedButton2.Enabled;
end;

procedure TFormImgListEditor.SpeedButton1Click(Sender: TObject);
var
  i : integer;
begin
  if OpenPictureDialog1.Execute then begin
    for i := 0 to OpenPictureDialog1.Files.Count - 1 do begin
      ImageList1.LoadFromfile(OpenPictureDialog1.Files[i]);
    end;
    ListViewRefresh;
  end;
end;

procedure TFormImgListEditor.SpeedButton2Click(Sender: TObject);
var
  i, j : integer;
begin
  if ListView1.Selected <> nil then begin
    i := ListView1.Selected.Index;
{$IFDEF DELPHI7UP}
    ListView1.DeleteSelected;
{$ELSE}
    ListView1.Items.Delete(i);
{$ENDIF}
    ImageList1.Delete(i);
    if i < ImageList1.Items.Count then ImageList1.Items.Delete(i);
    if ListView1.Items.Count > 0 then begin
      for j := 0 to ListView1.Items.Count - 1 do begin
        ListView1.Items[j].Caption := IntToStr(j);
        if j >= i then ListView1.Items[j].ImageIndex := ListView1.Items[j].ImageIndex - 1;
      end;
      if i > ListView1.Items.Count - 1 then i := ListView1.Items.Count - 1;
      ListView1.Selected := ListView1.Items[i];
    end;
  end;
end;

procedure TFormImgListEditor.SpeedButton3Click(Sender: TObject);
begin
  if MessageDlg('Do you want to remove all icons?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    ImageList1.Clear;
    ImageList1.Items.Clear;
    ListViewRefresh;
    SpeedButton3.Enabled := False;
  end;
end;

procedure TFormImgListEditor.sBitBtn5Click(Sender: TObject);
begin
  if Assigned(sImageList) then begin
    ImageList_SetBkColor(ImageList1.Handle, CLR_NONE);
    sImageList.Assign(ImageList1);
  end;
  Close;
end;

procedure TFormImgListEditor.sBitBtn6Click(Sender: TObject);
begin
  Close;
end;

procedure TFormImgListEditor.WMDropFiles(var Msg: TWMDropFiles);
const
  maxlen = 254;
var
  h : THandle;
  i, num : integer;
  pchr : array [0..maxlen] of acChar;
  fname : acString;
begin
  h := Msg.Drop;

  num := DragQueryFile(h, Dword(-1), nil, 0);
  if num > 0 then begin
    for i := 0 to num - 1 do begin
{$IFDEF TNTUNICODE}
      DragQueryFileW(h, i, pchr, maxlen);
{$ELSE}
      DragQueryFile(h, i, pchr, maxlen);
{$ENDIF}
      fname := acString(pchr);
      ImageList1.LoadFromfile(fname);
    end;
    ListViewRefresh;
  end;
  
  DragFinish(h);
end;

procedure TFormImgListEditor.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(Handle, True);
end;

procedure TFormImgListEditor.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(Handle, False);
end;

procedure TFormImgListEditor.ListView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = 46 then begin
    SpeedButton2.Click;
  end;
end;

procedure TFormImgListEditor.sBitBtn7Click(Sender: TObject);
begin
  if Assigned(sImageList) then begin
    ImageList_SetBkColor(ImageList1.Handle, CLR_NONE);
    sImageList.Assign(ImageList1);
  end;
end;

procedure TFormImgListEditor.SpeedButton4Click(Sender: TObject);
var
  Ico : TIcon;
begin
  if (ListView1.Selected <> nil) and SaveDialog1.Execute then begin
    Ico := TIcon.Create;
{$IFDEF DELPHI7UP}
    ImageList1.GetIcon(ListView1.ItemIndex, Ico);
{$ELSE}
    ImageList1.GetIcon(ListView1.Selected.Index, Ico);
{$ENDIF}
    Ico.SaveToFile(SaveDialog1.FileName);
    Ico.Free;
  end;
end;

procedure TFormImgListEditor.SpeedButton5Click(Sender: TObject);
var
  Index : integer;
  Icon: HICON;
  iFormat : TsImageFormat;
  Png : TPNGGraphic;
begin
  if OpenPictureDialog1.Execute then begin
    if GetImageFormat(OpenPictureDialog1.FileName, iFormat) then begin
{$IFDEF DELPHI7UP}
      Index := ListView1.ItemIndex;
{$ELSE}
      Index := ListView1.Selected.Index;
{$ENDIF}
      if Index < 0 then Exit;

      ImageList1.Items[Index].ImageFormat := iFormat;
      ImageList1.Items[Index].ImgData.Clear;
      ImageList1.Items[Index].ImgData.LoadFromFile(OpenPictureDialog1.FileName);

      case iFormat of
        ifPNG : begin
          Png := TPNGGraphic.Create;
          Png.LoadFromFile(OpenPictureDialog1.FileName);
          Icon := MakeIcon32(Png);
          Png.Free;
        end;
        ifICO : begin
  {$IFDEF TNTUNICODE}
          Icon := ExtractIconW(hInstance, PacChar(OpenPictureDialog1.FileName), 0);
  {$ELSE}
          Icon := ExtractIcon(hInstance, PacChar(OpenPictureDialog1.FileName), 0);
  {$ENDIF}
        end;
      end;
      if Icon <> 0 then begin
        ImageList_ReplaceIcon(ImageList1.Handle, Index, Icon);
        DestroyIcon(Icon);
        ListViewRefresh;
{$IFDEF DELPHI7UP}
        ListView1.ItemIndex := Index;
{$ELSE}
        ListView1. Selected := ListView1.Items[Index];
{$ENDIF}
      end;

    end
    else begin
      ShowError('Unsupported file format');
    end;
  end;
end;

procedure TFormImgListEditor.CheckScroll(Sender: TObject);
var
  P : TPoint;
begin
  if (ScrollTimer <> nil) and not (csDestroying in ScrollTimer.ComponentState) then begin
    ScrollTimer.Enabled := False;
    P := ListView1.ScreenToClient(acMousePos);
    DoScroll(P.X, P.Y);
    if ScrollTimer <> nil then ScrollTimer.Enabled := True;
  end;
end;

procedure TFormImgListEditor.ListView1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  EndScroll;
end;

end.
