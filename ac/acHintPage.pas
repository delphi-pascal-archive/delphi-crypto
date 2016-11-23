unit acHintPage;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, {$IFDEF DELPHI6}Variants, {$ENDIF}Classes, Graphics, Controls, Forms, 
  Dialogs, sScrollBox, Buttons, sSpeedButton, ComCtrls, sPageControl,
  sFrameAdapter, StdCtrls, sLabel, sTrackBar, sRadioButton, sGroupBox, acAlphaHints,
  ExtCtrls, ExtDlgs, sPanel;

type
  TFrameHintPage = class(TFrame)
    sScrollBox1: TsScrollBox;
    sFrameAdapter1: TsFrameAdapter;
    PaintBox1: TPaintBox;
    sGroupBox1: TsGroupBox;
    sSpeedButton3: TsSpeedButton;
    sLabel21: TsLabel;
    sLabel22: TsLabel;
    sLabel23: TsLabel;
    sLabel24: TsLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    sGroupBox2: TsGroupBox;
    sTrackBar1: TsTrackBar;
    sTrackBar4: TsTrackBar;
    sTrackBar6: TsTrackBar;
    sTrackBar8: TsTrackBar;
    sLabel14: TsLabel;
    sLabel19: TsLabel;
    sLabel2: TsLabel;
    sLabel9: TsLabel;
    sGroupBox3: TsGroupBox;
    sTrackBar2: TsTrackBar;
    sTrackBar3: TsTrackBar;
    sTrackBar5: TsTrackBar;
    sTrackBar7: TsTrackBar;
    sLabel4: TsLabel;
    sLabel10: TsLabel;
    sLabel15: TsLabel;
    sLabel20: TsLabel;
    sGroupBox4: TsGroupBox;
    sGroupBox5: TsPanel;
    sRadioButton1: TsRadioButton;
    sRadioButton2: TsRadioButton;
    sGroupBox6: TsPanel;
    sRadioButton3: TsRadioButton;
    sRadioButton4: TsRadioButton;
    sLabel26: TsLabel;
    sLabel27: TsLabel;
    sLabel28: TsLabel;
    sLabel29: TsLabel;
    sLabel30: TsLabel;
    sLabel31: TsLabel;
    sLabel32: TsLabel;
    sLabel33: TsLabel;
    sPanel1: TsPanel;
    sPanel2: TsPanel;
    sRadioButton5: TsRadioButton;
    sRadioButton6: TsRadioButton;
    sLabel34: TsLabel;
    sLabel35: TsLabel;
    sLabel36: TsLabel;
    sLabel37: TsLabel;
    sRadioButton7: TsRadioButton;
    sRadioButton8: TsRadioButton;
    sPanel3: TsPanel;
    sRadioButton9: TsRadioButton;
    sRadioButton10: TsRadioButton;
    sLabel38: TsLabel;
    procedure sTrackBar1Change(Sender: TObject);
    procedure sTrackBar2Change(Sender: TObject);
    procedure sTrackBar4Change(Sender: TObject);
    procedure sTrackBar3Change(Sender: TObject);
    procedure sTrackBar6Change(Sender: TObject);
    procedure sTrackBar5Change(Sender: TObject);
    procedure sTrackBar8Change(Sender: TObject);
    procedure sTrackBar7Change(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure sSpeedButton3Click(Sender: TObject);
    procedure sRadioButton1Click(Sender: TObject);
    procedure sRadioButton2Click(Sender: TObject);
    procedure sRadioButton3Click(Sender: TObject);
    procedure sRadioButton4Click(Sender: TObject);
    procedure sRadioButton5Click(Sender: TObject);
    procedure sRadioButton6Click(Sender: TObject);
    procedure sRadioButton7Click(Sender: TObject);
    procedure sRadioButton8Click(Sender: TObject);
    procedure sRadioButton9Click(Sender: TObject);
    procedure sRadioButton10Click(Sender: TObject);
  public
    { Public declarations }
    Image : TacHintImage;
    procedure LoadImage(Img : TacHintImage; Repaint : boolean = True);
    procedure UnloadImage;
    function CreatePreviewBmp(const Width, Height: integer) : TBitmap;
    function CreateAlphaBmp : TBitmap;
    procedure UpdateData(Repaint : boolean = True);
    procedure RefreshPaintBox;
    procedure WndProc(var Message: TMessage); override;
  end;

var
  acUpdating : boolean = False;

implementation

uses acAlphaHintsEdit, sGraphUtils, sConst, sVCLUtils, acPNG, math, sSkinProvider;

{$R *.dfm}

{ TFrameHintPage }

procedure TFrameHintPage.LoadImage(Img: TacHintImage; Repaint : boolean = True);
begin
  Image := Img;
  UpdateData(Repaint);
end;

procedure TFrameHintPage.sTrackBar1Change(Sender: TObject); // Border left
begin
  if (Image = nil) then Exit;
  if not acUpdating then Image.BordersWidths.Left := TsTrackBar(Sender).Position;
  sLabel2.Caption := IntToStr(TsTrackBar(Sender).Position);
end;

procedure TFrameHintPage.sTrackBar2Change(Sender: TObject); // Client left
begin
  if (Image = nil) then Exit;
  if not acUpdating then Image.ClientMargins.Left := TsTrackBar(Sender).Position;
  sLabel4.Caption := IntToStr(sTrackBar2.Position);
end;

procedure TFrameHintPage.sTrackBar4Change(Sender: TObject); // Border top
begin
  if (Image = nil) then Exit;
  if not acUpdating then Image.BordersWidths.Top := TsTrackBar(Sender).Position;
  sLabel9.Caption := IntToStr(sTrackBar4.Position);
end;

procedure TFrameHintPage.sTrackBar3Change(Sender: TObject); // Client top
begin
  if (Image = nil) then Exit;
  if not acUpdating then Image.ClientMargins.Top := TsTrackBar(Sender).Position;
  sLabel10.Caption := IntToStr(sTrackBar3.Position);
end;

procedure TFrameHintPage.sTrackBar6Change(Sender: TObject); // Border right
begin
  if (Image = nil) then Exit;
  if not acUpdating then Image.BordersWidths.Right := TsTrackBar(Sender).Position;
  sLabel14.Caption := IntToStr(sTrackBar6.Position);
end;

procedure TFrameHintPage.sTrackBar5Change(Sender: TObject); // Client right
begin
  if (Image = nil) then Exit;
  if not acUpdating then Image.ClientMargins.Right := TsTrackBar(Sender).Position;
  sLabel15.Caption := IntToStr(sTrackBar5.Position);
end;

procedure TFrameHintPage.sTrackBar8Change(Sender: TObject); // Border bottom
begin
  if (Image = nil) then Exit;
  if not acUpdating then Image.BordersWidths.Bottom := TsTrackBar(Sender).Position;
  sLabel19.Caption := IntToStr(sTrackBar8.Position);
end;

procedure TFrameHintPage.sTrackBar7Change(Sender: TObject); // Client bottom
begin
  if (Image = nil) then Exit;
  if not acUpdating then Image.ClientMargins.Bottom := TsTrackBar(Sender).Position;
  sLabel20.Caption := IntToStr(sTrackBar7.Position);
end;

function TFrameHintPage.CreatePreviewBmp(const Width, Height: integer): TBitmap;
const
  SqSize = 8;
  SqColor = $cccccc;
var
  X, Y, dx, dy, i : integer;
  R : TRect;
begin
  Result := CreateBmp24(Width, Height);
  i := 0;
  X := 0;
  dx := Width div SqSize;
  dy := Height div SqSize;
  while X < dx do begin
    if i = 0 then i := 1 else i := 0;
    Y := i;
    while Y < dy do begin
      R.Left := X * SqSize;
      R.Top := Y * SqSize;
      R.Right := R.Left + SqSize;
      R.Bottom := R.Top + SqSize;
      FillDC(Result.Canvas.Handle, R, SqColor);
      inc(Y, 2);
    end;
    inc(X);
  end;
end;

procedure TFrameHintPage.PaintBox1Paint(Sender: TObject);
var
  SrcBmp : TBitmap;
  X, Y : integer;
  BorderColor : TsColor;
begin
  if PreviewBmp = nil then begin
    PreviewBmp := CreatePreviewBmp(PaintBox1.Width, PaintBox1.Height);
    if (CurrentTemplate <> nil) and not Image.Image.Empty then begin
      SrcBmp := CreateAlphaBmp;

      X := (PaintBox1.Width - SrcBmp.Width) div 2;
      Y := (PaintBox1.Height - SrcBmp.Height) div 2;

      CopyBmp32(Rect(X, Y, X + SrcBmp.Width, Y + SrcBmp.Height), Rect(0, 0, SrcBmp.Width, SrcBmp.Height), PreviewBmp, SrcBmp, EmptyCI, False, clNone, 0, False);
      BorderColor.C := 0;
      if sTrackBar1.Focused then FadeBmp(PreviewBmp, Rect(X, Y, X + Image.BordersWidths.Left, Y + SrcBmp.Height), 50, BorderColor, 0, 0);
      if sTrackBar4.Focused then FadeBmp(PreviewBmp, Rect(X, Y, X + SrcBmp.Width, Y + Image.BordersWidths.Top), 50, BorderColor, 0, 0);
      if sTrackBar6.Focused then FadeBmp(PreviewBmp, Rect(X + SrcBmp.Width - Image.BordersWidths.Right, Y, X + SrcBmp.Width, Y + SrcBmp.Height), 50, BorderColor, 0, 0);
      if sTrackBar8.Focused then FadeBmp(PreviewBmp, Rect(X, Y + SrcBmp.Height - Image.BordersWidths.Bottom, X + SrcBmp.Width, Y + SrcBmp.Height), 50, BorderColor, 0, 0);

      FreeAndNil(SrcBmp);
    end;
  end;
  BitBlt(PaintBox1.Canvas.Handle, 0, 0, PaintBox1.Width, PaintBox1.Height, PreviewBmp.Canvas.Handle, 0, 0, SRCCOPY);
end;

function TFrameHintPage.CreateAlphaBmp : TBitmap;
const
  PreviewText = 'Hint preview';
var
  TempBmp : TBitmap;
begin
  Result := CreateBmp32(0, 0);
  Result.Canvas.Font.Assign(CurrentTemplate.Font);
  Result.Width := Result.Canvas.TextWidth(PreviewText);
  Result.Height := Result.Canvas.TextHeight(PreviewText);
  Result.Width := max(Result.Width + Image.ClientMargins.Left + Image.ClientMargins.Right, Image.BordersWidths.Left + Image.BordersWidths.Right);
  Result.Height := max(Result.Height + Image.ClientMargins.Top + Image.ClientMargins.Bottom, Image.BordersWidths.Top + Image.BordersWidths.Bottom);

  PaintControlByTemplate(Result, Image.Image, Rect(0, 0, Result.Width, Result.Height), Rect(0, 0, Image.Image.Width, Image.Image.Height),
      Rect(Image.BordersWidths.Left, Image.BordersWidths.Top, Image.BordersWidths.Right, Image.BordersWidths.Bottom),
      Rect(ord(Image.BorderDrawModes.Left), ord(Image.BorderDrawModes.Top), ord(Image.BorderDrawModes.Right), ord(Image.BorderDrawModes.Bottom)), boolean(ord(Image.BorderDrawModes.Center)));

  TempBmp := TBitmap.Create;
  TempBmp.Assign(Result);
  Result.Canvas.Brush.Style := bsClear;
  Result.Canvas.TextOut(Image.ClientMargins.Left, Image.ClientMargins.Top, PreviewText);
  CopyChannel32(Result, TempBmp, 3);
  TempBmp.Free;
end;

procedure TFrameHintPage.UpdateData(Repaint : boolean = True);
var
  w, h : integer;
begin
  if acUpdating then Exit;
  acUpdating := True;
  if not (AlphaHintsEdit.ActiveControl is TsRadioButton) then begin
    if (Image <> nil) and not Image.Image.Empty then begin
      w := Image.ImageWidth;
      h := Image.ImageHeight;
      sLabel23.Caption := IntToStr(w);
      sLabel24.Caption := IntToStr(h);

{      if sTrackBar6.Position + sTrackBar1.Position > w then begin
        sTrackBar6.Position := 0;
        sTrackBar1.Position := 0;
      end;
      if sTrackBar2.Position + sTrackBar5.Position > w then begin
        sTrackBar2.Position := 0;
        sTrackBar5.Position := 0;
      end;}

      sTrackBar1.Max := w - 1;
      sTrackBar6.Max := sTrackBar1.Max;
      sTrackBar2.Max := sTrackBar1.Max;
      sTrackBar5.Max := sTrackBar1.Max;

{      if sTrackBar4.Position + sTrackBar8.Position > h then begin
        sTrackBar4.Position := 0;
        sTrackBar8.Position := 0;
      end;
      if sTrackBar3.Position + sTrackBar7.Position > h then begin
        sTrackBar3.Position := 0;
        sTrackBar7.Position := 0;
      end;}

      sTrackBar3.Max := h - 1;
      sTrackBar4.Max := sTrackBar3.Max;
      sTrackBar7.Max := sTrackBar3.Max;
      sTrackBar8.Max := sTrackBar3.Max;

      if Image.BordersWidths.Left + Image.BordersWidths.Top + Image.BordersWidths.Right + Image.BordersWidths.Bottom = 0 then begin // First init
        Image.BordersWidths.Left := w div 2;
        Image.BordersWidths.Right := Image.BordersWidths.Left;
        Image.BordersWidths.Top := h div 2;
        Image.BordersWidths.Bottom := Image.BordersWidths.Top;
      end;
      sTrackBar1.Position := Image.BordersWidths.Left;
      sTrackBar4.Position := Image.BordersWidths.Top;
      sTrackBar6.Position := Image.BordersWidths.Right;
      sTrackBar8.Position := Image.BordersWidths.Bottom;

      if Image.BorderDrawModes.Left = dmRepeat then sRadioButton1.Checked := True else sRadioButton2.Checked := True;
      if Image.BorderDrawModes.Top = dmRepeat then sRadioButton3.Checked := True else sRadioButton4.Checked := True;
      if Image.BorderDrawModes.Right = dmRepeat then sRadioButton5.Checked := True else sRadioButton6.Checked := True;
      if Image.BorderDrawModes.Bottom = dmRepeat then sRadioButton7.Checked := True else sRadioButton8.Checked := True;
      if Image.BorderDrawModes.Center = dmRepeat then sRadioButton9.Checked := True else sRadioButton10.Checked := True;

      if Image.ClientMargins.Left + Image.ClientMargins.Top + Image.ClientMargins.Right + Image.ClientMargins.Bottom = 0 then begin // First init
        Image.ClientMargins.Left := w div 2;
        Image.ClientMargins.Right := Image.ClientMargins.Left;
        Image.ClientMargins.Top := h div 2;
        Image.ClientMargins.Bottom := Image.ClientMargins.Top;
      end;
      sTrackBar2.Position := Image.ClientMargins.Left;
      sTrackBar3.Position := Image.ClientMargins.Top;
      sTrackBar5.Position := Image.ClientMargins.Right;
      sTrackBar7.Position := Image.ClientMargins.Bottom;

      if AlphaHintsEdit.FrameTL = Self then begin
        ChangeStates(Self, 1, True);
      end;
//      if AlphaHintsEdit.CurrentFrame = Self then begin
        ChangeStates(Self, 2, not Image.Image.Empty);
//      end;
    end
    else begin
      sLabel23.Caption := '0';
      sLabel24.Caption := '0';
      sTrackBar1.Max := 0;
      sTrackBar6.Max := sTrackBar1.Max;
      sTrackBar2.Max := sTrackBar1.Max;
      sTrackBar5.Max := sTrackBar1.Max;

      sTrackBar3.Max := 0;
      sTrackBar4.Max := sTrackBar3.Max;
      sTrackBar7.Max := sTrackBar3.Max;
      sTrackBar8.Max := sTrackBar3.Max;

      sTrackBar1.Position := 0;
      sTrackBar4.Position := 0;
      sTrackBar6.Position := 0;
      sTrackBar8.Position := 0;

      sTrackBar2.Position := 0;
      sTrackBar3.Position := 0;
      sTrackBar5.Position := 0;
      sTrackBar7.Position := 0;

      if AlphaHintsEdit.FrameTL = Self then begin
//        ChangeStates(AlphaHintsEdit, 1, False); ???
      end;
//      if AlphaHintsEdit.CurrentFrame = Self then begin
        ChangeStates(AlphaHintsEdit, 2, False);
//      end;
    end;
  end;
  if Repaint and not InAnimationProcess then begin
    RefreshPaintBox;
  end;
  acUpdating := False;
end;

procedure TFrameHintPage.sSpeedButton3Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then begin
    Image.FImage.Free;
    Image.FImage := nil;
    Image.FImage := TPNGGraphic.Create;
//    Image.FImage.Assign(nil);

    Image.Image.LoadFromFile(OpenPictureDialog1.FileName);
    Image.ImageChanged;
    UpdateData;
  end;
end;

procedure TFrameHintPage.WndProc(var Message: TMessage);
begin
  inherited;
  if (Message.Msg = CM_FOCUSCHANGED) and not InAnimationProcess and not AeroIsEnabled and (PaintBox1 <> nil) and sFrameAdapter1.SkinData.Skinned then RefreshPaintBox;
end;

procedure TFrameHintPage.sRadioButton1Click(Sender: TObject);
begin
  if (Image = nil) then Exit;
  Image.BorderDrawModes.Left := dmRepeat;
end;

procedure TFrameHintPage.sRadioButton2Click(Sender: TObject);
begin
  if (Image = nil) then Exit;
  Image.BorderDrawModes.Left := dmStretch;
end;

procedure TFrameHintPage.sRadioButton3Click(Sender: TObject);
begin
  if (Image = nil) then Exit;
  Image.BorderDrawModes.Top := dmRepeat;
end;

procedure TFrameHintPage.sRadioButton4Click(Sender: TObject);
begin
  if (Image = nil) then Exit;
  Image.BorderDrawModes.Top := dmStretch;
end;

procedure TFrameHintPage.sRadioButton5Click(Sender: TObject);
begin
  if (Image = nil) then Exit;
  Image.BorderDrawModes.Right := dmRepeat;
end;

procedure TFrameHintPage.sRadioButton6Click(Sender: TObject);
begin
  if (Image = nil) then Exit;
  Image.BorderDrawModes.Right := dmStretch;
end;

procedure TFrameHintPage.sRadioButton7Click(Sender: TObject);
begin
  if (Image = nil) then Exit;
  Image.BorderDrawModes.Bottom := dmRepeat;
end;

procedure TFrameHintPage.sRadioButton8Click(Sender: TObject);
begin
  if (Image = nil) then Exit;
  Image.BorderDrawModes.Bottom := dmStretch;
end;

procedure TFrameHintPage.sRadioButton9Click(Sender: TObject);
begin
  if (Image = nil) then Exit;
  Image.BorderDrawModes.Center := dmRepeat;
end;

procedure TFrameHintPage.sRadioButton10Click(Sender: TObject);
begin
  if (Image = nil) then Exit;
  Image.BorderDrawModes.Center := dmStretch;
end;

procedure TFrameHintPage.UnloadImage;
begin
  if Image <> nil then begin
    Image.Image.Assign(nil);
  end;
end;

procedure TFrameHintPage.RefreshPaintBox;
begin
  FreeAndNil(PreviewBmp); // Update bitmap
  PaintBox1.Perform(WM_PAINT, Longint(PaintBox1.Canvas.Handle), 0);
end;

end.
