unit sHtmlParse;
{$I sDefs.inc}

interface
uses windows, Graphics, SysUtils, Classes, acntUtils, Math, sConst;

const
  Letters = ['A','B','C','D','E','F'];
  Digits = ['0','1','2','3','4','5','6','7','8','9'];
  DisabledChars = [#13, #10];

type
  TsHtml = class(TObject)
  protected
  public
    Bitmap : TBitmap;

    CurX, CurY, Len : integer;
    Origin, UppedText : string;
    aFonts : array of TFont;
    Area : TRect;

    MaxBmpWidth : integer;
    MaxBmpHeight : integer;
    CurWidthValue : integer;
    CurHeightValue : integer;
    CurrentRowHeight : integer;
    CurrentRowAlignment : TAlignment;
    destructor Destroy; override;
    procedure ShowCut(i2 : integer; var i1 : integer);
    procedure Init(Bmp : TBitmap; Text : string; aRect : TRect);
    procedure ExecTag(const s : string);
    procedure NewRow;
    procedure NewFont(const s : string);
    procedure BackFont;
    function HTMLText : TRect;
    procedure SetAlignment(const Tag : string);
  end;

function GetTag(const s: string; CurPos : integer) : string;
procedure SetFont(Font: TFont; const Tag : string);
//function HexToInt(const HexStr : string) : integer;

implementation

uses sGraphUtils;

function GetTag(const s: string; CurPos : integer) : string;
var
  i, l, j : integer;
//  sTag : string;
begin
  Result := '';
  l := length(s);
  for i := CurPos to l do begin
    if s[i] = '>' then begin
      for j := CurPos to i do begin
        Result := Result + s[j];
      end;
      break;
    end;
  end;
end;

procedure SetFont(Font: TFont; const Tag : string);
const
  Delims = [' ', '=', '"', #13, #10, '<', '>'];
var
  i, count, len, j1, j2 : integer;
  Atom, Value, Buffer : string;
//  Buffer: PChar;
begin

  count := WordCount(Tag, Delims);
  len := length(tag);
  for i := 1 to count do begin
    Atom := ExtractWord(i, Tag, Delims);
    if Atom = 'SIZE' then begin
      Value := ExtractWord(i + 1, Tag, Delims);
      if Value <> '' then begin
        if Value[1] = '-' then begin
          Delete(Value, 1, 1);
          if Value <> '' then Font.Size := Font.Size - StrToInt(Value);
        end
        else if Value[1] = '+' then begin
          Delete(Value, 1, 1);
          if Value <> '' then Font.Size := Font.Size + StrToInt(Value);
        end
        else begin
          try
            Font.Size := StrToInt(Value);
          finally
          end;
        end;
      end;
    end
    else if Atom = 'COLOR' then begin
      Value := ExtractWord(i + 1, Tag, Delims);
      if Value <> '' then begin
        if Value[1] = '#' then begin
          Delete(Value, 1, 1);
          Buffer := '';
          try
            Font.Color := SwapColor(HexToInt(Value));
          finally
          end;
        end else
        if Value = 'AQUA' then Font.Color := clAqua else
        if Value = 'BLACK' then Font.Color := clBlack else
        if Value = 'BLUE' then Font.Color := clBlue else
        if Value = 'FUCHSIA' then Font.Color := clFuchsia else
        if Value = 'GRAY' then Font.Color := clGray else
        if Value = 'GREEN' then Font.Color := clGreen else
        if Value = 'LIME' then Font.Color := clLime else
        if Value = 'MAROON' then Font.Color := clMaroon else
        if Value = 'NAVY' then Font.Color := clNavy else
        if Value = 'OLIVE' then Font.Color := clOlive else
        if Value = 'PURPLE' then Font.Color := clPurple else
        if Value = 'RED' then Font.Color := clRed else
        if Value = 'SILVER' then Font.Color := clSilver else
        if Value = 'TEAL' then Font.Color := clTeal else
        if Value = 'WHITE' then Font.Color := clWhite else
        if Value = 'YELLOW' then Font.Color := clYellow
        else begin
          Buffer := '';
          try
            Font.Color := SwapColor(HexToInt(Value));
          finally
          end;
        end;
      end;
    end
    else if Atom = 'FACE' then begin
      j1 := pos(Atom, Tag);
      while (Tag[j1] <> '"') and (j1 < len) do begin
        inc(j1);
      end;
      j2 := j1 + 1;
      while (Tag[j2] <> '"') and (j2 < len) do begin
        inc(j2);
      end;
      if j2 > j1 then begin
        Value := '';
        inc(j1);
        while j1 < j2 do begin
          Value := Value + Tag[j1];
          inc(j1);
        end;
        if Value <> '' then begin
          Font.Name := Value;
        end;
      end;
    end;
  end;
end;
(*
function HexToInt(const HexStr : string) : integer;
var
  len, i{, Val} : integer;
  c : char;
  function GetVal(i : integer) : integer;
  begin
    c := HexStr[i];
    if (c in Letters) then begin
      Result := Ord(c) - 55;
    end
    else Result := StrToInt(c);

  end;
begin
  try

  Result := 0;
  len := Length(HexStr);
  for i := len downto 1 do begin
    if (HexStr[i] in Letters) or (HexStr[i] in Digits) then begin
      Result := Round(Result + GetVal(i) * power(16, (len - i)));
    end
    else begin
      Result := -1;
      Break;
    end;
  end;
  except
    Result := -1;
  end;
end;
*)
{ TsHtml }

procedure TsHtml.BackFont;
var
  len : integer;
begin
  len := High(aFonts);
  if len <> -1 then begin
    Bitmap.Canvas.Font.Assign(aFonts[len]);
    if Assigned(aFonts[len]) then FreeAndNil(aFonts[len]);
    SetLength(aFonts, len);
  end;
end;

destructor TsHtml.Destroy;
var
  len{, i} : integer;
begin
  Bitmap := nil;
  len := High(aFonts);
  while Len >= 0 do begin
    if Assigned(aFonts[len]) then FreeAndNil(aFonts[len]);
    SetLength(aFonts, len);
    len := High(aFonts);
  end;
  inherited;
end;

procedure TsHtml.ExecTag(const s: string);
  function OpenTag(const Tag : string) : boolean; begin
    Result := pos(Tag, s) = 2;
  end;
  function CloseTag(const Tag : string) : boolean; begin
    Result := pos(Tag, s) = 3;
  end;
begin
  if OpenTag('/') then begin
    if CloseTag('B') then begin
      Bitmap.Canvas.Font.Style := Bitmap.Canvas.Font.Style - [fsBold];
    end else
    if CloseTag('I') then begin
      Bitmap.Canvas.Font.Style := Bitmap.Canvas.Font.Style - [fsItalic];
    end else
    if CloseTag('FONT') then begin
      BackFont;
    end;
  end else
  if OpenTag('BR') then begin
    CurX := Area.Left;
    NewRow;
  end else
  if OpenTag('B') then begin
    Bitmap.Canvas.Font.Style := Bitmap.Canvas.Font.Style + [fsBold];
  end else
  if OpenTag('I') then begin
    Bitmap.Canvas.Font.Style := Bitmap.Canvas.Font.Style + [fsItalic];
  end else
  if OpenTag('P') then begin
    CurX := Area.Left;
    NewRow;
    SetAlignment(s);
  end else
  if OpenTag('FONT') then begin
    NewFont(s);
  end;
end;

function TsHtml.HTMLText : TRect;
var
  CurPos, LastPos{, l} : integer;
  sCurrentTag : string;
//  sHTML : TsHtml;
begin
  Result := Rect(0, 0, 100, 0);

  try

  LastPos := 1;
  CurPos := 1;
  while CurPos <= Len do begin
    case UppedText[CurPos] of
      '<' : begin
        if UppedText[CurPos + 1] <> '<' then begin
          if CurPos > LastPos then begin
            ShowCut(CurPos, LastPos);
          end;
          sCurrentTag := GetTag(UppedText, CurPos);
          if sCurrentTag = '' then Exit;
          ExecTag(sCurrentTag);
          inc(CurPos, Length(sCurrentTag));
          LastPos := CurPos;
          dec(CurPos);
        end
        else begin
          if CurPos > LastPos then begin
            ShowCut(CurPos, LastPos);
          end;
          inc(CurPos);
          LastPos := CurPos;
        end;
      end;
      else begin
      end;
    end;
    inc(CurPos);
  end;
  if CurPos > LastPos then begin
    ShowCut(CurPos, LastPos);
  end;

  if CurWidthValue > MaxBmpWidth then MaxBmpWidth := CurWidthValue;
  CurWidthValue := 0;
  MaxBmpHeight := CurY + CurrentRowHeight;

//  aBmp.Canvas.CopyRect(Rect(0, 0, sHTML.MaxBmpWidth + sHTML.Margin, sHTML.MaxBmpHeight + sHTML.Margin), sHTML.Bitmap.Canvas, Rect(0, 0, sHTML.MaxBmpWidth + sHTML.Margin, sHTML.MaxBmpHeight + sHTML.Margin));
  finally
    Result := Rect(0, 0, MaxBmpWidth, MaxBmpHeight);
  end;
end;

procedure TsHtml.Init(Bmp : TBitmap; Text: string; aRect : TRect);
begin
  Bitmap := Bmp;
  Origin := Text;
  Area := aRect;
  UppedText := AnsiUpperCase(Origin);

  Len := Length(UppedText);
  CurX := Area.Left;
  CurY := Area.Top;

  MaxBmpWidth := 0;
  MaxBmpHeight := 0;
  CurWidthValue := 0;
  CurHeightValue := 0;
  CurrentRowAlignment := taLeftJustify;
end;

procedure TsHtml.NewFont(const s: string);
var
  len : integer;
begin
  len := High(aFonts);
  inc(len, 2);
  SetLength(aFonts, len);
  aFonts[len - 1] := TFont.Create;
  aFonts[len - 1].Assign(Bitmap.Canvas.Font);
  SetFont(Bitmap.Canvas.Font, s);
end;

procedure TsHtml.NewRow;
begin
  CurY := CurY + CurrentRowHeight + 2;
  if CurWidthValue > MaxBmpWidth then MaxBmpWidth := CurWidthValue;
  CurWidthValue := 0;
  CurrentRowHeight := 0;
end;

procedure TsHtml.SetAlignment(const Tag: string);
const
  Delims = [' ', '=', '"', #13, #10, '<', '>'];
var
  i, count{, len, j1, j2} : integer;
  Atom, Value{, Buffer} : string;
begin
  count := WordCount(Tag, Delims);
//  len := length(tag);
  for i := 1 to count do begin
    Atom := ExtractWord(i, Tag, Delims);
    if Atom = 'ALIGN' then begin
      Value := ExtractWord(i + 1, Tag, Delims);
      if Value <> '' then begin
        if Value = 'CENTER' then CurrentRowAlignment := taCenter;
        if Value = 'LEFT' then CurrentRowAlignment := taLeftJustify;
        if Value = 'RIGHT' then CurrentRowAlignment := taRightJustify;
      end;
    end;
  end;
end;

procedure TsHtml.ShowCut(i2: integer; var i1: integer);
var
  c : string;
  i : integer;
begin
  c := '';
  for i := i1 to i2 - 1 do begin
    if not (Origin[i] in DisabledChars) then begin
      c := c + Origin[i];
    end;
  end;
  if c <> '' then begin
    Bitmap.Canvas.TextOut(CurX, CurY, c);
    CurX := CurX + Bitmap.Canvas.TextWidth(c);
    CurWidthValue := CurX;
    if Bitmap.Canvas.TextHeight('X') > CurrentRowHeight then CurrentRowHeight := Bitmap.Canvas.TextHeight('X');
  end;
  i1 := i2;
end;

end.
