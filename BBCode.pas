unit BBCode;
{******************************************************************************}
{*  BB Code Unit                                                              *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2010-2012                                        *}
{******************************************************************************}
interface

uses
    Windows, SysUtils, Classes, Graphics,
    ComCtrls, RichEdit, RxRichEd,
    acPNG, jpeg, ImgList, acAlphaImageList,
    Strings;

const
    QUOTES : array [0..1] of String = ( '[QUOTE]', '[/QUOTE]' );

function ColorToHex (const aColor: TColor) : String;
procedure InsertBBCode (anObject: TRichEdit; const aBBCode: String); overload;
procedure InsertBBCode (anObject: TRxRichEdit; const aBBCode: String); overload;
procedure InsertBitMap (anObject: TRxRichEdit; aBitMap: TBitMap);
function StrToBitMap (anObject: TRxRichEdit; aStr: String; aBitMap: TBitMap) : Integer;
procedure MarkQuotes (var aBBCode: String; const aColor: TColor = clNone);
procedure InsertQuotes (anObject: TRxRichEdit;
                        anIcons : TsAlphaImageList;
                        aBackColor: TColor = clNone);
procedure InsertSmiles (anObject: TRxRichEdit;
                        const aSMILES: array of String;
                        anIcons : TsAlphaImageList;
                        aBackColor: TColor = clNone);
procedure GetTagWords (const aBBCode: String;
                       const aTagOpen: String;
                       const aTagClose: String;
                       out aWords: TStringList);
procedure GetBoldWords (const aBBCode: String;
                        out aWords: TStringList);
procedure GetItalicWords (const aBBCode: String;
                          out aWords: TStringList);
procedure GetUnderlineWords (const aBBCode: String;
                             out aWords: TStringList);
procedure GetKeyWords (const aBBCode: String;
                       out aWords: TStringList);

resourcestring
    ERR_BBCODE_NOT_INITIALIZE_OBJECT = 'Объект класса ''%s'' не инициализирован!';
    ERR_BBCODE_INSERT                = 'Ошибка инъекции данных!';
    ERR_BBCODE_BMP                   = 'Ошибка преобразования изображения в RTF-формат!';
    ERR_SMILES_INSERT                = 'Ошибка замены смайлов на изображения!';

implementation

type
    TEditStreamCallBack = function (dwCookie: LongInt;
                                    pbBuff: PByte;
                                    cb: LongInt;
                                    var pcb: LongInt) : DWORD; stdcall;
    TEditStreamData = packed record
        dwCookie    : LongInt;
        dwError     : LongInt;
        pfnCallback : TEditStreamCallBack;
    end;

function EditStreamInCallback (dwCookie: Longint;
                               pbBuff: PByte;
                               cb: Longint;
                               var pcb: Longint) : DWORD; stdcall;
var
    Stream    : TStream;
    dataAvail : LongInt;
begin
    Result := UINT (E_FAIL);
    try
        Stream := TStream (dwCookie);
        if Assigned (Stream) then
        with Stream do
        begin
            dataAvail := Size - Position;
            Result := 0;
            if ( dataAvail <= cb ) then
            begin
                pcb := Read (pbBuff^,dataAvail);
                if ( pcb <> dataAvail ) then
                    Result := UINT (E_FAIL);
            end
            else
            begin
                pcb := Read (pbBuff^,cb);
                if ( pcb <> cb ) then
                    Result := UINT (E_FAIL);
            end;
        end;
    except
        Result := UINT (E_FAIL);
    end;
end;

procedure PutRTFSelection (anObject: TRichEdit; aSourceStream: TStream); overload;
var
    Data : TEditStreamData;
begin
    try
        if ( not Assigned (anObject) ) then
            raise Exception.CreateFmt (ERR_BBCODE_NOT_INITIALIZE_OBJECT,
                                       [TRichEdit.ClassName]);
        with Data do
        begin
            dwCookie := LongInt (aSourceStream);
            dwError := 0;
            pfnCallback := EditStreamInCallBack;
        end;
        anObject.Perform ( EM_STREAMIN, SF_RTF or SFF_SELECTION, LongInt (@Data) );
    except on E: Exception do
        raise Exception.CreateFmt ('%s'#13#10'%s',[ERR_BBCODE_INSERT,E.Message]);
    end;
end;

procedure PutRTFSelection (anObject: TRxRichEdit; aSourceStream: TStream); overload;
var
    Data : TEditStreamData;
begin
    try
        if ( not Assigned (anObject) ) then
            raise Exception.CreateFmt (ERR_BBCODE_NOT_INITIALIZE_OBJECT,
                                       [TRxRichEdit.ClassName]);
        with Data do
        begin
            dwCookie := LongInt (aSourceStream);
            dwError := 0;
            pfnCallback := EditStreamInCallBack;
        end;
        anObject.Perform ( EM_STREAMIN, SF_RTF or SFF_SELECTION, LongInt (@Data) );
    except on E: Exception do
        raise Exception.CreateFmt ('%s'#13#10'%s',[ERR_BBCODE_INSERT,E.Message]);
    end;
end;

procedure ColorToRGB (const aColor: TColor;
                      var R: Byte;
                      var G: Byte;
                      var B: Byte);
var
    clr : LongInt;
begin
    clr := Graphics.ColorToRGB (aColor);
    R := clr;
    G := clr shr 8;
    B := clr shr 16;
end;

function ColorToHex (const aColor: TColor) : String;
var
    R : Byte;
    G : Byte;
    B : Byte;
begin
    Result := '000000';
    try
        ColorToRGB (aColor,R,G,B);
        Result := Format ('%s%s%s',[ IntToHex (R,2), IntToHex (G,2), IntToHex (B,2) ]);
    except
        Result := '000000';
    end;
end;

function GetColors (var aBBCode: String; var aPallete: WORD;
                    const aFontColor: TColor = clBlack) : String;
var
    Buf1   : String;
    Buf2   : String;
    Buf3   : String;
    Buffer : String;
    color  : String;
    R      : Byte;
    G      : Byte;
    B      : Byte;
begin
    Result := '';
    Buf1 := '';
    Buf2 := aBBCode;
    Buf3 := '';
    Buffer := aBBCode;
    R := 0;
    G := 0;
    B := 0;
    // цвет шрифта по-умолчанию
    ColorToRGB (aFontColor,R,G,B);
    Result := Format ('\red%d\green%d\blue%d;',[R,G,B]);
    Inc (aPallete);
    // разбираем палитру
    while ( Pos ('[COLOR:#',Buf2) > 0 ) do
    begin
        Buf2 := Copy (  Buf2, Pos ('[COLOR:#', Buf2), ( Length (Buf2) - Pos ('[COLOR:#', Buf2) ) + 1  );
        Buf1 := Copy  ( Buffer, 1, Length (Buffer) - Length (Buf2) );
        color := Copy  ( Buf2, Pos ('[COLOR:#', Buf2) + Length ('[COLOR:#'), Length ('RRGGBB') );
        R := StrToInt ( Format ('$%s',[Copy (color,1,2)]) );
        G := StrToInt ( Format ('$%s',[Copy (color,3,2)]) );
        B := StrToInt ( Format ('$%s',[Copy (color,5,2)]) );
        Result := Format ('%s\red%d\green%d\blue%d;',[Result,R,G,B]);
        Inc (aPallete);
        Delete ( Buf2, Pos ('[COLOR:#', Buf2), Length ('[COLOR:#RRGGBB]') );
        Buf3 := Copy (  Buf2, 1, ( Pos ('[/COLOR]', Buf2) - 1 )  );
        Buf2 := Copy (  Buf2, Pos ('[/COLOR]', Buf2) + Length ('[/COLOR]'), ( Length (Buf2) - Pos ('[/COLOR]', Buf2) ) + 1  );
        // возвращаемся к цвету шрифта 1 - начальный цвет шрифта
        Buffer := Format ('%s\cf%d %s\cf1 %s',[Buf1,aPallete,Buf3,Buf2]);
    end;
    Buffer := Format ('\cf1 %s\cf1',[Buffer]);
    aBBCode := Buffer;
end;

function GetBackgrounds (var aBBCode: String; var aPallete: WORD;
                         const aColor: TColor = clWhite) : String;
var
    Buf1   : String;
    Buf2   : String;
    Buf3   : String;
    Buffer : String;
    color  : String;
    R      : Byte;
    G      : Byte;
    B      : Byte;
begin
    Result := '';
    Buf1 := '';
    Buf2 := aBBCode;
    Buf3 := '';
    Buffer := aBBCode;
    R := 0;
    G := 0;
    B := 0;
    // цвет фона по-умолчанию
    ColorToRGB (aColor,R,G,B);
    Result := Format ('\red%d\green%d\blue%d;',[R,G,B]);
    Inc (aPallete);
    // разбираем палитру
    while ( Pos ('[BACKGROUND:#',Buf2) > 0 ) do
    begin
        Buf2 := Copy (  Buf2, Pos ('[BACKGROUND:#', Buf2), ( Length (Buf2) - Pos ('[BACKGROUND:#', Buf2) ) + 1  );
        Buf1 := Copy  ( Buffer, 1, Length (Buffer) - Length (Buf2) );
        color := Copy  ( Buf2, Pos ('[BACKGROUND:#', Buf2) + Length ('[BACKGROUND:#'), Length ('RRGGBB') );
        R := StrToInt ( Format ('$%s',[Copy (color,1,2)]) );
        G := StrToInt ( Format ('$%s',[Copy (color,3,2)]) );
        B := StrToInt ( Format ('$%s',[Copy (color,5,2)]) );
        Result := Format ('%s\red%d\green%d\blue%d;',[Result,R,G,B]);
        Inc (aPallete);
        Delete ( Buf2, Pos ('[BACKGROUND:#', Buf2), Length ('[BACKGROUND:#RRGGBB]') );
        Buf3 := Copy (  Buf2, 1, ( Pos ('[/BACKGROUND]', Buf2) - 1 )  );
        Buf2 := Copy (  Buf2, Pos ('[/BACKGROUND]', Buf2) + Length ('[/BACKGROUND]'), ( Length (Buf2) - Pos ('[/BACKGROUND]', Buf2) ) + 1  );
        // возвращаемся к цвету фона 0 - нет фона
        Buffer := Format ('%s\highlight%d %s\highlight0 %s',[Buf1,aPallete,Buf3,Buf2]);
    end;
    aBBCode := Buffer;
end;

function GetFonts (var aBBCode: String; var aFontNumber: WORD;
                   const aFont: TFont) : String;
var
    Buf1   : String;
    Buf2   : String;
    Buf3   : String;
    Buffer : String;
    font   : String;
    size   : String;
begin
    Result := '';
    Buf1 := '';
    Buf2 := aBBCode;
    Buf3 := '';
    Buffer := aBBCode;
    // шрифт по-умолчанию
    Result := Format ('{\f%d\fswiss\fcharset1 %s;}',[aFontNumber,aFont.Name]);
    Inc (aFontNumber);
    // разбираем шрифты
    while ( Pos ('[FONT:',Buf2) > 0 ) do
    begin
        Buf2 := Copy (  Buf2, Pos ('[FONT:', Buf2), ( Length (Buf2) - Pos ('[FONT:', Buf2) ) + 1  );
        Buf1 := Copy  ( Buffer, 1, Length (Buffer) - Length (Buf2) );
        font := Copy  ( Buf2, Pos ('[FONT:', Buf2) + Length ('[FONT:'),
                              Pos (']', Buf2) - Pos ('[FONT:', Buf2) - Length ('[FONT:') );
        Result := Format ('%s{\f%d\fswiss\fcharset1 %s;}',[Result,aFontNumber,font]);
        Delete ( Buf2, Pos ('[FONT:', Buf2), Length ( Format ('[FONT:%s]',[font]) ) );
        Buf3 := Copy (  Buf2, 1, ( Pos ('[/FONT]', Buf2) - 1 )  );
        Buf2 := Copy (  Buf2, Pos ('[/FONT]', Buf2) + Length ('[/FONT]'), ( Length (Buf2) - Pos ('[/FONT]', Buf2) ) + 1  );
        Buffer := Format ('%s\f%d %s\f0 %s',[Buf1,aFontNumber,Buf3,Buf2]);
        Inc (aFontNumber);
    end;
    // размер шрифта по-умолчанию
    Buffer := Format ('\fs%d %s',[aFont.Size*2,Buffer]);
    // разбираем размеры
    while ( Pos ('[SIZE:',Buf2) > 0 ) do
    begin
        Buf2 := Copy (  Buf2, Pos ('[SIZE:', Buf2), ( Length (Buf2) - Pos ('[SIZE:', Buf2) ) + 1  );
        Buf1 := Copy  ( Buffer, 1, Length (Buffer) - Length (Buf2) );
        size := Copy  ( Buf2, Pos ('[SIZE:', Buf2) + Length ('[SIZE:'),
                              Pos (']', Buf2) - Pos ('[SIZE:', Buf2) - Length ('[SIZE:') );
        Delete ( Buf2, Pos ('[SIZE:', Buf2), Length ( Format ('[SIZE:%s]',[size]) ) );
        Buf3 := Copy (  Buf2, 1, ( Pos ('[/SIZE]', Buf2) - 1 )  );
        Buf2 := Copy (  Buf2, Pos ('[/SIZE]', Buf2) + Length ('[/SIZE]'), ( Length (Buf2) - Pos ('[/SIZE]', Buf2) ) + 1  );
        Buffer := Format ('%s\fs%d %s\fs%d %s',[ Buf1, StrToInt (Trim(size))*2, Buf3, aFont.Size*2, Buf2 ]);
    end;
    aBBCode := Buffer;
end;

procedure MarkQuotes (var aBBCode: String; const aColor: TColor = clNone);
var
    Buf1   : String;
    Buf2   : String;
    Buf3   : String;
    Buffer : String;
    Author : String;
begin
    aBBCode := StrReplace (aBBCode,'[quote','[QUOTE');
    aBBCode := StrReplace (aBBCode,'[/quote]','[/QUOTE]');
    Buf1 := '';
    Buf2 := aBBCode;
    Buf3 := '';
    Buffer := aBBCode;
    while ( Pos ('[QUOTE:',Buf2) > 0 ) do
    begin
        Buf2 := Copy (  Buf2, Pos ('[QUOTE:', Buf2), ( Length (Buf2) - Pos ('[QUOTE:', Buf2) ) + 1  );
        Buf1 := Copy  ( Buffer, 1, Length (Buffer) - Length (Buf2) );
        Author := Copy  ( Buf2, Pos ('[QUOTE:', Buf2) + Length ('[QUOTE:'),
                                Pos (']', Buf2) - Pos ('[QUOTE:', Buf2) - Length ('[QUOTE:') );

        Delete ( Buf2, Pos ('[QUOTE:', Buf2), Length ( Format ('[QUOTE:%s]',[Author]) ) );
        Buf3 := Copy (  Buf2, 1, ( Pos ('[/QUOTE]', Buf2) - 1 )  );
        Buf2 := Copy (  Buf2, Pos ('[/QUOTE]', Buf2) + Length ('[/QUOTE]'), ( Length (Buf2) - Pos ('[/FONT]', Buf2) ) + 1  );
        Buffer := Format ('%s [QUOTE][B]%s[/B] %s[/QUOTE] %s',[Buf1,Author,Buf3,Buf2]);
    end;
    if ( aColor <> clNone ) then
    begin
        Buffer := StrReplace ( Buffer, '[QUOTE]', Format ('[BACKGROUND:#%s][QUOTE]',[ ColorToHex (aColor) ]) );
        Buffer := StrReplace ( Buffer, '[/QUOTE]', '[/QUOTE][/BACKGROUND]' );
    end;
    aBBCode := Buffer;
end;

procedure GetLists (var aBBCode: String);
begin
    aBBCode := StrReplace (aBBCode,'[LI]','•  ');
    aBBCode := StrReplace (aBBCode,'[/LI]','');
end;

procedure InsertBBCode (anObject: TRichEdit; const aBBCode: String);
var
    Stream     : TStringStream;
    s          : String;
    charset    : String;
    fonts      : String;
    fonttable  : WORD;
    colors     : String;
    backgrouns : String;
    palette    : WORD;
begin
    s := aBBCode;
    s := StrReplace (s,'[li]','[LI]');
    s := StrReplace (s,'[/li]','[/LI]');
    s := StrReplace (s,'[B]','\b ',FALSE);
    s := StrReplace (s,'[/B]','\b0 ',FALSE);
    s := StrReplace (s,'[I]','\i ',FALSE);
    s := StrReplace (s,'[/I]','\i0 ',FALSE);
    s := StrReplace (s,'[U]','\ul ',FALSE);
    s := StrReplace (s,'[/U]','\ulnone ',FALSE);
    s := StrReplace (s,'[S]','\strike ',FALSE);
    s := StrReplace (s,'[/S]','\strike0 ',FALSE);
    s := StrReplace (s,#13#10,'\par ');
    s := StrReplace (s,'[color:#','[COLOR:#');
    s := StrReplace (s,'[/color]','[/COLOR]');
    s := StrReplace (s,'[background:#','[BACKGROUND:#');
    s := StrReplace (s,'[/background]','[/BACKGROUND]');
    s := StrReplace (s,'[font:','[FONT:');
    s := StrReplace (s,'[/font]','[/FONT]');
    s := StrReplace (s,'[size:','[SIZE:');
    s := StrReplace (s,'[/size]','[/SIZE]');
    s := StrReplace (s,'[KEY]','');
    s := StrReplace (s,'[/KEY]','');
    s := StrReplace (s,'[key]','');
    s := StrReplace (s,'[/key]','');
    charset := '';
    if ( anObject.Font.CharSet = RUSSIAN_CHARSET ) then
        charset := '\ansi\ansicpg1251';
    palette := 0;
    colors := GetColors (s,palette,anObject.Font.Color);
    backgrouns := GetBackGrounds (s,palette,anObject.Color);
    fonttable := 0;
    fonts := GetFonts (s,fonttable,anObject.Font);
    GetLists (s);
    Stream := TStringStream.Create ( Format ('{\rtf1'+'%s'+
                                             '{\fonttbl %s}'+
                                             '{\colortbl ;%s%s}'+
                                             '%s}',
                                             [charset,
                                              fonts,
                                              colors,backgrouns,
                                              s]) );
    if Assigned (Stream) then
    try
        PutRTFSelection (anObject,Stream);
    finally
        FreeAndNil (Stream);
    end;
end;

procedure InsertBBCode (anObject: TRxRichEdit; const aBBCode: String);
var
    Stream     : TStringStream;
    s          : String;
    charset    : String;
    fonts      : String;
    fonttable  : WORD;
    colors     : String;
    backgrouns : String;
    palette    : WORD;
begin
    s := aBBCode;
    s := StrReplace (s,'[li]','[LI]');
    s := StrReplace (s,'[/li]','[/LI]');
    s := StrReplace (s,'[B]','\b ',FALSE);
    s := StrReplace (s,'[/B]','\b0 ',FALSE);
    s := StrReplace (s,'[I]','\i ',FALSE);
    s := StrReplace (s,'[/I]','\i0 ',FALSE);
    s := StrReplace (s,'[U]','\ul ',FALSE);
    s := StrReplace (s,'[/U]','\ulnone ',FALSE);
    s := StrReplace (s,'[S]','\strike ',FALSE);
    s := StrReplace (s,'[/S]','\strike0 ',FALSE);
    s := StrReplace (s,#13#10,'\par ');
    s := StrReplace (s,'[color:#','[COLOR:#');
    s := StrReplace (s,'[/color]','[/COLOR]');
    s := StrReplace (s,'[background:#','[BACKGROUND:#');
    s := StrReplace (s,'[/background]','[/BACKGROUND]');
    s := StrReplace (s,'[font:','[FONT:');
    s := StrReplace (s,'[/font]','[/FONT]');
    s := StrReplace (s,'[size:','[SIZE:');
    s := StrReplace (s,'[/size]','[/SIZE]');
    s := StrReplace (s,'[KEY]','');
    s := StrReplace (s,'[/KEY]','');
    s := StrReplace (s,'[key]','');
    s := StrReplace (s,'[/key]','');
    charset := '';
    if ( anObject.Font.CharSet = RUSSIAN_CHARSET ) then
        charset := '\ansi\ansicpg1251';
    palette := 0;
    colors := GetColors (s,palette,anObject.Font.Color);
    backgrouns := GetBackGrounds (s,palette,anObject.Color);
    fonttable := 0;
    fonts := GetFonts (s,fonttable,anObject.Font);
    GetLists (s);
    Stream := TStringStream.Create ( Format ('{\rtf1'+'%s'+
                                             '{\fonttbl %s}'+
                                             '{\colortbl ;%s%s}'+
                                             '%s}',
                                             [charset,
                                              fonts,
                                              colors,backgrouns,
                                              s]) );
    if Assigned (Stream) then
    try
        PutRTFSelection (anObject,Stream);
    finally
        FreeAndNil (Stream);
    end;
end;

function BitMapToRTF (const aValue: TBitMap) : String;
var
    Header, Image         : String;
    HeaderSize, ImageSize : Cardinal;
    S                     : ShortString;
    HEX                   : String;
    I                     : Integer;
begin
    try
        GetDIBSizes (aValue.Handle, HeaderSize, ImageSize);
        SetLength (Header, HeaderSize);
        SetLength (Image, ImageSize);
        GetDIB ( aValue.Handle, aValue.Palette, PChar (Header)^, PChar (Image)^ );
        Result := '{\rtf1 {\pict\dibitmap ';
        SetLength (  HEX, ( Length (Header) + Length (Image) ) * 2  );
        I := 2;
        for HeaderSize := 1 to Length (Header) do
        begin
            s := Format ('%x',[ Integer ( Header [HeaderSize] ) ]);
            if Length (s) = 1 then
                s := '0' + s;
            HEX [I-1] := s [1];
            HEX [I] := s [2];
            Inc (I,2);
        end;
        for ImageSize := 1 to Length (Image) do
        begin
            s := Format ('%x',[ Integer (Image [ImageSize]) ]);
            if Length (s) = 1 then
                s := '0' + s;
            HEX [I-1] := s [1];
            HEX [I] := s [2];
            Inc (I,2);
        end;
        Result := Result + HEX + ' }}';
    except on E: Exception do
        raise Exception.CreateFmt ('%s#13#10%s',[ERR_BBCODE_BMP,E.Message]);
    end;
end;

procedure InsertBitMap (anObject: TRxRichEdit; aBitMap: TBitMap);
var
    Stream : TStringStream;
begin
    Stream := TStringStream.Create ( BitMapToRTF (aBitMap) );
    if Assigned (Stream) then
    try
        PutRTFSelection (anObject,Stream);
    finally
        if Assigned (Stream) then
            FreeAndNil (Stream);
    end;
end;

function StrToBitMap (anObject: TRxRichEdit; aStr: String; aBitMap: TBitMap) : Integer;
var
    FindPos    : LongInt;
    CurrentPos : LongInt;
begin
    if Assigned (anObject) then
    with anObject do
    repeat
        FindPos := FindText ( aStr, 0, Length (Text),[] );
        if ( FindPos >= 0 ) then
        begin
            Lines.BeginUpdate;
            SelStart := FindPos;
            SelLength := Length (aStr);
            SelText := '';
            CurrentPos := FindPos;
            InsertBitMap (anObject,aBitMap);
            SelStart := CurrentPos;
            Lines.EndUpdate;
        end;
    until ( FindPos < 0 );
end;

procedure InsertSmiles (anObject: TRxRichEdit;
                        const aSMILES: array of String;
                        anIcons : TsAlphaImageList;
                        aBackColor: TColor = clNone);
var
    Bmp  : TBitMap;
    Rect : TRect;
    I    : Integer;
begin
    try
        if Assigned (anObject) and Assigned (anIcons) then
        begin
            Bmp := TBitmap.Create;
            try
                Rect.Left := 0;
                Rect.Top := 0;
                Rect.Right := anIcons.Width;
                Rect.Bottom := anIcons.Height;
                for I := 0 to High (aSMILES) do
                begin
                    if ( I <= anIcons.Count -1 ) and anIcons.GetBitmap32 (I,Bmp) then
                    begin
                        if ( aBackColor = clNone ) then
                        begin
                            Bmp.Canvas.Brush.Color := anObject.Color;
                            Bmp.Canvas.Pen.Color := anObject.Color;
                        end
                        else
                        begin
                            Bmp.Canvas.Brush.Color := aBackColor;
                            Bmp.Canvas.Pen.Color := aBackColor;
                        end;
                        Bmp.Canvas.FillRect (Rect);
                        anIcons.Draw (Bmp.Canvas,0,0,I,dsTransparent,itImage);
                        StrToBitMap ( anObject, aSMILES [I], Bmp );
                    end;
                end;
            finally
                FreeAndNil (Bmp);
            end;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s'#13#10'%s',[ERR_SMILES_INSERT,E.Message])
    end;
end;

procedure InsertQuotes (anObject: TRxRichEdit;
                        anIcons : TsAlphaImageList;
                        aBackColor: TColor = clNone);
begin
    InsertSmiles (anObject,QUOTES,anIcons,aBackColor);
end;

procedure GetTagWords (const aBBCode: String;
                       const aTagOpen: String;
                       const aTagClose: String;
                       out aWords: TStringList);
var
    Buf1  : String;
    Buf2  : String;
    Buf3  : String;
    lst   : TStringList;
    I     : Integer;
    Index : Integer;
begin
    if not Assigned (aWords) then
        raise Exception.CreateFmt (ERR_BBCODE_NOT_INITIALIZE_OBJECT,
                                   [TStringList.ClassName]);
    Buf1 := '';
    Buf2 := aBBCode;
    Buf3 := '';
    // разбираем
    while ( Pos (aTagOpen,Buf2) > 0 ) do
    begin
        Buf2 := Copy (  Buf2, Pos (aTagOpen, Buf2), ( Length (Buf2) - Pos (aTagOpen, Buf2) ) + 1  );
        Buf1 := Copy  ( aBBCode, 1, Length (aBBCode) - Length (Buf2) );
        Delete ( Buf2, Pos (aTagOpen, Buf2), Length (aTagOpen) );
        Buf3 := Copy (  Buf2, 1, ( Pos (aTagClose, Buf2) - 1 )  );
        Buf2 := Copy (  Buf2, Pos (aTagClose, Buf2) + Length (aTagClose), ( Length (Buf2) - Pos (aTagClose, Buf2) ) + 1  );
        Buf3 := Trim (Buf3);
        lst := TStringList.Create;
        try
            lst.CommaText := Buf3;
            for I := 0 to lst.Count - 1 do
                if notEmpty (lst [I]) and not aWords.Find (lst [I],Index) then
                    aWords.Add (lst [I]);
        finally
            FreeAndNil (lst);
        end;
    end;
end;

procedure GetBoldWords (const aBBCode: String;
                        out aWords: TStringList);
begin
    GetTagWords (aBBCode,'[B]','[/B]',aWords);
end;

procedure GetItalicWords (const aBBCode: String;
                          out aWords: TStringList);
begin
    GetTagWords (aBBCode,'[I]','[/I]',aWords);
end;

procedure GetUnderlineWords (const aBBCode: String;
                             out aWords: TStringList);
begin
    GetTagWords (aBBCode,'[U]','[/U]',aWords);
end;

procedure GetKeyWords (const aBBCode: String;
                       out aWords: TStringList);
begin
    GetTagWords (aBBCode,'[KEY]','[/KEY]',aWords);
end;


end.
