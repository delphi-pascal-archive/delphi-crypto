{******************************************************************************}
{*  Hex.int.inc                                                               *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ функции для работы с 16-ричными данными }
type
    Hex = String;

const
{$IFDEF HEX_UPPER_CASE}
    HexChars : array [0..15] of Char = (
        '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'
    );
    HexSymbols : ShortString = '0123456789ABCDEF';
{$ELSE}
    HexChars : array [0..15] of Char = (
        '0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'
    );
    HexSymbols : ShortString = '0123456789abcdef';
{$ENDIF}

function IntToHex (Int: Int64; IntSize: Byte) : Hex;
function FloatToHex (const aValue: Extended; Size: Byte) : Hex;
function StrToHex (const aValue: String) : Hex;
function DateTimeToHex (const aValue: TDateTime) : Hex;
function VersionInfoToHex (const aValue: TVersionInfo) : Hex;

function HexToInt (const aValue: Hex) : Int64;
function HexToFloat (const aValue: Hex) : Extended;
function HexToStr (const aValue: Hex) : String;
function HexToDateTime (const aValue: Hex) : TDateTime;
function HexToVersionInfo (const aValue: Hex) : TVersionInfo;

function IsHex (const aValue: String) : Boolean;

procedure StreamToHexStream (const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: WORD = 1024);
procedure HexStreamToStream (const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: WORD = 1024);
procedure ByteStreamToHexStream (const anInput: TStream;
                                 out anOutput: TStream);
procedure HexStreamToByteStream (const anInput: TStream;
                                 out anOutput: TStream);
function IsHexStream (const aStream: TStream;
                      const aBlockSize: WORD = 1024) : Boolean;

