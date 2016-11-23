{******************************************************************************}
{*  Shacal.int.inc                                                            *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{$IFDEF EXTERNAL_LINK}
type
    { 256-битный блок }
    PShacal256Block = ^TShacal256Block;
    TShacal256Block = packed array [0..31] of Byte;
    { расписание ключей }
    PShacalKeyArray = ^TSHACALKeyArray;
    TShacalKeyArray = packed array [0..63] of LongInt;
    { инкремент }
    PShacalIncProc = ^TShacalIncProc;
    TShacalIncProc = procedure (var ctr: TShacal256Block); stdcall;
    { контекст }
    PShacalCtx = ^TShacalCtx;
    TShacalCtx = packed record
        RK      : TShacalKeyArray; { round key array         }
        IV      : TShacal256Block; { IV or CTR               }
        buf     : TShacal256Block; { Work buffer             }
        bLen    : WORD;            { Bytes used in buf       }
        Flag    : WORD;            { Bit 1: Short block      }
        IncProc : TShacalIncProc;  { Increment proc CTR-Mode }
    end;
{$ELSE EXTERNAL_LINK}
type
    { 256-битный блок }
    PShacal256Block = ^TShacal256Block;
    TShacal256Block = TSCBlock;
    { расписание ключей }
    PShacalKeyArray = ^TSHACALKeyArray;
    TShacalKeyArray = packed array [0..63] of LongInt;
    { инкремент }
    PShacalIncProc = ^TShacalIncProc;
    TShacalIncProc = procedure (var ctr: TShacal256Block);
    { контекст }
    PShacalCtx = ^TShacalCtx;
    TShacalCtx = TSCContext;
{$ENDIF EXTERNAL_LINK}

{$IFDEF EXTERNAL_LINK}
{ shacal dll functions }
const
    SHACAL_DLL = 'shacal/sc_dll.dll';
{$IFDEF ECB}
function SC_ECB_Init (const Key;
                      KeyBits: WORD;
                      var ctx: TShacalCtx) : Integer; stdcall;
    external SHACAL_DLL name 'SC_ECB_Init';
function SC_ECB_Reset (var ctx: TShacalCtx) : Integer; stdcall;
    external SHACAL_DLL name 'SC_ECB_Reset';
function SC_ECB_Encrypt (ptp, ctp: Pointer;
                         iLen: LongInt;
                         var ctx: TShacalCtx) : Integer; stdcall;
    external SHACAL_DLL name 'SC_ECB_Encrypt';
function SC_ECB_Decrypt (ctp, ptp: Pointer;
                         iLen: LongInt;
                         var ctx: TShacalCtx) : Integer; stdcall;
    external SHACAL_DLL name 'SC_ECB_Decrypt';
{$ENDIF ECB}
{$IFDEF CBC}
function SC_CBC_Init (const Key;
                      KeyBits: WORD;
                      const IV: TShacal256Block;
                      var ctx: TShacalCtx) : Integer; stdcall;
    external SHACAL_DLL name 'SC_CBC_Init';
function SC_CBC_Reset (var IV: TShacal256Block;
                       var ctx: TShacalCtx) : Integer; stdcall;
    external SHACAL_DLL name 'SC_CBC_Reset';
function SC_CBC_Encrypt (ptp, ctp: Pointer;
                         iLen: LongInt;
                         var ctx: TShacalCtx) : Integer; stdcall;
    external SHACAL_DLL name 'SC_CBC_Encrypt';
function SC_CBC_Decrypt (ctp, ptp: Pointer;
                         iLen: LongInt;
                         var ctx: TShacalCtx) : Integer; stdcall;
    external SHACAL_DLL name 'SC_CBC_Decrypt';
{$ENDIF CBC}
{$IFDEF CFB}
function SC_CFB_Init (const Key;
                      KeyBits: WORD;
                      const IV: TShacal256Block;
                      var ctx: TShacalCtx) : Integer; stdcall;
    external SHACAL_DLL name 'SC_CFB_Init';
function SC_CFB_Reset (var IV: TShacal256Block;
                       var ctx: TShacalCtx) : Integer; stdcall;
    external SHACAL_DLL name 'SC_CFB_Reset';
function SC_CFB_Encrypt (ptp, ctp: Pointer;
                         iLen: LongInt;
                         var ctx: TShacalCtx) : Integer; stdcall;
    external SHACAL_DLL name 'SC_CFB_Encrypt';
function SC_CFB_Decrypt (ctp, ptp: Pointer;
                         iLen: LongInt;
                         var ctx: TShacalCtx) : Integer; stdcall;
    external SHACAL_DLL name 'SC_CFB_Decrypt';
{$ENDIF CFB}
{$IFDEF OFB}
function SC_OFB_Init (const Key;
                      KeyBits: WORD;
                      const IV: TShacal256Block;
                      var ctx: TShacalCtx) : Integer; stdcall;
    external SHACAL_DLL name 'SC_OFB_Init';
function SC_OFB_Reset (var IV: TShacal256Block;
                       var ctx: TShacalCtx) : Integer; stdcall;
    external SHACAL_DLL name 'SC_OFB_Reset';
function SC_OFB_Encrypt (ptp, ctp: Pointer;
                         iLen: LongInt;
                         var ctx: TShacalCtx) : Integer; stdcall;
    external SHACAL_DLL name 'SC_OFB_Encrypt';
function SC_OFB_Decrypt (ctp, ptp: Pointer;
                         iLen: LongInt;
                         var ctx: TShacalCtx) : Integer; stdcall;
    external SHACAL_DLL name 'SC_OFB_Decrypt';
{$ENDIF OFB}
function GET_SHACAL_DLL_VERSION : PAnsiChar; stdcall;
    external SHACAL_DLL name 'SC_DLL_Version';
{$ENDIF EXTERNAL_LINK}

function SHACAL_VERSION : String;

const
    NULL_SHACAL_256_BLOCK : TShacal256Block = (
        $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00
    );

function ShacalIV (anArgs: array of const) : TShacal256Block;
function HexToShacalIV (const aValue: Hex) : TShacal256Block;
function ShacalIVToHex (const aValue: TShacal256Block) : Hex;

function GetRandomShacalIV (aRandomFunc: TRandomFunction) : TShacal256Block;

{ не используется вектор инициализации }
{$IFDEF ECB}
procedure EncryptShacalECB (var aKey;
                            const aKeySize: WORD;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
procedure DecryptShacalECB (var aKey;
                            const aKeySize: WORD;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
function EncryptShacalECB (var aKey;
                           const aKeySize: WORD;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;
function DecryptShacalECB (var aKey;
                           const aKeySize: WORD;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;

procedure EncryptShacalECB (const aKey: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
procedure DecryptShacalECB (const aKey: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
function EncryptShacalECB (const aKey: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;
function DecryptShacalECB (const aKey: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;
{$ENDIF ECB}

{ используется псевдо-случайный вектор инициализации,
  который помещается в начало шифротекста }
{$IFDEF CBC}
procedure EncryptShacalCBC (var aKey;
                            const aKeySize: WORD;
                            aRandomFunc: TRandomFunction;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
procedure DecryptShacalCBC (var aKey;
                            const aKeySize: WORD;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
function EncryptShacalCBC (var aKey;
                           const aKeySize: WORD;
                           aRandomFunc: TRandomFunction;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;
function DecryptShacalCBC (var aKey;
                           const aKeySize: WORD;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;

procedure EncryptShacalCBC (const aKey: Hex;
                            aRandomFunc: TRandomFunction;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
procedure DecryptShacalCBC (const aKey: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
function EncryptShacalCBC (const aKey: Hex;
                           aRandomFunc: TRandomFunction;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;
function DecryptShacalCBC (const aKey: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;
{$ENDIF CBC}
{$IFDEF CFB}
procedure EncryptShacalCFB (var aKey;
                            const aKeySize: WORD;
                            aRandomFunc: TRandomFunction;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
procedure DecryptShacalCFB (var aKey;
                            const aKeySize: WORD;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
function EncryptShacalCFB (var aKey;
                           const aKeySize: WORD;
                           aRandomFunc: TRandomFunction;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;
function DecryptShacalCFB (var aKey;
                           const aKeySize: WORD;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;

procedure EncryptShacalCFB (const aKey: Hex;
                            aRandomFunc: TRandomFunction;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
procedure DecryptShacalCFB (const aKey: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
function EncryptShacalCFB (const aKey: Hex;
                           aRandomFunc: TRandomFunction;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;
function DecryptShacalCFB (const aKey: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;
{$ENDIF CFB}
{$IFDEF OFB}
procedure EncryptShacalOFB (var aKey;
                            const aKeySize: WORD;
                            aRandomFunc: TRandomFunction;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
procedure DecryptShacalOFB (var aKey;
                            const aKeySize: WORD;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
function EncryptShacalOFB (var aKey;
                           const aKeySize: WORD;
                           aRandomFunc: TRandomFunction;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;
function DecryptShacalOFB (var aKey;
                           const aKeySize: WORD;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;

procedure EncryptShacalOFB (const aKey: Hex;
                            aRandomFunc: TRandomFunction;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
procedure DecryptShacalOFB (const aKey: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
function EncryptShacalOFB (const aKey: Hex;
                           aRandomFunc: TRandomFunction;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;
function DecryptShacalOFB (const aKey: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;
{$ENDIF OFB}

{ используется внешний вектор инициализации,
  который необходимо пересылать отдельным каналом }
{$IFDEF CBC}
procedure EncryptShacalCBC (var aKey;
                            const aKeySize: WORD;
                            const anIV: TShacal256Block;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
procedure DecryptShacalCBC (var aKey;
                            const aKeySize: WORD;
                            const anIV: TShacal256Block;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
function EncryptShacalCBC (var aKey;
                           const aKeySize: WORD;
                           const anIV: TShacal256Block;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;
function DecryptShacalCBC (var aKey;
                           const aKeySize: WORD;
                           const anIV: TShacal256Block;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;

procedure EncryptShacalCBC (const aKey: Hex;
                            const anIV: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
procedure DecryptShacalCBC (const aKey: Hex;
                            const anIV: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
function EncryptShacalCBC (const aKey: Hex;
                           const anIV: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;
function DecryptShacalCBC (const aKey: Hex;
                           const anIV: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;
{$ENDIF CBC}
{$IFDEF CFB}
procedure EncryptShacalCFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TShacal256Block;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
procedure DecryptShacalCFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TShacal256Block;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
function EncryptShacalCFB (var aKey;
                           const aKeySize: WORD;
                           const anIV: TShacal256Block;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;
function DecryptShacalCFB (var aKey;
                           const aKeySize: WORD;
                           const anIV: TShacal256Block;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;

procedure EncryptShacalCFB (const aKey: Hex;
                            const anIV: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
procedure DecryptShacalCFB (const aKey: Hex;
                            const anIV: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
function EncryptShacalCFB (const aKey: Hex;
                           const anIV: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;
function DecryptShacalCFB (const aKey: Hex;
                           const anIV: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;
{$ENDIF CFB}
{$IFDEF OFB}
procedure EncryptShacalOFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TShacal256Block;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
procedure DecryptShacalOFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TShacal256Block;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
function EncryptShacalOFB (var aKey;
                           const aKeySize: WORD;
                           const anIV: TShacal256Block;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;
function DecryptShacalOFB (var aKey;
                           const aKeySize: WORD;
                           const anIV: TShacal256Block;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;

procedure EncryptShacalOFB (const aKey: Hex;
                            const anIV: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
procedure DecryptShacalOFB (const aKey: Hex;
                            const anIV: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32); overload;
function EncryptShacalOFB (const aKey: Hex;
                           const anIV: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;
function DecryptShacalOFB (const aKey: Hex;
                           const anIV: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String; overload;
{$ENDIF OFB}