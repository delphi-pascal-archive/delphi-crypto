{******************************************************************************}
{*  TwoFish.int.inc                                                           *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{$IFDEF EXTERNAL_LINK}
type
    { 128-битный блок }
    PTwoFish128Block = ^TTwoFish128Block;
    TTwoFish128Block = packed array [0..15] of Byte;
    { s-box }
    PTwoFishSBox = ^TTwoFishSBox;
    TTwoFishSBox = packed array [0..255] of LongInt;
    { расписание ключей }
    PTwoFishRndKey = ^TTwoFishRndKey;
    TTwoFishRndKey = packed array [0..39] of LongInt;
    { инкремент }
    PTwoFishIncProc = ^TTwoFishIncProc;
    TTwoFishIncProc = procedure (var ctr: TTwoFish128Block); stdcall;
    { контекст }
    PTwoFishCtx = ^TTwoFishCtx;
    TTwoFishCtx = packed record
        IV      : TTwoFish128Block; { IV or CTR               }
        buf     : TTwoFish128Block; { Work buffer             }
        bLen    : WORD;             { Bytes used in buf       }
        Flag    : WORD;             { Bit 1: Short block      }
        IncProc : TTwoFishIncProc;  { Increment proc CTR-Mode }
        RK      : TTwoFishRndKey;   { Round keys              }
        S0, S1,                     { Key dependent SBoxes    }
        S2, S3  : TTwoFishSBox;     { DO NOT CHANGE SEQUENCE! }
    end;
{$ELSE EXTERNAL_LINK}
type
    { 128-битный блок }
    PTwoFish128Block = ^TTwoFish128Block;
    TTwoFish128Block = TTFBlock;
    { s-box }
    PTwoFishSBox = ^TTwoFishSBox;
    TTwoFishSBox = packed array [0..255] of LongInt;
    { расписание ключей }
    PTwoFishRndKey = ^TTwoFishRndKey;
    TTwoFishRndKey = packed array [0..39] of LongInt;
    { инкремент }
    PTwoFishIncProc = ^TTwoFishIncProc;
    TTwoFishIncProc = procedure (var ctr: TTwoFish128Block);
    { контекст }
    PTwoFishCtx = ^TTwoFishCtx;
    TTwoFishCtx = TTFContext;
{$ENDIF EXTERNAL_LINK}

{$IFDEF EXTERNAL_LINK}
{ twofish dll functions }
const
    TWOFISH_DLL = 'twofish/tf_dll.dll';
{$IFDEF ECB}
function TF_ECB_Init (const Key;
                      KeyBits: WORD;
                      var ctx: TTwoFishCtx) : Integer; stdcall;
    external TWOFISH_DLL name 'TF_ECB_Init';
function TF_ECB_Reset (var ctx: TTwoFishCtx) : Integer; stdcall;
    external TWOFISH_DLL name 'TF_ECB_Reset';
function TF_ECB_Encrypt (ptp, ctp: Pointer;
                         iLen: LongInt;
                         var ctx: TTwoFishCtx) : Integer; stdcall;
    external TWOFISH_DLL name 'TF_ECB_Encrypt';
function TF_ECB_Decrypt (ctp, ptp: Pointer;
                         iLen: LongInt;
                         var ctx: TTwoFishCtx) : Integer; stdcall;
    external TWOFISH_DLL name 'TF_ECB_Decrypt';
{$ENDIF ECB}
{$IFDEF CBC}
function TF_CBC_Init (const Key;
                      KeyBits: WORD;
                      const IV: TTwoFish128Block;
                      var ctx: TTwoFishCtx) : Integer; stdcall;
    external TWOFISH_DLL name 'TF_CBC_Init';
function TF_CBC_Reset (var IV: TTwoFish128Block;
                       var ctx: TTwoFishCtx) : Integer; stdcall;
    external TWOFISH_DLL name 'TF_CBC_Reset';
function TF_CBC_Encrypt (ptp, ctp: Pointer;
                         iLen: LongInt;
                         var ctx: TTwoFishCtx) : Integer; stdcall;
    external TWOFISH_DLL name 'TF_CBC_Encrypt';
function TF_CBC_Decrypt (ctp, ptp: Pointer;
                         iLen: LongInt;
                         var ctx: TTwoFishCtx) : Integer; stdcall;
    external TWOFISH_DLL name 'TF_CBC_Decrypt';
{$ENDIF CBC}
{$IFDEF CFB}
function TF_CFB_Init (const Key;
                      KeyBits: WORD;
                      const IV: TTwoFish128Block;
                      var ctx: TTwoFishCtx) : Integer; stdcall;
    external TWOFISH_DLL name 'TF_CFB_Init';
function TF_CFB_Reset (var IV: TTwoFish128Block;
                       var ctx: TTwoFishCtx) : Integer; stdcall;
    external TWOFISH_DLL name 'TF_CFB_Reset';
function TF_CFB_Encrypt (ptp, ctp: Pointer;
                         iLen: LongInt;
                         var ctx: TTwoFishCtx) : Integer; stdcall;
    external TWOFISH_DLL name 'TF_CFB_Encrypt';
function TF_CFB_Decrypt (ctp, ptp: Pointer;
                         iLen: LongInt;
                         var ctx: TTwoFishCtx) : Integer; stdcall;
    external TWOFISH_DLL name 'TF_CFB_Decrypt';
{$ENDIF CFB}
{$IFDEF OFB}
function TF_OFB_Init (const Key;
                      KeyBits: WORD;
                      const IV: TTwoFish128Block;
                      var ctx: TTwoFishCtx) : Integer; stdcall;
    external TWOFISH_DLL name 'TF_OFB_Init';
function TF_OFB_Reset (var IV: TTwoFish128Block;
                       var ctx: TTwoFishCtx) : Integer; stdcall;
    external TWOFISH_DLL name 'TF_OFB_Reset';
function TF_OFB_Encrypt (ptp, ctp: Pointer;
                         iLen: LongInt;
                         var ctx: TTwoFishCtx) : Integer; stdcall;
    external TWOFISH_DLL name 'TF_OFB_Encrypt';
function TF_OFB_Decrypt (ctp, ptp: Pointer;
                         iLen: LongInt;
                         var ctx: TTwoFishCtx) : Integer; stdcall;
    external TWOFISH_DLL name 'TF_OFB_Decrypt';
{$ENDIF OFB}
function GET_TWOFISH_DLL_VERSION : PAnsiChar; stdcall;
    external TWOFISH_DLL name 'TF_DLL_Version';
{$ENDIF EXTERNAL_LINK}

function TWOFISH_VERSION : String;

const
    NULL_TWOFISH_128_BLOCK : TTwoFish128Block = (
        $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00
    );

function TwoFishIV (anArgs: array of const) : TTwoFish128Block;
function HexToTwoFishIV (const aValue: Hex) : TTwoFish128Block;
function TwoFishIVToHex (const aValue: TTwoFish128Block) : Hex;

function GetRandomTwoFishIV (aRandomFunc: TRandomFunction) : TTwoFish128Block;

{ не используется вектор инициализации }
{$IFDEF ECB}
procedure EncryptTwoFishECB (var aKey;
                             const aKeySize: WORD;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptTwoFishECB (var aKey;
                             const aKeySize: WORD;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptTwoFishECB (var aKey;
                            const aKeySize: WORD;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptTwoFishECB (var aKey;
                            const aKeySize: WORD;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptTwoFishECB (const aKey: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptTwoFishECB (const aKey: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptTwoFishECB (const aKey: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptTwoFishECB (const aKey: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF ECB}

{ используется псевдо-случайный вектор инициализации,
  который помещается в начало шифротекста }
{$IFDEF CBC}
procedure EncryptTwoFishCBC (var aKey;
                             const aKeySize: WORD;
                             aRandomFunc: TRandomFunction;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptTwoFishCBC (var aKey;
                             const aKeySize: WORD;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptTwoFishCBC (var aKey;
                            const aKeySize: WORD;
                            aRandomFunc: TRandomFunction;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptTwoFishCBC (var aKey;
                            const aKeySize: WORD;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptTwoFishCBC (const aKey: Hex;
                             aRandomFunc: TRandomFunction;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptTwoFishCBC (const aKey: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptTwoFishCBC (const aKey: Hex;
                            aRandomFunc: TRandomFunction;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptTwoFishCBC (const aKey: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF CBC}
{$IFDEF CFB}
procedure EncryptTwoFishCFB (var aKey;
                             const aKeySize: WORD;
                             aRandomFunc: TRandomFunction;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptTwoFishCFB (var aKey;
                             const aKeySize: WORD;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptTwoFishCFB (var aKey;
                            const aKeySize: WORD;
                            aRandomFunc: TRandomFunction;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptTwoFishCFB (var aKey;
                            const aKeySize: WORD;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptTwoFishCFB (const aKey: Hex;
                             aRandomFunc: TRandomFunction;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptTwoFishCFB (const aKey: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptTwoFishCFB (const aKey: Hex;
                            aRandomFunc: TRandomFunction;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptTwoFishCFB (const aKey: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF CFB}
{$IFDEF OFB}
procedure EncryptTwoFishOFB (var aKey;
                             const aKeySize: WORD;
                             aRandomFunc: TRandomFunction;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptTwoFishOFB (var aKey;
                             const aKeySize: WORD;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptTwoFishOFB (var aKey;
                            const aKeySize: WORD;
                            aRandomFunc: TRandomFunction;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptTwoFishOFB (var aKey;
                            const aKeySize: WORD;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptTwoFishOFB (const aKey: Hex;
                             aRandomFunc: TRandomFunction;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptTwoFishOFB (const aKey: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptTwoFishOFB (const aKey: Hex;
                            aRandomFunc: TRandomFunction;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptTwoFishOFB (const aKey: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF OFB}

{ используется внешний вектор инициализации,
  который необходимо пересылать отдельным каналом }
{$IFDEF CBC}
procedure EncryptTwoFishCBC (var aKey;
                             const aKeySize: WORD;
                             const anIV: TTwoFish128Block;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptTwoFishCBC (var aKey;
                             const aKeySize: WORD;
                             const anIV: TTwoFish128Block;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptTwoFishCBC (var aKey;
                            const aKeySize: WORD;
                            const anIV: TTwoFish128Block;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptTwoFishCBC (var aKey;
                            const aKeySize: WORD;
                            const anIV: TTwoFish128Block;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptTwoFishCBC (const aKey: Hex;
                             const anIV: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptTwoFishCBC (const aKey: Hex;
                             const anIV: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptTwoFishCBC (const aKey: Hex;
                            const anIV: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptTwoFishCBC (const aKey: Hex;
                            const anIV: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF CBC}
{$IFDEF CFB}
procedure EncryptTwoFishCFB (var aKey;
                             const aKeySize: WORD;
                             const anIV: TTwoFish128Block;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptTwoFishCFB (var aKey;
                             const aKeySize: WORD;
                             const anIV: TTwoFish128Block;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptTwoFishCFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TTwoFish128Block;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptTwoFishCFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TTwoFish128Block;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptTwoFishCFB (const aKey: Hex;
                             const anIV: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptTwoFishCFB (const aKey: Hex;
                             const anIV: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptTwoFishCFB (const aKey: Hex;
                            const anIV: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptTwoFishCFB (const aKey: Hex;
                            const anIV: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF CFB}
{$IFDEF OFB}
procedure EncryptTwoFishOFB (var aKey;
                             const aKeySize: WORD;
                             const anIV: TTwoFish128Block;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptTwoFishOFB (var aKey;
                             const aKeySize: WORD;
                             const anIV: TTwoFish128Block;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptTwoFishOFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TTwoFish128Block;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptTwoFishOFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TTwoFish128Block;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptTwoFishOFB (const aKey: Hex;
                             const anIV: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptTwoFishOFB (const aKey: Hex;
                             const anIV: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptTwoFishOFB (const aKey: Hex;
                            const anIV: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptTwoFishOFB (const aKey: Hex;
                            const anIV: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF OFB}