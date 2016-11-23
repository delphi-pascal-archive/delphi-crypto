{******************************************************************************}
{*  BlowFish.int.inc                                                          *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{$IFDEF EXTERNAL_LINK}
type
    { 64-битный блок }
    PBlowFish64Block = ^TBlowFish64Block;
    TBlowFish64Block = packed array [0..7] of Byte;
    { s-box }
    PBlowFishSBox = ^TBlowFishSBox;
    TBlowFishSBox = packed array [0..255] of LongInt;
    PBlowFishSBoxes = ^TBlowFishSBoxes;
    TBlowFishSBoxes = packed array [0..3] of TBlowFishSBox;
    { PArray }
    PBlowFishPArray = ^TBlowFishPArray;
    TBlowFishPArray =  packed array [0..17] of LongInt;
    { инкремент }
    PBlowFishIncProc = ^TBlowFishIncProc;
    TBlowFishIncProc = procedure (var ctr: TBlowFish64Block); stdcall;
    { контекст }
    PBlowFishCtx = ^TBlowFishCtx;
    TBlowFishCtx = packed record
        SBox    : TBlowFishSBoxes;  { key dependend SBox      }
        PArray  : TBlowFishPArray;  { key dependend PArray    }
        IV      : TBlowFish64Block; { IV or CTR               }
        buf     : TBlowFish64Block; { Work buffer             }
        bLen    : WORD;             { Bytes used in buf       }
        Flag    : WORD;             { Bit 1: Short block      }
        IncProc : TBlowFishIncProc; { Increment proc CTR-Mode }
    end;
{$ELSE EXTERNAL_LINK}
type
    { 64-битный блок }
    PBlowFish64Block = ^TBlowFish64Block;
    TBlowFish64Block = TBFBlock;
    { s-box }
    PBlowFishSBox = ^TBlowFishSBox;
    TBlowFishSBox = packed array [0..255] of LongInt;
    PBlowFishSBoxes = ^TBlowFishSBoxes;
    TBlowFishSBoxes = packed array [0..3] of TBlowFishSBox;
    { PArray }
    PBlowFishPArray = ^TBlowFishPArray;
    TBlowFishPArray =  packed array [0..17] of LongInt;
    { инкремент }
    PBlowFishIncProc = ^TBlowFishIncProc;
    TBlowFishIncProc = procedure (var ctr: TBlowFish64Block); 
    { контекст }
    PBlowFishCtx = ^TBlowFishCtx;
    TBlowFishCtx = TBFContext;
{$ENDIF EXTERNAL_LINK}

{$IFDEF EXTERNAL_LINK}
{ blowfish dll functions }
const
    BLOWFISH_DLL = 'blowfish/bf_dll.dll';
{$IFDEF ECB}
function BF_ECB_Init (const Key;
                      KeyBits: WORD;
                      var ctx: TBlowFishCtx) : Integer; stdcall;
    external BLOWFISH_DLL name 'BF_ECB_Init';
function BF_ECB_Reset (var ctx: TBlowFishCtx) : Integer; stdcall;
    external BLOWFISH_DLL name 'BF_ECB_Reset';
function BF_ECB_Encrypt (ptp, ctp: Pointer;
                         iLen: LongInt;
                         var ctx: TBlowFishCtx) : Integer; stdcall;
    external BLOWFISH_DLL name 'BF_ECB_Encrypt';
function BF_ECB_Decrypt (ctp, ptp: Pointer;
                         iLen: LongInt;
                         var ctx: TBlowFishCtx) : Integer; stdcall;
    external BLOWFISH_DLL name 'BF_ECB_Decrypt';
{$ENDIF ECB}
{$IFDEF CBC}
function BF_CBC_Init (const Key;
                      KeyBits: WORD;
                      const IV: TBlowFish64Block;
                      var ctx: TBlowFishCtx) : Integer; stdcall;
    external BLOWFISH_DLL name 'BF_CBC_Init';
function BF_CBC_Reset (var IV: TBlowFish64Block;
                       var ctx: TBlowFishCtx) : Integer; stdcall;
    external BLOWFISH_DLL name 'BF_CBC_Reset';
function BF_CBC_Encrypt (ptp, ctp: Pointer;
                         iLen: LongInt;
                         var ctx: TBlowFishCtx) : Integer; stdcall;
    external BLOWFISH_DLL name 'BF_CBC_Encrypt';
function BF_CBC_Decrypt (ctp, ptp: Pointer;
                         iLen: LongInt;
                         var ctx: TBlowFishCtx) : Integer; stdcall;
    external BLOWFISH_DLL name 'BF_CBC_Decrypt';
{$ENDIF CBC}
{$IFDEF CFB}
function BF_CFB_Init (const Key;
                      KeyBits: WORD;
                      const IV: TBlowFish64Block;
                      var ctx: TBlowFishCtx) : Integer; stdcall;
    external BLOWFISH_DLL name 'BF_CFB_Init';
function BF_CFB_Reset (var IV: TBlowFish64Block;
                       var ctx: TBlowFishCtx) : Integer; stdcall;
    external BLOWFISH_DLL name 'BF_CFB_Reset';
function BF_CFB_Encrypt (ptp, ctp: Pointer;
                         iLen: LongInt;
                         var ctx: TBlowFishCtx) : Integer; stdcall;
    external BLOWFISH_DLL name 'BF_CFB_Encrypt';
function BF_CFB_Decrypt (ctp, ptp: Pointer;
                         iLen: LongInt;
                         var ctx: TBlowFishCtx) : Integer; stdcall;
    external BLOWFISH_DLL name 'BF_CFB_Decrypt';
{$ENDIF CFB}
{$IFDEF OFB}
function BF_OFB_Init (const Key;
                      KeyBits: WORD;
                      const IV: TBlowFish64Block;
                      var ctx: TBlowFishCtx) : Integer; stdcall;
    external BLOWFISH_DLL name 'BF_OFB_Init';
function BF_OFB_Reset (var IV: TBlowFish64Block;
                       var ctx: TBlowFishCtx) : Integer; stdcall;
    external BLOWFISH_DLL name 'BF_OFB_Reset';
function BF_OFB_Encrypt (ptp, ctp: Pointer;
                         iLen: LongInt;
                         var ctx: TBlowFishCtx) : Integer; stdcall;
    external BLOWFISH_DLL name 'BF_OFB_Encrypt';
function BF_OFB_Decrypt (ctp, ptp: Pointer;
                         iLen: LongInt;
                         var ctx: TBlowFishCtx) : Integer; stdcall;
    external BLOWFISH_DLL name 'BF_OFB_Decrypt';
{$ENDIF OFB}
function GET_BLOWFISH_DLL_VERSION : PAnsiChar; stdcall;
    external BLOWFISH_DLL name 'BF_DLL_Version';
{$ENDIF EXTERNAL_LINK}

function BLOWFISH_VERSION : String;

const
    NULL_BLOWFISH_64_BLOCK : TBlowFish64Block = (
        $00, $00, $00, $00, $00, $00, $00, $00
    );

function BlowFishIV (anArgs: array of const) : TBlowFish64Block;
function HexToBlowFishIV (const aValue: Hex) : TBlowFish64Block;
function BlowFishIVToHex (const aValue: TBlowFish64Block) : Hex;

function GetRandomBlowFishIV (aRandomFunc: TRandomFunction) : TBlowFish64Block;

{ не используется вектор инициализации }
{$IFDEF ECB}
procedure EncryptBlowFishECB (var aKey;
                              const aKeySize: WORD;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
procedure DecryptBlowFishECB (var aKey;
                              const aKeySize: WORD;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
function EncryptBlowFishECB (var aKey;
                             const aKeySize: WORD;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;
function DecryptBlowFishECB (var aKey;
                             const aKeySize: WORD;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptBlowFishECB (const aKey: Hex;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
procedure DecryptBlowFishECB (const aKey: Hex;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
function EncryptBlowFishECB (const aKey: Hex;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;
function DecryptBlowFishECB (const aKey: Hex;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF ECB}

{ используется псевдо-случайный вектор инициализации,
  который помещается в начало шифротекста }
{$IFDEF CBC}
procedure EncryptBlowFishCBC (var aKey;
                              const aKeySize: WORD;
                              aRandomFunc: TRandomFunction;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
procedure DecryptBlowFishCBC (var aKey;
                              const aKeySize: WORD;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
function EncryptBlowFishCBC (var aKey;
                             const aKeySize: WORD;
                             aRandomFunc: TRandomFunction;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;
function DecryptBlowFishCBC (var aKey;
                             const aKeySize: WORD;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptBlowFishCBC (const aKey: Hex;
                              aRandomFunc: TRandomFunction;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
procedure DecryptBlowFishCBC (const aKey: Hex;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
function EncryptBlowFishCBC (const aKey: Hex;
                             aRandomFunc: TRandomFunction;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;
function DecryptBlowFishCBC (const aKey: Hex;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF CBC}
{$IFDEF CFB}
procedure EncryptBlowFishCFB (var aKey;
                              const aKeySize: WORD;
                              aRandomFunc: TRandomFunction;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
procedure DecryptBlowFishCFB (var aKey;
                              const aKeySize: WORD;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
function EncryptBlowFishCFB (var aKey;
                             const aKeySize: WORD;
                             aRandomFunc: TRandomFunction;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;
function DecryptBlowFishCFB (var aKey;
                             const aKeySize: WORD;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptBlowFishCFB (const aKey: Hex;
                              aRandomFunc: TRandomFunction;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
procedure DecryptBlowFishCFB (const aKey: Hex;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
function EncryptBlowFishCFB (const aKey: Hex;
                             aRandomFunc: TRandomFunction;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;
function DecryptBlowFishCFB (const aKey: Hex;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF CFB}
{$IFDEF OFB}
procedure EncryptBlowFishOFB (var aKey;
                              const aKeySize: WORD;
                              aRandomFunc: TRandomFunction;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
procedure DecryptBlowFishOFB (var aKey;
                              const aKeySize: WORD;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
function EncryptBlowFishOFB (var aKey;
                             const aKeySize: WORD;
                             aRandomFunc: TRandomFunction;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;
function DecryptBlowFishOFB (var aKey;
                             const aKeySize: WORD;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptBlowFishOFB (const aKey: Hex;
                              aRandomFunc: TRandomFunction;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
procedure DecryptBlowFishOFB (const aKey: Hex;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
function EncryptBlowFishOFB (const aKey: Hex;
                             aRandomFunc: TRandomFunction;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;
function DecryptBlowFishOFB (const aKey: Hex;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF OFB}

{ используется внешний вектор инициализации,
  который необходимо пересылать отдельным каналом }
{$IFDEF CBC}
procedure EncryptBlowFishCBC (var aKey;
                              const aKeySize: WORD;
                              const anIV: TBlowFish64Block;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
procedure DecryptBlowFishCBC (var aKey;
                              const aKeySize: WORD;
                              const anIV: TBlowFish64Block;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
function EncryptBlowFishCBC (var aKey;
                             const aKeySize: WORD;
                             const anIV: TBlowFish64Block;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;
function DecryptBlowFishCBC (var aKey;
                             const aKeySize: WORD;
                             const anIV: TBlowFish64Block;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptBlowFishCBC (const aKey: Hex;
                              const anIV: Hex;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
procedure DecryptBlowFishCBC (const aKey: Hex;
                              const anIV: Hex;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
function EncryptBlowFishCBC (const aKey: Hex;
                             const anIV: Hex;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;
function DecryptBlowFishCBC (const aKey: Hex;
                             const anIV: Hex;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF CBC}
{$IFDEF CFB}
procedure EncryptBlowFishCFB (var aKey;
                              const aKeySize: WORD;
                              const anIV: TBlowFish64Block;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
procedure DecryptBlowFishCFB (var aKey;
                              const aKeySize: WORD;
                              const anIV: TBlowFish64Block;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
function EncryptBlowFishCFB (var aKey;
                             const aKeySize: WORD;
                             const anIV: TBlowFish64Block;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;
function DecryptBlowFishCFB (var aKey;
                             const aKeySize: WORD;
                             const anIV: TBlowFish64Block;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptBlowFishCFB (const aKey: Hex;
                              const anIV: Hex;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
procedure DecryptBlowFishCFB (const aKey: Hex;
                              const anIV: Hex;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
function EncryptBlowFishCFB (const aKey: Hex;
                             const anIV: Hex;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;
function DecryptBlowFishCFB (const aKey: Hex;
                             const anIV: Hex;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF CFB}
{$IFDEF OFB}
procedure EncryptBlowFishOFB (var aKey;
                              const aKeySize: WORD;
                              const anIV: TBlowFish64Block;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
procedure DecryptBlowFishOFB (var aKey;
                              const aKeySize: WORD;
                              const anIV: TBlowFish64Block;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
function EncryptBlowFishOFB (var aKey;
                             const aKeySize: WORD;
                             const anIV: TBlowFish64Block;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;
function DecryptBlowFishOFB (var aKey;
                             const aKeySize: WORD;
                             const anIV: TBlowFish64Block;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptBlowFishOFB (const aKey: Hex;
                              const anIV: Hex;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
procedure DecryptBlowFishOFB (const aKey: Hex;
                              const anIV: Hex;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16); overload;
function EncryptBlowFishOFB (const aKey: Hex;
                             const anIV: Hex;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;
function DecryptBlowFishOFB (const aKey: Hex;
                             const anIV: Hex;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF OFB}