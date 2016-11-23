{******************************************************************************}
{*  Serpent.int.inc                                                           *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{$IFDEF EXTERNAL_LINK}
type
    { 128-битный блок }
    PSerpent128Block = ^TSerpent128Block;
    TSerpent128Block = packed array [0..15] of Byte;
    { расписание ключей }
    PSerpentRndKey = ^TSerpentRndKey;
    TSerpentRndKey = packed array [0..131] of LongInt;
    { инкремент }
    PSerpentIncProc = ^TSerpentIncProc;
    TSerpentIncProc = procedure (var ctr: TSerpent128Block); stdcall;
    { контекст }
    PSerpentCtx = ^TSerpentCtx;
    TSerpentCtx = packed record
        IV      : TSerpent128Block; { IV or CTR               }
        buf     : TSerpent128Block; { Work buffer             }
        bLen    : WORD;             { Bytes used in buf       }
        Flag    : WORD;             { Bit 1: Short block      }
        IncProc : TSerpentIncProc;  { Increment proc CTR-Mode }
        RK      : TSerpentRndKey;   { Round keys              }
    end;
{$ELSE EXTERNAL_LINK}
type
    { 128-битный блок }
    PSerpent128Block = ^TSerpent128Block;
    TSerpent128Block = TSPBlock;
    { расписание ключей }
    PSerpentRndKey = ^TSerpentRndKey;
    TSerpentRndKey = packed array [0..131] of LongInt;
    { инкремент }
    PSerpentIncProc = ^TSerpentIncProc;
    TSerpentIncProc = procedure (var ctr: TSerpent128Block); 
    { контекст }
    PSerpentCtx = ^TSerpentCtx;
    TSerpentCtx = TSPContext;
{$ENDIF EXTERNAL_LINK}

{$IFDEF EXTERNAL_LINK}
{ serpent dll functions }
const
    SERPENT_DLL = 'serpent/sp_dll.dll';
{$IFDEF ECB}
function SP_ECB_Init (const Key;
                      KeyBits: WORD;
                      var ctx: TSerpentCtx) : Integer; stdcall;
    external SERPENT_DLL name 'SP_ECB_Init';
function SP_ECB_Reset (var ctx: TSerpentCtx) : Integer; stdcall;
    external SERPENT_DLL name 'SP_ECB_Reset';
function SP_ECB_Encrypt (ptp, ctp: Pointer;
                         iLen: LongInt;
                         var ctx: TSerpentCtx) : Integer; stdcall;
    external SERPENT_DLL name 'SP_ECB_Encrypt';
function SP_ECB_Decrypt (ctp, ptp: Pointer;
                         iLen: LongInt;
                         var ctx: TSerpentCtx) : Integer; stdcall;
    external SERPENT_DLL name 'SP_ECB_Decrypt';
{$ENDIF ECB}
{$IFDEF CBC}
function SP_CBC_Init (const Key;
                      KeyBits: WORD;
                      const IV: TSerpent128Block;
                      var ctx: TSerpentCtx) : Integer; stdcall;
    external SERPENT_DLL name 'SP_CBC_Init';
function SP_CBC_Reset (var IV: TSerpent128Block;
                       var ctx: TSerpentCtx) : Integer; stdcall;
    external SERPENT_DLL name 'SP_CBC_Reset';
function SP_CBC_Encrypt (ptp, ctp: Pointer;
                         iLen: LongInt;
                         var ctx: TSerpentCtx) : Integer; stdcall;
    external SERPENT_DLL name 'SP_CBC_Encrypt';
function SP_CBC_Decrypt (ctp, ptp: Pointer;
                         iLen: LongInt;
                         var ctx: TSerpentCtx) : Integer; stdcall;
    external SERPENT_DLL name 'SP_CBC_Decrypt';
{$ENDIF CBC}
{$IFDEF CFB}
function SP_CFB_Init (const Key;
                      KeyBits: WORD;
                      const IV: TSerpent128Block;
                      var ctx: TSerpentCtx) : Integer; stdcall;
    external SERPENT_DLL name 'SP_CFB_Init';
function SP_CFB_Reset (var IV: TSerpent128Block;
                       var ctx: TSerpentCtx) : Integer; stdcall;
    external SERPENT_DLL name 'SP_CFB_Reset';
function SP_CFB_Encrypt (ptp, ctp: Pointer;
                         iLen: LongInt;
                         var ctx: TSerpentCtx) : Integer; stdcall;
    external SERPENT_DLL name 'SP_CFB_Encrypt';
function SP_CFB_Decrypt (ctp, ptp: Pointer;
                         iLen: LongInt;
                         var ctx: TSerpentCtx) : Integer; stdcall;
    external SERPENT_DLL name 'SP_CFB_Decrypt';
{$ENDIF CFB}
{$IFDEF OFB}
function SP_OFB_Init (const Key;
                      KeyBits: WORD;
                      const IV: TSerpent128Block;
                      var ctx: TSerpentCtx) : Integer; stdcall;
    external SERPENT_DLL name 'SP_OFB_Init';
function SP_OFB_Reset (var IV: TSerpent128Block;
                       var ctx: TSerpentCtx) : Integer; stdcall;
    external SERPENT_DLL name 'SP_OFB_Reset';
function SP_OFB_Encrypt (ptp, ctp: Pointer;
                         iLen: LongInt;
                         var ctx: TSerpentCtx) : Integer; stdcall;
    external SERPENT_DLL name 'SP_OFB_Encrypt';
function SP_OFB_Decrypt (ctp, ptp: Pointer;
                         iLen: LongInt;
                         var ctx: TSerpentCtx) : Integer; stdcall;
    external SERPENT_DLL name 'SP_OFB_Decrypt';
{$ENDIF OFB}
function GET_SERPENT_DLL_VERSION : PAnsiChar; stdcall;
    external SERPENT_DLL name 'SP_DLL_Version';
{$ENDIF EXTERNAL_LINK}

function SERPENT_VERSION : String;

const
    NULL_SERPENT_128_BLOCK : TSerpent128Block = (
        $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00
    );

function SerpentIV (anArgs: array of const) : TSerpent128Block;
function HexToSerpentIV (const aValue: Hex) : TSerpent128Block;
function SerpentIVToHex (const aValue: TSerpent128Block) : Hex;

function GetRandomSerpentIV (aRandomFunc: TRandomFunction) : TSerpent128Block;

{ не используется вектор инициализации }
{$IFDEF ECB}
procedure EncryptSerpentECB (var aKey;
                             const aKeySize: WORD;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptSerpentECB (var aKey;
                             const aKeySize: WORD;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptSerpentECB (var aKey;
                            const aKeySize: WORD;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptSerpentECB (var aKey;
                            const aKeySize: WORD;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptSerpentECB (const aKey: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptSerpentECB (const aKey: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptSerpentECB (const aKey: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptSerpentECB (const aKey: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF ECB}

{ используется псевдо-случайный вектор инициализации,
  который помещается в начало шифротекста }
{$IFDEF CBC}
procedure EncryptSerpentCBC (var aKey;
                             const aKeySize: WORD;
                             aRandomFunc: TRandomFunction;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptSerpentCBC (var aKey;
                             const aKeySize: WORD;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptSerpentCBC (var aKey;
                            const aKeySize: WORD;
                            aRandomFunc: TRandomFunction;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptSerpentCBC (var aKey;
                            const aKeySize: WORD;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptSerpentCBC (const aKey: Hex;
                             aRandomFunc: TRandomFunction;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptSerpentCBC (const aKey: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptSerpentCBC (const aKey: Hex;
                            aRandomFunc: TRandomFunction;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptSerpentCBC (const aKey: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF CBC}
{$IFDEF CFB}
procedure EncryptSerpentCFB (var aKey;
                             const aKeySize: WORD;
                             aRandomFunc: TRandomFunction;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptSerpentCFB (var aKey;
                             const aKeySize: WORD;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptSerpentCFB (var aKey;
                            const aKeySize: WORD;
                            aRandomFunc: TRandomFunction;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptSerpentCFB (var aKey;
                            const aKeySize: WORD;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptSerpentCFB (const aKey: Hex;
                             aRandomFunc: TRandomFunction;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptSerpentCFB (const aKey: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptSerpentCFB (const aKey: Hex;
                            aRandomFunc: TRandomFunction;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptSerpentCFB (const aKey: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF CFB}
{$IFDEF OFB}
procedure EncryptSerpentOFB (var aKey;
                             const aKeySize: WORD;
                             aRandomFunc: TRandomFunction;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptSerpentOFB (var aKey;
                             const aKeySize: WORD;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptSerpentOFB (var aKey;
                            const aKeySize: WORD;
                            aRandomFunc: TRandomFunction;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptSerpentOFB (var aKey;
                            const aKeySize: WORD;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptSerpentOFB (const aKey: Hex;
                             aRandomFunc: TRandomFunction;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptSerpentOFB (const aKey: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptSerpentOFB (const aKey: Hex;
                            aRandomFunc: TRandomFunction;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptSerpentOFB (const aKey: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF OFB}

{ используется внешний вектор инициализации,
  который необходимо пересылать отдельным каналом }
{$IFDEF CBC}
procedure EncryptSerpentCBC (var aKey;
                             const aKeySize: WORD;
                             const anIV: TSerpent128Block;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptSerpentCBC (var aKey;
                             const aKeySize: WORD;
                             const anIV: TSerpent128Block;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptSerpentCBC (var aKey;
                            const aKeySize: WORD;
                            const anIV: TSerpent128Block;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptSerpentCBC (var aKey;
                            const aKeySize: WORD;
                            const anIV: TSerpent128Block;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptSerpentCBC (const aKey: Hex;
                             const anIV: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptSerpentCBC (const aKey: Hex;
                             const anIV: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptSerpentCBC (const aKey: Hex;
                            const anIV: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptSerpentCBC (const aKey: Hex;
                            const anIV: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF CBC}
{$IFDEF CFB}
procedure EncryptSerpentCFB (var aKey;
                             const aKeySize: WORD;
                             const anIV: TSerpent128Block;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptSerpentCFB (var aKey;
                             const aKeySize: WORD;
                             const anIV: TSerpent128Block;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptSerpentCFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TSerpent128Block;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptSerpentCFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TSerpent128Block;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptSerpentCFB (const aKey: Hex;
                             const anIV: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptSerpentCFB (const aKey: Hex;
                             const anIV: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptSerpentCFB (const aKey: Hex;
                            const anIV: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptSerpentCFB (const aKey: Hex;
                            const anIV: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF CFB}
{$IFDEF OFB}
procedure EncryptSerpentOFB (var aKey;
                             const aKeySize: WORD;
                             const anIV: TSerpent128Block;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptSerpentOFB (var aKey;
                             const aKeySize: WORD;
                             const anIV: TSerpent128Block;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptSerpentOFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TSerpent128Block;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptSerpentOFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TSerpent128Block;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptSerpentOFB (const aKey: Hex;
                             const anIV: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
procedure DecryptSerpentOFB (const aKey: Hex;
                             const anIV: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16); overload;
function EncryptSerpentOFB (const aKey: Hex;
                            const anIV: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
function DecryptSerpentOFB (const aKey: Hex;
                            const anIV: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF OFB}