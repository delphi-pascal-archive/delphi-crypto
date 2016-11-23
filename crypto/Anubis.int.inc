{******************************************************************************}
{*  Anubis.int.inc                                                            *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{$IFDEF EXTERNAL_LINK}
type
    { 128-битный блок }
    PAnubis128Block = ^TAnubis128Block;
    TAnubis128Block = packed array [0..15] of Byte;
    { расписание ключей }
    PAnubisRndKey = ^TAnubisRndKey;
    TAnubisRndKey = packed array [0..18, 0..3] of LongInt;
    { инкремент }
    PAnubisIncProc = ^TAnubisIncProc;
    TAnubisIncProc = procedure (var ctr: TAnubis128Block); stdcall;
    { контекст }
    PAnubisCtx = ^TAnubisCtx;
    TAnubisCtx = packed record
        IV      : TAnubis128Block; { IV or CTR               }
        buf     : TAnubis128Block; { Work buffer             }
        bLen    : WORD;            { Bytes used in buf       }
        Rounds  : WORD;            { Number of rounds        }
        KeyBits : WORD;            { Number of bits in key   }
        Decrypt : Byte;            { <>0 if decrypting key   }
        Flag    : Byte;            { Bit 1: Short block      }
        IncProc : TAnubisIncProc;  { Increment proc CTR-Mode }
        RK      : TAnubisRndKey;   { Encr/Decr round keys    }
    end;
{$ELSE EXTERNAL_LINK}
type
    { 128-битный блок }
    PAnubis128Block = ^TAnubis128Block;
    TAnubis128Block = TANUBlock;
    { расписание ключей }
    PAnubisRndKey = ^TAnubisRndKey;
    TAnubisRndKey = packed array [0..18, 0..3] of LongInt;
    { инкремент }
    PAnubisIncProc = ^TAnubisIncProc;
    TAnubisIncProc = procedure (var ctr: TAnubis128Block);
    { контекст }
    PAnubisCtx = ^TAnubisCtx;
    TAnubisCtx = TANUContext;
{$ENDIF EXTERNAL_LINK}

{$IFDEF EXTERNAL_LINK}
{ anubis dll functions }
const
    ANUBIS_DLL = 'anubis/anu_dll.dll';
{$IFDEF ECB}
function ANU_ECB_Init_Encr (const Key;
                            KeyBits: WORD;
                            var ctx: TAnubisCtx) : Integer; stdcall;
    external ANUBIS_DLL name 'ANU_ECB_Init_Encr';
function ANU_ECB_Encrypt (ptp, ctp: Pointer;
                          iLen: LongInt;
                          var ctx: TAnubisCtx) : Integer; stdcall;
    external ANUBIS_DLL name 'ANU_ECB_Encrypt';
function ANU_ECB_Init_Decr (var Key;
                            KeyBits: WORD;
                            var ctx: TAnubisCtx) : Integer; stdcall;
    external ANUBIS_DLL name 'ANU_ECB_Init_Decr';
function ANU_ECB_Decrypt (ctp, ptp: Pointer;
                         iLen: LongInt;
                         var ctx: TAnubisCtx) : Integer; stdcall;
    external ANUBIS_DLL name 'ANU_ECB_Decrypt';
{$ENDIF ECB}
{$IFDEF CBC}
function ANU_CBC_Init_Encr (const Key;
                            KeyBits: WORD;
                            const IV: TAnubis128Block;
                            var ctx: TAnubisCtx) : Integer; stdcall;
    external ANUBIS_DLL name 'ANU_CBC_Init_Encr';
function ANU_CBC_Encrypt (ptp, ctp: Pointer;
                          iLen: LongInt;
                          var ctx: TAnubisCtx) : Integer; stdcall;
    external ANUBIS_DLL name 'ANU_CBC_Encrypt';
function ANU_CBC_Init_Decr (var Key;
                            KeyBits: WORD;
                            var IV: TAnubis128Block;
                            var ctx: TAnubisCtx) : Integer; stdcall;
    external ANUBIS_DLL name 'ANU_CBC_Init_Decr';
function ANU_CBC_Decrypt (ctp, ptp: Pointer;
                         iLen: LongInt;
                         var ctx: TAnubisCtx) : Integer; stdcall;
    external ANUBIS_DLL name 'ANU_CBC_Decrypt';
{$ENDIF CBC}
{$IFDEF CFB}
function ANU_CFB_Init (const Key;
                       KeyBits: WORD;
                       const IV: TAnubis128Block;
                       var ctx: TAnubisCtx) : Integer; stdcall;
    external ANUBIS_DLL name 'ANU_CFB_Init';
function ANU_CFB_Encrypt (ptp, ctp: Pointer;
                          iLen: LongInt;
                          var ctx: TAnubisCtx) : Integer; stdcall;
    external ANUBIS_DLL name 'ANU_CFB_Encrypt';
function ANU_CFB_Decrypt (ctp, ptp: Pointer;
                          iLen: LongInt;
                          var ctx: TAnubisCtx) : Integer; stdcall;
    external ANUBIS_DLL name 'ANU_CFB_Decrypt';
{$ENDIF CFB}
{$IFDEF OFB}
function ANU_OFB_Init (const Key;
                       KeyBits: WORD;
                       const IV: TAnubis128Block;
                       var ctx: TAnubisCtx) : Integer; stdcall;
    external ANUBIS_DLL name 'ANU_OFB_Init';
function ANU_OFB_Encrypt (ptp, ctp: Pointer;
                          iLen: LongInt;
                          var ctx: TAnubisCtx) : Integer; stdcall;
    external ANUBIS_DLL name 'ANU_OFB_Encrypt';
function ANU_OFB_Decrypt (ctp, ptp: Pointer;
                          iLen: LongInt;
                          var ctx: TAnubisCtx) : Integer; stdcall;
    external ANUBIS_DLL name 'ANU_OFB_Decrypt';
{$ENDIF OFB}
function GET_ANUBIS_DLL_VERSION : PAnsiChar; stdcall;
    external ANUBIS_DLL name 'ANU_DLL_Version';
{$ENDIF EXTERNAL_LINK}

function ANUBIS_VERSION : String;

const
    NULL_ANUBIS_128_BLOCK : TAnubis128Block = (
        $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00
    );

function AnubisIV (anArgs: array of const) : TAnubis128Block;
function HexToAnubisIV (const aValue: Hex) : TAnubis128Block;
function AnubisIVToHex (const aValue: TAnubis128Block) : Hex;

function GetRandomAnubisIV (aRandomFunc: TRandomFunction) : TAnubis128Block;

{ не используется вектор инициализации }
{$IFDEF ECB}
procedure EncryptAnubisECB (var aKey;
                            const aKeySize: WORD;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
procedure DecryptAnubisECB (var aKey;
                            const aKeySize: WORD;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
function EncryptAnubisECB (var aKey;
                           const aKeySize: WORD;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;
function DecryptAnubisECB (var aKey;
                           const aKeySize: WORD;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptAnubisECB (const aKey: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
procedure DecryptAnubisECB (const aKey: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
function EncryptAnubisECB (const aKey: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;
function DecryptAnubisECB (const aKey: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF ECB}

{ используется псевдо-случайный вектор инициализации,
  который помещается в начало шифротекста }
{$IFDEF CBC}
procedure EncryptAnubisCBC (var aKey;
                            const aKeySize: WORD;
                            aRandomFunc: TRandomFunction;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
procedure DecryptAnubisCBC (var aKey;
                            const aKeySize: WORD;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
function EncryptAnubisCBC (var aKey;
                           const aKeySize: WORD;
                           aRandomFunc: TRandomFunction;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;
function DecryptAnubisCBC (var aKey;
                           const aKeySize: WORD;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptAnubisCBC (const aKey: Hex;
                            aRandomFunc: TRandomFunction;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
procedure DecryptAnubisCBC (const aKey: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
function EncryptAnubisCBC (const aKey: Hex;
                           aRandomFunc: TRandomFunction;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;
function DecryptAnubisCBC (const aKey: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF CBC}
{$IFDEF CFB}
procedure EncryptAnubisCFB (var aKey;
                            const aKeySize: WORD;
                            aRandomFunc: TRandomFunction;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
procedure DecryptAnubisCFB (var aKey;
                            const aKeySize: WORD;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
function EncryptAnubisCFB (var aKey;
                           const aKeySize: WORD;
                           aRandomFunc: TRandomFunction;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;
function DecryptAnubisCFB (var aKey;
                           const aKeySize: WORD;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptAnubisCFB (const aKey: Hex;
                            aRandomFunc: TRandomFunction;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
procedure DecryptAnubisCFB (const aKey: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
function EncryptAnubisCFB (const aKey: Hex;
                           aRandomFunc: TRandomFunction;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;
function DecryptAnubisCFB (const aKey: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF CFB}
{$IFDEF OFB}
procedure EncryptAnubisOFB (var aKey;
                            const aKeySize: WORD;
                            aRandomFunc: TRandomFunction;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
procedure DecryptAnubisOFB (var aKey;
                            const aKeySize: WORD;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
function EncryptAnubisOFB (var aKey;
                           const aKeySize: WORD;
                           aRandomFunc: TRandomFunction;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;
function DecryptAnubisOFB (var aKey;
                           const aKeySize: WORD;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptAnubisOFB (const aKey: Hex;
                            aRandomFunc: TRandomFunction;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
procedure DecryptAnubisOFB (const aKey: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
function EncryptAnubisOFB (const aKey: Hex;
                           aRandomFunc: TRandomFunction;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;
function DecryptAnubisOFB (const aKey: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF OFB}

{ используется внешний вектор инициализации,
  который необходимо пересылать отдельным каналом }
{$IFDEF CBC}
procedure EncryptAnubisCBC (var aKey;
                            const aKeySize: WORD;
                            const anIV: TAnubis128Block;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
procedure DecryptAnubisCBC (var aKey;
                            const aKeySize: WORD;
                            const anIV: TAnubis128Block;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
function EncryptAnubisCBC (var aKey;
                           const aKeySize: WORD;
                           const anIV: TAnubis128Block;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;
function DecryptAnubisCBC (var aKey;
                           const aKeySize: WORD;
                           const anIV: TAnubis128Block;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptAnubisCBC (const aKey: Hex;
                            const anIV: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
procedure DecryptAnubisCBC (const aKey: Hex;
                            const anIV: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
function EncryptAnubisCBC (const aKey: Hex;
                           const anIV: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;
function DecryptAnubisCBC (const aKey: Hex;
                           const anIV: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF CBC}
{$IFDEF CFB}
procedure EncryptAnubisCFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TAnubis128Block;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
procedure DecryptAnubisCFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TAnubis128Block;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
function EncryptAnubisCFB (var aKey;
                           const aKeySize: WORD;
                           const anIV: TAnubis128Block;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;
function DecryptAnubisCFB (var aKey;
                           const aKeySize: WORD;
                           const anIV: TAnubis128Block;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptAnubisCFB (const aKey: Hex;
                            const anIV: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
procedure DecryptAnubisCFB (const aKey: Hex;
                            const anIV: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
function EncryptAnubisCFB (const aKey: Hex;
                           const anIV: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;
function DecryptAnubisCFB (const aKey: Hex;
                           const anIV: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF CFB}
{$IFDEF OFB}
procedure EncryptAnubisOFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TAnubis128Block;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
procedure DecryptAnubisOFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TAnubis128Block;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
function EncryptAnubisOFB (var aKey;
                           const aKeySize: WORD;
                           const anIV: TAnubis128Block;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;
function DecryptAnubisOFB (var aKey;
                           const aKeySize: WORD;
                           const anIV: TAnubis128Block;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptAnubisOFB (const aKey: Hex;
                            const anIV: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
procedure DecryptAnubisOFB (const aKey: Hex;
                            const anIV: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16); overload;
function EncryptAnubisOFB (const aKey: Hex;
                           const anIV: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;
function DecryptAnubisOFB (const aKey: Hex;
                           const anIV: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF OFB}