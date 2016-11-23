{******************************************************************************}
{*  AES.int.inc                                                               *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{$IFDEF EXTERNAL_LINK}
type
    { 128-битный блок }
    PAES128Block = ^TAES128Block;
    TAES128Block = packed array [0..15] of Byte;
    { расписание ключей }
    PAESKeyArray = ^TAESKeyArray;
    TAESKeyArray = packed array [0..14] of TAES128Block;
    { инкремент }
    PAESIncProc = ^TAESIncProc;
    TAESIncProc = procedure (var ctr: TAES128Block); stdcall;
    { контекст }
    PAESCtx = ^TAESCtx;
    TAESCtx = packed record
        RK      : TAESKeyArray; { Key (encr. or decr.)    }
        IV      : TAES128Block; { IV or CTR               }
        buf     : TAES128Block; { Work buffer             }
        bLen    : WORD;         { Bytes used in buf       }
        Rounds  : WORD;         { Number of rounds        }
        KeyBits : WORD;         { Number of bits in key   }
        Decrypt : Byte;         { <>0 if decrypting key   }
        Flag    : Byte;         { Bit 1: Short block      }
        IncProc : TAESIncProc;  { Increment proc CTR-Mode }
    end;
{$ELSE EXTERNAL_LINK}
type
    { 128-битный блок }
    PAES128Block = ^TAES128Block;
    TAES128Block = TAESBlock;
    { расписание ключей }
    PAESKeyArray = ^TAESKeyArray;
    TAESKeyArray = packed array [0..14] of TAES128Block;
    { инкремент }
    PAESIncProc = ^TAESIncProc;
    TAESIncProc = procedure (var ctr: TAES128Block);
    { контекст }
    PAESCtx = ^TAESCtx;
    TAESCtx = TAESContext;
{$ENDIF EXTERNAL_LINK}

{$IFDEF EXTERNAL_LINK}
{ aes dll functions }
const
    AES_DLL = 'aes/aes_dll.dll';
{$IFDEF ECB}
function AES_ECB_Init_Encr (const Key;
                            KeyBits: WORD;
                            var ctx: TAESCtx) : Integer; stdcall;
    external AES_DLL name 'AES_ECB_Init_Encr';
function AES_ECB_Encrypt (ptp, ctp: Pointer;
                          iLen: LongInt;
                          var ctx: TAESCtx) : Integer; stdcall;
    external AES_DLL name 'AES_ECB_Encrypt';
function AES_ECB_Init_Decr (var Key;
                            KeyBits: WORD;
                            var ctx: TAESCtx) : Integer; stdcall;
    external AES_DLL name 'AES_ECB_Init_Decr';
function AES_ECB_Decrypt (ctp, ptp: Pointer;
                          iLen: LongInt;
                          var ctx: TAESCtx) : Integer; stdcall;
    external AES_DLL name 'AES_ECB_Decrypt';
{$ENDIF ECB}
{$IFDEF CBC}
function AES_CBC_Init_Encr (const Key;
                            KeyBits: WORD;
                            const IV: TAES128Block;
                            var ctx: TAESCtx) : Integer; stdcall;
    external AES_DLL name 'AES_CBC_Init_Encr';
function AES_CBC_Encrypt (ptp, ctp: Pointer;
                          iLen: LongInt;
                          var ctx: TAESCtx) : Integer; stdcall;
    external AES_DLL name 'AES_CBC_Encrypt';
function AES_CBC_Init_Decr (var Key;
                            KeyBits: WORD;
                            var IV: TAES128Block;
                            var ctx: TAESCtx) : Integer; stdcall;
    external AES_DLL name 'AES_CBC_Init_Decr';
function AES_CBC_Decrypt (ctp, ptp: Pointer;
                          iLen: LongInt;
                          var ctx: TAESCtx) : Integer; stdcall;
    external AES_DLL name 'AES_CBC_Decrypt';
{$ENDIF CBC}
{$IFDEF CFB}
function AES_CFB_Init (const Key;
                       KeyBits: WORD;
                       const IV: TAES128Block;
                       var ctx: TAESCtx) : Integer; stdcall;
    external AES_DLL name 'AES_CFB_Init';
function AES_CFB_Encrypt (ptp, ctp: Pointer;
                          iLen: LongInt;
                          var ctx: TAESCtx) : Integer; stdcall;
    external AES_DLL name 'AES_CFB_Encrypt';
function AES_CFB_Decrypt (ctp, ptp: Pointer;
                          iLen: LongInt;
                          var ctx: TAESCtx) : Integer; stdcall;
    external AES_DLL name 'AES_CFB_Decrypt';
{$ENDIF CFB}
{$IFDEF OFB}
function AES_OFB_Init (const Key;
                       KeyBits: WORD;
                       const IV: TAES128Block;
                       var ctx: TAESCtx) : Integer; stdcall;
    external AES_DLL name 'AES_OFB_Init';
function AES_OFB_Encrypt (ptp, ctp: Pointer;
                          iLen: LongInt;
                          var ctx: TAESCtx) : Integer; stdcall;
    external AES_DLL name 'AES_OFB_Encrypt';
function AES_OFB_Decrypt (ctp, ptp: Pointer;
                          iLen: LongInt;
                          var ctx: TAESCtx) : Integer; stdcall;
    external AES_DLL name 'AES_OFB_Decrypt';
{$ENDIF OFB}
function GET_AES_DLL_VERSION : PAnsiChar; stdcall;
    external AES_DLL name 'AES_DLL_Version';
{$ENDIF EXTERNAL_LINK}

function AES_VERSION : String;

const
    NULL_AES_128_BLOCK : TAES128Block = (
        $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00
    );

function AESIV (anArgs: array of const) : TAES128Block;
function HexToAESIV (const aValue: Hex) : TAES128Block;
function AESIVToHex (const aValue: TAES128Block) : Hex;

function GetRandomAESIV (aRandomFunc: TRandomFunction) : TAES128Block;

{ не используется вектор инициализации }
{$IFDEF ECB}
procedure EncryptAESECB (var aKey;
                         const aKeySize: WORD;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
procedure DecryptAESECB (var aKey;
                         const aKeySize: WORD;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
function EncryptAESECB (var aKey;
                        const aKeySize: WORD;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;
function DecryptAESECB (var aKey;
                        const aKeySize: WORD;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptAESECB (const aKey: Hex;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
procedure DecryptAESECB (const aKey: Hex;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
function EncryptAESECB (const aKey: Hex;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;
function DecryptAESECB (const aKey: Hex;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF ECB}

{ используется псевдо-случайный вектор инициализации,
  который помещается в начало шифротекста }
{$IFDEF CBC}
procedure EncryptAESCBC (var aKey;
                         const aKeySize: WORD;
                         aRandomFunc: TRandomFunction;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
procedure DecryptAESCBC (var aKey;
                         const aKeySize: WORD;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
function EncryptAESCBC (var aKey;
                        const aKeySize: WORD;
                        aRandomFunc: TRandomFunction;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;
function DecryptAESCBC (var aKey;
                        const aKeySize: WORD;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptAESCBC (const aKey: Hex;
                         aRandomFunc: TRandomFunction;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
procedure DecryptAESCBC (const aKey: Hex;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
function EncryptAESCBC (const aKey: Hex;
                        aRandomFunc: TRandomFunction;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;
function DecryptAESCBC (const aKey: Hex;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF CBC}
{$IFDEF CFB}
procedure EncryptAESCFB (var aKey;
                         const aKeySize: WORD;
                         aRandomFunc: TRandomFunction;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
procedure DecryptAESCFB (var aKey;
                         const aKeySize: WORD;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
function EncryptAESCFB (var aKey;
                        const aKeySize: WORD;
                        aRandomFunc: TRandomFunction;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;
function DecryptAESCFB (var aKey;
                        const aKeySize: WORD;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptAESCFB (const aKey: Hex;
                         aRandomFunc: TRandomFunction;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
procedure DecryptAESCFB (const aKey: Hex;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
function EncryptAESCFB (const aKey: Hex;
                        aRandomFunc: TRandomFunction;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;
function DecryptAESCFB (const aKey: Hex;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF CFB}
{$IFDEF OFB}
procedure EncryptAESOFB (var aKey;
                         const aKeySize: WORD;
                         aRandomFunc: TRandomFunction;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
procedure DecryptAESOFB (var aKey;
                         const aKeySize: WORD;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
function EncryptAESOFB (var aKey;
                        const aKeySize: WORD;
                        aRandomFunc: TRandomFunction;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;
function DecryptAESOFB (var aKey;
                        const aKeySize: WORD;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptAESOFB (const aKey: Hex;
                         aRandomFunc: TRandomFunction;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
procedure DecryptAESOFB (const aKey: Hex;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
function EncryptAESOFB (const aKey: Hex;
                        aRandomFunc: TRandomFunction;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;
function DecryptAESOFB (const aKey: Hex;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF OFB}

{ используется внешний вектор инициализации,
  который необходимо пересылать отдельным каналом }
{$IFDEF CBC}
procedure EncryptAESCBC (var aKey;
                         const aKeySize: WORD;
                         const anIV: TAES128Block;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
procedure DecryptAESCBC (var aKey;
                         const aKeySize: WORD;
                         const anIV: TAES128Block;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
function EncryptAESCBC (var aKey;
                        const aKeySize: WORD;
                        const anIV: TAES128Block;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;
function DecryptAESCBC (var aKey;
                        const aKeySize: WORD;
                        const anIV: TAES128Block;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptAESCBC (const aKey: Hex;
                         const anIV: Hex;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
procedure DecryptAESCBC (const aKey: Hex;
                         const anIV: Hex;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
function EncryptAESCBC (const aKey: Hex;
                        const anIV: Hex;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;
function DecryptAESCBC (const aKey: Hex;
                        const anIV: Hex;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF CBC}
{$IFDEF CFB}
procedure EncryptAESCFB (var aKey;
                         const aKeySize: WORD;
                         const anIV: TAES128Block;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
procedure DecryptAESCFB (var aKey;
                         const aKeySize: WORD;
                         const anIV: TAES128Block;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
function EncryptAESCFB (var aKey;
                        const aKeySize: WORD;
                        const anIV: TAES128Block;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;
function DecryptAESCFB (var aKey;
                        const aKeySize: WORD;
                        const anIV: TAES128Block;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptAESCFB (const aKey: Hex;
                         const anIV: Hex;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
procedure DecryptAESCFB (const aKey: Hex;
                         const anIV: Hex;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
function EncryptAESCFB (const aKey: Hex;
                        const anIV: Hex;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;
function DecryptAESCFB (const aKey: Hex;
                        const anIV: Hex;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF CFB}
{$IFDEF OFB}
procedure EncryptAESOFB (var aKey;
                         const aKeySize: WORD;
                         const anIV: TAES128Block;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
procedure DecryptAESOFB (var aKey;
                         const aKeySize: WORD;
                         const anIV: TAES128Block;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
function EncryptAESOFB (var aKey;
                        const aKeySize: WORD;
                        const anIV: TAES128Block;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;
function DecryptAESOFB (var aKey;
                        const aKeySize: WORD;
                        const anIV: TAES128Block;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;

procedure EncryptAESOFB (const aKey: Hex;
                         const anIV: Hex;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
procedure DecryptAESOFB (const aKey: Hex;
                         const anIV: Hex;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16); overload;
function EncryptAESOFB (const aKey: Hex;
                        const anIV: Hex;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;
function DecryptAESOFB (const aKey: Hex;
                        const anIV: Hex;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String; overload;
{$ENDIF OFB}