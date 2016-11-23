unit Crypto;
{******************************************************************************}
{*  Crypto Unit                                                               *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
interface

{$I '../std.inc'}

uses
    {$I 'PRNG.uses.inc'}
    {$I 'RSA.uses.inc'}
    {$I 'AES.uses.inc'}
    {$I 'Anubis.uses.inc'}
    {$I 'Serpent.uses.inc'}
    {$I 'Shacal.uses.inc'}
    {$I 'BlowFish.uses.inc'}
    {$I 'TwoFish.uses.inc'}
    Windows, SysUtils, Classes, DateUtils,
    Strings, Versions, VarRecs,
    EClasses;

{ функция хэширования }
type
    PHashFunction = ^THashFunction;
    THashFunction = function (const aValue: String) : String;

    PHashStreamFunction = ^THashStreamFunction;
    THashStreamFunction = function (const aValue: TStream;
                                    const aBlockSize: LongWord = 1024) : String;

type
    PInt64Array = ^TInt64Array;
    TInt64Array = array [0..0] of Int64;

    PDWordArray = ^TDWordArray;
    TDWordArray = array [0..0] of LongWord;

    PByteArray = ^TByteArray;
    TByteArray = array [0..0] of Byte;

    PByte = ^Byte;
    PLongWord = ^LongWord;

function ror (x: LongWord; y: Byte) : LongWord;
function rol (x: LongWord; y: Byte) : LongWord;
function ror64 (x: Int64; y: Byte) : Int64;
function Endian (x: LongWord) : LongWord;
function Endian64 (x: Int64) : Int64;

{ ключи шифрования }
{$I 'TKey.int.inc'}

{ описание генераторов псевдо-случайных чисел }
{$I 'PRNG.int.inc'}
{ описание функций асимм. шифрования }
{$I 'RSA.int.inc'}
{ описание функций симм. шифрования }
{$I 'AES.int.inc'}
{$I 'Anubis.int.inc'}
{$I 'Serpent.int.inc'}
{$I 'Shacal.int.inc'}
{$I 'BlowFish.int.inc'}
{$I 'TwoFish.int.inc'}
{ описание функций хэширования }
{$I 'SHA.int.inc'}
{$I 'Tiger.int.inc'}
{$I 'MD.int.inc'}
{$I 'RipeMD.int.inc'}

{ параметры генераторов псевдо-случайных чисел }
{$I 'TRandomType.int.inc'}
{ параметры асимм. шифрования }
{$I 'TPKCryptoType.int.inc'}
{ параметры симм. шифрования }
{$I 'TCryptoMode.int.inc'}
{$I 'TCryptoType.int.inc'}
{ параметры хэширования }
{$I 'THashType.int.inc'}

type
{$M+}
    CCryptoKernel = class of TCryptoKernel;
    PCryptoKernel = ^TCryptoKernel;
    TCryptoKernel = class (TObject)
    public
        class procedure _raise (anArgs: array of const;
                                const anEGUID: String = ''); overload; virtual;
        class procedure _raise (anArgs: array of const;
                                anEGUID: array of const); overload; virtual;
    { генератор псевдо-случайных чисел }
    protected
        class function GetRandomFunction (const aRandomType: TRandomType) : TRandomFunction; virtual;
    public
        class procedure InitRandom (const aValue: LongWord); overload; virtual;
        class function Random (const aMin: LongWord = 0;
                               const aMax: LongWord = High (LongWord);
                               const aRandomType: TRandomType = rtSystem) : LongWord; overload; virtual;
        class function Random (anArgs: array of const) : LongWord; overload; virtual;
        { тестирование генератора псевдо-случайных чисел }
        class function TestRandom (const aRandomType: TRandomType) : Boolean; overload; virtual;
        class function TestRandom (anArgs: array of const) : Boolean; overload; virtual;
    { хэширование }
    protected
        class function GetHashFunction (const aHashType: THashType) : THashFunction; virtual;
        class function GetHashStreamFunction (const aHashType: THashType) : THashStreamFunction; virtual;
    public
        class function Hash (const aValue: String;
                             const aHashType: THashType) : String; overload; virtual;
        class function Hash (const aValue: String;
                             const aHashAlgoritm: THashAlgoritm) : String; overload; virtual;
        class function Hash (const aValue: String;
                             anArgs: array of const) : String; overload; virtual;
        class function Hash (const aValue: TStream;
                             const aHashType: THashType) : String; overload; virtual;
        class function Hash (const aValue: TStream;
                             const aHashAlgoritm: THashAlgoritm) : String; overload; virtual;
        class function Hash (const aValue: TStream;
                             anArgs: array of const) : String; overload; virtual;
        class function Hash (const aValue: String;
                             const aSalt: String;
                             const aHashType: THashType;
                             const aCount: DWORD = 1024) : String; overload; virtual;
        class function Hash (const aValue: String;
                             const aSalt: String;
                             const aHashAlgoritm: THashAlgoritm;
                             const aCount: DWORD = 1024) : String; overload; virtual;
        class function Hash (const aValue: String;
                             const aSalt: String;
                             anArgs: array of const) : String; overload; virtual;
    { тестирование хэш-функций }
        class function TestHash (const aHashType: THashType) : Boolean; overload; virtual;
        class function TestHash (const aHashAlgoritm: THashAlgoritm) : Boolean; overload; virtual;
        class function TestHash (anArgs: array of const) : Boolean; overload; virtual;
    { конструкция HMAC }
    public
        class function HMAC (const aValue: String;
                             const aKey: Hex;
                             const aHashType: THashType) : String; overload; virtual;
        class function HMAC (const aValue: String;
                             const aKey: Hex;
                             const aHashAlgoritm: THashAlgoritm) : String; overload; virtual;
        class function HMAC (const aValue: String;
                             const aKey: Hex;
                             anArgs: array of const) : String; overload; virtual;
        class function HMAC (const aValue: String;
                             const aKey: Hex;
                             const aSalt: String;
                             const aHashType: THashType;
                             const aCount: DWORD = 1024) : String; overload; virtual;
        class function HMAC (const aValue: String;
                             const aKey: Hex;
                             const aSalt: String;
                             const aHashAlgoritm: THashAlgoritm;
                             const aCount: DWORD = 1024) : String; overload; virtual;
        class function HMAC (const aValue: String;
                             const aKey: Hex;
                             const aSalt: String;
                             anArgs: array of const) : String; overload; virtual;
    { асимм. шифрование }
    protected
        class procedure GenerateRSAKeys (out aPublicKey: String;
                                         out aPrivateKey: String;
                                         const aPKCryptoType: TPKCryptoType = pkctRSA1024;
                                         const aRandomType: TRandomType = rtSystem); virtual;
    public
        class procedure GenerateKeys (out aPublicKey: String;
                                      out aPrivateKey: String;
                                      const aPKCryptoType: TPKCryptoType;
                                      const aRandomType: TRandomType = rtSystem); overload; virtual;
        class procedure GenerateKeys (out aPublicKey: String;
                                      out aPrivateKey: String;
                                      anArgs: array of const); overload; virtual;
        class function Encrypt (const aValue: String;
                                const aPublicKey: String;
                                const aPKCryptoType: TPKCryptoType) : String; overload; virtual;
        class function Decrypt (const aValue: String;
                                const aPrivateKey: String;
                                const aPKCryptoType: TPKCryptoType) : String; overload; virtual;
    { тестирование асимм. шифрования }
        class function TestAsymmetric (const aPKCryptoType: TPKCryptoType) : Boolean; overload; virtual;
        class function TestAsymmetric (anArgs: array of const) : Boolean; overload; virtual;
    { асимм. шифрование с подписью }
    public
        class function Encrypt (const aValue: String;
                                const aPublicKey: String;
                                const aPrivateKey: String;
                                const aPKCryptoType: TPKCryptoType;
                                const aHashType: THashType) : String; overload; virtual;
        class function Encrypt (const aValue: String;
                                const aPublicKey: String;
                                const aPrivateKey: String;
                                const aPKCryptoType: TPKCryptoType;
                                const aHashAlgoritm: THashAlgoritm) : String; overload; virtual;
        class function Decrypt (const aValue: String;
                                const aPublicKey: String;
                                const aPrivateKey: String;
                                const aPKCryptoType: TPKCryptoType;
                                const aHashType: THashType) : String; overload; virtual;
        class function Decrypt (const aValue: String;
                                const aPublicKey: String;
                                const aPrivateKey: String;
                                const aPKCryptoType: TPKCryptoType;
                                const aHashAlgoritm: THashAlgoritm) : String; overload; virtual;
    { симм. шифрование }
    public
        class procedure GenerateKey (out aKey: String;
                                     const aCryptoType: TCryptoType;
                                     const aRandomType: TRandomType = rtSystem); overload; virtual;
        class procedure GenerateKey (out aKey: String;
                                     const aCryptoAlgoritm: TCryptoAlgoritm;
                                     const aRandomType: TRandomType = rtSystem); overload; virtual;
        class procedure GenerateKey (out aKey: String;
                                     anArgs: array of const); overload; virtual;
        class function Encrypt (const aValue: String;
                                const aKey: Hex;
                                const aCryptoType: TCryptoType;
                                const aCryptoMode: TCryptoMode;
                                const aRandomType: TRandomType = rtSystem) : String; overload; virtual;
        class function Encrypt (const aValue: String;
                                const aKey: Hex;
                                const aCryptoAlgoritm: TCryptoAlgoritm;
                                const aCryptoMode: TCryptoMode;
                                const aRandomType: TRandomType = rtSystem) : String; overload; virtual;
        class function Encrypt (const aValue: String;
                                const aKey: Hex;
                                anArgs: array of const) : String; overload; virtual;
        class function Decrypt (const aValue: String;
                                const aKey: Hex;
                                const aCryptoType: TCryptoType;
                                const aCryptoMode: TCryptoMode) : String; overload; virtual;
        class function Decrypt (const aValue: String;
                                const aKey: Hex;
                                const aCryptoAlgoritm: TCryptoAlgoritm;
                                const aCryptoMode: TCryptoMode) : String; overload; virtual;
        class function Decrypt (const aValue: String;
                                const aKey: Hex;
                                anArgs: array of const) : String; overload; virtual;
        class procedure Encrypt (const anInput: TStream;
                                 out anOutput: TStream;
                                 const aKey: Hex;
                                 const aCryptoType: TCryptoType;
                                 const aCryptoMode: TCryptoMode;
                                 const aRandomType: TRandomType = rtSystem); overload; virtual;
        class procedure Encrypt (const anInput: TStream;
                                 out anOutput: TStream;
                                 const aKey: Hex;
                                 const aCryptoAlgoritm: TCryptoAlgoritm;
                                 const aCryptoMode: TCryptoMode;
                                 const aRandomType: TRandomType = rtSystem); overload; virtual;
        class procedure Encrypt (const anInput: TStream;
                                 out anOutput: TStream;
                                 const aKey: Hex;
                                 anArgs: array of const); overload; virtual;
        class procedure Decrypt (const anInput: TStream;
                                 out anOutput: TStream;
                                 const aKey: Hex;
                                 const aCryptoType: TCryptoType;
                                 const aCryptoMode: TCryptoMode); overload; virtual;
        class procedure Decrypt (const anInput: TStream;
                                 out anOutput: TStream;
                                 const aKey: Hex;
                                 const aCryptoAlgoritm: TCryptoAlgoritm;
                                 const aCryptoMode: TCryptoMode); overload; virtual;
        class procedure Decrypt (const anInput: TStream;
                                 out anOutput: TStream;
                                 const aKey: Hex;
                                 anArgs: array of const); overload; virtual;
    { тестирование симм. шифрования }
        class function TestSymmetric (const aCryptoType: TCryptoType;
                                      const aCryptoMode: TCryptoMode) : Boolean; overload; virtual;
        class function TestSymmetric (const aCryptoAlgoritm: TCryptoAlgoritm;
                                      const aCryptoMode: TCryptoMode) : Boolean; overload; virtual;
        class function TestSymmetric (anArgs: array of const) : Boolean; overload; virtual;
    { гибридное шифрование }
    public
        class function Encrypt (const aValue: String;
                                const aPublicKey: String;
                                const aPrivateKey: String;
                                const aPKCryptoType: TPKCryptoType;
                                const aCryptoType: TCryptoType;
                                const aCryptoMode: TCryptoMode;
                                const aHashType: THashType;
                                const aRandomType: TRandomType = rtSystem) : String; overload; virtual;
        class function Encrypt (const aValue: String;
                                const aPublicKey: String;
                                const aPrivateKey: String;
                                const aPKCryptoType: TPKCryptoType;
                                const aCryptoAlgoritm: TCryptoAlgoritm;
                                const aCryptoMode: TCryptoMode;
                                const aHashAlgoritm: THashAlgoritm;
                                const aRandomType: TRandomType = rtSystem) : String; overload; virtual;
        class function Encrypt (const aValue: String;
                                const aPublicKey: String;
                                const aPrivateKey: String;
                                anArgs: array of const) : String; overload; virtual;
        class function Decrypt (const aValue: String;
                                const aPublicKey: String;
                                const aPrivateKey: String;
                                const aPKCryptoType: TPKCryptoType;
                                const aCryptoType: TCryptoType;
                                const aCryptoMode: TCryptoMode;
                                const aHashType: THashType) : String; overload; virtual;
        class function Decrypt (const aValue: String;
                                const aPublicKey: String;
                                const aPrivateKey: String;
                                const aPKCryptoType: TPKCryptoType;
                                const aCryptoAlgoritm: TCryptoAlgoritm;
                                const aCryptoMode: TCryptoMode;
                                const aHashAlgoritm: THashAlgoritm) : String; overload; virtual;
        class function Decrypt (const aValue: String;
                                const aPublicKey: String;
                                const aPrivateKey: String;
                                anArgs: array of const) : String; overload; virtual;
        class procedure Encrypt (const anInput: TStream;
                                 out anOutput: TStream;
                                 const aPublicKey: String;
                                 const aPrivateKey: String;
                                 const aPKCryptoType: TPKCryptoType;
                                 const aCryptoType: TCryptoType;
                                 const aCryptoMode: TCryptoMode;
                                 const aHashType: THashType;
                                 const aRandomType: TRandomType = rtSystem); overload; virtual;
        class procedure Encrypt (const anInput: TStream;
                                 out anOutput: TStream;
                                 const aPublicKey: String;
                                 const aPrivateKey: String;
                                 const aPKCryptoType: TPKCryptoType;
                                 const aCryptoAlgoritm: TCryptoAlgoritm;
                                 const aCryptoMode: TCryptoMode;
                                 const aHashAlgoritm: THashAlgoritm;
                                 const aRandomType: TRandomType = rtSystem); overload; virtual;
        class procedure Encrypt (const anInput: TStream;
                                 out anOutput: TStream;
                                 const aPublicKey: String;
                                 const aPrivateKey: String;
                                 anArgs: array of const); overload; virtual;
        class procedure Decrypt (const anInput: TStream;
                                 out anOutput: TStream;
                                 const aPublicKey: String;
                                 const aPrivateKey: String;
                                 const aPKCryptoType: TPKCryptoType;
                                 const aCryptoType: TCryptoType;
                                 const aCryptoMode: TCryptoMode;
                                 const aHashType: THashType); overload; virtual;
        class procedure Decrypt (const anInput: TStream;
                                 out anOutput: TStream;
                                 const aPublicKey: String;
                                 const aPrivateKey: String;
                                 const aPKCryptoType: TPKCryptoType;
                                 const aCryptoAlgoritm: TCryptoAlgoritm;
                                 const aCryptoMode: TCryptoMode;
                                 const aHashAlgoritm: THashAlgoritm); overload; virtual;
        class procedure Decrypt (const anInput: TStream;
                                 out anOutput: TStream;
                                 const aPublicKey: String;
                                 const aPrivateKey: String;
                                 anArgs: array of const); overload; virtual;
    end;
{$M-}

{ тесты асимм. шифрования }
{$I 'RSA.test.int.inc'}
{ тесты симм. шифрования }
{$I 'AES.test.int.inc'}
{$I 'Anubis.test.int.inc'}
{$I 'Serpent.test.int.inc'}
{$I 'Shacal.test.int.inc'}
{$I 'BlowFish.test.int.inc'}
{$I 'TwoFish.test.int.inc'}
{ тесты функций хэширования }
{$I 'SHA.test.int.inc'}
{$I 'Tiger.test.int.inc'}
{$I 'MD.test.int.inc'}
{$I 'RipeMD.test.int.inc'}

{ ошибки генераторов псевдо-случайных чисел }
{$I 'PRNG.err.inc'}
{ ошибки функций асимм. шифрования }
{$I 'RSA.err.inc'}
{ ошибки функций симм. шифрования }
{$I 'AES.err.inc'}
{$I 'Anubis.err.inc'}
{$I 'Serpent.err.inc'}
{$I 'Shacal.err.inc'}
{$I 'BlowFish.err.inc'}
{$I 'TwoFish.err.inc'}
{ ошибки функций хэширования }
{$I 'SHA.err.inc'}
{$I 'Tiger.err.inc'}
{$I 'MD.err.inc'}
{$I 'RipeMD.err.inc'}

resourcestring
    ERR_TCRYPTOKERNEL                                = 'Ошибка крипто-системы.';
    ERR_TCRYPTOKERNEL_UNKNOWN_RANDOM_TYPE            = 'Неизвестный тип генератора псевдо-случайных чисел!';
    ERR_TCRYPTOKERNEL_UNKNOWN_HASH_TYPE              = 'Неизвестный тип хэш-функции!'; 
    ERR_TCRYPTOKERNEL_UNKNOWN_ASYMMETRIC_CRYPTO_TYPE = 'Неизвестный тип асимметричного шифрования!';
    ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE  = 'Неизвестный тип симметричного шифрования!';
    ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_MODE  = 'Неизвестный режим симметричного шифрования!';
    ERR_TCRYPTOKERNEL_VERIFY_FAILED                  = 'Ошибка проверки цифровой подписи!';
    ERR_TCRYPTOKERNEL_GET_RANDOM_FUNCTION            = 'Ошибка получения функции генератора псевдо-случайных чисел!';
    ERR_TCRYPTOKERNEL_GET_HASH_FUNCTION              = 'Ошибка получения хэш-функции!';
    ERR_TCRYPTOKERNEL_INIT_RANDOM                    = 'Ошибка инициализации генератора псевдо-случайных чисел!';
    ERR_TCRYPTOKERNEL_RANDOM                         = 'Ошибка генератора псевдо-случайных чисел!';
    ERR_TCRYPTOKERNEL_HASH                           = 'Ошибка вычисления хэш-функции!';
    ERR_TCRYPTOKERNEL_HMAC                           = 'Ошибка вычисления конструкции HMAC!';
    ERR_TCRYPTOKERNEL_GENERATE_KEYS                  = 'Ошибка генерации ключевой пары!';
    ERR_TCRYPTOKERNEL_GENERATE_KEY                   = 'Ошибка генерации ключа!';
    ERR_TCRYPTOKERNEL_ENCRYPT                        = 'Ошибка шифрования данных!';
    ERR_TCRYPTOKERNEL_ENCRYPT_ASYMMETRIC             = 'Ошибка асимметричного шифрования данных!';
    ERR_TCRYPTOKERNEL_ENCRYPT_SYMMETRIC              = 'Ошибка симметричного шифрования данных!';
    ERR_TCRYPTOKERNEL_ENCRYPT_HYBRID                 = 'Ошибка гибридного шифрования данных!';
    ERR_TCRYPTOKERNEL_DECRYPT                        = 'Ошибка дешифровки данных!';
    ERR_TCRYPTOKERNEL_DECRYPT_ASYMMETRIC             = 'Ошибка асимметричной дешифровки данных!';
    ERR_TCRYPTOKERNEL_DECRYPT_SYMMETRIC              = 'Ошибка симметричной дешифровки данных!';
    ERR_TCRYPTOKERNEL_DECRYPT_HYBRID                 = 'Ошибка гибридной дешифровки данных!';
    ERR_TCRYPTOKERNEL_TEST_RANDOM                    = 'Ошибка тестирования генератора псевдо-случайных чисел';
    ERR_TCRYPTOKERNEL_TEST_HASH                      = 'Ошибка тестирования хэш-функции!';
    ERR_TCRYPTOKERNEL_TEST_ASYMMETRIC                = 'Ошибка тестирования асимметричного шифра!';
    ERR_TCRYPTOKERNEL_TEST_SYMMETRIC                 = 'Ошибка тестирования симметричного шифра!';

implementation

function ror (x: LongWord; y: Byte) : LongWord; assembler;
asm
  mov   cl,dl
  ror   eax,cl
end;

function rol (x: LongWord; y: Byte) : LongWord; assembler;
asm
  mov   cl,dl
  rol   eax,cl
end;

function ror64 (x: Int64; y: Byte) : Int64;
begin
    Result := (x shr y) or ( x shl (64 - y) );
end;

function Endian (x: LongWord) : LongWord; assembler;
asm
  bswap eax
end;

function Endian64 (x: Int64) : Int64;
begin
    Result :=          (x and $00000000000000ff) shl 56;
    Result := Result + (x and $000000000000ff00) shl 40;
    Result := Result + (x and $0000000000ff0000) shl 24;
    Result := Result + (x and $00000000ff000000) shl 8;
    Result := Result + (x and $000000ff00000000) shr 8;
    Result := Result + (x and $0000ff0000000000) shr 24;
    Result := Result + (x and $00ff000000000000) shr 40;
    Result := Result + (x and $ff00000000000000) shr 56;
end;

{ ключи шифрования }
{$I 'TKey.imp.inc'}

{ реализация генераторов псевдо-случайных чисел }
{$I 'PRNG.imp.inc'}
{ реализация функций асимм. шифрования }
{$I 'RSA.imp.inc'}
{ реализация функций симм. шифрования }
{$I 'AES.imp.inc'}
{$I 'Anubis.imp.inc'}
{$I 'Serpent.imp.inc'}
{$I 'Shacal.imp.inc'}
{$I 'BlowFish.imp.inc'}
{$I 'TwoFish.imp.inc'}
{ реализация функций хэширования }
{$I 'SHA.imp.inc'}
{$I 'Tiger.imp.inc'}
{$I 'MD.imp.inc'}
{$I 'RipeMD.imp.inc'}

{ параметры генераторов псевдо-случайных чисел }
{$I 'TRandomType.imp.inc'}
{ параметры асимм. шифрования }
{$I 'TPKCryptoType.imp.inc'}
{ параметры симм. шифрования }
{$I 'TCryptoMode.imp.inc'}
{$I 'TCryptoType.imp.inc'}
{ параметры функций хэширования }
{$I 'THashType.imp.inc'}

{ TCryptoKernel }
class procedure TCryptoKernel._raise (anArgs: array of const;
                                      const anEGUID: String = '');
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

class procedure TCryptoKernel._raise (anArgs: array of const;
                                      anEGUID: array of const);
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

class function TCryptoKernel.GetRandomFunction (const aRandomType: TRandomType) : TRandomFunction;
begin
    Result := NIL;
    try
        case aRandomType of
            rtSystem  : Result := RandomSystem;
            rtTaus88  : Result := RandomTaus88;
            rtTaus113 : Result := RandomTaus113;
            rtKiss123 : Result := RandomKiss123;
            rtTT800   : Result := RandomTT800;
            rtXOR4096 : Result := RandomXOR4096;
            rtMT19937 : Result := RandomMT19937;
            rtAESR    : Result := RandomAES;
            rtSalsaR  : Result := RandomSalsa;
            rtISAAC   : Result := RandomISAAC;
            else        raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_RANDOM_TYPE);
        end;
    except on E : Exception do
        _raise (['GetRandomFunction',ERR_TCRYPTOKERNEL_GET_RANDOM_FUNCTION,E],
                ['{937D4AC6-7A58-4999-820C-1F5DA0313470}']);
    end;
end;

class procedure TCryptoKernel.InitRandom (const aValue: LongWord);
begin
    try
        if not RandomInit (aValue) then
            raise Exception.Create (ERR_TCRYPTOKERNEL_INIT_RANDOM);
    except on E : Exception do
        _raise (['InitRandom',ERR_TCRYPTOKERNEL_INIT_RANDOM,E],
                ['{B0F1B8ED-E2B7-4836-9E4F-70B387658F26}']);
    end;
end;

class function TCryptoKernel.Random (const aMin: LongWord = 0;
                                     const aMax: LongWord = High (LongWord);
                                     const aRandomType: TRandomType = rtSystem) : LongWord;
var
    RandomFunc : TRandomFunction;
begin
    Result := 0;
    try
        RandomFunc := GetRandomFunction (aRandomType);
        Result := RandomFunc (aMax, aMin);
    except on E : Exception do
        _raise (['Random',ERR_TCRYPTOKERNEL_RANDOM,E],
                ['{1E1DABB4-10D8-49E7-980A-FBFACECAEE7C}']);
    end;
end;

class function TCryptoKernel.Random (anArgs: array of const) : LongWord;
var
    RandomType : TRandomType;
    Min        : LongWord;
    Max        : LongWord;
begin
    Result := 0;
    try
        { первый параметр - левый предел }
        Min := High (LongWord);
        if notEmpty (0,anArgs) then
            Min := toInteger (anArgs [0]);
        { второй параметр - правый предел }
        Max := 0;
        if notEmpty (1,anArgs) then
            Max := toInteger (anArgs [1]);
        { третий параметр - тип генератора псевдо-случайных чисел }
        RandomType := rtSystem;
        if notEmpty (2,anArgs) then
            RandomType := toRandomType (anArgs [2]);
        Result := Random (Min,Max,RandomType);
    except on E : Exception do
        _raise (['Random',ERR_TCRYPTOKERNEL_RANDOM,E],
                ['{69AD691F-7E2E-406A-B045-27A2E31F3AE6}']);
    end;
end;

class function TCryptoKernel.TestRandom (const aRandomType: TRandomType) : Boolean;
begin
    Result := FALSE;
    try
        case aRandomType of
            rtSystem  : Result := TRUE;
            rtTaus88  : Result := taus88_selftest;
            rtTaus113 : Result := taus113_selftest;
            rtKiss123 : Result := kiss123_selftest;
            rtTT800   : Result := tt800_selftest;
            rtXOR4096 : Result := xor4096_selftest;
            rtMT19937 : Result := mt19937_selftest;
            rtAESR    : Result := aesr_selftest;
            rtSalsaR  : Result := salsar_selftest;
            rtISAAC   : Result := isaac_selftest;
            else        raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_RANDOM_TYPE);
        end;
    except on E : Exception do
        _raise (['TestRandom',ERR_TCRYPTOKERNEL_TEST_RANDOM,E],
                ['{196E41A7-34B7-44B4-A624-4F5E8AD2024D}']);
    end;
end;

class function TCryptoKernel.TestRandom (anArgs: array of const) : Boolean;
var
    RandomType : TRandomType;
begin
    Result := FALSE;
    try
        { первый параметр - тип генератора псевдо-случайных чисел }
        RandomType := rtSystem;
        if notEmpty (0,anArgs) then
            RandomType := toRandomType (anArgs [0]);
        Result := TestRandom (RandomType);
    except on E : Exception do
        _raise (['TestRandom',ERR_TCRYPTOKERNEL_TEST_RANDOM,E],
                ['{62949246-809C-483E-AECC-B3015135213F}']);
    end;
end;

class function TCryptoKernel.GetHashFunction (const aHashType: THashType) : THashFunction;
begin
    Result := NIL;
    try
        case aHashType of
            htSHA1      : Result := SHA1;
            htSHA256    : Result := SHA256;
            htSHA384    : Result := SHA384;
            htSHA512    : Result := SHA512;
            htMD5       : Result := MD5;
            htRipeMD128 : Result := RipeMD128;
            htRipeMD160 : Result := RipeMD160;
            htTiger128  : Result := Tiger128;
            htTiger160  : Result := Tiger160;
            htTiger192  : Result := Tiger192;
            else          raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_HASH_TYPE);
        end;
    except on E : Exception do
        _raise (['GetHashFunction',ERR_TCRYPTOKERNEL_GET_HASH_FUNCTION,E],
                ['{22F9A7C4-62BC-4B1C-AE7C-36A49B7AA265}']);
    end;
end;

class function TCryptoKernel.GetHashStreamFunction (const aHashType: THashType) : THashStreamFunction;
begin
    Result := NIL;
    try
        case aHashType of
            htSHA1      : Result := SHA1;
            htSHA256    : Result := SHA256;
            htSHA384    : Result := SHA384;
            htSHA512    : Result := SHA512;
            htMD5       : Result := MD5;
            htRipeMD128 : Result := RipeMD128;
            htRipeMD160 : Result := RipeMD160;
            htTiger128  : Result := Tiger128;
            htTiger160  : Result := Tiger160;
            htTiger192  : Result := Tiger192;
            else          raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_HASH_TYPE);
        end;
    except on E : Exception do
        _raise (['GetHashFunction',ERR_TCRYPTOKERNEL_GET_HASH_FUNCTION,E],
                ['{67A2D9E3-48F9-41A2-A545-3344CD920FAA}']);
    end;
end;

class function TCryptoKernel.Hash (const aValue: String;
                                   const aHashType: THashType) : String;
var
    HashFunc : THashFunction;
begin
    Result := '';
    try
        HashFunc := GetHashFunction (aHashType);
        Result := HashFunc (aValue);
    except on E : Exception do
        _raise (['Hash',ERR_TCRYPTOKERNEL_HASH,E],
                ['{2EFCA76F-7E4D-4EDD-8ECC-AC1DF6464BAB}']);
    end;
end;

class function TCryptoKernel.Hash (const aValue: TStream;
                                   const aHashType: THashType) : String;
var
    HashFunc : THashStreamFunction;
begin
    Result := '';
    try
        HashFunc := GetHashStreamFunction (aHashType);
        Result := HashFunc (aValue);
    except on E : Exception do
        _raise (['Hash',ERR_TCRYPTOKERNEL_HASH,E],
                ['{925C63FC-1308-454C-B60B-69B31F34B179}']);
    end;
end;

class function TCryptoKernel.Hash (const aValue: String;
                                   const aHashAlgoritm: THashAlgoritm) : String;
var
    I : Integer;
    S : String;
begin
    Result := '';
    try
        try
            S := '';
            for I := 0 to High (aHashAlgoritm) do
                S := Format ('%s%s',[ S, Hash (aValue,aHashAlgoritm [I]) ]);
            for I := 0 to High (aHashAlgoritm) do
                S := Hash (S,aHashAlgoritm [I]);
            Result := S;
        finally
            _FillChar ( S, Length (S), $00 );
        end;
    except on E : Exception do
        _raise (['Hash',ERR_TCRYPTOKERNEL_HASH,E],
                ['{BF6E0714-57E4-4143-B37A-8EFE7594BBCC}']);
    end;
end;

class function TCryptoKernel.Hash (const aValue: TStream;
                                   const aHashAlgoritm: THashAlgoritm) : String;
var
    I : Integer;
    S : String;
begin
    Result := '';
    try
        if ( High (aHashAlgoritm) >= 0 ) then
        try
            S := '';
            for I := 0 to High (aHashAlgoritm) do
                S := Format ('%s%s',[ S, Hash (aValue,aHashAlgoritm [I]) ]);
            for I := 0 to High (aHashAlgoritm) do
                S := Hash (S,aHashAlgoritm [I]);
            Result := S;
        finally
            _FillChar ( S, Length (S), $00 );
        end;
    except on E : Exception do
        _raise (['Hash',ERR_TCRYPTOKERNEL_HASH,E],
                ['{CABC4E1E-F229-45CB-B072-9BC744F82035}']);
    end;
end;

class function TCryptoKernel.Hash (const aValue: String;
                                   anArgs: array of const) : String;
var
    HashAlgoritm : THashAlgoritm;
begin
    Result := '';
    try
        { первый параметр - тип хэш-функции }
        SetLength (HashAlgoritm,1);
        HashAlgoritm [0] := htUnknown;
        if notEmpty (0,anArgs) then
            HashAlgoritm := toHashAlgoritm (anArgs [0]);
        Result := Hash (aValue,HashAlgoritm);
    except on E : Exception do
        _raise (['Hash',ERR_TCRYPTOKERNEL_HASH,E],
                ['{B7AD0A57-1908-4F04-A124-196004DF6A97}']);
    end;
end;

class function TCryptoKernel.Hash (const aValue: TStream;
                                   anArgs: array of const) : String;
var
    HashAlgoritm : THashAlgoritm;
begin
    Result := '';
    try
        { первый параметр - тип хэш-функции }
        SetLength (HashAlgoritm,1);
        HashAlgoritm [0] := htUnknown;
        if notEmpty (0,anArgs) then
            HashAlgoritm := toHashAlgoritm (anArgs [0]);
        Result := Hash (aValue,HashAlgoritm);
    except on E : Exception do
        _raise (['Hash',ERR_TCRYPTOKERNEL_HASH,E],
                ['{A8C4E446-D2CF-4DE2-BF20-AA119B5E6CCF}']);
    end;
end;

class function TCryptoKernel.Hash (const aValue: String;
                                   const aSalt: String;
                                   const aHashType: THashType;
                                   const aCount: DWORD = 1024) : String;
var
    S : String;
    I : DWORD;
begin
    Result := '';
    try
        S := aValue;
        try
            for I := 0 to aCount - 1 do
                S := Hash ( Format ('%s%s%d',[S,aSalt,I]), aHashType );
            Result := S;
        finally
            _FillChar ( S, Length (S), $00 );
        end;
    except on E : Exception do
        _raise (['Hash',ERR_TCRYPTOKERNEL_HASH,E],
                ['{666AA78F-8839-4A78-A83D-684C6D8D69AF}']);
    end;
end;

class function TCryptoKernel.Hash (const aValue: String;
                                   const aSalt: String;
                                   const aHashAlgoritm: THashAlgoritm;
                                   const aCount: DWORD = 1024) : String;
var
    S : String;
    I : DWORD;
begin
    Result := '';
    try
        S := aValue;
        try
            for I := 0 to aCount - 1 do
                S := Hash ( Format ('%s%s%d',[S,aSalt,I]), aHashAlgoritm );
            Result := S;
        finally
            _FillChar ( S, Length (S), $00 );
        end;
    except on E : Exception do
        _raise (['Hash',ERR_TCRYPTOKERNEL_HASH,E],
                ['{E1A82855-A004-4E7A-BE5F-DC602368DA1B}']);
    end;
end;

class function TCryptoKernel.Hash (const aValue: String;
                                   const aSalt: String;
                                   anArgs: array of const) : String;
var
    HashAlgoritm : THashAlgoritm;
    Count        : DWORD;
begin
    Result := '';
    try
        { первый параметр - тип хэш-функции }
        SetLength (HashAlgoritm,1);
        HashAlgoritm [0] := htUnknown;
        if notEmpty (0,anArgs) then
            HashAlgoritm := toHashAlgoritm (anArgs [0]);
        { второй параметр - количество итераций }
        Count := 1024;
        if notEmpty (1,anArgs) then
            Count := toInteger (anArgs [1]);
        Result := Hash (aValue,aSalt,HashAlgoritm,Count);
    except on E : Exception do
        _raise (['Hash',ERR_TCRYPTOKERNEL_HASH,E],
                ['{170F782D-51FD-4941-98F8-142B6F79DA86}']);
    end;
end;

class function TCryptoKernel.TestHash (const aHashType: THashType) : Boolean;
begin
    Result := FALSE;
    try
        case aHashType of
            htSHA1      : Result := TestSHA1;
            htSHA256    : Result := TestSHA256;
            htSHA384    : Result := TestSHA384;
            htSHA512    : Result := TestSHA512;
            htMD5       : Result := TestMD5;
            htRipeMD128 : Result := TestRipeMD128;
            htRipeMD160 : Result := TestRipeMD160;
            htTiger128  : Result := TestTiger128;
            htTiger160  : Result := TestTiger160;
            htTiger192  : Result := TestTiger192;
            else          raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_HASH_TYPE);
        end;
    except on E : Exception do
        _raise (['TestHash',ERR_TCRYPTOKERNEL_TEST_HASH,E],
                ['{8886970C-30DA-4FFC-9E55-D0C8E06EEBEA}']);
    end;
end;

class function TCryptoKernel.TestHash (const aHashAlgoritm: THashAlgoritm) : Boolean;
var
    I : Integer;
    B : Boolean;
begin
    Result := FALSE;
    try
        B := TRUE;
        for I := 0 to High (aHashAlgoritm) do
            B := B and TestHash (aHashAlgoritm [I]);
        Result := B;
    except on E : Exception do
        _raise (['TestHash',ERR_TCRYPTOKERNEL_TEST_HASH,E],
                ['{71D63EA9-D80C-4662-98BA-B45115C900C7}']);
    end;
end;

class function TCryptoKernel.TestHash (anArgs: array of const) : Boolean;
var
    HashAlgoritm : THashAlgoritm;
begin
    Result := FALSE;
    try
        { первый параметр - тип хэш-функции }
        SetLength (HashAlgoritm,1);
        HashAlgoritm [0] := htUnknown;
        if notEmpty (0,anArgs) then
            HashAlgoritm := toHashAlgoritm (anArgs [0]);
        Result := TestHash (HashAlgoritm);
    except on E : Exception do
        _raise (['TestHash',ERR_TCRYPTOKERNEL_TEST_HASH,E],
                ['{F32D9522-1749-4F68-ABB5-569794B9BC44}']);
    end;
end;

class function TCryptoKernel.HMAC (const aValue: String;
                                   const aKey: Hex;
                                   const aHashType: THashType) : String;
var
    BlockSize : WORD;
    I         : Integer;
    Key       : String;
    iKeyPad   : String;
    oKeyPad   : String;
begin
    Result := '';
    try
        try
            case aHashType of
                htSHA1      : BlockSize := 20;
                htSHA256    : BlockSize := 32;
                htSHA384    : BlockSize := 48;
                htSHA512    : BlockSize := 64;
                htMD5       : BlockSize := 20;
                htRipeMD128 : BlockSize := 16;
                htRipeMD160 : BlockSize := 20;
                htTiger128  : BlockSize := 20;
                htTiger160  : BlockSize := 20;
                htTiger192  : BlockSize := 24;
                else          raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_HASH_TYPE);
            end;
            Key := aKey;
            if ( Length (Key) > BlockSize ) then
                Key := Hash (Key,aHashType);
            if ( Length (Key) < BlockSize ) then
                for I := Length (Key) to BlockSize do
                    Key := Key + Chr ($00);
            SetLength (iKeyPad,BlockSize);
            SetLength (oKeyPad,BlockSize);
            for I := 1 to BlockSize do
            begin
                iKeyPad [I] := Chr ( Ord (Key [I]) xor $36 );
                oKeyPad [I] := Chr ( Ord (Key [I]) xor $5c );
            end;
            Result := Hash (  oKeyPad + Hash ( (iKeyPad + aValue), aHashType ), aHashType  );
        finally
            _FillChar ( Key, Length (Key), $00 );
            _FillChar ( iKeyPad, Length (iKeyPad), $00 );
            _FillChar ( oKeyPad, Length (oKeyPad), $00 );
            BlockSize := 0;
        end;
    except on E : Exception do
        _raise (['HMAC',ERR_TCRYPTOKERNEL_HMAC,E],
                ['{E1C690A6-3074-4E27-AB87-0540F1526481}']);
    end;
end;

class function TCryptoKernel.HMAC (const aValue: String;
                                   const aKey: Hex;
                                   const aHashAlgoritm: THashAlgoritm) : String;
var
    I : Integer;
    S : String;
begin
    Result := '';
    try
        S := aValue;
        try
            for I := 0 to High (aHashAlgoritm) do
                S := HMAC (S,aKey,aHashAlgoritm [I]);
            Result := S;
        finally
            _FillChar ( S, Length (S), $00 );
        end;
    except on E : Exception do
        _raise (['HMAC',ERR_TCRYPTOKERNEL_HMAC,E],
                ['{4FC554C9-DFFA-4821-AFB0-EFFCFA37AA75}']);
    end;
end;

class function TCryptoKernel.HMAC (const aValue: String;
                                   const aKey: Hex;
                                   anArgs: array of const) : String;
var
    HashAlgoritm : THashAlgoritm;
begin
    Result := '';
    try
        { первый параметр - тип хэш-функции }
        SetLength (HashAlgoritm,1);
        HashAlgoritm [0] := htUnknown;
        if notEmpty (0,anArgs) then
            HashAlgoritm := toHashAlgoritm (anArgs [0]);
        Result := HMAC (aValue,aKey,HashAlgoritm);
    except on E : Exception do
        _raise (['HMAC',ERR_TCRYPTOKERNEL_HMAC,E],
                ['{650FE1A7-1CB0-4980-B162-32854CEA8EFF}']);
    end;
end;

class function TCryptoKernel.HMAC (const aValue: String;
                                   const aKey: Hex;
                                   const aSalt: String;
                                   const aHashType: THashType;
                                   const aCount: DWORD = 1024) : String;
var
    BlockSize : WORD;
    I         : Integer;
    Key       : String;
    iKeyPad   : String;
    oKeyPad   : String;
begin
    Result := '';
    try
        try
            case aHashType of
                htSHA1      : BlockSize := 20;
                htSHA256    : BlockSize := 32;
                htSHA384    : BlockSize := 48;
                htSHA512    : BlockSize := 64;
                htMD5       : BlockSize := 20;
                htRipeMD128 : BlockSize := 16;
                htRipeMD160 : BlockSize := 20;
                htTiger128  : BlockSize := 20;
                htTiger160  : BlockSize := 20;
                htTiger192  : BlockSize := 24;
                else          raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_HASH_TYPE);
            end;
            Key := aKey;
            if ( Length (Key) > BlockSize ) then
                Key := Hash (Key,aSalt,aHashType,aCount);
            if ( Length (Key) < BlockSize ) then
                for I := Length (Key) to BlockSize do
                    Key := Key + Chr ($00);
            SetLength (iKeyPad,BlockSize);
            SetLength (oKeyPad,BlockSize);
            for I := 1 to BlockSize do
            begin
                iKeyPad [I] := Chr ( Ord (Key [I]) xor $36 );
                oKeyPad [I] := Chr ( Ord (Key [I]) xor $5c );
            end;
            Result := Hash (  oKeyPad + Hash ( (iKeyPad + aValue), aSalt, aHashType, aCount ), 
                              aSalt, 
                              aHashType, 
                              aCount  );
        finally
            _FillChar ( Key, Length (Key), $00 );
            _FillChar ( iKeyPad, Length (iKeyPad), $00 );
            _FillChar ( oKeyPad, Length (oKeyPad), $00 );
            BlockSize := 0;
        end;
    except on E : Exception do
        _raise (['HMAC',ERR_TCRYPTOKERNEL_HMAC,E],
                ['{B2612490-9EF5-43FF-892F-4B70BDF8758C}']);
    end;
end;

class function TCryptoKernel.HMAC (const aValue: String;
                                   const aKey: Hex;
                                   const aSalt: String;
                                   const aHashAlgoritm: THashAlgoritm;
                                   const aCount: DWORD = 1024) : String;
var
    I : Integer;
    S : String;
begin
    Result := '';
    try
        S := aValue;
        try
            for I := 0 to High (aHashAlgoritm) do
                S := HMAC (S,aKey,aSalt,aHashAlgoritm [I],aCount);
            Result := S;
        finally
            _FillChar ( S, Length (S), $00 );
        end;
    except on E : Exception do
        _raise (['HMAC',ERR_TCRYPTOKERNEL_HMAC,E],
                ['{DFB87ECF-B567-40E6-A247-90FC1ED09C55}']);
    end;
end;

class function TCryptoKernel.HMAC (const aValue: String;
                                   const aKey: Hex;
                                   const aSalt: String;
                                   anArgs: array of const) : String;
var
    HashAlgoritm : THashAlgoritm;
    Count        : DWORD;
begin
    Result := '';
    try
        { первый параметр - тип хэш-функции }
        SetLength (HashAlgoritm,1);
        HashAlgoritm [0] := htUnknown;
        if notEmpty (0,anArgs) then
            HashAlgoritm := toHashAlgoritm (anArgs [0]);
        { второй параметр - количество итераций }
        Count := 1024;
        if notEmpty (1,anArgs) then
            Count := toInteger (anArgs [1]);
        Result := HMAC (aValue,aKey,aSalt,HashAlgoritm,Count);
    except on E : Exception do
        _raise (['HMAC',ERR_TCRYPTOKERNEL_HMAC,E],
                ['{EE7892B9-D2EA-4EDD-8BED-A2CD9372B1CF}']);
    end;
end;

class procedure TCryptoKernel.GenerateRSAKeys (out aPublicKey: String;
                                               out aPrivateKey: String;
                                               const aPKCryptoType: TPKCryptoType = pkctRSA1024;
                                               const aRandomType: TRandomType = rtSystem);
var
    Size       : WORD;
    RandomFunc : TRandomFunction;
begin
    aPublicKey := '';
    aPrivateKey := '';
    try
        case aPKCryptoType of
            pkctRSA1024 : Size := 1024;
            pkctRSA2048 : Size := 2048;
            pkctRSA4096 : Size := 4096;
            pkctRSA8192 : Size := 8192;
            else          raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_ASYMMETRIC_CRYPTO_TYPE);
        end;
        RandomFunc := GetRandomFunction (aRandomType);
        GenKeysRSA (Size,RandomFunc,aPublicKey,aPrivateKey);
    except on E : Exception do
        _raise (['GenerateRSAKeys',ERR_TCRYPTOKERNEL_GENERATE_KEYS,E],
                ['{93F9D253-3977-4798-B9EE-25D6338157BE}']);
    end;
end;

class procedure TCryptoKernel.GenerateKeys (out aPublicKey: String;
                                            out aPrivateKey: String;
                                            const aPKCryptoType: TPKCryptoType;
                                            const aRandomType: TRandomType = rtSystem);
begin
    aPublicKey := '';
    aPrivateKey := '';
    try
        case aPKCryptoType of
            pkctRSA1024,
            pkctRSA2048,
            pkctRSA4096,
            pkctRSA8192 : GenerateRSAKeys (aPublicKey,aPrivateKey,aPKCryptoType,aRandomType);
            else          raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_ASYMMETRIC_CRYPTO_TYPE);
        end;
    except on E : Exception do
        _raise (['GenerateKeys',ERR_TCRYPTOKERNEL_GENERATE_KEYS,E],
                ['{E436ABB7-C6C1-4CB5-A579-2DDBF061659D}']);
    end;
end;

class procedure TCryptoKernel.GenerateKeys (out aPublicKey: String;
                                            out aPrivateKey: String;
                                            anArgs: array of const);
var
    PKCryptoType : TPKCryptoType;
    RandomType   : TRandomType;
begin
    aPublicKey := '';
    aPrivateKey := '';
    try
        { первый параметр - тип асимм. шифрования }
        PKCryptoType := pkctUnknown;
        if notEmpty (0,anArgs) then
            PKCryptoType := toPKCryptoType (anArgs [0]);
        { второй параметр - тип генератора псевдо-случайных чисел }
        RandomType := rtSystem;
        if notEmpty (1,anArgs) then
            RandomType := toRandomType (anArgs [1]);
        GenerateKeys (aPublicKey,aPrivateKey,PKCryptoType,RandomType);
    except on E : Exception do
        _raise (['GenerateKeys',ERR_TCRYPTOKERNEL_GENERATE_KEYS,E],
                ['{CF69EC88-F98C-4C57-B110-89642B95692B}']);
    end;
end;

class function TCryptoKernel.Encrypt (const aValue: String;
                                      const aPublicKey: String;
                                      const aPKCryptoType: TPKCryptoType) : String;
begin
    Result := '';
    try
        case aPKCryptoType of
            pkctRSA1024,
            pkctRSA2048,
            pkctRSA4096,
            pkctRSA8192 : Result := EncryptRSA (aPublicKey,aValue);
            else          raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_ASYMMETRIC_CRYPTO_TYPE);
        end;
    except on E : Exception do
        _raise (['Encrypt',ERR_TCRYPTOKERNEL_ENCRYPT_ASYMMETRIC,E],
                ['{89113B54-C966-4FEC-87F3-D1C06FA06741}']);
    end;
end;

class function TCryptoKernel.Decrypt (const aValue: String;
                                      const aPrivateKey: String;
                                      const aPKCryptoType: TPKCryptoType) : String;
begin
    Result := '';
    try
        case aPKCryptoType of
            pkctRSA1024,
            pkctRSA2048,
            pkctRSA4096,
            pkctRSA8192 : Result := DecryptRSA (aPrivateKey,aValue);
            else          raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_ASYMMETRIC_CRYPTO_TYPE);
        end;
    except on E : Exception do
        _raise (['Decrypt',ERR_TCRYPTOKERNEL_DECRYPT_ASYMMETRIC,E],
                ['{79A42269-425B-4923-BEA2-DE06024561C5}']);
    end;
end;

class function TCryptoKernel.TestAsymmetric (const aPKCryptoType: TPKCryptoType) : Boolean;
begin
    Result := FALSE;
    try
        case aPKCryptoType of
            pkctRSA1024,
            pkctRSA2048,
            pkctRSA4096,
            pkctRSA8192 : Result := TestRSA;
            else          raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_ASYMMETRIC_CRYPTO_TYPE);
        end;
    except on E : Exception do
        _raise (['TestAsymmetric',ERR_TCRYPTOKERNEL_TEST_ASYMMETRIC,E],
                ['{192D42B3-BC1B-4B2F-9969-9D6DF8A72B3B}']);
    end;
end;

class function TCryptoKernel.TestAsymmetric (anArgs: array of const) : Boolean;
var
    PKCryptoType : TPKCryptoType;
begin
    Result := FALSE;
    try
        { первый параметр - асимм. шифр }
        PKCryptoType := pkctUnknown;
        if notEmpty (0,anArgs) then
            PKCryptoType := toPKCryptoType (anArgs [0]);
        Result := TestAsymmetric (PKCryptoType);
    except on E : Exception do
        _raise (['TestAsymmetric',ERR_TCRYPTOKERNEL_TEST_ASYMMETRIC,E],
                ['{06F4FDFC-8973-4D74-9129-C4CC0A53FA10}']);
    end;
end;

class function TCryptoKernel.Encrypt (const aValue: String;
                                      const aPublicKey: String;
                                      const aPrivateKey: String;
                                      const aPKCryptoType: TPKCryptoType;
                                      const aHashType: THashType) : String;
var
    Msg : String;
    Sgn : String;
begin
    Result := '';
    try
        try
            Msg := aValue;
            { s := ( h(m) )^d mod n }
            Sgn := Encrypt (  Hash (Msg, aHashType),
                              aPrivateKey,
                              aPKCryptoType  );
            { c := (ms)^e mod n }
            Result := Encrypt ( Format ('%s%s%s%s',[ IntToHex ( Length (Msg), 8 ), Msg,
                                                     IntToHex ( Length (Sgn), 4 ), Sgn ]),
                                aPublicKey,
                                aPKCryptoType );
        finally
            _FillChar ( Msg, Length (Msg), $00 );
            _FillChar ( Sgn, Length (Sgn), $00 );
        end;
    except on E : Exception do
        _raise (['Encrypt',ERR_TCRYPTOKERNEL_ENCRYPT_ASYMMETRIC,E],
                ['{41A38C73-EE78-409D-89FF-DD297E695560}']);
    end;
end;

class function TCryptoKernel.Encrypt (const aValue: String;
                                      const aPublicKey: String;
                                      const aPrivateKey: String;
                                      const aPKCryptoType: TPKCryptoType;
                                      const aHashAlgoritm: THashAlgoritm) : String;
var
    Msg : String;
    Sgn : String;
begin
    Result := '';
    try
        try
            Msg := aValue;
            { s := ( h(m) )^d mod n }
            Sgn := Encrypt (  Hash (Msg,aHashAlgoritm),
                              aPrivateKey,
                              aPKCryptoType  );
            { c := (ms)^e mod n }
            Result := Encrypt ( Format ('%s%s%s%s',[ IntToHex ( Length (Msg), 8 ), Msg,
                                                     IntToHex ( Length (Sgn), 4 ), Sgn ]),
                                aPublicKey,
                                aPKCryptoType );
        finally
            _FillChar ( Msg, Length (Msg), $00 );
            _FillChar ( Sgn, Length (Sgn), $00 );
        end;
    except on E : Exception do
        _raise (['Encrypt',ERR_TCRYPTOKERNEL_ENCRYPT_ASYMMETRIC,E],
                ['{E2373BD9-E01A-4D46-B7F0-89A3F1618C04}']);
    end;
end;

class function TCryptoKernel.Decrypt (const aValue: String;
                                      const aPublicKey: String;
                                      const aPrivateKey: String;
                                      const aPKCryptoType: TPKCryptoType;
                                      const aHashType: THashType) : String;
var
    MsgSgn    : String;
    Msg       : String;
    MsgLength : Int64;
    Sgn       : String;
    SgnLength : WORD;
    Vrf       : String;
begin
    Result := '';
    try
        try
            { (m+s) := c^d mod n }
            MsgSgn := Decrypt (aValue,aPrivateKey,aPKCryptoType);
            MsgLength := HexToInt ( Copy (MsgSgn,1,8) );
            Msg := Copy (MsgSgn,9,MsgLength);
            SgnLength := HexToInt ( Copy (MsgSgn,MsgLength+9,4) );
            Sgn := Copy (MsgSgn,MsgLength+13,SgnLength);
            { v := s^e mod n }
            Vrf := Decrypt (Sgn,aPublicKey,aPKCryptoType);
            { v == h (m) }
            if ( Vrf = Hash (Msg,aHashType) ) then
                Result := Msg
            else
                raise Exception.Create (ERR_TCRYPTOKERNEL_VERIFY_FAILED);
        finally
            _FillChar ( MsgSgn, Length (MsgSgn), $00 );
            _FillChar ( Msg, Length (Msg), $00 );
            _FillChar ( Sgn, Length (Sgn), $00 );
            _FillChar ( Vrf, Length (Vrf), $00 );
        end;
    except on E : Exception do
        _raise (['Decrypt',ERR_TCRYPTOKERNEL_DECRYPT_ASYMMETRIC,E],
                ['{7AEA9489-26F1-4E05-9B5C-905FAE3A4968}']);
    end;
end;

class function TCryptoKernel.Decrypt (const aValue: String;
                                      const aPublicKey: String;
                                      const aPrivateKey: String;
                                      const aPKCryptoType: TPKCryptoType;
                                      const aHashAlgoritm: THashAlgoritm) : String;
var
    MsgSgn    : String;
    Msg       : String;
    MsgLength : Int64;
    Sgn       : String;
    SgnLength : WORD;
    Vrf       : String;
begin
    Result := '';
    try
        try
            { (m+s) := c^d mod n }
            MsgSgn := Decrypt (aValue,aPrivateKey,aPKCryptoType);
            MsgLength := HexToInt ( Copy (MsgSgn,1,8) );
            Msg := Copy (MsgSgn,9,MsgLength);
            SgnLength := HexToInt ( Copy (MsgSgn,MsgLength+9,4) );
            Sgn := Copy (MsgSgn,MsgLength+13,SgnLength);
            { v := s^e mod n }
            Vrf := Decrypt (Sgn,aPublicKey,aPKCryptoType);
            { v == h (m) }
            if ( Vrf = Hash (Msg,aHashAlgoritm) ) then
                Result := Msg
            else
                raise Exception.Create (ERR_TCRYPTOKERNEL_VERIFY_FAILED);
        finally
            _FillChar ( MsgSgn, Length (MsgSgn), $00 );
            _FillChar ( Msg, Length (Msg), $00 );
            _FillChar ( Sgn, Length (Sgn), $00 );
            _FillChar ( Vrf, Length (Vrf), $00 );
        end;
    except on E : Exception do
        _raise (['Decrypt',ERR_TCRYPTOKERNEL_DECRYPT_ASYMMETRIC,E],
                ['{753BD95A-3B2B-46E5-8FBF-C0923E8B37C3}']);
    end;
end;

class procedure TCryptoKernel.GenerateKey (out aKey: String;
                                           const aCryptoType: TCryptoType;
                                           const aRandomType: TRandomType = rtSystem);
var
    oSize : WORD;
    I     : WORD;
    R     : Byte;
begin
    aKey := '';
    try
        try
            case aCryptoType of
                ctAES128     : oSize := 16;
                ctAES192     : oSize := 24;
                ctAES256     : oSize := 32;
                ctAnubis128  : oSize := 16;
                ctAnubis192  : oSize := 24;
                ctAnubis256  : oSize := 32;
                ctSerpent128 : oSize := 16;
                ctSerpent192 : oSize := 24;
                ctSerpent256 : oSize := 32;
                ctShacal     : oSize := 64;
                ctBlowFish   : oSize := 16;
                ctTwoFish128 : oSize := 16;
                ctTwoFish192 : oSize := 24;
                ctTwoFish256 : oSize := 32;
                else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
            end;
            for I := 0 to oSize*2 - 1 do
            begin
                R := Random (16,0,aRandomType);
                aKey := aKey + IntToHex (R,1);
            end;
        finally
            oSize := 0;
            R := 0;
        end;
    except on E : Exception do
        _raise (['GenerateKey',ERR_TCRYPTOKERNEL_GENERATE_KEY,E],
                ['{98AE9D02-EF75-472D-8BC6-099DE7051E69}']);
    end;
end;

class procedure TCryptoKernel.GenerateKey (out aKey: String;
                                           const aCryptoAlgoritm: TCryptoAlgoritm;
                                           const aRandomType: TRandomType = rtSystem);
var
    I        : Integer;
    oSize    : WORD;
    oKeySize : WORD;
    R        : Byte;
begin
    aKey := '';
    try
        try
            oKeySize := 0;
            for I := 0 to High (aCryptoAlgoritm) do
            begin
                case aCryptoAlgoritm [I] of
                    ctAES128     : oSize := 16;
                    ctAES192     : oSize := 24;
                    ctAES256     : oSize := 32;
                    ctAnubis128  : oSize := 16;
                    ctAnubis192  : oSize := 24;
                    ctAnubis256  : oSize := 32;
                    ctSerpent128 : oSize := 16;
                    ctSerpent192 : oSize := 24;
                    ctSerpent256 : oSize := 32;
                    ctShacal     : oSize := 64;
                    ctBlowFish   : oSize := 16;
                    ctTwoFish128 : oSize := 16;
                    ctTwoFish192 : oSize := 24;
                    ctTwoFish256 : oSize := 32;
                    else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                end;
                if ( oSize > oKeySize ) then
                    oKeySize := oSize;
            end;
            for I := 0 to oKeySize*2 - 1 do
            begin
                R := Random (16,0,aRandomType);
                aKey := aKey + IntToHex (R,1);
            end;
        finally
            oSize := 0;
            oKeySize := 0;
            R := 0;
        end;
    except on E : Exception do
        _raise (['GenerateKey',ERR_TCRYPTOKERNEL_GENERATE_KEY,E],
                ['{5440FC49-8E8E-4DEF-9ADA-27B7087D9B8B}']);
    end;
end;

class procedure TCryptoKernel.GenerateKey (out aKey: String;
                                           anArgs: array of const);
var
    CryptoAlgoritm : TCryptoAlgoritm;
    RandomType     : TRandomType;
begin
    aKey := '';
    try
        { первый параметр - симм. шифр }
        SetLength (CryptoAlgoritm,1);
        CryptoAlgoritm [0] := ctUnknown;
        if notEmpty (0,anArgs) then
            CryptoAlgoritm := toCryptoAlgoritm (anArgs [0]);
        { второрй параметр - тип генератора псевдо-случайных чисел }
        RandomType := rtSystem;
        if notEmpty (1,anArgs) then
            RandomType := toRandomType (anArgs [1]);
        GenerateKey (aKey,CryptoAlgoritm,RandomType);
    except on E : Exception do
        _raise (['GenerateKey',ERR_TCRYPTOKERNEL_GENERATE_KEY,E],
                ['{0ABEA142-5C54-4A40-AFC9-24C026EB3D0E}']);
    end;
end;

class function TCryptoKernel.Encrypt (const aValue: String;
                                      const aKey: Hex;
                                      const aCryptoType: TCryptoType;
                                      const aCryptoMode: TCryptoMode;
                                      const aRandomType: TRandomType = rtSystem) : String;
var
    Key        : Hex;
    RandomFunc : TRandomFunction;
begin
    Result := '';
    try
        try
            case aCryptoType of
                ctAES128     : Key := rpad (aKey,16*2,'0');
                ctAES192     : Key := rpad (aKey,24*2,'0');
                ctAES256     : Key := rpad (aKey,32*2,'0');
                ctAnubis128  : Key := rpad (aKey,16*2,'0');
                ctAnubis192  : Key := rpad (aKey,24*2,'0');
                ctAnubis256  : Key := rpad (aKey,32*2,'0');
                ctSerpent128 : Key := rpad (aKey,16*2,'0');
                ctSerpent192 : Key := rpad (aKey,24*2,'0');
                ctSerpent256 : Key := rpad (aKey,32*2,'0');
                ctShacal     : Key := rpad (aKey,64*2,'0');
                ctBlowFish   : Key := rpad (aKey,16*2,'0');
                ctTwoFish128 : Key := rpad (aKey,16*2,'0');
                ctTwoFish192 : Key := rpad (aKey,24*2,'0');
                ctTwoFish256 : Key := rpad (aKey,32*2,'0');
                else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
            end;
            RandomFunc := GetRandomFunction (aRandomType);
            case aCryptoMode of
                {$IFDEF ECB}
                cmECB : case aCryptoType of
                            ctAES128     : Result := EncryptAESECB (Key,aValue);
                            ctAES192     : Result := EncryptAESECB (Key,aValue);
                            ctAES256     : Result := EncryptAESECB (Key,aValue);
                            ctAnubis128  : Result := EncryptAnubisECB (Key,aValue);
                            ctAnubis192  : Result := EncryptAnubisECB (Key,aValue);
                            ctAnubis256  : Result := EncryptAnubisECB (Key,aValue);
                            ctSerpent128 : Result := EncryptSerpentECB (Key,aValue);
                            ctSerpent192 : Result := EncryptSerpentECB (Key,aValue);
                            ctSerpent256 : Result := EncryptSerpentECB (Key,aValue);
                            ctShacal     : Result := EncryptShacalECB (Key,aValue);
                            ctBlowFish   : Result := EncryptBlowFishECB (Key,aValue);
                            ctTwoFish128 : Result := EncryptSerpentECB (Key,aValue);
                            ctTwoFish192 : Result := EncryptTwoFishECB (Key,aValue);
                            ctTwoFish256 : Result := EncryptTwoFishECB (Key,aValue);
                            else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                        end;
                {$ENDIF ECB}
                {$IFDEF CBC}
                cmCBC : case aCryptoType of
                            ctAES128     : Result := EncryptAESCBC (Key,RandomFunc,aValue);
                            ctAES192     : Result := EncryptAESCBC (Key,RandomFunc,aValue);
                            ctAES256     : Result := EncryptAESCBC (Key,RandomFunc,aValue);
                            ctAnubis128  : Result := EncryptAnubisCBC (Key,RandomFunc,aValue);
                            ctAnubis192  : Result := EncryptAnubisCBC (Key,RandomFunc,aValue);
                            ctAnubis256  : Result := EncryptAnubisCBC (Key,RandomFunc,aValue);
                            ctSerpent128 : Result := EncryptSerpentCBC (Key,RandomFunc,aValue);
                            ctSerpent192 : Result := EncryptSerpentCBC (Key,RandomFunc,aValue);
                            ctSerpent256 : Result := EncryptSerpentCBC (Key,RandomFunc,aValue);
                            ctShacal     : Result := EncryptShacalCBC (Key,RandomFunc,aValue);
                            ctBlowFish   : Result := EncryptBlowFishCBC (Key,RandomFunc,aValue);
                            ctTwoFish128 : Result := EncryptSerpentCBC (Key,RandomFunc,aValue);
                            ctTwoFish192 : Result := EncryptTwoFishCBC (Key,RandomFunc,aValue);
                            ctTwoFish256 : Result := EncryptTwoFishCBC (Key,RandomFunc,aValue);
                            else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                        end;
                {$ENDIF CBC}
                {$IFDEF CFB}
                cmCFB : case aCryptoType of
                            ctAES128     : Result := EncryptAESCFB (Key,RandomFunc,aValue);
                            ctAES192     : Result := EncryptAESCFB (Key,RandomFunc,aValue);
                            ctAES256     : Result := EncryptAESCFB (Key,RandomFunc,aValue);
                            ctAnubis128  : Result := EncryptAnubisCFB (Key,RandomFunc,aValue);
                            ctAnubis192  : Result := EncryptAnubisCFB (Key,RandomFunc,aValue);
                            ctAnubis256  : Result := EncryptAnubisCFB (Key,RandomFunc,aValue);
                            ctSerpent128 : Result := EncryptSerpentCFB (Key,RandomFunc,aValue);
                            ctSerpent192 : Result := EncryptSerpentCFB (Key,RandomFunc,aValue);
                            ctSerpent256 : Result := EncryptSerpentCFB (Key,RandomFunc,aValue);
                            ctShacal     : Result := EncryptShacalCFB (Key,RandomFunc,aValue);
                            ctBlowFish   : Result := EncryptBlowFishCFB (Key,RandomFunc,aValue);
                            ctTwoFish128 : Result := EncryptSerpentCFB (Key,RandomFunc,aValue);
                            ctTwoFish192 : Result := EncryptTwoFishCFB (Key,RandomFunc,aValue);
                            ctTwoFish256 : Result := EncryptTwoFishCFB (Key,RandomFunc,aValue);
                            else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                        end;
                {$ENDIF CFB}
                {$IFDEF OFB}
                cmOFB : case aCryptoType of
                            ctAES128     : Result := EncryptAESOFB (Key,RandomFunc,aValue);
                            ctAES192     : Result := EncryptAESOFB (Key,RandomFunc,aValue);
                            ctAES256     : Result := EncryptAESOFB (Key,RandomFunc,aValue);
                            ctAnubis128  : Result := EncryptAnubisOFB (Key,RandomFunc,aValue);
                            ctAnubis192  : Result := EncryptAnubisOFB (Key,RandomFunc,aValue);
                            ctAnubis256  : Result := EncryptAnubisOFB (Key,RandomFunc,aValue);
                            ctSerpent128 : Result := EncryptSerpentOFB (Key,RandomFunc,aValue);
                            ctSerpent192 : Result := EncryptSerpentOFB (Key,RandomFunc,aValue);
                            ctSerpent256 : Result := EncryptSerpentOFB (Key,RandomFunc,aValue);
                            ctShacal     : Result := EncryptShacalOFB (Key,RandomFunc,aValue);
                            ctBlowFish   : Result := EncryptBlowFishOFB (Key,RandomFunc,aValue);
                            ctTwoFish128 : Result := EncryptSerpentOFB (Key,RandomFunc,aValue);
                            ctTwoFish192 : Result := EncryptTwoFishOFB (Key,RandomFunc,aValue);
                            ctTwoFish256 : Result := EncryptTwoFishOFB (Key,RandomFunc,aValue);
                            else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                        end;
                {$ENDIF OFB}
                else    raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_MODE);
            end;
        finally
            _FillChar ( Key, Length (Key), $00 );
        end;
    except on E : Exception do
        _raise (['Encrypt',ERR_TCRYPTOKERNEL_ENCRYPT_SYMMETRIC,E],
                ['{8CB9F439-E97A-4896-96AB-5C65E0841F04}']);
    end;
end;

class function TCryptoKernel.Encrypt (const aValue: String;
                                      const aKey: Hex;
                                      const aCryptoAlgoritm: TCryptoAlgoritm;
                                      const aCryptoMode: TCryptoMode;
                                      const aRandomType: TRandomType = rtSystem) : String;
var
    I : Integer;
    S : String;
begin
    Result := '';
    try
        S := aValue;
        try
            for I := 0 to High (aCryptoAlgoritm) do
                S := Encrypt (S,aKey,aCryptoAlgoritm [I],aCryptoMode,aRandomType);
            Result := S;
        finally
            _FillChar ( S, Length (S), $00 );
        end;
    except on E : Exception do
        _raise (['Encrypt',ERR_TCRYPTOKERNEL_ENCRYPT_SYMMETRIC,E],
                ['{8592E17B-2120-4D92-AE89-E9A256195411}']);
    end;
end;

class function TCryptoKernel.Encrypt (const aValue: String;
                                      const aKey: Hex;
                                      anArgs: array of const) : String;
var
    CryptoAlgoritm : TCryptoAlgoritm;
    CryptoMode     : TCryptoMode;
    RandomType     : TRandomType;
begin
    Result := '';
    try
        { первый параметр - симм. шифр }
        SetLength (CryptoAlgoritm,1);
        CryptoAlgoritm [0] := ctUnknown;
        if notEmpty (0,anArgs) then
            CryptoAlgoritm := toCryptoAlgoritm (anArgs [0]);
        { второрй параметр - режим симм. шифрования }
        CryptoMode := cmUnknown;
        if notEmpty (1,anArgs) then
            CryptoMode := toCryptoMode (anArgs [1]);
        { третий параметр - тип генератора псевдо-случайных чисел }
        RandomType := rtSystem;
        if notEmpty (2,anArgs) then
            RandomType := toRandomType (anArgs [2]);
        Result := Encrypt (aValue,
                           aKey,
                           CryptoAlgoritm,
                           CryptoMode,
                           RandomType);
    except on E : Exception do
        _raise (['Encrypt',ERR_TCRYPTOKERNEL_ENCRYPT_SYMMETRIC,E],
                ['{8E2051A2-3A0D-46E8-9BCB-3772CA53D5DD}']);
    end;
end;

class function TCryptoKernel.Decrypt (const aValue: String;
                                      const aKey: Hex;
                                      const aCryptoType: TCryptoType;
                                      const aCryptoMode: TCryptoMode) : String;
var
    Key : Hex;
begin
    Result := '';
    try
        try
            case aCryptoType of
                ctAES128     : Key := rpad (aKey,16*2,'0');
                ctAES192     : Key := rpad (aKey,24*2,'0');
                ctAES256     : Key := rpad (aKey,32*2,'0');
                ctAnubis128  : Key := rpad (aKey,16*2,'0');
                ctAnubis192  : Key := rpad (aKey,24*2,'0');
                ctAnubis256  : Key := rpad (aKey,32*2,'0');
                ctSerpent128 : Key := rpad (aKey,16*2,'0');
                ctSerpent192 : Key := rpad (aKey,24*2,'0');
                ctSerpent256 : Key := rpad (aKey,32*2,'0');
                ctShacal     : Key := rpad (aKey,64*2,'0');
                ctBlowFish   : Key := rpad (aKey,16*2,'0');
                ctTwoFish128 : Key := rpad (aKey,16*2,'0');
                ctTwoFish192 : Key := rpad (aKey,24*2,'0');
                ctTwoFish256 : Key := rpad (aKey,32*2,'0');
                else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
            end;
            case aCryptoMode of
                {$IFDEF ECB}
                cmECB : case aCryptoType of
                            ctAES128     : Result := DecryptAESECB (Key,aValue);
                            ctAES192     : Result := DecryptAESECB (Key,aValue);
                            ctAES256     : Result := DecryptAESECB (Key,aValue);
                            ctAnubis128  : Result := DecryptAnubisECB (Key,aValue);
                            ctAnubis192  : Result := DecryptAnubisECB (Key,aValue);
                            ctAnubis256  : Result := DecryptAnubisECB (Key,aValue);
                            ctSerpent128 : Result := DecryptSerpentECB (Key,aValue);
                            ctSerpent192 : Result := DecryptSerpentECB (Key,aValue);
                            ctSerpent256 : Result := DecryptSerpentECB (Key,aValue);
                            ctShacal     : Result := DecryptShacalECB (Key,aValue);
                            ctBlowFish   : Result := DecryptBlowFishECB (Key,aValue);
                            ctTwoFish128 : Result := DecryptSerpentECB (Key,aValue);
                            ctTwoFish192 : Result := DecryptTwoFishECB (Key,aValue);
                            ctTwoFish256 : Result := DecryptTwoFishECB (Key,aValue);
                            else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                        end;
                {$ENDIF ECB}
                {$IFDEF CBC}
                cmCBC : case aCryptoType of
                            ctAES128     : Result := DecryptAESCBC (Key,aValue);
                            ctAES192     : Result := DecryptAESCBC (Key,aValue);
                            ctAES256     : Result := DecryptAESCBC (Key,aValue);
                            ctAnubis128  : Result := DecryptAnubisCBC (Key,aValue);
                            ctAnubis192  : Result := DecryptAnubisCBC (Key,aValue);
                            ctAnubis256  : Result := DecryptAnubisCBC (Key,aValue);
                            ctSerpent128 : Result := DecryptSerpentCBC (Key,aValue);
                            ctSerpent192 : Result := DecryptSerpentCBC (Key,aValue);
                            ctSerpent256 : Result := DecryptSerpentCBC (Key,aValue);
                            ctShacal     : Result := DecryptShacalCBC (Key,aValue);
                            ctBlowFish   : Result := DecryptBlowFishCBC (Key,aValue);
                            ctTwoFish128 : Result := DecryptSerpentCBC (Key,aValue);
                            ctTwoFish192 : Result := DecryptTwoFishCBC (Key,aValue);
                            ctTwoFish256 : Result := DecryptTwoFishCBC (Key,aValue);
                            else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                        end;
                {$ENDIF CBC}
                {$IFDEF CFB}
                cmCFB : case aCryptoType of
                            ctAES128     : Result := DecryptAESCFB (Key,aValue);
                            ctAES192     : Result := DecryptAESCFB (Key,aValue);
                            ctAES256     : Result := DecryptAESCFB (Key,aValue);
                            ctAnubis128  : Result := DecryptAnubisCFB (Key,aValue);
                            ctAnubis192  : Result := DecryptAnubisCFB (Key,aValue);
                            ctAnubis256  : Result := DecryptAnubisCFB (Key,aValue);
                            ctSerpent128 : Result := DecryptSerpentCFB (Key,aValue);
                            ctSerpent192 : Result := DecryptSerpentCFB (Key,aValue);
                            ctSerpent256 : Result := DecryptSerpentCFB (Key,aValue);
                            ctShacal     : Result := DecryptShacalCFB (Key,aValue);
                            ctBlowFish   : Result := DecryptBlowFishCFB (Key,aValue);
                            ctTwoFish128 : Result := DecryptSerpentCFB (Key,aValue);
                            ctTwoFish192 : Result := DecryptTwoFishCFB (Key,aValue);
                            ctTwoFish256 : Result := DecryptTwoFishCFB (Key,aValue);
                            else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                        end;
                {$ENDIF CFB}
                {$IFDEF OFB}
                cmOFB : case aCryptoType of
                            ctAES128     : Result := DecryptAESOFB (Key,aValue);
                            ctAES192     : Result := DecryptAESOFB (Key,aValue);
                            ctAES256     : Result := DecryptAESOFB (Key,aValue);
                            ctAnubis128  : Result := DecryptAnubisOFB (Key,aValue);
                            ctAnubis192  : Result := DecryptAnubisOFB (Key,aValue);
                            ctAnubis256  : Result := DecryptAnubisOFB (Key,aValue);
                            ctSerpent128 : Result := DecryptSerpentOFB (Key,aValue);
                            ctSerpent192 : Result := DecryptSerpentOFB (Key,aValue);
                            ctSerpent256 : Result := DecryptSerpentOFB (Key,aValue);
                            ctShacal     : Result := DecryptShacalOFB (Key,aValue);
                            ctBlowFish   : Result := DecryptBlowFishOFB (Key,aValue);
                            ctTwoFish128 : Result := DecryptSerpentOFB (Key,aValue);
                            ctTwoFish192 : Result := DecryptTwoFishOFB (Key,aValue);
                            ctTwoFish256 : Result := DecryptTwoFishOFB (Key,aValue);
                            else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                        end;
                {$ENDIF OFB}
                else    raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_MODE);
            end;
        finally
            _FillChar ( Key, Length (Key), $00 );
        end;
    except on E : Exception do
        _raise (['Decrypt',ERR_TCRYPTOKERNEL_DECRYPT_SYMMETRIC,E],
                ['{04328B0B-2481-41AB-A105-12F74E1AEC64}']);
    end;
end;

class function TCryptoKernel.Decrypt (const aValue: String;
                                      const aKey: Hex;
                                      const aCryptoAlgoritm: TCryptoAlgoritm;
                                      const aCryptoMode: TCryptoMode) : String;
var
    I : Integer;
    S : String;
begin
    Result := '';
    try
        S := aValue;
        try
            for I := High (aCryptoAlgoritm) downto 0 do
                S := Decrypt (S,aKey,aCryptoAlgoritm [I],aCryptoMode);
            Result := S;
        finally
            _FillChar ( S, Length (S), $00 );
        end;
    except on E : Exception do
        _raise (['Decrypt',ERR_TCRYPTOKERNEL_DECRYPT_SYMMETRIC,E],
                ['{415ADEE9-F00E-49D8-BDEF-E0CC3E3C623A}']);
    end;
end;

class function TCryptoKernel.Decrypt (const aValue: String;
                                      const aKey: Hex;
                                      anArgs: array of const) : String;
var
    CryptoAlgoritm : TCryptoAlgoritm;
    CryptoMode     : TCryptoMode;
begin
    Result := '';
    try
        { первый параметр - симм. шифр }
        SetLength (CryptoAlgoritm,1);
        CryptoAlgoritm [0] := ctUnknown;
        if notEmpty (0,anArgs) then
            CryptoAlgoritm := toCryptoAlgoritm (anArgs [0]);
        { второрй параметр - режим симм. шифрования }
        CryptoMode := cmUnknown;
        if notEmpty (1,anArgs) then
            CryptoMode := toCryptoMode (anArgs [1]);
        Result := Decrypt (aValue,
                           aKey,
                           CryptoAlgoritm,
                           CryptoMode);
    except on E : Exception do
        _raise (['Decrypt',ERR_TCRYPTOKERNEL_DECRYPT_SYMMETRIC,E],
                ['{A47FDACF-6B7C-437B-AB8B-ADD2F643F0B8}']);
    end;
end;

class procedure TCryptoKernel.Encrypt (const anInput: TStream;
                                       out anOutput: TStream;
                                       const aKey: Hex;
                                       const aCryptoType: TCryptoType;
                                       const aCryptoMode: TCryptoMode;
                                       const aRandomType: TRandomType = rtSystem);
var
    Key        : Hex;
    K          : PKey;
    oSize      : WORD;  
    RandomFunc : TRandomFunction;
    Reader     : TStream;
    Writer     : TStream;
begin
    try
        anInput.Position := 0;
        anOutput.Position := 0;
        try
            case aCryptoType of
                ctAES128     : Key := rpad (aKey,16*2,'0');
                ctAES192     : Key := rpad (aKey,24*2,'0');
                ctAES256     : Key := rpad (aKey,32*2,'0');
                ctAnubis128  : Key := rpad (aKey,16*2,'0');
                ctAnubis192  : Key := rpad (aKey,24*2,'0');
                ctAnubis256  : Key := rpad (aKey,32*2,'0');
                ctSerpent128 : Key := rpad (aKey,16*2,'0');
                ctSerpent192 : Key := rpad (aKey,24*2,'0');
                ctSerpent256 : Key := rpad (aKey,32*2,'0');
                ctShacal     : Key := rpad (aKey,64*2,'0');
                ctBlowFish   : Key := rpad (aKey,16*2,'0');
                ctTwoFish128 : Key := rpad (aKey,16*2,'0');
                ctTwoFish192 : Key := rpad (aKey,24*2,'0');
                ctTwoFish256 : Key := rpad (aKey,32*2,'0');
                else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
            end;
            oSize := Length (Key) div 2;
            K := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (Key,K);
            RandomFunc := GetRandomFunction (aRandomType);
            Reader := TMemoryStream.Create;
            Writer := TMemoryStream.Create;
            Reader.CopyFrom (anInput,anInput.Size);
            Reader.Position := 0;
            case aCryptoMode of
                {$IFDEF ECB}
                cmECB : case aCryptoType of
                            ctAES128     : EncryptAESECB (K^,oSize*8,Reader,Writer);
                            ctAES192     : EncryptAESECB (K^,oSize*8,Reader,Writer);
                            ctAES256     : EncryptAESECB (K^,oSize*8,Reader,Writer);
                            ctAnubis128  : EncryptAnubisECB (K^,oSize*8,Reader,Writer);
                            ctAnubis192  : EncryptAnubisECB (K^,oSize*8,Reader,Writer);
                            ctAnubis256  : EncryptAnubisECB (K^,oSize*8,Reader,Writer);
                            ctSerpent128 : EncryptSerpentECB (K^,oSize*8,Reader,Writer);
                            ctSerpent192 : EncryptSerpentECB (K^,oSize*8,Reader,Writer);
                            ctSerpent256 : EncryptSerpentECB (K^,oSize*8,Reader,Writer);
                            ctShacal     : EncryptShacalECB (K^,oSize*8,Reader,Writer);
                            ctBlowFish   : EncryptBlowFishECB (K^,oSize*8,Reader,Writer);
                            ctTwoFish128 : EncryptSerpentECB (K^,oSize*8,Reader,Writer);
                            ctTwoFish192 : EncryptTwoFishECB (K^,oSize*8,Reader,Writer);
                            ctTwoFish256 : EncryptTwoFishECB (K^,oSize*8,Reader,Writer);
                            else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                        end;
                {$ENDIF ECB}
                {$IFDEF CBC}
                cmCBC : case aCryptoType of
                            ctAES128     : EncryptAESCBC (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctAES192     : EncryptAESCBC (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctAES256     : EncryptAESCBC (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctAnubis128  : EncryptAnubisCBC (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctAnubis192  : EncryptAnubisCBC (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctAnubis256  : EncryptAnubisCBC (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctSerpent128 : EncryptSerpentCBC (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctSerpent192 : EncryptSerpentCBC (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctSerpent256 : EncryptSerpentCBC (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctShacal     : EncryptShacalCBC (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctBlowFish   : EncryptBlowFishCBC (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctTwoFish128 : EncryptSerpentCBC (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctTwoFish192 : EncryptTwoFishCBC (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctTwoFish256 : EncryptTwoFishCBC (K^,oSize*8,RandomFunc,Reader,Writer);
                            else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                        end;
                {$ENDIF CBC}
                {$IFDEF CFB}
                cmCFB : case aCryptoType of
                            ctAES128     : EncryptAESCFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctAES192     : EncryptAESCFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctAES256     : EncryptAESCFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctAnubis128  : EncryptAnubisCFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctAnubis192  : EncryptAnubisCFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctAnubis256  : EncryptAnubisCFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctSerpent128 : EncryptSerpentCFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctSerpent192 : EncryptSerpentCFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctSerpent256 : EncryptSerpentCFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctShacal     : EncryptShacalCFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctBlowFish   : EncryptBlowFishCFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctTwoFish128 : EncryptSerpentCFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctTwoFish192 : EncryptTwoFishCFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctTwoFish256 : EncryptTwoFishCFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                        end;
                {$ENDIF CFB}
                {$IFDEF OFB}
                cmOFB : case aCryptoType of
                            ctAES128     : EncryptAESOFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctAES192     : EncryptAESOFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctAES256     : EncryptAESOFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctAnubis128  : EncryptAnubisOFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctAnubis192  : EncryptAnubisOFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctAnubis256  : EncryptAnubisOFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctSerpent128 : EncryptSerpentOFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctSerpent192 : EncryptSerpentOFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctSerpent256 : EncryptSerpentOFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctShacal     : EncryptShacalOFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctBlowFish   : EncryptBlowFishOFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctTwoFish128 : EncryptSerpentOFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctTwoFish192 : EncryptTwoFishOFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            ctTwoFish256 : EncryptTwoFishOFB (K^,oSize*8,RandomFunc,Reader,Writer);
                            else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                        end;
                {$ENDIF OFB}
                else    raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_MODE);
            end;
            //_Trim (Writer,$00);
            ByteStreamToHexStream (Writer,anOutput);
        finally
            _FillChar ( Key, Length (Key), $00 );
            NULL_KEY ( K, oSize*8 );
            Dispose (K);
            K := NIL;
            oSize := 0;
            if Assigned (Reader) then
                TMemoryStream (Reader).Clear;
            FreeAndNil (Reader);
            if Assigned (Writer) then
                TMemoryStream (Writer).Clear;
            FreeAndNil (Writer);
        end;
    except on E : Exception do
        _raise (['Encrypt',ERR_TCRYPTOKERNEL_ENCRYPT_SYMMETRIC,E],
                ['{099CD6A2-D152-49B6-9E93-7CE0629A5103}']);
    end;
end;

class procedure TCryptoKernel.Encrypt (const anInput: TStream;
                                       out anOutput: TStream;
                                       const aKey: Hex;
                                       const aCryptoAlgoritm: TCryptoAlgoritm;
                                       const aCryptoMode: TCryptoMode;
                                       const aRandomType: TRandomType = rtSystem);
var
    I         : Integer;
    Reader  : TStream;
    Writer : TStream;
begin
    try
        Reader := TMemoryStream.Create;
        Writer := TMemoryStream.Create;
        try
            anInput.Position := 0;
            Reader.CopyFrom (anInput,anInput.Size);
            for I := 0 to High (aCryptoAlgoritm) do
            begin
                Encrypt (Reader,
                         Writer,
                         aKey,
                         aCryptoAlgoritm [I],
                         aCryptoMode,
                         aRandomType);
                TMemoryStream (Reader).Clear;
                Writer.Position := 0;
                Reader.CopyFrom (Writer,Writer.Size);
                Reader.Position := 0;
                TMemoryStream (Writer).Clear;
            end;
            anOutput.CopyFrom (Reader,Reader.Size);
        finally
            if Assigned (Reader) then
                TMemoryStream (Reader).Clear;
            FreeAndNil (Reader);
            if Assigned (Writer) then
                TMemoryStream (Writer).Clear;
            FreeAndNil (Writer);
        end;
    except on E : Exception do
        _raise (['Encrypt',ERR_TCRYPTOKERNEL_ENCRYPT_SYMMETRIC,E],
                ['{60FEE4D2-F25F-4B49-92E1-B5FA1F2D6FB0}']);
    end;
end;

class procedure TCryptoKernel.Encrypt (const anInput: TStream;
                                       out anOutput: TStream;
                                       const aKey: Hex;
                                       anArgs: array of const);
var
    CryptoAlgoritm : TCryptoAlgoritm;
    CryptoMode     : TCryptoMode;
    RandomType     : TRandomType;
begin
    try
        { первый параметр - симм. шифр }
        SetLength (CryptoAlgoritm,1);
        CryptoAlgoritm [0] := ctUnknown;
        if notEmpty (0,anArgs) then
            CryptoAlgoritm := toCryptoAlgoritm (anArgs [0]);
        { второрй параметр - режим симм. шифрования }
        CryptoMode := cmUnknown;
        if notEmpty (1,anArgs) then
            CryptoMode := toCryptoMode (anArgs [1]);
        { третий параметр - тип генератора псевдо-случайных чисел }
        RandomType := rtSystem;
        if notEmpty (2,anArgs) then
            RandomType := toRandomType (anArgs [2]);
        Encrypt (anInput,
                 anOutput,
                 aKey,
                 CryptoAlgoritm,
                 CryptoMode,
                 RandomType);
    except on E : Exception do
        _raise (['Encrypt',ERR_TCRYPTOKERNEL_ENCRYPT_SYMMETRIC,E],
                ['{6569464B-2D78-48B6-8696-0534DFEE8E23}']);
    end;
end;

class procedure TCryptoKernel.Decrypt (const anInput: TStream;
                                       out anOutput: TStream;
                                       const aKey: Hex;
                                       const aCryptoType: TCryptoType;
                                       const aCryptoMode: TCryptoMode);
var
    Key    : Hex;
    K      : PKey;
    oSize  : WORD;
    Reader : TStream;
    Writer : TStream;
begin
    try
        anInput.Position := 0;
        anOutput.Position := 0;
        try
            case aCryptoType of
                ctAES128     : Key := rpad (aKey,16*2,'0');
                ctAES192     : Key := rpad (aKey,24*2,'0');
                ctAES256     : Key := rpad (aKey,32*2,'0');
                ctAnubis128  : Key := rpad (aKey,16*2,'0');
                ctAnubis192  : Key := rpad (aKey,24*2,'0');
                ctAnubis256  : Key := rpad (aKey,32*2,'0');
                ctSerpent128 : Key := rpad (aKey,16*2,'0');
                ctSerpent192 : Key := rpad (aKey,24*2,'0');
                ctSerpent256 : Key := rpad (aKey,32*2,'0');
                ctShacal     : Key := rpad (aKey,64*2,'0');
                ctBlowFish   : Key := rpad (aKey,16*2,'0');
                ctTwoFish128 : Key := rpad (aKey,16*2,'0');
                ctTwoFish192 : Key := rpad (aKey,24*2,'0');
                ctTwoFish256 : Key := rpad (aKey,32*2,'0');
                else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
            end;
            oSize := Length (Key) div 2;
            K := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (Key,K);
            Reader := TMemoryStream.Create;
            Writer := TMemoryStream.Create;
            HexStreamToByteStream (anInput,Reader);
            Reader.Position := 0;
            case aCryptoMode of
                {$IFDEF ECB}
                cmECB : case aCryptoType of
                            ctAES128     : DecryptAESECB (K^,oSize*8,Reader,Writer);
                            ctAES192     : DecryptAESECB (K^,oSize*8,Reader,Writer);
                            ctAES256     : DecryptAESECB (K^,oSize*8,Reader,Writer);
                            ctAnubis128  : DecryptAnubisECB (K^,oSize*8,Reader,Writer);
                            ctAnubis192  : DecryptAnubisECB (K^,oSize*8,Reader,Writer);
                            ctAnubis256  : DecryptAnubisECB (K^,oSize*8,Reader,Writer);
                            ctSerpent128 : DecryptSerpentECB (K^,oSize*8,Reader,Writer);
                            ctSerpent192 : DecryptSerpentECB (K^,oSize*8,Reader,Writer);
                            ctSerpent256 : DecryptSerpentECB (K^,oSize*8,Reader,Writer);
                            ctShacal     : DecryptShacalECB (K^,oSize*8,Reader,Writer);
                            ctBlowFish   : DecryptBlowFishECB (K^,oSize*8,Reader,Writer);
                            ctTwoFish128 : DecryptSerpentECB (K^,oSize*8,Reader,Writer);
                            ctTwoFish192 : DecryptTwoFishECB (K^,oSize*8,Reader,Writer);
                            ctTwoFish256 : DecryptTwoFishECB (K^,oSize*8,Reader,Writer);
                            else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                        end;
                {$ENDIF ECB}
                {$IFDEF CBC}
                cmCBC : case aCryptoType of
                            ctAES128     : DecryptAESCBC (K^,oSize*8,Reader,Writer);
                            ctAES192     : DecryptAESCBC (K^,oSize*8,Reader,Writer);
                            ctAES256     : DecryptAESCBC (K^,oSize*8,Reader,Writer);
                            ctAnubis128  : DecryptAnubisCBC (K^,oSize*8,Reader,Writer);
                            ctAnubis192  : DecryptAnubisCBC (K^,oSize*8,Reader,Writer);
                            ctAnubis256  : DecryptAnubisCBC (K^,oSize*8,Reader,Writer);
                            ctSerpent128 : DecryptSerpentCBC (K^,oSize*8,Reader,Writer);
                            ctSerpent192 : DecryptSerpentCBC (K^,oSize*8,Reader,Writer);
                            ctSerpent256 : DecryptSerpentCBC (K^,oSize*8,Reader,Writer);
                            ctShacal     : DecryptShacalCBC (K^,oSize*8,Reader,Writer);
                            ctBlowFish   : DecryptBlowFishCBC (K^,oSize*8,Reader,Writer);
                            ctTwoFish128 : DecryptSerpentCBC (K^,oSize*8,Reader,Writer);
                            ctTwoFish192 : DecryptTwoFishCBC (K^,oSize*8,Reader,Writer);
                            ctTwoFish256 : DecryptTwoFishCBC (K^,oSize*8,Reader,Writer);
                            else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                        end;
                {$ENDIF CBC}
                {$IFDEF CFB}
                cmCFB : case aCryptoType of
                            ctAES128     : DecryptAESCFB (K^,oSize*8,Reader,Writer);
                            ctAES192     : DecryptAESCFB (K^,oSize*8,Reader,Writer);
                            ctAES256     : DecryptAESCFB (K^,oSize*8,Reader,Writer);
                            ctAnubis128  : DecryptAnubisCFB (K^,oSize*8,Reader,Writer);
                            ctAnubis192  : DecryptAnubisCFB (K^,oSize*8,Reader,Writer);
                            ctAnubis256  : DecryptAnubisCFB (K^,oSize*8,Reader,Writer);
                            ctSerpent128 : DecryptSerpentCFB (K^,oSize*8,Reader,Writer);
                            ctSerpent192 : DecryptSerpentCFB (K^,oSize*8,Reader,Writer);
                            ctSerpent256 : DecryptSerpentCFB (K^,oSize*8,Reader,Writer);
                            ctShacal     : DecryptShacalCFB (K^,oSize*8,Reader,Writer);
                            ctBlowFish   : DecryptBlowFishCFB (K^,oSize*8,Reader,Writer);
                            ctTwoFish128 : DecryptSerpentCFB (K^,oSize*8,Reader,Writer);
                            ctTwoFish192 : DecryptTwoFishCFB (K^,oSize*8,Reader,Writer);
                            ctTwoFish256 : DecryptTwoFishCFB (K^,oSize*8,Reader,Writer);
                            else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                        end;
                {$ENDIF CFB}
                {$IFDEF OFB}
                cmOFB : case aCryptoType of
                            ctAES128     : DecryptAESOFB (K^,oSize*8,Reader,Writer);
                            ctAES192     : DecryptAESOFB (K^,oSize*8,Reader,Writer);
                            ctAES256     : DecryptAESOFB (K^,oSize*8,Reader,Writer);
                            ctAnubis128  : DecryptAnubisOFB (K^,oSize*8,Reader,Writer);
                            ctAnubis192  : DecryptAnubisOFB (K^,oSize*8,Reader,Writer);
                            ctAnubis256  : DecryptAnubisOFB (K^,oSize*8,Reader,Writer);
                            ctSerpent128 : DecryptSerpentOFB (K^,oSize*8,Reader,Writer);
                            ctSerpent192 : DecryptSerpentOFB (K^,oSize*8,Reader,Writer);
                            ctSerpent256 : DecryptSerpentOFB (K^,oSize*8,Reader,Writer);
                            ctShacal     : DecryptShacalOFB (K^,oSize*8,Reader,Writer);
                            ctBlowFish   : DecryptBlowFishOFB (K^,oSize*8,Reader,Writer);
                            ctTwoFish128 : DecryptSerpentOFB (K^,oSize*8,Reader,Writer);
                            ctTwoFish192 : DecryptTwoFishOFB (K^,oSize*8,Reader,Writer);
                            ctTwoFish256 : DecryptTwoFishOFB (K^,oSize*8,Reader,Writer);
                            else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                        end;
                {$ENDIF OFB}
                else    raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_MODE);
            end;
            _Trim (Writer,$00);
            Writer.Position := 0;
            anOutput.CopyFrom (Writer,Writer.Size);
        finally
            _FillChar ( Key, Length (Key), $00 );
            NULL_KEY ( K, oSize*8 );
            Dispose (K);
            K := NIL;
            oSize := 0;
            if Assigned (Reader) then
                TMemoryStream (Reader).Clear;
            FreeAndNil (Reader);
            if Assigned (Writer) then
                TMemoryStream (Writer).Clear;
            FreeAndNil (Writer);
        end;
    except on E : Exception do
        _raise (['Decrypt',ERR_TCRYPTOKERNEL_DECRYPT_SYMMETRIC,E],
                ['{E5CD11E6-24AA-4D6F-BFB0-E851516C50E0}']);
    end;
end;

class procedure TCryptoKernel.Decrypt (const anInput: TStream;
                                       out anOutput: TStream;
                                       const aKey: Hex;
                                       const aCryptoAlgoritm: TCryptoAlgoritm;
                                       const aCryptoMode: TCryptoMode);
var
    I      : Integer;
    Reader : TStream;
    Writer : TStream;
begin
    try
        Reader := TMemoryStream.Create;
        Writer := TMemoryStream.Create;
        try
            anInput.Position := 0;
            Reader.CopyFrom (anInput,anInput.Size);
            for I := High (aCryptoAlgoritm) downto 0 do
            begin
                Decrypt (Reader,Writer,aKey,aCryptoAlgoritm [I],aCryptoMode);
                TMemoryStream (Reader).Clear;
                Writer.Position := 0;
                Reader.CopyFrom (Writer,Writer.Size);
                Reader.Position := 0;
                TMemoryStream (Writer).Clear;
            end;
            anOutput.CopyFrom (Reader,Reader.Size);
        finally
            if Assigned (Reader) then
                TMemoryStream (Reader).Clear;
            FreeAndNil (Reader);
            if Assigned (Writer) then
                TMemoryStream (Writer).Clear;
            FreeAndNil (Writer);
        end;
    except on E : Exception do
        _raise (['Decrypt',ERR_TCRYPTOKERNEL_DECRYPT_SYMMETRIC,E],
                ['{B1F3EB3E-D9FB-4565-A309-302FD2797529}']);
    end;
end;

class procedure TCryptoKernel.Decrypt (const anInput: TStream;
                                       out anOutput: TStream;
                                       const aKey: Hex;
                                       anArgs: array of const);
var
    CryptoAlgoritm : TCryptoAlgoritm;
    CryptoMode     : TCryptoMode;
begin
    try
        { первый параметр - симм. шифр }
        SetLength (CryptoAlgoritm,1);
        CryptoAlgoritm [0] := ctUnknown;
        if notEmpty (0,anArgs) then
            CryptoAlgoritm := toCryptoAlgoritm (anArgs [0]);
        { второрй параметр - режим симм. шифрования }
        CryptoMode := cmUnknown;
        if notEmpty (1,anArgs) then
            CryptoMode := toCryptoMode (anArgs [1]);
        Decrypt (anInput,
                 anOutput,
                 aKey,
                 CryptoAlgoritm,
                 CryptoMode);
    except on E : Exception do
        _raise (['Decrypt',ERR_TCRYPTOKERNEL_DECRYPT_SYMMETRIC,E],
                ['{161EA8B6-4CA7-43AA-914F-64EFAB29B423}']);
    end;
end;

class function TCryptoKernel.TestSymmetric (const aCryptoType: TCryptoType;
                                            const aCryptoMode: TCryptoMode) : Boolean;
begin
    Result := FALSE;
    try
        case aCryptoMode of
        {$IFDEF ECB}
            cmECB : case aCryptoType of
                        ctAES128,
                        ctAES192,
                        ctAES256     : Result := TestAESECB;
                        ctAnubis128,
                        ctAnubis192,
                        ctAnubis256  : Result := TestAnubisECB;
                        ctSerpent128,
                        ctSerpent192,
                        ctSerpent256 : Result := TestSerpentECB;
                        ctShacal     : Result := TestShacalECB;
                        ctBlowFish   : Result := TestBlowFishECB;
                        ctTwoFish128,
                        ctTwoFish192,
                        ctTwoFish256 : Result := TestTwoFishECB;
                        else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                    end;
        {$ENDIF ECB}
        {$IFDEF CBC}
            cmCBC : case aCryptoType of
                        ctAES128,
                        ctAES192,
                        ctAES256     : Result := TestAESCBC;
                        ctAnubis128,
                        ctAnubis192,
                        ctAnubis256  : Result := TestAnubisCBC;
                        ctSerpent128,
                        ctSerpent192,
                        ctSerpent256 : Result := TestSerpentCBC;
                        ctShacal     : Result := TestShacalCBC;
                        ctBlowFish   : Result := TestBlowFishCBC;
                        ctTwoFish128,
                        ctTwoFish192,
                        ctTwoFish256 : Result := TestTwoFishCBC;
                        else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                    end;
        {$ENDIF CBC}
        {$IFDEF CFB}
            cmCFB : case aCryptoType of
                        ctAES128,
                        ctAES192,
                        ctAES256     : Result := TestAESCFB;
                        ctAnubis128,
                        ctAnubis192,
                        ctAnubis256  : Result := TestAnubisCFB;
                        ctSerpent128,
                        ctSerpent192,
                        ctSerpent256 : Result := TestSerpentCFB;
                        ctShacal     : Result := TestShacalCFB;
                        ctBlowFish   : Result := TestBlowFishCFB;
                        ctTwoFish128,
                        ctTwoFish192,
                        ctTwoFish256 : Result := TestTwoFishCFB;
                        else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                    end;
        {$ENDIF CFB}
        {$IFDEF OFB}
            cmOFB : case aCryptoType of
                        ctAES128,
                        ctAES192,
                        ctAES256     : Result := TestAESOFB;
                        ctAnubis128,
                        ctAnubis192,
                        ctAnubis256  : Result := TestAnubisOFB;
                        ctSerpent128,
                        ctSerpent192,
                        ctSerpent256 : Result := TestSerpentOFB;
                        ctShacal     : Result := TestShacalOFB;
                        ctBlowFish   : Result := TestBlowFishOFB;
                        ctTwoFish128,
                        ctTwoFish192,
                        ctTwoFish256 : Result := TestTwoFishOFB;
                        else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                    end;
        {$ENDIF OFB}
            else    raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_MODE);
        end;
    except on E : Exception do
        _raise (['TestSymmetric',ERR_TCRYPTOKERNEL_TEST_SYMMETRIC,E],
                ['{C329B2E9-FEEE-42C8-879E-464CB3E29BA4}']);
    end;
end;

class function TCryptoKernel.TestSymmetric (const aCryptoAlgoritm: TCryptoAlgoritm;
                                            const aCryptoMode: TCryptoMode) : Boolean;
var
    I : Integer;
    B : Boolean;
begin
    Result := FALSE;
    try
        B := TRUE;
        for I := 0 to High (aCryptoAlgoritm) do
            B := B and TestSymmetric (aCryptoAlgoritm [I],aCryptoMode);
        Result := B;
    except on E : Exception do
        _raise (['TestSymmetric',ERR_TCRYPTOKERNEL_TEST_SYMMETRIC,E],
                ['{5ABC6F39-F666-4D4E-9D2D-77BB3AB303D2}']);
    end;
end;

class function TCryptoKernel.TestSymmetric (anArgs: array of const) : Boolean;
var
    CryptoAlgoritm : TCryptoAlgoritm;
    CryptoMode     : TCryptoMode;
begin
    Result := FALSE;
    try
        { первый параметр - симм. шифр }
        SetLength (CryptoAlgoritm,1);
        CryptoAlgoritm [0] := ctUnknown;
        if notEmpty (0,anArgs) then
            CryptoAlgoritm := toCryptoAlgoritm (anArgs [0]);
        { второрй параметр - режим симм. шифрования }
        CryptoMode := cmUnknown;
        if notEmpty (1,anArgs) then
            CryptoMode := toCryptoMode (anArgs [1]);
        Result := TestSymmetric (CryptoAlgoritm,CryptoMode);
    except on E : Exception do
        _raise (['TestSymmetric',ERR_TCRYPTOKERNEL_TEST_SYMMETRIC,E],
                ['{BD9C69DB-CCFD-4EBD-BE37-9AB77CB0D90B}']);
    end;
end;

class function TCryptoKernel.Encrypt (const aValue: String;
                                      const aPublicKey: String;
                                      const aPrivateKey: String;
                                      const aPKCryptoType: TPKCryptoType;
                                      const aCryptoType: TCryptoType;
                                      const aCryptoMode: TCryptoMode;
                                      const aHashType: THashType;
                                      const aRandomType: TRandomType = rtSystem) : String;
var
    Key    : String;
    KeyCph : String;
    Cph    : String;
    Sgn    : String;
begin
    Result := '';
    try
        try
            { генерируем сеансовый ключ  }
            GenerateKey (Key,aCryptoType,aRandomType);
            { шифруем сообщение симметричным алгоритмом }
            Cph := Encrypt (aValue,Key,aCryptoType,aCryptoMode,aRandomType);
            { подписываем хэш от сообщения сеансовым ключом }
            Sgn := HMAC ( Hash (aValue, aHashType), Key, aHashType );
            { шифруем и подписываем сеансовый ключ асимметричным алгоритмом }
            KeyCph := Encrypt (Key,aPublicKey,aPrivateKey,aPKCryptoType,aHashType);
            { объединяем }
            Result := Format ('%s%s%s%s%s%s',[ IntToHex ( Length (KeyCph), 4 ), KeyCph,
                                               IntToHex ( Length (Cph), 8 ), Cph,
                                               IntToHex ( Length (Sgn), 4 ), Sgn ]);
        finally
            _FillChar ( Key, Length (Key), $00 );
            _FillChar ( KeyCph, Length (KeyCph), $00 );
            _FillChar ( Cph, Length (Cph), $00 );
            _FillChar ( Sgn, Length (Sgn), $00 );
        end;
    except on E : Exception do
        _raise (['Encrypt',ERR_TCRYPTOKERNEL_ENCRYPT_HYBRID,E],
                ['{40E966F2-7F56-442C-8B16-3635962E045D}']);
    end;
end;

class function TCryptoKernel.Encrypt (const aValue: String;
                                      const aPublicKey: String;
                                      const aPrivateKey: String;
                                      const aPKCryptoType: TPKCryptoType;
                                      const aCryptoAlgoritm: TCryptoAlgoritm;
                                      const aCryptoMode: TCryptoMode;
                                      const aHashAlgoritm: THashAlgoritm;
                                      const aRandomType: TRandomType = rtSystem) : String;
var
    Key    : String;
    KeyCph : String;
    Cph    : String;
    Sgn    : String;
begin
    Result := '';
    try
        try
            { генерируем сеансовый ключ  }
            GenerateKey (Key,aCryptoAlgoritm,aRandomType);
            { шифруем сообщение симметричным алгоритмом }
            Cph := Encrypt (aValue,Key,aCryptoAlgoritm,aCryptoMode,aRandomType);
            { подписываем хэш от сообщения сеансовым ключом }
            Sgn := HMAC ( Hash (aValue,aHashAlgoritm), Key, aHashAlgoritm );
            { шифруем и подписываем сеансовый ключ асимметричным алгоритмом }
            KeyCph := Encrypt (Key,aPublicKey,aPrivateKey,aPKCryptoType,aHashAlgoritm);
            { объединяем }
            Result := Format ('%s%s%s%s%s%s',[ IntToHex ( Length (KeyCph), 4 ), KeyCph,
                                               IntToHex ( Length (Cph), 8 ), Cph,
                                               IntToHex ( Length (Sgn), 4 ), Sgn ]);
        finally
            _FillChar ( Key, Length (Key), $00 );
            _FillChar ( KeyCph, Length (KeyCph), $00 );
            _FillChar ( Cph, Length (Cph), $00 );
            _FillChar ( Sgn, Length (Sgn), $00 );
        end;
    except on E : Exception do
        _raise (['Encrypt',ERR_TCRYPTOKERNEL_ENCRYPT_HYBRID,E],
                ['{15C0E648-897D-43CE-A2E2-0B3FEF7FFA8B}']);
    end;
end;

class function TCryptoKernel.Encrypt (const aValue: String;
                                      const aPublicKey: String;
                                      const aPrivateKey: String;
                                      anArgs: array of const) : String;
var
    PKCryptoType   : TPKCryptoType;
    CryptoAlgoritm : TCryptoAlgoritm;
    CryptoMode     : TCryptoMode;
    HashAlgoritm   : THashAlgoritm;
    RandomType     : TRandomType;
begin
    Result := '';
    try
        { первый параметр - асимм. шифр }
        PKCryptoType := pkctUnknown;
        if notEmpty (0,anArgs) then
            PKCryptoType := toPKCryptoType (anArgs [0]);
        { второрй параметр - симм. шифр }
        SetLength (CryptoAlgoritm,1);
        CryptoAlgoritm [0] := ctUnknown;
        if notEmpty (1,anArgs) then
            CryptoAlgoritm := toCryptoAlgoritm (anArgs [1]);
        { третий параметр - режим симм. шифрования }
        CryptoMode := cmUnknown;
        if notEmpty (2,anArgs) then
            CryptoMode := toCryptoMode (anArgs [2]);
        { четвертый параметр - хэш-функция }
        SetLength (HashAlgoritm,1);
        HashAlgoritm [0] := htUnknown;
        if notEmpty (3,anArgs) then
            HashAlgoritm := toHashAlgoritm (anArgs [3]);
        { пятый параметр - тип генератора псевдо-случайных чисел }
        RandomType := rtSystem;
        if notEmpty (4,anArgs) then
            RandomType := toRandomType (anArgs [4]);
        Result := Encrypt (aValue,
                           aPublicKey,
                           aPrivateKey,
                           PKCryptoType,
                           CryptoAlgoritm,
                           CryptoMode,
                           HashAlgoritm,
                           RandomType);
    except on E : Exception do
        _raise (['Encrypt',ERR_TCRYPTOKERNEL_ENCRYPT_HYBRID,E],
                ['{7F179A77-C4F0-40D3-8610-324779944F53}']);
    end;
end;

class function TCryptoKernel.Decrypt (const aValue: String;
                                      const aPublicKey: String;
                                      const aPrivateKey: String;
                                      const aPKCryptoType: TPKCryptoType;
                                      const aCryptoType: TCryptoType;
                                      const aCryptoMode: TCryptoMode;
                                      const aHashType: THashType) : String;
var
    KeyCph       : String;
    KeyCphLength : WORD;
    Cph          : String;
    CphLength    : Int64;
    Sgn          : String;
    SgnLength    : WORD;
    Key          : String;
    Msg          : String;
begin
    Result := '';
    try
        try
            KeyCphLength := HexToInt ( Copy (aValue,1,4) );
            KeyCph := Copy (aValue,5,KeyCphLength);
            CphLength := HexToInt ( Copy (aValue,KeyCphLength+5,8) );
            Cph := Copy (aValue,KeyCphLength+13,CphLength);
            SgnLength := HexToInt ( Copy (aValue,KeyCphLength+CphLength+13,4) );
            Sgn := Copy (aValue,KeyCphLength+CphLength+17,SgnLength);
            { расшифровываем сеансовый ключ и проверяем подпись }
            Key := Decrypt (KeyCph,aPublicKey,aPrivateKey,aPKCryptoType,aHashType);
            { расшифровываем сообщение с помощью сеансового ключа }
            Msg := Decrypt (Cph,Key,aCryptoType,aCryptoMode);
            { проверяем подпись }
            if (  Sgn = HMAC ( Hash (Msg,aHashType), Key, aHashType )  ) then
                Result := Msg
            else
                raise Exception.Create (ERR_TCRYPTOKERNEL_VERIFY_FAILED);
        finally
            _FillChar ( KeyCph, Length (KeyCph), $00 );
            KeyCphLength := 0;
            _FillChar ( Cph, Length (Cph), $00 );
            CphLength := 0;
            _FillChar ( Sgn, Length (Sgn), $00 );
            SgnLength := 0;
            _FillChar ( Key, Length (Key), $00 );
            _FillChar ( Msg, Length (Msg), $00 );
        end;
    except on E : Exception do
        _raise (['Decrypt',ERR_TCRYPTOKERNEL_DECRYPT_HYBRID,E],
                ['{A52CAB40-66A0-4919-926A-3213CB70ED8C}']);
    end;
end;

class function TCryptoKernel.Decrypt (const aValue: String;
                                      const aPublicKey: String;
                                      const aPrivateKey: String;
                                      const aPKCryptoType: TPKCryptoType;
                                      const aCryptoAlgoritm: TCryptoAlgoritm;
                                      const aCryptoMode: TCryptoMode;
                                      const aHashAlgoritm: THashAlgoritm) : String;
var
    KeyCph       : String;
    KeyCphLength : WORD;
    Cph          : String;
    CphLength    : Int64;
    Sgn          : String;
    SgnLength    : WORD;
    Key          : String;
    Msg          : String;
begin
    Result := '';
    try
        try
            KeyCphLength := HexToInt ( Copy (aValue,1,4) );
            KeyCph := Copy (aValue,5,KeyCphLength);
            CphLength := HexToInt ( Copy (aValue,KeyCphLength+5,8) );
            Cph := Copy (aValue,KeyCphLength+13,CphLength);
            SgnLength := HexToInt ( Copy (aValue,KeyCphLength+CphLength+13,4) );
            Sgn := Copy (aValue,KeyCphLength+CphLength+17,SgnLength);
            { расшифровываем сеансовый ключ и проверяем подпись }
            Key := Decrypt (KeyCph,aPublicKey,aPrivateKey,aPKCryptoType,aHashAlgoritm);
            { расшифровываем сообщение с помощью сеансового ключа }
            Msg := Decrypt (Cph,Key,aCryptoAlgoritm,aCryptoMode);
            { проверяем подпись }
            if (  Sgn = HMAC ( Hash (Msg,aHashAlgoritm), Key, aHashAlgoritm )  ) then
                Result := Msg
            else
                raise Exception.Create (ERR_TCRYPTOKERNEL_VERIFY_FAILED);
        finally
            _FillChar ( KeyCph, Length (KeyCph), $00 );
            KeyCphLength := 0;
            _FillChar ( Cph, Length (Cph), $00 );
            CphLength := 0;
            _FillChar ( Sgn, Length (Sgn), $00 );
            SgnLength := 0;
            _FillChar ( Key, Length (Key), $00 );
            _FillChar ( Msg, Length (Msg), $00 );
        end;
    except on E : Exception do
        _raise (['Decrypt',ERR_TCRYPTOKERNEL_DECRYPT_HYBRID,E],
                ['{3A9A2533-CABA-4714-867E-150E28FCE9A3}']);
    end;
end;

class function TCryptoKernel.Decrypt (const aValue: String;
                                      const aPublicKey: String;
                                      const aPrivateKey: String;
                                      anArgs: array of const) : String;
var
    PKCryptoType   : TPKCryptoType;
    CryptoAlgoritm : TCryptoAlgoritm;
    CryptoMode     : TCryptoMode;
    HashAlgoritm   : THashAlgoritm;
begin
    Result := '';
    try
        { первый параметр - асимм. шифр }
        PKCryptoType := pkctUnknown;
        if notEmpty (0,anArgs) then
            PKCryptoType := toPKCryptoType (anArgs [0]);
        { второрй параметр - симм. шифр }
        SetLength (CryptoAlgoritm,1);
        CryptoAlgoritm [0] := ctUnknown;
        if notEmpty (1,anArgs) then
            CryptoAlgoritm := toCryptoAlgoritm (anArgs [1]);
        { третий параметр - режим симм. шифрования }
        CryptoMode := cmUnknown;
        if notEmpty (2,anArgs) then
            CryptoMode := toCryptoMode (anArgs [2]);
        { четвертый параметр - хэш-функция }
        SetLength (HashAlgoritm,1);
        HashAlgoritm [0] := htUnknown;
        if notEmpty (3,anArgs) then
            HashAlgoritm := toHashAlgoritm (anArgs [3]);
        Result := Decrypt (aValue,
                           aPublicKey,
                           aPrivateKey,
                           PKCryptoType,
                           CryptoAlgoritm,
                           CryptoMode,
                           HashAlgoritm);
    except on E : Exception do
        _raise (['Decrypt',ERR_TCRYPTOKERNEL_DECRYPT_HYBRID,E],
                ['{FF9FBF07-688E-4B20-AB49-733309409213}']);
    end;
end;

class procedure TCryptoKernel.Encrypt (const anInput: TStream;
                                       out anOutput: TStream;
                                       const aPublicKey: String;
                                       const aPrivateKey: String;
                                       const aPKCryptoType: TPKCryptoType;
                                       const aCryptoType: TCryptoType;
                                       const aCryptoMode: TCryptoMode;
                                       const aHashType: THashType;
                                       const aRandomType: TRandomType = rtSystem);
var
    Stream : TStream;
    Key    : String;
    KeyCph : String;
    Cph    : String;
    Sgn    : String;
begin
    try
        Stream := TMemoryStream.Create;
        try
            { генерируем сеансовый ключ  }
            GenerateKey (Key,aCryptoType,aRandomType);
            { шифруем сообщение симметричным алгоритмом }
            anInput.Position := 0;
            Encrypt (anInput,Stream,Key,aCryptoType,aCryptoMode,aRandomType);
            { подписываем хэш от сообщения сеансовым ключом }
            anInput.Position := 0;
            Sgn := HMAC ( Hash (anInput,aHashType), Key, aHashType );
            { шифруем и подписываем сеансовый ключ асимметричным алгоритмом }
            KeyCph := Encrypt (Key,aPublicKey,aPrivateKey,aPKCryptoType,aHashType);
            { объединяем }
            anOutput.Position := 0;
            WriteStrL (anOutput,KeyCph);
            Stream.Position := 0;
            WriteStreamL (anOutput,Stream);
            WriteStrL (anOutput,Sgn);
        finally
            _FillChar ( Key, Length (Key), $00 );
            _FillChar ( KeyCph, Length (KeyCph), $00 );
            _FillChar ( Cph, Length (Cph), $00 );
            _FillChar ( Sgn, Length (Sgn), $00 );
            FreeAndNil (Stream);
        end;
    except on E : Exception do
        _raise (['Encrypt',ERR_TCRYPTOKERNEL_ENCRYPT_HYBRID,E],
                ['{A738BAAC-4AAB-437F-A910-A025F240171D}']);
    end;
end;

class procedure TCryptoKernel.Encrypt (const anInput: TStream;
                                       out anOutput: TStream;
                                       const aPublicKey: String;
                                       const aPrivateKey: String;
                                       const aPKCryptoType: TPKCryptoType;
                                       const aCryptoAlgoritm: TCryptoAlgoritm;
                                       const aCryptoMode: TCryptoMode;
                                       const aHashAlgoritm: THashAlgoritm;
                                       const aRandomType: TRandomType = rtSystem);
var
    Stream : TStream;
    Key    : String;
    KeyCph : String;
    Cph    : String;
    Sgn    : String;
begin
    try
        Stream := TMemoryStream.Create;
        try
            { генерируем сеансовый ключ  }
            GenerateKey (Key,aCryptoAlgoritm,aRandomType);
            { шифруем сообщение симметричным алгоритмом }
            anInput.Position := 0;
            Encrypt (anInput,Stream,Key,aCryptoAlgoritm,aCryptoMode,aRandomType);
            { подписываем хэш от сообщения сеансовым ключом }
            anInput.Position := 0;
            Sgn := HMAC ( Hash (anInput,aHashAlgoritm), Key, aHashAlgoritm );
            { шифруем и подписываем сеансовый ключ асимметричным алгоритмом }
            KeyCph := Encrypt (Key,aPublicKey,aPrivateKey,aPKCryptoType,aHashAlgoritm);
            { объединяем }
            anOutput.Position := 0;
            WriteStrL (anOutput,KeyCph);
            Stream.Position := 0;
            WriteStreamL (anOutput,Stream);
            WriteStrL (anOutput,Sgn);
        finally
            _FillChar ( Key, Length (Key), $00 );
            _FillChar ( KeyCph, Length (KeyCph), $00 );
            _FillChar ( Cph, Length (Cph), $00 );
            _FillChar ( Sgn, Length (Sgn), $00 );
            FreeAndNil (Stream);
        end;
    except on E : Exception do
        _raise (['Encrypt',ERR_TCRYPTOKERNEL_ENCRYPT_HYBRID,E],
                ['{451E0FC7-EACB-44C4-82E9-C6116FCEB050}']);
    end;
end;

class procedure TCryptoKernel.Encrypt (const anInput: TStream;
                                       out anOutput: TStream;
                                       const aPublicKey: String;
                                       const aPrivateKey: String;
                                       anArgs: array of const);
var
    PKCryptoType   : TPKCryptoType;
    CryptoAlgoritm : TCryptoAlgoritm;
    CryptoMode     : TCryptoMode;
    HashAlgoritm   : THashAlgoritm;
    RandomType     : TRandomType;
begin
    try
        { первый параметр - асимм. шифр }
        PKCryptoType := pkctUnknown;
        if notEmpty (0,anArgs) then
            PKCryptoType := toPKCryptoType (anArgs [0]);
        { второрй параметр - симм. шифр }
        SetLength (CryptoAlgoritm,1);
        CryptoAlgoritm [0] := ctUnknown;
        if notEmpty (1,anArgs) then
            CryptoAlgoritm := toCryptoAlgoritm (anArgs [1]);
        { третий параметр - режим симм. шифрования }
        CryptoMode := cmUnknown;
        if notEmpty (2,anArgs) then
            CryptoMode := toCryptoMode (anArgs [2]);
        { четвертый параметр - хэш-функция }
        SetLength (HashAlgoritm,1);
        HashAlgoritm [0] := htUnknown;
        if notEmpty (3,anArgs) then
            HashAlgoritm := toHashAlgoritm (anArgs [3]);
        { пятый параметр - тип генератора псевдо-случайных чисел }
        RandomType := rtSystem;
        if notEmpty (4,anArgs) then
            RandomType := toRandomType (anArgs [4]);
        Encrypt (anInput,
                 anOutput,
                 aPublicKey,
                 aPrivateKey,
                 PKCryptoType,
                 CryptoAlgoritm,
                 CryptoMode,
                 HashAlgoritm,
                 RandomType);
    except on E : Exception do
        _raise (['Encrypt',ERR_TCRYPTOKERNEL_ENCRYPT_HYBRID,E],
                ['{2AD17EBD-CA29-477A-B324-18A7DD8C5820}']);
    end;
end;

class procedure TCryptoKernel.Decrypt (const anInput: TStream;
                                       out anOutput: TStream;
                                       const aPublicKey: String;
                                       const aPrivateKey: String;
                                       const aPKCryptoType: TPKCryptoType;
                                       const aCryptoType: TCryptoType;
                                       const aCryptoMode: TCryptoMode;
                                       const aHashType: THashType);
var
    KeyCph : String;
    Cph    : TStream;
    Sgn    : String;
    Key    : String;
    Msg    : TStream;
begin
    try
        Cph := TMemoryStream.Create;
        Msg := TMemoryStream.Create;
        try
            anInput.Position := 0;
            KeyCph := ReadStrL (anInput);
            ReadStreamL (anInput,Cph);
            Sgn := ReadStrL (anInput);
            { расшифровываем сеансовый ключ и проверяем подпись }
            Key := Decrypt (KeyCph,aPublicKey,aPrivateKey,aPKCryptoType,aHashType);
            { расшифровываем сообщение с помощью сеансового ключа }
            Cph.Position := 0;
            Decrypt (Cph,Msg,Key,aCryptoType,aCryptoMode);
            { проверяем подпись }
            Msg.Position := 0;
            if (  Sgn = HMAC ( Hash (Msg,aHashType), Key, aHashType )  ) then
            begin
                anOutput.Position := 0;
                Msg.Position := 0;
                anOutput.CopyFrom (Msg,Msg.Size);
            end
            else
                raise Exception.Create (ERR_TCRYPTOKERNEL_VERIFY_FAILED);
        finally
            _FillChar ( KeyCph, Length (KeyCph), $00 );
            _FillChar ( Sgn, Length (Sgn), $00 );
            _FillChar ( Key, Length (Key), $00 );
            if Assigned (Msg) then
                TMemoryStream (Msg).Clear;
            FreeAndNil (Msg);
            FreeAndNil (Cph);
        end;
    except on E : Exception do
        _raise (['Decrypt',ERR_TCRYPTOKERNEL_DECRYPT_HYBRID,E],
                ['{F7723799-E20A-4064-BD4B-E526E6CFE17E}']);
    end;
end;

class procedure TCryptoKernel.Decrypt (const anInput: TStream;
                                       out anOutput: TStream;
                                       const aPublicKey: String;
                                       const aPrivateKey: String;
                                       const aPKCryptoType: TPKCryptoType;
                                       const aCryptoAlgoritm: TCryptoAlgoritm;
                                       const aCryptoMode: TCryptoMode;
                                       const aHashAlgoritm: THashAlgoritm);
var
    KeyCph : String;
    Cph    : TStream;
    Sgn    : String;
    Key    : String;
    Msg    : TStream;
begin
    try
        Cph := TMemoryStream.Create;
        Msg := TMemoryStream.Create;
        try
            anInput.Position := 0;
            KeyCph := ReadStrL (anInput);
            ReadStreamL (anInput,Cph);
            Sgn := ReadStrL (anInput);
            { расшифровываем сеансовый ключ и проверяем подпись }
            Key := Decrypt (KeyCph,aPublicKey,aPrivateKey,aPKCryptoType,aHashAlgoritm);
            { расшифровываем сообщение с помощью сеансового ключа }
            Cph.Position := 0;
            Decrypt (Cph,Msg,Key,aCryptoAlgoritm,aCryptoMode);
            { проверяем подпись }
            Msg.Position := 0;
            if (  Sgn = HMAC ( Hash (Msg,aHashAlgoritm), Key, aHashAlgoritm )  ) then
            begin
                anOutput.Position := 0;
                Msg.Position := 0;
                anOutput.CopyFrom (Msg,Msg.Size);
            end
            else
                raise Exception.Create (ERR_TCRYPTOKERNEL_VERIFY_FAILED);
        finally
            _FillChar ( KeyCph, Length (KeyCph), $00 );
            _FillChar ( Sgn, Length (Sgn), $00 );
            _FillChar ( Key, Length (Key), $00 );
            if Assigned (Msg) then
                TMemoryStream (Msg).Clear;
            FreeAndNil (Msg);
            FreeAndNil (Cph);
        end;
    except on E : Exception do
        _raise (['Decrypt',ERR_TCRYPTOKERNEL_DECRYPT_HYBRID,E],
                ['{5827D4C1-721B-4F7C-9EE2-485DD7A5FE75}']);
    end;
end;

class procedure TCryptoKernel.Decrypt (const anInput: TStream;
                                       out anOutput: TStream;
                                       const aPublicKey: String;
                                       const aPrivateKey: String;
                                       anArgs: array of const);
var
    PKCryptoType   : TPKCryptoType;
    CryptoAlgoritm : TCryptoAlgoritm;
    CryptoMode     : TCryptoMode;
    HashAlgoritm   : THashAlgoritm;
begin
    try
        { первый параметр - асимм. шифр }
        PKCryptoType := pkctUnknown;
        if notEmpty (0,anArgs) then
            PKCryptoType := toPKCryptoType (anArgs [0]);
        { второрй параметр - симм. шифр }
        SetLength (CryptoAlgoritm,1);
        CryptoAlgoritm [0] := ctUnknown;
        if notEmpty (1,anArgs) then
            CryptoAlgoritm := toCryptoAlgoritm (anArgs [1]);
        { третий параметр - режим симм. шифрования }
        CryptoMode := cmUnknown;
        if notEmpty (2,anArgs) then
            CryptoMode := toCryptoMode (anArgs [2]);
        { четвертый параметр - хэш-функция }
        SetLength (HashAlgoritm,1);
        HashAlgoritm [0] := htUnknown;
        if notEmpty (3,anArgs) then
            HashAlgoritm := toHashAlgoritm (anArgs [3]);
        Decrypt (anInput,
                 anOutput,
                 aPublicKey,
                 aPrivateKey,
                 PKCryptoType,
                 CryptoAlgoritm,
                 CryptoMode,
                 HashAlgoritm);
    except on E : Exception do
        _raise (['Decrypt',ERR_TCRYPTOKERNEL_DECRYPT_HYBRID,E],
                ['{2BBF366C-B162-408E-947B-43AF1A363EDE}']);
    end;
end;

{ тесты асимм. шифрования }
{$I 'RSA.test.imp.inc'}
{ тесты симм. шифрования }
{$I 'AES.test.imp.inc'}
{$I 'Anubis.test.imp.inc'}
{$I 'Serpent.test.imp.inc'}
{$I 'Shacal.test.imp.inc'}
{$I 'BlowFish.test.imp.inc'}
{$I 'TwoFish.test.imp.inc'}
{ тесты функций хэширования }
{$I 'SHA.test.imp.inc'}
{$I 'Tiger.test.imp.inc'}
{$I 'MD.test.imp.inc'}
{$I 'RipeMD.test.imp.inc'}

initialization
    RandomInit;

end.
