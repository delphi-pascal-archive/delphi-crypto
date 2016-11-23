unit CryptoSystem;
{******************************************************************************}
{*  Crypto System Unit                                                        *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
interface

{$I '../std.inc'}

uses
    Windows, SysUtils, Classes, Variants, DateUtils,
    Utils, Strings, Versions, VarRecs,
    EClasses, Kernel, ProtoClasses, CryptoClasses, MetaClasses, 
    SQLite3, SQLite3DLL, SQLiteTable3;

type
{$M+}
    TCrypto = class;
{$M-}

{ описание крипто-системы }
{$I 'TCrypto.int.inc'}

resourcestring
    CLS_TCRYPTO_NAME           = 'крипто-система';
    PRP_TCRYPTO_ALG_ASYMMETRIC = 'асимметричный шифр';
    PRP_TCRYPTO_ALG_SYMMETRIC  = 'симметричный шифр';
    PRP_TCRYPTO_MODE_SYMMETRIC = 'режим симметричного шифра';
    PRP_TCRYPTO_ALG_HASH       = 'функция хэширования';
    PRP_TCRYPTO_GEN_RANDOM     = 'генератор псевдо-случайных чисел';

const
    _alg_asymmetric = 4;
    _alg_symmetric  = 5;
    _mode_symmetric = 6;
    _alg_hash       = 7;
    _gen_random     = 8;

{ ошибки крипто-системы }
{$I 'TCrypto.err.inc'}

implementation

uses
    Crypto;

{ реализация крипто-системы }
{$I 'TCrypto.imp.inc'}


end.