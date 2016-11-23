{******************************************************************************}
{*  TCryptoType.int.inc                                                       *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ типы шифрования }
type
    PCryptoType = ^TCryptoType;
    TCryptoType = (ctUnknown,
                   ctAES128, ctAES192, ctAES256,
                   ctAnubis128, ctAnubis192, ctAnubis256,
                   ctSerpent128, ctSerpent192, ctSerpent256,
                   ctShacal,
                   ctBlowFish,
                   ctTwoFish128, ctTwoFish192, ctTwoFish256);
    PCryptoAlgoritm = ^TCryptoAlgoritm;
    TCryptoAlgoritm = array of TCryptoType;

const
    CT_UNKNOWN    = 0;
    CT_AES128     = 1;
    CT_AES192     = 2;
    CT_AES256     = 3;
    CT_ANUBIS128  = 4;
    CT_ANUBIS192  = 5;
    CT_ANUBIS256  = 6;
    CT_SERPENT128 = 7;
    CT_SERPENT192 = 8;
    CT_SERPENT256 = 9;
    CT_SHACAL     = 10;
    CT_BLOWFISH   = 11;
    CT_TWOFISH128 = 12;
    CT_TWOFISH192 = 13;
    CT_TWOFISH256 = 14;

    CRYPTO_TYPE_STRING : array [CT_UNKNOWN..CT_TWOFISH256] of String =
    ( '',
      'AES128', 'AES192', 'AES256',
      'Anubis128', 'Anubis192', 'Anubis256',
      'Serpent128', 'Serpent192', 'Serpent256',
      'Shacal',
      'BlowFish',
      'TwoFish128', 'TwoFish192', 'TwoFish256' );

function StrToCryptoType (const aValue: String) : TCryptoType;
function CryptoTypeToStr (const aValue: TCryptoType) : String;

function IntToCryptoType (const aValue: Integer) : TCryptoType;
function CryptoTypeToInt (const aValue: TCryptoType) : Integer;

function toCryptoType (const aValue: TVarRec) : TCryptoType;

function StrToCryptoAlgoritm (const aValue: String) : TCryptoAlgoritm;
function CryptoAlgoritmToStr (const aValue: TCryptoAlgoritm) : String;

function toCryptoAlgoritm (const aValue: TVarRec) : TCryptoAlgoritm;
