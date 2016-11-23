{******************************************************************************}
{*  TPKCryptoType.int.inc                                                     *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ типы шифрования с открытым ключом }
type
    PPKCryptoType = ^TPKCryptoType;
    TPKCryptoType = (pkctUnknown,
                     pkctRSA1024, pkctRSA2048, pkctRSA4096, pkctRSA8192);

const
    PKCT_UNKNOWN = 0;
    PKCT_RSA1024 = 1;
    PKCT_RSA2048 = 2;
    PKCT_RSA4096 = 3;
    PKCT_RSA8192 = 4;

    PKCRYPTO_TYPE_STRING  : array [PKCT_UNKNOWN..PKCT_RSA8192] of String =
    ( '',
      'RSA1024', 'RSA2048', 'RSA4096', 'RSA8192' );

function StrToPKCryptoType (const aValue: String) : TPKCryptoType;
function PKCryptoTypeToStr (const aValue: TPKCryptoType) : String;

function IntToPKCryptoType (const aValue: Integer) : TPKCryptoType;
function PKCryptoTypeToInt (const aValue: TPKCryptoType) : Integer;

function toPKCryptoType (const aValue: TVarRec) : TPKCryptoType;
