{******************************************************************************}
{*  THashType.int.inc                                                         *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ типы хэширования }
type
    PHashType = ^THashType;
    THashType = (htUnknown,
                 htSHA1, htSHA256, htSHA384, htSHA512,
                 htMD5, htRipeMD128, htRipeMD160,
                 htTiger128, htTiger160, htTiger192);
    PHashAlgoritm = ^THashAlgoritm;
    THashAlgoritm = array of THashType;

const
    HT_UNKNOWN   = 0;
    HT_SHA1      = 1;
    HT_SHA256    = 2;
    HT_SHA384    = 3;
    HT_SHA512    = 4;
    HT_MD5       = 5;
    HT_RIPEMD128 = 6;
    HT_RIPEMD160 = 7;
    HT_TIGER128  = 8;
    HT_TIGER160  = 9;
    HT_TIGER192  = 10;

    HASH_TYPE_STRING : array [HT_UNKNOWN..HT_TIGER192] of String =
    ( '',
      'SHA1', 'SHA256', 'SHA384', 'SHA512',
      'MD5', 'RipeMD128', 'RipeMD160',
      'Tiger128', 'Tiger160', 'Tiger192' );

function StrToHashType (const aValue: String) : THashType;
function HashTypeToStr (const aValue: THashType) : String;

function IntToHashType (const aValue: Integer) : THashType;
function HashTypeToInt (const aValue: THashType) : Integer;

function toHashType (const aValue: TVarRec) : THashType;

function HashAlgoritmToStr (const aValue: THashAlgoritm) : String;
function StrToHashAlgoritm (const aValue: String) : THashAlgoritm;

function toHashAlgoritm (const aValue: TVarRec) : THashAlgoritm;
