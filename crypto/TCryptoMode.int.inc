{******************************************************************************}
{*  TCryptoMode.int.inc                                                       *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ режимы шифрования }
type
    PCryptoMode = ^TCryptoMode;
    TCryptoMode = (
        cmUnknown
        {$IFDEF ECB}
        , cmECB
        {$ENDIF ECB}
        {$IFDEF CBC}
        , cmCBC
        {$ENDIF CBC}
        {$IFDEF CFB}
        , cmCFB
        {$ENDIF CFB}
        {$IFDEF OFB}
        , cmOFB
        {$ENDIF OFB}
    );

const
    CM_UNKNOWN = 0;
    CM_ECB     = 1;
    CM_CBC     = 2;
    CM_CFB     = 3;
    CM_OFB     = 4;

    CRYPTO_MODE_STRING  : array [CM_UNKNOWN..CM_OFB] of String =
    ( '',
      'ECB',
      'CBC',
      'CFB',
      'OFB' );

function StrToCryptoMode (const aValue: String) : TCryptoMode;
function CryptoModeToStr (const aValue: TCryptoMode) : String;

function IntToCryptoMode (const aValue: Integer) : TCryptoMode;
function CryptoModeToInt (const aValue: TCryptoMode) : Integer;

function toCryptoMode (const aValue: TVarRec) : TCryptoMode;
