{******************************************************************************}
{*  TCryptoMode.imp.inc                                                       *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TCryptoMode }
function IntToCryptoMode (const aValue: Integer) : TCryptoMode;
begin
    Result := cmUnknown;
    case aValue of
        {$IFDEF ECB}
        CM_ECB : Result := cmECB;
        {$ENDIF ECB}
        {$IFDEF CBC}
        CM_CBC : Result := cmCBC;
        {$ENDIF CBC}
        {$IFDEF CFB}
        CM_CFB : Result := cmCFB;
        {$ENDIF CFB}
        {$IFDEF OFB}
        CM_OFB : Result := cmOFB;
        {$ENDIF OFB}
     end;
end;

function CryptoModeToInt (const aValue: TCryptoMode) : Integer;
begin
    Result := CM_UNKNOWN;
    case aValue of
        {$IFDEF ECB}
        cmECB : Result := CM_ECB;
        {$ENDIF ECB}
        {$IFDEF CBC}
        cmCBC : Result := CM_CBC;
        {$ENDIF CBC}
        {$IFDEF CFB}
        cmCFB : Result := CM_CFB;
        {$ENDIF CFB}
        {$IFDEF OFB}
        cmOFB : Result := CM_OFB;
        {$ENDIF OFB}
    end;
end;

function StrToCryptoMode (const aValue: String) : TCryptoMode;
begin
    Result := cmUnknown;
    {$IFDEF ECB}
    if ( aValue = CRYPTO_MODE_STRING [CM_ECB] ) then
        Result := cmECB;
    {$ENDIF ECB}
    {$IFDEF CBC}
    if ( aValue = CRYPTO_MODE_STRING [CM_CBC] ) then
        Result := cmCBC;
    {$ENDIF CBC}
    {$IFDEF CFB}
    if ( aValue = CRYPTO_MODE_STRING [CM_CFB] ) then
        Result := cmCFB;
    {$ENDIF CFB}
    {$IFDEF OFB}
    if ( aValue = CRYPTO_MODE_STRING [CM_OFB] ) then
        Result := cmOFB;
    {$ENDIF OFB}
end;

function CryptoModeToStr (const aValue: TCryptoMode) : String;
begin
    Result := CRYPTO_MODE_STRING [ CryptoModeToInt (aValue) ];
end;

function toCryptoMode (const aValue: TVarRec) : TCryptoMode;
begin
    Result := cmUnknown;
    try
        Result := StrToCryptoMode ( toString (aValue) );
        if ( Result = cmUnknown ) then
            Result := IntToCryptoMode ( toInteger (aValue) );
    except
        Result := cmUnknown;
    end;
end;