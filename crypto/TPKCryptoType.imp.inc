{******************************************************************************}
{*  TPKCryptoType.imp.inc                                                     *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TPKCryptoType }
function IntToPKCryptoType (const aValue: Integer) : TPKCryptoType;
begin
    Result := pkctUnknown;
    case aValue of
        PKCT_RSA1024 : Result := pkctRSA1024;
        PKCT_RSA2048 : Result := pkctRSA2048;
        PKCT_RSA4096 : Result := pkctRSA4096;
        PKCT_RSA8192 : Result := pkctRSA8192;
    end;
end;

function PKCryptoTypeToInt (const aValue: TPKCryptoType) : Integer;
begin
    Result := PKCT_UNKNOWN;
    case aValue of
        pkctRSA1024 : Result := PKCT_RSA1024;
        pkctRSA2048 : Result := PKCT_RSA2048;
        pkctRSA4096 : Result := PKCT_RSA4096;
        pkctRSA8192 : Result := PKCT_RSA8192;
    end;
end;

function StrToPKCryptoType (const aValue: String) : TPKCryptoType;
begin
    Result := pkctUnknown;
    if      ( aValue = PKCRYPTO_TYPE_STRING [PKCT_RSA1024] ) then
        Result := pkctRSA1024
    else if ( aValue = PKCRYPTO_TYPE_STRING [PKCT_RSA2048] ) then
        Result := pkctRSA2048
    else if ( aValue = PKCRYPTO_TYPE_STRING [PKCT_RSA4096] ) then
        Result := pkctRSA4096
    else if ( aValue = PKCRYPTO_TYPE_STRING [PKCT_RSA8192] ) then
        Result := pkctRSA8192;
end;

function PKCryptoTypeToStr (const aValue: TPKCryptoType) : String;
begin
    Result := PKCRYPTO_TYPE_STRING [ PKCryptoTypeToInt (aValue) ];
end;

function toPKCryptoType (const aValue: TVarRec) : TPKCryptoType;
begin
    Result := pkctUnknown;
    try
        Result := StrToPKCryptoType ( toString (aValue) );
        if ( Result = pkctUnknown ) then
            Result := IntToPKCryptoType ( toInteger (aValue) );
    except
        Result := pkctUnknown;
    end;
end;
