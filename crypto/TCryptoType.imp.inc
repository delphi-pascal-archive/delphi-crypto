{******************************************************************************}
{*  TCryptoType.imp.inc                                                       *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TCryptoType }
function IntToCryptoType (const aValue: Integer) : TCryptoType;
begin
    Result := ctUnknown;
    case aValue of
        CT_AES128     : Result := ctAES128;
        CT_AES192     : Result := ctAES192;
        CT_AES256     : Result := ctAES256;
        CT_ANUBIS128  : Result := ctAnubis128;
        CT_ANUBIS192  : Result := ctAnubis192;
        CT_ANUBIS256  : Result := ctAnubis256;
        CT_SERPENT128 : Result := ctSerpent128;
        CT_SERPENT192 : Result := ctSerpent192;
        CT_SERPENT256 : Result := ctSerpent256;
        CT_SHACAL     : Result := ctShacal;
        CT_BLOWFISH   : Result := ctBlowFish;
        CT_TWOFISH128 : Result := ctTwoFish128;
        CT_TWOFISH192 : Result := ctTwoFish192;
        CT_TWOFISH256 : Result := ctTwoFish256;
    end;
end;

function CryptoTypeToInt (const aValue: TCryptoType) : Integer;
begin
    Result := CT_UNKNOWN;
    case aValue of
        ctAES128     : Result := CT_AES128;
        ctAES192     : Result := CT_AES192;
        ctAES256     : Result := CT_AES256;
        ctAnubis128  : Result := CT_ANUBIS128;
        ctAnubis192  : Result := CT_ANUBIS192;
        ctAnubis256  : Result := CT_ANUBIS256;
        ctSerpent128 : Result := CT_SERPENT128;
        ctSerpent192 : Result := CT_SERPENT192;
        ctSerpent256 : Result := CT_SERPENT256;
        ctShacal     : Result := CT_SHACAL;
        ctBlowFish   : Result := CT_BLOWFISH;
        ctTwoFish128 : Result := CT_TWOFISH128;
        ctTwoFish192 : Result := CT_TWOFISH192;
        ctTwoFish256 : Result := CT_TWOFISH256;
    end;
end;

function StrToCryptoType (const aValue: String) : TCryptoType;
begin
    Result := ctUnknown;
    if      ( aValue = CRYPTO_TYPE_STRING [CT_AES128] ) then
        Result := ctAES128
    else if ( aValue = CRYPTO_TYPE_STRING [CT_AES192] ) then
        Result := ctAES192
    else if ( aValue = CRYPTO_TYPE_STRING [CT_AES256] ) then
        Result := ctAES256
    else if ( aValue = CRYPTO_TYPE_STRING [CT_ANUBIS128] ) then
        Result := ctAnubis128
    else if ( aValue = CRYPTO_TYPE_STRING [CT_ANUBIS192] ) then
        Result := ctAnubis192
    else if ( aValue = CRYPTO_TYPE_STRING [CT_ANUBIS256] ) then
        Result := ctAnubis256
    else if ( aValue = CRYPTO_TYPE_STRING [CT_SERPENT128] ) then
        Result := ctSerpent128
    else if ( aValue = CRYPTO_TYPE_STRING [CT_SERPENT192] ) then
        Result := ctSerpent192
    else if ( aValue = CRYPTO_TYPE_STRING [CT_SERPENT256] ) then
        Result := ctSerpent256
    else if ( aValue = CRYPTO_TYPE_STRING [CT_SHACAL] ) then
        Result := ctShacal
    else if ( aValue = CRYPTO_TYPE_STRING [CT_BLOWFISH] ) then
        Result := ctBlowFish
    else if ( aValue = CRYPTO_TYPE_STRING [CT_TWOFISH128] ) then
        Result := ctTwoFish128
    else if ( aValue = CRYPTO_TYPE_STRING [CT_TWOFISH192] ) then
        Result := ctTwoFish192
    else if ( aValue = CRYPTO_TYPE_STRING [CT_TWOFISH256] ) then
        Result := ctTwoFish256;
end;

function CryptoTypeToStr (const aValue: TCryptoType) : String;
begin
    Result := CRYPTO_TYPE_STRING [ CryptoTypeToInt (aValue) ];
end;

function toCryptoType (const aValue: TVarRec) : TCryptoType;
begin
    Result := ctUnknown;
    try
        Result := StrToCryptoType ( toString (aValue) );
        if ( Result = ctUnknown ) then
            Result := IntToCryptoType ( toInteger (aValue) );
    except
        Result := ctUnknown;
    end;
end;

function StrToCryptoAlgoritm (const aValue: String) : TCryptoAlgoritm;
const
    Delimiter = '-';
var
    I         : Integer;
    S         : String;
    Buffer    : String;
    Position  : Integer;
    lst       : TStringList;
begin
    SetLength (Result,1);
    Result [0] := ctUnknown;
    try
        if ( aValue <> '' ) then
        begin
            lst := TStringList.Create;
            if Assigned (lst) then
            try
                S := aValue;
                while ( Length (S) > 0 ) do
                begin
                    Position := Pos (Delimiter,S);
                    if ( Position > 0 ) then
                    begin
                        Buffer := Copy ( S, 1, Position - Length (Delimiter) );
                        if ( StrToCryptoType (Buffer) <> ctUnknown ) then
                            lst.Add (Buffer);
                        S := Copy ( S, Position + Length (Delimiter), Length (S) );
                    end
                    else
                    begin
                        if ( StrToCryptoType (S) <> ctUnknown ) then
                            lst.Add (S);
                        Break;
                    end;
                end;
                SetLength (Result,lst.Count);
                for I := 0 to lst.Count-1 do
                begin
                    Result [I] := StrToCryptoType (lst [I]);
                end;
            finally
                FreeAndNil (lst);
            end;
        end;
    except
        SetLength (Result,1);
        Result [0] := ctUnknown;
    end;
end;

function CryptoAlgoritmToStr (const aValue: TCryptoAlgoritm) : String;
var
    I : Integer;
begin
    Result := '';
    try
        for I := 0 to High (aValue) do
            if ( I = 0 ) then
                Result := Format ('%s',[ CryptoTypeToStr (aValue [I]) ])
            else
                Result := Format ('%s-%s',[ Result, CryptoTypeToStr (aValue [I]) ]);
    except
        Result := '';
    end;
end;

function toCryptoAlgoritm (const aValue: TVarRec) : TCryptoAlgoritm;
begin
    SetLength (Result,1);
    Result [0] := ctUnknown;
    try
        Result := StrToCryptoAlgoritm ( toString (aValue) );
        if ( Result [0] = ctUnknown ) then
            Result [0] := IntToCryptoType ( toInteger (aValue) );
    except
        SetLength (Result,1);
        Result [0] := ctUnknown;
    end;
end;
