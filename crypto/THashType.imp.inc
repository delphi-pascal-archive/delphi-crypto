{******************************************************************************}
{*  THashType.imp.inc                                                         *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ THashType }
function IntToHashType (const aValue: Integer) : THashType;
begin
    Result := htUnknown;
    case aValue of
        HT_SHA1      : Result := htSHA1;
        HT_SHA256    : Result := htSHA256;
        HT_SHA384    : Result := htSHA384;
        HT_SHA512    : Result := htSHA512;
        HT_MD5       : Result := htMD5;
        HT_RIPEMD128 : Result := htRipeMD128;
        HT_RIPEMD160 : Result := htRipeMD160;
        HT_TIGER128  : Result := htTiger128;
        HT_TIGER160  : Result := htTiger160;
        HT_TIGER192  : Result := htTiger192;
    end;
end;

function HashTypeToInt (const aValue: THashType) : Integer;
begin
    Result := HT_UNKNOWN;
    case aValue of
        htSHA1      : Result := HT_SHA1;
        htSHA256    : Result := HT_SHA256;
        htSHA384    : Result := HT_SHA384;
        htSHA512    : Result := HT_SHA512;
        htMD5       : Result := HT_MD5;
        htRipeMD128 : Result := HT_RIPEMD128;
        htRipeMD160 : Result := HT_RIPEMD160;
        htTiger128  : Result := HT_TIGER128;
        htTiger160  : Result := HT_TIGER160;
        htTiger192  : Result := HT_TIGER192;
    end;
end;

function StrToHashType (const aValue: String) : THashType;
begin
    Result := htUnknown;
    if      ( aValue = HASH_TYPE_STRING [HT_SHA1] ) then
        Result := htSHA1
    else if ( aValue = HASH_TYPE_STRING [HT_SHA256] ) then
        Result := htSHA256
    else if ( aValue = HASH_TYPE_STRING [HT_SHA384] ) then
        Result := htSHA384
    else if ( aValue = HASH_TYPE_STRING [HT_SHA512] ) then
        Result := htSHA512
    else if ( aValue = HASH_TYPE_STRING [HT_MD5] ) then
        Result := htMD5
    else if ( aValue = HASH_TYPE_STRING [HT_RIPEMD128] ) then
        Result := htRipeMD128
    else if ( aValue = HASH_TYPE_STRING [HT_RIPEMD160] ) then
        Result := htRipeMD160
    else if ( aValue = HASH_TYPE_STRING [HT_TIGER128] ) then
        Result := htTiger128
    else if ( aValue = HASH_TYPE_STRING [HT_TIGER160] ) then
        Result := htTiger160
    else if ( aValue = HASH_TYPE_STRING [HT_TIGER192] ) then
        Result := htTiger192;
end;

function HashTypeToStr (const aValue: THashType) : String;
begin
    Result := HASH_TYPE_STRING [ HashTypeToInt (aValue) ];
end;

function toHashType (const aValue: TVarRec) : THashType;
begin
    Result := htUnknown;
    try
        Result := StrToHashType ( toString (aValue) );
        if ( Result = htUnknown ) then
            Result := IntToHashType ( toInteger (aValue) );
    except
        Result := htUnknown;
    end;
end;

function StrToHashAlgoritm (const aValue: String) : THashAlgoritm;
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
    Result [0] := htUnknown;
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
                        if ( StrToHashType (Buffer) <> htUnknown ) then
                            lst.Add (Buffer);
                        S := Copy ( S, Position + Length (Delimiter), Length (S) );
                    end
                    else
                    begin
                        if ( StrToHashType (S) <> htUnknown ) then
                            lst.Add (S);
                        Break;
                    end;
                end;
                SetLength (Result,lst.Count);
                for I := 0 to lst.Count-1 do
                begin
                    Result [I] := StrToHashType (lst [I]);
                end;
            finally
                FreeAndNil (lst);
            end;
        end;
    except
        SetLength (Result,1);
        Result [0] := htUnknown;
    end;
end;

function HashAlgoritmToStr (const aValue: THashAlgoritm) : String;
var
    I : Integer;
begin
    Result := '';
    try
        for I := 0 to High (aValue) do
            if ( I = 0 ) then
                Result := Format ('%s',[ HashTypeToStr (aValue [I]) ])
            else
                Result := Format ('%s-%s',[ Result, HashTypeToStr (aValue [I]) ]);
    except
        Result := '';
    end;
end;

function toHashAlgoritm (const aValue: TVarRec) : THashAlgoritm;
begin
    SetLength (Result,1);
    Result [0] := htUnknown;
    try
        Result := StrToHashAlgoritm ( toString (aValue) );
        if ( Result [0] = htUnknown ) then
            Result [0] := IntToHashType ( toInteger (aValue) );
    except
        SetLength (Result,1);
        Result [0] := htUnknown;
    end;
end;