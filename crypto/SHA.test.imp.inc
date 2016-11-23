{******************************************************************************}
{*  SHA.test.imp.inc                                                          *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
function TestSHA1 : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (SHA1_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if ( SHA1 (SHA1_TEST_VECTORS [I].Plain) <> UpperCase (SHA1_TEST_VECTORS [I].Hash) ) then
    {$ELSE}
        if ( SHA1 (SHA1_TEST_VECTORS [I].Plain) <> LowerCase (SHA1_TEST_VECTORS [I].Hash) ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;

function TestSHA256 : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (SHA256_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if ( SHA256 (SHA256_TEST_VECTORS [I].Plain) <> UpperCase (SHA256_TEST_VECTORS [I].Hash) ) then
    {$ELSE}
        if ( SHA256 (SHA256_TEST_VECTORS [I].Plain) <> LowerCase (SHA256_TEST_VECTORS [I].Hash) ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;

function TestSHA384 : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (SHA384_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if ( SHA384 (SHA384_TEST_VECTORS [I].Plain) <> UpperCase (SHA384_TEST_VECTORS [I].Hash) ) then
    {$ELSE}
        if ( SHA384 (SHA384_TEST_VECTORS [I].Plain) <> LowerCase (SHA384_TEST_VECTORS [I].Hash) ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;

function TestSHA512 : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (SHA512_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if ( SHA512 (SHA512_TEST_VECTORS [I].Plain) <> UpperCase (SHA512_TEST_VECTORS [I].Hash) ) then
    {$ELSE}
        if ( SHA512 (SHA512_TEST_VECTORS [I].Plain) <> LowerCase (SHA512_TEST_VECTORS [I].Hash) ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;