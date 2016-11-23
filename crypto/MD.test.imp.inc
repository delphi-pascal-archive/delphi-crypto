{******************************************************************************}
{*  MD.test.imp.inc                                                           *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
function TestMD4 : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (MD4_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if ( MD4 (MD4_TEST_VECTORS [I].Plain) <> UpperCase (MD4_TEST_VECTORS [I].Hash) ) then
    {$ELSE}
        if ( MD4 (MD4_TEST_VECTORS [I].Plain) <> LowerCase (MD4_TEST_VECTORS [I].Hash) ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;

function TestMD5 : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (MD5_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if ( MD5 (MD5_TEST_VECTORS [I].Plain) <> UpperCase (MD5_TEST_VECTORS [I].Hash) ) then
    {$ELSE}
        if ( MD5 (MD5_TEST_VECTORS [I].Plain) <> LowerCase (MD5_TEST_VECTORS [I].Hash) ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;
