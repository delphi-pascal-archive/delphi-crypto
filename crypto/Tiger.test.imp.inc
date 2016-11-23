{******************************************************************************}
{*  Tiger.test.imp.inc                                                        *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
function TestTiger128 : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (TIGER128_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if ( Tiger128 (TIGER128_TEST_VECTORS [I].Plain) <> UpperCase (TIGER128_TEST_VECTORS [I].Hash) ) then
    {$ELSE}
        if ( Tiger128 (TIGER128_TEST_VECTORS [I].Plain) <> LowerCase (TIGER128_TEST_VECTORS [I].Hash) ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;

function TestTiger160 : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (TIGER160_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if ( Tiger160 (TIGER160_TEST_VECTORS [I].Plain) <> UpperCase (TIGER160_TEST_VECTORS [I].Hash) ) then
    {$ELSE}
        if ( Tiger160 (TIGER160_TEST_VECTORS [I].Plain) <> LowerCase (TIGER160_TEST_VECTORS [I].Hash) ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;

function TestTiger192 : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (TIGER192_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if ( Tiger192 (TIGER192_TEST_VECTORS [I].Plain) <> UpperCase (TIGER192_TEST_VECTORS [I].Hash) ) then
    {$ELSE}
        if ( Tiger192 (TIGER192_TEST_VECTORS [I].Plain) <> LowerCase (TIGER192_TEST_VECTORS [I].Hash) ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;