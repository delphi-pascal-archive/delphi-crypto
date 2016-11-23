{******************************************************************************}
{*  RipeMD.test.imp.inc                                                       *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
function TestRipeMD128 : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (RIPEMD128_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if ( RipeMD128 (RIPEMD128_TEST_VECTORS [I].Plain) <> UpperCase (RIPEMD128_TEST_VECTORS [I].Hash) ) then
    {$ELSE}
        if ( RipeMD128 (RIPEMD128_TEST_VECTORS [I].Plain) <> LowerCase (RIPEMD128_TEST_VECTORS [I].Hash) ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;

function TestRipeMD160 : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (RIPEMD160_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if ( RipeMD160 (RIPEMD160_TEST_VECTORS [I].Plain) <> UpperCase (RIPEMD160_TEST_VECTORS [I].Hash) ) then
    {$ELSE}
        if ( RipeMD160 (RIPEMD160_TEST_VECTORS [I].Plain) <> LowerCase (RIPEMD160_TEST_VECTORS [I].Hash) ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;