{******************************************************************************}
{*  TKey.imp.inc                                                              *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
procedure NULL_KEY (var aKey: PKey;
                    const aSize: WORD);
var
    oSize : WORD;
    I     : Integer;
begin
    oSize := aSize div 8;
    for I := 0 to oSize - 1 do
        aKey^ [I] := $00;
end;

procedure Key (anArgs: array of const;
               out aResult: PKey);
var
    I : Integer;
begin
    aResult := NIL;
    if (  ( High (anArgs) = 0 ) and IsHex ( toString (anArgs [0]) )  ) then
    begin
        HexToKey ( toString (anArgs [0]), aResult );
        Exit;
    end;
    NULL_KEY ( aResult, High (anArgs) );
    for I := 0 to High (anArgs) do
    begin
        aResult^ [I] := toInteger (anArgs [I]);
    end;
end;

procedure HexToKey (const aValue: Hex;
                    out aResult: PKey);
var
    I      : Integer;
    J      : Integer;
    Buffer : String [3];
begin
    if not IsHex (aValue) then
        Exit;
    NULL_KEY ( aResult, Length (aValue) div 2 );
    I := 0;
    J := 0;
    while ( I < Length (aValue) ) do
    begin
        Inc (I);
        if ( I mod 2 = 0 ) then
        begin
            Buffer := Format ('$%s%s',[ aValue [I-1], aValue [I] ]);
            aResult^ [J] := StrToInt (Buffer);
            Inc (J);
        end;
    end;
end;

function KeyToHex (const aValue: PKey;
                   const aSize: WORD) : Hex;
var
    I : Integer;
begin
    Result := '';
    for I := 0 to ( aSize div 8 -1 ) do
        Result := Result + IntToHex (aValue^ [I],2);
end;

function Key128 (anArgs: array of const) : TKey128;
var
    I : Integer;
begin
    Result := NULL_128_KEY;
    if (   ( High (anArgs) = 0 ) and
           IsHex ( toString (anArgs [0]) ) and
           (  Length ( toString (anArgs [0]) ) >= High (TKey128)  )   ) then
    begin
        Result := HexToKey128 ( toString (anArgs [0]) );
        Exit;
    end;
    for I := 0 to High (anArgs) do
    begin
        Result [I] := toInteger (anArgs [I]);
        if ( I >= High (TKey128) ) then
            Break;
    end;
end;

function Key128ToHex (const aValue: TKey128) : Hex;
var
    I : Integer;
begin
    Result := '';
    for I := Low (TKey128) to High (TKey128) do
        Result := Result + IntToHex (aValue [I],2);
end;

function HexToKey128 (const aValue: Hex) : TKey128;
var
    I      : Integer;
    J      : Integer;
    Buffer : String [3];
begin
    Result := NULL_128_KEY;
    if not IsHex (aValue) then
        Exit;
    I := 0;
    J := 0;
    while ( I < Length (aValue) ) do
    begin
        Inc (I);
        if ( I mod 2 = 0 ) then
        begin
            if ( J > High (TKey128) )  then
                Break;
            Buffer := Format ('$%s%s',[ aValue [I-1], aValue [I] ]);
            Result [J] := StrToInt (Buffer);
            Inc (J);
        end;
    end;
end;

function Key192 (anArgs: array of const) : TKey192;
var
    I : Integer;
begin
    Result := NULL_192_KEY;
    if (   ( High (anArgs) = 0 ) and
           IsHex ( toString (anArgs [0]) ) and
           (  Length ( toString (anArgs [0]) ) >= High (TKey192)  )   ) then
    begin
        Result := HexToKey192 ( toString (anArgs [0]) );
        Exit;
    end;
    for I := 0 to High (anArgs) do
    begin
        Result [I] := toInteger (anArgs [I]);
        if ( I >= High (TKey192) ) then
            Break;
    end;
end;

function Key192ToHex (const aValue: TKey192) : Hex;
var
    I : Integer;
begin
    Result := '';
    for I := Low (TKey192) to High (TKey192) do
        Result := Result + IntToHex (aValue [I],2);
end;

function HexToKey192 (const aValue: Hex) : TKey192;
var
    I      : Integer;
    J      : Integer;
    Buffer : String [3];
begin
    Result := NULL_192_KEY;
    if not IsHex (aValue) then
        Exit;
    I := 0;
    J := 0;
    while ( I < Length (aValue) ) do
    begin
        Inc (I);
        if ( I mod 2 = 0 ) then
        begin
            if ( J > High (TKey192) )  then
                Break;
            Buffer := Format ('$%s%s',[ aValue [I-1], aValue [I] ]);
            Result [J] := StrToInt (Buffer);
            Inc (J);
        end;
    end;
end;

function Key256 (anArgs: array of const) : TKey256;
var
    I : Integer;
begin
    Result := NULL_256_KEY;
    if (   ( High (anArgs) = 0 ) and
           IsHex ( toString (anArgs [0]) ) and
           (  Length ( toString (anArgs [0]) ) >= High (TKey256)  )   ) then
    begin
        Result := HexToKey256 ( toString (anArgs [0]) );
        Exit;
    end;
    for I := 0 to High (anArgs) do
    begin
        Result [I] := toInteger (anArgs [I]);
        if ( I >= High (TKey256) ) then
            Break;
    end;
end;

function Key256ToHex (const aValue: TKey256) : Hex;
var
    I : Integer;
begin
    Result := '';
    for I := Low (TKey256) to High (TKey256) do
        Result := Result + IntToHex (aValue [I],2);
end;

function HexToKey256 (const aValue: Hex) : TKey256;
var
    I      : Integer;
    J      : Integer;
    Buffer : String [3];
begin
    Result := NULL_256_KEY;
    if not IsHex (aValue) then
        Exit;
    I := 0;
    J := 0;
    while ( I < Length (aValue) ) do
    begin
        Inc (I);
        if ( I mod 2 = 0 ) then
        begin
            if ( J > High (TKey256) )  then
                Break;
            Buffer := Format ('$%s%s',[ aValue [I-1], aValue [I] ]);
            Result [J] := StrToInt (Buffer);
            Inc (J);
        end;
    end;
end;

function Key512 (anArgs: array of const) : TKey512;
var
    I : Integer;
begin
    Result := NULL_512_KEY;
    if (   ( High (anArgs) = 0 ) and
           IsHex ( toString (anArgs [0]) ) and
           (  Length ( toString (anArgs [0]) ) >= High (TKey128)  )   ) then
    begin
        Result := HexToKey512 ( toString (anArgs [0]) );
        Exit;
    end;
    for I := 0 to High (anArgs) do
    begin
        Result [I] := toInteger (anArgs [I]);
        if ( I >= High (TKey512) ) then
            Break;
    end;
end;

function Key512ToHex (const aValue: TKey512) : Hex;
var
    I : Integer;
begin
    Result := '';
    for I := Low (TKey512) to High (TKey512) do
        Result := Result + IntToHex (aValue [I],2);
end;

function HexToKey512 (const aValue: Hex) : TKey512;
var
    I      : Integer;
    J      : Integer;
    Buffer : String [3];
begin
    Result := NULL_512_KEY;
    if not IsHex (aValue) then
        Exit;
    I := 0;
    J := 0;
    while ( I < Length (aValue) ) do
    begin
        Inc (I);
        if ( I mod 2 = 0 ) then
        begin
            if ( J > High (TKey512) )  then
                Break;
            Buffer := Format ('$%s%s',[ aValue [I-1], aValue [I] ]);
            Result [J] := StrToInt (Buffer);
            Inc (J);
        end;
    end;
end;
