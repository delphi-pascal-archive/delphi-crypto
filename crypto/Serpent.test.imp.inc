{******************************************************************************}
{*  Serpent.test.imp.inc                                                       *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{$IFDEF ECB}
function TestEncryptSerpentECB (const aKey: String;
                                const aValue: String) : String;
var
    ctx       : TSerpentCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    Value     : TSerpent128Block;
    Cipher    : TSerpent128Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (aKey) = 32*2 ) then
            begin
                Key256 := HexToKey256 (aKey);
                KeySize := 256;
                ErrorCode := SP_ECB_Init (Key256,KeySize,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := SP_ECB_Init (Key192,KeySize,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := SP_ECB_Init (Key128,KeySize,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_SERPENT_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToSerpentIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_SERPENT_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_SERPENT_128_BLOCK;
            ErrorCode := SP_ECB_Encrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := SerpentIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            Value := NULL_SERPENT_128_BLOCK;
            Cipher := NULL_SERPENT_128_BLOCK;

            SP_ECB_Reset (ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SERPENT_ECB_ENCRYPT,[KeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

function TestDecryptSerpentECB (const aKey: String;
                                const aValue: String) : String;
var
    ctx       : TSerpentCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    Value     : TSerpent128Block;
    Cipher    : TSerpent128Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (aKey) = 32*2 ) then
            begin
                Key256 := HexToKey256 (aKey);
                KeySize := 256;
                ErrorCode := SP_ECB_Init (Key256,KeySize,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := SP_ECB_Init (Key192,KeySize,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := SP_ECB_Init (Key128,KeySize,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_SERPENT_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToSerpentIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_SERPENT_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_SERPENT_128_BLOCK;
            ErrorCode := SP_ECB_Decrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := SerpentIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            Value := NULL_SERPENT_128_BLOCK;
            Cipher := NULL_SERPENT_128_BLOCK;

            SP_ECB_Reset (ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SERPENT_ECB_DECRYPT,[KeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

function TestSerpentECB : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (SERPENT_ECB_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if (  TestEncryptSerpentECB ( UpperCase (SERPENT_ECB_TEST_VECTORS [I].Key),
                                      UpperCase (SERPENT_ECB_TEST_VECTORS [I].Plain) ) <> UpperCase (SERPENT_ECB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptSerpentECB ( UpperCase (SERPENT_ECB_TEST_VECTORS [I].Key),
                                      UpperCase (SERPENT_ECB_TEST_VECTORS [I].Cipher) ) <> UpperCase (SERPENT_ECB_TEST_VECTORS [I].Plain)  ) then
    {$ELSE}
        if (  TestEncryptSerpentECB ( LowerCase (SERPENT_ECB_TEST_VECTORS [I].Key),
                                      LowerCase (SERPENT_ECB_TEST_VECTORS [I].Plain) ) <> LowerCase (SERPENT_ECB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptSerpentECB ( LowerCase (SERPENT_ECB_TEST_VECTORS [I].Key),
                                      LowerCase (SERPENT_ECB_TEST_VECTORS [I].Cipher) ) <> LowerCase (SERPENT_ECB_TEST_VECTORS [I].Plain)  ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;
{$ENDIF ECB}

{$IFDEF CBC}
function TestEncryptSerpentCBC (const aKey: String;
                                const anIV: String;
                                const aValue: String) : String;
var
    ctx       : TSerpentCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    IV        : TSerpent128Block;
    Value     : TSerpent128Block;
    Cipher    : TSerpent128Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 16*2 ) then
            begin
                IV := HexToSerpentIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_SERPENT_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 32*2 ) then
            begin
                Key256 := HexToKey256 (aKey);
                KeySize := 256;
                ErrorCode := SP_CBC_Init (Key256,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := SP_CBC_Init (Key192,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := SP_CBC_Init (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_SERPENT_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToSerpentIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_SERPENT_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_SERPENT_128_BLOCK;
            ErrorCode := SP_CBC_Encrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := SerpentIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            IV := NULL_SERPENT_128_BLOCK;
            Value := NULL_SERPENT_128_BLOCK;
            Cipher := NULL_SERPENT_128_BLOCK;

            SP_CBC_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SERPENT_CBC_ENCRYPT,[KeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

function TestDecryptSerpentCBC (const aKey: String;
                                const anIV: String;
                                const aValue: String) : String;
var
    ctx       : TSerpentCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    IV        : TSerpent128Block;
    Value     : TSerpent128Block;
    Cipher    : TSerpent128Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 16*2 ) then
            begin
                IV := HexToSerpentIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_SERPENT_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 32*2 ) then
            begin
                Key256 := HexToKey256 (aKey);
                KeySize := 256;
                ErrorCode := SP_CBC_Init (Key256,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := SP_CBC_Init (Key192,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := SP_CBC_Init (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_SERPENT_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToSerpentIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_SERPENT_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_SERPENT_128_BLOCK;
            ErrorCode := SP_CBC_Decrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := SerpentIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            IV := NULL_SERPENT_128_BLOCK;
            Value := NULL_SERPENT_128_BLOCK;
            Cipher := NULL_SERPENT_128_BLOCK;

            SP_CBC_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SERPENT_CBC_DECRYPT,[KeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

function TestSerpentCBC : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (SERPENT_CBC_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if (  TestEncryptSerpentCBC ( UpperCase (SERPENT_CBC_TEST_VECTORS [I].Key),
                                      UpperCase (SERPENT_CBC_TEST_VECTORS [I].IV),
                                      UpperCase (SERPENT_CBC_TEST_VECTORS [I].Plain) ) <> UpperCase (SERPENT_CBC_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptSerpentCBC ( UpperCase (SERPENT_CBC_TEST_VECTORS [I].Key),
                                      UpperCase (SERPENT_CBC_TEST_VECTORS [I].IV),
                                      UpperCase (SERPENT_CBC_TEST_VECTORS [I].Cipher) ) <> UpperCase (SERPENT_CBC_TEST_VECTORS [I].Plain)  ) then
    {$ELSE}
        if (  TestEncryptSerpentCBC ( LowerCase (SERPENT_CBC_TEST_VECTORS [I].Key),
                                      LowerCase (SERPENT_CBC_TEST_VECTORS [I].IV),
                                      LowerCase (SERPENT_CBC_TEST_VECTORS [I].Plain) ) <> LowerCase (SERPENT_CBC_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptSerpentCBC ( LowerCase (SERPENT_CBC_TEST_VECTORS [I].Key),
                                      LowerCase (SERPENT_CBC_TEST_VECTORS [I].IV),
                                      LowerCase (SERPENT_CBC_TEST_VECTORS [I].Cipher) ) <> LowerCase (SERPENT_CBC_TEST_VECTORS [I].Plain)  ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;
{$ENDIF CBC}

{$IFDEF CFB}
function TestEncryptSerpentCFB (const aKey: String;
                                const anIV: String;
                                const aValue: String) : String;
var
    ctx       : TSerpentCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    IV        : TSerpent128Block;
    Value     : TSerpent128Block;
    Cipher    : TSerpent128Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 16*2 ) then
            begin
                IV := HexToSerpentIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_SERPENT_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 32*2 ) then
            begin
                Key256 := HexToKey256 (aKey);
                KeySize := 256;
                ErrorCode := SP_CFB_Init (Key256,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := SP_CFB_Init (Key192,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := SP_CFB_Init (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_SERPENT_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToSerpentIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_SERPENT_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_SERPENT_128_BLOCK;
            ErrorCode := SP_CFB_Encrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := SerpentIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            IV := NULL_SERPENT_128_BLOCK;
            Value := NULL_SERPENT_128_BLOCK;
            Cipher := NULL_SERPENT_128_BLOCK;

            SP_CFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SERPENT_CFB_ENCRYPT,[KeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

function TestDecryptSerpentCFB (const aKey: String;
                                const anIV: String;
                                const aValue: String) : String;
var
    ctx       : TSerpentCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    IV        : TSerpent128Block;
    Value     : TSerpent128Block;
    Cipher    : TSerpent128Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 16*2 ) then
            begin
                IV := HexToSerpentIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_SERPENT_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 32*2 ) then
            begin
                Key256 := HexToKey256 (aKey);
                KeySize := 256;
                ErrorCode := SP_CFB_Init (Key256,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := SP_CFB_Init (Key192,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := SP_CFB_Init (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_SERPENT_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToSerpentIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_SERPENT_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_SERPENT_128_BLOCK;
            ErrorCode := SP_CFB_Decrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := SerpentIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            IV := NULL_SERPENT_128_BLOCK;
            Value := NULL_SERPENT_128_BLOCK;
            Cipher := NULL_SERPENT_128_BLOCK;

            SP_CFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SERPENT_CFB_DECRYPT,[KeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

function TestSerpentCFB : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (SERPENT_CFB_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if (  TestEncryptSerpentCFB ( UpperCase (SERPENT_CFB_TEST_VECTORS [I].Key),
                                      UpperCase (SERPENT_CFB_TEST_VECTORS [I].IV),
                                      UpperCase (SERPENT_CFB_TEST_VECTORS [I].Plain) ) <> UpperCase (SERPENT_CFB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptSerpentCFB ( UpperCase (SERPENT_CFB_TEST_VECTORS [I].Key),
                                      UpperCase (SERPENT_CFB_TEST_VECTORS [I].IV),
                                      UpperCase (SERPENT_CFB_TEST_VECTORS [I].Cipher) ) <> UpperCase (SERPENT_CFB_TEST_VECTORS [I].Plain)  ) then
    {$ELSE}
        if (  TestEncryptSerpentCFB ( LowerCase (SERPENT_CFB_TEST_VECTORS [I].Key),
                                      LowerCase (SERPENT_CFB_TEST_VECTORS [I].IV),
                                      LowerCase (SERPENT_CFB_TEST_VECTORS [I].Plain) ) <> LowerCase (SERPENT_CFB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptSerpentCFB ( LowerCase (SERPENT_CFB_TEST_VECTORS [I].Key),
                                      LowerCase (SERPENT_CFB_TEST_VECTORS [I].IV),
                                      LowerCase (SERPENT_CFB_TEST_VECTORS [I].Cipher) ) <> LowerCase (SERPENT_CFB_TEST_VECTORS [I].Plain)  ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;
{$ENDIF CFB}

{$IFDEF OFB}
function TestEncryptSerpentOFB (const aKey: String;
                                const anIV: String;
                                const aValue: String) : String;
var
    ctx       : TSerpentCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    IV        : TSerpent128Block;
    Value     : TSerpent128Block;
    Cipher    : TSerpent128Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 16*2 ) then
            begin
                IV := HexToSerpentIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_SERPENT_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 32*2 ) then
            begin
                Key256 := HexToKey256 (aKey);
                KeySize := 256;
                ErrorCode := SP_OFB_Init (Key256,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := SP_OFB_Init (Key192,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := SP_OFB_Init (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_SERPENT_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToSerpentIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_SERPENT_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_SERPENT_128_BLOCK;
            ErrorCode := SP_OFB_Encrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := SerpentIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            IV := NULL_SERPENT_128_BLOCK;
            Value := NULL_SERPENT_128_BLOCK;
            Cipher := NULL_SERPENT_128_BLOCK;

            SP_OFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SERPENT_OFB_ENCRYPT,[KeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

function TestDecryptSerpentOFB (const aKey: String;
                                const anIV: String;
                                const aValue: String) : String;
var
    ctx       : TSerpentCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    IV        : TSerpent128Block;
    Value     : TSerpent128Block;
    Cipher    : TSerpent128Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 16*2 ) then
            begin
                IV := HexToSerpentIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_SERPENT_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 32*2 ) then
            begin
                Key256 := HexToKey256 (aKey);
                KeySize := 256;
                ErrorCode := SP_OFB_Init (Key256,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := SP_OFB_Init (Key192,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := SP_OFB_Init (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_SERPENT_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToSerpentIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_SERPENT_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_SERPENT_128_BLOCK;
            ErrorCode := SP_OFB_Decrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := SerpentIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            IV := NULL_SERPENT_128_BLOCK;
            Value := NULL_SERPENT_128_BLOCK;
            Cipher := NULL_SERPENT_128_BLOCK;

            SP_OFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SERPENT_OFB_DECRYPT,[KeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

function TestSerpentOFB : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (SERPENT_OFB_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if (  TestEncryptSerpentOFB ( UpperCase (SERPENT_OFB_TEST_VECTORS [I].Key),
                                      UpperCase (SERPENT_OFB_TEST_VECTORS [I].IV),
                                      UpperCase (SERPENT_OFB_TEST_VECTORS [I].Plain) ) <> UpperCase (SERPENT_OFB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptSerpentOFB ( UpperCase (SERPENT_OFB_TEST_VECTORS [I].Key),
                                      UpperCase (SERPENT_OFB_TEST_VECTORS [I].IV),
                                      UpperCase (SERPENT_OFB_TEST_VECTORS [I].Cipher) ) <> UpperCase (SERPENT_OFB_TEST_VECTORS [I].Plain)  ) then
    {$ELSE}
        if (  TestEncryptSerpentOFB ( LowerCase (SERPENT_OFB_TEST_VECTORS [I].Key),
                                      LowerCase (SERPENT_OFB_TEST_VECTORS [I].IV),
                                      LowerCase (SERPENT_OFB_TEST_VECTORS [I].Plain) ) <> LowerCase (SERPENT_OFB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptSerpentOFB ( LowerCase (SERPENT_OFB_TEST_VECTORS [I].Key),
                                      LowerCase (SERPENT_OFB_TEST_VECTORS [I].IV),
                                      LowerCase (SERPENT_OFB_TEST_VECTORS [I].Cipher) ) <> LowerCase (SERPENT_OFB_TEST_VECTORS [I].Plain)  ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;
{$ENDIF OFB}
