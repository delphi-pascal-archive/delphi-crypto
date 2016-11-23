{******************************************************************************}
{*  TwoFish.test.imp.inc                                                       *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{$IFDEF ECB}
function TestEncryptTwoFishECB (const aKey: String;
                                const aValue: String) : String;
var
    ctx       : TTwoFishCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    Value     : TTwoFish128Block;
    Cipher    : TTwoFish128Block;
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
                ErrorCode := TF_ECB_Init (Key256,KeySize,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := TF_ECB_Init (Key192,KeySize,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := TF_ECB_Init (Key128,KeySize,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_TWOFISH_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToTwoFishIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_TWOFISH_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_TWOFISH_128_BLOCK;
            ErrorCode := TF_ECB_Encrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := TwoFishIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            Value := NULL_TWOFISH_128_BLOCK;
            Cipher := NULL_TWOFISH_128_BLOCK;

            TF_ECB_Reset (ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_TWOFISH_ECB_ENCRYPT,[KeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

function TestDecryptTwoFishECB (const aKey: String;
                                const aValue: String) : String;
var
    ctx       : TTwoFishCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    Value     : TTwoFish128Block;
    Cipher    : TTwoFish128Block;
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
                ErrorCode := TF_ECB_Init (Key256,KeySize,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := TF_ECB_Init (Key192,KeySize,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := TF_ECB_Init (Key128,KeySize,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_TWOFISH_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToTwoFishIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_TWOFISH_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_TWOFISH_128_BLOCK;
            ErrorCode := TF_ECB_Decrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := TwoFishIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            Value := NULL_TWOFISH_128_BLOCK;
            Cipher := NULL_TWOFISH_128_BLOCK;

            TF_ECB_Reset (ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_TWOFISH_ECB_DECRYPT,[KeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

function TestTwoFishECB : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (TWOFISH_ECB_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if (  TestEncryptTwoFishECB ( UpperCase (TWOFISH_ECB_TEST_VECTORS [I].Key),
                                      UpperCase (TWOFISH_ECB_TEST_VECTORS [I].Plain) ) <> UpperCase (TWOFISH_ECB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptTwoFishECB ( UpperCase (TWOFISH_ECB_TEST_VECTORS [I].Key),
                                      UpperCase (TWOFISH_ECB_TEST_VECTORS [I].Cipher) ) <> UpperCase (TWOFISH_ECB_TEST_VECTORS [I].Plain)  ) then
    {$ELSE}
        if (  TestEncryptTwoFishECB ( LowerCase (TWOFISH_ECB_TEST_VECTORS [I].Key),
                                      LowerCase (TWOFISH_ECB_TEST_VECTORS [I].Plain) ) <> LowerCase (TWOFISH_ECB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptTwoFishECB ( LowerCase (TWOFISH_ECB_TEST_VECTORS [I].Key),
                                      LowerCase (TWOFISH_ECB_TEST_VECTORS [I].Cipher) ) <> LowerCase (TWOFISH_ECB_TEST_VECTORS [I].Plain)  ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;
{$ENDIF ECB}

{$IFDEF CBC}
function TestEncryptTwoFishCBC (const aKey: String;
                                const anIV: String;
                                const aValue: String) : String;
var
    ctx       : TTwoFishCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    IV        : TTwoFish128Block;
    Value     : TTwoFish128Block;
    Cipher    : TTwoFish128Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 16*2 ) then
            begin
                IV := HexToTwoFishIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_TWOFISH_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 32*2 ) then
            begin
                Key256 := HexToKey256 (aKey);
                KeySize := 256;
                ErrorCode := TF_CBC_Init (Key256,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := TF_CBC_Init (Key192,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := TF_CBC_Init (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_TWOFISH_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToTwoFishIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_TWOFISH_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_TWOFISH_128_BLOCK;
            ErrorCode := TF_CBC_Encrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := TwoFishIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            IV := NULL_TWOFISH_128_BLOCK;
            Value := NULL_TWOFISH_128_BLOCK;
            Cipher := NULL_TWOFISH_128_BLOCK;

            TF_CBC_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_TWOFISH_CBC_ENCRYPT,[KeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

function TestDecryptTwoFishCBC (const aKey: String;
                                const anIV: String;
                                const aValue: String) : String;
var
    ctx       : TTwoFishCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    IV        : TTwoFish128Block;
    Value     : TTwoFish128Block;
    Cipher    : TTwoFish128Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 16*2 ) then
            begin
                IV := HexToTwoFishIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_TWOFISH_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 32*2 ) then
            begin
                Key256 := HexToKey256 (aKey);
                KeySize := 256;
                ErrorCode := TF_CBC_Init (Key256,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := TF_CBC_Init (Key192,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := TF_CBC_Init (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_TWOFISH_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToTwoFishIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_TWOFISH_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_TWOFISH_128_BLOCK;
            ErrorCode := TF_CBC_Decrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := TwoFishIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            IV := NULL_TWOFISH_128_BLOCK;
            Value := NULL_TWOFISH_128_BLOCK;
            Cipher := NULL_TWOFISH_128_BLOCK;

            TF_CBC_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_TWOFISH_CBC_DECRYPT,[KeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

function TestTwoFishCBC : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (TWOFISH_CBC_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if (  TestEncryptTwoFishCBC ( UpperCase (TWOFISH_CBC_TEST_VECTORS [I].Key),
                                      UpperCase (TWOFISH_CBC_TEST_VECTORS [I].IV),
                                      UpperCase (TWOFISH_CBC_TEST_VECTORS [I].Plain) ) <> UpperCase (TWOFISH_CBC_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptTwoFishCBC ( UpperCase (TWOFISH_CBC_TEST_VECTORS [I].Key),
                                      UpperCase (TWOFISH_CBC_TEST_VECTORS [I].IV),
                                      UpperCase (TWOFISH_CBC_TEST_VECTORS [I].Cipher) ) <> UpperCase (TWOFISH_CBC_TEST_VECTORS [I].Plain)  ) then
    {$ELSE}
        if (  TestEncryptTwoFishCBC ( LowerCase (TWOFISH_CBC_TEST_VECTORS [I].Key),
                                      LowerCase (TWOFISH_CBC_TEST_VECTORS [I].IV),
                                      LowerCase (TWOFISH_CBC_TEST_VECTORS [I].Plain) ) <> LowerCase (TWOFISH_CBC_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptTwoFishCBC ( LowerCase (TWOFISH_CBC_TEST_VECTORS [I].Key),
                                      LowerCase (TWOFISH_CBC_TEST_VECTORS [I].IV),
                                      LowerCase (TWOFISH_CBC_TEST_VECTORS [I].Cipher) ) <> LowerCase (TWOFISH_CBC_TEST_VECTORS [I].Plain)  ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;
{$ENDIF CBC}

{$IFDEF CFB}
function TestEncryptTwoFishCFB (const aKey: String;
                                const anIV: String;
                                const aValue: String) : String;
var
    ctx       : TTwoFishCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    IV        : TTwoFish128Block;
    Value     : TTwoFish128Block;
    Cipher    : TTwoFish128Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 16*2 ) then
            begin
                IV := HexToTwoFishIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_TWOFISH_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 32*2 ) then
            begin
                Key256 := HexToKey256 (aKey);
                KeySize := 256;
                ErrorCode := TF_CFB_Init (Key256,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := TF_CFB_Init (Key192,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := TF_CFB_Init (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_TWOFISH_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToTwoFishIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_TWOFISH_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_TWOFISH_128_BLOCK;
            ErrorCode := TF_CFB_Encrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := TwoFishIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            IV := NULL_TWOFISH_128_BLOCK;
            Value := NULL_TWOFISH_128_BLOCK;
            Cipher := NULL_TWOFISH_128_BLOCK;

            TF_CFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_TWOFISH_CFB_ENCRYPT,[KeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

function TestDecryptTwoFishCFB (const aKey: String;
                                const anIV: String;
                                const aValue: String) : String;
var
    ctx       : TTwoFishCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    IV        : TTwoFish128Block;
    Value     : TTwoFish128Block;
    Cipher    : TTwoFish128Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 16*2 ) then
            begin
                IV := HexToTwoFishIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_TWOFISH_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 32*2 ) then
            begin
                Key256 := HexToKey256 (aKey);
                KeySize := 256;
                ErrorCode := TF_CFB_Init (Key256,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := TF_CFB_Init (Key192,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := TF_CFB_Init (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_TWOFISH_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToTwoFishIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_TWOFISH_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_TWOFISH_128_BLOCK;
            ErrorCode := TF_CFB_Decrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := TwoFishIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            IV := NULL_TWOFISH_128_BLOCK;
            Value := NULL_TWOFISH_128_BLOCK;
            Cipher := NULL_TWOFISH_128_BLOCK;

            TF_CFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_TWOFISH_CFB_DECRYPT,[KeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

function TestTwoFishCFB : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (TWOFISH_CFB_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if (  TestEncryptTwoFishCFB ( UpperCase (TWOFISH_CFB_TEST_VECTORS [I].Key),
                                      UpperCase (TWOFISH_CFB_TEST_VECTORS [I].IV),
                                      UpperCase (TWOFISH_CFB_TEST_VECTORS [I].Plain) ) <> UpperCase (TWOFISH_CFB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptTwoFishCFB ( UpperCase (TWOFISH_CFB_TEST_VECTORS [I].Key),
                                      UpperCase (TWOFISH_CFB_TEST_VECTORS [I].IV),
                                      UpperCase (TWOFISH_CFB_TEST_VECTORS [I].Cipher) ) <> UpperCase (TWOFISH_CFB_TEST_VECTORS [I].Plain)  ) then
    {$ELSE}
        if (  TestEncryptTwoFishCFB ( LowerCase (TWOFISH_CFB_TEST_VECTORS [I].Key),
                                      LowerCase (TWOFISH_CFB_TEST_VECTORS [I].IV),
                                      LowerCase (TWOFISH_CFB_TEST_VECTORS [I].Plain) ) <> LowerCase (TWOFISH_CFB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptTwoFishCFB ( LowerCase (TWOFISH_CFB_TEST_VECTORS [I].Key),
                                      LowerCase (TWOFISH_CFB_TEST_VECTORS [I].IV),
                                      LowerCase (TWOFISH_CFB_TEST_VECTORS [I].Cipher) ) <> LowerCase (TWOFISH_CFB_TEST_VECTORS [I].Plain)  ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;
{$ENDIF CFB}

{$IFDEF OFB}
function TestEncryptTwoFishOFB (const aKey: String;
                                const anIV: String;
                                const aValue: String) : String;
var
    ctx       : TTwoFishCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    IV        : TTwoFish128Block;
    Value     : TTwoFish128Block;
    Cipher    : TTwoFish128Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 16*2 ) then
            begin
                IV := HexToTwoFishIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_TWOFISH_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 32*2 ) then
            begin
                Key256 := HexToKey256 (aKey);
                KeySize := 256;
                ErrorCode := TF_OFB_Init (Key256,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := TF_OFB_Init (Key192,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := TF_OFB_Init (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_TWOFISH_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToTwoFishIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_TWOFISH_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_TWOFISH_128_BLOCK;
            ErrorCode := TF_OFB_Encrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := TwoFishIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            IV := NULL_TWOFISH_128_BLOCK;
            Value := NULL_TWOFISH_128_BLOCK;
            Cipher := NULL_TWOFISH_128_BLOCK;

            TF_OFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_TWOFISH_OFB_ENCRYPT,[KeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

function TestDecryptTwoFishOFB (const aKey: String;
                                const anIV: String;
                                const aValue: String) : String;
var
    ctx       : TTwoFishCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    IV        : TTwoFish128Block;
    Value     : TTwoFish128Block;
    Cipher    : TTwoFish128Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 16*2 ) then
            begin
                IV := HexToTwoFishIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_TWOFISH_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 32*2 ) then
            begin
                Key256 := HexToKey256 (aKey);
                KeySize := 256;
                ErrorCode := TF_OFB_Init (Key256,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := TF_OFB_Init (Key192,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := TF_OFB_Init (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_TWOFISH_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToTwoFishIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_TWOFISH_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_TWOFISH_128_BLOCK;
            ErrorCode := TF_OFB_Decrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := TwoFishIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            IV := NULL_TWOFISH_128_BLOCK;
            Value := NULL_TWOFISH_128_BLOCK;
            Cipher := NULL_TWOFISH_128_BLOCK;

            TF_OFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_TWOFISH_OFB_DECRYPT,[KeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

function TestTwoFishOFB : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (TWOFISH_OFB_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if (  TestEncryptTwoFishOFB ( UpperCase (TWOFISH_OFB_TEST_VECTORS [I].Key),
                                      UpperCase (TWOFISH_OFB_TEST_VECTORS [I].IV),
                                      UpperCase (TWOFISH_OFB_TEST_VECTORS [I].Plain) ) <> UpperCase (TWOFISH_OFB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptTwoFishOFB ( UpperCase (TWOFISH_OFB_TEST_VECTORS [I].Key),
                                      UpperCase (TWOFISH_OFB_TEST_VECTORS [I].IV),
                                      UpperCase (TWOFISH_OFB_TEST_VECTORS [I].Cipher) ) <> UpperCase (TWOFISH_OFB_TEST_VECTORS [I].Plain)  ) then
    {$ELSE}
        if (  TestEncryptTwoFishOFB ( LowerCase (TWOFISH_OFB_TEST_VECTORS [I].Key),
                                      LowerCase (TWOFISH_OFB_TEST_VECTORS [I].IV),
                                      LowerCase (TWOFISH_OFB_TEST_VECTORS [I].Plain) ) <> LowerCase (TWOFISH_OFB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptTwoFishOFB ( LowerCase (TWOFISH_OFB_TEST_VECTORS [I].Key),
                                      LowerCase (TWOFISH_OFB_TEST_VECTORS [I].IV),
                                      LowerCase (TWOFISH_OFB_TEST_VECTORS [I].Cipher) ) <> LowerCase (TWOFISH_OFB_TEST_VECTORS [I].Plain)  ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;
{$ENDIF OFB}
