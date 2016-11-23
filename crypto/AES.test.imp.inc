{******************************************************************************}
{*  AES.test.imp.inc                                                          *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{$IFDEF ECB}
function TestEncryptAESECB (const aKey: String;
                            const aValue: String) : String;
var
    ctx       : TAESCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    Value     : TAES128Block;
    Cipher    : TAES128Block;
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
                ErrorCode := AES_ECB_Init_Encr (Key256,KeySize,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := AES_ECB_Init_Encr (Key192,KeySize,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := AES_ECB_Init_Encr (Key128,KeySize,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_AES_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToAESIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_AES_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_AES_128_BLOCK;
            ErrorCode := AES_ECB_Encrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := AESIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            Value := NULL_AES_128_BLOCK;
            Cipher := NULL_AES_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_AES_ECB_ENCRYPT,[KeySize,AES_VERSION]), E.Message ]);
    end;
end;

function TestDecryptAESECB (const aKey: String;
                            const aValue: String) : String;
var
    ctx       : TAESCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    Value     : TAES128Block;
    Cipher    : TAES128Block;
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
                ErrorCode := AES_ECB_Init_Decr (Key256,KeySize,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := AES_ECB_Init_Decr (Key192,KeySize,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := AES_ECB_Init_Decr (Key128,KeySize,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_AES_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToAESIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_AES_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_AES_128_BLOCK;
            ErrorCode := AES_ECB_Decrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := AESIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            Value := NULL_AES_128_BLOCK;
            Cipher := NULL_AES_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_AES_ECB_DECRYPT,[KeySize,AES_VERSION]), E.Message ]);
    end;
end;

function TestAESECB : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (AES_ECB_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if (  TestEncryptAESECB ( UpperCase (AES_ECB_TEST_VECTORS [I].Key),
                                  UpperCase (AES_ECB_TEST_VECTORS [I].Plain) ) <> UpperCase (AES_ECB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptAESECB ( UpperCase (AES_ECB_TEST_VECTORS [I].Key),
                                  UpperCase (AES_ECB_TEST_VECTORS [I].Cipher) ) <> UpperCase (AES_ECB_TEST_VECTORS [I].Plain)  ) then
    {$ELSE}
        if (  TestEncryptAESECB ( LowerCase (AES_ECB_TEST_VECTORS [I].Key),
                                  LowerCase (AES_ECB_TEST_VECTORS [I].Plain) ) <> LowerCase (AES_ECB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptAESECB ( LowerCase (AES_ECB_TEST_VECTORS [I].Key),
                                  LowerCase (AES_ECB_TEST_VECTORS [I].Cipher) ) <> LowerCase (AES_ECB_TEST_VECTORS [I].Plain)  ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;
{$ENDIF ECB}

{$IFDEF CBC}
function TestEncryptAESCBC (const aKey: String;
                            const anIV: String;
                            const aValue: String) : String;
var
    ctx       : TAESCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    IV        : TAES128Block;
    Value     : TAES128Block;
    Cipher    : TAES128Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 16*2 ) then
            begin
                IV := HexToAESIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_AES_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 32*2 ) then
            begin
                Key256 := HexToKey256 (aKey);
                KeySize := 256;
                ErrorCode := AES_CBC_Init_Encr (Key256,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := AES_CBC_Init_Encr (Key192,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := AES_CBC_Init_Encr (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_AES_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToAESIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_AES_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_AES_128_BLOCK;
            ErrorCode := AES_CBC_Encrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := AESIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            IV := NULL_AES_128_BLOCK;
            Value := NULL_AES_128_BLOCK;
            Cipher := NULL_AES_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_AES_CBC_ENCRYPT,[KeySize,AES_VERSION]), E.Message ]);
    end;
end;

function TestDecryptAESCBC (const aKey: String;
                            const anIV: String;
                            const aValue: String) : String;
var
    ctx       : TAESCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    IV        : TAES128Block;
    Value     : TAES128Block;
    Cipher    : TAES128Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 16*2 ) then
            begin
                IV := HexToAESIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_AES_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 32*2 ) then
            begin
                Key256 := HexToKey256 (aKey);
                KeySize := 256;
                ErrorCode := AES_CBC_Init_Decr (Key256,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := AES_CBC_Init_Decr (Key192,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := AES_CBC_Init_Decr (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_AES_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToAESIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_AES_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_AES_128_BLOCK;
            ErrorCode := AES_CBC_Decrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := AESIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            IV := NULL_AES_128_BLOCK;
            Value := NULL_AES_128_BLOCK;
            Cipher := NULL_AES_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_AES_CBC_DECRYPT,[KeySize,AES_VERSION]), E.Message ]);
    end;
end;

function TestAESCBC : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (AES_CBC_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if (  TestEncryptAESCBC ( UpperCase (AES_CBC_TEST_VECTORS [I].Key),
                                  UpperCase (AES_CBC_TEST_VECTORS [I].IV),
                                  UpperCase (AES_CBC_TEST_VECTORS [I].Plain) ) <> UpperCase (AES_CBC_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptAESCBC ( UpperCase (AES_CBC_TEST_VECTORS [I].Key),
                                  UpperCase (AES_CBC_TEST_VECTORS [I].IV),
                                  UpperCase (AES_CBC_TEST_VECTORS [I].Cipher) ) <> UpperCase (AES_CBC_TEST_VECTORS [I].Plain)  ) then
    {$ELSE}
        if (  TestEncryptAESCBC ( LowerCase (AES_CBC_TEST_VECTORS [I].Key),
                                  LowerCase (AES_CBC_TEST_VECTORS [I].IV),
                                  LowerCase (AES_CBC_TEST_VECTORS [I].Plain) ) <> LowerCase (AES_CBC_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptAESCBC ( LowerCase (AES_CBC_TEST_VECTORS [I].Key),
                                  LowerCase (AES_CBC_TEST_VECTORS [I].IV),
                                  LowerCase (AES_CBC_TEST_VECTORS [I].Cipher) ) <> LowerCase (AES_CBC_TEST_VECTORS [I].Plain)  ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;
{$ENDIF CBC}

{$IFDEF CFB}
function TestEncryptAESCFB (const aKey: String;
                            const anIV: String;
                            const aValue: String) : String;
var
    ctx       : TAESCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    IV        : TAES128Block;
    Value     : TAES128Block;
    Cipher    : TAES128Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 16*2 ) then
            begin
                IV := HexToAESIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_AES_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 32*2 ) then
            begin
                Key256 := HexToKey256 (aKey);
                KeySize := 256;
                ErrorCode := AES_CFB_Init (Key256,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := AES_CFB_Init (Key192,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := AES_CFB_Init (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_AES_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToAESIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_AES_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_AES_128_BLOCK;
            ErrorCode := AES_CFB_Encrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := AESIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            IV := NULL_AES_128_BLOCK;
            Value := NULL_AES_128_BLOCK;
            Cipher := NULL_AES_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_AES_CFB_ENCRYPT,[KeySize,AES_VERSION]), E.Message ]);
    end;
end;

function TestDecryptAESCFB (const aKey: String;
                            const anIV: String;
                            const aValue: String) : String;
var
    ctx       : TAESCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    IV        : TAES128Block;
    Value     : TAES128Block;
    Cipher    : TAES128Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 16*2 ) then
            begin
                IV := HexToAESIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_AES_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 32*2 ) then
            begin
                Key256 := HexToKey256 (aKey);
                KeySize := 256;
                ErrorCode := AES_CFB_Init (Key256,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := AES_CFB_Init (Key192,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := AES_CFB_Init (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_AES_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToAESIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_AES_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_AES_128_BLOCK;
            ErrorCode := AES_CFB_Decrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := AESIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            IV := NULL_AES_128_BLOCK;
            Value := NULL_AES_128_BLOCK;
            Cipher := NULL_AES_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_AES_CFB_DECRYPT,[KeySize,AES_VERSION]), E.Message ]);
    end;
end;

function TestAESCFB : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (AES_CFB_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if (  TestEncryptAESCFB ( UpperCase (AES_CFB_TEST_VECTORS [I].Key),
                                  UpperCase (AES_CFB_TEST_VECTORS [I].IV),
                                  UpperCase (AES_CFB_TEST_VECTORS [I].Plain) ) <> UpperCase (AES_CFB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptAESCFB ( UpperCase (AES_CFB_TEST_VECTORS [I].Key),
                                  UpperCase (AES_CFB_TEST_VECTORS [I].IV),
                                  UpperCase (AES_CFB_TEST_VECTORS [I].Cipher) ) <> UpperCase (AES_CFB_TEST_VECTORS [I].Plain)  ) then
    {$ELSE}
        if (  TestEncryptAESCFB ( LowerCase (AES_CFB_TEST_VECTORS [I].Key),
                                  LowerCase (AES_CFB_TEST_VECTORS [I].IV),
                                  LowerCase (AES_CFB_TEST_VECTORS [I].Plain) ) <> LowerCase (AES_CFB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptAESCFB ( LowerCase (AES_CFB_TEST_VECTORS [I].Key),
                                  LowerCase (AES_CFB_TEST_VECTORS [I].IV),
                                  LowerCase (AES_CFB_TEST_VECTORS [I].Cipher) ) <> LowerCase (AES_CFB_TEST_VECTORS [I].Plain)  ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;
{$ENDIF CFB}

{$IFDEF OFB}
function TestEncryptAESOFB (const aKey: String;
                            const anIV: String;
                            const aValue: String) : String;
var
    ctx       : TAESCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    IV        : TAES128Block;
    Value     : TAES128Block;
    Cipher    : TAES128Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 16*2 ) then
            begin
                IV := HexToAESIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_AES_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 32*2 ) then
            begin
                Key256 := HexToKey256 (aKey);
                KeySize := 256;
                ErrorCode := AES_OFB_Init (Key256,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := AES_OFB_Init (Key192,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := AES_OFB_Init (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_AES_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToAESIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_AES_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_AES_128_BLOCK;
            ErrorCode := AES_OFB_Encrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := AESIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            IV := NULL_AES_128_BLOCK;
            Value := NULL_AES_128_BLOCK;
            Cipher := NULL_AES_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_AES_OFB_ENCRYPT,[KeySize,AES_VERSION]), E.Message ]);
    end;
end;

function TestDecryptAESOFB (const aKey: String;
                            const anIV: String;
                            const aValue: String) : String;
var
    ctx       : TAESCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    IV        : TAES128Block;
    Value     : TAES128Block;
    Cipher    : TAES128Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 16*2 ) then
            begin
                IV := HexToAESIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_AES_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 32*2 ) then
            begin
                Key256 := HexToKey256 (aKey);
                KeySize := 256;
                ErrorCode := AES_OFB_Init (Key256,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := AES_OFB_Init (Key192,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := AES_OFB_Init (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_AES_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToAESIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_AES_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_AES_128_BLOCK;
            ErrorCode := AES_OFB_Decrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := AESIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            IV := NULL_AES_128_BLOCK;
            Value := NULL_AES_128_BLOCK;
            Cipher := NULL_AES_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_AES_OFB_DECRYPT,[KeySize,AES_VERSION]), E.Message ]);
    end;
end;

function TestAESOFB : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (AES_OFB_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if (  TestEncryptAESOFB ( UpperCase (AES_OFB_TEST_VECTORS [I].Key),
                                  UpperCase (AES_OFB_TEST_VECTORS [I].IV),
                                  UpperCase (AES_OFB_TEST_VECTORS [I].Plain) ) <> UpperCase (AES_OFB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptAESOFB ( UpperCase (AES_OFB_TEST_VECTORS [I].Key),
                                  UpperCase (AES_OFB_TEST_VECTORS [I].IV),
                                  UpperCase (AES_OFB_TEST_VECTORS [I].Cipher) ) <> UpperCase (AES_OFB_TEST_VECTORS [I].Plain)  ) then
    {$ELSE}
        if (  TestEncryptAESOFB ( LowerCase (AES_OFB_TEST_VECTORS [I].Key),
                                  LowerCase (AES_OFB_TEST_VECTORS [I].IV),
                                  LowerCase (AES_OFB_TEST_VECTORS [I].Plain) ) <> LowerCase (AES_OFB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptAESOFB ( LowerCase (AES_OFB_TEST_VECTORS [I].Key),
                                  LowerCase (AES_OFB_TEST_VECTORS [I].IV),
                                  LowerCase (AES_OFB_TEST_VECTORS [I].Cipher) ) <> LowerCase (AES_OFB_TEST_VECTORS [I].Plain)  ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;
{$ENDIF OFB}
