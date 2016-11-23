{******************************************************************************}
{*  BlowFish.test.imp.inc                                                     *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{$IFDEF ECB}
function TestEncryptBlowFishECB (const aKey: String;
                                 const aValue: String) : String;
var
    ctx       : TBlowFishCtx;
    Key128    : TKey128;
    KeySize   : WORD;
    Value     : TBlowFish64Block;
    Cipher    : TBlowFish64Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := SizeOf (Key128);
                ErrorCode := BF_ECB_Init (Key128,KeySize,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_BLOWFISH_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 8*2 ) then
            begin
                Value := HexToBlowFishIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_BLOWFISH_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_BLOWFISH_64_BLOCK;
            ErrorCode := BF_ECB_Encrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := BlowFishIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Value := NULL_BLOWFISH_64_BLOCK;
            Cipher := NULL_BLOWFISH_64_BLOCK;

            BF_ECB_Reset (ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_BLOWFISH_ECB_ENCRYPT,[KeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

function TestDecryptBlowFishECB (const aKey: String;
                                 const aValue: String) : String;
var
    ctx       : TBlowFishCtx;
    Key128    : TKey128;
    KeySize   : WORD;
    Value     : TBlowFish64Block;
    Cipher    : TBlowFish64Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := SizeOf (Key128);;
                ErrorCode := BF_ECB_Init (Key128,KeySize,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_BLOWFISH_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 8*2 ) then
            begin
                Value := HexToBlowFishIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_BLOWFISH_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_BLOWFISH_64_BLOCK;
            ErrorCode := BF_ECB_Decrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := BlowFishIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Value := NULL_BLOWFISH_64_BLOCK;
            Cipher := NULL_BLOWFISH_64_BLOCK;

            BF_ECB_Reset (ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_BLOWFISH_ECB_DECRYPT,[KeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

function TestBlowFishECB : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (BLOWFISH_ECB_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if (  TestEncryptBlowFishECB ( UpperCase (BLOWFISH_ECB_TEST_VECTORS [I].Key),
                                       UpperCase (BLOWFISH_ECB_TEST_VECTORS [I].Plain) ) <> UpperCase (BLOWFISH_ECB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptBlowFishECB ( UpperCase (BLOWFISH_ECB_TEST_VECTORS [I].Key),
                                       UpperCase (BLOWFISH_ECB_TEST_VECTORS [I].Cipher) ) <> UpperCase (BLOWFISH_ECB_TEST_VECTORS [I].Plain)  ) then
    {$ELSE}
        if (  TestEncryptBlowFishECB ( LowerCase (BLOWFISH_ECB_TEST_VECTORS [I].Key),
                                       LowerCase (BLOWFISH_ECB_TEST_VECTORS [I].Plain) ) <> LowerCase (BLOWFISH_ECB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptBlowFishECB ( LowerCase (BLOWFISH_ECB_TEST_VECTORS [I].Key),
                                       LowerCase (BLOWFISH_ECB_TEST_VECTORS [I].Cipher) ) <> LowerCase (BLOWFISH_ECB_TEST_VECTORS [I].Plain)  ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;
{$ENDIF ECB}

{$IFDEF CBC}
function TestEncryptBlowFishCBC (const aKey: String;
                                 const anIV: String;
                                 const aValue: String) : String;
var
    ctx       : TBlowFishCtx;
    Key128    : TKey128;
    KeySize   : WORD;
    IV        : TBlowFish64Block;
    Value     : TBlowFish64Block;
    Cipher    : TBlowFish64Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 8*2 ) then
            begin
                IV := HexToBlowFishIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_BLOWFISH_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := SizeOf (Key128);
                ErrorCode := BF_CBC_Init (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_BLOWFISH_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 8*2 ) then
            begin
                Value := HexToBlowFishIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_BLOWFISH_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_BLOWFISH_64_BLOCK;
            ErrorCode := BF_CBC_Encrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := BlowFishIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            IV := NULL_BLOWFISH_64_BLOCK;
            Value := NULL_BLOWFISH_64_BLOCK;
            Cipher := NULL_BLOWFISH_64_BLOCK;

            BF_CBC_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_BLOWFISH_CBC_ENCRYPT,[KeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

function TestDecryptBlowFishCBC (const aKey: String;
                               const anIV: String;
                               const aValue: String) : String;
var
    ctx       : TBlowFishCtx;
    Key128    : TKey128;
    KeySize   : WORD;
    IV        : TBlowFish64Block;
    Value     : TBlowFish64Block;
    Cipher    : TBlowFish64Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 8*2 ) then
            begin
                IV := HexToBlowFishIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_BLOWFISH_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := SizeOf (Key128);
                ErrorCode := BF_CBC_Init (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_BLOWFISH_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 8*2 ) then
            begin
                Value := HexToBlowFishIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_BLOWFISH_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_BLOWFISH_64_BLOCK;
            ErrorCode := BF_CBC_Decrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := BlowFishIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            IV := NULL_BLOWFISH_64_BLOCK;
            Value := NULL_BLOWFISH_64_BLOCK;
            Cipher := NULL_BLOWFISH_64_BLOCK;

            BF_CBC_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_BLOWFISH_CBC_DECRYPT,[KeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

function TestBlowFishCBC : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (BLOWFISH_CBC_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if (  TestEncryptBlowFishCBC ( UpperCase (BLOWFISH_CBC_TEST_VECTORS [I].Key),
                                       UpperCase (BLOWFISH_CBC_TEST_VECTORS [I].IV),
                                       UpperCase (BLOWFISH_CBC_TEST_VECTORS [I].Plain) ) <> UpperCase (BLOWFISH_CBC_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptBlowFishCBC ( UpperCase (BLOWFISH_CBC_TEST_VECTORS [I].Key),
                                       UpperCase (BLOWFISH_CBC_TEST_VECTORS [I].IV),
                                       UpperCase (BLOWFISH_CBC_TEST_VECTORS [I].Cipher) ) <> UpperCase (BLOWFISH_CBC_TEST_VECTORS [I].Plain)  ) then
    {$ELSE}
        if (  TestEncryptBlowFishCBC ( LowerCase (BLOWFISH_CBC_TEST_VECTORS [I].Key),
                                       LowerCase (BLOWFISH_CBC_TEST_VECTORS [I].IV),
                                       LowerCase (BLOWFISH_CBC_TEST_VECTORS [I].Plain) ) <> LowerCase (BLOWFISH_CBC_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptBlowFishCBC ( LowerCase (BLOWFISH_CBC_TEST_VECTORS [I].Key),
                                       LowerCase (BLOWFISH_CBC_TEST_VECTORS [I].IV),
                                       LowerCase (BLOWFISH_CBC_TEST_VECTORS [I].Cipher) ) <> LowerCase (BLOWFISH_CBC_TEST_VECTORS [I].Plain)  ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;
{$ENDIF CBC}

{$IFDEF CFB}
function TestEncryptBlowFishCFB (const aKey: String;
                                 const anIV: String;
                                 const aValue: String) : String;
var
    ctx       : TBlowFishCtx;
    Key128    : TKey128;
    KeySize   : WORD;
    IV        : TBlowFish64Block;
    Value     : TBlowFish64Block;
    Cipher    : TBlowFish64Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 8*2 ) then
            begin
                IV := HexToBlowFishIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_BLOWFISH_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := SizeOf (Key128);
                ErrorCode := BF_CFB_Init (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_BLOWFISH_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 8*2 ) then
            begin
                Value := HexToBlowFishIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_BLOWFISH_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_BLOWFISH_64_BLOCK;
            ErrorCode := BF_CFB_Encrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := BlowFishIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            IV := NULL_BLOWFISH_64_BLOCK;
            Value := NULL_BLOWFISH_64_BLOCK;
            Cipher := NULL_BLOWFISH_64_BLOCK;

            BF_CFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_BLOWFISH_CFB_ENCRYPT,[KeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

function TestDecryptBlowFishCFB (const aKey: String;
                                 const anIV: String;
                                 const aValue: String) : String;
var
    ctx       : TBlowFishCtx;
    Key128    : TKey128;
    KeySize   : WORD;
    IV        : TBlowFish64Block;
    Value     : TBlowFish64Block;
    Cipher    : TBlowFish64Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 8*2 ) then
            begin
                IV := HexToBlowFishIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_BLOWFISH_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := SizeOf (Key128);
                ErrorCode := BF_CFB_Init (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_BLOWFISH_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 8*2 ) then
            begin
                Value := HexToBlowFishIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_BLOWFISH_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_BLOWFISH_64_BLOCK;
            ErrorCode := BF_CFB_Decrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := BlowFishIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            IV := NULL_BLOWFISH_64_BLOCK;
            Value := NULL_BLOWFISH_64_BLOCK;
            Cipher := NULL_BLOWFISH_64_BLOCK;

            BF_CFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_BLOWFISH_CFB_DECRYPT,[KeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

function TestBlowFishCFB : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (BLOWFISH_CFB_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if (  TestEncryptBlowFishCFB ( UpperCase (BLOWFISH_CFB_TEST_VECTORS [I].Key),
                                       UpperCase (BLOWFISH_CFB_TEST_VECTORS [I].IV),
                                       UpperCase (BLOWFISH_CFB_TEST_VECTORS [I].Plain) ) <> UpperCase (BLOWFISH_CFB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptBlowFishCFB ( UpperCase (BLOWFISH_CFB_TEST_VECTORS [I].Key),
                                       UpperCase (BLOWFISH_CFB_TEST_VECTORS [I].IV),
                                       UpperCase (BLOWFISH_CFB_TEST_VECTORS [I].Cipher) ) <> UpperCase (BLOWFISH_CFB_TEST_VECTORS [I].Plain)  ) then
    {$ELSE}
        if (  TestEncryptBlowFishCFB ( LowerCase (BLOWFISH_CFB_TEST_VECTORS [I].Key),
                                       LowerCase (BLOWFISH_CFB_TEST_VECTORS [I].IV),
                                       LowerCase (BLOWFISH_CFB_TEST_VECTORS [I].Plain) ) <> LowerCase (BLOWFISH_CFB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptBlowFishCFB ( LowerCase (BLOWFISH_CFB_TEST_VECTORS [I].Key),
                                       LowerCase (BLOWFISH_CFB_TEST_VECTORS [I].IV),
                                       LowerCase (BLOWFISH_CFB_TEST_VECTORS [I].Cipher) ) <> LowerCase (BLOWFISH_CFB_TEST_VECTORS [I].Plain)  ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;
{$ENDIF CFB}

{$IFDEF OFB}
function TestEncryptBlowFishOFB (const aKey: String;
                                 const anIV: String;
                                 const aValue: String) : String;
var
    ctx       : TBlowFishCtx;
    Key128    : TKey128;
    KeySize   : WORD;
    IV        : TBlowFish64Block;
    Value     : TBlowFish64Block;
    Cipher    : TBlowFish64Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 8*2 ) then
            begin
                IV := HexToBlowFishIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_BLOWFISH_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := SizeOf (Key128);
                ErrorCode := BF_OFB_Init (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_BLOWFISH_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 8*2 ) then
            begin
                Value := HexToBlowFishIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_BLOWFISH_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_BLOWFISH_64_BLOCK;
            ErrorCode := BF_OFB_Encrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := BlowFishIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            IV := NULL_BLOWFISH_64_BLOCK;
            Value := NULL_BLOWFISH_64_BLOCK;
            Cipher := NULL_BLOWFISH_64_BLOCK;

            BF_OFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_BLOWFISH_OFB_ENCRYPT,[KeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

function TestDecryptBlowFishOFB (const aKey: String;
                                 const anIV: String;
                                 const aValue: String) : String;
var
    ctx       : TBlowFishCtx;
    Key128    : TKey128;
    KeySize   : WORD;
    IV        : TBlowFish64Block;
    Value     : TBlowFish64Block;
    Cipher    : TBlowFish64Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 8*2 ) then
            begin
                IV := HexToBlowFishIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_BLOWFISH_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := SizeOf (Key128);
                ErrorCode := BF_OFB_Init (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_BLOWFISH_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 8*2 ) then
            begin
                Value := HexToBlowFishIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_BLOWFISH_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_BLOWFISH_64_BLOCK;
            ErrorCode := BF_OFB_Decrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := BlowFishIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            IV := NULL_BLOWFISH_64_BLOCK;
            Value := NULL_BLOWFISH_64_BLOCK;
            Cipher := NULL_BLOWFISH_64_BLOCK;

            BF_OFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_BLOWFISH_OFB_DECRYPT,[KeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

function TestBlowFishOFB : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (BLOWFISH_OFB_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if (  TestEncryptBlowFishOFB ( UpperCase (BLOWFISH_OFB_TEST_VECTORS [I].Key),
                                       UpperCase (BLOWFISH_OFB_TEST_VECTORS [I].IV),
                                       UpperCase (BLOWFISH_OFB_TEST_VECTORS [I].Plain) ) <> UpperCase (BLOWFISH_OFB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptBlowFishOFB ( UpperCase (BLOWFISH_OFB_TEST_VECTORS [I].Key),
                                       UpperCase (BLOWFISH_OFB_TEST_VECTORS [I].IV),
                                       UpperCase (BLOWFISH_OFB_TEST_VECTORS [I].Cipher) ) <> UpperCase (BLOWFISH_OFB_TEST_VECTORS [I].Plain)  ) then
    {$ELSE}
        if (  TestEncryptBlowFishOFB ( LowerCase (BLOWFISH_OFB_TEST_VECTORS [I].Key),
                                       LowerCase (BLOWFISH_OFB_TEST_VECTORS [I].IV),
                                       LowerCase (BLOWFISH_OFB_TEST_VECTORS [I].Plain) ) <> LowerCase (BLOWFISH_OFB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptBlowFishOFB ( LowerCase (BLOWFISH_OFB_TEST_VECTORS [I].Key),
                                       LowerCase (BLOWFISH_OFB_TEST_VECTORS [I].IV),
                                       LowerCase (BLOWFISH_OFB_TEST_VECTORS [I].Cipher) ) <> LowerCase (BLOWFISH_OFB_TEST_VECTORS [I].Plain)  ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;
{$ENDIF OFB}
