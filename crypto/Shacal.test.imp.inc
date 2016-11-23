{******************************************************************************}
{*  Shacal.test.imp.inc                                                       *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{$IFDEF ECB}
function TestEncryptShacalECB (const aKey: String;
                               const aValue: String) : String;
var
    ctx       : TShacalCtx;
    Key512    : TKey512;
    KeySize   : WORD;
    Value     : TShacal256Block;
    Cipher    : TShacal256Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (aKey) = 64*2 ) then
            begin
                Key512 := HexToKey512 (aKey);
                KeySize := 512;
                ErrorCode := SC_ECB_Init ( Key512, SizeOf (Key512), ctx);
            end
            else
                raise Exception.CreateFmt (ERR_SHACAL_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 32*2 ) then
            begin
                Value := HexToShacalIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_SHACAL_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_SHACAL_256_BLOCK;
            ErrorCode := SC_ECB_Encrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := ShacalIVToHex (Cipher);
        finally
            Key512 := NULL_512_KEY;
            Value := NULL_SHACAL_256_BLOCK;
            Cipher := NULL_SHACAL_256_BLOCK;

            SC_ECB_Reset (ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SHACAL_ECB_ENCRYPT,[KeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

function TestDecryptShacalECB (const aKey: String;
                               const aValue: String) : String;
var
    ctx       : TShacalCtx;
    Key512    : TKey512;
    KeySize   : WORD;
    Value     : TShacal256Block;
    Cipher    : TShacal256Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (aKey) = 64*2 ) then
            begin
                Key512 := HexToKey512 (aKey);
                KeySize := 512;
                ErrorCode := SC_ECB_Init ( Key512, SizeOf (Key512), ctx);
            end
            else
                raise Exception.CreateFmt (ERR_SHACAL_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 32*2 ) then
            begin
                Value := HexToShacalIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_SHACAL_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_SHACAL_256_BLOCK;
            ErrorCode := SC_ECB_Decrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := ShacalIVToHex (Cipher);
        finally
            Key512 := NULL_512_KEY;
            Value := NULL_SHACAL_256_BLOCK;
            Cipher := NULL_SHACAL_256_BLOCK;

            SC_ECB_Reset (ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SHACAL_ECB_DECRYPT,[KeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

function TestShacalECB : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (SHACAL_ECB_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if (  TestEncryptShacalECB ( UpperCase (SHACAL_ECB_TEST_VECTORS [I].Key),
                                     UpperCase (SHACAL_ECB_TEST_VECTORS [I].Plain) ) <> UpperCase (SHACAL_ECB_TEST_VECTORS [I].Cipher) ) or
           (  TestDecryptShacalECB ( UpperCase (SHACAL_ECB_TEST_VECTORS [I].Key),
                                     UpperCase (SHACAL_ECB_TEST_VECTORS [I].Cipher) ) <> UpperCase (SHACAL_ECB_TEST_VECTORS [I].Plain)  ) then
    {$ELSE}
        if (  TestEncryptShacalECB ( LowerCase (SHACAL_ECB_TEST_VECTORS [I].Key),
                                     LowerCase (SHACAL_ECB_TEST_VECTORS [I].Plain) ) <> LowerCase (SHACAL_ECB_TEST_VECTORS [I].Cipher) ) or
           (  TestDecryptShacalECB ( LowerCase (SHACAL_ECB_TEST_VECTORS [I].Key),
                                     LowerCase (SHACAL_ECB_TEST_VECTORS [I].Cipher) ) <> LowerCase (SHACAL_ECB_TEST_VECTORS [I].Plain)  ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;
{$ENDIF ECB}

{$IFDEF CBC}
function TestEncryptShacalCBC (const aKey: String;
                               const anIV: String;
                               const aValue: String) : String;
var
    ctx       : TShacalCtx;
    Key512    : TKey512;
    KeySize   : WORD;
    IV        : TShacal256Block;
    Value     : TShacal256Block;
    Cipher    : TShacal256Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 32*2 ) then
            begin
                IV := HexToShacalIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_SHACAL_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 64*2 ) then
            begin
                Key512 := HexToKey512 (aKey);
                KeySize := 512;
                ErrorCode := SC_CBC_Init ( Key512, SizeOf (Key512), IV, ctx);
            end
            else
                raise Exception.CreateFmt (ERR_SHACAL_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 32*2 ) then
            begin
                Value := HexToShacalIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_SHACAL_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_SHACAL_256_BLOCK;
            ErrorCode := SC_CBC_Encrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := ShacalIVToHex (Cipher);
        finally
            Key512 := NULL_512_KEY;
            Value := NULL_SHACAL_256_BLOCK;
            Cipher := NULL_SHACAL_256_BLOCK;

            SC_CBC_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SHACAL_CBC_ENCRYPT,[KeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

function TestDecryptShacalCBC (const aKey: String;
                               const anIV: String;
                               const aValue: String) : String;
var
    ctx       : TShacalCtx;
    Key512    : TKey512;
    KeySize   : WORD;
    IV        : TShacal256Block;
    Value     : TShacal256Block;
    Cipher    : TShacal256Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 32*2 ) then
            begin
                IV := HexToShacalIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_SHACAL_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 64*2 ) then
            begin
                Key512 := HexToKey512 (aKey);
                KeySize := 512;
                ErrorCode := SC_CBC_Init ( Key512, SizeOf (Key512), IV, ctx);
            end
            else
                raise Exception.CreateFmt (ERR_SHACAL_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 32*2 ) then
            begin
                Value := HexToShacalIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_SHACAL_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_SHACAL_256_BLOCK;
            ErrorCode := SC_CBC_Decrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := ShacalIVToHex (Cipher);
        finally
            Key512 := NULL_512_KEY;
            Value := NULL_SHACAL_256_BLOCK;
            Cipher := NULL_SHACAL_256_BLOCK;

            SC_CBC_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SHACAL_CBC_DECRYPT,[KeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

function TestShacalCBC : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (SHACAL_CBC_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if (  TestEncryptShacalCBC ( UpperCase (SHACAL_CBC_TEST_VECTORS [I].Key),
                                     UpperCase (SHACAL_CBC_TEST_VECTORS [I].IV),
                                     UpperCase (SHACAL_CBC_TEST_VECTORS [I].Plain) ) <> UpperCase (SHACAL_CBC_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptShacalCBC ( UpperCase (SHACAL_CBC_TEST_VECTORS [I].Key),
                                     UpperCase (SHACAL_CBC_TEST_VECTORS [I].IV),
                                     UpperCase (SHACAL_CBC_TEST_VECTORS [I].Cipher) ) <> UpperCase (SHACAL_CBC_TEST_VECTORS [I].Plain)  ) then
    {$ELSE}
        if (  TestEncryptShacalCBC ( LowerCase (SHACAL_CBC_TEST_VECTORS [I].Key),
                                     LowerCase (SHACAL_CBC_TEST_VECTORS [I].IV),
                                     LowerCase (SHACAL_CBC_TEST_VECTORS [I].Plain) ) <> LowerCase (SHACAL_CBC_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptShacalCBC ( LowerCase (SHACAL_CBC_TEST_VECTORS [I].Key),
                                     LowerCase (SHACAL_CBC_TEST_VECTORS [I].IV),
                                     LowerCase (SHACAL_CBC_TEST_VECTORS [I].Cipher) ) <> LowerCase (SHACAL_CBC_TEST_VECTORS [I].Plain)  ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;
{$ENDIF CBC}

{$IFDEF CFB}
function TestEncryptShacalCFB (const aKey: String;
                               const anIV: String;
                               const aValue: String) : String;
var
    ctx       : TShacalCtx;
    Key512    : TKey512;
    KeySize   : WORD;
    IV        : TShacal256Block;
    Value     : TShacal256Block;
    Cipher    : TShacal256Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 32*2 ) then
            begin
                IV := HexToShacalIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_SHACAL_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 64*2 ) then
            begin
                Key512 := HexToKey512 (aKey);
                KeySize := 512;
                ErrorCode := SC_CFB_Init ( Key512, SizeOf (Key512), IV, ctx);
            end
            else
                raise Exception.CreateFmt (ERR_SHACAL_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 32*2 ) then
            begin
                Value := HexToShacalIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_SHACAL_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_SHACAL_256_BLOCK;
            ErrorCode := SC_CFB_Encrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := ShacalIVToHex (Cipher);
        finally
            Key512 := NULL_512_KEY;
            Value := NULL_SHACAL_256_BLOCK;
            Cipher := NULL_SHACAL_256_BLOCK;

            SC_CFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SHACAL_CFB_ENCRYPT,[KeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

function TestDecryptShacalCFB (const aKey: String;
                               const anIV: String;
                               const aValue: String) : String;
var
    ctx       : TShacalCtx;
    Key512    : TKey512;
    KeySize   : WORD;
    IV        : TShacal256Block;
    Value     : TShacal256Block;
    Cipher    : TShacal256Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 32*2 ) then
            begin
                IV := HexToShacalIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_SHACAL_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 64*2 ) then
            begin
                Key512 := HexToKey512 (aKey);
                KeySize := 512;
                ErrorCode := SC_CFB_Init ( Key512, SizeOf (Key512), IV, ctx);
            end
            else
                raise Exception.CreateFmt (ERR_SHACAL_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 32*2 ) then
            begin
                Value := HexToShacalIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_SHACAL_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_SHACAL_256_BLOCK;
            ErrorCode := SC_CFB_Decrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := ShacalIVToHex (Cipher);
        finally
            Key512 := NULL_512_KEY;
            Value := NULL_SHACAL_256_BLOCK;
            Cipher := NULL_SHACAL_256_BLOCK;

            SC_CFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SHACAL_CFB_DECRYPT,[KeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

function TestShacalCFB : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (SHACAL_CFB_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if (  TestEncryptShacalCFB ( UpperCase (SHACAL_CFB_TEST_VECTORS [I].Key),
                                     UpperCase (SHACAL_CFB_TEST_VECTORS [I].IV),
                                     UpperCase (SHACAL_CFB_TEST_VECTORS [I].Plain) ) <> UpperCase (SHACAL_CFB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptShacalCFB ( UpperCase (SHACAL_CFB_TEST_VECTORS [I].Key),
                                     UpperCase (SHACAL_CFB_TEST_VECTORS [I].IV),
                                     UpperCase (SHACAL_CFB_TEST_VECTORS [I].Cipher) ) <> UpperCase (SHACAL_CFB_TEST_VECTORS [I].Plain)  ) then
    {$ELSE}
        if (  TestEncryptShacalCFB ( LowerCase (SHACAL_CFB_TEST_VECTORS [I].Key),
                                     LowerCase (SHACAL_CFB_TEST_VECTORS [I].IV),
                                     LowerCase (SHACAL_CFB_TEST_VECTORS [I].Plain) ) <> LowerCase (SHACAL_CFB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptShacalCFB ( LowerCase (SHACAL_CFB_TEST_VECTORS [I].Key),
                                     LowerCase (SHACAL_CFB_TEST_VECTORS [I].IV),
                                     LowerCase (SHACAL_CFB_TEST_VECTORS [I].Cipher) ) <> LowerCase (SHACAL_CFB_TEST_VECTORS [I].Plain)  ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;
{$ENDIF CFB}

{$IFDEF OFB}
function TestEncryptShacalOFB (const aKey: String;
                               const anIV: String;
                               const aValue: String) : String;
var
    ctx       : TShacalCtx;
    Key512    : TKey512;
    KeySize   : WORD;
    IV        : TShacal256Block;
    Value     : TShacal256Block;
    Cipher    : TShacal256Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 32*2 ) then
            begin
                IV := HexToShacalIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_SHACAL_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 64*2 ) then
            begin
                Key512 := HexToKey512 (aKey);
                KeySize := 512;
                ErrorCode := SC_OFB_Init ( Key512, SizeOf (Key512), IV, ctx);
            end
            else
                raise Exception.CreateFmt (ERR_SHACAL_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 32*2 ) then
            begin
                Value := HexToShacalIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_SHACAL_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_SHACAL_256_BLOCK;
            ErrorCode := SC_OFB_Encrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := ShacalIVToHex (Cipher);
        finally
            Key512 := NULL_512_KEY;
            Value := NULL_SHACAL_256_BLOCK;
            Cipher := NULL_SHACAL_256_BLOCK;

            SC_OFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SHACAL_OFB_ENCRYPT,[KeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

function TestDecryptShacalOFB (const aKey: String;
                               const anIV: String;
                               const aValue: String) : String;
var
    ctx       : TShacalCtx;
    Key512    : TKey512;
    KeySize   : WORD;
    IV        : TShacal256Block;
    Value     : TShacal256Block;
    Cipher    : TShacal256Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 32*2 ) then
            begin
                IV := HexToShacalIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_SHACAL_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 64*2 ) then
            begin
                Key512 := HexToKey512 (aKey);
                KeySize := 512;
                ErrorCode := SC_OFB_Init ( Key512, SizeOf (Key512), IV, ctx);
            end
            else
                raise Exception.CreateFmt (ERR_SHACAL_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 32*2 ) then
            begin
                Value := HexToShacalIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_SHACAL_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_SHACAL_256_BLOCK;
            ErrorCode := SC_OFB_Decrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := ShacalIVToHex (Cipher);
        finally
            Key512 := NULL_512_KEY;
            Value := NULL_SHACAL_256_BLOCK;
            Cipher := NULL_SHACAL_256_BLOCK;

            SC_OFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SHACAL_OFB_DECRYPT,[KeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

function TestShacalOFB : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (SHACAL_OFB_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if (  TestEncryptShacalOFB ( UpperCase (SHACAL_OFB_TEST_VECTORS [I].Key),
                                     UpperCase (SHACAL_OFB_TEST_VECTORS [I].IV),
                                     UpperCase (SHACAL_OFB_TEST_VECTORS [I].Plain) ) <> UpperCase (SHACAL_OFB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptShacalOFB ( UpperCase (SHACAL_OFB_TEST_VECTORS [I].Key),
                                     UpperCase (SHACAL_OFB_TEST_VECTORS [I].IV),
                                     UpperCase (SHACAL_OFB_TEST_VECTORS [I].Cipher) ) <> UpperCase (SHACAL_OFB_TEST_VECTORS [I].Plain)  ) then
    {$ELSE}
        if (  TestEncryptShacalOFB ( LowerCase (SHACAL_OFB_TEST_VECTORS [I].Key),
                                     LowerCase (SHACAL_OFB_TEST_VECTORS [I].IV),
                                     LowerCase (SHACAL_OFB_TEST_VECTORS [I].Plain) ) <> LowerCase (SHACAL_OFB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptShacalOFB ( LowerCase (SHACAL_OFB_TEST_VECTORS [I].Key),
                                     LowerCase (SHACAL_OFB_TEST_VECTORS [I].IV),
                                     LowerCase (SHACAL_OFB_TEST_VECTORS [I].Cipher) ) <> LowerCase (SHACAL_OFB_TEST_VECTORS [I].Plain)  ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;
{$ENDIF OFB}
