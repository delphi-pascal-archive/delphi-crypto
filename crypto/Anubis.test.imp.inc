{******************************************************************************}
{*  Anubis.test.imp.inc                                                       *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{$IFDEF ECB}
function TestEncryptAnubisECB (const aKey: String;
                               const aValue: String) : String;
var
    ctx       : TAnubisCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    Value     : TAnubis128Block;
    Cipher    : TAnubis128Block;
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
                ErrorCode := ANU_ECB_Init_Encr (Key256,KeySize,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := ANU_ECB_Init_Encr (Key192,KeySize,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := ANU_ECB_Init_Encr (Key128,KeySize,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_ANUBIS_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToAnubisIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_ANUBIS_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_ANUBIS_128_BLOCK;
            ErrorCode := ANU_ECB_Encrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := AnubisIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            Value := NULL_ANUBIS_128_BLOCK;
            Cipher := NULL_ANUBIS_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_ECB_ENCRYPT,[KeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

function TestDecryptAnubisECB (const aKey: String;
                               const aValue: String) : String;
var
    ctx       : TAnubisCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    Value     : TAnubis128Block;
    Cipher    : TAnubis128Block;
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
                ErrorCode := ANU_ECB_Init_Decr (Key256,KeySize,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := ANU_ECB_Init_Decr (Key192,KeySize,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := ANU_ECB_Init_Decr (Key128,KeySize,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_ANUBIS_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToAnubisIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_ANUBIS_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_ANUBIS_128_BLOCK;
            ErrorCode := ANU_ECB_Decrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := AnubisIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            Value := NULL_ANUBIS_128_BLOCK;
            Cipher := NULL_ANUBIS_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_ECB_DECRYPT,[KeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

function TestAnubisECB : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (ANUBIS_ECB_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if (  TestEncryptAnubisECB ( UpperCase (ANUBIS_ECB_TEST_VECTORS [I].Key),
                                     UpperCase (ANUBIS_ECB_TEST_VECTORS [I].Plain) ) <> UpperCase (ANUBIS_ECB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptAnubisECB ( UpperCase (ANUBIS_ECB_TEST_VECTORS [I].Key),
                                     UpperCase (ANUBIS_ECB_TEST_VECTORS [I].Cipher) ) <> UpperCase (ANUBIS_ECB_TEST_VECTORS [I].Plain)  ) then
    {$ELSE}
        if (  TestEncryptAnubisECB ( LowerCase (ANUBIS_ECB_TEST_VECTORS [I].Key),
                                     LowerCase (ANUBIS_ECB_TEST_VECTORS [I].Plain) ) <> LowerCase (ANUBIS_ECB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptAnubisECB ( LowerCase (ANUBIS_ECB_TEST_VECTORS [I].Key),
                                     LowerCase (ANUBIS_ECB_TEST_VECTORS [I].Cipher) ) <> LowerCase (ANUBIS_ECB_TEST_VECTORS [I].Plain)  ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;
{$ENDIF ECB}

{$IFDEF CBC}
function TestEncryptAnubisCBC (const aKey: String;
                               const anIV: String;
                               const aValue: String) : String;
var
    ctx       : TAnubisCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    IV        : TAnubis128Block;
    Value     : TAnubis128Block;
    Cipher    : TAnubis128Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 16*2 ) then
            begin
                IV := HexToAnubisIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_ANUBIS_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 32*2 ) then
            begin
                Key256 := HexToKey256 (aKey);
                KeySize := 256;
                ErrorCode := ANU_CBC_Init_Encr (Key256,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := ANU_CBC_Init_Encr (Key192,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := ANU_CBC_Init_Encr (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_ANUBIS_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToAnubisIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_ANUBIS_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_ANUBIS_128_BLOCK;
            ErrorCode := ANU_CBC_Encrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := AnubisIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            IV := NULL_ANUBIS_128_BLOCK;
            Value := NULL_ANUBIS_128_BLOCK;
            Cipher := NULL_ANUBIS_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_CBC_ENCRYPT,[KeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

function TestDecryptAnubisCBC (const aKey: String;
                               const anIV: String;
                               const aValue: String) : String;
var
    ctx       : TAnubisCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    IV        : TAnubis128Block;
    Value     : TAnubis128Block;
    Cipher    : TAnubis128Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 16*2 ) then
            begin
                IV := HexToAnubisIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_ANUBIS_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 32*2 ) then
            begin
                Key256 := HexToKey256 (aKey);
                KeySize := 256;
                ErrorCode := ANU_CBC_Init_Decr (Key256,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := ANU_CBC_Init_Decr (Key192,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := ANU_CBC_Init_Decr (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_ANUBIS_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToAnubisIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_ANUBIS_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_ANUBIS_128_BLOCK;
            ErrorCode := ANU_CBC_Decrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := AnubisIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            IV := NULL_ANUBIS_128_BLOCK;
            Value := NULL_ANUBIS_128_BLOCK;
            Cipher := NULL_ANUBIS_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_CBC_DECRYPT,[KeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

function TestAnubisCBC : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (ANUBIS_CBC_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if (  TestEncryptAnubisCBC ( UpperCase (ANUBIS_CBC_TEST_VECTORS [I].Key),
                                     UpperCase (ANUBIS_CBC_TEST_VECTORS [I].IV),
                                     UpperCase (ANUBIS_CBC_TEST_VECTORS [I].Plain) ) <> UpperCase (ANUBIS_CBC_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptAnubisCBC ( UpperCase (ANUBIS_CBC_TEST_VECTORS [I].Key),
                                     UpperCase (ANUBIS_CBC_TEST_VECTORS [I].IV),
                                     UpperCase (ANUBIS_CBC_TEST_VECTORS [I].Cipher) ) <> UpperCase (ANUBIS_CBC_TEST_VECTORS [I].Plain)  ) then
    {$ELSE}
        if (  TestEncryptAnubisCBC ( LowerCase (ANUBIS_CBC_TEST_VECTORS [I].Key),
                                     LowerCase (ANUBIS_CBC_TEST_VECTORS [I].IV),
                                     LowerCase (ANUBIS_CBC_TEST_VECTORS [I].Plain) ) <> LowerCase (ANUBIS_CBC_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptAnubisCBC ( LowerCase (ANUBIS_CBC_TEST_VECTORS [I].Key),
                                     LowerCase (ANUBIS_CBC_TEST_VECTORS [I].IV),
                                     LowerCase (ANUBIS_CBC_TEST_VECTORS [I].Cipher) ) <> LowerCase (ANUBIS_CBC_TEST_VECTORS [I].Plain)  ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;
{$ENDIF CBC}

{$IFDEF CFB}
function TestEncryptAnubisCFB (const aKey: String;
                               const anIV: String;
                               const aValue: String) : String;
var
    ctx       : TAnubisCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    IV        : TAnubis128Block;
    Value     : TAnubis128Block;
    Cipher    : TAnubis128Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 16*2 ) then
            begin
                IV := HexToAnubisIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_ANUBIS_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 32*2 ) then
            begin
                Key256 := HexToKey256 (aKey);
                KeySize := 256;
                ErrorCode := ANU_CFB_Init (Key256,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := ANU_CFB_Init (Key192,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := ANU_CFB_Init (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_ANUBIS_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToAnubisIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_ANUBIS_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_ANUBIS_128_BLOCK;
            ErrorCode := ANU_CFB_Encrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := AnubisIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            IV := NULL_ANUBIS_128_BLOCK;
            Value := NULL_ANUBIS_128_BLOCK;
            Cipher := NULL_ANUBIS_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_CFB_ENCRYPT,[KeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

function TestDecryptAnubisCFB (const aKey: String;
                               const anIV: String;
                               const aValue: String) : String;
var
    ctx       : TAnubisCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    IV        : TAnubis128Block;
    Value     : TAnubis128Block;
    Cipher    : TAnubis128Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 16*2 ) then
            begin
                IV := HexToAnubisIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_ANUBIS_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 32*2 ) then
            begin
                Key256 := HexToKey256 (aKey);
                KeySize := 256;
                ErrorCode := ANU_CFB_Init (Key256,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := ANU_CFB_Init (Key192,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := ANU_CFB_Init (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_ANUBIS_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToAnubisIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_ANUBIS_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_ANUBIS_128_BLOCK;
            ErrorCode := ANU_CFB_Decrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := AnubisIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            IV := NULL_ANUBIS_128_BLOCK;
            Value := NULL_ANUBIS_128_BLOCK;
            Cipher := NULL_ANUBIS_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_CFB_DECRYPT,[KeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

function TestAnubisCFB : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (ANUBIS_CFB_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if (  TestEncryptAnubisCFB ( UpperCase (ANUBIS_CFB_TEST_VECTORS [I].Key),
                                     UpperCase (ANUBIS_CFB_TEST_VECTORS [I].IV),
                                     UpperCase (ANUBIS_CFB_TEST_VECTORS [I].Plain) ) <> UpperCase (ANUBIS_CFB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptAnubisCFB ( UpperCase (ANUBIS_CFB_TEST_VECTORS [I].Key),
                                     UpperCase (ANUBIS_CFB_TEST_VECTORS [I].IV),
                                     UpperCase (ANUBIS_CFB_TEST_VECTORS [I].Cipher) ) <> UpperCase (ANUBIS_CFB_TEST_VECTORS [I].Plain)  ) then
    {$ELSE}
        if (  TestEncryptAnubisCFB ( LowerCase (ANUBIS_CFB_TEST_VECTORS [I].Key),
                                     LowerCase (ANUBIS_CFB_TEST_VECTORS [I].IV),
                                     LowerCase (ANUBIS_CFB_TEST_VECTORS [I].Plain) ) <> LowerCase (ANUBIS_CFB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptAnubisCFB ( LowerCase (ANUBIS_CFB_TEST_VECTORS [I].Key),
                                     LowerCase (ANUBIS_CFB_TEST_VECTORS [I].IV),
                                     LowerCase (ANUBIS_CFB_TEST_VECTORS [I].Cipher) ) <> LowerCase (ANUBIS_CFB_TEST_VECTORS [I].Plain)  ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;
{$ENDIF CFB}

{$IFDEF OFB}
function TestEncryptAnubisOFB (const aKey: String;
                               const anIV: String;
                               const aValue: String) : String;
var
    ctx       : TAnubisCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    IV        : TAnubis128Block;
    Value     : TAnubis128Block;
    Cipher    : TAnubis128Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 16*2 ) then
            begin
                IV := HexToAnubisIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_ANUBIS_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 32*2 ) then
            begin
                Key256 := HexToKey256 (aKey);
                KeySize := 256;
                ErrorCode := ANU_OFB_Init (Key256,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := ANU_OFB_Init (Key192,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := ANU_OFB_Init (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_ANUBIS_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToAnubisIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_ANUBIS_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_ANUBIS_128_BLOCK;
            ErrorCode := ANU_OFB_Encrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := AnubisIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            IV := NULL_ANUBIS_128_BLOCK;
            Value := NULL_ANUBIS_128_BLOCK;
            Cipher := NULL_ANUBIS_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_OFB_ENCRYPT,[KeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

function TestDecryptAnubisOFB (const aKey: String;
                               const anIV: String;
                               const aValue: String) : String;
var
    ctx       : TAnubisCtx;
    Key128    : TKey128;
    Key192    : TKey192;
    Key256    : TKey256;
    KeySize   : WORD;
    IV        : TAnubis128Block;
    Value     : TAnubis128Block;
    Cipher    : TAnubis128Block;
    ErrorCode : Integer;
begin
    Result := '';
    try
        ErrorCode := 0;
        try
            if ( Length (anIV) = 16*2 ) then
            begin
                IV := HexToAnubisIV (anIV);
            end
            else
                raise Exception.CreateFmt (ERR_ANUBIS_INCORRECT_IV,[anIV]);
            if ( Length (aKey) = 32*2 ) then
            begin
                Key256 := HexToKey256 (aKey);
                KeySize := 256;
                ErrorCode := ANU_OFB_Init (Key256,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 24*2 ) then
            begin
                Key192 := HexToKey192 (aKey);
                KeySize := 192;
                ErrorCode := ANU_OFB_Init (Key192,KeySize,IV,ctx);
            end
            else if ( Length (aKey) = 16*2 ) then
            begin
                Key128 := HexToKey128 (aKey);
                KeySize := 128;
                ErrorCode := ANU_OFB_Init (Key128,KeySize,IV,ctx);
            end
            else
                raise Exception.CreateFmt (ERR_ANUBIS_INCORRECT_KEY,[aKey]);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            if ( Length (aValue) = 16*2 ) then
            begin
                Value := HexToAnubisIV (aValue);
            end
            else
                raise Exception.CreateFmt (ERR_ANUBIS_INCORRECT_BLOCK,[aValue]);
            Cipher := NULL_ANUBIS_128_BLOCK;
            ErrorCode := ANU_OFB_Decrypt (@Value, @Cipher, SizeOf (Value), ctx);
            if ( ErrorCode <> 0 ) then
                raise Exception.CreateFmt ('%d',[ErrorCode]);
            Result := AnubisIVToHex (Cipher);
        finally
            Key128 := NULL_128_KEY;
            Key192 := NULL_192_KEY;
            Key256 := NULL_256_KEY;
            IV := NULL_ANUBIS_128_BLOCK;
            Value := NULL_ANUBIS_128_BLOCK;
            Cipher := NULL_ANUBIS_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_OFB_DECRYPT,[KeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

function TestAnubisOFB : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (ANUBIS_OFB_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if (  TestEncryptAnubisOFB ( UpperCase (ANUBIS_OFB_TEST_VECTORS [I].Key),
                                     UpperCase (ANUBIS_OFB_TEST_VECTORS [I].IV),
                                     UpperCase (ANUBIS_OFB_TEST_VECTORS [I].Plain) ) <> UpperCase (ANUBIS_OFB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptAnubisOFB ( UpperCase (ANUBIS_OFB_TEST_VECTORS [I].Key),
                                     UpperCase (ANUBIS_OFB_TEST_VECTORS [I].IV),
                                     UpperCase (ANUBIS_OFB_TEST_VECTORS [I].Cipher) ) <> UpperCase (ANUBIS_OFB_TEST_VECTORS [I].Plain)  ) then
    {$ELSE}
        if (  TestEncryptAnubisOFB ( LowerCase (ANUBIS_OFB_TEST_VECTORS [I].Key),
                                     LowerCase (ANUBIS_OFB_TEST_VECTORS [I].IV),
                                     LowerCase (ANUBIS_OFB_TEST_VECTORS [I].Plain) ) <> LowerCase (ANUBIS_OFB_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptAnubisOFB ( LowerCase (ANUBIS_OFB_TEST_VECTORS [I].Key),
                                     LowerCase (ANUBIS_OFB_TEST_VECTORS [I].IV),
                                     LowerCase (ANUBIS_OFB_TEST_VECTORS [I].Cipher) ) <> LowerCase (ANUBIS_OFB_TEST_VECTORS [I].Plain)  ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;
{$ENDIF OFB}
