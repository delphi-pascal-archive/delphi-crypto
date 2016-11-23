{******************************************************************************}
{*  RSA.test.imp.inc                                                          *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
function TestEncryptRSA (const pub_e: String;
                         const pub_n: String;
                         const aSeed: String;
                         const aValue: String) : String;
var
    oSize       : WORD;
    n           : mp_int; { модуль }
    e           : mp_int; { открытая экспонента }
    Seed        : mp_int; { смещение }
    L           : WORD;
    InputBlock  : PRSABlock;
    OutputBlock : PRSABlock;
begin
    Result := '';
    try
        {$IFDEF HEX_UPPER_CASE}
        HexUpper := TRUE;
        {$ELSE}
        HexUpper := FALSE;
        {$ENDIF HEX_UPPER_CASE}
        mp_clearzero := TRUE;
        mp_show_plus := FALSE;

        mp_init2 (n,e);
        mp_init (seed);
        try
            mp_read_radix (n, PAnsiChar (pub_n), 16);
            mp_read_radix (e, PAnsiChar (pub_e), 16);
            mp_read_radix (Seed, PAnsiChar (aSeed), 16);

            oSize := mp_unsigned_bin_size (n);
            L := oSize div 2;

            InputBlock := AllocMem ( oSize * SizeOf (Byte) );
            OutputBlock := AllocMem ( oSize * SizeOf (Byte) );
            try
                HexToRSABlock ( Copy (aValue,1,L), InputBlock, oSize*8 );
                NULL_RSA_BLOCK ( OutputBlock, oSize*8 );

                mp_pkcs1v15_encrypt (  e, n, Byte ( mp_get_int (Seed) ),
                                       InputBlock, OutputBlock,
                                       L-1, oSize, L  );

                Result := RSABlockToHex ( OutputBlock, oSize*8, L );
            finally
                L := 0;
                NULL_RSA_BLOCK ( InputBlock, oSize*8 );
                NULL_RSA_BLOCK ( OutputBlock, oSize*8 );

                Dispose (InputBlock);
                InputBlock := NIL;
                Dispose (OutputBlock);
                OutputBlock := NIL;
            end;

        finally
            mp_clear (Seed);
            mp_clear2 (n,e);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_RSA_ENCRYPT,[RSA_VERSION]), E.Message ]);
    end;
end;

function TestDecryptRSA (const prv_d: String;
                         const prv_n: String;
                         const aValue: String) : String;
var
    oSize       : WORD;
    n           : mp_int; { модуль }
    d           : mp_int; { секретная экспонента }
    L           : WORD;
    InputBlock  : PRSABlock;
    OutputBlock : PRSABlock;
begin
    Result := '';
    try
        {$IFDEF HEX_UPPER_CASE}
        HexUpper := TRUE;
        {$ELSE}
        HexUpper := FALSE;
        {$ENDIF HEX_UPPER_CASE}
        mp_clearzero := TRUE;
        mp_show_plus := FALSE;
        
        mp_init2 (n,d);
        try
            mp_read_radix (n, PAnsiChar (prv_n), 16);
            mp_read_radix (d, PAnsiChar (prv_d), 16);

            oSize := mp_unsigned_bin_size (n);
            L := (oSize+1)*2;

            InputBlock := AllocMem ( oSize * SizeOf (Byte) );
            OutputBlock := AllocMem ( oSize * SizeOf (Byte) );
            try
                HexToRSABlock ( Copy (aValue,1,L), InputBlock, oSize*8 );
                NULL_RSA_BLOCK ( OutputBlock, oSize*8 );

                mp_pkcs1v15_decrypt ( d, n,
                                      InputBlock, OutputBlock,
                                      (L div 2)-1, oSize, L );

                Result := RSABlockToHex ( OutputBlock, oSize*8, (L+1) );
                Result := Copy ( Result, 1, (L+1) );
            finally
                L := 0;
                NULL_RSA_BLOCK ( InputBlock, oSize*8 );
                NULL_RSA_BLOCK ( OutputBlock, oSize*8 );

                Dispose (InputBlock);
                InputBlock := NIL;
                Dispose (OutputBlock);
                OutputBlock := NIL;
            end;

        finally
            mp_clear2 (n,d);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_RSA_DECRYPT,[RSA_VERSION]), E.Message ]);
    end;
end;

function TestRSA : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 0 to High (RSA_TEST_VECTORS) do
    {$IFDEF HEX_UPPER_CASE}
        if (  TestEncryptRSA ( UpperCase (RSA_TEST_VECTORS [I].e),
                               UpperCase (RSA_TEST_VECTORS [I].n),
                               UpperCase (RSA_TEST_VECTORS [I].Seed),
                               UpperCase (RSA_TEST_VECTORS [I].Plain) ) <> UpperCase (RSA_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptRSA ( UpperCase (RSA_TEST_VECTORS [I].d),
                               UpperCase (RSA_TEST_VECTORS [I].n),
                               UpperCase (RSA_TEST_VECTORS [I].Cipher) ) <> UpperCase (RSA_TEST_VECTORS [I].Plain)  ) then
    {$ELSE}
        if (  TestEncryptRSA ( LowerCase (RSA_TEST_VECTORS [I].e),
                               LowerCase (RSA_TEST_VECTORS [I].n),
                               LowerCase (RSA_TEST_VECTORS [I].Seed),
                               LowerCase (RSA_TEST_VECTORS [I].Plain) ) <> LowerCase (RSA_TEST_VECTORS [I].Cipher)  ) or
           (  TestDecryptRSA ( LowerCase (RSA_TEST_VECTORS [I].d),
                               LowerCase (RSA_TEST_VECTORS [I].n),
                               LowerCase (RSA_TEST_VECTORS [I].Cipher) ) <> LowerCase (RSA_TEST_VECTORS [I].Plain)  ) then
    {$ENDIF HEX_UPPER_CASE}
        begin
            Result := FALSE;
            Break;
        end;
end;

function TestEncryptRSA (const aPublicKey: String;
                         const aSeed: String;
                         const aValue: String) : String;
var
    pub_n   : String;
    pub_e   : String;
    nLength : WORD;
    eLength : WORD;
begin
    try
        try
            eLength := StrToInt ( Copy (aPublicKey,1,4) );
            pub_e := Copy ( aPublicKey, 5, eLength );
            nLength := StrToInt ( Copy (aPublicKey,eLength+5,4) );
            pub_n := Copy ( aPublicKey, eLength+9, nLength );
            Result := TestEncryptRSA (pub_e,pub_n,aSeed,aValue);
        finally
            _FillChar ( pub_e, Length (pub_e), $00 );
            _FillChar ( pub_n, Length (pub_n), $00 );
            nLength := 0;
            eLength := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_RSA_ENCRYPT,[RSA_VERSION]), E.Message ]);
    end;
end;

function TestDecryptRSA (const aPrivateKey: String;
                         const aValue: String) : String;
var
    prv_n   : String;
    prv_d   : String;
    nLength : WORD;
    dLength : WORD;
begin
    try
        try
            dLength := StrToInt ( Copy (aPrivateKey,1,4) );
            prv_d := Copy ( aPrivateKey, 5, dLength );
            nLength := StrToInt ( Copy (aPrivateKey,dLength+5,4) );
            prv_n := Copy ( aPrivateKey, dLength+9, nLength );
            Result := TestDecryptRSA (prv_d,prv_n,aValue);
        finally
            _FillChar ( prv_d, Length (prv_d), $00 );
            _FillChar ( prv_n, Length (prv_n), $00 );
            nLength := 0;
            dLength := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_RSA_DECRYPT,[RSA_VERSION]), E.Message ]);
    end;
end;