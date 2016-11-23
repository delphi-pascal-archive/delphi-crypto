{******************************************************************************}
{*  RSA.imp.inc                                                               *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
procedure NULL_RSA_BLOCK (var aBlock: PRSABlock;
                          const aSize: WORD);
var
    oSize : WORD;
    I     : Integer;
begin
    oSize := aSize div 8;
    for I := 0 to oSize - 1 do
        aBlock^ [I] := $00;
end;

function RSABlockToHex (const aValue: PRSABlock;
                        const aSize: WORD;
                        const aLength: Integer = 0) : Hex;
var
    oSize : WORD;
    I     : Integer;
    L     : Integer;
begin
    Result := '';
    oSize := aSize div 8;
    if ( (aLength >= 0) and (aLength <= oSize - 1) ) then
        L := aLength
    else
        L := oSize - 1;
    for I := 0 to L do
        Result := Result + IntToHex ( aValue^ [I], 2 );
end;

procedure HexToRSABlock (const aValue: Hex;
                         out aResult: PRSABlock;
                         const aSize: WORD);
var
    oSize  : WORD;
    I      : Integer;
    J      : Integer;
    Buffer : String [3];
begin
    NULL_RSA_BLOCK (aResult,aSize);
    oSize := aSize div 8;
    if not IsHex (aValue) then
        Exit;
    I := 0;
    J := 0;
    while ( I < Length (aValue) ) do
    begin
        Inc (I);
        if ( I mod 2 = 0 ) then
        begin
            if ( J > (oSize - 1) )  then
                Break;
            Buffer := Format ('$%s%s',[ aValue [I-1], aValue [I] ]);
            aResult^ [J] := StrToInt (Buffer);
            Inc (J);
        end;
    end;
end;

procedure GenKeysRSA (const aSize: WORD;
                      aRandomFunc: TRandomFunction;
                      out aPublicKey: String;
                      out aPrivateKey: String);
var
    oSize : WORD;
    n     : mp_int; { модуль }
    d     : mp_int; { секретная экспонента }
    e     : mp_int; { открытая экспонента }
    n_hex : String;
    d_hex : String;
    e_hex : String;
    I     : LongWord;
    R     : LongWord;
begin
    try
        {$IFDEF HEX_UPPER_CASE}
        HexUpper := TRUE;
        {$ELSE}
        HexUpper := FALSE;
        {$ENDIF HEX_UPPER_CASE}
        mp_clearzero := TRUE;
        mp_show_plus := FALSE;

        { вычисляем размер в байтах }
        oSize := aSize div 8;

        mp_init3 (n,d,e);
        try
            R := aRandomFunc (oSize*2);
            for I := 0 to R do
                mp_rand_prime ( oSize*2, pt_normal, e );
            mp_rsa_keygen1 (e,oSize,d,n);

            n_hex := lpad ( mp_ahex (n), (oSize * 2),   '0' );
            d_hex := lpad ( mp_ahex (d), (oSize * 2),   '0' );
            e_hex := lpad ( mp_ahex (e), (oSize div 2), '0' );

            {$IFDEF HEX_UPPER_CASE}
            aPublicKey := UpperCase ( Format ('%s%s%s%s',
                                              [ IntToHex ( Length (e_hex), 4 ), e_hex,
                                                IntToHex ( Length (n_hex), 4 ), n_hex ]) );
            aPrivateKey := UpperCase ( Format ('%s%s%s%s',
                                               [ IntToHex ( Length (d_hex), 4 ), d_hex,
                                                 IntToHex ( Length (n_hex), 4 ), n_hex ]) );
            {$ELSE}
            aPublicKey := LowerCase ( Format ('%s%s%s%s',
                                              [ IntToHex ( Length (e_hex), 4 ), e_hex,
                                                IntToHex ( Length (n_hex), 4 ), n_hex ]) );
            aPrivateKey := LowerCase ( Format ('%s%s%s%s',
                                               [ IntToHex ( Length (d_hex), 4 ), d_hex,
                                                 IntToHex ( Length (n_hex), 4 ), n_hex ]) );
            {$ENDIF HEX_UPPER_CASE}
        finally
            _FillChar ( n_hex, Length (n_hex), $00 );
            _FillChar ( d_hex, Length (d_hex), $00 );
            _FillChar ( e_hex, Length (e_hex), $00 );
            I := 0;
            R := 0;
            mp_clear3 (n,d,e);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_RSA_GEN_KEYS,[RSA_VERSION]), E.Message ]);
    end;
end;

procedure _EncryptRSA (const aPublicKey: String;
                       const anInput: TStream;
                       out anOutput: TStream);
var
    oSize        : WORD;
    BlockSize    : WORD;
    n            : mp_int; { модуль }
    e            : mp_int; { открытая экспонента }
    nLength      : WORD;
    eLength      : WORD;
    pub_n        : String;
    pub_e        : String;
    InputBlock   : PRSABlock;
    OutputBlock  : PRSABlock;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : WORD;
begin
    try
        {$IFDEF HEX_UPPER_CASE}
        HexUpper := TRUE;
        {$ELSE}
        HexUpper := FALSE;
        {$ENDIF HEX_UPPER_CASE}
        mp_clearzero := TRUE;
        mp_show_plus := FALSE;

        mp_init2 (n,e);
        pub_n := '';
        pub_e := '';
        try
            eLength := HexToInt ( Copy (aPublicKey,1,4) );
            pub_e := Copy ( aPublicKey, 5, eLength );
            nLength := HexToInt ( Copy (aPublicKey,eLength+5,4) );
            pub_n := Copy ( aPublicKey, eLength+9, nLength );

            if (  (pub_n = '') or ( not IsHex (pub_n) ) or
                  (pub_e = '') or ( not IsHex (pub_e) )  ) then
                raise Exception.CreateFmt (ERR_RSA_INCORRECT_PUBLIC_KEY,[aPublicKey]);
            mp_read_radix (n, PAnsiChar (pub_n), 16);
            mp_read_radix (e, PAnsiChar (pub_e), 16);

            oSize := mp_unsigned_bin_size (n);
            BlockSize := oSize div 2;

            InputBlock := AllocMem ( oSize * SizeOf (Byte) );
            OutputBlock := AllocMem ( oSize * SizeOf (Byte) );
            try

                with anInput do
                try
                    anInput.Position := 0;
                    anOutput.Position := 0;
                    { шифрование }
                    L := BlockSize;
                    InputBuffer := AllocMem (L+1);
                    while ( Position < Size ) do
                    begin
                        if ( (Size - Position) < BlockSize ) then
                        begin
                            { очищаем буфер для чтения
                              в случае, если он не будет перезаписан полностью }
                            ZeroMemory (InputBuffer,L);
                            Dispose (InputBuffer);
                            InputBuffer := AllocMem (L+1);
                        end;
                        if ( Read (InputBuffer^,L) > 0 ) then
                        try
                            HexToRSABlock ( StrPas (InputBuffer), InputBlock, oSize*8 );
                            NULL_RSA_BLOCK ( OutputBlock, oSize*8 );

                            mp_pkcs1v15_encrypt ( e, n, 0,
                                                  InputBlock, OutputBlock,
                                                  BlockSize-1, oSize, L );

                            OutputBuffer := PChar (  RSABlockToHex ( OutputBlock, oSize*8, L )  );
                            anOutput.WriteBuffer (OutputBuffer^,L*2);
                        finally
                            FillChar (OutputBuffer^,L*2,$00);
                            L := BlockSize;
                        end;
                    end;
                finally
                    ZeroMemory (InputBuffer,L);
                    Dispose (InputBuffer);
                    L := 0;
                    NULL_RSA_BLOCK ( InputBlock, oSize*8 );
                    NULL_RSA_BLOCK ( OutputBlock, oSize*8 );
                end;

            finally
                Dispose (InputBlock);
                InputBlock := NIL;
                Dispose (OutputBlock);
                OutputBlock := NIL;
            end;

        finally
            _FillChar ( pub_n, Length (pub_n), $00 );
            _FillChar ( pub_e, Length (pub_e), $00 );
            nLength := 0;
            eLength := 0;
            mp_clear2 (n,e);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_RSA_ENCRYPT,[RSA_VERSION]), E.Message ]);
    end;
end;

procedure EncryptRSA (const aPublicKey: String;
                      const anInput: TStream;
                      out anOutput: TStream);
var
    Hex : TStream;
begin
    try
        Hex := TMemoryStream.Create;
        try
            StreamToHexStream (anInput,Hex);
            _EncryptRSA (aPublicKey,Hex,anOutput);
        finally
            //ZeroMemory ( TMemoryStream (Hex).Memory, Hex.Size );
            FreeAndNil (Hex);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_RSA_ENCRYPT,[RSA_VERSION]), E.Message ]);
    end;
end;


function EncryptRSA (const aPublicKey: String;
                     const aValue: String) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create (aValue);
        Output := TStringStream.Create ('');
        try
            EncryptRSA (aPublicKey,Input,Output);
            with TStringStream (Output) do
            begin
                Position := 0;
                Result := ReadString (Size);
                //Result := _Trim (Result,$00);
            end;
        finally
            FreeAndNil (Output);
            FreeAndNil (Input);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_RSA_ENCRYPT,[RSA_VERSION]), E.Message ]);
    end;
end;

procedure _DecryptRSA (const aPrivateKey: String;
                       const anInput: TStream;
                       out anOutput: TStream);
var
    oSize        : WORD;
    BlockSize    : WORD;
    n            : mp_int; { модуль }
    d            : mp_int; { секретная экспонента }
    nLength      : WORD;
    dLength      : WORD;
    prv_n        : String;
    prv_d        : String;
    InputBlock   : PRSABlock;
    OutputBlock  : PRSABlock;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : WORD;
begin
    try
        {$IFDEF HEX_UPPER_CASE}
        HexUpper := TRUE;
        {$ELSE}
        HexUpper := FALSE;
        {$ENDIF HEX_UPPER_CASE}
        mp_clearzero := TRUE;
        mp_show_plus := FALSE;

        mp_init2 (n,d);
        prv_n := '';
        prv_d := '';
        try
            dLength := HexToInt ( Copy (aPrivateKey,1,4) );
            prv_d := Copy ( aPrivateKey, 5, dLength );
            nLength := HexToInt ( Copy (aPrivateKey,dLength+5,4) );
            prv_n := Copy ( aPrivateKey, dLength+9, nLength );

            if (  (prv_n = '') or ( not IsHex (prv_n) ) or
                  (prv_d = '') or ( not IsHex (prv_d) )  ) then
                raise Exception.Create (ERR_RSA_INCORRECT_PRIVATE_KEY);
            mp_read_radix (n, PAnsiChar (prv_n), 16);
            mp_read_radix (d, PAnsiChar (prv_d), 16);

            oSize := mp_unsigned_bin_size (n);
            BlockSize := oSize*2;

            InputBlock := AllocMem ( oSize * SizeOf (Byte) );
            OutputBlock := AllocMem ( oSize * SizeOf (Byte) );
            try

                with anInput do
                try
                    anInput.Position := 0;
                    anOutput.Position := 0;
                    { дешифровка }
                    L := BlockSize;
                    InputBuffer := AllocMem (L+1);
                    while ( Position < Size ) do
                    begin
                        if ( (Size - Position) < BlockSize ) then
                        begin
                            { очищаем буфер для чтения
                              в случае, если он не будет перезаписан полностью }
                            ZeroMemory (InputBuffer,L);
                            Dispose (InputBuffer);
                            InputBuffer := AllocMem (L+1);
                        end;
                        if ( Read (InputBuffer^,L) > 0 ) then
                        try
                            HexToRSABlock ( StrPas (InputBuffer), InputBlock, oSize*8 );
                            NULL_RSA_BLOCK ( OutputBlock, oSize*8 );

                            mp_pkcs1v15_decrypt ( d, n,
                                                  InputBlock, OutputBlock,
                                                  (BlockSize div 2), oSize, L );

                            OutputBuffer := PChar ( RSABlockToHex (OutputBlock,oSize*8,L) );
                            anOutput.WriteBuffer (OutputBuffer^,L+1);
                        finally
                            FillChar (OutputBuffer^,L+1,$00);
                            L := BlockSize;
                        end;
                    end;
                finally
                    ZeroMemory (InputBuffer,L);
                    Dispose (InputBuffer);
                    L := 0;
                    NULL_RSA_BLOCK ( InputBlock, oSize*8 );
                    NULL_RSA_BLOCK ( OutputBlock, oSize*8 );
                end;
                
            finally
                Dispose (InputBlock);
                InputBlock := NIL;
                Dispose (OutputBlock);
                OutputBlock := NIL;
            end;

        finally
            _FillChar ( prv_n, Length (prv_n), $00 );
            _FillChar ( prv_d, Length (prv_d), $00 );
            nLength := 0;
            dLength := 0;
            mp_clear2 (n,d);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_RSA_DECRYPT,[RSA_VERSION]), E.Message ]);
    end;
end;

procedure DecryptRSA (const aPrivateKey: String;
                      const anInput: TStream;
                      out anOutput: TStream);
var
    Hex : TStream;
begin
    try
        Hex := TMemoryStream.Create;
        try
            _DecryptRSA (aPrivateKey,anInput,Hex);
            HexStreamToStream (Hex,anOutput);
        finally
            //ZeroMemory ( TMemoryStream (Hex).Memory, Hex.Size );
            FreeAndNil (Hex);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_RSA_DECRYPT,[RSA_VERSION]), E.Message ]);
    end;
end;

function DecryptRSA (const aPrivateKey: String;
                     const aValue: String) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create (aValue);
        Output := TStringStream.Create ('');
        try
            DecryptRSA (aPrivateKey,Input,Output);
            with TStringStream (Output) do
            begin
                Position := 0;
                Result := ReadString (Size);
                Result := _Trim (Result,$00);
            end;
        finally
            FreeAndNil (Output);
            FreeAndNil (Input);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_RSA_DECRYPT,[RSA_VERSION]), E.Message ]);
    end;
end;

function RSA_VERSION : String;
begin
    Result := Format ('v.%s',[MP_VERSION]);
end;