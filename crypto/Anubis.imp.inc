{******************************************************************************}
{*  Anubis.imp.inc                                                            *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
function AnubisIV (anArgs: array of const) : TAnubis128Block;
var
    I : Integer;
begin
    Result := NULL_ANUBIS_128_BLOCK;
    if (  ( High (anArgs) = 0 ) and IsHex ( toString (anArgs [0]) )  ) then
    begin
        Result := HexToAnubisIV ( toString (anArgs [0]) );
        Exit;
    end;
    for I := 0 to High (anArgs) do
    begin
        Result [I] := toInteger (anArgs [I]);
        if ( I >= High (TAnubis128Block) ) then
            Break;
    end;
end;

function AnubisIVToHex (const aValue: TAnubis128Block) : Hex;
var
    I : Integer;
begin
    Result := '';
    for I := Low (TAnubis128Block) to High (TAnubis128Block) do
        Result := Result + IntToHex (aValue [I],2);
end;

function HexToAnubisIV (const aValue: Hex) : TAnubis128Block;
var
    I      : Integer;
    J      : Integer;
    Buffer : String [3];
begin
    Result := NULL_ANUBIS_128_BLOCK;
    if not IsHex (aValue) then
        Exit;
    I := 0;
    J := 0;
    while ( I < Length (aValue) ) do
    begin
        Inc (I);
        if ( I mod 2 = 0 ) then
        begin
            if ( J > High (TAnubis128Block) )  then
                Break;
            Buffer := Format ('$%s%s',[ aValue [I-1], aValue [I] ]);
            Result [J] := StrToInt (Buffer);
            Inc (J);
        end;
    end;
end;

function GetRandomAnubisIV (aRandomFunc: TRandomFunction) : TAnubis128Block;
var
    I : Integer;
begin
    for I := Low (TAnubis128Block) to High (TAnubis128Block) do
        Result [I] := aRandomFunc (255);
end;

{$IFDEF ECB}
procedure EncryptAnubisECB (var aKey;
                            const aKeySize: WORD;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    ctx          : TAnubisCtx;
    ErrorCode    : Integer;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { инициализируем контекст }
        ErrorCode := ANU_ECB_Init_Encr (aKey,aKeySize,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { шифрование }
            L := aBlockSize;
            InputBuffer := AllocMem (L+1);
            OutputBuffer := AllocMem (L+1);
            while ( Position < Size ) do
            begin
                if ( (Size - Position) < aBlockSize ) then
                begin
                    { очищаем буфер для чтения
                      в случае, если он не будет перезаписан полностью }
                    ZeroMemory (InputBuffer,L);
                    Dispose (InputBuffer);
                    InputBuffer := AllocMem (L+1);
                end;
                if ( Read (InputBuffer^,L) > 0 ) then
                begin
                    ErrorCode := ANU_ECB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
                    if ( ErrorCode <> 0 ) then
                        raise Exception.CreateFmt ('%d',[ErrorCode]);
                    anOutput.Write (OutputBuffer^,L);
                end;
            end;
        finally
            ZeroMemory (InputBuffer,L);
            Dispose (InputBuffer);
            ZeroMemory (OutputBuffer,L);
            Dispose (OutputBuffer);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_ECB_ENCRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

procedure DecryptAnubisECB (var aKey;
                            const aKeySize: WORD;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    ctx          : TAnubisCtx;
    ErrorCode    : Integer;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { инициализируем контекст }
        ErrorCode := ANU_ECB_Init_Decr (aKey,aKeySize,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { расшифровка }
            L := aBlockSize;
            InputBuffer := AllocMem (L+1);
            OutputBuffer := AllocMem (L+1);
            while ( Position < Size ) do
            begin
                if ( (Size - Position) < aBlockSize ) then
                begin
                    { очищаем буфер для чтения
                      в случае, если он не будет перезаписан полностью }
                    ZeroMemory (InputBuffer,L);
                    Dispose (InputBuffer);
                    InputBuffer := AllocMem (L+1);
                end;
                if ( Read (InputBuffer^,L) > 0 ) then
                begin
                    ErrorCode := ANU_ECB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
                    if ( ErrorCode <> 0 ) then
                        raise Exception.CreateFmt ('%d',[ErrorCode]);
                    anOutput.WriteBuffer (OutputBuffer^,L);
                end;
            end;
        finally
            ZeroMemory (InputBuffer,L);
            Dispose (InputBuffer);
            ZeroMemory (OutputBuffer,L);
            Dispose (OutputBuffer);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_ECB_DECRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

function EncryptAnubisECB (var aKey;
                           const aKeySize: WORD;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create (aValue);
        Output := TStringStream.Create ('');
        try
            EncryptAnubisECB (aKey,aKeySize,Input,Output,aBlockSize);
            with TStringStream (Output) do
            begin
                Position := 0;
                Result := ReadString (Size);
                Result := StrToHex (Result);
                //Result := _Trim (Result,$00);
            end;
        finally
            FreeAndNil (Output);
            FreeAndNil (Input);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_ECB_ENCRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

function DecryptAnubisECB (var aKey;
                           const aKeySize: WORD;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create ( HexToStr (aValue) );
        Output := TStringStream.Create ('');
        try
            DecryptAnubisECB (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_ANUBIS_ECB_DECRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

procedure EncryptAnubisECB (const aKey: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        EncryptAnubisECB (Key^,
                          oSize*8,
                          anInput,
                          anOutput,
                          aBlockSize);
    finally
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

procedure DecryptAnubisECB (const aKey: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        DecryptAnubisECB (Key^,
                          oSize*8,
                          anInput,
                          anOutput,
                          aBlockSize);
    finally
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function EncryptAnubisECB (const aKey: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        Result := EncryptAnubisECB (Key^,
                                    oSize*8,
                                    aValue,
                                    aBlockSize);
    finally
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function DecryptAnubisECB (const aKey: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        Result := DecryptAnubisECB (Key^,
                                    oSize*8,
                                    aValue,
                                    aBlockSize);
    finally
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;
{$ENDIF ECB}

{$IFDEF CBC}
procedure EncryptAnubisCBC (var aKey;
                            const aKeySize: WORD;
                            aRandomFunc: TRandomFunction;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    ctx          : TAnubisCtx;
    IV           : TAnubis128Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { формируем псевдо-случайный 128-битный вектор инициализации }
        IV := GetRandomAnubisIV (aRandomFunc);
        { инициализируем контекст }
        ErrorCode := ANU_CBC_Init_Encr (aKey,aKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { записываем вектор инициализации в начало }
            Buffer := PChar ( AnubisIVToHex (IV) );
            anOutput.Write (Buffer^,32);
            { шифрование }
            L := aBlockSize;
            InputBuffer := AllocMem (L+1);
            OutputBuffer := AllocMem (L+1);
            while ( Position < Size ) do
            begin
                if ( (Size - Position) < aBlockSize ) then
                begin
                    { очищаем буфер для чтения
                      в случае, если он не будет перезаписан полностью }
                    ZeroMemory (InputBuffer,L);
                    Dispose (InputBuffer);
                    InputBuffer := AllocMem (L+1);
                end;
                if ( Read (InputBuffer^,L) > 0 ) then
                begin
                    ErrorCode := ANU_CBC_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
                    if ( ErrorCode <> 0 ) then
                        raise Exception.CreateFmt ('%d',[ErrorCode]);
                    anOutput.Write (OutputBuffer^,L);
                end;
            end;
        finally
            ZeroMemory (InputBuffer,L);
            Dispose (InputBuffer);
            ZeroMemory (OutputBuffer,L);
            Dispose (OutputBuffer);

            IV := NULL_ANUBIS_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_CBC_ENCRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

procedure EncryptAnubisCBC (var aKey;
                            const aKeySize: WORD;
                            const anIV: TAnubis128Block;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    ctx          : TAnubisCtx;
    IV           : TAnubis128Block;
    ErrorCode    : Integer;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { используем переданный вектор инициализации }
        IV := anIV;
        { инициализируем контекст }
        ErrorCode := ANU_CBC_Init_Encr (aKey,aKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { шифрование }
            L := aBlockSize;
            InputBuffer := AllocMem (L+1);
            OutputBuffer := AllocMem (L+1);
            while ( Position < Size ) do
            begin
                if ( (Size - Position) < aBlockSize ) then
                begin
                    { очищаем буфер для чтения
                      в случае, если он не будет перезаписан полностью }
                    ZeroMemory (InputBuffer,L);
                    Dispose (InputBuffer);
                    InputBuffer := AllocMem (L+1);
                end;
                if ( Read (InputBuffer^,L) > 0 ) then
                begin
                    ErrorCode := ANU_CBC_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
                    if ( ErrorCode <> 0 ) then
                        raise Exception.CreateFmt ('%d',[ErrorCode]);
                    anOutput.Write (OutputBuffer^,L);
                end;
            end;
        finally
            ZeroMemory (InputBuffer,L);
            Dispose (InputBuffer);
            ZeroMemory (OutputBuffer,L);
            Dispose (OutputBuffer);

            IV := NULL_ANUBIS_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_CBC_ENCRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

procedure DecryptAnubisCBC (var aKey;
                            const aKeySize: WORD;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    ctx          : TAnubisCtx;
    IV           : TAnubis128Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { обнуляем вектор инициализации }
        IV := NULL_ANUBIS_128_BLOCK;
        { инициализируем контекст }
        ErrorCode := ANU_CBC_Init_Decr (aKey,aKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { читаем вектор инициализации и обновляем контекст }
            try
                Buffer := AllocMem (32+1);
                if ( Read (Buffer^,32) > 0 ) then
                begin
                    IV := HexToAnubisIV ( StrPas (Buffer) );
                    ErrorCode := ANU_CBC_Init_Decr (aKey,aKeySize,IV,ctx);
                    if ( ErrorCode <> 0 ) then
                        raise Exception.CreateFmt ('%d',[ErrorCode]);
                end;
            finally
                Dispose (Buffer);
                Buffer := NIL;
            end;
            { расшифровка }
            L := aBlockSize;
            InputBuffer := AllocMem (L+1);
            OutputBuffer := AllocMem (L+1);
            while ( Position < Size ) do
            begin
                if ( (Size - Position) < aBlockSize ) then
                begin
                    { очищаем буфер для чтения
                      в случае, если он не будет перезаписан полностью }
                    ZeroMemory (InputBuffer,L);
                    Dispose (InputBuffer);
                    InputBuffer := AllocMem (L+1);
                end;
                if ( Read (InputBuffer^,L) > 0 ) then
                begin
                    ErrorCode := ANU_CBC_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
                    if ( ErrorCode <> 0 ) then
                        raise Exception.CreateFmt ('%d',[ErrorCode]);
                    anOutput.WriteBuffer (OutputBuffer^,L);
                end;
            end;
        finally
            ZeroMemory (InputBuffer,L);
            Dispose (InputBuffer);
            ZeroMemory (OutputBuffer,L);
            Dispose (OutputBuffer);

            IV := NULL_ANUBIS_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_CBC_DECRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

procedure DecryptAnubisCBC (var aKey;
                            const aKeySize: WORD;
                            const anIV: TAnubis128Block;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    ctx          : TAnubisCtx;
    IV           : TAnubis128Block;
    ErrorCode    : Integer;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { используем переданный вектор инициализации }
        IV := anIV;
        { инициализируем контекст }
        ErrorCode := ANU_CBC_Init_Decr (aKey,aKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { расшифровка }
            L := aBlockSize;
            InputBuffer := AllocMem (L+1);
            OutputBuffer := AllocMem (L+1);
            while ( Position < Size ) do
            begin
                if ( (Size - Position) < aBlockSize ) then
                begin
                    { очищаем буфер для чтения
                      в случае, если он не будет перезаписан полностью }
                    ZeroMemory (InputBuffer,L);
                    Dispose (InputBuffer);
                    InputBuffer := AllocMem (L+1);
                end;
                if ( Read (InputBuffer^,L) > 0 ) then
                begin
                    ErrorCode := ANU_CBC_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
                    if ( ErrorCode <> 0 ) then
                        raise Exception.CreateFmt ('%d',[ErrorCode]);
                    anOutput.WriteBuffer (OutputBuffer^,L);
                end;
            end;
        finally
            ZeroMemory (InputBuffer,L);
            Dispose (InputBuffer);
            ZeroMemory (OutputBuffer,L);
            Dispose (OutputBuffer);

            IV := NULL_ANUBIS_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_CBC_DECRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

function EncryptAnubisCBC (var aKey;
                           const aKeySize: WORD;
                           aRandomFunc: TRandomFunction;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create (aValue);
        Output := TStringStream.Create ('');
        try
            EncryptAnubisCBC (aKey,aKeySize,aRandomFunc,Input,Output,aBlockSize);
            with TStringStream (Output) do
            begin
                Position := 0;
                Result := ReadString (Size);
                Result := StrToHex (Result);
                //Result := _Trim (Result,$00);
            end;
        finally
            FreeAndNil (Output);
            FreeAndNil (Input);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_CBC_ENCRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

function EncryptAnubisCBC (var aKey;
                           const aKeySize: WORD;
                           const anIV: TAnubis128Block;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create (aValue);
        Output := TStringStream.Create ('');
        try
            EncryptAnubisCBC (aKey,aKeySize,anIV,Input,Output,aBlockSize);
            with TStringStream (Output) do
            begin
                Position := 0;
                Result := ReadString (Size);
                Result := StrToHex (Result);
                //Result := _Trim (Result,$00);
            end;
        finally
            FreeAndNil (Output);
            FreeAndNil (Input);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_CBC_ENCRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

function DecryptAnubisCBC (var aKey;
                           const aKeySize: WORD;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create ( HexToStr (aValue) );
        Output := TStringStream.Create ('');
        try
            DecryptAnubisCBC (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_ANUBIS_CBC_DECRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

function DecryptAnubisCBC (var aKey;
                           const aKeySize: WORD;
                           const anIV: TAnubis128Block;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create ( HexToStr (aValue) );
        Output := TStringStream.Create ('');
        try
            DecryptAnubisCBC (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_ANUBIS_CBC_DECRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

procedure EncryptAnubisCBC (const aKey: Hex;
                            aRandomFunc: TRandomFunction;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        EncryptAnubisCBC (Key^,
                          oSize*8,
                          aRandomFunc,
                          anInput,
                          anOutput,
                          aBlockSize);
    finally
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

procedure EncryptAnubisCBC (const aKey: Hex;
                            const anIV: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TAnubis128Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToAnubisIV (anIV);
        EncryptAnubisCBC (Key^,
                          oSize*8,
                          IV,
                          anInput,
                          anOutput,
                          aBlockSize);
    finally
        IV := NULL_ANUBIS_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

procedure DecryptAnubisCBC (const aKey: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        DecryptAnubisCBC (Key^,
                          oSize*8,
                          anInput,
                          anOutput,
                          aBlockSize);
    finally
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

procedure DecryptAnubisCBC (const aKey: Hex;
                            const anIV: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TAnubis128Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToAnubisIV (anIV);
        DecryptAnubisCBC (Key^,
                          oSize*8,
                          IV,
                          anInput,
                          anOutput,
                          aBlockSize);
    finally
        IV := NULL_ANUBIS_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function EncryptAnubisCBC (const aKey: Hex;
                           aRandomFunc: TRandomFunction;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        Result := EncryptAnubisCBC (Key^,
                                    oSize*8,
                                    aRandomFunc,
                                    aValue,
                                    aBlockSize);
    finally
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function EncryptAnubisCBC (const aKey: Hex;
                           const anIV: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TAnubis128Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToAnubisIV (anIV);
        Result := EncryptAnubisCBC (Key^,
                                    oSize*8,
                                    IV,
                                    aValue,
                                    aBlockSize);
    finally
        IV := NULL_ANUBIS_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function DecryptAnubisCBC (const aKey: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        Result := DecryptAnubisCBC (Key^,
                                    oSize*8,
                                    aValue,
                                    aBlockSize);
    finally
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function DecryptAnubisCBC (const aKey: Hex;
                           const anIV: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TAnubis128Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToAnubisIV (anIV);
        Result := DecryptAnubisCBC (Key^,
                                    oSize*8,
                                    IV,
                                    aValue,
                                    aBlockSize);
    finally
        IV := NULL_ANUBIS_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;
{$ENDIF CBC}

{$IFDEF CFB}
procedure EncryptAnubisCFB (var aKey;
                            const aKeySize: WORD;
                            aRandomFunc: TRandomFunction;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    ctx          : TAnubisCtx;
    IV           : TAnubis128Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { формируем псевдо-случайный 128-битный вектор инициализации }
        IV := GetRandomAnubisIV (aRandomFunc);
        { инициализируем контекст }
        ErrorCode := ANU_CFB_Init (aKey,aKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { записываем вектор инициализации в начало }
            Buffer := PChar ( AnubisIVToHex (IV) );
            anOutput.Write (Buffer^,32);
            { шифрование }
            L := aBlockSize;
            InputBuffer := AllocMem (L+1);
            OutputBuffer := AllocMem (L+1);
            while ( Position < Size ) do
            begin
                if ( (Size - Position) < aBlockSize ) then
                begin
                    { очищаем буфер для чтения
                      в случае, если он не будет перезаписан полностью }
                    ZeroMemory (InputBuffer,L);
                    Dispose (InputBuffer);
                    InputBuffer := AllocMem (L+1);
                end;
                if ( Read (InputBuffer^,L) > 0 ) then
                begin
                    ErrorCode := ANU_CFB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
                    if ( ErrorCode <> 0 ) then
                        raise Exception.CreateFmt ('%d',[ErrorCode]);
                    anOutput.Write (OutputBuffer^,L);
                end;
            end;
        finally
            ZeroMemory (InputBuffer,L);
            Dispose (InputBuffer);
            ZeroMemory (OutputBuffer,L);
            Dispose (OutputBuffer);

            IV := NULL_ANUBIS_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_CFB_ENCRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

procedure EncryptAnubisCFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TAnubis128Block;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    ctx          : TAnubisCtx;
    IV           : TAnubis128Block;
    ErrorCode    : Integer;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { используем переданный вектор инициализации }
        IV := anIV;
        { инициализируем контекст }
        ErrorCode := ANU_CFB_Init (aKey,aKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { шифрование }
            L := aBlockSize;
            InputBuffer := AllocMem (L+1);
            OutputBuffer := AllocMem (L+1);
            while ( Position < Size ) do
            begin
                if ( (Size - Position) < aBlockSize ) then
                begin
                    { очищаем буфер для чтения
                      в случае, если он не будет перезаписан полностью }
                    ZeroMemory (InputBuffer,L);
                    Dispose (InputBuffer);
                    InputBuffer := AllocMem (L+1);
                end;
                if ( Read (InputBuffer^,L) > 0 ) then
                begin
                    ErrorCode := ANU_CFB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
                    if ( ErrorCode <> 0 ) then
                        raise Exception.CreateFmt ('%d',[ErrorCode]);
                    anOutput.Write (OutputBuffer^,L);
                end;
            end;
        finally
            ZeroMemory (InputBuffer,L);
            Dispose (InputBuffer);
            ZeroMemory (OutputBuffer,L);
            Dispose (OutputBuffer);

            IV := NULL_ANUBIS_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_CFB_ENCRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

procedure DecryptAnubisCFB (var aKey;
                            const aKeySize: WORD;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    ctx          : TAnubisCtx;
    IV           : TAnubis128Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { обнуляем вектор инициализации }
        IV := NULL_ANUBIS_128_BLOCK;
        { инициализируем контекст }
        ErrorCode := ANU_CFB_Init (aKey,aKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { читаем вектор инициализации и обновляем контекст }
            try
                Buffer := AllocMem (32+1);
                if ( Read (Buffer^,32) > 0 ) then
                begin
                    IV := HexToAnubisIV ( StrPas (Buffer) );
                    ErrorCode := ANU_CFB_Init (aKey,aKeySize,IV,ctx);
                    if ( ErrorCode <> 0 ) then
                        raise Exception.CreateFmt ('%d',[ErrorCode]);
                end;
            finally
                Dispose (Buffer);
                Buffer := NIL;
            end;
            { расшифровка }
            L := aBlockSize;
            InputBuffer := AllocMem (L+1);
            OutputBuffer := AllocMem (L+1);
            while ( Position < Size ) do
            begin
                if ( (Size - Position) < aBlockSize ) then
                begin
                    { очищаем буфер для чтения
                      в случае, если он не будет перезаписан полностью }
                    ZeroMemory (InputBuffer,L);
                    Dispose (InputBuffer);
                    InputBuffer := AllocMem (L+1);
                end;
                if ( Read (InputBuffer^,L) > 0 ) then
                begin
                    ErrorCode := ANU_CFB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
                    if ( ErrorCode <> 0 ) then
                        raise Exception.CreateFmt ('%d',[ErrorCode]);
                    anOutput.WriteBuffer (OutputBuffer^,L);
                end;
            end;
        finally
            ZeroMemory (InputBuffer,L);
            Dispose (InputBuffer);
            ZeroMemory (OutputBuffer,L);
            Dispose (OutputBuffer);

            IV := NULL_ANUBIS_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_CFB_DECRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

procedure DecryptAnubisCFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TAnubis128Block;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    ctx          : TAnubisCtx;
    IV           : TAnubis128Block;
    ErrorCode    : Integer;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { используем переданный вектор инициализации }
        IV := anIV;
        { инициализируем контекст }
        ErrorCode := ANU_CFB_Init (aKey,aKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { расшифровка }
            L := aBlockSize;
            InputBuffer := AllocMem (L+1);
            OutputBuffer := AllocMem (L+1);
            while ( Position < Size ) do
            begin
                if ( (Size - Position) < aBlockSize ) then
                begin
                    { очищаем буфер для чтения
                      в случае, если он не будет перезаписан полностью }
                    ZeroMemory (InputBuffer,L);
                    Dispose (InputBuffer);
                    InputBuffer := AllocMem (L+1);
                end;
                if ( Read (InputBuffer^,L) > 0 ) then
                begin
                    ErrorCode := ANU_CFB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
                    if ( ErrorCode <> 0 ) then
                        raise Exception.CreateFmt ('%d',[ErrorCode]);
                    anOutput.WriteBuffer (OutputBuffer^,L);
                end;
            end;
        finally
            ZeroMemory (InputBuffer,L);
            Dispose (InputBuffer);
            ZeroMemory (OutputBuffer,L);
            Dispose (OutputBuffer);

            IV := NULL_ANUBIS_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_CFB_DECRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

function EncryptAnubisCFB (var aKey;
                           const aKeySize: WORD;
                           aRandomFunc: TRandomFunction;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create (aValue);
        Output := TStringStream.Create ('');
        try
            EncryptAnubisCFB (aKey,aKeySize,aRandomFunc,Input,Output,aBlockSize);
            with TStringStream (Output) do
            begin
                Position := 0;
                Result := ReadString (Size);
                Result := StrToHex (Result);
                //Result := _Trim (Result,$00);
            end;
        finally
            FreeAndNil (Output);
            FreeAndNil (Input);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_CFB_ENCRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

function EncryptAnubisCFB (var aKey;
                           const aKeySize: WORD;
                           const anIV: TAnubis128Block;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create (aValue);
        Output := TStringStream.Create ('');
        try
            EncryptAnubisCFB (aKey,aKeySize,anIV,Input,Output,aBlockSize);
            with TStringStream (Output) do
            begin
                Position := 0;
                Result := ReadString (Size);
                Result := StrToHex (Result);
                //Result := _Trim (Result,$00);
            end;
        finally
            FreeAndNil (Output);
            FreeAndNil (Input);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_CFB_ENCRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

function DecryptAnubisCFB (var aKey;
                           const aKeySize: WORD;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create ( HexToStr (aValue) );
        Output := TStringStream.Create ('');
        try
            DecryptAnubisCFB (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_ANUBIS_CFB_DECRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

function DecryptAnubisCFB (var aKey;
                           const aKeySize: WORD;
                           const anIV: TAnubis128Block;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create ( HexToStr (aValue) );
        Output := TStringStream.Create ('');
        try
            DecryptAnubisCFB (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_ANUBIS_CFB_DECRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

procedure EncryptAnubisCFB (const aKey: Hex;
                            aRandomFunc: TRandomFunction;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        EncryptAnubisCFB (Key^,
                          oSize*8,
                          aRandomFunc,
                          anInput,
                          anOutput,
                          aBlockSize);
    finally
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

procedure EncryptAnubisCFB (const aKey: Hex;
                            const anIV: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TAnubis128Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToAnubisIV (anIV);
        EncryptAnubisCFB (Key^,
                          oSize*8,
                          IV,
                          anInput,
                          anOutput,
                          aBlockSize);
    finally
        IV := NULL_ANUBIS_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

procedure DecryptAnubisCFB (const aKey: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        DecryptAnubisCFB (Key^,
                          oSize*8,
                          anInput,
                          anOutput,
                          aBlockSize);
    finally
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

procedure DecryptAnubisCFB (const aKey: Hex;
                            const anIV: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TAnubis128Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToAnubisIV (anIV);
        DecryptAnubisCFB (Key^,
                          oSize*8,
                          IV,
                          anInput,
                          anOutput,
                          aBlockSize);
    finally
        IV := NULL_ANUBIS_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function EncryptAnubisCFB (const aKey: Hex;
                           aRandomFunc: TRandomFunction;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        Result := EncryptAnubisCFB (Key^,
                                    oSize*8,
                                    aRandomFunc,
                                    aValue,
                                    aBlockSize);
    finally
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function EncryptAnubisCFB (const aKey: Hex;
                           const anIV: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TAnubis128Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToAnubisIV (anIV);
        Result := EncryptAnubisCFB (Key^,
                                    oSize*8,
                                    IV,
                                    aValue,
                                    aBlockSize);
    finally
        IV := NULL_ANUBIS_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function DecryptAnubisCFB (const aKey: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        Result := DecryptAnubisCFB (Key^,
                                    oSize*8,
                                    aValue,
                                    aBlockSize);
    finally
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function DecryptAnubisCFB (const aKey: Hex;
                           const anIV: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TAnubis128Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToAnubisIV (anIV);
        Result := DecryptAnubisCFB (Key^,
                                    oSize*8,
                                    IV,
                                    aValue,
                                    aBlockSize);
    finally
        IV := NULL_ANUBIS_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;
{$ENDIF CFB}

{$IFDEF OFB}
procedure EncryptAnubisOFB (var aKey;
                            const aKeySize: WORD;
                            aRandomFunc: TRandomFunction;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    ctx          : TAnubisCtx;
    IV           : TAnubis128Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { формируем псевдо-случайный 128-битный вектор инициализации }
        IV := GetRandomAnubisIV (aRandomFunc);
        { инициализируем контекст }
        ErrorCode := ANU_OFB_Init (aKey,aKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { записываем вектор инициализации в начало }
            Buffer := PChar ( AnubisIVToHex (IV) );
            anOutput.Write (Buffer^,32);
            { шифрование }
            L := aBlockSize;
            InputBuffer := AllocMem (L+1);
            OutputBuffer := AllocMem (L+1);
            while ( Position < Size ) do
            begin
                if ( (Size - Position) < aBlockSize ) then
                begin
                    { очищаем буфер для чтения
                      в случае, если он не будет перезаписан полностью }
                    ZeroMemory (InputBuffer,L);
                    Dispose (InputBuffer);
                    InputBuffer := AllocMem (L+1);
                end;
                if ( Read (InputBuffer^,L) > 0 ) then
                begin
                    ErrorCode := ANU_OFB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
                    if ( ErrorCode <> 0 ) then
                        raise Exception.CreateFmt ('%d',[ErrorCode]);
                    anOutput.Write (OutputBuffer^,L);
                end;
            end;
        finally
            ZeroMemory (InputBuffer,L);
            Dispose (InputBuffer);
            ZeroMemory (OutputBuffer,L);
            Dispose (OutputBuffer);

            IV := NULL_ANUBIS_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_OFB_ENCRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

procedure EncryptAnubisOFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TAnubis128Block;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    ctx          : TAnubisCtx;
    IV           : TAnubis128Block;
    ErrorCode    : Integer;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { используем переданный вектор инициализации }
        IV := anIV;
        { инициализируем контекст }
        ErrorCode := ANU_OFB_Init (aKey,aKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { шифрование }
            L := aBlockSize;
            InputBuffer := AllocMem (L+1);
            OutputBuffer := AllocMem (L+1);
            while ( Position < Size ) do
            begin
                if ( (Size - Position) < aBlockSize ) then
                begin
                    { очищаем буфер для чтения
                      в случае, если он не будет перезаписан полностью }
                    ZeroMemory (InputBuffer,L);
                    Dispose (InputBuffer);
                    InputBuffer := AllocMem (L+1);
                end;
                if ( Read (InputBuffer^,L) > 0 ) then
                begin
                    ErrorCode := ANU_OFB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
                    if ( ErrorCode <> 0 ) then
                        raise Exception.CreateFmt ('%d',[ErrorCode]);
                    anOutput.Write (OutputBuffer^,L);
                end;
            end;
        finally
            ZeroMemory (InputBuffer,L);
            Dispose (InputBuffer);
            ZeroMemory (OutputBuffer,L);
            Dispose (OutputBuffer);

            IV := NULL_ANUBIS_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_OFB_ENCRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

procedure DecryptAnubisOFB (var aKey;
                            const aKeySize: WORD;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    ctx          : TAnubisCtx;
    IV           : TAnubis128Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { обнуляем вектор инициализации }
        IV := NULL_ANUBIS_128_BLOCK;
        { инициализируем контекст }
        ErrorCode := ANU_OFB_Init (aKey,aKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { читаем вектор инициализации и обновляем контекст }
            try
                Buffer := AllocMem (32+1);
                if ( Read (Buffer^,32) > 0 ) then
                begin
                    IV := HexToAnubisIV ( StrPas (Buffer) );
                    ErrorCode := ANU_OFB_Init (aKey,aKeySize,IV,ctx);
                    if ( ErrorCode <> 0 ) then
                        raise Exception.CreateFmt ('%d',[ErrorCode]);
                end;
            finally
                Dispose (Buffer);
                Buffer := NIL;
            end;
            { расшифровка }
            L := aBlockSize;
            InputBuffer := AllocMem (L+1);
            OutputBuffer := AllocMem (L+1);
            while ( Position < Size ) do
            begin
                if ( (Size - Position) < aBlockSize ) then
                begin
                    { очищаем буфер для чтения
                      в случае, если он не будет перезаписан полностью }
                    ZeroMemory (InputBuffer,L);
                    Dispose (InputBuffer);
                    InputBuffer := AllocMem (L+1);
                end;
                if ( Read (InputBuffer^,L) > 0 ) then
                begin
                    ErrorCode := ANU_OFB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
                    if ( ErrorCode <> 0 ) then
                        raise Exception.CreateFmt ('%d',[ErrorCode]);
                    anOutput.WriteBuffer (OutputBuffer^,L);
                end;
            end;
        finally
            ZeroMemory (InputBuffer,L);
            Dispose (InputBuffer);
            ZeroMemory (OutputBuffer,L);
            Dispose (OutputBuffer);

            IV := NULL_ANUBIS_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_OFB_DECRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

procedure DecryptAnubisOFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TAnubis128Block;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    ctx          : TAnubisCtx;
    IV           : TAnubis128Block;
    ErrorCode    : Integer;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { используем переданный вектор инициализации }
        IV := anIV;
        { инициализируем контекст }
        ErrorCode := ANU_OFB_Init (aKey,aKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { расшифровка }
            L := aBlockSize;
            InputBuffer := AllocMem (L+1);
            OutputBuffer := AllocMem (L+1);
            while ( Position < Size ) do
            begin
                if ( (Size - Position) < aBlockSize ) then
                begin
                    { очищаем буфер для чтения
                      в случае, если он не будет перезаписан полностью }
                    ZeroMemory (InputBuffer,L);
                    Dispose (InputBuffer);
                    InputBuffer := AllocMem (L+1);
                end;
                if ( Read (InputBuffer^,L) > 0 ) then
                begin
                    ErrorCode := ANU_OFB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
                    if ( ErrorCode <> 0 ) then
                        raise Exception.CreateFmt ('%d',[ErrorCode]);
                    anOutput.WriteBuffer (OutputBuffer^,L);
                end;
            end;
        finally
            ZeroMemory (InputBuffer,L);
            Dispose (InputBuffer);
            ZeroMemory (OutputBuffer,L);
            Dispose (OutputBuffer);

            IV := NULL_ANUBIS_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_OFB_DECRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

function EncryptAnubisOFB (var aKey;
                           const aKeySize: WORD;
                           aRandomFunc: TRandomFunction;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create (aValue);
        Output := TStringStream.Create ('');
        try
            EncryptAnubisOFB (aKey,aKeySize,aRandomFunc,Input,Output,aBlockSize);
            with TStringStream (Output) do
            begin
                Position := 0;
                Result := ReadString (Size);
                Result := StrToHex (Result);
                //Result := _Trim (Result,$00);
            end;
        finally
            FreeAndNil (Output);
            FreeAndNil (Input);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_OFB_ENCRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

function EncryptAnubisOFB (var aKey;
                           const aKeySize: WORD;
                           const anIV: TAnubis128Block;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create (aValue);
        Output := TStringStream.Create ('');
        try
            EncryptAnubisOFB (aKey,aKeySize,anIV,Input,Output,aBlockSize);
            with TStringStream (Output) do
            begin
                Position := 0;
                Result := ReadString (Size);
                Result := StrToHex (Result);
                //Result := _Trim (Result,$00);
            end;
        finally
            FreeAndNil (Output);
            FreeAndNil (Input);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_ANUBIS_OFB_ENCRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

function DecryptAnubisOFB (var aKey;
                           const aKeySize: WORD;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create ( HexToStr (aValue) );
        Output := TStringStream.Create ('');
        try
            DecryptAnubisOFB (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_ANUBIS_OFB_DECRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

function DecryptAnubisOFB (var aKey;
                           const aKeySize: WORD;
                           const anIV: TAnubis128Block;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create ( HexToStr (aValue) );
        Output := TStringStream.Create ('');
        try
            DecryptAnubisOFB (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_ANUBIS_OFB_DECRYPT,[aKeySize,ANUBIS_VERSION]), E.Message ]);
    end;
end;

procedure EncryptAnubisOFB (const aKey: Hex;
                            aRandomFunc: TRandomFunction;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        EncryptAnubisOFB (Key^,
                          oSize*8,
                          aRandomFunc,
                          anInput,
                          anOutput,
                          aBlockSize);
    finally
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

procedure EncryptAnubisOFB (const aKey: Hex;
                            const anIV: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TAnubis128Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToAnubisIV (anIV);
        EncryptAnubisOFB (Key^,
                          oSize*8,
                          IV,
                          anInput,
                          anOutput,
                          aBlockSize);
    finally
        IV := NULL_ANUBIS_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

procedure DecryptAnubisOFB (const aKey: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        DecryptAnubisOFB (Key^,
                          oSize*8,
                          anInput,
                          anOutput,
                          aBlockSize);
    finally
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

procedure DecryptAnubisOFB (const aKey: Hex;
                            const anIV: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TAnubis128Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToAnubisIV (anIV);
        DecryptAnubisOFB (Key^,
                          oSize*8,
                          IV,
                          anInput,
                          anOutput,
                          aBlockSize);
    finally
        IV := NULL_ANUBIS_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function EncryptAnubisOFB (const aKey: Hex;
                           aRandomFunc: TRandomFunction;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        Result := EncryptAnubisOFB (Key^,
                                    oSize*8,
                                    aRandomFunc,
                                    aValue,
                                    aBlockSize);
    finally
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function EncryptAnubisOFB (const aKey: Hex;
                           const anIV: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TAnubis128Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToAnubisIV (anIV);
        Result := EncryptAnubisOFB (Key^,
                                    oSize*8,
                                    IV,
                                    aValue,
                                    aBlockSize);
    finally
        IV := NULL_ANUBIS_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function DecryptAnubisOFB (const aKey: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        Result := DecryptAnubisOFB (Key^,
                                    oSize*8,
                                    aValue,
                                    aBlockSize);
    finally
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function DecryptAnubisOFB (const aKey: Hex;
                           const anIV: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TAnubis128Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToAnubisIV (anIV);
        Result := DecryptAnubisOFB (Key^,
                                    oSize*8,
                                    IV,
                                    aValue,
                                    aBlockSize);
    finally
        IV := NULL_ANUBIS_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;
{$ENDIF OFB}

function ANUBIS_VERSION : String;
var
    Version : TVersionInfo;
begin
    {$IFDEF EXTERNAL_LINK}
    Version := GetFileVersionInfo (ANUBIS_DLL);
    // Result := Format ('v.%s',[GET_ANUBIS_DLL_VERSION]);
    {$ELSE EXTERNAL_LINK}
    Version := StrToVersionInfo ('0.13.0.0');
    {$ENDIF EXTERNAL_LINK}
    Result := Format ('v.%d.%d.%d.%d',[Version.Major,
                                       Version.Minor,
                                       Version.Release,
                                       Version.Build]);
end;
