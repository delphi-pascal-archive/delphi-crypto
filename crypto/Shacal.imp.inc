{******************************************************************************}
{*  Shacal.imp.inc                                                            *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
function ShacalIV (anArgs: array of const) : TShacal256Block;
var
    I : Integer;
begin
    Result := NULL_SHACAL_256_BLOCK;
    if (  ( High (anArgs) = 0 ) and IsHex ( toString (anArgs [0]) )  ) then
    begin
        Result := HexToShacalIV ( toString (anArgs [0]) );
        Exit;
    end;
    for I := 0 to High (anArgs) do
    begin
        Result [I] := toInteger (anArgs [I]);
        if ( I >= High (TShacal256Block) ) then
            Break;
    end;
end;

function ShacalIVToHex (const aValue: TShacal256Block) : Hex;
var
    I : Integer;
begin
    Result := '';
    for I := Low (TShacal256Block) to High (TShacal256Block) do
        Result := Result + IntToHex (aValue [I],2);
end;

function HexToShacalIV (const aValue: Hex) : TShacal256Block;
var
    I      : Integer;
    J      : Integer;
    Buffer : String [3];
begin
    Result := NULL_SHACAL_256_BLOCK;
    if not IsHex (aValue) then
        Exit;
    I := 0;
    J := 0;
    while ( I < Length (aValue) ) do
    begin
        Inc (I);
        if ( I mod 2 = 0 ) then
        begin
            if ( J > High (TShacal256Block) )  then
                Break;
            Buffer := Format ('$%s%s',[ aValue [I-1], aValue [I] ]);
            Result [J] := StrToInt (Buffer);
            Inc (J);
        end;
    end;
end;

function GetRandomShacalIV (aRandomFunc: TRandomFunction) : TShacal256Block;
var
    I : Integer;
begin
    for I := Low (TShacal256Block) to High (TShacal256Block) do
        Result [I] := aRandomFunc (255);
end;

{$IFDEF ECB}
procedure EncryptShacalECB (var aKey;
                            const aKeySize: WORD;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    ctx          : TShacalCtx;
    ErrorCode    : Integer;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
    oKeySize     : WORD;
begin
    ErrorCode := 0;
    try
        oKeySize := aKeySize div 8;
        { инициализируем контекст }
        ErrorCode := SC_ECB_Init (aKey,oKeySize,ctx);
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
                    ErrorCode := SC_ECB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            SC_ECB_Reset (ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SHACAL_ECB_ENCRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

procedure DecryptShacalECB (var aKey;
                            const aKeySize: WORD;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    ctx          : TShacalCtx;
    ErrorCode    : Integer;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
    oKeySize     : WORD;
begin
    ErrorCode := 0;
    try
        oKeySize := aKeySize div 8;
        { инициализируем контекст }
        ErrorCode := SC_ECB_Init (aKey,oKeySize,ctx);
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
                    ErrorCode := SC_ECB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            SC_ECB_Reset (ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SHACAL_ECB_DECRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

function EncryptShacalECB (var aKey;
                           const aKeySize: WORD;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create (aValue);
        Output := TStringStream.Create ('');
        try
            EncryptShacalECB (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SHACAL_ECB_ENCRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

function DecryptShacalECB (var aKey;
                           const aKeySize: WORD;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create ( HexToStr (aValue) );
        Output := TStringStream.Create ('');
        try
            DecryptShacalECB (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SHACAL_ECB_DECRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

procedure EncryptShacalECB (const aKey: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    Key   : PKey;
    oSize : WORD;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        EncryptShacalECB (Key^,
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

procedure DecryptShacalECB (const aKey: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    Key   : PKey;
    oSize : WORD;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        DecryptShacalECB (Key^,
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

function EncryptShacalECB (const aKey: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Key   : PKey;
    oSize : WORD;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        Result := EncryptShacalECB (Key^,
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

function DecryptShacalECB (const aKey: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Key   : PKey;
    oSize : WORD;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        Result := DecryptShacalECB (Key^,
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
procedure EncryptShacalCBC (var aKey;
                            const aKeySize: WORD;
                            aRandomFunc: TRandomFunction;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    ctx          : TShacalCtx;
    IV           : TShacal256Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
    oKeySize     : WORD;
begin
    ErrorCode := 0;
    try
        oKeySize := aKeySize div 8;
        { формируем псевдо-случайный 256-битный вектор инициализации }
        IV := GetRandomShacalIV (aRandomFunc);
        { инициализируем контекст }
        ErrorCode := SC_CBC_Init (aKey,oKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { записываем вектор инициализации в начало }
            Buffer := PChar ( ShacalIVToHex (IV) );
            anOutput.Write (Buffer^,64);
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
                    ErrorCode := SC_CBC_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_SHACAL_256_BLOCK;
            SC_CBC_Reset (IV,ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SHACAL_CBC_ENCRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

procedure EncryptShacalCBC (var aKey;
                            const aKeySize: WORD;
                            const anIV: TShacal256Block;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    ctx          : TShacalCtx;
    IV           : TShacal256Block;
    ErrorCode    : Integer;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
    oKeySize     : WORD;
begin
    ErrorCode := 0;
    try
        oKeySize := aKeySize div 8;
        { используем переданный вектор инициализации }
        IV := anIV;
        { инициализируем контекст }
        ErrorCode := SC_CBC_Init (aKey,oKeySize,IV,ctx);
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
                    ErrorCode := SC_CBC_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_SHACAL_256_BLOCK;
            SC_CBC_Reset (IV,ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SHACAL_CBC_ENCRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

procedure DecryptShacalCBC (var aKey;
                            const aKeySize: WORD;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    ctx          : TShacalCtx;
    IV           : TShacal256Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
    oKeySize     : WORD;
begin
    ErrorCode := 0;
    try
        oKeySize := aKeySize div 8;
        { обнуляем вектор инициализации }
        IV := NULL_SHACAL_256_BLOCK;
        { инициализируем контекст }
        ErrorCode := SC_CBC_Init (aKey,oKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { читаем вектор инициализации и обновляем контекст }
            try
                Buffer := AllocMem (64+1);
                if ( Read (Buffer^,64) > 0 ) then
                begin
                    IV := HexToShacalIV ( StrPas (Buffer) );
                    ErrorCode := SC_CBC_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := SC_CBC_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_SHACAL_256_BLOCK;
            SC_CBC_Reset (IV,ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SHACAL_CBC_DECRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

procedure DecryptShacalCBC (var aKey;
                            const aKeySize: WORD;
                            const anIV: TShacal256Block;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    ctx          : TShacalCtx;
    IV           : TShacal256Block;
    ErrorCode    : Integer;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
    oKeySize     : WORD;
begin
    ErrorCode := 0;
    try
        oKeySize := aKeySize div 8;
        { используем переданный вектор инициализации }
        IV := anIV;
        { инициализируем контекст }
        ErrorCode := SC_CBC_Init (aKey,oKeySize,IV,ctx);
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
                    ErrorCode := SC_CBC_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_SHACAL_256_BLOCK;
            SC_CBC_Reset (IV,ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SHACAL_CBC_DECRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

function EncryptShacalCBC (var aKey;
                           const aKeySize: WORD;
                           aRandomFunc: TRandomFunction;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create (aValue);
        Output := TStringStream.Create ('');
        try
            EncryptShacalCBC (aKey,aKeySize,aRandomFunc,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SHACAL_CBC_ENCRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

function EncryptShacalCBC (var aKey;
                           const aKeySize: WORD;
                           const anIV: TShacal256Block;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create (aValue);
        Output := TStringStream.Create ('');
        try
            EncryptShacalCBC (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SHACAL_CBC_ENCRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

function DecryptShacalCBC (var aKey;
                           const aKeySize: WORD;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create ( HexToStr (aValue) );
        Output := TStringStream.Create ('');
        try
            DecryptShacalCBC (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SHACAL_CBC_DECRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

function DecryptShacalCBC (var aKey;
                           const aKeySize: WORD;
                           const anIV: TShacal256Block;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create ( HexToStr (aValue) );
        Output := TStringStream.Create ('');
        try
            DecryptShacalCBC (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SHACAL_CBC_DECRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

procedure EncryptShacalCBC (const aKey: Hex;
                            aRandomFunc: TRandomFunction;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    Key   : PKey;
    oSize : WORD;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        EncryptShacalCBC (Key^,
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

procedure EncryptShacalCBC (const aKey: Hex;
                            const anIV: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TShacal256Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToShacalIV (anIV);
        EncryptShacalCBC (Key^,
                          oSize*8,
                          IV,
                          anInput,
                          anOutput,
                          aBlockSize);
    finally
        IV := NULL_SHACAL_256_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

procedure DecryptShacalCBC (const aKey: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    Key   : PKey;
    oSize : WORD;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        DecryptShacalCBC (Key^,
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

procedure DecryptShacalCBC (const aKey: Hex;
                            const anIV: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TShacal256Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToShacalIV (anIV);
        DecryptShacalCBC (Key^,
                          oSize*8,
                          IV,
                          anInput,
                          anOutput,
                          aBlockSize);
    finally
        IV := NULL_SHACAL_256_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function EncryptShacalCBC (const aKey: Hex;
                           aRandomFunc: TRandomFunction;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Key   : PKey;
    oSize : WORD;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        Result := EncryptShacalCBC (Key^,
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

function EncryptShacalCBC (const aKey: Hex;
                           const anIV: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TShacal256Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToShacalIV (anIV);
        Result := EncryptShacalCBC (Key^,
                                    oSize*8,
                                    IV,
                                    aValue,
                                    aBlockSize);
    finally
        IV := NULL_SHACAL_256_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function DecryptShacalCBC (const aKey: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Key   : PKey;
    oSize : WORD;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        Result := DecryptShacalCBC (Key^,
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

function DecryptShacalCBC (const aKey: Hex;
                           const anIV: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TShacal256Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToShacalIV (anIV);
        Result := DecryptShacalCBC (Key^,
                                    oSize*8,
                                    IV,
                                    aValue,
                                    aBlockSize);
    finally
        IV := NULL_SHACAL_256_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;
{$ENDIF CBC}

{$IFDEF CFB}
procedure EncryptShacalCFB (var aKey;
                            const aKeySize: WORD;
                            aRandomFunc: TRandomFunction;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    ctx          : TShacalCtx;
    IV           : TShacal256Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
    oKeySize     : WORD;
begin
    ErrorCode := 0;
    try
        oKeySize := aKeySize div 8;
        { формируем псевдо-случайный 256-битный вектор инициализации }
        IV := GetRandomShacalIV (aRandomFunc);
        { инициализируем контекст }
        ErrorCode := SC_CFB_Init (aKey,oKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { записываем вектор инициализации в начало }
            Buffer := PChar ( ShacalIVToHex (IV) );
            anOutput.Write (Buffer^,64);
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
                    ErrorCode := SC_CFB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_SHACAL_256_BLOCK;
            SC_CFB_Reset (IV,ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SHACAL_CFB_ENCRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

procedure EncryptShacalCFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TShacal256Block;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    ctx          : TShacalCtx;
    IV           : TShacal256Block;
    ErrorCode    : Integer;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
    oKeySize     : WORD;
begin
    ErrorCode := 0;
    try
        oKeySize := aKeySize div 8;
        { используем переданный вектор инициализации }
        IV := anIV;
        { инициализируем контекст }
        ErrorCode := SC_CFB_Init (aKey,oKeySize,IV,ctx);
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
                    ErrorCode := SC_CFB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_SHACAL_256_BLOCK;
            SC_CFB_Reset (IV,ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SHACAL_CFB_ENCRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

procedure DecryptShacalCFB (var aKey;
                            const aKeySize: WORD;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    ctx          : TShacalCtx;
    IV           : TShacal256Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
    oKeySize     : WORD;
begin
    ErrorCode := 0;
    try
        oKeySize := aKeySize div 8;
        { обнуляем вектор инициализации }
        IV := NULL_SHACAL_256_BLOCK;
        { инициализируем контекст }
        ErrorCode := SC_CFB_Init (aKey,oKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { читаем вектор инициализации и обновляем контекст }
            try
                Buffer := AllocMem (64+1);
                if ( Read (Buffer^,64) > 0 ) then
                begin
                    IV := HexToShacalIV ( StrPas (Buffer) );
                    ErrorCode := SC_CFB_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := SC_CFB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_SHACAL_256_BLOCK;
            SC_CFB_Reset (IV,ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SHACAL_CFB_DECRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

procedure DecryptShacalCFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TShacal256Block;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    ctx          : TShacalCtx;
    IV           : TShacal256Block;
    ErrorCode    : Integer;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
    oKeySize     : WORD;
begin
    ErrorCode := 0;
    try
        oKeySize := aKeySize div 8;
        { используем переданный вектор инициализации }
        IV := anIV;
        { инициализируем контекст }
        ErrorCode := SC_CFB_Init (aKey,oKeySize,IV,ctx);
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
                    ErrorCode := SC_CFB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_SHACAL_256_BLOCK;
            SC_CFB_Reset (IV,ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SHACAL_CFB_DECRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

function EncryptShacalCFB (var aKey;
                           const aKeySize: WORD;
                           aRandomFunc: TRandomFunction;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create (aValue);
        Output := TStringStream.Create ('');
        try
            EncryptShacalCFB (aKey,aKeySize,aRandomFunc,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SHACAL_CFB_ENCRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

function EncryptShacalCFB (var aKey;
                           const aKeySize: WORD;
                           const anIV: TShacal256Block;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create (aValue);
        Output := TStringStream.Create ('');
        try
            EncryptShacalCFB (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SHACAL_CFB_ENCRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

function DecryptShacalCFB (var aKey;
                           const aKeySize: WORD;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create ( HexToStr (aValue) );
        Output := TStringStream.Create ('');
        try
            DecryptShacalCFB (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SHACAL_CFB_DECRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

function DecryptShacalCFB (var aKey;
                           const aKeySize: WORD;
                           const anIV: TShacal256Block;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create ( HexToStr (aValue) );
        Output := TStringStream.Create ('');
        try
            DecryptShacalCFB (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SHACAL_CFB_DECRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

procedure EncryptShacalCFB (const aKey: Hex;
                            aRandomFunc: TRandomFunction;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    Key   : PKey;
    oSize : WORD;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        EncryptShacalCFB (Key^,
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

procedure EncryptShacalCFB (const aKey: Hex;
                            const anIV: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TShacal256Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToShacalIV (anIV);
        EncryptShacalCFB (Key^,
                          oSize*8,
                          IV,
                          anInput,
                          anOutput,
                          aBlockSize);
    finally
        IV := NULL_SHACAL_256_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

procedure DecryptShacalCFB (const aKey: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    Key   : PKey;
    oSize : WORD;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        DecryptShacalCFB (Key^,
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

procedure DecryptShacalCFB (const aKey: Hex;
                            const anIV: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TShacal256Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToShacalIV (anIV);
        DecryptShacalCFB (Key^,
                          oSize*8,
                          IV,
                          anInput,
                          anOutput,
                          aBlockSize);
    finally
        IV := NULL_SHACAL_256_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function EncryptShacalCFB (const aKey: Hex;
                           aRandomFunc: TRandomFunction;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Key   : PKey;
    oSize : WORD;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        Result := EncryptShacalCFB (Key^,
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

function EncryptShacalCFB (const aKey: Hex;
                           const anIV: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TShacal256Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToShacalIV (anIV);
        Result := EncryptShacalCFB (Key^,
                                    oSize*8,
                                    IV,
                                    aValue,
                                    aBlockSize);
    finally
        IV := NULL_SHACAL_256_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function DecryptShacalCFB (const aKey: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Key   : PKey;
    oSize : WORD;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        Result := DecryptShacalCFB (Key^,
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

function DecryptShacalCFB (const aKey: Hex;
                           const anIV: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TShacal256Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToShacalIV (anIV);
        Result := DecryptShacalCFB (Key^,
                                    oSize*8,
                                    IV,
                                    aValue,
                                    aBlockSize);
    finally
        IV := NULL_SHACAL_256_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;
{$ENDIF CFB}

{$IFDEF OFB}
procedure EncryptShacalOFB (var aKey;
                            const aKeySize: WORD;
                            aRandomFunc: TRandomFunction;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    ctx          : TShacalCtx;
    IV           : TShacal256Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
    oKeySize     : WORD;
begin
    ErrorCode := 0;
    try
        oKeySize := aKeySize div 8;
        { формируем псевдо-случайный 256-битный вектор инициализации }
        IV := GetRandomShacalIV (aRandomFunc);
        { инициализируем контекст }
        ErrorCode := SC_OFB_Init (aKey,oKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { записываем вектор инициализации в начало }
            Buffer := PChar ( ShacalIVToHex (IV) );
            anOutput.Write (Buffer^,64);
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
                    ErrorCode := SC_OFB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_SHACAL_256_BLOCK;
            SC_OFB_Reset (IV,ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SHACAL_OFB_ENCRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

procedure EncryptShacalOFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TShacal256Block;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    ctx          : TShacalCtx;
    IV           : TShacal256Block;
    ErrorCode    : Integer;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
    oKeySize     : WORD;
begin
    ErrorCode := 0;
    try
        oKeySize := aKeySize div 8;
        { используем переданный вектор инициализации }
        IV := anIV;
        { инициализируем контекст }
        ErrorCode := SC_OFB_Init (aKey,oKeySize,IV,ctx);
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
                    ErrorCode := SC_OFB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_SHACAL_256_BLOCK;
            SC_OFB_Reset (IV,ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SHACAL_OFB_ENCRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

procedure DecryptShacalOFB (var aKey;
                            const aKeySize: WORD;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    ctx          : TShacalCtx;
    IV           : TShacal256Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
    oKeySize     : WORD;
begin
    ErrorCode := 0;
    try
        oKeySize := aKeySize div 8;
        { обнуляем вектор инициализации }
        IV := NULL_SHACAL_256_BLOCK;
        { инициализируем контекст }
        ErrorCode := SC_OFB_Init (aKey,oKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { читаем вектор инициализации и обновляем контекст }
            try
                Buffer := AllocMem (64+1);
                if ( Read (Buffer^,64) > 0 ) then
                begin
                    IV := HexToShacalIV ( StrPas (Buffer) );
                    ErrorCode := SC_OFB_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := SC_OFB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_SHACAL_256_BLOCK;
            SC_OFB_Reset (IV,ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SHACAL_OFB_DECRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

procedure DecryptShacalOFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TShacal256Block;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    ctx          : TShacalCtx;
    IV           : TShacal256Block;
    ErrorCode    : Integer;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
    oKeySize     : WORD;
begin
    ErrorCode := 0;
    try
        oKeySize := aKeySize div 8;
        { используем переданный вектор инициализации }
        IV := anIV;
        { инициализируем контекст }
        ErrorCode := SC_OFB_Init (aKey,oKeySize,IV,ctx);
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
                    ErrorCode := SC_OFB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_SHACAL_256_BLOCK;
            SC_OFB_Reset (IV,ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SHACAL_OFB_DECRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

function EncryptShacalOFB (var aKey;
                           const aKeySize: WORD;
                           aRandomFunc: TRandomFunction;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create (aValue);
        Output := TStringStream.Create ('');
        try
            EncryptShacalOFB (aKey,aKeySize,aRandomFunc,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SHACAL_OFB_ENCRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

function EncryptShacalOFB (var aKey;
                           const aKeySize: WORD;
                           const anIV: TShacal256Block;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create (aValue);
        Output := TStringStream.Create ('');
        try
            EncryptShacalOFB (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SHACAL_OFB_ENCRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

function DecryptShacalOFB (var aKey;
                           const aKeySize: WORD;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create ( HexToStr (aValue) );
        Output := TStringStream.Create ('');
        try
            DecryptShacalOFB (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SHACAL_OFB_DECRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

function DecryptShacalOFB (var aKey;
                           const aKeySize: WORD;
                           const anIV: TShacal256Block;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Input  : TStream;
    Output : TStream;
begin
    Result := '';
    try
        Input := TStringStream.Create ( HexToStr (aValue) );
        Output := TStringStream.Create ('');
        try
            DecryptShacalOFB (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SHACAL_OFB_DECRYPT,[aKeySize,SHACAL_VERSION]), E.Message ]);
    end;
end;

procedure EncryptShacalOFB (const aKey: Hex;
                            aRandomFunc: TRandomFunction;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    Key   : PKey;
    oSize : WORD;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        EncryptShacalOFB (Key^,
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

procedure EncryptShacalOFB (const aKey: Hex;
                            const anIV: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TShacal256Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToShacalIV (anIV);
        EncryptShacalOFB (Key^,
                          oSize*8,
                          IV,
                          anInput,
                          anOutput,
                          aBlockSize);
    finally
        IV := NULL_SHACAL_256_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

procedure DecryptShacalOFB (const aKey: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    Key   : PKey;
    oSize : WORD;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        DecryptShacalOFB (Key^,
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

procedure DecryptShacalOFB (const aKey: Hex;
                            const anIV: Hex;
                            const anInput: TStream;
                            out anOutput: TStream;
                            const aBlockSize: LongWord = 32);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TShacal256Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToShacalIV (anIV);
        DecryptShacalOFB (Key^,
                          oSize*8,
                          IV,
                          anInput,
                          anOutput,
                          aBlockSize);
    finally
        IV := NULL_SHACAL_256_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function EncryptShacalOFB (const aKey: Hex;
                           aRandomFunc: TRandomFunction;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Key   : PKey;
    oSize : WORD;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        Result := EncryptShacalOFB (Key^,
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

function EncryptShacalOFB (const aKey: Hex;
                           const anIV: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TShacal256Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToShacalIV (anIV);
        Result := EncryptShacalOFB (Key^,
                                    oSize*8,
                                    IV,
                                    aValue,
                                    aBlockSize);
    finally
        IV := NULL_SHACAL_256_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function DecryptShacalOFB (const aKey: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Key   : PKey;
    oSize : WORD;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        Result := DecryptShacalOFB (Key^,
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

function DecryptShacalOFB (const aKey: Hex;
                           const anIV: Hex;
                           const aValue: String;
                           const aBlockSize: LongWord = 32) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TShacal256Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToShacalIV (anIV);
        Result := DecryptShacalOFB (Key^,
                                    oSize*8,
                                    IV,
                                    aValue,
                                    aBlockSize);
    finally
        IV := NULL_SHACAL_256_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;
{$ENDIF OFB}

function SHACAL_VERSION : String;
var
    Version : TVersionInfo;
begin
    {$IFDEF EXTERNAL_LINK}
    Version := GetFileVersionInfo (SHACAL_DLL);
    // Result := Format ('v.%s',[GET_SHACAL_DLL_VERSION]);
    {$ELSE EXTERNAL_LINK}
    Version := StrToVersionInfo ('0.12.0.0');
    {$ENDIF EXTERNAL_LINK}
    Result := Format ('v.%d.%d.%d.%d',[Version.Major,
                                       Version.Minor,
                                       Version.Release,
                                       Version.Build]);
end;