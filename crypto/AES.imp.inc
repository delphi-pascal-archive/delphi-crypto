{******************************************************************************}
{*  AES.imp.inc                                                               *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
function AESIV (anArgs: array of const) : TAES128Block;
var
    I : Integer;
begin
    Result := NULL_AES_128_BLOCK;
    if (  ( High (anArgs) = 0 ) and IsHex ( toString (anArgs [0]) )  ) then
    begin
        Result := HexToAESIV ( toString (anArgs [0]) );
        Exit;
    end;
    for I := 0 to High (anArgs) do
    begin
        Result [I] := toInteger (anArgs [I]);
        if ( I >= High (TAES128Block) ) then
            Break;
    end;
end;

function AESIVToHex (const aValue: TAES128Block) : Hex;
var
    I : Integer;
begin
    Result := '';
    for I := Low (TAES128Block) to High (TAES128Block) do
        Result := Result + IntToHex (aValue [I],2);
end;

function HexToAESIV (const aValue: Hex) : TAES128Block;
var
    I      : Integer;
    J      : Integer;
    Buffer : String [3];
begin
    Result := NULL_AES_128_BLOCK;
    if not IsHex (aValue) then
        Exit;
    I := 0;
    J := 0;
    while ( I < Length (aValue) ) do
    begin
        Inc (I);
        if ( I mod 2 = 0 ) then
        begin
            if ( J > High (TAES128Block) )  then
                Break;
            Buffer := Format ('$%s%s',[ aValue [I-1], aValue [I] ]);
            Result [J] := StrToInt (Buffer);
            Inc (J);
        end;
    end;
end;

function GetRandomAESIV (aRandomFunc: TRandomFunction) : TAES128Block;
var
    I : Integer;
begin
    for I := Low (TAES128Block) to High (TAES128Block) do
        Result [I] := aRandomFunc (255);
end;

{$IFDEF ECB}
procedure EncryptAESECB (var aKey;
                         const aKeySize: WORD;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16);
var
    ctx          : TAESCtx;
    ErrorCode    : Integer;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { инициализируем контекст }
        ErrorCode := AES_ECB_Init_Encr (aKey,aKeySize,ctx);
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
                    ErrorCode := AES_ECB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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
                                   [ Format (ERR_AES_ECB_ENCRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

procedure DecryptAESECB (var aKey;
                         const aKeySize: WORD;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16);
var
    ctx          : TAESCtx;
    ErrorCode    : Integer;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { инициализируем контекст }
        ErrorCode := AES_ECB_Init_Decr (aKey,aKeySize,ctx);
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
                    ErrorCode := AES_ECB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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
                                   [ Format (ERR_AES_ECB_DECRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

function EncryptAESECB (var aKey;
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
            EncryptAESECB (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_AES_ECB_ENCRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

function DecryptAESECB (var aKey;
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
            DecryptAESECB (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_AES_ECB_DECRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

procedure EncryptAESECB (const aKey: Hex;
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
        EncryptAESECB (Key^,
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

procedure DecryptAESECB (const aKey: Hex;
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
        DecryptAESECB (Key^,
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

function EncryptAESECB (const aKey: Hex;
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
        Result := EncryptAESECB (Key^,
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

function DecryptAESECB (const aKey: Hex;
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
        Result := DecryptAESECB (Key^,
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
procedure EncryptAESCBC (var aKey;
                         const aKeySize: WORD;
                         aRandomFunc: TRandomFunction;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16);
var
    ctx          : TAESCtx;
    IV           : TAES128Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { формируем псевдо-случайный 128-битный вектор инициализации }
        IV := GetRandomAESIV (aRandomFunc);
        { инициализируем контекст }
        ErrorCode := AES_CBC_Init_Encr (aKey,aKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { записываем вектор инициализации в начало }
            Buffer := PChar ( AESIVToHex (IV) );
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
                    ErrorCode := AES_CBC_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_AES_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_AES_CBC_ENCRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

procedure EncryptAESCBC (var aKey;
                         const aKeySize: WORD;
                         const anIV: TAES128Block;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16);
var
    ctx          : TAESCtx;
    IV           : TAES128Block;
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
        ErrorCode := AES_CBC_Init_Encr (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := AES_CBC_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_AES_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_AES_CBC_ENCRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

procedure DecryptAESCBC (var aKey;
                         const aKeySize: WORD;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16);
var
    ctx          : TAESCtx;
    IV           : TAES128Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { обнуляем вектор инициализации }
        IV := NULL_AES_128_BLOCK;
        { инициализируем контекст }
        ErrorCode := AES_CBC_Init_Decr (aKey,aKeySize,IV,ctx);
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
                    IV := HexToAESIV ( StrPas (Buffer) );
                    ErrorCode := AES_CBC_Init_Decr (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := AES_CBC_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_AES_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_AES_CBC_DECRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

procedure DecryptAESCBC (var aKey;
                         const aKeySize: WORD;
                         const anIV: TAES128Block;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16);
var
    ctx          : TAESCtx;
    IV           : TAES128Block;
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
        ErrorCode := AES_CBC_Init_Decr (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := AES_CBC_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_AES_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_AES_CBC_DECRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

function EncryptAESCBC (var aKey;
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
            EncryptAESCBC (aKey,aKeySize,aRandomFunc,Input,Output,aBlockSize);
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
                                   [ Format (ERR_AES_CBC_ENCRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

function EncryptAESCBC (var aKey;
                        const aKeySize: WORD;
                        const anIV: TAES128Block;
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
            EncryptAESCBC (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_AES_CBC_ENCRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

function DecryptAESCBC (var aKey;
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
            DecryptAESCBC (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_AES_CBC_DECRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

function DecryptAESCBC (var aKey;
                        const aKeySize: WORD;
                        const anIV: TAES128Block;
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
            DecryptAESCBC (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_AES_CBC_DECRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

procedure EncryptAESCBC (const aKey: Hex;
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
        EncryptAESCBC (Key^,
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

procedure EncryptAESCBC (const aKey: Hex;
                         const anIV: Hex;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TAES128Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToAESIV (anIV);
        EncryptAESCBC (Key^,
                       oSize*8,
                       IV,
                       anInput,
                       anOutput,
                       aBlockSize);
    finally
        IV := NULL_AES_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

procedure DecryptAESCBC (const aKey: Hex;
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
        DecryptAESCBC (Key^,
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

procedure DecryptAESCBC (const aKey: Hex;
                         const anIV: Hex;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TAES128Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToAESIV (anIV);
        DecryptAESCBC (Key^,
                       oSize*8,
                       IV,
                       anInput,
                       anOutput,
                       aBlockSize);
    finally
        IV := NULL_AES_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function EncryptAESCBC (const aKey: Hex;
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
        Result := EncryptAESCBC (Key^,
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

function EncryptAESCBC (const aKey: Hex;
                        const anIV: Hex;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TAES128Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToAESIV (anIV);
        Result := EncryptAESCBC (Key^,
                                 oSize*8,
                                 IV,
                                 aValue,
                                 aBlockSize);
    finally
        IV := NULL_AES_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function DecryptAESCBC (const aKey: Hex;
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
        Result := DecryptAESCBC (Key^,
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

function DecryptAESCBC (const aKey: Hex;
                        const anIV: Hex;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TAES128Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToAESIV (anIV);
        Result := DecryptAESCBC (Key^,
                                 oSize*8,
                                 IV,
                                 aValue,
                                 aBlockSize);
    finally
        IV := NULL_AES_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;
{$ENDIF CBC}

{$IFDEF CFB}
procedure EncryptAESCFB (var aKey;
                         const aKeySize: WORD;
                         aRandomFunc: TRandomFunction;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16);
var
    ctx          : TAESCtx;
    IV           : TAES128Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { формируем псевдо-случайный 128-битный вектор инициализации }
        IV := GetRandomAESIV (aRandomFunc);
        { инициализируем контекст }
        ErrorCode := AES_CFB_Init (aKey,aKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { записываем вектор инициализации в начало }
            Buffer := PChar ( AESIVToHex (IV) );
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
                    ErrorCode := AES_CFB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_AES_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_AES_CFB_ENCRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

procedure EncryptAESCFB (var aKey;
                         const aKeySize: WORD;
                         const anIV: TAES128Block;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16);
var
    ctx          : TAESCtx;
    IV           : TAES128Block;
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
        ErrorCode := AES_CFB_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := AES_CFB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_AES_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_AES_CFB_ENCRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

procedure DecryptAESCFB (var aKey;
                         const aKeySize: WORD;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16);
var
    ctx          : TAESCtx;
    IV           : TAES128Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { обнуляем вектор инициализации }
        IV := NULL_AES_128_BLOCK;
        { инициализируем контекст }
        ErrorCode := AES_CFB_Init (aKey,aKeySize,IV,ctx);
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
                    IV := HexToAESIV ( StrPas (Buffer) );
                    ErrorCode := AES_CFB_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := AES_CFB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_AES_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_AES_CFB_DECRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

procedure DecryptAESCFB (var aKey;
                         const aKeySize: WORD;
                         const anIV: TAES128Block;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16);
var
    ctx          : TAESCtx;
    IV           : TAES128Block;
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
        ErrorCode := AES_CFB_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := AES_CFB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_AES_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_AES_CFB_DECRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

function EncryptAESCFB (var aKey;
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
            EncryptAESCFB (aKey,aKeySize,aRandomFunc,Input,Output,aBlockSize);
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
                                   [ Format (ERR_AES_CFB_ENCRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

function EncryptAESCFB (var aKey;
                        const aKeySize: WORD;
                        const anIV: TAES128Block;
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
            EncryptAESCFB (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_AES_CFB_ENCRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

function DecryptAESCFB (var aKey;
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
            DecryptAESCFB (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_AES_CFB_DECRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

function DecryptAESCFB (var aKey;
                        const aKeySize: WORD;
                        const anIV: TAES128Block;
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
            DecryptAESCFB (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_AES_CFB_DECRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

procedure EncryptAESCFB (const aKey: Hex;
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
        EncryptAESCFB (Key^,
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

procedure EncryptAESCFB (const aKey: Hex;
                         const anIV: Hex;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TAES128Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToAESIV (anIV);
        EncryptAESCFB (Key^,
                       oSize*8,
                       IV,
                       anInput,
                       anOutput,
                       aBlockSize);
    finally
        IV := NULL_AES_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

procedure DecryptAESCFB (const aKey: Hex;
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
        DecryptAESCFB (Key^,
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

procedure DecryptAESCFB (const aKey: Hex;
                         const anIV: Hex;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TAES128Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToAESIV (anIV);
        DecryptAESCFB (Key^,
                       oSize*8,
                       IV,
                       anInput,
                       anOutput,
                       aBlockSize);
    finally
        IV := NULL_AES_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function EncryptAESCFB (const aKey: Hex;
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
        Result := EncryptAESCFB (Key^,
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

function EncryptAESCFB (const aKey: Hex;
                        const anIV: Hex;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TAES128Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToAESIV (anIV);
        Result := EncryptAESCFB (Key^,
                                 oSize*8,
                                 IV,
                                 aValue,
                                 aBlockSize);
    finally
        IV := NULL_AES_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function DecryptAESCFB (const aKey: Hex;
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
        Result := DecryptAESCFB (Key^,
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

function DecryptAESCFB (const aKey: Hex;
                        const anIV: Hex;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TAES128Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToAESIV (anIV);
        Result := DecryptAESCFB (Key^,
                                 oSize*8,
                                 IV,
                                 aValue,
                                 aBlockSize);
    finally
        IV := NULL_AES_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;
{$ENDIF CFB}

{$IFDEF OFB}
procedure EncryptAESOFB (var aKey;
                         const aKeySize: WORD;
                         aRandomFunc: TRandomFunction;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16);
var
    ctx          : TAESCtx;
    IV           : TAES128Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { формируем псевдо-случайный 128-битный вектор инициализации }
        IV := GetRandomAESIV (aRandomFunc);
        { инициализируем контекст }
        ErrorCode := AES_OFB_Init (aKey,aKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { записываем вектор инициализации в начало }
            Buffer := PChar ( AESIVToHex (IV) );
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
                    ErrorCode := AES_OFB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_AES_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_AES_OFB_ENCRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

procedure EncryptAESOFB (var aKey;
                         const aKeySize: WORD;
                         const anIV: TAES128Block;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16);
var
    ctx          : TAESCtx;
    IV           : TAES128Block;
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
        ErrorCode := AES_OFB_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := AES_OFB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_AES_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_AES_OFB_ENCRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

procedure DecryptAESOFB (var aKey;
                         const aKeySize: WORD;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16);
var
    ctx          : TAESCtx;
    IV           : TAES128Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { обнуляем вектор инициализации }
        IV := NULL_AES_128_BLOCK;
        { инициализируем контекст }
        ErrorCode := AES_OFB_Init (aKey,aKeySize,IV,ctx);
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
                    IV := HexToAESIV ( StrPas (Buffer) );
                    ErrorCode := AES_OFB_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := AES_OFB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_AES_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_AES_OFB_DECRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

procedure DecryptAESOFB (var aKey;
                         const aKeySize: WORD;
                         const anIV: TAES128Block;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16);
var
    ctx          : TAESCtx;
    IV           : TAES128Block;
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
        ErrorCode := AES_OFB_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := AES_OFB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_AES_128_BLOCK;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_AES_OFB_DECRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

function EncryptAESOFB (var aKey;
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
            EncryptAESOFB (aKey,aKeySize,aRandomFunc,Input,Output,aBlockSize);
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
                                   [ Format (ERR_AES_OFB_ENCRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

function EncryptAESOFB (var aKey;
                        const aKeySize: WORD;
                        const anIV: TAES128Block;
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
            EncryptAESOFB (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_AES_OFB_ENCRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

function DecryptAESOFB (var aKey;
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
            DecryptAESOFB (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_AES_OFB_DECRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

function DecryptAESOFB (var aKey;
                        const aKeySize: WORD;
                        const anIV: TAES128Block;
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
            DecryptAESOFB (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_AES_OFB_DECRYPT,[aKeySize,AES_VERSION]), E.Message ]);
    end;
end;

procedure EncryptAESOFB (const aKey: Hex;
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
        EncryptAESOFB (Key^,
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

procedure EncryptAESOFB (const aKey: Hex;
                         const anIV: Hex;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TAES128Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToAESIV (anIV);
        EncryptAESOFB (Key^,
                       oSize*8,
                       IV,
                       anInput,
                       anOutput,
                       aBlockSize);
    finally
        IV := NULL_AES_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

procedure DecryptAESOFB (const aKey: Hex;
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
        DecryptAESOFB (Key^,
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

procedure DecryptAESOFB (const aKey: Hex;
                         const anIV: Hex;
                         const anInput: TStream;
                         out anOutput: TStream;
                         const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TAES128Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToAESIV (anIV);
        DecryptAESOFB (Key^,
                       oSize*8,
                       IV,
                       anInput,
                       anOutput,
                       aBlockSize);
    finally
        IV := NULL_AES_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function EncryptAESOFB (const aKey: Hex;
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
        Result := EncryptAESOFB (Key^,
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

function EncryptAESOFB (const aKey: Hex;
                        const anIV: Hex;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TAES128Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToAESIV (anIV);
        Result := EncryptAESOFB (Key^,
                                 oSize*8,
                                 IV,
                                 aValue,
                                 aBlockSize);
    finally
        IV := NULL_AES_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function DecryptAESOFB (const aKey: Hex;
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
        Result := DecryptAESOFB (Key^,
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

function DecryptAESOFB (const aKey: Hex;
                        const anIV: Hex;
                        const aValue: String;
                        const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TAES128Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToAESIV (anIV);
        Result := DecryptAESOFB (Key^,
                                 oSize*8,
                                 IV,
                                 aValue,
                                 aBlockSize);
    finally
        IV := NULL_AES_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;
{$ENDIF OFB}

function AES_VERSION : String;
var
    Version : TVersionInfo;
begin
    {$IFDEF EXTERNAL_LINK}
    Version := GetFileVersionInfo (AES_DLL);
    // Result := Format ('v.%s',[GET_AES_DLL_VERSION]);
    {$ELSE EXTERNAL_LINK}
    Version := StrToVersionInfo ('0.26.0.0');
    {$ENDIF EXTERNAL_LINK}
    Result := Format ('v.%d.%d.%d.%d',[Version.Major,
                                       Version.Minor,
                                       Version.Release,
                                       Version.Build]);
end;
