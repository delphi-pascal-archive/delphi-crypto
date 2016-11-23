{******************************************************************************}
{*  TwoFish.imp.inc                                                           *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
function TwoFishIV (anArgs: array of const) : TTwoFish128Block;
var
    I : Integer;
begin
    Result := NULL_TWOFISH_128_BLOCK;
    if (  ( High (anArgs) = 0 ) and IsHex ( toString (anArgs [0]) )  ) then
    begin
        Result := HexToTwoFishIV ( toString (anArgs [0]) );
        Exit;
    end;
    for I := 0 to High (anArgs) do
    begin
        Result [I] := toInteger (anArgs [I]);
        if ( I >= High (TTwoFish128Block) ) then
            Break;
    end;
end;

function TwoFishIVToHex (const aValue: TTwoFish128Block) : Hex;
var
    I : Integer;
begin
    Result := '';
    for I := Low (TTwoFish128Block) to High (TTwoFish128Block) do
        Result := Result + IntToHex (aValue [I],2);
end;

function HexToTwoFishIV (const aValue: Hex) : TTwoFish128Block;
var
    I      : Integer;
    J      : Integer;
    Buffer : String [3];
begin
    Result := NULL_TWOFISH_128_BLOCK;
    if not IsHex (aValue) then
        Exit;
    I := 0;
    J := 0;
    while ( I < Length (aValue) ) do
    begin
        Inc (I);
        if ( I mod 2 = 0 ) then
        begin
            if ( J > High (TTwoFish128Block) )  then
                Break;
            Buffer := Format ('$%s%s',[ aValue [I-1], aValue [I] ]);
            Result [J] := StrToInt (Buffer);
            Inc (J);
        end;
    end;
end;

function GetRandomTwoFishIV (aRandomFunc: TRandomFunction) : TTwoFish128Block;
var
    I : Integer;
begin
    for I := Low (TTwoFish128Block) to High (TTwoFish128Block) do
        Result [I] := aRandomFunc (255);
end;

{$IFDEF ECB}
procedure EncryptTwoFishECB (var aKey;
                             const aKeySize: WORD;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TTwoFishCtx;
    ErrorCode    : Integer;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { инициализируем контекст }
        ErrorCode := TF_ECB_Init (aKey,aKeySize,ctx);
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
                    ErrorCode := TF_ECB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            TF_ECB_Reset (ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_TWOFISH_ECB_ENCRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

procedure DecryptTwoFishECB (var aKey;
                             const aKeySize: WORD;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TTwoFishCtx;
    ErrorCode    : Integer;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { инициализируем контекст }
        ErrorCode := TF_ECB_Init (aKey,aKeySize,ctx);
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
                    ErrorCode := TF_ECB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            TF_ECB_Reset (ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_TWOFISH_ECB_DECRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

function EncryptTwoFishECB (var aKey;
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
            EncryptTwoFishECB (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_TWOFISH_ECB_ENCRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

function DecryptTwoFishECB (var aKey;
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
            DecryptTwoFishECB (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_TWOFISH_ECB_DECRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

procedure EncryptTwoFishECB (const aKey: Hex;
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
        EncryptTwoFishECB (Key^,
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

procedure DecryptTwoFishECB (const aKey: Hex;
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
        DecryptTwoFishECB (Key^,
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

function EncryptTwoFishECB (const aKey: Hex;
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
        Result := EncryptTwoFishECB (Key^,
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

function DecryptTwoFishECB (const aKey: Hex;
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
        Result := DecryptTwoFishECB (Key^,
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
procedure EncryptTwoFishCBC (var aKey;
                             const aKeySize: WORD;
                             aRandomFunc: TRandomFunction;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TTwoFishCtx;
    IV           : TTwoFish128Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { формируем псевдо-случайный 128-битный вектор инициализации }
        IV := GetRandomTwoFishIV (aRandomFunc);
        { инициализируем контекст }
        ErrorCode := TF_CBC_Init (aKey,aKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { записываем вектор инициализации в начало }
            Buffer := PChar ( TwoFishIVToHex (IV) );
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
                    ErrorCode := TF_CBC_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_TWOFISH_128_BLOCK;
            TF_CBC_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_TWOFISH_CBC_ENCRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

procedure EncryptTwoFishCBC (var aKey;
                             const aKeySize: WORD;
                             const anIV: TTwoFish128Block;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TTwoFishCtx;
    IV           : TTwoFish128Block;
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
        ErrorCode := TF_CBC_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := TF_CBC_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_TWOFISH_128_BLOCK;
            TF_CBC_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_TWOFISH_CBC_ENCRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

procedure DecryptTwoFishCBC (var aKey;
                             const aKeySize: WORD;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TTwoFishCtx;
    IV           : TTwoFish128Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { обнуляем вектор инициализации }
        IV := NULL_TWOFISH_128_BLOCK;
        { инициализируем контекст }
        ErrorCode := TF_CBC_Init (aKey,aKeySize,IV,ctx);
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
                    IV := HexToTwoFishIV ( StrPas (Buffer) );
                    ErrorCode := TF_CBC_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := TF_CBC_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_TWOFISH_128_BLOCK;
            TF_CBC_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_TWOFISH_CBC_DECRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

procedure DecryptTwoFishCBC (var aKey;
                             const aKeySize: WORD;
                             const anIV: TTwoFish128Block;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TTwoFishCtx;
    IV           : TTwoFish128Block;
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
        ErrorCode := TF_CBC_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := TF_CBC_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_TWOFISH_128_BLOCK;
            TF_CBC_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_TWOFISH_CBC_DECRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

function EncryptTwoFishCBC (var aKey;
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
            EncryptTwoFishCBC (aKey,aKeySize,aRandomFunc,Input,Output,aBlockSize);
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
                                   [ Format (ERR_TWOFISH_CBC_ENCRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

function EncryptTwoFishCBC (var aKey;
                            const aKeySize: WORD;
                            const anIV: TTwoFish128Block;
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
            EncryptTwoFishCBC (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_TWOFISH_CBC_ENCRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

function DecryptTwoFishCBC (var aKey;
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
            DecryptTwoFishCBC (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_TWOFISH_CBC_DECRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

function DecryptTwoFishCBC (var aKey;
                            const aKeySize: WORD;
                            const anIV: TTwoFish128Block;
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
            DecryptTwoFishCBC (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_TWOFISH_CBC_DECRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

procedure EncryptTwoFishCBC (const aKey: Hex;
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
        EncryptTwoFishCBC (Key^,
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

procedure EncryptTwoFishCBC (const aKey: Hex;
                             const anIV: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TTwoFish128Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToTwoFishIV (anIV);
        EncryptTwoFishCBC (Key^,
                           oSize*8,
                           IV,
                           anInput,
                           anOutput,
                           aBlockSize);
    finally
        IV := NULL_TWOFISH_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

procedure DecryptTwoFishCBC (const aKey: Hex;
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
        DecryptTwoFishCBC (Key^,
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

procedure DecryptTwoFishCBC (const aKey: Hex;
                             const anIV: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TTwoFish128Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToTwoFishIV (anIV);
        DecryptTwoFishCBC (Key^,
                           oSize*8,
                           IV,
                           anInput,
                           anOutput,
                           aBlockSize);
    finally
        IV := NULL_TWOFISH_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function EncryptTwoFishCBC (const aKey: Hex;
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
        Result := EncryptTwoFishCBC (Key^,
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

function EncryptTwoFishCBC (const aKey: Hex;
                            const anIV: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TTwoFish128Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToTwoFishIV (anIV);
        Result := EncryptTwoFishCBC (Key^,
                                     oSize*8,
                                     IV,
                                     aValue,
                                     aBlockSize);
    finally
        IV := NULL_TWOFISH_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function DecryptTwoFishCBC (const aKey: Hex;
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
        Result := DecryptTwoFishCBC (Key^,
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

function DecryptTwoFishCBC (const aKey: Hex;
                            const anIV: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TTwoFish128Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToTwoFishIV (anIV);
        Result := DecryptTwoFishCBC (Key^,
                                     oSize*8,
                                     IV,
                                     aValue,
                                     aBlockSize);
    finally
        IV := NULL_TWOFISH_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;
{$ENDIF CBC}

{$IFDEF CFB}
procedure EncryptTwoFishCFB (var aKey;
                             const aKeySize: WORD;
                             aRandomFunc: TRandomFunction;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TTwoFishCtx;
    IV           : TTwoFish128Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { формируем псевдо-случайный 128-битный вектор инициализации }
        IV := GetRandomTwoFishIV (aRandomFunc);
        { инициализируем контекст }
        ErrorCode := TF_CFB_Init (aKey,aKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { записываем вектор инициализации в начало }
            Buffer := PChar ( TwoFishIVToHex (IV) );
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
                    ErrorCode := TF_CFB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_TWOFISH_128_BLOCK;
            TF_CFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_TWOFISH_CFB_ENCRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

procedure EncryptTwoFishCFB (var aKey;
                             const aKeySize: WORD;
                             const anIV: TTwoFish128Block;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TTwoFishCtx;
    IV           : TTwoFish128Block;
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
        ErrorCode := TF_CFB_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := TF_CFB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_TWOFISH_128_BLOCK;
            TF_CFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_TWOFISH_CFB_ENCRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

procedure DecryptTwoFishCFB (var aKey;
                             const aKeySize: WORD;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TTwoFishCtx;
    IV           : TTwoFish128Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { обнуляем вектор инициализации }
        IV := NULL_TWOFISH_128_BLOCK;
        { инициализируем контекст }
        ErrorCode := TF_CFB_Init (aKey,aKeySize,IV,ctx);
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
                    IV := HexToTwoFishIV ( StrPas (Buffer) );
                    ErrorCode := TF_CFB_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := TF_CFB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_TWOFISH_128_BLOCK;
            TF_CFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_TWOFISH_CFB_DECRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

procedure DecryptTwoFishCFB (var aKey;
                             const aKeySize: WORD;
                             const anIV: TTwoFish128Block;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TTwoFishCtx;
    IV           : TTwoFish128Block;
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
        ErrorCode := TF_CFB_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := TF_CFB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_TWOFISH_128_BLOCK;
            TF_CFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_TWOFISH_CFB_DECRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

function EncryptTwoFishCFB (var aKey;
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
            EncryptTwoFishCFB (aKey,aKeySize,aRandomFunc,Input,Output,aBlockSize);
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
                                   [ Format (ERR_TWOFISH_CFB_ENCRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

function EncryptTwoFishCFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TTwoFish128Block;
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
            EncryptTwoFishCFB (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_TWOFISH_CFB_ENCRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

function DecryptTwoFishCFB (var aKey;
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
            DecryptTwoFishCFB (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_TWOFISH_CFB_DECRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

function DecryptTwoFishCFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TTwoFish128Block;
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
            DecryptTwoFishCFB (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_TWOFISH_CFB_DECRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

procedure EncryptTwoFishCFB (const aKey: Hex;
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
        EncryptTwoFishCFB (Key^,
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

procedure EncryptTwoFishCFB (const aKey: Hex;
                             const anIV: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TTwoFish128Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToTwoFishIV (anIV);
        EncryptTwoFishCFB (Key^,
                           oSize*8,
                           IV,
                           anInput,
                           anOutput,
                           aBlockSize);
    finally
        IV := NULL_TWOFISH_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

procedure DecryptTwoFishCFB (const aKey: Hex;
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
        DecryptTwoFishCFB (Key^,
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

procedure DecryptTwoFishCFB (const aKey: Hex;
                             const anIV: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TTwoFish128Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToTwoFishIV (anIV);
        DecryptTwoFishCFB (Key^,
                           oSize*8,
                           IV,
                           anInput,
                           anOutput,
                           aBlockSize);
    finally
        IV := NULL_TWOFISH_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function EncryptTwoFishCFB (const aKey: Hex;
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
        Result := EncryptTwoFishCFB (Key^,
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

function EncryptTwoFishCFB (const aKey: Hex;
                            const anIV: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TTwoFish128Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToTwoFishIV (anIV);
        Result := EncryptTwoFishCFB (Key^,
                                     oSize*8,
                                     IV,
                                     aValue,
                                     aBlockSize);
    finally
        IV := NULL_TWOFISH_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function DecryptTwoFishCFB (const aKey: Hex;
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
        Result := DecryptTwoFishCFB (Key^,
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

function DecryptTwoFishCFB (const aKey: Hex;
                            const anIV: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TTwoFish128Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToTwoFishIV (anIV);
        Result := DecryptTwoFishCFB (Key^,
                                     oSize*8,
                                     IV,
                                     aValue,
                                     aBlockSize);
    finally
        IV := NULL_TWOFISH_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;
{$ENDIF CFB}

{$IFDEF OFB}
procedure EncryptTwoFishOFB (var aKey;
                             const aKeySize: WORD;
                             aRandomFunc: TRandomFunction;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TTwoFishCtx;
    IV           : TTwoFish128Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { формируем псевдо-случайный 128-битный вектор инициализации }
        IV := GetRandomTwoFishIV (aRandomFunc);
        { инициализируем контекст }
        ErrorCode := TF_OFB_Init (aKey,aKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { записываем вектор инициализации в начало }
            Buffer := PChar ( TwoFishIVToHex (IV) );
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
                    ErrorCode := TF_OFB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_TWOFISH_128_BLOCK;
            TF_OFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_TWOFISH_OFB_ENCRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

procedure EncryptTwoFishOFB (var aKey;
                             const aKeySize: WORD;
                             const anIV: TTwoFish128Block;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TTwoFishCtx;
    IV           : TTwoFish128Block;
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
        ErrorCode := TF_OFB_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := TF_OFB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_TWOFISH_128_BLOCK;
            TF_OFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_TWOFISH_OFB_ENCRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

procedure DecryptTwoFishOFB (var aKey;
                             const aKeySize: WORD;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TTwoFishCtx;
    IV           : TTwoFish128Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { обнуляем вектор инициализации }
        IV := NULL_TWOFISH_128_BLOCK;
        { инициализируем контекст }
        ErrorCode := TF_OFB_Init (aKey,aKeySize,IV,ctx);
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
                    IV := HexToTwoFishIV ( StrPas (Buffer) );
                    ErrorCode := TF_OFB_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := TF_OFB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_TWOFISH_128_BLOCK;
            TF_OFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_TWOFISH_OFB_DECRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

procedure DecryptTwoFishOFB (var aKey;
                             const aKeySize: WORD;
                             const anIV: TTwoFish128Block;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TTwoFishCtx;
    IV           : TTwoFish128Block;
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
        ErrorCode := TF_OFB_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := TF_OFB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_TWOFISH_128_BLOCK;
            TF_OFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_TWOFISH_OFB_DECRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

function EncryptTwoFishOFB (var aKey;
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
            EncryptTwoFishOFB (aKey,aKeySize,aRandomFunc,Input,Output,aBlockSize);
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
                                   [ Format (ERR_TWOFISH_OFB_ENCRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

function EncryptTwoFishOFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TTwoFish128Block;
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
            EncryptTwoFishOFB (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_TWOFISH_OFB_ENCRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

function DecryptTwoFishOFB (var aKey;
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
            DecryptTwoFishOFB (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_TWOFISH_OFB_DECRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

function DecryptTwoFishOFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TTwoFish128Block;
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
            DecryptTwoFishOFB (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_TWOFISH_OFB_DECRYPT,[aKeySize,TWOFISH_VERSION]), E.Message ]);
    end;
end;

procedure EncryptTwoFishOFB (const aKey: Hex;
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
        EncryptTwoFishOFB (Key^,
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

procedure EncryptTwoFishOFB (const aKey: Hex;
                             const anIV: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TTwoFish128Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToTwoFishIV (anIV);
        EncryptTwoFishOFB (Key^,
                           oSize*8,
                           IV,
                           anInput,
                           anOutput,
                           aBlockSize);
    finally
        IV := NULL_TWOFISH_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

procedure DecryptTwoFishOFB (const aKey: Hex;
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
        DecryptTwoFishOFB (Key^,
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

procedure DecryptTwoFishOFB (const aKey: Hex;
                             const anIV: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TTwoFish128Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToTwoFishIV (anIV);
        DecryptTwoFishOFB (Key^,
                           oSize*8,
                           IV,
                           anInput,
                           anOutput,
                           aBlockSize);
    finally
        IV := NULL_TWOFISH_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function EncryptTwoFishOFB (const aKey: Hex;
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
        Result := EncryptTwoFishOFB (Key^,
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

function EncryptTwoFishOFB (const aKey: Hex;
                            const anIV: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TTwoFish128Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToTwoFishIV (anIV);
        Result := EncryptTwoFishOFB (Key^,
                                     oSize*8,
                                     IV,
                                     aValue,
                                     aBlockSize);
    finally
        IV := NULL_TWOFISH_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function DecryptTwoFishOFB (const aKey: Hex;
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
        Result := DecryptTwoFishOFB (Key^,
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

function DecryptTwoFishOFB (const aKey: Hex;
                            const anIV: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TTwoFish128Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToTwoFishIV (anIV);
        Result := DecryptTwoFishOFB (Key^,
                                     oSize*8,
                                     IV,
                                     aValue,
                                     aBlockSize);
    finally
        IV := NULL_TWOFISH_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;
{$ENDIF OFB}

function TWOFISH_VERSION : String;
var
    Version : TVersionInfo;
begin
    {$IFDEF EXTERNAL_LINK}
    Version := GetFileVersionInfo (TWOFISH_DLL);
    // Result := Format ('v.%s',[GET_TWOFISH_DLL_VERSION]);
    {$ELSE EXTERNAL_LINK}
    Version := StrToVersionInfo ('0.15.0.0');
    {$ENDIF EXTERNAL_LINK}
    Result := Format ('v.%d.%d.%d.%d',[Version.Major,
                                       Version.Minor,
                                       Version.Release,
                                       Version.Build]);
end;