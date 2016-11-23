{******************************************************************************}
{*  BlowFish.imp.inc                                                           *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
function BlowFishIV (anArgs: array of const) : TBlowFish64Block;
var
    I : Integer;
begin
    Result := NULL_BLOWFISH_64_BLOCK;
    if (  ( High (anArgs) = 0 ) and IsHex ( toString (anArgs [0]) )  ) then
    begin
        Result := HexToBlowFishIV ( toString (anArgs [0]) );
        Exit;
    end;
    for I := 0 to High (anArgs) do
    begin
        Result [I] := toInteger (anArgs [I]);
        if ( I >= High (TBlowFish64Block) ) then
            Break;
    end;
end;

function BlowFishIVToHex (const aValue: TBlowFish64Block) : Hex;
var
    I : Integer;
begin
    Result := '';
    for I := Low (TBlowFish64Block) to High (TBlowFish64Block) do
        Result := Result + IntToHex (aValue [I],2);
end;

function HexToBlowFishIV (const aValue: Hex) : TBlowFish64Block;
var
    I      : Integer;
    J      : Integer;
    Buffer : String [3];
begin
    Result := NULL_BLOWFISH_64_BLOCK;
    if not IsHex (aValue) then
        Exit;
    I := 0;
    J := 0;
    while ( I < Length (aValue) ) do
    begin
        Inc (I);
        if ( I mod 2 = 0 ) then
        begin
            if ( J > High (TBlowFish64Block) )  then
                Break;
            Buffer := Format ('$%s%s',[ aValue [I-1], aValue [I] ]);
            Result [J] := StrToInt (Buffer);
            Inc (J);
        end;
    end;
end;

function GetRandomBlowFishIV (aRandomFunc: TRandomFunction) : TBlowFish64Block;
var
    I : Integer;
begin
    for I := Low (TBlowFish64Block) to High (TBlowFish64Block) do
        Result [I] := aRandomFunc (255);
end;

{$IFDEF ECB}
procedure EncryptBlowFishECB (var aKey;
                              const aKeySize: WORD;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16);
var
    ctx          : TBlowFishCtx;
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
        ErrorCode := BF_ECB_Init (aKey,oKeySize,ctx);
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
                    ErrorCode := BF_ECB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            BF_ECB_Reset (ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_BLOWFISH_ECB_ENCRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

procedure DecryptBlowFishECB (var aKey;
                              const aKeySize: WORD;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16);
var
    ctx          : TBlowFishCtx;
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
        ErrorCode := BF_ECB_Init (aKey,oKeySize,ctx);
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
                    ErrorCode := BF_ECB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            BF_ECB_Reset (ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_BLOWFISH_ECB_DECRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

function EncryptBlowFishECB (var aKey;
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
            EncryptBlowFishECB (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_BLOWFISH_ECB_ENCRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

function DecryptBlowFishECB (var aKey;
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
            DecryptBlowFishECB (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_BLOWFISH_ECB_DECRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

procedure EncryptBlowFishECB (const aKey: Hex;
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
        EncryptBlowFishECB (Key^,
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

procedure DecryptBlowFishECB (const aKey: Hex;
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
        DecryptBlowFishECB (Key^,
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

function EncryptBlowFishECB (const aKey: Hex;
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
        Result := EncryptBlowFishECB (Key^,
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

function DecryptBlowFishECB (const aKey: Hex;
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
        Result := DecryptBlowFishECB (Key^,
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
procedure EncryptBlowFishCBC (var aKey;
                              const aKeySize: WORD;
                              aRandomFunc: TRandomFunction;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16);
var
    ctx          : TBlowFishCtx;
    IV           : TBlowFish64Block;
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
        { формируем псевдо-случайный 128-битный вектор инициализации }
        IV := GetRandomBlowFishIV (aRandomFunc);
        { инициализируем контекст }
        ErrorCode := BF_CBC_Init (aKey,oKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { записываем вектор инициализации в начало }
            Buffer := PChar ( BlowFishIVToHex (IV) );
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
                    ErrorCode := BF_CBC_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_BLOWFISH_64_BLOCK;
            BF_CBC_Reset (IV,ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_BLOWFISH_CBC_ENCRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

procedure EncryptBlowFishCBC (var aKey;
                              const aKeySize: WORD;
                              const anIV: TBlowFish64Block;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16);
var
    ctx          : TBlowFishCtx;
    IV           : TBlowFish64Block;
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
        ErrorCode := BF_CBC_Init (aKey,oKeySize,IV,ctx);
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
                    ErrorCode := BF_CBC_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_BLOWFISH_64_BLOCK;
            BF_CBC_Reset (IV,ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_BLOWFISH_CBC_ENCRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

procedure DecryptBlowFishCBC (var aKey;
                              const aKeySize: WORD;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16);
var
    ctx          : TBlowFishCtx;
    IV           : TBlowFish64Block;
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
        IV := NULL_BLOWFISH_64_BLOCK;
        { инициализируем контекст }
        ErrorCode := BF_CBC_Init (aKey,oKeySize,IV,ctx);
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
                    IV := HexToBlowFishIV ( StrPas (Buffer) );
                    ErrorCode := BF_CBC_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := BF_CBC_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_BLOWFISH_64_BLOCK;
            BF_CBC_Reset (IV,ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_BLOWFISH_CBC_DECRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

procedure DecryptBlowFishCBC (var aKey;
                              const aKeySize: WORD;
                              const anIV: TBlowFish64Block;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16);
var
    ctx          : TBlowFishCtx;
    IV           : TBlowFish64Block;
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
        ErrorCode := BF_CBC_Init (aKey,oKeySize,IV,ctx);
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
                    ErrorCode := BF_CBC_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_BLOWFISH_64_BLOCK;
            BF_CBC_Reset (IV,ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_BLOWFISH_CBC_DECRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

function EncryptBlowFishCBC (var aKey;
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
            EncryptBlowFishCBC (aKey,aKeySize,aRandomFunc,Input,Output,aBlockSize);
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
                                   [ Format (ERR_BLOWFISH_CBC_ENCRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

function EncryptBlowFishCBC (var aKey;
                             const aKeySize: WORD;
                             const anIV: TBlowFish64Block;
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
            EncryptBlowFishCBC (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_BLOWFISH_CBC_ENCRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

function DecryptBlowFishCBC (var aKey;
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
            DecryptBlowFishCBC (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_BLOWFISH_CBC_DECRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

function DecryptBlowFishCBC (var aKey;
                             const aKeySize: WORD;
                             const anIV: TBlowFish64Block;
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
            DecryptBlowFishCBC (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_BLOWFISH_CBC_DECRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

procedure EncryptBlowFishCBC (const aKey: Hex;
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
        EncryptBlowFishCBC (Key^,
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

procedure EncryptBlowFishCBC (const aKey: Hex;
                              const anIV: Hex;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TBlowFish64Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToBlowFishIV (anIV);
        EncryptBlowFishCBC (Key^,
                            oSize*8,
                            IV,
                            anInput,
                            anOutput,
                            aBlockSize);
    finally
        IV := NULL_BLOWFISH_64_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

procedure DecryptBlowFishCBC (const aKey: Hex;
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
        DecryptBlowFishCBC (Key^,
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

procedure DecryptBlowFishCBC (const aKey: Hex;
                              const anIV: Hex;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TBlowFish64Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToBlowFishIV (anIV);
        DecryptBlowFishCBC (Key^,
                            oSize*8,
                            IV,
                            anInput,
                            anOutput,
                            aBlockSize);
    finally
        IV := NULL_BLOWFISH_64_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function EncryptBlowFishCBC (const aKey: Hex;
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
        Result := EncryptBlowFishCBC (Key^,
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

function EncryptBlowFishCBC (const aKey: Hex;
                             const anIV: Hex;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TBlowFish64Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToBlowFishIV (anIV);
        Result := EncryptBlowFishCBC (Key^,
                                      oSize*8,
                                      IV,
                                      aValue,
                                      aBlockSize);
    finally
        IV := NULL_BLOWFISH_64_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function DecryptBlowFishCBC (const aKey: Hex;
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
        Result := DecryptBlowFishCBC (Key^,
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

function DecryptBlowFishCBC (const aKey: Hex;
                             const anIV: Hex;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TBlowFish64Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToBlowFishIV (anIV);
        Result := DecryptBlowFishCBC (Key^,
                                      oSize*8,
                                      IV,
                                      aValue,
                                      aBlockSize);
    finally
        IV := NULL_BLOWFISH_64_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;
{$ENDIF CBC}

{$IFDEF CFB}
procedure EncryptBlowFishCFB (var aKey;
                              const aKeySize: WORD;
                              aRandomFunc: TRandomFunction;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16);
var
    ctx          : TBlowFishCtx;
    IV           : TBlowFish64Block;
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
        { формируем псевдо-случайный 128-битный вектор инициализации }
        IV := GetRandomBlowFishIV (aRandomFunc);
        { инициализируем контекст }
        ErrorCode := BF_CFB_Init (aKey,oKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { записываем вектор инициализации в начало }
            Buffer := PChar ( BlowFishIVToHex (IV) );
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
                    ErrorCode := BF_CFB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_BLOWFISH_64_BLOCK;
            BF_CFB_Reset (IV,ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_BLOWFISH_CFB_ENCRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

procedure EncryptBlowFishCFB (var aKey;
                              const aKeySize: WORD;
                              const anIV: TBlowFish64Block;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16);
var
    ctx          : TBlowFishCtx;
    IV           : TBlowFish64Block;
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
        ErrorCode := BF_CFB_Init (aKey,oKeySize,IV,ctx);
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
                    ErrorCode := BF_CFB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_BLOWFISH_64_BLOCK;
            BF_CFB_Reset (IV,ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_BLOWFISH_CFB_ENCRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

procedure DecryptBlowFishCFB (var aKey;
                              const aKeySize: WORD;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16);
var
    ctx          : TBlowFishCtx;
    IV           : TBlowFish64Block;
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
        IV := NULL_BLOWFISH_64_BLOCK;
        { инициализируем контекст }
        ErrorCode := BF_CFB_Init (aKey,oKeySize,IV,ctx);
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
                    IV := HexToBlowFishIV ( StrPas (Buffer) );
                    ErrorCode := BF_CFB_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := BF_CFB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_BLOWFISH_64_BLOCK;
            BF_CFB_Reset (IV,ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_BLOWFISH_CFB_DECRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

procedure DecryptBlowFishCFB (var aKey;
                              const aKeySize: WORD;
                              const anIV: TBlowFish64Block;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16);
var
    ctx          : TBlowFishCtx;
    IV           : TBlowFish64Block;
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
        ErrorCode := BF_CFB_Init (aKey,oKeySize,IV,ctx);
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
                    ErrorCode := BF_CFB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_BLOWFISH_64_BLOCK;
            BF_CFB_Reset (IV,ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_BLOWFISH_CFB_DECRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

function EncryptBlowFishCFB (var aKey;
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
            EncryptBlowFishCFB (aKey,aKeySize,aRandomFunc,Input,Output,aBlockSize);
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
                                   [ Format (ERR_BLOWFISH_CFB_ENCRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

function EncryptBlowFishCFB (var aKey;
                             const aKeySize: WORD;
                             const anIV: TBlowFish64Block;
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
            EncryptBlowFishCFB (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_BLOWFISH_CFB_ENCRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

function DecryptBlowFishCFB (var aKey;
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
            DecryptBlowFishCFB (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_BLOWFISH_CFB_DECRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

function DecryptBlowFishCFB (var aKey;
                             const aKeySize: WORD;
                             const anIV: TBlowFish64Block;
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
            DecryptBlowFishCFB (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_BLOWFISH_CFB_DECRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

procedure EncryptBlowFishCFB (const aKey: Hex;
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
        EncryptBlowFishCFB (Key^,
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

procedure EncryptBlowFishCFB (const aKey: Hex;
                              const anIV: Hex;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TBlowFish64Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToBlowFishIV (anIV);
        EncryptBlowFishCFB (Key^,
                            oSize*8,
                            IV,
                            anInput,
                            anOutput,
                            aBlockSize);
    finally
        IV := NULL_BLOWFISH_64_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

procedure DecryptBlowFishCFB (const aKey: Hex;
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
        DecryptBlowFishCFB (Key^,
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

procedure DecryptBlowFishCFB (const aKey: Hex;
                              const anIV: Hex;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TBlowFish64Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToBlowFishIV (anIV);
        DecryptBlowFishCFB (Key^,
                            oSize*8,
                            IV,
                            anInput,
                            anOutput,
                            aBlockSize);
    finally
        IV := NULL_BLOWFISH_64_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function EncryptBlowFishCFB (const aKey: Hex;
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
        Result := EncryptBlowFishCFB (Key^,
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

function EncryptBlowFishCFB (const aKey: Hex;
                             const anIV: Hex;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TBlowFish64Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToBlowFishIV (anIV);
        Result := EncryptBlowFishCFB (Key^,
                                      oSize*8,
                                      IV,
                                      aValue,
                                      aBlockSize);
    finally
        IV := NULL_BLOWFISH_64_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function DecryptBlowFishCFB (const aKey: Hex;
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
        Result := DecryptBlowFishCFB (Key^,
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

function DecryptBlowFishCFB (const aKey: Hex;
                             const anIV: Hex;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TBlowFish64Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToBlowFishIV (anIV);
        Result := DecryptBlowFishCFB (Key^,
                                      oSize*8,
                                      IV,
                                      aValue,
                                      aBlockSize);
    finally
        IV := NULL_BLOWFISH_64_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;
{$ENDIF CFB}

{$IFDEF OFB}
procedure EncryptBlowFishOFB (var aKey;
                              const aKeySize: WORD;
                              aRandomFunc: TRandomFunction;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16);
var
    ctx          : TBlowFishCtx;
    IV           : TBlowFish64Block;
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
        { формируем псевдо-случайный 128-битный вектор инициализации }
        IV := GetRandomBlowFishIV (aRandomFunc);
        { инициализируем контекст }
        ErrorCode := BF_OFB_Init (aKey,oKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { записываем вектор инициализации в начало }
            Buffer := PChar ( BlowFishIVToHex (IV) );
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
                    ErrorCode := BF_OFB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_BLOWFISH_64_BLOCK;
            BF_OFB_Reset (IV,ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_BLOWFISH_OFB_ENCRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

procedure EncryptBlowFishOFB (var aKey;
                              const aKeySize: WORD;
                              const anIV: TBlowFish64Block;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16);
var
    ctx          : TBlowFishCtx;
    IV           : TBlowFish64Block;
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
        ErrorCode := BF_OFB_Init (aKey,oKeySize,IV,ctx);
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
                    ErrorCode := BF_OFB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_BLOWFISH_64_BLOCK;
            BF_OFB_Reset (IV,ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_BLOWFISH_OFB_ENCRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

procedure DecryptBlowFishOFB (var aKey;
                              const aKeySize: WORD;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16);
var
    ctx          : TBlowFishCtx;
    IV           : TBlowFish64Block;
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
        IV := NULL_BLOWFISH_64_BLOCK;
        { инициализируем контекст }
        ErrorCode := BF_OFB_Init (aKey,oKeySize,IV,ctx);
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
                    IV := HexToBlowFishIV ( StrPas (Buffer) );
                    ErrorCode := BF_OFB_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := BF_OFB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_BLOWFISH_64_BLOCK;
            BF_OFB_Reset (IV,ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_BLOWFISH_OFB_DECRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

procedure DecryptBlowFishOFB (var aKey;
                              const aKeySize: WORD;
                              const anIV: TBlowFish64Block;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16);
var
    ctx          : TBlowFishCtx;
    IV           : TBlowFish64Block;
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
        ErrorCode := BF_OFB_Init (aKey,oKeySize,IV,ctx);
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
                    ErrorCode := BF_OFB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_BLOWFISH_64_BLOCK;
            BF_OFB_Reset (IV,ctx);
            oKeySize := 0;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_BLOWFISH_OFB_DECRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

function EncryptBlowFishOFB (var aKey;
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
            EncryptBlowFishOFB (aKey,aKeySize,aRandomFunc,Input,Output,aBlockSize);
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
                                   [ Format (ERR_BLOWFISH_OFB_ENCRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

function EncryptBlowFishOFB (var aKey;
                             const aKeySize: WORD;
                             const anIV: TBlowFish64Block;
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
            EncryptBlowFishOFB (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_BLOWFISH_OFB_ENCRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

function DecryptBlowFishOFB (var aKey;
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
            DecryptBlowFishOFB (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_BLOWFISH_OFB_DECRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

function DecryptBlowFishOFB (var aKey;
                             const aKeySize: WORD;
                             const anIV: TBlowFish64Block;
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
            DecryptBlowFishOFB (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_BLOWFISH_OFB_DECRYPT,[aKeySize,BLOWFISH_VERSION]), E.Message ]);
    end;
end;

procedure EncryptBlowFishOFB (const aKey: Hex;
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
        EncryptBlowFishOFB (Key^,
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

procedure EncryptBlowFishOFB (const aKey: Hex;
                              const anIV: Hex;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TBlowFish64Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToBlowFishIV (anIV);
        EncryptBlowFishOFB (Key^,
                            oSize*8,
                            IV,
                            anInput,
                            anOutput,
                            aBlockSize);
    finally
        IV := NULL_BLOWFISH_64_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

procedure DecryptBlowFishOFB (const aKey: Hex;
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
        DecryptBlowFishOFB (Key^,
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

procedure DecryptBlowFishOFB (const aKey: Hex;
                              const anIV: Hex;
                              const anInput: TStream;
                              out anOutput: TStream;
                              const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TBlowFish64Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToBlowFishIV (anIV);
        DecryptBlowFishOFB (Key^,
                            oSize*8,
                            IV,
                            anInput,
                            anOutput,
                            aBlockSize);
    finally
        IV := NULL_BLOWFISH_64_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function EncryptBlowFishOFB (const aKey: Hex;
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
        Result := EncryptBlowFishOFB (Key^,
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

function EncryptBlowFishOFB (const aKey: Hex;
                             const anIV: Hex;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TBlowFish64Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToBlowFishIV (anIV);
        Result := EncryptBlowFishOFB (Key^,
                                      oSize*8,
                                      IV,
                                      aValue,
                                      aBlockSize);
    finally
        IV := NULL_BLOWFISH_64_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function DecryptBlowFishOFB (const aKey: Hex;
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
        Result := DecryptBlowFishOFB (Key^,
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

function DecryptBlowFishOFB (const aKey: Hex;
                             const anIV: Hex;
                             const aValue: String;
                             const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TBlowFish64Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToBlowFishIV (anIV);
        Result := DecryptBlowFishOFB (Key^,
                                      oSize*8,
                                      IV,
                                      aValue,
                                      aBlockSize);
    finally
        IV := NULL_BLOWFISH_64_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;
{$ENDIF OFB}

function BLOWFISH_VERSION : String;
var
    Version : TVersionInfo;
begin
    {$IFDEF EXTERNAL_LINK}
    Version := GetFileVersionInfo (BLOWFISH_DLL);
    // Result := Format ('v.%s',[GET_BLOWFISH_DLL_VERSION]);
    {$ELSE EXTERNAL_LINK}
    Version := StrToVersionInfo ('0.14.0.0');
    {$ENDIF EXTERNAL_LINK}
    Result := Format ('v.%d.%d.%d.%d',[Version.Major,
                                       Version.Minor,
                                       Version.Release,
                                       Version.Build]);
end;