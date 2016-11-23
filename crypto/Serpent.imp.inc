{******************************************************************************}
{*  Serpent.imp.inc                                                           *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
function SerpentIV (anArgs: array of const) : TSerpent128Block;
var
    I : Integer;
begin
    Result := NULL_SERPENT_128_BLOCK;
    if (  ( High (anArgs) = 0 ) and IsHex ( toString (anArgs [0]) )  ) then
    begin
        Result := HexToSerpentIV ( toString (anArgs [0]) );
        Exit;
    end;
    for I := 0 to High (anArgs) do
    begin
        Result [I] := toInteger (anArgs [I]);
        if ( I >= High (TSerpent128Block) ) then
            Break;
    end;
end;

function SerpentIVToHex (const aValue: TSerpent128Block) : Hex;
var
    I : Integer;
begin
    Result := '';
    for I := Low (TSerpent128Block) to High (TSerpent128Block) do
        Result := Result + IntToHex (aValue [I],2);
end;

function HexToSerpentIV (const aValue: Hex) : TSerpent128Block;
var
    I      : Integer;
    J      : Integer;
    Buffer : String [3];
begin
    Result := NULL_SERPENT_128_BLOCK;
    if not IsHex (aValue) then
        Exit;
    I := 0;
    J := 0;
    while ( I < Length (aValue) ) do
    begin
        Inc (I);
        if ( I mod 2 = 0 ) then
        begin
            if ( J > High (TSerpent128Block) )  then
                Break;
            Buffer := Format ('$%s%s',[ aValue [I-1], aValue [I] ]);
            Result [J] := StrToInt (Buffer);
            Inc (J);
        end;
    end;
end;

function GetRandomSerpentIV (aRandomFunc: TRandomFunction) : TSerpent128Block;
var
    I : Integer;
begin
    for I := Low (TSerpent128Block) to High (TSerpent128Block) do
        Result [I] := aRandomFunc (255);
end;

{$IFDEF ECB}
procedure EncryptSerpentECB (var aKey;
                             const aKeySize: WORD;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TSerpentCtx;
    ErrorCode    : Integer;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { инициализируем контекст }
        ErrorCode := SP_ECB_Init (aKey,aKeySize,ctx);
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
                    ErrorCode := SP_ECB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            SP_ECB_Reset (ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SERPENT_ECB_ENCRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

procedure DecryptSerpentECB (var aKey;
                             const aKeySize: WORD;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TSerpentCtx;
    ErrorCode    : Integer;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { инициализируем контекст }
        ErrorCode := SP_ECB_Init (aKey,aKeySize,ctx);
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
                    ErrorCode := SP_ECB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            SP_ECB_Reset (ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SERPENT_ECB_DECRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

function EncryptSerpentECB (var aKey;
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
            EncryptSerpentECB (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SERPENT_ECB_ENCRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

function DecryptSerpentECB (var aKey;
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
            DecryptSerpentECB (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SERPENT_ECB_DECRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

procedure EncryptSerpentECB (const aKey: Hex;
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
        EncryptSerpentECB (Key^,
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

procedure DecryptSerpentECB (const aKey: Hex;
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
        DecryptSerpentECB (Key^,
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

function EncryptSerpentECB (const aKey: Hex;
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
        Result := EncryptSerpentECB (Key^,
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

function DecryptSerpentECB (const aKey: Hex;
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
        Result := DecryptSerpentECB (Key^,
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
procedure EncryptSerpentCBC (var aKey;
                             const aKeySize: WORD;
                             aRandomFunc: TRandomFunction;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TSerpentCtx;
    IV           : TSerpent128Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { формируем псевдо-случайный 128-битный вектор инициализации }
        IV := GetRandomSerpentIV (aRandomFunc);
        { инициализируем контекст }
        ErrorCode := SP_CBC_Init (aKey,aKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { записываем вектор инициализации в начало }
            Buffer := PChar ( SerpentIVToHex (IV) );
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
                    ErrorCode := SP_CBC_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_SERPENT_128_BLOCK;
            SP_CBC_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SERPENT_CBC_ENCRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

procedure EncryptSerpentCBC (var aKey;
                             const aKeySize: WORD;
                             const anIV: TSerpent128Block;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TSerpentCtx;
    IV           : TSerpent128Block;
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
        ErrorCode := SP_CBC_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := SP_CBC_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_SERPENT_128_BLOCK;
            SP_CBC_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SERPENT_CBC_ENCRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

procedure DecryptSerpentCBC (var aKey;
                             const aKeySize: WORD;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TSerpentCtx;
    IV           : TSerpent128Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { обнуляем вектор инициализации }
        IV := NULL_SERPENT_128_BLOCK;
        { инициализируем контекст }
        ErrorCode := SP_CBC_Init (aKey,aKeySize,IV,ctx);
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
                    IV := HexToSerpentIV ( StrPas (Buffer) );
                    ErrorCode := SP_CBC_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := SP_CBC_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_SERPENT_128_BLOCK;
            SP_CBC_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SERPENT_CBC_DECRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

procedure DecryptSerpentCBC (var aKey;
                             const aKeySize: WORD;
                             const anIV: TSerpent128Block;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TSerpentCtx;
    IV           : TSerpent128Block;
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
        ErrorCode := SP_CBC_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := SP_CBC_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_SERPENT_128_BLOCK;
            SP_CBC_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SERPENT_CBC_DECRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

function EncryptSerpentCBC (var aKey;
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
            EncryptSerpentCBC (aKey,aKeySize,aRandomFunc,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SERPENT_CBC_ENCRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

function EncryptSerpentCBC (var aKey;
                            const aKeySize: WORD;
                            const anIV: TSerpent128Block;
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
            EncryptSerpentCBC (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SERPENT_CBC_ENCRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

function DecryptSerpentCBC (var aKey;
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
            DecryptSerpentCBC (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SERPENT_CBC_DECRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

function DecryptSerpentCBC (var aKey;
                            const aKeySize: WORD;
                            const anIV: TSerpent128Block;
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
            DecryptSerpentCBC (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SERPENT_CBC_DECRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

procedure EncryptSerpentCBC (const aKey: Hex;
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
        EncryptSerpentCBC (Key^,
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

procedure EncryptSerpentCBC (const aKey: Hex;
                             const anIV: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TSerpent128Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToSerpentIV (anIV);
        EncryptSerpentCBC (Key^,
                           oSize*8,
                           IV,
                           anInput,
                           anOutput,
                           aBlockSize);
    finally
        IV := NULL_SERPENT_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

procedure DecryptSerpentCBC (const aKey: Hex;
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
        DecryptSerpentCBC (Key^,
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

procedure DecryptSerpentCBC (const aKey: Hex;
                             const anIV: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TSerpent128Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToSerpentIV (anIV);
        DecryptSerpentCBC (Key^,
                           oSize*8,
                           IV,
                           anInput,
                           anOutput,
                           aBlockSize);
    finally
        IV := NULL_SERPENT_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function EncryptSerpentCBC (const aKey: Hex;
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
        Result := EncryptSerpentCBC (Key^,
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

function EncryptSerpentCBC (const aKey: Hex;
                            const anIV: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TSerpent128Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToSerpentIV (anIV);
        Result := EncryptSerpentCBC (Key^,
                                     oSize*8,
                                     IV,
                                     aValue,
                                     aBlockSize);
    finally
        IV := NULL_SERPENT_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function DecryptSerpentCBC (const aKey: Hex;
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
        Result := DecryptSerpentCBC (Key^,
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

function DecryptSerpentCBC (const aKey: Hex;
                            const anIV: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TSerpent128Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToSerpentIV (anIV);
        Result := DecryptSerpentCBC (Key^,
                                     oSize*8,
                                     IV,
                                     aValue,
                                     aBlockSize);
    finally
        IV := NULL_SERPENT_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;
{$ENDIF CBC}

{$IFDEF CFB}
procedure EncryptSerpentCFB (var aKey;
                             const aKeySize: WORD;
                             aRandomFunc: TRandomFunction;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TSerpentCtx;
    IV           : TSerpent128Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { формируем псевдо-случайный 128-битный вектор инициализации }
        IV := GetRandomSerpentIV (aRandomFunc);
        { инициализируем контекст }
        ErrorCode := SP_CFB_Init (aKey,aKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { записываем вектор инициализации в начало }
            Buffer := PChar ( SerpentIVToHex (IV) );
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
                    ErrorCode := SP_CFB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_SERPENT_128_BLOCK;
            SP_CFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SERPENT_CFB_ENCRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

procedure EncryptSerpentCFB (var aKey;
                             const aKeySize: WORD;
                             const anIV: TSerpent128Block;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TSerpentCtx;
    IV           : TSerpent128Block;
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
        ErrorCode := SP_CFB_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := SP_CFB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_SERPENT_128_BLOCK;
            SP_CFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SERPENT_CFB_ENCRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

procedure DecryptSerpentCFB (var aKey;
                             const aKeySize: WORD;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TSerpentCtx;
    IV           : TSerpent128Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { обнуляем вектор инициализации }
        IV := NULL_SERPENT_128_BLOCK;
        { инициализируем контекст }
        ErrorCode := SP_CFB_Init (aKey,aKeySize,IV,ctx);
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
                    IV := HexToSerpentIV ( StrPas (Buffer) );
                    ErrorCode := SP_CFB_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := SP_CFB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_SERPENT_128_BLOCK;
            SP_CFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SERPENT_CFB_DECRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

procedure DecryptSerpentCFB (var aKey;
                             const aKeySize: WORD;
                             const anIV: TSerpent128Block;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TSerpentCtx;
    IV           : TSerpent128Block;
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
        ErrorCode := SP_CFB_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := SP_CFB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_SERPENT_128_BLOCK;
            SP_CFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SERPENT_CFB_DECRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

function EncryptSerpentCFB (var aKey;
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
            EncryptSerpentCFB (aKey,aKeySize,aRandomFunc,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SERPENT_CFB_ENCRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

function EncryptSerpentCFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TSerpent128Block;
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
            EncryptSerpentCFB (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SERPENT_CFB_ENCRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

function DecryptSerpentCFB (var aKey;
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
            DecryptSerpentCFB (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SERPENT_CFB_DECRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

function DecryptSerpentCFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TSerpent128Block;
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
            DecryptSerpentCFB (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SERPENT_CFB_DECRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

procedure EncryptSerpentCFB (const aKey: Hex;
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
        EncryptSerpentCFB (Key^,
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

procedure EncryptSerpentCFB (const aKey: Hex;
                             const anIV: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TSerpent128Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToSerpentIV (anIV);
        EncryptSerpentCFB (Key^,
                           oSize*8,
                           IV,
                           anInput,
                           anOutput,
                           aBlockSize);
    finally
        IV := NULL_SERPENT_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

procedure DecryptSerpentCFB (const aKey: Hex;
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
        DecryptSerpentCFB (Key^,
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

procedure DecryptSerpentCFB (const aKey: Hex;
                             const anIV: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TSerpent128Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToSerpentIV (anIV);
        DecryptSerpentCFB (Key^,
                           oSize*8,
                           IV,
                           anInput,
                           anOutput,
                           aBlockSize);
    finally
        IV := NULL_SERPENT_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function EncryptSerpentCFB (const aKey: Hex;
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
        Result := EncryptSerpentCFB (Key^,
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

function EncryptSerpentCFB (const aKey: Hex;
                            const anIV: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TSerpent128Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToSerpentIV (anIV);
        Result := EncryptSerpentCFB (Key^,
                                     oSize*8,
                                     IV,
                                     aValue,
                                     aBlockSize);
    finally
        IV := NULL_SERPENT_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function DecryptSerpentCFB (const aKey: Hex;
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
        Result := DecryptSerpentCFB (Key^,
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

function DecryptSerpentCFB (const aKey: Hex;
                            const anIV: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TSerpent128Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToSerpentIV (anIV);
        Result := DecryptSerpentCFB (Key^,
                                     oSize*8,
                                     IV,
                                     aValue,
                                     aBlockSize);
    finally
        IV := NULL_SERPENT_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;
{$ENDIF CFB}

{$IFDEF OFB}
procedure EncryptSerpentOFB (var aKey;
                             const aKeySize: WORD;
                             aRandomFunc: TRandomFunction;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TSerpentCtx;
    IV           : TSerpent128Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { формируем псевдо-случайный 128-битный вектор инициализации }
        IV := GetRandomSerpentIV (aRandomFunc);
        { инициализируем контекст }
        ErrorCode := SP_OFB_Init (aKey,aKeySize,IV,ctx);
        if ( ErrorCode <> 0 ) then
            raise Exception.CreateFmt ('%d',[ErrorCode]);
        with anInput do
        try
            anInput.Position := 0;
            anOutput.Position := 0;
            { записываем вектор инициализации в начало }
            Buffer := PChar ( SerpentIVToHex (IV) );
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
                    ErrorCode := SP_OFB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_SERPENT_128_BLOCK;
            SP_OFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SERPENT_OFB_ENCRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

procedure EncryptSerpentOFB (var aKey;
                             const aKeySize: WORD;
                             const anIV: TSerpent128Block;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TSerpentCtx;
    IV           : TSerpent128Block;
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
        ErrorCode := SP_OFB_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := SP_OFB_Encrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_SERPENT_128_BLOCK;
            SP_OFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SERPENT_OFB_ENCRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

procedure DecryptSerpentOFB (var aKey;
                             const aKeySize: WORD;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TSerpentCtx;
    IV           : TSerpent128Block;
    ErrorCode    : Integer;
    Buffer       : PChar;
    InputBuffer  : PChar;
    OutputBuffer : PChar;
    L            : LongWord;
begin
    ErrorCode := 0;
    try
        { обнуляем вектор инициализации }
        IV := NULL_SERPENT_128_BLOCK;
        { инициализируем контекст }
        ErrorCode := SP_OFB_Init (aKey,aKeySize,IV,ctx);
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
                    IV := HexToSerpentIV ( StrPas (Buffer) );
                    ErrorCode := SP_OFB_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := SP_OFB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_SERPENT_128_BLOCK;
            SP_OFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SERPENT_OFB_DECRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

procedure DecryptSerpentOFB (var aKey;
                             const aKeySize: WORD;
                             const anIV: TSerpent128Block;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    ctx          : TSerpentCtx;
    IV           : TSerpent128Block;
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
        ErrorCode := SP_OFB_Init (aKey,aKeySize,IV,ctx);
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
                    ErrorCode := SP_OFB_Decrypt (InputBuffer,OutputBuffer,aBlockSize,ctx);
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

            IV := NULL_SERPENT_128_BLOCK;
            SP_OFB_Reset (IV,ctx);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',
                                   [ Format (ERR_SERPENT_OFB_DECRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

function EncryptSerpentOFB (var aKey;
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
            EncryptSerpentOFB (aKey,aKeySize,aRandomFunc,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SERPENT_OFB_ENCRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

function EncryptSerpentOFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TSerpent128Block;
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
            EncryptSerpentOFB (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SERPENT_OFB_ENCRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

function DecryptSerpentOFB (var aKey;
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
            DecryptSerpentOFB (aKey,aKeySize,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SERPENT_OFB_DECRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

function DecryptSerpentOFB (var aKey;
                            const aKeySize: WORD;
                            const anIV: TSerpent128Block;
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
            DecryptSerpentOFB (aKey,aKeySize,anIV,Input,Output,aBlockSize);
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
                                   [ Format (ERR_SERPENT_OFB_DECRYPT,[aKeySize,SERPENT_VERSION]), E.Message ]);
    end;
end;

procedure EncryptSerpentOFB (const aKey: Hex;
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
        EncryptSerpentOFB (Key^,
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

procedure EncryptSerpentOFB (const aKey: Hex;
                             const anIV: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TSerpent128Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToSerpentIV (anIV);
        EncryptSerpentOFB (Key^,
                           oSize*8,
                           IV,
                           anInput,
                           anOutput,
                           aBlockSize);
    finally
        IV := NULL_SERPENT_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

procedure DecryptSerpentOFB (const aKey: Hex;
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
        DecryptSerpentOFB (Key^,
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

procedure DecryptSerpentOFB (const aKey: Hex;
                             const anIV: Hex;
                             const anInput: TStream;
                             out anOutput: TStream;
                             const aBlockSize: LongWord = 16);
var
    Key   : PKey;
    oSize : WORD;
    IV    : TSerpent128Block;
begin
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToSerpentIV (anIV);
        DecryptSerpentOFB (Key^,
                           oSize*8,
                           IV,
                           anInput,
                           anOutput,
                           aBlockSize);
    finally
        IV := NULL_SERPENT_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function EncryptSerpentOFB (const aKey: Hex;
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
        Result := EncryptSerpentOFB (Key^,
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

function EncryptSerpentOFB (const aKey: Hex;
                            const anIV: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TSerpent128Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToSerpentIV (anIV);
        Result := EncryptSerpentOFB (Key^,
                                     oSize*8,
                                     IV,
                                     aValue,
                                     aBlockSize);
    finally
        IV := NULL_SERPENT_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;

function DecryptSerpentOFB (const aKey: Hex;
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
        Result := DecryptSerpentOFB (Key^,
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

function DecryptSerpentOFB (const aKey: Hex;
                            const anIV: Hex;
                            const aValue: String;
                            const aBlockSize: LongWord = 16) : String;
var
    Key   : PKey;
    oSize : WORD;
    IV    : TSerpent128Block;
begin
    Result := '';
    try
        oSize := Length (aKey) div 2;
        Key := AllocMem ( oSize * SizeOf (Byte) ); HexToKey (aKey,Key);
        IV := HexToSerpentIV (anIV);
        Result := DecryptSerpentOFB (Key^,
                                     oSize*8,
                                     IV,
                                     aValue,
                                     aBlockSize);
    finally
        IV := NULL_SERPENT_128_BLOCK;
        NULL_KEY ( Key, oSize*8 );
        Dispose (Key);
        Key := NIL;
        oSize := 0;
    end;
end;
{$ENDIF OFB}

function SERPENT_VERSION : String;
var
    Version : TVersionInfo;
begin
    {$IFDEF EXTERNAL_LINK}
    Version := GetFileVersionInfo (SERPENT_DLL);
    // Result := Format ('v.%s',[GET_SERPENT_DLL_VERSION]);
    {$ELSE EXTERNAL_LINK}
    Version := StrToVersionInfo ('0.12.0.0');
    {$ENDIF EXTERNAL_LINK}
    Result := Format ('v.%d.%d.%d.%d',[Version.Major,
                                       Version.Minor,
                                       Version.Release,
                                       Version.Build]);
end;