{******************************************************************************}
{*  TCrypto.imp.inc                                                           *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TCrypto }
class function TCrypto.GetClassName : String;
begin
    Result := CLS_TCRYPTO_NAME;
end;

class function TCrypto.GetClassVersion : TVersionInfo;
begin
    Result := inherited GetClassVersion;
end;

class function TCrypto.GetClassTable : String;
begin
    Result := 'crypto_settings';
end;

class function TCrypto.GetClassProperties : TProtoProperties;
begin
    Result := NIL;
    try
        Result := inherited GetClassProperties;
        with Result do
        begin
            Register ( TMetaProperty, ['alg_asymmetric',PRP_TCRYPTO_ALG_ASYMMETRIC,dtString,'RSA1024'],[prpStored] );
            Register ( TMetaProperty, ['alg_symmetric',PRP_TCRYPTO_ALG_SYMMETRIC,dtString,'AES256-Serpent256-TwoFish256'],[prpStored] );
            Register ( TMetaProperty, ['mode_symmetric',PRP_TCRYPTO_MODE_SYMMETRIC,dtString,'OFB'],[prpStored] );
            Register ( TMetaProperty, ['alg_hash',PRP_TCRYPTO_ALG_HASH,dtString,'SHA256'],[prpStored] );
            Register ( TMetaProperty, ['gen_random',PRP_TCRYPTO_GEN_RANDOM,dtString,'ISAAC'],[prpStored] );
        end;
    except on E: Exception do
        _raise (['GetClassProperties',ERR_TCRYPTO_GET_ClASS_PROPERTIES,E],
                ['{751EEDF0-A36C-404C-A226-E9F47BB5C0C7}']);
    end;
end;

constructor TCrypto.Create (const aDB: TSQLiteDatabase;
                            anArgs: array of const;
                            aMode: TObjectMode = []);
begin
    try
        inherited Create (aDB,anArgs,aMode);
        Mode := Mode - [objCreated];
        { второй параметр - асимметричный шифр }
        if notEmpty (1,anArgs) then
            algAsymmetric := toString (anArgs [1]);
        { третий параметр - симметричный шифр }
        if notEmpty (2,anArgs) then
            algSymmetric := toString (anArgs [2]);
        { четвертый параметр - режим симметричного шифра }
        if notEmpty (3,anArgs) then
            modeSymmetric := toString (anArgs [3]);
        { пятый параметр - функция хэширования }
        if notEmpty (4,anArgs) then
            algHash := toString (anArgs [4]);
        { шестой параметр - генератор псевдо-случайных чисел }
        if notEmpty (5,anArgs) then
            genRandom := toString (anArgs [5]);
        Mode := Mode + [objCreated];
    except on E: Exception do begin
        Mode := Mode + [objCreateFailed];
        _raise (['Create',ERR_TCRYPTO_CREATE,E],
                ['{AFCE91C4-8E53-4088-8C2A-22CB483E9460}']);
    end; end;
end;

destructor TCrypto.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TCRYPTO_DESTROY,E],
                ['{C04689DB-A498-4A51-8B67-8623E6735F5A}']);
    end;
end;

function TCrypto.Check : Boolean;
begin
    Result := TRUE;
    try
        Result := inherited Check;
        if Result then
        begin
            try
                if not TCryptoKernel.TestAsymmetric ([algAsymmetric]) then
                    Errors.Add (ERR_TCRYPTO_TEST_ASYMMETRIC);
                if not TCryptoKernel.TestSymmetric ([algSymmetric,modeSymmetric]) then
                    Errors.Add (ERR_TCRYPTO_TEST_SYMMETRIC);
                if not TCryptoKernel.TestHash ([algHash]) then
                    Errors.Add (ERR_TCRYPTO_TEST_HASH);
                if not ( TCryptoKernel.Random ([0,255,genRandom]) >= 0 ) then
                    Errors.Add (ERR_TCRYPTO_TEST_RANDOM);
            except on E: Exception do
                Errors.Add (E.Message);
            end;
            Result := Result and ( Errors.Count = 0 );
        end;
    except on E: Exception do
        _raise (['Check',ERR_TCRYPTO_CHECK,E],
                ['{ED3FDC83-0A80-4D0C-86D9-1AD9241618BE}']);
    end;
end;

function TCrypto.Random (const aMin: LongWord = 0;
                         const aMax: LongWord = High (LongWord)) : LongWord;
begin
    Result := 0;
    try
        Result := TCryptoKernel.Random ([aMin,aMax,genRandom]);
    except on E: Exception do
        _raise (['Random',ERR_TCRYPTO_RANDOM,E],
                ['{971A1F54-8ACC-420C-9BE9-A438C20288A5}']);
    end;
end;

function TCrypto.Hash (const aValue: String) : Hex;
begin
    Result := '';
    try
        Result := TCryptoKernel.Hash (aValue,[algHash]);
    except on E: Exception do
        _raise (['Hash',ERR_TCRYPTO_HASH,E],
                ['{AFB9A8E9-D200-4972-9B1A-49B4F7A143D4}']);
    end;
end;

function TCrypto.Hash (const aValue: String;
                       const aSalt: String;
                       const aCount: DWORD = 1024) : Hex;
begin
    Result := '';
    try
        Result := TCryptoKernel.Hash (aValue,aSalt,[algHash,aCount]);
    except on E: Exception do
        _raise (['Hash',ERR_TCRYPTO_HASH,E],
                ['{EECD7150-9F98-4288-B99F-69909B8941DA}']);
    end;
end;

function TCrypto.Hash (const aValue: TStream) : Hex;
begin
    Result := '';
    try
        Result := TCryptoKernel.Hash (aValue,[algHash]);
    except on E: Exception do
        _raise (['Hash',ERR_TCRYPTO_HASH,E],
                ['{6BCFD665-A21B-4E85-8DEA-6E0F6B47172E}']);
    end;
end;

function TCrypto.HMAC (const aValue: String;
                       const aKey: String) : Hex;
begin
    Result := '';
    try
        Result := TCryptoKernel.HMAC (aValue,aKey,[algHash]);
    except on E: Exception do
        _raise (['HMAC',ERR_TCRYPTO_HMAC,E],
                ['{7F3C523C-7937-4824-80CE-9BCCF959B5D2}']);
    end;
end;

function TCrypto.HMAC (const aValue: String;
                       const aKey: String;
                       const aSalt: String;
                       const aCount: DWORD = 1024) : Hex;
begin
    Result := '';
    try
        Result := TCryptoKernel.HMAC (aValue,aKey,aSalt,[algHash,aCount]);
    except on E: Exception do
        _raise (['HMAC',ERR_TCRYPTO_HMAC,E],
                ['{5CA550F7-B193-4AA9-98E1-E6F9D83FCDB7}']);
    end;
end;

procedure TCrypto.GenerateKey (out aKey: String);
var
    isBad : Boolean;
    I     : Integer;
begin
    aKey := '';
    try
        isBad := TRUE;
        while isBad do
        begin
            TCryptoKernel.GenerateKey (aKey,
                                       [algSymmetric,
                                        genRandom]);
            { некоторые гпсч зависают,
              возвращая вырожденный результат, т.е.
              выдают ключ из одинаковых символов }
            for I := 1 to Length (aKey) do
            begin
                isBad := isBad and ( aKey [I] = aKey [1] );
                if not isBad then
                    Break;
            end;
            if isBad then
                TCryptoKernel.InitRandom (  Random ( High (LongWord) )  );
        end;
    except on E: Exception do
        _raise (['GenerateKey',ERR_TCRYPTO_GENERATE_KEY,E],
                ['{C556C6EF-C064-4373-943D-6D6F69949012}']);
    end;
end;

function TCrypto.GenerateKey : String;
begin
    Result := '';
    try
        GenerateKey (Result);
    except on E: Exception do
        _raise (['GenerateKey',ERR_TCRYPTO_GENERATE_KEY,E],
                ['{E7F98FC9-8D5B-47C9-A29F-614DCA106F76}']);
    end;
end;

procedure TCrypto.GenerateKeys (out aPublicKey: String;
                                out aPrivateKey: String);
begin
    aPublicKey := '';
    aPrivateKey := '';
    try
        TCryptoKernel.GenerateKeys (aPublicKey,
                                    aPrivateKey,
                                    [algAsymmetric,
                                     genRandom]);
    except on E: Exception do
        _raise (['GenerateKeys',ERR_TCRYPTO_GENERATE_KEYS,E],
                ['{E91A1880-1F35-45A6-B82D-2C76A5507CF7}']);
    end;
end;

function TCrypto.Encrypt (const aValue: String;
                          const aKey: String) : String;
const
    MaxTryCount = 3;
var
    Success  : Boolean;
    TryCount : Integer;
begin
    Result := '';
    try
        TryCount := 0;
        if ( aValue <> '' ) then
        repeat try
            Success := TRUE;
            Inc (TryCount);
            Result := TCryptoKernel.Encrypt (aValue,aKey,
                                             [algSymmetric,modeSymmetric,
                                              genRandom]);
        except
            Success := FALSE;
            if ( TryCount >= MaxTryCount ) then raise;
        end until Success;
    except on E: Exception do
        _raise (['Encrypt',ERR_TCRYPTO_ENCRYPT_SYMMETRIC,E],
                ['{131B8DBA-3667-48E2-A104-267A514039F0}']);
    end;
end;

function TCrypto.Decrypt (const aValue: String;
                          const aKey: String) : String;
const
    MaxTryCount = 3;
var
    Success  : Boolean;
    TryCount : Integer;
begin
    Result := '';
    try
        TryCount := 0;
        if ( aValue <> '' ) then
        repeat try
            Success := TRUE;
            Inc (TryCount);
            Result := TCryptoKernel.Decrypt (aValue,aKey,
                                             [algSymmetric,modeSymmetric]);
        except
            Success := FALSE;
            if ( TryCount >= MaxTryCount ) then raise;
        end until Success;
    except on E: Exception do
        _raise (['Decrypt',ERR_TCRYPTO_DECRYPT_SYMMETRIC,E],
                ['{16269AC2-0A7B-4341-9979-1A9245ED5637}']);
    end;
end;

function TCrypto.Encrypt (const aValue: String;
                          const aPublicKey: String;
                          const aPrivateKey: String) : String;
const
    MaxTryCount = 3;
var
    Success  : Boolean;
    TryCount : Integer;
begin
    Result := '';
    try
        TryCount := 0;
        if ( aValue <> '' ) then
        repeat try
            Success := TRUE;
            Inc (TryCount);
            Result := TCryptoKernel.Encrypt (aValue,
                                             aPublicKey,
                                             aPrivateKey,
                                             [algAsymmetric,
                                              algSymmetric,modeSymmetric,
                                              algHash,
                                              genRandom]);
        except
            Success := FALSE;
            if ( TryCount >= MaxTryCount ) then raise;
        end until Success;
    except on E: Exception do
        _raise (['Encrypt',ERR_TCRYPTO_ENCRYPT_ASYMMETRIC,E],
                ['{E0A8E1A3-493B-4140-87B1-4A0987B9977E}']);
    end;
end;

function TCrypto.Decrypt (const aValue: String;
                          const aPublicKey: String;
                          const aPrivateKey: String) : String;
const
    MaxTryCount = 3;
var
    Success  : Boolean;
    TryCount : Integer;
begin
    Result := '';
    try
        TryCount := 0;
        if ( aValue <> '' ) then
        repeat try
            Success := TRUE;
            Inc (TryCount);
            Result := TCryptoKernel.Decrypt (aValue,
                                             aPublicKey,
                                             aPrivateKey,
                                             [algAsymmetric,
                                              algSymmetric,modeSymmetric,
                                              algHash]);
        except
            Success := FALSE;
            if ( TryCount >= MaxTryCount ) then raise;
        end until Success;
    except on E: Exception do
        _raise (['Decrypt',ERR_TCRYPTO_DECRYPT_ASYMMETRIC,E],
                ['{8CE54E3E-3EBB-4B9E-B7CA-48683AE87972}']);
    end;
end;

procedure TCrypto.Encrypt (const anInput: TStream;
                           out anOutput: TStream;
                           const aPublicKey: String;
                           const aPrivateKey: String);
const
    MaxTryCount = 3;
var
    Success  : Boolean;
    TryCount : Integer;
begin
    try
        TryCount := 0;
        if ( anInput.Size > 0 ) then
        repeat try
            Success := TRUE;
            Inc (TryCount);
            anInput.Position := 0;
            anOutput.Position := 0;
            TCryptoKernel.Encrypt (anInput,
                                   anOutput,
                                   aPublicKey,
                                   aPrivateKey,
                                   [algAsymmetric,
                                    algSymmetric,modeSymmetric,
                                    algHash,
                                    genRandom]);
        except
            Success := FALSE;
            if ( TryCount >= MaxTryCount ) then raise;
        end until Success;
    except on E: Exception do
        _raise (['Encrypt',ERR_TCRYPTO_ENCRYPT_ASYMMETRIC,E],
                ['{DAA7E8C8-6D73-47A6-85F6-09DEBA9C897D}']);
    end;
end;

procedure TCrypto.Decrypt (const anInput: TStream;
                           out anOutput: TStream;
                           const aPublicKey: String;
                           const aPrivateKey: String);
const
    MaxTryCount = 3;
var
    Success  : Boolean;
    TryCount : Integer;
begin
    try
        TryCount := 0;
        if ( anInput.Size > 0 ) then
        repeat try
            Success := TRUE;
            Inc (TryCount);
            anInput.Position := 0;
            anOutput.Position := 0;
            TCryptoKernel.Decrypt (anInput,
                                   anOutput,
                                   aPublicKey,
                                   aPrivateKey,
                                   [algAsymmetric,
                                    algSymmetric,modeSymmetric,
                                    algHash,
                                    genRandom]);
        except
            Success := FALSE;
            if ( TryCount >= MaxTryCount ) then raise;
        end until Success;
    except on E: Exception do
        _raise (['Decrypt',ERR_TCRYPTO_DECRYPT_ASYMMETRIC,E],
                ['{A9F50BB0-DB08-40D1-B38F-2BB59C35F8D0}']);
    end;
end;

function TCrypto.GetAlgAsymmetric : String;
begin
    try
        Result := PropAt [_alg_asymmetric].asString;
    except on E: Exception do
        _raise (['GetAlgAsymmetric',ERR_TCRYPTO_GET_ALG_ASYMMETRIC,E],
                ['{6D2C38F7-C3CB-4355-A38F-05DE4104A046}']);
    end;
end;

procedure TCrypto.SetAlgAsymmetric (const aValue: String);
begin
    try
        PropAt [_alg_asymmetric].asString := aValue;
    except on E: Exception do
        _raise (['SetAlgAsymmetric',ERR_TCRYPTO_SET_ALG_ASYMMETRIC,E],
                ['{9E1A56B3-3ACC-4D72-864A-0C960DE07BB3}']);
    end;
end;

function TCrypto.GetAlgSymmetric : String;
begin
    try
        Result := PropAt [_alg_symmetric].asString;
    except on E: Exception do
        _raise (['GetAlgSymmetric',ERR_TCRYPTO_GET_ALG_SYMMETRIC,E],
                ['{F1C64021-DDE2-4C68-8CD5-9552158B6F6C}']);
    end;
end;

procedure TCrypto.SetAlgSymmetric (const aValue: String);
begin
    try
        PropAt [_alg_symmetric].asString := aValue;
    except on E: Exception do
        _raise (['SetAlgSymmetric',ERR_TCRYPTO_SET_ALG_SYMMETRIC,E],
                ['{6C1A9F9F-3F1D-407A-9676-664013F5C86A}']);
    end;
end;

function TCrypto.GetModeSymmetric : String;
begin
    try
        Result := PropAt [_mode_symmetric].asString;
    except on E: Exception do
        _raise (['GetModeSymmetric',ERR_TCRYPTO_GET_MODE_SYMMETRIC,E],
                ['{65AB18E4-1B9D-410F-B204-AB4BF0A8E80C}']);
    end;
end;

procedure TCrypto.SetModeSymmetric (const aValue: String);
begin
    try
        PropAt [_mode_symmetric].asString := aValue;
    except on E: Exception do
        _raise (['SetModeSymmetric',ERR_TCRYPTO_SET_MODE_SYMMETRIC,E],
                ['{C5843CE4-3603-415D-8D62-A02DF9AE0EB7}']);
    end;
end;

function TCrypto.GetAlgHash : String;
begin
    try
        Result := PropAt [_alg_hash].asString;
    except on E: Exception do
        _raise (['GetAlgHash',ERR_TCRYPTO_GET_ALG_HASH,E],
                ['{D0E5C1D2-06CB-43AE-B508-263AE85D1F3F}']);
    end;
end;

procedure TCrypto.SetAlgHash (const aValue: String);
begin
    try
        PropAt [_alg_hash].asString := aValue;
    except on E: Exception do
        _raise (['SetAlgHash',ERR_TCRYPTO_SET_ALG_HASH,E],
                ['{38C9B33F-1232-461A-B9D7-679120A82315}']);
    end;
end;

function TCrypto.GetGenRandom : String;
begin
    try
        Result := PropAt [_gen_random].asString;
    except on E: Exception do
        _raise (['GetGenRandom',ERR_TCRYPTO_GET_GEN_RANDOM,E],
                ['{A5534A10-2F30-490C-9478-79301DDF6FD3}']);
    end;
end;

procedure TCrypto.SetGenRandom (const aValue: String);
begin
    try
        PropAt [_gen_random].asString := aValue;
    except on E: Exception do
        _raise (['SetGenRandom',ERR_TCRYPTO_SET_GEN_RANDOM,E],
                ['{6C9ED2B6-C9B5-440D-A0F1-ED5272182001}']);
    end;
end;
