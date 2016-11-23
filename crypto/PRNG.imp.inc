{******************************************************************************}
{*  PRNG.imp.inc                                                              *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{$IFNDEF EXTERNAL_LINK}
function init_random : Boolean;
begin
    Result := FALSE;
    try
        randomize;
        taus88_init0 (ctx_taus88);
        taus113_init0 (ctx_taus113);
        kiss123_init0 (ctx_kiss123);
        tt800_init0 (ctx_tt800);
        xor4096_init0 (ctx_xor4096);
        mt19937_init0 (ctx_mt19937);
        aesr_init0 (ctx_aesr);
        salsar_init0 (ctx_salsar);
        salsar_set_rounds (20);
        isaac_init0 (ctx_isaac);
        Result := TRUE;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',[ERR_RANDOM_INIT,E.Message]);
    end;
end;

function get_random_taus88 : Extended;
begin
    Result := taus88_double (ctx_taus88);
end;

function get_random_interval_taus88 (const aMax: LongWord = High (LongWord);
                                     const aMin: LongWord = 0) : LongWord;
begin
    Result := Round ( aMin + (aMax - aMin) * get_random_taus88 );
end;

function get_random_taus113 : Extended;
begin
    Result := taus113_double (ctx_taus113);
end;

function get_random_interval_taus113 (const aMax: LongWord = High (LongWord);
                                      const aMin: LongWord = 0) : LongWord;
begin
    Result := Round ( aMin + (aMax - aMin) * get_random_taus113 );
end;

function get_random_kiss123 : Extended;
begin
    Result := kiss123_double (ctx_kiss123);
end;

function get_random_interval_kiss123 (const aMax: LongWord = High (LongWord);
                                      const aMin: LongWord = 0) : LongWord;
begin
    Result := Round ( aMin + (aMax - aMin) * get_random_kiss123 );
end;

function get_random_tt800 : Extended;
begin
    Result := tt800_double (ctx_tt800);
end;

function get_random_interval_tt800 (const aMax: LongWord = High (LongWord);
                                    const aMin: LongWord = 0) : LongWord;
begin
    Result := Round ( aMin + (aMax - aMin) * get_random_tt800 );
end;

function get_random_xor4096 : Extended;
begin
    Result := xor4096_double (ctx_xor4096);
end;

function get_random_interval_xor4096 (const aMax: LongWord = High (LongWord);
                                      const aMin: LongWord = 0) : LongWord;
begin
    Result := Round ( aMin + (aMax - aMin) * get_random_xor4096 );
end;

function get_random_mt19937 : Extended;
begin
    Result := mt19937_double (ctx_mt19937);
end;

function get_random_interval_mt19937 (const aMax: LongWord = High (LongWord);
                                      const aMin: LongWord = 0) : LongWord;
begin
    Result := Round ( aMin + (aMax - aMin) * get_random_mt19937 );
end;

function get_random_aesr : Extended;
begin
    Result := aesr_double (ctx_aesr);
end;

function get_random_interval_aesr (const aMax: LongWord = High (LongWord);
                                   const aMin: LongWord = 0) : LongWord;
begin
    Result := Round ( aMin + (aMax - aMin) * get_random_aesr );
end;

function get_random_salsar : Extended;
begin
    Result := salsar_double (ctx_salsar);
end;

function get_random_interval_salsar (const aMax: LongWord = High (LongWord);
                                     const aMin: LongWord = 0) : LongWord;
begin
    Result := Round ( aMin + (aMax - aMin) * get_random_salsar );
end;

function get_random_isaac : Extended;
begin
    Result := isaac_double (ctx_isaac);
end;

function get_random_interval_isaac (const aMax: LongWord = High (LongWord);
                                    const aMin: LongWord = 0) : LongWord;
begin
    Result := Round ( aMin + (aMax - aMin) * get_random_isaac );
end;
{$ENDIF EXTERNAL_LINK}

function RandomInit (const aValue: LongWord) : Boolean;
begin
    Result := FALSE;
    try
        randomize;
        random (aValue);
        taus88_init (ctx_taus88,aValue);
        taus113_init (ctx_taus113,aValue);
        kiss123_init (ctx_kiss123,aValue);
        tt800_init (ctx_tt800,aValue);
        xor4096_init (ctx_xor4096,aValue);
        mt19937_init (ctx_mt19937,aValue);
        aesr_init (ctx_aesr,aValue);
        salsar_init (ctx_salsar,aValue);
        salsar_set_rounds (20);
        isaac_init (ctx_isaac,aValue);
        Result := TRUE;
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',[ERR_RANDOM_INIT,E.Message]);
    end;
end;

function RandomInit : Boolean;
begin
    Result := init_random;
end;

function RandomSystem (const aMax: LongWord;
                       const aMin: LongWord = 0) : LongWord;
begin
    try
        Result := Round ( aMin + (aMax-aMin) * random );
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',[ERR_RANDOM_SYSTEM,E.Message]);
    end;
end;

function RandomTaus88 (const aMax: LongWord;
                       const aMin: LongWord = 0) : LongWord;
begin
    try
        Result := get_random_interval_taus88 (aMax,aMin);
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',[ERR_RANDOM_TAUS88,E.Message]);
    end;
end;

function RandomTaus113 (const aMax: LongWord;
                        const aMin: LongWord = 0) : LongWord;
begin
    try
        Result := get_random_interval_taus113 (aMax,aMin);
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',[ERR_RANDOM_TAUS113,E.Message]);
    end;
end;

function RandomKiss123 (const aMax: LongWord;
                        const aMin: LongWord = 0) : LongWord;
begin
    try
        Result := get_random_interval_kiss123 (aMax,aMin);
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',[ERR_RANDOM_KISS123,E.Message]);
    end;
end;

function RandomTT800 (const aMax: LongWord;
                      const aMin: LongWord = 0) : LongWord;
begin
    try
        Result := get_random_interval_tt800 (aMax,aMin);
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',[ERR_RANDOM_TT800,E.Message]);
    end;
end;

function RandomXOR4096 (const aMax: LongWord;
                        const aMin: LongWord = 0) : LongWord;
begin
    try
        Result := get_random_interval_xor4096 (aMax,aMin);
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',[ERR_RANDOM_XOR4096,E.Message]);
    end;
end;

function RandomMT19937 (const aMax: LongWord;
                        const aMin: LongWord = 0) : LongWord;
begin
    try
        Result := get_random_interval_mt19937 (aMax,aMin);
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',[ERR_RANDOM_MT19937,E.Message]);
    end;
end;

function RandomAES (const aMax: LongWord;
                    const aMin: LongWord = 0) : LongWord;
begin
    try
        Result := get_random_interval_aesr (aMax,aMin);
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',[ERR_RANDOM_AES,E.Message]);
    end;
end;

function RandomSalsa (const aMax: LongWord;
                      const aMin: LongWord = 0) : LongWord;
begin
    try
        Result := get_random_interval_salsar (aMax,aMin);
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',[ERR_RANDOM_SALSA,E.Message]);
    end;
end;

function RandomISAAC (const aMax: LongWord;
                      const aMin: LongWord = 0) : LongWord;
begin
    try
        Result := get_random_interval_isaac (aMax,aMin);
    except on E: Exception do
        raise Exception.CreateFmt ('%s: %s',[ERR_RANDOM_ISAAC,E.Message]);
    end;
end;