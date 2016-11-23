library prng_dll;
{******************************************************************************}
{*  Pseudo Random Number Generator                                            *}
{*  W.Ehrhardt prng library                                                   *}
{******************************************************************************}
uses
    Windows,
    SysUtils,
    bTypes,
    taus88,
    taus113,
    kiss123, 
    tt800,
    xor4096,
    mt19937,
    aes_type in '..\aes\aes_type.pas',
    aes_base in '..\aes\aes_base.pas',
    aes_encr in '..\aes\aes_encr.pas',
    aesr,
    salsa20 in '..\salsa\salsa20.pas',
    salsar,
    isaac;

{$R *.res}

threadvar
    ctx_taus88  : taus88_ctx;
    ctx_taus113 : taus113_ctx;
    ctx_kiss123 : kiss123_ctx;
    ctx_tt800   : tt800_ctx;
    ctx_xor4096 : xor4096_ctx;
    ctx_mt19937 : mt19937_ctx;
    ctx_aesr    : aesr_ctx;
    ctx_salsar  : salsar_ctx;
    ctx_isaac   : isaac_ctx;

function init_random : Boolean; stdcall;
begin
    Result := TRUE;

    taus88_init0 (ctx_taus88);
    Result := Result and taus88_selftest;

    taus113_init0 (ctx_taus113);
    Result := Result and taus113_selftest;

    kiss123_init0 (ctx_kiss123);
    Result := Result and kiss123_selftest;

    tt800_init0 (ctx_tt800);
    Result := Result and tt800_selftest;

    xor4096_init0 (ctx_xor4096);
    Result := Result and xor4096_selftest;

    mt19937_init0 (ctx_mt19937);
    Result := Result and mt19937_selftest;

    aesr_init0 (ctx_aesr);
    Result := Result and aesr_selftest;

    salsar_init0 (ctx_salsar);
    salsar_set_rounds (20);
    Result := Result and salsar_selftest;
    
    isaac_init0 (ctx_isaac);
    Result := Result and isaac_selftest;
end;

function final_random : Boolean; stdcall;
begin
    Result := init_random;
end;

function get_random_taus88 : Extended; stdcall;
begin
    Result := taus88_double (ctx_taus88);
end;

function get_random_interval_taus88 (const aMax: LongWord = High (LongWord);
                                      const aMin: LongWord = 0) : LongWord; stdcall;
begin
    Result := Round ( aMin + (aMax - aMin) * get_random_taus88 );
end;

function get_random_taus113 : Extended; stdcall;
begin
    Result := taus113_double (ctx_taus113);
end;

function get_random_interval_taus113 (const aMax: LongWord = High (LongWord);
                                      const aMin: LongWord = 0) : LongWord; stdcall;
begin
    Result := Round ( aMin + (aMax - aMin) * get_random_taus113 );
end;

function get_random_kiss123 : Extended; stdcall;
begin
    Result := kiss123_double (ctx_kiss123);
end;

function get_random_interval_kiss123 (const aMax: LongWord = High (LongWord);
                                      const aMin: LongWord = 0) : LongWord; stdcall;
begin
    Result := Round ( aMin + (aMax - aMin) * get_random_kiss123 );
end;

function get_random_tt800 : Extended; stdcall;
begin
    Result := tt800_double (ctx_tt800);
end;

function get_random_interval_tt800 (const aMax: LongWord = High (LongWord);
                                      const aMin: LongWord = 0) : LongWord; stdcall;
begin
    Result := Round ( aMin + (aMax - aMin) * get_random_tt800 );
end;

function get_random_xor4096 : Extended; stdcall;
begin
    Result := xor4096_double (ctx_xor4096);
end;

function get_random_interval_xor4096 (const aMax: LongWord = High (LongWord);
                                      const aMin: LongWord = 0) : LongWord; stdcall;
begin
    Result := Round ( aMin + (aMax - aMin) * get_random_xor4096 );
end;

function get_random_mt19937 : Extended; stdcall;
begin
    Result := mt19937_double (ctx_mt19937);
end;

function get_random_interval_mt19937 (const aMax: LongWord = High (LongWord);
                                      const aMin: LongWord = 0) : LongWord; stdcall;
begin
    Result := Round ( aMin + (aMax - aMin) * get_random_mt19937 );
end;

function get_random_aesr : Extended; stdcall;
begin
    Result := aesr_double (ctx_aesr);
end;

function get_random_interval_aesr (const aMax: LongWord = High (LongWord);
                                   const aMin: LongWord = 0) : LongWord; stdcall;
begin
    Result := Round ( aMin + (aMax - aMin) * get_random_aesr );
end;

function get_random_salsar : Extended; stdcall;
begin
    Result := salsar_double (ctx_salsar);
end;

function get_random_interval_salsar (const aMax: LongWord = High (LongWord);
                                     const aMin: LongWord = 0) : LongWord; stdcall;
begin
    Result := Round ( aMin + (aMax - aMin) * get_random_salsar );
end;

function get_random_isaac : Extended; stdcall;
begin
    Result := isaac_double (ctx_isaac);
end;

function get_random_interval_isaac (const aMax: LongWord = High (LongWord);
                                    const aMin: LongWord = 0) : LongWord; stdcall;
begin
    Result := Round ( aMin + (aMax - aMin) * get_random_isaac );
end;

var
    DllProc : Pointer = NIL;

procedure DLLEntryPoint (aReason: DWORD);
begin
    case aReason of
        DLL_PROCESS_ATTACH : init_random;
        DLL_PROCESS_DETACH : final_random;
        DLL_THREAD_ATTACH  : init_random;
        DLL_THREAD_DETACH  : final_random;
    end;
end;

exports
    init_random,
    get_random_taus88,
    get_random_interval_taus88,
    get_random_taus113,
    get_random_interval_taus113,
    get_random_kiss123,
    get_random_interval_kiss123,
    get_random_tt800,
    get_random_interval_tt800,
    get_random_xor4096,
    get_random_interval_xor4096,
    get_random_mt19937,
    get_random_interval_mt19937,
    get_random_aesr,
    get_random_interval_aesr,
    get_random_salsar,
    get_random_interval_salsar,
    get_random_isaac,
    get_random_interval_isaac;

begin
    if not Assigned (DllProc) then
    begin
        DllProc := @DLLEntryPoint;
        DllEntryPoint (DLL_PROCESS_ATTACH);
    end;
end.
