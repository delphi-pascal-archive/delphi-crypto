{******************************************************************************}
{*  PRNG.int.inc                                                              *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
type
    PRandomFunction = ^TRandomFunction;
    TRandomFunction = function (const aMax: LongWord;
                                const aMin: LongWord = 0) : LongWord;

{$IFNDEF EXTERNAL_LINK}
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

function init_random : Boolean;
function get_random_taus88 : Extended;
function get_random_interval_taus88 (const aMax: LongWord = High (LongWord);
                                     const aMin: LongWord = 0) : LongWord;
function get_random_taus113 : Extended;
function get_random_interval_taus113 (const aMax: LongWord = High (LongWord);
                                      const aMin: LongWord = 0) : LongWord;
function get_random_kiss123 : Extended;
function get_random_interval_kiss123 (const aMax: LongWord = High (LongWord);
                                      const aMin: LongWord = 0) : LongWord;
function get_random_tt800 : Extended;
function get_random_interval_tt800 (const aMax: LongWord = High (LongWord);
                                    const aMin: LongWord = 0) : LongWord;
function get_random_xor4096 : Extended;
function get_random_interval_xor4096 (const aMax: LongWord = High (LongWord);
                                      const aMin: LongWord = 0) : LongWord;
function get_random_mt19937 : Extended;
function get_random_interval_mt19937 (const aMax: LongWord = High (LongWord);
                                      const aMin: LongWord = 0) : LongWord;
function get_random_aesr : Extended;
function get_random_interval_aesr (const aMax: LongWord = High (LongWord);
                                   const aMin: LongWord = 0) : LongWord;
function get_random_salsar : Extended;
function get_random_interval_salsar (const aMax: LongWord = High (LongWord);
                                     const aMin: LongWord = 0) : LongWord;
function get_random_isaac : Extended;
function get_random_interval_isaac (const aMax: LongWord = High (LongWord);
                                    const aMin: LongWord = 0) : LongWord;
{$ENDIF EXTERNAL_LINK}

{$IFDEF EXTERNAL_LINK}
const
    PRNG_DLL = 'prng/prng_dll.dll';
{ prng dll functions }
function init_random : Boolean; stdcall;
    external PRNG_DLL name 'init_random';
function get_random_taus88 : Extended; stdcall;
    external PRNG_DLL name 'get_random_taus88';
function get_random_interval_taus88 (const aMax: LongWord = High (LongWord);
                                     const aMin: LongWord = 0) : LongWord; stdcall;
    external PRNG_DLL name 'get_random_interval_taus88';
function get_random_taus113 : Extended; stdcall;
    external PRNG_DLL name 'get_random_taus113';
function get_random_interval_taus113 (const aMax: LongWord = High (LongWord);
                                      const aMin: LongWord = 0) : LongWord; stdcall;
    external PRNG_DLL name 'get_random_interval_taus113';
function get_random_kiss123 : Extended; stdcall;
    external PRNG_DLL name 'get_random_kiss123';
function get_random_interval_kiss123 (const aMax: LongWord = High (LongWord);
                                      const aMin: LongWord = 0) : LongWord; stdcall;
    external PRNG_DLL name 'get_random_interval_kiss123';
function get_random_tt800 : Extended; stdcall;
    external PRNG_DLL name 'get_random_tt800';
function get_random_interval_tt800 (const aMax: LongWord = High (LongWord);
                                    const aMin: LongWord = 0) : LongWord; stdcall;
    external PRNG_DLL name 'get_random_interval_tt800';
function get_random_xor4096 : Extended; stdcall;
    external PRNG_DLL name 'get_random_xor4096';
function get_random_interval_xor4096 (const aMax: LongWord = High (LongWord);
                                      const aMin: LongWord = 0) : LongWord; stdcall;
    external PRNG_DLL name 'get_random_interval_xor4096';
function get_random_mt19937 : Extended; stdcall;
    external PRNG_DLL name 'get_random_mt19937';
function get_random_interval_mt19937 (const aMax: LongWord = High (LongWord);
                                      const aMin: LongWord = 0) : LongWord; stdcall;
    external PRNG_DLL name 'get_random_interval_mt19937';
function get_random_aesr : Extended; stdcall;
    external PRNG_DLL name 'get_random_aesr';
function get_random_interval_aesr (const aMax: LongWord = High (LongWord);
                                   const aMin: LongWord = 0) : LongWord; stdcall;
    external PRNG_DLL name 'get_random_interval_aesr';
function get_random_salsar : Extended; stdcall;
    external PRNG_DLL name 'get_random_salsar';
function get_random_interval_salsar (const aMax: LongWord = High (LongWord);
                                     const aMin: LongWord = 0) : LongWord; stdcall;
    external PRNG_DLL name 'get_random_interval_salsar';
function get_random_isaac : Extended; stdcall;
    external PRNG_DLL name 'get_random_isaac';
function get_random_interval_isaac (const aMax: LongWord = High (LongWord);
                                    const aMin: LongWord = 0) : LongWord; stdcall;
    external PRNG_DLL name 'get_random_interval_isaac';
{$ENDIF EXTERNAL_LINK}

function RandomInit (const aValue: LongWord) : Boolean; overload;
function RandomInit : Boolean; overload;

function RandomSystem (const aMax: LongWord;
                       const aMin: LongWord = 0) : LongWord;
function RandomTaus88 (const aMax: LongWord;
                       const aMin: LongWord = 0) : LongWord;
function RandomTaus113 (const aMax: LongWord;
                        const aMin: LongWord = 0) : LongWord;
function RandomKiss123 (const aMax: LongWord;
                        const aMin: LongWord = 0) : LongWord;
function RandomTT800 (const aMax: LongWord;
                      const aMin: LongWord = 0) : LongWord;
function RandomXOR4096 (const aMax: LongWord;
                        const aMin: LongWord = 0) : LongWord;
function RandomMT19937 (const aMax: LongWord;
                        const aMin: LongWord = 0) : LongWord;
function RandomAES (const aMax: LongWord;
                    const aMin: LongWord = 0) : LongWord;
function RandomSalsa (const aMax: LongWord;
                      const aMin: LongWord = 0) : LongWord;
function RandomISAAC (const aMax: LongWord;
                      const aMin: LongWord = 0) : LongWord;
