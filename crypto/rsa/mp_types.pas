unit mp_types;

{MPArith type definitions and constants}

interface

{$i STD.INC}

{$ifdef HAS_ASSERT}
{$C+}     {Turn assertions on for critical conditions in initialization}
{$endif}

(*************************************************************************

 DESCRIPTION     :  MPArith type definitions and constants

 REQUIREMENTS    :  TP 7, D1-D7/D9-D10/D12, FPC, VP

 EXTERNAL DATA   :  ----

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  [1]  LibTomMath 0.30+ by Tom St Denis
                    [2]  MPI, M.J. Fromberger

 Version  Date      Author      Modification (except MP_VERSION changes)
 -------  --------  -------     ------------------------------------------
 0.01     09.05.04  W.Ehrhardt  Initial version: BP7
 0.02     22.07.04  we          Add LibTomMath types/consts
 0.10     26.07.04  we          additional MPI constants
 0.11     27.07.04  we          InitCheck variable
 0.12     01.08.04  we          mp_show_plus variable
 0.13     01.08.04  we          mp_uppercase, MAXRadix, InitCheck->mp_argchk
 0.14     01.08.04  we          MP_RTE_xx constants
 0.15     02.08.04  we          mp_mul_cutoff
 0.16     02.08.04  we          MP_WARRAY removed (too small for 8/16 bit)
 0.17     04.08.04  we          mp_precision (8: minimum allocation unit)
 0.18     04.08.04  we          mp_sqr_cutoff
 0.19     05.08.04  we          32Bit (cardinal/int64) for D4+
 0.20     07.08.04  we          force DIGIT_BIT >= 8 if not MP_8BIT
 0.21     14.08.04  we          WEXP_ constants

 0.3.00   26.08.04  we          "release" version 0.3
 0.3.01   26.08.04  we          mp_error, mp_errchk variables
 0.3.02   30.08.04  we          type mp_string
 0.3.03   14.11.04  we          diagnostic types moved to implementation
 0.3.04   16.02.05  we          MPBITS for Montgomery
 0.3.05   27.02.05  we          var mp_memstat : TMemStatus;
 0.3.06   16.05.05  we          FPC: $mode objfpc
 0.3.07   07.08.05  we          mp_diagcounter: general diagnostic counter

 0.4.00   20.08.05  we          mp_set_error, $debug: mp_error as function
 0.4.01   21.08.05  we          WIN32: threadvar mp_error/mp_err
 0.4.02   21.08.05  we          mp_conf/MPC_Diagnostic related changes
 0.4.03   21.08.05  we          assert procedure, removed MPBITS
 0.4.04   22.08.05  we          removed mp_argcheck, mp_errchk
 0.4.05   23.08.05  we          MaxDigits doubled for 16/32 bit
 0.4.06   24.08.05  we          assert 386 or better
 0.4.07   28.08.05  we          usage of exceptions implemented
 0.4.08   21.09.05  we          boolean mp_clearzero

 0.5.00   29.09.05  we          Separate Karatsuba cutoffs for BIT32/BIT16

 0.6.00   30.12.05  we          MP_8BIT removed
 0.6.01   01.01.06  we          WEXP_TABSIZE = 2 ^ WEXP_MAX
 0.6.02   12.03.06  we          MAXPrecision, MAXAlloc

 0.7.00   19.03.06  we          assert DIGIT_BIT+16 < 8*sizeof(mp_word)
 0.7.01   11.08.06  we          $ifdef MPC_UseExceptions: don't use MP_RTE_ constants
 0.7.02   12.08.06  we          mp_diagctr: diagnostic counter array
 0.7.03   28.08.06  we          MP_MAXBIT: Maximum possible bit number

 0.9.00   03.01.07  we          TRadixCMap, mp_lcrmap, mp_ucrmap

 1.0.00   11.04.07  we          types mp_rat, pmp_rat
 1.0.01   29.04.07  we          mp_roundfloat: round float representation of mp_rat
 1.0.02   13.05.07  we          Forced assertions on in initialization
                                MPAF prefix in assert strings

 1.2.00   05.09.07  we          MP_32BIT: assert DIGIT_BIT >= 16
 1.2.01   10.09.07  we          MP_INV_MASK

 1.3.00   03.11.07  we          mp_/MAXPrecision renamed to mp_/MAXAllocPrec
 1.3.01   04.11.07  we          MP_OVERFLOW, MP_RTE_OVRFLOW, MPXOverflow
 1.3.02   09.12.07  we          types mp_float, pmp_float
 1.3.03   17.12.07  we          consts MPF_MIN_PREC/MPF_MAX_PREC
 1.3.04   23.12.07  we          assert MPF_MAX_PREC <= 200000

 1.4.00   10.01.08  we          MPF_MAX_PREC = MP_MAXBIT div 4, assert <= 124000

 1.7.00   15.09.08  we          mp_allocprec moved to mp_base/implementation
 1.7.01   24.09.08  we          string replaced by mp_string

 1.8.00   11.10.08  we          Separate Karatsuba cutoffs for 32/32 and 32/16 bit code
 1.8.01   26.10.08  we          MAX_DiagCTR = 5

 1.9.00   02.12.08  we          Uses BTypes: char8, pchar8
 1.9.01   02.01.09  we          mp_sqrt_cutoff: Karatsuba square root cutoff
 1.9.02   04.01.09  we          default mp_sqrt_cutoff set to 4
 1.9.03   06.01.09  we          Removed legacy mp_diagcounter
 1.9.04   06.01.09  we          Removed CHAR_BIT

 1.11.00  07.03.09  we          Burnikel-Ziegler cutoff
 1.11.01  08.03.09  we          Burnikel-Ziegler cutoffs in digits
 1.11.02  14.03.09  we          mp_t3m_cutoff, mp_t3s_cutoff
 1.11.03  29.03.09  we          MAX_DiagCTR = 7

 1.12.00  20.06.09  we          Trace output routines and variables, mp_int2str
 1.12.01  05.07.09  we          D12 fixes in mp_trace

 1.14.00  13.02.10  we          MPC_MAXRadix64 adjustments

 1.15.00  08.05.10  we          MP_ShortVERS
 1.15.01  13.05.10  we          mp_fract_sep, mp_arg_sep

 1.16.00  13.06.10  we          MAX_TCBITS from mp_rconp
 1.16.01  13.06.10  we          Uses MPC_FPrec30K

**************************************************************************)

(*-------------------------------------------------------------------------
  This code uses material/ideas from the following 3rd party libraries:
   - LibTomMath 0.30+ by Tom St Denis
   - MPI 1.8.6 by Michael J. Fromberger
  See the file '3rdparty.mpa' for the licenses.
----------------------------------------------------------------------------*)

(*-------------------------------------------------------------------------
 (C) Copyright 2004-2011 Wolfgang Ehrhardt

 This software is provided 'as-is', without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.

 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it
 freely, subject to the following restrictions:

 1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software in
    a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

 2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

 3. This notice may not be removed or altered from any source distribution.
----------------------------------------------------------------------------*)

{$i mp_conf.inc}

uses
  {$ifdef MPC_UseExceptions}
    sysutils,
  {$endif}
  BTypes;


(* An "mp_digit" must be able to hold DIGIT_BIT + 1 bits
 * An "mp_word" must be able to hold 2*DIGIT_BIT + 1 bits
 * DIGIT_BIT must be at least 8
 *)


{$ifdef MP_32BIT}
type
  mp_digit  = cardinal;           {type that holds an MP digit   }
  mp_word   = int64;              {type that holds two MP digits }
const
  DIGIT_BIT = 30;                 {number of bits of an MP digit }
  DIGBITSTR = '(30/32 bit)';      {(version) info about bit sizes}
  MAXDigits = 16000;              {max number of mp_int digits   }
{$else}
type
  mp_digit  = word;               {type that holds an MP digit   }
  mp_word   = longint;            {type that holds two MP digits }
const
  DIGIT_BIT = 15;                 {number of bits of an MP digit }
  DIGBITSTR = '(15/16 bit)';      {(version) info about bit sizes}
  MAXDigits = 32000;              {max number of mp_int digits   }
{$endif}


const
  MP_ShortVERS  = '1.17.06';      {Short version number string}

const
  MP_VERSION    = MP_ShortVERS + ' ' + DIGBITSTR;  {external MP version string}

var
  mp_mul_cutoff : word;    {Karatsuba multiplication cutoff }
  mp_sqr_cutoff : word;    {Karatsuba square cutoff         }
  mp_sqrt_cutoff: word;    {Karatsuba square root cutoff    }
  mp_bz_cutoff  : word;    {Burnikel-Ziegler division cutoff}
  mp_t3m_cutoff : word;    {Toom-3 multiplication cutoff    }
  mp_t3s_cutoff : word;    {Toom-3 square cutoff            }

var
  mp_fract_sep  : char8;   {Separator between integer and fractional part, default '.'}
  mp_arg_sep    : char8;   {Separator between arguments in parser units, default ','}

var
  mp_show_plus  : boolean; {default false: if true include "+" in ascii string }
  mp_uppercase  : boolean; {default false: uppercase chars for radix 11 .. 36  }
  mp_roundfloat : boolean; {default false: round float representation of mp_rat}
  mp_clearzero  : boolean; {default false: clear memory to 0 before freemem.   }
                           {Note: 32 bit realloc may NOT clear the old memory! }

const
  MP_DIGIT_BIT  = DIGIT_BIT;    {Alias for DIGIT_BIT   }
  MP_DIG1       = mp_digit(1);  {Const 1 as mp_digit   }
  MP_MASK       = mp_digit(MP_DIG1 shl DIGIT_BIT - MP_DIG1);  {Mask for mp_digits in base type}
  MP_INV_MASK   = not MP_MASK;  {inverted digit mask}
  MP_DIGIT_MAX  = MP_MASK;      {largest mp_digit}
  MP_MAXBIT     = longint(MAXDigits)*MP_DIGIT_BIT;   {Maximum possible bit number}

  MP_LT         = -1;  {constant for "less than"    }
  MP_EQ         =  0;  {constant for "equal to"     }
  MP_GT         =  1;  {constant for "greater than" }

  MP_ZPOS       =  0;  {constant for "positive"}
  MP_NEG        =  1;  {constant for "negative"}

  MP_OKAY       =  0;  {no error at all       }
  MP_MEM        = -2;  {out of memory         }
  MP_RANGE      = -3;  {argument out of range }
  MP_BADARG     = -4;  {invalid parameter     }
  MP_UNDEF      = -5;  {answer is undefined   }
  MP_MAXDIGITS  = -6;  {MaxDigits exceeded    }
  MP_NOTINIT    = -7;  {not initialized       }
  MP_OVERFLOW   = -8;  {(float) overflow      }

const
  MP_MAGIC   = $BAF5;  {Magic number for mp_int magic field}

{$ifdef MPC_MAXRadix64}
const
  MAXRadix   = 64;     {max radix for conversion}
type
  TRadixCMap = array[0..MAXRadix-1] of char8; {character mapping for radix conversion}
const
  mp_lcrmap  : TRadixCMap = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ|~';
                            {lower case characters for radix conversion}
  mp_ucrmap  : TRadixCMap = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz|~';
                            {uppercase case characters for radix conversion}
{$else}
const
  MAXRadix   = 36;     {max radix for conversion}
type
  TRadixCMap = array[0..MAXRadix-1] of char8; {character mapping for radix conversion}
const
  mp_lcrmap  : TRadixCMap = '0123456789abcdefghijklmnopqrstuvwxyz';
                            {lower case characters for radix conversion}
  mp_ucrmap  : TRadixCMap = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
                            {uppercase case characters for radix conversion}
{$endif}


type
  TDigitArray = packed array[0..MaxDigits+63] of mp_digit; {the digits of an mp_int}
  PDigitArray = ^TDigitArray;                              {pointer to digit array}

type
  mp_string   = string[255];           {short string to avoid problems with ansistrng, widestring etc}

type
  mp_int    = record                   {MP integer number           }
                pdigits : PDigitArray; {pointer to digit array      }
                alloc   : word;        {allocated digits in pdigits }
                used    : word;        {used digits in pdigits      }
                sign    : word;        {sign: MP_ZPOS or MP_NEG     }
                magic   : word;        {set to MP_MAGIC by mp_init  }
              end;

  pmp_int   = ^mp_int;                 {pointer to an MP integer    }
  pmp_digit = ^mp_digit;               {pointer to an MP digit      }

type
  mp_rat    = record                   {MP rational number          }
                num: mp_int;           {numerator                   }
                den: mp_int;           {denominator > 0             }
              end;
  pmp_rat   = ^mp_rat;                 {pointer to an MP rational   }

type
  mp_float  = record                   {MP floating point number    }
                mantissa: mp_int;      {mantissa of an mp_float     }
                exponent: longint;     {exponent of an mp_float     }
                bitprec : longint;     {bit precision=max bitsize of mantissa}
              end;
  pmp_float = ^mp_float;               {pointer to an MP float      }


const
  MPF_MIN_PREC  = 8;                   {minimum floating point bit precision}

{$ifdef MPC_FPrec30K}
const
  MPF_MAX_PREC =  30000;  {Maximum floating point bit precision}
  MAX_TCBITS   =  30000;  {Number of bits in tables = max. precision for float constants}
{$else}
const
  MPF_MAX_PREC = 120000;  {Maximum floating point bit precision}
  MAX_TCBITS   = 124000;  {Number of bits in tables = max. precision for float constants}
{$endif}


{$ifdef BIT32}
const
  WEXP_MAX     = 8;               {log2 of WEXP_TABSIZE}
  WEXP_TABSIZE = 1 shl WEXP_MAX;  {Sliding window table size in mp_exptmod}
{$else}
const
  WEXP_MAX     = 5;               {log2 of WEXP_TABSIZE}
  WEXP_TABSIZE = 1 shl WEXP_MAX;  {Sliding window table size in mp_exptmod}
{$endif}


{$ifdef MPC_ErrFunc}
function mp_error : integer; {mp error 'variable' is function in debug mode}
{$else}
{$ifdef WIN32}
threadvar mp_error: integer; {mp error variable, threadvar for WIN32}
{$else}
var mp_error      : integer; {mp error variable}
{$endif}
{$endif}


{$ifdef MPC_Diagnostic}
type
  TMemStatus = record
                 MemDiff  : longint;  {Difference allocated - deallocated}
                 InitDiff : longint;  {Difference mp_init - mp_clear     }
                 ACntHigh : longint;  {Alloc count high dword            }
                 ACntLow  : longint;  {Alloc count low  dword            }
               end;

const
  MAX_DiagCTR   = 7;     {Max. index for TDiagCtrArray}
type
  TDiagCtrArray = array[0..MAX_DiagCTR] of longint;  {Array of diagnostic counters}
var
  mp_memstat    : TMemStatus;    {Diagnostic: Global memory status}
  mp_diagctr    : TDiagCtrArray; {General diagnostic counters, initialized to zero}
{$endif}

{$ifdef MPC_TRACE}
var
  mp_verbose: integer;  {Verbosity level for trace output}
  mp_ods_strip_cr: boolean;  {Strip leading/trailing CR+LF for OutputDebugString}

procedure mp_trace(const x: mp_string);
  {-Trace output of x}

procedure mp_tracev(const x: mp_string);
  {-Trace output of x if mp_verbose>0}

procedure mp_tracevv(const x: mp_string);
  {-Trace output of x if mp_verbose>1}

procedure mp_tracevvv(const x: mp_string);
  {-Trace output of x if mp_verbose>2}

procedure mp_tracec(c: boolean; const x: mp_string);
  {-Trace output of x if c and mp_verbose>0}

procedure mp_tracecv(c: boolean; const x: mp_string);
  {-Trace output of x if c and mp_verbose>1}
{$endif}

function  mp_int2str(value: longint): mp_string;
  {-Convert integer to an mp_string }

procedure set_mp_error(const err: integer);
  {-Set error variable}

{$ifndef HAS_ASSERT}
procedure Assert(cond: boolean; const msg: mp_string);
  {-Assert for Pascal/Delphi without system.assert}
{$endif}


var
  mp_radpow: array[2..MAXRadix] of mp_digit; {max. power of radix <= MP_DIGIT_MAX}
  mp_radexp: array[2..MAXRadix] of integer;  {max. exponents for mp_radpow}

{$ifdef MPC_UseExceptions}
type
  MPXError      = class(Exception); {base/general          }
  MPXMemory     = class(MPXError);  {out of memory         }
  MPXRange      = class(MPXError);  {argument out of range }
  MPXBadArg     = class(MPXError);  {invalid parameter     }
  MPXUndef      = class(MPXError);  {answer is undefined   }
  MPXMaxDigits  = class(MPXError);  {MaxDigits exceeded    }
  MPXNotInit    = class(MPXError);  {not initialized       }
  MPXOverflow   = class(MPXError);  {(float) overflow      }
{$else}
const
  MP_RTE_MEM    = 203; {Runtime error for MP_MEM      }
  MP_RTE_RANGE  = 201; {Runtime error for MP_RANGE    }
  MP_RTE_NOTINIT= 210; {Runtime error for MP_NOTINIT  }
  MP_RTE_BADARG = 254; {Runtime error for MP_BADARG   }
  MP_RTE_OTHER  = 253; {Runtime error for other errors}
  MP_RTE_OVRFLOW= 205; {Runtime error for MP_OVERFLOW }
{$endif}

{$ifndef HAS_ASSERT}
const
  MP_RTE_ASSERT = 227; {Runtime error for assertion failed}
{$endif}


{#Z+}
{'Assertion failed' prefix missing in FPC}
{$ifdef FPC}
const MPAF = 'Assertion failed: ';
{$else}
const MPAF = '';
{$endif}
{#Z-}



implementation

{$ifdef MPC_TRACE}
  {$ifdef WIN32}
    uses windows;  {for OutputDebugString}
  {$endif}
{$endif}


{$ifdef MPC_ErrFunc}

{MPC_ErrFunc: mp_error is a function, so another (thread)var is
used as the error variable and mp_error returns its value.}

{$ifdef WIN32}
threadvar mp_err: integer; {MPC_ErrFunc: mp error variable, result for mp_error, threadvar for win32}
{$else}
var mp_err      : integer; {MPC_ErrFunc: mp error variable, result for mp_error}
{$endif}

{---------------------------------------------------------------------------}
function mp_error : integer;
  {-return mp error variable}
begin
  mp_error := mp_err;
end;

{---------------------------------------------------------------------------}
procedure set_mp_error(const err: integer);
  {-Set error variable}
begin
  mp_err := err;
end;

{$else}

{---------------------------------------------------------------------------}
procedure set_mp_error(const err: integer);
  {-Set error variable}
begin
  mp_error := err;
end;

{$endif}


{$ifndef HAS_ASSERT}
{---------------------------------------------------------------------------}
procedure Assert(cond: boolean; const msg: mp_string);
  {-Assert procedure for Pascal/Delphi without system.assert}
begin
  if not cond then begin
    writeln('Assertion failed: ', msg);
    RunError(MP_RTE_ASSERT);
  end;
end;
{$endif}


{$ifdef MPC_TRACE}

{$ifdef WIN32}
{---------------------------------------------------------------------------}
procedure mp_trace(const x: mp_string);
  {-Trace output of x}
var
  ax: ansistring;
  ls: integer;
begin
  {$ifndef MPC_USE_ODS}
    if IsConsole then begin
      write(x);
      exit;
    end;
  {$endif}
  {strip trailing #13#10 from debug string}
  ls := length(x);
  if mp_ods_strip_cr then begin
    if (ls>1) and (x[ls]=#10) and (x[ls-1]=#13) then dec(ls,2);
  end;
  ax := copy(x,1,ls);
  if mp_ods_strip_cr then begin
    {strip leading #13#10 from debug string}
    if (ls>1) and (x[1]=#10) and (x[2]=#13) then delete(ax,1,2);
  end;
  OutputDebugString(pchar(string(ax)));
end;

{$else}
{---------------------------------------------------------------------------}
procedure mp_trace(const x: mp_string);
  {-Trace output of x}
begin
  write(x);
end;
{$endif}


{---------------------------------------------------------------------------}
procedure mp_tracev(const x: mp_string);
  {-Trace output of x if mp_verbose>0}
begin
 if mp_verbose>0 then mp_trace(x);
end;


{---------------------------------------------------------------------------}
procedure mp_tracevv(const x: mp_string);
  {-Trace output of x if mp_verbose>1}
begin
  if mp_verbose>1 then mp_trace(x);
end;


{---------------------------------------------------------------------------}
procedure mp_tracevvv(const x: mp_string);
  {-Trace output of x if mp_verbose>2}
begin
  if mp_verbose>2 then mp_trace(x);
end;


{---------------------------------------------------------------------------}
procedure mp_tracec(c: boolean; const x: mp_string);
  {-Trace output of x if c and mp_verbose>0}
begin
  if (mp_verbose>0) and c then mp_trace(x);
end;


{---------------------------------------------------------------------------}
procedure mp_tracecv(c: boolean; const x: mp_string);
  {-Trace output of x if c and mp_verbose>1}
begin
  if (mp_verbose>1) and c then mp_trace(x);
end;
{$endif}


{---------------------------------------------------------------------------}
function mp_int2str(value: longint): mp_string;
  {-Convert integer to an mp_string }
var
  s: string[20];
begin
  str(value, s);
  mp_int2str := s;
end;


{---------------------------------------------------------------------------}
procedure calc_radtables;
  {-calculate mp_radpow and mp_radexp tables}
var
  d,rp,radix: mp_digit;
  re: integer;
begin
  {tables must be calculated because MP_DIGIT_MAX is configurable}
  for radix:=2 to MAXRadix do begin
    d  := MP_DIGIT_MAX;
    rp := 1;
    re := 0;
    while d>=radix do begin
      rp := rp*radix;
      inc(re);
      d := d div radix;
    end;
    mp_radpow[radix] := rp;
    mp_radexp[radix] := re;
  end;
end;


begin

  mp_show_plus  := false; {no "+" in ascii string            }
  mp_uppercase  := false; {no uppercase chars for radix > 10 }
  mp_clearzero  := false; {don't clear memory to zero before freemem}
  mp_roundfloat := false; {default false: round float representation of mp_rat}

  mp_fract_sep  := '.';   {Separator between integer and fractional part, default '.'}
  mp_arg_sep    := ',';   {Separator between arguments in parser units, default ','}

  set_mp_error(0);        {clear error variable              }

  {$ifdef BIT32}
    {$ifdef MP_32BIT}
      mp_mul_cutoff := 16;   {Karatsuba multiplication cutoff}
      mp_sqr_cutoff := 32;   {Karatsuba square cutoff        }
      mp_bz_cutoff  := 32;   {Burnikel-Ziegler cutoff        }
    {$else}
      mp_mul_cutoff := 64;   {Karatsuba multiplication cutoff}
      mp_sqr_cutoff := 96;   {Karatsuba square cutoff        }
      mp_bz_cutoff  := 96;   {Burnikel-Ziegler cutoff        }
    {$endif}
  {$else}
    mp_mul_cutoff := 48;     {Karatsuba multiplication cutoff}
    mp_sqr_cutoff := 64;     {Karatsuba square cuttof        }
    mp_bz_cutoff  := 64;     {Burnikel-Ziegler cutoff        }
  {$endif}

  mp_sqrt_cutoff := 4;       {Karatsuba square root cutoff   }
  mp_t3m_cutoff  := 2*mp_mul_cutoff;
  mp_t3s_cutoff  := 2*mp_sqr_cutoff;

  {$ifdef MPC_Diagnostic}
    fillchar(mp_diagctr,sizeof(mp_diagctr),0);  {initialize diagnostic counter array}
    fillchar(mp_memstat,sizeof(mp_memstat),0);  {initialize global memory status}
  {$endif}

  {$ifdef MPC_TRACE}
    mp_verbose      := 0;     {Verbosity level for trace output}
    mp_ods_strip_cr := true;  {Strip leading/trailing CR+LF for OutputDebugString}
  {$endif}

  {$ifdef MP_32BIT}
    {make sure DIGIT_BIT >= 16 }
    assert(DIGIT_BIT >= 16, MPAF+'DIGIT_BIT >= 16');
  {$else}
    {make sure DIGIT_BIT >= 8, needed e.g. in read/write binary}
    assert(DIGIT_BIT >= 8, MPAF+'DIGIT_BIT >= 8');
  {$endif}

  {make sure DIGIT_BIT+16 < 8*sizeof(mp_word) for mp_div_w}
  assert(DIGIT_BIT+16 < 8*sizeof(mp_word), MPAF+'DIGIT_BIT+16 < 8*sizeof(mp_word)');

  {make sure sizeof(pointer)=sizeof(longint), needed e.g. in mp_realloc}
  assert(sizeof(pointer)=sizeof(longint), MPAF+'sizeof(pointer)=sizeof(longint)');
  assert(Test8086>1,MPAF+'CPU386 or better');

  {make sure precompiled floats const are valid for all precisions}
  assert(MPF_MAX_PREC <= MAX_TCBITS, MPAF+'MPF_MAX_PREC <= MAX_TCBITS');

  assert(MP_LT < 0, MPAF+'MP_LT < 0');
  assert(MP_EQ = 0, MPAF+'MP_EQ = 0');
  assert(MP_GT > 0, MPAF+'MP_GT > 0');

  {calculate mp_radpow and mp_radexp tables}
  calc_radtables;

end.
