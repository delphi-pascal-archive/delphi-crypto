unit mp_real;

{Multi precision floating point arithmetic routines}

interface


{$ifdef VirtualPascal}
{$X+} {needed for pchars/RESULT}
{$endif}

{$i STD.INC}

{$ifdef BIT16}
{$N+}
{$X+} {needed for pchars}
{$endif}

uses
  BTypes, mp_types;

{$i mp_conf.inc}

(*************************************************************************

 DESCRIPTION   :  Multi precision floating point arithmetic routines

 REQUIREMENTS  :  BP7, D1-D7/D9-D10/D12, FPC, VP

 EXTERNAL DATA :  (mp_types)

 MEMORY USAGE  :  heap

 DISPLAY MODE  :  ---

 REFERENCES    :  [3] D.E. Knuth: The Art of computer programming, Volume 2,
                      Seminumerical Algorithms, 3rd ed., 1998
                      http://www-cs-faculty.stanford.edu/~knuth/taocp.html
                  [8] Marcel Martin: NX - Numerics library of multiprecision
                      numbers for Delphi and Free Pascal, 2006-2009
                      www.ellipsa.eu/public/nx/index.html
                 [22] LiDIA - A Library for Computational Number Theory,
                      http://www.cdc.informatik.tu-darmstadt.de/TI/LiDIA/
                 [27] T. Papanikolaou: libF - Eine lange Gleitpunktarithmetik,
                      Diplomarbeit 1995, available online from
                      http://www.cdc.informatik.tu-darmstadt.de/reports/reports/papa.diplom.ps.gz
                 [31] J. Arndt, Matters Computational. Ideas, algorithms, source code.
                      http://www.jjj.de/fxt/#fxtbook


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------

 0.0.01   01.11.07  W.Ehrhardt  Basic type definitions, mpf_init etc
 0.0.02   03.11.07  we          mpf_todouble/extended, s_mpf_normalize, mpf_set_int
 0.0.03   03.11.07  we          s_mpf_normalizep, mpf_initp, mpf_mul, mpf_expt_int
 0.0.04   03.11.07  we          mpf_exch, mpf_inv, mpf_div, mpf_is0
 0.0.05   04.11.07  we          mpf_abs, mpf_chs, mpf_mul_2k, s_mpf_incexp
 0.0.06   04.11.07  we          mpf_add, mpf_sub, s_mpf_addsub
 0.0.07   04.11.07  we          removed .flags, fix mpf_abs, mpf_chs
 0.0.08   04.11.07  we          mpf_cmp_mag, mpf_initp2
 0.0.09   05.11.07  we          mpf_get_default_prec, mpf_sqrt
 0.0.10   07.11.07  we          mpf_cmp, mpf_noinit, arg checks
 0.0.11   07.11.07  we          s_mpf_normalize: stick bit and postnormalization
 0.0.12   07.11.07  we          mpf_set_ext
 0.0.13   08.11.07  we          mpf_chg_prec, mpf_checksum
 0.0.14   08.11.07  we          mpf_int, mpf_frac, mpf_trunc
 0.0.15   09.11.07  we          mpf_add/sub for a=0, skip final sqr in mpf_expt_int
 0.0.16   10.11.07  we          mpf_is1, mpf_is1a, special cases in mpf_mul/div
 0.0.17   10.11.07  we          bugfix mpf_div
 0.0.18   10.11.07  we          mpf_div_mpi, mpf_mul_mpi, mpf_set_mpi
 0.0.19   10.11.07  we          s_mpf_addsub changed to s_mpf_inc, mpf_add/sub_mpi
 0.0.20   11.11.07  we          mpf_add_ext, mpf_sub_ext, mpf_div_ext, mpf_mul_ext
 0.0.21   11.11.07  we          mpf_read_decimal, mpf_read_radix
 0.0.22   12.11.07  we          Fix memory leak(s) if MPC_HaltOnError is not defined
 0.0.23   13.11.07  we          mpf_is_ge, mpf_is_lt
 0.0.24   14.11.07  we          s_mpf_to10_n, mpf_decimal, mpf_adecimal
 0.0.25   15.11.07  we          mpf_initp_multi_p, mpf_initp[x], mpf_clear[x],
 0.0.26   15.11.07  we          mpf_set1, overflow check in mpf_sqr
 0.0.27   16.11.07  we          s_mpf_inc: allow @x=@y, s_mpf_incf
 0.0.28   16.11.07  we          mpf_exp, bugfix mpf_read_radix for '-.xxx'
 0.0.29   16.11.07  we          s_mpf_incexp with BASM
 0.0.30   17.11.07  we          mpf_ln, use b with working precision in mpf_exp
 0.0.31   17.11.07  we          mpf_expt, mpf_iexpt
 0.0.32   18.11.07  we          exponent overflow checks via s_mpf_incexp
 0.0.33   19.11.07  we          mpf_pi_chud, mpf_pi_machin. pi via mp_read_unsigned_bin,
 0.0.34   20.11.07  we          mpf_set_pip uses AddrPiBytes (pi table linked only if needed)
 0.0.35   23.11.07  we          mpf_set_pip2k, mpf_set_pi2k, mpf_arctan
 0.0.36   25.11.07  we          mpf_trig, mpf_cos, mpf_sin, mpf_tan
 0.0.37   26.11.07  we          s_mpf_inc1
 0.0.38   26.11.07  we          rounding in mpf_set_pip2k, mpf_pi_chud/mpf_pi_machin outsourced
 0.0.39   26.11.07  we          mpf_is_eq, mpf_is_gt, mpf_is_le, mpf_is_ne
 0.0.40   26.11.07  we          mpf_random, s_mpf_to10_n (case a=1, removed c10)
 0.0.41   29.11.07  we          s_mpf_abs, s_mpf_chs, mpf_mul_d/int, mpf_div_d/int
 0.0.42   01.12.07  we          s_mpf_is0
 0.0.43   02.12.07  we          s_mpf_is_neg, bugfix mpf_div
 0.0.44   02.12.07  we          s_mpf_mod_pi2k (used in mpf_trig)
 0.0.45   09.12.07  we          mp_float to mp_types, mpf_ln with mpf_div_d/int
 0.0.46   09.12.07  we          use mpf_copyp in mp_trig to set ps^ and pc^
 0.0.47   09.12.07  we          improve mpf_sub_ext for a=0
 0.0.48   16.12.07  we          mpf_todecimal_n, mpf_is_eq_rel
 0.0.49   17.12.07  we          mpf_tohex_n, mpf_read_hex, (s_)mpf_toradix_n
 0.0.50   18.12.07  we          used s_mpf_normalizep in some routines for mpf_chg_prec
 0.0.51   18.12.07  we          (mpf_is_eq_rel_n)
 0.0.52   20.12.07  we          mpf_reldev
 0.0.53   21.12.07  we          mpf_trig: bugfix -eps, sum series for cos - 1
 0.0.54   21.12.07  we          improved mpf_exp

 1.4.00   01.01.08  we          check a<>0 in mpf_ln, s_mpf_add1
 1.4.01   01.01.08  we          s_mpf_dec1, mpf_expm1, mpf_ln1p
 1.4.02   02.01.08  we          mpf_cosh, mpf_sinh, mpf_tanh, mpf_atanh
 1.4.03   03.01.08  we          mpf_acosh, mpf_acosh1p, mpf_asinh
 1.4.04   03.01.08  we          mpf_arctan2, mpf_arccos, mpf_arcsin
 1.4.05   05.01.08  we          s_mpf_toradix_n: fix if rounded res = radix^ndd
 1.4.06   06.01.08  we          s_mpf_agm, mpf_agm, mpf_ccell1, mpf_ccell12
 1.4.07   10.01.08  we          mpf_set_mpi2k, mpf_ccell12: increased working precision

 1.5.00   16.01.08  we          Fix rounding/overflow in s_mpf_toradix_n
 1.5.01   16.01.08  we          mpf_writeln, mpf_write_decimal/radix, mpf_output_decimal/radix
 1.5.02   17.01.08  we          s_mpf_ldx, s_mpf_is_le0, s_mpf_is_ge0
 1.5.03   18.01.08  we          mpf_cosh/sinh: don't calculate inverse if it is to small
 1.5.04   19.01.08  we          mpf_acosh/asinh = ln(2a) for large a, mpf_sinh small fix
 1.5.05   20.01.08  we          mpf_ccell2, s_mpf_ccell12, s_mpf_incf with add32_ovr
 1.5.06   25.01.08  we          _set_ptab_const, mpf_set_ln2/10 functions
 1.5.07   25.01.08  we          mpf_10expt, mpf_2expt, mpf_log10, mpf_log2
 1.5.08   28.01.08  we          s_mpf_ldx returns MaxLongint if overflow, fix mpf_ln
 1.5.09   29.01.08  we          abs(b)=1 in mpf_mul/div_int/d
 1.5.10   30.01.08  we          mpf_read_radix: removed readxp quick hack
 1.5.11   31.01.08  we          {$x+} for VP and D1
 1.5.12   03.02.08  we          bugfix s_mpf_agm (don't use c in loop)

 1.6.00   11.06.08  we          MPXRange.Create('mpf_chg_prec: newprec out of range');

 1.7.00   23.08.08  we          Avoid FPC222 warning in mpf_decimal
 1.7.01   16.09.08  we          Accept 'b' or 'B' in binary exponents
 1.7.02   24.09.08  we          string replaced by mp_string

 1.9.00   02.12.08  we          Uses BTypes: char8, pchar8
 1.9.01   13.12.08  we          mpf_sumalt

 1.10.00  21.01.09  we          changes related to (s)mp_divrem
 1.10.01  21.02.09  we          mpf_sinhcosh
 1.10.02  23.02.09  we          mpf_read_radix: changed evaluation of fraction part
 1.10.03  24.02.09  we          mpf_round
 1.10.04  25.02.09  we          s_mpf_ldx(0)=-MaxLongint, mpf_ln1p for a=0,
 1.10.05  25.02.09  we          s_mpf_numbpart, mpf_numbpart
 1.10.06  25.02.09  we          mpf_add_int, mpf_sub_int
 1.10.07  27.02.09  we          s_mpf_frac, mpf_trig_ex

 1.11.00  23.03.09  we          mpf_sumaltf
 1.11.01  30.03.09  we          removed redefinition of pByte

 1.12.00  05.07.09  we          D12 fixes in mpf_read_radix and s_mpf_toradix_n

 1.13.00  14.08.09  we          small improvements in (s_)mpf_numbpart
 1.13.01  23.08.09  we          mpf_squad
 1.13.02  01.11.09  we          New names: mpf_arccosh,mpf_arccosh1p,mpf_arcsinh,mpf_arctanh,mpf_exp10,mpf_exp2
 1.13.03  01.11.09  we          mpf_cot,mpf_csc,mpf_sec,mpf_coth,mpf_csch,mpf_sech
 1.13.04  02.11.09  we          mpf_arccot,mpf_arccotc,mpf_arccsc,mpf_arcsec,mpf_arccoth,mpf_arccsch,mpf_arcsech
 1.13.05  02.11.09  we          changed mpf_arccosh domain to a >= 1
 1.13.06  16.11.09  we          mpf_init[x], x=2..5
 1.13.07  24.11.09  we          mpf_arccotc calls mpf_arccot for a>0
 1.13.08  24.11.09  we          inv_func/func_inv with 32 guard bits

 1.14.00  31.01.10  we          mpf_sqrt1pm1
 1.14.01  07.02.10  we          mpf_logbase
 1.14.02  09.02.10  we          improved guard bits calculation in mpf_expt
 1.14.03  09.02.10  we          mpf_cmp_ext, mpf_cmp_mag_ext

 1.15.00  05.05.10  we          basic s_mpf_toradix_alt
 1.15.01  06.05.10  we          mpf_(a)decimal_alt, mpf_todecimal_alt, mpf_toradix_alt
 1.15.02  07.05.10  we          mpf_write_radix_alt, mpf_write_decimal_alt
 1.15.03  07.05.10  we          mpf_output_radix_alt, mpf_output_decimal_alt
 1.15.04  08.05.10  we          s_mpf_toradix_alt: k = ndd-e-1
 1.15.05  08.05.10  we          mpf_round: fix quirk if @b = @a.mantissa
 1.15.07  09.05.10  we          mpf_todouble, mpf_toextended: pre-check max values
 1.15.08  13.05.10  we          mp_fract_sep
 1.15.09  13.05.10  we          improved s_mpf_toradix_alt: use s_mp_toradix_n for fractional part
 1.15.09  13.05.10  we          mpf_set_default_decprec
 1.15.10  14.05.10  we          improved s_mpf_toradix_alt: faster removal of trailing zeros
 1.15.11  15.05.10  we          s_mpf_toradix_alt: use IsPow2_w
 1.15.12  15.05.10  we          s_mpf_toradix_alt: improved for radix<>2^n

 1.16.00  12.06.10  we          Corrected some exception strings
 1.16.01  13.06.10  we          mpf_set_exp1, mpf_set_exp1p
 1.16.02  17.07.10  we          mpf_expt1pm1, mpf_exptm1

**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2007-2010 Wolfgang Ehrhardt

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

type
  sumalt_term_func = function (k: longint; var num, den: longint): boolean;
  {sumalt_term_func must be of type (-1)^k*x(k), x(k)=num(k)/den(k) > 0}
  {result = false if x(k) cannot be evaluated, e.g. den > MaxLongint.}

  sumalt_fterm_func = function (k: longint; var term: mp_float): boolean;
  {sumalt_fterm_func must be of type (-1)^k*x(k), x(k) >= 0}
  {result = false if x(k) cannot be evaluated}


procedure mpf_abs(const a: mp_float; var b: mp_float);
  {-absolute value, b = |a|}


procedure mpf_add(const a,b: mp_float; var c: mp_float);
  {-calculate c = a+b}

procedure mpf_add_ext(const a: mp_float; b: extended; var c: mp_float);
  {-calculate c = a+b}

procedure mpf_add_int(const a: mp_float; b: longint; var c: mp_float);
  {-calculate c = a+b}

procedure mpf_add_mpi(const a: mp_float; const b: mp_int; var c: mp_float);
  {-calculate c = a+b}

{$ifdef BIT32}
function  mpf_adecimal(const a: mp_float; ndd: word): ansistring;
  {-Convert to decimal scientific representation with ndd digits, max 65000 digits}

function  mpf_adecimal_alt(const a: mp_float; ndd: word): ansistring;
  {-Convert to decimal alternative representation with ndd digits, max 65000 digits}
{$endif}

procedure mpf_agm(const a,b: mp_float; var c: mp_float);
  {-calculate c = AGM(|a|,|b|)}

procedure mpf_arccos(const a: mp_float; var b: mp_float);
  {-calculate b = arccos(a), |a| <= 1}

procedure mpf_arccosh(const a: mp_float; var b: mp_float);
  {-calculate b = arccosh(a), a >= 1. Note: for a near 1 the function}
  { arccosh1p(a-1) should be used to reduce cancellation errors!}

procedure mpf_arccosh1p(const a: mp_float; var b: mp_float);
  {-calculate b = arccosh(1+a), a>=0}

procedure mpf_arccot(const a: mp_float; var b: mp_float);
  {-calculate the sign symmetric circular cotangent b = arccot(a) = arctan(1/a)}

procedure mpf_arccotc(const a: mp_float; var b: mp_float);
  {-calculate the continous circular cotangent b = arccotc(a) = Pi/2 - arctan(a)}

procedure mpf_arccoth(const a: mp_float; var b: mp_float);
  {-calculate the inverse hyperbolic cotangent b = arccoth(a), |a| > 1}

procedure mpf_arccsc(const a: mp_float; var b: mp_float);
  {-calculate the inverse circular cosecant b = arccsc(a), |a| >= 1}

procedure mpf_arccsch(const a: mp_float; var b: mp_float);
  {-calculate the inverse hyperbolic cosecant b = arccsch(a)}

procedure mpf_arcsec(const a: mp_float; var b: mp_float);
  {-calculate the inverse circular secant b = arcsec(a), |a| >= 1}

procedure mpf_arcsech(const a: mp_float; var b: mp_float);
  {-calculate the inverse hyperbolic secant b = arcsech(a), 0 < a <= 1}

procedure mpf_arcsin(const a: mp_float; var b: mp_float);
  {-calculate b = arcsin(a), |a| <= 1}

procedure mpf_arctan(const a: mp_float; var b: mp_float);
  {-calculate b = arctan(a)}

procedure mpf_arctan2(const y,x: mp_float; var a: mp_float);
  {-calculate a = arctan(y/x) with special treatment for zero x or y. a is}
  { the principal value of arg(x + i*y), i.e. -pi < arctan2(y,x) <= pi.}

procedure mpf_arcsinh(const a: mp_float; var b: mp_float);
  {-calculate b = arcsinh(a)}

procedure mpf_arctanh(const a: mp_float; var b: mp_float);
  {-calculate b = arctanh(a), |a| < 1}

procedure mpf_ccell1(const k: mp_float; var CK: mp_float);
  {-Complementary complete elliptic integral of the first kind CK = CK(k)}
  { with k>0 using AGM algorithm}

procedure mpf_ccell12(const k: mp_float; var CK, CE: mp_float);
  {-Complementary complete elliptic integrals of the 1st and 2nd kind using}
  { AGM algorithm; k>0 and  @CK <> @CE}

procedure mpf_ccell2(const k: mp_float; var CE: mp_float);
  {-Complementary complete elliptic integral of the 2nd kind CE = CE(k)}
  { with k>0 using AGM algorithm. Special case of mpf_ccell12}

function  mpf_checksum(const a: mp_float): longint;
  {-return a checksum for a, -1 if mp_error<>MP_OKAY, -2 if not initialized}

procedure mpf_chg_prec(var a: mp_float; newprec: longint);
  {-change bitprec of a to newprec}

procedure mpf_chs(const a: mp_float; var b: mp_float);
  {-change sign, b = -a}

procedure mpf_clear(var a: mp_float);
  {-clear an mp_float}

procedure mpf_clear2(var a,b: mp_float);
  {-clear 2 mp_floats}

procedure mpf_clear3(var a,b,c: mp_float);
  {-clear 3 mp_floats}

procedure mpf_clear4(var a,b,c,d: mp_float);
  {-clear 4 mp_floats}

procedure mpf_clear5(var a,b,c,d,e: mp_float);
  {-clear 5 mp_floats}

function  mpf_cmp(const a,b: mp_float): integer;
  {-compare two mp_floats, return sign(a-b)}

function  mpf_cmp_ext(const a: mp_float; b: extended): integer;
  {-compare a and b, return sign(a-b)}

function  mpf_cmp_mag(const a,b: mp_float): integer;
  {-compare magnitude of two mp_floats, return sign(|a|-|b|)}

function  mpf_cmp_mag_ext(const a: mp_float; b: extended): integer;
  {-compare magnitude of a and b, return sign(|a|-|b|)}

procedure mpf_copy(const a: mp_float; var b: mp_float);
  {-copy a to b with b.bitprec=a.bitprec}

procedure mpf_copyp(const a: mp_float; var b: mp_float);
  {-copy a to b, preserve b.bitprec}

procedure mpf_cos(const a: mp_float; var b: mp_float);
  {-calculate b = cos(a)}

procedure mpf_cosh(const a: mp_float; var b: mp_float);
  {-calculate b = cosh(a), a < 2^31 * ln(2)}

procedure mpf_cot(const a: mp_float; var b: mp_float);
  {-calculate the circular cotangent b := cot(a), a mod Pi <> 0}

procedure mpf_coth(const a: mp_float; var b: mp_float);
  {-calculate the hyperbolic cotangent b = coth(a), a <> 0}

procedure mpf_csc(const a: mp_float; var b: mp_float);
  {-calculate the circular cosecant b = csc(a), a mod Pi <> 0}

procedure mpf_csch(const a: mp_float; var b: mp_float);
  {-calculate the hyperbolic cosecant b = csch(a), a  <> 0}

function  mpf_decimal(const a: mp_float; ndd: word): mp_string;
  {-Convert to decimal scientific representation with ndd digits, max 255 chars}

function  mpf_decimal_alt(const a: mp_float; ndd: word): mp_string;
  {-Convert to decimal alternative representation with ndd digits, max 255 chars}

procedure mpf_div(const a,b: mp_float; var c: mp_float);
  {-calculate c = a/b}

procedure mpf_div_d(const a: mp_float; b: mp_digit; var c: mp_float);
  {-calculate c = a/b}

procedure mpf_div_ext(const a: mp_float; b: extended; var c: mp_float);
  {-calculate c = a/b}

procedure mpf_div_int(const a: mp_float; b: longint; var c: mp_float);
  {-calculate c = a/b}

procedure mpf_div_mpi(const a: mp_float; const b: mp_int; var c: mp_float);
  {-calculate c = a/b}

procedure mpf_exch(var a,b: mp_float);
  {-exchange two mp_floats (including bitprec!!)}

procedure mpf_exp(const a: mp_float; var b: mp_float);
  {-calculate b = exp(a), a < 2^31 * ln(2)}

procedure mpf_exp10(const a: mp_float; var b: mp_float);
  {-calculate b = 10^a}

procedure mpf_exp2(const a: mp_float; var b: mp_float);
  {-calculate b = 2^a}

procedure mpf_expm1(const a: mp_float; var b: mp_float);
  {-calculate b = exp(a)-1, a < 2^31 * ln(2); special version for small a}

procedure mpf_expt(const a,b: mp_float; var c: mp_float);
  {-calculate c = a^b, a>0}

procedure mpf_expt1pm1(const a,b: mp_float; var c: mp_float);
  {-calculate c = (1+a)^b-1, a>-1}

procedure mpf_exptm1(const a,b: mp_float; var c: mp_float);
  {-calculate c = a^b-1, a>0}

procedure mpf_expt_int(const a: mp_float; b: longint; var c: mp_float);
  {-calculate c = a^b}

procedure mpf_frac(const a: mp_float; var b: mp_float);
  {-set b to the fractional part of a; frac(x)=x-int(x)}

function  mpf_get_default_prec: longint;
  {-return current default (bit) precision, initial=240}

procedure mpf_iexpt(a: longint; const b: mp_float; var c: mp_float);
  {-calculate c = a^b, a>0}

procedure mpf_init(var a: mp_float);
  {-initialize an mp_float with default precision}

procedure mpf_init2(var a,b: mp_float);
  {-initialize two mp_floats with default precision}

procedure mpf_init3(var a,b,c: mp_float);
  {-initialize 3 mp_floats with default precision}

procedure mpf_init4(var a,b,c,d: mp_float);
  {-initialize 4 mp_floats with default precision}

procedure mpf_init5(var a,b,c,d,e: mp_float);
  {-initialize 5 mp_floats with default precision}

procedure mpf_initp(var a: mp_float; prec: longint);
  {-initialize an mp_float with bit precision prec}

procedure mpf_initp2(var a,b: mp_float; prec: longint);
  {-initialize two mp_floats with bit precision prec}

procedure mpf_initp3(var a,b,c: mp_float; prec: longint);
  {-initialize 3 mp_floats with bit precision prec}

procedure mpf_initp4(var a,b,c,d: mp_float; prec: longint);
  {-initialize 4 mp_floats with bit precision prec}

procedure mpf_initp5(var a,b,c,d,e: mp_float; prec: longint);
  {-initialize 5 mp_floats with bit precision prec}

procedure mpf_initp_multi_p(var pv: array of pmp_float; prec: longint);
  {-initialize with bit precision prec a list of mp_floats given as a pointer}
  { vector; on error the already initialized mp_floats will be cleared}

procedure mpf_int(const a: mp_float; var b: mp_float);
  {-set b to the integer part of a; i.e. is b rounded toward zero}

procedure mpf_inv(const a: mp_float; var b: mp_float);
  {-calculate b = 1/a}

function  mpf_is0(const a: mp_float): boolean;
  {-return true if a=0}

function  mpf_is1(const a: mp_float): boolean;
  {-return true if a=1}

function  mpf_is1a(const a: mp_float): boolean;
  {-return true if abs(a)=1}

function  mpf_is_eq(const a,b: mp_float): boolean;
  {-return a = b}

function  mpf_is_eq_rel(const a,b: mp_float): boolean;
  {-Check if |a-b| <= r*2^(1-b.bitprec);  r=1 if b=0, r=|b| otherwise}

function  mpf_is_ge(const a,b: mp_float): boolean;
  {-return a >= b}

function  mpf_is_gt(const a,b: mp_float): boolean;
  {-return a > b}

function  mpf_is_le(const a,b: mp_float): boolean;
  {-return a <= b}

function  mpf_is_lt(const a,b: mp_float): boolean;
  {-return a < b}

function  mpf_is_ne(const a,b: mp_float): boolean;
  {-return a <> b}

procedure mpf_ln(const a: mp_float; var b: mp_float);
  {-calculate b = ln(a), a>0}

procedure mpf_ln1p(const a: mp_float; var b: mp_float);
  {-calculate b = ln(1+a), a>-1; special version for small a}

procedure mpf_log10(const a: mp_float; var b: mp_float);
  {-calculate b = log10(a), a>0}

procedure mpf_log2(const a: mp_float; var b: mp_float);
  {-calculate b = log2(a), a>0}

procedure mpf_logbase(const b,x: mp_float; var y: mp_float);
  {-calculate y = base b logarithm of x}

procedure mpf_mul(const a,b: mp_float; var c: mp_float);
  {-calculate c = a*b}

procedure mpf_mul_2k(const a: mp_float; k: longint; var b: mp_float);
  {-calculate b = a*2^k}

procedure mpf_mul_d(const a: mp_float; b: mp_digit; var c: mp_float);
  {-multiply by a digit}

procedure mpf_mul_ext(const a: mp_float; b: extended; var c: mp_float);
  {-calculate c = a*b}

procedure mpf_mul_int(const a: mp_float; b: longint; var c: mp_float);
  {-multiply by a 32 bit integer}

procedure mpf_mul_mpi(const a: mp_float; const b: mp_int; var c: mp_float);
  {-calculate c = a*b}

function  mpf_not_init(const a: mp_float): boolean;
  {-sanity check if a is initialized, does not catch all cases!}

procedure mpf_numbpart(n: longint; var p: mp_int);
  {-Compute number of partitions of n with Hardy-Ramanujan-Rademacher formula}

procedure mpf_output_decimal(const a: mp_float; ndd: word);
  {-write an mp_float to output using decimal scientific representation, ndd>0}
  { is the total number of digits (including one digit before the '.')}

procedure mpf_output_decimal_alt(const a: mp_float; ndd: word);
  {-write an mp_float to output using decimal alternative representation, ndd>0 is}
  { the total number of digits, trailing '.' or '0' are suppressed. If a is too}
  { large or too small, scientific representation is used.}

procedure mpf_output_radix(const a: mp_float; radix,ndd: word);
  {-write an mp_float to output using radix scientific representation, ndd>0}
  { is the total number of digits (including one digit before the '.')}

procedure mpf_output_radix_alt(const a: mp_float; radix,ndd: word);
  {-write an mp_float to output using radix alternative representation, ndd>0 is}
  { the total number of digits, trailing '.' or '0' are suppressed. If a is too}
  { large or too small, scientific representation is used. NOTE: no radix prefix/suffix!}

procedure mpf_random(var a: mp_float);
  {-set to a random number uniformly distributed in [0,1)}

procedure mpf_read_decimal(var a: mp_float; str: pchar8);
  {-read a from ASCII float decimal string. str may contain a single '.'}
  { The exponent part is @[+|-]nnn (e or E can replace @). Integer or }
  { fractional part must be present.}

procedure mpf_read_hex(var a: mp_float; str: pchar8);
  {-read a from ASCII float hexadecimal string. str may contain a single}
  { '.'. The exponent part is @[+|-]nnn (h or H can replace @). Integer }
  { or fractional part must be present.}

procedure mpf_read_radix(var a: mp_float; str: pchar8; radix: word);
  {-read a from an ASCII float radix string. str may contain a single '.'.}
  { The exponent part <xp> is @[+|-]nnn, b/B, e/E, or h/H can replace @ if}
  { radix in [2,10,16]. The integer <ip> or fractional part <fp> must be }
  { present, nnn must be decimal, ie general format is <ip>.<fp>*radix^<xp>.}

function  mpf_reldev(const a,b: mp_float): double;
  {-Return abs((a-b)/b)*2^b.bitprec, special if b=0, or a-b=0}

procedure mpf_round(const a: mp_float; var b: mp_int);
  {-round an mp_float to nearest mp_int}

procedure mpf_sec(const a: mp_float; var b: mp_float);
  {-calculate the circular secant b = sec(a), a mod Pi <> Pi/2}

procedure mpf_sech(const a: mp_float; var b: mp_float);
  {-calculate the hyperbolic secant b = sech(a)}

procedure mpf_set0(var a: mp_float);
  {-set a=0, a.bitprec is preserved}

procedure mpf_set1(var a: mp_float);
  {-set a=1, a.bitprec is preserved}

procedure mpf_set_default_decprec(dprec: word);
  {-Set default bit precision to dprec*log_2(10), i.e. dprec decimal digits}

procedure mpf_set_default_prec(prec: longint);
  {-set new default (bit) precision}

procedure mpf_set_exp1(var a: mp_float);
  {-set a to exp(1), preserve a.bitprec}

procedure mpf_set_exp1p(var a: mp_float; prec: longint);
  {-set a to exp(1) with bit precision prec}

procedure mpf_set_ext(var a: mp_float; x: extended);
  {-set a to an extended. Error if x = NAN or INF}

procedure mpf_set_int(var a: mp_float; b: longint);
  {-set a to a longint}

procedure mpf_set_ln10(var a: mp_float);
  {-set a to ln(10), preserve a.bitprec}

procedure mpf_set_ln10p(var a: mp_float; prec: longint);
  {-set a to ln(10) with bit precision prec}

procedure mpf_set_ln10p2k(var a: mp_float; k,prec: longint);
  {-set a to ln(10)*2^k with bit precision prec}

procedure mpf_set_ln2(var a: mp_float);
  {-set a to ln(2), preserve a.bitprec}

procedure mpf_set_ln2p(var a: mp_float; prec: longint);
  {-set a to ln(2) with bit precision prec}

procedure mpf_set_ln2p2k(var a: mp_float; k,prec: longint);
  {-set a to ln(2)*2^k with bit precision prec}

procedure mpf_set_mpi(var a: mp_float; const b: mp_int);
  {-set a to an mp_int}

procedure mpf_set_mpi2k(var a: mp_float; const m: mp_int; e: longint);
  {-set a to m*2^e (build a from mantissa and exponent)}

procedure mpf_set_pi(var a: mp_float);
  {-set a to pi, preserve a.bitprec}

procedure mpf_set_pi2k(var a: mp_float; k: longint);
  {-set a to pi*2^k, preserve a.bitprec}

procedure mpf_set_pip(var a: mp_float; prec: longint);
  {-set a to pi with bit precision prec}

procedure mpf_set_pip2k(var a: mp_float; k,prec: longint);
  {-set a to pi*2^k with bit precision prec}

procedure mpf_sin(const a: mp_float; var b: mp_float);
  {-calculate b = sin(a)}

procedure mpf_sinh(const a: mp_float; var b: mp_float);
  {-calculate b = sinh(a), a < 2^31 * ln(2)}

procedure mpf_sinhcosh(const a: mp_float; var b,c: mp_float);
  {-calculate b = sinh(a), c = cosh(a);  @b<>@c,  a < 2^31 * ln(2)}

procedure mpf_sqr(const a: mp_float; var b: mp_float);
  {-calculate b = a*a}

procedure mpf_sqrt(const a: mp_float; var b: mp_float);
  {-calculate b = sqrt(a)}

procedure mpf_sqrt1pm1(const a: mp_float; var b: mp_float);
  {-calculate b = sqrt(1+a)-1 with increased accuracy for a near 0, a >= -1}

function  mpf_squad(const a,b,c: mp_float; var x1,y1,x2,y2: mp_float): integer;
  {-Solve the quadratic equation a*x^2 + b*x + c = 0. Result is the number}
  { of different solutions: 0 (if a=b=0), 1 (x1), or 2 (x1 and x2). If the}
  { result is = -2, x1 + i*y1 and x2 + i*y2 are the two complex solutions.}
  { x1,y1,x2,y2 should be different variables and not the same as a,b or c}

procedure mpf_sub(const a,b: mp_float; var c: mp_float);
  {-calculate c = a-b}

procedure mpf_sub_int(const a: mp_float; b: longint; var c: mp_float);
  {-calculate c = a-b}

procedure mpf_sub_ext(const a: mp_float; b: extended; var c: mp_float);
  {-calculate c = a-b}

procedure mpf_sub_mpi(const a: mp_float; const b: mp_int; var c: mp_float);
  {-calculate c = a-b}

procedure mpf_sumalt(term: sumalt_term_func; n: longint; var s: mp_float; var Err: integer);
  {-calculate s=sum(i=0..n, term(i)) of alternating series with sumalt algorithm.}
  { If n<4, n is adjusted to bitprec of s. Err<>0 if a term cannot be evaluated.}

procedure mpf_sumaltf(fterm: sumalt_fterm_func; n: longint; var s: mp_float; var Err: integer);
  {-calculate s=sum(i=0..n, term(i)) of alternating series with sumalt algorithm.}
  { If n<4, n is adjusted to bitprec of s. Err<>0 if a term cannot be evaluated.}

procedure mpf_tan(const a: mp_float; var b: mp_float);
  {-calculate b = tan(a)}

procedure mpf_tanh(const a: mp_float; var b: mp_float);
  {-calculate b = tanh(a)}

procedure mpf_todecimal_alt(const a: mp_float; ndd: word; str: pchar8; maxlen: word);
  {-convert an mp_float to decimal alternative representation, ndd>0 is the total}
  { number of digits, trailing '.' or '0' are suppressed. If a is too large or}
  { too small, scientific representation is used.}

procedure mpf_todecimal_n(const a: mp_float; ndd: word; str: pchar8; maxlen: word);
  {-convert an mp_float to decimal scientific representation, ndd>0 is the}
  { total number of digits (including one digit before the '.')}

function  mpf_todouble(const a: mp_float): double;
  {-convert a to double, +-inf if too large}

function  mpf_toextended(const a: mp_float): extended;
  {-convert a to extended, +-inf if too large}

procedure mpf_tohex_n(const a: mp_float; ndd: word; str: pchar8; maxlen: word);
  {-convert an mp_float to hexadecimal scientific representation, ndd>0 is the}
  { total number of digits (including one digit before the '.')}

procedure mpf_toradix_alt(const a: mp_float; radix, ndd: word; str: pchar8; maxlen: word);
  {-convert an mp_float to radix alternative representation, ndd>0 is the total}
  { number of digits, trailing '.' or '0' are suppressed. If a is too large or}
  { too small, scientific representation is used. NOTE: no radix prefix/suffix!}

procedure mpf_toradix_n(const a: mp_float; radix, ndd: word; str: pchar8; maxlen: word);
  {-convert an mp_float to radix scientific representation, ndd>0 is the}
  { total number of digits (including one digit before the '.')}

procedure mpf_trig(const a: mp_float; pc,ps,pt: pmp_float);
  {-calculate pc^=cos(a), ps^=sin(a), pt^=tan(a) if pointers are <> nil}

procedure mpf_trig_ex(const a: mp_float; mulpi: boolean; pc,ps,pt: pmp_float);
  {-calculate pc^=cos(a), ps^=sin(a), pt^=tan(a) if pointers are <> nil.}
  { If mulpi, calculate pc^=cos(a*Pi), ps^=sin(a*Pi), pt^=tan(a*Pi).}

procedure mpf_trunc(const a: mp_float; var b: mp_int);
  {-truncate mp_float to mp_int}

procedure mpf_writeln(const msg: mp_string; const a: mp_float; ndd: word);
  {-writeln a to output with leading msg}

procedure mpf_write_decimal(var tf: system.text; const a: mp_float; ndd: word);
  {-write an mp_float to file tf using decimal scientific representation, ndd>0}
  { is the total number of digits (including one digit before the '.')}

procedure mpf_write_decimal_alt(var tf: system.text; const a: mp_float; ndd: word);
  {-write an mp_float to file tf using decimal alternative representation, ndd>0 is}
  { the total number of digits, trailing '.' or '0' are suppressed. If a is too}
  { large or too small, scientific representation is used.}

procedure mpf_write_radix(var tf: system.text; const a: mp_float; radix,ndd: word);
  {-write an mp_float to file tf using radix scientific representation, ndd>0}
  { is the total number of digits (including one digit before the '.')}

procedure mpf_write_radix_alt(var tf: system.text; const a: mp_float; radix,ndd: word);
  {-write an mp_float to file tf using radix alternative representation, ndd>0 is}
  { the total number of digits, trailing '.' or '0' are suppressed. If a is too}
  { large or too small, scientific representation is used. NOTE: no radix prefix/suffix!}

{#Z+}
{---------------------------------------------------------------------------}
{- 'Internal' functions, don't use them unless you know what you are doing -}
{---------------------------------------------------------------------------}
{#Z-}

procedure s_mpf_abs(var a: mp_float);
  {-absolute value of an mp_float, no init check}

procedure s_mpf_add1(const a: mp_float; var b: mp_float);
  {-calculate b = a+1; no init checks}

procedure s_mpf_agm(const a,b: mp_float; var c: mp_float; ps: pmp_float);
  {-calculate c = AGM(|a|,|b|), if ps<>nil set ps^=sum(2^k*c_k^2); ps<>@c, no init check}

procedure s_mpf_ccell12(const k: mp_float; pCK, pCE: pmp_float);
  {-Complementary complete elliptic integrals of the 1st and 2nd kind using}
  { AGM algorithm; k>0 and pCK <> pCE, with init checks}

procedure s_mpf_chs(var a: mp_float);
  {-change sign of an mp_float, no init check}

function  s_mpf_cmp_mag(const a,b: mp_float): integer;
  {-compare magnitude of two mp_floats, return sign(|a|-|b|), no init check}

procedure s_mpf_dec1(var a: mp_float);
  {-calculate a = a-1; no init check}

procedure s_mpf_inc(var x: mp_float; const y: mp_float);
  {-calculate x = x+y}

procedure s_mpf_frac(const a: mp_float; var b: mp_float; podd: pBoolean);
  {-set b to the fractional part of a; frac(x)=x-int(x), if podd<>nil set podd^=odd(trunc(a))}

function  s_mpf_incf(var x: mp_float; const y: mp_float): boolean;
  {-calculate x = x+y; return true if x is changed}

procedure s_mpf_incexp(var a: mp_float; x: longint);
  {-increment a.exponent by x, do nothing if a.mantissa=0}

procedure s_mpf_inc1(var a: mp_float);
  {-calculate a = a+1; no init check}

function  s_mpf_is_ge0(const a: mp_float): boolean;
  {-return true if a>=0, no init checks}

function  s_mpf_is_le0(const a: mp_float): boolean;
  {-return true if a<=0, no init checks}

function  s_mpf_is_neg(const a: mp_float): boolean;
  {-return true if a<0, no init checks}

function  s_mpf_is0(const a: mp_float): boolean;
  {-return true if a=0, no init checks}

function  s_mpf_ldx(const a: mp_float): longint;
  {-return ldx(a) = a.exponent+mp_bitsize(a.mantissa), no init checks, a must be}
  { normalized, ldx(a) = floor(log2(|a|))+1, -MaxLongint if a=0, MaxLongint if overflow}

procedure s_mpf_mod_pi2k(const a: mp_float; k: integer; var b: mp_float; var oddm: boolean);
  {-calculate b = a mod pi*2^k, c in [0,pi*2^k); oddm: odd multiple of pi*2^k}
  { was used for reduction; no init check; extended precision used if necessary.}

procedure s_mpf_normalize(var a: mp_float);
  {-normalize an mp_float}

procedure s_mpf_normalizep(var a: mp_float; prec: longint);
  {-normalize an mp_float to bit precision prec}

procedure s_mpf_numbpart(n: longint; var p: mp_float);
  {-Compute number of partitions of n with Hardy-Ramanujan-Rademacher formula}

procedure s_mpf_toradix_n(const a: mp_float; radix, ndd: word; var maxlen: word; var pstr: pchar8);
  {-convert an mp_float to radix scientific representation, ndd>0 is the}
  { total number of digits (including one digit before the '.')}

procedure s_mpf_toradix_alt(const a: mp_float; radix, ndd: word; var maxlen: word; var pstr: pchar8);
  {-convert an mp_float to radix alternative representation, ndd>0 is the total}
  { number of digits, trailing '.' or '0' are suppressed. If a is too large or}
  { too small, scientific representation is used. NOTE: no radix prefix/suffix!}

{#Z+}
procedure _CheckBitPrec(const a: mp_float);
  {-Check a.bitprec; used for arg checks if init check is done otherwise}

procedure _set_ptab_const(var a: mp_float; e0,prec: longint; ptab: pointer);
  {-set a to [ptab] with bit precision prec; e0=ldx(a)}
{#Z-}


{$ifdef MPC_Old_EleFun}
{#Z+}
procedure mpf_10expt (const a: mp_float; var b: mp_float);
procedure mpf_2expt  (const a: mp_float; var b: mp_float);
procedure mpf_acosh  (const a: mp_float; var b: mp_float);
procedure mpf_acosh1p(const a: mp_float; var b: mp_float);
procedure mpf_asinh  (const a: mp_float; var b: mp_float);
procedure mpf_atanh  (const a: mp_float; var b: mp_float);
{#Z-}
{$endif}


implementation


uses
  mp_base, mp_rconp;

var
  mpf_default_prec : longint;  {current default bit precision, initial=240}

{$ifdef MPC_Old_EleFun}
procedure mpf_10expt (const a: mp_float; var b: mp_float); begin mpf_exp10(a,b);     end;
procedure mpf_2expt  (const a: mp_float; var b: mp_float); begin mpf_exp2(a,b);      end;
procedure mpf_acosh  (const a: mp_float; var b: mp_float); begin mpf_arccosh(a,b);   end;
procedure mpf_acosh1p(const a: mp_float; var b: mp_float); begin mpf_arccosh1p(a,b); end;
procedure mpf_asinh  (const a: mp_float; var b: mp_float); begin mpf_arcsinh(a,b);   end;
procedure mpf_atanh  (const a: mp_float; var b: mp_float); begin mpf_arctanh(a,b);   end;
{$endif}


{---------------------------------------------------------------------------}
procedure _CheckBitPrec(const a: mp_float);
  {-Check a.bitprec; used for arg checks if init check is done otherwise}
begin
  with a do begin
    if (bitprec<MPF_MIN_PREC) or (bitprec>MPF_MAX_PREC) then begin
      {$ifdef MPC_HaltOnError}
        {$ifdef MPC_UseExceptions}
          raise MPXRange.Create('_CheckBitPrec');
        {$else}
          RunError(MP_RTE_RANGE);
        {$endif}
      {$else}
        set_mp_error(MP_RANGE);
        exit;
      {$endif}
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure _set_ptab_const(var a: mp_float; e0,prec: longint; ptab: pointer);
  {-set a to [ptab] with bit precision prec; e0=ldx(a)}
var
  nb: word;
begin
  if mp_error<>MP_OKAY then exit;
  if prec<MPF_MIN_PREC then prec := MPF_MIN_PREC;
  if prec>MPF_MAX_PREC then prec := MPF_MAX_PREC;
  if prec>MAX_TCBITS   then prec := MAX_TCBITS;
  nb := (prec+7) div 8;
  {read an additional byte (if possible) for rounding}
  if nb<MAX_TCBITS div 8 then inc(nb);
  mp_read_unsigned_bin(a.mantissa, PByte(ptab)^,nb);
  a.exponent := e0-mp_bitsize(a.mantissa);
  a.bitprec  := prec;
  s_mpf_normalize(a);
end;


{---------------------------------------------------------------------------}
procedure mpf_exp10(const a: mp_float; var b: mp_float);
  {-calculate b = 10^a}
var
  t: mp_float;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    _CheckBitPrec(b);
  {$endif}
  if mpf_is0(a) then begin
    mpf_set1(b);
    exit;
  end;
  mpf_initp(t, b.bitprec+32);
  if mp_error=MP_OKAY then begin
    mpf_set_ln10(t);
    mpf_mul(t,a,t);
    mpf_exp(t,b);
    mpf_clear(t);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_exp2(const a: mp_float; var b: mp_float);
  {-calculate b = 2^a}
var
  t: mp_float;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    _CheckBitPrec(b);
  {$endif}
  if mpf_is0(a) then begin
    mpf_set1(b);
    exit;
  end;
  mpf_initp(t, b.bitprec+32);
  if mp_error=MP_OKAY then begin
    mpf_set_ln2(t);
    mpf_mul(t,a,t);
    mpf_exp(t,b);
    mpf_clear(t);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_abs(const a: mp_float; var b: mp_float);
  {-absolute value, b = |a|}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    _CheckBitPrec(a);
    _CheckBitPrec(b);
  {$endif}
  mp_abs(a.mantissa, b.mantissa);
  if @a<>@b then begin
    b.exponent := a.exponent;
    if a.bitprec<>b.bitprec then s_mpf_normalize(b);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_arccosh(const a: mp_float; var b: mp_float);
  {-calculate b = arccosh(a), a >= 1. Note: for a near 1 the function}
  { arccosh1p(a-1) should be used to reduce cancellation errors!}
var
  t: mp_float;
  l2: longint;
begin
  if mp_error<>MP_OKAY then exit;

  mpf_copyp(a,b);
  if mpf_is1(b) then begin
    mpf_set0(b);
    exit;
  end;

  l2 := s_mpf_ldx(b);
  if (l2 <= 0) or (s_mpf_is_neg(b)) then begin
    {if |a| < 1 then arccosh is undefined}
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mpf_arccosh: a < 1');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;

  if l2<2 then begin
    {if a<2 use arccosh(a) = arccosh(1+(a-1)) = arccosh1p(a-1)}
    s_mpf_dec1(b);
    mpf_arccosh1p(b,b);
    exit;
  end;

  mpf_initp(t, b.bitprec+32);
  if mp_error<>MP_OKAY then exit;

  if l2 > succ(t.bitprec div 2) then begin
    {arccosh(a) = ln(2a) if a^2+1=a^2}
    s_mpf_incexp(b,1);
    mpf_ln(b,b);
  end
  else begin
    {arccosh(a) = ln(a + sqrt(a^2 - 1))}
    mpf_sqr(b,t);
    s_mpf_dec1(t);
    mpf_sqrt(t,t);
    s_mpf_inc(t,b);
    mpf_ln(t,b);
  end;
  mpf_clear(t);
end;


{---------------------------------------------------------------------------}
procedure mpf_arccosh1p(const a: mp_float; var b: mp_float);
  {-calculate b = arccosh(1+a), a>=0}
var
  t: mp_float;
begin
  if mp_error<>MP_OKAY then exit;

  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b)then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_arccosh1p');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  if s_mpf_is0(a) then begin
    mpf_set0(b);
    exit;
  end;

  {a must be positive}
  if s_mpf_is_neg(a) then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mpf_arccosh1p: a < 0');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;
  mpf_initp(t, b.bitprec+32);
  if mp_error<>MP_OKAY then exit;

  {arccosh(x) = ln(x + sqrt(x^2 - 1)), substituting x=1+a gives}
  {arccosh1p(a) = ln1p(a+sqrt(a*(a+2)))}

  mpf_set_int(t,2);
  s_mpf_inc(t,a);
  mpf_mul(t,a,t);
  mpf_sqrt(t,t);
  s_mpf_inc(t,a);
  mpf_ln1p(t,b);

  mpf_clear(t);
end;


{---------------------------------------------------------------------------}
procedure mpf_add(const a,b: mp_float; var c: mp_float);
  {-calculate c = a+b}
var
  x: mp_float;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b) or mpf_not_init(c) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_add');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if s_mpf_is0(a) then begin
    mpf_copyp(b,c);
    exit;
  end;
  if s_mpf_is0(b) then begin
    mpf_copyp(a,c);
    exit;
  end;
  if @a=@b then begin
    mpf_mul_2k(a,1,c);
    exit;
  end;
  {make local copy of a}
  mpf_initp(x, c.bitprec);
  if mp_error=MP_OKAY then begin
    mpf_copyp(a,x);
    s_mpf_inc(x,b);
    mpf_exch(x,c);  {Note: x.bitprec=c.bitprec}
    mpf_clear(x);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_add_ext(const a: mp_float; b: extended; var c: mp_float);
  {-calculate c = a+b}
var
  x: mp_float;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(c) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_add_ext');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if s_mpf_is0(a) then begin
    mpf_set_ext(c,b);
    exit;
  end;
  if b=0.0 then begin
    mpf_copyp(a,c);
    exit;
  end;
  {make local copy of b}
  mpf_initp(x, c.bitprec);
  if mp_error=MP_OKAY then begin
    mpf_set_ext(x,b);
    s_mpf_inc(x,a);
    mpf_exch(x,c); {Note: x.bitprec=c.bitprec}
    mpf_clear(x);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_add_int(const a: mp_float; b: longint; var c: mp_float);
  {-calculate c = a+b}
var
  x: mp_float;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(c) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_add_int');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if s_mpf_is0(a) then begin
    mpf_set_int(c,b);
    exit;
  end;
  if b=0.0 then begin
    mpf_copyp(a,c);
    exit;
  end;
  {make local copy of b}
  mpf_initp(x, c.bitprec);
  if mp_error=MP_OKAY then begin
    mpf_set_int(x,b);
    s_mpf_inc(x,a);
    mpf_exch(x,c); {Note: x.bitprec=c.bitprec}
    mpf_clear(x);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_add_mpi(const a: mp_float; const b: mp_int; var c: mp_float);
  {-calculate c = a+b}
var
  x: mp_float;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mp_not_init(b) or mpf_not_init(c) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_add_mpi');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if s_mpf_is0(a) then begin
    mpf_set_mpi(c,b);
    exit;
  end;
  if mp_is0(b) then begin
    mpf_copyp(a,c);
    exit;
  end;
  {make local copy of b}
  mpf_initp(x, c.bitprec);
  if mp_error=MP_OKAY then begin
    mpf_set_mpi(x,b);
    s_mpf_inc(x,a);
    mpf_exch(x,c); {Note: x.bitprec=c.bitprec}
    mpf_clear(x);
  end;
end;


{$ifdef BIT32}
{---------------------------------------------------------------------------}
function mpf_adecimal(const a: mp_float; ndd: word): ansistring;
  {-Convert to decimal scientific representation with ndd digits, max 65000 digits}
const
  LMAX=65000;
var
  l,ls: word;
  pc: pchar8;
  {$ifndef RESULT}
    Result: ansistring;
  {$endif}
begin
  mpf_adecimal := '';
  {rough estimate > ndd + <1-> + 1<.> + 2<E-> + 12<expo>+ 1<#0>}
  {arg checks are done by mp_radix_size}
  if mp_error<>MP_OKAY then exit;
  if ndd+17<=LMAX then begin
    ls := ndd+17;
    SetLength(Result, ls);
    for l:=1 to length(Result) do Result[l] := #0;
    pc := @Result[1];
    s_mpf_toradix_n(a, 10, ndd, ls, pc);
    if mp_error=MP_OKAY then begin
      l := length(Result);
      {trim trailing #0}
      while (l>0) and (Result[l]=#0) do dec(l);
      if l<>length(Result) then SetLength(Result, l);
      {$ifndef RESULT}
        mpf_adecimal := Result;
      {$endif}
    end;
  end;
end;


{---------------------------------------------------------------------------}
function  mpf_adecimal_alt(const a: mp_float; ndd: word): ansistring;
  {-Convert to decimal alternative representation with ndd digits, max 65000 digits}
const
  LMAX=$FF00;
var
  l,ls: word;
  pc: pchar8;
  {$ifndef RESULT}
    Result: ansistring;
  {$endif}
begin
  mpf_adecimal_alt := '';
  {rough estimate > ndd + 1<-> + 1<.> + 2<E-> + 12<expo>+ 1<#0>}
  {arg checks are done by mp_radix_size}
  if mp_error<>MP_OKAY then exit;
  if ndd+17<=LMAX then begin
    ls := ndd+17;
    SetLength(Result, ls);
    for l:=1 to length(Result) do Result[l] := #0;
    pc := @Result[1];
    s_mpf_toradix_alt(a, 10, ndd, ls, pc);
    if mp_error=MP_OKAY then begin
      l := length(Result);
      {trim trailing #0}
      while (l>0) and (Result[l]=#0) do dec(l);
      if l<>length(Result) then SetLength(Result, l);
      {$ifndef RESULT}
        mpf_adecimal_alt := Result;
      {$endif}
    end;
  end;
end;
{$endif}


{---------------------------------------------------------------------------}
procedure mpf_agm(const a,b: mp_float; var c: mp_float);
  {-calculate c = AGM(|a|,|b|)}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b) or mpf_not_init(c) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_agm');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  s_mpf_agm(a,b,c,nil);
end;


{---------------------------------------------------------------------------}
procedure mpf_arccos(const a: mp_float; var b: mp_float);
  {-calculate b = arccos(a), |a| <= 1}
var
  t: mp_float;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b)then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_arccos');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  if s_mpf_is0(a) then begin
    mpf_set_pi2k(b,-1);
    exit;
  end;

  if mpf_is1a(a) then begin
    if s_mpf_is_neg(a) then mpf_set_pi(b) else mpf_set0(b);
    exit;
  end;

  if s_mpf_ldx(a) > 0 then begin
    {if |a| > 1 then arccos is undefined}
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mpf_arccos: |a| > 1');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;

  mpf_initp(t, b.bitprec+32);
  if mp_error<>MP_OKAY then exit;

  {arccos(a) := arctan2(sqrt(1 - a^2), a)}
  mpf_sqr(a,t);
  s_mpf_chs(t);
  s_mpf_inc1(t);
  mpf_sqrt(t,t);
  mpf_arctan2(t,a,b);

  mpf_clear(t);
end;


{---------------------------------------------------------------------------}
procedure mpf_arcsin(const a: mp_float; var b: mp_float);
  {-calculate b = arcsin(a), |a| <= 1}
var
  t: mp_float;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b)then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_arcsin');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  if s_mpf_is0(a) then begin
    mpf_set0(b);
    exit;
  end;

  if mpf_is1a(a) then begin
    if s_mpf_is_neg(a) then begin
      mpf_set_pi2k(b,-1);
      s_mpf_chs(b);
    end
    else mpf_set_pi2k(b,-1);
    exit;
  end;

  if s_mpf_ldx(a) > 0 then begin
    {if |a| > 1 then arcsin is undefined}
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mpf_arcsin: |a| > 1');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;

  mpf_initp(t, b.bitprec+32);
  if mp_error<>MP_OKAY then exit;

  {arcsin(a) := arctan2(a, sqrt(1 - a^2))}
  mpf_sqr(a,t);
  s_mpf_chs(t);
  s_mpf_inc1(t);
  mpf_sqrt(t,t);
  mpf_arctan2(a,t,b);

  mpf_clear(t);
end;


{---------------------------------------------------------------------------}
procedure mpf_arctan(const a: mp_float; var b: mp_float);
  {-calculate b = arctan(a)}
var
  x,y,t: mp_float;
  d: mp_int;
  i,f,bprec: longint;
  neg,inv: boolean;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b)then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_arctan');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  if s_mpf_is0(a) then begin
    mpf_set0(b);
    exit;
  end;

  {if a<0 then arctan(a)=-arctan(|a|)}
  neg := s_mpf_is_neg(a);

  {arctan(+-1) = +-pi/4}
  if mpf_is1a(a) then begin
    mpf_set_pi2k(b,-2);
    if neg then s_mpf_chs(b);
    exit;
  end;

  {save result precision and use slightly larger working precision}
  bprec := b.bitprec;
  i := bprec+32;
  if i>MPF_MAX_PREC then i:=MPF_MAX_PREC;

  mp_init(d);
  if mp_error<>MP_OKAY then exit;

  mpf_initp3(x,y,t,i);
  if mp_error<>MP_OKAY then begin
    mp_clear(d);
    exit;
  end;

  {set working precision for b, and x=|a|}
  s_mpf_normalizep(b,i);
  mpf_abs(a,x);

  {if x > 1 then arctan(x)=pi/2-arctan(1/x)}
  inv := s_mpf_ldx(x) > 0;
  if inv then mpf_inv(x,x);

  mpf_set1(t);
  f := 0;
  {Range reduction: make x < 2^-10}
  while s_mpf_ldx(x) >= -10 do begin
    {arctan(x) = 2*arctan(x/(1+sqrt(x*x+1)))}
    mpf_sqr(x,y);
    s_mpf_inc1(y);
    mpf_sqrt(y,y);
    s_mpf_inc1(y);
    mpf_div(x,y,x);
    inc(f);
    if mp_error<>MP_OKAY then break;
  end;

  {set y = x^2, and b to the first term}
  mpf_sqr(x,y);
  mpf_copy(x,b);

  {arctan(x) = sum( (-1)^i*x^(2i+1)/(2i+1) )}
  i := 1;
  repeat
    inc(i,2);
    {Too many terms or error?}
    if (i<0) or (mp_error<>MP_OKAY) then break;
    mp_set_int(d,i);
    if i and 3 = 3 then mp_set_int(d,-i) else mp_set_int(d,i);
    mpf_mul(x,y,x);
    mpf_div_mpi(x,d,t);
  until not s_mpf_incf(b,t);

  {undo range reduction}
  if f>0 then s_mpf_incexp(b, f);
  if inv then begin
    mpf_set_pi2k(t,-1);
    mpf_sub(t,b,b);
  end;

  {a was < 0}
  if neg then s_mpf_chs(b);

  {normalize to original precision}
  s_mpf_normalizep(b,bprec);

  mpf_clear3(x,y,t);
  mp_clear(d);
end;


{---------------------------------------------------------------------------}
procedure mpf_arctan2(const y,x: mp_float; var a: mp_float);
  {-calculate a = arctan(y/x) with special treatment for zero x or y. a is}
  { the principal value of arg(x + i*y), i.e. -pi < arctan2(y,x) <= pi.}
var
  xneg,yneg: boolean;
  t: mp_float;
begin
  {
  arctan2(0,0) =    0
  arctan2(z,z) =    pi/4  if z > 0
  arctan2(z,z) = -3*pi/4  if z < 0
  arctan2(y,0) =    0     if y > 0
  arctan2(y,0) =    pi    if y < 0
  arctan2(0,x) =    pi/2  if x > 0
  arctan2(0,x) =   -pi/2  if x > 0
  }

  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(x) or mpf_not_init(y) or mpf_not_init(a) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_arctan2');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  xneg := s_mpf_is_neg(x);

  if @x=@y then begin
    {special case if x same var as y: arg along the line y=x}
    if s_mpf_is0(x) then mpf_set0(a)
    else begin
      {pi/4 for x,y > 0 and -3*pi/4 for x,y < 0}
      mpf_set_pi2k(a,-2);
      if xneg then mpf_mul_int(a,-3,a);
    end;
    exit;
  end;

  if s_mpf_is0(y) then begin
    if xneg then mpf_set_pi(a) else mpf_set0(a);
    exit;
  end;

  if s_mpf_is0(x) then begin
    mpf_set_pi2k(a,-1);
    if s_mpf_is_neg(y) then s_mpf_chs(a);
    exit;
  end;

  yneg := s_mpf_is_neg(y);
  mpf_div(y,x,a);
  mpf_arctan(a,a);

  if xneg then begin
    mpf_initp(t,a.bitprec);
    if mp_error=MP_OKAY then begin
      mpf_set_pi(t);
      if yneg then mpf_sub(a,t,a)
      else mpf_add(a,t,a);
      mpf_clear(t);
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_arcsinh(const a: mp_float; var b: mp_float);
  {-calculate b = arcsinh(a)}
var
  t: mp_float;
  l2: longint;
  neg: boolean;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b)then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_arcsinh');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  if s_mpf_is0(a) then begin
    mpf_set0(b);
    exit;
  end;

  l2 := s_mpf_ldx(a);
  if l2 < -(b.bitprec+32) div 2 then begin
    {asin(a) = a*(1 - a^2/6 + O(a^4))}
    mpf_copyp(a,b);
    exit;
  end;

  mpf_initp(t, b.bitprec+32);
  if mp_error<>MP_OKAY then exit;

  neg := s_mpf_is_neg(a);
  mpf_abs(a,b);

  if l2>0 then begin
    {|a| >= 1.0}
    if l2 > succ(t.bitprec div 2) then begin
      {arcsinh(a) = ln(2a) if a^2+1=a^2}
      s_mpf_incexp(b,1);
      mpf_ln(b,b);
    end
    else begin
      {arcsinh(a) = ln(a + sqrt(a^2 + 1))}
      mpf_sqr(a,t);
      s_mpf_inc1(t);
      mpf_sqrt(t,t);
      s_mpf_inc(t,b);
      mpf_ln(t,b);
    end;
  end
  else begin
    {arcsinh(a) = ln1p(a*(1 + a/(1 + sqrt(1 + a^2))))}
    mpf_sqr(a,t);
    s_mpf_inc1(t);
    mpf_sqrt(t,t);
    s_mpf_inc1(t);
    mpf_div(b,t,t);
    s_mpf_inc1(t);
    mpf_mul(b,t,t);
    mpf_ln1p(t,b);
  end;

  if neg then s_mpf_chs(b);

  mpf_clear(t);
end;


{---------------------------------------------------------------------------}
procedure mpf_arctanh(const a: mp_float; var b: mp_float);
  {-calculate b = arctanh(a), |a| < 1}
var
  t: mp_float;
  neg: boolean;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b)then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_arctanh');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  if s_mpf_is0(a) then begin
    mpf_set0(b);
    exit;
  end;

  if s_mpf_ldx(a) > 0 then begin
    {if |a| >= 1 then arctanh is undefined}
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mpf_arctanh: |a| >= 1');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;

  mpf_initp(t, b.bitprec+32);
  if mp_error<>MP_OKAY then exit;

  {arctanh(x) =  0.5*ln((1+x)/(1-x)) = -0.5*ln((1-x)/(1+x))}
  {         = -0.5*ln(1-2x/(1+x))  = -0.5*ln1p(-2x/(1+x))}

  neg := not s_mpf_is_neg(a);
  mpf_abs(a,b);
  s_mpf_add1(b,t);
  mpf_div(b,t,t);
  s_mpf_incexp(t,1);
  s_mpf_chs(t);
  mpf_ln1p(t,b);
  s_mpf_incexp(b,-1);
  if neg then s_mpf_chs(b);

  mpf_clear(t);
end;


{---------------------------------------------------------------------------}
procedure mpf_ccell1(const k: mp_float; var CK: mp_float);
  {-Complementary complete elliptic integral of the first kind CK = CK(k)}
  { with k>0 using AGM algorithm}
var
  s: mp_float;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(k) or mpf_not_init(CK) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_ccell1');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  if s_mpf_is_le0(k) then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXBadArg.Create('mpf_ccell1: k <= 0');
      {$else}
        RunError(MP_RTE_BADARG);
      {$endif}
    {$else}
      set_mp_error(MP_BADARG);
      exit;
    {$endif}
  end;

  mpf_initp(s,CK.bitprec+32);

  {calculate AGM(1,k)}
  mpf_set1(s);
  s_mpf_agm(s,k,s,nil);

  {CK = pi/2/AGM = pi/2/CK}
  mpf_set_pi2k(CK,-1);
  mpf_div(CK,s,CK);

  mpf_clear(s);
end;


{---------------------------------------------------------------------------}
procedure mpf_ccell12(const k: mp_float; var CK, CE: mp_float);
  {-Complementary complete elliptic integrals of the 1st and 2nd kind using}
  { AGM algorithm; k>0 and  @CK <> @CE}
begin
  s_mpf_ccell12(k, @CK, @CE);
end;


{---------------------------------------------------------------------------}
procedure mpf_ccell2(const k: mp_float; var CE: mp_float);
  {-Complementary complete elliptic integral of the 2nd kind CE = CE(k)}
  { with k>0 using AGM algorithm. Special case of mpf_ccell12}
begin
  s_mpf_ccell12(k, nil, @CE);
end;


{---------------------------------------------------------------------------}
function mpf_checksum(const a: mp_float): longint;
  {-return a checksum for a, -1 if mp_error<>MP_OKAY, -2 if not initialized}
var
  adler: longint;
begin
  mpf_checksum := -1;
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) then begin
      mpf_checksum := -2;
      exit;
    end;
  {$endif}
  adler := mp_checksum(a.mantissa);
  with a do begin
    s_mp_checksum(adler,@exponent,sizeof(exponent));
    s_mp_checksum(adler,@bitprec, sizeof(bitprec));
  end;
  mpf_checksum := adler;
end;


{---------------------------------------------------------------------------}
procedure mpf_chg_prec(var a: mp_float; newprec: longint);
  {-change bitprec of a to newprec}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_chg_prec');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
    if (newprec<MPF_MIN_PREC) or (newprec>MPF_MAX_PREC) then begin
      {$ifdef MPC_HaltOnError}
        {$ifdef MPC_UseExceptions}
          raise MPXRange.Create('mpf_chg_prec: newprec out of range');
        {$else}
          RunError(MP_RTE_RANGE);
        {$endif}
      {$else}
        set_mp_error(MP_RANGE);
        exit;
      {$endif}
    end;
  {$endif}
  s_mpf_normalizep(a, newprec);
end;


{---------------------------------------------------------------------------}
procedure mpf_chs(const a: mp_float; var b: mp_float);
  {-change sign, b = -a}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    _CheckBitPrec(a);
    _CheckBitPrec(b);
  {$endif}
  mp_chs(a.mantissa, b.mantissa);
  if @a<>@b then begin
    b.exponent := a.exponent;
    if a.bitprec<>b.bitprec then s_mpf_normalize(b);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_clear(var a: mp_float);
  {-clear an mp_float}
begin
  with a do begin
    bitprec := 0;
    exponent:= 0;
    mp_clear(mantissa);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_clear2(var a,b: mp_float);
  {-clear 2 mp_floats}
begin
  mpf_clear(a);
  mpf_clear(b);
end;


{---------------------------------------------------------------------------}
procedure mpf_clear3(var a,b,c: mp_float);
  {-clear 3 mp_floats}
begin
  mpf_clear2(a,b);
  mpf_clear(c);
end;


{---------------------------------------------------------------------------}
procedure mpf_clear4(var a,b,c,d: mp_float);
  {-clear 4 mp_floats}
begin
  mpf_clear2(a,b);
  mpf_clear2(c,d);
end;


{---------------------------------------------------------------------------}
procedure mpf_clear5(var a,b,c,d,e: mp_float);
  {-clear 5 mp_floats}
begin
  mpf_clear2(a,b);
  mpf_clear2(c,d);
  mpf_clear(e);
end;


{---------------------------------------------------------------------------}
function mpf_cmp_ext(const a: mp_float; b: extended): integer;
  {-compare a and b, return sign(a-b)}
var
  x: mp_float;
begin
  mpf_cmp_ext := 0;
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_cmp_ext');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  {make local copy of b}
  mpf_initp(x, a.bitprec);
  if mp_error=MP_OKAY then begin
    mpf_set_ext(x,b);
    mpf_cmp_ext := mpf_cmp(a,x);
    mpf_clear(x);
  end;
end;


{---------------------------------------------------------------------------}
function mpf_cmp(const a,b: mp_float): integer;
  {-compare two mp_floats, return sign(a-b)}
begin
  {Value for last alternative, keep D6+ happy}
  mpf_cmp := 0;
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_cmp');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  {compare based on sign}
  if a.mantissa.sign<>b.mantissa.sign then begin
    if a.mantissa.sign=MP_NEG then mpf_cmp := -1 else mpf_cmp := 1;
    exit;
  end;
  {compare magnitude}
  if a.mantissa.sign=MP_NEG then begin
    {if negative compare opposite direction}
    mpf_cmp := s_mpf_cmp_mag(b, a);
  end
  else mpf_cmp := s_mpf_cmp_mag(a, b);
end;


{---------------------------------------------------------------------------}
function mpf_cmp_mag(const a,b: mp_float): integer;
  {-compare magnitude of two mp_floats, return sign(|a|-|b|)}
begin
  {Value for last alternative, keep D6+ happy}
  mpf_cmp_mag := 0;
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_cmp_mag');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  mpf_cmp_mag := s_mpf_cmp_mag(a,b);
end;


{---------------------------------------------------------------------------}
function mpf_cmp_mag_ext(const a: mp_float; b: extended): integer;
  {-compare magnitude of a and b, return sign(|a|-|b|)}
var
  x: mp_float;
begin
  mpf_cmp_mag_ext := 0;
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_cmp_mag_ext');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  {make local copy of b}
  mpf_initp(x, a.bitprec);
  if mp_error=MP_OKAY then begin
    mpf_set_ext(x,b);
    mpf_cmp_mag_ext := mpf_cmp_mag(a,x);
    mpf_clear(x);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_copy(const a: mp_float; var b: mp_float);
  {-copy a to b with b.bitprec=a.bitprec}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    _CheckBitPrec(a);
  {$endif}
  if @a<>@b then begin
    b.exponent:= a.exponent;
    b.bitprec := a.bitprec;
    mp_copy(a.mantissa, b.mantissa);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_copyp(const a: mp_float; var b: mp_float);
  {-copy a to b, preserve b.bitprec}
var
  pb: longint;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    _CheckBitPrec(b);
  {$endif}
  pb := b.bitprec;
  mpf_copy(a,b);
  if b.bitprec<>pb then s_mpf_normalizep(b, pb);
end;


{---------------------------------------------------------------------------}
procedure mpf_cos(const a: mp_float; var b: mp_float);
  {-calculate b = cos(a)}
begin
  mpf_trig(a, @b, nil, nil);
end;


{---------------------------------------------------------------------------}
procedure mpf_cosh(const a: mp_float; var b: mp_float);
  {-calculate b = cosh(a), a < 2^31 * ln(2)}
var
  t: mp_float;
  pb: longint;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b)then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_cosh');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  if s_mpf_is0(a) then begin
    mpf_set1(b);
    exit;
  end;

  pb := b.bitprec;
  mpf_initp(t, pb+32);
  if mp_error<>MP_OKAY then exit;

  mpf_abs(a,t);
  b.bitprec := t.bitprec;

  {cosh(t) = (exp(|t|)+exp(-|t|))/2}
  mpf_exp(t,b);

  {don't calculate inverse if it is to small}
  if s_mpf_ldx(b) < 16 + b.bitprec div 2 then begin
    mpf_inv(b,t);
    mpf_add(b,t,b);
  end;
  s_mpf_incexp(b,-1);
  s_mpf_normalizep(b,pb);

  mpf_clear(t);
end;


{---------------------------------------------------------------------------}
function mpf_decimal(const a: mp_float; ndd: word): mp_string;
  {-Convert to decimal scientific representation with ndd digits, max 255 chars}
var
  i: integer;
  {$ifdef FPC}
    d1,d2: cardinal;
  {$else}
    d: longint;
  {$endif}
  pc: pchar8;
  iostr: string[255];
begin
  {arg checks are done by mpf_toradix_n}
  mpf_decimal := '';
  if (mp_error<>MP_OKAY) or (ndd=0) then exit;
  pc := @iostr[1];
  mpf_toradix_n(a, 10, ndd, pc, 255);
  if mp_error<>MP_OKAY then exit;
  i := 0;
  {$ifdef FPC}
    d1 := cardinal(pc);
    d2 := cardinal(@iostr[1]);
    if (d2>d1) and (d2<d1+255) then i := d2-d1;
  {$else}
    d := longint(pc)-longint(@iostr[1]);
    if (d>0) and (d<255) then i := d;
  {$endif}
  if (i>0) and (i<255) and (iostr[i]<>#0) and (iostr[i+1]=#0) then iostr[0] := char8(i)
  else begin
    iostr[0] := #255;
    for i:=1 to 255 do begin
      if iostr[i]=#0 then begin
        iostr[0] := char8(i-1);
        break;
      end;
    end;
  end;
  mpf_decimal := iostr;
end;


{---------------------------------------------------------------------------}
function mpf_decimal_alt(const a: mp_float; ndd: word): mp_string;
  {-Convert to decimal alternative representation with ndd digits, max 255 chars}
var
  i: integer;
  {$ifdef FPC}
    d1,d2: cardinal;
  {$else}
    d: longint;
  {$endif}
  pc: pchar8;
  iostr: string[255];
begin
  {arg checks are done by mpf_toradix_n}
  mpf_decimal_alt := '';
  if (mp_error<>MP_OKAY) or (ndd=0) then exit;
  pc := @iostr[1];
  mpf_toradix_alt(a, 10, ndd, pc, 255);
  if mp_error<>MP_OKAY then exit;
  i := 0;
  {$ifdef FPC}
    d1 := cardinal(pc);
    d2 := cardinal(@iostr[1]);
    if (d2>d1) and (d2<d1+255) then i := d2-d1;
  {$else}
    d := longint(pc)-longint(@iostr[1]);
    if (d>0) and (d<255) then i := d;
  {$endif}
  if (i>0) and (i<255) and (iostr[i]<>#0) and (iostr[i+1]=#0) then iostr[0] := char8(i)
  else begin
    iostr[0] := #255;
    for i:=1 to 255 do begin
      if iostr[i]=#0 then begin
        iostr[0] := char8(i-1);
        break;
      end;
    end;
  end;
  mpf_decimal_alt := iostr;
end;


{---------------------------------------------------------------------------}
procedure mpf_div(const a,b: mp_float; var c: mp_float);
  {-calculate c = a/b}
var
  x: mp_float;
  diff: longint;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b) or mpf_not_init(c) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_div');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  if s_mpf_is0(b) then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mpf_div: b=0');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;

  if s_mpf_is0(a) then begin
    mpf_set0(c);
    exit;
  end;

  if @a=@b then begin
    mpf_set1(c);
    exit;
  end;

  {make local copy of a with precision c.bitprec}
  mpf_initp(x, c.bitprec);
  if mp_error<>MP_OKAY then exit;
  mpf_copyp(a,x);

  diff :=  mp_bitsize(b.mantissa) - mp_bitsize(x.mantissa) + 1 + x.bitprec;
  s_mpf_incexp(x, -b.exponent);
  if diff>0 then begin
    s_mpf_incexp(x, -diff);
    mp_shl(x.mantissa, diff, x.mantissa);
  end;
  mp_div(x.mantissa, b.mantissa, x.mantissa);
  s_mpf_normalize(x);
  mpf_exch(x,c); {Note: x.bitprec=c.bitprec}
  mpf_clear(x);
end;


{---------------------------------------------------------------------------}
procedure mpf_div_d(const a: mp_float; b: mp_digit; var c: mp_float);
  {-calculate c = a/b}
var
  diff: longint;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(c) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_div_d');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if b=0 then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mpf_div_d: b=0');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;
  if s_mpf_is0(a) then begin
    mpf_set0(c);
    exit;
  end;

  mpf_copyp(a,c);
  if b=1 then exit;

  {same calculation as mpf_div with b.exponent=0, b.mantissa=b}
  diff :=  bitsize32(b) - mp_bitsize(c.mantissa) + 1 + c.bitprec;
  if diff>0 then begin
    s_mpf_incexp(c, -diff);
    mp_shl(c.mantissa, diff, c.mantissa);
  end;
  mp_div_d(c.mantissa, b, @c.mantissa, b);
  s_mpf_normalize(c);
end;


{---------------------------------------------------------------------------}
procedure mpf_div_ext(const a: mp_float; b: extended; var c: mp_float);
  {-calculate c = a/b}
var
  x: mp_float;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(c) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_div_ext');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if b=0.0 then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mpf_div_ext: b=0');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;
  mpf_initp(x,c.bitprec);
  if mp_error=MP_OKAY then begin
    mpf_set_ext(x,b);
    mpf_div(a,x,c);
    mpf_clear(x);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_div_int(const a: mp_float; b: longint; var c: mp_float);
  {-calculate c = a/b}
var
  diff: longint;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(c) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_div_int');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if b=0 then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mpf_div_int: b=0');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;
  if s_mpf_is0(a) then begin
    mpf_set0(c);
    exit;
  end;
  mpf_copyp(a,c);
  if abs(b)=1 then begin
    if b<0 then s_mpf_chs(c);
    exit;
  end;
  {same calculation as mpf_div with b.exponent=0, b.mantissa=b}
  diff :=  bitsize32(abs(b)) - mp_bitsize(c.mantissa) + 1 + c.bitprec;
  if diff>0 then begin
    s_mpf_incexp(c, -diff);
    mp_shl(c.mantissa, diff, c.mantissa);
  end;
  mp_div_int(c.mantissa, b, @c.mantissa, diff);
  s_mpf_normalize(c);
end;


{---------------------------------------------------------------------------}
procedure mpf_div_mpi(const a: mp_float; const b: mp_int; var c: mp_float);
  {-calculate c = a/b}
var
  diff: longint;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mp_not_init(b) or mpf_not_init(c) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_div_mpi');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if mp_is0(b) then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mpf_div_mpi: b=0');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;
  if s_mpf_is0(a) then begin
    mpf_set0(c);
    exit;
  end;
  {same calculation as mpf_div with b.exponent=0, b.mantissa=b}
  mpf_copyp(a,c);
  diff :=  mp_bitsize(b) - mp_bitsize(c.mantissa) + 1 + c.bitprec;
  if diff>0 then begin
    s_mpf_incexp(c, -diff);
    mp_shl(c.mantissa, diff, c.mantissa);
  end;
  mp_div(c.mantissa, b, c.mantissa);
  s_mpf_normalize(c);
end;


{---------------------------------------------------------------------------}
procedure mpf_exch(var a,b: mp_float);
  {-exchange two mp_floats (including bitprec!!)}
var
  t: mp_float;
begin
  if mp_error=MP_OKAY then begin
    t := a;
    a := b;
    b := t;
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_exp(const a: mp_float; var b: mp_float);
  {-calculate b = exp(a), a < 2^31 * ln(2)}
var
  x: mp_float;
  c,f,n,l2,bprec: longint;
  neg: boolean;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b)then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_exp');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  if s_mpf_is0(a) then begin
    mpf_set1(b);
    exit;
  end;

  {Overflow for a > 2^31 * ln(2)}
  l2 := s_mpf_ldx(a);
  {Safe first check for over/underflow. Does not catch x=1.49E9 .. 2.14E9}
  if l2 > 30 then begin
    if s_mpf_is_neg(a) then begin
      mpf_set0(b);
      exit;
    end
    else if l2>31 then begin
      {$ifdef MPC_HaltOnError}
        {$ifdef MPC_UseExceptions}
          raise MPXOverflow.Create('mpf_exp');
        {$else}
          RunError(MP_RTE_OVRFLOW);
        {$endif}
      {$else}
        set_mp_error(MP_OVERFLOW);
        exit;
      {$endif}
    end;
  end;

  {save result precision and use larger working precision}
  bprec := b.bitprec;
  {get approx. sqr count in reconstruction step}
  l2:= s_mpf_ldx(a);
  c := trunc(sqrt(0.5*b.bitprec));
  f := 32+l2+c;
  if f<32 then f := 32;
  n := bprec+f;
  if n>MPF_MAX_PREC then n:=MPF_MAX_PREC;

  mpf_initp(x,n);
  if mp_error<>MP_OKAY then exit;

  {if a<0, use exp(a)=1/exp(|a|)}
  neg := s_mpf_is_neg(a);
  mpf_abs(a,x);

  {set working precision for b, init series with 1}
  b.bitprec := n;
  mpf_set1(b);

  {Algorithm from T. Papanikolaou [27], section 4.4.2}
  with x do begin
    {calculate number of steps and perform range reduction if necessary}
    l2:= s_mpf_ldx(x);
    c := trunc(sqrt(0.5*bitprec)); {Note: c>= sqrt(37/2) > 4}
    f := 1 + l2 + c;
    if f>0 then begin
      {Range reduction to (0..2^-c)}
      s_mpf_incexp(x, -f);
      n := 1 + bitprec div c;
    end
    else begin
      {0>=f = 1+l2+c --> l2 <= -1-c < -5  --> abs(l2) > 5}
      n := 1 + bitprec div abs(l2);
    end;
  end;

  {Taylor sum from n downto 0, b is initialized with 1}
  while n>0 do begin
    mpf_mul(b,x,b);
    if n <= longint(MP_DIGIT_MAX) then mpf_div_d(b,mp_digit(n),b)
    else mpf_div_int(b,n,b);
    s_mpf_inc1(b);
    dec(n);
  end;

  {if f>0, reconstruct result from range reduction}
  for n:=1 to f do mpf_sqr(b,b);

  {if a<0 uses exp(a)=1/exp(|a|)}
  if neg then mpf_inv(b,b);

  {normalize to original precision}
  s_mpf_normalizep(b,bprec);

  mpf_clear(x);
end;


{---------------------------------------------------------------------------}
procedure mpf_expm1(const a: mp_float; var b: mp_float);
  {-calculate b = exp(a)-1, a < 2^31 * ln(2); special version for small a}
var
  x,y: mp_float;
  n,l2,bprec: longint;
  neg: boolean;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_expm1');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  if s_mpf_is0(a) then begin
    mpf_set0(b);
    exit;
  end;

  if s_mpf_ldx(a) >= 0 then begin
    {if |a| > 0.5 then use standard function b = exp(a)-1}
    mpf_exp(a,b);
    s_mpf_dec1(b);
    exit;
  end;

  {save result precision and use slightly larger working precision}
  bprec := b.bitprec;
  mpf_initp2(x,y,bprec+32);
  if mp_error<>MP_OKAY then exit;

  {if a<0, note sign and calculate expm1(|a|)}
  neg := s_mpf_is_neg(a);
  mpf_abs(a,x);

  {range reduction x < 1/2^16}
  l2 := s_mpf_ldx(x) + 16;
  if l2>0 then dec(x.exponent,l2);

  {sum Taylor series for exp(x)-1}
  mpf_copy(x,y);
  mpf_copy(x,b);
  n := 1;
  repeat
    inc(n);
    mpf_mul(y,x,y);
    mpf_div_int(y,n,y);
    if mp_error<>MP_OKAY then break;
  until not s_mpf_incf(b,y);

  {undo range reduction, expm1(2x)=expm1(x)*(2+expm1(x))}
  if l2>0 then begin
    mpf_set_int(x,2);
    for n:=1 to l2 do begin
      mpf_add(b,x,y);
      mpf_mul(b,y,b);
    end;
  end;

  {if a<0 uses expm1(-a)=-expm1(a)/(1+expm1(a))}
  if neg then begin
    mpf_chs(b,x);
    s_mpf_inc1(b);
    mpf_div(x,b,b);
  end;

  {normalize to original precision}
  s_mpf_normalizep(b,bprec);

  mpf_clear2(x,y);
end;


{---------------------------------------------------------------------------}
procedure mpf_expt(const a,b: mp_float; var c: mp_float);
  {-calculate c = a^b, a>0}
var
  t: mp_float;
  p: longint;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b) or mpf_not_init(c) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_expt');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if s_mpf_is_le0(a) then p:=0
  else begin
    p := abs(s_mpf_ldx(a));
    if p<32 then p := 32;
  end;
  mpf_initp(t, c.bitprec+p);
  if mp_error=MP_OKAY then begin
    mpf_ln(a,t);
    mpf_mul(t,b,t);
    mpf_exp(t,c);
    mpf_clear(t);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_expt1pm1(const a,b: mp_float; var c: mp_float);
  {-calculate c = (1+a)^b-1, a>-1}
var
  t: mp_float;
  p: longint;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b) or mpf_not_init(c) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_expt1pm1');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if s_mpf_is_le0(a) then p:=0
  else begin
    p := abs(s_mpf_ldx(a));
    if p<32 then p := 32;
  end;
  mpf_initp(t, c.bitprec+p);
  if mp_error=MP_OKAY then begin
    mpf_ln1p(a,t);
    mpf_mul(t,b,t);
    mpf_expm1(t,c);
    mpf_clear(t);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_exptm1(const a,b: mp_float; var c: mp_float);
  {-calculate c = a^b-1, a>0}
var
  t: mp_float;
  p: longint;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b) or mpf_not_init(c) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_exptm1');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if s_mpf_is_le0(a) then p:=0
  else begin
    p := abs(s_mpf_ldx(a));
    if p<32 then p := 32;
  end;
  mpf_initp(t, c.bitprec+p);
  if mp_error=MP_OKAY then begin
    mpf_ln(a,t);
    mpf_mul(t,b,t);
    mpf_expm1(t,c);
    mpf_clear(t);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_expt_int(const a: mp_float; b: longint; var c: mp_float);
  {-calculate c = a^b}
var
  x: mp_float;
begin

  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(c) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_expt_int');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  if b<0 then begin
    if b = -MaxLongint then begin
      {$ifdef MPC_HaltOnError}
        {$ifdef MPC_UseExceptions}
          raise MPXBadArg.Create('mpf_expt_int: b = -MaxLongint');
        {$else}
          RunError(MP_RTE_BADARG);
        {$endif}
      {$else}
        set_mp_error(MP_BADARG);
        exit;
      {$endif}
    end
    else begin
      {c = (a^-1)^(-b)}
      {will give error if a=0}
      mpf_inv(a,c);
      mpf_expt_int(c, -b, c);
      exit;
    end
  end;

  {easy outs}
  if s_mpf_is0(a) then begin
    mpf_set0(c);
    exit;
  end;
  if b=0 then begin
    mpf_set1(c);
    exit;
  end
  else if b=1 then begin
    mpf_copyp(a,c);
    exit;
  end;

  {make local copy of a}
  mpf_initp(x, c.bitprec);
  if mp_error<>MP_OKAY then exit;

  mpf_copyp(a,x);
  mpf_set1(c);

  {initially b>=2}
  while mp_error=MP_OKAY do begin
    if odd(b) then mpf_mul(c,x,c);
    b := b shr 1;
    if b=0 then break else mpf_sqr(x,x);
  end;

  mpf_clear(x);
end;


{---------------------------------------------------------------------------}
procedure mpf_frac(const a: mp_float; var b: mp_float);
  {-set b to the fractional part of a; frac(x)=x-int(x)}
begin
  s_mpf_frac(a,b,nil);
end;


{---------------------------------------------------------------------------}
function mpf_get_default_prec: longint;
  {-return current default (bit) precision, initial=240}
begin
  mpf_get_default_prec := mpf_default_prec;
end;


{---------------------------------------------------------------------------}
procedure mpf_iexpt(a: longint; const b: mp_float; var c: mp_float);
  {-calculate c = a^b, a>0}
var
  t: mp_float;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    _CheckBitPrec(c);
  {$endif}
  {Essentially a copy of mpf_expt in order to avoid to temporary mp_floats}
  mpf_initp(t, c.bitprec);
  if mp_error=MP_OKAY then begin
    mpf_set_int(t,a);
    mpf_ln(t,t);
    mpf_mul(t,b,t);
    mpf_exp(t,c);
    mpf_clear(t);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_init(var a: mp_float);
  {-initialize an mp_float with default precision}
begin
  mpf_initp(a, mpf_default_prec);
end;


{---------------------------------------------------------------------------}
procedure mpf_init2(var a,b: mp_float);
  {-initialize two mp_floats with default precision}
begin
  mpf_initp2(a,b,mpf_default_prec);
end;


{---------------------------------------------------------------------------}
procedure mpf_init3(var a,b,c: mp_float);
  {-initialize 3 mp_floats with default precision}
begin
  mpf_initp3(a,b,c,mpf_default_prec);
end;


{---------------------------------------------------------------------------}
procedure mpf_init4(var a,b,c,d: mp_float);
  {-initialize 4 mp_floats with default precision}
begin
  mpf_initp4(a,b,c,d,mpf_default_prec);
end;


{---------------------------------------------------------------------------}
procedure mpf_init5(var a,b,c,d,e: mp_float);
  {-initialize 5 mp_floats with default precision}
begin
  mpf_initp5(a,b,c,d,e,mpf_default_prec);
end;


{---------------------------------------------------------------------------}
procedure mpf_initp(var a: mp_float; prec: longint);
  {-initialize an mp_float with bit precision prec}
begin
  if mp_error<>MP_OKAY then exit;
  if prec<MPF_MIN_PREC then prec := MPF_MIN_PREC;
  if prec>MPF_MAX_PREC then prec := MPF_MAX_PREC;
  with a do begin
    bitprec := prec;
    exponent:= 0;
    mp_init_size(mantissa, (prec+pred(DIGIT_BIT)) div DIGIT_BIT);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_initp2(var a,b: mp_float; prec: longint);
  {-initialize two mp_floats with bit precision prec}
var
  pa: array[0..1] of pmp_float;
begin
  pa[0] := @a;
  pa[1] := @b;
  mpf_initp_multi_p(pa, prec);
end;


{---------------------------------------------------------------------------}
procedure mpf_initp3(var a,b,c: mp_float; prec: longint);
  {-initialize 3 mp_floats with bit precision prec}
var
  pa: array[0..2] of pmp_float;
begin
  pa[0] := @a;
  pa[1] := @b;
  pa[2] := @c;
  mpf_initp_multi_p(pa, prec);
end;


{---------------------------------------------------------------------------}
procedure mpf_initp4(var a,b,c,d: mp_float; prec: longint);
  {-initialize 4 mp_floats with bit precision prec}
var
  pa: array[0..3] of pmp_float;
begin
  pa[0] := @a;
  pa[1] := @b;
  pa[2] := @c;
  pa[3] := @d;
  mpf_initp_multi_p(pa, prec);
end;


{---------------------------------------------------------------------------}
procedure mpf_initp5(var a,b,c,d,e: mp_float; prec: longint);
  {-initialize 5 mp_floats with bit precision prec}
var
  pa: array[0..4] of pmp_float;
begin
  pa[0] := @a;
  pa[1] := @b;
  pa[2] := @c;
  pa[3] := @d;
  pa[4] := @e;
  mpf_initp_multi_p(pa, prec);
end;


{---------------------------------------------------------------------------}
procedure mpf_initp_multi_p(var pv: array of pmp_float; prec: longint);
  {-initialize with bit precision prec a list of mp_floats given as a pointer}
  { vector; on error the already initialized mp_floats will be cleared}
var
  i,k: integer;
begin
  if mp_error<>MP_OKAY then exit;
  for i:=low(pv) to high(pv) do begin
    mpf_initp(pv[i]^, prec);
    if mp_error<>MP_OKAY then begin
      {error, clear all previous mp_floats}
      for k:=low(pv) to i-1 do mpf_clear(pv[k]^);
      break;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_int(const a: mp_float; var b: mp_float);
  {-set b to the integer part of a; i.e. is b rounded toward zero}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_int');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  mpf_copyp(a,b);
  with b do begin
    if exponent < 0 then begin
      mp_shr(mantissa,-exponent,mantissa);
      exponent := 0;
    end;
  end;
  s_mpf_normalize(b);
end;


{---------------------------------------------------------------------------}
procedure mpf_inv(const a: mp_float; var b: mp_float);
  {-calculate b = 1/a}
var
  x: mp_float;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_inv');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  mpf_init(x);
  if mp_error=MP_OKAY then begin
    mpf_set1(x);
    mpf_div(x,a,b);
    mpf_clear(x);
  end;
end;


{---------------------------------------------------------------------------}
function mpf_is0(const a: mp_float): boolean;
  {-return true if a=0}
begin
  {$ifdef MPC_ArgCheck}
    _CheckBitPrec(a);
  {$endif}
  mpf_is0 := (a.exponent=0) and mp_is0(a.mantissa);
end;


{---------------------------------------------------------------------------}
function mpf_is1(const a: mp_float): boolean;
  {-return true if a=1}
var
  n: longint;
begin
  {init check in mp_is_pow2}
  mpf_is1 := (a.mantissa.sign=MP_ZPOS) and mp_is_pow2(a.mantissa,n) and ((a.exponent+n)=0);
end;


{---------------------------------------------------------------------------}
function mpf_is1a(const a: mp_float): boolean;
  {-return true if abs(a)=1}
var
  n: longint;
begin
  {init check in mp_is_pow2}
  mpf_is1a := mp_is_pow2(a.mantissa,n) and ((a.exponent+n)=0);
end;


{---------------------------------------------------------------------------}
function mpf_is_eq(const a,b: mp_float): boolean;
  {-return a = b}
begin
  mpf_is_eq := mpf_cmp(a,b)=MP_EQ;
end;


{---------------------------------------------------------------------------}
function mpf_is_eq_rel(const a,b: mp_float): boolean;
  {-Check if |a-b| <= r*2^(1-b.bitprec);  r=1 if b=0, r=|b| otherwise}
var
  c,d: mp_float;
begin
  mpf_is_eq_rel := false;
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_is_eq_rel');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  mpf_initp2(c,d,b.bitprec);
  if mp_error<>MP_OKAY then exit;
  if s_mpf_is0(b) then mpf_set1(d) else mpf_abs(b,d);
  mpf_mul_2k(d,1-b.bitprec,d);
  mpf_sub(a,b,c);
  mpf_is_eq_rel := mpf_cmp_mag(c,d)<>MP_GT;
  mpf_clear2(c,d);
end;


{---------------------------------------------------------------------------}
function mpf_is_ge(const a,b: mp_float): boolean;
  {-return a >= b}
begin
  mpf_is_ge := mpf_cmp(a,b)<>MP_LT;
end;


{---------------------------------------------------------------------------}
function mpf_is_gt(const a,b: mp_float): boolean;
  {-return a > b}
begin
  mpf_is_gt := mpf_cmp(a,b)=MP_GT;
end;


{---------------------------------------------------------------------------}
function mpf_is_le(const a,b: mp_float): boolean;
  {-return a <= b}
begin
  mpf_is_le := mpf_cmp(a,b)<>MP_GT;
end;


{---------------------------------------------------------------------------}
function mpf_is_lt(const a,b: mp_float): boolean;
  {-return a < b}
begin
  mpf_is_lt := mpf_cmp(a,b)=MP_LT;
end;


{---------------------------------------------------------------------------}
function mpf_is_ne(const a,b: mp_float): boolean;
  {-return a <> b}
begin
  mpf_is_ne := mpf_cmp(a,b)<>MP_EQ;
end;


{---------------------------------------------------------------------------}
procedure mpf_ln(const a: mp_float; var b: mp_float);
  {-calculate b = ln(a), a>0}
var
  x,y,t: mp_float;
  i,f,bprec: longint;
  inv: boolean;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b)then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_ln');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {a must be positive}
  if s_mpf_is_le0(a) then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mpf_ln: a <= 0');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;

  {ln(1) = 0}
  if mpf_is1(a) then begin
    mpf_set0(b);
    exit;
  end;

  {save result precision and use slightly larger working precision}
  bprec := b.bitprec;
  i := bprec+32;
  if i>MPF_MAX_PREC then i:=MPF_MAX_PREC;

  mpf_initp3(x,y,t,i);
  if mp_error<>MP_OKAY then exit;

  {set working precision for b}
  s_mpf_normalizep(b,i);

  {if a < 1 then x=1/a else x=a}

  inv := s_mpf_ldx(a) <= 0;
  if inv then mpf_inv(a,x) else mpf_copyp(a,x);

  if s_mpf_is0(x) then begin
    {it could be possible that 1/a = 0, avoid endless loop}
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mpf_ln: 1/a = 0');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;

  {Range reduction 1: make x < 2}
  f := 0;
  while s_mpf_ldx(x) > 1 do begin
    inc(f);
    mpf_sqrt(x,x);
    if mp_error<>MP_OKAY then break;
  end;

  {y=x-1}
  mpf_set1(t);
  mpf_sub(x,t,y);

  if s_mpf_ldx(y) >= -7 then begin
    {Range reduction 2: make  1 < x < 1+2^-8}
    for i:=1 to 8 do begin
      inc(f);
      mpf_sqrt(x,x);
    end;
    {recalc y=x-1}
    mpf_sub(x,t,y);
  end;

  {ln(x) = 2*sum[ ((x-1)/(x+1))^(2i+1)/(2i+1) ] }
  mpf_add(x,t,x);
  {t will be the term ((x-1)/(x+1))^(2i+1)/(2i+1)}
  mpf_div(y,x,t);
  {y = ((x-1)/(x+1))^2}
  mpf_sqr(t,y);
  {set b to first term, loop starts with i=2}
  mpf_copyp(t,b);

  i := 1;
  repeat
    inc(i,2);
    if (i<0) or (mp_error<>MP_OKAY) then break;
    mpf_mul(t,y,t);
    if i <= longint(MP_DIGIT_MAX) then mpf_div_d(t,mp_digit(i),x)
    else mpf_div_int(t,i,x);
  until not s_mpf_incf(b,x);

  {Undo range reduction and multiply by 2}
  s_mpf_incexp(b, 1+f);

  {ln(1/a) = - ln(a)}
  if inv then s_mpf_chs(b);

  {normalize to original precision}
  s_mpf_normalizep(b,bprec);

  mpf_clear3(x,y,t);
end;


{---------------------------------------------------------------------------}
procedure mpf_ln1p(const a: mp_float; var b: mp_float);
  {-calculate b = ln(1+a), a>-1; special version for small a}
var
  x,y,t: mp_float;
  i,f,bprec: longint;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b)then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_ln1p');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if s_mpf_is0(a) then begin
    mpf_set0(b);
    exit;
  end
  else if s_mpf_ldx(a) >= 0 then begin
    {if |a| > 0.5 then use standard function b = ln(1+a)}
    s_mpf_add1(a,b);
    mpf_ln(b,b);
    exit;
  end;

  {save result precision and use slightly larger working precision}
  bprec := b.bitprec;
  i := bprec+32;
  if i>MPF_MAX_PREC then i:=MPF_MAX_PREC;

  mpf_initp3(x,y,t,i);
  if mp_error<>MP_OKAY then exit;

  {set working precision for b}
  s_mpf_normalizep(b,i);
  mpf_copyp(a,x);

  {range reduction: make x < 2^-10, using ln1p(x)=2*ln1p(x/(1+sqrt(1+x)))}
  f := 0;
  while s_mpf_ldx(x) >= -10 do begin
    s_mpf_add1(x,y);
    mpf_sqrt(y,y);
    s_mpf_inc1(y);
    mpf_div(x,y,x);
    inc(f);
  end;

  {ln(1+x) = 2y*[1 + t/3 + t^2/5 + t^3/7 ...],  y=x/(2+x), t=y^2}
  {b accumulates the result starting with b=x/(2+x)}
  mpf_set_int(b,2);
  s_mpf_inc(b,x);
  mpf_div(x,b,b);

  {t = y^2}
  mpf_sqr(b,t);

  {x is the term y*z^i}
  mpf_copy(b,x);

  {add the higher terms until no change in sum}
  i := 1;
  repeat
    inc(i,2);
    if (i<0) or (mp_error<>MP_OKAY) then break;
    mpf_mul(x,t,x);
    if i <= longint(MP_DIGIT_MAX) then mpf_div_d(x,mp_digit(i),y)
    else mpf_div_int(x,i,y);
  until not s_mpf_incf(b,y);

  {Undo range reduction and multiply by 2}
  s_mpf_incexp(b, 1+f);

  {normalize to original precision}
  s_mpf_normalizep(b,bprec);

  mpf_clear3(x,y,t);
end;


{---------------------------------------------------------------------------}
procedure mpf_log10(const a: mp_float; var b: mp_float);
  {-calculate b = log10(a), a>0}
var
  t: mp_float;
begin
  mpf_ln(a,b);
  if mpf_is0(b) or (mp_error<>MP_OKAY) then exit;
  mpf_initp(t, b.bitprec);
  if mp_error=MP_OKAY then begin
    mpf_set_ln10(t);
    mpf_div(b,t,b);
    mpf_clear(t);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_log2(const a: mp_float; var b: mp_float);
  {-calculate b = log2(a), a>0}
var
  t: mp_float;
begin
  mpf_ln(a,b);
  if mpf_is0(b) or (mp_error<>MP_OKAY) then exit;
  mpf_initp(t, b.bitprec);
  if mp_error=MP_OKAY then begin
    mpf_set_ln2(t);
    mpf_div(b,t,b);
    mpf_clear(t);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_logbase(const b,x: mp_float; var y: mp_float);
  {-calculate y = base b logarithm of x}
var
  u,v: mp_float;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(y) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_logbase');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  mpf_initp2(u,v,y.bitprec+16);
  if mp_error=MP_OKAY then begin
    mpf_ln(b,u);
    mpf_ln(x,v);
    mpf_div(v,u,y);
    mpf_clear2(u,v);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_mul(const a,b: mp_float; var c: mp_float);
  {-calculate c = a*b}
var
  be: longint;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    _CheckBitPrec(c);
  {$endif}
  if mpf_is0(a) or mpf_is0(b) then mpf_set0(c)
  else begin
    mp_mul(a.mantissa, b.mantissa, c.mantissa);
    {need temp var for b.exponent, if @c = @a or @b}
    be := b.exponent;
    c.exponent := a.exponent;
    s_mpf_incexp(c, be);
    s_mpf_normalize(c);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_mul_2k(const a: mp_float; k: longint; var b: mp_float);
  {-calculate b = a*2^k}
begin
  {arg check in mpf_copyp}
  mpf_copyp(a,b);
  s_mpf_incexp(b, k);
end;


{---------------------------------------------------------------------------}
procedure mpf_mul_d(const a: mp_float; b: mp_digit; var c: mp_float);
  {-multiply by a digit}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    _CheckBitPrec(a);
    _CheckBitPrec(c);
  {$endif}
  if (b=0) or mpf_is0(a) then mpf_set0(c)
  else if b=1 then mpf_copyp(a,c)
  else begin
    mp_mul_d(a.mantissa, b, c.mantissa);
    c.exponent := a. exponent;
    s_mpf_normalize(c);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_mul_ext(const a: mp_float; b: extended; var c: mp_float);
  {-calculate c = a*b}
var
  x: mp_float;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(c) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_mul_ext');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if s_mpf_is0(a) or (b=0.0) then begin
    mpf_set0(c);
    exit;
  end;
  mpf_initp(x,c.bitprec);
  if mp_error=MP_OKAY then begin
    mpf_set_ext(x,b);
    mpf_mul(a,x,c);
    mpf_clear(x);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_mul_int(const a: mp_float; b: longint; var c: mp_float);
  {-multiply by a 32 bit integer}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    _CheckBitPrec(a);
    _CheckBitPrec(c);
  {$endif}
  if mpf_is0(a) or (b=0) then mpf_set0(c)
  else if abs(b)=1 then begin
    if b<0 then mpf_chs(a,c) else mpf_copyp(a,c);
  end
  else begin
    mp_mul_int(a.mantissa, b, c.mantissa);
    c.exponent := a. exponent;
    s_mpf_normalize(c);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_mul_mpi(const a: mp_float; const b: mp_int; var c: mp_float);
  {-calculate c = a*b}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    _CheckBitPrec(a);
    _CheckBitPrec(c);
  {$endif}
  if mpf_is0(a) or mp_is0(b) then mpf_set0(c)
  else begin
    mp_mul(a.mantissa, b, c.mantissa);
    c.exponent := a.exponent;
    s_mpf_normalize(c);
  end;
end;


{---------------------------------------------------------------------------}
function mpf_not_init(const a: mp_float): boolean;
  {-sanity check if a is initialized, does not catch all cases!}
begin
  with a do begin
    mpf_not_init := mp_not_init(mantissa) or (bitprec<MPF_MIN_PREC) or (bitprec>MPF_MAX_PREC);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_numbpart(n: longint; var p: mp_int);
  {-Compute number of partitions of n with Hardy-Ramanujan-Rademacher formula}
const
  NS = 16;
  small: array[0..NS] of byte = (1,1,2,3,5,7,11,15,22,30,42,56,77,101,135,176,231);
var
  x: mp_float;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(p) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_numbpart');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if n<=NS then begin
    if n>=0 then mp_set_int(p,small[n]) else mp_zero(p);
    exit;
  end;
  mpf_init(x);
  if mp_error=MP_OKAY then begin
    s_mpf_numbpart(n,x);
    mpf_round(x,p);
    mpf_clear(x);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_output_decimal(const a: mp_float; ndd: word);
  {-write an mp_float to output using decimal scientific representation, ndd>0}
  { is the total number of digits (including one digit before the '.')}
begin
  mpf_write_decimal(output, a, ndd);
end;


{---------------------------------------------------------------------------}
procedure mpf_output_decimal_alt(const a: mp_float; ndd: word);
  {-write an mp_float to output using decimal alternative representation, ndd>0 is}
  { the total number of digits, trailing '.' or '0' are suppressed. If a is too}
  { large or too small, scientific representation is used.}
begin
  mpf_write_decimal_alt(output, a, ndd);
end;


{---------------------------------------------------------------------------}
procedure mpf_output_radix(const a: mp_float; radix,ndd: word);
  {-write an mp_float to output using radix scientific representation, ndd>0}
  { is the total number of digits (including one digit before the '.')}
begin
  mpf_write_radix(output, a, radix, ndd);
end;


{---------------------------------------------------------------------------}
procedure mpf_output_radix_alt(const a: mp_float; radix,ndd: word);
  {-write an mp_float to output using radix alternative representation, ndd>0 is}
  { the total number of digits, trailing '.' or '0' are suppressed. If a is too}
  { large or too small, scientific representation is used. NOTE: no radix prefix/suffix!}
begin
  mpf_write_radix_alt(output, a, radix, ndd);
end;


{---------------------------------------------------------------------------}
procedure mpf_random(var a: mp_float);
  {-set to a random number uniformly distributed in [0,1)}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    _CheckBitPrec(a);
  {$endif}
  with a do begin
    mp_rand_bits_ex(mantissa, bitprec, false);
    exponent := -bitprec;
  end;
  s_mpf_normalize(a);
end;


{---------------------------------------------------------------------------}
procedure mpf_read_decimal(var a: mp_float; str: pchar8);
  {-read a from ASCII float decimal string. str may contain a single '.'}
  { The exponent part is @[+|-]nnn (e or E can replace @). Integer or }
  { fractional part must be present.}
begin
  mpf_read_radix(a, str, 10);
end;


{---------------------------------------------------------------------------}
procedure mpf_read_hex(var a: mp_float; str: pchar8);
  {-read a from ASCII float hexadecimal string. str may contain a single}
  { '.'. The exponent part is @[+|-]nnn (h or H can replace @). Integer }
  { or fractional part must be present.}
begin
  mpf_read_radix(a, str, 16);
end;


{---------------------------------------------------------------------------}
procedure mpf_read_radix(var a: mp_float; str: pchar8; radix: word);
  {-read a from an ASCII float radix string. str may contain a single '.'.}
  { The exponent part <xp> is @[+|-]nnn, b/B, e/E, or h/H can replace @ if}
  { radix in [2,10,16]. The integer <ip> or fractional part <fp> must be }
  { present, nnn must be decimal, ie general format is <ip>.<fp>*radix^<xp>.}
var
  fs: pchar8;
  ip,fp: mp_int;
  x: mp_float;
  lf,xp,li: longint;
  {$ifdef VirtualPascal}
    ec: longint;
  {$else}
    ec: integer;
  {$endif}
  c: char8;
  neg: boolean;
  sep,dsep: string[5];

{$ifndef MPC_HaltOnError}
label leave;
{$endif}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_read_radix');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  mp_init2(ip,fp);
  if mp_error<>MP_OKAY then exit;

  mpf_initp(x,a.bitprec);
  if mp_error<>MP_OKAY then begin
    mp_clear2(ip,fp);
    exit;
  end;

  if radix=2 then sep := '@bB'
  else if radix=10 then sep := '@eE'
  else if radix=16 then sep := '@hH'
  else sep := '@';
  dsep := mp_fract_sep{'.'}+sep;
  lf := 0;
  li := 0;
  xp := 0;

  {skip leading white space}
  repeat
    c := upcase(str^);
    if c=#0 then begin
      {$ifdef MPC_HaltOnError}
        {$ifdef MPC_UseExceptions}
          raise MPXBadArg.Create('mpf_read_radix: invalid syntax');
        {$else}
          RunError(MP_RTE_BADARG);
        {$endif}
      {$else}
        set_mp_error(MP_BADARG);
        goto leave;
      {$endif}
    end;
    if (c<>' ') and (c<>#9) then break;
    inc(str);
  until false;
  neg := false;

  if c='-' then begin
    neg := true;
    inc(str);
  end
  else if c='+' then inc(str);
  if pos(str^,dsep)>0 then mp_zero(ip)
  else begin
    {read integer part until '.@eE' or #0}
    fs := str;
    s_mp_read_radix(ip,str,radix,dsep,false);
    li := str-fs;
  end;
  if str^=mp_fract_sep{'.'} then begin
    {read fractional part}
    inc(str);
    if pos(str^,sep+#0)=0 then begin
      {remember starting position of floating part}
      fs := str;
      s_mp_read_radix(fp,str,radix,sep,false);
      {fractional part = fp/radix^lf}
      lf := str-fs;
    end;
  end;
  if str^<>#0 then begin
    {exponent part}
    inc(str);
    {$ifdef D12Plus}
      {Make the implicit type cast explicit to avoid warning}
      val(string(str),xp,ec);
    {$else}
      val(str,xp,ec);
    {$endif}
    if ec<>0 then begin
      {$ifdef MPC_HaltOnError}
        {$ifdef MPC_UseExceptions}
          raise MPXBadArg.Create('mpf_read_radix: invalid syntax');
        {$else}
          RunError(MP_RTE_BADARG);
        {$endif}
      {$else}
        set_mp_error(MP_BADARG);
        goto leave;
      {$endif}
    end;
  end;
  if lf>0 then begin
    {fractional part present, calculate a = ip + fp/radix^lf}
    (*
      {Old code gives 0.5 <> 5/10!}
      mpf_set_int(a,radix);
      mpf_expt_int(a,-lf,a);
      mpf_mul_mpi(a,fp,a);
      mpf_add_mpi(a,ip,a);
    *)
    mpf_set_mpi(a,fp);
    mpf_set_int(x,radix);
    mpf_expt_int(x,lf,x);
    mpf_div(a,x,a);
    mpf_add_mpi(a,ip,a);
  end
  else begin
    if li=0 then begin
      {no integer and fractional part}
      {$ifdef MPC_HaltOnError}
        {$ifdef MPC_UseExceptions}
          raise MPXBadArg.Create('mpf_read_radix: invalid syntax');
        {$else}
          RunError(MP_RTE_BADARG);
        {$endif}
      {$else}
        set_mp_error(MP_BADARG);
        goto leave;
      {$endif}
    end
    else begin
      {no fractional part}
      mpf_set_mpi(a,ip);
    end;
  end;
  if xp<>0 then begin
    {multiply by radix^xp}
    mpf_set_int(x,radix);
    mpf_expt_int(x,xp,x);
    mpf_mul(x,a,a);
  end;
  if neg then s_mpf_chs(a);
{$ifndef MPC_HaltOnError}
leave:
{$endif}
  mp_clear2(ip,fp);
  mpf_clear(x);
end;


{---------------------------------------------------------------------------}
function mpf_reldev(const a,b: mp_float): double;
  {-Return abs((a-b)/b)*2^b.bitprec, special if b=0, or a-b=0}
var
  c: mp_float;
begin
  mpf_reldev := DblPosInf;
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_reldev');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  mpf_initp(c,b.bitprec);
  if mp_error<>MP_OKAY then exit;
  mpf_sub(a,b,c);
  if s_mpf_is0(c) then mpf_reldev := 0.0
  else begin
   if not s_mpf_is0(b) then mpf_div(c,b,c);
    mpf_mul_2k(c,b.bitprec,c);
    mpf_reldev := abs(mpf_todouble(c));
  end;
  mpf_clear(c);
end;


{---------------------------------------------------------------------------}
procedure mpf_round(const a: mp_float; var b: mp_int);
  {-round an mp_float to nearest mp_int}
var
  msign: word;
  rbset: boolean;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mp_not_init(b) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_round');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  with a do begin
    if exponent>=0 then mp_shl(mantissa,exponent,b)
    else begin
      {Use variables to fix quirk if @b = @a.mantissa}
      {Get rounding bit of mantissa}
      rbset := mp_isbit(mantissa, -exponent-1);
      msign := mantissa.sign;
      mp_shr(mantissa,-exponent,b);
      if rbset then begin
        if msign=MP_NEG then mp_dec(b) else mp_inc(b);
      end;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_set0(var a: mp_float);
  {-set a=0, a.bitprec is preserved}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    _CheckBitPrec(a);
  {$endif}
  mp_zero(a.mantissa);
  if mp_error=MP_OKAY then a.exponent:= 0;
end;


{---------------------------------------------------------------------------}
procedure mpf_set1(var a: mp_float);
  {-set a=1, a.bitprec is preserved}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    _CheckBitPrec(a);
  {$endif}
  with a do begin
    mp_set(mantissa,1);
    exponent := 0;
  end;
  s_mpf_normalize(a);
end;


{---------------------------------------------------------------------------}
procedure mpf_set_default_prec(prec: longint);
  {-set new default (bit) precision}
begin
  if prec<MPF_MIN_PREC then prec := MPF_MIN_PREC;
  if prec>MPF_MAX_PREC then prec := MPF_MAX_PREC;
  mpf_default_prec := prec;
end;


{---------------------------------------------------------------------------}
procedure mpf_set_default_decprec(dprec: word);
  {-Set default precision dprec*log_2(10), i.e. about dprec decimal digits}
begin
  mpf_set_default_prec(trunc(1.0 + 3.321928095*dprec));
end;


{---------------------------------------------------------------------------}
procedure mpf_set_exp1(var a: mp_float);
  {-set a to exp(1), preserve a.bitprec}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_set_exp1p');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  {$ifdef MPC_Ln2Ln10Tab}
    _set_ptab_const(a, 2, a.bitprec, AddrEBytes);
  {$else}
    mpf_set_int(a,1);
    mpf_exp(a,a);
  {$endif}
end;

{---------------------------------------------------------------------------}
procedure mpf_set_exp1p(var a: mp_float; prec: longint);
  {-set a to exp(1) with bit precision prec}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_set_exp1p');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  {$ifdef MPC_Ln2Ln10Tab}
    _set_ptab_const(a, 2, prec, AddrEBytes);
  {$else}
    a.bitprec := prec;
    mpf_set_int(a,1);
    mpf_exp(a,a);
  {$endif}
end;


{---------------------------------------------------------------------------}
procedure mpf_set_ext(var a: mp_float; x: extended);
  {-set a to an extended. Error if x = NAN or INF}
type
  txr = packed record
          mant: array[0..7] of byte;
          sexp: word;
        end;
var
  xr: txr absolute x;
  i: integer;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_set_ext');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if xr.sexp and $7fff = $7fff then begin
    {nan or inf}
    {$ifdef MPC_HaltOnError}
     {$ifdef MPC_UseExceptions}
       raise MPXBadArg.Create('mpf_set_ext: NAN or INF');
     {$else}
       RunError(MP_RTE_BADARG);
     {$endif}
   {$else}
     set_mp_error(MP_BADARG);
     exit;
   {$endif}
  end;
  mp_set(a.mantissa, xr.mant[7]);
  for i:=6 downto 0 do begin
    mp_shl(a.mantissa, 8, a.mantissa);
    mp_add_d(a.mantissa, xr.mant[i], a.mantissa);
  end;
  if xr.sexp and $8000 <> 0 then s_mpf_chs(a);
  a.exponent := longint(xr.sexp and $7fff) -($3fff+63);
  s_mpf_normalize(a);
end;


{---------------------------------------------------------------------------}
procedure mpf_set_int(var a: mp_float; b: longint);
  {-set a to a longint}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    _CheckBitPrec(a);
  {$endif}
  with a do begin
    mp_set_int(mantissa,b);
    exponent := 0;
  end;
  s_mpf_normalize(a);
end;


{---------------------------------------------------------------------------}
procedure mpf_set_ln10(var a: mp_float);
  {-set a to ln(10), preserve a.bitprec}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    _CheckBitPrec(a);
  {$endif}
  mpf_set_ln10p2k(a,0,a.bitprec);
end;


{---------------------------------------------------------------------------}
procedure mpf_set_ln10p(var a: mp_float; prec: longint);
  {-set a to ln(10) with bit precision prec}
begin
  mpf_set_ln10p2k(a,0,prec);
end;


{---------------------------------------------------------------------------}
procedure mpf_set_ln10p2k(var a: mp_float; k,prec: longint);
  {-set a to ln(10)*2^k with bit precision prec}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_set_ln10p2k');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  {$ifdef MPC_Ln2Ln10Tab}
    _set_ptab_const(a, 2, prec, AddrLn10Bytes);
  {$else}
    a.bitprec := prec;
    mpf_set_int(a,10);
    mpf_ln(a,a);
  {$endif}
  if k<>0 then s_mpf_incexp(a, k);
end;


{---------------------------------------------------------------------------}
procedure mpf_set_ln2(var a: mp_float);
  {-set a to ln(2), preserve a.bitprec}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    _CheckBitPrec(a);
  {$endif}
  mpf_set_ln2p2k(a,0,a.bitprec);
end;


{---------------------------------------------------------------------------}
procedure mpf_set_ln2p(var a: mp_float; prec: longint);
  {-set a to ln(2) with bit precision prec}
begin
  mpf_set_ln2p2k(a,0,prec);
end;


{---------------------------------------------------------------------------}
procedure mpf_set_ln2p2k(var a: mp_float; k,prec: longint);
  {-set a to ln(2)*2^k with bit precision prec}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_set_ln2p2k');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  {$ifdef MPC_Ln2Ln10Tab}
    _set_ptab_const(a, 0, prec, AddrLn2Bytes);
  {$else}
    a.bitprec := prec;
    mpf_set_int(a,2);
    mpf_ln(a,a);
  {$endif}
  if k<>0 then s_mpf_incexp(a, k);
end;


{---------------------------------------------------------------------------}
procedure mpf_set_mpi(var a: mp_float; const b: mp_int);
  {-set a to an mp_int}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    _CheckBitPrec(a);
  {$endif}
  with a do begin
    mp_copy(b, mantissa);
    exponent := 0;
  end;
  s_mpf_normalize(a);
end;


{---------------------------------------------------------------------------}
procedure mpf_set_mpi2k(var a: mp_float; const m: mp_int; e: longint);
  {-set a to m*2^e (build a from mantissa and exponent)}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    _CheckBitPrec(a);
  {$endif}
  with a do begin
    mp_copy(m, mantissa);
    exponent := e;
  end;
  s_mpf_normalize(a);
end;


{---------------------------------------------------------------------------}
procedure mpf_set_pi(var a: mp_float);
  {-set a to pi, preserve a.bitprec}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    _CheckBitPrec(a);
  {$endif}
  mpf_set_pip2k(a,0,a.bitprec);
end;


{---------------------------------------------------------------------------}
procedure mpf_set_pip(var a: mp_float; prec: longint);
  {-set a to pi with bit precision prec}
begin
  mpf_set_pip2k(a,0,prec);
end;


{---------------------------------------------------------------------------}
procedure mpf_set_pip2k(var a: mp_float; k,prec: longint);
  {-set a to pi*2^k with bit precision prec}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_set_pip2k');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  _set_ptab_const(a, 2, prec, AddrPiBytes);
  if k<>0 then s_mpf_incexp(a, k);
end;


{---------------------------------------------------------------------------}
procedure mpf_set_pi2k(var a: mp_float; k: longint);
  {-set a to pi*2^k, preserve a.bitprec}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    _CheckBitPrec(a);
  {$endif}
  mpf_set_pip2k(a,k,a.bitprec);
end;


{---------------------------------------------------------------------------}
procedure mpf_sin(const a: mp_float; var b: mp_float);
  {-calculate b = sin(a)}
begin
  mpf_trig(a, nil, @b, nil);
end;


{---------------------------------------------------------------------------}
procedure mpf_sinh(const a: mp_float; var b: mp_float);
  {-calculate b = sinh(a), a < 2^31 * ln(2)}
var
  t: mp_float;
  pb: longint;
  neg: boolean;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b)then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_sinh');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  if s_mpf_is0(a) then begin
    mpf_set0(b);
    exit;
  end;

  pb := b.bitprec;
  mpf_initp(t, pb+32);
  if mp_error<>MP_OKAY then exit;

  neg := s_mpf_is_neg(a);
  mpf_abs(a,t);
  b.bitprec := t.bitprec;

  if s_mpf_ldx(t) >= 0 then begin
    {t>=0.5, sinh(t) = (exp(t)-exp(-t))/2}
    mpf_exp(t,b);
    {don't calculate inverse if it is to small}
    if s_mpf_ldx(b) < 16 + b.bitprec div 2 then begin
      mpf_inv(b,t);
      mpf_sub(b,t,b);
    end;
  end
  else begin
    {t<0.5, sinh(t) = 0.5*expm1(t)*(1+exp(-t))}
    {sinh(t) = 0.5*y(1+1/(1+y)) with y = expm1(t)}
    mpf_expm1(t,b);
    s_mpf_add1(b,t);
    mpf_inv(t,t);
    s_mpf_inc1(t);
    mpf_mul(b,t,b);
  end;
  s_mpf_incexp(b,-1);
  s_mpf_normalizep(b,pb);
  if neg then s_mpf_chs(b);
  mpf_clear(t);
end;


{---------------------------------------------------------------------------}
procedure mpf_sinhcosh(const a: mp_float; var b,c: mp_float);
  {-calculate b = sinh(a), c = cosh(a);  @b<>@c,  a < 2^31 * ln(2)}
var
  t: mp_float;
  pb,pc,prec: longint;
  neg: boolean;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b) or mpf_not_init(c) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_sinhcosh');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
    if @b=@c then begin
      {$ifdef MPC_HaltOnError}
        {$ifdef MPC_UseExceptions}
          raise MPXBadArg.Create('mpf_sinhcosh: @b=@c');
        {$else}
          RunError(MP_RTE_BADARG);
        {$endif}
      {$else}
        set_mp_error(MP_BADARG);
        exit;
      {$endif}
    end;
  {$endif}

  if s_mpf_is0(a) then begin
    mpf_set0(b);
    mpf_set1(b);
    exit;
  end;

  pb := b.bitprec;
  pc := b.bitprec;
  if pb>pc then prec := pb else prec := pc;

  mpf_initp(t, prec+32);
  if mp_error<>MP_OKAY then exit;

  neg := s_mpf_is_neg(a);
  mpf_abs(a,t);

  b.bitprec := t.bitprec;
  c.bitprec := t.bitprec;

  if s_mpf_ldx(t) >= 0 then begin
    {t>=0.5, sinh(t) = (exp(t)-exp(-t))/2, cosg(t) = (exp(t)+exp(-t))/2}
    mpf_exp(t,b);
    {don't calculate inverse if it is to small}
    if s_mpf_ldx(b) < 16 + b.bitprec div 2 then begin
      mpf_inv(b,t);
      mpf_add(b,t,c);
      mpf_sub(b,t,b);
    end
    else mpf_copy(b,c);
  end
  else begin
    {t<0.5, sinh(t) = 0.5*expm1(t)*(1+exp(-t))}
    {sinh(t) = 0.5*y(1+1/(1+y)) with y = expm1(t)}
    {cosh(t) = 0.5*((1+y)+1/(1+y))}
    mpf_expm1(t,b);
    s_mpf_add1(b,c);
    {b=y, c=(1+y)}
    mpf_inv(c,t);
    mpf_add(c,t,c);
    s_mpf_inc1(t);
    mpf_mul(b,t,b);
  end;
  s_mpf_incexp(b,-1);
  s_mpf_incexp(c,-1);
  s_mpf_normalizep(b,pb);
  s_mpf_normalizep(c,pc);
  if neg then s_mpf_chs(b);
  mpf_clear(t);
end;


{---------------------------------------------------------------------------}
procedure mpf_sqr(const a: mp_float; var b: mp_float);
  {-Compute b = a*a}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    _CheckBitPrec(a);
    _CheckBitPrec(b);
  {$endif}
  mp_sqr(a.mantissa, b.mantissa);
  b.exponent := a.exponent;
  s_mpf_incexp(b,b.exponent);
  s_mpf_normalize(b);
end;


{---------------------------------------------------------------------------}
procedure mpf_sqrt(const a: mp_float; var b: mp_float);
  {-calculate b = sqrt(a)}
var
  diff: longint;
begin
  if mp_error<>MP_OKAY then exit;

  {a must be positive}
  if s_mpf_is_neg(a) then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mpf_sqrt: a < 0');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;

  {other arg check in mpf_copyp}
  {copy const a to working float b and normalize to b.bitprec}
  mpf_copyp(a,b);
  if s_mpf_is0(b) or mpf_is1(b) then exit;

  with b do begin
    {use bitprec+2 to allow more reliable rounding in normalize}
    diff := 2*(bitprec+2) - mp_bitsize(mantissa);
    if diff>=0 then begin
      {shift mantissa and make sure exponent is even}
      if odd(exponent xor diff) then inc(diff);
      s_mpf_incexp(b, -diff);
      mp_shl(mantissa, diff, mantissa);
    end
    else begin
      {This branch SHOULD not be taken because copyp normalizes to}
      {precision b.bitprec. Just paranoia, but make exponent even!}
      if odd(exponent) then begin
        s_mpf_incexp(b, 1);
        mp_shr(mantissa, 1, mantissa);
      end;
    end;
    mp_sqrt(mantissa, mantissa);
    exponent := exponent div 2;
  end;
  s_mpf_normalize(b);
end;


{---------------------------------------------------------------------------}
procedure mpf_sqrt1pm1(const a: mp_float; var b: mp_float);
  {-calculate b = sqrt(1+a)-1 with increased accuracy for a near 0, a >= -1}
var
  t: mp_float;
begin
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b)then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_sqrt1pm1');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  mpf_initp(t, b.bitprec+32);
  if mp_error=MP_OKAY then begin
    s_mpf_add1(a,t);
    mpf_sqrt(t,t);
    if s_mpf_ldx(a) >= 0 then begin
      {a >= 0.5}
      s_mpf_dec1(t);
      mpf_copyp(t,b);
    end
    else begin
      {sqrt(1+x)-1 = x/(sqrt(1+x)+1)}
      s_mpf_add1(t,t);
      mpf_div(a,t,b);
    end;
    mpf_clear(t);
  end;
end;


{---------------------------------------------------------------------------}
function mpf_squad(const a,b,c: mp_float; var x1,y1,x2,y2: mp_float): integer;
  {-Solve the quadratic equation a*x^2 + b*x + c = 0. Result is the number}
  { of different solutions: 0 (if a=b=0), 1 (x1), or 2 (x1 and x2). If the}
  { result is = -2, x1 + i*y1 and x2 + i*y2 are the two complex solutions.}
  { x1,y1,x2,y2 should be different variables and not the same as a,b or c}
var
  b2,d,t: mp_float;
  p,pd : longint;
begin

  mpf_squad := 0;
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b) or mpf_not_init(c) or
       mpf_not_init(x1) or mpf_not_init(x2) or mpf_not_init(y1) or mpf_not_init(y2) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_squad');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {Setup default return values}
  mpf_squad := 2;
  mpf_set0(x2);
  mpf_set0(y1);
  mpf_set0(y2);

  if mpf_is0(a) then begin
    {solve bx+c=0}
    if mpf_is0(b) then mpf_squad := 0
    else begin
      mpf_squad := 1;
      mpf_div(c,b,x1);
      s_mpf_chs(x1);
    end;
    exit;
  end;

  if mpf_is0(c) then begin
    {ax^2+bx = 0 = x(ax+b) with a<>0}
    {x1 := -b/a;}
    mpf_div(b,a,x1);
    s_mpf_chs(x1);
    if s_mpf_is_ge0(x1) then mpf_exch(x1,x2);
    exit;
  end;

  {Here a<>0 and c<>0. Set precision for discriminant}
  {calculation: maximum precision of b^2, 4ac, and x1}
  pd := 2*b.bitprec;
  p  := 2+a.bitprec+c.bitprec; if p>pd then pd := p;
  p  := x1.bitprec;            if p>pd then pd := p;

  mpf_initp(b2,b.bitprec+8);
  if mp_error<>MP_OKAY then exit;
  mpf_initp2(d,t,pd+8);
  if mp_error<>MP_OKAY then begin
    mpf_clear(b2);
    exit;
  end;

  {get b2 = -b/2}
  mpf_mul_2k(b,-1,b2);
  s_mpf_chs(b2);

  {d = discriminant(a,b2,c) = b2*b2 - a*c}
  mpf_sqr(b2,d);
  mpf_mul(a,c,t);
  mpf_sub(d,t,d);

  if mpf_is0(d) then begin
    {d=0, real double root}
    mpf_squad := 1;
    mpf_div(b2,a,x1);
    mpf_copy(x1,x2);
    exit;
  end
  else if s_mpf_is_neg(d) then begin
    {d<0: two complex roots}
    mpf_squad := -2;
    s_mpf_chs(d);
    mpf_sqrt(d,d);
    mpf_div(b2,a,x1);
    mpf_copy(x1,x2);
    mpf_div(d,a,y1);
    mpf_chs(y1,y2);
    if mpf_is_gt(y1,y2) then mpf_exch(y1,y2);
  end
  else begin
    {d>0: two real roots}
    mpf_sqrt(d,d);
    if s_mpf_is_ge0(b2) then mpf_add(b2,d,d)
    else mpf_sub(b2,d,d);
    mpf_div(d,a,x1);
    mpf_div(c,d,x2);
    if mpf_is_gt(x1,x2) then mpf_exch(x1,x2);
  end;
  mpf_clear3(b2,d,t);
end;


{---------------------------------------------------------------------------}
procedure mpf_sub(const a,b: mp_float; var c: mp_float);
  {-calculate c = a-b}
var
  x: mp_float;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b) or mpf_not_init(c) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_sub');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if s_mpf_is0(b) then begin
    mpf_copyp(a,c);
    exit;
  end;
  if s_mpf_is0(a) then begin
    mpf_chs(b,c);
    exit;
  end;
  if @a=@b then begin
    mpf_set0(c);
    exit;
  end;
  {make local copy of a}
  mpf_initp(x, c.bitprec);
  if mp_error=MP_OKAY then begin
    mpf_chs(b,x);
    {x = a-b = -a + b}
    s_mpf_inc(x,a);
    mpf_exch(x,c);  {Note: x.bitprec=c.bitprec}
    mpf_clear(x);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_sub_ext(const a: mp_float; b: extended; var c: mp_float);
  {-calculate c = a-b}
var
  x: mp_float;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(c) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_sub_ext');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if b=0.0 then begin
    mpf_copyp(a,c);
    exit;
  end;
  if s_mpf_is0(a) then begin
    {c = -b}
    mpf_set_ext(c,-b);
    exit;
  end;
  {make local copy of a}
  mpf_initp(x, c.bitprec);
  if mp_error=MP_OKAY then begin
    mpf_set_ext(x,-b);
    {c = (-b)+a}
    s_mpf_inc(x,a);
    mpf_exch(x,c);    {Note: x.bitprec=c.bitprec}
    mpf_clear(x);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_sub_int(const a: mp_float; b: longint; var c: mp_float);
  {-calculate c = a-b}
var
  x: mp_float;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(c) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_sub_int');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if b=0 then begin
    mpf_copyp(a,c);
    exit;
  end;
  if s_mpf_is0(a) then begin
    {c = -b}
    mpf_set_int(c,-b);
    exit;
  end;
  {make local copy of a}
  mpf_initp(x, c.bitprec);
  if mp_error=MP_OKAY then begin
    mpf_set_int(x,-b);
    {c = (-b)+a}
    s_mpf_inc(x,a);
    mpf_exch(x,c);    {Note: x.bitprec=c.bitprec}
    mpf_clear(x);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_sub_mpi(const a: mp_float; const b: mp_int; var c: mp_float);
  {-calculate c = a-b}
var
  x: mp_float;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mp_not_init(b) or mpf_not_init(c) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_sub_mpi');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if mp_is0(b) then begin
    mpf_copyp(a,c);
    exit;
  end;
  if s_mpf_is0(a) then begin
    mpf_set_mpi(c,b);
    s_mpf_chs(c);
    exit;
  end;
  {make local copy of a}
  mpf_initp(x, c.bitprec);
  if mp_error=MP_OKAY then begin
    mpf_set_mpi(x,b);
    s_mpf_chs(x);
    {c = (-b)+a}
    s_mpf_inc(x,a);
    mpf_exch(x,c);     {Note: x.bitprec=c.bitprec}
    mpf_clear(x);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_sumalt(term: sumalt_term_func; n: longint; var s: mp_float; var Err: integer);
  {-calculate s=sum(i=0..n, term(i)) of alternating series with sumalt algorithm.}
  { If n<4, n is adjusted to bitprec of s. Err<>0 if a term cannot be evaluated.}
var
  b,c,t: mp_float;
  k,bprec,xn,xd: longint;
begin
  {H. Cohen, F. Rodriguez Villegas, D. Zagier: Convergence acceleration of }
  {alternating series, Experimental Mathematics, vol.9, no.1, pp.3-12, 2000}
  {online available via http://www.expmath.org/expmath/contents.html       }

  {J. Arndt[31], Chapter: Numerical evaluation of power series}
  {Section: The magic sumalt algorithm for alternating series}

  Err := 0;
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(s)then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_sumalt');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {save result precision and use slightly larger working precision}
  bprec := s.bitprec;
  k := bprec+32;
  if k>MPF_MAX_PREC then k:=MPF_MAX_PREC;
  mpf_initp3(b,c,t,k);
  if mp_error<>MP_OKAY then exit;

  if n<4 then n := trunc(0.39321985067869744234*k); {ln(2)/ln(3 + sqrt(8)))}

  mpf_set1(b);
  mpf_mul_2k(b, 2*n-1, b);
  mpf_copy(b,c);

  {set working precision for s}
  mpf_set0(s);
  s_mpf_normalizep(s,k);

  for k:=n-1 downto 0 do begin
     {get x[k] = xn/xd}
     if not term(k,xn,xd) then begin
       Err := -1;
       break;
     end;
     {s := s + c * x[k]}
     mpf_div_int(c,xd,t);
     mpf_mul_int(t,xn,t);
     mpf_add(s,t,s);
     {b := b * ((2*k+1)*(k+1)) / (2*(n+k)*(n-k))}
     mpf_div_int(b, 2*(n+k)*(n-k), b);
     mpf_mul_int(b, (2*k+1)*(k+1), b);
     {c := c + b}
     mpf_add(c,b,c);
  end;
  if Err=0 then begin
    if mpf_is0(c) then Err := -2
    else begin
      mpf_div(s,c,s);
      {normalize to original precision}
      s_mpf_normalizep(s,bprec);
    end;
  end;
  mpf_clear3(b,c,t);
end;


{---------------------------------------------------------------------------}
procedure mpf_sumaltf(fterm: sumalt_fterm_func; n: longint; var s: mp_float; var Err: integer);
  {-calculate s=sum(i=0..n, term(i)) of alternating series with sumalt algorithm.}
  { If n<4, n is adjusted to bitprec of s. Err<>0 if a term cannot be evaluated.}
var
  b,c,t: mp_float;
  k,bprec: longint;
begin
  {H. Cohen, F. Rodriguez Villegas, D. Zagier: Convergence acceleration of }
  {alternating series, Experimental Mathematics, vol.9, no.1, pp.3-12, 2000}
  {online available via http://www.expmath.org/expmath/contents.html       }

  {J. Arndt[31], Chapter: Numerical evaluation of power series}
  {Section: The magic sumalt algorithm for alternating series}

  Err := 0;
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(s)then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_sumaltf');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {save result precision and use slightly larger working precision}
  bprec := s.bitprec;
  k := bprec+32;
  if k>MPF_MAX_PREC then k:=MPF_MAX_PREC;
  mpf_initp3(b,c,t,k);
  if mp_error<>MP_OKAY then exit;

  if n<4 then n := trunc(0.39321985067869744234*k); {ln(2)/ln(3 + sqrt(8)))}

  mpf_set1(b);
  mpf_mul_2k(b, 2*n-1, b);
  mpf_copy(b,c);

  {set working precision for s}
  mpf_set0(s);
  s_mpf_normalizep(s,k);

  for k:=n-1 downto 0 do begin
     {get k-th term x[k]}
     if not fterm(k,t) then begin
       Err := -1;
       break;
     end;
     {s := s + c * x[k]}
     mpf_mul(t,c,t);
     mpf_add(s,t,s);
     {b := b * ((2*k+1)*(k+1)) / (2*(n+k)*(n-k))}
     mpf_div_int(b, 2*(n+k)*(n-k), b);
     mpf_mul_int(b, (2*k+1)*(k+1), b);
     {c := c + b}
     mpf_add(c,b,c);
  end;
  if Err=0 then begin
    if mpf_is0(c) then Err := -2
    else begin
      mpf_div(s,c,s);
      {normalize to original precision}
      s_mpf_normalizep(s,bprec);
    end;
  end;
  mpf_clear3(b,c,t);
end;


{---------------------------------------------------------------------------}
procedure mpf_tan(const a: mp_float; var b: mp_float);
  {-calculate b = tan(a)}
begin
  mpf_trig(a, nil, nil, @b);
end;


{---------------------------------------------------------------------------}
procedure mpf_tanh(const a: mp_float; var b: mp_float);
  {-calculate b = tanh(a)}
var
  t: mp_float;
  neg: boolean;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b)then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_tanh');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  if s_mpf_is0(a) then begin
    mpf_set0(b);
    exit;
  end;

  mpf_initp(t, b.bitprec);
  if mp_error<>MP_OKAY then exit;

  neg := not s_mpf_is_neg(a);
  mpf_abs(a,t);

  {tanh(a) = -sign(a)*t/(t+2) with t = expm1(-2|a|)}
  s_mpf_chs(t);
  s_mpf_incexp(t,1);
  mpf_expm1(t,b);
  mpf_set_int(t,2);
  s_mpf_inc(t,b);
  mpf_div(b,t,b);

  if neg then s_mpf_chs(b);

  mpf_clear(t);
end;


{---------------------------------------------------------------------------}
procedure mpf_todecimal_n(const a: mp_float; ndd: word; str: pchar8; maxlen: word);
  {-convert an mp_float to decimal scientific representation, ndd>0 is the}
  { total number of digits (including one digit before the '.')}
begin
  mpf_toradix_n(a, 10, ndd, str, maxlen);
end;


{---------------------------------------------------------------------------}
procedure mpf_todecimal_alt(const a: mp_float; ndd: word; str: pchar8; maxlen: word);
  {-convert an mp_float to decimal alternative representation, ndd>0 is the total}
  { number of digits, trailing '.' or '0' are suppressed. If a is too large or}
  { too small, scientific representation is used.}
begin
  mpf_toradix_alt(a, 10, ndd, str, maxlen);
end;


{---------------------------------------------------------------------------}
function mpf_todouble(const a: mp_float): double;
  {-convert a to double, +-inf if too large}
const
  MaxDblHex: packed array[0..3] of word = ($ffff,$ffff,$ffff,$7fef);
var
  MaxDouble: double absolute MaxDblHex; {1.797693134862315E+308}
begin
  if mpf_cmp_mag_ext(a,MaxDouble)>0 then begin
    if a.mantissa.sign=MP_NEG then mpf_todouble := DblNegInf
    else mpf_todouble := DblPosInf;
  end
  else with a do mpf_todouble := mp_todouble_ex(mantissa, exponent);
end;


{---------------------------------------------------------------------------}
function mpf_toextended(const a: mp_float): extended;
  {-convert a to extended, +-inf if too large}
const
  MaxExtHex: packed array[0..4] of word = ($ffff,$ffff,$ffff,$ffff,$7ffe); {MaxExtended as Hex}
var
  MaxExtended: extended absolute MaxExtHex; {=1.189731495357231764E+4932}
begin
  if mpf_cmp_mag_ext(a,MaxExtended)>0 then begin
    if a.mantissa.sign=MP_NEG then mpf_toextended := DblNegInf
    else mpf_toextended := DblPosInf;
  end
  else with a do mpf_toextended := mp_toextended_ex(mantissa, exponent);
end;


{---------------------------------------------------------------------------}
procedure mpf_tohex_n(const a: mp_float; ndd: word; str: pchar8; maxlen: word);
  {-convert an mp_float to hexadecimal scientific representation, ndd>0 is the}
  { total number of digits (including one digit before the '.')}
begin
  mpf_toradix_n(a, 16, ndd, str, maxlen);
end;


{---------------------------------------------------------------------------}
procedure mpf_toradix_n(const a: mp_float; radix, ndd: word; str: pchar8; maxlen: word);
  {-convert an mp_float to radix scientific representation, ndd>0 is the}
  { total number of digits (including one digit before the '.')}
begin
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_toradix_n');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  s_mpf_toradix_n(a, radix, ndd, maxlen, str);
end;


{---------------------------------------------------------------------------}
procedure mpf_toradix_alt(const a: mp_float; radix, ndd: word; str: pchar8; maxlen: word);
  {-convert an mp_float to radix alternative representation, ndd>0 is the total}
  { number of digits, trailing '.' or '0' are suppressed. If a is too large or}
  { too small, scientific representation is used. NOTE: no radix prefix/suffix!}
begin
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_toradix_alt');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  s_mpf_toradix_alt(a, radix, ndd, maxlen, str);
end;


{---------------------------------------------------------------------------}
procedure mpf_trig_ex(const a: mp_float; mulpi: boolean; pc,ps,pt: pmp_float);
  {-calculate pc^=cos(a), ps^=sin(a), pt^=tan(a) if pointers are <> nil.}
  { If mulpi, calculate pc^=cos(a*Pi), ps^=sin(a*Pi), pt^=tan(a*Pi).}
var
  x,y,z: mp_float;
  prec: longint;
  lx,i: longint;
  negc, negst, sita, oddm: boolean;
label
  done;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a)
       or ((pc<>nil) and mpf_not_init(pc^))
       or ((ps<>nil) and mpf_not_init(ps^))
       or ((pt<>nil) and mpf_not_init(pt^))
    then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_trig_ex');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if (pc=nil) and (ps=nil) and (pt=nil) then exit;
  {handle trivial case}
  if s_mpf_is0(a) then begin
    if pc<>nil then mpf_set1(pc^);
    if ps<>nil then mpf_set0(ps^);
    if pt<>nil then mpf_set0(pt^);
    exit;
  end;

  {get maximum precision of output mp_floats}
  prec := 0;
  if (pc<>nil) and (pc^.bitprec>prec) then prec := pc^.bitprec;
  if (ps<>nil) and (ps^.bitprec>prec) then prec := ps^.bitprec;
  if (pt<>nil) and (pt^.bitprec>prec) then prec := pt^.bitprec;

  mpf_initp3(x,y,z,prec+32);
  if mp_error<>MP_OKAY then exit;

  mpf_abs(a,x);
  negc  := false;
  negst := s_mpf_is_neg(a);
  sita  := (ps<>nil) or (pt<>nil);

  {Range reduction mod Pi}
  if mulpi then begin
    {use x=a*Pi, first reduce a mod 1 then multiply by Pi}
    s_mpf_frac(x,x,@oddm);
    if not s_mpf_is0(x) then begin
      mpf_set_pi(y);
      mpf_mul(x,y,x);
    end;
  end
  else begin
    {Reduce modulo pi. WARNING: there may be severe loss of precision}
    {in the mod pi reduction, if the "real" mantissa m of a=m*2^e has}
    {more than prec significant high bits!}
    s_mpf_mod_pi2k(x,0,x,oddm);
  end;

  if oddm then begin
    {change sign because odd multiple of Pi has been used in range reduction}
    negc  := not negc;
    negst := not negst;
  end;

  lx := s_mpf_ldx(x);
  if s_mpf_is0(x) or (lx <= -(prec div 2)) then begin
    {x is zero or small enough to use only one term of Taylor series}
    if pc<>nil then begin
      mpf_set1(pc^);
      if negc then s_mpf_chs(pc^);
    end;
    if negst then s_mpf_chs(x);
    if ps<>nil then mpf_copyp(x,ps^);
    if pt<>nil then mpf_copyp(x,pt^);
    goto done;
  end;

  inc(lx,2);
  if lx>0 then begin
    {range reduction 2: make x < 1/4; "worst" x=0.499999....}
    s_mpf_incexp(x,-lx);
  end;

  {Taylor series h(x) = cos(x) - 1 = - x^2/2! + x^4/4! - x^6/6! + x^6/8! -}
  {Calculate h(x) instead of cos(x) to reduce rounding errors for x near 0}
  {z will accumulate the sum, y is the i-th term.}
  mpf_sqr(x,x);
  mpf_set0(z);
  mpf_set1(y);
  i := 1;
  repeat
    mpf_mul(y,x,y);
    if i<46340 then mpf_div_int(y,-i*succ(i),y)
    else mpf_div_ext(y,-i*(1.0+i),y);
    inc(i,2);
  until not s_mpf_incf(z,y);

  for i:=1 to lx do begin
    {undo range reduction 2 using cos(x)-1 = 2(cos(x/2)^2 - 1)}
    {or h(x) = 2*h(x/2)*(h(x/2)+2), ie iterate z=2*z*(z+2)}
    mpf_set_int(y,2);
    if s_mpf_incf(y,z) then begin
      mpf_mul(z,y,z);
      inc(z.exponent);
    end
    else begin
      {z near 2, ie z=2*z*(z+2) near 4z}
      inc(z.exponent,2);
    end;
  end;

  {here z = h = cos - 1}
  if sita then begin
    {sin and/or tan are requested, first calculate sin = sqrt(1-cos^2) =}
    {sqrt((1-cos)*(1+cos)) = sqrt(|(h+2)*h|) to reduce rounding errors!}
    mpf_set_int(y,2);
    s_mpf_inc(y,z);
    mpf_mul(z,y,y);
    s_mpf_abs(y);
    mpf_sqrt(y,y);
    {then adjust sign if necessary}
    if negst then s_mpf_chs(y);
  end;

  {cos = h+1}
  s_mpf_inc1(z);
  {adjust sign if necessary}
  if negc then s_mpf_chs(z);

  {here y = sin, z = cos}
  if pc<>nil then mpf_copyp(z,pc^);
  if pt<>nil then mpf_div(y,z,pt^);
  if ps<>nil then mpf_copyp(y,ps^);

done:
  mpf_clear3(x,y,z);
end;


{---------------------------------------------------------------------------}
procedure mpf_trig(const a: mp_float; pc,ps,pt: pmp_float);
  {-calculate pc^=cos(a), ps^=sin(a), pt^=tan(a) if pointers are <> nil}
begin
  mpf_trig_ex(a,false,pc,ps,pt);
end;


{---------------------------------------------------------------------------}
procedure mpf_trunc(const a: mp_float; var b: mp_int);
  {-truncate mp_float to mp_int}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mp_not_init(b) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_trunc');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  with a do begin
    if exponent<0 then mp_shr(mantissa,-exponent,b)
    else if exponent>0 then mp_shl(mantissa,exponent,b)
    else mp_copy(mantissa,b);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_writeln(const msg: mp_string; const a: mp_float; ndd: word);
  {-writeln a to output with leading msg}
begin
  write(msg);
  mpf_write_decimal(output,a, ndd);
  writeln;
end;


{---------------------------------------------------------------------------}
procedure mpf_write_decimal(var tf: system.text; const a: mp_float; ndd: word);
  {-write an mp_float to file tf using decimal scientific representation, ndd>0}
  { is the total number of digits (including one digit before the '.')}
begin
  mpf_write_radix(tf, a, 10, ndd);
end;


{---------------------------------------------------------------------------}
procedure mpf_write_decimal_alt(var tf: system.text; const a: mp_float; ndd: word);
  {-write an mp_float to file tf using decimal alternative representation, ndd>0 is}
  { the total number of digits, trailing '.' or '0' are suppressed. If a is too}
  { large or too small, scientific representation is used.}
begin
  mpf_write_radix_alt(tf, a, 10, ndd);
end;


{---------------------------------------------------------------------------}
procedure mpf_write_radix(var tf: system.text; const a: mp_float; radix,ndd: word);
  {-write an mp_float to file tf using radix scientific representation, ndd>0}
  { is the total number of digits (including one digit before the '.')}
var
  ls: longint;
  lw,la: word;
  pc,pt: pchar8;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_write_radix');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  ls := ndd+20;
  if (ls<=$FF00) and (mp_error=MP_OKAY) then begin
    lw := ls and $ffff;
    pc := mp_getmem(lw);
    if pc<>nil then begin
      pt := pc;
      {save alloc count, lw is changed in s_mp_toradix_n} {1.0.14}
      la := lw;
      s_mpf_toradix_n(a,radix, ndd, lw, pt);
      write(tf, pc);
      mp_freemem(pointer(pc),la);
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_write_radix_alt(var tf: system.text; const a: mp_float; radix,ndd: word);
  {-write an mp_float to file tf using radix alternative representation, ndd>0 is}
  { the total number of digits, trailing '.' or '0' are suppressed. If a is too}
  { large or too small, scientific representation is used. NOTE: no radix prefix/suffix!}
var
  ls: longint;
  lw,la: word;
  pc,pt: pchar8;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_write_radix_alt');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  ls := ndd+20;
  if (ls<=$FF00) and (mp_error=MP_OKAY) then begin
    lw := ls and $ffff;
    pc := mp_getmem(lw);
    if pc<>nil then begin
      pt := pc;
      {save alloc count, lw is changed in s_mp_toradix_n} {1.0.14}
      la := lw;
      s_mpf_toradix_alt(a,radix, ndd, lw, pt);
      write(tf, pc);
      mp_freemem(pointer(pc),la);
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure s_mpf_abs(var a: mp_float);
  {-absolute value of an mp_float, no init check}
begin
  with a.mantissa do begin
    if (mp_error=MP_OKAY) and (magic=MP_MAGIC) then sign := MP_ZPOS;
  end;
end;


{---------------------------------------------------------------------------}
procedure s_mpf_add1(const a: mp_float; var b: mp_float);
  {-calculate b = a+1; no init checks}
var
  bd,e1,e2,ed: longint;
  m: mp_int;
begin

  if s_mpf_is0(a) then begin
    mpf_set1(b);
    exit;
  end;

  bd := s_mpf_ldx(a) - 1;

  {check if total bitsizes are too different}
  if abs(bd) > b.bitprec+1 then begin
    if bd<0 then begin
      {a is almost 0 compared to 1}
      mpf_set1(b);
    end
    else begin
      mpf_copyp(a,b);
    end;
    exit;
  end;
  if bd<0 then begin
    e1 := 0;
    e2 := a.exponent;
  end
  else begin
    e1 := a.exponent;
    e2 := 0;
  end;
  ed := e1-e2;

  mp_init(m);
  if mp_error<>MP_OKAY then exit;

  if ed>0 then begin
    if bd<0 then begin
      mp_2expt(m,ed);
      mp_add(a.mantissa, m, b.mantissa);
      b.exponent := e2;
    end
    else begin
      mp_shl(a.mantissa, ed, b.mantissa);
      s_mp_add_d(b.mantissa,1,b.mantissa);
      b.exponent := 0;
    end;
  end
  else if ed<0 then begin
    if bd<0 then begin
      mp_shl(a.mantissa, -ed, b.mantissa);
      s_mp_add_d(b.mantissa,1,b.mantissa);
      b.exponent := 0;
    end
    else begin
      mp_2expt(m,-ed);
      mp_add(a.mantissa, m, b.mantissa);
      b.exponent := e1;
    end;
  end
  else begin
    {ed=0}
    s_mp_add_d(a.mantissa,1,b.mantissa);
  end;
  mp_clear(m);
  s_mpf_normalize(b);
end;


{---------------------------------------------------------------------------}
procedure s_mpf_agm(const a,b: mp_float; var c: mp_float; ps: pmp_float);
  {-calculate c = AGM(|a|,|b|), if ps<>nil set ps^=sum(2^k*c_k^2); ps<>@c, no init check}
var
  g,m,t: mp_float;
  cnt, bd, lim, maxi: longint;
  cchg: boolean;
begin

  if mpf_is0(a) or mpf_is0(b) then begin
    mpf_set0(c);
    if ps<>nil then mpf_set0(ps^);
    exit;
  end;

  mpf_initp3(g,m,t,c.bitprec);
  {m ~ arith, g ~ geo,  g <= m}
  if mpf_cmp_mag(a,b)=MP_LT then begin
    mpf_abs(a,g);
    mpf_abs(b,m);
  end
  else begin
    mpf_abs(a,m);
    mpf_abs(b,g);
  end;
  if ps<>nil then mpf_set0(ps^);

  {limiting precision}
  lim := c.bitprec-1;

  {Upper bound for iteration cnt}
  bd  := abs(s_mpf_ldx(m)) + abs(s_mpf_ldx(g));
  maxi:= 4+round((ln(1.0+bd)+ln(lim))/ln(2));

  cnt := 0;
  repeat
    inc(cnt);
    mpf_sub(g,m,t);
    if s_mpf_is0(t) then break;
    bd := s_mpf_ldx(m) - s_mpf_ldx(t);
    mpf_mul(g,m,g);
    dec(t.exponent);
    {remember if m has changed, break later. If not, update sum with 2^cnt*t^2}
    cchg := s_mpf_incf(m,t);
    if ps<>nil then begin
      mpf_sqr(t,t);
      mpf_mul_2k(t,cnt,t);
      s_mpf_inc(ps^,t);
    end;
    {avoid sqrt calculation if done}
    if (not cchg) or (bd>=lim) or (cnt>maxi) then break;
    mpf_sqrt(g,g);
  until false;
  mpf_exch(m,c);
  mpf_clear3(g,m,t);
end;


{---------------------------------------------------------------------------}
procedure s_mpf_ccell12(const k: mp_float; pCK, pCE: pmp_float);
  {-Complementary complete elliptic integrals of the 1st and 2nd kind using}
  { AGM algorithm; k>0 and pCK <> pCE, with init checks}
var
  c0,s,te,tk: mp_float;
  prec: longint;
begin

  {Ref: M. Abramowitz, I. Stegun; Handbook of Mathematical Functions, 1965}
  {     Ch. 17: Elliptic Integrals, Sec. 17.6: Arithmetic-Geometric Mean}

  { F(x,k) = integral(1/sqrt(1-t^2)/sqrt(1-k^2*t^2),t=0..x)}
  { CK = CK(k) = K'(k) = K(k') = F(1, sqrt(1-k^2))}

  { E(x,k) = integral(sqrt(1-k^2*t^2)/sqrt(1-t^2),t=0..x)}
  { CE = CE(k) = E'(k) = E(k') = E(1, sqrt(1-k^2))}

  { With the HMF notation k = m1^2, ie  m=1-k^2, cf the Ex3/4 in 17.8:}
  { CK=CK(1/9)=K(80/81) ~ 3.59145000, CE=CE(1/9)=E(80/81) ~ 1.019106047}

  { Maple V, Rel.4  notation is  EllipticCK(k) and  EllipticCE(k)}

  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(k) or ((pCK<>nil) and mpf_not_init(pCK^))
       or ((pCE<>nil) and mpf_not_init(pCE^)) then
    begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('s_mpf_ccell12');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {Check k>0 and  @CK <> @CE}
  if s_mpf_is_le0(k) then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXBadArg.Create('s_mpf_ccell12: k <= 0');
      {$else}
        RunError(MP_RTE_BADARG);
      {$endif}
    {$else}
      set_mp_error(MP_BADARG);
      exit;
    {$endif}
  end;
  if pCK=pCE then begin
    if pCK=nil then exit;
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXBadArg.Create('s_mpf_ccell12: @CK = @CE');
      {$else}
        RunError(MP_RTE_BADARG);
      {$endif}
    {$else}
      set_mp_error(MP_BADARG);
      exit;
    {$endif}
  end;

  {get maximum precision of output mp_floats}
  if pCK<>nil then prec := pCK^.bitprec else prec := 0;
  if (pCE<>nil) and (prec < pCE^.bitprec) then prec := pCE^.bitprec;

  {do calculation with slightly increased working precision}
  mpf_initp4(c0,s,te,tk,prec+32);

  {remember c0^2=1-k^2 and call AGM(1,k)}
  mpf_set1(s);
  mpf_sqr(k,c0);
  mpf_sub(s,c0,c0);
  s_mpf_agm(s{=1},k,tk,@te);

  {CE = 1 - 0.5(c0^2 + CE)}
  s_mpf_inc(te,c0);
  s_mpf_incexp(te,-1);
  mpf_sub(s,te,te);

  {CK = pi/2/AGM = pi/2/CK}
  mpf_set_pi2k(s,-1);
  mpf_div(s,tk,tk);

  {E'(k) = K'(k)*(1 - 0.5*(c_0^2 + sum(2^k*c_k^2)))}
  if pCE<>nil then mpf_mul(te,tk,pCE^);
  if pCK<>nil then mpf_copyp(tk,pCK^);
  mpf_clear4(c0,s,te,tk);
end;


{---------------------------------------------------------------------------}
procedure s_mpf_chs(var a: mp_float);
  {-change sign of an mp_float, no init check}
begin
  with a.mantissa do begin
    if (mp_error=MP_OKAY) and (magic=MP_MAGIC) then begin
      if used>0 then sign := sign xor (MP_NEG xor MP_ZPOS)
      else sign := MP_ZPOS;
    end;
  end;
end;


{---------------------------------------------------------------------------}
function s_mpf_cmp_mag(const a,b: mp_float): integer;
  {-compare magnitude of two mp_floats, return sign(|a|-|b|), no init check}
var
  ba,bb: longint;
  t: mp_float;
  sig: word;
begin
  {assign default to keep D6+ happy, combine error test with a=b=0}
  s_mpf_cmp_mag := 0;
  if (mp_error<>MP_OKAY) or (s_mpf_is0(a) and s_mpf_is0(b)) then exit;

  if s_mpf_is0(a) then begin
    {a=0,  |b|>0}
    s_mpf_cmp_mag := -1;
    exit;
  end;
  if s_mpf_is0(b) then begin
    {|a|>0, b=0}
    s_mpf_cmp_mag := 1;
    exit;
  end;

  {if exponents are equal, compare mantissas}
  if a.exponent=b.exponent then begin
    s_mpf_cmp_mag := mp_cmp_mag(a.mantissa, b.mantissa);
    exit;
  end;

  {compare effective bitsizes}
  ba := s_mpf_ldx(a);
  bb := s_mpf_ldx(b);
  if ba>bb then begin
    s_mpf_cmp_mag := 1;
    exit;
  end
  else if ba<bb then begin
    s_mpf_cmp_mag := -1;
    exit;
  end;

  {effective bitsizes are equal but exponents are different}
  {subtract using the maximum bitprec of a and b}
  if a.bitprec < b.bitprec then begin
    mpf_initp(t, b.bitprec);
    if mp_error=MP_OKAY then begin
      mpf_copyp(a,t);
      sig := b.mantissa.sign;
      t.mantissa.sign := sig;
      mpf_sub(t,b,t);
    end
    else exit;
  end
  else begin
    mpf_initp(t, a.bitprec);
    if mp_error=MP_OKAY then begin
      mpf_copyp(b,t);
      sig := a.mantissa.sign;
      t.mantissa.sign := sig;
      mpf_sub(a,t,t);
    end
    else exit;
  end;

  {here t=sign(b)|a|-b or t=a - sign(a)|b|}
  if s_mpf_is0(t) then s_mpf_cmp_mag := 0
  else begin
    {if sig=negative reverse normal compare}
    if sig=MP_NEG then begin
      if t.mantissa.sign=MP_NEG then s_mpf_cmp_mag := 1
      else s_mpf_cmp_mag := -1;
    end
    else begin
      if t.mantissa.sign=MP_NEG then s_mpf_cmp_mag := -1
      else s_mpf_cmp_mag := 1;
    end;
  end;
  mpf_clear(t);
end;


{---------------------------------------------------------------------------}
procedure s_mpf_dec1(var a: mp_float);
  {-calculate a = a-1; no init check}
begin
  {a-1 = -(-a + 1)}
  s_mpf_chs(a);
  s_mpf_inc1(a);
  s_mpf_chs(a);
end;


{---------------------------------------------------------------------------}
procedure s_mpf_frac(const a: mp_float; var b: mp_float; podd: pBoolean);
  {-set b to the fractional part of a; frac(x)=x-int(x), if podd<>nil set podd^=odd(trunc(a))}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) or mpf_not_init(b) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('s_mpf_frac');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  mpf_copyp(a,b);
  with b do begin
    if exponent>=0 then begin
      {a is an integer}
      if podd<>nil then podd^ := (exponent=0) and mp_isodd(mantissa);
      mpf_set0(b);
    end
    else begin
      {exponent<0, test bit 0 of trunc(a) = mantissa shr (-exponent)}
      if podd<>nil then podd^ := mp_isbit(mantissa, -exponent);
      s_mp_mod_2k(mantissa,-exponent,mantissa);
      s_mpf_normalize(b);
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure s_mpf_inc(var x: mp_float; const y: mp_float);
  {-calculate x = x+y}
begin
  if s_mpf_incf(x,y) then ;
end;


{---------------------------------------------------------------------------}
procedure s_mpf_incexp(var a: mp_float; x: longint);
  {-increment a.exponent by x, do nothing if a.mantissa=0}
var
  y : longint;
begin
  if (mp_error<>MP_OKAY) or mp_is0(a.mantissa) then exit;
  if add32_ovr(a.exponent,x,y) then begin
    if a.exponent<0 then begin
      {floating underflow, set a=0}
      mpf_set0(a);
    end
    else begin
      {$ifdef MPC_HaltOnError}
        {$ifdef MPC_UseExceptions}
          raise MPXOverflow.Create('s_mpf_incexp');
        {$else}
          RunError(MP_RTE_OVRFLOW);
        {$endif}
      {$else}
        set_mp_error(MP_OVERFLOW);
        exit;
      {$endif}
    end;
  end
  else a.exponent := y;
end;


{---------------------------------------------------------------------------}
function s_mpf_incf(var x: mp_float; const y: mp_float): boolean;
  {-calculate x = x+y; return true if x is changed}
var
  m: mp_int;
  bd,e1,e2,ed: longint;
  ovr: boolean;
begin
  s_mpf_incf := true;


  if mp_error<>MP_OKAY then exit;
  if s_mpf_is0(y) then begin
    s_mpf_incf := false;
    exit;
  end;

  if s_mpf_is0(x) then begin
    mpf_copyp(y,x);
    exit;
  end;
  if @x=@y then begin
    {x is non zero, so increment exponent is OK}
    s_mpf_incexp(x,1);
    exit;
  end;

  {check if total bitsizes are very different}
  ovr := add32_ovr(s_mpf_ldx(x), -s_mpf_ldx(y), bd);
  if (not ovr) and (abs(bd) > x.bitprec+1) then begin
    if bd<0 then begin
      {x is almost 0 compared to y}
      mpf_copyp(y,x);
    end
    else s_mpf_incf := false;
    exit;
  end;

  {assign e1,e2 even if overflow to avoid compiler warnings}
  if bd<0 then begin
    e1 := y.exponent;
    e2 := x.exponent;
  end
  else begin
    e1 := x.exponent;
    e2 := y.exponent;
  end;

  ovr := ovr or add32_ovr(e1, -e2, ed);
  if ovr then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXOverflow.Create('s_mpf_incf');
      {$else}
        RunError(MP_RTE_OVRFLOW);
      {$endif}
    {$else}
      set_mp_error(MP_OVERFLOW);
      exit;
    {$endif}
  end;

  mp_init(m);
  if mp_error<>MP_OKAY then exit;

  if ed>0 then begin
    if bd<0 then begin
      mp_shl(y.mantissa, ed, m);
      mp_add(x.mantissa, m, x.mantissa);
    end
    else begin
      mp_shl(x.mantissa, ed, m);
      mp_add(y.mantissa, m, x.mantissa);
    end;
    x.exponent := e2;
  end
  else if ed<0 then begin
    if bd<0 then begin
      mp_shl(x.mantissa, -ed, m);
      mp_add(y.mantissa, m, x.mantissa);
    end
    else begin
      mp_shl(y.mantissa, -ed, m);
      mp_add(x.mantissa, m, x.mantissa);
    end;
    x.exponent := e1;
  end
  else begin
    {ed=0}
    mp_add(y.mantissa, x.mantissa, x.mantissa);
  end;

  mp_clear(m);
  s_mpf_normalize(x);
end;


{---------------------------------------------------------------------------}
procedure s_mpf_inc1(var a: mp_float);
  {-calculate a = a+1; no init check}
begin
  s_mpf_add1(a,a);
end;


{---------------------------------------------------------------------------}
function s_mpf_is0(const a: mp_float): boolean;
  {-return true if a=0, no init checks}
begin
  s_mpf_is0 := (a.exponent=0) and (a.mantissa.used=0);
end;


{---------------------------------------------------------------------------}
function s_mpf_is_ge0(const a: mp_float): boolean;
  {-return true if a>=0, no init checks}
begin
  s_mpf_is_ge0 := (a.mantissa.sign=MP_ZPOS);
end;


{---------------------------------------------------------------------------}
function s_mpf_is_le0(const a: mp_float): boolean;
  {-return true if a<=0, no init checks}
begin
  s_mpf_is_le0 := (a.mantissa.sign=MP_NEG) or ((a.exponent=0) and (a.mantissa.used=0));
end;


{---------------------------------------------------------------------------}
function s_mpf_is_neg(const a: mp_float): boolean;
  {-return true if a<0, no init checks}
begin
  s_mpf_is_neg := a.mantissa.sign=MP_NEG;
end;


{---------------------------------------------------------------------------}
function s_mpf_ldx(const a: mp_float): longint;
  {-return ldx(a) = a.exponent+mp_bitsize(a.mantissa), no init checks, a must be}
  { normalized, ldx(a) = floor(log2(|a|))+1, -MaxLongint if a=0, MaxLongint if overflow}
var
  ldx: longint;
begin
  {s_mpf_ldx := a.exponent + mp_bitsize(a.mantissa);}
  with a.mantissa do begin
    if used>0 then begin
      if add32_ovr(a.exponent, longint(used-1)*DIGIT_BIT + bitsize32(pdigits^[used-1]), ldx) then begin
        s_mpf_ldx := MaxLongint;
      end
      else s_mpf_ldx := ldx;
    end
    else s_mpf_ldx := -MaxLongint;
  end;
end;


{---------------------------------------------------------------------------}
procedure s_mpf_numbpart(n: longint; var p: mp_float);
  {-Compute number of partitions of n with Hardy-Ramanujan-Rademacher formula}
var
  pfull,pq,q,sn: longint;
  bn,cn,x,y: mp_float;

  {---------------------------------------------------------------------------}
  procedure sigma(h,k: longint; var a,b: longint);
    {-calculate Dedekind sum sigma(h,k) = a+b/k}
  var
    aa,p,pp,r,s: longint;
  begin
    {sigma is computed with Knuth's algorithm [3], Chap 3.3.3, Ex.17}
    {specialized to c=0}
    a := 0;
    b := h;
    p := 1;
    pp:= 0;
    s := 1;
    while h>0 do begin
      aa := k div h;
      a  := a + (aa-3)*s;
      if h=1 then b := b +p*s;
      s := -s;
      r := k-aa*h;
      k := h;
      h := r;
      r := aa*p + pp;
      pp:= p;
      p := r;
    end;
  end;

  {-------------------------------------------------------------------}
  procedure Lq(n,q: longint; var L: mp_float);
  var
    h,a,b: longint;
    x,y: mp_float;
  begin
    if q=1 then mpf_set1(L)
    else begin
      mpf_initp2(x,y,L.bitprec);
      if mp_error<>MP_OKAY then exit;
      mpf_set0(L);
      {L(n,q) = sum(h=1..q-1 and gcd(h,q)=1,cos((g(h,q)-2*h*n)*Pi/q)))}
      for h:=1 to q-1 do begin
        if mp_error<>MP_OKAY then exit;
        if gcd32(h,q)=1 then begin
          {compute g(h,q)=q*s(h,q)=q/12*sigma(h,q)=q/12*(a+b/q)=(a*q+b)/12}
          if q<3 then mpf_set0(x)
          else begin
            sigma(h,q,a,b);
            mpf_set_int(x,a);
            mpf_mul_int(x,q,x);
            mpf_add_int(x,b,x);
            mpf_div_d(x,12,x);
          end;
          mpf_set_int(y,2*h);
          mpf_mul_int(y,n,y);
          mpf_sub(x,y,x);
          mpf_div_int(x,q,x);
          {x=cos(Pi*x)}
          mpf_trig_ex(x,true,@y,nil,nil);
          mpf_add(L,y,L);
        end;
      end;
      mpf_clear2(x,y);
    end;
  end;

  {-------------------------------------------------------------------}
  procedure Psiq(q: longint; const b, c: mp_float; var p: mp_float);
  var
    a,d: mp_float;
  begin
    {Psi(n,q) = (sqrt(q)/(2*sqrt(2)*b*Pi))*(a*cosh(d)-(sinh(d)/c))}
    {with a = sqrt(2/3)*Pi/q,  b = n-1/24,  c = sqrt(b),  d = a*c}
    mpf_initp2(a,d,p.bitprec);
    if mp_error<>MP_OKAY then exit;
    {a = sqrt(2/3)*Pi/q}
    mpf_set_pi(a);
    mpf_set_int(d,2);
    mpf_div_d(d,3,d);
    mpf_sqrt(d,d);
    mpf_mul(a,d,a);
    mpf_div_int(a,q,a);
    {d = a*c}
    mpf_mul(a,c,d);
    {p = sinh(d)/c,  d = a*cosh(d)}
    mpf_sinhcosh(d,p,d);
    mpf_mul(d,a,d);
    mpf_div(p,c,p);
    {p = a*cosh(d) - sinh(d)/c}
    mpf_sub(d,p,p);
    {d = sqrt(q/8)/(b*Pi)}
    mpf_set_pi(d);
    mpf_mul(d,b,d);
    mpf_set_int(a,q);
    s_mpf_incexp(a, -3);
    mpf_sqrt(a,a);
    mpf_div(a,d,d);
    {Final result = d*p}
    mpf_mul(d,p,p);
    mpf_clear2(a,d);
  end;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(p) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('s_mpf_numbpart');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  if n<0 then begin
    mpf_set0(p);
    exit;
  end
  else if n<2 then begin
    mpf_set1(p);
    exit;
  end;

  {c.f. the Pari implementation [12] based on Ralf Stephan's script and code}
  {http://www.ark.in-berlin.de/part.c and http://www.ark.in-berlin.de/part.pdf}
  {numpart(n) = sum(q=1,5 + 0.24*sqrt(n),L(n,q)*Psi(n,q)))}
  {Get estimated bit prec based on first term of HRR series}
  {p(n) ~ exp(pi*sqrt(2n/3))/(4n*sqrt(3))}
  pfull := MPF_MIN_PREC + round(sqrt(2*n/3.0)*pi/ln(2.0));
  p.bitprec := pfull;

  sn := round(5+0.24*sqrt(n));
  mpf_set0(p);
  mpf_initp4(bn,cn,x,y,p.bitprec);

  {Pre-compute bn,cn, because they are independent of q}
  {bn = n-1/24}
  mpf_set1(bn);
  mpf_div_int(bn,-24,bn);
  mpf_add_int(bn,n,bn);
  {cn = sqrt(bn)}
  mpf_sqrt(bn,cn);

  {sum backwards with increasing precision}
  for q:=sn downto 1 do begin
    pq := 8 + pfull div q;
    if pq<32 then pq := 32;
    x.bitprec := pq;
    y.bitprec := pq;
    Lq(n,q,x);
    if s_mpf_is0(x) or (s_mpf_ldx(x)<-pq) then continue;
    Psiq(q,bn,cn,y);
    mpf_mul(x,y,x);
    mpf_add(p,x,p);
  end;
  mpf_clear4(bn,cn,x,y);
end;


{---------------------------------------------------------------------------}
procedure s_mpf_mod_pi2k(const a: mp_float; k: integer; var b: mp_float; var oddm: boolean);
  {-calculate b = a mod pi*2^k, c in [0,pi*2^k); oddm: odd multiple of pi*2^k}
  { was used for reduction; no init check; extended precision used if necessary.}
var
  x,pi2k: mp_float;
  px: longint;
begin
  oddm := false;
  if mp_error<>MP_OKAY then exit;

  if s_mpf_is0(a) then begin
    mpf_set0(b);
    exit;
  end;

  {Use extended precision greater than b.bitprec if necessary}
  {k should be small, ie about -2 .. 2, otherwise adjust px}
  px := s_mpf_ldx(a);
  if px < 0 then px := 0;
  inc(px,b.bitprec+32);

  {$ifdef MPC_USE_Assert}
    {only if assert supported by compiler or debug}
    assert(px<MPF_MAX_PREC,'s_mpf_mod_pi2k: precision < MPF_MAX_PREC');
  {$endif}

  mpf_initp2(x,pi2k,px);
  if mp_error<>MP_OKAY then exit;

  mpf_set_pi2k(pi2k,k);
  if mpf_cmp_mag(a,pi2k)=MP_LT then begin
    {abs(a) < pi2k, just copy a to x for sign adjustment}
    mpf_copyp(a,x);
  end
  else begin
    {x = a - int(a/pi2k)*pi2k}
    mpf_div(a,pi2k,x);
    mpf_int(x,x);
    oddm := mp_isbit(x.mantissa,-x.exponent);
    mpf_mul(x,pi2k,x);
    mpf_sub(a,x,x);
  end;

  if s_mpf_is_neg(x) then begin
    mpf_add(x,pi2k,x);
    oddm := not oddm;
  end;
  {$ifdef MPC_USE_Assert}
    {only if assert supported by compiler or debug}
    assert((x.mantissa.sign=MP_ZPOS) and mpf_is_lt(x,pi2k),' 0 <= a mod pi*2^k < pi*2^k');
  {$endif}
  s_mpf_normalizep(x,b.bitprec);
  mpf_exch(x,b);
  mpf_clear2(x,pi2k);
end;


{---------------------------------------------------------------------------}
procedure s_mpf_normalize(var a: mp_float);
  {-normalize an mp_float}
var
  bs,diff: longint;
  dm,d0: mp_digit;
  up: word;
  mustround,sticky: boolean;
begin
  if mp_error<>MP_OKAY then exit;
  with a do begin
    bs := mp_bitsize(mantissa);
    if bs > bitprec then begin
      diff := bs - bitprec;
      s_mpf_incexp(a, diff);
      {mustround true if rounding bit diff-1 is set}
      mustround := mp_isbit(mantissa, pred(diff));
      {sticky bit present if any bit below rounding bit is set}
      sticky    := mustround and (mp_cnt_lsb(mantissa) < pred(diff));
      mp_shr(mantissa, diff, mantissa);
      {mantissa is still non zero}
      if mustround then with mantissa do begin
        d0 := pdigits^[0];
        if odd(d0) then begin
          {must inc/dec an odd number, remember used and high digit}
          up := used;
          dm := pdigits^[up-1];
          {round to even with odd mantissa: inc if positive, dec}
          {if negative: 1.5 -> 2 not 1,  -1.5 -> -2 not -1}
          if sign=MP_ZPOS then mp_inc(mantissa) else mp_dec(mantissa);
          if (up<>used) or (dm<>pdigits^[up-1]) then begin
            {a.used or highest digit changed: check if bitsize>bitprec}
            if mp_bitsize(mantissa)>bitprec then begin
              {postnormalization}
              mp_shr(mantissa, 1, mantissa);
              s_mpf_incexp(a,1);
            end;
          end;
        end
        else begin
          {even mantissa, no postnormalization necessary}
          if sticky then pdigits^[0] := d0+1;
        end;
      end;
    end
    else if bs < bitprec then begin
      if bs=0 then mpf_set0(a)
      else begin
        diff := bitprec - bs;
        s_mpf_incexp(a, -diff);
        mp_shl(mantissa, diff, mantissa);
      end;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure s_mpf_normalizep(var a: mp_float; prec: longint);
  {-normalize an mp_float to bit precision prec}
begin
  if mp_error<>MP_OKAY then exit;
  if (prec>=MPF_MIN_PREC) and (prec<=MPF_MAX_PREC) then begin
    a.bitprec := prec;
    s_mpf_normalize(a);
  end;
end;


{---------------------------------------------------------------------------}
procedure s_mpf_toradix_n(const a: mp_float; radix, ndd: word; var maxlen: word; var pstr: pchar8);
  {-convert an mp_float to radix scientific representation, ndd>0 is the}
  { total number of digits (including one digit before the '.')}
const
  ovmsg: array[1..10] of char8 = '*overflow*';
var
  e: longint;
  x,y: mp_float;
  ps1,ps2: pchar8;
  i: word;
  d: mp_digit;
  t: string[20];
  xc: char8;

  procedure appchar(c: char8);
    {-append char at pstr^}
  begin
    if maxlen>1 then begin
      pstr^ := c;
      inc(pstr);
      dec(maxlen);
    end;
  end;

begin

  if mp_error<>MP_OKAY then exit;
  if (radix < 2) or (radix > MAXRadix) or (maxlen <= ndd) then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mpf_toradix_n: radix out of range or maxlen too small');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;

  pstr^ := #0;
  if ndd=0 then exit;

  if s_mpf_is0(a) then begin
    {special case a=0}
    if maxlen>ndd+2+ord(mp_show_plus) then begin
      if mp_show_plus then appchar('+');
      appchar('0');
      appchar(mp_fract_sep{'.'});
      for i:=2 to ndd do appchar('0');
      pstr^ := #0;
      dec(maxlen);
    end;
    exit;
  end;

  {Step 0: get raw binary exponent e}
  e := s_mpf_ldx(a);
  if add32_ovr(a.exponent,mp_bitsize(a.mantissa),e) then begin
    {This is a conversion problem, bsx(a) overflows}
    for i:=1 to sizeof(ovmsg) do appchar(ovmsg[i]);
    pstr^ := #0;
    dec(maxlen);
    exit;
  end;

  mpf_initp2(x,y,a.bitprec+32);

  {Step 1: calculate abs(a) = x*radix^e with 1 <= x < radix}
  mpf_set_int(y,radix); {y=radix in Step 1}

  {Step 1a: get raw radix exponent e and raw x}
  if radix>2 then e := trunc(lograd[radix]*e);
  if e<>0 then begin
    mpf_expt_int(y,e,x);
    mpf_div(a,x,x);
    s_mpf_abs(x);
  end
  else mpf_abs(a,x);

  {Step 1b: make x<radix}
  while (mp_error=MP_OKAY) and mpf_is_ge(x,y) do begin
    mpf_div(x,y,x);
    inc(e);
  end;

  {Step 1c: make x>=1}
  while (mp_error=MP_OKAY) and (s_mpf_ldx(x) < 1) do begin
    mpf_mul_d(x,radix,x);
    dec(e);
  end;

  {Step 2: multiply by radix^ndd, this gives ndd+1 radix digits}
  if mpf_is1(x) then begin
    mp_set_pow(x.mantissa,radix,ndd);
  end
  else begin
    mp_set_pow(y.mantissa,radix,ndd);
    mpf_mul_mpi(x,y.mantissa,x);
    mpf_trunc(x,x.mantissa);
  end;

  {Step 3: divide by radix and round}
  mp_div_d(x.mantissa,radix,@x.mantissa,d);
  d := 2*d;
  if (d > radix) or ((d=radix) and mp_isodd(x.mantissa)) then begin
    mp_inc(x.mantissa);
    if mp_is_eq(x.mantissa,y.mantissa) then begin
      inc(e);
      {V1.4.05 - Fix if rounded x = radix^ndd: increment exponent AND }
      {divide by radix, otherwise there is an extra trailing '0'!}
      mp_div_d(x.mantissa, radix, @x.mantissa, d);
    end;
  end;

  {Step 4; convert exponent part to string}
  if e<>0 then begin
    case radix of
        2: xc := 'B';
       10: xc := 'E';
       16: xc := 'H';
      else xc := '@';
    end;
    str(e,t);
    {D12 warning fix: use char8 type cast}
    if e>0 then t := xc+char8('+')+t else t := xc+t;
  end
  else t := '';

  {Step 5: convert mantissa, fix and combine parts}
  if (mp_error=MP_OKAY) and (mp_radix_size(x.mantissa,radix)+2+length(t)<maxlen) then begin
    {insert sign char}
    if s_mpf_is_neg(a) then appchar('-')
    else if mp_show_plus then appchar('+');
    {insert '.', will be swapped later}
    ps1   := pstr;
    appchar(mp_fract_sep{'.'});
    ps2   := pstr;
    {convert mantissa}
    s_mp_toradix_n(x.mantissa, radix, false, maxlen, pstr);
    {swap '.' with first digit}
    if ps2^<>#0 then begin
      ps1^ := ps2^;
      ps2^ := mp_fract_sep{'.'};
    end;
    {append exponent}
    for i:=1 to length(t) do appchar(t[i]);
    pstr^ := #0;
    dec(maxlen);
  end;
  mpf_clear2(x,y);
end;


{---------------------------------------------------------------------------}
procedure s_mpf_toradix_alt(const a: mp_float; radix, ndd: word; var maxlen: word; var pstr: pchar8);
  {-convert an mp_float to radix alternative representation, ndd>0 is the total}
  { number of digits, trailing '.' or '0' are suppressed. If a is too large or}
  { too small, scientific representation is used. NOTE: no radix prefix/suffix!}
var
  x: mp_float;
  f,p,q: mp_int;
  k,e: longint;
  lra: double;
  prmap: ^TRadixCMap;
  d,rsqr: mp_digit;
  n: integer;
  psav: pchar8;
  reven: boolean;

  procedure appchar(c: char8);
    {-Append char at pstr^}
  begin
    if maxlen>1 then begin
      pstr^ := c;
      inc(pstr);
      dec(maxlen);
    end;
  end;

begin

  if mp_error<>MP_OKAY then exit;
  if (radix < 2) or (radix > MAXRadix) then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('s_mpf_toradix_alt: radix out of range or maxlen too small');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;

  pstr^ := #0;
  if ndd=0 then exit;

  if s_mpf_is0(a) then begin
    {special case a=0}
    if maxlen>ndd+2+ord(mp_show_plus) then begin
      appchar('0');
      pstr^ := #0;
      dec(maxlen);
    end;
    exit;
  end;

  {Basic principle of the alternative output format: With a suitable k}
  {compute m = round(x*radix^k), p = m div radix^k, q = m div radix^k.}
  {Output the integer part p, and if q<>0 then output '.' and q. }

  {Get raw binary exponent e and approximate logbase(radix,a)}
  e := s_mpf_ldx(a);
  if add32_ovr(a.exponent,mp_bitsize(a.mantissa),e) then begin
    {set lra to force scientific format, (and avoid warning 'k not initialized')}
    lra := ndd;
    {$ifdef Delphi}
      k := 0;
    {$endif}
  end
  else begin
    if radix=2 then lra := e else lra:= e*lograd[radix];
    if lra<0 then e:=0 else e := trunc(lra);
    {Get suitable radix exponent k for conversion}
    k := ndd-e-1;
  end;
  if (lra>=ndd) or (lra>=maxlen) or ((lra+ndd)<1) or (k<0) then begin
    {Use scientific format if alternative format not applicable}
    s_mpf_toradix_n(a, radix, ndd, maxlen, pstr);
    exit;
  end;

  mpf_initp(x,a.bitprec+32);
  if mp_error<>0 then exit;
  mp_init3(f,p,q);
  if mp_error<>0 then begin
    mpf_clear(x);
    exit;
  end;

  {Multiply with radix^k, make positive, and round to nearest}
  mp_set_pow(f,radix,k);
  mpf_mul_mpi(a,f,x);
  s_mpf_abs(x);
  mpf_round(x,x.mantissa);

  {Divide by radix^k and process quotient and remainder}
  mp_divrem(x.mantissa,f,@p,@q);

  {Insert sign of input}
  if s_mpf_is_neg(a) then appchar('-')
  else if mp_show_plus then appchar('+');

  {Convert integer part}
  s_mp_toradix_n(p, radix, false, maxlen, pstr);

  {Convert fractional part only if non-zero.}
  if (maxlen>1) and (not mp_is0(q)) then begin
    if maxlen=2 then begin
      {Only fract_sep and #0 can be inserted}
      appchar(mp_fract_sep)
    end
    else begin
      if maxlen>k+1 then begin
        {save fract_sep position in psav}
        psav := pstr;
        {Add radix^k, this will result in '1' at fract_sep position}
        mp_add(q,f,q);

        {remove trailing zeros}
        if IsPow2_w(radix, n) then begin
          {use shifts if radix is power of two}
          e := mp_cnt_lsb(q);
          if e>0 then begin
            e := (e div n)*n;
            if e>0 then mp_shr(q, e, q);
          end;
        end
        else begin
          reven := radix and 1 = 0;
          {try to remove pairs of zeros first, i.e. divide by radix^2}
          if mp_radexp[radix]=1 then rsqr := radix else rsqr := sqr(radix);
          {first loop using max. power of radix that fits into an mp_digit}
          repeat
            {loop invariant: q<>0. Done if radix is even and q is odd}
            if reven and odd(q.pdigits^[0]) then break;
            mp_div_d(q,rsqr,@f,d);
            if d=0 then mp_exch(q,f)
            else begin
              {d = q mod rsqr is nonzero, do final division only if d=0 mod radix}
              if (d mod radix <> 0) or (reven and odd(q.pdigits^[0])) then break;
              mp_div_d(q,radix,@f,d);
              if d=0 then mp_exch(q,f)
              else break;
            end;
          until false;
        end;

        s_mp_toradix_n(q, radix, false, maxlen, pstr);
        {$ifdef MPC_USE_Assert}
          assert(psav^='1','psav^ = ''1'' in s_mpf_toradix_alt');
        {$endif}
        {replace the '1' at fract_sep position}
        psav^ := mp_fract_sep;
      end
      else begin
        {Here maxlen <= k+1 and the fractional part will be truncated. Use slow}
        {double division loop to get the correct truncated sequence of digits}
        appchar(mp_fract_sep);
        {local pointer to radix map}
        if mp_uppercase or (radix>36) then prmap := @mp_ucrmap else prmap := @mp_lcrmap;
        {Calculate the quotients q = q mod radix^(i-1) and the remainders}
        {p_i = q div radix^(i-1) with 0 <= p_i < radix, i=k...}
        while (maxlen>1) and (not mp_is0(q)) do begin
          mp_div_d(f,radix,@f,d);
          mp_divrem(q,f,@p,@q);
          if p.used>0 then begin
            d := p.pdigits^[0];
            {$ifdef MPC_USE_Assert}
              assert((p.used=1) and (d<radix),'d<radix in s_mpf_toradix_alt');
            {$endif}
          end;
          appchar(prmap^[d]);
        end;
      end
    end;
  end;
  pstr^ := #0;
  dec(maxlen);
  mpf_clear(x);
  mp_clear3(f,p,q);
end;


{---------------------------------------------------------------------------}
{---------------------------------------------------------------------------}

type
  t_mpfunc = procedure(const a: mp_float; var b: mp_float);


{---------------------------------------------------------------------------}
procedure inv_func(f: t_mpfunc; const a: mp_float; var b: mp_float);
  {-calculate b := f(1/a) with 32 guard bits}
var
  t: mp_float;
begin
  if mp_error<>MP_OKAY then exit;
   {$ifdef MPC_ArgCheck}
    _CheckBitPrec(b);
  {$endif}
  mpf_initp(t, b.bitprec+32);
  if mp_error=MP_OKAY then begin
    mpf_inv(a,t);
    f(t,t);
    mpf_copyp(t,b);
    mpf_clear(t);
  end;
end;


{---------------------------------------------------------------------------}
procedure func_inv(f: t_mpfunc; const a: mp_float; var b: mp_float);
  {-calculate b := 1/f(a) with 32 guard bits}
var
  t: mp_float;
begin
  if mp_error<>MP_OKAY then exit;
   {$ifdef MPC_ArgCheck}
    _CheckBitPrec(b);
  {$endif}
  mpf_initp(t, b.bitprec+32);
  if mp_error=MP_OKAY then begin
    f(a,t);
    mpf_inv(t,b);
    mpf_clear(t);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_cot(const a: mp_float; var b: mp_float);
  {-calculate the circular cotangent b := cot(a), a mod Pi <> 0}
begin
  func_inv({$ifdef FPC_ProcVar}@{$endif}mpf_tan,a,b);
end;


{---------------------------------------------------------------------------}
procedure mpf_sec(const a: mp_float; var b: mp_float);
  {-calculate the circular secant b = sec(a), a mod Pi <> Pi/2}
begin
  func_inv({$ifdef FPC_ProcVar}@{$endif}mpf_cos,a,b);
end;


{---------------------------------------------------------------------------}
procedure mpf_csc(const a: mp_float; var b: mp_float);
  {-calculate the circular cosecant b = csc(a), a mod Pi <> 0}
begin
  func_inv({$ifdef FPC_ProcVar}@{$endif}mpf_sin,a,b);
end;


{---------------------------------------------------------------------------}
procedure mpf_coth(const a: mp_float; var b: mp_float);
  {-calculate the hyperbolic cotangent b = coth(a), a <> 0}
begin
  func_inv({$ifdef FPC_ProcVar}@{$endif}mpf_tanh,a,b);
end;


{---------------------------------------------------------------------------}
procedure mpf_sech(const a: mp_float; var b: mp_float);
  {-calculate the hyperbolic secant b = sech(a)}
begin
  func_inv({$ifdef FPC_ProcVar}@{$endif}mpf_cosh,a,b);
end;


{---------------------------------------------------------------------------}
procedure mpf_csch(const a: mp_float; var b: mp_float);
  {-calculate the hyperbolic cosecant b = csch(a), a <> 0}
begin
  func_inv({$ifdef FPC_ProcVar}@{$endif}mpf_sinh,a,b);
end;


{---------------------------------------------------------------------------}
procedure mpf_arccot(const a: mp_float; var b: mp_float);
  {-calculate the sign symmetric inverse circular cotangent; b = arccot(a) = arctan(1/a), a <> 0}
var
  neg: boolean;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mpf_not_init(a) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mpf_arccot');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  neg := s_mpf_is_neg(a);
  if s_mpf_ldx(a) > -(b.bitprec+32) then begin
    mpf_inv(a,b);
    mpf_arctan(b,b);
  end
  else begin
    mpf_set_pi2k(b,-1);
    if neg then s_mpf_chs(b);
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_arccotc(const a: mp_float; var b: mp_float);
  {-calculate the continous inverse circular cotangent; b = arccotc(a) = Pi/2 - arctan(a)}
var
  t: mp_float;
begin
  if mp_error<>MP_OKAY then exit;
   {$ifdef MPC_ArgCheck}
    _CheckBitPrec(b);
  {$endif}
  if s_mpf_is_neg(a) then begin
    mpf_initp(t, b.bitprec);
    if mp_error=MP_OKAY then begin
      mpf_arctan(a,b);
      mpf_set_pi2k(t,-1);
      mpf_sub(t,b,b);
      mpf_clear(t);
    end;
  end
  else mpf_arccot(a,b);
end;


{---------------------------------------------------------------------------}
procedure mpf_arccsc(const a: mp_float; var b: mp_float);
  {-calculate the inverse circular cosecant b = arccsc(a), |a| >= 1}
begin
  inv_func({$ifdef FPC_ProcVar}@{$endif}mpf_arcsin, a,b);
end;


{---------------------------------------------------------------------------}
procedure mpf_arcsec(const a: mp_float; var b: mp_float);
  {-calculate the inverse circular secant b = arcsec(a), |a| >= 1}
begin
  inv_func({$ifdef FPC_ProcVar}@{$endif}mpf_arccos,a,b);
end;


{---------------------------------------------------------------------------}
procedure mpf_arccoth(const a: mp_float; var b: mp_float);
  {-calculate the inverse hyperbolic cotangent b = arccoth(a), |a| > 1}
begin
  inv_func({$ifdef FPC_ProcVar}@{$endif}mpf_arctanh,a,b);
end;


{---------------------------------------------------------------------------}
procedure mpf_arccsch(const a: mp_float; var b: mp_float);
  {-calculate the inverse hyperbolic cosecant b = arccsch(a)}
begin
  inv_func({$ifdef FPC_ProcVar}@{$endif}mpf_arcsinh,a,b);
end;


{---------------------------------------------------------------------------}
procedure mpf_arcsech(const a: mp_float; var b: mp_float);
  {-calculate the inverse hyperbolic secant b = arcsech(a), 0 < a <= 1}
begin
  inv_func({$ifdef FPC_ProcVar}@{$endif}mpf_arccosh,a,b);
end;


{---------------------------------------------------------------------------}
{---------------------------------------------------------------------------}
{---------------------------------------------------------------------------}


begin
  mpf_set_default_prec(240);
end.

