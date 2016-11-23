unit mp_numth;

{MP integer modular arithmetic and number theoretic functions}

interface

{$i STD.INC}

uses
  mp_types;

{$i mp_conf.inc}

(*************************************************************************

 DESCRIPTION   :  MP modular arithmetic and number theoretic functions

 REQUIREMENTS  :  BP7, D1-D7/D9-D10/D12, FPC, VP

 EXTERNAL DATA :  (mp_types)

 MEMORY USAGE  :  heap

 DISPLAY MODE  :  (text if mp_show_progress)

 REFERENCES    :  [1] LibTomMath 0.30+ by Tom St Denis
                  [2] MPI, M.J. Fromberger, http://spinning-yarns.org/michael/sw/
                  [3] Knuth, D.E.: The Art of computer programming. Vol 2
                      Seminumerical Algorithms, 3rd ed., 1998
                  [4] Forster, O.: Algorithmische Zahlentheorie, 1996
                  [5] (HAC) Menezes,A., von Oorschot,P., Vanstone, S: Handbook of
                      Applied Cryptography, 1996, www.cacr.math.uwaterloo.ca/hac
                  [6] R. P. Brent, Factor: an integer factorization program for
                      the IBM PC, Report TR-CS-89-23, October 1989, 7 pp.
                      http://wwwmaths.anu.edu.au/~brent/pub/pub117.html
                      http://wwwmaths.anu.edu.au/~brent/ftp/rpb117/rpb117.exe
                  [7] P. Ribenboim: The New Book of Prime Number Records, 3rd ed., 1995.
                  [8] Marcel Martin: NX - Numerics library of multiprecision
                      numbers for Delphi and Free Pascal, 2006-2009
                      www.ellipsa.eu/public/nx/index.html
                  [9] Wei Dai: Lucas Sequences in Cryptography,
                      http://www.weidai.com/lucas.html
                 [10] Crandall,R., C.Pomerance: Prime Numbers, A Computational
                      Perspective, 2nd ed., 2005
                 [11] Peter Luschny's Homepage of Factorial Algorithms,
                      http://www.luschny.de/math/factorial/FastFactorialFunctions.htm
                 [12] PARI/GP at http://pari.math.u-bordeaux.fr/
                 [23] J. Shallit, J. Sorenson, A Binary Algorithm for the Jacobi Symbol,
                      SIGSAM Bulletin, 27(1), 4-11, 1993; available online at
                      http://euclid.butler.edu/~sorenson/papers/binjac.ps or
                      http://citeseer.ist.psu.edu/article/shallit93binary.html
                 [24] H. Cohen, A Course in Computational Algebraic Number Theory
                      4th printing, 2000
                 [25] IEEE P1363/Draft. Standard Specifications for Public Key Cryptography.
                      Annex A (informative). Number-Theoretic Background.
                      http://grouper.ieee.org/groups/1363/P1363/draft.html
                 [26] A. Adler, J.E. Coury: The Theory of Numbers, 1995
                 [28] J.P. Sorenson, An analysis of Lehmer's Euclidean GCD algorithm,
                      ACM International Symposium on Symbolic and Algebraic Computation, 1995
                      http://euclid.butler.edu/~sorenson/papers/lehmer.pdf
                 [30] J. v. zur Gathen, J. Gerhard, Modern computer algebra, 2nd ed., 2003
                      http://math-www.uni-paderborn.de/mca/
                 [32] S.C. Lindhurst, Computing Roots in Finite Fields and Groups, with a
                      Jaunt through Sums of Digits, University of Wisconsin, Madison 1997
                      http://scott.lindhurst.com/papers/thesis.ps.gz


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.0.01   09.08.04  W.Ehrhardt  Initial version: mp_addmod, mp_submod
 0.0.02   09.08.04  we          mp_mulmod, mp_sqrmod
 0.0.03   09.08.04  we          raw versions of mp_pollard_rho, mp_gcd
 0.0.04   09.08.04  we          mp_pollard_rho with Barrett reduction
 0.0.05   09.08.04  we          mp_gcd (binary version), mp_exteuclid (xgcd)
 0.0.06   10.08.04  we          mp_pollard_rho: Barrett after x*x+2
 0.0.07   11.08.04  we          mp_lcm
 0.0.08   11.08.04  we          mp_invmod using xgcd
 0.0.08   12.08.04  we          mp_invmodb using binary gcd
 0.0.09   12.08.04  we          fast_mp_invmod, inv_mod -> inv_mod_euclid, inv_modb -> inv_mod
 0.0.10   12.08.04  we          code cleanup
 0.0.11   13.08.04  we          s_mp_exptmod
 0.0.12   13.08.04  we          basic mp_exptmod {*tbd}
 0.0.13   14.08.04  we          WEXP_ constants
 0.0.14   14.08.04  we          mp_pollard_rho with cnt parameter
 0.0.15   17.08.04  we          mp_is_square
 0.0.16   17.08.04  we          mp_small_factor
 0.0.17   17.08.04  we          mp_small_factor with mod 30 increments
 0.0.18   18.08.04  we          mp_is_SPP
 0.0.19   22.08.04  we          small prime tables
 0.0.20   22.08.04  we          mp_miller_rabin
 0.0.21   23.08.04  we          mp_show_progress variable, mp_small_factor with f0
 0.0.22   23.08.04  we          Prime127 removed, reordering of functions
 0.0.23   24.08.04  we          mp_ppexpo, mp_pollard_pm1
 0.0.24   24.08.04  we          code cleanup, updated references

 0.3.00   26.08.04  we          "release" version 0.3
 0.3.01   26.08.04  we          mp_error, most functions now procedures
 0.3.02   26.08.04  we          mp_progress, mp_set_progress
 0.3.03   16.02.05  we          Montgomery Reduction
 0.3.04   16.02.05  we          temp. $R- for mp_montgomery_setup (for 8-bit)
 0.3.05   17.02.05  we          general mp_exptmod_win, removed s_mp_exptmod
 0.3.06   20.02.05  we          mp_jacobi
 0.3.07   20.02.05  we          mp_jacobi: fix up D6/FPC warnings
 0.3.08   27.03.05  we          Montgomery routines to mp_core
 0.3.09   29.03.05  we          mp_jacobi: non-recursive version
 0.3.10   08.05.05  we          mp_jacobi: clean up, remove old version
 0.3.11   16.05.05  we          FPC: $mode objfpc/$goto on
 0.3.12   10.08.05  we          mp_lucasv.., debug check in mp_xgcd
 0.3.13   15.08.05  we          bugfix mp_lucasv2p if v same var as p or q
 0.3.13   15.08.05  we          mp_williams_pp1: William's p+1 method
 0.3.15   16.08.05  we          mp_coshmult compatible with Forster
 0.3.16   17.08.05  we          s_mp_coshmult easy out for b=1, loop error check
 0.3.17   18.08.05  we          mp_williams_pp1: numtest parameter

 0.4.00   20.08.05  we          use mp_set_error
 0.4.01   20.08.05  we          longint bit count in mp_exptmod_win
 0.4.02   21.08.05  we          MPC_ArgCheck
 0.4.03   22.08.05  we          removed mp_argcheck, mp_errchk
 0.4.04   22.08.05  we          now functions: mp_is_SPP, mp_is_square
 0.4.05   22.08.05  we          mp_fermat
 0.4.06   22.08.05  we          pollard, williams, coshmult with init<i>
 0.4.07   23.08.05  we          mp_mersenne, MPC_HaltOnArgCheck
 0.4.08   23.08.05  we          new values for MaxFermat, MaxMersenne
 0.4.09   23.08.05  we          IsPrime15 (quick and dirty prime test)
 0.4.10   23.08.05  we          mp_exptmod_d
 0.4.11   24.08.05  we          Fibonacci numbers, mp_fib, mp_fib2
 0.4.12   25.08.05  we          Lucas numbers, mp_lucas, mp_lucas2
 0.4.13   25.08.05  we          mp_fib2 for n=0
 0.4.14   26.08.05  we          miller_rabin: generate a in the range 2<=a<n-1
 0.4.15   27.08.05  we          easy outs in mp_small_factor
 0.4.16   28.08.05  we          usage of exceptions implemented
 0.4.17   10.09.05  we          function IsMersennePrime
 0.4.18   12.09.05  we          mp_fact, MaxFact
 0.4.19   12.09.05  we          optimized 3-level mp_fact, FSIVec
 0.4.20   13.09.05  we          mp_fact optimization (balanced factors)
 0.4.22   16.09.05  we          changed mp_gcd easy out, comment in mp_xgcd
 0.4.23   17.09.05  we          mp_ecm_brent (Brent's ECM factoring method)
 0.4.24   18.09.05  we          mp_ecpwr (faster version of mp_ecpwr2)
 0.4.25   20.09.05  we          mp_lucasv2p without goto, changed TProgressProc
 0.4.25   22.09.05  we          $argcheck for pointers (mp_lucasv2p, mp_xgcd)
 0.4.26   25.09.05  we          Bugfix: k now longint in IsMersennePrime
 0.4.27   26.09.05  we          $argcheck for identical args in mp_fib2,mp_lucas2

 0.5.00   29.09.05  we          'internal' functions moved to end of interface
 0.5.01   30.09.05  we          IsPrime16, pbits16, pmask16
 0.5.02   01.10.05  we          easy out for IsMersennePrime
 0.5.03   02.10.05  we          changed 'SPP' to more conventional 'spsp'
 0.5.04   03.10.05  we          function nextprime32
 0.5.05   03.10.05  we          function prevprime32
 0.5.06   09.10.05  we          next/prev prime residues classes module via $ifdef
 0.5.07   09.10.05  we          mp_is_spsp_d, mp_is_pprime, mp_small_factor with fmax
 0.5.08   09.10.05  we          small changes in mp_miller_rabin
 0.5.08   13.10.05  we          mp_is_slpsp
 0.5.09   16.10.05  we          mp_is_pprime with BPSW-pseudoprime
 0.5.10   14.11.05  we          mp_nextprime/mp_prevprime
 0.5.11   15.11.05  we          mp_rand_prime
 0.5.12   18.11.05  we          mp_rand_prime improved for safe primes
 0.5.13   20.11.05  we          TPrimeType = (pt_normal, pt_3mod4, pt_safe)
 0.5.14   20.11.05  we          mp_is_pprime_ex, mp_rand_prime with mp_is_pprime_ex
 0.5.15   21.11.05  we          mp_rand_prime with mp_rand_bits
 0.5.16   22.11.05  we          New name: mp_isMersennePrime, mp_prng removed from uses

 0.6.00   30.12.05  we          mp_count_bits/CountBits32 renamed to mp_bitsize/bitsize32
 0.6.01   30.12.05  we          MP_8BIT removed
 0.6.02   05.01.06  we          MaxMersenne = DIGIT_BIT*MAXDigits-1;
 0.6.03   10.01.06  we          mp_fact using Recursive Split

 0.7.00   28.04.06  we          mp_xgcd: u2,v2,t2 suppressed
 0.7.01   04.08.06  we          TRedType MR_Reduce2k, mp_reduce_2k in mp_exptmod_win
 0.7.03   05.08.06  we          mp_show_progress initialized with false
 0.7.04   08.08.06  we          mp_makeodd in mp_miller_rabin, mp_is_slpsp, mp_jacobi
 0.7.05   08.08.06  we          mp_sqrtmod initial version
 0.7.06   09.08.06  we          mp_sqrtmod arg/init checks
 0.7.07   09.08.06  we          mp_lucasvmod
 0.7.08   09.08.06  we          mp_sqrtmod18_lucas, TPrimeType with pt_1mod8
 0.7.09   10.08.06  we          mp_sqrtmodp2, mp_sqrtmethod
 0.7.10   11.08.06  we          mp_miller_rabin and mp_is_spsp modified
 0.7.11   11.08.06  we          Avoid FPC warnings: nextprime32/prevprime32
 0.7.12   11.08.06  we          experimental mp_sqrtmod18_fp
 0.7.13   11.08.06  we          check prime/q-residue after 100 trials in mp_sqrtmod18_shanks/fp
 0.7.14   15.08.06  we          small tweak in mp_isMersennePrime
 0.7.15   15.08.06  we          $ifdef MPC_sqrtmod_fp
 0.7.16   16.08.06  we          bigprime_pm1: big prime stage for Pollard p-1
 0.7.17   17.08.06  we          bigprime_pm1: Barret and bitsize dependent constants
 0.7.17   20.08.06  we          const mp_max_small / mp_small_factor
 0.7.18   21.08.06  we          mp_ecm_factor (first complete version using 7KB stack)
 0.7.19   22.08.06  we          mp_ecm_factor (dynamic vectors, progress, intermeditate GCDs)
 0.7.20   23.08.06  we          mp_ecm_factor,mp_ecm_brent: check n>1, @n<>@f
 0.7.21   23.08.06  we          mp_ppexpo with mp_mul_w
 0.7.22   23.08.06  we          mp_ecm_factor with Barrett: speed more than doubled
 0.7.23   24.08.06  we          mp_ecm_factor: check seed>5 in ecm_setup, more comments
 0.7.24   26.08.06  we          nextprime32_array
 0.7.25   27.08.06  we          Prime table in mp_ecm_factor $ifdef MPC_ECM_Primetable
 0.7.26   28.08.06  we          basic mp_is_power
 0.7.27   07.09.06  we          mp_is_square2
 0.7.28   07.09.06  we          nextprime32_array with parameter cmax
 0.7.29   07.09.06  we          mp_ecm_factor with parameters C1,phase; mp_ecm_simple
 0.7.30   07.09.06  we          function ProgressAssigned

 0.8.00   19.09.06  we          mp_primorial, MaxPrimorial
 0.8.01   19.09.06  we          mp_isMersennePrime: search small factor before Lucas/Lehmer
 0.8.02   23.09.06  we          MaxFibonacci, MaxLucas
 0.8.03   25.09.06  we          mp_kronecker, mp_jacobi uses kron_intern

 0.9.00   25.12.06  we          minor changes in mp_isMersennePrime
 0.9.01   28.12.06  we          Bugfix mp_is_power for a=-4
 0.9.02   30.12.06  we          FindFirst/NextPrime32, rewrite nextprime32_array
 0.9.03   01.01.07  we          bigalloc renamed to IAlloc
 0.9.04   01.01.07  we          mp_primorial with mp_prod_int
 0.9.05   01.03.07  we          mp_primorial with mp_primor_cutoff

 1.0.00   07.05.07  we          mp_exptmod returns 0 for c=1, error for c<=0
 1.0.01   08.05.07  we          Removed Fp[sqrt(D)] arithmetic for MPC_sqrtmod_fp
 1.0.02   09.05.07  we          Removed mp_ecpwr2
 1.0.03   13.05.07  we          Corrected some exception strings

 1.1.00   19.05.07  we          break mp_gcd loop if u=v
 1.1.01   22.05.07  we          Binary Kronecker symbol algorithm
 1.1.02   22.05.07  we          Binary Kronecker: skip operations after mp_sub
 1.1.03   23.05.07  we          Binary Kronecker: s_mp_sub, break if x=y
 1.1.04   31.05.07  we          mp_is_power: check mod 4 before mp_expt_int
 1.1.05   27.06.07  we          mp_gcd_initial_mod in mp_gcd
 1.1.06   01.07.07  we          removed useless first iteration in mp_fact
                                (thanks to Marcel Martin)
 1.1.07   02.07.07  we          mp_miller_rabin uses IsPrime16 if possible
 1.1.08   09.07.07  we          mp_gcd_initial_mod renamed to mp_initial_mod
                                and also used in kron_intern
 1.1.09   12.07.07  we          mp_pell and mp_pell4
 1.1.10   14.07.07  we          s_mp_lucasvmod1 (used by mp_is_slpsp)
 1.1.11   14.07.07  we          mp_sqrtmod14_mueller
 1.1.12   14.07.07  we          Mueller sqrtmod: Bugfix use 1/t mod p
 1.1.13   15.07.07  we          Mueller sqrtmod: case t=1 handled in Jacobi loop
 1.1.14   15.07.07  we          Mueller sqrtmod: don't compute PP in Jacobi loop
 1.1.15   21.07.07  we          mp_xgcd: easy out for trivial cases
 1.1.17   30.07.07  we          mp_sqrtmodpk
 1.1.17   31.07.07  we          mp_sqrtmod14_mueller for mp_sqrtmethod=3 and
                                in auto mode instead of Lucas
 1.1.18   01.08.07  we          fast_mp_invmod: no init checks, y removed
 1.1.19   04.08.07  we          mp_sqrtmod2k
 1.1.20   05.08.07  we          special cases k=1,2 of mp_sqrtmod2k
                                easy out in mp_exptmod if b=1
 1.1.21   05.08.07  we          mp_sqrtmod_ex with optional Jacobi check
 1.1.22   05.08.07  we          mp_sqrtmodpq, removed mp_invmod_euclid

 1.2.00   16.08.07  we          mp_lcm: special cases, divide larger arg by gcd
 1.2.01   17.08.07  we          mp_is_primepower
 1.2.02   21.08.07  we          jacobi32
 1.2.03   23.08.07  we          mp_jacobi_lm
 1.2.04   24.08.07  we          mp_jacobi_lm used in: mp_is_slpsp, mp_sqrtmod18_shanks_intern
 1.2.05   26.08.07  we          improved error handling in mp_sqrtmod18_* routines
 1.2.06   30.08.07  we          t=1 in mp_sqrtmod14_mueller, improved jacobi32
 1.2.07   02.09.07  we          updated mp_primor_cutoff values
 1.2.08   03.09.07  we          mp_OddProd, mp_dfact, mp_fact with local mp stack
 1.2.09   04.09.07  we          small tables for mp_lucas and mp_fib
 1.2.10   04.09.07  we          Bugfix mp_fib2 for n=k*$10000000, k=1..7
 1.2.11   06.09.07  we          mp_binomial, s_mp_binom_w
 1.2.12   07.09.07  we          mp_binomial for negative arguments
 1.2.13   08.09.07  we          bugfix mp_binomial for small k and n>65535
 1.2.14   08.09.07  we          s_mp_binom_l, case k=1 in mp_binomial
 1.2.15   09.09.07  we          mp_binomial: init X[s] when needed
 1.2.16   10.09.07  we          mp_sqrtmod916_kong
 1.2.17   10.09.07  we          avoid warning in mp_miller_rabin if MP_16BIT
 1.2.18   11.09.07  we          mp_jacobi_ml
 1.2.19   12.09.07  we          Bugfix mp_is_primepower for a=2^1
 1.2.20   17.09.07  we          mp_is_power uses residues mod 8

 1.3.00   26.10.07  we          s_mp_sqrtmod2k for odd a, mp_sqrtmod2k for a>=0
 1.3.01   12.11.07  we          Fix memory leak(s) if MPC_HaltOnError is not defined
 1.3.02   10.12.07  we          improved mp_is_square2

 1.5.00   13.01.08  we          mp_sqrtmod2k: bugfix due to error in Adler/Coury [26]

 1.6.00   24.05.08  we          mp_crt_solve, mp_crt_setup, mp_crt_single
 1.6.01   24.05.08  we          functions mp_invmodf, mp_crt_setupf
 1.6.02   04.06.08  we          mp_is_square2: psqrt^ := a if a=0 or 1
 1.6.03   04.06.08  we          mp_cornacchia
 1.6.04   05.06.08  we          mp_cornacchia4
 1.6.05   06.06.08  we          function mp_gcd1
 1.6.06   08.06.08  we          Cosmetic changes / corrected exception strings
 1.6.07   11.06.08  we          Arg check in mp_binomial

 1.7.00   27.07.08  we          Improved trial factors in mp_isMersennePrime
 1.7.01   17.09.08  we          use mp_is_longint after mp_initial_mod in mp_gcd
 1.7.02   17.09.08  we          mp_gcd_euclid with mp_mod, more uses of mp_is_longint
 1.7.03   20.09.08  we          mp_xgcd with Lehmer, mp_gcd_ml (modified Lehmer)
 1.7.04   21.09.08  we          $ifdef MPC_UseGCD32 in mp_gcd
 1.7.05   22.09.08  we          mp_xgcd_bin
 1.7.06   22.09.08  we          mp_invmodf via xgcd, removed fast_mp_invmod
 1.7.07   22.09.08  we          extended range for Lehmer routine
 1.7.08   23.09.08  we          improved inner loop of (binary) mp_gcd
 1.7.09   24.09.08  we          use mp_sign for trivial cases in mp_xgcd_bin
 1.7.10   24.09.08  we          mp_is_pprime_ex skips 1-digit test, fixed mp_is_square2
 1.7.11   24.09.08  we          mp_invmodf with invmod32
 1.7.12   02.10.08  we          bugfix in mp_gcd $ifdef MPC_UseGCD32

 1.8.00   09.10.08  we          use mp_set1; mp_is_longint in mp_miller_rabin
 1.8.01   21.10.08  we          exptmod32, s_mp_is_pth_power, improved mp_is_power
 1.8.02   22.10.08  we          changed mp_is_primepower
 1.8.03   25.10.08  we          mp_is_power with s_mp_n_root2
 1.8.04   26.10.08  we          Check a mod k^3 and a mod q^2 in s_mp_is_pth_power
 1.8.05   27.10.08  we          improved mp_is_power
 1.8.06   01.11.08  we          mp_xlcm, mp_rnr
 1.8.07   02.11.08  we          sieve test loop in s_mp_is_pth_power, new: mp_is_pth_power

 1.9.00   06.12.08  we          mp_rnr2
 1.9.01   23.12.08  we          s_mp_mca_alg1816
 1.9.02   25.12.08  we          improved mp_small_factor
 1.9.03   29.12.08  we          uses mp_prime
 1.9.04   03.01.09  we          mp_is_square2, s_mp_pell4 with s_mp_sqrtrem
 1.9.05   04.01.09  we          mp_is_square2: s_mp_sqrtrem only (removed trick branch)
                                removed mp_ecm_brent, mp_ecpwr
 1.9.06   04.01.09  we          32 bit first/next prime routines to mp_prime
 1.9.07   06.01.09  we          minor changes in mp_isMersennePrime
 1.9.08   06.01.09  we          explicit range check in mp_fermat

 1.10.00  21.01.09  we          changes related to (s)mp_divrem
 1.10.01  22.01.09  we          mp_cbrtmod
 1.10.02  25.01.09  we          s_mp_is_pprime_ex
 1.10.03  28.01.09  we          improved mp_is_square2
 1.10.04  02.02.09  we          mp_sqrtmodpk: check invmod, store result only if Err=0
 1.10.05  03.02.09  we          s_mp_sqrtmodpk with red parameter, mp_sqrtmodpk with red=true
 1.10.06  03.02.09  we          mp_cbrtmodpk, s_mp_cbrtmodpk, mp_cbrtmod3k
 1.10.07  04.02.09  we          sqrt/cbrtmodpk functions: handle a mod p = 0
 1.10.08  04.02.09  we          mp_sqrtmodpk handles p=2
 1.10.09  07.02.09  we          mp_cbrtmodpq
 1.10.10  07.02.09  we          s_mp_is_cubres
 1.10.11  08.02.09  we          mp_cbrtmod_ex
 1.10.12  12.02.09  we          improved mp_cbrtmod_ex
 1.10.13  19.02.09  we          improved s_mp_is_cubres
 1.10.14  27.02.09  we          mp_isMersennePrime: check for spurious factor

 1.12.00  20.06.09  we          mp_is_power_max
 1.12.01  21.06.09  we          mp_provable_prime
 1.12.02  05.07.09  we          new: s_mp_npfirst, s_mp_npnext, updated: mp_nextprime, s_mp_is_pprime_ex
 1.12.04  29.07.09  we          Increased trace level in mp_provable_prime

 1.13.00  15.08.09  we          mp_crt_single/solve: improved and check n>0
 1.13.01  23.08.09  we          allow p=q in mp_sqrtmodpq/mp_cbrtmodpq

 1.16.00  04.06.10  we          mp_4sq, mp_4sq_sa, mp_4sq_sd

 1.17.00  30.12.10  we          mp_binomial for k < 0

**************************************************************************)

(*-------------------------------------------------------------------------
  This code uses material/ideas from the following 3rd party libraries:
   - LibTomMath 0.30+ by Tom St Denis
   - MPI 1.8.6 by Michael J. Fromberger
   - NX V0.18 and V0.9+ by Marcel Martin
  See the file '3rdparty.mpa' for the licenses.
----------------------------------------------------------------------------*)


(*-------------------------------------------------------------------------
 (C) Copyright 2004-2010 Wolfgang Ehrhardt

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

const
  mp_max_small = mp_digit(MP_DIGIT_MAX and $7FFF); {max. tested small factor}

type
  TProgressProc = procedure(checkonly: boolean; cnt,maxcnt: longint; var abort: boolean);
                    {procedure to show progress in factorization routines.}
                    {If on return abort is true, factorization is aborted.}
                    {checkonly: check only for abort, not recommended to show progress,}
                    {cnt, maxcnt: current and maximum iteration cnt, maxcnt=0; no maximum known,}
                    {abort: set to true to abort calculation, proc is called with abort=false}

var
  MaxFact: word;    {MaxFact depends on DIGIT_BIT and MAXDigits. It  }
                    {is an approx. value for max. valid mp_fact args.}
                    {Since no checks are performed some larger values}
                    {might work. With 16 bit real mode even smaller  }
                    {values may result in "Out of memory" errors.    }

var
  mp_show_progress: boolean;  {Factorization routines rho, (p+1), (p-1) call}
                              {progress procedure after one accum cycle}

var
  mp_initial_mod  : boolean;  {Single initial modular reduction in mp_gcd}
                              {and in mp_kronecker/mp_jacobi}

var
  mp_max_small_sqr: longint;  {square of max small factor, ie numbers less}
                              {are prime if mp_small_factor returns f=0}

var
  mp_sqrtmethod   : integer;  {Selects algorithm for sqrt mod p, p=1 mod 8.}
                              {0:Auto, 1:Shanks, 2:Lucas, 3:Mueller.}
                              {Default=0: auto select between Shanks/Mueller.}

var
  mp_primor_cutoff: longint;  {Cutoff point for recursive product in mp_primorial,}
                              {initialization will set rough value. For better}
                              {results use t_tunepr}

type
  TPrimeType    = (pt_normal, pt_3mod4, pt_safe, pt_1mod8);
                              {pt_normal: prime without special features.}
                              { pt_3mod4: prime p with p modulo 4 = 3.   }
                              { pt_1mod8: prime p with p modulo 8 = 1.   }
                              {  pt_safe: safe prime, ie p=2q+1, q prime.}

const
  MaxFermat    = 18;          {Maximum Fermat number with this implementation}
  MaxMersenne  = DIGIT_BIT*MAXDigits-1;      {Approx. maximum Mersenne number}
  MaxPrimorial = trunc(MaxMersenne*0.693);   {Approx. maximum arg for mp_primorial, Note: for real}
                                             {mode "out of memory" will occur for smaller values!}
  MaxFibonacci = trunc(MaxMersenne*1.44);    {Approx. maximum arg for mp_fib}
  MaxLucas     = MaxFibonacci-2;             {Approx. maximum arg for mp_lucas}

const
  ECM_C1Min = 400;        {Minimum and maximum of C1 in mp_ecm_factor  }
  ECM_C1Max = 4000;       {Phase #1 makes use of primes up to Prime[C1]}


function  exptmod32(a,b,c: longint): longint;
  {-calculate a^b mod c if a>=0, b>=0, c>0; result=0 otherwise}

function  jacobi32(a,b: longint): longint;
  {-Compute the Jacobi/Legendre symbol (a|b), b: odd and > 2}

procedure mp_4sq(const m: mp_int; var a,b,c,d: mp_int);
  {-Decompose m>=0 into 4 squares: m = a^2 + b^2 + c^2 + d^2.}
  { If no solutions for m>0 was found, a=b=c=d=0 is returned.}

procedure mp_4sq_sa(const m: mp_int; var a,b,c,d: mp_int);
  {-Decompose m>=0 into 4 squares: m = a^2 + b^2 + c^2 + d^2, a<=b<=c<=d.}
  { If no solutions for m>0 was found, a=b=c=d=0 is returned.}

procedure mp_4sq_sd(const m: mp_int; var a,b,c,d: mp_int);
  {-Decompose m>=0 into 4 squares: m = a^2 + b^2 + c^2 + d^2, a>=b>=c>=d.}
  { If no solutions for m>0 was found, a=b=c=d=0 is returned.}

procedure mp_addmod(const a,b,c: mp_int; var d: mp_int);
  {-calculate d = a + b (mod c)}

procedure mp_binomial(n,k: longint; var a: mp_int);
  {-calculate the binomial coefficient a = (n choose k)}

procedure mp_cbrtmod(const a, p: mp_int; var x: mp_int; var Err: integer);
  {-Compute a cube root x of a with x^3 = a mod p, p prime.  }
  { Err in [1..3], if a is not a 3rd power or p is not prime.}

procedure mp_cbrtmod3k(const a: mp_int; k: word; var b: mp_int; var Err: integer);
  {-Compute a cube root b of a with b^3 = a mod 3^k. Error codes: 1<Err<4 code from}
  { mp_cbrtmod, Err=4 if p=3, Err=5: no inverse mod 3^(2^i), Err=6: (internal d<>0)}

procedure mp_cbrtmodpk(const a,p: mp_int; k: word; var b: mp_int; var Err: integer);
  {-Compute a cube root b of a with b^3 = a mod p^k, p prime. b will be  }
  { reduced mod p^k. Error codes: 1<Err<4: from mp_cbrtmod, Err=4 if p=3,}
  { Err=5: no inverse mod p^(2^i), Err=6: internal check for p=3 failed. }

procedure mp_cbrtmodpq(const a,p,q: mp_int; var x: mp_int; var Err: integer);
  {-Compute a cube root x of a mod (pq); p,q primes. If p=q, mp_cbrtmodpk is}
  { used. For p<>q: Err=8 if gcd(p,q)<>1, otherwise error code from mp_cbrtmod.}

procedure mp_cbrtmod_ex(const a, p: mp_int; var x: mp_int; pru: pmp_int; var Err: integer);
  {-Compute a cube root x of a with x^3 = a mod p, p prime. If pru<>nil, pru^ }
  { is set to a 3rd root of unity (or 1 if x is unique), the other roots are  }
  { x*pru^, x*pru^*pru^. Err <> 0, if a is not a 3rd power or p is not prime. }

procedure mp_cornacchia(const d,p: mp_int; var x,y: mp_int; var Err: integer);
  {-Solve x^2 + d*y^2 = p, p prime, 0<d<p with Cornacchia's algorithm. Check}
  { @x<>@y, but not p prime. Err<>0 if no solution exist. x >= y if d=1.}

procedure mp_cornacchia4(const d,p: mp_int; var x,y: mp_int; var Err: integer);
  {-Solve x^2 + |d|*y^2 = 4p, p prime, -4p<d<0 with the modified Cornacchia}
  { algorithm. Check @x<>@y, but not p prime. Err<>0 if no solution exist.}

procedure mp_crt_setup(n: integer; const m: array of mp_int; var c: array of mp_int);
  {-Calculate CRT coefficients c[i] for pairwise co-prime moduli m[i], i=0..n-1.}

function  mp_crt_setupf(n: integer; const m: array of mp_int; var c: array of mp_int): boolean;
  {-Calculate CRT coefficients c[i] for pairwise co-prime moduli m[i], i=0..n-1.}
  { Return true if c[i] are successfully calculated.}

procedure mp_crt_single(n: integer; const m,v: array of mp_int; var x: mp_int);
  {-Calculate x with x mod m[i] = v[i], pairwise co-prime moduli m[i], i=0..n-1.}
  { Use mp_crt_setup/calc if more than one system shall be solved}

procedure mp_crt_solve(n: integer; const m,c,v: array of mp_int; var x: mp_int);
  {-Calculate x with x mod m[i] = v[i], pairwise co-prime moduli m[i], i=0..n-1.}
  { Coefficients c[i] must be precalculated with mp_crt_setup}

procedure mp_dfact(n: word; var a: mp_int);
  {-calculate double factorial a = n!!, a=n*(n-2)..., lowest term 1 or 2}

procedure mp_ecm_factor(const n: mp_int; var f: mp_int; CMax, C1: word; var seed,phase: longint);
  {-Find a factor f of n by inversionless ECM method, f=0 if no factor found.}
  { n must be > 1 and @f<>@n otherwise a runtime error / exception is raised.}
  { Two-phase elliptic curve algorithm, C1 primes are used in phase 1, C1 is}
  { clipped to [ECM_C1Min,ECM_C1Max]. Up to CMax curves (32 if CMax=0), if}
  { input seed=0, mp_random_int is used as initial value. The code is based}
  { on Marcel Martin's NX v0.18/FPC implementation of Algorithm 7.4.4 from}
  { Crandall/Pomerance, incorporating various enhancements of Brent,}
  { Crandall, Montgomery, Woltman, and Zimmermann. WE changes: with Barrett}
  { reduction and intermediate GCDs, 32 bit prime table via config.}

procedure mp_ecm_simple(const n: mp_int; var f: mp_int; seed: longint);
  {-Simplified version of mp_ecm_factor with CMax=0, C1=ECM_C1Min}

procedure mp_exptmod(const a,b,c: mp_int; var d: mp_int);
  {-Compute d = a^b mod c, c>0. If b<0, a must have an inverse mod c}

procedure mp_exptmod_d(const a: mp_int; b: longint; const c: mp_int; var d: mp_int);
  {-Compute d = a^b mod c, c>0, b longint. If b<0, a must have an inverse mod c}

procedure mp_fact(n: word; var a: mp_int);
  {-calculate a = factorial(n) using Recursive Split, error if n > MaxFact}

procedure mp_fermat(n: word; var fn: mp_int);
  {-return nth Fermat number, fn = 2^(2^n)+1 (MP_RANGE error for n>MaxFermat)}

procedure mp_fib(n: longint; var fn: mp_int);
  {-calculate Fibonacci number fn=fib(n), fib(-n)=(-1)^(n-1)*fib(n)}

procedure mp_fib2(n: longint; var fn,f1: mp_int);
  {-calculate two Fibonacci numbers fn=fib(n), f1=fib(n-1), n>=0}

procedure mp_gcd(const a,b: mp_int; var c: mp_int);
  {-calculate c = gcd(a,b) using the binary method}

function  mp_gcd1(const a,b: mp_int; var c: mp_int): boolean;
  {-calculate c = gcd(a,b) using the binary method, return true if c=1 and no error}

procedure mp_gcd_euclid(const a,b: mp_int; var c: mp_int);
  {-calculate c = gcd(a,b), non optimized Euclid}

procedure mp_gcd_ml(const a,b: mp_int; var u: mp_int);
  {-calculate u = gcd(a,b) using the Sorenson's Modified Lehmer method}

procedure mp_invmod(const a, b: mp_int; var c: mp_int);
  {-compute c = a^-1 (mod b), b>0, via mp_xgcd, MP_UNDEF error if there is no inverse}

function  mp_invmodf(const a, b: mp_int; var c: mp_int): boolean;
  {-compute c = a^-1 (mod b), b>0, via mp_xgcd, return true if inverse exists}

function  mp_isMersennePrime(p: longint): boolean;
  {-Lucas-Lehmer test for Mersenne number m=2^p-1, HAC 4.37}

procedure mp_is_power(const a: mp_int; var b: mp_int; var p: longint);
  {-Calculate smallest prime p with a=b^p; p=1,b=a if a is no power}

procedure mp_is_power_max(const a: mp_int; var b: mp_int; var k: longint);
  {-Calculate largest k with a=b^k; k=1,b=a if a is no power}

function  mp_is_pprime(const a: mp_int): boolean;
  {-Test if a is prime (BPSW pseudo prime if a>2^32)}

function  mp_is_pprime_ex(const a: mp_int; smax: mp_digit): boolean;
  {-Test if a is prime (BPSW pseudo prime if a>2^32); trial division up to smax}

function  mp_is_primepower(const a: mp_int; var b: mp_int; var k: longint): boolean;
   {-return true if a=b^k, b prime, k>1, otherwise false and a=b^k, k=1 if no power}

function  mp_is_pth_power(const a: mp_int; p: longint; var r: mp_int): boolean;
  {-return true if a is pth power, a>0, p prime. If true, calculate r with a=r^p}

function  mp_is_slpsp(const a: mp_int): boolean;
  {-strong Lucas pseudo prime test for a. Lucas test is }
  { done for the first p=2k+1 with mp_jacobi(p^2-4,a)=-1}

function  mp_is_spsp(const n,a: mp_int): boolean;
  {-strong pseudo prime test of n to base of a>1 from HAC p. 139 Alg.4.24}

function  mp_is_spsp_d(const n: mp_int; a: mp_digit): boolean;
  {-strong pseudo prime test of n to mp_digit base of a>1 from HAC p. 139 Alg.4.24}

function  mp_is_square(const a: mp_int): boolean;
  {-test if a is square}

function  mp_is_square2(const a: mp_int; psqrt: pmp_int): boolean;
  {-test if a is square, return sqrt(a) if a is a square and psqrt<>nil}

function  mp_jacobi(const a,n: mp_int): integer;
  {-Compute the Jacobi/Legendre symbol (a|n), n: odd and > 2}

function  mp_jacobi_lm(a: longint; const n: mp_int): longint;
  {-Compute the Jacobi/Legendre symbol (a|n), n: odd and > 2}

function  mp_jacobi_ml(const a: mp_int; n: longint): longint;
  {-Compute the Jacobi/Legendre symbol (a|n), n: odd and > 2}

function  mp_kronecker(const a,n: mp_int): integer;
  {-Compute the Kronecker symbol (a|n)}

procedure mp_lcm(const a,b: mp_int; var c: mp_int);
  {-Compute least common multiple as |a*b|/(a, b)}

procedure mp_lucas(k: longint; var lk: mp_int);
  {-calculate Lucas number lk=luc(k), luc(-k)=(-1)^k*luc(k)}

procedure mp_lucas2(k: longint; var lk,l1: mp_int);
  {-calculate two Lucas numbers lk=luc(k), l1=luc(k-1), k>=0}

procedure mp_lucasv(const p,q: mp_int; k: longint; var v: mp_int);
  {-calculate v[k] of Lucas V sequence for p,q, p^2-4q <>0}

procedure mp_lucasvmod(const p,q,n,k: mp_int; var vk: mp_int);
  {-calculate v[k] mod n of Lucas V sequence for p,q.}
  { Ranges n>1, 0 <= p,q,k < n (checked if MPC_ArgCheck).}
  { Note: p^2-4q<>0 is not tested, no proper Lucas sequence!!}

procedure mp_lucasv2(const p,q: mp_int; k: longint; var v,w: mp_int);
  {-calculate v=v[k],w=v[k+1] of Lucas V sequence for p,q, p^2-4q<>0}

procedure mp_lucasv2p(const p,q: mp_int; k: longint; var vk: mp_int; pvk1: pmp_int);
  {-calculate v[k] of Lucas V sequence for p,q; p^2-4q<>0}
  { if pvk1<>nil, pvk1^ will be set to v[k+1]}

procedure mp_mersenne(n: longint; var mn: mp_int);
  {-return nth Mersenne number, mn = 2^n-1, MP_RANGE err for n>MaxMersenne}

procedure mp_miller_rabin(const n: mp_int; t: word; var prime: boolean);
  {-Miller-Rabin test of n, security parameter t, from HAC p. 139 Alg.4.24}
  { if t<=0, calculate t from bit_count with prob of failure < 2^-96}

procedure mp_mulmod(const a,b,c: mp_int; var d: mp_int);
  {-calculate d = a * b (mod c)}

procedure mp_nextprime(var n: mp_int);
  {-next prime >= n, 2 if n<=2}

procedure mp_OddProd(a,b: word; var p: mp_int);
  {-calculate p=prod(2*i+1),i=a+1...b;  p=1 if a>=b}

procedure mp_pell(const d: mp_int; var x,y: mp_int);
  {-Calculate the smallest non-trivial solution of the Pell equation}
  { x^2 - d*y^2 = 1; error if d<2, if d is a square, or if @x = @y.}

procedure mp_pell4(const d: mp_int; var x,y: mp_int; var r: integer);
  {-Calculate the smallest non-trivial solution of  x^2 - d*y^2 = r,}
  { r in [-4,+4,-1,+1]. Uses continued fraction expansion of sqrt(d)}
  { from Forster [4]. Error if d<2, if d is a square, or if @x = @y.}

procedure mp_pollard_pm1(const N: mp_int; var f: mp_int; bound: word);
  {-find a factor f of N with p-1 method, f=0 if no factor found}
  { primes <= bound will be included in pp_expo product}

procedure mp_pollard_rho(const N: mp_int; var f: mp_int; cnt: longint);
  {-find a factor f of N, f=0 if no factor found; cnt: number accumulation cycles}

procedure mp_prevprime(var n: mp_int);
  {-previous prime <= n, 0 if n<2}

procedure mp_primorial(n: longint; var a: mp_int);
  {-Primorial of n;  a = n# = product of primes <= n.}

procedure mp_provable_prime(bits: longint; var p: mp_int);
  {-Generate a random provable prime p with bitsize bits using Maurer's algorithm}

procedure mp_rand_prime(bitsize: word; pt: TPrimeType; var p: mp_int);
  {-generate random (BPSW pseudo) prime of bitsize > 3, pt: prime type of p}

procedure mp_rnr(const a,m: mp_int; var x,y: mp_int);
  {-rational number reconstruction: for m>0 calculate x,y with a*x=y mod m,}
  { gcd(x,y)=1, 0<=x,|y|<sqrt(m/2), x<>0. x=y=0 if no (unique) solution exists.}

procedure mp_rnr2(const a,m,NN,DD: mp_int; var n,d: mp_int);
  {-rational number reconstruction: for m,NN,DD > 0 calculate co-prime d,n}
  { with a*d=n mod m, |n|<=N, 0<d<=DD, i.e. a=n/d mod m. Return d=n=0 if }
  { no solution exists. The reconstruction is unique if 2*NN*DD < m.}

procedure mp_set_progress(const PP: TProgressProc);
  {-make PP the new progress proc}

procedure mp_small_factor(const a: mp_int; f0,fmax: mp_digit; var f: mp_digit);
  {-compute small digit prime factor or 0, f0..fmax, f will be <= min(fmax,$7FFF)}

procedure mp_sqrmod(const a,c: mp_int; var d: mp_int);
  {-calculate d = a * a (mod c)}

procedure mp_sqrtmod(const a,p: mp_int; var b: mp_int; var Err: integer);
  {-calculate square root b of a with b*b = a mod p, p prime, with Jacobi check.}
  { Err=-1 if (a|p)<>1, Err=1 if failure for p=1 mod 8, Err=2 if p even and <>2}

procedure mp_sqrtmod_ex(const a,p: mp_int; ChkJ: boolean; var b: mp_int; var Err: integer);
  {-calculate square root b of a with b*b = a mod p, p prime.}
  { If ChkJ=true then check Jacobi symbol (a|p)=1, Err=-1 if check fails.}
  { Err=1 if failure for p=1 mod 8, Err=2 if p even and not 2.}

procedure mp_sqrtmod2k(const a: mp_int; k: word; var b: mp_int; var Err: integer);
  {-calculate a square root b of an integer a with b*b = a mod 2^k.}
  { Return Err=1 if a<0;  Err=-1 if there is no solution. For odd a}
  { the solutions are normalized to 0 < b < 2^k/4 for k>3.}

procedure mp_sqrtmodp2(const a,p: mp_int; var b: mp_int; var Err: integer);
  {-calculate square root b of a with b*b = a mod p^2, p prime.}
  { Alias for mp_sqrtmodpk(,,2,,). Err: error code from mp_sqrtmodpk.}

procedure mp_sqrtmodpk(const a,p: mp_int; k: word; var b: mp_int; var Err: integer);
  {-calculate square root b < p^k of a with b*b = a mod p^k, p prime.  }
  { Error codes: if p=2: Err=1 if a<0;  Err=-1 if there is no solution }
  { if p<>2: Err=-1 if (a|p)<>1, Err=1: failure for p=1 mod 8, Err=2 if}
  {          p is even, Err=4: no inverse mod p^(2^i)}

procedure mp_sqrtmodpq(const a,p,q: mp_int; var x,y: mp_int; var Err: integer);
  {-calculate square roots +x,-x,+y,-y of a mod (pq); p,q primes}
  { If p=q, x is the root from mp_sqrtmodp2 and y is not computed.}
  { For p<>q: Err=4 if gcd(p,q)<>1, otherwise error code from mp_sqrtmod}

procedure mp_submod(const a,b,c: mp_int; var d: mp_int);
  {-calculate d = a - b (mod c)}

procedure mp_williams_pp1(const N: mp_int; var f: mp_int; bound,numtest: word);
  {-find a factor f of N with William's p+1 method, f=0 if no success. numtest}
  { random seeds will be tried, should be about 3 or 4, if 0 then 3 is used}
  { primes <= bound will be included in pp_expo product}

procedure mp_xgcd(const a,b: mp_int; p1,p2,p3: pmp_int);
  {-extended gcd algorithm, calculate  a*p1^ + b*p2^ = p3^ = gcd(a,b)}
  { p1,p2,p3 may be nil if the values are not required}

procedure mp_xgcd_bin(const a,b: mp_int; p1,p2,p3: pmp_int);
  {-extended binary gcd, calculate a*p1^ + b*p2^ = p3^ = gcd(a,b)  }
  { p1,p2,p3 may be nil if the values are not needed. Note that p1^}
  { and p2^ are NOT unique and may differ from those of mp_xgcd!!  }

procedure mp_xlcm(const a,b: mp_int; var c,x,y: mp_int);
  {-calculate c,x,y with lcm(a,b)=c=x*y and x|a, y|b, gcd(x,y)=1.}
  { c,x,y should be different variables, otherwise result may be inconsistent}

{#Z+}
{---------------------------------------------------------------------------}
{- 'Internal' functions, don't use them unless you know what you are doing -}
{---------------------------------------------------------------------------}
{#Z-}

procedure mp_coshmult(const a,b,c: mp_int; var d: mp_int);
  {-Forster's coshmult function, d=mod_coshmult(a,b,c), a>=0, b>=0, c>0}

function  mp_sqrtmod14_mueller(const a,p: mp_int; var b: mp_int): boolean;
  {-calculate b with b*b = a mod p, p prime = 1 mod 4, return success status.}
  { *internal* assumes a>1,p>1, no argument checks}

function  mp_sqrtmod18_shanks_intern(const a,p: mp_int; var b,q: mp_int; k: longint): boolean;
  {-calculate b with b*b = a mod p, p prime = 1 mod 8, return success status.}
  { *internal* q and k must be setup with p-1 = 2^k*q}

function  mp_sqrtmod18_shanks(const a,p: mp_int; var b: mp_int): boolean;
  {-calculate b with b*b = a mod p, p prime = 1 mod 8, return success status; *internal*}

function  mp_sqrtmod18_lucas(const a,p: mp_int; var b: mp_int): boolean;
  {-calculate b with b*b = a mod p, p prime = 1 mod 8, return success status}
  { *internal* assumes a,p>1, no arg check}

procedure mp_sqrtmod34(const g,p: mp_int; var z: mp_int);
  {-calculate z with z*z = g mod p, p prime > 0, p mod 4 = 3; *internal*}

procedure mp_sqrtmod58(const g,p: mp_int; var z: mp_int);
  {-calculate z with z*z = g mod p, p prime = 5 mod 8; *internal*}

function  mp_sqrtmod916_kong(const a,p: mp_int; var b: mp_int): boolean;
  {-calculate b with b*b = a mod p, p prime = 9 mod 16, return success status}
  { *internal* assumes a,p>1, no arg check}

procedure s_mp_binom_l(n: longint; k: word; var a: mp_int);
  {-internal binomial coefficient for small k, no init check}

procedure s_mp_cbrtmodpk(const a,p: mp_int; k: word; red: boolean; var b: mp_int; var Err: integer);
  {-Compute a cube root b of a with b^3 = a mod p^k, p prime <> 3.}
  { If red=true, reduce b mod p^k, otherwise b may be >= p^k. Error codes:}
  { 1<Err<4: from mp_cbrtmod, Err=4 if p=3, Err=5: no inverse mod p^(2^i) }

function  s_mp_is_cubres(const a, p: mp_int): boolean;
  {-Simple test if a is a cubic residue mod p, p prime. Primality of p}
  { is not checked, but some trivial non-prime cases are detected.}

function  s_mp_is_pprime_ex(const a: mp_int; smin,smax: mp_digit): boolean;
  {-Test if a is prime (BPSW pseudo prime if a>2^32); trial division from}
  { smin to smax; no init check, no check if a is less than 2<31}

function  s_mp_is_pth_power(const a: mp_int; p: longint; var r: mp_int): boolean;
  {-return true if a is pth power, then a=r^p. a>0, p>2 prime, no checks}

procedure s_mp_lucasvmod1(const p,n,k,mu: mp_int; var v: mp_int);
  {-calculate v=v[k] mod n of Lucas V sequence for p,q=1. mu: Barrett parameter}
  { for n, k>0. Internal use: no init check, assumes p<n, return v=2 for k<=0.}

procedure s_mp_mca_alg1816(const n,L: mp_int; k:integer; var p: mp_int; var fail: boolean);
  {-Try to find a factor p of a squarefree odd integer n>5, with L a multiple}
  { of lambda(n) [lambda: Carmichael function] and a confidence parameter k.}
  { Fail=true, if no success (e.g. n is prime), n is even, n<=5, or L is odd.}

procedure s_mp_npfirst(var a: mp_int; var idx: integer);
  {-Setup idx and increment a to first prime candidate, no init check, a>7}

procedure s_mp_npnext(var a: mp_int; var idx: integer);
  {-Update idx and increment a to next prime candidate, no init check, a>7}

procedure s_mp_sqrtmod2k(const a: mp_int; k: word; var b: mp_int; var Err: integer);
  {-calculate unique square root b of an odd integer a with b*b = a mod 2^k and }
  { 0<b<2^k/4 for k<3. Err=1 if a<0; =2 for even a; =3 if a<>1 mod min(2^k,8)}

procedure s_mp_sqrtmodpk(const a,p: mp_int; k: word; red: boolean; var b: mp_int; var Err: integer);
  {-calculate square root b of a with b*b = a mod p^k, p odd prime.}
  { Err=-1 if (a|p)<>1, Err=1 if failure for p=1 mod 8, Err=2 if p even.}
  { Err=4 if no inverse mod p^(2^i). If red=true, reduce b mod p^k, otherwise}
  { b may be >= p^k; ex: a=22,p=3,k=3 --> b=34 and 34^2 = 22 mod 27}


{---------------------------------------------------------------------------}
implementation
{---------------------------------------------------------------------------}


uses
  mp_base, mp_prime, mp_prng;

type
  TRedType = (MR_Barret, MR_Montgomery
             {$ifdef MPC_Reduce_2k} ,MR_Reduce2k {$endif}); {supported reduction types for exptmod}

var
  mp_progress: TProgressProc;  {Progress function for factoring procedures}


{---------------------------------------------------------------------------}
function ProgressAssigned: boolean;
  {-Check if progress is <> nil}
begin
  {$ifdef FPC}
    {$ifdef FPC_DELPHI}
      ProgressAssigned := @mp_progress<>nil;
    {$else}
      ProgressAssigned := mp_progress<>nil;
    {$endif}
  {$else}
    ProgressAssigned := @mp_progress<>nil;
  {$endif}
end;


{---------------------------------------------------------------------------}
function exptmod32(a,b,c: longint): longint;
  {-calculate a^b mod c if a>=0, b>=0, c>0; result=0 otherwise}
begin
  exptmod32 := 0;
  if (c<=0) or (a<=0) or (b<0) then exit;
  if a>=c then begin
    a := a mod c; if a=0 then exit;
  end;
  if b<2 then begin
    if b=0 then exptmod32 := 1 else exptmod32 := a;
    exit;
  end;
  {$ifdef BIT32}
    asm
           push  edi
           push  esi
           push  ebx
           mov   edi,$40000000
           mov   ebx,[b]
      @@1: test  edi,ebx
           jnz   @@2
           shr   edi,1
           jmp   @@1
      @@2: shr   edi,1
           mov   esi,[a]
           mov   ecx,[c]
           mov   edx,esi
      @@3: mov   eax,edx
           mul   edx
           div   ecx
           test  edi,ebx
           jz    @@4
           mov   eax,esi
           mul   edx
           div   ecx
      @@4: shr   edi,1
           jnz   @@3
         {$ifdef FPC}
           mov   [a],edx
         {$else}
           mov   [@result],edx
         {$endif}
           pop   ebx
           pop   esi
           pop   edi
    end;
  {$else}
    asm
                   mov  di,$4000
           db $66; shl  di,16
           db $66; mov  bx,word ptr [b]
      @@1: db $66; test di,bx
           db $66; jnz  @@2
           db $66; shr  di,1
                   jmp  @@1
      @@2: db $66; shr  di,1
           db $66; mov  si,word ptr [a]
           db $66; mov  cx,word ptr [c]
           db $66; mov  dx,si
      @@3: db $66; mov  ax,dx
           db $66; mul  dx
           db $66; div  cx
           db $66; test di,bx
                   jz   @@4
           db $66; mov  ax,si
           db $66; mul  dx
           db $66; div  cx
      @@4: db $66; shr  di,1
                   jnz  @@3
           db $66; mov  word ptr [@result],dx
    end;
  {$endif}
  {$ifdef FPC}
    exptmod32 := a;
  {$endif}
end;


{---------------------------------------------------------------------------}
function jacobi32(a,b: longint): longint;
  {-Compute the Jacobi/Legendre symbol (a|b), b: odd and > 2}
var
  j,m8: integer;
  t: longint;
begin
  {This is the classic method using only mod see e.g. [10] Alg. 2.3.5.}
  {Binary Pascal algorithm as used in kron_intern is NOT faster.}

  jacobi32 := 0;
  if (b<3) or (b and 1 = 0) then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('jacobi32: b<3 or even');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;

  {initialize accumulated result}
  j := 1;

  {if a<0 use property of Jacobi symbol to make it positive. Don't}
  {rely on Pascal mod function to return positive result.}
  if a<0 then begin
    {(-a|b) = (a|b)*(-1|b); change sign if b=3 (mod 4)}
    if b and 3 = 3 then j := -j;
    a := -a;
  end;

  {do initial reduction to force a < b}
  if a>=b then a := a mod b;

  {loop invariant: a < b}
  while a<>0 do begin
    {divide out powers of 4}
    while a and 3 = 0 do a := a shr 2;
    {if a is even, divide by 2 and adjust result}
    if a and 1 = 0 then begin
      a  := a shr 1;
      m8 := b and 7;
      { (2|b) = -1  if b = +-3 (mod 8)}
      if (m8=3) or (m8=5) then j:=-j;
    end;
    if a=1 then begin
      jacobi32 := j;
      exit;
    end;
    {swap variables, reduce and adjust result using quadratic reciprocity}
    if (a and b and 3) = 3 then j := -j;
    t := b;
    b := a;
    a := t mod a;
  end;
  if b=1 then jacobi32 := j;
end;


{---------------------------------------------------------------------------}
procedure s_mp_npfirst(var a: mp_int; var idx: integer);
  {-Setup idx and increment a to first prime candidate, no init check, a>7}
var
  k: integer;
  m: mp_digit;
begin
  idx := -1;
  {make n odd}
  if mp_iseven(a) then mp_inc(a);
  mp_mod_d(a, NPRC_MOD, m);
  if mp_error<>MP_OKAY then exit;
  m := m shr 1;
  {move a to next prime residue class mod MP_MOD and index idx into diff array}
  for k:=m to (NPRC_MOD div 2)-1 do begin
    idx := NPRC_OddIdx[k];
    {note: loop terminates via break because NPRC_OddIdx[(NPRC_MOD div 2)-1]<>-1}
    if idx<>-1 then break;
    mp_add_d(a,2,a);
  end;
end;


{---------------------------------------------------------------------------}
procedure s_mp_npnext(var a: mp_int; var idx: integer);
  {-Update idx and increment a to next prime candidate, no init check, a>7}
begin
  if mp_error<>MP_OKAY then exit;
  {move to next candidate}
  mp_add_d(a,NPRC_Diff[idx],a);
  {get next increment index}
  inc(idx);
  if idx>=NPRC_NRC then idx:=0;
end;


{---------------------------------------------------------------------------}
procedure mp_nextprime(var n: mp_int);
  {-next prime >= n, 2 if n<=2}
var
  id: integer;
  l: longint;
begin
  if mp_error<>MP_OKAY then exit;

  {$ifdef MPC_ArgCheck}
    if mp_not_init(n) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_nextprime');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {if n<=2 then nextprime = 2}
  if mp_cmp_d(n,3)=MP_LT then begin
    mp_set(n,2);
    exit;
  end;

  {Here n>2. If n <= 2^31-1 then use nextprime32. Note: This is correct}
  {because 2^31-1 is prime and so no 'overflow' to 32 bits can happen!}
  if mp_is_longint(n,l) then begin
    mp_set_int(n,nextprime32(l));
    exit;
  end;

  {move to first candidate}
  s_mp_npfirst(n, id);

  {loop thru possible primes}
  repeat
    if mp_error<>MP_OKAY then exit;
    if s_mp_is_pprime_ex(n, 3, mp_digit(MP_DIGIT_MAX and $3FFF)) then exit;
    {move to next candidate}
    s_mp_npnext(n,id);
  until false;
end;


{---------------------------------------------------------------------------}
procedure mp_prevprime(var n: mp_int);
  {-previous prime <= n, 0 if n<2}
var
  id,k: integer;
  m: mp_digit;
  l: longint;
begin
  if mp_error<>MP_OKAY then exit;

  {$ifdef MPC_ArgCheck}
    if mp_not_init(n) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_prevprime');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {if n<2 then prevprime = 0}
  if mp_cmp_d(n,2)=MP_LT then begin
    mp_zero(n);
    exit;
  end;

  {Here n>=2. If n has less than 32 bits then use prevprime32}
  if mp_is_longint(n,l) then begin
    mp_set_int(n,prevprime32(l));
    exit;
  end;

  {make n odd}
  if mp_iseven(n) then mp_dec(n);

  {$ifdef DELPHI}
    {avoid warning, id WILL always be initialized}
    id := 0;
  {$endif}

  mp_mod_d(n, NPRC_MOD, m);
  m := m shr 1;

  {move n to prev prime residue class mod MP_MOD and index id into diff array}
  for k:=m downto 0 do begin
    id := NPRC_OddIdx[k];
    {note: loop is always terminated via break because NPRC_OddIdx[0]<>-1}
    if id<>-1 then break;
    mp_sub_d(n,2,n);
  end;
  repeat
    if mp_error<>MP_OKAY then exit;
    {loop thru possible primes}
    if mp_is_pprime(n) then exit;
    {get prev increment index}
    dec(id); if id<0 then id:=NPRC_NRC-1;
    {move to prev candidate}
    mp_sub_d(n,NPRC_Diff[id],n);
  until false;
end;


{---------------------------------------------------------------------------}
procedure s_mp_coshmult(const a,b,c,mc: mp_int; var d: mp_int);
  {-internal coshmult, d=mod_coshmult(a,b,c), mc: Barrett parameter for c}
var
  v,w,t,q: mp_int;
  k: longint;        {bit loop counter}
  cmp: integer;      {cmp(b,1) result}
  bk,b1: boolean;    {status bit[k], bit[k+1]}
begin

  if mp_error<>MP_OKAY then exit;

  {s_mp_coshmult = v[b] = v[b;a] mod c}
  {v[k+2] = 2|a|*v[k+1] - v[k], v[0]=1, v[1]=|a| }

  {easy outs for b<=1, b<0 is handled as b=0}
  cmp := mp_cmp_d(b,1);
  if cmp=MP_LT then begin
    {v[0] = 1}
    mp_set1(d);
    exit;
  end
  else if cmp=MP_EQ then begin
    {v[1] = |a|}
    mp_abs(a,d);
    exit;
  end;

  {create local mp_int}
  mp_init4(v,q,w,t);  if mp_error<>0 then exit;

  {compatibility with Forster: silently set q = abs(a) mod c}
  mp_abs(a,q);
  mp_mod(q,c,q);

  {initialize}
  mp_set1(v);     {v = 1}
  mp_copy(q,w);   {w = |a|}
  bk := false;    {bit[k+1] = 0}

  {v[2k]   = 2*v[k]*v[k]   - a}
  {v[2k+1] = 2*v[k]*v[k+1] - 1}

  for k:=mp_bitsize(b) downto 0 do begin
    {Here: 0 <= v,w < c}
    {exit if error}
    if mp_error<>MP_OKAY then break;
    {get bit[k], swap v,w if different from b1[k+1]}
    b1 := bk;
    bk := mp_isbit(b,k);
    if b1<>bk then mp_exch(v,w);
    {t = 2*v}
    mp_mul_2(v,t);
    if mp_cmp_mag(c,t)<>MP_GT then mp_sub(t,c,t);
    {w = 2*v*w - a}
    mp_mul(t,w,w);
    mp_reduce(w,c,mc);
    mp_sub(w,q,w);
    if w.sign=MP_NEG then mp_add(w,c,w);
    {v = 2*v*v - 1}
    mp_mul(t,v,v);
    mp_reduce(v,c,mc);
    mp_dec(v);
    if v.sign=MP_NEG then mp_add(v,c,v);
  end;
  if bk then mp_exch(w,d) else mp_exch(v,d);
  mp_clear4(t,w,q,v);
end;


{---------------------------------------------------------------------------}
procedure s_mp_lucasvmod1(const p,n,k,mu: mp_int; var v: mp_int);
  {-calculate v=v[k] mod n of Lucas V sequence for p,q=1. mu: Barrett parameter}
  { for n, k>0. Internal use: no init check, assumes p<n, return v=2 for k<=0.}
var
  i,bc: longint;
  v1: mp_int;
begin

  {return v=2 if k<1}
  bc := mp_bitsize(k);
  if (bc=0) or (k.sign=MP_NEG) then begin
    mp_set(v,2);
    exit;
  end;

  {Initialize v=p, and v1=(p^2-2) mod n if k>1}
  mp_copy(p,v);
  if bc=1 then exit;

  mp_init(v1);            if mp_error<>MP_OKAY then exit;
  mp_sqr(p,v1);           mp_reduce(v1,n,mu);
  mp_sub_d(v1,2,v1);      if v1.sign=MP_NEG then mp_add(v1,n,v1);

  for i:=bc-2 downto 1 do begin
    if mp_isbit(k,i) then begin
      {v = v*v1 - p (mod n), v1 = v1^2 - 2 (mod n)}
      mp_mul(v,v1,v);     mp_reduce(v,n,mu);
      mp_sub(v,p,v);      if v.sign=MP_NEG then mp_add(v,n,v);
      mp_sqr(v1,v1);      mp_reduce(v1,n,mu);
      mp_sub_d(v1,2,v1);  if v1.sign=MP_NEG then mp_add(v1,n,v1);
    end
    else begin
      {v1 = v*v1 - p (mod n), v = v^2 - 2 (mod n)}
      mp_mul(v,v1,v1);    mp_reduce(v1,n,mu);
      mp_sub(v1,p,v1);    if v1.sign=MP_NEG then mp_add(v1,n,v1);
      mp_sqr(v,v);        mp_reduce(v,n,mu);
      mp_sub_d(v,2,v);    if v.sign=MP_NEG then mp_add(v,n,v);
    end;
  end;
  {Calculate final v (v1 not needed)}
  if mp_isodd(k) then begin
    mp_mul(v,v1,v);       mp_reduce(v,n,mu);
    mp_sub(v,p,v);        if v.sign=MP_NEG then mp_add(v,n,v);
  end
  else begin
    mp_sqr(v,v);          mp_reduce(v,n,mu);
    mp_sub_d(v,2,v);      if v.sign=MP_NEG then mp_add(v,n,v);
  end;
  mp_clear(v1);
end;


{---------------------------------------------------------------------------}
procedure mp_4sq(const m: mp_int; var a,b,c,d: mp_int);
  {-Decompose m>=0 into 4 squares: m = a^2 + b^2 + c^2 + d^2.}
  { If no solutions for m>0 was found, a=b=c=d=0 is returned.}
var
  n,p,x: mp_int;
  n8,i, Err: integer;
  v,s: longint;
label
  done;
const
  sn: array[0..17] of word = (1,2,3,10,34,58,85,130,214,226,370,526,706,730,1414,1906,2986,9634);
  sa: array[0..17] of byte = (1,1,1, 1, 3, 3, 6,  3,  3,  8,  8,  6, 15,  1,   6,  13,  21,  56);
  sb: array[0..17] of byte = (0,1,1, 3, 3, 7, 7, 11,  6,  9,  9,  7, 15, 27,  17,  21,  32,  57);
  sc: array[0..17] of byte = (0,0,1, 0, 4, 0, 0,  0, 13,  9, 15, 21, 16,  0,  33,  36,  39,  57);
begin

  {Based on an algorithm by Peter Schorn http://www.schorn.ch/lagrange.html}
  {and his other resources howto.html, FourSquares.java, and decompose.py}

  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(m) or mp_not_init(a) or mp_not_init(b) or mp_not_init(c) or mp_not_init(d) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_4sq');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  {must be positive}
  if m.sign=MP_NEG then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mp_4sq: m < 0');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;

  if mp_is0(m) then begin
    mp_zero(a);
    mp_zero(b);
    mp_zero(c);
    mp_zero(d);
    exit;
  end;

  mp_init3(n,p,x);
  if mp_error<>MP_OKAY then exit;

  {Divide out powers of 4: m = 4^v*n}
  v := mp_cnt_lsb(m) div 2;
  mp_shr(m, 2*v, n);

  {Initialize default values after computing v,n: @m may be in [@a,@b,@c,@d]}
  mp_zero(a);
  mp_zero(b);
  mp_zero(c);
  mp_zero(d);

  {x = sqrt(n), p = n-x^2}
  mp_sqrtrem(n,x,p);
  if mp_is0(p) then begin
    {n is a square, almost done}
    mp_exch(a,x);
    goto done;
  end;

  {get n mod 8}
  n8 := n.pdigits^[0] and 7;
  if n8=7 then begin
    {If n=7 mod 8, then all four variables a,b,c,d<>0 are needed:}
    {Subtract largest square d^2 < n with d^2 mod 8 <> 0}
    if (x.used>0) and (x.pdigits^[0] and 3 <> 0) then begin
      {d = x = floor(sqrt(n)) already found}
      mp_exch(x,d);
      mp_exch(n,p);
    end
    else begin
      {x=0 mod 8, set d=x-1. Then n-d^2 = n-x^2 + 2x-1 = p + 2x-1}
      mp_sub_d(x,1,d);
      mp_shl(x,1,n);
      mp_dec(n);
      mp_add(n,p,n);
    end;
    {multiply d by 4^v, this scaling is skipped skip for d at label done}
    mp_shl(d,v,d);
    {Recalculate x,p with n = x^2 + p}
    mp_sqrtrem(n,x,p);
  end
  else if n8 and 3 = 1 then begin
    {Here n mod 4=1; if n is prime then n = a^2 + b^2 via Cornacchia}
    if mp_is_pprime(n) then begin
      mp_set(c,1);
      mp_cornacchia(c,n,a,b,Err);
      if Err=0 then begin
        mp_zero(c);
        goto done;
      end;
    end;
  end;

  {Check special cases which cannot be handled by rest of algorithm}
  if mp_is_longint(n,s) and (s<=9634) then begin
    for i:=0 to 17 do begin
      if s=sn[i] then begin
        mp_set(a,sa[i]);
        mp_set(b,sb[i]);
        mp_set(c,sc[i]);
        goto done;
      end;
    end;
  end;

  {Here n = x^2 + p with n<>0, n mod 8 <> 7, n mod 4 <> 0}
  {$ifdef MPC_USE_Assert}
    assert(n.used>0, MPAF+'n<>0');
    assert(n.pdigits^[0] and 7 <> 7, MPAF+'n mod 8 <> 7');
    assert(n.pdigits^[0] and 3 <> 0, MPAF+'n mod 4 <> 0');
  {$endif}

  if 3 = n.pdigits^[0] and 3 then begin
    {Case 3 = n mod 4 = n mod 8; find odd x with p = (n-x^2)/2 prime}
    if mp_iseven(x) then begin
      mp_add(p,x,p);
      mp_dec(x);
      mp_add(p,x,p);
    end;
    mp_shr(p,1,p);
    repeat
      if mp_is_pprime(p) then begin
        mp_set(n,1);
        mp_cornacchia(n,p,b,n,Err);
        if Err=0 then begin
          mp_sub(b,n,c);
          mp_abs(c,c);
          mp_add(b,n,b);
          mp_copy(x,a);
          goto done;
        end;
      end;
      {Next pair (p,x): p = p + 2(x-1);  x = x - 2}
      mp_sub_d(x,1,n);
      mp_shl(n,1,n);
      mp_add(p,n,p);
      mp_sub_d(x,2,x);
    until (mp_error<>MP_OKAY) or (x.sign=MP_NEG);
  end
  else begin
    {Case n mod 4 = 1 or 2; find even x with p = n - x^2 prime}
    if (n.pdigits^[0] and 1)=(x.pdigits^[0] and 1) then begin
      mp_shl(x,1,n);
      mp_add(p,n,p);
      mp_dec(x);
      mp_dec(p);
    end;
    repeat
      if mp_is_pprime(p) then begin
        mp_set(n,1);
        mp_cornacchia(n,p,b,c,Err);
        if Err=0 then begin
          mp_copy(x,a);
          goto done;
        end;
      end;
      {Next pair (p,x): p = p + 4(x-1);  x = x - 2}
      mp_sub_d(x,1,n);
      mp_shl(n,2,n);
      mp_add(p,n,p);
      mp_sub_d(x,2,x);
    until (mp_error<>MP_OKAY) or (x.sign=MP_NEG);
  end;

  {no solution found, reset d=0. a,b,c are still zero}
  mp_zero(d);

  {$ifdef MPC_TRACE}
    mp_tracev(#13#10'** No solution found!'#13#10);
  {$endif}

done:
  {rescale initial powers of 4, d is 0 or already scaled}
  if v>0 then begin
    mp_shl(a,v,a);
    mp_shl(b,v,b);
    mp_shl(c,v,c);
  end;

  mp_clear3(n,p,x);
end;


{---------------------------------------------------------------------------}
procedure mp_4sq_sa(const m: mp_int; var a,b,c,d: mp_int);
  {-Decompose m>=0 into 4 squares: m = a^2 + b^2 + c^2 + d^2, a<=b<=c<=d.}
  { If no solutions for m>0 was found, a=b=c=d=0 is returned.}
begin
  {Error checking in mp_4sq}
  mp_4sq(m,a,b,c,d);
  { make a <= b <= c <= d}
  if mp_error<>MP_OKAY then exit;
  if mp_cmp_mag(a,b)>0 then mp_exch(a,b);
  if mp_cmp_mag(a,c)>0 then mp_exch(a,c);
  if mp_cmp_mag(a,d)>0 then mp_exch(a,d);
  if mp_cmp_mag(b,c)>0 then mp_exch(b,c);
  if mp_cmp_mag(b,d)>0 then mp_exch(b,d);
  if mp_cmp_mag(c,d)>0 then mp_exch(c,d);
end;


{---------------------------------------------------------------------------}
procedure mp_4sq_sd(const m: mp_int; var a,b,c,d: mp_int);
  {-Decompose m>=0 into 4 squares: m = a^2 + b^2 + c^2 + d^2, a>=b>=c>=d.}
  { If no solutions for m>0 was found, a=b=c=d=0 is returned.}
begin
  {Error checking in mp_4sq}
  mp_4sq(m,a,b,c,d);
  { make a <= b <= c <= d}
  if mp_error<>MP_OKAY then exit;
  if mp_cmp_mag(a,b)<0 then mp_exch(a,b);
  if mp_cmp_mag(a,c)<0 then mp_exch(a,c);
  if mp_cmp_mag(a,d)<0 then mp_exch(a,d);
  if mp_cmp_mag(b,c)<0 then mp_exch(b,c);
  if mp_cmp_mag(b,d)<0 then mp_exch(b,d);
  if mp_cmp_mag(c,d)<0 then mp_exch(c,d);
end;


{---------------------------------------------------------------------------}
procedure mp_addmod(const a,b,c: mp_int; var d: mp_int);
  {-calculate d = a + b (mod c)}
var
  t: mp_int;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(b) or mp_not_init(c) or mp_not_init(d) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_addmod');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  mp_init(t);
  if mp_error<>MP_OKAY then exit;

  mp_add(a, b, t);
  mp_mod(t, c, d);
  mp_clear(t);
end;


{---------------------------------------------------------------------------}
procedure s_mp_binom_l(n: longint; k: word; var a: mp_int);
  {-internal binomial coefficient for small k, no init check}
var
  i,e2,q: word;
  p: longint;
  tmp: mp_int;
begin
  if mp_error<>MP_OKAY then exit;

  {special cases}
  if k>n then begin
    mp_zero(a);
    exit;
  end;
  if k=1 then begin
    mp_set_int(a,n);
    exit;
  end;
  mp_set1(a);
  if (n<2) or (k=0) or (k=n) then exit;

  mp_init(tmp);
  if mp_error<>MP_OKAY then exit;

  {if k>n/2 use symmetry to reduce k}
  if k>n-k then k := n-k;
  n := n-k+1;
  {e2 will count powers of 2}
  e2 := 0;
  for i:=1 to k do begin
    {divide out powers of 2 in numerator}
    p := n;
    while p and 1 = 0 do begin
      p := p shr 1;
      inc(e2);
    end;
    s_mp_mul_int(a,p,a,tmp);
    {divide out powers of 2 in denominator}
    q := i;
    while q and 1 = 0 do begin
      q := q shr 1;
      dec(e2);
    end;
    mp_div_w(a,q,@a,q);
    inc(n);
  end;
  if e2>0 then mp_shl(a,e2,a);
  mp_clear(tmp);
end;


{---------------------------------------------------------------------------}
procedure mp_binomial(n,k: longint; var a: mp_int);
  {-calculate the binomial coefficient a = (n choose k)}

  {The basic idea is from Peter Luschny's [11] FastBinomial page }
  {http://www.luschny.de/math/factorial/FastBinomialFunction.html}

  {For each prime 2 < p <= n the power p^e dividing the binomial }
  {coefficient is calculated. e is the number of borrows in the  }
  {subtraction n-k in base p. (P.Goetgheluck, Computing Binomial }
  {Coefficients, Amer. Math. Monthly 94 (1987), 360-365)         }

  {Also used is Marcel Martin's [8] idea to evaluate the product }
  {of a list of numbers: It is faster to calculate intermediate  }
  {products of roughly the same size rather sequentially multiply}
  {each factor of the list. The arrays X and Level are used for  }
  {managing the intermediates with a stack like structure.       }

const
  MAXS = 31;
var
  X    : array[0..MAXS] of mp_int;   {Used for managing the intermediate}
  Level: array[0..MAXS] of integer;  {products of the prime powers p^e}

  function BinPrimeExpo(p,n,k: longint): longint;
    {-compute max. exponent e with p^e divides n over k}
  var
    e,r: longint;
  begin
    e := 0;
    r := 0;
    while n>0 do begin
      r := ord(n mod p < (r + k mod p));
      e := e + r;
      n := n div p;
      k := k div p;
    end;
    BinPrimeExpo := e;
  end;

var
  ctx: TPrimeContext;   {used to generate the prime sequence}
  e,nmk,n2,p: longint;
  e2,s,smax: integer;

label
  leave;

begin

  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_binomial');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {If n<0 and k>=0, we use the identity 1.2.6 (17) from D.E. Knuth: The art}
  {of computer programming, Vol. 1, Fundamental algorithms, 3rd ed., 1997.}
  {[*] binomial(n,k) = (-1)^k * binomial(k-n-1,k)}

  {init default result}
  mp_set1(a);
  if (k=0) or (k=n) then exit;

  if (k=1) or (k=pred(n)) then begin
    mp_set_int(a,n);
    exit;
  end;

  if k<0 then begin
    if (n>=0) or (n<k) then mp_zero(a)
    else begin
      {Here k <= n < 0:  We use binomial(n,k) = binomial(n,n-k) which is}
      {valid for all integer k, and then use [*] since n<0 and n-k >= 0.}
      {binomial(n,k) = (-1)^(n-k)*binomial(n-k-n-1,n-k)}
      n := n-k;
      mp_binomial(-succ(k),n,a);
      if odd(n) then s_mp_chs(a);
    end;
    exit;
  end;

  if n<0 then begin
    {Here n < 0 and k >= 0. Apply Knuth's [*] identity}
    mp_binomial(k-n-1,k,a);
    if odd(k) then s_mp_chs(a);
    exit;
  end;

  {here n is >= 0, handle easy case first}
  if k>n then begin
    mp_zero(a);
    exit;
  end;

  {Here n >= k >= 2. If k>n/2 use symmetry to reduce k}
  if k>n-k then k := n-k;

  {handle small values without prime power method}
  if k<64 then begin
    s_mp_binom_l(n,word(k),a);
    exit;
  end;

  {Init partial product stack indices and some derived variables}
  s   := -1;
  smax:= -1;
  nmk := n-k;
  n2  := n div 2;

  {power of 2 calculated separately and shifted in at the end}
  {e2 = (n-popcount(n)) - (k-popcount(k)) - (n-k-popcount(n-k))}
  e2 := popcount32(k) + popcount32(nmk) - popcount32(n);

  {Init context so that FindNextPrime32 returns 3}
  FindFirstPrime32(2,ctx);

  {iteration over the primes > 2}
  while mp_error=MP_OKAY do begin
    FindNextPrime32(ctx);
    p := ctx.prime;
    {done if all primes <= n are processed}
    if p>n then break;

    {do some preprocessing to avoid calls to BinPrimeExpo}
    if p>nmk then e := 1
    else if p>n2  then e := 0
    else e := BinPrimeExpo(p,n,k);

    {if p divides n over k, accumulate p^e}
    if e>0 then begin
      if s<MAXS then begin
        inc(s);
        if s>smax then begin
          {init a new stack level, smax is only incremented if no error}
          mp_init(X[s]);
          if mp_error<>MP_OKAY then goto leave;
          smax := s;
        end;
        {calculate and push p^e }
        case e of
            1: mp_set_int(X[s],p);
            2: mp_set_int(X[s],sqr(p));
          else mp_set_pow(X[s],p,e);
        end;
        {new factor has level 0}
        Level[s] := 0;
        {reduce stack}
        while (s > 0) and (Level[s-1] = Level[s]) do begin
          mp_mul(X[s-1],X[s],X[s-1]);
          dec(s);
          {inc level of new partial factor}
          inc(Level[s]);
        end;
      end
      else begin
       {MAXS should be big enough, just paranoia}
       {$ifdef MPC_HaltOnError}
         {$ifdef MPC_UseExceptions}
           raise MPXRange.Create('mp_binomial: stack too small');
         {$else}
           RunError(MP_RTE_RANGE);
         {$endif}
       {$else}
         set_mp_error(MP_RANGE);
         goto leave;
       {$endif}
      end;
    end;
  end;
  if (mp_error=MP_OKAY) and (s>=0) then begin
    {finalize partial product processing if stack is not empty}
    mp_exch(a,X[s]);
    while s > 0 do begin
      dec(s);
      mp_mul(a,X[s],a);
    end;
  end;
  {shift in powers of 2}
  mp_shl(a,e2,a);

  {clear only the used X[s]}
leave:
  for s:=0 to smax do mp_clear(X[s]);
end;


{---------------------------------------------------------------------------}
procedure mp_cbrtmod_ex(const a, p: mp_int; var x: mp_int; pru: pmp_int; var Err: integer);
  {-Compute a cube root x of a with x^3 = a mod p, p prime. If pru<>nil, pru^ }
  { is set to a 3rd root of unity (or 1 if x is unique), the other roots are  }
  { x*pru^, x*pru^*pru^. Err <> 0, if a is not a 3rd power or p is not prime. }
var
  i,j,k: longint;
  d: mp_digit;
  b,q,t,y,z: mp_int;
label
  leave;
begin

  {See S.C. Lindhurst [32], Chap. 3.2: Extension of Shanks's Algorithm to rth Roots}
  Err := 0;

  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(p) or mp_not_init(x) or ((pru<>nil) and mp_not_init(pru^)) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_cbrtmod');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if (@a=@p) or (@a=@x) or (@a=pru) or (@p=@x) or (@p=pru) or (@x=pru) then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXBadArg.Create('mp_cbrtmod: var addresses not unique');
      {$else}
        RunError(MP_RTE_BADARG);
      {$endif}
    {$else}
      set_mp_error(MP_BADARG);
      exit;
    {$endif}
  end;

  if p.used=1 then begin
    d := p.pdigits^[0];
    if (d=2) or (d=3) then begin
      if pru<>nil then mp_set1(pru^);
      mp_mod_d(a,d,d);
      mp_set(x,d);
      exit;
    end;
  end;

  if s_mp_is_le0(p) or mp_is1(p) or mp_iseven(p) then begin
    {p<2 or even, ie p is not prime}
    Err := 1;
    exit;
  end;

  mp_init5(b,q,t,y,z);
  if mp_error<>MP_OKAY then exit;

  {case a=0 mod p}
  mp_mod(a,p,t);
  if t.used=0 then begin
    mp_zero(x);
    if pru<>nil then mp_set1(pru^);
    goto leave;
  end;

  mp_mod_d(p,3,d);

  if d=0 then begin
    {p>3 is a multiple of 3, i.e. p is not prime}
    Err := 1;
    goto leave;
  end
  else if d=2 then begin
    {Easy case: if p=2 mod 3, then  x = a^((2p-1)/3) mod p}
    mp_add(p,p,t);
    mp_dec(t);
    mp_div_d(t,3,@t,d);
    mp_exptmod(a,t,p,x);
    if pru<>nil then mp_set1(pru^);
    goto leave;
  end;

  {Here p=1 mod 3; calculate n,q with p-1 = 3^k*q and gcd(q,3)=1}

  {Compute b=(p-1)/3: used for k,q calculation, non-3rd power search,}
  {primitive 3rd root determination, and main loop initialization}
  mp_sub_d(p,1,b);
  mp_div_d(b,3,@t,d);

  {Get k and q, because p=1 mod 3 we can use repeat}
  k := 0;
  mp_copy(t,b);
  repeat
    inc(k);
    mp_exch(q,t);
    mp_div_d(q,3,@t,d)
  until d<>0;

  {find a cubic non-residue z}
  i := 1;
  repeat
    i := nextprime32(i+1);
    if i<0 then begin
      Err := 2;
      goto leave;
    end;
    mp_set_int(z,i);
    mp_exptmod(z,b,p,y);
    if not mp_is1(y) then break;
    if mp_error<>MP_OKAY then goto leave;
    if i=541 then begin
      {after unsuccessfully testing 100 primes as CNR candidate,}
      {check if p itself is prime before going on}
      if not mp_is_pprime(p) then begin
        Err := 1;
        goto leave;
      end;
    end;
  until false;

  {z is a cubic non-residue,  y = z^((p-1)/3) mod p is primitive 3rd root}
  {of unity. Initialize loop: (k=n), z = z^q, x = a^((mq+1)/3), b = a^mq,}
  {where m=1 if q mod 3=2, and m=2 if q mod 3 = 1}

  {z = z^q}
  mp_exptmod(z,q,p,z);

  {t = (mq-2)/3}
  mp_mod_d(q,3,d);
  if d=1 then mp_shl1(q);
  mp_sub_d(q,2,t);
  mp_div_d(t,3,@t,d);

  {x = a^((mq-2)/3)}
  mp_exptmod(a,t,p,x);

  {b = x*(ax)^2 = a^2*x^3 = a^(2+3(mq-2)/3) = a^mq}
  mp_mulmod(a,x,p,b);
  mp_sqrmod(b,p,b);
  mp_mulmod(b,x,p,b);

  {x = x*a = a^(1+(mq-2)/3) = a^((mq+1)/3)}
  mp_mulmod(a,x,p,x);

  {main loop, invariant: x^3 = a*b}
  while not mp_is1(b) do begin
    if mp_error<>MP_OKAY then goto leave;
    {find least positive integer with b^(3^j) = 1}
    j := 0;
    mp_copy(b,q);
    repeat
      inc(j);
      {j=k should not happen if p is prime and a is a 3rd power, indicate failure}
      if (j=k) or (mp_error<>MP_OKAY) then begin
        Err := 3;
        goto leave;
      end;
      mp_sqrmod(q,p,t);
      mp_mulmod(q,t,p,t);
      if mp_is1(t) then break;
      mp_exch(q,t);
    until false;

    {Get l=1 or 2 with (bz^l)^(3^j-1) = b^(3^j-1)*(z^(3^j-1))^l = 1 }
    {c.f.3.2.3 Speeding Things Up. We have q=b^(3^j-1), q^3=1, q<>1,}
    {i.e. q=y or q=y^2. If q=y, then l=2; continue with z^2 and y^2.}
    {see also: cubic_root.py from http://tnt.math.metro-u.ac.jp/nzmath}

    if mp_is_eq(q,y) then begin
      mp_sqrmod(z,p,t);
      mp_sqrmod(y,p,y);
    end
    else mp_copy(z,t);

    {t = t^(3^(k-j-1))}
    mp_set(q,3);
    mp_exptmod_d(q,k-j-1,p,q);
    mp_exptmod(t,q,p,t);

    {z = t^3}
    mp_sqrmod(t,p,z);
    mp_mulmod(z,t,p,z);
    {x = x*t}
    mp_mulmod(x,t,p,x);
    {b = b*z}
    mp_mulmod(b,z,p,b);
    k := j;
  end;

  if pru<>nil then mp_copy(y,pru^);

leave:
  mp_clear5(b,q,t,y,z);
end;


{---------------------------------------------------------------------------}
procedure mp_cbrtmod(const a, p: mp_int; var x: mp_int; var Err: integer);
  {-Compute a cube root x of a with x^3 = a mod p, p prime.  }
  { Err in [1..3], if a is not a 3rd power or p is not prime.}
begin
  mp_cbrtmod_ex(a, p, x, nil, Err);
end;


{---------------------------------------------------------------------------}
procedure s_mp_cbrtmodpk(const a,p: mp_int; k: word; red: boolean; var b: mp_int; var Err: integer);
  {-Compute a cube root b of a with b^3 = a mod p^k, p prime <> 3.}
  { If red=true, reduce b mod p^k, otherwise b may be >= p^k. Error codes:}
  { 1<Err<4: from mp_cbrtmod, Err=4 if p=3, Err=5: no inverse mod p^(2^i)}
var
  p2i,ri,x,z: mp_int;
  i: integer;
begin
  Err := 0;
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(p) or mp_not_init(b) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('s_mp_cbrtmodpk');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if mp_iszero(a) or mp_is1(a) or (k=0) then begin
    {if a=0 or 1, set b=a}
    if k=0 then mp_zero(b) else mp_copy(a,b);
    exit;
  end;
  if mp_cmp_d(p,3)<>0 then begin
    mp_init4(p2i,ri,x,z);
    if mp_error<>MP_OKAY then exit;
    {r0 = cbrt(a) mod p}
    mp_cbrtmod(a,p,ri,Err);
    if (Err=0) and (k>1) then begin
      if mp_is0(ri) and (k>=3) then begin
        {zero solution but a<>0: try to divide out p^3}
        mp_sqr(p,p2i);
        mp_mul(p2i,p,p2i);
        mp_divrem(a,p2i,@x,@z);
        if mp_is0(z) then begin
          s_mp_cbrtmodpk(x,p,k-3,true,ri,Err);
          if Err=0 then mp_mul(ri,p,ri);
        end
        else Err:=5;
      end
      else begin
        {[10] Alg 2.3.11 (Hensel lifting) with f(x)=a-x^3, f'(x)=-3x}
        {Calls function newr repeatedly until 2^i >= k}
        for i:=0 to bitsize32(k)-1 do begin
          {calculate p^(2^i): copy from p or square p^(2^(i-1))}
          if i=0 then mp_copy(p,p2i) else mp_sqr(p2i,p2i);
          {z = f'(ri)^-1 mod p^(2^i) = -(3*ri^2)^-1 mod p^(2^i)}
          mp_sqr(ri,z);
          mp_mul_d(z,3,z);
          if not mp_invmodf(z,p2i,z) then begin
            Err := 8;
            break;
          end;
          {x = f(ri)/p^(2^i) = (a-ri^3)/p^(2^i)}
          mp_sqr(ri,x);
          mp_mul(x,ri,x);
          mp_sub(a,x,x);
          mp_div(x,p2i,x);
          {x = -xz mod p^(2^i)) = x/(3*ri^2) mod p^(2^i))}
          mp_mulmod(z,x,p2i,x);
          {r(i+1) = ri + x*p^(2^i)}
          mp_mul(x,p2i,x);
          mp_add(x,ri,ri);
        end;
      end;
    end;
    if Err=0 then begin
      {store last ri as result into b, reduce if requested}
      if red then begin
        mp_expt_d(p,k,x);
        mp_mod(ri,x,b);
      end
      else mp_exch(b,ri);
    end;
    mp_clear4(p2i,ri,x,z);
  end
  else Err:=4;
end;


{---------------------------------------------------------------------------}
function s_mp_is_cubres(const a, p: mp_int): boolean;
  {-Simple test if a is a cubic residue mod p, p prime. Primality of p}
  { is not checked, but some trivial non-prime cases are detected.}
var
  d: mp_digit;
  t: mp_int;
begin
  s_mp_is_cubres := false;
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(p) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('s_mp_is_cubres');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  if p.sign=MP_NEG then exit;

  if p.used=1 then begin
    d := p.pdigits^[0];
    if d<2 then exit;  {p=0 or 1}
    if d<4 then begin
      {p=2 or 3: all a are CR}
      s_mp_is_cubres := true;
      exit;
    end;
  end;
  if mp_iseven(p) then exit;

  mp_init(t);
  if mp_error<>MP_OKAY then exit;

  mp_mod(a,p,t);
  if mp_is0(t) or mp_is1(t) then begin
    {case a=0,1 mod p}
    s_mp_is_cubres := true
  end
  else begin
    {Check t=p-1=-1 mod p. Don't waste time for inc(t) if t is too small.}
    {Here 0 <= t < p; and since p is odd, t.used <= p.used = (p-1).used}
    if t.used >= p.used then begin
      mp_inc(t);
      if mp_cmp_mag(p,t)=0 then begin
        mp_clear(t);
        s_mp_is_cubres := true;
        exit;
      end;
    end;
    mp_sub_d(p,1,t);
    mp_div_d(t,3,@t,d);
    if d=0 then begin
      {Euler's criterion for p=1 mod 3: x^3 = a mod is solvable if }
      {a^((p-1)/gcd(p-1,3)) = a^((p-1)/3)) = a^t = 1 mod p}
      mp_exptmod(a,t,p,t);
      s_mp_is_cubres := mp_is1(t);
    end
    else begin
      {Easy cases: if d=2 ie p=0 mod 3, then p is not prime and  }
      {if d=1 ie p=2 mod 3, then a^((2p-1)/3) mod p is a solution}
      s_mp_is_cubres := d=1;
    end;
  end;
  mp_clear(t);
end;


{---------------------------------------------------------------------------}
procedure mp_cbrtmod3k(const a: mp_int; k: word; var b: mp_int; var Err: integer);
  {-Compute a cube root b of a with b^3 = a mod 3^k. Error codes: 1<Err<4 code from}
  { mp_cbrtmod, Err=4 if p=3, Err=5: no inverse mod 3^(2^i), Err=6: (internal d<>0)}
var
  i: integer;
  d: mp_digit;
  p2i,pk,x,z: mp_int;
begin
  Err := 0;
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(b) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('s_mp_cbrtmod3k');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if mp_iszero(a) or mp_is1(a) or (k=0) then begin
    {if a=0 or 1, set b=a}
    mp_copy(a,b);
    exit;
  end;
  mp_init4(p2i,pk,x,z);
  if mp_error<>MP_OKAY then exit;
  mp_set_pow(pk,3,k);  {3^k}
  mp_set(p2i,3);       {3^2^i}
  {get solution mod 3}
  mp_cbrtmod(a,p2i,z,Err);
  {if no solution mod 3 then no solution mod 3^k}
  if (Err=0) and (k>1) then begin
    if mp_is0(z) and (k>=3) then begin
      {zero solution but a<>0: try to divide out 3^3}
      mp_div_d(a,27,@x,d);
      if d=0 then begin
        mp_cbrtmod3k(x,k-3,z,Err);
        if Err=0 then mp_mul_d(z,3,z);
      end
      else Err:=5;
    end
    else begin
      {loop is executed until 3^(2^i) >= 3^k, i.e. 2^i>=k}
      for i:=0 to bitsize32(k)-1 do begin
        {z<>0 is a solution mod 3^(2^i), calculate solution mod 3^(2^(i+1))}
        {s_mp_cbrtmodpk cannot be used because f'(z)= 0 mod 3. Use}
        {discrete Newton z~ = z - f(z)/f'(z)   = z - (z^3-a)/3z^2 }
        {                   = z - z/3 + a/3z^2 = (2z + a/z^2)/3   }
        {i.e. iterate z = (a*z^-2 +2z)/3 mod 3^(2^(i+1))}
        mp_sqr(p2i,p2i);
        mp_sqr(z,x);
        if not mp_invmodf(x,p2i,x) then begin
          Err := 5;
          break;
        end;
        mp_mul(a,x,x);
        mp_shl1(z);
        mp_add(z,x,z);
        mp_div_d(z,3,@z,d);
        if d<>0 then begin
          Err := 6;
          break;
        end;
        mp_mod(z,p2i,z);
      end;
    end;
  end;
  if Err=0 then begin
    {z is a solution, either directly for k=1 or via iteration for k>1}
    {always reduce mod 3^k}
    mp_mod(z,pk,b);
  end;
  mp_clear4(p2i,pk,x,z);
end;


{---------------------------------------------------------------------------}
procedure mp_cbrtmodpk(const a,p: mp_int; k: word; var b: mp_int; var Err: integer);
  {-Compute a cube root b of a with b^3 = a mod p^k, p prime. b will be  }
  { reduced mod p^k. Error codes: 1<Err<4: from mp_cbrtmod, Err=4 if p=3,}
  { Err=5: no inverse mod p^(2^i), Err=6: internal check for p=3 failed. }
begin
  if mp_cmp_d(p,3)=0 then mp_cbrtmod3k(a,k,b,Err)
  else s_mp_cbrtmodpk(a,p,k,true,b,Err);
end;


{---------------------------------------------------------------------------}
procedure mp_cbrtmodpq(const a,p,q: mp_int; var x: mp_int; var Err: integer);
  {-Compute a cube root x of a mod (pq); p,q primes. If p=q, mp_cbrtmodpk is}
  { used. For p<>q: Err=8 if gcd(p,q)<>1, otherwise error code from mp_cbrtmod.}
var
  c,d,n,r: mp_int;
begin
  Err := 0;
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(p) or mp_not_init(q) or mp_not_init(x) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_cbrtmodpq');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {Handle case p=q}
  if mp_is_eq(p,q) then begin
    mp_cbrtmodpk(a,p,2,x,Err);
    exit;
  end;

  mp_init4(c,d,n,r);
  {First compute extended gcd, c*p + d*q = gcd(p,q) = n. Error if n<>1}
  {r = cbrt(a) mod p, s = cbrt(a) mod q. Result x = (rdq+scp) mod pq}
  mp_xgcd(p,q,@c,@d,@n);
  if not mp_is1(n) then Err:=8
  else begin
    {calculate roots mod p and mod q}
    mp_cbrtmod(a,p,r,Err);
    if (Err=0) and (mp_error=MP_OKAY) then begin
      mp_mul(p,q,n);
      mp_mulmod(d,q,n,d);
      mp_mulmod(r,d,n,d);
      mp_cbrtmod(a,q,r,Err);
      if (Err=0) and (mp_error=MP_OKAY) then begin
        mp_mulmod(c,p,n,c);
        mp_mulmod(r,c,n,c);
        mp_addmod(c,d,n,x);
      end;
    end;
  end;
  mp_clear4(c,d,n,r);
end;


{---------------------------------------------------------------------------}
procedure mp_cornacchia(const d,p: mp_int; var x,y: mp_int; var Err: integer);
  {-Solve x^2 + d*y^2 = p, p prime, 0<d<p with Cornacchia's algorithm. Check}
  { @x<>@y, but not p prime. Err<>0 if no solution exist. x >= y if d=1.}
var
  a,b,t: mp_int;
  d1: boolean;
begin
  {Ref: [24] Cohen, Algorithm 1.5.2}
  {     [10] Crandall/Pomerance, Algorithm 2.3.12}

  {Error codes:
    -2:  d<=0 or d>p or initial mp_error<>0
     4:  (p-x^2)/d is no square
     5:  (p-x^2)/d is no integer
   else  error code from mp_sqrtmod}

  Err := -2;
  if mp_error<>MP_OKAY then exit;

  {$ifdef MPC_ArgCheck}
    if mp_not_init(d) or mp_not_init(p) or mp_not_init(x) or mp_not_init(y) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_cornacchia');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if @x=@y then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXBadArg.Create('mp_cornacchia: @x=@y');
      {$else}
        RunError(MP_RTE_BADARG);
      {$endif}
    {$else}
      set_mp_error(MP_BADARG);
      exit;
    {$endif}
  end;

  if s_mp_is_le0(d) then exit;

  mp_init3(a,b,t);

  {Check upper bound for d and handle special cases}
  mp_add_d(d,1,t);
  if mp_is_ge(t,p) then begin
    {no solution for d>p}
    if mp_is_le(d,p) then begin
      {d=p or d=p-1}
      Err := 0;
      mp_set1(y);
      if mp_is_eq(d,p) then begin
        {d=p  => x=0, y=1}
        mp_zero(x);
      end
      else begin
        {d=p-1  => x=1, y=1}
        mp_set1(x);
      end;
    end;
    mp_clear3(a,b,t);
    exit;
  end;

  {Remember if d=1}
  d1 := mp_is1(d);

  {Step 1a: Solve b^2 = -d mod p, Jacobi check (-d|p) is done in mp_sqrtmod}
  mp_chs(d,t);
  mp_sqrtmod(t,p,b,Err);
  if Err=0 then begin
    {Step 1b: if b <= p/2, or equivalent 2b <= p, then replace b by p-b}
    mp_mul_2(b,t);
    if mp_is_le(t,p) then mp_sub(p,b,b);
    mp_copy(p,a);
    mp_sqrt(p,t);
    {Step 2: Apply Euclidean algorithm to (a,b) until b <= sqrt(p)}
    while (mp_error=MP_OKAY) and mp_is_gt(b,t) do begin
      mp_mod(a,b,a);
      mp_exch(a,b);
    end;
    {Step 3: Set x=b, y = sqrt((p-x^2)/d). If y is in N, then the}
    {solution is (x,y). Otherwise there is no solution, return Err}
    mp_sqr(b,t);
    mp_sub(p,t,a);
    mp_divrem(a,d,@a,@t);
    if mp_is0(t) then begin
      if not mp_is_square2(a,@y) then Err := 4;
    end
    else Err := 5;
  end;
  if Err=0 then begin
    mp_exch(x,b);
    {Normalize solution: x >= y if d=1}
    if d1 and mp_is_lt(x,y) then mp_exch(x,y);
  end;

  mp_clear3(a,b,t);
end;


{---------------------------------------------------------------------------}
procedure mp_cornacchia4(const d,p: mp_int; var x,y: mp_int; var Err: integer);
  {-Solve x^2 + |d|*y^2 = 4p, p prime, -4p<d<0 with the modified Cornacchia}
  { algorithm. Checks @x<>@y, but not p prime. Err<>0 if no solution exist.}
var
  a,b,t: mp_int;
  c2: integer;
label
  leave;
begin
  {Ref: [24] Cohen, Algorithm 1.5.3}
  {     [10] Crandall/Pomerance, Algorithm 2.3.13}

  {Error codes:
    -2:  d >= 0 or d <= -4p or p<2 or initial mp_error<>0
     4:  (p-x^2)/d is no square
     5:  (p-x^2)/d is no integer
   else  error code from mp_sqrtmod}

  Err := -2;
  if mp_error<>MP_OKAY then exit;

  {$ifdef MPC_ArgCheck}
    if mp_not_init(d) or mp_not_init(p) or mp_not_init(x) or mp_not_init(y) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_cornacchia4');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if @x=@y then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXBadArg.Create('mp_cornacchia4: @x=@y');
      {$else}
        RunError(MP_RTE_BADARG);
      {$endif}
    {$else}
      set_mp_error(MP_BADARG);
      exit;
    {$endif}
  end;

  mp_init3(a,b,t);
  if mp_error<>MP_OKAY then exit;

  {check -4p < d < 0, implemented as d<0 and |d|<|4p|}
  {set t=4p, will be used here and later for sqrt(4p)}
  mp_shl(p,2,t);
  c2 := mp_cmp_d(p,2);

  if (d.sign=MP_ZPOS) or (mp_cmp_mag(d,t)<>MP_LT) or (c2=MP_LT) then goto leave;

  if c2=MP_EQ then begin
    {handle case p=2}
    mp_add_d(d,8,t);
    if mp_is_square2(t,@x) then mp_set1(y);
    goto leave;
  end;

  {Step 1a: Solve b^2 = d mod p, Jacobi check (d|p) is done in mp_sqrtmod}
  mp_sqrtmod(d,p,b,Err);
  if Err=0 then begin
    {Step 1b: if b <> d mod 2 then replace b by p-b}
    if (b.pdigits^[0] and 1)<>(d.pdigits^[0] and 1) then mp_sub(p,b,b);
    {t := sqrt(4p)}
    mp_sqrt(t,t);
    {Euclid starting value a = 2p}
    mp_mul_2(p,a);
    {Step 2: Apply Euclidean algorithm to (a,b) until b <= sqrt(p)}
    while (mp_error=MP_OKAY) and mp_is_gt(b,t) do begin
      mp_mod(a,b,a);
      mp_exch(a,b);
    end;
    {Step 3: Set x=b, y = sqrt((4p-x^2)/|d|). If y is in N, then the}
    {solution is (x,y). Otherwise there is no solution, return Err}
    mp_sqr(b,t);
    mp_shl(p,2,a);
    mp_sub(a,t,a);
    mp_abs(d,t);
    mp_divrem(a,t,@a,@t);
    if mp_is0(t) then begin
      if not mp_is_square2(a,@y) then Err := 4;
    end
    else Err := 5;
    if Err=0 then mp_exch(x,b);
  end;

leave:
  mp_clear3(a,b,t);
end;


{---------------------------------------------------------------------------}
procedure mp_crt_setup(n: integer; const m: array of mp_int; var c: array of mp_int);
  {-Calculate CRT coefficients c[i] for pairwise co-prime moduli m[i], i=0..n-1.}
begin
  if not mp_crt_setupf(n,m,c) then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXUndef.Create('mp_crt_setup: setup failed');
      {$else}
        RunError(MP_RTE_OTHER);
      {$endif}
    {$endif}
    set_mp_error(MP_UNDEF);
  end;
end;


{---------------------------------------------------------------------------}
function mp_crt_setupf(n: integer; const m: array of mp_int; var c: array of mp_int): boolean;
  {-Calculate CRT coefficients c[i] for pairwise co-prime moduli m[i], i=0..n-1.}
  { Return true if c[i] are successfully calculated.}
label
  _err;
var
  u: mp_int;
  i,j: integer;
begin
  mp_crt_setupf := false;
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init_multi(m) or mp_not_init_multi(c) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_crt_setupf');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  dec(n);
  if (n > high(m)) or (n > high(c)) then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mp_crt_setupf: invalid n');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;
  mp_init(u);
  if mp_error<>MP_OKAY then exit;
  {Step 1 of Garner's algorithm for CRT, HAC Alg. 14.71}
  for i:=0 to n do begin
    mp_set1(c[i]);
    for j:=0 to pred(i) do begin
      if (mp_error<>MP_OKAY) or not mp_invmodf(m[j],m[i],u) then goto _err;
      mp_mulmod(u,c[i],m[i],c[i]);
    end;
  end;
  mp_crt_setupf := mp_error=MP_OKAY;

_err:
  mp_clear(u);
end;


{---------------------------------------------------------------------------}
procedure mp_crt_single(n: integer; const m,v: array of mp_int; var x: mp_int);
  {-Calculate x with x mod m[i] = v[i], pairwise co-prime moduli m[i], i=0..n-1.}
  { Use mp_crt_setup/calc if more than one system shall be solved}
var
  u,p,ci: mp_int;
  i: integer;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init_multi(m) or mp_not_init_multi(v) or mp_not_init(x) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_crt_single');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  dec(n);
  if (n<0) or (n>high(m)) or (n>high(v)) then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mp_crt_single: invalid n');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;

  {start with x = v[0] mod m[0]}
  mp_mod(v[0],m[0],x);
  if n=0 then exit; {trivial case, don't use local mp_ints}

  mp_init3(u,p,ci);
  if mp_error=MP_OKAY then begin
    {already computed: mp_mod(v[0],m[0],x)}
    mp_set1(p);
    for i:=1 to n do begin
      {p = product(m[j], j=0..i-1)}
      mp_mul(p,m[i-1],p);
      {calculate c[i] from mp_crt_setup 'on the fly'}
      mp_invmod(p,m[i],ci);
      mp_submod(v[i],x,m[i],u);
      mp_mulmod(u,ci,m[i],u);
      mp_mul(u,p,u);
      mp_add(x,u,x);
    end;
    mp_clear3(u,p,ci);
  end;
end;


{---------------------------------------------------------------------------}
procedure mp_crt_solve(n: integer; const m,c,v: array of mp_int; var x: mp_int);
  {-Calculate x with x mod m[i] = v[i], pairwise co-prime moduli m[i], i=0..n-1.}
  { Coefficients c[i] must be precalculated with mp_crt_setup}
var
  u,p: mp_int;
  i: integer;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init_multi(m) or mp_not_init_multi(c) or mp_not_init_multi(v) or mp_not_init(x) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_crt_solve');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  dec(n);
  if (n<0) or (n>high(m)) or (n>high(c)) or (n>high(v)) then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mp_crt_solve: invalid n');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;

  {start with x = v[0] mod m[0]}
  mp_mod(v[0],m[0],x);
  if n=0 then exit; {trivial case, don't use local mp_ints}

  mp_init2(u,p);
  if mp_error=MP_OKAY then begin
    {Step 2 of Garner's algorithm for CRT, HAC Alg. 14.71}
    {already computed: mp_mod(v[0],m[0],x)}
    {p accumulates the product of the m[i]}
    mp_set1(p);
    {Step 3}
    for i:=1 to n do begin
      {p = product(m[j], j=0..i-1)}
      mp_mul(p,m[i-1],p);
      mp_submod(v[i],x,m[i],u);
      mp_mulmod(u,c[i],m[i],u);
      mp_mul(u,p,u);
      mp_add(x,u,x);
    end;
    mp_clear2(u,p);
  end;
end;


{---------------------------------------------------------------------------}
procedure mp_coshmult(const a,b,c: mp_int; var d: mp_int);
  {-Forster's coshmult function, d=mod_coshmult(a,b,c), a>=0, b>=0, c>0}
var
  mc: mp_int;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(b) or mp_not_init(c) or mp_not_init(c) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_coshmult');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  mp_init(mc);
  if mp_error=MP_OKAY then begin
    {Setup Barrett reduction}
    mp_reduce_setup(mc, c);
    s_mp_coshmult(a,b,c,mc,d);
    mp_clear(mc);
  end;
end;


{---------------------------------------------------------------------------}
procedure mp_dfact(n: word; var a: mp_int);
  {-calculate double factorial a = n!!, a=n*(n-2)..., lowest term 1 or 2}
  { Note: for protected mode code n=$FFFF is near the maximum value, but}
  { for real mode BP7 one might get a heap overflow even for smaller n!}
const
  {double factorial for small arguments}
  sdfact: array[0..19] of longint =
            (1,1,2,3,8,15,48,105,384,945,3840,10395,46080,135135,
             645120,2027025,10321920,34459425,185794560,654729075);
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_dfact');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if n<20 then mp_set_int(a,sdfact[n])
  else if odd(n) then mp_OddProd(0,n shr 1,a)
  else begin
    { (2n)!! = 2^n * n!}
    n := n shr 1;
    mp_fact(n,a);
    mp_shl(a,n,a);
  end;
end;


{---------------------------------------------------------------------------}
procedure mp_ecm_factor(const n: mp_int; var f: mp_int; CMax,C1: word; var seed,phase: longint);
  {-Find a factor f of n by inversionless ECM method, f=0 if no factor found.}
  { n must be > 1 and @f<>@n otherwise a runtime error / exception is raised.}
  { Two-phase elliptic curve algorithm, C1 primes are used in phase 1, C1 is}
  { clipped to [ECM_C1Min,ECM_C1Max]. Up to CMax curves (32 if CMax=0), if}
  { input seed=0, mp_random_int is used as initial value. The code is based}
  { on Marcel Martin's NX v0.18/FPC implementation of Algorithm 7.4.4 from}
  { Crandall/Pomerance, incorporating various enhancements of Brent,}
  { Crandall, Montgomery, Woltman, and Zimmermann. WE changes: with Barrett}
  { reduction and intermediate GCDs, 32 bit prime table via config.}
label
  nextcurve, gcd2, final, err1;
const
  D = 200;            {3*(D+2) mp_ints of order n in vectors xx,zz,xz}
type
  TECMVec = array[0..D+1] of mp_int;
  PECMVec = ^TECMVec;
var
  i: integer;
  curve: word;                 {curve counter}
  j,k,p1,p,q,max,kD1,kDBound: longint;
  C2,CC,CP: longint;           {C2 and progress bound calculated from C1}
  an,ad: mp_int;               {constants for curve, both reduced mod n}
  xr,zr,xs,zs: mp_int;         {Q=[xr:zr] in phase 1}
  x0,z0: mp_int;               {used only in ecm_mul}
  mu: mp_int;                  {Barrett parameter for n}
  t1,t2,t3: mp_int;            {temporary}
  xx,zz,xz: PECMVec;           {Phase 2 vectors}
  fs: mp_digit;                {small factor}
  allocated: boolean;          {Phase 2 vectors allocated and initialized}
  cancel: boolean;             {Progress cancel flag}

{$ifdef MPC_ECM_Primetable}
const
  C2Max = ECM_C1Max*100;  {Prime[C2Max] should be smaller than 2^31}
type
  TPrimeTable = array[1..C2MAX] of longint;
var
  ppt: ^TPrimeTable;
{$endif}

  {----------------------------------------------}
  procedure ecm_double(var x,z: mp_int);
    {-double the point: (x,z) := 2*(x,z)}
    { assume input in [0,n), return output in [0,n)}
  begin
    {t1 := (x + z)^2}
    mp_add(x,z,t1);        if mp_cmp(n,t1)<>MP_GT then mp_sub(t1,n,t1);
    mp_sqr(t1,t1);         mp_reduce(t1,n,mu);
    {t2 := (x - z)^2}
    mp_sub(x,z,t2);        {no need to make positive (x-z)^ < n^2}
    mp_sqr(t2,t2);         mp_reduce(t2,n,mu);
    {t3 := t1 - t2}
    mp_sub(t1,t2,t3);      if t3.sign=MP_NEG then mp_add(t3,n,t3);
    {t2 := 4*t2}
    mp_mul_2(t2,t2);       if mp_cmp(n,t2)<>MP_GT then mp_sub(t2,n,t2);
    mp_mul_2(t2,t2);       if mp_cmp(n,t2)<>MP_GT then mp_sub(t2,n,t2);
    mp_mul(t1,t2,x);       mp_reduce(x,n,mu);
    {x := t1*t2*ad}
    mp_mul(x,ad,x);        mp_reduce(x,n,mu);
    mp_mul(t2,ad,t2);      mp_reduce(t2,n,mu);
    {t1 := t3*an}
    mp_mul(t3,an,t1);      mp_reduce(t1,n,mu);
    {z := (t2+t1)*t3}
    mp_add(t2,t1,z);       if mp_cmp(n,z)<>MP_GT then mp_sub(z,n,z);
    mp_mul(z,t3,z);        mp_reduce(z,n,mu);
  end;


  {----------------------------------------------}
  procedure ecm_add(var x2,z2,x1,z1,xd,zd: mp_int);
    {-Add (x2,z2) = (x2,z2) + (x1,z1),  with (xd,zd) = (x2,z2) - (x1,z1)}
    { assume input in [0,n), return output in [0,n)}
  begin
    {t1 := x1-z1}
    mp_sub(x1,z1,t1);  if t1.sign=MP_NEG      then mp_add(t1,n,t1);
    {t2 := x2+z2}
    mp_add(x2,z2,t2);  if mp_cmp(n,t2)<>MP_GT then mp_sub(t2,n,t2);
    {t2 := (x2+z2)(x1-z1)}
    mp_mul(t2,t1,t2);  mp_reduce(t2,n,mu);
    {t1 := x1+z1}
    mp_add(x1,z1,t1);  if mp_cmp(n,t1)<>MP_GT then mp_sub(t1,n,t1);
    {t3 := x2-z2}
    mp_sub(x2,z2,t3);  if t3.sign=MP_NEG      then mp_add(t3,n,t3);
    {t1 := (x1+z1)(x2-z2)}
    mp_mul(t1,t3,t1);  mp_reduce(t1,n,mu);
    {x+ := (x2+z2)(x1-z1)+(x1+z1)(x2-z2) = 2(x1x2-z1z2)}
    mp_add(t2,t1,x2);  if mp_cmp(n,x2)<>MP_GT then mp_sub(x2,n,x2);
    mp_sqr(x2,x2);     mp_reduce(x2,n,mu);
    {x+ := 4zd*(x1x2-z1z2)^2}
    mp_mul(x2,zd,x2);  mp_reduce(x2,n,mu);
    {z+ := (x2+z2)(x1-z1)-(x1+z1)(x2-z2) = 2(x1z2-x2z1)}
    mp_sub(t2,t1,z2);  {no need to make positive (t2-t1)^2 < n^2}
    mp_sqr(z2,z2);     mp_reduce(z2,n,mu);
    {z+ := 4xd*(x1z2-x2z1)^2}
    mp_mul(z2,xd,z2);  mp_reduce(z2,n,mu);
    {cf C/P [10] (7.6), with B=0, A=1. Factors 4 are irrelevant for x/z}
  end;

  {----------------------------------------------}
  procedure ecm_mul_int(var x,z: mp_int; e: longint);
    {-Elliptic multiplication (Montgomery method) (x,y) := e*(x,y), e>0}
    { assume input x,z in (0,n), return output x,z in [0,n)}
  var
    c: longint;
  begin
    if e > 2 then begin
      mp_copy(x,x0);
      mp_copy(z,z0);
      mp_copy(x,xs);
      mp_copy(z,zs);
      ecm_double(xs,zs);
      c := longint($40000000);
      while (c and e) = 0 do c := c shr 1;
      c := c shr 1;
      repeat
        if (e and c) = 0 then begin
          ecm_add(xs,zs,x,z,x0,z0);
          ecm_double(x,z);
        end
        else begin
          ecm_add(x,z,xs,zs,x0,z0);
          ecm_double(xs,zs);
        end;
        c := c shr 1;
      until c = 0;
    end
    else if e=2 then ecm_double(x,z);
    {else if e=1 return x,z unchanged}
  end;

  {----------------------------------------------}
  procedure ecm_setup(var seed: longint);
    {-Calculate ad,an from random seed in [6,n-1], and point Q on curve}
    { C = an/ad - 2  mod n, from curve y^2 = x^3 + Cx^2 + x)}
    { Q : a point on the curve, with coordinates [xr:zr]}
  begin
    inc(seed);
    if (seed<2) or (seed=5) then seed := 6;

    {initial point}
    mp_set_int(xs, seed);
    mp_sqr(xs,xs);
    mp_sub_d(xs,5,xs);        {u  = s^2-5}

    mp_set_int(zs, seed);
    mp_shl(zs,2,zs);          {v  = 4s}

    mp_exptmod_d(xs,3,n,xr);  {xr = u^3}
    mp_exptmod_d(zs,3,n,zr);  {zr = v^3}

    {coef C = an/ad - 2}
    mp_sub(zs,xs,an);
    mp_exptmod_d(an,3,n,an);  { (v-u)^3}
    mp_mul_d(xs,3,xs);
    mp_add(xs,zs,ad);
    mp_mulmod(an,ad,n,an);    { (v-u)^3*(3u+v)}

    mp_shl(zs,2,zs);
    mp_mulmod(xr,zs,n,ad);    { 4*u^3*v}
  end;

begin

  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(n) or mp_not_init(f)  then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_ecm_factor');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {Check n>1 and @n<>@f}
  if mp_cmp_d(n,2)=MP_LT then begin
    {$ifdef MPC_HaltOnError}
        {$ifdef MPC_UseExceptions}
          raise MPXBadArg.Create('mp_ecm_factor: n < 2');
        {$else}
          RunError(MP_RTE_BADARG);
        {$endif}
    {$else}
      set_mp_error(MP_BADARG);
      exit;
    {$endif}
  end;
  if @n=@f then begin
    {$ifdef MPC_HaltOnError}
        {$ifdef MPC_UseExceptions}
          raise MPXBadArg.Create('mp_ecm_factor: @n=@f');
        {$else}
          RunError(MP_RTE_BADARG);
        {$endif}
    {$else}
      set_mp_error(MP_BADARG);
      exit;
    {$endif}
  end;

  {check small factor up to 127}
  mp_small_factor(n,2,127,fs);
  if fs<>0 then begin
    mp_set(f,fs);
    exit;
  end;

  if C1<800  then C1 := 800;
  if C1>4000 then C1 := 4000;
  C2 := 100*C1;

  if CMax=0 then CMax := 32;
  curve := 0;

  {Not yet allocated, set pointers to nil. Allocation and initialization}
  {of dynamic arrays will be done if phase 2 is entered the first time}
  allocated := false;
  xx := nil;
  zz := nil;
  xz := nil;
  {$ifdef MPC_ECM_Primetable}
    ppt := nil;
  {$endif}

  {initialize local mp_ints and calculate Barrett parameter for n}
  mp_init6(xr,zr,xs,zs,an,ad); if mp_error<>0 then exit;
  mp_init6(x0,z0,t1,t2,t3,mu); if mp_error<>0 then goto err1;
  mp_reduce_setup(mu,n);

  {generate seed for first curve, use random if not in (5,n)}
  while (seed<2) or (seed=5) or not (mp_cmp_int(n,seed)=MP_GT) do begin
    if mp_bitsize(n)<31 then seed := 6 + mp_random_digit mod (MP_DIGIT_MAX-6)
    else seed := 6+mp_random_int;
    if mp_error<>MP_OKAY then goto final;
  end;

  {Marcel Martin uses a prime table of C2 longints. Here the Primes[j]}
  {can be generated via next/prevprime32 as a space/time tradeoff:}
  {Always for 16 bit, used for 32 bit if MPC_ECM_Primetable is not defined.}

  {p1 := Prime[C1] (6133 for C1=800, 13499 for C1=2*800}
  p1 := 1;
  for i:=1 to C1 do p1 := nextprime32(p1+1);

nextcurve:

  mp_set1(f);

  inc(curve);
  ecm_setup(seed);

  {---------------------------}
  {---- Perform phase 1 ------}
  {---------------------------}

  phase := 1;
  q := 1;
  CP:= C1 div 5; {Progress call and check GCD five times in phase 2}
  CC:= CP;
  {loop over primes}
  for i:=1 to C1 do begin
    if mp_error<>MP_OKAY then goto final;
    p := nextprime32(q+1);
    q := p;
    {find max exponent max with p^max<B1=Prime[C1], p=Pime[i]}
    max := p1 div p;
    while p <= max do p := p*q;
    {Q := [p^max]*Q}
    ecm_mul_int(xr,zr,p);
    {Check if cancel, no progress indicator}
    if odd(i) and (ProgressAssigned) then begin
      cancel := false;
      mp_progress(true, 0, C1, cancel);
      if cancel then begin
        mp_zero(f);
        goto final;
      end;
    end;
    if i>CC then begin
      inc(CC,CP);
      if mp_show_progress then begin
        {show progress and check for cancel}
        if ProgressAssigned then begin
          cancel := false;
          mp_progress(false, 1, C1, cancel);
          if cancel then begin
            mp_zero(f);
            goto final;
          end;
        end
        else write('.');
      end;
      {WE: check non-trivial GCD after CP primes, this is not done in C/P/M}
      mp_gcd(zr,n,f);
      if (mp_cmp_d(f,1)=MP_GT) and mp_is_lt(f,n) then goto final;
    end;
  end;

  {gcd(z,n)}
  if not mp_gcd1(zr,n,f) then begin
    {GCD not 1, test other trivial case and exit if non-trivial}
    if mp_is_eq(f,n) then begin
      if curve<CMAX then goto nextcurve  {try next curve}
      else mp_zero(f); {indicate failure}
    end;
    goto final;
  end;

  {---------------------------}
  {---- Perform phase 2 ------}
  {---------------------------}

  phase := 2;
  if not allocated then begin
    allocated := true;
    {$ifdef MPC_ECM_Primetable}
      ppt := IAlloc(C2*sizeof(longint));
      nextprime32_array(1,C2,ppt^);
    {$endif}
    xx := mp_getmem(sizeof(TECMVec));
    zz := mp_getmem(sizeof(TECMVec));
    xz := mp_getmem(sizeof(TECMVec));
    if xx<>nil then mp_init_multi(xx^);
    if zz<>nil then mp_init_multi(zz^);
    if xz<>nil then mp_init_multi(xz^);
  end;

  k := (p1+1) div D;
  mp_copy(xr,xx^[0]);
  mp_copy(zr,zz^[0]);
  kD1 := k*D+1;
  ecm_mul_int(xx^[0],zz^[0],kD1);

  i := D+1;
  mp_copy(xr,xx^[i]);
  mp_copy(zr,zz^[i]);
  ecm_mul_int(xx^[i],zz^[i],kD1+D+D);

  {i=1}
  mp_copy(xr,xx^[1]);
  mp_copy(zr,zz^[1]);
  ecm_double(xx^[1],zz^[1]);
  mp_mul(xx^[1],zz^[1],xz^[1]);
  mp_reduce(xz^[1],n, mu);

  {i=2}
  mp_copy(xx^[1],xx^[2]);
  mp_copy(zz^[1],zz^[2]);
  ecm_double(xx^[2],zz^[2]);
  mp_mul(xx^[2],zz^[2],xz^[2]);
  mp_reduce(xz^[2],n,mu);

  for i:=3 to D do begin
    if mp_error<>MP_OKAY then goto final;
    mp_copy(xx^[i-1],xx^[i]);
    mp_copy(zz^[i-1],zz^[i]);
    ecm_add(xx^[i],zz^[i],xx^[1],zz^[1],xx^[i-2],zz^[i-2]);
    mp_mul(xx^[i],zz^[i],xz^[i]);
    mp_reduce(xz^[i],n,mu);
  end;

  CP:= C2 div 5; {Progress call and check GCD five times in phase 2}
  CC:= CP;
  j := C1+1;
  {$ifdef MPC_ECM_Primetable}
    if (ppt<>nil) and (j<=C2) then p:=ppt^[j]
    else p := nextprime32(p1+1);
  {$else}
    p := nextprime32(p1+1);
  {$endif}

  repeat
    if mp_error<>MP_OKAY then goto final;
    mp_mul(xx^[0],zz^[0],xz^[0]);
    mp_reduce(xz^[0],n,mu);
    kD1 := k*D + 1;
    if p=kD1 then begin
      if j=C2 then goto gcd2;
      inc(j);
      {$ifdef MPC_ECM_Primetable}
        if (ppt<>nil) and (j<=C2) then p:=ppt^[j]
        else p := nextprime32(p+1);
      {$else}
        p := nextprime32(p+1);
      {$endif}
    end;
    kDBound := kD1 + D + D - 2;
    repeat
      if mp_error<>MP_OKAY then goto final;
      i := (p-kD1) shr 1;
      {accumulate (xx0 - xxi)(zz0 + zzi) - xx0*zz0 + xxi*zzi}
      mp_sub(xx^[0],xx^[i],t1);   if t1.sign=MP_NEG      then mp_add(t1,n,t1);
      mp_add(zz^[0],zz^[i],t2);   if mp_cmp(n,t2)<>MP_GT then mp_sub(t2,n,t2);
      mp_mul(t2,t1,t2);           mp_reduce(t2,n,mu);
      mp_sub(t2,xz^[0],t2);       if t2.sign=MP_NEG then mp_add(t2,n,t2);
      mp_add(t2,xz^[i],t2);       if mp_cmp(n,t2)<>MP_GT then mp_sub(t2,n,t2);
      mp_mul(f,t2,f);             mp_reduce(f,n,mu);
      if j=C2 then goto gcd2;
      inc(j);
      {$ifdef MPC_ECM_Primetable}
        if (ppt<>nil) and (j<=C2) then p:=ppt^[j]
        else p := nextprime32(p+1);
      {$else}
        p := nextprime32(p+1);
      {$endif}
    until p > kDBound;

    if ProgressAssigned then begin
      cancel := false;
      if j>CC then begin
        mp_progress(false, j, C2, cancel);
        inc(CC,CP);
        if not mp_gcd1(f,n,t1) and not mp_is_eq(f,t1) then begin
          {non-trivial intermediate GCD found, set factor and leave}
          mp_exch(f,t1);
          goto final;
        end;
      end
      else mp_progress(true, j, C2, cancel);
      if cancel then begin
        mp_zero(f);
        goto final;
      end;
    end;

    inc(k,2);
    i := D+1;
    mp_copy(xx^[i],xs);
    mp_copy(zz^[i],zs);
    ecm_add(xx^[i],zz^[i],xx^[i-1],zz^[i-1],xx^[0],zz^[0]);
    mp_exch(xx^[0],xs);
    mp_exch(zz^[0],zs);
  until false;

gcd2:

  mp_gcd(f,n,f);
  {done if non-trivial GCD}
  if (mp_cmp_d(f,1)=MP_GT) and mp_is_lt(f,n) then goto final;

  {Try another curve if maximum curve count not exceeded}
  if curve < CMAX then goto nextcurve;

  {indicate failure}
  mp_zero(f);

final:

  {"finally" part, deallocate and clear resources}
  if allocated then begin
    {$ifdef MPC_ECM_Primetable}
      freemem(pointer(ppt),C2*sizeof(longint));
    {$endif}
    if xx<>nil then begin
      mp_clear_multi(xx^);
      mp_freemem(pointer(xx),sizeof(TECMVec));
    end;
    if zz<>nil then begin
      mp_clear_multi(zz^);
      mp_freemem(pointer(zz),sizeof(TECMVec));
    end;
    if xz<>nil then begin
      mp_clear_multi(xz^);
      mp_freemem(pointer(xz),sizeof(TECMVec));
    end;
  end;
  mp_clear6(x0,z0,t1,t2,t3,mu);

err1:
  mp_clear6(xr,zr,xs,zs,an,ad);

end;


{---------------------------------------------------------------------------}
procedure mp_ecm_simple(const n: mp_int; var f: mp_int; seed: longint);
  {-Simplified version of mp_ecm_factor with CMax=0, C1=ECM_C1Min}
var
  phase: longint;
begin
  mp_ecm_factor(n,f,0,2*ECM_C1Min,seed,phase);
end;


{---------------------------------------------------------------------------}
procedure mp_exptmod_win(const g,e,p: mp_int; var b: mp_int; redmode: TRedType);
  {-Internal: Compute y=g^|e| mod p, p>0, internal sliding windows}
label
  __M, __MU, __RES;
var
  bitbuf, bitcpy, bitcnt, mode, digidx, x, y, winsize, wmax1,wmax2: integer;
  ps2: word;
  buf: mp_digit;
  mp : mp_digit;
  {$ifdef MPC_Reduce_2k}
  d2k: mp_digit;
  {$endif}
  bc: longint;
  res, mu: mp_int;
  M: array[1..WEXP_TABSIZE] of mp_int;

  {---------------------------------------------}
  procedure Gen_Redux(var mpi: mp_int);
    {-General modular reduction of mpi driven by redmode}
  begin
    case redmode of
      MR_Montgomery: mp_montgomery_reduce(mpi, p, mp);
          MR_Barret: mp_reduce(mpi, p, mu);
    {$ifdef MPC_Reduce_2k}
        MR_Reduce2k: begin
                       mp_reduce_2k(mpi, p, d2k);
                     end;
    {$endif}
               else  if mp_error=MP_OKAY then set_mp_error(MP_BADARG);
    end;
  end;

begin
  {Uses a left-to-right k-ary sliding window to compute the modular exponentiation}
  {The value of k changes based on the size of the exponent.}
  if mp_error<>MP_OKAY then exit;
  {No checks}
  {find window size}
  bc := mp_bitsize(e);
  if bc<=7 then winsize := 2
  else if bc<=36 then winsize := 3
  else if bc<=140 then winsize := 4
  else if bc<=450 then winsize := 5
  else if bc<=1303 then winsize := 6
  else if bc<=3529 then winsize := 7
  else winsize := 8;

  if winsize>WEXP_MAX then winsize := WEXP_MAX;
  wmax1 := 1 shl (winsize-1);
  wmax2 := 1 shl winsize;

  ps2 := 2*(p.used+1);
  {initialize M array}
  {initialize first cell}
  mp_init_size(M[1],ps2);
  if mp_error<>MP_OKAY then exit;

  {now initialize the second half of the array}
  for x:=wmax1 to wmax2-1 do begin
    mp_init_size(M[x],ps2);
    if mp_error<>MP_OKAY then begin
      for y:=wmax2 to x-1 do mp_clear(M[y]);
      mp_clear(M[1]);
      exit;
    end;
  end;

  {create mu, used for Barrett reduction}
  mp_init_size(mu,ps2);
  if mp_error<>MP_OKAY then goto __M;

  {setup result}
  mp_init(res);
  if mp_error<>MP_OKAY then goto __MU;

  {Do initial setup depending on reduction type}
  if Redmode=MR_Montgomery then begin
    mp_montgomery_setup(p, mp);
    mp_montgomery_calcnorm(res, p);
    {now set M[1] to G * R mod m}
    mp_mulmod(g, res, p, M[1]);
  end
  else begin
    {The M table contains powers of the base, }
    {e.g. M[x] = g^x mod p                   }
    mp_set(res, 1);
    mp_mod(g, p, M[1]);
    if Redmode=MR_Barret then mp_reduce_setup(mu, p)
  {$ifdef MPC_Reduce_2k}
    else if Redmode=MR_Reduce2k then begin
      mp_reduce_2k_setup(p, d2k)
    end
  {$endif}
    else begin
      {unsupported reduction}
      set_mp_error(MP_BADARG);
    end;
  end;
  if mp_error<>MP_OKAY then goto __RES;

  {Create M table                           }
  {The first half of the table is not       }
  {computed though accept for M[0] and M[1] }

  {compute the value at M[wmax1] by squaring M[1] (winsize-1) times}
  mp_copy(M[1], M[wmax1]);
  for x:=0 to winsize-2 do begin
    mp_sqr(M[wmax1], M[wmax1]);
    Gen_Redux(M[wmax1]);
    if mp_error<>MP_OKAY then goto __RES;
  end;

  {create upper table, that is M[x] = M[x-1] * M[1] (mod p)}
  for x:=wmax1+1 to wmax2-1 do begin
    mp_mul(M[x-1], M[1], M[x]);
    Gen_Redux(M[x]);
    if mp_error<>MP_OKAY then goto __RES;
  end;

  {set initial mode and bit cnt}
  mode   := 0;
  bitcnt := 1;
  buf    := 0;
  digidx := integer(e.used) - 1;
  bitcpy := 0;
  bitbuf := 0;

  repeat
    if mp_error<>MP_OKAY then goto __RES;
    {grab next digit as required }
    dec(bitcnt);
    if bitcnt=0 then begin
      {if digidx = -1 we are out of digits}
      if digidx = -1 then break;
      {read next digit and reset the bitcnt}
      buf    := e.pdigits^[digidx]; dec(digidx);
      bitcnt := DIGIT_BIT;
    end;

    {grab the next msb from the exponent}
    y := (buf shr mp_digit(DIGIT_BIT - 1)) and 1;

    {temporarly turn range checks off}
    {$R-}
    buf := buf shl 1;
    {$ifdef RangeChecks_on} {$R+} {$endif}

    {if the bit is zero and mode = 0 then we ignore it             }
    {These represent the leading zero bits before the first 1 bit  }
    {in the exponent.  Technically this opt is not required but it }
    {does lower the # of trivial squaring/reductions used          }

    if (mode=0) and (y=0) then continue;

    {if the bit is zero and mode == 1 then we square}
    if (mode=1) and (y=0) then begin
      mp_sqr(res, res);
      Gen_Redux(res);
      if mp_error<>MP_OKAY then goto __RES;
      continue;
    end;

    {else we add it to the window}
    inc(bitcpy);
    bitbuf := bitbuf or (y shl (winsize - bitcpy));
    mode   := 2;

    if bitcpy=winsize then begin
      {ok window is filled so square as required and multiply}
      {square first}
      for x:=0 to winsize-1 do begin
        mp_sqr(res, res);
        Gen_Redux(res);
        if mp_error<>MP_OKAY then goto __RES;
      end;

      {then multiply}
      mp_mul(res, M[bitbuf], res);
      {and reduce}
      Gen_Redux(res);
      if mp_error<>MP_OKAY then goto __RES;

      {empty window and reset}
      bitcpy := 0;
      bitbuf := 0;
      mode   := 1;
    end;
  until false;

  {if bits remain then square/multiply}
  if (mode=2) and (bitcpy > 0) then begin
    {square then multiply if the bit is set}
    for x:=0 to bitcpy-1 do begin
      mp_sqr(res, res);
      Gen_Redux(res);
      if mp_error<>MP_OKAY then goto __RES;
      bitbuf := bitbuf shl 1;
      if (bitbuf and wmax2) <> 0 then begin
        {then multiply}
        mp_mul(res, M[1], res);
        {and reduce}
        Gen_Redux(res);
        if mp_error<>MP_OKAY then goto __RES;
      end;
    end;
  end;

  if Redmode=MR_Montgomery then begin
    {fix result if Montgomery reduction is used recall that any value}
    {in a Montgomery system is actually multiplied by R mod n.  So we}
    {have to reduce one more time to cancel out the factor of R.     }
    mp_montgomery_reduce(res, P, mp);
    if mp_error<>MP_OKAY then goto __RES;
  end;

  mp_exch(res, b);

__RES: mp_clear(res);
 __MU: mp_clear(mu);
  __M: mp_clear(M[1]);

  for x:=wmax1 to wmax2-1 do mp_clear(M[x]);

end;


{---------------------------------------------------------------------------}
procedure mp_exptmod(const a,b,c: mp_int; var d: mp_int);
  {-Compute d = a^b mod c, c>0. If b<0, a must have an inverse mod c}
var
  rt: TRedType;
  t: array[0..1] of mp_int; {a:t[0], b:t[1]}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(b) or mp_not_init(c) or mp_not_init(d) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_exptmod');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {modulus c must be positive}
  if s_mp_is_le0(c) then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mp_exptmod: c<=0');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;

  {if exponent b is negative we have to recourse}
  if b.sign=MP_NEG then begin
     {first compute 1/a mod c}
     mp_init_multi(t);
     if mp_error=MP_OKAY then begin
       mp_invmod(a, c, t[0]);
       {now get |b|}
       mp_abs(b, t[1]);
       {and now compute (1/a)^|b| instead of a^b [b < 0]}
       mp_exptmod(t[0], t[1], c, d);
       mp_clear_multi(t);
     end;
     exit;
  end;

  {easy outs}
  if mp_is1(c) then begin
    mp_zero(d);
    exit;
  end;
  if mp_is1(b) then begin
    mp_mod(a,c,d);
    exit;
  end;

  {Default: Barrett reduction}
  rt := MR_Barret;
  if mp_isodd(c) then rt := MR_Montgomery;

{$ifdef MPC_Reduce_2k}
  if mp_reduce_is_2k(c) then rt := MR_Reduce2k;
  {*tbd: DR module variants}
{$endif}

  {Use sliding window routine to compute the modular exponentiation}
  mp_exptmod_win(a, b, c, d, rt)

end;


{---------------------------------------------------------------------------}
procedure mp_exptmod_d(const a: mp_int; b: longint; const c: mp_int; var d: mp_int);
  {-Compute d = a^b mod c, c>0, b longint. If b<0, a must have an inverse mod c}
var
  tmp: mp_int;
begin
  if mp_error<>MP_OKAY then exit;
  {arg check in mp_exptmod}
  mp_init_set_int(tmp, b);
  if mp_error=MP_OKAY then begin
    mp_exptmod(a,tmp,c,d);
    mp_clear(tmp);
  end;
end;


{---------------------------------------------------------------------------}
procedure mp_fact(n: word; var a: mp_int);
  {-calculate a = factorial(n) using Recursive Split, error if n > MaxFact}
const
  MAXL = 14;
var
  np : word;                     {Luschny's "private long N"}
  st : array[0..MAXL] of mp_int; {local 'stack', avoids init/clear in product}
  lev: integer;                  {stack level >=2; 0,1 for local mp_ints}

  procedure product(var p: mp_int; l: word);
    {-recursive product of odd numbers (depends on np and calling sequence)}
  const
    lmin={$ifdef BIT16}8{$else}32{$endif}; {min value of l for recursion}
  begin
    if mp_error<>MP_OKAY then exit;
    if l<lmin then begin
      inc(np,2);
      mp_set_w(p, np);
      {use while, this allows variable limits}
      while l>1 do begin
        inc(np, 2);
        mp_mul_w(p, np, p);
        dec(l);
      end;
    end
    else begin
      if lev>=MAXL then begin
        {$ifdef MPC_HaltOnError}
          {$ifdef MPC_UseExceptions}
            raise MPXRange.Create('mp_fact: lev out of range');
          {$else}
            RunError(MP_RTE_RANGE);
          {$endif}
        {$else}
          set_mp_error(MP_RANGE);
          exit;
        {$endif}
      end;
      inc(lev);
      product(p, l shr 1);
      if mp_error=MP_OKAY then begin
        product(st[lev], l-(l shr 1));
        mp_mul(p, st[lev], p);
      end;
      dec(lev);
    end;
  end;

var
  i,high,len: word;

const
  {factorial for small arguments}
  sfact: array[0..12] of longint = (1,1,2,6,24,120,720,5040,40320,362880,
                                    3628800,39916800,479001600);
begin
  {Based of P.Luschny's FactorialSplitRecursive.java, available at}
  {http://www.luschny.de/math/factorial/FastFactorialFunctions.htm}
  {See also: Ch. 3 - Factorization of the factorial, available as }
  {http://www.cs.ru.nl/aio-info/FormerPhD/EerkeBoitenThesis.ps.Z  }
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_fact');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  if n <= 12 then mp_set_int(a, sfact[n])
  else begin
    {initialize "private long N"}
    np := 1;
    {Create and initialize local mp_ints. Note that using the stack is an}
    {improvement only for WIN32 (up to 30%) where memory allocation seems}
    {to be a bit time consuming. No improvement for BP7 and FPC/go32v2.}
    mp_init_multi(st);
    if mp_error<>MP_OKAY then exit;
    mp_set(st[0], 1);
    mp_set(st[1], 1);

    high := 1;
    for i:=bitsize32(n)-2 downto 0 do begin
      len  := high;
      high := n shr i;
      if not odd(high) then dec(high);
      len := (high-len) shr 1;
      if len>0 then begin
        {initialize stack level, product uses levels lev+1,...}
        lev := 1;
        {use a for product result}
        product(a,len);
        mp_mul(st[0],a,st[0]);
        mp_mul(st[1],st[0],st[1]);
      end;
    end;
    {Luschny's final shift is n - popcount(n)}
    mp_shl(st[1], n - popcount16(n), a);
    mp_clear_multi(st);
  end;
end;


{---------------------------------------------------------------------------}
procedure mp_fermat(n: word; var fn: mp_int);
  {-return nth Fermat number, fn = 2^(2^n)+1 (MP_RANGE error for n>MaxFermat)}
begin
  if mp_error<>MP_OKAY then exit;
  {Check n because 1 shl n is calculated as 1 shl (n mod 32) for n>31}
  if n>MaxFermat then begin
    {$ifdef MPC_HaltOnArgCheck}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mp_fermat: n too large');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;
  {Init check in mp_2expt}
  mp_2expt(fn, longint(1) shl n);
  if mp_error=MP_OKAY then begin
    fn.pdigits^[0] := fn.pdigits^[0] or 1;
  end;
end;


{---------------------------------------------------------------------------}
procedure mp_fib2(n: longint; var fn,f1: mp_int);
  {-calculate two Fibonacci numbers fn=fib(n), f1=fib(n-1), n>=0}
var
  t: mp_int;
  bk: longint;
{$ifdef MPC_ArgCheck}
const
  toobig = trunc(MaxMersenne*1.5);
{$endif}
begin
  if mp_error<>MP_OKAY then exit;

  {$ifdef MPC_ArgCheck}
    if mp_not_init(fn) or mp_not_init(f1) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_fib2');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
    if n<0 then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXRange.Create('mp_fib2: n<0');
        {$else}
          RunError(MP_RTE_RANGE);
        {$endif}
      {$else}
        set_mp_error(MP_RANGE);
        exit;
      {$endif}
    end;
    if n>toobig then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXRange.Create('mp_fib2: n too large');
        {$else}
          RunError(MP_RTE_RANGE);
        {$endif}
      {$else}
        set_mp_error(MP_RANGE);
        exit;
      {$endif}
    end;
    if @fn=@f1 then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXBadArg.Create('mp_fib2: @fn=@f1');
        {$else}
          RunError(MP_RTE_BADARG);
        {$endif}
      {$else}
        set_mp_error(MP_BADARG);
        exit;
      {$endif}
    end;
  {$endif}

  {initialize f1=0, fn=1}
  mp_zero(f1);
  mp_set1(fn);
  {easy out for n=0,1}
  if n=0 then mp_exch(fn,f1);
  if n<2 then exit;

  {create temporary variable}
  mp_init(t);

  {fn=F[n] and f1=F[n-1] are calculated simultaneously using a}
  {"left-to-right" binary algorithm from high to low bits of n}
  {   F[k+1] =   F[k]   + F[k-1],  F[0]=1, F[1]=1             }
  {  F[2k+1] = 4*F[k]^2 - F[k-1]^2 + 2*(-1)^k                 }
  {  F[2k-1] =   F[k]^2 + F[k-1]^2                            }
  {  F[2k]   = F[2k+1]  - F[2k-1]                             }

  {get highest bit of n, loop terminates because n>0}
  bk := longint($40000000);
  while n and bk = 0 do bk := bk shr 1;

  while bk>1 do begin
    {bk is the highest unprocessed bit}
    {f1 = F[k-1]^2}
    mp_sqr(f1,f1);
    {t  = F[k]^2  }
    mp_sqr(fn,t);

    {F[2k+1] = 4*F[k]^2 - F[k-1]^2 + 2*(-1)^k}
    {     fn =    4*t   -     f1   + 2*(-1)^k}
    mp_shl(t,2,fn);
    mp_sub(fn,f1,fn);
    {add 2*(-1)^k, note: mp_digits are positive so use add_d or sub_d}
    if n and bk = 0 then mp_add_d(fn,2,fn) else mp_sub_d(fn,2,fn);

    {F[2k-1] = F[k]^2 + F[k-1]^2}
    {     f1 =    t   +     f1  }
    mp_add(t,f1,f1);

    {Get next lower bit of n. If it is 0 then F[2k] and F[2k-1]}
    {are used, otherwise F[2k+1] and F[2k] are used}
    bk := bk shr 1;
    if n and bk = 0 then begin
      {f1 = F[2k-1], fn = F[2k]  }
      mp_sub(fn,f1,fn);
    end
    else begin
      {f1 = F[2k]),  fn = F[2k+1]}
      mp_sub(fn,f1,f1);
    end;
  end;
  mp_clear(t);
end;


{---------------------------------------------------------------------------}
procedure mp_fib(n: longint; var fn: mp_int);
  {-calculate Fibonacci number fn=fib(n), fib(-n)=(-1)^(n-1)*fib(n)}
var
  f1: mp_int;
const
  smfib: array[0..24] of word = (0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,
                                 1597,2584,4181,6765,10946,17711,28657,46368);
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(fn) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_fib');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if abs(n)<25 then mp_set_w(fn,smfib[abs(n)])
  else begin
    {create temporary variable for fib(n-1)}
    mp_init(f1);
    if mp_error=MP_OKAY then begin
      mp_fib2(abs(n),fn,f1);
      mp_clear(f1);
    end;
  end;
  {adjust sign if n negative}
  if (n<0) and not odd(n) then mp_chs(fn,fn);
end;


{---------------------------------------------------------------------------}
procedure mp_gcd(const a,b: mp_int; var c: mp_int);
  {-calculate c = gcd(a,b) using the binary method}
var
  u,v: mp_int;
  k,u_lsb,v_lsb: longint;
{$ifdef MPC_UseGCD32}
  ui,vi: longint;
{$endif}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(b) or mp_not_init(c) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_gcd');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {if one arg is zero, gcd is the abs of the other}
  if mp_iszero(a) then begin
    mp_abs(b, c);
    exit;
  end
  else if mp_iszero(b) then begin
    mp_abs(a, c);
    exit;
  end;

  {get copies of a and b we can modify}
  mp_init2(u,v);
  if mp_error<>MP_OKAY then exit;

  {u,v are positive for the remainder of the algorithm}
  mp_abs(a,u);
  mp_abs(b,v);

  if mp_error=MP_OKAY then begin
    {Do a single initial modular reduction if the sizes of u and v}
    {are very different, see H.Cohen's Alg.1.3.5 in [24]}
    if mp_initial_mod then begin
      {u>0, v>0. Make u >= v}
      if (u.used < v.used) or (mp_cmp_mag(u,v)=MP_LT) then mp_exch(u,v);
      if u.used > v.used+1 then begin
        {do the initial reduction}
        mp_mod(u,v,u);
        {done in u mod v = 0}
        if u.used=0 then begin
          {store result and clear local vars}
          mp_exch(c,v);
          mp_clear2(u,v);
          exit;
        end;
        {$ifdef MPC_UseGCD32}
          if mp_is_longint(u,ui) then begin
            {u <>0 but fits into longint}
            mp_mod_int(v,ui,vi);
            mp_set_int(c,GCD32(ui,vi));
            mp_clear2(u,v);
            exit;
          end;
        {$endif}
      end;
    end;

    {Here both u and v are > 0; find the common power of two for u and v}
    u_lsb := mp_cnt_lsb(u);
    v_lsb := mp_cnt_lsb(v);
    if u_lsb<v_lsb then k:=u_lsb else k:=v_lsb;

    {Make both u and v odd}
    if u_lsb>0 then mp_shr(u, u_lsb, u);
    if v_lsb>0 then mp_shr(v, v_lsb, v);

    while (mp_error=MP_OKAY) and (v.used>0) do begin
      {$ifdef MPC_UseGCD32}
        if mp_is_longint(v,vi) then begin
          mp_mod_int(u,vi,ui);
          mp_set_int(u,GCD32(ui,vi));
          break;
        end;
      {$endif}
      {done if u=v, if u>v swap u and v}
      if u.used>=v.used then begin
         case mp_cmp_mag(u,v) of
           MP_GT: mp_exch(u, v);
           MP_EQ: break;
         end;
      end;
      {subtract smaller from larger, resulting v is > 0}
      s_mp_sub(v, u, v);
      if v.pdigits^[0] and 2 <>0 then begin
        {Only one factor two can be removed, so reduce overhead. This}
        {occurs about one-half of the time, see Knuth [3] 4.5.2, p348}
        mp_shr1(v);
      end
      else begin
        {Divide out all factors of two}
        mp_shr(v, mp_cnt_lsb(v), v);
      end;
    end;

    {multiply by 2^k which we divided out at the beginning}
    mp_shl(u, k, c);
  end;
  mp_clear2(u,v);
end;


{---------------------------------------------------------------------------}
function mp_gcd1(const a,b: mp_int; var c: mp_int): boolean;
  {-calculate c = gcd(a,b) using the binary method, return true if c=1 and no error}
begin
  mp_gcd(a,b,c);
  mp_gcd1 := mp_is1(c);
end;


{---------------------------------------------------------------------------}
procedure mp_gcd_euclid(const a,b: mp_int; var c: mp_int);
  {-calculate c = gcd(a,b), non optimized Euclid}
var
  t: mp_int;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(b) or mp_not_init(c) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_gcd_euclid');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {Simple Euclid algorithm, Knuth A}
  mp_init(t);
  if mp_error=MP_OKAY then begin
    {make both mp_ints positive}
    {handle b first (may be @b=@c)}
    mp_abs(b,t);
    mp_abs(a,c);
    {ggc(c,t) = gcd(t, c mod t)}
    while (mp_error=MP_OKAY) and (not mp_iszero(t)) do begin
      mp_mod(c,t,c);
      mp_exch(t,c);
    end;
    mp_clear(t);
  end;
end;


{$ifdef MP_32BIT}
type
  tml_int = longint; {FPC has 16 bit integer/maxint with 32 bit code!!!!!}
  tml_dbl = int64;
const
  Lehmer_MaxSingle = MP_DIGIT_MAX;        {use with mul_d()}
  Lehmer_MaxK      = 2*DIGIT_BIT-1;
{$else}
type
  tml_int = integer;
  tml_dbl = longint;
const
  Lehmer_MaxSingle = $7FFF;               {use with mul_w()}
  Lehmer_MaxK      = 29;
{$endif}


{---------------------------------------------------------------------------}
function Lehmer(const u,v: mp_int; k: integer; var x0,x1,y0,y1: tml_int): boolean;
  {-Single precision Lehmer steps to reduce u,v based on highest k bits of}
  { u,v. Return true if successful and reduction using xi,yi should be done}
var
  hu,hv,hb: mp_word;
  i: integer;
  uh,vh,q,x2,y2: tml_dbl;

begin

  {This is an implementation of Sorenson's Modified Lehmer procedure [28]. }
  {It is based on results of Lehmer, Collins, Jebelean, and Sorenson. The  }
  {procedure performs single digit calculations that allow to combine some }
  {Euclidean GCD steps into a single multiprecision calculation. It returns}
  {Sorenson's x[i], y[i], x_[i-1], and y_[i-1] in x1, y1, x0, and y0. These}
  {values are used in both GCD procedures (standard and extended).         }

  Lehmer := false;

  {Check the technical conditions and exit if they are not fulfilled}
  if (k>Lehmer_MaxK) or (u.sign=MP_NEG) or (v.sign=MP_NEG) or (u.used<2) or (v.used>u.used) or (u.used>v.used+1) then exit;

  {Get the leading two digits of u and v; here v[u.used-1] may be zero.}
  i  := pred(u.used);
  hu := (mp_word(u.pdigits^[i]) shl DIGIT_BIT) + u.pdigits^[pred(i)];
  if v.used<u.used then hv := v.pdigits^[pred(i)]
  else hv := (mp_word(v.pdigits^[i]) shl DIGIT_BIT) + v.pdigits^[pred(i)];

  {Get the highest k bits of u and the corresponding bits of v}
  hb := mp_word(1) shl k;
  i  :=0;
  while hu>=hb do begin
    hu := hu shr 1;
    inc(i);
  end;
  hv := hv shr i;
  if hv=0 then exit;

  i  := 0;
  uh := hu;
  vh := hv;
  x0 := 1;
  y0 := 0;
  x1 := 0;
  y1 := 1;
  repeat
    q  := uh div vh;
    x2 := vh;
    vh := uh - q*vh;
    uh := x2;
    x2 := x0 - q*x1;
    y2 := y0 - q*y1;
    {In addition to the standard break conditions, exit if the next  }
    {new x2/y2 values are greater than the maximum allowed values. If}
    {this happens during the first iteration, Lehmer is still false  }
    if (abs(x2) > Lehmer_MaxSingle) or (abs(y2) > Lehmer_MaxSingle) then break;
    inc(i);
    if odd(i) then begin
      if (vh < -y2) or (uh-vh < x2-x1) then break;
    end
    else begin
      if (vh < -x2) or (uh-vh < y2-y1) then break;
    end;
    x0 := x1;
    x1 := x2;
    y0 := y1;
    y1 := y2;
    Lehmer := true;
  until false;
end;


{---------------------------------------------------------------------------}
procedure mp_gcd_ml(const a,b: mp_int; var u: mp_int);
  {-calculate u = gcd(a,b) using the Sorenson's Modified Lehmer method}
var
  s,t,v: mp_int;
  tu,tv: longint;
  bsu,bsv: longint;
  x0,x1,y0,y1: tml_int;
const
  k=Lehmer_MaxK; k2=(k+1) div 2;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(b) or mp_not_init(u) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_gcd_ml');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  mp_init3(s,t,v);
  if mp_error=MP_OKAY then begin
    {make both mp_ints positive}
    {handle b first (may be @b=@u)}
    mp_abs(b,v);
    mp_abs(a,u);
    {make u>=v, this will be a loop invariant}
    if (u.used<v.used) or (mp_cmp_mag(u,v)=MP_LT) then mp_exch(u,v);

    bsu := mp_bitsize(u);
    bsv := mp_bitsize(v);
    {Iterate until v is in longint range, then used GCD32}
    while (mp_error=MP_OKAY) and (bsv>31) do begin
      if (bsu-bsv <= k2) and Lehmer(u,v,k,x0,x1,y0,y1) then begin
        {calculate new (u, v):  v := x1*u + y1*v;  u := x0*u + y0*v;}
        {$ifdef MP_16BIT}
           {v := x1*u + y1*v}
           mp_mul_w(u,abs(x1),s);  if x1<0 then s_mp_chs(s);
           mp_mul_w(v,abs(y1),t);  if y1<0 then s_mp_chs(t);
           mp_add(s,t,t);
           mp_exch(t,v);
           {u := x0*u + y0*v}
           mp_mul_w(u,abs(x0),s);  if x0<0 then s_mp_chs(s);
           mp_mul_w(t,abs(y0),t);  if y0<0 then s_mp_chs(t);
           mp_add(s,t,u);
        {$else}
           {v := x1*u + y1*v}
           mp_mul_d(u,abs(x1),s);  if x1<0 then s_mp_chs(s);
           mp_mul_d(v,abs(y1),t);  if y1<0 then s_mp_chs(t);
           mp_add(s,t,t);
           mp_exch(t,v);
           {u := x0*u + y0*v}
           mp_mul_d(u,abs(x0),s);  if x0<0 then s_mp_chs(s);
           mp_mul_d(t,abs(y0),t);  if y0<0 then s_mp_chs(t);
           mp_add(s,t,u);
        {$endif}
      end;
      mp_mod(u,v,u);
      mp_exch(v,u);
      bsu := bsv;
      bsv := mp_bitsize(v);
    end;
    if (mp_error=MP_OKAY) and (v.used>0) then begin
      {v<>0 and fits into longint}
      tv := mp_get_int(v);
      mp_mod_int(u,tv,tu);
      mp_set_int(u,GCD32(tv,tu));
    end;
    mp_clear3(s,t,v);
  end;
end;


{---------------------------------------------------------------------------}
function mp_invmodf(const a, b: mp_int; var c: mp_int): boolean;
  {-compute c = a^-1 (mod b), b>0, via mp_xgcd, return true if inverse exists}
var
  u,v: mp_int;
  la, lb, lc: longint;
begin

  {Note: this function is normally faster than the old fast_invmod}
  {for odd b due to the added Lehmer steps in mp_xgcd. It is much }
  {faster than the old mp_invmodf for even b.}

  mp_invmodf := false;
  if mp_error<>MP_OKAY then exit;

  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(b) or mp_not_init(c) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_invmodf');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {if a and b are even or b<=0 then return no success}
  if (mp_iseven(b) and mp_iseven(a)) or (b.sign=MP_NEG) or mp_iszero(b) then exit;

  {use invmod32 if b has less than 32 bits}
  if mp_is_longint(b,lb) then begin
    if not mp_is_longint(a,la) then mp_mod_int(a,lb,la);
    lc := invmod32(la,lb);
    if lc<>0 then begin
      mp_set_int(c,lc);
      mp_invmodf := true;
    end;
    exit;
  end;

  {initialize temps}
  mp_init2(u,v);
  if mp_error<>MP_OKAY then exit;

  {calculate a*u + b*? = v = gcd(a,b)}
  mp_xgcd(a,b,@u,nil,@v);

  {if gcd(a,b><>1 then there is no inverse}
  if mp_is1(v) then begin
    {Make 0 <= a^-1 < b}
    while (mp_error=MP_OKAY) and (u.sign=MP_NEG) do mp_add(u,b,u);
    while (mp_error=MP_OKAY) and (mp_cmp_mag(u, b)<>MP_LT) do mp_sub(u,b,u);
    mp_exch(u, c);
    mp_invmodf := mp_error=MP_OKAY;
  end;
  mp_clear2(u,v);
end;


{---------------------------------------------------------------------------}
procedure mp_invmod(const a, b: mp_int; var c: mp_int);
  {-compute c = a^-1 (mod b), b>0, via mp_xgcd, MP_UNDEF error if there is no inverse}
begin
  if not mp_invmodf(a,b,c) then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXUndef.Create('mp_invmod: no inverse');
      {$else}
        RunError(MP_RTE_OTHER);
      {$endif}
    {$endif}
    set_mp_error(MP_UNDEF);
  end;
end;


{---------------------------------------------------------------------------}
function mp_isMersennePrime(p: longint): boolean;
  {-Lucas-Lehmer test for Mersenne number m=2^p-1, HAC 4.37}
var
  k,n,kmax,t: longint;
  r8 : integer;
  m,u: mp_int;
begin
  mp_isMersennePrime := false;

  {If p not prime, then m not prime}
  if (p<2) or not IsPrime32(p) then exit;

  {Here p is prime, check for p=2,3 or p a Sophie Germain prime}
  {Lucas-Lehmer test assumes p>=3}
  if p<=3 then begin
    mp_isMersennePrime := true;
    exit;
  end;

  {Ribenboim, Ch.2, VII: if p=3 (mod 4), p>3, q=2p+1 prime then q is a}
  {factor of m. This skips about 6% - 10% full tests for p < 100000000}
  if (p and 3 = 3) and (p<MaxLongint) and IsPrime32(succ(p shl 1)) then exit;

  mp_init2(m,u);
  if mp_error<>MP_OKAY then exit;

  {calculate 2^p-1}
  mp_mersenne(p,m);

  {Factors n of m have form n=1,7 mod 8 and n=1 mod 2p, ie n=2kp+1}
  {see http://www.utm.edu/research/primes/notes/proofs/MerDiv.html}
  {Search factor 2kp+1 with 1 <= k < min(MaxLongint/p, 2*p+1)}

  {smallest possible factor}
  n := succ(2*p);
  kmax := MaxLongint div p;
  if n<kmax then kmax := n;
  if p<60 then begin
    t := (1 shl ((p+1) div 2)) div p;
    if kmax>t then kmax := t;
  end;

  for k:=1 to kmax do begin
    r8 := n and 7;
    if ((r8=1) or (r8=7)) and IsPrime32(n) then begin
      mp_mod_int(m,n,t);
      if t=0 then begin
        {Check for spurious factor for small p}
        if mp_is_longint(m,t) and (n=t) then break;
        mp_clear2(m,u);
        exit;
      end;
    end;
    inc(n,2*p);
  end;

  {run Lucas-Lehmer test}
  mp_set(u,4);
  {Use unrestricted diminished radix reduction. Here we know that}
  {m>0 and the setup value is d=1, so skip test/setup functions  }
  for k:=1 to p-2 do begin
    {calculate u := u^2 - 2 mod m}
    mp_sqr(u,u);
    mp_sub_d(u,2,u);
    {if u<0 make u positive, no reduction}
    if u.sign=MP_NEG then mp_add(u,m,u)
    else mp_reduce_2k(u,m,1);
    if mp_error<>MP_OKAY then break;
  end;
  mp_isMersennePrime := mp_iszero(u) and (mp_error=MP_OKAY);
  mp_clear2(m,u);
end;


{---------------------------------------------------------------------------}
function s_mp_is_pth_power(const a: mp_int; p: longint; var r: mp_int): boolean;
  {-return true if a is pth power, then a=r^p. a>0, p>2 prime, no checks}
var
  amod64: mp_digit;
  w: word;
  j: integer;
  c,q,t: longint;
const
  {const arrays calculated with t_pmtabs.pas}
  ba_03_13: array[0..1] of byte = ($23,$11);
  ba_03_61: array[0..7] of byte = ($0b,$0b,$90,$19,$66,$02,$34,$14);
  ba_03_63: array[0..7] of byte = ($03,$01,$00,$18,$18,$00,$80,$40);
  ba_03_64: array[0..7] of byte = ($ab,$ab,$aa,$ab,$aa,$ab,$aa,$ab);

  ba_05_25: array[0..3] of byte = ($83,$00,$04,$01);
  ba_05_41: array[0..5] of byte = ($0b,$42,$00,$08,$41,$01);
  ba_05_44: array[0..5] of byte = ($03,$18,$a0,$00,$03,$08);
  ba_05_64: array[0..7] of byte = ($ab,$aa,$aa,$aa,$ab,$aa,$aa,$aa);

  ba_07_29: array[0..3] of byte = ($03,$10,$02,$10);
  ba_07_43: array[0..5] of byte = ($c3,$00,$00,$00,$30,$04);
  ba_07_49: array[0..6] of byte = ($03,$00,$0c,$c0,$00,$00,$01);

  ba_11_23: array[0..2] of byte = ($03,$00,$40);
  ba_11_36: array[0..4] of byte = ($b3,$2b,$9b,$ba,$09);

  ba_13_53: array[0..6] of byte = ($03,$00,$80,$40,$00,$00,$10);

  ba_xx_64: array[0..7] of byte = ($ab,$aa,$aa,$aa,$aa,$aa,$aa,$aa);

  {----------------------------------------------------------}
  function isclr(const ba: array of byte; k: word): boolean;
    {-test is bit k is zero}
  var
    i: integer;
  begin
    i := k shr 3;
    isclr := (i > high(ba)) or (ba[i] and (1 shl (k and 7)) = 0);
  end;

begin
  s_mp_is_pth_power := false;

  if (mp_error=MP_OKAY) and (a.used>0) and isprime32(p) then begin

    amod64 := a.pdigits^[0] and 63;
    case p of
        3: if isclr(ba_03_64, amod64) then exit;
        5: if isclr(ba_05_64, amod64) then exit;
      else if isclr(ba_xx_64, amod64) then exit;
    end;

    if p=3 then begin
      s_mp_mod_w(a,13*61*63,w);
      if isclr(ba_03_13, w mod 13) or isclr(ba_03_61, w mod 61) or isclr(ba_03_63, w mod 63) then exit;
    end
    else if p=5 then begin
      s_mp_mod_w(a,25*41*44,w);
      if isclr(ba_05_25, w mod 25) or isclr(ba_05_41, w mod 41) or isclr(ba_05_44, w mod 44) then exit;
    end
    else if p=7 then begin
      s_mp_mod_w(a,29*43*49,w);
      if isclr(ba_07_29, w mod 29) or isclr(ba_07_43, w mod 43) or isclr(ba_07_49, w mod 49) then exit;
    end
    else if p=11 then begin
      s_mp_mod_w(a,23*36,w);
      if isclr(ba_11_23, w mod 23) or isclr(ba_11_36, w mod 36) then exit;
    end
    else if p=13 then begin
      s_mp_mod_w(a,53,w);
      if isclr(ba_13_53, w) then exit;
    end;

    {Check if a is no pth power residue of m, (m=p^2 or m=q=t*p+1, q prime) }
    {Theorem 6.18 from A.Adler, J.E.Coury [26]: If m>0 has a primitive root,}
    {gcd(a,p)=1, then x^p = a (mod m) has a solution if and only if         }
    {a^(phi(m)/gcd(p,phi(m))) = 1 mod m.  Since m=p^2 or m=q are suitable,  }
    {we know that if  a<>0 mod m  and  a^(phi(m)/gcd(p,phi(m))) <> 1 mod m  }
    {then x^p = a has no solutions mod m, and x^p = a has no solution in Z. }
    {Since p is prime, gcd(a,p)=1 if a mod p <> 0}

    {First tests only if p^2 < MaxLongint}
    if p<=46340 then begin
      c := sqr(p);
      s_mp_mod_w(a,word(p),w);
      mp_mod_int(a,c,t);
      {w = a mod p; t = a mod p^2}
      if w<>0 then begin
        {a is pth power residue mod p^2 if a mod p <> 0 and}
        {1 = a^(phi(p^2)/gcd(p,phi(p^2))) = a^(p-1) mod p^2}
        if exptmod32(t,pred(p),c)<>1 then exit;
      end
      else begin
        {Here a mod p = 0, i.e. a = x*p. Since p>2, a must be a multiple}
        {of p^3 if it is a pth power. If  a mod p^2 <> 0, a is no power!}
        if t<>0 then exit
        else if p<=1290 then begin
          {Check a mod p^3 = 0}
          mp_mod_int(a,c*p,t);
          if t<>0 then exit;
        end;
      end;
    end;

    {get primes q = t*p + 1}
    q := 1;
    t := 0;
    {Note: Loop count for 'sieve tests' approximately equal to the value }
    {2*log2(log2(a)) / log2(p). Ref: E.Bach, J.Sorenson: Sieve Algorithms}
    {for Perfect Power Testing, Algorithmica 9 (1993), 313-328}
    for j:=0 to (2*bitsize32(mp_bitsize(a))) div bitsize32(p) do begin
      {find next prime q = t*p + 1}
      c := p+p;
      repeat
        inc(q,c);
        inc(t,2);
      until (q<0) or isprime32(q);
      {found prime if q>0}
      if q>0 then begin
        mp_mod_int(a,q,c);
        {Check a^(phi(q)/gcd(p,phi(q))) <> 1 mod q}
        {phi(q)=q-1, gcd(p,phi(q)) = gcd(p,p*t) = p}
        {phi(q)/gcd(p,phi(q)) = p*t/p = t}
        if c<>0 then begin
          if exptmod32(c,t,q)<>1 then begin
            {a is no pth power residue mod q}
            exit;
          end;
        end
        else begin
          {a mod q = 0, no power if a mod q^2 <> 0}
          if q<=46340 then begin
            mp_mod_int(a,q*q,c);
            if c<>0 then exit;
          end;
        end;
      end;
    end;
  end;
  {a has survived all fast checks, compute r=floor(a^(1/p)) and check r^p=a}
  s_mp_is_pth_power := s_mp_n_root2(a,p,r,nil) and (mp_error=MP_OKAY);
end;


{---------------------------------------------------------------------------}
function mp_is_pth_power(const a: mp_int; p: longint; var r: mp_int): boolean;
  {-return true if a is pth power, a>0, p prime. If true, calculate r with a=r^p}
begin
  if p=2 then begin
    mp_is_pth_power := mp_is_square2(a,@r);
    exit;
  end;
  mp_is_pth_power := false;
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(r) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_is_pth_power');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if a.sign=MP_NEG then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mp_is_pth_power: a<0');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;
  if (p<2) or not isprime32(p) then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXBadArg.Create('mp_is_pth_power: p not prime');
      {$else}
        RunError(MP_RTE_BADARG);
      {$endif}
    {$else}
      set_mp_error(MP_BADARG);
      exit;
    {$endif}
  end;
  mp_is_pth_power := s_mp_is_pth_power(a,p,r);
end;


{---------------------------------------------------------------------------}
procedure mp_is_power(const a: mp_int; var b: mp_int; var p: longint);
  {-Calculate smallest prime p with a=b^p; p=1,b=a if a is no power}
var
  r,t,s: mp_int;
  i,l: longint;
  sa: word;
  amod8: mp_digit;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(b) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_is_power');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {remember sign(a) and |a| mod 8}
  sa := a.SIGN;
  amod8 := a.pdigits^[0] and 7;

  {init default answer}
  p := 1;

  {check trivial cases}
  l := mp_bitsize(a);
  if (amod8 and 3 = 2) or (l<4) then begin
    {b=a^1 for a=-7..7; except 4=2^2. Further b=a^1 for a=2 mode 4}
    {because a^p <> 2 mod 4 for all primes}
    if (amod8=4) and (sa=MP_ZPOS) then begin
      {a=4, return b=2, p=2}
      mp_set(b,2);
      p := 2;
    end
    else mp_copy(a,b);
    exit;
  end;

  {if a is positive, check if a is square}
  if (sa=MP_ZPOS) and mp_is_square2(a,@b) then begin
    p := 2;
    exit;
  end;

  mp_init3(r,s,t);

  {perform calculation for t=abs(a)}
  mp_abs(a,t);

  {l=max. prime to test: worst case t=2^p}
  if (mp_popcount(t)=1) and isprime32(l-1) then begin
    {t=2^p with p=l-1}
    p := l-1;
    mp_set(r,2);
  end
  else begin
    {reduce bound to log3(t) < log2(t)*0.631}
    l := 1 + trunc(0.631*l);
    i := 3;
    while (i<=l) and (mp_error=MP_OKAY) do begin
      if s_mp_is_pth_power(t,i,r) then begin
        if not mp_is1(r) then p := i;
        break;
      end;
      i := nextprime32(i+1);
    end;
  end;
  {if no p found set b=a}
  if p=1 then begin
    {if no p>1 found set b=a}
    mp_copy(a,b)
  end
  else if p=2 then begin
    {Bugfix 0.9.01: if p=2 and a<0 then set p=1 and b=a. The only case}
    {should be a=-4 resulting from |a|=2^p with p even prime!}
    if sa=MP_NEG then begin
      p := 1;
      mp_copy(a,b)
    end
    else mp_exch(r,b);
  end
  else begin
    {set b=sign(a)*r, (note p is odd)}
    if sa=MP_NEG then mp_chs(r,b)
    else mp_exch(r,b);
  end;
  mp_clear3(r,s,t);
end;


{---------------------------------------------------------------------------}
procedure mp_is_power_max(const a: mp_int; var b: mp_int; var k: longint);
  {-Calculate largest k with a=b^k; k=1,b=a if a is no power}
var
  n: longint;
begin
  {Arg checking is done in mp_copy}
  mp_copy(a,b);
  if mp_error<>MP_OKAY then exit;

  {invariant a=b^k}
  k := 1;
  {done if |a|=1}
  if mp_is1a(b) then exit;

  repeat
    {set b'=b^n, k'=k*n until n=1}
    mp_is_power(b,b,n);
    k := k*n;
  until (n=1) or (mp_error<>MP_OKAY);
end;


{---------------------------------------------------------------------------}
function s_mp_is_pprime_ex(const a: mp_int; smin,smax: mp_digit): boolean;
  {-Test if a is prime (BPSW pseudo prime if a>2^32); trial division from}
  { smin to smax; no init check, no check if a is less than 2<31}
var
  f: mp_digit;
  n: longint;
const
  DC_MAXLONG = (30+DIGIT_BIT) div DIGIT_BIT;
begin
  if (a.used<=DC_MAXLONG) and mp_is_longint(a,n) then begin
    s_mp_is_pprime_ex := IsPrime32(n);
    exit;
  end;
  {Default to not prime}
  s_mp_is_pprime_ex := false;

  {check if a has small factor up to min(MP_DIGIT_MAX,smax)}
  mp_small_factor(a, smin, smax, f);
  if (mp_error<>MP_OKAY) or (f<>0) then exit;

  {Test for BPSW (Baillie-Pomerance-Selfridge-Wagstaff) pseudoprime:}
  {First check if a is spsp(2), exit if not}
  if not mp_is_spsp_d(a,2) then exit;

  {then do a strong Lucas pseudo prime test}
  s_mp_is_pprime_ex := mp_is_slpsp(a) and (mp_error=MP_OKAY);

end;


{---------------------------------------------------------------------------}
function mp_is_pprime_ex(const a: mp_int; smax: mp_digit): boolean;
  {-Test if a is prime (BPSW pseudo prime if a>2^32); trial division up to smax}
var
  n: longint;
begin
  {Default to not prime}
  mp_is_pprime_ex := false;

  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_is_pprime_ex');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {Negative numbers and 0 are not prime}
  if s_mp_is_le0(a) then exit;

  {uses IsPrime32 for less than 32 bit}
  if mp_is_longint(a,n) then begin
    mp_is_pprime_ex := (mp_error=MP_OKAY) and (n>0) and IsPrime32(n);
    exit;
  end;

  {here we have a with more than one mp_digit and at least 32 bit}
  mp_is_pprime_ex := s_mp_is_pprime_ex(a, 2, smax and MP_DIGIT_MAX);

end;


{---------------------------------------------------------------------------}
function mp_is_pprime(const a: mp_int): boolean;
  {-Test if a is prime (BPSW pseudo prime if a>2^32)}
  { Trial division up to min(MP_DIGIT_MAX,$3FFF)}
const
  smax = mp_digit(MP_DIGIT_MAX and $3FFF);  {max. for small factor}
begin
  mp_is_pprime := mp_is_pprime_ex(a, smax);
end;


{---------------------------------------------------------------------------}
function mp_is_primepower(const a: mp_int; var b: mp_int; var k: longint): boolean;
   {-return true if a=b^k, b prime, k>1, otherwise false and a=b^k, k=1 if no power}
var
  n: longint;
begin
  mp_is_primepower := false;

  {invariant a=b^k}
  mp_copy(a,b);
  k := 1;

  {Arg checking is done in mp_copy}
  if mp_error<>MP_OKAY then exit;

  {Numbers <= 1 are no prime powers}
  if s_mp_is_le0(b) or mp_is1(b) then exit;

  {Handle case of even a}
  if mp_iseven(b) then begin
    if mp_is_pow2(b,k) then begin
      mp_set(b,2);
      mp_is_primepower := (mp_error=MP_OKAY) and (k>1);
    end;
  end
  else begin
    repeat
      {set b'=b^n, k'=k*n until n=1}
      mp_is_power(b,b,n);
      k := k*n;
    until (n=1) or (mp_error<>MP_OKAY);
    {a=b^k with k>1. Check if b is prime}
    if (k>1) and mp_is_pprime(b) then begin
      mp_is_primepower := mp_error=MP_OKAY;
      exit;
    end;
  end;
end;


{---------------------------------------------------------------------------}
function mp_is_slpsp(const a: mp_int): boolean;
  {-strong Lucas pseudo prime test for a. Lucas test is }
  { done for the first p=2k+1 with mp_jacobi(p^2-4,a)=-1}
const
  bmax = $7FFF; {max p for Jabobi test)}
  bsqr = 127;   {p index for perfect square test, must be odd}

label
  leave;

var
  b: integer;
  d,i: longint;
  mu,p,t,v: mp_int;
begin

  {initialize function result with false}
  mp_is_slpsp := false;
  if mp_error<>MP_OKAY then exit;

  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_is_slpsp');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {easy outs: for a<3 return true only if a=2}
  b := mp_cmp_d(a,2);
  if (a.sign=MP_NEG) or (b=MP_LT) then exit;
  if mp_iseven(a) then begin
    mp_is_slpsp := (b=MP_EQ) and (mp_error=MP_OKAY);
    exit;
  end;

  {create temporary mp_ints}
  mp_init4(mu,p,t,v);
  if mp_error<>0 then exit;

  {generate sequence without squaring: p(k)=2k+1, D(k)=p(k)^2 - 4}
  {p(1)=3, D(1)=5, D(k+1)=D(k)+i(k+1), with i(k+1)=4p(k+1)=i(k)+8}
  {Lucas test is done for the first p with mp_jacobi(p^2-4,a)=-1.}
  {This sequence is from Wei Dai / Marcel Martin, see references.}

  b := 3;
  d := 5;
  i := 8;
  repeat
    if mp_jacobi_lm(d,a)=-1 then break;
    if b=bsqr then begin
      {Avoid 'hopeless' loop and test whether a is a perfect square. This }
      {is delayed because we are searching for jacobi(d,a)=-1; if there is}
      {such a d then a is not square, and a square root will be computed  }
      if mp_is_square(a) then goto leave;
    end;
    inc(i,8);
    inc(d,i);
    inc(b,2);
    {assert(d+4=sqr(longint(b)), 'd = b^2 - 4 in mp_is_slpsp');}
    if mp_error<>MP_OKAY then goto leave;
  until b>=bmax;

  {exit if no proper p=b found}
  if b>=bmax then goto leave;

  {p=b>0 should be MUCH less than a but ...}
  mp_set_int(p,b);
  if mp_cmp(a,p)=MP_LT then mp_mod(p,a,p);

  {calculate t,s with a+1 = 2^i*t}
  mp_add_d(a,1,t);
  mp_makeodd(t,t,i);

  {Setup Barrett reduction for a}
  mp_reduce_setup(mu, a);

  {Calculate Lucas sequence}
  s_mp_lucasvmod1(p,a,t,mu,v);

  {t=n-2 for the next compares}
  mp_sub_d(a,2,t);

  if (mp_cmp_d(v,2)=MP_EQ) or (mp_cmp(v,t)=MP_EQ) then begin
    {a is lpsp}
    mp_is_slpsp := true;
    goto leave;
  end;

  for d:=1 to i-1 do begin
    mp_sqr(v,v);      mp_reduce(v,a,mu);
    mp_sub_d(v,2,v);  if v.sign=MP_NEG then mp_add(v,a,v);
    if mp_cmp_d(v,2)=MP_EQ then goto leave;
    if mp_cmp(v,t)=MP_EQ then begin
      {a is lpsp}
      mp_is_slpsp := true;
      goto leave;
    end;
  end;

leave:
  if mp_error<>MP_OKAY then mp_is_slpsp := false;
  mp_clear4(mu,p,t,v);

end;


{---------------------------------------------------------------------------}
function mp_is_spsp(const n,a: mp_int): boolean;
  {-strong pseudo prime test of n to base of a>1 from HAC p. 139 Alg.4.24  }
  { Sets result to false if definitely composite or true if probably prime.}
  { Randomly the chance of error is <= 1/4 and often very much lower.      }
label
  leave;
var
  n1, y, r: mp_int;
  s,j: longint;
begin
  {init default result}
  mp_is_spsp := false;
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(n) or mp_not_init(a) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_is_spsp');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {ensure a > 1}
  if mp_cmp_d(a, 1)<>MP_GT then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mp_is_spsp: a<=1');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;

  mp_init3(n1,y,r);
  if mp_error<>MP_OKAY then exit;

  {get n1 = n - 1}
  mp_sub_d(n,1,n1);

  {set 2^s * r = n1}
  mp_makeodd(n1,r,s);

  {compute y = a^r mod n}
  mp_exptmod(a, r, n, y);

  {if y<>1 and y<>n-1 do}
  if (not mp_is1(y)) and mp_is_ne(y, n1) then begin
    j := 1;
    while (j <= s-1) and mp_is_ne(y, n1) and (mp_error=MP_OKAY) do begin
      mp_sqrmod(y, n, y);
      {if y=1 then composite}
      if mp_is1(y) then goto leave;
      inc(j);
    end;
    {if y<>n1 then composite}
    if mp_is_ne(y, n1) then goto leave;
  end;

  {probably prime now}
  mp_is_spsp := true;

leave:
  mp_clear3(n1,y,r);
end;


{---------------------------------------------------------------------------}
function mp_is_spsp_d(const n: mp_int; a: mp_digit): boolean;
  {-strong pseudo prime test of n to mp_digit base of a>1 from HAC p. 139 Alg.4.24}
var
  t: mp_int;
begin
  mp_is_spsp_d := false;
  if mp_error=MP_OKAY then begin
    mp_init_set(t,a);
    if mp_error=MP_OKAY then begin
      mp_is_spsp_d := mp_is_spsp(n,t);
      mp_clear(t);
    end;
  end;
end;


{---------------------------------------------------------------------------}
function mp_is_square(const a: mp_int): boolean;
  {-test if a is square}
begin
  mp_is_square := mp_is_square2(a, nil);
end;


{---------------------------------------------------------------------------}
function mp_is_square2(const a: mp_int; psqrt: pmp_int): boolean;
  {-test if a is square, return sqrt(a) if a is a square and psqrt<>nil}
const {bit i is cleared if i mod n is a square, i=0..m-1}
  ba_37  : array[0..04] of byte = ($64,$e1,$de,$a1,$09);
  ba_41  : array[0..05] of byte = ($c8,$f8,$4a,$7d,$4c,$00);
  ba_43  : array[0..05] of byte = ($ac,$11,$5c,$7c,$a7,$04);
  ba_47  : array[0..05] of byte = ($20,$ac,$d8,$e4,$ca,$7b);
  ba_128 : array[0..15] of byte = ($ec,$fd,$fc,$fd,$ed,$fd,$fd,$fd,$ec,$fd,$fd,$fd,$ed,$fd,$fd,$fd);
const
  DC_MAXLONG = (30+DIGIT_BIT) div DIGIT_BIT;
var
  s,t: mp_int;
  k,r: longint;
  i: integer;
begin
  {Default to Non-square}
  mp_is_square2 := false;

  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or ((psqrt<>nil) and mp_not_init(psqrt^)) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_is_square2');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {Negative numbers are not square}
  if a.sign=MP_NEG then exit;

  {0 and 1 are square}
  if (a.used=0) or ((a.used=1) and (a.pdigits^[0]=1)) then begin
    mp_is_square2 := true;
    if psqrt<>nil then mp_copy(a, psqrt^);
    exit;
  end;

  {First check mod 128 (DIGIT_BIT is at least 8)}
  i := a.pdigits^[0] and 127;
  if ba_128[i shr 3] and (1 shl (i and 7)) <> 0 then exit;
  {82.03% rejection rate}

  {Brute force for remaining 32 bit arguments}
  if (a.used<=DC_MAXLONG) and mp_is_longint(a,k) then begin
    r := isqrt32(k);
    if k=sqr(r) then begin
      mp_is_square2 := true;
      if psqrt<>nil then mp_set_int(psqrt^,r);
    end;
    exit;
  end;

  mp_mod_int(a,2029964475,r);   {31*29*25*23*21*17*11}
  if (1 shl (r mod 21)) and $1A7D6C   <> 0 then exit;  {0.6190}
  if (1 shl (r mod 25)) and $D6B5AC   <> 0 then exit;  {0.5600}
  if (1 shl (r mod 31)) and $6DE2B848 <> 0 then exit;  {0.4839}
  if (1 shl (r mod 29)) and $C2EDD0C  <> 0 then exit;  {0.4828}
  if (1 shl (r mod 23)) and $7ACCA0   <> 0 then exit;  {0.4783}
  if (1 shl (r mod 17)) and $5CE8     <> 0 then exit;  {0.4706}
  if (1 shl (r mod 11)) and $5C4      <> 0 then exit;  {0.4545}
  {99.88% cumulative rejection rate}

  mp_mod_int(a,757266679,r); {13*19*37*41*43*47}
  i := r mod 47; if ba_47[i shr 3] and (1 shl (i and 7)) <> 0 then exit; {0.4894}
  i := r mod 43; if ba_43[i shr 3] and (1 shl (i and 7)) <> 0 then exit; {0.4884}
  i := r mod 41; if ba_41[i shr 3] and (1 shl (i and 7)) <> 0 then exit; {0.4878}
  i := r mod 37; if ba_37[i shr 3] and (1 shl (i and 7)) <> 0 then exit; {0.4865}
  if (1 shl (r mod 19)) and $4F50C <> 0 then exit;  {0.4737}
  if (1 shl (r mod 13)) and $9E4   <> 0 then exit;  {0.4615}
  {99.9976% cumulative rejection rate}

  mp_init2(s,t);
  if mp_error=MP_OKAY then begin
    {Final check: test if a-sqrt(a)^2=0}
    s_mp_sqrtrem(a,s,t);
    if mp_is0(t) then begin
      mp_is_square2 := true;
      if psqrt<>nil then mp_exch(s, psqrt^);
    end;
    mp_clear2(s,t);
  end;
end;


{---------------------------------------------------------------------------}
function kron_intern(var x,y: mp_int): integer;
  {-internal kronecker: no mp_init, initial range/sanity checks etc}
var
  res: integer;
  m8: mp_digit;
  k: longint;
begin
  (*
  Here the PARI/GP definition is used: The Kronecker symbol is the Legendre
  symbol (x|y) extended to all integers by complete multiplicativity,
  plus special rules for y = 0, -1, or 2:

  y = 0:  (x|0)  =  1 if |x| = 1
                    0 otherwise

  y =-1: (x|-1)  =  1 if x >= 0,
                   -1 if x <  0.

  y = 2:  (x|2)  =  0 if x is even
                 =  1 if x = 1, 7 mod 8
                 = -1 if x = 3, 5 mod 8
  *)

  {initialize return val for zero/error}
  kron_intern := 0;

  {easy case (x|0)}
  if mp_iszero(y) then begin
    if mp_is1a(x) then kron_intern := 1;
    exit;
  end;

  {initialize accumulated result}
  res := 1;

  {here y<>0, make y positive}
  if y.sign=MP_NEG then begin
    {(x|y) = (x|-1)*(x|-y)}
    if x.sign=MP_NEG then res := -res;
    mp_abs(y,y);
  end;

  {if y even, reduce to odd case}
  if y.pdigits^[0] and 1 = 0 then begin
    {(x|2)=0 if x is even}
    if mp_iseven(x) then exit;
    {y is even; divide out highest power of two: y = 2^k*y'}
    {(x|y) = (x|2)^k * (x|y')}
    mp_makeodd(y,y,k);
    if odd(k) then begin
      {note: x is odd and <> 0, so pdigits^[0] is valid}
      m8 := x.pdigits^[0] and 7;
      if (m8=3) or (m8=5) then res := -res;
    end;
  end;

  {Here y is positive and odd, and we actually calculate a Jacobi symbol.}
  {x>=0 is needed for binary algorithm, so take abs and adjust sign.}
  if x.sign=MP_NEG then begin
    {(-x|y) = (x|y)*(-1|y); change sign if y=3 (mod 4)}
    if y.pdigits^[0] and 3 = 3 then res := -res;
    x.sign := MP_ZPOS;
  end;

  if mp_initial_mod and (y.used<=x.used) then begin
    {Here x,y>0, y is odd. Reduce size once, see Cohen [24], Alg.1.4.12}
    mp_mod(x,y,x);
  end;

  {This is the Jacobi loop from Shallit/Sorenson [23]}
  while (x.used>0) and (mp_error=MP_OKAY) do begin
    {y odd, x odd or even. divide out highest power of two: 2^k}
    mp_makeodd(x,x,k);
    {only if odd exponent check y mod 8 and change sign}
    if odd(k) then begin
      { (2|y) = -1 if y = +-3 (mod 8)}
      m8 := y.pdigits^[0] and 7;
      if (m8=3) or (m8=5) then res := -res;
    end;
    {x,y odd}
    if x.used<=y.used then begin
      case mp_cmp_mag(x,y) of
        MP_LT: begin
                 {if x<y then interchange(x,y) and adjust res}
                 mp_exch(x,y);
                 if (y.pdigits^[0] and 3 = 3) and (x.pdigits^[0] and 3 = 3) then res := -res;
               end;
        MP_EQ: begin
                 {done if x=y}
                 break;
               end;
      end;
    end;
    {Here x,y odd and x>y so we can use s_mp_sub}
    s_mp_sub(x,y,x);
    {y odd, x even, x > 0.  Division by 2 and sign adjustment from the}
    {Shallit/Sorenson code is not needed here because these operations}
    {will be done in the next pass of the loop.}
  end;
  if (mp_error=MP_OKAY) and mp_is1(y) then kron_intern := res;
end;


{---------------------------------------------------------------------------}
function mp_kronecker(const a,n: mp_int): integer;
  {-Compute the Kronecker symbol (a|n)}
var
  a2, n2: mp_int;
begin
  mp_kronecker := 0;
  if mp_error<>MP_OKAY then exit;

  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(n) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_kronecker');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {Initialize local mp_int copies}
  mp_init_copy(a2,a);
  if mp_error<>MP_OKAY then exit;

  mp_init_copy(n2, n);
  if mp_error=MP_OKAY then begin
    mp_kronecker := kron_intern(a2,n2);
    mp_clear(n2);
  end;
  mp_clear(a2);
end;


{---------------------------------------------------------------------------}
function mp_jacobi(const a,n: mp_int): integer;
  {-Compute the Jacobi/Legendre symbol (a|n), n: odd and > 2}
var
  a2, n2: mp_int;
begin
  mp_jacobi := 0;
  if mp_error<>MP_OKAY then exit;

  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(n) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_jacobi');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {Error if n<3 or even}
  if (n.sign=MP_NEG) or mp_is1(n) or mp_iseven(n) then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mp_jacobi: n<3 or even');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;

  {Initialize local mp_int copies}
  mp_init_copy(a2,a);
  if mp_error<>MP_OKAY then exit;

  mp_init_copy(n2, n);
  if mp_error=MP_OKAY then begin
    {Invalid n values are excluded above, use Kronecker symbol}
    mp_jacobi := kron_intern(a2,n2);;
    mp_clear(n2);
  end;
  mp_clear(a2);
end;


{---------------------------------------------------------------------------}
function mp_jacobi_lm(a: longint; const n: mp_int): longint;
  {-Compute the Jacobi/Legendre symbol (a|n), n: odd and > 2}
var
  j,k: integer;
  t: longint;
  m8: mp_digit;
begin
  mp_jacobi_lm := 0;
  if mp_error<>MP_OKAY then exit;

  {$ifdef MPC_ArgCheck}
    if mp_not_init(n) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_jacobi_lm');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {Error if n<3 or even}
  if (n.sign=MP_NEG) or mp_is1(n) or mp_iseven(n) then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mp_jacobi_lm: n<3 or even');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;

  if a=0 then exit;

  {initialize accumulated result}
  j := 1;
  m8 := n.pdigits^[0] and 7;

  if a<0 then begin
    {(-a|b) = (a|b)*(-1|b); change sign if b=3 (mod 4)}
    if m8 and 3 = 3 then j := -j;
    a := -a;
  end;

  {if a is even divide out highest power of two: 2^k}
  if a and 1 = 0 then begin
    k:=0;
    repeat
      inc(k);
      a := a shr 1;
    until odd(a);
    {if odd exponent use (2|b) = -1 if b = +-3 (mod 8)}
    if odd(k) and ((m8=3) or (m8=5)) then j:=-j;
  end;

  {done if a=1}
  if a=1 then mp_jacobi_lm := j
  else begin
    {adjust result using quadratic reciprocity}
    if (a and 3 = 3) and (m8 and 3 = 3) then j := -j;
    {swap variables, reduce mp_int mod longint, and use jacobi32}
    mp_mod_int(n,a,t);
    if t<>0 then mp_jacobi_lm := j*jacobi32(t,a);
  end;
end;


{---------------------------------------------------------------------------}
function mp_jacobi_ml(const a: mp_int; n: longint): longint;
  {-Compute the Jacobi/Legendre symbol (a|n), n: odd and > 2}
var
  am: longint;
begin
  mp_jacobi_ml := 0;
  if mp_error<>MP_OKAY then exit;

  {Error if n<3 or even}
  if (n<3) or (n and 1 = 0) then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mp_jacobi_ml: n<3 or even');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;

  {mp_mod_int does init check on a and returns am = a mod n}
  mp_mod_int(a,n,am);
  mp_jacobi_ml := jacobi32(am,n);
end;


{---------------------------------------------------------------------------}
procedure mp_lcm(const a,b: mp_int; var c: mp_int);
  {-Compute least common multiple as |a*b|/(a, b)}
var
  t: mp_int;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(b) or mp_not_init(c) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_lcm');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {if a=0 or b=0 then c:=0}
  if mp_is0(a) or mp_is0(b) then begin
    mp_zero(c);
    exit;
  end;

  mp_init(t);
  if mp_error<>MP_OKAY then exit;

  {get the gcd of the two inputs}
  if mp_gcd1(a,b,t) then mp_mul(a,b,c)
  else begin
    {divide the largest by the GCD}
    if mp_cmp_mag(a,b)=MP_GT then begin
      {lcm  = (a div t)*b}
      mp_div(a, t, t);
      mp_mul(b, t, c);
    end
    else begin
      {lcm  = a * (b div t)}
      mp_div(b, t, t);
      mp_mul(a, t, c);
    end;
  end;
  {fix the sign to positive}
  c.sign := MP_ZPOS;
  mp_clear(t);
end;


{---------------------------------------------------------------------------}
procedure mp_lucas(k: longint; var lk: mp_int);
  {-calculate Lucas number lk=luc(k), luc(-k)=(-1)^k*luc(k)}
var
  f1: mp_int;
const
  smluc: array[0..23] of word = (2,1,3,4,7,11,18,29,47,76,123,199,322,521,843,1364,
                                 2207,3571,5778,9349,15127,24476,39603,64079);
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(lk) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_lucas');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  { luc(k) = luc(k-1) + luc(k-2) }
  { luc(0) = 2, luc(1)=1         }
  { luc(k) = fib(k) + 2*fib(k-1) }

  if abs(k)<24 then mp_set_w(lk,smluc[abs(k)])
  else begin
    {create temporary for fib(k-1)}
    mp_init(f1);
    if mp_error<>MP_OKAY then exit;
    {get Fibonacci numbers for abs(k)}
    mp_fib2(abs(k),lk,f1);
    mp_mul_2(f1,f1);
    mp_add(lk,f1,lk);
    mp_clear(f1);
  end;
  {if k negative adjust sign of luc(k)}
  if (k<0) and odd(k) then mp_chs(lk,lk);
end;


{---------------------------------------------------------------------------}
procedure mp_lucas2(k: longint; var lk,l1: mp_int);
  {-calculate two Lucas numbers lk=luc(k), l1=luc(k-1), k>=0}
var
  t: mp_int;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(lk) or mp_not_init(l1) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_lucas2');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
    if k<0 then begin
      {$ifdef MPC_HaltOnArgCheck}
         {$ifdef MPC_UseExceptions}
           raise MPXRange.Create('mp_lucas2: k<0');
         {$else}
           RunError(MP_RTE_RANGE);
         {$endif}
      {$else}
        set_mp_error(MP_RANGE);
        exit;
      {$endif}
    end;
    if @lk=@l1 then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXBadArg.Create('mp_lucas2: @lk=@l1');
        {$else}
          RunError(MP_RTE_BADARG);
        {$endif}
      {$else}
        set_mp_error(MP_BADARG);
        exit;
      {$endif}
    end;
  {$endif}

  {easy outs for k=0,1}
  if k=0 then begin
    mp_set(lk,2);
    mp_set_short(l1, -1);
    exit;
  end
  else if k=1 then begin
    mp_set1(lk);
    mp_set(l1,2);
    exit;
  end;

  {calculate the pair of Lucas numbers from a Fibonacci pair}
  {   L[k] =   F[k] + 2*F[k-1] }
  { L[k-1] = 2*F[k] -   F[k-1] }

  {initialize temporary for F[k-1]}
  mp_init(t); if mp_error<>MP_OKAY then exit;

  {lk = F[k], t = F[k-1]}
  mp_fib2(k,lk,t);

  {L[k-1] = 2*F[k]-F[k-1]}
  mp_mul_2(lk,l1);
  mp_sub(l1,t,l1);

  {L[k] = F[k] + 2*F[k-1]}
  mp_mul_2(t,t);
  mp_add(lk,t,lk);
  mp_clear(t);
end;


{---------------------------------------------------------------------------}
procedure mp_lucasv2p(const p,q: mp_int; k: longint; var vk: mp_int; pvk1: pmp_int);
  {-calculate v[k] of Lucas V sequence for p,q; p^2-4q<>0}
  { if pvk1<>nil, pvk1^ will be set to v[k+1]}
var
  v0,v1,x,t: mp_int;
  i: integer;
begin

  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(p) or mp_not_init(q) or ((pvk1<>nil) and mp_not_init(pvk1^)) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_lucasv2p');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
    if @vk=pvk1 then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXBadArg.Create('mp_lucasv2p: @vk=pvk1');
        {$else}
          RunError(MP_RTE_BADARG);
        {$endif}
      {$else}
        set_mp_error(MP_BADARG);
        exit;
      {$endif}
    end;
  {$endif}

  {v[k+2] = p*v[k+1] - q*v[k], v[0]=2, v[1]=p}
  {p^2-4q<>0 is not tested, no proper Lucas sequence!!}

  {easy out for k<=0, k<0 is handled as k=0}
  if k<=0 then begin
    {v[0] = 2}
    mp_set(vk,2);
    {v[1] = p}
    if pvk1<>nil then mp_copy(p,pvk1^);
    exit;
  end;

  mp_init4(v0,v1,x,t);
  if mp_error<>0 then exit;

  {v0 = p}
  mp_copy(p, v0);

  {start with x=q}
  mp_copy(q, x);

  {v1 = p*p - 2q}
  mp_sqr(p,v1);
  mp_mul_2(q,t);
  mp_sub(v1,t,v1);

  {loop is never executed for k=1}
  for i:=bitsize32(k)-2 downto 0 do begin
    if mp_error<>MP_OKAY then break;
    if k and (1 shl i) <> 0 then begin
      {v0 = v0*v1 - x*p}
      mp_mul(v0,v1,v0);
      mp_mul(x,p,t);
      mp_sub(v0,t,v0);
      {v1 = v1*v1 - 2x*q}
      mp_sqr(v1,v1);
      mp_mul(x,q,t);
      mp_mul_2(t,t);
      mp_sub(v1,t,v1);
      if i<>0 then begin
        {x = x*x*q}
        mp_sqr(x,x);
        mp_mul(x,q,x);
      end;
    end
    else begin
      {v1 = v0*v1 - x*p}
      mp_mul(v0,v1,v1);
      mp_mul(x,p,t);
      mp_sub(v1,t,v1);
      {v0 = v0*v0 - 2x}
      mp_sqr(v0,v0);
      mp_mul_2(x,t);
      mp_sub(v0,t,v0);
      if i<>0 then begin
        {x = x*x}
        mp_sqr(x,x);
      end;
    end;
  end;

  if mp_error=MP_OKAY then begin
    {store v[k]}
    mp_copy(v0,vk);
    {store v[k+1] if requested}
    if pvk1<>nil then mp_exch(v1, pvk1^);
  end;

  mp_clear4(v0,v1,x,t);
end;


{---------------------------------------------------------------------------}
procedure mp_lucasv(const p,q: mp_int; k: longint; var v: mp_int);
  {-calculate v[k] of Lucas V sequence for p,q, p^2-4q <>0}
begin
  mp_lucasv2p(p,q,k,v,nil);
end;


{---------------------------------------------------------------------------}
procedure mp_lucasv2(const p,q: mp_int; k: longint; var v,w: mp_int);
  {-calculate v=v[k],w=v[k+1] of Lucas V sequence for p,q, p^2-4q<>0}
begin
  mp_lucasv2p(p,q,k,v,@w);
end;


{---------------------------------------------------------------------------}
procedure mp_lucasvmod(const p,q,n,k: mp_int; var vk: mp_int);
  {-calculate v[k] mod n of Lucas V sequence for p,q.}
  { Ranges n>1, 0 <= p,q,k < n (checked if MPC_ArgCheck).}
  { Note: p^2-4q<>0 is not tested, no proper Lucas sequence!!}
var
  i: longint;
  v0,v1,x,t,mu: mp_int;
begin

  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(p) or mp_not_init(q) or mp_not_init(k) or mp_not_init(n) or mp_not_init(vk) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_lucasvmod');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
    if mp_cmp_d(n,2)=MP_LT then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXBadArg.Create('mp_lucasvmod: n<2');
        {$else}
          RunError(MP_RTE_BADARG);
        {$endif}
      {$else}
        set_mp_error(MP_BADARG);
        exit;
      {$endif}
    end;
    if (p.sign=MP_NEG) or mp_is_ge(p,n) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXBadArg.Create('mp_lucasvmod: p<0 or p>=n');
        {$else}
          RunError(MP_RTE_BADARG);
        {$endif}
      {$else}
        set_mp_error(MP_BADARG);
        exit;
      {$endif}
    end;
    if (q.sign=MP_NEG) or mp_is_ge(q,n) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXBadArg.Create('mp_lucasvmod: q<0 or q>=n');
        {$else}
          RunError(MP_RTE_BADARG);
        {$endif}
      {$else}
        set_mp_error(MP_BADARG);
        exit;
      {$endif}
    end;
    if (k.sign=MP_NEG) or mp_is_ge(k,n) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXBadArg.Create('mp_lucasvmod: k<0 or k>=n');
        {$else}
          RunError(MP_RTE_BADARG);
        {$endif}
      {$else}
        set_mp_error(MP_BADARG);
        exit;
      {$endif}
    end;
  {$endif}

  {v[k+2] = p*v[k+1] - q*v[k], v[0]=2, v[1]=p}

  {easy out for k<=0, k<0 is handled as k=0}
  if mp_cmp_d(k,1)=MP_LT then begin
    {v[0] = 2}
    mp_set(vk,2);
    exit;
  end;

  mp_init5(v0,v1,x,t,mu);
  if mp_error<>0 then exit;

  {Setup Barrett reduction}
  mp_reduce_setup(mu, n);

  {Paranoia: Initialize with _mod (not really needed if MPC_ArgCheck)}
  {v0 = p}
  mp_mod(p,n,v0);

  {start with x=q}
  mp_mod(q,n,x);

  {v1 = p*p - 2q}
  mp_sqrmod(p,n,v1);
  mp_mul_2(q,t);
  mp_submod(v1,t,n,v1);

  {loop is never executed for k=1}
  for i:=mp_bitsize(k)-2 downto 0 do begin
    if mp_error<>MP_OKAY then break;
    if mp_isbit(k,i) then begin
      {v0 = v0*v1 - x*p}
      mp_mul(v0,v1,v0);     mp_reduce(v0,n,mu);
      mp_mul(x,p,t);        mp_reduce(t,n,mu);
      mp_sub(v0,t,v0);      while (v0.sign=MP_NEG) and (mp_error=MP_OKAY) do mp_add(v0,n,v0);
      {v1 = v1*v1 - 2x*q}
      mp_sqr(v1,v1);        mp_reduce(v1,n,mu);
      mp_mul(x,q,t);        mp_reduce(t,n,mu);
      mp_mul_2(t,t);
      mp_sub(v1,t,v1);      while (v1.sign=MP_NEG) and (mp_error=MP_OKAY) do mp_add(v1,n,v1);
      if i<>0 then begin
        {x = x*x*q}
        mp_sqr(x,x);        mp_reduce(x,n,mu);
        mp_mul(x,q,x);      mp_reduce(x,n,mu);
      end;
    end
    else begin
      {v1 = v0*v1 - x*p}
      mp_mul(v0,v1,v1);     mp_reduce(v1,n,mu);
      mp_mul(x,p,t);        mp_reduce(t,n,mu);
      mp_sub(v1,t,v1);      while (v1.sign=MP_NEG) and (mp_error=MP_OKAY) do mp_add(v1,n,v1);
      {v0 = v0*v0 - 2x}
      mp_sqr(v0,v0);        mp_reduce(v0,n,mu);
      mp_mul_2(x,t);
      mp_sub(v0,t,v0);      while (v0.sign=MP_NEG) and (mp_error=MP_OKAY) do mp_add(v0,n,v0);
      if i<>0 then begin
        {x = x*x}
        mp_sqr(x,x);        mp_reduce(x,n,mu);
      end;
    end;
  end;
  if mp_error=MP_OKAY then mp_exch(v0,vk); {store v[k]}
  mp_clear5(v0,v1,x,t,mu);
end;


{---------------------------------------------------------------------------}
procedure mp_mersenne(n: longint; var mn: mp_int);
  {-return nth Mersenne number, mn = 2^n-1, MP_RANGE err for n>MaxMersenne}
begin
  mp_2expt(mn, n);
  mp_dec(mn);
end;


{---------------------------------------------------------------------------}
procedure mp_miller_rabin(const n: mp_int; t: word; var prime: boolean);
  {-Miller-Rabin test of n, security parameter t, from HAC p. 139 Alg.4.24}
  { if t=0, calculate t from bit_count with prob of failure < 2^-96}
label
  leave;
var
  n1, y, r: mp_int;
  s,j: longint;
const
  t0: array[0..14] of byte = (50,28,16,10,7,6,5,4,4,3,3,3,3,3,2);
begin
  {default}
  prime := false;

  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(n) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_miller_rabin');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {easy case n < 2^31 or even}
  if mp_is_longint(n,s) then begin
    if s>1 then prime := IsPrime32(s);
    exit;
  end;
  if mp_iseven(n) or (n.sign=MP_NEG) then exit;

  mp_init3(n1,r,y);
  if mp_error<>MP_OKAY then exit;

  {get n1 = n - 1}
  mp_sub_d(n,1,n1);

  {calculate r,s with n-1=2^s*r}
  mp_makeodd(n1,r,s);

  if t=0 then begin
    {calculate t from bit_count with prob of failure lower than 2^-96}
    j := mp_bitsize(n) div 128;
    if j>14 then t:=14 else t:= word(j);
    t := t0[t];
  end;

  while t>0 do begin
    {generate a in the range 2<=a<n-1, calculate y = a^r mod n}
    repeat
      mp_rand(y, n.used);
      mp_mod(y,n1,y);
      if mp_error<>MP_OKAY then goto leave;
    until mp_cmp_d(y,1)=MP_GT;
    mp_exptmod(y, r, n, y);
    if mp_error<>MP_OKAY then goto leave;

    {if y<>1 and y<>n-1 do}
    if (not mp_is1(y)) and mp_is_ne(y, n1) then begin
      j := 1;
      while (j <= s-1) and mp_is_ne(y, n1) do begin
        mp_sqrmod(y, n, y);
        {if y=1 then composite}
        if mp_is1(y) or (mp_error<>MP_OKAY) then goto leave;
        inc(j);
      end;
      {if y<>n1 then composite}
      if (mp_is_ne(y, n1)) or (mp_error<>MP_OKAY) then goto leave;
    end;
    dec(t);
  end;
  {probably prime now}
  prime := true;

leave:
  mp_clear3(n1,r,y);
end;


{---------------------------------------------------------------------------}
procedure mp_mulmod(const a,b,c: mp_int; var d: mp_int);
  {-calculate d = a * b (mod c)}
var
  t: mp_int;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(b) or mp_not_init(c) or mp_not_init(d) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_mulmod');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  mp_init(t);
  if mp_error<>MP_OKAY then exit;

  mp_mul(a, b, t);
  mp_mod(t, c, d);
  mp_clear(t);
end;


{---------------------------------------------------------------------------}
procedure mp_OddProd(a,b: word; var p: mp_int);
  {-calculate p=prod(2*i+1),i=a+1...b;  p=1 if a>=b}
const
  MAXL = 14;
var
  st : array[0..MAXL] of mp_int; {local 'stack', avoids init/clear in product}
  lev: integer;                  {stack level}

  procedure OProd(a,b: word; var p: mp_int);
    {-calculate f(a+1)*f(a+2)*..f(b-1)*f(b), f(i)=2*i+1}
  const
    dmin={$ifdef BIT16}8{$else}32{$endif}; {min value of d for recursion}
  var
    d: word;
  begin
    if mp_error<>MP_OKAY then exit;
    d := b-a;
    if d<dmin then begin
      a := 2*b+1;
      mp_set_w(p,a);
      while d>1 do begin
        dec(a,2);
        mp_mul_w(p,a,p);
        dec(d);
      end;
    end
    else begin
      if lev>=MAXL then begin
        {$ifdef MPC_HaltOnError}
          {$ifdef MPC_UseExceptions}
            raise MPXRange.Create('mp_OddProd: lev out of range');
          {$else}
            RunError(MP_RTE_RANGE);
          {$endif}
        {$else}
          set_mp_error(MP_RANGE);
          exit;
        {$endif}
      end;
      inc(lev);
      d := (b+a) shr 1;
      OProd(a,d,p);
      if mp_error=MP_OKAY then begin
        OProd(d,b,st[lev]);
        mp_mul(p,st[lev],p);
      end;
      dec(lev);
    end;
  end;

begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(p) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_OddProd');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if a<b then begin
    lev := -1;
    mp_init_multi(st);
    OProd(a,b,p);
    mp_clear_multi(st);
  end
  else mp_set1(p);
end;


{---------------------------------------------------------------------------}
procedure s_mp_pell4(const d: mp_int; var x,y: mp_int; var r: integer);
  {-Calculate the smallest non-trivial solution of  x^2 - d*y^2 = r}
  { r in [-4,+4,-1,+1]; returns r=0 if d<2 or d is a square. Uses  }
  { continued fraction expansion of sqrt(d) from Forster [4], ch.25}
  { 'Internal' procedure: Assumes d,x,y are initialized and @x<>@y.}
var
  a, w, q, q0, m: mp_int;
  x0, y0, t:  mp_int;
  sign: integer;
  rd: mp_digit;
begin

  r := 0;
  if (mp_error<>MP_OKAY) or (d.sign=MP_NEG) then exit;

  {special cases are d=5,13. But 13 fits in the algorithm framework}
  {d=5 must be treated separately: solution is x=1, y=1, r=-4}
  if mp_cmp_d(d,5)=MP_EQ then begin
    mp_set1(x);
    mp_set1(y);
    r :=-4;
    exit;
  end;

  mp_init8(a, w, q, q0, m, x0, y0, t);

  {calculate w=floor(sqrt(d)), return 0 if d is square}
  s_mp_sqrtrem(d, w, q);
  if mp_is0(q) then begin
    mp_clear8(a, w, q, q0, m, x0, y0, t);
    exit;
  end;

  {Initialize CF algorithm:
   q0 := 1; m := w;
   x0 := 1; x := m;
   y0 := 0; y := 1;}

  mp_set1(q0);  mp_copy(w, m);
  mp_set1(x0);  mp_copy(w, x);
  mp_zero(y0);  mp_set1(y);
  sign := -1;

  {Note 1: the variables with index 1 from [4] are not needed and }
  {swaps are implemented via mp_exch instead of chain assignments.}

  {Note 2: theoretically it is possible to use only one x or y in }
  {the loop instead of both and finally calculate the second from }
  {x^2 - d*y^2 = r. But as long as there is no faster sqrt routine}
  {in this library this alternative is almost always slower.      }

  {loop while q<>1 and q<>4, rd=0 if q=0 or q>MP_DIGIT_MAX, else rd=q}
  if q.used<>1 then rd:=0 else rd := q.pdigits^[0];
  while (mp_error=MP_OKAY) and (rd<>1) and (rd<>4) do begin
    { a := (m + w) div q;}
    mp_add(m,w,a);   mp_div(a,q,a);
    { m1 := a*q -  m;   q1 := q0 + a*(m - m1); }
    { x1 := a*x + x0;   y1 := a*y + y0;        }
    { m  := m1;  q0 := q;    q := q1;          }
    { x0 := x;    x := x1;  y0 := y;  y := y1; }
    mp_mul(a,q,t);   mp_sub(t,m,t);   mp_sub(m,t,m);  mp_exch(m,t);
    mp_mul(a,t,t);   mp_add(t,q0,t);  mp_exch(q,q0);  mp_exch(q,t);
    mp_mul(a,x,t);   mp_add(t,x0,t);  mp_exch(x,x0);  mp_exch(x,t);
    mp_mul(a,y,t);   mp_add(t,y0,t);  mp_exch(y,y0);  mp_exch(y,t);
    {update sign and get next rd}
    sign := -sign;
    if q.used<>1 then rd:=0 else rd := q.pdigits^[0];
  end;

  {if no error calculate final r, avoid D6+ warnings}
  if mp_error=MP_OKAY then r := sign*integer(rd and $7f);
  mp_clear8(a, w, q, q0, m, x0, y0, t);

end;


{---------------------------------------------------------------------------}
procedure mp_pell(const d: mp_int; var x,y: mp_int);
  {-Calculate the smallest non-trivial solution of the Pell equation}
  { x^2 - d*y^2 = 1; error if d<2, if d is a square, or if @x = @y.}
var
  c: integer;
  t,x2: mp_int;
begin
  {Checks in mp_pell4}
  mp_pell4(d,x,y,c);
  if abs(c)=4 then begin
    {calculate ((x+y*sqrt(d))/2)^3}
    mp_init2(t,x2);
    if mp_error=MP_OKAY then begin
      c := c div 4;
      {here c=-1 or 1}
      {x' = x*(x^2-3c)/2}
      {y' = y*(x^2- c)/2}
      mp_sqr(x,x2);
      {t=x2-3*c}
      if c<0 then mp_add_d(x2,3,t) else mp_sub_d(x2,3,t);
      mp_shr(t,1,t);
      mp_mul(x,t,x);
      {t=x2-c}
      if c<0 then mp_add_d(x2,1,t) else mp_sub_d(x2,1,t);
      mp_shr(t,1,t);
      mp_mul(y,t,y);
      mp_clear2(t,x2);
    end;
  end;
  if (c<0) and (mp_error=MP_OKAY) then begin
    {calculate (x+y*sqrt(d))^2, here is x^2 - d*y^2 = -1}
    {x' = 2x^2+1}
    {y' = 2xy   }
    mp_mul(y,x,y);
    mp_shl1(y);
    mp_sqr(x,x);
    mp_shl1(x);
    mp_inc(x);
  end;
end;


{---------------------------------------------------------------------------}
procedure mp_pell4(const d: mp_int; var x,y: mp_int; var r: integer);
  {-Calculate the smallest non-trivial solution of  x^2 - d*y^2 = r,}
  { r in [-4,+4,-1,+1]. Uses continued fraction expansion of sqrt(d)}
  { from Forster [4]. Error if d<2, if d is a square, or if @x = @y.}
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(d) or mp_not_init(x) or mp_not_init(y) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_pell4');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if @x=@y then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXBadArg.Create('mp_pell4: @x=@y');
      {$else}
        RunError(MP_RTE_BADARG);
      {$endif}
    {$else}
      set_mp_error(MP_BADARG);
      exit;
    {$endif}
  end;
  {call internal procedure, error if r=0}
  s_mp_pell4(d,x,y,r);
  if (mp_error=MP_OKAY) and (r=0) then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXBadArg.Create('mp_pell4: invalid d');
      {$else}
        RunError(MP_RTE_BADARG);
      {$endif}
    {$else}
      set_mp_error(MP_BADARG);
      exit;
    {$endif}
  end;
end;


{---------------------------------------------------------------------------}
procedure mp_ppexpo(var x: mp_int; B0,B1: word);
  {-product of primes B0 < p <= B1 and integers isqrt(B0) < n <= isqrt(B1)}
var
  m0, m1, i: word;
  function isqrt(w: word): word;
  begin
    {No error possible}
    isqrt := word(trunc(sqrt(w)));
  end;
begin
  if mp_error<>MP_OKAY then exit;
  {Ref: Forster[4], 14, "Die (p-1)-Faktorisierungs-Methode"}

  {$ifdef MPC_ArgCheck}
    if mp_not_init(x) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_ppexpo');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  mp_set1(x);
  if mp_error=MP_OKAY then begin
    m0 := isqrt(B0)+1; if m0<2 then m0:=2;
    m1 := isqrt(B1);
    for i := m0 to m1 do begin
      mp_mul_w(x,i,x);
      if mp_error<>MP_OKAY then break;
    end;
    if odd(B0) then inc(B0);
    i := B0+1;
    while (i<=B1) and (mp_error=MP_OKAY) do begin
      if IsPrime16(i) then mp_mul_w(x,i,x);
      inc(i,2);
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure bigprime_pm1(const N: mp_int; var y,d: mp_int; bound: longint);
  {-big prime stage for Pollard p-1, find factor d or d=0 if no success}
label
  found;
{$ifdef BIT32}
const
  MAXHDIFF=66;
  MAXBOUND=2000000;    {148932 odd primes}
{$else}
const
  MAXHDIFF=57;
  MAXBOUND=1000000;    { 78497 odd prime}
{$endif}
var
  i,count: integer;
  q,q0: longint;                    {current, prev prime}
  x: array[0..MAXHDIFF] of mp_int;  {x[i] = y^(2i) mod N}
  t,z,mu: mp_int;
  cancel: boolean;
begin
  {Ref: Forster[4], 14, "Die (p-1)-Faktorisierungs-Methode"}
  {Note: Forster uses a file with the halved differences of consecutive}
  {primes, here the primes are obtained by calls to nextprime32}
  if mp_error<>MP_OKAY then exit;

  {$ifdef MPC_ArgCheck}
    if mp_not_init(N) or mp_not_init(y) or mp_not_init(d)  then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('bigprime_pm1');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  mp_init3(t,z,mu);
  if mp_error<>MP_OKAY then exit;

  mp_init_multi(x);
  if mp_error<>MP_OKAY then begin
    mp_clear3(t,z,mu);
    exit;
  end;

  {Get Barret parameter}
  mp_reduce_setup(mu, N);

  {z=y^2}
  mp_sqrmod(y,N,z);
  {calculate x[i]=y^(2i) mod N}
  mp_set1(x[0]);
  for i:=1 to MAXHDIFF do mp_mulmod(x[i-1],z,N,x[i]);

  {Note: if MAXBOUND is increased the maximum halved difference MAXHDIFF}
  {      should also be adjusted}
  if bound>MAXBOUND then bound:=MAXBOUND;
  q := 3;
  count := 0;
  mp_exptmod_d(y,q,N,y);
  mp_sub_d(y,1,z);
  {Number of GCDs about 150 for 32Bit, 80 for 16Bit}
  while q<bound do begin
    q0 := q;
    q := nextprime32(q0+1);
    i := (q-q0) shr 1;
    {q0 = prev prime, q= current prime, i = difference/2}
    if i>MAXHDIFF then break;
    mp_mul(y,x[i],y);   mp_reduce(y,N,mu);
    {z = z*(y-1) mod N = (z*y)-z mod N}
    mp_mul(z,y,t);      mp_reduce(t,N,mu);
    mp_sub(t,z,z);      if z.sign=MP_NEG then mp_add(z,N,z);
    inc(count);
    {Note: Forster only calculates the gcd if q>=1000}
    if (count>=1000) or (q>=bound) then begin
      mp_gcd(z,N,d);
      if mp_cmp_d(d,1)=MP_GT then begin
        {only nontrivial factors 1 < d < N}
        if mp_cmp_mag(d,N)=MP_LT then goto found;
      end;
      if mp_show_progress then begin
        if ProgressAssigned then begin
          cancel := false;
          mp_progress(false, q, bound, cancel);
          if cancel then break;
        end
        else write('.');
      end;
      count := 0;
    end;
  end;

  {loop exceeded or i>MAXHDIFF}
  mp_zero(d);

found:
  mp_clear_multi(x);
  mp_clear3(t,z,mu);
end;


{---------------------------------------------------------------------------}
procedure mp_pollard_pm1(const N: mp_int; var f: mp_int; bound: word);
  {-find a factor f of N with p-1 method, f=0 if no factor found}
  { primes <= bound will be included in pp_expo product}
const
  anz0=128;
label
  leave;
var
  B0,B1: word;
  base,x: mp_int;
  cancel: boolean;
begin

  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(N) or mp_not_init(f)  then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_pollard_pm1');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {Easy out}
  if mp_cmp_mag_d(N,4)=MP_LT then begin
    mp_abs(N,f);
    exit;
  end;

  {Ref: Forster[4], 14, "Die (p-1)-Faktorisierungs-Methode"}
  mp_init2(base,x);
  if mp_error<>MP_OKAY then exit;

  mp_rand(base, pred(N.used));
  mp_add_d(base,2,base);
  mp_gcd(base,N,f);

  {exit if non trivial factor}
  if mp_cmp_d(f,1)=MP_GT then goto leave;

  if bound>$7FF0 then bound:=$7FF0;
  B0 := 0;
  while (mp_error<>MP_OKAY) and (B0<bound) do begin
    B1 := B0+anz0;
    if B1>bound then B1:=bound;
    mp_ppexpo(x, B0, B1);
    mp_exptmod(base,x,N,base);
    if mp_is1(base) then begin
      mp_zero(f);
      goto leave;
    end;
    if mp_show_progress then begin
      if ProgressAssigned then begin
        cancel := false;
        mp_progress(false, B0, 0, cancel);
        if cancel then break;
      end
      else write('.');
    end;
    mp_sub_d(base,1,x);
    mp_gcd(x,N,f);
    if mp_cmp_d(f,1)=MP_GT then goto leave;
    inc(B0, anz0);
  end;
  if mp_is1(f) then bigprime_pm1(N, base, f, longint(bound)*100);

leave:

 mp_clear2(x,base);
end;


{---------------------------------------------------------------------------}
procedure mp_pollard_rho(const N: mp_int; var f: mp_int; cnt: longint);
  {-find a factor f of N, f=0 if no factor found; cnt: number accumulation cycles}
label
  leave;
const
  acc = 256;
var
  p,x,y,d,m: mp_int;
  k: integer;
  i: longint;
  cancel: boolean;
begin
  if mp_error<>MP_OKAY then exit;

  {Ref: Forster[4], 13, "Die Pollard'sche-Rho-Methode"}
  {New WE version using Barrett reduction}

  {$ifdef MPC_ArgCheck}
    if mp_not_init(N) or mp_not_init(f) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_pollard_rho');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {Easy out}
  if mp_cmp_mag_d(N,4)=MP_LT then begin
    mp_abs(N,f);
    exit;
  end;

  mp_init5(m,p,x,y,d);
  if mp_error<>MP_OKAY then exit;

  {See Forster's poll_rho}
  {øøøøøøøøøøøøøøøøøøøøøø}

  {N.used>0!, x<N}
  mp_rand(x, pred(N.used));
  mp_copy(x,y);
  if mp_error=MP_OKAY then begin
    {Setup Barrett reduction, Forster does not use Barrett}
    mp_reduce_setup(m, N);
    for i:=1 to cnt do begin
      {accumulate differences}
      mp_set1(p);
      {if error burn some cycles}
      for k:=1 to acc do begin
        if mp_error<>MP_OKAY then goto leave;
        {x*x+2 <= (N-1)^2 + 2 = N*N - 2*N + 3 <= N*N - 1 for N>3}
        {same for y, so Barrett can be used}
        {x = x*x + 2 mod N}
        mp_sqr(x,x);
        mp_add_d(x,2,x);
        mp_reduce(x,N,m);
        {y = y*y + 2 mod N}
        mp_sqr(y,y);
        mp_add_d(y,2,y);
        mp_reduce(y,N,m);
        {y = y*y + 2 mod N}
        mp_sqr(y,y);
        mp_add_d(y,2,y);
        mp_reduce(y,N,m);
        {p = p*(y-x) mod N}
        mp_sub(y,x,d); d.sign := MP_ZPOS;
        mp_mul(p,d,p);
        mp_reduce(p,N,m);
      end;

      mp_gcd(p,N,f);
      if mp_show_progress then begin
        if ProgressAssigned then begin
          cancel := false;
          mp_progress(false, i, 0, cancel);
          if cancel then break;
        end
        else write('.');
      end;

      {factor found if 1 < f < N}
      if (mp_cmp_d(f,1)=MP_GT) and (mp_cmp_mag(f,N)=MP_LT) then goto leave;
    end;
    {f=0 indicates no factor found}
    mp_zero(f);
  end;

leave:
  mp_clear5(m,p,x,y,d);
end;


{---------------------------------------------------------------------------}
procedure mp_primorial(n: longint; var a: mp_int);
  {-Primorial of n;  a = n# = product of primes <= n.}
{$ifdef BIT32}
const
  NMAX=32000;   {pi(376131)=32000, change this if MaxPrimorial>376131}
{$else}
const
  NMAX=16000;   {to partial products for n>=176087}
{$endif}
type
  TPTab = array[1..NMAX] of longint;
var
  m: longint;
  t : mp_int;
  pp: ^TPTab;
  ctx: TPrimeContext;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_primorial');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  mp_set1(a);
  if n<2 then exit;

  {Initialize FindPrime context and allocate memory for array of primes}
  FindFirstPrime32(2, ctx);
  if n>=mp_primor_cutoff then pp := IAlloc(sizeof(TPTab))
  else pp := nil;

  if pp=nil then begin
    {no prime array available, use iterative multiplication}
    while (ctx.prime<=n) and (mp_error=MP_OKAY) do begin
      mp_mul_int(a,ctx.prime,a);
      FindNextPrime32(ctx);
    end;
    exit;
  end;

  {Here we can use the fast mp_prod_int procedure}
  m := 0;
  mp_init(t);
  while (ctx.prime<=n) and (mp_error=MP_OKAY) do begin
    inc(m);
    pp^[m] := ctx.prime;
    if m=NMAX then begin
      {array capacity reached, get partial product and accumulate it in a}
      mp_prod_int(t,pp^,m);
      mp_mul(a,t,a);
      {Reset array index}
      m := 0;
    end;
    FindNextPrime32(ctx);
  end;
  {accumulate remaining primes into a}
  if m>0 then begin
    mp_prod_int(t,pp^,m);
    mp_mul(a,t,a);
  end;

  mp_clear(t);
  freemem(pp,sizeof(TPTab));
end;


{---------------------------------------------------------------------------}
procedure mp_provable_prime(bits: longint; var p: mp_int);
  {-Generate a random provable prime p with bitsize bits using Maurer's algorithm}
var
  a,q,r: mp_int;

  {-------------------------------------------------------}
  procedure ProvablePrime(k: longint);
    {-Actual recursive core procedure}
  var
    kr: longint;
    f: mp_digit;
    i: integer;
    success: boolean;
  const
    smax = mp_digit(MP_DIGIT_MAX and $3FFF);
    imax = 32;
  begin
    {Generate provable prime with bitsize k using Maurer's algorithm based}
    {on Pocklington's theorem. Simplified version of Alg. 4.62 from HAC[5]}

    {Although ProvablePrime is recursive, the structure of the algorithm}
    {allows the mp_ints to be allocated only once in the outer procedure}

    {Step 1: Special treatment of small bitsizes}
    if k<=31 then begin
      {Step 1.1: Select a random k bit integer}
      mp_rand_bits(p,k);
      {Step 1.2/3: Return next prime}
      {Note that mp_nextprime is exact for bitsizes <= 31}
      mp_nextprime(p);
      {$ifdef MPC_TRACE}
        mp_tracevv(#13#10'provable prime of bitsize '+mp_int2str(mp_bitsize(p))+#13#10);
      {$endif}
      exit;
    end;

    {Omit steps 2,3,4: Use fix values B = smax, r = 0.5}
    {Step 5: q = provable_prime(1+floor(r*k)) }
    ProvablePrime(1 + k div 2);

    {Exchange p and q because result of ProvablePrime is returned in p}
    mp_exch(p,q);

    {Get remaining number of bits to generate}
    kr := k - mp_bitsize(q);

    success := false;
    {Step 8}
    while not success do begin
      {Step 8.1: Select next candidate p}
      {try upto imax times to get bitsize(p)=k}
      for i:=1 to imax do begin
        mp_rand_bits(r,kr);
        mp_shl1(r);
        {note that from here r = 2R, R as in HAC, p = 2*R*q + 1}
        mp_mul(r,q,p);
        mp_inc(p);
        if k=mp_bitsize(p) then break;
      end;
      {Step 8.2.a: Use trial division to cast out non-primes}
      mp_small_factor(p, 3, smax, f);
      if f=0 then begin
        {Step 8.2.b: Select random integer >=2}
        mp_rand_bits_ex(a,k-1,false);
        mp_add_d(a,2,a);
        {In the remaining steps 8.2 Pocklington's theorem is used. We have to}
        {compute b1 = a^(n-1) mod p and b2 = a^r mod p. Since n-1=q*r, first}
        {compute b2 = a' = a^r mod p, then b1 = r' = a'^q mod p}
        mp_exptmod(a,r,p,a);
        mp_exptmod(a,q,p,r);
        {check if b1 = r' = a^(n-1) = 1}
        if mp_is1(r) then begin
          {calculate gcd(a^r-1,p) = gcd(a'-1,p)}
          mp_dec(a);
          if mp_gcd1(a,p,r) then begin
            {$ifdef MPC_TRACE}
              mp_tracevv(#13#10'provable prime of bitsize '+mp_int2str(mp_bitsize(p))+#13#10);
            {$endif}
            success := true;
          end;
        end
        else begin
          {$ifdef MPC_TRACE}
            mp_tracevvv('+');
          {$endif}
        end;
      end
      else begin
        {found small factor}
        {$ifdef MPC_TRACE}
          mp_tracevvv('.');
        {$endif}
      end
    end;
  end;

begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(p) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_provable_prime');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  mp_init3(a,q,r);
  if mp_error=MP_OKAY then begin
    ProvablePrime(bits);
    mp_clear3(a,q,r);
  end;
end;


{---------------------------------------------------------------------------}
procedure mp_rand_prime(bitsize: word; pt: TPrimeType; var p: mp_int);
  {-generate random (BPSW pseudo) prime of bitsize > 3, pt: prime type of p}
var
  q: mp_int;
  L: longint;
  f: mp_digit;
  bl: byte;
  done: boolean;
const
  fmaxp = mp_digit(MP_DIGIT_MAX and $3FF);
  fmaxq = mp_digit(MP_DIGIT_MAX and $3FF);
  Mask7 = mp_digit(MP_DIGIT_MAX and (not 7));
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(p) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_rand_prime');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  if bitsize<4 then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mp_rand_prime: bitsize<4');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;

  {if safe prime is requested, initialize q}
  if pt=pt_safe then mp_init(q);

  if mp_error=MP_OKAY then begin
    if (pt=pt_3mod4) or (pt=pt_safe) then bl:=3 else bl:=1;
    done := false;
    repeat
      mp_rand_bits(p,bitsize);
      if pt=pt_1mod8 then p.pdigits^[0] := (p.pdigits^[0] and Mask7) or 1
      else p.pdigits^[0] := p.pdigits^[0] or bl;
      if mp_is_longint(p,L) then begin
        {handle small bit sizes}
        done := (L>0) and IsPrime32(L);
        if pt=pt_safe then done := done and IsPrime32((L-1) div 2);
      end
      else if pt=pt_safe then begin
        {first test if p has small factor}
        mp_small_factor(p,2,fmaxp,f);
        if f=0 then begin
          {calculate q = (p-1)/2}
          mp_sub_d(p,1,q);
          mp_div_2(q,q);
          {get small factor of q}
          mp_small_factor(q,2,fmaxq,f);
          {p,q prime if both have no small factors and are BPSW pseudo primes}
          {Note: std.inc has short-circuit boolean expression evaluation}
          done := (f=0) and mp_is_spsp_d(q,2) and mp_is_spsp_d(p,2) and mp_is_slpsp(q) and mp_is_slpsp(p);
        end;
      end
      else done := mp_is_pprime_ex(p, fmaxp);
    until done or (mp_error<>MP_OKAY);
    if pt=pt_safe then mp_clear(q);
  end;
end;


{---------------------------------------------------------------------------}
procedure mp_rnr(const a,m: mp_int; var x,y: mp_int);
  {-rational number reconstruction: for m>0 calculate x,y with a*x=y mod m,}
  { gcd(x,y)=1, 0<=x,|y|<sqrt(m/2), x<>0. x=y=0 if no (unique) solution exists.}
var
  u2,u3,v2,v3,q,sqrtm2: mp_int;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(m) or mp_not_init(x) or mp_not_init(y) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_rnr');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {m must be positive}
  if s_mp_is_le0(m) then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mp_rnr: m <= 0');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;

  mp_init6(u2,u3,v2,v3,q,sqrtm2);
  if mp_error<>MP_OKAY then exit;

  {The following algorithm for rational reconstruction is from Knuth [3],   }
  {Section 4.5.3, Exercise 51. Quote from the answer:                       }

  {'This discussion proves that the problem can be solved efficiently by    }
  {applying Algorithm 4.5.2X with u = m and v = a, but with the following   }
  {replacement for step X2: "If v3 <= sqrt(m/2), the algorithm terminates.  }
  {The pair (x,y) = (|v2|,v3*sign(v2)) is then the unique solution, provided}
  {that gcd(x,y)=1 and x <= sqrt(m/2); otherwise there is no solution."'    }

  {Knuth's Algorithm 4.5.2X is the extended Euclidean algorithm. Here the   }
  {usage of u1,v2 is suppressed and the main loop is simplified via mp_exch }

  {initialize, (v2,v3) = (1,a), force v3 = a mod m into range 0..m-1}
  mp_set(v2,1);
  if (a.sign=MP_NEG) or (mp_cmp_mag(a,m)>=0) then mp_mod(a,m,v3)
  else mp_copy(a,v3);

  {initialize, (u2,u3) = (0,m)}
  mp_set(u2,0);
  mp_copy(m,u3);

  {calculate sqrt(m/2)}
  mp_shr(u3,1,q);
  mp_sqrt(q,sqrtm2);

  {loop invariants  u3>=0, v3>=0}
  while (mp_error=MP_OKAY) and (mp_cmp_mag(v3,sqrtm2)>0) do begin
    mp_divrem(u3, v3, @q, @u3);
    mp_mul(v2, q, q);
    mp_sub(u2, q, u2);
    mp_exch(u2, v2);
    mp_exch(u3, v3);
  end;

  {check if gcd(x,y)=1 and x < sqrt(m/2)}
  if (mp_error=MP_OKAY) and (mp_cmp_mag(v2,sqrtm2)<=0) and mp_gcd1(v2,v3,q) then begin
    mp_abs(v2,x);
    if v2.sign=MP_NEG then mp_chs(v3,y) else mp_copy(v3,y);
  end
  else begin
    mp_zero(x);
    mp_zero(y);
  end;
  mp_clear6(u2,u3,v2,v3,q,sqrtm2);
end;


{---------------------------------------------------------------------------}
procedure mp_rnr2(const a,m,NN,DD: mp_int; var n,d: mp_int);
  {-rational number reconstruction: for m,NN,DD > 0 calculate co-prime d,n}
  { with a*d=n mod m, |n|<=N, 0<d<=DD, i.e. a=n/d mod m. Return d=n=0 if }
  { no solution exists. The reconstruction is unique if 2*NN*DD < m.}
var
  u2,u3,v2,v3,q: mp_int;
begin
  { This is a generalization of procedure mp_rnr. For a discussion see e.g.}
  { M. Monagan, Maximal quotient rational reconstruction: an almost optimal}
  { algorithm for rational reconstruction. In the Proceedings of ISSAC 2004}
  { pp. 243-249. Available from http://www.cecm.sfu.ca/CAG/papers/MQRR.pdf }

  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(m) or mp_not_init(NN) or mp_not_init(DD) or mp_not_init(d) or mp_not_init(n) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_rnr2');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {m must be positive}
  if s_mp_is_le0(m) then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mp_rnr2: m <= 0');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;

  {NN must be positive}
  if s_mp_is_le0(NN) then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mp_rnr2: NN <= 0');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;

  {DD must be positive}
  if s_mp_is_le0(DD) then begin
    {$ifdef MPC_HaltOnError}
      {$ifdef MPC_UseExceptions}
        raise MPXRange.Create('mp_rnr2: DD <= 0');
      {$else}
        RunError(MP_RTE_RANGE);
      {$endif}
    {$else}
      set_mp_error(MP_RANGE);
      exit;
    {$endif}
  end;

  mp_init5(u2,u3,v2,v3,q);
  if mp_error<>MP_OKAY then exit;

  {initialize, (v2,v3) = (1,a), force v3 = a mod m into range 0..m-1}
  mp_set(v2,1);
  if (a.sign=MP_NEG) or (mp_cmp_mag(a,m)>=0) then mp_mod(a,m,v3)
  else mp_copy(a,v3);

  {initialize, (u2,u3) = (0,m)}
  mp_set(u2,0);
  mp_copy(m,u3);

  {loop invariants:  u3>=0, v3>=0}
  while (mp_error=MP_OKAY) and (mp_cmp_mag(v3,NN)>0) do begin
    mp_divrem(u3, v3, @q, @u3);
    mp_mul(v2, q, q);
    mp_sub(u2, q, u2);
    mp_exch(u2, v2);
    mp_exch(u3, v3);
  end;

  {check if gcd(d,n)=1 and |d| <= DD}
  if (mp_error=MP_OKAY) and (mp_cmp_mag(v2,DD)<=0) and mp_gcd1(v2,v3,q) then begin
    mp_abs(v2,d);
    if v2.sign=MP_NEG then mp_chs(v3,n) else mp_copy(v3,n);
  end
  else begin
    mp_zero(d);
    mp_zero(n);
  end;
  mp_clear5(u2,u3,v2,v3,q);
end;


{---------------------------------------------------------------------------}
procedure mp_set_progress(const PP: TProgressProc);
  {-make PP the new progress proc}
begin
  mp_progress := PP;
end;


{---------------------------------------------------------------------------}
procedure mp_small_factor(const a: mp_int; f0,fmax: mp_digit; var f: mp_digit);
  {-compute small digit prime factor or 0, f0..fmax, f will be <= min(fmax,$7FFF)}
var
  i,imax,imin: word;
  d,q: mp_digit;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_small_factor');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {assume no error}
  if fmax<mp_max_small then imax := word(fmax) else imax := word(mp_max_small);

  if f0=0 then imin := 2
  else begin
    if f0>imax then imin:=imax else imin := word(f0);
  end;

  {easy outs}
  f := 0;
  if mp_cmp_mag_d(a,2)=MP_LT then exit;
  if imin=2 then begin
    if mp_iseven(a) then begin
      f := 2;
      exit;
    end
    else imin := 3;
  end;

  {imin>2}
  i := imin;
  if i and 1 = 0 then inc(i);
  while i<=imax do begin
    if IsPrime16(i) then begin
      {Note: i<=imax<=MP_DIGIT_MAX}
      d := mp_digit(i);
      mp_mod_d(a, d, q);
      if mp_error<>MP_OKAY then exit;
      if q=0 then begin
        {return the factor}
        f := d;
        exit;
      end;
    end;
    inc(i,2);
  end;
end;


{---------------------------------------------------------------------------}
procedure mp_sqrmod(const a,c: mp_int; var d: mp_int);
  {-calculate d = a * a (mod c)}
var
  t: mp_int;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(c) or mp_not_init(d) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_sqrmod');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  mp_init(t);
  if mp_error=MP_OKAY then begin
    mp_sqr(a, t);
    mp_mod(t, c, d);
    mp_clear(t);
  end;
end;


{---------------------------------------------------------------------------}
{-------------------  Start of mp_sqrtmod routines  ------------------------}
{---------------------------------------------------------------------------}

{---------------------------------------------------------------------------}
procedure mp_sqrtmod34(const g,p: mp_int; var z: mp_int);
  {-calculate z with z*z = g mod p, p prime > 0, p mod 4 = 3; *internal*}
var
  t: mp_int;
begin
  mp_init_copy(t,p);
  if mp_error=MP_OKAY then begin
    {Algorithm from P1363/D8 [25], A.2.5, I.}
    mp_inc(t);
    {t = (p+1)/4}
    mp_shr(t,2,t);
    {g^(1/2) = g^((p+1)/4) mod p}
    mp_exptmod(g,t,p,z);
    mp_clear(t);
  end;
end;


{---------------------------------------------------------------------------}
procedure mp_sqrtmod58(const g,p: mp_int; var z: mp_int);
  {-calculate z with z*z = g mod p, p prime = 5 mod 8; *internal*}
var
  r,s,t: mp_int;
begin
  mp_init3(r,s,t);
  if mp_error=MP_OKAY then begin
    {Algorithm from P1363/D8 [25], A.2.5, II.}
    mp_copy(p,s);
    mp_copy(g,r);           {r = g}
    mp_mul_2(g,t);          {t = 2g}
    mp_sub_d(s,5,s);        {s = (p-5)/8}
    mp_shr(s,3,s);
    mp_exptmod(t,s,p,t);    {t = (2g)^(p-5)/8}
    mp_sqrmod(t,p,s);       {s = 2gt^2}
    mp_mul_2(s,s);
    mp_mulmod(s,r,p,s);
    {z = g*t*(s-1) mod p,   [z = g*gamma*(i-1) in P1363]}
    {if s=0 use s-1=p-1}
    if s.used=0 then mp_sub_d(p,1,s) else mp_dec(s);
    mp_mulmod(r,t,p,r);
    mp_mulmod(r,s,p,z);
    mp_clear3(r,s,t);
  end;
end;


{---------------------------------------------------------------------------}
function mp_sqrtmod18_shanks_intern(const a,p: mp_int; var b,q: mp_int; k: longint): boolean;
  {-calculate b with b*b = a mod p, p prime = 1 mod 8, return success status}
  { *internal* q and k must be setup with p-1 = 2^k*q}
var
  i,j: longint;
  g,r,x: mp_int;
label
  leave;
begin

  {See S.C. Lindhurst [32], Chap.2.2: Shanks's Algorithm for square roots mod p}

  mp_sqrtmod18_shanks_intern := false;
  mp_init3(g,r,x);
  if mp_error=MP_OKAY then begin
    {find quadratic nonresidue g}
    i := 2;
    while mp_jacobi_lm(i,p)<>-1 do begin
      if mp_error<>MP_OKAY then goto leave;
      inc(i);
      if i=100 then begin
        {test p prime and a quadratic residue before going on}
        if (not mp_is_pprime(p)) or (mp_jacobi(a,p)<>1) then goto leave;
      end;
    end;
    mp_set_int(g,i);
    mp_exptmod(g,q,p,g);   {g = g^q}

    mp_div_2(q,q);         {q = (q-1)/2 = q/2, since q is odd}
    mp_exptmod(a,q,p,x);   {x = a^((q-1)/2}
    mp_mulmod(a,x,p,r);    {r = a*x=a^((q+1)/2}
    mp_mulmod(x,r,p,x);    {x = a^q}

    while (not mp_is1(x)) and (mp_error=MP_OKAY) do begin
      {loop invariant: x*r}
      j := 0;
      {find least positive integer with x^(2^j) = 1}
      mp_copy(x,q);
      repeat
        inc(j);
        {j=k should not happen if p is prime, indicate failure}
        if (j=k) or (mp_error<>MP_OKAY) then goto leave;
        mp_sqrmod(q,p,q);
      until mp_is1(q);

      {t := g^(2^(k-j-1)}
      for i:=2 to k-j do mp_sqrmod(g,p,g);

      {update; r=r*g, g=t^2, x=g*x, k=j}
      mp_mulmod(r,g,p,r);
      mp_sqrmod(g,p,g);
      mp_mulmod(x,g,p,x);
      k := j;
    end;
    mp_exch(r,b);
    mp_sqrtmod18_shanks_intern := mp_error=MP_OKAY;
leave:
    mp_clear3(g,r,x);
  end;
end;


{---------------------------------------------------------------------------}
function mp_sqrtmod18_shanks(const a,p: mp_int; var b: mp_int): boolean;
  {-calculate b with b*b = a mod p, p prime = 1 mod 8, return success status; *internal*}
var
  k: longint;
  q: mp_int;
begin
  mp_sqrtmod18_shanks := false;
  mp_init(q);
  if mp_error=MP_OKAY then begin
    mp_sub_d(p,1,q);
    mp_makeodd(q,q,k);     {p-1 = 2^k*q}
    mp_sqrtmod18_shanks := mp_sqrtmod18_shanks_intern(a,p,b,q,k);
    mp_clear(q);
  end;
end;


{---------------------------------------------------------------------------}
function mp_sqrtmod18_lucas(const a,p: mp_int; var b: mp_int): boolean;
  {-calculate b with b*b = a mod p, p prime = 1 mod 8, return success status}
  { *internal* assumes a,p>1, no arg check}
var
  h,k,vk: mp_int;
  i: longint;
  j: integer;
begin
  {sqrt(a) mod p via Lucas chain,  Crandall/Pomerance: [10], Ex.2.31}
  mp_sqrtmod18_lucas := false;
  mp_init3(h,k,vk);
  if mp_error=MP_OKAY then begin
    {calculate h with jacobi(-4a+h^2,p)=-1}
    mp_mul_d(a,4,k);
    for i:=1 to $3FFF do begin
      mp_set_int(vk,sqr(i));
      mp_sub(vk,k,vk);
      j := mp_jacobi(vk,p);
      if j=-1 then begin
        mp_set_int(h,i);
        break;
      end
      else if i=100 then begin
        {test p prime and a quadratic residue before going on}
        {break loop with j=1 indicating failure}
        if (mp_jacobi(a,p)<>1) or (not mp_is_pprime(p)) then break;
      end;
    end;
    if j=-1 then begin
      {calculate Lucas chain for k=(p+1)/2}
      mp_add_d(p,1,k);
      mp_div_2(k,k);
      mp_lucasvmod(h,a,p,k,vk);
      {b = vk/2 mod p}
      if mp_isodd(vk) then mp_add(vk,p,vk);
      {Lucas failed, may be a is a nonresidue or p is not prime.}
      if not mp_iszero(vk) then begin
        mp_div_2(vk,b);
        mp_sqrtmod18_lucas := mp_error=MP_OKAY;
      end;
    end;
    mp_clear3(h,k,vk);
  end;
end;


{---------------------------------------------------------------------------}
function mp_sqrtmod14_mueller(const a,p: mp_int; var b: mp_int): boolean;
  {-calculate b with b*b = a mod p, p prime = 1 mod 4, return success status}
  { *internal* assumes a>1,p>1, no argument checks}

  { Siguna Mller, On the Computation of Square Roots and Cube Roots Modulo p}
  { http://math.uwyo.edu/RMMC/2006/course%20material/RMMC-mueller-2.pdf}
var
  PP,mu,u,v: mp_int;
  j: longint;
  t: word;
begin
  mp_sqrtmod14_mueller := false;

   (* Theorem on page 11 of RMMC-mueller-2.pdf
   **
   ** Suppose p =1 mod 4 and (Q|p) = 1
   **  - If (Q-4|p) = -1, let P = Q -2. Then V[(p - 1)/4](P, 1) is a
   **    square root of Q.
   **  - If (Q-4|p) = 1, let P = Q*t^2 - 2, where t is such that
   **    (Q*t^2 - 4 | p) = -1.
   **    Then V[(p - 1)/4](P, 1)/t is a square root of Q.
   *)

  {Variable correspondence to Mller:  a => Q, PP => P, t => t}
  mp_init4(PP,mu,u,v);
  if mp_error<>MP_OKAY then exit;

  {Setup Barrett reduction for p, used in s_mp_lucasvmod1}
  mp_reduce_setup(mu,p);

  {No special treatment of case t=1, fits perfectly in for loop}
  for t:=1 to 255 do begin
    {search u = u(t) = a*t^2 - 4 with (u|p) = -1}
    mp_mul_w(a,sqr(t),u);
    mp_sub_d(u,4,u);
    j := mp_jacobi(u,p);
    if (mp_error<>MP_OKAY) or (j=-1) then break;
  end;
  if (mp_error=MP_OKAY) and (j=-1) then begin
    {PP = a*t^2 - 2 = u(t) + 2, force PP into range [0..p-1]}
    mp_add_d(u,2,PP);
    if PP.sign=MP_NEG then mp_add(PP,p,PP)
    else mp_reduce(PP, p, mu);
    {u = (p-1)/4}
    mp_sub_d(p,1,u);
    mp_shr(u,2,u);
    {v = LucasV[u](PP,1) mod p}
    s_mp_lucasvmod1(PP,p,u,mu,v);
    {b = v/t}
    if t=1 then mp_exch(b,v)
    else begin
      mp_set(u,t);
      mp_invmod(u,p,u);
      mp_mulmod(u,v,p,b);
    end;
    mp_sqrtmod14_mueller := mp_error=MP_OKAY;
  end;
  mp_clear4(PP,mu,u,v)
end;


{---------------------------------------------------------------------------}
function mp_sqrtmod916_kong(const a,p: mp_int; var b: mp_int): boolean;
  {-calculate b with b*b = a mod p, p prime = 9 mod 16, return success status}
  { *internal* assumes a,p>1, no arg check}

  { This procedure implements Algorithm 2 from: F. Kong et al.,    }
  { Improved generalized Atkin algorithm for computing square roots}
  { in finite fields, Information Processing Letters 98 (2006) 1-5 }
var
  r,s,t,d: mp_int;
  ld: longint;
const
  JACMAX=1000;
label
  leave;
begin
  mp_sqrtmod916_kong := false;
  mp_init4(r,s,t,d);
  if mp_error<>MP_OKAY then exit;

  {r = (2a)^((p-9)/16)};
  mp_addmod(a,a,p,s);
  mp_sub_d(p,9,t);
  mp_shr(t,4,t);
  mp_exptmod(s,t,p,r);
  {t = 2ar^2}
  mp_sqrmod(r,p,t);
  mp_mulmod(s,t,p,t);
  {s = t^2}
  mp_sqrmod(t,p,s);
  {if s=1 then ...}
  if mp_is1(s) then begin
    {select d with (d|p)=-1}
    ld := 2;
    repeat
      if mp_jacobi_lm(ld,p)=-1 then break;
      inc(ld);
      if ld=JACMAX then goto leave;
    until false;
    mp_set_int(d,ld);
    {t = r*d^((p-9)/8}
    mp_sub_d(p,9,t);
    mp_shr(t,3,t);
    mp_exptmod(d,t,p,t);
    mp_mulmod(t,r,p,t);
    {s = 2*(t*d)^2*a}
    mp_mulmod(t,d,p,s);
    mp_sqrmod(s,p,s);
    mp_mulmod(s,a,p,s);
    mp_addmod(s,s,p,s);
    {b = t*d*a*(s-1)}
    if s.used=0 then mp_sub_d(p,1,s) else mp_dec(s);
    mp_mulmod(s,a,p,s);
    mp_mulmod(s,d,p,s);
    mp_mulmod(s,t,p,b);
  end
  else begin
    {b = r*a*(t-1)}
    if t.used=0 then mp_sub_d(p,1,t) else mp_dec(t);
    mp_mulmod(t,a,p,t);
    mp_mulmod(r,t,p,b);
  end;
  mp_sqrtmod916_kong := true;

leave:
  mp_clear4(r,s,t,d);
end;


{---------------------------------------------------------------------------}
function mp_sqrtmod18(const a,p: mp_int; var b: mp_int): boolean;
  {-internal wrapper function for p=1 mod 8}
var
  k,bs: longint;
  q: mp_int;
begin
  mp_sqrtmod18 := false;
  if mp_sqrtmethod=1 then mp_sqrtmod18 := mp_sqrtmod18_shanks(a,p,b)
  else if mp_sqrtmethod=2 then mp_sqrtmod18 := mp_sqrtmod18_lucas(a,p,b)
  else if mp_sqrtmethod=3 then mp_sqrtmod18 := mp_sqrtmod14_mueller(a,p,b)
  else begin
    {Auto mode}
    {$ifdef MPC_UseKONG}
      if p.pdigits^[0] and 15 = 9 then begin
        mp_sqrtmod18 := mp_sqrtmod916_kong(a,p,b);
        exit;
      end;
    {$endif}
    mp_init(q);
    if mp_error=MP_OKAY then begin
      mp_sub_d(p,1,q);
      mp_makeodd(q,q,k);     {p-1 = 2^k*q}
      bs := mp_bitsize(p);

      {V0.7.08: M.Martin [8] uses Lucas if (bs>128) and (k > bs div 8),}
      {PARI/GP [12] uses Cipolla if k(k-1) > 8*bs + 20, cf. Theorem 3.3}
      {in "Square Roots Modulo p" by Gonzalo Tornar¡a, available from}
      {http://www.cmat.edu.uy/~tornaria/pub/Tornaria-2002.pdf}

      {We use the PARI/Tornar¡a test to select between Shanks and Mueller.}
      {Since Mueller is almost always faster than Lucas/Cipolla this is }
      {quite conservative.}

      if k>2+trunc(sqrt(8.0*bs+20.0)) then mp_sqrtmod18 := mp_sqrtmod14_mueller(a,p,b)
      else mp_sqrtmod18 := mp_sqrtmod18_shanks_intern(a,p,b,q,k);
      mp_clear(q);
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure mp_sqrtmod_ex(const a,p: mp_int; ChkJ: boolean; var b: mp_int; var Err: integer);
  {-calculate square root b of a with b*b = a mod p, p prime.}
  { If ChkJ=true then check Jacobi symbol (a|p)=1, Err=-1 if check fails.}
  { Err=1 if failure for p=1 mod 8, Err=2 if p even and not 2.}
begin
  Err := 0;
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(p) or mp_not_init(b) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_sqrtmod_ex');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if mp_is0(p) then begin
    Err:=2;
    exit;
  end;
  if s_mp_mod_is0(a,p) then begin
    mp_zero(b);
    exit;
  end;
  if mp_isodd(p) then begin
    if mp_is1(p) then begin
      Err := 1;
      exit;
    end;
    if mp_is0(a) or mp_is1(a) then begin
      {if a=0 or 1, set b=a}
      mp_copy(a,b);
      exit;
    end
    else begin
      if (not ChkJ) or (mp_jacobi(a,p)=1) then begin
        {selected algorithm depends on p mod 8}
        case p.pdigits^[0] and 7 of
            1: if not mp_sqrtmod18(a,p,b) then Err := 1;
            5: mp_sqrtmod58(a,p,b);
          else mp_sqrtmod34(a,p,b);  {cases 3,7; p=3 mod 4}
        end;
      end
      else Err:=-1;
    end;
  end
  else begin
    {even p, set b=a mod 2 if p=2, else error}
    if mp_cmp_d(p,2)=MP_EQ then mp_mod(a,p,b)
    else Err:=2;
  end;
end;


{---------------------------------------------------------------------------}
procedure mp_sqrtmod(const a,p: mp_int; var b: mp_int; var Err: integer);
  {-calculate square root b of a with b*b = a mod p, p prime, with Jacobi check.}
  { Err=-1 if (a|p)<>1, Err=1 if failure for p=1 mod 8, Err=2 if p even and <>2}
begin
  mp_sqrtmod_ex(a,p,true,b,Err);
end;


{---------------------------------------------------------------------------}
procedure s_mp_sqrtmod2k(const a: mp_int; k: word; var b: mp_int; var Err: integer);
  {-calculate unique square root b of an odd integer a with b*b = a mod 2^k and }
  { 0<b<2^k/4 for k>3. Err=1 if a<0; =2 for even a; =3 if a<>1 mod min(2^k,8)}
var
  h,t,w: mp_int;
  j,j1,j2: integer;
  am8: mp_digit;
begin
  Err := 1;
  {Err=1 if a is negative}
  if (mp_error<>MP_OKAY) or (a.sign=MP_NEG) then exit;

  {Err=2 for even a}
  if mp_iseven(a) or (k=0) then begin
    Err := 2;
    exit;
  end;

  {Err=3 if a<>1 mod min(2^k,8)}
  Err := 3;
  {am8 = a mod 8}
  am8 := a.pdigits^[0] and 7;
  if k<3 then begin
    if ((k=1) and odd(am8)) or ((k=2) and (am8 and 3 = 1)) then begin
      mp_set1(b);
      Err := 0;
    end;
    exit;
  end;
  if am8<>1 then exit;

  {Algorithm is from P1363/D8 [25], A.2.6 Finding Square Roots Modulo}
  {a power of 2. See also Marcel Martin's ISqrtMod2K in [8]}

  Err := 0;
  mp_init3(w,h,t);
  mp_set1(w);
  mp_set1(h);
  for j:=2 to k-2 do begin
    j1 := j+1;
    if mp_isbit(h,j1) <> mp_isbit(a,j1) then begin
      mp_setbit(w,j);
      {h := (h + w * 2^(j+1)) mod 2^k}
      mp_shl(w,j1,t);
      mp_add(t,h,t);
      s_mp_mod_2k(t,k,h);
      j2 := j+j;
      if j2<k then begin
        {h := h - 2^j2}
        if mp_isbit(h,j2) then begin
          {easy case, just clear the bit}
          mp_clrbit(h,j2);
        end
        else begin
          {subtract 2^j2 and correct if h - 2^j2 < 0}
          mp_2expt(t,j2);
          mp_sub(h,t,h);
          if h.sign=MP_NEG then begin
            mp_setbit(t,k);
            mp_clrbit(t,j2);
            mp_add(h,t,h);
          end;
        end;
      end;
    end;
  end;
  {make sure 0 < w < 2^k/4}
  if mp_isbit(w,k-2) then begin
    mp_2expt(t,k-1);
    mp_sub(t,w,b);
  end
  else mp_exch(w,b);
  mp_clear3(w,h,t);
end;


{---------------------------------------------------------------------------}
procedure mp_sqrtmod2k(const a: mp_int; k: word; var b: mp_int; var Err: integer);
  {-calculate a square root b of an integer a with b*b = a mod 2^k.}
  { Return Err=1 if a<0;  Err=-1 if there is no solution. For odd a}
  { the solutions are normalized to 0 < b < 2^k/4 for k>3.}
var
  t: mp_int;
  s,h: longint;
  d: mp_digit;
begin
  Err := 0;
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(b) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_sqrtmod2k');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {easy outs}
  if mp_is0(a) then begin
    mp_zero(b);
    exit;
  end
  else if a.sign=MP_NEG then begin
    Err := 1;
    exit;
  end;

  {Handle odd a and map error (although all error conditions}
  {should have been handled above)}
  if mp_isodd(a) then begin
    s_mp_sqrtmod2k(a,k,b,Err);
    if Err<>0 then Err := -1;
    exit;
  end;

  {If a is even the solutions are from Adler & Coury[26], 4-39. If}
  {solvable, the (one) solution with j=0 is calculated, see below.}

  Err := -1;

  {Decompose a=2^s*t, a>0, s>0}
  s := mp_cnt_lsb(a);

  {if s is odd there are no solutions ([26] 4-38)}
  if odd(s) then exit;

  mp_init(t);
  if mp_error<>MP_OKAY then exit;

  mp_shr(a, s, t);
  h := s div 2;
  d := t.pdigits^[0];

  if k=s+1 then begin
    {[26] 4-39,iii: there are 2^h solutions: 1 + j*2^(h+1), j=0..2^h-1.}
    {This is wrong (e.g. for sqrt(16) mod 32), but the actual calculation}
    {in [26] shows that there are 2^h solutions 2^h*(1 + 2*j), j=0..2^h-1.}
    Err := 0;
    mp_2expt(b, h);
  end
  else if k=s+2 then begin
    {[26] 4-39,ii: solvable iff t=1 mod 4; there are 2^(h+1) solutions:}
    {+-2^h + j*2^(h+2), j=0..2^h-1}
    if d and 3 = 1 then begin
      Err := 0;
      mp_2expt(b, h);
    end;
  end
  else if k>s+2 then begin
    {[26] 4-39,i: solvable iff t=1 mod 8; there are 2^(h+2) solutions:}
    {2^h*y + j*2^(k-h), j=0..2^h-1, y one of the 4 solutions of x^2=t mod 2^(k-s)}
    if d and 7 = 1 then begin
      s_mp_sqrtmod2k(t,k-s,b,Err);
      if Err=0 then mp_shl(b,h,b)
      else Err := -1;
    end;
  end;
  mp_clear(t);
end;


{---------------------------------------------------------------------------}
procedure mp_sqrtmodp2(const a,p: mp_int; var b: mp_int; var Err: integer);
  {-calculate square root b of a with b*b = a mod p^2, p prime.}
  { Alias for mp_sqrtmodpk(,,2,,). Err: error code from mp_sqrtmodpk.}
begin
  mp_sqrtmodpk(a,p,2,b,Err);
end;


{---------------------------------------------------------------------------}
procedure s_mp_sqrtmodpk(const a,p: mp_int; k: word; red: boolean; var b: mp_int; var Err: integer);
  {-calculate square root b of a with b*b = a mod p^k, p odd prime.}
  { Err=-1 if (a|p)<>1, Err=1 if failure for p=1 mod 8, Err=2 if p even.}
  { Err=4 if no inverse mod p^(2^i). If red=true, reduce b mod p^k, otherwise}
  { b may be >= p^k; ex: a=22,p=3,k=3 --> b=34 and 34^2 = 22 mod 27}
var
  p2i,ri,x,z: mp_int;
  i: integer;
begin
  Err := 0;
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(p) or mp_not_init(b) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('s_mp_sqrtmodpk');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if mp_iszero(a) or mp_is1(a) or (k=0) then begin
    {if a=0 or 1, set b=a}
    mp_copy(a,b);
    exit;
  end;
  if mp_isodd(p) then begin
    mp_init4(p2i,ri,x,z);
    if mp_error<>MP_OKAY then exit;
    {[10] Alg 2.3.11 (Hensel lifting) with f(x)=a-x^2, f'(x)=-2x}
    {Calls function newr repeatedly until 2^i >= k}
    {r0 = sqrt(a) mod p}
    mp_sqrtmod(a,p,ri,Err);
    if (Err=0) and (k>1) then begin
      if mp_is0(ri) then begin
        {zero solution but a<>0: try to divide out p^2}
        mp_sqr(p,p2i);
        mp_divrem(a,p2i,@x,@z);
        if mp_is0(z) then begin
          s_mp_sqrtmodpk(x,p,k-2,true,ri,Err);
          if Err=0 then mp_mul(ri,p,ri);
        end
        else Err:=4;
      end
      else begin
        for i:=0 to bitsize32(k)-1 do begin
          {calculate p^(2^i): copy from p or square p^(2^(i-1))}
          if i=0 then mp_copy(p,p2i) else mp_sqr(p2i,p2i);
          {z = f'(ri)^-1 mod p^(2^i) = -(2ri)^-1 mod p^(2^i)}
          mp_mul_2(ri,z);
          if not mp_invmodf(z,p2i,z) then begin
            Err := 4;
            break;
          end;
          {x = f(ri)/p^(2^i) = (a-ri^2)/p^(2^i)}
          mp_sqr(ri,x);
          mp_sub(a,x,x);
          mp_div(x,p2i,x);
          {x = -xz mod p^(2^i)) = x/(2ri) mod p^(2^i))}
          mp_mulmod(z,x,p2i,x);
          {r(i+1) = ri + x*p^(2^i)}
          mp_mul(x,p2i,x);
          mp_add(x,ri,ri);
        end;
      end;
    end;
    if Err=0 then begin
      {store last ri as result into b, reduce if requested}
      if red then begin
        mp_expt_d(p,k,x);
        mp_mod(ri,x,b);
      end
      else mp_exch(b,ri);
    end;
    mp_clear4(p2i,ri,x,z);
  end
  else Err:=2;
end;


{---------------------------------------------------------------------------}
procedure mp_sqrtmodpk(const a,p: mp_int; k: word; var b: mp_int; var Err: integer);
  {-calculate square root b < p^k of a with b*b = a mod p^k, p prime.  }
  { Error codes: if p=2: Err=1 if a<0;  Err=-1 if there is no solution }
  { if p<>2: Err=-1 if (a|p)<>1, Err=1: failure for p=1 mod 8, Err=2 if}
  {          p is even, Err=4: no inverse mod p^(2^i)}
begin
  if mp_cmp_d(p,2)=0 then mp_sqrtmod2k(a,k,b,Err)
  else s_mp_sqrtmodpk(a,p,k,true,b,Err);
end;


(*************************************************************************
The 'quadratic' Hensel lifting used in the standard version of mp_sqrtmodpk
is normally (much) faster than the non-quadratic version listed below, but
it has larger numbers p^(2^i) > p^k if k is not a power of 2.

{---------------------------------------------------------------------------}
procedure mp_sqrtmodpk(const a,p: mp_int; k: integer; var b: mp_int; var Err: integer);
  {-calculate square root b of a with b*b = a mod p^k, p odd prime.}
  { Err=1 if failure for p=1 mod 8, Err=2 if p even.}
var
  pj,r,x,z: mp_int;
  j: integer;
begin
  err := 0;
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(p) or mp_not_init(b) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_sqrtmodpk');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}
  if mp_isodd(p) then begin
    if mp_iszero(a) or mp_is1(a) then begin
      {if a=0 or 1, set b=a}
      mp_copy(a,b);
      exit;
    end
    else begin
      mp_init4(pj,r,x,z);
      if mp_error<>MP_OKAY then exit;
      {Non-quadratic Hensel lifting for f(x)=a-x^2, f'(x)=2x}
      {Example in http://en.wikipedia.org/wiki/Hensel's_lemma}
      mp_sqrtmod(a,p,r,Err);       {r = sqrt(a)}
      if (Err=0) and (k>1) then begin
        mp_mul_2(r,z);
        mp_invmod(z,p,z);          {z = f'(r)^-1 mod p = -(2r)^-1 mod p}
        for j:=1 to k-1 do begin
          if j=1 then mp_copy(p,pj)
          else mp_mul(p,pj,pj);
          mp_sqr(r,x);
          mp_sub(a,x,x);
          mp_div(x,pj,@x,nil);     {x = f(rj)*p^(-j) = (a-rj^2)/p^j)}
          mp_mulmod(z,x,pj,x);     {x = -xz mod p^j) = x/(2r) mod p^j)}
          mp_mul(x,pj,x);
          mp_add(x,r,r);           {r = r + x*p^j}
        end;
      end;
      mp_exch(b,r);
      mp_clear4(pj,r,x,z);
    end;
  end
  else begin
    Err:=2;
  end;
end;
**************************************************************************)


{---------------------------------------------------------------------------}
procedure mp_sqrtmodpq(const a,p,q: mp_int; var x,y: mp_int; var Err: integer);
  {-calculate square roots +x,-x,+y,-y of a mod (pq); p,q primes}
  { If p=q, x is the root from mp_sqrtmodp2 and y is not computed.}
  { For p<>q: Err=4 if gcd(p,q)<>1, otherwise error code from mp_sqrtmod}
var
  c,d,n,r,s: mp_int;
begin
  Err := 0;
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(p) or mp_not_init(q) or mp_not_init(x) or mp_not_init(y) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_sqrtmodpq');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {Handle case p=q: only one solution}
  if mp_is_eq(p,q) then begin
    mp_sqrtmodp2(a,p,x,Err);
    exit;
  end;

  mp_init5(c,d,n,r,s);
  {Ref: Alg. 3.44 from HAC [5], slightly rearranged.}
  {First compute extended gcd, c*p + d*q = gcd(p,q) = n. Error if n<>1}
  {Then compute r = sqrt(a) mod p, s = sqrt(a) mod q}
  {Results x = (rdq+scp) mod pq, y = (rdq-scp) mod pq}
  mp_xgcd(p,q,@c,@d,@n);
  if not mp_is1(n) then Err:=4
  else begin
    {calculate roots mod p and mod q, do nothing if error}
    mp_sqrtmod(a,p,r,Err);
    if (Err=0) and (mp_error=MP_OKAY) then begin
      mp_sqrtmod(a,q,s,Err);
      if (Err=0) and (mp_error=MP_OKAY) then begin
        mp_mul(p,q,n);
        mp_mulmod(d,q,n,d);
        mp_mulmod(c,p,n,c);
        mp_mulmod(r,d,n,r);
        mp_mulmod(s,c,n,s);
        {combine results to get roots x,y. The other roots are -x, -y}
        mp_addmod(r,s,n,x);
        mp_submod(r,s,n,y);
      end;
    end;
  end;
  mp_clear5(c,d,n,r,s);
end;


{---------------------------------------------------------------------------}
{---------------------  End of mp_sqrtmod routines  ------------------------}
{---------------------------------------------------------------------------}


{---------------------------------------------------------------------------}
procedure mp_submod(const a,b,c: mp_int; var d: mp_int);
  {-calculate d = a - b (mod c)}
var
  t: mp_int;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(b) or mp_not_init(c) or mp_not_init(d) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_submod');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  mp_init(t);
  if mp_error<>MP_OKAY then exit;

  mp_sub(a, b, t);
  mp_mod(t, c, d);
  mp_clear(t);
end;


{---------------------------------------------------------------------------}
procedure mp_williams_pp1(const N: mp_int; var f: mp_int; bound,numtest: word);
  {-find a factor f of N with William's p+1 method, f=0 if no success. numtest}
  { random seeds will be tried, should be about 3 or 4, if 0 then 3 is used}
  { primes <= bound will be included in pp_expo product}
const
  anz0=128;
label
  leave;
var
  B0,B1: word;
  a,r,x: mp_int;
  cancel: boolean;
begin
  if mp_error<>MP_OKAY then exit;
  {Ref: Forster[4], 18, "Die (p+1)-Faktorisierungs-Methode"}

  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(N) or mp_not_init(f)  then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_williams_pp1');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  mp_init3(r,a,x); if mp_error<>MP_OKAY then exit;

  {Setup Barrett reduction}
  mp_reduce_setup(r, N);

  if numtest=0 then numtest := 3;
  cancel := false;

  while (numtest>0) and (not cancel) do begin
    dec(numtest);

    {a random >= 2}
    repeat
      mp_rand(a, N.used);
      mp_mod(a,N,a);
      if mp_error<>MP_OKAY then goto leave;
    until mp_cmp_d(a,1)=MP_GT;

    {x = a*a-1}
    mp_sqr(a,x);
    mp_dec(x);
    mp_gcd(x,N,f);

    {found non trivial factor, goto leave with res=MP_OKAY}
    if (mp_error<>MP_OKAY) or (mp_cmp_d(f,1)=MP_GT) then goto leave;
    if bound>$7FF0 then bound:=$7FF0;

    B0 := 0;
    while B0<bound do begin
      B1 := B0+anz0;
      if B1>bound then B1:=bound;
      mp_ppexpo(x, B0, B1);
      s_mp_coshmult(a,x,N,r,a);
      if mp_error<>MP_OKAY then goto leave;
      if mp_is1(a) then break;
      if mp_show_progress then begin
        if ProgressAssigned then begin
          cancel := false;
          mp_progress(false, B0, 0, cancel);
          if cancel then break;
        end
        else write('.');
      end;
      mp_sub_d(a,1,x);
      mp_gcd(x,n,f);
      {error or non trivial factor?}
      if (mp_error<>MP_OKAY) or (mp_cmp_d(f,1)=MP_GT) then goto leave;
      inc(B0, anz0);
    end;
    mp_zero(f);
  end;

leave:
   mp_clear3(x,a,r);
end;


{---------------------------------------------------------------------------}
procedure mp_xgcd(const a,b: mp_int; p1,p2,p3: pmp_int);
  {-extended gcd algorithm, calculate  a*p1^ + b*p2^ = p3^ = gcd(a,b)}
  { p1,p2,p3 may be nil if the values are not required.}
var
  u1,u3,v1,v3,t1,t3,q: mp_int;
  x0,x1,y0,y1: tml_int;
  TryLehmer: boolean;
const
  k=Lehmer_MaxK; k2=(k+1) div 2;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(b) or ((p1<>nil) and mp_not_init(p1^)) or
       ((p2<>nil) and mp_not_init(p2^)) or ((p3<>nil) and mp_not_init(p3^))
    then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_xgcd');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
    if ((p1=p2) and (p1<>nil)) or ((p1=p3) and (p1<>nil)) or ((p2=p3) and (p2<>nil)) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXBadArg.Create('mp_xgcd: identical pointers');
        {$else}
          RunError(MP_RTE_BADARG);
        {$endif}
      {$else}
        set_mp_error(MP_BADARG);
        exit;
      {$endif}
    end;
  {$endif}

  if (p1=nil) and (p2=nil) then begin
    {easy out for trivial cases}
    if p3<>nil then mp_gcd(a,b,p3^);
    exit;
  end;

  mp_init7(u1,u3,v1,v3,t1,t3,q);
  if mp_error<>MP_OKAY then exit;

  {Note: the following setup returns 1*0 + 0*0 = 0, for a=b=0! This}
  {is somewhat strange but OK. Knuth's algorithm X gives the same. }
  {Usage of u2,v2,t2 suppressed, p2^ = u2 will be = (u3 - a*u1)/b. }
  {We need positive working variables u3,v3. So init via abs, the  }
  {sign of u3 is corrected after loop and before calculation of u2.}

  {initialize, (u1,u3) = (1,a)}
  mp_set1(u1);
  mp_abs(a, u3);

  {initialize, (v1,v3) = (0,b), v1=0 after init}
  mp_abs(b, v3);

  {The Lehmer step needs u3 >= v3 > 0. This condition may no hold}
  {initially. For extended GCD we cannot simply swap u3 and v3.  }
  {Therefore we skip the step in the first iteration, after that }
  {the condition becomes true via the Euclidean mod operation.   }
  TryLehmer := mp_cmp_mag(u3,v3)<>MP_LT;

  {loop while v3 != 0}
  while (mp_error=MP_OKAY) and (not mp_iszero(v3)) do begin
    if TryLehmer and (mp_bitsize(u3)-mp_bitsize(v3) <= k2) then begin
      if Lehmer(u3,v3,k,x0,x1,y0,y1) then begin
        {calculate new (ui, vi):  vi := x1*ui + y1*vi;  ui := x0*ui + y0*vi;}
        {$ifdef MP_16BIT}
          {v3 := x1*u3 + y1*v3}
          mp_mul_w(u3,abs(x1), q);  if x1<0 then s_mp_chs(q);
          mp_mul_w(v3,abs(y1),t1);  if y1<0 then s_mp_chs(t1);
          mp_add(q,t1,t1);
          {u3 := x0*u3 + y0*v3}
          mp_mul_w(u3,abs(x0), q);  if x0<0 then s_mp_chs(q);
          mp_mul_w(v3,abs(y0),t3);  if y0<0 then s_mp_chs(t3);
          mp_add(q,t3,t3);
          {the new u3,v3 should be >= 0, skip the Lehmer step if not}
          if (t1.sign=MP_ZPOS) and (t3.sign=MP_ZPOS) then begin
            mp_exch(t3,u3);
            mp_exch(t1,v3);
            {v1 := x1*u1 + y1*v1}
            mp_mul_w(u1,abs(x1), q);  if x1<0 then s_mp_chs(q);
            mp_mul_w(v1,abs(y1),t1);  if y1<0 then s_mp_chs(t1);
            mp_add(q,t1,t1);
            mp_exch(t1,v1);
            {u1 := x0*u1 + y0*v1}
            mp_mul_w(u1,abs(x0), q);  if x0<0 then s_mp_chs(q);
            mp_mul_w(t1,abs(y0),t1);  if y0<0 then s_mp_chs(t1);
            mp_add(q,t1,u1);
          end;
        {$else}
          {v3 := x1*u3 + y1*v3}
          mp_mul_d(u3,abs(x1), q);  if x1<0 then s_mp_chs(q);
          mp_mul_d(v3,abs(y1),t1);  if y1<0 then s_mp_chs(t1);
          mp_add(q,t1,t1);
          {u3 := x0*u3 + y0*v3}
          mp_mul_d(u3,abs(x0), q);  if x0<0 then s_mp_chs(q);
          mp_mul_d(v3,abs(y0),t3);  if y0<0 then s_mp_chs(t3);
          mp_add(q,t3,t3);
          {the new u3,v3 should be >= 0, skip the Lehmer step if not}
          if (t1.sign=MP_ZPOS) and (t3.sign=MP_ZPOS) then begin
            mp_exch(t3,u3);
            mp_exch(t1,v3);
            {v1 := x1*u1 + y1*v1}
            mp_mul_d(u1,abs(x1), q);  if x1<0 then s_mp_chs(q);
            mp_mul_d(v1,abs(y1),t1);  if y1<0 then s_mp_chs(t1);
            mp_add(q,t1,t1);
            mp_exch(t1,v1);
            {u1 := x0*u1 + y0*v1}
            mp_mul_d(u1,abs(x0), q);  if x0<0 then s_mp_chs(q);
            mp_mul_d(t1,abs(y0),t1);  if y0<0 then s_mp_chs(t1);
            mp_add(q,t1,u1);
          end;
        {$endif}
      end;
    end;

    {q = u3/v3, t3=u3-q*v3}
    mp_divrem(u3, v3, @q, @t3);

    {(t1,t3) = (u1,u3) - q*(v1,v3)}
    mp_mul(v1, q,  t1);
    mp_sub(u1, t1, t1);

    {(u1,u3) = (v1,v3)}
    mp_exch(v1, u1);
    mp_exch(v3, u3);

    {(v1,v3) = (t1,t3)}
    mp_exch(t1, v1);
    mp_exch(t3, v3);

    TryLehmer := true;
  end;

  if mp_error=MP_OKAY then begin
    {adjust sign if a<0}
    if a.sign=MP_NEG then s_mp_chs(u1);
    {copy results if requested}
    if p2<>nil then begin
      {here v3=0}
      if mp_iszero(b) then mp_exch(p2^, v3)
      else begin
        {use v3 to calculate p2^ = (u3 - a*u1)/b}
        mp_mul(a,u1,v1);
        mp_sub(u3,v1,v3);
        mp_div(v3,b,p2^);
      end;
    end;
    if p1<>nil then mp_exch(p1^, u1);
    if p3<>nil then mp_exch(p3^, u3);
  end;
  mp_clear7(u1,u3,v1,v3,t1,t3,q);
end;


{---------------------------------------------------------------------------}
procedure mp_xgcd_bin(const a,b: mp_int; p1,p2,p3: pmp_int);
  {-extended binary gcd, calculate a*p1^ + b*p2^ = p3^ = gcd(a,b)  }
  { p1,p2,p3 may be nil if the values are not needed. Note that p1^}
  { and p2^ are NOT unique and may differ from those of mp_xgcd!!  }
var
  x,y,u,v,c,d: mp_int;
  g: longint;
  pt: pmp_int;
  swapped: boolean;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(b) or ((p1<>nil) and mp_not_init(p1^)) or
       ((p2<>nil) and mp_not_init(p2^)) or ((p3<>nil) and mp_not_init(p3^))
    then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_xgcd_bin');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
    if ((p1=p2) and (p1<>nil)) or ((p1=p3) and (p1<>nil)) or ((p2=p3) and (p2<>nil)) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXBadArg.Create('mp_xgcd_bin: identical pointers');
        {$else}
          RunError(MP_RTE_BADARG);
        {$endif}
      {$else}
        set_mp_error(MP_BADARG);
        exit;
      {$endif}
    end;
  {$endif}

  {Handle trivial cases}
  if mp_iszero(a) then begin
    {0*a + sign(b)*b = |b|}
    if p3<>nil then mp_abs(b,p3^);
    if p1<>nil then mp_zero(p1^);
    if p2<>nil then mp_set_short(p2^,mp_sign(b));
    exit;
  end;
  if mp_iszero(b) then begin
    {sign(a)*a + 0*b = |a|}
    if p3<>nil then mp_abs(a,p3^);
    if p2<>nil then mp_zero(p2^);
    if p1<>nil then mp_set_short(p1^,mp_sign(a));
    exit;
  end;
  if (p1=nil) and (p2=nil) then begin
    if p3<>nil then mp_gcd(a,b,p3^);
    exit;
  end;

  {Here a<>0, b<>0}
  mp_init6(x,y,u,v,c,d);

  mp_copy(a,x);
  mp_copy(b,y);
  g := 0;

  {Note mp_iseven() returns false if mp_error<>MP_OKAY}

  {Get common power of 2}
  while mp_iseven(x) and mp_iseven(y) do begin
    mp_shr1(x);
    mp_shr1(y);
    inc(g);
  end;

  {Core routine needs odd x, use symmetry of x*p1^ + y*p2^ = gcd(x,y)}
  {and swap x,y and the pointers to the corresponding factors}
  if mp_iseven(x) then begin
    mp_exch(x,y);
    pt := p1;
    p1 := p2;
    p2 := pt;
    swapped := true;
  end
  else swapped := false;

  {Here x odd, perform binary extended core routine v=gcd(x,y).}
  {This is essentially the same routine as in the old}
  {fast_mp_invmod from LTM / HAC 14.61, pp608}

  mp_abs(y,v);
  mp_abs(x,u);
  mp_set1(d);

  repeat
    {u>0, v>0}
    while mp_iseven(u) do begin
      mp_shr1(u);
      if mp_isodd(c) then mp_sub(c, x, c);
      mp_shr1(c);
    end;
    while mp_iseven(v) do begin
      mp_shr1(v);
      if mp_isodd(d) then mp_sub(d, x, d);
      mp_shr1(d);
    end;
    if (u.used>v.used) or (mp_cmp(u, v)<>MP_LT) then begin
      {u >= v, v unchanged > 0}
      mp_sub(u, v, u);
      mp_sub(c, d, c);
    end
    else begin
      {u<v, new v remains > 0}
      mp_sub(v, u, v);
      mp_sub(d, c, d);
    end;
  until (mp_error<>MP_OKAY) or mp_iszero(u);

  {Multiply by common power of 2}
  mp_shl(v,g,v);

  {Adjust coefficient of y}
  if y.sign=MP_NEG then s_mp_chs(d);

  {Here (??)*p1^ + d*p2^ = v = gcd(a,b)}
  {Calculate p1^ if requested}
  if p1<>nil then begin
    if swapped then begin
      if mp_iszero(b) then mp_zero(p1^)
      else begin
        {p1^ = (v - d*a)/b}
        mp_mul(d, a, y);
        mp_sub(v, y, y);
        mp_div(y, b, p1^);
      end;
    end
    else begin
      if mp_iszero(a) then mp_zero(p1^)
      else begin
        {p1^ = (v - d*b)/a}
        mp_mul(d, b, y);
        mp_sub(v, y, y);
        mp_div(y, a, p1^);
      end;
    end;
  end;

  {Store p2^ and p3^ if requested}
  if p2<>nil then mp_exch(p2^, d);
  if p3<>nil then mp_exch(p3^, v);

  mp_clear6(x,y,u,v,c,d);
end;


{---------------------------------------------------------------------------}
procedure mp_xlcm(const a,b: mp_int; var c,x,y: mp_int);
  {-calculate c,x,y with lcm(a,b)=c=x*y and x|a, y|b, gcd(x,y)=1.}
  { c,x,y should be different variables, otherwise result may be inconsistent}
var
  g,t: mp_int;
begin
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(a) or mp_not_init(b) or mp_not_init(c) or mp_not_init(x) or mp_not_init(y) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('mp_xlcm');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  if mp_is0(a) or mp_is0(b) then begin
    {if a=0 or b=0 return (0,|a|,|b|)}
    {The following code looks unnecessary complicated but is}
    {needed if the variables 'overlap' @a=@y, @b=@x etc}
    if mp_is0(a) then begin
      mp_abs(b,y);
      mp_zero(x);
    end
    else begin
      mp_abs(a,x);
      mp_zero(y);
    end;
    mp_zero(c);
    exit;
  end;

  mp_init2(g,t);
  if mp_error<>MP_OKAY then exit;

  {t = |a|}
  mp_abs(a,t);
  {first get gcd(a,b)}
  if mp_gcd1(a,b,g) then begin
    {gcd(a,b)=1:  x=|a|, y=|b|, c=x*y}
    mp_abs(b,g);
    mp_mul(t,g,c);
    mp_exch(t,x);
    mp_exch(g,y);
  end
  else begin
    {g = |b|/gcd(a,b)}
    mp_div(b,g,g); g.sign := MP_ZPOS;
    {c = lcm(a,b) = t*g}
    mp_mul(t,g,c);
    {now calculate x alias t}
    while (mp_error=MP_OKAY) and not mp_gcd1(t,g,g) do mp_div(t,g,t);
    {get y=lcm(a,b)/x}
    mp_div(c,t,y);
    mp_exch(t,x);
  end;
  mp_clear2(g,t);
end;


{---------------------------------------------------------------------------}
procedure s_mp_mca_alg1816(const n,L: mp_int; k:integer; var p: mp_int; var fail: boolean);
  {-Try to find a factor p of a squarefree odd integer n>5, with L a multiple}
  { of lambda(n) [lambda: Carmichael function] and a confidence parameter k.}
  { Fail=true, if no success (e.g. n is prime), n is even, n<=5, or L is odd.}
label
  leave;
var
  x,y: mp_int;
  h: integer;
  i,t: longint;
begin
  {Uses Algorithm 18.16 (Special integer factorization) from the}
  {solution to exercise 18.12 (ii) of MCA [30], available online}
  {as http://www-math.uni-paderborn.de/mca/solutions.ps.gz}

  fail := true;
  if mp_error<>MP_OKAY then exit;
  {$ifdef MPC_ArgCheck}
    if mp_not_init(n) or mp_not_init(L) or mp_not_init(p) then begin
      {$ifdef MPC_HaltOnArgCheck}
        {$ifdef MPC_UseExceptions}
          raise MPXNotInit.Create('s_mp_mca_alg1816');
        {$else}
          RunError(MP_RTE_NOTINIT);
        {$endif}
      {$else}
        set_mp_error(MP_NOTINIT);
        exit;
      {$endif}
    end;
  {$endif}

  {Easy outs}
  if mp_isodd(L) or mp_iseven(n) or (mp_cmp_d(n,5)<0) then exit;

  mp_init2(x,y);

  {The variables x,y are used for varying entities from Alg. 18.16; x will}
  {be a non-trivial factor found by gcd(n,.) if fail=false}

  for h:=1 to k do begin
    if mp_error<>MP_OKAY then goto leave;
    {choose 1 < a < n-1 uniformly at random}
    mp_sub_d(n,3,x);   {OK, because n>6}
    mp_rand(y,n.used);
    mp_mod(y,x,y);     {0 <= y < n-3}
    mp_add_d(y,2,y);   {2 <= y < n-1}

    {check if a factor is found by accident}
    if not mp_gcd1(y,n,x) then begin
      {x<>1. Since y < n, x will be a proper factor}
      fail := false;
      goto leave;
    end;

    {write L = 2^t*m with m odd and t>0}
    mp_makeodd(L,x,t);

    {calculate b0 = a^m mod n}
    mp_exptmod(y, x, n, y);

    {if b0=1, try another random a}
    if mp_is1(y) then continue;

    {here y is the last b_i with b_i <> 1}
    for i:=1 to t do begin
      {calculate x=b_i}
      mp_sqrmod(y,n,x);
      if mp_is1(x) then begin
        {x=b_i=1, i.e. found maximal j=i-1 with b_j<>1, use it for trial gcd}
        mp_inc(y);
        {exclude trivial case b_j+1=n}
        if mp_is_ne(n,y) then begin
          if not mp_gcd1(y,n,x) then begin
            fail := false;
            goto leave;
          end;
        end;
        {skip useless squarings because all other b_i will be = 1}
        break; {for i loop}
      end;
      {update b_i with b_i <> 1}
      mp_exch(x,y);
      if mp_error<>MP_OKAY then goto leave;
    end;
  end;

leave:

  if (not fail) and (mp_error=MP_OKAY) then begin
    {x is a proper factor}
    mp_exch(x,p);
  end;

  mp_clear2(x,y);
end;


(*
{---------------------------------------------------------------------------}
{---------------------------------------------------------------------------}
*)


begin

  {MaxFact depends on DIGIT_BIT and MAXDigits. It is an approximate value}
  {for the maximum valid mp_fact argument. Since no checks are performed }
  {some larger values might work. In 16 bit real mode even smaller values}
  {may result in "Out of memory" errors.}

  {$ifdef MP_32BIT}
    MaxFact := 1160*word(DIGIT_BIT);
  {$else}
    MaxFact := 2340*word(DIGIT_BIT);
  {$endif}

  mp_sqrtmethod      := 0;      {0 = Auto, 1: Shanks, 2: Lucas 3: Mueller}
  mp_show_progress   := false;  {rho, (p+1), (p-1) after one accum cycle}
  mp_progress        := nil;
  mp_max_small_sqr   := sqr(longint(MP_DIGIT_MAX and $7FFF));
  mp_initial_mod     := true;

  {Cutoff point for recursive product in mp_primorial}
  {set rough value. For better results use t_tunepr!}

  {$ifdef BIT16}
    mp_primor_cutoff := 400{1000};
  {$else}
    {$ifdef FPC}
       mp_primor_cutoff := 400{1000};
    {$else}
      {$ifdef MP_32BIT}
        mp_primor_cutoff := 1000{2500};
      {$else}
        mp_primor_cutoff := 6000{16000};
      {$endif}
    {$endif}
  {$endif}

end.
