unit mp_prime;

{Basic 16/32 bit prime number support for MPArith}

interface

{$i STD.INC}

{$ifdef VER70}
{$A+}
{$endif}

uses
  mp_types;

{$i mp_conf.inc}

(*************************************************************************

 DESCRIPTION   :  Basic 16/32 bit prime number support for MPArith

 REQUIREMENTS  :  BP7, D1-D7/D9-D10/D12, FPC, VP

 EXTERNAL DATA :  mp_prm16.inc, mp_pbits.inc

 MEMORY USAGE  :  4 KB pbits, 13KB Primes16
                  heap for prime sieve routines

 DISPLAY MODE  :  ---

 REFERENCES    :  [3] Knuth, D.E.: The Art of computer programming. Vol 2
                      Seminumerical Algorithms, 3rd ed., 1998
                  [4] Forster, O.: Algorithmische Zahlentheorie, 1996
                  [5] (HAC) Menezes,A., von Oorschot,P., Vanstone, S: Handbook of
                      Applied Cryptography, 1996, www.cacr.math.uwaterloo.ca/hac
                  [7] P. Ribenboim: The New Book of Prime Number Records, 3rd ed., 1995.
                  [8] Marcel Martin: NX - Numerics library of multiprecision
                      numbers for Delphi and Free Pascal, 2006-2009
                      www.ellipsa.eu/public/nx/index.html
                 [10] Crandall,R., C.Pomerance: Prime Numbers, A Computational
                      Perspective, 2nd ed., 2005
                 [24] H. Cohen, A Course in Computational Algebraic Number Theory
                      4th printing, 2000


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 1.9.00   26.12.08  W.Ehrhardt  Initial version: IsPrime16/32,is_spsp32,is_spsp32A from mp_base
 1.9.01   29.12.08  we          prime_sieve routines
 1.9.02   30.12.08  we          prime_sieve with mp_getmem, prime_sieve_reset
 1.9.03   04.01.09  we          Primes16 array as global data, Primes16Index
 1.9.04   04.01.09  we          32 bit first/next prime routines from mp_numth
 1.9.04   04.01.09  we          32 bit first/next prime routines from mp_numth

 1.12.00  05.07.09  we          Removed prime residue classes mod 30

**************************************************************************)

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
  NumPrimes16 = 6542;                      {Number of 16 bit primes}

const
  Primes16: array[1..NumPrimes16] of word  {array of all 16 bit primes}
               = {#Z+}({$i mp_prm16.inc}){#Z-};

const
  pbits16: array[0..4095] of byte = {#Z+}({$i mp_pbits.inc}){#Z-};
           {Bit array for 16 bit primes. Only odd integers are represented}
  pmask16: array[0..15] of byte = (0,1,0,2,0,4,0,8,0,16,0,32,0,64,0,128);
           {Mask array: odd i is prime if pbits16[i shr 4] and pmask16[i and $f] <> 0)}


function  IsPrime16(N: word): boolean;
  {-Test if N is prime}

function  Primes16Index(n: word): integer;
  {-Get index of largest prime <= n in Primes16 array; 1 if n<=2}

function  IsPrime32(N: longint): boolean;
  {-Test if longint (DWORD) N is prime}

function  is_spsp32(N, A: longint): boolean;
  {-Strong pseudo prime test for N with base A, calls is_spsp32A}

function  is_spsp32A(N: longint; const bases: array of longint): boolean;
  {-Strong pseudo prime test for N with a number of bases.}
  { Return true if N is a spsp for all bases, or if N=2.  }
  { Negative numbers are treated as unsigned (=cardinal)  }



type
  TPrimeContext = record             {Context for FindFirst/NextPrime32}
                    prime: longint;  {last found prime}
                    next : longint;  {next value to test}
                    index: integer;  {index in residue class table}
                  end;

procedure FindFirstPrime32(n: longint; var ctx: TPrimeContext);
  {-Find first prime >= n and initialize ctx for FindNextPrime32}

procedure FindNextPrime32(var ctx: TPrimeContext);
  {-Find next 32 bit prime (DWORD interpretation, see note), success if ctx.prime<>0}

function  nextprime32(n: longint): longint;
  {-next 32 bit prime >= n (DWORD interpretation, see note)}

procedure nextprime32_array(n,cmax: longint; var a: array of longint);
  {-fill an array with the next 32 bit primes >= n (DWORD interpretation).}
  { If a[i0]=4294967291 then a[i]=0 for i>i0. If cmax<=0 fill complete array.}

function  prevprime32(n: longint): longint;
  {-previous 32 bit prime <= n, prevprime32(0)=0, (DWORD interpretation)}



const
  SIEVE_MAXPRIME = 1908867043; {maximum prime that will be generated, see note}
  SIEVE_PRIMES   = 4550;       {= primepi(sqrt(SIEVE_MAXPRIME))-1}
  SIEVE_BLOCK    = 32768;      {sieve size, may be set to 16384 for BIT16 if memory is low}

type
  TAux   = array[0..SIEVE_PRIMES-1] of word;   {prime offsets in sieve}
  TFlags = array[0..SIEVE_BLOCK-1] of boolean; {prime flags in sieve}
  TSieve = record                              {prime sieve context}
             paux    : ^TAux;                  {pointer to offsets }
             psieve  : ^TFlags;                {pointer to flags   }
             curr_blk: longint;                {current sieve block}
             curr_off: longint;                {offset in curr_blk }
           end;

procedure prime_sieve_clear(var sieve: TSieve);
  {-Release memory allocated by prime_sieve_init}

procedure prime_sieve_init(var sieve: TSieve; first_prime: longint);
  {-Allocate/initialize sieve to return primes >= first_prime; first_prime <= SIEVE_MAXPRIME}

function  prime_sieve_next(var sieve: TSieve): longint;
  {-Return next prime from sieve, 1 if done}

procedure prime_sieve_reset(var sieve: TSieve; first_prime: longint);
  {-Initialize already allocated sieve to return primes >= first_prime; first_prime <= SIEVE_MAXPRIME}


{#Z+}
{Tables for prime residue classes mod 210, calculated with t_rcnp.pas}
{Must be interfaced because they are used by mp_nextprime/mp_prevprime.}
{NPRC_OddIdx has the number of the prime residue classes of an odd value }
{n mod NPRC_MOD, or -1 if there is no corresponding prime residue class. }
{NPRC_Diff contains the differences: NPRC_Diff[i] = prctab[i+1]-prctab[i]}

{prime residue classes mod 210. Cnt=48=1*2*4*6, numbered from 0 to 47}
  { 1,  11,  13,  17,  19,  23,  29,  31,  37,  41,  43,  47,
   53,  59,  61,  67,  71,  73,  79,  83,  89,  97, 101, 103,
  107, 109, 113, 121, 127, 131, 137, 139, 143, 149, 151, 157,
  163, 167, 169, 173, 179, 181, 187, 191, 193, 197, 199, 209}

const
  NPRC_MOD = 210;
  NPRC_NRC = 48;
const
  NPRC_OddIdx:  array[0..(NPRC_MOD div 2)-1] of shortint =
                ( 0, -1, -1, -1, -1,  1,  2, -1,  3,  4, -1,  5, -1, -1,  6,
                  7, -1, -1,  8, -1,  9, 10, -1, 11, -1, -1, 12, -1, -1, 13,
                 14, -1, -1, 15, -1, 16, 17, -1, -1, 18, -1, 19, -1, -1, 20,
                 -1, -1, -1, 21, -1, 22, 23, -1, 24, 25, -1, 26, -1, -1, -1,
                 27, -1, -1, 28, -1, 29, -1, -1, 30, 31, -1, 32, -1, -1, 33,
                 34, -1, -1, 35, -1, -1, 36, -1, 37, 38, -1, 39, -1, -1, 40,
                 41, -1, -1, 42, -1, 43, 44, -1, 45, 46, -1, -1, -1, -1, 47);
const
  NPRC_Diff:    array[0..NPRC_NRC-1] of mp_digit =
                (10, 2, 4, 2, 4, 6, 2, 6, 4, 2, 4, 6, 6, 2, 6, 4,
                  2, 6, 4, 6, 8, 4, 2, 4, 2, 4, 8, 6, 4, 6, 2, 4,
                  6, 2, 6, 6, 4, 2, 4, 6, 2, 6, 4, 2, 4, 2,10, 2);
{#Z-}


implementation

uses
  mp_base; {for mp_getmem/mp_freemem}


{---------------------------------------------------------------------------}
function IsPrime16(N: word): boolean;
  {-test if N is prime}
begin
  {use inline if speed is needed}
  IsPrime16 := (N=2) or (pbits16[N shr 4] and pmask16[N and $0F] <> 0);
end;


{---------------------------------------------------------------------------}
function Primes16Index(n: word): integer;
  {-Get index of largest prime <= n in Primes16 array; 1 if n<=2}
var
  l,r,m: integer;
  p: word;
begin
  if n<=2 then Primes16Index := 1
  else if n>=Primes16[NumPrimes16] then Primes16Index := NumPrimes16
  else begin
    {binary search of index}
    l := 1;
    r := NumPrimes16;
    repeat
      m := (l+r) shr 1;
      p := Primes16[m];
      if n<p then r := m-1
      else if n>p then l := m+1
      else begin
        Primes16Index := m;
        exit;
      end;
    until l>r;
    {n not prime, if n<p adjust middle index}
    if n<p then Primes16Index := m-1
    else Primes16Index := m;
  end;
end;


{$ifdef BIT32}
{---------------------------------------------------------------------------}
function _spsp32(a,N,d,s: longint): boolean;
  {-Strong pseudo prime test for N with base a, N-1=2^s*d, s>0}
var
  res: boolean;
begin
  res := true;
  asm
        pushad
        mov   esi,[N]          {esi=N}
        mov   ecx,[a]          {ecx= a mod N}
        cmp   ecx,esi
        jb    @@0
        mov   eax,ecx          {reduce a mod N}
        sub   edx,edx          {to avoid overflow}
        div   esi
        mov   ecx,edx          {ecx=a mod N}
@@0:    mov   ebx,ecx          {ebx=p=a}
        mov   edi,[d]          {edi=dk}
        shr   edi,1            {dk=dk shr 1}
      {while   dk<>0 do }
@@1:    or    edi,edi
        jz    @@3
        mov   eax,ecx          {a=a^2 mod N}
        mul   eax
        div   esi
        mov   ecx,edx
        shr   edi,1            {dk=dk shr 1}
        jnc   @@1
        mov   eax,ebx          {p=p*a mod N}
        mul   ecx
        div   esi
        mov   ebx,edx
        jmp   @@1
      {end while}

@@3:    dec   ebx              {if p=1 then goto done}
        jz    @@5
        inc   ebx
        mov   edi,[N]
        dec   edi              {edi=N1}
        cmp   ebx,edi          {if p=N1 then goto done}
        jz    @@5
        mov   ecx,s            {remember: s > 0}
        dec   ecx              {if s=1 then N not prime}
        jz    @@np
      {for i=2 to s do}
@@4:    mov   eax,ebx          {p=p^2 mod N}
        mul   ebx
        div   esi
        mov   ebx,edx
        cmp   ebx,edi          {if p=N1 then goto done}
        jz    @@5
        dec   ecx
        jnz   @@4
      {end for}
@@np:   mov   [res],cl         {not prime, here cl=0!}
@@5:
        popad
  end;
  _spsp32 := res;
end;
{$else}
{---------------------------------------------------------------------------}
function _spsp32(a,N,d,s: longint): boolean;
  {-Strong pseudo prime test for N with base a, N-1=2^s*d, s>0}
var
  res: boolean;
begin
  res := true;
  asm
        db $66;  mov  si,word ptr [N]    {si=N}
        db $66;  mov  cx,word ptr [a]    {cx=a mod N}
        db $66;  cmp  cx,si
                 jb   @@0
        db $66;  mov  ax,cx
        db $66;  sub  dx,dx              {to avoid overflow}
        db $66;  div  si
        db $66;  mov  cx,dx              {cx=a mod N fixed in V1.2.12}
@@0:    db $66;  mov  bx,cx              {bx=p=a}
        db $66;  mov  di,word ptr [d]    {di=dk}
        db $66;  shr  di,1               {dk=dk shr 1}
        {while   dk<>0 do }
@@1:    db $66;  or   di,di
                 jz   @@3
        db $66;  mov  ax,cx              {a=a^2 mod N}
        db $66;  mul  ax
        db $66;  div  si
        db $66;  mov  cx,dx
        db $66;  shr  di,1               {dk=dk shr 1}
                 jnc  @@1
        db $66;  mov  ax,bx              {p=p*a mod N}
        db $66;  mul  cx
        db $66;  div  si
        db $66;  mov  bx,dx
                 jmp  @@1
        {end while}

@@3:    db $66;  dec  bx                 {if p=1 then goto done}
                 jz   @@5
        db $66;  inc  bx
        db $66;  mov  di,word ptr [N]    {di=N1}
        db $66;  dec  di
        db $66;  cmp  bx,di              {if p=N1 then goto done}
                 jz   @@5
                 mov  cx,word ptr [s]    {remember: s > 0}
                 dec  cx                 {if s=1 then N not prime}
                 jz   @@np
        {for i=2 to s do}
@@4:    db $66;  mov  ax,bx              {p=p^2 mod N}
        db $66;  mul  bx
        db $66;  div  si
        db $66;  mov  bx,dx
        db $66;  cmp  bx,di              {if p=N1 then goto done}
                 jz   @@5
                 dec  cx
                 jnz  @@4
        {end for}
@@np:   mov      [res],cl                {not prime, here cl=0!}
@@5:
  end;
  _spsp32 := res;
end;
{$endif}


{---------------------------------------------------------------------------}
function IsPrime32(N: longint): boolean;
  {-test if longint (DWORD) N is prime}
const
  a1: array[0..1] of longint = (31,73);
  a2: array[0..2] of longint = (2,7,61);
type
  LH = packed record L,H: word; end;
begin
  if LH(N).H=0 then begin
    {$ifdef BIT32}
      IsPrime32 := (N=2) or (pbits16[N shr 4] and pmask16[N and $0F] <> 0);
    {$else}
      IsPrime32 := (word(N)=2) or (pbits16[word(N) shr 4] and pmask16[word(N) and $0F] <> 0);
    {$endif}
  end
  else begin
    {Normally here N>=2^16, IN ANY CASE: N MUST NOT BE IN [2,7,31,61,73]!}
    if odd(N) then begin
      {First test N and $80000000 <> 0 selects N>MaxLongint,}
      {second test is the upper bound for spsp set (31,73)] }
      if (N and $80000000 <> 0) or (N>=9080191) then IsPrime32 := is_spsp32A(N, a2)
      else IsPrime32 := is_spsp32A(N, a1);
    end
    else begin
      {N is even, N<>2 therefore N not prime}
      IsPrime32 := false;
    end;
  end;
end;


{-----------------------------------------------------------------------------}
function is_spsp32A(N: longint; const bases: array of longint): boolean;
  {-Strong pseudo prime test for N with a number of bases.}
  { Return true if N is a spsp for all bases, or if N=2.  }
  { Negative numbers are treated as unsigned (=cardinal)  }
var
  s,d,N1: longint;
  k: integer;
begin
  N1 := N-1;
  if odd(N1) or (N1=0) then begin
    is_spsp32A := N=2;
    exit;
  end;

  {calculate d*2^s = N-1, this is done once for all bases}
  {$ifdef BIT16}
    asm
      db $66;  mov  ax,word ptr [N1]
      db $66,       $0F,$BC,$C8     {bsf  ecx,eax}
      db $66;  mov  word ptr [s],cx
      db $66;  shr  ax,cl
      db $66;  mov  word ptr [d],ax
    end;
  {$else}
    asm
      mov  eax,[N1]
      bsf  ecx,eax
      mov  [s],ecx
      shr  eax,cl
      mov  [d],eax
    end;
  {$endif}

  {default no spsp}
  is_spsp32A := false;
  {now loop thru all the bases}
  for k:=low(bases) to high(bases) do begin
    {spsp test for bases[k]}
    if not _spsp32(bases[k],N,d,s) then exit;
  end;
  {All tests passed, this is a spsp}
  is_spsp32A := true;
end;


{---------------------------------------------------------------------------}
function is_spsp32(N, A: longint): boolean;
  {-Strong pseudo prime test for N with base A, calls is_spsp32A}
begin
  is_spsp32 := is_spsp32A(N, A);
end;


{$ifdef BIT32}
{---------------------------------------------------------------------------}
function modnpd2(n: longint): integer; assembler; {&frame-}
  {-calculate (n mod NPRC_MOD) div 2 (DWORD interpretation)}
asm
  {$ifdef LoadArgs}
    mov eax,[n]
  {$endif}
   mov  ecx,NPRC_MOD
   sub  edx,edx
   div  ecx
   mov  eax,edx
   shr  eax,1
end;
{$else}
{---------------------------------------------------------------------------}
function modnpd2(n: longint): integer; assembler;
  {-calculate (n mod NPRC_MOD) div 2 (DWORD interpretation)}
asm
  db $66; mov ax, word ptr [n]
  db $66; sub dx,dx
  db $66; mov cx,NPRC_MOD; dw 0;
  db $66; div cx
          mov ax,dx
          shr ax,1
end;
{$endif}


{---------------------------------------------------------------------------}
function nextprime32(n: longint): longint;
  {-next 32 bit prime >= n (DWORD interpretation, see note)}
var
  id,k: integer;
begin
  {easy outs and assure valid range for mod MP_MOD calculation}

  {note: (n>=-4) CANNOT be omitted because of DWORD interpretation}
  {for m=-4=$FFFFFFFC=4294967292, nextprime(m) is greater 2^32 and}
  {will be set to 0 (nextprime(m) mod 2^32 = 15 would be strange!)}

  if (n>=-4) and (n<=7) then begin
    if n<0 then nextprime32 := 0
    else if n<=2 then nextprime32 := 2
    else if n<=3 then nextprime32 := 3
    else if n<=5 then nextprime32 := 5
    else  nextprime32 := 7;
    exit;
  end;

  {make n odd}
  if n and 1 = 0 then inc(n);

  {$ifdef DELPHI}
    {avoid warning, id WILL always be initialized (for bug-free modnpd2!)}
    id := 0;
  {$endif}

  {move n to next prime residue class mod MP_MOD and index id into diff array}
  for k:=modnpd2(n) to (NPRC_MOD div 2)-1 do begin
    id := NPRC_OddIdx[k];
    {note: loop terminates via break because NPRC_OddIdx[(NPRC_MOD div 2)-1]<>-1}
    if id<>-1 then break;
    inc(n,2);
  end;

  repeat
    {loop thru possible primes}
    if IsPrime32(n) then begin
      nextprime32 := n;
      exit;
    end;
    {move to next candidate}
    inc(n,longint(NPRC_Diff[id]));
    {get next increment index}
    inc(id); if id>=NPRC_NRC then id:=0;
  until false;
end;


{---------------------------------------------------------------------------}
procedure FindFirstPrime32(n: longint; var ctx: TPrimeContext);
  {-Find first prime >= n and initialize ctx for FindNextPrime32}
begin
  with ctx do begin
    next  := n;
    index := -1;
  end;
  FindNextPrime32(ctx);
end;


{---------------------------------------------------------------------------}
procedure FindNextPrime32(var ctx: TPrimeContext);
  {-Find next 32 bit prime (DWORD interpretation, see note), success if ctx.prime<>0}
var
  k: integer;
  found: boolean;
const
  MP32 = longint($FFFFFFFB); {4294967291 = prevprime(2^32)}
begin
  with ctx do begin
    if index<0 then begin
      {note: (n>=-4) CANNOT be omitted because of DWORD interpretation}
      if (next>=-4) and (next<=7) then begin
        if next<0 then begin
          prime := 0;
          next  := -1;
        end
        else if next<=2 then begin
          prime := 2;
          next  := 3;
        end
        else if next<=3 then begin
          prime := 3;
          next  := 5;
        end
        else if next<=5 then begin
          prime := 5;
          next  := 7;
        end
        else  begin
          prime := 7;
          next  := 11;
        end;
        exit;
      end;
      {first index calculation after FindFirstPrim32}
      {make n odd}
      if next and 1 = 0 then inc(next);
      {move n to next prime residue class mod MP_MOD and index id into diff array}
      for k:=modnpd2(next) to (NPRC_MOD div 2)-1 do begin
        index := NPRC_OddIdx[k];
        {note: loop terminates via break because NPRC_OddIdx[(NPRC_MOD div 2)-1]<>-1}
        if index<>-1 then break;
        inc(next,2);
      end;
    end;
    repeat
      {loop thru possible primes}
      found := IsPrime32(next);
      if found then begin
        prime := next;
        if next=MP32 then begin
          next  := -1;
          index := -2;
          exit;
        end;
      end;
      {move to next candidate}
      inc(next,longint(NPRC_Diff[index]));
      {get next increment index}
      inc(index);
      if index>=NPRC_NRC then index:=0;
    until found;
  end;
end;


{---------------------------------------------------------------------------}
procedure nextprime32_array(n,cmax: longint; var a: array of longint);
  {-fill an array with the next 32 bit primes >= n (DWORD interpretation).}
  { If a[i0]=4294967291 then a[i]=0 for i>i0. If cmax<=0 fill complete array.}
var
  i,k,ma: longint;
  ctx: TPrimeContext;
begin
  ma := high(a);
  if cmax>0 then begin
    dec(cmax);
    if cmax<ma then ma := cmax;
  end;
  FindFirstPrime32(n, ctx);
  for i:=low(a) to ma do begin
    a[i] := ctx.prime;
    if ctx.prime=0 then begin
      {if no more 32 bit primes fill rest of array with 0}
      for k:=i+1 to ma do a[k] := 0;
      break;
    end;
    FindNextPrime32(ctx);
  end;
end;


{---------------------------------------------------------------------------}
function prevprime32(n: longint): longint;
  {-previous 32 bit prime <= n, prevprime32(0)=0, (DWORD interpretation)}
var
  id,k: integer;
begin
  {easy outs and assure valid range for mod MP_MOD calculation}
  {note: (n>=0) CANNOT be omitted because of DWORD interpretation}

  if (n>=0) and (n<11) then begin
    if n<2 then prevprime32 := 0
    else if n<3 then prevprime32 := 2
    else if n<5 then prevprime32 := 3
    else if n<7 then prevprime32 := 5
    else prevprime32 := 7;
    exit;
  end;

  {make n odd}
  if n and 1 = 0 then dec(n);

  {$ifdef DELPHI}
    {avoid warning, id WILL always be initialized (for bug-free modnpd2!)}
    id := 0;
  {$endif}

  {move n to prev prime residue class mod MP_MOD and index id into diff array}
  for k:=modnpd2(n) downto 0 do begin
    id := NPRC_OddIdx[k];
    {note: loop is always terminated via break because NPRC_OddIdx[0]<>-1}
    if id<>-1 then break;
    dec(n,2);
  end;

  repeat
    {loop thru possible primes}
    if IsPrime32(n) then begin
      prevprime32 := n;
      exit;
    end;
    {get prev increment index}
    dec(id); if id<0 then id:=NPRC_NRC-1;
    {move to prev candidate}
    dec(n,longint(NPRC_Diff[id]));
  until false;
end;


{---------------------------------------------------------------------------}
{---------------------------------------------------------------------------}

{This prime sieve is based on Jason Papadopoulos's public domain code}
{prime_delta.c available from http://www.boo.net/~jasonp/qs.html     }
{Note, that this is a heavily modified version, which uses booleans  }
{instead of bits. The maximum prime is tailored to 1908867043, which }
{is the largest prime below MaxLongint with all paux^[i] <= $ffff.   }
{Jason generates the 16 bit odd primes on the fly, numbered from 0.  }
{We use the Primes16 array with Primes16[1]=2, therefore paux^[i] is }
{indexed with the shift +2. Additionally the sieve procedures do some}
{checks and return 1 if all primes are generated.}

const
  SIEVE_BLOCK2   = 2*SIEVE_BLOCK;
  SIEVE_MAXBLOCK = 1 + SIEVE_MAXPRIME div SIEVE_BLOCK2;


{---------------------------------------------------------------------------}
procedure prime_sieve_nextblock(var sieve: TSieve);
  {-Perform sieving for the next block}
var
  i: integer;
  {$ifdef BIT16}
    p,r: word;
  {$else}
    p,r: longint;
  {$endif}
begin
  with sieve do begin
    fillchar(psieve^,SIEVE_BLOCK,ord(false));
    for i:=0 to (SIEVE_PRIMES-1) do begin
      p := Primes16[i+2];
      r := paux^[i];
      while r < SIEVE_BLOCK do begin
        psieve^[r] := true;
        inc(r,p);
        {$ifdef BIT16}
          {detect overflow, note that inc/dec do not produce overflow RTE}
          if r<p then break;
        {$endif}
      end;
      dec(r,SIEVE_BLOCK);
      paux^[i] := word(r);
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure prime_sieve_reset(var sieve: TSieve; first_prime: longint);
  {-Initialize already allocated sieve to return primes >= first_prime; first_prime <= SIEVE_MAXPRIME}
var
  i: integer;
  block_start: longint;
  {$ifdef BIT16}
    p,r: word;
  {$else}
    p,r: longint;
  {$endif}
begin
  with sieve do begin
    if (paux=nil) or (psieve=nil) or (first_prime>SIEVE_MAXPRIME) then begin
      {Out of memory/not allocated or first_prime too large, indicate 'done'}
      curr_blk := SIEVE_MAXBLOCK;
      exit;
    end;

    fillchar(psieve^,SIEVE_BLOCK,ord(false));

    if first_prime and 1 <> 0 then dec(first_prime)
    else if first_prime<=2 then first_prime := 1;

    {Calculate block number and offset of first prime}
    curr_blk :=  first_prime div SIEVE_BLOCK2;
    curr_off := (first_prime mod SIEVE_BLOCK2) shr 1;

    {Calculate the sieve offsets into the current block. The sieve is}
    {compressed so that even multiples of sieving primes are skipped.}
    block_start := curr_blk*SIEVE_BLOCK2;
    if block_start=0 then begin
      {if preparing block 0, also skip the first sieve update}
      for i:= 0 to (SIEVE_PRIMES-1) do begin
        p := Primes16[i+2];
        paux^[i] := p + p shr 1;
      end;
    end
    else begin
      for i:=0 to (SIEVE_PRIMES-1) do begin
        p := Primes16[i+2];
        r := p - (block_start mod p);
        if r and 1 = 0 then inc(r,p);
        paux^[i] := r shr 1;
      end;
    end;
  end;
  prime_sieve_nextblock(sieve);
end;


{---------------------------------------------------------------------------}
procedure prime_sieve_init(var sieve: TSieve; first_prime: longint);
  {-Allocate/initialize sieve to return primes >= first_prime; first_prime <= SIEVE_MAXPRIME-SIEVE_BLOCK}
begin
  with sieve do begin
    paux   := mp_getmem(sizeof(TAux));
    psieve := mp_getmem(sizeof(TFlags));
    if (paux=nil) or (psieve=nil) then begin
      {Out of memory, indicate 'done'}
      curr_blk := SIEVE_MAXBLOCK;
      exit;
    end;
  end;
  prime_sieve_reset(sieve, first_prime);
end;


{---------------------------------------------------------------------------}
procedure prime_sieve_clear(var sieve: TSieve);
  {-release memory allocated by prime_sieve_init}
begin
  with sieve do begin
    mp_freemem(pointer(paux),sizeof(TAux));
    mp_freemem(pointer(psieve),sizeof(TFlags));
  end;
end;


{---------------------------------------------------------------------------}
function prime_sieve_next(var sieve: TSieve): longint;
  {-Return next prime from sieve, 1 if done}
var
  {$ifdef BIT16}
    off: word;
  {$else}
    off: longint;
  {$endif}
begin
  with sieve do begin
    if curr_blk>=SIEVE_MAXBLOCK then begin
      prime_sieve_next := 1;
      exit;
    end;
    off := curr_off;
    if (off=0) and (curr_blk=0) then begin
      {special case 2}
      curr_off := 1;
      prime_sieve_next := 2;
      exit;
    end;
    repeat
      while off < SIEVE_BLOCK do begin
        if not psieve^[off] then begin
          curr_off := off + 1;
          {If SIEVE_BLOCK2 is no power of 2 use * instead of shl!}
          {prime_sieve_next := curr_blk * SIEVE_BLOCK2 + curr_off + off;}
          prime_sieve_next := curr_blk shl 16 + curr_off + off;
          exit;
        end;
        inc(off);
      end;
      inc(curr_blk);
      off := 0;
      if curr_blk<SIEVE_MAXBLOCK then prime_sieve_nextblock(sieve)
      else begin
        prime_sieve_next := 1;
        exit;
      end;
    until false;
  end;
end;


begin
  {If assert fails adjust code in prime_sieve_next}
  assert(SIEVE_BLOCK2 = 1 shl 16, MPAF+'SIEVE_BLOCK2 = 1 shl 16');
end.
