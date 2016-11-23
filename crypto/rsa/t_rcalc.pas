{Test program for mp_rcalc unit, (c) W.Ehrhardt 2008-2010}

program t_rcalc;

{$i STD.INC}
{$i mp_conf.inc}

{$x+}  {pchar I/O}
{$i+}  {RTE on I/O error}

{$ifdef BIT16}
{$N+}
{$endif}

{$ifdef Delphi}
{$J+}
{$endif}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

(*
2008-01-17: prec [n], display prec
2008-04-19: help: added pi, removed !
2008-09-16: bin, adjust ndd if radix changes, help: log(a,b)
2008-09-25: HexLong, removed mem_util
2008-11-04: Initial Starttimer
2008-12-02: Uses BTypes
2009-01-09: display sign with ; line terminator
2009-07-07: D12 fixes, display new prec
2009-11-10: additional elementary transcendental functions
2010-01-09: XH command
2010-09-05: DDH, SCI, ALT commands
03.06.10    display MPF_MAX_PREC related info
*)

{
TODO:
 - edit last line
}

uses
  BTypes, HRTimer,
  {$ifdef MPC_Diagnostic}
     mp_supp,
  {$endif}
  mp_types, mp_base, mp_real, mp_rcalc;


{---------------------------------------------------------------------------}
function HexLong(L: longint): mp_string;
  {-longint as hex string, LSB first}
var
  i: integer;
  s: string[8];
begin
  s := '';
  for i:=0 to 7 do begin
    s := mp_ucrmap[L and $F] + s;
    L := L shr 4;
  end;
  HexLong := s;
end;


{---------------------------------------------------------------------------}
function HexW(w: word): mp_string;
  {-longint as hex string, LSB first}
var
  i: integer;
  s: string[8];
begin
  s := '';
  for i:=0 to 3 do begin
    s := mp_ucrmap[w and $F] + s;
    w := w shr 4;
  end;
  HexW := s;
end;


{---------------------------------------------------------------------------}
function HexExtended(x: extended): mp_string;
  {-Extended as Hex array of word}
var
  xa: array[0..4] of word absolute x;
begin
  HexExtended := '($'+HexW(xa[0])+',$'+HexW(xa[1])+',$'+HexW(xa[2])+',$'+HexW(xa[3])+',$'+HexW(xa[4])+')';
end;


{---------------------------------------------------------------------------}
function HexDoubleDouble(ddh,ddl: double): mp_string;
var
  h: array[0..3] of word absolute ddh;
  l: array[0..3] of word absolute ddl;
begin
  HexDoubleDouble := '($'+HexW(l[0])+',$'+HexW(l[1])+',$'+HexW(l[2])+',$'+HexW(l[3])
                    +',$'+HexW(h[0])+',$'+HexW(h[1])+',$'+HexW(h[2])+',$'+HexW(h[3])+')';
end;


{---------------------------------------------------------------------------}
procedure ShowInfo;
begin
  writeln;
  writeln('Operators:  +  -  *  /  ^              Constant:  pi');
  writeln;
  writeln('Functions: abs(a)         agm(a,b)     arccos(a)    arccosh(a)   arccosh1p(a)');
  writeln('           arccot(a)      arccotc(a)   arccoth(a)   arccsc(a)    arccsch(a)');
  writeln('           arcsec(a)      arcsech(a)   arcsin(a)    arcsinh(a)   arctan(a)');
  writeln('           arctan2(y,x)   arctanh(a)   CE(a)        CK(a)        cos(a)');
  writeln('           cosh(a)        cot(a)       coth(a)      csc(a)       csch(a)');
  writeln('           exp(a)         expm1(a)     frac(a)      int(a)       ln(a)');
  writeln('           ln1p(a)        log10(a)     log2(a)      log(a,b)     max(a,b)');
  writeln('           min(a,b)       random(a)    sec(a)       sech(a)      sin(a)');
  writeln('           sinh(a)        sqr(a)       sqrt(a)      tan(a)       tanh(a)');
  writeln;
  writeln('Variables:  x  y  z,  Syntax: Var=expression[;] or Var=_[;]');
  writeln;
  writeln('Other    :  line terminator ";" shows only sign/ldx/chksum/time of result');
  writeln('            bin, dec, hex: set radix for result display');
  writeln('            prec [n]: display/set bit precision');
  writeln('            xh, ddh: re-displays last result as hex extended/doubledouble');
  writeln('            sci, alt: display results using scientific/alternative format');
  writeln('            "_<enter>" re-displays last result');
  writeln('            ".<enter>" displays time for last calculation');
  writeln;
end;


{---------------------------------------------------------------------------}
procedure HelpLine;
begin
  writeln('Type "?<enter>" to get some info about commands, "\q" or "quit" to end.');
end;


const
 ndd: word = 60;

var
  evr: TFEval;
{$ifdef VirtualPascal}
  ir: longint;
{$else}
  ir: integer;
{$endif}
  EP,i: integer;
  HR: THRTimer;
  dp,ctime,ddh,ddl: double;
  ac: char8;
  use_sci,print,script: boolean;
  s: string[255];
  ss: string[2];
  cmd: string[20];
  radix: word;
  rc: char8;
  prec: longint;
  tmp: mp_float;
  tx: extended;
begin

  StartTimer(HR);
  mpf_init_eval(evr);
  mpf_init(tmp);

  mp_uppercase := true;
  use_sci := true;
  script := false;
  ctime  := 0.0;
  ac := #0;
  rc := 'D';
  radix := 10;

  for i:=1 to paramcount do begin
    if (paramstr(i)='/s') or (paramstr(i)='/S') then script := true;
  end;

  writeln('Test of MPArith V', MP_VERSION, ' [mp_rcalc]   (c) W.Ehrhardt 2008-2010');
  writeln('Karatsuba  cutoffs: mul/sqr = ',mp_mul_cutoff,'/',mp_sqr_cutoff);
  writeln('Toom-3, BZ cutoffs: mul/sqr = ',mp_t3m_cutoff,'/',mp_t3s_cutoff,  ',  div = ',mp_bz_cutoff);
  if not script then HelpLine;
  prec := mpf_get_default_prec;
  dp :=prec*ln(2)/ln(10);
  writeln('Current bit precision = ', prec, ' (max:',MPF_MAX_PREC, '),  decimal precision = ', dp:1:1);
  writeln;

  repeat
    if not script then write('[',rc,']:=> ');
    readln(s);
    while (s<>'') and (s[1]=' ') do delete(s,1,1);
    while (s<>'') and (s[length(s)]=' ') do delete(s,length(s),1);
    if s='' then continue;

    if s='?' then begin
      ShowInfo;
      continue;
    end;
    if s='.' then begin
      writeln('Time = ', ctime:1:3, ' ms');
      continue;
    end;
    if s[1]='"' then begin
      delete(s,1,1);
      writeln(s);
      continue;
    end;

    cmd := copy(s,1,10);
    for i:=1 to length(cmd) do cmd[i] := upcase(cmd[i]);

    if (cmd='\Q') or (cmd='Q') or (cmd='QUIT') or (cmd='\@') then break;
    if cmd='HEX' then begin
      radix := 16;
      rc := 'H';
      dp := prec/4.0;
      if dp<60 then ndd := trunc(dp) else ndd := 60;
      if script then writeln(cmd);
      continue;
    end;
    if cmd='DEC' then begin
      radix := 10;
      rc := 'D';
      dp := prec*ln(2)/ln(10);
      if dp<60 then ndd := trunc(dp) else ndd := 60;
      if script then writeln(cmd);
      continue;
    end;
    if cmd='BIN' then begin
      radix := 2;
      rc := 'B';
      if prec<60 then ndd := prec else ndd := 60;
      if script then writeln(cmd);
      continue;
    end;
    if (cmd='ALT') or (cmd='SCI') then begin
      use_sci := cmd='SCI';
      continue;
    end;
    if copy(cmd,1,4)='PREC' then begin
      if length(cmd)=4 then begin
        prec := mpf_get_default_prec;
        dp := prec*ln(2)/ln(10);
        writeln('Current bit precision = ', prec, ',  decimal precision = ', dp:1:1);
        continue;
      end;
      delete(cmd,1,4);
      while (cmd<>'') and (cmd[1]=' ') do delete(cmd,1,1);
      {$ifdef D12Plus}
        val(string(cmd),prec,ir);
      {$else}
        val(cmd,prec,ir);
      {$endif}
      if (ir=0) and (prec>=MPF_MIN_PREC) then begin
        if prec>MPF_MAX_PREC then begin
          prec := MPF_MAX_PREC;
          write('** prec > ',MPF_MAX_PREC, ' - ');
        end;
        mpf_set_default_prec(prec);
        prec := mpf_get_default_prec;
        dp :=prec*ln(2)/ln(10);
        if dp<60 then ndd := trunc(dp) else ndd := 60;
        mpf_chg_prec(evr.Res, prec);
        writeln('New bit precision = ', prec, ',  decimal precision = ', dp:1:1);
        continue;
      end;
    end;
    if cmd='XH' then begin
      if (Evr.Err=0) and (Evr.Res.bitprec>64) then begin
        tx := mpf_toextended(Evr.Res);
        if abs(tx)<> DblPosInf then begin
          mpf_set_ext(tmp,tx);
          write('Result = ', HexExtended(tx));
          writeln('  {',mpf_decimal(tmp,20),'}');
        end
        else writeln('** INF');
      end;
      continue;
    end;
    if cmd='DDH' then begin
      if (Evr.Err=0) and (Evr.Res.bitprec>106) then begin
        ddh := mpf_todouble(Evr.Res);
        if abs(ddh)<>DblPosInf then begin
          mpf_set_ext(tmp,ddh);
          mpf_sub(Evr.Res,tmp,tmp);
          ddl := mpf_todouble(tmp);
          write('Result = ', HexDoubleDouble(ddh,ddl));
          writeln('  {',ddh,',', ddl,'}');
        end
        else writeln('** INF');
      end;
      continue;
    end;

    {Echo input in script mode}
    if script then writeln('[Expr] = ',s);

    {Check for trailing ";", i.e. dont print the result}
    if (s<>'') and (s[length(s)]=';') then begin
      delete(s,length(s),1);
      while (s<>'') and (s[length(s)]=' ') do delete(s,length(s),1);
      print := false;
    end
    else print := true;
    if s='' then continue;

    if s<>'_' then begin
      s := s + #0;
      ac := #0;
      if (length(s)>1) and (upcase(s[1]) in ['X','Y','Z']) then begin
        {Check if we have an assignment to x,y,z. Analyse first non-blank}
        {char; next while/if statements are OK because s has trailing #0}
        while (s[2]=' ') and (s[3]=' ') do delete(s,2,1);
        if (s[2]=' ') and (s[3]='=') then delete(s,2,1);
        if s[2]='=' then begin
          ac := upcase(s[1]);
          delete(s,1,2);
          while (s<>'') and (s[1]=' ') do delete(s,1,1);
        end;
      end;
      if s<>'_'#0 then begin
        RestartTimer(HR);
        mpf_calculate(@s[1],evr,EP);
        ctime := 1000.0*ReadSeconds(HR);
        if evr.Err=Err_MPERR_Eval then set_mp_error(MP_OKAY);
      end;
    end
    else if evr.Err>0 then continue;

    if evr.Err>0 then begin
      writeln('Error ', evr.Err,', ',mpf_calc_errorstr(evr.Err),':  <',copy(s,EP+1,length(s)-EP-1), '>');
      if evr.Err=Err_Unknown_Function then HelpLine;
    end
    else if evr.Err<0 then writeln(#7'Error: ', evr.Err, ' [',mpf_calc_errorstr(evr.Err),']')
    else begin
      if ac in ['X', 'Y', 'Z'] then begin
        case ac of
          'X':  mpf_copy(evr.Res,evr.X);
          'Y':  mpf_copy(evr.Res,evr.Y);
          'Z':  mpf_copy(evr.Res,evr.Z);
        end;
        write(ac,' = ');
      end
      else write('Result = ');
      if print then begin
        if use_sci then mpf_output_radix(evr.Res, radix, ndd)
        else mpf_output_radix_alt(evr.Res, radix, ndd);
      end
      else begin
        case mp_sign(evr.Res.Mantissa) of
          +1 : ss := '>0';
           0 : ss := '=0';
          else ss := '<0'
        end;
        write('[ ',ss,',  ldx=',s_mpf_ldx(evr.Res), ',  chksum=$',
              HexLong(mpf_checksum(evr.Res)),',  time=', ctime:1:3, ' ms]');
      end;
    end;
    writeln;
  until false;
  mpf_clear(tmp);
  mpf_clear_eval(evr);
  {$ifdef MPC_Diagnostic}
     mp_dump_meminfo;
     mp_dump_diagctr;
  {$endif}
end.
