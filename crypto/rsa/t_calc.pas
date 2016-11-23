{Test program for mp_calc unit, (c) W.Ehrhardt 2006-2009}

program t_calc;

{$i std.inc}
{$i mp_conf.inc}

{$x+}  {pchar I/O}
{$i+}  {RTE on I/O error}

{$ifdef BIT16}
{$N+}
{$endif}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


(*
2007-09-04: line terminator ";", redisplays with "_", script mode
2007-09-09: hex, dec, bin, binomial, var=_, checksum
2007-10-24: measure/display calculation time
2007-10-25: sqrtmod
2008-01-30: and/or/xor
2008-04-15: fix parsing of e.g. 'x mod 10',
            show warning if result has more than $F000 chars
2008-06-15: ispprime
2008-09-25: HexLong, removed mem_util
2008-11-04: Initial Starttimer
2008-12-02: Uses BTypes
2009-01-09: display sign with ; line terminator
2009-02-02: cbrtmod in help display
2010-12-30: !! in help display
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
  mp_types, mp_base, mp_calc;


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
procedure ShowInfo;
begin
  writeln;
  writeln('Operators:  +  -  *  /  div  mod  ^  !  !!  # (primorial)  % (same as mod)');
  writeln;
  writeln('Functions:  abs(a)         and(a,b)      binomial(a,b)    cbrtmod(a,b) ');
  writeln('            fermat(a)      fib(a)        gcd(a,b)         invmod(a,b)  ');
  writeln('            ispprime(a)    jacobi(a,b)   kronecker(a,b)   lcm(a,b)     ');
  writeln('            luc(a,b)       max(a,b)      mersenne(a)      min(a,b)     ');
  writeln('            nextprime(a)   or(a,b)       prevprime(a)     random(a)    ');
  writeln('            root(a,b)      sqr(a)        sqrt(a)          sqrtmod(a,b) ');
  writeln('            xor(a,b)');
  writeln;
  writeln('Variables:  x  y  z,  Syntax: Var=expression[;] or Var=_[;]');
  writeln;
  writeln('Other    :  line terminator ";" shows only sign/bitsize/chksum/time of result');
  writeln('            bin, dec, hex: set radix for result display');
  writeln('            "_<enter>" re-displays last result');
  writeln('            ".<enter>" displays time for last calculation');
  writeln;
end;


{---------------------------------------------------------------------------}
procedure HelpLine;
begin
  writeln('Type "?<enter>" to get some info about commands, "\q" or "quit" to end.');
end;


var
  evr: TEval;
  EP,i: integer;
  HR: THRTimer;
  ctime: double;
  ac: char8;
  print,script: boolean;
  s: mp_string;
  cmd: string[10];
  ss: string[2];
  radix: word;
  rc: char8;

begin

  StartTimer(HR);
  mp_init_eval(evr);

  mp_uppercase := true;
  script := false;
  ctime  := 0.0;
  ac := #0;
  rc := 'D';
  radix := 10;

  for i:=1 to paramcount do begin
    if (paramstr(i)='/s') or (paramstr(i)='/S') then script := true;
  end;

  writeln('Test of MPArith V', MP_VERSION, ' [mp_calc]   (c) W.Ehrhardt 2006-2010');
  if not script then HelpLine;
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

    cmd := copy(s,1,10);
    for i:=1 to length(cmd) do cmd[i] := upcase(cmd[i]);

    if (cmd='\Q') or (cmd='Q') or (cmd='QUIT') or (cmd='\@') then break
    else if cmd='HEX' then begin
      rc := 'H';
      radix := 16;
      if script then writeln(cmd);
      continue;
    end
    else if cmd='DEC' then begin
      rc := 'D';
      radix := 10;
      if script then writeln(cmd);
      continue;
    end
    else if cmd='BIN' then begin
      rc := 'B';
      radix := 2;
      if script then writeln(cmd);
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
        {Check if we have an assignment to x,y,z or just an expression}
        {starting with x,y,z like x mod 10. Analyse first non-blank char}
        {next while/if are OK because s has trailing #0}
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
        mp_calculate(@s[1],evr,EP);
        ctime := 1000.0*ReadSeconds(HR);
        if evr.Err=Err_MPERR_Eval then set_mp_error(MP_OKAY);
      end;
    end
    else if evr.Err>0 then continue;

    if evr.Err>0 then begin
      writeln('Error ', evr.Err,', ',mp_calc_errorstr(evr.Err),':  <',copy(s,EP+1,length(s)-EP-1), '>');
      if evr.Err=Err_Unknown_Function then HelpLine;
    end
    else if evr.Err<0 then writeln(#7'Error: ', evr.Err, ' [',mp_calc_errorstr(evr.Err),']')
    else begin
      if ac in ['X', 'Y', 'Z'] then begin
        case ac of
          'X':  mp_copy(evr.Res,evr.X);
          'Y':  mp_copy(evr.Res,evr.Y);
          'Z':  mp_copy(evr.Res,evr.Z);
        end;
        write(ac,' = ');
      end
      else write('Result = ');
        case mp_sign(evr.Res) of
          +1 : ss := '>0';
           0 : ss := '=0';
          else ss := '<0'
        end;
        if print then begin
        if mp_radix_size(evr.Res, radix) > $F000 then begin
          writeln('** to many chars in result **');
          write(' [',ss, ', ',mp_bitsize(evr.Res):6, ' bits,  chksum=$',HexLong(mp_checksum(evr.Res)),
                ',  time=', ctime:1:3, ' ms]');
        end;
        mp_output_radix(evr.Res, radix)
      end
      else begin
        write(' [',ss, ', ',mp_bitsize(evr.Res):6, ' bits,  chksum=$',HexLong(mp_checksum(evr.Res)),
              ',  time=', ctime:1:3, ' ms]');
      end;
    end;
    writeln;
  until false;
  mp_clear_eval(evr);
  {$ifdef MPC_Diagnostic}
     mp_dump_meminfo;
     mp_dump_diagctr;
  {$endif}
end.
