unit mp_rcalc;

{Parse and evaluate mp_float expressions}

interface

{$i STD.INC}
{$x+}
{$ifdef BIT16}
{$N+}
{$endif}


uses
  BTypes, mp_types;

{$i mp_conf.inc}


(*************************************************************************

 DESCRIPTION   :  Parse and evaluate mp_float expressions

 REQUIREMENTS  :  BP7, D1-D7/D9-D10/D12, FPC, VP

 EXTERNAL DATA :  (mp_types)

 MEMORY USAGE  :  heap

 DISPLAY MODE  :  ---

 REFERENCES    :  [19] T. Norvell: Parsing Expressions by Recursive Descent,
                       http://www.engr.mun.ca/~theo/Misc/exp_parsing.htm
                  [20] G. Toal's tutorial pages OperatorPrecedence.html,CompilersOneOhOne.html,
                       GrahamToalsCompilerDemo.html at http://www.gtoal.com/software/
                  [21] T.R. Nicely's parser.c in factor1.zip from http://www.trnicely.net
                       derived from GMP demo pexpr.c (see [15])

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.0.10   15.01.08  W.Ehrhardt  First working version derived from mp_calc
 0.0.11   16.01.08  we          Added a lot of functions
 0.0.12   16.01.08  we          parse floating point numbers
 0.0.13   16.01.08  we          remaining functions
 0.0.14   17.01.08  we          remaining pre-checks (except EXPT)
 0.0.15   16.01.08  we          special cases and pre-checks for EXPT
 0.0.16   19.01.08  we          Err_Overflow, pre-checks mul/div
 0.0.17   20.01.08  we          test() function, CE uses mpf_cell2
 0.0.18   26.01.08  we          log2, log10; fix _EXPT overflow check

 1.7.00   16.09.08  we          log(a,b); better _EXPT overflow check
 1.7.01   24.09.08  we          string replaced by mp_string

 1.9.00   02.12.08  we          Uses BTypes: char8, pchar8

 1.11.00  23.03.09  we          initialize e:=nil in Element,Expr,Factor,Func,Term
 1.11.01  30.03.09  we          removed redefinition of str255

 1.13.00  01.11.09  we          mpf_cot, mpf_csc, mpf_sec, mpf_coth, mpf_csch, mpf_sech
 1.13.01  02.11.09  we          mpf_arccot, mpf_arccotc, mpf_arccsc, mpf_arcsec, mpf_arccoth,mpf_arccsch,mpf_arcsech

 1.14.00  14.02.10  we          _LOG uses mpf_logbase

 1.15.00  13.05.10  we          mp_fract_sep, mp_arg_sep

 1.17.00  02.01.11  we          avoid D12 warning in mpf_calc_errorstr

**************************************************************************)


(*
*** to do ***
- init to res.bitprec?
- x^i for frac(i)=0, i<Maxlongint
*)


(*-------------------------------------------------------------------------
 (C) Copyright 2008-2011 Wolfgang Ehrhardt

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


{#Z+} {Turn off reference for help}

{Parse errors >0, eval errors < 0}
const
  Err_Missing_LeftBracket  =  1;   {Missing "("}
  Err_Missing_Comma        =  2;   {Missing argument separator}
  Err_Missing_RightBracket =  3;   {Missing ")"}
  Err_Unknown_Function     =  4;   {Unknown function}
  Err_Unknown_Element      =  5;   {Unknown element}
  Err_Trailing_Gargabe     =  6;   {Trailing garbage}
  Err_Invalid_Number       =  7;   {Invalid number}
  Err_Unknown_Operation    =  8;   {Unknown operation}

  Err_MPERR_Eval           = -1;   {MP_Err <> MP_OK}
  Err_Division_by_zero     = -2;   {Division by zero}
  Err_Invalid_argument     = -3;   {Invalid arg, e.g. sqrt(-2)}
  Err_X_not_init           = -4;   {Variable X not initialized}
  Err_Y_not_init           = -5;   {Variable Y not initialized}
  Err_Z_not_init           = -6;   {Variable Z not initialized}
  Err_Overflow             = -8;   {Overflow}
{#Z-}


type
  TFOperation = (_CONST, _CHS, _ABS, _ADD, _SUB, _MUL, _DIV, _EXPT, _SQRT,
                 _SQR, _RANDOM, _MIN, _MAX, _X, _Y, _Z, _ARCCOSH, _ARCCOSH1P,
                 _AGM, _ARCCOS, _ARCSIN, _ARCTAN, _ARCTAN2, _ARCSINH, _ARCTANH,
                 _CCELL1, _CCELL2, _COS, _COSH, _EXP, _EXPM1, _FRAC, _INT,
                 _LN, _LN1P, _LOG, _LOG2, _LOG10, _SIN, _SINH, _TAN, _TANH,
                 _COT, _CSC, _SEC, _COTH, _CSCH, _SECH,
                 _ARCCOT,_ARCCOTC,_ARCCSC,_ARCSEC,_ARCCOTH,_ARCCSCH,_ARCSECH,
                 _TEST);
                {implemented operators, functions, and variables}

type
  PFExpr  = ^TFExpr;                       {Expression node pointer}
  TFExpr  = record                         {binary tree node}
              op:  TFOperation;            {operation/function/variable}
              nn:  byte;                   {number of nodes}
              case integer of
                0: (Value: mp_float);      {value if op=_CONST}
                1: (SNode: PFExpr);        {expr = (SNode op) or op(SNode)}
                2: (LNode, RNode: PFExpr;) {expr = LNode op RNode or op(LNode,RNode)}
            end;

  TFEval  = record                         {Evaluation record}
              X  : mp_float;               {Variable X}
              Y  : mp_float;               {Variable Y}
              Z  : mp_float;               {Variable Z}
              Res: mp_float;               {Evaluation result}
              Err: integer;                {Eval error code}
            end;


function  mpf_parse(psz: pchar8; var e: PFExpr; var Err: integer): pchar8;
  {-parse string psz into expression tree e, if OK Err=0 and result^=#0,}
  { else Err=Err_xx and result points to error position}

procedure mpf_eval(e: PFExpr; var evr: TFEval);
  {-evaluate expression tree e, result in evr}

procedure mpf_clear_expr(var e: PFExpr);
  {-release memory used by e}

procedure mpf_calculate(psz: pchar8; var evr: TFEval; var EPos: integer);
  {-parse and evaluate string psz}

function  mpf_calc_errorstr(Err: integer): mp_string;
  {-translate known error codes}

procedure mpf_init_eval(var evr: TFEval);
  {-initialize the mp_floats of evr}

procedure mpf_clear_eval(var evr: TFEval);
  {-clear the mp_floats of evr}


implementation


uses
  mp_base, mp_numth, mp_real;


(* Approx. grammar for expression parser:

  <Expr>    ::=   <Term> '+' <Term>
                | <Term> '-' <Term>
                | '+'<Term>
                | '-'<Term>;
  <Term>    ::=   <Factor> '*' <Factor>
                | <Factor> '/' <Factor>
                | <Factor>;
  <Factor>  ::=   <Element> '^' <Factor>
                | <Element>;
  <Element> ::= <Func> | <Var> | <number> | 'pi' | '(' <Expr> ')';
  <Func>    ::= <Ident> '(' <Arglist> ')';
  <Var>     ::= 'X'..'Z' | 'x'..'z';
  <Arglist  ::= <Expr> | <Expr> ',' <Expr>;
  <Ident>   ::= <alpha> {<alpha> | <digit>};
  <intnum>  ::= <digit> { <digit> };
  <expo>    ::=   'e' ['+' | '-'] <intnum>
                | 'E' ['+' | '-'] <intnum>
  <number>  ::= ['+' | '-'] [<intnum>] ['.' [<intnum>] <expo>];
  <digit>   ::= '0'..'9';
  <alpha>   ::= 'A'..'Z'| 'a'..'z';
*)



type
  str10  = string[10];
  TFunc  = record
               op: TFOperation;
             arg2: boolean;
             name: str10;
           end;
type
  TOpVar = _X.._Z;

const
  MaxFun = 46;

const
  FuncTab : array[1..MaxFun] of TFunc = (
             (op: _SQRT      ; arg2: false; name: 'SQRT'),
             (op: _SQR       ; arg2: false; name: 'SQR'),
             (op: _ABS       ; arg2: false; name: 'ABS'),
             (op: _RANDOM    ; arg2: false; name: 'RANDOM'),
             (op: _MIN       ; arg2: true ; name: 'MIN'),
             (op: _MAX       ; arg2: true ; name: 'MAX'),
             (op: _ARCCOSH   ; arg2: false; name: 'ARCCOSH'),
             (op: _ARCCOSH1P ; arg2: false; name: 'ARCCOSH1P'),
             (op: _AGM       ; arg2: true ; name: 'AGM'),
             (op: _ARCCOS    ; arg2: false; name: 'ARCCOS'),
             (op: _ARCSIN    ; arg2: false; name: 'ARCSIN'),
             (op: _ARCTAN2   ; arg2: true ; name: 'ARCTAN2'),
             (op: _ARCTAN    ; arg2: false; name: 'ARCTAN'),
             (op: _ARCSINH   ; arg2: false; name: 'ARCSINH'),
             (op: _ARCTANH   ; arg2: false; name: 'ARCTANH'),
             (op: _CCELL1    ; arg2: false; name: 'CK'),
             (op: _CCELL2    ; arg2: false; name: 'CE'),
             (op: _COS       ; arg2: false; name: 'COS'),
             (op: _COSH      ; arg2: false; name: 'COSH'),
             (op: _EXP       ; arg2: false; name: 'EXP'),
             (op: _EXPM1     ; arg2: false; name: 'EXPM1'),
             (op: _FRAC      ; arg2: false; name: 'FRAC'),
             (op: _INT       ; arg2: false; name: 'INT'),
             (op: _LN        ; arg2: false; name: 'LN'),
             (op: _LN1P      ; arg2: false; name: 'LN1P'),
             (op: _LOG       ; arg2: true ; name: 'LOG'),
             (op: _LOG2      ; arg2: false; name: 'LOG2'),
             (op: _LOG10     ; arg2: false; name: 'LOG10'),
             (op: _SIN       ; arg2: false; name: 'SIN'),
             (op: _SINH      ; arg2: false; name: 'SINH'),
             (op: _TAN       ; arg2: false; name: 'TAN'),
             (op: _TANH      ; arg2: false; name: 'TANH'),
             (op: _COT       ; arg2: false; name: 'COT'),
             (op: _CSC       ; arg2: false; name: 'CSC'),
             (op: _SEC       ; arg2: false; name: 'SEC'),
             (op: _COTH      ; arg2: false; name: 'COTH'),
             (op: _CSCH      ; arg2: false; name: 'CSCH'),
             (op: _SECH      ; arg2: false; name: 'SECH'),
             (op: _ARCCOT    ; arg2: false; name: 'ARCCOT'),
             (op: _ARCCOTC   ; arg2: false; name: 'ARCCOTC'),
             (op: _ARCCSC    ; arg2: false; name: 'ARCCSC'),
             (op: _ARCSEC    ; arg2: false; name: 'ARCSEC'),
             (op: _ARCCOTH   ; arg2: false; name: 'ARCCOTH'),
             (op: _ARCCSCH   ; arg2: false; name: 'ARCCSCH'),
             (op: _ARCSECH   ; arg2: false; name: 'ARCSECH'),
             (op: _TEST      ; arg2: false; name: 'TEST')  {used for tests, development etc}
           );


{---------------------------------------------------------------------------}
function SkipWhite(psz: pchar8): pchar8;
  {-skip white space}
begin
  while psz^ in [' ',#13,#10,#9] do inc(psz);
  SkipWhite := psz;
end;


{---------------------------------------------------------------------------}
procedure mkNode(var r: PFExpr; op: TFOperation; nn: byte; e1, e2: PFExpr);
  {-Make a new expression node for (e1 op e2) or (e1 op)}
begin
  r := mp_alloc(sizeof(TFExpr));
  r^.nn := nn;
  r^.op := op;
  r^.LNode := e1;
  r^.RNode := e2;
end;


{---------------------------------------------------------------------------}
procedure mkNode0c(var r: PFExpr);
  {-Make a new node for a constant}
begin
  {alloc Value node initialize mp_float}
  r := mp_alloc(sizeof(TFExpr));
  r^.op := _CONST;
  r^.nn := 0;
  mpf_init(r^.Value);
end;


{---------------------------------------------------------------------------}
procedure mkNode0v(var r: PFExpr; const v: TOpVar);
  {-Make a new node for a variable v = _X, _Y, or _Z}
begin
  {alloc Value node initialize mp_float}
  r := mp_alloc(sizeof(TFExpr));
  r^.op := v;
  r^.nn := 0;
end;


{---------------------------------------------------------------------------}
procedure mkNode1(var r: PFExpr; op: TFOperation; e: PFExpr);
  {-Make a new expression node for r := e op}
begin
  mkNode(r,op,1,e,nil);
end;


{---------------------------------------------------------------------------}
procedure mkNode2(var r: PFExpr; op: TFOperation; e1, e2: PFExpr);
  {-Make a new expression node for r := e1 op e2}
begin
  mkNode(r,op,2,e1,e2);
end;


{---------------------------------------------------------------------------}
function GetIdent(psz: pchar8): mp_string;
  {-gather next identifier, break if not in [A-Z, a-z, 0-9], result is uppercase}
  { first character in must be in [A-Z, a-z]}
var
  s: mp_string;
begin
  s := '';
  if psz^ in ['A'..'Z', 'a'..'z'] then begin
    while psz^ in ['A'..'Z', 'a'..'z', '0'..'9'] do begin
      s := s+upcase(psz^);
      inc(psz);
    end;
  end;
  GetIdent := s;
end;


{---------------------------------------------------------------------------}
function GetFuncIndex(const s: mp_string): integer;
  {-test if s is a known function name, if yes return index else 0}
var
  i: integer;
begin
  for i:=1 to MaxFun do begin
    if FuncTab[i].name=s then begin
      GetFuncIndex := i;
      exit;
    end;
  end;
  GetFuncIndex := 0;
end;


{---------------------------------------------------------------------------}
function Expr(psz: pchar8; var e: PFExpr; var Err: integer): pchar8; forward;
  {-parse string psz into expression tree}
{---------------------------------------------------------------------------}


{---------------------------------------------------------------------------}
function Func(psz: pchar8; idx: integer; var e: PFExpr; var Err: integer): pchar8;
  {-build function expression, psz points between function name and "("}
var
  e1,e2: PFExpr;
const
  na: array[boolean] of byte = (1,2);

  procedure clear;
    {-clear local PFExpr if error}
  begin
    if e1<>nil then mpf_clear_expr(e1);
    if e2<>nil then mpf_clear_expr(e2);
  end;

begin
  e1 := nil;
  e2 := nil;
  e  := nil;
  Func := psz;
  if Err<>0 then exit;
  psz := SkipWhite(psz);
  if psz^ <> '(' then begin
    Func := psz;
    Err := Err_Missing_LeftBracket;
    exit;
  end;
  {get first argument}
  psz := Expr(psz+1,e1, Err);
  Func := psz;
  if Err<>0 then begin
    clear;
    exit;
  end;
  psz := SkipWhite(psz);
  if FuncTab[idx].arg2 then begin
    {if two arguments search for ","}
    if psz^ <> mp_arg_sep{','} then begin
      Func := psz;
      Err := Err_Missing_Comma;
      clear;
      exit;
    end;
    {evaluate second argument}
    psz := Expr(psz+1,e2,Err);
    Func := psz;
    if Err<>0 then begin
      clear;
      exit;
    end;
    psz := SkipWhite(psz);
  end
  else e2:=nil;
  {search for closing ")"}
  if psz^ <> ')' then begin
    Func := psz;
    Err := Err_Missing_RightBracket;
    clear;
    exit;
  end;
  inc(psz);
  with FuncTab[idx] do mkNode(e, op, na[arg2], e1, e2);
  Func := psz;
end;


{---------------------------------------------------------------------------}
function Element(psz: pchar8; var e: PFExpr; var Err: integer): pchar8;
  {-parse an Element}
var
  res: PFExpr;
  s,sc: pchar8;
  lsc: word;
  i: integer;
  s0: char8;
  neg: boolean;
  id: str255;
begin
  Element := psz;
  e := nil;
  if Err<>0 then exit;
  psz := SkipWhite(psz);
  s0 := psz^;
  if s0 in ['A'..'Z', 'a'..'z'] then begin
    id := GetIdent(psz);
    i := GetFuncIndex(id);
    if i=0 then begin
      if id='X' then begin mkNode0v(e, _X); inc(psz); end
      else if id='Y' then begin mkNode0v(e, _Y); inc(psz); end
      else if id='Z' then begin mkNode0v(e, _Z); inc(psz); end
      else if id='PI' then begin
        mkNode0c(res);
        mpf_set_pi(res^.Value);
        inc(psz,2);
        e := res;
      end
      else begin
        Err := Err_Unknown_Function;
        Element := psz;
        exit;
      end;
    end
    else begin
      inc(psz, length(id));
      psz := Func(psz,i,e, Err);
    end;
  end
  else if s0='(' then begin
    psz := Expr(psz+1,e,Err);
    if Err<>0 then exit;
    psz := SkipWhite(psz);
    if psz^<>')' then begin
      Err := Err_Missing_RightBracket;
      Element := psz;
      exit;
    end;
    inc(psz);
  end
  else if s0 in ['0'..'9','+','-',mp_fract_sep{'.'}] then begin
    {get sign}
    neg := false;
    if psz^ in ['+','-'] then begin
      if psz^='-' then neg := true;
      inc(psz);
    end;

    {count decimal characters}
    s := psz;
    {integer part}
    while psz^ in ['0'..'9'] do inc(psz);

    if psz^=mp_fract_sep{'.'} then begin
      inc(psz);
      {fractional part}
      while psz^ in ['0'..'9'] do inc(psz);
    end;

    if (upcase(psz^)='E') and (s<>psz) then begin
       {exponent part}
       inc(psz);
       if psz^ in ['+','-'] then inc(psz);
       while psz^ in ['0'..'9'] do inc(psz);
    end;

    {alloc and move digit string to temp storage}
    if s=psz then begin
      {empty digit string}
      Err := Err_Invalid_Number;
      Element := psz;
      exit;
    end;

    lsc := psz-s+1;
    sc := mp_getmem(lsc);
    move(s^,sc^,lsc-1);
    sc[psz-s] := #0;
    {Make a constant node}
    mkNode0c(res);
    {convert digit string to mp_float}
    mpf_read_decimal(res^.Value,sc);
    {apply sign}
    if neg then mpf_chs(res^.Value, res^.Value);
    e := res;
    mp_freemem(pointer(sc),lsc);
  end
  else begin
    Err := Err_Unknown_Element;
    Element := psz;
    exit;
  end;
  Element := psz;
end;


{---------------------------------------------------------------------------}
function Factor(psz: pchar8; var e: PFExpr; var Err: integer): pchar8;
  {-parse a Factor}
var
  t: PFExpr;
begin
  e := nil;
  if Err<>0 then begin
    Factor := psz;
    exit;
  end;
  psz := Element(psz,e,Err);
  if Err<>0 then begin
    Factor := psz;
    exit;
  end;
  {Look for optional power part of Factor}
  psz := SkipWhite(psz);
  if psz^='^' then begin
    inc(psz);
    t := nil;
    psz := Factor(psz, t, Err);
    if Err=0 then mkNode2(e, _EXPT, e, t)
    else if t<>nil then mpf_clear_expr(t);
  end;
  Factor := psz;
end;


{---------------------------------------------------------------------------}
function Term(psz: pchar8; var e: PFExpr; var Err: integer): pchar8;
  {-parse a Term}
var
  t: PFExpr;
begin
  e := nil;
  if Err<>0 then begin
    Term := psz;
    exit;
  end;
  t := nil;
  psz := Factor(psz,e, Err);
  while Err=0 do begin
    psz := SkipWhite(psz);
    case upcase(psz[0]) of
      '*' : begin
              psz := Factor(psz+1, t, Err);
              if Err=0 then mkNode2(e, _MUL, e, t);
            end;
      '/':  begin
              psz := Factor(psz+1, t, Err);
              if Err=0 then mkNode2(e, _DIV, e, t);
            end;
     else   break;
    end;
  end;
  if (Err<>0) and (t<>nil) then mpf_clear_expr(t);
  Term := psz;
end;


{---------------------------------------------------------------------------}
function Expr(psz: pchar8; var e: PFExpr; var Err: integer): pchar8;
  {-parse an Expr}
var
  t: PFExpr;
  c: char8;
begin
  e := nil;
  if Err<>0 then begin
    Expr := psz;
    exit;
  end;
  psz := SkipWhite(psz);
  c  := psz^;
  {Unary prefix operators}
  if c='+' then begin
    {just skip the +}
    psz := Term(psz+1, e, Err)
  end
  else if c='-' then begin
    psz := Term(psz+1, e, Err);
    if Err<>0 then begin
      Expr := psz;
      exit;
    end;
    mkNode1(e, _CHS, e);
  end
  else psz := Term(psz, e, Err);

  t := nil;
  while Err=0 do begin
    psz := SkipWhite(psz);
    case psz^ of
      '+': begin
             psz := Term(psz+1, t, Err);
             if Err=0 then mkNode2(e, _ADD, e, t);
           end;
      '-': begin
             psz := Term(psz+1, t, Err);
             if Err=0 then mkNode2(e, _SUB, e, t);
           end;
      else break;
    end; {case}
  end;
  if (Err<>0) and (t<>nil) then mpf_clear_expr(t);
  Expr := psz;
end;


{---------------------------------------------------------------------------}
function mpf_parse(psz: pchar8; var e: PFExpr; var Err: integer): pchar8;
  {-parse string psz into expression tree e, Err=0 and psz^=#0 if OK,}
  { else Err=Err_xx and result points to error postions}
begin
  Err := 0;
  psz := Expr(psz, e, Err);
  if (Err=0) and (psz^<>#0) then Err := Err_Trailing_Gargabe;
  mpf_parse := psz;
end;



{---------------------------------------------------------------------------}
procedure eval(e: PFExpr; var r: mp_float; var evr: TFEval);
  {-(internal) evaluate expression tree e, result in r}
var
  v1,v2: mp_float;
  l1,l2: longint;
  eodd,einv: boolean;
begin
  if evr.Err<>0 then exit;

  if e^.nn=0 then begin
    case e^.op of
      _CONST: begin
                mpf_copy(e^.Value,r);
                if MP_Error<>0 then evr.Err := Err_MPERR_Eval;
              end;
          _X: begin
                if mpf_not_init(evr.X) then evr.Err := Err_X_not_init
                else mpf_copy(evr.X,r);
              end;
          _Y: begin
                if mpf_not_init(evr.Y) then evr.Err := Err_Y_not_init
                else mpf_copy(evr.Y,r);
              end;
          _Z: begin
                if mpf_not_init(evr.Z) then evr.Err := Err_Z_not_init
                else mpf_copy(evr.Z,r);
              end;
         else evr.Err := Err_Unknown_Operation;
    end;
    exit;
  end;

  {always initialize two mp_floats although some ops need only 0 or 1}
  mpf_initp2(v1,v2,mpf_get_default_prec);
  if MP_Error<>MP_OKAY then begin
    evr.Err := Err_MPERR_Eval;
    exit;
  end;

  if e^.nn=1 then begin
    eval(e^.LNode, v1, evr);
    if evr.Err=0 then begin
      l1 := s_mpf_ldx(v1);
      case e^.op of
            _CHS:  mpf_chs(v1,r);
            _ABS:  mpf_abs(v1,r);
            _SQR:  begin
                     if l1<MaxLongint then mpf_sqr(v1,r)
                     else evr.Err := Err_Overflow;
                   end;
           _SQRT:  begin
                     if s_mpf_is_neg(v1) then evr.Err := Err_Invalid_argument
                     else mpf_sqrt(v1,r)
                   end;

         _RANDOM:  begin
                     {random in [0..v1)}
                     mpf_random(r);
                     mpf_mul(r,v1,r);
                   end;

        _ARCCOSH:  begin
                     if s_mpf_is_neg(v1) or (l1 <= 0) then evr.Err := Err_Invalid_argument
                     else mpf_arccosh(v1,r);
                   end;

      _ARCCOSH1P:  begin
                     if s_mpf_is_neg(v1) then evr.Err := Err_Invalid_argument
                     else mpf_arccosh1p(v1,r);
                   end;

         _ARCCOS:  begin
                     {|v1| <= 1}
                     if (l1<=0) or mpf_is1a(v1) then mpf_arccos(v1,r)
                     else evr.Err := Err_Invalid_argument;
                   end;

         _ARCSIN:  begin
                     {|v1| <= 1}
                     if (l1<=0) or mpf_is1a(v1) then mpf_arcsin(v1,r)
                     else evr.Err := Err_Invalid_argument;
                   end;

         _ARCTAN:  begin
                     mpf_arctan(v1,r);
                   end;

        _ARCSINH:  begin
                     mpf_arcsinh(v1,r);
                   end;

        _ARCTANH:  begin
                     {|v1| < 1}
                     if l1<=0 then mpf_arctanh(v1,r)
                     else evr.Err := Err_Invalid_argument;
                   end;

         _CCELL1:  begin
                     {v1>0}
                     if s_mpf_is_le0(v1) then evr.Err := Err_Invalid_argument
                     else mpf_ccell1(v1,r);
                   end;

         _CCELL2:  begin
                     {v1>0}
                     if s_mpf_is_le0(v1) then evr.Err := Err_Invalid_argument
                     else mpf_ccell2(v1,r);
                   end;

            _COS:  begin
                     mpf_cos(v1,r);
                   end;

           _COSH:  begin
                     if l1<=30 then mpf_cosh(v1,r)
                     else evr.Err := Err_Overflow;
                   end;

            _EXP:  begin
                     if l1<=30 then mpf_exp(v1,r)
                     else evr.Err := Err_Overflow;
                   end;

          _EXPM1:  begin
                     if l1<=30 then mpf_expm1(v1,r)
                     else evr.Err := Err_Overflow;
                   end;

           _FRAC:  begin
                     mpf_frac(v1,r);
                   end;

            _INT:  begin
                     mpf_int(v1,r);
                   end;

             _LN:  begin
                     if s_mpf_is_le0(v1) then evr.Err := Err_Invalid_argument
                     else mpf_ln(v1,r)
                   end;

           _LOG2:  begin
                     if s_mpf_is_le0(v1) then evr.Err := Err_Invalid_argument
                     else mpf_log2(v1,r)
                   end;

          _LOG10:  begin
                     if s_mpf_is_le0(v1) then evr.Err := Err_Invalid_argument
                     else mpf_log10(v1,r)
                   end;

           _LN1P:  begin
                     if s_mpf_is_neg(v1) and (l1 > 0) then evr.Err := Err_Invalid_argument
                     else mpf_ln1p(v1,r)
                   end;

            _SIN:  begin
                     mpf_sin(v1,r);
                   end;

           _SINH:  begin
                     if l1<=30 then mpf_sinh(v1,r)
                     else evr.Err := Err_Overflow;
                   end;

            _TAN:  begin
                     mpf_tan(v1,r);
                   end;

           _TANH:  begin
                     mpf_tanh(v1,r);
                   end;

            _COT:  begin
                     {indirect calc to avoid div by 0}
                     mpf_tan(v1,v2);
                     if s_mpf_is0(v2) then evr.Err := Err_Invalid_argument
                     else mpf_inv(v2,r);
                   end;

            _CSC:  begin
                     {indirect calc to avoid div by 0}
                     mpf_sin(v1,v2);
                     if s_mpf_is0(v2) then evr.Err := Err_Invalid_argument
                     else mpf_inv(v2,r);
                   end;

            _SEC:  begin
                     {indirect calc to avoid div by 0}
                     mpf_cos(v1,v2);
                     if s_mpf_is0(v2) then evr.Err := Err_Invalid_argument
                     else mpf_inv(v2,r);
                   end;

           _COTH:  begin
                     if l1>30 then mpf_set0(r)
                     else if s_mpf_is0(v1) then evr.Err := Err_Invalid_argument
                     else mpf_coth(v1,r);
                   end;

           _CSCH:  begin
                     if l1>30 then mpf_set0(r)
                     else if s_mpf_is0(v1) then evr.Err := Err_Invalid_argument
                     else mpf_csch(v1,r);
                   end;

           _SECH:  begin
                     if l1>30 then mpf_set0(r)
                     else mpf_sech(v1,r);
                   end;

         _ARCCOT:  begin
                     mpf_arccot(v1,r);
                   end;

        _ARCCOTC:  begin
                     mpf_arccotc(v1,r);
                   end;

         _ARCCSC:  begin
                     if l1>0 then mpf_arccsc(v1,r)
                     else evr.Err := Err_Invalid_argument;
                   end;

         _ARCSEC:  begin
                     if l1>0 then mpf_arcsec(v1,r)
                     else evr.Err := Err_Invalid_argument;
                   end;

        _ARCCOTH:  begin
                     mpf_arccoth(v1,r);
                   end;

        _ARCCSCH:  begin
                     mpf_arccsch(v1,r);
                   end;

        _ARCSECH:  begin
                     {domain (0,1]}
                     if s_mpf_is_le0(v1) then evr.Err := Err_Invalid_argument
                     else if (l1<1) or mpf_is1(v1) then mpf_arcsech(v1,r)
                     else evr.Err := Err_Invalid_argument;
                   end;

           _TEST:  begin
                     mpf_sqrt1pm1(v1,r);
                     {used for tests, development etc}
                     (*
                     if (l1<30) then begin
                       mpf_trunc(v1,v2.mantissa);
                       if mp_is_longint(v2.mantissa,l2) then begin
                         mpf_numbpart(l2,v2.mantissa);
                         mpf_set_mpi(r,v2.mantissa);
                       end;
                     end;
                     *)
                   end;

            else   evr.Err := Err_Unknown_Operation
      end;
    end;
  end
  else begin
    eval(e^.LNode, v1, evr);
    if evr.Err=0 then begin
      eval(e^.RNode, v2, evr);
    end;
    if evr.Err=0 then begin
      l1 := s_mpf_ldx(v1);
      l2 := s_mpf_ldx(v2);
      case e^.op of
          _ADD:  mpf_add(v1,v2,r);
          _SUB:  mpf_sub(v1,v2,r);
          _MUL:  begin
                   if add32_ovr(l1,l2,l2) then begin
                     if l1<0 then mpf_set0(r)
                     else evr.Err := Err_Overflow;
                   end
                   else mpf_mul(v1,v2,r);
                 end;
          _DIV:  begin
                   if s_mpf_is0(v2) then evr.Err := Err_Division_by_zero
                   else begin
                     if add32_ovr(l1,-l2,l2) then begin
                       if l1<0 then mpf_set0(r)
                       else evr.Err := Err_Overflow;
                     end
                     else mpf_div(v1,v2,r);
                   end;
                 end;

          _LOG:  begin
                   if (s_mpf_is_le0(v1)) or mpf_is1(v1) or (s_mpf_is_le0(v2)) then evr.Err := Err_Invalid_argument
                   else mpf_logbase(v1,v2,r);
                 end;
          _MIN:  if mpf_is_lt(v1,v2) then mpf_exch(v1,r) else mpf_exch(v2,r);
          _MAX:  if mpf_is_gt(v1,v2) then mpf_exch(v1,r) else mpf_exch(v2,r);

         _EXPT:  begin
                   if s_mpf_is0(v2) or mpf_is1(v1) then mpf_set1(r)
                   else begin
                     {r=v1^v2, ldx(r) ~ ldx(v1)*v2}
                     if (l1>1) or ((l1=1) and (not mpf_is1a(v1))) then begin
                       {here |v1|>1: check |v2|*log2(|v1|) < 2^31, or}
                       {approx. log2(|v2|) + log2(log2(|v1|)) < 31, or}
                       {more approximative l2 + log2(l1) < 32}
                       if l2+ln(l1)/ln(2) > 32 then begin
                         if s_mpf_is_ge0(v2) then evr.Err := Err_Overflow;
                       end;
                     end;
                     if evr.Err=0 then begin
                       if s_mpf_is_le0(v1) then begin
                         {v1 < 0, check if v2 is a positive integer}
                         mpf_frac(v2,r);
                         if mpf_is0(r) then begin
                           if s_mpf_is_neg(v2) then begin
                             s_mpf_abs(v2);
                             einv := true;
                           end
                           else einv := false;
                           mpf_trunc(v2,r.mantissa);
                           eodd := mp_isodd(r.mantissa);
                           s_mpf_abs(v1);
                           mpf_expt(v1,v2,r);
                           if eodd then s_mpf_chs(r);
                           if einv then mpf_inv(r,r);
                         end
                         else evr.Err := Err_Invalid_argument
                       end
                       else begin
                         {todo: test if v1=2 or v1=10}
                         mpf_expt(v1,v2,r);
                       end;
                     end;
                   end;
                 end;

          _AGM:  begin
                   mpf_agm(v1,v2,r);
                 end;

      _ARCTAN2:  begin
                   mpf_arctan2(v2,v1,r);
                 end;

          else   evr.Err := Err_Unknown_Operation
      end;
    end;
  end;
  if (evr.ERR=0) and (MP_Error<>MP_OKAY) then evr.Err := Err_MPERR_Eval;
  mpf_clear2(v1,v2);
end;



{---------------------------------------------------------------------------}
procedure mpf_clear_expr(var e: PFExpr);
  {-release memory used by e and clear mp_float values}
begin
  if e<>nil then with e^ do begin
    case nn of
         0:  if op=_CONST then mpf_clear(Value);
         1:  if SNode<>nil then mpf_clear_expr(SNode);
       else  begin
               if e^.LNode<>nil then mpf_clear_expr(e^.LNode);
               if e^.RNode<>nil then mpf_clear_expr(e^.RNode);
             end;
    end;
    mp_freemem(pointer(e),sizeof(TFExpr));
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_eval(e: PFExpr; var evr: TFEval);
  {-evaluate expression tree e, result in evr}
begin
  with evr do begin
    Err := 0;
    eval(e,Res,evr);
  end;
end;



{---------------------------------------------------------------------------}
procedure mpf_calculate(psz: pchar8; var evr: TFEval; var EPos: integer);
  {-parse and evaluate string psz}
var
  e: PFExpr;
  pc: pchar8;
begin
  e := nil;
  pc := mpf_parse(psz,e,evr.Err);
  EPos := pc-psz;
  if evr.Err=0 then mpf_eval(e, evr);
  mpf_clear_expr(e);
end;


{---------------------------------------------------------------------------}
function mpf_calc_errorstr(Err: integer): mp_string;
  {-translate known error codes}
var
  s: string[20];
begin
  case Err of
    Err_Missing_LeftBracket  : mpf_calc_errorstr := 'Missing "("';
  {$ifdef Unicode}
    Err_Missing_Comma        : mpf_calc_errorstr := mp_string('Missing argument separator ("'+mp_arg_sep+'")');
  {$else}
    Err_Missing_Comma        : mpf_calc_errorstr := 'Missing argument separator ("'+mp_arg_sep+'")';
  {$endif}
    Err_Missing_RightBracket : mpf_calc_errorstr := 'Missing ")"';
    Err_Unknown_Function     : mpf_calc_errorstr := 'Unknown function';
    Err_Unknown_Element      : mpf_calc_errorstr := 'Unknown element';
    Err_Trailing_Gargabe     : mpf_calc_errorstr := 'Trailing garbage';
    Err_Invalid_Number       : mpf_calc_errorstr := 'Invalid number';
    Err_Unknown_Operation    : mpf_calc_errorstr := 'Unknown operation';
    Err_MPERR_Eval           : mpf_calc_errorstr := 'MP_Err <> MP_OK';
    Err_Division_by_zero     : mpf_calc_errorstr := 'Division by zero';
    Err_Invalid_argument     : mpf_calc_errorstr := 'Invalid argument(s)';
    Err_X_not_init           : mpf_calc_errorstr := 'Variable X not initialized';
    Err_Y_not_init           : mpf_calc_errorstr := 'Variable Y not initialized';
    Err_Z_not_init           : mpf_calc_errorstr := 'Variable Z not initialized';
    Err_Overflow             : mpf_calc_errorstr := 'Overflow';
    else begin
      str(Err,s);
      mpf_calc_errorstr := 'mpf_calc error '+s;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure mpf_init_eval(var evr: TFEval);
  {-initialize the mp_floats of evr}
begin
  with evr do mpf_initp4(X,Y,Z,Res,mpf_get_default_prec);
  evr.Err := 0;
end;


{---------------------------------------------------------------------------}
procedure mpf_clear_eval(var evr: TFEval);
  {-clear the mp_floats of evr}
begin
  with evr do mpf_clear4(X,Y,Z,Res);
end;


end.

