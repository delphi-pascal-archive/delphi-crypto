{******************************************************************************}
{*  TRandomType.imp.inc                                                       *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ TRandomType }
function IntToRandomType (const aValue: Integer) : TRandomType;
begin
    Result := rtUnknown;
    case aValue of
        RT_SYSTEM   : Result := rtSystem;
        RT_TAUS88   : Result := rtTaus88;
        RT_TAUS113  : Result := rtTaus113;
        RT_KISS123  : Result := rtKiss123;
        RT_TT800    : Result := rtTT800;
        RT_XOR4096  : Result := rtXOR4096;
        RT_MT19937  : Result := rtMT19937;
        RT_AESR     : Result := rtAESR;
        RT_SALSAR   : Result := rtSalsaR;
        RT_ISAAC    : Result := rtISAAC;
    end;
end;

function RandomTypeToInt (const aValue: TRandomType) : Integer;
begin
    Result := RT_UNKNOWN;
    case aValue of
        rtSystem   : Result := RT_SYSTEM;
        rtTaus88   : Result := RT_TAUS88;
        rtTaus113  : Result := RT_TAUS113;
        rtKiss123  : Result := RT_KISS123;
        rtTT800    : Result := RT_TT800;
        rtXOR4096  : Result := RT_XOR4096;
        rtMT19937  : Result := RT_MT19937;
        rtAESR     : Result := RT_AESR;
        rtSalsaR   : Result := RT_SALSAR;
        rtISAAC    : Result := RT_ISAAC;
    end;
end;

function StrToRandomType (const aValue: String) : TRandomType;
begin
    Result := rtUnknown;
    if      ( aValue = RANDOM_TYPE_STRING [RT_SYSTEM] ) then
        Result := rtSystem
    else if ( aValue = RANDOM_TYPE_STRING [RT_TAUS88] ) then
        Result := rtTaus88
    else if ( aValue = RANDOM_TYPE_STRING [RT_TAUS113] ) then
        Result := rtTaus113
    else if ( aValue = RANDOM_TYPE_STRING [RT_KISS123] ) then
        Result := rtKiss123
    else if ( aValue = RANDOM_TYPE_STRING [RT_TT800] ) then
        Result := rtTT800
    else if ( aValue = RANDOM_TYPE_STRING [RT_XOR4096] ) then
        Result := rtXOR4096
    else if ( aValue = RANDOM_TYPE_STRING [RT_MT19937] ) then
        Result := rtMT19937
    else if ( aValue = RANDOM_TYPE_STRING [RT_AESR] ) then
        Result := rtAESR
    else if ( aValue = RANDOM_TYPE_STRING [RT_SALSAR] ) then
        Result := rtSalsaR
    else if ( aValue = RANDOM_TYPE_STRING [RT_ISAAC] ) then
        Result := rtISAAC;
end;

function RandomTypeToStr (const aValue: TRandomType) : String;
begin
    Result := RANDOM_TYPE_STRING [ RandomTypeToInt (aValue) ];
end;

function toRandomType (const aValue: TVarRec) : TRandomType;
begin
    Result := rtUnknown;
    try
        Result := StrToRandomType ( toString (aValue) );
        if ( Result = rtUnknown ) then
            Result := IntToRandomType ( toInteger (aValue) );
    except
        Result := rtUnknown;
    end;
end;
