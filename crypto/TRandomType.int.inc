{******************************************************************************}
{*  TRandomType.int.inc                                                       *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ типы генератора псевдо-случайных чисел }
type
    PRandomType = ^TRandomType;
    TRandomType = (
        rtUnknown,
        rtSystem,
        rtTaus88,
        rtTaus113,
        rtKiss123,
        rtTT800,
        rtXOR4096,
        rtMT19937,
        rtAESR,
        rtSalsaR,
        rtISAAC
    );

const
    RT_UNKNOWN = 0;
    RT_SYSTEM  = 1;
    RT_TAUS88  = 2;
    RT_TAUS113 = 3;
    RT_KISS123 = 4;
    RT_TT800   = 5;
    RT_XOR4096 = 6;
    RT_MT19937 = 7;
    RT_AESR    = 8;
    RT_SALSAR  = 9;
    RT_ISAAC   = 10;

    RANDOM_TYPE_STRING : array [RT_UNKNOWN..RT_ISAAC] of String =
    ( '',
      'System',
      'Taus88',
      'Taus113',
      'Kiss123',
      'TT800',
      'XOR4096',
      'MT19937',
      'AESR',
      'SalsaR',
      'ISAAC' );

function StrToRandomType (const aValue: String) : TRandomType;
function RandomTypeToStr (const aValue: TRandomType) : String;

function IntToRandomType (const aValue: Integer) : TRandomType;
function RandomTypeToInt (const aValue: TRandomType) : Integer;

function toRandomType (const aValue: TVarRec) : TRandomType;
