{******************************************************************************}
{*  TKey.int.inc                                                              *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
type
    PKey128 = ^TKey128;
    TKey128 = array [0..15] of Byte;

    PKey192 = ^TKey192;
    TKey192 = array [0..23] of Byte;

    PKey256 = ^TKey256;
    TKey256 = array [0..31] of Byte;

    PKey512 = ^TKey512;
    TKey512 = array [0..63] of Byte;

    PKey    = ^TKey;
    TKey    = array [0..0] of Byte;

const
    NULL_128_KEY : TKey128 = (
        $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00
    );

    NULL_192_KEY : TKey192 = (
        $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00
    );

    NULL_256_KEY : TKey256 = (
        $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00
    );

    NULL_512_KEY : TKey512 = (
        $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $00, $00, $00, $00, $00, $00
    );

procedure NULL_KEY (var aKey: PKey;
                    const aSize: WORD);

function Key128 (anArgs: array of const) : TKey128;
function HexToKey128 (const aValue: Hex) : TKey128;
function Key128ToHex (const aValue: TKey128) : Hex;

function Key192 (anArgs: array of const) : TKey192;
function HexToKey192 (const aValue: Hex) : TKey192;
function Key192ToHex (const aValue: TKey192) : Hex;

function Key256 (anArgs: array of const) : TKey256;
function HexToKey256 (const aValue: Hex) : TKey256;
function Key256ToHex (const aValue: TKey256) : Hex;

function Key512 (anArgs: array of const) : TKey512;
function HexToKey512 (const aValue: Hex) : TKey512;
function Key512ToHex (const aValue: TKey512) : Hex;

procedure Key (anArgs: array of const;
               out aResult: PKey);
procedure HexToKey (const aValue: Hex;
                    out aResult: PKey);
function KeyToHex (const aValue: PKey;
                   const aSize: WORD) : Hex;
