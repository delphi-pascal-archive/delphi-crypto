{******************************************************************************}
{*  RipeMD.test.int.inc                                                       *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
type
    PRipeMD128TestVector = ^TRipeMD128TestVector;
    TRipeMD128TestVector = packed record
        Plain : String;
        Hash  : String [32];
    end;

    PRipeMD160TestVector = ^TRipeMD160TestVector;
    TRipeMD160TestVector = packed record
        Plain : String;
        Hash  : String [40];
    end;

function TestRipeMD128 : Boolean;
function TestRipeMD160 : Boolean;

const
    RIPEMD128_TEST_VECTORS : packed array [0..1] of TRipeMD128TestVector = (
        ( Plain : '';
          Hash  : 'cdf26213a150dc3ecb610f18f6b38b46' ),

        ( Plain : 'The quick brown fox jumps over the lazy dog';
          Hash  : '3fa9b57f053c053fbe2735b2380db596' )
    );

    RIPEMD160_TEST_VECTORS : packed array [0..1] of TRipeMD160TestVector = (
        ( Plain : '';
          Hash  : '9c1185a5c5e9fc54612808977ee8f548b2258d31' ),

        ( Plain : 'The quick brown fox jumps over the lazy dog';
          Hash  : '37f332f68db77bd9d7edd4969571ad671cf9dd3b' )
    );
