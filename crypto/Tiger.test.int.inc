{******************************************************************************}
{*  Tiger.test.int.inc                                                        *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
type
    PTiger128TestVector = ^TTiger128TestVector;
    TTiger128TestVector = packed record
        Plain : String;
        Hash  : String [32];
    end;

    PTiger160TestVector = ^TTiger160TestVector;
    TTiger160TestVector = packed record
        Plain : String;
        Hash  : String [40];
    end;

    PTiger192TestVector = ^TTiger192TestVector;
    TTiger192TestVector = packed record
        Plain : String;
        Hash  : String [48];
    end;

function TestTiger128 : Boolean;
function TestTiger160 : Boolean;
function TestTiger192 : Boolean;

const
    TIGER128_TEST_VECTORS : packed array [0..1] of TTiger128TestVector = (
        ( Plain : '';
          Hash  : '24f0130c63ac933216166e76b1bb925f' ),

        ( Plain : 'The quick brown fox jumps over the lazy dog';
          Hash  : 'f044e6721ea4126d624cb4f7e2f0b617' )
    );

    TIGER160_TEST_VECTORS : packed array [0..1] of TTiger160TestVector = (
        ( Plain : '';
          Hash  : '24f0130c63ac933216166e76b1bb925ff373de2d' ),

        ( Plain : 'The quick brown fox jumps over the lazy dog';
          Hash  : 'f044e6721ea4126d624cb4f7e2f0b61775b0c5d2' )
    );

    TIGER192_TEST_VECTORS : packed array [0..1] of TTiger192TestVector = (
        ( Plain : '';
          Hash  : '24f0130c63ac933216166e76b1bb925ff373de2d49584e7a' ),

        ( Plain : 'The quick brown fox jumps over the lazy dog';
          Hash  : 'f044e6721ea4126d624cb4f7e2f0b61775b0c5d2d56df085' )
    );
