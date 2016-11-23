{******************************************************************************}
{*  MD.test.int.inc                                                           *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
type
    PMD4TestVector = ^TMD4TestVector;
    TMD4TestVector = packed record
        Plain : String;
        Hash  : String [32];
    end;

    PMD5TestVector = ^TMD5TestVector;
    TMD5TestVector = packed record
        Plain : String;
        Hash  : String [32];
    end;

function TestMD4 : Boolean;
function TestMD5 : Boolean;

const
    MD4_TEST_VECTORS : packed array [0..1] of TMD4TestVector = (
        ( Plain : '';
          Hash  : '31d6cfe0d16ae931b73c59d7e0c089c0' ),

        ( Plain : 'The quick brown fox jumps over the lazy dog';
          Hash  : '1bee69a46ba811185c194762abaeae90' )
    );

    MD5_TEST_VECTORS : packed array [0..1] of TMD5TestVector = (
        ( Plain : '';
          Hash  : 'd41d8cd98f00b204e9800998ecf8427e' ),

        ( Plain : 'The quick brown fox jumps over the lazy dog';
          Hash  : '9e107d9d372bb6826bd81d3542a419d6' )
    );
