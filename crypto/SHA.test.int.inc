{******************************************************************************}
{*  SHA.test.int.inc                                                          *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
type
    PSHA1TestVector = ^TSHA1TestVector;
    TSHA1TestVector = packed record
        Plain : String;
        Hash  : String [40];
    end;

    PSHA256TestVector = ^TSHA256TestVector;
    TSHA256TestVector = packed record
        Plain : String;
        Hash  : String [64];
    end;

    PSHA384TestVector = ^TSHA384TestVector;
    TSHA384TestVector = packed record
        Plain : String;
        Hash  : String [96];
    end;

    PSHA512TestVector = ^TSHA512TestVector;
    TSHA512TestVector = packed record
        Plain : String;
        Hash  : String [128];
    end;

function TestSHA1 : Boolean;
function TestSHA256 : Boolean;
function TestSHA384 : Boolean;
function TestSHA512 : Boolean;

const
    SHA1_TEST_VECTORS : packed array [0..1] of TSHA1TestVector = (
        ( Plain : '';
          Hash  : 'da39a3ee5e6b4b0d3255bfef95601890afd80709' ),

        ( Plain : 'The quick brown fox jumps over the lazy dog';
          Hash  : '2fd4e1c67a2d28fced849ee1bb76e7391b93eb12' )
    );

    SHA256_TEST_VECTORS : packed array [0..1] of TSHA256TestVector = (
        ( Plain : '';
          Hash  : 'e3b0c44298fc1c149afbf4c8996fb924'+
                  '27ae41e4649b934ca495991b7852b855' ),

        ( Plain : 'The quick brown fox jumps over the lazy dog';
          Hash  : 'd7a8fbb307d7809469ca9abcb0082e4f'+
                  '8d5651e46d3cdb762d02d0bf37c9e592' )
    );

    SHA384_TEST_VECTORS : packed array [0..1] of TSHA384TestVector = (
        ( Plain : '';
          Hash  : '38b060a751ac96384cd9327eb1b1e36a'+
                  '21fdb71114be07434c0cc7bf63f6e1da'+
                  '274edebfe76f65fbd51ad2f14898b95b' ),

        ( Plain : 'The quick brown fox jumps over the lazy dog';
          Hash  : 'ca737f1014a48f4c0b6dd43cb177b0af'+
                  'd9e5169367544c494011e3317dbf9a50'+
                  '9cb1e5dc1e85a941bbee3d7f2afbc9b1' )
    );

    SHA512_TEST_VECTORS : packed array [0..1] of TSHA512TestVector = (
        ( Plain : '';
          Hash  : 'cf83e1357eefb8bdf1542850d66d8007'+
                  'd620e4050b5715dc83f4a921d36ce9ce'+
                  '47d0d13c5d85f2b0ff8318d2877eec2f'+
                  '63b931bd47417a81a538327af927da3e' ),

        ( Plain : 'The quick brown fox jumps over the lazy dog';
          Hash  : '07e547d9586f6a73f73fbac0435ed769'+
                  '51218fb7d0c8d788a309d785436bbb64'+
                  '2e93a252a954f23912547d1e8a3b5ed6'+
                  'e1bfd7097821233fa0538f3db854fee6' )
    );
