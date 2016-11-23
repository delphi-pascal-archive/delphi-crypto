{******************************************************************************}
{*  BlowFish.test.int.inc                                                     *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
type
    PBlowFishTestVector = ^TBlowFishTestVector;
    TBlowFishTestVector = packed record
        Key    : String;
        IV     : String [32];
        Plain  : String [16];
        Cipher : String [16];
    end;

{$IFDEF ECB}
function TestEncryptBlowFishECB (const aKey: String;
                                 const aValue: String) : String;
function TestDecryptBlowFishECB (const aKey: String;
                                 const aValue: String) : String;
function TestBlowFishECB : Boolean;

const
    BLOWFISH_ECB_TEST_VECTORS : packed array [0..3] of TBlowFishTestVector = (
        { 128-битный ключ }
        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '';
          Plain  : '6bc1bee22e409f96';
          Cipher : '05ac654c244c5865' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '';
          Plain  : 'ae2d8a571e03ac9c';
          Cipher : '1dfa4a3817b13ed3' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '';
          Plain  : '30c81c46a35ce411';
          Cipher : '4b4634dda3dfb3c9' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '';
          Plain  : 'f69f2445df4f9b17';
          Cipher : '0ea0101b3060b116' )
    );
{$ENDIF ECB}
{$IFDEF CBC}
function TestEncryptBlowFishCBC (const aKey: String;
                                 const anIV: String;
                                 const aValue: String) : String;
function TestDecryptBlowFishCBC (const aKey: String;
                                 const anIV: String;
                                 const aValue: String) : String;
function TestBlowFishCBC : Boolean;

const
    BLOWFISH_CBC_TEST_VECTORS : packed array [0..3] of TBlowFishTestVector = (
        { 128-битный ключ }
        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '0001020304050607';
          Plain  : '6bc1bee22e409f96';
          Cipher : 'ee94800dec0bf893' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '7649abac8119b246';
          Plain  : 'ae2d8a571e03ac9c';
          Cipher : '76438adab5a92187' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '5086cb9b507219ee';
          Plain  : '30c81c46a35ce411';
          Cipher : '4b4fb1566521b614' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '73bed6b8e3c1743b';
          Plain  : 'f69f2445df4f9b17';
          Cipher : 'eb0e267e50613f05' )
    );
{$ENDIF CBC}
{$IFDEF CFB}
function TestEncryptBlowFishCFB (const aKey: String;
                                 const anIV: String;
                                 const aValue: String) : String;
function TestDecryptBlowFishCFB (const aKey: String;
                                 const anIV: String;
                                 const aValue: String) : String;
function TestBlowFishCFB : Boolean;

const
    BLOWFISH_CFB_TEST_VECTORS : packed array [0..3] of TBlowFishTestVector = (
        { 128-битный ключ }
        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '0001020304050607';
          Plain  : '6bc1bee22e409f96';
          Cipher : '371f2e29411439c8' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '7649abac8119b246';
          Plain  : 'ae2d8a571e03ac9c';
          Cipher : 'f4e9ce3eb546c9f0' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '5086cb9b507219ee';
          Plain  : '30c81c46a35ce411';
          Cipher : '15b26bc48529b01e' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '73bed6b8e3c1743b';
          Plain  : 'f69f2445df4f9b17';
          Cipher : '53de7860412e67a7' )
    );
{$ENDIF CFB}
{$IFDEF OFB}
function TestEncryptBlowFishOFB (const aKey: String;
                                 const anIV: String;
                                 const aValue: String) : String;
function TestDecryptBlowFishOFB (const aKey: String;
                                 const anIV: String;
                                 const aValue: String) : String;
function TestBlowFishOFB : Boolean;

const
    BLOWFISH_OFB_TEST_VECTORS : packed array [0..3] of TBlowFishTestVector = (
        { 128-битный ключ }
        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '0001020304050607';
          Plain  : '6bc1bee22e409f96';
          Cipher : '371f2e29411439c8' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '7649abac8119b246';
          Plain  : 'ae2d8a571e03ac9c';
          Cipher : 'f4e9ce3eb546c9f0' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '5086cb9b507219ee';
          Plain  : '30c81c46a35ce411';
          Cipher : '15b26bc48529b01e' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '73bed6b8e3c1743b';
          Plain  : 'f69f2445df4f9b17';
          Cipher : '53de7860412e67a7' )
    );
{$ENDIF OFB}
