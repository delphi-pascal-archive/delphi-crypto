{******************************************************************************}
{*  Anubis.test.int.inc                                                       *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
type
    PAnubisTestVector = ^TAnubisTestVector;
    TAnubisTestVector = packed record
        Key    : String;
        IV     : String [32];
        Plain  : String [32];
        Cipher : String [32];
    end;

{$IFDEF ECB}
function TestEncryptAnubisECB (const aKey: String;
                               const aValue: String) : String;
function TestDecryptAnubisECB (const aKey: String;
                               const aValue: String) : String;
function TestAnubisECB : Boolean;

const
    ANUBIS_ECB_TEST_VECTORS : packed array [0..11] of TAnubisTestVector = (
        { 128-битный ключ }
        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : 'dae1f24dbc637a4dc4e39650354c7df2' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '36d5a635b7a83299d3cb6b748988c272' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '670f1971dfd8d58c0a2004a0a624783e' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : '0c97881898f76fe0f75d3557fa9ed6d2' ),

        { 192-битный ключ }
        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : '7cf45061b28894eb4c5ad66a4cc58f52'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : 'e9f570f9861059173f4401c1508a6b55'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '4316850c529c73ef95860ee6226f67c8'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : '6c865a0145be3c45ffac4a26aaa85a42'                  ),

        { 256-битный ключ }
        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : '326bd0e044bafea0b4488c2b9c62f6b3'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '532e25ceed8db192ab764e4d7b32cbd1'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : 'c7d1dfe1f5390f2bd8aaa7d1006aa757'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : 'e0bcc40933f444907f6081d8e680d822'                                  )
    );
{$ENDIF ECB}
{$IFDEF CBC}
function TestEncryptAnubisCBC (const aKey: String;
                               const anIV: String;
                               const aValue: String) : String;
function TestDecryptAnubisCBC (const aKey: String;
                               const anIV: String;
                               const aValue: String) : String;
function TestAnubisCBC : Boolean;

const
    ANUBIS_CBC_TEST_VECTORS : packed array [0..11] of TAnubisTestVector = (
        { 128-битный ключ }
        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : 'ef24331781279b58003f4d29e3f0a583' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '7649abac8119b246cee98e9b12e9197d';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : 'c7f57c3f863156dcd1e0ae7676823d9a' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '5086cb9b507219ee95db113a917678b2';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : 'b33781269334d6d25067dadf9364dddf' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '73bed6b8e3c1743b7116e69e22229516';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : '42cc0f4b52c41f4b2ffef4d6914619bd' ),

        { 192-битный ключ }
        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : 'd22e4f9303798ade69c707e78a5002e2'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '4f021db243bc633d7178183a9fa071e8';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : 'b57b1077432f32ec8b309e6c320a82d9'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : 'b4d9ada9ad7dedf4e5e738763f69145a';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '30d38197a8ae08e21b1131c0a97503ff'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '571b242012fb7ae07fa9baac3df102e0';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : '41c21c0ff27ca1c8c34041ef03a4bb44'                  ),

        { 256-битный ключ }
        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : '8234870e067904ac02b5040ec1740569'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : 'f58c4c04d6e5f1ba779eabfb5f7bfbd6';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '119bb808f38bb29b3d8e2b8c7813696a'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '9cfc4e967edb808d679f777bc6702c7d';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '538293eebdcee6d1aedd30c4fadd804e'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '39f23369a9d9bacfa530e26304231461';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : '374ba901335fd6151fc1a88560b92520'                                  )
    );
{$ENDIF CBC}
{$IFDEF CFB}
function TestEncryptAnubisCFB (const aKey: String;
                               const anIV: String;
                               const aValue: String) : String;
function TestDecryptAnubisCFB (const aKey: String;
                               const anIV: String;
                               const aValue: String) : String;
function TestAnubisCFB : Boolean;

const
    ANUBIS_CFB_TEST_VECTORS : packed array [0..11] of TAnubisTestVector = (
        { 128-битный ключ }
        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : '024f81753056dca449109d9e3843f24e' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '7649abac8119b246cee98e9b12e9197d';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '7d9c5f428899399a004c5cdfad512f5a' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '5086cb9b507219ee95db113a917678b2';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '557d2aebe5964151900260a48373774d' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '73bed6b8e3c1743b7116e69e22229516';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : 'ba372ff049d4429c2013ec4fa9c09d95' ),

        { 192-битный ключ }
        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : 'decdd9b4377737ceae58e13e3eec88a6'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '4f021db243bc633d7178183a9fa071e8';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '191d7760c3664228cf9d8d74d4eb9cef'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : 'b4d9ada9ad7dedf4e5e738763f69145a';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '082eb17f5c17110625d69c5bf744ddb3'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '571b242012fb7ae07fa9baac3df102e0';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : '7541f30f5b76338a090f432b6403503b'                  ),

        { 256-битный ключ }
        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : '5a842bb658e6c8133836f7d20a0b500f'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : 'f58c4c04d6e5f1ba779eabfb5f7bfbd6';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '8854eb15a2b7e27c33c1cde4aa7e7d3c'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '9cfc4e967edb808d679f777bc6702c7d';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '7a1ad40ec027ce1131f992c694645778'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '39f23369a9d9bacfa530e26304231461';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : 'd291d54650bcd365181b253c19831c50'                                  )
    );
{$ENDIF CFB}
{$IFDEF OFB}
function TestEncryptAnubisOFB (const aKey: String;
                               const anIV: String;
                               const aValue: String) : String;
function TestDecryptAnubisOFB (const aKey: String;
                               const anIV: String;
                               const aValue: String) : String;
function TestAnubisOFB : Boolean;

const
    ANUBIS_OFB_TEST_VECTORS : packed array [0..11] of TAnubisTestVector = (
        { 128-битный ключ }
        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : '024f81753056dca449109d9e3843f24e' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '7649abac8119b246cee98e9b12e9197d';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '7d9c5f428899399a004c5cdfad512f5a' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '5086cb9b507219ee95db113a917678b2';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '557d2aebe5964151900260a48373774d' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '73bed6b8e3c1743b7116e69e22229516';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : 'ba372ff049d4429c2013ec4fa9c09d95' ),

        { 192-битный ключ }
        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : 'decdd9b4377737ceae58e13e3eec88a6'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '4f021db243bc633d7178183a9fa071e8';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '191d7760c3664228cf9d8d74d4eb9cef'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : 'b4d9ada9ad7dedf4e5e738763f69145a';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '082eb17f5c17110625d69c5bf744ddb3'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '571b242012fb7ae07fa9baac3df102e0';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : '7541f30f5b76338a090f432b6403503b'                  ),

        { 256-битный ключ }
        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : '5a842bb658e6c8133836f7d20a0b500f'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : 'f58c4c04d6e5f1ba779eabfb5f7bfbd6';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '8854eb15a2b7e27c33c1cde4aa7e7d3c'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '9cfc4e967edb808d679f777bc6702c7d';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '7a1ad40ec027ce1131f992c694645778'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '39f23369a9d9bacfa530e26304231461';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : 'd291d54650bcd365181b253c19831c50'                                  )
    );
{$ENDIF OFB}
