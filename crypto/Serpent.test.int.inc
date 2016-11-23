{******************************************************************************}
{*  Serpent.test.int.inc                                                      *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
type
    PSerpentTestVector = ^TSerpentTestVector;
    TSerpentTestVector = packed record
        Key    : String;
        IV     : String [32];
        Plain  : String [32];
        Cipher : String [32];
    end;

{$IFDEF ECB}
function TestEncryptSerpentECB (const aKey: String;
                                const aValue: String) : String;
function TestDecryptSerpentECB (const aKey: String;
                                const aValue: String) : String;
function TestSerpentECB : Boolean;

const
    SERPENT_ECB_TEST_VECTORS : packed array [0..11] of TSerpentTestVector = (
        { 128-битный ключ }
        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : 'f7a721e6c756b655cbdf533fc3b31ac4' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '4bc604293a81a6a6e4cba78d1a32a29e' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : 'cfc28e5097dd6b49a938b1515ebc5aac' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : 'fed2c49592f91c0c9f17cd86386529eb' ),

        { 192-битный ключ }
        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : '0dfb53231f1f3515b9519133e67cec9e'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '291862d808d8c8ef47497e0941b94823'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '54b0129554dd954e09b09cbe0142449d'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : 'c72b88487cb1636f4a31a92ecb05b24b'                  ),

        { 256-битный ключ }
        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : '78e5848ed9d5de2d4db02f53616afdf2'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '505df16892408ef69c3b9ea667d9ddb8'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : 'b95fc82076521dce60e4fcace3d39151'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : '092262de626dc57b4c870c65e71fc713'                                  )
    );
{$ENDIF ECB}
{$IFDEF CBC}
function TestEncryptSerpentCBC (const aKey: String;
                                const anIV: String;
                                const aValue: String) : String;
function TestDecryptSerpentCBC (const aKey: String;
                                const anIV: String;
                                const aValue: String) : String;
function TestSerpentCBC : Boolean;

const
    SERPENT_CBC_TEST_VECTORS : packed array [0..11] of TSerpentTestVector = (
        { 128-битный ключ }
        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : 'dd73691ab566b638e3b96236c8c8a1dd' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '7649abac8119b246cee98e9b12e9197d';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '65d480fb0e3630edf2d2f0601be62e10' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '5086cb9b507219ee95db113a917678b2';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '175853d355109e6ffee74c3c734ba377' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '73bed6b8e3c1743b7116e69e22229516';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : '0de9e34db1b71a809dbbaa82aadf5d98' ),

        { 192-битный ключ }
        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : '3c37e8cffbd84fefee71c8e57fd8d271'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '4f021db243bc633d7178183a9fa071e8';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : 'ba452bd403ad61df1d90566eb62f70c8'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : 'b4d9ada9ad7dedf4e5e738763f69145a';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : 'a5da94eed8d92510edab4db6d89bd0fe'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '571b242012fb7ae07fa9baac3df102e0';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : '96731d19ec8af4493f2717f0d4843408'                  ),

        { 256-битный ключ }
        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : 'b893c8dec5c85f0301ac3274dfc6719d'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : 'f58c4c04d6e5f1ba779eabfb5f7bfbd6';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '50c5eadd81667916335e65501f3a896a'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '9cfc4e967edb808d679f777bc6702c7d';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '0b7e17fe6aa99b6f2fd87daca7d45bcf'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '39f23369a9d9bacfa530e26304231461';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : 'b87fe9c0e6ad06f36b11db4950ecf290'                                  )
    );
{$ENDIF CBC}
{$IFDEF CFB}
function TestEncryptSerpentCFB (const aKey: String;
                                const anIV: String;
                                const aValue: String) : String;
function TestDecryptSerpentCFB (const aKey: String;
                                const anIV: String;
                                const aValue: String) : String;
function TestSerpentCFB : Boolean;

const
    SERPENT_CFB_TEST_VECTORS : packed array [0..11] of TSerpentTestVector = (
        { 128-битный ключ }
        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : 'e2c436613fe58f75033f1a7e9b3bddd6' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '7649abac8119b246cee98e9b12e9197d';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '63b498910116624b3ebb66c89bde720f' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '5086cb9b507219ee95db113a917678b2';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '90df33070e58bbfc63a76f053e77aa16' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '73bed6b8e3c1743b7116e69e22229516';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : 'ffaeaf129f4be1bc7f2dc99e690970b7' ),

        { 192-битный ключ }
        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : '12fd290909f874ea042298adea478b34'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '4f021db243bc633d7178183a9fa071e8';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '9a9fbe7c03cf3808070e803497556ebb'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : 'b4d9ada9ad7dedf4e5e738763f69145a';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : 'e9c91d72be0ca68e4234fcbe3a25d190'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '571b242012fb7ae07fa9baac3df102e0';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : 'ffb13c73617fc8b06569c480220afe4a'                  ),

        { 256-битный ключ }
        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : '658b177eb796c13cf800c1d15d6a3438'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : 'f58c4c04d6e5f1ba779eabfb5f7bfbd6';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '09a0f1f201ae16e159e7a168e5db1652'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '9cfc4e967edb808d679f777bc6702c7d';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : 'a470eccce7ddf8f0b1e1671fceec9b0c'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '39f23369a9d9bacfa530e26304231461';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : 'ddfc9c49e3d19472090e3a93c041d88d'                                  )
    );
{$ENDIF CFB}
{$IFDEF OFB}
function TestEncryptSerpentOFB (const aKey: String;
                                const anIV: String;
                                const aValue: String) : String;
function TestDecryptSerpentOFB (const aKey: String;
                                const anIV: String;
                                const aValue: String) : String;
function TestSerpentOFB : Boolean;

const
    SERPENT_OFB_TEST_VECTORS : packed array [0..11] of TSerpentTestVector = (
        { 128-битный ключ }
        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : 'e2c436613fe58f75033f1a7e9b3bddd6' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '7649abac8119b246cee98e9b12e9197d';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '63b498910116624b3ebb66c89bde720f' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '5086cb9b507219ee95db113a917678b2';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '90df33070e58bbfc63a76f053e77aa16' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '73bed6b8e3c1743b7116e69e22229516';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : 'ffaeaf129f4be1bc7f2dc99e690970b7' ),

        { 192-битный ключ }
        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : '12fd290909f874ea042298adea478b34'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '4f021db243bc633d7178183a9fa071e8';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '9a9fbe7c03cf3808070e803497556ebb'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : 'b4d9ada9ad7dedf4e5e738763f69145a';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : 'e9c91d72be0ca68e4234fcbe3a25d190'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '571b242012fb7ae07fa9baac3df102e0';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : 'ffb13c73617fc8b06569c480220afe4a'                  ),

        { 256-битный ключ }
        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : '658b177eb796c13cf800c1d15d6a3438'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : 'f58c4c04d6e5f1ba779eabfb5f7bfbd6';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '09a0f1f201ae16e159e7a168e5db1652'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '9cfc4e967edb808d679f777bc6702c7d';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : 'a470eccce7ddf8f0b1e1671fceec9b0c'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '39f23369a9d9bacfa530e26304231461';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : 'ddfc9c49e3d19472090e3a93c041d88d'                                  )
    );
{$ENDIF OFB}
