{******************************************************************************}
{*  TwoFish.test.int.inc                                                      *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
type
    PTwoFishTestVector = ^TTwoFishTestVector;
    TTwoFishTestVector = packed record
        Key    : String;
        IV     : String [32];
        Plain  : String [32];
        Cipher : String [32];
    end;

{$IFDEF ECB}
function TestEncryptTwoFishECB (const aKey: String;
                                const aValue: String) : String;
function TestDecryptTwoFishECB (const aKey: String;
                                const aValue: String) : String;
function TestTwoFishECB : Boolean;

const
    TWOFISH_ECB_TEST_VECTORS : packed array [0..11] of TTwoFishTestVector = (
        { 128-битный ключ }
        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : '291ed11a7b141a067e773959f13974df' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '9b05aa1ecbb6650fe3080aa10c2333d3' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '69e255a54af4617452a65ff06375ab73' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : 'ec939150df26b24d377943e213332f47' ),

        { 192-битный ключ }
        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : 'dd250b486e904968b4495f5a110a6936'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : 'bab94ee6dbb02af4502c90087ea78b4d'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : 'dba3ec4cf1ac94b1270f6ed948691807'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : 'c05fa84d69f3dd5d16f2798faf14f833'                  ),

        { 256-битный ключ }
        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : 'e1b45f5f5bd0c9ea0de77424054222a4'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '341a841d6fff65c96f7924f8bf481072'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '8c03971edb3fc796f861998549b7055d'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : 'f8c5a1e78eba66292b59d0baaa235d07'                                  )
    );
{$ENDIF ECB}
{$IFDEF CBC}
function TestEncryptTwoFishCBC (const aKey: String;
                                const anIV: String;
                                const aValue: String) : String;
function TestDecryptTwoFishCBC (const aKey: String;
                                const anIV: String;
                                const aValue: String) : String;
function TestTwoFishCBC : Boolean;

const
    TWOFISH_CBC_TEST_VECTORS : packed array [0..11] of TTwoFishTestVector = (
        { 128-битный ключ }
        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : 'c2067ac0f36692c15e0fbbef48acf4ab' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '7649abac8119b246cee98e9b12e9197d';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '53715ed074414b2cdebf9f8e0d30a9e6' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '5086cb9b507219ee95db113a917678b2';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : 'a4dc385c52552b8db2acb48373871d4a' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '73bed6b8e3c1743b7116e69e22229516';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : '97cbad8d70ed446ccaa1643a28460989' ),

        { 192-битный ключ }
        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : 'fa2245e40a3f42b6eed643740bf56868'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '4f021db243bc633d7178183a9fa071e8';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '0550cf13fd4d095bd9dac30283f078be'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : 'b4d9ada9ad7dedf4e5e738763f69145a';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : 'abc865a83d53bba3ab481c4ce15ab6f4'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '571b242012fb7ae07fa9baac3df102e0';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : '70170acde6cdf8b93a76dae9fe467706'                  ),

        { 256-битный ключ }
        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : 'e0227c3cc80f3cb1b2ed847cc6f57d3c'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : 'f58c4c04d6e5f1ba779eabfb5f7bfbd6';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '05334cd73d26dbe882784118b4a6b5ed'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '9cfc4e967edb808d679f777bc6702c7d';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '0dda75a90ed29a1e844971af69f3550f'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '39f23369a9d9bacfa530e26304231461';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : '075c28e2b19c15132308ed9c9b6efd6d'                                  )
    );
{$ENDIF CBC}
{$IFDEF CFB}
function TestEncryptTwoFishCFB (const aKey: String;
                                const anIV: String;
                                const aValue: String) : String;
function TestDecryptTwoFishCFB (const aKey: String;
                                const anIV: String;
                                const aValue: String) : String;
function TestTwoFishCFB : Boolean;

const
    TWOFISH_CFB_TEST_VECTORS : packed array [0..11] of TTwoFishTestVector = (
        { 128-битный ключ }
        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : '99279b8c6ceaec582deaf8bc7aa380ee' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '7649abac8119b246cee98e9b12e9197d';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : 'e5a4df6e1eea1c4d5fa5927ea3276fa2' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '5086cb9b507219ee95db113a917678b2';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : 'e884de9f55b1319499cea4393418f2e0' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '73bed6b8e3c1743b7116e69e22229516';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : '3ec3e826b625cf5518758384008c46fa' ),

        { 192-битный ключ }
        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : '05cefff307bfa29ce1137d29d302ac5e'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '4f021db243bc633d7178183a9fa071e8';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '031360e7d487aaedfdb5f06649589870'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : 'b4d9ada9ad7dedf4e5e738763f69145a';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : 'f7b2b00d2986fde30f4b5c6d7a833ff1'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '571b242012fb7ae07fa9baac3df102e0';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : '1cf8db800da36e4c645e1970985dc664'                  ),

        { 256-битный ключ }
        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : '6c11a373e3d93b0cdb4c81a58b8be284'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : 'f58c4c04d6e5f1ba779eabfb5f7bfbd6';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : 'b6eb5ab0ac8be99d89665de9794acdee'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '9cfc4e967edb808d679f777bc6702c7d';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '5f8817488acefe48d2e3b0893fad88bf'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '39f23369a9d9bacfa530e26304231461';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : '3986d948ddcda75cbef9edf54ff21e24'                                  )
    );
{$ENDIF CFB}
{$IFDEF OFB}
function TestEncryptTwoFishOFB (const aKey: String;
                                const anIV: String;
                                const aValue: String) : String;
function TestDecryptTwoFishOFB (const aKey: String;
                                const anIV: String;
                                const aValue: String) : String;
function TestTwoFishOFB : Boolean;

const
    TWOFISH_OFB_TEST_VECTORS : packed array [0..11] of TTwoFishTestVector = (
        { 128-битный ключ }
        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : '99279b8c6ceaec582deaf8bc7aa380ee' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '7649abac8119b246cee98e9b12e9197d';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : 'e5a4df6e1eea1c4d5fa5927ea3276fa2' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '5086cb9b507219ee95db113a917678b2';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : 'e884de9f55b1319499cea4393418f2e0' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '73bed6b8e3c1743b7116e69e22229516';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : '3ec3e826b625cf5518758384008c46fa' ),

        { 192-битный ключ }
        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : '05cefff307bfa29ce1137d29d302ac5e'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '4f021db243bc633d7178183a9fa071e8';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '031360e7d487aaedfdb5f06649589870'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : 'b4d9ada9ad7dedf4e5e738763f69145a';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : 'f7b2b00d2986fde30f4b5c6d7a833ff1'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '571b242012fb7ae07fa9baac3df102e0';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : '1cf8db800da36e4c645e1970985dc664'                  ),

        { 256-битный ключ }
        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : '6c11a373e3d93b0cdb4c81a58b8be284'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : 'f58c4c04d6e5f1ba779eabfb5f7bfbd6';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : 'b6eb5ab0ac8be99d89665de9794acdee'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '9cfc4e967edb808d679f777bc6702c7d';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '5f8817488acefe48d2e3b0893fad88bf'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '39f23369a9d9bacfa530e26304231461';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : '3986d948ddcda75cbef9edf54ff21e24'                                  )
    );
{$ENDIF OFB}
