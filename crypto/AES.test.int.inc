{******************************************************************************}
{*  AES.test.int.inc                                                          *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
type
    PAESTestVector = ^TAESTestVector;
    TAESTestVector = packed record
        Key    : String;
        IV     : String [32];
        Plain  : String [32];
        Cipher : String [32];
    end;

{$IFDEF ECB}
function TestEncryptAESECB (const aKey: String;
                            const aValue: String) : String;
function TestDecryptAESECB (const aKey: String;
                            const aValue: String) : String;
function TestAESECB : Boolean;

const
    AES_ECB_TEST_VECTORS : packed array [0..11] of TAESTestVector = (
        { 128-битный ключ }
        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : '3ad77bb40d7a3660a89ecaf32466ef97' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : 'f5d3d58503b9699de785895a96fdbaaf' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '43b1cd7f598ece23881b00e3ed030688' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : '7b0c785e27e8ad3f8223207104725dd4' ),

        { 192-битный ключ }
        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : 'bd334f1d6e45f25ff712a214571fa5cc'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '974104846d0ad3ad7734ecb3ecee4eef'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : 'ef7afd2270e2e60adce0ba2face6444e'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : '9a4b41ba738d6c72fb16691603c18e0e'                  ),

        { 256-битный ключ }
        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : 'f3eed1bdb5d2a03c064b5a7e3db181f8'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '591ccb10d410ed26dc5ba74a31362870'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : 'b6ed21b99ca6f4f9f153e7b1beafed1d'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : '23304b7a39f9f3ff067d8d8f9e24ecc7'                                  )
    );
{$ENDIF ECB}
{$IFDEF CBC}
function TestEncryptAESCBC (const aKey: String;
                            const anIV: String;
                            const aValue: String) : String;
function TestDecryptAESCBC (const aKey: String;
                            const anIV: String;
                            const aValue: String) : String;
function TestAESCBC : Boolean;

const
    AES_CBC_TEST_VECTORS : packed array [0..11] of TAESTestVector = (
        { 128-битный ключ }
        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : '7649abac8119b246cee98e9b12e9197d' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '7649abac8119b246cee98e9b12e9197d';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '5086cb9b507219ee95db113a917678b2' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '5086cb9b507219ee95db113a917678b2';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '73bed6b8e3c1743b7116e69e22229516' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '73bed6b8e3c1743b7116e69e22229516';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : '3ff1caa1681fac09120eca307586e1a7' ),

        { 192-битный ключ }
        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : '4f021db243bc633d7178183a9fa071e8'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '4f021db243bc633d7178183a9fa071e8';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : 'b4d9ada9ad7dedf4e5e738763f69145a'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : 'b4d9ada9ad7dedf4e5e738763f69145a';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '571b242012fb7ae07fa9baac3df102e0'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '571b242012fb7ae07fa9baac3df102e0';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : '08b0e27988598881d920a9e64f5615cd'                  ),

        { 256-битный ключ }
        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : 'f58c4c04d6e5f1ba779eabfb5f7bfbd6'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : 'f58c4c04d6e5f1ba779eabfb5f7bfbd6';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '9cfc4e967edb808d679f777bc6702c7d'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '9cfc4e967edb808d679f777bc6702c7d';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '39f23369a9d9bacfa530e26304231461'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '39f23369a9d9bacfa530e26304231461';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : 'b2eb05e2c39be9fcda6c19078c6a9d1b'                                  )
    );
{$ENDIF CBC}
{$IFDEF CFB}
function TestEncryptAESCFB (const aKey: String;
                            const anIV: String;
                            const aValue: String) : String;
function TestDecryptAESCFB (const aKey: String;
                            const anIV: String;
                            const aValue: String) : String;
function TestAESCFB : Boolean;

const
    AES_CFB_TEST_VECTORS : packed array [0..11] of TAESTestVector = (
        { 128-битный ключ }
        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : '3b3fd92eb72dad20333449f8e83cfb4a' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '3b3fd92eb72dad20333449f8e83cfb4a';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : 'c8a64537a0b3a93fcde3cdad9f1ce58b' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : 'c8a64537a0b3a93fcde3cdad9f1ce58b';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '26751f67a3cbb140b1808cf187a4f4df' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '26751f67a3cbb140b1808cf187a4f4df';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : 'c04b05357c5d1c0eeac4c66f9ff7f2e6' ),

        { 192-битный ключ }
        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : 'cdc80d6fddf18cab34c25909c99a4174'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : 'cdc80d6fddf18cab34c25909c99a4174';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '67ce7f7f81173621961a2b70171d3d7a'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '67ce7f7f81173621961a2b70171d3d7a';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '2e1e8a1dd59b88b1c8e60fed1efac4c9'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '2e1e8a1dd59b88b1c8e60fed1efac4c9';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : 'c05f9f9ca9834fa042ae8fba584b09ff'                  ),

        { 256-битный ключ }
        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : 'dc7e84bfda79164b7ecd8486985d3860'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : 'dc7e84bfda79164b7ecd8486985d3860';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '39ffed143b28b1c832113c6331e5407b'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '39ffed143b28b1c832113c6331e5407b';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : 'df10132415e54b92a13ed0a8267ae2f9'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : 'df10132415e54b92a13ed0a8267ae2f9';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : '75a385741ab9cef82031623d55b1e471'                                  )
    );
{$ENDIF CFB}
{$IFDEF OFB}
function TestEncryptAESOFB (const aKey: String;
                            const anIV: String;
                            const aValue: String) : String;
function TestDecryptAESOFB (const aKey: String;
                            const anIV: String;
                            const aValue: String) : String;
function TestAESOFB : Boolean;

const
    AES_OFB_TEST_VECTORS : packed array [0..11] of TAESTestVector = (
        { 128-битный ключ }
        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : '3b3fd92eb72dad20333449f8e83cfb4a' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '3b3fd92eb72dad20333449f8e83cfb4a';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : 'c8a64537a0b3a93fcde3cdad9f1ce58b' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : 'c8a64537a0b3a93fcde3cdad9f1ce58b';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '26751f67a3cbb140b1808cf187a4f4df' ),

        ( Key    : '2b7e151628aed2a6abf7158809cf4f3c';
          IV     : '26751f67a3cbb140b1808cf187a4f4df';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : 'c04b05357c5d1c0eeac4c66f9ff7f2e6' ),

        { 192-битный ключ }
        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : 'cdc80d6fddf18cab34c25909c99a4174'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : 'cdc80d6fddf18cab34c25909c99a4174';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '67ce7f7f81173621961a2b70171d3d7a'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '67ce7f7f81173621961a2b70171d3d7a';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : '2e1e8a1dd59b88b1c8e60fed1efac4c9'                  ),

        ( Key    : '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
          IV     : '2e1e8a1dd59b88b1c8e60fed1efac4c9';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : 'c05f9f9ca9834fa042ae8fba584b09ff'                  ),

        { 256-битный ключ }
        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '000102030405060708090a0b0c0d0e0f';
          Plain  : '6bc1bee22e409f96e93d7e117393172a';
          Cipher : 'dc7e84bfda79164b7ecd8486985d3860'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : 'dc7e84bfda79164b7ecd8486985d3860';
          Plain  : 'ae2d8a571e03ac9c9eb76fac45af8e51';
          Cipher : '39ffed143b28b1c832113c6331e5407b'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : '39ffed143b28b1c832113c6331e5407b';
          Plain  : '30c81c46a35ce411e5fbc1191a0a52ef';
          Cipher : 'df10132415e54b92a13ed0a8267ae2f9'                                  ),

        ( Key    : '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
          IV     : 'df10132415e54b92a13ed0a8267ae2f9';
          Plain  : 'f69f2445df4f9b17ad2b417be66c3710';
          Cipher : '75a385741ab9cef82031623d55b1e471'                                  )
    );
{$ENDIF OFB}
