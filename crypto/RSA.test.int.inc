{******************************************************************************}
{*  RSA.test.int.inc                                                          *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
type
    PRSATestVector = ^TRSATestVector;
    TRSATestVector = packed record
        n      : String;
        e      : String;
        d      : String;
        Seed   : String;
        Plain  : String;
        Cipher : String;
    end;

function TestEncryptRSA (const pub_e: String;
                         const pub_n: String;
                         const aSeed: String;
                         const aValue: String) : String; overload;
function TestEncryptRSA (const aPublicKey: String;
                         const aSeed: String;
                         const aValue: String) : String; overload;

function TestDecryptRSA (const prv_d: String;
                         const prv_n: String;
                         const aValue: String) : String; overload;
function TestDecryptRSA (const aPrivateKey: String;
                         const aValue: String) : String; overload;
                         
function TestRSA : Boolean;

const
    RSA_TEST_VECTORS : packed array [0..3] of TRSATestVector = (
        { 1024-битный ключ }
        ( n      : '9caaddf1fd250b22e33c47644c5f7fa2'+
                   '55f0301d6e14c27fa2f89ceb5ea1b748'+
                   '9908ef5b89f5a6c3fe7b04006c4065f5'+
                   'd8eb5cde956a938a2cfc1706bf37884e'+
                   '22b9f8a4c71aeb45ab48f5c161f9d35f'+
                   '6b1c9aef3aa8a5b404ba2475064f2920'+
                   'e0820925c7ef571c30bfbf75415d2042'+
                   'e2c29ef5ff4d623d0539ac3cea74841d';
          e      : 'ad22ee94ab9f5d2ae56d7078fb53fab5'+
                   '7c3548413ed592d9216c1fa2ca45edab';
          d      : '1e88fd650a4f3847100eded50d8592a0'+
                   '918571c6dcd497b5e30ae9cfc8e63d8f'+
                   '222521eafd50f9f7fab8807ecc24033c'+
                   '2eed7a048a8b98f72b29ea9340472cd2'+
                   '87e39a249ecb7734c8708599b251ea34'+
                   '89e2508fd1eae2505d52726ced7dc051'+
                   'b6a405ebcf44fcfaaa8e09ef8d8dadfc'+
                   '9b78b97ee815456f8fe3a95e62228a13';
          Seed   : '4ceea1a89464a5d2f89e078953caf776'+
                   '365898a5bd5e8e448c65da26ff98900c'+
                   'd08061ef446c69b48dc4609ed8654a64'+
                   '6d708262cb8409ac27c4a49adfed47a8'+
                   '5ad429ed75077578e4c273c61e2c3b46'+
                   'beb472f0a345a05d61a7eaaad8a63e0b'+
                   '3d4952f27c4081329e';
          Plain  : '7672cfc27a41d501aa4c41baabf4525a'+
                   '7c455fc8000000000000000000000000';
          Cipher : '8d58174a2a948c8022489a451f63d87f'+
                   'c9be40b9be501e4c15c2cde0a16f9e2e'+
                   'c224f549016f5cab2914aef99bae323b'+
                   '7018ae404fc8e717e1a8479e2cdb032f'+
                   '8f5329aa2d3533bc4efe4a5db8d45232'+
                   '51c1aa10cd8625655a5d84e2a45ee449'+
                   '3af2a67c0158dc718a91dfe62d155a31'+
                   '81ecc668ffd8c51fa67275a6ad909637' ),

        ( n      : '9caaddf1fd250b22e33c47644c5f7fa2'+
                   '55f0301d6e14c27fa2f89ceb5ea1b748'+
                   '9908ef5b89f5a6c3fe7b04006c4065f5'+
                   'd8eb5cde956a938a2cfc1706bf37884e'+
                   '22b9f8a4c71aeb45ab48f5c161f9d35f'+
                   '6b1c9aef3aa8a5b404ba2475064f2920'+
                   'e0820925c7ef571c30bfbf75415d2042'+
                   'e2c29ef5ff4d623d0539ac3cea74841d';
          e      : 'ad22ee94ab9f5d2ae56d7078fb53fab5'+
                   '7c3548413ed592d9216c1fa2ca45edab';
          d      : '1e88fd650a4f3847100eded50d8592a0'+
                   '918571c6dcd497b5e30ae9cfc8e63d8f'+
                   '222521eafd50f9f7fab8807ecc24033c'+
                   '2eed7a048a8b98f72b29ea9340472cd2'+
                   '87e39a249ecb7734c8708599b251ea34'+
                   '89e2508fd1eae2505d52726ced7dc051'+
                   'b6a405ebcf44fcfaaa8e09ef8d8dadfc'+
                   '9b78b97ee815456f8fe3a95e62228a13';
          Seed   : '55884c830d4a80b79f089da74dc25ae0'+
                   'c482462145e1d09523da3c9344bb97b0'+
                   '52fbdc1543dfb53cf2378259687c7b1b'+
                   '35caf2f91999ed4ace39af10d6bed0fa'+
                   '22444c129d90741cfcda90198e2782fb'+
                   '03bdcc7cfafd89db6fb0fed224';
          Plain  : 'a16ad8f2e0932342ed21e13777f4652a'+
                   '3550ddb4368b5ea71c66dbc3bbfeb7db';
          Cipher : '1ee876239c8ad659fa98977ef8febb77'+
                   '2e11838836dabd0d501731596ad01e22'+
                   '3e1c7a283b28aabd1a39553871c381f5'+
                   '48ccbff10425454ab369f8869fbc075c'+
                   'e4b19e5c347318b9bd3ed70f50f1618a'+
                   'cdc7e2ebfdcbe06833ac1c13ccbdbb06'+
                   'f84a0266184bd19690a85d147ac13f8e'+
                   'f450ca1e24157318d2b8f859dc4871b0' ),

        ( n      : '9caaddf1fd250b22e33c47644c5f7fa2'+
                   '55f0301d6e14c27fa2f89ceb5ea1b748'+
                   '9908ef5b89f5a6c3fe7b04006c4065f5'+
                   'd8eb5cde956a938a2cfc1706bf37884e'+
                   '22b9f8a4c71aeb45ab48f5c161f9d35f'+
                   '6b1c9aef3aa8a5b404ba2475064f2920'+
                   'e0820925c7ef571c30bfbf75415d2042'+
                   'e2c29ef5ff4d623d0539ac3cea74841d';
          e      : 'ad22ee94ab9f5d2ae56d7078fb53fab5'+
                   '7c3548413ed592d9216c1fa2ca45edab';
          d      : '1e88fd650a4f3847100eded50d8592a0'+
                   '918571c6dcd497b5e30ae9cfc8e63d8f'+
                   '222521eafd50f9f7fab8807ecc24033c'+
                   '2eed7a048a8b98f72b29ea9340472cd2'+
                   '87e39a249ecb7734c8708599b251ea34'+
                   '89e2508fd1eae2505d52726ced7dc051'+
                   'b6a405ebcf44fcfaaa8e09ef8d8dadfc'+
                   '9b78b97ee815456f8fe3a95e62228a13';
          Seed   : '5878c91b166e90c34e6e66568c151f4d'+
                   '444340b5f1d73052cc5633ea2e47aced'+
                   '7b178a64fb09a5ad0846aee4116d6780'+
                   'ee75eb20851668820cdec0f2c496e4c2'+
                   '88d8279c1c5d4ec00d980c272e870518'+
                   '486dcaea85';
          Plain  : '1a9b8729210a8471fc5cd709f2edd324'+
                   '0150244bec96a92ff807e3b30d295d3c';
          Cipher : '2c5a98eb778095ce0f931130f8a53037'+
                   'f4327d112cfdf935a8858ce62c2e9047'+
                   'a4fdcf008f65ccad5169c8a4d5e9e5ea'+
                   '5ffdac11e21c9a3e6bce30125e9e9e7b'+
                   '06e9696863b63a0eed91d2cb49c82ce2'+
                   '5f0d2818f3a4928b43481f7170c8c036'+
                   '9b4c00805bf972f1884616b5c6e7744c'+
                   '026a2f236de705dc12ac52a3f1d68c4e' ),

        ( n      : '9caaddf1fd250b22e33c47644c5f7fa2'+
                   '55f0301d6e14c27fa2f89ceb5ea1b748'+
                   '9908ef5b89f5a6c3fe7b04006c4065f5'+
                   'd8eb5cde956a938a2cfc1706bf37884e'+
                   '22b9f8a4c71aeb45ab48f5c161f9d35f'+
                   '6b1c9aef3aa8a5b404ba2475064f2920'+
                   'e0820925c7ef571c30bfbf75415d2042'+
                   'e2c29ef5ff4d623d0539ac3cea74841d';
          e      : 'ad22ee94ab9f5d2ae56d7078fb53fab5'+
                   '7c3548413ed592d9216c1fa2ca45edab';
          d      : '1e88fd650a4f3847100eded50d8592a0'+
                   '918571c6dcd497b5e30ae9cfc8e63d8f'+
                   '222521eafd50f9f7fab8807ecc24033c'+
                   '2eed7a048a8b98f72b29ea9340472cd2'+
                   '87e39a249ecb7734c8708599b251ea34'+
                   '89e2508fd1eae2505d52726ced7dc051'+
                   'b6a405ebcf44fcfaaa8e09ef8d8dadfc'+
                   '9b78b97ee815456f8fe3a95e62228a13';
          Seed   : '1c25c9b832169a1fdb6c148e47e66c3c'+
                   'c82141e611a6f30cc90c5049e8c502b3'+
                   '1cadc76239b7bdaf93fa97343e7ee551'+
                   'bc52fdb5ec9e400af05dbeacda';
          Plain  : 'a6d0e8c1ea4ab4ecc8957d6228157967'+
                   '5a648d62b7f22b2b08d1313f406f137e';
          Cipher : '0bbd48f5a41acf7a2564990edb8a490c'+
                   '7a7aef7b4ac1e5698f3040d657f2b5cf'+
                   '337df9f5ba62b5cd4bc8111b2f90d0d7'+
                   'a78643c18f7f75449f7ea721d9e6f3af'+
                   '35f8229d27f0cc21ff85e6f0abef02c1'+
                   '9f042d6e5a3753afaadae99aafc8172b'+
                   '3f82670f41b6bb6f11f3f0ce4b5537b7'+
                   '194317ac81156ca18e100fda815a1564' )
    );
