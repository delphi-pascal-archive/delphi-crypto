{******************************************************************************}
{*  RSA.int.inc                                                               *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
type
    PRSABlock = ^TRSABlock;
    TRSABlock = packed array [0..0] of Byte;

procedure NULL_RSA_BLOCK (var aBlock: PRSABlock;
                          const aSize: WORD);

function RSABlockToHex (const aValue: PRSABlock;
                        const aSize: WORD;
                        const aLength: Integer = 0) : Hex;
procedure HexToRSABlock (const aValue: Hex;
                         out aResult: PRSABlock;
                         const aSize: WORD);

procedure GenKeysRSA (const aSize: WORD;
                      aRandomFunc: TRandomFunction;
                      out aPublicKey: String;
                      out aPrivateKey: String);

procedure EncryptRSA (const aPublicKey: String;
                      const anInput: TStream;
                      out anOutput: TStream); overload;
function EncryptRSA (const aPublicKey: String;
                     const aValue: String) : String; overload;

procedure DecryptRSA (const aPrivateKey: String;
                      const anInput: TStream;
                      out anOutput: TStream); overload;
function DecryptRSA (const aPrivateKey: String;
                     const aValue: String) : String; overload;

function RSA_VERSION : String;
