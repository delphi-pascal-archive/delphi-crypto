{******************************************************************************}
{*  TCrypto.int.inc                                                           *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ крипто-система }
{$M+}
    CCrypto = class of TCrypto;
    PCrypto = ^TCrypto;
    TCrypto = class (TMetaObject)
    protected
        function GetAlgAsymmetric : String; virtual;
        function GetAlgSymmetric : String; virtual;
        function GetModeSymmetric : String; virtual;
        function GetAlgHash : String; virtual;
        function GetGenRandom : String; virtual;
        procedure SetAlgAsymmetric (const aValue: String); virtual;
        procedure SetAlgSymmetric (const aValue: String); virtual;
        procedure SetModeSymmetric (const aValue: String); virtual;
        procedure SetAlgHash (const aValue: String); virtual;
        procedure SetGenRandom (const aValue: String); virtual;
    public
        class function GetClassName : String; override;
        class function GetClassVersion : TVersionInfo; override;
        class function GetClassProperties : TProtoProperties; override;
        class function GetClassTable : String; override;
    public
        // id, alg_asymmetric, alg_symmetric, mode_symmetric, alg_hash, gen_random
        constructor Create (const aDB: TSQLiteDatabase;
                            anArgs: array of const;
                            aMode: TObjectMode = []); override;
        destructor Destroy; override;
    public
        function Random (const aMin: LongWord = 0;
                         const aMax: LongWord = High (LongWord)) : LongWord; virtual;
        function Hash (const aValue: String) : Hex; overload; virtual;
        function Hash (const aValue: String;
                       const aSalt: String;
                       const aCount: DWORD = 1024) : Hex; overload; virtual;
        function Hash (const aValue: TStream) : Hex; overload; virtual;
        function HMAC (const aValue: String;
                       const aKey: String) : Hex; overload; virtual;
        function HMAC (const aValue: String;
                       const aKey: String;
                       const aSalt: String;
                       const aCount: DWORD = 1024) : Hex; overload; virtual;
        procedure GenerateKey (out aKey: String); overload; virtual;
        function GenerateKey : String; overload; virtual;
        function Encrypt (const aValue: String;
                          const aKey: String) : String; overload; virtual;
        function Decrypt (const aValue: String;
                          const aKey: String) : String; overload; virtual;
        procedure GenerateKeys (out aPublicKey: String;
                                out aPrivateKey: String); virtual;
        function Encrypt (const aValue: String;
                          const aPublicKey: String;
                          const aPrivateKey: String) : String; overload; virtual;
        function Decrypt (const aValue: String;
                          const aPublicKey: String;
                          const aPrivateKey: String) : String; overload; virtual;
        procedure Encrypt (const anInput: TStream;
                           out anOutput: TStream;
                           const aPublicKey: String;
                           const aPrivateKey: String); overload; virtual;
        procedure Decrypt (const anInput: TStream;
                           out anOutput: TStream;
                           const aPublicKey: String;
                           const aPrivateKey: String); overload; virtual;
    public
        function Check : Boolean; override;
    public
        property algAsymmetric: String read GetAlgAsymmetric write SetAlgAsymmetric;
        property algSymmetric: String read GetAlgSymmetric write SetAlgSymmetric;
        property modeSymmetric: String read GetModeSymmetric write SetModeSymmetric;
        property algHash: String read GetAlgHash write SetAlgHash;
        property genRandom: String read GetGenRandom write SetGenRandom;
    end;
{$M-}