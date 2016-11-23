{******************************************************************************}
{*  TUser.int.inc                                                             *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011-2012                                        *}
{******************************************************************************}
{ пользователь }
{$M+}
    CUser = class of TUser;
    PUser = ^TUser;
    TUser = class (THyperObject)
    private
        f_Crypto: TCrypto; { настройки крипто-системы }
        f_Pic: TPic;       { иконка }
    protected
        function CalcKeyHash : Hex; override;
        function CalcDataHash : Hex; override;
    protected
        function GetIDOwner : TID; virtual; 
        function GetIDCrypto : TID; virtual;
        function GetCrypto : TCrypto; virtual;
        function GetPassword : String; virtual;
        function GetSalt : Hex; virtual;
        function GetEMail : String; virtual;
        function GetEMailPassword : String; virtual;
        function GetIP : String; virtual;
        function GetPort : WORD; virtual;
        function GetDescription : String; virtual;
        function GetSex : Byte; virtual;
        function GetBirthday : TDateTime; virtual;
        function GetHashPic : Hex; virtual;
        function GetIDPic : TID; virtual;
        function GetPic : TPic; virtual;
        function GetPublicKey : String; virtual;
        function GetPrivateKey : String; virtual;
        function GetTimeOut : WORD; virtual;
        function GetUseProxy : Boolean; virtual;
        function GetProxyIP : String; virtual;
        function GetProxyPort : WORD; virtual;
        function GetProxyLogin : String; virtual;
        function GetProxyPassword : String; virtual;
        function GetProxyProtocol : String; virtual;
        function GetSMTPHost : String; virtual;
        function GetSMTPPort : WORD; virtual;
        function GetPOP3Host : String; virtual;
        function GetPOP3Port : WORD; virtual;
        function GetAutoTLS : Boolean; virtual;
        function GetFullSSL : Boolean; virtual;
        procedure SetIDOwner (const aValue: TID); virtual;
        procedure SetIDCrypto (const aValue: TID); virtual;
        procedure SetCrypto (const aValue: TCrypto); virtual;
        procedure SetPassword (const aValue: String); virtual;
        procedure SetSalt (const aValue: Hex); virtual;
        procedure SetEMail (const aValue: String); virtual;
        procedure SetEMailPassword (const aValue: String); virtual;
        procedure SetIP (const aValue: String); virtual;
        procedure SetPort (const aValue: WORD); virtual;
        procedure SetDescription (const aValue: String); virtual;
        procedure SetSex (const aValue: Byte); virtual;
        procedure SetBirthday (const aValue: TDateTime); virtual;
        procedure SetHashPic (const aValue: Hex); virtual;
        procedure SetIDPic (const aValue: TID); virtual;
        procedure SetPic (const aValue: TPic); virtual;
        procedure SetPublicKey (const aValue: String); virtual;
        procedure SetPrivateKey (const aValue: String); virtual;
        procedure SetTimeOut (const aValue: WORD); virtual;
        procedure SetUseProxy (const aValue: Boolean); virtual;
        procedure SetProxyIP (const aValue: String); virtual;
        procedure SetProxyPort (const aValue: WORD); virtual;
        procedure SetProxyLogin (const aValue: String); virtual;
        procedure SetProxyPassword (const aValue: String); virtual;
        procedure SetProxyProtocol (const aValue: String); virtual;
        procedure SetSMTPHost (const aValue: String); virtual;
        procedure SetSMTPPort (const aValue: WORD); virtual;
        procedure SetPOP3Host (const aValue: String); virtual;
        procedure SetPOP3Port (const aValue: WORD); virtual;
        procedure SetAutoTLS (const aValue: Boolean); virtual;
        procedure SetFullSSL (const aValue: Boolean); virtual;
    protected
        procedure SetDB (const aDB: TSQLiteDatabase); override;
    public
        class function GetClassName : String; override;
        class function GetClassVersion : TVersionInfo; override;
        class function GetClassProperties : TProtoProperties; override;
        class function GetClassList : CProtoObjects; override;
        class function GetClassTable : String; override;
        class procedure CreateClassTable (const aDB: TSQLiteDatabase); override;
    public
        // id, login, id_owner, id_crypto, id_pic, time_stamp_create, time_stamp_modify, time_stamp_public, version
        // остальные параметры необходимо указывать после создания
        constructor Create (const aDB: TSQLiteDatabase;
                            anArgs: array of const;
                            aMode: TObjectMode = []); override;
        destructor Destroy; override;
    public
        function Check : Boolean; override;
        class function Find (const aDB: TSQLiteDatabase;
                             const aLogin: String;
                             const anIDOwner: TID) : TID; overload; virtual;
        function Find : TID; override;
        procedure Load; override;
        procedure Save; override;
        {$IFNDEF DELETE_TRIGGER}
        procedure Delete; override;
        {$ENDIF DELETE_TRIGGER}
        function CheckPassword: Boolean; overload; virtual;
    public
        class function CheckLogin (const aLogin: String;
                                   var Errors: String) : Boolean; overload; virtual;
        class function CheckPassword (const aPassword: String;
                                      var Errors: String) : Boolean; overload; virtual;
        class function CheckEMail (const anEMail: String;
                                   var Errors: String) : Boolean; overload; virtual;
        class function CheckIP (const anIP: String;
                                var Errors: String) : Boolean; overload; virtual;
    public
        procedure LoadFromStream (const aStream: TStream); override;
        procedure SaveToStream (out aStream: TStream); override;
    protected
        property IDExternal;
    public
        property IDCrypto: TID read GetIDCrypto write SetIDCrypto;
        property Crypto: TCrypto read GetCrypto write SetCrypto;
        property Login: String read GetIDExternal write SetIDExternal;
        property IDOwner: TID read GetIDOwner write SetIDOwner;
        property Password: String read GetPassword write SetPassword;
        property Salt: Hex read GetSalt write SetSalt;
        property EMail: String read GetEMail write SetEMail;
        property EMailPassword: String read GetEMailPassword write SetEMailPassword;
        property IP: String read GetIP write SetIP;
        property Port: WORD read GetPort write SetPort;
        property Description: String read GetDescription write SetDescription;
        property Sex: Byte read GetSex write SetSex;
        property Birthday: TDateTime read GetBirthday write SetBirthday;
        property HashPic: Hex read GetHashPic write SetHashPic;
        property IDPic: TID read GetIDPic write SetIDPic;
        property Pic: TPic read GetPic write SetPic;
        property PublicKey: String read GetPublicKey write SetPublicKey;
        property PrivateKey: String read GetPrivateKey write SetPrivateKey;
        property TimeOut: WORD read GetTimeOut write SetTimeOut;
        property UseProxy: Boolean read GetUseProxy write SetUseProxy;
        property ProxyIP: String read GetProxyIP write SetProxyIP;
        property ProxyPort: WORD read GetProxyPort write SetProxyPort;
        property ProxyLogin: String read GetProxyLogin write SetProxyLogin;
        property ProxyPassword: String read GetProxyPassword write SetProxyPassword;
        property ProxyProtocol: String read GetProxyProtocol write SetProxyProtocol;
        property SMTPHost: String read GetSMTPHost write SetSMTPHost;
        property SMTPPort: WORD read GetSMTPPort write SetSMTPPort;
        property POP3Host: String read GetPOP3Host write SetPOP3Host;
        property POP3Port: WORD read GetPOP3Port write SetPOP3Port;
        property AutoTLS: Boolean read GetAutoTLS write SetAutoTLS;
        property FullSSL: Boolean read GetFullSSL write SetFullSSL;
    end;
{$M-}
