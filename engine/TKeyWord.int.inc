{******************************************************************************}
{*  TKeyWord.int.inc                                                          *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2012                                             *}
{******************************************************************************}
{ ключевое слово }
{$M+}
    CKeyWord = class of TKeyWord;
    PKeyWord = ^TKeyWord;
    TKeyWord = class (TMetaObject)
    protected
        class function CalcHash (const aKeyWord: String) : Hex; overload; virtual;
    protected
        function GetKeyWord : String; virtual;
        function GetHash : Hex; virtual;
        function GetHashAuthor : Hex; virtual;
        function GetIDAuthor : TID; virtual;
        function GetIDOwner : TID; virtual;
        function GetHashCategorie : Hex; virtual;
        function GetIDCategorie : TID; virtual;
        function GetHashMessage : Hex; virtual;
        function GetIDMessage : TID; virtual;
        function GetSalt : Hex; virtual;
        procedure SetKeyWord (const aValue: String); virtual;
        procedure SetHashAuthor (const aValue: Hex); virtual;
        procedure SetIDAuthor (const aValue: TID); virtual;
        procedure SetIDOwner (const aValue: TID); virtual;
        procedure SetHashCategorie (const aValue: Hex); virtual;
        procedure SetIDCategorie (const aValue: TID); virtual;
        procedure SetHashMessage (const aValue: Hex); virtual;
        procedure SetIDMessage (const aValue: TID); virtual;
        procedure SetSalt (const aValue: Hex); virtual;
    public
        class function GetClassName : String; override;
        class function GetClassVersion : TVersionInfo; override;
        class function GetClassProperties : TProtoProperties; override;
        class function GetClassList : CProtoObjects; override;
        class function GetClassTable : String; override;
        class procedure CreateClassTable (const aDB: TSQLiteDatabase); override;
    public
        // id, key_word, id_categorie, id_message, id_author, id_owner
        constructor Create (const aDB: TSQLiteDatabase;
                            anArgs: array of const;
                            aMode: TObjectMode = []); override;
        destructor Destroy; override;
    public
        function Check : Boolean; override;
        procedure Save; override;
    public
        procedure LoadFromStream (const aStream: TStream); override;
        procedure SaveToStream (out aStream: TStream); override;
    public
        property KeyWord: String read GetKeyWord write SetKeyWord;
        property Hash: Hex read GetHash;
        property HashCategorie: Hex read GetHashCategorie write SetHashCategorie;
        property IDCategorie: TID read GetIDCategorie write SetIDCategorie;
        property HashMessage: Hex read GetHashMessage write SetHashMessage;
        property IDMessage: TID read GetIDMessage write SetIDMessage;
        property HashAuthor: Hex read GetHashAuthor write SetHashAuthor;
        property IDAuthor: TID read GetIDAuthor write SetIDAuthor;
        property IDOwner: TID read GetIDOwner write SetIDOwner;
        property Salt: Hex read GetSalt write SetSalt;
    end;
{$M-}