{******************************************************************************}
{*  TMessage.int.inc                                                          *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ сообщение }
{$M+}
    CMessage = class of TMessage;
    PMessage = ^TMessage;
    TMessage = class (THypoObject)
    private
        f_Categorie: TCategorie;
        f_Author: TUser;
        f_MsgType: TMessageType;
        f_MsgStatus: TMessageStatus;
        f_Pic: TPic;
    protected
        function CalcKeyHash : Hex; override;
        function CalcDataHash : Hex; override;
    protected
        function GetHashCategorie : Hex; virtual;
        function GetIDCategorie : TID; virtual;
        function GetCategorie : TCategorie; virtual;
        function GetHashAuthor : Hex; virtual;
        function GetIDAuthor : TID; virtual;
        function GetAuthor : TUser; virtual;
        function GetIDOwner : TID; virtual;
        function GetIDType : TID; virtual;
        function GetMsgType : TMessageType; virtual;
        function GetIDStatus : TID; virtual;
        function GetMsgStatus : TMessageStatus; virtual;
        function GetSubject : String; virtual;
        function GetText : String; virtual;
        function GetHashPic : Hex; virtual;
        function GetIDPic : TID; virtual;
        function GetPic : TPic; virtual;
        function GetIndexPic : Integer; virtual;
        function GetSalt : Hex; virtual;
        procedure SetHashCategorie (const aValue: Hex); virtual;
        procedure SetIDCategorie (const aValue: TID); virtual;
        procedure SetCategorie (const aValue: TCategorie); virtual;
        procedure SetHashAuthor (const aValue: Hex); virtual;
        procedure SetIDAuthor (const aValue: TID); virtual;
        procedure SetAuthor (const aValue: TUser); virtual;
        procedure SetIDOwner (const aValue: TID); virtual;
        procedure SetIDType (const aValue: TID); virtual;
        procedure SetMsgType (const aValue: TMessageType); virtual;
        procedure SetIDStatus (const aValue: TID); virtual;
        procedure SetMsgStatus (const aValue: TMessageStatus); virtual;
        procedure SetSubject (const aValue: String); virtual;
        procedure SetText (const aValue: String); virtual;
        procedure SetHashPic (const aValue: Hex); virtual;
        procedure SetIDPic (const aValue: TID); virtual;
        procedure SetPic (const aValue: TPic); virtual;
        procedure SetIndexPic (const aValue: Integer); virtual;
        procedure SetSalt (const aValue: Hex); virtual;
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
        // id, id_categorie, id_author, id_owner, id_type, id_status, id_pic, index_pic, time_stamp_create, time_stamp_modify, time_stamp_public, version
        constructor Create (const aDB: TSQLiteDatabase;
                            anArgs: array of const;
                            aMode: TObjectMode = []); override;
        destructor Destroy; override;
    public
        function Check : Boolean; override;
        procedure Load; override;
        procedure Save; override;
        {$IFNDEF DELETE_TRIGGER}
        procedure Delete; override;
        {$ENDIF DELETE_TRIGGER}
    public
        procedure LoadFromStream (const aStream: TStream); override;
        procedure SaveToStream (out aStream: TStream); override;
    public
        property HashCategorie: Hex read GetHashCategorie write SetHashCategorie;
        property IDCategorie: TID read GetIDCategorie write SetIDCategorie;
        property Categorie: TCategorie read GetCategorie write SetCategorie;
        property HashAuthor: Hex read GetHashAuthor write SetHashAuthor;
        property IDAuthor: TID read GetIDAuthor write SetIDAuthor;
        property Author: TUser read GetAuthor write SetAuthor;
        property IDOwner: TID read GetIDOwner write SetIDOwner;
        property IDType: TID read GetIDType write SetIDType;
        property MsgType: TMessageType read GetMsgType write SetMsgType;
        property IDStatus: TID read GetIDStatus write SetIDStatus;
        property MsgStatus: TMessageStatus read GetMsgStatus write SetMsgStatus;
        property Subject: String read GetSubject write SetSubject;
        property Text: String read GetText write SetText;
        property HashPic: Hex read GetHashPic write SetHashPic;
        property IDPic: TID read GetIDPic write SetIDPic;
        property Pic: TPic read GetPic write SetPic;
        property IndexPic: Integer read GetIndexPic write SetIndexPic;
        property Salt: Hex read GetSalt write SetSalt;
    end;
{$M-}