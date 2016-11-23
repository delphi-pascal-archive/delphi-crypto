{******************************************************************************}
{*  TCategorie.int.inc                                                        *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ категория }
{$M+}
    CCategorie = class of TCategorie;
    PCategorie = ^TCategorie;
    TCategorie = class (THypoObject)
    private
        f_Parent: TCategorie;
        f_Author: TUser;
        f_CtgType: TCategorieType;
        f_CtgStatus: TCategorieStatus;
        f_Pic: TPic;
    protected
        function CalcKeyHash : Hex; override;
        function CalcDataHash : Hex; override;
    protected
        function GetHashParent : Hex; virtual;
        function GetIDParent : TID; virtual;
        function GetParent : TCategorie; virtual;
        function GetHashAuthor : Hex; virtual;
        function GetIDAuthor : TID; virtual;
        function GetAuthor : TUser; virtual;
        function GetIDOwner : TID; virtual;
        function GetIDType : TID; virtual;
        function GetCtgType : TCategorieType; virtual;
        function GetIDStatus : TID; virtual;
        function GetCtgStatus : TCategorieStatus; virtual;
        function GetName : String; virtual;
        function GetDescription : String; virtual;
        function GetHashPic : Hex; virtual;
        function GetIDPic : TID; virtual;
        function GetPic : TPic; virtual;
        function GetIndexPic : Integer; virtual;
        function GetSalt : Hex; virtual;
        procedure SetHashParent (const aValue: Hex); virtual;
        procedure SetIDParent (const aValue: TID); virtual;
        procedure SetParent (const aValue: TCategorie); virtual;
        procedure SetHashAuthor (const aValue: Hex); virtual;
        procedure SetIDAuthor (const aValue: TID); virtual;
        procedure SetAuthor (const aValue: TUser); virtual;
        procedure SetIDOwner (const aValue: TID); virtual;
        procedure SetIDType (const aValue: TID); virtual;
        procedure SetCtgType (const aValue: TCategorieType); virtual;
        procedure SetIDStatus (const aValue: TID); virtual;
        procedure SetCtgStatus (const aValue: TCategorieStatus); virtual;
        procedure SetName (const aValue: String); virtual;
        procedure SetDescription (const aValue: String); virtual;
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
        // id, id_parent, id_author, id_owner, id_type, id_status, id_pic, index_pic, time_stamp_create, time_stamp_modify, time_stamp_public, version
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
        property HashParent: Hex read GetHashParent write SetHashParent;
        property IDParent: TID read GetIDParent write SetIDParent;
        property Parent: TCategorie read GetParent write SetParent;
        property HashAuthor: Hex read GetHashAuthor write SetHashAuthor;
        property IDAuthor: TID read GetIDAuthor write SetIDAuthor;
        property Author: TUser read GetAuthor write SetAuthor;
        property IDOwner: TID read GetIDOwner write SetIDOwner;
        property IDType: TID read GetIDType write SetIDType;
        property CtgType: TCategorieType read GetCtgType write SetCtgType;
        property IDStatus: TID read GetIDStatus write SetIDStatus;
        property CtgStatus: TCategorieStatus read GetCtgStatus write SetCtgStatus;
        property Name: String read GetName write SetName;
        property Description: String read GetDescription write SetDescription;
        property HashPic: Hex read GetHashPic write SetHashPic;
        property IDPic: TID read GetIDPic write SetIDPic;
        property Pic: TPic read GetPic write SetPic;
        property IndexPic: Integer read GetIndexPic write SetIndexPic;
        property Salt: Hex read GetSalt write SetSalt;
    end;
{$M-}