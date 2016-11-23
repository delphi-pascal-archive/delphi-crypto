{******************************************************************************}
{*  TPackage.int.inc                                                          *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011-2012                                        *}
{******************************************************************************}
{ пакет }
{
  - структура пакетов :
  [PACKAGE_LENGTH]
  [VERSION_LENGTH][VERSION]
  [SENDER_HASH_LENGTH][SENDER_HASH]
  [RECEIVER_HASH_LENGTH][RECEIVER_HASH]
  [TIME_STAMP_CREATE_LENGTH][TIME_STAMP_CREATE]
  [TIME_STAMP_SEND_LENGTH][TIME_STAMP_SEND]
  [TYPE_LENGTH][TYPE]
  [META_CLASS_ID_LENGTH][META_CLASS_ID]
  [META_DATA_LENGTH][META_DATA]
}
{$M+}
    CPackage = class of TPackage;
    PPackage = ^TPackage;
    TPackage = class (TMetaObject)
    private
        f_Sender: TUser;
        f_Receiver: TUser;
        f_PckType: TPackageType;
        f_PckStatus: TPackageStatus;
        f_MetaClass: CMetaObject;
    protected
        function CalcKeyHash : Hex; overload; virtual;
        function CalcDataHash : Hex; overload; virtual;
    protected
        function GetIDSender : TID; virtual;
        function GetSender : TUser; virtual;
        function GetSenderHash : Hex; virtual;
        function GetIDReceiver : TID; virtual;
        function GetReceiver : TUser; virtual;
        function GetReceiverHash : Hex; virtual;
        function GetTimeStampCreate : TDateTime; virtual;
        function GetTimeStampSend : TDateTime; virtual;
        function GetTimeStampReceive : TDateTime; virtual;
        function GetIDOwner : TID; virtual;
        function GetIDType : TID; virtual;
        function GetPckType : TPackageType; virtual;
        function GetIDStatus : TID; virtual;
        function GetPckStatus : TPackageStatus; virtual;
        function GetMetaClassID : String; virtual;
        function GetMetaClass : CMetaObject; virtual;
        function GetMetaData : TStream; virtual;
        function GetVersion : TVersionInfo; virtual;
        function GetKeyHash : Hex; virtual;
        function GetDataHash : Hex; virtual;
        procedure SetIDSender (const aValue: TID); virtual;
        procedure SetSender (const aValue: TUser); virtual;
        procedure SetSenderHash (const aValue: Hex); virtual;
        procedure SetIDReceiver (const aValue: TID); virtual;
        procedure SetReceiver (const aValue: TUser); virtual;
        procedure SetReceiverHash (const aValue: Hex); virtual;
        procedure SetTimeStampCreate (const aValue: TDateTime); virtual;
        procedure SetTimeStampSend (const aValue: TDateTime); virtual;
        procedure SetTimeStampReceive (const aValue: TDateTime); virtual;
        procedure SetIDOwner (const aValue: TID); virtual;
        procedure SetIDType (const aValue: TID); virtual;
        procedure SetPckType (const aValue: TPackageType); virtual;
        procedure SetIDStatus (const aValue: TID); virtual;
        procedure SetPckStatus (const aValue: TPackageStatus); virtual;
        procedure SetMetaClassID (const aValue: String); virtual;
        procedure SetMetaClass (const aValue: CMetaObject); virtual;
        procedure SetMetaData (const aValue: TStream); virtual;
        procedure SetVersion (const aValue: TVersionInfo); virtual;
        procedure SetKeyHash (const aValue: Hex); virtual;
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
        // id, id_sender, id_receiver, id_owner, id_type, id_status, meta_class_type, version
        constructor Create (const aDB: TSQLiteDatabase;
                            anArgs: array of const;
                            aMode: TObjectMode = []); override;
        destructor Destroy; override;
    public
        function Check : Boolean; override;
        procedure Load; override;
        procedure Save; override;
    public
        class function GetMetaClassByID (const aMetaClassID: String) : CMetaObject; overload; virtual;
    protected
        procedure WriteMetaData (const anInput: TStream); virtual;
        procedure ReadMetaData (out anOutput: TStream); virtual;
        function GetHashList : THashList; virtual;
        function FindMetaObject (const aHash: Hex) : TMetaObject; virtual;
        class procedure WriteMetaObject (const anObject: TMetaObject;
                                         out aStream: TStream); virtual;
        class procedure ReadMetaObject (const aStream: TStream;
                                        out anObject: TMetaObject); virtual;
    public
        procedure CreateMetaData (anArgs: array of const); overload; virtual;
        procedure ProcessMetaData; virtual;
    public
        procedure LoadFromStream (const aStream: TStream); override;
        procedure SaveToStream (out aStream: TStream); override;
    public
        property IDSender: TID read GetIDSender write SetIDSender;
        property Sender: TUser read GetSender write SetSender;
        property SenderHash: Hex read GetSenderHash write SetSenderHash;
        property IDReceiver: TID read GetIDReceiver write SetIDReceiver;
        property Receiver: TUser read GetReceiver write SetReceiver;
        property ReceiverHash: Hex read GetReceiverHash write SetReceiverHash;
        property TimeStampCreate: TDateTime read GetTimeStampCreate write SetTimeStampCreate;
        property TimeStampSend: TDateTime read GetTimeStampSend write SetTimeStampSend;
        property TimeStampReceive: TDateTime read GetTimeStampReceive write SetTimeStampReceive;
        property IDOwner: TID read GetIDOwner write SetIDOwner;
        property IDType: TID read GetIDType write SetIDType;
        property PckType: TPackageType read GetPckType write SetPckType;
        property IDStatus: TID read GetIDStatus write SetIDStatus;
        property PckStatus: TPackageStatus read GetPckStatus write SetPckStatus;
        property MetaClassID: String read GetMetaClassID write SetMetaClassID;
        property MetaClass: CMetaObject read GetMetaClass write SetMetaClass;
        property MetaData: TStream read GetMetaData write SetMetaData;
        property Version: TVersionInfo read GetVersion write SetVersion;
        property KeyHash: Hex read GetKeyHash write SetKeyHash;
        property DataHash: Hex read GetDataHash;
    end;
{$M-}