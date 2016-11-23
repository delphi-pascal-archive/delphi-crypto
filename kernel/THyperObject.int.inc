{******************************************************************************}
{*  THyperObject.int.inc                                                      *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit & rat 2011                                       *}
{******************************************************************************}
{ гипер-объект }
{$M+}
    CHyperObject = class of THyperObject;
    PHyperObject = ^THyperObject;
    THyperObject = class (TParaObject)
    protected
        function CalcKeyHash : Hex; overload; virtual;
        function CalcDataHash : Hex; overload; virtual;
    protected
        function GetTimeStampCreate : TDateTime; virtual;
        function GetTimeStampModify : TDateTime; virtual;
        function GetTimeStampPublic : TDateTime; virtual;
        function GetTimeStampVector : Int64; virtual;
        function GetVersion : TVersionInfo; virtual;
        function GetKeyHash : Hex; virtual;
        function GetDataHash : Hex; virtual;
        procedure SetTimeStampCreate (const aValue: TDateTime); virtual;
        procedure SetTimeStampModify (const aValue: TDateTime); virtual;
        procedure SetTimeStampPublic (const aValue: TDateTime); virtual;
        procedure SetTimeStampVector (const aValue: Int64); virtual;
        procedure SetVersion (const aValue: TVersionInfo); virtual;
        procedure SetKeyHash (const aValue: Hex); virtual;
    public
        class function GetClassName : String; override;
        class function GetClassVersion : TVersionInfo; override;
        class function GetClassProperties : TProtoProperties; override;
        class function GetClassList : CProtoObjects; override;
        class function GetClassTable : String; override;
        class procedure CreateClassTable (const aDB: TSQLiteDatabase); override;
    public
        // id, id_external, time_stamp_create, time_stamp_modify, time_stamp_public, version
        constructor Create (const aDB: TSQLiteDatabase;
                            anArgs: array of const;
                            aMode: TObjectMode = []); override;
        destructor Destroy; override;
    public
        function Check : Boolean; override;
        procedure Save; override;
    public
        class function GetHash (const aDB: TSQLiteDatabase;
                                const anID: TID) : Hex; overload; virtual;
    public
        procedure LoadFromStream (const aStream: TStream); override;
        procedure SaveToStream (out aStream: TStream); override;
    public
        property TimeStampCreate: TDateTime read GetTimeStampCreate write SetTimeStampCreate;
        property TimeStampModify: TDateTime read GetTimeStampModify write SetTimeStampModify;
        property TimeStampPublic: TDateTime read GetTimeStampPublic write SetTimeStampPublic;
        property TimeStampVector: Int64 read GetTimeStampVector write SetTimeStampVector;
        property Version: TVersionInfo read GetVersion write SetVersion;
        property KeyHash: Hex read GetKeyHash write SetKeyHash;
        property DataHash: Hex read GetDataHash;
    end;
{$M-}
