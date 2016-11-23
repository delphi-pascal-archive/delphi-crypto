{******************************************************************************}
{*  TSession.int.inc                                                          *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2012                                             *}
{******************************************************************************}
{ параметр сессии }
{$M+}
    CSession = class of TSession;
    PSession = ^TSession;
    TSession = class (TMetaObject)
    protected
        function GetName : String; virtual;
        function GetValue : String; virtual;
        function GetTimeStampStart : TDateTime; virtual;
        function GetTimeStampFinish : TDateTime; virtual;
        function GetIDOwner : TID; virtual;
        function GetSalt : Hex; virtual;
        procedure SetName (const aValue: String); virtual;
        procedure SetValue (const aValue: String); virtual;
        procedure SetTimeStampStart (const aValue: TDateTime); virtual;
        procedure SetTimeStampFinish (const aValue: TDateTime); virtual;
        procedure SetIDOwner (const aValue: TID); virtual;
        procedure SetSalt (const aValue: Hex); virtual;
    public
        class function GetClassName : String; override;
        class function GetClassVersion : TVersionInfo; override;
        class function GetClassProperties : TProtoProperties; override;
        class function GetClassList : CProtoObjects; override;
        class function GetClassTable : String; override;
    public
        // id, name, id_owner, time_stamp_start, time_stamp_finish
        constructor Create (const aDB: TSQLiteDatabase;
                            anArgs: array of const;
                            aMode: TObjectMode = []); override;
        destructor Destroy; override;
    public
        function Check : Boolean; override;
    public
        property Name: String read GetName write SetName;
        property Value: String read GetValue write SetValue;
        property TimeStampStart: TDateTime read GetTimeStampStart write SetTimeStampStart;
        property TimeStampFinish: TDateTime read GetTimeStampFinish write SetTimeStampFinish;
        property IDOwner: TID read GetIDOwner write SetIDOwner;
        property Salt: Hex read GetSalt write SetSalt;
    end;
{$M-}