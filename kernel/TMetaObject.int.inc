{******************************************************************************}
{*  TMetaObject.int.inc                                                       *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ мета-объект }
{$M+}
    CMetaObject = class of TMetaObject;
    PMetaObject = ^TMetaObject;
    TMetaObject = class (TCryptoObject)
    private
        f_DB: TSQLiteDatabase; { объект базы данных }
    protected
        function RegisterProperty (anArgs: array of const;
                                   aMode: TPropertyMode = []) : Integer; override;
    protected
        function GetPropAt (anIndex: Integer) : TMetaProperty; overload; virtual;
        function GetPropOf (aName: String) : TMetaProperty; overload; virtual;
        procedure SetPropAt (anIndex: Integer; anItem: TMetaProperty); overload; virtual;
        procedure SetPropOf (aName: String; anItem: TMetaProperty); overload; virtual;
    protected
        function GetID : TID; virtual;
        procedure SetID (const aValue: TID); virtual;
    public
        class function GetClassName : String; override;
        class function GetClassVersion : TVersionInfo; override;
        class function GetClassProperties : TProtoProperties; override;
        class function GetClassList : CProtoObjects; override;
        class function GetClassTable : String; virtual;
        class procedure CreateClassTable (const aDB: TSQLiteDatabase); overload; virtual;
        procedure CreateClassTable; overload; virtual;
    protected
        procedure SetDB (const aDB: TSQLiteDatabase); virtual;
    public
        // id
        constructor Create (const aDB: TSQLiteDatabase;
                            anArgs: array of const;
                            aMode: TObjectMode = []); virtual;
        destructor Destroy; override;
    public
        function Check : Boolean; override;
        class function Load (const aDB: TSQLiteDatabase;
                             const anID: TID;
                             aMode: TObjectMode = []) : TMetaObject; overload; virtual;
        procedure Load; overload; virtual;
        class function Save (const aDB: TSQLiteDatabase;
                             anArgs: array of const;
                             aMode: TObjectMode = []) : TID; overload; virtual;
        procedure Save; overload; virtual;
        class procedure Delete (const aDB: TSQLiteDatabase;
                                const anID: TID;
                                aMode: TObjectMode = []); overload; virtual;
        procedure Delete; overload; virtual;
    public
        procedure LoadFromStream (const aStream: TStream); virtual;
        procedure SaveToStream (out aStream: TStream); virtual;
    public
        property DB: TSQLiteDatabase read f_DB write SetDB;
        property PropAt [anIndex: Integer]: TMetaProperty read GetPropAt write SetPropAt;
        property PropOf [aName: String]: TMetaProperty read GetPropOf write SetPropOf;
        property ID: TID read GetID write SetID;
    end;
{$M-}