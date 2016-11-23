{******************************************************************************}
{*  TMetaObjects.int.inc                                                      *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ список мета-объектов }
{$M+}
    CMetaObjects = class of TMetaObjects;
    PMetaObjects = ^TMetaObjects;
    TMetaObjects = class (TCryptoObjects)
    private
        f_DB: TSQLiteDatabase; { объект базы данных }
    protected
        function Check (anItem: Pointer) : Boolean; override;
        function Check (anItem: TMetaObject) : Boolean; overload; virtual;
    protected
        function GetItemAt (anIndex: Integer) : TMetaObject; overload; virtual;
        function GetItemOf (aName: String) : TMetaObject; overload; virtual;
        function GetItemOn (anID: TID) : TMetaObject; overload; virtual;
        procedure SetItemAt (anIndex: Integer; anItem: TMetaObject); overload; virtual;
        procedure SetItemOf (aName: String; anItem: TMetaObject); overload; virtual;
        procedure SetItemOn (anID: TID; anItem: TMetaObject); overload; virtual;
    public
        constructor Create (const aDB: TSQLiteDatabase;
                            anArgs: array of const); overload; virtual;
        destructor Destroy; override;
    protected
        class function ItemClassType : CMetaObject; virtual;
    public
        function Add (anItem: TMetaObject) : Integer; overload; virtual;
        function Add (anArgs: array of const;
                      aMode: TObjectMode = []) : Integer; override;
        function AddLoad (const anID: TID) : Integer; overload; virtual;
        function AddSave (anArgs: array of const;
                          aMode: TObjectMode = []) : Integer; overload; virtual;
    protected
        class procedure Filter (var Query: String;
                                anArgs: array of const); overload; virtual;
        class procedure Order (var Query: String;
                               anArgs: array of const); overload; virtual;
        class procedure Limit (var Query: String;
                               anArgs: array of const); overload; virtual;
        class procedure OnLoad (const aDB: TSQLiteDatabase;
                                anObject: TMetaObject;
                                aTable: TSQLiteTable); overload; virtual;
    public
        class function GetCount (const aDB: TSQLiteDatabase;
                                 anArgs: array of const) : Integer; overload; virtual;
        class function Load (const aDB: TSQLiteDatabase;
                             anArgs: array of const;
                             anOrder: array of const;
                             aLimit: array of const;
                             aMode: TObjectMode = []) : TMetaObjects; overload; virtual;
        class function Load (const aDB: TSQLiteDatabase;
                             anArgs: array of const;
                             anOrder: array of const) : TMetaObjects; overload; virtual;
        class function Load (const aDB: TSQLiteDatabase;
                             anArgs: array of const) : TMetaObjects; overload; virtual;
        procedure Load; overload; virtual;
        procedure Save; overload; virtual;
        class procedure Delete (const aDB: TSQLiteDatabase;
                                anArgs: array of const); overload; virtual;
        procedure Delete; overload; virtual;
    public
        procedure LoadFromStream (const aStream: TStream); virtual;
        procedure SaveToStream (out aStream: TStream); virtual;
    public
        property DB: TSQLiteDatabase read f_DB write f_DB;
        property ItemAt [anIndex: Integer]: TMetaObject read GetItemAt write SetItemAt;
        property ItemOf [aName: String]: TMetaObject read GetItemOf write SetItemOf;
        property ItemOn [anID: TID]: TMetaObject read GetItemOn write SetItemOn;
    end;
{$M-}
