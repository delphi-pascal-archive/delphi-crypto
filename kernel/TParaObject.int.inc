{******************************************************************************}
{*  TParaObject.int.inc                                                       *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ пара-объект }
{$M+}
    CParaObject = class of TParaObject;
    PParaObject = ^TParaObject;
    TParaObject = class (TMetaObject)
    protected
        function GetIDExternal : String; virtual;
        procedure SetObjectName (const aValue: String); override;
        procedure SetIDExternal (const aValue: String); virtual;
    public
        class function GetClassName : String; override;
        class function GetClassVersion : TVersionInfo; override;
        class function GetClassProperties : TProtoProperties; override;
        class function GetClassList : CProtoObjects; override;
        class function GetClassTable : String; override;
        class procedure CreateClassTable (const aDB: TSQLiteDatabase); override;
    public
        // id, id_external
        constructor Create (const aDB: TSQLiteDatabase;
                            anArgs: array of const;
                            aMode: TObjectMode = []); override;
        destructor Destroy; override;
    public
        function Check : Boolean; override;
        class function Find (const aDB: TSQLiteDatabase;
                             const anIDExternal: String) : TID; overload; virtual;
        function Find : TID; overload; virtual;
        class function Load (const aDB: TSQLiteDatabase;
                             const anIDExternal: String) : TParaObject; overload; virtual;
        procedure Load; override;
        procedure Save; override;
        class procedure Delete (const aDB: TSQLiteDatabase;
                                const anIDExternal: String); overload; virtual;
        procedure Delete; override;
    public
        property IDExternal: String read GetIDExternal write SetIDExternal;
    end;
{$M-}
