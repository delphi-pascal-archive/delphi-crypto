{******************************************************************************}
{*  TPackageType.int.inc                                                      *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ тип пакета }
{$M+}
    CPackageType = class of TPackageType;
    PPackageType = ^TPackageType;
    TPackageType = class (TParaObject)
    protected
        function GetDescription : String; virtual;
        procedure SetDescription (const aValue: String); virtual;
    public
        class function GetClassName : String; override;
        class function GetClassVersion : TVersionInfo; override;
        class function GetClassProperties : TProtoProperties; override;
        class function GetClassTable : String; override;
        class procedure CreateClassTable (const aDB: TSQLiteDatabase); override;
    public
        // id, name, description
        constructor Create (const aDB: TSQLiteDatabase;
                            anArgs: array of const;
                            aMode: TObjectMode = []); override;
        destructor Destroy; override;
    public
        function Check : Boolean; override;
        {$IFNDEF DELETE_TRIGGER}
        procedure Delete; override;
        {$ENDIF DELETE_TRIGGER}
    protected
        property IDExternal;
    public
        property Name: String read GetIDExternal write SetIDExternal;
        property Description: String read GetDescription write SetDescription;
    end;
{$M-}