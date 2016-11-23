{******************************************************************************}
{*  TMessageStatus.int.inc                                                    *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ статус сообщения }
{$M+}
    CMessageStatus = class of TMessageStatus;
    PMessageStatus = ^TMessageStatus;
    TMessageStatus = class (TParaObject)
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
        procedure Delete; override;
    protected
        property IDExternal;
    public
        property Name: String read GetIDExternal write SetIDExternal;
        property Description: String read GetDescription write SetDescription;
    end;
{$M-}