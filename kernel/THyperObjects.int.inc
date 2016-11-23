{******************************************************************************}
{*  THyperObjects.int.inc                                                      *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ список гипер-объектов }
{$M+}
    CHyperObjects = class of THyperObjects;
    PHyperObjects = ^THyperObjects;
    THyperObjects = class (TParaObjects)
    protected
        function Check (anItem: Pointer) : Boolean; override;
        function Check (anItem: THyperObject) : Boolean; overload; virtual;
    protected
        function GetItemAt (anIndex: Integer) : THyperObject; overload; virtual;
        function GetItemOf (aName: String) : THyperObject; overload; virtual;
        function GetItemOn (anID: TID) : THyperObject; overload; virtual;
        procedure SetItemAt (anIndex: Integer; anItem: THyperObject); overload; virtual;
        procedure SetItemOf (aName: String; anItem: THyperObject); overload; virtual;
        procedure SetItemOn (anID: TID; anItem: THyperObject); overload; virtual;
    protected
        class function ItemClassType : CMetaObject; override;
    public
        function Add (anItem: THyperObject) : Integer; overload; virtual;
    protected
        class procedure Filter (var Query: String;
                                anArgs: array of const); override;
    public
        class function Find (const aDB: TSQLiteDatabase;
                             const aHash: Hex) : TID; overload; virtual;
        class function GetHashes (const aDB: TSQLiteDatabase;
                                  anArgs: array of const) : THashList; overload; virtual;
    public
        property ItemAt [anIndex: Integer]: THyperObject read GetItemAt write SetItemAt;
        property ItemOf [aName: String]: THyperObject read GetItemOf write SetItemOf;
        property ItemOn [anID: TID]: THyperObject read GetItemOn write SetItemOn;
    end;
{$M-}