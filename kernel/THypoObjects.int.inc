{******************************************************************************}
{*  THypoObjects.int.inc                                                      *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ список гипо-объектов }
{$M+}
    CHypoObjects = class of THypoObjects;
    PHypoObjects = ^THypoObjects;
    THypoObjects = class (TMetaObjects)
    protected
        function Check (anItem: Pointer) : Boolean; override;
        function Check (anItem: THypoObject) : Boolean; overload; virtual;
    protected
        function GetItemAt (anIndex: Integer) : THypoObject; overload; virtual;
        function GetItemOf (aName: String) : THypoObject; overload; virtual;
        function GetItemOn (anID: TID) : THypoObject; overload; virtual;
        procedure SetItemAt (anIndex: Integer; anItem: THypoObject); overload; virtual;
        procedure SetItemOf (aName: String; anItem: THypoObject); overload; virtual;
        procedure SetItemOn (anID: TID; anItem: THypoObject); overload; virtual;
    protected
        class function ItemClassType : CMetaObject; override;
    public
        function Add (anItem: THypoObject) : Integer; overload; virtual;
    protected
        class procedure Filter (var Query: String;
                                anArgs: array of const); override;
    public
        class function Find (const aDB: TSQLiteDatabase;
                             const aHash: Hex) : TID; overload; virtual;
        class function GetHashes (const aDB: TSQLiteDatabase;
                                  anArgs: array of const) : THashList; overload; virtual;
    public
        property ItemAt [anIndex: Integer]: THypoObject read GetItemAt write SetItemAt;
        property ItemOf [aName: String]: THypoObject read GetItemOf write SetItemOf;
        property ItemOn [anID: TID]: THypoObject read GetItemOn write SetItemOn;
    end;
{$M-}