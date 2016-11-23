{******************************************************************************}
{*  TParaObjects.int.inc                                                      *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ список пара-объектов }
{$M+}
    CParaObjects = class of TParaObjects;
    PParaObjects = ^TParaObjects;
    TParaObjects = class (TMetaObjects)
    protected
        function Check (anItem: Pointer) : Boolean; override;
        function Check (anItem: TParaObject) : Boolean; overload; virtual;
    protected
        function GetItemAt (anIndex: Integer) : TParaObject; overload; virtual;
        function GetItemOf (aName: String) : TParaObject; overload; virtual;
        function GetItemOn (anID: TID) : TParaObject; overload; virtual;
        procedure SetItemAt (anIndex: Integer; anItem: TParaObject); overload; virtual;
        procedure SetItemOf (aName: String; anItem: TParaObject); overload; virtual;
        procedure SetItemOn (anID: TID; anItem: TParaObject); overload; virtual;
    protected
        class function ItemClassType : CMetaObject; override;
    public
        function Add (anItem: TParaObject) : Integer; overload; virtual;
        function AddLoad (const anIDExternal: String) : Integer; overload; virtual;
    protected
        class procedure Filter (var Query: String;
                                anArgs: array of const); override;
        class procedure OnLoad (const aDB: TSQLiteDatabase;
                                anObject: TMetaObject;
                                aTable: TSQLiteTable); override;
    public
        property ItemAt [anIndex: Integer]: TParaObject read GetItemAt write SetItemAt;
        property ItemOf [aName: String]: TParaObject read GetItemOf write SetItemOf;
        property ItemOn [anID: TID]: TParaObject read GetItemOn write SetItemOn;
    end;
{$M-}