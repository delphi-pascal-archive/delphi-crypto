{******************************************************************************}
{*  TPics.int.inc                                                             *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ список иконок }
{$M+}
    CPics = class of TPics;
    PPics = ^TPics;
    TPics = class (THypoObjects)
    protected
        function Check (anItem: Pointer) : Boolean; override;
        function Check (anItem: TPic) : Boolean; overload; virtual;
    protected
        function GetItemAt (anIndex: Integer) : TPic; overload; virtual;
        function GetItemOf (aName: String) : TPic; overload; virtual;
        function GetItemOn (anID: TID) : TPic; overload; virtual;
        procedure SetItemAt (anIndex: Integer; anItem: TPic); overload; virtual;
        procedure SetItemOf (aName: String; anItem: TPic); overload; virtual;
        procedure SetItemOn (anID: TID; anItem: TPic); overload; virtual;
    protected
        class function ItemClassType : CMetaObject; override;
    public
        function Add (anItem: TPic) : Integer; overload; virtual;
    protected
        // id, id_owner, time_stamp_create, time_stamp_modify, time_stamp_public, version
        class procedure Filter (var Query: String;
                                anArgs: array of const); override;
        class procedure OnLoad (const aDB: TSQLiteDatabase;
                                anObject: TMetaObject;
                                aTable: TSQLiteTable); override;
    public
        class function Find (const aDB: TSQLiteDatabase;
                             const aHash: String;
                             const anIDOwner: TID) : TID; overload; virtual;
    public
        property ItemAt [anIndex: Integer]: TPic read GetItemAt write SetItemAt;
        property ItemOf [aName: String]: TPic read GetItemOf write SetItemOf;
        property ItemOn [anID: TID]: TPic read GetItemOn write SetItemOn;
    end;
{$M-}