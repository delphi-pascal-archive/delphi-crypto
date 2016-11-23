{******************************************************************************}
{*  TPackages.int.inc                                                         *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ список пакетов }
{$M+}
    CPackages = class of TPackages;
    PPackages = ^TPackages;
    TPackages = class (TMetaObjects)
    protected
        function Check (anItem: Pointer) : Boolean; override;
        function Check (anItem: TPackage) : Boolean; overload; virtual;
    protected
        function GetItemAt (anIndex: Integer) : TPackage; overload; virtual;
        function GetItemOf (aName: String) : TPackage; overload; virtual;
        function GetItemOn (anID: TID) : TPackage; overload; virtual;
        procedure SetItemAt (anIndex: Integer; anItem: TPackage); overload; virtual;
        procedure SetItemOf (aName: String; anItem: TPackage); overload; virtual;
        procedure SetItemOn (anID: TID; anItem: TPackage); overload; virtual;
    protected
        class function ItemClassType : CMetaObject; override;
    public
        function Add (anItem: TPackage) : Integer; overload; virtual;
    protected
        // id, sender_hash, receiver_hash, id_owner, id_type, id_status, meta_class_type, time_stamp_create, time_stamp_send, time_stamp_receive, version
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
        property ItemAt [anIndex: Integer]: TPackage read GetItemAt write SetItemAt;
        property ItemOf [aName: String]: TPackage read GetItemOf write SetItemOf;
        property ItemOn [anID: TID]: TPackage read GetItemOn write SetItemOn;
    end;
{$M-}
