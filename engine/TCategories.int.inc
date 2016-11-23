{******************************************************************************}
{*  TCategories.int.inc                                                       *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ список категорий }
{$M+}
    CCategories = class of TCategories;
    PCategories = ^TCategories;
    TCategories = class (THypoObjects)
    protected
        function Check (anItem: Pointer) : Boolean; override;
        function Check (anItem: TCategorie) : Boolean; overload; virtual;
    protected
        function GetItemAt (anIndex: Integer) : TCategorie; overload; virtual;
        function GetItemOf (aName: String) : TCategorie; overload; virtual;
        function GetItemOn (anID: TID) : TCategorie; overload; virtual;
        procedure SetItemAt (anIndex: Integer; anItem: TCategorie); overload; virtual;
        procedure SetItemOf (aName: String; anItem: TCategorie); overload; virtual;
        procedure SetItemOn (anID: TID; anItem: TCategorie); overload; virtual;
    protected
        class function ItemClassType : CMetaObject; override;
    public
        function Add (anItem: TCategorie) : Integer; overload; virtual;
    protected
        // id, id_parent, id_author, id_owner, id_type, id_status, time_stamp_create, time_stamp_modify, time_stamp_public, version
        class procedure Filter (var Query: String;
                                anArgs: array of const); override;
        class procedure OnLoad (const aDB: TSQLiteDatabase;
                                anObject: TMetaObject;
                                aTable: TSQLiteTable); override;
        {$IFNDEF DELETE_TRIGGER}
        class procedure DeleteByParent (const aDB: TSQLiteDatabase;
                                        const aParentID: TID); virtual;
        {$ENDIF DELETE_TRIGGER}
    public
        {$IFNDEF DELETE_TRIGGER}
        class procedure Delete (const aDB: TSQLiteDatabase;
                                anArgs: array of const); override;
        {$ENDIF DELETE_TRIGGER}
        class function Find (const aDB: TSQLiteDatabase;
                             const aHash: String;
                             const anIDOwner: TID) : TID; overload; virtual;
        class procedure ReplacePic (const aDB: TSQLiteDatabase;
                                    const aHashPic: Hex;
                                    const aNewIDPic: TID;
                                    const anIDOwner: TID); virtual;
    public
        property ItemAt [anIndex: Integer]: TCategorie read GetItemAt write SetItemAt;
        property ItemOf [aName: String]: TCategorie read GetItemOf write SetItemOf;
        property ItemOn [anID: TID]: TCategorie read GetItemOn write SetItemOn;
    end;
{$M-}