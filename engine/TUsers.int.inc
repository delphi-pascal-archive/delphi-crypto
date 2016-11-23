{******************************************************************************}
{*  TUsers.int.inc                                                            *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011-2012                                        *}
{******************************************************************************}
{ список пользователей }
{$M+}
    CUsers = class of TUsers;
    PUsers = ^TUsers;
    TUsers = class (THyperObjects)
    protected
        function Check (anItem: Pointer) : Boolean; override;
        function Check (anItem: TUser) : Boolean; overload; virtual;
    protected
        function GetItemAt (anIndex: Integer) : TUser; overload; virtual;
        function GetItemOf (aName: String) : TUser; overload; virtual;
        function GetItemOn (anID: TID) : TUser; overload; virtual;
        procedure SetItemAt (anIndex: Integer; anItem: TUser); overload; virtual;
        procedure SetItemOf (aName: String; anItem: TUser); overload; virtual;
        procedure SetItemOn (anID: TID; anItem: TUser); overload; virtual;
    protected
        class function ItemClassType : CMetaObject; override;
    public
        function Add (anItem: TUser) : Integer; overload; virtual;
    protected
        // id, id_external (login), id_owner, time_stamp_create, time_stamp_modify, time_stamp_public, version
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
        {$IFNDEF DELETE_TRIGGER}
        class procedure Delete (const aDB: TSQLiteDatabase;
                                anArgs: array of const); override;
        {$ENDIF DELETE_TRIGGER}
        class procedure ReplacePic (const aDB: TSQLiteDatabase;
                                    const aHashPic: Hex;
                                    const aNewIDPic: TID;
                                    const anIDOwner: TID); virtual;
    public
        property ItemAt [anIndex: Integer]: TUser read GetItemAt write SetItemAt;
        property ItemOf [aName: String]: TUser read GetItemOf write SetItemOf;
        property ItemOn [anID: TID]: TUser read GetItemOn write SetItemOn;
    end;
{$M-}