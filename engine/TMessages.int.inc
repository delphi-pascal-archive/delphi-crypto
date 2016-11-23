{******************************************************************************}
{*  TMessages.int.inc                                                         *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ список сообщений }
{$M+}
    CMessages = class of TMessages;
    PMessages = ^TMessages;
    TMessages = class (THypoObjects)
    protected
        function Check (anItem: Pointer) : Boolean; override;
        function Check (anItem: TMessage) : Boolean; overload; virtual;
    protected
        function GetItemAt (anIndex: Integer) : TMessage; overload; virtual;
        function GetItemOf (aName: String) : TMessage; overload; virtual;
        function GetItemOn (anID: TID) : TMessage; overload; virtual;
        procedure SetItemAt (anIndex: Integer; anItem: TMessage); overload; virtual;
        procedure SetItemOf (aName: String; anItem: TMessage); overload; virtual;
        procedure SetItemOn (anID: TID; anItem: TMessage); overload; virtual;
    protected
        class function ItemClassType : CMetaObject; override;
    public
        function Add (anItem: TMessage) : Integer; overload; virtual;
    protected
        // id, id_categorie, id_author, id_owner, id_type, id_status, time_stamp_create, time_stamp_modify, time_stamp_public, version
        class procedure Filter (var Query: String;
                                anArgs: array of const); override;
        class procedure OnLoad (const aDB: TSQLiteDatabase;
                                anObject: TMetaObject;
                                aTable: TSQLiteTable); override;
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
        property ItemAt [anIndex: Integer]: TMessage read GetItemAt write SetItemAt;
        property ItemOf [aName: String]: TMessage read GetItemOf write SetItemOf;
        property ItemOn [anID: TID]: TMessage read GetItemOn write SetItemOn;
    end;
{$M-}