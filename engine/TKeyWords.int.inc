{******************************************************************************}
{*  TKeyWords.int.inc                                                         *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2012                                             *}
{******************************************************************************}
{ список сообщений }
{$M+}
    CKeyWords = class of TKeyWords;
    PKeyWords = ^TKeyWords;
    TKeyWords = class (TMetaObjects)
    protected
        function Check (anItem: Pointer) : Boolean; override;
        function Check (anItem: TKeyWord) : Boolean; overload; virtual;
    protected
        function GetItemAt (anIndex: Integer) : TKeyWord; overload; virtual;
        function GetItemOn (anID: TID) : TKeyWord; overload; virtual;
        procedure SetItemAt (anIndex: Integer; anItem: TKeyWord); overload; virtual;
        procedure SetItemOn (anID: TID; anItem: TKeyWord); overload; virtual;
    protected
        class function ItemClassType : CMetaObject; override;
    public
        function Add (anItem: TKeyWord) : Integer; overload; virtual;
    protected
        // id, key_word, id_categorie, id_message, id_author, id_owner
        class procedure Filter (var Query: String;
                                anArgs: array of const); override;
    public
        property ItemAt [anIndex: Integer]: TKeyWord read GetItemAt write SetItemAt;
        property ItemOn [anID: TID]: TKeyWord read GetItemOn write SetItemOn;
    end;
{$M-}