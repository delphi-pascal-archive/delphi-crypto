{******************************************************************************}
{*  TDialogFields.int.inc                                                     *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ список полей диалогового окна }
{$M+}
    CDialogFields = class of TDialogFields;
    PDialogFields = ^TDialogFields;
    TDialogFields = class (TProtoProperties)
    protected
        function Check (anItem: Pointer) : Boolean; override;
        function Check (anItem: TDialogField) : Boolean; overload; virtual;
    protected
        function GetItemAt (anIndex: Integer) : TDialogField; virtual;
        function GetItemOf (aName: String) : TDialogField; virtual;
        procedure SetItemAt (anIndex: Integer; anItem: TDialogField); virtual;
        procedure SetItemOf (aName: String; anItem: TDialogField); virtual;
    protected
        class function ItemClassType : CProtoProperty; override;
    public
        function Add (anItem: TDialogField) : Integer; virtual;
    public
        property ItemAt [anIndex: Integer]: TDialogField read GetItemAt write SetItemAt;
        property ItemOf [aName: String]: TDialogField read GetItemOf write SetItemOf;
    end;
{$M-}
