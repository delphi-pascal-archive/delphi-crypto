{******************************************************************************}
{*  TCryptoObjects.int.inc                                                    *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ список крипто-объектов }
{$M+}
    CCryptoObjects = class of TCryptoObjects;
    PCryptoObjects = ^TCryptoObjects;
    TCryptoObjects = class (TProtoObjects)
    protected
        function Check (anItem: Pointer) : Boolean; override;
        function Check (anItem: TCryptoObject) : Boolean; overload; virtual;
    protected
        function GetItemAt (anIndex: Integer) : TCryptoObject; overload; virtual;
        function GetItemOf (aName: String) : TCryptoObject; overload; virtual;
        procedure SetItemAt (anIndex: Integer; anItem: TCryptoObject); overload; virtual;
        procedure SetItemOf (aName: String; anItem: TCryptoObject); overload; virtual;
    protected
        class function ItemClassType : CProtoObject; override;
    public
        function Add (anItem: TCryptoObject) : Integer; overload; virtual;
    public
        property ItemAt [anIndex: Integer]: TCryptoObject read GetItemAt write SetItemAt;
        property ItemOf [aName: String]: TCryptoObject read GetItemOf write SetItemOf;
    end;
{$M-}