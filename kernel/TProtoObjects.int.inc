{******************************************************************************}
{*  TProtoObjects.int.inc                                                     *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ список прото-объектов }
{$M+}
    //CProtoObjects = class of TProtoObjects;
    PProtoObjects = ^TProtoObjects;
    TProtoObjects = class (TItems)
    protected
        function Check (anItem: Pointer) : Boolean; override;
        function Check (anItem: TProtoObject) : Boolean; overload; virtual;
    protected
        function GetItemAt (anIndex: Integer) : TProtoObject; overload; virtual;
        function GetItemOf (aName: String) : TProtoObject; overload; virtual;
        procedure SetItemAt (anIndex: Integer; anItem: TProtoObject); overload; virtual;
        procedure SetItemOf (aName: String; anItem: TProtoObject); overload; virtual;
    public
        constructor Create (anArgs: array of const); override;
        destructor Destroy; override;
    protected
        class function ItemClassType : CProtoObject; virtual;
    public
        function Add (anItem: TProtoObject) : Integer; overload; virtual;
        function Add (anArgs: array of const;
                      aMode: TObjectMode = []) : Integer; overload; virtual;
    public
        property ItemAt [anIndex: Integer]: TProtoObject read GetItemAt write SetItemAt;
        property ItemOf [aName: String]: TProtoObject read GetItemOf write SetItemOf;
    end;
{$M-}
