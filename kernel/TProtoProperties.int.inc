{******************************************************************************}
{*  TProtoProperties.int.inc                                                  *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ список прото-свойств }
{$M+}
    TProtoProperties = class (TItems)
    protected
        procedure AssignTo (Dest: TPersistent); override;
    protected
        function Check (anItem: Pointer) : Boolean; override;
        function Check (anItem: TProtoProperty) : Boolean; overload; virtual;
    protected
        function GetItemAt (anIndex: Integer) : TProtoProperty; overload; virtual;
        function GetItemOf (aName: String) : TProtoProperty; overload; virtual;
        procedure SetItemAt (anIndex: Integer; anItem: TProtoProperty); overload; virtual;
        procedure SetItemOf (aName: String; anItem: TProtoProperty); overload; virtual;
    public
        constructor Create (anArgs: array of const); override;
        destructor Destroy; override;
    protected
        class function ItemClassType : CProtoProperty; virtual;
    public
        function Add (anItem: TProtoProperty) : Integer; overload; virtual;
        function Add (anArgs: array of const;
                      aMode: TPropertyMode = []) : Integer; overload; virtual;
        function Register (const anExtraClassType: CProtoProperty;
                           anArgs: array of const;
                           aMode: TPropertyMode = []) : Integer; overload; virtual;
    public
        property ItemAt [anIndex: Integer]: TProtoProperty read GetItemAt write SetItemAt;
        property ItemOf [aName: String]: TProtoProperty read GetItemOf write SetItemOf;
    end;
{$M-}

