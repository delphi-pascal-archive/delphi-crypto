{******************************************************************************}
{*  TCryptoObject.int.inc                                                     *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ крипто-объект }
{$M+}
    CCryptoObject = class of TCryptoObject;
    PCryptoObject = ^TCryptoObject;
    TCryptoObject = class (TProtoObject)
    protected
        function RegisterProperty (anArgs: array of const;
                                   aMode: TPropertyMode = []) : Integer; override;
    protected
        function GetPropAt (anIndex: Integer) : TCryptoProperty; overload; virtual;
        function GetPropOf (aName: String) : TCryptoProperty; overload; virtual;
        procedure SetPropAt (anIndex: Integer; anItem: TCryptoProperty); overload; virtual;
        procedure SetPropOf (aName: String; anItem: TCryptoProperty); overload; virtual;
    public
        class function GetClassName : String; override;
        class function GetClassVersion : TVersionInfo; override;
    public
        property PropAt [anIndex: Integer]: TCryptoProperty read GetPropAt write SetPropAt;
        property PropOf [aName: String]: TCryptoProperty read GetPropOf write SetPropOf;
    end;
{$M-}