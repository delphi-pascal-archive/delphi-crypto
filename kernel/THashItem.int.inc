{******************************************************************************}
{*  THashItem.int.inc                                                         *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ элемент хэш-таблицы }
{$M+}
    CHashItem = class of THashItem;
    PHashItem = ^THashItem;
    THashItem = class (TCryptoObject)
    protected
        function RegisterProperty (anArgs: array of const;
                                   aMode: TPropertyMode = []) : Integer; override;
    protected
        function GetPropAt (anIndex: Integer) : TMetaProperty; overload; virtual;
        function GetPropOf (aName: String) : TMetaProperty; overload; virtual;
        procedure SetPropAt (anIndex: Integer; anItem: TMetaProperty); overload; virtual;
        procedure SetPropOf (aName: String; anItem: TMetaProperty); overload; virtual;
    protected
        function GetKeyHash : Hex; virtual;
        function GetDataHash : Hex; virtual;
        function GetTimeStampVector : Int64;
        procedure SetKeyHash (const aValue: Hex); virtual;
        procedure SetDataHash (const aValue: Hex); virtual;
        procedure SetTimeStampVector (const aValue: Int64); virtual;
    public
        class function GetClassName : String; override;
        class function GetClassVersion : TVersionInfo; override;
        class function GetClassProperties : TProtoProperties; override;
    public
        // key_hash, data_hash, time_stamp_vector
        constructor Create (anArgs: array of const;
                            aMode: TObjectMode = []); override;
        destructor Destroy; override;
    public
        function Check : Boolean; override;
    public
        property PropAt [anIndex: Integer]: TMetaProperty read GetPropAt write SetPropAt;
        property PropOf [aName: String]: TMetaProperty read GetPropOf write SetPropOf;
        property KeyHash: Hex read GetKeyHash write SetKeyHash;
        property DataHash: Hex read GetDataHash write SetDataHash;
        property TimeStampVector: Int64 read GetTimeStampVector write SetTimeStampVector;
    end;
{$M-}