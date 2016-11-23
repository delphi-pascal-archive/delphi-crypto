{******************************************************************************}
{*  THashList.int.inc                                                         *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ хэш-таблица }
{$M+}
    CHashList = class of THashList;
    PHashList = ^THashList;
    THashList = class (TCryptoObjects)
    protected
        function CalcMercleKeyHash : Hex; overload; virtual;
        function CalcMercleDataHash : Hex; overload; virtual;
    protected
        function Check (anItem: Pointer) : Boolean; override;
        function Check (anItem: THashItem) : Boolean; overload; virtual;
    protected
        function GetItemAt (anIndex: Integer) : THashItem; overload; virtual;
        function GetItemOf (aName: String) : THashItem; overload; virtual;
        procedure SetItemAt (anIndex: Integer; anItem: THashItem); overload; virtual;
        procedure SetItemOf (aName: String; anItem: THashItem); overload; virtual;
    protected
        class function ItemClassType : CProtoObject; override;
    public
        function Add (anItem: THashItem) : Integer; overload; virtual;
    protected
        class function Compare (anItemA, anItemB: Pointer) : Integer; override;
    public
        class function Substract (const aListA, aListB: THashList) : THashList; virtual;
        class function Extract (const aListA, aListB: THashList) : THashList; virtual;
    public
        procedure LoadFromStream (const aStream: TStream); virtual;
        procedure SaveToStream (out aStream: TStream); virtual;
    public
        property ItemAt [anIndex: Integer]: THashItem read GetItemAt write SetItemAt;
        property ItemOf [aName: String]: THashItem read GetItemOf write SetItemOf;
        property MercleKeyHash: Hex read CalcMercleKeyHash;
        property MercleDataHash: Hex read CalcMercleDataHash;
    end;
{$M-}