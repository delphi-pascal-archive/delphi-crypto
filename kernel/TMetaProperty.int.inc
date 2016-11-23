{******************************************************************************}
{*  TMetaProperty.int.inc                                                     *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ мета-свойство }
{$M+}
    CMetaProperty = class of TMetaProperty;
    PMetaProperty = ^TMetaProperty;
    TMetaProperty = class (TCryptoProperty)
    protected
        class function toDataType (const aValue: TVarRec) : TDataType; override;
        class function toDataValue (const aType: TDataType;
                                    const aValue: TVarRec) : Variant; override;
        class function toDBType (const aType: TDataType) : String; overload; virtual;
        function GetDBtype : String; virtual;
    protected
        function GetAsBoolean : Boolean; override;
        function GetAsInteger : Integer; override;
        function GetAsInt64 : Int64; override;
        function GetAsExtended : Extended; override;
        function GetAsDouble : Double; override;
        function GetAsDateTime : TDateTime; override;
        function GetAsDate : TDateTime; override;
        function GetAsTime : TDateTime; override;
        function GetAsString : String; override;
        function GetAsHex : Hex; override;
        function GetAsVersion : TVersionInfo; override;
        function GetAsStream : TStream; override;
        function GetAsID : TID; virtual;
        function GetAsText : String; virtual;
        function GetAsDBFormat : String; virtual;
        procedure SetAsBoolean (const aValue: Boolean); override;
        procedure SetAsInteger (const aValue: Integer); override;
        procedure SetAsInt64 (const aValue: Int64); override;
        procedure SetAsExtended (const aValue: Extended); override;
        procedure SetAsDouble (const aValue: Double); override;
        procedure SetAsDateTime (const aValue: TDateTime); override;
        procedure SetAsDate (const aValue: TDateTime); override;
        procedure SetAsTime (const aValue: TDateTime); override;
        procedure SetAsString (const aValue: String); override;
        procedure SetAsHex (const aValue: Hex); override;
        procedure SetAsVersion (const aValue: TVersionInfo); override;
        procedure SetAsStream (const aValue: TStream); override;
        procedure SetAsID (const aValue: TID); virtual;
        procedure SetAsText (const aValue: String); virtual;
        procedure SetAsDBFormat (const aValue: String); virtual;
    public
        property DBType: String read GetDBtype;
    public
        property asID: TID read GetAsID write SetAsID;
        property asText: String read GetAsText write SetAsText;
        property asDBFormat: String read GetAsDBFormat write SetAsDBFormat;
    end;
{$M-}