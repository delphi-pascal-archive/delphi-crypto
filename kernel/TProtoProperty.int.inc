{******************************************************************************}
{*  TProtoProperty.int.inc                                                    *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ прото-свойство }
{$M+}
    CProtoProperty = class of TProtoProperty;
    PProtoProperty = ^TProtoProperty;
    TProtoProperty = class (TPersistent)
    protected
        procedure AssignTo (Dest: TPersistent); override;
    public
        class procedure _raise (anArgs: array of const;
                                const anEGUID: String = ''); overload; virtual;
        class procedure _raise (anArgs: array of const;
                                anEGUID: array of const); overload; virtual;
    private
        f_Mode: TPropertyMode;
        f_Name: String;
        f_Caption: String;
        f_DataType: TDataType;
        f_Value: Variant;
        f_ReadOnly: Boolean;
        f_Version: TVersionInfo;
    protected
        class function toDataType (const aValue: TVarRec) : TDataType; virtual;
        class function toDataValue (const aType: TDataType;
                                    const aValue: TVarRec) : Variant; virtual;
    protected
        function GetReadOnly : Boolean; virtual;
        procedure SetReadOnly (const aValue: Boolean); virtual;
    protected
        function GetAsVariant : Variant; virtual;
        function GetAsBoolean : Boolean; virtual;
        function GetAsInteger : Integer; virtual;
        function GetAsInt64 : Int64; virtual;
        function GetAsExtended : Extended; virtual;
        function GetAsDouble : Double; virtual;
        function GetAsDateTime : TDateTime; virtual;
        function GetAsDate : TDateTime; virtual;
        function GetAsTime : TDateTime; virtual;
        function GetAsString : String; virtual;
        function GetAsHex : Hex; virtual;
        function GetAsVersion : TVersionInfo; virtual;
        function GetAsPointer : Pointer; virtual;
        function GetAsClass : TClass; virtual;
        function GetAsObject : TObject; virtual;
        function GetAsStream : TStream; virtual;
        procedure SetAsVariant (const aValue: Variant); virtual;
        procedure SetAsBoolean (const aValue: Boolean); virtual;
        procedure SetAsInteger (const aValue: Integer); virtual;
        procedure SetAsInt64 (const aValue: Int64); virtual;
        procedure SetAsExtended (const aValue: Extended); virtual;
        procedure SetAsDouble (const aValue: Double); virtual;
        procedure SetAsDateTime (const aValue: TDateTime); virtual;
        procedure SetAsDate (const aValue: TDateTime); virtual;
        procedure SetAsTime (const aValue: TDateTime); virtual;
        procedure SetAsString (const aValue: String); virtual;
        procedure SetAsHex (const aValue: Hex); virtual;
        procedure SetAsVersion (const aValue: TVersionInfo); virtual;
        procedure SetAsPointer (const aValue: Pointer); virtual;
        procedure SetAsClass (const aValue: TClass); virtual;
        procedure SetAsObject (const aValue: TObject); virtual;
        procedure SetAsStream (const aValue: TStream); virtual;
    public
        constructor Create (aName: String;
                            aCaption: String;
                            aType: TDataType;
                            aValue: TVarRec;
                            aReadOnly: Boolean = FALSE;
                            aVersion: String = NULL_VERSION_STRING;
                            aMode: TPropertyMode = []); overload; virtual;
        constructor Create (anArgs: array of const;
                            aMode: TPropertyMode = []); overload; virtual;
        destructor Destroy; override;
    public
        property Name: String read f_Name write f_Name;
        property Caption: String read f_Caption write f_Caption;
        property DataType: Integer read f_DataType write f_DataType;
        property Value: Variant read f_Value write f_Value;
        property Mode: TPropertyMode read f_Mode write f_Mode;
        property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
        property Version: TVersionInfo read f_Version write f_Version;
    public
        property asVariant: Variant read GetAsVariant write SetAsVariant;
        property asBoolean: Boolean read GetAsBoolean write SetAsBoolean;
        property asInteger: Integer read GetAsInteger write SetAsInteger;
        property asInt64: Int64 read GetAsInt64 write SetAsInt64;
        property asExtended: Extended read GetAsExtended write SetAsExtended;
        property asDouble: Double read GetAsDouble write SetAsDouble;
        property asDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
        property asDate: TDateTime read GetAsDate write SetAsDate;
        property asTime: TDateTime read GetAsTime write SetAsTime;
        property asString: String read GetAsString write SetAsString;
        property asHex: Hex read GetAsHex write SetAsHex;
        property asVersion: TVersionInfo read GetAsVersion write SetAsVersion;
        property asPointer: Pointer read GetAsPointer write SetAsPointer;
        property asClass: TClass read GetAsClass write SetAsClass;
        property asObject: TObject read GetAsObject write SetAsObject;
        property asStream: TStream read GetAsStream write SetAsStream;
    end;
{$M-}
