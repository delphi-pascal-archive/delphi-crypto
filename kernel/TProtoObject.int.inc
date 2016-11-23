{******************************************************************************}
{*  TProtoObject.int.inc                                                      *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ прото-объект }
{$M+}
    CProtoObject = class of TProtoObject;
    PProtoObject = ^TProtoObject;
    TProtoObject = class (TPersistent)
    protected
        procedure AssignTo (Dest: TPersistent); override;
    public
        class procedure _raise (anArgs: array of const;
                                const anEGUID: String = ''); overload; virtual;
        class procedure _raise (anArgs: array of const;
                                anEGUID: array of const); overload; virtual;
    private
        f_ClassID: ShortString;         { идентификатор класса }
        f_Properties: TProtoProperties; { свойства }
        f_Mode: TObjectMode;            { режим }
        f_Errors: TStringList;          { список ошибок }
    protected
        function RegisterProperty (anArgs: array of const;
                                   aMode: TPropertyMode = []) : Integer; overload; virtual;
        procedure RegisterProperties; virtual;
    protected
        function GetPropAt (anIndex: Integer) : TProtoProperty; overload; virtual;
        function GetPropOf (aName: String) : TProtoProperty; overload; virtual;
        procedure SetPropAt (anIndex: Integer; anItem: TProtoProperty); overload; virtual;
        procedure SetPropOf (aName: String; anItem: TProtoProperty); overload; virtual;
    protected
        function GetObjectName : String; virtual;        
        procedure SetObjectName (const aValue: String); virtual;
    public
        class function GetClassID : ShortString; virtual;
        class function GetClassName : String; virtual;
        class function GetClassVersion : TVersionInfo; virtual;
        class function GetClassProperties : TProtoProperties; overload; virtual;
        class function GetClassList : CProtoObjects; overload; virtual;
    public
        // object_name
        constructor Create (anArgs: array of const;
                            aMode: TObjectMode = []); virtual;
        destructor Destroy; override;
    public
        function Check : Boolean; overload; virtual;
        procedure Error (const aMessage: String); overload; virtual;
        procedure ErrorFmt (const aMessage: String; anArgs: array of const); overload; virtual;
    public
        property ClassID: ShortString read f_ClassID;
        property Properties: TProtoProperties read f_Properties write f_Properties;
        property PropAt [anIndex: Integer]: TProtoProperty read GetPropAt write SetPropAt;
        property PropOf [aName: String]: TProtoProperty read GetPropOf write SetPropOf;
        property ObjectName: String read GetObjectName write SetObjectName;
        property Mode: TObjectMode read f_Mode write f_Mode;
        property Errors: TStringList read f_Errors write f_Errors;
    end;
{$M-}
