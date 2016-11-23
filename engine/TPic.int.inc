{******************************************************************************}
{*  TPic.int.inc                                                              *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
// Внимание! Данный класс предназначен
// для хранения изображения в памяти и в БД в открытом, незашифрованном виде
{ иконка }
{$M+}
    CPic = class of TPic;
    PPic = ^TPic;
    TPic = class (THypoObject)
    private
        f_Picture: TPicture;
        f_Bitmap: TBitmap;
    protected
        function CalcKeyHash : Hex; override;
        function CalcDataHash : Hex; override;
    protected
        function GetIDOwner : TID; virtual;
        function GetData : TStream; virtual;
        function GetFormat : String; virtual;
        procedure SetIDOwner (const aValue: TID); virtual;
        procedure SetData (const aValue: TStream); virtual;
        procedure SetFormat (const aValue: String); virtual;
    public
        class function GetClassName : String; override;
        class function GetClassVersion : TVersionInfo; override;
        class function GetClassProperties : TProtoProperties; override;
        class function GetClassList : CProtoObjects; override;
        class function GetClassTable : String; override;
        class procedure CreateClassTable (const aDB: TSQLiteDatabase); override;
    public
        class function GetGraphicClass (const aFormat: String) : TGraphicClass; virtual;
        function GraphicClass : TGraphicClass; virtual;
        class function GetGraphicFormat (const aClass: TClass) : String; virtual;
        function GraphicFormat : String; virtual;
    public
        // id, id_owner, format, data, time_stamp_create, time_stamp_modify, time_stamp_public, version
        constructor Create (const aDB: TSQLiteDatabase;
                            anArgs: array of const;
                            aMode: TObjectMode = []); override;
        destructor Destroy; override;
    public
        function Check : Boolean; override;
        procedure Load; override;
        procedure Save; override;
    public
        procedure LoadFromStream (const aStream: TStream); override;
        procedure SaveToStream (out aStream: TStream); override;
    public
        property IDOwner: TID read GetIDOwner write SetIDOwner;
        // данные в потоке актуальны лишь после сохранения/загрузки
        // в остальное время данные в Picture могут отличаться от данных в потоке
        property Picture: TPicture read f_Picture write f_Picture;
        property Bitmap: TBitmap read f_Bitmap write f_Bitmap;
        property Data: TStream read GetData write SetData;
        property DataFormat: String read GetFormat write SetFormat;
    end;
{$M-}