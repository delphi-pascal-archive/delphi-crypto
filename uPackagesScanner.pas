unit uPackagesScanner;
{******************************************************************************}
{*  Packages Scanner Unit                                                     *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2012                                             *}
{******************************************************************************}
interface

{$I 'std.inc'}

uses
    Windows, SysUtils, Variants, Classes,
    Controls, ComCtrls, Gauges,
    sListView,
{ utils }
    DateUtils, Utils, Strings, VarRecs, Versions, EClasses,
    DllThreads,
{ kernel }
    Kernel, ProtoClasses, CryptoClasses, MetaClasses, ParaClasses,
    HypoClasses, HyperClasses,
{ engine }
    Engine,
{ SQLite }
    SQLite3, SQLite3DLL, SQLiteTable3;

{ иконки }
const
    pckDefault = 0;
    pckEmpty   = 1;
    pckFull    = 2;
    pckTest    = 3;
    pckError   = 4;
    pckSend    = 5;

type
    PLogRecord = ^TLogRecord;
    TLogRecord = packed record
        Caption     : ShortString;
        ImageIndex  : Integer;
        Sender      : ShortString;
        Receiver    : ShortString;
        Message     : ShortString;
        PackageType : ShortString;
        MetaClassID : ShortString;
    end;

type
{ обработчик входящих пакетов }
{$M+}
    CPackagesScanner = class of TPackagesScanner;
    PPackagesScanner = ^TPackagesScanner;
    TPackagesScanner = class (TDllThread)
    private
        f_DBFileName: String;        { файл базы данных }
        f_DB: TSQLiteDatabase;       { объект базы данных }
        f_ListView: TsListView;      { лог }
        f_Gauge: TGauge;             { показатель прогресса загрузки }
        f_StatusPanel: TStatusPanel; { строка состояния загрузки }
        f_Packages: TPackages;       { список пакетов }
        f_PackageIndex: LongInt;     { позиция в обходе списка }
    private
        f_Log: TItems;
        f_Status: String;
        f_MaxProgress: WORD;
        f_Progress: WORD;
    public
        constructor Create (anArgs: array of const); override;
        destructor Destroy; override;
    public
        procedure Main; override;
        procedure Return; override;
    public
        procedure ProcessPackage (aPackage: TPackage); virtual;
    protected
        procedure WriteStatus (const aMessage: String); overload;
        procedure WriteStatus (const aMessage: String;
                               aParams: array of const); overload;
        procedure WriteLog (const aSender: String;
                            const aReceiver: String;
                            const aMessage: String;
                            const anImageIndex: Integer = pckDefault;
                            const aPackageType: String = '';
                            const aMetaClassID: String = ''); overload;
        procedure WriteLog (const aSender: String;
                            const aReceiver: String;
                            const aMessage: String;
                            aParams: array of const;
                            const anImageIndex: Integer = pckDefault;
                            const aPackageType: String = '';
                            const aMetaClassID: String = ''); overload;
    public
        property DBFileName: String read f_DBFileName;
        property DB: TSQLiteDatabase read f_DB write f_DB;
        property ListView: TsListView read f_ListView write f_ListView;
        property Gauge: TGauge read f_Gauge write f_Gauge;
        property StatusPanel: TStatusPanel read f_StatusPanel write f_StatusPanel;
        property Packages: TPackages read f_Packages write f_Packages;
        property PackageIndex: LongInt read f_PackageIndex write f_PackageIndex;
        property Log: TItems read f_Log write f_Log;
        property Status: String read f_Status write f_Status;
        property MaxProgress: WORD read f_MaxProgress write f_MaxProgress;
        property Progress: WORD read f_Progress write f_Progress;
    end;
{$M-}

{ TPackagesScanner Errors }
resourcestring
    ERR_TPACKAGESCANNER_INCORRECT_DATABASE     = 'Не инициализирован объект БД!';
    ERR_TPACKAGESCANNER_INCORRECT_GAUGE        = 'Не инициализирован элемент контроля прогресса!';
    ERR_TPACKAGESCANNER_INCORRECT_STATUS_PANEL = 'Не инициализирована строка состояния!';
    ERR_TPACKAGESCANNER_INVALID_PACKAGE        = 'Получен некорректный пакет!';
    ERR_TPACKAGESCANNER_CREATE                 = 'Ошибка создания потока обработки пакетов!';
    ERR_TPACKAGESCANNER_DESTROY                = 'Ошибка уничтожения потока обработки пакетов!';
    ERR_TPACKAGESCANNER_MAIN                   = 'Ошибка главной функции потока!';
    ERR_TPACKAGESCANNER_RETURN                 = 'Ошибка функции возврата потока!';
    ERR_TPACKAGESCANNER_WRITE_STATUS           = 'Ошибка отображения статуса!';
    ERR_TPACKAGESCANNER_WRITE_LOG              = 'Ошибка записи в лог!';

{ TPackagesScanner Hints }
resourcestring
    MSG_TPACKAGESCANNER_EXECUTING_PACKAGE = 'Обработка пакета ''%s''...';
    MSG_TPACKAGESCANNER_EXECUTED_PACKAGE  = 'Пакет ''%s'' обработан.';
    MSG_TPACKAGESCANNER_REJECTED_PACKAGE  = 'Пакет ''%s'' отвергнут. Ошибка : %s';

implementation

constructor TPackagesScanner.Create (anArgs: array of const);
var
    I    : Integer;
    args : array_of_const;
    OBJ  : TObject;
begin
    try
        { передаем параметры создания базового потока,
          начиная с пятого аргумента }
        if ( High (anArgs) >= 4 ) then
        begin
            SetLength ( Args, High (anArgs)-4 +1 );
            for I := 4 to High (anArgs) do
                args [I-4] := anArgs [I];
        end
        else
            args := _array_of_const ([]);
        inherited Create (args);
        { явно указываем на необходимость уничтожения потока по окончанию работы }
        FreeOnTerminate := TRUE;
        { имя потока }
        Name := ClassName;
        { первый параметр - файл БД }
        f_DBFileName := '';
        f_DB := NIL;
        if notEmpty (0,anArgs) then
        begin
            f_DBFileName := toString (anArgs [0]);
            f_DB := TSQLiteDatabase.Create (f_DBFileName);
        end;
        if ( not Assigned (f_DB) ) then
            raise Exception.Create (ERR_TPACKAGESCANNER_INCORRECT_DATABASE);
        { второй параметр - ListView
          лог почты }
        f_ListView := NIL;
        if notEmpty (1,anArgs) then
        begin
            OBJ := toObject (anArgs [1]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TsListView) ) then
                f_ListView := TsListView (OBJ);
        end;
        { третий параметр - Gauge
          показатель прогресса загрузки }
        f_Gauge := NIL;
        if notEmpty (2,anArgs) then
        begin
            OBJ := toObject (anArgs [2]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TControl) ) then
                f_Gauge := TGauge (OBJ);
        end;
        { четвертый параметр - StatusPanel
          показатель прогресса загрузки }
        f_StatusPanel := NIL;
        if notEmpty (3,anArgs) then
        begin
            OBJ := toObject (anArgs [3]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TStatusPanel) ) then
                f_StatusPanel := TStatusPanel (OBJ);
        end;
        { список пакетов }
        f_Packages := NIL;
        f_PackageIndex := -1;
        { лог }
        f_Log := TItems.Create ([]);
    except on E: Exception do
        _raise ([ 'Create', ERR_TPACKAGESCANNER_CREATE, E, Exception (FatalException) ],
                ['{C4C3FCC8-25BE-4DAF-AE8C-ED2B25A61F63}']);
    end;
end;

destructor TPackagesScanner.Destroy;
begin
    try
        FreeAndNil (f_Log);
        FreeAndNil (f_Packages);
        FreeAndNil (f_DB);
        inherited Destroy;
    except on E: Exception do
        _raise ([ 'Destroy', ERR_TPACKAGESCANNER_DESTROY, E, Exception (FatalException) ],
                ['{E7C92472-772D-4B7F-83A3-8AEB36BD7EFD}']);
    end;
end;

procedure TPackagesScanner.Main;
begin
    try
        if Terminated then Exit;
        inherited Main;
        { запрашиваем все полученные пакеты,
          предназначенные нам }
        if not Assigned (Packages) then
        try
            f_Packages := TPackages.Load (DB,[ _([]),
                                               _([]),
                                               _([ USER_KEY_HASH ]),
                                               _([USER_ID]),
                                               _([]),
                                               _([PACKAGE_RECEIVED_STATUS_ID]),
                                               _([ TUser, {TPic,} TMessage, TCategorie, TMetaObject ]) ],
                                             [ _pck_time_stamp_create ]) as TPackages;
            MaxProgress := Packages.Count;
            Progress := 0;
        except
            FreeAndNil (f_Packages);
        end;
        if not Assigned (Packages) or ( Packages.Count <= 0 ) then
            Terminate
        else if ( PackageIndex >= Packages.Count-1 ) then
            Terminate
        else
        try
            PackageIndex := PackageIndex + 1;
            { обрабатываем пакет }
            ProcessPackage (Packages.ItemAt [PackageIndex]);
            { обработка прошла успешно - пакет обработан }
            Packages.ItemAt [PackageIndex].IDStatus := PACKAGE_EXECUTED_STATUS_ID;
            Packages.ItemAt [PackageIndex].Save;
            { прогресс }
            Progress := Progress + 1;
        except on ProcessError: Exception do
            if Assigned (Packages.ItemAt [PackageIndex]) then
            begin
                { при обработке произошла ошибка - пакет отвергнут }
                Packages.ItemAt [PackageIndex].IDStatus := PACKAGE_REJECTED_STATUS_ID;
                Packages.ItemAt [PackageIndex].Save;
                { пишем в лог причину ошибки }
                WriteLog ( '...', User.Login,
                           Format (MSG_TPACKAGESCANNER_REJECTED_PACKAGE,
                                   [ Packages.ItemAt [PackageIndex].KeyHash,
                                     ProcessError.Message ]),
                           pckEmpty,
                           Packages.ItemAt [PackageIndex].PckType.IDExternal,
                           Packages.ItemAt [PackageIndex].MetaClass.GetClassID );
            end
            else
                WriteStatus (ProcessError.Message);
        end;
    except on E: Exception do
        _raise ([ 'Main', ERR_TPACKAGESCANNER_MAIN, E, Exception (FatalException) ],
                ['{A99E3E33-1DA6-4180-9C88-8DE52F14E2C5}']);
    end;
end;

procedure TPackagesScanner.Return;
var
    Itm : TListItem;
begin
    try
        inherited Return;
        if Assigned (ListView) and Assigned (Log) then
        begin
            while Log.Count > 0 do
            begin
                Itm := ListView.Items.Add;
                Itm.Caption := PLogRecord (Log.Item [0])^.Caption;
                Itm.ImageIndex := PLogRecord (Log.Item [0])^.ImageIndex;
                Itm.SubItems.Add ( PLogRecord (Log.Item [0])^.Sender );
                Itm.SubItems.Add ( PLogRecord (Log.Item [0])^.Receiver );
                Itm.SubItems.Add ( PLogRecord (Log.Item [0])^.Message );
                if (  ( PLogRecord (Log.Item [0])^.PackageType = 'PUT' ) or
                      ( PLogRecord (Log.Item [0])^.PackageType = 'DEL' )  ) and
                   (  ( PLogRecord (Log.Item [0])^.MetaClassID = TCategorie.GetClassID ) or
                      ( PLogRecord (Log.Item [0])^.MetaClassID = TMessage.GetClassID )  ) then
                    SetTabStatus (tabForum,tbsEmpty);
                Dispose ( PLogRecord (Log.Item [0]) );
                Log.Delete (0);
            end;
        end;
        if Assigned (StatusPanel) and notEmpty (Status) then
        begin
            StatusPanel.Text := Status;
        end;
        if Assigned (Gauge) then
        begin
            Gauge.MinValue := 0;
            Gauge.MaxValue := MaxProgress;
            Gauge.Progress := Progress;
        end;
        ProcessMessages;
    except on E: Exception do
        _raise ([ 'Return', ERR_TPACKAGESCANNER_RETURN, E, Exception (FatalException) ],
                ['{813F60E5-0E43-478E-8A32-B4BC75D4CD34}']);
    end;
end;

procedure TPackagesScanner.ProcessPackage (aPackage: TPackage);
begin
    if not Assigned (aPackage) then
        raise Exception.Create (ERR_TPACKAGESCANNER_INVALID_PACKAGE);
    WriteStatus (MSG_TPACKAGESCANNER_EXECUTING_PACKAGE,
                 [ aPackage.KeyHash ]);
    aPackage.ProcessMetaData;
    { пишем в лог, что пакет обработан }
    WriteLog ( '...', User.Login,
               Format (MSG_TPACKAGESCANNER_EXECUTED_PACKAGE,
                       [ aPackage.KeyHash ]),
               pckFull,
               aPackage.PckType.IDExternal,
               aPackage.MetaClass.GetClassID );
end;

procedure TPackagesScanner.WriteStatus (const aMessage: String);
begin
    try
        Status := aMessage;
    except on E: Exception do
        _raise (['WriteStatus',ERR_TPACKAGESCANNER_WRITE_STATUS,E],
                ['{38EE1B09-5C82-4D05-A623-E9305352D91B}']);
    end;
end;

procedure TPackagesScanner.WriteStatus (const aMessage: String;
                                        aParams: array of const);
begin
    try
        WriteStatus ( Format (aMessage,aParams) );
    except on E: Exception do
        _raise (['WriteStatus',ERR_TPACKAGESCANNER_WRITE_STATUS,E],
                ['{57C86D62-0E46-4D2D-BF5B-DAD6BB746598}']);
    end;
end;

procedure TPackagesScanner.WriteLog (const aSender: String;
                                     const aReceiver: String;
                                     const aMessage: String;
                                     const anImageIndex: Integer = pckDefault;
                                     const aPackageType: String = '';
                                     const aMetaClassID: String = '');
var
    Rec : PLogRecord;
begin
    try
        if Assigned (Log) then
        begin
            Rec := AllocMem ( SizeOf (TLogRecord) + 1 );
            Rec^.Caption := _DateTimeToStr (now);
            Rec^.ImageIndex := anImageIndex;
            Rec^.Sender := aSender;
            Rec^.Receiver := aReceiver;
            if notEmpty (aMetaClassID) then
                Rec^.Message := Format ('%s : %s : %s',[aMetaClassID,aPackageType,aMessage])
            else
                Rec^.Message := aMessage;
            Rec^.PackageType := aPackageType;
            Rec^.MetaClassID := aMetaClassID;
            Log.Add (Rec);
        end;
    except on E: Exception do
        _raise (['WriteLog',ERR_TPACKAGESCANNER_WRITE_LOG,E],
                ['{A484C147-114D-49A8-9F55-0CF50D0D3047}']);
    end;
end;

procedure TPackagesScanner.WriteLog (const aSender: String;
                                     const aReceiver: String;
                                     const aMessage: String;
                                     aParams: array of const;
                                     const anImageIndex: Integer = pckDefault;
                                     const aPackageType: String = '';
                                     const aMetaClassID: String = '');
begin
    try
        WriteLog ( aSender, aReceiver, Format (aMessage,aParams), anImageIndex, aPackageType, aMetaClassID );
    except on E: Exception do
        _raise (['WriteLog',ERR_TPACKAGESCANNER_WRITE_LOG,E],
                ['{86B0F1A8-F5D5-4ED6-8F6B-E9155771325E}']);
    end;
end;

end.