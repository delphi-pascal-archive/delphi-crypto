unit uPackagesConstructor;
{******************************************************************************}
{*  Packages Constructor Unit                                                 *}
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
        Caption    : ShortString;
        ImageIndex : Integer;
        Sender     : ShortString;
        Receiver   : ShortString;
        Message    : ShortString;
    end;

type
{ конструктор пакетов }
{$M+}
    CPackagesConstructor = class of TPackagesConstructor;
    PPackagesConstructor = ^TPackagesConstructor;
    TPackagesConstructor = class (TDllThread)
    private
        f_DBFileName: String;        { файл базы данных }
        f_DB: TSQLiteDatabase;       { объект базы данных }
        f_MetaObject: TMetaObject;   { мета-объект }
        f_MetaClass: CMetaObject;    { класс мета-объекта }
        f_MetaHash: Hex;             { хэш-ключ мета-объекта }
        f_PackageTypeID: TID;        { идентификатор типа пакета }
        f_ListView: TsListView;      { лог }
        f_Gauge: TGauge;             { показатель прогресса загрузки }
        f_StatusPanel: TStatusPanel; { строка состояния загрузки }
        f_Users: TUsers;             { список пользователей }
        f_UserIndex: LongInt;        { позиция в обходе списка }
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
        property MetaObject: TMetaObject read f_MetaObject write f_MetaObject;
        property MetaClass: CMetaObject read f_MetaClass write f_MetaClass;
        property MetaHash: Hex read f_MetaHash write f_MetaHash;
        property PackageTypeID: TID read f_PackageTypeID write f_PackageTypeID;
        property ListView: TsListView read f_ListView write f_ListView;
        property Gauge: TGauge read f_Gauge write f_Gauge;
        property StatusPanel: TStatusPanel read f_StatusPanel write f_StatusPanel;
        property Users: TUsers read f_Users write f_Users;
        property UserIndex: LongInt read f_UserIndex write f_UserIndex;
        property Log: TItems read f_Log write f_Log;
        property Status: String read f_Status write f_Status;
        property MaxProgress: WORD read f_MaxProgress write f_MaxProgress;
        property Progress: WORD read f_Progress write f_Progress;
    end;
{$M-}

{ конструктор пакетов-обновлений }
{$M+}
    CForumUpdater = class of TForumUpdater;
    PForumUpdater = ^TForumUpdater;
    TForumUpdater = class (TDllThread)
    private
        f_DBFileName: String;        { файл базы данных }
        f_DB: TSQLiteDatabase;       { объект базы данных }
        f_LocalUserID: TID;          { идентификатор контакта }
        f_LocalUser: TUser;          { контакт }
        f_ListView: TsListView;      { лог }
        f_Gauge: TGauge;             { показатель прогресса загрузки }
        f_StatusPanel: TStatusPanel; { строка состояния загрузки }
        f_Step: Integer;             { текущий шаг создания обновлений }
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
        property LocalUserID: TID read f_LocalUserID write f_LocalUserID;
        property LocalUser: TUser read f_LocalUser write f_LocalUser;
        property ListView: TsListView read f_ListView write f_ListView;
        property Gauge: TGauge read f_Gauge write f_Gauge;
        property StatusPanel: TStatusPanel read f_StatusPanel write f_StatusPanel;
        property Step: Integer read f_Step write f_Step;
        property Log: TItems read f_Log write f_Log;
        property Status: String read f_Status write f_Status;
        property MaxProgress: WORD read f_MaxProgress write f_MaxProgress;
        property Progress: WORD read f_Progress write f_Progress;
    end;
{$M-}

{ TPackagesConstructor Errors }
resourcestring
    ERR_TPACKAGECONSTRUCTOR_INCORRECT_DATABASE     = 'Не инициализирован объект БД!';
    ERR_TPACKAGECONSTRUCTOR_INVALID_METAOBJECT     = 'Некорректный мета-объект!';
    ERR_TPACKAGECONSTRUCTOR_INVALID_METACLASS      = 'Некорректный мета-класс ''%s''!';
    ERR_TPACKAGECONSTRUCTOR_INCORRECT_GAUGE        = 'Не инициализирован элемент контроля прогресса!';
    ERR_TPACKAGECONSTRUCTOR_INCORRECT_STATUS_PANEL = 'Не инициализирована строка состояния!';
    ERR_TPACKAGECONSTRUCTOR_INVALID_PACKAGE        = 'Создан некорректный пакет!';
    ERR_TPACKAGECONSTRUCTOR_CREATE                 = 'Ошибка создания потока конструктора пакетов!';
    ERR_TPACKAGECONSTRUCTOR_DESTROY                = 'Ошибка уничтожения потока конструктора пакетов!';
    ERR_TPACKAGECONSTRUCTOR_MAIN                   = 'Ошибка главной функции потока!';
    ERR_TPACKAGECONSTRUCTOR_RETURN                 = 'Ошибка функции возврата потока!';
    ERR_TPACKAGECONSTRUCTOR_WRITE_STATUS           = 'Ошибка отображения статуса!';
    ERR_TPACKAGECONSTRUCTOR_WRITE_LOG              = 'Ошибка записи в лог!';

{ TPackagesConstructor Hints }
resourcestring
    MSG_TPACKAGECONSTRUCTOR_CREATE_PACKAGE       = 'Обмен данными с ''%s'' ...';
    MSG_TPACKAGECONSTRUCTOR_CREATED_PACKAGE      = 'Пакет ''%s'' создан.';
    MSG_TPACKAGECONSTRUCTOR_CREATE_ERROR_PACKAGE = 'Объект ''%s'' Ошибка создания пакета : %s';

{ TForumUpdater Errors }
resourcestring
    ERR_TFORUMUPDATER_INCORRECT_DATABASE     = 'Не инициализирован объект БД!';
    ERR_TFORUMUPDATER_INVALID_USER_ID        = 'Некорректный идентификатор пользователя!';
    ERR_TFORUMUPDATER_INCORRECT_GAUGE        = 'Не инициализирован элемент контроля прогресса!';
    ERR_TFORUMUPDATER_INCORRECT_STATUS_PANEL = 'Не инициализирована строка состояния!';
    ERR_TFORUMUPDATER_INVALID_PACKAGE        = 'Создан некорректный пакет!';
    ERR_TFORUMUPDATER_CREATE                 = 'Ошибка создания потока конструктора пакетов!';
    ERR_TFORUMUPDATER_DESTROY                = 'Ошибка уничтожения потока конструктора пакетов!';
    ERR_TFORUMUPDATER_MAIN                   = 'Ошибка главной функции потока!';
    ERR_TFORUMUPDATER_RETURN                 = 'Ошибка функции возврата потока!';
    ERR_TFORUMUPDATER_WRITE_STATUS           = 'Ошибка отображения статуса!';
    ERR_TFORUMUPDATER_WRITE_LOG              = 'Ошибка записи в лог!';

{ TForumUpdater Hints }
resourcestring
    MSG_TFORUMUPDATER_CREATE_PACKAGE_CATEGORIES = 'Обмен списком категорий с ''%s'' ...';
    MSG_TFORUMUPDATER_CREATE_PACKAGE_MESSAGES   = 'Обмен списком сообщений с ''%s'' ...';
    MSG_TFORUMUPDATER_CREATE_PACKAGE_USERS      = 'Обмен списком контактов с ''%s'' ...';
    MSG_TFORUMUPDATER_CREATED_PACKAGE           = 'Пакет ''%s'' создан.';
    MSG_TFORUMUPDATER_CREATE_ERROR_PACKAGE      = 'Ошибка создания пакета : %s';

implementation

constructor TPackagesConstructor.Create (anArgs: array of const);
var
    I    : Integer;
    args : array_of_const;
    OBJ  : TObject;
begin
    try
        { передаем параметры создания базового потока,
          начиная с шестого аргумента }
        if ( High (anArgs) >= 5 ) then
        begin
            SetLength ( Args, High (anArgs)-5 +1 );
            for I := 5 to High (anArgs) do
                args [I-5] := anArgs [I];
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
            raise Exception.Create (ERR_TPACKAGECONSTRUCTOR_INCORRECT_DATABASE);
        { второй параметр - мета-объект - источник данных }
        f_MetaObject := NIL;
        if notEmpty (1,anArgs) then
        begin
            OBJ := toObject (anArgs [1]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TMetaObject) ) then
                f_MetaObject := TMetaObject (OBJ);
        end;
        if ( not Assigned (f_MetaObject) ) then
            raise Exception.Create (ERR_TPACKAGECONSTRUCTOR_INVALID_METAOBJECT);
        { определяем класс мета-объекта }
        f_MetaClass := NIL;
        MetaClass := CMetaObject ( MetaObject.ClassType );
        if not ( MetaObject is TUser ) and
           {not ( MetaObject is TPic ) and}
           not ( MetaObject is TCategorie ) and
           not ( MetaObject is TMessage ) then
            raise Exception.CreateFmt (ERR_TPACKAGECONSTRUCTOR_INVALID_METACLASS,[ MetaObject.ClassID ]);
        { определяем хэш-ключ мета-объекта }
        f_MetaHash := '';
        if ( MetaObject is THypoObject ) then
            MetaHash := THypoObject (MetaObject).KeyHash
        else if ( MetaObject is THyperObject ) then
            MetaHash := THyperObject (MetaObject).KeyHash;
        { определяем тип пакета по статусу объекта }
        f_PackageTypeID := 0;
        if objDeleted in MetaObject.Mode then
            PackageTypeID := PACKAGE_DEL_TYPE_ID
        else if objSaved in MetaObject.Mode then
            PackageTypeID := PACKAGE_PUT_TYPE_ID
        else
            PackageTypeID := PACKAGE_GET_TYPE_ID;
        { третий параметр - ListView
          лог почты }
        f_ListView := NIL;
        if notEmpty (2,anArgs) then
        begin
            OBJ := toObject (anArgs [2]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TsListView) ) then
                f_ListView := TsListView (OBJ);
        end;
        { четвертый параметр - Gauge
          показатель прогресса загрузки }
        f_Gauge := NIL;
        if notEmpty (3,anArgs) then
        begin
            OBJ := toObject (anArgs [3]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TControl) ) then
                f_Gauge := TGauge (OBJ);
        end;
        { пятый параметр - StatusPanel
          показатель прогресса загрузки }
        f_StatusPanel := NIL;
        if notEmpty (4,anArgs) then
        begin
            OBJ := toObject (anArgs [4]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TStatusPanel) ) then
                f_StatusPanel := TStatusPanel (OBJ);
        end;
        { список пользователей }
        f_Users := NIL;
        f_UserIndex := -1;
        { лог }
        f_Log := TItems.Create ([]);
    except on E: Exception do
        _raise ([ 'Create', ERR_TPACKAGECONSTRUCTOR_CREATE, E, Exception (FatalException) ],
                ['{52908050-2F33-401B-9D12-DE71623106D4}']);
    end;
end;

destructor TPackagesConstructor.Destroy;
begin
    try
        FreeAndNil (f_Log);
        FreeAndNil (f_Users);
        FreeAndNil (f_DB);
        inherited Destroy;
    except on E: Exception do
        _raise ([ 'Destroy', ERR_TPACKAGECONSTRUCTOR_DESTROY, E, Exception (FatalException) ],
                ['{F151CEA3-97FC-4B3C-9C33-DB5F4B2B7B8E}']);
    end;
end;

procedure TPackagesConstructor.Main;
var
    Package : TPackage;
begin
    try
        if Terminated then Exit;
        inherited Main;
        { загружаем список контактов }
        if not Assigned (Users) then
        try
            f_Users := TUsers.Load (DB,[ _([]),
                                         _([]),
                                         _([USER_ID]) ],
                                       [],
                                       [],
                                       [objSimple]) as TUsers;
            MaxProgress := Users.Count;
            Progress := 0;
        except
            FreeAndNil (f_Users);
        end;
        if not Assigned (Users) or ( Users.Count <= 0 ) then
            Terminate
        else if ( UserIndex >= Users.Count-1 ) then
            Terminate
        else
        try
            UserIndex := UserIndex + 1;
            { прогресс }
            Progress := Progress + 1;
            { не создаем пакет для собственного пользователя }
            if ( Users.ItemAt [UserIndex].ID = USER_ID ) then
                Exit;
            { создаем пакет }
            WriteStatus (MSG_TPACKAGECONSTRUCTOR_CREATE_PACKAGE,[ Users.ItemAt [UserIndex].Login ]);
            Package := TPackage.Create (DB,[0,USER_ID,Users.ItemAt [UserIndex].ID,USER_ID,
                                            PackageTypeID,
                                            PACKAGE_CREATED_STATUS_ID,
                                            MetaClass]);
            try
                Package.CreateMetaData ([MetaHash]);
                Package.Save;
                { пишем в лог }
                WriteLog ( User.Login,
                           Users.ItemAt [UserIndex].Login,
                           Format (MSG_TPACKAGECONSTRUCTOR_CREATED_PACKAGE,
                                   [ Package.KeyHash ]),
                           pckDefault,
                           GetPckTypeExternal (PackageTypeID),
                           MetaClass.GetClassID );
            finally
                FreeAndNil (Package);
            end;
        except on CreateError: Exception do
            if Assigned (Users.ItemAt [UserIndex]) then
            begin
                { пишем в лог причину ошибки }
                WriteLog ( User.Login,
                           Users.ItemAt [UserIndex].Login,
                           Format (MSG_TPACKAGECONSTRUCTOR_CREATE_ERROR_PACKAGE,
                                   [ MetaHash ]),
                           pckError,
                           GetPckTypeExternal (PackageTypeID),
                           MetaClass.GetClassID );
            end
            else
                WriteStatus (CreateError.Message);
        end;
    except on E: Exception do
        _raise ([ 'Main', ERR_TPACKAGECONSTRUCTOR_MAIN, E, Exception (FatalException) ],
                ['{465643E6-FED0-42BA-88AA-9B9556628E23}']);
    end;
end;

procedure TPackagesConstructor.Return;
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
        _raise ([ 'Return', ERR_TPACKAGECONSTRUCTOR_RETURN, E, Exception (FatalException) ],
                ['{309AE935-E39F-49FC-B3A7-56F21A0166C8}']);
    end;
end;

procedure TPackagesConstructor.WriteStatus (const aMessage: String);
begin
    try
        Status := aMessage;
    except on E: Exception do
        _raise (['WriteStatus',ERR_TPACKAGECONSTRUCTOR_WRITE_STATUS,E],
                ['{E7053B34-01D2-4AD9-B68E-4E443C8BE2DA}']);
    end;
end;

procedure TPackagesConstructor.WriteStatus (const aMessage: String;
                                        aParams: array of const);
begin
    try
        WriteStatus ( Format (aMessage,aParams) );
    except on E: Exception do
        _raise (['WriteStatus',ERR_TPACKAGECONSTRUCTOR_WRITE_STATUS,E],
                ['{40D65BA3-B0E3-48F4-A1BE-70B82FCF40FC}']);
    end;
end;

procedure TPackagesConstructor.WriteLog (const aSender: String;
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
            Log.Add (Rec);
        end;
    except on E: Exception do
        _raise (['WriteLog',ERR_TPACKAGECONSTRUCTOR_WRITE_LOG,E],
                ['{FDE75E40-CA4E-40FF-B8AB-924F1EC8661B}']);
    end;
end;

procedure TPackagesConstructor.WriteLog (const aSender: String;
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
        _raise (['WriteLog',ERR_TPACKAGECONSTRUCTOR_WRITE_LOG,E],
                ['{7C982F9C-C5B1-456E-9D84-D3D6045C868D}']);
    end;
end;

{ TForumUpdater }
constructor TForumUpdater.Create (anArgs: array of const);
var
    I    : Integer;
    args : array_of_const;
    OBJ  : TObject;
begin
    try
        { передаем параметры создания базового потока,
          начиная с шестого аргумента }
        if ( High (anArgs) >= 5 ) then
        begin
            SetLength ( Args, High (anArgs)-5 +1 );
            for I := 5 to High (anArgs) do
                args [I-5] := anArgs [I];
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
            raise Exception.Create (ERR_TFORUMUPDATER_INCORRECT_DATABASE);
        { второй параметр - идентификатор пользователя }
        f_LocalUserID := 0;
        if notEmpty (1,anArgs) then
            f_LocalUserID := toInt64 (anArgs [1]);
        if not ( LocalUserID > 0 ) then
            raise Exception.Create (ERR_TFORUMUPDATER_INVALID_USER_ID);
        { третий параметр - ListView
          лог почты }
        f_ListView := NIL;
        if notEmpty (2,anArgs) then
        begin
            OBJ := toObject (anArgs [2]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TsListView) ) then
                f_ListView := TsListView (OBJ);
        end;
        { четвертый параметр - Gauge
          показатель прогресса загрузки }
        f_Gauge := NIL;
        if notEmpty (3,anArgs) then
        begin
            OBJ := toObject (anArgs [3]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TControl) ) then
                f_Gauge := TGauge (OBJ);
        end;
        { пятый параметр - StatusPanel
          показатель прогресса загрузки }
        f_StatusPanel := NIL;
        if notEmpty (4,anArgs) then
        begin
            OBJ := toObject (anArgs [4]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TStatusPanel) ) then
                f_StatusPanel := TStatusPanel (OBJ);
        end;
        { контакт }
        f_LocalUser := NIl;
        { текущий шаг }
        f_Step := 0;
        { лог }
        f_Log := TItems.Create ([]);
    except on E: Exception do
        _raise ([ 'Create', ERR_TFORUMUPDATER_CREATE, E, Exception (FatalException) ],
                ['{543AEB18-13FE-43AB-83AA-ADD57F5747D8}']);
    end;
end;

destructor TForumUpdater.Destroy;
begin
    try
        FreeAndNil (f_Log);
        FreeAndNil (f_LocalUser);
        FreeAndNil (f_DB);
        inherited Destroy;
    except on E: Exception do
        _raise ([ 'Destroy', ERR_TFORUMUPDATER_DESTROY, E, Exception (FatalException) ],
                ['{506418A5-B31D-422D-B15D-CF2D015E170F}']);
    end;
end;

procedure TForumUpdater.Main;
var
    MetaClass : CMetaObject;
    Package   : TPackage;
begin
    try
        if Terminated then Exit;
        inherited Main;
        if not Assigned (LocalUser) then
        try
            f_LocalUser := TUser.Load (DB,LocalUserID,[objSimple]) as TUser;
            MaxProgress := 3;
            Progress := 0;
        except
            FreeAndNil (f_LocalUser);
        end;
        if not Assigned (LocalUser) or ( LocalUserID = USER_ID ) then
            Terminate
        else if ( Step >= 3 ) then
            Terminate
        else
        try
            Step := Step + 1;
            { прогресс }
            Progress := Progress + 1;
            { определяем тип пакета }
            case Step of
                1 : begin
                        MetaClass := TCategorie;
                        WriteStatus (MSG_TFORUMUPDATER_CREATE_PACKAGE_CATEGORIES,[ LocalUser.Login ]);
                    end;
                2 : begin
                        MetaClass := TMessage;
                        WriteStatus (MSG_TFORUMUPDATER_CREATE_PACKAGE_MESSAGES,[ LocalUser.Login ]);
                    end;
                3 : begin
                        MetaClass := TUser;
                        WriteStatus (MSG_TFORUMUPDATER_CREATE_PACKAGE_USERS,[ LocalUser.Login ]);
                    end;
            end;
            { создаем пакет }
            Package := TPackage.Create (DB,[0,USER_ID,LocalUserID,USER_ID,
                                            PACKAGE_UPD_TYPE_ID,
                                            PACKAGE_CREATED_STATUS_ID,
                                            MetaClass]);
            try
                Package.CreateMetaData ([]);
                Package.Save;
                { пишем в лог }
                WriteLog ( User.Login,
                           LocalUser.Login,
                           Format (MSG_TFORUMUPDATER_CREATED_PACKAGE,
                                   [ Package.KeyHash ]),
                           pckDefault,
                           GetPckTypeExternal (PACKAGE_UPD_TYPE_ID),
                           MetaClass.GetClassID );
            finally
                FreeAndNil (Package);
            end;
        except on CreateError: Exception do
            if Assigned (LocalUser) then
            begin
                { пишем в лог причину ошибки }
                WriteLog ( User.Login,
                           LocalUser.Login,
                           MSG_TFORUMUPDATER_CREATE_ERROR_PACKAGE,
                           pckError,
                           GetPckTypeExternal (PACKAGE_UPD_TYPE_ID),
                           MetaClass.GetClassID );
            end
            else
                WriteStatus (CreateError.Message);
        end;
    except on E: Exception do
        _raise ([ 'Main', ERR_TFORUMUPDATER_MAIN, E, Exception (FatalException) ],
                ['{809BFBA5-152E-42FF-9E11-F14B95595A3B}']);
    end;
end;

procedure TForumUpdater.Return;
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
        _raise ([ 'Return', ERR_TFORUMUPDATER_RETURN, E, Exception (FatalException) ],
                ['{D5934537-A16F-464F-B149-AED5E69C358B}']);
    end;
end;

procedure TForumUpdater.WriteStatus (const aMessage: String);
begin
    try
        Status := aMessage;
    except on E: Exception do
        _raise (['WriteStatus',ERR_TFORUMUPDATER_WRITE_STATUS,E],
                ['{4C62406A-B2FC-4EFB-A080-8B29FE2BFF4B}']);
    end;
end;

procedure TForumUpdater.WriteStatus (const aMessage: String;
                                     aParams: array of const);
begin
    try
        WriteStatus ( Format (aMessage,aParams) );
    except on E: Exception do
        _raise (['WriteStatus',ERR_TFORUMUPDATER_WRITE_STATUS,E],
                ['{AF71AA5E-EFB2-42FA-8DEC-1A246E65AF53}']);
    end;
end;

procedure TForumUpdater.WriteLog (const aSender: String;
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
            Log.Add (Rec);
        end;
    except on E: Exception do
        _raise (['WriteLog',ERR_TFORUMUPDATER_WRITE_LOG,E],
                ['{F79DBD7B-CBC5-4E1A-93A8-390440B70897}']);
    end;
end;

procedure TForumUpdater.WriteLog (const aSender: String;
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
        _raise (['WriteLog',ERR_TFORUMUPDATER_WRITE_LOG,E],
                ['{D4AF48BA-F573-4652-8912-2713C0FC6500}']);
    end;
end;

end.