unit uHTTPServer;
{******************************************************************************}
{*  Package HTTP Server Unit                                                  *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011-2012                                        *}
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
{ synapse }
    WinSock, BlckSock, SynaUtil,
{ http }
    HTTPServer,
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
    pckReceive = 6;

type
{ пакетный сервер }
{$M+}
    CHTTPPackageServer = class of THTTPPackageServer;
    PHTTPPackageServer = ^THTTPPackageServer;
    THTTPPackageServer = class (THTTPServer)
    private
        f_DBFileName: String;        { файл базы данных }
        f_DB: TSQLiteDatabase;       { объект базы данных }
        f_ListView: TsListView;      { лог }
        f_Gauge: TGauge;             { показатель прогресса загрузки }
        f_StatusPanel: TStatusPanel; { строка состояния загрузки }
    public
        constructor Create (anArgs: array of const); override;
        destructor Destroy; override;
    public
        procedure Main; override;
    protected
        procedure WriteStatus (const aMessage: String); overload;
        procedure WriteStatus (const aMessage: String;
                               aParams: array of const); overload;
    public
        property DBFileName: String read f_DBFileName;
        property DB: TSQLiteDatabase read f_DB write f_DB;
        property ListView: TsListView read f_ListView write f_ListView;
        property Gauge: TGauge read f_Gauge write f_Gauge;
        property StatusPanel: TStatusPanel read f_StatusPanel write f_StatusPanel;
    end;
{$M-}

{ дочерний поток пакетного сервера }
{$M+}
    CHTTPPackageServerThread = class of THTTPPackageServerThread;
    PHTTPPackageServerThread = ^THTTPPackageServerThread;
    THTTPPackageServerThread = class (THTTPServerThread)
    private
        f_DBFileName: String;        { файл базы данных }
        f_DB: TSQLiteDatabase;       { объект базы данных }
        f_ListView: TsListView;      { лог }
        f_Gauge: TGauge;             { показатель прогресса загрузки }
        f_StatusPanel: TStatusPanel; { строка состояния загрузки }
    public
        constructor Create (anArgs: array of const); override;
        destructor Destroy; override;
    public
        procedure Main; override;
        procedure Return; override;
        function Process : WORD; override;
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
    end;
{$M-}

{ THTTPPackageServer Errors }
resourcestring
    ERR_THTTPPACKAGESERVER_INCORRECT_DATABASE = 'Не инициализирован объект БД!';
    ERR_THTTPPACKAGESERVER_CREATE             = 'Ошибка создания потока пакетного сервера!';
    ERR_THTTPPACKAGESERVER_DESTROY            = 'Ошибка уничтожения потока пакетного сервера!';
    ERR_THTTPPACKAGESERVER_EXECUTE            = 'Ошибка выполнения потока пакетного сервера!';
    ERR_THTTPPACKAGESERVER_MAIN               = 'Ошибка главной функции потока!';
    ERR_THTTPPACKAGESERVER_RETURN             = 'Ошибка функции возврата потока!';
    ERR_THTTPPACKAGESERVER_INVALID_PORT       = 'Некорректный порт!';
    ERR_THTTPPACKAGESERVER_INVALID_TIMEOUT    = 'Некорректное время ожидания!';
    ERR_THTTPPACKAGESERVER_WRITE_STATUS       = 'Ошибка отображения статуса!';

{ THTTPPackageServerThread Errors }
resourcestring
    ERR_THTTPPACKAGESERVERTHREAD_INCORRECT_DATABASE = 'Не инициализирован объект БД!';
    ERR_THTTPPACKAGESERVERTHREAD_CREATE             = 'Ошибка создания дочернего потока пакетного сервера!';
    ERR_THTTPPACKAGESERVERTHREAD_DESTROY            = 'Ошибка уничтожения дочернего потока пакетного сервера!';
    ERR_THTTPPACKAGESERVERTHREAD_MAIN               = 'Ошибка главной функции потока!';
    ERR_THTTPPACKAGESERVERTHREAD_RETURN             = 'Ошибка функции возврата потока!';
    ERR_THTTPPACKAGESERVERTHREAD_PROCESS            = 'Ошибка обработки запроса!';
    ERR_THTTPPACKAGESERVERTHREAD_INVALID_SOCKET_ID  = 'Некорректный дискриптор сокета!';
    ERR_THTTPPACKAGESERVERTHREAD_INVALID_TIMEOUT    = 'Некорректное время ожидания!';
    ERR_THTTPPACKAGESERVERTHREAD_SOCKET_ERROR       = 'Ошибка сокета %d : %s';
    ERR_THTTPPACKAGESERVERTHREAD_IVALID_REQUEST     = 'Некорректный запрос!';
    ERR_THTTPPACKAGESERVERTHREAD_IVALID_PROTOCOL    = 'Некорректный протокол!';
    ERR_THTTPPACKAGESERVERTHREAD_IVALID_METHOD      = 'Некорректный метод!';
    ERR_THTTPPACKAGESERVERTHREAD_IVALID_URI         = 'Некорректный URI!';
    ERR_THTTPPACKAGESERVERTHREAD_INVALID_PACKAGE    = 'Получен некорректный пакет!';
    ERR_THTTPPACKAGESERVERTHREAD_LOAD_PACKAGES      = 'Ошибка загрузки пакетов из потока!';
    ERR_THTTPPACKAGESERVERTHREAD_WRITE_STATUS       = 'Ошибка отображения статуса!';
    ERR_THTTPPACKAGESERVERTHREAD_WRITE_LOG          = 'Ошибка ведения лога!';

{ THTTPPackageServerThread Hints }
resourcestring
    MSG_TPACKAGESERVERTHREAD_RECEIVED_PACKAGE  = 'Получен пакет от %s:%d';
    MSG_TPACKAGESERVERTHREAD_EXECUTING_PACKAGE = 'Обработка пакета ''%s''...';
    MSG_TPACKAGESERVERTHREAD_TEST              = 'Тест: «%s» Версия клиента: v.%s';
    MSG_TPACKAGESERVERTHREAD_SAVED_PACKAGE     = 'Получен пакет ''%s''.';

implementation

uses
    DialogClasses;

{ THTTPPackageServer }
constructor THTTPPackageServer.Create (anArgs: array of const);
var
    I     : Integer;
    args1 : array_of_const;
    args2 : array_of_const;
    OBJ   : TObject;
begin
    try
        { второй и третий параметры - порт и время ожидания }
        args1 := _([NIL,NIL]);
        if notEmpty (1,anArgs) then
            args1 [0] := anArgs [1];
        if notEmpty (2,anArgs) then
            args1 [1] := anArgs [2];
        { передаем параметры создания базового потока,
          начиная с седьмого аргумента }
        if ( High (anArgs) >= 6 ) then
        begin
            SetLength ( args2, High (anArgs)-6 +1 );
            for I := 6 to High (anArgs) do
                args2 [I-6] := anArgs [I];
        end
        else
            args2 := _([]);
        inherited Create ( _(args1,args2) );
        { имя потока }
        Name := ClassName;
        { приоритет }
        Priority := tpNormal;
        { первый параметр - файл БД }
        f_DBFileName := '';
        f_DB := NIL;
        if notEmpty (0,anArgs) then
        begin
            f_DBFileName := toString (anArgs [0]);
            f_DB := TSQLiteDatabase.Create (f_DBFileName);
        end;
        if ( not Assigned (f_DB) ) then
            raise Exception.Create (ERR_THTTPPACKAGESERVER_INCORRECT_DATABASE);
        { четвертый параметр - ListView
          показатель прогресса загрузки }
        f_ListView := NIL;
        if notEmpty (3,anArgs) then
        begin
            OBJ := toObject (anArgs [3]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TsListView) ) then
                f_ListView := TsListView (OBJ);
        end;
        { пятый параметр - Gauge
          показатель прогресса загрузки }
        f_Gauge := NIL;
        if notEmpty (4,anArgs) then
        begin
            OBJ := toObject (anArgs [4]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TControl) ) then
                f_Gauge := TGauge (OBJ);
        end;
        { шестой параметр - StatusPanel
          показатель прогресса загрузки }
        f_StatusPanel := NIL;
        if notEmpty (5,anArgs) then
        begin
            OBJ := toObject (anArgs [5]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TStatusPanel) ) then
                f_StatusPanel := TStatusPanel (OBJ);
        end;
    except on E: Exception do
        _raise ([ 'Create', ERR_THTTPPACKAGESERVER_CREATE, E, Exception (FatalException) ],
                ['{CCE24D09-B65F-4E90-9C8C-C9F9950A7A9E}']);
    end;
end;

destructor THTTPPackageServer.Destroy;
begin
    try
        try
            FreeAndNil (f_DB);
        finally
            inherited Destroy;
        end;
    except on E: Exception do
        _raise ([ 'Destroy', ERR_THTTPPACKAGESERVER_DESTROY, E, Exception (FatalException) ],
                ['{BB9FE7C3-A458-4B76-BBDA-4589FAC8D1AA}']);
    end;
end;

procedure THTTPPackageServer.Main;
var
    ClientSock : TSocket;
    Thr        : THTTPPackageServerThread;
begin
    try
        if Terminated then Exit;
        with Sock do
        begin
            if CanRead (TimeOut) then
            begin
                ClientSock := Accept;
                if ( LastError = 0 ) and ( ClientSock > 0 ) then
                try
                    Thr := THTTPPackageServerThread.Create ([ DBFileName,
                                                              ClientSock,
                                                              TimeOut,
                                                              ListView,
                                                              Gauge,
                                                              StatusPanel ]);
                    if ( Threads.Add (Thr) < 0 ) then
                        FreeAndNil (Thr);
                except
                    FreeAndNil (Thr);
                end;
            end; 
        end;
    except on E: Exception do
        _raise ([ 'Main', ERR_THTTPPACKAGESERVER_MAIN, E, Exception (FatalException) ],
                ['{05D7A6D7-0509-4E4C-8DC9-4FF3182FC334}']);
    end;
end;

procedure THTTPPackageServer.WriteStatus (const aMessage: String);
begin
    try
        if Assigned (StatusPanel) then
        begin
            StatusPanel.Text := aMessage;
            ProcessMessages;
        end;
    except on E: Exception do
        _raise (['WriteStatus',ERR_THTTPPACKAGESERVER_WRITE_STATUS,E],
                ['{273F84AB-3B52-499B-B129-70D32BF144C3}']);
    end;
end;

procedure THTTPPackageServer.WriteStatus (const aMessage: String;
                                          aParams: array of const);
begin
    try
        WriteStatus ( Format (aMessage,aParams) );
    except on E: Exception do
        _raise (['WriteStatus',ERR_THTTPPACKAGESERVER_WRITE_STATUS,E],
                ['{46305BEA-C0A6-4E89-9912-9B77C84C98BB}']);
    end;
end;

{ THTTPPackageServerThread }
constructor THTTPPackageServerThread.Create (anArgs: array of const);
var
    I     : Integer;
    args1 : array_of_const;
    args2 : array_of_const;
    OBJ   : TObject;
begin
    try
        { второй и третий параметры - сокет и время ожидания }
        args1 := _([NIL,NIL]);
        if notEmpty (1,anArgs) then
            args1 [0] := anArgs [1];
        if notEmpty (2,anArgs) then
            args1 [1] := anArgs [2];
        { передаем параметры создания базового потока,
          начиная с седьмого аргумента }
        if ( High (anArgs) >= 6 ) then
        begin
            SetLength ( args2, High (anArgs)-6 +1 );
            for I := 6 to High (anArgs) do
                args2 [I-6] := anArgs [I];
        end
        else
            args2 := _([]);
        inherited Create ( _(args1,args2) );
        { первый параметр - файл БД }
        f_DBFileName := '';
        f_DB := NIL;
        if notEmpty (0,anArgs) then
        begin
            f_DBFileName := toString (anArgs [0]);
            f_DB := TSQLiteDatabase.Create (f_DBFileName);
        end;
        if ( not Assigned (f_DB) ) then
            raise Exception.Create (ERR_THTTPPACKAGESERVERTHREAD_INCORRECT_DATABASE);
        { четвертый параметр - ListView
          показатель прогресса загрузки }
        f_ListView := NIL;
        if notEmpty (3,anArgs) then
        begin
            OBJ := toObject (anArgs [3]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TsListView) ) then
                f_ListView := TsListView (OBJ);
        end;
        { пятый параметр - Gauge
          показатель прогресса загрузки }
        f_Gauge := NIL;
        if notEmpty (4,anArgs) then
        begin
            OBJ := toObject (anArgs [4]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TControl) ) then
                f_Gauge := TGauge (OBJ);
        end;
        { шестой параметр - StatusPanel
          показатель прогресса загрузки }
        f_StatusPanel := NIL;
        if notEmpty (5,anArgs) then
        begin
            OBJ := toObject (anArgs [5]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TStatusPanel) ) then
                f_StatusPanel := TStatusPanel (OBJ);
        end;
    except on E: Exception do
        _raise ([ 'Create', ERR_THTTPPACKAGESERVERTHREAD_CREATE, E, Exception (FatalException) ],
                ['{42006DEB-FD90-4A51-A3CB-2BFA9DA2EB6D}']);
    end;
end;

destructor THTTPPackageServerThread.Destroy;
begin
    try
        try
            FreeAndNil (f_DB);
        finally
            inherited Destroy;
        end;
    except on E: Exception do
        _raise ([ 'Destroy', ERR_THTTPPACKAGESERVERTHREAD_DESTROY, E, Exception (FatalException) ],
                ['{53EA63BC-6A17-442C-BF9F-8504C7F05C0A}']);
    end;
end;

procedure THTTPPackageServerThread.Main;
begin
    try
        inherited Main;
    except
        Terminate;
    end;
end;

procedure THTTPPackageServerThread.Return;
begin
    try
        inherited Return;
    except on E: Exception do
        WriteStatus (E.Message);
    end;
end;

function THTTPPackageServerThread.Process : WORD;
var
    Packages   : TPackages;
    I          : Integer;
    vClient    : Variant;
    uriProt    : String;
    uriUser    : String;
    uriPass    : String;
    uriHost    : String;
    uriPort    : String;
    uriPath    : String;
    uriPara    : String;
    Params     : TStringList;
    ClientHash : Hex;
    ClientID   : TID;
    Client     : TUser;
    SessionKey : Hex;
    Sign       : Hex;
    TimeStamp  : TDateTime;
begin
    Result := HTTP_STATUS_INTERNAL_SERVER_ERROR;
    try
        if ( Sock.LastError <> 0 ) then
            raise ESocketError.CreateFmt (ERR_THTTPSERVERTHREAD_SOCKET_ERROR,[Sock.LastError,Sock.LastErrorDesc]);
        { обработка пакета }
        WriteStatus (MSG_TPACKAGESERVERTHREAD_RECEIVED_PACKAGE,
                     [ Sock.GetRemoteSinIP,
                       Sock.GetRemoteSinPort ]);
        if ( Method = 'GET' ) then
        begin
            ParseURL (URI,uriProt,uriUser,uriPass,uriHost,uriPort,uriPath,uriPara);
            try
                Params := TStringList.Create;
                Params.Delimiter := '&';
                Params.DelimitedText := uriPara;
                { версия клиента }
                vClient := v(NULL_VERSION);
                vClient := Params.Values ['version'];
                { тестовый запрос }
                if notEmpty (Params.Values ['test']) then
                begin
                    WriteLog ( Format ('%s:%d',
                                       [ Sock.GetRemoteSinIP,
                                         Sock.GetRemoteSinPort ]),
                               Format ('%s:%d',
                                       [ Sock.GetLocalSinIP,
                                         Sock.GetLocalSinPort ]),
                               MSG_TPACKAGESERVERTHREAD_TEST,
                               [ Params.Values ['test'], vClient ],
                               pckTest );
                    Result := inherited Process;
                end
                { передан хэш-ключ клиента }
                else if notEmpty (Params.Values ['user']) and
                        isHex (Params.Values ['user']) then
                begin
                    Result := HTTP_STATUS_NOT_FOUND;
                    ClientHash := Params.Values ['user'];
                    ClientID := TUsers.Find (DB,ClientHash,USER_ID);
                    if ( ClientID > 0 ) then
                        Client := TUser.Load (DB,ClientID,[objSimple]) as TUser;
                    if Assigned (Client) then
                    begin
                        (*{ запрос сессионного ключа }
                        if ( Params.Values ['get_session_key'] = '1' ) then
                        try
                            { генерируем сессионный ключ }
                            SessionKey := User.Crypto.GenerateKey;
                            { удаляем записи о сессионном ключе данного клиента }
                            TSessions.Delete (DB,[ _([]),
                                                   _([ Format ('%s_session_key',[ClientHash]) ]),
                                                   _([USER_ID]) ])
                            { сохраняем сессионный ключ }
                            TimeStamp := now;
                            Session := TSession.Create (DB,[ 0,
                                                             Format ('%s_session_key',[ClientHash]),
                                                             USER_ID
                                                             TimeStamp,
                                                             TimeStamp + User.SessionTimeOut ]);
                            try
                                Session.Value := SessionKey;
                                Session.Save;
                            finally
                                FreeAndNil (Session);
                            end;
                            { в ответе сервера пишем сессионный ключ,
                              зашифрованный публичным ключом клиента
                              и подписанный приватным ключом сервера }
                            Headers.Clear;
                            Headers.Add ('Content-type: Application/hex-stream');
                            TMemoryStream (OutputData).Clear;
                            WriteStrL ( OutputData,
                                        User.Crypto.Encrypt (SessionKey,
                                                             Client.PublicKey,
                                                             User.PrivateKey) );
                            Result := HTTP_STATUS_OK;
                        finally
                            _FillChar ( SessionKey, Length (SessionKey), $00 );
                            TimeStamp := 0.0;
                        end
                        { проверяем подпись }
                        else if notEmpty (Params.Values ['sign']) then
                        begin
                            Result := HTTP_STATUS_FORBIDDEN;
                            Sessions := TSessions.Load (DB,[ _([]),
                                                              _([ Format ('%s_session_key',[ClientHash]) ]),
                                                              _([USER_ID]),
                                                              _([NIL, now]) ]) as TSessions;
                            try
                                if ( Sessions.Count = 1 ) then
                                try
                                    SessionKey := Sessions.ItemAt [0].Value;
                                    { проверяем подпись }
                                    Sign := Params.Values ['sign'];
                                    if notEmpty (SessionKey) and
                                       notEmpty (Sign) and
                                       ( User.Crypto.Decrypt (Sign,
                                                              Client.PublicKey,
                                                              User.PrivateKey) = SessionKey ) then
                                    begin
                                        { проверка подписи пройдена успешно }

                                    end;
                                finally
                                    _FillChar ( SessionKey, Length (SessionKey), $00 );
                                    _FillChar ( Sign, Length (Sign), $00 );
                                end
                                else if ( Sessions.Count > 1 ) then
                                    raise Exception.CreateFmt (ERR_THTTPPACKAGESERVERTHREAD_TOO_MANY_SESSION_KEYS,
                                                               [ Format ('%s_session_key',[ClientHash]) ]);
                            finally
                                FreeAndNil (Sessions);
                            end;
                        end;*)
                    end;
                end;
            finally
                _FillChar ( uriProt, Length (uriProt), $00 );
                _FillChar ( uriUser, Length (uriUser), $00 );
                _FillChar ( uriPass, Length (uriPass), $00 );
                _FillChar ( uriHost, Length (uriHost), $00 );
                _FillChar ( uriPort, Length (uriPort), $00 );
                _FillChar ( uriPath, Length (uriPath), $00 );
                _FillChar ( uriPara, Length (uriPara), $00 );
                FreeAndNil (Params);
                _FillChar ( ClientHash, Length (ClientHash), $00 );
                ClientID := 0;
                FreeAndNil (Client);
            end;
            { запрос пакетов }
            // клиент делает запрос
            // сервер отсылает сессионный ключ
            // клиент подписывает сессионный ключ
            // клиент посылает серверу подписанный ключ
        end
        else if ( Method = 'POST' ) then
        begin
            Headers.Clear;
            Headers.Add ('Content-type: Application/hex-stream');

            TMemoryStream (OutputData).Clear;

            Packages := TPackages.Create (DB,[]);
            try
                try
                    InputData.Position := 0;
                    Packages.LoadFromStream (InputData);
                except on E: Exception do begin
                    WriteLog ( Format ('%s:%d',
                                       [ Sock.GetRemoteSinIP,
                                         Sock.GetRemoteSinPort ]),
                               Format ('%s:%d',
                                       [ Sock.GetLocalSinIP,
                                         Sock.GetLocalSinPort ]),
                               ERR_THTTPPACKAGESERVERTHREAD_LOAD_PACKAGES,
                               pckError );
                end; end;
                for I := 0 to Packages.Count-1 do
                try
                    { проверяем - новый ли пакет? }
                    if not (  TPackages.Find ( DB,
                                               Packages.ItemAt [I].KeyHash,
                                               USER_ID ) > 0  ) then
                    begin
                        { пришел новый пакет - пакет получен }
                        Packages.ItemAt [I].IDStatus := PACKAGE_RECEIVED_STATUS_ID;
                        Packages.ItemAt [I].Save;
                        { пишем в лог статус пакета }
                        WriteLog ( Format ('%s:%d',
                                           [ Sock.GetRemoteSinIP,
                                             Sock.GetRemoteSinPort ]),
                                   Format ('%s:%d',
                                           [ Sock.GetLocalSinIP,
                                             Sock.GetLocalSinPort ]),
                                   Format (MSG_TPACKAGESERVERTHREAD_SAVED_PACKAGE,
                                           [ Packages.ItemAt [I].KeyHash ]),
                                   pckReceive,
                                   GetPckTypeExternal (Packages.ItemAt [I].IDType),
                                   Packages.ItemAt [I].MetaClass.GetClassID );
                    end;
                    { пишем ответ сервера по каждому пакету }
                    WriteStrL ( OutputData, Packages.ItemAt [I].KeyHash );
                    WriteStrL (  OutputData, StrToHex ( IntToStr (Packages.ItemAt [I].IDStatus) )  );
                except on SaveError: Exception do begin
                    if Assigned (Packages.ItemAt [I]) then
                    begin
                        { пишем ответ сервера по каждому пакету }
                        WriteStrL ( OutputData, Packages.ItemAt [I].KeyHash );
                        WriteStrL (  OutputData, StrToHex ( IntToStr (PACKAGE_REJECTED_STATUS_ID) )  );
                        { пишем в лог причину ошибки }
                        WriteLog ( Format ('%s:%d',
                                           [ Sock.GetRemoteSinIP,
                                             Sock.GetRemoteSinPort ]),
                                   Format ('%s:%d',
                                           [ Sock.GetLocalSinIP,
                                             Sock.GetLocalSinPort ]),
                                   Format ('%s : %s',
                                           [ ERR_THTTPPACKAGESERVERTHREAD_INVALID_PACKAGE,
                                             SaveError.Message ]),
                                   pckError );
                    end
                    else
                        WriteStatus (SaveError.Message);
                end; end;
                Result := HTTP_STATUS_OK;
            finally
                FreeAndNil (Packages);
            end;

        end
        else
        begin
            WriteLog ( Format ('%s:%d',
                               [ Sock.GetRemoteSinIP,
                                 Sock.GetRemoteSinPort ]),
                       Format ('%s:%d',
                               [ Sock.GetLocalSinIP,
                                 Sock.GetLocalSinPort ]),
                       Format ('%s : ''%s''',
                               [ ERR_THTTPPACKAGESERVERTHREAD_IVALID_METHOD,
                                 Method ]),
                       pckError );
            Result := HTTP_STATUS_BAD_REQUEST;
        end;
    except on E: Exception do
        _raise ([ 'Process', ERR_THTTPPACKAGESERVERTHREAD_PROCESS, E, Exception (FatalException) ],
                ['{729EEE41-8224-4965-BFFA-C2B3BA669B54}']);
    end;
end;

procedure THTTPPackageServerThread.ProcessPackage (aPackage: TPackage);
begin
    if not Assigned (aPackage) then
        raise Exception.Create (ERR_THTTPPACKAGESERVERTHREAD_INVALID_PACKAGE);
    WriteStatus (MSG_TPACKAGESERVERTHREAD_EXECUTING_PACKAGE,
                 [ aPackage.KeyHash ]);
    aPackage.ProcessMetaData;
end;

procedure THTTPPackageServerThread.WriteStatus (const aMessage: String);
begin
    try
        if Assigned (StatusPanel) then
        begin
            StatusPanel.Text := aMessage;
            ProcessMessages;
        end;
    except on E: Exception do
        _raise (['WriteStatus',ERR_THTTPPACKAGESERVERTHREAD_WRITE_STATUS,E],
                ['{201B6CBB-B248-43D5-B702-670DE570052E}']);
    end;
end;

procedure THTTPPackageServerThread.WriteStatus (const aMessage: String;
                                                aParams: array of const);
begin
    try
        WriteStatus ( Format (aMessage,aParams) );
    except on E: Exception do
        _raise (['WriteStatus',ERR_THTTPPACKAGESERVERTHREAD_WRITE_STATUS,E],
                ['{9976176A-96ED-4CE5-8549-D2F5F696A9BD}']);
    end;
end;

procedure THTTPPackageServerThread.WriteLog (const aSender: String;
                                             const aReceiver: String;
                                             const aMessage: String;
                                             const anImageIndex: Integer = pckDefault;
                                             const aPackageType: String = '';
                                             const aMetaClassID: String = '');
var
    Itm : TListItem;
begin
    try
        if Assigned (ListView) then
        begin
            Itm := ListView.Items.Add;
            Itm.Caption := _DateTimeToStr (now);
            Itm.ImageIndex := anImageIndex;
            Itm.SubItems.Add (aSender);
            Itm.SubItems.Add (aReceiver);
            if notEmpty (aMetaClassID) then
                Itm.SubItems.Add ( Format ('%s : %s : %s',[aMetaClassID,aPackageType,aMessage]) )
            else
                Itm.SubItems.Add (aMessage);
        end;
    except on E: Exception do
        _raise (['WriteLog',ERR_THTTPPACKAGESERVERTHREAD_WRITE_LOG,E],
                ['{E9F9E881-D650-405C-8AF4-50B817650EB8}']);
    end;
end;

procedure THTTPPackageServerThread.WriteLog (const aSender: String;
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
        _raise (['WriteLog',ERR_THTTPPACKAGESERVERTHREAD_WRITE_LOG,E],
                ['{781BA73B-A41E-44C0-A8EA-90AB08DFC138}']);
    end;
end;


end.
