unit uUsersLoader;
{******************************************************************************}
{*  Users Loader Unit                                                         *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
interface

{$I 'std.inc'}

uses
    Windows, SysUtils, Variants, Classes, Graphics,
    Controls, ComCtrls, CommCtrl, Gauges,
    ImgList, acAlphaImageList, jpeg, pngimage, acPNG,
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

type
{ загрузчик пользователей }
{$M+}
    CUsersLoader = class of TUsersLoader;
    PUsersLoader = ^TUsersLoader;
    TUsersLoader = class (TDllThread)
    private
        f_DBFileName: String;        { файл базы данных }
        f_DB: TSQLiteDatabase;       { объект базы данных }
        f_TreeNodes: TTreeNodes;     { дерево }
        f_Images: TsAlphaImageList;  { набор пиктограмм }
        f_Gauge: TGauge;             { показатель прогресса загрузки }
        f_StatusPanel: TStatusPanel; { строка состояния загрузки }
        f_ParentNode: TTreeNode;     { начальный узел }
        f_Users: TUsers;             { список пользователей }
        f_UserIndex: LongInt;        { позиция в обходе списка }
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
    public
        property DBFileName: String read f_DBFileName;
        property DB: TSQLiteDatabase read f_DB write f_DB;
        property TreeNodes: TTreeNodes read f_TreeNodes write f_TreeNodes;
        property Images: TsAlphaImageList read f_Images write f_Images;
        property Gauge: TGauge read f_Gauge write f_Gauge;
        property StatusPanel: TStatusPanel read f_StatusPanel write f_StatusPanel;
        property ParentNode: TTreeNode read f_ParentNode write f_ParentNode;
        property Users: TUsers read f_Users write f_Users;
        property UserIndex: LongInt read f_UserIndex write f_UserIndex;
    end;
{$M-}

{ TUsersLoader Errors }
resourcestring
    ERR_TUSERSLOADER_INCORRECT_DATABASE     = 'Не инициализирован объект БД!';
    ERR_TUSERSLOADER_INCORRECT_NODES        = 'Объект дерева не инициализирован!';
    ERR_TUSERSLOADER_INCORRECT_NODE         = 'Объект узла дерева не инициализирован!';
    ERR_TUSERSLOADER_INCORRECT_ID           = 'Узел дерева содержит некорректный идентификатор!';
    ERR_TUSERSLOADER_INCORRECT_IMAGES       = 'Контейнер для изображений не инициализирован!';
    ERR_TUSERSLOADER_INCORRECT_GAUGE        = 'Не инициализирован элемент контроля прогресса!';
    ERR_TUSERSLOADER_INCORRECT_STATUS_PANEL = 'Не инициализирована строка состояния!';
    ERR_TUSERSLOADER_CREATE                 = 'Ошибка создания потока загрузки списка пользователей!';
    ERR_TUSERSLOADER_DESTROY                = 'Ошибка уничтожения потока загрузки списка пользователей!';
    ERR_TUSERSLOADER_MAIN                   = 'Ошибка главной функции потока!';
    ERR_TUSERSLOADER_RETURN                 = 'Ошибка функции возврата потока!';
    ERR_TUSERSLOADER_IMAGE_LOAD_FAILED      = 'Ошибка загрузки изображения: %s';
    ERR_TUSERSLOADER_WRITE_STATUS           = 'Ошибка отображения статуса!';

{ TUsersLoader Hints }
resourcestring
    MSG_TUSERSLOADER_LOAD_OBJECT          = 'Загрузка ''%s'' ...';
    MSG_TUSERSLOADER_LOAD_OBJECT_PROPERTY = 'Загрузка ''%s'' ... %s';

implementation

{ TUsersLoader }
constructor TUsersLoader.Create (anArgs: array of const);
var
    I    : Integer;
    args : array_of_const;
    OBJ  : TObject;
begin
    try
        { передаем параметры создания базового потока,
          начиная с седьмого аргумента }
        if ( High (anArgs) >= 6 ) then
        begin
            SetLength ( Args, High (anArgs)-6 +1 );
            for I := 6 to High (anArgs) do
                args [I-6] := anArgs [I];
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
            raise Exception.Create (ERR_TUSERSLOADER_INCORRECT_DATABASE);
        { второй параметр - TreeNodes
          дерево, куда будут загружаться пользователи }
        f_TreeNodes := NIL;
        if notEmpty (1,anArgs) then
        begin
            OBJ := toObject (anArgs [1]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TTreeNodes) ) then
                f_TreeNodes := TTreeNodes (OBJ);
        end;
        if ( not Assigned (f_TreeNodes) ) then
            raise Exception.Create (ERR_TUSERSLOADER_INCORRECT_NODES);
        { третий параметр - ParentNode
          корневой узел }
        f_ParentNode := NIL;
        if notEmpty (2,anArgs) then
        begin
            OBJ := toObject (anArgs [2]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TTreeNode) ) then
                f_ParentNode := TTreeNode (OBJ);
        end;
        { четвертый параметр - Images
          контейнер для аватарок пользователей }
        f_Images := NIL;
        if notEmpty (3,anArgs) then
        begin
            OBJ := toObject (anArgs [3]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TsAlphaImageList) ) then
                f_Images := TsAlphaImageList (OBJ);
        end;
        {if ( not Assigned (f_Images) ) then
            raise Exception.Create (ERR_TUSERSLOADER_INCORRECT_IMAGES);}
        {if ( not Assigned (f_ParentNode) ) then
            raise Exception.Create (ERR_TUSERSLOADER_INCORRECT_NODE);}
        { пятый параметр - Gauge
          показатель прогресса загрузки }
        f_Gauge := NIL;
        if notEmpty (4,anArgs) then
        begin
            OBJ := toObject (anArgs [4]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TControl) ) then
                f_Gauge := TGauge (OBJ);
        end;
        {if ( not Assigned (f_Gauge) ) then
            raise Exception.Create (ERR_TUSERSLOADER_INCORRECT_GAUGE);}
        { шестой параметр - StatusPanel
          показатель прогресса загрузки }
        f_StatusPanel := NIL;
        if notEmpty (5,anArgs) then
        begin
            OBJ := toObject (anArgs [5]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TStatusPanel) ) then
                f_StatusPanel := TStatusPanel (OBJ);
        end;
        {if ( not Assigned (f_StatusPanel) ) then
            raise Exception.Create (ERR_TUSERSLOADER_INCORRECT_STATUS_PANEL);}
        f_Users := NIL;
        f_UserIndex := -1;
    except on E: Exception do
        _raise ([ 'Create', ERR_TUSERSLOADER_CREATE, E, Exception (FatalException) ],
                ['{09C45EE2-687D-48FA-9983-6EC266F4DC8F}']);
    end;
end;

destructor TUsersLoader.Destroy;
begin
    try
        FreeAndNil (f_Users);
        FreeAndNil (f_DB);
        inherited Destroy;
    except on E: Exception do
        _raise ([ 'Destroy', ERR_TUSERSLOADER_DESTROY, E, Exception (FatalException) ],
                ['{4B2C7575-FFC3-4104-B9F7-D9D756D1D6B2}']);
    end;
end;

procedure TUsersLoader.Main;
begin
    try
        if Terminated then Exit;
        inherited Main;
        if not Assigned (Users) then
            f_Users := TUsers.Load (DB,[ _([]),
                                         _([]),
                                         _([USER_ID]) ]) as TUsers;
    except on E: Exception do
        _raise ([ 'Main', ERR_TUSERSLOADER_MAIN, E, Exception (FatalException) ],
                ['{87F29C26-7A30-4714-B7B5-0D9BECD76150}']);
    end;
end;

procedure TUsersLoader.Return;
var
    Node : TTreeNode;
    Bmp  : TBitMap;
begin
    try
        if Terminated then Exit;
        if ( not Assigned (Users) or (Users.Count <= 0) ) then
        begin
            SetTabStatus (tabUsers,tbsLoaded);
            Terminate;
        end
        else if UserIndex >= Users.Count-1 then
        begin
            SetTabStatus (tabUsers,tbsLoaded);
            Terminate;
        end
        else
        try
            inherited Return;
            UserIndex := UserIndex + 1;
            if Assigned (StatusPanel) then
                StatusPanel.Text := Format (MSG_TUSERSLOADER_LOAD_OBJECT,
                                            [ Users.ItemAt [UserIndex].Login ]);
            Node := TreeNodes.AddChild ( ParentNode,
                                         {Format (' %s '#13#10' %s ',
                                                  [ Users.ItemAt [UserIndex].Login,
                                                    Users.ItemAt [UserIndex].Description ])}
                                         ''
                                        );
            Node.ImageIndex := -1;
            Node.SelectedIndex := -1;
            Node.Data := CreateUserNode;
            with PUserNode (Node.Data)^ do
            begin
                //WriteStatus (MSG_TUSERSLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Users.ItemAt [UserIndex].Login,
                //               Users.ItemAt [UserIndex].PropAt [_usr_id].Caption ]);
                ID := Users.ItemAt [UserIndex].ID;
                //WriteStatus (MSG_TUSERSLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Users.ItemAt [UserIndex].Login,
                //               Users.ItemAt [UserIndex].PropAt [_usr_id_pic].Caption ]);
                IDPic := Users.ItemAt [UserIndex].IDPic;
                //WriteStatus (MSG_TUSERSLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Users.ItemAt [UserIndex].Login,
                //               Users.ItemAt [UserIndex].PropAt [_usr_login].Caption ]);
                Login := Users.ItemAt [UserIndex].Login;
                //WriteStatus (MSG_TUSERSLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Users.ItemAt [UserIndex].Login,
                //               Users.ItemAt [UserIndex].PropAt [_usr_description].Caption ]);
                Description := Users.ItemAt [UserIndex].Description;
                //WriteStatus (MSG_TUSERSLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Users.ItemAt [UserIndex].Login,
                //               Users.ItemAt [UserIndex].PropAt [_usr_public_key].Caption ]);
                PublicKey := Users.ItemAt [UserIndex].PublicKey;
                //WriteStatus (MSG_TUSERSLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Users.ItemAt [UserIndex].Login,
                //               Users.ItemAt [UserIndex].PropAt [_usr_time_stamp_create].Caption ]);
                TimeStampCreate := Users.ItemAt [UserIndex].TimeStampCreate;
                //WriteStatus (MSG_TUSERSLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Users.ItemAt [UserIndex].Login,
                //               Users.ItemAt [UserIndex].PropAt [_usr_time_stamp_modify].Caption ]);
                TimeStampModify := Users.ItemAt [UserIndex].TimeStampModify;
                //WriteStatus (MSG_TUSERSLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Users.ItemAt [UserIndex].Login,
                //               Users.ItemAt [UserIndex].PropAt [_usr_time_stamp_public].Caption ]);
                TimeStampPublic := Users.ItemAt [UserIndex].TimeStampPublic;
                //WriteStatus (MSG_TUSERSLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Users.ItemAt [UserIndex].Login,
                //               Users.ItemAt [UserIndex].PropAt [_usr_key_hash].Caption ]);
                KeyHash := Users.ItemAt [UserIndex].KeyHash;
                //WriteStatus (MSG_TUSERSLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Users.ItemAt [UserIndex].Login,
                //               Users.ItemAt [UserIndex].PropAt [_usr_data_hash].Caption ]);
                DataHash := Users.ItemAt [UserIndex].DataHash;
            end;
            if Assigned (Images) and
               ( Users.ItemAt [UserIndex].IDPic > 0 ) then
            try
                //WriteStatus (MSG_TUSERSLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Users.ItemAt [UserIndex].Login,
                //               Users.ItemAt [UserIndex].Pic.GetClassName ]);
                Images.AcBeginUpdate;
                Bmp := TBitMap.Create;
                try
                    if Assigned ( Users.ItemAt [UserIndex].Pic.Picture ) and
                       ( (Users.ItemAt [UserIndex].Pic.Picture.Width > 0) or
                         (Users.ItemAt [UserIndex].Pic.Picture.Height > 0) ) then
                        Bmp.Assign (Users.ItemAt [UserIndex].Pic.Bitmap)
                    {else if Assigned (MainForm) then
                        Bmp.Assign (MainForm.imgAnonymous.Picture.Graphic)};
                    ResizeImage (Bmp,Images.Width,Images.Height,2,Images.BkColor);
                    Images.Add (Bmp,NIL);
                    Node.ImageIndex    := UserIndex;
                    Node.SelectedIndex := UserIndex;
                finally
                    FreeAndNil (Bmp);
                    Images.AcEndUpdate;
                end;
            except on E: Exception do begin
                Node.ImageIndex := -1;
                Node.SelectedIndex := -1;
                WriteStatus (ERR_TUSERSLOADER_IMAGE_LOAD_FAILED,[E.Message]);
            end; end;
            if Assigned (Gauge) then
                Gauge.Progress := Gauge.Progress + 1;
        except on E: Exception do
            WriteStatus (E.Message);
        end;
    except on E: Exception do
        _raise ([ 'Return', ERR_TUSERSLOADER_RETURN, E, Exception (FatalException) ],
                ['{A99D218D-5E59-4A6E-9E8A-B2B41D785941}']);
    end;
end;

procedure TUsersLoader.WriteStatus (const aMessage: String);
begin
    try
        if Assigned (StatusPanel) then
        begin
            StatusPanel.Text := aMessage;
            ProcessMessages;
        end;
    except on E: Exception do
        _raise (['WriteStatus',ERR_TUSERSLOADER_WRITE_STATUS,E],
                ['{09F81E23-AE5B-4876-8D81-288D2418E508}']);
    end;
end;

procedure TUsersLoader.WriteStatus (const aMessage: String;
                                    aParams: array of const);
begin
    try
        WriteStatus ( Format (aMessage,aParams) );
    except on E: Exception do
        _raise (['WriteStatus',ERR_TUSERSLOADER_WRITE_STATUS,E],
                ['{E5135175-7326-4CC4-8B15-1E2424B1F2B4}']);
    end;
end;


end.
