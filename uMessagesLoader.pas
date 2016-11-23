unit uMessagesLoader;
{******************************************************************************}
{*  Messages Loader Unit                                                      *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
interface

{$I 'std.inc'}

uses
    Windows, SysUtils, Variants, Classes, Graphics, 
    Controls, ComCtrls, CommCtrl, Forms, Gauges,
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
{ загрузчик сообщений }
{$M+}
    CMessagesLoader = class of TMessagesLoader;
    PMessagesLoader = ^TMessagesLoader;
    TMessagesLoader = class (TDllThread)
    private
        f_DBFileName: String;        { файл базы данных }
        f_DB: TSQLiteDatabase;       { объект базы данных }
        f_Box: TScrollingWinControl; { контейнер }
        f_Gauge: TGauge;             { показатель прогресса загрузки }
        f_StatusPanel: TStatusPanel; { строка состояния загрузки }
        f_CategorieID: TID;          { категория }
        f_Messages: TMessages;       { список сообщений }
        f_MessageIndex: LongInt;     { позиция в обходе списка }
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
        property Box: TScrollingWinControl read f_Box write f_Box;
        property Gauge: TGauge read f_Gauge write f_Gauge;
        property StatusPanel: TStatusPanel read f_StatusPanel write f_StatusPanel;
        property CategorieID: TID read f_CategorieID write f_CategorieID;
        property Messages: TMessages read f_Messages write f_Messages;
        property MessageIndex: LongInt read f_MessageIndex write f_MessageIndex;
    end;
{$M-}

{ TMessagesLoader Errors }
resourcestring
    ERR_TMESSAGESLOADER_INCORRECT_DATABASE     = 'Не инициализирован объект БД!';
    ERR_TMESSAGESLOADER_INCORRECT_BOX          = 'Объект контейнера инициализирован!';
    ERR_TMESSAGESLOADER_INCORRECT_NODE         = 'Объект содержимого контейнера не инициализирован!';
    ERR_TMESSAGESLOADER_INCORRECT_ID           = 'Узел дерева содержит некорректный идентификатор!';
    ERR_TMESSAGESLOADER_INCORRECT_GAUGE        = 'Не инициализирован элемент контроля прогресса!';
    ERR_TMESSAGESLOADER_INCORRECT_STATUS_PANEL = 'Не инициализирована строка состояния!';
    ERR_TMESSAGESLOADER_CREATE                 = 'Ошибка создания потока загрузки списка сообщений!';
    ERR_TMESSAGESLOADER_DESTROY                = 'Ошибка уничтожения потока загрузки списка сообщений!';
    ERR_TMESSAGESLOADER_MAIN                   = 'Ошибка главной функции потока!';
    ERR_TMESSAGESLOADER_RETURN                 = 'Ошибка функции возврата потока!';
    ERR_TMESSAGESLOADER_IMAGE_LOAD_FAILED      = 'Ошибка загрузки изображения: %s';
    ERR_TMESSAGESLOADER_WRITE_STATUS           = 'Ошибка отображения статуса!';

{ TMessagesLoader Hints }
resourcestring
    MSG_TMESSAGESLOADER_LOAD_OBJECT          = 'Загрузка ''%s'' ...';
    MSG_TMESSAGESLOADER_LOAD_OBJECT_PROPERTY = 'Загрузка ''%s'' ... %s';

implementation

uses
    uTmpMessage;

{ TMessagesLoader }
constructor TMessagesLoader.Create (anArgs: array of const);
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
            raise Exception.Create (ERR_TMESSAGESLOADER_INCORRECT_DATABASE);
        { второй параметр - Box
          контейнер, куда будут загружаться сообщения }
        f_Box := NIL;
        if notEmpty (1,anArgs) then
        begin
            OBJ := toObject (anArgs [1]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TScrollingWinControl) ) then
                f_Box := TScrollingWinControl (OBJ);
        end;
        if ( not Assigned (f_Box) ) then
            raise Exception.Create (ERR_TMESSAGESLOADER_INCORRECT_BOX);
        { третий параметр - CategorieID
          идентификатор текущей категории }
        f_CategorieID := 0;
        if notEmpty (2,anArgs) then
            f_CategorieID := toInt64 (anArgs [2]);
        if ( f_CategorieID <= 0 ) then
            raise Exception.Create (ERR_TMESSAGESLOADER_INCORRECT_ID);
        { четвертый параметр - Gauge
          показатель прогресса загрузки }
        f_Gauge := NIL;
        if notEmpty (3,anArgs) then
        begin
            OBJ := toObject (anArgs [3]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TControl) ) then
                f_Gauge := TGauge (OBJ);
        end;
        {if ( not Assigned (f_Gauge) ) then
            raise Exception.Create (ERR_TMESSAGESLOADER_INCORRECT_GAUGE);}
        { пятый параметр - StatusPanel
          показатель прогресса загрузки }
        f_StatusPanel := NIL;
        if notEmpty (4,anArgs) then
        begin
            OBJ := toObject (anArgs [4]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TStatusPanel) ) then
                f_StatusPanel := TStatusPanel (OBJ);
        end;
        {if ( not Assigned (f_StatusPanel) ) then
            raise Exception.Create (ERR_TMESSAGESLOADER_INCORRECT_STATUS_PANEL);}
        f_Messages := NIL;
        f_MessageIndex := -1;
    except on E: Exception do
        _raise ([ 'Create', ERR_TMESSAGESLOADER_CREATE, E, Exception (FatalException) ],
                ['{8FB83F58-CE7A-417C-A19C-2F4B03B87FBA}']);
    end;
end;

destructor TMessagesLoader.Destroy;
begin
    try
        FreeAndNil (f_Messages);
        FreeAndNil (f_DB);
        inherited Destroy;
    except on E: Exception do
        _raise ([ 'Destroy', ERR_TMESSAGESLOADER_DESTROY, E, Exception (FatalException) ],
                ['{F318EC71-1316-4319-ADC1-82F3AC1F17E1}']);
    end;
end;

procedure TMessagesLoader.Main;
begin
    try
        if Terminated then Exit;
        inherited Main;
        if not Assigned (Messages) then
            f_Messages := TMessages.Load (DB,[ _([]),
                                               _([CategorieID]),
                                               _([]),
                                               _([USER_ID]),
                                               _([MESSAGE_FORUM_TYPE_ID]),
                                               _([MESSAGE_ACTIVE_STATUS_ID]) ],
                                             [ _msg_time_stamp_create,
                                               'DESC' ]) as TMessages;
    except on E: Exception do
        _raise ([ 'Main', ERR_TMESSAGESLOADER_MAIN, E, Exception (FatalException) ],
                ['{3AD44240-8FDC-408B-861B-2017F9B3C14D}']);
    end;
end;

procedure TMessagesLoader.Return;
var
    Node : TTmpMessage;
    Bmp  : TBitMap;
begin
    try
        if Terminated then Exit;
        if ( not Assigned (Messages) or (Messages.Count <= 0) ) then
        begin
            SetTabStatus (tabForum,tbsLoaded);
            Terminate;
        end
        else if MessageIndex >= Messages.Count-1 then
        begin
            SetTabStatus (tabForum,tbsLoaded);
            Terminate;
        end
        else
        try
            inherited Return;
            MessageIndex := MessageIndex + 1;
            if Assigned (StatusPanel) then
                StatusPanel.Text := Format (MSG_TMESSAGESLOADER_LOAD_OBJECT,
                                            [ Messages.ItemAt [MessageIndex].Subject ]);
            Node := TTmpMessage.Create (Box,MessageIndex);
            with PMessageNode (Node.Data)^ do
            begin
                //WriteStatus (MSG_TMESSAGESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Messages.ItemAt [MessageIndex].Subject,
                //               Messages.ItemAt [MessageIndex].PropAt [_msg_id].Caption ]);
                ID := Messages.ItemAt [MessageIndex].ID;
                //WriteStatus (MSG_TMESSAGESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Messages.ItemAt [MessageIndex].Subject,
                //               Messages.ItemAt [MessageIndex].PropAt [_msg_id_categorie].Caption ]);
                IDCategorie := Messages.ItemAt [MessageIndex].IDCategorie;
                //WriteStatus (MSG_TMESSAGESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Messages.ItemAt [MessageIndex].Subject,
                //               Messages.ItemAt [MessageIndex].PropAt [_msg_id_author].Caption ]);
                IDAuthor := Messages.ItemAt [MessageIndex].IDAuthor;
                //WriteStatus (MSG_TMESSAGESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Messages.ItemAt [MessageIndex].Subject,
                //               Messages.ItemAt [MessageIndex].PropAt [_msg_id_type].Caption ]);
                IDType := Messages.ItemAt [MessageIndex].IDType;
                //WriteStatus (MSG_TMESSAGESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Messages.ItemAt [MessageIndex].Subject,
                //               Messages.ItemAt [MessageIndex].PropAt [_msg_id_status].Caption ]);
                IDStatus := Messages.ItemAt [MessageIndex].IDStatus;
                //WriteStatus (MSG_TMESSAGESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Messages.ItemAt [MessageIndex].Subject,
                //               Messages.ItemAt [MessageIndex].PropAt [_msg_id_pic].Caption ]);
                IDPic := Messages.ItemAt [MessageIndex].IDPic;
                //WriteStatus (MSG_TMESSAGESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Messages.ItemAt [MessageIndex].Subject,
                //               Messages.ItemAt [MessageIndex].PropAt [_msg_index_pic].Caption ]);
                IndexPic := Messages.ItemAt [MessageIndex].IndexPic;
                //WriteStatus (MSG_TMESSAGESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Messages.ItemAt [MessageIndex].Subject,
                //               Messages.ItemAt [MessageIndex].PropAt [_msg_subject].Caption ]);
                Subject := Messages.ItemAt [MessageIndex].Subject;
                //WriteStatus (MSG_TMESSAGESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Messages.ItemAt [MessageIndex].Subject,
                //               Messages.ItemAt [MessageIndex].PropAt [_msg_text].Caption ]);
                CopyMemory ( Text, PChar (Messages.ItemAt [MessageIndex].Text), MAX_MESSAGE_LENGTH );
                //WriteStatus (MSG_TMESSAGESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Messages.ItemAt [MessageIndex].Subject,
                //               Messages.ItemAt [MessageIndex].Author.PropAt [_usr_login].Caption ]);
                Author := Messages.ItemAt [MessageIndex].Author.Login;
                //WriteStatus (MSG_TMESSAGESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Messages.ItemAt [MessageIndex].Subject,
                //               Messages.ItemAt [MessageIndex].PropAt [_msg_time_stamp_create].Caption ]);
                //WriteStatus (MSG_TMESSAGESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Messages.ItemAt [MessageIndex].Subject,
                //               Messages.ItemAt [MessageIndex].PropAt [_msg_time_stamp_create].Caption ]);
                TimeStampCreate := Messages.ItemAt [MessageIndex].TimeStampCreate;
                //WriteStatus (MSG_TMESSAGESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Messages.ItemAt [MessageIndex].Subject,
                //               Messages.ItemAt [MessageIndex].PropAt [_msg_time_stamp_modify].Caption ]);
                TimeStampModify := Messages.ItemAt [MessageIndex].TimeStampModify;
                //WriteStatus (MSG_TMESSAGESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Messages.ItemAt [MessageIndex].Subject,
                //               Messages.ItemAt [MessageIndex].PropAt [_msg_time_stamp_public].Caption ]);
                TimeStampPublic := Messages.ItemAt [MessageIndex].TimeStampPublic;
                //WriteStatus (MSG_TMESSAGESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Messages.ItemAt [MessageIndex].Subject,
                //               Messages.ItemAt [MessageIndex].PropAt [_msg_key_hash].Caption ]);
                KeyHash := Messages.ItemAt [MessageIndex].KeyHash;
                //WriteStatus (MSG_TMESSAGESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Messages.ItemAt [MessageIndex].Subject,
                //               Messages.ItemAt [MessageIndex].PropAt [_msg_data_hash].Caption ]);
                DataHash := Messages.ItemAt [MessageIndex].DataHash;
            end;
            if Assigned (Gauge) then
                Gauge.Progress := Gauge.Progress + 1;
        except on E: Exception do
            WriteStatus (E.Message);
        end;
    except on E: Exception do
        _raise ([ 'Return', ERR_TMESSAGESLOADER_RETURN, E, Exception (FatalException) ],
                ['{E5E815D6-1923-453F-A98C-4D7EF74C0997}']);
    end;
end;

procedure TMessagesLoader.WriteStatus (const aMessage: String);
begin
    try
        if Assigned (StatusPanel) then
        begin
            StatusPanel.Text := aMessage;
            ProcessMessages;
        end;
    except on E: Exception do
        _raise (['WriteStatus',ERR_TMESSAGESLOADER_WRITE_STATUS,E],
                ['{1D9CEE12-B437-4159-A763-F03790771B12}']);
    end;
end;

procedure TMessagesLoader.WriteStatus (const aMessage: String;
                                       aParams: array of const);
begin
    try
        WriteStatus ( Format (aMessage,aParams) );
    except on E: Exception do
        _raise (['WriteStatus',ERR_TMESSAGESLOADER_WRITE_STATUS,E],
                ['{85AFD790-ADF5-4AA9-B658-812FA8257E26}']);
    end;
end;


end.
