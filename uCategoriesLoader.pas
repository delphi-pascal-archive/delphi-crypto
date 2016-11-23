unit uCategoriesLoader;
{******************************************************************************}
{*  Categories Loader Unit                                                    *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
interface

{$I 'std.inc'}

uses
    Windows, SysUtils, Variants, Classes,
    Controls, ComCtrls, Gauges,
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
{ загрузчик категорий }
{$M+}
    CCategoriesLoader = class of TCategoriesLoader;
    PCategoriesLoader = ^TCategoriesLoader;
    TCategoriesLoader = class (TDllThread)
    private
        f_DBFileName: String;        { файл базы данных }
        f_DB: TSQLiteDatabase;       { объект базы данных }
        f_TreeNodes: TTreeNodes;     { дерево }
        f_Gauge: TGauge;             { показатель прогресса загрузки }
        f_StatusPanel: TStatusPanel; { строка состояния загрузки }
        f_ParentNode: TTreeNode;     { узел, содержащий идентификатор предка текущей категории }
        f_CategorieID: TID;          { идентификатор текущей категории }
        f_Children: TCategories;     { потомки текущей категории }
        f_ChildIndex: LongInt;       { позиция в обходе потомков }
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
        property Gauge: TGauge read f_Gauge write f_Gauge;
        property StatusPanel: TStatusPanel read f_StatusPanel write f_StatusPanel;
        property ParentNode: TTreeNode read f_ParentNode write f_ParentNode;
        property CategorieID: TID read f_CategorieID write f_CategorieID;
        property Children: TCategories read f_Children write f_Children;
        property ChildIndex: LongInt read f_ChildIndex write f_ChildIndex;
    end;
{$M-}

{ TCategoriesLoader Errors }
resourcestring
    ERR_TCATEGORIESLOADER_INCORRECT_DATABASE     = 'Не инициализирован объект БД!';
    ERR_TCATEGORIESLOADER_INCORRECT_NODES        = 'Объект дерева не инициализирован!';
    ERR_TCATEGORIESLOADER_INCORRECT_NODE         = 'Объект узла дерева не инициализирован!';
    ERR_TCATEGORIESLOADER_INCORRECT_ID           = 'Узел дерева содержит некорректный идентификатор!';
    ERR_TCATEGORIESLOADER_INCORRECT_GAUGE        = 'Не инициализирован элемент контроля прогресса!';
    ERR_TCATEGORIESLOADER_INCORRECT_STATUS_PANEL = 'Не инициализирована строка состояния!';
    ERR_TCATEGORIESLOADER_CREATE                 = 'Ошибка создания потока загрузки дерева категорий!';
    ERR_TCATEGORIESLOADER_DESTROY                = 'Ошибка уничтожения потока загрузки дерева категорий!';
    ERR_TCATEGORIESLOADER_MAIN                   = 'Ошибка главной функции потока!';
    ERR_TCATEGORIESLOADER_RETURN                 = 'Ошибка функции возврата потока!';
    ERR_TCATEGORIESLOADER_WRITE_STATUS           = 'Ошибка отображения статуса!';

{ TCategoriesLoader Hints }
resourcestring
    MSG_TCATEGORIESLOADER_LOAD_OBJECT          = 'Загрузка ''%s'' ...';
    MSG_TCATEGORIESLOADER_LOAD_OBJECT_PROPERTY = 'Загрузка ''%s'' ... %s';

implementation

{ TCategoriesLoader }
constructor TCategoriesLoader.Create (anArgs: array of const);
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
            raise Exception.Create (ERR_TCATEGORIESLOADER_INCORRECT_DATABASE);
        { второй параметр - TreeNodes
          дерево, куда будут загружаться категории }
        f_TreeNodes := NIL;
        if notEmpty (1,anArgs) then
        begin
            OBJ := toObject (anArgs [1]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TTreeNodes) ) then
                f_TreeNodes := TTreeNodes (OBJ);
        end;
        if ( not Assigned (f_TreeNodes) ) then
            raise Exception.Create (ERR_TCATEGORIESLOADER_INCORRECT_NODES);
        { третий параметр - ParentNode
          узел, содержащий идентификатор предка текущей категории }
        f_ParentNode := NIL;
        if notEmpty (2,anArgs) then
        begin
            OBJ := toObject (anArgs [2]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TTreeNode) ) then
                f_ParentNode := TTreeNode (OBJ);
        end;
        {if ( not Assigned (f_ParentNode) ) then
            raise Exception.Create (ERR_TCATEGORIESLOADER_INCORRECT_NODE);}
        { четвертый параметр - CategorieID
          текущая категория }
        f_CategorieID := 0;
        if notEmpty (3,anArgs) then
        begin
            f_CategorieID := toInt64 (anArgs [3]);
        end;
        if ( f_CategorieID <= 0 ) then
            raise Exception.Create (ERR_TCATEGORIESLOADER_INCORRECT_ID);
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
            raise Exception.Create (ERR_TCATEGORIESLOADER_INCORRECT_GAUGE);}
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
            raise Exception.Create (ERR_TCATEGORIESLOADER_INCORRECT_STATUS_PANEL);}
        f_Children := NIL;
        f_ChildIndex := -1;
    except on E: Exception do
        _raise ([ 'Create', ERR_TCATEGORIESLOADER_CREATE, E, Exception (FatalException) ],
                ['{D1C22245-693F-44A1-B1D0-18EF3798C90C}']);
    end;
end;

destructor TCategoriesLoader.Destroy;
begin
    try
        FreeAndNil (f_Children);
        FreeAndNil (f_DB);
        inherited Destroy;
    except on E: Exception do
        _raise ([ 'Destroy', ERR_TCATEGORIESLOADER_DESTROY, E, Exception (FatalException) ],
                ['{0A0FF0E4-5255-420A-892D-2C492BBBF52B}']);
    end;
end;

procedure TCategoriesLoader.Main;
begin
    try
        if Terminated then Exit;
        inherited Main;
        if not Assigned (Children) then
            f_Children := TCategories.Load (DB,[ _([]),
                                                 _([CategorieID]),
                                                 _([]),
                                                 _([USER_ID]),
                                                 _([]),
                                                 _([CATEGORIE_OPENED_STATUS_ID,
                                                    CATEGORIE_CLOSED_STATUS_ID]) ]) as TCategories;
    except on E: Exception do
        _raise ([ 'Main', ERR_TCATEGORIESLOADER_MAIN, E, Exception (FatalException) ],
                ['{FEE108D9-BDF2-4E0F-A99B-BC3F6400F47C}']);
    end;
end;

procedure TCategoriesLoader.Return;
var
    Node : TTreeNode;
begin
    try
        if Terminated then Exit;
        if ( not Assigned (Children) or (Children.Count <= 0) ) then
        begin
            SetTabStatus (tabForum,tbsLoaded);
            Terminate;
        end
        else if ChildIndex >= Children.Count-1 then
        begin
            SetTabStatus (tabForum,tbsLoaded);
            Terminate;
        end
        else
        try
            inherited Return;
            ChildIndex := ChildIndex + 1;
            if Assigned (StatusPanel) then
                StatusPanel.Text := Format (MSG_TCATEGORIESLOADER_LOAD_OBJECT,
                                            [ Children.ItemAt [ChildIndex].Name ]);
            Node := TreeNodes.AddChild ( ParentNode,
                                         {Format (' %s '#13#10' %s ',
                                                  [ Children.ItemAt [ChildIndex].Name,
                                                    Children.ItemAt [ChildIndex].Description ])}
                                         ''
                                        );
            Node.ImageIndex := Children.ItemAt [ChildIndex].IndexPic*4 + 0;
            Node.Data := CreateCategorieNode;
            with PCategorieNode (Node.Data)^ do
            begin
                //WriteStatus (MSG_TCATEGORIESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Children.ItemAt [ChildIndex].Name,
                //               Children.ItemAt [ChildIndex].PropAt [_ctg_id].Caption ]);
                ID := Children.ItemAt [ChildIndex].ID;
                //WriteStatus (MSG_TCATEGORIESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Children.ItemAt [ChildIndex].Name,
                //               Children.ItemAt [ChildIndex].PropAt [_ctg_id_parent].Caption ]);
                IDParent := Children.ItemAt [ChildIndex].IDparent;
                //WriteStatus (MSG_TCATEGORIESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Children.ItemAt [ChildIndex].Name,
                //               Children.ItemAt [ChildIndex].PropAt [_ctg_id_author].Caption ]);
                IDAuthor := Children.ItemAt [ChildIndex].IDAuthor;
                //WriteStatus (MSG_TCATEGORIESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Children.ItemAt [ChildIndex].Name,
                //               Children.ItemAt [ChildIndex].PropAt [_ctg_id_type].Caption ]);
                IDType := Children.ItemAt [ChildIndex].IDType;
                //WriteStatus (MSG_TCATEGORIESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Children.ItemAt [ChildIndex].Name,
                //               Children.ItemAt [ChildIndex].PropAt [_ctg_id_status].Caption ]);
                IDStatus := Children.ItemAt [ChildIndex].IDStatus;
                //WriteStatus (MSG_TCATEGORIESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Children.ItemAt [ChildIndex].Name,
                //               Children.ItemAt [ChildIndex].PropAt [_ctg_id_pic].Caption ]);
                IDPic := Children.ItemAt [ChildIndex].IDPic;
                //WriteStatus (MSG_TCATEGORIESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Children.ItemAt [ChildIndex].Name,
                //               Children.ItemAt [ChildIndex].PropAt [_ctg_index_pic].Caption ]);
                IndexPic := Children.ItemAt [ChildIndex].IndexPic;
                //WriteStatus (MSG_TCATEGORIESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Children.ItemAt [ChildIndex].Name,
                //               Children.ItemAt [ChildIndex].PropAt [_ctg_name].Caption ]);
                Name := Children.ItemAt [ChildIndex].Name;
                //WriteStatus (MSG_TCATEGORIESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Children.ItemAt [ChildIndex].Name,
                //               Children.ItemAt [ChildIndex].PropAt [_ctg_description].Caption ]);
                Description := Children.ItemAt [ChildIndex].Description;
                //WriteStatus (MSG_TCATEGORIESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Children.ItemAt [ChildIndex].Name,
                //               Children.ItemAt [ChildIndex].Author.PropAt [_usr_login].Caption ]);
                Author := Children.ItemAt [ChildIndex].Author.Login;
                //WriteStatus (MSG_TCATEGORIESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Children.ItemAt [ChildIndex].Name,
                //               Children.ItemAt [ChildIndex].PropAt [_ctg_time_stamp_create].Caption ]);
                TimeStampCreate := Children.ItemAt [ChildIndex].TimeStampCreate;
                //WriteStatus (MSG_TCATEGORIESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Children.ItemAt [ChildIndex].Name,
                //               Children.ItemAt [ChildIndex].PropAt [_ctg_time_stamp_modify].Caption ]);
                TimeStampModify := Children.ItemAt [ChildIndex].TimeStampModify;
                //WriteStatus (MSG_TCATEGORIESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Children.ItemAt [ChildIndex].Name,
                //               Children.ItemAt [ChildIndex].PropAt [_ctg_time_stamp_public].Caption ]);
                TimeStampPublic := Children.ItemAt [ChildIndex].TimeStampPublic;
                //WriteStatus (MSG_TCATEGORIESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Children.ItemAt [ChildIndex].Name,
                //               Children.ItemAt [ChildIndex].PropAt [_ctg_key_hash].Caption ]);
                KeyHash := Children.ItemAt [ChildIndex].KeyHash;
                //WriteStatus (MSG_TCATEGORIESLOADER_LOAD_OBJECT_PROPERTY,
                //             [ Children.ItemAt [ChildIndex].Name,
                //               Children.ItemAt [ChildIndex].PropAt [_ctg_data_hash].Caption ]);
                DataHash := Children.ItemAt [ChildIndex].DataHash;
                // подсчет количества сообщений
                MsgCount := TMessages.GetCount (DB,[ _([]),
                                                     _([ Children.ItemAt [ChildIndex].ID ]),
                                                     _([]),
                                                     _([USER_ID]),
                                                     _([MESSAGE_FORUM_TYPE_ID]),
                                                     _([MESSAGE_ACTIVE_STATUS_ID]) ]);
            end;
            if Assigned (Gauge) then
                Gauge.Progress := Gauge.Progress + 1;
            { распаралеливаем рекурсию }
            if Assigned (Owner) then
                Owner.Add ( TCategoriesLoader.Create ([ DBFileName,
                                                        TreeNodes,
                                                        Node,
                                                        Children.ItemAt [ChildIndex].ID,
                                                        Gauge,
                                                        StatusPanel,
                                                        FALSE,
                                                        TRUE ]) );
        except on E: Exception do
            WriteStatus (E.Message);
        end;
    except on E: Exception do
        _raise ([ 'Return', ERR_TCATEGORIESLOADER_RETURN, E, Exception (FatalException) ],
                ['{21121ED6-8F56-4ED8-B2AA-FC1B7EACEF82}']);
    end;
end;

procedure TCategoriesLoader.WriteStatus (const aMessage: String);
begin
    try
        if Assigned (StatusPanel) then
        begin
            StatusPanel.Text := aMessage;
            ProcessMessages;
        end;
    except on E: Exception do
        _raise (['WriteStatus',ERR_TCATEGORIESLOADER_WRITE_STATUS,E],
                ['{D589475D-1FD3-437E-94EC-CC9DCD834F17}']);
    end;
end;

procedure TCategoriesLoader.WriteStatus (const aMessage: String;
                                         aParams: array of const);
begin
    try
        WriteStatus ( Format (aMessage,aParams) );
    except on E: Exception do
        _raise (['WriteStatus',ERR_TCATEGORIESLOADER_WRITE_STATUS,E],
                ['{5587B7DB-F6AA-411B-B53F-249D7D510F16}']);
    end;
end;


end.
