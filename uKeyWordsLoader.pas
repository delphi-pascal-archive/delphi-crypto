unit uKeyWordsLoader;
{******************************************************************************}
{*  KeyWords Loader Unit                                                      *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2012                                             *}
{******************************************************************************}
interface

{$I 'std.inc'}

uses
    Windows, SysUtils, Variants, Classes,
    Controls, ComCtrls, Gauges,
    sTreeView,
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
{ загрузчик ключевых слов }
{$M+}
    CKeyWordsLoader = class of TKeyWordsLoader;
    PKeyWordsLoader = ^TKeyWordsLoader;
    TKeyWordsLoader = class (TDllThread)
    private
        f_DBFileName: String;        { файл базы данных }
        f_DB: TSQLiteDatabase;       { объект базы данных }
        f_TreeNodes: TTreeNodes;     { дерево }
        f_Gauge: TGauge;             { показатель прогресса загрузки }
        f_StatusPanel: TStatusPanel; { строка состояния загрузки }
        f_ParentNode: TTreeNode;     { узел, содержащий идентификатор предка текущей категории }
        f_Words: TStringList;        { ключевые слова }
        f_KeyWords: TKeyWords;       { список ключевых слов }
        f_KeyWordIndex: LongInt;     { позиция в обходе списка }
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
        property Words: TStringList read f_Words write f_Words;
        property KeyWords: TKeyWords read f_KeyWords write f_KeyWords;
        property KeyWordIndex: LongInt read f_KeyWordIndex write f_KeyWordIndex;
    end;
{$M-}

{ TKeyWordsLoader Errors }
resourcestring
    ERR_TKEYWORDSLOADER_INCORRECT_DATABASE     = 'Не инициализирован объект БД!';
    ERR_TKEYWORDSLOADER_INCORRECT_NODES        = 'Объект дерева не инициализирован!';
    ERR_TKEYWORDSLOADER_INCORRECT_NODE         = 'Объект узла дерева не инициализирован!';
    ERR_TKEYWORDSLOADER_INCORRECT_WORDS        = 'Некорректный список ключевых слов!';
    ERR_TKEYWORDSLOADER_INCORRECT_GAUGE        = 'Не инициализирован элемент контроля прогресса!';
    ERR_TKEYWORDSLOADER_INCORRECT_STATUS_PANEL = 'Не инициализирована строка состояния!';
    ERR_TKEYWORDSLOADER_CREATE                 = 'Ошибка создания потока загрузки дерева категорий!';
    ERR_TKEYWORDSLOADER_DESTROY                = 'Ошибка уничтожения потока загрузки дерева категорий!';
    ERR_TKEYWORDSLOADER_MAIN                   = 'Ошибка главной функции потока!';
    ERR_TKEYWORDSLOADER_RETURN                 = 'Ошибка функции возврата потока!';
    ERR_TKEYWORDSLOADER_WRITE_STATUS           = 'Ошибка отображения статуса!';

{ TKeyWordsLoader Hints }
resourcestring
    MSG_TKEYWORDSLOADER_LOAD_OBJECT          = 'Загрузка ''%s'' ...';
    MSG_TKEYWORDSLOADER_LOAD_OBJECT_PROPERTY = 'Загрузка ''%s'' ... %s';

implementation

{ TKeyWordsLoader }
constructor TKeyWordsLoader.Create (anArgs: array of const);
var
    I    : Integer;
    args : array_of_const;
    OBJ  : TObject;
    arr  : array_of_const;
    S    : String;
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
            raise Exception.Create (ERR_TKEYWORDSLOADER_INCORRECT_DATABASE);
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
            raise Exception.Create (ERR_TKEYWORDSLOADER_INCORRECT_NODES);
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
            raise Exception.Create (ERR_TKEYWORDSLOADER_INCORRECT_NODE);}
        { четвертый параметр - Words
          список ключевых слов для поиска }
        f_Words := TStringList.Create;
        if notEmpty (3,anArgs) then
        begin
            { параметр передан списком }
            OBJ := toObject (anArgs [3]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TStringList) ) then
                Words.Assign ( TStringList (OBJ) )
            { параметр передан массивом }
            else
            begin
                arr := toArrayOfConst (anArgs [3]);
                for I := 0 to High (arr) do
                begin
                    S := Trim ( toString (arr [I]) );
                    if notEmpty (S) then
                    try
                        Words.Add (S);
                    finally
                        _FillChar ( S, Length (S), $00 );
                    end;
                end;
            end;
        end;
        if not Assigned (Words) then
            raise Exception.Create (ERR_TKEYWORDSLOADER_INCORRECT_WORDS);
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
            raise Exception.Create (ERR_TKEYWORDSLOADER_INCORRECT_GAUGE);}
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
            raise Exception.Create (ERR_TKEYWORDSLOADER_INCORRECT_STATUS_PANEL);}
        f_KeyWords := NIL;
        f_KeyWordIndex := -1;
    except on E: Exception do
        _raise ([ 'Create', ERR_TKEYWORDSLOADER_CREATE, E, Exception (FatalException) ],
                ['{111C0FF1-7B56-4AE4-BA0A-226A4F2BD82F}']);
    end;
end;

destructor TKeyWordsLoader.Destroy;
begin
    try
        FreeAndNil (f_KeyWords);
        FreeAndNil (f_Words);
        FreeAndNil (f_DB);
        inherited Destroy;
    except on E: Exception do
        _raise ([ 'Destroy', ERR_TKEYWORDSLOADER_DESTROY, E, Exception (FatalException) ],
                ['{DE614587-EE9D-4DD2-AF4B-0F5BE575F621}']);
    end;
end;

procedure TKeyWordsLoader.Main;
begin
    try
        if Terminated then Exit;
        inherited Main;
        if ( Words.Count <= 0 ) then
            Terminate
        else if not Assigned (KeyWords) then
            f_KeyWords := TKeyWords.Load (DB,[ _([]),
                                               Words,
                                               _([]),
                                               _([]),
                                               _([]),
                                               _([USER_ID]) ]) as TKeyWords;
    except on E: Exception do
        _raise ([ 'Main', ERR_TKEYWORDSLOADER_MAIN, E, Exception (FatalException) ],
                ['{C95C4BE0-3BD6-40CE-8180-A6EF6D64DDA5}']);
    end;
end;

procedure TKeyWordsLoader.Return;
var
    Ctg  : TCategorie;
    Msg  : TMessage;
    Node : TTreeNode;
    P    : Integer;
begin
    try
        if Terminated then Exit;
        if ( not Assigned (KeyWords) or (KeyWords.Count <= 0) ) then
        begin
            SetTabStatus (tabForum,tbsLoaded);
            Terminate;
        end
        else if KeyWordIndex >= KeyWords.Count-1 then
        begin
            SetTabStatus (tabUsers,tbsLoaded); 
            Terminate;
        end
        else
        try
            inherited Return;
            KeyWordIndex := KeyWordIndex + 1;
            Ctg := NIL;
            if ( KeyWords.ItemAt [KeyWordIndex].IDCategorie > 0 ) then
                Ctg := TCategorie.Load ( DB, KeyWords.ItemAt [KeyWordIndex].IDCategorie ) as TCategorie;
            if ( KeyWords.ItemAt [KeyWordIndex].IDMessage > 0 ) then
            begin
                Msg := TMessage.Load ( DB, KeyWords.ItemAt [KeyWordIndex].IDMessage ) as TMessage;
                if not Assigned (Ctg) then
                    Ctg := TCategorie.Load ( DB, Msg.IDCategorie ) as TCategorie;
            end;
            try
                if Assigned (Ctg) then
                begin
                    if Assigned (StatusPanel) then
                        StatusPanel.Text := Format (MSG_TKEYWORDSLOADER_LOAD_OBJECT,
                                                    [ Ctg.Name ]);
                    Node := TreeNodes.AddChild ( ParentNode,
                                                 {Format (' %s '#13#10' %s ',
                                                          [ Ctg.Name,
                                                            Ctg.Description ])}
                                                 '' );
                    Node.ImageIndex := Ctg.IndexPic*4 + 0;
                    Node.Data := CreateCategorieNode;
                    with PCategorieNode (Node.Data)^ do
                    begin
                        //WriteStatus (MSG_TKEYWORDSLOADER_LOAD_OBJECT_PROPERTY,
                        //             [ Ctg.Name,
                        //               Ctg.PropAt [_ctg_id].Caption ]);
                        ID := Ctg.ID;
                        //WriteStatus (MSG_TKEYWORDSLOADER_LOAD_OBJECT_PROPERTY,
                        //             [ Ctg.Name,
                        //               Ctg.PropAt [_ctg_id_parent].Caption ]);
                        IDParent := Ctg.IDparent;
                        //WriteStatus (MSG_TKEYWORDSLOADER_LOAD_OBJECT_PROPERTY,
                        //             [ Ctg.Name,
                        //               Ctg.PropAt [_ctg_id_author].Caption ]);
                        IDAuthor := Ctg.IDAuthor;
                        //WriteStatus (MSG_TKEYWORDSLOADER_LOAD_OBJECT_PROPERTY,
                        //             [ Ctg.Name,
                        //               Ctg.PropAt [_ctg_id_type].Caption ]);
                        IDType := Ctg.IDType;
                        //WriteStatus (MSG_TKEYWORDSLOADER_LOAD_OBJECT_PROPERTY,
                        //             [ Ctg.Name,
                        //               Ctg.PropAt [_ctg_id_status].Caption ]);
                        IDStatus := Ctg.IDStatus;
                        //WriteStatus (MSG_TKEYWORDSLOADER_LOAD_OBJECT_PROPERTY,
                        //             [ Ctg.Name,
                        //               Ctg.PropAt [_ctg_id_pic].Caption ]);
                        IDPic := Ctg.IDPic;
                        //WriteStatus (MSG_TKEYWORDSLOADER_LOAD_OBJECT_PROPERTY,
                        //             [ Ctg.Name,
                        //               Ctg.PropAt [_ctg_index_pic].Caption ]);
                        IndexPic := Ctg.IndexPic;
                        //WriteStatus (MSG_TKEYWORDSLOADER_LOAD_OBJECT_PROPERTY,
                        //             [ Ctg.Name,
                        //               Ctg.PropAt [_ctg_name].Caption ]);
                        Name := Ctg.Name;
                        //WriteStatus (MSG_TKEYWORDSLOADER_LOAD_OBJECT_PROPERTY,
                        //             [ Ctg.Name,
                        //               Ctg.PropAt [_ctg_description].Caption ]);
                        Description := Ctg.Description;
                        //WriteStatus (MSG_TKEYWORDSLOADER_LOAD_OBJECT_PROPERTY,
                        //             [ Ctg.Name,
                        //               Ctg.Author.PropAt [_usr_login].Caption ]);
                        Author := Ctg.Author.Login;
                        //WriteStatus (MSG_TKEYWORDSLOADER_LOAD_OBJECT_PROPERTY,
                        //             [ Ctg.Name,
                        //               Ctg.PropAt [_ctg_time_stamp_create].Caption ]);
                        TimeStampCreate := Ctg.TimeStampCreate;
                        //WriteStatus (MSG_TKEYWORDSLOADER_LOAD_OBJECT_PROPERTY,
                        //             [ Ctg.Name,
                        //               Ctg.PropAt [_ctg_time_stamp_modify].Caption ]);
                        TimeStampModify := Ctg.TimeStampModify;
                        //WriteStatus (MSG_TKEYWORDSLOADER_LOAD_OBJECT_PROPERTY,
                        //             [ Ctg.Name,
                        //               Ctg.PropAt [_ctg_time_stamp_public].Caption ]);
                        TimeStampPublic := Ctg.TimeStampPublic;
                        //WriteStatus (MSG_TKEYWORDSLOADER_LOAD_OBJECT_PROPERTY,
                        //             [ Ctg.Name,
                        //               Ctg.PropAt [_ctg_key_hash].Caption ]);
                        KeyHash := Ctg.KeyHash;
                        //WriteStatus (MSG_TKEYWORDSLOADER_LOAD_OBJECT_PROPERTY,
                        //             [ Ctg.Name,
                        //               Ctg.PropAt [_ctg_data_hash].Caption ]);
                        DataHash := Ctg.DataHash;
                    end;
                end;
                if Assigned (Ctg) and Assigned (Msg) then
                begin
                    if Assigned (StatusPanel) then
                        StatusPanel.Text := Format (MSG_TKEYWORDSLOADER_LOAD_OBJECT,
                                                    [ Msg.Subject ]);
                    Node := TreeNodes.AddChild ( Node, '' );
                    Node.ImageIndex := TsTreeView (TreeNodes.Owner).Images.Count - 1;
                    Node.Data := CreateCategorieNode;
                    with PCategorieNode (Node.Data)^ do
                    begin
                        isMessage := TRUE;
                        ID := 0;
                        IDParent := 0;
                        IDAuthor := Msg.IDAuthor;
                        IDType := 0;
                        IDStatus := 0;
                        IDPic := 0;
                        IndexPic := -1;
                        Name := Msg.Subject;
                        P := Pos ( KeyWords.ItemAt [KeyWordIndex].KeyWord, Msg.Text );
                        if ( P > 32 ) then
                            Description := Format ('...%s...',[ Copy (Msg.Text,P-32,64) ])
                        else
                            Description := Format ('%s...',[ Copy (Msg.Text,1,64) ]);
                        Author := Msg.Author.Login;
                        TimeStampCreate := Msg.TimeStampCreate;
                        TimeStampModify := Msg.TimeStampModify;
                        TimeStampPublic := Msg.TimeStampPublic;
                        KeyHash := Msg.KeyHash;
                        DataHash := Msg.DataHash;
                    end;
                    //Node.Parent.Expand (FALSE);
                end;
            finally
                FreeAndNil (Msg);
                FreeAndNil (Ctg);
            end;
            { прогресс }
            if Assigned (Gauge) then
                Gauge.Progress := Gauge.Progress + 1;
        except on E: Exception do
            WriteStatus (E.Message);
        end;
    except on E: Exception do
        _raise ([ 'Return', ERR_TKEYWORDSLOADER_RETURN, E, Exception (FatalException) ],
                ['{5B0D7C1C-1D0C-4788-932F-15EA24F9E012}']);
    end;
end;

procedure TKeyWordsLoader.WriteStatus (const aMessage: String);
begin
    try
        if Assigned (StatusPanel) then
        begin
            StatusPanel.Text := aMessage;
            ProcessMessages;
        end;
    except on E: Exception do
        _raise (['WriteStatus',ERR_TKEYWORDSLOADER_WRITE_STATUS,E],
                ['{C4084910-5044-4A59-9ED5-FA13130A5256}']);
    end;
end;

procedure TKeyWordsLoader.WriteStatus (const aMessage: String;
                                       aParams: array of const);
begin
    try
        WriteStatus ( Format (aMessage,aParams) );
    except on E: Exception do
        _raise (['WriteStatus',ERR_TKEYWORDSLOADER_WRITE_STATUS,E],
                ['{C1F578AA-6DE7-493B-A55A-F922BBF81F05}']);
    end;
end;


end.
