unit uMain;
{******************************************************************************}
{*  Main Form Unit                                                            *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011-2012                                        *}
{******************************************************************************}
interface

{$I 'std.inc'}

uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, ExtCtrls,
    ComCtrls, StdCtrls, Forms, Menus, ActnList, StdActns, Dialogs,
    ToolWin,
    sSkinManager, sSkinProvider,
    ImgList, acAlphaImageList, jpeg, pngimage, acPNG,
    sPanel, sPageControl, sTabControl, sGroupBox, sScrollBox, sStatusBar, acCoolBar,
    Buttons, sBitBtn, sSpeedButton, sCheckBox,
    sEdit, sMemo, sRichEdit, RxRichEd,
    Mask, sMaskEdit, sTooledit, sSpinEdit,
    sCustomComboEdit, sComboBoxes, sComboBox,
    sLabel, sColorSelect, sTreeView, sListView, sGauge,
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

{ иконки лога почты }
const
    pckDefault = 0;
    pckEmpty   = 1;
    pckFull    = 2;
    pckTest    = 3;
    pckError   = 4;
    pckSend    = 5;

type
    TfmMain = class (TForm)
        SkinProvider: TsSkinProvider;
        SkinBlack: TsSkinManager;
        imgTabs: TsAlphaImageList;
        imgIconsSmall: TsAlphaImageList;
        imgIcons: TsAlphaImageList;
        imgTree: TsAlphaImageList;
        imgSmiles: TsAlphaImageList;
        imgSmilesOct31: TsAlphaImageList;
        imgSmilesAdvanced: TsAlphaImageList;
        imgQuotes: TsAlphaImageList;
        imgUsers: TsAlphaImageList;
        imgMail: TsAlphaImageList;
    { actions }
        lstActions: TActionList;
        actHelp: TAction;
        actAbout: TAction;
        actAdd: TAction;
        actEdit: TAction;
        actDelete: TAction;
        { text }
        actTextCopy: TEditCopy;
        actTextCut: TEditCut;
        actTextPaste: TEditPaste;
        actTextDelete: TEditDelete;
        actTextSelectAll: TEditSelectAll;
        { user }
        actSaveUser: TAction;
        { users }
        actAddUser: TAction;
        actEditUser: TAction;
        actDeleteUser: TAction;
        actSynchronizeUser: TAction;
        { forum }
        actAddCategorie: TAction;
        actEditCategorie: TAction;
        actDeleteCategorie: TAction;
        actQuoteMessage: TAction;
        actAddMessage: TAction;
        actEditMessage: TAction;
        actDeleteMessage: TAction;
        actRefreshMessage: TAction;
        actPrev: TAction;
        actNext: TAction;
        actRefresh: TAction;
        actURL: TAction;
        actHome: TAction;
    { menu }
        { text }
        mnTextPopup: TPopupMenu;
        mnItemCopy: TMenuItem;
        mnItemCut: TMenuItem;
        mnItemPaste: TMenuItem;
        mnItemDelete: TMenuItem;
        mnItemBreak: TMenuItem;
        mnItemSelectAll: TMenuItem;
        { categories }
        mnCategoriesPopup: TPopupMenu;
        mnItemAddCtg: TMenuItem;
        mnItemEditCtg: TMenuItem;
        mnItemDeleteCtg: TMenuItem;
        { messages }
        mnMessagesPopup: TPopupMenu;
        mnItemEditMessage: TMenuItem;
        mnItemDeleteMessage: TMenuItem;
        { users }
        mnUsersPopup: TPopupMenu;
        mnItemAddUser: TMenuItem;
        mnItemEditUser: TMenuItem;
        mnItemDeleteUser: TMenuItem;
        mnItemUpdateForum: TMenuItem;
    { status bar }
        StatusBar: TsStatusBar;
        Gauge: TsGauge;
    { left panel }
        btLeftPanel: TsSpeedButton;
        pnlLeft: TsPanel;
        imLeftRed: TImage;
        imgArrowRed: TImage;
        pnlMenu: TsPanel;
        btCopy: TsSpeedButton;
        btPaste: TsSpeedButton;
        btCut: TsSpeedButton;
        btClear: TsSpeedButton;
        btSelectAll: TsSpeedButton;
        btAdd: TsSpeedButton;
        btEdit: TsSpeedButton;
        btDelete: TsSpeedButton;
        btHelp: TsSpeedButton;
        btAbout: TsSpeedButton;
    { tabs }
        tabs: TsPageControl;
        { forum }
        tbForum: TsTabSheet;
        pnlNavigation: TsPanel;
        btNext: TsSpeedButton;
        btRefresh: TsSpeedButton;
        btPrev: TsSpeedButton;
        btHome: TsSpeedButton;
        edURL: TsEdit;
        btURL: TsSpeedButton;
        bxForum: TsScrollBox;
        Tree: TsTreeView;
        pnlReply: TsPanel;
        pnlReplyMenu: TsPanel;
        btBold: TsSpeedButton;
        btUnderline: TsSpeedButton;
        btItalic: TsSpeedButton;
        btKey: TsSpeedButton;
        btReply: TsSpeedButton;
        btCancelMessage: TsSpeedButton;
        edReply: TRxRichEdit;
        btColor: TPanel;
        btSmile: TsSpeedButton;
        { users }
        tbUsers: TsTabSheet;
        bxUsers: TsScrollBox;
        lstUsers: TsTreeView;
        pnlCryptoMessage: TsPanel;
        edCryptoMessage: TsRichEdit;
        btEncrypt: TsSpeedButton;
        btDecrypt: TsSpeedButton;
        { user }
        { - profile }
        tbUser: TsTabSheet;
        btSaveUser: TsSpeedButton;
        actRefreshUser: TAction;
        btCancelUser: TsSpeedButton;
        bxUser: TsScrollBox;
        pnlProfile: TsPanel;
        imgProfile: TImage;
        lbProfile: TsWebLabel;
        pnlUserPic: TsPanel;
        imgUserPic: TImage;
        UserPic: TsFilenameEdit;
        lbUserDescription: TsLabel;
        UserDescription: TsRichEdit;
        lbUserMail: TsLabel;
        UserMail: TsEdit;
        lbUserMailPassword: TsLabel;
        UserMailPassword: TsMaskEdit;
        lbUserIP: TsLabel;
        UserIP: TsEdit;
        lbUserPort: TsLabel;
        UserPort: TsSpinEdit;
        UserSex: TsComboBoxEx;
        UserBirthday: TsDateEdit;
        lbUserSMTPHost: TsLabel;
        UserSMTPHost: TsEdit;
        lbUserSMTPPort: TsLabel;
        UserSMTPPort: TsSpinEdit;
        lbUserPOP3Host: TsLabel;
        UserPOP3Host: TsEdit;
        lbUserPOP3Port: TsLabel;
        UserPOP3Port: TsSpinEdit;
        cbxUserAutoTLS: TsCheckBox;
        cbxUserFullSSL: TsCheckBox;
        { - crypto }
        pnlCryptoProfile: TsPanel;
        pnlCryptoLabels: TsPanel;
        lbUserRandom: TsLabel;
        lbUserAsymmetric: TsLabel;
        lbUserSymmetric: TsLabel;
        lbUserMode: TsLabel;
        lbUserHash: TsLabel;
        lbUserPublicKey: TsLabel;
        pnlCryptoControls: TsPanel;
        btRandomTest: TsSpeedButton;
        btAsymmetricTest: TsSpeedButton;
        btSymmetricTest: TsSpeedButton;
        btHashTest: TsSpeedButton;
        UserRandom: TsComboBox;
        UserPublicKey: TsRichEdit;
        UserAsymmetric: TsComboBox;
        UserSymmetric: TsComboBox;
        UserMode: TsComboBox;
        UserHash: TsComboBox;
        { - proxy }
        pnlUseProxy: TsPanel;
        cbxUserUseProxy: TsCheckBox;
        pnlProxy: TsPanel;
        lbUserProxyIP: TsLabel;
        lbUserProxyPort: TsLabel;
        lbUserProxyLogin: TsLabel;
        lbUserProxyPassword: TsLabel;
        lbUserProxyProtocol: TsLabel;
        UserProxyIP: TsEdit;
        UserProxyPort: TsSpinEdit;
        UserProxyLogin: TsEdit;
        UserProxyPassword: TsMaskEdit;
        UserProxyProtocol: TsComboBox;
        { - connection }
        pnlConnection: TsPanel;
        lbUserTimeOut: TsLabel;
        lbUserTimeOutDesc: TsLabel;
        UserTimeOut: TsSpinEdit;
        { mail }
        tbMail: TsTabSheet;
        tmMail: TTimer;
        lstMail: TsListView;
    { actions }
        procedure actAboutExecute (Sender: TObject);
        procedure actHelpExecute (Sender: TObject);
        procedure actAddExecute (Sender: TObject);
        procedure actAddUpdate (Sender: TObject);
        procedure actAddCategorieExecute (Sender: TObject);
        procedure actAddCategorieUpdate (Sender: TObject);
        procedure actAddMessageExecute (Sender: TObject);
        procedure actAddMessageUpdate (Sender: TObject);
        procedure actAddUserExecute (Sender: TObject);
        procedure actAddUserUpdate (Sender: TObject);
        procedure actEditExecute (Sender: TObject);
        procedure actEditUpdate (Sender: TObject);
        procedure actEditCategorieExecute (Sender: TObject);
        procedure actEditCategorieUpdate (Sender: TObject);
        procedure actEditMessageExecute (Sender: TObject);
        procedure actEditMessageUpdate (Sender: TObject);
        procedure actEditUserExecute (Sender: TObject);
        procedure actEditUserUpdate (Sender: TObject);
        procedure actDeleteExecute (Sender: TObject);
        procedure actDeleteUpdate (Sender: TObject);
        procedure actDeleteCategorieExecute (Sender: TObject);
        procedure actDeleteCategorieUpdate (Sender: TObject);
        procedure actDeleteMessageExecute (Sender: TObject);
        procedure actDeleteMessageUpdate (Sender: TObject);
        procedure actDeleteUserExecute (Sender: TObject);
        procedure actDeleteUserUpdate (Sender: TObject);
        procedure actSaveUserUpdate (Sender: TObject);
        procedure actSaveUserExecute (Sender: TObject);
        procedure actRefreshUserUpdate (Sender: TObject);
        procedure actRefreshUserExecute (Sender: TObject);
        procedure actRefreshMessageExecute (Sender: TObject);
        procedure actRefreshMessageUpdate (Sender: TObject);
        procedure actRefreshExecute (Sender: TObject);
        procedure actRefreshUpdate (Sender: TObject);
        procedure actURLExecute (Sender: TObject);
        procedure actURLUpdate (Sender: TObject);
        procedure actHomeExecute (Sender: TObject);
        procedure actNextExecute (Sender: TObject);
        procedure actNextUpdate (Sender: TObject);
        procedure actPrevExecute (Sender: TObject);
        procedure actPrevUpdate (Sender: TObject);
        procedure actQuoteMessageExecute (Sender: TObject);
        procedure actQuoteMessageUpdate (Sender: TObject);
        procedure actSynchronizeUserUpdate (Sender: TObject);
        procedure actSynchronizeUserExecute (Sender: TObject);
    { events }
        procedure tmMailTimer (Sender: TObject);
        procedure FormCloseQuery (Sender: TObject; var CanClose: Boolean);
        procedure FormResize (Sender: TObject);
        procedure FormKeyDown (Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure tabsChange (Sender: TObject);
        procedure TreeAdvancedCustomDrawItem (Sender: TCustomTreeView;
                                              Node: TTreeNode;
                                              State: TCustomDrawState;
                                              Stage: TCustomDrawStage;
                                              var PaintImages, DefaultDraw: Boolean);
        procedure TreeChange (Sender: TObject; Node: TTreeNode);
        procedure TreeDblClick (Sender: TObject);
        procedure lstUsersAdvancedCustomDrawItem (Sender: TCustomTreeView;
                                                  Node: TTreeNode;
                                                  State: TCustomDrawState;
                                                  Stage: TCustomDrawStage;
                                                  var PaintImages, DefaultDraw: Boolean);
        procedure lstUsersChange (Sender: TObject; Node: TTreeNode);
        procedure lstUsersDblClick (Sender: TObject);
        procedure lstMailAdvancedCustomDrawItem (Sender: TCustomListView;
                                                 Item: TListItem;
                                                 State: TCustomDrawState;
                                                 Stage: TCustomDrawStage;
                                                 var DefaultDraw: Boolean);
        procedure btEncryptClick (Sender: TObject);
        procedure btDecryptClick (Sender: TObject);
        procedure btLeftPanelClick (Sender: TObject);
        procedure btBoldClick (Sender: TObject);
        procedure btUnderlineClick (Sender: TObject);
        procedure btItalicClick (Sender: TObject);
        procedure btKeyClick (Sender: TObject);
        procedure btColorMouseDown (Sender: TObject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: Integer);
        procedure btSmileMouseDown (Sender: TObject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: Integer);
        procedure UserPicAfterDialog (Sender: TObject; var Name: string; var Action: Boolean);
        procedure btRandomTestClick (Sender: TObject);
        procedure btAsymmetricTestClick (Sender: TObject);
        procedure btSymmetricTestClick (Sender: TObject);
        procedure btHashTestClick (Sender: TObject);
        procedure cbxUserUseProxyClick (Sender: TObject);
    private
        ActiveLeftButton : TObject;
    public
        procedure OnButtonMove (Sender: TObject; Shift: TShiftState; X,Y: Integer);
        procedure OnBackGroundMove (Sender: TObject; Shift: TShiftState; X,Y: Integer);
        procedure OnLeftButtonMove (Sender: TObject; Shift: TShiftState; X,Y: Integer);
        procedure OnRichEditMouseDown (Sender: TObject; Button: TMouseButton;
                                       Shift: TShiftState; X, Y: Integer);
        procedure OnEditChange (Sender: TObject);
        procedure OnControlMouseWheel (Sender: TObject;
                                       Shift: TShiftState;
                                       WheelDelta: Integer;
                                       MousePos: TPoint;
                                       var Handled: Boolean);
    { non visual }
    public
        class procedure _raise (anArgs: array of const;
                                const anEGUID: String = ''); overload; virtual;
        class procedure _raise (anArgs: array of const;
                                anEGUID: array of const); overload; virtual;
    private
        f_CurrentSmiles: TsAlphaImageList;
        f_DBFileName: String;
        f_DB: TSQLiteDatabase;
        f_Threads: TDllThreads;
        f_MailThreads: TDllThreads;
        f_CurrCtgKeyHash: Hex;
        f_CurrCtgID: TID;
        f_CurrMsgKeyHash: Hex;
        f_CurrMsgID: TID;
        f_CurrMsgTmp: TFrame;
        f_EditMsgID: TID;
        f_CurrUsrID: TID;
        f_History: TStringList;
        f_HistoryIndex: Integer;
    protected
        procedure GetURLData;
        procedure GetUserData;
        procedure GetCategoriesData;
        procedure UnloadMessagesList;
        procedure GetMessagesData;
        procedure GetKeyWordsData;
        procedure GetUsersData;
        procedure GetMailData; overload;
        procedure SetUserData;
        procedure SetMailData; overload;
    public
        procedure UpdateMailData (const anUser: TUser); overload;
        procedure SetMailData (const anObject: TMetaObject); overload;
    public
        function GetImgSmiles : TsAlphaImageList;
        procedure GetData;
        procedure SetData;
    public
        constructor Create (anOwner: TComponent); override;
        destructor Destroy; override;
    public
        procedure WriteStatus (const aMessage: String); overload;
        procedure WriteStatus (const aMessage: String;
                               aParams: array of const); overload;
        procedure Progress (const aCount: Integer); overload;
        procedure Progress (const aCount: Integer;
                            const aMessage: String); overload;
        procedure Progress (const aCount: Integer;
                            const aMessage: String;
                            aParams: array of const); overload;
    public
        procedure MailLog (const aSender: String;
                           const aReceiver: String;
                           const aMessage: String;
                           const anImageIndex: Integer = pckDefault); overload;
        procedure MailLog (const aSender: String;
                           const aReceiver: String;
                           const aMessage: String;
                           aParams: array of const;
                           const anImageIndex: Integer = pckDefault); overload;
    public
        property CurrentSmiles: TsAlphaImageList read f_CurrentSmiles write f_CurrentSmiles;
        property DBFileName: String read f_DBFileName;
        property DB: TSQLiteDatabase read f_DB write f_DB;
        property Threads: TDllThreads read f_Threads write f_Threads;
        property MailThreads: TDllThreads read f_MailThreads write f_MailThreads;
        property CurrCtgKeyHash: Hex read f_CurrCtgKeyHash write f_CurrCtgKeyHash;
        property CurrCtgID: TID read f_CurrCtgID write f_CurrCtgID;
        property CurrMsgKeyHash: Hex read f_CurrMsgKeyHash write f_CurrMsgKeyHash;
        property CurrMsgID: TID read f_CurrMsgID write f_CurrMsgID;
        property CurrMsgTmp: TFrame read f_CurrMsgTmp write f_CurrMsgTmp;
        property EditMsgID: TID read f_EditMsgID write f_EditMsgID;
        property CurrUsrID: TID read f_CurrUsrID write f_CurrUsrID;
        property History: TStringList read f_History write f_History;
        property HistoryIndex: Integer read f_HistoryIndex write f_HistoryIndex;
    end;

const
    pnlLeftMenuButton = 0; { кнопка управлени левым меню }
    pnlProgress       = 1; { полоса загрузки }
    pnlStatusText     = 2; { текстовая часть панели состояния }

resourcestring
    ERR_TFMMAIN_CREATE               = 'Ошибка создания главного окна программы!';
    ERR_TFMMAIN_DESTROY              = 'Ошибка уничтожения главного окна программы!';
    ERR_TFMMAIN_WRITE_STATUS         = 'Ошибка отображения статуса!';
    ERR_TFMMAIN_PROGRESS             = 'Ошибка отображения прогресса!';
    ERR_TFMMAIN_WRITE_LOG            = 'Ошибка ведения лога!';
    ERR_TFMMAIN_CLOSE_QUERY          = 'Ошибка закрытия главного окна программы!';
    ERR_TFMMAIN_TABS_CHANGE          = 'Ошибка переключения вкладок!';
    ERR_TFMMAIN_RESIZE               = 'Ошибка изменения размера главного окна программы!';
    ERR_TFMMAIN_DRAW                 = 'Ошибка отрисовки главного окна программы!';
    ERR_TFMMAIN_ACTION_EXECUTE       = 'Ошибка выполнения действия!';
    ERR_TFMMAIN_ACTION_UPDATE        = 'Ошибка обработки статуса действия!';
    ERR_TFMMAIN_GET_SMILES           = 'Ошибка получения набора смайлов!';
    ERR_TFMMAIN_GET_DATA             = 'Ошибка чтения данных!';
    ERR_TFMMAIN_GET_USER_DATA        = 'Ошибка чтения пользовательских данных!';
    ERR_TFMMAIN_GET_KEYWORDS_DATA    = 'Ошибка поиска по ключевым словам!';
    ERR_TFMMAIN_GET_CATEGORIES_DATA  = 'Ошибка загрузки дерева категорий!';
    ERR_TFMMAIN_GET_MESSAGES_DATA    = 'Ошибка загрузки списка сообщений!';
    ERR_TFMMAIN_GET_MESSAGE_DATA     = 'Не удалось загрузить сообщение : id = %d';
    ERR_TFMMAIN_UNLOAD_MESSAGES_LIST = 'Ошибка выгрузки списка сообщений!';
    ERR_TFMMAIN_GET_USERS_DATA       = 'Ошибка загрузки списка пользователей!';
    ERR_TFMMAIN_GET_URL_DATA         = 'Ошибка загрузки URL!';
    ERR_TFMMAIN_GET_MAIL_DATA        = 'Ошибка загрузки почты!';
    ERR_TFMMAIN_GET_COLOR            = 'Ошибка выбора цвета!';
    ERR_TFMMAIN_GET_SMILE            = 'Ошибка выбора смайла!';
    ERR_TFMMAIN_SET_TEXT_ATTRIBUTES  = 'Ошибка установки текстовых атрибутов!'; 
    ERR_TFMMAIN_SET_DATA             = 'Ошибка записи данных!';
    ERR_TFMMAIN_SET_USER_DATA        = 'Ошибка записи пользовательских данных!';
    ERR_TFMMAIN_SET_CATEGORIES_DATA  = 'Ошибка сохранения дерева категорий!';
    ERR_TFMMAIN_SET_USERS_DATA       = 'Ошибка сохранения списка пользователей!';
    ERR_TFMMAIN_SET_MAIL_DATA        = 'Ошибка отправки почты!';
    ERR_TFMMAIN_UPDATE_MAIL_DATA     = 'Ошибка формирования пакетов обновлений!';
    ERR_TFMMAIN_INVALID_OBJECT       = 'Некорректный объект!';
    ERR_TFMMAIN_INVALID_OBJECT_CLASS = 'Некорректный тип пакета: "%s"!';

var
    MainForm: TfmMain;

implementation

{$R *.dfm}

uses
{ crypto }
    Crypto,
{ bb-code }
    BBCode,
{ dialogs }
    DialogClasses, uProtoDialog, uMetaDialog, uWizardDialog,
    uLoginDialog, uRegisterDialog,
    uRandomTestDialog, uAsymmetricTestDialog, uSymmetricTestDialog, uHashTestDialog,
    uCategorieDialog,
{ templates }
    uTmpMessage,
{ loaders }
    uMessagesLoader,
    uCategoriesLoader,
    uKeyWordsLoader,
    uUsersLoader,
{ http }
{$IFDEF HTTP}
    HTTPServer,
    HTTPClient,
    uHTTPServer,
    uHTTPCLient,
{$ENDIF HTTP}
{ smtp/pop3 }
{$IFDEF SMTP_POP3}
    SMTPClient,
    POP3Client,
    uSMTPClient,
    uPOP3Client,
{$ENDIF SMTP_POP3}
{ scanners }
    uPackagesScanner,
{ constructors }
    uPackagesConstructor;

class procedure TfmMain._raise (anArgs: array of const;
                                const anEGUID: String = '');
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

class procedure TfmMain._raise (anArgs: array of const;
                                anEGUID: array of const);
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

constructor TfmMain.Create (anOwner: TComponent);
begin
    try
        inherited Create (anOwner);
        { DB }
        f_DBFileName := DATABASE_FILE_NAME;
        f_DB := TSQLiteDatabase.Create (f_DBFileName);
        if Assigned (User) then
            User.DB := DB;
        { threads }
        f_Threads := TDllThreads.Create;
        f_MailThreads := TDllThreads.Create;
        { history }
        f_History := TStringList.Create;
        f_HistoryIndex := -1;
        { version }
        Caption := Format ('%s - %s',[ ProductName, User.Login ]);
        { mouse wheel }
        OnMouseWheel := OnControlMouseWheel;
        { left menu panel }
        pnlMenu.OnMouseMove := OnBackGroundMove;
        btCopy.OnMouseMove := OnLeftButtonMove;
        btCut.OnMouseMove := OnLeftButtonMove;
        btPaste.OnMouseMove := OnLeftButtonMove;
        btClear.OnMouseMove := OnLeftButtonMove;
        btSelectAll.OnMouseMove := OnLeftButtonMove;
        btAdd.OnMouseMove := OnLeftButtonMove;
        btEdit.OnMouseMove := OnLeftButtonMove;
        btDelete.OnMouseMove := OnLeftButtonMove;
        btHelp.OnMouseMove := OnLeftButtonMove;
        btAbout.OnMouseMove := OnLeftButtonMove;
        { forum }
        pnlNavigation.OnMouseMove := OnBackGroundMove;
        btPrev.OnMouseMove := OnButtonMove;
        btNext.OnMouseMove := OnButtonMove;
        btRefresh.OnMouseMove := OnButtonMove;
        btURL.OnMouseMove := OnButtonMove;
        btHome.OnMouseMove := OnButtonMove;
        btReply.OnMouseMove := OnButtonMove;
        btCancelMessage.OnMouseMove := OnButtonMove;
        edReply.OnMouseDown := OnRichEditMouseDown;
        edReply.MaxLength := 0; //MAX_MESSAGE_LENGTH;
        btBold.OnMouseMove := OnButtonMove;
        btUnderline.OnMouseMove := OnButtonMove;
        btItalic.OnMouseMove := OnButtonMove;
        btKey.OnMouseMove := OnButtonMove;
        btColor.OnMouseMove := OnButtonMove;
        btSmile.OnMouseMove := OnButtonMove;
        { user }
        lbProfile.Caption := Format ('профиль: %s',[User.Login]);
        btSaveUser.OnMouseMove := OnButtonMove;
        btCancelUser.OnMouseMove := OnButtonMove;
        UserMail.OnChange := OnEditChange;
        UserMailPassword.OnChange := OnEditChange;
        UserIP.OnChange := OnEditChange;
        UserPort.OnChange := OnEditChange;
        UserDescription.OnChange := OnEditChange;
        UserSex.OnChange := OnEditChange;
        UserBirthday.OnChange := OnEditChange;
        UserPic.OnChange := OnEditChange;
        UserRandom.OnChange := OnEditChange;
        //UserAsymmetric.OnChange := OnEditChange;
        //UserSymmetric.OnChange := OnEditChange;
        //UserMode.OnChange := OnEditChange;
        //UserHash.OnChange := OnEditChange;
        //UserPublicKey.OnChange := OnEditChange;
        cbxUserUseProxy.OnClick := OnEditChange;
        UserProxyIP.OnChange := OnEditChange;
        UserProxyPort.OnChange := OnEditChange;
        UserProxyLogin.OnChange := OnEditChange;
        UserProxyPassword.OnChange := OnEditChange;
        UserProxyProtocol.OnChange := OnEditChange;
        UserTimeOut.OnChange := OnEditChange;
        UserSMTPHost.OnChange := OnEditChange;
        UserSMTPPort.OnChange := OnEditChange;
        UserPOP3Host.OnChange := OnEditChange;
        UserPOP3Port.OnChange := OnEditChange;
        cbxUserAutoTLS.OnClick := OnEditChange;
        cbxUserFullSSL.OnClick := OnEditChange;
        { users }
        pnlCryptoMessage.OnMouseMove := OnBackGroundMove;
        edCryptoMessage.OnMouseMove := OnButtonMove;
        btEncrypt.OnMouseMove := OnButtonMove;
        btDecrypt.OnMouseMove := OnButtonMove;
        { mail }
        tmMail.Interval := User.TimeOut * 10;
        tmMail.Enabled := TRUE;
        { active page }
        tabs.TabIndex := 0;
    except on E: Exception do
        _raise (['Create',ERR_TFMMAIN_CREATE,E],
                ['{92249C2F-F939-4BE2-96AC-EE224A71FEDF}']);
    end;
end;

destructor TfmMain.Destroy;
begin
    try
        FreeAndNil (f_History);
        MailThreads.Terminate;
        FreeAndNil (f_MailThreads);
        Threads.Terminate;
        FreeAndNil (f_Threads);
        UnLoadCategoriesTree (Tree.Items);
        UnloadMessagesList;
        UnLoadUsersList (lstUsers.Items);
        FreeAndNil (f_DB);
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TFMMAIN_DESTROY,E],
                ['{A24E525F-6632-44B9-95FF-19E412D76D19}']);
    end;
end;

procedure TfmMain.FormKeyDown (Sender: TObject; var Key: Word; Shift: TShiftState);
const
    VK_B = 66;
    VK_U = 85;
    VK_I = 73;
    VK_K = 75;
begin
    { Ctrl + Left -- prev }
    if ( Key = VK_LEFT )
           and ( ssCtrl in Shift )
       and not ( ssAlt in Shift )
       and not ( ssShift in Shift )
       and actPrev.Enabled then
        actPrev.Execute
    { Ctrl + Right -- next }
    else if ( Key = VK_RIGHT )
           and ( ssCtrl in Shift )
       and not ( ssAlt in Shift )
       and not ( ssShift in Shift )
       and actNext.Enabled then
        actNext.Execute
    { Enter -- go to url }
    else if ( Key = VK_RETURN )
       and not ( ssCtrl in Shift )
       and not ( ssAlt in Shift )
       and not ( ssShift in Shift )
       and ( ActiveControl = edURL )
       and actURL.Enabled then
        actURL.Execute
    { Enter -- expand / collapse categorie }
    else if ( Key = VK_RETURN )
       and not ( ssCtrl in Shift )
       and not ( ssAlt in Shift )
       and not ( ssShift in Shift )
       and ( ActiveControl = Tree )
       and Assigned (Tree.Selected) then
    begin
        if Tree.Selected.Expanded then
            Tree.Selected.Collapse (FALSE)
        else
            Tree.Selected.Expand (FALSE)
    end
    { Ctrl + Enter -- load categorie messages }
    else if ( Key = VK_RETURN )
           and ( ssCtrl in Shift )
       and not ( ssAlt in Shift )
       and not ( ssShift in Shift )
       and ( ActiveControl = Tree )
       and Assigned (Tree.Selected)
       and ( CurrCtgID > 0 )
       and actURL.Enabled then
        actURL.Execute
    { Ctrl + B -- bold }
    else if ( Key = VK_B )
           and ( ssCtrl in Shift )
       and not ( ssAlt in Shift )
       and not ( ssShift in Shift )
       and ( ActiveControl = edReply )
       and btBold.Enabled then
        btBold.Click
    { Ctrl + U -- underline }
    else if ( Key = VK_U )
           and ( ssCtrl in Shift )
       and not ( ssAlt in Shift )
       and not ( ssShift in Shift )
       and ( ActiveControl = edReply )
       and btUnderline.Enabled then
        btUnderline.Click
    { Ctrl + I -- italic }
    else if ( Key = VK_I )
           and ( ssCtrl in Shift )
       and not ( ssAlt in Shift )
       and not ( ssShift in Shift )
       and ( ActiveControl = edReply )
       and btItalic.Enabled then
        btItalic.Click
    { Ctrl + K -- keyword }
    else if ( Key = VK_K )
           and ( ssCtrl in Shift )
       and not ( ssAlt in Shift )
       and not ( ssShift in Shift )
       and ( ActiveControl = edReply )
       and btItalic.Enabled then
        btKey.Click
end;

procedure TfmMain.FormCloseQuery (Sender: TObject; var CanClose: Boolean);
begin
    try
        CanClose := FALSE;
        try
            WriteStatus ('Завершение работы...');
            Gauge.Progress := 0;
            Gauge.MaxValue := 6;
            Progress (1,'Завершение потоков...');
            MailThreads.Terminate;
            Threads.Terminate;
            Progress (1,'Выгрузка разделов...');
            UnLoadCategoriesTree (Tree.Items);
            Progress (1,'Выгрузка списка контактов...');
            UnLoadUsersList (lstUsers.Items);
            { Удаляем пакеты, пришедшие нам: обработанные и отвергнутые.
              Пакетов, предназначенных нам, со статусами "создан" или "отправлен"
              быть не должно, но на всякий случай мы удаляем и такие пакеты.
              Оставляем только пакеты со статусом "получен",
              которые необходимо обработать в дальнейшем. }
            Progress (1,'Удаление обработанных и отвергнутых пакетов...');
            TPackages.Delete (DB,[ _([]),
                                   _([]),
                                   _([USER_KEY_HASH]),
                                   _([USER_ID]),
                                   _([]),
                                   _([PACKAGE_REJECTED_STATUS_ID,
                                      PACKAGE_EXECUTED_STATUS_ID,
                                      PACKAGE_CREATED_STATUS_ID,
                                      PACKAGE_SENDED_STATUS_ID]),
                                   _([]) ]);
            { Удаляем пакеты, отправленные нами.
              Отвергнутые получателем пакеты мы должны удалять сразу,
              но на всякий случай удалим и их.
              Созданных нами пакетов со статусами "получен" или "исполнен"
              быть не должно, но на всякий случай мы удаляем и такие пакеты.
              Оставляем только пакеты со статусом "создан",
              которые необходимо отправить в дальнейшем. }
            Progress (1,'Удаление отправленных пакетов...');
            TPackages.Delete (DB,[ _([]),
                                   _([USER_KEY_HASH]),
                                   _([]),
                                   _([USER_ID]),
                                   _([]),
                                   _([PACKAGE_SENDED_STATUS_ID,
                                      PACKAGE_REJECTED_STATUS_ID,
                                      PACKAGE_RECEIVED_STATUS_ID,
                                      PACKAGE_EXECUTED_STATUS_ID]),
                                   _([]) ]);
            Progress (1,'Сжатие базы данных...');
            DB.Compress;
        finally
            CanClose := TRUE;
            Halt;
        end;
    except on E: Exception do
        _raise (['FormCloseQuery',ERR_TFMMAIN_CLOSE_QUERY,E],
                ['{E0F33354-384D-4CB9-87E3-2CAA2700C1C3}']);
    end;
end;

function TfmMain.GetImgSmiles : TsAlphaImageList;
var
    y : WORD;
    m : WORD;
    d : WORD;
begin
    Result := NIL;
    try
        if not Assigned (CurrentSmiles) then
        begin
            DecodeDate (now,y,m,d);
            if (  ( m = 10 ) and ( d >= 29 )  ) or
               (  ( m = 11 ) and ( d <= 3 )  ) then
                CurrentSmiles := imgSmilesOct31
            else
                CurrentSmiles := imgSmiles;
        end;
        Result := CurrentSmiles;
    except on E: Exception do
        _raise (['GetImgSmiles',ERR_TFMMAIN_GET_SMILES,E],
                ['{B9BC23FD-E15B-4250-B27E-8F52EF1EEBE9}']);
    end;
end;

procedure TfmMain.GetData;
var
    URI : String;
    I   : Integer;
begin
    try
        { start mail services }
        if ( MailThreads.Count = 0 ) and Assigned (User) then
            GetMailData;
        { categories }
        if ( tabs.ActivePage = tbForum ) and Tree.Visible then
            GetCategoriesData
        { messages }
        else if ( tabs.ActivePage = tbForum ) and not Tree.Visible and ( CurrCtgID > 0 ) then
            GetMessagesData
        { users }
        else if ( tabs.ActivePage = tbUsers ) and lstUsers.Visible then
            GetUsersData
        { user }
        else if ( tabs.ActivePage = tbUser ) then
            GetUserData
        { mail }
        else if ( tabs.ActivePage = tbMail ) then
            GetMailData;
        { history }
        URI := Trim (edURL.Text);
        try
            if not isEmpty ( Trim (URI)  ) then
            begin
                { очищаем историю после текущего состояния }
                for I := HistoryIndex + 1 to History.Count - 1 do
                    History.Delete (I);
                { добавляем новое состояние в историю }
                if ( History.Count = 0 ) or ( History.Strings [HistoryIndex] <> URI ) then
                begin
                    History.Add (URI);
                    HistoryIndex := History.Count - 1;
                end;
            end
        finally
            _FillChar ( URI, Length (URI), $00 );
        end;
    except on E: Exception do
        _raise (['GetData',ERR_TFMMAIN_GET_DATA,E],
                ['{A43EB1D3-5F53-4D5A-84AF-E0188ED290EF}']);
    end;
end;

procedure TfmMain.SetData;
begin
    try
        { categories }
        if ( tabs.ActivePage = tbForum ) and Tree.Visible then
            //SetCategoriesData
        { messages }
        else if ( tabs.ActivePage = tbForum ) and not Tree.Visible and ( CurrCtgID > 0 ) then
            //SetMessagesData
        { users }
        else if ( tabs.ActivePage = tbUsers ) and lstUsers.Visible then
            //SetUsersData
        { user }
        else if ( tabs.ActivePage = tbUser ) then
            SetUserData
        { mail }
        else if ( tabs.ActivePage = tbMail ) then
            //SetMailsData;
    except on E: Exception do
        _raise (['SetData',ERR_TFMMAIN_SET_DATA,E],
                ['{BD9738EB-8F22-4333-85A0-1ECFDC9D4AA6}']);
    end;
end;

procedure TfmMain.GetUserData;
var
    I : Integer;
begin
    try
        WriteStatus ('Загрузка профиля...');
        Gauge.Progress := 0;
        Gauge.MaxValue := 30;
        Progress (1,'Завершение потоков...');
        Threads.Terminate;
        Progress (1,'Очистка строки адреса...');
        edURL.Text := 'crypto://user/';
        if ( GetTabStatus (tabUser) = tbsLoaded ) then
        begin
            Progress (Gauge.MaxValue,'...');
            Exit;
        end;
        Progress (1,'логин...');
        lbProfile.Caption := Format ('профиль: %s',[User.Login]);
        Progress (1,'аватар...');
        imgUserPic.Picture.Assign (User.Pic.Picture);
        imgUserPic.Repaint;
        Progress (1,'эл.почта...');
        UserMail.Text := User.EMail;
        Progress (1,'пароль к эл.почте...');
        UserMailPassword.Text := User.EMailPassword;
        Progress (1,'ip-адрес...');
        UserIP.Text := User.IP;
        Progress (1,'порт...');
        UserPort.Value := User.Port;
        Progress (1,'личная информация...');
        UserDescription.Clear;
        UserDescription.Text := User.Description;
        Progress (1,'личная информация...');
        UserSex.ItemIndex := User.Sex;
        Progress (1,'личная информация...');
        UserBirthday.Date := User.Birthday;
        Progress (1,'генератор случайных чисел...');
        with UserRandom.Items do
        try
            BeginUpdate;
            Clear;
            for I := Low (RANDOM_TYPE_STRING) + 1 to High (RANDOM_TYPE_STRING) do
                Add (RANDOM_TYPE_STRING [I]);
            UserRandom.Text := User.Crypto.genRandom;
            UserRandom.ReadOnly := FALSE;
        finally
            EndUpdate;
        end;
        Progress (1,'асимметричный шифр...');
        with UserAsymmetric.Items do
        try
            BeginUpdate;
            Clear;
            for I := Low (PKCRYPTO_TYPE_STRING) + 1 to High (PKCRYPTO_TYPE_STRING) do
                Add (PKCRYPTO_TYPE_STRING [I]);
            UserAsymmetric.Text := User.Crypto.algAsymmetric;
            UserAsymmetric.ReadOnly := TRUE;
        finally
            EndUpdate;
        end;
        Progress (1,'симметричный шифр...');
        with UserSymmetric.Items do
        try
            BeginUpdate;
            Clear;
            for I := Low (CRYPTO_TYPE_STRING) + 1 to High (CRYPTO_TYPE_STRING) do
                Add (CRYPTO_TYPE_STRING [I]);
            UserSymmetric.Text := User.Crypto.algSymmetric;
            UserSymmetric.ReadOnly := TRUE;
        finally
            EndUpdate;
        end;
        Progress (1,'режим симметричного шифрования...');
        with UserMode.Items do
        try
            BeginUpdate;
            Clear;
            for I := Low (CRYPTO_MODE_STRING) + 1 to High (CRYPTO_MODE_STRING) do
                Add (CRYPTO_MODE_STRING [I]);
            UserMode.Text := User.Crypto.modeSymmetric;
            UserMode.ReadOnly := TRUE;
        finally
            EndUpdate;
        end;
        Progress (1,'хэш-функция...');
        with UserHash.Items do
        try
            BeginUpdate;
            Clear;
            for I := Low (HASH_TYPE_STRING) + 1 to High (HASH_TYPE_STRING) do
                Add (HASH_TYPE_STRING [I]);
            UserHash.Text := User.Crypto.algHash;
            UserHash.ReadOnly := TRUE;
        finally
            EndUpdate;
        end;
        Progress (1,'публичный ключ...');
        UserPublicKey.Text := User.PublicKey;
        Progress (1,'использование proxy...');
        cbxUserUseProxy.Checked := User.UseProxy and
                                   ( User.ProxyIP <> '' ) and
                                   ( User.ProxyPort > 0 );
        pnlProxy.Visible := cbxUserUseProxy.Checked;
        Progress (1,'proxy ip-адрес...');
        UserProxyIP.Text := User.ProxyIP;
        Progress (1,'proxy порт...');
        UserProxyPort.Value := User.ProxyPort;
        Progress (1,'proxy логин...');
        UserProxyLogin.Text := User.ProxyLogin;
        Progress (1,'proxy пароль...');
        UserProxyPassword.Text := User.ProxyPassword;
        Progress (1,'proxy протокол...');
        with UserProxyProtocol.Items do
        try
            BeginUpdate;
            Clear;
            {$IFDEF HTTP}
            Add ('HTTP');
            {$ENDIF HTTP}
            Add ('SOCKS4');
            Add ('SOCKS5');
            if isEmpty (User.ProxyProtocol) then
                UserProxyProtocol.Text := UserProxyProtocol.Items [0]
            else
                UserProxyProtocol.Text := User.ProxyProtocol;
        finally
            EndUpdate;
        end;
        Progress (1,'таймаут...');
        UserTimeOut.Value := User.TimeOut div 1000;
        Progress (1,'smtp-хост...');
        UserSMTPHost.Text := User.SMTPHost;
        Progress (1,'smtp-порт...');
        UserSMTPPort.Value := User.SMTPPort;
        Progress (1,'pop3-хост...');
        UserPOP3Host.Text := User.POP3Host;
        Progress (1,'pop3-порт...');
        UserPOP3Port.Value := User.POP3Port;
        Progress (1,'auto tls...');
        cbxUserAutoTLS.Checked := User.AutoTLS;
        Progress (1,'full ssl...');
        cbxUserFullSSL.Checked := User.FullSSL;
        WriteStatus ('%s, товарищ %s.',[ GetGood (now), User.Login ]);
        actSaveUser.Enabled := FALSE;
        SetTabStatus (tabUser,tbsLoaded);
    except on E: Exception do
        _raise (['GetUserData',ERR_TFMMAIN_GET_USER_DATA,E],
                ['{B26418CC-EE1D-48F6-8C0F-9277D2DBD8A8}']);
    end;
end;

procedure TfmMain.SetUserData;
begin
    try
        WriteStatus ('Сохранение профиля...');
        Gauge.Progress := 0;
        Gauge.MaxValue := 24;//29;
        Progress (1,'Завершение потоков...');
        Threads.Terminate;
        Progress (1,'аватар...');
        try
            if ValidateFileSize (UserPic.FileName,24) then
            begin
                User.Pic.Picture.LoadFromFile (UserPic.FileName);
                User.Pic.DataFormat := ExtractFileExt (UserPic.FileName);
            end;
        except on E: Exception do
            WriteStatus ('Ошибка: %s',[E.Message]);
        end;
        Progress (1,'эл.почта...');
        User.EMail := Trim (UserMail.Text);
        Progress (1,'пароль к эл.почте...');
        User.EMailPassword := UserMailPassword.Text;
        Progress (1,'ip-адрес...');
        User.IP := Trim (UserIP.Text);
        Progress (1,'порт...');
        User.Port := UserPort.Value;
        Progress (1,'личная информация...');
        User.Description := Trim (UserDescription.Text);
        Progress (1,'личная информация...');
        User.Sex := UserSex.ItemIndex;
        Progress (1,'личная информация...');
        User.Birthday := UserBirthday.Date;
        Progress (1,'генератор случайных чисел...');
        User.Crypto.genRandom := UserRandom.Text;
        //Progress (1,'асимметричный шифр...');
        //User.Crypto.algAsymmetric := UserAsymmetric.Text;
        //Progress (1,'симметричный шифр...');
        //User.Crypto.algSymmetric := UserSymmetric.Text;
        //Progress (1,'режим симметричного шифрования...');
        //User.Crypto.modeSymmetric := UserMode.Text;
        //Progress (1,'хэш-функция...');
        //User.Crypto.algHash := UserHash.Text;
        //Progress (1,'публичный ключ...');
        //User.PublicKey := UserPublicKey.Text;
        Progress (1,'использование proxy...');
        User.UseProxy := cbxUserUseProxy.Checked and
                         ( Trim (UserProxyIP.Text) <> '' ) and
                         ( UserProxyPort.Value > 0 );
        Progress (1,'proxy ip-адрес...');
        User.ProxyIP := Trim (UserProxyIP.Text);
        Progress (1,'proxy порт...');
        User.ProxyPort := UserProxyPort.Value;
        Progress (1,'proxy логин...');
        User.ProxyLogin := Trim (UserProxyLogin.Text);
        Progress (1,'proxy пароль...');
        User.ProxyPassword := UserProxyPassword.Text;
        Progress (1,'proxy протокол...');
        User.ProxyProtocol := UserProxyProtocol.Text;
        Progress (1,'таймаут...');
        User.TimeOut := UserTimeOut.Value * 1000;
        Progress (1,'сохранение...');
        Progress (1,'smtp-хост...');
        User.SMTPHost := Trim (UserSMTPHost.Text);
        Progress (1,'smtp-порт...');
        User.SMTPPort := UserSMTPPort.Value;
        Progress (1,'pop3-хост...');
        User.POP3Host := Trim (UserPOP3Host.Text);
        Progress (1,'pop3-порт...');
        User.POP3Port := UserPOP3Port.Value;
        Progress (1,'auto tls...');
        User.AutoTLS := cbxUserAutoTLS.Checked;
        Progress (1,'full ssl...');
        User.FullSSL := cbxUserFullSSL.Checked;
        User.Save;
        WriteStatus ('Создание пакетов...');
        SetMailData (User);
        WriteStatus ('Профиль сохранен.');
        actSaveUser.Enabled := FALSE;
        SetTabStatus (tabUser,tbsLoaded);
    except on E: Exception do
        _raise (['SetUserData',ERR_TFMMAIN_SET_USER_DATA,E],
                ['{7F893CCE-5310-4669-A8D2-0DA73614C551}']);
    end;
end;

procedure TfmMain.GetCategoriesData;
var
    CtgCount : Integer;
begin
    try
        Tree.Visible := TRUE;
        pnlReply.Visible := FALSE;
        WriteStatus ('Загрузка категорий...');
        Gauge.Progress := 0;
        Gauge.MaxValue := 5;
        Progress (1,'Завершение потоков...');
        Threads.Terminate;
        Progress (1,'Выгрузка разделов...');
        if not ( GetTabStatus (tabForum) = tbsLoaded ) then
            UnLoadCategoriesTree (Tree.Items);
        Progress (1,'Выгрузка списка сообщений...');
        if not ( GetTabStatus (tabForum) = tbsLoaded ) then
            UnloadMessagesList;
        Progress (1,'Очистка строки адреса...');
        edURL.Text := 'crypto://categories/';
        if ( GetTabStatus (tabForum) = tbsLoaded ) then
        begin
            Progress (Gauge.MaxValue,'...');
            Exit;
        end;
        Progress (1,'Подсчет количества доступных категорий...');
        CtgCount := TCategories.GetCount (DB,[ _([]),
                                               _([]),
                                               _([]),
                                               _([USER_ID]),
                                               _([]),
                                               _([CATEGORIE_OPENED_STATUS_ID,
                                                  CATEGORIE_CLOSED_STATUS_ID]) ]);
        if ( CtgCount-1 > 0 ) then
        begin
            WriteStatus ('Загрузка категорий... Найдено %d элементов.',[CtgCount-1]); // don't show one root categorie
            Gauge.Enabled := TRUE;
            Gauge.Progress := 0;
            Gauge.MaxValue := CtgCount-1;
            Threads.Add ( TCategoriesLoader.Create ([ DBFileName,
                                                      Tree.Items,
                                                      NIL,
                                                      ROOT_CATEGORIE_ID,
                                                      Gauge,
                                                      StatusBar.Panels [pnlStatusText],
                                                      FALSE,
                                                      TRUE,
                                                      TP_NORMAL ]) );
        end
        else
            WriteStatus ('Список разделов пуст.');
    except on E: Exception do
        _raise (['GetCategoriesData',ERR_TFMMAIN_GET_CATEGORIES_DATA,E],
                ['{306173A2-A3BB-4FD9-85CB-1921FBD999F5}']);
    end;
end;

procedure TfmMain.UnloadMessagesList;
var
    I     : Integer;
    Found : Boolean;
begin
    try
        SetTabStatus (tabForum,tbsEmpty);
        CurrMsgID := 0;
        repeat
            Found := FAlSE;
            for I := 0 to bxForum.ComponentCount - 1 do
            begin
                Found := ( bxForum.Components [I] is TTmpMessage );
                if Found then Break;
            end;
            if Found then
                bxForum.Components [I].Free;
        until not Found;
        CurrMsgTmp := NIL;
    except on E: Exception do
        _raise (['UnloadMessagesList',ERR_TFMMAIN_UNLOAD_MESSAGES_LIST,E],
                ['{B163FF64-C349-4E8C-9B79-C450315E9B65}']);
    end;
end;

procedure TfmMain.GetMessagesData;
var
    MsgCount : Integer;
    Ctg      : TCategorie;
begin
    try
        if ( CurrCtgID <= 0 ) then
            Exit;
        CurrMsgID := 0;
        if ( EditMsgID > 0 ) then
            edReply.Clear;
        EditMsgID := 0;
        Tree.Visible := FALSE;
        pnlReply.Visible := TRUE;
        WriteStatus ('Загрузка сообщений...');
        Gauge.Progress := 0;
        Gauge.MaxValue := 5;
        Progress (1,'Завершение потоков...');
        Threads.Terminate;
        Progress (1,'Выгрузка разделов...');
        if not ( GetTabStatus (tabForum) = tbsLoaded ) then
            UnLoadCategoriesTree (Tree.Items);
        Progress (1,'Выгрузка списка сообщений...');
        if not ( GetTabStatus (tabForum) = tbsLoaded ) then
            UnloadMessagesList;
        Progress (1,'Очистка строки адреса...');
        Ctg := TCategorie.Load (DB,CurrCtgID) as TCategorie;
        try
            CurrCtgKeyHash := Ctg.KeyHash;
            if isEmpty (CurrMsgKeyHash) then
                edURL.Text := Format ('crypto://categorie/%s/',
                                      [CurrCtgKeyHash])
            else
                edURL.Text := Format ('crypto://categorie/%s#%s/',
                                      [CurrCtgKeyHash,CurrMsgKeyHash])
        finally
            FreeAndNil (Ctg);
        end;
        if ( GetTabStatus (tabForum) = tbsLoaded ) then
        begin
            Progress (Gauge.MaxValue,'...');
            Exit;
        end;
        Progress (1,'Подсчет количества доступных сообщений...');
        MsgCount := TMessages.GetCount (DB,[ _([]),
                                             _([CurrCtgID]),
                                             _([]),
                                             _([USER_ID]),
                                             _([MESSAGE_FORUM_TYPE_ID]),
                                             _([MESSAGE_ACTIVE_STATUS_ID]) ]);
        if ( MsgCount > 0 ) then
        begin
            WriteStatus ('Загрузка сообщений... Найдено %d элементов.',[MsgCount]);
            Gauge.Enabled := TRUE;
            Gauge.Progress := 0;
            Gauge.MaxValue := MsgCount;
            Threads.Add ( TMessagesLoader.Create ([ DBFileName,
                                                    bxForum,
                                                    CurrCtgID,
                                                    Gauge,
                                                    StatusBar.Panels [pnlStatusText],
                                                    FALSE,
                                                    TRUE,
                                                    TP_NORMAL ]) );
        end
        else
            WriteStatus ('Раздел пуст.');
    except on E: Exception do
        _raise (['GetMessagesData',ERR_TFMMAIN_GET_MESSAGES_DATA,E],
                ['{C80F7E6C-67E8-4CBB-B0A9-57ED1A75CEF3}']);
    end;
end;

procedure TfmMain.GetKeyWordsData;
var
    Words    : TStringList;
    KwdCount : Integer;
begin
    try
        Tree.Visible := TRUE;
        pnlReply.Visible := FALSE;
        WriteStatus ('Поиск по ключевым словам...');
        Gauge.Progress := 0;
        Gauge.MaxValue := 5;
        Progress (1,'Завершение потоков...');
        Threads.Terminate;
        Progress (1,'Выгрузка разделов...');
        if not ( GetTabStatus (tabForum) = tbsLoaded ) then
            UnLoadCategoriesTree (Tree.Items);
        Progress (1,'Выгрузка списка сообщений...');
        if not ( GetTabStatus (tabForum) = tbsLoaded ) then
            UnloadMessagesList;
        if ( GetTabStatus (tabForum) = tbsLoaded ) then
        begin
            Progress (Gauge.MaxValue,'...');
            Exit;
        end;
        Progress (1,'Чтение ключевых слов...');
        Words := TStringList.Create;
        try
            Words.CommaText := Trim (edURL.Text);
            Progress (1,'Подсчет количества найденных совпадений...');
            KwdCount := TKeyWords.GetCount (DB,[ _([]),
                                                 Words,
                                                 _([]),
                                                 _([]),
                                                 _([USER_ID]) ]);
            if ( KwdCount > 0 ) then
            begin
                WriteStatus ('Загрузка... Найдено %d элементов.',[KwdCount]);
                Gauge.Enabled := TRUE;
                Gauge.Progress := 0;
                Gauge.MaxValue := KwdCount;
                Threads.Add ( TKeyWordsLoader.Create ([ DBFileName,
                                                        Tree.Items,
                                                        NIL,
                                                        Words,
                                                        Gauge,
                                                        StatusBar.Panels [pnlStatusText],
                                                        FALSE,
                                                        TRUE,
                                                        TP_NORMAL ]) );
            end
            else
                WriteStatus ('Совпадений не найдено.');
        finally
            FreeAndNil (Words);
        end;
    except on E: Exception do
        _raise (['GetKeyWordsData',ERR_TFMMAIN_GET_KEYWORDS_DATA,E],
                ['{6056D5DE-9640-4655-9E96-5CEB68DFC940}']);
    end;
end;


procedure TfmMain.GetUsersData;
var
    UsrCount : Integer;
begin
    try
        pnlCryptoMessage.Visible := FALSE;
        WriteStatus ('Загрузка пользователей...');
        Gauge.Progress := 0;
        Gauge.MaxValue := 5;
        Progress (1,'Завершение потоков...');
        Threads.Terminate;
        Progress (1,'Выгрузка списка контактов...');
        if not ( GetTabStatus (tabUsers) = tbsLoaded ) then
            UnLoadUsersList (lstUsers.Items);
        Progress (1,'Очистка коллекции аватаров...');
        if not ( GetTabStatus (tabUsers) = tbsLoaded ) then
            imgUsers.Clear;
        Progress (1,'Очистка строки адреса...');
        edURL.Text := 'crypto://users/';
        if ( GetTabStatus (tabUsers) = tbsLoaded ) then
        begin
            Progress (Gauge.MaxValue,'...');
            Exit;
        end;
        Progress (1,'Подсчет количества доступных контактов...');
        UsrCount := TUsers.GetCount (DB,[ _([]),
                                          _([]),
                                          _([USER_ID]) ]);
        if ( UsrCount > 0 ) then
        begin
            WriteStatus ('Загрузка пользователей... Найдено %d элементов.',[UsrCount]);
            Gauge.Enabled := TRUE;
            Gauge.Progress := 0;
            Gauge.MaxValue := UsrCount;
            Threads.Add ( TUsersLoader.Create ([ DBFileName,
                                                 lstUsers.Items,
                                                 NIL,
                                                 imgUsers,
                                                 Gauge,
                                                 StatusBar.Panels [pnlStatusText],
                                                 FALSE,
                                                 TRUE,
                                                 TP_NORMAL ]) );
        end
        else
            WriteStatus ('Список контактов пуст.');
    except on E: Exception do
        _raise (['GetUsersData',ERR_TFMMAIN_GET_USERS_DATA,E],
                ['{278A9F8C-7969-4FEF-B512-51A70217CD38}']);
    end;
end;

procedure TfmMain.GetURLData;
var
    URI        : String;
    L          : Integer;
    P          : Integer;
    KeyHash    : String;
    CtgKeyHash : String;
    MsgKeyHash : String;
    CtgID      : TID;
    MsgID      : TID;
begin
    try
        URI := Trim (edURL.Text);
        try
            { если это не ссылка, значит ведем поиск по ключевым словам }
            if ( Pos ('crypto://',URI) <= 0 ) then
            begin
                SetTabStatus (tabForum,tbsEmpty);
                tabs.ActivePage := tbForum;
                pnlNavigation.Parent := tabs.ActivePage;
                { отображаем поиск }
                CurrCtgID := 0;
                CurrMsgID := 0;
                { поиск по ключевым словам }
                GetKeyWordsData;
            end
            { если это ссылка - приводим ссылку к нормальному виду }
            else if ( Pos ('crypto://',URI) = 1 ) then
            begin
                URI := StrReplace (URI,'\','/');
                if ( Copy ( URI, Length (URI), 1 ) <> '/' ) then
                    URI := Format ('%s/',[URI]);
                edURL.Text := URI;
            end;
            { переходим по ссылке }
            if ( Pos ('crypto://categories/',URI) = 1 ) then
            begin
                SetTabStatus (tabForum,tbsEmpty);
                tabs.ActivePage := tbForum;
                pnlNavigation.Parent := tabs.ActivePage;
                { отображаем категории }
                CurrCtgID := 0;
                GetCategoriesData;
            end
            else if ( Pos ('crypto://categorie/',URI) = 1 ) then
            begin
                SetTabStatus (tabForum,tbsEmpty);
                tabs.ActivePage := tbForum;
                pnlNavigation.Parent := tabs.ActivePage;
                { отображаем сообщения категории }
                L := Length ('crypto://categorie/');
                KeyHash := Copy ( URI, L + 1, Length (URI) - L -1  );
                if not isEmpty (KeyHash) then
                begin
                    P := Pos ('#',KeyHash);
                    if not ( P > 0 ) then
                        CtgID := TCategories.Find (DB,KeyHash,USER_ID)
                    else
                    begin
                        L := Length (KeyHash);
                        CtgKeyHash := Copy (KeyHash,1,P-1);
                        CtgID := TCategories.Find (DB,CtgKeyHash,USER_ID);
                        MsgKeyHash := Copy (KeyHash,P+1,L);
                        MsgID := TMessages.Find (DB,MsgKeyHash,USER_ID);
                        if ( MsgID > 0 ) then
                            CurrMsgKeyHash := MsgKeyHash;
                    end;
                    if ( CtgID > 0 ) then
                    begin
                        { отображаем сообщения }
                        CurrCtgID := CtgID;
                        GetMessagesData;
                    end;
                end;
            end
            else if ( Pos ('crypto://users/',URI) = 1 ) then
            begin
                SetTabStatus (tabUsers,tbsEmpty);
                tabs.ActivePage := tbUsers;
                pnlNavigation.Parent := tabs.ActivePage;
                { отображаем список пользователей }
                GetUsersData;
            end
            else if ( Pos ('crypto://user/',URI) = 1 ) then
            begin
                SetTabStatus (tabUser,tbsEmpty);
                tabs.ActivePage := tbUser;
                pnlNavigation.Parent := tabs.ActivePage;
                { отображаем профиль пользователя }
                GetUserData;
            end
            else if ( Pos ('crypto://mail/',URI) = 1 ) then
            begin
                SetTabStatus (tabMail,tbsEmpty);
                tabs.ActivePage := tbMail;
                pnlNavigation.Parent := tabs.ActivePage;
                { отображаем список пакетов }
                GetMailData;
            end;
        finally
            _FillChar ( CtgKeyHash, Length (CtgKeyHash), $00 );
            _FillChar ( MsgKeyHash, Length (MsgKeyHash), $00 );
            _FillChar ( KeyHash, Length (KeyHash), $00 );
            _FillChar ( URI, Length (URI), $00 );
        end;
    except on E: Exception do
        _raise (['GetURLData',ERR_TFMMAIN_GET_URL_DATA,E],
                ['{0B417E32-49EA-4442-9F54-BFB2FB9C9E53}']);
    end;
end;

procedure TfmMain.GetMailData;
begin
    try
        WriteStatus ('Загрузка почты...');
        Gauge.Progress := 0;
        Gauge.MaxValue := 2;
        Progress (1,'Завершение потоков...');
        Threads.Terminate;
        Progress (1,'Очистка строки адреса...');
        edURL.Text := 'crypto://mail/';
        if ( MailThreads.Count = 0 ) then
        begin
            WriteStatus ('Очистка лога почтовых сообщений...');
            lstMail.Clear;
        end;
        WriteStatus ('Запуск почтовых служб...');
        {$IFDEF HTTP}
            { HTTP }
            if not Assigned ( MailThreads.ItemOf [ THTTPPackageServer.ClassName ] ) then
                MailThreads.Add ( THTTPPackageServer.Create ([ DBFileName,
                                                               User.Port,
                                                               User.TimeOut,
                                                               lstMail,
                                                               NIL,
                                                               StatusBar.Panels [pnlStatusText] ]) );
            if not Assigned ( MailThreads.ItemOf [ THTTPPackageClient.ClassName ] ) then
            begin
                if User.UseProxy then
                    MailThreads.Add ( THTTPPackageClient.Create ([ DBFileName,
                                                                   lstMail,
                                                                   NIL,
                                                                   StatusBar.Panels [pnlStatusText],
                                                                   User.TimeOut,
                                                                   User.ProxyIP,
                                                                   User.ProxyPort,
                                                                   User.ProxyLogin,
                                                                   User.ProxyPassword,
                                                                   User.ProxyProtocol ]) )
                else
                    MailThreads.Add ( THTTPPackageClient.Create ([ DBFileName,
                                                                   lstMail,
                                                                   NIL,
                                                                   StatusBar.Panels [pnlStatusText],
                                                                   User.TimeOut ]) );
            end;
        {$ENDIF HTTP}
        {$IFDEF SMTP_POP3}
            { SMTP }
            if notEmpty (User.SMTPHost) and ( User.SMTPPort > 0 ) and
               not Assigned ( MailThreads.ItemOf [ TSMTPPackageClient.ClassName ] )
            then
            begin
                if User.UseProxy and
                   (  ( User.ProxyProtocol = 'SOCKS4' ) or
                      ( User.ProxyProtocol = 'SOCKS5' )  )
                then
                    MailThreads.Add ( TSMTPPackageClient.Create ([ DBFileName,
                                                                   User.SMTPHost, User.SMTPPort,
                                                                   User.EMail, User.EmailPassword,
                                                                   User.AutoTLS, User.FullSSL,
                                                                   lstMail,
                                                                   NIL,
                                                                   StatusBar.Panels [pnlStatusText],
                                                                   User.TimeOut,
                                                                   User.ProxyIP,
                                                                   User.ProxyPort,
                                                                   User.ProxyLogin,
                                                                   User.ProxyPassword,
                                                                   User.ProxyProtocol ]) )
                else
                    MailThreads.Add ( TSMTPPackageClient.Create ([ DBFileName,
                                                                   User.SMTPHost, User.SMTPPort,
                                                                   User.EMail, User.EmailPassword,
                                                                   User.AutoTLS, User.FullSSL,
                                                                   lstMail,
                                                                   NIL,
                                                                   StatusBar.Panels [pnlStatusText],
                                                                   User.TimeOut ]) );
            end;
            { POP3 }
            if notEmpty (User.POP3Host) and ( User.POP3Port > 0 ) and
               not Assigned ( MailThreads.ItemOf [ TPOP3PackageClient.ClassName ] )
            then
            begin
                if User.UseProxy and
                   (  ( User.ProxyProtocol = 'SOCKS4' ) or
                      ( User.ProxyProtocol = 'SOCKS5' )  )
                then
                    MailThreads.Add ( TPOP3PackageClient.Create ([ DBFileName,
                                                                   User.POP3Host, User.POP3Port,
                                                                   User.EMail, User.EmailPassword,
                                                                   User.AutoTLS, User.FullSSL,
                                                                   lstMail,
                                                                   NIL,
                                                                   StatusBar.Panels [pnlStatusText],
                                                                   User.TimeOut,
                                                                   User.ProxyIP,
                                                                   User.ProxyPort,
                                                                   User.ProxyLogin,
                                                                   User.ProxyPassword,
                                                                   User.ProxyProtocol ]) )
                else
                    MailThreads.Add ( TPOP3PackageClient.Create ([ DBFileName,
                                                                   User.POP3Host, User.POP3Port,
                                                                   User.EMail, User.EmailPassword,
                                                                   User.AutoTLS, User.FullSSL,
                                                                   lstMail,
                                                                   NIL,
                                                                   StatusBar.Panels [pnlStatusText],
                                                                   User.TimeOut ]) );
            end;
        {$ENDIF SMTP_POP3}
        { обработка почты }
        if tabs.ActivePage = tbMail then
            SetMailData;
    except on E: Exception do
        _raise (['GetMailData',ERR_TFMMAIN_GET_MAIL_DATA,E],
                ['{99BB8726-35D8-40FD-806D-973651DF06F4}']);
    end;
end;

procedure TfmMain.SetMailData;
var
    PckCount : LongInt;
begin
    try
        WriteStatus ('Подсчет количества необработанных пакетов...');
        PckCount := TPackages.GetCount (DB,[ _([]),
                                             _([]),
                                             _([USER_KEY_HASH]),
                                             _([USER_ID]),
                                             _([]),
                                             _([PACKAGE_RECEIVED_STATUS_ID]),
                                             _([ TUser, {TPic,} TMessage, TCategorie, TMetaObject ]) ]);
        if ( PckCount > 0 ) then
        begin
            WriteStatus ('Обработка почты... Найдено %d элементов.',[PckCount]);
            if not Assigned ( MailThreads.ItemOf [ TPackagesScanner.ClassName ] ) then
                MailThreads.Add ( TPackagesScanner.Create ([ DBFileName,
                                                             lstMail,
                                                             NIL,
                                                             StatusBar.Panels [pnlStatusText] ]) );
        end
        else
            WriteStatus ('Обработка почты завершена.');
    except on E: Exception do
        _raise (['SetMailData',ERR_TFMMAIN_SET_MAIL_DATA,E],
                ['{8B6DA132-434A-435F-A51A-D000BEB4ACB0}']);
    end;
end;

procedure TfmMain.SetMailData (const anObject: TMetaObject);
begin
    try
        if not Assigned (anObject) then
            raise Exception.Create (ERR_TFMMAIN_INVALID_OBJECT);
        if not ( anObject is TUser ) and
           {not ( anObject is TPic ) and}
           not ( anObject is TCategorie ) and
           not ( anObject is TMessage ) then
            raise Exception.CreateFmt (ERR_TFMMAIN_INVALID_OBJECT_CLASS,[ anObject.ClassID ]);
        { создаем пакеты с данным объектом для списка контактов }
        if not Assigned ( MailThreads.ItemOf [ TPackagesConstructor.ClassName ] ) then
            MailThreads.Add ( TPackagesConstructor.Create ([ DBFileName,
                                                             anObject,
                                                             lstMail,
                                                             NIL,
                                                             StatusBar.Panels [pnlStatusText] ]) );
    except on E: Exception do
        _raise (['SetMailData',ERR_TFMMAIN_SET_MAIL_DATA,E],
                ['{ABB87E2C-BADA-47BE-A338-744BEC69088D}']);
    end;
end;

procedure TfmMain.UpdateMailData (const anUser: TUser);
var
    Package : TPackage;
begin
    try
        if not Assigned (anUser) or
           not ( anUser.ID > 0 ) or
           isEmpty (anUser.KeyHash) then
            raise Exception.Create (ERR_TFMMAIN_INVALID_OBJECT);
        { создаем пакет обмена структурой форума с данным пользователем }
        if ( anUser.ID <> USER_ID ) then
        begin
            if not Assigned ( MailThreads.ItemOf [ TForumUpdater.ClassName ] ) then
                MailThreads.Add ( TForumUpdater.Create ([ DBFileName,
                                                          anUser.ID,
                                                          lstMail,
                                                          NIL,
                                                          StatusBar.Panels [pnlStatusText] ]) );
        end;
    except on E: Exception do
        _raise (['UpdateMailData',ERR_TFMMAIN_UPDATE_MAIL_DATA,E],
                ['{FBF21002-63BF-4007-972F-4C0D0C22654C}']);
    end;
end;

procedure TfmMain.tmMailTimer (Sender: TObject);
begin
    GetMailData;
    SetMailData;
end;

procedure TfmMain.WriteStatus (const aMessage: String);
begin
    try
        StatusBar.Panels.Items [pnlStatusText].Text := aMessage;
        Application.ProcessMessages;
    except on E: Exception do
        _raise (['WriteStatus',ERR_TFMMAIN_WRITE_STATUS,E],
                ['{396E636B-F1B3-4364-8CC1-67C1E6C66EE6}']);
    end;
end;

procedure TfmMain.WriteStatus (const aMessage: String;
                               aParams: array of const);
begin
    try
        WriteStatus ( Format (aMessage,aParams) );
    except on E: Exception do
        _raise (['WriteStatus',ERR_TFMMAIN_WRITE_STATUS,E],
                ['{FDC58503-24C5-485C-B3F2-A13FB5F1711D}']);
    end;
end;

procedure TfmMain.Progress (const aCount: Integer);
begin
    try
        Gauge.Progress := Gauge.Progress + 1;
    except on E: Exception do
        _raise (['Progress',ERR_TFMMAIN_PROGRESS,E],
                ['{557D1C63-606C-4E99-9119-3F99405FA6C6}']);
    end;
end;

procedure TfmMain.Progress (const aCount: Integer;
                            const aMessage: String);
begin
    try
        Gauge.Progress := Gauge.Progress + aCount;
        WriteStatus (aMessage);
    except on E: Exception do
        _raise (['Progress',ERR_TFMMAIN_PROGRESS,E],
                ['{1E6921D6-C5B1-4482-A087-37E8932177FC}']);
    end;
end;

procedure TfmMain.Progress (const aCount: Integer;
                            const aMessage: String;
                            aParams: array of const);
begin
    try
        Gauge.Progress := Gauge.Progress + aCount;
        WriteStatus (aMessage,aParams);
    except on E: Exception do
        _raise (['Progress',ERR_TFMMAIN_PROGRESS,E],
                ['{1AC07D6B-8139-4C25-90C7-C4E90E26A99C}']);
    end;
end;

procedure TfmMain.MailLog (const aSender: String;
                           const aReceiver: String;
                           const aMessage: String;
                           const anImageIndex: Integer = pckDefault);
var
    Itm : TListItem;
begin
    try
        if Assigned (lstMail) then
        begin
            Itm := lstMail.Items.Add;
            Itm.Caption := _DateTimeToStr (now);
            Itm.ImageIndex := anImageIndex;
            Itm.SubItems.Add (aSender);
            Itm.SubItems.Add (aReceiver);
            Itm.SubItems.Add (aMessage);
        end;
    except on E: Exception do
        _raise (['MailLog',ERR_TFMMAIN_WRITE_LOG,E],
                ['{39271C36-4658-4C66-B6CF-FCA184DD862A}']);
    end;
end;

procedure TfmMain.MailLog (const aSender: String;
                           const aReceiver: String;
                           const aMessage: String;
                           aParams: array of const;
                           const anImageIndex: Integer = pckDefault);
begin
    try
        MailLog ( aSender, aReceiver, Format (aMessage,aParams), anImageIndex );
    except on E: Exception do
        _raise (['MailLog',ERR_TFMMAIN_WRITE_LOG,E],
                ['{B49FC541-3771-46D4-BA3B-0FC42F9C7168}']);
    end;
end;

procedure TfmMain.tabsChange (Sender: TObject);
begin
    try
        pnlNavigation.Parent := tabs.ActivePage;
        if Assigned (User) then
        begin
            GetData;
        end;
    except on E: Exception do
        _raise (['tabsChange',ERR_TFMMAIN_TABS_CHANGE,E],
                ['{8A15D684-2244-4055-89E5-C25520898F7B}']);
    end;
end;

procedure TfmMain.actHelpExecute (Sender: TObject);
begin
    try
        ShowMessage ('help');
    except on E: Exception do
        _raise (['actHelpExecute',ERR_TFMMAIN_ACTION_EXECUTE,E],
                ['{EA774467-445E-427A-984E-3343777D26C3}']);
    end;
end;

procedure TfmMain.actAboutExecute (Sender: TObject);
begin
    try

    except on E: Exception do
        _raise (['actAboutExecute',ERR_TFMMAIN_ACTION_EXECUTE,E],
                ['{997468B1-BDA8-4406-B54B-A8D72F7007DF}']);
    end;
end;

procedure TfmMain.actAddCategorieExecute (Sender: TObject);
var
    CtgIDParent : TID;
begin
    try
        CtgIDParent := ROOT_CATEGORIE_ID;
        if ( CurrCtgID > 0 ) then
            CtgIDParent := CurrCtgID;
        if AddCategorie (DBFileName,CtgIDParent) then
        begin
            SetTabStatus (tabForum,tbsEmpty);
            GetCategoriesData;
        end;
    except on E: Exception do
        _raise (['actAddCategorieExecute',ERR_TFMMAIN_ACTION_EXECUTE,E],
                ['{74F53C6F-2CCC-48F0-AAD4-D0ADB567A558}']);
    end;
end;

procedure TfmMain.actAddCategorieUpdate (Sender: TObject);
begin
    try
        actAddCategorie.Enabled := ( tabs.ActivePage = tbForum ) and
                                   Tree.Visible and
                                   ( Threads.Count = 0 );
    except on E: Exception do
        _raise (['actAddCategorieUpdate',ERR_TFMMAIN_ACTION_UPDATE,E],
                ['{456F64C2-B9C6-4F46-818D-A5DCB59B0698}']);
    end;
end;

procedure TfmMain.actEditCategorieExecute (Sender: TObject);
begin
    try
        if ( CurrCtgID > 0 ) and EditCategorie (DBFileName,CurrCtgID) then
        begin
            SetTabStatus (tabForum,tbsEmpty);
            GetCategoriesData;
        end;
    except on E: Exception do
        _raise (['actEditCategorieExecute',ERR_TFMMAIN_ACTION_EXECUTE,E],
                ['{EE574BCA-141C-482E-BB0A-23C0331B45CD}']);
    end;
end;

procedure TfmMain.actEditCategorieUpdate (Sender: TObject);
begin
    try
        actEditCategorie.Enabled := ( tabs.ActivePage = tbForum ) and
                                    Tree.Visible and
                                    Tree.Focused and
                                    ( CurrCtgID > 0 ) and
                                    ( Threads.Count = 0 );
    except on E: Exception do
        _raise (['actEditCategorieUpdate',ERR_TFMMAIN_ACTION_UPDATE,E],
                ['{51D102E8-4425-4CC2-B831-9FC6D28A220A}']);
    end;
end;

procedure TfmMain.actDeleteCategorieExecute (Sender: TObject);

begin
    try
        if ( CurrCtgID > 0 ) and DeleteCategorie (DBFileName,CurrCtgID) then
        begin
            SetTabStatus (tabForum,tbsEmpty);
            CurrCtgID := 0;
            GetCategoriesData;
        end;
    except on E: Exception do
        _raise (['actDeleteCategorieExecute',ERR_TFMMAIN_ACTION_EXECUTE,E],
                ['{0538AA37-62B7-4650-AA97-25A22D530FE9}']);
    end;
end;

procedure TfmMain.actDeleteCategorieUpdate (Sender: TObject);
begin
    try
        actDeleteCategorie.Enabled := ( tabs.ActivePage = tbForum ) and
                                      Tree.Visible and
                                      Tree.Focused and
                                      ( CurrCtgID > 0 ) and
                                      ( Threads.Count = 0 );
    except on E: Exception do
        _raise (['actDeleteCategorieUpdate',ERR_TFMMAIN_ACTION_UPDATE,E],
                ['{FE1D74B2-6552-426A-A07D-B38A49E41F0F}']);
    end;
end;

procedure TfmMain.actAddUserExecute (Sender: TObject);
begin
    try
        if AddUser (DBFileName,'') then
        begin
            SetTabStatus (tabUsers,tbsEmpty);
            GetUsersData;
        end;
    except on E: Exception do
        _raise (['actAddUserExecute',ERR_TFMMAIN_ACTION_EXECUTE,E],
                ['{A6DFEE2E-DAB1-4136-BE95-08F6C063706E}']);
    end;
end;

procedure TfmMain.actAddUserUpdate (Sender: TObject);
begin
    try
        actAddUser.Enabled := ( tabs.ActivePage = tbUsers ) and
                              lstUsers.Visible and
                              ( Threads.Count = 0 );
    except on E: Exception do
        _raise (['actAddUserUpdate',ERR_TFMMAIN_ACTION_UPDATE,E],
                ['{D687C931-A410-4FCE-A475-B98D1149E745}']);
    end;
end;

procedure TfmMain.actEditUserExecute (Sender: TObject);
begin
    try
        if ( CurrUsrID > 0 ) and EditUser (DBFileName,CurrUsrID) then
        begin
            SetTabStatus (tabUsers,tbsEmpty);
            GetUsersData;
        end;
    except on E: Exception do
        _raise (['actEditUserExecute',ERR_TFMMAIN_ACTION_EXECUTE,E],
                ['{10A9AFDB-8D04-4942-B473-293BD16BA122}']);
    end;
end;

procedure TfmMain.actEditUserUpdate (Sender: TObject);
begin
    try
        actEditUser.Enabled := ( tabs.ActivePage = tbUsers ) and
                               lstUsers.Visible and
                               lstUsers.Focused and
                               ( CurrUsrID > 0 ) and
                               ( Threads.Count = 0 );
    except on E: Exception do
        _raise (['actEditUserUpdate',ERR_TFMMAIN_ACTION_UPDATE,E],
                ['{30433AAB-01D1-4AC7-94EC-E7BC52281E95}']);
    end;
end;

procedure TfmMain.actDeleteUserExecute (Sender: TObject);
begin
    try
        if ( CurrUsrID > 0 ) and DeleteUser (DBFileName,CurrUsrID) then
        begin
            SetTabStatus (tabUsers,tbsEmpty);
            CurrUsrID := 0;
            GetUsersData;
        end;
    except on E: Exception do
        _raise (['actDeleteUserExecute',ERR_TFMMAIN_ACTION_EXECUTE,E],
                ['{F16AB7AD-C158-4A04-A8A9-D6708A4F6643}']);
    end;
end;

procedure TfmMain.actDeleteUserUpdate (Sender: TObject);
begin
    try
        actDeleteUser.Enabled := ( tabs.ActivePage = tbUsers ) and
                                 lstUsers.Visible and
                                 lstUsers.Focused and
                                 ( CurrUsrID > 0 ) and
                                 ( Threads.Count = 0 );
    except on E: Exception do
        _raise (['actDeleteUserUpdate',ERR_TFMMAIN_ACTION_UPDATE,E],
                ['{C7A220E5-5F40-4337-B7D3-E5B49A8FD3BD}']);
    end;
end;

procedure TfmMain.actSynchronizeUserExecute (Sender: TObject);
var
    LocalUser : TUser;
begin
    try
        if ( CurrUsrID > 0 ) then
        begin
            LocalUser := TUser.Load (DB,CurrUsrID,[objSimple]) as TUser;
            if Assigned (LocalUser) then
            try
                UpdateMailData (LocalUser);
            finally
                FreeAndNil (LocalUser);
            end;
        end;
    except on E: Exception do
        _raise (['actSynchronizeUserExecute',ERR_TFMMAIN_ACTION_EXECUTE,E],
                ['{470EF8CC-4481-4130-A6C5-6A4792763109}']);
    end;
end;

procedure TfmMain.actSynchronizeUserUpdate (Sender: TObject);
begin
    try
        actSynchronizeUser.Enabled := ( tabs.ActivePage = tbUsers ) and
                                      lstUsers.Visible and
                                      lstUsers.Focused and
                                      ( CurrUsrID > 0 ) and
                                      ( Threads.Count = 0 );
    except on E: Exception do
        _raise (['actSynchronizeUserUpdate',ERR_TFMMAIN_ACTION_UPDATE,E],
                ['{F9423DC5-61F1-4C9B-B428-61DE788D9A77}']);
    end;
end;

procedure TfmMain.actQuoteMessageExecute (Sender: TObject);
begin
    try
        if notEmpty (  TTmpMessage (CurrMsgTmp).edMsg.SelText ) then
        begin
            edReply.SelText := Format ('[QUOTE:%s %s]%s[/QUOTE]',
                                       [ PMessageNode (TTmpMessage (CurrMsgTmp).Data)^.Author,
                                         _DateTimeToStr ( PMessageNode (TTmpMessage (CurrMsgTmp).Data)^.TimeStampCreate ),
                                         TTmpMessage (CurrMsgTmp).edMsg.SelText ]);
        end
        else if notEmpty (  PMessageNode (TTmpMessage (CurrMsgTmp).Data)^.Text  ) then
        begin
            edReply.SelText := Format ('[QUOTE:%s %s]%s[/QUOTE]',
                                       [ PMessageNode (TTmpMessage (CurrMsgTmp).Data)^.Author,
                                         _DateTimeToStr ( PMessageNode (TTmpMessage (CurrMsgTmp).Data)^.TimeStampCreate ),
                                         PMessageNode (TTmpMessage (CurrMsgTmp).Data)^.Text ]);
        end;
    except on E: Exception do
        _raise (['actQuoteMessageExecute',ERR_TFMMAIN_ACTION_EXECUTE,E],
                ['{6D73D183-DB3C-4634-9D2A-29D9AC3E4D1A}']);
    end;
end;

procedure TfmMain.actQuoteMessageUpdate (Sender: TObject);
begin
    try
        actQuoteMessage.Enabled := ( CurrMsgID > 0 ) and
                                     Assigned (CurrMsgTmp) and Assigned ( TTmpMessage (CurrMsgTmp).Data ) and
                                   ( CurrCtgID > 0 ) and ( Threads.Count = 0 );
    except on E: Exception do
        _raise (['actQuoteMessageUpdate',ERR_TFMMAIN_ACTION_UPDATE,E],
                ['{1E00E4E7-B4EA-4ED6-8CBE-73B4E41DA7B4}']);
    end;
end;

procedure TfmMain.actAddMessageExecute (Sender: TObject);
var
    Stream  : TStream;
    Buffer  : String;
    L       : LongWord;
    Msg     : TMessage;
    Ctg     : TCategorie;
begin
    try
        if isEmpty ( Trim (edReply.Text) ) then Exit;
        { редактирование сообщения }
        if ( EditMsgID > 0 ) then
        begin
            { следим за тем, чтобы сообщение не превышало максимальную длину }
            Buffer := Copy (edReply.Text,1,MAX_MESSAGE_LENGTH);
            L := Length (Buffer);
            if ( L > 0 ) then
            try
                Msg := TMessage.Load (DB,EditMsgID) as TMessage;
                if not Assigned (Msg) then
                    raise Exception.CreateFmt (ERR_TFMMAIN_GET_MESSAGE_DATA,[EditMsgID]);
                try
                    Msg.Text := Buffer;
                    WriteStatus ('Сохранение...');
                    Msg.Save;
                    WriteStatus ('Создание пакетов...');
                    SetMailData (Msg);
                finally
                    FreeAndNil (Msg);
                end;
            finally
                _FillChar ( Buffer, L, $00 );
                edReply.Clear;
                edReply.MaxLength := 0;
                EditMsgID := 0;
            end;
            { обновляем список сообщений }
            SetTabStatus (tabForum,tbsEmpty);
            GetMessagesData;
        end
        { создание нового сообщения }
        else if ( CurrCtgID > 0 ) then
        begin
            Ctg := TCategorie.Load (DB,CurrCtgID) as TCategorie;
            try
                { разбиваем введенное сообщение на части, если оно слишком длинное }
                Stream := TStringStream.Create (edReply.Text);
                with TStringStream (Stream) do
                try
                    Gauge.Progress := 0;
                    Gauge.MaxValue := Size;
                    Position := 0;
                    while ( Position < Size ) do
                    begin
                        Buffer := ReadString (MAX_MESSAGE_LENGTH);
                        L := Length (Buffer);
                        if ( L > 0 ) then
                        try
                            Msg := TMessage.Create (DB,[0,CurrCtgID,USER_ID,USER_ID,MESSAGE_FORUM_TYPE_ID,MESSAGE_ACTIVE_STATUS_ID]);
                            try
                                if Assigned (Ctg) and ( Ctg.ID > 0 ) then
                                    Msg.Subject := Ctg.Name;
                                Msg.Text := Buffer;
                                Progress (L,'Сохранение...');
                                Msg.Save;
                                WriteStatus ('Создание пакетов...');
                                SetMailData (Msg);
                            finally
                                FreeAndNil (Msg);
                            end;
                        finally
                            _FillChar ( Buffer, L, $00 );
                        end;
                    end;
                finally
                    FreeAndNil (Stream);
                end;
            finally
                edReply.Clear;
                FreeAndNil (Ctg);
            end;
            { обновляем список сообщений }
            SetTabStatus (tabForum,tbsEmpty);
            GetMessagesData;
        end;
    except on E: Exception do
        _raise (['actAddMessageExecute',ERR_TFMMAIN_ACTION_EXECUTE,E],
                ['{8D9D2B47-84D0-42BE-9A8C-ED4911ED02C2}']);
    end;
end;

procedure TfmMain.actAddMessageUpdate (Sender: TObject);
begin
    try
        actAddMessage.Enabled := ( CurrCtgID > 0 ) and ( Threads.Count = 0 );
    except on E: Exception do
        _raise (['actAddMessageUpdate',ERR_TFMMAIN_ACTION_UPDATE,E],
                ['{C2ED6DC9-32F2-400D-B96F-5757B704C16B}']);
    end;
end;

procedure TfmMain.actAddExecute (Sender: TObject);
begin
    try
        if ( tabs.ActivePage = tbForum ) and Tree.Visible and actAddCategorie.Enabled then
            actAddCategorie.OnExecute (Sender)
        else if ( tabs.ActivePage = tbUsers ) and lstUsers.Visible and actAddUser.Enabled then
            actAddUser.OnExecute (Sender)
    except on E: Exception do
        _raise (['actAddExecute',ERR_TFMMAIN_ACTION_EXECUTE,E],
                ['{D339A130-7CD7-4AE1-B312-99B3E856CBA1}']);
    end;
end;

procedure TfmMain.actAddUpdate (Sender: TObject);
begin
    try
        if ( tabs.ActivePage = tbForum ) and Tree.Visible then
        begin
            actAddCategorieUpdate (Sender);
            actAdd.Enabled := actAddCategorie.Enabled;
        end
        else if ( tabs.ActivePage = tbUsers ) and lstUsers.Visible then
        begin
            actAddUserUpdate (Sender);
            actAdd.Enabled := actAddUser.Enabled;
        end
        else
            actAdd.Enabled := FALSE;
    except on E: Exception do
        _raise (['actAddUpdate',ERR_TFMMAIN_ACTION_UPDATE,E],
                ['{5597AD36-AE9E-49F5-B711-0D6984EFA977}']);
    end;
end;

procedure TfmMain.actEditMessageExecute (Sender: TObject);
begin
    try
        if ( CurrMsgID > 0 ) and
           Assigned (CurrMsgTmp) and Assigned ( TTmpMessage (CurrMsgTmp).Data ) then
        begin
            EditMsgID := CurrMsgID;
            edReply.Clear;
            edReply.MaxLength := MAX_MESSAGE_LENGTH;
            edReply.Text := PMessageNode ( TTmpMessage (CurrMsgTmp).Data )^.Text;
        end;
    except on E: Exception do
        _raise (['actEditMessageExecute',ERR_TFMMAIN_ACTION_EXECUTE,E],
                ['{61F5F614-A4E6-411C-B030-AA7ADD2AFF0F}']);
    end;
end;

procedure TfmMain.actEditMessageUpdate (Sender: TObject);
begin
    try
        actEditMessage.Enabled := ( CurrMsgID > 0 ) and
                                  Assigned (CurrMsgTmp) and Assigned ( TTmpMessage (CurrMsgTmp).Data ) and
                                  (  PMessageNode ( TTmpMessage (CurrMsgTmp).Data ).IDAuthor = USER_ID  ) and
                                  ( CurrCtgID > 0 ) and ( Threads.Count = 0 );
    except on E: Exception do
        _raise (['actEditMessageUpdate',ERR_TFMMAIN_ACTION_UPDATE,E],
                ['{2F181639-26DF-40CD-A327-935DA2F3A590}']);
    end;
end;

procedure TfmMain.actEditExecute (Sender: TObject);
begin
    try
        if ( tabs.ActivePage = tbForum ) and Tree.Visible and actEditCategorie.Enabled then
            actEditCategorie.OnExecute (Sender)
        else if ( tabs.ActivePage = tbForum ) and not Tree.Visible and actEditMessage.Enabled then
            actEditMessage.OnExecute (Sender)
        else if ( tabs.ActivePage = tbUsers ) and lstUsers.Visible and actEditUser.Enabled then
            actEditUser.OnExecute (Sender)
    except on E: Exception do
        _raise (['actEditExecute',ERR_TFMMAIN_ACTION_EXECUTE,E],
                ['{DC03E9A3-95B0-4C3E-934C-D1AEC96D1444}']);
    end;
end;

procedure TfmMain.actEditUpdate (Sender: TObject);
begin
    try
        if ( tabs.ActivePage = tbForum ) and Tree.Visible then
        begin
            actEditCategorieUpdate (Sender);
            actEdit.Enabled := actEditCategorie.Enabled;
        end
        else if ( tabs.ActivePage = tbForum ) and not Tree.Visible then
        begin
            actEditMessageUpdate (Sender);
            actEdit.Enabled := actEditMessage.Enabled;
        end
        else if ( tabs.ActivePage = tbUsers ) and lstUsers.Visible then
        begin
            actEditUserUpdate (Sender);
            actEdit.Enabled := actEditUser.Enabled;
        end
        else
            actEdit.Enabled := FALSE;
    except on E: Exception do
        _raise (['actEditUpdate',ERR_TFMMAIN_ACTION_UPDATE,E],
                ['{4FE8EBCB-7216-4058-AC44-57D92788CBB2}']);
    end;
end;

procedure TfmMain.actDeleteMessageExecute (Sender: TObject);
begin
    try
        if ( CurrMsgID > 0 ) and DeleteMessage (DBFileName,CurrMsgID) then
        begin
            SetTabStatus (tabForum,tbsEmpty);
            GetMessagesData;
        end;
    except on E: Exception do
        _raise (['actDeleteMessageExecute',ERR_TFMMAIN_ACTION_EXECUTE,E],
                ['{24B93374-9909-4D26-A91A-A8E66D4F8F86}']);
    end;
end;

procedure TfmMain.actDeleteMessageUpdate (Sender: TObject);
begin
    try
        actDeleteMessage.Enabled := ( CurrMsgID > 0 ) and
                                    Assigned (CurrMsgTmp) and Assigned ( TTmpMessage (CurrMsgTmp).Data ) and
                                    (  PMessageNode ( TTmpMessage (CurrMsgTmp).Data ).IDAuthor = USER_ID  ) and
                                    ( CurrCtgID > 0 ) and ( Threads.Count = 0 );
    except on E: Exception do
        _raise (['actDeleteMessageUpdate',ERR_TFMMAIN_ACTION_UPDATE,E],
                ['{AF751C3C-29C9-46C6-9806-60F2E9B553A5}']);
    end;
end;

procedure TfmMain.actDeleteExecute (Sender: TObject);
begin
    try
        if ( tabs.ActivePage = tbForum ) and Tree.Visible and actDeleteCategorie.Enabled then
            actDeleteCategorie.OnExecute (Sender)
        else if ( tabs.ActivePage = tbForum ) and not Tree.Visible and actDeleteMessage.Enabled then
            actDeleteMessage.OnExecute (Sender)
        else if ( tabs.ActivePage = tbUsers ) and lstUsers.Visible and actDeleteUser.Enabled then
            actDeleteUser.OnExecute (Sender)
    except on E: Exception do
        _raise (['actDeleteExecute',ERR_TFMMAIN_ACTION_EXECUTE,E],
                ['{FB986680-69A5-4953-93D6-6346C2233B4D}']);
    end;
end;

procedure TfmMain.actDeleteUpdate (Sender: TObject);
begin
    try
        if ( tabs.ActivePage = tbForum ) and Tree.Visible then
        begin
            actDeleteCategorieUpdate (Sender);
            actDelete.Enabled := actDeleteCategorie.Enabled;
        end
        else if ( tabs.ActivePage = tbForum ) and not Tree.Visible then
        begin
            actDeleteMessageUpdate (Sender);
            actDelete.Enabled := actDeleteMessage.Enabled;
        end
        else if ( tabs.ActivePage = tbUsers ) and lstUsers.Visible then
        begin
            actDeleteUserUpdate (Sender);
            actDelete.Enabled := actDeleteUser.Enabled;
        end
        else
            actDelete.Enabled := FALSE;
    except on E: Exception do
        _raise (['actDeleteUpdate',ERR_TFMMAIN_ACTION_UPDATE,E],
                ['{CB473CAC-EF7D-419F-B0E1-579EBF5E19C1}']);
    end;
end;

procedure TfmMain.actSaveUserExecute (Sender: TObject);
begin
    try
        SetUserData;
    except on E: Exception do
        _raise (['actSaveUserExecute',ERR_TFMMAIN_ACTION_EXECUTE,E],
                ['{D9D758F8-F148-4FBE-8AAA-7E089BDCD89F}']);
    end;
end;

procedure TfmMain.actSaveUserUpdate (Sender: TObject);
begin
    try
        {actSaveUser.Enabled :=
        ( Trim (UserMail.Text) <> User.EMail ) or
        ( UserMailPassword.Text <> User.EMailPassword ) or
        ( Trim (UserIP.Text) <> User.IP ) or
        ( UserPort.Value <> User.Port ) or
        ( Trim (UserDescription.Text) <> User.Description ) or
        ( UserSex.ItemIndex <> User.Sex ) or
        ( UserBirthday.Date <> User.Birthday ) or
        ( Trim (UserRandom.Text) <> User.Crypto.genRandom ) or
        ( Trim (UserAsymmetric.Text) <> User.Crypto.algAsymmetric ) or
        ( Trim (UserSymmetric.Text) <> User.Crypto.algSymmetric ) or
        ( Trim (UserMode.Text) <> User.Crypto.modeSymmetric ) or
        ( Trim (UserHash.Text) <> User.Crypto.algHash ) or
        ( Trim (UserPublicKey.Text) <> User.PublicKey ) or
        (  cbxUserUseProxy.Checked and
           ( Trim (UserProxyIP.Text) <> '' ) and
           ( UserProxyPort.Value > 0 ) <> User.UseProxy  ) or
        ( Trim (UserProxyIP.Text) <> User.ProxyIP ) or
        ( Trim (UserProxyLogin.Text) <> User.ProxyLogin ) or
        ( Trim (UserProxyPassword.Text) <> User.ProxyPassword );}
    except on E: Exception do
        _raise (['actSaveUserUpdate',ERR_TFMMAIN_ACTION_UPDATE,E],
                ['{E0B44CC4-0F39-413D-828A-3B4ECF3246AF}']);
    end;
end;

procedure TfmMain.actRefreshExecute (Sender: TObject);
begin
    try
        SetTabStatus (tabForum,tbsEmpty);
        SetTabStatus (tabUsers,tbsEmpty);
        SetTabStatus (tabUser,tbsEmpty);
        SetTabStatus (tabMail,tbsEmpty);
        GetData;
    except on E: Exception do
        _raise (['actRefreshExecute',ERR_TFMMAIN_ACTION_EXECUTE,E],
                ['{438F56ED-51EC-4B52-A719-218CDA47E1EE}']);
    end;
end;

procedure TfmMain.actRefreshUpdate (Sender: TObject);
begin
    try
        actRefresh.Enabled := ( Threads.Count = 0 );
    except on E: Exception do
        _raise (['actRefreshUpdate',ERR_TFMMAIN_ACTION_UPDATE,E],
                ['{9E9A41E8-B5BE-4673-B691-75BF5B55B3DF}']);
    end;
end;

procedure TfmMain.actRefreshUserExecute (Sender: TObject);
begin
    actRefresh.OnExecute (Sender);
end;

procedure TfmMain.actRefreshUserUpdate (Sender: TObject);
begin
    actRefreshUser.Enabled := actSaveUser.Enabled;
end;

procedure TfmMain.actRefreshMessageExecute (Sender: TObject);
begin
    EditMsgID := 0;
    edReply.Clear;
end;

procedure TfmMain.actRefreshMessageUpdate (Sender: TObject);
begin
    actRefreshMessage.Enabled := ( EditMsgID > 0 ) and actAddMessage.Enabled;
end;

procedure TfmMain.actURLExecute (Sender: TObject);
var
    URI : String;
    I   : Integer;
begin
    try
        { history }
        URI := Trim (edURL.Text);
        try
            if not isEmpty ( Trim (URI)  ) then
            begin
                { очищаем историю после текущего состояния }
                for I := HistoryIndex + 1 to History.Count - 1 do
                    History.Delete (I);
                { добавляем новое состояние в историю }
                History.Add (URI);
                HistoryIndex := History.Count - 1;
                { переходим по ссылке }
                GetURLData;
            end
        finally
            _FillChar ( URI, Length (URI), $00 );
        end;
    except on E: Exception do
        _raise (['actURLExecute',ERR_TFMMAIN_ACTION_EXECUTE,E],
                ['{650394D8-6DF2-4BE0-B8F8-7DC4C7C7E8FC}']);
    end;
end;

procedure TfmMain.actURLUpdate (Sender: TObject);
begin
    try
        actURL.Enabled := not isEmpty ( Trim (edURL.Text) ) and ( Threads.Count = 0 );
    except on E: Exception do
        _raise (['actURLUpdate',ERR_TFMMAIN_ACTION_UPDATE,E],
                ['{9B1B5395-95B6-4BE6-8F3A-AED1E574ED41}']);
    end;
end;

procedure TfmMain.actHomeExecute (Sender: TObject);
begin
    edURL.Text := 'crypto://categories/';
    if actURL.Enabled then
        actURL.Execute;
end;

procedure TfmMain.actPrevExecute (Sender: TObject);
var
    URI : String;
begin
    try
        HistoryIndex := HistoryIndex - 1;
        edURL.Text := History [HistoryIndex];
        URI := Trim (edURL.Text);
        try
            if not isEmpty ( Trim (URI)  ) then
            begin
                { переходим по ссылке }
                GetURLData;
            end
        finally
            _FillChar ( URI, Length (URI), $00 );
        end;
    except on E: Exception do
        _raise (['actPrevExecute',ERR_TFMMAIN_ACTION_EXECUTE,E],
                ['{5B0E953A-F6E1-4BA1-A1D3-6663855FEF37}']);
    end;
end;

procedure TfmMain.actPrevUpdate (Sender: TObject);
begin
    try
        actPrev.Enabled := ( History.Count > 0 ) and ( HistoryIndex > 0 ) and ( Threads.Count = 0 );
    except on E: Exception do
        _raise (['actPrevUpdate',ERR_TFMMAIN_ACTION_UPDATE,E],
                ['{CAF12CFB-FF08-4F40-A862-823C2533183F}']);
    end;
end;

procedure TfmMain.actNextExecute (Sender: TObject);
var
    URI : String;
begin
    try
        HistoryIndex := HistoryIndex + 1;
        edURL.Text := History [HistoryIndex];
        URI := Trim (edURL.Text);
        try
            if not isEmpty ( Trim (URI)  ) then
            begin
                { переходим по ссылке }
                GetURLData;
            end
        finally
            _FillChar ( URI, Length (URI), $00 );
        end;
    except on E: Exception do
        _raise (['actNextExecute',ERR_TFMMAIN_ACTION_EXECUTE,E],
                ['{6048F328-6937-4195-BC35-0B7A85FFBA9B}']);
    end;
end;

procedure TfmMain.actNextUpdate (Sender: TObject);
begin
    try
        actNext.Enabled := ( History.Count > 0 ) and ( HistoryIndex < History.Count-1 ) and ( Threads.Count = 0 );
    except on E: Exception do
        _raise (['actNextUpdate',ERR_TFMMAIN_ACTION_UPDATE,E],
                ['{2B266B7A-E046-4FF9-BF16-1F12493FA4A9}']);
    end;
end;

procedure TfmMain.FormResize (Sender: TObject);
begin
    try
        if Assigned (ActiveLeftButton) then
            imgArrowRed.Top := TWinControl (ActiveLeftButton).Top + TWinControl (ActiveLeftButton).Height div 2
        else
            imgArrowRed.Top := -1 * imgArrowRed.Height;
    except on E: Exception do
        _raise (['FormResize',ERR_TFMMAIN_RESIZE,E],
                ['{30E7B9BA-8F1F-49F3-A00D-537BF45D1586}']);
    end;
end;

procedure TfmMain.btBoldClick (Sender: TObject);
var
    S : String;
begin
    try
        S := edReply.SelText;
        try
            if notEmpty (S) and
               (  ( Pos ('[B]',S) > 0  ) or
                  ( Pos ('[/B]',S) > 0 )  ) then
            begin
                S := StrReplace (S,'[B]','');
                S := StrReplace (S,'[/B]','');
                edReply.SelText := S;
            end
            else if notEmpty (S) then
                edReply.SelText := Format ('[B]%s[/B]',[S]);
        finally
            _FillChar ( S, Length (S), $00 );
        end;
    except on E: Exception do
        _raise (['btBoldClick',ERR_TFMMAIN_SET_TEXT_ATTRIBUTES,E],
                ['{7FDD1F02-A688-4F6F-9BC0-3A37CC124495}']);
    end;
end;

procedure TfmMain.btUnderlineClick (Sender: TObject);
var
    S : String;
begin
    try
        S := edReply.SelText;
        try
            if notEmpty (S) and
               (  ( Pos ('[U]',S) > 0  ) or
                  ( Pos ('[/U]',S) > 0 )  ) then
            begin
                S := StrReplace (S,'[U]','');
                S := StrReplace (S,'[/U]','');
                edReply.SelText := S;
            end
            else if notEmpty (S) then
                edReply.SelText := Format ('[U]%s[/U]',[S]);
        finally
            _FillChar ( S, Length (S), $00 );
        end;
    except on E: Exception do
        _raise (['btUnderlineClick',ERR_TFMMAIN_SET_TEXT_ATTRIBUTES,E],
                ['{02FD59AD-E9FB-4AA8-89FE-073561FD1E87}']);
    end;
end;

procedure TfmMain.btItalicClick (Sender: TObject);
var
    S : String;
begin
    try
        S := edReply.SelText;
        try
            if notEmpty (S) and
               (  ( Pos ('[I]',S) > 0  ) or
                  ( Pos ('[/I]',S) > 0 )  ) then
            begin
                S := StrReplace (S,'[I]','');
                S := StrReplace (S,'[/I]','');
                edReply.SelText := S;
            end
            else if notEmpty (S) then
                edReply.SelText := Format ('[I]%s[/I]',[S]);
        finally
            _FillChar ( S, Length (S), $00 );
        end;
    except on E: Exception do
        _raise (['btItalicClick',ERR_TFMMAIN_SET_TEXT_ATTRIBUTES,E],
                ['{F3EC7F17-8D97-4012-AAF2-84C4827884CC}']);
    end;
end;

procedure TfmMain.btKeyClick (Sender: TObject);
var
    S : String;
begin
    try
        S := edReply.SelText;
        try
            if notEmpty (S) and
               (  ( Pos ('[KEY]',S) > 0  ) or
                  ( Pos ('[/KEY]',S) > 0 )  ) then
            begin
                S := StrReplace (S,'[KEY]','');
                S := StrReplace (S,'[/KEY]','');
                edReply.SelText := S;
            end
            else if notEmpty (S) then
                edReply.SelText := Format ('[KEY]%s[/KEY]',[S]);
        finally
            _FillChar ( S, Length (S), $00 );
        end;
    except on E: Exception do
        _raise (['btKeyClick',ERR_TFMMAIN_SET_TEXT_ATTRIBUTES,E],
                ['{65BCEB8E-BB2C-4478-ACFE-49A0AAC5011C}']);
    end;
end;

procedure TfmMain.btColorMouseDown (Sender: TObject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: Integer);
begin
    try
        if notEmpty (edReply.SelText) then
        begin
            btColor.Color := GetColor (btColor.Color);
            edReply.SelText := Format ('[COLOR:#%s]%s[/COLOR]',
                                       [ ColorToHex (btColor.Color), edReply.SelText ]);
        end;
    except on E: Exception do
        _raise (['btColorMouseDown',ERR_TFMMAIN_GET_COLOR,E],
                ['{97C8A015-1849-46EE-B8F1-F13923D59670}']);
    end;
end;

procedure TfmMain.btSmileMouseDown (Sender: TObject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: Integer);
var
    Smile : ShortString;
begin
    try
        Smile := GetSmile (GetImgSmiles);
        if notEmpty (Smile) then
            edReply.SelText := edReply.SelText + Format (' %s ',[Smile]);
    except on E: Exception do
        _raise (['btSmileMouseDown',ERR_TFMMAIN_GET_SMILE,E],
                ['{21BF9139-ADE2-43FF-AEBE-A5D43552D6ED}']);
    end;
end;

procedure TfmMain.OnButtonMove (Sender: TObject; Shift: TShiftState; X,Y: Integer);
begin
    if TWinControl (Sender).Enabled then
        StatusBar.Panels.Items [2].Text := TWinControl (Sender).Hint;
end;

procedure TfmMain.btLeftPanelClick (Sender: TObject);
begin
    pnlLeft.Visible := not pnlLeft.Visible;
    case pnlLeft.Visible of
        FALSE : TsSpeedButton (Sender).ImageIndex := 0;
        TRUE  : TsSpeedButton (Sender).ImageIndex := 1;
    end;
end;

procedure TfmMain.OnLeftButtonMove (Sender: TObject; Shift: TShiftState; X,Y: Integer);
begin
    ActiveLeftButton := Sender;
    imgArrowRed.Top := TWinControl (Sender).Top + TWinControl (Sender).Height div 2;
    StatusBar.Panels.Items [2].Text := TWinControl (Sender).Hint;
end;

procedure TfmMain.OnBackGroundMove (Sender: TObject; Shift: TShiftState; X,Y: Integer);
begin
    StatusBar.Panels.Items [2].Text := '';
end;

procedure TfmMain.OnRichEditMouseDown (Sender: TObject; Button: TMouseButton;
                                       Shift: TShiftState; X, Y: Integer);
begin
    TRichEdit (Sender).SetFocus;
    if ( Button = mbRight ) then
        TRichEdit (Sender).PopupMenu.Popup (Mouse.CursorPos.X,
                                            Mouse.CursorPos.Y);
end;

procedure TfmMain.OnEditChange (Sender: TObject);
begin
    //actSaveUpdate (actSave);
    if ( tabs.ActivePage = tbUser ) then
    begin
        actSaveUser.Enabled := TRUE;
        if ( Sender = cbxUserUseProxy ) then
            cbxUserUseProxyClick (Sender);
    end;
end;

procedure TfmMain.OnControlMouseWheel (Sender: TObject;
                                       Shift: TShiftState;
                                       WheelDelta: Integer;
                                       MousePos: TPoint;
                                       var Handled: Boolean);
begin
    if ( tabs.ActivePage = tbForum ) and
       ( ActiveControl <> bxForum ) and
       ( ActiveControl <> edReply ) then
    begin
        bxForum.VertScrollBar.Position := bxForum.VertScrollBar.Position - WheelDelta;
    end
    else if ( tabs.ActivePage = tbUsers ) and
            ( ActiveControl <> bxUsers ) then
    begin
        bxUsers.VertScrollBar.Position := bxUsers.VertScrollBar.Position - WheelDelta;
    end
    else if ( tabs.ActivePage = tbUser ) and
            ( ActiveControl <> bxUser ) then
    begin
        bxUser.VertScrollBar.Position := bxUser.VertScrollBar.Position - WheelDelta;
    end
    else if ( tabs.ActivePage = tbMail ) and
            ( ActiveControl <> lstMail ) then
    begin
        lstMail.ScrollBy (0,WheelDelta);
    end;
end;

procedure TfmMain.TreeAdvancedCustomDrawItem (Sender: TCustomTreeView;
                                              Node: TTreeNode;
                                              State: TCustomDrawState;
                                              Stage: TCustomDrawStage;
                                              var PaintImages, DefaultDraw: Boolean);
var
    Rect     : TRect;
    Child    : TTreeNode;
    IndexPic : Integer;
    Text     : String;
    S        : String;
    P        : Integer;
begin
    with Sender.Canvas do
    begin
        Font.Color := clWhite;
        { иконка }
        IndexPic := 0;
        if Assigned (Node.Data) and not PCategorieNode (Node.Data)^.isMessage then
        begin
            IndexPic := PCategorieNode (Node.Data)^.IndexPic;
            Child := Node.GetFirstChild;
            if Assigned (Child) then
            begin
                Node.ImageIndex := IndexPic*4 + 2;
                Node.SelectedIndex := IndexPic*4 + 3;
            end
            else
            begin
                Node.ImageIndex    := IndexPic*4 + 0;
                Node.SelectedIndex := IndexPic*4 + 1;
            end;
        end
        else if Assigned (Node.Data) and PCategorieNode (Node.Data)^.isMessage then
        begin
            IndexPic := TsTreeView (Sender).Images.Count - 1;
            Node.ImageIndex    := IndexPic;
            Node.SelectedIndex := IndexPic;
        end;
        { фон }
        Rect := Node.DisplayRect (TRUE);
        Rect.Right := Sender.Width - 132;
        if Node.Selected then
        begin
            Brush.Color := clMaroon;
            Pen.Color := clMaroon;
            FillRect (Rect);
        end
        else
        begin
            Brush.Color := TsTreeView (Sender).Color;
            Pen.Color := TsTreeView (Sender).Color;
            FillRect (Rect);
        end;
        { текст }
        Rect.Left := Rect.Left + 8;
        Rect.Right := Rect.Right - 8;
        Rect.Top := Rect.Top + 4;
        Text := Node.Text;
        if Assigned (Node.Data) then
            Text := Format ('%s'#13#10'%s',
                            [ PCategorieNode (Node.Data)^.Name,
                              PCategorieNode (Node.Data)^.Description ]);
        try
            repeat
                P := Pos (#13#10,Text);
                if ( P > 0 ) then
                    S := Copy ( Text, 1, P-1 )
                else
                    S := Text;
                TextRect (Rect,S,[tfWordBreak]);
                if ( P > 0 ) then
                begin
                    Rect.Top := Rect.Top + TextHeight (S);
                    Text := Copy ( Text, P + Length (#13#10), Length (Text) );
                end;
            until ( P <= 0 );
        finally
            _FillChar ( Text, Length (Text), $00 );
            _FillChar ( S, Length (S), $00 );
        end;
        { правая колонка - для отображения статуса и пр. }
        Rect := Node.DisplayRect (TRUE);
        Rect.Right := Sender.Width;
        Rect.Left := Rect.Right - 132;
        Brush.Color := TsTreeView (Sender).Color;
        Pen.Color := TsTreeView (Sender).Color;
        FillRect (Rect);
        Rect.Left := Rect.Left + 4;
        if Node.Selected then
        begin
            Brush.Color := clMaroon;
            Pen.Color := clMaroon;
            FillRect (Rect);
        end;
        { текст в правой колонке }
        Rect.Left := Rect.Left + 8;
        Rect.Right := Rect.Right - 8;
        Rect.Top := Rect.Top + 4;
        Text := '';
        if Assigned (Node.Data) then
        begin
            if ( PCategorieNode (Node.Data)^.MsgCount > 0 ) then
                Text := Format ('%s'#13#10'%s'#13#10'сообщений: %d',
                                [ PCategorieNode (Node.Data)^.Author,
                                  _DateToStr (PCategorieNode (Node.Data)^.TimeStampModify),
                                  PCategorieNode (Node.Data)^.MsgCount ])
            else
                Text := Format ('%s'#13#10'%s',
                                [ PCategorieNode (Node.Data)^.Author,
                                  _DateToStr (PCategorieNode (Node.Data)^.TimeStampModify) ])
        end;
        try
            repeat
                P := Pos (#13#10,Text);
                if ( P > 0 ) then
                    S := Copy ( Text, 1, P-1 )
                else
                    S := Text;
                TextRect (Rect,S,[{tfCenter,}tfWordBreak]);
                if ( P > 0 ) then
                begin
                    Rect.Top := Rect.Top + TextHeight (S);
                    Text := Copy ( Text, P + Length (#13#10), Length (Text) );
                end;
            until ( P <= 0 );
        finally
            _FillChar ( Text, Length (Text), $00 );
            _FillChar ( S, Length (S), $00 );
        end;
    end;
end;

procedure TfmMain.TreeChange (Sender: TObject; Node: TTreeNode);
begin
    if Assigned ( PCategorieNode (Node.Data) ) and ( PCategorieNode (Node.Data)^.ID > 0 ) then
    begin
        CurrCtgID := PCategorieNode (Node.Data)^.ID;
        edURL.Text := Format ('crypto://categorie/%s/',[ PCategorieNode (Node.Data)^.KeyHash ]);
    end
    else if Assigned ( PCategorieNode (Node.Data) ) and
            PCategorieNode (Node.Data)^.isMessage and
            Assigned (Node.Parent) and
            Assigned ( PCategorieNode (Node.Parent.Data) ) and
            ( PCategorieNode (Node.Parent.Data)^.ID > 0 ) then
    begin
         CurrCtgID := PCategorieNode (Node.Parent.Data)^.ID;
         edURL.Text := Format ('crypto://categorie/%s#%s/',
                               [ PCategorieNode (Node.Parent.Data)^.KeyHash,
                                 PCategorieNode (Node.Data)^.KeyHash ]);
    end;
end;

procedure TfmMain.TreeDblClick (Sender: TObject);
begin
    if actURL.Enabled then
        actURL.Execute;
end;

procedure TfmMain.lstUsersAdvancedCustomDrawItem (Sender: TCustomTreeView;
                                                  Node: TTreeNode;
                                                  State: TCustomDrawState;
                                                  Stage: TCustomDrawStage;
                                                  var PaintImages, DefaultDraw: Boolean);
var
    Rect : TRect;
    Text : String;
    S    : String;
    P    : Integer;
begin
    with Sender.Canvas do
    begin
        Font.Color := clWhite;
        { фон }
        Rect := Node.DisplayRect (TRUE);
        Rect.Right := Sender.Width - 132;
        if Node.Selected then
        begin
            Brush.Color := clMaroon;
            Pen.Color := clMaroon;
            FillRect (Rect);
        end
        else
        begin
            Brush.Color := TsTreeView (Sender).Color;
            Pen.Color := TsTreeView (Sender).Color;
            FillRect (Rect);
        end;
        { текст }
        Rect.Left := Rect.Left + 8;
        Rect.Right := Rect.Right - 8;
        Rect.Top := Rect.Top + 4;
        Text := Node.Text;
        if Assigned (Node.Data) then
            Text := Format ('%s'#13#10'%s',
                            [ PUserNode (Node.Data)^.Login,
                              PUserNode (Node.Data)^.Description ]);
        try
            repeat
                P := Pos (#13#10,Text);
                if ( P > 0 ) then
                    S := Copy ( Text, 1, P-1 )
                else
                    S := Text;
                TextRect (Rect,S,[tfWordBreak]);
                if ( P > 0 ) then
                begin
                    Rect.Top := Rect.Top + TextHeight (S);
                    Text := Copy ( Text, P + Length (#13#10), Length (Text) );
                end;
            until ( P <= 0 );
        finally
            _FillChar ( Text, Length (Text), $00 );
            _FillChar ( S, Length (S), $00 );
        end;
        { правая колонка - для отображения статуса и пр. }
        Rect := Node.DisplayRect (TRUE);
        Rect.Right := Sender.Width;
        Rect.Left := Rect.Right - 132;
        Brush.Color := TsTreeView (Sender).Color;
        Pen.Color := TsTreeView (Sender).Color;
        FillRect (Rect);
        Rect.Left := Rect.Left + 4;
        if Node.Selected then
        begin
            Brush.Color := clMaroon;
            Pen.Color := clMaroon;
            FillRect (Rect);
        end;
        { текст в правой колонке }
        Rect.Left := Rect.Left + 8;
        Rect.Right := Rect.Right - 8;
        Rect.Top := Rect.Top + 4;
        Text := '';
        if Assigned (Node.Data) then
            Text := Format ('%s',
                            [ _DateToStr (PUserNode (Node.Data)^.TimeStampCreate) ]);
        try
            repeat
                P := Pos (#13#10,Text);
                if ( P > 0 ) then
                    S := Copy ( Text, 1, P-1 )
                else
                    S := Text;
                TextRect (Rect,S,[tfCenter,tfWordBreak]);
                if ( P > 0 ) then
                begin
                    Rect.Top := Rect.Top + TextHeight (S);
                    Text := Copy ( Text, P + Length (#13#10), Length (Text) );
                end;
            until ( P <= 0 );
        finally
            _FillChar ( Text, Length (Text), $00 );
            _FillChar ( S, Length (S), $00 );
        end;
    end;
end;

procedure TfmMain.lstUsersDblClick (Sender: TObject);
begin
    if actEditUser.Enabled then
        actEditUser.Execute;
end;

procedure TfmMain.lstUsersChange (Sender: TObject; Node: TTreeNode);
begin
    if Assigned (Node.Data) and ( PUserNode (Node.Data)^.ID > 0 ) then
    begin
        CurrUsrID := PUserNode (Node.Data)^.ID;
        //edURL.Text := Format ('crypto://user/%s',[ PUserNode (Node.Data)^.KeyHash ])
    end;
end;

procedure TfmMain.lstMailAdvancedCustomDrawItem (Sender: TCustomListView;
                                                 Item: TListItem;
                                                 State: TCustomDrawState;
                                                 Stage: TCustomDrawStage;
                                                 var DefaultDraw: Boolean);
var
    Rect   : TRect;
    Text   : String;
    TxtFmt : TTextFormat;
    S      : String;
    P      : Integer;
    I      : Integer;
    J      : Integer;
begin
    with TsListView (Sender), Sender.Canvas do
    begin
        Font.Color := clWhite;
        { фон }
        Rect := Item.DisplayRect (drBounds);
        if Item.Selected then
        begin
            Brush.Color := clMaroon;
            Pen.Color := clMaroon;
            FillRect (Rect);
        end
        else
        begin
            Brush.Color := TsListView (Sender).Color;
            Pen.Color := TsListView (Sender).Color;
            FillRect (Rect);
        end;
        { картинка }
        if Assigned (SmallImages) then
        begin
            if Item.Selected and ( Item.StateIndex >= 0 ) then
                SmallImages.Draw (Sender.Canvas,
                                  Rect.Left+2,
                                  Rect.Top,
                                  Item.StateIndex,
                                  dsTransparent,
                                  itImage)
            else
                SmallImages.Draw (Sender.Canvas,
                                  Rect.Left+2,
                                  Rect.Top,
                                  Item.ImageIndex,
                                  dsTransparent,
                                  itImage);
        end;
        { текст }
        for I := 0 to Columns.Count - 1 do
        begin
            Rect := Item.DisplayRect (drBounds);
            if ( I = 0 ) and Assigned (SmallImages) then
                Rect.Left := Rect.Left + SmallImages.Width + 4;
            Rect.Top := Rect.Top + 4;
            for J := 0 to I-1 do
                Rect.Left := Rect.Left + Columns [J].Width;
            Rect.Right := Rect.Left + Columns [I].Width;
            Rect.Left := Rect.Left + 4;
            Rect.Right := Rect.Right - 4;

            if ( I = 0 ) then
                Text := Item.Caption
            else
                Text := Item.SubItems [I-1];
            try
                repeat
                    P := Pos (#13#10,Text);
                    if ( P > 0 ) then
                        S := Copy ( Text, 1, P-1 )
                    else
                        S := Text;

                    TxtFmt := [tfWordBreak];
                    if ( Columns [I].Alignment = taCenter ) then
                        TxtFmt := TxtFmt + [tfCenter]
                    else if ( Columns [I].Alignment = taLeftJustify ) then
                        TxtFmt := TxtFmt + [tfLeft]
                    else if ( Columns [I].Alignment = taRightJustify ) then
                        TxtFmt := TxtFmt + [tfRight];

                    TextRect (Rect,S,TxtFmt);

                    if ( P > 0 ) then
                    begin
                        Rect.Top := Rect.Top + TextHeight (S);
                        Text := Copy ( Text, P + Length (#13#10), Length (Text) );
                    end;
                until ( P <= 0 );
            finally
                _FillChar ( Text, Length (Text), $00 );
                _FillChar ( S, Length (S), $00 );
            end;
        end;
    end;
end;

procedure TfmMain.btEncryptClick (Sender: TObject);
var
    UsrID   : TID;
    Contact : TUser;
    Msg     : String;
    Cipher  : String;
begin
    if lstUsers.Visible and
       Assigned (lstUsers.Selected) and
       Assigned (lstUsers.Selected.Data) then
    begin
        UsrID := PUserNode (lstUsers.Selected.Data)^.ID;
        Contact := TUser.Load (DB,UsrID) as TUser;
        try
            Msg := Trim (edCryptoMessage.Text);
            Cipher := Contact.Crypto.Encrypt (Msg,Contact.PublicKey,User.PrivateKey);
            edCryptoMessage.Clear;
            edCryptoMessage.Text := Cipher;
        finally
            _FillChar ( Msg, Length (Msg), $00 );
            _FillChar ( Cipher, Length (Cipher), $00 );
            FreeAndNil (Contact);
        end;
    end;
end;

procedure TfmMain.btDecryptClick (Sender: TObject);
var
    UsrID   : TID;
    Contact : TUser;
    Cipher  : String;
    Msg     : String;
begin
    if lstUsers.Visible and
       Assigned (lstUsers.Selected) and
       Assigned (lstUsers.Selected.Data) then
    begin
        UsrID := PUserNode (lstUsers.Selected.Data)^.ID;
        Contact := TUser.Load (DB,UsrID) as TUser;
        try
            Cipher := Trim (edCryptoMessage.Text);
            Msg := User.Crypto.Decrypt (Cipher,Contact.PublicKey,User.PrivateKey);
            edCryptoMessage.Clear;
            edCryptoMessage.Text := Msg;
        finally
            _FillChar ( Msg, Length (Msg), $00 );
            _FillChar ( Cipher, Length (Cipher), $00 );
            FreeAndNil (Contact);
        end;
    end;
end;

procedure TfmMain.UserPicAfterDialog (Sender: TObject; var Name: string; var Action: Boolean);
begin
    try
        ValidateFileSize (Name,24);
        imgUserPic.Picture.Bitmap.FreeImage;
        imgUserPic.Picture.LoadFromFile (Name);
        imgUserPic.Refresh;
    except
        UserPic.Clear;
        raise;
    end;
end;

procedure TfmMain.btRandomTestClick (Sender: TObject);
begin
    RandomTest (UserRandom.Text);
end;

procedure TfmMain.btAsymmetricTestClick (Sender: TObject);
begin
    AsymmetricTest (UserAsymmetric.Text);
end;

procedure TfmMain.btSymmetricTestClick (Sender: TObject);
begin
    SymmetricTest (UserSymmetric.Text,UserMode.Text);
end;

procedure TfmMain.btHashTestClick (Sender: TObject);
begin
    HashTest (UserHash.Text);
end;

procedure TfmMain.cbxUserUseProxyClick (Sender: TObject);
begin
    pnlProxy.Visible := cbxUserUseProxy.Checked;
end;

end.
