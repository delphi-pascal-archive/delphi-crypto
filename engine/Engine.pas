unit Engine;
{******************************************************************************}
{*  Engine Unit                                                               *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
////////////////////////////////////////////////////////////////////////////////
// TMetaObject --+------------------- THypoObject --------------------- TPic  //
//      |        |                         |                                  //
//      |     TCrypto       +--------------+----------------+                 //
//      |                   |                               |                 //
//      |               TMessage                       TCategorie             //
//      |                                                                     //
//      +------------------------------ TKeyWord                              //
//      |                                                                     //
// TParaObject -----+---------------+---------------+----------------+        //
//      |           |               |               |                |        //
//      |      TMessageType  TMessageStatus  TCategorieType  TCategorieStatus //
//      |                                                                     //
// THyperObject                                                               //
//      |                                                                     //
//    TUser                                                                   //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
interface

{$I '../std.inc'}

uses
    Windows, SysUtils, Classes, Variants, Consts, Forms, ComCtrls,
    Graphics, jpeg, pngimage,
    DateUtils, Utils, Strings, Versions, VarRecs,
    EClasses, Kernel, ProtoClasses, CryptoClasses, MetaClasses, ParaClasses,
    HypoClasses, HyperClasses, CryptoSystem,
    HashTable,
    XMLUtils, XMLParser, XMLDOM,
    SQLite3, SQLite3DLL, SQLiteTable3;

type
{$M+}
    TPic = class;
    TPics = class;
    TUser = class;
    TUsers = class;
    TMessageType = class;
    TMessageStatus = class;
    TMessage = class;
    TMessages = class;
    TCategorieType = class;
    TCategorieStatus = class; 
    TCategorie = class;
    TCategories = class;
    TKeyWord = class;
    TKeyWords = class;
    TPackageType = class;
    TPackageStatus = class;
    TPackage = class;
    TPackages = class;
{$M-}

{ описание иконки }
{$I 'TPic.int.inc'}
{$I 'TPics.int.inc'}
{ описание объекта пользователя }
{$I 'TUser.int.inc'}
{$I 'TUsers.int.inc'}
{ описание объекта типа сообщения }
{$I 'TMessageType.int.inc'}
{ описание объекта статуса сообщения }
{$I 'TMessageStatus.int.inc'}
{ описание объекта сообщения }
{$I 'TMessage.int.inc'}
{$I 'TMessages.int.inc'}
{ описание объекта типа категории }
{$I 'TCategorieType.int.inc'}
{ описание объекта статуса категории }
{$I 'TCategorieStatus.int.inc'}
{ описание объекта категории }
{$I 'TCategorie.int.inc'}
{$I 'TCategories.int.inc'}
{ описание объекта ключевого слова }
{$I 'TKeyWord.int.inc'}
{$I 'TKeyWords.int.inc'}
{ описание объекта типа пакета }
{$I 'TPackageType.int.inc'}
{ описание объекта статуса пакета }
{$I 'TPackageStatus.int.inc'}
{ описание объекта пакета }
{$I 'TPackage.int.inc'}
{$I 'TPackages.int.inc'}
{ описание списка пользователей }
{$I 'UsersList.int.inc'}
{ описание дерева категорий }
{$I 'CategoriesTree.int.inc'}
{ описание списка сообщений }
{$I 'MessagesList.int.inc'}

resourcestring
    CLS_TPIC_NAME     = 'иконка';
    PRP_TPIC_ID_OWNER = 'идентификатор владельца';
    PRP_TPIC_FORMAT   = 'формат данных';
    PRP_TPIC_DATA     = 'потоковые данные';

const
    _pic_id                = _id;
    _pic_time_stamp_create = _hypo_time_stamp_create;
    _pic_time_stamp_modify = _hypo_time_stamp_modify;
    _pic_time_stamp_public = _hypo_time_stamp_public;
    _pic_time_stamp_vector = _hypo_time_stamp_vector;
    _pic_version           = _hypo_version;
    _pic_key_hash          = _hypo_key_hash;
    _pic_data_hash         = _hypo_data_hash;
    _pic_id_owner          = _id + 8;
    _pic_format            = _id + 9;
    _pic_data              = _id + 10;

resourcestring
    CLS_TUSER_NAME           = 'пользователь';
    PRP_TUSER_LOGIN          = 'логин';
    PRP_TUSER_ID_OWNER       = 'идентификатор владельца';
    PRP_TUSER_PASSWORD       = 'пароль';
    PRP_TUSER_SALT           = 'соль';
    PRP_TUSER_ID_CRYPTO      = 'идентификатор крипто-системы';
    PRP_TUSER_EMAIL          = 'электронная почта';
    PRP_TUSER_EMAIL_PASSWORD = 'пароль к электронной почте';
    PRP_TUSER_IP             = 'ip-адрес';
    PRP_TUSER_PORT           = 'порт';
    PRP_TUSER_DESCRIPTION    = 'описание';
    PRP_TUSER_SEX            = 'пол';
    PRP_TUSER_BIRTHDAY       = 'дата рождения';
    PRP_TUSER_HASH_PIC       = 'хэш-ключ иконки';
    PRP_TUSER_ID_PIC         = 'идентификатор иконки';
    PRP_TUSER_PUBLIC_KEY     = 'публичный ключ';
    PRP_TUSER_PRIVATE_KEY    = 'приватный ключ';
    PRP_TUSER_TIMEOUT        = 'таймаут соединения';
    PRP_TUSER_USE_PROXY      = 'использование proxy-сервера';
    PRP_TUSER_PROXY_IP       = 'ip-адрес proxy-сервера';
    PRP_TUSER_PROXY_PORT     = 'порт proxy-сервера';
    PRP_TUSER_PROXY_LOGIN    = 'логин proxy-сервера';
    PRP_TUSER_PROXY_PASSWORD = 'пароль proxy-сервера';
    PRP_TUSER_PROXY_PROTOCOL = 'протокол proxy-сервера';
    PRP_TUSER_SMTP_HOST      = 'smtp-сервер';
    PRP_TUSER_SMTP_PORT      = 'порт smtp-сервера';
    PRP_TUSER_POP3_HOST      = 'pop3-сервер';
    PRP_TUSER_POP3_PORT      = 'порт pop3-сервера';
    PRP_TUSER_AUTO_TLS       = 'auto tls';
    PRP_TUSER_FULL_SSL       = 'full ssl';

const
    _usr_id                = _id;
    _usr_login             = _id_external;
    _usr_time_stamp_create = _hyper_time_stamp_create;
    _usr_time_stamp_modify = _hyper_time_stamp_modify;
    _usr_time_stamp_public = _hyper_time_stamp_public;
    _usr_time_stamp_vector = _hyper_time_stamp_vector;
    _usr_version           = _hyper_version;
    _usr_key_hash          = _hyper_key_hash;
    _usr_data_hash         = _hyper_data_hash;
    _usr_id_owner          = _id_external + 8;
    _usr_password          = _id_external + 9;
    _usr_salt              = _id_external + 10;
    _usr_id_crypto         = _id_external + 11;
    _usr_email             = _id_external + 12;
    _usr_email_password    = _id_external + 13;
    _usr_ip                = _id_external + 14;
    _usr_port              = _id_external + 15;
    _usr_description       = _id_external + 16;
    _usr_sex               = _id_external + 17;
    _usr_birthday          = _id_external + 18;
    _usr_hash_pic          = _id_external + 19;
    _usr_id_pic            = _id_external + 20;
    _usr_public_key        = _id_external + 21;
    _usr_private_key       = _id_external + 22;
    _usr_timeout           = _id_external + 23;
    _usr_use_proxy         = _id_external + 24;
    _usr_proxy_ip          = _id_external + 25;
    _usr_proxy_port        = _id_external + 26;
    _usr_proxy_login       = _id_external + 27;
    _usr_proxy_password    = _id_external + 28;
    _usr_proxy_protocol    = _id_external + 29;
    _usr_smtp_host         = _id_external + 30;
    _usr_smtp_port         = _id_external + 31;
    _usr_pop3_host         = _id_external + 32;
    _usr_pop3_port         = _id_external + 33;
    _usr_auto_tls          = _id_external + 34;
    _usr_full_ssl          = _id_external + 35;

resourcestring
    CLS_TMESSAGETYPE_NAME        = 'тип сообщения';
    PRP_TMESSAGETYPE_NAME        = 'наименование';
    PRP_TMESSAGETYPE_DESCRIPTION = 'описание';

const
    _msg_type_id          = _id;
    _msg_type_name        = _id_external;
    _msg_type_description = _id_external + 1;

resourcestring
    CLS_TMESSAGESTATUS_NAME        = 'статус сообщения';
    PRP_TMESSAGESTATUS_NAME        = 'наименование';
    PRP_TMESSAGESTATUS_DESCRIPTION = 'описание';

const
    _msg_status_id          = _id;
    _msg_status_name        = _id_external;
    _msg_status_description = _id_external + 1;

resourcestring
    CLS_TMESSAGE_NAME           = 'сообщение';
    PRP_TMESSAGE_HASH_AUTHOR    = 'хэш-ключ автора';
    PRP_TMESSAGE_ID_AUTHOR      = 'идентификатор автора';
    PRP_TMESSAGE_ID_OWNER       = 'идентификатор владельца';
    PRP_TMESSAGE_HASH_CATEGORIE = 'хэш-ключ категории-владельца';
    PRP_TMESSAGE_ID_CATEGORIE   = 'идентификатор категории-владельца';
    PRP_TMESSAGE_ID_TYPE        = 'идентификатор типа';
    PRP_TMESSAGE_ID_STATUS      = 'идентификатор статуса';
    PRP_TMESSAGE_SUBJECT        = 'тема';
    PRP_TMESSAGE_TEXT           = 'текст';
    PRP_TMESSAGE_HASH_PIC       = 'хэш-ключ иконки';
    PRP_TMESSAGE_ID_PIC         = 'идентификатор иконки';
    PRP_TMESSAGE_INDEX_PIC      = 'индекс стандартной иконки';
    PRP_TMESSAGE_SALT           = 'соль';

const
    _msg_id                = _id;
    _msg_time_stamp_create = _hypo_time_stamp_create;
    _msg_time_stamp_modify = _hypo_time_stamp_modify;
    _msg_time_stamp_public = _hypo_time_stamp_public;
    _msg_time_stamp_vector = _hypo_time_stamp_vector;
    _msg_version           = _hypo_version;
    _msg_key_hash          = _hypo_key_hash;
    _msg_data_hash         = _hypo_data_hash;
    _msg_hash_categorie    = _id + 8;
    _msg_id_categorie      = _id + 9;
    _msg_hash_author       = _id + 10;
    _msg_id_author         = _id + 11;
    _msg_id_owner          = _id + 12;
    _msg_id_type           = _id + 13;
    _msg_id_status         = _id + 14;
    _msg_subject           = _id + 15;
    _msg_text              = _id + 16;
    _msg_hash_pic          = _id + 17;
    _msg_id_pic            = _id + 18;
    _msg_index_pic         = _id + 19;
    _msg_salt              = _id + 20;

resourcestring
    CLS_TCATEGORIETYPE_NAME        = 'тип категории';
    PRP_TCATEGORIETYPE_NAME        = 'наименование';
    PRP_TCATEGORIETYPE_DESCRIPTION = 'описание';

const
    _ctg_type_id          = _id;
    _ctg_type_name        = _id_external;
    _ctg_type_description = _id_external + 1;

resourcestring
    CLS_TCATEGORIESTATUS_NAME        = 'статус категории';
    PRP_TCATEGORIESTATUS_NAME        = 'наименование';
    PRP_TCATEGORIESTATUS_DESCRIPTION = 'описание';

const
    _ctg_status_id          = _id;
    _ctg_status_name        = _id_external;
    _ctg_status_description = _id_external + 1;

resourcestring
    CLS_TCATEGORIE_NAME        = 'категория';
    PRP_TCATEGORIE_HASH_AUTHOR = 'хэш-ключ автора';
    PRP_TCATEGORIE_ID_AUTHOR   = 'идентификатор автора';
    PRP_TCATEGORIE_ID_OWNER    = 'идентификатор владельца';
    PRP_TCATEGORIE_HASH_PARENT = 'хэш-ключ предка';
    PRP_TCATEGORIE_ID_PARENT   = 'идентификатор предка';
    PRP_TCATEGORIE_ID_TYPE     = 'идентификатор типа';
    PRP_TCATEGORIE_ID_STATUS   = 'идентификатор статуса';
    PRP_TCATEGORIE_NAME        = 'название';
    PRP_TCATEGORIE_DESCRIPTION = 'описание';
    PRP_TCATEGORIE_HASH_PIC    = 'хэш-ключ иконки';
    PRP_TCATEGORIE_ID_PIC      = 'идентификатор иконки';
    PRP_TCATEGORIE_INDEX_PIC   = 'индекс стандартной иконки';
    PRP_TCATEGORIE_SALT        = 'соль';

const
    _ctg_id                = _id;
    _ctg_time_stamp_create = _hypo_time_stamp_create;
    _ctg_time_stamp_modify = _hypo_time_stamp_modify;
    _ctg_time_stamp_public = _hypo_time_stamp_public;
    _ctg_time_stamp_vector = _hypo_time_stamp_vector;
    _ctg_version           = _hypo_version;
    _ctg_key_hash          = _hypo_key_hash;
    _ctg_data_hash         = _hypo_data_hash;
    _ctg_hash_parent       = _id + 8;
    _ctg_id_parent         = _id + 9;
    _ctg_hash_author       = _id + 10;
    _ctg_id_author         = _id + 11;
    _ctg_id_owner          = _id + 12;
    _ctg_id_type           = _id + 13;
    _ctg_id_status         = _id + 14;
    _ctg_name              = _id + 15;
    _ctg_description       = _id + 16;
    _ctg_hash_pic          = _id + 17;
    _ctg_id_pic            = _id + 18;
    _ctg_index_pic         = _id + 19;
    _ctg_salt              = _id + 20;

resourcestring
    CLS_TKEYWORD_NAME           = 'ключевое слово';
    PRP_TKEYWORD_KEY_WORD       = 'ключевое слово';
    PRP_TKEYWORD_HASH           = 'хэш ключевого слова';
    PRP_TKEYWORD_HASH_CATEGORIE = 'хэш-ключ категории';
    PRP_TKEYWORD_ID_CATEGORIE   = 'идентификатор категории';
    PRP_TKEYWORD_HASH_MESSAGE   = 'хэш-ключ сообщения';
    PRP_TKEYWORD_ID_MESSAGE     = 'идентификатор сообщения';
    PRP_TKEYWORD_HASH_AUTHOR    = 'хэш-ключ автора';
    PRP_TKEYWORD_ID_AUTHOR      = 'идентификатор автора';
    PRP_TKEYWORD_ID_OWNER       = 'идентификатор владельца';
    PRP_TKEYWORD_SALT           = 'соль';

const
    _kwd_id             = _id;
    _kwd_key_word       = _id + 1;
    _kwd_hash           = _id + 2;
    _kwd_hash_categorie = _id + 3;
    _kwd_id_categorie   = _id + 4;
    _kwd_hash_message   = _id + 5;
    _kwd_id_message     = _id + 6;
    _kwd_hash_author    = _id + 7;
    _kwd_id_author      = _id + 8;
    _kwd_id_owner       = _id + 9;
    _kwd_salt           = _id + 10;

resourcestring
    CLS_TPACKAGETYPE_NAME        = 'тип пакета';
    PRP_TPACKAGETYPE_NAME        = 'наименование';
    PRP_TPACKAGETYPE_DESCRIPTION = 'описание';

const
    _pck_type_id          = _id;
    _pck_type_name        = _id_external;
    _pck_type_description = _id_external + 1;

resourcestring
    CLS_TPACKAGESTATUS_NAME        = 'статус пакета';
    PRP_TPACKAGESTATUS_NAME        = 'наименование';
    PRP_TPACKAGESTATUS_DESCRIPTION = 'описание';

const
    _pck_status_id          = _id;
    _pck_status_name        = _id_external;
    _pck_status_description = _id_external + 1;

resourcestring
    CLS_TPACKAGE_NAME               = 'пакет';
    PRP_TPACKAGE_ID_SENDER          = 'идентификатор отправителя';
    PRP_TPACKAGE_SENDER_HASH        = 'хэш-ключ отправителя';
    PRP_TPACKAGE_ID_RECEIVER        = 'идентификатор получателя';
    PRP_TPACKAGE_RECEIVER_HASH      = 'хэш-ключ получателя';
    PRP_TPACKAGE_TIME_STAMP_CREATE  = 'дата и время создания пакета';
    PRP_TPACKAGE_TIME_STAMP_SEND    = 'дата и время отправки пакета';
    PRP_TPACKAGE_TIME_STAMP_RECEIVE = 'дата и время получения пакета';
    PRP_TPACKAGE_VERSION            = 'версия';
    PRP_TPACKAGE_KEY_HASH           = 'хэш-ключ';
    PRP_TPACKAGE_DATA_HASH          = 'хэш данных';
    PRP_TPACKAGE_ID_OWNER           = 'идентификатор владельца';
    PRP_TPACKAGE_ID_TYPE            = 'идентификатор типа';
    PRP_TPACKAGE_ID_STATUS          = 'идентификатор статуса';
    PRP_TPACKAGE_META_CLASS_ID      = 'класс мета-данных';
    PRP_TPACKAGE_META_DATA          = 'мета-данные';

const
    _pck_id                 = _id;
    _pck_id_sender          = _id + 1;
    _pck_sender_hash        = _id + 2;
    _pck_id_receiver        = _id + 3;
    _pck_receiver_hash      = _id + 4;
    _pck_time_stamp_create  = _id + 5;
    _pck_time_stamp_send    = _id + 6;
    _pck_time_stamp_receive = _id + 7;
    _pck_version            = _id + 8;
    _pck_key_hash           = _id + 9;
    _pck_data_hash          = _id + 10;
    _pck_id_owner           = _id + 11;
    _pck_id_type            = _id + 12;
    _pck_id_status          = _id + 13;
    _pck_meta_class_id      = _id + 14;
    _pck_meta_data          = _id + 15;

{resourcestring
    CLS_TSESSION_NAME              = 'параметр сессии';
    PRP_TSESSION_ID_OWNER          = 'идентификатор владельца';
    PRP_TSESSION_NAME              = 'наименование параметра сессии';
    PRP_TSESSION_VALUE             = 'значение параметра сессии';
    PRP_TSESSION_TIME_STAMP_START  = 'время начала действия параметра сессии';
    PRP_TSESSION_TIME_STAMP_FINISH = 'время окончания действия параметра сессии';
    PRP_TSESSION_SALT              = 'соль';

const
    _ssn_id                = _id;
    _ssn_id_owner          = _id + 1;
    _ssn_name              = _id + 2;
    _ssn_value             = _id + 3;
    _ssn_time_stamp_start  = _id + 4;
    _ssn_time_stamp_finish = _id + 5;
    _ssn_salt              = _id + 6;}

{ ошибки иконки }
{$I 'TPic.err.inc'}
{$I 'TPics.err.inc'}
{ ошибки пользователя }
{$I 'TUser.err.inc'}
{$I 'TUsers.err.inc'}
{ ошибки объекта типа сообщения }
{$I 'TMessageType.err.inc'}
{ ошибки объекта статуса сообщения }
{$I 'TMessageStatus.err.inc'}
{ ошибки объекта сообщения }
{$I 'TMessage.err.inc'}
{$I 'TMessages.err.inc'}
{ ошибки объекта типа категории }
{$I 'TCategorieType.err.inc'}
{ ошибки объекта статуса категории }
{$I 'TCategorieStatus.err.inc'}
{ ошибки объекта категории }
{$I 'TCategorie.err.inc'}
{$I 'TCategories.err.inc'}
{ ошибки объекта ключевого слова }
{$I 'TKeyWord.err.inc'}
{$I 'TKeyWords.err.inc'}
{ ошибки объекта типа пакета }
{$I 'TPackageType.err.inc'}
{ ошибки объекта статуса пакета }
{$I 'TPackageStatus.err.inc'}
{ ошибки объекта пакета }
{$I 'TPackage.err.inc'}
{$I 'TPackages.err.inc'}
{ ошибки списка пользователей }
{$I 'UsersList.err.inc'}
{ ошибки дерева категорий }
{$I 'CategoriesTree.err.inc'}
{ ошибки списка сообщений }
{$I 'MessagesList.err.inc'}

resourcestring
    ERR_USER_NOT_ASSIGNED = 'Не удалось создать глобальный объект пользователя!';

var
    DATABASE_FILE_NAME      : String = '';  { файл БД }
    USER_ID                 : TID    = 0;   { идентификатор глобального объекта пользователя }
    USER_KEY_HASH           : Hex    = '';
    USER                    : TUser  = NIL; { глобальный объект пользователя }
    ROOT_CATEGORIE_ID       : TID    = 0;   { корневая категория }
    ROOT_CATEGORIE_KEY_HASH : Hex    = '';

const
    CATEGORIE_ROOT_TYPE_ID      = 1; { корень }
    CATEGORIE_FORUM_TYPE_ID     = 2; { раздел }
    CATEGORIE_TOPIC_TYPE_ID     = 3; { тема или под-тема }

    CATEGORIE_OPENED_STATUS_ID  = 1; { открытая тема }
    CATEGORIE_CLOSED_STATUS_ID  = 2; { закрытая тема }
    CATEGORIE_DELETED_STATUS_ID = 3; { удаленная тема }

    MESSAGE_FORUM_TYPE_ID       = 1; { сообщение форума }
    MESSAGE_PUBLIC_TYPE_ID      = 2; { публичное сообщение }
    MESSAGE_PRIVATE_TYPE_ID     = 3; { приватное сообщение }

    MESSAGE_ACTIVE_STATUS_ID    = 1; { активное сообщение }
    MESSAGE_DELETED_STATUS_ID   = 2; { удаленное сообщение }

const
    MAX_MESSAGE_LENGTH = 4096;

const
    PACKAGE_GET_TYPE_ID = 1; { запросить объект }
    PACKAGE_PUT_TYPE_ID = 2; { передать объект }
    PACKAGE_DEL_TYPE_ID = 3; { удалить объект }
    PACKAGE_UPD_TYPE_ID = 4; { отправить обновления }

    PACKAGE_TYPE : array [PACKAGE_GET_TYPE_ID..PACKAGE_UPD_TYPE_ID ] of ShortString = (
        'GET',
        'PUT',
        'DEL',
        'UPD'
    );

function GetPckTypeExternal (const anID: TID) : ShortString;

const
    PACKAGE_CREATED_STATUS_ID  = 1; { пакет создан }
    PACKAGE_SENDED_STATUS_ID   = 2; { пакет отправлен }
    PACKAGE_RECEIVED_STATUS_ID = 3; { пакет принят }
    PACKAGE_REJECTED_STATUS_ID = 4; { пакет отвергнут }
    PACKAGE_EXECUTED_STATUS_ID = 5; { пакет исполнен }

procedure UpdateMailData (const anUser: TUser);
procedure SetMailData (const anObject: TMetaObject);

const
    tbsEmpty  = 0;
    tbsLoaded = 1;

const
    tabForum = 0;
    tabUsers = 1;
    tabUser  = 2;
    tabMail  = 3;

procedure SetTabStatus (const aTabIndex: Integer; const aStatus: Integer);
function GetTabStatus (const aTabIndex: Integer) : Integer;

implementation

uses
    BBCode,
    DllThreads,
    DialogClasses,
    uMain;

{ реализация иконки }
{$I 'TPic.imp.inc'}
{$I 'TPics.imp.inc'}
{ реализация пользователя }
{$I 'TUser.imp.inc'}
{$I 'TUsers.imp.inc'}
{ реализация объекта типа сообщения }
{$I 'TMessageType.imp.inc'}
{ реализация объекта статуса сообщения }
{$I 'TMessageStatus.imp.inc'}
{ реализация объекта сообщения }
{$I 'TMessage.imp.inc'}
{$I 'TMessages.imp.inc'}
{ реализация объекта типа категории }
{$I 'TCategorieType.imp.inc'}
{ реализация объекта статуса категории }
{$I 'TCategorieStatus.imp.inc'}
{ реализация объекта категории }
{$I 'TCategorie.imp.inc'}
{$I 'TCategories.imp.inc'}
{ реализация объекта ключевого слова }
{$I 'TKeyWord.imp.inc'}
{$I 'TKeyWords.imp.inc'}
{ реализация объекта типа пакета }
{$I 'TPackageType.imp.inc'}
{ реализация объекта статуса пакета }
{$I 'TPackageStatus.imp.inc'}
{ реализация объекта пакета }
{$I 'TPackage.imp.inc'}
{$I 'TPackages.imp.inc'}
{ реализация списка пользователей }
{$I 'UsersList.imp.inc'}
{ реализация дерева категорий }
{$I 'CategoriesTree.imp.inc'}
{ реализация списка сообщений }
{$I 'MessagesList.imp.inc'}

var
    DATABASE       : TSQLiteDatabase;
    RootCategories : TCategories;

function GetPckTypeExternal (const anID: TID) : ShortString;
begin
    Result := '';
    if ( anID >= Low (PACKAGE_TYPE) ) and ( anID <= High (PACKAGE_TYPE) ) then
        Result := PACKAGE_TYPE [anID];
end;

procedure UpdateMailData (const anUser: TUser);
begin
    if Assigned (MainForm) then
        MainForm.UpdateMailData (anUser);
end;

procedure SetMailData (const anObject: TMetaObject);
begin
    if Assigned (MainForm) then
        MainForm.SetMailData (anObject);
end;

procedure SetTabStatus (const aTabIndex: Integer; const aStatus: Integer);
begin
    if Assigned (MainForm) and
       ( aTabIndex >= tabForum ) and
       ( aTabIndex <= tabMail ) then
    with MainForm.tabs.Pages [aTabIndex] do
    begin
        Tag := aStatus;
        if ( aTabIndex = tabForum ) then
            case aStatus of
                tbsEmpty  : ImageIndex := 1;
                else        ImageIndex := 0;
            end;
    end;
end;

function GetTabStatus (const aTabIndex: Integer) : Integer;
begin
    Result := -1;
    if Assigned (MainForm) and
       ( aTabIndex >= tabForum ) and
       ( aTabIndex <= tabMail ) then
        Result := MainForm.tabs.Pages [aTabIndex].Tag;
end;

initialization
{ файл БД }
    DATABASE_FILE_NAME := ExtractFilePath (Application.ExeName) + 'database.db';
{ авторизация }
    if not SignIn (DATABASE_FILE_NAME,'') then
    begin
        FreeAndNil (User);
        Application.Terminate;
        Exit;
    end
    else
    begin
        USER_ID := User.ID;
        USER_KEY_HASH := User.KeyHash;
    end;
    // отображаем логин в наименовании приложения
    Application.Title := User.Login;
{ инициализация БД }
    DATABASE := TSQLiteDatabase.Create (DATABASE_FILE_NAME);
    try
        { перезапись типов категорий }
        TCategorieType.Save (DATABASE,[ CATEGORIE_ROOT_TYPE_ID,  'ROOT',  'Root Categorie Type'  ]);
        TCategorieType.Save (DATABASE,[ CATEGORIE_FORUM_TYPE_ID, 'FORUM', 'Forum Categorie Type' ]);
        TCategorieType.Save (DATABASE,[ CATEGORIE_TOPIC_TYPE_ID, 'TOPIC', 'Topic Categorie Type' ]);
        { перезапись статусов категорий }
        TCategorieStatus.Save (DATABASE,[ CATEGORIE_OPENED_STATUS_ID,  'OPENED',  'Opened Categorie Status' ]);
        TCategorieStatus.Save (DATABASE,[ CATEGORIE_CLOSED_STATUS_ID,  'CLOSED',  'Closed Categorie Status' ]);
        TCategorieStatus.Save (DATABASE,[ CATEGORIE_DELETED_STATUS_ID, 'DELETED', 'Deleted Categorie Status' ]);
        { поиск или создание корневой категории }
        ROOT_CATEGORIE_ID := 0;
        RootCategories := TCategories.Load (DATABASE,[ _([]),
                                                       _([0]),
                                                       _([USER_ID]),
                                                       _([USER_ID]),
                                                       _([CATEGORIE_ROOT_TYPE_ID]) ]) as TCategories;
        if Assigned (RootCategories) and ( RootCategories.Count > 0 ) then
        begin
            ROOT_CATEGORIE_ID := RootCategories.ItemAt [0].ID;
            ROOT_CATEGORIE_KEY_HASH := RootCategories.ItemAt [0].KeyHash;
        end
        else
        begin
            with TCategorie.Create (DATABASE,[0,0,USER_ID,USER_ID,CATEGORIE_ROOT_TYPE_ID,CATEGORIE_OPENED_STATUS_ID]) do
            try
                Name := 'ROOT';
                Description := 'Root Categorie';
                Save;
                ROOT_CATEGORIE_ID := ID;
                ROOT_CATEGORIE_KEY_HASH := KeyHash;
                //ShowMessageFmt ('Root Categorie ID = %d',[ROOT_CATEGORIE_ID]);
            finally
                Free;
            end;
        end;
        { перезапись типов сообщений }
        TMessageType.Save (DATABASE,[ MESSAGE_FORUM_TYPE_ID,   'FORUM',   'Forum Message Type'   ]);
        TMessageType.Save (DATABASE,[ MESSAGE_PRIVATE_TYPE_ID, 'PRIVATE', 'Private Message Type' ]);
        TMessageType.Save (DATABASE,[ MESSAGE_PUBLIC_TYPE_ID,  'PUBLIC',  'Public Message Type'  ]);
        { перезапись статусов сообщений }
        TMessageStatus.Save (DATABASE,[ MESSAGE_ACTIVE_STATUS_ID,  'OPENED', 'Active Message Status'  ]);
        TMessageStatus.Save (DATABASE,[ MESSAGE_DELETED_STATUS_ID, 'CLOSED', 'Deleted Message Status' ]);
        { перезапись типов пакетов }
        TPackageType.Save (DATABASE,[ PACKAGE_GET_TYPE_ID, 'GET', 'Get object'       ]);
        TPackageType.Save (DATABASE,[ PACKAGE_PUT_TYPE_ID, 'PUT', 'Put object'       ]);
        TPackageType.Save (DATABASE,[ PACKAGE_DEL_TYPE_ID, 'DEL', 'Delete object'    ]);
        TPackageType.Save (DATABASE,[ PACKAGE_UPD_TYPE_ID, 'UPD', 'Update objects'   ]);
        { перезапись статусов пакетов }
        TPackageStatus.Save (DATABASE,[ PACKAGE_CREATED_STATUS_ID,  'CREATED',  'Created Package Status'  ]);
        TPackageStatus.Save (DATABASE,[ PACKAGE_SENDED_STATUS_ID,   'SENDED',   'Sended Package Status'   ]);
        TPackageStatus.Save (DATABASE,[ PACKAGE_RECEIVED_STATUS_ID, 'RECEIVED', 'Received Package Status' ]);
        TPackageStatus.Save (DATABASE,[ PACKAGE_REJECTED_STATUS_ID, 'REJECTED', 'Rejected Package Status' ]);
        TPackageStatus.Save (DATABASE,[ PACKAGE_EXECUTED_STATUS_ID, 'EXECUTED', 'Executed Package Status' ]);
    finally
        FreeAndNil (RootCategories);
        FreeAndNil (DATABASE);
    end;

finalization
    FreeAndNil (User);


end.
