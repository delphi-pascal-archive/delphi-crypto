unit HyperClasses;
{******************************************************************************}
{*  Hyper Classes Unit                                                        *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit & rat 2011                                       *}
{******************************************************************************}
interface

{$I '../std.inc'}

uses
    Windows, SysUtils, Classes, Variants, DateUtils,
    Utils, Strings, Versions, VarRecs,
    EClasses, Kernel, ProtoClasses, CryptoClasses, MetaClasses, ParaClasses,
    HashTable, 
    SQLite3, SQLite3DLL, SQLiteTable3;

type
{$M+}
    THyperObject = class;
    THyperObjects = class;
{$M-}

{ описание гипер-объекта }
{$I 'THyperObject.int.inc'}
{$I 'THyperObjects.int.inc'}

resourcestring
    CLS_THYPEROBJECT_NAME              = 'гипер-объект';
    PRP_THYPEROBJECT_TIME_STAMP_CREATE = 'дата и время создания';
    PRP_THYPEROBJECT_TIME_STAMP_MODIFY = 'дата и время последнего редактирования';
    PRP_THYPEROBJECT_TIME_STAMP_PUBLIC = 'дата и время публикации';
    PRP_THYPEROBJECT_TIME_STAMP_VECTOR = 'значение векторных часов';
    PRP_THYPEROBJECT_VERSION           = 'версия';
    PRP_THYPEROBJECT_KEY_HASH          = 'хэш-ключ';
    PRP_THYPEROBJECT_DATA_HASH         = 'хэш данных';

const
    _hyper_time_stamp_create = _id_external + 1;
    _hyper_time_stamp_modify = _id_external + 2;
    _hyper_time_stamp_public = _id_external + 3;
    _hyper_time_stamp_vector = _id_external + 4;
    _hyper_version           = _id_external + 5;
    _hyper_key_hash          = _id_external + 6;
    _hyper_data_hash         = _id_external + 7;

{ ошибки гипер-объекта }
{$I 'THyperObject.err.inc'}
{$I 'THyperObjects.err.inc'}

implementation

{ реализация гипер-объекта }
{$I 'THyperObject.imp.inc'}
{$I 'THyperObjects.imp.inc'}


end.