unit HypoClasses;
{******************************************************************************}
{*  Hypo Classes Unit                                                         *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit & rat 2011                                       *}
{******************************************************************************}
interface

{$I '../std.inc'}

uses
    Windows, SysUtils, Classes, Variants, DateUtils,
    Utils, Strings, Versions, VarRecs,
    EClasses, Kernel, ProtoClasses, CryptoClasses, MetaClasses,
    HashTable,
    SQLite3, SQLite3DLL, SQLiteTable3;

type
{$M+}
    THypoObject = class;
    THypoObjects = class;
{$M-}

{ описание гипо-объекта }
{$I 'THypoObject.int.inc'}
{$I 'THypoObjects.int.inc'}

resourcestring
    CLS_THYPOOBJECT_NAME              = 'гипо-объект';
    PRP_THYPOOBJECT_TIME_STAMP_CREATE = 'дата и время создания';
    PRP_THYPOOBJECT_TIME_STAMP_MODIFY = 'дата и время последнего редактирования';
    PRP_THYPOOBJECT_TIME_STAMP_PUBLIC = 'дата и время публикации';
    PRP_THYPOOBJECT_TIME_STAMP_VECTOR = 'значение векторных часов';
    PRP_THYPOOBJECT_VERSION           = 'версия';
    PRP_THYPOOBJECT_KEY_HASH          = 'хэш-ключ';
    PRP_THYPOOBJECT_DATA_HASH         = 'хэш данных';

const
    _hypo_time_stamp_create = _id + 1;
    _hypo_time_stamp_modify = _id + 2;
    _hypo_time_stamp_public = _id + 3;
    _hypo_time_stamp_vector = _id + 4;
    _hypo_version           = _id + 5;
    _hypo_key_hash          = _id + 6;
    _hypo_data_hash         = _id + 7;

{ ошибки гипо-объекта }
{$I 'THypoObject.err.inc'}
{$I 'THypoObjects.err.inc'}

implementation

{ реализация гипо-объекта }
{$I 'THypoObject.imp.inc'}
{$I 'THypoObjects.imp.inc'}


end.