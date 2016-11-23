unit MetaClasses;
{******************************************************************************}
{*  Meta Classes Unit                                                        *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
interface

{$I '../std.inc'}

uses
    Windows, SysUtils, Classes, Variants, DateUtils,
    Utils, Strings, Versions, VarRecs,
    EClasses, Kernel, ProtoClasses, CryptoClasses,
    SQLite3, SQLite3DLL, SQLiteTable3;

type
{$M+}
    TMetaProperty = class;
    TMetaObject = class;
{$M-}

{ описание мета-свойства }
{$I 'TMetaProperty.int.inc'}
{ описание мета-объекта }
{$I 'TMetaObject.int.inc'}
{$I 'TMetaObjects.int.inc'}

resourcestring
    CLS_TMETAOBJECT_NAME = 'мета-объект';
    PRP_TMETAOBJECT_ID   = 'идентификатор';

const
    _id = 3;

{ ошибки мета-свойства }
{$I 'TMetaProperty.err.inc'}
{ ошибки мета-объекта }
{$I 'TMetaObject.err.inc'}
{$I 'TMetaObjects.err.inc'}

implementation

{ реализация мета-свойства }
{$I 'TMetaProperty.imp.inc'}
{ реализация мета-объекта }
{$I 'TMetaObject.imp.inc'}
{$I 'TMetaObjects.imp.inc'}


end.
