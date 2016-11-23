unit ParaClasses;
{******************************************************************************}
{*  Para Classes Unit                                                         *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
interface

{$I '../std.inc'}

uses
    Windows, SysUtils, Classes, Variants, DateUtils,
    Utils, Strings, Versions, VarRecs,
    EClasses, Kernel, ProtoClasses, CryptoClasses, MetaClasses, 
    SQLite3, SQLite3DLL, SQLiteTable3;

type
{$M+}
    TParaObject = class;
    TParaObjects = class;
{$M-}

{ описание пара-объекта }
{$I 'TParaObject.int.inc'}
{$I 'TParaObjects.int.inc'}

resourcestring
    CLS_TPARAOBJECT_NAME        = 'пара-объект';
    PRP_TPARAOBJECT_ID_EXTERNAL = 'пара-идентификатор';

const
    _id_external = _id + 1;

{ ошибки пара-объекта }
{$I 'TParaObject.err.inc'}
{$I 'TParaObjects.err.inc'}

implementation

{ реализация пара-объекта }
{$I 'TParaObject.imp.inc'}
{$I 'TParaObjects.imp.inc'}


end.