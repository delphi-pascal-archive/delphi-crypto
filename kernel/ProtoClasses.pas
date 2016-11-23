unit ProtoClasses;
{******************************************************************************}
{*  Proto Classes Unit                                                        *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
interface

{$I '../std.inc'}

uses
    Windows, SysUtils, Classes, Variants, DateUtils,
    Utils, Strings, Versions, VarRecs,
    EClasses, Kernel;

type
{$M+}
    TProtoProperty = class;
    TProtoProperties = class;
    TProtoObject = class;
    TProtoObjects = class;
{$M-}

    CProtoObjects = class of TProtoObjects;

{ описание прото-свойства }
{$I 'TProtoProperty.int.inc'}
{$I 'TProtoProperties.int.inc'}
{ описание прото-объекта }
{$I 'TProtoObject.int.inc'}
{$I 'TProtoObjects.int.inc'}

resourcestring
    CLS_TPROTOOBJECT_NAME          = 'прото-объект';
    PRP_TPROTOOBJECT_OBJECT_NAME   = 'наименование объекта';
    PRP_TPROTOOBJECT_CLASS_NAME    = 'наименование класса';
    PRP_TPROTOOBJECT_CLASS_VERSION = 'версия класса';

const
    _object_name   = 0;
    _class_name    = 1;
    _class_version = 2;

{ ошибки прото-свойства }
{$I 'TProtoProperty.err.inc'}
{$I 'TProtoProperties.err.inc'}
{ ошибки прото-объекта }
{$I 'TProtoObject.err.inc'}
{$I 'TProtoObjects.err.inc'}

implementation

{ реализация прото-свойства }
{$I 'TProtoProperty.imp.inc'}
{$I 'TProtoProperties.imp.inc'}
{ реализация прото-объекта }
{$I 'TProtoObject.imp.inc'}
{$I 'TProtoObjects.imp.inc'}


end.
