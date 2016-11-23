unit CryptoClasses;
{******************************************************************************}
{*  Crypto Classes Unit                                                       *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
interface

{$I '../std.inc'}

uses
    Windows, SysUtils, Classes, Variants, DateUtils,
    Utils, Strings, Versions, VarRecs,
    EClasses, Kernel, ProtoClasses;

type
{$M+}
    TCryptoProperty = class;
    TCryptoObject = class;
    TCryptoObjects = class;
{$M-}

{ описание крипто-свойства }
{$I 'TCryptoProperty.int.inc'}
{ описание крипто-объекта }
{$I 'TCryptoObject.int.inc'}
{$I 'TCryptoObjects.int.inc'}

resourcestring
    CLS_TCRYPTOOBJECT_NAME = 'крипто-объект';

{ ошибки крипто-свойства }
{$I 'TCryptoProperty.err.inc'}
{ ошибки крипто-объекта }
{$I 'TCryptoObject.err.inc'}
{$I 'TCryptoObjects.err.inc'}

implementation

uses
    Crypto, CryptoSystem;

{ реализация крипто-свойства }
{$I 'TCryptoProperty.imp.inc'}
{ реализация крипто-объекта }
{$I 'TCryptoObject.imp.inc'}
{$I 'TCryptoObjects.imp.inc'}


end.
