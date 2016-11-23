unit Kernel;
{******************************************************************************}
{*  Kernel Unit                                                               *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
////////////////////////////////////////////////////////////////////////////////
//      TProtoObject                                                          //
//            |                                                               //
//      TCryptoObject                                                         //
//            |                                                               //
//       TMetaObject -- TParaObject                                           //
//            |              |                                                //
//       THypoObject    THyperObject                                          //
//            |              |                                                //
//           ...            ...                                               //
////////////////////////////////////////////////////////////////////////////////
interface

{$I '../std.inc'}

uses
    Windows, SysUtils, Classes, Variants, DateUtils,
    Utils, Strings, Versions, VarRecs,
    EClasses;

type
{$M+}
    TItems = class;
{$M-}

{$I 'TItems.int.inc'}

type
    TDataType = Integer;

{ proto properties types }
const
    dtUnknown   = 0;
    dtBoolean   = 1;
    dtInteger   = 2;
    dtInt64     = 3;
    dtExtended  = 4;
    dtDouble    = 5;
    dtDateTime  = 6;
    dtDate      = 7;
    dtTime      = 8;
    dtString    = 9;
    dtHex       = 10;
    dtVersion   = 11;
    dtPointer   = 12;
    dtClass     = 13;
    dtObject    = 14;
    dtStream    = 15;

type
    TID = Int64;

{ crypto properties types }
const
    dtCrypto    = 16;
    dtSafe      = 17;

{ meta properties types }
const
    dtUID       = 18;
    dtID        = 19;
    dtText      = 20;

type
    TPropertyModeType = ( prpDefault,
                          prpReadOnly,
                          prpStored );
    TPropertyMode = set of TPropertyModeType;

    TObjectModeType = ( objDefault,
                        objSimple,
                        objCreated,
                        objCreateFailed,
                        objLoaded,
                        objLoadFailed,
                        objSaved,
                        objSaveFailed,
                        objDeleted,
                        objDeleteFailed,
                        objNoLoad,
                        objNoSave,
                        objNoDelete,
                        objImported,
                        objExported );
    TObjectMode = set of TObjectModeType;

{$I 'TItems.err.inc'}

implementation

{$I 'TItems.imp.inc'}


end.