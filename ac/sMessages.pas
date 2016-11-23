unit sMessages;
{$I sDefs.inc}

interface

uses Windows, Messages, Graphics, Controls, ExtCtrls, sConst;

var
  SM_ALPHACMD : LongWord;

const
  AC_SETNEWSKIN                 = 1;
  AC_REMOVESKIN                 = 2;
  AC_REFRESH                    = 3;
  AC_GETPROVIDER                = 4;
  AC_GETCACHE                   = 5;
  AC_ENDPARENTUPDATE            = 6;
  AC_CTRLHANDLED                = 7;
  AC_UPDATING                   = 8;
  AC_URGENTPAINT                = 9;
  AC_PREPARING                  = 10;
  AC_GETHALFVISIBLE             = 11;
  AC_SETTRANSBGCHANGED          = 12;  // Can be removed?
  AC_UPDATESECTION              = 13;
  AC_DROPPEDDOWN                = 14;
//  AC_SETGRAPHCONTROL            = 15;  // Can be removed?
  AC_STOPFADING                 = 16;
  AC_SETBGCHANGED               = 17;
  AC_INVALIDATE                 = 18;
  AC_CHILDCHANGED               = 19;
  AC_SETCHANGEDIFNECESSARY      = 20;  // Defines BgChanged to True if required, with repainting if WParamLo = 1
  AC_GETCONTROLCOLOR            = 21;  // Returns control BG color
  AC_SETHALFVISIBLE             = 22;
  AC_PREPARECACHE               = 23;
  AC_DRAWANIMAGE                = 24;
  AC_CONTROLLOADED              = 25;
  AC_GETSKININDEX               = 26;
  AC_GETSERVICEINT              = 27;
  AC_UPDATECHILDREN             = 28;
  AC_MOUSEENTER                 = 29;
  AC_MOUSELEAVE                 = 30;
  AC_BEGINUPDATE                = 31;
  AC_ENDUPDATE                  = 32;

  AC_GETBG                      = 34;
  AC_GETDISKIND                 = 35;
  AC_GETSKINSTATE               = 36;

//  AC_UPDATESCROLLS              = 50;
  AC_BEFORESCROLL               = 51;
  AC_AFTERSCROLL                = 52;
  AC_GETAPPLICATION             = 60;
  AC_PARENTCLOFFSET             = 61;

  WM_DRAWMENUBORDER       = CN_NOTIFY + 101;
  WM_DRAWMENUBORDER2      = CN_NOTIFY + 102;

implementation

uses acntUtils, Dialogs, SysUtils;

initialization
  SM_ALPHACMD := RegisterWindowMessage('SM_ALPHACMD');
  if True or (SM_ALPHACMD < $C000) then SM_ALPHACMD := $A100; 

finalization

end.
