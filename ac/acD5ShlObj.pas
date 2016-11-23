unit acD5ShlObj;
{$I sDefs.inc}

interface

uses Classes, windows, ShlObj;

const
  SID_IShellFolder2      = '{93F2F68C-1D1B-11D3-A30E-00C04F79ABD1}';
  SID_IEnumExtraSearch   = '{0E700BE1-9DB6-11D1-A1CE-00C04FD75D13}';
  SID_IShellDetails      = '{000214EC-0000-0000-C000-000000000046}';

  IID_IShellDetails: TGUID = (D1:$000214EC; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  IID_IShellFolder2: TGUID = (D1:$93F2F68C; D2:$1D1B; D3:$11D3; D4:($A3,$0E,$00,$C0,$4F,$79,$AB,$D1));

type

  _SHELLDETAILS = record
    fmt,
    cxChar: Integer;
    str: STRRET;
  end;
  TShellDetails = _SHELLDETAILS;

  {$EXTERNALSYM PShColumnID}
  PShColumnID = ^TShColumnID;
  {$EXTERNALSYM SHCOLUMNID}
  SHCOLUMNID = record
    fmtid: TGUID;
    pid: DWORD;
  end;
  {$EXTERNALSYM TShColumnID}
  TShColumnID = SHCOLUMNID;

  {$EXTERNALSYM PExtraSearch}
  PExtraSearch = ^TExtraSearch;
  {$EXTERNALSYM tagExtraSearch}
  tagExtraSearch = record
    guidSearch: TGUID;
    wszFriendlyName,
    wszMenuText: array[0..79] of WideChar;
    wszHelpText: array[0..MAX_PATH] of WideChar;
    wszUrl: array[0..2047] of WideChar;
    wszIcon,
    wszGreyIcon,
    wszClrIcon: array[0..MAX_PATH+10] of WideChar;
  end;
  {$EXTERNALSYM TExtraSearch}
  TExtraSearch = tagExtraSearch;

  {$EXTERNALSYM IEnumExtraSearch}
  IEnumExtraSearch = interface(IUnknown)
    [SID_IEnumExtraSearch]
    function Next(celt: ULONG; out rgelt: PExtraSearch;
      out pceltFetched: ULONG): HResult; stdcall;
    function Skip(celt: ULONG): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppEnum: IEnumExtraSearch): HResult; stdcall;
  end;

  {$EXTERNALSYM IShellFolder2}
  IShellFolder2 = interface(IShellFolder)
    [SID_IShellFolder2]
    function GetDefaultSearchGUID(out pguid: TGUID): HResult; stdcall;
    function EnumSearches(out ppEnum: IEnumExtraSearch): HResult; stdcall;
    function GetDefaultColumn(dwRes: DWORD; var pSort: ULONG;
      var pDisplay: ULONG): HResult; stdcall;
    function GetDefaultColumnState(iColumn: UINT; var pcsFlags: DWORD): HResult; stdcall;
    function GetDetailsEx(pidl: PItemIDList; const pscid: SHCOLUMNID;
      pv: POleVariant): HResult; stdcall;
    function GetDetailsOf(pidl: PItemIDList; iColumn: UINT;
      var psd: TShellDetails): HResult; stdcall;
    function MapNameToSCID(pwszName: LPCWSTR; var pscid: TShColumnID): HResult; stdcall;
  end;

implementation

end.
