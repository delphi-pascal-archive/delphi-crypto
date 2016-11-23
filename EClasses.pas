unit EClasses;
{******************************************************************************}
{*  Unit to work with Exceptions in the Classes                               *}
{*  Class declaration, the owner of an exception                              *}
{*  must be declared with the directive $M+                                   *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2010                                             *}
{******************************************************************************}
interface

{$I 'std.inc'}

uses
    Windows, SysUtils, Classes, TypInfo,
    VarRecs;

type
{$M+}
    EClass = class (Exception)
    private
        f_EGUID: String; { уникальный идентификатор исключения }
    protected
        procedure SetEGUID (const aValue: String);
    public
        constructor Create (anArgs: array of const;
                            const anEGUID: String = ''); overload;
        constructor Create (anArgs: array of const;
                            anEGUID: array of const); overload;
        property EGUID: String read f_EGUID write SetEGUID;
    end;
{$M-}

function toExceptionMessage (const aValue: TVarRec) : String;

function RaiseErrorInClass (doRaise: Boolean;
                            anArgs: array of const;
                            const anEGUID: String = '') : String;

implementation

function toExceptionMessage (const aValue: TVarRec) : String;
begin
    Result := '';
    with aValue do
    try
        case VType of
            vtObject : if Assigned ( TObject (VObject) ) and
                          VObject.InheritsFrom (Exception) then
                           Result := Exception (VObject).Message
                       else if Assigned ( TObject (VObject) ) then
                           Result := VObject.ClassName;
            else       Result := toString (aValue);
        end;
    except
        Result := 'Unknown Error';
    end;
end;

function RaiseErrorInClass (doRaise: Boolean;
                            anArgs: array of const;
                            const anEGUID: String = '') : String;
var
    I : Integer;

    { функция определения исполняемого модуля приложения }
    function GetClassPackageName (aClass: TClass) : String;
    var
        M : TMemoryBasicInformation;
    begin
        { определяем хэндл DLL, которая владеет классом }
        VirtualQuery ( aClass, M, SizeOf (M) );
        SetLength (Result,MAX_PATH+1);
        { если это не главная программа }
        if ( hModule (M.AllocationBase) <> hInstance ) then
        begin
            GetModuleFileName ( hModule (M.AllocationBase), PChar (Result), MAX_PATH );
            SetLength (  Result, StrLen ( PChar (Result) )  );
            Result := ExtractFileName (Result);
        end
        else
            Result := ExtractFileName ( ParamStr (0) );
    end;
    
    { функция определения внутреннего модуля }
    function GetClassUnitName (aClass: TClass) : String;
    var
        C : Pointer;
    begin
        Result := 'Unknown';
        C := aClass.ClassInfo; 
        if Assigned (C) then
            Result := GetTypeData (C).UnitName;
    end;

begin
    Result := '';
    for I := Low (anArgs) to High (anArgs) do
    with anArgs [I] do
    begin
        { первый параметр - класс, в котором возникло исключение }
        if ( I = 0 ) then
        begin
            case VType of
                vtClass  : Result := Format( '%s::%s::%s',[ GetClassPackageName (VClass),
                                                            GetClassUnitName (VClass),
                                                            VClass.ClassName ] );
                vtObject : if VObject.InheritsFrom (Exception) then
                               Result := Exception (VObject).Message
                           else
                               Result := Format( '%s::%s::%s',[ GetClassPackageName (VObject.ClassType),
                                                                GetClassUnitName (VObject.ClassType),
                                                                VObject.ClassName ] );
                else       Result := toExceptionMessage (anArgs [I]);
            end;
        end
        { второй параметр - имя метода класса, в котором возникло исключение }
        else if ( I = 1 ) then
        begin
            case VType of
                vtChar       : Result := Format ('%s.%s',[Result,VChar]);
                vtString     : Result := Format ('%s.%s',[Result,VString^]);
                vtPChar      : Result := Format ( '%s.%s',[ Result, StrPas (VPChar) ] );
                vtAnsiString : Result := Format ( '%s.%s',[ Result, String (VAnsiString) ] );
                vtWideChar   : Result := Format ( '%s.%s',[ Result, Char (VWideChar) ] );
                vtPWideChar  : Result := Format ( '%s.%s',[ Result, WideCharToString (VPWideChar) ] );
                vtWideString : Result := Format ( '%s.%s',[ Result, WideCharToString (VWideString) ] );
                vtVariant    : Result := Format ('%s.%s',[Result,VVariant^]);
                else           Result := Format ( '%s : '#13#10'%s',[ Result, toExceptionMessage (anArgs [I]) ] );
            end;
        end
        { остальные параметры - текстовые сообщения или экземпляры класса исключения }
        else
            Result := Format ( '%s : '#13#10'%s',[ Result, toExceptionMessage (anArgs [I]) ] );
    end;
    { уникальный идентификатор исключения }
    if ( anEGUID <> '' ) then
        Result := Format ('%s'#13#10'%s',[anEGUID,Result]);
    if doRaise then
        raise Exception.Create (Result);
end;

procedure EClass.SetEGUID (const aValue: String);
begin
    {$IFDEF HEX_UPPER_CASE}
    f_EGUID := UpperCase (aValue);
    {$ELSE}
    f_EGUID := LowerCase (aValue);
    {$ENDIF HEX_UPPER_CASE}
end;

constructor EClass.Create (anArgs: array of const;
                           const anEGUID: String = '');
begin
    EGUID := anEGUID;
    inherited Create ( RaiseErrorInClass (FALSE,anArgs,anEGUID) );
end;

constructor EClass.Create (anArgs: array of const;
                           anEGUID: array of const);
var
    I : Integer;
begin
    EGUID := '';
    for I := Low (anEGUID) to High (anEGUID) do
        EGUID := Format ('%s%s',[ EGUID, toString (anEGUID [I]) ]);
    inherited Create ( RaiseErrorInClass (FALSE,anArgs,EGUID) );
end;


end.
