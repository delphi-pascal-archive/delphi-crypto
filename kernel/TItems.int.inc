{******************************************************************************}
{*  TItems.int.inc                                                            *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ универсальный список }
{$M+}
    CItems = class of TItems;
    PItems = ^TItems;
    TItems = class (TPersistent)
    public
        class procedure _raise (anArgs: array of const;
                                const anEGUID: String = ''); overload; virtual;
        class procedure _raise (anArgs: array of const;
                                anEGUID: array of const); overload; virtual;
    private
        f_List: TList; { базовый список }
    protected
        function Check (anItem: Pointer) : Boolean; overload; virtual;
    protected
        function GetCount : Integer; overload; virtual;
    private
        procedure QuickSort (L, R: Integer);
    protected
        class function Compare (anItemA, anItemB: Pointer) : Integer; overload; virtual;
    protected
        function GetItem (anIndex: Integer) : Pointer; overload; virtual;
        function GetIndex (anItem: Pointer) : Integer; overload; virtual;
        procedure SetItem (anIndex: Integer; anItem: Pointer); overload; virtual;
        procedure SetIndex (anItem: Pointer; aNewIndex: Integer); overload; virtual;
    public
        constructor Create (anArgs: array of const); overload; virtual;
        constructor Create; overload; virtual;
        destructor Destroy (doFree: Boolean); overload; virtual;
        destructor Destroy; overload; override;
        procedure Assign (aListA: TItems;
                          anOperation: TListAssignOp = laCopy;
                          aListB: TItems = NIL); overload; virtual;
    public
        class function Equals (aListA, aListB: TItems) : Boolean; overload; virtual;
        function Add (anItem: Pointer) : Integer; overload; virtual;
        function Add (anArgs: array of const) : Integer; overload; virtual;
        function Insert (anIndex: Integer; anItem: Pointer) : Integer; overload; virtual;
        function Insert (anIndex: Integer; anArgs: array of const) : Integer; overload; virtual;
        procedure Move (aCurrentIndex, aNewIndex: Integer); virtual;
        procedure Sort; overload; virtual;
        procedure Delete (anIndex: Integer; doFree: Boolean = FALSE); overload; virtual;
        procedure Clear (doFree: Boolean = FALSE); overload; virtual;
        procedure FreeItem (anIndex: Integer); overload; virtual;
        procedure FreeAll; virtual;
    public
        property Index [anItem: Pointer]: Integer read GetIndex write SetIndex;
        property Item [anIndex: Integer]: Pointer read GetItem write SetItem; default;
        property Count: Integer read GetCount;
    end;
{$M-}