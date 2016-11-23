{******************************************************************************}
{*  TDialogControls.int.inc                                                   *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ список элементов управления диалогового окна }
{$M+}
    CDialogControls = class of TDialogControls;
    PDialogControls = ^TDialogControls;
    TDialogControls = class (TItems)
    private
        f_Fields: TDialogFields;
        f_OwnerControls: TComponent;
        f_OwnerLabels: TComponent;
        f_PopupMenu: TPopupMenu;
    protected
        function Check (anItem: Pointer) : Boolean; override;
        function Check (anItem: TDialogControl) : Boolean; overload; virtual;
    protected
        function GetItemAt (anIndex: Integer) : TDialogControl; overload; virtual;
        function GetItemOf (aName: String) : TDialogControl; overload; virtual;
        function GetCaptionAt (anIndex: Integer) : String; virtual;
        function GetCaptionOf (aName: String) : String; virtual;
        function GetValueAt (anIndex: Integer) : Variant; virtual;
        function GetValueOf (aName: String) : Variant; virtual;
        procedure SetItemAt (anIndex: Integer; anItem: TDialogControl); overload; virtual;
        procedure SetItemOf (aName: String; anItem: TDialogControl); overload; virtual;
        procedure SetCaptionAt (anIndex: Integer; aValue: String); virtual;
        procedure SetCaptionOf (aName: String; aValue: String); virtual;
        procedure SetValueAt (anIndex: Integer; aValue: Variant); virtual;
        procedure SetValueOf (aName: String; aValue: Variant); virtual;
    public
        constructor Create (anOwnerControls: TComponent;
                            anOwnerLabels: TComponent;
                            aFields: TDialogFields;
                            aPopupMenu: TPopupMenu = NIL); overload; virtual;
        destructor Destroy; override;
    protected
        class function ItemClassType : CDialogControl; virtual;
    public
        function Add (anItem: TDialogControl; doResize: Boolean = TRUE) : Integer; overload; virtual;
        function Add (aField: TDialogField; doResize: Boolean = TRUE) : Integer; overload; virtual;
        function Add (anArgs: array of const; doResize: Boolean = TRUE) : Integer; overload; virtual;
        procedure Delete (anIndex: Integer; doFree: Boolean = FALSE; doResize: Boolean = TRUE); overload; virtual;
        procedure Clear (doFree: Boolean = FALSE; doResize: Boolean = TRUE); overload; virtual;
        procedure Resize; virtual;
    public
        property OwnerControls: TComponent read f_OwnerControls write f_OwnerControls;
        property OwnerLabels: TComponent read f_OwnerLabels write f_OwnerLabels;
        property PopupMenu: TPopupMenu read f_PopupMenu write f_PopupMenu;
        property ItemAt [anIndex: Integer]: TDialogControl read GetItemAt write SetItemAt;
        property ItemOf [aName: String]: TDialogControl read GetItemOf write SetItemOf;
        property Fields: TDialogFields read f_Fields;
        property CaptionAt [anIndex: Integer]: String read GetCaptionAt write SetCaptionAt;
        property CaptionOf [aName: String]: String read GetCaptionOf write SetCaptionOf;
        property ValueAt [anIndex: Integer]: Variant read GetValueAt write SetValueAt;
        property ValueOf [aName: String]: Variant read GetValueOf write SetValueOf;
    end;
{$M-}