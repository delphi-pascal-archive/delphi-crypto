{******************************************************************************}
{*  TDialogControl.int.inc                                                    *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ класс элемента управления }
    CControl = class of TControl;
{ элемент управления диалогового окна }
{$M+}
    CDialogControl = class of TDialogControl;
    PDialogControl = ^TDialogControl;
    TDialogControl = class (TObject)
    public
        class procedure _raise (anArgs: array of const;
                                const anEGUID: String = ''); overload; virtual;
        class procedure _raise (anArgs: array of const;
                                anEGUID: array of const); overload; virtual;
    private
        f_Field: TDialogField;
        f_Control: TControl;
        f_Label: TsLabel;
        f_OwnerControl: TComponent;
        f_OwnerLabel: TComponent;
    protected
        function GetName : String; virtual;
        function GetCaption : String; virtual;
        function GetValue : Variant; virtual;
        function GetReadOnly : Boolean; virtual;
        function GetVisible : Boolean; virtual;
        procedure SetName (const aValue: String); virtual; 
        procedure SetCaption (const aValue: String); virtual;
        procedure SetValue (const aValue: Variant); virtual;
        procedure SetReadOnly (const aValue: Boolean); virtual;
        procedure SetVisible (const aValue: Boolean); virtual;
        procedure SetPopupMenu (const aValue: TPopupMenu); virtual;
    protected
        class function GetControlClassType (const aType: TDataType) : CControl; overload; virtual;
    public
        function GetControlClassType : CControl; overload; virtual;
    public
        constructor Create (anOwnerControl: TComponent;
                            anOwnerLabel: TComponent;
                            aField: TDialogField;
                            aPopupMenu: TPopupMenu = NIL); overload; virtual;
        constructor Create (anArgs: array of const); overload; virtual;
        destructor Destroy; override;
    public
        property OwnerControl: TComponent read f_OwnerControl;
        property OwnerLabel: TComponent read f_OwnerLabel;
        property Control: TControl read f_Control write f_Control;
        property LabelControl: TsLabel read f_Label write f_Label;
        property Field: TDialogField read f_Field;
        property Name: String read GetName write SetName;
        property Caption: String read GetCaption write SetCaption;
        property Value: Variant read GetValue write SetValue;
        property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
        property Visible: Boolean read GetVisible write SetVisible;
    end;
{$M-}