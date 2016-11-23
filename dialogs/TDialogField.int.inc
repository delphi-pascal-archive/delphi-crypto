{******************************************************************************}
{*  TDialogField.int.inc                                                      *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ поле диалогового окна }
{$M+}
    CDialogField = class of TDialogField;
    PDialogField = ^TDialogField;
    {$IF not Defined (MINI_CRYPTO) }
    TDialogField = class (TCryptoProperty)
    {$ELSE}
    TDialogField = class (TProtoProperty)
    {$IFEND}
    protected
        class function toDataType (const aValue: TVarRec) : TDataType; override;
        class function toDataValue (const aType: TDataType;
                                    const aValue: TVarRec) : Variant; override;
    protected
        function GetAsString : String; override;
        function GetAsDialogField : Variant; virtual;
        procedure SetAsString (const aValue: String); override;
        procedure SetAsDialogField (const aValue: Variant); virtual;
    public
        property asDialogField: Variant read GetAsDialogField write SetAsDialogField;
    end;
{$M-}
