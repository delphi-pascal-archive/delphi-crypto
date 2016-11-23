{******************************************************************************}
{*  TCryptoProperty.int.inc                                                   *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ крипто-свойство }
{$M+}
    CCryptoProperty = class of TCryptoProperty;
    PCryptoProperty = ^TCryptoProperty;
    TCryptoProperty = class (TProtoProperty)
    protected
        class function toDataType (const aValue: TVarRec) : TDataType; override;
        class function toDataValue (const aType: TDataType;
                                    const aValue: TVarRec) : Variant; override;
    protected
        function GetAsCrypto (aPublicKey: String;
                              aPrivateKey: String;
                              aCrypto: TProtoObject) : String; virtual;
        function GetAsSafe (aKey: String;
                            aCrypto: TProtoObject) : String; virtual;
        procedure SetAsCrypto (aPublicKey: String;
                               aPrivateKey: String;
                               aCrypto: TProtoObject;
                               const aValue: String); virtual;
        procedure SetAsSafe (aKey: String;
                             aCrypto: TProtoObject;
                             const aValue: String); virtual;
    public
        property asCrypto [aPublicKey: String;
                           aPrivateKey: String;
                           aCrypto: TProtoObject]: String read GetAsCrypto write SetAsCrypto;
        property asSafe [aKey: String;
                         aCrypto: TProtoObject]: String read GetAsSafe write SetAsSafe;
    end;
{$M-}