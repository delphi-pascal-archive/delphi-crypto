{******************************************************************************}
{*  MessagesList.int.inc                                                      *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ список сообщений }
type
    PMessageNode = ^TMessageNode;
    TMessageNode = packed record
        ID              : TID;
        IDCategorie     : TID;
        IDAuthor        : TID;
        IDType          : TID;
        IDStatus        : TID;
        IDPic           : TID;
        IndexPic        : Integer;
        Subject         : ShortString;
        Text            : PChar;
        Author          : ShortString;
        TimeStampCreate : TDateTime;
        TimeStampModify : TDateTime;
        TimeStampPublic : TDateTime;
        KeyHash         : ShortString;
        DataHash        : ShortString;
    end;

function CreateMessageNode : PMessageNode;
procedure FreeMessageNode (aValue: PMessageNode);
