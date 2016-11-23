{******************************************************************************}
{*  UsersList.int.inc                                                         *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ список пользователей }
type
    PUserNode = ^TUserNode;
    TUserNode = packed record
        ID              : TID;
        IDPic           : TID;
        Login           : ShortString;
        Description     : ShortString;
        PublicKey       : ShortString;
        TimeStampCreate : TDateTime;
        TimeStampModify : TDateTime;
        TimeStampPublic : TDateTime;
        KeyHash         : ShortString;
        DataHash        : ShortString;
    end;

function CreateUserNode : PUserNode;
procedure FreeUserNode (aValue: PUserNode);

procedure LoadUsersList (aTreeNodes: TTreeNodes; aParent: TTreeNode;
                         aDB: TSQLiteDatabase; anUserID: TID);
function GetUserNodeID (const aValue: TTreeNode) : TID;
procedure UnLoadUsersList (aTreeNodes: TTreeNodes);
