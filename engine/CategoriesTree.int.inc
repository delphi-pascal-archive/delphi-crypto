{******************************************************************************}
{*  CategoriesTree.int.inc                                                    *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ дерево категорий }
type
    PCategorieNode = ^TCategorieNode;
    TCategorieNode = packed record
        ID              : TID;
        IDParent        : TID;
        IDAuthor        : TID;
        IDType          : TID;
        IDStatus        : TID;
        IDPic           : TID;
        IndexPic        : Integer;
        Name            : ShortString;
        Description     : ShortString;
        Author          : ShortString;
        TimeStampCreate : TDateTime;
        TimeStampModify : TDateTime;
        TimeStampPublic : TDateTime;
        KeyHash         : ShortString;
        DataHash        : ShortString;
        MsgCount        : Integer;
        isMessage       : Boolean;
    end;

function CreateCategorieNode : PCategorieNode;
procedure FreeCategorieNode (aValue: PCategorieNode);

procedure LoadCategoriesTree (aTreeNodes: TTreeNodes; aParent: TTreeNode;
                              aDB: TSQLiteDatabase; aCategorieID: TID);
function GetCategorieNodeID (const aValue: TTreeNode) : TID;
procedure UnLoadCategoriesTree (aTreeNodes: TTreeNodes);
