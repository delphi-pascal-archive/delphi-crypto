{******************************************************************************}
{*  CategoriesTree.imp.inc                                                    *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ CategoriesTree }
function CreateCategorieNode : PCategorieNode;
begin
    Result := AllocMem ( SizeOf (TCategorieNode) );
    Result^.ID := 0;
    Result^.IDParent := 0;
    Result^.IDAuthor := 0;
    Result^.IDType := 0;
    Result^.IDStatus := 0;
    Result^.IDPic := 0;
    Result^.IndexPic := 0;
    Result^.Name := '';
    Result^.Description := '';
    Result^.Author := '';
    Result^.TimeStampCreate := 0.0;
    Result^.TimeStampModify := 0.0;
    Result^.TimeStampPublic := 0.0;
    Result^.KeyHash := '';
    Result^.DataHash := '';
    Result^.MsgCount := 0;
    Result^.isMessage := FALSE;
end;

procedure FreeCategorieNode (aValue: PCategorieNode);
begin
    if Assigned (aValue) then
    try
        aValue^.ID := 0;
        aValue^.IDParent := 0;
        aValue^.IDAuthor := 0;
        aValue^.IDType := 0;
        aValue^.IDStatus := 0;
        aValue^.IDPic := 0;
        aValue^.IndexPic := 0;
        _FillChar ( aValue^.Name, Length (aValue^.Name), $00 );
        _FillChar ( aValue^.Description, Length (aValue^.Description), $00 );
        _FillChar ( aValue^.Author, Length (aValue^.Author), $00 );
        aValue^.TimeStampCreate := 0.0;
        aValue^.TimeStampModify := 0.0;
        aValue^.TimeStampPublic := 0.0;
        _FillChar ( aValue^.KeyHash, Length (aValue^.KeyHash), $00 );
        _FillChar ( aValue^.DataHash, Length (aValue^.DataHash), $00 );
        aValue^.MsgCount := 0;
        aValue^.isMessage := FALSE;
    finally
        Dispose (aValue);
        aValue := NIL;
    end;
end;

procedure LoadCategoriesTree (aTreeNodes: TTreeNodes; aParent: TTreeNode;
                              aDB: TSQLiteDatabase; aCategorieID: TID);
var
    Children : TCategories;
    Node     : TTreeNode;
    I        : Integer;
begin
    try
        if ( not Assigned (aDB) ) then
            raise Exception.Create (ERR_CATEGORIES_TREE_INCORRECT_DATABASE);
        if ( not Assigned (aTreeNodes) ) then
            raise Exception.Create (ERR_CATEGORIES_TREE_INCORRECT_NODES);
        Node := NIL;
        if ( aCategorieID > 0 ) then
        try
            Children := TCategories.Load (aDB,[ NIL, _([aCategorieID]), NIL, _([USER_ID]) ]) as TCategories;
            if Assigned (Children) then
                for I := 0 to Children.Count-1 do
                    if ( Children.ItemAt [I].ID > 0 ) then
                    begin
                        Node := aTreeNodes.AddChild ( aParent, Format ('%s',[ Children.ItemAt [I].Name ]) );
                        Node.Data := CreateCategorieNode;
                        PCategorieNode (Node.Data)^.ID := Children.ItemAt [I].ID;
                        LoadCategoriesTree ( aTreeNodes, Node, aDB, Children.ItemAt [I].ID );
                    end;
        finally
            FreeAndNil (Children);
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s'#13#10'%s',
                                   [ERR_CATEGORIES_TREE_CREATE,E.Message]);
    end;
end;

function GetCategorieNodeID (const aValue: TTreeNode) : TID;
begin
    Result := 0;
    try
        if Assigned (aValue) then
        begin
            if Assigned (aValue) and ( PCategorieNode (aValue.Data)^.ID > 0 ) then
                Result := PCategorieNode (aValue.Data)^.ID
            else
                raise Exception.Create (ERR_CATEGORIES_TREE_INCORRECT_ID);
        end
        else
            raise Exception.Create (ERR_CATEGORIES_TREE_INCORRECT_NODE);
    except on E: Exception do
        raise Exception.CreateFmt ('%s'#13#10'%s',
                                   [ERR_CATEGORIES_TREE_GET_NODE_ID,E.Message]);
    end;
end;

procedure UnLoadCategoriesTree (aTreeNodes: TTreeNodes);
var
    I : Integer;
begin
    try
        SetTabStatus (tabForum,tbsEmpty);
        if ( not Assigned (aTreeNodes) ) then
            raise Exception.Create (ERR_CATEGORIES_TREE_INCORRECT_NODES);
        for I := 0 to aTreeNodes.Count - 1 do
            if Assigned (aTreeNodes [I].Data) then
                FreeCategorieNode ( PCategorieNode (aTreeNodes [I].Data) );
        aTreeNodes.Clear;
    except on E: Exception do
        raise Exception.CreateFmt ('%s'#13#10'%s',
                                   [ERR_CATEGORIES_TREE_DELETE,E.Message]);
    end;
end;
