{******************************************************************************}
{ written by : Ravil Batirshin 2002                                            }
{ added property TXmlDNodeList.ItemByName : black.rabbit 16.02.2012            }
{******************************************************************************}
unit XMLDOM;

interface

uses
	SysUtils, Classes, XMLParser, XMLUtils;

type
	TXmlDAttribute = class;
	TXmlDCDATASection = class;
	TXmlDComment = class;
	TXmlDDocument = class;
	TXmlDDocumentFragment = class;
	TXmlDDocumentType = class;
	TXmlDElement = class;
	TXmlDEntityReference = class;
	TXmlDNamedNodeMap = class;
	TXmlDNodeList = class;
	TXmlDParseError = class;
	TXmlDProcessingInstruction = class;
	TXmlDText = class;

	//Don't change order of elements! It is important.
	TXmlDNodeType = (
		XMLDNODE_ELEMENT,
		XMLDNODE_ATTRIBUTE,
		XMLDNODE_TEXT,
		XMLDNODE_CDATA_SECTION,
		XMLDNODE_ENTITY_REFERENCE,
		XMLDNODE_ENTITY,
		XMLDNODE_PROCESSING_INSTRUCTION,
		XMLDNODE_COMMENT,
		XMLDNODE_DOCUMENT,
		XMLDNODE_DOCUMENT_TYPE,
		XMLDNODE_DOCUMENT_FRAGMENT,
		XMLDNODE_NOTATION
	);

	TXmlDNode = class(TRefObject)
	private
		FNodeType: TXmlDNodeType; //read-only
		FNodeName: String;
		FPrefix: String;
		FBaseName: String;
		FChildNodes: TXmlDNodeList;
		FAttributes: TXmlDNamedNodeMap;

		function GetFirstChild: TXmlDNode;
		function GetLastChild: TXmlDNode;
		function GetHasChildNodes: Boolean;
		function GetNodeTypeString: String;
	protected
		FParentNode: TXmlDNode; //not addrefed
		FPreviousSibling: TXmlDNode; //not addrefed
		FNextSibling: TXmlDNode; //not addrefed

		procedure CheckParentNode;
		procedure NodeNameChanged;
		function InternalGetNodeName: String; virtual; abstract;
		function GetNodeValue: String; virtual; abstract;
		procedure SetNodeValue(const aValue: String); virtual; abstract;
		function GetText: String; virtual; abstract;
		procedure SetText(const aText: String); virtual; abstract;
		function GetXml: String; virtual; abstract;
		function Clone: TXmlDNode; virtual; abstract;
	public
		UserData: Longint;
		
		constructor Create(aNodeType: TXmlDNodeType);
		destructor Destroy; override;

		function IndexOfChild(aNode: TXmlDNode): Integer;
		{Inserts a child node to the specified position or
		at the end of the list}
		function InsertChild(aNode: TXmlDNode; anIndex: Integer = -1): Integer;
		{Replaces the specified old child node with the supplied
		new child node in the set of children of this node}
		function ReplaceChild(aNewNode, anOldNode: TXmlDNode): Integer;
		{Removes the specified child node from the list of children}
		function RemoveChild(aNode: TXmlDNode): Integer;
		{Appends aNode as the last child of this node}
		function AppendChild(aNode: TXmlDNode): Integer;
		{Creates a new node that is an exact clone of this node}
		function CloneNode(aDeep: Boolean): TXmlDNode;
		{Execute query on the subtree}
		function SelectNodes(const aQuery: String): TXmlDNodeList;
		{Execute query on the subtree}
		function SelectSingleNode(const aQuery: String): TXmlDNode;
		procedure RemoveAllChildren;
		procedure RemoveAllAttributes;

		{Contains the qualified name of the element, attribute, or
		entity reference, or a fixed string for other node types}
		property NodeName: String read FNodeName;
		{Contains the text associated with the node}
		property NodeValue: String read GetNodeValue write SetNodeValue;
		{Specifies the DOM node type}
		property NodeType: TXmlDNodeType read FNodeType;
		{Contains the parent node}
		property ParentNode: TXmlDNode read FParentNode;
		{Contains a node list containing the children}
		property ChildNodes: TXmlDNodeList read FChildNodes;
		{Contains the first child of this node}
		property FirstChild: TXmlDNode read GetFirstChild;
		{Returns the last child node}
		property LastChild: TXmlDNode read GetLastChild;
		{Contains the left sibling of this node}
		property PreviousSibling: TXmlDNode read FPreviousSibling;
		{Contains the next sibling of this node in the parent's child list}
		property NextSibling: TXmlDNode read FNextSibling;
		{Contains the list of attributes for this node}
		property Attributes: TXmlDNamedNodeMap read FAttributes;
		{Returns true if this node has children}
		property HasChildNodes: Boolean read GetHasChildNodes;
		{Returns the node type in string form}
		property NodeTypeString: String read GetNodeTypeString;
		{Contains the text content of the node and its subtrees}
		property Text: String read GetText write SetText;
		{Contains the XML representation of the node and all its descendants}
		property Xml: String read GetXml;
		{Returns the namespace prefix}
		property Prefix: String read FPrefix;
		{Returns the base name for the name qualified with the namespace}
		property BaseName: String read FBaseName;
	end;

	TXmlDNodeList = class(TRefObject)
	private
		FParentNode: TXmlDNode; //not addrefed
		FIsOwner: Boolean;
		FItems: TRefObjectList;

		function GetItem(anIndex: Integer): TXmlDNode;
    function GetItemByName(const aName: String; const aCounter: Integer): TXmlDNode; overload;
    function GetItemByName(const aName: String): TXmlDNode; overload;
		function GetCount: Integer;
	protected
		function Insert(aNode: TXmlDNode; anIndex: Integer = -1): Integer;
		{Deletes the node from the collection}
		procedure Delete(anIndex: Integer);
		{Deletes the node from the collection}
		function Remove(aNode: TXmlDNode): Integer;
		{Deletes all nodes from the collection}
		procedure Clear;
		procedure Replace(aNode: TXmlDNode; anIndex: Integer);
	public
		constructor Create(aParentNode: TXmlDNode; anIsOwner: Boolean);
		destructor Destroy; override;

		function IndexOf(aNode: TXmlDNode): Integer;

		property IsOwner: Boolean read FIsOwner;
		{Allows random access to individual nodes within the collection}
		property Items[anIndex: Integer]: TXmlDNode read GetItem; default;
    {Return the first item with selected name}
    property ItemByNameUnq[const aName: String]: TXmlDNode read GetItemByName;
    {Return the n-item with selected name}
    property ItemByName[const aName: String; const n: Integer]: TXmlDNode read GetItemByName;
		{Indicates the number of items in the collection}
		property Count: Integer read GetCount;
	end;

	TXmlDNamedNodeMap = class(TXmlDNodeList)
	private
		function FindNamedItem(const aName: String; out anIndex: Integer): TXmlDNode;
	public
		{Retrieves the attribute with the specified name}
		function GetNamedItem(const aName: String): TXmlDNode;
		function GetValidNamedItem(const aName: String): TXmlDNode;
		{Adds the supplied node to the collection}
		function SetNamedItem(aNode: TXmlDNode): Integer;
		{Removes an attribute from the collection}
		function RemoveNamedItem(const aName: String): Integer;
	end;

	TXmlDDocument = class(TXmlDNode)
	private
//		FParseError: TXmlDParseError;
		FPreserveWhiteSpace: Boolean;

		function GetDoctype: TXmlDDocumentType;
		function GetDocumentElement: TXmlDElement;
//		procedure SetDocumentElement(anElement: TXmlDElement);
	protected
		function InternalGetNodeName: String; override;
		function GetNodeValue: String; override;
		procedure SetNodeValue(const aValue: String); override;
		function GetText: String; override;
		procedure SetText(const aText: String); override;
		function GetXml: String; override;
		function Clone: TXmlDNode; override;
	public
		constructor Create;

		{Returns a collection of elements that have the specified name}
//		function GetElementsByTagName(const aName: String): TXmlDNodeList;
		procedure Load(aStm: TStream); overload;
		{Loads an XML document from the specified location}
		procedure Load(const aFileName: String); overload;
		{Loads an XML document using the supplied string}
		procedure LoadXML(const aXml: String);
		procedure Save(aStm: TStream); overload;
		{Saves an XML document to the specified location}
		procedure Save(const aFileName: String); overload;

		{Contains the document type node that specifies the DTD for this document}
		property Doctype: TXmlDDocumentType read GetDoctype;
		{Contains the root element of the document}
		property DocumentElement: TXmlDElement read GetDocumentElement;
		{Returns an object that contains information about the last parsing error}
//		property ParseError: TXmlDParseError read FParseError;
		{Indicates whether to preserve all white space in the XML document}
		property PreserveWhiteSpace: Boolean read FPreserveWhiteSpace write FPreserveWhiteSpace;
	end;

	TXmlDDocumentType = class(TXmlDNode)
	private
		FName: String;
		FEntities: TXmlDNamedNodeMap;
		FNotations: TXmlDNamedNodeMap;
	protected
		procedure SetName(const aName: String);
		function InternalGetNodeName: String; override;
		function GetNodeValue: String; override;
		procedure SetNodeValue(const aValue: String); override;
		function GetText: String; override;
		procedure SetText(const aText: String); override;
		function GetXml: String; override;
		function Clone: TXmlDNode; override;
	public
		constructor Create(const aName: String);
		destructor Destroy; override;

		{Contains the name of the document type}
		property Name: String read FName;
		{Contains a list of the entities declared in the document type declaration}
		property Entities: TXmlDNamedNodeMap read FEntities;
		{Contains a list of the notations present in the document type declaration}
		property Notations: TXmlDNamedNodeMap read FNotations;
	end;

	TXmlDElement = class(TXmlDNode)
	private
		FTagName: String;

		function GetAttributeValue(const aName: String): String;
		procedure SetAttributeValue(const aName, aValue: String);
		function GetAttributeNode(const aName: String): TXmlDAttribute;
	protected
		procedure SetTagName(const aName: String);
		function InternalGetNodeName: String; override;
		function GetNodeValue: String; override;
		procedure SetNodeValue(const aValue: String); override;
		function GetText: String; override;
		procedure SetText(const aText: String); override;
		function GetXml: String; override;
		function Clone: TXmlDNode; override;
	public
		constructor Create(const aName: String);

		function GetAttr(const aName: String; const aDefault: String = ''): String;
    function NeedAttr(const aName: String): String;

		{Removes or replaces the named attribute}
		function RemoveAttribute(const aName: String): Integer;
		{Adds or changes the supplied attribute node on this element}
		function SetAttributeNode(anAttribute: TXmlDAttribute): Integer;
		{Removes the specified attribute from this element}
		function RemoveAttributeNode(anAttribute: TXmlDAttribute): Integer;
		{Normalizes all descendent elements; combines two or more adjacent
		text nodes into one unified text node}
//		procedure Normalize;

		{Contains the element name, the name that appears within the tag}
		property TagName: String read FTagName;
		{Value of the named attribute}
		property AttributeValue[const aName: String]: String read GetAttributeValue write SetAttributeValue;
		{Named attribute node}
		property AttributeNode[const aName: String]: TXmlDAttribute read GetAttributeNode;
	end;

	TXmlDAttribute = class(TXmlDNode)
	private
		FName: String;
		FValue: String;
	protected
		procedure SetName(const aName: String);
		function InternalGetNodeName: String; override;
		function GetNodeValue: String; override;
		procedure SetNodeValue(const aValue: String); override;
		function GetText: String; override;
		procedure SetText(const aText: String); override;
		function GetXml: String; override;
		function Clone: TXmlDNode; override;
	public
		constructor Create(const aName: String);

		{Contains the attribute name}
		property Name: String read FName;
		{Contains the attribute value}
		property Value: String read FValue write FValue;
	end;

	TXmlDDocumentFragment = class(TXmlDNode)
	private
	protected
		function InternalGetNodeName: String; override;
		function GetNodeValue: String; override;
		procedure SetNodeValue(const aValue: String); override;
		function GetText: String; override;
		procedure SetText(const aText: String); override;
		function GetXml: String; override;
		function Clone: TXmlDNode; override;
	public
		constructor Create;
	end;

	TXmlDCharacterData = class(TXmlDNode)
	private
		FData: String;

		function GetCount: Integer;
	protected
		function GetNodeValue: String; override;
		procedure SetNodeValue(const aValue: String); override;
		function GetText: String; override;
		procedure SetText(const aText: String); override;
	public
		constructor Create(aNodeType: TXmlDNodeType; const aData: String);

		{Retrieves a substring of the full string from the specified range}
		function SubstringData(anOffset, aCount: Integer): String;
		{Appends the supplied string to the existing string data}
		procedure AppendData(const aData: String);
		{Inserts the supplied string at the specified offset}
		procedure InsertData(anOffset: Integer; const aData: String);
		{Deletes the specified substring within the string data}
		procedure DeleteData(anOffset, aCount: Integer);
		{Replaces the specified number of characters with the supplied string}
		procedure ReplaceData(anOffset, aCount: Integer; const aData: String);

		{Contains this node's data, which depends on the node type}
		property Data: String read FData write FData;
		{Specifies the length, in characters, of the data}
		property Count: Integer read GetCount;
	end;

	TXmlDComment = class(TXmlDCharacterData)
	private
	protected
		function InternalGetNodeName: String; override;
		function GetXml: String; override;
		function Clone: TXmlDNode; override;
	public
		constructor Create(const aData: String);
	end;

	TXmlDText = class(TXmlDCharacterData)
	private
	protected
		function InternalGetNodeName: String; override;
		function GetXml: String; override;
		function Clone: TXmlDNode; override;

		constructor Create(aNodeType: TXmlDNodeType; const aData: String); overload;
	public
		constructor Create(const aData: String); overload;

		{Breaks this text node into two text nodes at the specified offset,
		and inserts the new text node into the tree as a sibling that
		immediately follows this node}
		function SplitText(anOffset: Longint): TXmlDText;
	end;

	TXmlDCDATASection = class(TXmlDText)
	private
	protected
		function InternalGetNodeName: String; override;
		function GetXml: String; override;
		function Clone: TXmlDNode; override;
	public
		constructor Create(const aData: String);
	end;

	TXmlDProcessingInstruction = class(TXmlDNode)
	private
		FTarget: String;
		FData: String;
	protected
		procedure SetTarget(const aTarget: String);
		function InternalGetNodeName: String; override;
		function GetNodeValue: String; override;
		procedure SetNodeValue(const aValue: String); override;
		function GetText: String; override;
		procedure SetText(const aText: String); override;
		function GetXml: String; override;
		function Clone: TXmlDNode; override;
	public
		constructor Create(const aTarget, aData: String);

		{Specifies the target, the application to which this
		processing instruction is directed}
		property Target: String read FTarget;
		{Contains the content of the processing instruction, excluding the target}
		property Data: String read FData write FData;
	end;

	TXmlDEntityReference = class(TXmlDNode)
	private
		FName: String;
	protected
		procedure SetName(const aName: String);
		function InternalGetNodeName: String; override;
		function GetNodeValue: String; override;
		procedure SetNodeValue(const aValue: String); override;
		function GetText: String; override;
		procedure SetText(const aText: String); override;
		function GetXml: String; override;
		function Clone: TXmlDNode; override;
	public
		constructor Create(const aName: String);

		property Name: String read FName;
	end;

	TXmlDEntity = class(TXmlDNode)
	private
		FPublicId: Variant;
		FSystemId: Variant;
		FNotationName: String;
	protected
		procedure SetNotationName(const aName: String);
		function InternalGetNodeName: String; override;
		function GetNodeValue: String; override;
		procedure SetNodeValue(const aValue: String); override;
		function GetText: String; override;
		procedure SetText(const aText: String); override;
		function GetXml: String; override;
		function Clone: TXmlDNode; override;
	public
		constructor Create;

		{Contains the public identifier}
		property PublicId: Variant read FPublicId;
		{Contains the system identifier}
		property SystemId: Variant read FSystemId;
		{Contains the notation name}
		property NotationName: String read FNotationName;
	end;

	TXmlDNotation = class(TXmlDNode)
	private
		FPublicId: Variant;
		FSystemId: Variant;
	protected
		function InternalGetNodeName: String; override;
		function GetNodeValue: String; override;
		procedure SetNodeValue(const aValue: String); override;
		function GetText: String; override;
		procedure SetText(const aText: String); override;
		function GetXml: String; override;
		function Clone: TXmlDNode; override;
	public
		constructor Create;

		{Contains the public identifier for the notation}
		property PublicId: Variant read FPublicId;
		{Contains the system identifier for the notation}
		property SystemId: Variant read FSystemId;
	end;

	TXmlDParseError = class(TRefObject)
	private
		FErrorCode: Longint;
		FReason: String;
		FSrcText: String;
		FLine: Longint;
		FLinepos: Longint;
		FFilepos: Longint;
	public
		{Returns the error code of the last parse error}
		property ErrorCode: Longint read FErrorCode;
		{Explains the reason for the error}
		property Reason: String read FReason;
		{Returns the text of the line containing the error}
		property SrcText: String read FSrcText;
		{Specifies the line number that contains the error}
		property Line: Longint read FLine;
		{Contains the character position within the line where the error occurred}
		property Linepos: Longint read FLinepos;
		{Contains the absolute file position where the error occurred}
		property Filepos: Longint read FFilepos;
	end;

implementation

{ TXmlDNode }

constructor TXmlDNode.Create(aNodeType: TXmlDNodeType);
begin
	inherited Create;
	FNodeType := aNodeType;
	RO_Init(FChildNodes, TXmlDNodeList.Create(Self, True));
	RO_Init(FAttributes, TXmlDNamedNodeMap.Create(Self, True));
end;

destructor TXmlDNode.Destroy;
begin
	RO_Release(FAttributes);
	RO_Release(FChildNodes);
	inherited Destroy;
end;

function TXmlDNode.GetFirstChild: TXmlDNode;
begin
	if ChildNodes.Count > 0 then
		Result := ChildNodes[0]
	else
		Result := nil;
end;

function TXmlDNode.GetLastChild: TXmlDNode;
begin
	if ChildNodes.Count > 0 then
		Result := ChildNodes[ChildNodes.Count - 1]
	else
		Result := nil;
end;

function TXmlDNode.GetHasChildNodes: Boolean;
begin
	Result := ChildNodes.Count > 0;
end;

function TXmlDNode.GetNodeTypeString: String;
const
	kNames: array[TXmlDNodeType] of String = (
		'element',
		'attribute',
		'text',
		'cdatasection',
		'entityreference',
		'entity',
		'processinginstruction',
		'comment',
		'document',
		'documenttype',
		'documentfragment',
		'notation'
	);
begin
	Result := kNames[NodeType];
end;

procedure TXmlDNode.CheckParentNode;
begin
	if not Assigned(FParentNode) then
		raise Exception.Create('XML: у узла не установлен родительский узел');
end;

procedure TXmlDNode.NodeNameChanged;
var
	i: Integer;
begin
	FNodeName := InternalGetNodeName;
	i := Pos(':', FNodeName);
	if i > 0 then begin
		FPrefix := Copy(FNodeName, 1, i - 1);
		FBaseName := Copy(FNodeName, i + 1, Length(FNodeName) - i);
	end
	else begin
		FPrefix := '';
		FBaseName := FNodeName;
	end;
end;

function TXmlDNode.IndexOfChild(aNode: TXmlDNode): Integer;
begin
	Result := ChildNodes.IndexOf(aNode);
end;

function TXmlDNode.InsertChild(aNode: TXmlDNode; anIndex: Integer): Integer;

	procedure InsertDocumentFragment;
	var
		aChild: TXmlDNode;
	begin
		RO_Init(aChild, aNode.LastChild);
		try
			while Assigned(aChild) do begin
				InsertChild(aChild, Result);
				RO_Replace(aChild, aChild.PreviousSibling);
			end;
		finally
			RO_Release(aChild);
		end;
	end;

begin
	if anIndex = -1 then
		anIndex := ChildNodes.Count;
	Result := anIndex;

	case NodeType of
		XMLDNODE_ELEMENT:
			case aNode.NodeType of
				XMLDNODE_CDATA_SECTION,
				XMLDNODE_COMMENT,
				XMLDNODE_ELEMENT,
				XMLDNODE_ENTITY_REFERENCE,
				XMLDNODE_TEXT,
				XMLDNODE_PROCESSING_INSTRUCTION,
				XMLDNODE_DOCUMENT_FRAGMENT: ;
				else
					raise Exception.Create('XML: узел "' + NodeTypeString +
							'" не может содержать подузел "' + aNode.NodeTypeString + '"');
			end;
		XMLDNODE_ENTITY_REFERENCE: ; //???
		XMLDNODE_ENTITY: ; //???
		XMLDNODE_DOCUMENT:
			case aNode.NodeType of
				XMLDNODE_COMMENT,
				XMLDNODE_PROCESSING_INSTRUCTION,
				XMLDNODE_DOCUMENT_TYPE,
				XMLDNODE_ELEMENT,
				XMLDNODE_DOCUMENT_FRAGMENT: ;
				else
					raise Exception.Create('XML: узел "' + NodeTypeString +
							'" не может содержать подузел "' + aNode.NodeTypeString + '"');
			end;
		XMLDNODE_DOCUMENT_TYPE: ; //???
		XMLDNODE_DOCUMENT_FRAGMENT:
			case aNode.NodeType of
				XMLDNODE_CDATA_SECTION,
				XMLDNODE_COMMENT,
				XMLDNODE_ELEMENT,
				XMLDNODE_ENTITY_REFERENCE,
				XMLDNODE_PROCESSING_INSTRUCTION,
				XMLDNODE_TEXT,
				XMLDNODE_DOCUMENT_FRAGMENT: ;
				else
					raise Exception.Create('XML: узел "' + NodeTypeString +
							'" не может содержать подузел "' + aNode.NodeTypeString + '"');
			end;
		else //??? XMLNODE_ATTRIBUTE:
			raise Exception.Create('XML: узел "' + NodeTypeString +
					'" не может содержать подузлы');
	end;

	aNode.AddRef;
	try
		if Assigned(aNode.ParentNode) then
			aNode.ParentNode.RemoveChild(aNode);
		if aNode.NodeType = XMLDNODE_DOCUMENT_FRAGMENT then
			InsertDocumentFragment
		else
			ChildNodes.Insert(aNode, Result);
	finally
		aNode.Release;
	end;
end;

function TXmlDNode.ReplaceChild(aNewNode, anOldNode: TXmlDNode): Integer;
begin
	Result := ChildNodes.IndexOf(anOldNode);
	if Result < 0 then
		raise Exception.Create('XML: Не найден подузел для замены');
	if Assigned(aNewNode) then
		InsertChild(aNewNode, Result + 1);
	ChildNodes.Delete(Result);
end;

function TXmlDNode.RemoveChild(aNode: TXmlDNode): Integer;
begin
	Result := ChildNodes.IndexOf(aNode);
	if Result < 0 then
		raise Exception.Create('XML: Не найден подузел для удаления');
	ChildNodes.Delete(Result);
end;

function TXmlDNode.AppendChild(aNode: TXmlDNode): Integer;
begin
	Result := InsertChild(aNode);
end;

function TXmlDNode.CloneNode(aDeep: Boolean): TXmlDNode;
var
	i: Integer;
begin
	Result := Clone;
	for i := 0 to Attributes.Count - 1 do
		Result.Attributes.Insert(Attributes[i].CloneNode(aDeep));
	if aDeep then
		for i := 0 to ChildNodes.Count - 1 do
			Result.ChildNodes.Insert(ChildNodes[i].CloneNode(aDeep));
end;

function TXmlDNode.SelectNodes(const aQuery: String): TXmlDNodeList;
var
	aChild: TXmlDNode;
	i: Integer;
begin
	Result := TXmlDNodeList.Create(Self, False);
	try
		for i := 0 to ChildNodes.Count - 1 do begin
			aChild := ChildNodes[i];
			if aChild.NodeName = aQuery then
				Result.Insert(aChild);
		end;
	except
		RO_Free(Result);
		raise;
	end;
end;

function TXmlDNode.SelectSingleNode(const aQuery: String): TXmlDNode;
var
	i: Integer;
begin
	for i := 0 to ChildNodes.Count - 1 do begin
		Result := ChildNodes[i];
		if Result.NodeName = aQuery then
			exit;
	end;
	Result := nil;
end;

procedure TXmlDNode.RemoveAllChildren;
begin
	ChildNodes.Clear;
end;

procedure TXmlDNode.RemoveAllAttributes;
begin
	Attributes.Clear;
end;

{ TXmlDNodeList }

constructor TXmlDNodeList.Create(aParentNode: TXmlDNode; anIsOwner: Boolean);
begin
	inherited Create;
	FParentNode := aParentNode;
	FIsOwner := anIsOwner;
	FItems := TRefObjectList.Create;
end;

destructor TXmlDNodeList.Destroy;
begin
	FItems.Free;
	inherited Destroy;
end;

function TXmlDNodeList.GetItem(anIndex: Integer): TXmlDNode;
begin
	Result := FItems[anIndex];
end;

function TXmlDNodeList.GetItemByName(const aName: String; const aCounter: Integer): TXmlDNode;
var
  I: Integer;
  Counter: Integer;
begin
  Result := NIL;
  Counter := -1;
  for I := 0 to Count - 1 do
      if ( Items[I].NodeName = aName ) then
      begin
          Result := Items[I];
          Inc (Counter);
          if ( Counter >= aCounter ) then
              Break;
      end;
end;

function TXmlDNodeList.GetItemByName(const aName: String): TXmlDNode;
begin
    Result := GetItemByName (aName,0);
end;

function TXmlDNodeList.GetCount: Integer;
begin
	Result := FItems.Count;
end;

function TXmlDNodeList.Insert(aNode: TXmlDNode; anIndex: Integer): Integer;
var
	aNode2: TXmlDNode;
begin
	if IsOwner and Assigned(aNode.ParentNode) then
		raise Exception.Create('XML: Нельзя добавлять узел в два списка');
	if anIndex = -1 then
		anIndex := FItems.Count;
	Result := anIndex;
	FItems.Insert(anIndex, aNode);
	if IsOwner then begin
		aNode.FParentNode := FParentNode;
		aNode.FPreviousSibling := nil;
		aNode.FNextSibling := nil;
		if anIndex > 0 then begin
			aNode2 := FItems[anIndex - 1];
			aNode2.FNextSibling := aNode;
			aNode.FPreviousSibling := aNode2;
		end;
		if anIndex < FItems.Count - 1 then begin
			aNode2 := FItems[anIndex + 1];
			aNode2.FPreviousSibling := aNode;
			aNode.FNextSibling := aNode2;
		end;
	end;
end;

procedure TXmlDNodeList.Delete(anIndex: Integer);
var
	aNode, aNodePrev, aNodeNext: TXmlDNode;
begin
	RO_Init(aNode, FItems[anIndex]);
	try
		FItems.Delete(anIndex);
		if IsOwner then begin
			aNode.FParentNode := nil;
			aNode.FPreviousSibling := nil;
			aNode.FNextSibling := nil;

			aNodePrev := nil;
			aNodeNext := nil;
			if anIndex > 0 then
				aNodePrev := FItems[anIndex - 1];
			if anIndex < FItems.Count then
				aNodeNext := FItems[anIndex];

			if Assigned(aNodePrev) then
				aNodePrev.FNextSibling := aNodeNext;
			if Assigned(aNodeNext) then
				aNodeNext.FPreviousSibling := aNodePrev;
		end;
	finally
		RO_Release(aNode);
	end;
end;

function TXmlDNodeList.Remove(aNode: TXmlDNode): Integer;
begin
	Result := IndexOf(aNode);
	if Result >= 0 then
		Delete(Result);
end;

procedure TXmlDNodeList.Clear;
begin
	FItems.Clear;
end;

procedure TXmlDNodeList.Replace(aNode: TXmlDNode; anIndex: Integer);
begin
	if IsOwner and Assigned(aNode.ParentNode) then
		raise Exception.Create('XML: Нельзя добавлять подузел в два списка');
	Delete(anIndex);
	Insert(aNode, anIndex);
end;

function TXmlDNodeList.IndexOf(aNode: TXmlDNode): Integer;
begin
	Result := FItems.IndexOf(aNode);
end;

{ TXmlDNamedNodeMap }

function TXmlDNamedNodeMap.FindNamedItem(const aName: String;
	out anIndex: Integer): TXmlDNode;
var
	i: Integer;
begin
	for i := 0 to FItems.Count - 1 do begin
		Result := FItems[i];
		if Result.NodeName = aName then begin
			anIndex := i;
			exit;
		end;
	end;
	anIndex := -1;
	Result := nil;
end;

function TXmlDNamedNodeMap.GetNamedItem(const aName: String): TXmlDNode;
var
	anIndex: Integer;
begin
	Result := FindNamedItem(aName, anIndex);
end;

function TXmlDNamedNodeMap.GetValidNamedItem(const aName: String): TXmlDNode;
var
	anIndex: Integer;
begin
	Result := FindNamedItem(aName, anIndex);
	if not Assigned(Result) then
		raise Exception.Create('XML: в списке не найден узел с именем "' + aName + '"');
end;

function TXmlDNamedNodeMap.SetNamedItem(aNode: TXmlDNode): Integer;
begin
	FindNamedItem(aNode.NodeName, Result);
	if Result >= 0 then
		Replace(aNode, Result)
	else
		Result := Insert(aNode);
end;

function TXmlDNamedNodeMap.RemoveNamedItem(const aName: String): Integer;
begin
	FindNamedItem(aName, Result);
	if Result >= 0 then
		Delete(Result);
end;

{ TXmlDDocument }

constructor TXmlDDocument.Create;
begin
	inherited Create(XMLDNODE_DOCUMENT);
	NodeNameChanged;
end;

function TXmlDDocument.GetDoctype: TXmlDDocumentType;
var
	aNode: TXmlDNode;
begin
	aNode := FirstChild;
	while Assigned(aNode) do begin
		if aNode.NodeType = XMLDNODE_DOCUMENT_TYPE then
			break;
		aNode := aNode.NextSibling;
	end;
	Result := aNode as TXmlDDocumentType;
end;

function TXmlDDocument.GetDocumentElement: TXmlDElement;
var
	aNode: TXmlDNode;
begin
	aNode := FirstChild;
	while Assigned(aNode) do begin
		if aNode.NodeType = XMLDNODE_ELEMENT then
			break;
		aNode := aNode.NextSibling;
	end;
	Result := aNode as TXmlDElement;
end;

function TXmlDDocument.InternalGetNodeName: String;
begin
	Result := '#document';
end;

function TXmlDDocument.GetNodeValue: String;
begin
	Result := '';
end;

procedure TXmlDDocument.SetNodeValue(const aValue: String);
begin
	raise Exception.Create('XML: Нельзя изменять значение узла "' +
			NodeTypeString + '"');
end;

function TXmlDDocument.GetText: String;
var
	aChild: TXmlDNode;
begin
	Result := '';
	aChild := FirstChild;
	while Assigned(aChild) do begin
		if aChild.NodeType in [XMLDNODE_ELEMENT, XMLDNODE_TEXT, XMLDNODE_CDATA_SECTION] then
			Result := Result + aChild.Text;
		aChild := aChild.NextSibling;
	end;
end;

procedure TXmlDDocument.SetText(const aText: String);
begin
	raise Exception.Create('XML: Нельзя изменять текст узла "' +
			NodeTypeString + '"'); //???
end;

function TXmlDDocument.GetXml: String;
var
	aChild: TXmlDNode;
begin
	Result := '';
	aChild := FirstChild;
	while Assigned(aChild) do begin
		Result := Result + aChild.Xml;
		aChild := aChild.NextSibling;
	end;
end;

function TXmlDDocument.Clone: TXmlDNode;
var
	aClone: TXmlDDocument;
begin
	aClone := TXmlDDocument.Create;
	aClone.PreserveWhiteSpace := PreserveWhiteSpace;
	Result := aClone;
end;

procedure TXmlDDocument.Load(aStm: TStream);
var
	aReader: TXmlReader;
	aToken: TXmlReaderTokenType;

	procedure LoadProcessingInstruction(aParent: TXmlDNode);
	var
		anInstruction: TXmlDProcessingInstruction;
	begin
		RO_Init(anInstruction,
				TXmlDProcessingInstruction.Create(aReader.Name, aReader.Value));
		try
			aParent.AppendChild(anInstruction);
		finally
			RO_Release(anInstruction);
		end;
		aToken := aReader.ReadNextToken;
	end;

	procedure LoadDocumentType(aParent: TXmlDNode);
	var
		aDocTypeNode: TXmlDDocumentType;
	begin
		RO_Init(aDocTypeNode, TXmlDDocumentType.Create(aReader.Name));
		try
			aParent.AppendChild(aDocTypeNode);
			//!!!Process aReader.Value --> children, entries, notations

		finally
			RO_Release(aDocTypeNode);
		end;
		aToken := aReader.ReadNextToken;
	end;

	procedure LoadComment(aParent: TXmlDNode);
	var
		aComment: TXmlDComment;
	begin
		RO_Init(aComment, TXmlDComment.Create(aReader.Value));
		try
			aParent.AppendChild(aComment);
		finally
			RO_Release(aComment);
		end;
		aToken := aReader.ReadNextToken;
	end;

	procedure LoadCData(aParent: TXmlDNode);
	var
		aCData: TXmlDCDataSection;
	begin
		RO_Init(aCData, TXmlDCDataSection.Create(aReader.Value));
		try
			aParent.AppendChild(aCData);
		finally
			RO_Release(aCData);
		end;
		aToken := aReader.ReadNextToken;
	end;

	procedure LoadText(aParent: TXmlDNode);
	var
		s: String;
		aText: TXmlDText;
	begin
		s := aReader.Value;
		if PreserveWhiteSpace or ((Pos(#13, s) = 0) and (Pos(#10, s) = 0)) or
				(Trim(s) <> '') then begin
			RO_Init(aText, TXmlDText.Create(DecodeHtmlString(s)));
			try
				aParent.AppendChild(aText);
			finally
				RO_Release(aText);
			end;
		end;
		aToken := aReader.ReadNextToken;
	end;

	procedure LoadAttribute(aParent: TXmlDElement);
	begin
		aParent.SetAttributeValue(aReader.Name, DecodeHtmlString(aReader.Value));
		aToken := aReader.ReadNextToken;
	end;

	procedure LoadElement(aParent: TXmlDNode);
	var
		anElement: TXmlDElement;
	begin
		RO_Init(anElement, TXmlDElement.Create(aReader.Name));
		try
			aParent.AppendChild(anElement);
			aToken := aReader.ReadNextToken;
			while aToken <> xrtEof do begin
				case aToken of
					xrtProcessingInstruction: LoadProcessingInstruction(anElement);
					xrtDocumentType: LoadDocumentType(anElement);
					xrtComment: LoadComment(anElement);
					xrtCData: LoadCData(anElement);
					xrtElementBegin: LoadElement(anElement);
					xrtElementEnd: begin
						if aReader.Name <> anElement.TagName then
							raise Exception.Create('XML: Имена открывающего и закрывающего тэгов не совпадают');
						aToken := aReader.ReadNextToken;
						break;
					end;
					xrtElementAttribute: LoadAttribute(anElement);
					xrtText: LoadText(anElement);
					else
						raise Exception.Create('XML: неправильная конструкция в XML-документе');
				end;
			end;
		finally
			RO_Release(anElement);
		end;
	end;

begin
	RemoveAllChildren;
	RemoveAllAttributes;
	aReader := TXmlReader.Create;
	try
		aReader.Open(aStm, False);
		aToken := aReader.ReadNextToken;
		while aToken <> xrtEof do begin
			case aToken of
				xrtProcessingInstruction: LoadProcessingInstruction(Self);
				xrtDocumentType: LoadDocumentType(Self);
				xrtComment: LoadComment(Self);
				xrtElementBegin: LoadElement(Self);
				else
					raise Exception.Create('XML: неправильная конструкция в XML-документе');
			end;
		end;
	finally
		aReader.Free;
	end;
end;

procedure TXmlDDocument.Load(const aFileName: String);
var
	aStm: TFileStream;
begin
	aStm := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
	try
		Load(aStm);
	finally
		aStm.Free;
	end;
end;

procedure TXmlDDocument.LoadXML(const aXml: String);
var
	aStm: TStringStream;
begin
	aStm := TStringStream.Create(aXml);
	try
		Load(aStm);
	finally
		aStm.Free;
	end;
end;

procedure TXmlDDocument.Save(aStm: TStream);
var
	aXml: String;
begin
	aXml := Xml;
	aStm.WriteBuffer(Pointer(aXml)^, Length(aXml));
end;

procedure TXmlDDocument.Save(const aFileName: String);
var
	aStm: TFileStream;
begin
	aStm := TFileStream.Create(aFileName, fmCreate);
	try
		Save(aStm);
	finally
		aStm.Free;
	end;
end;

{ TXmlDDocumentType }

constructor TXmlDDocumentType.Create(const aName: String);
begin
	inherited Create(XMLDNODE_DOCUMENT_TYPE);
	RO_Init(FEntities, TXmlDNamedNodeMap.Create(Self, True));
	RO_Init(FNotations, TXmlDNamedNodeMap.Create(Self, True));
	SetName(aName);
end;

destructor TXmlDDocumentType.Destroy;
begin
	RO_Release(FNotations);
	RO_Release(FEntities);
	inherited Destroy;
end;

procedure TXmlDDocumentType.SetName(const aName: String);
begin
	FName := aName;
	NodeNameChanged;
end;

function TXmlDDocumentType.InternalGetNodeName: String;
begin
	Result := Name;
end;

function TXmlDDocumentType.GetNodeValue: String;
begin
	Result := '';
end;

procedure TXmlDDocumentType.SetNodeValue(const aValue: String);
begin
	raise Exception.Create('XML: Нельзя изменять значение узла "' +
			NodeTypeString + '"');
end;

function TXmlDDocumentType.GetText: String;
begin
	Result := '';
end;

procedure TXmlDDocumentType.SetText(const aText: String);
begin
	raise Exception.Create('XML: Нельзя изменять текст узла "' +
			NodeTypeString + '"');
end;

function TXmlDDocumentType.GetXml: String;
var
	aChild: TXmlDNode;
begin
	Result := '<!DOCTYPE ' + Name + ' [';
	aChild := FirstChild;
	while Assigned(aChild) do begin
		Result := Result + aChild.Xml;
		aChild := aChild.NextSibling;
	end;
	Result := Result + ']>';
end;

function TXmlDDocumentType.Clone: TXmlDNode;
begin
	Result := TXmlDDocumentType.Create(Name);
end;

{ TXmlDElement }

constructor TXmlDElement.Create(const aName: String);
begin
	inherited Create(XMLDNODE_ELEMENT);
	SetTagName(aName);
end;

function TXmlDElement.GetAttributeValue(const aName: String): String;
var
	anAttr: TXmlDAttribute;
begin
	anAttr := AttributeNode[aName];
	if Assigned(anAttr) then
		Result := anAttr.Value
	else
		Result := '';
end;

procedure TXmlDElement.SetAttributeValue(const aName, aValue: String);
var
	anAttr: TXmlDAttribute;
begin
	RO_Init(anAttr, AttributeNode[aName]);
	try
		if not Assigned(anAttr) then begin
			RO_Init(anAttr, TXmlDAttribute.Create(aName));
			Attributes.Insert(anAttr);
		end;
		anAttr.Value := aValue;
	finally
		RO_Release(anAttr);
	end;
end;

function TXmlDElement.GetAttributeNode(const aName: String): TXmlDAttribute;
begin
	Result := Attributes.GetNamedItem(aName) as TXmlDAttribute;
end;

procedure TXmlDElement.SetTagName(const aName: String);
begin
	FTagName := aName;
	NodeNameChanged;
end;

function TXmlDElement.InternalGetNodeName: String;
begin
	Result := TagName;
end;

function TXmlDElement.GetNodeValue: String;
begin
	Result := '';
end;

procedure TXmlDElement.SetNodeValue(const aValue: String);
begin
	raise Exception.Create('XML: Нельзя изменять значение узла "' +
			NodeTypeString + '"');
end;

function TXmlDElement.GetText: String;
var
	aChild: TXmlDNode;
begin
	Result := '';
	aChild := FirstChild;
	while Assigned(aChild) do begin
		if aChild.NodeType in [XMLDNODE_ELEMENT, XMLDNODE_TEXT, XMLDNODE_CDATA_SECTION] then
			Result := Result + aChild.Text;
		aChild := aChild.NextSibling;
	end;
end;

procedure TXmlDElement.SetText(const aText: String);
var
	aChild: TXmlDText;
begin
	RO_Init(aChild, TXmlDText.Create(aText));
	try
		RemoveAllChildren;
		AppendChild(aChild);
	finally
		RO_Release(aChild);
	end;
end;

function TXmlDElement.GetXml: String;
var
	i: Integer;
begin
	Result := '<' + TagName;
	for i := 0 to Attributes.Count - 1 do
		Result := Result + ' ' + Attributes[i].Xml;
	if not HasChildNodes then
		Result := Result + '/>'
	else begin
		Result := Result + '>';
		for i := 0 to ChildNodes.Count - 1 do
			Result := Result + ChildNodes[i].Xml;
		Result := Result + '</' + TagName + '>';
	end;
end;

function TXmlDElement.Clone: TXmlDNode;
begin
	Result := TXmlDElement.Create(TagName);
end;

function TXmlDElement.RemoveAttribute(const aName: String): Integer;
begin
	Result := Attributes.RemoveNamedItem(aName);
end;

function TXmlDElement.SetAttributeNode(anAttribute: TXmlDAttribute): Integer;
begin
	Result := Attributes.SetNamedItem(anAttribute);
end;

function TXmlDElement.RemoveAttributeNode(anAttribute: TXmlDAttribute): Integer;
begin
	Result := Attributes.Remove(anAttribute);
end;

function TXmlDElement.GetAttr(const aName, aDefault: String): String;
var
	anAttr: TXmlDAttribute;
begin
	anAttr := Attributes.GetNamedItem(aName) as TXmlDAttribute;
	if Assigned(anAttr) then
		Result := anAttr.FValue
	else
		Result := aDefault;
end;

function TXmlDElement.NeedAttr(const aName: String): String;
var
	anAttr: TXmlDAttribute;
begin
	anAttr := Attributes.GetNamedItem(aName) as TXmlDAttribute;
	if not Assigned(anAttr) then
		raise Exception.CreateFmt('Не задан атрибут "%s" у элемента "%s".', [aName, FNodeName]);
	Result := anAttr.FValue
end;

{ TXmlDAttribute }

constructor TXmlDAttribute.Create(const aName: String);
begin
	inherited Create(XMLDNODE_ATTRIBUTE);
	SetName(aName);
end;

procedure TXmlDAttribute.SetName(const aName: String);
begin
	FName := aName;
	NodeNameChanged;
end;

function TXmlDAttribute.InternalGetNodeName: String;
begin
	Result := Name;
end;

function TXmlDAttribute.GetNodeValue: String;
begin
	Result := Value;
end;

procedure TXmlDAttribute.SetNodeValue(const aValue: String);
begin
	Value := aValue;
end;

function TXmlDAttribute.GetText: String;
begin
	Result := NodeValue;
end;

procedure TXmlDAttribute.SetText(const aText: String);
begin
	NodeValue := aText;
end;

function TXmlDAttribute.GetXml: String;
begin
	Result := Name + '="' + EncodeHtmlString(Value) + '"';
end;

function TXmlDAttribute.Clone: TXmlDNode;
var
	aClone: TXmlDAttribute;
begin
	aClone := TXmlDAttribute.Create(Name);
	aClone.Value := Value;
	Result := aClone;
end;

{ TXmlDDocumentFragment }

constructor TXmlDDocumentFragment.Create;
begin
	inherited Create(XMLDNODE_DOCUMENT_FRAGMENT);
	NodeNameChanged;
end;

function TXmlDDocumentFragment.InternalGetNodeName: String;
begin
	Result := '#document-fragment';
end;

function TXmlDDocumentFragment.GetNodeValue: String;
begin
	Result := '';
end;

procedure TXmlDDocumentFragment.SetNodeValue(const aValue: String);
begin
	raise Exception.Create('XML: Нельзя изменять значение узла "' +
			NodeTypeString + '"');
end;

function TXmlDDocumentFragment.GetText: String;
var
	aChild: TXmlDNode;
begin
	Result := '';
	aChild := FirstChild;
	while Assigned(aChild) do begin
		if aChild.NodeType in [XMLDNODE_ELEMENT, XMLDNODE_TEXT, XMLDNODE_CDATA_SECTION] then
			Result := Result + aChild.Text;
		aChild := aChild.NextSibling;
	end;
end;

procedure TXmlDDocumentFragment.SetText(const aText: String);
begin
	raise Exception.Create('XML: Нельзя изменять текст узла "' +
			NodeTypeString + '"');
end;

function TXmlDDocumentFragment.GetXml: String;
var
	aChild: TXmlDNode;
begin
	Result := '';
	aChild := FirstChild;
	while Assigned(aChild) do begin
		Result := Result + aChild.Xml;
		aChild := aChild.NextSibling;
	end;
end;

function TXmlDDocumentFragment.Clone: TXmlDNode;
begin
	Result := TXmlDDocumentFragment.Create;
end;

{ TXmlDCharacterData }

constructor TXmlDCharacterData.Create(aNodeType: TXmlDNodeType; const aData: String);
begin
	inherited Create(aNodeType);
	FData := aData;
end;

function TXmlDCharacterData.GetCount: Integer;
begin
	Result := Length(Data);
end;

function TXmlDCharacterData.GetNodeValue: String;
begin
	Result := Data;
end;

procedure TXmlDCharacterData.SetNodeValue(const aValue: String);
begin
	Data := aValue;
end;

function TXmlDCharacterData.GetText: String;
begin
	Result := NodeValue;
end;

procedure TXmlDCharacterData.SetText(const aText: String);
begin
	NodeValue := aText;
end;

function TXmlDCharacterData.SubstringData(anOffset, aCount: Integer): String;
begin
	Result := Copy(Data, anOffset + 1, aCount);
end;

procedure TXmlDCharacterData.AppendData(const aData: String);
begin
	Data := Data + aData;
end;

procedure TXmlDCharacterData.InsertData(anOffset: Integer; const aData: String);
var
	aLen, anAddLen: Integer;
begin
	aLen := Length(FData);
	anAddLen := Length(aData);
	SetLength(FData, aLen + anAddLen);
	Move(FData[anOffset + 1], FData[anOffset + anAddLen + 1], aLen - anOffset);
	Move(Pointer(aData)^, FData[anOffset + 1], anAddLen);
end;

procedure TXmlDCharacterData.DeleteData(anOffset, aCount: Integer);
begin
	Delete(FData, anOffset + 1, aCount);
end;

procedure TXmlDCharacterData.ReplaceData(anOffset, aCount: Integer; const aData: String);
begin
	DeleteData(anOffset, aCount);
	InsertData(anOffset, aData);
end;

{ TXmlDComment }

constructor TXmlDComment.Create(const aData: String);
begin
	inherited Create(XMLDNODE_COMMENT, aData);
	NodeNameChanged;
end;

function TXmlDComment.InternalGetNodeName: String;
begin
	Result := '#comment';
end;

function TXmlDComment.GetXml: String;
begin
	Result := '<!--' + Data + '-->';
end;

function TXmlDComment.Clone: TXmlDNode;
begin
	Result := TXmlDComment.Create(Data);
end;

{ TXmlDText }

constructor TXmlDText.Create(aNodeType: TXmlDNodeType; const aData: String);
begin
	inherited Create(aNodeType, aData);
end;

constructor TXmlDText.Create(const aData: String);
begin
	Create(XMLDNODE_TEXT, aData);
	NodeNameChanged;
end;

function TXmlDText.InternalGetNodeName: String;
begin
	Result := '#text';
end;

function TXmlDText.GetXml: String;
begin
	Result := EncodeHtmlString(Data);
end;

function TXmlDText.Clone: TXmlDNode;
begin
	Result := TXmlDText.Create(Data);
end;

function TXmlDText.SplitText(anOffset: Longint): TXmlDText;
begin
	CheckParentNode;
	RO_Init(Result, TXmlDText.Create(SubstringData(anOffset, Count - anOffset)));
	try
		ParentNode.InsertChild(Result, ParentNode.IndexOfChild(Self) + 1);
		DeleteData(0, anOffset + 1);
	finally
		RO_Release(Result);
	end;
end;

{ TXmlDCDATASection }

constructor TXmlDCDATASection.Create(const aData: String);
begin
	inherited Create(XMLDNODE_CDATA_SECTION, aData);
	NodeNameChanged;
end;

function TXmlDCDATASection.InternalGetNodeName: String;
begin
	Result := '#cdata-section';
end;

function TXmlDCDATASection.GetXml: String;
begin
	Result := '<![CDATA[' + Data + ']]>';
end;

function TXmlDCDATASection.Clone: TXmlDNode;
begin
	Result := TXmlDCDATASection.Create(Data);
end;

{ TXmlDProcessingInstruction }

constructor TXmlDProcessingInstruction.Create(const aTarget, aData: String);
begin
	inherited Create(XMLDNODE_PROCESSING_INSTRUCTION);
	SetTarget(aTarget);
	FData := aData;
end;

procedure TXmlDProcessingInstruction.SetTarget(const aTarget: String);
begin
	FTarget := aTarget;
	NodeNameChanged;
end;

function TXmlDProcessingInstruction.InternalGetNodeName: String;
begin
	Result := Target;
end;

function TXmlDProcessingInstruction.GetNodeValue: String;
begin
	Result := Data;
end;

procedure TXmlDProcessingInstruction.SetNodeValue(const aValue: String);
begin
	Data := aValue;
end;

function TXmlDProcessingInstruction.GetText: String;
begin
	Result := NodeValue;
end;

procedure TXmlDProcessingInstruction.SetText(const aText: String);
begin
	NodeValue := aText;
end;

function TXmlDProcessingInstruction.GetXml: String;
begin
	Result := '<?' + Target + ' ' + Data + '?>'; //!!!Attention. Non-encoding.
end;

function TXmlDProcessingInstruction.Clone: TXmlDNode;
begin
	Result := TXmlDProcessingInstruction.Create(Target, Data);
end;

{ TXmlDEntityReference }

constructor TXmlDEntityReference.Create(const aName: String);
begin
	inherited Create(XMLDNODE_ENTITY_REFERENCE);
	SetName(aName);
end;

procedure TXmlDEntityReference.SetName(const aName: String);
begin
	FName := aName;
	NodeNameChanged;
end;

function TXmlDEntityReference.InternalGetNodeName: String;
begin
	Result := Name;
end;

function TXmlDEntityReference.GetNodeValue: String;
begin
	Result := '';
end;

procedure TXmlDEntityReference.SetNodeValue(const aValue: String);
begin
	raise Exception.Create('XML: Нельзя изменять значение узла "' +
			NodeTypeString + '"');
end;

function TXmlDEntityReference.GetText: String;
begin
	Result := ''; //!!!
end;

procedure TXmlDEntityReference.SetText(const aText: String);
begin
	raise Exception.Create('XML: Нельзя изменять текст узла "' +
			NodeTypeString + '"');
end;

function TXmlDEntityReference.GetXml: String;
begin
	Result := ''; //!!!
end;

function TXmlDEntityReference.Clone: TXmlDNode;
begin
	Result := TXmlDEntityReference.Create(Name);
end;

{ TXmlDEntity }

constructor TXmlDEntity.Create;
begin
	inherited Create(XMLDNODE_ENTITY);
	NodeNameChanged;
end;

procedure TXmlDEntity.SetNotationName(const aName: String);
begin
	FNotationName := aName;
	NodeNameChanged; //???
end;

function TXmlDEntity.InternalGetNodeName: String;
begin
	Result := NotationName; //???
end;

function TXmlDEntity.GetNodeValue: String;
begin
	Result := '';
end;

procedure TXmlDEntity.SetNodeValue(const aValue: String);
begin
	raise Exception.Create('XML: Нельзя изменять значение узла "' +
			NodeTypeString + '"');
end;

function TXmlDEntity.GetText: String;
begin
	Result := ''; //!!!
end;

procedure TXmlDEntity.SetText(const aText: String);
begin
	raise Exception.Create('XML: Нельзя изменять текст узла "' +
			NodeTypeString + '"');
end;

function TXmlDEntity.GetXml: String;
begin
	Result := ''; //!!!
end;

function TXmlDEntity.Clone: TXmlDNode;
var
	aClone: TXmlDEntity;
begin
	aClone := TXmlDEntity.Create;
	aClone.FPublicId := PublicId;
	aClone.FSystemId := SystemId;
	aClone.SetNotationName(NotationName);
	Result := aClone;
end;

{ TXmlDNotation }

constructor TXmlDNotation.Create;
begin
	inherited Create(XMLDNODE_NOTATION);
	NodeNameChanged;
end;

function TXmlDNotation.InternalGetNodeName: String;
begin
	Result := ''; //!!!
end;

function TXmlDNotation.GetNodeValue: String;
begin
	Result := '';
end;

procedure TXmlDNotation.SetNodeValue(const aValue: String);
begin
	raise Exception.Create('XML: Нельзя изменять значение узла "' +
			NodeTypeString + '"');
end;

function TXmlDNotation.GetText: String;
begin
	Result := '';
end;

procedure TXmlDNotation.SetText(const aText: String);
begin
	raise Exception.Create('XML: Нельзя изменять текст узла "' +
			NodeTypeString + '"');
end;

function TXmlDNotation.GetXml: String;
begin
	Result := ''; //!!!
end;

function TXmlDNotation.Clone: TXmlDNode;
var
	aClone: TXmlDNotation;
begin
	aClone := TXmlDNotation.Create;
	aClone.FPublicId := PublicId;
	aClone.FSystemId := SystemId;
	Result := aClone;
end;

end.

