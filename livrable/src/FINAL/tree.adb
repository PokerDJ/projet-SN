with Ada.Unchecked_Deallocation;

package body tree is

    procedure Free is
	    new Ada.Unchecked_Deallocation (T_Leaf, T_Tree);

    -- Initialiser
    procedure Initialize(Tree : out T_Tree) is
    begin
	Tree := null;
    end;

    -- Get caracter
    function Get_Character(Tree: in T_Tree) return Character is
    begin
	return Tree.all.char;
    end;


    -- Ajouter une occurence
    procedure Add_Occurence(Tree : in out T_Tree) is
    begin
	Tree.all.occurrence := Tree.all.occurrence + 1;
    end;


    function Get_Occurence(Tree : in T_Tree) return Integer is
    begin
	return Tree.all.occurrence;
    end;


    function Get_Bit(Tree : in T_Tree) return T_Bit is
    begin
	return Tree.all.bit;
    end;


    function Get_Children_Left(Tree : in T_Tree) return T_Tree is
    begin
	return Tree.all.children_left;
    end;

    function Get_Children_Right(Tree : in T_Tree) return T_Tree is
    begin
	return Tree.all.children_right;
    end;


    -- Est-il vide ?
    function Is_Empty(Tree : in T_Tree) return boolean is
    begin
	return Tree = null;
    end;


    -- Est-ce un noeud ?
    function Is_Node(Tree : in T_Tree) return boolean is
    begin
	return Tree.all.children_left /= null and Tree.all.children_right /= null;
    end;


    function Create_Leaf(c : Character) return T_Tree is
	Leaf : T_Tree;
    begin
	Leaf := new T_Leaf'(c, 0, 0, null, null);
	return Leaf;
    end;


    -- Lier deux arbres
    function Bind(Tree_Left : in out T_Tree; Tree_Right : in out T_Tree) return T_Tree is
	Node : T_Tree;
    begin
	Node := new T_Leaf;
	Node.all.occurrence := Tree_Left.all.occurrence + Tree_Right.all.occurrence;
	Tree_Left.all.bit := 0;
	Tree_Right.all.bit := 1;
	Node.all.children_left := Tree_Left;
	Node.all.children_right := Tree_Right;
	return Node;
    end;

    procedure Clean_Up(Tree : in out T_Tree) is
    begin
	if not Is_Empty(Tree.all.children_left) then
	    Clean_Up(Tree.all.children_left);
	end if;
	if not Is_Empty(Tree.all.children_right) then
	    Clean_Up(Tree.all.children_right);
	end if;
	Free(Tree);
    end;



end tree;
