package tree is
    
    type T_Tree is private;
    type T_Bit is mod 2;

    
    -- Initialiser un arbre
    --
    -- Paramètre:
    --		tree: l'arbre à initialiser
    --
    -- Assure: 
    --          l'arbre est vide pointeur = null 
    procedure Initialize(Tree : out T_Tree);
    
    
    -- Renvoie le caractere de la la feuille pointée par l'arbre 
    --
    -- Paramètre:
    --		tree: l'arbre dont on souhaite connaitre le caractere de la la feuille pointée
    -- Retourne : 
    --          charactere
    -- Necessite : 
    --          l'arbre ne soit pas vide et le caractere soit definie 
    function Get_Character(Tree: in T_Tree) return Character with
	    Pre => not is_Empty(Tree);
     
    
     -- Incrémente l'occurence de la feuille pointée par l'arbre  
     --
     -- Paramètre:
     --		tree: l'arbre dont on souhaite incremente la feuille pointée
     --
     -- Nécessite : 
     --          l'arbre ne soit pas vide 
     -- Assure :
     --          occurence apres apelle = ocurence avant appelle +1  
    procedure Add_Occurence(Tree : in out T_Tree) with
	    Pre => not is_Empty(Tree);   
    
    
     -- Renvoie l'occurence de la la feuille pointée par l'arbre 
     --
     -- Paramètre:
     --		tree: l'arbre dont on souhaite connaitre l'occurence de la la feuille pointée
     -- Retourne : 
     --           integer
     -- Nécessite : 
     --          l'arbre ne soit pas vide 
     -- Assure :
     --          entier renvoyer >= 0 
    function Get_Occurence(Tree : in T_Tree) return Integer;
    
    
    -- Renvoie le byte de la la feuille pointée par l'arbre 
    --
    -- Paramètre:
    --		tree: l'arbre dont on souhaite connaitre le byte de la la feuille pointée
    -- Retourne : 
    --          T_Bit
    -- Nécessite : 
    --          l'arbre ne soit pas vide 
    function Get_Bit(Tree : in T_Tree) return T_Bit;
    
    
    -- Renvoie le pointeur de l'enfant de gauche  de la la feuille pointée par l'arbre 
    --
    -- Paramètre:
    --		tree: l'arbre dont on souhaite connaitre l'enfant de gauche de la la feuille pointée
    -- Retourne : 
    --          T_Tree
    -- Necessite : 
    --          l'arbre soit un noeud donc qu'il est un enfant à droite et à gauche  (gauche uniquement suffit)
    function Get_Children_Left(Tree : in T_Tree) return T_Tree with
	    Pre => Is_Node(Tree);
    
    
    -- Renvoie le pointeur de l'enfant de droite  de la la feuille pointée par l'arbre 
    --
    -- Paramètre:
    --		tree: l'arbre dont on souhaite connaitre l'enfant de droite de la la feuille pointée
    -- Retourne : 
    --          T_Tree
    -- Nécessite : 
    --          l'arbre soit un noeud donc qu'il est un enfant à droite et à gauche (droite uniquement suffit)
    function Get_Children_Right(Tree : in T_Tree) return T_Tree with
	    Pre => Is_Node(Tree);
    
    
    -- Indique si l'arbre est vide 
    --
    -- Paramètre:
    --		tree: l'arbre dont on souhaite savoir si il est vide 
    -- Retourne : 
    --        boolean 
    function Is_Empty(Tree : in T_Tree) return Boolean;

    
     -- Indique si l'arbre est un noeud 
     --
     -- Paramètre:
     --		tree: l'arbre dont on souhaite savoir si il est un noeud
     -- Retourne : 
     --        boolean 
    function Is_Node(Tree : in T_Tree) return Boolean;

     -- Renvoie un arbre dont char vaut c  
     --
     -- Paramètre:
     --               c  : charactere la valeur de charactere souhaité
     -- Retourne : 
     --          T_Tree
     -- Assure :
     --         arbre renvoyé pas vide et que le caractere de l'arbre est bien c 
    function Create_Leaf(c : Character) return T_Tree;
    
    
    -- Lie deux arbres   
    --
    -- Paramètre:
    --		tree_left : l'arbre que l'on souhaite ajouter à gauche
    --		tree_right : l'arbre que l'on souhaite ajouter à droite
    --           
    -- Retourne : 
    --          T_Tree
    -- Assure :
    --         l'arbre droit et bien  l'enfant droit de la feuille de l'arbre renvoyée
    --         l'arbre gauche et bien  l'enfant gauche de la feuille de l'arbre renvoyée
    function Bind(Tree_Left : in out T_Tree; Tree_Right : in out T_Tree) return T_Tree with
	    Pre => not Is_Empty(Tree_Left) and not is_Empty(Tree_Right);
    
    
    -- Supprime proprement l'arbre    
    --
    -- Paramètre:
    --		tree : l'arbre à supprimer 
    -- Assure : 
    -- 	        l'arbre est vide  
    procedure Clean_Up(Tree : in out T_Tree);

    
private

    type T_Leaf is record
	char : Character;
	occurrence : Integer;
	bit : T_Bit;
	children_left : T_Tree;
	children_right : T_Tree;
    end record;
    type T_Tree is access T_Leaf;

end tree;
