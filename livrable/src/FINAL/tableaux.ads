-- Spécification du module Tableaux.

generic

    type T_Element is private;
    Capacity : Integer;

package tableaux is

    type T_Tableau is private;

    -- Initialiser un tableau
    --
    -- Paramètre:
    --		Tableau: le tableau à initialiser
    --
    -- Assure:
    --		Tableau est vide (taille = 0)
    procedure Initialize(Tableau : out T_Tableau) with
	    Post => Get_Size(Tableau) = 0;


    -- Obtenir le nombre d'élément dans le tableau
    --
    -- Retourne:
    --		Integer: nombre d'élément dans le tableau
    --
    -- Paramètre:
    --		Tableau
    function Get_Size(Tableau: in T_Tableau) return Integer;


    -- Obtenir l'élément à la i-ème place
    --
    -- Paramètres:
    --		Tableau
    --		Index: indice de l'élément situé dans le tableau
    --
    -- Retourne:
    --		T_Element: l'élément à la i-ème place
    --
    -- Necessite:
    --		Index compris entre 1 et Get_Size(Tableau)
    function Get_Element(Tableau: in T_Tableau; Index : in Integer) return T_Element with
	    Pre => Index >= 1 and Index <= Get_Size(Tableau);


    -- Modifier un élément du tableau à la i-ème place
    --
    -- Paramètres:
    --		Tableau
    --		Index: indice de l'élément que l'on souhaite modifier
    --		Element: élément qu'on souhaite mettre dans le tableau
    --
    -- Necessite:
    --		Index compris entre 1 et Get_Size(Tableau)
    --
    -- Assure:
    --		Tableau.Elements(Index) = Element
    procedure Set_Element(Tableau: in out T_Tableau; Index : in Integer; Element : in T_Element) with
	    Pre => Index >= 1 and Index <= Get_Size(Tableau),
	    Post => Get_Element(Tableau, Index) = Element;


    -- Ajouter un élément en fin de tableau
    --
    -- Paramètres:
    --		Tableau
    --		Element: l'élément que l'on souhaite ajouter
    --
    -- Necessite:
    --		Taille du tableau < Capacité maximale du tableau
    --
    -- Assure:
    --		Tableau.Elements(Tableau.Size) = Element
    procedure Add(Tableau: in out T_Tableau; Element : T_Element) with
	    Pre => Get_Size(Tableau) < Capacity,
	    Post => Get_Element(Tableau, Get_Size(Tableau)) = Element;


    -- Vérifie si le tableau contient un élément donné
    --
    -- Paramètres:
    --		Tableau
    --		Element: l'élément que l'on souhaite comparé à ceux dans le tableau
    --
    -- Retourne:
    --		Boolean: vrai si l'élément est dans le tableau
    --
    -- Necessite:
    --		Le tableau doit être initialisé
    function Contains(Tableau: in T_Tableau; Element: T_Element) return Boolean with
	    Pre => Get_Size(Tableau) >= 0;


    -- Inserer un élément dans le tableau à la i-ème place
    --
    -- Paramètres:
    --		Tableau
    --		Index: indice où l'on veut insérer
    --		Element: l'élément que l'on veut insérer
    --
    -- Necessite:
    --		Index compris entre 1 et Get_Size(Tableau)
    --		Get_Size(Tableau) < Capacité maximale du tableau
    --
    -- Assure:
    --		Tableau.Elements(Index) = Element
    --		Tableau.Size = Tableau'Old.Size + 1
    procedure Insert(Tableau : in out T_Tableau; Index : Integer; Element : T_Element) with
	    Pre => Index >= 1 and Index <= Get_Size(Tableau) and Get_Size(Tableau) < Capacity,
	    Post => Get_Element(Tableau, Index) = Element and Get_Size(Tableau) =  Get_Size(Tableau'Old) + 1;


    -- Supprimer un élément du tableau à un indice donné
    --
    -- Paramètres:
    --		Tableau
    --		Index: indice de l'élément que l'on veut supprimer
    --
    -- Necessite:
    -- 		Index compris entre 1 et Get_Size(Tableau)
    --
    -- Assure:
    --		Tableau.Size = Tableau'Old.Size - 1
    procedure Delete(Tableau : in out T_Tableau; Index : Integer) with
	    Pre => Index >= 1 and Index <= Get_Size(Tableau),
	    Post => Get_Size(Tableau) = Get_Size(Tableau'Old) - 1;


    -- Supprimer toutes les occurences d'un élément dans un tableau
    --
    -- Paramètres:
    --		Tableau
    --		Element: l'élément dont on veut supprimer toutes les occurences
    --
    -- Necessite:
    --		Le tableau doit être initialisé
    --
    -- Assure:
    --		not Contains(Tableau, Element)
    procedure Delete_All(Tableau : in out T_Tableau; Element : T_Element) with
	    Pre => Get_Size(Tableau) >= 0,
	    Post => not Contains(Tableau, Element);


    -- Echanger Tab(i) et T(j) dans le tableau
    --
    -- Paramètres:
    --            Tableau
    --            i, j: indices des éléments
    --
    -- Necessite:
    --            i et j doivent être compris entre 1 et la taille du tableau
    procedure Exchange(Tab : in out T_Tableau; i, j : in Integer) with
	    Pre => 1 <= i and i <= Get_Size(Tab) and 1 <= j and j <= Get_Size(Tab);


    -- Trier le tableau via un tri rapide
    --
    -- Generique:
    --            Comparer les éléments a et b afin d'éviter les conflits de types
    --
    -- Paramètres:
    --            Tableau
    --
    -- Assure:
    --            Le tableau est trié dans l'ordre croissant.
    generic
	with function Compare(a, b : T_Element) return Boolean;
    procedure Sort(Tab : in out T_Tableau);

private

    type T_Tab_Element is array (1..Capacity) of T_Element;
    type T_Tableau is record
	Size : Integer;
	Elements : T_Tab_Element;
    end record;

end tableaux;
