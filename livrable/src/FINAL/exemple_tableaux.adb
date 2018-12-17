with tableaux;
with Ada.Text_IO;		use Ada.Text_IO;
with Ada.Integer_Text_IO;	use Ada.Integer_Text_IO;


procedure exemple_tableaux is

    procedure Tableau_Entier is

	package Tab_Entier is
		new tableaux(Integer, 10);

	use Tab_Entier;

	procedure Afficher(Tableau: T_Tableau) is
	begin
	    Put("[");
	    if Get_Size(Tableau) /= 0 then
		Put(Get_Element(Tableau, 1), 1);
		for i in 2 .. Get_Size(Tableau) loop
		    Put(", ");
		    Put(Get_Element(Tableau, i), 1);
		end loop;
	    end if;
	    Put("]");
	    New_Line;
	end;

	Tab : T_Tableau;
	a : Boolean;

    begin

	Initialize(Tab);

	if Contains(Tab, -5) then
	    Put_Line("Présent");
	else
	    Put_Line("Non présent");
	end if;

	Add(Tab, 5);
	Add(Tab, 49);
	Add(Tab, 20);
	Add(Tab, 0);
	Add(Tab, -2);
	Add(Tab, 5);
	Add(Tab, 1);
	Add(Tab, 0);
	Add(Tab, 0);
	Add(Tab, -5);
	Afficher(Tab);
	Put(Get_Size(Tab), 1);
	New_Line;
	Put(Get_Element(Tab, 2), 1);
	New_Line;
	Afficher(Tab);
	Set_Element(Tab, 2, 5);
	Afficher(Tab);

	if Contains(Tab, -5) then
	    Put_Line("Présent");
	else
	    Put_Line("Non présent");
	end if;

	a := Contains(Tab, 50);
	if a then
	    Put_Line("Présent");
	else
	    Put_Line("Non présent");
	end if;

	Delete(Tab, 1);
	Afficher(Tab);
	Put(Get_Size(Tab), 1);
	New_Line;

	Delete_All(Tab, 0);
	Afficher(Tab);
	Put(Get_Size(Tab), 1);
	New_Line;

	Insert(Tab, 1, 30);
	Afficher(Tab);
	Put(Get_Size(Tab), 1);
	New_Line;

	Insert(Tab, 7, 30);
	Afficher(Tab);
	Put(Get_Size(Tab), 1);
	New_Line;

	Insert(Tab, 4, 44);
	Afficher(Tab);
	Put(Get_Size(Tab), 1);
	New_Line;

    end;

    procedure Tableau_Character is

	package Tab_Character is
		new tableaux(Character, 20);

	use Tab_Character;


	Tab : T_Tableau;
    begin

	Initialize(Tab);

	if Contains(Tab, 'L') then
	    Put_Line("Présent");
	else
	    Put_Line("Non présent");
	end if;

	Add(Tab, 'A');
	Add(Tab, 'B');
	Add(Tab, 'C');
	Add(Tab, 'D');
	Add(Tab, 'E');
	Add(Tab, 'F');
	Add(Tab, 'Z');
	Set_Element(Tab, 2, 'Z');

	if Contains(Tab, 'Z') then
	    Put_Line("Présent");
	else
	    Put_Line("Non présent");
	end if;

	Delete(Tab, 1);
	Delete_All(Tab, 'Z');
	Insert(Tab, 1, 'Y');
	Insert(Tab, 2, 'Y');

    end;


begin

    Tableau_Entier;
    Tableau_Character;

end;
