with tableaux;
with Ada.Text_IO;		use Ada.Text_IO;
with Ada.Integer_Text_IO;	use Ada.Integer_Text_IO;

procedure test_tableaux is

    package Tableau_Entier is
	    new tableaux(Integer, 10);
    use Tableau_Entier;

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

    procedure Tester_Add is
	Tab : T_Tableau;
    begin
	Initialize(Tab);
	Add(Tab, 2);
	pragma Assert(Get_Element(Tab, 1) = 2);
	Add(Tab, 5);
	pragma Assert(Get_Element(Tab, 2) = 5);
    end;

    procedure Tester_Contains is
	Tab : T_Tableau;
    begin
	Initialize(Tab);
	Add(Tab, 2);
	Add(Tab, 5);
	Add(Tab, -6);
	pragma Assert(Contains(Tab, -6));
	pragma Assert(not Contains(Tab, 0));
    end;

    procedure Tester_Insert is
	Tab : T_Tableau;
    begin
	Initialize(Tab);
	Add(Tab, 2);
	Add(Tab, 5);
	Add(Tab, -6);
	Insert(Tab, 2, 0);
	Insert(Tab, 4, 8);
	pragma Assert(Get_Element(Tab, 2) = 0);
	pragma Assert(Get_Element(Tab, 2) = 8);
    end;

    procedure Tester_Delete is
	Tab : T_Tableau;
    begin
	Initialize(Tab);
	Add(Tab, 2);
	Add(Tab, 5);
	Add(Tab, -6);
	pragma Assert(Contains(Tab, 5));
	Delete(Tab, 2);
	pragma Assert(not Contains(Tab, 5));
    end;

    procedure Tester_Delete_All is
	Tab : T_Tableau;
    begin
	Initialize(Tab);
	Add(Tab, 2);
	Add(Tab, 5);
	Add(Tab, -6);
	Add(Tab, 5);
	Add(Tab, 5);
	Add(Tab, 0);
	Delete_All(Tab, 5);
	pragma Assert(not Contains(Tab, 5));
    end;


    procedure Tester_Sort is
	Tab : T_Tableau;

	function Is_Inf(a, b : Integer) return Boolean is
	begin
	    return a < b;
	end;

	procedure Tri_Rapide is new Sort(Compare => Is_Inf);

    begin
	Initialize(Tab);
	Add(Tab, 2);
	Add(Tab, 5);
	Add(Tab, -6);
	Add(Tab, 3);
	Add(Tab, 10);
	Add(Tab, 1);
	Afficher(Tab);
	Tri_Rapide(Tab);
	Afficher(Tab);
    end;


begin

    Tester_Add;
    Tester_Contains;
    Tester_Insert;
    Tester_Delete;
    Tester_Delete_All;
    Tester_Sort;

end;
