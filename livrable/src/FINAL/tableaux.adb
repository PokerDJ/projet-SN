-- Implantation du module Tableaux

with Ada.Text_IO;
use Ada.Text_IO;

package body tableaux is

    procedure Initialize(Tableau : out T_Tableau) is
    begin
	Tableau.Size := 0;
    end;

    function Get_Size(Tableau : in T_Tableau) return Integer is
    begin
	return Tableau.Size;
    end;

    function Get_Element(Tableau: in T_Tableau; Index : Integer) return T_Element is
    begin
	return Tableau.Elements(Index);
    end;

    procedure Set_Element(Tableau: in out T_Tableau; Index : Integer; Element : T_Element) is
    begin
	Tableau.Elements(Index) := Element;
    end;

    procedure Add(Tableau: in out T_Tableau; Element : T_Element) is
    begin
	Tableau.Elements(Tableau.Size + 1) := Element;
	Tableau.Size := Tableau.Size + 1;
    end;

    function Contains(Tableau: in T_Tableau; Element: T_Element) return Boolean is
	Trouve : Boolean := False;
	i : Integer := 1;
    begin
	while Trouve = False and i <= Tableau.Size loop
	    if Get_Element(Tableau, i) = Element then
		Trouve := True;
	    end if;
	    i := i + 1;
	end loop;
	return Trouve;
    end;

    procedure Insert(Tableau : in out T_Tableau; Index : Integer; Element : T_Element) is
    begin
	for i in reverse Index .. Tableau.Size loop
	    Set_Element(Tableau, i+1, Get_Element(Tableau, i));
	end loop;
	Set_Element(Tableau, Index, Element);
	Tableau.Size := Tableau.Size + 1;
    end;

    procedure Delete(Tableau : in out T_Tableau; Index : Integer) is
    begin
	for i in Index .. (Tableau.Size - 1) loop
	    Set_Element(Tableau, i, Get_Element(Tableau, i+1));
	end loop;
	Tableau.Size := Tableau.Size - 1;
    end;

    procedure Delete_All(Tableau : in out T_Tableau; Element : T_Element) is
	i : Integer := 1;
    begin
	while i <= Tableau.Size loop
	    if Get_Element(Tableau, i) = Element then
		Delete(Tableau, i);
	    else
		i := i + 1;
	    end if;
	end loop;
    end;

    procedure Exchange(Tab : in out T_Tableau; i, j : in Integer) is
	c : T_Element;
    begin
	c := Get_Element(Tab, j);
	Set_Element(Tab, j, Get_Element(Tab, i));
	Set_Element(Tab, i, c);
    end;

    procedure Sort(Tab : in out T_Tableau) is

	procedure Quick(Tab : in out T_Tableau; first, last : Integer) is
	    pivot : Integer := (first + last) / 2;
	    j : Integer := first + 1;
	begin
	    if first < last then
		Exchange(Tab, first, pivot);
		pivot := first;
		for i in (first+1) .. last loop
		    if Compare(Get_Element(Tab, i), Get_Element(Tab, pivot)) then
			Exchange(Tab, i, j);
			j := j + 1;
		    end if;
		end loop;
		Exchange(Tab, pivot, j-1);
		pivot := j - 1;
		Quick(Tab, first, pivot - 1);
		Quick(Tab, pivot + 1, last);
	    end if;
	end;
    begin
	Quick(Tab, 1, Get_Size(Tab));
    end;


end tableaux;
