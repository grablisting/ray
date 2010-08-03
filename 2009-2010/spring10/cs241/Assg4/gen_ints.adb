with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

procedure gen_ints is

	package Integer_Random is new Ada.Numerics.Discrete_Random(Integer);
	use Integer_Random;

	type Int_Array_Type is Array(Integer range <>) of Boolean;
	type Int_Array is access Int_Array_Type;

	num_ints : Natural;
	next     : Natural;
	ints     : Int_Array;
	g        : Generator;

begin

	Reset(g);

	get(num_ints);

	ints := new Int_Array_Type(1..num_ints);
	ints.all := (Others => false);

	for i in ints'range loop
		loop
			next := (Random(g) mod num_ints) + 1;
			if(not ints(next)) then
				ints(next) := true;
				put(next);
				new_line;
				exit;
			end if;
		end loop;
	end loop;



end gen_ints;
