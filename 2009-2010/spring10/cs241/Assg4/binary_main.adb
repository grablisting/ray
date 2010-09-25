
with Ada.Text_IO ;
use Ada.Text_IO ;
with Ada.Integer_Text_IO ;
use Ada.Integer_Text_IO ;
with Ada.Float_Text_IO ;
use Ada.Float_Text_IO ;
with Generic_Binary_Tree ;


procedure Binary_Main is

   procedure My_Visit (V : in Integer) is
   begin
      Ada.Integer_Text_IO.Put (Item => V) ;
      Ada.Text_IO.New_Line ;
   end My_Visit ;

   package Integer_Binary_Tree is new Generic_Binary_Tree
      (
         Item    => Integer,
         Compare => "<",
         Visit   => My_Visit
      );

   T : Integer_Binary_Tree.Tree;
   F, Output1, Output2 : File_Type;
   Value : Integer;
   counter : Natural := 0;

   -- Used to hold the length of the input filename:
   --Length : Natural;
   --Name   : String (1 .. 25);

begin

   --Put (Item => "Input Filename:") ;
   --Get_Line (Item => Name, Last => Length) ;
   Open (File => F, Mode => In_File, Name => "test.dat") ;
   Create (File => Output1, Mode => Out_File, Name => "Output1.dat");
   Create (File => Output2, Mode => Out_File, Name => "Output2.dat");
      --Name => Name (1 .. Length)) ;

   -- Read all values from the input and insert them into the BST
   while not End_Of_File (File => F) loop
      Get (File => F, Item => Value) ;
      Integer_Binary_Tree.Insert (T => T, V => Value) ;

--        counter := counter + 1;
--
--        Put (File => Output1, Item => counter, Width => 10);
--        Put (File => Output1, Item => Integer_Binary_Tree.avgLengthToLeaves (T), Fore => 10, Aft => 2, Exp => 0);
--        Put (File => Output1, Item => Integer_Binary_Tree.minLeaf (T), Width => 10);
--        Put (File => Output1, Item => Integer_Binary_Tree.maxLeaf (T), Width => 10);
--        New_Line(File => Output1);
--
--        Put (File => Output2, Item => counter, Width => 10);
--        Put (File => Output2, Item => Integer_Binary_Tree.difMaxMinLeaves (T));
--        New_Line(File => Output2);
   end loop ;

   Close (File => Output1);
   Close (File => Output2);

   -- Do a in-order traverse of the binary search tree.
   Integer_Binary_Tree.In_Order_Traverse (T => T) ;

   Reset (File => F) ;

   while not End_Of_File (File => F) loop
      Get (File => F, Item => Value) ;
      Integer_Binary_Tree.Delete (T => T, V => Value) ;
--        counter := counter - 1;
--
--        if counter mod 20000 = 0 then
           Integer_Binary_Tree.In_Order_Traverse (T => T) ;
           New_Line;
--           New_Line;
--        end if;
   end loop ;

   Close (File => F);

end Binary_Main ;

