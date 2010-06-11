
with Ada.Text_IO ;
use Ada.Text_IO ;
with Ada.Integer_Text_IO ;
use Ada.Integer_Text_IO ;
with Generic_Binary_Tree ;


procedure Binary_Main is

   procedure My_Visit (
         V : in     Integer) is
   begin
      Ada.Integer_Text_IO.Put (Item => V) ;
      Ada.Text_IO.New_Line ;
   end My_Visit ;

   package Integer_Binary_Tree is new Generic_Binary_Tree (
         Item    => Integer,
         Compare => "<",
         Visit   => My_Visit) ;

   F : File_Type;
   -- Used to hold the value read from the input file:
   Value : Integer;
   -- Used to hold the length of the input filename:
   --Length : Natural;
   --Name   : String (1 .. 25);
   T      : Integer_Binary_Tree.Tree;


begin

   --Put (Item => "Input Filename:") ;
   --Get_Line (
   --   Item => Name,
   --   Last => Length) ;
   Open (
      File => F,
      Mode => In_File,
      Name => "test.dat") ;
      --Name => Name (1 .. Length)) ;

   -- Read all values from the input and insert them into the BST
   while not End_Of_File (File => F) loop
      Get (
         File => F,
         Item => Value) ;
      Integer_Binary_Tree.Insert (
         T => T,
         V => Value) ;
   end loop ;

   -- Do a in-order traverse of the binary search tree.
   Integer_Binary_Tree.In_Order_Traverse (T => T) ;

   Reset (File => F) ;

   while not End_Of_File (File => F) loop
      Get (
         File => F,
         Item => Value) ;
      Integer_Binary_Tree.Delete (
         V => Value,
         T => T) ;
      Integer_Binary_Tree.In_Order_Traverse (T => T) ;
      Ada.Text_IO.New_Line;
   end loop ;

   Close (File => F);

end Binary_Main ;

