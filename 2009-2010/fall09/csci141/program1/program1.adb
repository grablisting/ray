WITH Ada.Text_IO;
WITH Ada.Integer_Text_IO;

PROCEDURE Program1 IS

   TYPE WeekDay IS (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday);

   PACKAGE Date_IO IS NEW Ada.Text_IO.Enumeration_IO(Enum => WeekDay);

   -- Subtypes provide constraints for user inputs
   SUBTYPE ValidYears IS Integer RANGE 1801..2199;
   SUBTYPE ValidMonths IS Integer RANGE 1..12;
   SUBTYPE ValidDays IS Integer RANGE 1..31;

   Year : ValidYears;
   Month : ValidMonths;
   Day : ValidDays;
   C, L, C1, Leap, Adj, M, D : Integer;
   Result : WeekDay;

BEGIN
   -- Greeting
   Ada.Text_IO.Put(Item => "-------------------------------------");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put(Item => "WELCOME TO DATE FINDER!");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put(Item => "-------------------------------------");
   Ada.Text_IO.New_Line;

   -- Acquire data
   Ada.Text_IO.Put(Item => "Enter a YEAR (1801..2199): ");
   Ada.Integer_Text_IO.Get(Item => Year);
   Ada.Text_IO.Put(Item => "Enter a MONTH (January = 1): ");
   Ada.Integer_Text_IO.Get(Item => Month);
   Ada.Text_IO.Put(Item => "Enter the DAY (1..31): ");
   Ada.Integer_Text_IO.Get(Item => Day);

   -- Calculate the day of the week
   C := Year/100;
   L := Year/4 - (3*C + 3)/4;
   C1 := (Year - 1)/100;
   Leap := L - (Year - 1)/4 + (3*C1 + 3)/4;
   Adj := 1 - (Month + 9)/12;
   M := (13*Month + 3 + 17*Adj)/5;
   M := M - (Leap + 1)*Adj;
   D := (Year + L + M + Day + 1) rem 7;
   Result := WeekDay'Val(D);

   -- Aesthetic line break
   Ada.Text_IO.Put(Item => "-------------------------------------");
   Ada.Text_IO.New_Line;

   -- Complete task
   Ada.Text_IO.Put(Item => "The day of the week on ");
   Ada.Integer_Text_IO.Put(Item => Month, Width => 1);
   Ada.Text_IO.Put(Item => ".");
   Ada.Integer_Text_IO.Put(Item => Day, Width => 1);
   Ada.Text_IO.Put(Item => ".");
   Ada.Integer_Text_IO.Put(Item => Year, Width => 1);
   Ada.Text_IO.Put(Item => " is: ");
   Date_IO.Put(Item => Result);

   -- Last divider
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put(Item => "-------------------------------------");

END Program1;
