with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.Characters.Handling;

PROCEDURE Lab1 IS
   InputFile : String := "DataFile.txt";

   SUBTYPE IDRange IS Positive RANGE 1111..9999;
   SUBTYPE NameRange IS Positive RANGE 1..20;
   SUBTYPE NameType IS String(NameRange);
   SUBTYPE NonNegFloat IS Float RANGE 0.0..Float'Last;

   TYPE PayCategories IS (Unknown, Professional, Sales, Clerical);
   PACKAGE Category_IO IS NEW Ada.Text_IO.Enumeration_IO (Enum => PayCategories);

   TYPE Employee (PayStatus : PayCategories := Unknown) IS RECORD
      ID : IDRange := 1111;
      NameLength : NameRange := 20;
      Name : NameType := "                    ";

      CASE PayStatus IS
         WHEN Professional =>
            MonthlySalary : NonNegFloat;
         WHEN Sales =>
            WeeksSalary : NonNegFloat;
            CommRate : NonNegFloat;
            SalesAmount : NonNegFloat;
         WHEN Clerical =>
            HourlyWage : NonNegFloat;
            Hours : NonNegFloat;
         WHEN Unknown =>
            NULL;
      END CASE;
   END RECORD;

   SUBTYPE ArraySize IS Natural RANGE 1..5;
   TYPE EmployeeArray IS ARRAY (ArraySize, ArraySize) OF Employee;
   Employed : EmployeeArray;

   PROCEDURE Input (DataFrom: IN String; Matrix: OUT EmployeeArray) IS
      Category : PayCategories;
      Num0, Marker, row, col : Integer := 0;
      InData : Ada.Text_IO.File_Type;
      EmployeeName : NameType;
      ID, NameLength : Integer := 0;
      MonthlySalary, WeeksSalary, CommRate, SalesAmount, HourlyWage, Hours : Float := 0.0;
   BEGIN
      Ada.Text_IO.Open (File => InData, Mode => Ada.Text_IO.In_File, Name => DataFrom);
      WHILE NOT Ada.Text_IO.End_of_File (File => InData) LOOP
         Category_IO.Get (File => InData, Item => Category);

         Ada.Integer_Text_IO.Get (File => InData, Item => ID);
         Ada.Text_IO.Skip_Line (File => InData);
         Ada.Integer_Text_IO.Get (File => InData, Item => NameLength);
         Ada.Text_IO.Skip_Line (File => InData);

         Ada.Text_IO.Get_Line (File => InData, Item => EmployeeName, Last => Num0);
         FOR Here IN (Num0 + 1)..NameRange'Last LOOP
            EmployeeName(Here) := ' ';
         END LOOP;

         Marker := Marker + 1;
         row := ((Marker - 1) / ArraySize'Last) + 1;
         col := ArraySize'Last - ((Marker - 1) REM ArraySize'Last);

         CASE Category IS
            WHEN Professional =>
               Ada.Float_Text_IO.Get (File => InData, Item => MonthlySalary);
               Ada.Text_IO.Skip_Line (File => InData);
               Matrix(row, col) := (Professional, ID, NameLength, EmployeeName, MonthlySalary);
            WHEN Sales =>
               Ada.Float_Text_IO.Get (File => InData, Item => WeeksSalary);
               Ada.Text_IO.Skip_Line (File => InData);
               Ada.Float_Text_IO.Get (File => InData, Item => CommRate);
               Ada.Text_IO.Skip_Line (File => InData);
               Ada.Float_Text_IO.Get (File => InData, Item => SalesAmount);
               Ada.Text_IO.Skip_Line (File => InData);
               Matrix(row, col) := (Sales, ID, NameLength, EmployeeName, WeeksSalary, CommRate, SalesAmount);
            WHEN Clerical =>
               Ada.Float_Text_IO.Get (File => InData, Item => HourlyWage);
               Ada.Text_IO.Skip_Line (File => InData);
               Ada.Float_Text_IO.Get (File => InData, Item => Hours);
               Ada.Text_IO.Skip_Line (File => InData);
               Matrix(row, col) := (Clerical, ID, NameLength, EmployeeName, HourlyWage, Hours);
            WHEN Unknown =>
               Matrix(row, col) := (Unknown, ID, NameLength, EmployeeName);
         END CASE;
      END LOOP;
      Ada.Text_IO.Close (File => InData);
   END Input;

   PROCEDURE Output (From: IN EmployeeArray) IS
      CurrentEmployee : Employee;
   BEGIN --Output
      FOR col IN ArraySize LOOP
         FOR row IN ArraySize LOOP
            CurrentEmployee := From(row, (ArraySize'Last - col) + 1);

            Ada.Text_IO.Put (Item => "Employee Number: ");
            Ada.Integer_Text_IO.Put (Item => CurrentEmployee.ID, Width => 1);
            Ada.Text_IO.New_Line;

            Ada.Text_IO.Put (Item => "Name Length: ");
            Ada.Integer_Text_IO.Put (Item => CurrentEmployee.NameLength, Width => 1);
            Ada.Text_IO.New_Line;

            Ada.Text_IO.Put (Item => "Employee: ");
            Ada.Text_IO.Put (Item => CurrentEmployee.Name);
            Ada.Text_IO.New_Line;

            CASE CurrentEmployee.PayStatus IS
               WHEN Professional =>
                  Ada.Text_IO.Put (Item => "Monthly Salary: ");
                  Ada.Float_Text_IO.Put (Item => CurrentEmployee.MonthlySalary, Fore => 1, Aft => 2, Exp => 0);
                  Ada.Text_IO.New_Line;
               WHEN Sales =>
                  Ada.Text_IO.Put (Item => "Weekly Salary: ");
                  Ada.Float_Text_IO.Put (Item => CurrentEmployee.WeeksSalary, Fore => 1, Aft => 2, Exp => 0);
                  Ada.Text_IO.New_Line;
                  Ada.Text_IO.Put (Item => "Commission Rate: ");
                  Ada.Float_Text_IO.Put (Item => CurrentEmployee.CommRate, Fore => 1, Aft => 2, Exp => 0);
                  Ada.Text_IO.New_Line;
                  Ada.Text_IO.Put (Item => "Sales Amount: ");
                  Ada.Float_Text_IO.Put (Item => CurrentEmployee.SalesAmount, Fore => 1, Aft => 2, Exp => 0);
                  Ada.Text_IO.New_Line;
               WHEN Clerical =>
                  Ada.Text_IO.Put (Item => "Hourly Wage: ");
                  Ada.Float_Text_IO.Put (Item => CurrentEmployee.HourlyWage, Fore => 1, Aft => 2, Exp => 0);
                  Ada.Text_IO.New_Line;
                  Ada.Text_IO.Put (Item => "Hours Worked: ");
                  Ada.Float_Text_IO.Put (Item => CurrentEmployee.Hours, Fore => 1, Aft => 2, Exp => 0);
                  Ada.Text_IO.New_Line;
               WHEN OTHERS =>
                  NULL;
            END CASE;
            Ada.Text_IO.Put (Item => "-------------------");
            Ada.Text_IO.New_Line;
         END LOOP;
      END LOOP;
   END Output;

   Farewell : String := "That's it, That's all!";
BEGIN -- Lab1
   Input(DataFrom => InputFile, Matrix => Employed);
   Output(From => Employed);

   FOR x IN 1..Farewell'Last LOOP
      DELAY 0.2;
      Ada.Text_IO.Put (Item => Farewell(x));
   END LOOP;
   Ada.Text_IO.New_Line;
END Lab1;
