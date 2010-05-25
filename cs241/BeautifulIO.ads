WITH Ada.Text_IO;
USE Ada.Text_IO;
WITH Ada.Strings.Unbounded;
WITH Ada.Text_IO.Unbounded_IO;
WITH Ada.Float_Text_IO;
WITH Ada.Integer_Text_IO;
WITH Ada.Long_Long_Integer_Text_IO;
WITH Ada.Long_Long_Float_Text_IO;
WITH lists_generic;

GENERIC

   LineLength : Natural;
   MaxLineLength : Natural;

PACKAGE BeautifulIO IS

   PACKAGE Str RENAMES Ada.Strings.Unbounded;
   SUBTYPE BigString IS Str.Unbounded_String;
   SUBTYPE File IS Ada.Text_IO.File_Type;


   PROCEDURE Spaces (num : Natural; To : File := Standard_Output);
   PROCEDURE Clear (To : File := Standard_Output);
   PROCEDURE Clear (num : Natural; To : File := Standard_Output);
   PROCEDURE myRepeat (Obj : String; Times : Natural; To : File := Standard_Output);
   FUNCTION BigStringify (That_Is_The_Question : String) RETURN BigString;
   FUNCTION Stringify (Or_Not_To_String : BigString) RETURN String;

   PROCEDURE PageBreak (Message : String; Here : File := Standard_Output);
   PROCEDURE PrintString (ExtraLong : Boolean; Name : String; Value : String; ExtraLineBreaks : Natural);
   PROCEDURE PrintBigString (ExtraLong : Boolean; Name : String; Value : BigString; ExtraLineBreaks : Natural);
   PROCEDURE PrintInt (ExtraLong : Boolean; Name : String; Value : Integer; ExtraLineBreaks : Natural);
   PROCEDURE PrintLongInt (ExtraLong : Boolean; Name : String; Value : Long_Long_Integer; ExtraLineBreaks : Natural);
   PROCEDURE PrintFloat (ExtraLong : Boolean; Name : String; Value : Float; AfterVal : String; ExtraLineBreaks : Natural);
   FUNCTION Percentage (Thing1, Thing2 : Integer) RETURN Float;
   FUNCTION BigPercentage (Thing1 : Long_Long_Integer; Thing2 : Integer) RETURN Float;

   FUNCTION AskForValidFileName (Title : String; OutputBool : Boolean) RETURN BigString;
   FUNCTION Ask (Question : String) RETURN Boolean;
   FUNCTION Include (Title, Location : String) RETURN BigString;
   FUNCTION Request (Title : String) RETURN BigString;

END BeautifulIO;
