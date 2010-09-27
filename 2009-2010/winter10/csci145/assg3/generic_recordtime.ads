with Ada.Real_Time;
use Ada.Real_Time;

GENERIC
   WITH PROCEDURE SortProcedure (Source : IN OUT pMyData; Key : IN Integer);
PACKAGE RecordTime IS
   FUNCTION RecordTime (Parameter1 : pMyData; Parameter2 : Integer) RETURN Duration;
END RecordTime;
