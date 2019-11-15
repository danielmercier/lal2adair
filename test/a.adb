procedure A is
   function F1 (X1, X2 : Integer := 1; X3 : Integer := 3) return Integer;
   function F2 (X1 : Integer := 1; X2 : Integer := 2; X3, X4 : Integer := 4) return Integer;

   type F1Ptr is access function (X1, X2 : Integer; X3 : Integer := 3) return Integer;
   type F2Ptr is access function (X1, X2 : Integer; X3, X4 : Integer := 3) return Integer;

   X : Integer;

   P1 : constant F1Ptr := F1'Access;
   P2 : constant F2Ptr := F2'Access;
begin
   X := F1 (X2 => 2, X3 => 3, X1 => 1);
   X := F1 (1, 2, 3);
   X := F1 (1, X3 => 3, X2 => 2);
   X := F1;
   X := F1 (1, 2 X3 => 3);
   X := F1 (1);
   X := F1 (X2 => 2);

   X := F2 (X3 => 3);
   X := F2;
end A;
