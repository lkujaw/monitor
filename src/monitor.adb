-----------------------------------------------------------------------
--  Copyright 2021 Lev Kujawski                                      --
--                                                                   --
--   Permission is hereby granted, free of charge, to any person     --
--  obtaining a copy of this software and associated documentation   --
--      files (the "Software") to deal in the Software without       --
--   restriction, including without limitation the rights to use,    --
--  copy, modify, merge, publish, distribute, sublicense, and sell   --
--    copies of the Software, and to permit persons to whom the      --
--                 Software is furnished to do so.                   --
--                                                                   --
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,  --
--  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES  --
--     OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND      --
--   NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    --
--   HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,    --
--   WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    --
--  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR    --
--                 OTHER DEALINGS IN THE SOFTWARE.                   --
--                                                                   --
--  SPDX-License-Identifier: MIT-0                                   --
--                                                                   --
--  File:          monitor.adb (Ada Package Body)                    --
--  Language:      Ada (1995) [1]                                    --
--  Author:        Lev Kujawski                                      --
--  Description:   Behavior-Driven Development (BDD)-style testing   --
--                 framework                                         --
--                                                                   --
--  References:                                                      --
--  [1] Information technology - Programming languages - Ada,        --
--      ISO/IEC 8652:1995(E), 15 Feb. 1995.                          --
-----------------------------------------------------------------------

with C_Standard_IO;

with System;
use type System.Address;

package body Monitor is

   --  TODO: Rewrite I/O using Felix for I18n.

   package CIO renames C_Standard_IO;

   Assertion_Fail : exception;

   Indentation : Natural := 0;
   Is_New_Line : Boolean := True;

   function Trim
     (Source : in String)
      return String
   is
   begin
      for I in Source'Range loop
         if Source (I) /= ' ' then
            declare
               Normalized : constant String
                 (1 .. Source'Last - (I - 1)) :=
                 Source
                   (I .. Source'Last);
            begin
               return Normalized;
            end;
         end if;
      end loop;

      return "";
   end Trim;

   procedure Indent
   is
   begin
      if Is_New_Line then
         for I in 1 .. Indentation loop
            CIO.Put
              (File => CIO.Standard_Output,
               Item => ' ');
         end loop;
         Is_New_Line := False;
      end if;
   end Indent;

   procedure Put_Line
     (This : in String)
   is
   begin
      Indent;
      CIO.Put_Line
        (File => CIO.Standard_Output,
         Item => This);
      Is_New_Line := True;
   end Put_Line;

   procedure Put
     (This : in String)
   is
   begin
      Indent;
      CIO.Put
        (File => CIO.Standard_Output,
         Item => This);
   end Put;

   procedure Null_Callback
   is
   begin
      null;
   end Null_Callback;

   procedure Register_System
     (To_Systems    : in out Systems_T;
      With_Name     : in     String;
      With_Setup    : in     Callback_T := Null_Callback'Access;
      With_Teardown : in     Callback_T := Null_Callback'Access)
   is
   begin
      pragma Assert (With_Name'Length > 0);
      declare
         New_System : constant System_Node_A :=
           new System_Node_T'
             (System_Name_Length => With_Name'Length, Link => null,
              Specifications     => 0, Specification_List => null,
              Setup_Callback     => With_Setup,
              Teardown_Callback  => With_Teardown,
              System_Name        => With_Name);
      begin
         if To_Systems = null then
            New_System.all.Link := New_System;
         else
            New_System.all.Link := To_Systems.all.Link;
            To_Systems.all.Link := New_System;
         end if;
         To_Systems := Systems_T (New_System);
      end;
   end Register_System;

   procedure Register
     (Of_The_System     : in out Systems_T;
      The_Specification : in     Callback_T;
      With_Description  : in     String := "")
   is
   begin
      pragma Assert (Of_The_System /= null);
      pragma Assert (The_Specification /= null);
      pragma Assert (The_Specification /= Null_Callback'Access);
      pragma Assert (With_Description'Length >= 0);
      declare
         New_Specification : constant Specification_Node_A :=
           new Specification_Node_T'
             (Description_Length => With_Description'Length,
              Link => null, Specification => The_Specification,
              Description        => With_Description);
      begin
         if Of_The_System.all.Specification_List = null then
            New_Specification.all.Link := New_Specification;
         else
            New_Specification.all.Link :=
              Of_The_System.all.Specification_List.all.Link;
            Of_The_System.all.Specification_List.all.Link :=
              New_Specification;
         end if;
         Of_The_System.all.Specification_List := New_Specification;
         Of_The_System.all.Specifications     :=
           Of_The_System.all.Specifications + 1;
      end;
   end Register;

   procedure Validate
     (The_Systems : in out Systems_T)
   is
      Test_Number : Natural := 0;

      procedure Validate_Specifications
        (List : in Specification_Node_A)
      is
         Specification_Pointer : Specification_Node_A;
      begin
         if List = null then
            return;
         end if;
         pragma Assert (List.all.Link /= null);
         Specification_Pointer := List.all.Link;
         loop
            if Specification_Pointer.all.Description = "" then
               Put_Line
                 (">> Specification "
                  & Trim (Integer'Image (Test_Number)) & " <<");
            else
               Put_Line
                 (">> Specification "
                  & Trim (Integer'Image (Test_Number)) & " ["
                  & Specification_Pointer.all.Description & "]"
                  & " <<");
            end if;

            begin
               Indentation := Indentation + 2;
               Specification_Pointer.all.Specification.all;
               Indentation := Indentation - 2;
            exception
               when Assertion_Fail =>
                  null;
            end;
            exit when Specification_Pointer = List;
         end loop;
      end Validate_Specifications;

      System_Pointer : System_Node_A;
   begin  --  Validate
      pragma Assert (The_Systems /= null);
      pragma Assert (The_Systems.all.Link /= null);
      Indentation    := 0;
      Is_New_Line    := True;
      System_Pointer := The_Systems.all.Link;
      loop
         Test_Number := Test_Number + 1;
         Put_Line
           ("Validating """ & System_Pointer.all.System_Name & """ ("
            & Trim (Integer'Image (System_Pointer.all.Specifications))
            & " specifications)...");
         Indentation := Indentation + 2;
         Validate_Specifications
           (System_Pointer.all.Specification_List);
         Indentation    := Indentation - 2;
         System_Pointer := System_Pointer.all.Link;
         exit when System_Pointer = System_Node_A (The_Systems);
      end loop;
   end Validate;

   function Is_True
      return Boolean_Constraint_T
   is
   begin
      return True;
   end Is_True;

   function Is_False
      return Boolean_Constraint_T
   is
   begin
      return False;
   end Is_False;

   function Is_Equal_To
     (The_Value : in Integer_T)
      return Integer_Constraint_T
   is
   begin
      return
        Integer_Constraint_T'(Kind => Equality, Value => The_Value);
   end Is_Equal_To;

   function Is_Not_Equal_To
     (The_Value : in Integer_T)
      return Integer_Constraint_T
   is
   begin
      return
        Integer_Constraint_T'(Kind => Inequality, Value => The_Value);
   end Is_Not_Equal_To;

   function Is_Equal_To
     (The_Value : in Float_T)
      return Float_Constraint_T
   is
   begin
      return Float_Constraint_T'(Kind => Equality, Value => The_Value);
   end Is_Equal_To;

   function Is_Not_Equal_To
     (The_Value : in Float_T)
      return Float_Constraint_T
   is
   begin
      return
        Float_Constraint_T'(Kind => Inequality, Value => The_Value);
   end Is_Not_Equal_To;

   function Is_Null
      return Address_Constraint_T
   is
   begin
      return
        Address_Constraint_T'
          (Kind => Equality, Value => System.Null_Address);
   end Is_Null;

   function Is_Not_Null
      return Address_Constraint_T
   is
   begin
      return
        Address_Constraint_T'
          (Kind => Inequality, Value => System.Null_Address);
   end Is_Not_Null;

   function Is_Empty
      return String_Constraint_T
   is
   begin
      return
        String_Constraint_T'(Last => 0, Kind => Equality, Value => "");
   end Is_Empty;

   function Is_Not_Empty
      return String_Constraint_T
   is
   begin
      return
        String_Constraint_T'
          (Last => 0, Kind => Inequality, Value => "");
   end Is_Not_Empty;

   function Is_Equal_To
     (The_String : in String)
      return String_Constraint_T
   is
   begin
      return
        String_Constraint_T'
          (Last  => The_String'Length, Kind => Equality,
           Value => The_String);
   end Is_Equal_To;

   function Is_Not_Equal_To
     (The_String : in String)
      return String_Constraint_T
   is
   begin
      return
        String_Constraint_T'
          (Last  => The_String'Length, Kind => Inequality,
           Value => The_String);
   end Is_Not_Equal_To;

   procedure Assert_Prefix
     (From_File : in String;
      From_Line : in Natural)
   is
   begin
      if From_File /= "" and then From_Line > 0 then
         Put
           ("[" & From_File & ":"
            & Trim (Integer'Image (From_Line) & "] "));
      end if;
   end Assert_Prefix;

   procedure Pass_Fail
     (This : in Boolean)
   is
   begin
      if This then
         Put_Line ("CONFORMS");
      else
         Put_Line ("DIVERGES");
         raise Assertion_Fail;
      end if;
   end Pass_Fail;

   procedure Assert_That
     (This      : in Boolean;
      Satisfies : in Boolean_Constraint_T;
      Has_Image : in String  := "";
      From_File : in String  := Source_File;
      From_Line : in Natural := Source_Line)
   is
      function Best_Image
         return String
      is
      begin
         if Has_Image = "" then
            return "[" & Boolean'Image (This) & "]";
         else
            return "`" & Has_Image & "`";
         end if;
      end Best_Image;
   begin
      Assert_Prefix (From_File, From_Line);
      if Boolean (Satisfies) then
         Put (Best_Image & " is true: ");
      else
         Put (Best_Image & " is false: ");
      end if;
      Pass_Fail (This = Boolean (Satisfies));
   end Assert_That;

   procedure Assert_That
     (This      : in Integer_T;
      Satisfies : in Integer_Constraint_T;
      Has_Image : in String  := "";
      From_File : in String  := Source_File;
      From_Line : in Natural := Source_Line)
   is
      function Best_Image
         return String
      is
      begin
         if Has_Image = "" then
            return '[' & Trim (Integer_T'Image (This)) & ']';
         else
            return '`' & Has_Image & '`';
         end if;
      end Best_Image;
   begin
      Assert_Prefix (From_File, From_Line);
      case Satisfies.Kind is
         when Equality =>
            Put
              (Best_Image & " is equal to integer ["
               & Trim (Integer_T'Image (Satisfies.Value)) & "]: ");
            Pass_Fail (This = Satisfies.Value);
         when Inequality =>
            Put
              (Best_Image & " is not equal to integer ["
               & Trim (Integer_T'Image (Satisfies.Value)) & "]: ");
            Pass_Fail (This /= Satisfies.Value);
      end case;
   end Assert_That;

   procedure Assert_That
     (This      : in Float_T;
      Satisfies : in Float_Constraint_T;
      Has_Image : in String  := "";
      From_File : in String  := Source_File;
      From_Line : in Natural := Source_Line)
   is
      function Best_Image
         return String
      is
      begin
         if Has_Image = "" then
            return '[' & Trim (Float_T'Image (This)) & ']';
         else
            return '`' & Has_Image & '`';
         end if;
      end Best_Image;
   begin
      Assert_Prefix (From_File, From_Line);
      case Satisfies.Kind is
         when Equality =>
            Put
              (Best_Image & " is equal to float ["
               & Trim (Float_T'Image (Satisfies.Value)) & "]: ");
            Pass_Fail (This = Satisfies.Value);
         when Inequality =>
            Put
              (Best_Image & " is not equal to float ["
               & Trim (Float_T'Image (Satisfies.Value)) & "]: ");
            Pass_Fail (This /= Satisfies.Value);
      end case;
   end Assert_That;

   procedure Assert_That
     (This      : in System.Address;
      Satisfies : in Address_Constraint_T;
      Has_Image : in String  := "";
      From_File : in String  := Source_File;
      From_Line : in Natural := Source_Line)
   is
      function Best_Image
         return String
      is
      begin
         if Has_Image = "" then
            return '[' & CIO.Address_Image (This) & ']';
         else
            return '`' & Has_Image & '`';
         end if;
      end Best_Image;
   begin
      Assert_Prefix (From_File, From_Line);
      case Satisfies.Kind is
         when Equality =>
            Put
              (Best_Image & " is equal to address ["
               & CIO.Address_Image (Satisfies.Value) & "]: ");
            Pass_Fail (This = Satisfies.Value);
         when Inequality =>
            Put
              (Best_Image & " is not equal to address ["
               & CIO.Address_Image (Satisfies.Value) & "]: ");
            Pass_Fail (This /= Satisfies.Value);
      end case;
   end Assert_That;

   procedure Assert_That
     (This      : in String;
      Satisfies : in String_Constraint_T;
      Has_Image : in String  := "";
      From_File : in String  := Source_File;
      From_Line : in Natural := Source_Line)
   is
      function Best_Image
         return String
      is
      begin
         if Has_Image = "" then
            return '"' & This & '"';
         else
            return '`' & Has_Image & '`';
         end if;
      end Best_Image;
   begin
      Assert_Prefix (From_File, From_Line);
      case Satisfies.Kind is
         when Equality =>
            Put
              (Best_Image & " is equal to string """ & Satisfies.Value
               & """: ");
            Pass_Fail (This = Satisfies.Value);
         when Inequality =>
            Put
              (Best_Image & " is not equal to string """
               & Satisfies.Value & """: ");
            Pass_Fail (This /= Satisfies.Value);
      end case;
   end Assert_That;

end Monitor;
