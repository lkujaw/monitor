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
--  File:          monitor.ads (Ada Package Specification)           --
--  Language:      Ada (1995) [1]                                    --
--  Author:        Lev Kujawski                                      --
--  Description:   Behavior-Driven Development (BDD)-style testing   --
--                 framework                                         --
--                                                                   --
--  References:                                                      --
--  [1] Information technology - Programming languages - Ada,        --
--      ISO/IEC 8652:1995(E), 15 Feb. 1995.                          --
-----------------------------------------------------------------------

with GNAT.Source_Info;
with System;

package Monitor is

   type Systems_T is limited private;

   procedure Null_Callback;
   type Callback_T is access procedure;

   procedure Register_System
     (To_Systems    : in out Systems_T;
      With_Name     : in     String;
      With_Setup    : in     Callback_T := Null_Callback'Access;
      With_Teardown : in     Callback_T := Null_Callback'Access);

   procedure Register
     (Of_The_System     : in out Systems_T;
      The_Specification : in     Callback_T;
      With_Description  : in     String := "");

   procedure Validate
     (The_Systems : in out Systems_T);

   --  TODO: procedure Clear to free memory

   --  The below should be considered behavioral aspects of a
   --  specification.

   --  BOOLEAN CONSTRAINTS --------------------------------------------

   type Boolean_Constraint_T (<>) is limited private;

   function Is_True
      return Boolean_Constraint_T;
   function Is_False
      return Boolean_Constraint_T;

   --  INTEGER CONSTRAINTS --------------------------------------------

   type Integer_T is range System.Min_Int .. System.Max_Int;

   type Integer_Constraint_T (<>) is limited private;

   function Is_Equal_To
     (The_Value : in Integer_T)
      return Integer_Constraint_T;

   function Is_Not_Equal_To
     (The_Value : in Integer_T)
      return Integer_Constraint_T;

   --  FLOAT CONSTRAINTS ----------------------------------------------

   type Float_T is digits System.Max_Digits;

   type Float_Constraint_T (<>) is limited private;

   function Is_Equal_To
     (The_Value : in Float_T)
      return Float_Constraint_T;

   function Is_Not_Equal_To
     (The_Value : in Float_T)
      return Float_Constraint_T;

   --  ADDRESS CONSTRAINTS --------------------------------------------

   type Address_Constraint_T (<>) is limited private;

   function Is_Null     return Address_Constraint_T;
   function Is_Not_Null return Address_Constraint_T;

   --  STRING CONSTRAINTS ---------------------------------------------

   type String_Constraint_T (<>) is limited private;

   function Is_Empty     return String_Constraint_T;
   function Is_Not_Empty return String_Constraint_T;

   function Is_Equal_To
     (The_String : in String)
      return String_Constraint_T;

   function Is_Not_Equal_To
     (The_String : in String)
      return String_Constraint_T;

   --  ASSERTIONS -----------------------------------------------------

   function Source_File
      return String renames GNAT.Source_Info.File;

   function Source_Line
      return Natural renames GNAT.Source_Info.Line;

   procedure Assert_That
     (This      : in Boolean;
      Satisfies : in Boolean_Constraint_T;
      Has_Image : in String  := "";
      From_File : in String  := Source_File;
      From_Line : in Natural := Source_Line);

   procedure Assert_That
     (This      : in Integer_T;
      Satisfies : in Integer_Constraint_T;
      Has_Image : in String  := "";
      From_File : in String  := Source_File;
      From_Line : in Natural := Source_Line);

   procedure Assert_That
     (This      : in Float_T;
      Satisfies : in Float_Constraint_T;
      Has_Image : in String  := "";
      From_File : in String  := Source_File;
      From_Line : in Natural := Source_Line);

   procedure Assert_That
     (This      : in System.Address;
      Satisfies : in Address_Constraint_T;
      Has_Image : in String  := "";
      From_File : in String  := Source_File;
      From_Line : in Natural := Source_Line);

   procedure Assert_That
     (This      : in String;
      Satisfies : in String_Constraint_T;
      Has_Image : in String  := "";
      From_File : in String  := Source_File;
      From_Line : in Natural := Source_Line);

private  --  Monitor --------------------------------------------------

   type Specification_Node_T;
   type Specification_Node_A is access Specification_Node_T;

   type Specification_Node_T (Description_Length : Natural) is
      record
         Link          : Specification_Node_A := null;
         Specification : Callback_T           := null;
         Description   : String (1 .. Description_Length);
      end record;

   type System_Node_T;
   type System_Node_A is access System_Node_T;

   type System_Node_T (System_Name_Length : Positive) is
      record
         Link               : System_Node_A        := null;
         Specifications     : Natural              := 0;
         Specification_List : Specification_Node_A := null;
         Setup_Callback     : Callback_T           := null;
         Teardown_Callback  : Callback_T           := null;
         System_Name        : String (1 .. System_Name_Length);
      end record;

   type Systems_T is new System_Node_A;

   type Boolean_Constraint_T is new Boolean;

   type Integer_Constraint_Kind_T is (Equality, Inequality);

   type Integer_Constraint_T is
      record
         Kind  : Integer_Constraint_Kind_T;
         Value : Integer_T;
      end record;

   type Float_Constraint_Kind_T is (Equality, Inequality);

   type Float_Constraint_T is
      record
         Kind  : Float_Constraint_Kind_T;
         Value : Float_T;
      end record;

   type Address_Constraint_Kind_T is (Equality, Inequality);

   type Address_Constraint_T is
      record
         Kind  : Address_Constraint_Kind_T;
         Value : System.Address;
      end record;

   type String_Constraint_Kind_T is (Equality, Inequality);

   type String_Constraint_T (Last : Natural) is
      record
         Kind  : String_Constraint_Kind_T;
         Value : String (1 .. Last);
      end record;

end Monitor;
