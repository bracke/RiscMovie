--------------------------------------------------------------------------------
--                                                                            --
-- Copyright (C) 2004, RISC OS Ada Library (RASCAL) developers.               --
--                                                                            --
-- This library is free software; you can redistribute it and/or              --
-- modify it under the terms of the GNU Lesser General Public                 --
-- License as published by the Free Software Foundation; either               --
-- version 2.1 of the License, or (at your option) any later version.         --
--                                                                            --
-- This library is distributed in the hope that it will be useful,            --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of             --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU           --
-- Lesser General Public License for more details.                            --
--                                                                            --
-- You should have received a copy of the GNU Lesser General Public           --
-- License along with this library; if not, write to the Free Software        --
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA    --
--                                                                            --
--------------------------------------------------------------------------------

-- $Author$
-- $Date$
-- $Revision$

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Kernel;                use Kernel;
with Interfaces.C;          use Interfaces.C;
with Reporter;

with RASCAL.Utility;        use RASCAL.Utility;
with RASCAL.Memory;         use RASCAL.Memory;
with RASCAL.OS;

package body RASCAL.ToolboxScrollList is

   Toolbox_ObjectMiscOp    : constant := 16#44EC6#;
   
   ScrollList_Get_State    : constant := 16#401a# + 0;
   ScrollList_Set_State    : constant := 16#401a# + 1;
   ScrollList_Add_Item     : constant := 16#401a# + 2;
   ScrollList_Delete_Item  : constant := 16#401a# + 3;
   ScrollList_Select_Item  : constant := 16#401a# + 4;
   ScrollList_DeSelect_Item: constant := 16#401a# + 5;
   ScrollList_GetSelected  : constant := 16#401a# + 6;
   ScrollList_Make_Visible : constant := 16#401a# + 7;
   ScrollList_SetColour    : constant := 16#401a# + 8;
   ScrollList_GetColour    : constant := 16#401a# + 9;
   ScrollList_Set_Font     : constant := 16#401a# + 10;
   ScrollList_GetItemText  : constant := 16#401a# + 11;
   ScrollList_Count_Items  : constant := 16#401a# + 12;
   ScrollList_SetItemText  : constant := 16#401a# + 13;
   ScrollList_Set_Heading  : constant := 16#401a# + 14;

   --
   
   function Get_State (Window    : in Object_ID;
                       Component : in Component_ID;
                       Flags     : in System.Unsigned_Types.Unsigned := 0) return System.Unsigned_Types.Unsigned is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin

      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := ScrollList_Get_State;
      Register.R(3) := int(Component);

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxScrollList.Get_State: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      return Utility.Int_To_Unsigned(integer(Register.R(0)));
      
   end Get_State;

   --

   procedure Set_State (Window    : in Object_ID;
                        Component : in Component_ID;
                        State     : in System.Unsigned_Types.Unsigned;
                        Flags         : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin

      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := ScrollList_Set_State;
      Register.R(3) := int(Component);
      Register.R(4) := int(Utility.Unsigned_To_Int(State));

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxScrollList.Set_State: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

   end Set_State;

   --

   procedure Add_Item (Window       : in Object_ID;
                       Component    : in Component_ID;
                       Text         : in String;
                       Index        : in integer := -1;
                       Flags        : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Text_0   : String := Text & ASCII.NUL; 
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := ScrollList_Add_Item;
      Register.R(3) := int(Component);
      Register.R(4) := Adr_To_Int(Text_0'Address);
      Register.R(5) := 0;--int(Sprite_Area);
      Register.R(6) := 0;--Adr_To_Int(Sprite_Name'Address);
      Register.R(7) := int(Index);

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxScrollList.Add_Item: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Add_Item;

   --

   procedure Add_Last_Item (Window       : in Object_ID;
                            Component    : in Component_ID;
                            Text         : in String;
                            Flags        : in System.Unsigned_Types.Unsigned := 0) is
   begin
      Add_Item(Window,Component,Text,-1,Flags);
   end Add_Last_Item;

   --

   procedure Delete_Items (Window       : in Object_ID;
                           Component    : in Component_ID;
                           Start_Index  : in integer := 0;
                           End_Index    : in integer := -1;
                           Flags        : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := ScrollList_Delete_Item;
      Register.R(3) := int(Component);
      Register.R(4) := int(Start_Index);
      Register.R(5) := int(End_Index);

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxScrollList.Delete_Item: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

   end Delete_Items;

   --

   procedure Delete_All (Window       : in Object_ID;
                         Component    : in Component_ID;
                         Start_Index  : in Integer := 0) is
   begin
      Delete_Items(Window,Component,Start_Index,-1);
   end Delete_All;

   --

   procedure Select_Item (Window    : in Object_ID;
                          Component : in Component_ID;
                          Index     : in integer;
                          Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := ScrollList_Select_Item;
      Register.R(3) := int(Component);
      Register.R(4) := int(Index);

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxScrollList.Select_Item: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Select_Item;

   --

   procedure Select_Items (Window    : in Object_ID;
                           Component : in Component_ID;
                           Items     : in ItemNumber_List_Type;
                           Flags     : in System.Unsigned_Types.Unsigned := 0) is
   begin
      for i in Items'Range loop
         Select_Item (Window,Component,Items(i),Flags);
      end loop;
   end Select_Items;

   --

   procedure Select_All (Window    : in Object_ID;
                         Component : in Component_ID;
                         Start     : in Natural := 1;
                         Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Items : Natural := Count_Items (Window,Component,Flags);
   begin
      for i in Start .. (Items-1+Start) loop
         Select_Item (Window,Component,i);
      end loop;
   exception
      when others => null;
   end Select_All;

   --

   procedure DeSelect_Item (Window    : in Object_ID;
                            Component : in Component_ID;
                            Index     : in integer;
                            Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := ScrollList_DeSelect_Item;
      Register.R(3) := int(Component);
      Register.R(4) := int(Index);

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxScrollList.DeSelect_item: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end DeSelect_Item;

   --

   procedure DeSelect_Items (Window    : in Object_ID;
                             Component : in Component_ID;
                             Items     : in ItemNumber_List_Type;
                             Flags     : in System.Unsigned_Types.Unsigned := 0) is
   begin
      for i in Items'Range loop
         DeSelect_Item (Window,Component,Items(i),Flags);
      end loop;
   end DeSelect_Items;


   --

   procedure DeSelect_All (Window    : in Object_ID;
                           Component : in Component_ID;
                           Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Selections : ItemNumber_List_Type := Get_SelectionNumbers(Window,Component,Flags);
   begin
      for i in Selections'Range loop
          DeSelect_Item (Window,Component,Selections(i));
      end loop;
   end DeSelect_All;

   --

   function Get_Selected (Window    : in Object_ID;
                          Component : in Component_ID;
                          Offset    : in Integer := -1;
                          Flags     : in System.Unsigned_Types.Unsigned := 0) return integer is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := ScrollList_GetSelected;
      Register.R(3) := Int(Component);
      Register.R(4) := Int(Offset);
      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxScrollList.Get_Selected: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      return integer(Register.R(0));
   end Get_Selected;

   --

   function Count_Selections (Window    : in Object_ID;
                              Component : in Component_ID;
                              Flags     : in System.Unsigned_Types.Unsigned := 0) return Natural is

      Offset : Integer := Get_Selected(Window,Component,-1,Flags);
      Count  : Natural := 0;
   begin
      while Offset /= -1 loop
         Count := Count + 1;
         Offset := Get_Selected(Window,Component,Offset,Flags);
      end loop;
      return Count;

   end Count_Selections;

   --

   function Get_Selections (Window    : in Object_ID;
                            Component : in Component_ID;
                            Flags     : in System.Unsigned_Types.Unsigned := 0) return Item_List_Type is

      Selections : Natural := Count_Selections (Window,Component,Flags);
      Item_List  : Item_List_Type(1..Selections);

      Index      : Positive := 1;
      Offset     : Integer  := Get_Selected(Window,Component,-1,Flags);
   begin
      while Offset /= -1 loop
         Item_List (Index) := U(Get_ItemText(Window,Component,Offset,Flags));
         Index  := Index + 1;
         Offset := Get_Selected(Window,Component,Offset,Flags);
      end loop;

      return Item_List;

   end Get_Selections;

   --

   function Get_SelectionNumbers (Window    : in Object_ID;
                                  Component : in Component_ID;
                                  Flags     : in System.Unsigned_Types.Unsigned := 0) return ItemNumber_List_Type is

      Selections : Natural := Count_Selections (Window,Component,Flags);
      Item_List  : ItemNumber_List_Type(1..Selections);

      Index      : Positive := 1;
      Offset     : Integer  := Get_Selected(Window,Component,-1,Flags);
   begin
      while Offset /= -1 loop
         Item_List (Index) := Offset;
         Index  := Index + 1;
         Offset := Get_Selected(Window,Component,Offset,Flags);
      end loop;

      return Item_List;

   end Get_SelectionNumbers;

   --

   function Is_Selected (Window    : in Object_ID;
                         Component : in Component_ID;
                         Index     : in Integer;
                         Flags     : in System.Unsigned_Types.Unsigned := 0) return Boolean is

      Selections : ItemNumber_List_Type := Get_SelectionNumbers(Window,Component,Flags);
   begin
      for i in Selections'Range loop
          if Selections(i) = Index then
             return true;
          end if;
      end loop;
      return false;
   end Is_Selected;

   --

   function Count_Items (Window    : in Object_ID;
                         Component : in Component_ID;
                         Flags     : in System.Unsigned_Types.Unsigned := 0) return Integer is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := ScrollList_Count_Items;
      Register.R(3) := int(Component);

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxScrollList.Count_Items: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return integer(Register.R(0));

   end Count_Items;

   --

   function Get_Items (Window    : in Object_ID;
                       Component : in Component_ID;
                       Flags     : in System.Unsigned_Types.Unsigned := 0) return Item_List_Type is

      Register    : aliased Kernel.swi_regs;
      Error       : oserror_access;
      Buffer_Size : integer  := 0;
      Index       : Positive := 1;
      Offset      : Natural  := 0;
      Items       : Natural  := Count_Items (Window,Component,Flags);
      Item_List   : Item_List_Type (1..Items);

   begin

      while Index <= Items loop
         Register.R(0) := Int(Unsigned_to_Int(Flags));
         Register.R(1) := Int(Window);
         Register.R(2) := ScrollList_GetItemText;
         Register.R(3) := Int(Component);
         Register.R(4) := 0;
         Register.R(5) := 0;
         Register.R(6) := Int(Offset);
         
         Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

         if Error /= null then
            null;
         else
            Buffer_Size := Integer(Register.R(5));

            declare
               Buffer : String(1..Buffer_Size);
            begin
               Register.R(0) := Int(Unsigned_to_Int(Flags));
               Register.R(1) := Int(Window);
               Register.R(2) := ScrollList_GetItemText;
               Register.R(3) := Int(Component);
               Register.R(4) := Adr_To_Int(Buffer'Address);
               Register.R(5) := Int(Buffer_Size);
               Register.R(6) := Int(Offset);
            
               Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);
            
               if Error = null then
                  Item_List(Index) := U(MemoryToString(Buffer'Address,0,Integer(Register.R(5))));
                  Index := Index + 1;
               end if;
            end;
         end if;
         Offset := Offset + 1;         
      end loop;

      return Item_List;
       
   end Get_Items;

   --

   procedure Make_Visible (Window    : in Object_ID;
                           Component : in Component_ID;
                           Index     : in integer;
                           Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := ScrollList_Make_Visible;
      Register.R(3) := int(Component);
      Register.R(4) := int(Index);

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxScrollList.Make_Visible: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Make_Visible;

   --

   procedure Set_Colour (Window    : in Object_ID;
                         Component : in Component_ID;
                         Foreground: in integer;
                         Background: in integer;
                         Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := ScrollList_SetColour;
      Register.R(3) := int(Component);
      Register.R(4) := int(Foreground);
      Register.R(5) := int(Background);

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxScrollList.Set_Colour: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Colour;

   --

   procedure Get_Colour (Window    : in Object_ID;
                         Component : in Component_ID;
                         Foreground: out integer;
                         Background: out integer;
                         Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := ScrollList_GetColour;
      Register.R(3) := int(Component);

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxScrollList.Get_Colour: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Foreground := integer(Register.R(4));
      Background := integer(Register.R(5));
   end Get_Colour;

   --

   procedure Set_Font (Window    : in Object_ID;
                       Component : in Component_ID;
                       Font_Handle : in Font_Handle_Type;
                       Font_Height : in integer:=12;
                       Font_Width  : in integer:=12;
                       Flags       : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := ScrollList_Set_Font;
      Register.R(3) := int(Component);
      Register.R(4) := int(Font_Handle);
      Register.R(5) := int(Font_Width);
      Register.R(6) := int(Font_Height);

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxScrollList.Set_Font: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Font;

   --

   function Get_ItemText (Window    : in Object_ID;
                          Component : in Component_ID;
                          Item      : in integer;
                          Flags     : in System.Unsigned_Types.Unsigned := 0) return String is

      Register    : aliased Kernel.swi_regs;
      Buffer_Size : Integer := 0;
      Error       : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := ScrollList_GetItemText;
      Register.R(3) := int(Component);
      Register.R(4) := 0;
      Register.R(5) := 0;
      Register.R(6) := int(Item);
      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxScrollList.Get_ItemText: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Buffer_Size := Integer(Register.R(5));
      declare
         Buffer : String(1..Buffer_Size);
      begin
         Register.R(0) := int(Unsigned_to_Int(Flags));
         Register.R(1) := int(Window);
         Register.R(2) := ScrollList_GetItemText;
         Register.R(3) := int(Component);
         Register.R(4) := Adr_To_Int(Buffer'Address);
         Register.R(5) := int(Buffer_Size);
         Register.R(6) := int(Item);

         Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);
         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxScrollList.Get_ItemText: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
         return Buffer;     
      end;
   end Get_ItemText;

   --

   procedure Set_ItemText(Window    : in Object_ID;
                          Component : in Component_ID;
                          Index     : in Integer;
                          Text      : in String;
                          Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Text_0   : string := Text & ASCII.NUL;
      Error    : oserror_access;
   begin
      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := ScrollList_SetItemText;
      Register.R(3) := Int(Component);
      Register.R(4) := Adr_To_Int(Text_0'Address);
      Register.R(5) := Int(Index);

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxScrollList.Set_ItemText: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_ItemText;
   
   --

   procedure Set_Heading (Window    : in Object_ID;
                          Component : in Component_ID;
                          Index     : in integer;
                          Text      : in String;
                          Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Text_0   : string := Text & ASCII.NUL;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := ScrollList_Set_Heading;
      Register.R(3) := int(Component);
      Register.R(4) := Adr_To_Int(Text_0'Address);
      Register.R(5) := 0;--int(Sprite_Area);
      Register.R(6) := 0;--Adr_To_Int(Sprite_Name'Address);
      Register.R(7) := int(Index);

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxScrollList.Set_Heading: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Heading;

   --

   function Get_Column (Items     : in Item_List_Type;
                        Column_Nr : in Positive := 1) return Item_List_Type is

      Column : Item_List_Type(Items'Range);

      Item   : UString;
      L      : Natural  := 0;
      C      : Positive := 1;
      I      : Natural  := 0;
   begin
      for Index in Column'Range loop
         Item := Items (Index);
         I := Ada.Strings.Unbounded.Index(Item,"" & ASCII.HT);
         while I > 0 and C < Column_Nr loop
            L    := Ada.Strings.Unbounded.Length(Item);
            Item := Ada.Strings.Unbounded.Tail(Item,L-(I+1));
            C := C + 1;
            I := Ada.Strings.Unbounded.Index(Item,"" & ASCII.HT);
         end loop;
         if C < Column_Nr then
            Column(Index) := U("");
         else
            Column(Index) := Item;
         end if;
      end loop;

      return Column;
   end Get_Column;

   --

end RASCAL.ToolboxScrollList;