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

with RASCAL.Memory;        use RASCAL.Memory;
with RASCAL.OS;            use RASCAL.OS;
with RASCAL.Utility;       use RASCAL.Utility;

with Kernel;               use Kernel;
with Interfaces.C;         use Interfaces.C;
with Reporter;

package body RASCAL.Toolbox is

   Toolbox_CreateObject            : constant := 16#044EC0#;
   Toolbox_DeleteObject            : constant := 16#044EC1#;
   Toolbox_CopyObject              : constant := 16#044EC2#;
   Toolbox_Show_Object             : constant := 16#044EC3#;
   Toolbox_Hide_Object             : constant := 16#044EC4#;
   Toolbox_GetObjectInfo           : constant := 16#044EC5#;
   Toolbox_ObjectMiscOp            : constant := 16#044EC6#;
   Toolbox_SetClientHandle         : constant := 16#044EC7#;
   Toolbox_GetClientHandle         : constant := 16#044EC8#;
   Toolbox_GetObjectClass          : constant := 16#044EC9#;
   Toolbox_GetParent               : constant := 16#044ECA#;
   Toolbox_GetAncestor             : constant := 16#044ECB#;
   Toolbox_GetTemplateName         : constant := 16#044ECC#;
   Toolbox_RaiseToolboxEvent       : constant := 16#044ECD#;
   Toolbox_GetSysInfo              : constant := 16#044ECE#;
   Toolbox_Initialise              : constant := 16#044ECF#;
   Toolbox_Load_Resources          : constant := 16#044ED0#;
   Toolbox_Memory                  : constant := 16#044EF9#;
   Toolbox_DeRegisterObjectModule  : constant := 16#044EFA#;
   Toolbox_Template_LookUp         : constant := 16#044EFB#;
   Toolbox_GetInternalHandle       : constant := 16#044EFC#;
   Toolbox_RegisterPostFilter      : constant := 16#044EFD#;
   Toolbox_RegisterPreFilter       : constant := 16#044EFE#;
   Toolbox_RegisterObjectModule    : constant := 16#044EFF#;

   --

   function Create_Object (Template : in string) return Object_ID is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
      Template_0           : string             := Template & ASCII.NUL;
   begin
      Register.R(0) := Creation_Type'Pos(From_Template);
      Register.R(1) := Adr_To_Int(Template_0'Address);

      Error := Kernel.swi(Toolbox_CreateObject,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Toolbox.Create_Object1: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      return Object_ID(Register.R(0));

   end Create_Object;

   --

   function Create_Object (Template : in Address) return Object_ID is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
   begin
      Register.R(0) := Creation_Type'Pos(From_Memory);
      Register.R(1) := Adr_To_Int(Template);

      Error := Kernel.swi(Toolbox_CreateObject,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Toolbox.Create_Object2: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      return Object_ID(Register.R(0));

   end Create_Object;

   --

   procedure Delete_Object (Object : in Object_ID;
                            Flags  : in System.Unsigned_Types.Unsigned := 0) is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Object);

      Error := Kernel.swi(Toolbox_DeleteObject,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Toolbox.Delete_Object: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

   end Delete_Object;

   --

   procedure Get_Ancestor (Object   : in Object_ID;
                           Ancestor : out Object_ID;
                           Component: out Component_ID;
                           Flags    : in System.Unsigned_Types.Unsigned := 0) is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Object);

      Error := Kernel.swi(Toolbox_GetAncestor,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Toolbox.Get_Ancestor: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Ancestor := Object_ID(Register.R(0));
      Component := Component_ID(Register.R(1));

   end Get_Ancestor;

   --

   function Get_Client (Object : in Object_ID;
                        Flags  : in System.Unsigned_Types.Unsigned := 0) return Object_ID is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Object);

      Error := Kernel.swi(Toolbox_GetClientHandle,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Toolbox.Get_Client: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      return Object_ID(Register.R(0));

   end Get_Client;

   --

   function Get_Class (Object : in Object_ID;
                       Flags  : in System.Unsigned_Types.Unsigned := 0) return Object_Class is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Object);

      Error := Kernel.swi(Toolbox_GetObjectClass,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Toolbox.Get_Class: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      return Object_Class(Register.R(0));

   end Get_Class;

   --

   function Get_State (Object : in Object_ID;
                       Flags  : in System.Unsigned_Types.Unsigned := 0) return Object_State is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Object);

      Error := Kernel.swi(Toolbox_GetObjectInfo,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Toolbox.Get_State: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      return Object_State'Val(integer(Register.R(0)));

   end Get_State;

   --

   procedure Get_Parent (Object   : in Object_ID;
                         Parent   : out Object_ID;
                         Component: out Component_ID;
                         Flags    : in System.Unsigned_Types.Unsigned := 0) is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Object);

      Error := Kernel.swi(Toolbox_GetParent,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Toolbox.Get_Parent: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Parent      := Object_ID(Register.R(0));
      Component   := Component_ID(Register.R(1));

   end Get_Parent;

   --

   function Get_Template_Name (Object : in Object_ID;
                               Flags  : in System.Unsigned_Types.Unsigned := 0) return string is

      Register                : aliased Kernel.swi_regs;
      Error                   : oserror_access;
      Buffer_Size             : integer;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Object);
      Register.R(2) := 0;
      Register.R(3) := 0;

      Error := Kernel.swi(Toolbox_GetTemplateName,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Toolbox.Get_Template_Name: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Buffer_Size := integer(Register.R(3));

      declare
         Buffer : string(1..Buffer_Size);
      begin
         Register.R(0) := int(Unsigned_to_Int(Flags));
         Register.R(1) := int(Object);
         Register.R(2) := Adr_To_Int(Buffer'Address);
         Register.R(3) := int(Buffer_Size);

         Error := Kernel.swi(Toolbox_GetTemplateName,Register'Access,Register'Access);

         if Error /=null then
            return "";
         else
            return MemoryToString(Buffer'Address,0,integer(Register.R(3))-1);
         end if;
      end;

   end Get_Template_Name;

   --

   procedure Hide_Object (Object : in Object_ID;
                          Flags  : in System.Unsigned_Types.Unsigned := 0) is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Object);

      Error := Kernel.swi(Toolbox_Hide_Object,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Toolbox.Hide_Object: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

   end Hide_Object;

   --

   procedure Load_Resources (Filename : in string;
                             Flags    : in System.Unsigned_Types.Unsigned := 0) is

      Register                : aliased Kernel.swi_regs;
      Error                   : oserror_access;
      Filename_0              : string := Filename & ASCII.NUL;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := Adr_To_Int(Filename_0'Address);

      Error := Kernel.swi(Toolbox_Load_Resources,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Toolbox.Load_Reaources: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

   end Load_Resources;

   --

   procedure Set_Client_Handle (Object : in Object_ID;
                                Client : in Object_ID;
                                Flags  : in System.Unsigned_Types.Unsigned := 0) is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
      Toolbox_Client_Handle: constant Interfaces.C.unsigned :=16#44EC7#;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Object);
      Register.R(2) := int(Object);

      Error := Kernel.swi(Toolbox_Client_Handle,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Toolbox.Set_Client_Handle: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

   end Set_Client_Handle;

   --

   procedure Show_Object_At (Object             : in Object_ID;
                             X                  : integer;
                             Y                  : integer; 
                             Parent_Object      : in Object_ID;
                             Parent_Component   : in Component_ID;
                             Flags              : in System.Unsigned_Types.Unsigned := 0) is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
      BBox                 : Toolbox_BBox_Type;
   begin
      BBox.XMin := x;
      BBox.YMin := y;

      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Object);
      Register.R(2) := 2;
      Register.R(3) := Adr_To_Int(BBox'Address);
      Register.R(4) := int(Parent_Object);
      Register.R(5) := int(Parent_Component);

      Error := Kernel.swi(Toolbox_Show_Object,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Toolbox.Show_Object_At: " & To_Ada(Error.errmess)));
         OS.Raise_Error(Error);
      end if;
      
   end Show_Object_At;

   --
   
   procedure Show_Object (Object             : in Object_ID;
                          Parent_Object      : in Object_ID := 0;
                          Parent_Component   : in Component_ID := 0;
                          Show               : in Object_ShowType := Default;
                          Flags              : in System.Unsigned_Types.Unsigned := 0) is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Object);
      Register.R(2) := int(Object_ShowType'Pos(Show));
      Register.R(3) := 0;
      Register.R(4) := int(Parent_Object);
      Register.R(5) := int(Parent_Component);

      Error := Kernel.swi(Toolbox_Show_Object,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Toolbox.Show_Object: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

   end Show_Object;

   --

   function Template_Lookup(Template_Name : in string;
                            Flags         : in System.Unsigned_Types.Unsigned := 0) return Address is

      Register                : aliased Kernel.swi_regs;
      Error                   : oserror_access;
      Template_0              : string := Template_Name & ASCII.NUL;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := Adr_To_Int(Template_0'Address);

      Error := Kernel.swi(Toolbox_Template_Lookup,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Toolbox.Template_Lookup: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      return Int_To_adr(Register.R(0));
      
   end Template_Lookup;

   --

   procedure Raise_Event (Object    : in Object_ID;
                          Component : in Component_ID;
                          Event     : in Address;
                          Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Toolbox_RaiseToolboxEvent : constant Interfaces.C.unsigned :=16#44ecd#;

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Integer(Object));
      Register.R(2) := int(Integer(Component));
      Register.R(3) := Adr_To_Int(Event);
      Error := Kernel.swi(Toolbox_RaiseToolboxEvent,Register'Access,Register'Access);
   
      if Error /= null then
         pragma Debug(Reporter.Report("Toolbox.Raise_Event: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Raise_Event;

   --
   
end RASCAL.Toolbox;
