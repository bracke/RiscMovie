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

with Interfaces.C;            use Interfaces.C;
with Ada.Exceptions;          use Ada.Exceptions;

with RASCAL.Utility;

package body RASCAL.OS is

   --
   
   procedure Raise_Error (Error : OSError_Access) is

      Nr      : Integer := Utility."And"(Integer(Error.ErrNum),16#FF#);
      Message : String  := To_Ada(Error.ErrMess);
   begin
      case Nr is
      when Error_Escape                 => Raise_Exception (Exception_Escape'Identity , Message);
      when Error_Bad_mode               => Raise_Exception (Exception_Bad_mode'Identity , Message);
      when Error_Is_adir                => Raise_Exception (Exception_Is_adir'Identity , Message);
      when Error_Types_dont_match       => Raise_Exception (Exception_Types_dont_match'Identity , Message);
      when Error_Bad_rename             => Raise_Exception (Exception_Bad_rename'Identity , Message);
      when Error_Bad_copy               => Raise_Exception (Exception_Bad_copy'Identity , Message);
      when Error_Outside_file           => Raise_Exception (Exception_Outside_file'Identity , Message);
      when Error_Access_violation       => Raise_Exception (Exception_Access_violation'Identity , Message);
      when Error_Too_many_open_files    => Raise_Exception (Exception_Too_many_open_files'Identity , Message);
      when Error_Not_open_for_update    => Raise_Exception (Exception_Not_open_for_update'Identity , Message);
      when Error_File_open              => Raise_Exception (Exception_File_open'Identity , Message);
      when Error_Object_locked          => Raise_Exception (Exception_Object_locked'Identity , Message);
      when Error_Already_exists         => Raise_Exception (Exception_Already_exists'Identity , Message);
      when Error_Bad_file_name          => Raise_Exception (Exception_Bad_file_name'Identity , Message);
      when Error_File_not_found         => Raise_Exception (Exception_File_not_found'Identity , Message);
      when Error_Syntax                 => Raise_Exception (Exception_Syntax'Identity , Message);
      when Error_Channel                => Raise_Exception (Exception_Channel'Identity , Message);
      when Error_End_of_file            => Raise_Exception (Exception_End_of_file'Identity , Message);
      when Error_Buffer_Overflow        => Raise_Exception (Exception_Buffer_Overflow'Identity , Message);
      when Error_Bad_filing_system_name => Raise_Exception (Exception_Bad_filing_system_name'Identity , Message);
      when Error_Bad_key                => Raise_Exception (Exception_Bad_key'Identity , Message);
      when Error_Bad_address            => Raise_Exception (Exception_Bad_address'Identity , Message);
      when Error_Bad_string             => Raise_Exception (Exception_Bad_string'Identity , Message);
      when Error_Bad_command            => Raise_Exception (Exception_Bad_command'Identity , Message);
      when Error_Bad_mac_val            => Raise_Exception (Exception_Bad_mac_val'Identity , Message);
      when Error_Bad_var_nam            => Raise_Exception (Exception_Bad_var_nam'Identity , Message);
      when Error_Bad_var_type           => Raise_Exception (Exception_Bad_var_type'Identity , Message);
      when Error_Var_no_room            => Raise_Exception (Exception_Var_no_room'Identity , Message);
      when Error_Var_cant_find          => Raise_Exception (Exception_Var_cant_find'Identity , Message);
      when Error_Var_too_long           => Raise_Exception (Exception_Var_too_long'Identity , Message);
      when Error_Redirect_fail          => Raise_Exception (Exception_Redirect_fail'Identity , Message);
      when Error_Stack_full             => Raise_Exception (Exception_Stack_full'Identity , Message);
      when Error_Bad_hex                => Raise_Exception (Exception_Bad_hex'Identity , Message);
      when Error_Bad_expr               => Raise_Exception (Exception_Bad_expr'Identity , Message);
      when Error_Bad_bra                => Raise_Exception (Exception_Bad_bra'Identity , Message);
      when Error_Stk_oflo               => Raise_Exception (Exception_Stk_oflo'Identity , Message);
      when Error_Miss_opn               => Raise_Exception (Exception_Miss_opn'Identity , Message);
      when Error_Miss_opr               => Raise_Exception (Exception_Miss_opr'Identity , Message);
      when Error_Bad_bits               => Raise_Exception (Exception_Bad_bits'Identity , Message);
      when Error_Str_oflo               => Raise_Exception (Exception_Str_oflo'Identity , Message);
      when Error_Bad_itm                => Raise_Exception (Exception_Bad_itm'Identity , Message);
      when Error_Div_zero               => Raise_Exception (Exception_Div_zero'Identity , Message);
      when Error_Bad_base               => Raise_Exception (Exception_Bad_base'Identity , Message);
      when Error_Bad_numb               => Raise_Exception (Exception_Bad_numb'Identity , Message);
      when Error_Numb_too_big           => Raise_Exception (Exception_Numb_too_big'Identity , Message);
      when Error_Bad_claim_num          => Raise_Exception (Exception_Bad_claim_num'Identity , Message);
      when Error_Bad_release            => Raise_Exception (Exception_Bad_release'Identity , Message);
      when Error_Bad_dev_no             => Raise_Exception (Exception_Bad_dev_no'Identity , Message);
      when Error_Bad_dev_vec_rel        => Raise_Exception (Exception_Bad_dev_vec_rel'Identity , Message);
      when Error_Bad_env_number         => Raise_Exception (Exception_Bad_env_number'Identity , Message);
      when Error_Cant_cancel_quit       => Raise_Exception (Exception_Cant_cancel_quit'Identity , Message);
      when Error_Ch_dynam_cao           => Raise_Exception (Exception_Ch_dynam_cao'Identity , Message);
      when Error_Ch_dynam_not_all_moved => Raise_Exception (Exception_Ch_dynam_not_all_moved'Identity , Message);
      when Error_Apl_wspace_in_use      => Raise_Exception (Exception_Apl_wspace_in_use'Identity , Message);
      when Error_Ram_fs_unchangeable    => Raise_Exception (Exception_Ram_fs_unchangeable'Identity , Message);
      when Error_Oscli_long_line        => Raise_Exception (Exception_Oscli_long_line'Identity , Message);
      when Error_Oscli_too_hard         => Raise_Exception (Exception_Oscli_too_hard'Identity , Message);
      when Error_Rc_exc                 => Raise_Exception (Exception_Rc_exc'Identity , Message);
      when Error_Sys_heap_full          => Raise_Exception (Exception_Sys_heap_full'Identity , Message);
      when Error_Buff_overflow          => Raise_Exception (Exception_Buff_overflow'Identity , Message);
      when Error_Bad_time               => Raise_Exception (Exception_Bad_time'Identity , Message);
      when Error_No_such_swi            => Raise_Exception (Exception_No_such_swi'Identity , Message);
      when Error_Unimplemented          => Raise_Exception (Exception_Unimplemented'Identity , Message);
      when Error_Out_of_range           => Raise_Exception (Exception_Out_of_range'Identity , Message);
      when Error_No_oscli_specials      => Raise_Exception (Exception_No_oscli_specials'Identity , Message);
      when Error_Bad_parameters         => Raise_Exception (Exception_Bad_parameters'Identity , Message);
      when Error_Arg_repeated           => Raise_Exception (Exception_Arg_repeated'Identity , Message);
      when Error_Bad_read_sys_info      => Raise_Exception (Exception_Bad_read_sys_info'Identity , Message);
      when Error_Cdat_stack_overflow    => Raise_Exception (Exception_Cdat_stack_overflow'Identity , Message);
      when Error_Cdat_buffer_overflow   => Raise_Exception (Exception_Cdat_buffer_overflow'Identity , Message);
      when Error_Cdat_bad_field         => Raise_Exception (Exception_Cdat_bad_field'Identity , Message);
      when Error_Cant_start_application => Raise_Exception (Exception_Cant_start_application'Identity , Message);

      when Error_Tool_Action_Out_of_Memory      => Raise_Exception (Exception_Tool_Action_Out_of_Memory'Identity , Message);
      when Error_Tool_Action_Cant_Create_Icon   => Raise_Exception (Exception_Tool_Action_Cant_Create_Icon'Identity , Message);
      when Error_Tool_Action_Cant_Create_Object => Raise_Exception (Exception_Tool_Action_Cant_Create_Object'Identity , Message);

      when others => Raise_Exception(Exception_Unknown_Error'Identity,"Error: " & Utility.intstr(Nr) & " - " & Message);
      end case;
   end Raise_Error;

   --

end RASCAL.OS;

