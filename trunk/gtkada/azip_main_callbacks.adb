-----------------------------------------------------------------------------
--  Legal licensing note:
--
--  Copyright (c) surname name
--  FRANCE
--  Send bug reports to : capitaine.nemo@jules-verne.fr
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
--  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
--  DEALINGS IN THE SOFTWARE.
--  NB: this is the MIT License, as found 12-Sep-2007 on the site
--  http://www.opensource.org/licenses/mit-license.php
-----------------------------------------------------------------------------
-- units from Gtk
with Gtk.Main;    use Gtk.Main;
with Gtk.Builder; use Gtk.Builder;
with Gtk.Widget;  use Gtk.Widget;
with Gtk.Dialog;  use Gtk.Dialog;

-- Ada predefined units
with Ada.Text_IO; use Ada.Text_IO;

-- Application specific units

package body Azip_Main_Callbacks is

   -----------------------------------------------
   -- On_Quit
   -----------------------------------------------

   procedure On_Quit (Builder : access Gtkada_Builder_Record'Class) is
   begin
      Put_Line ("Within handler On_Quit");
      Destroy_Cb (Get_Widget (Builder, "azip_about"));
      Gtk.Main.Main_Quit;
   end On_Quit;

   -----------------------------------------------
   -- On_New_Archive
   -----------------------------------------------

   procedure On_New_Archive (Builder : access Gtkada_Builder_Record'Class) is
      pragma Unreferenced (Builder);
   begin
      Put_Line ("Within handler On_New_Archive");
   end On_New_Archive;

   -----------------------------------------------
   -- On_Open_Archive
   -----------------------------------------------

   procedure On_Open_Archive (Builder : access Gtkada_Builder_Record'Class) is
      pragma Unreferenced (Builder);
   begin
      Put_Line ("Within handler On_Open_Archive");
   end On_Open_Archive;

   -----------------------------------------------
   -- On_Save_Archive_As
   -----------------------------------------------

   procedure On_Save_Archive_As
     (Builder : access Gtkada_Builder_Record'Class)
   is
      pragma Unreferenced (Builder);
   begin
      Put_Line ("Within handler On_Save_Archive_As");
   end On_Save_Archive_As;

   -----------------------------------------------
   -- On_Recent
   -----------------------------------------------

   procedure On_Recent (Builder : access Gtkada_Builder_Record'Class) is
      pragma Unreferenced (Builder);
   begin
      Put_Line ("Within handler On_Recent");
   end On_Recent;

   -----------------------------------------------
   -- On_Cut
   -----------------------------------------------

   procedure On_Cut (Builder : access Gtkada_Builder_Record'Class) is
      pragma Unreferenced (Builder);
   begin
      Put_Line ("Within handler On_Cut");
   end On_Cut;

   -----------------------------------------------
   -- On_Copy
   -----------------------------------------------

   procedure On_Copy (Builder : access Gtkada_Builder_Record'Class) is
      pragma Unreferenced (Builder);
   begin
      Put_Line ("Within handler On_Copy");
   end On_Copy;

   -----------------------------------------------
   -- On_Paste
   -----------------------------------------------

   procedure On_Paste (Builder : access Gtkada_Builder_Record'Class) is
      pragma Unreferenced (Builder);
   begin
      Put_Line ("Within handler On_Paste");
   end On_Paste;

   -----------------------------------------------
   -- On_Delete
   -----------------------------------------------

   procedure On_Delete (Builder : access Gtkada_Builder_Record'Class) is
      pragma Unreferenced (Builder);
   begin
      Put_Line ("Within handler On_Delete");
   end On_Delete;

   -----------------------------------------------
   -- On_Test_Archive
   -----------------------------------------------

   procedure On_Test_Archive (Builder : access Gtkada_Builder_Record'Class) is
      pragma Unreferenced (Builder);
   begin
      Put_Line ("Within handler On_Test_Archive");
   end On_Test_Archive;

   -----------------------------------------------
   -- On_Recompress_Archive
   -----------------------------------------------

   procedure On_Recompress_Archive
     (Builder : access Gtkada_Builder_Record'Class)
   is
      pragma Unreferenced (Builder);
   begin
      Put_Line ("Within handler On_Recompress_Archive");
   end On_Recompress_Archive;

   -----------------------------------------------
   -- On_Find_File_In_Archive
   -----------------------------------------------

   procedure On_Find_File_In_Archive
     (Builder : access Gtkada_Builder_Record'Class)
   is
      pragma Unreferenced (Builder);
   begin
      Put_Line ("Within handler On_Find_File_In_Archive");
   end On_Find_File_In_Archive;

   -----------------------------------------------
   -- On_Find_Contents_In_Archive
   -----------------------------------------------

   procedure On_Find_Contents_In_Archive
     (Builder : access Gtkada_Builder_Record'Class)
   is
      pragma Unreferenced (Builder);
   begin
      Put_Line ("Within handler On_Find_Contents_In_Archive");
   end On_Find_Contents_In_Archive;

   -----------------------------------------------
   -- On_Compare_Archive_With
   -----------------------------------------------

   procedure On_Compare_Archive_With
     (Builder : access Gtkada_Builder_Record'Class)
   is
      pragma Unreferenced (Builder);
   begin
      Put_Line ("Within handler On_Compare_Archive_With");
   end On_Compare_Archive_With;

   -----------------------------------------------
   -- On_Flat_View
   -----------------------------------------------

   procedure On_Flat_View (Builder : access Gtkada_Builder_Record'Class) is
      pragma Unreferenced (Builder);
   begin
      Put_Line ("Within handler On_Flat_View");
   end On_Flat_View;

   -----------------------------------------------
   -- On_Tree_View
   -----------------------------------------------

   procedure On_Tree_View (Builder : access Gtkada_Builder_Record'Class) is
      pragma Unreferenced (Builder);
   begin
      Put_Line ("Within handler On_Tree_View");
   end On_Tree_View;

   -----------------------------------------------
   -- On_About
   -----------------------------------------------

   procedure On_About (Builder : access Gtkada_Builder_Record'Class) is
      Reponse : Gtk_Response_Type;
      About   : Gtk_Dialog :=
         Gtk_Dialog (Get_Widget (Builder, "azip_about"));
   begin
      Put_Line ("Within handler On_About_Activate");
      Show_All (About);
      Reponse := Run (About);
      Hide_All (About);
   end On_About;

   -----------------------------------------------
   -- On_Open_Toolbutton
   -----------------------------------------------

   procedure On_Open_Toolbutton
     (Builder : access Gtkada_Builder_Record'Class)
   is
      pragma Unreferenced (Builder);
   begin
      Put_Line ("Within handler On_Open_Toolbutton");
   end On_Open_Toolbutton;

   -----------------------------------------------
   -- On_Add_Toolbutton
   -----------------------------------------------

   procedure On_Add_Toolbutton
     (Builder : access Gtkada_Builder_Record'Class)
   is
      pragma Unreferenced (Builder);
   begin
      Put_Line ("Within handler On_Add_Toolbutton");
   end On_Add_Toolbutton;

   -----------------------------------------------
   -- On_Suppress_Toolbutton
   -----------------------------------------------

   procedure On_Suppress_Toolbutton
     (Builder : access Gtkada_Builder_Record'Class)
   is
      pragma Unreferenced (Builder);
   begin
      Put_Line ("Within handler On_Suppress_Toolbutton");
   end On_Suppress_Toolbutton;

   -----------------------------------------------
   -- On_Exam_Toolbutton
   -----------------------------------------------

   procedure On_Exam_Toolbutton
     (Builder : access Gtkada_Builder_Record'Class)
   is
      pragma Unreferenced (Builder);
   begin
      Put_Line ("Within handler On_Exam_Toolbutton");
   end On_Exam_Toolbutton;

end Azip_Main_Callbacks;
