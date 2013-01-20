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
with Gtkada.Builder; use Gtkada.Builder;

package Azip_Main_Callbacks is

   procedure On_Quit (Builder : access Gtkada_Builder_Record'Class);

   procedure On_New_Archive (Builder : access Gtkada_Builder_Record'Class);

   procedure On_Open_Archive (Builder : access Gtkada_Builder_Record'Class);

   procedure On_Save_Archive_As
     (Builder : access Gtkada_Builder_Record'Class);

   procedure On_Recent (Builder : access Gtkada_Builder_Record'Class);

   procedure On_Cut (Builder : access Gtkada_Builder_Record'Class);

   procedure On_Copy (Builder : access Gtkada_Builder_Record'Class);

   procedure On_Paste (Builder : access Gtkada_Builder_Record'Class);

   procedure On_Delete (Builder : access Gtkada_Builder_Record'Class);

   procedure On_Test_Archive (Builder : access Gtkada_Builder_Record'Class);

   procedure On_Recompress_Archive
     (Builder : access Gtkada_Builder_Record'Class);

   procedure On_Find_File_In_Archive
     (Builder : access Gtkada_Builder_Record'Class);

   procedure On_Find_Contents_In_Archive
     (Builder : access Gtkada_Builder_Record'Class);

   procedure On_Compare_Archive_With
     (Builder : access Gtkada_Builder_Record'Class);

   procedure On_Flat_View (Builder : access Gtkada_Builder_Record'Class);

   procedure On_Tree_View (Builder : access Gtkada_Builder_Record'Class);

   procedure On_About (Builder : access Gtkada_Builder_Record'Class);

   procedure On_Open_Toolbutton
     (Builder : access Gtkada_Builder_Record'Class);

   procedure On_Add_Toolbutton
     (Builder : access Gtkada_Builder_Record'Class);

   procedure On_Suppress_Toolbutton
     (Builder : access Gtkada_Builder_Record'Class);

   procedure On_Exam_Toolbutton
     (Builder : access Gtkada_Builder_Record'Class);

end Azip_Main_Callbacks;
