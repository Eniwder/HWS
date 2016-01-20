

package main.scala

import javax.swing.table.DefaultTableCellRenderer
import java.awt.Color
import javax.swing.JTable

class MyRenderer(s: Int, bnd: Int, e: Int, u: Int, b: Int) extends DefaultTableCellRenderer {
  override def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean,
                                             hasFocus: Boolean, row: Int, column: Int) = {
    super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
    val evOrOdd = (u + 1) % 2
    if (isSelected) {
      setForeground(table.getSelectionForeground())
      setBackground(table.getSelectionBackground())
    } else {
      setForeground(table.getForeground());
      val c = if (row % 2 == evOrOdd || column < s || e < column || row < u || b < row) table.getBackground()
      else if (column > bnd) new Color(255, 228, 181)
      else new Color(240, 240, 255)
      setBackground(c)
    }
    this
  }
}