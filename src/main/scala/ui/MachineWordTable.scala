package ui

import scala.scalajs.js.Dynamic.{global => g}
import emulator.MachineWord

import scala.scalajs.js
import scala.util._


class MachineWordTable(cols: Int, showFirstHeaderRow: Boolean, customHeaderRow: String = null) {
  
  def setData(d: Seq[MachineWord]) { _data = d; update() }

  def element = tableElement
  
  var onChangeHandler: ((MachineWord, MachineWord) => Unit) = null
  
  var onHoverHandler: (Option[MachineWord] => Unit) = null
  
  private var _data: Seq[MachineWord] = Seq.empty

  private lazy val tableElement = g.document.createElement("table")
  
  private def makeFirstRow = {
    val tr = g.document.createElement("tr")
    
    if (customHeaderRow != null) {
      val th = g.document.createElement("th")
      th.textContent = customHeaderRow
      th.colSpan = cols + 1
      tr.appendChild(th)
    } else {
      tr.appendChild(g.document.createElement("th"))
      
      (0 until cols) foreach { i =>
        val th = g.document.createElement("th")
        th.textContent = f"$i%X"
        tr.appendChild(th)
      }
    }
    
    tr
  }
  
  private var lastIndex: Option[Int] = None
  
  private def onInput(text: String, index: Int) {
    
    val hexValue = Try(Integer.parseInt(text, 16)) filter (v => v >=0 && v <= 0xff)
    
    hexValue match {
      case Success(h) if onChangeHandler != null => onChangeHandler(MachineWord(index), MachineWord(h))
      case _ => update()
    }
  }
  
  private def focusAndSelectText(e: js.Dynamic) {
    e.focus()
    val range = g.document.createRange()
    range.selectNodeContents(e)
    val sel = g.window.getSelection()
    sel.removeAllRanges()
    sel.addRange(range)
  }
  
  private def update() {
    
    var elementToFocus: Option[js.Dynamic] = None
    
    tableElement.innerHTML = ""
    tableElement.onmouseout = () => if (onHoverHandler != null) onHoverHandler(None)
    
    if (showFirstHeaderRow) 
      tableElement.appendChild(makeFirstRow)
    
    for ((row, rowIndex) <- _data.zipWithIndex.grouped(cols).zipWithIndex) {
   
      val tr = g.document.createElement("tr")
      
      val th = g.document.createElement("th")
      th.textContent = f"$rowIndex%X"
      tr.appendChild(th)
      
      for ((cell, cellIndex) <- row) {
        
        val td = g.document.createElement("td")
        td.textContent = f"${cell.toInt}%02X"
        td.contentEditable = true
        
        td.onblur = (e: js.Dynamic) => onInput(td.textContent.toString, cellIndex)
        td.onkeypress = (e: js.Dynamic) => if (e.keyCode.toString == "13") {
          lastIndex = Some(cellIndex)
          td.blur()
          e.preventDefault()
          false 
        }
        td.onmouseover = () => if (onHoverHandler != null) onHoverHandler(Some(MachineWord(cellIndex))) 
        
        lastIndex filter (_ == cellIndex-1) foreach { _ => elementToFocus = Some(td); lastIndex = None }
        
        tr.appendChild(td)
      } 
      tableElement.appendChild(tr)
    }
    elementToFocus foreach (focusAndSelectText(_)) 
  }
  
}
