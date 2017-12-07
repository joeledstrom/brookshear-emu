package ui

import scala.scalajs.js.Dynamic.{global => g}
import emulator._

import scala.scalajs.js
import scala.util._



object Main extends js.JSApp {
  
  var ms = MachineState()
  var running = false
  var wait_time = 500
  val memoryTable = new MachineWordTable(16, true)
  val registerTable = new MachineWordTable(1, true, "GPRs")
  
  def run() {
    running = true
    ms = ms.step
    updateUI()
    
    if (ms.cycleState == Halted)
      running = false
    else
      g.setTimeout(run _, wait_time)
  }

  def updateLights() {
    val fetch = g.document.getElementById("fetch")
    val decode = g.document.getElementById("decode")
    val execute = g.document.getElementById("execute")
    
    Seq(fetch, decode, execute) foreach (_.style.backgroundColor = "white")
    
    val current = ms.cycleState match {
      case Fetch => Some(fetch)
      case Decode => Some(decode)
      case Execute => Some(execute)
      case _ => None
    }
    
    current foreach (_.style.backgroundColor = "red")
  }
  
  def updateHashLink() {
    val hashlink = g.document.getElementById("hashlink")
    hashlink.href = "#" + (ms.memory.reverse.foldLeft(("", false)) { 
      case ((z, false), x) => if (0 == x.unsignedValue) (z, false) else (x.toHexString + z, true)
      case ((z, true), x) => (x.toHexString + z, true)
    })._1
  }
  
  def readHashLink() {
    val hash = g.window.location.hash.asInstanceOf[String]
    
    if (hash.length <= 1) return
    
    val decodedHashData = Try(hash.tail.grouped(2) map { (s) =>
      val hexValue = Try(Integer.parseInt(s, 16)) filter (v => v >=0 && v <= 0xff)
      
      hexValue match {
        case Success(v) => MachineWord(v)
        case _ => throw new Exception("parse error")
      }
    })
    
    for {
      l <- decodedHashData
      lSeq = l.toSeq
    } { 
      ms = MachineState(lSeq ++ Vector.fill(256 - lSeq.length)(MachineWord(0))) 
    }
  }
  
  def updateUI() {
    memoryTable.setData(ms.memory)
    registerTable.setData(ms.gprs)
    g.document.getElementById("pc_value").textContent = ms.pc.toHexString
    g.document.getElementById("ir_value").textContent = ms.ir._1.toHexString + ms.ir._2.toHexString
    g.document.getElementById("dec_value").textContent = ms.decodedInstruction.toString
    updateLights()
    updateHashLink()
    // TODO: disable buttons while running
  }
  def main(): Unit = {
    
    readHashLink()
    
    updateUI()
    
    memoryTable.onChangeHandler = (address: MachineWord, value: MachineWord) => {
      ms = ms.withUpdatedMemory(address, value)
      updateUI()
    }
    
    val addr = g.document.getElementById("addr")
    val hex_value = g.document.getElementById("hex_value")
    val binary_value = g.document.getElementById("binary_value")
    val signed_value = g.document.getElementById("signed_value")
    val float_value = g.document.getElementById("float_value")
    
    memoryTable.onHoverHandler = {
      case Some(address) =>
        val memValue = ms.memory(address.unsignedValue)
        addr.textContent = "MEM: 0x" + address.toHexString
        hex_value.textContent = "0x" + memValue.toHexString
        binary_value.textContent = f"${Integer.toBinaryString(memValue)}%8s"
        float_value.textContent = memValue.floatValue
        signed_value.textContent = memValue.signedValue
            
      case None =>
        Seq(addr, hex_value, binary_value, float_value, signed_value).foreach(_.textContent = "")
    }
       
    g.document.getElementById("memory").appendChild(memoryTable.element)
    g.document.getElementById("gprs_container").appendChild(registerTable.element)
    
    g.document.getElementById("clear").onclick = () => {
      if (!running) {
        ms = ms.withClearedCPU
        updateUI()
      }
    }
    
    g.document.getElementById("clear_and_run").onclick = () => {
      if (!running) {
        ms = ms.withClearedCPU
        updateUI()
        g.setTimeout(run _, wait_time)
      }
    }
    
    g.document.getElementById("run").onclick = () => {
      if (!running) {
        g.setTimeout(run _, wait_time)
      }
    }
    
    g.document.getElementById("step").onclick = () => {
      if (!running) {
        ms = ms.step
        updateUI()
      }
    }

    val field = g.document.getElementById("wait_time")

    field.onchange = () => {
      wait_time = g.parseInt(field.value).asInstanceOf[Int]
    }
  }
}
