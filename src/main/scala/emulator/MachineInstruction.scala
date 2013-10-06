package emulator

import scala.util._

sealed trait MachineInstruction {
  def execute(st: MachineState): MachineState = st
}

object MachineInstruction {
  def decode(words: Pair[MachineWord, MachineWord]): MachineInstruction = {
    val (w1, w2) = words
    val opcode = w1 >>> 4
    val firstOperand = w1 & 0xf
    val secondOperand = w2 >>> 4
    val thirdOperand = w2 & 0xf
    
    val decodedInst = Try(opcode match {
      case 1 => Load(firstOperand, w2)
      case 2 => LoadImmediate(firstOperand, w2)
      case 3 => Store(firstOperand, w2)
      case 4 => Move(secondOperand, thirdOperand)
      case 5 => AddInt(firstOperand, secondOperand, thirdOperand)
      case 6 => AddFloat(firstOperand, secondOperand, thirdOperand)
      case 7 => BitwiseOp(firstOperand, secondOperand, thirdOperand, (_ | _), "OR r%d <- r%d | r%d")
      case 8 => BitwiseOp(firstOperand, secondOperand, thirdOperand, (_ & _), "AND r%d <- r%d & r%d")
      case 9 => BitwiseOp(firstOperand, secondOperand, thirdOperand, (_ ^ _), "XOR r%d <- r%d ^ r%d")
      case 0xA => ShiftOp(firstOperand, thirdOperand, 
                          ((r,x) => (r >>> x) | (r << (8-x) & 0xff)),
                          "ROTATE-RIGHT r%d <- r%d [ror] r%d")
      case 0xB => Jump(firstOperand, w2)
      case 0xC if firstOperand == 0 && w2.unsignedValue == 0 => Halt
      case 0xD => ShiftOp(firstOperand, thirdOperand, (_ << _), "SHL r%d <- r%d << r%d")
      case 0xE => ShiftOp(firstOperand, thirdOperand, (_ >> _), "SAR r%d <- r%d >> r%d")
      case _ => IllegalInstruction
    })
    
    
    decodedInst getOrElse IllegalInstruction
  }
}

case class Load(toGpr: Int, fromAddress: MachineWord) extends MachineInstruction {
  override def execute(st: MachineState) = st.withUpdatedGPR(toGpr, st.memory(fromAddress))
  override def toString = s"LOAD r$toGpr <- *(0x${fromAddress.toHexString})"
}

case class LoadImmediate(toGpr: Int, immediateValue: MachineWord) extends MachineInstruction {
  override def execute(st: MachineState) = st.withUpdatedGPR(toGpr, immediateValue)
  override def toString = s"LOADI r$toGpr <- 0x${immediateValue.toHexString}"
}

case class Store(fromGpr: Int, toAddress: MachineWord) extends MachineInstruction {
  override def execute(st: MachineState) = st.withUpdatedMemory(toAddress, st.gprs(fromGpr))
  override def toString = s"STORE r$fromGpr -> *(0x${toAddress.toHexString})"
}

case class Move(copyFromGpr: Int, toGpr: Int) extends MachineInstruction {
  override def execute(st: MachineState) = st.withUpdatedGPR(toGpr, st.gprs(copyFromGpr))
  override def toString = s"MOVE r$copyFromGpr -> r$toGpr"
}

case class AddInt(r: Int, s: Int, t: Int) extends MachineInstruction {
  override def execute(st: MachineState) = {
    val sValue = st.gprs(s).signedValue
    val tValue = st.gprs(t).signedValue
    
    st.withUpdatedGPR(r, MachineWord(sValue + tValue))
  }
  override def toString = s"ADD r$r <- r$s + r$t"
}

case class AddFloat(r: Int, s: Int, t: Int) extends MachineInstruction {
    
  override def execute(st: MachineState) = {  
    import scala.annotation.tailrec
    
    val (s_signbit, s_exponent_excess, s_mantissa) = st.gprs(s).floatComponents
    val (t_signbit, t_exponent_excess, t_mantissa) = st.gprs(t).floatComponents
  
    // unify exponents by converting both floating points values to
    // to fixed point with the same implied exponent of -4
    val s_fixed = (1 - 2*s_signbit) * (s_mantissa << s_exponent_excess)
    val t_fixed = (1 - 2*t_signbit) * (t_mantissa << t_exponent_excess)
    
    // use the built in integer adder to add
    val r_fixed = s_fixed + t_fixed
    
    // seperate out sign
    val r_signbit = if (r_fixed < 0) 1 else 0
    val r_mantissa_fixed = if (r_signbit == 0) r_fixed else -r_fixed
    
    // special case if the mantissa is 0
    if (r_mantissa_fixed == 0) st.withUpdatedGPR(r, MachineWord(0x00))  
    
    @tailrec
    def normalise(f: Int, e_excess: Int): (Int, Int) = {        
      val p = (f >>> e_excess) & 0xf
      
      // IF the MSB of the potential mantissa is 1 (which means we found its normalized form)
      // OR the input data wasn't in normalized form, then we silently return a non-normalized result
      //    since this machine doesn't have any form of status/error flags
      if (((p >>> 3) & 0x1) == 1 || e_excess == 0)
        (p, e_excess)
      else
        normalise(f, e_excess - 1)
    }
    
    val (r_mantissa, r_exp_excess) = normalise(r_mantissa_fixed, 8)  // start at 8 to test for overflow
    
    if (r_exp_excess > 7) // if overflow: return the biggest storable absolute value with correct sign
      st.withUpdatedGPR(r, MachineWord(r_signbit << 7 | 7 << 4 | 0xf)) 
    else
      st.withUpdatedGPR(r, MachineWord(r_signbit << 7 | r_exp_excess << 4 | r_mantissa))
  }
  
  override def toString = s"ADD_FLOAT r$r <- r$s + r$t"
}

case class BitwiseOp(r: Int, s: Int, t: Int, op: (Int,Int) => Int, toStringPattern: String) extends MachineInstruction {
  override def execute(st: MachineState) = {
    val sValue = st.gprs(s).unsignedValue
    val tValue = st.gprs(t).unsignedValue
    
    st.withUpdatedGPR(r, MachineWord(op(sValue,tValue)))
  }
  override def toString = String.format(toStringPattern, new Integer(r), new Integer(s), new Integer(t))
}

case class ShiftOp(r: Int, x: Int, op: (Int,Int) => Int, toStringPattern: String) extends MachineInstruction {
  override def execute(st: MachineState) = {
    val rValue = st.gprs(r).unsignedValue
    
    st.withUpdatedGPR(r, MachineWord(op(rValue,x)))
  }
  override def toString = String.format(toStringPattern, new Integer(r), new Integer(r), new Integer(x))
}

case class Jump(r: Int, targetAddress: MachineWord) extends MachineInstruction {
  override def execute(st: MachineState) = {
    val r0Value = st.gprs(0).unsignedValue
    val rValue = st.gprs(r).unsignedValue
    
    if (rValue == r0Value) st.withUpdatedPC(targetAddress) else st
  }
  override def toString = s"JUMP ${targetAddress.toHexString} IF (r0 == r$r)"
}

case object Halt extends MachineInstruction {
  override def execute(s: MachineState): MachineState = s.withUpdatedCycleState(Halted)
  override def toString = "HALT"
}

case object IllegalInstruction extends MachineInstruction {
  override def execute(s: MachineState): MachineState = s.withUpdatedCycleState(Halted)
  override def toString = "Illegal Instruction"
}

case object Undefined extends MachineInstruction {
  override def toString = ""
}
